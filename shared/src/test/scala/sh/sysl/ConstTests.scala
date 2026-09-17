package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `const` — the one kind of module-level binding there is (`reference/modules.md § const — a value`).
 *
 * A constant is folded into every use and has no storage, so nothing here checks what is emitted:
 * what it names is a value, and the assertions are about the value arriving intact at each of the
 * four places a constant may stand — an expression, an array bound, an enum discriminant, and a
 * pattern. The last three are what a nullary function cannot do, and are the reason the
 * declaration exists.
 */
class ConstTests extends AnyFreeSpec with CodegenSupport with RunSupport with ParseSupport {

  "the declaration parses" - {
    "with its type and its value" in {
      prog("const n: int = 1") shouldBe List(ConstDecl("n", NamedType("int"), i(1)))
    }

    "and carries a visibility modifier like any other declaration" in {
      prog("private const n: int = 1") shouldBe
        List(ConstDecl("n", NamedType("int"), i(1), vis = Visibility.File))
      prog("private[geom] const n: int = 1") shouldBe
        List(ConstDecl("n", NamedType("int"), i(1), vis = Visibility.Scoped("geom")))
    }

    "a value is not optional" in {
      progError("const n: int") should not be empty
    }

    "and neither is a type" in {
      progError("const n = 1") should not be empty
    }
  }

  /** A constant is folded into its uses before the program runs, so there is nothing for one written
    * inside a body to be folded *in* — it belongs to a module or to nowhere.
    *
    * The second assertion is the one with history. A block listed its constants among the names it
    * binds, so that a use above a declaration could be told it was written too early — but nothing
    * ever bound them, so every use of one got that sentence instead, from *below* the declaration as
    * readily as from above it. The message named a mistake the program had not made.
    */
  "a constant belongs to a module, not to a block" - {
    val inABody =
      """f() -> int
        |    const k: int = 7
        |    k + 1
        |
        |print(str(f()))
        |""".stripMargin

    "so one written inside a function body is refused where it stands" in {
      err(inABody) should include("a constant is a module member and is declared at the top level")
    }

    "and the use below it is never told the name was declared below it" in {
      err(inABody) should not include "declared below this"
    }

    "the same inside a loop body, which is a block like any other" in {
      err("""var i = 0
            |while i < 2
            |    const step: int = 1
            |    i += step
            |""".stripMargin) should include("a constant is a module member")
    }

    "while the one written at the top of the file is untouched" in {
      run("const k: int = 7\nprint(str(k + 1))") shouldBe "8\n"
    }
  }

  "a constant is a value" - {
    "read in an expression" in {
      run("const n: int = 7\nprint(str(n + 1))") shouldBe "8\n"
    }

    "with its declared type, not the one the context wanted" in {
      err("const n: usize = 7\nvar m: int = n") should include("usize")
    }

    // The literal takes its type from the declaration, which is the ordinary rule for where a
    // literal sits (`01`) — so no suffix is written and none is needed.
    "of any scalar type" in {
      run(
        """const a: u8 = 255
          |const b: real = 0.5
          |const c: bool = true
          |const d: char = 'q'
          |const e: string = "hi"
          |print(s"${str(a)} ${str(b)} ${str(c)} ${str(d)} $e")
          |""".stripMargin,
      ) shouldBe "255 0.5 true q hi\n"
    }

    "reachable from another module by its full path" in {
      runIn(
        ("limits", "limits.sysl", "module limits\nconst width: int = 12\n"),
        ("", "main.sysl", "print(str(limits.width))\n"),
      ) shouldBe "12\n"
    }

    "and by an import" in {
      runIn(
        ("limits", "limits.sysl", "module limits\nconst width: int = 12\n"),
        ("", "main.sysl", "import limits.width\nprint(str(width))\n"),
      ) shouldBe "12\n"
    }

    /** The three positions above — an array bound, a discriminant, a pattern — are what a constant
     * is *for*, and the qualified spelling reached none of them: the folder matched a bare name and
     * a full path is a field read after parsing, so the very same declaration was a constant when
     * imported and not one when named through its module.
     *
     * It read as a rule about `c const`, since a binding is where the qualified spelling turns up in
     * bulk. It was never about `c const` at all, which is why these are written with an ordinary one.
     */
    "in every position a constant may stand, and not only in an expression" in {
      runIn(
        ("limits", "limits.sysl", "module limits\nconst width: int = 3\n"),
        ("", "main.sysl",
          """const same: int = limits.width * 2
            |
            |enum Step
            |    Small = limits.width
            |    Large = 9
            |
            |var xs: [limits.width]int
            |
            |print(s"$same ${int(Step.Small)} ${xs.len}")
            |""".stripMargin),
      ) shouldBe "6 3 3\n"
    }

    /** A path deeper than one segment is the same question asked twice, and it is worth its own case
     * because flattening a chain of field reads is where a fix could stop one level short.
     */
    "however deep the module path is" in {
      runIn(
        ("a.b", "b.sysl", "module a.b\nconst width: int = 4\n"),
        ("", "main.sysl", "const same: int = a.b.width\nvar xs: [a.b.width]int\nprint(s\"$same ${xs.len}\")\n"),
      ) shouldBe "4 4\n"
    }

    /** The other half: a `Field` that names no constant has to go on being reported as what it is,
     * rather than becoming "not a constant expression" for every field read in the language.
     */
    "while a field read of a value is still not a constant" in {
      err("struct P\n    x: int\nend P\nval p = P(1)\nconst n: int = p.x") should include("not a constant")
    }

    "unless it is private to its file" in {
      errIn(
        ("limits", "a.sysl", "module limits\nprivate const width: int = 12\n"),
        ("limits", "b.sysl", "module limits\nwide() -> int = width\n"),
        ("", "main.sysl", "print(str(limits.wide()))\n"),
      ) should include("private")
    }
  }

  "a constant expression folds" - {
    "arithmetic and bit operations" in {
      run(
        """const a: int = 2 + 3 * 4
          |const b: int = 1 << 10
          |const c: int = 0xFF & 0x0F | 0x30
          |const d: int = ~0
          |print(s"${str(a)} ${str(b)} ${str(c)} ${str(d)}")
          |""".stripMargin,
      ) shouldBe "14 1024 63 -1\n"
    }

    "one constant in terms of another, whichever order they were written in" in {
      run(
        """const total: usize = head + tail
          |const head: usize = 8
          |const tail: usize = 4
          |print(str(total))
          |""".stripMargin,
      ) shouldBe "12\n"
    }

    "a comparison, to a boolean" in {
      run("const big: bool = 100 > 99\nprint(str(big))") shouldBe "true\n"
    }

    "a conversion, which truncates exactly as a written one does" in {
      run("const low: u8 = u8(300)\nprint(str(low))") shouldBe "44\n"
    }

    "floating-point arithmetic" in {
      run("const half: real = 1.0 / 4.0 + 0.25\nprint(str(half))") shouldBe "0.5\n"
    }

    /** Two string literals joined, which is the one operator on strings that makes a value rather
     * than a verdict. It is what a long constant written in pieces needs — a table, a census, a
     * usage message, one line per source line so a reader can see the lines — and without it such a
     * value had to be a `val`, which is storage and which no array bound may name.
     */
    "two string literals joined, which is what lets a long constant be written in pieces" in {
      run(
        """const greeting: string = "hello" + ", " + "world"
          |print(greeting)
          |""".stripMargin,
      ) shouldBe "hello, world\n"
    }

    "including one built out of other constants" in {
      run(
        """const head: string = "usage: "
          |const tail: string = "sysl <command>"
          |const usage: string = head + tail
          |print(usage)
          |""".stripMargin,
      ) shouldBe "usage: sysl <command>\n"
    }

    // It is folded rather than concatenated at run time, which is the claim that makes it a
    // *constant* expression: a value that had to be built could not stand where a constant stands,
    // and a pattern is one of the places that separates the two.
    "and it is folded, so the joined value may stand as a pattern" in {
      run(
        """const tag: string = "ab" + "cd"
          |describe(s: string) -> string =
          |    s match
          |        tag -> "the tag"
          |        else "something else"
          |print(s"${describe("abcd")} ${describe("ab")}")
          |""".stripMargin,
      ) shouldBe "the tag something else\n"
    }
  }

  "a constant may be an array bound" - {
    "which is the whole reason for the declaration" in {
      run(
        """const capacity: usize = 6
          |var buf: [capacity]u8
          |print(str(buf.len))
          |""".stripMargin,
      ) shouldBe "6\n"
    }

    "including one computed from others" in {
      run(
        """const lit: usize = 286
          |const dist: usize = 30
          |var lengths: [lit + dist]u8
          |print(str(lengths.len))
          |""".stripMargin,
      ) shouldBe "316\n"
    }

    "and it sizes a struct's field, so two declarations cannot drift apart" in {
      run(
        """const capacity: usize = 4
          |struct Chunk
          |    code: [capacity]u8
          |    len: usize
          |end Chunk
          |var c: Chunk
          |print(str(c.code.len))
          |""".stripMargin,
      ) shouldBe "4\n"
    }

    "a call is still not one" in {
      err("capacity() -> usize = 8usize\nvar buf: [capacity()]u8") should include("must be a constant")
    }
  }

  "a constant may be an enum discriminant" - {
    "on its own" in {
      run(
        """const base: int = 10
          |enum Step
          |    First = base
          |    Second
          |end Step
          |print(s"${str(int(Step.First))} ${str(int(Step.Second))}")
          |""".stripMargin,
      ) shouldBe "10 11\n"
    }

    "and in an expression, at the enum's own width" in {
      run(
        """const top: u8 = 255
          |enum Level: u8
          |    Low = 1
          |    High = top - 1
          |end Level
          |print(str(u8(Level.High)))
          |""".stripMargin,
      ) shouldBe "254\n"
    }

    "one that does not fit is still refused" in {
      err("const top: int = 300\nenum Level: u8\n    High = top\nend Level") should include("does not fit")
    }
  }

  // The trap this rides past: in Rust a lowercase `const` in a pattern binds instead of matching,
  // silently making the arm irrefutable. A name here resolves against what is declared before it is
  // taken as a binding, which is the same path an enum variant already went down.
  "a constant may be a pattern" - {
    "and matches by value rather than binding" in {
      run(
        """const limit: int = 3
          |describe(n: int) -> string =
          |    n match
          |        limit -> "at the limit"
          |        else "somewhere else"
          |print(s"${describe(3)} ${describe(4)}")
          |""".stripMargin,
      ) shouldBe "at the limit somewhere else\n"
    }

    "a name that is not a constant still binds" in {
      run(
        """describe(n: int) -> string =
          |    n match
          |        other -> s"bound ${str(other)}"
          |print(describe(9))
          |""".stripMargin,
      ) shouldBe "bound 9\n"
    }
  }

  "what a constant may not be" - {
    "defined in terms of itself" in {
      err("const n: int = n + 1") should include("in terms of itself")
    }

    "or in terms of something that is defined in terms of it" in {
      val message = err("const a: int = b\nconst b: int = a\nprint(str(a))")

      message should include("in terms of itself")
      message should include("b")
    }

    "given a value that is not constant" in {
      err("f() -> int = 1\nconst n: int = f()") should include("not a constant expression")
    }

    "given a value that does not fit its type" in {
      err("const n: u8 = 256") should include("does not fit")
    }

    "given a value of another kind entirely" in {
      err("const n: int = \"twelve\"") should include("declared int")
    }

    "declared at a type that is not a scalar" in {
      err("enum Mode\n    On\n    Off\nend Mode\nconst m: Mode = 1") should include("is not")
    }

    /** An AGGREGATE is refused for being storage, and is pointed at the `val` that holds it.
     *
     * **The message a reader used to get was about constancy and was misleading**: an aggregate
     * initializer folds to nothing, so the refusal fired at the fold — *"the value of 'zeros' is
     * not a constant expression"* — about an expression a `val` one keyword away lays into the
     * object file as constant data. `ModuleStorage.isStatic` answers `true` for all three of these
     * shapes, which is why `sysl.encoding.nil` is a `val` and links on a freestanding target.
     *
     * The fix is an ordering: the declared type is resolved before the value is folded, so the rule
     * that was always the right answer is the one that gets to speak.
     */
    "declared at an aggregate type, which is storage rather than a value" in {
      val cases = List(
        "an array literal" -> "const zeros: [3]u8 = [0, 0, 0]",
        "an array fill"    -> "const zeros: [16]u8 = [0; 16]",
        "a struct over an array" ->
          "struct Id\n    raw: [16]u8\nend Id\nconst nil: Id = Id([0; 16])",
        "a tuple"          -> "const pair: (int, int) = (1, 2)",
      )

      for (what, src) <- cases do
        withClue(s"$what: ") {
          val message = err(s"$src\nprint(1)")

          message should include("is storage")
          message should include("write")
          message should include("'val'")
          // The old wording made a claim about constancy that the neighbouring `val` contradicts,
          // and this is what says it has stopped being made.
          message should not include "not a constant expression"
        }
    }

    /** And the `val` really does hold what the `const` would not, which is the half a diagnostic
     * test cannot show: advice nothing compiles is advice that can rot (`sysl.sh`'s CLAUDE.md makes
     * the same point about an `error` block on a page).
     */
    "and the 'val' the refusal names compiles and holds the value" in {
      run(
        """struct Id
          |    raw: [16]u8
          |end Id
          |val nil: Id = Id([0; 16])
          |print(nil.raw[0], nil.raw[15])
          |""".stripMargin,
      ) shouldBe "0 0\n"
    }

    "divided by zero where the compiler is the one dividing" in {
      err("const n: int = 1 / 0") should include("divided by zero")
    }

    "declared twice" in {
      err("const n: int = 1\nconst n: int = 2") should include("already declared")
    }

    "declared over a function's name" in {
      err("const n: int = 1\nn() -> int = 2") should include("already declared as a constant")
    }

    "declared over an enum variant's name" in {
      err("enum Colour\n    Red\nend Colour\nconst Red: int = 1") should include("already used by enum")
    }

    // `reference/modules.md § const — a value` turns on a constant having no address: it is folded into each use, occupies no
    // storage, needs no initialization order, and a `no alloc` module may hold one. `&capacity`
    // being unwritable is that property seen from the program's side, and it is what divides a
    // `const` from the `val` that exists to be indexed.
    "pointed at, since it is folded into each use and sits nowhere" in {
      err("const capacity: usize = 512\nvar p = &capacity\nprint(*p)") should
        include("'&' needs a variable, a field, an element, or a dereference")
    }
  }

  /** A constrained subtype is a scalar for this purpose, which `reference/errors.md § Constrained
    * types` settles rather than this file: without `new` such a type *is* its base. What it adds is
    * the `within` range, checked here against a value that is already known — the run-time check a
    * `val` would have had, made one step earlier because a constant has no run time of its own.
    */
  "a constant may be declared at a constrained subtype" - {
    "and holds a value the range admits" in {
      run("type Age = int within 0..150\n\nconst a: Age = 42\n\nprint(str(a))\n") shouldBe "42\n"
    }

    "while a value outside it is refused where it is written, naming both ends" in {
      val message = err("type Age = int within 0..150\n\nconst a: Age = 200\n\nprint(str(a))\n")

      message should include("does not admit")
      message should include("200")
      message should include("150")
    }

    "an exclusive upper bound says so rather than naming a value it excludes" in {
      err("type Slot = u8 within 0..<200\n\nconst s: Slot = 200\n\nprint(str(s))\n") should
        include("under 200")
    }

    /** The base's own width still answers first, so the two checks do not have to agree about which
      * mistake was made — a value no `u8` could hold is not a range's business.
      */
    "and the base's width is checked before the range is" in {
      err("type Slot = u8 within 0..200\n\nconst s: Slot = 300\n\nprint(str(s))\n") should
        include("does not fit")
    }

    /** `reference/errors.md § new is what makes it a type`: a derived type is reached only through
      * a written conversion, in both directions and with no position excused — and a constant is
      * the value it was written as, so there is nowhere on the line to write one.
      */
    "a 'new' type is refused, since a constant has nowhere to write the conversion" in {
      err("type Meters = new int\n\nconst m: Meters = 3\n\nprint(str(m))\n") should
        include("'new' type")
    }

    /** A predicate is a function, checked where a value is *made* — and a constant is folded into
      * every use rather than made anywhere, so admitting one would be a check the declaration claims
      * and the program never gets.
      */
    "and a 'where' predicate is refused, since there is no site to run it at" in {
      err("type Even = int where value % 2 == 0\n\nconst n: Even = 4\n\nprint(str(n))\n") should
        include("'where' predicate")
    }
  }
}
