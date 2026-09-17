package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Diagnostics: where they point, how they render, and how many of them one compilation yields.
 *
 * These assert the *rendering*, the *choice of position*, and the *recovery* — three separable
 * things. A diagnostic that quotes the wrong line is as useless as one that quotes none, and a
 * compiler that stops at the first mistake makes the other two matter less than they should.
 */
class DiagnosticTests extends AnyFreeSpec with Matchers {

  /** The rendered diagnostics for a program that must be rejected. */
  private def diag(src: String, name: String = "t.sysl"): String =
    Compiler.compileToLlvm(src, name) match {
      case Left(e)  => e
      case Right(_) => fail(s"expected an error from:\n$src")
    }

  /** How many separate errors a compilation reported. */
  private def count(out: String): Int = out.linesIterator.count(_.startsWith("error: "))

  /** The source lines the errors point at, in the order they were reported. */
  private def at(out: String): List[Int] =
    """--> t\.sysl:(\d+):""".r.findAllMatchIn(out).map(_.group(1).toInt).toList

  /** The underline of the one diagnostic `out` carries, as the reader sees it. */
  private def underline(out: String): String =
    out.linesIterator.toList.last.dropWhile(_ != '|').drop(1)

  /** Every span in a parsed program, by the node that carries it, read through `read` — which is
   * `pos` for what a diagnostic points at and `extent` for what the node was parsed from.
   */
  private def spansBy(read: Positioned => Option[Pos])(src: String): List[(Any, (Int, Int, Int, Int))] = {
    def walk(node: Any): List[(Any, (Int, Int, Int, Int))] = {
      val here = node match
        case p: Positioned => read(p).toList.map(x => (node, (x.line, x.col, x.endLine, x.endCol)))
        case _             => Nil

      val below = node match
        case xs: List[?]  => xs.flatMap(walk)
        case o: Option[?] => o.toList.flatMap(walk)
        case p: Product   => p.productIterator.toList.flatMap(walk)
        case _            => Nil

      here ::: below
    }

    SyslParser.parse(src, "t.sysl") match
      case Right(p) => walk(p.body)
      case Left(e)  => fail(s"the fixture does not parse: $e")
  }

  private val spans   = spansBy(_.extent)
  private val anchors = spansBy(_.pos)

  "rendering" - {
    "names the file, the line, and the column, and underlines the token" in {
      val src =
        """var x = 1
          |print(nope)
          |""".stripMargin

      diag(src) shouldBe
        List(
          "error: undefined name 'nope'",
          " --> t.sysl:2:7",
          "  |",
          "2 | print(nope)",
          "  |       ^^^^",
        ).mkString("\n")
    }

    "keeps the gutter aligned when the line number takes two digits" in {
      val padding = (1 to 9).map(n => s"var a$n = $n").mkString("\n")

      diag(s"$padding\nprint(nope)\n") shouldBe
        List(
          "error: undefined name 'nope'",
          "  --> t.sysl:10:7",
          "   |",
          "10 | print(nope)",
          "   |       ^^^^",
        ).mkString("\n")
    }

    "carries whatever name the driver gave the source" in {
      diag("print(nope)\n", "src/deep/thing.sysl") should include("--> src/deep/thing.sysl:1:7")
    }

    "indents the caret with tabs where the line has tabs, so it lands under the column" in {
      Pos(Source("t.sysl", "a\tb\n"), 1, 3).render("boom") shouldBe
        List(
          "error: boom",
          " --> t.sysl:1:3",
          "  |",
          "1 | a\tb",
          "  |  \t^",
        ).mkString("\n")
    }

    "renders a message with no position at all rather than inventing one" in {
      Diagnostic.render("something went wrong", None) shouldBe "error: something went wrong"
    }

    "survives a column past the end of its line" in {
      Pos(Source("t.sysl", "ab"), 1, 99).render("boom") should include("|   ^")
    }
  }

  /** The underline is as wide as what is being complained about and no wider — a name, a literal,
   * or a whole expression where the expression is what is wrong. These are the cases the width is
   * decided by, and the last three are the ones that would otherwise underline something that is
   * not there.
   */
  "how far the underline runs" - {
    "the whole of an identifier, however long" in {
      underline(diag("print(a_rather_long_undefined_name)\n")) shouldBe "       " + "^" * 28
    }

    "the whole of a string literal, its quotes included" in {
      val src =
        """add(a: int, b: int) -> int = a + b
          |print(add(1, "two"))
          |""".stripMargin

      underline(diag(src)) shouldBe "              ^^^^^"
    }

    "the whole of a character literal, which is three columns and not one" in {
      val src =
        """takes(n: int) -> int = n
          |print(takes('x'))
          |""".stripMargin

      underline(diag(src)) shouldBe "             ^^^"
    }

    "one caret for a one-character token" in {
      val src =
        """var a = 1
          |var = 5
          |""".stripMargin

      underline(diag(src)) shouldBe "     ^"
    }

    // Where the mistake is a whole expression rather than a name inside it, the expression is what
    // the reader is shown. Before a node knew how far it ran this was one caret, under the `1`.
    "the whole of an expression, where the expression is what is wrong" in {
      val src =
        """takes(s: string) -> string = s
          |print(takes(1 + 2))
          |""".stripMargin

      underline(diag(src)) shouldBe "             ^^^^^"
    }

    // The quote shows one line, so an underline longer than it would be pointing past the end of
    // what the reader can see. A text block's opening delimiter is what is left.
    "no further than the end of the line a span starts on" in {
      val src =
        "takes(n: int) -> int = n\nprint(takes(\"\"\"\nspanning\n\"\"\"))\n"

      underline(diag(src)) shouldBe "             ^^^"
    }

    // Every position built from a line and a column alone — a literate margin, a conditional
    // directive, a parse that ran out of input — has no token to measure.
    "one caret where the position carries no extent at all" in {
      Pos(Source("t.sysl", "abcdef\n"), 1, 3).render("boom") should endWith("|   ^")
    }

    "and one caret where a span ends before it begins" in {
      Pos(Source("t.sysl", "abcdef\n"), 1, 4, 1, 2).render("boom") should endWith("|    ^")
    }
  }

  /** The lexer knows where a token ends as an **offset**, and everything that reports a position
   * speaks in lines and columns, so this conversion sits between the two.
   */
  "an offset becomes a line and a column" - {
    val src = Source("t.sysl", "ab\ncde\n\nf")

    "at the very beginning" in {
      src.placeOf(0) shouldBe (1, 1)
    }

    "within the first line" in {
      src.placeOf(1) shouldBe (1, 2)
    }

    "at a newline, which belongs to the line it ends" in {
      src.placeOf(2) shouldBe (1, 3)
    }

    "at the start of the line after it" in {
      src.placeOf(3) shouldBe (2, 1)
    }

    "on an empty line" in {
      src.placeOf(7) shouldBe (3, 1)
    }

    "on the last line, which has no newline of its own" in {
      src.placeOf(8) shouldBe (4, 1)
    }

    // Where a token runs to the end of input, and where anything asks about a place that is not
    // in the file at all.
    "past the end, answering the place just past the last character" in {
      src.placeOf(9) shouldBe (4, 2)
      src.placeOf(9999) shouldBe (4, 2)
    }

    "before the beginning" in {
      src.placeOf(-1) shouldBe (1, 1)
    }

    "in a file with no newline in it whatsoever" in {
      Source("t.sysl", "abc").placeOf(2) shouldBe (1, 3)
    }

    "in an empty file" in {
      Source("t.sysl", "").placeOf(0) shouldBe (1, 1)
    }
  }

  "where the caret lands" - {
    // The grammar picks which of its failures to report by asking which got furthest, and running
    // out of input is always further along than the mistake that caused it. A reader that gave the
    // end of the file a real line would therefore win every one of those comparisons and answer
    // `end of input` to everything — so it answers line zero, which is what `NoPosition` answered
    // and what the arithmetic was built on. This is the shortest input that shows it.
    "not on the end of the file, when what ran out of input is the grammar and not the reader" in {
      val out = diag("print(,)")

      out should include("expected")
      out should not include "end of input"
    }

    "on the argument that is wrong, not on the call" in {
      // `"two"` starts at column 14 of line 2; the call starts at column 7.
      val src =
        """add(a: int, b: int) -> int = a + b
          |print(add(1, "two"))
          |""".stripMargin

      diag(src) should include("--> t.sysl:2:14")
    }

    "on the callee, for a call that names nothing or takes other arguments" in {
      val src =
        """add(a: int, b: int) -> int = a + b
          |print(missing())
          |print(add(1))
          |""".stripMargin
      val out = diag(src)

      out should include("--> t.sysl:2:7")
      out should include("--> t.sysl:3:7")
    }

    // Under the field's own name, not under the `.` beside it — the message says `'y'`, so that is
    // where a reader is sent. `DiagnosticPositionTests` holds the general form of this.
    "on the selection that names a missing field" in {
      val src =
        """struct P
          |    x: int
          |var p = P(1)
          |print(p.y)
          |""".stripMargin

      diag(src) should include("--> t.sysl:4:9")
    }

    "on the construct that raised the error, not on the last thing it looked at" in {
      // The branch types are compared *after* both branches are analyzed, so a position that
      // was not restored would point at the `"s"` in column 30 instead of the `if` in column 9.
      val src =
        """var x = 1
          |var y = if x > 0 then 1 else "s"
          |""".stripMargin
      val out = diag(src)

      out should include("if branches have different types")
      out should include("--> t.sysl:2:9")
    }

    "just after the '=' of a binding whose value was forgotten" in {
      // The caret lands in the space the value should have occupied: column 8, immediately past the
      // `=` in column 7.
      //
      // **The fixture used to indent the following line**, back when a binding's `=` introduced no
      // block and that was the same mistake. It introduces one now, so an indented line after it is
      // the value rather than a missing one — what remains a mistake, and what a forgotten value
      // actually leaves behind, is a following line at the same indentation. The position claim is
      // unchanged and is the point of the test; only the way of making the mistake moved.
      val out = diag("var x =\nprint(str(1))\n")

      out should include("--> t.sysl:1:8")
      out should include("expression expected")
    }

    // The same mistake, one indent deeper. A function's body is parsed as part of the declaration,
    // so a body that will not parse makes the *declaration* fail — and what used to survive was the
    // position where the declaration was reconsidered as something else, which is its first line.
    //
    // The invariant this asserts is the one every other test in this section asserts: a diagnostic
    // points at the construct that is wrong. It pointed several lines above it, at a line that is
    // correct, and sent the reader looking at the signature.
    "on the binding inside a body, and not on the declaration the body belongs to" in {
      // The value is on the line below at the body's own indentation, so it is neither a
      // continuation nor a block — the same mistake as above, one indent deeper.
      val src =
        """f(a: int) -> int
          |    val b =
          |    a + 1
          |
          |    b
          |end f
          |
          |print(str(f(1)))
          |""".stripMargin

      diag(src) should include("--> t.sysl:2:")
    }

    "on the declaration that redeclares a name" in {
      val src =
        """f() -> int = 1
          |f() -> int = 2
          |""".stripMargin

      diag(src) should include("--> t.sysl:2:1")
    }

    "on the type reference that names nothing" in {
      diag("var x: Nope = 1\n") should include("--> t.sysl:1:8")
    }
  }

  "other passes" - {
    "a parse error renders like any other diagnostic" in {
      val src =
        """var a = 1
          |var = 5
          |""".stripMargin

      diag(src) shouldBe
        List(
          "error: identifier expected",
          " --> t.sysl:2:5",
          "  |",
          "2 | var = 5",
          "  |     ^",
        ).mkString("\n")
    }

    "a parse error that runs out of input still renders against the source" in {
      val out = diag("f(a: int")

      out should include("--> t.sysl:")
      out should include("f(a: int")
    }

    "an escape error points at the slice that leaves the frame" in {
      // The vehicle is an array **parameter**, since a local one is promoted rather than reported
      // now (`05`). What is being checked is the caret, not the rule.
      val src =
        """sum(bytes: []u8) -> int = 0
          |
          |scratch(s: [4]u8) -> []u8
          |    s[..]
          |""".stripMargin
      val out = diag(src)

      out should include("would outlive the array")
      out should include("--> t.sysl:4:6")
    }

    /** A lexical error must say what the lexer knew, not what the grammar made of the wreckage.
     * Every message in `SyslLexical` used to be unreachable this way: the token it produced matched
     * no rule, so the reported failure was wherever the longest partial match stopped — `print("oops`
     * said "newline expected" at the paren, because `print` alone is a complete statement. The lexer
     * tests call the lexer directly and so could not see it.
     */
    "a lexical error reports itself rather than the grammar's reaction to it" in {
      diag("print(\"oops\n") shouldBe
        List(
          "error: unterminated string literal",
          " --> t.sysl:1:7",
          "  |",
          "1 | print(\"oops",
          "  |       ^",
        ).mkString("\n")
    }

    // The comment case is the one the lexer could not report on its own at all: an unclosed comment
    // consumes to end of input, so every later token is gone and the grammar's complaint is about
    // whatever the first line happened to end in.
    "an unclosed block comment says so, at the delimiter that opened it" in {
      val src =
        """print(1)
          |/* never closed
          |print(2)
          |""".stripMargin

      diag(src) shouldBe
        List(
          "error: unclosed comment",
          " --> t.sysl:2:1",
          "  |",
          "2 | /* never closed",
          "  | ^",
        ).mkString("\n")
    }

    "an unterminated character literal reports itself too" in {
      diag("var c = '1\n") should include("error: unterminated character literal")
    }

    // An inner close satisfies only the comment it opened, so a nested comment left open reports the
    // outer one — the case a depth counter gets wrong by treating the first `*/` as the end.
    "a nested comment left open reports the outer one" in {
      val src =
        """print(1)
          |/* outer /* inner */
          |print(2)
          |""".stripMargin
      val out = diag(src)

      out should include("error: unclosed comment")
      out should include("--> t.sysl:2:1")
    }

    // Indentation made of both tabs and spaces on one line has no defensible width, so it is
    // refused — and refused by the lexer, which is why it reaches a reader through the same route
    // an unterminated literal does.
    "indentation mixing tabs and spaces on one line is refused" in {
      val src = "f(n: int) -> int\n\tif n > 0 then\n    \tn\n\telse\n\t    0\n"
      val out = diag(src)

      out should include("only tabs or spaces")
      out should include("--> t.sysl:3:1")
    }

    // The negative names the library's own directory rather than a source name of the compiler's.
    // It used to say `<prelude>`, which was that string literal's `Source.name` — a name nothing
    // carries now, so the assertion would hold whatever the diagnostic pointed at.
    "an error about a library type still quotes the user's file" in {
      val src =
        """f() -> Option[int]
          |    Some("x")
          |""".stripMargin
      val out = diag(src)

      out should include("--> t.sysl:2:")
      out should not include "library/sysl"
    }
  }

  "reporting more than one error" - {
    "reports every independent mistake, in source order" in {
      val src =
        """var a = nope1
          |var b = nope2
          |var c = nope3
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 3
      at(out) shouldBe List(1, 2, 3)
    }

    "separates one diagnostic from the next with a blank line" in {
      val src =
        """var a = nope1
          |var b = nope2
          |""".stripMargin

      diag(src) should include("^\n\nerror:")
    }

    "keeps going through the statements of a single function body" in {
      val src =
        """f() -> int
          |    var a = nope1
          |    var b = nope2
          |    3
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 2
      at(out) shouldBe List(2, 3)
    }

    "keeps going from one function to the next" in {
      val src =
        """f() -> int = nope1
          |g() -> int = nope2
          |h() -> int = 3
          |print(h())
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 2
      at(out) shouldBe List(1, 2)
    }

    "keeps going through declarations, so a bad one does not hide a later one" in {
      val src =
        """struct P
          |    x: Nope
          |struct Q
          |    y: AlsoNope
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 2
      at(out) shouldBe List(2, 4)
    }

    "reports a whole file's worth of unrelated mistakes at once" in {
      val src =
        """add(a: int, b: int) -> int = a + b
          |
          |struct Point
          |    x: int
          |    y: int
          |end Point
          |
          |var p = Point(1, 2)
          |print(add(1, "two"))
          |print(p.zed)
          |print(other)
          |print(add(1))
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 4
      at(out) shouldBe List(9, 10, 11, 12)
    }
  }

  "not reporting the consequences of an error twice" - {
    "a name whose initializer failed is still bound, so later uses stay quiet" in {
      val src =
        """f() -> int
          |    var a = nope
          |    var b = a + 1
          |    var c = a * a
          |    b + c
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 1
      at(out) shouldBe List(2)
    }

    "but a *different* mistake after one is still reported" in {
      val src =
        """f() -> int
          |    var a = nope
          |    var b = a + 1
          |    var c = alsoNope
          |    b + c
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 2
      at(out) shouldBe List(2, 4)
    }

    "a parameter whose type does not resolve does not make its function undefined too" in {
      val src =
        """f(a: Nope) -> int = 1
          |print(f(1))
          |print(f(2))
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 1
      out should include("Nope")
      out should not include "undefined function"
    }

    "one mistake in a generic body is one error however many times it is instantiated" in {
      val src =
        """bad[T](x: T) -> int
          |    nope
          |
          |print(bad(1))
          |print(bad("s"))
          |print(bad(true))
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 1
      at(out) shouldBe List(2)
    }
  }

  "how many errors are reported" - {
    "stops at five, and says how many there were" in {
      val src = (1 to 8).map(n => s"var v$n = nope$n").mkString("\n")
      val out = diag(src)

      count(out) shouldBe 5
      at(out) shouldBe List(1, 2, 3, 4, 5)
      out should endWith("showing the first 5 of 8 errors")
    }

    "keeps the earliest five, since an error early in a file is the one worth reading" in {
      val src = (1 to 8).map(n => s"var v$n = nope$n").mkString("\n")

      diag(src) should include("undefined name 'nope1'")
      diag(src) should not include "nope6"
    }

    "says nothing extra when the count is exactly at the limit" in {
      val src = (1 to 5).map(n => s"var v$n = nope$n").mkString("\n")
      val out = diag(src)

      count(out) shouldBe 5
      out should not include "showing the first"
    }

    "caps escape errors the same way" in {
      val funcs = (1 to 7).map { n =>
        s"""f$n(a$n: [4]u8) -> []u8
           |    a$n[..]
           |""".stripMargin
      }.mkString("\n")
      val out = diag(funcs)

      count(out) shouldBe 5
      out should endWith("showing the first 5 of 7 errors")
    }
  }

  "counted things read as English" - {
    "one argument is an argument, not arguments" in {
      val src =
        """f(a: int) -> int = a
          |print(f(1, 2))
          |""".stripMargin

      diag(src) should include("takes 1 argument, but 2 arguments were given")
    }

    "one value given is 'was given', not 'were given'" in {
      val src =
        """struct P
          |    x: int
          |    y: int
          |var p = P(1)
          |""".stripMargin

      diag(src) should include("has 2 fields, but 1 value was given")
    }

    "and the plural forms still agree" in {
      val src =
        """f(a: int, b: int) -> int = a
          |print(f(1))
          |""".stripMargin

      diag(src) should include("takes 2 arguments, but 1 argument was given")
    }
  }

  "state left consistent by an abandoned region" - {
    "a field whose type does not resolve does not make later mentions look like a cycle" in {
      // The resolver marks a type as in-progress while it works out the fields. Leaving that mark
      // behind when a field fails made the next mention of the type report that it contains
      // itself — a diagnostic about nothing at all, on top of the real one.
      val src =
        """struct P
          |    x: Nope
          |    y: int
          |end P
          |
          |var p = P(1, 2)
          |print(p.x)
          |print(p.y)
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 1
      out should include("unknown type 'Nope'")
      out should not include "contains itself"
    }

    "a block abandoned part-way does not leak its bindings to the statements after it" in {
      // The `if` opens a scope that the failing branch never closes. Winding it back is what
      // keeps `hidden` out of scope on the last line — without it the analyzer would be more
      // permissive after an error than before one, and quietly accept this.
      val src =
        """f() -> int
          |    var x = if true then
          |        var hidden = 1
          |        nope
          |    else 2
          |    hidden
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 2
      at(out) shouldBe List(4, 6)
      out should include("undefined name 'hidden'")
    }

    "every function that lets a slice escape is reported, not just the first" in {
      val src =
        """sum(bytes: []u8) -> int = 0
          |
          |one(a: [4]u8) -> []u8
          |    a[..]
          |
          |two(b: [4]u8) -> []u8
          |    b[..]
          |""".stripMargin
      val out = diag(src)

      count(out) shouldBe 2
      at(out) shouldBe List(4, 7)
    }
  }

  /** A diagnostic is **carried** now and rendered at the edge, so there are two roads to one text.
   * These pin them together: the compiler's string entry points are `Diagnostic.report` over the
   * structured ones, and nothing a reader sees moved when the type arrived.
   */
  "carried rather than rendered" - {

    def sources(src: String) = List(Source("t.sysl", src))

    "the rendered report is exactly what the string entry point answers" in {
      val src = "var a = nope1\nvar b = nope2\nprint(p.zed)\n"

      Compiler.checked(sources(src)).swap.toOption.map(Diagnostic.report) shouldBe
        Compiler.compile(sources(src)).swap.toOption
    }

    "and so it is for a parse error, which comes from a different stage entirely" in {
      val src = "var a = 1\nvar = 5\n"

      Compiler.checked(sources(src)).swap.toOption.map(Diagnostic.report) shouldBe
        Compiler.compile(sources(src)).swap.toOption
    }

    // The limit exists so a reader is not handed a wall of text. A caller reading them as data
    // wants all of them, so the truncation belongs to `report` and to nothing before it.
    "the five-diagnostic limit is the renderer's rule and not the list's" in {
      val src = (1 to 8).map(n => s"var v$n = nope$n").mkString("\n")

      Compiler.checked(sources(src)).swap.toOption.map(_.length) shouldBe Some(8)
      Compiler.compile(sources(src)).swap.toOption.map(count) shouldBe Some(5)
    }

    "a report of one is that one, with nothing wrapped around it" in {
      val one = Diagnostic("boom", Some(Pos(Source("t.sysl", "abc\n"), 1, 2, 1, 4)))

      Diagnostic.report(List(one)) shouldBe one.rendered
    }

    "and a report of none is nothing at all" in {
      Diagnostic.report(Nil) shouldBe ""
    }

    "source order is by file, then line, then column, and nowhere sorts last" in {
      val a = Source("a.sysl", "xx\nxx\n")
      val b = Source("b.sysl", "xx\n")

      val unordered = List(
        Diagnostic("nowhere", None),
        Diagnostic("b:1:1", Some(Pos(b, 1, 1))),
        Diagnostic("a:2:1", Some(Pos(a, 2, 1))),
        Diagnostic("a:1:2", Some(Pos(a, 1, 2))),
        Diagnostic("a:1:1", Some(Pos(a, 1, 1))),
      )

      Diagnostic.inSourceOrder(unordered).map(_.message) shouldBe
        List("a:1:1", "a:1:2", "a:2:1", "b:1:1", "nowhere")
    }
  }

  /** What a node's span covers, which is the claim everything above rests on and which the
   * rendering only shows indirectly. An editor asking "which construct is the cursor inside" reads
   * this rather than a diagnostic.
   */
  "a node carries the extent it was parsed from" - {

    "an identifier, from its first character to just past its last" in {
      spans("var x = 1\nprint(alpha)\n").collectFirst { case (Ident("alpha"), s) => s } shouldBe
        Some((2, 7, 2, 12))
    }

    "a string literal, quotes included, so the span is what was written" in {
      // The token is `"two"` — five columns — though the value it denotes is three characters.
      spans("""print("two")""" + "\n").collectFirst { case (StrLit("two"), s) => s } shouldBe
        Some((1, 7, 1, 12))
    }

    "a text block, whose end is on a later line than its start" in {
      val src = "var t = \"\"\"\nspanning\n\"\"\"\nprint(t)\n"

      spans(src).collectFirst { case (StrLit("spanning\n"), s) => s } shouldBe Some((1, 9, 3, 4))
    }

    // Everything above is one token, which is most of what a diagnostic points at and none of what
    // makes a span worth having. These are the nodes a cursor lands *inside*.

    "a call, from its callee to its closing bracket" in {
      spans("var x = 1\nprint(alpha)\n").collectFirst { case (Call(Ident("print"), _), s) => s } shouldBe
        Some((2, 1, 2, 13))
    }

    "a binary operation, over both its operands and the operator between them" in {
      spans("var x = 1 + 2\n").collectFirst { case (Binary("+", _, _), s) => s } shouldBe
        Some((1, 9, 1, 14))
    }

    // The precedence ladder is a dozen rules deep and hands a lone operand up through every one of
    // them. `setPos` keeping the first position is what stops the operand inheriting the ladder's
    // extent — without it every literal in the language would span its whole enclosing expression.
    "and each operand keeps its own, though a dozen rules passed it through" in {
      spans("var x = 1 + 2\n").collectFirst { case (IntLit(v, _), s) if v == 1 => s } shouldBe
        Some((1, 9, 1, 10))
    }

    "a binding, from its keyword to the end of what it was given" in {
      spans("var x = 1 + 2\n").collectFirst { case (_: VarDecl, s) => s } shouldBe Some((1, 1, 1, 14))
    }

    "a construct written over two lines, which ends on the second of them" in {
      val src = "var x = (1 +\n    2)\n"

      spans(src).collectFirst { case (_: VarDecl, s) => s } shouldBe Some((1, 1, 2, 7))
    }

    // The last token of the file is the one case with no token after it to carry its end, so the
    // reader answers for it — and it must answer with where the token stopped rather than with
    // where the file does.
    "a node whose last token is the file's last token" in {
      spans("print(alpha)").collectFirst { case (Call(Ident("print"), _), s) => s } shouldBe
        Some((1, 1, 1, 13))
    }
  }

  /** Where a node was parsed from and where a complaint about it belongs are two questions, and a
   * node answers each separately. The grammar anchors some nodes inside themselves on purpose —
   * `postfixTail` says why — and the extent must not disturb that, because the anchor is what every
   * diagnostic in the compiler is rendered against.
   */
  "a node's anchor and its extent are separate" - {

    // What is wrong with `foo(…)` is nearly always `foo` — it does not exist, or it does not take
    // these arguments — so that is where the caret goes, and it went there before extents existed.
    "a call still points a diagnostic at its callee" in {
      anchors("var x = 1\nprint(alpha)\n").collectFirst { case (Call(Ident("print"), _), s) => s } shouldBe
        Some((2, 1, 2, 6))
    }

    "while covering the whole of itself" in {
      spans("var x = 1\nprint(alpha)\n").collectFirst { case (Call(Ident("print"), _), s) => s } shouldBe
        Some((2, 1, 2, 13))
    }

    // A caret under the `.` sends the reader looking one column to the left of what was said, so a
    // field selection is anchored on the member name. Its extent begins at the receiver, which is
    // the first case where the two do not even start in the same place.
    "a field selection points at the member and covers the receiver too" in {
      val src = "var p = 1\nprint(p.missing)\n"

      anchors(src).collectFirst { case (Field(_, "missing"), s) => s } shouldBe Some((2, 9, 2, 16))
      spans(src).collectFirst { case (Field(_, "missing"), s) => s } shouldBe Some((2, 7, 2, 16))
    }

    // Nothing parsed a node the analyzer made up, so there is no extent to have — and answering
    // `pos` is better than answering nothing, since that is honestly where the node came from.
    "a node with no extent of its own falls back to where it points" in {
      val node = Ident("x").setPos(Pos(Source("t.sysl", "x\n"), 1, 1, 1, 2))

      node.extent shouldBe node.pos
    }
  }

  /** Widening a span is the one operation `at` performs on the extent it computes, and it is a
   * guard as much as an arithmetic: the end it is offered comes from the token after the rule, and
   * for a rule that consumed nothing that is a place *before* the span began.
   */
  "a span is widened, never shortened" - {
    val pos = Pos(Source("t.sysl", "abcdef\n"), 1, 2, 1, 4)

    "out to a later column on the same line" in {
      pos.endingAt(1, 6) shouldBe Pos(pos.source, 1, 2, 1, 6)
    }

    "out to a later line" in {
      pos.endingAt(3, 1) shouldBe Pos(pos.source, 1, 2, 3, 1)
    }

    "and not back to an earlier one, which is what a rule that consumed nothing offers" in {
      pos.endingAt(1, 2) shouldBe pos
      pos.endingAt(1, 3) shouldBe pos
    }

    "nor back to an earlier line" in {
      pos.endingAt(0, 99) shouldBe pos
    }

    "an end it already has costs nothing and changes nothing" in {
      pos.endingAt(1, 4) shouldBe pos
    }
  }

  "positions and structural equality" - {
    // A position is metadata a node carries, not part of what the node *is*, so the same text
    // parses to the same tree wherever it was read from. The `Program` around it is the one thing
    // that does differ: a file is a file, and which one it is is exactly what it records.
    "two parses of the same text from different files compare equal" in {
      def bodyOf(name: String) = SyslParser.parse("var x = 1 + 2", name).map(_.body)

      bodyOf("a.sysl") shouldBe bodyOf("b.sysl")
    }

    "even though each tree really does carry its own file's positions" in {
      def sourceOf(name: String): Option[String] =
        SyslParser.parse("var x = 1 + 2", name) match {
          case Right(Program(List(VarDecl(_, _, Some(init), _, _, _, _)), _, _, _, _, _, _, _)) =>
            init.pos.map(_.source.name)
          case other => fail(s"unexpected parse: $other")
        }

      sourceOf("a.sysl") shouldBe Some("a.sysl")
      sourceOf("b.sysl") shouldBe Some("b.sysl")
    }
  }
}
