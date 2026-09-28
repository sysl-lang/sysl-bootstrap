package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A module is a directory, so its files are one scope (`reference/modules.md`,
 * `reference/modules.md § The module graph is acyclic`).
 *
 * These are that claim from four sides: the header a file writes to say which module it contributes
 * to, the fact that splitting a module across files changes nothing about what its declarations can
 * see, the agreement between a header and the directory the file was found in, and which one file of
 * a program carries the statements it runs (`reference/modules.md § Where a program starts`).
 */
class ModuleTests extends AnyFreeSpec with ParseSupport with CodegenSupport with RunSupport {

  "the module header" - {
    "names the module a file contributes to" in {
      moduleOf("module oskit.arch\nprint(1)") shouldBe Some("oskit.arch")
    }

    "may be a single segment" in {
      moduleOf("module std\nprint(1)") shouldBe Some("std")
    }

    "is absent from a file that writes none" in {
      moduleOf("print(1)") shouldBe None
    }

    "does not become a statement" in {
      prog("module a.b\nprint(1)") shouldBe List(printStmt(i(1)))
    }

    "comes before everything, so a second one is not a statement" in {
      progError("module a\nprint(1)\nmodule b") should include("module")
    }

    // A capability annotation is a line of the header rather than part of this one
    // (`reference/modules.md § Capabilities are a module property`), and the refusal says so rather
    // than reporting a token it did not expect (`CapabilityClauseTests`).
    "does not carry the capability annotation on its own line" in {
      progError("module m @no_alloc\nprint(1)") should include("belongs in the file's header")
    }

    "may be preceded by blank lines" in {
      moduleOf("\n\nmodule a.b\nprint(1)") shouldBe Some("a.b")
    }

    // `module` is a reserved word, which is the cost of the header having no other marker.
    "spends the word, so nothing else may be called it" in {
      progError("var module = 1") should include("'module' is a reserved word")
    }
  }

  "one module, however many files" - {
    "a call reaches a function declared in another file" in {
      runOf(
        "main.sysl" -> "print(double(21))",
        "math.sysl" -> "double(n: int) -> int = n * 2",
      ) shouldBe "42\n"
    }

    // Hoisting registers every signature in the module before it checks any body, so a file may
    // call something declared in a file that comes later — there is no ordering to get wrong.
    "and the order the files are handed over does not matter" in {
      runOf(
        "math.sysl" -> "double(n: int) -> int = n * 2",
        "main.sysl" -> "print(double(21))",
      ) shouldBe "42\n"
    }

    "two files may call each other" in {
      val src = runOf(
        "main.sysl" -> "print(even(10))",
        "a.sysl"    -> "even(n: int) -> bool =\n    if n == 0 then true else odd(n - 1)",
        "b.sysl"    -> "odd(n: int) -> bool =\n    if n == 0 then false else even(n - 1)",
      )

      src shouldBe "true\n"
    }

    "a type declared in one file is used in another" in {
      runOf(
        "main.sysl"  -> "var p = Point(3, 4)\nprint(p.x + p.y)",
        "point.sysl" -> "struct Point\n    x: int\n    y: int",
      ) shouldBe "7\n"
    }

    "a trait, its implementation, and its use may each be in their own file" in {
      runOf(
        "main.sysl"  -> "var s: &Show = Point(3, 4)\nprint(s.show())",
        "trait.sysl" -> "trait Show\n    show(self) -> int",
        "point.sysl" -> "struct Point\n    x: int\n    y: int",
        "impl.sysl"  -> "impl Show for Point\n    show(self) -> int = self.x + self.y",
      ) shouldBe "7\n"
    }

    "a generic declared in one file is instantiated from another" in {
      runOf(
        "main.sysl" -> "print(first(Pair(7, 9)))",
        "pair.sysl" -> "struct Pair[T]\n    a: T\n    b: T\nfirst[T](p: Pair[T]) -> T = p.a",
      ) shouldBe "7\n"
    }

    "every file writes the same header" in {
      runOf(
        "main.sysl" -> "module oskit.arch\nprint(halt())",
        "cpu.sysl"  -> "module oskit.arch\nhalt() -> int = 3",
      ) shouldBe "3\n"
    }

    "and one file is the whole module where that is all there is" in {
      runOf("only.sysl" -> "print(1)") shouldBe "1\n"
    }

    "a file may be a header and nothing else" in {
      runOf(
        "main.sysl" -> "module m\nprint(halt())",
        "spare.sysl" -> "module m\n",
        "cpu.sysl"  -> "module m\nhalt() -> int = 3",
      ) shouldBe "3\n"
    }

    // A module none of whose files carries a statement still has an entry point, which does
    // nothing and succeeds. Its declarations are checked either way; what nothing reaches is not
    // written out, so the program that comes back does nothing at all.
    "a module that runs nothing still compiles" in {
      val out = irOf("a.sysl" -> "f() -> int = 1")

      out should include("define i32 @main(")
      out should not include "define i32 @f()"
    }

    "and a module of no files at all is the empty program" in {
      Compiler.compile(Nil) match {
        case Right(out) => out should include("define i32 @main(")
        case Left(e)    => fail(e)
      }
    }
  }

  "a header and the directory it was found in" - {
    // The header is checked against where the file sits, which is what makes the name a property of
    // the directory rather than of any file in it. It is the driver that knows the location, and it
    // hands it over on the source.
    "must agree" in {
      errIn(
        ("geom", "point.sysl", "module oskit\nf() -> int = 1"),
      ) should include("point.sysl declares 'oskit', but it sits in 'geom'")
    }

    "so a file at the root writes no header" in {
      errIn(
        ("", "main.sysl", "module geom\nf() -> int = 1"),
      ) should include("main.sysl declares 'geom', but it sits at the project root")
    }

    "and a file in a directory writes one" in {
      errIn(
        ("geom", "point.sysl", "f() -> int = 1"),
      ) should include("point.sysl declares no module, but it sits in 'geom'")
    }

    "however deep the directory is" in {
      runIn(
        ("", "main.sysl", "print(text.util.wrap(1))"),
        ("text.util", "pad.sysl", "module text.util\nwrap(n: int) -> int = n + 1"),
      ) shouldBe "2\n"
    }

    // Holding every file to the name its own location gives it is what replaces comparing the files
    // of a directory against each other: a file edited without its siblings is reported on its own
    // line rather than as a disagreement with whichever sibling happened to be read first.
    "with one report per file that strayed" in {
      val out = errIn(
        ("m", "a.sysl", "module m\nf() -> int = 1"),
        ("m", "b.sysl", "module n\ng() -> int = 2"),
        ("m", "c.sysl", "module o\nh() -> int = 3"),
      )

      out should include("b.sysl declares 'n'")
      out should include("c.sysl declares 'o'")
      out should not include "a.sysl declares"
    }

    // Requiring the header rather than inferring it from the path is what keeps a file
    // self-describing (`reference/modules.md`), and a file with nothing in it has not written one.
    // There is nothing to point at, so the diagnostic names the file and carries no caret.
    "and a file with nothing in it has not said which module it is" in {
      errIn(
        ("m", "a.sysl", "module m\nf() -> int = 1"),
        ("m", "empty.sysl", ""),
      ) should include("empty.sysl declares no module")
    }

    "which is no complaint at all at the root, where the empty path is what it says" in {
      runIn(
        ("", "a.sysl", "print(1)"),
        ("", "empty.sysl", ""),
      ) shouldBe "1\n"
    }

    // Files handed over with no project around them carry no location, so their headers are the
    // whole of what says which module each is in — which is how a test compiles a handful of them
    // directly, and how a single file compiles with nothing to be measured against.
    "with no project around them, the headers are the whole of it" in {
      val out = irOf(
        "a.sysl"    -> "module oskit.arch\nf() -> int = 1",
        "b.sysl"    -> "module oskit.vm\ng() -> int = 2",
        "main.sysl" -> "print(oskit.arch.f() + oskit.vm.g())",
      )

      out should include("define i32 @oskit.arch$f()")
      out should include("define i32 @oskit.vm$g()")
    }
  }

  "which one file of a program runs" - {
    // A declaration is hoisted and belongs to its module; a statement runs, and running has an
    // order that neither files nor modules have. So one file carries them (`reference/modules.md § Where a program starts`).
    "is a question with one answer" in {
      errOf(
        "a.sysl" -> "print(1)",
        "b.sysl" -> "print(2)",
      ) should include("a.sysl already carries the statements this program runs, so b.sysl may hold declarations only")
    }

    "across modules just as within one" in {
      errIn(
        ("", "a.sysl", "print(1)"),
        ("m", "b.sysl", "module m\nprint(2)"),
      ) should include("may hold declarations only")
    }

    // But NOT counting a top-level binding as one of them. A `var` beside a file that really does
    // carry statements is module storage (`reference/modules.md § val — a thing`), so it is not a second beginning and the two
    // files compile together — which is the whole of the mutable-module-storage rule seen from this
    // side.
    "though a binding beside them is module storage rather than a second beginning" in {
      runIn(
        ("", "main.sysl", "counter.bump()\ncounter.bump()\nprint(str(counter.count))"),
        ("counter", "c.sysl", "module counter\n\nvar count: int = 0\n\nbump() = count += 1"),
      ) shouldBe "2\n"
    }

    // And the same where the sibling has no header, so nothing but the other file's statements
    // distinguishes them. This is the discriminating half: it is not the `module` line that makes
    // the binding storage, it is that the beginning is somewhere else.
    "with no header on the sibling either, since it is the statements that decide" in {
      runOf(
        "a.sysl" -> "bump()\nprint(str(x))",
        "b.sysl" -> "var x: int = 2\n\nbump() = x += 1",
      ) shouldBe "3\n"
    }

    // Where NOTHING runs there is no beginning for a second to compete with, so several files of
    // bindings are several files of module storage rather than a mistake. A program that does
    // nothing is a library, and a library is not an error.
    "while several files of bindings and nothing else is a library, not a mistake" in {
      runOf(
        "a.sysl" -> "var x: int = 1",
        "b.sysl" -> "var y: int = 2",
      ) shouldBe ""
    }

    "though a file of nothing but declarations is never the one that runs" in {
      runOf(
        "a.sysl" -> "f() -> int = 1",
        "b.sysl" -> "g() -> int = 2",
        "c.sysl" -> "print(f() + g())",
      ) shouldBe "3\n"
    }

    // The statements run in the module that carried them, so an unqualified name in them is that
    // module's — the entry point is a body like any other in that respect.
    "and the statements read their names in the module that wrote them" in {
      runIn(
        ("m", "a.sysl", "module m\nf() -> int = 4\nprint(f())"),
      ) shouldBe "4\n"
    }
  }

  /** Mutable module storage in a file that is not the entry file (`reference/modules.md § val — a thing`).
    *
    * It is the same declaration `static var` is, and the two spellings exist because the entry file
    * is the one place a top-level `var` has a body to be a local of: there the modifier asks for the
    * module instead, and everywhere else there is nothing to ask. So the rules below are `static
    * var`'s rules, tested through the other spelling — a type is mandatory, an initializer is not,
    * and the type may owe no release.
    */
  "a 'var' outside the entry file is the module's storage" - {
    "so another module reads it by its qualified name" in {
      runIn(
        ("", "main.sysl", "print(str(counter.count))"),
        ("counter", "c.sysl", "module counter\n\nvar count: int = 7"),
      ) shouldBe "7\n"
    }

    // Two files of one module share one set of declarations (`reference/modules.md`), so the
    // storage is reachable from a sibling exactly as a `val` would be — which a local of the entry
    // point never was.
    "and a sibling file of the same module writes it" in {
      runIn(
        ("", "main.sysl", "m.bump()\nm.bump()\nm.bump()\nprint(str(m.n))"),
        ("m", "a.sysl", "module m\n\nvar n: int = 0"),
        ("m", "b.sysl", "module m\n\nbump() = n += 1"),
      ) shouldBe "3\n"
    }

    // A headerless file that is not the one carrying the statements is the anonymous root module,
    // which is a module like any other — so this needs no rule of its own.
    "including a headerless file that is not the one the program starts in" in {
      runIn(
        ("", "main.sysl", "bump()\nprint(str(n))"),
        ("", "other.sysl", "var n: int = 5\n\nbump() = n += 1"),
      ) shouldBe "6\n"
    }

    // The cheapest form, and the reason the type is mandatory rather than merely conventional:
    // there may be no value anywhere for one to be inferred from.
    "its initializer may be absent, and the type's zero is what it starts at" in {
      runIn(
        ("", "main.sysl", "print(str(m.slot))"),
        ("m", "a.sysl", "module m\n\nvar slot: int"),
      ) shouldBe "0\n"
    }

    "so the type is mandatory" in {
      errIn(
        ("", "main.sysl", "print(str(m.n))"),
        ("m", "a.sysl", "module m\n\nvar n = 1"),
      ) should include("module storage states its type")
    }

    // The diagnostic must not name `static`, which is refused in this very file — a reader told to
    // write it would be told something false. This is the discriminating half of the test above.
    "and the diagnostic does not name a spelling this file refuses" in {
      errIn(
        ("", "main.sysl", "print(str(m.n))"),
        ("m", "a.sysl", "module m\n\nvar n = 1"),
      ) should not include "static"
    }

    // It may hold a counted value. The last one it holds is never released, which is what a static
    // is — and this pair was refused outright until the rule was read again (`reference/modules.md § val — a thing`).
    "and it may hold a value that owes a release" in {
      runIn(
        ("", "main.sysl", "print(m.greeting)"),
        ("m", "a.sysl", "module m\n\nvar greeting: string = \"hello\""),
      ) shouldBe "hello\n"
    }

    // A **slice** is the same answer, and it is worth asking separately because it is where the
    // question is least obvious: a `[]const u8` reads as a borrowed view of somebody else's bytes,
    // and a rule tested only through `string` would not say whether the count travels with the view.
    "which a slice may as much as a string, however much it reads like a view" in {
      runIn(
        ("", "main.sysl", "print(str(m.name.len))"),
        ("m", "a.sysl", "module m\n\nvar name: []const u8 = \"abc\".bytes"),
      ) shouldBe "3\n"
    }

    // The half that has to work for the rest to be worth anything: a value **built while the program
    // runs**, which is what the old rule refused by name. Its initializer is code, so it runs in the
    // prologue, and the storage takes a count of its own for it — without which the temporary would
    // be released at the end of the initializer's own region and the global left holding freed bytes.
    "including one built while the program runs" in {
      runIn(
        ("", "main.sysl", "print(m.label)"),
        ("m", "a.sysl", "module m\n\nvar label: string = \"n=\" + str(6 * 7)"),
      ) shouldBe "n=42\n"
    }

    // Reassignment is where the release *does* have a line to write itself on, and it is the whole
    // reason the old rule was too broad. Three stores, and what each replaces is let go of.
    "and reassigning one lets go of what was there" in {
      runIn(
        ("", "main.sysl", "m.set(str(1))\nm.set(str(2))\nprint(m.get())"),
        ("m", "a.sysl", "module m\n\nvar cur: string = \"\"\n\nset(s: string) = cur = s\n\nget() -> string = cur"),
      ) shouldBe "2\n"
    }

    // A module `val` answers the same, and it is the declaration the old rule bit hardest: a `val`
    // was admitted only when its initializer was a constant tree, so a table of strings was fine and
    // one string made out of two was not.
    "as may a 'val', whose value is built the same way" in {
      runIn(
        ("", "main.sysl", "print(m.banner)"),
        ("m", "a.sysl", "module m\n\nval banner: string = \"v\" + str(1) + \".\" + str(0)"),
      ) shouldBe "v1.0\n"
    }

    // The narrow refusal that replaces the broad one, and it is about a different thing: storage with
    // no initializer starts at its type's zero, and a `&T` has none. Note it is `hasZero` — the same
    // question a **local** with no initializer is already held to — rather than a rule of its own.
    "though one with no zero must be given a value" in {
      errIn(
        ("", "main.sysl", "print(str(m.cell.v))"),
        ("m", "a.sysl", "module m\n\nstruct Cell\n    v: int\n\nvar cell: &Cell"),
      ) should include("needs a value")
    }

    // The other side of it: given a value, the same declaration is fine. Without this the test above
    // would pass just as well against a compiler that refused every reference.
    "and is fine once it has one" in {
      runIn(
        ("", "main.sysl", "print(str(m.cell.v))"),
        ("m", "a.sysl", "module m\n\nstruct Cell\n    v: int\n\nvar cell: &Cell = Cell(7)"),
      ) shouldBe "7\n"
    }

    // The shape this was relaxed **for**: a boxed callable kept where something else can find it
    // again. Every C interface that calls back takes a function pointer and an opaque word, so a
    // binding that wants to offer a sysl closure has to put it somewhere that outlives the call — and
    // module storage is the only storage that does. Written as `Option` because the slot is empty
    // before anything registers and empty again after it has run, which is also what makes the enum
    // need its `= None`: an enum has no zero.
    "which is what lets a callable be kept where something else finds it again" in {
      runIn(
        ("", "main.sysl", "m.on_done(n -> print(str(n * 2)))\nm.fire(21)\nm.fire(1)"),
        (
          "m",
          "a.sysl",
          """module m
            |
            |private var pending: Option[&Fn(int) -> unit] = None
            |
            |on_done(f: &Fn(int) -> unit) = pending = Some(f)
            |
            |fire(n: int)
            |    pending match
            |        Some(f) ->
            |            f(n)
            |            pending = None
            |        None -> print("nothing registered")""".stripMargin,
        ),
      ) shouldBe "42\nnothing registered\n"
    }

    // Asserted against the visibility diagnostic rather than against there being *a* diagnostic,
    // which is what this used to check. `private var` did not parse, so the test passed on
    // "identifier expected" — a parse error about the line, standing in for a rule about reach.
    "while 'private' keeps it inside the file that declares it" in {
      errIn(
        ("", "main.sysl", "print(str(m.n))"),
        ("m", "a.sysl", "module m\n\nprivate var n: int = 1"),
      ) should include("'m.n' is private to 'a.sysl', the file that declares it")
    }

    // The other half of that, and the half a parse error could never have shown: the file that
    // declared it uses it freely, so the modifier restricts rather than refuses.
    "though the file that declares it reads and writes it as before" in {
      runIn(
        ("", "main.sysl", "print(str(m.peek()))"),
        ("m", "a.sysl", "module m\n\nprivate var n: int = 1\n\npeek() -> int\n    n += 1\n    n"),
      ) shouldBe "2\n"
    }

    // It reaches the rest of the machinery through the same table `static var` does, so the checks
    // stated over module storage answer for it without being restated. A `@pure` function reading
    // one (`reference/verification.md § @pure`) is the cheapest proof of that.
    "and a '@pure' function may not read one, as of any module storage" in {
      errIn(
        ("", "main.sysl", "print(str(m.peek()))"),
        ("m", "a.sysl", "module m\n\nvar seed: int = 1\n\n@pure\npeek() -> int = seed"),
      ) should not be empty
    }

    // `static` is still refused here, and this is what keeps the two spellings from being two ways
    // to say one thing in one place: it asks for the module instead of the *body*, and a file with a
    // header has no body for it to be asking about.
    "though 'static' is still refused there, having nothing to ask for" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "a.sysl", "module m\n\nstatic var n: int = 0"),
      ) should include("this file has no such body")
    }

    // The other half of the rule, and the one that could regress silently: the entry file's own
    // top-level `var` is STILL a local of its body, so it is not the module's and a sibling file
    // cannot reach it. Without this, making a `var` a declaration everywhere would look correct.
    "while the entry file's own 'var' stays a local, invisible to a sibling" in {
      errIn(
        ("", "main.sysl", "var n: int = 1\nprint(str(peek()))"),
        ("", "other.sysl", "peek() -> int = n"),
      ) should not be empty
    }

    // `reference/modules.md § val — a thing` gives module storage written with `var` the two things the word already means:
    // assignment at every depth, and `&`. The second is what a `val` is refused, so it is the
    // discriminating one.
    "and its address may be taken, which a module 'val' is refused" in {
      runIn(
        ("", "main.sysl", "m.bump()\nprint(str(m.k[0]))"),
        ("m", "a.sysl", "module m\n\nvar k: [2]int = [1, 2]\n\nbump()\n    var p: *int = &k[0]\n    *p = 9\nend bump"),
      ) shouldBe "9\n"
    }

    // It joins the same initializer dependency graph a `val` is in, so a cycle between two of them
    // is the cycle diagnostic rather than a value read before it was stored.
    "and a cycle between two of them is reported as one" in {
      errIn(
        ("", "main.sysl", "print(str(m.a))"),
        ("m", "x.sysl", "module m\n\nvar a: int = b + 1\n\nvar b: int = a + 1"),
      ) should not be empty
    }

    // The value namespace is shared (`reference/modules.md § Visibility`), so a `var` and a `val`
    // of one name in one module are the collision they are — reported at whichever was written
    // second.
    "and it shares the value namespace with a 'val' of the same name" in {
      errIn(
        ("", "main.sysl", "print(str(m.n))"),
        ("m", "x.sysl", "module m\n\nvar n: int = 1"),
        ("m", "y.sysl", "module m\n\nval n: int = 2"),
      ) should include("already")
    }

    // A third file holding only a binding is not a rival to either of two that really do carry
    // statements, so the two-beginnings report names those two and says nothing about it. This is
    // what the first pass being over non-bindings buys, and it would be easy to lose.
    "while a binding beside two real beginnings is not named as a third" in {
      val e = errOf(
        "a.sysl" -> "print(1)",
        "b.sysl" -> "print(2)",
        "c.sysl" -> "var x: int = 3",
      )

      e should include("b.sysl may hold declarations only")
      e should not include "c.sysl"
    }
  }

  "what one scope means for a mistake" - {
    "a name declared twice across two files is declared twice" in {
      errOf(
        "a.sysl" -> "f() -> int = 1",
        "b.sysl" -> "f() -> int = 2",
      ) should include("already declared")
    }

    "a type declared twice across two files is too" in {
      errOf(
        "a.sysl" -> "struct P\n    x: int",
        "b.sysl" -> "struct P\n    y: int",
      ) should include("already declared")
    }

    "and one implementation per type holds across files" in {
      errOf(
        "t.sysl" -> "trait Show\n    show(self) -> int",
        "p.sysl" -> "struct P\n    x: int",
        "a.sysl" -> "impl Show for P\n    show(self) -> int = 1",
        "b.sysl" -> "impl Show for P\n    show(self) -> int = 2",
      ) should include("Show")
    }

    // Diagnostics sort by file and then by line, so reading them top to bottom is reading the
    // module top to bottom rather than following the order the driver happened to collect it in.
    "an error in each file is reported against that file" in {
      val out = errOf(
        "b.sysl" -> "g() -> int = nope",
        "a.sysl" -> "f() -> int = missing",
      )

      out should include("a.sysl")
      out should include("b.sysl")
      out.indexOf("a.sysl") should be < out.indexOf("b.sysl")
    }

    "a syntax error in one file does not hide one in another" in {
      val out = errOf(
        "a.sysl" -> "f() -> int = (",
        "b.sysl" -> "g() -> int = )",
      )

      out should include("a.sysl")
      out should include("b.sysl")
    }
  }

  "the emitted module is one module" - {
    "whatever it was written across" in {
      val out = irOf(
        "main.sysl" -> "print(double(21))",
        "math.sysl" -> "double(n: int) -> int = n * 2",
      )

      out should include("define i32 @double(i32 %n.param)")
      out should include("define i32 @main(")
    }

    "and holds exactly what the single-file spelling holds" in {
      val split = irOf(
        "main.sysl" -> "print(double(21))",
        "math.sysl" -> "double(n: int) -> int = n * 2",
      )

      split shouldBe ir("double(n: int) -> int = n * 2\nprint(double(21))")
    }
  }
}
