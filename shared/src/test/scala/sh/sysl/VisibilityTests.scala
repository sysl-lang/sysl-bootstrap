package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `private` and `private[M]` — how far a declaration may be named from (`reference/modules.md §
 * Visibility`).
 *
 * Public is the unmarked default, so every case here is about what a modifier takes away. The two
 * levels are one keyword and its argument: a bare `private` is the **file**, and `private[M]`
 * widens to a module the declaration is already inside and everything beneath it.
 */
class VisibilityTests extends AnyFreeSpec with CodegenSupport with RunSupport with ParseSupport {

  // A module in two files, one of which keeps a helper to itself. Every case below differs only in
  // where it tries to name `scale` from.
  private val hidden =
    ("geom", "g.sysl",
     """module geom
       |private scale(n: int) -> int = n * 2
       |twice(n: int) -> int = scale(n)
       |""".stripMargin)

  // A stand-in standard module keeping one member to itself, for the cases below that are about the
  // library's own step in resolution. The real library declares nothing private, so it cannot pose
  // the question at all.
  private val lib =
    ("std.sysl",
     """module sysl
       |private[sysl] carry(n: int) -> int = n * 2
       |twice(n: int) -> int = carry(n)
       |""".stripMargin)

  "the modifiers parse" - {
    "a bare 'private' is the file" in {
      prog("private f() -> int = 1") shouldBe
        List(FuncDecl("f", Nil, Nil, Some(NamedType("int")), List(ExprStmt(i(1))), vis = Visibility.File))
    }

    "'private[M]' carries the name as written" in {
      prog("private[geom] f() -> int = 1") shouldBe
        List(FuncDecl("f", Nil, Nil, Some(NamedType("int")), List(ExprStmt(i(1))), vis = Visibility.Scoped("geom")))
    }

    "an unmarked declaration is public" in {
      prog("f() -> int = 1") shouldBe
        List(FuncDecl("f", Nil, Nil, Some(NamedType("int")), List(ExprStmt(i(1)))))
    }

    "a struct takes one" in {
      prog("private struct P\n    x: int\nend P") shouldBe
        List(StructDecl("P", Nil, List(Param("x", NamedType("int"))), vis = Visibility.File))
    }

    "an enum takes one" in {
      prog("private[a] enum E\n    A\nend E") shouldBe
        List(EnumDecl("E", Nil, None, List(EnumVariantDecl("A", None, Nil)), vis = Visibility.Scoped("a")))
    }

    "a trait takes one" in {
      prog("private trait T\n    show(self) -> int\nend T") shouldBe
        List(TraitDecl("T", Nil,
          List(MethodDecl("show", Some(RecvMode.ByValue), false, Nil, Nil, Some(NamedType("int")), Nil)),
          vis = Visibility.File))
    }

    "an extern takes one" in {
      prog("private extern abs(n: int) -> int") shouldBe
        List(ExternDecl("abs", List(Param("n", NamedType("int"))), Some(NamedType("int")), vis = Visibility.File))
    }

    // A `var` at the top of a file that names a module is that module's storage and is the same
    // declaration `static var` spells in the entry file (`reference/modules.md § Where a program starts`), so it takes a modifier for the
    // same reason the `val` beside it does. It did not parse at all until the form was added to
    // `declaration`, and what a reader got was "identifier expected" pointing at the `private` —
    // which reads as a missing name rather than as a form that takes no modifier.
    "and so does a 'var', which is module storage outside the entry file" in {
      prog("private var n: int = 1") shouldBe
        List(VarDecl("n", Some(NamedType("int")), Some(i(1)), vis = Visibility.File))
    }

    "with the scoped spelling too" in {
      prog("private[m] var n: int") shouldBe
        List(VarDecl("n", Some(NamedType("int")), None, vis = Visibility.Scoped("m")))
    }

    "while an unmarked one is public, exactly as it was" in {
      prog("var n: int = 1") shouldBe List(VarDecl("n", Some(NamedType("int")), Some(i(1))))
    }

    // The argument is a simple name, not a path: a visibility scope is always an enclosing module,
    // and there is no way to name an unrelated one (`reference/modules.md § Visibility`). The
    // refusal says so, at the first dot, rather than `']' expected` — which reads as a bracket left
    // open — and offers the last segment, the one innermost-outward resolution would have matched.
    "but the scope argument is one segment, not a path" - {
      "two segments" in {
        val out = progError("module geo.flat\nprivate[geo.flat] helper() -> int = 1\n")
        out should include("names one enclosing module by its simple name rather than by a path")
        out should include("write 'private[flat]'")
        out should not include "']' expected"
        out should include("<input>:2:12")
      }

      "three segments, where the offer is still the last" in {
        val out = progError("module a.b.c\nprivate[a.b.c] struct S\n    n: int\n")
        out should include("write 'private[c]'")
        out should include("<input>:2:10")
      }

      "and in front of an 'impl', which reads the modifier only to refuse it" in {
        progError("private[a.b] impl T for S\n") should include("write 'private[b]'")
      }
    }
  }

  "a bare 'private' is the file that declares it" - {
    "which may use it freely" in {
      runIn(("", "main.sysl", "print(geom.twice(21))"), hidden) shouldBe "42\n"
    }

    "a sibling file of its own module may not" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        hidden,
        ("geom", "h.sysl", "module geom\nquad(n: int) -> int = scale(scale(n))"),
      ) should include("'geom.scale' is private to 'g.sysl', the file that declares it")
    }

    "another module may not name it in full" in {
      errIn(("", "main.sysl", "print(geom.scale(21))"), hidden) should
        include("'geom.scale' is private to 'g.sysl', the file that declares it")
    }

    "and importing it is refused at the import" in {
      errIn(("", "main.sysl", "import geom.scale\nprint(1)"), hidden) should
        include("'geom.scale' is private to 'g.sysl', the file that declares it")
    }

    "including a selector list, which points at the selector" in {
      errIn(("", "main.sysl", "import geom.{twice, scale}\nprint(1)"), hidden) should
        include("'geom.scale' is private to 'g.sysl'")
    }

    // A wildcard offers what it can see, so a private helper is not among what it brings in — which
    // is a different answer from being told the import cannot have it.
    "a wildcard does not offer it at all" in {
      errIn(("", "main.sysl", "import geom.*\nprint(scale(21))"), hidden) should
        include("undefined function 'scale'")
    }

    "so it cannot make a name from a second wildcard ambiguous" in {
      runIn(
        ("", "main.sysl", "import geom.*\nimport text.*\nprint(scale(21))"),
        hidden,
        ("text", "t.sysl", "module text\nscale(n: int) -> int = n + 1"),
      ) shouldBe "22\n"
    }
  }

  /** **A file-private name is scoped to its file, and a public one is not** (`reference/modules.md
   * § Visibility`).
   *
   * This section pinned the opposite until 2026-08-27 — a file was a visibility level and not a
   * namespace at all, so `private` restricted the *reach* of a name without restricting the
   * *namespace*, and a sibling file could not declare its own `scale`. Card `0306`: that defeated
   * what file-privacy is for, since the reason to keep a helper to its file is that its name is a
   * local matter. Rust, C and Go all scope the name as well as the reach.
   *
   * **And since 0.0.151 a private declaration shadows a sibling's public one** rather than colliding
   * with it: the private file's references mean its own, every other file's the public one. This
   * section asserted the collision until then; `FilePrivateNameTests` carries the rule in full.
   */
  "a file scopes a private name, and only a private one" - {
    "a private declaration shadows a sibling file's public one, in its own file only" in {
      runIn(
        ("", "main.sysl", "print(geom.from_g(), geom.from_h(), geom.scale(10))"),
        ("geom", "g.sysl", "module geom\nprivate scale(n: int) -> int = n * 2\nfrom_g() -> int = scale(10)"),
        ("geom", "h.sysl", "module geom\nscale(n: int) -> int = n + 1\nfrom_h() -> int = scale(10)"),
      ) shouldBe "20 11 11\n"
    }

    // The half that moved. Two files that each keep a `scale` to themselves have written two
    // declarations no call site can confuse, since neither is ever a candidate where the other is.
    "but a second file's private one is its own, and both are named from their own file" in {
      runIn(
        ("", "main.sysl", "print(geom.from_g(), geom.from_h())"),
        ("geom", "g.sysl",
         "module geom\nprivate scale(n: int) -> int = n * 2\nfrom_g() -> int = scale(10)"),
        ("geom", "h.sysl",
         "module geom\nprivate scale(n: int) -> int = n + 1\nfrom_h() -> int = scale(10)"),
      ) shouldBe "20 11\n"
    }

    "and two modules may each keep one of the same name" in {
      runIn(
        ("", "main.sysl", "print(geom.twice(10) + text.twice(10))"),
        ("geom", "g.sysl", "module geom\nprivate scale(n: int) -> int = n * 2\ntwice(n: int) -> int = scale(n)"),
        ("text", "t.sysl", "module text\nprivate scale(n: int) -> int = n * 3\ntwice(n: int) -> int = scale(n)"),
      ) shouldBe "50\n"
    }

    // The two of them land in one LLVM module, so what keeps them apart is the module segment in
    // the symbol and nothing else. This is the case that decides `reference/modules.md §
    // Visibility`'s claim about mangling: drop the segment for a file-private name and these two
    // definitions collide.
    "which they can only do because the symbol still carries the module" in {
      val out = irIn(
        ("", "main.sysl", "print(geom.twice(10) + text.twice(10))"),
        ("geom", "g.sysl", "module geom\nprivate scale(n: int) -> int = n * 2\ntwice(n: int) -> int = scale(n)"),
        ("text", "t.sysl", "module text\nprivate scale(n: int) -> int = n * 3\ntwice(n: int) -> int = scale(n)"),
      )

      out should include("@geom$scale(")
      out should include("@text$scale(")
      out should not include "@scale("
    }

    // A collision is one mistake, so it is one diagnostic. The losing declaration is still
    // registered under the name, so without care the file that wrote it is then told the name
    // belongs to another declaration — a name it declares itself, three lines up. What still
    // collides is one file declaring the spelling twice; a public declaration in another file
    // must not turn that into a second complaint either.
    "and the file that loses the collision is not also told the name is not its own" in {
      val out = errIn(
        ("", "main.sysl", "print(1)"),
        ("geom", "g.sysl", "module geom\nscale(n: int) -> int = n * 3"),
        ("geom", "h.sysl",
         "module geom\nscale(n: int) -> int = n * 2\nprivate scale(n: int) -> int = n + 1\n" +
           "twice(n: int) -> int = scale(n)"),
      )

      out should include("function 'scale' is already declared")
      out should not include "private to"
      out should not include "ambiguous"
    }
  }

  /** What the file level buys the backend (`reference/modules.md § Visibility`).
   *
   * A bare `private` is the one reach that provably never crosses a file boundary, and every file of
   * a compilation is emitted into one LLVM module — so a declaration at that reach has all of its
   * callers in the module that defines it, which is what `internal` linkage states. Nothing wider
   * qualifies: a `private[M]` reaches a whole subtree, and public reaches anyone.
   */
  "a file-private declaration is internal to the module that defines it" - {
    "a bare 'private' function" in {
      irIn(
        ("", "main.sysl", "print(geom.twice(10))"),
        ("geom", "g.sysl", "module geom\nprivate scale(n: int) -> int = n * 2\ntwice(n: int) -> int = scale(n)"),
      ) should include("define internal i32 @geom$scale(")
    }

    "while the public one beside it keeps external linkage" in {
      irIn(
        ("", "main.sysl", "print(geom.twice(10))"),
        ("geom", "g.sysl", "module geom\nprivate scale(n: int) -> int = n * 2\ntwice(n: int) -> int = scale(n)"),
      ) should include("define i32 @geom$twice(")
    }

    // The discriminating half: a scoped-private declaration is reachable from every file of the
    // subtree, so its reach is not the file and the linkage is unchanged.
    "a 'private[M]' one does not qualify, however narrow the subtree" in {
      val out = irIn(
        ("", "main.sysl", "print(geom.twice(10))"),
        ("geom", "g.sysl", "module geom\nprivate[geom] scale(n: int) -> int = n * 2"),
        ("geom", "h.sysl", "module geom\ntwice(n: int) -> int = scale(n)"),
      )

      out should include("define i32 @geom$scale(")
      out should not include "define internal i32 @geom$scale("
    }

    // `reference/modules.md § Visibility`: an unmarked member sits at its type's reach, so a member
    // of a file-private type is file-private without saying so.
    "an unmarked member of a file-private type" in {
      ir("""private struct Hidden
           |    n: int
           |    scale(self, k: int) -> int = self.n * k
           |end Hidden
           |private val h: Hidden = Hidden(3)
           |print(h.scale(2))
           |""".stripMargin) should include("define internal i32 @Hidden.scale(")
    }

    "a private member of a public type" in {
      val out = ir("""struct Shown
                     |    n: int
                     |    private secret(self) -> int = self.n + 1
                     |    visible(self) -> int = self.secret()
                     |end Shown
                     |val s: Shown = Shown(4)
                     |print(s.visible())
                     |""".stripMargin)

      out should include("define internal i32 @Shown.secret(")
      out should include("define i32 @Shown.visible(")
    }

    // An instantiation is emitted under a name of its own — `geom$pick.int` — and that name has no
    // visibility of its own, because it is not what anyone wrote. The answer comes from the
    // declaration it was made from, or a private generic helper in a library would go on exporting
    // one symbol per instantiation.
    "an instantiation of a file-private generic, which is emitted under another name" in {
      val out = irIn(
        ("", "main.sysl", "print(geom.larger(3, 9))"),
        ("geom", "g.sysl",
         "module geom\nprivate pick[T: Ord](a: T, b: T) -> T = if a < b then b else a\n" +
           "larger(a: int, b: int) -> int = pick(a, b)"),
      )

      out should include("define internal i32 @geom$pick.int(")
      out should include("define i32 @geom$larger(")
    }

    // The entry point is the one symbol the platform resolves, so it stays external whatever the
    // program said about the function behind it — which is a different symbol, and does not.
    "a private 'main', whose own symbol is not the entry point" in {
      val out = ir("private main() -> unit = print(\"hi\")\n")

      out should include("define internal void @$$main(")
      out should include("define i32 @main(i32 %argc, ptr %argv)")
    }

    "and it still runs" in {
      run("private main() -> unit = print(\"hi\")\n") shouldBe "hi\n"
    }

    "and the program still runs" in {
      runIn(
        ("", "main.sysl", "print(geom.twice(10))"),
        ("geom", "g.sysl", "module geom\nprivate scale(n: int) -> int = n * 2\ntwice(n: int) -> int = scale(n)"),
      ) shouldBe "20\n"
    }
  }

  "'private[M]' widens to a module and its subtree" - {
    "every file of the declaring module may use it" in {
      runIn(
        ("", "main.sysl", "print(geom.quad(5))"),
        ("geom", "g.sysl", "module geom\nprivate[geom] scale(n: int) -> int = n * 2"),
        ("geom", "h.sysl", "module geom\nquad(n: int) -> int = scale(scale(n))"),
      ) shouldBe "20\n"
    }

    "but a module outside it may not" in {
      errIn(
        ("", "main.sysl", "print(geom.scale(5))"),
        ("geom", "g.sysl", "module geom\nprivate[geom] scale(n: int) -> int = n * 2"),
      ) should include("'geom.scale' is private to module 'geom'")
    }

    "an ancestor reaches every module beneath it" in {
      runIn(
        ("", "main.sysl", "print(oskit.mm.reserve(3))"),
        ("oskit.arch", "cpu.sysl", "module oskit.arch\nprivate[oskit] frames(n: int) -> int = n * 4"),
        ("oskit.mm", "mm.sysl", "module oskit.mm\nreserve(n: int) -> int = oskit.arch.frames(n)"),
      ) shouldBe "12\n"
    }

    "and the ancestor itself" in {
      runIn(
        ("", "main.sysl", "print(oskit.pages(3))"),
        ("oskit.arch", "cpu.sysl", "module oskit.arch\nprivate[oskit] frames(n: int) -> int = n * 4"),
        ("oskit", "k.sysl", "module oskit\npages(n: int) -> int = oskit.arch.frames(n)"),
      ) shouldBe "12\n"
    }

    "but nothing outside that subtree" in {
      errIn(
        ("", "main.sysl", "print(oskit.arch.frames(3))"),
        ("oskit.arch", "cpu.sysl", "module oskit.arch\nprivate[oskit] frames(n: int) -> int = n * 4"),
      ) should include("'oskit.arch.frames' is private to module 'oskit'")
    }

    // Read outward from the declaration, first hit winning, so the nearer `geom` is the one meant.
    "a repeated segment binds to the innermost one" in {
      errIn(
        ("", "main.sysl", "print(geom.reach(1))"),
        ("geom.mesh.geom.tri", "t.sysl", "module geom.mesh.geom.tri\nprivate[geom] area(n: int) -> int = n * 2"),
        ("geom", "g.sysl", "module geom\nreach(n: int) -> int = geom.mesh.geom.tri.area(n)"),
      ) should include("is private to module 'geom.mesh.geom'")
    }

    "and the module the declaration is in counts as enclosing itself" in {
      runIn(
        ("", "main.sysl", "print(a.b.go(4))"),
        ("a.b", "x.sysl", "module a.b\nprivate[b] step(n: int) -> int = n + 1"),
        ("a.b", "y.sysl", "module a.b\ngo(n: int) -> int = step(n)"),
      ) shouldBe "5\n"
    }
  }

  "a scope that names nothing" - {
    "a module the declaration is not inside" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("geom", "g.sysl", "module geom\nprivate[text] scale(n: int) -> int = n * 2"),
        ("text", "t.sysl", "module text\nwrap(n: int) -> int = n"),
      ) should include("'text' is not 'geom' or one of its ancestors")
    }

    "a descendant of it, which is inside-out" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("oskit", "k.sysl", "module oskit\nprivate[arch] pages(n: int) -> int = n"),
        ("oskit.arch", "cpu.sysl", "module oskit.arch\nframes(n: int) -> int = n"),
      ) should include("'arch' is not 'oskit' or one of its ancestors")
    }

    "a file at the project root, whose module has no name" in {
      errIn(("", "main.sysl", "private[root] scale(n: int) -> int = n * 2\nprint(1)")) should
        include("this file is at the project root, whose module has no name")
    }

    // The declaration still exists: one mistake is one diagnostic, and the uses of a name whose
    // scope could not be worked out are not each a second complaint about it.
    "and the declaration it was written on still stands" in {
      errIn(("", "main.sysl", "private[root] scale(n: int) -> int = n * 2\nprint(scale(21))")) should
        not include "undefined function 'scale'"
    }
  }

  "every declaration form takes one" - {
    "a struct" in {
      errIn(
        ("", "main.sysl", "var p: geom.Point = geom.Point(1, 2)\nprint(p.x)"),
        ("geom", "g.sysl", "module geom\nprivate struct Point\n    x: int\n    y: int"),
      ) should include("'geom.Point' is private to 'g.sysl'")
    }

    "an enum" in {
      errIn(
        ("", "main.sysl", "var s: geom.Shape = geom.Shape.Dot\nprint(1)"),
        ("geom", "g.sysl", "module geom\nprivate enum Shape\n    Dot\n    Round"),
      ) should include("'geom.Shape' is private to 'g.sysl'")
    }

    "and its variants with it" in {
      errIn(
        ("", "main.sysl", "import geom.Dot\nprint(1)"),
        ("geom", "g.sysl", "module geom\nprivate enum Shape\n    Dot\n    Round"),
      ) should include("'geom.Dot' is private to 'g.sysl'")
    }

    "a trait" in {
      errIn(
        ("", "main.sysl", "loud[T: geom.Show](x: T) -> int = x.show()\nprint(1)"),
        ("geom", "g.sysl", "module geom\nprivate trait Show\n    show(self) -> int"),
      ) should include("'geom.Show' is private to 'g.sysl'")
    }

    "an extern" in {
      errIn(
        ("", "main.sysl", "print(geom.abs(0 - 3))"),
        ("geom", "g.sysl", "module geom\nprivate extern abs(n: int) -> int"),
      ) should include("'geom.abs' is private to 'g.sysl'")
    }

    "a function" in {
      errIn(("", "main.sysl", "print(geom.scale(21))"), hidden) should include("'geom.scale' is private")
    }

    // `reference/modules.md § Visibility` gives the exception a reason of its own, so the refusal
    // has to carry the reason rather than the grammar's complaint that the modifier was not
    // followed by a name. This is the same refusal the members *inside* an `impl` already get.
    "and an 'impl' does not, having no name for one to restrict" in {
      val src =
        """trait Show
          |    show(self) -> int
          |
          |struct P
          |    v: int
          |
          |private impl Show for P
          |    show(self) -> int = self.v
          |
          |print(P(1).show())""".stripMargin

      err(src) should include("an 'impl' block carries no visibility of its own")
      err(src) should not include "identifier expected"
      err(src.replace("private impl", "private[geom] impl")) should
        include("an 'impl' block carries no visibility of its own")
    }
  }

  "what a restriction does not touch" - {
    // An import binds a shorter spelling; visibility decides what may be spelled at all. A public
    // wrapper over a private helper is the whole point of the level, and it keeps working.
    "a public wrapper reaches its own module's private helper" in {
      runIn(
        ("", "main.sysl", "import geom.twice\nprint(twice(21))"),
        hidden,
      ) shouldBe "42\n"
    }

    "a private type is still a type its own file may use" in {
      runIn(
        ("", "main.sysl", "print(geom.area())"),
        ("geom", "g.sysl",
         "module geom\nprivate struct Point\n    x: int\n    y: int\narea() -> int = Point(3, 4).x * Point(3, 4).y"),
      ) shouldBe "12\n"
    }

    "a private trait may still be implemented and used inside its file" in {
      runIn(
        ("", "main.sysl", "print(geom.go())"),
        ("geom", "g.sysl",
         "module geom\nprivate trait Show\n    show(self) -> int\nstruct Tag\n    n: int\n" +
           "impl Show for Tag\n    show(self) -> int = self.n * 2\ngo() -> int = Tag(5).show()"),
      ) shouldBe "10\n"
    }
  }

  "an import inside a block is held to the same rule" in {
    errIn(
      ("", "main.sysl", "f() -> int =\n    import geom.scale\n    scale(21)\nprint(f())"),
      hidden,
    ) should include("'geom.scale' is private to 'g.sysl', the file that declares it")
  }

  // A name a file may not reach is not a candidate for it, so resolution goes on past it rather
  // than stopping there. Only where nothing else answers at all does the restriction get reported —
  // at which point it is the whole story, and a better one than an undefined name.
  "a name a file cannot reach does not stand in the way of one it can" - {
    "an explicit import beats a sibling file's private declaration of the same name" in {
      runIn(
        ("", "main.sysl", "print(geom.go())"),
        ("geom", "g.sysl", "module geom\nprivate width(n: int) -> int = n * 2"),
        ("geom", "h.sysl", "module geom\nimport text.width\ngo() -> int = width(21)"),
        ("text", "t.sysl", "module text\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "22\n"
    }

    "a wildcard does too" in {
      runIn(
        ("", "main.sysl", "print(geom.go())"),
        ("geom", "g.sysl", "module geom\nprivate width(n: int) -> int = n * 2"),
        ("geom", "h.sysl", "module geom\nimport text.*\ngo() -> int = width(21)"),
        ("text", "t.sysl", "module text\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "22\n"
    }

    "and so does one written inside a block" in {
      runIn(
        ("", "main.sysl", "print(geom.go())"),
        ("geom", "g.sysl", "module geom\nprivate width(n: int) -> int = n * 2"),
        ("geom", "h.sysl", "module geom\ngo() -> int =\n    import text.width\n    width(21)"),
        ("text", "t.sysl", "module text\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "22\n"
    }

    "the library still answers where only a private declaration would have" in {
      runIn(
        ("", "main.sysl", "print(geom.go())"),
        ("geom", "g.sysl", "module geom\nprivate print(n: int) -> int = n"),
        ("geom", "h.sysl", "module geom\ngo() -> int =\n    print(9)\n    1"),
      ) shouldBe "9\n1\n"
    }

    "while the declaring file goes on getting its own" in {
      runIn(
        ("", "main.sysl", "print(geom.go())"),
        ("geom", "g.sysl", "module geom\nprivate width(n: int) -> int = n * 2\ngo() -> int = width(21)"),
        ("text", "t.sysl", "module text\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "42\n"
    }
  }

  // The library is reached without an import and searched after everything else, which is what makes
  // it the one step where a restriction could go unasked: a program writes its members bare, and a
  // bare name that resolved is a name nothing questioned. These are asked of a stand-in standard
  // module because the real one declares nothing private, so the real one cannot pose the question.
  "a member the library keeps to itself" - {
    "is not what a program's bare name resolves to" in {
      errAgainst(lib)(
        "main.sysl" -> "carry(21)",
      ) should include("'sysl.carry' is private to module 'sysl'")
    }

    "and the qualified spelling says the same thing, which is the point" in {
      errAgainst(lib)(
        "main.sysl" -> "sysl.carry(21)",
      ) should include("'sysl.carry' is private to module 'sysl'")
    }

    "while what the library does offer is reached with no import at all" in {
      irAgainst(lib)(
        "main.sysl" -> "twice(21)",
      ) should include(s"call i32 @${Library.key("twice")}")
    }

    "and the library's own files go on reaching it" in {
      irAgainst(lib)(
        "main.sysl" -> "twice(21)",
      ) should include(s"call i32 @${Library.key("carry")}")
    }

    "a program may declare the name the library kept, and mean its own" in {
      irAgainst(lib)(
        "main.sysl" -> "carry(n: int) -> int = n + 1\ncarry(21)",
      ) should include("call i32 @carry")
    }

    "a file-private one is out of reach the same way" in {
      errAgainst(
        ("std.sysl", "module sysl\nprivate carry(n: int) -> int = n * 2\ntwice(n: int) -> int = carry(n)"),
      )(
        "main.sysl" -> "carry(21)",
      ) should include("private to 'std.sysl', the file that declares it")
    }

    "and a second library file may not reach that one either" in {
      errAgainst(
        ("a.sysl", "module sysl\nprivate carry(n: int) -> int = n * 2"),
        ("b.sysl", "module sysl\ntwice(n: int) -> int = carry(n)"),
      )(
        "main.sysl" -> "twice(21)",
      ) should include("private to 'a.sysl', the file that declares it")
    }

    // The tables are asked separately — a spelling may be a type in one module and a function in
    // another — so a restriction that held for one of them says nothing about the rest.
    "a type it keeps is out of reach the same way" in {
      errAgainst(
        ("std.sysl", "module sysl\nprivate[sysl] struct Cell\n    n: int"),
      )(
        "main.sysl" -> "f(c: Cell) -> int = c.n",
      ) should include("'sysl.Cell' is private to module 'sysl'")
    }

    "and so is a trait it keeps" in {
      errAgainst(
        ("std.sysl", "module sysl\nprivate[sysl] trait Carry\n    carry(self) -> int"),
      )(
        "main.sysl" -> "f[T: Carry](x: T) -> int = x.carry()",
      ) should include("'sysl.Carry' is private to module 'sysl'")
    }

    "and a variant of an enum it keeps, which is named without naming the enum" in {
      errAgainst(
        ("std.sysl", "module sysl\nprivate[sysl] enum Step\n    Go(n: int)\n    Stop"),
      )(
        "main.sysl" -> "f() -> int = 1\nvar s = Go(3)",
      ) should include("private to module 'sysl'")
    }

    "naming it in an import is refused where the import is written" in {
      errAgainst(lib)(
        "main.sysl" -> "import sysl.carry\ncarry(21)",
      ) should include("'sysl.carry' is private to module 'sysl'")
    }

    "and a wildcard over the library does not offer it" in {
      errAgainst(lib)(
        "main.sysl" -> "import sysl.*\ncarry(21)",
      ) should include("'sysl.carry' is private to module 'sysl'")
    }
  }

  "the rest of the surface it reaches" - {
    "a module brought in by name, whose member is private" in {
      errIn(
        ("", "main.sysl", "import text.util\nprint(util.width(4))"),
        ("text.util", "u.sysl", "module text.util\nprivate width(n: int) -> int = n + 1"),
      ) should include("'text.util.width' is private to 'u.sysl'")
    }

    // A directory holding only sub-directories is a module's parent without being a module itself,
    // and `private[M]` names an enclosing *path* — so it is a scope like any other.
    "an ancestor that holds no source of its own" in {
      runIn(
        ("", "main.sysl", "print(a.b.c.go())"),
        ("a.b.c", "c.sysl", "module a.b.c\nprivate[b] step(n: int) -> int = n + 1\ngo() -> int = step(4)"),
      ) shouldBe "5\n"
    }

    "which still keeps everything outside it out" in {
      errIn(
        ("", "main.sysl", "print(a.b.c.step(4))"),
        ("a.b.c", "c.sysl", "module a.b.c\nprivate[b] step(n: int) -> int = n + 1"),
      ) should include("'a.b.c.step' is private to module 'a.b'")
    }

    "a sibling module under the same ancestor may reach it, and one outside may not" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("a.b", "x.sysl", "module a.b\nprivate[a] step(n: int) -> int = n"),
        ("a.c", "y.sysl", "module a.c\ngo() -> int = a.b.step(1)"),
        ("d", "z.sysl", "module d\nreach() -> int = a.b.step(1)"),
      ) should include("'a.b.step' is private to module 'a'")
    }

    "a private declaration may name itself" in {
      runIn(
        ("", "main.sysl", "private down(n: int) -> int =\n    if n <= 0 then 0 else down(n - 1) + 1\nprint(down(5))"),
      ) shouldBe "5\n"
    }

    // Naming a trait is resolved twice — once where the bound is written, once by the
    // definition-time walk — and one unreachable name is one mistake, not two.
    "a private trait in a bound is reported once" in {
      val out = errIn(
        ("", "main.sysl", "loud[T: geom.Show](x: T) -> int = 1\nprint(1)"),
        ("geom", "g.sysl", "module geom\nprivate trait Show\n    show(self) -> int"),
      )

      out should include("'geom.Show' is private to 'g.sysl'")
      out.linesIterator.count(_.contains("is private to")) shouldBe 1
    }
  }

  // A restriction a signature could carry a value out of would hardly be one: another module could
  // hold a value of a type it cannot name, pass it on, and read its fields. So what a declaration
  // says about itself has to be as nameable as the declaration is.
  "a declaration may not be more visible than the types it names" - {
    "a public function's result" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |make() -> Point = Point(1)
          |print(1)
          |""".stripMargin)) should include(
        "'make' is public, but its result names 'Point', which is private to 'main.sysl', the file " +
          "that declares it — a declaration may not be more visible than the types it names")
    }

    "a public function's parameter" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |read(p: Point) -> int = p.x
          |print(1)
          |""".stripMargin)) should include("'read' is public, but parameter 'p' names 'Point'")
    }

    "and a type reached through a slice or a memory mode is named just as much" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |count(ps: []Point) -> int = 0
          |print(1)
          |""".stripMargin)) should include("'count' is public, but parameter 'ps' names 'Point'")
    }

    // A bound is part of what a caller has to satisfy, so a trait it cannot name leaves it unable to
    // say what the declaration is asking of it.
    "a bound naming a private trait" in {
      errIn(("", "main.sysl",
        """private trait Show
          |    show(self) -> int
          |loud[T: Show](x: T) -> int = x.show()
          |print(1)
          |""".stripMargin)) should include(
        "'loud' is public, but the bound on 'T' names 'Show', which is private to 'main.sysl'")
    }

    // A field carries its own reach, so it is named as one and asked at its own — public here,
    // since it said nothing and the struct it belongs to is public.
    "a field of a public struct" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |struct Line
          |    a: Point
          |print(1)
          |""".stripMargin)) should include("'Line.a' is public, but its type names 'Point'")
    }

    "the payload of a public enum's variant" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |enum Shape
          |    Dot(at: Point)
          |    Round
          |print(1)
          |""".stripMargin)) should include("'Shape' is public, but the 'at' of variant 'Dot' names 'Point'")
    }

    // A member carries no modifier of its own, so it is as visible as the type it belongs to.
    "a member of a public struct" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |struct Line
          |    n: int
          |    tip(self) -> Point = Point(self.n)
          |print(1)
          |""".stripMargin)) should include("'Line.tip' is public, but its result names 'Point'")
    }

    "a method a public trait declares" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |trait Tip
          |    tip(self) -> Point
          |print(1)
          |""".stripMargin)) should include("'Tip.tip' is public, but its result names 'Point'")
    }

    "an extern, which the linker resolves and the rule reaches all the same" in {
      errIn(("", "main.sysl",
        """private enum Mode
          |    On
          |    Off
          |extern pick() -> Mode
          |print(1)
          |""".stripMargin)) should include("'pick' is public, but its result names 'Mode'")
    }

    "a type argument, which is named as much as the type it is applied to" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |struct Box[T]
          |    value: T
          |make() -> Box[Point] = Box(Point(1))
          |print(1)
          |""".stripMargin)) should include("'make' is public, but its result names 'Point'")
    }

    "a trait behind a memory mode, which is an object over it" in {
      errIn(("", "main.sysl",
        """private trait Show
          |    show(self) -> int
          |render(s: &Show) -> int = s.show()
          |print(1)
          |""".stripMargin)) should include("'render' is public, but parameter 's' names 'Show'")
    }

    // The diagnostic names the type by the path a reader would have to be able to write, not by the
    // shorter spelling this file gave it — the alias is exactly what they do not have.
    "and an alias does not hide which type is meant" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("geom", "g.sysl", "module geom\nprivate[geom] struct P\n    x: int"),
        ("geom", "h.sysl", "module geom\nimport geom.{P as Q}\nmake() -> Q = Q(1)"),
      ) should include("'make' is public, but its result names 'geom.P', which is private to module 'geom'")
    }

    /** The declarations that are a **name and one type**. They carry no signature, so the hole is
      * reached by a shorter route than a function's — a module that may write the name holds a value
      * whose type it cannot write, which is the same hole and not a smaller one.
      *
      * A `const` is the third of them and cannot reach this rule at all: `reference/modules.md § const — a value` holds a constant to
      * being a scalar, and every scalar is a builtin nobody may restrict. The test below is what says
      * so, and the check covers `const` anyway so that widening what a constant may hold cannot
      * quietly reopen what these two had.
      */
    "a public module-level 'val'" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |static val here: Point = Point(1)
          |print(1)
          |""".stripMargin)) should include("'here' is public, but its type names 'Point'")
    }

    "and a public 'extern' variable, whose storage the linker supplies" in {
      errIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |extern there: Point
          |print(1)
          |""".stripMargin)) should include("'there' is public, but its type names 'Point'")
    }

    // A `const` used not to get far enough to be asked: every user-declared type was refused as a
    // constant's type before visibility was looked at. That stopped being true when a
    // **transparent** subtype became one (`reference/errors.md § Constrained types`,
    // `reference/ffi.md § A library may carry C`), which is what the last case below is for — and
    // it is exactly what the line in `checkExposedTypes` was written in advance for, so that
    // widening what a constant may hold could not quietly reopen the hole `val` and `extern` had.
    //
    // The other three are still refused a step earlier, and are asserted here rather than only where
    // the refusals live so that the next form to become constant-able shows up in this block too.
    "while a 'const' naming a type that is not one is refused before the question arises" in {
      // The two halves of the scalar rule read differently now: a struct is **storage**, and is
      // told which keyword holds it, where an enum is simply not a scalar. Both are refused a step
      // before visibility is looked at, which is what this case is about.
      val declared = List(
        "Point" -> ("struct Point\n    x: int", "a constant is a scalar, and Point is storage"),
        "Mode"  -> ("enum Mode\n    On\n    Off", "a constant is a scalar, and Mode is not"),
      )

      for (named, (base, expected)) <- declared do
        withClue(s"a const of '$named': ") {
          err(s"$base\nconst c: $named = 5\nprint(1)") should include(expected)
        }

      withClue("a const of 'Tag': ") {
        err("type Tag = new int\nconst c: Tag = 5\nprint(1)") should include("is a 'new' type")
      }
    }

    "and a 'const' at a transparent subtype, which is the one form that does reach the question" in {
      errIn(("", "main.sysl",
        """private type Small = int within 0..<10
          |const c: Small = 5
          |print(1)
          |""".stripMargin)) should include("'c' is public, but its type names 'Small'")
    }

    // Both are restricted; what fails is that one subtree does not contain the other.
    "a 'private[M]' signature naming a type scoped more narrowly than it is" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("a.b", "x.sysl",
         """module a.b
           |private[b] struct P
           |    x: int
           |private[a] make() -> P = P(1)
           |""".stripMargin),
      ) should include(
        "'make' is visible throughout module 'a', but its result names 'a.b.P', which is private " +
          "to module 'a.b'")
    }
  }

  "but a signature naming a type that reaches at least as far is fine" - {
    // The one level that never needs checking: a file-private declaration is read in one file, and
    // every type it names is visible there or the signature would not have resolved.
    "a private function may name a private type" in {
      runIn(
        ("", "main.sysl", "print(geom.read())"),
        ("geom", "g.sysl",
         """module geom
           |private struct P
           |    x: int
           |private make() -> P = P(7)
           |read() -> int = make().x
           |""".stripMargin),
      ) shouldBe "7\n"
    }

    "and a narrower scope may name a type its ancestor keeps" in {
      runIn(
        ("", "main.sysl", "print(a.b.go())"),
        ("a.b", "x.sysl",
         """module a.b
           |private[a] struct P
           |    x: int
           |private[b] make() -> P = P(7)
           |go() -> int = make().x
           |""".stripMargin),
      ) shouldBe "7\n"
    }

    "and one private type may be another's field, in the file that has both" in {
      runIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |private struct Line
          |    a: Point
          |print(Line(Point(6)).a.x)
          |""".stripMargin)) shouldBe "6\n"
    }

    // The rule is about names, and a signature's type parameters are not names of declarations —
    // even where the module happens to declare something spelled the same way.
    "a type parameter is not the private type it shares a spelling with" in {
      runIn(("", "main.sysl",
        """private struct T
          |    x: int
          |id[T](x: T) -> T = x
          |print(id(5))
          |""".stripMargin)) shouldBe "5\n"
    }

    // The same exemption the function form gets, asked of the three name-and-one-type declarations:
    // restricted to the file that declares the type, there is nobody who could hold the value and be
    // unable to name it. These are also what says the refusals above are about the *reaches* rather
    // than about a `const`, a `val` or an `extern` variable naming a restricted type at all.
    "a private 'val' and 'extern' variable may each name a private type" in {
      runIn(("", "main.sysl",
        """private struct Point
          |    x: int
          |private val here: Point = Point(6)
          |private extern there: Point
          |print(here.x)
          |""".stripMargin)) shouldBe "6\n"
    }


    "and a scoped one may name a type its ancestor keeps" in {
      runIn(
        ("", "main.sysl", "print(a.b.go())"),
        ("a.b", "x.sysl",
         """module a.b
           |private[a] struct P
           |    x: int
           |private[b] val kept: P = P(7)
           |go() -> int = kept.x
           |""".stripMargin),
      ) shouldBe "7\n"
    }
  }

  // An `impl` names no type of its own, so neither half of it is a leak: the members' signatures are
  // the trait's, and a mismatch is refused as non-conformance rather than reaching this rule at all.
  "an 'impl' is outside the rule, in both directions" - {
    "a private trait may be implemented for a public type" in {
      runIn(("", "main.sysl",
        """private trait Show
          |    show(self) -> int
          |struct Tag
          |    n: int
          |impl Show for Tag
          |    show(self) -> int = self.n
          |print(Tag(5).show())
          |""".stripMargin)) shouldBe "5\n"
    }

    "and a public trait for a private type" in {
      runIn(("", "main.sysl",
        """trait Show
          |    show(self) -> int
          |private struct Tag
          |    n: int
          |impl Show for Tag
          |    show(self) -> int = self.n
          |print(Tag(5).show())
          |""".stripMargin)) shouldBe "5\n"
    }
  }
}
