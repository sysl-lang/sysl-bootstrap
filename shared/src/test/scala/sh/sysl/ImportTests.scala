package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `import` — the Scala forms, and the one thing they all do: shorten a path that already works
 * (`reference/modules.md § Imports`).
 *
 * Because an import grants nothing, every case here has a longhand that compiles without it. What
 * is under test is which shorter spelling means what, and which shorter spellings are refused for
 * meaning two things.
 */
class ImportTests extends AnyFreeSpec with CodegenSupport with RunSupport with ParseSupport {

  // A module whose members the cases below reach for, so each one differs only in how it names them.
  private val geom =
    ("geom", "g.sysl",
     """module geom
       |twice(n: int) -> int = n * 2
       |thrice(n: int) -> int = n * 3
       |struct Point
       |    x: int
       |    y: int
       |enum Shape
       |    Dot
       |    Round(r: int)
       |trait Show
       |    show(self) -> int
       |pair[T](a: T) -> T = a
       |extern abs(n: int) -> int
       |""".stripMargin)

  "the forms parse" - {
    "a bare path" in {
      prog("import std.fs.read") shouldBe List(ImportDecl(List("std", "fs", "read")))
    }

    "a selector list" in {
      prog("import std.fs.{read, write}") shouldBe
        List(ImportDecl(List("std", "fs"), List(ImportSelector("read", None), ImportSelector("write", None))))
    }

    "a rename" in {
      prog("import std.fs.{read as rd}") shouldBe
        List(ImportDecl(List("std", "fs"), List(ImportSelector("read", Some("rd")))))
    }

    "a wildcard" in {
      prog("import std.fs.*") shouldBe List(ImportDecl(List("std", "fs"), Nil, wildcard = true))
    }

    // The unbraced rename, which Scala 3 allows and `§3` had listed only in its braced form. It
    // belongs to the bare-path form alone: there is exactly one thing being named, so there is no
    // ambiguity about what the new word refers to.
    "an unbraced rename" in {
      prog("import std.fs.read as rd") shouldBe
        List(ImportDecl(List("std", "fs", "read"), Nil, wildcard = false, alias = Some("rd")))
    }

    "and one on a single segment, which renames the module" in {
      prog("import geom as g") shouldBe
        List(ImportDecl(List("geom"), Nil, wildcard = false, alias = Some("g")))
    }

    // Refused rather than ignored. Silently dropping the alias would leave a program whose text
    // says one thing and whose bindings say another.
    "a rename after a wildcard has nothing to name" in {
      progError("import std.fs.* as x") should include("a wildcard brings in every member")
    }

    "and a selector list carries its own, so a trailing one is refused too" in {
      progError("import std.fs.{read} as x") should include("carries its own 'as' per name")
    }

    "a single segment, which is a module or nothing" in {
      prog("import geom") shouldBe List(ImportDecl(List("geom")))
    }

    "an import sits among the statements, not above them" in {
      prog("print(1)\nimport geom.twice") shouldBe
        List(printStmt(i(1)), ImportDecl(List("geom", "twice")))
    }
  }

  "an import shortens" - {
    "one member" in {
      runIn(("", "main.sysl", "import geom.twice\nprint(twice(21))"), geom) shouldBe "42\n"
    }

    "several at once" in {
      runIn(("", "main.sysl", "import geom.{twice, thrice}\nprint(twice(3) + thrice(4))"), geom) shouldBe "18\n"
    }

    "every member of a module" in {
      runIn(("", "main.sysl", "import geom.*\nprint(twice(3) + thrice(4))"), geom) shouldBe "18\n"
    }

    "a member under another name" in {
      runIn(("", "main.sysl", "import geom.{twice as double}\nprint(double(21))"), geom) shouldBe "42\n"
    }

    // The same binding by the unbraced spelling, so the two forms are one feature rather than two
    // that happen to agree — and the old name is gone, which is what makes it a rename.
    "the same, written without the braces" in {
      runIn(("", "main.sysl", "import geom.twice as double\nprint(double(21))"), geom) shouldBe "42\n"
    }

    "and the name it replaced is not also bound" in {
      errIn(("", "main.sysl", "import geom.twice as double\nprint(twice(21))"), geom) should
        include("twice")
    }

    // A module renamed by the unbraced form, which the braced one cannot express at the top level:
    // `import text.util as u` has no selector list to hang the rename on.
    "a module under a shorter name, which only this form can say" in {
      runIn(
        ("", "main.sysl", "import text.util as u\nprint(u.width(4))"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "5\n"
    }

    "a module, so its members are reached by its last segment" in {
      runIn(
        ("", "main.sysl", "import text.util\nprint(util.width(4))"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "5\n"
    }

    "a module named among the selectors, which is the same import" in {
      runIn(
        ("", "main.sysl", "import text.{util}\nprint(util.width(4))"),
        ("text", "t.sysl", "module text\nwrap(n: int) -> int = n"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "5\n"
    }

    // The directory a sub-module sits in need not be a module itself: a `text/` holding only
    // `util/` has no members of its own, and a selector list over it reaches the one it does have.
    "a sub-module of a directory that holds no source of its own" in {
      runIn(
        ("", "main.sysl", "import text.{util}\nprint(util.width(4))"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "5\n"
    }

    // A wildcard is over a module's members, so a directory that is not one has none to offer.
    "but a wildcard over such a directory has no members to bring in" in {
      errIn(
        ("", "main.sysl", "import text.*\nprint(1)"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) should include("no module is called 'text'")
    }

    "a module under another name" in {
      runIn(
        ("", "main.sysl", "import text.{util as u}\nprint(u.width(4))"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "5\n"
    }
  }

  "an imported name works wherever the path did" - {
    "a type, in an annotation and in a construction" in {
      runIn(("", "main.sysl", "import geom.Point\nvar p: Point = Point(3, 4)\nprint(p.x + p.y)"), geom) shouldBe "7\n"
    }

    "an enum, reached by name" in {
      runIn(
        ("", "main.sysl",
         "import geom.Shape\nvar s: Shape = Shape.Round(7)\ns match\n    Dot -> print(0)\n    Round(r) -> print(r)"),
        geom,
      ) shouldBe "7\n"
    }

    "a variant, constructed unqualified" in {
      runIn(
        ("", "main.sysl",
         "import geom.{Shape, Round}\nvar s: Shape = Round(7)\ns match\n    Dot -> print(0)\n    Round(r) -> print(r)"),
        geom,
      ) shouldBe "7\n"
    }

    "a trait, as a bound" in {
      runIn(
        ("", "main.sysl",
         "import geom.Show\nloud[T: Show](x: T) -> int = x.show()\nstruct Tag\n    n: int\n" +
           "impl Show for Tag\n    show(self) -> int = self.n * 2\nprint(loud(Tag(5)))"),
        geom,
      ) shouldBe "10\n"
    }

    "a trait, as an object" in {
      runIn(
        ("", "main.sysl",
         "import geom.Show\nstruct Tag\n    n: int\nimpl Show for Tag\n    show(self) -> int = self.n * 2\n" +
           "var s: &Show = Tag(5)\nprint(s.show())"),
        geom,
      ) shouldBe "10\n"
    }

    "an associated function, reached through the imported type" in {
      runIn(
        ("", "main.sysl", "import geom.Point\nprint(Point.zero().x)"),
        ("geom", "g.sysl",
         "module geom\nstruct Point\n    x: int\n    y: int\n\n    zero() -> Point = Point(0, 0)"),
      ) shouldBe "0\n"
    }

    "and the full path still does, alongside the short one" in {
      runIn(("", "main.sysl", "import geom.twice\nprint(twice(1) + geom.twice(2))"), geom) shouldBe "6\n"
    }
  }

  "which name wins" - {
    // The order is `reference/modules.md § Imports`'s: this module, then the imports, then the
    // library.
    "a declaration of this module beats an import of the same name" in {
      runIn(("", "main.sysl", "import geom.twice\ntwice(n: int) -> int = n * 10\nprint(twice(4))"), geom) shouldBe
        "40\n"
    }

    "a selective import beats a wildcard that also offers the name" in {
      runIn(
        ("", "main.sysl", "import a.*\nimport b.f\nprint(f())"),
        ("a", "a.sysl", "module a\nf() -> int = 1"),
        ("b", "b.sysl", "module b\nf() -> int = 2"),
      ) shouldBe "2\n"
    }

    "a local binding shadows a module a file imported under its name" in {
      runIn(
        ("", "main.sysl",
         "import text.util\nstruct Box\n    width: int\nvar util: Box = Box(9)\nprint(util.width)"),
        ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n + 1"),
      ) shouldBe "9\n"
    }

    "an import does not hide a library name unless it offers one" in {
      runIn(("", "main.sysl", "import geom.*\nprint(twice(21))"), geom) shouldBe "42\n"
    }
  }

  "an import belongs to the file that wrote it" - {
    "a sibling file of the same module does not get it" in {
      errIn(
        ("m", "a.sysl", "module m\nimport geom.twice\nuse() -> int = twice(2)"),
        ("m", "b.sysl", "module m\nalso() -> int = twice(3)"),
        geom,
      ) should include("undefined function 'twice'")
    }

    "a body reads the imports of the file it was written in, not the caller's" in {
      runIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "a.sysl", "module m\nimport geom.twice\nuse() -> int = twice(21)"),
        geom,
      ) shouldBe "42\n"
    }

    "a member's signature is read there too" in {
      runIn(
        ("", "main.sysl", "print(m.Wrap(geom.Point(3, 4)).sum())"),
        ("m", "a.sysl",
         "module m\nimport geom.Point\nstruct Wrap\n    p: Point\n\n    sum(self) -> int = self.p.x + self.p.y"),
        geom,
      ) shouldBe "7\n"
    }

    "two files of one module may each bind the same name to a different thing" in {
      runIn(
        ("", "main.sysl", "print(m.one() + m.two())"),
        ("m", "a.sysl", "module m\nimport x.f\none() -> int = f()"),
        ("m", "b.sysl", "module m\nimport y.{g as f}\ntwo() -> int = f()"),
        ("x", "x.sysl", "module x\nf() -> int = 1"),
        ("y", "y.sysl", "module y\ng() -> int = 2"),
      ) shouldBe "3\n"
    }

    // A trait's default is copied into every implementing type, and it means what it meant at the
    // trait — including which module its file imported names from.
    "and so is a default a trait supplies" in {
      runIn(
        ("", "main.sysl", "import t.Loud\nstruct Tag\n    n: int\nimpl Loud for Tag\n    base(self) -> int = self.n\n" +
          "print(Tag(5).noise())"),
        ("t", "t.sysl",
         "module t\nimport geom.twice\ntrait Loud\n    base(self) -> int\n    noise(self) -> int = twice(self.base())"),
        geom,
      ) shouldBe "10\n"
    }
  }

  "an import inside a block" - {
    "binds for the rest of that block" in {
      runIn(
        ("", "main.sysl", "use() -> int =\n    import geom.twice\n    twice(21)\nprint(use())"),
        geom,
      ) shouldBe "42\n"
    }

    "and not for the function beside it" in {
      errIn(
        ("", "main.sysl", "one() -> int =\n    import geom.twice\n    twice(1)\ntwo() -> int = twice(2)"),
        geom,
      ) should include("undefined function 'twice'")
    }

    "and not after the block it is in closes" in {
      errIn(
        ("", "main.sysl", "use() -> int =\n    if true\n        import geom.twice\n        print(twice(1))\n    twice(2)"),
        geom,
      ) should include("undefined function 'twice'")
    }

    "shadowing what the file imported under the same name" in {
      runIn(
        ("", "main.sysl", "import geom.{twice as f}\nuse() -> int =\n    import geom.{thrice as f}\n    f(4)\n" +
          "print(use() + f(4))"),
        geom,
      ) shouldBe "20\n"
    }
  }

  // Two modules declaring one spelling to different effect, so what a program prints says which one
  // a name inside a closure reached. The file imports `a`'s; every block below imports `b`'s.
  private def rivals(main: String): Seq[(String, String, String)] =
    Seq(("", "main.sysl", main), ("a", "a.sysl", "module a\nf() -> int = 1"), ("b", "b.sysl", "module b\nf() -> int = 2"))

  "a closure written inside a block reads names as that block does" - {
    "its named import" in {
      runIn(rivals("import a.f\nuse() -> int =\n    import b.f\n    val g = () -> f()\n    g()\nprint(use())")*) shouldBe
        "2\n"
    }

    "its wildcard" in {
      runIn(rivals("import a.f\nuse() -> int =\n    import b.*\n    val g = () -> f()\n    g()\nprint(use())")*) shouldBe
        "2\n"
    }

    "an enclosing block's import, from a nested block" in {
      runIn(rivals("import a.f\nuse() -> int =\n    import b.f\n    if true\n        val g = () -> f()\n" +
        "        return g()\n    0\nprint(use())")*) shouldBe "2\n"
    }

    "and the inner block's before the outer one's" in {
      runIn(rivals("use() -> int =\n    import b.*\n    if true\n        import a.*\n        val g = () -> f()\n" +
        "        return g()\n    0\nprint(use())")*) shouldBe "1\n"
    }

    "a closure written as an argument" in {
      runIn(rivals("import a.f\napply(h: &Fn() -> int) -> int = h()\nuse() -> int =\n    import b.f\n" +
        "    apply(() -> f())\nprint(use())")*) shouldBe "2\n"
    }

    "a closure stored, and called after the block that imported has closed" in {
      runIn(rivals("import a.f\nuse() -> int =\n    var g: &Fn() -> int = () -> 0\n    if true\n        import b.f\n" +
        "        g = () -> f()\n    g()\nprint(use())")*) shouldBe "2\n"
    }

    "a closure returned out of the function whose block imported" in {
      runIn(rivals("import a.f\nmake() -> &Fn() -> int =\n    import b.f\n    () -> f()\nprint(make()())")*) shouldBe
        "2\n"
    }

    "a nested function written below the import" in {
      runIn(rivals("import a.f\nuse() -> int =\n    import b.f\n    h() -> int = f()\n    h()\nprint(use())")*) shouldBe
        "2\n"
    }

    "while a closure outside any importing block still reads the file's" in {
      runIn(rivals("import a.f\nuse() -> int =\n    val g = () -> f()\n    g()\nprint(use())")*) shouldBe "1\n"
    }

    "including one written after the importing block has closed" in {
      runIn(rivals("import a.f\nuse() -> int =\n    if true\n        import b.f\n        val k = () -> f()\n" +
        "        print(k())\n" +
        "    val g = () -> f()\n    g()\nprint(use())")*) shouldBe "2\n1\n"
    }
  }

  "an import runs nothing" - {
    "so two files may each import and neither becomes the one that starts the program" in {
      runIn(
        ("", "main.sysl", "import geom.twice\nprint(twice(21))"),
        ("m", "a.sysl", "module m\nimport geom.thrice\nuse() -> int = thrice(1)"),
        geom,
      ) shouldBe "42\n"
    }

    "and a program of imports and declarations alone is a program that does nothing" in {
      irIn(("", "main.sysl", "import geom.twice\nuse() -> int = twice(1)"), geom) should include("define i32 @main(")
    }
  }

  "a path that names nothing" - {
    "no module of that name at all" in {
      errIn(("", "main.sysl", "import nope.thing\nprint(1)"), geom) should
        include("no module is called 'nope.thing', and nothing declares it")
    }

    "a wildcard on something that is not a module" in {
      errIn(("", "main.sysl", "import nope.*\nprint(1)"), geom) should include("no module is called 'nope'")
    }

    "a selector list on something that is not a module" in {
      errIn(("", "main.sysl", "import geom.twice.{a}\nprint(1)"), geom) should
        include("no module is called 'geom.twice'")
    }

    "a single segment, which can only ever be a module" in {
      errIn(("", "main.sysl", "import geom2\nprint(1)"), geom) should
        include("no module is called 'geom2', and nothing declares it")
    }

    "a module that declares no such name" in {
      errIn(("", "main.sysl", "import geom.nope\nprint(1)"), geom) should
        include("'geom' declares no 'nope' — there is no 'geom.nope'")
    }

    "a selector that names no member" in {
      errIn(("", "main.sysl", "import geom.{twice, nope}\nprint(1)"), geom) should
        include("'geom' declares no 'nope'")
    }

    // The rename is what the file calls it; the mistake is still in what it asked the module for.
    "a rename of a member that is not there" in {
      errIn(("", "main.sysl", "import geom.{nope as n}\nprint(1)"), geom) should include("'geom' declares no 'nope'")
    }
  }

  "a name an import may not bind" - {
    "one already imported" in {
      errIn(("", "main.sysl", "import geom.twice\nimport geom.{thrice as twice}\nprint(1)"), geom) should
        include("'twice' is already imported")
    }

    "one already imported by the same statement" in {
      errIn(("", "main.sysl", "import geom.{twice, thrice as twice}\nprint(1)"), geom) should
        include("'twice' is already imported")
    }

    "one that is a module, which a dotted reference would then read two ways" in {
      errIn(("", "main.sysl", "import geom.{twice as text}\nprint(1)"),
            ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n"),
            geom) should include("'text' is a module, so importing something else under that name would hide it")
    }

    "and a module imported under a module's name is refused the same way" in {
      errIn(("", "main.sysl", "import text.{util as geom}\nprint(1)"),
            ("text", "t.sysl", "module text\nwrap(n: int) -> int = n"),
            ("text.util", "u.sysl", "module text.util\nwidth(n: int) -> int = n"),
            geom) should include("'geom' is a module")
    }

    // Asking for what is already true is not a collision with itself.
    "though importing a module under the name it already has is allowed and does nothing" in {
      runIn(("", "main.sysl", "import geom\nprint(geom.twice(21))"), geom) shouldBe "42\n"
    }
  }

  "a module alias is a prefix wherever a written path is" - {
    "in a type annotation and a construction" in {
      runIn(
        ("", "main.sysl", "import text.util\nvar p: util.Point = util.Point(3, 4)\nprint(p.x + p.y)"),
        ("text.util", "u.sysl", "module text.util\nstruct Point\n    x: int\n    y: int"),
      ) shouldBe "7\n"
    }

    "reaching a variant through its enum" in {
      runIn(
        ("", "main.sysl",
         "import text.util\nvar s: util.Shape = util.Shape.Round(7)\ns match\n    Dot -> print(0)\n" +
           "    Round(r) -> print(r)"),
        ("text.util", "u.sysl", "module text.util\nenum Shape\n    Dot\n    Round(r: int)"),
      ) shouldBe "7\n"
    }

    "as a bound, and as the trait an 'impl' is for" in {
      runIn(
        ("", "main.sysl",
         "import text.util\nloud[T: util.Show](x: T) -> int = x.show()\nstruct Tag\n    n: int\n" +
           "impl util.Show for Tag\n    show(self) -> int = self.n * 2\nprint(loud(Tag(5)))"),
        ("text.util", "u.sysl", "module text.util\ntrait Show\n    show(self) -> int"),
      ) shouldBe "10\n"
    }

    "and a module several segments deep is reached by its last one" in {
      runIn(
        ("", "main.sysl", "import a.b.c\nprint(c.f(21))"),
        ("a.b.c", "c.sysl", "module a.b.c\nf(n: int) -> int = n * 2"),
      ) shouldBe "42\n"
    }

    // The two are told apart by what is being named: a member is reached by the module's own path,
    // a sub-module by the name the import gave it.
    "even where the parent module declares a member of the same name" in {
      runIn(
        ("", "main.sysl", "import a.b\nprint(b.f() + a.b())"),
        ("a", "a.sysl", "module a\nb() -> int = 1"),
        ("a.b", "b.sysl", "module a.b\nf() -> int = 2"),
      ) shouldBe "3\n"
    }
  }

  "an import reaches whatever the module declares" - {
    "a generic function, instantiated at the call" in {
      runIn(
        ("", "main.sysl", "import geom.pair\nprint(pair(41) + 1)"),
        ("geom", "g.sysl", "module geom\npair[T](a: T) -> T = a"),
      ) shouldBe "42\n"
    }

    // An extern's symbol is the linker's and carries no module, but the name a program calls it by
    // is a name of its module like any other.
    "an extern, whose symbol the linker still resolves unqualified" in {
      runIn(
        ("", "main.sysl", "import geom.abs\nprint(abs(0 - 42))"),
        ("geom", "g.sysl", "module geom\nextern abs(n: int) -> int"),
      ) shouldBe "42\n"
    }

    "an imported type used in a struct pattern" in {
      runIn(
        ("", "main.sysl", "import geom.Point\nvar p: Point = Point(3, 4)\np match\n    Point{x, y} -> print(x + y)"),
        geom,
      ) shouldBe "7\n"
    }

    // The same spelling may be a type in one module and a function in another, and one import binds
    // a name rather than a kind — which of the two a use means is settled by where it is written.
    "a type from one module beside a function of the same name from another" in {
      runIn(
        ("", "main.sysl", "import a.X\nimport b.{X as X2}\nvar v: X = X(3)\nprint(v.n + X2(4))"),
        ("a", "a.sysl", "module a\nstruct X\n    n: int"),
        ("b", "b.sysl", "module b\nX(n: int) -> int = n * 10"),
      ) shouldBe "43\n"
    }
  }

  "the wildcard's own edges" - {
    "over the module the file is itself in, which changes nothing" in {
      runIn(("geom", "g2.sysl", "module geom\nimport geom.*\nprint(twice(21))"), geom) shouldBe "42\n"
    }

    // A wildcard offers rather than binds, so it neither collides with a selective import of the
    // same name nor with a second wildcard over the same module.
    "beside a selective import of a name it also offers" in {
      runIn(
        ("", "main.sysl", "import a.*\nimport a.f\nprint(f())"),
        ("a", "a.sysl", "module a\nf() -> int = 7"),
      ) shouldBe "7\n"
    }

    "and written twice over the one module" in {
      runIn(
        ("", "main.sysl", "import a.*\nimport a.*\nprint(f())"),
        ("a", "a.sysl", "module a\nf() -> int = 7"),
      ) shouldBe "7\n"
    }

    // A written wildcard offering a name the standard module also offers is the two-wildcard case,
    // not a shadowing one — the standard module is auto-imported, and an auto-import is exactly a
    // wildcard every file starts with (`AutoImport`), so the pair is ambiguous by §3's rule and the
    // message names both.
    //
    // This is what a prelude answered differently: it sat *below* the imports as a third step, so a
    // written wildcard beat it silently. The behaviour changed when the last declaration left, which
    // is the one place the drain is visible in a program rather than only in a symbol.
    "and a wildcard offering a library name is ambiguous rather than shadowing it" in {
      errIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "m.sysl", "module m\nimport a.*\nuse() -> int =\n    var o: Option = Option(7)\n    o.n"),
        ("a", "a.sysl", "module a\nstruct Option\n    n: int"),
      ) should include(s"'Option' is offered by '${Std.module}.*' and 'a.*'")
    }

    // And §3's stated fixes both work, which is what makes the ambiguity a nuisance rather than a
    // wall: name the module, or import the one you meant selectively.
    "which either of the fixes it names resolves" in {
      runIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "m.sysl", "module m\nimport a.*\nuse() -> int =\n    var o: a.Option = a.Option(7)\n    o.n"),
        ("a", "a.sysl", "module a\nstruct Option\n    n: int"),
      ) shouldBe "7\n"

      runIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "m.sysl", "module m\nimport a.Option\nuse() -> int =\n    var o: Option = Option(7)\n    o.n"),
        ("a", "a.sysl", "module a\nstruct Option\n    n: int"),
      ) shouldBe "7\n"
    }
  }

  "a mistake in an import costs the import and no more" - {
    "the rest of the file is still read" in {
      errIn(("", "main.sysl", "import nope.thing\nimport geom.twice\nprint(twice(21))\nprint(gone())"), geom) should
        include("undefined function 'gone'")
    }

    "and so is the rest of a block" in {
      errIn(("", "main.sysl", "use() -> int =\n    import nope.thing\n    gone()\nprint(use())"), geom) should
        include("undefined function 'gone'")
    }

    "an imported name that is not a trait is still told it is not one" in {
      errIn(("", "main.sysl", "import geom.Point\nf[T: Point](x: T) -> int = 1\nprint(1)"), geom) should
        include("names 'Point', which is not a trait")
    }

    "an empty selector list does not parse" in {
      // The refusal is inside the braces, where a name would have gone, rather than at the end of
      // the line the braces are on.
      errIn(("", "main.sysl", "import geom.{}\nprint(1)"), geom) should include("identifier expected")
    }
  }

  "two wildcards offering one name" - {
    "make an unqualified use of it ambiguous" in {
      errIn(
        ("", "main.sysl", "import a.*\nimport b.*\nprint(f())"),
        ("a", "a.sysl", "module a\nf() -> int = 1"),
        ("b", "b.sysl", "module b\nf() -> int = 2"),
      ) should include("'f' is offered by 'a.*' and 'b.*'")
    }

    "and the fix is to name the module, which the wildcards never took away" in {
      runIn(
        ("", "main.sysl", "import a.*\nimport b.*\nprint(a.f() + b.f())"),
        ("a", "a.sysl", "module a\nf() -> int = 1"),
        ("b", "b.sysl", "module b\nf() -> int = 2"),
      ) shouldBe "3\n"
    }

    // Only a name they *both* declare is ambiguous; the rest of each module comes through.
    "while a name only one of them has is not ambiguous at all" in {
      runIn(
        ("", "main.sysl", "import a.*\nimport b.*\nprint(g() + h())"),
        ("a", "a.sysl", "module a\ng() -> int = 1"),
        ("b", "b.sysl", "module b\nh() -> int = 2"),
      ) shouldBe "3\n"
    }
  }
}
