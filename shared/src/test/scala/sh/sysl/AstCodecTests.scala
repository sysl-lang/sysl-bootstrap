package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The AST codec reads back exactly the tree the parser produced, which is what
 * `reference/modules.md § Separate compilation` rests on.
 *
 * **The shipped library is the load-bearing case**, and deliberately so: it is real sysl using
 * nearly every declaration the language has — generic enums, traits with defaults and supertraits,
 * `impl` blocks, externs with link names, contracts, closures, patterns — so a node the codec cannot
 * carry shows up here rather than in a hand-written fixture that happens to avoid it. The small cases
 * below exist for the shapes it does not reach and for the ways the format goes wrong.
 *
 * It is round-tripped **whole**, every file of it, rather than one file standing for the rest — a
 * declaration shape the codec cannot carry is as likely to be in the one file left out. A count of
 * lines or declarations is deliberately not asserted anywhere here: the library grows, and such a
 * threshold measures that rather than the codec. The one count that is asserted is over positions,
 * and it guards against the comparisons holding vacuously rather than describing the library.
 *
 * **Structural equality alone is not enough to pin this.** A position is deliberately not a
 * constructor parameter, so `==` ignores it entirely — a codec that dropped every position would pass
 * a naive round-trip test and then report every library diagnostic at line 0. The positions are
 * therefore walked and compared separately, which is what `positionsOf` is for.
 */
class AstCodecTests extends AnyFreeSpec with Matchers {

  private def parsed(src: String, name: String = "<t>"): Program =
    SyslParser.parse(Source(name, src)) match
      case Right(p) => p
      case Left(e)  => fail(s"the fixture does not parse: $e")

  private def roundTrip(programs: List[Program], known: Map[String, Source] = Map.empty): List[Program] =
    AstCodec.decode(AstCodec.encode(programs), known) match
      case Right(ps) => ps
      case Left(e)   => fail(s"decode failed: $e")

  /** Every position in a tree, in the order a walk over the case-class children reaches them. Two
   * trees that agree here agree about what each node points at, which `==` cannot tell us.
   *
   * **The end of each span is compared as well as its start**, since a codec that wrote the start
   * and dropped the rest would read back a tree whose every diagnostic underlines one character.
   */
  private def positionsOf(node: Any): List[Option[(String, Int, Int, Int, Int)]] = {
    val here = node match
      case p: Positioned => List(p.pos.map(x => (x.source.name, x.line, x.col, x.endLine, x.endCol)))
      case _             => Nil

    // A `List` and a `Some` are themselves `Product`s, so the collections are matched first —
    // otherwise a list would be walked as its `::` cells, which reaches the same nodes by a route
    // that is far less obvious to read.
    val below = node match
      case xs: List[?]  => xs.flatMap(positionsOf)
      case o: Option[?] => o.toList.flatMap(positionsOf)
      case m: Map[?, ?] => m.toList.sortBy(_._1.toString).flatMap((k, v) => positionsOf(k) ::: positionsOf(v))
      case p: Product   => p.productIterator.toList.flatMap(positionsOf)
      case _            => Nil

    here ::: below
  }

  "the library" - {

    // One file rather than all of them where a test is about a single `Program`'s round trip. It is
    // the first in module order, so which one it is does not depend on a directory listing.
    val one = Std.parsed(Target.default).head

    "round-trips to a structurally equal tree" in {
      val back = roundTrip(Std.parsed(Target.default))

      back should have length Std.parsed(Target.default).length
      back.map(_.body) shouldBe Std.parsed(Target.default).map(_.body)
    }

    "round-trips with every position intact" in {
      val back = roundTrip(Std.parsed(Target.default))

      positionsOf(back.map(_.body)) shouldBe positionsOf(Std.parsed(Target.default).map(_.body))
    }

    "carries enough positions to be worth carrying" in {
      // Guards the two tests above: if the trees somehow had no positions, their comparisons would
      // hold vacuously and a codec that dropped every one of them would pass.
      //
      // Counted over the library through `Library.decls` rather than over a file, so the threshold
      // does not move when a declaration moves between files.
      val stamped = positionsOf(Library.decls).count(_.isDefined)

      stamped should be > 1000
    }

    "and enough of them have an extent for the ends to be worth comparing" in {
      // The same guard one level in. Every span above compares equal if every span is a point, so
      // the ends would be carried vacuously by a codec that had quietly stopped writing them.
      val wide = positionsOf(Library.decls).flatten.count((_, ln, cl, endLn, endCl) => endLn > ln || endCl > cl)

      wide should be > 1000
    }

    // **The library uses no vectors, so nothing else encodes a `VectorType`** — the round trip over
    // `Std.parsed` above would stay green with the `tvec` tag missing from the decoder entirely, and
    // the failure would appear only when somebody put a vector in a package and read the artifact
    // back. A tag with no encoder or no decoder is precisely what this suite exists to catch, so a
    // type the standard library does not happen to use needs a fixture of its own.
    "round-trips a vector type, which the library itself never writes" in {
      val src =
        """scale[const W: usize](v: <W>f32, by: f32) -> <W>f32 = v * by
          |
          |f(a: <4>u8, b: <(2 * 4)>f32) -> <4>u8 = a
          |""".stripMargin

      val one  = parsed(src)
      val back = roundTrip(List(one))

      back.map(_.body) shouldBe List(one.body)
      positionsOf(back.map(_.body)) shouldBe positionsOf(List(one.body))
    }

    "rebinds to the caller's own Source, so a decoded declaration is still the library's" in {
      val back = roundTrip(List(one), Map(one.source.name -> one.source))

      // `Stdlib.owns` is identity on the Source, and it is what decides whether an unreached
      // declaration may be dropped — so a decoded tree handed the embedded std's own `Source` has
      // to land on that object rather than on a copy of it.
      back.head.source should be theSameInstanceAs one.source
      back.head.body.forall(Library.carried.owns) shouldBe true
    }

    "reconstructs a usable Source when the caller supplies none" in {
      val back = roundTrip(List(one))

      back.head.source.name shouldBe one.source.name
      back.head.source.text shouldBe one.source.text
      // The text is carried so a diagnostic against a library declaration can quote its line.
      back.head.source.line(1) shouldBe one.source.line(1)
    }

    "encodes deterministically, so an artifact can be cached and diffed" in {
      AstCodec.encode(Std.parsed(Target.default)) shouldBe AstCodec.encode(Std.parsed(Target.default))
    }
  }

  "a declaration round-trips" - {

    def check(label: String, src: String): Unit =
      label in {
        val original = parsed(src)
        val back     = roundTrip(List(original))

        back.head.body shouldBe original.body
        positionsOf(back.head.body) shouldBe positionsOf(original.body)
      }

    check("a generic function with bounds", "f[T: Ord, U: Eq + Hash](a: T, b: U) -> T = a")
    // The one statement whose payload is another *statement*, so the encoding recurses through
    // `stmt` rather than bottoming out in expressions. A library exporting an inline function with a
    // `defer` in it is how this reaches an artifact, and a codec that dropped the payload would
    // decode to a body that releases nothing and says so nowhere.
    check("a defer carrying a statement", "f()\n    defer print(\"out\")\n    print(\"in\")")
    // Assembly is the one statement whose payload is neither a statement nor an expression — three
    // lists of strings, an operand list, and two arm shapes told apart by a tag. An artifact
    // carrying an inline function with an arch layer in it is how this arrives at a library, and
    // every part of an arm has to come back or the arm answers for the wrong processor.
    check(
      "assembly with every arm shape",
      "f(n: int)\n" +
        "    asm\n" +
        "        [x86_64]\n" +
        "            \"nop {n}\"\n" +
        "            in n : reg\n" +
        "            out n : \"rax\"\n" +
        "            clobbers \"rdx\"\n" +
        "        [aarch64]\n" +
        "        [riscv64] unavailable \"nothing to say here\"\n",
    )
    check("a variadic extern under a link name", "extern \"snprintf\" fmt(f: *u8, ...) -> int")
    // The two `extern` forms together, since the tag is what tells them apart and a codec that wrote
    // one where the other belonged would still round-trip every field either of them has.
    check("an extern variable, beside the function it is not",
      "extern \"environ\" env: **u8\nprivate extern optind: i32\nextern abs(n: int) -> int")
    // A default is part of what a signature says, so a library that lost one across the artifact
    // would tell a caller in another project that the argument is missing. The undefaulted
    // parameter beside it is what keeps the case from passing for a codec that defaulted every
    // parameter to the same thing.
    check("a parameter's default, and a named argument at a call",
      """f(a: int, b: int = 2 + 3, c: string = "x") -> int = a + b
        |g() -> int = f(1, c = "y")
        |""".stripMargin)
    check("a struct with members, invariants and a private field",
      """struct Span
        |    private lo: int
        |    hi: int
        |    invariant lo <= hi
        |
        |    width(self) -> int = self.hi - self.lo
        |    zero() -> Span = Span(0, 0)
        |""".stripMargin)
    check("a data enum with an underlying type and a member",
      """enum Shape
        |    Dot
        |    Round(r: int)
        |
        |    area(self) -> int = 0
        |""".stripMargin)
    check("a trait with a default, a supertrait and a type default",
      """trait Word: Add + BitXor
        |    show(self) -> string
        |    twice(self) -> int = 2
        |""".stripMargin)
    check("a trait declaring an associated type, and the projection its member returns",
      """trait Seq
        |    type Item: Show + Eq
        |    head(self) -> Self::Item
        |""".stripMargin)
    check("an impl supplying an associated type, and one inferring it from a body",
      """struct Box
        |    v: int
        |impl Seq for Box
        |    type Item = int
        |    head(self) -> Self::Item = self.v
        |impl Other for Box
        |    tail(self) -> some Show = self.v
        |""".stripMargin)
    check("an impl with its own parameters and trait arguments",
      """struct Box[T]
        |    v: T
        |impl[T: Show] Show for Box[T]
        |    show(self) -> string = "b"
        |""".stripMargin)
    check("a constrained subtype with a range and a predicate",
      "type Age = u8 within 0..<200 where value != 13u8")
    check("a const and a module-level val", "const cap: usize = 512\nval order: [3]int = [2, 0, 1]")
    // The one expression whose payload is a bare statement *list*. The library may well hold none
    // today, and that is the argument for the case rather than against it: an inline function whose
    // local is bound to a block is how one arrives, and a codec that dropped the list would decode
    // to a binding with no value and say so nowhere. The second binding pins the other half — a
    // block of one expression is collapsed by the parser, so what round-trips there is the sum.
    check("a block initializer, and the single expression that collapses out of one",
      """f() -> int
        |    val computed =
        |        val a = 6
        |        a * 7
        |    val plain =
        |        1 + 2
        |    computed + plain
        |""".stripMargin)
    check("a read-only view, beside the mutable one it is not",
      // Whether a view is read-only is one `Boolean` on `ArrayType`, and dropping it on the way
      // through would still compile and still round-trip every other field — the library would just
      // quietly arrive with `putbytes` taking a slice it is allowed to write. The two forms sit
      // together here so a codec that encoded neither, or conflated them, has nowhere to hide.
      "f(a: []const u8, b: []u8) -> []const u8 = a")
    check("every import form",
      """import a.b.c
        |import a.b.{c, d as e}
        |import a.b.*
        |import a.b
        |""".stripMargin)
    check("a module header", "module geom.shape\n\nf() -> int = 1")
    // A library ships no tests, so nothing that travels in a real artifact carries one of these —
    // which is exactly why the round trip is asserted here rather than left to the library above.
    // The codec's promise is that a tree reads back as the tree that was written, and a field only
    // ever seen holding `None` is one that could be dropped with nothing noticing.
    check("a test in each form its attribute takes",
      """@test
        |plain() = 0
        |
        |@test("a sentence about what holds")
        |named() = 0
        |
        |@test(should_trap)
        |trapping() = 0
        |
        |@test("both at once", should_trap: "past the end")
        |both() = 0
        |
        |@test("waiting", should_trap, ignore: "the check is not emitted yet")
        |waiting() = 0
        |""".stripMargin)
    // A section that did not survive the trip would leave a library's placed object landing wherever
    // the linker chose — in a program that read the artifact, and nowhere else, so nothing about
    // compiling that library from source would show it. The binding carries `@align` beside it
    // because the two are folded by different lines and one could travel without the other.
    // A package's chosen C name has to survive the artifact for the reason a function's exported
    // symbol does: a consumer generating a header from a library it read back must spell the type
    // the way the library chose, and a codec that dropped it would silently derive one instead. The
    // bare form is beside the named one because they are folded by one line and stored as two
    // different things — `None` and the declared name.
    check("a struct's C name, chosen and taken from the declaration",
      """@export("b2BodyId")
        |struct Id
        |    index1: i32
        |
        |@export
        |struct Handle
        |    slot: u16
        |""".stripMargin)
    check("a section on a binding and on a definition",
      """@align(4096)
        |@section(".noinit")
        |var reason: [4]u32
        |
        |@section(".ramfunc")
        |erase() -> int = 7
        |""".stripMargin)
  }

  "an expression round-trips" - {

    def check(label: String, src: String): Unit =
      label in {
        val original = parsed(s"f() -> int =\n    $src\n    0")
        val back     = roundTrip(List(original))

        back.head.body shouldBe original.body
        positionsOf(back.head.body) shouldBe positionsOf(original.body)
      }

    check("a comparison chain", "var c = 1 < 2 < 3")
    check("a match with guards, ranges and nested patterns",
      "var m = 1 match\n        1..<5 if true -> 1\n        Wrap(Val(v)) -> v\n        P{x: 0, y} -> y\n        _ -> 0")
    check("a closure with an annotated parameter", "var g = (x: int) -> x + 1")
    check("a three-clause for with an else", "for var i = 0; i < 3; i++\n        print(i)\n    else\n        print(9)")
    check("a labelled loop with a value-carrying break", "var v = 'outer loop\n        break 'outer 7")
    check("a tuple, an index and a type attribute", "var t = ((1, 2).0, [1, 2][0], int::Range)")
    check("an array fill and a slice", "var a = [0; 4]\n    var s = a[0..<2]")
    check("a try, a unit, a null and a c-string", "var q = ()\n    var n: *u8 = null\n    var cs = c\"hi\"")
    check("a 128-bit literal with a suffix", "var w = 170141183460469231731687303715884105727i128")
    check("a multi-assignment and a multi-declaration", "var a = 1\n    var b = 2\n    a, b = b, a\n    val p, q = 3, 4")
    // Both quantifiers, both range forms, and a predicate that is itself a conjunction — so the
    // greedy body has to survive the trip as the body rather than as a sibling of it.
    check("both quantifiers, over both range forms",
      "var u = for all i in 0..<3 do i >= 0 && i < 3\n    var e = for some j in 1..4 do j == 2")
    // A trailing block is the one expression node that is *replaced* rather than analyzed — a call's
    // argument binding turns it into an array literal or a closure — so a library carrying one
    // stores it as itself and has to read it back that way. Nothing else would notice if the codec
    // dropped it: the failure would be a call in the library that had lost an argument.
    check("a trailing block", "var v = g:\n        1\n        2")
    // A `with` clause carries a field list that is neither an expression nor a statement, so it has
    // a writer and a reader of its own — and the name it carries is the half a structural comparison
    // of the values alone would not notice going missing.
    check("a with clause, chained and with several fields",
      "var w = s with { bg = 9, pad = 8 } with { fg = 7 }")
    // A loop's clauses are statements at the head of its body, so what has to survive is their
    // *position* as much as their contents — a message on one and none on the other.
    check("a loop carrying an invariant with a message and a variant",
      "var i = 0\n    while i < 3\n        invariant i >= 0, \"never backwards\"\n" +
        "        variant 3 - i\n        i += 1")
  }

  "the format refuses" - {

    "something that is not an artifact at all" in {
      AstCodec.decode("print(1)\n") shouldBe Left("this is not a sysl AST artifact")
    }

    "an artifact from a different version, naming both" in {
      // The version travels from the constant. Written out as a literal, this test goes on passing
      // after a format bump while silently checking that a header naming a version nobody uses is
      // refused — which is not the claim.
      val original = AstCodec.encode(List(parsed("f() -> int = 1")))
      val older    = original.replaceFirst(s"sysl-ast ${AstCodec.Version}", "sysl-ast 0")

      AstCodec.decode(older) match
        case Left(e) =>
          e should include("version 0")
          e should include(s"version ${AstCodec.Version}")
          e should include("regenerate")
        case Right(_) => fail("a version mismatch was accepted")
    }

    "a truncated body rather than returning half a tree" in {
      val original = AstCodec.encode(List(parsed("f() -> int = 1 + 2")))

      AstCodec.decode(original.take(original.length - 12)).isLeft shouldBe true
    }

    "a tag it does not know" in {
      val original = AstCodec.encode(List(parsed("f() -> int = 1")))

      AstCodec.decode(original.replace(" il ", " zz ")) match
        case Left(e)  => e should include("'zz'")
        case Right(_) => fail("an unknown tag was accepted")
    }
  }

  "a string in the artifact survives" - {

    "a newline, a space and a colon inside a literal" in {
      val original = parsed("f() -> string = \"a: b\\nc d\"")
      val back     = roundTrip(List(original))

      back.head.body shouldBe original.body
    }

    "being the source text itself, which holds every newline the file had" in {
      val src      = "f() -> int =\n    var x = 1\n    x\n"
      val original = parsed(src, "multi.sysl")
      val back     = roundTrip(List(original))

      back.head.source.text shouldBe src
      back.head.source.line(2) shouldBe "    var x = 1"
    }
  }

  "several files in one artifact" - {

    "keep their own module headers, sources and directories" in {
      val a = Program(parsed("module geom\nf() -> int = 1").body,
        Some(ModuleName(List("geom"))), Nil, Nil,
        new Source("a.sysl", "module geom\nf() -> int = 1", Some(List("geom"))))
      val b = Program(parsed("module geom.shape\ng() -> int = 2").body,
        Some(ModuleName(List("geom", "shape"))), Nil, Nil,
        new Source("b.sysl", "module geom.shape\ng() -> int = 2", Some(List("geom", "shape"))))

      val back = roundTrip(List(a, b))

      back should have length 2
      back(0).module.map(_.show) shouldBe Some("geom")
      back(1).module.map(_.show) shouldBe Some("geom.shape")
      back(0).source.name shouldBe "a.sysl"
      back(1).source.dir shouldBe Some(List("geom", "shape"))
    }

    // `__VERSION__` in a generic body is folded by the importer, which reads the tree back out of
    // the artifact — so the package a file belongs to has to survive the trip, or the importer
    // would refuse the library's own `__VERSION__` as a file that belongs to no package.
    "and the manifest each file belongs to, with or without a version and with or without a directory" in {
      def file(name: String, dir: Option[List[String]], m: Option[SourceManifest]): Program =
        Program(parsed("f() -> int = 1").body, None, Nil, Nil,
          new Source(name, "f() -> int = 1", dir, manifest = m))

      val back = roundTrip(List(
        file("a.sysl", Some(List("geom")), Some(SourceManifest("/p/package.hocon", Some("1.2.3")))),
        file("b.sysl", None, Some(SourceManifest("/q/package.hocon", None))),
        file("c.sysl", Some(Nil), None),
      ))

      back.map(_.source.manifest) shouldBe List(
        Some(SourceManifest("/p/package.hocon", Some("1.2.3"))), Some(SourceManifest("/q/package.hocon", None)), None)
      back.map(_.source.dir) shouldBe List(Some(List("geom")), None, Some(Nil))
    }

    "share one string table, so a name written twice is stored once" in {
      val one  = AstCodec.encode(List(parsed("f(averylongparametername: int) -> int = averylongparametername")))
      val two  = AstCodec.encode(List(
        parsed("f(averylongparametername: int) -> int = averylongparametername"),
        parsed("g(averylongparametername: int) -> int = averylongparametername"),
      ))

      // Two files naming the same identifier cost one table entry, so the second file adds far
      // less than the first did.
      (two.length - one.length) should be < one.length
    }
  }

  "a literate file's positions survive the round trip" in {
    // What is stored is the text the positions were recorded against, which for a `.lsysl` file is
    // its program with the four columns that made it program text already gone (`Literate`). A
    // decoded source that had forgotten the margin would report every column in a library's literate
    // file four too small — right up until someone tried to open one at the location given.
    val src = "    add(a: int, b: int) -> int = a + b\n"

    val back = roundTrip(List(SyslParser.parse(Source("m.lsysl", src)) match {
      case Right(p) => p
      case Left(e)  => fail(e)
    }))

    val at = positionsOf(back.head.body).flatten.head

    at._1 shouldBe "m.lsysl"
    back.head.source.columnOffset shouldBe 4
    Pos(back.head.source, at._2, at._3).location shouldBe s"m.lsysl:${at._2}:${at._3 + 4}"
  }

  // What an artifact's positions are for is a diagnostic quoting the library, and that is `pos`. An
  // extent answers what an editor asks of a file it has open, which is parsed rather than decoded —
  // so it is not written, and a decoded node says its extent is where it points. The assertion is
  // here rather than in a comment because the fallback is a decision and not an oversight: a node
  // that came back claiming a *wider* extent than the artifact carries would be inventing one.
  "an extent is not carried by the artifact, and falls back to where the node points" in {
    val src   = "var x = 1\nprint(alpha)\n"
    val call  = (p: Program) => p.body.collectFirst { case ExprStmt(c: Call) => c }
    val fresh = call(parsed(src)).getOrElse(fail("the fixture has no call in it"))
    val back  = call(roundTrip(List(parsed(src))).head).getOrElse(fail("the call did not survive"))

    // A `Source` is compared by identity, so the two trees' positions are never equal as objects
    // however well they agree — which is the whole reason `positionsOf` walks numbers.
    val place = (p: Option[Pos]) => p.map(x => (x.line, x.col, x.endLine, x.endCol))

    place(fresh.pos) shouldBe Some((2, 1, 2, 6))
    place(fresh.extent) shouldBe Some((2, 1, 2, 13))

    place(back.pos) shouldBe place(fresh.pos)
    place(back.extent) shouldBe place(back.pos)
  }

  // A pattern binding carries a *pattern* rather than a list of names, so the encoder has a nested
  // structure to write where every other binding form has a flat one. Nesting and a wildcard are
  // both here because each is a way the shape can be lost while the names survive.
  "a pattern binding keeps its shape across the format" in {
    val src =
      """show() =
        |    val ((a, b), _) = ((1, 2), 3)
        |    var (c, d) = (4, 5)
        |    val Point{x, y: (e, f)} = mk()
        |    print(a, b, c, d, x, e, f)
        |
        |show()""".stripMargin

    roundTrip(List(parsed(src))).head.body shouldBe parsed(src).body
  }

  "a decoded tree compiles to the same program the parsed one does" in {
    // The point of the format: what comes back is not merely equal, it is usable. Compiling
    // through it exercises the analyzer and codegen over decoded nodes rather than parsed ones.
    val src = "add(a: int, b: int) -> int = a + b\nprint(add(2, 3))"

    val direct = Compiler.compileToLlvm(src, "d.sysl")
    val back   = roundTrip(List(parsed(src, "d.sysl")))
    val viaIr  = Compiler.compileTrees(back)

    direct.isRight shouldBe true
    viaIr shouldBe direct
  }
}
