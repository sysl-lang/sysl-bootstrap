package sh.sysl

import java.nio.charset.StandardCharsets.UTF_8

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Separate compilation: a library built once into a `.syslib`, and a program linked against it
 * (`LibraryArtifact`, `reference/modules.md § Separate compilation`).
 *
 * **What is worth pinning here is the split**, because it is the whole claim. A declaration with no
 * type parameters is compiled by whoever built the library and *linked* by whoever uses it; a
 * generic has nothing to compile until a caller fixes its arguments, so it travels as a tree and is
 * monomorphized in the consuming program. Both halves are checked at the seam a user sees — the
 * emitted IR for which is which, and a program that actually runs.
 */
class LibraryArtifactTests extends AnyFreeSpec with Matchers {

  private val library =
    """module demo
      |
      |double(n: int) -> int = n * 2
      |
      |larger[T: Ord](a: T, b: T) -> T = if a < b then b else a
      |
      |val squares: [4]int = [0, 1, 4, 9]
      |
      |lookup(i: int) -> int = squares[i]
      |
      |struct Money
      |    cents: int
      |end Money
      |
      |impl Eq for Money
      |    eq(self, rhs: Money) -> bool = self.cents == rhs.cents
      |end Money
      |
      |impl Ord for Money
      |    lt(self, rhs: Money) -> bool = self.cents < rhs.cents
      |end Money
      |
      |impl Add for Money
      |    add(self, rhs: Money) -> Money = Money(self.cents + rhs.cents)
      |end Money
      |""".stripMargin

  private def sources: List[Source] = List(Source("demo/lib.sysl", library, List("demo")))

  private def built: (String, String) =
    LibraryArtifact.build(sources) match
      case Right(r)  => r
      case Left(err) => fail(s"the library did not build: $err")

  /** The metadata as a consumer reads it back: the trees, the precompiled symbols, and the
   * fingerprint of the source it was built from.
   */
  private def metadata: (List[Program], Set[String], String) =
    LibraryArtifact.read("demo.syslib", built._2, Target.default) match
      case Right(r)  => r
      case Left(err) => fail(s"the metadata did not read back: $err")

  /** A library that forms a closure of its own, which is card `0229`'s condition — see the section
   * at the foot of this file.
   */
  private val closing =
    """module zone
      |
      |struct Off
      |    mins: int
      |
      |fixed(t: int) -> Off = Off(0 - 300)
      |
      |pick(t: int, at: int -> Off) -> Off = at(t)
      |
      |house(t: int) -> Off = pick(t, fixed)
      |""".stripMargin

  private def zone: (String, String) =
    LibraryArtifact.build(List(Source("zone/lib.sysl", closing, List("zone")))) match
      case Right(r)  => r
      case Left(err) => fail(s"the library did not build: $err")

  /** The emitted lines defining a symbol named after a closure, and the symbols themselves. The name
   * is cut before the parameter list: a function *taking* a closure names that struct among its
   * parameters, and the question here is what the function is called by.
   */
  private def closureDefinitions(ir: String): List[String] =
    ir.linesIterator.filter(_.startsWith("define")).filter(l => Closures.mentioned(symbolOn(l))).toList

  private def closureSymbols(ir: String, form: String): List[String] =
    ir.linesIterator.filter(_.startsWith(s"$form ")).map(symbolOn).filter(Closures.mentioned).toList

  private def symbolOn(line: String): String = {
    val at = line.indexOf('@')

    if at < 0 then "" else line.drop(at + 1).takeWhile(c => c != '(' && c != ' ')
  }

  "what a library compiles ahead of time" - {

    "a declaration with no type parameters is compiled once, by the library" in {
      metadata._2 should contain("demo$double")
    }

    "a generic is not, because there is nothing to compile until a caller fixes its arguments" in {
      metadata._2.filter(_.startsWith("demo$larger")) shouldBe empty
    }

    "nor is one that reads module-level storage, which no library initializes" in {
      // The honest boundary of what separate compilation reaches today: a `val`'s storage is written
      // by the entry point, and a library has none — so `lookup` is compiled in the program, where
      // the initialization it depends on actually happens.
      metadata._2 should not contain "demo$lookup"
    }

    "and the tree carries every declaration, precompiled or not" in {
      // A call into the precompiled half still has to be type-checked, and the signature is in the
      // tree — so the tree is not just the generics.
      val names = metadata._1.flatMap(_.body).collect { case f: FuncDecl => f.name }

      names should contain allOf ("double", "larger", "lookup")
    }
  }

  /** An artifact carrying given metadata, and object code the linker would have taken seriously.
   *
   * The metadata rides in a member of its own here rather than inside an object file, because that is
   * exactly what the reader is entitled to assume: it finds the payload by scanning member bytes for
   * a marker, so *how* the bytes got into the member is the object writer's business and not its own.
   * A fixture that had to run clang to say what happens to a damaged length would be a fixture that
   * could not say it at all.
   */
  private def artifact(meta: String, code: Array[Byte] = Array[Byte](1, 2, 3)): Array[Byte] =
    FakeAr(LibraryArtifact.codeMember -> code, LibraryArtifact.metadataMember -> LibraryArtifact.frame(meta))

  "the container" - {

    "hands back the metadata buried in it" in {
      LibraryArtifact.metadataOf("x.syslib", artifact("meta")) shouldBe Right("meta")
    }

    "finds it wherever in the archive it happens to sit" in {
      // Member order is the archiver's to choose, and the reader must not depend on having been the
      // one that chose it.
      val reversed =
        FakeAr(LibraryArtifact.metadataMember -> LibraryArtifact.frame("meta"),
          LibraryArtifact.codeMember -> Array[Byte](1, 2, 3))

      LibraryArtifact.metadataOf("x.syslib", reversed) shouldBe Right("meta")
    }

    "keeps a boundary that holds when the metadata is not ASCII" in {
      // The length in the frame is in **bytes**, and the metadata carries the library's own source
      // text, which is UTF-8. Counting characters would end it somewhere inside that text.
      LibraryArtifact.metadataOf("x.syslib", artifact("π≈3")) shouldBe Right("π≈3")
    }

    "reads its own member rather than a marker the compiled half happens to contain" in {
      // The compiled half carries the library's string literals, which are the only bytes of an
      // artifact a user chooses — so a library whose own source spelled the marker out would, on a
      // plain scan, have its metadata read out of the middle of its own text. Naming the member is
      // what settles which one is meant, and the fixture puts the impostor first so that member order
      // cannot be what makes this pass.
      val impostor = LibraryArtifact.framed("syslib 1 4", "junk")
      val mixed =
        FakeAr(LibraryArtifact.codeMember -> impostor,
          LibraryArtifact.metadataMember -> LibraryArtifact.frame("the real one"))

      LibraryArtifact.metadataOf("x.syslib", mixed) shouldBe Right("the real one")
    }

    "keeps one that holds when the metadata itself contains the marker" in {
      // A library whose own source spelled out the marker would otherwise be one whose metadata was
      // found in the middle of itself. The length is what settles it: the *first* marker is the real
      // one, and everything after it is payload counted in bytes.
      val awkward = "before " + new String(LibraryArtifact.framed("syslib 99 4", "junk"), UTF_8) + " after"

      LibraryArtifact.metadataOf("x.syslib", artifact(awkward)) shouldBe Right(awkward)
    }

    "refuses a file that is not an archive rather than reading it as one" in {
      LibraryArtifact.metadataOf("x.syslib", "not a library at all\n".getBytes) match
        case Left(err) => err should include("is not a sysl library")
        case Right(_)  => fail("a foreign file was read as a library")
    }

    "refuses an archive that carries no metadata, which is any other library" in {
      // A `.a` from a C project is a real archive full of real objects, and every part of reading it
      // works until the part that looks for something only we put there.
      LibraryArtifact.metadataOf("x.syslib", FakeAr("foreign.o" -> Array[Byte](7, 7))) match
        case Left(err) => err should include("carries no metadata")
        case Right(_)  => fail("an archive with no metadata was read as a library")
    }

    "refuses one built by a different compiler, and says to rebuild it" in {
      val stale = LibraryArtifact.framed(s"syslib ${LibraryArtifact.Version + 1} 0")

      LibraryArtifact.metadataOf("x.syslib", FakeAr(LibraryArtifact.metadataMember -> stale)) match
        case Left(err) => err should include("rebuild it with 'sysl build-lib'")
        case Right(_)  => fail("an artifact from another format version was accepted")
    }

    "refuses a truncated one rather than handing back half the metadata" in {
      // The version travels from the constant rather than being written out, so that a bump to the
      // container format leaves this testing truncation instead of quietly testing the version
      // check that now fires ahead of it.
      val short = LibraryArtifact.framed(s"syslib ${LibraryArtifact.Version} 500", "short")

      LibraryArtifact.metadataOf("x.syslib", FakeAr(LibraryArtifact.metadataMember -> short)) match
        case Left(err) => err should include("truncated")
        case Right(_)  => fail("a truncated artifact was accepted")
    }
  }

  "the artifact a real toolchain writes" - {

    // Everything above reads a container this suite assembled. This is the one that reads a container
    // `clang` and `llvm-ar` produced, which is the only way to find out whether the metadata survives
    // the round trip through an object file at all — a constant nothing refers to is exactly the kind
    // of thing a compiler is entitled to delete.

    /** A library built and archived for a given machine: the artifact's bytes, the metadata that went
     * in, and the metadata that came back out.
     */
    def roundTrip(target: Target): (Array[Byte], String, Either[String, String]) = {
      val ar = Toolchain.findAr(None) match
        case Right(path) => path
        case Left(why)   => cancel(why)

      val (ir, meta) = LibraryArtifact.build(sources, target) match
        case Right(r)  => r
        case Left(err) => fail(s"the library did not build: $err")

      val staging  = createTempDirectory("sysl-artifact-")
      val code     = s"$staging/${LibraryArtifact.codeMember}"
      val metadata = s"$staging/${LibraryArtifact.metadataMember}"
      val out      = s"$staging/demo${LibraryArtifact.extension}"

      val built =
        for
          _ <- Toolchain.compileObject(ir, code, target)
          _ <- Toolchain.compileObject(LibraryArtifact.metadataIr(meta, target), metadata, target)
          _ <- Toolchain.archive(List(code, metadata), out, ar)
        yield readBytes(out)

      val bytes = built match
        case Right(b)  => b
        case Left(err) => fail(s"the artifact was not built: $err")

      List(code, metadata, out, staging).foreach(p => try deleteFile(p) catch case _: Exception => ())

      (bytes, meta, LibraryArtifact.metadataOf(out, bytes))
    }

    "carries metadata that survives being buried in an object file" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val (_, wrote, read) = roundTrip(Target.default)

      // Byte-for-byte, because anything less passes for a payload truncated at its first NUL or
      // padded by the assembler, and both of those decode far enough to look fine.
      read shouldBe Right(wrote)
    }

    "carries it out of one cross-built for a machine whose objects this one cannot even run" in {
      // The gap this closes is real and was open: every other test here runs on the machine the suite
      // is on, so the ELF half of every per-format decision — the section the metadata sits in, the
      // long-name convention `llvm-ar` writes, the archive layout `Ar` walks — had never once been
      // executed. Building for Linux from anywhere exercises all three without needing a Linux.
      assume(Toolchain.clangAvailable, "clang not available")

      val elf = if Target.default == Target.x86_64Linux then Target.aarch64MacOS else Target.x86_64Linux
      val (bytes, wrote, read) = roundTrip(elf)

      read shouldBe Right(wrote)

      // And it really was cross-built: the compiled member is in the other machine's object format,
      // so this cannot be passing by having quietly produced a host artifact.
      val code = Ar.members(bytes).map(_.find(_.name == LibraryArtifact.codeMember).map(_.body.take(4)))

      code.map(_.map(_.toList)) shouldBe
        Right(Some(if elf.os == Os.Linux then List[Byte](0x7f, 'E', 'L', 'F') else List[Byte](-49, -6, -19, -2)))
    }
  }

  /** A library whose one function prints — which is what reaches the shipped library's own
   * `printi`, `printc` and `putbytes` from inside a library build.
   */
  private def printing(module: String, fn: String): (String, String) =
    LibraryArtifact.build(List(Source(s"$module/lib.sysl",
      s"module $module\n\n$fn(n: int)\n    print(n)\nend $fn\n", List(module)))) match
      case Right(r)  => r
      case Left(err) => fail(s"the library did not build: $err")

  "what a library does NOT compile, however much it uses it" - {

    "the shipped library's own functions are declared, not defined a second time here" in {
      // A library that prints reaches `printi` and `putbytes` exactly as a program does, and
      // emitting them would put a copy of the printing surface in every artifact.
      val (ir, meta) = printing("say", "hello")

      LibraryArtifact.read("say.syslib", meta, Target.default) match
        case Right((_, syms, _)) => syms shouldBe Set("say$hello")
        case Left(err)           => fail(err)

      ir should include("define void @say$hello(")
      ir should include(s"declare void @${Library.key("printi")}(")
      ir should not include s"define void @${Library.key("printi")}("
    }

    "so two libraries that both print can be linked into one program" in {
      // Before the split above they could not: each artifact defined `printi`, `printc` and
      // `putbytes`, and the linker refused the pair with three duplicate symbols. The consuming
      // program defines them once, reached through the very bodies that called for them.
      assume(Toolchain.clangAvailable, "clang not available")

      val built = List(printing("say", "hello"), printing("shout", "loud"))
      val objects = built.map { (ir, _) =>
        val obj = createTempFile("sysl-test-", ".o")

        Toolchain.compileObject(ir, obj) match
          case Left(err) => fail(s"a library did not assemble: $err")
          case Right(_)  => obj
      }
      val trees = built.flatMap { (_, meta) =>
        LibraryArtifact.read("x.syslib", meta, Target.default) match
          case Right((t, _, _)) => t
          case Left(err)        => fail(err)
      }
      val syms = Set("say$hello", "shout$loud")
      val emitted =
        Compiler.compiledWith(List(Source("<input>", "say.hello(1)\nshout.loud(3)\nprint(2)")),
          trees, Target.default, syms) match
          case Right(built) => built
          case Left(err)    => fail(err)

      val exe = createTempFile("sysl-test-", "")
      // The library's own C goes on the line too: a hosted program's entry point calls into it
      // (`Codegen.genStackGuard`), so the sysl half alone is not a program.
      val cs  = StdNative.objects()
      val ran = Toolchain.build(emitted.ir, exe, Target.default, objects ::: cs, links = emitted.links).map { _ =>
        val r = exec(List(exe))

        (r.exitCode, r.stdout)
      }

      objects.foreach(deleteFile)
      StdNative.clean(cs)
      deleteFile(exe)
      ran shouldBe Right((0, "1\n3\n2\n"))
    }
  }

  /** A library built **on another library** — what `--lib` means at `build-lib` (`reference/ffi.md
   * § A library may carry C`). `sdl3-ttf` is the case in the org: it declares `Font` in terms of
   * `sdl3`'s `Surface`, so without the other library's declarations it does not compile at all.
   *
   * The claim worth pinning is the same one separate compilation rests on everywhere else: **a
   * library defines its own modules and nobody else's.** Folded in with the dependent's own files the
   * dependency would be *its* module, and the artifact would carry a second copy of a compiled half
   * somebody else already shipped — which builds, archives, and is a duplicate symbol at whatever
   * program later links both. That is invisible on a Mach-O link, so it is checked here, in the IR,
   * where it is a difference between two words.
   */
  "a library built on another library" - {

    val base =
      """module base
        |
        |struct Colour
        |    red: int
        |end Colour
        |
        |brighten(c: Colour) -> Colour = Colour(c.red + 1)
        |""".stripMargin

    val skin =
      """module skin
        |
        |import base.{Colour, brighten}
        |
        |twice(c: Colour) -> Colour = brighten(brighten(c))
        |""".stripMargin

    val baseSource = List(Source("base/lib.sysl", base, List("base")))
    val skinSource = List(Source("skin/lib.sysl", skin, List("skin")))

    /** The dependency as a consumer would receive it: an artifact, read back into trees. */
    def baseTrees: List[Program] =
      LibraryArtifact.build(baseSource) match
        case Left(err) => fail(s"the dependency did not build: $err")
        case Right((_, meta)) =>
          LibraryArtifact.read("base.syslib", meta, Target.default) match
            case Right((trees, _, _)) => trees
            case Left(err)            => fail(s"the dependency's metadata did not read back: $err")

    def onto(libraries: List[Source] = Nil, trees: List[Program] = Nil): (String, Set[String]) =
      LibraryArtifact.build(skinSource, Target.default, Set.empty, None, Nil, SearchPaths.none,
        libraries, trees) match
        case Left(err) => fail(s"the dependent did not build: $err")
        case Right((ir, meta)) =>
          LibraryArtifact.read("skin.syslib", meta, Target.default) match
            case Right((_, syms, _)) => (ir, syms)
            case Left(err)           => fail(s"the dependent's metadata did not read back: $err")

    def defines(ir: String, name: String): Boolean =
      ir.linesIterator.exists(l => l.startsWith("define ") && l.contains(s"@$name("))

    def declares(ir: String, name: String): Boolean =
      ir.linesIterator.exists(l => l.startsWith("declare ") && l.contains(s"@$name("))

    "compiles against a dependency given as a source root" in {
      val (ir, _) = onto(libraries = baseSource)

      defines(ir, "skin$twice") shouldBe true
    }

    "compiles against one given as an artifact, which is the other half of what --lib takes" in {
      val (ir, _) = onto(trees = baseTrees)

      defines(ir, "skin$twice") shouldBe true
    }

    "declares the dependency's compiled half rather than emitting a second copy of it" in {
      val (ir, _) = onto(libraries = baseSource)

      defines(ir, "base$brighten") shouldBe false
      declares(ir, "base$brighten") shouldBe true
    }

    "advertises only its own, so a program is never told this artifact holds the other's" in {
      // What the metadata says is what a consuming compilation stops emitting for itself. A name
      // advertised here that this artifact does not define is a link that fails in somebody else's
      // program, with nothing in either library to point at.
      val (_, syms) = onto(libraries = baseSource)

      syms should contain("skin$twice")
      syms.filter(_.startsWith("base$")) shouldBe empty
    }
  }

  "a library may not sit in the module a program's own headerless files are in" in {
    // The root module has no name, so nothing that depended on this library could write a path to
    // what it declares — and its keys would be the consuming program's own.
    LibraryArtifact.build(List(Source("lib.sysl", "double(n: int) -> int = n * 2\n", Nil))) match
      case Left(err) => err should include("is reached by naming its module")
      case Right(_)  => fail("a library with no module of its own produced an artifact")
  }

  "a library that does not check is refused before anything is written" in {
    // Otherwise the artifact ships anyway and every program that links against it is handed a
    // diagnostic pointing into somebody else's source.
    LibraryArtifact.build(List(Source("demo/bad.sysl", "module demo\n\nf() -> int = \"no\"\n", List("demo")))) match
      case Left(err) => err should include("int")
      case Right(_)  => fail("a library that does not type-check produced an artifact")
  }

  /** The library built to a real object file, and a program compiled against it exactly as the
   * driver does it — decoded metadata for the trees, the unpacked object handed to the linker.
   */
  private def linked(program: String): (String, Either[String, (Int, String)]) = {
    val (ir, _)          = built
    val (trees, syms, _) = metadata
    val obj              = createTempFile("sysl-test-", ".o")

    Toolchain.compileObject(ir, obj) match
      case Left(err) => fail(s"the library did not assemble: $err")
      case Right(_)  => ()

    val emitted = Compiler.compiledWith(List(Source("<input>", program)), trees, Target.default, syms) match
      case Right(built) => built
      case Left(err)    => fail(err)

    val exe = createTempFile("sysl-test-", "")
    val cs  = StdNative.objects()
    val ran =
      Toolchain.build(emitted.ir, exe, Target.default, obj :: cs, links = emitted.links).map { _ =>
        val r = exec(List(exe))
        (r.exitCode, r.stdout)
      }

    deleteFile(obj)
    StdNative.clean(cs)
    deleteFile(exe)
    (emitted.ir, ran)
  }

  "a program linked against the artifact" - {

    "declares the precompiled half rather than defining it a second time" in {
      // Defining it here as well is a duplicate symbol at the link, which is the failure this
      // whole mechanism exists to avoid.
      val (ir, _) = linked("print(demo.double(21))")

      ir should include("declare i32 @demo$double(i32)")
      ir should not include "define i32 @demo$double("
    }

    "defines a generic here, at each type the program uses it at" in {
      val (ir, _) = linked("print(demo.larger(3, 7))\nprint(demo.larger(\"a\", \"b\"))")

      ir should include("define i32 @demo$larger.int(")
      ir should include regex """define \{ ptr, ptr, i64 \} @demo\$larger\.string\("""
    }

    "runs, with the precompiled body coming from the library's object file" in {
      assume(Toolchain.clangAvailable, "clang not available")

      linked("print(demo.double(21))\nprint(demo.larger(3, 7))")._2 shouldBe Right((0, "42\n7\n"))
    }

    /** An operator on a library's own type reaches a member of that library, and reaching one is
     * what brings it into the program at all: a library declaration nothing names is neither
     * analyzed nor emitted, so a name resolved without being recorded leaves a call to a function
     * the program never compiled. The three spellings below are three separate resolutions —
     * a comparison chain and a compound assignment hand the method over as a name rather than
     * building a call around it — and only the middle one used to keep the record.
     */
    "reaches the member an operator on a library type dispatches to" in {
      val (ir, _) = linked(
        """var a = demo.Money(5)
          |var b = demo.Money(7)
          |
          |print(a < b)
          |print(a + b == demo.Money(12))
          |
          |a += b
          |
          |print(a == demo.Money(12))
          |""".stripMargin)

      // The symbol and its parameters are the claim — that the *library's* definition is what a
      // program links to. A `bool` result also carries the target's widening (`CAbi.extension`),
      // which is a fact about the convention rather than about where the body came from.
      ir should include("@demo$Money.lt(")
      ir should include("@demo$Money.eq(")
      ir should include regex """declare %struct\.demo\$Money @demo\$Money\.add\("""
    }

    "and runs, with those bodies coming from the library's object file too" in {
      assume(Toolchain.clangAvailable, "clang not available")

      linked(
        """var a = demo.Money(5)
          |
          |print(a < demo.Money(7))
          |
          |a += demo.Money(7)
          |
          |print(a == demo.Money(12))
          |""".stripMargin)._2 shouldBe Right((0, "true\ntrue\n"))
    }

    "and the library carries no entry point of its own to collide with the program's" in {
      // A `main` in the library's object would be a duplicate symbol in every program that linked
      // it, and the linker's complaint names neither the library nor the cause.
      built._1 should not include "define i32 @main("
    }
  }

  /** `LibraryArtifact.collisions`, on the one member name that would evict the library itself.
   *
   * It is pinned here rather than through the CLI because `reference/ffi.md § A library may carry
   * C` takes a tree's C from a module or its root and from nowhere else, so a plain `sysl/`
   * directory of C is skipped before this is ever consulted. **The guard is still live for the one
   * build whose modules really are `sysl/…`** — a `build-lib --std` over the standard library — and
   * that build is far too expensive to reach for an assertion about a string.
   */
  "the member name a library's own compiled half uses" - {

    "is refused to a C file that would take it" in {
      val clash = Source("library/sysl/code.c", "int f(void) { return 0; }\n", List("sysl"))

      LibraryArtifact.nativeMember(clash) shouldBe LibraryArtifact.codeMember

      LibraryArtifact.collisions(List(clash)) match
        case Some(why) => why should include(LibraryArtifact.codeMember)
        case None      => fail("a C file mapping to the code member was allowed through")
    }

    "and so is one that would take the metadata member's" in {
      val clash = Source("library/sysl/smeta.c", "int f(void) { return 0; }\n", List("sysl"))

      LibraryArtifact.collisions(List(clash)) shouldBe defined
    }

    "while the same file one directory over is ordinary" in {
      val fine = Source("library/sysl/text/code.c", "int f(void) { return 0; }\n",
        List("sysl", "text"))

      LibraryArtifact.nativeMember(fine) shouldBe "sysl.text.code.o"
      LibraryArtifact.collisions(List(fine)) shouldBe None
    }
  }

  /** A library that forms a closure of its own, which is card `0229`'s condition and is easy to miss.
   *
   * `pick` takes a bare-arrow parameter, so `house` passing the named `fixed` to it wraps that
   * function in a closure exactly as an arrow literal would, and monomorphizes a `pick` at it. Both
   * symbols carry the closure's name, and that name is a **counter** in the compilation that lowered
   * it — so a consumer that lowered a closure of its own at the same number means something else
   * entirely by `$closure4`.
   *
   * The standard module shipped in 0.0.70 with exactly this pair, and `nm` said the whole of it:
   *
   * {{{
   *                  U _\$closure4.call
   * 0000000000053cc0 T _sysl.time\$resolve.\$closure4
   * }}}
   *
   * The library **declared** the closure's body — its key begins with the module separator, so
   * ownership read off the name filed it as something another library supplies, where the
   * declaration (`TFunc.module`) says it is the enclosing body's module's — while advertising the
   * instantiation that calls it. A program then declared the
   * instantiation rather than building one, and the artifact's body called straight into the
   * program's own closure: a different environment under a different body.
   *
   * **Structural rather than behavioural, because the behaviour is a coincidence of counters.** Two
   * compilations collide only where they happen to reach the same number, which makes a program that
   * prints the wrong answer a test of arithmetic nobody controls. What is always true is the rule:
   * a symbol named after a closure is this compilation's and does not leave its object file.
   */
  "a closure the library lowered for itself" - {

    "is compiled into the artifact rather than left for the linker" in {
      // Non-vacuous by construction: `house` forms one, so there is a body here to be about.
      closureSymbols(zone._1, "define") should not be empty
      closureSymbols(zone._1, "declare") shouldBe Nil
    }

    "and is internal, so a consumer's closure of the same name is a different symbol" in {
      closureDefinitions(zone._1)
        .filterNot(l => l.startsWith("define internal") || l.startsWith("define private")) shouldBe Nil
    }

    "and is not advertised, so a consumer builds its own instantiation" in {
      LibraryArtifact.read("zone.syslib", zone._2, Target.default) match
        case Right((_, precompiled, _)) =>
          // Discriminating: the library does advertise its ordinary functions, so an empty filter
          // below is the closure rule rather than an empty set.
          precompiled should contain("zone$house")
          precompiled.filter(Closures.mentioned) shouldBe empty
        case Left(err) => fail(err)
    }
  }
}
