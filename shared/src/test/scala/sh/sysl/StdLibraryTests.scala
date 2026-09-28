package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Where the standard module library sits, as seen from wherever the suite was started.
 *
 * `library/` is a **library project root** holding the single module `sysl`, so a module's name is
 * the path below it exactly as it is for a program (`reference/modules.md`) — which is why the root
 * is what is found here and `library/sysl` is not.
 */
object StdRoot {

  /** Asked of `Std.candidates` rather than of a list written out again here. The suite used to
   * carry its own copy of the search paths, which meant the one thing it could not notice was the
   * compiler looking somewhere else — and that is precisely what a rename of the directory does.
   */
  def root: Option[String] = Std.candidates(None).map(_._1).find(isDirectory)
}

/** How a compilation gets to the standard module's source, now that the compiler reads it off disk
 * rather than carrying a generated copy of it (`Std`).
 *
 * **This used to be the test that said the two copies agreed**, and there is only one copy now — so
 * what it asks instead is the question that replaced that one: *can the compiler find its library,
 * and is what it found the library?* The failure it guards against changed shape with the mechanism.
 * A generator that stopped running left a compiler working against source a reader could not see; a
 * resolution that goes wrong leaves a compiler that cannot compile anything at all, and has to say
 * so well enough to be fixed.
 */
class StdLibraryTests extends AnyFreeSpec with Matchers with TreeSupport {

  /** A file's place in the library: the module directories it sits under, then its own name. The
   * one thing that is true of a file wherever the library was found, which is what lets two readings
   * of it be compared at all.
   */
  private def place(s: Source): String =
    (s.dir.getOrElse(Nil) :+ Project.basename(s.name)).mkString("/")

  /** The resolution, driven over made-up inputs so that every branch of it is reachable from any
   * machine — the ambient one can only ever exercise whichever case happens to hold here.
   */
  private def resolve(named: Option[String], exe: Option[String], present: String*) =
    Std.rootOf(named, exe, present.map(root => s"$root/${Std.module}").toSet)

  "finding the library" - {

    "takes an installed compiler to the tree installed beside it" in {
      // The whole point of the change: `/opt/homebrew/bin/sysl` is a symlink, `executablePath`
      // resolves it to the Cellar, and the library is under that prefix in the ordinary Unix place.
      // Nothing is configured and nothing is carried.
      resolve(None, Some("/opt/homebrew/Cellar/sysl/0.0.2/bin/sysl"),
        "/opt/homebrew/Cellar/sysl/0.0.2/share/sysl/library") shouldBe
        Right("/opt/homebrew/Cellar/sysl/0.0.2/share/sysl/library")
    }

    "and two installs of different versions each find their own" in {
      // Which is what keying on the compiler's own location buys over a fixed path: the answer moves
      // with the binary, so an old sysl left in the Cellar compiles against the library it shipped
      // with rather than against the newest one on the machine.
      val both = Seq("/opt/sysl/0.0.1/share/sysl/library", "/opt/sysl/0.0.2/share/sysl/library")

      resolve(None, Some("/opt/sysl/0.0.1/bin/sysl"), both*) shouldBe Right(both.head)
      resolve(None, Some("/opt/sysl/0.0.2/bin/sysl"), both*) shouldBe Right(both.last)
    }

    "while a compiler run out of a checkout finds the tree it was built from" in {
      // The development case, and the one every run of this suite is actually in. The JVM build has
      // no executable path to speak of — its executable is `java` — so this is the only answer it
      // ever gets, which is why it has to work with nothing else present.
      resolve(None, None, "library") shouldBe Right("library")
    }

    "from a directory or two below it, since a suite need not run at the root" in {
      resolve(None, None, "../library") shouldBe Right("../library")
      resolve(None, None, "../../library") shouldBe Right("../../library")
    }

    "and still finds a tree that has not been renamed yet, at either kind of path" in {
      // The directory was `lib` until it was renamed, and copies of it are on disk in eleven other
      // repositories and inside every compiler already installed. Both spellings are searched for
      // that reason, so this asserts the compatibility rather than leaving it to be discovered by
      // whoever's checkout stops building.
      resolve(None, None, "lib") shouldBe Right("lib")
      resolve(None, None, "../lib") shouldBe Right("../lib")

      resolve(None, Some("/opt/sysl/0.0.40/bin/sysl"), "/opt/sysl/0.0.40/share/sysl/lib") shouldBe
        Right("/opt/sysl/0.0.40/share/sysl/lib")
    }

    "and prefers the new spelling where a tree carries both" in {
      // Which is the case a checkout is in for exactly as long as it takes to delete the old one:
      // `git mv` leaves nothing behind, but an artifact directory or a stray copy does.
      resolve(None, None, "lib", "library") shouldBe Right("library")
    }

    "and prefers the installed tree to whatever the working directory happens to hold" in {
      // Ordering is a guard, so it is asserted with both present rather than read off the list.
      resolve(None, Some("/usr/local/bin/sysl"), "/usr/local/share/sysl/library", "library") shouldBe
        Right("/usr/local/share/sysl/library")
    }

    "and a directory of that name that is not a sysl library is not a candidate at all" in {
      // The other half of that guard, and the one ordering cannot supply: both spellings are
      // ordinary directory names — a C project has a `lib`, and so does half of everything else — so
      // a compiler standing in someone's source tree would take theirs and fail somewhere far from
      // the cause. What is asked for is `<root>/sysl`, so one holding anything else is skipped.
      Std.rootOf(None, None, Set("library", "lib")) match
        case Right(found) => fail(s"a root with no '${Std.module}' in it resolved to $found")
        case Left(err)    => err should include("library")
    }
  }

  "SYSL_LIB" - {

    "names the library outright, beating everything else" in {
      resolve(Some("/tmp/mine"), Some("/usr/local/bin/sysl"),
        "/tmp/mine", "/usr/local/share/sysl/library", "lib") shouldBe Right("/tmp/mine")
    }

    "aimed at the module rather than at the root, says which of the two is wanted" in {
      // The mistake anybody would make once: `library/sysl` is the module, `lib` is the root, and they
      // differ by one segment. Told the wrong one, the message says which — rather than reporting
      // that a directory plainly sitting there is not a library.
      Std.rootOf(Some("/tmp/library/sysl"), None, Set("/tmp/library/sysl")) match
        case Right(found) => fail(s"the module directory should not have resolved as a root: $found")
        case Left(err) =>
          err should include("/tmp/library/sysl")
          err should include("above")
    }

    "and one that is not a library root at all is refused rather than fallen back from" in {
      // The rule a named `--std-lib` and a named `--ar` already take: someone who wrote down which
      // library to use is owed an error when it is not there, not a different library compiled
      // against underneath them. It is the only candidate that reports its own failure.
      resolve(Some("/tmp/typo"), None, "lib") match
        case Right(found) => fail(s"SYSL_LIB naming nothing should not have resolved to $found")
        case Left(err) =>
          err should include("/tmp/typo")
          err should include("SYSL_LIB")
    }
  }

  "a compiler that cannot find its library" - {

    "says so, and names every place it looked" in {
      // What replaces the guarantee the carried copy gave. A broken install is now possible, so the
      // difference between a broken install and a mystery is this list — and each path is asserted
      // rather than the message merely being non-empty.
      val err = resolve(None, Some("/opt/sysl/0.0.2/bin/sysl")).left.getOrElse(fail("should not have resolved"))

      err should include("/opt/sysl/0.0.2/share/sysl/library")
      err should include("  library (")
      err should include("  ../library (")
      err should include("  ../../library (")
      err should include("SYSL_LIB")
    }

    "and lists them in the order it tried them" in {
      val err = resolve(None, Some("/opt/sysl/0.0.2/bin/sysl")).left.getOrElse(fail("should not have resolved"))

      err.indexOf("/opt/sysl/0.0.2/share/sysl/library") should be < err.indexOf("  library (")
      err.indexOf("  library (") should be < err.indexOf("  ../library (")
      err.indexOf("  ../library (") should be < err.indexOf("  ../../library (")

      // And the spelling kept for compatibility is listed after the one a reader should be using,
      // so the list reads as an order of preference rather than as two equal answers.
      err.indexOf("  ../../library (") should be < err.indexOf("  lib (")
    }
  }

  "the library this compiler resolved" - {

    "is the one in this checkout" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      // The development-loop claim, and the one the whole suite rests on: what every test compiles
      // against is the tree a reader can open and edit. Compared by place and by text, so a file
      // added to the tree and not seen by the compiler fails here.
      val onDisk = Project.collect(StdRoot.root.get, Some(Target.default.os)).map(s => place(s) -> s.text).toMap

      Std.sources(Target.default.os).map(s => place(s) -> s.text).toMap shouldBe onDisk
    }

    "and names each file by its place in the library rather than by where it was read from" in {
      // So that a diagnostic naming a library file reads the same on every machine. See `Std.named`
      // — this is the claim the full suite caught being broken, in two library pages that quote a
      // refusal about a private field of `sysl.posix.threads.Mutex`.
      Std.sources(Target.default.os).map(_.name) shouldBe Std.sources(Target.default.os).map(s => s"${Std.Prefix}/${place(s)}")
    }

    "the same name whether it was read from a checkout or from an install" in {
      // The discriminating case, which the checkout alone cannot show: the two roots differ in every
      // segment but the last, and the names have to come out identical.
      val checkout = Std.named(Source("library/sysl/print.sysl", "x", List("sysl")))

      val installed =
        Std.named(Source("/opt/homebrew/Cellar/sysl/0.0.3/share/sysl/library/sysl/print.sysl", "x",
          List("sysl")))

      checkout.name shouldBe "library/sysl/print.sysl"
      installed.name shouldBe checkout.name
    }

    "and the same name out of a tree that still has the old directory name" in {
      // The case the constant prefix exists for. Read off the root's own basename, this file would
      // be `lib/sysl/print.sysl` out of a compiler installed last week and `library/sysl/print.sysl`
      // out of a moved checkout — and a documentation page quoting the diagnostic could only be
      // right for one of them.
      Std.named(Source("lib/sysl/print.sysl", "x", List("sysl"))).name shouldBe
        "library/sysl/print.sysl"
    }

    "in a fixed order, decided by the library rather than by a directory listing" in {
      Std.sources(Target.default.os).map(place) shouldBe Std.sources(Target.default.os).map(place).sorted
    }

    "with every file in the module its own location says, however deep" in {
      // The tree claim, asked of the real library rather than of a string helper: a file's `dir` is
      // exactly the directories between the root and it, so its path has to end with them.
      for s <- Std.sources(Target.default.os) do withClue(s"${s.name}: ")(s.name should endWith(place(s)))

      Std.sources(Target.default.os).foreach(_.dir.get.head shouldBe Std.module)

      // And the deep case is genuinely exercised — `sysl.math.complex` is two below the standard
      // module — so the claim above is not being met by a flat directory.
      Std.sources(Target.default.os).map(_.dir.get.length).max should be >= 3
    }
  }

  "where the library was found does not change what it is" - {

    "so the same tree read from somewhere else fingerprints the same" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      // **This is the claim the whole change rests on.** The artifact is keyed by this fingerprint,
      // and an installed compiler reads the library from its own prefix while a checkout reads it
      // from `library/`. If the paths reached the hash, every install would key an artifact of its own
      // and an artifact built anywhere would be refused everywhere else.
      val copy = createTempDirectory("sysl-lib-copy-")

      try
        copyTree(StdRoot.root.get, copy)

        val moved = Project.collect(copy, Some(Target.default.os))

        moved.map(place).sorted shouldBe Std.sources(Target.default.os).map(place)

        // The C moves with it, and the fingerprint is over both (`Std.files`) — so a library whose
        // shims were left behind must not hash as the one that has them.
        val movedC = Project.cSources(copy, Some(Target.default.os))

        LibraryArtifact.fingerprint(moved ::: movedC) shouldBe Std.fingerprint(Target.default.os)
      finally discardTree(copy)
    }

    /** `Std.fingerprintOf` is the same hash read off a **named** root, and `build-lib --std` names
     * the tree it was pointed at with it rather than with `Std.fingerprint` — which is the tree this
     * machine *resolves*, and need not be the same one at all.
     *
     * **That the two agree on one tree is the whole of what makes re-keying safe**, and it is a
     * property of the hash rather than something two call sites keep in step: `fingerprint` reduces
     * each file to its `place` and its text and sorts by `place` itself, so neither the order the
     * files arrive in nor the renaming `Std.collect` applies can reach it.
     */
    "and the same hash read off a named root agrees with it" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val copy = createTempDirectory("sysl-lib-named-")

      try
        copyTree(StdRoot.root.get, copy)
        Std.fingerprintOf(copy, Target.default.os) shouldBe Std.fingerprint(Target.default.os)
      finally discardTree(copy)
    }

    /** The other half, and the defect it closes: a tree that is *not* the resolved library must key
     * an artifact of its own. Before this, `build-lib --std` in a checkout with an installed sysl
     * wrote the checkout's library under the installed library's key — invisible while the compiled
     * tree was a superset of the resolved one, an undefined symbol or silently the wrong
     * implementation the day it was not.
     */
    "so a different tree keys a different artifact" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val copy = createTempDirectory("sysl-lib-other-")

      try
        copyTree(StdRoot.root.get, copy)
        writeFile(s"$copy/sysl/marker.sysl", "module sysl\n\nprivate[sysl] a_marker() -> int = 1\n")

        val theirs = Std.fingerprintOf(copy, Target.default.os)

        theirs should not be Std.fingerprint(Target.default.os)

        LibraryArtifact.stdDefault(Target.default, Allocator.c, Some(theirs)) should not be
          LibraryArtifact.stdDefault(Target.default, Allocator.c)
      finally discardTree(copy)
    }

    // And naming nothing is the compilation's own question, unchanged: it asks about the library it
    // resolved, which is what every consumer of the artifact does.
    "while naming no root at all is still the resolved library's key" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      LibraryArtifact.stdDefault(Target.default, Allocator.c) shouldBe
        LibraryArtifact.stdDefault(Target.default, Allocator.c, Some(Std.fingerprint(Target.default.os)))
    }

    "while an edit to it does, so a changed library cannot take a stale artifact" in {
      // The other half, and the reason editing the library is now something anybody can do: the
      // artifact's path holds this fingerprint, so a changed file *is* a different path. Nothing has
      // to be invalidated and a compiler running against the unedited library is unaffected.
      val first = Std.files(Target.default.os).headOption.getOrElse(fail("the library has no files"))
      val edited = Source(first.name, first.text + "\n", first.dir.getOrElse(Nil)) :: Std.files(Target.default.os).tail

      LibraryArtifact.fingerprint(edited) should not be Std.fingerprint(Target.default.os)
    }
  }

  "the library as the driver reads it" - {

    "puts every file in the module its own header names" in {
      // The header and the directory both say it, and the driver is what checks they agree — so
      // this is the same question `build-lib library` would ask, asked without building anything.
      for source <- Std.sources(Target.default.os) do
        SyslParser.parse(source) match
          case Right(p)  => p.module.map(_.show) shouldBe Some(source.dir.get.mkString("."))
          case Left(err) => fail(s"${source.name} does not parse: $err")
    }

    "and every module those headers name is one the library says it declares" in {
      Std.sources(Target.default.os).map(_.dir.get.mkString(".")).toSet shouldBe Library.modules.toSet
    }
  }

  /** **A library that is found and cannot be used is a refusal, never an exception.** Each of these
   * used to be a `sys.error` deep inside the reading of the library, which reached the user of
   * `sysl build` as `Exception in thread "main" java.lang.RuntimeException` and a stack trace
   * through `Stdlib.found` — reachable by nothing more exotic than a `SYSL_LIB` pointed at a copy of
   * the library with one bad line in it.
   *
   * The library itself is resolved once per process (`Std.root`), so these hand the readers inputs
   * of their own rather than swapping the library under the suite; `Stdlib.refusing` is the catch
   * `resolve` puts around all of them, and `Main` prints its `Left` as `sysl: error:` with exit 1.
   */
  "a library that cannot be used" - {

    val target = Target.default

    def file(name: String, text: String): Source =
      Source(s"${Std.Prefix}/sysl/$name", text, List(Std.module))

    "is refused when a file does not parse, with the parser's own diagnostic under one sentence" in {
      val problem = intercept[Std.Unusable](
        Std.parsedFrom(List(file("std.sysl", "module sysl\n)))) broken\n")), target)).getMessage

      problem.linesIterator.next() should startWith(
        "the standard module does not parse, so nothing can be compiled against it")

      // Located as any file is, by its place in the library — and the diagnostic is the parser's,
      // not a paraphrase of it.
      problem should include("error: expression expected")
      problem should include("--> library/sysl/std.sysl:2:1")

      // **Not wrapped in `rebuildFailure`'s advice**: building the artifact and compiling the source
      // in both parse this same library, so recommending either sends the reader round a loop.
      problem should not include "build-lib"
      problem should not include "--no-std-lib"
    }

    "and names every file that does not parse, not only the first" in {
      val problem = intercept[Std.Unusable](
        Std.parsedFrom(List(file("a.sysl", "module sysl\n)))\n"), file("fine.sysl", "module sysl\n"),
                            file("b.sysl", "module sysl\n\n]]]\n")), target)).getMessage

      problem should include("--> library/sysl/a.sysl:2:1")
      problem should include("--> library/sysl/b.sysl:3:1")
    }

    "while a library that parses is handed back whole, in the order it came" in {
      val files = List(file("a.sysl", "module sysl\n"), file("b.sysl", "module sysl\n"))

      Std.parsedFrom(files, target).map(_.source) shouldBe files
    }

    "is refused when its module directory holds no source" in {
      val root = createTempDirectory("sysl-empty-lib-")

      try
        Project.makeDirectories(s"$root/${Std.module}")

        intercept[Std.Unusable](Std.collectAt(root, target.os)).getMessage shouldBe
          s"the library at $root holds no sysl source files"
      finally discardTree(root)
    }

    "is refused when a 'c const' of it cannot be measured, with the C compiler's diagnostic rendered" in {
      assume(Toolchain.findClang(target, None).isRight, "no C compiler for this machine")

      val units = Std.parsedFrom(
        List(file("zz.sysl", "module sysl\n\nc const\n    NOPE: int = \"SYSL_NO_SUCH_MACRO\"\n")), target)
      val problem = intercept[Std.Unusable](Stdlib.lowered(units, target, None)).getMessage

      problem.linesIterator.next() shouldBe "the standard module's 'c const' could not be measured"
      problem should include("--> library/sysl/zz.sysl:4:5")
      problem should include("SYSL_NO_SUCH_MACRO")
      // It printed the case class's `toString` before — `Diagnostic(the C compiler refused …`.
      problem should not include "Diagnostic("
    }

    "is answered by resolve as the refusal it carries" in {
      Stdlib.refusing[Unit](throw Std.Unusable("the library at x holds no sysl source files")) shouldBe
        Left("the library at x holds no sysl source files")
    }

    "while any other exception is left to escape, since it is a defect in the compiler" in {
      intercept[IllegalStateException](Stdlib.refusing[Unit](throw IllegalStateException("a defect")))
    }
  }

  "two files of one module reach each other with nothing imported" in {
    // `Display.display` names `Writer`, which the *other* file declares, and `reference/modules.md
    // § The module graph is acyclic` is why that needs no import: a module's members are one set
    // however many files they came from. It is also what a one-file standard module would never
    // have said.
    val declared = Std.parsed(Target.default).map(p => place(p.source) -> p.body).toMap

    declared("sysl/write.sysl").collect { case t: TraitDecl => t.name } shouldBe List("Writer")
    declared("sysl/display.sysl").collect { case t: TraitDecl => t.name } shouldBe List("Display", "Integer")

    // Which is the claim, rather than "the library imports nothing": a file may well have a sibling
    // module of the library to import, and none of them has itself. The two import forms spell the
    // module differently — a selector list or a wildcard names it outright, and a bare path names it
    // with the imported name still on the end — so which segments are the module is read off the
    // form.
    for p <- Std.parsed(Target.default); i <- p.body.collect { case i: ImportDecl => i } do
      val named = if i.selectors.nonEmpty || i.wildcard then i.path else i.path.init

      named.mkString(".") should not be p.module.map(_.show).get
  }

}
