package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** What a build asks of LLVM **beyond the level** — link-time optimization and profile guidance —
 * at the seams that decide it: what an argument list parses to, what a manifest may say, and what
 * ends up on the clang command lines a build drives (`Pipeline`).
 *
 * `OptimizeCliTests` is this file's twin and covers the level itself; the shape of the fixtures is
 * taken from it deliberately, because the precedence rule these two levers follow is the same one
 * and a reader comparing them should not have to read two different suites.
 *
 * **The evidence is the command line and not a stopwatch.** Whether `-flto=thin` makes a given
 * program faster is a measurement, and measurements belong in the benchmark that made them; what a
 * suite can settle is whether the flag a person asked for reached every clang the build ran, which
 * is the thing that has silently not happened.
 */
class PipelineCliTests extends AnyFreeSpec with Matchers {

  /** Through the driver's own entry, so what is asked here is what a shell asks. A refusal is
    * scopt's and goes to stderr with a usage message behind it; a suite that let that through would
    * bury every other suite's output under it.
    */
  private def parse(args: String*): Option[Config] = Console.withErr(Discarded)(parseArgs(args))

  private def driver(cfg: Config): Int = sh.sysl.execute(cfg)

  private def cli(cfg: Config): Int = Console.withOut(Discarded)(driver(cfg))

  private def manifest(name: String, says: String): String =
    s"""package { name = "$name", version = "0.1.0" }
       |$says
       |""".stripMargin

  /** A project with a program in it, and — where `carriesC` — a C file beside the module, so that a
   * build drives the *C* compiler as well as the link and both traces can be read.
   */
  private def project(says: String, carriesC: Boolean = false): String = {
    val root = createTempDirectory("sysl-pipeline-project-")

    writeFile(s"$root/${PackageConfig.FileName}", manifest("prog", says))
    writeFile(s"$root/main.sysl", "main()\n    print(1)\n")

    if carriesC then writeFile(s"$root/shim.c", "int sysl_pipeline_shim(void) { return 7; }\n")
    root
  }

  private def dependency(says: String): String = {
    val root = createTempDirectory("sysl-pipeline-package-")

    writeFile(s"$root/${PackageConfig.FileName}", manifest("geom", says))
    createDirectories(s"$root/geom")
    writeFile(s"$root/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")
    root
  }

  /** Everything the build traced, which is every clang command line it ran — with the run cache off,
   * since a replayed binary runs no clang at all and would leave a suite asserting about whichever
   * run last filled the slot.
   */
  private def clang(cfg: Config): String = {
    val notes  = new java.io.ByteArrayOutputStream
    val status = RunCache.disabledFor(
      Console.withOut(Discarded)(Console.withErr(notes)(driver(cfg.copy(verbose = true)))))

    if status != 0 then fail(s"the driver exited with $status:\n${notes.toString}")

    notes.toString
  }

  private def building(root: String, command: String = "build", cfg: Config => Config = identity): String =
    clang(cfg(Config(command = command, file = root,
                     output = Option.when(command == "build")(s"$root/out"))))

  /** The traced command lines of one kind — `compile:` for a C file, `link:` for the link.
    *
    * **The trace is prefixed by the compiler's own name**, so a filter anchored at the start of the
    * line matches nothing and an assertion over "all of them" then passes vacuously. That is the
    * shape of mistake this helper exists to make once instead of at every case.
    */
  private def drivenBy(notes: String, kind: String): List[String] =
    notes.linesIterator.filter(_.contains(s" $kind ")).toList

  /** The read of a manifest, and the sentence a bad one is refused with. */
  private def read(says: String): PackageConfig =
    PackageConfig.read(manifest("thing", says)).fold(e => fail(e), identity)

  private def refused(says: String): String =
    PackageConfig.read(manifest("thing", says)).fold(identity, _ => fail(s"'$says' was accepted"))

  "link-time optimization" - {

    "what an argument list says" - {

      "a build that asks for none asks for none, and the pipeline is empty" in {
        parse("build", "prog.sysl").map(_.lto) shouldBe Some(None)
        parse("build", "prog.sysl").map(_.pipeline.isEmpty) shouldBe Some(true)
      }

      "and each mode clang has may be named" in {
        for mode <- Toolchain.ltoModes do
          withClue(mode) {
            parse("build", "prog.sysl", "--lto", mode).map(_.lto) shouldBe Some(Some(mode))
            parse("build", "prog.sysl", "--lto", mode).map(_.pipeline.flags) shouldBe
              Some(List(s"-flto=$mode"))
          }
      }

      // Refused by the parser rather than by clang, because the failure otherwise arrives from
      // inside a tool the reader did not invoke, naming a flag they did not type.
      "a mode clang does not have is refused where it was typed" in {
        parse("build", "prog.sysl", "--lto", "thick") shouldBe None
      }

      // It belongs to every command that produces code, for the reason the level does: a flag that
      // reached `build` and not `test` would be a project measured one way and shipped another.
      "every command that emits code accepts it" in {
        for command <- List("run", "build", "build-lib", "test") do
          withClue(command) {
            parse(command, "prog.sysl", "--lto", "thin").map(_.lto) shouldBe Some(Some("thin"))
          }
      }
    }

    "what a manifest says" - {

      "the mode it names is the mode the project is linked with" in {
        read("""lto = "thin"""").lto shouldBe Some("thin")
        read("""lto = "full"""").lto shouldBe Some("full")
      }

      // What somebody writes who has not read which modes there are, and thin is the one they want.
      "'true' means thin, which is the answer for a project that has not thought about it" in {
        read("lto = true").lto shouldBe Some("thin")
      }

      // So that a fork of a manifest can turn it off without deleting the line.
      "'false' says the same as saying nothing" in {
        read("lto = false").lto shouldBe None
        read("""package { name = "thing", version = "0.1.0" }""").lto shouldBe None
      }

      "'lto' is a key this compiler knows, so it draws no unknown-key warning" in {
        read("""lto = "thin"""").warnings shouldBe empty
      }

      "a mode clang does not have is refused here rather than left to clang" in {
        val e = refused("""lto = "thick"""")

        e should include("""'lto = "thick"'""")
        e should include("thin or full")
        e should include(PackageConfig.FileName)
      }

      "and a block is refused with the same set, since there is nothing else it could mean" in {
        refused("""lto { mode = "thin" }""") should include("thin or full")
      }
    }

    "what reaches clang" - {

      // The link is where LTO happens, so this is the one command line it cannot be missing from.
      "the link carries the mode asked for, and carries nothing when none was" in {
        for mode <- Toolchain.ltoModes do
          withClue(mode) {
            Toolchain.linkCommand("prog.ll", Nil, "prog", Target.aarch64MacOS,
                                  pipeline = Pipeline(lto = Some(mode))) should contain(s"-flto=$mode")
          }

        Toolchain.linkCommand("prog.ll", Nil, "prog", Target.aarch64MacOS)
          .filter(_.startsWith("-flto")) shouldBe empty
      }

      // A driver flag, so it goes in front of the inputs it applies to, as the level does.
      "and states it before the module it applies to" in {
        val cmd = Toolchain.linkCommand("prog.ll", Nil, "prog", Target.x86_64Linux,
                                        pipeline = Pipeline(lto = Some("thin")))

        cmd.indexOf("-flto=thin") should be < cmd.indexOf("prog.ll")
      }

      "the mode a project's manifest names is what its build is linked with" in {
        assume(Toolchain.clangAvailable, "clang not available")

        building(project("""lto = "thin"""")) should include("-flto=thin")
      }

      // **The half that is the whole point of the feature.** A sysl program is one LLVM module
      // already, so LTO buys nothing for sysl's own code and everything for the seam between it and
      // the C a package carries — which is a different clang invocation, and the one a flag threaded
      // through the link alone would silently miss.
      "including the C the project carries, which is the seam LTO exists to cross" in {
        assume(Toolchain.clangAvailable, "clang not available")

        val notes     = building(project("""lto = "thin"""", carriesC = true))
        val compiling = drivenBy(notes, "compile:")

        withClue(notes) { compiling should not be empty }
        all(compiling) should include("-flto=thin")
      }

      "and a mode on the command line beats the project's key" in {
        assume(Toolchain.clangAvailable, "clang not available")

        val notes = building(project("""lto = "thin""""), cfg = _.copy(lto = Some("full")))

        notes should include("-flto=full")
        notes should not include "-flto=thin"
      }

      // A dependency's key is not applied, for the reason the level's is not — and one step stronger,
      // since LTO is a property of the link, which happens once for the whole program.
      "a dependency's key is not applied, and is not an error either" in {
        assume(Toolchain.clangAvailable, "clang not available")

        val geom = dependency("""lto = "full"""")
        val root = createTempDirectory("sysl-pipeline-app-")

        writeFile(s"$root/${PackageConfig.FileName}",
          s"""package { name = "app", version = "0.1.0" }
             |dependencies { g { path = "$geom" } }
             |""".stripMargin)
        writeFile(s"$root/main.sysl", "main()\n    print(geom.double(21))\n")

        clang(Config(command = "build", file = root, output = Some(s"$root/out"))) should
          not include "-flto"
      }

      "and a program built with it still answers what the program says" in {
        assume(Toolchain.clangAvailable, "clang not available")

        val path = createTempFile("sysl-pipeline-", ".sysl")
        writeFile(path, "main()\n    print(6 * 7)\n")

        try
          val out = new java.io.ByteArrayOutputStream
          val status = Console.withOut(out)(RunCache.disabledFor(
            driver(Config(command = "run", file = path, noStdLib = true, lto = Some("thin")))))

          status shouldBe 0
          out.toString shouldBe "42\n"
        finally deleteFile(path)
      }
    }

    // `sysl run` replays a cached binary without reaching clang, so a lever the key does not carry
    // is a lever that silently does nothing on the second run of an unchanged tree.
    "asking for it is a different program to the run cache" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val cache = createTempDirectory("sysl-pipeline-cache-")
      val root  = project("")

      def entries: Int =
        if isDirectory(s"$cache/sysl/run") then listFiles(s"$cache/sysl/run").length else 0

      RunCache.usingCache(cache) {
        cli(Config(command = "run", file = root)) shouldBe 0
        entries shouldBe 1

        cli(Config(command = "run", file = root, lto = Some("thin"))) shouldBe 0
        entries shouldBe 2
      }
    }
  }

  /** Profile-guided optimization, which is three steps and not one: build instrumented, run the
   * training set, merge the counters with `llvm-profdata`, and build again against the result.
   *
   * What sysl exposes is the first and the last of those; the merge is LLVM's own tool and is not
   * wrapped, because the profile a training set produces is several files and which of them belong
   * in one profile is the question the person training is answering.
   */
  "profile-guided optimization" - {

    /** The `llvm-profdata` **that clang itself would use**, asked of the clang a build here runs.
      *
      * ==Why the PATH is the wrong place to look, and how it fails==
      *
      * A `.profraw` carries a format version, and `llvm-profdata` reads only the version its own
      * LLVM writes. So the tool has to match the *compiler*, not the machine — and on a Mac those
      * are routinely different: Apple's clang writes version 10, and the `llvm-profdata` Homebrew's
      * LLVM puts on the PATH is from LLVM 23 and expects version 11. Merging with it fails with
      * `raw profile version mismatch`, which reads as a corrupt profile and is not one.
      *
      * `clang -print-prog-name=` is the question that cannot get this wrong: it asks the driver
      * where its own tools live, and Apple's answers with the one inside Xcode's toolchain, which
      * is nowhere on the PATH. It is also the line the documentation gives a reader, so a suite
      * that found the tool some other way would be testing a recipe nobody follows.
      */
    def profdata: Option[String] =
      Toolchain.findClang(Target.default).toOption
        .map(cc => try exec(Seq(cc, "-print-prog-name=llvm-profdata")).stdout.trim
                   catch case _: Exception => "")
        .filter(_.nonEmpty)
        .filter(p => try exec(Seq(p, "--version")).exitCode == 0 catch case _: Exception => false)

    "what an argument list says" - {

      "a build that asks for neither asks for neither" in {
        parse("build", "prog.sysl").map(_.pipeline) shouldBe Some(Pipeline.none)
      }

      "generating names the directory the counters are written into" in {
        parse("build", "prog.sysl", "--profile-generate", "/tmp/p").map(_.profileGenerate) shouldBe
          Some(Some("/tmp/p"))
      }

      "and using names the merged profile the optimizer reads" in {
        parse("build", "prog.sysl", "--profile-use", "/tmp/p.profdata").map(_.profileUse) shouldBe
          Some(Some("/tmp/p.profdata"))
      }

      // Clang takes both and instruments the build, which is the opposite of what the second flag
      // asked for and reports nothing about it — so the contradiction is refused where it was typed.
      "and the two ends of the workflow cannot be asked for at once" in {
        parse("build", "prog.sysl", "--profile-generate", "/tmp/p",
              "--profile-use", "/tmp/p.profdata") shouldBe None
      }

      // Paths are made absolute because the instrumented program writes its counters when it RUNS,
      // from whatever directory it is run in — a relative one scatters a training set.
      "the directory a training run writes into is absolute, wherever the program is run from" in {
        Pipeline(profileGenerate = Some("prof")).flags.head should startWith("-fprofile-generate=/")
      }
    }

    "what reaches clang" - {

      "an instrumented build says so on every command line it drives" in {
        assume(Toolchain.clangAvailable, "clang not available")

        val dir   = createTempDirectory("sysl-pipeline-profraw-")
        val notes = building(project("", carriesC = true), cfg = _.copy(profileGenerate = Some(dir)))
        val lines = drivenBy(notes, "compile:") ::: drivenBy(notes, "link:")

        withClue(notes) { lines.length should be >= 2 }
        all(lines) should include(s"-fprofile-generate=$dir")
      }

      // The absence that the whole feature depends on. An ordinary build that carried the
      // instrumentation would be a program paying for counters nobody asked for and writing a file
      // into whatever directory it was run in — and nothing about it would look wrong.
      "and a build that did not ask for it carries no profile runtime at all" in {
        assume(Toolchain.clangAvailable, "clang not available")

        val root = project("")

        cli(Config(command = "build", file = root, output = Some(s"$root/out"))) shouldBe 0

        val symbols = exec(Seq("nm", s"$root/out"))

        withClue(symbols.stderr) { symbols.exitCode shouldBe 0 }
        symbols.stdout should not include "__llvm_profile"
      }
    }

    // The round trip, which is the only thing that says the three steps fit together: an
    // instrumented program writes counters when it runs, `llvm-profdata` merges them into an indexed
    // profile, and a build against that profile is a build clang accepts.
    "a profile written by a training run is one a later build can be optimized against" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val merge = profdata.getOrElse(cancel("no llvm-profdata here, so no profile can be merged"))
      val dir   = createTempDirectory("sysl-pipeline-training-")
      val root  = project("")
      val exe   = s"$root/trained"

      // 1. Instrumented.
      cli(Config(command = "build", file = root, output = Some(exe),
                 profileGenerate = Some(dir))) shouldBe 0

      exec(Seq("nm", exe)).stdout should include("__llvm_profile")

      // 2. Trained — one run of the program, which is what writes the counters.
      exec(Seq(exe)).exitCode shouldBe 0

      val raw = listFiles(dir).filter(_.endsWith(".profraw"))

      withClue(s"nothing was written into $dir") { raw should not be empty }

      // 3. Merged, which is LLVM's own tool and not sysl's.
      val profile = s"$dir/merged.profdata"
      val merged  = exec(Seq(merge, "merge", "-output", profile) ++ raw)

      withClue(merged.stderr) { merged.exitCode shouldBe 0 }

      // 4. Built against it — and the optimizer is told where to read it.
      val notes = building(root, cfg = _.copy(profileUse = Some(profile)))

      notes should include(s"-fprofile-use=$profile")

      // And the result is an ordinary program again: the counters are gone, and it still runs.
      val out = exec(Seq(s"$root/out"))

      out.exitCode shouldBe 0
      out.stdout.trim shouldBe "1"
      exec(Seq("nm", s"$root/out")).stdout should not include "__llvm_profile"
    }
  }
}
