package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The optimization level, at the seams that matter: what an argument list parses to, what a program
 * built at a level actually does, and what a project that states its own level in its manifest is
 * built at.
 *
 * The level exists because a build used to ask for none at all, and `-O0` is a different instruction
 * selector rather than merely a slower one — the mode a back end's own suite covers least, and the
 * one a miscompile was found living in (`Toolchain.defaultOptimization` holds the case). So what is
 * asserted here is that the default is not `0` and that naming another reaches clang.
 */
class OptimizeCliTests extends AnyFreeSpec with Matchers {

  /** Through the driver's own entry, so what is asked here is what a shell asks. */
  private def parse(args: String*): Option[Config] = parseArgs(args)

  /** The driver under a name of its own — `Suite` has an `execute` too, and it wins unqualified.
   *
   * Stdout is thrown away unless a test captured it for itself (see `Discarded`): a `run` here
   * prints whatever the program printed, into the middle of some other suite's output.
   */
  private def cli(cfg: Config): Int = Console.withOut(Discarded)(driver(cfg))

  private def driver(cfg: Config): Int = sh.sysl.execute(cfg)

  /** The key, written as a manifest writes it. */
  private def asking(level: String): String = s"optimization = \"$level\""

  private def manifest(name: String, says: String): String =
    s"""package { name = "$name", version = "0.1.0" }
       |$says
       |""".stripMargin

  /** A project whose manifest says whatever the case is about, with a program and a test in it so
   * that one tree serves every command that builds.
   */
  private def project(says: String): String = {
    val root = createTempDirectory("sysl-optimize-project-")

    writeFile(s"$root/${PackageConfig.FileName}", manifest("prog", says))
    writeFile(s"$root/main.sysl",
      """main()
        |    print(1)
        |
        |@test("one is one")
        |one() =
        |    assert(1 == 1)
        |""".stripMargin)
    root
  }

  /** A package beside it, which is a project in every way except which one is being built. */
  private def dependency(says: String): String = {
    val root = createTempDirectory("sysl-optimize-package-")

    writeFile(s"$root/${PackageConfig.FileName}", manifest("geom", says))
    createDirectories(s"$root/geom")
    writeFile(s"$root/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")
    root
  }

  /** What the build handed clang, which `--verbose` traces on stderr.
   *
   * **With the run cache off**, because a replayed binary runs no clang at all — so a suite whose
   * evidence is a command line would otherwise be asserting about whichever run last filled the slot.
   */
  private def clang(cfg: Config): String = {
    val notes  = new java.io.ByteArrayOutputStream
    val status = RunCache.disabledFor(
      Console.withOut(Discarded)(Console.withErr(notes)(driver(cfg.copy(verbose = true)))))

    if status != 0 then fail(s"the driver exited with $status:\n${notes.toString}")

    notes.toString
  }

  /** The same, for a project — `build` alone is given somewhere to write, since it is the only one of
   * the three that writes an executable the caller named.
   */
  private def building(root: String, command: String = "build", level: Option[String] = None): String =
    clang(Config(command = command, file = root, optimize = level,
                 output = Option.when(command == "build")(s"$root/out")))

  private val program =
    """import sysl.math.Bits
      |
      |spin[T: Bits](x: T, n: u32) -> T = x.rotate_left(n)
      |
      |main()
      |    var a: u8 = 0b10110000
      |    print(spin(a, 1), a.count_ones(), (0x0123456789abcdefu64).rotate_right(8))
      |""".stripMargin

  "what an argument list says" - {

    // **Naming none and naming the default are two different answers**, which is what lets a
    // manifest's key apply to the first and not the second: a build that asked for `-O1` asked for
    // it, and nothing may overrule a level somebody typed.
    "a build that names no level names none, and is built at the default" in {
      parse("build", "prog.sysl").map(_.optimize) shouldBe Some(None)
      parse("build", "prog.sysl").map(_.optimization) shouldBe Some(Toolchain.defaultOptimization)
      Toolchain.defaultOptimization should not be "0"
    }

    "and one that names a level carries it, whichever of clang's spellings it is" in {
      for level <- List("0", "2", "3", "s", "z", "fast") do
        withClue(level) {
          parse("build", "prog.sysl", "--optimize", level).map(_.optimize) shouldBe Some(Some(level))
          parse("build", "prog.sysl", "--optimize", level).map(_.optimization) shouldBe Some(level)
        }
    }

    // The short form is clang's own, so a level can be written the way the person reaching for it
    // already writes it. **The joined spelling is the one that needed work**: a short option takes
    // its value as the next argument, so `-O2` does not parse on its own and is split before the
    // parser sees it. Without that, sysl would offer a flag that looked like clang's and was not.
    "the short form is spelled the way clang spells it, joined or apart" in {
      for level <- List("0", "2", "3", "s", "z", "fast") do
        withClue(level) {
          parse("build", "prog.sysl", s"-O$level").map(_.optimize) shouldBe Some(Some(level))
          parse("build", "prog.sysl", "-O", level).map(_.optimize) shouldBe Some(Some(level))
        }
    }

    // The split is one letter and only where something follows it, so nothing else that starts with
    // a dash is touched — and `--optimize` does not begin `-O`, so the long form goes by untouched.
    "and splitting the joined level leaves every other argument alone" in {
      val cfg = parse("build", "prog.sysl", "-O2", "--target", "aarch64-linux", "--lib", "a.syslib")

      cfg.map(_.target) shouldBe Some(Some("aarch64-linux"))
      cfg.map(_.libs) shouldBe Some(List("a.syslib"))
      cfg.map(_.file) shouldBe Some("prog.sysl")
    }

    // Case is the whole difference between this and the output path, so it is asserted rather than
    // assumed: `-o` names a file and `-O` names a level, and neither quietly answers for the other.
    "and lower-case '-o' is still the output path, which is the collision worth pinning" in {
      val cfg = parse("build", "prog.sysl", "-o", "prog", "-O2")

      cfg.map(_.output) shouldBe Some(Some("prog"))
      cfg.map(_.optimize) shouldBe Some(Some("2"))
    }

    // The flag belongs to every command that produces code, not to `build` alone — `run` links an
    // executable and `test` links one too, and a level that reached only one of them would be a
    // level that silently stopped applying when you changed command.
    "every command that emits code accepts it" in {
      for command <- List("run", "build", "build-lib", "test") do
        withClue(command) {
          parse(command, "prog.sysl", "--optimize", "2").map(_.optimize) shouldBe Some(Some("2"))
        }
    }

    // What is after `--` is the program's, so a program with an option of the same name still gets
    // its own — the split happens before this parser ever sees the arguments.
    "and the driver's own split keeps the program's arguments out of it" in {
      val (own, forwarded) = List("run", "prog.sysl", "-O2", "--", "-O9", "--optimize", "9").span(_ != "--")

      parseArgs(own).map(_.optimize) shouldBe Some(Some("2"))
      forwarded.drop(1) shouldBe List("-O9", "--optimize", "9")
    }
  }

  "what a level does to a program" - {

    // The answer is the program's and not the optimizer's, so every level has to agree on it. This
    // is also the only assertion here that would have caught the miscompile the default exists for.
    "every level it can be built at answers the same thing" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val path = createTempFile("sysl-optimize-", ".sysl")
      writeFile(path, program)

      try
        val answers =
          for level <- List("0", "1", "2", "s")
          yield
            val out = new java.io.ByteArrayOutputStream
            val status = Console.withOut(out)(
              driver(Config(command = "run", file = path, noStdLib = true, optimize = Some(level))))

            withClue(s"-O$level") { status.shouldBe(0) }
            out.toString

        answers.distinct shouldBe List("97 3 17222085231038278605\n")
      finally deleteFile(path)
    }

    // A level clang has no answer for is clang's to report, and the driver's job is to fail rather
    // than to carry on having quietly dropped it.
    "a level clang does not have stops the build" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val path = createTempFile("sysl-optimize-", ".sysl")
      writeFile(path, "main()\n    print(1)\n")

      try
        val errs = new java.io.ByteArrayOutputStream
        val status = Console.withErr(errs)(
          cli(Config(command = "run", file = path, noStdLib = true, optimize = Some("nonsense"))))

        status.should(not).be(0)
        errs.toString should include("clang")
      finally deleteFile(path)
    }
  }

  /** The same level, stated by the project instead of typed at every build
   * (`PackageConfig.optimization`, `reference/packages.md § The optimization level a project is
   * built at`).
   *
   * **What is asserted is the clang command line**, because that is the only place the three sources
   * of a level — the flag, the key, the default — become one answer, and a suite that asked a
   * `Config` instead would be asking the question one layer above the one that decides.
   */
  "what a project's manifest says" - {

    "the level it names is what the build hands clang" in {
      assume(Toolchain.clangAvailable, "clang not available")

      building(project(asking("2"))) should include(" -O2 ")
    }

    "a project that names none is built at the default" in {
      assume(Toolchain.clangAvailable, "clang not available")

      building(project("")) should include(s" -O${Toolchain.defaultOptimization} ")
    }

    // The precedence, in the direction that needs the flag to be an `Option`: a project built at `2`
    // is still profiled at `0` by typing it, and typing the default is typing something.
    "and a level on the command line beats it" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val notes = building(project(asking("2")), level = Some("0"))

      notes should include(" -O0 ")
      notes should not include " -O2 "
    }

    // A level reaches every object a build produces, so a package that could set it would be
    // deciding how its consumer's whole program is compiled — including the half it has nothing to
    // do with. It is the root's key or nobody's, exactly as `targets.default` is, and a package that
    // states one has said nothing wrong: it is simply not the project being built.
    "a dependency's key is not applied, and is not an error either" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val geom = dependency(asking("3"))
      val root = createTempDirectory("sysl-optimize-app-")

      writeFile(s"$root/${PackageConfig.FileName}",
        s"""package { name = "app", version = "0.1.0" }
           |dependencies { g { path = "$geom" } }
           |""".stripMargin)
      writeFile(s"$root/main.sysl", "main()\n    print(geom.double(21))\n")

      val notes = clang(Config(command = "build", file = root, output = Some(s"$root/out")))

      notes should include(s" -O${Toolchain.defaultOptimization} ")
      notes should not include " -O3 "
    }

    // The flag reaches every command that emits code, and so does the key — a level that applied to
    // `build` and not to `test` would be a project measured at one level and shipped at another.
    "every command that builds the project honours it" in {
      assume(Toolchain.clangAvailable, "clang not available")

      for command <- List("build", "run", "test") do
        withClue(command) { building(project(asking("2")), command) should include(" -O2 ") }
    }

    // `sysl run` keeps what it built, keyed on the level among everything else that reaches the
    // bytes (`RunCache`) — so raising the key has to rebuild rather than replay a binary compiled at
    // the old level from a tree that did not move.
    "and raising it is a different program to the run cache" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val cache = createTempDirectory("sysl-optimize-cache-")
      val root  = project(asking("1"))

      def entries: Int =
        if isDirectory(s"$cache/sysl/run") then listFiles(s"$cache/sysl/run").length else 0

      RunCache.usingCache(cache) {
        cli(Config(command = "run", file = root)) shouldBe 0
        entries shouldBe 1

        writeFile(s"$root/${PackageConfig.FileName}", manifest("prog", asking("2")))

        cli(Config(command = "run", file = root)) shouldBe 0
        entries shouldBe 2
      }
    }

    // Refused where the key is, naming the key, what was written and what may be — rather than by
    // clang, which would answer for a manifest it cannot name from a build the reader did not type.
    "a level clang does not have stops the build before clang is reached" in {
      val root  = project(asking("9"))
      val errs  = new java.io.ByteArrayOutputStream
      val state = Console.withErr(errs)(
        cli(Config(command = "build", file = root, output = Some(s"$root/out"))))

      state.should(not).be(0)
      errs.toString should include("'optimization = \"9\"'")
      errs.toString should include("0, 1, 2, 3, s, z")

      // The refusal is the manifest reader's — it names the file and the key — and nothing was
      // compiled, which is the other half of "before clang is reached": clang writing the executable
      // and then failing would have left one.
      errs.toString should include(PackageConfig.FileName)
      isFile(s"$root/out") shouldBe false
    }
  }
}
