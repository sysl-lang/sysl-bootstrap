package sh.sysl

import io.github.edadma.cross_platform.*

/** The two capability rules of `reference/modules.md § Capabilities are a module property`, asked of
 * a module that arrives through a **`dependencies` block** rather than a `--lib` root.
 *
 * `TargetCapabilityTests` holds both rules against a library handed over as source, and a fetched
 * package is handed over the same way — but a package's modules are filed under its canonical
 * prefix, and a clause recorded under the name its header wrote was one no reference ever reached.
 * So a `requires posix` in a dependency refused nothing, on a machine without POSIX or from a module
 * that gave POSIX up. These run the driver over a real project, for a `path` dependency and a `git`
 * one, since the two are filed under different prefixes.
 */
class PackageCapabilityTests extends PackageCacheSupport {

  private val probe = "module demo.probe\n@requires(posix)\n\nsize() -> usize = 32\n"

  /** A package declaring the one module `demo.probe`, which needs POSIX. */
  private def pathPackage(): String = {
    val root = createTempDirectory("sysl-pkg-demo-")

    writeFile(s"$root/${PackageConfig.FileName}", manifest("demo", "0.1.0"))
    createDirectories(s"$root/demo/probe")
    writeFile(s"$root/demo/probe/probe.sysl", probe)
    root
  }

  /** The same package, published under a coordinate in `cache`. */
  private def gitPackage(cache: String): Unit =
    published(cache, "github.com/e/demo", Version(0, 1, 0), manifest("demo", "0.1.0"),
      "demo/probe/probe.sysl" -> probe)

  /** A program depending on `deps`, whose config says `config` besides. */
  private def app(deps: String, config: String, program: String): String = {
    val root = createTempDirectory("sysl-app-")

    writeFile(s"$root/${PackageConfig.FileName}",
      s"""package { name = "app", version = "0.1.0" }
         |$config
         |dependencies { $deps }
         |""".stripMargin)
    writeFile(s"$root/main.sysl", program)
    root
  }

  private def run(root: String): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val result = Console.withOut(out)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root))))

    if result != 0 then fail(s"the driver exited with $result:\n${out.toString}${notes.toString}")

    out.toString
  }

  /** A refusal asked through `build`, which consults no cache: `sysl run` replays a binary keyed on
   * the source, so a refusal through it can be answered by a build some earlier compiler made.
   */
  private def refused(root: String, command: String = "build"): String = {
    val notes  = new java.io.ByteArrayOutputStream
    val result = Console.withOut(Discarded)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = command, file = root))))

    if result == 0 then fail("expected a refusal, got a build")

    notes.toString
  }

  private val noPosix  = "capabilities { posix = false }"
  private val reaching = "print(demo.probe.size())\n"
  private val givenUp  = "@no_posix\n\nprint(demo.probe.size())\n"

  private def notProvided(module: String): String =
    s"this reaches '$module', which requires 'posix', and '${Target.default.name}' does not provide " +
      s"it — a target's capabilities are what '${PackageConfig.FileName}' declares"

  private def declaredNo(module: String): String =
    s"this reaches '$module', which requires 'posix', and this module declared 'no posix' — an " +
      "environment capability gates which modules exist"

  "a path dependency's gated module" - {
    def dep = s"""demo { path = "${pathPackage()}" }"""

    "is refused where a target without the capability reaches it" in {
      refused(app(dep, noPosix, reaching)) should include(notProvided("demo.demo.probe"))
    }

    "is refused where a module that gave the capability up reaches it" in {
      val e = refused(app(dep, "", givenUp))

      e should include(declaredNo("demo.demo.probe"))
      e shouldNot include("does not provide it")
    }

    "and is reached as before on a target that provides it" in {
      run(app(dep, "", reaching)) shouldBe "32\n"
    }

    // `sysl run`'s cache is keyed on the source, and the config is not source: without what the
    // target provides in the key, the second run here was handed the first one's binary.
    "and a run the config has since narrowed is not answered by the run before it" in {
      val program = s"print(demo.probe.size() + ${System.nanoTime % 1000000})\n"
      val pkg     = pathPackage()

      run(app(s"""demo { path = "$pkg" }""", "", program)) should not be empty
      refused(app(s"""demo { path = "$pkg" }""", noPosix, program), "run") should
        include(notProvided("demo.demo.probe"))
    }
  }

  "a git dependency's gated module" - {
    val dep = """demo { git = "github.com/e/demo", version = "0.1.0" }"""

    "is refused where a target without the capability reaches it" in {
      val cache = emptyCache()

      gitPackage(cache)
      Fetch.usingCache(cache)(refused(app(dep, noPosix, reaching))) should
        include(notProvided("github.com.e.demo.demo.probe"))
    }

    "is refused where a module that gave the capability up reaches it" in {
      val cache = emptyCache()

      gitPackage(cache)
      Fetch.usingCache(cache)(refused(app(dep, "", givenUp))) should
        include(declaredNo("github.com.e.demo.demo.probe"))
    }

    "and is reached as before on a target that provides it" in {
      val cache = emptyCache()

      gitPackage(cache)
      Fetch.usingCache(cache)(run(app(dep, "", reaching))) shouldBe "32\n"
    }
  }

  // `reference/modules.md § A @tests file states its own capabilities`: a package whose tests make a
  // scratch directory is still one a board program links. `sysl.fs` declares no `make_temp_dir` on a
  // machine with no operating system, so the package's test file has to go unread by the consumer's
  // build, not merely uncharged — the same rule `NeedsScopeCliTests` holds for a `--lib` root, here
  // through a fetched coordinate, whose modules are filed under its canonical prefix.
  "a git dependency whose '@tests' file the board's machine could not compile" - {
    val dep = """notes { git = "github.com/e/notes", version = "0.1.0" }"""

    val board =
      """targets {
        |  default = "thumbv6m-freestanding"
        |  thumbv6m-freestanding { capabilities { os = false, posix = false } }
        |}""".stripMargin

    "is linked by a board program all the same" in {
      val cache = emptyCache()

      published(cache, "github.com/e/notes", Version(0, 1, 0), manifest("notes", "0.1.0"),
        "notes/notes.sysl"       -> "module notes\n\nrender() -> int = 5\n",
        "notes/notes_tests.sysl" -> ("module notes\n@tests\n\nimport sysl.fs.make_temp_dir\n\n@test\n" +
          "makes_a_scratch_dir() =\n    assert(make_temp_dir(\"t\").is_ok())\n"))

      val root  = app(dep, board, "import notes.render\n\n@export(\"main\")\nboot() -> int = render()\n")
      val notes = new java.io.ByteArrayOutputStream
      val state = Fetch.usingCache(cache)(Console.withOut(Discarded)(
        Console.withErr(notes)(sh.sysl.execute(Config(command = "emit-llvm", file = root)))))

      withClue(notes.toString)(state shouldBe 0)
    }
  }
}
