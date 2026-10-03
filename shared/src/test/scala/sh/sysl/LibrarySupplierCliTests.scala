package sh.sysl

import io.github.edadma.cross_platform.*

/** The standard library supplying a seam it declares, where the library is compiled from source into
 * the program (`reference/ffi.md § A module may supply another module's extern`).
 *
 * `sysl.time` declares `sysl_wall_us` and `sysl.posix.time` defines it, and nothing a program writes
 * imports the second. `Reachability.walk` keeps such a supplier as a root, but only one the analysis
 * put in the program — and the analyzer holds the library's functions back until something *calls*
 * one. So every compilation that takes the library as source (`build-c`, `--no-std-lib`, every
 * freestanding target) analyzed the declaration, never the definition, and a `build-c` archive went
 * out with `sysl_wall_us` undefined: found linking a skitter app, where lld named it from inside
 * `sysl.log`. A prebuilt standard module never showed it, because the library's own artifact was
 * compiled whole.
 *
 * `LibraryCliSupport` runs every case here with `--no-std-lib` unless it names a standard module,
 * which is exactly the path the defect was on.
 */
class LibrarySupplierCliTests extends LibraryCliSupport {

  /** An archive project: a directory holding one entry file, as `sysl build-c` is handed one. */
  private def project(main: String): String = {
    val root = createTempDirectory("sysl-cli-supply-")

    writeFile(s"$root/${PackageConfig.FileName}", "")
    writeFile(s"$root/main.sysl", main)
    root
  }

  private val asksTheTime =
    """import sysl.time.now
      |
      |@export("entry")
      |entry() -> long = now().us
      |""".stripMargin

  private val logs =
    """import sysl.log.info
      |
      |@export("entry")
      |entry() -> i32
      |    info("hi", [])
      |    0
      |""".stripMargin

  /** What `nm` says of one symbol in an archive: `T` where a member defines it, `U` where a member
   * only refers to it. Darwin prefixes C symbols with an underscore and ELF does not, so both
   * spellings are read.
   */
  private def kinds(archive: String, symbol: String): Set[String] = {
    val listed = exec(List("nm", archive))

    assume(listed.exitCode == 0, "nm not available")

    listed.stdout.linesIterator.map(_.trim.split("\\s+").toList).collect {
      case List(_, kind, name) if name == symbol || name == s"_$symbol" => kind
      case List(kind, name) if name == symbol || name == s"_$symbol"    => kind
    }.toSet
  }

  /** `build-c` of a program, and the archive it wrote. */
  private def archived(main: String): String = {
    val root = project(main)
    val out  = s"$root/libentry.a"

    succeeds(Config(command = "build-c", file = root, output = Some(out)))
    out
  }

  "a build-c archive carries the standard library's supplier of a seam the program calls" - {

    "for sysl.time.now" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val found = kinds(archived(asksTheTime), "sysl_wall_us")

      withClue(found)(found should contain("T"))
      found should not contain "U"
    }

    // The case it was found by: a logger stamps every record with the wall clock, so every `sysl.log`
    // call reaches the seam without the program ever naming `sysl.time`.
    "for a sysl.log call, which reads the clock without the program naming it" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val found = kinds(archived(logs), "sysl_wall_us")

      withClue(found)(found should contain("T"))
      found should not contain "U"
    }

    // A cross target, asked through `emit-llvm` because a `build-c` for Android needs an NDK and the
    // question is about what sysl emits rather than what the NDK's clang does with it.
    "for a cross target with a POSIX clock, aarch64-android" in {
      val ir = emitted(Config(command = "emit-llvm", file = project(asksTheTime),
        target = Some("aarch64-android")))

      symbols(ir, "define") should contain("sysl_wall_us")
      symbols(ir, "declare") should not contain "sysl_wall_us"
    }

    "and only the supplier the program asks for" in {
      val ir = emitted(Config(command = "emit-llvm", file = project(asksTheTime)))

      symbols(ir, "define") should contain("sysl_wall_us")
      symbols(ir, "define") should not contain "sysl_monotonic_us"
    }

    // The library's other seam, answered by the same rule: these two are its only suppliers.
    "for sysl.time.monotonic, the library's other seam" in {
      val ir = emitted(Config(command = "emit-llvm", file = project(
        "import sysl.time.monotonic\n\n@export(\"entry\")\nentry() -> long = monotonic().us\n")))

      symbols(ir, "define") should contain("sysl_monotonic_us")
      symbols(ir, "define") should not contain "sysl_wall_us"
    }

    "and none where the program never asks the time" in {
      val ir = emitted(Config(command = "emit-llvm",
        file = project("@export(\"entry\")\nentry() -> long = 7\n")))

      ir should not include "sysl_wall_us"
    }
  }

  "an ordinary build against a standard module compiled from source links the supplier too" in {
    assume(Toolchain.clangAvailable, "clang not available")

    ran(Config(command = "run", file = program("import sysl.time.now\n\nprint(now().us > 0)\n"))) should
      include("true")
  }

  // The prebuilt path, where the supplier is the artifact's and this compilation only declares it:
  // the `extern` the program calls must still be declared, since the definition is in another object.
  "a build against a prebuilt standard module still resolves the seam from the artifact" in {
    assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")
    assume(Toolchain.clangAvailable, "clang not available")

    ran(Config(command = "run", file = program("import sysl.time.now\n\nprint(now().us > 0)\n"),
      stdLib = Some(std))) should include("true")
  }

  "where something else supplies the seam" - {

    val chip =
      """module chip
        |
        |@export("sysl_wall_us")
        |wall_us() -> long = 7
        |""".stripMargin

    // The board's case on a POSIX target: a package binding its own clock is the one that answers,
    // and the library's is a fallback rather than a second claimant to be refused.
    "a package's supplier answers instead of the library's, with no conflict" in {
      val ir = emitted(Config(command = "emit-llvm", file = project(asksTheTime),
        libs = List(rootOf("chip", chip))))

      symbols(ir, "define") should contain("sysl_wall_us")
      ir should not include "sysl_posix_time_realtime_us"
    }

    // A freestanding target has no `posix`, so `sysl.posix.time` is not compiled at all and nothing in
    // the library can answer: the seam is left declared for the board to supply, and the link names
    // it — never a POSIX clock smuggled onto a machine that has none.
    "a freestanding target leaves it to the board" in {
      val ir = emitted(Config(command = "emit-llvm", file = project(asksTheTime),
        target = Some("thumb-freestanding-softfp")))

      symbols(ir, "declare") should contain("sysl_wall_us")
      symbols(ir, "define") should not contain "sysl_wall_us"
      ir should not include "sysl_posix_time_realtime_us"
    }

    "and a freestanding program handed a package's supplier is given that one" in {
      val ir = emitted(Config(command = "emit-llvm", file = project(asksTheTime),
        target = Some("thumb-freestanding-softfp"), libs = List(rootOf("chip", chip))))

      symbols(ir, "define") should contain("sysl_wall_us")
    }
  }
}
