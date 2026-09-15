package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `--features`, `--no-default-features` and `--all-features`, at the seam a shell types them
 * (`PackageResolution.featureRequest` already reads `Config`'s three fields — this is the argument
 * list reaching them at all).
 *
 * They belong to exactly the commands that resolve a package graph — `run`, `build`, `build-c`,
 * `test`, `deps` and `vendor`, found by grepping for where `featureRequest` and `resolvedGraph` are
 * called (`PackageResolution.scala`). `build-lib` fetches nothing and reads `PackageSources.none`
 * directly, so it never reaches `featureRequest` and does not take these; neither does a command
 * that compiles no project at all, such as `weave`.
 */
class FeatureCliTests extends AnyFreeSpec with Matchers {

  private def parsed(args: String*): Option[Config] = parseArgs(args)

  /** The driver's refusal, off stderr — the check fires before `readPackageConfig`, so `file` need
   * not name anything real.
   */
  private def stderrOf(cfg: Config): (Int, String) = {
    val out  = new java.io.ByteArrayOutputStream
    val code = Console.withErr(out)(sh.sysl.execute(cfg))

    (code, out.toString)
  }

  "--features" - {

    "parses into Config.features, on a command that resolves a graph" in {
      parsed("build", "somewhere", "--features", "server").map(_.features) shouldBe Some(List("server"))
    }

    "and a comma-separated value splits into several" in {
      parsed("build", "somewhere", "--features", "server,tls").map(_.features) shouldBe
        Some(List("server", "tls"))
    }

    "and the flag given more than once accumulates rather than overwriting" in {
      parsed("build", "somewhere", "--features", "server", "--features", "tls").map(_.features) shouldBe
        Some(List("server", "tls"))
    }

    "and the two forms combine: repeated, each itself comma-separated" in {
      parsed("build", "somewhere", "--features", "a,b", "--features", "c,d").map(_.features) shouldBe
        Some(List("a", "b", "c", "d"))
    }

    "parses on every command that resolves a graph" in {
      for command <- List("run", "build", "build-c", "test", "deps", "vendor") do
        withClue(s"'$command --features x' did not parse: ") {
          parsed(command, "somewhere", "--features", "x").map(_.features) shouldBe Some(List("x"))
        }
    }

    "and is refused on a command that resolves no graph, exactly as an unknown option is" in {
      parsed("build-lib", "somewhere", "--features", "x") shouldBe None
      parsed("weave", "somewhere", "--features", "x") shouldBe None
    }
  }

  "--no-default-features" - {

    "parses into Config.noDefaultFeatures" in {
      parsed("build", "somewhere", "--no-default-features").map(_.noDefaultFeatures) shouldBe Some(true)
    }

    "and is refused on a command that resolves no graph" in {
      parsed("build-lib", "somewhere", "--no-default-features") shouldBe None
    }
  }

  "--all-features" - {

    "parses into Config.allFeatures" in {
      parsed("build", "somewhere", "--all-features").map(_.allFeatures) shouldBe Some(true)
    }

    "and is refused on a command that resolves no graph" in {
      parsed("build-lib", "somewhere", "--all-features") shouldBe None
    }
  }

  "--all-features together with --features or --no-default-features" - {

    // The check runs before the manifest is read, so a path naming nothing real still reaches it —
    // what is being asserted is the refusal, not anything about the project at that path.
    "is refused, since the two say different things about the same set" in {
      val (status, err) = stderrOf(Config(command = "build", file = "/does/not/exist",
        allFeatures = true, features = List("server")))

      status should not be 0
      err should include("--all-features")
      err should include("--features")
    }

    "and the same refusal reaches --no-default-features" in {
      val (status, err) = stderrOf(Config(command = "build", file = "/does/not/exist",
        allFeatures = true, noDefaultFeatures = true))

      status should not be 0
      err should include("--all-features")
      err should include("--no-default-features")
    }

    "but --all-features alone is not a conflict with itself" in {
      parsed("build", "somewhere", "--all-features").map(_.allFeatures) shouldBe Some(true)
    }
  }

  "the usage text" - {

    "names all three flags" in {
      val out = new java.io.ByteArrayOutputStream
      Console.withOut(out)(sh.sysl.execute(Config(command = "help")))

      out.toString should include("--features")
      out.toString should include("--no-default-features")
      out.toString should include("--all-features")
    }
  }
}
