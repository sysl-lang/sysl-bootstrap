package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `-v` / `--verbose` — what a build decided, on stderr.
 *
 * **The four things it reports were chosen rather than accumulated**: which standard module the
 * compilation got and by which route, the files it read, the command lines it handed to clang
 * together with the search paths behind them, and — for `build-lib` alone — the directory it staged
 * the artifact's members in. Phase timings were offered and declined, which is why there are none —
 * a build that is slow is diagnosed by asking what it *did*, and every one of the four above has
 * been the answer to a real question already.
 *
 * The fourth is the odd one, because the other three are decisions and it is a **write**. It earns
 * its place on exactly that: a staging directory is the one thing `build-lib` puts anywhere but the
 * artifact it was asked for, so a run interrupted before the cleanup leaves something behind that
 * nothing else in the run would name. `LibraryBuildCliTests` pins the cleanup, and this line is what
 * lets it ask about **one invocation's** directory rather than count the entries in a temp directory
 * every other build on the machine is writing into at the same time.
 *
 * It goes to stderr for the reason `wrote <exe>` does: stdout is whatever the build was for, and
 * `emit-llvm` prints a module there.
 */
class VerboseTests extends AnyFreeSpec with Matchers {

  private def parse(args: String*): Option[Config] = parseArgs(args)

  /** A run with stderr captured, which is where all of this goes. */
  private def said(cfg: Config): String = {
    val notes = new java.io.ByteArrayOutputStream

    Console.withOut(Discarded)(Console.withErr(notes)(sh.sysl.execute(cfg.copy(noStdLib = true))))

    notes.toString
  }

  private def project(check: String => Unit): Unit = {
    val dir = createTempDirectory("sysl-verbose-")

    try
      writeFile(s"$dir/main.sysl", "main()\n    print(1)\n")
      check(dir)
    finally
      for f <- listFiles(dir) do deleteFile(f)
      deleteFile(dir)
  }

  "the flag" - {

    "is spelled both ways, and is off unless asked for" in {
      parse("build", "p.sysl", "--verbose").map(_.verbose) shouldBe Some(true)
      parse("build", "p.sysl", "-v").map(_.verbose) shouldBe Some(true)
      parse("build", "p.sysl").map(_.verbose) shouldBe Some(false)
    }
  }

  "what it reports" - {

    "the files it read, and how many" in {
      project { dir =>
        val notes = said(Config(command = "build", file = dir, output = Some(s"$dir/out"), verbose = true))

        notes should include("1 source file(s)")
        notes should include(s"read $dir/main.sysl")
      }
    }

    // The quiet case, and the one worth having: an artifact that was read and one that was rejected
    // and rebuilt reach the end looking identical, and which happened is the first thing anybody
    // diagnosing a stale library wants to know.
    "which standard module it got, and by which route" in {
      project { dir =>
        val notes = said(Config(command = "build", file = dir, output = Some(s"$dir/out"), verbose = true))

        notes should include("standard module compiled from source")
      }
    }

    "the whole command line it handed to clang" in {
      project { dir =>
        val notes = said(Config(command = "build", file = dir, output = Some(s"$dir/out"), verbose = true))

        notes should include("link: clang")
        notes should include(s"-o $dir/out")
      }
    }

    "and the search paths, which are the other half of a link that failed" in {
      project { dir =>
        val notes = said(Config(command = "build", file = dir, output = Some(s"$dir/out"), verbose = true,
          linkPaths = List("/opt/somewhere/lib"), includePaths = List("/opt/somewhere/include")))

        notes should include("link path: /opt/somewhere/lib")
        notes should include("include path: /opt/somewhere/include")
      }
    }

    // Only `build-lib` stages anything, so this is the one line here an ordinary build does not
    // print — and both halves are worth pinning: a flag that reported a step which did not happen
    // would be worse than one that reported nothing.
    "and where build-lib staged the members, which no other command has to say" in {
      val root = createTempDirectory("sysl-verbose-lib-")
      val dir  = s"$root/demo"

      createDirectory(dir)
      writeFile(s"$dir/lib.sysl", "module demo\n\ndouble(n: int) -> int = n * 2\n")

      val out = s"$root/demo${LibraryArtifact.extension}"

      said(Config(command = "build-lib", file = root, output = Some(out), verbose = true)) should
        include("members staged in")

      project { plain =>
        said(Config(command = "build", file = plain, output = Some(s"$plain/out"), verbose = true)) should
          not include "members staged in"
      }
    }

    // **A suite links like anything else and was the one build that said nothing about it**, which
    // is the wrong way round: a question about what clang was handed most often starts from a suite
    // that failed to link, and the answer was available for every command but that one.
    "the command line a test suite was linked with, which is a build like any other" in {
      val dir = createTempDirectory("sysl-verbose-test-")

      writeFile(s"$dir/main.sysl",
        """@test("one is one")
          |one() =
          |    assert(1 == 1)
          |""".stripMargin)

      val notes = RunCache.disabledFor(said(Config(command = "test", file = dir, verbose = true)))

      notes should include("link: clang")
    }
  }

  "without it" - {

    // The default has to stay quiet: `wrote <exe>` is the whole of what a build says, and a tool
    // reading sysl's stderr should not have to filter.
    "a build says only what it wrote" in {
      project { dir =>
        val notes = said(Config(command = "build", file = dir, output = Some(s"$dir/out")))

        notes should not include "source file(s)"
        notes should not include "link: clang"
        notes should not include "standard module"
      }
    }
  }
}
