package sh.sysl

import io.github.edadma.cross_platform.*

import java.io.{ByteArrayOutputStream, IOException, PrintStream}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A path the filesystem refuses is answered with `sysl: error: …`, never a stack trace.
 *
 * **Every case here escaped as an uncaught exception before `IoFailure`**: `build-c -o
 * p4/main.sysl/lib.a` died in `Project.makeDirectory` with `FileAlreadyExistsException:
 * p4/main.sysl`, a `--header` into an unwritable directory with `FileNotFoundException`, `sysl add`
 * on a read-only manifest the same way, and an unwritable cache with a bare `IOException` carrying no
 * message at all. Each is the user's or the environment's path, so each is an answer to give: the
 * status is 1 and stderr names the path and what was wrong with it.
 *
 * The permissions are set with `chmod` rather than a Java API, since that is the one spelling every
 * platform this suite runs on agrees about.
 */
class IoRefusalTests extends AnyFreeSpec with Matchers {

  /** The driver's status and what it said on stderr — with `noStdLib`, so no artifact cache is in
   * the way of the path under test.
   */
  private def cli(cfg: Config): (Int, String) = {
    val err    = new ByteArrayOutputStream
    val status =
      Console.withOut(Discarded)(Console.withErr(new PrintStream(err, true, "UTF-8"))(
        sh.sysl.execute(cfg.copy(noStdLib = true))))

    (status, err.toString("UTF-8"))
  }

  /** A scratch directory holding an exported C function, a directory nobody may write, and a plain
   * file — the three shapes every refusal below is built from. Permissions are put back before the
   * tree is removed, or the removal would be refused too.
   */
  private def scratch(check: String => Unit): Unit = {
    val dir = createTempDirectory("sysl-io-")

    try
      createDirectory(s"$dir/p4")
      writeFile(s"$dir/p4/main.sysl", "@export(\"f\")\nf() -> int = 1\n")
      createDirectory(s"$dir/locked")
      chmod("555", s"$dir/locked")
      check(dir)
    finally
      chmod("-R", "u+rwx", dir)
      Fetch.removeTree(dir)
  }

  private def chmod(args: String*): Unit = exec("chmod" +: args).exitCode shouldBe 0

  /** The shape of every refusal: status 1, the driver's own prefix, the path named, no trace. */
  private def refused(result: (Int, String), path: String, what: String): Unit = {
    val (status, err) = result

    withClue(err) {
      status shouldBe 1
      err should startWith("sysl: error: ")
      err should include(path)
      err should include(what)
      err should not include "Exception"
    }
  }

  "an output path under an existing FILE is refused, naming the file" - {

    "for build-c's archive — the case that was reported" in scratch { dir =>
      refused(cli(Config(command = "build-c", file = s"$dir/p4", output = Some(s"$dir/p4/main.sysl/lib.a"))),
        s"$dir/p4/main.sysl", "a file is already there")
    }

    "for build-c's header" in scratch { dir =>
      refused(
        cli(Config(command = "build-c", file = s"$dir/p4", output = Some(s"$dir/out.a"),
          header = Some(s"$dir/p4/main.sysl/f.h"))),
        s"$dir/p4/main.sysl", "a file is already there")
    }

    "for build-lib's artifact" in scratch { dir =>
      createDirectory(s"$dir/lib")
      createDirectory(s"$dir/lib/demo")
      writeFile(s"$dir/lib/demo/lib.sysl", "module demo\n\ndouble(n: int) -> int = n * 2\n")

      refused(cli(Config(command = "build-lib", file = s"$dir/lib", output = Some(s"$dir/p4/main.sysl/x.syslib"))),
        s"$dir/p4/main.sysl", "a file is already there")
    }

    "for weave and tangle" in scratch { dir =>
      createDirectory(s"$dir/lit")
      writeFile(s"$dir/lit/main.lsysl", "# Title\n\n```sysl\nprint(1)\n```\n")

      for verb <- List("weave", "tangle") do
        refused(cli(Config(command = verb, file = s"$dir/lit", output = Some(s"$dir/p4/main.sysl/x"))),
          s"$dir/p4/main.sysl", "a file is already there")
    }
  }

  "an output path under an UNWRITABLE directory is refused, naming the directory" - {

    "when a directory has to be made there" in scratch { dir =>
      refused(cli(Config(command = "build-c", file = s"$dir/p4", output = Some(s"$dir/locked/sub/lib.a"))),
        s"$dir/locked", "is not writable")
    }

    "when a file has to be written there" in scratch { dir =>
      refused(
        cli(Config(command = "build-c", file = s"$dir/p4", output = Some(s"$dir/out.a"),
          header = Some(s"$dir/locked/f.h"))),
        s"$dir/locked/f.h", "permission denied")
    }
  }

  "a header path that is a DIRECTORY is refused, naming it" in scratch { dir =>
    refused(
      cli(Config(command = "build-c", file = s"$dir/p4", output = Some(s"$dir/out.a"),
        header = Some(s"$dir/locked"))),
      s"$dir/locked", "is a directory")
  }

  "sysl add refuses a manifest nobody may write, and leaves it as it was" in scratch { dir =>
    createDirectory(s"$dir/proj")
    writeFile(s"$dir/proj/package.hocon", "dependencies {\n}\n")
    writeFile(s"$dir/proj/main.sysl", "print(1)\n")
    chmod("444", s"$dir/proj/package.hocon")

    refused(cli(Config(command = "add", file = s"$dir/proj", spec = "github.com/sysl-lang/table@0.1.3")),
      s"$dir/proj/package.hocon", "permission denied")

    readFile(s"$dir/proj/package.hocon") shouldBe "dependencies {\n}\n"
  }

  "a project holding a directory nobody may read is refused rather than thrown" in scratch { dir =>
    createDirectory(s"$dir/prog")
    writeFile(s"$dir/prog/main.sysl", "print(1)\n")
    createDirectory(s"$dir/prog/hidden")
    chmod("000", s"$dir/prog/hidden")

    val (status, err) = cli(Config(command = "build", file = s"$dir/prog", output = Some(s"$dir/a")))

    withClue(err) {
      status shouldBe 1
      err should startWith("sysl: error: ")
      err should include(s"$dir/prog/hidden")
      err should not include "Exception"
    }
  }

  "what IoFailure reads, and what it leaves alone" - {

    "an IO failure is recognised, wrapped or not" in {
      IoFailure.unapply(new IOException("x")).isDefined shouldBe true
      IoFailure.unapply(new java.io.UncheckedIOException(new IOException("x"))).isDefined shouldBe true
    }

    "anything else is not, so a compiler bug still escapes as one" in {
      IoFailure.unapply(new IllegalStateException("x")) shouldBe None
      IoFailure.unapply(new NullPointerException) shouldBe None
    }

    "a stream's '<path> (<reason>)' becomes the path and the reason" in {
      IoFailure.describe(new java.io.FileNotFoundException("a/b.h (Permission denied)")) shouldBe
        "'a/b.h': permission denied"
    }

    "an exception that says nothing still says what kind of refusal it was" in {
      IoFailure.describe(new IOException()) should include("IOException")
    }
  }
}
