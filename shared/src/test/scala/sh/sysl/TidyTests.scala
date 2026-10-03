package sh.sysl

import io.github.edadma.cross_platform.*

/** `sysl tidy` — `sysl.sum` cut down to what the project resolves to now
  * (`reference/packages.md § sysl.sum`).
  *
  * A build only ever adds a line, so a version the project has moved off stays recorded until this
  * runs. The claims pinned here are the ones that make it safe to run in CI: a line that stays is
  * the line that was there, byte for byte; a line a feature or a `dev_dependencies` entry needs is
  * not mistaken for a stale one; running it twice is running it once; and `--check` writes nothing.
  *
  * The cache is built on disk and handed to the driver through `Fetch.usingCache`, which redirects
  * per thread, so every case drives the whole command with no network.
  */
class TidyTests extends PackageCacheSupport {

  private def dep(label: String, coordinate: String, version: String, extra: String = ""): String =
    s"""$label { git = "$coordinate", version = "$version"${if extra.isEmpty then "" else s", $extra"} }"""

  /** A package in the cache with one module and its hash recorded beside it, as a fetch leaves it. */
  private def put(cache: String, coordinate: String, version: Version, deps: String = ""): String = {
    val leaf = coordinate.split('/').last

    publishedModule(cache, coordinate, version, leaf, deps = deps)
    record(cache, coordinate, version)
  }

  /** A stand-in digest for a line whose package is never read, which nothing ever checks. `c` has to
    * be a hex digit, or the file is refused as unreadable before anything is tidied.
    */
  private def stale(c: Char): String = s"${Hashing.Prefix}${c.toString * 64}"

  private def sums(root: String): String = readFile(s"$root/${Sums.FileName}")

  private def hasSums(root: String): Boolean = isFile(s"$root/${Sums.FileName}")

  /** The driver, as `sysl tidy [--check] <root>` reaches it: the status, stdout and stderr. */
  private def tidy(root: String, cache: String, check: Boolean = false): (Int, String, String) = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Fetch.usingCache(cache)(Console.withOut(out)(Console.withErr(notes)(
      sh.sysl.execute(Config(command = "tidy", file = root, tidyCheck = check)))))

    (status, out.toString, notes.toString)
  }

  "what an argument list says" - {

    "tidy takes the working directory where no path is given" in {
      parseArgs(Seq("tidy")).map(c => (c.command, c.file, c.tidyCheck)) shouldBe Some(("tidy", ".", false))
    }

    "and a path and --check where they are" in {
      parseArgs(Seq("tidy", "--check", "/somewhere")).map(c => (c.command, c.file, c.tidyCheck)) shouldBe
        Some(("tidy", "/somewhere", true))
    }
  }

  "what it removes and what it keeps" - {

    // The case the command exists for: a project that moved from 0.9.0 to 1.0.0, and a package it
    // stopped depending on at all, both still recorded. The live lines are deliberately out of the
    // order a build would write them in, so that keeping them "byte for byte" is a claim that could
    // fail.
    "a version nothing selects goes, and the lines that stay are the lines that were there" in {
      val cache = emptyCache()
      val geom  = put(cache, "github.com/e/geom", Version(1, 0, 0))
      val buf   = put(cache, "github.com/e/buf", Version(1, 2, 0))
      val root  = project(manifest("app", "0.1.0",
        dep("geom", "github.com/e/geom", "1.0.0") + "\n" + dep("buf", "github.com/e/buf", "1.2.0")))

      writeFile(s"$root/${Sums.FileName}",
        s"github.com/e/geom v1.0.0 $geom\n" +
        s"github.com/e/geom v0.9.0 ${stale('a')}\n" +
        s"github.com/e/buf v1.2.0 $buf\n" +
        s"github.com/e/gone v3.1.0 ${stale('b')}\n")

      tidy(root, cache) shouldBe (0,
        "removed github.com/e/geom v0.9.0\nremoved github.com/e/gone v3.1.0\n", "")

      sums(root) shouldBe s"github.com/e/geom v1.0.0 $geom\ngithub.com/e/buf v1.2.0 $buf\n"
    }

    // A build of the default features would not reach `tiny`, which is exactly why a build cannot be
    // the thing that prunes: the line has to survive a tidy run with no feature named.
    "a dependency only a feature turns on keeps its line" in {
      val cache = emptyCache()
      val tiny  = put(cache, "github.com/e/tiny", Version(1, 0, 0))
      val root  = project(
        s"""package { name = "app", version = "0.1.0" }
           |dependencies { ${dep("tiny", "github.com/e/tiny", "1.0.0", "optional = true")} }
           |features { small = [tiny] }
           |""".stripMargin)

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/tiny v1.0.0 $tiny\n")

      tidy(root, cache) shouldBe (0, "", "")
      sums(root) shouldBe s"github.com/e/tiny v1.0.0 $tiny\n"
    }

    "and so does one only the project's own tests use" in {
      val cache = emptyCache()
      val check = put(cache, "github.com/e/check", Version(1, 0, 0))
      val root  = project(
        s"""package { name = "app", version = "0.1.0" }
           |dev_dependencies { ${dep("check", "github.com/e/check", "1.0.0")} }
           |""".stripMargin)

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/check v1.0.0 $check\n")

      tidy(root, cache) shouldBe (0, "", "")
      sums(root) shouldBe s"github.com/e/check v1.0.0 $check\n"
    }

    // `b` 1.0.0 is read before `z` raises it to 1.1.0, so every resolution of these manifests reads
    // it — a build after a tidy that dropped it would put it straight back.
    "a version read on the way to a higher one keeps its line, since every build reads it again" in {
      val cache = emptyCache()
      val b10   = put(cache, "github.com/e/b", Version(1, 0, 0))
      val b11   = put(cache, "github.com/e/b", Version(1, 1, 0))
      val z     = put(cache, "github.com/e/z", Version(1, 0, 0), deps = dep("b", "github.com/e/b", "1.1.0"))
      val root  = project(manifest("app", "0.1.0",
        dep("b", "github.com/e/b", "1.0.0") + "\n" + dep("z", "github.com/e/z", "1.0.0")))
      val file  = s"github.com/e/b v1.0.0 $b10\ngithub.com/e/b v1.1.0 $b11\ngithub.com/e/z v1.0.0 $z\n"

      writeFile(s"$root/${Sums.FileName}", file)

      tidy(root, cache) shouldBe (0, "", "")
      sums(root) shouldBe file
    }

    "a package with no line yet is recorded as a build would record it" in {
      val cache = emptyCache()
      val geom  = put(cache, "github.com/e/geom", Version(1, 0, 0))
      val root  = project(manifest("app", "0.1.0", dep("geom", "github.com/e/geom", "1.0.0")))

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/old v1.0.0 ${stale('c')}\n")

      tidy(root, cache) shouldBe (0,
        "removed github.com/e/old v1.0.0\nadded github.com/e/geom v1.0.0\n", "")
      sums(root) shouldBe s"github.com/e/geom v1.0.0 $geom\n"
    }
  }

  "a path dependency" - {

    // A directory beside the project is expected to change, so it never has a line — and a project
    // whose only dependencies are paths ends with no file, which is what a build leaves for one.
    "gets no line, and a file left holding nothing is removed" in {
      val lib  = project(manifest("geom", "0.1.0"), "geom")
      val root = project(manifest("app", "0.1.0", s"""g { path = "$lib" }"""))

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/geom v1.0.0 ${stale('d')}\n")

      tidy(root, emptyCache()) shouldBe (0, "removed github.com/e/geom v1.0.0\n", "")
      hasSums(root) shouldBe false
    }

    "and beside a git dependency only the git one is recorded" in {
      val cache = emptyCache()
      val buf   = put(cache, "github.com/e/buf", Version(1, 0, 0))
      val lib   = project(manifest("geom", "0.1.0"), "geom")
      val root  = project(manifest("app", "0.1.0",
        s"""g { path = "$lib" }""" + "\n" + dep("buf", "github.com/e/buf", "1.0.0")))

      tidy(root, cache) shouldBe (0, "added github.com/e/buf v1.0.0\n", "")
      sums(root) shouldBe s"github.com/e/buf v1.0.0 $buf\n"
    }
  }

  "a project depending on nothing" - {

    "is tidy with no file, and says nothing" in {
      val root = project(manifest("app", "0.1.0"))

      tidy(root, emptyCache()) shouldBe (0, "", "")
      hasSums(root) shouldBe false
    }

    "and a file it still carries is removed" in {
      val root = project(manifest("app", "0.1.0"))

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/geom v1.0.0 ${stale('e')}\n")

      tidy(root, emptyCache()) shouldBe (0, "removed github.com/e/geom v1.0.0\n", "")
      hasSums(root) shouldBe false
    }
  }

  "a tidy file" - {

    "is left alone, and nothing is printed" in {
      val cache = emptyCache()
      val geom  = put(cache, "github.com/e/geom", Version(1, 0, 0))
      val root  = project(manifest("app", "0.1.0", dep("geom", "github.com/e/geom", "1.0.0")))

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/geom v1.0.0 $geom\n")

      val before = lastModified(s"$root/${Sums.FileName}")

      tidy(root, cache) shouldBe (0, "", "")
      lastModified(s"$root/${Sums.FileName}") shouldBe before
    }

    "and a second run after a first does nothing at all" in {
      val cache = emptyCache()
      val geom  = put(cache, "github.com/e/geom", Version(1, 0, 0))
      val root  = project(manifest("app", "0.1.0", dep("geom", "github.com/e/geom", "1.0.0")))

      writeFile(s"$root/${Sums.FileName}",
        s"github.com/e/geom v0.1.0 ${stale('f')}\ngithub.com/e/geom v1.0.0 $geom\n")

      tidy(root, cache)._2 shouldBe "removed github.com/e/geom v0.1.0\n"

      val once = sums(root)

      tidy(root, cache) shouldBe (0, "", "")
      sums(root) shouldBe once
    }
  }

  "--check" - {

    "passes a tidy file, quietly" in {
      val cache = emptyCache()
      val geom  = put(cache, "github.com/e/geom", Version(1, 0, 0))
      val root  = project(manifest("app", "0.1.0", dep("geom", "github.com/e/geom", "1.0.0")))

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/geom v1.0.0 $geom\n")

      tidy(root, cache, check = true) shouldBe (0, "", "")
    }

    "fails an untidy one, says what would go, and writes nothing" in {
      val cache = emptyCache()
      val geom  = put(cache, "github.com/e/geom", Version(1, 0, 0))
      val root  = project(manifest("app", "0.1.0", dep("geom", "github.com/e/geom", "1.0.0")))
      val file  = s"github.com/e/geom v0.1.0 ${stale('0')}\ngithub.com/e/geom v1.0.0 $geom\n"

      writeFile(s"$root/${Sums.FileName}", file)

      val (status, out, err) = tidy(root, cache, check = true)

      status should not be 0
      out shouldBe "would remove github.com/e/geom v0.1.0\n"
      err should include("not tidy")
      sums(root) shouldBe file
    }

    "and fails a file missing a line the project needs" in {
      val cache = emptyCache()

      put(cache, "github.com/e/geom", Version(1, 0, 0))

      val root = project(manifest("app", "0.1.0", dep("geom", "github.com/e/geom", "1.0.0")))
      val (status, out, _) = tidy(root, cache, check = true)

      status should not be 0
      out shouldBe "would add github.com/e/geom v1.0.0\n"
      hasSums(root) shouldBe false
    }
  }

  "outside a project" - {

    "is refused, and names the manifest it looked for" in {
      val dir = createTempDirectory("sysl-no-project-")

      writeFile(s"$dir/main.sysl", "print(1)\n")

      val (status, out, err) = tidy(dir, emptyCache())

      status should not be 0
      out shouldBe ""
      err should include(PackageConfig.FileName)
      hasSums(dir) shouldBe false
    }
  }
}
