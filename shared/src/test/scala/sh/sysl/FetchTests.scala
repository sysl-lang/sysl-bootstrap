package sh.sysl

import io.github.edadma.cross_platform.*

/** Getting a package's source onto this machine, and what it is checked against on the way
 * (`reference/packages.md § Dependencies`, `§ 6`).
 *
 * The clone itself is one `git` invocation and is not exercised here; everything either side of it
 * is. What that covers is the part with decisions in it — whether a populated cache may be trusted,
 * what a `sysl.sum` disagreement says, and what happens to a directory nothing vouches for.
 */
class FetchTests extends PackageCacheSupport {

  private val coordinate = "github.com/e/json"
  private val version    = Version(1, 4, 0)

  private def dep(label: String = "json") =
    Dependency(label, Origin.Git(coordinate, version))

  private def ensure(cache: String, sums: Sums = Sums.empty, d: Dependency = dep()) =
    Fetch.ensure(d, sums, cache)

  private def refused(cache: String, sums: Sums, d: Dependency = dep()): String =
    ensure(cache, sums, d) match
      case Left(e)  => e
      case Right(f) => fail(s"expected a refusal, got: $f")

  "where a package lands" in {
    Fetch.directory("/cache", "github.com/e/json/v2", Version(2, 1, 0)) shouldBe
      "/cache/github.com/e/json/v2/@v2.1.0"
  }

  "a package already in the cache" - {

    "is used where nothing records what it should hash to" in {
      val cache = emptyCache()
      val dir   = published(cache, coordinate, version, manifest("json", "1.4.0"))

      ensure(cache) shouldBe Right(Fetch.Fetched(dep(), dir, None))
    }

    // Nothing is walked again: what is asked is whether this project's record agrees with what was
    // verified when the directory was written, and that is two strings.
    "is checked against the hash recorded beside it" in {
      val cache = emptyCache()
      val dir   = published(cache, coordinate, version, manifest("json", "1.4.0"))
      val hash  = record(cache, coordinate, version)

      ensure(cache, Sums.empty.recording(coordinate, version, hash)) shouldBe
        Right(Fetch.Fetched(dep(), dir, Some(hash)))
    }

    // Otherwise a project depending on something another project had already fetched would write no
    // sysl.sum line at all, and so would never be checked afterwards.
    "answers with what was recorded, so a project that did not fetch it can still record it" in {
      val cache = emptyCache()

      published(cache, coordinate, version, manifest("json", "1.4.0"))

      val hash = record(cache, coordinate, version)

      ensure(cache).map(_.hash) shouldBe Right(Some(hash))
    }

    "and refused when the two disagree" in {
      val cache = emptyCache()

      published(cache, coordinate, version, manifest("json", "1.4.0"))
      record(cache, coordinate, version)

      val e = refused(cache, Sums.empty.recording(coordinate, version, s"${Hashing.Prefix}${"a" * 64}"))

      e should include("does not hash to what sysl.sum records")
      e should include("A version's content is not supposed to change")
    }

    // An interrupted fetch, or something a person put there. It is not evidence about anything, so
    // it is neither trusted nor silently replaced.
    "a directory with nothing vouching for it is refused rather than trusted" in {
      val cache = emptyCache()

      published(cache, coordinate, version, manifest("json", "1.4.0"))

      refused(cache, Sums.empty.recording(coordinate, version, s"${Hashing.Prefix}${"a" * 64}")) should
        include("nothing recording what it hashed to")
    }

    "and is not refused for a package no line covers, whatever is beside it" in {
      val cache = emptyCache()

      published(cache, coordinate, version, manifest("json", "1.4.0"))

      ensure(cache, Sums.empty.recording("github.com/e/other", version, s"${Hashing.Prefix}${"a" * 64}"))
        .map(_.root) shouldBe Right(Fetch.directory(cache, coordinate, version))
    }
  }

  "a path dependency" - {

    "is the directory it names" in {
      val dir = project(manifest("local", "0.1.0"))
      val d   = Dependency("local", Origin.Local(dir))

      Fetch.ensure(d, Sums.empty, emptyCache()) shouldBe Right(Fetch.Fetched(d, dir, None))
    }

    // No entry in `sysl.sum` and none wanted: a directory beside the consumer is expected to change,
    // which is what it is for.
    "is never checked against sysl.sum" in {
      val dir  = project(manifest("local", "0.1.0"))
      val d    = Dependency("local", Origin.Local(dir))
      val sums = Sums.empty.recording("local", Version(0, 1, 0), s"${Hashing.Prefix}${"a" * 64}")

      Fetch.ensure(d, sums, emptyCache()).map(_.hash) shouldBe Right(None)
    }

    "that is not there says so" in {
      val d = Dependency("local", Origin.Local("/no/such/directory"))

      refused(emptyCache(), Sums.empty, d) should include("is not a directory")
    }
  }

  /** Two builds on a cold cache fetch one package at one version at once. Each clones under a name of
   * its own (`Publish.pending`) — a fixed `<dir>.partial` had each clearing the other's clone — and
   * the one finishing second finds a whole package in place.
   */
  "two fetches of one package" - {
    def cloned(dir: String, marker: String): String = {
      val pending = Publish.pending(dir)

      createDirectories(s"$pending/sh/demo")
      writeFile(s"$pending/sh/demo/demo.sysl", marker)
      pending
    }

    "clone apart" in {
      val dir = s"${emptyCache()}/github.com/e/json/@v1.4.0"

      Publish.pending(dir) should not be Publish.pending(dir)
    }

    "and the second to finish succeeds, leaving the first one's package untouched" in {
      val dir    = s"${emptyCache()}/github.com/e/json/@v1.4.0"
      val first  = cloned(dir, "first")
      val second = cloned(dir, "second")

      Publish.directory(first, dir) shouldBe Right(())
      Publish.directory(second, dir) shouldBe Right(())
      readFile(s"$dir/sh/demo/demo.sysl") shouldBe "first"
      exists(first) shouldBe false
      exists(second) shouldBe false
      Project.parentOf(dir).map(p => listFiles(p).length) shouldBe Some(1)
    }

    "but a directory that cannot be put in place at all is a refusal, with the clone removed" in {
      val cache   = emptyCache()
      val pending = cloned(s"$cache/nowhere/@v1.4.0", "x")

      Publish.directory(pending, s"$cache/no/such/parent/@v1.4.0").isLeft shouldBe true
      exists(pending) shouldBe false
    }
  }

  "removing a tree takes what is under it" in {
    val root = project("", "a/b/c")

    writeFile(s"$root/a/b/c/x", "x")
    Fetch.removeTree(root)
    exists(root) shouldBe false
  }
}
