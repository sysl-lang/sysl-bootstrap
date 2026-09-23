package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `vendor/` — a project's own copy of what it depends on, which `sysl vendor` fills.
  *
  * **It is the machine's cache moved into the project rather than a second mechanism beside it**, so
  * there are only two claims to make: `Fetch.cacheRoot` prefers it when it is there, and
  * `Project.collect` walks past it. Everything downstream — resolution, the sums, the fetch — is the
  * code that was already there, which is the point of the design and the reason there is so little
  * to test.
  *
  * **The second claim is the one that bit.** A `vendor/` full of `.sysl` sitting at a project root is
  * compiled *as that project's own modules* by a walk that does not know better, and the first thing
  * it says is that `sh.sysl.json` sits in `vendor.github.com.sysl-lang.json.@v0.1.2.sh.sysl.json` —
  * which reads as a defect in the dependency. `examples/` is skipped for the same reason one line
  * above it.
  */
class VendorTests extends AnyFreeSpec with Matchers {

  /** A project with a dependency-shaped tree under `vendor/`, laid out the way the cache lays one
    * out — and deliberately a module whose name cannot be the project's own, since that is exactly
    * what a walk taking it would complain about.
    */
  private def vendored(): String = {
    val root = createTempDirectory("sysl-vproj-")
    val pkg  = s"$root/${Project.VendorDir}/github.com/sysl-lang/json/@v0.1.2/sh/sysl/json"

    writeFile(s"$root/${PackageConfig.FileName}",
      "package {\n  name = \"demo\"\n  version = \"0.1.0\"\n}\n")
    writeFile(s"$root/main.sysl", "print(21 * 2)\n")
    createDirectories(pkg)
    writeFile(s"$pkg/json.sysl", "module sh.sysl.json\n\nfour() -> int = 4\n")

    root
  }

  "a project's dependencies are not its own source" in {
    val root  = vendored()
    val files = Project.collect(root, Some(Target.default.os)).map(_.name)

    files.map(Project.basename) should contain("main.sysl")
    files.map(Project.basename) should not contain "json.sysl"
  }

  // The whole of what makes a vendored project build with the network off.
  "the cache is the project's own where it has one" in {
    val root = vendored()

    Fetch.cacheRoot(root) shouldBe Right(s"$root/${Project.VendorDir}")
  }

  // And a project without one is exactly where it was: the machine's cache, shared between every
  // project on it, which is what keeps N projects from holding N copies of one library.
  "and the machine's where it has not" in {
    val root = createTempDirectory("sysl-plain-")

    Fetch.cacheRoot(root).getOrElse("") should not include Project.VendorDir
  }

  /** A relative spelling of `target`, reached by climbing from the real working directory with
    * `..` and back down — the same *kind* of literal, non-absolute argument `sysl vendor .` types
    * from inside a project, without requiring this suite to change the JVM's own working directory
    * (there is no such call in `cross_platform`, and none is added for a test).
    */
  private def relativeTo(target: String): String = {
    val cwd    = getCurrentDirectory.stripPrefix("/").split("/").filter(_.nonEmpty).toList
    val to     = target.stripPrefix("/").split("/").filter(_.nonEmpty).toList
    val common = cwd.zip(to).takeWhile(_ == _).length

    (List.fill(cwd.length - common)("..") ::: to.drop(common)).mkString("/")
  }

  // Found on slate 2026-09-22: `sysl vendor .` reported freshly-fetched packages as not hashing to
  // what `sysl.sum` recorded, while `sysl vendor <absolute path>` reproduced the recorded hashes
  // exactly. `projectRoot` returned a directory argument literally, so a project reached as `.`
  // stayed `.` all the way into `Fetch.cacheRoot` and the `vendor/` it derives — and a `.`-rooted
  // path handed to `Hashing.treeHash` cannot strip itself back off the absolute paths a directory
  // walk returns, so the listing that gets hashed holds unstripped absolute paths, which differ by
  // machine and by working directory even for byte-identical content.
  "a project root reached by a relative path resolves to the same absolute root as its absolute spelling" in {
    val root = vendored()
    val rel  = relativeTo(root)

    rel should not startWith "/"
    projectRoot(rel) shouldBe projectRoot(root)
    projectRoot(rel) should startWith("/")
  }

  "and a fetched package hashes the same whether the project it was fetched for was named relatively or absolutely" in {
    val root = createTempDirectory("sysl-vendor-relroot-")
    val rel  = relativeTo(root)

    // The shape `Fetch.clone` computes a hash over: `<projectRoot>/vendor/<coordinate>/@v<version>.partial`,
    // built here directly rather than through a real `git clone` so the test needs no network.
    def partialUnder(base: String): String =
      s"$base/${Project.VendorDir}/github.com/sysl-lang/demo/@v1.0.0.partial"

    val pkg = partialUnder(root)
    createDirectories(s"$pkg/sh/sysl/demo")
    writeFile(s"$pkg/sh/sysl/demo/demo.sysl", "module sh.sysl.demo\n\nfour() -> int = 4\n")

    val fromAbsoluteRoot = Hashing.treeHash(partialUnder(projectRoot(root)))
    val fromRelativeRoot = Hashing.treeHash(partialUnder(projectRoot(rel)))

    fromRelativeRoot shouldBe fromAbsoluteRoot
  }
}
