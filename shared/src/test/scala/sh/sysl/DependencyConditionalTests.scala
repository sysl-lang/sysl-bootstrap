package sh.sysl

import io.github.edadma.cross_platform.*

/** A dependency whose files use `#if`, built from the outside (`Conditional`, `Packages.prefixOf`).
 *
 * `#if` gates the lines of the branch not taken and nothing else, so a module with one gated
 * declaration in one of its files still has every other declaration — whether the module is the
 * project's own, a `--lib` root's, or a package the manifest depends on.
 *
 * The third is the one that broke. A file the parser has to prepare — a branch blanked out, a
 * literate file tangled — comes back as a new `Source`, and which package a file belongs to was a
 * table keyed by the `Source` as collected. So the prepared file was read as the program's own: its
 * module became a second, unprefixed `sh.x.lib`, the program's own modules win an import, and
 * `import sh.x.lib.bar` was answered by a module holding none of the package's other files.
 *
 * **A module of one file cannot show this**, since the misfiled module is then the whole of it and
 * answers every import correctly by accident — which is how `FeatureGateTests` passed throughout.
 * Every case here is two files, the declaration being asked for in the one with no `#if` in it.
 */
class DependencyConditionalTests extends PackageCacheSupport {

  private val plain = "module sh.x.lib\n\nbar() -> int = 3\n"

  /** `b.sysl` gating a declaration of its own at top level, on a condition `cond`. */
  private def gatedTop(cond: String): String =
    s"module sh.x.lib\n\n#if $cond\nfoo() -> int = 1\n#endif\n"

  /** `b.sysl` gating lines inside a function body rather than a declaration. */
  private val gatedBody =
    """module sh.x.lib
      |
      |baz() -> int
      |    var n = 10
      |#if android
      |    n = 20
      |#endif
      |    n
      |""".stripMargin

  /** Writes the module `sh.x.lib` as `a.sysl` and `b.sysl` under `root`. */
  private def writeModule(root: String, a: String, b: String, bName: String = "b.sysl"): Unit = {
    createDirectories(s"$root/sh/x/lib")
    writeFile(s"$root/sh/x/lib/a.sysl", a)
    writeFile(s"$root/sh/x/lib/$bName", b)
  }

  /** A package on disk holding the module, with a manifest of its own. */
  private def dependency(b: String, bName: String = "b.sysl"): String = {
    val root = createTempDirectory("sysl-dep-if-")

    writeFile(s"$root/${PackageConfig.FileName}", """package { name = "lib", version = "0.1.0" }""" + "\n")
    writeModule(root, plain, b, bName)
    root
  }

  /** A program whose manifest depends on `dep` by path. */
  private def app(dep: String, program: String): String = {
    val root = createTempDirectory("sysl-dep-if-app-")

    writeFile(s"$root/${PackageConfig.FileName}",
      s"""package { name = "app", version = "0.1.0" }
         |dependencies { lib { path = "$dep" } }
         |""".stripMargin)
    writeFile(s"$root/main.sysl", program)
    root
  }

  private def run(root: String, libs: List[String] = Nil): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root, libs = libs))))

    if status != 0 then fail(s"the driver exited with $status:\n${out.toString}${notes.toString}")

    out.toString
  }

  private def refused(root: String): String = {
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(Discarded)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root))))

    if status == 0 then fail("expected a refusal, got a build")

    notes.toString
  }

  private val importsBar = "import sh.x.lib.bar\n\nprint(bar())\n"

  "a dependency named in the manifest" - {

    "keeps the declarations of a module one of whose files has an '#if' at top level" in {
      run(app(dependency(gatedTop("android")), importsBar)) shouldBe "3\n"
    }

    // Freedom to disagree, both ways round: the gated declaration is there where its condition holds
    // and absent where it does not, and in both builds the package's other file is untouched.
    "and the gated declaration resolves where its condition holds" in {
      run(app(dependency(gatedTop("hosted")),
        "import sh.x.lib.bar\nimport sh.x.lib.foo\n\nprint(bar() + foo())\n")) shouldBe "4\n"
    }

    "and only there" in {
      refused(app(dependency(gatedTop("android")), "import sh.x.lib.foo\n\nprint(foo())\n")) should
        include("sh.x.lib' declares no 'foo'")
    }

    "keeps them when the '#if' is inside a function body, and gates the body's lines" in {
      run(app(dependency(gatedBody),
        "import sh.x.lib.bar\nimport sh.x.lib.baz\n\nprint(bar())\nprint(baz())\n")) shouldBe "3\n10\n"
    }

    // The other preparation that makes a new `Source`, so the other way into the same defect.
    "keeps them when one of its files is literate" in {
      val literate =
        """The module's second file, written as prose with the program indented.
          |
          |    module sh.x.lib
          |
          |    qux() -> int = 5
          |""".stripMargin

      run(app(dependency(literate, "b.lsysl"),
        "import sh.x.lib.bar\nimport sh.x.lib.qux\n\nprint(bar() + qux())\n")) shouldBe "8\n"
    }
  }

  "the same module" - {

    "named as a '--lib' root" in {
      val lib = createTempDirectory("sysl-dep-if-lib-")
      val prog = createTempDirectory("sysl-dep-if-prog-")

      writeModule(lib, plain, gatedTop("android"))
      writeFile(s"$prog/main.sysl", importsBar)

      run(prog, libs = List(lib)) shouldBe "3\n"
    }

    "as the project's own" in {
      val root = createTempDirectory("sysl-dep-if-root-")

      writeFile(s"$root/${PackageConfig.FileName}", """package { name = "app", version = "0.1.0" }""" + "\n")
      writeModule(root, plain, gatedTop("android"))
      writeFile(s"$root/main.sysl", importsBar)

      run(root) shouldBe "3\n"
    }
  }
}
