package sh.sysl

import io.github.edadma.cross_platform.*

/** `emit-typed` and `sysl prove` over a project with a dependency: the same compilation the build
 * makes, so a package's modules carry the name the build gives them and its test scaffolding is
 * nowhere in the tree (`Compiler.typedWith`).
 *
 * They were one compilation short of it. The driver handed a dependency's sources in with the
 * program's own and no `Packages`, so `emit-typed` named the package's modules as written
 * (`geom$double`) where `emit-llvm` names them under the canonical prefix
 * (`github.com.e.geom.geom$double`), counted them as the program's own, and printed the package's
 * `@test` functions — which no build of the consumer ever keeps.
 *
 * **Every case compares against `emit-llvm` rather than against a spelling written here**, since the
 * claim is agreement with the build; the literal prefix is asserted once so a change to how the
 * build spells it cannot make both sides agree about something nobody meant.
 */
class TypedPackageTests extends PackageCacheSupport {

  private val coordinate = "github.com/e/geom"
  private val prefixed   = "github.com.e.geom.geom$double"

  /** The package in a cache of its own: one module, and a `@tests` file beside it whose one test
   * is named so that no other line of any output can contain it by accident.
   */
  private def geomCache(): String = {
    val cache = emptyCache()
    val at    = published(cache, coordinate, Version(1, 0, 0), manifest("geom", "1.0.0"))

    createDirectories(s"$at/geom")
    writeFile(s"$at/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")
    writeFile(s"$at/geom/tests.sysl",
      """module geom
        |@tests
        |
        |@test("doubling")
        |geom_scaffolding_doubles()
        |    assert_eq(double(21), 42)
        |""".stripMargin)
    cache
  }

  private def project(program: String, deps: String): String = {
    val root = createTempDirectory("sysl-typed-app-")

    writeFile(s"$root/${PackageConfig.FileName}",
      s"""package { name = "app", version = "0.1.0" }
         |${if deps.isEmpty then "" else s"dependencies { $deps }"}
         |""".stripMargin)
    writeFile(s"$root/main.sysl", program)
    root
  }

  private val consumer = "import geom.double\n\nprint(double(21))\n"
  private val dep      = s"""g { git = "$coordinate", version = "1.0.0" }"""

  private def driven(cfg: Config): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(Console.withErr(notes)(sh.sysl.execute(cfg)))

    if status != 0 then fail(s"the driver exited with $status:\n${out.toString}${notes.toString}")

    out.toString
  }

  private def command(name: String, root: String): String = driven(Config(command = name, file = root))

  "emit-typed over a project with a dependency" - {

    "names the package's function by the prefix emit-llvm gives it" in {
      val cache = geomCache()
      val root  = project(consumer, dep)
      val (typed, ir) = Fetch.usingCache(cache)((command("emit-typed", root), command("emit-llvm", root)))

      ir should include(prefixed)
      typed should include("name: \"" + prefixed + "\"")
    }

    "and never by the name the package wrote" in {
      val cache = geomCache()
      val root  = project(consumer, dep)
      val typed = Fetch.usingCache(cache)(command("emit-typed", root))

      typed should not include "\"geom$double\""
    }

    "and carries none of the package's @tests scaffolding, exactly as the build does not" in {
      val cache = geomCache()
      val root  = project(consumer, dep)
      val (typed, ir) = Fetch.usingCache(cache)((command("emit-typed", root), command("emit-llvm", root)))

      ir should not include "geom_scaffolding_doubles"
      typed should not include "geom_scaffolding_doubles"
    }
  }

  "a project with no dependency" - {

    // The strip is of what was *handed* to the compilation. The program's own `@test` functions are
    // part of what it declares, and printing them is what `emit-typed` did before any of this.
    "still shows its own @test functions" in {
      val root = project(
        """double(n: int) -> int = n * 2
          |
          |@test("doubling")
          |own_test_doubles()
          |    assert_eq(double(21), 42)
          |
          |print(double(21))
          |""".stripMargin, "")

      command("emit-typed", root) should include("own_test_doubles")
    }
  }

  "prove over a project with a dependency" - {

    // WhyML translates the program's *own* modules (`typedWith`'s second answer), and a dependency
    // counted as one of them had its bodies translated as though the consumer had written them.
    "does not translate the package's scaffolding as the program's own" in {
      val cache = geomCache()
      val root  = project(consumer, dep)
      val mlw   = Fetch.usingCache(cache)(driven(Config(command = "prove", file = root, emitWhyML = true)))

      mlw should not include "geom_scaffolding_doubles"
    }
  }
}
