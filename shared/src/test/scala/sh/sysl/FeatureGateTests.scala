package sh.sysl

import io.github.edadma.cross_platform.*

/** A package's enabled features as `#if` symbols, end to end: manifest, resolution, gate and the
 * program that comes out (`Conditional`, `FeatureResolution`).
 *
 * Every case here is written as an assertion about what the built program **prints**, because the
 * claim is about which lines reached the compiler at all and a feature set is a thing a resolver
 * could answer correctly while nothing downstream read it.
 *
 * The isolation case is the one this suite exists for: a symbol is a fact about the package whose
 * manifest declared it, so the root's `feature_x` has to be invisible in a dependency's source and
 * a dependency's in the root's. A gate that read one global set would pass every other case here.
 */
class FeatureGateTests extends PackageCacheSupport {

  /** A project with a manifest of its own and one program file at its root. */
  private def app(config: String, program: String): String = {
    val root = createTempDirectory("sysl-feature-app-")

    writeFile(s"$root/${PackageConfig.FileName}", config)
    writeFile(s"$root/main.sysl", program)
    root
  }

  /** A package on disk holding one module, whose manifest is whatever the case needs. */
  private def dependency(module: String, text: String, feats: String = ""): String = {
    val root = createTempDirectory("sysl-feature-dep-")

    writeFile(s"$root/${PackageConfig.FileName}",
      s"""package { name = "$module", version = "1.0.0" }
         |${if feats.isEmpty then "" else s"features { $feats }"}
         |""".stripMargin)
    createDirectories(s"$root/$module")
    writeFile(s"$root/$module/$module.sysl", s"module $module\n\n$text\n")
    root
  }

  private def manifestSaying(deps: String, feats: String): String =
    s"""package { name = "app", version = "0.1.0" }
       |${if deps.isEmpty then "" else s"dependencies { $deps }"}
       |${if feats.isEmpty then "" else s"features { $feats }"}
       |""".stripMargin

  private def run(root: String): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root))))

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

  private val twoBlocks =
    """#if feature_a
      |print("a")
      |#endif
      |#if feature_b
      |print("b")
      |#endif
      |print("end")
      |""".stripMargin

  "a project's own features" - {

    "the enabled one's block is compiled in and the declared one nobody turned on is not" in {
      run(app(manifestSaying("", "default = [a], a = [], b = []"), twoBlocks)) shouldBe "a\nend\n"
    }

    // Freedom to disagree: the same program against a manifest whose `default` turns on `b` instead,
    // so a gate that kept everything and a gate that kept nothing both fail one of the two.
    //
    // It is also what pins the **run cache**'s key, which is over each file's text: this program is
    // byte-identical to the one above, so a key that did not carry the features would hand this case
    // the binary built for that one.
    "and turning the other one on instead swaps which block is there" in {
      run(app(manifestSaying("", "default = [b], a = [], b = []"), twoBlocks)) shouldBe "b\nend\n"
    }

    // The stronger claim, and the one an output comparison alone cannot make: the lines are gone
    // before anything reads them, rather than present and unreached.
    "a gated-out block is not compiled at all, so what it names need not exist" in {
      val program =
        """#if feature_b
          |print(nothing_declares_this())
          |#endif
          |print("built")
          |""".stripMargin

      run(app(manifestSaying("", "default = [a], a = [], b = []"), program)) shouldBe "built\n"
      refused(app(manifestSaying("", "default = [b], a = [], b = []"), program)) should
        include("nothing_declares_this")
    }
  }

  "a dependency's features are its own" - {

    "what a consumer asked for is a symbol in the package it asked of" in {
      val dep = dependency("engine",
        """#if feature_fast
          |tag() -> string = "fast"
          |#else
          |tag() -> string = "plain"
          |#endif""".stripMargin,
        feats = "fast = []")

      run(app(manifestSaying(s"""e { path = "$dep", features = [fast] }""", ""),
        "print(engine.tag())\n")) shouldBe "fast\n"

      run(app(manifestSaying(s"""e { path = "$dep" }""", ""),
        "print(engine.tag())\n")) shouldBe "plain\n"
    }

    // Both directions of the isolation in one program: the root's own file sees `feature_x` and the
    // dependency's file, gated on the same word, does not.
    "and the root's features are invisible in it, as its are in the root" in {
      val dep = dependency("engine",
        """#if feature_x
          |tag() -> string = "engine saw x"
          |#else
          |tag() -> string = "engine did not see x"
          |#endif""".stripMargin)

      val program =
        """#if feature_x
          |print("root sees x")
          |#endif
          |print(engine.tag())
          |""".stripMargin

      run(app(manifestSaying(s"""e { path = "$dep" }""", "default = [x], x = []"), program)) shouldBe
        "root sees x\nengine did not see x\n"
    }
  }

  // The shape a package whose feature is named after the dependency it turns on actually has, end to
  // end. Both halves have to land: the dependency in the graph, AND `feature_zstd` defined so the
  // gated body is compiled. Reading `default`'s `zstd` as the dependency keeps the first and loses
  // the second in silence -- the library links and every gated body is gone.
  "a feature named after the dependency it turns on" - {

    val dep = dependency("zstd", """tag() -> string = "compressed"""")

    val program =
      """#if feature_zstd
        |print(zstd.tag())
        |#else
        |print("no zstd in this build")
        |#endif
        |""".stripMargin

    "brings the dependency in AND defines its gate symbol" in {
      run(app(manifestSaying(s"""zstd { path = "$dep", optional = true }""",
        "default = [zstd], zstd = [zstd]"), program)) shouldBe "compressed\n"
    }

    // Freedom to disagree: the same program and the same manifest but for `default`, which now turns
    // nothing on -- so a gate that kept every block regardless fails here.
    "and leaves both out where default turns it off" in {
      run(app(manifestSaying(s"""zstd { path = "$dep", optional = true }""",
        "default = [], zstd = [zstd]"), program)) shouldBe "no zstd in this build\n"
    }
  }
}
