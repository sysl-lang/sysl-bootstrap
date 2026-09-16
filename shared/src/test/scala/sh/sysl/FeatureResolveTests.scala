package sh.sysl

/** Which features are on, and therefore which optional dependencies are in the graph
 * (`reference/packages.md § Dependencies`).
 *
 * The two questions are one question asked from opposite ends: a feature is a name for a set of
 * optional dependencies, so what is enabled decides what is fetched, and what is fetched brings
 * consumers whose own requests decide what else is enabled. Every case here is written as an
 * assertion about the **graph** rather than about the feature set alone, because a feature nothing
 * turns on is indistinguishable from one that is on and selects nothing.
 */
class FeatureResolveTests extends PackageCacheSupport {

  private def entry(label: String, coordinate: String, version: String, extra: String = ""): String =
    s"""$label { git = "$coordinate", version = "$version"${if extra.isEmpty then "" else s", $extra"} }"""

  private def pkg(name: String, version: String, deps: String, feats: String = ""): String =
    s"""package { name = "$name", version = "$version" }
       |${if deps.isEmpty then "" else s"dependencies { $deps }"}
       |${if feats.isEmpty then "" else s"features { $feats }"}
       |""".stripMargin

  private def publish(cache: String, coordinate: String, version: Version, module: String,
                      deps: String = "", feats: String = ""): String =
    published(cache, coordinate, version, pkg(module, version.toString, deps, feats),
      s"$module/$module.sysl" -> s"module $module\n")

  /** A cache holding `tiny`, and a `buf` whose two features each turn it on. */
  private def withBuf(cache: String): Unit = {
    publish(cache, "github.com/e/tiny", Version(1, 0, 0), "tiny")
    publish(cache, "github.com/e/buf", Version(1, 0, 0), "buf",
      deps = entry("tiny", "github.com/e/tiny", "1.0.0", "optional = true"),
      feats = "default = [tiny], x = [tiny], y = [tiny]")
  }

  private def graphFor(root: String, cache: String, request: FeatureRequest = FeatureRequest(),
                       testing: Boolean = false): Resolve.Graph =
    resolved(root, cache, Sums.empty, request, testing) match
      case Right(g) => g
      case Left(e)  => fail(s"expected a graph, got: $e")

  private def refusalFor(root: String, cache: String, request: FeatureRequest = FeatureRequest(),
                         testing: Boolean = false): String =
    resolved(root, cache, Sums.empty, request, testing) match
      case Left(e)  => e
      case Right(g) => fail(s"expected a refusal, got: ${g.packages.map(_.canonical)}")

  "an optional dependency is in the graph only where a feature names it" - {

    // Nothing asked for `server`, so `extra` is not fetched at all — which is what keeps whatever
    // its manifest requires from being asked of this machine.
    "nothing enables it, so it is not there" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "server = [extra]"))

      val g = graphFor(root, cache)

      selected(g) shouldBe Map.empty
      g.features("") shouldBe Set.empty
    }

    // `default` is an ordinary feature that happens to be the one asking for nothing gets.
    "default turns it on" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "default = [extra]"))

      val g = graphFor(root, cache)

      selected(g).keySet shouldBe Set("github.com.e.extra")
      g.features("") shouldBe Set("default")
    }

    "no-default-features takes the same one away" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "default = [extra]"))

      val g = graphFor(root, cache, FeatureRequest(noDefaultFeatures = true))

      selected(g) shouldBe Map.empty
      g.features("") shouldBe Set.empty
    }

    "asking for it by name adds it" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "server = [extra]"))

      val g = graphFor(root, cache, FeatureRequest(features = List("server")))

      selected(g).keySet shouldBe Set("github.com.e.extra")
      g.features("") shouldBe Set("server")
    }

    // A feature may name another feature, and following the implications is what decides the set.
    "a feature implying a feature reaches what the second one names" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "default = [server], server = [extra]"))

      val g = graphFor(root, cache)

      selected(g).keySet shouldBe Set("github.com.e.extra")
      g.features("") shouldBe Set("default", "server")
    }

    // Every feature this manifest declares, without naming any of them.
    "all-features turns on every one the manifest declares" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "server = [extra], client = [server]"))

      val g = graphFor(root, cache, FeatureRequest(allFeatures = true))

      selected(g).keySet shouldBe Set("github.com.e.extra")
      g.features("") shouldBe Set("server", "client")
    }

    // The ordinary gate compiles what the ordinary build compiles, so gated code would otherwise be
    // code the suite never sees.
    "sysl test on the root enables all of them" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "server = [extra]"))

      graphFor(root, cache, testing = true).features("") shouldBe Set("server")
    }

    // ...unless the caller said what it wanted, in which case that is the configuration under test.
    "an explicit request under test is not widened" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "default = [extra], server = [extra]"))

      val g = graphFor(root, cache, FeatureRequest(noDefaultFeatures = true), testing = true)

      g.features("") shouldBe Set.empty
    }
  }

  "a dependency gets the union of what its consumers asked for" - {

    "two consumers, two features" in {
      val cache = emptyCache()

      withBuf(cache)
      publish(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = entry("buf", "github.com/e/buf", "1.0.0", """features = [x], default_features = false"""))
      publish(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = entry("buf", "github.com/e/buf", "1.0.0", """features = [y], default_features = false"""))

      val root = project(pkg("app", "0.1.0",
        deps = s"${entry("a", "github.com/e/a", "1.0.0")}, ${entry("b", "github.com/e/b", "1.0.0")}"))

      graphFor(root, cache).features("github.com.e.buf") shouldBe Set("x", "y")
    }

    // One consumer turning `default` off is saying what *it* does not need; it does not take the
    // feature away from a sibling that took the package the ordinary way.
    "one consumer turning default off does not drop it" in {
      val cache = emptyCache()

      withBuf(cache)
      publish(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = entry("buf", "github.com/e/buf", "1.0.0", """features = [x], default_features = false"""))
      publish(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = entry("buf", "github.com/e/buf", "1.0.0"))

      val root = project(pkg("app", "0.1.0",
        deps = s"${entry("a", "github.com/e/a", "1.0.0")}, ${entry("b", "github.com/e/b", "1.0.0")}"))

      graphFor(root, cache).features("github.com.e.buf") shouldBe Set("default", "x")
    }

    "every consumer turning it off does drop it" in {
      val cache = emptyCache()

      withBuf(cache)
      publish(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = entry("buf", "github.com/e/buf", "1.0.0", """features = [x], default_features = false"""))

      val root = project(pkg("app", "0.1.0",
        deps = entry("a", "github.com/e/a", "1.0.0")))

      graphFor(root, cache).features("github.com.e.buf") shouldBe Set("x")
    }
  }

  "a request has to name a feature that package declares" - {

    "the refusal names the consumer, the dependency and the feature" in {
      val cache = emptyCache()

      withBuf(cache)
      publish(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = entry("buf", "github.com/e/buf", "1.0.0", "features = [nope]"))

      val root = project(pkg("app", "0.1.0", deps = entry("a", "github.com/e/a", "1.0.0")))
      val out  = refusalFor(root, cache)

      out should include("github.com.e.a")
      out should include("'buf'")
      out should include("'nope'")
    }

    "and so does the root's own request" in {
      val cache = emptyCache()

      publish(cache, "github.com/e/extra", Version(1, 0, 0), "extra")

      val root = project(pkg("app", "0.1.0",
        deps = entry("extra", "github.com/e/extra", "1.0.0", "optional = true"),
        feats = "server = [extra]"))

      refusalFor(root, cache, FeatureRequest(features = List("srever"))) should include("'srever'")
    }
  }

  // The case the resolution has to be a fixpoint for: turning a feature on brings a package into the
  // graph, and *that* package is a consumer whose own request enables something one pass had already
  // settled.
  "enabling a feature brings a consumer whose own request enables another" in {
    val cache = emptyCache()

    withBuf(cache)
    publish(cache, "github.com/e/a", Version(1, 0, 0), "a",
      deps = entry("buf", "github.com/e/buf", "1.0.0", """features = [y], default_features = false"""))

    val root = project(pkg("app", "0.1.0",
      deps = s"""${entry("a", "github.com/e/a", "1.0.0", "optional = true")}, ${entry(
          "buf", "github.com/e/buf", "1.0.0", "default_features = false")}""",
      feats = "server = [a]"))

    val off = graphFor(root, cache)
    val on  = graphFor(root, cache, FeatureRequest(features = List("server")))

    off.features("github.com.e.buf") shouldBe Set.empty
    on.features("github.com.e.buf") shouldBe Set("y")

    // `tiny` is buf's own optional dependency, reachable only once `y` is on — which is a fact about
    // buf that nothing knew when the round that put `a` in the graph resolved it. One pass cannot
    // produce this line.
    off.packages.map(_.canonical) should not contain "github.com.e.tiny"
    on.packages.map(_.canonical) should contain("github.com.e.tiny")
  }

  // A bare member names the FEATURE of that name where one is declared, so `server`'s `lmdb` is the
  // feature `lmdb` and its own list is followed -- `zstd` comes on with it.
  "a feature member naming a feature chains into that feature even where a dependency shares its label" in {
    val cache = emptyCache()

    publish(cache, "github.com/e/lmdb", Version(1, 0, 0), "lmdb")
    publish(cache, "github.com/e/zstd", Version(1, 0, 0), "zstd")

    val root = project(pkg("app", "0.1.0",
      deps = s"""${entry("lmdb", "github.com/e/lmdb", "1.0.0", "optional = true")}, ${entry(
          "zstd", "github.com/e/zstd", "1.0.0", "optional = true")}""",
      feats = "server = [lmdb], lmdb = [zstd]"))

    val g = graphFor(root, cache, FeatureRequest(features = List("server")))

    g.features("") shouldBe Set("server", "lmdb")
    selected(g).keySet shouldBe Set("github.com.e.lmdb", "github.com.e.zstd")
  }

  // The shape a package whose features are named after the dependencies they turn on actually has:
  // every member of `default` is a feature, and each of those features turns its own dependency on
  // through a self-reference. Both halves have to come out -- all six features enabled, so every
  // `feature_*` symbol is defined, AND all six dependencies in the graph.
  "a default naming features that each turn on their own like-named dependency enables both" in {
    val cache = emptyCache()

    for label <- List("redis", "lmdb", "zstd", "brotli", "nghttp2", "libwebp") do
      publish(cache, s"github.com/e/$label", Version(1, 0, 0), label)

    val optional = List("redis", "lmdb", "zstd", "brotli", "nghttp2", "libwebp")
      .map(l => entry(l, s"github.com/e/$l", "1.0.0", "optional = true"))
      .mkString(", ")

    val root = project(pkg("app", "0.1.0", deps = optional,
      feats = """default = [http2, redis, lmdb, zstd, brotli, webp],
                 redis = [redis], lmdb = [lmdb], zstd = [zstd], brotli = [brotli],
                 http2 = [nghttp2], webp = [libwebp]"""))

    val g = graphFor(root, cache)

    g.features("") shouldBe Set("default", "http2", "redis", "lmdb", "zstd", "brotli", "webp")
    selected(g).keySet shouldBe Set("github.com.e.redis", "github.com.e.lmdb", "github.com.e.zstd",
      "github.com.e.brotli", "github.com.e.nghttp2", "github.com.e.libwebp")
  }

  // The self-reference on its own: the feature is on, its dependency is on, and nothing read it as a
  // feature turning itself on.
  "a feature whose only member is its own like-named dependency turns that dependency on" in {
    val cache = emptyCache()

    publish(cache, "github.com/e/lmdb", Version(1, 0, 0), "lmdb")

    val root = project(pkg("app", "0.1.0",
      deps = entry("lmdb", "github.com/e/lmdb", "1.0.0", "optional = true"),
      feats = "lmdb = [lmdb]"))

    val g = graphFor(root, cache, FeatureRequest(features = List("lmdb")))

    g.features("") shouldBe Set("lmdb")
    selected(g).keySet shouldBe Set("github.com.e.lmdb")
  }

  // `dep:X` names the dependency wherever it is written, so it turns the dependency on WITHOUT
  // enabling the feature that shares its name.
  "a 'dep:' member turns the dependency on and leaves the like-named feature off" in {
    val cache = emptyCache()

    publish(cache, "github.com/e/lmdb", Version(1, 0, 0), "lmdb")
    publish(cache, "github.com/e/zstd", Version(1, 0, 0), "zstd")

    val root = project(pkg("app", "0.1.0",
      deps = s"""${entry("lmdb", "github.com/e/lmdb", "1.0.0", "optional = true")}, ${entry(
          "zstd", "github.com/e/zstd", "1.0.0", "optional = true")}""",
      feats = """server = ["dep:lmdb"], lmdb = [zstd]"""))

    val g = graphFor(root, cache, FeatureRequest(features = List("server")))

    g.features("") shouldBe Set("server")
    selected(g).keySet shouldBe Set("github.com.e.lmdb")
  }
}
