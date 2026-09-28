package sh.sysl

import io.github.edadma.cross_platform.*

/** `sysl deps` — the resolved graph, printed, and who asked for each version
 * (`reference/packages.md § Which version you get`).
 *
 * ==Why the command exists, which is what these cases are really pinning==
 *
 * A package reached through another is importable (`§ 9`), so a program can be built against
 * packages its own manifest never names, and nothing in the files in front of a reader says what
 * those are. Selection is silent by design; this is where somebody goes to ask.
 *
 * **The claims are the half nothing else could reconstruct**, and two cases below say so directly: a
 * version that was passed over has its manifest dropped from the graph, so the claim that *lost* —
 * the whole reason a version is higher than the manifest says — survives only because the resolver
 * wrote it down as it went. `ResolveTests` pins the recording; this suite pins the reading.
 *
 * ==The graph is resolved rather than the driver being driven, for all but two cases==
 *
 * `Fetch.usingCache` redirects per thread, so holding it here would not collide with another suite
 * holding it on its own thread — but this suite still has no reason to drive the whole compiler for
 * a claim that is about the graph, not the build. So the cases with git dependencies in them build a
 * graph against a cache of their own — the same `Resolve.graph` the driver calls, one frame lower —
 * and hand it to the printer. The two that go through `execute` are the ones that need no cache at
 * all: a project with no dependencies, and one whose dependency is a directory.
 */
class DepsCliTests extends PackageCacheSupport {

  private def dep(label: String, coordinate: String, version: String): String =
    s"""$label { git = "$coordinate", version = "$version" }"""

  private def configOf(root: String): PackageConfig =
    PackageConfig.read(readFile(s"$root/${PackageConfig.FileName}")) match
      case Right(c) => c
      case Left(e)  => fail(e)

  /** What the command prints for a graph, captured off the printer the driver reaches. */
  private def shown(root: String, cache: String): String = {
    val out    = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(printGraph(configOf(root), resolve(root, cache)))

    status shouldBe 0
    out.toString
  }

  /** The whole driver, for the cases that need no package cache. */
  private def ran(root: String): (Int, String) = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(Console.withErr(notes)(
      sh.sysl.execute(Config(command = "deps", file = root))))

    (status, out.toString)
  }

  "what an argument list says" - {

    "deps names a command and takes a path" in {
      val cfg = parseArgs(Seq("deps", "/somewhere"))

      cfg.map(_.command) shouldBe Some("deps")
      cfg.map(_.file) shouldBe Some("/somewhere")
    }

    // Unlike `targets`, which stands alone — this one is a question about a particular project.
    "and will not stand without one" in {
      parseArgs(Seq("deps")) shouldBe None
    }
  }

  "what it prints" - {

    "the project first, by the name and version its own manifest gives" in {
      shown(project(manifest("app", "0.1.0")), emptyCache()) should startWith("app 0.1.0\n")
    }

    "and says so rather than printing an empty list where there is nothing" in {
      shown(project(manifest("app", "0.1.0")), emptyCache()) shouldBe
        "app 0.1.0\n\nthis project depends on nothing\n"
    }

    "a dependency, the version settled on, and who asked" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val root = project(manifest("app", "0.1.0", dep("buf", "github.com/e/buf", "1.4.0")))

      shown(root, cache) shouldBe
        """app 0.1.0
          |
          |github.com/e/buf  1.4.0
          |    app asks for 1.4.0
          |""".stripMargin
    }

    // The whole point of the command: the manifest says 1.2.0, the build uses 1.4.0, and the line
    // that explains it belongs to a package this project never named.
    "a coordinate somebody was overtaken at, with both claims and a marker" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "buf")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("buf", "github.com/e/buf", "1.4.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}"))

      shown(root, cache) shouldBe
        """app 0.1.0
          |
          |github.com/e/a    1.0.0
          |    app asks for 1.0.0
          |github.com/e/b    1.0.0
          |    app asks for 1.0.0
          |github.com/e/buf  1.4.0  (raised)
          |    github.com/e/a asks for 1.2.0
          |    github.com/e/b asks for 1.4.0
          |""".stripMargin
    }

    // The marker is about a claim losing, not about the graph being deep — so a package reached only
    // through another, at the version it asked for, carries none.
    "and no marker where everybody got what they asked for" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.4.0"))

      val root = project(manifest("app", "0.1.0", dep("a", "github.com/e/a", "1.0.0")))

      shown(root, cache) should not include "(raised)"
    }

    // Wider than the note a build prints, which fires only when the ROOT's own manifest was
    // overtaken — here the root never mentions buf at all and the reader still wants to know.
    "including where the root never named the coordinate that moved" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "buf")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("buf", "github.com/e/buf", "1.4.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}"))

      shown(root, cache) should include("github.com/e/buf  1.4.0  (raised)")
    }

    // A coordinate is full of dots, so the canonical name cannot be turned back into one — the
    // spelling has to come from the claims, which are keyed on what the manifest wrote.
    "a coordinate as its manifest wrote it, slashes and all" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/sysl-json", Version(1, 4, 0), "json")

      val root = project(manifest("app", "0.1.0", dep("j", "github.com/e/sysl-json", "1.4.0")))

      shown(root, cache) should include("github.com/e/sysl-json  1.4.0")
    }
  }

  "a path dependency" - {

    // It has no version, and the directory is the only thing that says which tree it is.
    "prints its directory where a coordinate would print a version" in {
      val other = project(manifest("helper", "0.1.0"), "helper")
      val root  = project(s"""package { name = "app", version = "0.1.0" }
                             |dependencies { h { path = "$other" } }
                             |""".stripMargin)

      val (status, out) = ran(root)

      status shouldBe 0
      out should startWith("app 0.1.0\n")
      out should include(s"h  $other\n")
    }

    // Its own git dependencies are in the graph and are attributed to it, since the label is the only
    // name a package with no coordinate has.
    "and the floors it raises are attributed to its label" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val other = project(manifest("helper", "0.1.0", dep("buf", "github.com/e/buf", "1.4.0")), "helper")
      val root  = project(s"""package { name = "app", version = "0.1.0" }
                             |dependencies { h { path = "$other" } }
                             |""".stripMargin)

      shown(root, cache) should include("    h asks for 1.4.0\n")
    }
  }

  "through the driver" - {

    "a project with no dependencies is answered rather than refused" in {
      ran(project(manifest("app", "0.1.0"))) shouldBe
        (0, "app 0.1.0\n\nthis project depends on nothing\n")
    }

    // A manifest need not name its package, and a heading reading as though nobody owned the project
    // is worse than a stand-in.
    "a project whose manifest names no package still has a heading" in {
      val (status, out) = ran(project(""))

      status shouldBe 0
      out should startWith("this project\n")
    }

    // The command asks nothing of a machine, so it stops above the target and a project pinned to
    // something this machine cannot build for is still inspectable.
    "a project whose default target is not this machine" in {
      val root = project(s"""package { name = "app", version = "0.1.0", target = "thumb-freestanding" }
                            |""".stripMargin)

      ran(root)._1 shouldBe 0
    }
  }
}
