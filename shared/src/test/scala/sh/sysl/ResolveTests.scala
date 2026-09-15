package sh.sysl

import io.github.edadma.cross_platform.*

/** Minimal Version Selection and the naming rules over it (`reference/packages.md § Which version
 * you get`, `§ 9`).
 *
 * Every graph here is built in a cache of its own, so the answers are about the resolver rather than
 * about what this machine happens to have fetched before.
 */
class ResolveTests extends PackageCacheSupport {

  private def dep(label: String, coordinate: String, version: String, mount: String = ""): String =
    s"""$label { git = "$coordinate", version = "$version"${if mount.isEmpty then "" else s""", mount = "$mount""""} }"""

  "the highest minimum wins, not the newest that exists" - {

    // your project depends on a 1.0.0 and b 1.0.0
    //          a 1.0.0 depends on buf 1.2.0
    //          b 1.0.0 depends on buf 1.4.0
    //                                  -----
    //                    buf resolves to 1.4.0
    "a floor raised from further down the graph" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "buf")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("buf", "github.com/e/buf", "1.4.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}"))

      selected(resolve(root, cache)) shouldBe Map(
        "github.com.e.a" -> "1.0.0",
        "github.com.e.b" -> "1.0.0",
        "github.com.e.buf" -> "1.4.0",
      )
    }

    // 1.10.0 is higher than 1.9.0, which is the case a comparison done on the text gets wrong.
    "versions are compared as numbers" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 9, 0), "buf")
      publishedModule(cache, "github.com/e/buf", Version(1, 10, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.10.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("buf", "github.com/e/buf", "1.9.0")}"))

      selected(resolve(root, cache))("github.com.e.buf") shouldBe "1.10.0"
    }

    // The whole point of MVS: adding a dependency that wants nothing new cannot move anything.
    "a dependency that asks for less does not lower a floor" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "buf")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("buf", "github.com/e/buf", "1.4.0")}"))

      selected(resolve(root, cache))("github.com.e.buf") shouldBe "1.4.0"
    }

    // The lower version's manifest was read on the way to raising the floor; it must not survive
    // into what gets built, and its modules must not be what the import table names.
    "the version that was passed over is not in the graph" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "early")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "later")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("buf", "github.com/e/buf", "1.4.0")}"))

      val graph = resolve(root, cache)

      graph.packages.count(_.canonical == "github.com.e.buf") shouldBe 1

      // `a` asked for 1.2.0 and gets the selected 1.4.0, so what its import lines may name is what
      // the built version holds rather than what the version it asked for held.
      val a = graph.packages.find(_.canonical == "github.com.e.a").get

      a.imports shouldBe Map("later" -> "github.com.e.buf.later")
    }
  }

  "what a name at the head of an import line means" - {

    // The library's own documentation says `json.parse`, and `§ 9` rejected mandatory mounting so
    // that a consumer could write exactly that.
    "a package's own modules, under their own names" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/sysl-json", Version(1, 4, 0), "json", name = "json-lib")

      val root = project(manifest("app", "0.1.0", dep("j", "github.com/e/sysl-json", "1.4.0")))

      resolve(root, cache).packages.head.imports shouldBe
        Map("json" -> "github.com.e.sysl-json.json")
    }

    // The package is the unit of distribution and the module is the unit of code: sqlite3's package
    // is `sqlite3` and its module is `sqlite`, and a consumer reaching the second should not have to
    // say the first.
    "and not the name the package calls itself" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/sqlite3", Version(1, 0, 0), "sqlite", name = "sqlite3")

      val root = project(manifest("app", "0.1.0", dep("s", "github.com/e/sqlite3", "1.0.0")))

      resolve(root, cache).packages.head.imports.keys should contain only "sqlite"
    }

    "a mount hangs the whole package under one segment" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/sysl-json", Version(1, 4, 0), "json")

      val root = project(manifest("app", "0.1.0",
        dep("j", "github.com/e/sysl-json", "1.4.0", mount = "ejson")))

      resolve(root, cache).packages.head.imports shouldBe Map("ejson" -> "github.com.e.sysl-json")
    }

    "every module a package has, where there is more than one" in {
      val cache = emptyCache()

      published(cache, "github.com/e/two", Version(1, 0, 0), manifest("two", "1.0.0"),
        "alpha/alpha.sysl" -> "module alpha\n", "beta/beta.sysl" -> "module beta\n")

      val root = project(manifest("app", "0.1.0", dep("t", "github.com/e/two", "1.0.0")))

      resolve(root, cache).packages.head.imports shouldBe Map(
        "alpha" -> "github.com.e.two.alpha",
        "beta" -> "github.com.e.two.beta",
      )
    }

    // Per-consumer, which is the whole of the two-layer identity: two projects may write different
    // words and still link one copy of the package.
    "is per-package, so a dependency's own table is its own" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 0, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("b", "github.com/e/buf", "1.0.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, " +
          s"${dep("buf", "github.com/e/buf", "1.0.0", mount = "bytes")}"))

      val graph = resolve(root, cache)

      graph.packages.head.imports shouldBe
        Map("a" -> "github.com.e.a.a", "bytes" -> "github.com.e.buf")
      graph.packages.find(_.canonical == "github.com.e.a").get.imports shouldBe
        Map("buf" -> "github.com.e.buf.buf")
    }

    // **A dependency's public surface is made of its own dependencies' types**: a driver hands out a
    // `&Fn() -> &View` and `View` belongs to the toolkit it was built on, so a consumer that could
    // not name the toolkit could not call the one function the driver exists for. Declaring it
    // anyway is a line that says nothing the build could not work out.
    "a package reached through another is importable too" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/ui", Version(1, 0, 0), "ui")
      publishedModule(cache, "github.com/e/driver", Version(1, 0, 0), "driver",
        deps = dep("ui", "github.com/e/ui", "1.0.0"))

      val root = project(manifest("app", "0.1.0", dep("d", "github.com/e/driver", "1.0.0")))

      resolve(root, cache).packages.head.imports shouldBe
        Map("driver" -> "github.com.e.driver.driver", "ui" -> "github.com.e.ui.ui")
    }

    // However far down it is: what a program can name is the whole graph, not the first layer of it.
    "and one reached through two of them" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 0, 0), "buf")
      publishedModule(cache, "github.com/e/ui", Version(1, 0, 0), "ui",
        deps = dep("buf", "github.com/e/buf", "1.0.0"))
      publishedModule(cache, "github.com/e/driver", Version(1, 0, 0), "driver",
        deps = dep("ui", "github.com/e/ui", "1.0.0"))

      val root = project(manifest("app", "0.1.0", dep("d", "github.com/e/driver", "1.0.0")))

      resolve(root, cache).packages.head.imports shouldBe Map(
        "driver" -> "github.com.e.driver.driver",
        "ui" -> "github.com.e.ui.ui",
        "buf" -> "github.com.e.buf.buf",
      )
    }

    // **A mount is a name its writer chose for itself**, so it is not something a consumer inherits:
    // what an inherited package offers is what its own documentation shows.
    "an intermediate's mount does not leak to whoever depends on it" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/json", Version(1, 0, 0), "json")
      publishedModule(cache, "github.com/e/driver", Version(1, 0, 0), "driver",
        deps = dep("j", "github.com/e/json", "1.0.0", mount = "ejson"))

      val root = project(manifest("app", "0.1.0", dep("d", "github.com/e/driver", "1.0.0")))

      val graph = resolve(root, cache)

      graph.packages.head.imports shouldBe
        Map("driver" -> "github.com.e.driver.driver", "json" -> "github.com.e.json.json")
      graph.packages.find(_.canonical == "github.com.e.driver").get.imports shouldBe
        Map("ejson" -> "github.com.e.json")
    }

    // **What a manifest declared beats what arrived through something else**, which is what makes a
    // mount worth writing: the nearer name is the one in front of the person reading the file.
    "a declared name beats an inherited one" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/json", Version(1, 0, 0), "json")
      publishedModule(cache, "github.com/e/driver", Version(1, 0, 0), "driver",
        deps = dep("j", "github.com/e/json", "1.0.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("d", "github.com/e/driver", "1.0.0")}, " +
          s"${dep("j", "github.com/e/json", "1.0.0", mount = "ejson")}"))

      resolve(root, cache).packages.head.imports shouldBe
        Map("driver" -> "github.com.e.driver.driver", "ejson" -> "github.com.e.json")
    }

    // And a name the project wrote itself beats one it never asked for. **Refusing here would mean a
    // project's own module names could be broken by a package it has never heard of.**
    "the project's own module beats an inherited one" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/json", Version(1, 0, 0), "json")
      publishedModule(cache, "github.com/e/driver", Version(1, 0, 0), "driver",
        deps = dep("j", "github.com/e/json", "1.0.0"))

      val root = project(manifest("app", "0.1.0", dep("d", "github.com/e/driver", "1.0.0")), "json")

      resolve(root, cache).packages.head.imports shouldBe
        Map("driver" -> "github.com.e.driver.driver")
    }

    "a package with no modules at all offers nothing to import" in {
      val cache = emptyCache()

      published(cache, "github.com/e/j", Version(1, 0, 0), manifest("j", "1.0.0"))

      val root = project(manifest("app", "0.1.0", dep("j", "github.com/e/j", "1.0.0")))

      resolveRefused(root, cache) should include("has no modules")
    }
  }

  "a collision is an error and never a silent winner" - {

    "two dependencies whose modules want one name" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")
      publishedModule(cache, "github.com/e/two", Version(1, 0, 0), "json", name = "two")

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/one", "1.0.0")}, ${dep("b", "github.com/e/two", "1.0.0")}"))

      val e = resolveRefused(root, cache)

      e should include("claim the same module")
      e should include("mount")
    }

    // **Two packages that both arrived through something else** are the same collision one layer
    // further away, and are refused in the same words — with the fix stated as what a consumer can
    // actually do about it, which is to name one of them.
    "two inherited packages wanting one name" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")
      publishedModule(cache, "github.com/e/two", Version(1, 0, 0), "json", name = "two")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("j", "github.com/e/one", "1.0.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("j", "github.com/e/two", "1.0.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}"))

      val e = resolveRefused(root, cache)

      e should include("claim the same module")
      e should include("mount")
    }

    // ...and naming one of them is what settles it, because a declared name beats an inherited one.
    "which naming one of them settles" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")
      publishedModule(cache, "github.com/e/two", Version(1, 0, 0), "json", name = "two")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("j", "github.com/e/one", "1.0.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("j", "github.com/e/two", "1.0.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}, " +
          s"${dep("one", "github.com/e/one", "1.0.0")}"))

      resolve(root, cache).packages.head.imports shouldBe Map(
        "a" -> "github.com.e.a.a",
        "b" -> "github.com.e.b.b",
        "json" -> "github.com.e.one.json",
      )
    }

    // **Two major versions of one library is the collision worth naming**, and transitive imports
    // made it likely: `§ 4` puts a major above the first in the coordinate, so selection sees two
    // different packages and cannot fold them — while their modules have identical names, because a
    // module's name is its directory. A consumer that named neither otherwise gets a message about
    // two coordinates it has never typed.
    "two major versions of one library say so" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/json", Version(1, 0, 0), "json")
      publishedModule(cache, "github.com/e/json/v2", Version(2, 0, 0), "json")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("j", "github.com/e/json", "1.0.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("j", "github.com/e/json/v2", "2.0.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}"))

      val e = resolveRefused(root, cache)

      e should include("two major versions of one library")
      e should include("mount")
    }

    // And two packages that are not one library keep the plainer sentence, because they are not the
    // same situation and reading them as one would send somebody looking for a version they do not
    // have.
    "and two unrelated packages do not" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")
      publishedModule(cache, "github.com/e/two", Version(1, 0, 0), "json", name = "two")

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/one", "1.0.0")}, ${dep("b", "github.com/e/two", "1.0.0")}"))

      resolveRefused(root, cache) should not include "major versions"
    }

    "and a mount settles it" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")
      publishedModule(cache, "github.com/e/two", Version(1, 0, 0), "json", name = "two")

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/one", "1.0.0")}, " +
          s"${dep("b", "github.com/e/two", "1.0.0", mount = "json2")}"))

      resolve(root, cache).packages.head.imports shouldBe
        Map("json" -> "github.com.e.one.json", "json2" -> "github.com.e.two")
    }

    // The common case rather than an exotic one, and the reason the check cannot be only about
    // dependencies disagreeing with each other.
    "a dependency taking a name the project already uses for a module of its own" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")

      val root = project(manifest("app", "0.1.0", dep("a", "github.com/e/one", "1.0.0")), "json")

      resolveRefused(root, cache) should include("is both a module of this project")
    }

    "and a directory that is not a module does not collide with anything" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")

      val root = bare(manifest("app", "0.1.0", dep("a", "github.com/e/one", "1.0.0")), "json")

      resolve(root, cache).packages.head.imports shouldBe Map("json" -> "github.com.e.one.json")
    }

    "nor does a dot directory, which is never walked at all" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/one", Version(1, 0, 0), "json", name = "one")

      val root = project(manifest("app", "0.1.0", dep("a", "github.com/e/one", "1.0.0")), ".git")

      resolve(root, cache).packages.head.imports shouldBe Map("json" -> "github.com.e.one.json")
    }

    // The case the whole path rule exists for. Two packages laid out by the convention
    // `reference/packages.md § What a dependency's modules are called` recommends share `sh` and
    // `sh.sysl`, and neither declares either — a directory holding no source is not a module.
    // Binding the top-level directory made every namespaced package claim `sh`, so no project could
    // depend on two of them.
    "two packages namespaced under one prefix are not a collision" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/sysl-lang/sqlite3", Version(0, 2, 0), "sh/sysl/sqlite",
        name = "sqlite3")
      publishedModule(cache, "github.com/sysl-lang/table", Version(0, 1, 2), "sh/sysl/table",
        name = "table")

      val root = project(manifest("app", "0.1.0",
        s"${dep("sqlite3", "github.com/sysl-lang/sqlite3", "0.2.0")}, " +
          s"${dep("table", "github.com/sysl-lang/table", "0.1.2")}"))

      resolve(root, cache).packages.head.imports shouldBe Map(
        "sh.sysl.sqlite" -> "github.com.sysl-lang.sqlite3.sh.sysl.sqlite",
        "sh.sysl.table" -> "github.com.sysl-lang.table.sh.sysl.table",
      )
    }

    // Nesting is refused as well as equality, because a written path that two entries could answer
    // is the silent winner this rule exists to forbid — picking the longer would be a rule nobody
    // wrote down.
    "but a package offering a path INSIDE another's is" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/outer", Version(1, 0, 0), "sh/sysl", name = "outer")
      publishedModule(cache, "github.com/e/inner", Version(1, 0, 0), "sh/sysl/table", name = "inner")

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/outer", "1.0.0")}, ${dep("b", "github.com/e/inner", "1.0.0")}"))

      resolveRefused(root, cache) should include("claim the same module")
    }

    // A project namespacing itself the same way is held to the same rule, and at the same depth:
    // its own `sh/sysl/table` collides, and an unrelated `sh/sysl/other` does not.
    "a project's own namespaced module collides only at the path it actually declares" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/sysl-lang/table", Version(0, 1, 2), "sh/sysl/table",
        name = "table")

      val clash = project(manifest("app", "0.1.0",
        dep("table", "github.com/sysl-lang/table", "0.1.2")), "sh/sysl/table")

      resolveRefused(clash, cache) should include("is both a module of this project")

      val fine = project(manifest("app", "0.1.0",
        dep("table", "github.com/sysl-lang/table", "0.1.2")), "sh/sysl/other")

      resolve(fine, cache).packages.head.imports shouldBe
        Map("sh.sysl.table" -> "github.com.sysl-lang.table.sh.sysl.table")
    }
  }

  "a path dependency" - {

    "is read and named like any other" in {
      val cache = emptyCache()
      val other = project(manifest("helper", "0.1.0"), "helper")
      val root  = project(s"""package { name = "app", version = "0.1.0" }
                             |dependencies { h { path = "$other" } }
                             |""".stripMargin)

      val graph = resolve(root, cache)

      graph.packages.head.imports shouldBe Map("helper" -> "h.helper")
      graph.packages.find(_.canonical == "h").map(_.root) shouldBe Some(other)
    }

    // Every other case in this file writes `path` as an interpolated, already-absolute temp
    // directory, which never exercises what a person actually types. A manifest that says
    // `path = "../helper"` means nothing until it is joined to the directory holding the manifest
    // that wrote it -- and `root` here is a directory this suite made, not wherever the JVM's own
    // working directory happens to be, so a resolution that fell back to the process's cwd (the bug
    // this pins) would find no such directory and refuse the build.
    "a relative one is read against the directory of the manifest that wrote it" in {
      val cache  = emptyCache()
      val parent = createTempDirectory("sysl-pkg-relative-")
      val helper = s"$parent/helper"

      createDirectories(helper)
      writeFile(s"$helper/${PackageConfig.FileName}", manifest("helper", "0.1.0"))
      createDirectories(s"$helper/helper")
      writeFile(s"$helper/helper/helper.sysl", "module helper\n")

      val root = s"$parent/app"

      createDirectories(root)
      writeFile(s"$root/${PackageConfig.FileName}",
        """package { name = "app", version = "0.1.0" }
          |dependencies { h { path = "../helper" } }
          |""".stripMargin)

      val graph = resolve(root, cache)

      graph.packages.head.imports shouldBe Map("helper" -> "h.helper")
      graph.packages.find(_.canonical == "h").map(_.root) shouldBe Some(helper)
    }

    // A relative path in a fetched package is relative to a checkout that exists on one machine.
    "may not itself have one, because a relative path is only meaningful where it was written" in {
      val cache  = emptyCache()
      val bottom = project(manifest("bottom", "0.1.0"), "bottom")
      val middle = project(s"""package { name = "middle", version = "0.1.0" }
                              |dependencies { b { path = "$bottom" } }
                              |""".stripMargin, "middle")
      val root   = project(s"""package { name = "app", version = "0.1.0" }
                              |dependencies { m { path = "$middle" } }
                              |""".stripMargin)

      resolveRefused(root, cache) should include("only means something in the project it was written in")
    }

    // Its git dependencies still take part in selection, so a floor it raises is a floor.
    "raises the floors its own manifest asks for" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val other = project(manifest("helper", "0.1.0", dep("buf", "github.com/e/buf", "1.4.0")), "helper")
      val root  = project(s"""package { name = "app", version = "0.1.0" }
                             |dependencies { h { path = "$other" } }
                             |""".stripMargin)

      selected(resolve(root, cache)) shouldBe Map("github.com.e.buf" -> "1.4.0")
    }
  }

  /** The claims the resolver keeps beside its answer, which is what `sysl deps` prints (`§ 5`).
   *
   * These are about the question rather than the answer, so every one of them asserts something the
   * selection itself has thrown away: a floor that lost, who a demand belonged to, and the fact that
   * the root is an asker like any other rather than a special case above the graph.
   */
  "every claim is recorded, whether or not it won" - {

    "the floor that lost is kept, which selection alone cannot say" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "buf")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))
      publishedModule(cache, "github.com/e/b", Version(1, 0, 0), "b",
        deps = dep("buf", "github.com/e/buf", "1.4.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("b", "github.com/e/b", "1.0.0")}"))

      claimed(resolve(root, cache))("github.com/e/buf") shouldBe
        List("github.com/e/a" -> "1.2.0", "github.com/e/b" -> "1.4.0")
    }

    // The manifest of the version that was passed over is dropped by `materialize`, so nothing in the
    // finished graph could answer this by being scanned — which is the whole argument for carrying it.
    "so a losing claim survives its own manifest leaving the graph" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 2, 0), "early")
      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "later")
      publishedModule(cache, "github.com/e/a", Version(1, 0, 0), "a",
        deps = dep("buf", "github.com/e/buf", "1.2.0"))

      val root = project(manifest("app", "0.1.0",
        s"${dep("a", "github.com/e/a", "1.0.0")}, ${dep("buf", "github.com/e/buf", "1.4.0")}"))

      val graph = resolve(root, cache)

      graph.packages.exists(_.version.contains(Version(1, 2, 0))) shouldBe false
      claimed(graph)("github.com/e/buf") should contain("github.com/e/a" -> "1.2.0")
    }

    "the root asks under the name its manifest gives it" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val root = project(manifest("app", "0.1.0", dep("buf", "github.com/e/buf", "1.4.0")))

      claimed(resolve(root, cache)) shouldBe Map("github.com/e/buf" -> List("app" -> "1.4.0"))
    }

    // A manifest need not name its package, and a claim attributed to the empty string would read as
    // though nobody had asked for it.
    "and under a stand-in where it names none" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val root = project(s"""dependencies { ${dep("buf", "github.com/e/buf", "1.4.0")} }\n""")

      claimed(resolve(root, cache))("github.com/e/buf").map(_._1) shouldBe List("this project")
    }

    // A path dependency has no coordinate, so it is attributed by the label its consumer wrote — the
    // same answer `Dependency.canonical` gives, and the only name it has.
    "a path dependency's demands are attributed to its label" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val other = project(manifest("helper", "0.1.0", dep("buf", "github.com/e/buf", "1.4.0")), "helper")
      val root  = project(s"""package { name = "app", version = "0.1.0" }
                             |dependencies { h { path = "$other" } }
                             |""".stripMargin)

      claimed(resolve(root, cache)) shouldBe Map("github.com/e/buf" -> List("h" -> "1.4.0"))
    }

    // It is keyed as `selected` is, so the two join on one key and no round trip through the dotted
    // form is needed — which would not survive the trip anyway, a coordinate being full of dots.
    "and the key is the coordinate as a manifest writes it" in {
      val cache = emptyCache()

      publishedModule(cache, "github.com/e/buf", Version(1, 4, 0), "buf")

      val root = project(manifest("app", "0.1.0", dep("buf", "github.com/e/buf", "1.4.0")))

      resolve(root, cache).claims.keys should contain only "github.com/e/buf"
    }

    "a project with no dependencies claims nothing" in {
      resolve(project(manifest("app", "0.1.0")), emptyCache()).claims shouldBe empty
    }
  }

  "a project with no dependencies resolves to itself" in {
    val root  = project(manifest("app", "0.1.0"))
    val graph = resolve(root, emptyCache())

    graph.packages.map(_.canonical) shouldBe List("")
    graph.packages.head.isRoot shouldBe true
    graph.sumsChanged shouldBe false
  }

  /** `package.sysl` at a **dependency**, which is the case the field exists for: a package using
   * something the language grew otherwise fails somewhere inside itself, with a diagnostic pointing
   * at a line in a tree the consumer did not write.
   */
  "a dependency states the oldest compiler it builds with" - {

    "and one too new for the compiler in hand is refused by name" in {
      val cache = emptyCache()

      published(cache, "github.com/e/future", Version(1, 0, 0),
        """package { name = "future", version = "1.0.0", sysl = "99.0.0" }
          |""".stripMargin,
        "future/future.sysl" -> "module future\n")

      val root = project(manifest("app", "0.1.0", dep("f", "github.com/e/future", "1.0.0")))
      val e    = resolveRefused(root, cache)

      e should include("package future v1.0.0 cannot be built because it requires sysl 99.0.0")
      e should include("while the compiler in hand is")
    }

    "and one this compiler satisfies is nothing at all" in {
      val cache = emptyCache()

      published(cache, "github.com/e/past", Version(1, 0, 0),
        """package { name = "past", version = "1.0.0", sysl = "0.0.1" }
          |""".stripMargin,
        "past/past.sysl" -> "module past\n")

      val root = project(manifest("app", "0.1.0", dep("p", "github.com/e/past", "1.0.0")))

      selected(resolve(root, cache)) shouldBe Map("github.com.e.past" -> "1.0.0")
    }
  }

  // The one path that reaches `git`, pointed at a host that cannot exist: `.invalid` is reserved by
  // RFC 2606 precisely so that it never resolves, which makes this a refusal rather than a network
  // round trip — a unit suite should not be able to tell whether the machine is online.
  "a package the cache has not got, and nothing can fetch" in {
    val cache = emptyCache()
    val root  = project(manifest("app", "0.1.0", dep("j", "sysl.invalid/e/nothing-here", "1.0.0")))

    resolveRefused(root, cache) should include("cannot fetch sysl.invalid/e/nothing-here")
  }
}
