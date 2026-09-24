package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Linking a `pkg_config` library from its static archive — the root manifest's `link` key and
 * `--link` (`reference/packages.md § Linking a library statically`, `LinkMode`, `StaticLink`).
 *
 * The rule itself is asserted as a pure function over a made-up `pkg-config` answer and a made-up
 * filesystem, so every case — an archive found, a named one missing, a private one missing, a
 * system library — runs on any machine. The end-to-end cases need libuv installed **with** its
 * archive, which Homebrew's is, and are skipped where it is not.
 */
class StaticLinkTests extends AnyFreeSpec with Matchers {

  private def manifest(says: String): String =
    s"""package { name = "thing", version = "0.1.0" }
       |$says
       |""".stripMargin

  private def read(says: String): PackageConfig =
    PackageConfig.read(manifest(says)).fold(e => fail(e), identity)

  private def refused(says: String): String =
    PackageConfig.read(manifest(says)).fold(identity, _ => fail(s"'$says' was accepted"))

  private def parse(args: String*): Option[Config] = Console.withErr(Discarded)(parseArgs(args))

  "the 'link' key" - {

    "is absent unless written" in {
      read("").link shouldBe None
    }

    "takes 'static', 'dynamic' and a list of pkg_config names" in {
      read("""link = "static"""").link shouldBe Some(LinkMode.Static)
      read("""link = "dynamic"""").link shouldBe Some(LinkMode.Dynamic)
      read("""link = ["openssl", "libuv"]""").link shouldBe Some(LinkMode.Only(List("openssl", "libuv")))
    }

    "reads an empty list as dynamic, since it names nothing to link statically" in {
      read("link = []").link shouldBe Some(LinkMode.Dynamic)
    }

    "is a key this compiler knows, so it draws no unknown-key warning" in {
      read("""link = "static"""").warnings shouldBe empty
    }

    // One library written as a bare string is the likeliest mistake, so the refusal writes the list.
    "refuses a string that is neither mode, and says how to name one library" in {
      val e = refused("""link = "libuv"""")

      e should include("""'link = "libuv"' is neither "static" nor "dynamic"""")
      e should include("""'link = ["libuv"]'""")
    }

    "refuses a list holding something other than names, and a block" in {
      refused("link = [1, 2]") should include("each one a name")
      refused("""link { libuv = true }""") should include("a list of the pkg_config names")
    }
  }

  "the '--link' flag" - {

    "takes the same three forms, the list comma-separated" in {
      parse("build", "p.sysl").map(_.link) shouldBe Some(None)
      parse("build", "p.sysl", "--link", "static").map(_.link) shouldBe Some(Some(LinkMode.Static))
      parse("build", "p.sysl", "--link", "dynamic").map(_.link) shouldBe Some(Some(LinkMode.Dynamic))
      parse("build", "p.sysl", "--link", "libuv,openssl").map(_.link) shouldBe
        Some(Some(LinkMode.Only(List("libuv", "openssl"))))
    }

    "belongs to every command that links" in {
      for command <- List("build", "run", "test") do
        withClue(command) {
          parse(command, "p.sysl", "--link", "static").map(_.linkMode) shouldBe Some(LinkMode.Static)
        }
    }

    "refuses an empty name where it was typed" in {
      parse("build", "p.sysl", "--link", "libuv,") shouldBe None
      LinkMode.parse("libuv,").left.toOption.get should include("names an empty library")
    }

    // `orElse`, as `--lto` and `-O`: the flag typed for one invocation beats the key written for all.
    "beats the root manifest's key, and the key beats the default" in {
      Config(link = Some(LinkMode.Dynamic)).withLink(Some(LinkMode.Static)).linkMode shouldBe LinkMode.Dynamic
      Config().withLink(Some(LinkMode.Static)).linkMode shouldBe LinkMode.Static
      Config().withLink(None).linkMode shouldBe LinkMode.Dynamic
    }
  }

  "which -l becomes which archive" - {

    val answer = StaticAnswer(
      direct = List("-L/p/lib", "-luv", "-lpthread", "-lm"),
      static = List("-L/p/lib", "-luv", "-lpthread", "-lm", "-lz", "-lextra"),
      libdir = Some("/p/lib"))

    "a library with an archive in a directory pkg-config named becomes that archive's path" in {
      StaticLink.archives("libuv", answer, Nil, Set("/p/lib/libuv.a")) shouldBe
        Right(Map("uv" -> "/p/lib/libuv.a"))
    }

    "and a --link-path directory is searched after pkg-config's" in {
      StaticLink.archives("libuv", answer, List("/mine"), Set("/mine/libuv.a", "/mine/libz.a")) shouldBe
        Right(Map("uv" -> "/mine/libuv.a", "z" -> "/mine/libz.a"))
    }

    "the libdir is searched where pkg-config printed no -L" in {
      val bare = StaticAnswer(List("-luv"), List("-luv"), Some("/usr/lib/x"))

      StaticLink.archives("libuv", bare, Nil, Set("/usr/lib/x/libuv.a")) shouldBe
        Right(Map("uv" -> "/usr/lib/x/libuv.a"))
    }

    // Only `--static` named these, so the reader did not: they stay an `-l`.
    "a private library with no archive is left an -l" in {
      val found = StaticLink.archives("libuv", answer, Nil, Set("/p/lib/libuv.a")).toOption.get

      found.keySet should not contain "z"
      StaticLink.rewrite(List("-luv", "-lz"), found) shouldBe List("/p/lib/libuv.a", "-lz")
    }

    "the platform's own libraries are never replaced, even where an archive of one exists" in {
      val found = StaticLink.archives("libuv", answer, Nil,
        Set("/p/lib/libuv.a", "/p/lib/libm.a", "/p/lib/libpthread.a")).toOption.get

      found shouldBe Map("uv" -> "/p/lib/libuv.a")
    }

    "a library the program links directly with no archive is refused, naming the file and the directories" in {
      StaticLink.archives("libuv", answer, List("/mine"), Set.empty) shouldBe
        Left("'libuv' is to be linked statically, and there is no 'libuv.a' to link it from — looked " +
          "in /p/lib, /mine. Install its static archive there, or leave 'libuv' out of 'link' to link " +
          "it dynamically")
    }

    // `link = "static"`: nobody named this library, so it is linked dynamically and traced.
    "under 'static', a library with no archive is linked dynamically rather than refused" in {
      val found = StaticLink.archives("sqlite3", answer, List("/mine"), Set.empty, required = false)

      found shouldBe Right(Map.empty)
      StaticLink.kept(answer, found.toOption.get) shouldBe List("uv")
      StaticLink.keptNote("sqlite3", "uv", List("/p/lib", "/mine")) shouldBe
        "static: 'sqlite3' has no 'libuv.a', so -luv is linked dynamically — looked in /p/lib, /mine"
    }

    // A private archive alone would link the library's .dylib beside a copy of what it links.
    "and one with no archive for any direct library takes none of its private ones either" in {
      StaticLink.archives("libuv", answer, Nil, Set("/p/lib/libz.a"), required = false) shouldBe
        Right(Map.empty)
    }

    "while the direct libraries that do have one are still taken from it" in {
      val two   = StaticAnswer(List("-L/p", "-lssl", "-lcrypto"), List("-L/p", "-lssl", "-lcrypto"), None)
      val found = StaticLink.archives("libssl", two, Nil, Set("/p/libcrypto.a"), required = false).toOption.get

      found shouldBe Map("crypto" -> "/p/libcrypto.a")
      StaticLink.kept(two, found) shouldBe List("ssl")
    }

    "but a library the list NAMES with no archive is refused exactly as before" in {
      StaticLink.archives("sqlite3", answer, Nil, Set.empty, required = true).left.toOption.get should
        startWith("'sqlite3' is to be linked statically, and there is no 'libuv.a' to link it from")
    }

    "the rewrite touches only the -l names it has an archive for" in {
      StaticLink.rewrite(List("-L/p", "-luv", "-framework", "Cocoa", "-lSystem"), Map("uv" -> "/p/libuv.a")) shouldBe
        List("-L/p", "/p/libuv.a", "-framework", "Cocoa", "-lSystem")
    }

    // `@link("uv")` and pkg-config's `-luv` name one library; the later place is after everything.
    "and an archive named twice is kept at its last place only" in {
      StaticLink.rewrite(List("-luv", "-lm", "-L/p", "-luv", "-lpthread"), Map("uv" -> "/p/libuv.a")) shouldBe
        List("-lm", "-L/p", "/p/libuv.a", "-lpthread")
    }

    "and the link line carries the archive where the -l was" in {
      val cmd = Toolchain.linkCommand("p.ll", Nil, "out", Target.default, links = List("uv"),
        paths = SearchPaths(probedLibs = List("-L/p", "-luv"), archives = Map("uv" -> "/p/libuv.a")))

      cmd should contain("/p/libuv.a")
      cmd should not contain "-luv"
    }
  }

  "a name the build does not declare" - {

    "is refused, naming the ones it does" in {
      StaticLink.unknown(LinkMode.Only(List("libvu")), Set("libuv", "openssl"), Set.empty) shouldBe
        Some("'link' names 'libvu', and no pkg_config requirement in this build is called that — the " +
          "names are the ones the manifests write under 'requires.pkg_config', which here are libuv, openssl")
    }

    "and one answered by --include-path is refused, since pkg-config was never asked" in {
      StaticLink.unknown(LinkMode.Only(List("libuv")), Set("libuv"), Set("libuv")).get should
        include("'--include-path libuv=<dir>' answered")
    }

    "while 'static' and a declared name are not" in {
      StaticLink.unknown(LinkMode.Static, Set.empty, Set.empty) shouldBe None
      StaticLink.unknown(LinkMode.Only(List("libuv")), Set("libuv"), Set.empty) shouldBe None
    }

    // `could` is what the build with every feature on requires.
    "a name only a feature this build leaves off requires is left out, not refused" in {
      val mode = LinkMode.Only(List("libuv", "lmdb"))

      StaticLink.unmatched(mode, Set("libuv")) shouldBe List("lmdb")
      StaticLink.unknown(mode, Set("libuv"), Set.empty, could = Set("libuv", "lmdb")) shouldBe None
      StaticLink.gated(mode, Set("libuv"), Set("libuv", "lmdb")) shouldBe List("lmdb")
    }

    "and a misspelt one is still refused, naming the gated ones among the rest" in {
      StaticLink.unknown(LinkMode.Only(List("lmbd")), Set("libuv", "sqlite3"), Set.empty,
          could = Set("libuv", "lmdb", "sqlite3")) shouldBe
        Some("'link' names 'lmbd', and no pkg_config requirement in this build is called that — the " +
          "names are the ones the manifests write under 'requires.pkg_config', which here are libuv, " +
          "lmdb, sqlite3")
    }
  }

  "a real build against libuv" - {

    /** libuv's archive, where this machine has libuv and an archive beside it. */
    lazy val archive: Option[String] =
      PkgConfig.queryStatic("libuv").toOption.flatMap { a =>
        StaticLink.searched(a, Nil).map(d => s"$d/libuv.a").find(isFile)
      }

    /** What the binary loads at run time, as the platform's own tool says. */
    def loads(exe: String): String =
      val otool = exec(Seq("otool", "-L", exe))

      if otool.exitCode == 0 then otool.stdout else exec(Seq("ldd", exe)).stdout

    def project(link: String): String = {
      val root = createTempDirectory("sysl-static-link-")

      writeFile(s"$root/${PackageConfig.FileName}",
        s"""package { name = "app", version = "0.1.0" }
           |requires { os = true, heap = true, pkg_config { libuv = "libuv — brew install libuv" } }
           |$link
           |""".stripMargin)
      writeFile(s"$root/main.sysl",
        """@link("uv")
          |
          |extern uv_version() -> u32
          |
          |print(uv_version() > 0)
          |""".stripMargin)
      root
    }

    def built(root: String, cfg: Config => Config = identity): (String, String) = {
      val exe    = s"$root/out"
      val notes  = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(Console.withErr(notes)(
        sh.sysl.execute(cfg(Config(command = "build", file = root, output = Some(exe), verbose = true)))))

      if status != 0 then fail(s"the driver exited with $status:\n${notes.toString}")

      (exe, notes.toString)
    }

    def refusal(root: String): String = {
      val notes  = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(Console.withErr(notes)(
        sh.sysl.execute(Config(command = "build", file = root, output = Some(s"$root/out")))))

      if status == 0 then fail("expected a refusal, got a build")

      notes.toString
    }

    // The dynamic build is here so that the static one's assertion is one that could have failed.
    "links libuv's dylib when nothing asks otherwise" in {
      assume(Toolchain.clangAvailable && archive.isDefined)

      val (exe, _) = built(project(""))

      loads(exe) should include("libuv")
    }

    "and its archive when 'link' names it, so the binary no longer loads libuv" in {
      assume(Toolchain.clangAvailable && archive.isDefined)

      val (exe, notes) = built(project("""link = ["libuv"]"""))

      loads(exe) should not include "libuv"
      notes should include(s"static: -luv is ${archive.get}")
      exec(Seq(exe)).stdout shouldBe "true\n"
    }

    "and when 'link' says static" in {
      assume(Toolchain.clangAvailable && archive.isDefined)

      loads(built(project("""link = "static""""))._1) should not include "libuv"
    }

    "and '--link dynamic' overrules the key" in {
      assume(Toolchain.clangAvailable && archive.isDefined)

      val (exe, _) = built(project("""link = "static""""), _.copy(link = Some(LinkMode.Dynamic)))

      loads(exe) should include("libuv")
    }

    // A package cannot decide how the application links, for `optimization`'s reason.
    "a dependency's own 'link' key is not consulted" in {
      assume(Toolchain.clangAvailable && archive.isDefined)

      val dep = createTempDirectory("sysl-static-link-dep-")

      writeFile(s"$dep/${PackageConfig.FileName}",
        """package { name = "dep", version = "1.0.0" }
          |requires { os = true, pkg_config { libuv = "libuv — brew install libuv" } }
          |link = "static"
          |""".stripMargin)
      createDirectories(s"$dep/dep")
      writeFile(s"$dep/dep/dep.sysl",
        "module dep\n@link(\"uv\")\n\nextern \"uv_version\" version() -> u32\n")

      val root = createTempDirectory("sysl-static-link-app-")

      writeFile(s"$root/${PackageConfig.FileName}",
        s"""package { name = "app", version = "0.1.0" }
           |requires { os = true, heap = true }
           |dependencies { d { path = "$dep" } }
           |""".stripMargin)
      writeFile(s"$root/main.sysl", "print(dep.version() > 0)\n")

      val (exe, notes) = built(root)

      loads(exe) should include("libuv")
      notes should not include "static: "
    }

    "a name the build does not declare stops it" in {
      assume(Toolchain.clangAvailable && archive.isDefined)

      refusal(project("""link = ["libvu"]""")) should include("'link' names 'libvu'")
    }
  }

  // slate's shape: the library is a dependency's, and the dependency is behind a feature.
  "a name behind a feature this build leaves off" - {

    def gated(link: String): String = {
      val dep = createTempDirectory("sysl-static-link-gated-dep-")

      writeFile(s"$dep/${PackageConfig.FileName}",
        """package { name = "dep", version = "1.0.0" }
          |requires { os = true, pkg_config { libuv = "libuv — brew install libuv" } }
          |""".stripMargin)
      createDirectories(s"$dep/dep")
      writeFile(s"$dep/dep/dep.sysl",
        "module dep\n@link(\"uv\")\n\nextern \"uv_version\" version() -> u32\n")

      val root = createTempDirectory("sysl-static-link-gated-")

      writeFile(s"$root/${PackageConfig.FileName}",
        s"""package { name = "app", version = "0.1.0" }
           |requires { os = true, heap = true }
           |dependencies { d { path = "$dep", optional = true } }
           |features { default = [uv], uv = [d] }
           |$link
           |""".stripMargin)
      writeFile(s"$root/main.sysl", "print(true)\n")
      root
    }

    def run(root: String): (Int, String) = {
      val notes  = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(Console.withErr(notes)(
        sh.sysl.execute(Config(command = "build", file = root, output = Some(s"$root/out"),
          verbose = true, noDefaultFeatures = true))))

      (status, notes.toString)
    }

    "is left out with a trace, and the build links" in {
      assume(Toolchain.clangAvailable)

      val (status, notes) = run(gated("""link = ["libuv"]"""))

      withClue(notes) { status shouldBe 0 }
      notes should include("link: 'libuv' is required only under a feature this build leaves off")
    }

    "while a misspelt one is still refused, naming the gated name among the rest" in {
      val (status, notes) = run(gated("""link = ["libvu"]"""))

      status should not be 0
      notes should include("'link' names 'libvu', and no pkg_config requirement in this build is " +
        "called that — the names are the ones the manifests write under 'requires.pkg_config', " +
        "which here are libuv")
    }
  }
}
