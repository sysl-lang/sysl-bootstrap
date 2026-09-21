package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The project config (`reference/packages.md § What a project is called`): what `package.hocon`
 * says, and what a file that says it wrongly is told.
 *
 * Reading is a pure function of the file's text, so every case here runs identically on all three
 * platforms — which is what anything below the driver is held to, and the
 * reason the parse is separated from finding the file at all.
 */
class PackageConfigTests extends AnyFreeSpec with Matchers {

  private def read(text: String): PackageConfig =
    PackageConfig.read(text) match
      case Right(c) => c
      case Left(e)  => fail(s"expected a config, got: $e")

  private def refused(text: String): String =
    PackageConfig.read(text) match
      case Left(e)  => e
      case Right(c) => fail(s"expected a refusal, got: $c")

  /** **An unknown key is said out loud and then ignored** (`PackageConfig.unknownKeys`, card
   * `0194 d`).
   *
   * Whatever 0.1.0's parser does with a key it does not know is the compatibility floor every later
   * manifest has to clear, so this is the tag-time decision: refuse, and no key can ever be added
   * without breaking every released compiler; ignore silently, which is what this replaces, and a
   * mistyped block does nothing and says nothing.
   */
  "an unknown key" - {

    "at the top level is reported and ignored" in {
      val c = read("""profiles { release { } }""")

      c.warnings.length shouldBe 1
      c.warnings.head should include("'profiles'")
      c.warnings.head should include(PackageConfig.FileName)
      c.warnings.head should include("ignored")
    }

    // The point of warning rather than refusing: the rest of the file still takes effect, so a
    // project built by a compiler older than one of its keys still builds.
    "does not stop the keys beside it from being read" in {
      val c = read(
        """package { name = "thing", version = "1.0.0" }
          |profiles { release { } }
          |""".stripMargin)

      c.name shouldBe Some("thing")
      c.version shouldBe Some("1.0.0")
      c.warnings.length shouldBe 1
    }

    "inside 'package' is reported the same way, under its own path" in {
      val c = read("""package { name = "thing", licence = "MIT" }""")

      c.warnings.length shouldBe 1
      c.warnings.head should include("'package.licence'")
    }

    // One per key, so a file with two mistakes is told about both rather than about whichever came
    // first — which is the behaviour a refusal cannot have.
    "and each one is reported, not merely the first" in {
      read("""alpha { }
             |beta { }
             |""".stripMargin).warnings.length shouldBe 2
    }

    // The negative control: everything the format actually has must stay silent, or the warning is
    // noise and gets ignored the way noise is.
    "while a file using every block this compiler knows says nothing" in {
      read(
        """package { name = "thing", version = "1.0.0", sysl = "0.0.80" }
          |targets { default = "aarch64-macos" }
          |capabilities { heap = true }
          |requires { heap = true }
          |dependencies { }
          |defines { }
          |""".stripMargin).warnings shouldBe Nil
    }

    // **The two argued refusals are untouched.** They are about a closed vocabulary rather than a
    // growing one: somebody who wrote `versoin` believes they have pinned a version, and resolving
    // the default branch while they believe it is worse than stopping.
    "but a misspelling inside 'dependencies' is still refused, not warned about" in {
      refused("""dependencies { ogol { git = "github.com/sysl-lang/ogol", versoin = "0.1.0" } }""") should
        include("versoin")
    }
  }

  "a project may have no file at all, and every capability is then provided" in {
    PackageConfig.empty.provides("aarch64-macos") shouldBe Set("heap", "os", "posix")
  }

  "an empty file is a project that said nothing" in {
    read("") shouldBe PackageConfig.empty
  }

  "identity" - {

    "a name and a version" in {
      val c = read(
        """package {
          |  name    = "geom"
          |  version = "1.4.2"
          |}
          |""".stripMargin)

      c.name shouldBe Some("geom")
      c.version shouldBe Some("1.4.2")
    }

    "and neither is required" in {
      read("targets { default = \"x86_64-linux\" }").name shouldBe None
    }

    /** The name is what a directory project's output is called, so it reaches the filesystem. Each
     * of these is refused rather than sanitized: a file that names its output was written by
     * somebody expecting an answer, and both silent repairs — falling back to the directory name, or
     * flattening the separator — write a different executable and say nothing.
     */
    "a name that is a path rather than a name is refused" in {
      refused("package { name = \"build/tool\" }") should include("names a path rather than a name")
      refused("package { name = \"..\\\\tool\" }") should include("names a path rather than a name")
    }

    // Both are legal path segments and neither names a file, so the link would fail at the far end
    // with a message about a directory instead of about this line.
    "and so are the two segments that name a directory" in {
      refused("package { name = \".\" }") should include("names a path rather than a name")
      refused("package { name = \"..\" }") should include("names a path rather than a name")
    }

    "an empty name is refused as its own mistake" in {
      refused("package { name = \"\" }") should include("is empty")
    }

    // A name with spaces or dots in it is awkward rather than wrong, and the shell can quote it.
    // Refusing it would be this check deciding what a project may call itself, which is not its job.
    "while an unusual but usable name is left alone" in {
      read("package { name = \"my tool\" }").name shouldBe Some("my tool")
      read("package { name = \"tool.v2\" }").name shouldBe Some("tool.v2")
    }
  }

  /** `package.sysl` — the oldest compiler this package is known to build with.
   *
   * The whole of what it buys is one sentence at the top of a build instead of a type error inside
   * somebody else's package. So the tests are the field's own spelling, and the sentence.
   */
  "the compiler floor" - {

    "a package may state one" in {
      read("package { name = \"sdl3\"\n version = \"0.2.6\"\n sysl = \"0.0.62\" }").sysl shouldBe
        Some(Version(0, 0, 62))
    }

    "and saying nothing is the ordinary case" in {
      read("package { name = \"sdl3\" }").sysl shouldBe None
    }

    // It is a version like every other version here, so `Version.parse` is what reads it — which
    // refuses a range, a pre-release and a two-part number for free. The message names the field,
    // since a string in a file has no shape a reader can see from the line around it.
    "something that is not a version is refused, and the field is named" in {
      val e = refused("package { sysl = \">= 0.0.62\" }")

      e should include("'package.sysl'")
      e should include("is not a version")
    }

    "an interim spelling is refused too — a package states a release" in {
      refused("package { sysl = \"0.0.66-fcf4e33a\" }") should include("is not a version")
    }

    "an older compiler is refused, naming the package, the floor and what is in hand" in {
      val c = read("package { sysl = \"0.0.62\" }")

      c.checkFloor("github.com/sysl-lang/sdl3 v0.2.6", Version(0, 0, 61)) shouldBe
        Left("github.com/sysl-lang/sdl3 v0.2.6 cannot be built because it requires sysl 0.0.62 or " +
          "newer, while the compiler in hand is 0.0.61")
    }

    "the floor itself is old enough, and so is anything above it" in {
      val c = read("package { sysl = \"0.0.62\" }")

      c.checkFloor("this project", Version(0, 0, 62)) shouldBe Right(())
      c.checkFloor("this project", Version(0, 1, 0)) shouldBe Right(())
      c.checkFloor("this project", Version(1, 0, 0)) shouldBe Right(())
    }

    "a package that states nothing is never refused" in {
      PackageConfig.empty.checkFloor("this project", Version(0, 0, 1)) shouldBe Right(())
    }

    // An interim is stamped `<next patch>-<sha>`, which `Version.parse` refuses and which a
    // comparison still has to answer. It satisfies whatever its numbers reach, which is Cargo's
    // ruling for a nightly toolchain against `rust-version`.
    "an interim compiler is read as the numbers it carries" in {
      Version.ofCompiler("0.0.66-fcf4e33a") shouldBe Some(Version(0, 0, 66))
      Version.ofCompiler("0.0.65") shouldBe Some(Version(0, 0, 65))

      read("package { sysl = \"0.0.65\" }")
        .checkFloor("this project", Version.ofCompiler("0.0.66-fcf4e33a").get) shouldBe Right(())
    }

    // Nothing here may be the thing that stops a build: a version the compiler cannot read is its
    // own, and making no claim is the only honest answer to that.
    "a compiler version that will not read makes no claim" in {
      Version.ofCompiler("dirty") shouldBe None
    }
  }

  "targets" - {

    "'default' names one rather than being one" in {
      val c = read(
        """targets {
          |  default = "x86_64-linux"
          |}
          |""".stripMargin)

      c.defaultTarget shouldBe Some("x86_64-linux")
      c.targets shouldBe empty
    }

    "a block may declare a machine the registry does not have" in {
      val c = read(
        """targets {
          |  aarch64-kernel {
          |    triple = "aarch64-none-elf"
          |  }
          |}
          |""".stripMargin)

      c.targets("aarch64-kernel").triple shouldBe Some("aarch64-none-elf")
    }

    "a capability set is read, and what it does not mention stays provided" in {
      val c = read(
        """targets {
          |  aarch64-kernel {
          |    triple = "aarch64-none-elf"
          |    capabilities { os = false, posix = false }
          |  }
          |}
          |""".stripMargin)

      c.provides("aarch64-kernel") shouldBe Set("heap")
    }

    "a target the file says nothing about provides everything" in {
      val c = read(
        """targets {
          |  aarch64-kernel { capabilities { heap = false } }
          |}
          |""".stripMargin)

      c.provides("x86_64-linux") shouldBe Set("heap", "os", "posix")
      c.provides("aarch64-kernel") shouldBe Set("os", "posix")
    }

    // Whether a heap exists is a project engineering decision, so it is stated once for the project
    // rather than once per machine it is built for. Keyed only by target it could not be said at all
    // for a target the registry already has, since a block would then be a machine being redefined.
    "a project states its own policy, for every target it builds for" in {
      val c = read("capabilities { heap = false }\n")

      c.provides("aarch64-macos") shouldBe Set("os", "posix")
      c.provides("thumbv7em-freestanding") shouldBe Set("os", "posix")
    }

    "and a target block layers over it, for the one machine where it is not so" in {
      val c = read(
        """capabilities { heap = false }
          |targets {
          |  aarch64-macos { capabilities { heap = true } }
          |}
          |""".stripMargin)

      c.provides("aarch64-macos") should contain("heap")
      c.provides("thumbv7em-freestanding") shouldNot contain("heap")
    }

    // The layering is per capability rather than per block: a target block saying one thing does not
    // discard what the project said about the others.
    "the layering is per capability, not per block" in {
      val c = read(
        """capabilities { heap = false, posix = false }
          |targets {
          |  kernel { capabilities { posix = true } }
          |}
          |""".stripMargin)

      c.provides("kernel") shouldBe Set("os", "posix")
    }

    // `alloc` is what a *module* promises about its conduct and `heap` is the facility, so the config
    // wants `heap`. The old word is accepted and mapped, transitionally, because **a tag is
    // immutable**: every package in the org is fetched at a pinned version whose `package.hocon` says
    // `requires { alloc = true }` and always will, and a fetched dependency's file is validated
    // exactly as the project's own is. Refusing it would stop every pinned dependency resolving.
    "the module's own word is accepted in the config and mapped, so a pinned dependency still reads" in {
      read("capabilities { alloc = false }\n").provides("aarch64-macos") shouldNot contain("heap")
      read("requires { alloc = true }\n").requires should contain("heap")
    }

    "while a word that is neither is still refused" in {
      refused("capabilities { treads = false }\n") should include("is not a capability")
    }

    // It parsed cleanly and was then dropped by `collect { case (name, true) => name }`, so the file
    // read as though the project had said something. It is the spelling reached for first.
    "'requires' with a false says nothing, and is refused rather than discarded" in {
      val e = refused("requires { heap = false }\n")

      e should include("says nothing")
      e should include("capabilities { heap = false }")
      e should include("@no_alloc")
    }

    "a capability that is not one is refused, rather than quietly doing nothing" in {
      val e = refused(
        """targets {
          |  kernel { capabilities { treads = false } }
          |}
          |""".stripMargin)

      e should include("'treads'")
      e should include("is not a capability")
      e should include("'heap', 'os', 'posix'")
    }

    "a capability that is not true or false" in {
      refused(
        """targets {
          |  kernel { capabilities { heap = "no" } }
          |}
          |""".stripMargin) should include("must be true or false")
    }

    "a target that is a name where a block was expected" in {
      refused(
        """targets {
          |  kernel = "aarch64-none-elf"
          |}
          |""".stripMargin) should include("is not a block describing a target")
    }
  }

  "a package's own requires block" - {

    "names what it needs of whatever builds it" in {
      read("requires { os = true }").requires shouldBe Set("os")
    }

    "and folds in what that implies — posix needs an operating system under it" in {
      read("requires { posix = true }").requires shouldBe Set("os", "posix")
    }

    // This used to assert that a `false` here asks for nothing, which was true and was the defect:
    // it parsed, it was dropped, and the file then read as though the project had said something. A
    // package cannot need a facility *not* to exist, so there is nothing for the entry to mean.
    "a false entry is refused, since 'requires' cannot ask for an absence" in {
      refused("requires { os = false }") should include("says nothing")
    }

    "a name that is not a capability" in {
      refused("requires { sockets = true }") should include("is not a capability")
    }
  }

  /** The `headers` sub-block: which C headers this package includes and does not carry, each under
   * a name a consumer answers with `--include-path <name>=<dir>` (`reference/packages.md §
   * Capabilities`).
   *
   * It sits under `requires` because that is already *what this package needs of its environment*,
   * and the only thing separating it from the capabilities beside it is that satisfying it is a
   * path — which `reference/ffi.md § @link` settles is the driver's and may never be written in
   * this file.
   */
  "the headers a package requires" - {

    "are read as a name and the reason the consumer is given" in {
      read(
        """requires {
          |  headers { lwip = "lwIP's headers, from the pico-sdk" }
          |}
          |""".stripMargin).headers shouldBe
        Map("lwip" -> HeaderReq("lwIP's headers, from the pico-sdk", None))
    }

    // The two kinds of requirement share the block, so each has to survive the other being there.
    // Read as a capability, `headers` would be refused as a name that is not one.
    "sit beside the capabilities without disturbing them" in {
      val c = read(
        """requires {
          |  os = true
          |  headers { lwip = "lwIP's headers" }
          |}
          |""".stripMargin)

      c.requires shouldBe Set("os")
      c.headers.keySet shouldBe Set("lwip")
    }

    "and a file that declares none needs none" in {
      read("requires { os = true }").headers shouldBe empty
    }

    // The name is written on a command line as `--include-path <name>=<dir>`, so one the flag could
    // not carry is refused where the package chose it rather than where a consumer types it.
    "a name a command line could not carry is refused" in {
      val e = refused("""requires { headers { "lw/ip" = "why" } }""")

      e should include("--include-path <name>=<dir>")
    }

    // The string is quoted back at whoever has to go and find the headers, and is the only part of
    // the refusal nothing in the compiler could have written.
    "a reason that says nothing is refused, because the reason is the point" in {
      refused("""requires { headers { lwip = "" } }""") should include("says nothing about what it needs")
    }

    "and a path where the reason belongs is refused as the wrong kind of thing" in {
      refused("""requires { headers { lwip = true } }""") should include("must be a string")
    }

    /** The long form: the prose, plus the environment variable the ecosystem keeps the path in.
     *
     * **A variable's NAME is not a path**, which is the whole of why this may be in a committed
     * file when a directory may not: `PICO_SDK_PATH` is the same string on every machine, and it is
     * what the pico-sdk's own CMake reads.
     */
    "may name the environment variable the path conventionally lives in" in {
      read(
        """requires {
          |  headers {
          |    pico_sdk = { note = "the pico-sdk's headers", env = "PICO_SDK_PATH" }
          |  }
          |}
          |""".stripMargin).headers shouldBe
        Map("pico_sdk" -> HeaderReq("the pico-sdk's headers", Some("PICO_SDK_PATH")))
    }

    // The `note` is what a consumer *without* the variable set is shown, so a block that gave only a
    // variable name would leave them nothing to go and look for -- which is the one case this whole
    // requirement exists to serve.
    "but the note is still required, because it is what an unset variable falls back to" in {
      refused("""requires { headers { pico_sdk = { env = "PICO_SDK_PATH" } } }""") should
        include("needs a 'note'")
    }

    "and the env is optional, so a block may carry only a note" in {
      read("""requires { headers { lwip = { note = "lwIP's headers" } } }""").headers shouldBe
        Map("lwip" -> HeaderReq("lwIP's headers", None))
    }

    // A directory holds a separator and a variable's name does not, so the refusal can say which
    // mistake was made -- which matters because putting a *path* here is exactly the mistake the
    // manifest refuses everywhere else.
    "a path where the variable's name belongs is refused as what it is" in {
      refused("""requires { headers { pico_sdk = { note = "why", env = "/opt/pico-sdk" } } }""") should
        include("not the name of an environment variable")
    }

    "a key the block does not have is refused rather than ignored" in {
      refused("""requires { headers { pico_sdk = { note = "why", path = "/opt" } } }""") should
        include("no 'path'")
    }
  }

  /** The `pkg_config` sub-block: which installed libraries this package binds, each under the name
   * `pkg-config` files it as (`reference/packages.md § Capabilities`).
   *
   * A requirement kind of its own rather than a field on `headers`, because it answers both halves of
   * one fact — the headers to compile against and the library to link — and a consumer who has one of
   * those has not got a build.
   */
  "the pkg-config libraries a package requires" - {

    "are read as a module name and the reason the consumer is given" in {
      read(
        """requires {
          |  pkg_config { sdl3 = "SDL3 — brew install sdl3, or Debian's libsdl3-dev" }
          |}
          |""".stripMargin).pkgConfig shouldBe Map("sdl3" -> "SDL3 — brew install sdl3, or Debian's libsdl3-dev")
    }

    // Three kinds of requirement now share the block, so each has to survive the other two being
    // there. Read as a capability, `pkg_config` would be refused as a name that is not one.
    "sit beside the capabilities and the headers without disturbing either" in {
      val c = read(
        """requires {
          |  os = true
          |  headers { lwip = "lwIP's headers" }
          |  pkg_config { sdl3 = "SDL3" }
          |}
          |""".stripMargin)

      c.requires shouldBe Set("os")
      c.headers.keySet shouldBe Set("lwip")
      c.pkgConfig.keySet shouldBe Set("sdl3")
    }

    "and a file that declares none needs none" in {
      read("requires { os = true }").pkgConfig shouldBe empty
    }

    // A consumer overrides this with `--include-path <name>=<dir>`, exactly as they answer a header
    // requirement, so the name has to survive the same flag.
    "a name a command line could not carry is refused" in {
      refused("""requires { pkg_config { "sd/l3" = "why" } }""") should include("--include-path <name>=<dir>")
      refused("""requires { pkg_config { "sd=l3" = "why" } }""") should include("--include-path <name>=<dir>")
    }

    // The name is not the package author's to choose: it is what `pkg-config` answers to, and a
    // great many of the world's libraries file under one with a version in it. A rule that refused
    // a dot refused to bind them at all — `sysl-lang/yaml` had to declare a `headers` requirement
    // instead and hand every consumer two flags on the command line.
    "a '.pc' name with a version in it is taken, which is how much of the world is packaged" in {
      read("""requires { pkg_config { "yaml-0.1" = "libyaml — brew install libyaml" } }""")
        .pkgConfig.keySet shouldBe Set("yaml-0.1")
      read("""requires { pkg_config { "glib-2.0" = "GLib — brew install glib" } }""")
        .pkgConfig.keySet shouldBe Set("glib-2.0")
      read("""requires { pkg_config { "gtk+-3.0" = "GTK 3 — brew install gtk+3" } }""")
        .pkgConfig.keySet shouldBe Set("gtk+-3.0")
    }

    // The widening is the `pkg_config` block's alone. A `headers` name *is* invented by whoever
    // writes the manifest, so there is nothing it cannot be called and no reason to widen it — and
    // the two refusals say different things because they are about different authors.
    "and the same name in the headers block is still refused, where the author chose it" in {
      val why = refused("""requires { headers { "yaml-0.1" = "libyaml" } }""")

      why should include("header requirement's name")
      why should include("letters, digits, '_' and '-'")
    }

    "the refusal says what a '.pc' name may be, rather than suggesting a better one" in {
      val why = refused("""requires { pkg_config { "sd/l3" = "why" } }""")

      why should include("pkg-config module's name")
      why should include("'.' and '+'")
    }

    "a reason that says nothing is refused, because the reason is the point" in {
      refused("""requires { pkg_config { sdl3 = "" } }""") should include("says nothing about what it needs")
    }

    "and a version where the reason belongs is refused as the wrong kind of thing" in {
      refused("""requires { pkg_config { sdl3 = 3 } }""") should include("must be a string")
    }
  }

  "the dependencies block" - {

    "a coordinate and a version" in {
      val deps = read(
        """dependencies {
          |  json { git = "github.com/edadma/sysl-json", version = "1.4.0" }
          |}""".stripMargin).dependencies

      deps shouldBe List(Dependency("json", Origin.Git("github.com/edadma/sysl-json", Version(1, 4, 0))))
    }

    "a path, for a package being written beside its consumer" in {
      val deps = read("""dependencies { local { path = "../experiment" } }""").dependencies

      deps shouldBe List(Dependency("local", Origin.Local("../experiment")))
    }

    "a mount, which is what a consumer writes when two packages want one name" in {
      val deps = read(
        """dependencies {
          |  regex { git = "github.com/edadma/sysl-regex/v2", version = "2.0.4", mount = "re" }
          |}""".stripMargin).dependencies

      deps.head.mount shouldBe Some("re")
    }

    "several, in the order a reader finds them rather than the order they were written" in {
      val deps = read(
        """dependencies {
          |  zeta { path = "../z" }
          |  alpha { path = "../a" }
          |}""".stripMargin).dependencies

      deps.map(_.label) shouldBe List("alpha", "zeta")
    }

    "what a coordinate has to be" - {

      "a URL is refused rather than stripped, because the coordinate is the identity" in {
        refused("""dependencies { j { git = "https://github.com/e/j", version = "1.0.0" } }""") should
          include("is a URL")
      }

      "a host with nothing under it" in {
        refused("""dependencies { j { git = "github.com", version = "1.0.0" } }""") should
          include("names a host and nothing under it")
      }

      "a trailing slash" in {
        refused("""dependencies { j { git = "github.com/e/j/", version = "1.0.0" } }""") should
          include("is not a coordinate")
      }
    }

    "the major version rides in the coordinate" - {

      "0.x and 1.x ride in the bare path" in {
        read("""dependencies { j { git = "github.com/e/j", version = "1.9.0" } }""")
          .dependencies.head.origin shouldBe Origin.Git("github.com/e/j", Version(1, 9, 0))
      }

      "2.x needs the suffix, or two majors would share one module name" in {
        refused("""dependencies { j { git = "github.com/e/j", version = "2.0.0" } }""") should
          include("no '/v2'")
      }

      "with the suffix it is accepted, and the suffix stays part of the coordinate" in {
        val dep = read("""dependencies { j { git = "github.com/e/j/v2", version = "2.0.0" } }""")
          .dependencies.head

        dep.origin shouldBe Origin.Git("github.com/e/j/v2", Version(2, 0, 0))
        dep.canonical shouldBe "github.com.e.j.v2"
      }

      "a suffix that disagrees with the version it is asked for" in {
        refused("""dependencies { j { git = "github.com/e/j/v3", version = "2.0.0" } }""") should
          include("holds 3.x and nothing else")
      }
    }

    "a version is three numbers" - {

      "two is not enough" in {
        refused("""dependencies { j { git = "github.com/e/j", version = "1.4" } }""") should
          include("is not a version")
      }

      "and a leading zero is a typo rather than a number" in {
        refused("""dependencies { j { git = "github.com/e/j", version = "01.4.2" } }""") should
          include("is not a version")
      }
    }

    "the two halves of an entry that decide each other" - {

      "both a coordinate and a path" in {
        refused("""dependencies { j { git = "github.com/e/j", version = "1.0.0", path = "../j" } }""") should
          include("cannot be both")
      }

      "neither" in {
        refused("""dependencies { j { mount = "x" } }""") should include("names neither")
      }

      "a coordinate with no version" in {
        refused("""dependencies { j { git = "github.com/e/j" } }""") should include("and no 'version'")
      }

      // Nothing could keep the promise: the directory is whatever it is on disk right now.
      "a path with a version" in {
        refused("""dependencies { j { path = "../j", version = "1.0.0" } }""") should
          include("nothing to resolve a version against")
      }
    }

    // The same judgement `requires` makes about a misspelled capability, and for a sharper reason:
    // ignoring the key would resolve the default branch while the author believed they had pinned
    // a version.
    "a misspelled key is refused rather than ignored" in {
      refused("""dependencies { j { git = "github.com/e/j", versoin = "1.0.0" } }""") should
        include("is not something a dependency says")
    }

    "one thing named twice" - {

      "the same coordinate under two labels" in {
        refused(
          """dependencies {
            |  a { git = "github.com/e/j", version = "1.0.0" }
            |  b { git = "github.com/e/j", version = "1.2.0" }
            |}""".stripMargin) should include("one package is one dependency")
      }

      "two packages mounted under one root name" in {
        refused(
          """dependencies {
            |  a { path = "../a", mount = "json" }
            |  b { path = "../b", mount = "json" }
            |}""".stripMargin) should include("cannot share a root name")
      }
    }

    "a mount has to be a name a module could have" in {
      refused("""dependencies { j { path = "../j", mount = "sysl.json" } }""") should
        include("not a name a module can have")
    }

    "an entry that is not a block at all" in {
      refused("""dependencies { j = "github.com/e/j" }""") should include("is not a block")
    }
  }

  "how a coordinate becomes something git can clone" - {

    // The suffix is identity and not location: there is no branch or directory called v2 in the
    // repository, so it comes off before the URL is built.
    "the major suffix comes off" in {
      Dependency.cloneUrl("github.com/e/j/v2") shouldBe "https://github.com/e/j.git"
    }

    "and a coordinate without one is left alone" in {
      Dependency.cloneUrl("github.com/e/j") shouldBe "https://github.com/e/j.git"
    }

    "v0 and v1 are not suffixes — the first two majors ride in the bare path" in {
      Dependency.majorSuffix("github.com/e/j/v1") shouldBe None
      Dependency.majorSuffix("github.com/e/j/v2") shouldBe Some(2)
      Dependency.majorSuffix("github.com/e/j") shouldBe None
    }

    // A repository whose own name ends in something v-shaped is not a major suffix.
    "a path segment that merely starts with a v" in {
      Dependency.majorSuffix("github.com/e/vector") shouldBe None
      Dependency.cloneUrl("github.com/e/vector") shouldBe "https://github.com/e/vector.git"
    }

    "a version knows the tag it is published under" in {
      Version(2, 1, 0).tag shouldBe "v2.1.0"
    }
  }

  /** `reference/packages.md § One heap, and the package that names it` — a package may name the
   * pair a program's storage comes from, and a program has one of them.
   */
  "the allocator" - {

    "a package names a pair" in {
      val c = read(
        """allocator {
          |  alloc = "pvPortMalloc"
          |  free  = "vPortFree"
          |}
          |""".stripMargin)

      c.allocator shouldBe Some(Allocator("pvPortMalloc", "vPortFree"))
    }

    // The direction that cannot silently change an existing program.
    "a file that says nothing gets libc's, which is what every program had before" in {
      read("").allocator shouldBe None
      Allocator.choose(Nil) shouldBe Right(Allocator("malloc", "free"))
    }

    "half a pair is refused rather than completed from libc" in {
      val e = refused("""allocator { alloc = "pvPortMalloc" }""")

      e should include("names no 'free'")
      e should include("both halves")
    }

    "a name a C function could not have" in {
      refused("""allocator { alloc = "pv Port", free = "vPortFree" }""") should
        include("not a name a C function can have")
    }

    // The same judgement `dependencies` makes about a misspelled key: somebody wrote `malloc` and
    // believes they have redirected the allocator.
    "a key an allocator does not have" in {
      val e = refused("""allocator { malloc = "pvPortMalloc", free = "vPortFree" }""")

      e should include("'allocator.malloc' is not something an allocator says")
      e should include("'alloc' and 'free'")
    }

    "one symbol for both halves" in {
      refused("""allocator { alloc = "xmalloc", free = "xmalloc" }""") should
        include("names 'xmalloc' for both halves")
    }

    "one package's declaration is the program's" in {
      Allocator.choose(List("freertos" -> Allocator("pvPortMalloc", "vPortFree"))) shouldBe
        Right(Allocator("pvPortMalloc", "vPortFree"))
    }

    // Two packages knowing the same kernel is one fact stated twice, which is the ordinary case as
    // soon as a driver package is built on the binding that names the allocator.
    "two packages naming the same pair agree rather than conflict" in {
      val freertos = Allocator("pvPortMalloc", "vPortFree")

      Allocator.choose(List("freertos" -> freertos, "st7796" -> freertos)) shouldBe Right(freertos)
    }

    "two packages naming different pairs is refused, and both are named" in {
      val e = Allocator.choose(List(
        "freertos" -> Allocator("pvPortMalloc", "vPortFree"),
        "arena"    -> Allocator("arena_alloc", "arena_free"),
      )).swap.getOrElse(fail("expected a refusal"))

      e should include("a program has one heap")
      e should include("'freertos' names pvPortMalloc / vPortFree")
      e should include("'arena' names arena_alloc / arena_free")
    }

    // The message groups by pair rather than listing packages, so three packages against two
    // allocators reads as two claims and not as three.
    "three packages against two pairs reads as two claims" in {
      val freertos = Allocator("pvPortMalloc", "vPortFree")
      val e = Allocator.choose(List(
        "freertos" -> freertos,
        "st7796"   -> freertos,
        "arena"    -> Allocator("arena_alloc", "arena_free"),
      )).swap.getOrElse(fail("expected a refusal"))

      // And the verb agrees with the count on each claim independently — two packages `name` a pair
      // where one `names` it, which is the whole reason the grouping is worth reading.
      e should include("'freertos' and 'st7796' name pvPortMalloc / vPortFree")
      e should include("'arena' names arena_alloc / arena_free")
    }

    "a symbol is what LLVM will accept after an '@'" in {
      Allocator.isSymbol("pvPortMalloc") shouldBe true
      Allocator.isSymbol("_malloc_r") shouldBe true
      Allocator.isSymbol("") shouldBe false
      Allocator.isSymbol("2malloc") shouldBe false
      Allocator.isSymbol("my malloc") shouldBe false
      Allocator.isSymbol("my.malloc") shouldBe false
    }
  }

  "defines" - {

    "a carried C file is compiled with the macros the block names" in {
      val c = read(
        """defines {
          |  "sh/sysl/miniz/c/miniz.c" {
          |    MINIZ_NO_MALLOC   = true
          |    TDEFL_LESS_MEMORY = 1
          |    MZ_ASSERT         = "((void)0)"
          |  }
          |}
        """.stripMargin)

      c.definesFor("sh/sysl/miniz/c/miniz.c") shouldBe
        List("MINIZ_NO_MALLOC", "MZ_ASSERT=((void)0)", "TDEFL_LESS_MEMORY=1")
    }

    "a file the block does not name is compiled with nothing extra" in {
      val c = read(
        """defines {
          |  "a.c" { X = true }
          |}
        """.stripMargin)

      c.definesFor("b.c") shouldBe Nil
    }

    "true is a bare -DNAME, which is what '#ifdef' reads" in {
      read("""defines { "a.c" { X = true } }""").definesFor("a.c") shouldBe List("X")
    }

    /* The one that would otherwise be guessed at. `#ifdef` sees a macro defined as zero, so
     * "don't define it" and "define it as 0" are different instructions and false could be either.
     */
    "false is refused, because it does not say which of two things is meant" in {
      val e = refused("""defines { "a.c" { X = false } }""")

      e should include("does not say which of two things is meant")
      e should include("write '0'")
    }

    "a key that is not a C file is refused" in {
      refused("""defines { "c/miniz.h" { X = true } }""") should include("does not name a '.c' file")
    }

    "a key that is absolute or climbs out of the package is refused" in {
      refused("""defines { "/etc/x.c" { X = true } }""") should include("is an absolute path")
      refused("""defines { "../other/x.c" { X = true } }""") should include("climbs out of the package")
    }

    "a name the preprocessor would not take is refused" in {
      refused("""defines { "a.c" { "2X" = true } }""") should include("not a name a C macro can have")
    }

    "a value that is not scalar is refused, since a macro is text" in {
      refused("""defines { "a.c" { X { y = 1 } } }""") should include("must be true or a scalar")
    }

    "an entry that is not a block says what a key is for" in {
      refused("""defines { "a.c" = true }""") should include("is not a block of macros")
    }

    /* The manifest writes a relative path and the compilation names the file by whatever the source
     * walk produced -- which is absolute whatever the reader typed, so `sysl test .` in a package's
     * own tree yields an absolute path for a root of `.`. Keying off the root instead was the defect
     * this replaced: it matched only when the root happened to be typed the same way the walk
     * spells it, and when it did not match, the C compiled under its defaults with nothing to say so.
     */
    "are keyed by the path the source walk found, not by the one the manifest wrote" in {
      val c = read(
        """defines {
          |  "sh/sysl/miniz/c/miniz.c" { MINIZ_NO_MALLOC = true }
          |  "sh/sysl/miniz/c/shim.c"  { MINIZ_NO_MALLOC = true }
          |}
        """.stripMargin)

      val found = List("/private/tmp/miniz/sh/sysl/miniz/c/miniz.c",
                       "/private/tmp/miniz/sh/sysl/miniz/c/shim.c",
                       "/private/tmp/miniz/sh/sysl/miniz/c/unrelated.c")

      c.carriedDefines(found) shouldBe Right(Map(
        "/private/tmp/miniz/sh/sysl/miniz/c/miniz.c" -> List("MINIZ_NO_MALLOC"),
        "/private/tmp/miniz/sh/sysl/miniz/c/shim.c"  -> List("MINIZ_NO_MALLOC")))
    }

    "a C file at the package root is matched by its own name" in {
      val c = read("""defines { "thing.c" { X = true } }""")

      c.carriedDefines(List("/pkg/thing.c")) shouldBe Right(Map("/pkg/thing.c" -> List("X")))
    }

    /* The one mistake in a `defines` block that reading the file cannot catch, and the one that is
     * otherwise silent: everything else is refused at parse time, while a path that is merely wrong
     * would leave the macros reaching nothing and the C compiling under its defaults.
     */
    "a path naming no file the package carries is refused rather than passed over" in {
      val c = read("""defines { "sh/sysl/miniz/c/typo.c" { X = true } }""")

      c.carriedDefines(List("/pkg/sh/sysl/miniz/c/miniz.c")) match
        case Left(e)  => e should include("names a file this package does not carry")
        case Right(m) => fail(s"expected a refusal, got: $m")
    }

    /* Suffix matching is within one package's own file list, so a name that merely ends the same way
     * as another package's does not reach across -- the list handed in is that package's C and
     * nothing else.
     */
    "a partial segment is not a match" in {
      val c = read("""defines { "c/util.c" { X = true } }""")

      c.carriedDefines(List("/pkg/src/notc/util.c")) match
        case Left(e)  => e should include("does not carry")
        case Right(m) => fail(s"expected a refusal, got: $m")
    }

    /* The case this exists for: a package whose C shares one configuration says it once. miniz's
     * implementation and its shim read one header under five options, and two copies of that list
     * is how the two translation units start to disagree -- which is the silent ABI skew the block
     * exists to prevent.
     */
    "a key names several files with braces, as a shell writes them" in {
      val c = read(
        """defines {
          |  "sh/sysl/miniz/c/{miniz,shim}.c" { MINIZ_NO_MALLOC = true, TDEFL_LESS_MEMORY = 1 }
          |}
        """.stripMargin)

      c.definesFor("sh/sysl/miniz/c/miniz.c") shouldBe List("MINIZ_NO_MALLOC", "TDEFL_LESS_MEMORY=1")
      c.definesFor("sh/sysl/miniz/c/shim.c")  shouldBe List("MINIZ_NO_MALLOC", "TDEFL_LESS_MEMORY=1")
      c.defines.keySet shouldBe Set("sh/sysl/miniz/c/miniz.c", "sh/sysl/miniz/c/shim.c")
    }

    "several groups multiply out" in {
      PackageConfig.expand("{a,b}/{x,y}.c") shouldBe Right(List("a/x.c", "a/y.c", "b/x.c", "b/y.c"))
    }

    "a key with no group is itself" in {
      PackageConfig.expand("a/b.c") shouldBe Right(List("a/b.c"))
    }

    "a group of one is that one" in {
      PackageConfig.expand("{only}.c") shouldBe Right(List("only.c"))
    }

    /* Each alternative is still a path, so a brace cannot smuggle past the checks a written-out key
     * has to pass.
     */
    "an alternative that is not a C file is refused like any other key" in {
      refused("""defines { "c/{miniz.c,shim.h}" { X = true } }""") should
        include("does not name a '.c' file")
    }

    "a nested group is refused" in {
      refused("""defines { "c/{a,{b,c}}.c" { X = true } }""") should include("nests one group")
    }

    "an empty alternative is refused" in {
      refused("""defines { "c/{a,}.c" { X = true } }""") should include("empty alternative")
    }

    "an unbalanced brace is refused" in {
      refused("""defines { "c/{a,b.c" { X = true } }""") should include("unbalanced brace")
      refused("""defines { "c/a,b}.c" { X = true } }""") should include("unbalanced brace")
    }

    /* Two blocks reaching one file has no sensible merge -- the later would silently win, which is
     * the one outcome nobody could have meant by writing both.
     */
    "a file configured from two blocks is refused" in {
      val e = refused(
        """defines {
          |  "c/{a,b}.c" { X = true }
          |  "c/a.c"     { Y = true }
          |}
        """.stripMargin)

      e should include("from more than one block")
      e should include("c/a.c")
    }

    "a macro name is one the preprocessor would take" in {
      PackageConfig.isMacroName("MINIZ_NO_MALLOC") shouldBe true
      PackageConfig.isMacroName("_X2") shouldBe true
      PackageConfig.isMacroName("") shouldBe false
      PackageConfig.isMacroName("2X") shouldBe false
      PackageConfig.isMacroName("X-Y") shouldBe false
      PackageConfig.isMacroName("X Y") shouldBe false
    }
  }

  "a file that is not HOCON at all is one line rather than a stack trace" in {
    val e = refused("package { name = ")

    e should startWith("package.hocon:")
    e shouldNot include("Exception")
  }

  /** `dev_dependencies` — what this package's own tests need and its consumers do not
   * (`reference/packages.md § Dependencies a test alone needs`).
   *
   * Read exactly as `dependencies` is, which is the point: the entries are the same shape, get the
   * same refusals, and differ only in who resolves them.
   */
  "dev_dependencies" - {

    "reads the same entries dependencies does" in {
      val c = read("""
        dependencies { parsing { git = "github.com/sysl-lang/parsing", version = "0.5.0" } }
        dev_dependencies { quickjs { git = "github.com/sysl-lang/quickjs", version = "0.1.0" } }
      """)

      c.dependencies.map(_.label) shouldBe List("parsing")
      c.devDependencies.map(_.label) shouldBe List("quickjs")
      c.devDependencies.head.canonical shouldBe "github.com.sysl-lang.quickjs"
    }

    "is not an unknown key" in {
      read("""dev_dependencies { q { path = "../q" } }""").warnings shouldBe Nil
    }

    "is absent by default" in {
      read("""package { name = "p" }""").devDependencies shouldBe Nil
    }

    // The same misspelling refusal `dependencies` makes, and it has to name the right block or a
    // reader goes looking in the other one.
    "reports a misspelled key against its own block" in {
      val e = refused("""dev_dependencies { q { versoin = "1.0.0", git = "github.com/e/q" } }""")

      e should include("'dev_dependencies.q.versoin'")
    }

    "is not a block of blocks" in {
      refused("""dev_dependencies { q = "1.0.0" }""") should include("'dev_dependencies.q'")
    }

    /** A package named by both blocks says two things about one package — that a consumer gets it,
     * and that a consumer does not. It is `readOrigin`'s judgement about a `git` beside a `path`:
     * a closed vocabulary, where quietly picking one lets the writer go on believing the other.
     */
    "refuses a package named by both blocks" in {
      val e = refused("""
        dependencies     { q { git = "github.com/e/q", version = "1.0.0" } }
        dev_dependencies { q { git = "github.com/e/q", version = "1.0.0" } }
      """)

      e should include("'q'")
      e should include("both")
      e should include("cannot do both")
    }

    "names every package in both, not only the first" in {
      val e = refused("""
        dependencies     { a { path = "../a" }, b { path = "../b" } }
        dev_dependencies { a { path = "../a" }, b { path = "../b" } }
      """)

      e should include("'a'")
      e should include("'b'")
      e should include("are named by both")
    }

    // Different labels for different packages is the ordinary case and must not be caught by the
    // check above -- which is the assertion that says the refusal is about the label rather than
    // about the two blocks both being present.
    "allows both blocks when they name different packages" in {
      val c = read("""
        dependencies     { a { path = "../a" } }
        dev_dependencies { b { path = "../b" } }
      """)

      c.dependencies.map(_.label) shouldBe List("a")
      c.devDependencies.map(_.label) shouldBe List("b")
    }
  }

  /** The `features` block and the three fields it adds to a dependency entry (`PackageFeatures`).
   *
   * What is checked here is the *manifest* alone: what the block parses to, and the four things a
   * file can say wrongly about one. Resolving a consumer's `features = [desktop]` against the
   * dependency's own manifest is a separate question, because nothing at this point has fetched the
   * package it would have to ask.
   */
  "features" - {

    val whole =
      """
        features {
          default = [server]
          server  = [llhttp, nghttp2]
          desktop = [webview]
        }
        dependencies {
          llhttp  { git = "github.com/sysl-lang/llhttp",  version = "0.2.0", optional = true }
          nghttp2 { git = "github.com/sysl-lang/nghttp2", version = "0.3.0", optional = true }
          webview { git = "github.com/sysl-lang/webview", version = "0.1.0", optional = true }
        }
      """

    "a feature names optional dependencies and other features, in the order the file writes them" in {
      val c = read(whole)

      c.features.keys.toList shouldBe List("default", "server", "desktop")
      c.features("default") shouldBe List("server")
      c.features("server") shouldBe List("llhttp", "nghttp2")
      c.features("desktop") shouldBe List("webview")
    }

    "the dependencies a feature turns on are read as optional" in {
      val c = read(whole)

      c.dependencies.map(_.label) shouldBe List("llhttp", "nghttp2", "webview")
      c.dependencies.forall(_.optional) shouldBe true
    }

    // A manifest that says nothing about features is a manifest this compiler reads exactly as the
    // one before it did -- the direction that cannot be allowed to change, since every package in
    // the org is written this way.
    "an old-style manifest has no features and its dependencies are unchanged" in {
      val c = read("""
        package { name = "thing", version = "1.0.0" }
        dependencies { json { git = "github.com/edadma/sysl-json", version = "1.4.0" } }
      """)

      c.features shouldBe empty
      c.dependencies shouldBe
        List(Dependency("json", Origin.Git("github.com/edadma/sysl-json", Version(1, 4, 0))))
      c.warnings shouldBe empty
    }

    "'optional' is false and 'default_features' is true where the entry says neither" in {
      val d = read("""dependencies { json { path = "../json" } }""").dependencies.head

      d.optional shouldBe false
      d.features shouldBe empty
      d.defaultFeatures shouldBe true
    }

    // The consumer side: which of the dependency's own features this entry asks for. Accepted as
    // written here and resolved against that package's manifest elsewhere.
    "a consumer entry may ask for features and refuse the defaults" in {
      val d = read("""
        dependencies { ui { path = "../ui", features = [desktop, tray], default_features = false } }
      """).dependencies.head

      d.features shouldBe List("desktop", "tray")
      d.defaultFeatures shouldBe false
    }

    "a feature that names a dependency which is not optional is refused" in {
      val e = refused("""
        features     { server = [llhttp] }
        dependencies { llhttp { path = "../llhttp" } }
      """)

      e should include("'features.server'")
      e should include("'llhttp'")
      e should include("not optional")
      e should include("optional = true")
    }

    "an optional dependency that no feature names is refused" in {
      val e = refused("""
        features     { server = [llhttp] }
        dependencies {
          llhttp  { path = "../llhttp",  optional = true }
          webview { path = "../webview", optional = true }
        }
      """)

      e should include("'webview'")
      e should include("no feature names")
    }

    "a feature that names something declared nowhere is refused" in {
      val e = refused("""
        features     { server = [llhttp, nghttp3] }
        dependencies { llhttp { path = "../llhttp", optional = true } }
      """)

      e should include("'features.server'")
      e should include("'nghttp3'")
      e should include("neither a dependency")
    }

    // A feature may imply a feature, so the implications form a graph and the graph has to be
    // acyclic -- otherwise selecting one never finishes.
    "features that turn each other on are refused" in {
      val e = refused("""features { a = [b], b = [a] }""")

      e should include("'a' turns on 'b'")
      e should include("turns on 'a'")
      e should include("reach an end")
    }

    // A feature member that is ALSO the label of an optional dependency of the same name names that
    // dependency, not a feature turning itself on -- this is not the cycle above.
    "a feature naming an optional dependency of its own name is accepted" in {
      val c = read("""
        features     { lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb", optional = true } }
      """)

      c.features("lmdb") shouldBe List("lmdb")
    }

    "a feature naming a NON-optional dependency of its own name is refused as not optional" in {
      val e = refused("""
        features     { lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb" } }
      """)

      e should include("'features.lmdb'")
      e should include("'lmdb'")
      e should include("not optional")
      e shouldNot include("turns on")
      e shouldNot include("reach an end")
    }

    // The exception is a SELF-reference only. A member naming a feature of this package is that
    // feature wherever a dependency happens to share its label, which is what lets a package name
    // its features after the dependencies they turn on.
    "a feature member naming another feature is that feature even where a dependency shares its label" in {
      val c = read("""
        features     { default = [lmdb], lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb", optional = true } }
      """)

      c.features("default") shouldBe List("lmdb")
      c.features("lmdb") shouldBe List("lmdb")
    }

    // A self-reference with NO dependency of that label really is a feature turning itself on, and
    // there is nothing else it could mean, so the cycle refusal still has to fire.
    "a feature that turns itself on with no dependency of that label is refused as a cycle" in {
      val e = refused("""features { a = [b], b = [b] }""")

      e should include("'b' turns on 'b'")
      e should include("reach an end")
    }

    // `dep:X` names the dependency wherever it is written, so it is held to the same optional rule a
    // bare dependency member is.
    "a 'dep:' member naming an optional dependency is accepted" in {
      val c = read("""
        features     { server = ["dep:lmdb"], lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb", optional = true } }
      """)

      c.features("server") shouldBe List("dep:lmdb")
    }

    // `dep:` is WRITTEN IN QUOTES, because HOCON reads a bare colon as the separator between a key
    // and its value and refuses the line long before any of this is reached. Cargo's TOML requires
    // the quotes of it too, so there is nothing to choose between the two -- but a manifest that
    // leaves them off gets a parse error rather than anything about features, which is worth pinning
    // so a later reader knows the quotes are load-bearing.
    "a 'dep:' member written without quotes is a parse error" in {
      val e = refused("""
        features     { server = [dep:lmdb], lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb", optional = true } }
      """)

      e should include("unexpected token")
    }

    "a 'dep:' member naming a NON-optional dependency is refused as not optional" in {
      val e = refused("""
        features     { server = ["dep:lmdb"] }
        dependencies { lmdb { path = "../lmdb" } }
      """)

      e should include("'features.server'")
      e should include("'lmdb'")
      e should include("not optional")
    }

    // `dep:` says a dependency and nothing else, so it never falls back to reading as a feature --
    // the refusal names the dependency the manifest is missing.
    "a 'dep:' member naming no dependency of this package is refused" in {
      val e = refused("""
        features     { server = ["dep:nosuch"], lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb", optional = true } }
      """)

      e should include("'features.server'")
      e should include("'nosuch'")
      e should include("does not declare")
    }

    "a 'dep:' member naming a feature rather than a dependency is refused" in {
      val e = refused("""
        features     { server = ["dep:extras"], extras = [lmdb], lmdb = [lmdb] }
        dependencies { lmdb { path = "../lmdb", optional = true } }
      """)

      e should include("'extras'")
      e should include("does not declare")
    }

    "a feature may imply a feature as long as the chain ends" in {
      val c = read("""
        features {
          default = [server]
          server  = [tls]
          tls     = [openssl]
        }
        dependencies { openssl { path = "../openssl", optional = true } }
      """)

      c.features("default") shouldBe List("server")
      c.features("tls") shouldBe List("openssl")
    }

    "'features' is a key this compiler knows, so it draws no unknown-key warning" in {
      read("""features { }""").warnings shouldBe empty
    }

    "a feature whose value is not a list is refused" in {
      val e = refused("""features { server = "llhttp" }""")

      e should include("'features.server'")
      e should include("is not a list")
    }

    "'optional' that is not a yes-or-no answer is refused" in {
      val e = refused("""dependencies { llhttp { path = "../llhttp", optional = "yes" } }""")

      e should include("'dependencies.llhttp.optional'")
      e should include("'true' or 'false'")
    }
  }

  /** `optimization` — the level this project's builds hand clang, stated once instead of on every
   * command line (`PackageConfig.optimization`).
   *
   * **The refusal is the half worth pinning.** `--optimize` lets clang rule on what it was given,
   * which is right for something typed at a build somebody is watching; a manifest is read by every
   * later build and by consumers who did not write it, so a level clang has no answer for is refused
   * where the key is, naming the key, what was written, and what may be.
   */
  "the optimization level" - {

    "is the level a build hands clang, written as clang spells one after the '-O'" in {
      read("optimization = \"2\"").optimization shouldBe Some("2")
    }

    "every level clang always has may be named" in {
      for level <- Toolchain.levels do
        withClue(level) { read(s"optimization = \"$level\"").optimization shouldBe Some(level) }
    }

    // What somebody writes before remembering the quotes, and it says exactly one thing. HOCON keeps
    // a number's literal text, so what is read is the `2` they wrote.
    "and an unquoted number is the level it spells" in {
      read("""optimization = 2""").optimization shouldBe Some("2")
    }

    "a file that names none says nothing about the level" in {
      read("""package { name = "thing", version = "1.0.0" }""").optimization shouldBe None
    }

    "'optimization' is a key this compiler knows, so it draws no unknown-key warning" in {
      read("optimization = \"2\"").warnings shouldBe empty
    }

    "a level clang does not have is refused here rather than left to clang" in {
      val e = refused("optimization = \"9\"")

      e should include("'optimization = \"9\"'")
      e should include("0, 1, 2, 3, s, z")
    }

    // `fast` and `g` are clang's own and are deliberately outside the set a manifest may name: one is
    // being retired and the other is about debugging rather than about what a project is built at.
    // Both are still typable on the command line, where clang answers for itself.
    "including the ones clang has and a manifest may not name" in {
      for level <- List("fast", "g") do
        withClue(level) { refused(s"optimization = \"$level\"") should include(level) }
    }

    "and a block is refused with the same set, since there is nothing else it could mean" in {
      val e = refused("""optimization { level = "2" }""")

      e should include("'optimization'")
      e should include("0, 1, 2, 3, s, z")
    }
  }
}
