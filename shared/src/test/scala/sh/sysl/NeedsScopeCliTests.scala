package sh.sysl

import io.github.edadma.cross_platform.*

/** The two capability rules a board program meets first, driven the way a board program is built: a
 * project whose `package.hocon` says the machine has no operating system, and a library reached with
 * `--lib` (`reference/modules.md § The target's half needs no clause at all`).
 *
 * - what a `@needs(os)` declaration's body reaches is charged to whoever reaches the declaration
 *   (`reference/modules.md § A declaration may name what reaching it needs`), so the module holding
 *   it is still importable for everything else;
 * - what a library's `@tests` file imports is not something the library ships
 *   (`reference/modules.md § A @tests file states its own capabilities`), so a program linking the
 *   library is not refused over it — while the library's own `sysl test`, on a machine that has an
 *   operating system, still runs that file.
 *
 * In-process forms of both are in `DeclCapabilityTests` and `TargetCapabilityTests`; these exist
 * because the config is what a board program actually writes, and nothing in-process reads one.
 */
class NeedsScopeCliTests extends LibraryCliSupport {

  /** A board with no operating system, said the way a project says it. */
  private val board =
    """targets {
      |  default = "thumbv6m-freestanding"
      |  thumbv6m-freestanding { capabilities { os = false, posix = false } }
      |}
      |""".stripMargin

  private def tree(files: (String, String)*): String = {
    val root = createTempDirectory("sysl-needs-scope-")

    for (file, body) <- files do
      Project.parentOf(s"$root/$file").foreach(createDirectories)
      writeFile(s"$root/$file", body)

    root
  }

  private val sys =
    "sys/a.sysl" -> "module sys\n\n@needs(os)\nnow() -> bool = sysl.fs.exists(\"/\")\n\nplain() -> int = 3\n"

  "a '@needs(os)' declaration in a board program's own module" - {

    "leaves the module importable for the declaration beside it" in {
      val root = tree(sys, PackageConfig.FileName -> board,
        "main.sysl" -> "import sys.plain\n\nprint(plain())\n")

      emitted(Config(command = "emit-llvm", file = root)) should include("define")
    }

    "and calling it is refused at the call only, naming the machine" in {
      val root = tree(sys, PackageConfig.FileName -> board,
        "main.sysl" -> "import sys.now\n\nprint(now())\n")

      val (status, said) = diagnostics(Config(command = "emit-llvm", file = root))

      status should not be 0
      said should include("this reaches 'sys.now', which needs 'os', and 'thumbv6m-freestanding' does not provide it")
      said shouldNot include("which requires 'os'")
      // One per capability the declaration names, each the call's; none about the module.
      withClue(said)(all(said.split("error:").toList.drop(1)) should include("this reaches 'sys.now', which needs "))
    }

    // The import the declaration's body writes through, and a gated type its signature names, are
    // what its callers meet and nobody else does — so neither costs the module either.
    val viaImport = "sys/a.sysl" -> ("module sys\n\nimport sysl.fs.write_bytes\n\n@needs(os)\n" +
      "now(f: string) -> bool = write_bytes(f, \"x\".bytes).is_ok()\n\nplain() -> int = 3\n")

    val viaSignature = "sys/a.sysl" -> ("module sys\n\n@needs(os)\n" +
      "now(f: string) -> Result[unit, sysl.fs.IoError] = sysl.fs.write_bytes(f, \"x\".bytes)\n\n" +
      "plain() -> int = 3\n")

    "nor does the import its body writes through, or a type its signature names" in {
      for (sys, call) <- List(viaImport -> "now(\"/tmp/x\")", viaSignature -> "now(\"/tmp/x\").is_ok()") do
        val ok = tree(sys, PackageConfig.FileName -> board, "main.sysl" -> "import sys.plain\n\nprint(plain())\n")

        emitted(Config(command = "emit-llvm", file = ok)) should include("define")

        val calls = tree(sys, PackageConfig.FileName -> board,
          "main.sysl" -> s"import sys.now\n\nprint($call)\n")
        val (status, said) = diagnostics(Config(command = "emit-llvm", file = calls))

        status should not be 0
        said should include("this reaches 'sys.now', which needs 'os', and 'thumbv6m-freestanding' does not provide it")
        said shouldNot include("which requires 'os'")
    }

    "while one a shipping declaration also uses still costs the module" in {
      val shared = "sys/a.sysl" -> ("module sys\n\nimport sysl.fs.write_bytes\n\n@needs(os)\n" +
        "now(f: string) -> bool = write_bytes(f, \"x\".bytes).is_ok()\n\n" +
        "keep(f: string) -> bool = write_bytes(f, \"y\".bytes).is_ok()\n\nplain() -> int = 3\n")
      val root = tree(shared, PackageConfig.FileName -> board, "main.sysl" -> "import sys.plain\n\nprint(plain())\n")

      val (status, said) = diagnostics(Config(command = "emit-llvm", file = root))

      status should not be 0
      said should include("which requires 'os'")
    }
  }

  // `sysl.fs` requires `os` and not `posix` (`reference/modules.md § Capabilities are a module
  // property`), so a project saying its machine has an operating system that is not POSIX builds and
  // runs a program that reads and writes the filesystem.
  "a project whose machine has an os that is not POSIX" - {

    val notPosix = PackageConfig.FileName -> "capabilities { os = true, posix = false }\n"

    "reaches sysl.fs" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = tree(notPosix,
        "main.sysl" -> ("import sysl.fs.{make_temp_dir, read_text, write_text}\nimport sysl.path.join\n\n" +
          "val at = join(make_temp_dir(\"pub\").unwrap(), \"x\")\n\n" +
          "write_text(at, \"hi\").unwrap()\nprint(read_text(at).unwrap())\n"))

      ran(Config(command = "run", file = root)) should include("hi")
    }

    // Publishing names its pending file partly from the wall clock, and `sysl.time` reads no clock of
    // its own: the host's supplier is `sysl.posix.time`, which a machine without POSIX does not link
    // (`library/sysl/time/clock.sysl`). So the link names the seam left unsupplied -- never a
    // capability refusal of `sysl.fs`.
    "and publishing through it asks the project for a clock" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = tree(notPosix,
        "main.sysl" -> "import sysl.fs.write_text_atomic\n\nprint(write_text_atomic(\"/nonexistent/x\", \"\").is_ok())\n")

      val (state, said) = diagnostics(Config(command = "run", file = root))

      state should not be 0
      said should include("sysl_wall_us")
      said shouldNot include("which requires 'posix'")
    }
  }

  "a library whose '@tests' file reads the filesystem" - {

    def library(): String = tree(
      "notes/notes.sysl"       -> "module notes\n\nrender() -> int = 5\n",
      "notes/notes_tests.sysl" -> ("module notes\n@tests\n\nimport sysl.fs.exists\n\n@test\n" +
        "reads_a_fixture() =\n    assert(exists(\"/\"))\n"),
    )

    "is linked by a board program that only renders" in {
      val lib  = library()
      val root = tree(PackageConfig.FileName -> board, "main.sysl" -> "import notes.render\n\nprint(render())\n")

      emitted(Config(command = "emit-llvm", file = root, libs = List(lib))) should include("define")
    }

    "and still runs that file under its own 'sysl test' on a machine with an operating system" in {
      assume(Toolchain.clangAvailable, "clang not available")

      ran(Config(command = "test", file = library())) should include("1 passed")
    }
  }

  // Charging was half of it. The other half is that a consumer's build never READS a dependency's
  // test file: it used to be name-resolved against the consumer's target and dropped afterwards, and
  // `sysl.fs` declares no `make_temp_dir` on a machine with no operating system — so the import
  // itself was refused, in a file no build of the board program keeps.
  "a library whose '@tests' file imports what the board's machine does not declare" - {

    def library(tests: String = "import sysl.fs.make_temp_dir\n\n@test\n" +
        "makes_a_scratch_dir() =\n    assert(make_temp_dir(\"t\").is_ok())\n"): String = tree(
      "notes/notes.sysl"       -> "module notes\n\nrender() -> int = 5\n",
      "notes/notes_tests.sysl" -> s"module notes\n@tests\n\n$tests",
    )

    val boot = "import notes.render\n\n@export(\"main\")\nboot() -> int = render()\n"

    "is linked by a board program through '--lib'" in {
      val root = tree(PackageConfig.FileName -> board, "main.sysl" -> boot)

      emitted(Config(command = "emit-llvm", file = root, libs = List(library()))) should include("define")
    }

    "and archived for it by 'build-c'" in {
      assume(Target.named("thumbv6m-freestanding").flatMap(Toolchain.findBackendClang).isRight,
        "no clang that lowers for thumbv6m")

      val root = tree(PackageConfig.FileName -> board, "main.sysl" -> boot)

      succeeds(Config(command = "build-c", file = root, libs = List(library()),
        output = Some(s"$root/app.a")))
    }

    // A dependency's tests are its own author's to get right, and nothing of them ships: a mistake in
    // one is reported where that package is tested, never to a program that only links it.
    "nor is a type error in that file reported to the consumer" in {
      val broken = library("@test\nwrong() =\n    assert(\"not a bool\")\n")
      val root   = tree(PackageConfig.FileName -> board, "main.sysl" -> boot)

      emitted(Config(command = "emit-llvm", file = root, libs = List(broken))) should include("define")

      // ...and it is the package's own suite that says so.
      val (status, said) = diagnostics(Config(command = "test", file = broken))

      status should not be 0
      said should include("notes_tests.sysl")
    }

    // The consumer's OWN scaffolding is unchanged: analyzed and then dropped, as before, so its
    // mistakes are still the consumer's to hear about.
    "while the consumer's own '@tests' file is still read by its build" in {
      val root = tree(PackageConfig.FileName -> board, "main.sysl" -> boot,
        "own/own.sysl"       -> "module own\n\nk() -> int = 1\n",
        "own/own_tests.sysl" -> "module own\n@tests\n\n@test\nwrong() =\n    assert(\"not a bool\")\n")

      val (status, said) = diagnostics(Config(command = "emit-llvm", file = root, libs = List(library())))

      status should not be 0
      said should include("own_tests.sysl")
      said shouldNot include("notes_tests.sysl")
    }

    "and the library's own 'sysl test' still compiles and runs that file on the host" in {
      assume(Toolchain.clangAvailable, "clang not available")

      ran(Config(command = "test", file = library())) should include("1 passed")
    }
  }
}
