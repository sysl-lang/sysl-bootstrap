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
}
