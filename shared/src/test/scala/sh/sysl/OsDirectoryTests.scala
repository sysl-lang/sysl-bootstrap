package sh.sysl

import io.github.edadma.cross_platform.*

/** `reference/modules.md § Platform selection`: a directory named `__<os>__` selects source for an
 * operating system and names nothing.
 *
 * **Every test here is written against the machine it runs on**, which is what lets one suite cover
 * a mechanism whose whole subject is that two machines see different files: the folder for
 * `Target.default.os` holds what must be compiled, and the folder for another operating system holds
 * something that could not be. What the second one holds is chosen so that a build compiling it
 * *fails* — a duplicate definition, a `#error`, source that does not parse — so a green run says the
 * file was not read and cannot say it by accident.
 *
 * That is the same trick `NativeSourceTests` uses for the directories a C walk must not take, and it
 * is the only honest way to test an absence.
 */
class OsDirectoryTests extends LibraryCliSupport {

  /** The per-OS directory this machine selects, and one it never will. */
  private val here: String = Project.spelling(Target.default.os)

  private val other: String =
    Project.spelling(if Target.default.os == Os.MacOS then Os.Linux else Os.MacOS)

  /** The same two as bare symbols, for building a comma-separated selector out of them. */
  private val hereSym: String = Conditional.osSymbol(Target.default.os)

  private val otherSym: String =
    Conditional.osSymbol(if Target.default.os == Os.MacOS then Os.Linux else Os.MacOS)

  private def projectOf(files: (String, String)*): String = {
    val root = createTempDirectory("sysl-osdir-")

    for (path, body) <- files do
      Project.parentOf(s"$root/$path").foreach(createDirectories)
      writeFile(s"$root/$path", body)

    root
  }

  /** Seven is a number only the C knows and the multiplication is only the sysl's, so neither half
   * can be dropped without the answer changing.
   */
  private val shim = "int demo_seven(void) { return 7; }\n"

  /** C that cannot compile, for the folder this machine must never look inside. */
  private val refuses = "#error this file is for another operating system\n"

  "a per-OS directory" - {

    "selects the implementation for this machine and contributes no name" in {
      val root = projectOf(
        "main.sysl"          -> "print(demo.tag())\n",
        "demo/common.sysl"   -> "module demo\n\nloud(s: string) -> string = s + \"!\"\n",
        s"demo/$here/impl.sysl"  -> "module demo\n\ntag() -> string = loud(\"selected\")\n",
        s"demo/$other/impl.sysl" -> "module demo\n\ntag() -> string = loud(\"other\")\n",
      )

      // The module is `demo` and not `demo.__macos__`, or the program's call would not resolve; and
      // the other folder's file is not compiled, or the two `tag`s would collide. One line asserts
      // both, which is the point of writing the two implementations at one name.
      ran(Config(command = "run", file = root)) shouldBe "selected!\n"
    }

    "leaves the files beside it compiled for every machine" in {
      val root = projectOf(
        "main.sysl"             -> "print(demo.tag())\n",
        "demo/common.sysl"      -> "module demo\n\nloud(s: string) -> string = s + \"!\"\n",
        s"demo/$here/impl.sysl" -> "module demo\n\ntag() -> string = loud(\"selected\")\n",
      )

      // `loud` is defined outside any folder and called from inside one. A rule that replaced a
      // directory's files with its selected folder's rather than adding to them would lose it.
      ran(Config(command = "run", file = root)) shouldBe "selected!\n"
    }

    "may hold a directory, which is a module of the one holding the folder" in {
      val root = projectOf(
        "main.sysl"                  -> "print(demo.sub.answer())\n",
        "demo/common.sysl"           -> "module demo\n\nloud(s: string) -> string = s + \"!\"\n",
        s"demo/$here/sub/deep.sysl"  -> "module demo.sub\n\nanswer() -> string = \"nested\"\n",
        s"demo/$other/sub/deep.sysl" -> "module demo.sub\n\nanswer() -> string = \"wrong\"\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "nested\n"
    }

    "works at the project root, where the folder is above every module" in {
      val root = projectOf(
        "main.sysl"                  -> "print(util.name())\n",
        s"$here/util/util.sysl"      -> "module util\n\nname() -> string = \"selected\"\n",
        s"$other/util/util.sysl"     -> "module util\n\nname() -> string = \"wrong\"\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "selected\n"
    }

    // The claim `reference/modules.md § Platform selection` makes and the one that costs something:
    // an unselected tree is not compiled, not analyzed and not *parsed*. Source that could never
    // lex proves the last of those, which nothing about a duplicate definition can.
    "is not parsed at all when it is not the one selected" in {
      val root = projectOf(
        "main.sysl"               -> "print(demo.tag())\n",
        s"demo/$here/impl.sysl"   -> "module demo\n\ntag() -> string = \"selected\"\n",
        s"demo/$other/broken.sysl" -> "module demo\n\n((( this is not sysl at all\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "selected\n"
    }
  }

  /** The case the whole mechanism exists for (`reference/ffi.md § A library may carry C`): a `.c`
   * cannot carry a sysl attribute, so the path is the only place a selector could go — and a shim
   * written against one system's header must not reach the other's compiler.
   */
  "the C under a per-OS directory" - {

    "is compiled for this machine, and the other's is never offered to clang" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = projectOf(
        "main.sysl"      -> "print(demo.seven_times(6))\n",
        "demo/demo.sysl" -> """module demo
                              |
                              |extern "demo_seven" c_seven() -> int
                              |
                              |seven_times(n: int) -> int = c_seven() * n
                              |""".stripMargin,
        s"demo/$here/shim.c"  -> shim,
        s"demo/$other/shim.c" -> refuses,
      )

      // A build that offered the other folder's file to clang stops on its `#error`, so 42 is the
      // whole assertion: the shim linked, and the one beside it did not.
      ran(Config(command = "run", file = root)) shouldBe "42\n"
    }

    // `reference/ffi.md § A library may carry C`'s rule is that C belongs to a **module**, and a
    // folder is not one — so the module it belongs to is the directory holding the folder, exactly
    // as the sysl inside one does.
    "belongs to the module holding the folder, not to the folder" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = projectOf(
        "main.sysl"          -> "print(demo.seven_times(6))\n",
        s"demo/$here/demo.sysl" -> """module demo
                                     |
                                     |extern "demo_seven" c_seven() -> int
                                     |
                                     |seven_times(n: int) -> int = c_seven() * n
                                     |""".stripMargin,
        s"demo/$here/shim.c" -> shim,
      )

      // `demo/` holds no sysl of its own — all of it is inside the folder — so the flattening is
      // what makes `demo` a module at all, and `walkModules` takes its C only because of that.
      ran(Config(command = "run", file = root)) shouldBe "42\n"
    }

    // `reference/ffi.md § A library may carry C`'s root exemption meets the folder. The root is the
    // tree rather than a directory in it, so its C is taken whether or not it holds sysl — and a
    // folder at the root has to inherit that, or a package namespaced by reverse DNS could not put
    // a per-OS shim where its other C goes.
    "is taken at the tree's own root, where no module is declared" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = projectOf(
        "main.sysl"      -> "print(demo.seven_times(6))\n",
        "demo/demo.sysl" -> """module demo
                              |
                              |extern "demo_seven" c_seven() -> int
                              |
                              |seven_times(n: int) -> int = c_seven() * n
                              |""".stripMargin,
        s"$here/shim.c"  -> shim,
        s"$other/shim.c" -> refuses,
      )

      ran(Config(command = "run", file = root)) shouldBe "42\n"
    }
  }

  /** `reference/modules.md § Capabilities are a module property`: a capability is a property of the
   * **module**, so every file of one states it. A file inside a folder is a file of the module
   * holding the folder, so the clause reaches in there too — which is a consequence of §5 rather
   * than a rule of its own, and is exactly what `library/sysl/fs/tests.sysl` tripped over on the
   * way to being written.
   */
  "a capability clause" - {

    "must appear on a file inside a per-OS directory, like every other file of the module" in {
      val root = projectOf(
        "main.sysl"             -> "print(1)\n",
        "demo/common.sysl"      -> "module demo\n@requires(os)\n\nloud(s: string) -> string = s\n",
        s"demo/$here/impl.sysl" -> "module demo\n\ntag() -> string = loud(\"x\")\n",
      )

      val (status, notes) = diagnostics(Config(command = "run", file = root))

      status should not be 0
      notes should include("capabilit")
    }

    "and the two agreeing is what a module with a per-OS half looks like" in {
      val root = projectOf(
        "main.sysl"             -> "print(demo.tag())\n",
        "demo/common.sysl"      -> "module demo\n@requires(os)\n\nloud(s: string) -> string = s + \"!\"\n",
        s"demo/$here/impl.sysl" -> "module demo\n@requires(os)\n\ntag() -> string = loud(\"selected\")\n",
      )

      // Discriminating against the case above: without this the refusal could be about the folder
      // rather than about the clause, and would look exactly as green.
      ran(Config(command = "run", file = root)) shouldBe "selected!\n"
    }
  }

  "a directory that looks like one and is not" - {

    // A misspelling that read as an ordinary directory would compile nothing on any target and be
    // reported, eventually, as a missing function — which is the failure the closed vocabulary of
    // `Conditional.symbols` exists to refuse, in the other place a source tree names an OS.
    "is refused, and the message says which machines there are" in {
      val root = projectOf(
        "main.sysl"            -> "print(demo.tag())\n",
        "demo/__linx__/x.sysl" -> "module demo\n\ntag() -> string = \"typo\"\n",
      )

      val (status, notes) = diagnostics(Config(command = "run", file = root))

      status should not be 0
      notes should include("__linx__")
      // The vocabulary is listed as **bare symbols** rather than as `__linux__`, because a selector
      // may name several of them at once and what a reader has to write inside the underscores is
      // one element of a list. This assertion said `__linux__` while a directory could name exactly
      // one machine, and there was no difference between the two spellings then.
      notes should include("linux")
    }

    "is refused whichever machine is asking" in {
      // `__linx__` is not the selected folder on any target, so this is the same tree as above and
      // the point is that it is still refused: validation is over every folder a walk meets, not
      // only the one it takes. `Conditional` checks the conditions on branches it is not taking for
      // the same reason.
      val root = projectOf("main.sysl" -> "print(1)\n", "demo/__nosuchos__/x.sysl" -> "module demo\n")

      refused(Config(command = "run", file = root))
    }

    // A directory named for nothing in particular is an ordinary module directory and must stay one.
    // The shape is what marks a selector, so this is the line between the two.
    "is an ordinary directory when it does not have the shape" in {
      val root = projectOf(
        "main.sysl"           -> "print(demo.linux.tag())\n",
        "demo/linux/x.sysl"   -> "module demo.linux\n\ntag() -> string = \"a module called linux\"\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "a module called linux\n"
    }
  }

  "nesting one inside another" - {

    // Two axes are `#if` inside a sysl file or the C preprocessor inside a `.c`, and a target has
    // exactly one operating system — so a folder inside a folder could never select anything.
    "is refused directly" in {
      val root = projectOf(
        "main.sysl"                    -> "print(1)\n",
        s"demo/$here/$other/x.sysl"    -> "module demo\n",
      )

      val (status, notes) = diagnostics(Config(command = "run", file = root))

      status should not be 0
      notes should include(other)
      notes should include(here)
    }

    "is refused through a module in between" in {
      val root = projectOf(
        "main.sysl"                       -> "print(1)\n",
        s"demo/$here/sub/$other/x.sysl"    -> "module demo.sub\n",
      )

      // The refusal follows the folder down rather than stopping at its own listing, which is why
      // `contents` pairs each sub-directory with the folder it came out of.
      refused(Config(command = "run", file = root))
    }
  }

  /** A selector names one or more symbols and is taken when **any** of them holds — so it may name
   * a family (`posix`) or a list (`macos,linux`) as well as one operating system.
   *
   * **The vocabulary is `#if`'s**, which is what makes this one idea rather than two: `Conditional`
   * already had `posix` and `hosted` beside every operating system, and a source line could test
   * them while a directory could not name them. What is *not* in it is a processor — this walk has
   * an operating system and nothing else to ask.
   */
  "a selector naming more than one machine" - {

    "selects for a family, which is what a POSIX shim actually means" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = projectOf(
        "main.sysl"      -> "print(demo.seven_times(6))\n",
        "demo/demo.sysl" -> """module demo
                              |
                              |extern "demo_seven" c_seven() -> int
                              |
                              |seven_times(n: int) -> int = c_seven() * n
                              |""".stripMargin,
        "demo/__posix__/shim.c" -> shim,
      )

      ran(Config(command = "run", file = root)) shouldBe "42\n"
    }

    "and for 'hosted', which is the same claim one step weaker" in {
      val root = projectOf(
        "main.sysl"                    -> "print(demo.tag())\n",
        "demo/__hosted__/impl.sysl"    -> "module demo\n\ntag() -> string = \"hosted\"\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "hosted\n"
    }

    "and for a comma-separated list, when one of its names is this machine" in {
      val root = projectOf(
        "main.sysl"                              -> "print(demo.tag())\n",
        s"demo/__$hereSym,${otherSym}__/impl.sysl" -> "module demo\n\ntag() -> string = \"listed\"\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "listed\n"
    }

    // The negative half, and it is the one worth having: a list is *any*, so a list naming neither
    // must be as unread as a single name that does not match. Source that cannot lex says so.
    "and is not read at all when none of its names is" in {
      val root = projectOf(
        "main.sysl"                              -> "print(demo.tag())\n",
        "demo/impl.sysl"                         -> "module demo\n\ntag() -> string = \"outside\"\n",
        "demo/__windows,freestanding__/no.sysl"  -> "module demo\n\n((( this is not sysl at all\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "outside\n"
    }

    // The card this was written for: a freestanding machine has neither capability, so neither
    // folder is taken and the module contributes no C. Asked of the walk rather than of a build,
    // because there is no freestanding clang here to run one against.
    "so a freestanding machine takes neither, while the folder naming it is taken" in {
      val root = projectOf(
        "demo/demo.sysl"                -> "module demo\n",
        "demo/__posix__/posix.c"        -> shim,
        "demo/__hosted__/hosted.c"      -> shim,
        "demo/__freestanding__/bare.c"  -> shim,
      )

      val bare = Project.cSources(root, Some(Os.Freestanding)).map(s => Project.basename(s.name))

      bare shouldBe List("bare.c")
      Project.cSources(root, Some(Os.Linux)).map(s => Project.basename(s.name)).sorted shouldBe
        List("hosted.c", "posix.c")
    }

    // **`reference/modules.md § Platform selection`'s claim, now that there is a third POSIX system
    // to test it with.** The chapter says to prefer the name that says why over the list that says
    // which, and predicts exactly this: a `__posix__` folder covers a new POSIX machine untouched
    // while a `__macos,linux__` folder silently does not. Both halves are asserted, because the
    // failure is an **absence** — a folder that selects nothing is not an error, so the list form
    // would have produced a module missing its shim with no diagnostic anywhere, surfacing much
    // later as a function that is not there.
    //
    // The library follows its own advice: all three of its selector directories are `__posix__`, so
    // adding this machine to the registry cost them nothing.
    "so Android takes every __posix__ folder, and a spelled list of the other two silently misses it" in {
      val root = projectOf(
        "demo/demo.sysl"               -> "module demo\n",
        "demo/__posix__/why.c"         -> shim,
        "demo/__macos,linux__/which.c" -> shim,
      )

      Project.cSources(root, Some(Os.Android)).map(s => Project.basename(s.name)) shouldBe List("why.c")

      // The control: on Linux both are taken, so the difference above is Android's and not something
      // wrong with the tree.
      Project.cSources(root, Some(Os.Linux)).map(s => Project.basename(s.name)).sorted shouldBe
        List("which.c", "why.c")
    }

    // A machine's own folder is taken as well as the family one, which is what makes an Android-only
    // shim writable at all — logcat and `ANativeWindow` are on no other POSIX system.
    "and its own __android__ folder besides, which no other machine reads" in {
      val root = projectOf(
        "demo/demo.sysl"           -> "module demo\n",
        "demo/__android__/log.c"   -> shim,
        "demo/__posix__/shared.c"  -> shim,
      )

      Project.cSources(root, Some(Os.Android)).map(s => Project.basename(s.name)).sorted shouldBe
        List("log.c", "shared.c")
      Project.cSources(root, Some(Os.Linux)).map(s => Project.basename(s.name)) shouldBe List("shared.c")
      Project.cSources(root, Some(Os.MacOS)).map(s => Project.basename(s.name)) shouldBe List("shared.c")
    }

    // The same question asked of the tree that ships, which is the one the card was filed about: the
    // library's shims are POSIX and must reach a hosted build and no other.
    //
    // **`bare shouldBe empty` STOOD HERE AND WAS RIGHT UNTIL THE LIBRARY CARRIED C THAT IS NOT A
    // SHIM.** Every `.c` in `library/` sat under a `__posix__` folder, so a freestanding machine took
    // none of them and the claim could be written as *nothing at all*. `library/sysl/unicode` is the
    // Unicode Character Database, which is not a shim for anything and belongs on every target — so
    // the claim narrows to the one it always meant: **the POSIX shims stay off a bare machine, and
    // what does reach one is named.**
    //
    // The narrower form is also the stronger test. `empty` would go on passing if the selection
    // broke in the direction of taking nothing, and this cannot.
    "which is what keeps the library's own shims off a bare machine" in {
      val bare   = Std.cSources(Os.Freestanding).map(s => Project.basename(s.name)).sorted
      val hosted = Std.cSources(Os.Linux).map(s => Project.basename(s.name)).sorted

      bare shouldBe List("utf8proc.c")
      // `stdout.c` sits under `__hosted__` rather than `__posix__` -- it needs a C library's stdio,
      // not POSIX -- and a bare machine takes it no more than it takes the shims.
      hosted shouldBe List("clock.c", "dirent.c", "dirs.c", "meta.c", "net.c", "sleep.c", "spawn.c",
        "stackguard.c", "stat.c", "stdout.c", "termios.c", "utf8proc.c", "zone.c")

      // And macOS sees the same files, which is the deduplication itself: before this they were
      // two directories of identical copies, and a build could only ever have seen one of them.
      Std.cSources(Os.MacOS).map(s => Project.basename(s.name)).sorted shouldBe hosted
    }

    "names the element that is wrong, not the whole directory" in {
      val root = projectOf(
        "main.sysl"                        -> "print(1)\n",
        s"demo/__$hereSym,linxu__/x.sysl"  -> "module demo\n",
      )

      val (status, notes) = diagnostics(Config(command = "run", file = root))

      status should not be 0
      notes should include("linxu")
    }

    // The deliberate hole, pinned so that widening it is a decision rather than a drift. A processor
    // is not answerable to a walk that has only an operating system.
    "and refuses a processor, which is what '#if' is for" in {
      val root = projectOf("main.sysl" -> "print(1)\n", "demo/__aarch64__/x.sysl" -> "module demo\n")

      val (status, notes) = diagnostics(Config(command = "run", file = root))

      status should not be 0
      notes should include("aarch64")
    }

    // Two selectors that both answer is not itself the fault, and this is the case that says so:
    // every POSIX machine is hosted, so these two are true together always, and a module may
    // reasonably want to say both *needs an operating system* and *needs POSIX*.
    "so two that both answer are taken together, when what they hold differs" in {
      val root = projectOf(
        "demo/demo.sysl"           -> "module demo\n",
        "demo/__hosted__/any.c"    -> shim,
        "demo/__posix__/posix.c"   -> shim,
      )

      Project.cSources(root, Some(Os.Linux)).map(s => Project.basename(s.name)).sorted shouldBe
        List("any.c", "posix.c")
    }

    // The fault is the name, and it is one a selector naming a single operating system could never
    // produce. Two files of one name is a duplicate symbol reported a long way from the directories
    // that caused it, so it is refused where it can still be explained.
    "and refuses two selectors that both answer and hold one name between them" in {
      val root = projectOf(
        "main.sysl"                     -> "print(1)\n",
        "demo/demo.sysl"                -> "module demo\n",
        "demo/__posix__/shim.c"         -> shim,
        s"demo/$here/shim.c"            -> shim,
      )

      val thrown = the[SelectionError] thrownBy Project.cSources(root, Some(Target.default.os))

      thrown.getMessage should include("__posix__")
      thrown.getMessage should include(here)
    }

    // The other side of it: the pair a module actually wants is one that no machine takes together,
    // which is the shape `library/sysl/fs` will have when Windows arrives.
    "while a family and a machine outside it live together" in {
      val root = projectOf(
        "main.sysl"                    -> "print(demo.tag())\n",
        "demo/__posix__/impl.sysl"     -> "module demo\n\ntag() -> string = \"posix\"\n",
        "demo/__windows__/impl.sysl"   -> "module demo\n\ntag() -> string = \"windows\"\n",
      )

      ran(Config(command = "run", file = root)) shouldBe "posix\n"
    }
  }

  /** What a tree *offers* is a property of the tree rather than of the machine consuming it
   * (`reference/packages.md § What a dependency's modules are called`), so this one walk takes
   * every folder whatever target is asking.
   */
  "the modules a tree offers" - {

    "include one whose source is only under a per-OS directory" in {
      val root = projectOf(
        s"pkg/$other/only.sysl" -> "module pkg\n",
        "plain/plain.sysl"      -> "module plain\n",
      )

      // `$other` is the folder this machine does not select. A dependency's mount that came and went
      // with the target would resolve on one machine and not on another, and the collision check
      // under it would be checking a different table each time.
      Project.modules(root) shouldBe Set("pkg", "plain")
    }

    "and a malformed one is still refused rather than silently offering nothing" in {
      val root = projectOf("pkg/__linx__/only.sysl" -> "module pkg\n")

      // This walk tolerates a directory it cannot read, because a dependency's root may be anything
      // on disk. A mistake in the tree must not be swallowed by that tolerance.
      a[SelectionError] should be thrownBy Project.modules(root)
    }
  }
}
