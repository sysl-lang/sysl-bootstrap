package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@needs(...)` — a **declaration** naming what reaching it requires
 * (`reference/modules.md § A declaration may name what reaching it needs`).
 *
 * It is the finer half of the capability clause. A file header's `@requires(...)` is about the
 * **module** and is checked once against the target; this is about one declaration and is checked at
 * the **call**, in the caller's module, which is where the line a reader can change is.
 *
 * **The declaration it exists for is `extern`**, and that is what most of this suite is about. Every
 * other declaration has a body the compiler reads — `NoAlloc` finds an allocation by looking — so an
 * `extern` was the one route by which a module that had given up an environment capability could
 * reach `open()`. Nothing but the declaration itself can close that.
 */
class DeclCapabilityTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "an extern may name the capability calling it needs" - {

    "and a module that gave that capability up may not reach it" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "main.sysl"  -> "@no_os\n\nprint(sys.pid())\n",
      ) should include("this reaches 'sys.pid', which needs 'os', and this module declared '@no_os'")
    }

    "while a module that said nothing reaches it exactly as before" in {
      irOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "main.sysl"  -> "print(sys.pid())\n",
      ) should include("declare i32 @getpid()")
    }

    // The half with 46 files behind it: `no alloc` is checked at every construction that makes heap
    // storage, and an `extern` that allocates walks straight past that.
    "the heap is the same rule, and it is the case a module clause could not see" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(heap)\nextern \"malloc\" grab(n: usize) -> *u8\n",
        "main.sysl"  -> "@no_alloc\n\nval p = sys.grab(8)\nprint(p == null)\n",
      ) should include("this reaches 'sys.grab', which needs 'heap'")
    }

    "and a `private` extern is reached through the wrapper that exports it, which is where the caret goes" in {
      errOf(
        "sys/a.sysl" -> ("module sys\n\n@needs(os)\nprivate extern \"getpid\" c_pid() -> int\n\n" +
          "pid() -> int = c_pid()\n"),
        "main.sysl"  -> "@no_os\n\nprint(sys.pid())\n",
      ) should include("which needs 'os'")
    }
  }

  "the requirement is transitive, because reaching is" - {

    "through a function of the caller's own" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "main.sysl"  -> "@no_os\n\nask() -> int = sys.pid()\n\nprint(ask())\n",
      ) should include("which needs 'os'")
    }

    "and through a third module that has the capability itself" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "mid/a.sysl" -> "module mid\n\nask() -> int = sys.pid()\n",
        "main.sysl"  -> "@no_os\n\nprint(mid.ask())\n",
      ) should include("which needs 'os'")
    }
  }

  "a capability implies what it rests on" - {

    // POSIX needs an operating system under it, so a declaration needing `posix` needs `os` — and a
    // module that gave up only `os` is refused, naming the capability it actually lacks.
    "so `@needs(posix)` is out of reach of a module that gave up `os` alone" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(posix)\nextern \"getpid\" pid() -> int\n",
        "main.sysl"  -> "@no_os\n\nprint(sys.pid())\n",
      ) should include("which needs 'os'")
    }
  }

  "an ordinary function may carry it too" - {

    "which is the granularity a module clause could not give a library" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nnow() -> int = 7\n\nplain() -> int = 3\n",
        "main.sysl"  -> "@no_os\n\nprint(sys.now())\n",
      ) should include("which needs 'os'")
    }

    "and the declaration beside it, which said nothing, is reached freely" in {
      runOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nnow() -> int = 7\n\nplain() -> int = 3\n",
        "main.sysl"  -> "@no_os\n\nprint(sys.plain())\n",
      ) shouldBe "3\n"
    }
  }

  /** A module's **tests** answer to their own clause here, exactly as they do for the allocator
   * (`reference/modules.md § A @tests file states its own capabilities`).
   *
   * The clause a module writes is a promise about what **ships**, and scaffolding does not ship — so
   * a `@tests` file may take back what the module gave up. `NoAlloc` implemented that from the day
   * the table existed; this check read the module's clause for every capability and every
   * declaration, so a module that gave up `os` in order to say something true about what it ships
   * could not test itself against a real filesystem. Card `0318`.
   *
   * **Only the module's half moves. The target's does not**, which is the asymmetry `noAllocTests`
   * already carries: a file may lift a promise its author made, and cannot lift what the machine
   * never had.
   */
  "a '@tests' file's clause is what its tests are held to" - {

    "so a test may reach what the module gave up, since what it declares does not ship" in {
      runOf(
        "sys/a.sysl" -> "module sys\n@no_os\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "sys/tests.sysl" -> ("module sys\n@tests\n@requires(os)\n\n@test\n" +
          "reaches_the_machine() =\n    assert(pid() > 0)\n"),
        "main.sysl" -> "print(1)\n",
      ) shouldBe "1\n"
    }

    // The half that must not move with it: a shipping file of the same module is refused exactly as
    // it was, so what the `@tests` file lifted is lifted for scaffolding and for nothing else.
    "while a shipping declaration of that module is refused as it always was" in {
      errOf(
        "sys/a.sysl" -> ("module sys\n@no_os\n\n@needs(os)\nextern \"getpid\" pid() -> int\n\n" +
          "ships() -> int = pid()\n"),
        "sys/tests.sysl" -> "module sys\n@tests\n@requires(os)\n\nhelper() -> int = 2\n",
        "main.sysl" -> "print(1)\n",
      ) should include("this reaches 'sys.pid', which needs 'os', and 'sys' declared '@no_os'")
    }

    // The silent case has to keep meaning what it meant, which is the reason `testNarrows` falls
    // back rather than being a table of its own.
    "and a test file that says nothing is held to its module's clause" in {
      errOf(
        "sys/a.sysl" -> "module sys\n@no_os\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "sys/tests.sysl" -> ("module sys\n@tests\n\n@test\n" +
          "reaches_the_machine() =\n    assert(pid() > 0)\n"),
        "main.sysl" -> "print(1)\n",
      ) should include("which needs 'os', and 'sys' declared '@no_os'")
    }

    // `why` moves with `lacks` or the diagnostic sends a reader to delete a line that is not there.
    "a test refused under its OWN narrowing is told which file declared it" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\n@needs(os)\nextern \"getpid\" pid() -> int\n",
        "sys/tests.sysl" -> ("module sys\n@tests\n@no_os\n\n@test\n" +
          "reaches_the_machine() =\n    assert(pid() > 0)\n"),
        "main.sysl" -> "print(1)\n",
      ) should include("the '@tests' file of 'sys' declared '@no_os'")
    }

    // A test is scaffolding wherever it is written, which is the set `checkNoAlloc` builds and the
    // set this now builds — a `@test` beside what it tests answers to the tests' clause.
    "a '@test' in an ordinary file answers to the tests' clause too" in {
      runOf(
        "sys/a.sysl" -> ("module sys\n@no_os\n\n@needs(os)\nextern \"getpid\" pid() -> int\n\n" +
          "@test\nbeside_what_it_tests() =\n    assert(pid() > 0)\n"),
        "sys/tests.sysl" -> "module sys\n@tests\n@requires(os)\n\nhelper() -> int = 2\n",
        "main.sysl" -> "print(1)\n",
      ) shouldBe "1\n"
    }
  }

  "what the annotation may say" - {

    "a capability nothing has heard of is refused, in the words a file header's is" in {
      err("@needs(sockets)\nf() -> int = 1\n\nprint(f())\n") should
        include("no capability is called 'sockets'")
    }

    // The mistake somebody makes having just read `@no_alloc` beside it: `alloc` is what a module
    // *does*, and a requirement names the facility.
    "and the narrowing's own word is answered by naming the facility" in {
      err("@needs(alloc)\nf() -> int = 1\n\nprint(f())\n") should
        include("a '@needs' names the facility, so this is '@needs(heap)'")
    }

    "the parentheses are mandatory and may not be empty" in {
      err("@needs\nf() -> int = 1\n\nprint(f())\n") should
        include("names the capabilities reaching this declaration requires, in parentheses")
    }

    "so is the list inside them" in {
      err("@needs()\nf() -> int = 1\n\nprint(f())\n") should
        include("There is no empty form")
    }

    "and it marks a function or an 'extern', not a struct" in {
      err("@needs(os)\nstruct P\n    x: int\n\nprint(1)\n") should
        include("so it marks a function or an 'extern'")
    }
  }

  /** What a `@needs(os)` declaration's **body** reaches is charged to whoever reaches the
   * declaration, and to nobody else (`reference/modules.md § A declaration may name what reaching it
   * needs`: *"what it says is charged to whoever reaches it rather than to whoever holds it"*).
   *
   * Until this the body counted twice. Its reference into `sysl.fs` was an edge of the module graph
   * like any other, so the whole module came to require `os` — and a `@no_os` program importing only
   * the declaration beside it was refused at the import, which is the granularity the annotation
   * exists to give, taken away again by the module graph.
   */
  "a '@needs' body is charged to its callers, not to its module" - {

    // Every diagnostic is the call's refusal of `who`. The declaration check walks once per
    // capability, so a `@needs(posix)` declaration reached from a module without `os` is refused
    // once for each; what must not be there is any refusal that is not about the call.
    def onlyAtTheCall(e: String, who: String): Unit =
      withClue(e) {
        val each = e.split("error:").toList.drop(1)

        each should not be empty
        all(each) should include(s"this reaches '$who', which needs ")
      }

    // `posix` rather than `os`: `sysl.fs` reaches the POSIX modules under it on every hosted target,
    // so its requirement as the module graph counts it is both, and a body covers what its
    // annotation names. `posix` implies `os`, so this names both.
    val sys = "sys/a.sysl" -> ("module sys\n\n@needs(posix)\nnow() -> bool = sysl.fs.exists(\"/\")\n\n" +
      "plain() -> int = 3\n")

    "so a module that gave os up imports the declaration beside it" in {
      runOf(sys, "main.sysl" -> "@no_os\n\nimport sys.plain\n\nprint(plain())\n") shouldBe "3\n"
    }

    // The refusal that remains is the call's, once, at the call — not that one with the module's
    // stacked on top at the import, which named a module the reader had every right to import.
    "and calling the declaration is refused at the call only, and not at the import" in {
      val e = errOf(sys, "main.sysl" -> "@no_os\n\nimport sys.now\n\nprint(now())\n")

      e should include("this reaches 'sys.now', which needs 'os', and this module declared '@no_os'")
      e should include("main.sysl:5:7")
      e shouldNot include("which requires 'os'")
      onlyAtTheCall(e, "sys.now")
    }

    // `reference/modules.md`: "It is **transitive**, because reaching is: a module that gave a
    // capability up may not arrive at such a declaration through a third that has it." So a function
    // that said nothing and calls one that did inherits the need: its callers are refused, at their
    // call, and the module it sits in is still importable for everything else.
    "an unannotated function calling it passes the need on to ITS callers" in {
      val relay = "sys/a.sysl" -> ("module sys\n\n@needs(posix)\nnow() -> bool = sysl.fs.exists(\"/\")\n\n" +
        "relay() -> bool = now()\n\nplain() -> int = 3\n")

      val e = errOf(relay, "main.sysl" -> "@no_os\n\nimport sys.relay\n\nprint(relay())\n")

      e should include("this reaches 'sys.now', which needs 'os', and this module declared '@no_os'")
      e should include("main.sysl:5:7")
      e shouldNot include("which requires 'os'")
      onlyAtTheCall(e, "sys.now")

      runOf(relay, "main.sysl" -> "@no_os\n\nimport sys.plain\n\nprint(plain())\n") shouldBe "3\n"
      runOf(relay, "main.sysl" -> "import sys.relay\n\nprint(relay())\n") shouldBe "true\n"
    }

    // The floor stays the floor: a body that did NOT say it needs `os` and reaches `sysl.fs` makes
    // its module require it, exactly as before, so the import is still where a '@no_os' program hears.
    "while an unannotated body that reaches sysl.fs still costs the whole module" in {
      errOf(
        "sys/a.sysl" -> "module sys\n\nnow() -> bool = sysl.fs.exists(\"/\")\n\nplain() -> int = 3\n",
        "main.sysl"  -> "@no_os\n\nimport sys.plain\n\nprint(plain())\n",
      ) should include("this reaches 'sys', which requires 'os', and this module declared 'no os'")
    }

    // What the declaration covers is what it NAMES. A '@needs(heap)' body says nothing about `os`,
    // so its reference into `sysl.fs` is still the module's.
    "and a body covers only what its own annotation names" in {
      errOf(
        "sys/a.sysl" -> ("module sys\n\n@needs(heap)\nnow() -> bool = sysl.fs.exists(\"/\")\n\n" +
          "plain() -> int = 3\n"),
        "main.sysl"  -> "@no_os\n\nimport sys.plain\n\nprint(plain())\n",
      ) should include("this reaches 'sys', which requires 'os'")
    }

    // A closure inside such a body is reached only through it, so it is covered with it.
    "and a closure written inside the body is covered with it" in {
      runOf(
        "sys/a.sysl" -> ("module sys\n\n@needs(posix)\nnow() -> bool\n" +
          "    val ask = (p: string) -> sysl.fs.exists(p)\n    ask(\"/\")\n\nplain() -> int = 3\n"),
        "main.sysl"  -> "@no_os\n\nimport sys.plain\n\nprint(plain())\n",
      ) shouldBe "3\n"
    }
  }

  /** A declaration that dropped the annotation on the way through an archive would be a capability
    * requirement that held inside the library and nowhere else — the check it asks for is made at
    * the **call**, and the calls an artifact is read for are all in the consumer.
    */
  "it travels in a library artifact, because the calls it governs are in the consumer" in {
    val src = "module sys\n\n@needs(os)\nextern \"getpid\" pid() -> int\n\n" +
      "@needs(heap, posix)\nnow() -> int = 1\n"

    val parsed = SyslParser.parse(Source("<t>", src)) match
      case Right(p) => p
      case Left(e)  => fail(s"the fixture does not parse: $e")

    val back = AstCodec.decode(AstCodec.encode(List(parsed)), Map.empty) match
      case Right(ps) => ps.head
      case Left(e)   => fail(s"decode failed: $e")

    back.body.collect { case e: ExternDecl => e.needs } shouldBe List(List("os"))
    back.body.collect { case f: FuncDecl => f.needs } shouldBe List(List("heap", "posix"))
  }
}
