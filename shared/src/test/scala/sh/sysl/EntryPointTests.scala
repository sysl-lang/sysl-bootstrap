package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `main` — the named form of the entry point, and the arguments it is handed (`reference/modules.md § Where a program starts`).
 *
 * A program starts in **one** place. Statements at the top of a file and a `main` are two ways of
 * writing that place, and a program writing both is refused: what a declared `main` adds is a
 * parameter list, which is the one thing a statement has no way to receive, so a program that wants
 * the arguments writes `main` and puts inside it what it would otherwise have written above. The only
 * signatures are `main()` and `main(args: []string)`.
 *
 * The arguments arrive as a slice of `string` and never as C's pair: the library's `args_of` walks
 * the vector, finds each run's terminator, validates its bytes and copies them into strings the
 * program owns. The zeroth is the executable's own path, which is what the platform passes and no
 * test can predict — so an assertion here is about `args[1..]` or about the count.
 */
class EntryPointTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  "a program starts in one place" - {
    // Two entry points is what a reader sees, because it is what it is — and the order between them
    // would be a rule to remember for a program that could have written one of them inside the other.
    "so statements above a main are refused, naming the file that already carries them" in {
      err("""main()
            |    print("main")
            |
            |print("statements")
            |""".stripMargin) should include("this 'main' is a second")
    }

    "a main with nothing above it still runs" in {
      run("""main()
            |    print("only main")
            |""".stripMargin) shouldBe "only main\n"
    }

    "a program with no main is what it always was" in {
      run("""print("no main here")""") shouldBe "no main here\n"
    }

    // A `val` beside a `main` is a declaration, so the file carries no statements and there is no
    // second entry point: it is storage filled before anything runs (`reference/modules.md § val — a thing`), which is the one
    // ordering left now that there is one place to start.
    "a val beside it is filled before it runs" in {
      run("""val table: [3]int = [1, 2, 3]
            |
            |main()
            |    print("main", table[0], table[2])
            |""".stripMargin) shouldBe "main 1 3\n"
    }

    // A file whose only running lines are `var`s is a body only for want of anything else to be the
    // beginning. A declared `main` is that beginning, so the `var` beside it is storage the module
    // owns — as a `val` beside it is, and as a `var` in a file with a `module` line is. This is the
    // shape a first benchmark takes: a counter, the functions that bump it, and a `main` calling them.
    "a var beside it is the module's storage, which the functions and main share" in {
      run("""var counter: int = 0
            |
            |bump() =
            |    counter += 1
            |
            |twice() =
            |    bump()
            |    bump()
            |
            |main() =
            |    twice()
            |    print(counter)
            |""".stripMargin) shouldBe "2\n"
    }

    // Read as a body, the same program was refused over its own `main` ("a second"), and every
    // function touching the counter was nested out of `main`'s reach and reported as an undefined
    // name. What is wrong with it untyped is the one thing module storage asks, on the `var`'s line,
    // and nothing is reported after it.
    "and untyped, it is refused on its own line for the type and for nothing else" in {
      val e = err("""var counter = 0
                    |
                    |bump() =
                    |    counter += 1
                    |
                    |twice() =
                    |    bump()
                    |    bump()
                    |
                    |main() =
                    |    twice()
                    |    print(counter)
                    |""".stripMargin)

      e should include("'counter' is module storage, and module storage states its type — write 'counter: T'")
      e should include("<input>:1:1")
      e should not include "undefined"
      e should not include "this 'main' is a second"
      e.linesIterator.count(_.startsWith("error")) shouldBe 1
    }

    // The same recovery holds wherever module storage is declared untyped: the missing type is the
    // mistake, and the uses of the name are not.
    "a file with a module line reports an untyped var once, with no undefined name after it" in {
      val e = errIn(("app", "app.sysl", """module app
                                          |
                                          |var counter = 0
                                          |
                                          |val limit = 3
                                          |
                                          |main() =
                                          |    counter += limit
                                          |    print(counter)
                                          |""".stripMargin))

      e should include("'counter' is module storage, and module storage states its type")
      e should include("a module-level 'val' states its type, so 'limit' needs one")
      e should not include "undefined"
      e.linesIterator.count(_.startsWith("error")) shouldBe 2
    }

    // With no `main`, a lone `var` is still what a one-file script's first line has always been.
    "while with no main a lone var is still the script's local" in {
      run("""var counter = 0
            |
            |bump() =
            |    counter += 1
            |
            |bump()
            |print(counter)
            |""".stripMargin) shouldBe "1\n"
    }

    // A file that runs a real statement is a body however a `main` is written, so its `var` is a local
    // and the `main` is the second beginning, as ever.
    "while a statement beside main is still reported on the main" in {
      err("""var count = 0
            |
            |print(count)
            |
            |main()
            |    print("main")
            |""".stripMargin) should include("this 'main' is a second")
    }

    // `main` is renamed on the way out, because the symbol it is written as is the one the platform
    // starts the program at. Calling it from the program too is what shows the definition and the
    // call site agree about that rename.
    "a program may call its own main, which then runs twice" in {
      runWith("""main(args: []string)
                |    print("ran")
                |    if args.len > 0 then main([])
                |""".stripMargin, "once") shouldBe "ran\nran\n"
    }

    "and a main declared in another module is still the program's" in {
      runOf(
        "app.sysl" -> """module app
                        |
                        |main()
                        |    print("app's main", app.tag)
                        |
                        |val tag: int = 7
                        |""".stripMargin,
        "start.sysl" -> """import app.*
                          |""".stripMargin,
      ) shouldBe "app's main 7\n"
    }

    // With no statements anywhere, the file the entry point is *read* in is the root (`reference/modules.md § Where a program starts`) while
    // this `main` is a member of `app` — so a program of one module and no statements is the case a
    // rule about the entry file's own module would have missed.
    "even when nothing in the program carries a statement at all" in {
      runIn(("app", "only.sysl", """module app
                                   |
                                   |main()
                                   |    print("a module's main, and nothing else")
                                   |""".stripMargin)) shouldBe "a module's main, and nothing else\n"
    }
  }

  "the arguments arrive as a slice of strings" - {
    "the zeroth is the program itself, so a bare run has one" in {
      runWith("""main(args: []string)
                |    print(args.len)
                |""".stripMargin) shouldBe "1\n"
    }

    "each word given is one element after it" in {
      runWith("""main(args: []string)
                |    print(args.len)
                |    for a in args[1..]
                |        print(a)
                |""".stripMargin, "alpha", "beta") shouldBe "3\nalpha\nbeta\n"
    }

    "an argument that looks like an option is an argument" in {
      runWith("""main(args: []string)
                |    for a in args[1..]
                |        print(a)
                |""".stripMargin, "-v", "--target", "-") shouldBe "-v\n--target\n-\n"
    }

    "an empty argument is an empty string" in {
      runWith("""main(args: []string)
                |    print(args.len, args[1].len)
                |""".stripMargin, "") shouldBe "2 0\n"
    }

    // The bytes are validated and copied, so a multi-byte argument has the length its own encoding
    // gives it — the count is bytes, as `s.len` is everywhere (`04`) — while its characters are one
    // fewer, which is what shows the run reached the string whole rather than a byte at a time.
    "a non-ASCII argument arrives as its UTF-8" in {
      runWith("""main(args: []string)
                |    print(args[1], args[1].len)
                |""".stripMargin, "héllo") shouldBe "héllo 6\n"
    }

    "and its characters are decodable" in {
      runWith("""main(args: []string)
                |    var n = 0
                |    for c in args[1].chars do n += 1
                |    print(n)
                |""".stripMargin, "héllo") shouldBe "5\n"
    }

    // The conversion gathers into a `Buf`, which starts at eight elements and doubles. Twenty
    // arguments is past two of those growths, so a test with three would not see a copy go wrong.
    "twenty arguments all arrive, past two growths of the buffer" in {
      val src = """main(args: []string)
                  |    print(args.len)
                  |    var total = 0usize
                  |    for a in args[1..] do total += a.len
                  |    print(total)
                  |""".stripMargin

      runWith(src, (1 to 20).map(i => s"arg$i")*) shouldBe "21\n91\n"
    }

    "an argument outlives the vector it came from" in {
      runWith("""import sysl.text.str_builder
                |
                |keep(args: []string) -> string
                |    var b = str_builder()
                |    for a in args[1..]
                |        b.push(a)
                |        b.push("/")
                |    b.finish()
                |
                |main(args: []string)
                |    var s = keep(args)
                |    print(s)
                |""".stripMargin, "a", "b", "c") shouldBe "a/b/c/\n"
    }

    "the slice is an ordinary one, so it can be indexed, sliced and iterated" in {
      runWith("""main(args: []string)
                |    print(args[2])
                |    for a in args[2..<4] do print(a)
                |    print(args[1..].len)
                |""".stripMargin, "one", "two", "three", "four") shouldBe "two\ntwo\nthree\n4\n"
    }
  }

  "what a main may not be" - {
    "a parameter that is not a slice of strings" in {
      err("""main(n: int)
            |    print(n)
            |""".stripMargin) should include("'main' takes either nothing or one '[]string'")
    }

    "a second parameter beside the arguments" in {
      err("""main(args: []string, verbose: bool)
            |    print(args.len, verbose)
            |""".stripMargin) should include("not ([]string, bool)")
    }

    "a slice of something else" in {
      err("""main(args: []int)
            |    print(args.len)
            |""".stripMargin) should include("not ([]int)")
    }

    "a result, which would be an exit status" in {
      err("""main() -> int
            |    0
            |""".stripMargin) should include("a program's exit status is not something a signature can say")
    }

    // **And the refusal names the answer.** A reader here wants to choose a status; `exit` is how,
    // and has been since before this rule was written. Saying only what a signature may not do sent
    // somebody looking for a language feature that was already built, which is card `0326` — filed
    // to ask for `exit` by an author who had read this message.
    "and it says how a program does choose one, which is the half it was missing" in {
      err("""main() -> int
            |    0
            |""".stripMargin) should include("'exit(code)' is how a program chooses its own")
    }

    "type parameters the platform could not supply" in {
      err("""main[T]()
            |    print("hi")
            |""".stripMargin) should include("no type arguments to give it")
    }

    "a list it reads for itself" in {
      err("""main(args: []string, ...)
            |    print(args.len)
            |""".stripMargin) should include("not a list it reads")
    }

    // Two in one module is caught by the ordinary duplicate rule; two in two modules is not, and is
    // exactly the case where nothing else in the language would collide.
    "a second main, in a module of its own" in {
      val msg = errOf(
        "a.sysl" -> """main()
                      |    print("a")
                      |""".stripMargin,
        "b.sysl" -> """module lib
                      |
                      |main()
                      |    print("b")
                      |""".stripMargin,
      )

      msg should include("'main' is where a program starts, so there is one")
      msg should include("main already declares it")
    }

    // Both halves of an `extern` are refused, and for different reasons: the symbol because this
    // program defines it, the name because a `main` the program does not start at would read as
    // though it were the one it does.
    "an extern that links to the symbol" in {
      err("""extern "main" other() -> int
            |
            |print(other())
            |""".stripMargin) should include("an 'extern' may not name that symbol")
    }

    "and an extern that merely takes the name" in {
      err("""extern "puts" main(s: *u8) -> int
            |
            |main(c"hi")
            |""".stripMargin) should include("not a name an 'extern' may take")
    }
  }

  "an argument that is not text stops the program" - {
    // Argv bytes cannot be made ill-formed through a test harness — a `String` handed to the process
    // is encoded on the way out — so the conversion is called directly, with a vector built here.
    // `0xC3` opens a two-byte sequence and `0x28` cannot continue one.
    val bad = """var run: [3]u8 = [0xC3u8, 0x28u8, 0u8]
                |var vec: [1]*u8 = [&run[0]]
                |""".stripMargin

    "with the offset of the byte that made it ill-formed" in {
      panics(bad + "var args = sysl.args.args_of(1i32, &vec[0])\nprint(args.len)\n",
        "panic: command-line argument 0 is not UTF-8 at byte 0")
    }

    "and it reports which argument, not just that one was wrong" in {
      panics("""var ok: [3]u8 = [0x68u8, 0x69u8, 0u8]
               |""".stripMargin + bad +
        """var vec2: [2]*u8 = [&ok[0], &run[0]]
          |var args = sysl.args.args_of(2i32, &vec2[0])
          |print(args.len)
          |""".stripMargin,
        "command-line argument 1 is not UTF-8")
    }

    "while a well-formed vector converts" in {
      run("""var a: [3]u8 = [0x68u8, 0x69u8, 0u8]
            |var vec: [1]*u8 = [&a[0]]
            |var args = sysl.args.args_of(1i32, &vec[0])
            |
            |print(args.len, args[0])
            |""".stripMargin) shouldBe "1 hi\n"
    }

    "and an empty vector converts to an empty slice" in {
      run("""var vec: [1]*u8 = [c"unused"]
            |var args = sysl.args.args_of(0i32, &vec[0])
            |
            |print(args.len)
            |""".stripMargin) shouldBe "0\n"
    }
  }

  // Everything a `main` is besides the entry point, which is: an ordinary function. Each of these was
  // probed against the chapter's claim that it is one, and each is pinned rather than assumed.
  "otherwise it is a function like any other" - {
    "it may be private, which is about who may name it and not about who calls it" in {
      run("""private main()
            |    print("private main ran")
            |""".stripMargin) shouldBe "private main ran\n"
    }

    // `reference/modules.md § Where a program starts` says a program that must choose its exit status calls `exit`. This is that program, and
    // a `-> never` result is accepted because `never` is not a value the wrapper would have to place.
    "it may diverge, which is how a program chooses its status today" in {
      exitsWith("""main() -> never
                  |    print("about to stop")
                  |    exit(3)
                  |""".stripMargin, 3)
    }

    "its contracts are checked" in {
      run("""main(args: []string)
            |    require args.len > 0, "the platform passes the program's own path"
            |
            |    print("checked")
            |""".stripMargin) shouldBe "checked\n"
    }

    // A contract traps rather than printing: the message is for whoever reads the line, and what a
    // trap leaves is the status.
    "and a broken one stops the program before its body runs" in {
      exits("""main(args: []string)
              |    require args.len > 5, "more arguments than a bare run has"
              |
              |    print("this never reaches the pipe")
              |""".stripMargin)
    }

    // The rename has to hold at three places at once — the definition, a direct call, and the `call`
    // a closure's wrapper struct emits — and a callable is the one that reaches the third.
    // The `main(args)` form, because the argument-free one passed to itself would not terminate:
    // the recursion stops on the empty vector this hands it the second time round.
    "it may be passed as a callable" in {
      runWith("""apply(f: []string -> unit, a: []string)
                |    f(a)
                |
                |main(args: []string)
                |    print("ran")
                |    if args.len > 0 then apply(main, [])
                |""".stripMargin, "once") shouldBe "ran\nran\n"
    }

    "it may recurse, on a slice of its own arguments" in {
      runWith("""main(args: []string)
                |    if args.len > 1
                |        print("descending", args.len)
                |        main(args[1..])
                |    else
                |        print("done")
                |""".stripMargin, "a", "b") shouldBe "descending 3\ndescending 2\ndone\n"
    }

    "its parameter is a binding it may write to" in {
      runWith("""main(args: []string)
                |    args = args[1..]
                |    print(args.len)
                |""".stripMargin, "a", "b") shouldBe "2\n"
    }

    // An array whose view leaves the body is moved to the heap (`05`), and `main` is a function, so
    // nothing about promotion is different inside one.
    "an escaping view of its local array is promoted, not refused" in {
      run("""keep(v: []int) -> []int = v
            |
            |main()
            |    var a: [4]int = [1, 2, 3, 4]
            |    var v = keep(a[..])
            |
            |    print(v.len, v[3])
            |""".stripMargin) shouldBe "4 4\n"
    }

    "and the arguments survive being churned through" in {
      runWith("""import sysl.text.str_builder
                |
                |main(args: []string)
                |    var total = 0usize
                |
                |    for i in 0..<100000
                |        var b = str_builder()
                |
                |        for a in args
                |            b.push(a)
                |
                |        total += b.finish().len
                |
                |    print(total > 0usize)
                |""".stripMargin, "one", "two") shouldBe "true\n"
    }
  }

  // The name is reserved for a *top-level* function, because that is the only place the platform
  // could call one from. Everything else that can be called `main` goes on meaning what it meant.
  "what the name does not reserve" - {
    "a member of a type may be called main" in {
      run("""struct Point
            |    x: int
            |
            |    main(self)
            |        print("a member named main", self.x)
            |end Point
            |
            |var p = Point(7)
            |
            |p.main()
            |print("statements")
            |""".stripMargin) shouldBe "a member named main 7\nstatements\n"
    }

    "and so may a function nested inside another" in {
      run("""outer()
            |    main()
            |        print("a nested main")
            |
            |    main()
            |
            |outer()
            |print("statements")
            |""".stripMargin) shouldBe "a nested main\nstatements\n"
    }

    // The wrapper's own parameters are `%argc` and `%argv`; a `var` of either name inside `main` is a
    // slot called `%argc.addr`, so the two cannot be confused — but only a test says so.
    "a var inside main may be called argc or argv" in {
      runWith("""main(args: []string)
                |    var argc = 99
                |    var argv = "not the vector"
                |
                |    print(argc, argv)
                |    print("main sees", args.len)
                |""".stripMargin, "one") shouldBe "99 not the vector\nmain sees 2\n"
    }

    // The conversion used to be a name a program could not take, because the prelude declared it in
    // the same root module a headerless program is in. It is `sysl.args.args_of` now — a submodule's
    // name, so it is not even among the ones a file gets for free — and the entry point goes on
    // calling the library's, which it names by key and never by what the word resolves to.
    "while the conversion is a word the program may spend on something else" in {
      runWith("""args_of(n: int) -> string = "mine"
                |
                |main(args: []string)
                |    print(args_of(1), args.len)
                |""".stripMargin, "one", "two") shouldBe "mine 3\n"
    }

    // Both meanings at once, which is the thing neither half says on its own: the shadowed word is
    // the program's, the path still reaches the library's, and the entry point — which named it by
    // key — agrees with the path rather than with the word. A program's own `args_of` takes an int,
    // so resolution picking the wrong one either way would not type-check.
    //
    // The vector is a local of `main` because a top-level `var` is a local of the *entry point* and
    // `main` cannot see it (§7) — nothing to do with the conversion, but it is what a first attempt
    // writes.
    "and the library's is still there under its path, beside the program's own" in {
      runWith("""args_of(n: int) -> string = "mine"
                |
                |main(args: []string)
                |    var vec: [1]*u8 = [c"direct"]
                |
                |    print(args_of(1), sysl.args.args_of(1i32, &vec[0])[0], args.len)
                |""".stripMargin, "one", "two") shouldBe "mine direct 3\n"
    }
  }

  "what is emitted" - {
    "the entry point takes the pair the platform passes" in {
      ir("""print("hi")""") should include("define i32 @main(i32 %argc, ptr %argv)")
    }

    "a program's own main is emitted under a reserved symbol" in {
      val out = ir("""main()
                     |    print("hi")
                     |""".stripMargin)

      out should include("define void @$$main()")
      out should include("call void @$$main()")
    }

    "a main taking arguments is called with the conversion's result" in {
      val out = mainOf(ir("""main(args: []string)
                            |    print(args.len)
                            |""".stripMargin))

      out should include(s"@${Library.key("args_of")}(i32 %argc, ptr %argv)")
      out should include("call void @$$main(")
    }

    // The conversion reaches `from_utf8`, `Buf` and the printing a panic needs, so leaving it in a
    // program that never asks for its arguments would be the whole of that surface for nothing.
    //
    // These two are deliberately the *bare* name where the assertion above is the qualified one: a
    // negative reads better wide, since `sysl$args_of` contains `args_of` and so either spelling
    // trips it. Narrowing them to `lib(...)` would leave a stray unqualified emission unnoticed.
    "a program with no main carries none of the conversion" in {
      ir("""print("hi")""") should not include "args_of"
    }

    "and neither does a main that takes no arguments" in {
      ir("""main()
           |    print("hi")
           |""".stripMargin) should not include "args_of"
    }
  }
}
