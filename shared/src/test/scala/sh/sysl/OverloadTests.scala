package sh.sysl

import io.github.edadma.cross_platform.*

/** Two declarations of one name, and which of them a use means — `reference/declarations.md §
 * Overloading`.
 *
 * The claim this file holds is that **a name may stand for several functions and every use of it
 * still names exactly one**. That is checked by running rather than by reading IR wherever it can
 * be: each declaration answers differently, so a call reaching the wrong one prints the wrong thing
 * rather than failing to compile, and no accident produces the right answer.
 *
 * The refusals are the other half and they divide in two. A pair no call could tell apart is refused
 * **at the declaration**, because the mistake is in the pair and reporting it at each use would
 * report one mistake many times. A use that fits none or several is refused **at the use**, because
 * that is where the mistake is.
 */
class OverloadTests extends LibraryCliSupport with RunSupport with CodegenSupport {

  "a name may be declared more than once" - {
    // Arity is the plainest case and the one a binding wants: SDL_ttf renders text at one colour or
    // at two, and the pair reads as one operation with an option rather than as `render_shaded`.
    "and a call chooses by how many arguments it passes" in {
      run("""paint(x: int) -> string = s"one $x"
            |paint(x: int, y: int) -> string = s"two $x $y"
            |
            |print(paint(1))
            |print(paint(1, 2))""".stripMargin) shouldBe "one 1\ntwo 1 2\n"
    }

    "or by what type they are" in {
      run("""show(x: int) -> string = s"int $x"
            |show(x: string) -> string = s"str $x"
            |show(x: bool) -> string = s"bool $x"
            |
            |print(show(1))
            |print(show("a"))
            |print(show(true))""".stripMargin) shouldBe "int 1\nstr a\nbool true\n"
    }

    // The case `13` records as the reason `sysl.math` could not be a module of free functions: one
    // name over two float widths. It is the tie-break on exactness that decides this — a literal fits
    // both, and its own type is what says which was meant.
    "including two widths of one number, told apart by the literal's own type" in {
      run("""width(x: int) -> string = "int"
            |width(x: i64) -> string = "i64"
            |
            |print(width(1))
            |print(width(1i64))""".stripMargin) shouldBe "int\ni64\n"
    }

    // Three or more, since a set of two could be told apart by a rule that compared one against the
    // other rather than by choosing among candidates.
    "and there may be more than two of them" in {
      run("""at(a: int) -> string = "1"
            |at(a: int, b: int) -> string = "2"
            |at(a: int, b: int, c: int) -> string = "3"
            |
            |print(at(1) + at(1, 2) + at(1, 2, 3))""".stripMargin) shouldBe "123\n"
    }

    // Overloads declared across two files of one module, which is what makes this a fact about the
    // module rather than about a file. A reader adding a width to a library adds a file.
    "across the files of one module, which is where a name lives" in {
      runIn(
        ("", "main.sysl", "print(m.pick(1))\nprint(m.pick(\"a\"))"),
        ("m", "a.sysl", "module m\n\npick(x: int) -> string = \"int\""),
        ("m", "b.sysl", "module m\n\npick(x: string) -> string = \"str\""),
      ) shouldBe "int\nstr\n"
    }

    // A name declared once must take exactly the path it always did — the fast path is a property
    // worth pinning, since a regression in it would be a slowdown rather than a failure.
    "while a name declared once is unchanged, defaults and all" in {
      run("""only(a: int, b: int = 10) -> int = a + b
            |print(str(only(1)), str(only(1, 2)))""".stripMargin) shouldBe "11 3\n"
    }
  }

  "an address is of one of them, read off the type the context wants" - {
    "which the expected type settles" in {
      run("""add(a: int) -> int = a + 1
            |add(a: int, b: int) -> int = a + b
            |
            |val one: *extern(int) -> int = &add
            |val two: *extern(int, int) -> int = &add
            |
            |print(str(one(41)), str(two(6, 7)))""".stripMargin) shouldBe "42 13\n"
    }

    // The declaration order must not decide it, which is the thing an implementation taking the
    // first candidate would pass the test above on.
    "whichever order they were declared in" in {
      run("""add(a: int, b: int) -> int = a + b
            |add(a: int) -> int = a + 1
            |
            |val one: *extern(int) -> int = &add
            |print(str(one(41)))""".stripMargin) shouldBe "42\n"
    }

    "and with no expected type to read, it says so rather than choosing" in {
      err("""add(a: int) -> int = a + 1
            |add(a: int, b: int) -> int = a + b
            |
            |var f = &add""".stripMargin) should include("read off the type the context wants")
    }
  }

  "a bare name used as a callable chooses the same way" in {
    // Same arity on both, so only the parameter's *type* can have decided it — an implementation
    // taking the first candidate reaches `tag(s: string)` and fails.
    run("""tag(s: string) -> int = 99
          |tag(a: int) -> int = a + 1
          |
          |apply(f: int -> int, x: int) -> int = f(x)
          |
          |print(str(apply(tag, 41)))""".stripMargin) shouldBe "42\n"
  }

  "an 'extern' overloads, and the symbol is what keeps them apart" - {
    // The one case that has to reach a real linker, since the whole claim is about which symbol each
    // call resolves to. The two C functions answer differently for the same input, so a call reaching
    // the wrong one prints the wrong number rather than failing to build.
    "so two of them naming two symbols are two functions" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = createTempDirectory("sysl-overload-")

      writeFile(s"$root/shim.c",
        "int probe_one(int a) { return a + 1; }\nint probe_two(int a, int b) { return a * b; }\n")
      writeFile(s"$root/main.sysl",
        """extern "probe_one" bump(a: int) -> int
          |extern "probe_two" bump(a: int, b: int) -> int
          |
          |print(str(bump(41)), str(bump(6, 7)))
          |""".stripMargin)

      ran(Config(command = "run", file = root)) shouldBe "42 42\n"
    }

    // One C function claimed at two signatures. Nothing downstream could tell the two apart, since
    // the symbol is what is emitted — both calls would reach the same code with different arguments.
    "while two naming ONE symbol are refused" in {
      err("""extern "probe_one" bump(a: int) -> int
            |extern "probe_one" bump(a: int, b: int) -> int""".stripMargin) should
        include("one C function cannot be two")
    }

    // A sysl function has no symbol to be told apart by, so the two kinds do not mix. Asked both
    // ways round, because the check is written in two places and one of them could be missing.
    "and an 'extern' may not overload a sysl function" in {
      err("""bump(a: int) -> int = a + 1
            |extern "probe_two" bump(a: int, b: int) -> int""".stripMargin) should
        include("what tells overloads of an 'extern' apart is the symbol each names")
    }

    "nor a sysl function an 'extern'" in {
      err("""extern "probe_two" bump(a: int, b: int) -> int
            |bump(a: int) -> int = a + 1""".stripMargin) should
        include("what tells overloads of an 'extern' apart is the symbol each names")
    }
  }

  // A refused declaration is still a declaration: it has been counted into the name's overload set,
  // and a call of the name reads every member of that set. So each refusal is asked again with a
  // call below it — the refusal has to be what comes out, and the only thing that does. The quoted
  // source line is asserted so that the absence of the call's line is a statement about the
  // diagnostics rather than about the message carrying no source at all.
  "a refused declaration is still there for a call of the name to find" - {
    "a sysl function refused for overloading an 'extern'" in {
      val e = err("""extern "abs" bump(a: int) -> int
                    |bump(a: long) -> long = a
                    |print(bump(3))""".stripMargin)

      e should include("'bump' is already declared as an 'extern', which this would overload")
      e should include("bump(a: long) -> long = a")
      e should not include "print(bump(3))"
    }

    "an 'extern' refused for overloading a sysl function" in {
      val e = err("""bump(a: long) -> long = a
                    |extern "abs" bump(a: int) -> int
                    |print(bump(3))""".stripMargin)

      e should include("'bump' is already declared as a function, so this 'extern' would overload it")
      e should include("""extern "abs" bump(a: int) -> int""")
      e should not include "print(bump(3))"
    }

    "an 'extern' refused for naming a symbol another of the name has" in {
      val e = err("""extern "abs" bump(a: int) -> int
                    |extern "abs" bump(a: long) -> long
                    |print(bump(3))""".stripMargin)

      e should include("one C function cannot be two")
      e should include("""extern "abs" bump(a: long) -> long""")
      e should not include "print(bump(3))"
    }

    "a function refused for differing from another only in what it returns" in {
      val e = err("""bump(a: int) -> int = a
                    |bump(a: int) -> long = 1
                    |print(bump(3))""".stripMargin)

      e should include("function 'bump' is already declared")
      e should include("bump(a: int) -> long = 1")
      e should not include "print(bump(3))"
    }
  }

  "a pair no call could tell apart is refused where it is written" - {
    // Resolution never looks at the result, so this pair has no call that distinguishes them. Refused
    // at the declaration: the mistake is in the pair, and every use of the name would otherwise
    // report the same ambiguity.
    "so two differing only in what they return are refused" in {
      val e = err("""h(x: int) -> string = "s"
                    |h(x: int) -> int = 1""".stripMargin)

      // Told as the duplicate it is — the parameters are the same, so it is one declaration written
      // twice — with the sentence finished, because this is the pair a reader wrote on purpose.
      e should include("function 'h' is already declared")
      e should include("never by what it returns")
    }

    // And the same parameters with the same result is the plain duplicate, which gets the plain
    // message: somebody who wrote one function twice is not owed a paragraph about overloading.
    "while an outright duplicate is told it is one, with nothing about overloads" in {
      val e = err("""h(x: int) -> string = "a"
                    |h(x: int) -> string = "b"""".stripMargin)

      e should include("function 'h' is already declared")
      e should not include "what it returns"
    }

    // The second's default is unreachable — no call can supply one argument to it, because the first
    // takes that call. A default nothing can use is worth saying out loud.
    "as are two whose difference is behind a default" in {
      err("""g(x: int) -> string = "one"
            |g(x: int, y: int = 0) -> string = "two"""".stripMargin) should
        include("could not be told from")
    }

    // The ranges overlap only where the types agree, so this pair is fine: a call of one argument
    // reaches the first, and one of two reaches the second whatever its first argument is.
    "though a default is fine where the overlap has different types" in {
      run("""g(x: int) -> string = "one"
            |g(x: string, y: int = 0) -> string = s"two $y"
            |
            |print(g(1), g("a"), g("a", 5))""".stripMargin) shouldBe "one two 0 two 5\n"
    }
  }

  "a use that fits none or several is refused where it is written" - {
    "with the roster, when nothing takes these arguments" in {
      val e = err("""k(x: int) -> string = "a"
                    |k(x: string) -> string = "b"
                    |print(k(1.5))""".stripMargin)

      e should include("no 'k' takes these arguments")
      e should include("k(x: int)")
      e should include("k(x: string)")
    }

    // Where exactly one declaration could have taken this many arguments, the reader meant that one
    // and got a type wrong — so its own complaint is the useful message and the roster is not.
    "and with the callee's own complaint, where only one could have been meant" in {
      err("""k(x: int) -> string = "a"
            |k(x: string, y: string) -> string = "b"
            |print(k(1.5))""".stripMargin) should include("k(x: int)")
    }

    // An argument that was already refused where it was made carries `Type.Unknown`, which fits no
    // parameter — so *every* candidate fails and the roster is printed with nothing eliminated,
    // listing declarations that plainly do take the call as written. That is the worst message
    // available: it says resolution rejected a call the reader can see matches, and sends them off
    // to look at the overload set instead of at the first error. Say nothing.
    "and not at all, when an argument was already refused upstream" in {
      val e = err("""k(x: int) -> string = "a"
                    |k(x: int, y: int) -> string = "b"
                    |val bad = nope
                    |print(k(bad))""".stripMargin)

      e should include("undefined name 'nope'")
      e should not include "takes these arguments"
    }

    // The same value at a name declared *once* has always been silent, which is what makes the
    // roster the odd one out rather than the cascade a thing sysl reports everywhere.
    "as a call to a name declared once has always been" in {
      val e = err("""k(x: int) -> string = "a"
                    |val bad = nope
                    |print(k(bad))""".stripMargin)

      e should include("undefined name 'nope'")
      e.linesIterator.count(_.startsWith("error: ")) shouldBe 1
    }
  }

  /** A generic declaration beside an ordinary one, which is the pair the exactness tie-break could
   * not judge until it was asked of the **instantiated** parameter types rather than of the ones the
   * declaration wrote. A `T` resolves to nothing on its own, so a generic candidate used to be
   * inexact whatever the call solved it to, and tied with anything a conversion reached.
   *
   * Checked by running, like the rest of this file: each declaration answers differently, so a call
   * reaching the wrong one prints the wrong word.
   */
  "a generic declaration takes its place among the candidates" - {
    // The card's own reduction. `v` is a `[]int`, which `g[T]` takes as written and `g(s: []const
    // int)` takes only by giving up the ability to write — so the generic is the exact one.
    "and wins where it fits as written and the ordinary one needs a view" in {
      run("""g(s: []const int) -> string = "const"
            |g[T](x: T) -> string = "generic"
            |
            |var a = [1, 2, 3]
            |val v: []int = a[..]
            |print(g(v))""".stripMargin) shouldBe "generic\n"
    }

    // A second conversion, so the rule is not read as being about const views. An `int` erases to a
    // trait object and is exact at `T`.
    "whatever the conversion is — an erasure decides the same way" in {
      run("""p(x: &Display) -> string = "erased"
            |p[T](x: T) -> string = "generic"
            |
            |print(p(1))""".stripMargin) shouldBe "generic\n"
    }

    // The pair 0110 made reach this tie: an array is now a view of itself where a view is asked for,
    // so both candidates fit `pick(a)` and only the const-generic one fits it as an array.
    "including a const-generic candidate against a slice parameter" in {
      run("""pick(s: []const int) -> string = "slice"
            |pick[const N: usize](a: [N]int) -> string = "array"
            |
            |var a = [1, 2, 3]
            |print(pick(a))""".stripMargin) shouldBe "array\n"
    }

    // Tie-break three. Both are exact at `int`, and the reader who wrote `f(x: int)` said what it
    // takes rather than being solved for it. Without this the tie-break above would turn a call that
    // resolves today into an ambiguity, which is why it is a guard and not a preference.
    "while an ordinary declaration beats it where both are exact" in {
      run("""f(x: int) -> string = "plain"
            |f[T](x: T) -> string = "generic"
            |
            |print(f(0))""".stripMargin) shouldBe "plain\n"
    }

    // The literal-width tie-break with a generic in the set, which is the case that says the third
    // rule is applied to the *exact* candidates rather than to every one that fits: `i64` is not
    // exact at a bare `1` and must not be reached by being the only other ordinary declaration.
    "and the literal's own type still decides among the ordinary ones" in {
      run("""width(x: int) -> string = "int"
            |width(x: i64) -> string = "i64"
            |width[T](x: T) -> string = "generic"
            |
            |print(width(1))
            |print(width(1i64))""".stripMargin) shouldBe "int\ni64\n"
    }

    // Nothing about this makes a generic candidate win by default: where it is the only one that
    // fits at all it is chosen, and where an ordinary one fits exactly it is not.
    "and it is still chosen where it is the only candidate that fits" in {
      run("""q(x: int) -> string = "int"
            |q[T](x: T) -> string = "generic"
            |
            |print(q("a"))""".stripMargin) shouldBe "generic\n"
    }

    // Card 0369. The type parameter is inside the parameter's type rather than being the whole of
    // it, so nothing is exact at an array argument — the view is a conversion, and both candidates
    // need it. Tie-break three was filtering the *exact* candidates, so an empty set meant it never
    // ran, and the same two declarations resolved at a slice and were ambiguous at an array.
    "and an ordinary declaration wins through a conversion both candidates took" in {
      run("""g(x: []u8) -> string = "plain"
            |g[T](x: []T) -> string = "generic"
            |
            |var a: [3]u8 = [1, 2, 3]
            |val v: []u8 = a[..]
            |
            |print(g(v))
            |print(g(a))""".stripMargin) shouldBe "plain\nplain\n"
    }

    // And the widening stops exactly where the "no ranking between conversions" rule does. These two
    // fit at `[]const int` and `[]int`, which are two different routes out of one array, so there is
    // nothing to choose between them that is not a preference among conversions.
    "while two fitted at DIFFERENT types are still ambiguous" in {
      val e = err("""h(x: []const int) -> string = "const"
                    |h[T](x: []T) -> string = "generic"
                    |
                    |var a = [1, 2, 3]
                    |
                    |print(h(a))""".stripMargin)

      e should include("'h' is ambiguous here")
    }
  }

  // C has no overloading, so an overload set has at most one member that may take its own name as a
  // symbol. Caught by name rather than left to the rule that an export's symbol must be a C
  // identifier — which does refuse it, and does so quoting `pick.2`, a spelling the source does not
  // contain.
  "an overload exported under its own name is refused, and not by accident" in {
    val e = err("""@export
                  |pick(a: int) -> int = a + 1
                  |
                  |@export
                  |pick(a: int, b: int) -> int = a + b""".stripMargin)

    e should include("C has no overloading")
    e should not include "pick.2"
  }

  "though each may be exported under a symbol of its own" in {
    val out = ir("""@export("pick_one")
                   |pick(a: int) -> int = a + 1
                   |
                   |@export("pick_two")
                   |pick(a: int, b: int) -> int = a + b""".stripMargin)

    out should include("@pick_one")
    out should include("@pick_two")
  }

  // **An overload set has to survive being shipped**, which is a different path from every test
  // above: a library is compiled once, archived, and hoisted again out of the artifact by whoever
  // links it. Nothing serializes the set — it is rebuilt by hoisting, exactly as it is from source —
  // so this is what says that rebuilding really happens. The two declarations answer differently, so
  // a consumer reaching the wrong one prints the wrong thing.
  "an overload set survives being built into a library and linked against" in {
    val lib = rootOf(
      "demo",
      """module demo
        |
        |scale(n: int) -> string = s"one $n"
        |scale(n: int, by: int) -> string = s"two ${n * by}"
        |""".stripMargin,
    )

    val prog = program("print(demo.scale(3))\nprint(demo.scale(3, 4))")

    ran(Config(command = "run", file = prog, libs = List(artifactOf(lib)))) shouldBe "one 3\ntwo 12\n"
  }

  // `main` is the one name it must not reach: nothing calls `main`, so there is no call site to
  // choose between two of them, and the entry point is found by asking which declarations are called
  // that. A second would be filed under a key of its own and silently never run.
  "'main' does not overload, since a program has one beginning" in {
    err("""main()
          |    print("one")
          |
          |main(args: []string)
          |    print("two")""".stripMargin) should include("a program has one beginning")
  }

  "and the compiler's spelling of an overload never reaches a reader" in {
    // The second declaration is keyed `paint.2` so that the tables stay one-declaration-per-key. A
    // diagnostic naming it that would be naming something the source does not contain.
    val e = err("""paint(x: int) -> string = "a"
                  |paint(x: string) -> string = "b"
                  |print(paint(1.5))""".stripMargin)

    e should not include "paint.2"
  }
}
