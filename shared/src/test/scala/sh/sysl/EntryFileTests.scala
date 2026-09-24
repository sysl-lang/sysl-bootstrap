package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What a file's top level means, which depends on where the file sits (`reference/modules.md § Where a program starts`).
 *
 * A file carrying statements is the one the program starts in, and its top level is a **body**:
 * what it declares is local to that body, so a `val` there is a stack local initialized where it
 * stands and a function there is a nested function (`reference/declarations.md`) capturing the
 * locals above it. A file carrying no statements declares module members, as does one with a
 * `module` header.
 *
 * `static` is how a declaration in the body opts back into the module — the escape hatch for the
 * three things a nested function cannot be and the one thing a local cannot be.
 *
 * The single fact worth holding on to: **which file the program starts in is decided by statements,
 * and what that file's declarations mean is decided by which file it is.** Those two questions used
 * to have one answer, and a top-level `var` being invisible to every function while a top-level `val`
 * was visible to all of them was the seam where that showed.
 */
class EntryFileTests extends AnyFreeSpec with CodegenSupport with RunSupport with ParseSupport {

  "a declaration in the file the program starts in is local to it" - {
    // The whole point, and the thing that could not be written before: a sequence that is a sequence.
    // A `val` bound from what the statements above it did is what a script wants, and what a module
    // member could never be, since a module member is bound before any statement runs.
    "so a 'val' runs its initializer where it is written, not ahead of the statements" in {
      run(
        """noisy() -> int
          |    print("second")
          |    7
          |end noisy
          |
          |print("first")
          |val n: int = noisy()
          |print(str(n))""".stripMargin,
      ) shouldBe "first\nsecond\n7\n"
    }

    // A helper that reads nothing is the module's, so declaration order is free exactly as it is
    // anywhere else (`reference/modules.md § The module graph is acyclic`).
    "while one that reads nothing may be called above where it is written" in {
      run(
        """print(str(later()))
          |later() -> int = 1""".stripMargin,
      ) shouldBe "1\n"
    }

    // The cost of being a nested function, and it is worth stating rather than discovering: a
    // group's environment is formed where the first of them is written, so a call above that point
    // has no environment to make (`reference/declarations.md`). It is paid only by a helper that
    // has an environment.
    "but one that reads a binding may not, since its environment is formed where it is" in {
      err(
        """print(str(later()))
          |val base = 1
          |later() -> int = base""".stripMargin,
      ) should include("declared below this call")
    }

    "and one bound from a variable the statements above it set is ordinary" in {
      run(
        """var total = 0
          |for i in 1..<4 do total += i
          |val frozen = total
          |total = 0
          |print(str(frozen), str(total))""".stripMargin,
      ) shouldBe "6 0\n"
    }

    // A local has no address in the object file, which is the observable half of "it is not a module
    // member". The same program with `static` on it is in `ValTests`, emitting `private constant`.
    "so it is not laid into the object file the way a module member is" in {
      ir("val k: [3]u32 = [7, 8, 9]\nprint(str(k[0]))") should not include "private constant [3 x i32]"
    }

    "while 'static' on the same declaration puts it back" in {
      ir("static val k: [3]u32 = [7, 8, 9]\nprint(str(k[0]))") should
        include("private constant [3 x i32] [i32 7, i32 8, i32 9]")
    }
  }

  /** A function written at the top of the entry file is a nested function, so
    * `reference/declarations.md` applies to it whole — including the half that is the reason
    * anybody wants this: it may read and write the bindings above it, because the environment holds
    * their addresses rather than copies.
    */
  "a function declared there is a nested function" - {
    "so it reads the bindings above it, with nothing passed in" in {
      run(
        """val base = 10
          |
          |plus(n: int) -> int = base + n
          |
          |print(str(plus(5)))""".stripMargin,
      ) shouldBe "15\n"
    }

    "and assigns to them, which is what an environment of addresses buys" in {
      run(
        """var count = 0
          |
          |bump()
          |    count += 1
          |end bump
          |
          |bump()
          |bump()
          |print(str(count))""".stripMargin,
      ) shouldBe "2\n"
    }

    // The one-line `=` form works here as it does anywhere, assignment included — so a helper whose
    // whole body writes a binding above it needs no indented block.
    "and the one-line '=' form writes one just as well" in {
      run("""var count = 0
            |bump() = count += 1
            |bump()
            |bump()
            |print(str(count))""".stripMargin) shouldBe "2\n"
    }

    // Names hoist, captures do not (`reference/declarations.md`). Two helpers may call each other
    // whichever order they are written in, which is what makes the group a group.
    "while two of them call each other, whichever order they are written in" in {
      run(
        """even(n: int) -> bool = if n == 0 then true else odd(n - 1)
          |odd(n: int) -> bool = if n == 0 then false else even(n - 1)
          |
          |print(str(even(4)), str(odd(4)))""".stripMargin,
      ) shouldBe "true false\n"
    }

    // Card `0224`: it is one it can capture. The group's environment is built after the last binding
    // any of its functions reads, so a helper written above the data it uses — the natural layout for
    // a script, and the first thing anybody writes in an entry file — is ordinary.
    "and a binding written below the group is one it can capture too" in {
      run(
        """show() -> int = later
          |val later = 1
          |print(str(show()))""".stripMargin,
      ) shouldBe "1\n"
    }
  }

  /** `reference/declarations.md`'s three limits — no generic, no address, not a value — are what
    * holding a frame costs, not what being written in the entry file costs. So a helper reading
    * nothing keeps all three, and only one that reads a binding pays them. A comparison handed to
    * `qsort` is the shape that makes this matter: it reads nothing, and refusing its address for
    * the frame it reads would be naming a frame that does not exist.
    */
  "what a helper may be depends on whether it reads anything, not on where it is written" - {
    "so a generic one is ordinary" in {
      run(
        """first[A, B](a: A, b: B) -> A = a
          |print(str(first(1, "x")))""".stripMargin,
      ) shouldBe "1\n"
    }

    "and its address may be taken" in {
      run(
        """twice(n: int) -> int = n * 2
          |val f: *extern(int) -> int = &twice
          |print(str(f(21)))""".stripMargin,
      ) shouldBe "42\n"
    }

    "and another file reads it, since it is the module's" in {
      runIn(
        ("", "main.sysl", "print(str(doubled(4)))"),
        ("", "other.sysl", "doubled(n: int) -> int = n * 2"),
      ) shouldBe "8\n"
    }

    "while one that reads a binding is nested, and has no address" in {
      err(
        """val base = 2
          |scaled(n: int) -> int = n * base
          |val f: *extern(int) -> int = &scaled
          |print(str(f(21)))""".stripMargin,
      ) should include("has no address to take")
    }

    "and may not be generic" in {
      err(
        """val base = 2
          |scaled[T](n: T) -> int = base
          |print(str(scaled(1)))""".stripMargin,
      ) should include("cannot be generic")
    }

    // Capture reaches through a sibling call: the nested functions of a block share one environment
    // (`reference/declarations.md`), so calling one that reads a binding needs that environment
    // too. `main` is the platform's symbol, not a name the program calls, so it can never be one of
    // the body's — and a `main` reading a binding used to become one silently, leaving the program
    // with no entry point at all and no complaint about it. The file runs a statement, which is what
    // makes it a body whatever else it declares — a lone `var` beside a `main` is the module's
    // (`EntryPointTests`).
    "while 'main' is never one of them, however it is written" in {
      err(
        """var count = 0
          |print("start")
          |main()
          |    print(count)""".stripMargin,
      ) should include("this 'main' is a second")
    }

    "and neither may one that only calls such a helper" in {
      err(
        """val base = 2
          |scaled(n: int) -> int = n * base
          |twice(n: int) -> int = scaled(n)
          |val f: *extern(int) -> int = &twice
          |print(str(f(21)))""".stripMargin,
      ) should include("has no address to take")
    }
  }

  "'static' is what a binding writes to belong to the module instead" - {
    "so another file reads it" in {
      runIn(
        ("", "main.sysl", "static val shared: int = 4\nprint(str(doubled()))"),
        ("", "other.sysl", "doubled() -> int = shared * 2"),
      ) shouldBe "8\n"
    }

    // The mirror of the first test in this file: a module member is bound before any statement runs,
    // so its initializer cannot call something that is part of the body.
    "while its initializer is bound before the body, so it may not call a helper that reads one" in {
      err(
        """var seed = 1
          |helper() -> int = seed
          |static val n: int = helper()
          |print(str(n))""".stripMargin,
      ) should include("undefined function 'helper'")
    }
  }

  /** A visibility modifier on a binding of the **body**, which restricts nothing and is accepted.
    *
    * Both spellings take one at a file's top level, and in the entry file both are locals — so the
    * modifier says nothing there, exactly as `private` on a `val` inside a function body would. It is
    * not refused, which is a choice rather than an oversight: the same line is meaningful the moment
    * the file gains a `module` header or stops carrying the statements, so refusing it would make
    * moving a declaration between files a rewrite.
    *
    * The pair is here so the two cannot drift. `private var` did not parse at all until 2026-08-06,
    * and the argument for accepting it was that the `val` beside it always had been.
    */
  "a modifier on the entry file's own binding says nothing, and is not refused for it" - {
    "a 'val'" in {
      run("private val a: int = 1\nprint(str(a))") shouldBe "1\n"
    }

    "and a 'var'" in {
      run("private var n: int = 1\nn += 1\nprint(str(n))") shouldBe "2\n"
    }
  }

  /** `static var` — module storage the program may **write**, which is what `static val` is not.
    *
    * Three things separate it from the `val`, and each is what the word `var` already means: it may
    * be assigned at every depth, its initializer may be absent, and the release rule is asked of its
    * **type** rather than of its value, since a variable may be given a different value tomorrow.
    */
  "a 'static var' is module storage that may be written" - {
    "so a helper reads and writes it, with nothing passed in" in {
      run(
        """static var ticks: int = 0
          |
          |tick() = ticks += 1
          |
          |tick()
          |tick()
          |tick()
          |print(str(ticks))""".stripMargin,
      ) shouldBe "3\n"
    }

    "and another file writes it too, which a local could never allow" in {
      runIn(
        ("", "main.sysl", "static var hits: int = 0\nbump()\nbump()\nprint(str(hits))"),
        ("", "other.sysl", "bump() = hits += 1"),
      ) shouldBe "2\n"
    }

    // The cheapest form, and the one an arena wants: `zeroinitializer` and no store at all.
    "its initializer may be absent, which a 'val' may not" in {
      run("static var slot: int\nslot = 7\nprint(str(slot))") shouldBe "7\n"
    }

    "and it is a 'global' rather than a 'constant', since the program writes it" in {
      ir("static var n: int = 5\nn = 6\nprint(str(n))") should include("private global")
    }

    "while a 'static val' of the same shape stays a constant" in {
      ir("static val n: int = 5\nprint(str(n))") should include("private constant")
    }

    // It holds a counted value like any other module storage (`reference/modules.md § val — a thing`), and the `var` half is where
    // that has to be shown rather than merely allowed: the storage is given a literal and then a
    // built string, so the second store is the one that has a release to write and a line to write it
    // on. Reading it back afterwards is what says the first was let go of without taking the second.
    "and it holds a type that owes a release, both given and then reassigned" in {
      run("static var greeting: string = \"hello\"\ngreeting = greeting + \" \" + str(42)\nprint(greeting)") shouldBe
        "hello 42\n"
    }

    "where a 'static val' given that same literal is admitted" in {
      run("static val greeting: string = \"hello\"\nprint(greeting)") shouldBe "hello\n"
    }

    // The one refusal that replaces the broad one, in the entry file's spelling: storage with no
    // initializer starts at its type's zero, and a reference has none.
    "though one with no zero must be given a value" in {
      err("struct P\n    x: int\nend P\nstatic var p: &P\nprint(str(p.x))") should include("needs a value")
    }

    "it states its type, having no value to infer one from" in {
      err("static var n = 1\nprint(str(n))") should include("states its type")
    }

    "and a '@pure' function may not read one" in {
      err(
        """static var seed: int = 1
          |
          |@pure
          |peek() -> int = seed
          |
          |print(str(peek()))""".stripMargin,
      ) should not be empty
    }
  }

  "'static' says nothing anywhere else, and is refused rather than ignored" - {
    "in a file that carries no statements, where everything is the module's already" in {
      errIn(
        ("", "main.sysl", "print(str(k))"),
        ("", "table.sysl", "static val k: int = 1"),
      ) should include("this file has no such body")
    }

    "in a file with a module header, for the same reason" in {
      errIn(
        ("", "main.sysl", "print(str(tables.k))"),
        ("tables", "t.sysl", "module tables\nstatic val k: int = 1"),
      ) should include("this file has no such body")
    }

    "inside a block, where there is no module member for a declaration to be instead" in {
      err(
        """f() -> int
          |    static val n: int = 1
          |    n
          |end f
          |print(str(f()))""".stripMargin,
      ) should include("local to that block")
    }

    "and on a declaration that is a module member wherever it is written" in {
      err("static const n: int = 1\nprint(str(n))") should
        include("'static' marks a 'val' or a 'var'")
    }

    // A function is settled by what it reads, so the modifier would be either redundant or
    // impossible — and a reader expecting it on one is told which.
    "and on a function, which is settled by what it reads rather than by a modifier" in {
      err("static f() -> int = 1\nprint(str(f()))") should
        include("a function is one unless it reads a binding of the body")
    }
  }

  "one file carries the statements the program runs" - {
    "so a second that carries any is told which one already does" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("", "other.sysl", "print(2)"),
      ) should include("already carries the statements this program runs")
    }

    // A file of declarations is not a file of statements however much it declares, which is what
    // keeps a program's tables and helpers from making their file the one it starts in.
    "while a file of nothing but declarations is not one of them" in {
      runIn(
        ("", "main.sysl", "print(str(size()))"),
        ("", "table.sysl", "val k: [2]int = [1, 2]\nsize() -> usize = k.len\nconst width: int = 2"),
      ) shouldBe "2\n"
    }

    "and a program with no statements at all runs and does nothing" in {
      run("f() -> int = 1") shouldBe ""
    }
  }

  /** A file that names a module is never the one the program starts in (`reference/modules.md § Where a program starts`).
    *
    * Which file that is comes down to what a file *runs*, and where nothing runs a lone file of
    * bindings is a body after all — that is what keeps a one-file `var n = 1` meaning what it always
    * meant. But the fallback reached files with a `module` header too, and a header says there is no
    * body for a binding to belong to instead.
    *
    * What that cost is not visible at the `var`. The file became a body, so every function *reading*
    * the `var` became a nested function of it — and a nested function may not be `private` and may not
    * be generic, so those were the two diagnostics a library got. They named the function rather than
    * the cause, and they appeared only where no other file supplied a beginning: the module compiled
    * when a program imported it, and was refused by `build-lib`, where a library is all there is.
    */
  "a file that names a module is not one of them, whatever else the program carries" - {
    // A library, which is what `build-lib` compiles: files, and no beginning anywhere.
    "so a library's private helper may read its module's storage" in {
      irIn(("m", "a.sysl", "module m\n\nvar n: int = 1\n\nprivate peek() -> int = n")) should not be empty
    }

    // The same fault, seen through the other thing a nested function may not be. There is no
    // workaround for this one — dropping the modifier answers the test above and not this.
    "and a generic one may too" in {
      irIn(
        ("m", "a.sysl",
         "module m\n\nvar n: int = 0\n\ntally[T: Display](x: T) -> int\n    n += 1\n    n"),
      ) should not be empty
    }

    // Where the program's beginning is a declared `main` rather than loose statements, nothing at
    // any file's top level runs — so this is the fallback's case with a program around it.
    "even where the program's beginning is a 'main' and so runs nothing at the top level" in {
      runIn(
        ("", "main.sysl", "main()\n    m.bump()\n    print(str(m.peek()))"),
        ("m", "a.sysl", "module m\n\nvar n: int = 1\n\nbump() = n += 1\n\nprivate seen() -> int = n\n\npeek() -> int = seen()"),
      ) shouldBe "2\n"
    }

    // The guard on the change, and the reason it is a condition rather than the fallback's removal:
    // a **headerless** file is still eligible, so where nothing runs its lone binding is still a
    // local of the body it was written in — which is what a sibling failing to see it says.
    "while a headerless one is still eligible, its binding still a local a sibling cannot see" in {
      errOf(
        "a.sysl" -> "var n: int = 1",
        "b.sysl" -> "peek() -> int = n",
      ) should not be empty
    }
  }
}
