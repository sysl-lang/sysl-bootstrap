package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `defer` — scope exit for what the language does not own (`reference/memory.md § Where defer
 * sits`).
 *
 * Almost everything here is a **run** test that prints in the order things happened, because that
 * order is the whole content of the feature and it is not visible in the IR: a deferred statement
 * is emitted at each edge that leaves its block, so reading one edge tells you nothing about the
 * others. A test that only checked the statement was emitted would pass for a compiler that ran it
 * at the wrong time, at the wrong edge, or twice.
 *
 * The claims being pinned are the ones `reference/memory.md § Where defer sits` makes: it runs at
 * the end of its **block**, on every ordinary exit including `?`, last-registered-first, only if
 * control reached it, and never on a trap.
 */
class DeferTests extends AnyFreeSpec with RunSupport with CodegenSupport with TestFrameworkSupport {

  "when a deferred statement runs" - {
    "at the end of the block, after everything written after it" in {
      run("""print("open")
            |defer print("close")
            |print("work")
            |""".stripMargin) shouldBe "open\nwork\nclose\n"
    }

    // The registration is the only thing that happens where the `defer` stands. If the statement
    // ran there instead, this would print in written order and the feature would be nothing.
    "and not where it is written" in {
      run("""defer print("last")
            |print("first")
            |""".stripMargin) shouldBe "first\nlast\n"
    }

    "on an early return, before the caller sees the result" in {
      run("""f(early: bool) -> int
            |    defer print("cleanup")
            |    if early then return 1
            |    2
            |
            |print(f(true))
            |print(f(false))
            |""".stripMargin) shouldBe "cleanup\n1\ncleanup\n2\n"
    }

    // `reference/errors.md § ? and the memory model` makes `?` the normal way to leave a function,
    // so a form that did not fire on it would miss the exit that matters most.
    "on the failure arm of a '?'" in {
      run("""parse(ok: bool) -> Result[int, string] =
            |    if ok then Ok(7) else Err("bad")
            |
            |use(ok: bool) -> Result[int, string]
            |    defer print("released")
            |    var n = parse(ok)?
            |    Ok(n + 1)
            |
            |use(true) match
            |    Ok(v) -> print("ok", v)
            |    Err(e) -> print("err", e)
            |
            |use(false) match
            |    Ok(v) -> print("ok", v)
            |    Err(e) -> print("err", e)
            |""".stripMargin) shouldBe "released\nok 8\nreleased\nerr bad\n"
    }

    // The trailing expression is the function's result, and `reference/memory.md § Where defer
    // sits` says the deferred statement runs after it is computed — so a defer that mutates what
    // was already returned cannot change it.
    "after the returned value has been computed" in {
      run("""f() -> int
            |    var n = 1
            |    defer n = 99
            |    n
            |
            |print(f())
            |""".stripMargin) shouldBe "1\n"
    }
  }

  "the block it belongs to" - {
    // The case that decided block scope over Go's function scope. Each iteration closes what it
    // opened; under function scope all three would print after the loop.
    "is the loop body, so each iteration runs its own" in {
      run("""for i in 0..<3
            |    print("open", i)
            |    defer print("close", i)
            |    print("use", i)
            |""".stripMargin) shouldBe
        "open 0\nuse 0\nclose 0\nopen 1\nuse 1\nclose 1\nopen 2\nuse 2\nclose 2\n"
    }

    "is the branch arm, so an arm not taken schedules nothing" in {
      run("""f(t: bool)
            |    if t
            |        defer print("then")
            |        print("in then")
            |    else
            |        defer print("else")
            |        print("in else")
            |    print("after")
            |
            |f(true)
            |f(false)
            |""".stripMargin) shouldBe "in then\nthen\nafter\nin else\nelse\nafter\n"
    }

    "is the function body when that is where it was written" in {
      run("""f()
            |    defer print("body end")
            |    if true
            |        print("inner")
            |    print("still in body")
            |
            |f()
            |print("returned")
            |""".stripMargin) shouldBe "inner\nstill in body\nbody end\nreturned\n"
    }

    // A `defer` in an inner block must not wait for the outer one, and the outer must still run
    // after it. One test for both halves, because the bug is getting the nesting order backwards.
    "nests, innermost block first" in {
      run("""defer print("outer")
            |if true
            |    defer print("inner")
            |    print("work")
            |print("between")
            |""".stripMargin) shouldBe "work\ninner\nbetween\nouter\n"
    }
  }

  "leaving from the middle" - {
    "runs a loop body's defer before the 'break' takes effect" in {
      run("""for i in 0..<5
            |    defer print("close", i)
            |    if i == 1 then break
            |    print("use", i)
            |print("after")
            |""".stripMargin) shouldBe "use 0\nclose 0\nclose 1\nafter\n"
    }

    "runs it on a 'continue' too" in {
      run("""for i in 0..<3
            |    defer print("close", i)
            |    if i == 1 then continue
            |    print("use", i)
            |print("after")
            |""".stripMargin) shouldBe "use 0\nclose 0\nclose 1\nuse 2\nclose 2\nafter\n"
    }

    // A `return` from inside a loop leaves two blocks at once, so both scopes' deferred statements
    // run, innermost first. This is the case `releaseAll` has to walk outward for.
    "runs every enclosing block's defers on a return from inside a loop" in {
      run("""f() -> int
            |    defer print("function")
            |    for i in 0..<3
            |        defer print("loop", i)
            |        if i == 1 then return i
            |    9
            |
            |print(f())
            |""".stripMargin) shouldBe "loop 0\nloop 1\nfunction\n1\n"
    }
  }

  "several in one block" - {
    "run last-registered-first" in {
      run("""defer print("c")
            |defer print("b")
            |defer print("a")
            |print("body")
            |""".stripMargin) shouldBe "body\na\nb\nc\n"
    }

    // LIFO is what lets a later defer depend on an earlier one's resource still being there. Written
    // as an ordering a FIFO implementation would get exactly backwards.
    "so a later one may rely on what an earlier one has not yet undone" in {
      run("""var log = ""
            |defer print(log)
            |defer log = log + "outer "
            |defer log = log + "inner "
            |log = "start "
            |""".stripMargin) shouldBe "start inner outer \n"
    }
  }

  "only if control reached it" - {
    "one after an early return never registers" in {
      run("""f(early: bool)
            |    defer print("first")
            |    if early then return
            |    defer print("second")
            |
            |f(true)
            |print("--")
            |f(false)
            |""".stripMargin) shouldBe "first\n--\nsecond\nfirst\n"
    }

    "one in a loop that never iterates schedules nothing" in {
      run("""for i in 0..<0
            |    defer print("never")
            |print("done")
            |""".stripMargin) shouldBe "done\n"
    }
  }

  "what it costs" - {
    // `reference/memory.md § Where defer sits` claims it allocates nothing and takes no count,
    // which is what keeps it usable under `no alloc`. Asserted at the capability, which is the
    // surface that would refuse it.
    "nothing that 'no alloc' forbids" in {
      run("""@no_alloc
            |
            |f() -> i32
            |    defer print("released")
            |    7i32
            |
            |print(f())
            |""".stripMargin) shouldBe "released\n7\n"
    }

    // The statement is emitted at each edge, so a block with two exits carries two copies and a
    // block with one carries one. This is the claim that there is no runtime defer list to consult.
    "no registration at the point the 'defer' stands" in {
      val out = irMain("""defer print("x")
                         |print("y")
                         |""".stripMargin)

      out should not include "defer"
      out should not include "alloca"
    }
  }

  "what it is refused for, and why" - {
    "a 'return', which would leave a block already being left" in {
      err("""f() -> int
            |    defer return 1
            |    2
            |""".stripMargin) should include("cannot 'return'")
    }

    "a 'break', for the same reason" in {
      err("""for i in 0..<3
            |    defer break
            |""".stripMargin) should include("cannot 'break' or 'continue'")
    }

    "a '?', which is the same exit written as an operator" in {
      err("""parse() -> Result[int, string] = Ok(1)
            |
            |f() -> Result[int, string]
            |    defer parse()?
            |    Ok(2)
            |""".stripMargin) should include("nowhere to return to")
    }

    "a declaration, whose name nothing could ever read" in {
      err("""f()
            |    defer var x = 1
            |""".stripMargin) should include("dies the moment the statement finishes")
    }

    "another 'defer', which would schedule a scheduling" in {
      err("""f()
            |    defer defer print("x")
            |""".stripMargin) should include("scheduling a scheduling")
    }
  }

  "against the rest of the memory model" - {
    // The load-bearing ordering claim: a deferred statement runs before its block's ARC releases, so
    // the local holding the resource is still alive. A counted value read in the defer proves it —
    // if the release came first this would read freed storage.
    "a deferred statement still sees the block's counted locals" in {
      run("""struct Box
            |    n: int
            |
            |f()
            |    var b: &Box = Box(42)
            |    defer print("closing", b.n)
            |    print("using", b.n)
            |
            |f()
            |""".stripMargin) shouldBe "using 42\nclosing 42\n"
    }

    // A `defer` in an inner block naming an outer local: the outer scope's counts must outlive the
    // inner scope's deferred statement, which is what unwinding outward one scope at a time buys.
    "an inner block's defer may name an outer block's local" in {
      run("""struct Box
            |    n: int
            |
            |f()
            |    var b: &Box = Box(7)
            |    if true
            |        defer print("inner sees", b.n)
            |        print("inner")
            |    print("outer")
            |
            |f()
            |""".stripMargin) shouldBe "inner\ninner sees 7\nouter\n"
    }

    // A deferred statement is still a statement of the body, so a view it lets out of the frame has
    // to be seen — the array is promoted to the heap or the slice reads a dead frame. This is what
    // the passes' shared walk buys: a walk that stopped at a `defer` would compile this silently and
    // the reads below would be of storage that is gone.
    "a view let out by a deferred statement still promotes its array" in {
      val src = """struct Sink
                  |    v: []int
                  |
                  |f(s: &Sink)
                  |    var xs = [1, 2, 3]
                  |    defer s.v = xs[..]
                  |
                  |var sink: &Sink = Sink([0])
                  |f(sink)
                  |print(sink.v[0], sink.v[1], sink.v[2])
                  |""".stripMargin

      Compiler.compiled(List(Source("t.sysl", src))) match
        case Right(built) =>
          built.notes should have length 1
          built.notes.head should include("'xs' is promoted to the heap")
        case Left(e) => fail(s"did not compile:\n$e")

      run(src) shouldBe "1 2 3\n"
    }

    // The function whose only call site is a deferred statement must survive pruning. `Reachability`
    // descends structurally, so this passes today — it is here because the failure would be a link
    // error rather than a wrong answer, and nothing else in the suite would produce one.
    "a function called only from a deferred statement is not pruned" in {
      run("""release()
            |    print("released")
            |
            |f()
            |    defer release()
            |    print("working")
            |
            |f()
            |""".stripMargin) shouldBe "working\nreleased\n"
    }
  }

  "what the chapters claim" - {
    // `reference/errors.md § Traps` settles that a trap aborts without stack cleanup, and
    // `reference/memory.md § Where defer sits` does not qualify it. Asserted through the exit
    // status rather than through output, because a trap does not flush stdio — so a program whose
    // deferred statement is `exit(0)` exits zero if defers ran on the trap path and non-zero if
    // they did not. Nothing about buffering can fake that.
    "a trap runs no deferred statement" in {
      exits("""f()
              |    var xs = [1, 2, 3]
              |    var i = 5
              |    defer exit(0)
              |    print(xs[i])
              |
              |f()
              |""".stripMargin)
    }

    // The same program with the trap removed must exit through the defer, or the test above would
    // pass for a compiler that never ran deferred statements at all.
    "and the same program without the trap exits through it" in {
      exitsWith("""f()
                  |    var xs = [1, 2, 3]
                  |    var i = 1
                  |    defer exit(3)
                  |    print(xs[i])
                  |
                  |f()
                  |""".stripMargin, 3)
    }

    // `reference/memory.md § Where defer sits` names the loop body as a block without saying which
    // loop, so each form is asked. Both read `i` at its exit-time value, per the rule pinned below.
    "a 'while' body is a block like any other" in {
      run("""var i = 0
            |while i < 2
            |    defer print("close", i)
            |    print("use", i)
            |    i += 1
            |""".stripMargin) shouldBe "use 0\nclose 1\nuse 1\nclose 2\n"
    }

    "so is a 'loop' body" in {
      run("""var i = 0
            |loop
            |    defer print("close", i)
            |    if i == 1 then break
            |    print("use", i)
            |    i += 1
            |""".stripMargin) shouldBe "use 0\nclose 1\nclose 1\n"
    }

    // **Where Go and sysl part company on a second axis.** Go evaluates a deferred call's arguments
    // at the `defer` and runs the call later; sysl runs the whole statement later, so everything in
    // it is read at exit. Discriminating by construction: the two rules print different numbers, and
    // eager capture is what would need a per-defer slot to hold the captured value in.
    "a deferred statement reads its variables at exit, not at registration" in {
      run("""f()
            |    var n = 1
            |    defer print("n at exit:", n)
            |    n = 2
            |    print("n now:", n)
            |
            |f()
            |""".stripMargin) shouldBe "n now: 2\nn at exit: 2\n"
    }

    "so is a 'match' arm" in {
      run("""f(n: int)
            |    n match
            |        0 ->
            |            defer print("zero out")
            |            print("zero in")
            |        _ ->
            |            defer print("other out")
            |            print("other in")
            |    print("after")
            |
            |f(0)
            |f(1)
            |""".stripMargin) shouldBe "zero in\nzero out\nafter\nother in\nother out\nafter\n"
    }

    // `reference/declarations.md` — a nested function is a function, so its body is a block and its
    // defers are its own rather than the enclosing body's.
    "a nested function's defers belong to the nested function" in {
      run("""outer()
            |    inner()
            |        defer print("inner out")
            |        print("inner in")
            |
            |    defer print("outer out")
            |    inner()
            |    print("outer in")
            |
            |outer()
            |""".stripMargin) shouldBe "inner in\ninner out\nouter in\nouter out\n"
    }

    "a closure body is a block too" in {
      run("""var f: &Fn(int) -> int = n ->
            |    defer print("in closure")
            |    n * 2
            |
            |print(f(21))
            |""".stripMargin) shouldBe "in closure\n42\n"
    }

    // `16` checks a postcondition before the function returns, and a deferred statement is cleanup
    // that runs after the result is fixed — so the `ensure` sees the value and the defer runs after
    // the check. Pinned because the reverse order would let cleanup invalidate what was checked.
    "an 'ensure' is checked before the deferred statement runs" in {
      run("""f() -> int
            |    ensure result == 1
            |    defer print("after the check")
            |    1
            |
            |print(f())
            |""".stripMargin) shouldBe "after the check\n1\n"
    }

    // `@test` functions are ordinary bodies that `sysl test` calls, so a defer in one runs the same.
    // The inner block is what makes it observable: a defer at the top of the test would run after
    // the assertion that would have to check it.
    "a '@test' function's body is a block like any other" in {
      verdicts("""@test
                 |t() =
                 |    var log = ""
                 |    if true
                 |        defer log = log + "torn "
                 |        log = log + "in "
                 |    assert(log == "in torn ", "the defer ran at the end of the inner block")
                 |""".stripMargin) shouldBe Map("t" -> None)
    }
  }

  "the edges" - {
    "a defer alone in a body still runs" in {
      run("""f()
            |    defer print("only")
            |
            |f()
            |""".stripMargin) shouldBe "only\n"
    }

    // Two loops and a function body, left at once. Every scope between the `return` and the function
    // runs its own, innermost first — the case `releaseAll` walks outward for.
    "a return from two loops deep runs all three scopes' defers" in {
      run("""f() -> int
            |    defer print("function")
            |    for i in 0..<2
            |        defer print("outer loop", i)
            |        for j in 0..<2
            |            defer print("inner loop", j)
            |            if j == 1 then return 5
            |    0
            |
            |print(f())
            |""".stripMargin) shouldBe "inner loop 0\ninner loop 1\nouter loop 0\nfunction\n5\n"
    }

    // A deferred statement is an ordinary statement, so a function it calls has its own defers and
    // they run inside it. The nesting must not flatten into one list.
    "a deferred call's own defers run inside it" in {
      run("""inner()
            |    defer print("inner out")
            |    print("inner in")
            |
            |f()
            |    defer inner()
            |    print("body")
            |
            |f()
            |print("done")
            |""".stripMargin) shouldBe "body\ninner in\ninner out\ndone\n"
    }

    // A `defer` written inside a nested block registers against that block, so a second one in the
    // outer block runs after it even though it was written later.
    "written order across nesting is not run order" in {
      run("""if true
            |    defer print("inner")
            |    print("in block")
            |defer print("outer, written second")
            |print("after block")
            |""".stripMargin) shouldBe "in block\ninner\nafter block\nouter, written second\n"
    }

    // A branch used as a value computes it and then leaves the block, so the defer runs after the
    // value is fixed — the same rule the function result follows. A defer that runs first would
    // change what the branch yielded.
    "a block used as a value runs its defers after the value is computed" in {
      run("""f(c: bool) -> int
            |    var n = 1
            |    var got = if c
            |        defer n = 99
            |        n
            |    else
            |        0
            |    print("n after:", n)
            |    got
            |
            |print(f(true))
            |""".stripMargin) shouldBe "n after: 99\n1\n"
    }

    // A deferred statement carrying its own branch is what makes the teardown walk re-entrant: it is
    // emitted by the same `genStmt` that pushes and pops the scope stacks the walk is in the middle
    // of. Counted locals on both sides of it, and a `return` from the middle so the whole-function
    // unwind is the path taken, since that is the walk with more than one scope to get wrong.
    "a deferred statement may itself branch, while the scopes are being unwound" in {
      run("""struct Box
            |    n: int
            |
            |f(c: bool) -> int
            |    var a: &Box = Box(1)
            |    for i in 0..<2
            |        var b: &Box = Box(2)
            |        defer if c then print("chose", a.n, b.n) else print("other", b.n)
            |        if i == 1 then return a.n + b.n
            |    0
            |
            |print(f(true))
            |print(f(false))
            |""".stripMargin) shouldBe "chose 1 2\nchose 1 2\n3\nother 2\nother 2\n3\n"
    }

    // A multi-assignment is a statement in its own right (`reference/expressions.md § Several places at once`), and deferring one reaches the
    // arm-walking that every pass does through a shape it does not otherwise meet inside a `defer`.
    "a multi-assignment may be the deferred statement" in {
      run("""f()
            |    var a = 1
            |    var b = 2
            |    defer print(a, b)
            |    defer a, b = b, a
            |    print(a, b)
            |
            |f()
            |""".stripMargin) shouldBe "1 2\n2 1\n"
    }

    // A loop's `else` runs when the loop ends without breaking, and it is its own block.
    "a loop's 'else' block carries its own" in {
      run("""for i in 0..<2
            |    print("body", i)
            |else
            |    defer print("else out")
            |    print("else in")
            |""".stripMargin) shouldBe "body 0\nbody 1\nelse in\nelse out\n"
    }
  }

  /** The program is refused with this message and no other, at this `line:column`. */
  private def refused(src: String, message: String, at: String): Unit = {
    val said = err(src)

    withClue(said) {
      said.linesIterator.count(_.startsWith("error: ")) shouldBe 1
      said should include(s"error: $message\n")
      said should include(s"--> <input>:$at\n")
    }
  }

  private val loopEdge =
    "a deferred statement runs while its block is being left, so it cannot 'break' or 'continue' " +
      "— the loop edge it would take is the one already being taken"

  private val noExit =
    "a deferred statement runs while its block is being left, so it cannot 'return' — there is " +
      "no exit left to take. Compute what the function returns before the block ends"

  private val noFrameLeft =
    "'become' replaces this frame with the call's, and this function has a 'defer', which runs " +
      "on the way out of its scope — and the jump is the way out, so there would be nowhere " +
      "left to run it"

  // A deferred statement may not contain a jump that leaves it, at whatever depth the jump is
  // written: a `return`, or a `break`/`continue` of a loop that was already open where the `defer`
  // stands. Laying such a jump down runs the block's deferred statements, of which it is one, so
  // there is nothing to lower it to. Each is told what the direct form is told, at the jump.
  "a jump that leaves the deferred statement, written inside it" - {
    "a 'break' of the loop around a deferred 'if' is refused where it stands" in {
      refused("""for i in 0..<3
                |    defer if i == 1 then break
                |    print(i)
                |""".stripMargin, loopEdge, "2:26")
    }

    "a 'return' under a deferred 'if' is refused where it stands" in {
      refused("""f(c: bool) -> int
                |    defer if c then return 1
                |    2
                |
                |print(f(true))
                |""".stripMargin, noExit, "2:21")
    }

    "a 'continue' of the loop around a deferred 'if' is refused" in {
      refused("""for i in 0..<3
                |    defer if i == 1 then continue
                |    print(i)
                |""".stripMargin, loopEdge, "2:26")
    }

    "a labelled 'break' of an outer loop from a loop inside the deferred statement is refused" in {
      refused("""'outer for i in 0..<3
                |    defer for j in 0..<3
                |        if j == 1 then break 'outer
                |
                |    print(i)
                |""".stripMargin, loopEdge, "3:24")
    }

    "a labelled 'continue' of an outer loop is refused the same way" in {
      refused("""'outer for i in 0..<3
                |    defer for j in 0..<3
                |        if j == 1 then continue 'outer
                |
                |    print(i)
                |""".stripMargin, loopEdge, "3:24")
    }

    "a 'return' several statements deep in a deferred block is refused at the 'return'" in {
      refused("""f(c: bool) -> int
                |    defer if c
                |        print("a")
                |        print("b")
                |
                |        for j in 0..<2
                |            print(j)
                |            if j == 1 then return 7
                |
                |        print("c")
                |
                |    2
                |
                |print(f(true))
                |""".stripMargin, noExit, "8:28")
    }

    "two jumps in one deferred statement are one refusal, of the first in source order" in {
      refused("""f(n: int) -> int
                |    for i in 0..<n
                |        defer if i == 1 then break else if i == 2 then return 5
                |        print(i)
                |
                |    2
                |
                |print(f(3))
                |""".stripMargin, loopEdge, "3:30")
    }

    "a 'return' in an arm of a deferred 'match' is refused" in {
      refused("""f(n: int) -> int
                |    defer n match
                |        0 -> print("zero")
                |        _ -> return 9
                |
                |    2
                |
                |print(f(1))
                |""".stripMargin, noExit, "4:14")
    }

    "a 'return' in a 'defer' written inside a deferred statement is one refusal" in {
      refused("""f(c: bool) -> int
                |    defer if c
                |        defer if c then return 1
                |        print("inner")
                |
                |    2
                |
                |print(f(true))
                |""".stripMargin, noExit, "3:25")
    }

    // The jump is what is wrong, so its value is not read and the loop hears nothing of it: no
    // second complaint about a loop whose `break` and `else` disagree.
    "a 'break' that leaves the deferred statement is refused before its value is read" in {
      refused("""val r = for i in 0..<3
                |    defer if i == 1 then break "early"
                |    print(i)
                |else 0
                |
                |print(r)
                |""".stripMargin, loopEdge, "2:26")
    }

    // A `loop` takes its type from its breaks, and the one it has was refused unread — so it has
    // no type to be wrong about, and nothing after it is complained of on its account.
    "a refused 'break' out of a 'loop' leaves the loop's result unquestioned" in {
      refused("""var n = 0
                |
                |val r = loop
                |    defer if n > 2 then break n
                |    n += 1
                |
                |print(r)
                |""".stripMargin, loopEdge, "4:25")
    }

    "a 'return' under a deferred 'if' does not have its value read either" in {
      refused("""f(c: bool) -> int
                |    defer if c then return nope
                |    2
                |
                |print(f(true))
                |""".stripMargin, noExit, "2:21")
    }

    // A refusal of another kind, and the only one: the jump names no loop at all, so there is no
    // edge it could have left by.
    "a 'break' under a deferred 'if' with no loop around it is told there is no loop" in {
      refused("""work(c: bool)
                |    defer if c then break
                |    print("body")
                |
                |work(true)
                |""".stripMargin, "'break' is only allowed inside a loop", "2:21")
    }

    "a deferred 'become' is refused at its call" in {
      refused("""g(n: int) -> int = n
                |f(c: bool) -> int
                |    defer become g(1)
                |    2
                |print(f(true))
                |""".stripMargin, noFrameLeft, "3:18")
    }

    "a 'become' under a deferred 'if' is refused the same way" in {
      refused("""g(n: int) -> int = n
                |f(c: bool) -> int
                |    defer if c then become g(1)
                |    2
                |print(f(true))
                |""".stripMargin, noFrameLeft, "3:28")
    }
  }

  "a jump that stays inside the deferred statement" - {
    "a 'break' of a loop written inside it leaves that loop" in {
      run("""work()
            |    defer for i in 0..<5
            |        if i == 2 then break
            |        print(i)
            |
            |    print("body")
            |
            |work()
            |""".stripMargin) shouldBe "body\n0\n1\n"
    }

    "a 'continue' of a loop written inside it restarts that loop" in {
      run("""work()
            |    defer for i in 0..<4
            |        if i == 1 then continue
            |        print(i)
            |
            |    print("body")
            |
            |work()
            |""".stripMargin) shouldBe "body\n0\n2\n3\n"
    }

    "a labelled jump to a loop written inside it stays inside it" in {
      run("""work()
            |    defer 'rows for i in 0..<3
            |        for j in 0..<3
            |            if j == 2 then continue 'rows
            |            if i == 2 then break 'rows
            |            print(i, j)
            |
            |    print("body")
            |
            |work()
            |""".stripMargin) shouldBe "body\n0 0\n0 1\n1 0\n1 1\n"
    }

    "a 'break' carrying a value out of a loop inside a deferred block gives the loop its value" in {
      run("""work(c: bool)
            |    defer if c
            |        val hit = for j in 0..<9
            |            if j * j > 10 then break j
            |        else -1
            |
            |        print(hit)
            |
            |    print("body")
            |
            |work(true)
            |""".stripMargin) shouldBe "body\n4\n"
    }

    "a 'while' and a 'loop' written as deferred statements are left by their own 'break'" in {
      run("""work()
            |    defer while true do break
            |    defer loop
            |        print("once")
            |        break
            |
            |    print("body")
            |
            |work()
            |""".stripMargin) shouldBe "body\nonce\n"
    }

    "a loop inside a deferred 'for const' is left by its own 'break'" in {
      run("""work()
            |    defer for const k in 0..<2
            |        for j in 0..<3
            |            if j == 1 then break
            |            print(k, j)
            |
            |    print("body")
            |
            |work()
            |""".stripMargin) shouldBe "body\n0 0\n1 0\n"
    }

    "a 'return' in a closure written inside it leaves the closure" in {
      run("""twice(f: int -> int, x: int) -> int = f(f(x))
            |
            |work()
            |    defer print(twice((x) ->
            |        if x > 3 then return 100
            |        x + 3, 1))
            |
            |    print("body")
            |
            |work()
            |""".stripMargin) shouldBe "body\n100\n"
    }

    "a 'return' in a nested function written inside it leaves that function" in {
      run("""work(c: bool)
            |    defer if c
            |        pick(n: int) -> int
            |            if n > 1 then return 10
            |            n
            |
            |        print(pick(1), pick(2))
            |
            |    print("body")
            |
            |work(true)
            |""".stripMargin) shouldBe "body\n1 10\n"
    }
  }

  // A deferred statement is laid down once for each edge that leaves its block, so a local it
  // declares is declared once per copy. The copies share the local's slot: they lie on different
  // ways out, and each writes the slot before reading it.
  "a deferred statement that declares a local, in a block with more than one exit" - {
    val twoExits =
      """f(c: bool)
        |    defer for j in 0..<2
        |        print(j)
        |    if c then return
        |    print("x")
        |f(true)
        |f(false)
        |""".stripMargin

    "runs on each of them" in {
      run(twoExits) shouldBe "0\n1\nx\n0\n1\n"
    }

    "and its copies share the one slot" in {
      "%j\\.addr = alloca".r.findAllIn(ir(twoExits)).length shouldBe 1
    }

    "under a labelled loop left from a loop inside it" in {
      run("""'outer for i in 0..<2
            |    defer for j in 0..<2
            |        print(i, j)
            |
            |    for k in 0..<2
            |        if i == 1 then break 'outer
            |
            |    print("end", i)
            |""".stripMargin) shouldBe "end 0\n0 0\n0 1\n1 0\n1 1\n"
    }

    "a counted local in a deferred block is released by whichever copy ran" in {
      run("""struct Box
            |    n: int
            |
            |f(c: bool) -> int
            |    defer if true
            |        val b: &Box = Box(7)
            |        print("box", b.n)
            |
            |    if c then return 1
            |    2
            |
            |print(f(true))
            |print(f(false))
            |""".stripMargin) shouldBe "box 7\n1\nbox 7\n2\n"
    }

    "a pattern binding in a deferred 'match', across 'continue', 'break' and the end of the body" in {
      run("""for i in 0..<4
            |    defer Some(i) match
            |        Some(v) -> print("out", v)
            |        None -> print("none")
            |
            |    if i == 1 then continue
            |    if i == 2 then break
            |    print("in", i)
            |""".stripMargin) shouldBe "in 0\nout 0\nout 1\nout 2\n"
    }

    "an iteration, an 'is' binding and an array in one deferred block, across three exits" in {
      run("""f(n: int) -> Result[int, string]
            |    defer if n >= 0
            |        var xs: [3]int = [1, 2, 3]
            |        var sum = 0
            |
            |        for x in xs[0..<3]
            |            sum += x
            |
            |        val o: Option[int] = Some(sum + n)
            |
            |        if o is Some(v) then print("out", v)
            |
            |    if n == 1 then return Ok(10)
            |    if n == 2 then return Err("two")
            |    Ok(0)
            |
            |for i in 0..<3
            |    f(i) match
            |        Ok(v) -> print("ok", v)
            |        Err(e) -> print("err", e)
            |""".stripMargin) shouldBe "out 6\nok 0\nout 7\nok 10\nout 8\nerr two\n"
    }

    "a closure in a deferred statement is built by each copy" in {
      run("""twice(f: int -> int, x: int) -> int = f(f(x))
            |
            |f(c: bool)
            |    defer print(twice((x) -> x + 3, 1))
            |    if c then return
            |    print("x")
            |
            |f(true)
            |f(false)
            |""".stripMargin) shouldBe "7\nx\n7\n"
    }

    "a loop ahead of a closing 'return' runs once, the copy at the block's end being unreached" in {
      run("""f()
            |    defer for j in 0..<2
            |        print(j)
            |
            |    print("x")
            |    return
            |
            |f()
            |""".stripMargin) shouldBe "x\n0\n1\n"
    }

    "a 'while' with a 'variant' shares the slots its measure is kept in" in {
      run("""f(c: bool)
            |    defer if true
            |        var k = 2
            |
            |        while k > 0
            |            variant k
            |            print(k)
            |            k -= 1
            |
            |    if c then return
            |    print("x")
            |
            |f(true)
            |f(false)
            |""".stripMargin) shouldBe "2\n1\nx\n2\n1\n"
    }

    "a 'defer' with a local, written inside a deferred block, runs at that block's end each time" in {
      run("""f(c: bool, d: bool)
            |    defer if true
            |        defer for j in 0..<2
            |            print(j)
            |
            |        if d
            |            print("d")
            |        else
            |            print("not d")
            |
            |    if c then return
            |    print("x")
            |
            |f(true, true)
            |f(false, false)
            |""".stripMargin) shouldBe "d\n0\n1\nx\nnot d\n0\n1\n"
    }
  }

  // A slot can be shared by the copies of a deferred statement and a register cannot: it is defined
  // by one instruction, once. So where a local's address is a register — a `ref`, and an array moved
  // to a buffer — the first copy keeps the local's name and each later one takes `.d1`, `.d2`.
  "a deferred statement whose local's address is a register, in a block with more than one exit" - {
    def count(text: String, piece: String): Int = text.sliding(piece.length).count(_ == piece)

    val refTwoExits =
      """struct P
        |    a: int
        |    b: int
        |
        |f(c: bool)
        |    var p = P(1, 2)
        |
        |    defer if true
        |        ref r = p.b
        |
        |        r += 10
        |        print(r)
        |
        |    if c then return
        |    print("x")
        |
        |f(true)
        |f(false)
        |""".stripMargin

    "a 'ref' names its place on each of them" in {
      run(refTwoExits) shouldBe "12\nx\n12\n"
    }

    "and the second copy's address is a register of its own, read by that copy's uses" in {
      val text = ir(refTwoExits)

      count(text, "  %r.addr = getelementptr i8, ptr ") shouldBe 1
      count(text, "  %r.d1.addr = getelementptr i8, ptr ") shouldBe 1
      count(text, ", ptr %r.d1.addr\n") shouldBe 3
      count(text, "%r.d2") shouldBe 0
    }

    "a third copy takes the next name" in {
      val three =
        """struct P
          |    a: int
          |    b: int
          |
          |f(n: int)
          |    var p = P(1, n)
          |
          |    defer if true
          |        ref r = p.b
          |
          |        r += 10
          |        print(r)
          |
          |    if n == 1 then return
          |    if n == 2 then return
          |    print("x")
          |
          |f(1)
          |f(2)
          |f(3)
          |""".stripMargin

      run(three) shouldBe "11\n12\nx\n13\n"
      count(ir(three), "  %r.d2.addr = getelementptr i8, ptr ") shouldBe 1
    }

    "a 'ref' ahead of a closing 'return' takes no second name, its second copy being unreached" in {
      val closing =
        """struct P
          |    a: int
          |    b: int
          |
          |f(c: bool)
          |    var p = P(1, 2)
          |
          |    defer if true
          |        ref r = p.b
          |
          |        if c then r += 10
          |
          |        print(r)
          |
          |    print("x")
          |    return
          |
          |f(true)
          |f(false)
          |""".stripMargin

      run(closing) shouldBe "x\n12\nx\n2\n"
      count(ir(closing), "%r.d1") shouldBe 0
    }

    val arrayTwoExits =
      """var kept: []int = []
        |
        |f(c: bool)
        |    defer if true
        |        var a = [1, 2, 3]
        |
        |        a[0] = if c then 7 else 9
        |        kept = a[..]
        |
        |    if c then return
        |    print("x")
        |
        |f(true)
        |print(kept[0])
        |f(false)
        |print(kept[0])
        |""".stripMargin

    "an array whose view gets out is a buffer of its own on each of them" in {
      run(arrayTwoExits) shouldBe "7\nx\n9\n"
    }

    "and the copies share the slot holding the box while each names its own elements" in {
      val text = ir(arrayTwoExits)

      count(text, "  %a.box = alloca ptr\n") shouldBe 1
      count(text, "  %a.addr = getelementptr ") shouldBe 1
      count(text, "  %a.d1.addr = getelementptr ") shouldBe 1
    }

    "an array whose view gets out, ahead of a closing 'return', runs once per call" in {
      run("""var kept: []int = []
            |
            |f(c: bool)
            |    defer if true
            |        var a = [1, 2, 3]
            |
            |        a[0] = if c then 7 else 9
            |        kept = a[..]
            |
            |    print("x")
            |    return
            |
            |f(true)
            |print(kept[0])
            |f(false)
            |print(kept[0])
            |""".stripMargin) shouldBe "x\n7\nx\n9\n"
    }

    "a view taken through a 'ref' to such an array counts against the copy's own buffer" in {
      run("""var kept: []int = []
            |
            |f(c: bool)
            |    defer if true
            |        var a = [1, 2, 3]
            |        ref r = a
            |
            |        r[0] = if c then 7 else 9
            |        kept = r[..]
            |
            |    if c then return
            |    print("x")
            |
            |f(true)
            |print(kept[0])
            |f(false)
            |print(kept[0])
            |""".stripMargin) shouldBe "7\nx\n9\n"
    }
  }

  "a real resource" - {
    // What the whole form exists for, run against libc rather than simulated: the descriptor is
    // closed on the failure path without the failure path saying so. `close` on an already-closed
    // descriptor returns -1, so the second close reporting failure is what says the first happened.
    "closes a descriptor on the path that did not open successfully" in {
      run("""extern open(path: *u8, flags: i32) -> i32
            |extern close(fd: i32) -> i32
            |
            |read_it() -> Result[int, string]
            |    var fd = open(c"/etc/hosts", 0i32)
            |    if fd < 0i32 then return Err("cannot open")
            |    defer close(fd)
            |
            |    if fd >= 0i32 then return Err("gave up early")
            |    Ok(int(fd))
            |
            |read_it() match
            |    Ok(v) -> print("ok", v)
            |    Err(e) -> print("err", e)
            |""".stripMargin) shouldBe "err gave up early\n"
    }
  }
}
