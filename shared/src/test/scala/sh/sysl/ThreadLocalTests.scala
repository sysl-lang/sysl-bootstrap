package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@thread_local` — one copy of a module `var`'s storage per thread
 * (`reference/attributes.md § @thread_local`).
 *
 * The suite is in two halves and they answer different questions. The **IR** half says what the
 * back end was told, which is the whole of the claim for anything a program cannot read back: the
 * storage class, and that it composes with the other two attributes about storage. The **run** half
 * is the one that matters, because a `thread_local` the linker quietly turned into an ordinary
 * global emits text that looks right and produces a program in which two threads share a counter —
 * so every claim here is written so that a shared object gives a *different answer*, not a slower
 * one.
 *
 * The sharpest of them is the main thread's own copy. Two workers each counting to five leave a
 * shared counter at ten and a per-thread one at zero, and zero is a number no arrangement of a
 * shared object produces.
 */
class ThreadLocalTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  private val threading = "import sysl.posix.threads.*\n\n"

  "@thread_local marks module storage" - {
    "a 'static var' becomes a thread-local global" in {
      ir("@thread_local\nstatic var n: u32 = 7u32\nprint(n)") should
        include("thread_local global i32 7")
    }

    // Asserted against the object's own line rather than against the text, because the ownership
    // runtime's reaper slot is a `thread_local` of its own and is in every hosted program.
    "one that asks for nothing is an ordinary global, which is what makes that a claim" in {
      val out = ir("static var n: u32 = 7u32\nprint(n)")

      out should include("global i32 7")
      out should not include "thread_local global i32 7"
    }

    "and so does a plain 'var' in any other file, the two being one declaration" in {
      irOf("m/m.sysl" -> "module m\n\n@thread_local\nvar slot: u32 = 0u32\n",
        "main.sysl" -> "print(m.slot)\n") should include("thread_local global i32 0")
    }

    // No model is named, so the back end chooses from what it knows about the link. A program
    // pinning `localexec` would be deciding that on a linker's behalf.
    "no thread-local model is written, which leaves the choice to the back end" in {
      ir("@thread_local\nstatic var n: u32 = 0u32\nprint(n)") should not include "thread_local("
    }

    "a 'var' with no initializer starts every thread at the type's zero" in {
      ir("@thread_local\nstatic var page: [8]u8\nprint(page[0])") should
        include("thread_local global [8 x i8] zeroinitializer")
    }
  }

  /** The pair `@section` already composes with, plus the new one — all three are facts about where
   * one object's storage lives, so nothing about writing them together is a corner.
   */
  "it composes with the other two attributes about storage" - {
    "beside '@align(n)'" in {
      ir("@thread_local\n@align(64)\nstatic var page: [8]u8\nprint(page[0])") should
        include("thread_local global [8 x i8] zeroinitializer, align 64")
    }

    "beside '@section(\"...\")'" in {
      ir("@thread_local\n@section(\".tdata\")\nstatic var n: u32 = 0u32\nprint(n)") should
        include("thread_local global i32 0, section \".tdata\"")
    }

    "and the order they are written in does not matter" in {
      ir("@section(\".tdata\")\n@align(16)\n@thread_local\nstatic var n: u32 = 0u32\nprint(n)") should
        include("thread_local global i32 0, section \".tdata\", align 16")
    }
  }

  /** Every one of these is written so that a **shared** object answers differently, rather than
   * answering the same thing more slowly.
   */
  "two threads get two copies" - {
    /** The discriminating test for the whole feature. A shared counter reaches ten across the two
     * workers and main reads ten; a per-thread one leaves each worker at five and main — which
     * never ran the loop — at nought.
     */
    "each thread counts its own, and the main thread's copy is untouched" in {
      run(threading +
        """@thread_local
          |static var count: int = 0
          |
          |bump(p: *int)
          |    var i = 0
          |
          |    while i < 5
          |        count++
          |        i++
          |
          |    *p = count
          |
          |var a = 0
          |var b = 0
          |var one = spawn(&bump, &a).unwrap()
          |var two = spawn(&bump, &b).unwrap()
          |
          |one.join()
          |two.join()
          |print(a, b, count)""".stripMargin
      ) shouldBe "5 5 0\n"
    }

    /** slate's `the_vm` written out: a pointer to the machine this thread is running, set by
     * whichever thread entered one. A shared slot would leave main holding a pointer into a frame
     * that has already gone, which is the defect the attribute exists to make unwriteable.
     */
    "a thread-local pointer a worker sets is still null on the thread that never set one" in {
      run(threading +
        """struct Machine
          |    id: int
          |
          |@thread_local
          |static var running: *Machine = null
          |
          |enter(p: *bool)
          |    var m = Machine(9)
          |
          |    running = &m
          |    *p = running != null && running.id == 9
          |
          |var saw = false
          |
          |spawn(&enter, &saw).unwrap().join()
          |print(saw, running == null)""".stripMargin
      ) shouldBe "true true\n"
    }

    /** `&name` is an ordinary address-of and answers **this** thread's copy, which is what makes a
     * thread-local reachable through a pointer at all. Two threads taking it get two addresses; the
     * claim a program can check is that writing through one is writing to the name.
     */
    "'&name' answers the address of this thread's copy" in {
      run(threading +
        """@thread_local
          |static var slot: int = 0
          |
          |fill(p: *bool)
          |    var here = &slot
          |
          |    *here = 42
          |    *p = slot == 42
          |
          |var wrote = false
          |
          |spawn(&fill, &wrote).unwrap().join()
          |print(wrote, slot)""".stripMargin
      ) shouldBe "true 0\n"
    }

    /** A `&T` is a non-atomic count and is fine here for the reason the doc gives: the value belongs
     * to one thread by construction, so there is no second thread to tear the count. This is not a
     * domain crossing and nothing asks it to be `&sync`.
     */
    "a counted value may be held, the storage belonging to one thread by construction" in {
      run(threading +
        """struct Node
          |    value: int
          |
          |@thread_local
          |static var held: Option[&Node] = None
          |
          |keep(p: *int)
          |    held = Some(Node(3))
          |    *p = held.unwrap().value
          |
          |var got = 0
          |
          |spawn(&keep, &got).unwrap().join()
          |print(got, held.is_none())""".stripMargin
      ) shouldBe "3 true\n"
    }
  }

  "the initializer has to be a value the compiler can write down" - {
    /** The rule, and the sentence a reader gets. Every thread's copy is made from one image in the
     * object file, so there is nowhere for code to run once per thread.
     */
    "a call is not one" in {
      val e = err("""@thread_local
                    |static var n: int = size()
                    |
                    |size() -> int = 4
                    |
                    |print(n)""".stripMargin)

      e should include("every thread gets a copy of the initial value")
      e should include("rather than by code that runs per thread")
    }

    "a literal is" in {
      ir("@thread_local\nstatic var n: int = 4\nprint(n)") should include("thread_local global")
    }

    "and so is a 'const', which folds to one" in {
      ir("const Start: int = 9\n@thread_local\nstatic var n: int = Start\nprint(n)") should
        include("thread_local global i32 9")
    }

    "and so is a struct of constants" in {
      ir("""struct Point
           |    x: int
           |    y: int
           |
           |@thread_local
           |static var origin: Point = Point(1, 2)
           |print(origin.x)""".stripMargin) should include("thread_local global")
    }

    // A plain module `var` takes a computed initializer perfectly well — it is filled by the
    // prologue `main` opens with, which runs on one thread. So this is a rule the attribute adds
    // rather than one the declaration already had.
    "the same initializer is fine without the attribute, which is what makes this the rule's own" in {
      ir("""static var n: int = size()
           |
           |size() -> int = 4
           |
           |print(n)""".stripMargin) should include("@n")
    }
  }

  "a target with no thread-local storage refuses it, rather than emitting something silent" - {
    /** The refusal names the target, because the reader's next question is *which* one. What it
     * would otherwise be is the worst case in the file: LLVM accepts the keyword everywhere and a
     * freestanding target silently gets the local-exec model, whose offset is read from a register
     * nothing on a bare machine has written.
     */
    "a freestanding target names itself" in {
      val e = errFor(Target.craftFreestanding,
        "@thread_local\nstatic var n: u32 = 0u32\nprint(n)")

      e should include("cannot be '@thread_local'")
      e should include(Target.craftFreestanding.name)
    }

    "and points at the pattern a bare target actually has" in {
      errFor(Target.riscv32Freestanding,
        "@thread_local\nstatic var n: u32 = 0u32\nprint(n)") should
        include("let the port's scheduler answer for which task is running")
    }

    "wasm is freestanding too, the keyword being accepted there and silently meaningless" in {
      errFor(Target.wasm32Freestanding,
        "@thread_local\nstatic var n: u32 = 0u32\nprint(n)") should
        include("cannot be '@thread_local'")
    }

    "and the same declaration compiles for the target this suite runs on" in {
      irFor(Target.default, "@thread_local\nstatic var n: u32 = 0u32\nprint(n)") should
        include("thread_local global")
    }
  }

  "it marks a 'var', and only a 'var'" - {
    "a 'val' never changes, so one copy of it is already every thread's" in {
      val e = err("@thread_local\nstatic val n: int = 1\nprint(n)")

      e should include("a 'val' never changes")
      e should include("a per-thread constant is a constant")
    }

    "a 'const' is folded into every use and declares no storage at all" in {
      err("@thread_local\nconst N: int = 4\nprint(N)") should include("declares no storage at all")
    }

    "an 'extern' names storage this program does not lay down" in {
      err("@thread_local\nextern errno: int\nprint(errno)") should
        include("declares no storage at all")
    }

    "a function is code every thread runs the one copy of" in {
      err("@thread_local\nf() -> int = 1\nprint(f())") should include("declares no storage at all")
    }

    "a struct is a type rather than an object" in {
      err("@thread_local\nstruct S\n    a: int\n") should include("declares no storage at all")
    }

    "and a binding that names several has no one object to be about" in {
      err("@thread_local\nstatic var a, b = 1, 2\nprint(a)") should include("names several")
    }
  }

  /** A local and module storage are the same syntax and differ only in where they stand
   * (`reference/modules.md § Where a program starts`), so this is the analyzer's refusal rather than
   * the grammar's — exactly as `@section`'s is.
   */
  "a local is already one thread's" - {
    "a 'var' inside a function" in {
      val e = err("f() -> int\n    @thread_local\n    var n: int = 1\n    n\n\nprint(f())")

      e should include("is a local, and a local is already one thread's")
      e should include("every thread has a stack of its own")
    }

    // The entry file is the case worth pinning: a top-level `var` there is a local of the entry
    // point, which is exactly why the grammar cannot answer this.
    "and a top-level 'var' in the file the program starts in, which is one" in {
      err("@thread_local\nvar n: int = 1\nprint(n)") should
        include("is a local, and a local is already one thread's")
    }

    "while the 'static' spelling beside it is module storage and is accepted" in {
      ir("@thread_local\nstatic var n: int = 1\nprint(n)") should include("thread_local global")
    }
  }

  "it does not stand beside an annotation about something else" - {
    "one about a function" in {
      err("@thread_local\n@pure\nf() -> int = 1\nprint(f())") should
        include("the other two about storage")
    }

    "one about a layout" in {
      err("@thread_local\n@packed\nstruct S\n    a: int\n") should
        include("the other two about storage")
    }
  }

  "one written twice is refused, as any repeated attribute is" in {
    err("@thread_local\n@thread_local\nstatic var n: int = 1\nprint(n)") should
      include("written twice")
  }
}
