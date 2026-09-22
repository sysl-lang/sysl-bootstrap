package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A function that calls itself as the last thing it does reuses its own frame
 * (`reference/declarations.md § Tail calls`).
 *
 * The runtime cases are all deep enough that a second frame per call would exhaust the stack, which
 * is the only assertion that distinguishes the jump from a call the machine happens to survive: a
 * recursion of ten works either way. The IR cases are the other half — that the ordinary shapes
 * really do jump, and that the shapes which must *not* jump still emit the call.
 */
class TailCallTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** Deeper than any stack will hold a frame apiece for. */
  private val deep = 1000000

  /** Past `Layout.DirectBytes`, so it is returned through the caller's storage and passed by
   * address — the two paths a jump has to carry that a register-sized value does not.
   */
  private val big =
    """struct Big
      |    tag: int
      |    data: [40]int
      |""".stripMargin

  "the jump" - {
    "a self-call in the trailing expression becomes a branch, not a call" in {
      val out = defineOf(
        ir("""count(n: int, acc: int) -> int =
             |    if n == 0 then acc else count(n - 1, acc + n)
             |print(count(5, 0))""".stripMargin),
        "count",
      )

      out should include("br label %tailrec1")
      out should not include "call i32 @count"
    }

    "the arguments land in the parameter slots the body reads" in {
      val out = defineOf(
        ir("""count(n: int, acc: int) -> int =
             |    if n == 0 then acc else count(n - 1, acc + n)
             |print(count(5, 0))""".stripMargin),
        "count",
      )

      out should include("store i32 %t6, ptr %n.addr")
      out should include("store i32 %t9, ptr %acc.addr")
    }

    "the frame is entered once however deep the recursion goes" in {
      run(
        s"""count(n: int, acc: int) -> int =
           |    if n == 0 then acc else count(n - 1, acc + 1)
           |print(count($deep, 0))""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "a self-call in a 'return' is a tail call too" in {
      run(
        s"""count(n: int, acc: int) -> int
           |    if n == 0 then
           |        return acc
           |    return count(n - 1, acc + 1)
           |print(count($deep, 0))""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "a self-call in a match arm is a tail call" in {
      run(
        s"""count(n: int, acc: int) -> int
           |    n match
           |        0 -> acc
           |        else count(n - 1, acc + 1)
           |print(count($deep, 0))""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "both arms may jump, and they are independent" in {
      run(
        s"""walk(n: int, evens: int) -> int =
           |    if n == 0 then evens
           |    elif n % 2 == 0 then walk(n - 1, evens + 1)
           |    else walk(n - 1, evens)
           |print(walk($deep, 0))""".stripMargin
      ) shouldBe s"${deep / 2}\n"
    }
  }

  "the awkward shapes" - {
    "a 'return' from inside a loop is still a tail position" in {
      run(
        s"""count(n: int, acc: int) -> int
           |    loop
           |        if n == 0 then
           |            return acc
           |        return count(n - 1, acc + 1)
           |print(count($deep, 0))""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "a 'return' from inside a post-test loop is a tail position too" in {
      // The walk names the loop forms one by one, so a loop form added later is a tail call it stops
      // finding — safely, since an unrecognized one is compiled as the call it already was, but
      // silently. This is the case that says the `do … while` added beside this feature is named.
      // A post-test loop yields `unit` rather than `never` — it is a loop that can complete — so the
      // body needs a trailing expression the `return`s above make unreachable.
      run(
        s"""count(n: int, acc: int) -> int
           |    do
           |        if n == 0 then
           |            return acc
           |        return count(n - 1, acc + 1)
           |    while true
           |
           |    0
           |print(count($deep, 0))""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "a loop's own bindings are released on the way out of the frame" in {
      // `held` is a fresh string per invocation and the jump leaves the loop scope from inside it,
      // so a release the jump skipped would be a million strings the program never gives back —
      // which is why this one is deep and its accumulator is not.
      run(
        s"""count(n: int, acc: string) -> string
           |    loop
           |        var held = acc + ""
           |        if n == 0 then
           |            return held
           |        return count(n - 1, held)
           |print(count($deep, "x"))""".stripMargin
      ) shouldBe "x\n"
    }

    "a function with no parameters jumps with nothing to rebind" in {
      val out = defineOf(
        ir("""spin(stop: bool) -> int =
             |    if stop then 0 else spin(true)
             |print(spin(false))""".stripMargin),
        "spin",
      )

      out should include("tailrec")
    }

    "a generic's instantiation jumps to its own entry" in {
      run(
        s"""count[T: sysl.Sub + sysl.Add + sysl.Eq](n: T, acc: T, zero: T, one: T) -> T =
           |    if n == zero then acc else count(n - one, acc + one, zero, one)
           |var total: i64 = count($deep, 0, 0, 1)
           |print(total)""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "a recursion that ends by trapping still traps" in {
      exits(
        """down(n: int) -> int
          |    require n >= 0
          |    down(n - 1)
          |print(down(3))""".stripMargin
      )
    }
  }

  "what is not a tail call" - {
    "a call whose result is used is still a call" in {
      val out = defineOf(
        ir("""sum(n: int) -> int =
             |    if n == 0 then 0 else n + sum(n - 1)
             |print(sum(5))""".stripMargin),
        "sum",
      )

      out should include("call i32 @sum")
      out should not include "tailrec"
    }

    "a 'defer' in scope holds the frame, so the call stays a call" in {
      val out = defineOf(
        ir("""count(n: int, acc: int) -> int
             |    defer print("out")
             |    if n == 0 then acc else count(n - 1, acc + 1)
             |print(count(2, 0))""".stripMargin),
        "count",
      )

      out should include("call i32 @count")
      out should not include "tailrec"
    }

    "a deferred statement still runs once per call" in {
      run(
        """count(n: int, acc: int) -> int
          |    defer print("out")
          |    if n == 0 then acc else count(n - 1, acc + 1)
          |print(count(2, 0))""".stripMargin
      ) shouldBe "out\nout\nout\n2\n"
    }

    "an 'ensures' is checked when a call returns, so the call is not replaced" in {
      val out = defineOf(
        ir("""count(n: int, acc: int) -> int
             |    ensure result >= acc
             |    if n == 0 then acc else count(n - 1, acc + 1)
             |print(count(3, 0))""".stripMargin),
        "count",
      )

      out should include("call i32 @count")
      out should not include "tailrec"
    }

    // `reference/statements.md § return` says the jump is self-recursion only, and gives the
    // reason: a large argument crosses as the address of the caller's storage, and a frame being
    // replaced cannot be the frame an argument still points into. So `even` ending in `odd` emits
    // an ordinary call.
    //
    // **The assertion is on the IR and not on the depth, deliberately.** A run would be measuring the
    // *back end*: at the default `-O1` LLVM's sibling-call pass turns exactly this pair into jumps
    // and ten million of them return, while the same source at `-O0` dies of a stack overflow. That
    // is a property of clang rather than a promise of sysl's, so what belongs here is what sysl's own
    // walk did, which is the same at every optimization level.
    "a mutual call is a tail call in the same sense and is still emitted as a call" in {
      val out = defineOf(
        ir("""even(n: int) -> bool = if n == 0 then true else odd(n - 1)
             |odd(n: int) -> bool = if n == 0 then false else even(n - 1)
             |print(even(4))""".stripMargin),
        "even",
      )

      // Named without the result's type, which on a widening target carries a `zeroext` the
      // convention asks for: what this is about is that a `call` was emitted at all.
      out should include("call zeroext i1 @odd")
      out should not include "tailrec"
    }
  }

  "the invocation, not the frame" - {
    "a precondition is checked again on every jump" in {
      // 5, 3, 1, -1 — the fourth invocation is the one that breaks the requirement, and it is an
      // invocation the jump made. A loop that entered past the check would run away instead.
      exits(
        """down(n: int) -> int
          |    require n >= 0
          |    if n == 0 then 0 else down(n - 2)
          |print(down(5))""".stripMargin
      )
    }

    "a precondition that holds at every depth lets the recursion finish" in {
      run(
        s"""down(n: int) -> int
           |    require n >= 0
           |    if n == 0 then 0 else down(n - 1)
           |print(down($deep))""".stripMargin
      ) shouldBe "0\n"
    }
  }

  "ownership across the jump" - {
    "a counted argument survives the frame letting go of the old one" in {
      run(
        """join(n: int, acc: string) -> string =
          |    if n == 0 then acc else join(n - 1, acc + "x")
          |print(join(3, ""))""".stripMargin
      ) shouldBe "xxx\n"
    }

    "an argument reading a parameter the jump overwrites sees the value it was called with" in {
      // Each jump swaps the two, so the second argument is read out of the slot the first argument
      // is about to land in. Both are counted, and each is the only holder of its string — so an
      // order that released before retaining would free one of them here rather than at the end.
      run(
        """flip(a: string, b: string, n: int) -> string =
          |    if n == 0 then a + b else flip(b, a, n - 1)
          |print(flip("A", "B", 3))""".stripMargin
      ) shouldBe "BA\n"
    }

    "a counted accumulator does not leak over a deep recursion" in {
      run(
        s"""last(n: int, acc: string) -> string =
           |    if n == 0 then acc else last(n - 1, "y")
           |print(last($deep, "x"))""".stripMargin
      ) shouldBe "y\n"
    }
  }

  "a large value" - {
    "a result returned through memory jumps without a second out-pointer" in {
      val out = defineOf(
        ir(big +
          """grow(n: int, acc: Big) -> Big =
            |    if n == 0 then acc else grow(n - 1, Big(acc.tag + 1, acc.data))
            |var b = grow(3, Big(0, [0; 40]))
            |print(b.tag)""".stripMargin),
        "grow",
      )

      out should include("tailrec")
      out should not include "call void @grow"
    }

    "a large parameter is carried across the jump" in {
      run(big +
        s"""grow(n: int, acc: Big) -> Big =
           |    if n == 0 then acc else grow(n - 1, Big(acc.tag + 1, acc.data))
           |var b = grow($deep, Big(0, [0; 40]))
           |print(b.tag)""".stripMargin
      ) shouldBe s"$deep\n"
    }
  }

  "@tailrec" - {
    "a function whose self-call is a tail call is accepted" in {
      run(
        s"""@tailrec
           |count(n: int, acc: int) -> int =
           |    if n == 0 then acc else count(n - 1, acc + 1)
           |print(count($deep, 0))""".stripMargin
      ) shouldBe s"$deep\n"
    }

    "it does not change what is emitted — the jump applies either way" in {
      val marked = defineOf(
        ir("""@tailrec
             |count(n: int, acc: int) -> int =
             |    if n == 0 then acc else count(n - 1, acc + 1)
             |print(count(5, 0))""".stripMargin),
        "count",
      )
      val plain = defineOf(
        ir("""count(n: int, acc: int) -> int =
             |    if n == 0 then acc else count(n - 1, acc + 1)
             |print(count(5, 0))""".stripMargin),
        "count",
      )

      marked shouldBe plain
    }

    "a recursion the jump cannot replace is refused" in {
      err(
        """@tailrec
          |sum(n: int) -> int =
          |    if n == 0 then 0 else n + sum(n - 1)
          |print(sum(5))""".stripMargin
      ) should include("calls itself nowhere the jump can replace")
    }

    "a function that does not recurse at all is refused" in {
      err(
        """@tailrec
          |double(n: int) -> int = n * 2
          |print(double(5))""".stripMargin
      ) should include("calls itself nowhere the jump can replace")
    }

    "an 'ensures' is named as the reason rather than reported as an absence" in {
      err(
        """@tailrec
          |count(n: int, acc: int) -> int
          |    ensure result >= acc
          |    if n == 0 then acc else count(n - 1, acc + 1)
          |print(count(3, 0))""".stripMargin
      ) should include("postcondition of every frame but the last would go unchecked")
    }

    "a 'defer' in scope is refused, since it is what ends the tail position" in {
      err(
        """@tailrec
          |count(n: int, acc: int) -> int
          |    defer print("out")
          |    if n == 0 then acc else count(n - 1, acc + 1)
          |print(count(2, 0))""".stripMargin
      ) should include("calls itself nowhere the jump can replace")
    }

    "it is checked on a function the build goes on to strip" in {
      // A `@test` function is left out of an ordinary build, and the check still runs on it: the
      // whole tree is read before anything is dropped from it, so a `@tailrec` that has stopped
      // holding is reported in the build that would never have called it. It cannot *satisfy* the
      // attribute — a test takes no parameters and nothing may call it, itself included — so what
      // this pins is that the refusal arrives rather than being skipped.
      err(
        """@test
          |@tailrec
          |counts_down() -> unit =
          |    print("ran")
          |print("main")""".stripMargin
      ) should include("calls itself nowhere the jump can replace")
    }
  }

  "the attribute grammar" - {
    // What this is about is that the *word written* is named back, which is the half `@tailrec` owns.
    // Which annotations exist is `TestAttributeTests`' assertion, and it is asserted there one name
    // at a time — a sentence counting them goes stale every time the set grows, which it did twice
    // in one afternoon when `17` added `@pure` and `@ghost`.
    // The word written here was `@inline` until the language grew one, which is the hazard
    // `TestAttributeTests` records against the same case: one written against a plausible word is
    // one waiting to be rewritten. `@serializable` names a facility sysl has no notion of at all.
    "an attribute sysl does not know is named in the refusal" in {
      err(
        """@serializable
          |double(n: int) -> int = n * 2
          |print(double(5))""".stripMargin
      ) should include("'serializable' is not an annotation a declaration takes")
    }

    "the same attribute twice is refused" in {
      err(
        """@tailrec
          |@tailrec
          |count(n: int, acc: int) -> int =
          |    if n == 0 then acc else count(n - 1, acc + 1)
          |print(count(3, 0))""".stripMargin
      ) should include("written twice above one declaration")
    }

    "an attribute above something that is not a function is refused" in {
      err(
        """@tailrec
          |struct Point
          |    x: int
          |print(1)""".stripMargin
      ) should include("an annotation marks a function, and only a function")
    }
  }
}
