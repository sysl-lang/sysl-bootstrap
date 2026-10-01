package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `x: -> T` — a parameter passed by name (`reference/declarations.md § Default parameters and
 * named arguments`).
  *
  * The feature is one thing the call site does, so the tests are about *when the argument runs*
  * rather than about what it is. Three properties carry the design and each has a test that would
  * fail if it were got backwards:
  *
  *   - the argument is not evaluated where it is written;
  *   - the body evaluates it at **each** use, which is what makes it different from a `val`;
  *   - a body that never uses it never evaluates it at all, which is the point of the feature.
  *
  * `x: () -> T` is the neighbouring case and is tested beside it: same type, different call site.
  * The two spellings have to stay distinguishable, because the shorter one is the one that changes
  * what a caller writes.
  */
class ByNameTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "what by name means" - {

    "the argument is evaluated in the callee, not at the call" in {
      run("""noisy() -> int
            |    print(1)
            |    7
            |
            |take(x: -> int) -> int
            |    print(2)
            |    x
            |
            |print(take(noisy()))
            |""".stripMargin) shouldBe "2\n1\n7\n"
    }

    // The whole of what separates this from a `val`: two uses are two evaluations.
    "it is evaluated once per use" in {
      run("""static var calls: int = 0
            |
            |tick() -> int
            |    calls += 1
            |    calls
            |
            |twice(x: -> int) -> int = x + x
            |
            |print(twice(tick()))
            |print(calls)
            |""".stripMargin) shouldBe "3\n2\n"
    }

    "a body that never uses it never evaluates it" in {
      run("""boom() -> int
            |    print(99)
            |    1
            |
            |ignore(x: -> int) -> int = 5
            |
            |print(ignore(boom()))
            |""".stripMargin) shouldBe "5\n"
    }

    // The case the feature is usually reached for: the expensive argument of a call that may not
    // want it.
    "a guard that decides whether the argument runs at all" in {
      run("""static var built: int = 0
            |
            |message() -> int
            |    built += 1
            |    42
            |
            |log(on: bool, m: -> int)
            |    if on then print(m)
            |
            |log(false, message())
            |log(true, message())
            |print(built)
            |""".stripMargin) shouldBe "42\n1\n"
    }

    "an ordinary expression, not just a call" in {
      run("""twice(x: -> int) -> int = x + x
            |
            |print(twice(3 * 7))
            |""".stripMargin) shouldBe "42\n"
    }

    "several by-name parameters, each independent" in {
      run("""pick(c: bool, a: -> int, b: -> int) -> int
            |    if c then a else b
            |
            |print(pick(true, 1, 2))
            |print(pick(false, 1, 2))
            |""".stripMargin) shouldBe "1\n2\n"
    }

    "a by-name parameter beside ordinary ones" in {
      run("""rep(n: int, x: -> int) -> int
            |    var t = 0
            |    for i in 0..<n
            |        t += x
            |    t
            |
            |print(rep(3, 5))
            |""".stripMargin) shouldBe "15\n"
    }

    "a by-name parameter of a type other than int" in {
      run("""greet(s: -> string)
            |    print(s)
            |    print(s)
            |
            |greet("hi")
            |""".stripMargin) shouldBe "hi\nhi\n"
    }

    // The read is keyed by the **uniqued** name, so a local declared over the parameter is simply a
    // different name and needs no rule of its own. Without that, this would evaluate the thunk.
    "a local shadowing it is an ordinary local" in {
      run("""static var calls: int = 0
            |
            |tick() -> int
            |    calls += 1
            |    calls
            |
            |f(x: -> int) -> int
            |    var x = 100
            |    x + x
            |
            |print(f(tick()))
            |print(calls)
            |""".stripMargin) shouldBe "200\n0\n"
    }

    "it reaches a name the caller had in scope" in {
      run("""twice(x: -> int) -> int = x + x
            |
            |var n = 21
            |print(twice(n))
            |""".stripMargin) shouldBe "42\n"
    }
  }

  "how it differs from the neighbouring spelling" - {

    // `() -> T` is the same type. What differs is that the caller writes the callable, so the
    // argument is a closure rather than an expression that became one.
    "'() -> T' still asks the caller for a callable" in {
      run("""take(f: () -> int) -> int = f() + f()
            |
            |print(take(() -> 21))
            |""".stripMargin) shouldBe "42\n"
    }

    // The by-name body writes the parameter bare, with no call. That is the other half of the
    // sugar: the caller stops writing the closure and the callee stops writing the call.
    "a by-name body names it without calling it" in {
      run("""take(x: -> int) -> int = x + x
            |
            |print(take(21))
            |""".stripMargin) shouldBe "42\n"
    }
  }

  // Each use is a call wherever it is written, so a read inside a closure or a nested function of
  // the body is the same evaluation a read in the body is: the capture holds the callable the call
  // made of the argument, and naming it calls that.
  "read inside a body written in the body" - {

    "a closure of the body calls it" in {
      run("""in_closure(x: -> int) -> int
            |    val g = () -> x + 1
            |    g()
            |
            |print(in_closure(41))
            |""".stripMargin) shouldBe "42\n"
    }

    "a nested function of the body calls it" in {
      run("""in_nested(x: -> int) -> int
            |    inner() -> int = x + 2
            |    inner()
            |
            |print(in_nested(40))
            |""".stripMargin) shouldBe "42\n"
    }

    "a closure evaluates the argument at every call of it" in {
      run("""static var calls: int = 0
            |
            |tick() -> int
            |    calls += 1
            |    print("eval")
            |    calls
            |
            |closure_twice(x: -> int) -> int
            |    val g = () -> x
            |    g() + g()
            |
            |print(closure_twice(tick()))
            |print(calls)
            |""".stripMargin) shouldBe "eval\neval\n3\n2\n"
    }

    // An argument with an effect, so the environment has to hold the callable itself: a capture of
    // anything else would print a different count, or none.
    "a nested function evaluates the argument at every read" in {
      run("""static var calls: int = 0
            |
            |tick() -> int
            |    calls += 1
            |    print("eval")
            |    calls
            |
            |nested_twice(x: -> int) -> int
            |    inner() -> int = x * 10
            |    inner() + inner()
            |
            |print(nested_twice(tick()))
            |print(calls)
            |""".stripMargin) shouldBe "eval\neval\n30\n2\n"
    }

    "a closure that outlives the call evaluates the argument when it is called" in {
      run("""static var calls: int = 0
            |
            |tick() -> int
            |    calls += 1
            |    print("eval")
            |    calls
            |
            |later(x: -> int) -> &Fn() -> int = () -> x * 100
            |
            |val f = later(tick())
            |print("made")
            |print(f())
            |print(f())
            |print(calls)
            |""".stripMargin) shouldBe "made\neval\n100\neval\n200\n2\n"
    }

    "a closure inside a nested function reaches it through both" in {
      run("""deep(x: -> int) -> int
            |    inner() -> int
            |        val g = () -> x * 2
            |        g()
            |    inner()
            |
            |print(deep(21))
            |""".stripMargin) shouldBe "42\n"
    }

    // Walking the closure's body must hand the frame back as it found it, so the body's own reads
    // after one are still calls.
    "the body still calls it after a closure is written" in {
      run("""after(x: -> int) -> int
            |    val g = () -> 1
            |    x + g()
            |
            |print(after(41))
            |""".stripMargin) shouldBe "42\n"
    }

    "calling it inside a closure is refused, as it is in the body" in {
      val body    = err("""in_body(x: -> int) -> int = x() + 1
                           |
                           |print(in_body(41))
                           |""".stripMargin)
      val closure = err("""in_closure(x: -> int) -> int
                           |    val g = () -> x() + 1
                           |    g()
                           |
                           |print(in_closure(41))
                           |""".stripMargin)

      body should include("type 'int' has no method 'call'")
      closure should include("type 'int' has no method 'call'")
    }
  }

  "what it refuses" - {

    "a struct field written by name, which is storage and not a call" in {
      err("""struct Holder
            |    x: -> int
            |end Holder
            |
            |print(1)
            |""".stripMargin) should not be empty
    }
  }
}
