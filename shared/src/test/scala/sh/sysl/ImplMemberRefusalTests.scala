package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What an `impl` block's refusals say, and how much of the block they take with them.
 *
 * A member of an `impl` that collides with one the type already has is refused, and that refusal
 * is the whole of the report: the block's other members are still filed and still checked, so a
 * call to one of them — a trait default's, or a generic body's in another module — finds it rather
 * than being told the type has no such method.
 */
class ImplMemberRefusalTests extends AnyFreeSpec with CodegenSupport {

  private def errors(e: String): Int = e.split("error:").length - 1

  "a member colliding with the type's own is refused on its own" - {
    val stream =
      """trait Stream
        |    look(self) -> int
        |    take(*self) -> int
        |    both(*self) -> int = self.look() + self.take()
        |struct P
        |    n: int
        |    look(self) -> int = self.n
        |""".stripMargin

    "and it is the only error, though the block's other members and the trait's default are called" in {
      val e = err(
        stream +
          """impl Stream for P
            |    look(self) -> int = self.n
            |    take(*self) -> int = self.n
            |var p = P(1)
            |print(p.both())
            |print(p.take())
            |""".stripMargin
      )

      e should include("type 'P' already has a member named 'look'")
      e should not include "has no method"
      errors(e) shouldBe 1
    }

    // The members after the refused one are not merely registered but analyzed: a mistake in one of
    // them is still found, beside the collision rather than instead of it.
    "and the block's other members are still checked" in {
      val e = err(
        stream +
          """impl Stream for P
            |    look(self) -> int = self.n
            |    take(*self) -> int = "no"
            |print(P(1).n)
            |""".stripMargin
      )

      e should include("already has a member named 'look'")
      e should include("--> <input>:10:")
      errors(e) shouldBe 2
    }

    // The case the org met it in: the trait and a generic body calling its members live in another
    // module, and the call that could not find a member was reported there, in source the consumer
    // never wrote.
    "and a generic body in the module declaring the trait still finds the other members" in {
      val e = errIn(
        ("", "main.sysl",
          """struct P
            |    n: int
            |    look(self) -> int = self.n
            |impl parsing.Stream for P
            |    look(self) -> int = self.n
            |    take(self) -> int = self.n + 1
            |print(parsing.drive(P(1)))
            |""".stripMargin),
        ("parsing", "pratt.sysl",
          """module parsing
            |trait Stream
            |    look(self) -> int
            |    take(self) -> int
            |    both(self) -> int = self.look() + self.take()
            |drive[S: Stream](s: S) -> int = s.take() + s.both()
            |""".stripMargin),
      )

      e should include("type 'P' already has a member named 'look'")
      e should not include "pratt.sysl"
      errors(e) shouldBe 1
    }
  }

  "a parameter count that differs from the trait's is counted in words that agree with it" - {
    def refusal(declared: String, written: String): String =
      err(
        s"""trait T
           |    f(self$declared) -> int
           |struct S
           |    n: int
           |impl T for S
           |    f(self$written) -> int = 0
           |print(S(1).n)
           |""".stripMargin
      )

    "one written where the trait has two" in {
      refusal(", a: int, b: int", ", a: int") should include(
        "method 'f' of 'impl T for S' takes 1 parameter, but the trait declares 2")
    }

    "none written where the trait has one" in {
      refusal(", a: int", "") should include(
        "method 'f' of 'impl T for S' takes 0 parameters, but the trait declares 1")
    }

    "two written where the trait has none" in {
      refusal("", ", a: int, b: int") should include(
        "method 'f' of 'impl T for S' takes 2 parameters, but the trait declares 0")
    }

    "two written where the trait has one" in {
      refusal(", a: int", ", a: int, b: int") should include(
        "method 'f' of 'impl T for S' takes 2 parameters, but the trait declares 1")
    }
  }
}
