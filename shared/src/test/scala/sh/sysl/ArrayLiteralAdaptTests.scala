package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** An array literal with no element type asked of it settles one from its own elements, and a bare
 * literal among them adapts to an element that has a type — the rule `n + 1` already follows for the
 * two sides of an operator.
 *
 * Found writing slate: `val xs = [1usize, 2, 7]` was refused with *"an array literal needs one
 * element type, got usize and int"*, although `val n: usize = 2` makes the same `2` a `usize` without
 * a word. Each element was read alone, so the bare ones fell to `int` before anything could tell them
 * otherwise. Every program below reads an element back into a `usize` (or the width in question),
 * which is refused unless the array's element type really is that width.
 */
class ArrayLiteralAdaptTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "a bare literal adapts to a typed element of the same array literal" - {

    "when the typed element is first" in {
      run("val xs = [1usize, 2, 7]\nval n: usize = xs[1] + xs[2]\nprint(n)") shouldBe "9\n"
    }

    "when it is second, since the element that knows need not lead" in {
      run("val xs = [1, 2usize, 7]\nval n: usize = xs[0] + xs[2]\nprint(n)") shouldBe "8\n"
    }

    "when it is last" in {
      run("val xs = [1, 2, 7usize]\nval n: usize = xs[0]\nprint(n)") shouldBe "1\n"
    }

    "when what knows is a name rather than a suffix" in {
      run("val k: usize = 5\nval xs = [1, k, 3]\nval n: usize = xs[0] + xs[1]\nprint(n)") shouldBe "6\n"
    }

    "and a negative literal adapts the same way" in {
      run("val xs = [-1, 2i8]\nval n: i8 = xs[0]\nprint(n)") shouldBe "-1\n"
    }

    "reading the literal at the settled width, not at int first, so a value past int is fine" in {
      run("val xs = [1u64, 5000000000]\nval n: u64 = xs[1]\nprint(n)") shouldBe "5000000000\n"
    }

    "a float literal adapts to a suffixed float beside it" in {
      run("val xs = [1.5f32, 2.25]\nval f: f32 = xs[1]\nprint(f)") shouldBe "2.25\n"
    }

    "and a nested literal settles row by row, from whichever row has a type" in {
      run("val ys = [[1usize, 2], [3, 4]]\nval n: usize = ys[1][1]\nprint(n)") shouldBe "4\n"
      run("val ys = [[1, 2], [3usize, 4]]\nval n: usize = ys[0][1]\nprint(n)") shouldBe "2\n"
    }
  }

  "what the element type settles on still has to hold every element" - {

    "a literal that does not fit the settled width is refused, naming the literal" in {
      val e = err("val zs = [1u8, 300]\nprint(zs[0])")

      e should include("the literal 300 does not fit byte")
      e should not include "one element type"
    }

    "two elements that each have a type, and disagree, are still refused" in {
      err("val zs = [1u8, 2usize]\nprint(zs[0])") should
        include("an array literal needs one element type, got byte and usize")
    }

    // An integer literal never becomes a float on its own (`Literals.floatLiteral`), and that holds
    // inside an array as it does in `val r: real = 1`: the `2` is an `int` beside a `real`.
    "an integer literal beside a real is not made a real" in {
      err("val rs = [1.5, 2]\nprint(rs[0])") should
        include("an array literal needs one element type, got real and int")
    }
  }
}
