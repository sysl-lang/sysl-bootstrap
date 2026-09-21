package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@noinline` and `@cold` — what a definition tells the optimizer about itself
 * (`reference/attributes.md § @noinline and @cold`).
 *
 * **The point of the pair is a library keeping a rare path out of a hot one, and it is not something
 * the emitted text can show.** A `private` function lowers to `internal`, so with a single call site
 * the inliner folds it back into its caller and the caller is then too large to inline into *its*
 * callers — which is the opposite of what splitting the rare path out was for. So the seam this
 * suite asserts at is the optimizer's output rather than the compiler's: that the definition and the
 * call are still there after `-O2`, against a control that shows the unmarked one is folded away.
 *
 * The two are separate axes and compose, which is LLVM's division rather than one chosen here:
 * `noinline` forbids inlining outright, while `cold` states a frequency and leaves the call
 * eligible. `InliningAttrErrorTests` is the other half — every way of writing one that means
 * nothing.
 */
class InliningAttrTests extends AnyFreeSpec with CodegenSupport {

  "the bare keywords reach the 'define' line" - {

    "'@noinline' on a function" in {
      defineLine(ir("@noinline\nslow(n: i32) -> i32 = n + 1\n\nprint(slow(1))\n"), "@slow") should
        include("noinline")
    }

    "'@cold' on a function" in {
      defineLine(ir("@cold\nrare(n: i32) -> i32 = n + 1\n\nprint(rare(1))\n"), "@rare") should
        include("cold")
    }

    // Both, because they are two axes rather than a stronger and a weaker form of one: a rare slow
    // path wants to be kept out of line *and* to be known to be rare, and either alone leaves half
    // of that unsaid.
    "and both together, in LLVM's own order" in {
      defineLine(ir("@noinline\n@cold\nrare(n: i32) -> i32 = n + 1\n\nprint(rare(1))\n"), "@rare") should
        include("noinline cold")
    }

    // Written the other way round, since the order above a declaration is the writer's and the order
    // on the line is LLVM's — a reader comparing two definitions should not be reading the order
    // somebody happened to type.
    "whichever order they were written in" in {
      defineLine(ir("@cold\n@noinline\nrare(n: i32) -> i32 = n + 1\n\nprint(rare(1))\n"), "@rare") should
        include("noinline cold")
    }

    // The case the feature exists for. A generic member's mark belongs to the **declaration**, so
    // every instantiation the program asks for carries it — a buffer's grow is written once and is
    // out of line at every element type.
    "on a generic method, at each instantiation" in {
      val out = ir(
        """struct Box[T]
          |    value: T
          |
          |    @noinline
          |    @cold
          |    grow(self) -> T = self.value
          |
          |var a = Box(1i32)
          |var b = Box(2i64)
          |print(a.grow(), b.grow())
          |""".stripMargin,
      )

      val lines = out.linesIterator.filter(l => l.startsWith("define") && l.contains("Box.grow")).toList

      lines.length shouldBe 2
      all(lines) should include("noinline cold")
    }

    "and on a generic free function" in {
      val out = ir(
        """@noinline
          |twice[T: sysl.Add](x: T) -> T = x + x
          |
          |print(twice(1i32), twice(2i64))
          |""".stripMargin,
      )

      val lines = out.linesIterator.filter(l => l.startsWith("define") && l.contains("@twice")).toList

      lines.length shouldBe 2
      all(lines) should include("noinline")
    }

    // Nothing is written where nothing was asked for, which is the assertion that keeps the two
    // above from passing on a `define` line that says `noinline` about everything.
    "and nothing at all where neither was written" in {
      defineLine(ir("slow(n: i32) -> i32 = n + 1\n\nprint(slow(1))\n"), "@slow") should
        not include "noinline"
    }
  }

  /** A hot exported function and the one private helper it calls — the shape a buffer's grow has.
   * `@export` is what keeps the caller itself from being deleted: nothing in this program calls it,
   * and the whole point is that something outside will.
   */
  private def program(marks: String): String =
    s"""module demo
       |
       |@export
       |hot(n: i32) -> i32 = n + rare(n)
       |
       |${marks}private rare(n: i32) -> i32 = n * 3 - 7
       |""".stripMargin

  /** The attribute group a `define` line refers to, resolved.
   *
   * **Optimized IR does not carry the keywords on the line.** clang gathers a definition's function
   * attributes into an `attributes #N = { … }` at the foot of the module and leaves a `#N` behind,
   * so an assertion reading the `define` line alone would report that `noinline` had gone the moment
   * the optimizer had looked at it. Resolving the reference is also the stronger question: it says
   * LLVM *parsed and kept* the attribute rather than that sysl printed the word.
   */
  private def attributesOn(out: String, name: String): String = {
    val line  = defineLine(out, name)
    val group = """#(\d+)""".r.findFirstMatchIn(line).map(_.group(1))
      .getOrElse(fail(s"the definition refers to no attribute group:\n$line"))

    out.linesIterator.find(_.startsWith(s"attributes #$group = "))
      .getOrElse(fail(s"nothing defines attribute group #$group:\n$out"))
  }

  /** Whether some IR still calls the function whose symbol holds `name`. */
  private def calls(out: String, name: String): Boolean =
    out.linesIterator.exists(l => l.contains("call ") && l.contains(name))

  /** The claim the feature is *for*, asked of clang rather than of the emitted text. */
  "a marked private callee survives -O2 with one call site" - {

    // The control, and it is what makes the cases below evidence rather than a tautology: the very
    // same program without the annotation loses the definition entirely, so a run in which both
    // passed would be a run in which clang inlined nothing at all.
    "the unmarked one is folded away, which is the control" in {
      val out = optimizedIr(program(""))

      defines(out, "rare") shouldBe false
      calls(out, "demo$rare") shouldBe false
    }

    "and the marked one is still a definition, and still a call" in {
      val out = optimizedIr(program("@noinline\n"))

      defines(out, "rare") shouldBe true
      calls(out, "demo$rare") shouldBe true
      attributesOn(out, "rare") should include("noinline")
    }

    // `@cold` alone is a frequency and not a prohibition — it lowers the inliner's threshold rather
    // than closing the door — so this pins the division the pair is designed around rather than
    // letting the one be read as a spelling of the other.
    "while '@cold' alone does not keep it out of line" in {
      defines(optimizedIr(program("@cold\n")), "rare") shouldBe false
    }

    "and the two together keep it out of line and say it is rare" in {
      val out = optimizedIr(program("@noinline\n@cold\n"))

      calls(out, "demo$rare") shouldBe true
      attributesOn(out, "rare") should include("noinline")
      attributesOn(out, "rare") should include("cold")
    }
  }

  /** The combinations that are ordinary, written down because the temptation is to refuse them. */
  "the marks stand beside what they have nothing to do with" - {

    // `@tailrec` is about this function's call to **itself** becoming a jump; `@noinline` is about
    // the call into it from elsewhere staying a call. Two different calls, so there is nothing for
    // them to disagree about.
    "'@tailrec', which is about a different call" in {
      defineLine(
        ir("""@tailrec
             |@noinline
             |count(n: i32, acc: i32) -> i32 =
             |    if n == 0i32
             |        return acc
             |    return count(n - 1i32, acc + 1i32)
             |
             |print(count(3i32, 0i32))
             |""".stripMargin),
        "@count",
      ) should include("noinline")
    }

    // A test is an ordinary function with a caller nothing else has, and keeping one out of line is
    // a perfectly sensible thing to ask for — clang and rustc refuse the analogue on neither. `ir`
    // fails the case on any diagnostic, so compiling at all is the assertion; the marked body is not
    // in this module because a test is emitted under `sysl test` and nowhere else.
    "'@test', which only says who calls it" in {
      ir("""@test("it holds")
           |@noinline
           |holds() =
           |    assert(1 == 1)
           |
           |print(1)
           |""".stripMargin) should include("define")
    }
  }
}
