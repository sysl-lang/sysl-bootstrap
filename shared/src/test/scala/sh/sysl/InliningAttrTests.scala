package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@noinline`, `@inline` and `@cold` — what a definition tells the optimizer about itself
 * (`reference/attributes.md § @noinline, @inline and @cold`).
 *
 * **The point of the group is a library deciding which of its members a caller absorbs, and it is
 * not something the emitted text can show.** A `private` function lowers to `internal`, so with a
 * single call site the inliner folds it back into its caller and the caller is then too large to
 * inline into *its* callers — which is the opposite of what splitting the rare path out was for. So
 * the seam this suite asserts at is the optimizer's output rather than the compiler's: that the
 * definition and the call are still there after `-O2`, against a control that shows the unmarked one
 * is folded away.
 *
 * `noinline` and `inline` are one axis and contradict; `cold` is another and composes with either.
 * `noinline` forbids inlining outright, `inline` raises what the inliner will spend on the callee,
 * and `cold` states a frequency and leaves the call eligible whichever of the two stands beside it.
 * `InliningAttrErrorTests` is the other half — every way of writing one that means nothing.
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

    // LLVM's name for the hint is not the word the language uses, which is the one place in the
    // group where the two spellings differ: `@inline` is a request and `inlinehint` is what LLVM
    // calls a request, while `alwaysinline` — which LLVM also has — is an instruction the language
    // does not offer.
    "'@inline' on a function, which LLVM spells 'inlinehint'" in {
      val line = defineLine(ir("@inline\nsmall(n: i32) -> i32 = n + 1\n\nprint(small(1))\n"), "@small")

      line should include("inlinehint")
      line should not include "alwaysinline"
    }

    // The composition the pair below is the whole reason `@inline` is a separate axis from `@cold`:
    // a member can be worth absorbing and still be reached rarely, and LLVM accepts the two
    // keywords side by side.
    "'@inline' and '@cold' together, which LLVM accepts" in {
      defineLine(ir("@inline\n@cold\nrare(n: i32) -> i32 = n + 1\n\nprint(rare(1))\n"), "@rare") should
        include("inlinehint cold")
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

    "and '@inline' on a generic method, at each instantiation" in {
      val out = ir(
        """struct Box[T]
          |    value: T
          |
          |    @inline
          |    read(self) -> T = self.value
          |
          |var a = Box(1i32)
          |var b = Box(2i64)
          |print(a.read(), b.read())
          |""".stripMargin,
      )

      val lines = out.linesIterator.filter(l => l.startsWith("define") && l.contains("Box.read")).toList

      lines.length shouldBe 2
      all(lines) should include("inlinehint")
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
    "and nothing at all where none was written" in {
      val line = defineLine(ir("slow(n: i32) -> i32 = n + 1\n\nprint(slow(1))\n"), "@slow")

      line should not include "noinline"
      line should not include "inlinehint"
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

    // `@inline` stands beside the same two for the same reasons: a self-call becoming a jump is a
    // different call from the one a hint is about, and who calls a test says nothing about how.
    "and '@inline' beside either of them" in {
      defineLine(
        ir("""@tailrec
             |@inline
             |count(n: i32, acc: i32) -> i32 =
             |    if n == 0i32
             |        return acc
             |    return count(n - 1i32, acc + 1i32)
             |
             |print(count(3i32, 0i32))
             |""".stripMargin),
        "@count",
      ) should include("inlinehint")

      ir("""@test("it holds")
           |@inline
           |holds() =
           |    assert(1 == 1)
           |
           |print(1)
           |""".stripMargin) should include("define")
    }
  }

  /** A hot loop over a `Buf[int]`, reading the elements back so that nothing in it is dead. */
  private val plainBuffer =
    """module demo
      |
      |import sysl.buf.*
      |
      |@export
      |total(n: int) -> int
      |    var b: Buf[int] = buf()
      |    var i = 0
      |
      |    while i < n
      |        b.push(i * 3)
      |        i += 1
      |
      |    var sum = 0
      |
      |    for x in b.view() do sum += x
      |
      |    sum
      |""".stripMargin

  /** A `Buf` of a struct carrying a reference, which is the case that pays for a release: the slot
   * being written to still holds whatever seeded it, so a push lets that occupant go.
   */
  private val countedBuffer =
    """module demo
      |
      |import sysl.buf.*
      |
      |struct Node
      |    x: int
      |
      |struct Cell
      |    tag: i64
      |    node: &Node
      |
      |@export
      |gather(n: i64) -> usize
      |    var node: &Node = Node(7)
      |    var b: Buf[Cell] = buf()
      |    var i = 0i64
      |
      |    while i < n
      |        b.push(Cell(i, node))
      |        i += 1
      |
      |    b.len()
      |""".stripMargin

  /** A `Buf` of a **wider** element — nine words, one of them a reference — which is the shape that
   * decided whether `push` carried `@inline` at all.
   *
   * **The member's cost grows with the element type and the budget a caller may spend on it does
   * not**, so somewhere along a struct's field list an append stops being a store and becomes a call
   * and a frame. This element is one field past that line at the default budget and inside it at the
   * raised one, which is the whole of what the mark on `push` buys. It was found by running the same
   * sweep of element shapes with the mark and without it rather than reasoned to: the two agreed
   * everywhere else, and the reference is what makes the line fall this early — an element of the
   * same width carrying none is absorbed either way.
   */
  private val wideBuffer =
    """module demo
      |
      |import sysl.buf.*
      |
      |struct Node
      |    x: int
      |
      |struct Reading
      |    at: i64
      |    seq: i64
      |    lo: i64
      |    hi: i64
      |    sum: i64
      |    count: i64
      |    flags: i64
      |    mask: i64
      |    source: &Node
      |
      |@export
      |collect(n: i64) -> usize
      |    var source: &Node = Node(7)
      |    var b: Buf[Reading] = buf()
      |    var i = 0i64
      |
      |    while i < n
      |        b.push(Reading(i, i+1i64, i+2i64, i+3i64, i+4i64, i+5i64, i+6i64, i+7i64, source))
      |        i += 1
      |
      |    b.len()
      |""".stripMargin

  /** The lines of one definition, for a claim about what is *inside* a function rather than about
   * what the module holds somewhere. A `}` alone on a line ends a definition in LLVM's text.
   */
  private def functionBody(out: String, name: String): String = {
    val lines = out.linesIterator.toList
    val start = lines.indexWhere(l => l.startsWith("define") && l.contains(name))

    if start < 0 then fail(s"nothing defines a function named '$name':\n$out")

    lines.drop(start).takeWhile(_ != "}").mkString("\n")
  }

  /** The library member the pair was added for: `sysl.buf`'s `push`.
   *
   * **A sequence's append is a store and a rare reallocation, and whether a caller has to hold the
   * second decides what the first costs.** Written as one member, `push` carries an allocation, a
   * copy and the release of the storage it replaces — enough that no caller can absorb it, so every
   * append is a call, a frame, and the callee-saved registers spilled around it. Split, what is left
   * is a compare, a store and a bump, and the caller takes all three.
   *
   * **What is behind the out-of-line name is the growth *and the store that follows it*,** which is
   * what the second case below is about. An element written only after the growth returns is an
   * element the caller has to keep across that call, so the frame it saves for one is a frame every
   * append pays for; written on the rare side as well, nothing outlives the call and the element
   * type stops deciding whether a caller can hold a push at all.
   *
   * The assertions are on clang's output rather than on the emitted text, because every half of the
   * claim is the optimizer's answer: that `push` is gone from its caller, and that the growth is not.
   */
  "a buffer's push is absorbed by its caller and its growth is not" - {

    "for a plain element, the caller holds the push and calls the growth" in {
      val out  = optimizedIr(plainBuffer)
      val body = functionBody(out, "demo$total")

      // Asked of the call instructions rather than of the whole text: clang names the block an
      // inlined callee returned to after that callee, so the caller's *labels* mention `push`
      // precisely because it is no longer called.
      calls(body, "Buf.push.int") shouldBe false

      // The other half, and what keeps the first from passing vacuously: the growth `push` guards
      // is in the caller as a call, which is only true if `push` itself was absorbed.
      calls(body, "Buf.grow_store.int") shouldBe true

      defines(out, "Buf.grow_store.int") shouldBe true
      attributesOn(out, "Buf.grow_store.int") should include("noinline")

      // The allocation and the copy are a further call in behind that one, rather than a copy of
      // themselves at each element type's rare arm.
      defines(out, "Buf.grow.int") shouldBe true
    }

    /** The case the element type used to decide, and the one a program that pushes values around
     * actually runs: a struct carrying a reference.
     *
     * A push of one is a store, a share taken of what is stored, and a share given back by whatever
     * the slot held before — and with the element also having to survive the growth call, that was
     * over the cost of anything a caller would absorb, so every append of a counted value was a call
     * and a frame while an append of an `int` was a store.
     */
    "and for an element carrying a count, which is the case that pays for a release" in {
      val out  = optimizedIr(countedBuffer)
      val body = functionBody(out, "demo$gather")

      calls(body, "Buf.push.demo$Cell") shouldBe false
      calls(body, "Buf.grow_store.demo$Cell") shouldBe true

      // And the store the caller absorbed carries no check of its own. Room is asked for with `>=`,
      // so the arm that does not grow is reached knowing the count is below the length — there is
      // nothing left for a bounds check to decide, and a trap in here would say the fact was lost.
      body should not include "@llvm.trap"
    }

    /** The case `@inline` was added for, and the one the mark on `push` is paying for.
     *
     * **A member written to be absorbed sits a few units either side of the default budget, and
     * which side is decided by the element type rather than by the member.** The `Cell` above is
     * inside it; a nine-word element carrying one reference is outside, so the same `push` — the
     * same source, the same shape, the same intent — stopped being a store and became a call and a
     * frame because a struct grew fields. The mark raises what the inliner will spend on `push`,
     * and this element is the evidence that the raise reaches a real shape rather than only the
     * `define` line: it is a call without the mark and a store with it.
     *
     * **This is the assertion that fails on a tree where `push` is unmarked**, which is what makes
     * it a test of the feature rather than of clang.
     */
    "and for a wider element, which the default budget refuses" in {
      val out  = optimizedIr(wideBuffer)
      val body = functionBody(out, "demo$collect")

      calls(body, "Buf.push.demo$Reading") shouldBe false

      // The other half, and what keeps the first from passing vacuously: the growth is still the
      // call it is marked to be, so what was absorbed is the append and not the whole member.
      calls(body, "Buf.grow_store.demo$Reading") shouldBe true

      // And the marks reached this instantiation rather than only the declaration, which is what a
      // mark on a generic member has to do to be worth anything.
      attributesOn(out, "Buf.grow_store.demo$Reading") should include("noinline")
    }
  }

  /** What a release costs at the site it is emitted, which is what decides whether the member
   * holding one can be inlined at all.
   *
   * **Releasing is a decrement and a test; only the test succeeding reaches the worklist.** With the
   * drain loop inlinable, every release site in every program carries a copy of it — the queue push,
   * the re-entry flag, the `step` loop and the indirect call to the object's hook — which is how a
   * `push` storing a counted element came to be a hundred and seventy lines of IR. Marked `noinline
   * cold`, the drain is a call that is almost never made and the release is a decrement and a branch.
   */
  "the reaper's drain is not copied into the code that releases" - {

    // Asked of the caller, because that is where a push over a counted slot ends up: the release it
    // carries is what the drain would otherwise be copied into, and a caller holding a copy of the
    // drain is a caller that could not have held the push.
    "so a push over a counted slot reaches it by a call" in {
      val body = functionBody(optimizedIr(countedBuffer), "demo$gather")

      body should include("void @arc.reap(")
      body should not include "arc.reaper"
      body should not include "drain"
    }

    "and the drain is still a definition, kept out of line and declared rare" in {
      val out = optimizedIr(countedBuffer)

      defines(out, "@arc.reap") shouldBe true
      attributesOn(out, "@arc.reap") should include("noinline")
      attributesOn(out, "@arc.reap") should include("cold")
    }

    // The growth is out of line here too, at an element type that carries a reference — the mark is
    // on the declaration, so it belongs to every instantiation rather than to the one that was
    // easiest to measure.
    "while the growth for a counted element is out of line as well" in {
      val out = optimizedIr(countedBuffer)

      defines(out, "Buf.grow.demo$Cell") shouldBe true
      calls(out, "Buf.grow.demo$Cell") shouldBe true
    }
  }
}
