package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Reading a by-value parameter through the caller's count instead of taking one.
 *
 * `BorrowedParams` drops the retain at entry and the release at each return where the body can
 * release nothing at all. The whole risk is in the word *nothing*, so the negatives here matter more
 * than the positives: each is a body that can reach a release, and each asserts the count is still
 * taken. A wrong answer to any of them is a use-after-free rather than a slow program, which is why
 * the shape is checked in the emitted text and the consequence is checked by running.
 */
class BorrowedParamTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** A payload whose destructor says so, which is how a run test sees a count reach zero. */
  private val node =
    """struct Node
      |    v: int
      |impl Drop for Node
      |    drop(self) = print("dropped", self.v)
      |struct Holder
      |    r: &Node
      |""".stripMargin

  /** One function's emitted text, from its `define` to the brace that closes it.
   *
   * A module-wide `should not include` cannot ask this question: `arc.copy.Holder` is what an
   * ordinary local assignment emits too, so the answer would be about `main` rather than about the
   * function under test.
   */
  private def body(out: String, name: String): String = {
    // A function reaching module-level storage is lowered with an environment and carries its name
    // as a suffix (`@$env0.clobber`), so the match is on the tail rather than on the whole symbol.
    val head = raw"(?m)^define [^@]*@[^(]*\Q$name\E\(".r

    head.findFirstMatchIn(out) match {
      case None    => fail(s"no definition of '$name' in:\n$out")
      case Some(m) =>
        val end = out.indexOf("\n}\n", m.start)

        out.substring(m.start, if end < 0 then out.length else end)
    }
  }

  "a reader that can release nothing takes no count of its own" in {
    val out = body(ir(node + "peek(h: Holder) -> int = h.r.v\nvar n: &Node = Node(1)\nprint(peek(Holder(n)))"),
                   "peek")

    out should not include "arc.copy"
    out should not include "arc.dispose"
    out should not include "arc.release"
  }

  // The borrow is only sound while the caller's own share outlives the call, so the observable
  // claim is that the object is still whole inside the callee and is let go exactly once after it.
  "and the object it borrowed is still released exactly once" in {
    val src = node +
      """peek(h: Holder) -> int = h.r.v
        |report()
        |    var n: &Node = Node(1)
        |    print("read", peek(Holder(n)))
        |    print("returning")
        |report()""".stripMargin

    run(src) shouldBe "read 1\nreturning\ndropped 1\n"
  }

  // A call that cannot come back never reaches the release, so the panic an accessor's bounds check
  // branches to does not cost it the borrow. This is the shape `sysl.buf` was rewritten into.
  "a call that does not return leaves the borrow in place" in {
    val src = node +
      """gone(v: int) -> never
        |    print("past the end", v)
        |    exit(1)
        |peek(h: Holder, i: int) -> int
        |    if i > 0 then gone(i)
        |    h.r.v
        |var n: &Node = Node(1)
        |print(peek(Holder(n), 0))""".stripMargin

    body(ir(src), "peek") should not include "arc.copy"
    run(src) shouldBe "1\ndropped 1\n"
  }

  "a parameter handed straight back still reaches the caller with a count" in {
    val src = node +
      """same(h: Holder) -> Holder = h
        |report()
        |    var n: &Node = Node(1)
        |    var k = same(Holder(n))
        |    print("held", k.r.v)
        |report()
        |print("gone")""".stripMargin

    body(ir(src), "same") should not include "arc.dispose"
    run(src) shouldBe "held 1\ndropped 1\ngone\n"
  }

  "a parameter holding several references is borrowed as one" in {
    val src =
      """struct Node
        |    v: int
        |impl Drop for Node
        |    drop(self) = print("dropped", self.v)
        |struct Pair
        |    a: &Node
        |    b: &Node
        |total(p: Pair) -> int = p.a.v + p.b.v
        |report()
        |    var x: &Node = Node(1)
        |    var y: &Node = Node(2)
        |    print(total(Pair(x, y)))
        |report()
        |print("gone")""".stripMargin

    // `arc.` alone would match the box type `%arc.Node` a field read goes through, which is not a
    // count at all — the two calls are what a count costs.
    body(ir(src), "total") should not include "arc.copy"
    body(ir(src), "total") should not include "arc.dispose"
    run(src) shouldBe "3\ndropped 2\ndropped 1\ngone\n"
  }

  "a generic reader is borrowed at the instantiation whose argument is a reference" in {
    val src = node +
      """first[T](xs: []T) -> T = xs[0]
        |var n: &Node = Node(1)
        |var xs: []&Node = [n]
        |print(first(xs).v)""".stripMargin

    val out = ir(src)

    // The instantiation for a reference element is the one that would have retained; the returned
    // element still does, because the caller owns what comes back.
    body(out, "first.ref.Node") should not include "arc.dispose"
  }

  "the buffer's own accessor reads its storage without touching a count" in {
    val src =
      """import sysl.buf.buf
        |var b = buf[int]()
        |b.push(3)
        |print(b.at(0))""".stripMargin

    val out = ir(src)

    body(out, "sysl.buf$Buf.at.int") should not include "arc."
    // The report is a call that does not return rather than a dozen inline ones, which is what
    // leaves the accessor small enough for a caller to absorb.
    out should include("define internal void @sysl.buf$past_end(")
  }

  "a reader that writes to module storage keeps its count" - {

    // The hazard the retain exists for: the only share of what the parameter refers to is the one
    // the assignment below gives back, so a borrowed `h` would be reading freed storage on the very
    // next line.
    val src = node +
      """var g: Holder = Holder(Node(1))
        |clobber(h: Holder) -> int
        |    g = Holder(Node(2))
        |    h.r.v
        |print(clobber(g))""".stripMargin

    "so the count is taken at entry" in {
      body(ir(src), "clobber") should include("arc.copy.Holder")
    }

    "and what it read is the object it was given" in {
      run(src) should include("1\n")
    }
  }

  "a reader that calls anything at all keeps its count" in {
    val src = node +
      """side() = print("side")
        |peek(h: Holder) -> int
        |    side()
        |    h.r.v
        |var n: &Node = Node(1)
        |print(peek(Holder(n)))""".stripMargin

    body(ir(src), "peek") should include("arc.copy.Holder")
  }

  "a reader that reassigns its own parameter keeps its count" in {
    val src = node +
      """peek(h: Holder) -> int
        |    h = Holder(Node(2))
        |    h.r.v
        |var n: &Node = Node(1)
        |print(peek(Holder(n)))""".stripMargin

    body(ir(src), "peek") should include("arc.copy.Holder")
  }

  "a reader that writes through a pointer keeps its count" in {
    val src = node +
      """swap(h: Holder, out: *Holder) -> int
        |    *out = Holder(Node(2))
        |    h.r.v
        |var g: Holder = Holder(Node(1))
        |print(swap(g, &g))""".stripMargin

    body(ir(src), "swap") should include("arc.copy.Holder")
  }

  "a reader that captures its parameter in a closure keeps its count" in {
    val src = node +
      """peek(h: Holder) -> int
        |    var f = () -> h.r.v
        |    f()
        |var n: &Node = Node(1)
        |print(peek(Holder(n)))""".stripMargin

    body(ir(src), "peek") should include("arc.copy.Holder")
  }

  "a reader with a contract keeps its count" in {
    val src = node +
      """peek(h: Holder) -> int
        |    require h.r.v > 0
        |    h.r.v
        |var n: &Node = Node(1)
        |print(peek(Holder(n)))""".stripMargin

    body(ir(src), "peek") should include("arc.copy.Holder")
  }
}
