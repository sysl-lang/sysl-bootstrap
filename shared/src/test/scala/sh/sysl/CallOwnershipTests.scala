package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Who holds a count for a by-value argument while the call runs (`CallOwnership`).
 *
 * The convention is one-sided: a **caller** guarantees the argument stays alive for the length of
 * the call, and a **callee** reads its parameters through that guarantee and takes nothing at entry.
 * So the interesting question at a call site is which of three things the argument is — a temporary
 * this frame already owns, a local whose address never got out, or a place the call itself may
 * overwrite — and only the third costs anything.
 *
 * **The negative cases are the ones that matter**, because a missing count is a use-after-free and
 * not a slow program. Each shape is asserted twice: in the emitted text, which says what was
 * decided, and by running a program whose destructor prints, which says what the decision cost. A
 * test that only read the IR would pass on a rule that frees the wrong object.
 */
class CallOwnershipTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** A payload whose destructor says so, which is how a run test sees a count reach zero, and a
   * one-reference wrapper so that a by-value parameter is a counted aggregate rather than a bare
   * pointer — the case the convention is written for.
   */
  private val node =
    """struct Node
      |    v: int
      |impl Drop for Node
      |    drop(self) = print("dropped", self.v)
      |struct Holder
      |    r: &Node
      |struct Pair
      |    h: Holder
      |""".stripMargin

  /** A callee that writes memory, which is the whole of what used to forbid the borrow. */
  private val sink =
    """var sink: Holder = Holder(Node(9))
      |take(h: Holder) -> int
      |    sink = Holder(Node(8))
      |    h.r.v
      |""".stripMargin

  /** One function's emitted text, from its `define` to the brace that closes it.
   *
   * A module-wide assertion cannot ask this question: `arc.copy.Holder` is what an ordinary
   * assignment emits too, so the answer would be about the whole program rather than about the
   * function under test.
   */
  private def body(out: String, name: String): String = {
    val head = raw"(?m)^define [^@]*@[^(]*\Q$name\E\(".r

    head.findFirstMatchIn(out) match {
      case None    => fail(s"no definition of '$name' in:\n$out")
      case Some(m) =>
        val end = out.indexOf("\n}\n", m.start)

        out.substring(m.start, if end < 0 then out.length else end)
    }
  }

  private def occurrences(s: String, sub: String): Int =
    s.sliding(sub.length).count(_ == sub)

  /** The retain a callee would emit for `h` at entry — spelled precisely, because a bare
   * `arc.copy.Holder` also matches what an assignment inside the body emits.
   */
  private val entryRetain = "@arc.copy.Holder(%struct.Holder %h.param)"

  "a local handed to a callee that writes memory costs nothing at either end" - {

    // `outer`'s own parameter is a slot no callee has a name for, so passing it on asks nothing of
    // this frame — and `take` writes module storage, which is exactly the body the old callee-side
    // rule refused the borrow to.
    val src = node + sink +
      """outer(h: Holder) -> int = take(h)
        |var n: &Node = Node(1)
        |print(outer(Holder(n)))
        |print("end")""".stripMargin

    "nothing at the call site" in {
      body(ir(src), "outer") should not include "arc."
    }

    "nothing in the callee's prologue" in {
      body(ir(src), "take") should not include entryRetain
    }

    // `dropped 9` is the `sink` the callee overwrote; `1` is what the borrowed parameter read; the
    // last two are module storage at exit, each let go of exactly once.
    "and the object it was given is the one it read, let go of exactly once" in {
      run(src) shouldBe "dropped 9\n1\nend\ndropped 1\ndropped 8\n"
    }
  }

  "a field is the one argument that costs a count, taken around the call" - {

    // The count is on the **loaded value**, not at the field, which is what makes it a snapshot: the
    // call is free to overwrite `p.h` and the release afterwards still reaches what was passed.
    val src = node + sink +
      """via(p: Pair) -> int = take(p.h)
        |var n: &Node = Node(1)
        |print(via(Pair(Holder(n))))
        |print("end")""".stripMargin

    "exactly one retain and one release, both in the caller" in {
      val out = body(ir(src), "via")

      occurrences(out, "@arc.copy.Holder(")    shouldBe 1
      occurrences(out, "@arc.dispose.Holder(") shouldBe 1
    }

    "and still nothing in the callee" in {
      body(ir(src), "take") should not include entryRetain
    }

    "and everything is let go of exactly once" in {
      run(src) shouldBe "dropped 9\n1\nend\ndropped 1\ndropped 8\n"
    }
  }

  "a temporary is already this frame's, so it is handed over with no count taken anywhere" - {

    val src = node + sink +
      """make() -> &Node = Node(2)
        |report()
        |    print(take(Holder(make())))
        |report()
        |print("end")""".stripMargin

    "nothing is retained at the call site" in {
      body(ir(src), "report") should not include "@arc.copy.Holder("
    }

    // The one release is the temporary's own, at the end of the statement that made it — the callee
    // emits none, so this is the only place the count goes back.
    "and the single release is the caller's, after the call" in {
      occurrences(body(ir(src), "report"), "@arc.release(") shouldBe 1
    }

    "and the temporary is destroyed exactly once, after the call returns" in {
      run(src) shouldBe "dropped 9\n2\ndropped 2\nend\ndropped 8\n"
    }
  }

  // The shape the old rule could not be widened to: the only count for what `v` refers to lives in
  // `s.v`, and the callee overwrites `s.v` before reading `v`. Under a rule that let the callee
  // borrow it unconditionally this reads freed storage; the caller's count is what makes it legal,
  // and the run is the half that would crash or read rubbish without it.
  "the shape a callee-side rule could never allow: the call frees the place it was passed" in {
    val src = node +
      """struct S
        |    v: Holder
        |    w: Holder
        |zap(v: Holder, s: *S) -> int
        |    s.v = Holder(Node(2))
        |    v.r.v
        |report()
        |    var s = S(Holder(Node(1)), Holder(Node(3)))
        |    print(zap(s.v, &s))
        |report()
        |print("end")""".stripMargin

    val out = run(src)

    out.linesIterator.next() shouldBe "1"
    out.linesIterator.count(_.startsWith("dropped")) shouldBe 3
    out should endWith("end\n")
  }

  // A store takes a count of its own, which is what makes a borrowed parameter safe to keep: the
  // callee holds nothing at entry and the assignment is where the new owner's count comes from.
  "a callee that stores its parameter keeps what it stored alive" in {
    val src = node +
      """var kept: Holder = Holder(Node(9))
        |keep(h: Holder) = kept = h
        |report()
        |    var n: &Node = Node(5)
        |    keep(Holder(n))
        |report()
        |print("still", kept.r.v)
        |print("end")""".stripMargin

    run(src) shouldBe "dropped 9\nstill 5\nend\ndropped 5\n"
  }

  "no path out of a callee leaks or double-frees what it borrowed" - {

    // An early return leaves before the body's own releases, and the parameter is not among them —
    // so the count that comes back is the caller's, at the end of the statement, on both paths.
    "an early return" in {
      val src = node +
        """early(h: Holder, k: int) -> int
          |    if k > 0 then return 7
          |    h.r.v
          |report()
          |    print(early(Holder(Node(1)), 1))
          |    print(early(Holder(Node(2)), 0))
          |report()
          |print("end")""".stripMargin

      run(src) shouldBe "7\ndropped 1\n2\ndropped 2\nend\n"
    }

    // A call that cannot come back never reaches a release at all, which is the case `sysl.buf`'s
    // bounds reporting is built out of. What it must not do is destroy the argument on the way.
    "a path that does not come back" in {
      val src = node +
        """gone(v: int) -> never
          |    print("past the end", v)
          |    exit(0)
          |edge(h: Holder, k: int) -> int
          |    if k > 0 then gone(h.r.v)
          |    h.r.v
          |var n: &Node = Node(4)
          |print(edge(Holder(n), 1))""".stripMargin

      run(src) shouldBe "past the end 4\n"
    }
  }

  // **The shape is asserted at the emitting seam above; what `-O2` is asked is whether the program
  // is still right.** An assertion on optimized *text* would be a claim about LLVM's inlining
  // budget rather than about the convention — a program this small flattens into arithmetic, every
  // helper absorbed, and both a sound rule and an unsound one read zero. What the level can still
  // say is whether the counts survive every pass that reorders and merges the loads and stores they
  // are made of, which is exactly where a missing retain stops being survivable.
  "the freed-place shape is still right once the optimizer has had its turn" in {
    val src = node +
      """struct S
        |    v: Holder
        |    w: Holder
        |zap(v: Holder, s: *S) -> int
        |    s.v = Holder(Node(2))
        |    v.r.v
        |report()
        |    var s = S(Holder(Node(1)), Holder(Node(3)))
        |    print(zap(s.v, &s))
        |report()
        |print("end")""".stripMargin

    val out = run(src, optimize = "2")

    out.linesIterator.next() shouldBe "1"
    out.linesIterator.count(_.startsWith("dropped")) shouldBe 3
  }
}
