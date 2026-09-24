package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What a struct's `invariant` tells the **optimizer**: the clause, laid down as an `llvm.assume`
 * on entry to every member whose receiver carries it (`ContractEmitter.assumeReceiverInvariant`).
 *
 * The shape it exists for is `Buf.at` — the index tested against `count` for the panic, and the
 * slice underneath testing it again against `elems.len`. Told `count <= elems.len`, the second test
 * is implied by the first. As in `ContractAssumeTests`, each claim is asked of a real optimizer and
 * stands beside a control that still carries what the clause removed.
 */
class StructInvariantAssumeTests extends AnyFreeSpec with CodegenSupport {

  /** The body of the function whose `define` line holds `name`. */
  private def bodyOf(out: String, name: String): String = {
    val lines = out.linesIterator.toList
    val start = lines.indexWhere(l => l.startsWith("define") && l.contains(name))

    if start < 0 then fail(s"nothing defines a function named '$name':\n$out")

    val end = lines.indexWhere(_ == "}", start)

    lines.slice(start, if end < 0 then lines.length else end + 1).mkString("\n")
  }

  private def compares(body: String): Int = "= icmp ".r.findAllIn(body).size

  /** A buffer's reading half, generic as `Buf` is, with the clause supplied by the case. */
  private def seq(clause: String) =
    s"""struct Seq[T]
       |    elems: []T
       |    count: usize
       |$clause
       |    @noinline
       |    at(self, i: usize) -> T
       |        if i >= self.count then exit(3)
       |
       |        self.elems[i]
       |
       |var s = Seq([1, 2, 3], 2usize)
       |print(s.at(1usize))
       |""".stripMargin

  "a receiver's clause reaches the member" - {

    "the element read is one compare" in {
      compares(bodyOf(optimizedIr(seq("    invariant count <= elems.len\n")), "Seq.at")) shouldBe 1
    }

    // Without the clause the slice's own test is still there beside the member's.
    "and is two without the clause" in {
      compares(bodyOf(optimizedIr(seq("")), "Seq.at")) shouldBe 2
    }

    "the clause is laid down as an assume at the member's entry" in {
      bodyOf(ir(seq("    invariant count <= elems.len\n")), "Seq.at") should include("@llvm.assume")
    }

    "and through a pointer receiver" in {
      val src =
        """struct Seq
          |    elems: []int
          |    count: usize
          |    invariant count <= elems.len
          |
          |    @noinline
          |    put(*self, i: usize, v: int)
          |        if i >= self.count then exit(3)
          |
          |        self.elems[i] = v
          |
          |var s = Seq([1, 2, 3], 2usize)
          |s.put(1usize, 7)
          |print(s.elems[1])
          |""".stripMargin

      bodyOf(ir(src), "Seq.put") should include("@llvm.assume")
      compares(bodyOf(optimizedIr(src), "Seq.put")) shouldBe 1
    }

    // The library's own: `Buf.at` is the member the whole feature was for.
    "Buf.at is one compare" in {
      val out = optimizedIr("var b: Buf[int] = buf()\nb.push(1)\nprint(b.at(0usize))\n", "2")
      val at  = out.linesIterator.find(l => l.startsWith("define") && l.contains("Buf.at")).map(_ => bodyOf(out, "Buf.at"))

      // It may have been absorbed into its caller, in which case there is no body to count in and
      // `main` is where the compares went.
      compares(at.getOrElse(mainOf(out))) should be <= 1
    }
  }

  "a clause that cannot be repeated is not assumed" - {

    // A division is not something an assume may introduce: a zero divisor is undefined behaviour
    // the source never wrote at the entry.
    "one that divides" in {
      bodyOf(ir(seq("    invariant count / 2usize <= elems.len\n")), "Seq.at") should not include "@llvm.assume"
    }

    // A module `var` may have moved since the write that checked the struct, so the clause is no
    // longer known to hold at the entry.
    "one that reads module storage" in {
      val src = "var cap: usize = 8usize\n\n" + seq("    invariant count <= cap\n")

      bodyOf(ir(src), "Seq.at") should not include "@llvm.assume"
    }

    "one that calls something" in {
      val src = "@noinline\nroom(n: usize) -> usize = n\n\n" + seq("    invariant count <= room(elems.len)\n")

      bodyOf(ir(src), "Seq.at") should not include "@llvm.assume"
    }
  }

  "a struct without clauses is told nothing" in {
    bodyOf(ir(seq("")), "Seq.at") should not include "@llvm.assume"
  }

  /* A view's element is on its far side, so writing one owes the clause over the view's length no
   * re-check — which is what keeps `Buf.set` and `push` the stores they were.
   */
  "a write to a view's element is not re-checked" - {
    def checks(write: String) =
      val src =
        s"""struct Seq
           |    elems: []int
           |    count: usize
           |    invariant count <= elems.len
           |
           |var s = Seq([1, 2, 3], 2usize)
           |$write
           |print(s.elems[0])
           |""".stripMargin

      "call i1 @[^(]*\\$inv".r.findAllIn(mainOf(ir(src))).size

    // The construction is the one check either way.
    "an element write adds none" in {
      checks("s.elems[0] = 5") shouldBe 1
    }

    "where a write of the count adds one" in {
      checks("s.count = 1usize") shouldBe 2
    }
  }
}
