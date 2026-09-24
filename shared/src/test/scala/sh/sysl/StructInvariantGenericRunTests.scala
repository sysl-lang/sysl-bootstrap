package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A **generic** struct's `invariant` is checked exactly where a non-generic one's is — at the
 * construction, at every field write, at a whole-struct assignment — with its clauses made real at
 * each instantiation's own type arguments (`reference/errors.md § Generic structs`).
 *
 * And a **zero** is a construction the program did not spell: a declaration with no initializer is
 * checked against every clause its zero holds, since a struct that began its life broken is a value
 * no write ever checked — and a clause is assumed at every member's entry on the strength of every
 * value having been checked.
 */
class StructInvariantGenericRunTests extends AnyFreeSpec with RunSupport {

  private val Range =
    """|struct Range[T: Ord]
       |    lo: T
       |    hi: T
       |    invariant lo <= hi
       |""".stripMargin

  "a generic struct's clause is checked at its construction" - {
    "one that holds proceeds" in {
      run(Range + "var r = Range(1, 5)\nprint(r.lo, r.hi)") shouldBe "1 5\n"
    }

    "one that does not traps" in {
      exits(Range + "var r = Range(5, 1)\nprint(r.lo)")
    }

    // Two instantiations are two functions, and each is the clause at its own type: a float range
    // is compared as floats, not through whatever the first instantiation happened to be.
    "at each instantiation's own type" in {
      run(Range + "var a = Range(1, 5)\nvar b = Range(0.5, 2.5)\nprint(a.hi, b.hi)") shouldBe "5 2.5\n"
      exits(Range + "var a = Range(1, 5)\nvar b = Range(2.5, 0.5)\nprint(a.hi, b.hi)")
    }
  }

  "a generic struct's clause is checked at every field write" - {
    "one that holds proceeds" in {
      run(Range + "var r = Range(1, 5)\nr.hi = 9\nr.lo += 2\nprint(r.lo, r.hi)") shouldBe "3 9\n"
    }

    "one that breaks it traps at that write" in {
      exits(Range + "var r = Range(1, 5)\nr.hi = 0\nprint(r.hi)")
    }

    "and through a pointer" in {
      exits(Range + "var r = Range(1, 5)\nvar p = &r\np.lo = 7\nprint(r.lo)")
    }

    "and from inside a member" in {
      val stretch =
        """|struct Span[T: Ord]
           |    lo: T
           |    hi: T
           |    invariant lo <= hi
           |
           |    set_hi(*self, v: T)
           |        self.hi = v
           |""".stripMargin

      run(stretch + "var s = Span(1, 5)\ns.set_hi(8)\nprint(s.hi)") shouldBe "8\n"
      exits(stretch + "var s = Span(1, 5)\ns.set_hi(0)\nprint(s.hi)")
    }
  }

  /* A view's elements are on its far side, which no clause may read, so writing one is owed no
   * re-check — and the program that relies on that is the ordinary one: a clause over the view's
   * length, and elements written freely underneath it.
   */
  "a write to a view's element owes the clause over its length nothing" in {
    val seq =
      """|struct Seq
         |    elems: []int
         |    count: usize
         |    invariant count <= elems.len
         |""".stripMargin

    run(seq + "var s = Seq([1, 2, 3], 2)\ns.elems[2] = 9\nvar p = &s.elems[0]\n*p = 4\n" +
      "var v = s.elems[..<s.count]\nv[1] = 5\nprint(s.elems[0], s.elems[1], s.elems[2])") shouldBe "4 5 9\n"
  }

  "a zero is checked as the construction it is" - {
    val Window =
      """|struct Window
         |    lo: int
         |    hi: int
         |    invariant lo < hi
         |""".stripMargin

    val Count =
      """|struct Count
         |    n: int
         |    invariant n >= 0
         |""".stripMargin

    "a zero that breaks the clause traps where it is declared" in {
      exits(Window + "var w: Window\nprint(w.lo)")
    }

    "one that satisfies it is the ordinary declaration" in {
      run(Count + "var c: Count\nc.n += 2\nprint(c.n)") shouldBe "2\n"
    }

    "inside an array" in {
      exits(Window + "var ws: [2]Window\nprint(ws[0].lo)")
    }

    "inside another struct's field" in {
      exits(Window + "struct Frame\n    w: Window\n    k: int\nvar f: Frame\nprint(f.k)")
    }

    "as module storage" in {
      exits(Window + "var w: Window\n\nmain()\n    print(w.hi)")
    }

    "and a generic struct's zero at its instantiation" in {
      exits("struct Gap[T: Ord]\n    lo: T\n    hi: T\n    invariant lo < hi\n\nvar g: Gap[int]\nprint(g.lo)")
      run("struct Gap[T: Ord]\n    lo: T\n    hi: T\n    invariant lo <= hi\n\nvar g: Gap[int]\nprint(g.lo)") shouldBe
        "0\n"
    }
  }

  "a Buf behaves as it did before it carried a clause" - {
    val prelude = "import sysl.buf.{Buf, buf}\n\n"

    "a zero Buf is the empty one, and grows" in {
      run(prelude + "var b: Buf[int]\nb.push(4)\nb.push(5)\nprint(b.len(), b.at(1))") shouldBe "2 5\n"
    }

    "reading past the count still panics with the length, though storage is there" in {
      panics(prelude + "var b: Buf[int] = buf()\nb.extend([1, 2, 3])\nb.truncate(1)\nprint(b.at(1))",
        "panic: index 1 past the 1 elements of a Buf")
    }

    "the count may be moved within the storage" in {
      run(prelude + "var b: Buf[int] = buf()\nb.extend([1, 2, 3])\nb.truncate(1)\nb.count = 3\nprint(b.at(2))") shouldBe
        "3\n"
    }

    "but not past it" in {
      exits(prelude + "var b: Buf[int] = buf()\nb.push(1)\nb.count = b.elems.len + 1\nprint(b.len())")
    }
  }
}
