package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Compile-time diagnostics for a misused `invariant`: a clause that is not a `bool`, one that
 * names something not in scope, a generic struct's clause its bounds do not license, and the
 * aliases a clause forbids — which is the larger half of the file, because a clause is checked at
 * the write by walking the *place*, and every one of these is a way of reaching the storage with no
 * place that names the struct.
 */
class StructInvariantErrorTests extends AnyFreeSpec with CodegenSupport {

  /** A clause that reads **through** a field, which is what makes `o.a.n` a write only `Outer` knows
   * is checked. `c` is read by nothing, and is here so that the refusals below can be shown to be
   * about the clause rather than about the struct carrying one.
   */
  private val Outer =
    """|struct Inner
       |    n: int
       |struct Outer
       |    a: Inner
       |    b: int
       |    c: int
       |    invariant a.n <= b
       |""".stripMargin

  "a non-bool invariant is rejected without leaking the synthesised function's name" - {
    "a bare integer field is not a condition" in {
      val e = err("struct Account\n    balance: int\n    invariant balance\nvar a = Account(1)")
      e should include("an 'invariant' must be a 'bool'")
      e should not include "$inv"
    }
    "an arithmetic expression is not a condition" in {
      err("struct Account\n    balance: int\n    invariant balance + 1\nvar a = Account(1)") should
        include("an 'invariant' must be a 'bool'")
    }
  }

  "an invariant that names an unknown field is rejected" in {
    err("struct Account\n    balance: int\n    invariant blance >= 0\nvar a = Account(1)") should include("blance")
  }

  /* A generic struct's clauses are a generic body over the struct's parameters, checked once against
   * the bounds it declares — so the mistakes are reported at the declaration, as any generic body's
   * are, and not once per instantiation in terms of whatever type it was made at.
   */
  "a generic struct's clause is held to the struct's own bounds" - {
    "a non-bool clause is refused as a non-generic one is" in {
      val e = err("struct Box[T]\n    v: T\n    n: int\n    invariant n + 1\nvar b = Box(1, 2)")

      e should include("an 'invariant' must be a 'bool'")
      e should not include "$inv"
    }

    "a comparison the parameter is not bounded for is refused" in {
      err("struct Span[T]\n    lo: T\n    hi: T\n    invariant lo <= hi\nvar s = Span(1, 2)") should include("Ord")
    }

    "and the bounded one compiles" in {
      ir("struct Span[T: Ord]\n    lo: T\n    hi: T\n    invariant lo <= hi\nvar s = Span(1, 2)\nprint(s.lo)") should
        include("$inv")
    }
  }

  /* The whole point of refusing at the `&` is that the pointer's *type* is what is wrong with it: a
   * `*Inner` is a licence to write an `Inner`, and the `Inner` it is aimed at is one an `Outer` has
   * a claim about. Nothing later in the program can put that right, because nothing later can tell
   * that this `Inner` is the one inside an `Outer`, which is exactly why the refusal is here.
   */
  "an alias typed below an invariant is refused where it is made" - {
    "a pointer to a field the clause reads through" in {
      val e = err(Outer + "wreck(p: *Inner)\n    p.n = 9\nvar o = Outer(Inner(1), 5, 0)\nwreck(&o.a)")

      e should include("whose invariant reads 'a.n'")
      e should include("a '*Inner' names no Outer")
    }

    // A field the clause reads *directly* is the same mistake with a shorter path, and it is worth
    // its own case because the cruder rule this one replaced would have refused it for the wrong
    // reason — for being a field of a struct with clauses, rather than for being read by one.
    "a pointer to a field the clause reads directly" in {
      err(Outer + "var o = Outer(Inner(1), 5, 0)\nvar p = &o.b") should include("whose invariant reads 'b'")
    }

    // Deeper than the clause reads, which still changes what it reads: `a.n` is read whole, so a
    // pointer at the `int` inside it is a pointer at the clause's own operand.
    "a pointer to storage below what the clause reads" in {
      err(Outer + "var o = Outer(Inner(1), 5, 0)\nvar p = &o.a.n") should include("whose invariant reads 'a.n'")
    }

    // Inside the struct's own method the receiver is a `*Outer`, so the place is `(*self).a` and the
    // walk finds `Outer` exactly as it does at the top level. A clause is not a rule about who may
    // break it.
    "and a method of the struct itself is not exempt" in {
      val handing =
        """|struct Inner
           |    n: int
           |struct Outer
           |    a: Inner
           |    b: int
           |    invariant a.n <= b
           |
           |    hand(*self) -> *Inner
           |        &self.a
           |""".stripMargin

      err(handing + "var o = Outer(Inner(1), 5)\nprint(o.b)") should include("whose invariant reads 'a.n'")
    }

    // The place is what is walked, not the expression that reached it, so a struct arrived at
    // through a pointer is the same struct: `(*p).a` names an `Outer` exactly as `o.a` does.
    "a pointer taken through a pointer to the enclosing struct" in {
      err(Outer + "var o = Outer(Inner(1), 5, 0)\nvar p = &o\nvar q = &(*p).a") should
        include("whose invariant reads 'a.n'")
    }

    /** The descent through a clause is **total** — it reads each node's children off the case class
      * rather than matching arm by arm, which is why `Expr` is declared a `Product` (`ast.scala`).
      * These are the two shapes that descent takes: a child held directly, and children held in a
      * list. A walk that stopped at either would report *no* reads for the node, the field would drop
      * out of what the clause is taken to read, and the `&` below would be **allowed** — the one
      * direction this file exists to rule out.
      */
    "a field the clause reaches only through nested forms is still read" in {
      def wrapping(clause: String) =
        s"""|struct Outer
            |    b: int
            |    c: int
            |    invariant $clause
            |""".stripMargin

      // `Unary` inside `Binary` — a child held directly, two levels down.
      err(wrapping("-b < 0") + "var o = Outer(5, 0)\nvar p = &o.b") should
        include("whose invariant reads 'b'")
      // `Compare` — children held in a list, which is the other arm of the same walk.
      err(wrapping("0 < b < 100") + "var o = Outer(5, 0)\nvar p = &o.b") should
        include("whose invariant reads 'b'")

      // The control, and the reason these say anything: a field no clause reaches is still ordinary,
      // so the refusals above are about the descent finding `b` rather than about the struct.
      irOf("main.sysl" -> (wrapping("0 < b < 100") + "var o = Outer(5, 0)\nvar p = &o.c\nprint(*p)")) should
        include("define")
    }

    // A clause is read for the paths it names, and a path hidden inside a statement — an `if` used
    // as a value has statement bodies — is one this reading cannot see. So such a clause is taken to
    // read every field, and `c` stops being ordinary. Refusing too much is the safe direction.
    "every field of a struct whose clause hides its reads inside a branch" in {
      val branching =
        """|struct Inner
           |    n: int
           |struct Outer
           |    a: Inner
           |    b: int
           |    c: int
           |    invariant if b > 0 then a.n <= b else true
           |""".stripMargin

      err(branching + "var o = Outer(Inner(1), 5, 0)\nvar p = &o.c") should include("whose invariant reads 'c'")
    }

    // A view that may be written is a licence to write elements, which is the same licence a `*T`
    // is, so it is refused in the same place and for the same reason.
    "a writable view of an array the clause reads" in {
      val grid =
        """|struct Grid
           |    items: [2]int
           |    cap: int
           |    invariant items[0] <= cap
           |""".stripMargin

      err(grid + "var g = Grid([0, 0], 5)\nvar v = g.items[0..<2]") should include("this view may be written")
    }
  }

  /* The route the refusal above cannot see: a method of a struct that carries no clause of its own
   * hands out a pointer into its receiver, and the caller's receiver turns out to have been a field
   * of a struct that does. No `&` in the caller's source spells the severed alias, so the rule has
   * to be stated where the pointer is made instead — as a limit on what a `*self` method may leave
   * behind it.
   */
  "a '*self' method may not let a pointer into its receiver outlive the call" - {
    "by returning it" in {
      val leaking =
        """|struct Inner
           |    n: int
           |
           |    leak(*self) -> *int
           |        &self.n
           |struct Outer
           |    a: Inner
           |    b: int
           |    invariant a.n <= b
           |""".stripMargin

      err(leaking + "var o = Outer(Inner(1), 5)\nvar p = o.a.leak()") should include("is returned")
    }

    "by storing it where the caller can still reach it" in {
      val stashing =
        """|struct Slot
           |    p: *int
           |struct Inner
           |    n: int
           |
           |    hide(*self, s: *Slot)
           |        s.p = &self.n
           |struct Outer
           |    a: Inner
           |    b: int
           |    invariant a.n <= b
           |""".stripMargin

      err(stashing + "var s = Slot(null)\nvar o = Outer(Inner(1), 5)\no.a.hide(&s)") should
        include("is stored somewhere the call does not own")
    }

    // Through a local first, which is why the locals holding one are worked out to a fixpoint rather
    // than read off the returned expression.
    "by passing it through a local on the way out" in {
      val relayed =
        """|struct Inner
           |    n: int
           |
           |    leak(*self) -> *int
           |        var q = &self.n
           |        var r = q
           |        r
           |struct Outer
           |    a: Inner
           |    b: int
           |    invariant a.n <= b
           |""".stripMargin

      err(relayed + "var o = Outer(Inner(1), 5)\nvar p = o.a.leak()") should include("is returned")
    }

    // And inside something built around it, since what leaves the call is the pointer either way.
    "by wrapping it in a struct on the way out" in {
      val wrapped =
        """|struct Wrap
           |    p: *int
           |struct Inner
           |    n: int
           |
           |    held(*self) -> Wrap
           |        Wrap(&self.n)
           |struct Outer
           |    a: Inner
           |    b: int
           |    invariant a.n <= b
           |""".stripMargin

      err(wrapped + "var o = Outer(Inner(1), 5)\nvar w = o.a.held()") should include("is returned")
    }

    // An array between the two is still one object, so the element type is as much "inside" the
    // struct with the clause as a plain field would be.
    "and a struct reached through an array field is covered too" in {
      val celled =
        """|struct Cell
           |    n: int
           |
           |    at(*self) -> *int
           |        &self.n
           |struct Grid
           |    items: [2]Cell
           |    cap: int
           |    invariant items[0].n <= cap
           |""".stripMargin

      err(celled + "var g = Grid([Cell(0), Cell(0)], 5)\nvar p = g.items[1].at()") should include("is returned")
    }
  }

  /* A clause is a claim about the struct's own bytes, and everything else here depends on that being
   * true. Where a clause reads through an indirection the claim is about storage with an identity of
   * its own — every other alias of it is outside this struct's sight — so no rule about `&` could
   * make the clause hold, and the clause itself is what is refused.
   */
  "an invariant may only read storage the struct owns" - {
    "not through a raw pointer" in {
      err("struct Inner\n    n: int\nstruct Outer\n    a: *Inner\n    b: int\n    invariant a.n <= b\nprint(1)") should
        include("'n' is read through *Inner")
    }

    "not through a reference" in {
      err("struct Inner\n    n: int\nstruct Outer\n    a: &Inner\n    b: int\n    invariant a.n <= b\nprint(1)") should
        include("'n' is read through &Inner")
    }

    // A view's elements are somebody else's storage even though the three words that name them are
    // the struct's own — which is the distinction the next test is the other half of.
    "not the elements of a view" in {
      err("struct Buf\n    xs: []int\n    cap: int\n    invariant xs[0] <= cap\nprint(1)") should
        include("an element is read through []int")
    }
  }
}
