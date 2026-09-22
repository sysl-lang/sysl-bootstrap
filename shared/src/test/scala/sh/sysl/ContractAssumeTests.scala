package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What a contract tells the **optimizer**, which is a different question from what it checks
 * (`ContractEmitter.assumeEnsures`).
 *
 * **A postcondition used to stop at the frame that declared it.** `grow` promises room and traps if
 * it did not deliver it, and the store on the line after the call still carried a bounds test —
 * because LLVM saw a call to a function it was told not to inline, then a subscript, and had nothing
 * joining the two. The clause is now said again at the call site as an `llvm.assume`, which emits no
 * code and lets the test fold.
 *
 * **Every case here is asked of a real optimizer rather than of the emitted text**, because the
 * claim is about what survives `-O2`: the emitted module has the check either way and only clang can
 * say whether the assume removed it. The negative control beside each positive is what makes the
 * answer mean anything — the same program without the clause, still carrying its trap.
 */
class ContractAssumeTests extends AnyFreeSpec with CodegenSupport {

  /** The body of the function whose symbol holds `name`, from its `define` line to the brace that
   * closes it — what a claim about *where* a trap went has to be asked of, since a module always has
   * traps somewhere.
   */
  private def bodyOf(out: String, name: String): String = {
    val lines = out.linesIterator.toList
    val start = lines.indexWhere(l => l.startsWith("define") && l.contains(name))

    if start < 0 then fail(s"nothing defines a function named '$name':\n$out")

    val end = lines.indexWhere(_ == "}", start)

    lines.slice(start, if end < 0 then lines.length else end + 1).mkString("\n")
  }

  /** The shape the feature exists for: growth behind a name that is deliberately not inlined, and a
   * store on the line after it whose index the growth is what makes valid.
   *
   * `fill` is what the clause is written about in the library too — `self.count < self.elems.len` is
   * exactly the fact the subscript needs, said by the callee that established it.
   */
  private def grower(clause: String) =
    s"""struct Store
       |    elems: []i32
       |    count: usize
       |
       |    @noinline
       |    @cold
       |    private grow(*self)
       |$clause        self.elems = [0; self.count + 8usize]
       |
       |    @noinline
       |    store(*self, v: i32)
       |        self.grow()
       |
       |        self.elems[self.count] = v
       |
       |var s = Store([0; 4], 0usize)
       |s.store(7)
       |print(s.count)
       |""".stripMargin

  "a postcondition reaches the caller" - {

    // The claim, against the control below it: the store after the call needs no test of its own,
    // because the callee promised the index is in range and traps if it is not.
    "the bounds test after the call is folded away" in {
      bodyOf(optimizedIr(grower("        ensure self.count < self.elems.len\n")), "store") should
        not include "@llvm.trap"
    }

    // The same program with the promise taken out. Without this the case above would pass on a
    // program whose store had no test to begin with.
    "and is still there without the clause" in {
      bodyOf(optimizedIr(grower("")), "store") should include("@llvm.trap")
    }

    // The mechanism itself, read in the emitted text rather than in what an optimizer made of it —
    // so a failure above can be told from a failure to lay anything down at all.
    "the clause is laid down as an assume at the call site" in {
      ir(grower("        ensure self.count < self.elems.len\n")) should include("@llvm.assume")
    }
  }

  /** A precondition covering the subscript underneath it, which is the case that settles whether a
   * callee needs an assume at its **entry** as well.
   */
  private val covered =
    """@noinline
      |at(xs: []const i32, i: usize) -> i32
      |    require i < xs.len
      |    xs[i]
      |
      |val ns: []i32 = [1, 2, 3]
      |print(at(ns, 1usize))
      |""".stripMargin

  "a precondition is already a fact inside its own body" - {

    // Two checks are emitted, and they are the same comparison written twice: the precondition's,
    // and the subscript's own.
    "the compiler emits the precondition's check and the subscript's" in {
      val out = bodyOf(ir(covered), "@at")

      out should include("require.bad")
      out should include("bounds.bad")
    }

    // And one survives. `require` traps on false before the body runs, so its branch **dominates**
    // the subscript, and LLVM reads a dominating branch as a fact about the values it tested: the
    // second comparison folds into the first with nothing added.
    //
    // **This is why nothing is assumed at a callee's entry.** An `llvm.assume` there would restate
    // what the branch already says, and an assume is not free — it keeps its operands alive to the
    // end of the block. The call *site* is the only place the fact is otherwise unavailable, which
    // is where the one assume goes.
    "and the optimizer folds them into one" in {
      val out = bodyOf(optimizedIr(covered), "@at")

      "@llvm.trap".r.findAllIn(out).size shouldBe 1
    }
  }

  "a clause that cannot be moved is not repeated" - {

    // A call in the clause is the plain case: the callee evaluates it once on its way out, and
    // repeating it at the caller would run it a second time.
    "one that calls something" in {
      val src =
        """@noinline
          |floor() -> usize = 3usize
          |
          |@noinline
          |wide(n: usize) -> usize
          |    ensure result >= floor()
          |    n + 8usize
          |
          |print(wide(1usize))
          |""".stripMargin

      ir(src) should not include "@llvm.assume"
    }

    // `old(e)` is a slot in the callee's frame holding the value a parameter had on entry. A caller
    // has neither the slot nor the value, so there is nothing for the clause to read.
    "one that reads 'old'" in {
      val src =
        """@noinline
          |bump(p: *usize)
          |    ensure *p > old(*p)
          |    *p += 1usize
          |
          |var n: usize = 1usize
          |bump(&n)
          |print(n)
          |""".stripMargin

      ir(src) should not include "@llvm.assume"
    }

    // A clause that traps on its way to an answer: the subscript inside it is a check and a branch,
    // and a caller repeating it would be running a second check rather than stating a fact.
    "one that subscripts" in {
      val src =
        """@noinline
          |first(xs: []const i32) -> i32
          |    ensure result == xs[0]
          |    xs[0]
          |
          |val ns: []i32 = [4, 5]
          |print(first(ns))
          |""".stripMargin

      ir(src) should not include "@llvm.assume"
    }
  }

  "the switch the assume is wired to" - {

    // The only way a clause does not run is `@ghost`: it names a function that is erased before
    // codegen, so nothing checks it and nothing may claim it. The assume follows that switch rather
    // than a second test of its own — which is the invariant the whole feature rests on.
    "a ghostly clause is neither checked nor assumed" in {
      val src =
        """@ghost
          |big_enough(n: usize) -> bool = n >= 8usize
          |
          |@noinline
          |wide(n: usize) -> usize
          |    ensure big_enough(result)
          |    n + 8usize
          |
          |print(wide(1usize))
          |""".stripMargin
      val out = ir(src)

      out should not include "@llvm.assume"
      bodyOf(out, "@wide") should not include "@llvm.trap"
    }
  }

  "an argument is substituted only where reading it twice is reading the same thing" - {

    // The parameter is filled from a call, so the clause cannot be written in terms of it: putting
    // the argument in would compute a second value, and it is not the one the callee checked.
    "an argument that is itself a call" in {
      val src =
        """@noinline
          |n() -> usize = 2usize
          |
          |@noinline
          |wide(k: usize) -> usize
          |    ensure result >= k
          |    k + 8usize
          |
          |print(wide(n()))
          |""".stripMargin

      ir(src) should not include "@llvm.assume"
    }

    // The dangerous case, and the reason an argument is tested at all rather than trusted: the
    // callee is handed a pointer to the very variable the other argument reads, and writes through
    // it before it returns. The promise was made about the value that went in — re-reading `n`
    // here would read the 99 that came out, and `result >= 99` is false where `result >= n` was
    // true.
    "an argument the call itself can write" in {
      val src =
        """@noinline
          |bump(p: *usize, k: usize) -> usize
          |    ensure result >= k
          |    *p = 99usize
          |    k + 8usize
          |
          |fn(n: *usize) -> usize = bump(n, *n)
          |
          |var m: usize = 1usize
          |print(fn(&m))
          |""".stripMargin

      ir(src) should not include "@llvm.assume"
    }

    // A global is the same case with nothing visible to point at it: any call at all may write one,
    // and the clause would be repeated about whatever the call left behind. A top-level `var` is
    // **not** one — it is a local of the entry function like any other, which is why this says
    // `static`.
    "an argument read out of a global" in {
      val src =
        """@noinline
          |wide(k: usize) -> usize
          |    ensure result >= k
          |    k + 8usize
          |
          |static var g: usize = 2usize
          |print(wide(g))
          |""".stripMargin

      ir(src) should not include "@llvm.assume"
    }

    // And the case it must not refuse: a local the caller never let an address out of cannot have
    // been written by the call, so reading it twice reads the same value.
    "an argument that is a local" in {
      val src =
        """@noinline
          |wide(k: usize) -> usize
          |    ensure result >= k
          |    k + 8usize
          |
          |@noinline
          |use(m: usize) -> usize = wide(m)
          |
          |print(use(2usize))
          |""".stripMargin

      ir(src) should include("@llvm.assume")
    }
  }
}
