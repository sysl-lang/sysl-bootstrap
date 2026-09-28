package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Type packs and the unrolled loop that walks one (`reference/generics.md § A parameter may stand
 * for a list of types`) — a parameter standing for a **list** of types, written `[..A]`, which is
 * what lets one declaration cover every tuple.
 */
class TypePackTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "the parameter list" - {
    "reads a pack beside an ordinary type parameter" in {
      run("""first[..A, T](t: (..A), x: T) -> T = x
            |print(first((1, 2, 3), "ok"))""".stripMargin) shouldBe "ok\n"
    }

    /** A default is one type and a pack stands for a list of them, so there is nothing a default
     * could be. Raised at the `=` rather than after the whole form, since a failure written after
     * one is outranked by whatever the form itself reached.
     */
    "refuses a default on a pack" in {
      err("f[..A = int](t: (..A)) = 0\nprint(1)") should include("a type pack takes no default")
    }

    "refuses a pack on a struct, whose parameters are its own shape" in {
      err("struct Row[..A]\n    x: int\nend Row\nprint(1)") should
        include("has no way to spread one over its own shape")
    }

    "refuses a pack on an enum" in {
      err("enum E[..A]\n    One\nend E\nprint(1)") should
        include("has no way to spread one over its own shape")
    }

    "refuses a pack on a trait" in {
      err("trait T[..A]\n    f(self) -> int\nprint(1)") should
        include("has no way to spread one over its own shape")
    }
  }

  "the tuple of a pack" - {
    "matches a tuple of any arity" in {
      run("""count[..A](t: (..A)) -> usize = A.len
            |print(count((1, 2)))
            |print(count((1, 2, 3)))
            |print(count((1, "a", true, 2.5, 'c')))""".stripMargin) shouldBe "2\n3\n5\n"
    }

    /** `(..A, int)` is pack *expansion*, which is not built — so it is refused by name rather than
     * left to fail as a type called `..A`, which is what a reader would then go looking for.
     */
    "refuses parts written beside a pack" in {
      err("f[..A](t: (..A, int)) = 0\nprint(1)") should include("appends to a pack")
    }

    "refuses a bare pack where a type belongs" in {
      err("f[..A](x: ..A) = 0\nprint(1)") should include("is a type pack and not a type")
    }

    "refuses a name declared as one type but spread as a list" in {
      err("f[T](t: (..T)) -> int = 0\nprint(1)") should
        include("a parameter that stands for a list is declared '..T'")
    }

    "refuses a pack that was never declared" in {
      err("f(t: (..A)) -> int = 0\nprint(1)") should
        include("a pack is declared in the parameter list, as '[..A]'")
    }
  }

  "'for const'" - {
    "unrolls over a pack's length" in {
      run("""shout[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        print(t.i)
            |shout((1, "two", true))""".stripMargin) shouldBe "1\ntwo\ntrue\n"
    }

    /** The whole feature: one written line covers parts of *different* types, because each copy is
     * type-checked on its own. A single typed body could not exist here.
     */
    "type-checks each copy separately" in {
      run("""widths[..A: Display](t: (..A)) -> usize
            |    var n = 0usize
            |    for const i in 0..<A.len
            |        n = n + str(t.i).len
            |    n
            |print(widths((1, "abc", true)))""".stripMargin) shouldBe "8\n"
    }

    "folds the index into an ordinary comparison" in {
      run("""joined[..A: Display](t: (..A)) -> string
            |    var s = ""
            |    for const i in 0..<A.len
            |        if i > 0usize then s = s + "-"
            |        s = s + str(t.i)
            |    s
            |print(joined((1, 2, 3)))""".stripMargin) shouldBe "1-2-3\n"
    }

    "takes an inclusive range too" in {
      run("""f[..A: Display](t: (..A))
            |    for const i in 0..1
            |        print(t.i)
            |f((7, 8, 9))""".stripMargin) shouldBe "7\n8\n"
    }

    /** A copy is a block, so a `var` written in one is that copy's own — not one variable
     * redeclared as many times as the loop unrolls to.
     */
    "gives each copy its own scope" in {
      run("""f[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        var here = str(t.i)
            |        print(here)
            |f((1, 2))""".stripMargin) shouldBe "1\n2\n"
    }

    "returns out of the enclosing function" in {
      run("""any_empty[..A: Display](t: (..A)) -> bool
            |    for const i in 0..<A.len
            |        if str(t.i).len == 0usize then return true
            |    false
            |print(any_empty(("a", "")), any_empty(("a", "b")))""".stripMargin) shouldBe "true false\n"
    }

    "wants a range known at compile time" in {
      err("""f(n: usize)
            |    for const i in 0..<n
            |        print(i)
            |f(3usize)""".stripMargin) should include("must be known at compile time")
    }

    "wants a range at all" in {
      err("""f(xs: []int)
            |    for const i in xs
            |        print(i)
            |f([1, 2])""".stripMargin) should include("walks a range with both ends written")
    }

    /** There is no loop at run time for either to act on: what the analyzer produces is the copies
     * in a block, so a `break` written here would silently leave whatever loop the `for const`
     * happens to sit inside — which is the wrong answer rather than a missing feature.
     */
    "refuses 'break'" in {
      err("""f[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        break
            |f((1, 2))""".stripMargin) should include("there is no loop for")
    }

    "refuses 'continue'" in {
      err("""f[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        continue
            |f((1, 2))""".stripMargin) should include("there is no loop for")
    }

    /** The copies are a sequence in the enclosing block, so a `break` in one of them would leave the
     * *outer* loop — silently, and one copy at a time. Refused for that reason and not because an
     * unrolled loop has no use for the word.
     */
    "allows a real loop written inside the body to break" in {
      run("""f[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        for n in 0..<5
            |            if n > 0 then break
            |            print(t.i)
            |f((1, 2))""".stripMargin) shouldBe "1\n2\n"
    }

    "refuses 'break' even inside an enclosing loop" in {
      err("""f[..A: Display](t: (..A))
            |    for n in 0..<2
            |        for const i in 0..<A.len
            |            break
            |f((1, 2))""".stripMargin) should include("there is no loop for")
    }

    "refuses a range longer than it will unroll" in {
      err("""f()
            |    for const i in 0..<100
            |        print(i)
            |f()""".stripMargin) should include("is the most one is unrolled to")
    }
  }

  "the edges" - {
    /** Zero copies, which is what an empty range unrolls to. There is no zero-tuple to reach this
     * from a pack (`reference/types.md § Tuples`), so it takes a written range — and it is worth
     * pinning that the answer is nothing rather than a copy at some default.
     */
    "an empty range unrolls to nothing" in {
      run("""f()
            |    print("before")
            |    for const i in 0..<0
            |        print("never")
            |    print("after")
            |f()""".stripMargin) shouldBe "before\nafter\n"
    }

    "nested unrolled loops multiply" in {
      run("""f[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        for const j in 0..<A.len
            |            print(t.i, t.j)
            |f((1, 2))""".stripMargin) shouldBe "1 1\n1 2\n2 1\n2 2\n"
    }

    "one pack may stand in two parameters, at one arity" in {
      run("""zip_len[..A: Display](x: (..A), y: (..A)) -> usize = A.len
            |print(zip_len((1, 2), (3, 4)))""".stripMargin) shouldBe "2\n"
    }

    /** A pack binds from the first argument that names it and the later one is blamed, exactly as
     * `same[T](x: T, y: T)` handed `(1, "a")` blames `y`: unification writes only where the map is
     * silent. The pack used to be re-bound by every argument, so the last one won and the first was
     * reported against an arity it never had.
     */
    "and refuses two arities for one pack, blaming the later argument" in {
      val e = err("""zip_len[..A: Display](x: (..A), y: (..A)) -> usize = A.len
                    |print(zip_len((1, 2), (3, 4, 5)))""".stripMargin)
      e should include("'y' of 'zip_len' is (int, int), but (int, int, int) was given")
      e should include("<input>:2:24")
    }

    "binds a pack from the first of three arguments, whichever later one disagrees" in {
      val src = """zip3[..A: Display](x: (..A), y: (..A), z: (..A)) -> usize = A.len
                  |""".stripMargin
      run(src + "print(zip3((1, 2), (3, 4), (5, 6)))") shouldBe "2\n"
      val third = err(src + "print(zip3((1, 2), (3, 4), (5, 6, 7)))")
      third should include("'z' of 'zip3' is (int, int), but (int, int, int) was given")
      third should include("<input>:2:29")
      val second = err(src + "print(zip3((1, 2), (3, 4, 5), (6, 7)))")
      second should include("'y' of 'zip3' is (int, int), but (int, int, int) was given")
      second should include("<input>:2:21")
    }

    "binds a pack beside a scalar parameter by the same rule" in {
      val src = """tagged[T: Display, ..A: Display](k: T, x: (..A), j: T, y: (..A)) -> usize = A.len
                  |""".stripMargin
      run(src + """print(tagged("k", (1, true), "j", (2, false)))""") shouldBe "2\n"
      err(src + """print(tagged("k", (1, 2), "j", (3, 4, 5)))""") should include(
        "'y' of 'tagged' is (int, int), but (int, int, int) was given")
      err(src + """print(tagged("k", (1, 2), 7, (3, 4)))""") should include(
        "'j' of 'tagged' is string, but int was given")
    }

    /** Where the first argument is not a tuple at all it binds nothing, so the pack is read off the
     * second and the first is the one blamed — for being no tuple, which is the true complaint.
     */
    "blames a first argument that is no tuple, against the arity the second gave" in {
      val e = err("""zip_len[..A: Display](x: (..A), y: (..A)) -> usize = A.len
                    |print(zip_len(5, (3, 4)))""".stripMargin)
      e should include("'x' of 'zip_len' is (int, int), but int was given")
      e should include("<input>:2:15")
    }

    /** Every part is counted, so the parts of a tuple rendered through the pack's block are retained
     * and released exactly as a struct's fields are (`03`). Strings built at run time are what makes
     * this a real question rather than a formality — a literal owns nothing to get wrong.
     */
    "counts the parts of a tuple whose members are refcounted" in {
      run("""var a = "x" + "y"
            |var b = "p" + "q"
            |val t = (a, b, a + b)
            |print(t)
            |print(t)""".stripMargin) shouldBe "(xy, pq, xypq)\n(xy, pq, xypq)\n"
    }

    /** Each part erased to a `&Display` **individually**, which is a different capability from the
     * whole tuple rendering itself: a caller that lays parts out has to see them one at a time, and
     * a tuple's own `Display` answers a single joined string with no way back into it.
     *
     * A bound is a promise about one type, so a heterogeneous row is written as a slice of erased
     * elements; this is the same erasure reached through a pack instead.
     *
     * **The part is passed as it stands, not as `&t.i`.** Erasure is a coercion at a position whose
     * type is already the trait; `&` is address-of, so writing it there asks for a `*int` and gets
     * one, and the diagnostic then reads as though the erasure were unavailable.
     */
    "erases each part to a trait object on its own" in {
      run("""import sysl.buf.byte_sink
            |width(v: &Display) -> usize
            |    var sink = byte_sink()
            |    var out: *Writer = &sink
            |    v.display(out, FormatSpec(0, -1, false))
            |    sink.text().len
            |row[..A: Display](t: (..A)) -> usize
            |    var n = 0usize
            |    for const i in 0..<A.len
            |        n = n + width(t.i)
            |    n
            |print(row((1, "abc", true)))""".stripMargin) shouldBe "8\n"
    }

    /** A pack on a **method's own** parameter list, which is a different position from a struct's.
     * `Row[..A]` is refused because a struct has nowhere to spread a list over its own shape, and
     * that reason does not reach a method, whose parameters are its own exactly as a free
     * function's are.
     *
     * The two lists meet in the lowered function, so a member is generic over the block's parameters
     * and its own together — which is why the kinds have to come from both sides and not just from
     * the block.
     */
    "may stand on a method of an ordinary struct" in {
      run("""struct Row
            |    n: usize
            |    take[..A: Display](*self, t: (..A))
            |        for const i in 0..<A.len
            |            self.n = self.n + str(t.i).len
            |end Row
            |var r = Row(0usize)
            |r.take((1, "abc", true))
            |print(r.n)""".stripMargin) shouldBe "8\n"
    }

    /** And on a **trait's** member too, for a reason that has nothing to do with packs: a member may
     * declare parameters of its own, and a pack is one more way of writing that list, so it meets
     * the rule already there rather than one of its own.
     *
     * What such a member gives up is its table slot — no slot can hold a function that does not
     * exist until a call names its types — so it is reached on a value whose type is known, or
     * through a bound, and not on an object.
     */
    "and on a trait's member, which is the same list written another way" in {
      run("""trait Take
            |    take[..A: Display](self, t: (..A)) -> usize
            |struct Row
            |    n: usize
            |impl Take for Row
            |    take[..A: Display](self, t: (..A)) -> usize
            |        var total = self.n
            |        for const i in 0..<A.len
            |            total = total + str(t.i).len
            |        total
            |print(Row(0).take((1, "abc", true)))""".stripMargin) shouldBe "8\n"
    }

    "a bound reaches one, and unrolls it at the instantiation" in {
      run("""trait Take
            |    take[..A: Display](self, t: (..A)) -> usize
            |struct Row
            |    n: usize
            |impl Take for Row
            |    take[..A: Display](self, t: (..A)) -> usize
            |        var total = self.n
            |        for const i in 0..<A.len
            |            total = total + str(t.i).len
            |        total
            |through[S: Take](s: S) -> usize = s.take((1, "abc", true))
            |print(through(Row(0)))""".stripMargin) shouldBe "8\n"
    }

    "an object cannot, and the refusal names the member" in {
      err("""trait Take
            |    tag(self) -> usize
            |    take[..A: Display](self, t: (..A)) -> usize
            |struct Row
            |    n: usize
            |impl Take for Row
            |    tag(self) -> usize = self.n
            |    take[..A: Display](self, t: (..A)) -> usize = self.n
            |val o: &Take = Row(5)
            |print(o.take((1, "ab")))""".stripMargin) should
        include("'take' of 'Take' declares type parameters of its own")
    }

    "reaches the parts through a reference receiver" in {
      run("""trait Tag
            |    tag(self) -> string
            |impl[..A: Display] Tag for (..A)
            |    tag(self) -> string
            |        var s = ""
            |        for const i in 0..<A.len
            |            s = s + str(self.i)
            |        s
            |var t = (1, 2, 3)
            |var r = &t
            |print(r.tag())""".stripMargin) shouldBe "123\n"
    }
  }

  "a pack in a signature" - {
    "may be the result" in {
      run("""pair[..A](t: (..A)) -> (..A) = t
            |print(pair((1, "x")))""".stripMargin) shouldBe "(1, x)\n"
    }

    /** Spreading a pack into an *argument list* is expansion, which is not built — the unrolled loop
     * is what replaces it, and in the one direction the catalog wants.
     */
    "is not spread into a call's arguments" in {
      err("""f[..A: Display](t: (..A))
            |    print(..t)
            |f((1, 2))""".stripMargin) should not be empty
    }
  }

  "a compile-time index" - {
    "selects a part by its value" in {
      run("""f[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        print(t.i)
            |f(("x", "y"))""".stripMargin) shouldBe "x\ny\n"
    }

    "names the constant when it runs past the end" in {
      err("""f[..A: Display](t: (..A))
            |    for const i in 0..2
            |        print(t.i)
            |f((1, 2))""".stripMargin) should include("'i' is 2 here")
    }

    /** A struct's fields have names, and a number does not address one — said here rather than left
     * to a complaint about a missing property, which sends a reader looking for a field.
     */
    "is refused on a struct" in {
      err("""struct P
            |    x: int
            |    y: int
            |end P
            |f(p: P)
            |    for const i in 0..<1
            |        print(p.i)
            |f(P(1, 2))""".stripMargin) should include("its fields are reached by name")
    }

  }

  "a pack's bound" - {
    "distributes over the members" in {
      run("""show_all[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        print(t.i)
            |show_all((1, 2.5, 'z'))""".stripMargin) shouldBe "1\n2.5\nz\n"
    }

    /** The property a bounded generic exists to have: the membership is answered where the call is
     * written, not inside a body that has already been committed to.
     */
    "is checked at the call, not inside the body" in {
      err("""struct Q
            |    x: int
            |end Q
            |show_all[..A: Display](t: (..A))
            |    for const i in 0..<A.len
            |        print(t.i)
            |show_all((1, Q(2)))""".stripMargin) should include("Display")
    }

    /** With no bound the parts promise nothing, and the complaint names the pack the way the reader
     * wrote it — `A`, not the numbered stand-in the walk builds behind it.
     */
    "reports the missing bound inside the body when nothing promises it" in {
      val m = err("""f[..A](t: (..A))
                    |    for const i in 0..<A.len
                    |        print(t.i)
                    |f((1, 2))""".stripMargin)

      m should include("sysl.Display")
      m should include("'A:")
      m should not include "A#"
    }

    "'len' is the whole of what a pack offers" in {
      err("f[..A](t: (..A)) -> usize = A.size\nprint(f((1, 2)))") should include("there is no 'A.size'")
    }
  }

  "the body is checked at a representative arity of two" - {
    /** The walk that checks a generic body stands the pack at two types, so a body that only works
     * on the first part is caught at the declaration rather than at somebody's first triple.
     */
    /** Nothing calls `f`, so this is the walk reporting and nothing else — which is the property a
     * generic body is walked for at all: a body that assumes more than it declared is wrong whether
     * or not anything ever instantiates it.
     */
    "reports a bound the body wants with no call anywhere" in {
      err("""f[..A](t: (..A))
            |    for const i in 0..<A.len
            |        print(t.i)
            |print(1)""".stripMargin) should include("needs 'A: sysl.Display'")
    }

    /** The parts are two *different* types, so a body that would only work if they were the same is
     * caught — which is what one stand-in could not have caught.
     */
    "makes the parts two types rather than one" in {
      err("""f[..A: Display](t: (..A)) -> bool = t.0 == t.1
            |print(1)""".stripMargin) should include("A")
    }

    /** An arity mistake is *not* the walk's to report: the abstract pass reports what the bounds do
     * not license and drops the rest, so this is found at the instantiation, which has a real arity
     * to be past. Called at three parts, where the loop is in range, and it compiles.
     */
    "leaves an arity mistake to the instantiation" in {
      run("""f[..A: Display](t: (..A))
            |    for const i in 0..2
            |        print(t.i)
            |f((1, 2, 3))""".stripMargin) shouldBe "1\n2\n3\n"
    }
  }

  "an 'impl' over every tuple" - {
    "is filed under a shape of its own" in {
      run("""trait Tag
            |    tag(self) -> string
            |impl[..A: Display] Tag for (..A)
            |    tag(self) -> string
            |        var s = ""
            |        for const i in 0..<A.len
            |            s = s + str(self.i)
            |        s
            |print((1, 2).tag(), (1, 2, 3).tag(), ("a", "b", "c", "d").tag())""".stripMargin) shouldBe
        "12 123 abcd\n"
    }

    /** The third rung of the ladder a tuple's own type and its arity's shape begin —
     * `reference/traits.md § override — when the overlap is deliberate`'s "written-out beats a
     * parameter", applied twice down one chain.
     */
    "loses to a block written for one arity" in {
      run("""trait Tag
            |    tag(self) -> string
            |impl[..A: Display] Tag for (..A)
            |    tag(self) -> string = "any"
            |impl[A: Display, B: Display] Tag for (A, B)
            |    tag(self) -> string = "pair"
            |print((1, 2).tag(), (1, 2, 3).tag())""".stripMargin) shouldBe "pair any\n"
    }

    "loses to a block written for one tuple type" in {
      run("""trait Tag
            |    tag(self) -> string
            |impl[..A: Display] Tag for (..A)
            |    tag(self) -> string = "any"
            |impl Tag for (int, int)
            |    tag(self) -> string = "two ints"
            |print((1, 2).tag(), (1, "x").tag())""".stripMargin) shouldBe "two ints any\n"
    }

    "makes two arities two instantiations" in {
      run("""trait Size
            |    size(self) -> usize
            |impl[..A] Size for (..A)
            |    size(self) -> usize = A.len
            |print((1, 2).size(), (1, 2, 3, 4).size())""".stripMargin) shouldBe "2 4\n"
    }
  }

  "the library's catalog" - {
    "prints a tuple of any arity" in {
      run("""print((1, "a", true, 2.5))""") shouldBe "(1, a, true, 2.5)\n"
    }

    "prints a pair and a triple exactly as before" in {
      run("""print((1, "one"))
            |print(str((1, 2, 3)))""".stripMargin) shouldBe "(1, one)\n(1, 2, 3)\n"
    }

    "pads the whole tuple as one field" in {
      run("""val p = (1, 2)
            |print(f"[${p}%10s]", f"[${p}%-10s]")""".stripMargin) shouldBe "[    (1, 2)] [(1, 2)    ]\n"
    }

    "compares wide tuples for equality" in {
      run("""print((1, 2, 3, 4) == (1, 2, 3, 4), (1, 2, 3, 4) == (1, 2, 9, 4))""") shouldBe "true false\n"
    }

    /** Lexicographic, and the loop drops the last-position special case the hand-written ladder
     * had: every position runs the two-test ladder and all-tied ends `false`.
     */
    "orders wide tuples lexicographically" in {
      run("""print((1, 1, 1, 2) < (1, 1, 1, 3), (1, 2, 0, 0) < (1, 1, 9, 9), (1, 2) < (1, 2))""") shouldBe
        "true false false\n"
    }

    "hashes a wide tuple, and by the order of its parts" in {
      run("""print((1, 2, 3, 4).hash() == (1, 2, 3, 4).hash(), (1, 2, 3, 4).hash() == (4, 3, 2, 1).hash())""") shouldBe
        "true false\n"
    }

    "still refuses a tuple whose parts are not all printable" in {
      err("""struct Q
            |    x: int
            |end Q
            |print((1, Q(2)))""".stripMargin) should include("cannot print")
    }

    /** An array of tuples: the array block covers every length (`reference/generics.md § A
     * parameter may stand for a value`) and the tuple block every arity, so the two compose with
     * nothing written for the pair of them.
     */
    "prints an array of tuples" in {
      run("""var a: [2](int, int) = [(1, 2), (3, 4)]
            |print(a)""".stripMargin) shouldBe "[(1, 2), (3, 4)]\n"
    }

    "prints a tuple of tuples" in {
      run("""print(((1, 2), (3, 4, 5)))""") shouldBe "((1, 2), (3, 4, 5))\n"
    }
  }
}
