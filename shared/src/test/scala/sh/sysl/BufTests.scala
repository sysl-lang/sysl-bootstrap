package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `Buf[T]` — a growable array (`reference/arrays.md § Growing one`).
 *
 * It is **ordinary sysl in the library**, not a type the compiler knows, and that is the finding
 * rather than an implementation note. Storage sized while running gave a library the one thing it
 * was missing: a `[]T` field is storage a container can make for itself and that ARC destroys on
 * its behalf, so no `Drop`, no `sizeof` over a parameter, and no pointer cast are needed.
 *
 * The other thing that had looked like a blocker was that a generic container cannot make its own
 * storage, since a repeat needs a value and no bound promises one. A `push` arrives holding one —
 * so the value being pushed seeds the new storage, and the question never comes up.
 *
 * **What a push does to the other views** was `07`'s open question, and the answer is that sysl
 * does not have to choose one: a growable array is a struct, so whether a push is seen follows from
 * whether it is held by reference or by value, which is a choice the language already makes you
 * write. Go's confusion comes from having only one answer available.
 */
class BufTests extends AnyFreeSpec with RunSupport {

  /** `Buf` is `sysl.buf`'s: a growable sequence is something a program asks for, not something the
   * language desugars onto. Written once here so that each program below is about the sequence.
   */
  private val importing = "import sysl.buf.*\n\n"

  override protected def run(src: String, optimize: String = Toolchain.defaultOptimization): String =
    super.run(importing + src, optimize)

  override protected def exits(src: String): Unit = super.exits(importing + src)

  override protected def panics(src: String, message: String): Unit = super.panics(importing + src, message)

  "the basic shape" - {
    "push, len, and read back" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<5 do b.push(i * i)
          |print(b.len(), b.at(0), b.at(4))""".stripMargin
      ) shouldBe "5 0 16\n"
    }

    "set writes an element that is there" in {
      run(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |b.push(2)
          |b.set(0, 99)
          |print(b.at(0), b.at(1))""".stripMargin
      ) shouldBe "99 2\n"
    }

    "pop takes the last one back off" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<3 do b.push(i)
          |print(b.pop().unwrap(), b.pop().unwrap(), b.len(), b.pop().unwrap(), b.pop().is_none())""".stripMargin
      ) shouldBe "2 1 1 0 true\n"
    }

    "clear empties it without giving the storage back" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<10 do b.push(i)
          |var had = b.cap()
          |b.clear()
          |print(b.len(), b.is_empty(), b.cap() == had)""".stripMargin
      ) shouldBe "0 true true\n"
    }

    "and a view is the elements that are actually there" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<5 do b.push(i)
          |var s = b.view()
          |var total = 0
          |for x in s do total += x
          |print(s.len, total, s[4])""".stripMargin
      ) shouldBe "5 10 4\n"
    }

    /* The view is the WRITABLE form, and these two facts together are why: a read site pays nothing
     * for it, because `[]T` widens to `[]const T` on its own, while a read-only view would foreclose
     * mutating a buffer in place — which is the only route a sort or a reverse could ever take, since
     * `set` reaches one element at a time. Strictly more capable, at no cost to the readers. */
    "and that view may be written, while still satisfying a reader that asks for a read-only one" in {
      run(
        """total(xs: []const int) -> int
          |    var s = 0
          |    for x in xs do s += x
          |    s
          |var b: &Buf[int] = buf()
          |b.push(3)
          |b.push(4)
          |var v = b.view()
          |v[0] = 10
          |print(total(b.view()), b.at(0))""".stripMargin
      ) shouldBe "14 10\n"
    }
  }

  // A list somebody *leaves*: taking an element out of the middle, and cutting a length down to a
  // number. `remove` is written in terms of `truncate`, which is what `reference/arrays.md § What
  // is still refused` said it should be, and `clear` is `truncate(0)` — so all three shorten a
  // buffer the one way. What `truncate` does to the *length* — cutting to a number, and a number
  // past the end being a no-op rather than a panic — is asserted in `library/sysl/buf/tests.sysl`,
  // where the buffer is. What stays here is what the length cannot show: the storage that survives
  // a truncation, and the counted elements it lets go of.
  "shortening one" - {
    "truncating to nothing is what clear does" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<4 do b.push(i)
          |var had = b.cap()
          |b.truncate(0)
          |print(b.len(), b.is_empty(), b.cap() == had)""".stripMargin
      ) shouldBe "0 true true\n"
    }

    "remove hands back the element it took out" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<5 do b.push(i * 10)
          |print(b.remove(1), b.len(), b.at(0), b.at(1), b.at(2), b.at(3))""".stripMargin
      ) shouldBe "10 4 0 20 30 40\n"
    }

    // The first and last elements are where an off-by-one in the shift shows: removing at 0 moves
    // every survivor, and removing the last moves none.
    "at either end" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<4 do b.push(i)
          |print(b.remove(0), b.at(0), b.at(2), b.len())
          |print(b.remove(2), b.at(0), b.at(1), b.len())""".stripMargin
      ) shouldBe "0 1 3 3\n3 1 2 2\n"
    }

    "the only element" in {
      run(
        """var b: &Buf[int] = buf()
          |b.push(7)
          |print(b.remove(0), b.len(), b.is_empty())""".stripMargin
      ) shouldBe "7 0 true\n"
    }

    // Every element removed one at a time, always at the front, so each removal shifts the whole
    // remainder — the order it comes out in is the whole assertion.
    "one at a time until there is nothing left" in {
      run(
        """import sysl.text.str_builder
          |
          |var b: &Buf[int] = buf()
          |for i in 0..<6 do b.push(i)
          |var out = str_builder()
          |while !b.is_empty() do out.push(str(b.remove(0)))
          |print(out.finish(), b.len())""".stripMargin
      ) shouldBe "012345 0\n"
    }

    // An element that holds something is handed to the caller *and* shifted past by its neighbours,
    // so a count taken once and a count taken twice look identical until a buffer of references is
    // churned. Removing from the front of a hundred thousand does both at every step.
    "removing an element that holds something balances its count" in {
      run(
        """struct Cell
          |    v: int
          |end Cell
          |
          |var total = 0
          |
          |for round in 0..<100000
          |    var b: &Buf[&Cell] = buf()
          |    b.push(Cell(1))
          |    b.push(Cell(2))
          |    b.push(Cell(3))
          |
          |    var gone = b.remove(0)
          |    total += gone.v + b.at(0).v + b.at(1).v
          |
          |print(total)""".stripMargin
      ) shouldBe "600000\n"
    }

    "and a truncated one is let go of too" in {
      run(
        """var total = 0
          |
          |for round in 0..<100000
          |    var b: &Buf[string] = buf()
          |    for i in 0..<4 do b.push(s"item $i")
          |    b.truncate(1)
          |    total += int(b.at(0).len)
          |
          |print(total)""".stripMargin
      ) shouldBe "600000\n"
    }

    // A copy of a `Buf` copies the two fields and not the allocation behind them, so what a copy
    // keeps is the **count** it was taken at — the shift a removal makes lands in storage they
    // share, and the copy reads the shifted elements at a length that no longer describes them.
    // That is the same shallow copy `§ how a push is seen` is about, seen from the shortening side.
    "a copy taken before a removal keeps the length, not the elements" in {
      run(
        """var p: &Buf[int] = buf()
          |for i in 0..<3 do p.push(i)
          |var c = *p
          |print(p.remove(0), p.len(), p.at(0), c.len(), c.at(0), c.at(2))""".stripMargin
      ) shouldBe "0 2 1 3 1 2\n"
    }

    "removing an index that names no element says which and how many there were" in {
      panics(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |b.push(2)
          |print(b.remove(2))""".stripMargin,
        "past the 2 elements",
      )
    }

    "and so does removing from an empty one" in {
      panics(
        """var b: &Buf[int] = buf()
          |print(b.remove(0))""".stripMargin,
        "past the 0 elements",
      )
    }
  }

  "growth" - {
    "capacity doubles from nothing, and every element survives it" in {
      run(
        """var b: &Buf[int] = buf()
          |for i in 0..<100 do b.push(i)
          |var total = 0
          |for x in b.view() do total += x
          |print(b.len(), b.cap(), total, b.at(0), b.at(99))""".stripMargin
      ) shouldBe "100 128 4950 0 99\n"
    }

    // The count is the thing that grows; capacity is only ever a power of two above it, so a push
    // that fits costs no allocation at all.
    "a push that fits does not reallocate" in {
      run(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |var had = b.cap()
          |for i in 0..<7 do b.push(i)
          |print(had, b.cap(), b.len())""".stripMargin
      ) shouldBe "8 8 8\n"
    }

    "growth carries elements that hold something" in {
      run(
        """var b: &Buf[string] = buf()
          |for i in 0..<40 do b.push(s"k$i")
          |print(b.len(), b.at(0), b.at(39))""".stripMargin
      ) shouldBe "40 k0 k39\n"
    }
  }

  // This is the design answer. Go has one representation and therefore one behaviour, and the
  // behaviour it picked is why two Go slices agree until one of them grows. sysl already makes the
  // caller write which of the two they meant.
  "how a push is seen follows from how the buffer is held" - {
    "two names for one buffer see each other's pushes" in {
      run(
        """var p: &Buf[int] = buf()
          |var q = p
          |p.push(7)
          |q.push(8)
          |print(p.len(), q.len(), p.at(1), q.at(0))""".stripMargin
      ) shouldBe "2 2 8 7\n"
    }

    // A struct copied out of its box is a copy, which is what copying a struct means everywhere in
    // the language — nothing about `Buf` makes it an exception, and nothing about it is hidden.
    "a copy taken out of the reference is a copy" in {
      run(
        """var p: &Buf[int] = buf()
          |p.push(1)
          |var c = *p
          |c.push(2)
          |print(p.len(), c.len())""".stripMargin
      ) shouldBe "1 2\n"
    }

    "and one passed by reference is grown by the callee" in {
      run(
        """fill(b: &Buf[int], n: int)
          |    for i in 0..<n do b.push(i)
          |var p: &Buf[int] = buf()
          |fill(p, 30)
          |print(p.len(), p.at(29))""".stripMargin
      ) shouldBe "30 29\n"
    }
  }

  // The safety property that made a reference the right answer: an append can move the storage, and
  // a view taken before it keeps the storage it was made from alive rather than dangling.
  "a view taken before a grow stays valid" - {
    "showing the elements it was made from" in {
      run(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |b.push(2)
          |var early = b.view()
          |for i in 0..<50 do b.push(i)
          |print(early.len, early[0], early[1], b.len())""".stripMargin
      ) shouldBe "2 1 2 52\n"
    }

    "and a view of references keeps them alive too" in {
      run(
        """struct Node
          |    value: int
          |var b: &Buf[&Node] = buf()
          |b.push(Node(5))
          |var early = b.view()
          |for i in 0..<50 do b.push(Node(i))
          |print(early.len, early[0].value)""".stripMargin
      ) shouldBe "1 5\n"
    }
  }

  "elements that hold something are held and let go" - {
    "a buffer of references reads back what went in" in {
      run(
        """struct Node
          |    value: int
          |var b: &Buf[&Node] = buf()
          |for i in 0..<30 do b.push(Node(i * 3))
          |print(b.len(), b.at(0).value, b.at(29).value)""".stripMargin
      ) shouldBe "30 0 87\n"
    }

    "many grown buffers come apart rather than accumulating" in {
      run(
        """struct Node
          |    value: int
          |var i = 0
          |while i < 20000
          |    var t: &Buf[&Node] = buf()
          |    for k in 0..<10 do t.push(Node(k))
          |    i++
          |print("done")""".stripMargin
      ) shouldBe "done\n"
    }

    "a buffer of buffers" in {
      run(
        """var rows: &Buf[&Buf[int]] = buf()
          |for y in 0..<5
          |    var row: &Buf[int] = buf()
          |    for x in 0..<4 do row.push(y * 10 + x)
          |    rows.push(row)
          |print(rows.len(), rows.at(0).len(), rows.at(4).at(3))""".stripMargin
      ) shouldBe "5 4 43\n"
    }
  }

  /** A type whose destructor announces itself, so that the lines a program prints count the
   * releases the buffer made.
   */
  private val handle =
    """struct Handle
      |    id: int
      |
      |impl Drop for Handle
      |    drop(self) = print("drop", self.id)
      |""".stripMargin

  /** The counts themselves, read through a destructor rather than inferred from a value surviving.
   *
   * **The two failures a buffer of counted elements can have are invisible to every test above.** A
   * release too few leaks, and a program that leaks still prints the right answers; a release too
   * many frees an object something else is holding, and on a small program the freed bytes are
   * usually still readable. A destructor that announces itself turns both into different output: a
   * missing line is the leak and a repeated one is the double release.
   *
   * **The lines are sorted rather than pinned in order**, because the order is the reaper's — a
   * worklist drained when the first count reaches zero, so which element announces itself first is
   * a property of the drain rather than of the buffer. What the buffer promises is that each
   * element is let go of exactly once, and a sorted comparison says exactly that.
   */
  "a counted element is released exactly once" - {

    // Growth is where a double release would come from: the storage is seeded by repeating the
    // value being pushed, the old storage is copied across, and then the buffer lets the old
    // storage go. An element counted once for the copy and once for the seed would be freed twice.
    "across the growth that the first push causes" in {
      run(
        s"""$handle
           |hold()
           |    var b: &Buf[&Handle] = buf()
           |    for i in 0..<3 do b.push(Handle(i))
           |    print("filled", b.len())
           |
           |hold()
           |print("out")""".stripMargin
      ).linesIterator.toList.sorted shouldBe List("drop 0", "drop 1", "drop 2", "filled 3", "out")
    }

    // A slot past the count still holds whatever seeded it, so pushing over one releases that
    // occupant. Shortening and pushing again is the shortest program that reaches the case, and it
    // is the one a stack written on a buffer runs on every step.
    "when a push writes over a slot the count had left behind" in {
      run(
        s"""$handle
           |hold()
           |    var b: &Buf[&Handle] = buf()
           |    b.push(Handle(1))
           |    b.truncate(0)
           |    b.push(Handle(2))
           |    print("swapped", b.len())
           |
           |hold()
           |print("out")""".stripMargin
      ).linesIterator.toList.sorted shouldBe List("drop 1", "drop 2", "out", "swapped 1")
    }

    // Many growths rather than one, so that an element released once too often somewhere in the
    // middle of the doublings is caught as well as one at the join.
    "through every doubling on the way up" in {
      run(
        s"""$handle
           |hold()
           |    var b: &Buf[&Handle] = buf()
           |    for i in 0..<40 do b.push(Handle(i))
           |    print("filled", b.len())
           |
           |hold()
           |print("out")""".stripMargin
      ).linesIterator.count(_.startsWith("drop ")) shouldBe 40
    }
  }

  /** `extend` and `buf_with_capacity` — appending a run at once, and starting with room for one.
   *
   * Both are about the cost rather than the result, so the tests are written to pin the *result*
   * exactly and to catch the ways a bulk copy can go wrong that a loop of `push` cannot: an
   * off-by-one at the join between what was there and what arrives, a growth that fits the new run
   * but forgets the old elements, and the empty run that has no first element to seed storage with.
   */
  "extending by a whole slice" - {

    "appends every element after the ones already there" in {
      run("""var b: &Buf[int] = buf()
            |b.push(1)
            |b.push(2)
            |b.extend([3, 4, 5])
            |print(b.len(), b.at(0), b.at(2), b.at(4))""".stripMargin) shouldBe "5 1 3 5\n"
    }

    // The discriminating case for the growth: the run is longer than one doubling would give, so an
    // implementation that grew once by a factor of two and then copied would write past the end.
    "grows enough for a run far longer than the current capacity" in {
      run("""var b: &Buf[int] = buf()
            |b.push(7)
            |var xs: [40]int
            |for i in 0..<40 do xs[i] = i
            |b.extend(xs[..])
            |print(b.len(), b.at(0), b.at(1), b.at(40))""".stripMargin) shouldBe "41 7 0 39\n"
    }

    // The empty run — the one case `extend` has to leave early for, since it has no first element to
    // repeat into new storage — is asserted in `library/sysl/buf/tests.sysl`.
    "extending an empty buffer works the same as filling one" in {
      run("""var b: &Buf[int] = buf()
            |b.extend([9, 8])
            |print(b.len(), b.at(0), b.at(1))""".stripMargin) shouldBe "2 9 8\n"
    }

    // Repeated extends have to keep the geometric growth, or a loop of them is quadratic. What is
    // observable from sysl is that the result is right after many of them; the capacity check
    // below is what says the growth was not exact-fit.
    "many extends in a row keep every element in order" in {
      run("""var b: &Buf[int] = buf()
            |for i in 0..<20 do b.extend([i, i])
            |print(b.len(), b.at(0), b.at(1), b.at(38), b.at(39))""".stripMargin) shouldBe
        "40 0 0 19 19\n"
    }
  }

  "starting with room" - {

    "a buffer with capacity is still empty" in {
      run("""var b: &Buf[int] = buf_with_capacity(32usize, 0)
            |print(b.len(), b.is_empty(), b.cap())""".stripMargin) shouldBe "0 true 32\n"
    }

    // The point of the capacity: pushing up to it does not reallocate, so the capacity is the one
    // it was given rather than a power of two arrived at by doubling from eight.
    "filling up to the capacity does not grow it" in {
      run("""var b: &Buf[int] = buf_with_capacity(32usize, 0)
            |for i in 0..<32 do b.push(i)
            |print(b.len(), b.cap(), b.at(31))""".stripMargin) shouldBe "32 32 31\n"
    }

    "and going past it grows the way any other buffer does" in {
      run("""var b: &Buf[int] = buf_with_capacity(4usize, 0)
            |for i in 0..<5 do b.push(i)
            |print(b.len(), b.cap(), b.at(4))""".stripMargin) shouldBe "5 8 4\n"
    }

    // The fill is a parameter because a generic `T` has no zero, and none of those slots is ever
    // read — `count` is what says which elements are real, so `at` refuses them all.
    "the fill is never visible, since the count starts at zero" in {
      panics(
        """var b: &Buf[int] = buf_with_capacity(8usize, 99)
          |print(b.at(0))""".stripMargin,
        "past the 0 elements",
      )
    }
  }

  // Both blocks delegate to the slice's, over `view()` — so what is asserted here is that a `Buf`
  // says it is the sequence it holds, and that the storage past `count` is not part of it.
  //
  // **The buffers below are values, where every other block in this file holds a `&Buf`.** A
  // reference compares by address (`reference/memory.md § &T — counted references`), so `==` between
  // two `&Buf` asks whether they are the same buffer and not whether they hold the same elements —
  // and `print` on one refuses, a `&T` implementing nothing its referent implements. The last test
  // here pins both, since this file's own convention is the one that walks into them.
  "comparing and rendering one" - {
    val three = "var b: Buf[int] = buf()\nfor i in 1..3 do b.push(i)\n"

    "two buffers with the same elements are equal" in {
      run(three + "var c: Buf[int] = buf()\nfor i in 1..3 do c.push(i)\nprint(b == c)") shouldBe "true\n"
    }

    "a differing element or a differing length makes them unequal" in {
      val src =
        """var b: Buf[int] = buf()
          |var c: Buf[int] = buf()
          |var d: Buf[int] = buf()
          |for i in 1..3 do b.push(i)
          |c.push(1)
          |c.push(9)
          |c.push(3)
          |d.push(1)
          |d.push(2)
          |print(b == c, b == d, b != d)""".stripMargin

      run(src) shouldBe "false false true\n"
    }

    // The capacity is eight after two pushes and sixteen after nine, so a comparison reading the
    // backing slice rather than the view would be comparing slots nobody wrote.
    "the spare capacity is not part of the value" in {
      val src =
        """var b: Buf[int] = buf()
          |var c: Buf[int] = buf()
          |b.push(1)
          |b.push(2)
          |for i in 0..<9 do c.push(i)
          |c.truncate(2)
          |c.set(0, 1)
          |c.set(1, 2)
          |print(b.cap() == c.cap(), b == c)""".stripMargin

      run(src) shouldBe "false true\n"
    }

    "two empty buffers are equal" in {
      run("var b: Buf[int] = buf()\nvar c: Buf[int] = buf()\nprint(b == c)") shouldBe "true\n"
    }

    "it renders as the sequence it holds" in {
      run(three + "print(b)") shouldBe "[1, 2, 3]\n"
    }

    "an empty one renders as an empty sequence" in {
      run("var b: Buf[int] = buf()\nprint(b)") shouldBe "[]\n"
    }

    // A specifier describes the field the whole value occupies, which is the padding the slice
    // block already measures — the delegation is what gets it, rather than a second copy.
    "a width pads the whole rendering, on either side" in {
      run(three + """print(f"${b}%13s|")""" + "\n" + """print(f"${b}%-13s|")""") shouldBe
        "    [1, 2, 3]|\n[1, 2, 3]    |\n"
    }

    // Neither of these is about `Buf`: a reference compares by address and implements nothing its
    // referent implements, whatever the referent is. They are here because this file holds its
    // buffers by reference everywhere else, so this is where somebody meets both.
    "a reference to one compares by address, and the deref is the way past" in {
      val src =
        """var b: &Buf[int] = buf()
          |var c: &Buf[int] = buf()
          |var d = b
          |for i in 1..3
          |    b.push(i)
          |    c.push(i)
          |print(b == c, b == d, *b == *c)""".stripMargin

      run(src) shouldBe "false true true\n"
    }
  }

  "what stops the program" - {
    // Reading past the end is a panic rather than a trap, because a `Buf` is written in sysl and
    // what sysl has to stop with is the library's own `exit` — so it can say what went wrong.
    "reading past the last element says which index and how many there were" in {
      panics(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |print(b.at(3))""".stripMargin,
        "past the 1 elements",
      )
    }

    // The spare capacity is real storage holding real values, so an index inside it would read a
    // copy of whatever seeded the growth rather than failing — which is exactly why `at` checks
    // against the count and not against the slice it indexes.
    "including an index inside the spare capacity" in {
      panics(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |print(b.at(5))""".stripMargin,
        "past the 1 elements",
      )
    }

    "and writing past it likewise" in {
      panics(
        """var b: &Buf[int] = buf()
          |b.push(1)
          |b.set(2, 9)""".stripMargin,
        "past the 1 elements",
      )
    }
  }
}
