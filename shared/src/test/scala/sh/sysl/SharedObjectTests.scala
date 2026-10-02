package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `&sync T` — the reference whose count is atomic, and so the one that may be held in two
 * concurrency domains at once (`reference/memory.md § Crossing a concurrency domain`).
 *
 * The type has always parsed and always lowered to the atomic pair. What is checked here is the
 * condition the chapter attaches to it: an atomic count on the object promises nothing about the
 * counts the object *reaches*, so a `&sync T` is sound only when every one of them is atomic too.
 * That question is asked wherever a `&sync T` is written, and — for the one shape whose contents
 * are not known there — wherever a value is erased into a `&sync Trait`.
 */
class SharedObjectTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  private val inner = "struct Inner\n    v: int\n"
  private val show  = "trait Show\n    show(&self) -> string\n"

  "an object reached from two domains may hold" - {

    "scalars, a char, and a bool" in {
      run("""struct Reading
            |    n: i64
            |    scale: real
            |    unit: char
            |    ok: bool
            |var r: &sync Reading = Reading(7, 0.5, 'm', true)
            |print(r.n, r.scale, r.unit, r.ok)
            |""".stripMargin) shouldBe "7 0.5 m true\n"
    }

    // `unit` is the fourth name on `§ Crossing copies`' list and the one nothing had asked about.
    // It occupies no storage, so there is nothing for a domain to race over.
    "a 'unit', which the crossable list names beside the three above" in {
      run("""struct Marked
            |    n: int
            |    tag: unit
            |var m: &sync Marked = Marked(7, ())
            |print(m.n)
            |""".stripMargin) shouldBe "7\n"
    }

    "a fixed array of them" in {
      run("""struct Table
            |    cells: [4]int
            |var t: &sync Table = Table([1, 2, 3, 4])
            |print(t.cells[0] + t.cells[3])
            |""".stripMargin) shouldBe "5\n"
    }

    "another object held atomically, including itself" in {
      run("""struct Node
            |    v: int
            |    peer: Option[&sync Node]
            |var a: &sync Node = Node(1, None)
            |var b: &sync Node = Node(2, Some(a))
            |print(b.peer.unwrap().v)
            |""".stripMargin) shouldBe "1\n"
    }

    // The unsafe tier the chapter names as the *other* greppable way to share: a `*T` carries no
    // count, so there is none to make atomic and nothing here to say about it.
    "a raw pointer, which carries no count at all" in {
      run("""struct Reg
            |    slot: *int
            |    width: usize
            |var n = 41
            |var r: &sync Reg = Reg(&n, 4)
            |print(r.slot[0] + 1)
            |""".stripMargin) shouldBe "42\n"
    }

    "a data enum whose every payload is crossable" in {
      run("""enum Signal
            |    Idle
            |    Tick(n: int)
            |struct Port
            |    last: Signal
            |var p: &sync Port = Port(Signal.Tick(3))
            |var out = 0
            |p.last match
            |    Tick(n) -> out = n
            |    Idle -> out = 0
            |print(out)
            |""".stripMargin) shouldBe "3\n"
    }

    "a tuple of them" in {
      run("""struct Span
            |    ends: (int, int)
            |var s: &sync Span = Span((2, 5))
            |print(s.ends.1 - s.ends.0)
            |""".stripMargin) shouldBe "3\n"
    }

    "a constrained scalar, which is its base" in {
      run("""type Small = int within 0..9
            |struct Dial
            |    at: Small
            |var d: &sync Dial = Dial(4)
            |print(d.at)
            |""".stripMargin) shouldBe "4\n"
    }

    "a scalar directly, with no struct around it" in {
      run("""keep(n: &sync int) -> &sync int = n
            |var n: &sync int = 41
            |print(keep(n) == n)
            |""".stripMargin) shouldBe "true\n"
    }
  }

  "an object reached from two domains may not hold" - {

    "an ordinary reference, and the message says which spelling to reach for" in {
      val e = err(inner + "struct Holder\n    kid: &Inner\nvar h: &sync Holder = Holder(Inner(1))")

      e should include("'&sync Holder' may be reached from two domains at once")
      e should include("its 'kid' reaches a '&Inner', whose count is not")
      e should include("Hold it as a '&sync Inner'")
    }

    "a weak reference, which has no atomic form yet" in {
      err("""struct Node
            |    v: int
            |    peer: weak Node
            |var n: &sync Node = Node(1, None)
            |""".stripMargin) should
        include("its 'peer' reaches a 'weak Node', and a weak count has no atomic form yet")
    }

    // The chapter says an immortal string would be safe to share and a heap-backed one would not.
    // Nothing in a `string`'s type says which it is, so the rule as written cannot be applied and
    // the strict half is what ships — recorded in `06`, not worked around here.
    "a string, whichever kind of bytes it turns out to hold" in {
      val e = err("struct Reg\n    tag: string\nvar r: &sync Reg = Reg(\"dev0\")")

      e should include("its 'tag' reaches a 'string', which owns its bytes through a count that is not atomic")
      e should include("nothing in the type says whether these are")
    }

    "a slice, for the same reason" in {
      err("struct Win\n    cells: []int\nvar xs = [1, 2, 3]\nvar w: &sync Win = Win(xs[..])") should
        include("its 'cells' reaches a '[]int', which owns its elements")
    }

    "one inside a fixed array" in {
      err(inner + "struct Bad\n    kids: [2]&Inner\nvar b: &sync Bad = Bad([Inner(1), Inner(2)])") should
        include("its 'kids' reaches a '&Inner'")
    }

    "one inside a tuple" in {
      err(inner + "struct Bad\n    pair: (int, &Inner)\nvar b: &sync Bad = Bad((1, Inner(2)))") should
        include("its 'pair.1' reaches a '&Inner'")
    }

    "one inside an enum payload, wherever the variant sits" in {
      err(inner + """enum Slot
                    |    Empty
                    |    Full(p: &Inner)
                    |struct Bag
                    |    slot: Slot
                    |var b: &sync Bag = Bag(Slot.Empty)
                    |""".stripMargin) should include("its 'slot.Full(p)' reaches a '&Inner'")
    }

    // The path is what makes a deep one findable: naming only the type in the way would leave the
    // reader to search a struct they may not have written.
    "one several fields down, and the whole path is named" in {
      err(inner + """struct Mid
                    |    kid: &Inner
                    |struct Top
                    |    mid: Mid
                    |struct Root
                    |    top: Top
                    |var r: &sync Root = Root(Top(Mid(Inner(1))))
                    |""".stripMargin) should include("its 'top.mid.kid' reaches a '&Inner'")
    }

    "a string, when the string is the whole of what is shared" in {
      err("var s: &sync string = \"hi\"") should
        include("'&sync string' may be reached from two domains at once")
    }
  }

  "the question is asked wherever a '&sync' is written" - {

    "a declared local" in {
      err(inner + "struct H\n    kid: &Inner\nvar h: &sync H = H(Inner(1))") should
        include("has to be atomic")
    }

    "a parameter" in {
      err(inner + "struct H\n    kid: &Inner\nf(h: &sync H) -> int = h.kid.v") should
        include("has to be atomic")
    }

    "a result" in {
      err(inner + "struct H\n    kid: &Inner\nf(h: &H) -> &sync H = h") should
        include("has to be atomic")
    }

    "a field of another type" in {
      err(inner + "struct H\n    kid: &Inner\nstruct Reg\n    slot: &sync H") should
        include("has to be atomic")
    }

    "an element type" in {
      err(inner + "struct H\n    kid: &Inner\nstruct Reg\n    slots: [2]&sync H") should
        include("has to be atomic")
    }

    "a type argument" in {
      err(inner + """struct H
                    |    kid: &Inner
                    |struct Box[T]
                    |    it: T
                    |var b: Box[&sync H] = Box(H(Inner(1)))
                    |""".stripMargin) should include("has to be atomic")
    }

    // A generic type is one declaration covering every instantiation, so the question belongs to
    // the argument rather than to the parameter: `Box[int]` shares and `Box[&Inner]` does not.
    "the argument of a generic pointee, and not its parameter" in {
      run("""struct Box[T]
            |    it: T
            |var b: &sync Box[int] = Box(7)
            |print(b.it)
            |""".stripMargin) shouldBe "7\n"

      err(inner + """struct Box[T]
                    |    it: T
                    |var b: &sync Box[&Inner] = Box(Inner(1))
                    |""".stripMargin) should include("its 'it' reaches a '&Inner'")
    }

    // A member's receiver is a type like any other, and a `&sync self` on a type nothing may share
    // is a member no call could reach.
    "a '&sync self' receiver" in {
      err("struct P\n    tag: string\n    ping(&sync self) -> int = 1") should
        include("'&sync P' may be reached from two domains at once")
    }
  }

  "the question waits for an answer rather than being asked early" - {

    // A type reaching itself through a `&sync` field is resolved while its own field list is still
    // being filled, so asking then would find nothing in the way of anything. Both of these are
    // that shape, and the second is what proves the wait is not merely a wait for *this* type.
    "a type that reaches itself is still checked, and passes" in {
      run("""struct Ring
            |    v: int
            |    next: Option[&sync Ring]
            |var a: &sync Ring = Ring(1, None)
            |print(a.v)
            |""".stripMargin) shouldBe "1\n"
    }

    "a type that reaches itself and holds something unshareable is caught" in {
      err(inner + """struct Ring
                    |    kid: &Inner
                    |    next: Option[&sync Ring]
                    |var a: &sync Ring = Ring(Inner(1), None)
                    |""".stripMargin) should include("its 'kid' reaches a '&Inner'")
    }

    "two types that reach each other" in {
      err(inner + """struct A
                    |    b: Option[&sync B]
                    |struct B
                    |    kid: &Inner
                    |var a: &sync A = A(None)
                    |""".stripMargin) should include("'&sync B' may be reached from two domains at once")
    }

    // A type parameter promises nothing about its counts, so a declaration writing `&sync T` is
    // not a mistake — the instantiation is where the question has an answer, and it is asked there.
    "a declaration over a type parameter is not asked, and its instantiation is" in {
      run("""share[T](x: T) -> &sync T = x
            |var n = share(7)
            |print(n == n)
            |""".stripMargin) shouldBe "true\n"

      err(inner + """struct Box[T]
                    |    it: T
                    |share[T](x: Box[T]) -> &sync Box[T] = x
                    |var k: &Inner = Inner(1)
                    |var r = share(Box(k))
                    |""".stripMargin) should include("'&sync Box[&Inner]' may be reached from two domains at once")
    }

    // A trait's own signature is resolved where the trait is written (`reference/traits.md § A
    // default may assume exactly what its own trait declares`), so a promise nothing can keep is
    // refused there — with nothing implementing it, and again when something does.
    "a trait's signature is asked at the trait, implemented or not" in {
      val declared = inner + "struct H\n    kid: &Inner\ntrait Registry\n    put(self, h: &sync H) -> int\n"

      err(declared + "var n = 1\nprint(n)") should include("'&sync H' may be reached from two domains at once")

      err(declared + """struct R
                       |    n: int
                       |impl Registry for R
                       |    put(self, h: &sync H) -> int = 1
                       |""".stripMargin) should include("'&sync H' may be reached from two domains at once")
    }
  }

  "a pointee that is not a struct" - {

    "an array of references is named as something the object holds, not as what it is" in {
      err(inner + "var a: &sync [2]&Inner = [Inner(1), Inner(2)]") should
        include("'&sync [2]&Inner' may be reached from two domains at once, so every count inside " +
          "it has to be atomic — but it holds a '&Inner'")
    }

    "a shareable generic of the library's is ordinary" in {
      run("""var o: &sync Option[int] = Some(3)
            |print(o.unwrap())
            |""".stripMargin) shouldBe "3\n"
    }

    "and an unshareable one is not" in {
      err(inner + "var k: &Inner = Inner(1)\nvar o: &sync Option[&Inner] = Some(k)") should
        include("reaches a '&Inner'")
    }
  }

  "a trait object is asked where the type it forgot is known" - {

    "an implementor that may be shared is erased as usual" in {
      run(show + """struct P
                   |    v: int
                   |impl Show for P
                   |    show(&self) -> string = "p"
                   |var s: &sync Show = P(1)
                   |print(s.show())
                   |""".stripMargin) shouldBe "p\n"
    }

    // The type is not in the `&sync Show` that was written, so this is the one place the question
    // can be asked at all — and what it names is the concrete type, which is the thing in the way.
    "one that may not is refused at the value, not at the type" in {
      err(show + """struct P
                   |    tag: string
                   |impl Show for P
                   |    show(&self) -> string = self.tag
                   |var s: &sync Show = P("x")
                   |""".stripMargin) should include("'&sync P' may be reached from two domains at once")
    }
  }

  "a closure is asked about what it captured" - {

    "one capturing only crossable values may be shared" in {
      run("""var k = 5
            |var f: &sync Fn(int) -> int = x -> x + k
            |print(f(37))
            |""".stripMargin) shouldBe "42\n"
    }

    // A closure is a struct a program wrote and did not name, so nothing it is told may repeat the
    // name the compiler filed it under (`reference/types.md § Function types`).
    "one capturing a reference is refused, and the message says 'closure'" in {
      val e = err(inner + "var k: &Inner = Inner(3)\nvar f: &sync Fn(int) -> int = x -> x + k.v")

      e should include("a closure shared between two domains may be called from either")
      e should include("the 'k' it captures reaches a '&Inner'")
      e should not include "closure0"
    }
  }

  "the two reference modes" - {

    "do not convert in either direction, and the complaint says why" in {
      err(inner + "f(p: &Inner) -> int = p.v\nvar a: &sync Inner = Inner(1)\nprint(f(a))") should
        include("neither converts to the other")

      err(inner + "f(p: &sync Inner) -> int = p.v\nvar a: &Inner = Inner(1)\nprint(f(a))") should
        include("a count is atomic or it is not from the moment the object is allocated")
    }

    "are separately allocated, so a program may hold both" in {
      run(inner + """var a: &Inner = Inner(1)
                    |var b: &sync Inner = Inner(2)
                    |print(a.v + b.v)
                    |""".stripMargin) shouldBe "3\n"
    }
  }

  "the atomic count itself" - {

    // The orderings are the standard sequence and are written down because getting one wrong
    // produces a bug nobody finds (`library/sync.md § Ordering`).
    "takes a share with no ordering and gives one back with release, acquiring before it frees" in {
      val out = ir(inner + "var a: &sync Inner = Inner(1)\nvar b = a\nprint(b.v)")

      out should include("atomicrmw add ptr %p, i64 1 monotonic")
      out should include("atomicrmw sub ptr %p, i64 1 release")
      out should include("fence acquire")
    }

    "is emitted only into a program that has one" in {
      ir(inner + "var a: &Inner = Inner(1)\nprint(a.v)") should not include "arc.retain_sync"
    }

    "neither leaks nor frees twice over a long run" in {
      run(inner + """var i = 0
                    |var last = 0
                    |while i < 200000 do
                    |    var a: &sync Inner = Inner(i)
                    |    var b = a
                    |    last = b.v
                    |    i += 1
                    |print(last)
                    |""".stripMargin) shouldBe "199999\n"
    }
  }

  "what the chapter says the language does not have" - {

    // No `async`, no `await`, and no task runtime — so none of the three is a reserved word, and a
    // program may use all of them as names (`library/threads.md § There is no async`).
    "no async, await, or actor, so all three are ordinary names" in {
      run("var async = 1\nvar await = 2\nvar actor = 3\nprint(async + await + actor)") shouldBe "6\n"
    }

    // "`&sync T` makes the reference safe to share, not the object safe to mutate" — the chapter's
    // own most important sentence. A field of one is written exactly as any other field is.
    "an atomic reference does not make its object's fields any less writable" in {
      run("""struct Counter
            |    n: int
            |var a: &sync Counter = Counter(1)
            |var b = a
            |b.n = 41
            |print(a.n + 1)
            |""".stripMargin) shouldBe "42\n"
    }

    // A view records nothing about whether its owner's count is atomic (`07`), which is the same
    // gap the string rule above runs into, one level out.
    "a '&sync' array still cannot be sliced" in {
      err("var b: &sync [4]int = [1, 2, 3, 4]\nvar s = b[..]") should
        include("'&sync' array cannot be sliced")
    }

    "and 'weak sync T' still names the chapter it is waiting for" in {
      err("struct Node\n    v: int\nvar w: weak sync Node = None") should
        include("wants the concurrency model of '06'")
    }
  }

  // An array FIELD of a `&sync` box is the same storage one hop in: a view of it holds its share of
  // the box, and takes and gives back that share with an ordinary load-add-store, racing every
  // other domain that holds the box. Found by TSan on an audio ring (`r.samples[a..<b]`), where the
  // emitted IR read `@arc.retain_maybe` on a box every other count of which was `atomicrmw`.
  "a view of storage inside a '&sync' box is refused, whatever the path to it" - {

    val ring =
      """struct Inner
        |    a: [4]u8
        |struct Ring
        |    samples: [8]i16
        |    inner: Inner
        |total(xs: []const i16) -> int
        |    var s = 0
        |    for x in xs
        |        s += int(x)
        |    s
        |val r: &sync Ring = Ring([1, 2, 3, 4, 5, 6, 7, 8], Inner([9, 10, 11, 12]))
        |""".stripMargin

    // The line every refusal below is on: the one after the prelude.
    val at = ring.linesIterator.length + 1

    val refusal =
      "a slice does not record whether its owner's count is atomic, so storage inside a " +
        "'&sync Ring' cannot be sliced — a view of it would take its share of the box with a count " +
        "update that is not atomic. Read it by index, or walk it with 'for'"

    "a range of an array field" in {
      val e = err(ring + "val v = r.samples[2..<5]\nprint(v.len)\n")
      e should include(refusal)
      e should include(s"<input>:$at:18")
    }

    "the whole of an array field" in {
      val e = err(ring + "val v = r.samples[..]\nprint(v.len)\n")
      e should include(refusal)
      e should include(s"<input>:$at:18")
    }

    "an array inside a by-value field of the box" in {
      val e = err(ring + "val v = r.inner.a[1..<3]\nprint(v.len)\n")
      e should include(refusal)
      e should include(s"<input>:$at:18")
    }

    "an array field handed to a parameter that takes a view" in {
      val e = err(ring + "print(total(r.samples))\n")
      e should include(refusal)
      e should include(s"<input>:$at:15")
    }

    // The innermost box is the one the view would count against, and so the one named.
    "an array inside a '&sync' box reached through another" in {
      val e = err(
        """struct Cell
          |    a: [4]u8
          |struct Outer
          |    c: &sync Cell
          |val o: &sync Outer = Outer(Cell([1, 2, 3, 4]))
          |val v = o.c.a[..]
          |print(v.len)
          |""".stripMargin)
      e should include("storage inside a '&sync Cell' cannot be sliced")
      e should include("<input>:6:14")
    }
  }

  "and what the refusal points to instead still works" - {

    val ring =
      """struct Ring
        |    samples: [8]i16
        |total(xs: []const i16) -> int
        |    var s = 0
        |    for x in xs
        |        s += int(x)
        |    s
        |""".stripMargin

    "walking an array field of a '&sync' box with 'for'" in {
      run(ring + """val r: &sync Ring = Ring([1, 2, 3, 4, 5, 6, 7, 8])
                   |var s = 0
                   |for x in r.samples
                   |    s += int(x)
                   |print(s)
                   |""".stripMargin) shouldBe "36\n"
    }

    "reading and writing it by index" in {
      run(ring + """val r: &sync Ring = Ring([1, 2, 3, 4, 5, 6, 7, 8])
                   |r.samples[3] = 40
                   |print(r.samples[3] + r.samples[7])
                   |""".stripMargin) shouldBe "48\n"
    }

    "a view of the same field through an ordinary '&' box" in {
      run(ring + """val r: &Ring = Ring([1, 2, 3, 4, 5, 6, 7, 8])
                   |print(total(r.samples[2..<5]), total(r.samples))
                   |""".stripMargin) shouldBe "12 36\n"
    }

    "a view of a copy taken out of the '&sync' box" in {
      run(ring + """val r: &sync Ring = Ring([1, 2, 3, 4, 5, 6, 7, 8])
                   |val mine = r.samples
                   |print(total(mine[2..<5]))
                   |""".stripMargin) shouldBe "12\n"
    }
  }
}
