package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A large by-value parameter — a `self` of kilobytes, most of all — is read where the caller put it,
 * and copied only where a copy could be told apart from the original.
 *
 * `reference/declarations.md`'s receiver table says `self` hands a method a copy, and that is a
 * promise about what the method can **observe**: nothing done to the caller's value while it runs
 * reaches it, and nothing it does to its own reaches the caller. A copy of bytes at every entry was
 * one way to keep it, and on a board it was the stack: a `*self` method calling a `self` one paid a
 * staged copy, a whole-aggregate load to hand `arc.dispose` its value, and the callee's own copy at
 * entry — three times the struct, for a call that only read two fields.
 *
 * What keeps the promise now is split along the call, and each half is tested here:
 *
 *   - **the caller** hands over storage nothing can change for the length of the call — a temporary,
 *     a local nobody else can name, or a snapshot it stages — and needs no snapshot at all where the
 *     callee can write nothing (`BorrowedParams.inert`), which is now asked over the whole program
 *     rather than of a leaf;
 *   - **the callee** reads that storage in place, and keeps a copy of its own only where it changes
 *     the parameter, jumps to its own entry, or may be called from outside (`Codegen.readsInPlace`);
 *   - **a staged snapshot is given back at its address**, not loaded as one aggregate value.
 *
 * The shapes are asserted in the emitted text, and the promise itself by running: a write through an
 * alias in the middle of the call must not show through the receiver.
 */
class ReceiverInPlaceTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** One function's emitted text, from its `define` to the brace that closes it. */
  private def body(out: String, name: String): String = {
    val head = raw"(?m)^define [^@]*@[^(]*\Q$name\E\(".r

    head.findFirstMatchIn(out) match {
      case None    => fail(s"no definition of '$name' in:\n$out")
      case Some(m) =>
        val end = out.indexOf("\n}\n", m.start)

        out.substring(m.start, if end < 0 then out.length else end)
    }
  }

  /** The musicbox shape: a `*self` method asking a `self` one, which asks another. */
  private val big =
    """struct Big
      |    notes: []const u32
      |    n: usize
      |    table: [1024]u32
      |
      |    busy(self) -> usize
      |        var c: usize = 0
      |        for k in 0..<1024
      |            if self.table[k] != 0 then c += 1
      |        c
      |
      |    more(self) -> bool = self.n < self.notes.len || self.busy() > 0
      |
      |    step(*self) -> bool
      |        self.n += 1
      |        self.more()
      |
      |var b = Big([1, 2, 3], 0, [0; 1024])
      |print(b.step())
      |""".stripMargin

  "a self method that only reads its receiver makes no copy of it" in {
    val out = ir(big)

    for name <- List("Big.more", "Big.busy") do
      val f = body(out, name)

      f should not include "llvm.memcpy"
      f should not include "alloca %struct.Big"
  }

  "a *self method asking a self method that can write nothing hands over its own receiver" in {
    val step = body(ir(big), "Big.step")

    step should not include "llvm.memcpy"
    step should not include "alloca %struct.Big"
    step should not include "arc.copy_at"
    step should not include "arc.dispose"
  }

  "and the program still answers what it did" in {
    run(big) shouldBe "true\n"
  }

  // `more` prints, so it is not inert and the receiver — a place behind `*self` — is staged. What is
  // asserted is how the staged copy is given back: at its address, never as a 4 KB value.
  "a staged receiver is given back at its address" in {
    val src = big.replace("more(self) -> bool = self.n < self.notes.len || self.busy() > 0",
                          "more(self) -> bool\n        print(\"more\")\n        self.n < self.notes.len")
    val out = ir(src)
    val step = body(out, "Big.step")

    step should include("@arc.dispose_at.Big(ptr")
    step should not include "load %struct.Big"
    out should not include "@arc.dispose.Big(%struct.Big"
    run(src) shouldBe "more\ntrue\n"
  }

  "the receiver is still a copy wherever a copy can be seen" - {

    // `&b` lets the caller's storage out, so the receiver is a snapshot staged by the caller: the
    // write through `p` lands in `b` and not in what the method is reading.
    "a write through an alias in the middle of the call" in {
      val src =
        """struct Big
          |    n: int
          |    table: [1024]u32
          |
          |    snap(self, p: *Big) -> int
          |        val before = self.n
          |        p.n += 1
          |        before * 100 + self.n
          |
          |var b = Big(1, [0; 1024])
          |print(b.snap(&b))
          |print(b.n)
          |""".stripMargin

      run(src) shouldBe "101\n2\n"
    }

    "a write made by a function the method calls" in {
      val src =
        """struct Big
          |    n: int
          |    table: [1024]u32
          |
          |    look(self, p: *Big) -> int
          |        val before = self.n
          |        bump(p)
          |        before * 100 + self.n
          |
          |bump(p: *Big) = p.n += 1
          |
          |var b = Big(3, [0; 1024])
          |print(b.look(&b))
          |print(b.n)
          |""".stripMargin

      run(src) shouldBe "303\n4\n"
    }

    // A method writing its own receiver writes its copy, so it keeps one: the slot is an `alloca`
    // filled at entry, and the caller's value is untouched afterwards.
    "a method that changes its own receiver" in {
      val src =
        """struct Big
          |    n: int
          |    table: [1024]u32
          |
          |    bumped(self) -> int
          |        self.n += 1
          |        self.n
          |
          |var b = Big(1, [0; 1024])
          |print(b.bumped())
          |print(b.n)
          |""".stripMargin

      body(ir(src), "Big.bumped") should include("llvm.memcpy")
      run(src) shouldBe "2\n1\n"
    }

    // Through a trait object the data word is the object itself, which the call may change; the
    // adapter stages the snapshot a direct caller would have.
    "a receiver reached through a trait object" in {
      val src =
        """trait Peek
          |    peek(self, p: *Big) -> int
          |
          |struct Big
          |    n: int
          |    table: [1024]u32
          |
          |impl Peek for Big
          |    peek(self, p: *Big) -> int
          |        val before = self.n
          |        p.n += 1
          |        before * 100 + self.n
          |
          |var b = Big(1, [0; 1024])
          |val o: *Peek = &b
          |print(o.peek(&b))
          |print(b.n)
          |""".stripMargin

      run(src) shouldBe "101\n2\n"
    }
  }

  "what a receiver refers to is counted exactly once" - {

    val node =
      """struct Node
        |    v: int
        |impl Drop for Node
        |    drop(self) = print("dropped", self.v)
        |
        |struct Big
        |    r: &Node
        |    table: [1024]u32
        |
        |    read(self) -> int = self.r.v + first(self)
        |
        |    swap(self, p: *Big) -> int
        |        p.r = Node(9)
        |        self.r.v
        |
        |first(b: Big) -> int = int(b.table[0])
        |""".stripMargin

    // Every node built is dropped once, whether the receiver was read in place or staged.
    "a counted receiver read in a loop, many times over" in {
      val src = node +
        """for i in 0..<3
          |    var b = Big(Node(i), [0; 1024])
          |    var sum = 0
          |    for _ in 0..<100 do sum += b.read()
          |    print(sum)
          |""".stripMargin

      run(src) shouldBe "0\ndropped 0\n100\ndropped 1\n200\ndropped 2\n"
    }

    // The write frees the node the caller's `b.r` held, and the snapshot's own count is what keeps
    // the receiver's copy of it readable for the rest of the call.
    "a receiver whose field the call replaces" in {
      val src = node +
        """var b = Big(Node(1), [0; 1024])
          |print(b.swap(&b))
          |print(b.r.v)
          |""".stripMargin

      run(src) shouldBe "1\n9\ndropped 1\ndropped 9\n"
    }

    // A writable view of an array inside the receiver writes the receiver, though no place in the
    // write is rooted at `self`: the write is `v[0]`. Read in place, it would land in the caller's
    // `b`, which nothing else names, so the caller hands `b` over unstaged.
    "a write through a view of an array inside it" in {
      val src =
        """struct Big
          |    n: int
          |    table: [256]u32
          |
          |    scribble(self) -> u32
          |        var v = self.table[..]
          |        v[0] = 9
          |        self.table[0]
          |
          |var b = Big(1, [0; 256])
          |print(b.scribble(), b.table[0])
          |""".stripMargin

      val f = body(ir(src), "Big.scribble")

      f should include("alloca %struct.Big")
      f should include("llvm.memcpy")
      run(src) shouldBe "9 0\n"
    }

    // The neighbours of that one: a `ref` to an element, a view of an array one struct further in,
    // and a view taken through the address of the field.
    "a write through a ref to an element, a nested view, or a view through the field's address" in {
      val src =
        """struct Inner
          |    k: int
          |    arr: [256]u32
          |
          |struct Big
          |    n: int
          |    inner: Inner
          |    table: [256]u32
          |
          |    by_ref(self) -> u32
          |        ref r = self.table[1]
          |        r = 5
          |        self.table[1]
          |
          |    nested(self) -> u32
          |        var v = self.inner.arr[..]
          |        v[2] = 6
          |        self.inner.arr[2]
          |
          |    through_addr(self) -> u32
          |        var p = &self.table
          |        var v = p[..]
          |        v[3] = 8
          |        self.table[3]
          |
          |var b = Big(1, Inner(2, [0; 256]), [0; 256])
          |print(b.by_ref(), b.table[1])
          |print(b.nested(), b.inner.arr[2])
          |print(b.through_addr(), b.table[3])
          |""".stripMargin

      run(src) shouldBe "5 0\n6 0\n8 0\n"
    }

    // The caller's half: a writable view of `b` handed to the same call lets the callee write `b`
    // while it reads its by-value copy, so `b` is not a slot nobody else can name.
    "a view of the argument handed to the same call" in {
      val src =
        """struct Big
          |    n: int
          |    table: [256]u32
          |
          |poke(b: Big, out: []u32) -> u32
          |    print("not inert")
          |    out[0] = 7
          |    b.table[0]
          |
          |var b = Big(1, [0; 256])
          |print(poke(b, b.table[..]), b.table[0])
          |""".stripMargin

      run(src) shouldBe "not inert\n0 7\n"
    }
  }

  "a body that calls only functions that can write nothing can write nothing either" - {

    val holder =
      """struct Node
        |    v: int
        |struct Holder
        |    r: &Node
        |inner(h: Holder) -> int = h.r.v
        |""".stripMargin

    "and takes no count for what it is passed" in {
      val src = holder +
        """outer(h: Holder) -> int
          |    var t = 0
          |    t += inner(h)
          |    t
          |var n: &Node = Node(1)
          |print(outer(Holder(n)))""".stripMargin

      body(ir(src), "outer") should not include "@arc.copy.Holder(%struct.Holder %h.param)"
      run(src) shouldBe "1\n"
    }

    "however the calls recurse" in {
      val src = holder +
        """depth(h: Holder, k: int) -> int = if k == 0 then inner(h) else depth(h, k - 1) + 0
          |var n: &Node = Node(4)
          |print(depth(Holder(n), 3))""".stripMargin

      body(ir(src), "depth") should not include "@arc.copy.Holder(%struct.Holder %h.param)"
      run(src) shouldBe "4\n"
    }

    // A write to the parameter's own slot would give back the caller's count, so it is not a write
    // to the function's own storage and the count is still taken.
    "but not one that writes its parameter" in {
      val src = holder +
        """outer(h: Holder) -> int
          |    h = Holder(Node(2))
          |    inner(h)
          |var n: &Node = Node(1)
          |print(outer(Holder(n)))""".stripMargin

      body(ir(src), "outer") should include("@arc.copy.Holder(%struct.Holder %h.param)")
      run(src) shouldBe "2\n"
    }

    // A slice the body declared is its own local and its elements are not: a write through one is a
    // write to whoever owns them — here the very storage the caller lent as `b` — so the body is not
    // inert and the caller stages a snapshot.
    "but not one that writes through a slice it was handed" in {
      val src =
        """struct Big
          |    n: int
          |    table: [256]u32
          |
          |poke(b: Big, out: []u32) -> u32
          |    var o = out
          |    o[0] = 7
          |    b.table[0]
          |
          |caller(p: *Big) -> u32 = poke(*p, p.table[..])
          |
          |var b = Big(1, [0; 256])
          |print(caller(&b), b.table[0])
          |""".stripMargin

      body(ir(src), "caller") should include("@llvm.memcpy")
      run(src) shouldBe "0 7\n"
    }
  }
}
