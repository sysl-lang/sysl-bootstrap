package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Promotion: what `reference/memory.md § What happens when a slice escapes` says to do about a
 * view that outlives the array it views.
 *
 * Every program here was a **compile error** until promotion was built, and the diagnostic told the
 * programmer to declare the storage as a `[]T` or a `&[N]T` themselves. Now the compiler does it:
 * the array is allocated as an ARC buffer instead of a stack slot, the view's owner points at that
 * buffer, and the storage lives exactly as long as the last view of it. Nothing else about the
 * program changes — the array keeps its `[N]T` type, so every index, store and copy is emitted as
 * it always was.
 *
 * So each case is checked twice over: that the program **runs and gives the right answer**, which is
 * what a dangling view would not, and that the storage really moved, which is what the diagnostic
 * used to stand in for. The second is worth asserting separately because a promotion that silently
 * did not happen would still compile — it would just read a frame that has gone.
 */
class EscapePromotionTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** An array that was promoted is allocated rather than laid out, and the views of it carry a real
   * owner. A frame-backed array's slice carries `null`, so the presence of an owner at all is the
   * observable difference.
   */
  private def promotes(src: String): Unit = {
    val out = ir(src)

    out should include("call ptr @malloc")
    out should include regex """%\w+\.addr = getelementptr"""
    out should include regex """%\w+\.box = alloca ptr"""
  }

  "a view that gets out is promoted when it" - {
    "is returned" in {
      val src =
        """view() -> []int
          |    var buf: [4]int
          |    buf[0usize] = 7
          |    buf[0..<2]
          |end view
          |var v = view()
          |print(v.len, v[0usize])
          |""".stripMargin

      run(src) shouldBe "2 7\n"
      promotes(src)
    }

    "is returned from a `return` in the middle" in {
      val src =
        """view(c: bool) -> []int
          |    var buf: [4]int
          |    buf[1usize] = 9
          |    if c then return buf[..]
          |    buf[0..<0]
          |end view
          |print(view(true).len, view(true)[1usize], view(false).len)
          |""".stripMargin

      run(src) shouldBe "4 9 0\n"
      promotes(src)
    }

    "is returned inside a struct" in {
      val src =
        """struct Header
          |    body: []u8
          |end Header
          |make() -> Header
          |    var buf: [4]u8
          |    buf[0usize] = 65u8
          |    Header(buf[..])
          |end make
          |var h = make()
          |print(h.body.len, str(char(h.body[0usize])))
          |""".stripMargin

      run(src) shouldBe "4 A\n"
      promotes(src)
    }

    "is returned by a function that only passed it along" in {
      val src =
        """pass(s: []int) -> []int = s
          |leak() -> []int
          |    var buf: [4]int
          |    buf[3usize] = 4
          |    pass(buf[..])
          |end leak
          |var v = leak()
          |print(v.len, v[3usize])
          |""".stripMargin

      run(src) shouldBe "4 4\n"
      promotes(src)
    }

    // A loop is an expression, so a `break` of a frame-backed slice out of a loop whose value is
    // returned carries it out exactly as a `return` would — and promotes for the same reason.
    "is broken out of a loop that is returned" in {
      val src =
        """leak() -> []int
          |    var buf: [4]int
          |    buf[2usize] = 5
          |    for i in 0..<4
          |        if i == 2 then break buf[..]
          |    else buf[0..<0]
          |end leak
          |var v = leak()
          |print(v.len, v[2usize])
          |""".stripMargin

      run(src) shouldBe "4 5\n"
      promotes(src)
    }

    // A view of a view still roots at the array, so what moves is the array and both views count
    // against the one buffer.
    "is a slice of a slice of a frame-local array" in {
      val src =
        """view() -> []int
          |    var buf: [8]int
          |    buf[2usize] = 3
          |    var s = buf[..]
          |    s[1..<4]
          |end view
          |var v = view()
          |print(v.len, v[1usize])
          |""".stripMargin

      run(src) shouldBe "3 3\n"
      promotes(src)
    }

    "reaches a local that is returned" in {
      val src =
        """leak() -> []int
          |    var buf: [4]int
          |    buf[0usize] = 1
          |    var s = buf[..]
          |    var t = s
          |    t
          |end leak
          |print(leak()[0usize])
          |""".stripMargin

      run(src) shouldBe "1\n"
      promotes(src)
    }

    "goes on the heap" in {
      val src =
        """struct Held
          |    body: []int
          |end Held
          |stash() -> &Held
          |    var buf: [4]int
          |    buf[1usize] = 6
          |    var h: &Held = Held(buf[..])
          |    h
          |end stash
          |var h = stash()
          |print(h.body.len, h.body[1usize])
          |""".stripMargin

      run(src) shouldBe "4 6\n"
      promotes(src)
    }

    "is handed to a callee that holds on to it" in {
      val src =
        """stash(dest: *[]int, s: []int)
          |    *dest = s
          |grab() -> int
          |    var buf: [4]int
          |    buf[0usize] = 8
          |    var out: []int
          |    stash(&out, buf[..])
          |    out[0usize]
          |end grab
          |print(grab())
          |""".stripMargin

      run(src) shouldBe "8\n"
      promotes(src)
    }

    // The array is a local whichever pointers happen to be in scope beside it, which is what this
    // checked before promotion existed and still checks now — the answer just changed from a
    // diagnostic to a buffer.
    "is a local array declared beside a pointer parameter" in {
      val src =
        """struct Box
          |    a: [8]u8
          |end Box
          |view(b: *Box) -> []u8
          |    var mine: [4]u8
          |    mine[0usize] = 66u8
          |    mine[..]
          |end view
          |var b = Box([65u8; 8])
          |var v = view(&b)
          |print(v.len, str(char(v[0usize])))
          |""".stripMargin

      run(src) shouldBe "4 B\n"
      promotes(src)
    }

    // An element of a local array of arrays is part of that array's storage, so the whole thing
    // moves and the element goes with it. A *field* of a struct does not — see the error suite.
    "is an element of a local array of arrays" in {
      val src =
        """view(i: usize) -> []u8
          |    var grid: [2][4]u8
          |    grid[1usize][0usize] = 67u8
          |    grid[i][..]
          |end view
          |var v = view(1usize)
          |print(v.len, str(char(v[0usize])))
          |""".stripMargin

      run(src) shouldBe "4 C\n"
      promotes(src)
    }

    // `05` assumes a body it cannot see keeps whatever it is handed, so an extern is the
    // pessimistic case — and the answer to it is now promotion rather than refusal.
    "is handed to an extern, whose body nothing can see" in {
      promotes("""extern take(s: []u8)
                 |use()
                 |    var buf: [4]u8
                 |    take(buf[0..<2])
                 |end use
                 |use()
                 |""".stripMargin)
    }

    // The tail of a variadic call is passed alongside the declared parameters rather than instead
    // of them, so the declared one is still analyzed — which is now visible as a promotion.
    "is the declared parameter of a variadic extern" in {
      promotes("""extern take(s: []u8, ...)
                 |use()
                 |    var buf: [4]u8
                 |    take(buf[0..<2], 1)
                 |end use
                 |use()
                 |""".stripMargin)
    }

    "is erased into a trait object" in {
      promotes("""trait Sink
                 |    put(self, s: []u8)
                 |struct Keep
                 |    tag: int
                 |impl Sink for Keep
                 |    put(self, s: []u8)
                 |        print(s.len)
                 |use()
                 |    var buf: [4]u8
                 |    var k: &Sink = Keep(1)
                 |    k.put(buf[..])
                 |end use
                 |use()
                 |""".stripMargin)
    }

    // `Writer` is the earned exception and `Reader` is not one, so reading into a local array through
    // an erased reader promotes it. `05` records why the two directions differ and leaves the
    // question open; this is what the answer is today.
    "is read into through an erased reader, where a written one would not have been" in {
      promotes("""import sysl.io.*
                 |use(r: *Reader) -> usize
                 |    var room: [64]u8
                 |    r.read(room[..]).len
                 |end use
                 |var s = stdin()
                 |print(use(&s))
                 |""".stripMargin)
    }
  }

  /** The counts are what make promotion correct rather than merely compiling, and they are not
   * visible in one run: a buffer released once too often is a crash and one released too seldom is
   * a leak, and a single allocation shows neither. So the shape is exercised enough times that a
   * double free would have to fire.
   */
  "the buffer's count is right, over enough traffic to show it" in {
    run("""tag(n: int) -> []u8
          |    var buf: [16]u8
          |    buf[0usize] = u8(n % 26 + 65)
          |    buf[0..<1]
          |end tag
          |var total = 0
          |for i in 0..<200000
          |    var t = tag(i)
          |    total += int(t[0usize])
          |print(total)
          |""".stripMargin) shouldBe "15499928\n"
  }

  // An array nothing takes a view of has no reason to move, and one whose views stay in the frame
  // has none either. Promotion is for arrays that are **both** sliced and escaped (`05`), and an
  // allocation appearing under either of these would be the compiler allocating for nothing.
  "an array that does not need moving keeps its frame slot" - {
    "one that is only read" in {
      ir("""use() -> int
           |    var buf: [4]int
           |    buf[0usize] = 1
           |    buf[0usize]
           |end use
           |print(use())
           |""".stripMargin) should not include "call ptr @malloc"
    }

    // `Reader` is not `Writer`: a concrete reader is a direct call, so the summary applies and
    // `FdReader.read` keeps nothing — the buffer stays where it was declared.
    "and one read into through a reader whose body is known" in {
      ir("""import sysl.io.*
           |fill() -> usize
           |    var r = stdin()
           |    var room: [64]u8
           |    r.read(room[..]).len
           |end fill
           |print(fill())
           |""".stripMargin) should not include "call ptr @malloc"
    }

    // Nor does a *generic* one, which is the fact that decides how much the erased case matters:
    // monomorphization turns the call into a direct one, so the summary applies again and the
    // buffer stays where it was declared. Reading through `[R: Reader]` rather than through
    // `*Reader` is therefore both the faster shape and the one with no promotion in it.
    "and one read into through a reader a type parameter stands for" in {
      ir("""import sysl.io.*
           |fill[R: Reader](r: R) -> usize
           |    var room: [64]u8
           |    var it = r
           |    it.read(room[..]).len
           |end fill
           |print(fill(stdin()))
           |""".stripMargin) should not include "call ptr @malloc"
    }

    "and one whose view never leaves" in {
      ir("""sum(s: []int) -> int
           |    var t = 0
           |    for i in 0usize..<s.len do t += s[i]
           |    t
           |end sum
           |use() -> int
           |    var buf: [4]int
           |    sum(buf[..])
           |end use
           |print(use())
           |""".stripMargin) should not include "call ptr @malloc"
    }
  }

  /** `reference/memory.md § Promotion is silent, not hidden`. Silent promotion earns the obvious
   * objection — an allocation appears that nothing in the source asked for — and the answer is
   * discoverability rather than ceremony: the common case costs no reading, and "why did this
   * allocate?" always has an answer. This is Go's `-m`, reached with `--explain-escapes`.
   */
  "every promotion can be asked about" - {
    def explain(src: String): List[String] =
      Compiler.compiled(List(Source("t.sysl", src))) match
        case Right(built) => built.notes
        case Left(err)    => fail(s"did not compile:\n$err")

    "the array is named, with the view that forced it" in {
      val notes = explain("""first() -> []u8
                            |    var buf: [8]u8
                            |    buf[0..<3]
                            |end first
                            |print(first().len)
                            |""".stripMargin)

      notes should have length 1
      notes.head should include("'buf' is promoted to the heap, because this view of it is returned")
      notes.head should startWith("t.sysl:3:8")
    }

    // A string outlives every frame, so bytes made into one in place have left the frame where
    // they became one — even though the string itself is never returned or stored anywhere.
    "a view made into a string in place moves the array, and says why" in {
      val notes = explain("""first() -> usize
                            |    var buf: [8]u8
                            |    val s = str_alias(buf[0..<3])
                            |    s.len
                            |end first
                            |print(first())
                            |""".stripMargin)

      notes should have length 1
      notes.head should include("'buf' is promoted to the heap, because this view of it is made into a string")
    }

    "one line per array, in source order" in {
      val notes = explain("""first() -> []u8
                            |    var buf: [8]u8
                            |    buf[0..<3]
                            |end first
                            |second() -> []int
                            |    var nums: [4]int
                            |    nums[..]
                            |end second
                            |print(first().len + second().len)
                            |""".stripMargin)

      notes should have length 2
      notes(0) should include("'buf'")
      notes(1) should include("'nums'")
    }

    // An array is named once however many views of it get out, because the question the report
    // answers is "why did this allocate", and it allocated once.
    "an array that escapes twice is reported once" in {
      val notes = explain("""both(c: bool) -> []int
                            |    var buf: [4]int
                            |    if c then return buf[0..<1]
                            |    buf[..]
                            |end both
                            |print(both(true).len)
                            |""".stripMargin)

      notes should have length 1
      notes.head should include("'buf'")
    }

    "and a program that promotes nothing has nothing to say" in {
      explain("""use() -> int
                |    var buf: [4]int
                |    buf[0usize]
                |end use
                |print(use())
                |""".stripMargin) shouldBe empty
    }

    // The reason is the *route* out, not a fixed phrase, so a program that escapes a different way
    // is told which way.
    "the route out is named" in {
      val notes = explain("""stash(dest: *[]int, s: []int)
                            |    *dest = s
                            |grab() -> int
                            |    var buf: [4]int
                            |    var out: []int
                            |    stash(&out, buf[..])
                            |    out[0usize]
                            |end grab
                            |print(grab())
                            |""".stripMargin)

      notes.head should include("is passed to 'stash', which holds on to it")
    }
  }

  /** The case that needs **neither** promotion nor a refusal, and was getting the refusal.
   *
   * Promotion is for storage that dies with the frame. Module storage does not: it is laid down once
   * for the whole run, so a view of it is not a view of any frame and may leave one freely. The
   * analysis reached that conclusion for a local (promote it) and for a caller's array (refuse), and
   * fell through to the refusal for a module `val` — with a sentence offering two reasons, "a field
   * of a value" and "an array a caller passed by value", neither of which is what a `static val` is.
   *
   * A table declared once and viewed from a function is the ordinary shape of it, and the one that
   * found this: `sysl.time.tzif`'s tests carry a zone as a module-level array of bytes and hand a
   * decoded view of it back out of a helper.
   */
  "a view of module storage leaves the frame without promotion or refusal" - {
    "because the storage outlives every frame there is" in {
      run("""static val table: [4]u8 = [1, 2, 3, 4]
            |
            |head() -> []const u8 = table[0..<2]
            |
            |print(head().len, head()[1])
            |""".stripMargin) shouldBe "2 2\n"
    }

    // A module `var` is the same storage with writing allowed, so it answers the same way — which is
    // worth pinning separately, since `TVal.writable` is the only thing that distinguishes the two
    // and nothing about the lifetime differs.
    "and a module 'var' is the same storage, so it answers alike" in {
      run("""static var table: [4]u8 = [1, 2, 3, 4]
            |
            |head() -> []const u8 = table[0..<2]
            |
            |table[0usize] = 9
            |print(head()[0], head().len)
            |""".stripMargin) shouldBe "9 2\n"
    }
  }
}
