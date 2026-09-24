package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-2 runtime behaviour of `Display` and its `Writer` sink
 * (`library/core.md § Rendering to a sink`).
 *
 * The claim under test is that rendering is now **one** mechanism: a scalar, a struct, an enum, and
 * a bounded type parameter all reach text the same way, and the difference between `print` and
 * `str` is only which sink the text lands in. So most cases here drive the same value through both,
 * and expect the two to agree to the byte.
 */
class DisplayRunTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** `Display`, `Writer` and the renderers are the standard module's, since `print` desugars onto
   * them. The one sink the library supplies is `sysl.buf`'s, being ordinary sysl over the growable
   * buffer, so the programs that gather into one ask for it.
   */
  private val importing = "import sysl.buf.*\n\n"

  override protected def run(src: String, optimize: String = Toolchain.defaultOptimization): String =
    super.run(importing + src, optimize)

  /** A pair whose rendering is unmistakable — punctuation the two fields could not have produced
   * on their own, so a call that reached the wrong renderer shows up as missing text rather than
   * as a plausible wrong number.
   */
  /** The two widths past what `snprintf` can take, each behind a struct that forwards to it — since
   * `print` and `str` on a scalar do not go through `Display` at all, and a test driving those
   * would pass whatever the wide renderers did.
   */
  private val wide =
    """struct W
      |    n: u128
      |impl Display for W
      |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
      |struct S
      |    n: i128
      |impl Display for S
      |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
      |""".stripMargin

  private val point =
    """struct Point
      |    x: int
      |    y: int
      |impl Display for Point
      |    display(self, out: *Writer, fmt: FormatSpec)
      |        display_str("(", out, fmt)
      |        self.x.display(out, fmt)
      |        display_str(", ", out, fmt)
      |        self.y.display(out, fmt)
      |        display_str(")", out, fmt)
      |""".stripMargin

  "a type that renders itself" - {
    "prints through the impl its type carries" in {
      run(point + "print(Point(3, 4))") shouldBe "(3, 4)\n"
    }

    // The same text through the other sink. If the two ever disagreed, one of them would be doing
    // its own rendering rather than reaching the one `display` the type declares.
    "makes the same string as it prints" in {
      run(point +
        """var p = Point(3, 4)
          |print(p)
          |print(str(p))""".stripMargin) shouldBe "(3, 4)\n(3, 4)\n"
    }

    "produces a real string, which joins and slices like any other" in {
      run(point +
        """var s = str(Point(1, 2)) + "!"
          |print(s.len, s[0..<3])""".stripMargin) shouldBe "7 (1,\n"
    }

    "is spliced into an interpolation" in {
      run(point + """print(s"a ${Point(1, 2)} b")""") shouldBe "a (1, 2) b\n"
    }

    "renders an enum" in {
      run("""enum Colour
            |    Red
            |    Green
            |impl Display for Colour
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        var name = self match
            |            Red -> "red"
            |            Green -> "green"
            |        display_str(name, out, fmt)
            |print(Green, str(Red))""".stripMargin) shouldBe "green red\n"
    }

    "renders a type whose fields render themselves" in {
      run("""struct Leaf
            |    n: int
            |struct Node
            |    a: Leaf
            |    b: Leaf
            |impl Display for Leaf
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
            |impl Display for Node
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        self.a.display(out, fmt)
            |        display_char('/', out, fmt)
            |        self.b.display(out, fmt)
            |print(Node(Leaf(1), Leaf(2)))""".stripMargin) shouldBe "1/2\n"
    }

    // A render inside a render: the inner one has a buffer of its own, so the two cannot be sharing
    // the stack slot the sink lives in.
    "renders through a string it built on the way" in {
      run("""struct Leaf
            |    n: int
            |struct Node
            |    a: Leaf
            |impl Display for Leaf
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
            |impl Display for Node
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        display_str("<" + str(self.a) + ">", out, fmt)
            |print(str(Node(Leaf(5))))""".stripMargin) shouldBe "<5>\n"
    }

    "writes nothing at all when its impl writes nothing" in {
      run("""struct E
            |    n: int
            |impl Display for E
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        if self.n > 0 then display_str("x", out, fmt)
            |var s = str(E(0))
            |print(s.len, "[" + s + "]")""".stripMargin) shouldBe "0 []\n"
    }

    // Far past the buffer's starting capacity, so growth has run several times and every byte has
    // been carried across at least one reallocation.
    "renders text far larger than the buffer starts at" in {
      run("""struct Rep
            |    n: int
            |impl Display for Rep
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        var i = 0
            |        while i < 1000
            |            display_str("ab", out, fmt)
            |            i++
            |var s = str(Rep(0))
            |print(s.len, s[0..<4], s[1996..<2000])""".stripMargin) shouldBe "2000 abab abab\n"
    }
  }

  "the built-ins render through the same trait" - {
    // Every scalar `reference/expressions.md § Operator dispatch` calls `Display`, reached as a
    // method rather than through `print` — which is the call a struct's own `display` makes about
    // its fields.
    "every scalar type has a display of its own" in {
      run("""struct All
            |    n: int
            |impl Display for All
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        (1).display(out, fmt)
            |        display_char(' ', out, fmt)
            |        (2u8).display(out, fmt)
            |        display_char(' ', out, fmt)
            |        (2.5).display(out, fmt)
            |        display_char(' ', out, fmt)
            |        true.display(out, fmt)
            |        display_char(' ', out, fmt)
            |        'é'.display(out, fmt)
            |        display_char(' ', out, fmt)
            |        "s".display(out, fmt)
            |print(All(0))""".stripMargin) shouldBe "1 2 2.5 true é s\n"
    }

    "a scalar prints the same whether it goes direct or through the sink" in {
      run("""struct W
            |    n: int
            |impl Display for W
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
            |print(-2147483648)
            |print(W(-2147483648))""".stripMargin) shouldBe "-2147483648\n-2147483648\n"
    }
  }

  "a writer a program wrote itself" - {
    "receives the bytes of everything rendered into it" in {
      run("""struct Counter
            |    n: usize
            |impl Fallible for Counter
            |
            |impl Writer for Counter
            |    write(*self, bytes: []const u8)
            |        self.n += bytes.len
            |var c: Counter
            |var w: *Writer = &c
            |display_int(12345, w, FormatSpec(0, -1, false))
            |display_str("abc", w, FormatSpec(0, -1, false))
            |print(c.n)""".stripMargin) shouldBe "8\n"
    }

    // Most sinks cannot fail, and `Writer.failed` defaults to saying so — a writer that only
    // counts bytes writes nothing about failure at all.
    "may leave out 'failed', which the trait defaults to false" in {
      run("""struct Counter
            |    n: usize
            |impl Fallible for Counter
            |
            |impl Writer for Counter
            |    write(*self, bytes: []const u8)
            |        self.n += bytes.len
            |var c: Counter
            |var w: *Writer = &c
            |display_str("abc", w, FormatSpec(0, -1, false))
            |print(c.n, w.failed())""".stripMargin) shouldBe "3 false\n"
    }

    // The latch: a write that overruns sets a flag the *required* trait reports, and nothing in
    // between had to return or check anything. A sink that can fail is where the two blocks earn
    // their separation — `Fallible` carries the answer and `Writer` carries the writing.
    "latches a failure the writes themselves do not report" in {
      run("""struct Cap
            |    n: usize
            |    over: bool
            |impl Fallible for Cap
            |    override failed(*self) -> bool = self.over
            |
            |impl Writer for Cap
            |    write(*self, bytes: []const u8)
            |        self.n += bytes.len
            |        if self.n > 4usize then self.over = true
            |var c: Cap
            |var w: *Writer = &c
            |display_str("abc", w, FormatSpec(0, -1, false))
            |print(w.failed())
            |display_str("defg", w, FormatSpec(0, -1, false))
            |print(w.failed())""".stripMargin) shouldBe "false\ntrue\n"
    }

    "takes a value's own display, so a program can choose where its text goes" in {
      run(point +
        """struct Counter
          |    n: usize
          |impl Fallible for Counter
          |
          |impl Writer for Counter
          |    write(*self, bytes: []const u8)
          |        self.n += bytes.len
          |var c: Counter
          |var w: *Writer = &c
          |Point(10, 20).display(w, FormatSpec(0, -1, false))
          |print(c.n)""".stripMargin) shouldBe "8\n"
    }
  }

  /** `ByteSink` — the one writer the library does supply.
   *
   * It exists because a specifier describes the field the **whole** value occupies (`library/core.md § A specifier is the whole value's field`), so an
   * implementation rendering more than one part has to know what those parts came to before it can
   * pad once. Every such implementation was writing the same dozen lines to find out.
   *
   * **Gathering is one way to answer that and not the cheap one.** `Counting` runs the same writes
   * and keeps only their length, so a width costs a second pass and no storage — which is what
   * `[]T`, `Option`, `Result` and `Complex[F]` all do. What a sink is for is the case where the
   * *bytes* are wanted rather than the width: a rendering handed somewhere that is not a stream.
   *
   * It is ordinary sysl over `Buf[u8]`, which is why it could not be written until a growable array
   * could.
   */
  "the sink the library supplies" - {
    "keeps what is written into it" in {
      run("""var g = byte_sink()
            |var w: *Writer = &g
            |display_str("abc", w, FormatSpec(0, -1, false))
            |display_int(45, w, FormatSpec(0, -1, false))
            |print(g.text().len, str(g.text()[0]), str(g.text()[4]))""".stripMargin) shouldBe "5 97 53\n"
    }

    "reports no failure, having nothing to fail at" in {
      run("""var g = byte_sink()
            |var w: *Writer = &g
            |display_str("abc", w, FormatSpec(0, -1, false))
            |print(w.failed())""".stripMargin) shouldBe "false\n"
    }

    "starts with nothing and grows to whatever is written" in {
      run("""var g = byte_sink()
            |var w: *Writer = &g
            |print(g.text().len)
            |for i in 0..<200 do display_str("xy", w, FormatSpec(0, -1, false))
            |print(g.text().len, str(g.text()[399]))""".stripMargin) shouldBe "0\n400 121\n"
    }

    // The point of it: the three pieces are rendered plainly and the field applied to the whole,
    // so `%9s` pads once rather than three times.
    "lets an implementation put its whole rendering in one field" in {
      val pair =
        """struct Pair
          |    a: int
          |    b: int
          |impl Display for Pair
          |    display(self, out: *Writer, fmt: FormatSpec)
          |        var g = byte_sink()
          |        var plain = FormatSpec(0, -1, false)
          |        display_int(long(self.a), &g, plain)
          |        display_str(":", &g, plain)
          |        display_int(long(self.b), &g, plain)
          |        display_pad(g.text(), out, fmt)
          |    end display
          |""".stripMargin

      run(pair + """print(f"[${Pair(1, 2)}%9s]")
                   |print(f"[${Pair(1, 2)}%-9s]")
                   |print(f"[${Pair(1, 2)}%2s]")
                   |print(Pair(30, -4))""".stripMargin) shouldBe
        "[      1:2]\n[1:2      ]\n[1:2]\n30:-4\n"
    }

    // A sink written into by two implementations in the same expression keeps them apart, since
    // each is a value of its own with its own buffer.
    "two of them do not run into each other" in {
      run("""var a = byte_sink()
            |var b = byte_sink()
            |display_str("left", &a, FormatSpec(0, -1, false))
            |display_str("right", &b, FormatSpec(0, -1, false))
            |print(a.text().len, b.text().len)""".stripMargin) shouldBe "4 5\n"
    }
  }

  "a format specifier reaches the implementation" - {
    // The parts are handed the neutral specifier rather than the one that arrived: `%-12.3s`
    // describes the field this *value* occupies, so applying it to each piece would pad three times.
    "an f-string hole hands its width, precision, and justification over" in {
      run("""struct W
            |    n: int
            |impl Display for W
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        var plain = FormatSpec(0, -1, false)
            |        fmt.width.display(out, plain)
            |        display_char(' ', out, plain)
            |        fmt.prec.display(out, plain)
            |        display_char(' ', out, plain)
            |        fmt.left.display(out, plain)
            |print(f"${W(0)}%-12.3s")""".stripMargin) shouldBe "12 3 true\n"
    }

    // `print` and `str` write no specifier, so what a `Display` gets is the neutral one — and a
    // width of zero has to be distinguishable from a precision that was not written at all.
    "print and str hand over a neutral specifier" in {
      run("""struct W
            |    n: int
            |impl Display for W
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        fmt.width.display(out, fmt)
            |        display_char(' ', out, fmt)
            |        fmt.prec.display(out, fmt)
            |        display_char(' ', out, fmt)
            |        fmt.left.display(out, fmt)
            |print(W(0))
            |print(str(W(0)))""".stripMargin) shouldBe "0 -1 false\n0 -1 false\n"
    }

    // A wrapper whose whole rendering *is* one field's hands the specifier straight down, and the
    // width arrives where the text does.
    "is honoured by the renderer an implementation forwards it to" in {
      run("""struct P
            |    n: int
            |impl Display for P
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
            |print(f"[${P(7)}%10s]")""".stripMargin) shouldBe "[         7]\n"
    }

    // An implementation is free to drop it, and then nothing pads: the specifier is something a
    // `Display` is told, not something applied around it.
    "leaves an implementation that ignores it rendering plainly" in {
      run("""struct P
            |    n: int
            |impl Display for P
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, FormatSpec(0, -1, false))
            |print(f"[${P(7)}%10s]")""".stripMargin) shouldBe "[7]\n"
    }
  }

  "a bounded type parameter" - {
    // One body, three instantiations: two built-ins that render by the compiler's rule and a user
    // type that renders by its `impl`. If the bound were not doing the work, one of the three
    // would have had to be written differently.
    "renders whatever satisfies 'Display'" in {
      run("""struct P
            |    v: int
            |impl Display for P
            |    display(self, out: *Writer, fmt: FormatSpec) = self.v.display(out, fmt)
            |both[T: Display](a: T, b: T)
            |    print(a)
            |    print(str(b))
            |both(1, 2)
            |both("x", "y")
            |both(P(7), P(8))""".stripMargin) shouldBe "1\n2\nx\ny\n7\n8\n"
    }

    "carries the bound down into another generic it calls" in {
      run("""twice[T: Display](x: T)
            |    print(x)
            |    print(x)
            |once[T: Display](x: T) = twice(x)
            |once(5)""".stripMargin) shouldBe "5\n5\n"
    }
  }

  /** A slice of anything printable, which is one `impl[T: Display] Display for []T` rather than a
   * case per element type.
   *
   * **What it is written to avoid is a buffer.** Gathering the elements and padding the result would
   * be the obvious shape and it is the one thing this cannot do: `sysl` is the module `sysl.buf` is
   * built on, so a growable buffer is not reachable from where `Display` lives, and printing a slice
   * would start allocating in a language whose printing does not. So the elements go straight to the
   * writer as they are met, and a width — the one thing that needs a length before any byte is
   * written — is answered by rendering once into a sink that counts and keeps nothing.
   */
  "a slice renders its elements" - {
    "of a built-in" in {
      run("var a = [1, 2, 3]\nprint(a[..])") shouldBe "[1, 2, 3]\n"
    }

    "of strings, which are not scalars" in {
      run("var a = [\"a\", \"b\"]\nprint(a[..])") shouldBe "[a, b]\n"
    }

    // The bound is doing the work: `Rect` renders by its own `impl` and nothing about slices knows
    // that, so an element type the library never heard of goes through the same one block.
    "of a user type, through that type's own impl" in {
      run("""struct Rect
            |    w: int
            |    h: int
            |impl Display for Rect
            |    display(self, out: *Writer, fmt: FormatSpec) = display_str("a rect", out, fmt)
            |var r = [Rect(3, 4), Rect(1, 2)]
            |print(r[..])""".stripMargin) shouldBe "[a rect, a rect]\n"
    }

    // The impl satisfies its own bound, so a slice is an element type like any other.
    "of slices, which is the impl reaching itself" in {
      run("var n = [[1, 2][..], [3, 4][..]]\nprint(n[..])") shouldBe "[[1, 2], [3, 4]]\n"
    }

    "with no elements at all" in {
      run("var e: [0]int = []\nprint(e[..])") shouldBe "[]\n"
    }

    "and reached through an ordinary bound, not only through print" in {
      run("show[T: Display](x: T) = print(x)\nvar a = [1, 2, 3]\nshow(a[..])") shouldBe "[1, 2, 3]\n"
    }

    "and through str, which is the other sink" in {
      run("var a = [1, 2, 3]\nprint(str(a[..]) + \"!\")") shouldBe "[1, 2, 3]!\n"
    }

    /** `library/core.md § A specifier is the whole value's field`'s rule — a specifier describes the field the **whole** value occupies — which for a
     * slice is the part that cannot be done in one pass. The width is 14 and the rendering is 9
     * bytes, so five spaces land on whichever side the justification says; getting the count wrong
     * in either direction moves them.
     */
    "padded as one field, on the right" in {
      run("var a = [1, 2, 3]\nprint(f\"[${a[..]}%14s]\")") shouldBe "[     [1, 2, 3]]\n"
    }

    "and on the left" in {
      run("var a = [1, 2, 3]\nprint(f\"[${a[..]}%-14s]\")") shouldBe "[[1, 2, 3]     ]\n"
    }

    // The counting pass measures what the second pass writes, so an element whose rendering is not
    // its own length — a user type whose text is nothing like its fields — still pads correctly.
    "including where an element's text is unrelated to its value" in {
      run("""struct Rect
            |    w: int
            |    h: int
            |impl Display for Rect
            |    display(self, out: *Writer, fmt: FormatSpec) = display_str("a rect", out, fmt)
            |var r = [Rect(3, 4)]
            |print(f"[${r[..]}%12s]")""".stripMargin) shouldBe "[    [a rect]]\n"
    }

    /** A fixed array renders, and it renders **itself** rather than through a view the reader had
     * to know to take. Value generics (`reference/generics.md § A parameter may stand for a value`)
     * are what made that writable: a length became an argument to the array shape instead of part
     * of it, so one `impl[const N: usize, T: Display] Display for [N]T` covers every length.
     *
     * The two messages this replaces are worth recording because both were true when written and
     * both are gone. First `print(a)` said only that nothing rendered a `[3]int`, which sent a
     * reader to a wrapper struct. Then it said to take the whole-array view — one character rather
     * than nine lines, and a real improvement over the first. Now there is nothing to say.
     */
    "and a fixed-size array renders itself, at whatever length it is" in {
      run("var a = [1, 2, 3]\nprint(a)") shouldBe "[1, 2, 3]\n"
      run("var a: [2]int = [7, 8]\nprint(a)") shouldBe "[7, 8]\n"
    }

    // The view still renders, and renders the same, since the block for an array is a delegation to
    // the one for a slice rather than a second copy of it.
    "and the view of it renders the same" in {
      run("var a = [1, 2, 3]\nprint(a[..])") shouldBe "[1, 2, 3]\n"
    }

    /** An array whose *elements* do not render fails the covering block's **condition**, which is
     * now the same answer a slice of them gets — the element is named, because the element is the
     * part a reader can act on.
     */
    "while an array whose elements do not render names the element" in {
      err("""struct P
            |    v: int
            |var p: [1]P = [P(1)]
            |print(p)""".stripMargin) should
        include(s"the 'impl' that covers it asks '${lib("Display")}' of P, which does not implement it")
    }

    /** A nested one is answered one level at a time, exactly as a `[][]P` is: the block covering
     * `[2][1]P` asks `Display` of `[1]P`, and the block covering *that* is what asks it of `P`. The
     * reader is sent one step in rather than to the bottom, which is the same depth of answer every
     * composed type gets.
     */
    "and a nested one names the array its own block asks about" in {
      err("""struct P
            |    v: int
            |var p: [2][1]P = [[P(1)], [P(2)]]
            |print(p)""".stripMargin) should
        include(s"the 'impl' that covers it asks '${lib("Display")}' of [1]P, which does not implement it")
    }

    // And an array of a type that *does* render needs no block of its own, which is the whole point
    // of one library block covering every length.
    "and an array of a printable user type renders" in {
      run("""struct P
            |    v: int
            |impl Display for P
            |    display(self, out: *Writer, fmt: FormatSpec) = display_str("p", out, fmt)
            |var p: [1]P = [P(1)]
            |print(p)""".stripMargin) shouldBe "[p]\n"
    }

    // The element type is where the bound bites, and the message names the element rather than the
    // slice — which is the part a reader can act on.
    "and a slice of something unprintable names the element" in {
      err("""struct P
            |    v: int
            |var p = [P(1)]
            |print(p[..])""".stripMargin) should
        include(s"the 'impl' that covers it asks '${lib("Display")}' of P, which does not implement it")
    }

    /** The block that made this land: a program's own `override` beats the library's block for one
     * element type, which is the case `reference/traits.md § override — when the overlap is deliberate` was written for and the reason this feature
     * waited on it. Every `impl Display for []N` in the tree became an `override impl` the day the
     * library grew one for every slice.
     */
    "and a program may override it for a slice of its own type" in {
      run("""struct P
            |    v: int
            |override impl Display for []P
            |    display(self, out: *Writer, fmt: FormatSpec) = display_str("points", out, fmt)
            |var p = [P(1), P(2)]
            |print(p[..])""".stripMargin) shouldBe "points\n"
    }
  }

  /** A render owns the buffer it fills and the string it hands back, so the ordinary discipline has
   * to hold: a leak grows without bound and a double free crashes, and a long loop is what makes
   * either show up as something other than a passing test.
   */
  "ownership" - {
    "rendering in a loop neither leaks nor frees twice" in {
      run(point +
        """var i = 0
          |var total: usize = 0
          |while i < 200000
          |    total += str(Point(i, i)).len
          |    i++
          |print(total)""".stripMargin) shouldBe "2977780\n"
    }

    // The rendered value holds a count of its own, so the render has to leave it exactly where it
    // found it: the receiver is borrowed by the call, not consumed by it.
    "a value carrying a string is rendered without disturbing its count" in {
      run("""struct Tag
            |    name: string
            |    n: int
            |impl Display for Tag
            |    display(self, out: *Writer, fmt: FormatSpec)
            |        display_str(self.name, out, fmt)
            |        self.n.display(out, fmt)
            |var i = 0
            |var total: usize = 0
            |while i < 200000
            |    var t = Tag("tag" + "!", i)
            |    total += str(t).len
            |    total += str(t).len
            |    i++
            |print(total)""".stripMargin) shouldBe "3777780\n"
    }

    "printing in a loop reaches the impl every time" in {
      run("""struct T
            |    n: int
            |impl Display for T
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
            |var i = 0
            |while i < 3
            |    print(T(i))
            |    i++""".stripMargin) shouldBe "0\n1\n2\n"
    }
  }

  /** Integers past 64 bits, which reach renderers of their own.
   *
   * The 64-bit pair has its own digit loop in `sysl.render`, and C never had a conversion wider than
   * `%lld`, so these work the digits out against a frame-local buffer too — which is also what keeps them
   * inside `Display`'s allocation-free promise, pinned from the other side in `CapabilityClauseTests`.
   */
  "an integer wider than 64 bits" - {

    "renders every unsigned digit, to the full width of the type" in {
      val expected = "0\n7\n10\n340282366920938463463374607431768211455\n"

      run(wide +
        """print(W(u128(0)))
          |print(W(u128(7)))
          |print(W(u128(10)))
          |print(W(340282366920938463463374607431768211455))""".stripMargin).shouldBe(expected)
    }

    // The most negative value is the case a magnitude taken in the value's own type gets wrong:
    // negating it overflows, so the digits are worked out in the unsigned domain instead.
    "renders a signed one, including the value that cannot be negated" in {
      val expected =
        "0\n42\n-42\n170141183460469231731687303715884105727\n-170141183460469231731687303715884105728\n"

      run(wide +
        """print(S(i128(0)))
          |print(S(i128(42)))
          |print(S(i128(-42)))
          |print(S(170141183460469231731687303715884105727))
          |print(S(-170141183460469231731687303715884105728))""".stripMargin).shouldBe(expected)
    }

    // The padding is `display_digits`', not each renderer's, so a wide value fills a field exactly
    // as a narrow one does — sign inside the width, and justification either way.
    "fills a field the same way a narrow one does" in {
      run(wide +
        """print(f"[${W(u128(42))}%8s]")
          |print(f"[${S(i128(-42))}%8s]")
          |print(f"[${W(u128(42))}%-8s]")
          |print(f"[${W(u128(0))}%4s]")""".stripMargin).shouldBe("[      42]\n[     -42]\n[42      ]\n[   0]\n")
    }

    // Wider still keeps the path that renders through the digits `str` writes. Nothing here is a
    // width to widen to, so a buffer sized from the receiver would be what covered it, and a fixed
    // array's length cannot be written in terms of one.
    "beyond 128 bits it still renders, through the digits" in {
      run("""struct H
            |    n: u256
            |impl Display for H
            |    display(self, out: *Writer, fmt: FormatSpec) = self.n.display(out, fmt)
            |print(H(u256(12345)))
            |print(f"[${H(u256(7))}%4s]")""".stripMargin) shouldBe "12345\n[   7]\n"
    }
  }
}
