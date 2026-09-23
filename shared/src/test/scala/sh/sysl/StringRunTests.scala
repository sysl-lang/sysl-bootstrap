package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-2: what a `string` does as a program runs. A string is an immutable validated `[]u8`,
 * so what is worth checking is the part a slice does not already cover — that a length is in
 * bytes, that comparison is by bytes, that a substring shares rather than copies, and that the
 * UTF-8 guarantee survives every way of taking one apart.
 */
class StringRunTests extends AnyFreeSpec with RunSupport {

  "length and indexing" - {
    "a length counts bytes, not characters" in {
      run("""print("hello".len, "héllo".len, "".len)""") shouldBe "5 6 0\n"
    }

    "an index reads one byte" in {
      run("""var s = "AB"
            |print(s[0], s[1])""".stripMargin) shouldBe "65 66\n"
    }

    "the bytes of a non-ASCII character are the ones UTF-8 gives it" in {
      run("""var s = "é"
            |print(s.len, s[0], s[1])""".stripMargin) shouldBe "2 195 169\n"
    }

    "an index past the end stops the program" in {
      exits("""var s = "ab"
              |var i = 2
              |print(s[i])""".stripMargin)
    }
  }

  "substrings" - {
    "every range form takes the bytes it names" in {
      val src =
        """var s = "abcdef"
          |print(s[..], s[0..<3], s[0..3], s[3..], s[..<2])
          |""".stripMargin

      run(src) shouldBe "abcdef abc abcd def ab\n"
    }

    "a substring of a substring is relative to the substring" in {
      run("""var s = "abcdef"
            |var t = s[1..<5]
            |print(t, t[1..<3])""".stripMargin) shouldBe "bcde cd\n"
    }

    "an empty substring is legal, at the end and in the middle" in {
      run("""var s = "abc"
            |print(s[3..].len, s[1..<1].len)""".stripMargin) shouldBe "0 0\n"
    }

    "a substring shares its parent's bytes rather than copying them" in {
      val src =
        """tail(s: string) -> string
          |    s[1..]
          |end tail
          |var s = "hello"
          |print(tail(s), tail(tail(s)))
          |""".stripMargin

      run(src) shouldBe "ello llo\n"
    }

    "a bound past the end stops the program" in {
      exits("""var s = "abc"
              |var n = 4
              |print(s[0..<n].len)""".stripMargin)
    }

    "a cut inside a character stops the program" in {
      exits("""var s = "é"
              |var n = 1
              |print(s[0..<n].len)""".stripMargin)
    }

    "a cut at the start of a character is fine" in {
      run("""var s = "aéb"
            |print(s[0..<1], s[1..<3], s[3..])""".stripMargin) shouldBe "a é b\n"
    }
  }

  "concatenation" - {
    "two strings join into one" in {
      run("""print("foo" + "bar")""") shouldBe "foobar\n"
    }

    "concatenation chains left to right" in {
      run("""print("a" + "b" + "c" + "d")""") shouldBe "abcd\n"
    }

    "a literal joins with a variable, in either order" in {
      run("""var name = "ada"
            |print("hi " + name, name + "!")""".stripMargin) shouldBe "hi ada ada!\n"
    }

    // The empty string is the identity on both sides, and the join of two empties is empty.
    "the empty string is the identity" in {
      run("""var s = "xy"
            |print(s + "" == s, "" + s == s, ("" + "").len)""".stripMargin) shouldBe "true true 0\n"
    }

    "the joined length is the sum of the byte lengths, multibyte included" in {
      run("""var s = "é" + "☃"
            |print(s.len, s == "é☃")""".stripMargin) shouldBe "5 true\n"
    }

    // The result is a real owning string, not a view of an operand — it can be sliced, indexed,
    // compared, and its bytes reach through, all of which need it to hold its own buffer.
    "the result is a fully-fledged string" in {
      run("""var s = "abc" + "def"
            |print(s[2..<4], s[0], s.bytes.len, s > "abc")""".stripMargin) shouldBe "cd 97 6 true\n"
    }

    "operands that share a buffer join by content" in {
      run("""var s = "abcdef"
            |print(s[0..<2] + s[4..])""".stripMargin) shouldBe "abef\n"
    }

    "a NUL survives concatenation as an ordinary byte" in {
      run("""var s = "a\0" + "b"
            |print(s.len, s[1])""".stripMargin) shouldBe "3 0\n"
    }

    "a joined string outlives the frame when it is returned" in {
      val src =
        """join(a: string, b: string) -> string
          |    a + b
          |end join
          |print(join("out", "live"))
          |""".stripMargin

      run(src) shouldBe "outlive\n"
    }

    "a joined string survives being put on the heap" in {
      val src =
        """struct Label
          |    text: string
          |end Label
          |tag() -> string
          |    var l: &Label = Label("v" + "1")
          |    l.text
          |end tag
          |print(tag())
          |""".stripMargin

      run(src) shouldBe "v1\n"
    }

    "joining in a loop neither leaks nor frees twice" in {
      val src =
        """var total = 0
          |for i in 1..20000 do
          |    var s = "ab" + "cd"
          |    total += int(s[2]) - int(s[0])
          |print(total)
          |""".stripMargin

      run(src) shouldBe "40000\n"
    }
  }

  "in-place append" - {
    "`+=` grows a string in place" in {
      run("""var s = "a"
            |s += "b"
            |s += "cd"
            |print(s, s.len)""".stripMargin) shouldBe "abcd 4\n"
    }

    "appending the empty string is a no-op on the contents" in {
      run("""var s = "keep"
            |s += ""
            |print(s, s.len)""".stripMargin) shouldBe "keep 4\n"
    }

    // Appending a string to itself must read the old value before the slot is overwritten.
    "a string can be appended to itself" in {
      run("""var s = "ab"
            |s += s
            |print(s)""".stripMargin) shouldBe "abab\n"
    }

    "repeated append accumulates a growing string" in {
      run("""var s = ""
            |for i in 0..<5 do s += "-"
            |print(s, s.len)""".stripMargin) shouldBe "----- 5\n"
    }

    "appending in a loop neither leaks nor frees twice" in {
      val src =
        """var t = 0
          |for i in 1..20000 do
          |    var s = "ab"
          |    s += "cd"
          |    t += int(s[2])
          |print(t)
          |""".stripMargin

      run(src) shouldBe "1980000\n"
    }
  }

  "str" - {
    "renders an integer as its decimal digits, sign and all" in {
      run("""print(str(0), str(7), str(-7), str(42), str(-1000))""") shouldBe "0 7 -7 42 -1000\n"
    }

    "renders the extremes of a 64-bit integer, signed and unsigned" in {
      run("""var lo: i64 = -9223372036854775808
            |var hi: i64 = 9223372036854775807
            |var um: u64 = 18446744073709551615
            |print(str(lo), str(hi), str(um))""".stripMargin) shouldBe
        "-9223372036854775808 9223372036854775807 18446744073709551615\n"
    }

    // A narrow unsigned value renders by its magnitude, not sign-extended — 200 as a `u8`, not -56.
    "renders a narrow value by widening for its signedness" in {
      run("""var b: u8 = 200
            |var s: i8 = -5
            |print(str(b), str(s))""".stripMargin) shouldBe "200 -5\n"
    }

    "renders a bool as one of two words" in {
      run("""print(str(true), str(false))""") shouldBe "true false\n"
    }

    "renders a char as its UTF-8, one byte or several" in {
      run("""print(str('A'), str('é'), str('☃'), str('😀'))""") shouldBe "A é ☃ 😀\n"
    }

    // The NUL scalar value is a one-byte string, not the empty string a terminator would make it.
    "renders the NUL char as a single byte" in {
      run("""var s = str('\0')
            |print(s.len, s[0])""".stripMargin) shouldBe "1 0\n"
    }

    "renders a float the way print does" in {
      run("""print(str(3.5), str(-0.25), str(0.0), str(100.0))""") shouldBe "3.5 -0.25 0 100\n"
    }

    "hands a string straight back" in {
      run("""var s = "hello"
            |print(str(s), str(s) == s, str("x").len)""".stripMargin) shouldBe "hello true 1\n"
    }

    // The result is a real owning string: it can be joined, sliced, indexed, and compared.
    "produces a fully-fledged string" in {
      run("""var s = str(-40) + "!"
            |print(s, s.len, s[0], s[1..<3])""".stripMargin) shouldBe "-40! 4 45 40\n"
    }

    "a rendered string outlives the frame when it is returned" in {
      val src =
        """label(n: int) -> string
          |    "#" + str(n)
          |end label
          |print(label(17))
          |""".stripMargin

      run(src) shouldBe "#17\n"
    }

    "a rendered string survives being put on the heap" in {
      val src =
        """struct Tag
          |    text: string
          |end Tag
          |make() -> string
          |    var t: &Tag = Tag(str(99))
          |    t.text
          |end make
          |print(make())
          |""".stripMargin

      run(src) shouldBe "99\n"
    }

    "rendering in a loop neither leaks nor frees twice" in {
      val src =
        """var total = 0
          |for i in 1..20000 do
          |    var s = str(i)
          |    total += int(s.len)
          |print(total)
          |""".stripMargin

      // 1..9 → 1 digit (9), 10..99 → 2 (180), 100..999 → 3 (2700), 1000..9999 → 4 (36000),
      // 10000..20000 → 5 (50005): 9 + 180 + 2700 + 36000 + 50005.
      run(src) shouldBe "88894\n"
    }

    // str(s) on a string is the identity — the same value, so the loop must not release it a
    // second time on top of the variable it came from.
    "the identity on a string does not free it twice" in {
      val src =
        """var s = "abcd"
          |var total = 0
          |for i in 1..20000 do
          |    var t = str(s)
          |    total += int(t[0])
          |print(total)
          |""".stripMargin

      run(src) shouldBe "1940000\n"
    }
  }

  "interpolation" - {
    "a name is spliced in where it is written" in {
      run("""var who = "ada"
            |print(s"hello, $who!")""".stripMargin) shouldBe "hello, ada!\n"
    }

    "a braced hole is any expression, rendered by str" in {
      run("""var n = 20
            |print(s"n is $n, doubled ${n * 2}")""".stripMargin) shouldBe "n is 20, doubled 40\n"
    }

    "each primitive type renders as str gives it" in {
      run("""print(s"${1 < 2} ${3.5} ${'x'} ${-9}")""") shouldBe "true 3.5 x -9\n"
    }

    "a hole may itself hold a string expression" in {
      run("""var a = "foo"
            |print(s"[${ a + "!" }]")""".stripMargin) shouldBe "[foo!]\n"
    }

    "an interpolation with no holes is the literal, escapes and all" in {
      run("""print(s"tab\tend", s"")""") shouldBe "tab\tend \n"
    }

    "adjacent holes butt together with nothing between them" in {
      run("""var a = "ab"
            |var b = "cd"
            |print(s"$a$b")""".stripMargin) shouldBe "abcd\n"
    }

    "a doubled dollar is one literal dollar" in {
      run("""var n = 5
            |print(s"$$$n")""".stripMargin) shouldBe "$5\n"
    }

    // `raw` is the one form that reads nothing: no escape decoded and no hole rendered. What it is
    // for is carrying another language's source, and `${` is how shell, Make, Kotlin, Groovy and JS
    // template literals all spell interpolation — so a `raw` that read holes could not carry them.
    "raw leaves both a backslash and a hole alone" in {
      run("""var x = "hi"
            |print(raw"a\n$x and ${x}")""".stripMargin) shouldBe "a\\n$x and ${x}\n"
    }

    "the result is a real owning string, usable like any other" in {
      run("""var w = "world"
            |var s = s"hello $w"
            |print(s, s.len, s[0..<5])""".stripMargin) shouldBe "hello world 11 hello\n"
    }

    "a nested interpolation composes" in {
      run("""var n = "ada"
            |print(s"[${ s"<$n>" }]")""".stripMargin) shouldBe "[<ada>]\n"
    }

    "interpolating in a loop neither leaks nor frees twice" in {
      val src =
        """var total = 0
          |for i in 1..20000 do
          |    var s = s"n=$i"
          |    total += int(s[0])
          |print(total)
          |""".stripMargin

      run(src) shouldBe "2200000\n"
    }
  }

  "format interpolation" - {
    "an f-string hole formats through its specifier" in {
      run("""var n = 42
            |print(f"${n}%d ${n}%x ${n}%o ${n}%X")""".stripMargin) shouldBe "42 2a 52 2A\n"
    }

    "width, zero-pad, sign, and left-justify all apply" in {
      run("""var n = 42
            |print(f"[${n}%5d][${n}%05d][${n}%-5d][${n}%+d]")""".stripMargin) shouldBe
        "[   42][00042][42   ][+42]\n"
    }

    "a float takes precision and width" in {
      run("""var x = 3.14159
            |print(f"${x}%.2f ${x}%8.3f")""".stripMargin) shouldBe "3.14    3.142\n"
    }

    "a string takes width, justification, and precision" in {
      run("""var s = "hi"
            |print(f"[${s}%5s][${s}%-5s][${"hello"}%.3s]")""".stripMargin) shouldBe "[   hi][hi   ][hel]\n"
    }

    // `%x` reads the value's own bit width: a negative i32 shows eight hex digits, a u8 two.
    "an unsigned conversion shows the value's width, signed keeps its value" in {
      run("""var neg: i32 = -1
            |var b: u8 = 255
            |print(f"${neg}%x ${b}%x ${neg}%d")""".stripMargin) shouldBe "ffffffff ff -1\n"
    }

    "a bare percent in an f-string is literal text" in {
      run("""var n = 90
            |print(f"${n}%d% complete")""".stripMargin) shouldBe "90% complete\n"
    }

    "plain and formatted holes mix in one string" in {
      run("""var name = "ada"
            |var n = 7
            |print(f"$name scored ${n}%03d")""".stripMargin) shouldBe "ada scored 007\n"
    }

    "the formatted result is a real owning string" in {
      run("""var n = 255
            |var s = f"${n}%x"
            |print(s, s.len, s[0])""".stripMargin) shouldBe "ff 2 102\n"
    }

    "formatting in a loop neither leaks nor frees twice" in {
      val src =
        """var total = 0
          |for i in 1..20000 do
          |    var s = f"${i}%08d"
          |    total += int(s[0])
          |print(total)
          |""".stripMargin

      // Every rendering is zero-padded to 8 digits, so s[0] is always '0' (48).
      run(src) shouldBe "960000\n"
    }
  }

  "comparison" - {
    "equality is by bytes" in {
      run("""print("abc" == "abc", "abc" == "abd", "abc" != "ab")""") shouldBe "true false true\n"
    }

    "a shorter string that is a prefix comes first" in {
      run("""print("ab" < "abc", "abc" < "ab", "" < "a")""") shouldBe "true false true\n"
    }

    "ordering is by byte, which for UTF-8 is by codepoint" in {
      run("""print("a" < "b", "z" < "é", "é" < "☃")""") shouldBe "true true true\n"
    }

    "a comparison chains like any other" in {
      run("""print("a" < "b" < "c")""") shouldBe "true\n"
    }

    "two strings that share a buffer still compare by content" in {
      run("""var s = "abab"
            |print(s[0..<2] == s[2..], s[0..<2] == s[1..<3])""".stripMargin) shouldBe "true false\n"
    }

    "a literal is matched like any other value" in {
      val src =
        """kind(s: string) -> int
          |    s match
          |        "yes" -> 1
          |        "no" -> 0
          |        else -1
          |end kind
          |print(kind("yes"), kind("no"), kind("maybe"))
          |""".stripMargin

      run(src) shouldBe "1 0 -1\n"
    }
  }

  "bytes" - {
    "a string's bytes are a slice of the same length" in {
      run("""var s = "héllo"
            |print(s.bytes.len, s.bytes[1])""".stripMargin) shouldBe "6 195\n"
    }

    "iterating the bytes reaches each one in turn" in {
      val src =
        """var s = "abc"
          |var total = 0
          |for b in s.bytes do total += int(b)
          |print(total)
          |""".stripMargin

      run(src) shouldBe "294\n"
    }

    // The parameter is `[]const u8` because that is what `.bytes` yields: the bytes may be a
    // literal's, and those live where the platform will not let anybody write. A reader who only
    // adds them up loses nothing by saying so.
    "a byte view can be handed to anything that takes a view it does not write" in {
      val src =
        """total(bytes: []const u8) -> int
          |    var t = 0
          |    for b in bytes do t += int(b)
          |    t
          |end total
          |print(total("abc".bytes), total("abc".bytes[1..]))
          |""".stripMargin

      run(src) shouldBe "294 197\n"
    }

    // A parameter that reserves the right to write refuses these bytes instead, which is the
    // soundness item `03` opens with — the refusals are pinned in `MemoryModelClaimTests`, where
    // the chapter's claim is.
    "and the view a program keeps reading is still the string's own storage, not a copy" in {
      val src =
        """var s = "abc"
          |var b = s.bytes
          |print(b.len, b[0], b[2])
          |""".stripMargin

      run(src) shouldBe "3 97 99\n"
    }
  }

  "content" - {
    "a NUL is an ordinary byte, in the middle and at the end" in {
      run("""var s = "a\0b"
            |print(s.len, s[1], s)""".stripMargin) shouldBe "3 0 a" + 0.toChar + "b\n"
    }

    "a zero-valued string is the empty string" in {
      run("""var s: string
            |print(s.len, s == "", s)""".stripMargin) shouldBe "0 true \n"
    }

    "a string survives being carried through a struct and an enum" in {
      val src =
        """struct Named
          |    name: string
          |end Named
          |enum Answer
          |    Word(w: string)
          |    Nothing
          |end Answer
          |say(a: Answer) -> string
          |    a match
          |        Word(w) -> w
          |        Nothing -> "-"
          |end say
          |var n = Named("ada")
          |print(n.name, say(Word(n.name)), say(Nothing))
          |""".stripMargin

      run(src) shouldBe "ada ada -\n"
    }

    "a string on the heap outlives the reference that put it there" in {
      val src =
        """struct Label
          |    text: string
          |end Label
          |name() -> string
          |    var l: &Label = Label("boxed")
          |    l.text
          |end name
          |print(name())
          |""".stripMargin

      run(src) shouldBe "boxed\n"
    }

    "taking substrings in a loop neither leaks nor frees twice" in {
      val src =
        """var s = "abcdefgh"
          |var total = 0
          |for i in 1..20000 do
          |    var t = s[1..<7]
          |    total += int(t[0])
          |print(total)
          |""".stripMargin

      run(src) shouldBe "1960000\n"
    }
  }

  "a string viewed in place over bytes" - {
    // Optimized, since what could go wrong is the optimizer taking a `string`'s bytes for ones
    // nothing writes and folding the second read into the first.
    "sees a write made through the slice after it, optimized" in {
      val src =
        """import sysl.text.str_view
          |var store: []u8 = [0; 1048576]
          |store[0] = 104
          |store[1] = 105
          |val s = str_view(store[0..<2])
          |print(s)
          |store[1] = 97
          |print(s, s.len)
          |""".stripMargin

      run(src, optimize = "2") shouldBe "hi\nha 2\n"
    }

    // A string outlives every frame, so a view of an array the frame owns moves the array to the
    // heap rather than leaving the string pointing at a stack that has been popped.
    "of an array the frame owns moves the array to the heap" in {
      val src =
        """import sysl.text.str_view
          |made() -> string
          |    var a: [2]u8 = [111, 107]
          |    str_view(a[0..<2])
          |deeper(n: int) -> int
          |    var pad: [64]u8 = [0; 64]
          |    if n == 0 then int(pad[0]) else deeper(n - 1) + int(pad[1])
          |val s = made()
          |print(deeper(8), s)
          |""".stripMargin

      run(src) shouldBe "0 ok\n"
    }

    "of a slice a function was handed moves its caller's array too" in {
      val src =
        """import sysl.text.str_view
          |keep(b: []u8) -> string = str_view(b)
          |made() -> string
          |    var a: [2]u8 = [111, 107]
          |    keep(a[0..<2])
          |deeper(n: int) -> int
          |    var pad: [64]u8 = [0; 64]
          |    if n == 0 then int(pad[0]) else deeper(n - 1) + int(pad[1])
          |val s = made()
          |print(deeper(8), s)
          |""".stripMargin

      run(src) shouldBe "0 ok\n"
    }
  }
}
