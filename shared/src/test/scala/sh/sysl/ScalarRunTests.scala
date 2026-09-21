package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-2: the widened scalar table — integer widths and signedness, the IEEE float widths,
 * `char`, and the explicit conversions between them.
 */
class ScalarRunTests extends AnyFreeSpec with RunSupport {

  "integer widths" - {
    "arithmetic wraps at the declared width" in {
      run("""var b: byte = 200
            |var c: byte = 100
            |print(b + c)
            |""".stripMargin) shouldBe "44\n"
    }

    "an odd width wraps at its own bit count" in {
      run("""var n: u12 = 4095
            |print(n, n + 1)
            |""".stripMargin) shouldBe "4095 0\n"
    }

    "a signed minimum is writable as a negated literal" in {
      run("""var s: i8 = -128
            |var w: i32 = -2147483648
            |print(s, w)
            |""".stripMargin) shouldBe "-128 -2147483648\n"
    }

    "the 64-bit extremes print unmangled" in {
      run("""var big: i64 = 9223372036854775807
            |var u: u64 = 18446744073709551615
            |print(big, u)
            |""".stripMargin) shouldBe "9223372036854775807 18446744073709551615\n"
    }

    "a literal takes the width its context expects" in {
      run("""f(x: u16) -> u16 = x * 2
            |print(f(300))
            |""".stripMargin) shouldBe "600\n"
    }

    "a suffix names the width on the spot" in {
      run("print(7i8, 250u8, 10usize)") shouldBe "7 250 10\n"
    }

    "usize divides and compares unsigned" in {
      run("""var n: usize = 40
            |print(n / 3, n % 3, n > 39usize)
            |""".stripMargin) shouldBe "13 1 true\n"
    }
  }

  "signedness picks the instruction" - {
    "division and remainder" in {
      run("""var s: i32 = -7
            |var u: u32 = 4294967289
            |print(s / 2, u / 2)
            |""".stripMargin) shouldBe "-3 2147483644\n"
    }

    "the right shift keeps the sign only when the type has one" in {
      run("""var s: i8 = -8
            |var u: u8 = 200
            |print(s >> 1, u >> 2)
            |""".stripMargin) shouldBe "-4 50\n"
    }

    "comparison of a large unsigned value is not a negative one" in {
      run("""var u: u32 = 4000000000
            |print(u > 1u32)
            |""".stripMargin) shouldBe "true\n"
    }
  }

  "floating-point widths" - {
    "f32 computes and prints" in {
      run("""var f: f32 = 1.5
            |print(f, f * 2.0)
            |""".stripMargin) shouldBe "1.5 3\n"
    }

    "f16 holds a value it can represent exactly" in {
      run("""var h: f16 = 0.5
            |print(h + 0.25)
            |""".stripMargin) shouldBe "0.75\n"
    }

    /** **And rounds one it cannot, which the sibling above reads as though it could not.** A literal
     * crosses the emitter as a binary64 bit pattern narrowed to `float`, so a value that is not a
     * `half` either raises a real question — and the answer is that it is rounded to the nearest
     * one, exactly as the wider widths round. `0.1` is the shortest input that shows it: what prints
     * is the nearest `f16` to a tenth, and is not a tenth.
     */
    "and rounds one it cannot to the nearest it can" in {
      run("""var h: f16 = 0.1
            |print(h)
            |""".stripMargin) shouldBe "0.0999756\n"
    }

    "bf16 holds a value it can represent exactly" in {
      run("""var b: bf16 = 0.5
            |print(b + 0.25)
            |""".stripMargin) shouldBe "0.75\n"
    }

    /** **The two sixteen-bit formats differ, and a tenth is the shortest input that says how.** They
     * are the same width and divide it differently — `f16` keeps ten bits of significand and `bf16`
     * seven — so the nearest each holds to a tenth is a different number, and `bf16`'s is the coarser
     * of the two. A compiler that had quietly treated `bf16` as a second name for `f16` would print
     * one answer twice here, which is the mistake this asserts against.
     */
    "f16 and bf16 round a tenth to different values" in {
      run("""var h: f16 = 0.1
            |var b: bf16 = 0.1
            |print(h, b)
            |""".stripMargin) shouldBe "0.0999756 0.100098\n"
    }

    /** **And the precision `bf16` gives up buys range, which is the whole point of it.** Its exponent
     * is binary32's, so the largest finite `f16` doubles to an ordinary `bf16` number where at `f16`
     * it is already past the end of the format.
     */
    "bf16 reaches where f16 overflows" in {
      run("""var h: f16 = 65504.0
            |var b: bf16 = 65504.0
            |print(h * 2.0, b * 2.0)
            |""".stripMargin) shouldBe "inf 131072\n"
    }

    /** **`sysl.math`'s `Float` reaches both of them, and what a reader checks that against is the
     * printed answer** — which is what `library/math.md` puts on the page. It is pinned here because
     * the site compiles against a *published* compiler, so a change to either width's constants or
     * to the route the narrow widths compute by would be caught here and nowhere else before a
     * release had already shipped it.
     */
    "the Float trait reaches both sixteen-bit widths" in {
      run("""import sysl.math.Float
            |
            |var h: f16 = 9.0
            |var b: bf16 = 9.0
            |
            |print(h.sqrt(), b.sqrt())
            |print(f16.max_value(), bf16.epsilon())
            |""".stripMargin) shouldBe "3 3\n65504 0.0078125\n"
    }

    /** IEEE 754 leaves a `NaN` unequal to everything including itself, and makes `!=` the negation of
     * `==` rather than the ordered comparison the other three are. So exactly one of the five answers
     * true, and getting `!=` wrong is invisible in every test that does not use a `NaN` — which is why
     * this is written as all five at once.
     */
    "a NaN is unequal to itself, and unordered against everything" in {
      run("""var nan = 0.0 / 0.0
            |print(nan != nan, nan == nan, nan < 1.0, nan > 1.0, nan <= 1.0, nan >= 1.0)
            |""".stripMargin) shouldBe "true false false false false false\n"
    }

    "and unequal to an ordinary number, which is the same answer for the same reason" in {
      run("""var nan = 0.0 / 0.0
            |print(nan != 1.0, 1.0 != nan, nan == 1.0)
            |""".stripMargin) shouldBe "true true false\n"
    }

    "while two ordinary floats still compare the ordinary way" in {
      run("""print(1.5 != 2.5, 1.5 != 1.5, 1.5 == 1.5)
            |""".stripMargin) shouldBe "true false true\n"
    }

    "the same holds at a narrower width" in {
      run("""var z: f32 = 0.0
            |var nan: f32 = z / z
            |print(nan != nan, nan == nan)
            |""".stripMargin) shouldBe "true false\n"
    }
  }

  "for loops follow the width of their bounds" in {
    run("""var count: u16 = 0
          |for i in 0u16..<5u16
          |    count += i
          |print(count)
          |""".stripMargin) shouldBe "10\n"
  }

  "char" - {
    "prints as UTF-8 at every encoded length" in {
      run("print('A', 'é', '\\u{2603}', '\\u{1F600}')") shouldBe "A é ☃ 😀\n"
    }

    "orders by scalar value, which chains" in {
      run("""var c = 'q'
            |print('a' <= c <= 'z')
            |""".stripMargin) shouldBe "true\n"
    }

    "matches literals and ranges" in {
      run("""kind(c: char) -> string
            |    c match
            |        'a'..'z' -> "lower"
            |        ' ' -> "space"
            |        else "other"
            |end kind
            |print(kind('m'), kind(' '), kind('7'))
            |""".stripMargin) shouldBe "lower space other\n"
    }
  }

  "conversions" - {
    "widen, narrow, and change signedness" in {
      run("""var s: i8 = -1
            |print(i32(s), byte(s), u16(300u32), byte(300u32))
            |""".stripMargin) shouldBe "-1 255 300 44\n"
    }

    "cross between integer and floating point" in {
      run("print(int(3.9), int(-3.9), f32(7) / f32(2), real(3))") shouldBe "3 -3 3.5 3\n"
    }

    "carry a char to its codepoint and back" in {
      run("print(u32('A'), char(9731))") shouldBe "65 ☃\n"
    }

    /** **`f16` and `bf16` are the one pair where neither conversion is a widening or a narrowing**,
     * and the back end has no instruction for it: both `fptrunc` and `fpext` insist the destination
     * differ in width. The conversion goes through `f32`, which holds both exactly, so the only
     * rounding is the destination's own — a quarter survives both directions because both formats
     * hold it, and a tenth comes back as the destination's nearest rather than as the source's.
     */
    "carry a value between the two sixteen-bit formats" in {
      run("""var h: f16 = 0.25
            |var b: bf16 = 0.1
            |print(bf16(h), f16(b), f16(bf16(0.1f16)))
            |""".stripMargin) shouldBe "0.25 0.100098 0.100098\n"
    }
  }

  /** A scalar type's name in call position is a conversion **only where no declaration claims it**,
   * which is the rule a declared *type* of that name has always been held to and which a declared
   * **function** was not.
   *
   * What a reader got instead was a diagnostic about a conversion they had not written. A program
   * needing an accessor called `unit` — one that answers the unit a machine is running, say — was
   * refused with *"a 'unit' conversion takes exactly one value"*, which names a built-in nobody
   * reached for and sends the reader to count arguments on a call that has the right number.
   */
  "a declared function claims its name ahead of a built-in conversion" - {
    "a nullary one, which is the shape the old diagnostic made unwriteable" in {
      run("""unit() -> int = 7
            |print(unit())
            |""".stripMargin) shouldBe "7\n"
    }

    "one taking an argument, where the conversion would have been reached with the right count" in {
      run("""int(n: int) -> int = n * 2
            |print(int(21))
            |""".stripMargin) shouldBe "42\n"
    }

    "and the conversion is untouched wherever the name is nobody's declaration" in {
      run("print(int(3.9), byte(300u32))") shouldBe "3 44\n"
    }

    // A function in another module takes the name there and nowhere else, so a file that never
    // imported it still writes the conversion.
    "a module taking the name does not take it from a file that has not imported the function" in {
      runOf("m/m.sysl" -> "module m\n\nbyte(n: int) -> int = n + 1\n",
        "main.sysl"    -> "print(m.byte(1), byte(300u32))\n") shouldBe "2 44\n"
    }
  }

  /** `reference/types.md § char` gives `u32` → `char` two spellings by how trustworthy the value
   * is. `char(u)` traps; `char_from_u32(u)` answers. It is a free function because a scalar has no
   * member namespace for the `char.try` an earlier draft named — the obstacle `string.from_utf8`
   * met.
   *
   * The three ways a `u32` fails to be a scalar value are what the tests are built around: past
   * `0x10FFFF`, and either end of the surrogate range, whose interior is easy to get right by
   * accident and whose boundaries are not.
   */
  "the fallible way from a codepoint" - {
    "a valid scalar value comes back, and is the character it names" in {
      ask("9731") shouldBe "9731\n"

      run("""import sysl.text.char_from_u32
            |
            |char_from_u32(9731u32) match
            |    Some(c) -> print(c)
            |    None -> print("no")""".stripMargin) shouldBe "☃\n"
    }

    "the highest scalar value comes back" in {
      ask("1114111") shouldBe "1114111\n"
    }

    "one past the highest does not" in {
      ask("1114112") shouldBe "no\n"
    }

    "a surrogate does not, at either end of the range" in {
      ask("55296") shouldBe "no\n"
      ask("57343") shouldBe "no\n"
    }

    // The values either side of the hole are the ones a `<`/`<=` slip moves, so they are what tells
    // a correct guard from one that is one off at each end. Neither is printable, so each is read
    // back as its own codepoint rather than as a glyph.
    "and the values either side of the surrogates do" in {
      ask("55295") shouldBe "55295\n"
      ask("57344") shouldBe "57344\n"
    }

    // The trapping cast is the other spelling, and it has to keep trapping — a value this answered
    // `None` for must not come back from that one.
    "the trapping spelling still traps on what this refuses" in {
      exits("print(char(1114112u32))")
    }
  }

  /** Round-trips one `u32` through the fallible constructor, reading a success back as its own
   * codepoint so an unprintable answer is still something a test can compare.
   */
  private def ask(u: String): String =
    run(s"""import sysl.text.char_from_u32
           |
           |char_from_u32(${u}u32) match
           |    Some(c) -> print(u32(c))
           |    None -> print("no")""".stripMargin)
}
