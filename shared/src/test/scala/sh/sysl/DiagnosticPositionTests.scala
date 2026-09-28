package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Where a caret lands, for the diagnostics that name a thing rather than a construct.
 *
 * **The rule this suite pins: a message that names an identifier points at that identifier.** A
 * caret under the punctuation beside it sends the reader one column away from the word they were
 * just told about — and where a single mistake produces several of these, as one `val` bound to a
 * mutable object does, the reader is looking in the wrong place several times.
 *
 * It is **not** a rule about every diagnostic. An expression's position is its start, deliberately,
 * which is why an `if` keeps the column of its keyword (`DiagnosticTests` pins that one). What is
 * asserted here is narrower: where the message names the thing to change, the caret is under it.
 *
 * A binary operator's mismatch is the second case of that, and it is a message naming *two* types
 * and complaining about the second. It points at the right operand — the one the reader edits —
 * rather than at the start of the expression. The dispatched path had always done so; the scalar
 * path pointed at the start, so `1 + "x"` and a `Box[int] + string` disagreed about where a mismatch
 * lives. They agree here.
 */
class DiagnosticPositionTests extends AnyFreeSpec with CodegenSupport {

  /** The 1-based line and column the caret points at. */
  private def location(src: String): (Int, Int) = {
    val rendered = err(src)
    val loc      = rendered.linesIterator.find(_.trim.startsWith("-->")).getOrElse(fail(rendered))
    val parts    = loc.trim.split(":").takeRight(2)

    (parts(0).toInt, parts(1).toInt)
  }

  /** The 1-based column the caret points at in the source line above it. */
  private def column(src: String): Int = location(src)._2

  "a message naming a member points at the member" - {

    // The case that prompted the rule: one `val` bound to a mutable object produces one of these per
    // mutating call, and every one used to point at a `.`.
    "a receiver that may not be written through" in {
      val src = "struct C\n    n: int\n\n    bump(*self) = self.n += 1\nend C\n\nval c: C = C(0)\nc.bump()"

      // `c.bump()` is the last line; `bump` starts at column 3.
      column(src) shouldBe 3
    }

    "a field that is not there" in {
      val src = "struct S\n    n: int\nend S\n\nvar s = S(1)\nprint(s.missing)"

      // `missing` starts at column 9 of `print(s.missing)`.
      column(src) shouldBe 9
    }

    "and a method that is not there" in {
      val src = "var n = 1\nprint(n.nope())"

      column(src) shouldBe 9
    }
  }

  "a message about what was written between brackets points there" - {

    "an index of the wrong type" in {
      val src = "var a = [1, 2, 3]\nprint(a[\"x\"])"

      // The `"x"` starts at column 9; the `[` it used to point at is column 8.
      column(src) shouldBe 9
    }
  }

  "a binary operator's mismatch points at the operand that is wrong" - {

    // The message is about `+` and names `int and string`; the operand to change is `"x"`, at
    // column 11. Column 7 is the `1`, which is what this used to say and is the one thing in the
    // expression nobody is complaining about.
    "the right operand, not the start of the expression" in {
      column("print(1 + \"x\")") shouldBe 11
    }

    // The operand's own position, not the operator's — a wide expression puts them far apart, and
    // it is the operand a reader edits.
    "wherever that operand was written" in {
      column("print(1     +     \"x\")") shouldBe 19
    }

    // The same rule through the compound-assignment path, which reaches `arithType` by a different
    // route and would not have moved on its own.
    "and through a compound assignment, at the value" in {
      column("var n = 1\nn += \"x\"") shouldBe 6
    }
  }

  /** A literal keyword is not one node the whole file shares.
   *
   * `op("true") ^^^ BoolLit(true)` builds the node once and hands the same object back for every
   * `true` the process ever parses; the first one to be given a position keeps it, so a complaint
   * about the hundredth `true` in a file pointed at the first. The caret went to an earlier line
   * that is not wrong, which reads as the compiler having found a different mistake than it did.
   */
  "a literal keyword is complained about where it is written, not where its first twin is" - {

    "the second 'true' in a file" in {
      location("val ok = true\nprint(1 + true)") shouldBe (2, 11)
    }

    // The first `null` needs a pointer to take its type from, since a bare one is a mistake in its
    // own right and would be the diagnostic this asserts about.
    "the second 'null'" in {
      location("var p: *int = null\nprint(1 + null)") shouldBe (2, 11)
    }

    "the second '()'" in {
      location("val ok = ()\nprint(1 + ())") shouldBe (2, 11)
    }

    "the second 'self', in another method" in {
      val src =
        """struct S
          |    n: int
          |
          |    a(self) -> int = self.n
          |    b(self) -> int = 1 + self
          |""".stripMargin

      location(src) shouldBe (5, 26)
    }
  }

  /** A hole in an interpolated string is lexed and parsed as a source of its own, and until its nodes
   * were placed back in the file a complaint about one pointed at line 1, column 1 of a file named
   * `<file> (interpolation)` — quoting the hole's text rather than the line it sits on.
   */
  "a complaint about what is inside an interpolation's hole points into the hole" - {

    /** The `-->` line itself, which names the file as well as the place. */
    def arrow(src: String): String = {
      val rendered = err(src)

      rendered.linesIterator.find(_.trim.startsWith("-->")).getOrElse(fail(rendered)).trim
    }

    "a braced hole, after another one on the same line" in {
      val src = "val a = 1\nprint(s\"x ${a} y ${nope} z\")"

      location(src) shouldBe (2, 20)
      arrow(src) should not include "(interpolation)"
    }

    "a bare '$name' hole" in {
      location("print(s\"x $nope\")") shouldBe (1, 12)
    }

    "a hole inside a hole" in {
      location("print(s\"a ${s\"b ${nope}\"}\")") shouldBe (1, 19)
    }

    // A text block strips its lines' indentation out of the *value*; the hole's column is still
    // the one it was written at.
    "a hole in a text block, on the block's own line and column" in {
      val src =
        "print(s\"\"\"\n" +
          "    x ${nope}\n" +
          "    \"\"\")\n"

      location(src) shouldBe (2, 9)
    }
  }

  "what deliberately keeps the older convention" - {

    // An expression's position is its start, and this message is about the operator and the type it
    // was applied to rather than about one operand — there is no offending operand to point at, so
    // the caret stays where the expression begins.
    "an operator that the type does not have" in {
      column("var a = [1, 2, 3]\nprint(a * a)") shouldBe 7
    }
  }
}
