package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A **reserved word** written where a name is being introduced.
 *
 * This is not the `__NAME__` shape `ReservedNameTests` covers — those are identifiers the compiler
 * answers for, and a declaration taking one is refused by a pass that knows what it is looking at.
 * These are the words the *grammar* spends: `val`, `match`, `struct`. Nothing knew to say so, because
 * a reserved word does not fail a rule so much as end one — a struct's body stops at the line, and
 * the reader is told `dedent expected`; a parameter list stops at the token, and the reader is told
 * `')' expected`. Both are about layout, which is the one thing that is not wrong with what they
 * wrote.
 *
 * **The backtick form is the other half of the sentence and the reason there is one to write.** A
 * quoted word is an ordinary name at every position (`QuotedIdentTests` pins the token), so the
 * refusal has somewhere to send the reader rather than only somewhere to stop them — and the tests
 * below check that the form it names actually compiles, which is what keeps the advice honest.
 */
class ReservedWordNameTests extends AnyFreeSpec with ParseSupport with CodegenSupport with RunSupport {

  /** The message and the `file:line:column` of a refusal, from the rendered diagnostic. */
  private def refusal(src: String): (String, String) = {
    val out   = progError(src)
    val lines = out.linesIterator.toList
    val msg   = lines.find(_.startsWith("error: ")).getOrElse(fail(out)).stripPrefix("error: ")
    val where = lines.find(_.trim.startsWith("-->")).getOrElse(fail(out)).trim.stripPrefix("--> ")

    (msg, where)
  }

  "a field named with one is told which word it is" - {
    "and the caret is on the word rather than at the end of the block" in {
      val (msg, where) = refusal(
        """struct S
          |    a: int
          |    val: int
          |
          |main() =
          |    print(1)
          |""".stripMargin,
      )

      msg should include("'val' is a reserved word")
      msg should include("`val`")
      where shouldBe "<input>:3:5"
    }

    // The class rather than the one word: `val` is the one somebody writes by accident, and the rule
    // is about the token kind, so any of them arrives here.
    "whichever word it is" in {
      err(
        """struct S
          |    match: int
          |
          |main()
          |    print(1)
          |""".stripMargin) should include("'match' is a reserved word")
    }

    "including as the block's first line, where nothing had been read to end" in {
      err(
        """struct S
          |    struct: int
          |
          |main()
          |    print(1)
          |""".stripMargin) should include("'struct' is a reserved word")
    }
  }

  "a parameter named with one is told the same thing" - {
    "in a function" in {
      val (msg, where) = refusal("f(val: int) -> int = 1\n\nmain() =\n    print(f(1))\n")

      msg should include("'val' is a reserved word")
      where shouldBe "<input>:1:3"
    }

    "in an extern, which binds a name nothing else would" in {
      err("""extern "puts" p(val: *u8) -> i32
            |
            |main()
            |    print(1)
            |""".stripMargin) should include("'val' is a reserved word")
    }

    "and after a parameter that was fine, so the list had already started" in {
      err("f(a: int, val: int) -> int = a\n\nmain()\n    print(f(1, 2))\n") should
        include("'val' is a reserved word")
    }
  }

  /** Every other position a name is bound at, each of which used to answer with the grammar's
   * complaint about the token after the word: `newline expected` for a function (the `loop`
   * statement had read further), `dedent expected` for a method (the type's body had ended), and
   * `identifier expected` for a `val`.
   */
  "a function named with one" - {
    "is told which word it is, at the word" in {
      val (msg, where) = refusal("loop() -> int = 1\nprint(1)\n")
      msg should include("'loop' is a reserved word, so it cannot stand as a function's name")
      msg should include("`loop`")
      where shouldBe "<input>:1:1"
    }

    "whichever word it is, and with a block body" in {
      refusal("match(n: int) -> int\n    n\n\nprint(1)\n")._1 should include("'match' is a reserved word")
    }

    "and with type parameters" in {
      refusal("then[T](x: T) -> T = x\nprint(1)\n")._1 should include("'then' is a reserved word")
    }
  }

  "a method named with one" - {
    "in a struct's body, where the reader used to be told 'dedent expected'" in {
      val (msg, where) = refusal(
        """struct Handle
          |    n: int
          |    ref(self) -> int = self.n
          |
          |print(1)
          |""".stripMargin)
      msg should include("'ref' is a reserved word, so it cannot stand as a member's name")
      where shouldBe "<input>:3:5"
    }

    "as a property" in {
      refusal("struct S\n    n: int\n    loop -> int = self.n\n\nprint(1)\n")._1 should
        include("'loop' is a reserved word, so it cannot stand as a member's name")
    }

    "in a trait, as a signature" in {
      refusal("trait T\n    while(self) -> int\n\nprint(1)\n")._1 should include("'while' is a reserved word")
    }

    "in an 'impl' block" in {
      refusal(
        """trait T
          |    f(self) -> int
          |struct S
          |    n: int
          |impl T for S
          |    for(self) -> int = 1
          |
          |print(1)
          |""".stripMargin)._1 should include("'for' is a reserved word")
    }
  }

  "a binding named with one" - {
    "a 'val'" in {
      val (msg, where) = refusal("val type = 3\nprint(1)\n")
      msg should include("'type' is a reserved word, so it cannot stand as the name a 'val' binds")
      where shouldBe "<input>:1:5"
    }

    "a 'var'" in {
      refusal("var loop = 3\nprint(1)\n")._1 should include("'loop' is a reserved word")
    }

    "a 'const'" in {
      refusal("const match: int = 3\nprint(1)\n")._1 should include("'match' is a reserved word")
    }

    "a 'ref'" in {
      refusal("var n = 1\nref loop = n\nprint(n)\n")._1 should include("'loop' is a reserved word")
    }

    "the second name of several" in {
      val (msg, where) = refusal("val a, if = 1, 2\nprint(a)\n")
      msg should include("'if' is a reserved word")
      where shouldBe "<input>:1:8"
    }

    "a 'for' variable" in {
      refusal("for loop in 0..<3\n    print(1)\n")._1 should include("'loop' is a reserved word")
    }
  }

  "a pattern binding named with one" - {
    "inside a variant" in {
      val (msg, where) = refusal(
        """val o: Option[int] = Some(1)
          |o match
          |    Some(loop) -> print(1)
          |    None -> print(0)
          |""".stripMargin)
      msg should include("'loop' is a reserved word, so it cannot stand as a name a pattern binds")
      where shouldBe "<input>:3:10"
    }

    "before an '@'" in {
      refusal(
        """val o: Option[int] = Some(1)
          |o match
          |    while @ Some(_) -> print(1)
          |    else print(0)
          |""".stripMargin)._1 should include("'while' is a reserved word")
    }

    "as a struct pattern's shorthand" in {
      refusal(
        """struct P
          |    x: int
          |val p = P(1)
          |p match
          |    P{loop} -> print(1)
          |""".stripMargin)._1 should include("'loop' is a reserved word")
    }
  }

  /** The lookahead at a statement's head is the **whole** declaration, and these are why: each
   * opens with a reserved word and a parenthesis, and each is tried against the declaration grammar
   * before the statement grammar reads it.
   */
  "and a statement that opens with a reserved word and a parenthesis is left alone" - {
    "a return of a closure with typed parameters" in {
      run("f() -> &Fn(int) -> int\n    return (x: int) -> x * 2\n\nval g = f()\nprint(g(4))\n") shouldBe "8\n"
    }

    "a condition in parentheses" in {
      run("val n = 3\nif (n > 1) then print(1) else print(0)\nwhile (false)\n    print(2)\n") shouldBe "1\n"
    }

    "a 'true' or 'else' arm in a match" in {
      run("val b = true\nb match\n    true -> print(1)\n    else print(0)\n") shouldBe "1\n"
    }
  }

  /** The lookahead is the word **and the colon**, and this is what it is for. A reserved word may
   * perfectly well begin an argument, and a call written at statement position is read against the
   * declaration grammar first — so a refusal keyed on the word alone would take these with it.
   */
  "and a reserved word that begins an argument is left alone" - {
    "a literal" in {
      run("f(x: bool) -> int = if x then 1 else 0\n\nmain()\n    print(f(true))\n") shouldBe "1\n"
    }

    "a conditional" in {
      run("f(n: int) -> int = n\n\nmain()\n    print(f(if true then 7 else 8))\n") shouldBe "7\n"
    }

    "and a call whose whole argument list is one" in {
      run("f(x: bool) -> int = if x then 1 else 0\n\nmain()\n    print(f(false))\n") shouldBe "0\n"
    }
  }

  "the form the message names is a name" - {
    "as a field, constructed and selected" in {
      run(
        """struct S
          |    a: int
          |    `val`: int
          |
          |main()
          |    var s = S(1, 2)
          |    print(s.`val`)
          |""".stripMargin) shouldBe "2\n"
    }

    "as a parameter, called by position" in {
      run("f(`val`: int) -> int = `val` + 1\n\nmain()\n    print(f(1))\n") shouldBe "2\n"
    }

    "as a function, a method and a 'val'" in {
      run(
        """`loop`() -> int = 1
          |struct H
          |    n: int
          |    `ref`(self) -> int = self.n
          |val `type` = `loop`() + H(2).`ref`()
          |print(`type`)
          |""".stripMargin) shouldBe "3\n"
    }
  }
}
