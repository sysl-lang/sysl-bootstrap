package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What a file that will not parse is told, and where the caret goes.
 *
 * **The rule: the refusal is reported at the furthest point the grammar reached, and it names
 * something the reader could have written there.** Both halves were broken, separately, and the
 * combination was the first thing a newcomer met on their first typo.
 *
 * The position was the damaging half. A `Success` carries the furthest failure the parse reached on
 * its way to succeeding, which is the only record of a mistake the grammar backtracked away from —
 * and every rule in the grammar is wrapped in `at`, which rebuilt the result and so emptied that
 * field. What survived to be reported came from outside the outermost rule, so the caret landed at
 * the top of the enclosing block however far down the mistake was.
 *
 * The message was the other half. Alternatives that fail at one position are ranked last-wins, so a
 * token that can begin no expression at all was reported against whatever sits at the bottom of the
 * ladder's last alternative — `'..' expected`, a range operator, for four unrelated mistakes and
 * none of them about a range. What answers for that now is `expression`, and the three combinators
 * in `SyslParserBase` that keep an absence from being spoken of as an expectation: `skipNewlines`,
 * `onNextLine`, and `maybe`.
 */
class ParseDiagnosticTests extends AnyFreeSpec with ParseSupport {

  /** The message and the `file:line:column` of a refusal, from the rendered diagnostic. */
  private def refusal(src: String): (String, String) = {
    val out   = progError(src)
    val lines = out.linesIterator.toList
    val msg   = lines.find(_.startsWith("error: ")).getOrElse(fail(out)).stripPrefix("error: ")
    val where = lines.find(_.trim.startsWith("-->")).getOrElse(fail(out)).trim.stripPrefix("--> ")

    (msg, where)
  }

  "the caret is at the mistake, however far into a block it is" - {

    // The case the rule was written from. Every line above the last one is correct, and the caret
    // used to sit on the first of them.
    "a stray token in the sixth line of a body is reported in the sixth line" in {
      refusal("main()\n    val a = 1\n\n    val b = 2\n\n    print(a b)\n") shouldBe
        ("')' expected", "<input>:6:13")
    }

    // A function's body is parsed as part of its declaration, so a body that will not parse makes
    // the *declaration* fail — and the position that used to survive was the declaration's own first
    // line, several lines above anything wrong.
    //
    // **The fixture used to indent the value under the `=`**, back when a binding's `=` introduced
    // no block. It introduces one now, so that spelling is the feature rather than the mistake —
    // what a forgotten value actually leaves is a following line at the *same* indentation, which is
    // what this writes. The position claim is the point of the test and is unchanged.
    "a body that will not parse does not report against the signature above it" in {
      val src =
        """f(a: int) -> int
          |    val b =
          |    a + 1
          |
          |    b
          |end f
          |
          |print(str(f(1)))
          |""".stripMargin

      refusal(src) shouldBe ("expression expected", "<input>:2:12")
    }

    "and one inside a branch is reported in the branch" in {
      refusal("if a\n    print(1)\nelse\n    print(2 3)\nend if\n") shouldBe
        ("')' expected", "<input>:4:13")
    }
  }

  "four mistakes that gave one message now give four" - {

    // Each row of these was `'..' expected`, at the start of the enclosing block. None of them
    // involves a range, and the last is somebody reaching for a byte literal the language does not
    // have, told about an operator they did not write.
    "a second argument with no comma wants the comma's closing bracket" in {
      refusal("print(1 2)\n") shouldBe ("')' expected", "<input>:1:9")
    }

    "a second value after a binding wants the end of the statement" in {
      refusal("val x = 1 2\n") shouldBe ("newline expected", "<input>:1:11")
    }

    "an operator with nothing after it wants a value" in {
      refusal("print(1 +)\n") shouldBe ("expression expected", "<input>:1:10")
    }

    "and a byte literal that does not exist is read as the name beside it" in {
      refusal("print(b[0] == b';')\n") shouldBe ("')' expected", "<input>:1:16")
    }
  }

  // Both fixtures here used to put the value on an *indented* next line, which is a block now and
  // therefore not a mistake at all. A value that is genuinely missing leaves the following line at
  // the same indentation, and that is what these write; the columns are what the tests are about and
  // neither has moved.
  "a value left off the end of a line is asked for where it should have been" - {

    "a const" in {
      refusal("const A: string =\nprint(1)\n") shouldBe ("expression expected", "<input>:1:18")
    }

    // The same shape one construct over. These two used to disagree about the column, the `var`
    // pointing at the `=` and the `const` just past it.
    "and a var, in the same column relative to its '='" in {
      refusal("var x =\nprint(str(1))\n") shouldBe ("expression expected", "<input>:1:8")
    }

    // …and the value *is* allowed on the next line, indented, which is what makes the two above the
    // narrow case they are. A `const` takes it because a block of one expression is that expression,
    // so nothing here has to fold a block: what reaches the folder is the string.
    "while an indented one is the value, for a const as much as for a var" in {
      prog("const A: string =\n    \"x\"\n") shouldBe prog("const A: string = \"x\"\n")
      prog("var x =\n    1 + 2\n") shouldBe prog("var x = 1 + 2\n")
    }
  }

  "an absence is not an expectation" - {

    // `match` is written after the value it transforms and may sit on a following line, so every
    // expression statement looks for one across the blank lines behind it. Finding none is ordinary,
    // and saying so out loud outranked the real mistake, being further into the file.
    "a stray bracket is not told a 'match' is missing" in {
      refusal("print(1)\n)\n") shouldBe ("expression expected", "<input>:2:1")
    }

    "and blank lines before it do not move the complaint onto them" in {
      refusal("print(1)\n\n)\n") shouldBe ("expression expected", "<input>:3:1")
    }

    // A module header is what a file may open with and usually does not, so its absence is not
    // something to report either. This said `'module' expected` against a line that had one problem
    // and it was not the header.
    "nor is a file that opens badly told it is missing a module header" in {
      refusal(")\nprint(1)\n") shouldBe ("expression expected", "<input>:1:1")
    }

    // The same for the attributes that may follow a header, which is a repetition rather than an
    // option: `rep` records what one more of them would have wanted, and it recorded it *before* the
    // statement that actually failed, so it won the tie. Both spellings of the header are covered
    // because they take different paths through `program`.
    "nor one whose first statement fails after a module header" in {
      refusal("module m\n)\nprint(1)\n") shouldBe ("expression expected", "<input>:2:1")
    }

    "nor one after a leading attribute" in {
      refusal("@no_alloc\n)\nprint(1)\n") shouldBe ("expression expected", "<input>:2:1")
    }

    // The assignment form is `place = value`, tried at every expression and abandoned wherever there
    // is no `=`. The last of the operators it looks for was the one reported.
    "nor is a statement that runs on told about an assignment operator" in {
      refusal("val x = 1 2\n")._1 should not include "'>>='"
    }

    // A loop's label is written immediately before the loop's keyword, so every labelled form used
    // to take a stray label and then ask for its keyword on the token after it — a `for` nobody was
    // writing, reported a line further on than the label that is the actual problem.
    "and a stray label is not told a loop keyword is missing" in {
      refusal("break 'a 'b\n") shouldBe ("newline expected", "<input>:1:10")
    }
  }

  "the other alternations that answered with their last candidate" - {

    // Each of these is `describe` at work, or an indented block that must hold at least one item
    // and so lets the item's own refusal stand instead of the block's closing `dedent`.
    "an attribute with no name" in {
      refusal("@\nprint(1)\n") shouldBe ("an attribute expected", "<input>:1:2")
    }

    "an arm with no pattern" in {
      refusal("x match\n    -> 1\n") shouldBe ("a pattern expected", "<input>:2:5")
    }

    "a field with no name" in {
      refusal("struct S\n    : int\nend S\n") shouldBe ("identifier expected", "<input>:2:5")
    }

    // The same two candidates meet in a parameter list, and neither is what is reported: an empty
    // list is legal, so a name that is not there reads as a list that has ended and what is owed is
    // the bracket. The assertion is here because the *wrong* answer is a live possibility — the
    // reserved-word refusal sits at this position too, and while its lookahead reported from the
    // colon it had reached rather than from the token it started at, it outranked the bracket and
    // said `reserved word expected` at a line with no reserved word in it.
    "a parameter with no name is owed the bracket, not either candidate at the name" in {
      refusal("f(: int) -> int = 1\n") shouldBe ("')' expected", "<input>:1:3")
    }
  }

  // Both landed on dev while this branch was open, and both are the shape that produces the
  // artifact: a form that consumes tokens before discovering it is the wrong one.
  "the grammar that arrived alongside this" - {

    // `x: -> T` and `x: T` share their first two tokens, so an ordinary parameter is reached only
    // after the by-name form has taken the name and the colon and found no arrow. That refusal sits
    // before the point the parameter finished at, which is what `at` drops.
    "a by-name parameter's backtrack does not leave an arrow behind it" in {
      refusal("f(a: 1) -> int = 1\n")._1 should not include "'->'"
    }

    "and both spellings still parse" in {
      prog("f(x: -> int) -> int = x\ng(x: int) -> int = x\nprint(1)\n") should not be empty
    }

    "a frame list that runs on wants its closing bracket" in {
      refusal("@reads(a b)\nf() -> int = 1\n")._1 shouldBe "')' expected"
    }
  }

  "the forms these could have broken still parse" - {

    // `repeatedly`, the label lookahead and the `rep1sep` blocks all sit on paths every file takes.
    "a header with attributes under it, blank lines and all" in {
      prog("module m.n\n\n@no_alloc\n@requires(os)\n\n@tests\n\nprint(1)\n") should not be empty
    }

    "attributes with no header above them" in {
      prog("@no_alloc\n@requires(os)\nprint(1)\n") should not be empty
    }

    "a labelled loop, and a break that names it" in {
      prog("'outer for i in 0..<3 do\n    break 'outer\nend for\n") should not be empty
    }

    "a loop whose label is the only thing before it" in {
      prog("'a loop do\n    break 'a\nend loop\n") should not be empty
    }
  }

  // A type the grammar reads whole and then refuses is refused as a whole, so the caret is on its
  // first token. Raised after the read, each of these landed wherever the type reader stopped: the
  // '=' after the type, or the start of the next line when the type ended one.
  "a refused type spelling is reported where the type begins, not where the reader stopped" - {

    "an array written 'const' points at its '['" in {
      refusal("main()\n    val a: [4]const int = [1, 2, 3, 4]\n") shouldBe
        ("'const' says a view refuses writes, and an array is storage rather than a view of one — " +
          "read-only storage is declared with 'val', as 'val name: [N]int'", "<input>:2:12")
    }

    "and one nested in a slice points at its own '[', not the slice's" in {
      refusal("main()\n    val b: []const [4]const u8 = []\n")._2 shouldBe "<input>:2:20"
    }

    "a vector with no lane count points at its '<'" in {
      refusal("main()\n    val v: <>f32 = [1.0]\n")._2 shouldBe "<input>:2:12"
    }

    "a 'some' outside a result points at the 'some', not at the next line" in {
      refusal("struct S\n    x: some Display\nprint(1)\n")._2 shouldBe "<input>:2:8"
    }

    "and 'weak sync' points at the 'weak', not at the type after it" in {
      refusal("main()\n    val w: weak sync int = 1\n")._2 shouldBe "<input>:2:12"
    }
  }

  "what the change did not do" - {

    // The rename fires only where a rule refused *without consuming anything*. A rule that got
    // somewhere and then found a specific token missing keeps its own message, which is the more
    // useful one.
    "a specific token still names itself rather than becoming 'expression'" in {
      refusal("var x = (1, 2\n")._1 shouldBe "')' expected"
    }

    "and a block that never closes still reports the block's own expectation" in {
      refusal("main()\n    print(1)\n    )\n") shouldBe ("dedent expected", "<input>:3:5")
    }

    // A lexical error is reported ahead of whatever the grammar made of the tokens around it, and
    // that path is separate from all of this.
    "a lexical error still outranks the grammar's reaction to it" in {
      refusal("print(\"oops\n")._1 shouldBe "unterminated string literal"
    }
  }
}
