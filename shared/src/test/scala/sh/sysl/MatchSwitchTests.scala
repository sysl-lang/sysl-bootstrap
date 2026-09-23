package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A `match` decided by a discriminant lowers to **one** `switch`, whatever order its arms are
 * written in and however many there are.
 *
 * What a chain of `icmp`/`br` costs is not what it looks like it costs. The arm an input selects is
 * reached by a run of comparisons as long as the arm's position in the source, and nothing but the
 * optimizer stands between that and the program's speed: it folds the chain back into a `switch`
 * as far as its own budget reaches, and what the budget does not reach stays comparisons. So an arm
 * written late enough pays for being written late, and a reordering that changes nothing a program
 * can observe changes what it costs. An interpreter's dispatch loop is where that is felt, because
 * it has no cold arm to put last.
 *
 * Every case here therefore asks its question of two spellings of one program — the arms in
 * declaration order, and the arms shuffled. A chain passes the first and fails the second.
 */
class MatchSwitchTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** An `n`-variant dataless enum and a function matching every variant, with the arms visited in
   * `order` and each yielding `body` — which lets one case ask the same question of two spellings.
   */
  private def dispatch(n: Int, order: Seq[Int] = Nil, body: Int => String = i => s"a + ${1000 + i}"): String = {
    val arms = if order.isEmpty then 0 until n else order
    val decl = (0 until n).map(i => s"    V$i").mkString("enum Op\n", "\n", "\n")
    val head = "run(o: Op, a: int) -> int\n    o match\n"
    val body_ = arms.map(i => s"        V$i -> ${body(i)}").mkString(head, "\n", "\n")

    decl + body_ + "print(run(V3, 0))\n"
  }

  /** Every `switch` in one function's IR, as its case count. The emitter writes a switch on one
   * line and an optimizer writes it across several, so the entries are counted off the bracketed
   * table rather than off lines — and off **one function**, since the module also holds the library.
   */
  private def switches(out: String, fn: String = "run"): List[Int] =
    """(?s)switch [^\[]*\[(.*?)\]""".r
      .findAllMatchIn(defineOf(out, fn).linesIterator.map(_.trim).mkString("\n"))
      .map(m => ", label %".r.findAllIn(m.group(1)).size)
      .toList

  /** The tag comparisons a chain emits, which a switch emits none of. */
  private def tagTests(out: String, fn: String = "run"): Int =
    defineOf(out, fn).linesIterator.count(_.contains("icmp eq i32"))

  "a match over an enum" - {
    "lowers to one switch carrying every arm" in {
      switches(ir(dispatch(60))) shouldBe List(60)
    }

    // The case a chain gets wrong. Reversing the arms changes nothing a program can observe, so it
    // must change nothing about the table either.
    "lowers to the same one switch with its arms reversed" in {
      switches(ir(dispatch(60, (0 until 60).reverse))) shouldBe List(60)
    }

    "and with its arms in an order that follows nothing" in {
      switches(ir(dispatch(60, (0 until 60).sortBy(i => (i * 37) % 61)))) shouldBe List(60)
    }

    // Forty-two was the number of arms the optimizer's fold reached on one real interpreter, so the
    // arm on either side of it is where a budget would show itself.
    "at 42 arms and at 43, which a folding budget would tell apart" in {
      switches(ir(dispatch(42))) shouldBe List(42)
      switches(ir(dispatch(43))) shouldBe List(43)
    }

    "and emits no tag comparison at all" in {
      tagTests(ir(dispatch(60))) shouldBe 0
    }

    // The question the emitted IR cannot answer on its own: what a real optimizer leaves of it. The
    // arms here each do different arithmetic, so there is no table of constants to collapse into
    // and the dispatch is the whole of what survives. One switch in, one switch out.
    "which survives -O2 as one switch, in any arm order" in {
      val work = (i: Int) => s"(a * ${3 + i % 5} + $i) % 9973"

      switches(optimizedIr(dispatch(60, Nil, work))) shouldBe List(60)
      switches(optimizedIr(dispatch(60, (0 until 60).reverse, work))) shouldBe List(60)
    }
  }

  "a data enum's match switches on the tag read out of the value" in {
    val src =
      """enum Shape
        |    Circle(r: int)
        |    Square(s: int)
        |    Point
        |area(x: Shape) -> int
        |    x match
        |        Circle(r) -> r * 3
        |        Square(s) -> s * s
        |        Point -> 0
        |print(area(Square(4)))
        |""".stripMargin

    switches(ir(src), "area") shouldBe List(3)
    run(src) shouldBe "16\n"
  }

  "an arm serving two variants is two entries pointing at one block" in {
    val src =
      """enum Op
        |    A
        |    B
        |    C
        |code(o: Op) -> int
        |    o match
        |        A | B -> 1
        |        C -> 2
        |print(code(B))
        |""".stripMargin

    switches(ir(src), "code") shouldBe List(3)
    run(src) shouldBe "1\n"
  }

  "a catch-all arm becomes the switch's default" in {
    val src =
      """enum Op
        |    A
        |    B
        |    C
        |code(o: Op) -> int
        |    o match
        |        A -> 1
        |        _ -> 2
        |print(code(C))
        |""".stripMargin

    switches(ir(src), "code") shouldBe List(1)
    run(src) shouldBe "2\n"
  }

  "a named catch-all still binds the whole value" in {
    val src =
      """enum Op
        |    A
        |    B
        |code(o: Op) -> int
        |    o match
        |        A -> 1
        |        other -> int(other) + 10
        |print(code(B))
        |""".stripMargin

    switches(ir(src), "code") shouldBe List(1)
    run(src) shouldBe "11\n"
  }

  /** The shapes that keep the chain, because the tag alone does not decide them: an arm can still
   * fail after its tag has matched, and what it falls through to is the next arm in source order.
   */
  "a match the tag does not decide keeps the chain" - {
    "a guarded arm" in {
      val src =
        """enum Op
          |    Add(n: int)
          |    Nop
          |code(o: Op) -> int
          |    o match
          |        Add(n) if n > 10 -> 1
          |        Add(n) -> 2
          |        Nop -> 3
          |print(code(Add(4)))
          |""".stripMargin

      switches(ir(src), "code") shouldBe Nil
      run(src) shouldBe "2\n"
      run(src.replace("Add(4)", "Add(40)")) shouldBe "1\n"
    }

    "an arm whose payload is itself tested" in {
      val src =
        """enum Op
          |    Add(n: int)
          |    Nop
          |code(o: Op) -> int
          |    o match
          |        Add(0) -> 1
          |        Add(n) -> 2
          |        Nop -> 3
          |print(code(Add(0)))
          |""".stripMargin

      switches(ir(src), "code") shouldBe Nil
      run(src) shouldBe "1\n"
      run(src.replace("Add(0))", "Add(6))")) shouldBe "2\n"
    }
  }

  // Sixty arms, every one of them taken, through a switch whose entries are in no useful order:
  // the shape cases above cannot say that each entry still lands on the arm written for it.
  "every arm of a sixty-arm match answers for its own variant" in {
    val decl  = (0 until 60).map(i => s"    V$i").mkString("enum Op\n", "\n", "\n")
    val order = (0 until 60).sortBy(i => (i * 37) % 61)
    val head  = "run(o: Op, a: int) -> int\n    o match\n"
    val arms  = order.map(i => s"        V$i -> a + ${1000 + i}").mkString(head, "\n", "\n")
    val all   = (0 until 60).map(i => s"print(run(V$i, 0))").mkString("", "\n", "\n")

    run(decl + arms + all) shouldBe (0 until 60).map(i => s"${1000 + i}\n").mkString
  }

  /** Where one function's switch sends a tag no arm names: the first instruction of its default
   * block. Emitted IR and optimized IR both write the default as the label straight after the
   * scrutinee, so one pattern reads either.
   */
  private def defaultOf(out: String, fn: String): String = {
    val body  = defineOf(out, fn)
    val label = """switch i\d+ [^,]+, label %([\w.$-]+)""".r.findFirstMatchIn(body).map(_.group(1))
    val lines = body.linesIterator.map(_.trim).toList

    label.fold("no switch") { l =>
      lines.dropWhile(line => !line.startsWith(s"$l:")).drop(1).headOption.getOrElse("no such block")
    }
  }

  /** A statement match: every arm writes a local and none yields anything, which is the shape of an
   * interpreter's dispatch — the arm's work is its effect. A catch-all takes the last variant's
   * place, so that it has something left to cover.
   */
  private def statementDispatch(n: Int, catchAll: Boolean = false): String = {
    val decl = (0 until n).map(i => s"    V$i").mkString("enum Op\n", "\n", "\n")
    val head = "run(o: Op, a: int) -> int\n    var r = a\n    o match\n"
    val named = if catchAll then 0 until n - 1 else 0 until n
    val arms  = named.map(i => s"        V$i -> r = r * ${3 + i % 5} + $i").mkString(head, "\n", "\n")
    val rest = if catchAll then "        _ -> r = 0\n" else ""

    decl + arms + rest + "    r\nprint(run(V3, 1))\n"
  }

  /** **A statement match over every variant has an `unreachable` default, exactly as a value match
   * does.** Branching to the merge instead told the optimizer the default was a real edge, so it
   * kept the jump table's range check — a compare and a branch in front of every dispatch of an
   * interpreter's loop — for a tag no value of the enum can hold. The analyzer already refuses a
   * statement match over an enum that misses a variant, so the default is as impossible here as it
   * is for a value.
   */
  "a statement match naming every variant" - {
    "sends the default to unreachable, not to the merge" in {
      defaultOf(ir(statementDispatch(60)), "run") shouldBe "unreachable"
    }

    "and the optimizer keeps it unreachable, so no range check survives" in {
      defaultOf(optimizedIr(statementDispatch(60)), "run") shouldBe "unreachable"
    }

    "for a data enum's tag too" in {
      val src =
        """enum Shape
          |    Circle(r: int)
          |    Square(s: int)
          |    Point
          |area(x: Shape) -> int
          |    var a = 0
          |    x match
          |        Circle(r) -> a = r * 3
          |        Square(s) -> a = s * s
          |        Point -> a = 7
          |    a
          |print(area(Square(4)))
          |print(area(Point))
          |""".stripMargin

      defaultOf(ir(src), "area") shouldBe "unreachable"
      run(src) shouldBe "16\n7\n"
    }

    "and every arm still answers for its own variant" in {
      val src = statementDispatch(60).replace("print(run(V3, 1))\n", "") +
        (0 until 60).map(i => s"print(run(V$i, 1))").mkString("", "\n", "\n")

      run(src) shouldBe (0 until 60).map(i => s"${3 + i % 5 + i}\n").mkString
    }

    // A catch-all is what the default is for, so it keeps its block whatever the other arms cover.
    "but a catch-all arm is still the default" in {
      defaultOf(ir(statementDispatch(4, catchAll = true)), "run") should not be "unreachable"
    }
  }
}
