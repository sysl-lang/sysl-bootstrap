package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@threaded` on a `loop` (`reference/attributes.md § @threaded`): the head of the body and the
 * closing `match`'s one `switch` are laid down again at the foot of every arm, so an interpreter's
 * dispatch has one indirect jump per arm for the branch predictor to learn from instead of one per
 * loop.
 *
 * Three things are asked of it. The **shape** — one switch at the head and one more per arm, each
 * carrying the whole table. The **meaning** — a program runs to the same answer with the attribute
 * and without it, through every way an arm can end. And the **refusals**, each a body with no single
 * dispatch to replicate or a head that cannot be emitted twice.
 */
class ThreadedDispatchTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** A small accumulator machine: `Inc`, `Dec`, `Double`, a conditional jump, a `Skip` that leaves
   * its arm by `continue`, and `Halt`, which leaves the loop by `break`. The answer folds the
   * accumulator and the number of instructions run into one number, so an arm that ran twice or not
   * at all shows.
   */
  private def machine(threaded: Boolean): String =
    s"""enum Op
       |    Inc(n: int)
       |    Dec
       |    Double
       |    Jnz(to: usize)
       |    Skip
       |    Halt
       |exec(code: []const Op) -> int
       |    var pc: usize = 0
       |    var acc = 0
       |    var steps = 0
       |    ${if threaded then "@threaded" else ""}
       |    loop
       |        if pc >= code.len then break
       |        val op = code[pc]
       |        pc += 1
       |        steps += 1
       |        op match
       |            Inc(n) -> acc += n
       |            Dec -> acc -= 1
       |            Double -> acc *= 2
       |            Jnz(to) ->
       |                if acc != 0 then pc = to
       |            Skip ->
       |                pc += 1
       |                continue
       |            Halt -> break
       |    acc * 1000 + steps
       |print(exec([Inc(3), Double, Dec, Jnz(2), Skip, Inc(100), Inc(7), Halt, Inc(1000)]))
       |print(exec([Inc(2), Dec, Jnz(1)]))
       |""".stripMargin

  /** Every `switch` in one function's IR, as its case count — read off the bracketed table, since
   * the emitter writes a switch on one line and an optimizer across several.
   */
  private def switches(out: String, fn: String): List[Int] =
    """(?s)switch [^\[]*\[(.*?)\]""".r
      .findAllMatchIn(defineOf(out, fn).linesIterator.map(_.trim).mkString("\n"))
      .map(m => ", label %".r.findAllIn(m.group(1)).size)
      .toList

  "the dispatch is laid down at the head and again at the foot of every arm that reaches it" in {
    // Four arms reach their foot; `Skip` leaves by `continue` and `Halt` by `break`.
    switches(ir(machine(true)), "exec") shouldBe List.fill(5)(6)
    switches(ir(machine(false)), "exec") shouldBe List(6)
  }

  "a program answers the same with the attribute and without it" in {
    run(machine(true)) shouldBe "7017\n5\n"
    run(machine(false)) shouldBe "7017\n5\n"
  }

  "a catch-all arm is dispatched to and dispatches again" in {
    val src =
      """enum Op
        |    Inc
        |    Dec
        |    Halt
        |exec(code: []const Op) -> int
        |    var pc: usize = 0
        |    var acc = 0
        |    @threaded
        |    loop
        |        val op = code[pc]
        |        pc += 1
        |        op match
        |            Halt -> break
        |            other -> acc += int(other) + 1
        |    acc
        |print(exec([Inc, Dec, Dec, Halt]))
        |""".stripMargin

    run(src) shouldBe "5\n"
  }

  "what it refuses" - {
    "a body that does not end in a match" in {
      err(
        """exec() -> int
          |    var n = 0
          |    @threaded
          |    loop
          |        n += 1
          |        if n > 3 then break
          |    n
          |print(exec())
          |""".stripMargin) should include("this body does not end in a 'match'")
    }

    "a match with a guard" in {
      err(
        """enum Op
          |    Add(n: int)
          |    Halt
          |exec(code: []const Op) -> int
          |    var pc: usize = 0
          |    var acc = 0
          |    @threaded
          |    loop
          |        val op = code[pc]
          |        pc += 1
          |        op match
          |            Add(n) if n > 0 -> acc += n
          |            Add(n) -> acc -= n
          |            Halt -> break
          |    acc
          |print(exec([Add(1), Halt]))
          |""".stripMargin) should include("an arm carries a guard")
    }

    "a scrutinee that holds a counted reference" in {
      err(
        """enum Op
          |    Say(s: string)
          |    Halt
          |exec(code: []const Op) -> int
          |    var pc: usize = 0
          |    @threaded
          |    loop
          |        val op = code[pc]
          |        pc += 1
          |        op match
          |            Say(s) -> print(s)
          |            Halt -> break
          |    0
          |print(exec([Say("a"), Halt]))
          |""".stripMargin) should include("holds a counted reference")
    }

    "something that is not a loop" in {
      err(
        """exec() -> int
          |    var n = 0
          |    @threaded
          |    while n < 3
          |        n += 1
          |    n
          |print(exec())
          |""".stripMargin) should include("what follows is not a 'loop'")
    }

    "another annotation beside it" in {
      err(
        """exec() -> int
          |    var n = 0
          |    @threaded
          |    @cold
          |    loop
          |        break
          |    n
          |print(exec())
          |""".stripMargin) should include("nothing stands beside it")
    }
  }
}
