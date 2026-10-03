package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec

/** A `match` arm that hands a large payload back unchanged moves it where the value is going,
 * rather than binding it into a slot of its own and copying it again (`ControlFlowEmitter.payloadMove`).
 *
 * Two destinations are tested, each of which used to cost the payload two or three times over on
 * the stack beside the result the `match` was over:
 *
 *   - **a box** — `val p: &Big = make() match Ok(s) -> s; Err(_) -> return 1` allocates the box and
 *     copies the payload straight from the call's result into it;
 *   - **a local** — `var p = make() match …` with every other arm leaving: the local *is* the payload,
 *     inside the slot the call's result was written to (`Codegen.matchInPlace`), and nothing is copied.
 *
 * What makes them invisible is that the arm does nothing with the binding but hand it back, and the
 * counts: the destination takes its own share of everything in the payload and the result's slot
 * gives back its own when the statement ends, so every destructor runs exactly once.
 */
class MatchMoveTests extends AnyFreeSpec with CodegenSupport with RunSupport {

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

  /** `Big` is 8 + 2048 bytes on a 64-bit host. */
  private val wholeCopy = "i64 2056".r

  private val big =
    """struct Big
      |    n: usize
      |    table: [512]u32
      |
      |    step(*self) -> u32
      |        self.n += 1
      |        self.table[self.n % 512]
      |
      |@noinline
      |make(n: usize) -> Result[Big, int]
      |    if n == 0 then return Err(1)
      |    var b = Big(n, [0; 512])
      |    for i in 0..<b.table.len do b.table[i] = u32(i * n)
      |    Ok(b)
      |
      |@noinline
      |boxed(k: usize) -> int
      |    val p: &Big = make(k) match
      |        Ok(s) -> s
      |        Err(_) -> return 1
      |
      |    var t: u32 = 0
      |    for _ in 0..<10 do t += p.step()
      |    int(t)
      |
      |@noinline
      |owned(k: usize) -> int
      |    var p = make(k) match
      |        Ok(s) -> s
      |        Err(_) -> return 1
      |
      |    var t: u32 = 0
      |    for _ in 0..<10 do t += p.step()
      |    int(t)
      |
      |@noinline
      |unwrapped(k: usize) -> int
      |    val r = make(k)
      |    if r.is_err() then return 1
      |    val p: &Big = r.unwrap()
      |
      |    var t: u32 = 0
      |    for _ in 0..<10 do t += p.step()
      |    int(t)
      |
      |print(boxed(3), owned(3), unwrapped(3), boxed(0), owned(0))
      |""".stripMargin

  "a payload moved out of a call's result is never bound" - {
    "into a box: built straight in the box, with no slot of its own" in {
      val f = body(ir(big), "boxed")

      f should not include "alloca %struct.Big"
      f should not include "load %enum.sysl$Result.Big.int, ptr"
      f.linesIterator.count(_.contains("alloca %enum.sysl$Result.Big.int")) shouldBe 1
      wholeCopy.findAllIn(f).length shouldBe 1
    }

    "into a local: the local is the payload, inside the result's slot" in {
      val f = body(ir(big), "owned")

      f should not include "alloca %struct.Big"
      f.linesIterator.count(_.contains("alloca %enum.sysl$Result.Big.int")) shouldBe 1
      wholeCopy.findFirstIn(f) shouldBe None
    }

    "and the program answers what it did" in {
      run(big) shouldBe "255 255 255 1 1\n"
    }
  }

  "on a board, neither costs more than unwrapping the result" in {
    val target = Target.thumbFreestandingSoftfp
    val clang  = Toolchain.findClang(target).getOrElse(cancel(s"no clang for ${target.name}"))
    val out    = irFor(target, big)
    val triple = """(?m)^target triple = "([^"]+)"""".r.findFirstMatchIn(out).map(_.group(1)).get
    val dir    = createTempDirectory("sysl-match-move-")

    writeFile(s"$dir/p.ll", out)

    val r = exec(Seq(clang, s"--target=$triple", "-O1", "-c", "-fstack-usage", "-o", s"$dir/p.o", s"$dir/p.ll"))

    withClue(r.stderr)(r.exitCode shouldBe 0)
    assume(isFile(s"$dir/p.su"), s"$clang wrote no stack-usage file")

    val frames = readFile(s"$dir/p.su").linesIterator.flatMap { l =>
      l.split('\t') match
        case Array(where, bytes, _*) => Some(where.drop(where.lastIndexOf(':') + 1) -> bytes.toInt)
        case _                       => None
    }.toMap

    // `Big` is 4 + 2048 bytes here and its `Result` 2056: one of those is the result itself, which
    // every form has to hold; a second would be past 4 KB.
    for name <- List("boxed", "owned", "unwrapped") do
      withClue(s"$name in $frames")(frames(name) should be < 2056 + 256)

    for name <- List("boxed", "owned") do
      withClue(s"$name in $frames")(frames(name) should be <= frames("unwrapped"))
  }

  "every destructor runs exactly once, on the arm that moves and on the arm that leaves" - {
    val src =
      """struct Res
        |    id: int
        |
        |impl Drop for Res
        |    drop(self) = print("dropped", self.id)
        |
        |struct Big2
        |    r: &Res
        |    table: [512]u32
        |
        |@noinline
        |mk(i: int) -> Result[Big2, &Res]
        |    if i < 0 then return Err(Res(i))
        |    Ok(Big2(Res(i), [u32(i); 512]))
        |
        |boxed(i: int) -> int
        |    val p: &Big2 = mk(i) match
        |        Ok(s) -> s
        |        Err(e) -> return e.id
        |
        |    print("boxed", p.r.id, p.table[7])
        |    0
        |
        |owned(i: int) -> int
        |    var p = mk(i) match
        |        Ok(s) -> s
        |        Err(e) -> return e.id
        |
        |    p.table[7] = 70
        |    print("owned", p.r.id, p.table[7])
        |    0
        |
        |looped()
        |    for i in 5..<7
        |        val p = mk(i) match
        |            Ok(s) -> s
        |            Err(_) -> return
        |
        |        print("looped", p.r.id)
        |
        |from_local()
        |    val r = mk(8)
        |    val p: &Big2 = r match
        |        Ok(s) -> s
        |        Err(_) -> return
        |
        |    print("from local", p.r.id, r.unwrap().r.id)
        |
        |print(boxed(1))
        |print(boxed(-2))
        |print(owned(3))
        |print(owned(-4))
        |looped()
        |from_local()
        |print("done")
        |""".stripMargin

    "with no copy of the payload in either function" in {
      val out = ir(src)

      for name <- List("boxed", "owned") do
        withClue(name)(body(out, name) should not include "alloca %struct.Big2")
    }

    "and each count given back once" in {
      run(src) shouldBe
        """boxed 1 1
          |dropped 1
          |0
          |dropped -2
          |-2
          |owned 3 70
          |dropped 3
          |0
          |dropped -4
          |-4
          |looped 5
          |dropped 5
          |looped 6
          |dropped 6
          |from local 8 8
          |dropped 8
          |done
          |""".stripMargin
    }
  }

  "a binding the arm does anything else with is still a binding of its own" - {
    val shared =
      """struct Big
        |    n: usize
        |    table: [512]u32
        |
        |@noinline
        |make(n: usize) -> Result[Big, int]
        |    if n == 0 then return Err(1)
        |    Ok(Big(n, [u32(n); 512]))
        |
        |@noinline
        |peek(b: *Big) -> usize = b.n
        |""".stripMargin

    "an arm that takes the binding's address before handing it back" in {
      val src = shared +
        """
          |f() -> usize
          |    var p = make(4) match
          |        Ok(s) ->
          |            var c = s
          |            c.n += peek(&c)
          |            c
          |        Err(_) -> return 0
          |
          |    p.n
          |
          |print(f())
          |""".stripMargin

      run(src) shouldBe "8\n"
    }

    "the local's address kept and written through, across passes of a loop" in {
      val src = shared +
        """
          |f() -> usize
          |    var total: usize = 0
          |    for i in 1..<4
          |        var p = make(usize(i)) match
          |            Ok(s) -> s
          |            Err(_) -> return 0
          |
          |        val q: *Big = &p
          |        q.n += 10
          |        q.table[0] = 5
          |        total += p.n + usize(p.table[0]) + peek(q)
          |    total
          |
          |print(f())
          |""".stripMargin

      // Each pass: n = i + 10, read twice, plus 5.
      run(src) shouldBe s"${(1 until 4).map(i => 2 * (i + 10) + 5).sum}\n"
    }

    "a ref into the local writes the local" in {
      val src = shared +
        """
          |f() -> u32
          |    var p = make(2) match
          |        Ok(s) -> s
          |        Err(_) -> return 0
          |
          |    ref t = p.table
          |    t[3] = 30
          |    p.table[3] + p.table[4]
          |
          |print(f())
          |""".stripMargin

      run(src) shouldBe "32\n"
    }

    "a local scrutinee keeps its own payload when the moved one is changed" in {
      val src = shared +
        """
          |f() -> usize
          |    val r = make(6)
          |    var p = r match
          |        Ok(s) -> s
          |        Err(_) -> return 0
          |
          |    p.n = 100
          |    p.n + r.unwrap().n
          |
          |print(f())
          |""".stripMargin

      run(src) shouldBe "106\n"
    }

    "two arms that arrive build into the local rather than live in the result" in {
      val src = shared +
        """
          |f(k: usize) -> usize
          |    var p = make(k) match
          |        Ok(s) -> s
          |        Err(_) -> Big(7, [0; 512])
          |
          |    p.n += 1
          |    p.n
          |
          |print(f(0), f(3))
          |""".stripMargin

      run(src) shouldBe "8 4\n"
    }
  }
}
