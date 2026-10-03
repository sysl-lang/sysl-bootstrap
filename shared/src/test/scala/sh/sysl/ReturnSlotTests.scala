package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec

/** A large result is written into storage its caller supplies, and the value that becomes it is
 * built there rather than built in the frame and copied there (`ReturnSlot`).
 *
 * Three shapes are tested, each of which used to cost the whole value twice over on the stack:
 *
 *   - **a local returned on every path**, alone or as the payload of `Ok`, `Some` or a struct —
 *     `var s = Big(…); s.touch(); Ok(s)` — lives in the result storage from its declaration;
 *   - **a `match` or an `if` building a large value** writes each branch's value where it is going,
 *     and `Result.unwrap`'s `Ok(v) -> v` copies the payload straight out of the receiver, with no
 *     binding and no merge slot between;
 *   - **a large call result read through its address** — a receiver, `make().unwrap()` — is written
 *     into the slot the read uses and given back there, not loaded as one value and stored again.
 *
 * The rule is what keeps it invisible, so each condition that keeps a copy has a program whose
 * output a copy-less lowering would change.
 */
class ReturnSlotTests extends AnyFreeSpec with CodegenSupport with RunSupport {

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

  /** `Big` is 8 + 2048 bytes on a 64-bit host, which is the copy every one of these used to make. */
  private val wholeCopy = "i64 2056".r

  private val big =
    """struct Big
      |    n: usize
      |    table: [512]u32
      |
      |    @noinline
      |    touch(*self)
      |        self.table[self.n % 512] = 7
      |
      |enum Err
      |    Bad
      |
      |make(k: usize) -> Big
      |    var s = Big(k, [0; 512])
      |    s.touch()
      |    s
      |
      |make_ok(k: usize) -> Result[Big, Err]
      |    if k == 0 then return Err(Bad)
      |    var s = Big(k, [0; 512])
      |    s.touch()
      |    Ok(s)
      |
      |make_some(k: usize) -> Option[Big]
      |    var s = Big(k, [0; 512])
      |    s.touch()
      |    Some(s)
      |
      |boot() -> usize
      |    val a = make(3)
      |    val b = make_ok(4).unwrap()
      |    val c = make_some(5).unwrap()
      |    a.n + b.n + c.n + usize(a.table[3] + b.table[4] + c.table[5])
      |
      |print(boot())
      |""".stripMargin

  "a local returned on every path is built in the caller's storage" - {
    "returned as itself" in {
      val f = body(ir(big), "make")

      f should not include "alloca %struct.Big"
      wholeCopy.findFirstIn(f) shouldBe None
    }

    "returned inside Ok, after a return of Err that comes before it exists" in {
      val f = body(ir(big), "make_ok")

      f should not include "alloca %struct.Big"
      wholeCopy.findFirstIn(f) shouldBe None
    }

    "returned inside Some" in {
      val f = body(ir(big), "make_some")

      f should not include "alloca %struct.Big"
      wholeCopy.findFirstIn(f) shouldBe None
    }

    "and the program answers what it did" in {
      run(big) shouldBe "33\n"
    }
  }

  "unwrap copies its payload once, straight from the receiver to the destination" in {
    val out = ir(big)

    for name <- List("sysl$Option.unwrap.Big", "sysl$Result.unwrap.Big.Err") do
      val f = body(out, name)

      withClue(name) {
        f should not include "alloca %struct.Big"
        f should not include "alloca %enum"
        wholeCopy.findAllIn(f).length shouldBe 1
      }
  }

  "on a board, none of them has a frame the size of what it returns" in {
    val target = Target.thumbFreestandingSoftfp
    val clang  = Toolchain.findClang(target).getOrElse(cancel(s"no clang for ${target.name}"))
    val out    = irFor(target, big)
    val triple = """(?m)^target triple = "([^"]+)"""".r.findFirstMatchIn(out).map(_.group(1)).get
    val dir    = createTempDirectory("sysl-frames-")

    writeFile(s"$dir/p.ll", out)

    val r = exec(Seq(clang, s"--target=$triple", "-O1", "-c", "-fstack-usage", "-o", s"$dir/p.o", s"$dir/p.ll"))

    withClue(r.stderr)(r.exitCode shouldBe 0)
    assume(isFile(s"$dir/p.su"),s"$clang wrote no stack-usage file")

    // `p.ll:make	8	static` — the function, its frame in bytes, and how it was decided.
    val frames = readFile(s"$dir/p.su").linesIterator.flatMap { l =>
      l.split('\t') match
        case Array(where, bytes, _*) => Some(where.drop(where.lastIndexOf(':') + 1) -> bytes.toInt)
        case _                       => None
    }.toMap

    // `Big` is 4 + 2048 bytes here, so a frame holding one is past 2 KB.
    for name <- List("make", "make_ok", "make_some", "sysl$Option.unwrap.Big", "sysl$Result.unwrap.Big.Err") do
      withClue(s"$name in $frames")(frames(name) should be < 256)
  }

  "a counted local handed back is owned once by the caller" - {
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
        |mk(i: int) -> Big2
        |    var s = Big2(Res(i), [0; 512])
        |    s.table[0] = 1
        |    s
        |
        |mk_ok(i: int) -> Result[Big2, int]
        |    if i == 0 then return Err(0)
        |    var s = Big2(Res(i), [0; 512])
        |    Ok(s)
        |
        |pick(c: bool) -> Big2 = if c then mk(3) else mk(4)
        |
        |f()
        |    val a = mk(1)
        |    val b = mk_ok(2).unwrap()
        |    val c = pick(true)
        |    print("live", a.r.id, b.r.id, c.r.id)
        |
        |f()
        |print("done")
        |""".stripMargin

    "with no copy in the function and the receiver given back at its address" in {
      val out = ir(src)

      body(out, "mk") should not include "alloca %struct.Big2"
      body(out, "mk_ok") should not include "alloca %struct.Big2"

      val f = body(out, "f")

      f should include("@arc.dispose_at.sysl$Result.Big2.int(ptr")
      f should not include "load %enum.sysl$Result.Big2.int"
    }

    "and each destructor runs exactly once, when the caller lets go" in {
      run(src) shouldBe "live 1 2 3\ndropped 3\ndropped 2\ndropped 1\ndone\n"
    }
  }

  "a local keeps its copy where the program could tell" - {
    val shared =
      """struct Big
        |    n: usize
        |    table: [512]u32
        |
        |struct Spoiler
        |    p: *Big
        |
        |impl Drop for Spoiler
        |    drop(self)
        |        self.p.n = 99
        |""".stripMargin

    "its address stored somewhere, written through after the value was returned" in {
      val src = shared +
        """
          |make() -> Big
          |    var s = Big(1, [0; 512])
          |    val g: &Spoiler = Spoiler(&s)
          |    s
          |
          |print(make().n)
          |""".stripMargin

      run(src) shouldBe "1\n"
    }

    "its address handed to a call that keeps it, and a destructor run on the way out" in {
      val src = shared +
        """
          |spoil(p: *Big) -> &Spoiler = Spoiler(p)
          |
          |make() -> Big
          |    var s = Big(2, [0; 512])
          |    val g = spoil(&s)
          |    s
          |
          |print(make().n)
          |""".stripMargin

      body(ir(src), "make") should include("alloca %struct.Big")
      run(src) shouldBe "2\n"
    }

    "a different value returned after it exists, built out of its own parts" in {
      val src =
        """struct Two
          |    a: [256]u32
          |    b: [256]u32
          |
          |flip(k: u32) -> Option[Two]
          |    var s = Two([k; 256], [k + 1; 256])
          |    if k == 1 then return Some(Two(s.b, s.a))
          |    Some(s)
          |
          |val f = flip(1).unwrap()
          |val g = flip(5).unwrap()
          |print(f.a[0], f.b[0], g.a[0], g.b[0])
          |""".stripMargin

      run(src) shouldBe "2 1 5 6\n"
    }

    "another part of the result changing it after it was taken" in {
      val src =
        """struct Big
          |    n: usize
          |    table: [512]u32
          |
          |bump(p: *Big) -> usize
          |    p.n += 1
          |    p.n
          |
          |struct Held
          |    big: Big
          |    m: usize
          |
          |pair() -> Held
          |    var s = Big(1, [0; 512])
          |    Held(s, bump(&s))
          |
          |val h = pair()
          |print(h.big.n, h.m)
          |""".stripMargin

      run(src) shouldBe "1 2\n"
    }

    // A `?` leaves through the result storage like a `return`, and what it writes there is the
    // error — so a local built in that storage would have its bytes written over with its counts
    // still owed, and its destructor would never run.
    "a try after it, which leaves through the result storage" in {
      run(tried) shouldBe "dropped 1\nerr\nlive 2\ndropped 2\ndone\n"
      body(ir(tried), "mk") should include("alloca %struct.Big2")
    }

    "a try inside the construction that returns it" in {
      val src = tried.replace(
        """    val n = check(k - 1)?
          |    s.table[0] = u32(n)
          |    Ok(s)""".stripMargin,
        """    Ok(Held(s, check(k - 1)?))""".stripMargin)
        .replace("mk(k: int) -> Result[Big2, Fault]", "mk(k: int) -> Result[Held, Fault]")
        .replace("val b = mk(2).unwrap()\n    print(\"live\", b.r.id)",
                 "val b = mk(2).unwrap()\n    print(\"live\", b.big.r.id, b.n)")
        .replace("enum Fault", "struct Held\n    big: Big2\n    n: int\n\nenum Fault")

      run(src) shouldBe "dropped 1\nerr\nlive 2 1\ndropped 2\ndone\n"
      body(ir(src), "mk") should include("alloca %struct.Big2")
    }
  }

  "a try that comes before the local exists, or inside its initializer, leaves nothing behind" - {
    "before it: the local is still built in the caller's storage" in {
      val src = tried.replace(
        """    var s = Big2(Res(k), [0; 512])
          |    val n = check(k - 1)?
          |""".stripMargin,
        """    val n = check(k - 1)?
          |    var s = Big2(Res(k), [0; 512])
          |""".stripMargin)

      body(ir(src), "mk") should not include "alloca %struct.Big2"
      run(src) shouldBe "err\nlive 2\ndropped 2\ndone\n"
    }

    "inside its initializer: what was built of it is let go of once" in {
      val src = tried.replace(
        """    var s = Big2(Res(k), [0; 512])
          |    val n = check(k - 1)?
          |    s.table[0] = u32(n)
          |""".stripMargin,
        """    var s = Big2(Res(k), [u32(check(k - 1)?); 512])
          |""".stripMargin)

      run(src) shouldBe "dropped 1\nerr\nlive 2\ndropped 2\ndone\n"
    }
  }

  private val tried =
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
      |enum Fault
      |    Bad
      |
      |check(k: int) -> Result[int, Fault] = if k == 0 then Err(Bad) else Ok(k)
      |
      |mk(k: int) -> Result[Big2, Fault]
      |    var s = Big2(Res(k), [0; 512])
      |    val n = check(k - 1)?
      |    s.table[0] = u32(n)
      |    Ok(s)
      |
      |f()
      |    mk(1) match
      |        Ok(_) -> print("ok")
      |        Err(_) -> print("err")
      |
      |    val b = mk(2).unwrap()
      |    print("live", b.r.id)
      |
      |f()
      |print("done")
      |""".stripMargin
}
