package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

import sh.sysl.ir.LType

/** Every `llvm.` name sysl emits comes from one place (`Llvm`).
 *
 * **What is asserted here is a census rather than a spelling**, which is what makes it different
 * from the suites that already pin an individual intrinsic. `BitsTests` says the population count
 * is `llvm.ctpop.i32`; `ScalarCodegenTests` says a float-to-integer cast saturates; neither could
 * notice a *tenth* name appearing somewhere neither of them looks. Before the registry there was
 * nowhere to ask that question from — the names were literals in nine files — so this suite is the
 * one that would have had no answer.
 *
 * The claim it makes is the one a port to another back end needs: the list is the whole list. A
 * program compiled here reaches nothing in LLVM's namespace that `Llvm` does not declare.
 */
class LlvmRegistryTests extends AnyFreeSpec with CodegenSupport {

  /** A large struct, so a copy of one is a copy of bytes rather than a load and a store. */
  private val big =
    """struct Big
      |    cells: [64]i64
      |    tag: int
      |""".stripMargin

  /** One program per family the compiler emits for, named by what it is here to reach.
   *
   * A family missing from this list is a family the census cannot see, so the coverage assertion
   * below checks the *names found* against the registry's own list rather than trusting that the
   * programs are enough.
   */
  private val programs: List[(String, String)] = List(
    "the runtime trap"       -> "var n: int = 65\nprint(char(n))",
    "the saturating casts"   -> "print(int(1.5), u8(1.5))",
    "an aggregate copy"      -> (big + "var a = Big([0; 64], 1)\nvar b = a\nprint(b.tag)"),
    "an allocation's size"   -> "f(n: usize) -> []i64 = [0; n]\nprint(f(2usize).len)",
    // All six overflow overloads, which needs both signednesses and all three operators: the
    // range has to be wide enough that the result could leave the base width, or the plain
    // instruction is emitted and the intrinsic never appears.
    "checked arithmetic"     ->
      ("""type Wide = int within -2000000000..2000000000
         |type Room = int within 0..100000
         |type Small = uint within 0..10
         |var a: Wide = 1
         |var b: Wide = 2
         |var c: Room = 3
         |var d: Room = 4
         |var e: Small = 5
         |var f: Small = 6
         |print(a + b, a - b, c * d)
         |print(e + f, e - f, e * f)""".stripMargin),
    "the bit operations"     ->
      ("import sysl.math.Bits\n\nvar x: u32 = 0b10110000\n" +
        "print(x.count_ones(), x.leading_zeros(), x.trailing_zeros())\n" +
        "print(x.reverse_bits(), x.rotate_left(1), x.rotate_right(1))"),
    "the vector reductions"  ->
      ("val f: <4>f32 = [1.0, 2.0, 3.0, 4.0]\nval i: <4>i32 = [1, 2, 3, 4]\n" +
        "val u: <4>u32 = [1, 2, 3, 4]\n" +
        "val b: <4>bool = [true, false, true, false]\n" +
        "print(f.sum(), f.min(), f.max())\n" +
        "print(i.sum(), i.min(), i.max())\n" +
        // The unsigned minimum and maximum are different intrinsics from the signed ones, and an
        // `i32` vector reaches neither of them.
        "print(u.sum(), u.min(), u.max())\n" +
        "print(b.any(), b.all())"),
    "the library's floats"   -> "import sysl.math.*\n\nprint((144.0).sqrt(), (-2.5).abs())",
    "the varargs walk"       ->
      ("""total(first: int, ...) -> int
         |    var ap: va_list
         |    var bp: va_list
         |    va_start(ap)
         |    va_copy(bp, ap)
         |    va_end(bp)
         |    va_end(ap)
         |    return first
         |print(total(1, 2))""".stripMargin),
    "a placed symbol"        -> "@section(\".noinit\")\nstatic var reason: u32 = 0u32\nprint(reason)",
    // What a caller is told a postcondition established. The callee is `@noinline` so the call is
    // still a call by the time the assume is laid down beside it.
    "a contract at a call"   ->
      ("""@noinline
         |wide(n: usize) -> usize
         |    ensure result >= n
         |    n + 8usize
         |print(wide(3usize))""".stripMargin),
  )

  /** Every name in LLVM's namespace a module reaches: the globals and calls it writes with an `@`,
   * and the sections it puts them in.
   */
  private def namesIn(out: String): Set[String] =
    ("""@(llvm\.[A-Za-z0-9_.]+)""".r.findAllMatchIn(out).map(_.group(1)) ++
      """section "(llvm\.[^"]+)"""".r.findAllMatchIn(out).map(_.group(1))).toSet

  /** The union over every program above, compiled once. */
  private lazy val emitted: Set[String] =
    programs.flatMap((_, src) => namesIn(ir(src))).toSet ++
      namesIn(LibraryArtifact.metadataIr("{}", Target.default)) ++
      // A module-level `val` with a computed initializer, in an archive — which is the one shape
      // that lays down a constructor list, and it needs a compilation with no entry point of its
      // own to reach it.
      namesIn(Compiler.compiledWith(
        List(Source("<input>", "module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
          "@export\nbegin() -> i32 = start\n")),
        Nil, Target.default, entryPoint = false) match
          case Right(out) => out.ir
          case Left(e)    => fail(e))

  "the registry accounts for every name a program reaches" - {
    // The whole claim, and the one that catches an emitter reaching for a literal: a name in LLVM's
    // namespace that no entry declares is one nothing wrote down.
    "over every family at once" in {
      emitted.filterNot(Llvm.accounts) shouldBe empty
    }

    // Per program as well, so a failure names which lowering grew the name rather than handing back
    // one set to bisect by hand.
    programs.foreach { (what, src) =>
      s"reaching $what" in {
        namesIn(ir(src)).filterNot(Llvm.accounts) shouldBe empty
      }
    }
  }

  "the census is wide enough to be worth taking" - {
    // A registry every program satisfies because the programs reach two of its entries would pass
    // the assertion above and mean nothing. These are the families, each named by a base the
    // registry declares rather than by a literal.
    "every family the compiler emits for is reached" in {
      val bases = List(
        Llvm.trap.base, Llvm.memcpy.base, Llvm.vaStart.base, Llvm.vaCopy.base, Llvm.assume.base,
        Llvm.fptosiSat.base, Llvm.fptouiSat.base,
        Llvm.withOverflow("mul", signed = false).base, Llvm.withOverflow("add", signed = true).base,
        Llvm.bits("ctpop").base, Llvm.bits("fshl").base,
        Llvm.reduce("fadd").base, Llvm.reduce("umin").base,
        Llvm.sqrt.base, Llvm.fabs.base,
        Llvm.globalCtors.name, Llvm.used.name, Llvm.compilerUsed.name, Llvm.metadataSection.name,
      )

      bases.filterNot(b => emitted.exists(n => n == b || n.startsWith(b + "."))) shouldBe empty
    }

    // The complement, and the one that keeps the list honest as the compiler changes: an entry
    // nothing emits is either a name that has gone or one this suite stopped exercising, and both
    // are worth a failure. The library's floats are excepted because reaching all seven means
    // seven `extern`s a program has no other reason to write — `IntrinsicTests` covers those.
    "no callable entry is declared and never emitted" in {
      val library = List(Llvm.sqrt, Llvm.fabs, Llvm.floor, Llvm.ceil, Llvm.trunc, Llvm.round,
                         Llvm.copysign).map(_.base).toSet

      Llvm.callable.filterNot(library)
        .filterNot(b => emitted.exists(n => n == b || n.startsWith(b + "."))) shouldBe empty
    }
  }

  "the entries themselves" - {
    "are all in the namespace that makes them recognisable" in {
      Llvm.callable should not be empty
      Llvm.globals should not be empty
      all(Llvm.callable) should startWith(Llvm.prefix)
      all(Llvm.globals) should startWith(Llvm.prefix)
    }

    // The suffix is LLVM's spelling of a type rather than the type's own text — `f32` and not
    // `float` — which is the mistake `LType.overloadSuffix` exists to stop being made twice.
    "complete a base name with the operand types, in LLVM's order" in {
      Llvm.sqrt.at(LType.F(64)) shouldBe "llvm.sqrt.f64"
      Llvm.fptosiSat.at(LType.I(32), LType.F(64)) shouldBe "llvm.fptosi.sat.i32.f64"
      Llvm.withOverflow("mul", signed = false).at(LType.I(64)) shouldBe "llvm.umul.with.overflow.i64"
      Llvm.withOverflow("add", signed = true).at(LType.I(32)) shouldBe "llvm.sadd.with.overflow.i32"
      Llvm.reduce("fadd").at(LType.Vec(4, LType.F(32))) shouldBe "llvm.vector.reduce.fadd.v4f32"
      Llvm.bits("ctlz").at(LType.I(8)) shouldBe "llvm.ctlz.i8"
      Llvm.vaStart.at(LType.Ptr) shouldBe "llvm.va_start.p0"
      Llvm.memcpyName shouldBe "llvm.memcpy.p0.p0.i64"
      Llvm.trap.name shouldBe "llvm.trap"
    }

    // A base with an overload suffix on it is accounted for; a name that merely begins with the
    // same letters is not, which is what keeps `accounts` from being a substring test.
    "account for a suffixed name and not for a neighbour that shares a prefix" in {
      Llvm.accounts("llvm.sqrt") shouldBe true
      Llvm.accounts("llvm.sqrt.f64") shouldBe true
      Llvm.accounts("llvm.sqrtx.f64") shouldBe false
      Llvm.accounts("llvm.frobnicate") shouldBe false
      Llvm.accounts("llvm.metadata") shouldBe true
    }

    "refuse a name no entry declares" in {
      an[RuntimeException] should be thrownBy Llvm.bits("frobnicate")
      an[RuntimeException] should be thrownBy Llvm.reduce("frobnicate")
      an[RuntimeException] should be thrownBy Llvm.withOverflow("div", signed = true)
    }
  }

  // The two files answer two questions and both need the base names: `Intrinsics` holds a
  // declaration somebody wrote to LLVM's signature, and `Llvm` says which names exist at all. The
  // second is what the first reads, so there is one declaration site across both.
  "the library's own table names entries from the registry" in {
    Intrinsics.prefix shouldBe Llvm.prefix
    Intrinsics.supported.toSet.subsetOf(Llvm.callable.toSet) shouldBe true
  }
}
