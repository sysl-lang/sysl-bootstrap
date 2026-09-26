package sh.sysl

import io.github.edadma.cross_platform.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `bf16` on WebAssembly, whose back end cannot convert one (`Target.bf16AsBits`, `SoftBf16`).
 *
 * Until this, the standard library did not build for either wasm row at all: every `Float for bf16`
 * body widens to `f32`, and LLVM's wasm back end stops at the first widening with *"Cannot select:
 * bf16_to_fp"* — so `sysl emit-llvm --target wasm32-freestanding` failed on a one-line program that
 * named no `bf16`, while building the standard module it needed.
 *
 * **Two claims, and neither stands in for the other.** That the rewritten module *assembles* for
 * wasm is asserted against the whole standard library, which is what was failing. That it computes
 * the *same answers* is asserted on the host, where there is a second implementation to disagree
 * with: the same program is run once as LLVM lowers `bfloat` for this machine and once through the
 * rewrite, and the two outputs have to match — and match the values written here, which are the
 * cases where a careless narrowing rounds twice.
 */
class SoftBf16Tests extends AnyFreeSpec with Matchers {

  private val wasm = List(Target.wasm32Freestanding, Target.wasm32Wasi)

  "the standard library assembles for WebAssembly" - {
    for t <- wasm do
      s"for ${t.name}" in {
        val cc = Toolchain.findBackendClang(t).getOrElse(cancel(s"no clang for ${t.name}"))

        val ir = LibraryArtifact.build(Std.sources(t.os), t, LibraryArtifact.std) match
          case Right((ir, _)) => ir
          case Left(why)      => fail(s"the standard module did not build for ${t.name}: $why")

        val obj = createTempFile("sysl-bf16-", ".o")
        withClue(s"$cc, ${t.triple}: ")(Toolchain.compileObject(ir, obj, t, named = Some(cc)) shouldBe Right(()))

        withClue("a bfloat left in the module: ")(ir.contains("bfloat") shouldBe false)
      }
  }

  /** Every operation the rewrite has a case for, and the inputs where each could go wrong: a sum
   * that ties at `bf16` and must go to even, a `real` and an integer that each land on a midpoint if
   * rounded to binary32 first, the largest and smallest integers of several widths, a NaN and both
   * infinities, the saturating conversions back, and a vector through all three reductions.
   */
  private val program =
    """import sysl.math.Float
      |
      |var a: bf16 = 1.5
      |var b: bf16 = 0.1
      |print(a + b, a - b, a * b, a / b, -b, a < b, a == a, b > a)
      |var t: bf16 = 1.0078125
      |var q: bf16 = 0.00390625
      |print(t + q, (t - q) + q, 1.0bf16 + q)
      |var d: real = 1.0039062509313226
      |print(bf16(d), bf16(-d), bf16(1e300), bf16(-1e-300), bf16(3.3961e38))
      |var i: i32 = 16842753
      |var j: i64 = 4629700416936869889
      |var k: u64 = 18446744073709551615
      |var m: i64 = -9223372036854775807 - 1
      |var s: i8 = -128
      |var w: u16 = 65535
      |var z: i32 = 0
      |print(bf16(i), bf16(-i), bf16(j), bf16(k), bf16(m), bf16(s), bf16(w), bf16(z))
      |var big: u128 = 340282366920938463463374607431768211455
      |var low: i128 = -170141183460469231731687303715884105728
      |print(bf16(big), bf16(low))
      |var nan: bf16 = 0.0 / 0.0
      |var inf: bf16 = 1.0 / 0.0
      |print(nan, inf, -inf, nan == nan, nan < a, i32(bf16(-3.75)), u8(bf16(300.0)), i64(inf), u32(nan))
      |var h: f16 = 0.1
      |print(bf16(h), f16(b), f32(b), real(b))
      |print(bf16(2.0).sqrt(), a.floor(), b.abs(), (-a).abs(), a.copysign(-1.0))
      |var v: <4>bf16 = [1.0, 2.0, 3.0, 4.0]
      |var u = v * v + v
      |print(u[0], u[3], u.sum(), u.min(), u.max(), (v < u).any())
      |""".stripMargin

  private val expected =
    """1.60156 1.39844 0.150391 15 -0.100098 false true false
      |1.01562 1 1
      |1.00781 -1.00781 inf -0 3.38953e+38
      |1.69083e+07 -1.69083e+07 4.64771e+18 1.84467e+19 -9.22337e+18 -128 65536 0
      |inf -1.70141e+38
      |nan inf -inf false false -3 255 9223372036854775807 0
      |0.100098 0.100098 0.100098 0.100098
      |1.41406 1 0.100098 1.5 -1.5
      |2 20 40 2 20 true
      |""".stripMargin

  private def ran(compiled: Compiled): String =
    Toolchain.runIr(Right(compiled), Nil) match
      case Right((0, out))    => out
      case Right((code, out)) => fail(s"program exited with $code:\n$out")
      case Left(err)          => fail(err)

  "carried as bits, a bf16 computes what the machine's own lowering computes" - {
    lazy val compiled = Compiler.compiledWith(List(Source("<input>", program)), Nil, Target.default, Set.empty, None) match
      case Right(c)  => c
      case Left(err) => fail(err)

    lazy val soft = {
      val c = compiled
      Compiled(SoftBf16.lower(c.module), c.notes, c.links, c.exports, c.warnings)
    }

    "the rewritten module holds no bfloat at all" in {
      compiled.ir.contains("bfloat") shouldBe true
      soft.ir.contains("bfloat") shouldBe false
    }

    "and prints the same answers as the native lowering, which are the correctly rounded ones" in {
      assume(Toolchain.clangAvailable, "clang not available")

      ran(compiled) shouldBe expected
      ran(soft) shouldBe expected
    }
  }

  /** A constant is its sixteen bits, at compile time — the narrowing done once in Scala, and so the
   * one copy of the rounding this suite can check against numbers written down by hand.
   */
  "a constant's bits" - {
    def of(d: Double) = SoftBf16.bitsOf(java.lang.Double.doubleToRawLongBits(d))

    "an exact value is its top sixteen bits" in {
      of(1.0) shouldBe 0x3f80
      of(-2.0) shouldBe 0xc000
      of(-0.0) shouldBe 0x8000
    }

    "a tie goes to even, and a value just past one does not" in {
      of(1.00390625) shouldBe 0x3f80
      of(1.01171875) shouldBe 0x3f82
      of(1.0039062509313226) shouldBe 0x3f81
    }

    "a value past the largest bf16's midpoint is infinity, and a NaN stays one" in {
      of(3.4e38) shouldBe 0x7f80
      of(Double.NaN) & 0x7fc0 shouldBe 0x7fc0
    }
  }
}
