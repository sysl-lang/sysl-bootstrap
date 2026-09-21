package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** The back end's own operations, reached from sysl as `extern`s (`Intrinsics`).
 *
 * The compiler has always emitted LLVM intrinsics — the bounds trap, the overflow-checked multiply,
 * the varargs walk — but each was wired to a need the compiler had, and nothing written in sysl
 * could name one. What is asserted here is that the library can, that the width in the emitted name
 * is derived rather than written, and that a declaration LLVM would refuse is refused earlier and
 * with a better message.
 */
class IntrinsicTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "the declaration" - {
    // One base name, two widths, and the suffix comes from the signature. Writing the whole name
    // would state the width twice and let the two disagree.
    "derives the width suffix from the types it was declared at" in {
      val out = ir(
        """private extern "llvm.sqrt" root(x: f64) -> f64
          |private extern "llvm.sqrt" rootf(x: f32) -> f32
          |
          |print(root(144.0), rootf(16.0f32))""".stripMargin
      )

      out should include("declare double @llvm.sqrt.f64(double)")
      out should include("declare float @llvm.sqrt.f32(float)")
      out should include("call double @llvm.sqrt.f64(")
      out should include("call float @llvm.sqrt.f32(")
    }

    "reaches the right values at both widths" in {
      run(
        """private extern "llvm.sqrt" root(x: f64) -> f64
          |private extern "llvm.sqrt" rootf(x: f32) -> f32
          |private extern "llvm.fabs" mag(x: f64) -> f64
          |private extern "llvm.copysign" signed(x: f64, y: f64) -> f64
          |private extern "llvm.floor" down(x: f64) -> f64
          |private extern "llvm.ceil" up(x: f64) -> f64
          |private extern "llvm.trunc" cut(x: f64) -> f64
          |private extern "llvm.round" near(x: f64) -> f64
          |
          |print(root(144.0), rootf(16.0f32), mag(-2.5), signed(1.0, -0.0))
          |print(down(-2.5), up(-2.5), cut(-2.5), near(-2.5), near(2.5))""".stripMargin
      ) shouldBe "12 4 2.5 -1\n-3 -2 -2 -3 3\n"
    }

    // The one that says the mechanism is doing what it is for: the magnitude comes from the sign bit,
    // so it is right for the operand a comparison cannot see.
    "an intrinsic magnitude is right at a negative zero" in {
      run(
        """private extern "llvm.fabs" mag(x: f64) -> f64
          |
          |print(mag(-0.0), mag(0.0), mag(-2.5))""".stripMargin
      ) shouldBe "0 0 2.5\n"
    }

    "the supported names are all in the namespace that makes them recognisable" in {
      Intrinsics.supported should not be empty
      all(Intrinsics.supported) should startWith(Intrinsics.prefix)
    }

    // Two declarations may share one symbol under different sysl names — the rule an ordinary
    // `extern` already follows — and the derived suffix does not change it: a module declares each
    // symbol once however many names reach it.
    "two names for one intrinsic declare it once" in {
      val out = ir(
        """private extern "llvm.sqrt" root(x: f64) -> f64
          |private extern "llvm.sqrt" also(x: f64) -> f64
          |
          |print(root(4.0), also(9.0))""".stripMargin
      )

      out.linesIterator.count(_.contains("declare double @llvm.sqrt.f64")) shouldBe 1
    }

    // The third lowered width, which nothing else in the library reaches: the suffix is read off the
    // signature rather than off a list of the two widths `sysl.math` happens to use.
    "a width the library does not itself use" in {
      ir("""private extern "llvm.fabs" mag(x: f16) -> f16""" + "\n\nprint(mag(-1.5f16))") should
        include("declare half @llvm.fabs.f16(half)")
    }

    // And the fourth, which shares the third's width and not its name. The suffix comes off the
    // type rather than off the bit count, so `bf16` names its own intrinsic instead of `f16`'s.
    "the other format at that width names its own intrinsic" in {
      ir("""private extern "llvm.fabs" mag(x: bf16) -> bf16""" + "\n\nprint(mag(-1.5bf16))") should
        include("declare bfloat @llvm.fabs.bf16(bfloat)")
    }
  }

  "the library takes the instruction rather than the call" - {
    // The whole point, asserted where it is visible: a program that takes a square root declares the
    // intrinsic and does **not** declare libm's `sqrt`, so there is no symbol left for a linker to
    // resolve and a machine with no libc can run it.
    "a program that roots a number names no libm symbol for it" in {
      val out = ir("import sysl.math.*\n\nprint((144.0).sqrt())")

      out should include("@llvm.sqrt.f64")
      out should not include "declare double @sqrt(double)"
    }

    // The complement, and what keeps the split honest: no machine sysl targets has a sine, so asking
    // LLVM for one would produce a call to this same symbol one indirection later. It stays libm's.
    "a program that takes a sine still names libm's" in {
      val out = ir("import sysl.math.*\n\nprint((0.5).sin())")

      out should include("declare double @sin(double)")
      out should not include "@llvm.sin"
    }

    // What the split buys beyond speed, and the only place it can be asserted: a bare machine has no
    // libm to link against, so before this the whole module was hosted-only. The operations that
    // became instructions are the ones that now reach a freestanding target — which is a claim about
    // the IR, since there is nothing here to link a kernel with.
    "the instructions reach a target that has no library to ask" in {
      val out = irFor(Target.aarch64Freestanding, "import sysl.math.*\n\nprint((144.0).sqrt())")

      out should include("@llvm.sqrt.f64")
      out should not include "declare double @sqrt(double)"
    }
  }

  "the error path" - {
    // The list is closed on purpose: an intrinsic's signature is LLVM's to change, and a declaration
    // that disagrees is a verifier failure or a miscompile rather than a link error.
    "an intrinsic sysl does not support is refused by name" in {
      val e = err("""private extern "llvm.frobnicate" f(x: f64) -> f64""" + "\n\nprint(f(1.0))")

      e should include("'llvm.frobnicate' is not an intrinsic sysl supports")
      e should include("llvm.sqrt")
    }

    "the wrong number of arguments" in {
      err("""private extern "llvm.sqrt" f(x: f64, y: f64) -> f64""" + "\n\nprint(f(1.0, 2.0))") should
        include("'llvm.sqrt' takes 1 argument, but 2 were declared")

      err("""private extern "llvm.copysign" f(x: f64) -> f64""" + "\n\nprint(f(1.0))") should
        include("'llvm.copysign' takes 2 arguments, but 1 were declared")
    }

    "a type the intrinsic is not overloaded on" in {
      err("""private extern "llvm.sqrt" f(x: int) -> int""" + "\n\nprint(f(1))") should
        include("takes and returns one floating-point type")
    }

    // Every operand and the result share one width, and this is the declaration that would emit a
    // `.f64` intrinsic called with a `float` — which the verifier catches and no one reads.
    "widths that disagree with each other" in {
      err("""private extern "llvm.copysign" f(x: f64, y: f32) -> f64""" + "\n\nprint(f(1.0, 2.0f32))") should
        include("takes and returns one floating-point type")
    }

    "a tail an intrinsic has nowhere to put" in {
      err("""private extern "llvm.sqrt" f(x: f64, ...) -> f64""" + "\n\nprint(f(1.0))") should
        include("an intrinsic takes a fixed argument list")
    }

    // The namespace holds *code*. Left alone this emits `@llvm.x = external global`, which the
    // verifier rejects for a reason nobody would connect back to the line that caused it.
    "an intrinsic name may not be storage" in {
      err("""private extern "llvm.sqrt" x: f64""" + "\n\nprint(x)") should
        include("code rather than storage")
    }

    // There is no body anywhere for an address to name, and LLVM refuses a module that takes one.
    "an intrinsic has no address" in {
      err(
        """private extern "llvm.sqrt" root(x: f64) -> f64
          |
          |var f = &root
          |print(f(4.0))""".stripMargin
      ) should include("'root' is an intrinsic")
    }
  }
}
