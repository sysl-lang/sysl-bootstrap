package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-1: the instruction selection the scalar table drives — width, signedness, and the
 * conversions — asserted on the emitted IR text.
 */
class ScalarCodegenTests extends AnyFreeSpec with CodegenSupport {

  "a declared width becomes the LLVM integer width" in {
    ir("var n: u12 = 7\nprint(n + 1)") should include("add i12")
  }

  "signedness picks between the division pairs" in {
    ir("var u: uint = 9\nprint(u / 2)") should include("udiv i32")
    ir("var s: int = 9\nprint(s / 2)") should include("sdiv i32")
  }

  "signedness picks between the right shifts" in {
    ir("var u: byte = 9\nprint(u >> 1)") should include("lshr i8")
    ir("var s: i8 = 9\nprint(s >> 1)") should include("ashr i8")
  }

  "an unsigned comparison uses the unsigned predicate" in {
    ir("var u: uint = 9\nprint(u < 10)") should include("icmp ult i32")
  }

  "conversions" - {
    "a narrower target truncates" in {
      ir("var n: int = 9\nprint(byte(n))") should include("trunc i32")
    }

    "a wider target extends by the source's signedness" in {
      ir("var n: i8 = 9\nprint(i32(n))") should include("sext i8")
      ir("var n: byte = 9\nprint(u32(n))") should include("zext i8")
    }

    "between integer and float" in {
      ir("var n: int = 9\nprint(real(n))") should include("sitofp i32")
      ir("var n: uint = 9\nprint(real(n))") should include("uitofp i32")
    }

    // Float-to-integer saturates through the LLVM intrinsic (defined on every target), rather
    // than a bare fptosi/fptoui whose out-of-range result is poison. Each is declared once and
    // called; the target's signedness picks fptosi.sat vs fptoui.sat.
    "float to integer uses the saturating intrinsic, not a bare cast" in {
      val signed = ir("print(int(1.5))")
      signed should include("declare i32 @llvm.fptosi.sat.i32.f64(double)")
      signed should include("call i32 @llvm.fptosi.sat.i32.f64(double")
      signed should not include "fptosi double"

      val unsigned = ir("print(u8(1.5))")
      unsigned should include("declare i8 @llvm.fptoui.sat.i8.f64(double)")
      unsigned should include("call i8 @llvm.fptoui.sat.i8.f64(double")
    }

    "a checked char conversion traps on a value that is not a scalar value" in {
      val out = ir("var n: int = 65\nprint(char(n))")

      out should include("declare void @llvm.trap()")
      out should include("call void @llvm.trap()")
    }
  }

  "a narrower float constant is the double rounded down to it" in {
    ir("var f: f32 = 1.5\nprint(f)") should include("fptrunc double 0x3FF8000000000000 to float")
  }

  // `print` picks one library renderer per *kind* of value and widens to the width that renderer
  // takes, so a narrow integer is extended at the call rather than each width having a renderer.
  "printing widens each value to the width its renderer takes" in {
    val byteMain = irMain("var b: byte = 9\nprint(b)")

    byteMain should include regex raw"zext i8 %t\d+ to i64"
    byteMain should include regex raw"call void @${keyRe("printu")}\(i64 %t\d+\)"

    irMain("var n: long = 9\nprint(n)") should include regex raw"call void @${keyRe("printi")}\(i64 %t\d+\)"
    irMain("var u: ulong = 9\nprint(u)") should include regex raw"call void @${keyRe("printu")}\(i64 %t\d+\)"
    irMain("var f: f32 = 1.5\nprint(f)") should include regex raw"call void @${keyRe("printr")}\(double %t\d+\)"
  }

  // Signedness picks the renderer and the renderer picks its digit writer; a real still hands
  // `snprintf` a `%g`.
  "the renderer a value reaches carries the matching conversion" in {
    val unsigned = ir("var b: byte = 9\nprint(b)")
    val signed   = ir("var n: long = 9\nprint(n)")

    defineOf(unsigned, Library.key("printu")) should include(s"@${Library.key("digits_ulong")}(")
    defineOf(signed, Library.key("printi")) should include(s"@${Library.key("digits_long")}(")
    ir("var x = 2.5\nprint(x)") should include("""c"%g\00"""")
  }

  // Encoding a code point is sysl in the library, not a runtime helper the compiler emits.
  "printing a char goes to the library's encoder" in {
    val out = ir("print('A')")

    mainOf(out) should include(s"call void @${Library.key("printc")}(i32 65)")
    out should include(s"define void @${Library.key("printc")}(i32 %ch.param) {")
    out should not include "@sysl.utf8"
  }

  "stack slots are hoisted into the entry block" in {
    val out = ir("for i in 1..3\n    var x = i * 2\n    print(x)")

    out should include("entry:\n  %i.addr = alloca i32\n  %x.addr = alloca i32")
  }
}
