package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-1 lowering of arrays: the layout, the checked subscript, and the two lengths — one a
 * constant folded into the IR, one a word of a header.
 */
class ArrayCodegenTests extends AnyFreeSpec with CodegenSupport {

  "an array is its elements, with no header" in {
    ir("var a = [1, 2, 3]\nprint(a[0])") should include("%a.addr = alloca [3 x i32]")
  }

  "a declaration with no initializer starts at zero" in {
    val out = ir("var buf: [8]u8\nprint(buf[0])")

    out should include("%buf.addr = alloca [8 x i8]")
    out should include("store [8 x i8] zeroinitializer, ptr %buf.addr")
  }

  "an element read is a bounds test, then a pointer into the storage" in {
    val out = ir("var a = [1, 2, 3]\nprint(a[1])")

    out should include regex raw"%t\d+ = icmp ult i64 1, 3"
    out should include("call void @llvm.trap()")
    out should include regex raw"getelementptr i32, ptr %a\.addr, i64 1"
  }

  "a signed index is widened before the unsigned test, so a negative one fails it" in {
    val out = ir("var a = [1, 2, 3]\nvar i = 0\nprint(a[i])")

    out should include regex raw"%t\d+ = sext i32 %t\d+ to i64"
    out should include regex raw"icmp ult i64 %t\d+, 3"
  }

  "an array's length is a constant" in {
    val out = ir("var a = [1, 2, 3, 4, 5]\nprint(a.len)")

    out should include("i64 5)")
    out should not include "extractvalue [5 x i32]"
  }

  "writing an element goes through the same address" in {
    val out = ir("var a = [1, 2, 3]\na[2] = 9")

    out should include regex raw"getelementptr i32, ptr %a\.addr, i64 2\n  store i32 9"
  }

  "iterating walks the storage rather than copying the array" in {
    val out = ir("var a = [1, 2, 3]\nvar s = 0\nfor x in a do s += x")

    out should include regex raw"icmp ult i64 %t\d+, 3"
    out should include regex raw"getelementptr i32, ptr %a\.addr, i64 %t\d+"
  }

  // The count is taken into a local rather than at the parameter, because a function that only
  // reads a parameter reads it through the caller's count and takes none (`BorrowedParams`) — and
  // what this is about is the shape of the walk, which a local's own copy asks for just as well.
  "an array of references is walked with a loop, not an unrolled chain" in {
    val src =
      """struct P
        |    x: int
        |hold(a: [4]&P) -> int
        |    var kept = a
        |    kept[0].x
        |var p: &P = P(1)
        |print(hold([p, p, p, p]))
        |""".stripMargin
    val out = ir(src)

    out should include("define private void @arc.copy.arr4.ref.P([4 x ptr] %v) {")
    out.linesIterator.count(_.contains("call void @arc.retain(")) should be < 8
  }

  // The null-tolerant pair exists for a slice's optional owner, so a program without a slice must
  // not have them. It cannot print, either: `print` renders through a slice of a local buffer.
  "a program with no slice in it emits no null-tolerant helpers" in {
    ir("struct P\n    x: int\nvar p: &P = P(1)\nvar x = p.x") should not include "@arc.retain_maybe"
  }
}
