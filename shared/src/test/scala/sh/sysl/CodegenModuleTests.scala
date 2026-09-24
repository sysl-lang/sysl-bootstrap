package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Codegen of the module scaffolding and of the `print` desugaring: the preamble a module carries,
 * string interning, and which library renderer each type of value reaches.
 */
class CodegenModuleTests extends AnyFreeSpec with CodegenSupport {

  "module scaffolding" - {
    // The signature is C's, because this is the function the platform's own start-up code calls and
    // that code passes a count and a vector (`reference/modules.md § Where a program starts`). They are taken whether or not the program
    // asks for them: a `main(args: []string)` is where they turn into a slice.
    "defines main, with the pair the platform passes" in {
      val out = ir("print(1)")

      out should include("define i32 @main(i32 %argc, ptr %argv) {")
      out should include("ret i32 0")
    }

    // `printf` was the old builtin's whole implementation. Nothing emits a call to it now, so
    // nothing may declare it either — a program reaches libc only where it says so itself.
    "declares no printf of its own" in {
      ir("print(1)") should not include "@printf"
    }

    "an extern may still take the name" in {
      val out = ir("""extern printf(fmt: *u8, ...) -> int
                     |printf(c"hi\n")""".stripMargin)

      out should include("declare i32 @printf(ptr, ...)")
    }
  }

  "literals and print" - {
    "a string constant is interned NUL-terminated" in {
      ir("print(\"hi\")") should include("""c"hi\00"""")
    }

    "an int goes to the signed renderer, widened to its width" in {
      val out = ir("print(42)")

      mainOf(out) should include(s"call void @${Library.key("printi")}(i64 %t1)")
      mainOf(out) should include("sext i32 42 to i64")
      out should include(s"define i64 @${Library.key("digits_long")}(")
    }

    "a float is emitted as a hex double and rendered via %g" in {
      val out = ir("print(1.5)")

      mainOf(out) should include(s"call void @${Library.key("printr")}(double 0x3FF8000000000000)")
      out should include("""c"%g\00"""")
    }

    "multiple args are separated by a printed space" in {
      val out = irMain("print(1, 2)")

      out should include(s"call void @${Library.key("printc")}(i32 32)")
      out.linesIterator.count(_.contains(s"@${Library.key("printi")}")) shouldBe 2
    }
  }
}
