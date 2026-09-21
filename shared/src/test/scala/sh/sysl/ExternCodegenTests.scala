package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Codegen for `extern` declarations and for the `never` type.
 *
 * An extern turns into a `declare` and its calls into ordinary calls, so what is worth asserting
 * is the two things codegen decides on its own: *which* externs are declared, and that a call to
 * one that does not return ends its basic block. The second is what lets a diverging `match` arm
 * work without any dataflow analysis — everything after the `unreachable` is dropped because the
 * block is closed.
 */
class ExternCodegenTests extends AnyFreeSpec with CodegenSupport {

  "declarations" - {
    /** The narrow parts carry the widening the convention asks for (`CAbi.extension`) — `zeroext` on
      * the `bool` argument and on the `u8` result, this being a Darwin target. The `i32` and the
      * `double` fill a register already and take nothing, which is what makes this one assertion
      * show both halves of the rule.
      */
    "a called extern is declared with its lowered signature" in {
      ir("extern f(a: int, b: real, c: bool) -> u8\nprint(f(1, 2.0, true))") should
        include("declare zeroext i8 @f(i32, double, i1 zeroext)")
    }

    "no result at all is declared void" in {
      ir("extern f()\nf()") should include("declare void @f()")
    }

    "a 'never' result is declared void too, since nothing comes back" in {
      ir("extern stop(code: int) -> never\nstop(1)") should include("declare void @stop(i32)")
    }

    // A view is three words, which is more than any of the four conventions puts in registers, so
    // the C boundary is the one place it does *not* lower as it does everywhere else: it is passed
    // by address, exactly as C passes a struct of that size (`CAbi`).
    //
    // **Compiled for a named target rather than for this machine**, because how an aggregate is
    // passed is the one thing that is genuinely a property of the ABI: AAPCS64 and SysV x86-64
    // classify the same struct differently and spell it differently in the IR. Left at the default
    // this asserted whichever answer the author's laptop gave, which is a test that checks one
    // convention on one machine and nothing at all anywhere else. Naming the target is what makes it
    // a claim about AAPCS64 that holds wherever the suite runs.
    "a pointer lowers as it does everywhere else, and a view as the ABI passes an aggregate" in {
      irFor(Target.aarch64MacOS, "extern f(p: *u8, s: string) -> *int\nvar b: u8 = 0\nprint(f(&b, \"hi\") == null)") should
        include("declare ptr @f(ptr, ptr align 8)")
    }

    // The library declares `exit` for `unwrap` to stop with, and almost no program calls it. An
    // extern nothing reaches must not reach the output either.
    "an unused extern is not declared" in {
      val out = ir("print(1)")

      out should not include "@exit"
      out should include("declare i32 @snprintf(ptr, i64, ptr, ...)") // one that print does reach
    }

    "an extern is declared once however many times it is called" in {
      val out = ir("extern f(n: int) -> int\nprint(f(1), f(2), f(3))")

      out.linesIterator.count(_.startsWith("declare i32 @f")) shouldBe 1
    }

    // The runtime declares `malloc` for its own use, and a module may not declare one symbol
    // twice — so the extern defers to the declaration that is already there.
    "an extern does not redeclare a name the runtime already declared" in {
      val src =
        """extern malloc(n: usize) -> *u8
          |struct Inner
          |    v: int
          |var r: &Inner = Inner(1)
          |print(r.v, malloc(8) == null)""".stripMargin
      val out = ir(src)

      out.linesIterator.count(l => l.startsWith("declare") && l.contains("@malloc(")) shouldBe 1
      out should include("declare ptr @malloc(i64)")
    }
  }

  /** A link name separates the symbol the linker resolves from the name the program calls it by,
   * so a declaration can reach a C function whose spelling is taken, or shaped nothing like sysl.
   * Both the `declare` and every call have to name the symbol; nothing else may.
   */
  "a link name" - {
    "declares and calls the symbol, not the sysl name" in {
      val out = ir("""extern "abort" stop() -> never
                     |stop()""".stripMargin)

      out should include("declare void @abort()")
      out should include("call void @abort()")
      out should not include "@stop"
    }

    "carries the ellipsis to the symbol as well" in {
      val out = ir("""extern "printf" say(fmt: *u8, ...) -> int
                     |var p: *u8 = null
                     |print(say(p, 1))""".stripMargin)

      out should include("declare i32 @printf(ptr, ...)")
      out should include regex raw"call i32 \(ptr, \.\.\.\) @printf\(ptr %t\d+, i32 1\)"
      out should not include "@say"
    }

    // The whole point: the library reaches `snprintf` under a name of its own, so a program may
    // still declare `snprintf` itself. Two declarations, one symbol, one `declare`.
    "lets two declarations share one symbol" in {
      val src =
        """extern "putchar" emit(c: int) -> int
          |extern putchar(c: int) -> int
          |print(emit(65), putchar(66))""".stripMargin
      val out = ir(src)

      out.linesIterator.count(l => l.startsWith("declare") && l.contains("@putchar(")) shouldBe 1
      out should include("declare i32 @putchar(i32)")
      out should include("call i32 @putchar(i32 65)")
      out should include("call i32 @putchar(i32 66)")
    }

    "is rejected when it is not a symbol a linker could resolve" in {
      err("""extern "no such thing" f()""") should include("is not a symbol a linker can resolve")
      err("""extern "" f()""") should include("is not a symbol a linker can resolve")
    }

    /** The accepting half of the same rule (`reference/ffi.md § extern — a declaration with no body`), which is the half that reaches the emitter: a
      * symbol may hold `_`, `$` and `.` as well as letters and digits, and those are exactly the
      * characters LLVM's own unquoted identifier admits — so the name goes through as written,
      * with no quoting and no mangling. Real symbols look like this: a C++ mangling carries `$`, an
      * Objective-C or versioned libc symbol carries `.`.
      */
    "may hold every character a linker symbol may hold, unquoted" in {
      val out = ir("""extern "foo.bar$baz_9" f(n: int) -> int
                     |print(f(1))""".stripMargin)

      out should include("declare i32 @foo.bar$baz_9(i32)")
      out should include("call i32 @foo.bar$baz_9(i32 1)")
      out should not include "\"foo.bar"
    }
  }

  /** A variadic callee is written two ways codegen has to get right: the `...` in its declaration,
   * and the whole function type at each call — LLVM will not take the short form for a callee that
   * has an ellipsis, because the argument list alone does not say where the declared parameters
   * stop. The promotions are the analyzer's, so what is asserted here is that they arrive.
   */
  "variadic" - {
    "the declaration carries the ellipsis" in {
      ir("extern log(fmt: *u8, n: int, ...)\nvar p: *u8 = null\nlog(p, 1)") should
        include("declare void @log(ptr, i32, ...)")
    }

    "a call names the callee's whole function type" in {
      ir("extern f(n: int, ...) -> int\nprint(f(1, 2))") should include("call i32 (i32, ...) @f(i32 1, i32 2)")
    }

    "a call with no tail at all is written the same way" in {
      ir("extern f(n: int, ...) -> int\nprint(f(1))") should include("call i32 (i32, ...) @f(i32 1)")
    }

    // An ordinary callee keeps the short form, so the ellipsis is what causes the difference and
    // not merely something that happens to accompany it.
    "a non-variadic extern is still called by result type alone" in {
      val out = ir("extern f(n: int) -> int\nprint(f(1))")

      out should include("call i32 @f(i32 1)")
      out should include("declare i32 @f(i32)")
    }

    // LLVM applies no default argument promotions of its own, so a narrow value handed over as
    // written would be read back as garbage. Signedness decides which widening.
    "a narrow integer is widened to 32 bits, by its own signedness" in {
      val out = ir("extern f(n: int, ...)\nvar a: u8 = 200\nvar b: i8 = -5\nf(1, a, b)")

      out should include("zext i8")
      out should include("sext i8")
      out should include regex "call void \\(i32, \\.\\.\\.\\) @f\\(i32 1, i32 %t\\d+, i32 %t\\d+\\)"
    }

    "a narrow float is widened to double, whichever narrow float it is" in {
      ir("extern f(n: int, ...)\nvar x: f32 = 1.5f32\nf(1, x)") should include("fpext float")
      ir("extern f(n: int, ...)\nvar x: f16 = 1.5f16\nf(1, x)") should include("fpext half")
      ir("extern f(n: int, ...)\nvar x: bf16 = 1.5bf16\nf(1, x)") should include("fpext bfloat")
    }

    // Each variadic callee is written with *its own* function type, so two of them in one program
    // must not be given each other's.
    "two variadic externs each get their own signature" in {
      val out = ir("extern f(n: int, ...)\nextern g(p: *u8, ...) -> int\nvar p: *u8 = null\nf(1)\nprint(g(p))")

      out should include("call void (i32, ...) @f(i32 1)")
      out should include regex "call i32 \\(ptr, \\.\\.\\.\\) @g\\(ptr %t\\d+\\)"
      out should include("declare void @f(i32, ...)")
      out should include("declare i32 @g(ptr, ...)")
    }

    // 32- and 64-bit values are already what C would promote them to, and a `char` is an `i32`
    // already, so nothing is inserted for any of them.
    "what is already wide enough is passed as it stands" in {
      val out = ir("extern f(n: int, ...)\nvar a: i64 = 7i64\nvar b: real = 2.5\nf(1, a, b, 'A', 3)")

      out should not include "zext"
      out should not include "fpext"
      out should include regex "call void \\(i32, \\.\\.\\.\\) @f\\(i32 1, i64 %t\\d+, double %t\\d+, i32 65, i32 3\\)"
    }

    "a raw pointer crosses as it stands" in {
      ir("extern f(n: int, ...)\nvar x = 1\nf(1, &x)") should include regex
        "call void \\(i32, \\.\\.\\.\\) @f\\(i32 1, ptr %[\\w.]+\\)"
    }

    // `printf` was the old `print` builtin's whole implementation, declared unconditionally. It is
    // an ordinary foreign name now: declared where a program asks for it, and nowhere else.
    "printf is declared only by a program that declares it" in {
      val out = ir("extern printf(fmt: *u8, ...) -> int\nvar p: *u8 = null\nprint(printf(p))")

      out.linesIterator.count(l => l.startsWith("declare") && l.contains("@printf(")) shouldBe 1
      out should include("declare i32 @printf(ptr, ...)")
      ir("print(1)") should not include "@printf"
    }

    // `snprintf` is declared only by a program that renders a float or a format hole, so this is
    // the same collision as `printf`'s reached through a condition rather than unconditionally.
    "declaring snprintf does not collide with the runtime's own either" in {
      val src =
        """extern snprintf(buf: *u8, n: usize, fmt: *u8, ...) -> int
          |var buf: [4]u8
          |var fmt: [3]u8 = [37u8, 100u8, 0u8]
          |print(snprintf(&buf[0], 4usize, &fmt[0], 1), str(1.5))""".stripMargin
      val out = ir(src)

      out.linesIterator.count(l => l.startsWith("declare") && l.contains("@snprintf(")) shouldBe 1
      out should include("declare i32 @snprintf(ptr, i64, ptr, ...)")
    }

    "a variadic that does not return still ends its block" in {
      val out = ir("extern die(fmt: *u8, ...) -> never\nvar p: *u8 = null\ndie(p, 1)")

      out should include("call void (ptr, ...) @die(ptr %t1, i32 1)")
      out should include("declare void @die(ptr, ...)")
      out should include("unreachable")
    }
  }

  "the library's forcing combinators" - {
    // They are members of a generic enum, so one exists per element type and no more — and a
    // program that never forces anything pays for none of it, nor for the `exit` they stop with.
    "unwrap is monomorphized once per element type, not once per call" in {
      val src =
        """var a: Option[int] = Some(1)
          |var b: Option[real] = Some(2.5)
          |var c: Option[int] = Some(3)
          |print(a.unwrap(), b.unwrap(), c.unwrap())""".stripMargin
      val out = ir(src)

      // Three calls, two element types.
      out.linesIterator.count(l => l.startsWith("define") && l.contains(s"@${Library.key("Option")}.unwrap")) shouldBe 2
      out should include(s"define i32 @${Library.key("Option")}.unwrap.int(")
      out should include(s"define double @${Library.key("Option")}.unwrap.real(")
      out should include("declare void @exit(i32)")
    }

    "unwrap and expect are separate members, each on demand" in {
      val src =
        """var a: Option[int] = Some(1)
          |print(a.unwrap(), a.expect("here"))""".stripMargin
      val out = ir(src)

      out should include(s"define i32 @${Library.key("Option")}.unwrap.int(")
      out should include(s"define i32 @${Library.key("Option")}.expect.int(")
    }

    "a program that forces nothing carries neither of them" in {
      val out = ir("var a: Option[int] = Some(1)\nprint(a.is_some())")

      out should not include s"@${Library.key("Option")}.unwrap"
      out should not include s"@${Library.key("Option")}.expect"
      out should not include "@exit"
    }
  }

  "a call that does not return" - {
    "ends its block with unreachable" in {
      val out = ir("extern stop() -> never\nstop()")

      out should include("call void @stop()")
      out should include("unreachable")
    }

    "a function whose body diverges needs no return" in {
      val out = ir("extern stop() -> never\nf() -> never = stop()\nf()")

      out should include("define void @f()")
      // The `ret` codegen would otherwise place is dropped: the block is already closed.
      out should not include "ret void\n}"
    }

    // A merge slot is a slot for a *value*, and `never` has none — asking for one would emit an
    // `alloca void`, which is not a type LLVM has.
    "an if whose branches both diverge allocates no slot" in {
      val out = ir("extern stop() -> never\nf(c: bool) -> never\n    if c then stop() else stop()\nf(true)")

      out should not include "alloca void"
      out should include("define void @f(i1 %c.param)")
    }

    "a match whose arms all diverge allocates no slot" in {
      val src =
        """extern stop() -> never
          |f(c: bool) -> never
          |    c match
          |        true -> stop()
          |        false -> stop()
          |f(true)""".stripMargin

      ir(src) should not include "alloca void"
    }

    // The diverging branch stores nothing, so the slot is written only on the paths that arrive.
    "a diverging branch stores nothing into the merge slot" in {
      val out = ir("extern stop() -> never\nvar x = if true then 5 else stop()\nprint(x)")

      out.linesIterator.count(_.contains("store i32 5")) shouldBe 1
      out should include("call void @stop()")
    }
  }

  // A `never`-typed expression must leave its block terminated — that invariant is what lets every
  // consumer of a value ignore the case where none arrives. For a call it falls out of the
  // `unreachable`; for an `if` or `match` whose every path leaves, the merge label has to say so
  // itself, or the `ret` that follows would be emitted with no value to return.
  "a merge nothing reaches" - {
    "an all-returning if ends in unreachable, not a valueless ret" in {
      val out = ir("f(c: bool) -> int\n    if c then return 1 else return 2\nprint(f(true))")

      out should include("ret i32 1")
      out should include("ret i32 2")
      out should include("unreachable")
      out.linesIterator.map(_.trim).toList should not contain "ret i32"
    }

    "an all-returning match does too" in {
      val src =
        """f(o: Option[int]) -> int = o match
          |    Some(v) -> return v
          |    None -> return -1
          |print(f(Some(1)))""".stripMargin
      val out = ir(src)

      out.linesIterator.map(_.trim).toList should not contain "ret i32"
      out should include("unreachable")
    }

    "a loop whose break and else both diverge ends the same way" in {
      val src =
        """extern stop() -> never
          |f(c: bool)
          |    while c
          |        break stop()
          |    else stop()
          |f(true)""".stripMargin
      val out = ir(src)

      out should include("call void @stop()")
      out.linesIterator.map(_.trim).toList should not contain "store void"
    }
  }
}
