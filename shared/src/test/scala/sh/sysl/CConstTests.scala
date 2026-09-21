package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec

/** `c const` — a constant whose value the **C compiler** works out (`reference/ffi.md § A library
 * may carry C`).
 *
 * Every assertion here that reads a number reads one this file does not name. That is deliberate and
 * it is the whole point of the feature: a test asserting `sizeof(long long) == 8` against a literal
 * `8` would be the transcription this exists to abolish, written one layer up. So the assertions are
 * about **agreement** — that sysl's answer is the one C gives, on this machine and on a machine of
 * another width — and about the refusals, which are the half a transcription never had.
 *
 * `AbiAgainstClangTests` is the same shape of test for the same reason, and the cross-target case
 * below borrows its rule about cancelling by name when this machine's clang has no back end for a
 * target rather than passing quietly.
 */
class CConstTests extends AnyFreeSpec with CodegenSupport with RunSupport with ParseSupport {

  "the block parses" - {
    "as its constants, each with a type and a C expression" in {
      prog("c const\n    A: usize = \"sizeof(int)\"\n    B: u32 = \"1 + 1\"\n") shouldBe
        List(CConstBlock(List(
          CConstDecl("A", NamedType("usize"), "sizeof(int)"),
          CConstDecl("B", NamedType("u32"), "1 + 1"))))
    }

    /** The modifier is written on the header and governs the lines under it, which is what a reader
      * would expect of a block and is the only place there is to write one.
      */
    "and a visibility on the header reaches every constant in it" in {
      prog("private c const\n    A: usize = \"sizeof(int)\"\n") shouldBe
        List(CConstBlock(List(
          CConstDecl("A", NamedType("usize"), "sizeof(int)", vis = Visibility.File))))
    }

    /** `c` is contextual, so a program that wants the name keeps it. Nothing else in the language
      * puts a keyword after a name, which is what makes the two words unambiguous together.
      */
    "and 'c' is still an ordinary name" in {
      run("var c = 3\nc += 1\nprint(str(c))\n") shouldBe "4\n"
    }

    "a block with nothing under it is refused with the form" in {
      progError("c const\n") should include("'c const' is followed by its constants")
    }
  }

  "the value comes from the C compiler" - {
    /** The assertion is against C's own answer for the same expression, asked separately — so it
      * pins agreement rather than a number, and stays true on a machine where the number differs.
      */
    "so a 'sizeof' is what a C program would have printed" in {
      run("""@include("<stddef.h>")
            |
            |c const
            |    N: usize = "sizeof(long long) + sizeof(char)"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "9\n"
    }

    "a macro with no symbol behind it arrives as a value" in {
      run("""@include("<limits.h>")
            |
            |c const
            |    BITS: u32 = "CHAR_BIT"
            |
            |print(str(BITS))
            |""".stripMargin) shouldBe "8\n"
    }

    /** The case the feature exists for: a length no call could supply, because a call has no value
      * until the program runs and an array bound is settled before it does.
      */
    "and it may size an array, which is what a function could never do" in {
      run("""@include("<stddef.h>")
            |
            |c const
            |    SIZE: usize = "sizeof(long long)"
            |
            |var xs: [SIZE]u8 = [0; SIZE]
            |
            |xs[0] = 7
            |print(str(xs.len) + " " + str(xs[0]))
            |""".stripMargin) shouldBe "8 7\n"
    }

    "several blocks in one file are all measured" in {
      run("""c const
            |    A: usize = "sizeof(char)"
            |
            |c const
            |    B: usize = "sizeof(short)"
            |
            |print(str(A + B))
            |""".stripMargin) shouldBe "3\n"
    }

    "a negative value survives, because the probe follows the declared type's signedness" in {
      run("""c const
            |    N: i32 = "-3 * 5"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "-15\n"
    }

    "and a value past the signed ceiling does too, for the same reason read the other way" in {
      run("""c const
            |    N: u64 = "0xFFFFFFFFFFFFFFFFull"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "18446744073709551615\n"
    }
  }

  /** **A float macro is measured on the same terms an integer one is**, and the case that decides it
    * is not the plain literal — it is the macro written as an *expression* over other macros, where
    * transcribing means doing the arithmetic by hand and writing down the answer. Every assertion
    * here is agreement between two spellings of one number for the reason the file gives at the top:
    * a decimal written out in this file would be the transcription the feature abolishes.
    */
  "a float is measured the same way" - {
    /** `0.25 * PI` is the shape a physics header is full of, and the half of it a reader cannot
      * check by eye. What is asserted is that C's arithmetic and sysl's agree about the same two
      * numbers, so nothing here depends on what either of them is.
      */
    "an expression over other constants, which is the case hand-copying gets wrong" in {
      run("""c const
            |    WHOLE:   real = "3.14159265359"
            |    QUARTER: real = "0.25 * 3.14159265359"
            |
            |print(QUARTER * 4.0 == WHOLE)
            |""".stripMargin) shouldBe "true\n"
    }

    /** `FLT_EPSILON` is the definition of the width rather than a number about it, so the assertion
      * is the property that defines it: the smallest step above one, and half of it is no step at
      * all. A transcription with a digit wrong passes neither half.
      */
    "a value from <float.h>, asserted by the property that defines it" in {
      run("""@include("<float.h>")
            |
            |c const
            |    EPS: f32 = "FLT_EPSILON"
            |
            |var one: f32 = 1.0
            |
            |print(one + EPS > one, one + EPS / 2.0 == one)
            |""".stripMargin) shouldBe "true true\n"
    }

    /** The narrowing is sysl's, so it has to be the one sysl would do anywhere else — the same value
      * measured at both widths and cast down has to land on the constant measured at the narrow one.
      */
    "a value asked for as 'f32' narrows exactly as a written cast would" in {
      run("""c const
            |    NARROW: f32 = "3.14159265359"
            |    WIDE:   real = "3.14159265359"
            |
            |print(NARROW == f32(WIDE))
            |""".stripMargin) shouldBe "true\n"
    }

    "'f64' and 'real' are the one width under two names" in {
      run("""c const
            |    A: f64  = "1.0 / 3.0"
            |    B: real = "1.0 / 3.0"
            |
            |print(A == B)
            |""".stripMargin) shouldBe "true\n"
    }

    /** The cast in the probe is C's, so an integer expression asked for as a float converts rather
      * than being refused — which is what a header full of `#define SCALE 2` mixed in among the
      * fractional ones needs.
      */
    "an integer expression asked for as a float converts" in {
      run("""@include("<limits.h>")
            |
            |c const
            |    BITS: real = "CHAR_BIT"
            |
            |print(BITS / 2.0)
            |""".stripMargin) shouldBe "4\n"
    }

    /** `reference/errors.md § Constrained types` again, on the other carrier: a transparent subtype
      * *is* its base, so a float constant may be declared at one and the `within` bound is checked
      * against the measured number.
      */
    "the type may be a transparent subtype of a float, bound and all" in {
      run("""type Fraction = f32 within 0.0..1.0
            |
            |c const
            |    HALF: Fraction = "1.0 / 2.0"
            |
            |print(HALF)
            |""".stripMargin) shouldBe "0.5\n"
    }

    /** **The two carriers share one probe and one numbering**, so a block mixing them is where an
      * off-by-one between the two sets of globals would show — and it would show as one line
      * reporting another line's number, which reads as a wrong measurement rather than as a shuffle.
      * The integers here are asked for in the same block, out of order, and each is asserted against
      * the other spelling of itself.
      */
    "a block mixing floats and integers keeps each line's own answer" in {
      run("""@include("<limits.h>")
            |
            |c const
            |    HALF:    real  = "0.5"
            |    BITS:    u32   = "CHAR_BIT"
            |    QUARTER: real  = "0.5 / 2.0"
            |    BYTES:   usize = "sizeof(char)"
            |
            |print(HALF == QUARTER * 2.0, BITS == u32(8 * BYTES), QUARTER * 4.0 == 1.0)
            |""".stripMargin) shouldBe "true true true\n"
    }

    "a negative one survives, since the carrier is a signed double either way" in {
      run("""c const
            |    NEG: real = "-1.5"
            |
            |print(NEG + 1.5 == 0.0, NEG < 0.0)
            |""".stripMargin) shouldBe "true true\n"
    }

    "and a bound the measured value misses is refused, as it is for an integer" in {
      err("""type Fraction = f32 within 0.0..1.0
            |
            |c const
            |    OVER: Fraction = "1.5"
            |
            |print(OVER)
            |""".stripMargin) should include("Fraction")
    }

    /** The probe is compiled for the target and never run, which has to hold for a float exactly as
      * it does for a `sizeof` — a freestanding machine has no C library to have printed it with.
      * What is asserted is that the freestanding build and the host build carry the *same* bit
      * pattern, which says the probe answered on both without this file naming what it answered.
      */
    "and it is measured for the target, on a machine with no way to run anything" in {
      val target = Target.named("thumbv7em-freestanding").getOrElse(cancel("no such target"))

      Toolchain.findClang(target).getOrElse(cancel(s"no clang here has a back end for ${target.name}"))

      val src =
        """c const
          |    Q: f32 = "0.25 * 3.14159265359"
          |
          |var x: f32 = Q
          |
          |print(x)
          |""".stripMargin

      val emitted = raw"double (0x[0-9A-Fa-f]+) to float".r

      def measured(t: Target) = emitted.findFirstMatchIn(irFor(t, src)).map(_.group(1))

      measured(target) should not be None
      measured(target) shouldBe measured(Target.default)
    }
  }

  /** **The claim the whole mechanism rests on**: the answer is the *target's*, not this machine's.
    * Nothing runs, so there is nothing here that could have been right by accident — a pointer is
    * four bytes on the 32-bit machines and eight on the 64-bit one, and the array the constant sizes
    * says which the compiler believed.
    */
  "the answer is the target's rather than the host's" - {
    val src =
      """c const
        |    P: usize = "sizeof(void *)"
        |
        |var xs: [P]u8 = [0; P]
        |
        |print(str(xs.len))
        |""".stripMargin

    for (name, width) <- List(Target.default.name -> 8, "thumbv7em-freestanding" -> 4) do
      s"$name sizes the array at $width" in {
        val target = Target.named(name).getOrElse(cancel(s"no target named '$name'"))

        Toolchain.findClang(target).getOrElse(cancel(s"no clang here has a back end for ${target.name}"))

        irFor(target, src) should include(s"[$width x i8]")
      }
  }

  "what it refuses" - {
    /** The C compiler is the judge of what a constant expression is, which is what lets "any C
      * constant expression" be an honest claim rather than a subset somebody maintains. Its refusal
      * is quoted rather than paraphrased.
      */
    "an expression C cannot settle at compile time, in C's own words" in {
      val message = err("""c const
                          |    N: usize = "atoi(\"3\")"
                          |
                          |print(str(N))
                          |""".stripMargin)

      message should include("the C compiler refused")
      message should include("constant")
    }

    "a header that is not there" in {
      err("""@include("no_such_header_at_all.h")
            |
            |c const
            |    N: usize = "sizeof(int)"
            |
            |print(str(N))
            |""".stripMargin) should include("no_such_header_at_all.h")
    }

    /** The transcription error, caught against the real number instead of against a remembered one —
      * which is the version of this mistake that used to ship.
      */
    "a value the declared type cannot hold, naming both ends" in {
      val message = err("""c const
                          |    N: u8 = "sizeof(long long) * 100"
                          |
                          |print(str(N))
                          |""".stripMargin)

      message should include("which 'u8' cannot hold")
      message should include("800")
    }

    "a type that is not a number, since a string from C is not written this way" in {
      err("""c const
            |    S: string = "\"hello\""
            |
            |print(S)
            |""".stripMargin) should include("is not a number")
    }

    /** The two widths a C constant expression is actually written at are `float` and `double`, and
      * `f16` is neither — so it is refused by name rather than reached by a rounding nobody asked
      * for.
      */
    "'f16', which is not a width a C constant is written at" in {
      err("""c const
            |    H: f16 = "0.5"
            |
            |print(H)
            |""".stripMargin) should include("'f16' is not a width a 'c const' is measured at")
    }

    /** `bf16` is refused for the same reason and named for itself. The two sixteen-bit formats are
      * one case to the rule and two to the reader, and a message about `f16` sent to somebody who
      * wrote `bf16` reads as a compiler confusing the two.
      */
    "'bf16', refused the same way and under its own name" in {
      err("""c const
            |    B: bf16 = "0.5"
            |
            |print(B)
            |""".stripMargin) should include("'bf16' is not a width a 'c const' is measured at")
    }

    /** C settles this one and hands back an infinity, so the refusal is sysl's rather than clang's —
      * and it has to be, because an infinity folded into a program is a number nobody wrote.
      */
    "an expression that overflows while C is working it out" in {
      err("""c const
            |    BIG: real = "1e308 * 10"
            |
            |print(BIG)
            |""".stripMargin) should include("a 'c const' carries a finite number")
    }

    "a NaN, which is not a value to carry either" in {
      err("""c const
            |    N: real = "__builtin_nan(\"\")"
            |
            |print(N)
            |""".stripMargin) should include("is not a number here")
    }

    /** The float half of the transcription check: a value that fits the probe's `double` and not the
      * width it was asked for has been thrown away rather than rounded, and the message names it.
      */
    "a value the declared float width turns into an infinity" in {
      val message = err("""c const
                          |    BIG: f32 = "1e300"
                          |
                          |print(BIG)
                          |""".stripMargin)

      message should include("which 'f32' cannot hold")
      message should include("1.0E300")
    }

    "and one it cannot tell from zero, which is the same loss read the other way" in {
      err("""c const
            |    TINY: f32 = "1e-300"
            |
            |print(TINY)
            |""".stripMargin) should include("cannot tell from zero")
    }

    /** Rounding is **not** in that list, deliberately: naming a narrower width is asking for the
      * nearest value in it, which is what C does for `float x = M_PI;`. Refusing it would leave
      * `f32` unable to read the double-typed macros that are most of them.
      */
    "but ordinary rounding is not refused, because that is what naming the width asked for" in {
      run("""c const
            |    THIRD: f32 = "1.0 / 3.0"
            |
            |print(THIRD > 0.333, THIRD < 0.334)
            |""".stripMargin) shouldBe "true true\n"
    }

    "and a block inside a body, which has no file's headers to be compiled against" in {
      err("""f() -> int
            |    c const
            |        N: int = "1"
            |    N
            |
            |print(str(f()))
            |""".stripMargin) should include("'c const' block is declared at the top level")
    }
  }

  /** **The two blocks used as the pair they are** (`reference/ffi.md § A library may carry C`): a
    * typedef whose width the target or a `#define` decides, and the constants that have to be that
    * width. Neither half is any use to a binding without the other — `TickType_t` spelled exactly
    * in a signature and `portMAX_DELAY` spelled `usize` beside it is a package that does not
    * compile on a port where the two disagree.
    *
    * `reference/errors.md § Constrained types` is what makes this a rule holding rather than an
    * exception: without `new` a constrained type *is* its base, so a constant declared at one is a
    * constant declared at an integer.
    */
  "the type may be a transparent subtype of an integer" - {
    "a 'c type' measured beside it, which is what the pair is for" in {
      run("""@include("<stdint.h>")
            |
            |c type
            |    T = "uint32_t"
            |
            |c const
            |    N: T = "42"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "42\n"
    }

    /** The constant is that type rather than a number that happens to fit it, which is the half a
      * `usize` and an `@assert` never bought: it goes where the type goes, with nothing written.
      */
    "and the constant is that type, so it goes where the type goes" in {
      run("""@include("<stdint.h>")
            |
            |c type
            |    T = "uint16_t"
            |
            |c const
            |    N: T = "40"
            |
            |plus_two(x: T) -> T
            |    x + 2
            |
            |print(str(plus_two(N)))
            |""".stripMargin) shouldBe "42\n"
    }

    /** The measured signedness is what the value is read back at, which it has to be: the C side
      * casts through the typedef and the IR prints an `i64` bit pattern either way, so a full-width
      * unsigned answer is a negative number until the measurement says otherwise.
      */
    "a full-width unsigned measurement survives, read at the width C gave" in {
      run("""@include("<stdint.h>")
            |
            |c type
            |    T = "uint64_t"
            |
            |c const
            |    N: T = "0xFFFFFFFFFFFFFFFFull"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "18446744073709551615\n"
    }

    "and a measured signed one keeps a negative value, read the other way" in {
      run("""@include("<stdint.h>")
            |
            |c type
            |    T = "int32_t"
            |
            |c const
            |    N: T = "-3 * 5"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "-15\n"
    }

    /** The transcription error again, one layer up: the range checked is the measured type's, and
      * the refusal quotes the name the reader wrote rather than the integer it turned out to be.
      *
      * **It is also what says the value is not carried through the C type**, which reads as the
      * faithful thing to do and is not: C narrows, so `(uint8_t)800` is `32` and a constant that
      * should have been refused arrives looking like one that fits.
      */
    "a value the measured type cannot hold is refused, naming it as written" in {
      val message = err("""@include("<stdint.h>")
                          |
                          |c type
                          |    T = "uint8_t"
                          |
                          |c const
                          |    N: T = "sizeof(long long) * 100"
                          |
                          |print(str(N))
                          |""".stripMargin)

      message should include("which 'T' cannot hold")
      message should include("800")
    }

    /** A `c type` resolves a `_Bool` and a constant cannot be read as one, so the refusal is the
      * constant's rather than the type declaration's — which is good on its own.
      */
    "a measured type that is not an integer is refused at the constant" in {
      err("""c type
            |    T = "_Bool"
            |
            |c const
            |    N: T = "1"
            |
            |print(str(N))
            |""".stripMargin) should include("is not an integer")
    }

    "a written subtype is followed to its base, and admits a value in range" in {
      run("""type Small = u32 within 0..10
            |
            |c const
            |    N: Small = "3 + 4"
            |
            |print(str(N))
            |""".stripMargin) shouldBe "7\n"
    }

    /** The check a program would otherwise write an `@assert` for, made against a number nobody
      * chose — and made while compiling, because a constant has no run time to be checked at.
      */
    "and refuses one outside the range, naming both ends" in {
      val message = err("""type Small = u32 within 0..10
                          |
                          |c const
                          |    N: Small = "sizeof(long long) * 100"
                          |
                          |print(str(N))
                          |""".stripMargin)

      message should include("does not admit")
      message should include("800")
    }

    /** The two halves composed: a width the C compiler decides, narrowed by a range the program
      * decides. Neither check knows about the other — the width is measured here and the range is
      * folded after the analyzer has run — and a value has to satisfy both.
      */
    "a written subtype over a measured one is held to both" in {
      val source =
        """@include("<stdint.h>")
          |
          |c type
          |    Tick = "uint32_t"
          |
          |type Slow = Tick within 0..1000
          |
          |c const
          |    N: Slow = "%s"
          |
          |print(str(N))
          |""".stripMargin

      run(source.format("500")) shouldBe "500\n"
      err(source.format("sizeof(long long) * 1000")) should include("does not admit")
    }

    "a 'where' predicate is refused, since a folded constant is made nowhere" in {
      err("""type Even = int where value % 2 == 0
            |
            |c const
            |    N: Even = "4"
            |
            |print(str(N))
            |""".stripMargin) should include("'where' predicate")
    }

    "a 'new' type is refused, since reaching one is a written conversion" in {
      err("""type Ticks = new int within 0..100
            |
            |c const
            |    N: Ticks = "4"
            |
            |print(str(N))
            |""".stripMargin) should include("'new' type")
    }

    /** A probe answers for one file's headers, so the type it is declared at is one that file
      * declares. A name from elsewhere is refused by name rather than approximated.
      */
    "a name this file does not declare is refused by name" in {
      err("""c const
            |    N: Nope = "1"
            |
            |print(str(N))
            |""".stripMargin) should include("'Nope' is not a type this file declares")
    }

    "and a name from another module is that same refusal, not a resolution" in {
      errIn(
        ("other", "other.sysl", "module other\n\ntype Small = u32 within 0..10\n"),
        ("", "main.sysl",
          """import other.Small
            |
            |c const
            |    N: Small = "1"
            |
            |print(str(N))
            |""".stripMargin),
      ) should include("is not a type this file declares")
    }
  }

  /** **The motivating shape is a package, not a program**, so the artifact path is the one that has
    * to work. A library is measured when it is *built* and ships the number, which is what lets a
    * program link a binding without the library's headers — and is the only honest arrangement
    * anyway, since an artifact is built for one target and re-measuring it elsewhere would answer a
    * different question under the same name.
    */
  "a library ships the measured value rather than the expression" - {
    val source = List(Source("demo/lib.sysl",
      """module demo
        |@include("<limits.h>")
        |
        |c const
        |    BITS: u32  = "CHAR_BIT"
        |    HALF: real = "1.0 / 2.0"
        |""".stripMargin, List("demo")))

    lazy val trees: List[Program] =
      LibraryArtifact.build(source) match
        case Left(e) => fail(s"the library did not build: $e")
        case Right((_, meta)) =>
          LibraryArtifact.read("demo.syslib", meta, Target.default) match
            case Left(e)            => fail(s"the metadata did not read back: $e")
            case Right((units, _, _)) => units

    "the artifact carries an ordinary constant holding a literal" in {
      trees.flatMap(_.body).collect { case ConstDecl("BITS", _, IntLit(v, _), _) => v } shouldBe
        List(BigInt(8))
    }

    /** A float goes the same way, and it is the one that could have gone wrong quietly: the value
      * crosses the codec as *text*, so a rounding or a reformatting there would come back as a
      * number nobody measured rather than as a failure to decode.
      */
    "a measured float crosses the codec as the same number it was measured as" in {
      trees.flatMap(_.body).collect { case ConstDecl("HALF", _, FloatLit(t, _), _) => t.toDouble }
        .shouldBe(List(0.5))
    }

    /** Compiled against the **decoded** trees, which is the path a package takes: the program never
      * sees `<limits.h>` and never invokes a C compiler, because the question was settled when the
      * library was built.
      */
    "and a program reads it from the decoded trees, with no header of its own" in {
      Compiler.compiledWith(List(Source("main.sysl", "print(str(demo.BITS))\n")), trees) match
        case Left(e)  => fail(s"the program did not compile against the artifact: $e")
        case Right(c) => c.ir should include("main")
    }
  }

  /** The remaining edges, each found by asking what a second occurrence or another machine does. */
  "the edges" - {
    /** `usize` is the target's width, so its range is too — the check reads `Target.word` rather
      * than assuming the host's. A 64-bit value asked for as `usize` is fine here and would not be
      * on the 32-bit machine, which is the answer a transcription cannot give at all.
      */
    "a 'usize' is range-checked at the target's width, not the host's" in {
      val target = Target.named("thumbv7em-freestanding").getOrElse(cancel("no such target"))

      Toolchain.findClang(target).getOrElse(cancel(s"no clang here has a back end for ${target.name}"))

      errFor(target,
        """c const
          |    N: usize = "0x1FFFFFFFFull"
          |
          |print(str(N))
          |""".stripMargin) should include("which 'usize' cannot hold")
    }

    /** Two files of one module are two probes, which is what lets each carry its own headers. A
      * single probe over the module would be compiling a translation unit neither file wrote.
      */
    "each file is measured against its own '@include' lines" in {
      irOf(
        "a.sysl" ->
          """@include("<limits.h>")
            |
            |c const
            |    A: u32 = "CHAR_BIT"
            |""".stripMargin,
        "b.sysl" ->
          """@include("<stddef.h>")
            |
            |c const
            |    B: usize = "sizeof(long long)"
            |
            |print(str(A + u32(B)))
            |""".stripMargin) should include("main")
    }

    /** The two spellings of one declaration, asserted to agree rather than to be any number.
      *
      * A binding puts its measured constants in a `c` sub-module of its own, so a consumer names them
      * qualified — and the qualified spelling was not folding, so a constant reached through its
      * module was refused in the two positions a constant exists for while the same declaration
      * imported unqualified was accepted. What is asserted is the *agreement*, for the reason this
      * whole file asserts agreement: a literal `8` here would be the transcription the feature
      * abolishes.
      *
      * The defect was the folder's and had nothing to do with `c const` — an ordinary `const` reached
      * through its module failed identically, which is what `ConstTests` pins. This is here because a
      * binding is where the qualified spelling is unavoidable, and so where it will next be noticed.
      */
    "a module-qualified one is the same constant as the imported one, in every position" in {
      runIn(
        ("bits", "bits.sysl",
          """module bits
            |@include("<limits.h>")
            |
            |c const
            |    byte_width: int = "CHAR_BIT"
            |""".stripMargin),
        ("", "main.sysl",
          """import bits.byte_width
            |
            |const near: int = byte_width
            |const far: int = bits.byte_width
            |
            |var here: [byte_width]int
            |var there: [bits.byte_width]int
            |
            |print(s"${near == far} ${here.len == there.len}")
            |""".stripMargin),
      ) shouldBe "true true\n"
    }

    /** A header outside the toolchain's own directories is reached by `--include-path`, exactly as
      * one a shim includes is (`SearchPaths`). This is what makes the feature usable against a real
      * C project, whose headers are never on the default path.
      */
    "a header is found through the search paths the build was given" in {
      val dir = createTempDirectory("sysl-cconst-hdr-")

      try
        writeFile(s"$dir/measured.h", "#define MEASURED_WIDTH 37\n")

        val src = Source("<input>",
          """@include("measured.h")
            |
            |c const
            |    W: u32 = "MEASURED_WIDTH"
            |
            |print(str(W))
            |""".stripMargin)

        Compiler.compiledWith(List(src), Nil, paths = SearchPaths(include = List(dir))) match
          case Left(e)  => fail(s"the include path did not reach the probe: $e")
          case Right(c) => c.ir should include("main")

        // And without it, the same program is refused — so the assertion above is about the flag
        // rather than about a header that happened to be findable anyway.
        err(src.text) should include("measured.h")
      finally try deleteFile(s"$dir/measured.h") catch case _: Exception => ()
    }

    /** **A test build is a compilation and needs the same headers.** `sysl test` was the one
      * subcommand that did not pass its `--include-path` directories on to the probe, so a tree whose
      * `@include` names a header outside the toolchain's own path could be run, built and turned into
      * a library, and could not have its tests run — which is exactly the tree the feature exists for,
      * since a package binding a C library is nothing but `c const` over somebody else's headers. It
      * was found in `sysl-lang/freertos`, whose whole surface is measured out of the consumer's kernel
      * headers.
      *
      * The assertion is written per **tier** rather than for the one call that was broken. `test`
      * reaches the probe through `Compiler.compileTests` and an ordinary build through `compiledWith`;
      * a suite pinning only the second is what let this through, and one pinning only the first would
      * let the next through in the other direction.
      */
    "every tier of build finds a header through the search paths, the test build included" in {
      val dir = createTempDirectory("sysl-cconst-tiers-")

      try
        writeFile(s"$dir/tiered.h", "#define TIERED_WIDTH 41\n")

        val text =
          """@include("tiered.h")
            |
            |c const
            |    W: u32 = "TIERED_WIDTH"
            |
            |@test
            |it_measured() = assert(W == 41, "the header said 41")
            |""".stripMargin

        val src   = Source("<input>", text)
        val paths = SearchPaths(include = List(dir))

        Compiler.compileTests(List(src), Nil, paths = paths) match
          case Left(e)           => fail(s"the include path did not reach the test build's probe: $e")
          case Right((c, tests)) =>
            tests.map(_.display) shouldBe List("it_measured")
            c.ir should include("main")

        // The same tree through the ordinary path, so this case says the two *agree* rather than
        // saying only that one of them works.
        Compiler.compiledWith(List(src), Nil, paths = paths) match
          case Left(e)  => fail(s"the include path did not reach an ordinary build's probe: $e")
          case Right(c) => c.ir should include("main")

        // And a test build with no paths is still refused, naming the header — so the case above is
        // about the flag arriving rather than about a header findable without it.
        Compiler.compileTests(List(src), Nil) match
          case Left(e)  => e should include("tiered.h")
          case Right(_) => fail("a header outside the toolchain's path was found with no flag")
      finally try deleteFile(s"$dir/tiered.h") catch case _: Exception => ()
    }

    /** **The case the whole feature was surveyed for.** A package vendors its headers beside its
      * modules and its shim reaches them with a bare `#include "qcbor.h"`, because C resolves a
      * quoted include relative to the file doing the including. The probe is a temporary file
      * somewhere else, so without a `-I` for the module's own directory the header is unreachable —
      * and the packages this exists to serve could not use it.
      */
    "a header vendored beside the module is found with no flag at all" in {
      val dir = createTempDirectory("sysl-cconst-pkg-")
      val src =
        """@include("vendored.h")
          |
          |c const
          |    W: u32 = "VENDORED_WIDTH"
          |
          |print(str(W))
          |""".stripMargin

      try
        writeFile(s"$dir/vendored.h", "#define VENDORED_WIDTH 41\n")

        Compiler.compiledWith(List(Source(s"$dir/lib.sysl", src)), Nil) match
          case Left(e)  => fail(s"a header beside the module was not found: $e")
          case Right(c) => c.ir should include("main")
      finally Project.discard(s"$dir/vendored.h")
    }

    /** Two constants of one name are a duplicate declaration and are reported as one. The lowering
      * used to hand both lines the *same* value, so what got reported was a duplicate of a constant
      * neither line had written.
      */
    "a name declared twice is refused as the duplicate it is" in {
      err("""c const
            |    N: u32 = "1"
            |
            |c const
            |    N: u32 = "2"
            |
            |print(str(N))
            |""".stripMargin) should not be empty
    }
  }

  /** A file that writes none pays nothing, which is what keeps this from being a tax on the
    * programs that will never bind a line of C. There is no observable difference to assert against,
    * so the claim is pinned where it is made: `lower` returns its units untouched.
    */
  "a compilation with no block never asks for a clang" in {
    val units = List(SyslParser.parse(Source("<input>", "print(1)\n"), Target.default).toOption.get)

    CProbe.lower(units, Target.default) shouldBe Right(units)
  }

  /** **A probe is a C compilation, so a file carrying one asks for headers — and a file that also
    * declares what it needs has said which machines it is for.**
    *
    * This is what lets a *library* hold a probe at all. A library is compiled for every target, so
    * without the gate one POSIX module measuring `sizeof(regex_t)` fails every freestanding build,
    * including builds of programs that never name it — `<regex.h>` does not exist for a bare
    * Cortex-M and there is no reason it should.
    *
    * The gate asks `Target.inherentCapabilities` and **not** what the project provides, which is the
    * distinction the whole thing turns on: `package.hocon` defaults every capability to provided, so
    * a freestanding target nominally offers `posix` and gating on that would gate nothing at all.
    */
  "a probe the machine cannot answer" - {
    val bare = Target.named("thumbv7em-freestanding").getOrElse(cancel("no such target"))

    val tree = project(
      ("host", "host.sysl",
        """module host
          |@requires(posix)
          |@include("regex.h")
          |
          |c const
          |    REGEX_SIZE: usize = "sizeof(regex_t)"
          |
          |size() -> usize = REGEX_SIZE
          |""".stripMargin),
      ("", "main.sysl", "print(\"nothing here names host\")\n"),
    )

    /** The case the gate exists for, and the one that used to fail: clang is never asked, because
      * the file said it needs an operating system and this machine has none.
      */
    "is not asked for, on a machine whose header it could not have" in {
      Compiler.compile(tree, bare) match {
        case Right(ir) => ir should include("main")
        case Left(e)   => fail(s"the probe should have been skipped, but:\n$e")
      }
    }

    /** The other half, and the reason this is a gate rather than a suppression: the same tree on a
      * machine that *does* have POSIX is measured exactly as before. A test asserting only the skip
      * would pass against a `c const` that had stopped working everywhere.
      */
    "and is asked for, on a machine that has it" in {
      Toolchain.findClang(Target.default).getOrElse(cancel("no clang here"))

      Compiler.compile(tree, Target.default) match {
        case Right(ir) => ir should include("main")
        case Left(e)   => fail(s"the probe should have been measured here, but:\n$e")
      }
    }

    /** **The header is kept when the body is dropped**, which is what makes the skip invisible to
      * everybody except the module that opted out of this machine. A program that *does* reach it
      * gets the capability diagnostic naming what it needs — not an undefined name, which would send
      * a reader looking for a typo in a module that is exactly where they left it.
      */
    "while a program that reaches it is told what it requires" in {
      val reaching = project(
        ("host", "host.sysl",
          """module host
            |@requires(posix)
            |@include("regex.h")
            |
            |c const
            |    REGEX_SIZE: usize = "sizeof(regex_t)"
            |
            |size() -> usize = REGEX_SIZE
            |""".stripMargin),
        ("", "main.sysl", "@no_posix\n\nprint(str(host.size()))\n"),
      )

      Compiler.compile(reaching, bare) match {
        case Right(out) => fail(s"expected a refusal, got:\n$out")
        case Left(e) =>
          e should include("requires 'posix'")
          e should not include "regex.h"
      }
    }

    /** A file that requires nothing is measured whatever the target, which keeps this a rule about
      * files that opted in rather than a rule about machines. Such a file claims to build anywhere,
      * and one whose header is missing has mis-stated itself.
      */
    "but a file that requires nothing is still measured, and still refused where it cannot be" in {
      val unguarded = project(
        ("host", "host.sysl",
          """module host
            |@include("regex.h")
            |
            |c const
            |    REGEX_SIZE: usize = "sizeof(regex_t)"
            |""".stripMargin),
        ("", "main.sysl", "print(\"nothing here names host\")\n"),
      )

      Compiler.compile(unguarded, bare) match {
        case Right(out) => fail(s"expected a refusal, got:\n$out")
        case Left(e)    => e should include("regex.h")
      }
    }
  }

  /** **Both spellings of a hex floating operand, because this machine's clang only ever writes one
    * of them and the suite above therefore cannot see the other.**
    *
    * LLVM 23 puts an `f` in front of a hex float — `double f0x400921FB54442EEA` for what 22 wrote as
    * `double 0x400921FB54442EEA` — and the bits are identical either way. Reading one and not the
    * other is not a wrong answer but **no** answer: the text parses as neither form, the line is
    * dropped, and every floating `c const` refuses with a diagnostic saying sysl's probe is at
    * fault. It was, for as long as the probe knew one spelling.
    *
    * The cases above cannot cover this. They run the real clang, and which spelling comes back is a
    * property of whichever clang is installed — Apple's writes the old one, so on this machine the
    * whole suite passes against a probe that cannot read a current LLVM's output. Found by moving CI
    * to LLVM 23 (card `0339`), which is a second reason the two versions are one decision.
    */
  "a hex float operand is read at either LLVM's spelling of it, and a decimal one as itself" in {
    val bits = 0x400921FB54442EEAL

    CProbe.floatOf("0x400921FB54442EEA") shouldBe Some(java.lang.Double.longBitsToDouble(bits))
    CProbe.floatOf("f0x400921FB54442EEA") shouldBe Some(java.lang.Double.longBitsToDouble(bits))

    // The trailing comma is what an operand carries when the global states an alignment after it.
    CProbe.floatOf("f0x400921FB54442EEA,") shouldBe Some(java.lang.Double.longBitsToDouble(bits))

    // The short decimal form, which LLVM prints only where it reads back as the same double.
    CProbe.floatOf("5.000000e-01") shouldBe Some(0.5)

    // The top bit is a bit rather than a sign, which is what `parseUnsignedLong` is for: this is a
    // NaN, and reading it as a signed long would refuse the whole line.
    CProbe.floatOf("f0x7FF8000000000000").exists(_.isNaN) shouldBe true
    CProbe.floatOf("0xFFF0000000000000") shouldBe Some(Double.NegativeInfinity)

    // A width this probe never asks for is dropped rather than read at the wrong one -- `0xH` is a
    // half and `0xK` an x86 long double. Dropping is what makes the caller say so.
    CProbe.floatOf("0xH3C00") shouldBe None
    CProbe.floatOf("f0xH3C00") shouldBe None

    // **And LLVM 23 writes a non-finite value as a WORD**, which is the other half of the same
    // change and is easy to miss, since the two cases that reach it are the ones asserting a
    // REFUSAL -- so they stay red either way and it takes reading the message to see that the
    // refusal is the wrong one. sysl declines to carry any of these; what these lines pin is that
    // the value parses, so the diagnostic is about the value rather than about the probe.
    CProbe.floatOf("+inf") shouldBe Some(Double.PositiveInfinity)
    CProbe.floatOf("-inf") shouldBe Some(Double.NegativeInfinity)
    CProbe.floatOf("+qnan").exists(_.isNaN) shouldBe true
    CProbe.floatOf("-snan").exists(_.isNaN) shouldBe true
  }

  /** The two vocabularies for one fact, held to agreeing. `Conditional` names a symbol a source line
    * may test and `Capability` names something a module may require, and both are asking whether the
    * machine is hosted — so a target that answered one way here and the other way there would gate a
    * `c const` and a `#if` differently, which nobody would find by reading either file alone.
    */
  "the machine's own capabilities are the symbols conditional compilation defines" in {
    for target <- Target.all do
      val machine = target.inherentCapabilities
      val symbols = Conditional.defined(target)

      withClue(s"${target.name}: ") {
        machine(Capability.Os) shouldBe symbols("hosted")
        machine(Capability.Posix) shouldBe symbols("posix")
      }
  }
}
