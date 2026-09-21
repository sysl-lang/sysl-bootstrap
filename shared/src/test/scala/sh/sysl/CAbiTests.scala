package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** How an aggregate crosses the boundary to a C function, one convention at a time (`CAbi`).
 *
 * These are declaration tests, and they are exact strings rather than substring checks for a reason:
 * **every expectation here was read off `clang -S -emit-llvm` for the same triple**, from a C file
 * declaring the same shapes. That is what makes them an oracle rather than a record of what sysl
 * happens to do — an ABI is an agreement with code this compiler did not build, so the only test
 * that means anything is one whose expected value came from the other side.
 *
 * They are also the tests that would have caught the miscompile they were written for. LLVM applies
 * no C classification to an aggregate of its own accord — it assigns one register per element — and
 * for a handful of shapes that *coincides* with the convention: a homogeneous floating aggregate on
 * AAPCS64, and any sixteen-byte pair of integers. Every one of those coincidences is here too, so a
 * future change cannot pass by testing only the shapes that were never broken.
 *
 * The run tests at the bottom are the other half: a declaration that compiles and passes the wrong
 * bytes is exactly the failure mode being ruled out, and only a call whose answer is known in
 * advance rules it out.
 */
class CAbiTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** The two declarations a shape gets on one machine: a foreign function that returns it, and one
   * that takes it by value. Both directions matter and they disagree — AAPCS64 hands back a
   * five-byte struct in an `i40` and takes the same struct widened to a whole `i64`.
   */
  private def shape(target: Target, fields: String, before: String = ""): (String, String) = {
    val out = irFor(target,
      s"""$before
         |struct S
         |$fields
         |extern give() -> S
         |extern take(v: S)
         |take(give())""".stripMargin)

    (declaring(out, "give"), declaring(out, "take"))
  }

  /** The same for a type that is not a struct declaration — a view, a tuple, an enum. */
  private def spelled(target: Target, ty: String, before: String = ""): (String, String) = {
    val out = irFor(target,
      s"""$before
         |extern give() -> $ty
         |extern take(v: $ty)
         |take(give())""".stripMargin)

    (declaring(out, "give"), declaring(out, "take"))
  }

  private def declaring(ir: String, symbol: String): String =
    ir.linesIterator.find(l => l.startsWith("declare") && l.contains(s"@$symbol(")).getOrElse(
      fail(s"nothing declares '$symbol' in\n$ir"))

  private val arm     = Target.aarch64MacOS
  private val armLnx  = Target.aarch64Linux
  private val x64     = Target.x86_64Linux
  private val rv      = Target.riscv64Linux
  private val rvBare  = Target.riscv64Freestanding
  private val win     = Target.x86_64Windows
  private val rv32    = Target.riscv32Freestanding
  private val wasm    = Target.wasm32Freestanding
  private val thumb   = Target.thumbFreestanding

  "AAPCS64 packs a small aggregate into whole registers" - {

    // The width a result comes back in is the struct's own, to the bit; the width it goes out in is
    // the register's. That asymmetry is the convention's, and getting it backwards is invisible for
    // any struct whose size happens to be eight.
    "a result is its exact width and an argument is the register's" in {
      shape(arm, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take(i64)")
      shape(arm, "    a: u8\n    b: u8") shouldBe ("declare i16 @give()", "declare void @take(i64)")
      shape(arm, "    a: u8\n    b: u8\n    c: u8") shouldBe ("declare i24 @give()", "declare void @take(i64)")
      shape(arm, "    a: i32") shouldBe ("declare i32 @give()", "declare void @take(i64)")
      shape(arm, "    a: [5]u8") shouldBe ("declare i40 @give()", "declare void @take(i64)")
    }

    // The shape the miscompile was found on: two `i32`s are eight bytes, so both fields travel in
    // one register, and reading the second out of a second register got the caller's own argument.
    "two fields that fit one register travel in one register" in {
      shape(arm, "    a: i32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(arm, "    a: f32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(arm, "    a: i32\n    b: u8") shouldBe ("declare i64 @give()", "declare void @take(i64)")
    }

    // Nine to sixteen bytes is a pair of registers, named as an array of two rather than as two
    // parameters — which is where AAPCS64 and System V part company over the same shape.
    "past one register and up to two it is an array of two" in {
      shape(arm, "    a: i64\n    b: u8") shouldBe ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(arm, "    a: i32\n    b: i32\n    c: i32") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(arm, "    p: *u8\n    n: i64") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(arm, "    a: f64\n    b: i64") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    // This is the coincidence that hid the bug: LLVM's per-element assignment lands on the same two
    // registers the convention asks for, so a sixteen-byte pair of integers always worked. It is
    // here so that a test written only against this shape cannot be mistaken for coverage.
    "a sixteen-byte pair of integers is where the naive answer happens to be right" in {
      shape(arm, "    a: i64\n    b: i64") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    // A homogeneous floating aggregate is decided before size is even looked at, which is why four
    // doubles — thirty-two bytes — are still registers while five are not.
    "an aggregate of up to four members of one floating width goes in floating registers" in {
      shape(arm, "    a: f32") shouldBe ("declare %struct.S @give()", "declare void @take([1 x float])")
      shape(arm, "    a: f32\n    b: f32") shouldBe ("declare %struct.S @give()", "declare void @take([2 x float])")
      shape(arm, "    a: f32\n    b: f32\n    c: f32") shouldBe
        ("declare %struct.S @give()", "declare void @take([3 x float])")
      shape(arm, "    a: f64\n    b: f64") shouldBe ("declare %struct.S @give()", "declare void @take([2 x double])")
      shape(arm, "    a: f64\n    b: f64\n    c: f64\n    d: f64") shouldBe
        ("declare %struct.S @give()", "declare void @take([4 x double])")
    }

    "an array of floats is one, and so is a struct of structs of them" in {
      shape(arm, "    a: [4]f32") shouldBe ("declare %struct.S @give()", "declare void @take([4 x float])")
      shape(arm, "    p: Pair\n    q: Pair", "struct Pair\n    x: f32\n    y: f32\n") shouldBe
        ("declare %struct.S @give()", "declare void @take([4 x float])")
    }

    "a fifth member makes it an ordinary aggregate, and its size is then too much" in {
      shape(arm, "    a: f64\n    b: f64\n    c: f64\n    d: f64\n    e: f64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take(ptr align 8)")
      // Twenty bytes, and aligned to four — the aggregate's own alignment, not the register's.
      shape(arm, "    a: f32\n    b: f32\n    c: f32\n    d: f32\n    e: f32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)", "declare void @take(ptr align 4)")
    }

    "a mixture of two floating widths is never homogeneous" in {
      shape(arm, "    a: f64\n    b: f32") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    "half-width floats are homogeneous like any other one width" in {
      shape(arm, "    a: f16\n    b: f16") shouldBe ("declare %struct.S @give()", "declare void @take([2 x half])")
      shape(arm, "    a: f16\n    b: f16\n    c: f16\n    d: f16") shouldBe
        ("declare %struct.S @give()", "declare void @take([4 x half])")
    }

    /** **And `bf16` is a width of its own for this purpose, though it shares `f16`'s bit count.**
      * Homogeneity is about the members being the same *type*, and these two are not: an aggregate
      * of four `bf16` is four of one thing and passes as an array of them, while one holding both
      * formats is a mixture and falls back exactly as `f64` beside `f32` does above.
      */
    "bf16 is homogeneous on its own and a mixture beside f16" in {
      shape(arm, "    a: bf16\n    b: bf16") shouldBe
        ("declare %struct.S @give()", "declare void @take([2 x bfloat])")
      shape(arm, "    a: bf16\n    b: bf16\n    c: bf16\n    d: bf16") shouldBe
        ("declare %struct.S @give()", "declare void @take([4 x bfloat])")
      // The mixture is asserted against the shape a pair of plain `u16` gets, rather than against
      // whatever text the ABI produces for four bytes. What is being claimed is that these two are
      // not one width — so the aggregate stops being a floating one and travels as ordinary bytes —
      // and putting it this way claims exactly that and nothing about how four bytes are carried.
      shape(arm, "    a: f16\n    b: bf16") shouldBe shape(arm, "    a: u16\n    b: u16")
    }

    // Sixteen bytes is two registers either way; what decides how they are *named* is the
    // alignment. Sixteen-byte alignment asks for one `i128`, which is a register pair; eight-byte
    // alignment asks for two of them.
    "a sixteen-byte aggregate wanting sixteen-byte alignment is one wide integer" in {
      shape(arm, "    a: u128") shouldBe ("declare i128 @give()", "declare void @take(i128)")
      shape(arm, "    a: i64\n    b: i64") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    // An argument every member of which is an address is named in addresses, so that whatever a
    // pointer carries beyond its bits survives the coercion. A *result* is not — the two directions
    // disagree here as they do everywhere else on this convention.
    "an argument made only of addresses is named in addresses" in {
      shape(arm, "    p: *u8") shouldBe ("declare i64 @give()", "declare void @take(ptr)")
      shape(arm, "    p: *u8\n    q: *u8") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x ptr])")
      // One non-address member is enough to end it.
      shape(arm, "    c: u8\n    p: *u8") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(arm, "    d: f64\n    p: *u8") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    "a bool is the byte it occupies, not the bit it is written as" in {
      shape(arm, "    a: bool") shouldBe ("declare i8 @give()", "declare void @take(i64)")
    }

    "past two registers the caller supplies the storage" in {
      shape(arm, "    a: i64\n    b: i64\n    c: i64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take(ptr align 8)")
    }

    // Darwin's variant of AAPCS64 leaves this off and every other AAPCS64 system asks for it: a
    // floating aggregate that runs out of registers and lands on the stack is aligned to eight.
    "away from Darwin a floating aggregate says how far to align it on the stack" in {
      shape(armLnx, "    a: f32\n    b: f32") shouldBe
        ("declare %struct.S @give()", "declare void @take([2 x float] alignstack(8))")
      // And nothing else on the same machine gains the attribute.
      shape(armLnx, "    a: i32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
    }
  }

  "System V classifies one eightbyte at a time" - {

    "a single chunk is the exact width, up to the whole register" in {
      shape(x64, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take(i8)")
      shape(x64, "    a: u8\n    b: u8") shouldBe ("declare i16 @give()", "declare void @take(i16)")
      shape(x64, "    a: u8\n    b: u8\n    c: u8") shouldBe ("declare i24 @give()", "declare void @take(i24)")
      shape(x64, "    a: i32") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(x64, "    a: [5]u8") shouldBe ("declare i40 @give()", "declare void @take(i40)")
      shape(x64, "    a: i32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
    }

    // Two chunks are two *parameters*, where AAPCS64 passed one array of two — the same registers,
    // spelled differently, and a difference no amount of reasoning about the documents would settle.
    "two chunks are two parameters and a literal struct of two" in {
      shape(x64, "    a: i64\n    b: i64") shouldBe ("declare { i64, i64 } @give()", "declare void @take(i64, i64)")
      shape(x64, "    a: i32\n    b: i32\n    c: i32") shouldBe
        ("declare { i64, i32 } @give()", "declare void @take(i64, i32)")
    }

    // A chunk is named after the member that *starts* it when that member is all the chunk carries:
    // the `u8` after an `i64` is an `i8`, not the register it will travel in. Where the chunk holds
    // more than that member there is nothing to name it after and the remaining bytes decide.
    "a chunk carrying one small member is named after it, and otherwise after its bytes" in {
      shape(x64, "    a: i64\n    b: u8") shouldBe ("declare { i64, i8 } @give()", "declare void @take(i64, i8)")
      shape(x64, "    a: i16\n    b: u8") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(x64, "    a: i32\n    b: u8") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(x64, "    a: [13]u8") shouldBe ("declare { i64, i40 } @give()", "declare void @take(i64, i40)")
    }

    // Unlike AAPCS64, System V names an address chunk the same way in both directions.
    "a chunk that is exactly one address is named as one, in either direction" in {
      shape(x64, "    p: *u8") shouldBe ("declare ptr @give()", "declare void @take(ptr)")
      shape(x64, "    p: *u8\n    n: i64") shouldBe ("declare { ptr, i64 } @give()", "declare void @take(ptr, i64)")
      shape(x64, "    p: *u8\n    q: *u8") shouldBe ("declare { ptr, ptr } @give()", "declare void @take(ptr, ptr)")
      shape(x64, "    p: *u8\n    f: f32") shouldBe
        ("declare { ptr, float } @give()", "declare void @take(ptr, float)")
      shape(x64, "    c: u8\n    p: *u8") shouldBe ("declare { i8, ptr } @give()", "declare void @take(i8, ptr)")
    }

    "half-width floats share a chunk as a vector of however many fit" in {
      shape(x64, "    a: f16\n    b: f16") shouldBe
        ("declare <2 x half> @give()", "declare void @take(<2 x half>)")
      shape(x64, "    a: f16\n    b: f16\n    c: f16\n    d: f16") shouldBe
        ("declare <4 x half> @give()", "declare void @take(<4 x half>)")
    }

    // Where AAPCS64 named this `i128`, System V splits it like any other sixteen bytes: the
    // alignment changes nothing here.
    "a wide integer is two chunks like anything else of that size" in {
      shape(x64, "    a: u128") shouldBe ("declare { i64, i64 } @give()", "declare void @take(i64, i64)")
    }

    "a chunk of floating members is a floating register, and two floats share one as a vector" in {
      shape(x64, "    a: f32") shouldBe ("declare float @give()", "declare void @take(float)")
      shape(x64, "    a: f64") shouldBe ("declare double @give()", "declare void @take(double)")
      shape(x64, "    a: f32\n    b: f32") shouldBe
        ("declare <2 x float> @give()", "declare void @take(<2 x float>)")
      shape(x64, "    a: f32\n    b: f32\n    c: f32") shouldBe
        ("declare { <2 x float>, float } @give()", "declare void @take(<2 x float>, float)")
      shape(x64, "    a: [4]f32") shouldBe
        ("declare { <2 x float>, <2 x float> } @give()", "declare void @take(<2 x float>, <2 x float>)")
      shape(x64, "    a: f64\n    b: f64") shouldBe
        ("declare { double, double } @give()", "declare void @take(double, double)")
    }

    "a chunk mixing a float with anything else is an integer register" in {
      shape(x64, "    a: f32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(x64, "    a: i64\n    b: f64") shouldBe
        ("declare { i64, double } @give()", "declare void @take(i64, double)")
      shape(x64, "    a: f64\n    b: f32") shouldBe
        ("declare { double, float } @give()", "declare void @take(double, float)")
    }

    // System V is the one convention where the copy is made on the caller's stack rather than
    // handed over as an address, and `byval` is how LLVM is told to make it.
    "past two chunks the argument goes on the stack and the result into caller storage" in {
      shape(x64, "    a: i64\n    b: i64\n    c: i64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)",
         "declare void @take(ptr byval(%struct.S) align 8)")
      shape(x64, "    a: f64\n    b: f64\n    c: f64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)",
         "declare void @take(ptr byval(%struct.S) align 8)")
    }
  }

  "RISC-V flattens the narrow floating cases" - {

    // **One register is named by the aggregate's own WIDTH and not by the register's**, which is
    // what LLVM 23 changed and sysl followed (card `0339`): `i8`, `i24`, `i32` for one, three and
    // four bytes, where clang 22 said `i64` for all of them. Two registers are unmoved — the pair
    // has always been named in whole ones.
    "everything that fits two registers is named in whole ones" in {
      shape(rv, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take(i8)")
      shape(rv, "    a: u8\n    b: u8\n    c: u8") shouldBe ("declare i24 @give()", "declare void @take(i24)")
      shape(rv, "    a: i32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(rv, "    a: i64\n    b: u8") shouldBe ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(rv, "    a: i64\n    b: i64") shouldBe ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    "one or two floating members are passed as themselves" in {
      shape(rv, "    a: f32") shouldBe ("declare float @give()", "declare void @take(float)")
      shape(rv, "    a: f64") shouldBe ("declare double @give()", "declare void @take(double)")
      shape(rv, "    a: f32\n    b: f32") shouldBe
        ("declare { float, float } @give()", "declare void @take(float, float)")
      shape(rv, "    a: f64\n    b: f64") shouldBe
        ("declare { double, double } @give()", "declare void @take(double, double)")
    }

    "and so is one floating member beside one integer, in either order" in {
      shape(rv, "    a: f32\n    b: i32") shouldBe
        ("declare { float, i32 } @give()", "declare void @take(float, i32)")
      shape(rv, "    a: i32\n    b: f32") shouldBe
        ("declare { i32, float } @give()", "declare void @take(i32, float)")
      shape(rv, "    a: i64\n    b: f64") shouldBe
        ("declare { i64, double } @give()", "declare void @take(i64, double)")
    }

    // Not every eight-byte member counts as the integer case. A pointer beside a float does not
    // flatten, which no reading of "one floating and one integer field" would have predicted — and
    // a struct of nothing but a pointer is an `i64` here, where both other conventions said `ptr`.
    "a pointer beside a float does not flatten, and a pointer is never named as one" in {
      shape(rv, "    p: *u8\n    b: f32") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(rv, "    p: *u8") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(rv, "    p: *u8\n    q: *u8") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    "half-width floats flatten in pairs and no further" in {
      shape(rv, "    a: f16\n    b: f16") shouldBe
        ("declare { half, half } @give()", "declare void @take(half, half)")
      shape(rv, "    a: f16\n    b: f16\n    c: f16\n    d: f16") shouldBe
        ("declare i64 @give()", "declare void @take(i64)")
    }

    "and sixteen-byte alignment names two registers as one wide integer, as it does on AAPCS64" in {
      shape(rv, "    a: u128") shouldBe ("declare i128 @give()", "declare void @take(i128)")
    }

    "a third floating member ends the flattening" in {
      shape(rv, "    a: f32\n    b: f32\n    c: f32") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
      shape(rv, "    a: [4]f32") shouldBe ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    // A bare-metal RISC-V target is not built for the floating extension, so there are no floating
    // registers to flatten into and the size rule is the whole of it.
    "and a bare-metal target flattens nothing, having no floating registers" in {
      shape(rvBare, "    a: f64") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(rvBare, "    a: f32\n    b: f32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(rvBare, "    a: f64\n    b: f64") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }
  }

  /** RV32 is not a second convention. It is the RISC-V rule above with XLEN of four rather than
   * eight, and `CAbi.riscv` takes that width as a parameter — one function, two targets. What is
   * asserted here is that the *rule* survives the substitution: the shapes that were one register
   * and two registers and memory on RV64 are one register and two registers and memory here too,
   * at half the size each.
   *
   * The Hazard3 is RV32IMAC with no F extension, so nothing flattens and the size rule is the whole
   * of it — the same as `rvBare` above, and for the same reason.
   */
  "RV32 is the same rule with a narrower word" - {

    "one word or less is one register, named by the aggregate's own width" in {
      shape(rv32, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take(i8)")
      shape(rv32, "    a: u8\n    b: u8\n    c: u8") shouldBe ("declare i24 @give()", "declare void @take(i24)")
      shape(rv32, "    a: i32") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(rv32, "    p: *u8") shouldBe ("declare i32 @give()", "declare void @take(i32)")
    }

    // Two words is two registers, and how they are *named* is the aggregate's alignment rather than
    // its size: eight bytes aligned to eight is one `i64`, and the same eight bytes aligned to four
    // are two `i32`s. RV64 has the identical asymmetry one width up, where sixteen bytes aligned to
    // sixteen become an `i128`.
    "two words are named by the alignment, not by the size" in {
      shape(rv32, "    a: i32\n    b: i32") shouldBe ("declare [2 x i32] @give()", "declare void @take([2 x i32])")
      shape(rv32, "    p: *u8\n    q: *u8") shouldBe ("declare [2 x i32] @give()", "declare void @take([2 x i32])")
      shape(rv32, "    a: i64") shouldBe ("declare i64 @give()", "declare void @take(i64)")
    }

    "and past two words it is memory, by address in both directions" in {
      shape(rv32, "    a: i64\n    b: u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take(ptr align 8)")
      shape(rv32, "    a: i32\n    b: i32\n    c: i32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)", "declare void @take(ptr align 4)")
    }

    // No floating registers to flatten into, so a float is bytes like any other. `f64` beside `f64`
    // is sixteen bytes here and was two registers on RV64 -- the shape did not change, the word did.
    "a float is bytes, there being no floating registers on this core" in {
      shape(rv32, "    a: f32") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(rv32, "    a: f64") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(rv32, "    a: f32\n    b: f32") shouldBe ("declare [2 x i32] @give()", "declare void @take([2 x i32])")
    }
  }

  /** AAPCS32 is the one convention whose two directions disagree about **memory itself**, which is
   * why `Shape.Split` exists beside `Memory` and `Registers`: a result larger than four bytes goes
   * through `sret` *always*, while an argument goes in registers *at any size*. Sixty-four bytes is
   * `[16 x i32]` going out and an `sret` coming back, and no other convention here does that.
   *
   * A consequence worth stating, because it decides whether a code path is reachable at all:
   * **`Param.Indirect` cannot happen on this target.** Every argument is registers.
   *
   * Measured against clang 21 for `thumbv8m.main-none-eabihf`, not read out of a document.
   */
  "AAPCS32 returns through memory long before it passes through it" - {

    "a result of one word or less is an integer of its own width" in {
      shape(thumb, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take([1 x i32])")
      shape(thumb, "    a: u8\n    b: u8") shouldBe ("declare i16 @give()", "declare void @take([1 x i32])")
      shape(thumb, "    a: u8\n    b: u8\n    c: u8") shouldBe ("declare i32 @give()", "declare void @take([1 x i32])")
      shape(thumb, "    a: i32") shouldBe ("declare i32 @give()", "declare void @take([1 x i32])")
    }

    // Five bytes is already too much to return and nowhere near too much to pass. This is the pair
    // that `Shape.Split` was added for, and reading only the result would say "memory" about a call
    // that puts every byte in a register.
    "and one byte more than that is memory coming back and registers going out" in {
      shape(thumb, "    a: [5]u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)", "declare void @take([2 x i32])")
      shape(thumb, "    a: i32\n    b: i32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)", "declare void @take([2 x i32])")
      shape(thumb, "    p: *u8\n    q: *u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)", "declare void @take([2 x i32])")
    }

    // The register *element* is the aggregate's alignment and not the machine's word, which is the
    // detail no document states: a machine whose registers are four bytes is told `[2 x i64]`,
    // because what LLVM is being given is the shape to copy rather than the registers to use.
    "an eight-aligned aggregate is named in eights, on a machine whose registers are four" in {
      shape(thumb, "    a: i64\n    b: i32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take([2 x i64])")
    }

    // No size at all ends the register case, which is the sentence that separates this convention
    // from every other one here.
    "and an aggregate far past any register file still goes out in registers" in {
      shape(thumb, "    a: [64]u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)", "declare void @take([16 x i32])")
    }

    // An HFA travels as the struct type itself in **both** directions -- the opposite of AAPCS64,
    // which coerces the argument to an array of its element. Same idea, different spelling, and a
    // convention copied from the neighbouring one would be wrong in the direction that is hardest
    // to see.
    "a homogeneous floating aggregate is the struct type, going out and coming back" in {
      shape(thumb, "    a: f32") shouldBe ("declare %struct.S @give()", "declare void @take(%struct.S)")
      shape(thumb, "    a: f32\n    b: f32") shouldBe
        ("declare %struct.S @give()", "declare void @take(%struct.S)")
      shape(thumb, "    a: f64\n    b: f64\n    c: f64\n    d: f64") shouldBe
        ("declare %struct.S @give()", "declare void @take(%struct.S)")
    }
  }

  "the Microsoft convention takes one register's worth or an address" - {

    "an aggregate of exactly one, two, four or eight bytes goes in a register" in {
      shape(win, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take(i8)")
      shape(win, "    a: u8\n    b: u8") shouldBe ("declare i16 @give()", "declare void @take(i16)")
      shape(win, "    a: i32") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(win, "    a: i32\n    b: i32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
    }

    // No floating case at all: two floats are eight bytes, so they go in an integer register, where
    // both other 64-bit conventions would have used the floating ones.
    "floating members get no special treatment" in {
      shape(win, "    a: f32") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(win, "    a: f32\n    b: f32") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(win, "    a: f64\n    b: f64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take(ptr align 8)")
    }

    "and any other size at all goes by address, three bytes as much as thirty" in {
      shape(win, "    a: u8\n    b: u8\n    c: u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)", "declare void @take(ptr align 1)")
      shape(win, "    a: i32\n    b: i32\n    c: i32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)", "declare void @take(ptr align 4)")
      shape(win, "    a: i64\n    b: i64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take(ptr align 8)")
      shape(win, "    a: u128") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 16)", "declare void @take(ptr align 16)")
    }

    // An address gets no special name here either: it is the eight bytes it occupies.
    "an address is eight bytes and nothing more" in {
      shape(win, "    p: *u8") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(win, "    p: *u8\n    q: *u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)", "declare void @take(ptr align 8)")
    }
  }

  /** WebAssembly asks nothing about size, which makes it the one convention here whose table can be
   * stated in a sentence — and the one whose cases are all boundaries of the *same* question rather
   * than of a threshold.
   */
  "WebAssembly unwraps a lone scalar and sends everything else to memory" - {

    "one scalar wrapped in a struct is that scalar, whatever it is" in {
      shape(wasm, "    a: u8") shouldBe ("declare i8 @give()", "declare void @take(i8)")
      shape(wasm, "    a: i32") shouldBe ("declare i32 @give()", "declare void @take(i32)")
      shape(wasm, "    a: i64") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      shape(wasm, "    a: f32") shouldBe ("declare float @give()", "declare void @take(float)")
      shape(wasm, "    a: f64") shouldBe ("declare double @give()", "declare void @take(double)")
    }

    // An address is named as one here, where the Microsoft convention calls it the eight bytes it
    // occupies -- because this convention never asks how many bytes anything is.
    "and an address stays an address" in {
      shape(wasm, "    p: *u8") shouldBe ("declare ptr @give()", "declare void @take(ptr)")
    }

    // The whole of what separates this convention from every other one: eight bytes of two members
    // is a register or two everywhere else and is memory here. There is no threshold to be off by
    // one about, because there is no threshold.
    "two members are memory at any size, which no other convention says" in {
      shape(wasm, "    a: i32\n    b: i32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)",
          "declare void @take(ptr byval(%struct.S) align 4)")
      shape(wasm, "    a: f32\n    b: f32") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 4)",
          "declare void @take(ptr byval(%struct.S) align 4)")
      shape(wasm, "    a: u8\n    b: u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)",
          "declare void @take(ptr byval(%struct.S) align 1)")
    }

    // Four doubles are a homogeneous floating aggregate on AAPCS64 and flatten on RISC-V; here they
    // are four members, so they are memory like any other four.
    "a floating aggregate gets no floating case, there being none in this convention" in {
      shape(wasm, "    a: f64\n    b: f64\n    c: f64\n    d: f64") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 8)",
          "declare void @take(ptr byval(%struct.S) align 8)")
    }

    // The argument is a copy the caller makes, as on System V -- but the alignment rule is the other
    // one. System V floors a stack argument at eight; this states the type's own, so sixty-four bytes
    // of `u8` is `align 1` where System V says `align 8`.
    //
    // Only the *argument* differs. The `sret` alignment is the type's under both conventions, which
    // is the reminder that the floor is a fact about the stack slot a caller writes and not about the
    // aggregate.
    "the caller's copy is aligned to the type and not floored at a word" in {
      shape(wasm, "    a: [64]u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)",
          "declare void @take(ptr byval(%struct.S) align 1)")
      shape(x64, "    a: [64]u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)",
          "declare void @take(ptr byval(%struct.S) align 8)")
    }

    // clang's `isSingleElementStruct` sees through nesting in both directions, so the unwrapping has
    // to as well: a struct of a struct of a double is a double, and a one-element array is its one
    // element.
    "the unwrapping sees through nesting and through a one-element array" in {
      shape(wasm, "    inner: Inner", "struct Inner\n    a: i32\n") shouldBe
        ("declare i32 @give()", "declare void @take(i32)")
      shape(wasm, "    a: [1]f64") shouldBe ("declare double @give()", "declare void @take(double)")
    }

    // And it stops where the padding starts. A five-byte array is five members, not one -- the
    // arithmetic that would make it "one thing" is exactly the size question this convention does
    // not ask.
    "but a longer array is its members, so it is memory" in {
      shape(wasm, "    a: [5]u8") shouldBe
        ("declare void @give(ptr sret(%struct.S) align 1)",
          "declare void @take(ptr byval(%struct.S) align 1)")
    }
  }

  "an aggregate that is not a struct declaration is classified all the same" - {

    // A view is three words, which is past what any of the four conventions puts in registers. This
    // is the one place a view does *not* lower as it does everywhere else, and it is right: a C
    // function declared to take a matching three-word struct is the only one it can reach.
    "a view is three words and so travels by address" in {
      spelled(arm, "[]u8") shouldBe ("declare void @give(ptr sret({ ptr, ptr, i64 }) align 8)",
        "declare void @take(ptr align 8)")
      spelled(x64, "string") shouldBe ("declare void @give(ptr sret({ ptr, ptr, i64 }) align 8)",
        "declare void @take(ptr byval({ ptr, ptr, i64 }) align 8)")
    }

    "a tuple is a struct and is classified as one" in {
      spelled(arm, "(i32, i32)") shouldBe ("declare i64 @give()", "declare void @take(i64)")
      spelled(x64, "(f64, f64)") shouldBe
        ("declare { double, double } @give()", "declare void @take(double, double)")
    }

    // A data enum is a tag and a union, and a union has no member to name a chunk after — so it is
    // classified as the integers it is emitted as, which is what makes it never homogeneous. Both of
    // these are what clang emits for the same tag-and-union struct written in C.
    "a data enum is its tag and the region its payloads share" in {
      spelled(arm, "E", "enum E\n    A(x: f32)\n    B(y: f32)\n") shouldBe
        ("declare i64 @give()", "declare void @take(i64)")
      spelled(x64, "E", "enum E\n    A(x: f32)\n    B(y: f32)\n") shouldBe
        ("declare i64 @give()", "declare void @take(i64)")
      // The tag is alone in the first chunk here, because the payload's alignment pushes it to the
      // second — so the chunk is named after the tag and not after the eight bytes it spans.
      spelled(x64, "E", "enum E\n    A(x: f64)\n    B(y: i64)\n") shouldBe
        ("declare { i32, i64 } @give()", "declare void @take(i32, i64)")
      spelled(arm, "E", "enum E\n    A(x: f64)\n    B(y: i64)\n") shouldBe
        ("declare [2 x i64] @give()", "declare void @take([2 x i64])")
    }

    "a simple enum is its underlying integer and has nothing to classify" in {
      spelled(arm, "Colour", "enum Colour\n    Red\n    Green\n") shouldBe
        ("declare i32 @give()", "declare void @take(i32)")
    }

    // C passes nothing for an empty struct and LLVM gives an empty aggregate no register either, so
    // the two already agree and there is nothing to coerce.
    "an aggregate that occupies nothing is left alone" in {
      shape(arm, "    a: unit") shouldBe ("declare %struct.S @give()", "declare void @take(%struct.S)")
    }

    // An argument past a variadic callee's declared parameters gets the same classification a
    // declared parameter of that type would, which is what C does with one too — so the tail needs
    // no rule of its own.
    "an argument past the declared parameters is classified the same way" in {
      val src =
        """struct P
          |    a: i32
          |    b: i32
          |extern report(fmt: *u8, ...) -> i32
          |var f: u8 = 0
          |print(report(&f, P(1, 2)))""".stripMargin

      irFor(x64, src) should include regex """call i32 \(ptr, \.\.\.\) @report\(ptr %[\w.]+, i64 %[\w.]+\)"""
      irFor(arm, src) should include regex """call i32 \(ptr, \.\.\.\) @report\(ptr %[\w.]+, i64 %[\w.]+\)"""
    }

    "including one that goes in floating registers, or by address" in {
      val hfa =
        """struct D2
          |    a: f64
          |    b: f64
          |extern report(fmt: *u8, ...) -> i32
          |var f: u8 = 0
          |print(report(&f, D2(1.0, 2.0)))""".stripMargin

      irFor(arm, hfa) should include regex """@report\(ptr %[\w.]+, \[2 x double\] %[\w.]+\)"""
      irFor(armLnx, hfa) should include regex """@report\(ptr %[\w.]+, \[2 x double\] alignstack\(8\) %[\w.]+\)"""
      irFor(x64, hfa) should include regex """@report\(ptr %[\w.]+, double %[\w.]+, double %[\w.]+\)"""

      val big =
        """struct S24
          |    a: i64
          |    b: i64
          |    c: i64
          |extern report(fmt: *u8, ...) -> i32
          |var f: u8 = 0
          |print(report(&f, S24(1i64, 2i64, 3i64)))""".stripMargin

      irFor(arm, big) should include regex """@report\(ptr %[\w.]+, ptr align 8 %[\w.]+\)"""
      irFor(x64, big) should include regex """@report\(ptr %[\w.]+, ptr byval\(%struct.S24\) align 8 %[\w.]+\)"""
    }

    // What is still refused there is what has no layout to hand over at all.
    "while something with no layout is refused" in {
      err("""extern report(fmt: *u8, ...) -> i32
            |var f: u8 = 0
            |print(report(&f, ()))""".stripMargin) should include(
        "a unit cannot be passed to '...' — a variadic argument must be an integer, a float, a " +
          "char, a raw pointer, or the address of a function")
    }

    // And a *sysl* variadic callee still refuses one, because it is the callee's own walk that would
    // have to read it back and the walk reads one register at a time.
    "and a sysl function's own tail still refuses an aggregate, for a reason of its own" in {
      err("""total(first: int, ...) -> int
            |    var ap: va_list
            |    va_start(ap)
            |    va_end(ap)
            |    return first
            |struct P
            |    a: i32
            |    b: i32
            |print(total(1, P(2, 3)))""".stripMargin) should include(
        "a P cannot be passed to a sysl function's '...' — a walk over the tail reads back one " +
          "register at a time and an aggregate is not one, where a foreign callee takes it because " +
          "C says which registers it arrives in")
    }

    "a declared parameter in front of the ellipsis is coerced like any other" in {
      val out = irFor(x64,
        """struct P
          |    a: i32
          |    b: i32
          |extern report(p: P, ...) -> i32
          |print(report(P(1, 2)))""".stripMargin)

      out should include("declare i32 @report(i64, ...)")
      out should include regex """call i32 \(i64, \.\.\.\) @report\(i64 %[\w.]+\)"""
    }
  }

  /** The other half: a call whose answer is known in advance. A declaration that compiles and moves
    * the wrong bytes is exactly what is being ruled out, and only running one rules it out.
    */
  "and the bytes arrive where the callee looks for them" - {

    // `div_t` is two `i32`s — eight bytes, one register under AAPCS64 — and reading the remainder
    // out of a second register got `2`, the second argument, still sitting there.
    "a struct of two integers comes back with both fields" in {
      run("""struct div_t
            |    quot: i32
            |    rem: i32
            |extern div(a: i32, b: i32) -> div_t
            |var d = div(7, 2)
            |print(d.quot, d.rem)""".stripMargin) shouldBe "3 1\n"
    }

    // The coincidence, run rather than read: sixteen bytes of integers always worked, and it has to
    // go on working.
    "and so does one of two words, which is where the naive answer was already right" in {
      run("""struct ldiv_t
            |    quot: i64
            |    rem: i64
            |extern ldiv(a: i64, b: i64) -> ldiv_t
            |var d = ldiv(-7i64, 2i64)
            |print(d.quot, d.rem)""".stripMargin) shouldBe "-3 -1\n"
    }

    // C's `double complex` is a struct of two doubles, which is the one by-value aggregate parameter
    // a standard library offers — so it is the only place the *argument* direction can be run at all
    // rather than read off a declaration.
    "a floating aggregate is passed by value and read on the other side" in {
      run("""struct cplx
            |    re: f64
            |    im: f64
            |extern cabs(z: cplx) -> f64
            |print(cabs(cplx(3.0, 4.0)))""".stripMargin) shouldBe "5\n"
    }

    "and one of two floats, which travels in narrower registers" in {
      run("""struct cplxf
            |    re: f32
            |    im: f32
            |extern cabsf(z: cplxf) -> f32
            |print(cabsf(cplxf(3.0f32, 4.0f32)))""".stripMargin) shouldBe "5\n"
    }

    "a floating aggregate comes back by value too" in {
      run("""struct cplx
            |    re: f64
            |    im: f64
            |extern csqrt(z: cplx) -> cplx
            |var r = csqrt(cplx(-4.0, 0.0))
            |print(r.re, r.im)""".stripMargin) shouldBe "0 2\n"
    }

    "and two of them go out while a third comes back" in {
      run("""struct cplx
            |    re: f64
            |    im: f64
            |extern cpow(z: cplx, w: cplx) -> cplx
            |var r = cpow(cplx(2.0, 0.0), cplx(3.0, 0.0))
            |print(r.re, r.im)""".stripMargin) shouldBe "8 0\n"
    }

    // A struct behind a pointer needs no classification at all, which is why it was the workaround
    // while the boundary was broken — and it has to keep working now that it is not.
    "a struct handed over by address is untouched by any of this" in {
      run("""struct timespec
            |    sec: i64
            |    nsec: i64
            |extern clock_gettime(id: i32, t: *timespec) -> int
            |var ts: timespec
            |print(clock_gettime(0, &ts) == 0)""".stripMargin) shouldBe "true\n"
    }

    // Two conversions in one expression, and a result nothing reads: each call gets its own storage,
    // so neither can overwrite the other's bytes before they are handed over.
    "two calls in one expression do not share the storage the conversion needs" in {
      run("""struct div_t
            |    quot: i32
            |    rem: i32
            |extern div(a: i32, b: i32) -> div_t
            |print(div(7, 2).quot, div(9, 2).rem)
            |div(1, 1)""".stripMargin) shouldBe "3 1\n"
    }

    // A zero-sized parameter is dropped from the signature (`reference/types.md § unit and never`),
    // which has to happen *before* the rest are classified or every argument after it lands one
    // register early.
    "a parameter that occupies nothing is dropped from in front of a coerced one" in {
      run("""struct div_t
            |    quot: i32
            |    rem: i32
            |extern "div" divide(nothing: unit, a: i32, b: i32) -> div_t
            |var d = divide((), 7, 2)
            |print(d.quot, d.rem)""".stripMargin) shouldBe "3 1\n"
    }

    "and a call in a loop keeps its storage in the frame rather than growing it" in {
      run("""struct div_t
            |    quot: i32
            |    rem: i32
            |extern div(a: i32, b: i32) -> div_t
            |var total = 0
            |for i in 1..5
            |    total = total + div(i * 10, 3).quot
            |print(total)""".stripMargin) shouldBe "48\n"

      // The slots the conversion needs are hoisted, so the loop body allocates nothing.
      val body = mainOf(ir("""struct div_t
                             |    quot: i32
                             |    rem: i32
                             |extern div(a: i32, b: i32) -> div_t
                             |var total = 0
                             |for i in 1..5
                             |    total = total + div(i * 10, 3).quot""".stripMargin))

      body.linesIterator.count(_.contains("= alloca")) shouldBe
        body.linesIterator.takeWhile(!_.contains("br label")).count(_.contains("= alloca"))
    }
  }

  /** A counted field makes the aggregate no different to the ABI, and it must make no difference to
    * the ownership either: the callee borrows, so the conversion is a copy of the bytes and nothing
    * is retained for it.
    */
  "a counted field changes the ownership not at all" - {

    "a struct holding a string is passed by address and released once, at its own scope" in {
      // Named target: both conventions pass this aggregate in memory, but they *spell* it
      // differently in the IR -- AAPCS64 as a bare `ptr`, SysV x86-64 with `byval` -- so an
      // assertion on the declaration is an assertion about one of them. The ownership half below is
      // ABI-independent and would hold either way; it is the `declare` line that has to name a
      // convention to mean anything.
      val out = irFor(Target.aarch64MacOS,
                      """struct Named
                        |    tag: i32
                        |    name: string
                        |extern take(v: Named)
                        |use()
                        |    var n = Named(7, "hi")
                        |    take(n)
                        |end use
                        |use()""".stripMargin)

      out should include("declare void @take(ptr align 8)")

      val body = defineOf(out, "use")

      body.linesIterator.count(_.contains("@arc.copy.Named")) shouldBe 1
      body.linesIterator.count(_.contains("@arc.dispose.Named")) shouldBe 1
    }
  }

  /** The widening a narrow scalar takes (`CAbi.extension`), for the parts `AbiAgainstClangTests`
   * cannot ask clang about.
   *
   * That suite is the oracle for everything with a C spelling — every width, on every target — and
   * nothing here duplicates it. What is left over is the half of the rule that has no C counterpart
   * to measure against: **a sysl `define` states the extension of its own result**, because widening
   * a result is the callee's obligation and `@export` and `&f` hand this very definition to C. There
   * is no clang output for "what should a sysl function's signature say", so it is written down.
   */
  "a narrow scalar is widened by whoever owes it" - {

    /** The callee's half. Nothing here is exported or address-taken — the attribute is on every
     * definition, because a definition cannot know which of them C will reach and a sysl caller is
     * unharmed by a guarantee it never asked for.
     */
    "a sysl definition states its own result's extension" in {
      val out = irFor(arm,
        """yes() -> bool = true
          |small() -> u8 = 7
          |wide() -> i64 = 7
          |print(if yes() then int(small()) else int(wide()))""".stripMargin)

      defineOf(out, "yes") should include("define zeroext i1 @yes()")
      defineOf(out, "small") should include("define zeroext i8 @small()")
      // A register-width result has nothing owed on it, so nothing is written — an attribute that
      // appeared everywhere would say nothing about where the rule applies.
      defineOf(out, "wide") should include("define i64 @wide()")
    }

    /** The caller's half, which is the reported bug: the extension is at the **call** as well as on
     * the declaration, since it is the call that makes the back end widen the value.
     */
    "a foreign call states its arguments'" in {
      val out = irFor(arm,
        """extern take(v: u8)
          |take(7)""".stripMargin)

      out should include("declare void @take(i8 zeroext)")
      out should include("call void @take(i8 zeroext 7)")
    }

    /** A **simple** enum is its discriminant, so a narrow one is widened by its own annotation's
     * width and signedness rather than as the `int` an enum is assumed to be. This is the
     * `-fshort-enums` shape from the other direction, and reading the width off the enum is what
     * keeps the two agreeing.
     */
    "a narrow simple enum by the width its annotation chose" in {
      val out = irFor(arm,
        """enum Code : u8
          |    Ok
          |    Bad
          |extern take(c: Code)
          |take(Code.Ok)""".stripMargin)

      out should include("declare void @take(i8 zeroext)")
    }

    /** AArch64 away from Darwin is the departure that matters most here, because it is the same
     * convention as the host's and answers the opposite way: AAPCS64 leaves the top bits
     * unspecified and makes the callee narrow what it reads. A rule written from the host alone
     * would have put `zeroext` on both.
     */
    /** The variadic tail, where C's own default argument promotion has already been applied — a
      * narrow argument past the declared parameters becomes an `int` before it is handed over, so
      * there is nothing narrow left for a convention to widen. On RISC-V 64 that promoted `int` is
      * still widened, because that is the one target whose rule reaches 32 bits, and the result of a
      * variadic declaration is widened with it.
      *
      * Asserted because the two rules meet here and could each have been applied to the other's
      * value: promoting and *then* widening the original width would put `zeroext` on an `i32`.
      */
    "a promoted variadic argument takes the width it was promoted to" in {
      irFor(arm,
        """extern vf(f: *u8, ...) -> int
          |val b: u8 = 7
          |print(vf(null, b))""".stripMargin) should include("call i32 (ptr, ...) @vf(ptr null, i32")

      val out = irFor(rv,
        """extern vf(f: *u8, ...) -> int
          |val b: u8 = 7
          |print(vf(null, b))""".stripMargin)

      out should include("declare signext i32 @vf(ptr, ...)")
      out should include("i32 signext")
    }

    "and nowhere at all on AArch64 away from Darwin, which widens nothing" in {
      val out = irFor(armLnx,
        """extern take(v: u8)
          |yes() -> bool = true
          |take(7)
          |print(yes())""".stripMargin)

      out should include("declare void @take(i8)")
      defineOf(out, "yes") should include("define i1 @yes()")
    }
  }
}
