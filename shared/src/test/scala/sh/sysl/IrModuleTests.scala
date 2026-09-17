package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import ir.{Attr, Global, Inst, LType, Runtime, TypeDef, Val}

/** **Reading a compilation as data** (`ir.Module`), which is the thing this whole representation
 * exists for.
 *
 * Every other codegen suite asserts on the *text* — which is right, because the text is what clang
 * takes and what a released compiler is judged by. None of them can say whether a consumer that is
 * not LLVM could do anything with the result, and until `Codegen.module` existed the answer was no:
 * the only way out was a `String`, so a second back end would have had to parse the compiler's own
 * output back into the shapes the compiler had just finished deciding.
 *
 * **So nothing here renders anything.** An assertion that went through `render` would be the
 * text-tier suites again with more steps, and would pass just as well if the model underneath were
 * one long string. What is asked instead is what a back end asks: which functions are there, what is
 * in their blocks, what the globals hold, what the module is for.
 */
class IrModuleTests extends AnyFreeSpec with Matchers {

  private val i32 = LType.I(32)

  private def module(src: String, target: Target = Target.default): ir.Module =
    modules(List(Source("<input>", src)), target)

  /** Several files, so that a question about **module** storage can be asked at all: a `val` written
   * at the top of a headerless program is a local of the entry point, and a section is a region of
   * the image rather than of a frame.
   */
  private def modules(sources: List[Source], target: Target = Target.default): ir.Module =
    Compiler.compiled(sources, target) match
      case Right(c) => c.module
      case Left(e)  => fail(e)

  private def find(m: ir.Module, name: String): ir.Func =
    m.funcs.find(_.sig.name == name).getOrElse(fail(s"no function '$name' in the module"))

  private def instrs(f: ir.Func): List[Inst] = f.blocks.flatMap(b => b.instrs ::: b.terminator.toList)

  "the module records the machine it was built for" in {
    module("print(1)\n").triple shouldBe Target.default.triple
    module("print(1)\n", Target.riscv64Freestanding).triple should include("riscv64")
  }

  "a function comes back as its signature and its blocks, with no text anywhere" in {
    val m = module("twice(n: int) -> int = n * 2\nprint(twice(21))\n")
    val f = find(m, "twice")

    f.sig.ty.ret shouldBe i32
    f.sig.ty.params.map(_.ty) shouldBe List(i32)
    f.sig.ty.params.head.name shouldBe Some(Val.Reg("n.param"))

    // The entry block holds the parameter's slot, and the body multiplies what it reads back.
    f.blocks.head.instrs.collectFirst { case a: Inst.Alloca => a.dest } shouldBe Some(Val.Reg("n.addr"))
    instrs(f).collectFirst { case Inst.Bin(_, ir.BinOp.Mul, _, _, b) => b } shouldBe Some(Val.Int(2))
    // Every block ends somewhere, which is the one thing a consumer must not have to work out.
    f.blocks.foreach(b => b.terminator.map(_.terminates) shouldBe Some(true))
  }

  "a call names its callee as a symbol rather than as characters" in {
    val m      = module("twice(n: int) -> int = n * 2\nprint(twice(21))\n")
    // The entry point is its own field rather than one of `funcs`, because a library has none.
    val main   = m.entry.getOrElse(fail("the program has no entry point"))
    val called = instrs(main).collect { case Inst.Call(_, _, Val.Global(n), _, _, _, _, _) => n }

    called should contain("twice")
  }

  "a declared struct is a type definition, and its fields are types" in {
    val m = module("struct Point\n    x: int\n    y: int\n\nvar p = Point(1, 2)\nprint(p.x)\n")

    m.structs should contain(TypeDef("%struct.Point", List(i32, i32)))
  }

  // `@packed` is the one thing a type *definition* says that a use of the same type does not, which
  // is why `TypeDef` writes its own braces instead of borrowing `LType.Struct`'s.
  "and a packed one says so on the definition" in {
    val m = module("@packed\nstruct Raw\n    tag: u8\n    n: u32\n\nvar r = Raw(1, 2)\nprint(r.n)\n")

    m.structs.find(_.name == "%struct.Raw").map(_.packed) shouldBe Some(true)
  }

  /** Module storage, and the `@section` case with it — which is the one place a global says
   * something the type system does not: a linker script gathers it, so nothing in the program reads
   * it and `@llvm.used` is what keeps `-O1` from deleting the object the attribute was written for
   * (`reference/attributes.md § @section("...")`).
   */
  "module storage is a constant expression and carries where the linker is to put it" in {
    val m = modules(List(
      Source("m.sysl", "module m\n\n@section(\".cmd\")\nval marker: int = 7\n", List("m")),
      Source("main.sysl", "print(m.marker)\n")))
    val g = m.globals.find(_.section.contains(".cmd")).getOrElse(fail("no placed global"))

    g.constant shouldBe true
    g.ty shouldBe i32
    g.value shouldBe Some(Val.Int(7))

    m.used.map(_.linkage) shouldBe Some(ir.Linkage.Appending)
    m.used.flatMap(_.value) match
      case Some(Val.Array(refs)) => refs.map(_.value) should contain(Val.Global(g.name))
      case other                 => fail(s"@llvm.used is not an array of symbols: $other")
  }

  /** A zero-filled array is one word, and the one that is not zero-filled is still N of them.
   *
   * **The size is paid entirely in compiling.** Writing `[0; N]` out element by element is correct
   * — LLVM folds an all-zero global straight into a zerofill section, so the binary never grows —
   * and it costs the whole array in the module's text on the way there. A 16 MiB byte array made a
   * 100 MB `.ll`, and the build went from 2.4 s at 1 MiB to 12.6 s at 16 for an output that was
   * identical. Found from `slate`, whose collected heap is exactly this declaration and which was
   * capped at 16 MiB because of it.
   *
   * **Asserted on the model rather than on a size or a duration**, which is what makes it a test
   * rather than a benchmark: `Val.Zero` is the one-word form and `Val.Array` is not, so the claim
   * is about which of the two the global holds and nothing about how long anything took.
   */
  "a zero fill is one word of storage and a non-zero fill is still every element" in {
    def storage(ty: String, init: String, lty: LType, len: Int): Option[Val] =
      modules(List(
        Source("m.sysl", s"module m\n\nvar cells: [$len]$ty = $init\n", List("m")),
        Source("main.sysl", "print(m.cells[0])\n")))
        .globals.find(_.ty == LType.Arr(len, lty)).flatMap(_.value)

    // The whole point: 4096 elements, one word of initializer.
    storage("u8", "[0u8; 4096]", LType.I(8), 4096) shouldBe Some(Val.Zero)

    // An array has no splat form the way a vector does, so a fill of anything else genuinely needs
    // its elements — the test is on the value being zero, not on the shape being a fill.
    storage("u8", "[7u8; 4]", LType.I(8), 4) match
      case Some(Val.Array(elems)) => elems.map(_.value) shouldBe List.fill(4)(Val.Int(7))
      case other                  => fail(s"a non-zero fill collapsed to $other")
  }

  /** A float is zero at bit pattern zero alone, so `-0.0` is a different value and an array of it
   * is an array LLVM has to be told about. The obvious spelling of the optimization — "the literal
   * reads as zero" — gets this one wrong and silently changes what the program starts with.
   */
  "a fill of negative zero is not a zero fill" in {
    val m = modules(List(
      Source("m.sysl", "module m\n\nvar cells: [4]real = [-0.0; 4]\n", List("m")),
      Source("main.sysl", "print(m.cells[0])\n")))

    m.globals.find(_.ty == LType.Arr(4, LType.F(64))).flatMap(_.value) match
      case Some(Val.Zero) => fail("'-0.0' was collapsed to zeroinitializer, which is a different value")
      case Some(Val.Array(elems)) => elems.map(_.value).distinct.length shouldBe 1
      case other                  => fail(s"unexpected initializer: $other")
  }

  /** The interned bytes carry the terminator a C caller reads by, which a `string` — knowing its own
   * length — has never had a use for.
   *
   * **And they are bytes rather than LLVM's escaping of them.** A back end emitting a `.byte`
   * directive wants the numbers; `c"h\\C3\\A9\\00"` is one back end's spelling, produced by
   * `render` and nowhere else.
   */
  "a string literal is the bytes of one, in a global of its own" in {
    val m     = module("print(\"h\u00e9\")\n")
    val bytes = m.globals.collect {
      case Global(_, _, LType.Arr(n, LType.I(8)), Some(Val.Bytes(bs)), _, _, _, _) => (n, bs)
    }
    val utf8  = List[Byte](0x68, 0xc3.toByte, 0xa9.toByte, 0)

    bytes should contain((4, utf8))
    Val.Bytes(utf8).render shouldBe "c\"h\\C3\\A9\\00\""
  }

  /** The `sret` case, which is what the signature model exists for: the attribute names a *type*
   * and the parameter is still a `ptr`, so a consumer reading the two as one string would have to
   * take them apart again.
   */
  "a large result is an out-parameter whose attributes are values" in {
    val m = module("struct Big\n    cells: [64]i64\n    tag: int\n\n" +
                     "make(t: int) -> Big = Big([0; 64], t)\nprint(make(3).tag)\n")
    val f = find(m, "make")
    val out = f.sig.ty.params.head

    out.ty shouldBe LType.Ptr
    out.attrs should contain(Attr.NoAlias)
    out.attrs.collectFirst { case Attr.SRet(t) => t } shouldBe Some(LType.Named("%struct.Big"))
    f.sig.ty.ret shouldBe LType.Void
  }

  /** What `Runtime` is for. A generated helper is a `Func` and a consumer reads it; a hand-written
   * one is LLVM text and all a consumer gets is the name — which is the whole point of it having
   * one, and is what a back end that is not LLVM matches on to supply its own.
   */
  "the runtime is data where it was generated and a name where it was written by hand" in {
    val m         = module("print(\"a\" + \"b\")\n")
    val templates = m.runtime.collect { case Runtime.Template(n, _) => n }

    templates should contain("sysl.str.concat")
    // Every template carries a name that says what it is, so none of them is anonymous text.
    templates.foreach(_ should not be empty)
    // And the generated half is data rather than text, which is the whole of the distinction.
    m.runtime.collect { case Runtime.Emitted(f) => f.sig.name } should not be empty
  }

  "an extern the program calls is declared as a signature like any other" in {
    val m = module("extern f(a: int, b: real, c: bool) -> u8\nprint(f(1, 2.0, true))\n")
    val d = m.declares.find(_.name == "f").getOrElse(fail("no declaration for 'f'"))

    d.ty.ret shouldBe LType.I(8)
    d.ty.params.map(_.ty) shouldBe List(i32, LType.F(64), LType.I(1))
    d.ty.variadic shouldBe false
  }
}
