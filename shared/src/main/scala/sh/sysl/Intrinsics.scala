package sh.sysl

import sh.sysl.ir.LType

/** The back end's own operations, reachable from the library as `extern`s.
 *
 * **This is not a new mechanism so much as an opened one.** The compiler has always emitted LLVM
 * intrinsics — the bounds trap, the overflow-checked multiply, the varargs walk, the aggregate copy
 * — but every one of them is wired to a need the compiler has, and nothing written in sysl could
 * reach one. What is here lets the library name one, which is the difference between a square root
 * that is a call into libm and a square root that is an instruction.
 *
 * **It costs no keyword, because LLVM owns the `llvm.` namespace.** A module that defines a symbol
 * beginning `llvm.` is invalid IR, so no C library can export one and a link name in that namespace
 * cannot mean anything else. `extern "llvm.sqrt" sysl_sqrt(x: f64) -> f64` is therefore read as an
 * intrinsic without a second declaration form, and the two kinds of `extern` differ only in who
 * resolves them: the linker, or the back end.
 *
 * **The list is closed, and that is the point.** An intrinsic's signature is LLVM's to change
 * between releases, and a declaration that disagrees with it is not a link error — it is a verifier
 * failure at best and a miscompile at worst. So a program cannot reach an arbitrary one: the base
 * names below are what sysl supports, each checked against the signature it was declared with, and
 * anything else is refused by name. Rust draws the line in the same place, with `core::intrinsics`
 * internal and `f64::sqrt` the surface.
 *
 * **The suffix is derived rather than written.** LLVM overloads an intrinsic on its operand type and
 * spells the choice in the name — `llvm.sqrt.f64`, `llvm.sqrt.f32` — so a declaration that wrote the
 * whole name would be a fact about the types stated twice, and the two could disagree. What is
 * declared is the base, and the width comes from the signature.
 */
object Intrinsics {

  /** The namespace LLVM reserves, and the whole of how an intrinsic is told from a linked symbol.
   *
   * `Llvm` is where it is declared, along with every name the compiler emits on its own account.
   */
  val prefix = Llvm.prefix

  /** What an intrinsic takes: `arity` value parameters, all of one floating type, returning that
   * same type.
   *
   * Every entry has that shape, which is why it is the only one modelled — and this used to predict
   * that the integer operations would make it grow a case, naming `llvm.ctlz` and `llvm.fshl`. They
   * arrived, in `sysl.math`'s `Bits`, and they did **not** come through here.
   *
   * The reason is the one this whole file is organised around, and it is worth stating rather than
   * leaving as an absence. What is validated here is a signature **somebody wrote** — an `extern` in
   * library source, which can disagree with LLVM's and has to be held to it before the verifier is
   * reached. `Bits` has no such declaration to check: its memberships cover an open family of
   * widths, so there was never a finite set of `extern`s, and the calls are built by the compiler
   * from the receiver's own type (`ArithEmitter.intrinsic`). A signature the compiler constructs is
   * checked by every build that runs one, which is a stronger guarantee than a table could give and
   * the same one the checked arithmetic and the saturating casts already rely on.
   *
   * So this stays float-shaped, and stays about the library. **The list of `llvm.` names sysl
   * itself emits is `Llvm`**, which is also where the base names below are declared — this file
   * holds what a declaration is checked against, and that file holds what may be named at all.
   */
  private case class Shape(arity: Int)

  /** The supported base names, with what each takes.
   *
   * These are the float operations that lower to an **instruction** on the machines sysl targets;
   * `Llvm` declares them and says which ones are deliberately absent and why. What is added here is
   * the arity, since that is what a written declaration can get wrong.
   */
  private val table: Map[String, (Llvm.Intrinsic, Shape)] = List(
    Llvm.sqrt     -> Shape(1),
    Llvm.fabs     -> Shape(1),
    Llvm.floor    -> Shape(1),
    Llvm.ceil     -> Shape(1),
    Llvm.trunc    -> Shape(1),
    Llvm.round    -> Shape(1),
    Llvm.copysign -> Shape(2),
  ).map((entry, shape) => entry.base -> (entry, shape)).toMap

  /** The supported base names, for a diagnostic to list. */
  def supported: List[String] = table.keys.toList.sorted

  /** Whether a link name asks for an intrinsic rather than a symbol the linker will find. */
  def declared(symbol: String): Boolean = symbol.startsWith(prefix)

  /** The name to emit, or why the declaration cannot be honoured.
   *
   * The checks are the ones that would otherwise be found by the LLVM verifier, where the message
   * names generated IR rather than the line someone wrote.
   */
  def resolve(base: String, params: List[Type], ret: Type): Either[String, String] =
    table.get(base) match
      case None => Left(s"'$base' is not an intrinsic sysl supports — it has ${supported.mkString(", ")}")
      case Some((entry, shape)) =>
        if params.length != shape.arity then
          Left(s"'$base' takes ${shape.arity} argument${if shape.arity == 1 then "" else "s"}, " +
            s"but ${params.length} were declared")
        else
          (params :+ ret).distinct match
            // The suffix is built by the registry rather than spelled here, so the one place a name
            // is assembled is the one place they are declared.
            case List(Type.Floating(bits, brain)) => Right(entry.at(LType.F(bits, brain)))
            case _                                =>
              Left(s"'$base' takes and returns one floating-point type, and this declaration " +
                s"states ${(params :+ ret).map(Type.show).distinct.mkString(" and ")}")
}
