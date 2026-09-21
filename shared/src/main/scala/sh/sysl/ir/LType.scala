package sh.sysl
package ir

/** **An LLVM type, as data.** The eight shapes the back end lowers a sysl type to, and the one
 * function that writes one down.
 *
 * Every LLVM type text the compiler emits comes from `render` here. Nothing else concatenates one:
 * a type built by interpolation is a type nothing can read back, which is the whole reason this
 * exists — a second back end consuming what codegen produced needs the shape rather than the
 * characters, and a shape that is only ever a `String` is one it would have to parse.
 *
 * **It is deliberately not `sh.sysl.Type`.** A sysl type carries what the *language* knows — a
 * `Constrained` subtype, a `Volatile` qualifier, the difference between a `*T` and a `&T`, a type
 * parameter that has no representation at all — and lowering is precisely the step that erases all
 * of it. Handing a consumer the sysl type would hand back the information the compiler had just
 * finished discarding, and leave it to re-derive an erasure that had already been performed
 * correctly once. What a back end wants is what LLVM's own type system says, which is this.
 *
 * **Widths are resolved, never symbolic.** A view's length is a `usize`, so its third member is
 * `i64` on one machine and `i32` on another — and that answer is settled when the type is *built*,
 * from the `Word` the emitter was given (`Emitter`'s `given Word`, which has no default on purpose).
 * There is no node here standing for "whatever a pointer is": a target-less type would be a type
 * nothing could lay out, and making one unrepresentable is cheaper than checking for one.
 */
enum LType {

  /** An integer of any width — `i1` for a `bool`, `i32` for a `char`, `i5` for a bitfield's
   * container, and whatever a `usize` came out as.
   */
  case I(bits: Int)

  /** A binary float. LLVM names these rather than numbering them, which is the one place its type
   * text is not systematic.
   *
   * `brain` picks `bfloat` out of the two sixteen-bit formats. It is not an IEEE binary16 — it is
   * binary32 with the low sixteen bits of the significand dropped — so LLVM gives it a name of its
   * own and no width distinguishes the pair.
   */
  case F(bits: Int, brain: Boolean = false)

  /** An address. Opaque, as LLVM's pointers have been since it stopped typing them: what is at the
   * far end is said by the instruction that reaches through, not by the pointer.
   */
  case Ptr

  /** No value at all — what a function returning nothing is declared as, and what a zero-sized
   * type lowers to.
   */
  case Void

  /** `[N x T]`, which is an array and also how a `va_list`'s reserved bytes are spelled. */
  case Arr(length: Int, elem: LType)

  /** `<N x T>`, which is an array whose arithmetic happens in every lane at once.
   *
   * **The difference from `Arr` is what the instructions do, not what the bytes are.** LLVM's
   * arithmetic mnemonics are the same for both widths of the word — `fadd <4 x float>` is the same
   * opcode as `fadd float` — so a vector needs no operation of its own anywhere in the emitter, and
   * an array needs no lane semantics. Two shapes that render one bracket pair apart is the whole of
   * the distinction, and it is exactly the distinction the language draws between `[N]T` and `<N>T`.
   *
   * **A machine with no vector unit is not a special case.** LLVM legalizes: `<4 x float>` becomes
   * four scalar operations where there is nothing wider to put them in, so this lowers on a Cortex-M
   * without the back end being asked whether it should. That is why nothing here, and nothing in
   * `Target`, gates on a vector unit being present.
   *
   * **A C convention names one too**, independently of anything a program wrote: System V calls an
   * eightbyte that two floating members share a `<2 x float>` (`CAbi.floatingChunk`), so this shape
   * is reachable from a struct that has no vector in it at all.
   */
  case Vec(length: Int, elem: LType)

  /** An **anonymous** aggregate — `{ ptr, ptr }` for a trait object, `{ ptr, ptr, i64 }` for a view.
   * A declared struct is `Named` instead: LLVM gives one a name at module level and refers to it by
   * that name everywhere afterwards, which is what keeps a recursive type writable.
   */
  case Struct(fields: List[LType])

  /** A type LLVM knows by name — `%struct.Point`, `%enum.Shape`, `%Shape.Circle`, `%arc.Point`.
   * The sigil is part of the name, because it is part of every use of it.
   */
  case Named(name: String)

  /** The LLVM text for this type, and the only place any of it is written down.
   *
   * The spacing is LLVM's own and is not a matter of taste here: the compiler's tests assert on
   * emitted IR by substring, so `{ ptr, ptr }` with its interior spaces is the answer and
   * `{ptr,ptr}` is a different one.
   */
  def render: String = this match
    case I(bits)         => s"i$bits"
    case F(16, true)     => "bfloat"
    case F(16, _)        => "half"
    case F(32, _)        => "float"
    case F(_, _)         => "double"
    case Ptr             => "ptr"
    case Void            => "void"
    case Arr(n, elem)    => s"[$n x ${elem.render}]"
    case Vec(n, elem)    => s"<$n x ${elem.render}>"
    // An aggregate with nothing in it renders as `{}` rather than as `{  }`, which is what LLVM
    // itself writes. Nothing the compiler lowers produces one — a zero-sized value takes no storage
    // at all — so this is here to be right rather than because anything reaches it.
    case Struct(Nil)     => "{}"
    case Struct(fields)  => fields.map(_.render).mkString("{ ", ", ", " }")
    case Named(name)     => LlvmName.safeSigiled(name)

  /** The suffix LLVM spells an **overloaded intrinsic's** operand type with — `v4f32` in
   * `llvm.vector.reduce.fadd.v4f32`.
   *
   * **It is not `render` with the spaces taken out, which is the mistake worth naming**: a float is
   * `float` in a type and `f32` in a name, so the two spellings genuinely differ and a suffix built
   * by filtering the rendered text produces `4xfloat`, which names no intrinsic. LLVM accepts that
   * silently at the text level and fails in the verifier, one layer further on than the mistake.
   *
   * Only the shapes an intrinsic sysl emits is overloaded on are here. An aggregate has no suffix
   * because no intrinsic the compiler reaches takes one.
   */
  def overloadSuffix: String = this match
    case I(bits)      => s"i$bits"
    case F(16, true)  => "bf16"
    case F(bits, _)   => s"f$bits"
    case Ptr          => "p0"
    case Vec(n, elem) => s"v$n${elem.overloadSuffix}"
    case other        => sys.error(s"no intrinsic overload suffix for ${other.render}")

  override def toString: String = render
}

object LType {

  /** A trait object: the method table for the type it forgot, and the value itself. Two words
   * rather than one, which is the whole of what a `dyn` keyword would have announced.
   */
  val fat: LType = Struct(List(Ptr, Ptr))

  /** A view of elements someone else owns — where they are, where they end, and how many there
   * are. The count is a `usize`, which is why this is the one type in the language whose text
   * depends on the machine and so the one that has to be asked for rather than named.
   */
  def view(using w: Word): LType = Struct(List(Ptr, Ptr, I(w.bits)))
}
