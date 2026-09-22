package sh.sysl

import scala.collection.mutable

import sh.sysl.ir.LType

/** Every `llvm.` name sysl emits, and the whole of what the compiler asks of LLVM.
 *
 * **This is documentation that happens to be executable, which is the point of it.** A port to a
 * back end that is not LLVM — CRAFT is one — needs to know exactly what the LLVM path assumes, and
 * so does anyone bringing up a target whose LLVM is missing an intrinsic. A page saying so would go
 * stale the first time an emitter reached for a name, because nothing fails when prose is wrong.
 * What is here cannot: an emitter builds its names by asking for an entry, so a name that is not
 * declared below is a name that cannot be emitted.
 *
 * **The namespace is LLVM's, which is what makes the list closable.** A module that defines a
 * symbol beginning `llvm.` is invalid IR, so nothing a C library exports and nothing sysl mangles
 * can land in here, and a name in this namespace is resolved by the back end rather than by the
 * linker.
 *
 * **The suffix is not part of a base name.** LLVM overloads an intrinsic on its operand type and
 * spells the choice in the name — `llvm.sqrt.f64` beside `llvm.sqrt.f32` — so what is declared is
 * the base and the suffix comes from the type at the call. `LType.overloadSuffix` is where that
 * spelling lives, and it is deliberately not `render`: a float is `float` in a type and `f32` in a
 * name.
 *
 * **What this does NOT check is that a rendered name is a real intrinsic at the LLVM in use.** That
 * is the verifier's job and a second table restating it would be one more thing to keep in step.
 * What is pinned here is that the name came from a declared entry.
 *
 * The companion question — which intrinsics a *program* may name through an `extern` — is
 * `Intrinsics`, which validates a signature somebody wrote against the base names declared here.
 */
object Llvm {

  /** The namespace LLVM reserves, and the whole of how one of these is told from a linked symbol. */
  val prefix = "llvm."

  /** One name the compiler emits, with what a back end that is not LLVM has to answer.
   *
   * `base` is LLVM's own base name, with no overload suffix on it; `render` is how a call site
   * completes one.
   */
  case class Intrinsic(base: String, purpose: String) {

    /** The full name, with the overload suffixes this call needs, in LLVM's order. */
    def at(suffixes: LType*): String = (base +: suffixes.map(_.overloadSuffix)).mkString(".")

    /** The full name of an intrinsic that is not overloaded. */
    def name: String = base

    override def toString: String = base
  }

  /** A global or a section name LLVM owns rather than a function anything calls.
   *
   * These are here because they are equally things sysl asks of LLVM and equally things a port has
   * to answer — a back end with no `llvm.used` needs some other way to keep a global its own
   * optimizer cannot see a reference to.
   */
  case class Reserved(name: String, purpose: String) {
    override def toString: String = name
  }

  private val intrinsics = mutable.LinkedHashMap.empty[String, Intrinsic]
  private val reserved   = mutable.LinkedHashMap.empty[String, Reserved]

  private def declare(base: String, purpose: String): Intrinsic = {
    require(base.startsWith(prefix), s"'$base' is not in LLVM's namespace")
    val entry = Intrinsic(base, purpose)
    require(intrinsics.put(base, entry).isEmpty, s"'$base' is declared twice")
    entry
  }

  private def reserve(name: String, purpose: String): Reserved = {
    require(name.startsWith(prefix), s"'$name' is not in LLVM's namespace")
    val entry = Reserved(name, purpose)
    require(reserved.put(name, entry).isEmpty, s"'$name' is reserved twice")
    entry
  }

  // ---- what the compiler reaches for on its own ---------------------------------------------

  /** The end of every runtime check: a bounds test, a `char()` conversion, an allocation whose size
   * overflowed. A back end without one needs an instruction that stops the program without
   * returning — the arm after it is `unreachable`, so anything that could fall through is wrong.
   */
  val trap = declare("llvm.trap", "the runtime-safety stop")

  /** A fact the optimizer may rely on and that nothing evaluates at run time — what a caller is told
   * a callee's `ensure` established, and the whole of how a contract reaches the optimizer
   * (`ContractEmitter.assumeEnsures`).
   *
   * **It is a claim rather than a check, and the check is somewhere else.** The callee traps on the
   * same condition before every return, so what is asserted here is something the program has
   * already established by the time control arrives; this only says it in a form LLVM reads. A back
   * end without an assume drops these entirely and loses nothing but the folding they buy — which is
   * why the caller's own bounds test is still emitted and still correct.
   */
  val assume = declare("llvm.assume", "a fact the optimizer may rely on")

  /** An aggregate copied whole. A back end with no memcpy intrinsic emits the call to libc's, which
   * costs a hosted target nothing and is exactly what a freestanding one cannot do.
   *
   * The suffix is `.p0.p0.i64` and does not vary: both pointers are in the default address space,
   * and the length is 64 bits on every target sysl serves whatever the machine's word is.
   */
  val memcpy = declare("llvm.memcpy", "an aggregate copied whole")

  /** `llvm.memcpy` at the only overload sysl emits, so the two sites that name it — the declaration
   * and the call — cannot spell it differently.
   */
  val memcpyName: String = memcpy.at(LType.Ptr, LType.Ptr, LType.I(64))

  /** The varargs walk. These are the three operations a `...` parameter list is made of, and they
   * are the ones most tied to a machine's calling convention — a port supplies whatever its own ABI
   * document says a variadic callee does, which is rarely anything as tidy as three calls.
   */
  val vaStart = declare("llvm.va_start", "a varargs walk begins")
  val vaEnd   = declare("llvm.va_end", "a varargs walk ends")
  val vaCopy  = declare("llvm.va_copy", "a varargs walk duplicated")

  /** Integer arithmetic that answers the value **and** whether it overflowed, in one aggregate.
   *
   * Two things reach these. A declared range narrow enough to make a result unrepresentable at the
   * base width is checked here rather than after a plain instruction has already wrapped
   * (`ScalarEmitter.checkedArith`), and an allocation's size is computed through the unsigned pair
   * so that a count times an element size cannot silently become a small number
   * (`PlaceEmitter`). A back end without them computes at a wider width and compares.
   *
   * **The width is in the name as well as the signature**, so naming the wrong overload is a call
   * to a function that does not exist rather than a type error anything would catch.
   */
  val overflowOps: Map[(String, Boolean), Intrinsic] =
    (for
      op     <- List("add", "sub", "mul")
      signed <- List(true, false)
    yield (op, signed) -> declare(
      s"$prefix${if signed then "s" else "u"}$op.with.overflow",
      s"a${if signed then " signed" else "n unsigned"} $op that reports its own overflow")).toMap

  /** The overflow-checked form of one operator, by the name `ScalarEmitter` knows it as. */
  def withOverflow(op: String, signed: Boolean): Intrinsic =
    overflowOps.getOrElse((op, signed), sys.error(s"no overflow intrinsic for '$op'"))

  /** Float to integer, saturating: out of range clamps to the target's minimum or maximum and NaN
   * becomes zero.
   *
   * A plain `fptosi`/`fptoui` is **poison** outside the range, and what the hardware then does
   * differs by machine — so a port that lowers these to the bare instruction has made the same
   * program print different numbers on different targets, which is precisely what sysl uses these
   * to rule out.
   *
   * Both operand types are in the name, result first: `llvm.fptosi.sat.i32.f64`.
   */
  val fptosiSat = declare("llvm.fptosi.sat", "float to signed integer, saturating")
  val fptouiSat = declare("llvm.fptoui.sat", "float to unsigned integer, saturating")

  /** The saturating cast to an integer of a given signedness. */
  def fptoiSat(signed: Boolean): Intrinsic = if signed then fptosiSat else fptouiSat

  /** The bit operations behind `sysl.math`'s `Bits`.
   *
   * These are the compiler's own reach for an instruction rather than the library's: `Bits`'
   * membership covers an open family of widths, so there was never a finite set of `extern`s to
   * declare and the calls are built from the receiver's type. A back end without them writes the
   * loops, which is what a C library does for the same operations.
   *
   * `ctlz` and `cttz` take a trailing `i1` saying whether a zero operand is poison; sysl always
   * passes `false`, so zero answers the width — the only answer that is the same number on every
   * target.
   */
  val bitOps: Map[String, Intrinsic] = List(
    "ctpop"      -> "the population count",
    "ctlz"       -> "the leading-zero count",
    "cttz"       -> "the trailing-zero count",
    "bitreverse" -> "the bits in the opposite order",
    "fshl"       -> "a funnel shift left, which is how a rotation is spelled",
    "fshr"       -> "a funnel shift right, which is how a rotation is spelled",
  ).map((op, purpose) => op -> declare(prefix + op, purpose)).toMap

  /** One bit operation, by the base name the emitter knows it as. */
  def bits(op: String): Intrinsic =
    bitOps.getOrElse(op, sys.error(s"no bit intrinsic '$op'"))

  /** A vector folded to one lane — what `.sum()`, `.min()`, `.any()` and their neighbours lower to.
   *
   * The suffix is the **vector's** type, `v4f32` rather than the element's, and `fadd` is the one
   * that also takes a starting accumulator: floating addition is not associative, so the intrinsic
   * makes the caller say what to start from and whether the order may be changed. A back end
   * without these writes the tree, and a scalar one writes the loop.
   */
  val reduceOps: Map[String, Intrinsic] =
    List("add", "fadd", "and", "or", "smin", "smax", "umin", "umax", "fmin", "fmax")
      .map(op => op -> declare(s"${prefix}vector.reduce.$op", s"a vector folded to one lane with '$op'"))
      .toMap

  /** One reduction, by the operation `VectorMethods` chose for the member. */
  def reduce(op: String): Intrinsic =
    reduceOps.getOrElse(op, sys.error(s"no vector reduction '$op'"))

  // ---- what a program may reach through an `extern` -----------------------------------------

  /** The float operations the library names, each of which lowers to an **instruction** on the
   * machines sysl targets.
   *
   * `llvm.sin`, `llvm.exp`, `llvm.log` and `llvm.pow` are deliberately absent: they exist, but no
   * target sysl serves has hardware for them, so each lowers to a call to the libm function of the
   * same name — which is what the library's own `extern` already says, more directly.
   *
   * `Intrinsics` is what holds a declaration to the arity and width these are overloaded on; what
   * is here is the names, so that the single declaration site this file claims to be covers the
   * library's half too.
   */
  val sqrt     = declare("llvm.sqrt", "a square root, as an instruction")
  val fabs     = declare("llvm.fabs", "a magnitude, read off the sign bit")
  val floor    = declare("llvm.floor", "rounded towards negative infinity")
  val ceil     = declare("llvm.ceil", "rounded towards positive infinity")
  val trunc    = declare("llvm.trunc", "rounded towards zero")
  val round    = declare("llvm.round", "rounded to nearest, halves away from zero")
  val copysign = declare("llvm.copysign", "one value's magnitude with another's sign")

  // ---- globals and sections LLVM owns -------------------------------------------------------

  /** The list of functions run before the program's own statements, which is where a module-level
   * `val` with a computed initializer is filled in. A freestanding artifact has no loader to walk
   * it, which is why `build-c` refuses an `@export` that reaches one.
   */
  val globalCtors = reserve("llvm.global_ctors", "the initializers run before the program")

  /** What keeps a global nothing refers to from being deleted. The sections feature emits its items
   * `private` and a program builds at `-O1`, so the pass that removes an unreferenced private
   * global would remove exactly the object the attribute was written for. C answers this the same
   * way, with `__used`.
   */
  val used = reserve("llvm.used", "a global the optimizer may not delete")

  /** The same, minus the linker: `llvm.compiler.used` is dropped before the object is written, so a
   * constant survives the optimizer without leaving a symbol behind for the linker to collide on.
   * A library artifact's metadata is held this way, since every artifact would otherwise define one
   * symbol of the same name.
   */
  val compilerUsed = reserve("llvm.compiler.used", "a global held through the optimizer only")

  /** Where the two lists above are put, and the marker that says they are not program data. */
  val metadataSection = reserve("llvm.metadata", "the section the two lists above sit in")

  // ---- the registry as a list ---------------------------------------------------------------

  /** Every base name the compiler emits a call to, sorted. */
  def callable: List[String] = intrinsics.keys.toList.sorted

  /** Every global and section name LLVM owns that the compiler lays down, sorted. */
  def globals: List[String] = reserved.keys.toList.sorted

  /** Whether a name read out of emitted IR is one this registry declared — either a reserved name
   * exactly, or a declared base with an overload suffix on it.
   *
   * This is the claim the file exists to make checkable: a `llvm.` name in a module sysl produced
   * came from an entry above, so the list is the whole list rather than the part somebody
   * remembered.
   */
  def accounts(name: String): Boolean =
    reserved.contains(name) ||
      intrinsics.keys.exists(base => name == base || name.startsWith(base + "."))
}
