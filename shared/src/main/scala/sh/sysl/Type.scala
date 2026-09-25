package sh.sysl

import ir.LType

/** A sysl type, as resolved by the analyzer: the scalar table of
 * `01-scalar-types-and-operators.md` plus value structs and enums, carrying just enough to
 * drive instruction selection in codegen. It grows toward the memory-mode qualifiers.
 *
 * Generic types are *monomorphic* here: a named type carries the type arguments it was
 * instantiated with, so `Box[int]` and `Box[real]` are two distinct `Struct` values with
 * distinct LLVM names. Nothing in this ADT is ever left with an unresolved type parameter.
 */
sealed trait Type {

  /** The LLVM type this lowers to (`ir.LType`).
   *
   * This is the lowering — the step that erases everything the language knows and the back end must
   * not see. A `Constrained` answers with its base's, a `Volatile` with its inner's, every reference
   * mode with an address, and the four types that have no representation at all throw rather than
   * inventing one.
   */
  def lty(using Word): LType

  /** The same, written down. Every LLVM type text in the compiler comes from here, and none of it
   * is built by concatenation: `lty` is what decides the shape and `LType.render` is what spells
   * it, so the two questions have one answer each rather than one answer between them.
   */
  final def llvm(using Word): String = lty.render
}

object Type extends TypeQueries {

  /** The widest integer the back end lowers, which is LLVM's own `iN` maximum.
   *
   * It is the back end's number rather than one this language chose: LLVM expands arithmetic at any
   * width up to it, division included and with no runtime routine behind it. What a *program*
   * should want is a separate question, and a narrower one.
   */
  val MaxIntegerBits: Int = (1 << 23) - 1

  /** An integer type of any width. Arithmetic wraps at `bits` and never promotes, so the
   * width is part of the type rather than a property of the operation.
   *
   * `pointerWidth` marks `usize` / `isize`. They are pointer-width *by definition* on every
   * target, which makes them distinct types from the fixed-width integer that happens to
   * match on this one — converting between the two is a cast the programmer writes.
   */
  case class Integer(bits: Int, signed: Boolean, pointerWidth: Boolean = false) extends Type {
    def lty(using Word): LType = LType.I(bits)
  }

  /** A binary floating-point type: `f16`, `bf16`, `f32`, `f64`. A closed set, not a family.
   *
   * `brain` marks `bf16`, and it is here rather than in a case of its own because **width alone no
   * longer identifies a format**. `bf16` is sixteen bits like `f16` and divides them differently —
   * eight bits of exponent and seven of significand, which is binary32's range at a quarter of its
   * precision. Every question the rest of the compiler asks a float is a question about its *width*
   * — how many bytes it occupies, whether a conversion widens or narrows, which register class it
   * is passed in — so the flag rides along with the width and only the handful of places that
   * spell the format out have to read it.
   *
   * **The one place the width stops answering is a conversion between the two sixteen-bit
   * formats**, where neither is wider and LLVM has no instruction that reinterprets one as the
   * other; `ScalarEmitter.convert` routes that pair through `float`.
   *
   * `brain` is meaningful only at sixteen bits. Nothing constructs it otherwise.
   */
  case class Floating(bits: Int, brain: Boolean = false) extends Type {
    def lty(using Word): LType = LType.F(bits, brain)
  }

  /** A Unicode scalar value. Layout-compatible with `u32` but not type-compatible: it has
   * equality and ordering and no arithmetic at all, so reaching a codepoint means casting.
   */
  case object Char extends Type { def lty(using Word) = LType.I(32) }

  case object Bool extends Type { def lty(using Word) = LType.I(1) }
  case object Unit extends Type { def lty(using Word) = LType.Void }

  /** The state of a walk through a variadic function's tail (`reference/ffi.md § Variadic
   * functions`) — C's `va_list`.
   *
   * A predeclared type rather than a struct a program could have written, because its layout is the
   * target ABI's: 24 bytes of register-save bookkeeping under x86-64 SysV, 32 under AAPCS64, a bare
   * pointer under Darwin's arm64. Four pointers' worth covers every one of them, and only the
   * prefix the ABI defines is ever touched — over-reserving a stack slot costs nothing, while
   * under-reserving would be silent corruption.
   */
  case object VaList extends Type { def lty(using Word) = LType.Arr(4, LType.Ptr) }

  /** The type of an expression that does not finish — a call to something that never returns, and
   * so the arm of a `match` or `if` that aborts rather than yielding a value.
   *
   * It is the *bottom* type: a `never` stands where any type was asked for, because control never
   * reaches the place the value would have been used. That one rule is the whole of it, and it is
   * what lets `None -> exit(1)` sit beside `Some(v) -> v` and the `match` still have type `T`.
   *
   * It is not `Unknown`. `Unknown` is what a *mistake* leaves behind and never survives a clean
   * compile; `never` is a real type a program declares (`exit(code: int) -> never`) and reasons
   * about. Nothing is ever *of* type `never` at run time — there is no value of it — so it lowers
   * to `void` and takes no slot, no register, and no `phi`.
   */
  case object Never extends Type { def lty(using Word) = LType.Void }

  /** The type of something whose real type could not be worked out, because the thing that would
   * have decided it was already reported as an error.
   *
   * It exists only so the analyzer can keep going after a mistake: a `var` whose initializer
   * failed still binds its name, at this type, so the rest of the function reads as the
   * programmer wrote it instead of dissolving into "undefined name". Nothing of this type ever
   * reaches codegen — a program with an error is never lowered — and touching a value of it
   * raises `Poisoned`, which abandons the statement without reporting a second time.
   */
  case object Unknown extends Type { def lty(using Word) = LType.Void }

  /** A type parameter as the body that declares it sees it: opaque, and licensed to do exactly what
   * `bounds` promise (`reference/generics.md § Bounds`).
   *
   * It exists for the one pass that checks a generic body **at its definition**, where `T` stands
   * for itself rather than for whatever a call site supplied. A value of it may be copied, passed,
   * returned, and stored — the operations every sysl value has — and may additionally call a method
   * one of its bounds declares. Nothing else is licensed.
   *
   * It is a diagnostic type, not a lowered one. Monomorphization is still what emits code, and it
   * never sees this: the pass discards the tree it builds, and `llvm` says so rather than inventing
   * a representation for a type that has none.
   */
  case class Abstract(name: String, bounds: List[Bound]) extends Type {
    def lty(using Word): LType =
      throw new IllegalStateException(s"the type parameter '$name' reached codegen")

    /** Identity is the **name**, and deliberately not the bounds, for the reason `Bound.key` is a
      * string one paragraph down: one parameter has more than one stand-in. Resolving what a
      * declaration asks of `T` needs a `T` to ask it about, and that one cannot carry the bounds
      * being resolved without walking back around forever — so it is built without them, while the
      * `T` a parameter's *type* resolves to carries them all. Both are the same parameter, and
      * anything that compares two types would say otherwise if the bounds were part of the answer.
      */
    override def equals(other: Any): Boolean = other match
      case a: Abstract => a.name == name
      case _           => false

    override def hashCode: Int = name.hashCode
  }

  /** One trait a parameter is bounded by, or one an implementation supplies, with the arguments the
   * trait was applied to: `Show`, `From[int]`.
   *
   * Two bounds are the same promise exactly when they name the same trait at the same arguments, and
   * `key` is what says so. It is a *string* rather than structural equality on purpose: an argument
   * may be a type parameter standing in for itself, whose `Abstract` carries its own bounds along
   * with it, and two spellings of one parameter must still compare equal. The key is also what
   * implementations are filed under, so `From[int]` and `From[real]` are two of them for one type.
   */
  case class Bound(name: String, args: List[Type]) {
    def key: String = qualified(name, args)

    /** The promise as a diagnostic spells it, with the trait's module read back as a dotted path. */
    def show: String = qualified(Modules.show(name), args)

    override def toString: String = key
  }

  /** A trait, in the one position a trait may stand where a type is asked for: behind a memory
   * mode. `*Trait` and `&Trait` are the **trait objects** of `02` — a value whose type has been
   * forgotten, carried with the method table that says what may still be done to it.
   *
   * It is never a type on its own. An erased value has no known size, so there is nothing to lay
   * out, nothing to copy by value, and nothing to return; the sigil is what makes it a pointer,
   * and the trait-ness is what makes that pointer fat. Resolving a bare trait name says so rather
   * than producing one of these.
   */
  case class Trait(name: String, args: List[Type] = Nil, assocs: List[(String, Type)] = Nil) extends Type {

    /** The trait as a bound names it, which is what an implementation is filed under: an object over
     * `From[int]` dispatches through the implementation written for exactly that.
     *
     * The associated types are deliberately **not** part of it. An argument selects between
     * implementations and one of these does not — the subject is what supplies it — so a bound
     * carrying one would be a promise no `impl` is filed under.
     */
    def bound: Bound = Bound(name, args)

    /** What the object fixed the associated type `name` to, where it fixed one. */
    def assoc(name: String): Option[Type] = assocs.collectFirst { case (n, t) if n == name => t }

    def lty(using Word): LType =
      throw new IllegalStateException(s"the trait '$name' reached codegen as a type of its own")
  }

  /** The call trait a callable's type names (`reference/types.md § Function types`), which the
   * library declares one of per arity.
   *
   * The arity is in the name for the reason a tuple's is in its base: one declaration cannot promise
   * a `call` of an arity it does not know, so each arity is its own trait and each is written out.
   * Nothing about that is visible in a program, which spells every one of them `Fn(A, B) -> R`.
   */
  object Fn {

    /** The trait a callable of `n` parameters implements. */
    def base(n: Int): String = s"Fn$n"

    /** How many parameters the widest declared call trait takes. Past this the library has nothing
     * to offer and the diagnostic says so, exactly as it does for a tuple too wide to compare.
     */
    val maxArity = 4

    /** Whether a trait name is one of the call traits, whatever module qualified it. */
    def isCall(name: String): Boolean = Modules.bare(name).matches("""Fn\d+""")

    /** The parameter and result types of an applied call trait, or `None` for any other trait. The
     * arguments are the parameters and then the result, so the split is the last one off the end.
     */
    def parts(name: String, args: List[Type]): Option[(List[Type], Type)] =
      Option.when(isCall(name) && args.nonEmpty)((args.init, args.last))
  }

  /** The layout of a trait object: the method table for the type it forgot, and the value itself.
   * Two words rather than one, which is the whole of what a `dyn` keyword would have announced.
   */
  val fatPointer: String = LType.fat.render

  /** Whether a memory mode points at a trait rather than at a concrete type, and so is fat. */
  def erased(t: Type): Boolean = erasedTrait(t).isDefined

  /** The trait a `*Trait` / `&Trait` dispatches through. */
  def erasedTrait(t: Type): Option[Trait] = t match
    case Ptr(tr: Trait)    => Some(tr)
    case Ref(tr: Trait, _) => Some(tr)
    case _                 => None

  /** `*T` — a bare machine address: no length, no refcount, no checks, and a lifetime the
   * programmer keeps track of. The one unsafe primitive, and the reason it is spelled with a
   * sigil is so a reader can find every place a program takes on C's risks.
   */
  case class Ptr(inner: Type) extends Type {
    def lty(using Word): LType = if inner.isInstanceOf[Trait] then LType.fat else LType.Ptr
  }

  /** `*extern(A, B) -> R` — the address of a function that obeys the machine's C convention, which
   * is the one word a C library means when it says function pointer (`reference/ffi.md § A
   * function's address`).
   *
   * It is its own type rather than `Ptr` of something for the reason `03` gives `*T` its meaning: a
   * raw pointer addresses a *value*, one that can be read through, written through, and measured.
   * There is no value at the end of this one — code is not data the language offers a view of — so
   * the operations `*T` carries would each need an exception. What it can do instead is the one
   * thing an address of code is for: be called, and be handed to whoever asked for it.
   *
   * The signature is part of the type because it is the whole of what makes a call to one safe to
   * emit. Nothing checks it against the function the address actually came from — that is the
   * promise the `*` announces, the same one every raw pointer announces.
   */
  case class CFn(params: List[Type], ret: Type) extends Type {
    def lty(using Word): LType = LType.Ptr

    /** The written spelling, so a debug rendering reads the way the program does. What a diagnostic
     * shows goes through `show`, and what the emitter writes in front of an indirect callee is the
     * *result* type alone — neither reaches this.
     */
    override def toString: String = s"*extern(${params.mkString(", ")}) -> $ret"
  }

  /** `volatile T` — storage whose reads and writes are **effects rather than value computations**
   * (`reference/memory.md § Device memory`).
   *
   * It qualifies the storage, not the value: what comes back out of a `volatile u32` is an ordinary
   * `u32`, so this is stripped the moment a place is projected and never becomes the type of an
   * expression. Where it survives is inside the composites that *name* somebody else's storage — the
   * pointee of a `*T`, an array or view element, a struct field — which is exactly where a program
   * has said the storage is a device's rather than its own.
   *
   * It lowers to nothing at all: `llvm` delegates, so a register block lays out as the plain struct
   * it looks like. What the qualifier changes is the instruction that reaches it, and only that.
   */
  case class Volatile(inner: Type) extends Type {
    def lty(using Word): LType = inner.lty
  }

  /** A type with any `volatile` taken off the front of it — the type of the **value** read out of a
   * place, as against the type of the place itself. Idempotent, and applied without asking.
   */
  def unqualified(t: Type): Type = t match
    case Volatile(inner) => unqualified(inner)
    case other           => other

  /** Whether touching storage of this type is an effect, so the access is emitted exactly as
   * written.
   *
   * It looks **through** an array, a struct, and an enum, because a whole-aggregate copy of a
   * register block reads every register in it and that read is as much an effect as reading one
   * would be. It does **not** look through a `*T` or a `&T`: copying a pointer at device memory
   * touches no device, and what is at the far end is the far end's business.
   */
  def volatileIn(t: Type): Boolean = t match
    case _: Volatile         => true
    case Array(_, elem)      => volatileIn(elem)
    case s: Struct           => s.fields.exists(f => volatileIn(f._2))
    case e: Enum             => e.variants.exists(_.fields.exists(f => volatileIn(f._2)))
    case c: Constrained      => volatileIn(c.base)
    case _                   => false

  /** `&T` — a reference to a reference-counted heap object, and `&sync T` when its refcount is
   * atomic so the reference may cross a concurrency domain. The two are distinct types with no
   * conversion either way: atomicity is fixed when the object is allocated.
   */
  case class Ref(inner: Type, sync: Boolean) extends Type {
    def lty(using Word): LType = if inner.isInstanceOf[Trait] then LType.fat else LType.Ptr
  }

  /** `weak T` — a reference that does not keep its referent alive (`03`).
   *
   * It addresses the same box a `&T` does and is the same width, so a weak edge costs a program
   * nothing beyond the count it takes in the box's third header word. What separates the two is
   * what may be done with one: nothing at all, until `get()` has asked the box whether the object
   * is still there and handed back an `Option[&T]`.
   */
  case class Weak(inner: Type) extends Type {
    def lty(using Word): LType = if inner.isInstanceOf[Trait] then LType.fat else LType.Ptr

    /** The reference this weakens, which is what `get()` yields and what makes one. */
    def strong: Ref = Ref(inner, sync = false)
  }

  /** `[N]T` — N elements of `T`, laid out end to end with no header. An array *is* its
   * elements: copying one copies all of them, and its length is part of its type, which is what
   * lets every index be checked against a constant.
   */
  case class Array(length: Int, elem: Type) extends Type {
    def lty(using Word): LType = LType.Arr(length, elem.lty)
  }

  object Array {

    /** The key an `impl` written for **every** array is filed under, whatever the length — the
     * block whose length is a value parameter (`reference/generics.md § A parameter may stand for a
     * value`), `impl[const N: usize, T: Display] Display for [N]T`.
     *
     * An array filed under it keeps its per-length key as well, and that one is asked first: a block
     * that wrote `[3]T` covers every array of three, and one that wrote `[N]T` covers every array at
     * all, so the first is the more specific of the two. That is "written-out beats a parameter"
     * (`reference/traits.md § override — when the overlap is deliberate`) applied to a length, which is what the length became when it stopped being
     * part of the shape and started being an argument to it.
     */
    val shape: String = "[N]"
  }

  /** `<N>T` — N lanes of `T`, which is an array whose operators work on every lane at once.
   *
   * **The bytes are an array's; the arithmetic is what differs.** `<4>f32` holds the same four
   * floats `[4]f32` does and in the same order, and `a + b` on the first is one instruction
   * computing four sums where on the second it is not an operation at all. That the two type
   * constructors differ by one bracket pair is deliberate: a vector *is* an array that computes
   * lane-wise, and the spelling says so.
   *
   * **The element is a scalar, and the length is what the machine can hold — neither is checked
   * here.** A lane of a struct, of a view or of another vector has no LLVM meaning, and a length of
   * zero has none either; both are refused where the type is *written*, which is where there is a
   * position to report and a spelling to quote. What reaches this constructor has already been held
   * to that.
   *
   * **No target gates it.** LLVM legalizes a vector wider than the machine into several registers
   * and one on a machine with no vector unit into scalars, so `<4>f32` compiles for a Cortex-M as
   * four ordinary FPU operations. A program therefore never has to ask whether it may write one —
   * only, where it cares about speed, how wide to make it, which is what the conditional-compilation
   * symbols answer.
   */
  case class Vector(length: Int, elem: Type) extends Type {
    def lty(using Word): LType = LType.Vec(length, elem.lty)
  }

  object Vector {

    /** Whether `t` may be a lane.
     *
     * LLVM's vectors are of scalars: an integer or a float. `bool` is in because a comparison has to
     * produce a mask — `<N>bool` is LLVM's `<N x i1>` — and `char` is in because it lowers to `i32`
     * like any other integer. Everything else, an aggregate above all, has no lane-wise arithmetic
     * to give and is refused where the type is written.
     *
     * **A constrained lane is allowed and a volatile one is not**, which is not a symmetry anyone
     * would guess and so is decided here rather than by `underlying`. A subtype is a set of values
     * and lays out as its base, so a lane of one is a lane of the base — the same reason arithmetic
     * on a constrained scalar reaches the base's instruction. A `volatile` lane would be asking for
     * per-lane access ordering out of a load that is one instruction for the whole register, which
     * LLVM has no way to give; `volatile <N>T` says the thing that can be honoured, and is spelled
     * the other way round.
     */
    def lanes(t: Type): Boolean = t match
      case c: Constrained                         => lanes(c.base)
      case _: Integer | _: Floating | Bool | Char => true
      case _                                      => false
  }

  /** The argument bound to a **value** parameter (`reference/generics.md § A parameter may stand
   * for a value`) — `3` where the declaration wrote `[const N: usize]`.
   *
   * It is a `Type` because a declaration's parameters are one list, one namespace and one argument
   * position, so the substitution that answers "what is this parameter?" answers for both kinds and
   * is keyed by name for both. Rust models the same thing the same way, as a `GenericArg` that is a
   * type or a const. What is awkward is only that the map's value type is spelled `Type`.
   *
   * Like `Abstract`, it is a **diagnostic and substitution type, never a lowered one**: a value
   * argument is folded into the length or the expression that names it before anything is laid out,
   * so reaching codegen means a substitution was dropped rather than that this needs a
   * representation.
   */
  case class ConstArg(value: BigInt, ty: Type) extends Type {
    def lty(using Word): LType =
      throw new IllegalStateException(s"the value argument '$value' reached codegen")
  }

  /** The list bound to a **type pack** (`reference/generics.md § A parameter may stand for a list
   * of types`) — `int, string` where the declaration wrote `[..A]` and the subject matched a `(int,
   * string)`.
   *
   * A `Type` for the reason `ConstArg` is one: a declaration's parameters are one list keyed by
   * name, whichever kind each of them is, so the substitution answering "what is this parameter?"
   * has to be able to answer with a list. `(..A)` is the only place one may be written, and
   * resolving that spelling against a substitution holding this is what produces the ordinary
   * `Tuple` everything downstream sees.
   *
   * Like `Abstract` and `ConstArg` it is a **substitution type and never a lowered one** — reaching
   * codegen means the tuple it stands for was never formed.
   */
  case class Pack(elems: List[Type]) extends Type {
    def lty(using Word): LType =
      throw new IllegalStateException(s"a type pack of ${elems.length} reached codegen")
  }

  /** A view of elements someone else owns: the reference that keeps the storage alive, the first
   * element, and how many there are. Every view has that same layout, so the element type shows
   * up only in the instructions that reach through it — which is what lets a slice and a string
   * share one implementation.
   */
  sealed trait View extends Type {
    def elem: Type

    /** Three words: where the elements are, where they end, and how many there are. The length is a
     * `usize`, so this is the one LLVM type in the language whose text depends on the machine.
     */
    def lty(using Word): LType = LType.view
  }

  /** `[]T` — a view of any elements at all, and `[]const T` when the elements may not be written
   * through it.
   *
   * The two are one type with a bit rather than two types, because they are one *view*: the same
   * three words, the same instructions to reach through, and the same thing to keep alive. What the
   * bit changes is only what may be *done* with the view, which is why a `[]T` is accepted wherever
   * a `[]const T` is wanted and never the other way round — dropping the ability to write is safe,
   * and inventing it is the hole (`reference/arrays.md § What is still refused`).
   *
   * `string` is the same idea arrived at from the other side and kept separate: it is a read-only
   * view of `u8` *plus* the promise that the bytes are well-formed UTF-8, and it is the promise, not
   * the read-only-ness, that makes it its own type.
   */
  case class Slice(elem: Type, readOnly: Boolean = false) extends View

  object Slice {

    /** The key an `impl` written for **every** slice is filed under — `impl[T: Display] Display for
     * []T`, and the one place both views of a slice answer to the same name.
     *
     * A `[]T` and a `[]const T` share it because the block is written against what a slice *is*, a
     * pointer and a count of `T`, and whether this one may be written through is not part of that
     * (`TraitLookup.shapeOwners`). What the shared key does not mean is a shared *body*: the block
     * is made real at the receiver's own view, so a member reached on a `[]const T` is checked with
     * a `self` it may not write, and one that writes is refused there rather than being missing here.
     */
    val shape: String = "[]"

    /** The name a slice shape block's members take when the block is made real at the **read-only**
     * view — `slice.display` becoming `constslice.display`, which is the rename `mangle` gives the
     * type itself.
     *
     * A shape block's members are emitted under `slice` (`ImplTarget.shapeSymbol`), so this is a
     * prefix rather than a suffix and the two instantiations sort beside their own views' types. It
     * is the identity for anything else, which is what makes it safe to apply without asking: only
     * a slice shape reaches it.
     */
    def constOwner(name: String): String = if name.startsWith("slice.") then s"const$name" else name
  }

  /** A view of bytes that are well-formed UTF-8 and stay that way: the same three words a slice
   * is, minus the ability to write through it. The validity invariant is what separates the two
   * types, so converting a `[]u8` to a `string` is checked and the other direction is free.
   */
  case object Str extends View { def elem: Type = Byte }

  /** Whether a value of this type carries anything a **refcount** has to be touched for: a `&T`, a
   * `weak T`, or a view's owner word — directly, or in a field of a struct or an enum variant.
   *
   * A raw pointer never does; it is the mode that opts out of management. A `&T` is a **leaf** here
   * rather than something to recur into, which is what keeps a recursive type from recurring
   * forever: the reference is itself the thing being asked about.
   *
   * Two passes ask this and they must not be able to disagree, which is why it is here rather than
   * in either of them. `ArcEmitter` asks it to decide whether copying a value emits a retain, and
   * the raw tier asks it to decide whether an address may be read as a pointer to this type at all
   * (`RawStorage.castTarget`) — the assignment through such a pointer is what would release
   * whatever the bytes happened to look like.
   */
  def containsCounted(t: Type): Boolean = t match
    case _: Ref         => true
    case _: Weak        => true
    case _: View        => true
    case Array(_, elem) => containsCounted(elem)
    case s: Struct      => s.fields.exists(f => containsCounted(f._2))
    case e: Enum        => e.variants.exists(_.fields.exists(f => containsCounted(f._2)))
    case _              => false

  /** Whether a view refuses to be written through. Both read-only views answer yes, and they are
   * read-only for different reasons: a `[]const T` views elements somebody else promised not to
   * have written, and a `string` views bytes whose UTF-8 a write is what would break.
   */
  def readOnlyView(t: Type): Boolean = t match
    case Slice(_, ro) => ro
    case Str          => true
    case _            => false

  /** The same view, minus the ability to write through it — the one direction that is safe, and
   * what makes a `[]T` argument acceptable where a `[]const T` is asked for. A view that is already
   * read-only is returned unchanged, so this is idempotent and may be applied without asking.
   */
  def constView(t: Type): Type = t match
    case Slice(e, false) => Slice(e, readOnly = true)
    case other           => other

  /** The element type of whatever a subscript may be applied to. */
  def element(t: Type): Option[Type] = t match
    case Array(_, e) => Some(e)
    // A lane, which is read by `extractelement` rather than by reaching through an address — the
    // one subscript in the language whose subject need never be in memory. What it answers here is
    // only the type; `PlaceEmitter` is where the difference in how it is reached lives.
    case Vector(_, e) => Some(e)
    case v: View     => Some(v.elem)
    // A `*T` is a bare address, so its `i`th element is C's `p[i]` — unchecked, since there is no
    // length in the type to check against (`03`).
    case Ptr(e)      => Some(e)
    case _           => None

  /** The type a `*T` or `&T` points at, for the one level of automatic dereference that field
   * selection performs. A trait object has none: it has forgotten what it points at, which is
   * exactly why its methods are reached through a table instead.
   */
  def pointee(t: Type): Option[Type] = t match
    case _ if erased(t)  => None
    case Ptr(inner)      => Some(inner)
    case Ref(inner, _)   => Some(inner)
    case _               => None

  /** The types an unsuffixed literal falls back to when nothing else fixes it. */
  val Int: Integer   = Integer(32, signed = true)
  val Real: Floating = Floating(64)

  val Byte:  Integer = Integer(8, signed = false)

  /** `usize` and `isize`, which are pointer-width **by definition** — so unlike every other scalar
   * their width is the target's answer rather than a number this file can hold. That is the whole of
   * why they are functions and `byte` is a value.
   */
  def usize(using w: Word): Integer = Integer(w.bits, signed = false, pointerWidth = true)
  def isize(using w: Word): Integer = Integer(w.bits, signed = true, pointerWidth = true)

  /** The scalar type names that are not systematic — the non-numeric primitives and the friendly
   * aliases over the common integer and float widths. The `iN` / `uN` / `fN` spellings are
   * recognised by width instead, so the open integer family needs no table, and the pointer-width
   * pair is added by `scalars` because only it depends on the machine.
   */
  private val fixedScalars: Map[String, Type] = Map(
    "bool"   -> Bool,
    "char"   -> Char,
    "string" -> Str,
    "unit"   -> Unit,
    "int"    -> Integer(32, signed = true),
    "short"  -> Integer(16, signed = true),
    "long"   -> Integer(64, signed = true),
    "byte"   -> Integer(8, signed = false),
    "ushort" -> Integer(16, signed = false),
    "uint"   -> Integer(32, signed = false),
    "ulong"  -> Integer(64, signed = false),
    "real"    -> Floating(64),
    // `bf16` is the one floating-point type whose name is not a width, because it is not a width
    // that tells it apart from `f16`. So it is a name in this table rather than a shape `widthType`
    // recognises, and everything that reaches a scalar by name — a type annotation, a cast, a
    // literal suffix — finds it here.
    "bf16"    -> Floating(16, brain = true),
    "va_list" -> VaList,
  )

  /** Every scalar name, for the machine being compiled for. */
  def scalars(using Word): Map[String, Type] =
    fixedScalars + ("usize" -> usize) + ("isize" -> isize)

  /** The alias a diagnostic prefers when a width has a friendly name. `usize` and `isize` are not
   * here and must not be: on a target where they are the same width as `uint` and `int` they would
   * displace those names for a type that is deliberately distinct from them.
   */
  private val friendly: Map[Type, String] =
    fixedScalars.collect { case (name, t: Integer) => (t, name) } ++ Map(Floating(64) -> "real")

  private def canonicalName(t: Type): String = t match
    case Integer(_, signed, true) => if signed then "isize" else "usize"
    case Integer(bits, signed, _) => (if signed then "i" else "u") + bits
    case Floating(16, true)       => "bf16"
    case Floating(bits, _)        => s"f$bits"
    case Char                     => "char"
    case Bool                     => "bool"
    case Str                      => "string"
    case Unit                     => "unit"
    case VaList                   => "va_list"
    case Never                    => "never"
    case Unknown                  => "?"
    case Ptr(inner)               => s"*${show(inner)}"
    case Ref(inner, sync)         => s"&${if sync then "sync " else ""}${show(inner)}"
    case Weak(inner)              => s"weak ${show(inner)}"
    case CFn(ps, r)               => s"*extern(${ps.map(show).mkString(", ")}) -> ${show(r)}"
    case Array(n, elem)           => s"[$n]${show(elem)}"
    case Vector(n, elem)          => s"<$n>${show(elem)}"
    case Slice(elem, ro)          => s"[]${if ro then "const " else ""}${show(elem)}"
    case Volatile(inner)          => s"volatile ${show(inner)}"
    // A **pack member**'s stand-in is named `A#0` so that two of them are two types (`Abstract` is
    // identified by its name), and shown as `A` because that is the parameter the reader wrote —
    // the number is the compiler's bookkeeping and names nothing a program can refer to. `#` is in
    // no identifier, so this cannot shorten a name somebody chose.
    case Abstract(n, _)           => n.takeWhile(_ != '#')
    // A value argument is shown as the value, since that is what a reader wrote and what tells two
    // instantiations apart: `len[3]` and `len[4]` differ by this and nothing else
    // (`reference/generics.md § A parameter may stand for a value`).
    //
    // **Shown the way it was written, not the way it travels.** A `bool`, a `char` and an enum
    // variant all reach here as the number that makes their type's identity, and a diagnostic
    // reading `Run[1]` names something no program wrote — the reader has to know an internal tag to
    // recognise their own type.
    case ConstArg(v, Bool)        => (v != 0).toString
    case ConstArg(v, Char)        => s"'${v.toInt.toChar}'"
    case ConstArg(v, e: Enum)     => e.variants.find(_.tag == v.toInt).fold(v.toString)(_.name)
    case ConstArg(v, _)           => v.toString
    // A pack is shown as the list it stands for, with no parentheses: the only place one is written
    // is inside a tuple, so whatever is naming this has already supplied them
    // (`reference/generics.md § A parameter may stand for a list of types`).
    case Pack(es)                 => es.map(show).mkString(", ")
    // A call trait is spelled the way it is written rather than the way it is filed, so nothing a
    // reader is told names the arity-carrying declaration behind it (`reference/types.md § Function
    // types`).
    case Trait(n, args, assocs) =>
      Fn.parts(n, args) match
        case Some((ps, r)) => s"Fn(${ps.map(show).mkString(", ")}) -> ${show(r)}"
        // An object's associated types are written back where they were written: inside the same
        // brackets, after the arguments, since that is the one spelling a reader can copy.
        case None if assocs.isEmpty => qualified(Modules.show(n), args)
        case None =>
          val written = args.map(show) ::: assocs.map((a, t) => s"$a = ${show(t)}")
          s"${Modules.show(n)}[${written.mkString(", ")}]"
    // Every type a programmer can write is above, and `show` has already taken the named ones, so
    // nothing reaches this. It answers with the case class rather than with an LLVM type because a
    // **diagnostic must not have to know the machine**: a view's LLVM form is the one form that
    // does, and a view is spelled `[]T` or `string` several lines up, so it could not arrive here
    // in any case. Threading the width this far to render an unreachable message would have put a
    // `using Word` on every diagnostic in the compiler.
    case other                    => other.toString

  /** A type the programmer declared and may hang members off: a struct or an enum.
   *
   * Both are *nominal* — identified by the name they were declared under together with the type
   * arguments this instantiation was made with — and that identity is what member lookup, trait
   * conformance, and the mangled name of a lowered member all key on. Nothing about the two
   * layouts is shared, so this carries only the identity, which is exactly the part the analyzer
   * resolves members through: a method on an enum is found the same way a method on a struct is.
   */
  sealed trait Named extends Type {

    /** The name the type was declared under, with no type arguments applied. */
    def base: String

    /** The type arguments this instantiation was made with; empty for a non-generic type. */
    def targs: List[Type]

    /** The instantiation as a diagnostic spells it: `Point`, `Option[int]`. */
    def name: String
  }

  /** A value struct: fields in declaration order, lowered to a named LLVM aggregate. `targs`
   * is empty for an ordinary struct and holds the instantiation for a generic one.
   *
   * `fields` is filled in *after* the instantiation is registered, because a struct may reach
   * itself through a `*T` or `&T` field and resolving that field has to find the instantiation
   * already in place. Identity is therefore `(base, targs)` — the display name identifies the
   * instantiation, and the field list is a consequence of it rather than part of it.
   */
  class Struct(val base: String, val targs: List[Type]) extends Named {
    var fields: List[(String, Type)] = Nil

    /** `@packed` — fields sit at their declared offsets with no interior padding, and the aggregate
      * needs no alignment of its own (`reference/types.md § Structs`).
      *
      * The two layout facts are separate because they are separate axes: this one is about the gaps
      * *between* fields, `minAlign` about where the whole thing may *start*. A struct may be both,
      * which is what a wire header living in a DMA buffer is.
      */
    var packed: Boolean = false

    /** `@align(n)` — a floor under the aggregate's alignment, once folded. Never below what the
      * fields already require: the attribute may raise an alignment and may not lower one, since
      * lowering is what `packed` is for and a type that under-promised would be unsound to pass.
      */
    var minAlign: Option[Int] = None

    /** `@export("b2BodyId")` — the name a generated C header's `typedef` gives this type
      * (`reference/ffi.md § @export`), where `CHeader` otherwise derives one from the mangled
      * instantiation.
      *
      * **It reaches the header and nothing else.** The emitted aggregate keeps the mangled name
      * `lty` below gives it, which is what every other part of the compiler keys on, and C links
      * nothing on a type name — so this is a spelling for a reader rather than a fact anything
      * depends on.
      */
    var cname: Option[String] = None

    /** `opaque struct` — the shape is withheld from everything outside the declaring module
      * (`reference/ffi.md § opaque`). A generated C header is outside every module, so it declares
      * the type **incomplete**, the way C's own `struct foo;` handles are, and lays out no field.
      */
    var opaque: Boolean = false

    def name: String = qualified(base, targs)

    def lty(using Word): LType = LType.Named(s"%struct.${mangled(base, targs)}")

    def fieldIndex(field: String): Int = fields.indexWhere(_._1 == field)

    def fieldType(field: String): Option[Type] = fields.find(_._1 == field).map(_._2)

    /** The fields that occupy storage, in order. A zero-sized field is not one of them, so this is
     * what the emitted aggregate is made of.
     */
    def stored: List[(String, Type)] = Type.stored(fields)

    /** Where the field written `i`th lands in the emitted aggregate. Undefined for a zero-sized
     * field, which lands nowhere — nothing reads or writes one, so nothing asks.
     */
    def slot(i: Int): Int = Type.slot(fields, i)

    override def equals(other: Any): Boolean = other match
      case s: Struct => s.base == base && s.targs == targs
      case _         => false

    override def hashCode: Int  = (base, targs).hashCode
    override def toString: String = s"Struct($name)"
  }

  /** A tuple — a positional product with no declaration and no module (`reference/types.md §
   * Tuples`).
   *
   * **A tuple is a struct**, and not by analogy: it carries the same field list, so it lays out the
   * same way, retains and releases the same way, and is destructured by the same
   * `TStructPattern`. Its fields are named for their positions, which is what makes `t.0` an
   * ordinary field selection rather than a form of its own.
   *
   * What it is not is *declared*. Its base name holds a `$`, which no identifier and no module name
   * may, so nothing a program can write collides with it and nothing looks it up among the
   * declarations. The **arity is part of the base**, so each arity is its own key — that is what
   * lets the library write one implementation per arity (`impl[A, B] Eq for (A, B)`) and have the
   * two not collide, which a shared base would not.
   */
  final class Tuple(elems: List[Type]) extends Struct(Tuple.base(elems.length), elems) {
    fields = elems.zipWithIndex.map((t, i) => (i.toString, t))

    override def name: String     = s"(${targs.map(show).mkString(", ")})"
    override def toString: String = s"Tuple($name)"
  }

  object Tuple {

    /** The base name a tuple of `n` parts is keyed under. */
    def base(n: Int): String = s"${Modules.sep}tuple$n"

    /** The key an `impl` written for **every** tuple of `n` parts is filed under, spelled the way
     * the shape reads: `(,)` for a pair, `(,,)` for a triple.
     */
    def shape(n: Int): String = "(" + "," * (n - 1) + ")"

    /** The key an `impl` written for **every tuple at every arity** is filed under
     * (`reference/generics.md § A parameter may stand for a list of types`) — the third and least
     * specific rung of the ladder a tuple's own type and its arity's shape begin. `[N]` is the same
     * rung one kind down, which is why it is spelled to match.
     */
    val pack: String = "(..)"
  }

  /** Several results as a **signature** carries them (`reference/declarations.md § Several
   * results`) — `-> int, int`.
   *
   * This is not a type any value has, and that is the whole design: a result list travels from
   * callee to caller and is taken apart there, so it appears in the signature table and nowhere
   * else. The analyzer's one funnel unwraps it into the tuple its parts lay out as the moment the
   * call is used somewhere a result list is allowed, and complains where one is not — so nothing in
   * the typed tree, and nothing in codegen, ever meets one.
   */
  final class Results(val parts: Tuple) extends Type {
    def lty(using Word): LType = parts.lty

    override def equals(other: Any): Boolean = other match
      case r: Results => r.parts == parts
      case _          => false

    override def hashCode: Int    = parts.hashCode * 31 + 1
    override def toString: String = s"Results(${parts.name})"
  }

  /** One variant of an enum.
   *
   *   - `tag` is the discriminant: a simple enum's integer value, or a data enum's 0-based
   *     variant index.
   *   - `fields` are the variant's payload (empty for a nullary variant).
   *   - `carries` is whether this variant was written with a payload. Every variant that does
   *     shares the enum's one payload region, so there is nothing further to record: what
   *     distinguishes them is the type the region is written and read at, which is `fields`.
   */
  case class EnumVariant(name: String, tag: Int, fields: List[(String, Type)], carries: Boolean) {

    /** The payload fields that occupy storage, and where each written field lands among them — the
     * same skipping a struct does, so `Ok(())` carries a payload aggregate with nothing in it.
     */
    def stored: List[(String, Type)] = Type.stored(fields)
    def slot(i: Int): Int            = Type.slot(fields, i)
  }

  /** The members of a field list that occupy storage. */
  def stored(fields: List[(String, Type)]): List[(String, Type)] = fields.filterNot((_, t) => zeroSized(t))

  /** Where the `i`th written field lands once the zero-sized ones before it are dropped. */
  def slot(fields: List[(String, Type)], i: Int): Int = fields.take(i).count((_, t) => !zeroSized(t))

  /** An enum. A *simple* enum (every variant dataless) lowers to its underlying integer; a *data*
   * enum lowers to a value aggregate `{ i32 tag, payload }` whose payload is the one region every
   * variant shares, sized and aligned for the widest of them (`Layout.payloadArea`). The payload
   * of variant `V` is written and read at the named aggregate `%Name.V`.
   */
  final class Enum(val base: String, val targs: List[Type]) extends Named {
    var simple: Boolean            = true
    var variants: List[EnumVariant] = Nil

    /** A simple enum's storage type — its `: iN` annotation, or `int` when unspecified. A data
     * enum lowers to an aggregate and ignores this; its internal tag is always `i32`.
     */
    var underlying: Integer = Type.Int

    def name: String = qualified(base, targs)

    def lty(using Word): LType = if simple then underlying.lty else LType.Named(s"%enum.${mangled(base, targs)}")

    /** The type a discriminant is compared at. A simple enum **is** its discriminant, so the width
     * is whatever its `: iN` annotation said; a data enum keeps its tag in the aggregate's first
     * field, which is always `i32`. Reading it off the enum rather than assuming `i32` is what
     * keeps a narrow simple enum's variant test well-typed.
     */
    def tagLty(using Word): LType = if simple then underlying.lty else LType.I(32)

    def tagLlvm(using Word): String = tagLty.render

    def variant(v: String): Option[EnumVariant] = variants.find(_.name == v)

    /** The payload aggregate type name for a data variant, e.g. `%Shape.Circle`. */
    def payloadLty(v: EnumVariant): LType.Named = LType.Named(s"%${mangled(base, targs)}.${v.name}")

    def payloadLlvm(v: EnumVariant): String = payloadLty(v).render

    override def equals(other: Any): Boolean = other match
      case e: Enum => e.base == base && e.targs == targs
      case _       => false

    override def hashCode: Int    = (base, targs).hashCode
    override def toString: String = s"Enum($name)"
  }

  /** A named scalar carrying runtime constraints (`03`): an integer, float, or `char` `base` given
   * a name, with an optional `within` range (`lo`/`hi`, `exclusiveHi` for `..<`) and an optional
   * `where` predicate (the synthetic function `predFn` checks). It lowers exactly to its base, so
   * `llvm` delegates and a constrained value costs nothing beyond the check at the point it is made.
   *
   * `derived` is the `new` modifier. A transparent subtype (`derived = false`) is interchangeable
   * with its base; a derived one is nominally distinct and mixes with the base only through an
   * explicit cast. Identity is the whole tuple, but `name` alone already separates two declarations,
   * so a derived type is distinct from its base and from every other derived type over it.
   */
  case class Constrained(
      name: String,
      base: Type,
      derived: Boolean,
      lo: Option[BigDecimal],
      hi: Option[BigDecimal],
      exclusiveHi: Boolean,
      predFn: Option[String],
  ) extends Type {
    def lty(using Word): LType = base.lty
  }

  /** A transparent constrained subtype seen as its base — the identity for type *agreement*, so a
   * transparent `Age` stands where an `int` is asked for and the reverse. A derived type keeps its
   * own identity here (it agrees only with itself), which is what makes `new` nominal.
   */
  def repr(t: Type): Type = t match
    case c: Constrained if !c.derived => repr(c.base)
    // A qualifier says how the storage is reached, never what is in it, so a `volatile u32` agrees
    // with a `u32` for every question about the value. The distinction that has to survive is the
    // one *inside* a mode — a `*volatile u32` is not a `*u32` — and stripping at the top leaves that
    // alone, since neither of those is stripped at all.
    case Volatile(inner)              => repr(inner)
    case _                            => t

  /** Every constrained subtype seen as its ultimate base representation — the identity for explicit
   * conversions and for the scalar operations codegen lowers. Unlike `repr`, this strips a derived
   * type too, since a written conversion (`f64(m)`) is exactly the licence to reach the base.
   */
  def underlying(t: Type): Type = t match
    case c: Constrained  => underlying(c.base)
    case Volatile(inner) => underlying(inner)
    case _               => t

  /** The source-level spelling of an instantiated named type: `Box`, `Result[int, string]`. */
  def qualified(base: String, targs: List[Type]): String =
    if targs.isEmpty then base else s"$base[${targs.map(show).mkString(", ")}]"

  /** An LLVM-safe name for an instantiation: `Result[int, string]` becomes
   * `Result.int.string`. Every name has a fixed arity, so flattening the arguments this way
   * stays unambiguous while keeping the emitted IR readable.
   */
  def mangled(base: String, targs: List[Type]): String =
    if targs.isEmpty then base else s"$base.${targs.map(mangleOne).mkString(".")}"

  /** One type's contribution to a mangled name, which is also how a runtime helper for a type
   * is named (`@arc.release.Node`, `%arc.Option.int`).
   */
  def mangle(t: Type): String = mangleOne(t)

  /** The prefix a type's **members** are emitted under, which is the mangling for every type but
   * one.
   *
   * A transparent constrained subtype mangles as its base, deliberately: sharing a representation
   * is what makes `Vec[Meters]` and `Vec[f64]` one *emitted layout* rather than two identical ones.
   * They remain two instantiations to the analyzer, and have to — only one of them checks what is
   * written into it — so what the shared name obliges is that the layout be defined once, which is
   * `Codegen`'s business rather than this function's.
   * Members are the place that rule must not reach — `Age`'s and `int`'s are different bodies, and
   * naming both `int.describe` gives one symbol two definitions. So a member of a constrained type
   * is prefixed with the type's own name whichever kind it is, which is also the key its members
   * are filed under, so a call built from the key and a definition built from the type agree.
   */
  def memberSymbol(t: Type): String = t match
    case c: Constrained => show(c)
    case other          => mangleOne(other)

  /** A memory mode is mangled as a word rather than its sigil, since `*` and `&` are not
   * LLVM-name characters.
   */
  private def mangleOne(t: Type): String = t match
    case n: Named         => mangled(n.base, n.targs)
    case Ptr(inner)       => s"ptr.${mangleOne(inner)}"
    case Ref(inner, false) => s"ref.${mangleOne(inner)}"
    case Ref(inner, true)  => s"sync.${mangleOne(inner)}"
    case Weak(inner)       => s"weak.${mangleOne(inner)}"
    case Array(n, elem)    => s"arr$n.${mangleOne(elem)}"
    // A vector's lane count is mangled for the reason an array's length is, and it matters more
    // here: a kernel generic over its width is instantiated once per width and the two bodies hold
    // different instructions, so a name that dropped the count would give `solve` at 4 lanes and
    // `solve` at 8 one body.
    case Vector(n, elem)   => s"vec$n.${mangleOne(elem)}"
    // **A value argument is part of the mangled name**, for exactly the reason a type argument is:
    // two instantiations that differ only in it are two bodies, and a name that dropped it would
    // let `total` at length 3 share a body with `total` at length 4 (`reference/generics.md § A
    // parameter may stand for a value`).
    case ConstArg(v, _)    => s"c$v"
    // A pack carries its length as well as its members, so that two instantiations of one block at
    // two arities are two bodies — the same reason a value argument is mangled
    // (`reference/generics.md § A parameter may stand for a list of types`). The members alone
    // would already differ, and the count is what keeps the boundary between them unambiguous when
    // a member's own mangling contains a dot.
    case Pack(es)          => s"pk${es.length}.${es.map(mangleOne).mkString(".")}"
    // The bit is mangled even though both forms have one layout and one set of instructions, so
    // that a generic instantiated at `[]const T` never shares a body with one instantiated at
    // `[]T` — the bodies would be identical machine code, but only one of them had its writes
    // checked, and reusing that analysis is the way the bit stops meaning anything.
    case Slice(elem, false) => s"slice.${mangleOne(elem)}"
    case Slice(elem, true)  => s"constslice.${mangleOne(elem)}"
    // Mangled for the reason the read-only bit is: `*volatile u32` and `*u32` lay out identically
    // and are reached by different instructions, so an instantiation at one must never share a body
    // with an instantiation at the other.
    case Volatile(inner)    => s"volatile.${mangleOne(inner)}"
    // The associated types are **not** mangled in, and leaving them out is what keeps one table per
    // (trait, type): the object fixes them, but a given type supplies exactly one of each, so two
    // objects over one type could only ever have agreed about them.
    case Trait(n, args, _) => mangled(n, args)
    // The signature is part of the name for the reason a generic's arguments are: two of these are
    // the same type only where they are called the same way, and a mangled name that dropped the
    // signature would let an instantiation at one share a body with an instantiation at another.
    case CFn(ps, r)        => s"cfn${ps.length}.${(ps :+ r).map(mangleOne).mkString(".")}"
    // A transparent subtype shares its base's representation, so it mangles as the base; a derived
    // one is its own type and mangles under its name, keeping `Vec[Meters]` and `Vec[f64]` apart.
    case c: Constrained    => if c.derived then mangled(c.name, Nil) else mangleOne(c.base)
    case other            => show(other)

  /** How a type is written in a diagnostic: the friendly alias where one exists (`int`,
   * `byte`, `real`), the canonical width spelling otherwise (`i5`, `u12`, `f32`).
   *
   * A declared type is named by the key its module gives it (`Modules`), which a reader spells
   * with a dot rather than the separator that keeps it apart from a member's name — so a
   * `geom$Point` is shown as the `geom.Point` a program would write.
   *
   * **A closure has no name and is not given one here.** The struct a closure literal lowers to is
   * filed under a serial number, and that number is a fact about the whole compilation rather than
   * about the program: the library is lowered first, so every closure a program writes is numbered
   * after every closure `library/` holds, and adding one closure literal to the standard library
   * renumbers the closures in every program there is. A reader told `.closure4` has nothing to grep
   * for and no question answered, so they are told what it is instead — the wording
   * `Sharing.complaint` already uses for the same reason.
   */
  def show(t: Type): String = t match
    case r: Results     => r.parts.targs.map(show).mkString(", ")
    case t: Tuple       => t.name
    case _ if Closures.literal(t) => "a closure"
    case n: Named       => qualified(Modules.show(n.base), n.targs)
    case c: Constrained => Modules.show(c.name)
    case other          => friendly.getOrElse(other, canonicalName(other))

  /** [[show]] for a sentence that supplies its own article — "the \${showBare(t)}".
   *
   * The one description [[show]] gives that is a **phrase rather than a name** is a closure's, and
   * it carries an article because everywhere else in a message it stands alone: *"the default for
   * 'f' is a closure"* reads correctly and *"is closure"* does not. After a `the` it reads *"the a
   * closure"*, which is how this was found — a closure literal at a `*Fn` parameter was advised to
   * *"write '&' in front of the a closure"*.
   *
   * Dropping the article rather than adding a second description is the fix that stays right as
   * more phrases arrive: what a sentence with its own `the` wants is the noun, and every name
   * [[show]] returns is already one.
   */
  def showBare(t: Type): String = show(t) match
    case s if s.startsWith("a ")  => s.drop(2)
    case s if s.startsWith("an ") => s.drop(3)
    case s                        => s
}
