package sh.sysl

/** The *typed* abstract syntax tree.
 *
 * The analyzer turns the structural `ast.scala` tree into this one: every node carries a
 * resolved `Type`, names are resolved to their unique bindings, and every rule that could
 * fail (unknown name, type mismatch, wrong arity) has already been checked. Codegen is then
 * a straight lowering — it selects instructions from the types it is handed and never makes
 * a semantic decision of its own.
 *
 * A typed node keeps the position of the untyped one it came from, so the passes that run *after*
 * the analyzer — escape analysis, which sees only this tree — can point at source too.
 *
 * Three files hold it, because `sealed` asks for one: `TExpr` here, `TPattern` in
 * `tastPatterns.scala`, and `TStmt` with the declaration nodes in `tastStmts.scala`. That is the
 * same division `ast.scala`, `astPatterns.scala` and `astStmts.scala` already make for the untyped
 * tree, and for the same reason — a hierarchy is split where the language lets it be split.
 */

sealed trait TExpr extends Positioned {
  def ty: Type

  /** The type of the **storage** this node names, as against `ty`, which is the type of the value
   * read out of it. The two differ only by a `volatile` qualifier (`reference/memory.md § Device
   * memory`), which the projection that built the node stripped: a read of a `volatile u32` hands
   * back a `u32`.
   *
   * It is recovered from the receiver rather than kept on the node, because the receiver is where
   * the declaration that wrote the qualifier still is — a struct's field list, an array's element
   * type, a pointer's pointee. That keeps one statement of the fact instead of two that could drift,
   * and it is what lets `&regs.status` be a `*volatile u32` and every access through the result stay
   * an access to a register.
   */
  def placeTy: Type = this match
    case TDeref(operand, t)     => Type.pointee(operand.ty).getOrElse(t)
    case TIndex(receiver, _, t) => Type.element(receiver.ty).getOrElse(t)
    case TField(receiver, i, t) =>
      receiver.ty match
        case s: Type.Struct if i >= 0 && i < s.fields.length => s.fields(i)._2
        case _                                               => t
    case _ => ty

  /** Whether this names **storage** — somewhere with an address of its own — rather than a value
   * that has just been computed.
   *
   * It is what tells a temporary from a place, and two passes need the same answer: codegen gives a
   * computed value a slot before it can reach into one, and escape analysis moves a temporary to the
   * heap rather than refusing a view of it that gets out. Two definitions of this could disagree
   * without anything failing to compile, so there is one.
   *
   * **An index is a place whatever its receiver is**, since an element of a computed array is
   * reached through the slot that array is materialized into. A **field** is one only where its
   * receiver is: `f().x` is part of a value nobody has a name for.
   */
  def isPlace: Boolean = this match
    case _: TLoad | _: TGlobal | _: TDeref | _: TIndex => true
    case TField(receiver, _, _)                        => receiver.isPlace
    case _                                             => false
}

/** An integer, `char`, or simple-enum constant — anything whose value is one whole number. */
case class TIntLit(value: BigInt, ty: Type) extends TExpr

/** A floating-point constant, held as the bits of its `double` value. A narrower type is
 * reached by rounding that constant down to it, which costs nothing at run time.
 */
case class TFloatLit(bits: Long, ty: Type) extends TExpr

case class TStrLit(value: String)   extends TExpr { def ty: Type = Type.Str  }
case class TBoolLit(value: Boolean) extends TExpr { def ty: Type = Type.Bool }
case class TUnitLit()               extends TExpr { def ty: Type = Type.Unit }

/** `null` at the pointer type its context fixed. */
case class TNullLit(ty: Type) extends TExpr

/** Puts a value on the heap, because a `&T` was expected where a `T` was written. The box holds
 * the refcount, the deallocation hook, and the payload; the expression yields a reference the
 * caller owns.
 */
case class TBox(value: TExpr, refTy: Type.Ref) extends TExpr { def ty: Type = refTy }

/** Weakens a reference, because a `weak T` was expected where a `&T` was written (`03`). The
 * object's weak count goes up and its strong count does not, so the edge this makes does not keep
 * the object alive.
 */
case class TDowngrade(value: TExpr, weakTy: Type.Weak) extends TExpr { def ty: Type = weakTy }

/** `w.get()` — asks the box whether the object is still there, and takes a count if it is.
 * `optTy` is the `Option[&T]` handed back, with `some`/`none` its two variants.
 */
case class TUpgrade(value: TExpr, optTy: Type.Enum, some: Type.EnumVariant, none: Type.EnumVariant)
    extends TExpr { def ty: Type = optTy }

/** The zero value of a type: what a declaration with no initializer starts at. */
case class TZero(ty: Type) extends TExpr

/** `[a, b, c]` — an array value built from its elements. */
case class TArrayLit(elems: List[TExpr], arrayTy: Type.Array) extends TExpr { def ty: Type = arrayTy }
case class TArrayFill(value: TExpr, arrayTy: Type.Array)      extends TExpr { def ty: Type = arrayTy }

/** `[a, b, c, d]` written where a `<4>T` was expected — a vector built from its lanes.
 *
 * It is the array literal's spelling because it is the array literal's meaning: the same values in
 * the same order. What differs is where they end up, and a register is not storage — which is why
 * this is a distinct node rather than a `TArrayLit` with a different type on it. An array is built
 * in a slot and filled through it; a vector is built by `insertelement` and never has an address at
 * all unless something takes one.
 */
case class TVectorLit(lanes: List[TExpr], vecTy: Type.Vector) extends TExpr { def ty: Type = vecTy }

/** A scalar broadcast into every lane — the splat.
 *
 * Written nowhere: it is what a scalar becomes where a vector was asked for, so `a * 2.0` and
 * `val v: <4>f32 = 0.0` both reach it through `coerce`. Giving it a spelling of its own would put a
 * word in the way of the one thing every SIMD kernel does on nearly every line.
 */
case class TSplat(value: TExpr, vecTy: Type.Vector) extends TExpr { def ty: Type = vecTy }

/** `v[0]` — one lane read out of a vector.
 *
 * **The index is a compile-time constant and the node carries it as an `Int`**, which is the one
 * way this differs from `TIndex` and is not a limitation so much as what a register is. An array
 * index is checked against the length at run time and traps; a vector has no address to check
 * against, and LLVM's answer to an out-of-range `extractelement` is poison — a value that is not a
 * value, spreading silently through everything computed from it. Refusing the dynamic form is
 * therefore the only reading that keeps sysl's promise about a subscript. Somebody who needs a
 * computed lane wants the vector in memory, where an array's checked subscript already answers.
 */
case class TLane(receiver: TExpr, lane: Int, ty: Type) extends TExpr

/** A lane-wise comparison — `a < b` on two vectors, yielding a mask.
 *
 * **It is not a `TCompare`, and the difference is that a chain is meaningless here.** `a < b < c`
 * on scalars is two comparisons sharing a middle operand and `&&`-ed together, and there is no
 * lane-wise `&&` to do the joining with. So this is exactly two operands, and a written chain of
 * vectors is refused where it is written rather than lowered into something that reads like it
 * short-circuits.
 */
case class TVecCompare(op: String, left: TExpr, right: TExpr, ty: Type) extends TExpr

/** `m.select(a, b)` — `a`'s lane where `m`'s is true and `b`'s where it is false.
 *
 * **This is the lane-wise `if`, and it is a method rather than a keyword because it cannot be one.**
 * `if` branches: it evaluates one side or the other, and a register has no way to take one branch in
 * two lanes and the other in the remaining two. So both sides are evaluated and the mask chooses
 * between the results, which is a different operation from the one `if` names and is spelled
 * differently for that reason.
 */
case class TSelect(mask: TExpr, whenTrue: TExpr, whenFalse: TExpr, ty: Type) extends TExpr

/** A vector collapsed to a scalar — `v.sum()`, `v.min()`, `v.max()`, and a mask's `any()` and
 * `all()`. `op` is the LLVM reduction's own name, which is what the emitter writes.
 */
case class TReduce(op: String, receiver: TExpr, ty: Type) extends TExpr

/** `xs.load(i)` — a run of an array's or a slice's elements read into a vector.
 *
 * **This is the one vector operation that touches memory, and so the only one with a run-time
 * check.** Everything else a vector does is a register operation that cannot fail; a run of `W`
 * elements starting at `i` needs `i + W <= xs.len`, which is not knowable until the program runs.
 * The check is the subscript's, widened from one element to `W` of them, and it traps the same way
 * — a vector is not a hole through which a program reaches past the end of an array.
 *
 * **The width is the answer's rather than the receiver's**, which is why the whole vector type is
 * carried here: a slice has whatever length it has, and how many of its elements one load takes is
 * the type asked for. That is what makes this usable from a `[const W: usize]` body, where the
 * width is a parameter and no literal could stand in for it.
 */
case class TVecLoad(receiver: TExpr, index: TExpr, vecTy: Type.Vector) extends TExpr {
  def ty: Type = vecTy
}

/** `xs.store(i, v)` — a vector's lanes written into a run of an array's or a slice's elements.
 *
 * The mirror of `TVecLoad`, checked identically. It answers `unit`, because what it is for is the
 * effect and a value could only be the vector handed back.
 */
case class TVecStore(receiver: TExpr, index: TExpr, value: TExpr) extends TExpr {
  def ty: Type = Type.Unit
}

/** The same two forms written where a `[]T` was expected: storage of the program's own, and a view
 * of all of it. The count of a `TBufFill` is an ordinary expression rather than part of a type,
 * which is the whole reason these exist — an array's length is fixed when it is compiled, and a
 * length read out of a file is not (`reference/arrays.md § Storage sized while running`).
 */
case class TBufLit(elems: List[TExpr], sliceTy: Type.Slice) extends TExpr { def ty: Type = sliceTy }
case class TBufFill(value: TExpr, count: TExpr, sliceTy: Type.Slice) extends TExpr { def ty: Type = sliceTy }

/** `a[i]` — one element of an array, slice, or string, checked against the length. It is a
 * *place* when its receiver is one, which is what makes `a[i] = v` and `&a[i]` ordinary.
 */
case class TIndex(receiver: TExpr, index: TExpr, ty: Type) extends TExpr

/** `a.len` — how many elements, as a `usize`. Constant for an array, a word of the view for a
 * slice or a string, where it counts bytes.
 *
 * The type is carried rather than computed, and it is the only node in this tree where that is so.
 * A `usize` is pointer-width by definition, so its width is the target's answer — and a node that
 * worked its own type out would have to be told the machine to do it, which is a thing a typed tree
 * should already be past. The analyzer knows the target and writes it down once.
 */
case class TLen(receiver: TExpr, ty: Type) extends TExpr

/** `s.bytes` — a string's bytes, as a `[]const u8`. The same three words the string already is, so
 * this reinterprets rather than converts; only the validity guarantee is given up.
 *
 * The view is read-only because the string is, and because the elements may be a literal's, which
 * live in memory the platform will not let anybody write. A writable view of them was the one way
 * a program with no `*T` in it could still be killed by the machine (`03`), so the guarantee that
 * chapter opens with is what this bit is holding up.
 */
case class TBytes(receiver: TExpr) extends TExpr {
  def ty: Type = Type.Slice(Type.Byte, readOnly = true)
}

/** `a[lo..hi]` — a view of some of `base`'s elements. `base` is either the reference that owns
 * a heap array or another view, so that one expression yields both where the elements are and
 * what keeps them alive. An absent bound is the start or the end.
 */
case class TSlice(base: TExpr, lo: Option[TExpr], hi: Option[TExpr], inclusive: Boolean, sliceTy: Type.View)
    extends TExpr {
  def ty: Type = sliceTy
}

/** An explicit scalar conversion, written with call syntax: `u32(c)`, `byte(n)`, `char(u)`.
 * Every conversion between scalar types is written, never inferred.
 */
case class TCast(operand: TExpr, ty: Type) extends TExpr

/** A value produced into a constrained subtype (`03`): `value` is the base-typed value, and
 * `target` names the subtype whose `within` range and `where` predicate it must satisfy. Codegen
 * emits `value`, checks it, and yields the same value — the representation is unchanged, so the
 * only run-time effect is a trap when a constraint is violated.
 */
case class TConstrainedCheck(value: TExpr, target: Type.Constrained) extends TExpr { def ty: Type = target }

/** `T::Valid(x)` — whether `x` satisfies `target`'s `within` range, as a `bool`. Never traps; it is
 * the total test a checked cast is the trapping form of.
 */
case class TConstrainedValid(value: TExpr, target: Type.Constrained) extends TExpr { def ty: Type = Type.Bool }

/** `T::Succ(x)` / `T::Pred(x)` — the next (`up`) or previous value in `target`'s range, trapping at
 * the far end (`Succ` at `Last`, `Pred` at `First`). Yields the base integer.
 */
case class TConstrainedStep(value: TExpr, target: Type.Constrained, up: Boolean, ty: Type) extends TExpr

/** Reads a local variable (or parameter) by its unique name. */
case class TLoad(name: String, ty: Type) extends TExpr

/** `result` inside an `ensure` postcondition — the value the function is about to return. */
case class TResult(ty: Type) extends TExpr

/** `old(e)` inside an `ensure` — reads the value expression `e` had at function entry, captured
 * into the function's `olds` slab before the body ran. `index` is that slab position.
 */
case class TOld(index: Int, ty: Type) extends TExpr

/** Names module-level storage — a `val`, under the key its module gives it, or an `extern` variable,
 * under the symbol the linker resolves. Either way it exists for the whole run and it is a *place*,
 * so indexing and iterating reach into it without copying the whole thing out.
 *
 * `writable` is what tells the two apart, and it is the only thing that does. A `val` is written
 * once and the analyzer refuses both an assignment to one and a `*T` into it (`reference/modules.md § val — a thing`); an `extern`
 * variable is storage this program did not lay down, so there is no such promise to keep — `optind`
 * and `optarg` are assigned by ordinary C and a declaration that could not would name half of what
 * it was added to reach.
 */
case class TGlobal(symbol: String, ty: Type, writable: Boolean = false) extends TExpr

/** `*p` — reads through a pointer or reference. */
case class TDeref(operand: TExpr, ty: Type) extends TExpr

/** `o::Id` on an **erased** value — the compile-time identity of the type inside it, read out of
 * the method table the object points at (`02`, `TypeId`).
 *
 * It is a *load* rather than a constant, and that is the whole of what separates it from `T::Id`:
 * the type is not known here, which is the case the form exists for. What it costs is one load from
 * a constant global, and the object was already going to be dereferenced to call anything on it.
 */
case class TTypeId(receiver: TExpr, ty: Type) extends TExpr

/** `&place` — the address of a place, as a raw pointer. */
case class TAddrOf(place: TExpr, ty: Type) extends TExpr

/** `&value` where the value has no address of its own — a construction, a call result, an
 * arithmetic result, a literal.
 *
 * The storage is a **hidden local of the scope the `&` was written in**: a slot is laid down, the
 * value is written into it with a count taken exactly as a `var`'s initializer takes one, and it is
 * released where that scope ends. So `&Rect(2)` is `var t = Rect(2)` with the name left out, and the
 * pointer it yields is as good — and as unchecked (`03`) — as one taken of any local.
 */
case class TTempAddr(value: TExpr, ty: Type) extends TExpr

/** `place = value` — stores and yields the assigned value. The place is a `TLoad`, a `TDeref`,
 * or a `TField` chain over one of those; codegen computes its address rather than its value.
 */
case class TStore(place: TExpr, value: TExpr, ty: Type) extends TExpr

/** A compound assignment `place op= value`, yielding the updated value. `dispatch` is present when
 * the operator is a trait method rather than an instruction (`reference/expressions.md § Operator
 * dispatch`).
 *
 * `check` is present when the place holds a constrained subtype: the operation computes a value the
 * place cannot be given unexamined, so the constraint is tested between the arithmetic and the
 * store (`reference/errors.md § Where a constraint is checked`). It is the same test a
 * `TConstrainedCheck` makes; it lives here rather than in a wrapping node because the store is
 * inside this one, and a value has to be refused before it lands.
 */
case class TUpdate(place: TExpr, op: String, value: TExpr, ty: Type, dispatch: Option[TDispatch] = None,
                   check: Option[Type.Constrained] = None)
    extends TExpr

/** `++`/`--`, prefix (new value) or postfix (old value). `check` carries a constrained place's
 * constraint, for the reason `TUpdate`'s does.
 */
case class TIncDec(place: TExpr, op: String, pre: Boolean, ty: Type, check: Option[Type.Constrained] = None)
    extends TExpr

case class TBinary(op: String, left: TExpr, right: TExpr, ty: Type) extends TExpr
case class TUnary(op: String, operand: TExpr, ty: Type)             extends TExpr

/** A member a built-in has by rule rather than by an `impl`, and which lowers to instructions
 * rather than to a call (`reference/expressions.md § Operator dispatch`, `CoreTraits.numeric`).
 *
 * A node of its own rather than a tree of the operators it is equivalent to, because every one of
 * these reads its operand more than once — a magnitude compares it and negates it — and a tree would
 * evaluate the receiver expression once per mention. `f().abs()` must call `f` once.
 *
 * `width` is the integer type the instruction runs at, which is the receiver's own type with any
 * constrained subtype resolved away; `ty` is what the member answers, and the two differ wherever
 * the answer is a **count** of bits rather than a value of the receiver's type. `amount` is the
 * rotations' second operand and nothing else's.
 */
case class TIntOp(op: String, operand: TExpr, amount: Option[TExpr], width: Type, ty: Type) extends TExpr

/** One atomic access, at the address `addr` points at (`library/sync.md § Atomic[T]`, `Atomics`).
 *
 * `ops` are the value operands the form takes — none for a load, one for a store or a
 * read-modify-write, two for a compare-and-swap — and `at` is the type they and the access run at,
 * read off the address rather than off the operands so that a widened literal cannot decide it.
 *
 * `ordering` is a variant *name* rather than a value, because that is what it becomes in the emitted
 * instruction: LLVM spells the ordering as a keyword, so nothing here could be computed. The
 * analyzer is what refuses an ordering the call did not write out.
 *
 * `ty` differs from `at` for exactly one form — a store answers nothing — which is why both are
 * carried instead of the result being derived where it is read.
 */
case class TAtomic(op: String, addr: TExpr, ops: List[TExpr], ordering: String, at: Type, ty: Type)
    extends TExpr

/** `atomic_fence(ord)` — a barrier that reaches no address, ordering the accesses around it. */
case class TFence(ordering: String) extends TExpr { def ty: Type = Type.Unit }

/** `&&` / `||` — short-circuit, always boolean. */
case class TLogical(op: String, left: TExpr, right: TExpr) extends TExpr { def ty: Type = Type.Bool }

/** An operator that a trait supplies rather than the machine (`reference/expressions.md § Operator
 * dispatch`): the function it lowers to, whether the derivation swaps its operands, and whether it
 * negates the result (`reference/expressions.md § Operator dispatch`).
 *
 * It rides on the node the operator already lowers to instead of replacing that node with a `TCall`,
 * and the reason is the two forms that use one operand **twice from a single evaluation** — a
 * comparison chain, which compares each middle operand against both its neighbours, and a compound
 * assignment, which updates the place it read. Codegen holds that operand in a register for the
 * scalar lowering; naming the method here lets the call read the same register, where a call built
 * over the operand's own tree would evaluate it a second time.
 */
case class TDispatch(name: String, swap: Boolean = false, negate: Boolean = false)

/** One comparison in a chain: the operator, and the method that performs it when the operand type
 * reaches `Eq`/`Ord` through a trait instead of an instruction.
 */
case class TCmp(op: String, dispatch: Option[TDispatch] = None)

/** A comparison chain `a < b < c`, ANDing the pairwise results. */
case class TCompare(operands: List[TExpr], cmps: List[TCmp]) extends TExpr { def ty: Type = Type.Bool }

/** Several expressions evaluated in order for their effects, yielding nothing. `print(a, b, c)`
 * desugars to one of these — a call per value, with the separators between — which is what lets the
 * printing itself live in the library rather than in codegen.
 */
case class TSeq(exprs: List[TExpr]) extends TExpr { def ty: Type = Type.Unit }

/** The built-in `str(x)` — a primitive value's string form: a decimal for an integer, its UTF-8
 * for a `char`, `"true"`/`"false"` for a `bool`, a `%g` rendering for a float, and a `string`
 * unchanged. It allocates a fresh buffer for every type but `string`, where it is the identity;
 * a `Display` trait method replaces it once traits land.
 */
case class TStr(arg: TExpr) extends TExpr { def ty: Type = Type.Str }

/** The built-in `str_cast(b)` — the bytes of a `[]u8` as a `string`, with nothing
 * checked (`reference/strings.md § Validity`).
 *
 * It is here rather than in the library for the reason the `va_*` forms are: no sysl body can build
 * a `string`, since every safe route to one already has the UTF-8 guarantee behind it. The bytes are
 * **copied** into a fresh owning string rather than shared with the slice — a `[]u8` is mutable and
 * a `string` is not, so aliasing the source would let a later write break the invariant of a value
 * that had already been checked.
 */
case class TFromBytes(arg: TExpr) extends TExpr { def ty: Type = Type.Str }

/** The built-in `str_alias(b)` — the bytes of a `[]u8` as a `string` **in place**: the same three
 * words, so the string shares the slice's owner exactly as `s[a..b]` shares a string's, and nothing
 * is allocated or copied. It is `TBytes` read the other way round.
 *
 * It is the one route to a `string` whose bytes can change while it lives, which is why it is the
 * raw tier's and why the library's spelling of it, `sysl.text.str_view`, says what its caller owes:
 * the bytes are UTF-8, and nothing writes them while the string is alive. What the compiler does
 * still guarantee is the storage — a view of a frame's array moves that array to the heap, since a
 * `string` outlives every frame (`Escape`).
 */
case class TStrView(arg: TExpr) extends TExpr { def ty: Type = Type.Str }

/** A `[]T` standing where a `[]const T` was asked for — the one direction between the two views
 * that is safe, since giving up the ability to write cannot be observed by anything holding the
 * view it came from.
 *
 * It carries no instruction. Both views are the same three words and the same layout, so this
 * exists only so that what is written on the expression matches what the context asked for; codegen
 * emits the operand and nothing else.
 */
case class TConstView(arg: TExpr) extends TExpr { def ty: Type = Type.constView(arg.ty) }

/** `format(value, spec)` — one value rendered through a printf specifier, the lowering of an
 * `f"…"` hole. Always allocates a fresh string; `spec` is the sysl specifier (`%08.2f`), which
 * codegen turns into the C one it hands `snprintf`.
 */
case class TFormat(arg: TExpr, spec: String) extends TExpr { def ty: Type = Type.Str }

/** `str(x)` and an `f"…"` hole on a value that renders itself: `method` is the `display` its type
 * reaches, and it writes into a growable buffer whose bytes this yields as a fresh `string`.
 *
 * That buffer is the one sink the compiler still provides. Standard output is no longer one of them:
 * a fieldless struct with an `impl Writer` says the whole of what that sink is, and the library
 * declares it. What keeps this one here is the *buffer*, whose growth and handover are written by
 * hand in `WriterEmitter`.
 *
 * `slot` is set where the value is a **trait object** whose trait requires `Display`: the renderer is
 * then a word read out of the object's own table rather than a function named here, and `method`
 * carries only the name a diagnostic would use.
 */
case class TRender(value: TExpr, method: String, spec: TExpr, slot: Option[Int] = None) extends TExpr {
  def ty: Type = Type.Str
}

/** `c"…"` — the address of a NUL-terminated constant, which is what a C interface reads a string
 * as. The terminator is not counted in anything: it is there for the callee to find the end by, and
 * the value is a plain `*u8`.
 */
case class TCStrLit(value: String) extends TExpr { def ty: Type = Type.Ptr(Type.Integer(8, signed = false)) }

/** A call to a user function. */
case class TCall(name: String, args: List[TExpr], ty: Type, results: Boolean = false) extends TExpr

/** `&f` — the address of a declared function, as the C convention would have somebody call it
 * (`reference/ffi.md § A function's address`). `name` is the function this addresses; `entry` is
 * the symbol the address is *of*, which is the function itself where sysl's own convention already
 * agrees with C's and a generated adapter where it does not.
 *
 * The two are separate because the choice is the emitter's and the reachability walk's question is
 * about the first: what keeps the definition in the program is that something took its address, and
 * that stays true whichever symbol the address turns out to name.
 */
case class TFuncAddr(name: String, entry: String, ty: Type) extends TExpr

/** A call through a `*extern` — a function pointer with no definition in sight (`reference/ffi.md §
 * A function's address`).
 *
 * It carries the whole signature rather than looking one up, because there is nothing to look up:
 * the callee is a value, and what is known about it is exactly what its type said. That is also why
 * the arguments cross under the C convention — the type says the other end obeys it, and nothing
 * here can check that it does.
 */
case class TCallPtr(callee: TExpr, args: List[TExpr], params: List[Type], ty: Type) extends TExpr

/** Forgets a value's type, keeping what its trait says can still be done to it (`02`): the operand
 * goes on pointing where it pointed, and the method table for the type it is losing rides beside it.
 *
 * The operand is a `*T` for a raw trait object and a `&T` for a counted one, which is what decides
 * which of the type's two tables this names — the data word addresses the value in the first case
 * and its box in the second.
 */
case class TErase(operand: TExpr, vtable: String, ty: Type) extends TExpr

/** A call through a trait object's method table. `slot` is the method's index in the trait's
 * declaration order, and the receiver's data word is the first argument — so which function runs is
 * read out of the table at run time rather than named here.
 */
case class TVCall(receiver: TExpr, slot: Int, args: List[TExpr], ty: Type, results: Boolean = false)
    extends TExpr

/** The ABI primitives of a variadic body (`reference/ffi.md § Variadic functions`), each holding
 * the *address* of the `va_list` it works on — they advance it rather than reading a copy of it, so
 * what the analyzer hands over is the place, exactly as `&ap` would. A `*va_list` a caller lent is
 * already that address and is handed over as it stands.
 *
 * `TVaArg` carries the type it reads, which the analyzer took from the context the value is used
 * in; there is nothing in the tail to check that against, which is what makes it as unsafe as C's.
 *
 * `TVaCopy` takes a second walk over the same tail from where the first has reached, which is what
 * a body needs before lending its walk to somebody who will advance it.
 */
case class TVaStart(ap: TExpr) extends TExpr { def ty: Type = Type.Unit }
case class TVaEnd(ap: TExpr)   extends TExpr { def ty: Type = Type.Unit }
case class TVaArg(ap: TExpr, ty: Type) extends TExpr
case class TVaCopy(dst: TExpr, src: TExpr) extends TExpr { def ty: Type = Type.Unit }

/** A walk handed to a **foreign** function whose C parameter is a `va_list` — `vprintf` and its
 * family (`reference/ffi.md § Variadic functions`).
 *
 * It holds the address of the walk, like the four forms above, and differs from passing that
 * address in what the callee is given: C's `va_list` is a different type on every target and is
 * passed three different ways, so this is the node that turns the one thing sysl has into what the
 * target's C ABI wants. The result is a `ptr` whichever way that is, which is why the distinction
 * cannot be left for codegen to rediscover from the types — see `VaListAbi`.
 */
case class TVaPass(ap: TExpr) extends TExpr { def ty: Type = Type.Ptr(Type.VaList) }

/** Positional construction of a value struct. */
case class TStructNew(struct: Type.Struct, args: List[TExpr]) extends TExpr { def ty: Type = struct }

/** A struct value checked against its `invariant` clauses (`reference/errors.md § Struct
 * invariants`): `value` builds the struct, `invFn` is the synthesised `<Struct>$inv` that takes the
 * fields and returns a `bool`. Codegen emits `value`, calls `invFn` with its field values, traps on
 * a false result, and yields the same value — the struct is unchanged, so the only run-time effect
 * is a trap when an invariant is violated.
 */
case class TStructInvCheck(value: TExpr, struct: Type.Struct, invFn: String) extends TExpr { def ty: Type = struct }

/** Something that may have changed a struct carrying `invariant` clauses (`reference/errors.md §
 * Struct invariants`): `after` runs, then `recv` — the struct that could have been mutated — is
 * re-read and passed to `invFn`, trapping on a false result. Yields what `after` yields, so `s.f =
 * v` remains an expression of the field's type.
 *
 * Two things wear this. A **write** into the struct: a direct `s.f = v`, a compound `s.f op= v`, a
 * through-pointer `(*p).f = v`, or one nested inside — `o.a.n = 9`. And a **`*self` method call on a
 * field of it**: `o.a.bump()` hands the callee somewhere to write that no longer names the `Outer`,
 * so the clause is re-run where the whole place is still known, which is the call site.
 *
 * These **nest** where a place lies inside more than one struct that carries clauses: `o.a.n` wraps
 * the store in `Inner`'s check and that in `Outer`'s, so the inner one runs first.
 */
case class TRecheck(after: TExpr, recv: TExpr, struct: Type.Struct, invFn: String) extends TExpr {
  def ty: Type = after.ty
}

/** Construction of an enum value: a simple enum's integer constant, or a data enum's variant
 * (with `args` for a data-carrying variant, empty for a nullary one).
 */
case class TEnumNew(enumTy: Type.Enum, variant: Type.EnumVariant, args: List[TExpr]) extends TExpr {
  def ty: Type = enumTy
}

/** `Color(n)` — an integer reinterpreted as a simple enum, checked: it traps on an integer that
 * is not a declared discriminant. Yields the enum, stored at its underlying integer width.
 */
case class TEnumFromInt(value: TExpr, en: Type.Enum) extends TExpr { def ty: Type = en }

/** `Color.try(n)` — the fallible constructor: `Some(Color)` when `n` is a declared discriminant,
 * `None` otherwise. `optTy` is the resulting `Option[Color]` and `some`/`none` its two variants.
 */
case class TEnumTry(value: TExpr, en: Type.Enum, optTy: Type.Enum,
                    some: Type.EnumVariant, none: Type.EnumVariant) extends TExpr { def ty: Type = optTy }

/** A simple enum's type attribute with a runtime argument (`reference/types.md § Enums`): `kind`
 * names which one — `Pos` (a value's 0-based position), `Val` (the value at a position, trapping
 * out of range), `Succ`/`Pred` (the neighbouring value, trapping at the end), `Image` (a value's
 * name as a string), or `Value` (the value named by a string, trapping on no match). `arg` is the
 * one operand. The bare `First`/`Last` are compile-time constants and need no node of their own.
 */
case class TEnumAttr(kind: String, en: Type.Enum, arg: TExpr, ty: Type) extends TExpr

/** The postfix `?` on an `Option`/`Result` value: yields the success payload, or returns the
 * enclosing function early with the failure re-wrapped in *its* return type.
 *
 *   - `okVariant` / `failVariant` are the operand's two variants.
 *   - `retEnum` / `retFail` are the enclosing function's return enum and *its* failing
 *     variant, which the early return constructs (carrying the operand's error payload, if
 *     the variant has one).
 */
case class TTry(
    operand: TExpr,
    okVariant: Type.EnumVariant,
    failVariant: Type.EnumVariant,
    retEnum: Type.Enum,
    retFail: Type.EnumVariant,
    ty: Type,
    /** The `impl From[E] for F` this `?` widens the failure through, as the key of the function it
      * lowered to — nothing where the two error types already agree
      * (`reference/errors.md § A ? converts through From`).
      *
      * It is a **name** rather than a node because there is no expression for it to be applied to:
      * what it converts is the failure payload, which exists only inside the branch the emitter
      * builds. `genTry` calls it between unwrapping and re-wrapping.
      */
    convert: Option[String] = None,
) extends TExpr

/** Read field `index` of a struct value. It is also a *place* when its receiver is one, which
 * is what makes `s.f = v` and `p.f = v` (through a pointer) ordinary assignments.
 */
case class TField(receiver: TExpr, index: Int, ty: Type) extends TExpr

/** One term of an `if`'s or a `while`'s condition — the `&&`-joined chain written out
 * (`reference/expressions.md § is — a pattern where a condition is wanted`).
 *
 * A condition is a **list** of these rather than one expression because a term may bind: an `is`
 * that matched leaves names the terms to its right and the guarded branch can read, and that is a
 * fact about the chain's shape, not about a value. An ordinary condition is a one-element list
 * holding a [[TCondTest]], so nothing that never writes `is` pays for it.
 */
sealed trait TCondTerm

/** A plain boolean term — every condition that says nothing about a pattern. */
case class TCondTest(cond: TExpr) extends TCondTerm

/** `subject is Pat` / `subject is not Pat`. The un-negated form's bindings are live from here
 * rightward; the negated form binds nothing, which is why it may hold no binding pattern. Several
 * `patterns` are `|`-alternatives, which share one answer and so may not bind either.
 */
case class TCondIs(subject: TExpr, patterns: List[TPattern], negated: Boolean) extends TCondTerm

/** `if cond then … else …` as a value (or unit when there is no else). */
case class TIf(cond: List[TCondTerm], thenBlock: TBlock, elseBlock: Option[TBlock], ty: Type) extends TExpr

/** `match scrutinee` — arms are tried in order; `ty` is the common arm type (or unit). */
case class TMatch(scrutinee: TExpr, arms: List[TArm], ty: Type) extends TExpr

/** One arm: the scrutinee matches if any alternative pattern holds and the guard (if any) is
 * true. Only non-binding patterns may share an arm as alternatives.
 */
case class TArm(patterns: List[TPattern], guard: Option[TExpr], body: TBlock)

/** A block: a sequence of statements optionally ending in a value expression. When `result`
 * is `None` the block's type is `unit`.
 */
case class TBlock(stmts: List[TStmt], result: Option[TExpr], ty: Type)

/** A block standing where an expression does, which is what one copy of an unrolled `for const`
 * becomes (`reference/generics.md § A parameter may stand for a list of types`).
 *
 * `if` and `match` carry their blocks in their own nodes because the branch is part of what they
 * are; an unrolled loop has no node of its own by the time this exists — it has become its copies —
 * so each copy needs to be an expression in its own right. It is a scope like any other block, which
 * is what keeps a `var` written inside the loop from being one variable redeclared per copy.
 */
case class TBlockExpr(block: TBlock) extends TExpr { def ty: Type = block.ty }

/** `while cond body [else …]` as an expression. `body` runs for effect each iteration; a `break`
 * in it carries the loop's value, and `elseBlock` (if present) supplies the value on normal
 * completion. `ty` is the loop's result type — `unit` when nothing carries a value.
 */
case class TWhile(cond: List[TCondTerm], body: List[TStmt], elseBlock: Option[TBlock], ty: Type) extends TExpr

/** `do body while cond [else …]` — `TWhile`'s shape entered at the body instead of at the test, so
 * the body runs once before `cond` is asked. `continue` targets the test, which is what the `loop` +
 * `if !cond then break` rewrite cannot say.
 */
case class TDoWhile(body: List[TStmt], cond: TExpr, elseBlock: Option[TBlock], ty: Type) extends TExpr

/** `loop body` — the same shape with the condition removed, so the only way out is a `break`.
 * `ty` is the type its `break`s meet at, and `never` where it has none: nothing arrives after a
 * loop that cannot end.
 */
case class TLoop(body: List[TStmt], ty: Type) extends TExpr

/** `for name in lo..hi [else …]` — the loop variable has the integer type `varTy` of its bounds;
 * `ty` is the loop expression's result type.
 */
case class TFor(name: String, varTy: Type, lo: TExpr, hi: TExpr, inclusive: Boolean,
                body: List[TStmt], elseBlock: Option[TBlock], ty: Type) extends TExpr

/** `for init; cond; step [else …]` — the three-clause loop. An absent condition is `true`, and the
 * step is what `continue` runs before the next test.
 */
case class TCFor(init: List[TStmt], cond: Option[TExpr], step: List[TStmt], body: List[TStmt],
                 elseBlock: Option[TBlock], ty: Type) extends TExpr

/** `for all name in lo..hi do pred` / `for some …` — a quantifier over an integer range
 * (`reference/verification.md § for all and for some`), which is an **expression** yielding a
 * `bool` and not a loop: it carries no `break`, no `else`, and no label, so its type is settled
 * rather than met.
 *
 * The bounds share `TFor`'s shape because they are the same range. What differs is the body: a
 * single `TExpr` rather than a statement list, since a predicate is a condition and not a block.
 */
case class TQuantifier(universal: Boolean, name: String, varTy: Type, lo: TExpr, hi: TExpr,
                       inclusive: Boolean, pred: TExpr) extends TExpr { def ty: Type = Type.Bool }

/** A loop whose body carries a `variant`, wrapped so the measure's slots are set up **once per
 * entry** to the loop rather than once per call to the function (`reference/verification.md §
 * invariant and variant on a loop`).
 *
 * It wraps rather than adding a field to each of the seven loop nodes, and what it holds is only
 * what has to happen outside the body: the two allocas and the store that disarms the comparison.
 * The check itself is a `TVariantCheck` among the body's statements, where it was written.
 *
 * A loop with only `invariant` clauses is not wrapped — those need nothing outside the body.
 */
case class TCheckedLoop(slot: String, varTy: Type, loop: TExpr) extends TExpr { def ty: Type = loop.ty }

/** `for name in seq [else …]` over an array or a slice. The loop variable is a *copy* of each
 * element, and the sequence is evaluated once.
 */
case class TForEach(name: String, elemTy: Type, seq: TExpr, body: List[TStmt],
                    elseBlock: Option[TBlock], ty: Type) extends TExpr

/** `for name in cursor [else …]` over a type that implements `Iterate` (`library/core.md § Walking
 * a type of your own`).
 *
 * `cursor` is the name of the loop's own slot holding the iterator: the sequence expression is
 * evaluated once into it, and `next` is the already-built call that reads it and advances it, so
 * the loop's state is the loop's and nothing outside it moves. `bind` is the `Some(name)` pattern
 * that takes the element out of what `next` gave back — a `None` ends the loop, running the `else`
 * exactly as running out of a range does.
 */
case class TIterate(cursor: String, cursorTy: Type, init: TExpr, next: TExpr, bind: TPattern,
                    body: List[TStmt], elseBlock: Option[TBlock], ty: Type) extends TExpr
