package sh.sysl

/** Who is holding a count for a by-value argument while the call it was passed to runs.
 *
 * ==The convention==
 *
 * **A caller guarantees that every by-value argument it hands over stays alive for the whole of the
 * call, and a callee reads its parameters through that guarantee without taking a count of its
 * own.** That is Swift's `guaranteed` convention and Rust's `&self` borrow, and it is the cheap
 * half of a call: `push(*self, v: T)` with a local `v` costs no reference traffic at all, at either
 * end, however much memory the body writes.
 *
 * **The guarantee is one-sided, which is what makes it work across a compilation boundary.** The
 * caller's obligation is the same whatever the callee turns out to do, so it can be discharged
 * against a name, a vtable slot or a function the compiler has never seen. A callee that takes a
 * count anyway is still correct — an extra count held by a frame that gives it back is invisible —
 * so `owning` below is free to be conservative without the caller having to know, and a trait
 * method's implementations may disagree with each other about it.
 *
 * ==Why the caller is the one that can decide==
 *
 * The decision used to sit entirely in the callee (`BorrowedParams`), which could only ask what the
 * *body* does: a body that writes memory might release the very thing it was passed, because a
 * caller is free to hand over a **place** — `f(s.v)` passes the field itself, with no count taken
 * anywhere, so `s.v` is the only owner and `zap(v: V, s: *S)` writing `s.v = other` and then reading
 * `v.obj` reads freed storage. Nothing a callee can see distinguishes that argument from a local.
 *
 * The caller can see it, and it is the only thing it has to look at:
 *
 *   - a **temporary** — a call's result, a box, a branch's value, a struct built out of held parts —
 *     is a count this frame took and gives back after the call returns, so there is nothing to do;
 *   - a **local or parameter whose address was never let out** holds its count in a slot no callee
 *     can name, so there is nothing to do;
 *   - **anything else** — a field, an element, a dereference, a global, a `ref`, a local whose
 *     address escaped — keeps its count in memory the callee may write, so the caller takes one of
 *     its own for the value it loaded and gives it back with the statement's other temporaries.
 *
 * `held` is that classification and is written as a list of what is **allowed**: an unrecognized
 * node has to mean "take a count", or a node added later silently stops guaranteeing anything.
 *
 * ==What the callee still owns, and why each one is not an exception==
 *
 *   - **A parameter whose own slot the body rebinds.** `v = other` inside the callee gives back
 *     whatever was in the slot, and under a borrow that count is the caller's. `ContractAssume.rebound`
 *     already answers exactly this question — every parameter a body may assign to, take the address
 *     of, or bind a `ref` to — so the parameter is owned and the assignment is releasing a count the
 *     entry retain took.
 *   - **Every parameter of a function with a tail self-call.** The jump computes its arguments, then
 *     releases everything the frame holds, then stores into the parameter slots
 *     (`CallEmitter.genTailSelfCall`); the count taken to carry a value across that release is one
 *     the slot has to give back later.
 *   - **Every parameter of a function something outside the program may call** — `@export`, a
 *     `@section`, a foreign calling convention, or an address taken for a `*fn`, which is lowered as
 *     a C call. Nothing on the other side of those discharges a caller's obligation, so the callee
 *     discharges it for itself.
 *
 * A parameter passed **in memory** (`layout.indirect`) is no different in kind, and the only thing
 * that changes is what the caller does when it has to take a count. The callee's entry copy is a copy
 * of *bytes* read out of the caller's storage, so a body that then overwrites that storage frees what
 * the copy points at exactly as it would for a register-passed value. A place argument is therefore
 * staged into a slot of the caller's own with a count taken at it, and the callee copies from
 * **that** — the same snapshot a small argument's loaded value already is. A temporary or an
 * unexposed local is handed over at its own address as before, and nothing is taken at either end:
 * `b.push(V(i, x, node, …))` costs the one retain the buffer takes when it stores the value, where it
 * used to cost a walk of the whole aggregate at entry and another at exit.
 *
 * And a function whose body can release nothing at all keeps the borrow in every one of those cases:
 * `BorrowedParams` is a stronger fact than any of them, because a body with no returning call and no
 * write to memory cannot reach a release however it was entered. That is also what lets a **caller**
 * skip its own retain on a place argument at a call to such a function, which is the one place the
 * old rule is still consulted at a call site.
 */
object CallOwnership {

  /** The parameters of `f` that it takes a count for at entry and gives back at each return. */
  def owning(f: TFunc, addressed: Boolean): Set[String] = {
    val every = f.params.map(_._1).toSet
    // A self-jump is checked before anything else, because the count it takes to carry an argument
    // across the release of the whole frame has to be one the slot gives back afterwards — and that
    // is true even of a body a borrow would otherwise be sound for.
    val jumps = TailCalls.of(f).nonEmpty

    if jumps then every
    // A body that can release nothing keeps the borrow whoever entered it, which is what makes this
    // stronger than every condition below rather than another one beside them.
    else if BorrowedParams.borrows(f) then Set.empty
    else if addressed || f.exported.isDefined || f.section.isDefined || f.conv.isDefined then every
    else
      val bound = ContractAssume.rebound((f.body, f.requires, f.ensures, f.olds, f.variant))

      if bound(ContractAssume.opaque) then every else f.params.map(_._1).filter(bound).toSet
  }

  /** Every function whose address the program takes, which is a caller this side cannot reason
   * about: a `*fn` is called under C's convention (`ExprEmitter`'s `TCallPtr`), so nothing at that
   * call site takes a count for what it hands over.
   *
   * The walk is generic, as `ContractAssume`'s are, because the alternative is a list of every node
   * that can hold one and a silent hole the first time another is added.
   */
  def addressed(x: Any): Set[String] = x match
    case _: Type         => Set.empty
    case a: TFuncAddr    => Set(a.name, a.entry)
    case xs: Iterable[?] => xs.flatMap(addressed).toSet
    case p: Product      => p.productIterator.flatMap(addressed).toSet
    case _               => Set.empty

  /** Whether the caller already holds a count for this argument that outlives the call.
   *
   * **The default is `false`, which is why this is a list of what is allowed.** Every node here is
   * one whose value is either a temporary the enclosing region will release *after* the call, or a
   * slot the callee has no way to name; anything else keeps its count somewhere a callee can reach,
   * and guessing wrong about a new node is a use-after-free rather than a slow program.
   *
   * `exposed` is `ContractAssume.exposed` over the **caller's** body, plus its promoted locals: the
   * names whose storage a call it makes could change under it.
   */
  def held(a: TExpr, exposed: Set[String]): Boolean = a match
    // A local or parameter the caller never let the address of out. Its slot holds the count for as
    // long as the scope does, and no callee has a name for the slot.
    case TLoad(name, _) => !exposed(ContractAssume.opaque) && !exposed(name)

    // A result, a box and a branch's value all arrive with a count already taken, registered in the
    // region the enclosing statement closes — which is after the call this is an argument to.
    case _: TCall | _: TVCall | _: TCallPtr | _: TBox | _: TIf | _: TMatch | _: TBlockExpr => true

    // Building a value takes no count of its own, so what it stands on is what has to be held.
    case TStructNew(_, args)  => args.forall(held(_, exposed))
    case TEnumNew(_, _, args) => args.forall(held(_, exposed))

    // Neither changes what is counted or where the count is: a cast is a reinterpretation, and
    // erasing to a trait object goes on pointing at the box it pointed at.
    case TCast(o, _)     => held(o, exposed)
    case TErase(o, _, _) => held(o, exposed)

    // A literal owns nothing and outlives everything. A string's is the case worth naming: it is a
    // view whose owner word is a null constant (`StringEmitter.stringConst`), so there is no count
    // to lose and taking one would be an `arc.retain_maybe(null)` that does nothing — while pulling
    // the whole ARC runtime into a program whose only reference is the text inside a `print`.
    case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TUnitLit | _: TNullLit | _: TZero |
         _: TStrLit | _: TCStrLit => true

    case _ => false
}
