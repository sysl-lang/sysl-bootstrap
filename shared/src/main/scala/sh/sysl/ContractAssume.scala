package sh.sysl

/** A postcondition read at a **call site** rather than inside the function that declared it —
 * the rewriting half of what `ContractEmitter.assumeEnsures` lays down.
 *
 * **The problem is that a clause is written in the callee's vocabulary.** `ensure self.elems.len >=
 * need` names two parameters and, where there is a result, the value about to be returned; none of
 * those is anything the caller has a name for. What the caller *does* have is the arguments it just
 * passed and the register the call answered with, so the clause is rewritten in terms of those: each
 * parameter becomes the argument that filled it, and `result` stays as it is because the emitter
 * binds it to the returned register while lowering.
 *
 * **Re-evaluating an expression somewhere else is only sound where the two readings agree**, and
 * three things can break that, each answered below:
 *
 *   1. The clause may not be re-runnable at all — it may call something, allocate, or trap. That is
 *      not decided here: the clause is lowered and the *instructions* are read
 *      (`Emitter.tryPure`), which is a stronger test than any list of node kinds and cannot go stale.
 *      What this file rules out are the two shapes that would not even lower in a caller's frame:
 *      `old(e)`, whose snapshot slot belongs to the callee, and `result` where the call left no
 *      register to bind it to.
 *   2. An argument may not read the same thing twice — `f(next())` passes one value and would
 *      compute a second one here, and `f(&n, n)` passes a value the call is then free to overwrite
 *      before this reads it again. So an argument is substituted only where it is `stable`: built
 *      from literals and locals the call cannot reach.
 *   3. **The callee may have changed its own copy of a parameter before the check ran.** The clause
 *      is checked at the callee's returns, reading the callee's slots; the caller re-reads its own
 *      arguments, which are the values those slots were *entered* with. Where the body assigns to a
 *      parameter — or to a field or element of one, or takes its address — the two are different
 *      values and the substitution would assume something the callee never established. `rebound`
 *      finds those and they are left alone.
 *
 * Writing **through** a parameter is the case that deliberately does not disqualify it, and it is
 * the one the feature exists for: `*self` is a pointer, `self.count = n` writes the caller's own
 * object, and re-reading `self.count` here reads exactly what the callee's check read. `rebound`
 * stops at a dereference for that reason.
 */
object ContractAssume {

  /** The name that stands for "something was reached that could have written anything", which no
   * parameter can be called — `Modules.qualify` writes a separator into every key.
   */
  val opaque = "*"

  /** What a body that has not said which of its locals a call can reach exposes: everything. */
  val unknown: Set[String] = Set(opaque)

  /** One clause of `params`' function, rewritten for a caller that passed `args`, or nothing where
   * it cannot be.
   *
   * `written` is what the callee's body may have rebound (`rebound`), `exposed` what the *caller*
   * must assume the call can change under it (`exposed`), and `hasResult` whether the call left a
   * register for `result` to name — a clause mentioning `result` at a call with none is refused
   * rather than lowered into a frame that has no value to bind.
   */
  def atCall(clause: TExpr, params: List[(String, Type)], args: List[TExpr], written: Set[String],
             hasResult: Boolean, exposed: Set[String]): Option[TExpr] = {
    if written(opaque) then None
    else
      val sub = params.map(_._1).zip(args)
        .filterNot((name, _) => written(name))
        .filter((_, arg) => stable(arg, exposed))
        .toMap

      port(clause, sub, hasResult)
  }

  /** A struct's `invariant` clause, read over a receiver's fields rather than over the parameters
   * its synthesised function takes them as — `sub` maps each field's name to the read of it.
   *
   * **A module-level name is refused here where `atCall` keeps it**, because the two facts are
   * about different moments. A postcondition is repeated on the instruction after the check that
   * established it; an invariant is assumed at a member's entry, arbitrarily long after the write
   * that checked it, and a `var` the clause read may have moved in between with nothing re-checking
   * the struct. A constant never reaches here as a name — it is folded into the clause — so what
   * this gives up is exactly the storage that could have changed.
   */
  def substitute(clause: TExpr, sub: Map[String, TExpr]): Option[TExpr] =
    port(clause, sub, hasResult = false, globals = false)

  /** The clause in the caller's terms, or nothing. A parameter the substitution has no entry for is
   * one whose argument could not be moved here, and a clause naming it cannot be repeated at all.
   */
  private def port(e: TExpr, sub: Map[String, TExpr], hasResult: Boolean,
                   globals: Boolean = true): Option[TExpr] = {
    def go(x: TExpr) = port(x, sub, hasResult, globals)

    e match
      case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TNullLit | _: TUnitLit | _: TStrLit =>
        Some(e)

      case _: TGlobal => Option.when(globals)(e)

      case _: TResult => Option.when(hasResult)(e)

      // The one node that cannot be moved: the snapshot is a slot in the callee's frame, and a
      // caller has neither the slot nor the entry value it was taken from.
      case _: TOld => None

      case TLoad(name, _) => sub.get(name)

      case TField(r, i, ty)      => go(r).map(TField(_, i, ty))
      case TDeref(o, ty)         => go(o).map(TDeref(_, ty))
      case TLen(r, ty)           => go(r).map(TLen(_, ty))
      case TCast(o, ty)          => go(o).map(TCast(_, ty))
      case TAddrOf(p, ty)        => go(p).map(TAddrOf(_, ty))
      case TUnary(op, o, ty)     => go(o).map(TUnary(op, _, ty))
      case TBinary(op, l, r, ty) => for a <- go(l); b <- go(r) yield TBinary(op, a, b, ty)
      case TLogical(op, l, r)    => for a <- go(l); b <- go(r) yield TLogical(op, a, b)

      case TCompare(operands, cmps) =>
        val ported = operands.map(go)

        Option.when(ported.forall(_.isDefined))(TCompare(ported.map(_.get), cmps))

      case _ => None
  }

  /** Whether reading this argument again, on the instruction after the call it was passed to,
   * reads what the callee was handed.
   *
   * **It is a list of what is allowed rather than of what is not, because the dangerous case is the
   * ordinary-looking one.** `f(&n, n)` hands over a pointer and a value, and the callee is free to
   * write through the pointer before it returns — so re-reading `n` here yields a number the
   * promise was never made about, and assuming a false thing is worse than assuming nothing. The
   * same is true of a global, which any call may write, and of a `ref`, which is somebody else's
   * storage wearing a local's name.
   *
   * What is left is a value the call cannot have touched: a constant, a local whose address the
   * caller never let out (`exposed`), the address of a slot — which does not move whatever is
   * written into it — and arithmetic over those.
   */
  private def stable(e: TExpr, exposed: Set[String]): Boolean = {
    def go(x: TExpr) = stable(x, exposed)

    e match
      case _: TIntLit | _: TFloatLit | _: TBoolLit | _: TNullLit | _: TUnitLit | _: TStrLit => true

      case TLoad(name, _)        => !exposed(opaque) && !exposed(name)
      case TAddrOf(p, _)         => p.isInstanceOf[TLoad] || p.isInstanceOf[TGlobal]
      case TCast(o, _)           => go(o)
      case TUnary(_, o, _)       => go(o)
      case TBinary(_, l, r, _)   => go(l) && go(r)
      case TLogical(_, l, r)     => go(l) && go(r)
      case TCompare(operands, _) => operands.forall(go)
      case _                     => false
  }

  /** Every local of a caller whose value a **call it makes** could change under it, plus `opaque`
   * where something was reached that cannot be reasoned about at all.
   *
   * A callee reaches a caller's local only through an address the caller let out, so taking one is
   * what puts a name in here — and a `ref` is in for the same reason under a different spelling,
   * since the name stands for storage the caller does not own.
   */
  def exposed(x: Any): Set[String] = x match
    case _: Type         => Set.empty
    case a: TAddrOf      => root(a.place) ++ a.productIterator.flatMap(exposed)
    case r: TRefDecl     => Set(r.name) ++ root(r.place) ++ r.productIterator.flatMap(exposed)
    case _: TAsm         => Set(opaque)
    case xs: Iterable[?] => xs.flatMap(exposed).toSet
    case p: Product      => p.productIterator.flatMap(exposed).toSet
    case _               => Set.empty

  /** Every parameter whose **own storage** a body may have changed before its postcondition was
   * checked, plus `opaque` where something was reached that cannot be reasoned about at all.
   *
   * The walk is generic, as `Ghost.mentions` is, because the alternative is a list of every node
   * that holds a place and a silent hole the first time one is added.
   */
  def rebound(x: Any): Set[String] = x match
    case _: Type          => Set.empty
    case s: TStore        => root(s.place) ++ s.productIterator.flatMap(rebound)
    case u: TUpdate       => root(u.place) ++ u.productIterator.flatMap(rebound)
    case d: TIncDec       => root(d.place) ++ d.productIterator.flatMap(rebound)
    case a: TAddrOf       => root(a.place) ++ a.productIterator.flatMap(rebound)
    case w: TWrite        => root(w.place) ++ w.productIterator.flatMap(rebound)
    case r: TRefDecl      => root(r.place) ++ r.productIterator.flatMap(rebound)
    // A block of assembly names its operands' storage directly and says what it does to them in a
    // constraint string, which is not something to parse for this.
    case _: TAsm          => Set(opaque)
    case xs: Iterable[?]  => xs.flatMap(rebound).toSet
    case p: Product       => p.productIterator.flatMap(rebound).toSet
    case _                => Set.empty

  /** The local whose storage a place reaches, where it reaches one.
   *
   * **It stops at a dereference on purpose.** `*p = 1` writes what `p` points at, which is the
   * caller's object and is what both readings of the clause see; `p = q` would write the slot, and
   * only that is what disqualifies a parameter.
   */
  private def root(e: TExpr): Set[String] = e match
    case TLoad(name, _)  => Set(name)
    case TField(r, _, _) => root(r)
    case TIndex(r, _, _) => root(r)
    case TLane(r, _, _)  => root(r)
    case _               => Set.empty
}
