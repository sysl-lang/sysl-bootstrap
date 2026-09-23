package sh.sysl

/** Walking a typed tree without caring what is in it.
 *
 * Two passes ask the same structural questions of a body — which statements it contains once loops
 * and branches are followed, and which sub-expressions each node has — and both would answer them
 * with the same total match over the node types. Written twice, the two copies drift the moment a
 * node is added, and a pass that silently stops descending is a pass that silently stops checking.
 * So the descent is written once, here, and the passes bring only what they are looking for.
 */
object TreeWalk {

  /** Applies `f` to every statement, including the ones nested in loop and branch bodies. Loops
   * are expressions, so they are reached through the `blocks` an expression carries rather
   * than as statements of their own.
   */
  def forEachStmt(stmts: List[TStmt])(f: PartialFunction[TStmt, Unit]): Unit =
    for s <- stmts do
      f.applyOrElse(s, (_: TStmt) => ())
      s match
        case TExprStmt(e)      => blocks(e).foreach(b => forEachStmt(b.stmts)(f))
        case TVarDecl(_, _, e, _) => blocks(e).foreach(b => forEachStmt(b.stmts)(f))
        // A ref's place is an expression like any other — its index may be a branch, and a branch is
        // a block. The walk goes through it for the same reason it goes through an initializer.
        case TRefDecl(_, _, e) => blocks(e).foreach(b => forEachStmt(b.stmts)(f))
        case TReturn(Some(e))  => blocks(e).foreach(b => forEachStmt(b.stmts)(f))
        case TBreak(Some(e), _) => blocks(e).foreach(b => forEachStmt(b.stmts)(f))
        case TMultiAssign(writes) =>
          for w <- writes; e <- List(w.place, w.value); b <- blocks(e) do forEachStmt(b.stmts)(f)
        // A deferred statement runs where its block is left rather than where it stands, but it is
        // still a statement of this body — so every pass that asks what a body contains has to see
        // it. A walk that stopped here would leave the escape analysis blind to a view a deferred
        // statement lets out.
        case TDefer(stmts) => forEachStmt(stmts)(f)
        case _ =>

  /** The `break` values that belong to a loop with these body statements — those in its body but
   * not inside a nested loop, whose `break`s are that loop's. Walks through the branch bodies of
   * `if`/`match`, which do not introduce a new loop.
   */
  def ownBreakValues(stmts: List[TStmt]): List[TExpr] = stmts.flatMap {
    case TBreak(Some(v), _) => List(v)
    case TExprStmt(e)       => ownBreaksInExpr(e)
    case TVarDecl(_, _, e, _)  => ownBreaksInExpr(e)
    case TRefDecl(_, _, e)  => ownBreaksInExpr(e)
    case TReturn(Some(e))   => ownBreaksInExpr(e)
    case TMultiAssign(writes) => writes.flatMap(w => ownBreaksInExpr(w.place) ::: ownBreaksInExpr(w.value))
    case _                  => Nil
  }

  def ownBreaksInExpr(e: TExpr): List[TExpr] = e match
    case _: TWhile | _: TDoWhile | _: TLoop | _: TFor | _: TForEach | _: TCFor | _: TIterate => Nil
    case TIf(_, t, el, _)   => ownBreakValues(t.stmts) ::: el.toList.flatMap(b => ownBreakValues(b.stmts))
    case TMatch(_, arms, _) => arms.flatMap(a => ownBreakValues(a.body.stmts))
    case _                  => Nil

  /** Every block an expression contains, so a statement nested inside an `if`, a `match`, or a
   * loop used as a value is walked too. A loop's body is wrapped as a block; its `else` is one.
   */
  def blocks(e: TExpr): List[TBlock] = e match
    case TIf(_, t, el, _)   => t :: el.toList ::: children(e).flatMap(blocks)
    case TMatch(_, arms, _) => arms.map(_.body) ::: children(e).flatMap(blocks)
    case TWhile(_, body, el, _)           => TBlock(body, None, Type.Unit) :: el.toList ::: children(e).flatMap(blocks)
    case TDoWhile(body, _, el, _)         => TBlock(body, None, Type.Unit) :: el.toList ::: children(e).flatMap(blocks)
    case TLoop(body, _)                   => TBlock(body, None, Type.Unit) :: children(e).flatMap(blocks)
    // The init and the step are statements of the loop's own scope, so they are walked as part of
    // the block its body makes rather than as expressions beside it.
    case TCFor(init, _, step, body, el, _) =>
      TBlock(init ::: body ::: step, None, Type.Unit) :: el.toList ::: children(e).flatMap(blocks)
    case TFor(_, _, _, _, _, body, el, _) => TBlock(body, None, Type.Unit) :: el.toList ::: children(e).flatMap(blocks)
    case TForEach(_, _, _, body, el, _)   => TBlock(body, None, Type.Unit) :: el.toList ::: children(e).flatMap(blocks)
    case TIterate(_, _, _, _, _, body, el, _) =>
      TBlock(body, None, Type.Unit) :: el.toList ::: children(e).flatMap(blocks)
    case _                  => children(e).flatMap(blocks)

  /** The expressions a condition evaluates — the boolean terms, and the subject each `is` tests
   * (`reference/expressions.md § is — a pattern where a condition is wanted`). A pattern holds no
   * expression of its own worth walking: its literals and range ends are constants, and its
   * bindings are places rather than reads.
   */
  def condExprs(terms: List[TCondTerm]): List[TExpr] = terms.map {
    case TCondTest(c)        => c
    case TCondIs(subj, _, _) => subj
  }

  def children(e: TExpr): List[TExpr] = e match
    case TBox(v, _)                 => List(v)
    case TCast(v, _)                => List(v)
    case TDeref(v, _)               => List(v)
    case TAddrOf(v, _)              => List(v)
    case TTypeId(v, _)              => List(v)
    case TTempAddr(v, _)            => List(v)
    case TStore(p, v, _)            => List(p, v)
    case TUpdate(p, _, v, _, _, _)  => List(p, v)
    case TIncDec(p, _, _, _, _)     => List(p)
    case TBinary(_, l, r, _)        => List(l, r)
    case TUnary(_, v, _)            => List(v)
    case TIntOp(_, v, n, _, _)      => v :: n.toList
    // The ordering is a keyword rather than an operand, so a walk sees the address and the values.
    case TAtomic(_, a, ops, _, _, _) => a :: ops
    case TFence(_)                  => Nil
    case TLogical(_, l, r)          => List(l, r)
    case TCompare(ops, _)           => ops
    case TSeq(exprs)                => exprs
    case TCall(_, args, _, _)       => args
    // A function's address has no operand — the symbol is a constant, not something evaluated.
    case TFuncAddr(_, _, _)         => Nil
    case TCallPtr(callee, args, _, _) => callee :: args
    // Which function a trait object's call reaches is a run-time word, so there is no parameter
    // list to ask whether an argument is kept — a callee that might keep anything is exactly what
    // a name the summaries do not recognise already stands for.
    case TVCall(r, _, args, _, _)   => r :: args
    case TErase(v, _, _)            => List(v)
    case TStructNew(_, args)        => args
    case TStructInvCheck(v, _, _)   => List(v)
    case TRecheck(after, recv, _, _) => List(after, recv)
    case TEnumNew(_, _, args)       => args
    case TEnumFromInt(v, _)         => List(v)
    case TEnumTry(v, _, _, _, _)    => List(v)
    case TDowngrade(v, _)           => List(v)
    case TUpgrade(v, _, _, _)       => List(v)
    case TArrayLit(elems, _)        => elems
    // The vector forms. None of them can carry a reference — a lane is a scalar, so there is no
    // count to take and nothing to escape through one — and they are listed anyway, because the
    // catch-all at the bottom of this match returns `Nil` and a node missing from here is a node
    // whose *operands* are invisible to every walk that uses this. `(f() < g()).select(a, b)` has
    // four calls in it and would have had none.
    case TVectorLit(lanes, _)       => lanes
    case TSplat(v, _)               => List(v)
    case TVecCompare(_, l, r, _)    => List(l, r)
    case TSelect(m, a, b, _)        => List(m, a, b)
    case TReduce(_, r, _)           => List(r)
    case TLane(r, _, _)             => List(r)
    // The two that reach memory, and the two that make the paragraph above more than a precaution:
    // a load's receiver is a slice, which *can* carry a reference, so leaving these out would hide
    // an owner from the escape and ARC walks rather than merely hiding a call.
    case TVecLoad(r, i, _)          => List(r, i)
    case TVecStore(r, i, v)         => List(r, i, v)
    case TArrayFill(v, _)           => List(v)
    case TBufLit(elems, _)          => elems
    case TBufFill(v, n, _)          => List(v, n)
    case TIndex(r, i, _)            => List(r, i)
    case TLen(r, _)                    => List(r)
    case TBytes(r)                  => List(r)
    case TStr(a)                    => List(a)
    // The string it yields owns a copy, so nothing of the argument's storage survives in it — the
    // walk is here for the argument's own sake.
    case TFromBytes(a)              => List(a)
    case TStrView(a)                => List(a)
    case TConstView(a)              => List(a)
    case TFormat(a, _)              => List(a)
    // A render's result is a fresh string that owns its own bytes, so it views nothing; what
    // is worth walking is the value and the specifier it hands the implementation.
    case TRender(v, _, s, _)        => List(v, s)
    case TSlice(b, lo, hi, _, _)    => b :: lo.toList ::: hi.toList
    // A `va_list` carries no view of this frame, and a value read out of the tail came from the
    // caller's — so walking these finds nothing, and they are here to keep the walk complete
    // rather than because anything can escape through them.
    case TVaStart(ap)               => List(ap)
    case TVaEnd(ap)                 => List(ap)
    case TVaArg(ap, _)              => List(ap)
    case TVaCopy(d, s)              => List(d, s)
    case TVaPass(ap)                => List(ap)
    case TTry(v, _, _, _, _, _, _)  => List(v)
    case TField(r, _, _)            => List(r)
    case TIf(c, t, el, _)           => condExprs(c) ::: t.result.toList ::: el.flatMap(_.result).toList
    // One copy of an unrolled `for const` (`reference/generics.md § A parameter may stand for a
    // list of types`). Its value is what the block yields, exactly as an `if` branch's is — the
    // statements are reached the way every other block's are.
    case TBlockExpr(b)              => b.result.toList
    case TMatch(s, arms, _)         => s :: arms.flatMap(a => a.guard.toList ::: a.body.result.toList)
    // A loop's own sub-expressions plus its `else` value; the `break` values are reached through the
    // body statements, so they are not repeated here.
    case TWhile(c, _, el, _)             => condExprs(c) ::: el.flatMap(_.result).toList
    case TDoWhile(_, c, el, _)           => c :: el.flatMap(_.result).toList
    case TFor(_, _, lo, hi, _, _, el, _) => lo :: hi :: el.flatMap(_.result).toList
    case TForEach(_, _, seq, _, el, _)   => seq :: el.flatMap(_.result).toList
    // The cursor's initializer and the `next` call that reads it are both the loop's own, and the
    // element the loop binds comes out of the second — so both are walked.
    case TIterate(_, _, init, next, _, _, el, _) => init :: next :: el.flatMap(_.result).toList
    case _                          => Nil
}
