package sh.sysl

/** When a function may read its by-value parameters without taking a count of its own.
 *
 * A caller hands an argument over with a count it is itself holding — its own variable's, or a
 * temporary's — and stays blocked for the whole of the call, so the value cannot go away while the
 * callee runs *unless something inside the call gives back a count nobody inside the call took*.
 * A function that does nothing of the kind can read a parameter through the caller's share and skip
 * the retain at entry and the release at each return. That is the `guaranteed` convention Swift
 * passes ordinary arguments under and the `&self` borrow Rust writes; the difference is only in
 * what establishes it, which here is the test below rather than a checker or a written mode.
 *
 * **The test is deliberately crude, because what it has to be is obviously right.** A retain is
 * dropped only where the body cannot, by any path that comes back, release anything at all: no call
 * that returns, and no write to memory. What that leaves is the small leaf reader — an accessor, a
 * length, a predicate over fields — which is exactly where the pair costs most, because there is
 * nothing else in the function for it to hide behind.
 *
 * A call that cannot return is not an exception to the rule but an application of it: control never
 * comes back to the release, so whatever it does with the value is not observable through the
 * borrow. That is what makes an out-of-line panic free here, and why the bounds-checked members of
 * `sysl.buf` and `sysl.container.ring` report through a `-> never` helper rather than inline.
 *
 * Widening this past a leaf needs a summary of what a callee may release that it does not own, which
 * is an interprocedural question and is not asked anywhere yet.
 */
object BorrowedParams {

  /** Whether this function's by-value parameters may be read through the caller's count. */
  def borrows(f: TFunc): Boolean =
    // A contract clause is code too, and it is emitted around the body rather than inside it, so a
    // function carrying one is left alone rather than reasoned about twice.
    f.requires.isEmpty && f.ensures.isEmpty && f.olds.isEmpty && f.variant.isEmpty &&
      blockOk(f.body)

  private def blockOk(b: TBlock): Boolean = stmtsOk(b.stmts) && b.result.forall(exprOk)

  private def stmtsOk(stmts: List[TStmt]): Boolean = stmts.forall {
    // A local's own storage is the function's, and the count it holds is one the function took, so
    // giving it back at the end of the block gives back nothing the caller was relying on.
    case TVarDecl(_, _, init, _) => exprOk(init)
    case TRefDecl(_, _, place)   => exprOk(place)
    case TExprStmt(e)            => exprOk(e)
    case TReturn(v)              => v.forall(exprOk)
    case TBreak(v, _)            => v.forall(exprOk)
    case _: TContinue            => true
    case _                       => false
  }

  /** Whether this expression, and everything under it, can neither hand control to code that
   * returns nor write to memory.
   *
   * **The default is `false`, which is the whole reason this is written as a list of what is
   * allowed.** A node added later is unknown to this, and unknown has to mean "keep the retain" —
   * the other way round, a new way of reaching arbitrary code would silently start dropping counts
   * that were holding something up.
   */
  private def exprOk(e: TExpr): Boolean = nodeOk(e) && TreeWalk.children(e).forall(exprOk) &&
    ownBlocks(e).forall(blockOk)

  private def nodeOk(e: TExpr): Boolean = e match
    // A call that does not come back never reaches the release this is deciding about, so what it
    // does to the value is not something the borrow can observe.
    case c: TCall => c.ty == Type.Never

    case _: TIntLit | _: TFloatLit | _: TStrLit | _: TBoolLit | _: TUnitLit | _: TNullLit |
         _: TZero | _: TCStrLit | _: TLoad | _: TGlobal | _: TFuncAddr => true

    case _: TCast | _: TDeref | _: TAddrOf | _: TTypeId | _: TTempAddr => true

    case _: TBinary | _: TUnary | _: TIntOp | _: TLogical | _: TCompare | _: TSeq => true

    case _: TField | _: TIndex | _: TLen | _: TBytes | _: TConstView | _: TSlice => true

    case _: TIf | _: TMatch | _: TBlockExpr => true

    case _: TWhile | _: TDoWhile | _: TLoop | _: TFor => true

    case _: TVectorLit | _: TSplat | _: TVecCompare | _: TSelect | _: TReduce | _: TLane |
         _: TVecLoad => true

    // Building a value takes shares of what goes into it and gives none back, so none of these can
    // be the release the rule is looking for.
    case _: TStructNew | _: TEnumNew | _: TEnumFromInt | _: TEnumTry | _: TErase | _: TDowngrade =>
      true

    case _ => false

  /** The blocks this node holds directly — an `if`'s arms, a `match`'s, a loop's body.
   *
   * `TreeWalk.blocks` answers the same question for a whole subtree, which walks a nested node once
   * per ancestor; this pairs with the descent through `TreeWalk.children` above so that every node
   * is seen once.
   */
  private def ownBlocks(e: TExpr): List[TBlock] = e match
    case TIf(_, t, el, _)                 => stmts(t) :: el.toList.map(stmts)
    case TMatch(_, arms, _)               => arms.map(a => stmts(a.body))
    case TWhile(_, bs, el, _)           => body(bs) :: el.toList.map(stmts)
    case TDoWhile(bs, _, el, _)         => body(bs) :: el.toList.map(stmts)
    case TLoop(bs, _)                   => List(body(bs))
    case TFor(_, _, _, _, _, bs, el, _) => body(bs) :: el.toList.map(stmts)
    case TBlockExpr(b)                    => List(stmts(b))
    case _                                => Nil

  /** A block's statements without its result, which `TreeWalk.children` has already handed back as
   * a sub-expression of the node holding it. Walking it from both sides would visit a nested node
   * once per level of nesting.
   */
  private def stmts(b: TBlock): TBlock = TBlock(b.stmts, None, b.ty)

  /** A loop body, which is a statement list rather than a block and so yields nothing. */
  private def body(ss: List[TStmt]): TBlock = TBlock(ss, None, Type.Unit)
}
