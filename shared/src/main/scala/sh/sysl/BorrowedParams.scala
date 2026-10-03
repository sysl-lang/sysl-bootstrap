package sh.sysl

import scala.collection.mutable

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
 * dropped only where the body cannot, by any path that comes back, release anything it did not take:
 * no call that returns into something that might, and no write to memory but its own locals'. What
 * that leaves is the reader — an accessor, a length, a predicate over fields, and whatever calls only
 * those — which is exactly where the pair costs most, because there is nothing else in the function
 * for it to hide behind. It is also what lets a caller hand such a function a place with no snapshot
 * at all (`CallEmitter.indirectArg`): a body that can write nothing cannot change what it was given.
 *
 * A call that cannot return is not an exception to the rule but an application of it: control never
 * comes back to the release, so whatever it does with the value is not observable through the
 * borrow. That is what makes an out-of-line panic free here, and why the bounds-checked members of
 * `sysl.buf` and `sysl.container.ring` report through a `-> never` helper rather than inline.
 *
 * **A call is allowed where its callee is itself one of these**, which is what lets the rule reach
 * past a leaf: whether a body can release anything is a question about everything it reaches, so
 * it is answered over the whole program at once (`inert`). A function that calls only functions
 * that can release nothing cannot release anything either, and a cycle of them — recursion — is
 * no different, since a release needs a finite path to it and none of the members has one. That is
 * the *greatest* set closed under the rule, which is why the computation starts from every candidate
 * and only ever removes one.
 *
 * **A write to the function's own local is allowed too**, for the reason a declaration already is:
 * the count the slot gives back is the one the slot took when it was written, so nothing the caller
 * is holding is let go. Only a `var` the body itself declared qualifies — a parameter's slot holds
 * the caller's count under this very borrow, and a `ref` names storage somebody else owns.
 */
object BorrowedParams {

  /** The functions whose by-value parameters may be read through the caller's count: every body
   * that can neither write memory it does not own nor call anything that might.
   */
  def inert(funcs: List[TFunc]): Set[String] = {
    // A contract clause is code too, and it is emitted around the body rather than inside it, so a
    // function carrying one is left alone rather than reasoned about twice.
    val candidates = funcs.filter(f =>
      f.requires.isEmpty && f.ensures.isEmpty && f.olds.isEmpty && f.variant.isEmpty)
    var set     = candidates.map(_.name).toSet
    var changed = true

    while changed do
      val keep = candidates.filter(f => set(f.name) && new Scan(set, owned(f)).blockOk(f.body))
      changed = keep.length != set.size
      set = keep.map(_.name).toSet

    set
  }

  /** The names whose slots are this body's own: every `var`/`val` it declares, less any spelling a
   * parameter or a `ref` shares, since a name declared twice is one the write cannot be told apart
   * by.
   */
  private def owned(f: TFunc): Set[String] = {
    val declared = mutable.Set.empty[String]
    val refs     = mutable.Set.empty[String]

    def walk(x: Any): Unit = x match
      case _: Type         => ()
      case d: TVarDecl     => declared += d.name; walk(d.init)
      case r: TRefDecl     => refs += r.name; walk(r.place)
      case xs: Iterable[?] => xs.foreach(walk)
      case p: Product      => p.productIterator.foreach(walk)
      case _               => ()

    walk(f.body)
    declared.toSet -- refs -- f.params.map(_._1)
  }

  /** The place a write lands in, where that is one of `locals` reached without a dereference. */
  private def ownPlace(place: TExpr, locals: Set[String]): Boolean = place match
    case TLoad(name, _)  => locals(name)
    case TField(r, _, _) => ownPlace(r, locals)
    case TIndex(r, _, _) => ownPlace(r, locals)
    case _               => false

  private final class Scan(inertSet: Set[String], locals: Set[String]) {

    def blockOk(b: TBlock): Boolean = stmtsOk(b.stmts) && b.result.forall(exprOk)

    private def stmtsOk(stmts: List[TStmt]): Boolean = stmts.forall {
      // A local's own storage is the function's, and the count it holds is one the function took,
      // so giving it back at the end of the block gives back nothing the caller was relying on.
      case TVarDecl(_, _, init, _) => exprOk(init)
      case TRefDecl(_, _, place)   => exprOk(place)
      case TExprStmt(e)            => exprOk(e)
      case TReturn(v)              => v.forall(exprOk)
      case TBreak(v, _)            => v.forall(exprOk)
      case _: TContinue            => true
      case _                       => false
    }

    /** Whether this expression, and everything under it, can neither hand control to code that
     * might release nor write to memory the function does not own.
     *
     * **The default is `false`, which is the whole reason this is written as a list of what is
     * allowed.** A node added later is unknown to this, and unknown has to mean "keep the retain" —
     * the other way round, a new way of reaching arbitrary code would silently start dropping
     * counts that were holding something up.
     */
    private def exprOk(e: TExpr): Boolean = nodeOk(e) && TreeWalk.children(e).forall(exprOk) &&
      ownBlocks(e).forall(blockOk)

    private def dispatchOk(d: Option[TDispatch]): Boolean = d.forall(x => inertSet(x.name))

    private def nodeOk(e: TExpr): Boolean = e match
      // A call that does not come back never reaches the release this is deciding about, so what it
      // does to the value is not something the borrow can observe. One that does come back is
      // allowed where its callee is in the set too.
      case c: TCall => c.ty == Type.Never || inertSet(c.name)

      case TStore(place, _, _)                  => ownPlace(place, locals)
      case TUpdate(place, _, _, _, dispatch, _) => ownPlace(place, locals) && dispatchOk(dispatch)
      case TIncDec(place, _, _, _, _)           => ownPlace(place, locals)

      // A comparison may be a trait method rather than an instruction, and then it is a call.
      case TCompare(_, cmps) => cmps.forall(c => dispatchOk(c.dispatch))

      case _: TIntLit | _: TFloatLit | _: TStrLit | _: TBoolLit | _: TUnitLit | _: TNullLit |
           _: TZero | _: TCStrLit | _: TLoad | _: TGlobal | _: TFuncAddr => true

      case _: TCast | _: TDeref | _: TAddrOf | _: TTypeId | _: TTempAddr => true

      case _: TBinary | _: TUnary | _: TIntOp | _: TLogical | _: TSeq => true

      case _: TField | _: TIndex | _: TLen | _: TBytes | _: TConstView | _: TSlice | _: TStrView => true

      case _: TIf | _: TMatch | _: TBlockExpr => true

      case _: TWhile | _: TDoWhile | _: TLoop | _: TFor => true

      case _: TVectorLit | _: TSplat | _: TVecCompare | _: TSelect | _: TReduce | _: TLane |
           _: TVecLoad => true

      // Building a value takes shares of what goes into it and gives none back, so none of these
      // can be the release the rule is looking for.
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
}
