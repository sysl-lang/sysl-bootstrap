package sh.sysl

/** Which of a function's calls to itself are the last thing it does, and so can be a jump back to
 * its own entry instead of a second frame (`reference/declarations.md § Tail calls`).
 *
 * The whole of the transformation is here and in `Codegen.genFunction`, and the division is the
 * usual one: this pass decides, codegen emits. A call named here is one where replacing the frame
 * is invisible — the caller's locals are already released along the same edge a `return` would
 * release them on, and what the callee does with the frame is what the caller was going to do with
 * the value it handed back, which is return it.
 *
 * **Missing a tail call costs nothing; naming one wrongly costs everything.** So the walk below is
 * deliberately incomplete rather than deliberately clever: it recognizes the shapes a recursive
 * function is actually written in — the body's trailing expression, a `return`, and the arms of the
 * `if` and `match` those reach through — and says no to anything it has not thought about. A
 * self-call it fails to recognize is compiled as an ordinary call, which is what it was before this
 * pass existed. That is also why `@tailrec` exists: it turns a silent miss into a diagnostic for
 * the one reader who was counting on the jump.
 */
object TailCalls {

  /** The self-calls in `f` that codegen may lower as a jump to its entry.
   *
   * Compared by **identity** at the emission site rather than by value: two calls written with the
   * same arguments are equal case classes and different tail positions, and the list is a handful
   * of entries at most, so a scan over it is cheaper than any keying that would tell them apart.
   */
  def of(f: TFunc): List[TCall] =
    if disqualified(f).isDefined then Nil
    else block(f, f.body, deferred = false)

  /** Why a function is not a candidate at all, as the sentence `@tailrec` reports — `None` where it
   * is one. These are properties of the *declaration*, so they are answered once rather than at
   * each call, and they are the cases where a jump would change what the program does rather than
   * only what it costs.
   */
  def disqualified(f: TFunc): Option[String] = f match
    // A variadic function's tail is read relative to its last named argument (`reference/ffi.md §
    // Variadic functions`), and the arguments a jump rebinds are the named ones — so the second
    // iteration would walk the first call's tail. Nothing here can rebind it, and a `va_list` into
    // a frame that is being reused is worse than a missed optimization.
    case _ if f.variadic =>
      Some("it is variadic, and a jump back to the entry cannot rebind the tail the first call was " +
        "passed")

    // A postcondition is checked **when a call returns**, and a tail call never returns: the frame
    // that would have checked it is the frame being replaced. Optimizing anyway would leave only
    // the last iteration's `ensures` running, so a contract that fails at every depth but the last
    // would stop being reported — a check quietly weakened by a decision about speed, which is the
    // one trade a language with contracts should not make silently.
    case _ if f.ensures.nonEmpty =>
      Some("it has an 'ensures', which is checked when a call returns — a tail call never returns, " +
        "so the postcondition of every frame but the last would go unchecked")

    case _ => None

  /** The tail positions of a block: everything its statements can return from, and its trailing
   * expression.
   *
   * `deferred` arrives true where some enclosing scope has already registered a `defer`, and turns
   * true here at the first one this block registers. A deferred statement runs on the way *out* of
   * the scope, which for an ordinary call is after the callee has returned and for a jump would be
   * before it is even entered — a difference a `defer` closing something the call reads through can
   * see. So a `defer` in scope ends the tail positions beneath it rather than reordering them.
   */
  private def block(f: TFunc, b: TBlock, deferred: Boolean): List[TCall] = {
    val (fromStmts, after) = stmts(f, b.stmts, deferred)

    fromStmts ::: b.result.toList.flatMap(tail(f, _, after))
  }

  /** Walks a statement list in order, collecting what its `return`s put in tail position and
   * reporting whether a `defer` was registered along the way — which the caller needs, because the
   * block's trailing expression sits after all of them.
   */
  private def stmts(f: TFunc, ss: List[TStmt], deferred: Boolean): (List[TCall], Boolean) =
    ss.foldLeft((List.empty[TCall], deferred)) { case ((found, d), s) =>
      s match
        // Registering one puts every tail position after it — in this block and in every block it
        // encloses — out of reach. Its own statements are walked with that already true, so a
        // `return` written inside a `defer` is not a candidate either.
        case TDefer(inner) => (found ::: stmts(f, inner, true)._1, true)

        case TReturn(Some(e)) => (found ::: tail(f, e, d), d)
        case TReturn(None)    => (found, d)

        // A block-bearing expression in statement position yields no value — the arms' results are
        // discarded — so nothing in it is in tail position. What it can still hold is a `return`,
        // and that is what the recursion is for.
        case TExprStmt(e) => (found ::: nested(f, e, d), d)

        case _ => (found, d)
    }

  /** The `return`s inside an expression whose own value is **not** in tail position. Only the forms
   * that carry statements are followed: a `return` is a statement, so it can be nowhere else.
   */
  private def nested(f: TFunc, e: TExpr, deferred: Boolean): List[TCall] = e match
    case TIf(_, t, els, _)  => stmts(f, t.stmts, deferred)._1 ::: els.toList.flatMap(b => stmts(f, b.stmts, deferred)._1)
    case TMatch(_, arms, _) => arms.flatMap(a => stmts(f, a.body.stmts, deferred)._1)
    // A copy of an unrolled `for const` may `return`, and its own value is not in tail position:
    // the copies are a sequence, so only the last of them could be, and `TSeq` is not followed here
    // for the same reason a loop's body is not (`reference/generics.md § A parameter may stand for
    // a list of types`).
    case TBlockExpr(b)      => stmts(f, b.stmts, deferred)._1

    // A loop's body may return, and none of it is in tail position however the loop ends: the value
    // a `break` carries is the loop's, and the loop's value is what the expression around it goes on
    // to use. Recognizing a `break` of a self-call as a tail call is a real case and a later one —
    // it needs the release bounded to the loop rather than to the frame.
    case TWhile(_, body, els, _)             => loop(f, body, els, deferred)
    case TDoWhile(body, _, els, _)           => loop(f, body, els, deferred)
    case TLoop(body, _, _)                   => loop(f, body, None, deferred)
    case TFor(_, _, _, _, _, body, els, _)   => loop(f, body, els, deferred)
    case TCFor(_, _, _, body, els, _)        => loop(f, body, els, deferred)
    case TForEach(_, _, _, body, els, _)     => loop(f, body, els, deferred)
    case TIterate(_, _, _, _, _, body, els, _) => loop(f, body, els, deferred)

    case _ => Nil

  private def loop(f: TFunc, body: List[TStmt], els: Option[TBlock], deferred: Boolean): List[TCall] =
    stmts(f, body, deferred)._1 ::: els.toList.flatMap(b => stmts(f, b.stmts, deferred)._1)

  /** What an expression in tail position contributes: itself, where it is a self-call the jump can
   * replace, and whatever its arms hold where it is a form that has arms.
   */
  private def tail(f: TFunc, e: TExpr, deferred: Boolean): List[TCall] = e match
    case c: TCall if !deferred && isSelf(f, c) => List(c)

    // Both arms end the function, so both are tail positions — and they are independent, so a
    // function may jump back from one and return from the other, which is what the ordinary
    // recursive shape does.
    case TIf(_, t, els, _)  => block(f, t, deferred) ::: els.toList.flatMap(block(f, _, deferred))
    case TMatch(_, arms, _) => arms.flatMap(a => block(f, a.body, deferred))

    case _ => nested(f, e, deferred)

  /** Whether a call in tail position is one to the very function it stands in, at the shape a jump
   * can rebind.
   *
   * The name is the mangled one on both sides, so a generic's instantiation matches its own
   * recursive call and not the instantiation beside it — which is what makes this work for a
   * generic at all: `sum[u32]` jumping to `sum[u64]`'s entry would be a type error made of a
   * branch.
   *
   * The result type is checked rather than assumed. A tail position is a position of the
   * function's *result*, so the two agree wherever the analyzer has done its job — but a call whose
   * result is wrapped (a constrained subtype's check, an `invariant` re-check) reaches here as the
   * wrapper's operand and not as the tail expression, and this is what keeps the walk from being
   * the place that has to know which wrappers those are.
   */
  private def isSelf(f: TFunc, c: TCall): Boolean =
    c.name == f.name && c.ty == f.retTy && c.args.length == f.params.length

  /** The refusals `@tailrec` earns across a whole program: one sentence per function that asked for
   * the jump and did not get it.
   *
   * The attribute is an assertion rather than a request — the optimization applies wherever it
   * applies, written or not — so what it buys is exactly this diagnostic. A function that recurses
   * deeply enough to care is one whose author needs to hear that an edit has just cost them the
   * jump, and hear it at the compile rather than at the stack overflow.
   */
  def check(program: TProgram): Either[List[Diagnostic], Unit] = {
    val refused =
      program.funcs.filter(_.tailrec).flatMap { f =>
        disqualified(f) match
          case Some(why) =>
            Some(Diagnostic(s"'${Modules.show(f.name)}' is marked '@tailrec' but $why", f.pos))
          case None if of(f).isEmpty =>
            Some(Diagnostic(
              s"'${Modules.show(f.name)}' is marked '@tailrec' but calls itself nowhere the jump " +
                "can replace — a tail call is the last thing the function does, so nothing may " +
                "wait on its result and no 'defer' may be in scope where it stands",
              f.pos))
          case None => None
      }

    if refused.nonEmpty then Left(refused) else Right(())
  }
}
