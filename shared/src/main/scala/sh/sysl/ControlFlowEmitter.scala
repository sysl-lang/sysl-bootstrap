package sh.sysl

import ir.{Access, Arg, BinOp, ICmp, Inst, Val}

/** Everything that makes a basic block: `if`, `match` and its patterns, and the three loops.
 *
 * The shape is the same in all of them. A construct that yields a value allocates a **merge slot**,
 * each path stores into it, and the merge label loads it — there is no `phi` construction, which is
 * what lets a branch be generated without knowing what the other branches did. A path that does not
 * arrive stores nothing, so a diverging arm needs no special handling: its block is already closed
 * and `emit` drops what would follow.
 *
 * A pattern is split in two on purpose. `patternTest` answers whether an arm matches, with no side
 * effects and no bindings, so it can run before the arm is chosen; `patternBind` establishes the
 * bindings afterwards, once the arm is known to be taken. Splitting them is what keeps a failed
 * match from retaining anything it then has to release. The test is free of side effects but not
 * of control flow: a variant's payload may only be read once its tag has been checked, so that
 * check is a branch.
 */
trait ControlFlowEmitter extends PlaceEmitter {

  protected def genIf(cond: List[TCondTerm], thenBlock: TBlock, elseBlock: Option[TBlock], ty: Type): Val = {
    val thenL  = freshLabel("if.then")
    val elseL  = freshLabel("if.else")
    val endL   = freshLabel("if.end")
    val target = if elseBlock.isDefined then elseL else endL
    val slot   = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)

    // The last term's success edge is the branch's entry, so a condition with nothing to bind emits
    // exactly the one test and the one `br` it always did.
    val paths = genCond(cond, thenL, target)

    if Type.noValue(ty) then genBlockVoid(thenBlock) else storeBlockValue(thenBlock, ty, slot)
    // The branch's value is computed before the condition's bindings are given back, so a `then`
    // that yields what it destructured has taken its own count by the time this runs.
    paths.release()
    emitTerm(Inst.Br(endL))
    paths.emitCleanups()

    elseBlock.foreach { eb =>
      emitLabel(elseL)
      if Type.noValue(ty) then genBlockVoid(eb) else storeBlockValue(eb, ty, slot)
      emitTerm(Inst.Br(endL))
    }

    emitLabel(endL)
    endsNowhere(ty)
    if Type.noValue(ty) then Val.Nothing
    // Each branch handed its value over with a count taken, so what the merge loads is the
    // one temporary the enclosing region has to let go of.
    else { val r = freshReg(); emit(Inst.Load(r, ty.lty, slot, Access.Plain)); ownTemp(r, ty) }
  }

  /** What emitting a condition left open: the owned scopes its `is` terms pushed for their
   * bindings, and the small blocks that sit on the edges leaving the chain part-way through.
   *
   * The two are separate because they run at different points. `release` closes the success path,
   * where every binding was made and the branch has finished with them. `emitCleanups` lays down the
   * blocks a failing term branches to, each giving back exactly what had been taken by the time
   * control reached it and falling into the block for the term before it — so a chain that fails at
   * its third term releases what its first two took, and a chain that fails at its first releases
   * nothing.
   */
  protected class CondPaths(scopes: Int, cleanups: List[(String, List[(Val, Type)], Boolean, String)]) {
    def release(): Unit = for _ <- 1 to scopes do popOwned()

    def emitCleanups(): Unit =
      for (label, held, slots, next) <- cleanups do
        emitLabel(label)
        if slots then releaseSlots(held) else releaseValues(held)
        emitTerm(Inst.Br(next))
  }

  /** Emits an `if`'s or a `while`'s condition as the chain of `&&`-joined terms it is
   * (`reference/expressions.md § is — a pattern where a condition is wanted`), landing on
   * `successL` — open, with every binding established — where all of them held.
   *
   * Each term branches to the next on success and to `failL` on failure, the last one branching to
   * `successL`; so a condition with no `is` in it is the one test and the one `br` it always was,
   * and the labels a reader of the IR looks for are still the ones the branch is named after.
   *
   * A term that **bound** something moves the failure target to an unbind block of its own, so
   * everything to its right leaves through the releases it owes. That is the whole reason this is
   * not a `TExpr` the short-circuit `&&` could have generated: a `&&` has no bindings to unwind, and
   * its two edges meet again at one point, where these do not.
   *
   * A pattern's test and its bindings are separated for the reason `genMatch` separates them — the
   * test is a pure read, so a chain may run several before anything is committed to, and a failure
   * retains nothing it then has to hand back.
   *
   * **Every term borrows in a region of its own**, which is the discipline the short-circuit `&&`
   * keeps and which flattening the chain would otherwise have thrown away: a later term's
   * temporaries exist only on the edges that reached it, so releasing them anywhere the whole chain
   * meets would release values the incoming path never made. A term that does not bind closes its
   * region *before* it branches, so both of its edges are already clear; a term that binds cannot,
   * having to retain out of the very value it is holding, so its failing edge gets a block that
   * closes the region for it.
   */
  protected def genCond(terms: List[TCondTerm], successL: String, failL: String): CondPaths = {
    var target   = failL
    var scopes   = 0
    var cleanups = List.empty[(String, List[(Val, Type)], Boolean, String)]

    for (term, i) <- terms.zipWithIndex do
      // The block a term hands control to when it holds: the branch's own entry for the last term,
      // and otherwise a label named after what happens there — a binding is established in it, so a
      // reader of the IR can see where the pattern was committed to.
      val nextL =
        if i == terms.length - 1 then successL
        else
          term match
            case TCondIs(_, List(pat), false) if bindsAny(pat) => freshLabel("cond.bind")
            case _                                             => freshLabel("cond.and")

      pushTemps()
      // **And an ownership region of its own, for the same reason one term further along.** A term
      // may *materialize* storage — `&Named("x")` writes a hidden local — and that slot is written
      // only where the chain reached this term, while a release registered with the enclosing scope
      // is emitted on every path out of it. Closing the region here puts the release on the edges
      // the store dominates, which is exactly the argument the temp region above is already making.
      pushOwned()

      term match
        case TCondTest(c) =>
          val v = genExpr(c)
          popOwned()
          popTemps()
          emitTerm(Inst.CondBr(v, nextL, target))
          emitLabel(nextL)

        case TCondIs(subject, pats, negated) =>
          val sv   = genExpr(subject)
          val held = pats.map(patternTest(_, sv)).reduce(orI1)
          val ok   = if negated then notI1(held) else held

          // Closed before the branch in every case below, so both edges are clear of it — and
          // before the bindings open a region of their own, which the arms below manage separately.
          popOwned()

          // A negated test binds nothing, and neither does a list of alternatives — the analyzer
          // refused a pattern that tried in either position — so both close their region here and
          // leave the failure target where it was.
          if negated || pats.length != 1 || !bindsAny(pats.head) then
            popTemps()
            emitTerm(Inst.CondBr(ok, nextL, target))
            emitLabel(nextL)
          else
            // The bind retains out of the subject, so the subject has to still be held when it runs.
            // That puts the release after the branch on the taken edge, and in a block of its own on
            // the other.
            val borrowed = tempsHere
            val onFail =
              if borrowed.isEmpty then target
              else
                val l = freshLabel("cond.drop")
                cleanups ::= (l, borrowed, false, target)
                l

            emitTerm(Inst.CondBr(ok, nextL, onFail))
            emitLabel(nextL)
            pushOwned()
            patternBind(pats.head, sv)
            scopes += 1
            popTemps()

            // Only a binding that holds a count has anything to give back; the rest of the chain can
            // keep failing straight to where it was already going. Its region is closed by then, so
            // this block owes the bindings alone.
            val slots = ownedHere
            if slots.nonEmpty then
              val unbindL = freshLabel("cond.unbind")
              cleanups ::= (unbindL, slots, true, target)
              target = unbindL

    new CondPaths(scopes, cleanups)
  }

  /** Feeds one branch's value into the merge slot. A branch that does not finish — one that aborts
   * or returns — has no value to feed and terminates its own block, so it is run for its effect
   * and nothing is stored: the merge is reached only from the branches that do arrive.
   */
  protected def storeBlockValue(b: TBlock, ty: Type, slot: Val): Unit =
    if b.ty == Type.Never then genBlockVoid(b)
    else emit(Inst.Store(ty.lty, genBlockValue(b), slot, Access.Plain))

  /** Closes the merge point of an `if`, a `match`, or a loop whose every path diverges: no branch
   * arrives, so the label control would have landed on is unreachable.
   *
   * This is what keeps the invariant every consumer of a value relies on — **a `never`-typed
   * expression always leaves the block terminated** — true for the aggregate forms as well as for
   * the call that starts it. With that, a `return`, a `store`, a `break`, or an argument built from
   * one is dropped rather than emitted with nothing to say.
   */
  protected def endsNowhere(ty: Type): Unit =
    if ty == Type.Never then emitTerm(Inst.Unreachable)

  protected def genMatch(scrutinee: TExpr, arms: List[TArm], ty: Type): Val = {
    val sv   = genExpr(scrutinee)
    val endL = freshLabel("match.end")
    val slot = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)

    tagSwitch(arms) match
      case Some(dispatch) => genTagSwitch(dispatch, sv, ty, slot, endL)
      case None           => genArmChain(arms, sv, ty, slot, endL)

    emitLabel(endL)
    endsNowhere(ty)
    if Type.noValue(ty) then Val.Nothing
    else { val r = freshReg(); emit(Inst.Load(r, ty.lty, slot, Access.Plain)); ownTemp(r, ty) }
  }

  /** What a `match` is when the scrutinee's discriminant alone decides the arm: the enum it is read
   * off, each arm with the tags that select it, and a trailing catch-all where there is one.
   */
  private case class TagDispatch(en: Type.Enum, arms: List[(TArm, List[Int])], catchAll: Option[TArm])

  /** Recognises the `match` that lowers to **one** `switch`, and answers `None` for every other
   * shape — which then goes through `genArmChain` exactly as it always did.
   *
   * The chain tests one arm at a time and falls through to the next, so the arm an input selects is
   * decided by a run of comparisons whose length is the arm's position in the source. Nothing in
   * the language says that, and for a match over a tag it is not even true — the tag names the arm
   * outright. It is the optimizer that has to notice, by folding a chain of `icmp`/`br` back into a
   * `switch`, and a fold has a budget: past a few dozen arms the tail of the chain is left as
   * comparisons, so an arm written late enough pays an extra unpredictable branch on every
   * execution. An interpreter's dispatch loop is where that is felt, because there is no cold arm
   * to put last. Emitting the `switch` outright says what the match means in one instruction,
   * leaves the table to the back end, and costs the same whatever order the arms are written in.
   *
   * **The form is recognised rather than assumed, and the conditions are what make the tag
   * sufficient.** Every arm must test a variant's tag and nothing else, so that no arm can fail
   * *after* the switch has branched to it — a refutable payload pattern or a guard can, and those
   * need the fall-through the chain provides. Two arms claiming one tag would likewise need source
   * order to break the tie. Any of the three falls back to the chain, which handles them all.
   */
  private def tagSwitch(arms: List[TArm]): Option[TagDispatch] = {
    // `n @ V(x)` decides on the same tag `V(x)` does; the outer name is established by
    // `patternBind` off the whole value, which the arm's block does after the branch.
    def variantOf(p: TPattern): Option[TVariantPattern] = p match
      case v: TVariantPattern if !v.args.exists(refutable) => Some(v)
      case a: TAtPattern                                   => variantOf(a.inner)
      case _                                               => None

    if arms.isEmpty || arms.exists(_.guard.isDefined) then None
    else
      val catchAll = arms.lastOption.filter(a => a.patterns.lengthIs == 1 && !refutable(a.patterns.head))
      val tagged   = if catchAll.isDefined then arms.init else arms
      val found    = tagged.map(_.patterns.map(variantOf))

      if tagged.isEmpty || found.exists(_.exists(_.isEmpty)) then None
      else
        val vs   = found.map(_.map(_.get))
        val ens  = vs.flatten.map(_.enumTy).distinct
        val tags = vs.flatten.map(_.variant.tag)

        if ens.lengthIs != 1 || tags.distinct.lengthIs != tags.length then None
        else Some(TagDispatch(ens.head, tagged.zip(vs.map(_.map(_.variant.tag))), catchAll))
  }

  /** One `switch` on the discriminant, and one block per arm. The default edge carries what the
   * chain's fallthrough carried: the catch-all arm where the match has one, and otherwise the same
   * `unreachable` an exhaustive value match ended in, or the merge for a scalar statement match
   * that is allowed to simply proceed.
   */
  private def genTagSwitch(d: TagDispatch, sv: Val, ty: Type, slot: Val, endL: String): Unit = {
    val tagVal =
      if d.en.simple then sv
      else { val t = freshReg(); emit(Inst.Extract(t, d.en.lty, sv, List(0))); t }

    val armLs = d.arms.map(_ => freshLabel("match.arm"))
    val noneL =
      if d.catchAll.isDefined then freshLabel("match.arm")
      else if Type.noValue(ty) then endL
      else freshLabel("match.none")

    val table = d.arms.zip(armLs).flatMap { case ((_, tags), l) => tags.map(t => (BigInt(t), l)) }
    emitTerm(Inst.Switch(d.en.tagLty, tagVal, noneL, table))

    for ((arm, _), l) <- d.arms.zip(armLs) do
      emitLabel(l)
      genArmBody(arm, sv, ty, slot, endL)

    d.catchAll match
      case Some(arm) =>
        emitLabel(noneL)
        genArmBody(arm, sv, ty, slot, endL)
      case None =>
        if !Type.noValue(ty) then
          emitLabel(noneL)
          emitTerm(Inst.Unreachable)
  }

  /** An arm's bindings and body, in the block the branch to it has already opened. Only a single
   * (non-alternative) pattern may bind, which is what makes one block serve several tags.
   */
  private def genArmBody(arm: TArm, sv: Val, ty: Type, slot: Val, endL: String): Unit = {
    pushOwned()
    if arm.patterns.lengthIs == 1 then patternBind(arm.patterns.head, sv)
    if Type.noValue(ty) then genBlockVoid(arm.body) else storeBlockValue(arm.body, ty, slot)
    popOwned()
    emitTerm(Inst.Br(endL))
  }

  /** Every other `match`: each arm tested in turn, falling through to the next one when its pattern
   * or its guard says no.
   */
  private def genArmChain(arms: List[TArm], sv: Val, ty: Type, slot: Val, endL: String): Unit = {
    for arm <- arms do
      val bodyL = freshLabel("match.arm")
      val nextL = freshLabel("match.next")
      val patCond =
        arm.patterns.map(patternTest(_, sv)).reduce(orI1)

      // Bindings are established only after the pattern matches, and a guard may reference them,
      // so a guarded arm branches first on the pattern, then binds, then tests the guard.
      // Only a single (non-alternative) pattern may bind.
      def bind(): Unit = if arm.patterns.length == 1 then patternBind(arm.patterns.head, sv)

      arm.guard match
        case None =>
          emitTerm(Inst.CondBr(patCond, bodyL, nextL))
          emitLabel(bodyL)
          pushOwned()
          bind()
        case Some(g) =>
          val guardL = freshLabel("match.guard")
          emitTerm(Inst.CondBr(patCond, guardL, nextL))
          emitLabel(guardL)
          pushOwned()
          bind()
          pushTemps()
          val gv = genExpr(g)
          popTemps()
          // A guard that fails leaves an arm whose bindings were already made, so they are
          // given back before falling through to the next one.
          val unbindL = freshLabel("match.unbind")
          emitTerm(Inst.CondBr(gv, bodyL, unbindL))
          emitLabel(unbindL)
          releaseOwned()
          emitTerm(Inst.Br(nextL))
          emitLabel(bodyL)

      if Type.noValue(ty) then genBlockVoid(arm.body)
      else storeBlockValue(arm.body, ty, slot)
      popOwned()
      emitTerm(Inst.Br(endL))
      emitLabel(nextL)

    // Fallthrough with no matching arm: a value or enum match is exhaustive (the analyzer
    // required full coverage or a catch-all), so this point is unreachable; a plain scalar
    // statement match simply proceeds.
    if Type.noValue(ty) then emitTerm(Inst.Br(endL)) else emitTerm(Inst.Unreachable)
  }

  /** The i1 result of testing a pattern against a value. Every pattern node carries the type it
   * tests at, so the value's type does not have to be passed alongside it.
   *
   * A variant's payload is tested **behind** its tag rather than beside it. The payload is only
   * this variant's while the tag says so, so reading it out of a value that turned out to be
   * another variant reinterprets whatever that other variant stored — and a test over the result
   * is not merely wrong but unsafe, since comparing a `string` is a call to `sysl.str.cmp` that
   * dereferences the pointer it is handed. Everything else here is a pure value read.
   */
  private def patternTest(p: TPattern, value: Val): Val = p match
    case _: TWildPattern | _: TBindPattern => yes
    // The binding is established later, in `patternBind`; what decides the arm is the inner test
    // alone, since naming a value says nothing about whether it matched.
    case a: TAtPattern                     => patternTest(a.inner, value)
    case TLitPattern(v)                    => compareValue("==", v.ty, value, genExpr(v))
    case TRangePattern(lo, hi, inclusive) =>
      val loOk = compareValue(">=", lo.ty, value, genExpr(lo))
      val hiOk = compareValue(if inclusive then "<=" else "<", hi.ty, value, genExpr(hi))
      andI1(loOk, hiOk)
    case TVariantPattern(en, variant, args) =>
      val tagVal =
        if en.simple then value
        else { val t = freshReg(); emit(Inst.Extract(t, en.lty, value, List(0))); t }
      val tagOk = freshReg(); emit(Inst.IntCmp(tagOk, ICmp.Eq, en.tagLty, tagVal, Val.Int(variant.tag)))

      // Nothing in the payload to ask about — a variant with no fields, or one destructured only
      // into bindings, which `patternBind` establishes later and after the arm has been taken.
      if !args.exists(refutable) then tagOk
      else
        // The tag decides whether the payload may be read at all, so its test is a branch and not
        // an operand of an `and`: the block below runs only for a value of this variant.
        val answer = emitAlloca(freshReg(), i1)
        val testL  = freshLabel("pat.payload")
        val doneL  = freshLabel("pat.done")

        emit(Inst.Store(i1, Val.Bool(false), answer, Access.Plain))
        emitTerm(Inst.CondBr(tagOk, testL, doneL))
        emitLabel(testL)

        val payload = enumPayload(en, variant, value)
        val inner = args.zipWithIndex.foldLeft(yes) { case (acc, (arg, i)) =>
          if !refutable(arg) then acc
          else andI1(acc, patternTest(arg, payloadField(en, variant, payload, i)))
        }

        emit(Inst.Store(i1, inner, answer, Access.Plain))
        emitTerm(Inst.Br(doneL))
        emitLabel(doneL)

        val r = freshReg(); emit(Inst.Load(r, i1, answer, Access.Plain)); r

    // A struct has no tag, so the test is just its refutable fields' tests ANDed together; an
    // irrefutable field needs none, so nothing is emitted for the parts a named pattern omitted.
    // Any bindings such a field carries are established later, in `patternBind`.
    case TStructPattern(struct, args) =>
      args.zipWithIndex.foldLeft(yes) { case (acc, (arg, i)) =>
        if !refutable(arg) then acc
        else andI1(acc, patternTest(arg, structField(struct, value, i)))
      }

  /** Establishes the bindings a pattern introduces, once its arm has been taken. Only binding
   * and (nested) variant patterns carry bindings; the rest are no-ops.
   */
  private def patternBind(p: TPattern, value: Val): Unit = p match
    // A zero-sized binding is not a slot, exactly as a `var` of one is not. The name is still in
    // scope for the arm; every read of it yields nothing.
    case TBindPattern(_, bty) if Type.zeroSized(bty) => ()
    case TBindPattern(name, bty) =>
      emitAlloca(Val.Reg(s"$name.addr"), bty.lty)
      retainValue(bty, value)
      emit(Inst.Store(bty.lty, value, Val.Reg(s"$name.addr"), Access.Plain))
      ownSlot(name, bty)

    // `n @ pat` binds the whole value and then whatever the inner pattern binds, both off the same
    // `value` — so a `&T` matched this way is retained once for the outer name and once for each
    // inner one, which is what every other pattern that names a value more than once already does.
    case TAtPattern(name, inner) =>
      patternBind(TBindPattern(name, inner.ty), value)
      patternBind(inner, value)
    case TVariantPattern(en, variant, args) if args.exists(bindsAny) =>
      val payload = enumPayload(en, variant, value)
      for (arg, i) <- args.zipWithIndex do patternBind(arg, payloadField(en, variant, payload, i))
    case TStructPattern(struct, args) if args.exists(bindsAny) =>
      for (arg, i) <- args.zipWithIndex if bindsAny(arg) do patternBind(arg, structField(struct, value, i))
    case _ => ()

  /** One field of a variant's payload, or nothing where the field is zero-sized — the same skipping
   * the layout does, seen from the reading side.
   */
  private def payloadField(en: Type.Enum, variant: Type.EnumVariant, payload: Val, i: Int): Val =
    if Type.zeroSized(variant.fields(i)._2) then Val.Nothing
    else
      val fv = freshReg()
      emit(Inst.Extract(fv, en.payloadLty(variant), payload, List(variant.slot(i))))
      fv

  /** One field of a struct being destructured — a bit range of the container where the struct is a
   * bitfield struct, and a slot of the aggregate otherwise (`Bitfields`).
   */
  private def structField(struct: Type.Struct, value: Val, i: Int): Val =
    if Type.zeroSized(struct.fields(i)._2) then Val.Nothing
    else
      Bitfields.of(struct) match
        case Some(ranges) =>
          val c = freshReg()
          emit(Inst.Extract(c, struct.lty, value, List(0)))
          readBits(ranges, ranges(struct.slot(i)), c)
        case None =>
          val fv = freshReg()
          emit(Inst.Extract(fv, struct.lty, value, List(struct.slot(i))))
          fv

  private def bindsAny(p: TPattern): Boolean = p match
    case _: TBindPattern    => true
    case _: TAtPattern      => true
    case v: TVariantPattern => v.args.exists(bindsAny)
    case s: TStructPattern  => s.args.exists(bindsAny)
    case _                  => false

  /** Whether a pattern needs a run-time test — false for a wildcard, a binding, or a struct whose
   * fields all need none, so a struct pattern emits reads only for the fields it actually checks.
   */
  private def refutable(p: TPattern): Boolean = p match
    case _: TWildPattern | _: TBindPattern => false
    case a: TAtPattern                     => refutable(a.inner)
    case s: TStructPattern                 => s.args.exists(refutable)
    case _                                 => true

  /** ANDs / ORs two i1 values, folding away the `"true"` immediate a trivially-true pattern
   * produces so the emitted condition stays readable.
   */
  // --- loops ---------------------------------------------------------------------------
  //
  // A loop is an expression: a `break value` stores into the loop's result slot and jumps to the
  // end, and normal completion runs the optional `else` (whose value feeds the same slot). When
  // the loop yields nothing (`unit`), there is no slot and the end is a plain merge. The `else`
  // target doubles as the end when there is no `else`, so a bare loop keeps its old shape.

  protected def genWhile(w: TWhile): Val = {
    val TWhile(cond, body, elseBlock, ty) = w
    val condL = freshLabel("while.cond")
    val bodyL = freshLabel("while.body")
    val endL  = freshLabel("while.end")
    val elseL = if elseBlock.isDefined then freshLabel("while.else") else endL
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)
    // Recorded before the condition's bindings, so a `break` or a `continue` from the body unwinds
    // the round's bindings along with the body's own scope. The condition's *borrowing* is not the
    // loop's to unwind: each term closes its own region (`genCond`), so by the time the body runs
    // there is nothing of the test left outstanding.
    genLoops = GenLoop(endL, condL, slot, ty, owned.length, tempStack.length) :: genLoops

    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    val paths = genCond(cond, bodyL, elseL)
    pushOwned()
    body.foreach(genStmt)
    popOwned()
    // A binding is per-iteration: it is released at the bottom of the body and made again by the
    // next round's test, so nothing accumulates across a loop that runs a million times.
    paths.release()
    emitTerm(Inst.Br(condL))
    paths.emitCleanups()

    genLoops = genLoops.tail
    genLoopResult(slot, ty, elseL, endL, elseBlock)
  }

  /** The post-test loop. It is `genWhile`'s shape entered one label later — the entry branch goes to
   * the body rather than to the test — so the body runs before anything is asked.
   *
   * `continue` targets the **test**, which is what the form exists for: the `loop` with `if !cond
   * then break` at its foot that a program writes instead has no test for a `continue` to reach, so
   * the first one added to it jumps over the exit and never leaves.
   */
  protected def genDoWhile(d: TDoWhile): Val = {
    val TDoWhile(body, cond, elseBlock, ty) = d
    val bodyL = freshLabel("dowhile.body")
    val condL = freshLabel("dowhile.cond")
    val endL  = freshLabel("dowhile.end")
    val elseL = if elseBlock.isDefined then freshLabel("dowhile.else") else endL
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)
    genLoops = GenLoop(endL, condL, slot, ty, owned.length, tempStack.length) :: genLoops

    emitTerm(Inst.Br(bodyL))
    emitLabel(bodyL)
    pushOwned()
    body.foreach(genStmt)
    popOwned()
    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    // Re-evaluated every round, so what the test borrows is let go before the branch rather than
    // piling up in the enclosing statement's region — as the three-clause loop's test does.
    pushTemps()
    val v = genExpr(cond)
    popTemps()
    emitTerm(Inst.CondBr(v, bodyL, elseL))

    genLoops = genLoops.tail
    genLoopResult(slot, ty, elseL, endL, elseBlock)
  }

  /** A `loop` is the same shape with the test gone: the body branches straight back to itself, and
   * the end is reached only by a `break`. `continue` targets the body's own label, since starting
   * the next iteration is all there is to do. Where nothing breaks, the loop's type is `never` and
   * the end label closes as unreachable.
   */
  protected def genLoop(l: TLoop): Val = {
    val TLoop(body, ty) = l
    val bodyL = freshLabel("loop.body")
    val endL  = freshLabel("loop.end")
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)
    genLoops = GenLoop(endL, bodyL, slot, ty, owned.length, tempStack.length) :: genLoops

    emitTerm(Inst.Br(bodyL))
    emitLabel(bodyL)
    pushOwned()
    body.foreach(genStmt)
    popOwned()
    emitTerm(Inst.Br(bodyL))

    genLoops = genLoops.tail
    genLoopResult(slot, ty, endL, endL, None)
  }

  protected def genFor(f: TFor): Val = {
    val TFor(name, varTy, lo, hi, inclusive, body, elseBlock, ty) = f
    val w     = varTy.lty
    val loV   = genExpr(lo)
    val hiV   = genExpr(hi)
    val condL = freshLabel("for.cond")
    val bodyL = freshLabel("for.body")
    val stepL = freshLabel("for.step")
    val endL  = freshLabel("for.end")
    val elseL = if elseBlock.isDefined then freshLabel("for.else") else endL
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)
    emitAlloca(Val.Reg(s"$name.addr"), w)
    emit(Inst.Store(w, loV, Val.Reg(s"$name.addr"), Access.Plain))
    genLoops = GenLoop(endL, stepL, slot, ty, owned.length, tempStack.length) :: genLoops

    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    val iv  = freshReg(); emit(Inst.Load(iv, w, Val.Reg(s"$name.addr"), Access.Plain))
    val cmp = freshReg()

    emit(Inst.IntCmp(cmp, intPred(if inclusive then "<=" else "<", varTy), w, iv,
      hiV))
    emitTerm(Inst.CondBr(cmp, bodyL, elseL))
    emitLabel(bodyL)
    pushOwned()
    body.foreach(genStmt)
    popOwned()
    // `continue` lands here so the counter still advances before the next test.
    emitTerm(Inst.Br(stepL))
    emitLabel(stepL)
    val cur = freshReg(); emit(Inst.Load(cur, w, Val.Reg(s"$name.addr"), Access.Plain))
    // **An inclusive loop stops at its bound rather than stepping past it**, and that is not a
    // nicety: at `250u8..255u8` the increment below wraps to zero and `0 <= 255` starts the walk
    // again, so the loop never ends. There is no value one greater than the last one to test
    // against, which is why the test has to happen *here*, before the increment that has nowhere
    // to go. It costs one compare and one branch, on inclusive loops only — `0..<n` is untouched
    // and is what nearly every loop in the language is.
    if inclusive then
      val done = freshReg()
      val more = freshLabel("for.more")
      emit(Inst.IntCmp(done, intPred("==", varTy), w, cur, hiV))
      emitTerm(Inst.CondBr(done, elseL, more))
      emitLabel(more)
    val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, w, cur, Val.Int(1)))
    emit(Inst.Store(w, nxt, Val.Reg(s"$name.addr"), Access.Plain))
    emitTerm(Inst.Br(condL))

    genLoops = genLoops.tail
    genLoopResult(slot, ty, elseL, endL, elseBlock)
  }

  /** `for all i in lo..hi do pred` and `for some …` — a counted loop over an accumulator slot,
   * yielding the `i1` the slot holds when it stops (`reference/verification.md § for all and for
   * some`).
   *
   * **The slot starts at the quantifier's identity and is written exactly once, by the iteration
   * that decides the answer.** A conjunction over nothing is true and a disjunction over nothing is
   * false, so an empty range falls out of the initial store with no case for it: the loop simply
   * never runs. The store on the deciding iteration is the opposite value, and the branch after it
   * leaves — which is what makes both forms short-circuit, `for all` at the first counterexample and
   * `for some` at the first witness.
   *
   * The predicate opens its own temporary region because it is re-evaluated every iteration: a
   * condition that allocates on its way to a `bool` would otherwise accumulate one allocation per
   * element in the enclosing statement's region, and the region is not the loop's to keep.
   *
   * This is not `genFor` with a different body. A quantifier carries no `break`, no `else` and no
   * label, so it registers no `GenLoop` — a `break` written inside a predicate belongs to whatever
   * loop encloses the quantifier, which is what the reader of that line means.
   */
  protected def genQuantifier(q: TQuantifier): Val = {
    val TQuantifier(universal, name, varTy, lo, hi, inclusive, pred) = q
    val w     = varTy.lty
    val loV   = genExpr(lo)
    val hiV   = genExpr(hi)
    val condL = freshLabel("quant.cond")
    val bodyL = freshLabel("quant.body")
    val stepL = freshLabel("quant.step")
    val doneL = freshLabel("quant.done")
    val endL  = freshLabel("quant.end")
    val acc   = emitAlloca(freshReg(), i1)

    emit(Inst.Store(i1, Val.Int(if universal then 1 else 0), acc, Access.Plain))
    emitAlloca(Val.Reg(s"$name.addr"), w)
    emit(Inst.Store(w, loV, Val.Reg(s"$name.addr"), Access.Plain))

    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    val iv  = freshReg(); emit(Inst.Load(iv, w, Val.Reg(s"$name.addr"), Access.Plain))
    val cmp = freshReg()

    emit(Inst.IntCmp(cmp, intPred(if inclusive then "<=" else "<", varTy), w, iv,
      hiV))
    emitTerm(Inst.CondBr(cmp, bodyL, endL))

    emitLabel(bodyL)
    pushTemps()
    val p = genExpr(pred)
    popTemps()
    // A true predicate continues a `for all` and settles a `for some`; a false one does the reverse.
    if universal then emitTerm(Inst.CondBr(p, stepL, doneL))
    else emitTerm(Inst.CondBr(p, doneL, stepL))

    emitLabel(doneL)
    emit(Inst.Store(i1, Val.Int(if universal then 0 else 1), acc, Access.Plain))
    emitTerm(Inst.Br(endL))

    emitLabel(stepL)
    val cur = freshReg(); emit(Inst.Load(cur, w, Val.Reg(s"$name.addr"), Access.Plain))
    // The same wrap `genFor` guards against, at the same place and for the same reason: without
    // this, `for all i in 0..255u8 do p` never finishes, and a quantifier that does not finish is
    // worse than a loop that does not — it is a *question* the program hangs on.
    if inclusive then
      val done = freshReg()
      val more = freshLabel("quant.more")
      emit(Inst.IntCmp(done, intPred("==", varTy), w, cur, hiV))
      emitTerm(Inst.CondBr(done, endL, more))
      emitLabel(more)
    val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, w, cur, Val.Int(1)))
    emit(Inst.Store(w, nxt, Val.Reg(s"$name.addr"), Access.Plain))
    emitTerm(Inst.Br(condL))

    emitLabel(endL)
    val res = freshReg(); emit(Inst.Load(res, i1, acc, Access.Plain)); res
  }

  /** The three-clause loop. It is `genFor`'s shape with both fixed parts opened up: the test is
   * whatever was written (absent ⇒ always taken, so the `else` target is unreachable and the loop
   * ends only through a `break`), and the step is whatever was written rather than an increment.
   *
   * `continue` targets the **step** block, which is the whole point of the form: a counted loop
   * written as a `while` skips its increment on the first `continue` somebody adds.
   */
  protected def genCFor(f: TCFor): Val = {
    val TCFor(init, cond, step, body, elseBlock, ty) = f
    val condL = freshLabel("cfor.cond")
    val bodyL = freshLabel("cfor.body")
    val stepL = freshLabel("cfor.step")
    val endL  = freshLabel("cfor.end")
    val elseL = if elseBlock.isDefined then freshLabel("cfor.else") else endL
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)

    init.foreach(genStmt)
    genLoops = GenLoop(endL, stepL, slot, ty, owned.length, tempStack.length) :: genLoops

    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    cond match
      case Some(c) =>
        // Re-evaluated every iteration, so what it borrows is let go before the branch rather than
        // accumulating in the enclosing statement's region.
        pushTemps()
        val v = genExpr(c)
        popTemps()
        emitTerm(Inst.CondBr(v, bodyL, elseL))
      case None => emitTerm(Inst.Br(bodyL))

    emitLabel(bodyL)
    pushOwned()
    body.foreach(genStmt)
    popOwned()
    emitTerm(Inst.Br(stepL))
    emitLabel(stepL)
    pushTemps()
    step.foreach(genStmt)
    popTemps()
    emitTerm(Inst.Br(condL))

    genLoops = genLoops.tail
    genLoopResult(slot, ty, if cond.isDefined then elseL else endL, endL, elseBlock)
  }

  // The sequence is evaluated once, into the statement's own region, so a slice temporary stays
  // alive for the whole loop; the loop variable is a copy, released each iteration.
  protected def genForEach(e: TForEach): Val = {
    val TForEach(name, elemTy, seq, body, elseBlock, ty) = e
    val (base, len) = seq.ty match
      case Type.Array(n, _) => (address(seq), Val.Int(n))
      case s: Type.Slice =>
        val v = genExpr(seq)
        val p = freshReg(); emit(Inst.Extract(p, s.lty, v, List(1)))
        val l = freshReg(); emit(Inst.Extract(l, s.lty, v, List(2)))
        (p, l)
      case other => sys.error(s"unreachable iteration over ${other.llvm}")

    val idx   = emitAlloca(freshReg(), wordLty)
    val condL = freshLabel("each.cond")
    val bodyL = freshLabel("each.body")
    val stepL = freshLabel("each.step")
    val endL  = freshLabel("each.end")
    val elseL = if elseBlock.isDefined then freshLabel("each.else") else endL
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)
    emit(Inst.Store(wordLty, Val.Int(0), idx, Access.Plain))
    genLoops = GenLoop(endL, stepL, slot, ty, owned.length, tempStack.length) :: genLoops

    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    val iv   = freshReg(); emit(Inst.Load(iv, wordLty, idx, Access.Plain))
    val more = freshReg(); emit(Inst.IntCmp(more, ICmp.Ult, wordLty, iv, len))
    emitTerm(Inst.CondBr(more, bodyL, elseL))
    emitLabel(bodyL)
    val ep = freshReg(); emit(Inst.Gep(ep, elemTy.lty, base, List(Arg(wordLty, iv))))
    val ev = freshReg(); emit(Inst.Load(ev, elemTy.lty, ep, Access.Plain))
    emitAlloca(Val.Reg(s"$name.addr"), elemTy.lty)
    retainValue(elemTy, ev)
    emit(Inst.Store(elemTy.lty, ev, Val.Reg(s"$name.addr"), Access.Plain))
    pushOwned()
    ownSlot(name, elemTy)
    body.foreach(genStmt)
    popOwned()
    emitTerm(Inst.Br(stepL))
    emitLabel(stepL)
    val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, wordLty, iv, Val.Int(1)))
    emit(Inst.Store(wordLty, nxt, idx, Access.Plain))
    emitTerm(Inst.Br(condL))

    genLoops = genLoops.tail
    genLoopResult(slot, ty, elseL, endL, elseBlock)
  }

  /** The iterating loop. The cursor gets a slot with a count of its own, taken before the loop is
   * recorded so that a `break` — which lets go of everything pushed *after* the record — leaves it
   * alone; the end label is where every path meets, and the release goes there.
   *
   * What `next` gives back is a temporary of its own each round, and the two edges out of the test
   * let go of it separately: the body binds the element (retaining it into its own slot) and then
   * releases the option, while the exhausted path releases it with nothing taken out.
   */
  protected def genIterate(e: TIterate): Val = {
    val TIterate(cursor, cursorTy, init, next, bind, body, elseBlock, ty) = e
    val iv = genExpr(init)
    pushOwned()
    emitAlloca(Val.Reg(s"$cursor.addr"), cursorTy.lty)
    retainValue(cursorTy, iv)
    emit(Inst.Store(cursorTy.lty, iv, Val.Reg(s"$cursor.addr"), Access.Plain))
    ownSlot(cursor, cursorTy)

    val condL = freshLabel("iter.cond")
    val bodyL = freshLabel("iter.body")
    val doneL = freshLabel("iter.done")
    val endL  = freshLabel("iter.end")
    val elseL = if elseBlock.isDefined then freshLabel("iter.else") else endL
    val slot  = if Type.noValue(ty) then Val.Nothing else emitAlloca(freshReg(), ty.lty)

    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    pushTemps()
    // `continue` goes back to the test, which is where the next element comes from: an iterating
    // loop has no step of its own, since advancing is what `next` did.
    //
    // Recorded **after** the frame the option lives in, and deliberately: the body was reached
    // through the `releaseTemps` below, so a `break` that unwound that frame as well would give the
    // option back twice.
    genLoops = GenLoop(endL, condL, slot, ty, owned.length, tempStack.length) :: genLoops
    val opt = genExpr(next)
    val ok  = patternTest(bind, opt)
    emitTerm(Inst.CondBr(ok, bodyL, doneL))

    emitLabel(bodyL)
    pushOwned()
    patternBind(bind, opt)

    // **The frame is emptied here, not merely released, and that is what a `return` from the body
    // needs.** `releaseTemps` gives the counts back on this edge and leaves the region populated so
    // that the exhausted edge below can give them back too — but `releaseAll`, which a `return`
    // emits, walks *every* temp region and so would give this one back a second time. A `break` is
    // already safe: `releaseToDepth` stops at the depth the `GenLoop` recorded, which is outside
    // this frame. Clearing rather than popping keeps the stack the same height, so the depths that
    // `break` and `continue` were recorded against still mean what they meant.
    val held = tempsHere

    releaseValues(held)
    tempStack.head.clear()
    body.foreach(genStmt)
    popOwned()
    emitTerm(Inst.Br(condL))

    emitLabel(doneL)
    tempStack.head ++= held
    releaseTemps()
    dropTemps()
    emitTerm(Inst.Br(elseL))

    genLoops = genLoops.tail
    val result = genLoopResult(slot, ty, elseL, endL, elseBlock)
    popOwned()
    result
  }

  /** Finishes a loop expression: run the `else` (if any) on the normal-completion path into the
   * result slot, then land at the end and hand the slot's value out as the enclosing region's to
   * release. A `unit` loop has no slot and yields nothing.
   */
  private def genLoopResult(slot: Val, ty: Type, elseL: String, endL: String,
                            elseBlock: Option[TBlock]): Val = {
    elseBlock.foreach { eb =>
      emitLabel(elseL)
      if Type.noValue(ty) then genBlockVoid(eb)
      else storeBlockValue(eb, ty, slot)
      emitTerm(Inst.Br(endL))
    }
    emitLabel(endL)
    endsNowhere(ty)
    if Type.noValue(ty) then Val.Nothing
    else { val r = freshReg(); emit(Inst.Load(r, ty.lty, slot, Access.Plain)); ownTemp(r, ty) }
  }

  protected def genBlockVoid(b: TBlock): Unit = {
    pushTemps()
    pushOwned()
    b.stmts.foreach(genStmt)
    b.result.foreach(genExpr)
    popOwned()
    popTemps()
  }

  /** A branch's value, handed out with a count of its own. The block's locals and temporaries
   * are released before control leaves it — that is what keeps every release site dominating
   * the value it releases — so the result is retained first and becomes the caller's to let go.
   */
  protected def genBlockValue(b: TBlock): Val = {
    pushTemps()
    pushOwned()
    b.stmts.foreach(genStmt)
    val v = genExpr(b.result.get)
    retainValue(b.result.get.ty, v)
    popOwned()
    popTemps()
    v
  }

}
