package sh.sysl

/** What a `match` is when the scrutinee's discriminant alone decides the arm: the enum it is read
 * off, each arm with the tags that select it, and a trailing catch-all where there is one.
 *
 * It is the emitter's reason to lower a `match` to **one** `switch` (`ControlFlowEmitter.genMatch`),
 * and it is the analyzer's condition for `@threaded`, which lays that one dispatch down again at the
 * end of every arm and so needs there to be one. Both ask here so the two cannot drift about which
 * matches qualify.
 */
case class TagDispatch(en: Type.Enum, arms: List[(TArm, List[Int])], catchAll: Option[TArm]) {

  /** Whether the table has a case for every variant of its enum, so that its default edge can only
   * be taken by a value holding no variant at all.
   *
   * **That is what makes the default `unreachable` for a statement match too, and not only for one
   * that yields a value.** A match over an enum is held to exhaustiveness wherever it stands
   * (`PatternAnalysis`), because falling off the end of one has no defined result even for effect —
   * so a statement match with no catch-all names every variant, and its default is exactly as
   * impossible as a value match's, which has always ended in `unreachable`. A branch to the merge
   * instead tells the optimizer the default is a real edge, and it keeps the jump table's range
   * check — a compare and a conditional branch in front of every dispatch — for a case no enum
   * value can reach. The coverage is read off the table here rather than trusted from the analyzer,
   * so a switch that somehow lacks a variant keeps its fall-through.
   */
  def everyVariant: Boolean = {
    val covered = arms.flatMap(_._2).toSet
    en.variants.nonEmpty && en.variants.forall(v => covered(v.tag))
  }
}

object TagDispatch {

  /** Recognises the `match` that lowers to one `switch`, and answers why not for every other shape
   * — which then goes through the emitter's chain of tests exactly as it always did.
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
   *
   * The `Left` is the reason as a phrase, for the one caller that has to say it: `@threaded`, which
   * cannot fall back to anything.
   */
  def of(arms: List[TArm]): Either[String, TagDispatch] = {
    // `n @ V(x)` decides on the same tag `V(x)` does; the outer name is established by
    // `patternBind` off the whole value, which the arm's block does after the branch.
    def variantOf(p: TPattern): Option[TVariantPattern] = p match
      case v: TVariantPattern if !v.args.exists(refutable) => Some(v)
      case a: TAtPattern                                   => variantOf(a.inner)
      case _                                               => None

    if arms.isEmpty then Left("it has no arms")
    else if arms.exists(_.guard.isDefined) then
      Left("an arm carries a guard, which can fail after the dispatch has chosen that arm")
    else
      val catchAll = arms.lastOption.filter(a => a.patterns.lengthIs == 1 && !refutable(a.patterns.head))
      val tagged   = if catchAll.isDefined then arms.init else arms
      val found    = tagged.map(_.patterns.map(variantOf))

      if tagged.isEmpty then Left("no arm tests a variant")
      else if found.exists(_.exists(_.isEmpty)) then
        Left("an arm tests something other than which variant the value holds — a payload that " +
          "has to match, or a value that is not a variant — and a test that can fail after the " +
          "dispatch needs the arm below it to fall through to")
      else
        val vs   = found.map(_.map(_.get))
        val ens  = vs.flatten.map(_.enumTy).distinct
        val tags = vs.flatten.map(_.variant.tag)

        if ens.lengthIs != 1 then Left("its arms test variants of more than one enum")
        else if tags.distinct.lengthIs != tags.length then
          Left("two arms test one variant, and only the order they are written in says which is taken")
        else Right(TagDispatch(ens.head, tagged.zip(vs.map(_.map(_.variant.tag))), catchAll))
  }

  /** Whether a pattern needs a run-time test — false for a wildcard, a binding, or a struct whose
   * fields all need none.
   */
  private def refutable(p: TPattern): Boolean = p match
    case _: TWildPattern | _: TBindPattern => false
    case a: TAtPattern                     => refutable(a.inner)
    case s: TStructPattern                 => s.args.exists(refutable)
    case _                                 => true
}
