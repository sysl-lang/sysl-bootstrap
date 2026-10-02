package sh.sysl

/** Settling every `some` result: the one pass that reads a body in order to learn a **type**.
 *
 * `-> some View` says the concrete result is whatever the body produced, and that the reader may
 * rely on it implementing `View`. Nothing else in sysl infers a declared result — a signature says
 * what it says — so this is the one place a body is analyzed before anything asks about it, and it
 * is deliberately a *pass* rather than something done on demand: a body may name anything the
 * program declares, so it cannot be looked at until every declaration is in, and everything that
 * reads an associated type runs after this.
 *
 * What it produces is one entry per job in `opaqueResults`, keyed by the lowered function the member
 * became. `implAssoc` reads it, and a projection off a concrete subject then answers with a concrete
 * type exactly as a written `type Item = int` would have.
 */
trait OpaqueResults extends AbstractBodies {

  /** Analyzes each `some` member's body, binds the associated type to what it produced, and holds
   * that type both to what the member promised and to what the trait asked.
   *
   * **The analysis is speculative and takes back what it said.** The body is analyzed again, for
   * real, when something instantiates the member — so a mistake inside it would otherwise be
   * reported twice, once here against a signature the reader never wrote. `complaints` is saved and
   * restored for exactly that, and a body that did not analyze leaves the associated type unknown,
   * which is the state every other unresolved type reaches and which nothing downstream complains
   * about a second time.
   *
   * A job whose body reaches **another** `some` result is why `settling` is here. The second job's
   * type is not known yet, so its projection is still the stand-in `implAssoc` hands out, and a
   * chain that leads back to where it started would ask this pass for an answer it is in the middle
   * of computing. That is a genuinely circular program — a member whose result type is read off its
   * own result — and it is reported as one rather than left to recurse.
   */
  protected def settleOpaqueResults(): Unit =
    if opaqueJobs.nonEmpty then
      // Ordered so that a job reaching another settles the one it reaches first where it can. The
      // list is in hoist order, which is source order, and nothing depends on it: a chain in the
      // other direction is settled by the second pass below rather than by luck.
      for job <- opaqueJobs.toList do settle(job)

      // A second sweep, for the jobs whose first attempt read a stand-in because the job it depends
      // on had not been settled yet. One extra sweep is what a chain of two costs; a longer chain
      // resolves to the stand-in, and the bound check below is what notices, since a stand-in
      // implements nothing.
      for job <- opaqueJobs.toList if unsettled.contains(job.func) do settle(job)

      for job <- opaqueJobs.toList do checkOpaque(job)

  /** The jobs whose settled type still mentions a stand-in, and so are worth one more sweep. */
  private def unsettled: Set[String] =
    opaqueJobs.view.map(_.func).filter(f => opaqueResults.get(f).exists(Type.mentionsAbstract)).toSet

  /** Analyzes one body and records what it produced.
   *
   * The signature it is analyzed against is the block's own, with the block's parameters standing
   * in for themselves — the same setup the definition-time pass of `reference/generics.md § Bounds`
   * uses, and for the same reason: a generic block's `some` result is one type per instantiation,
   * and the type it is recorded as has to be written in the block's terms so that a particular
   * subject's arguments substitute into it. `impl[T] Sequence for Buf[T]` whose body builds a
   * `Cursor[T]` records exactly that, and a `Buf[int]` then has `Cursor[int]`.
   *
   * The declared result handed to the walk is `Type.Unknown`, which is the honest answer: it is what
   * this pass is trying to find out. Nothing is checked against it — a result the analyzer could not
   * work out is never the subject of a complaint — so what comes back is the body's own type and
   * nothing has been asserted about it yet.
   */
  private def settle(job: OpaqueJob): Unit = {
    val saved = complaints
    val f     = job.decl

    val ty = inScope(job.scope)(sandboxed {
      abstractPass = true

      try
        currentPos = f.pos

        recover(Type.Unknown)(at(f.pos)(inDecl(f.name) {
          val subst  = withSelf(f.name, abstractSubst(f.tparams, f.bounds, f.tvalues, f.tpacks))
          val params = inSignature(f.needs) {
            f.params.map(p => (p.name, recover(Type.Unknown)(resolveType(p.typ, subst))))
          }

          analyzeBodyWith(f.name, f, subst, params, Type.Unknown).body.ty
        }))
      finally
        abstractPass = false
        restoreComplaints(saved)
    })

    opaqueResults(job.func) = ty

    // **A member of a concrete type had its signature resolved and filed at the hoist**, which was
    // before this pass could exist — so what `funcInsts` holds for it is the stand-in a projection
    // answers with while its `some` result is unsettled, and every later reading of the member's
    // result comes from there rather than from the projection. Correcting the entry is what makes
    // the settled type reach a call, the member's own body check, and the declared type of anything
    // holding what it returned.
    //
    // A **generic** block has no entry to correct: its members are instantiated on demand, and every
    // instantiation happens after this pass and resolves the projection freshly.
    for (params, _) <- funcInsts.get(job.func) do funcInsts(job.func) = (params, ty)
  }

  /** Holds a settled type to the two promises made about it, which are made by two different people
   * and are worth telling apart.
   *
   * `some View` is the **implementation's** promise, written on the member and read by whoever reads
   * the block: it says what a caller may rely on without naming the type. `type Body: View` is the
   * **trait's** requirement, written where the trait was declared. They are usually the same trait
   * and are checked separately anyway, because the fix differs — one is a word on this line, and the
   * other is a type this block chose.
   */
  private def checkOpaque(job: OpaqueJob): Unit =
    for ty <- opaqueResults.get(job.func) if !Type.mentionsUnknown(ty) do
      currentPos = job.pos

      inScope(job.scope)(recover(())(at(job.pos) {
        // A result that never settled to a real type is a chain this pass could not unwind. Said
        // here rather than left to the bound checks, which would blame the bound for it.
        if Type.mentionsAbstract(ty) && job.decl.tparams.isEmpty then
          err(s"the result of '${job.member}' in '${job.label}' is read off its own body, and " +
            "working out what that body produces needs the same answer — a 'some' result cannot " +
            "depend on itself, however many members the loop runs through")

        val own = abstractSubst(job.decl.tparams, job.decl.bounds, job.decl.tvalues, job.decl.tpacks)

        for ref <- job.promised do
          val b = resolveBound(ref, own)

          if !satisfies(b, ty) then
            err(s"'${job.member}' promises 'some ${ref.show}' and its body yields ${show(ty)}, " +
              s"which does not implement '${showBound(b, ty)}'")

        // The trait's bounds on the associated type were resolved with `Self` left abstract, since
        // the type they are about is what this pass works out. Now it is known, it goes in — a
        // `type W: Add` asks whether the settled type is an `Add[<it>, <it>]`, not an `Add[Self, Self]`.
        for
          asked <- job.asked
          b = Type.Bound(asked.name, asked.args.map(substParams(_, selfBinding(ty))))
          if !satisfies(b, ty)
        do
          err(s"the associated type '${job.assoc}' must implement '${showBound(b, ty)}', and the " +
            s"body of '${job.member}' — which is what settles it — yields ${show(ty)}, which does not")
      }))
}
