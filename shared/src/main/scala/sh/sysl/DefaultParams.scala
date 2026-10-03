package sh.sysl

/** What a parameter's default has to satisfy at the **declaration** (`reference/declarations.md §
 * Default parameters and named arguments`).
 *
 * A default is filled at the call, so without this pass everything wrong with one would be reported
 * at whichever call first left the argument out — a mistake in a signature, reported in somebody
 * else's file, and never reported at all for a default nothing happens to take. So each is analyzed
 * here exactly once, in the terms it was written in, whether or not any call uses it. That is the
 * same reason a `val`'s initializer and a `const`'s value are checked where they are written rather
 * than where they are read.
 *
 * The shape rules — a suffix, no variadic, no `impl` — are checked first and separately, because
 * each of them makes the parameter list unreadable rather than the default wrong, and a list that
 * cannot be read has nothing to say about the expressions in it.
 */
trait DefaultParams extends StmtAnalysis with SignatureVisibility {

  /** Every parameter list that may carry a default, checked once each. */
  protected def checkValueDefaults(): Unit = {
    for (key, f) <- funcDecls.toList do
      inScope(scopeFor(key))(check(qn(key), Some(key), f.params, f.variadic, expectedAt(key, f, 0),
        standing(key, f)))

    for ((tname, mname), m) <- memberDecls.toList do
      // A member whose signature is written in terms nothing has fixed — its type's parameters,
      // its own, or the one a bare arrow added — is kept as it was written, and *that* is the
      // list its defaults are read against. Its lowered form carries the type's parameters as
      // well as the member's, which is why the answer comes from there rather than from `m`.
      val generic = genericMembers.get((tname, mname))

      inDecl(tname)(check(s"${qn(tname)}.$mname", Some(tname), m.params, m.variadic,
        generic match
          case Some(fd) => expectedAt(fd.name, fd, m.recvMode.size)
          case None     => memberTyped(tname, mname, m.recvMode.size),
        generic.fold(noDefaultTypes)(fd => standing(fd.name, fd))))

    // An `impl` block's members are the one kind refused outright, so they are not checked for
    // anything else — the refusal is the whole of what this has to say about them.
    for (scope, impl) <- implDecls.toList; m <- impl.methods; p <- m.params if p.default.isDefined do
      inScope(scope)(recover(())(at(p.default.get.pos)(err(
        s"a member of an 'impl' block declares no default — a call through a trait object holds " +
          s"something that has forgotten which type it is, so there is no implementation to read " +
          s"one off. Declare it on trait '${impl.traitName}', where every implementation shares it"))))
  }

  /** One parameter list. `expectedAt` says what type an argument at a position would be checked
   * against; a default is analyzed against nothing only where the declaration is too broken to say,
   * which still holds it to naming something that exists and to naming nothing local.
   */
  private def check(
      shown: String,
      owner: Option[String],
      params: List[Param],
      variadic: Boolean,
      expectedAt: Int => Option[Type],
      types: DefaultTypes,
  ): Unit = {
    if params.exists(_.default.isDefined) then
      // C reads a variadic call's tail relative to the last named argument (`reference/ffi.md §
      // Variadic functions`), so an argument that might be the last declared parameter or might be
      // the first of the tail leaves nowhere for the tail to begin.
      if variadic then
        for p <- params.find(_.default.isDefined) do
          recover(())(at(p.default.get.pos)(err(
            s"$shown takes a '...', and a parameter list with a tail declares no default — where the " +
              s"tail begins would then depend on how many arguments a call chose to write")))
      else
        val first = params.indexWhere(_.default.isDefined)

        for p <- params.drop(first + 1) if p.default.isEmpty do
          recover(())(at(p.pos)(err(
            s"'${p.name}' has no default and comes after '${params(first).name}', which has one — a " +
              s"call writes its arguments in order, so nothing could leave out '${params(first).name}' " +
              s"and still supply '${p.name}'")))

    // A default already wrapped in its own scope is a **copy** of a trait's, carried onto an `impl`
    // block's method. It is checked once at the trait that wrote it, and checking it again here
    // would report one mistake once per implementing type.
    for (p, i) <- params.zipWithIndex; d <- p.default if !d.isInstanceOf[DefaultArg] do
      recover(())(at(d.pos)(sandboxed {
        resetFunction()
        // Analyzed exactly as the call that takes it will analyze it: in this declaration's terms
        // and with nothing local in scope, so a default naming a parameter is undefined here and
        // says so rather than finding a caller's variable of that name.
        //
        // Its declaration's type parameters are in scope, each standing for itself.
        def read() = withDefaultTypes(types)(inDefault(owner)(inDefaultTypes(analyzeExpr(d, expectedAt(i)))))

        // **A default that NAMES one is read the way a generic body is at its definition**
        // (`reference/generics.md § Bounds`): what the bounds do not license is reported here, and
        // so is a name that names nothing, while the rest — a conversion at `T`, which is checked
        // at each instantiation (`reference/generics.md § Converting through a parameter`) — is
        // left to the call that settles `T`, as it is in a body.
        val t =
          if namesTypeParam(d, (types._1.keySet ++ types._2.keySet).filterNot(_.startsWith("$"))) then
            val saved = abstractPass

            abstractPass = true
            try recover(None)(Some(read()))
            finally abstractPass = saved
          else Some(read())

        for t <- t do
          // **Asked only of a parameter whose type is settled here.** Where a type parameter stands
          // for itself the two sides are not comparable: a closure default at a `$F0` yields the
          // struct that closure became, which is what an instantiation *solves* `$F0` to, so a
          // mismatch here would be the stand-in disagreeing with the thing it stands for. That
          // comparison belongs to the call that fixes it, and it happens there.
          for want <- expectedAt(i) if !Type.mentionsAbstract(want) && disagree(t.ty, want) do
            err(s"the default for '${p.name}' is ${show(t.ty)}, and the parameter is ${show(want)}")

          for key <- owner do exposed(shown, key, p.name, t)
      }))
  }

  /** Whether an expression names one of `tps` — as a value (`T.zero()`, `T(0.0)`) or as a type
   * written inside it. Read off the written tree, which is every node a default can hold.
   */
  private def namesTypeParam(e: Any, tps: Set[String]): Boolean = tps.nonEmpty && (e match
    case Ident(n) if tps(n)        => true
    case NamedType(n, _) if tps(n) => true
    case p: Product                => p.productIterator.exists(namesTypeParam(_, tps))
    case xs: Iterable[?]           => xs.exists(namesTypeParam(_, tps))
    case _                         => false)

  /** A default is the one part of a signature a call does not write, so what it names has to reach
   * as far as the declaration does (`reference/modules.md § Visibility`) — otherwise a caller who
   * leaves the argument out has had a call made on their behalf that they could neither have
   * written nor can see.
   *
   * The names are the ones the default reaches **directly**: what a function it calls does inside
   * itself is that function's business and restricts nothing here. Passing no functions to the walk
   * is what stops it following them.
   *
   * A member is held to its **type's** reach. A member may restrict itself further than its type
   * does, so this is the weaker of the two questions — but it is the one that matches how the rest
   * of `reference/modules.md § Visibility` reads a member, and the stricter half belongs with it
   * rather than here.
   */
  private def exposed(shown: String, key: String, param: String, t: TExpr): Unit = {
    val refs = Reachability.reachedFrom(List(t), Nil, Nil)
    val mine = reachOf(key)

    for named <- (refs.calls ++ refs.vals).toList.sorted if !covers(reachOf(named), mine) do
      err(s"the default for '$param' names '${qn(named)}', which does not reach as far as $shown " +
        s"does — a caller that leaves the argument out would be given something they could not " +
        s"have written")
  }

  /** The parameter types of a declaration whose signature is already resolved, which is every
   * non-generic function and `extern`. A generic one has none to give: its parameters are written
   * in terms nothing has fixed yet.
   */
  private def typed(key: String)(i: Int): Option[Type] =
    funcInsts.get(key).map(_._1).filter(i < _.length).map(_(i)._2)

  /** The same, for a member — whose parameters a call writes, and whose lowered signature has the
   * **receiver** in front of them. `skip` is how many of those a receiver took, so that the `i`th
   * parameter as written is looked up where it actually landed; an associated function takes none
   * and the two indices coincide.
   *
   * A member is lowered under its type mangled rather than under the key it is filed by, so the
   * name comes from `memberFuncs` rather than being spelled here. Where there is no entry — a
   * generic type's member, whose signature is not resolved until a call fixes it — this answers
   * nothing, and the default is held to naming something that exists and nothing more.
   */
  private def memberTyped(tname: String, mname: String, skip: Int)(i: Int): Option[Type] =
    memberFuncs.get((tname, mname)).flatMap(typed(_)(i + skip))

  /** What an argument standing at the `i`th parameter **a call writes** would be checked against,
   * in the declaration's own terms. `skip` is how many parameters the receiver took, since a
   * member's lowered list carries `self` in front of the ones a call writes.
   *
   * Two roads to one answer. A declaration whose signature is already resolved has it in
   * `funcInsts`, which is the very list the call will check the argument against. A **generic** one
   * has no resolved signature — its parameters are written in terms nothing has fixed yet — so the
   * written type is resolved with each type parameter standing for **itself**, which is the
   * substitution `reference/generics.md § Bounds`'s definition-time pass walks a generic body
   * under, and for the same reason: it is what a declaration can be held to before any call exists.
   *
   * **The second road is what lets a closure literal be a default at all.** A parameter written
   * with a bare arrow *is* a bounded type parameter (`reference/types.md § Function types`), so the
   * only thing that says what the closure takes is that bound — and a stand-in carries its bounds,
   * which is exactly what a call reads a held-back callable argument against. Without it the arrow,
   * the one spelling made for taking a closure, was the one spelling whose default could not be
   * one.
   */
  /** A generic declaration's type parameters each standing for itself, with their bounds — what its
   * defaults may name at the declaration, where no call has settled them yet.
   */
  private def standing(lowered: String, f: FuncDecl): DefaultTypes =
    if f.tparams.isEmpty then noDefaultTypes
    else (withSelf(lowered, abstractSubst(f.tparams, f.bounds, f.tvalues, f.tpacks)), f.bounds)

  private def expectedAt(lowered: String, f: FuncDecl, skip: Int)(i: Int): Option[Type] =
    typed(lowered)(i + skip).orElse(
      f.params
        .lift(i + skip)
        .flatMap(p =>
          recoverOpt(resolveType(
            p.typ,
            withSelf(lowered, abstractSubst(f.tparams, f.bounds, f.tvalues, f.tpacks)),
          ))))

}
