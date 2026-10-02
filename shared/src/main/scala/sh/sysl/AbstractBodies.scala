package sh.sysl

/** The definition-time pass of `reference/generics.md § Bounds`: every generic body checked once,
 * with each type parameter opaque except for what its bounds promise.
 *
 * This is the mechanism `reference/generics.md § Bounds` committed to, and what tells sysl's
 * generics apart from a C++ template. A body that assumes more than it declared is wrong whether or
 * not anything ever instantiates it, so `sum[T](a: T, b: T) = a.plus(b)` fails on its own line
 * rather than at whichever call site first supplied a type without a `plus`.
 *
 * It runs above the bodies it checks because that is what it checks them with: each one is analyzed
 * through `analyzeBodyWith`, against a signature resolved here rather than looked up, since a
 * generic declaration has no entry in `funcInsts` until something instantiates it.
 */
trait AbstractBodies extends FunctionBodies {

  /** Checks every generic body once, at its definition, with each type parameter opaque except for
   * what its bounds promise (`reference/generics.md § Bounds`). This is the mechanism
   * `reference/generics.md § Bounds` committed to, and what tells sysl's generics apart from a C++
   * template: a body that assumes more than it declared is wrong whether or not anything ever
   * instantiates it, and this is where it is told so.
   *
   * Every declaration that carries type parameters is walked, and that now includes a generic
   * *type's* own members: `struct SortedList[T: Ord]` is where its bound is written, so a member may
   * assume `Ord` of `T` and nothing more, whether or not anything ever instantiates the type. A
   * generic `impl`'s members are walked for the same reason and against the block's own bounds —
   * which is what conditional conformance buys beyond deciding whether a `Box[int]` conforms.
   *
   * **A trait's default bodies are walked here too**, each as the generic function it is: one
   * parameter, `Self`, bounded by its own trait (`Hoisting.traitDefaults`). A default may assume of
   * its receiver exactly what the trait promises, which is the same rule this pass already enforces
   * — so it is checked at the trait, once, rather than once per implementing type, and a trait with
   * no implementations at all still has its defaults checked.
   *
   * **And a generic type's own fields are laid out once here**, which is the same rule applied to a
   * declaration that has no body at all: a field applying another bounded type to this one's
   * parameter is wrong at the declaration, and saying so per instantiation would blame whatever type
   * turned up for something the line never mentioned.
   */
  protected def checkAbstractBodies(): Unit = {
    val generics = funcDecls.values.toList.filter(_.tparams.nonEmpty)
    val members  = abstractMembers.toList
    val defaults = traitDefaults

    if generics.nonEmpty || members.nonEmpty || defaults.nonEmpty then
      sandboxed {
        abstractPass = true

        try
          checkAbstractLayouts()

          for f <- generics do
            currentPos = f.pos
            val before = diagnosticCount
            inDecl(f.name)(recover(())(sandboxed(keep(before, checkAbstractBody(f)))))

          // A member reported here has been reported against the body as written, naming the bound
          // that would license what it does. Every instantiation would fail the same way and say so
          // in terms of whatever type it was made at, so those are dropped instead — one mistake,
          // one diagnostic, in the words that name the fix.
          for f <- members do
            currentPos = f.pos
            val before = diagnosticCount
            inDecl(f.name)(recover(())(sandboxed(keep(before, checkAbstractBody(f)))))
            if diagnosticCount > before then brokenMembers += f.name

          // A default that fails here has been reported, at the trait, against the body a
          // programmer actually wrote. The copies made for each implementing type would fail the
          // same way — against the same source line, blaming a type the line does not mention — so
          // they are dropped rather than analyzed, and one mistake stays one diagnostic.
          for f <- defaults do
            currentPos = f.pos
            val before = diagnosticCount
            inDecl(f.name)(recover(())(sandboxed(keep(before, checkAbstractBody(f)))))
            if diagnosticCount > before then brokenDefaults += f.name
        finally abstractPass = false
      }
  }

  /** Lays out every generic type once with its parameters standing in for themselves, so that what
   * its fields and payloads *apply* those parameters to is checked against the bounds it wrote.
   *
   * A non-generic type is laid out eagerly and needs none of this. A generic one has no layout until
   * something instantiates it, so a field holding an `Inner[T]` where `Inner` asks more of its
   * parameter than this type asks of `T` would otherwise be found at the instantiation — reported
   * against a type argument the declaration never named.
   *
   * Each layout is walked in a sandbox of its own, as each body below is. A parameter standing in
   * for itself is memoized under the name it was written with — `Box[T]` — and two declarations
   * whose parameters are both spelled `T` bound it to different things, so an instantiation kept
   * from one walk would answer the next walk's question with the first one's bounds.
   */
  private def checkAbstractLayouts(): Unit = {
    def abstracts(
        tparams: List[String],
        bounds: Map[String, List[BoundRef]],
        values: Map[String, TypeRef],
    ): List[Type] = {
      val subst = abstractSubst(tparams, bounds, values)

      tparams.map(subst)
    }

    // Each layout is built in **the declaring file's** terms, because a bound is a reference like
    // any other and an `import` is what a short name means by. Reading `[T: Scale]` from anywhere
    // else leaves the parameter carrying a bound that never resolved, which the instantiation then
    // compares against the same bound resolved properly — and the type is told its own parameter is
    // not bounded by the trait it is bounded by.
    for (n, d) <- structDecls if d.tparams.nonEmpty do
      currentPos = d.pos
      inDecl(n)(recover(())(sandboxed(instantiateStruct(n, abstracts(d.tparams, d.bounds, d.tvalues)))))

    for (n, d) <- enumDecls if d.tparams.nonEmpty do
      currentPos = d.pos
      inDecl(n)(recover(())(sandboxed(instantiateEnum(n, abstracts(d.tparams, d.bounds, d.tvalues)))))
  }

  /** Keeps what this walk built, which every other part of it throws away.
   *
   * The pass exists to *report*, so `sandboxed` drops everything it registered and the analyzed
   * bodies go with it. One question is asked of the body a generic wrote rather than of any
   * instantiation of it — what storage this module makes, which its caller's type argument does not
   * decide — and it is asked after this pass is over, so the body has to outlive it.
   *
   * A body that was reported on is not kept: it is being compiled no further, and holding the
   * declaring module to a tree that is already wrong would say a second thing about one mistake.
   */
  private def keep(before: Int, f: TFunc): Unit =
    if diagnosticCount == before then abstractFuncs += f

  /** One generic body, analyzed with each of its type parameters substituted by itself. */
  private def checkAbstractBody(f: FuncDecl): TFunc = at(f.pos) {
    val subst: Map[String, Type] = withSelf(f.name, abstractSubst(f.tparams, f.bounds, f.tvalues, f.tpacks))
    val (params, rtype) = inSignature(f.needs) {
      (f.params.map(p => (p.name, recover(Type.Unknown)(resolveType(p.typ, subst)))),
       f.retType.map(t => recover(Type.Unknown)(resolveReturn(t, subst))).getOrElse(Type.Unit))
    }

    analyzeBodyWith(f.name, f, subst, params, rtype)
  }
}
