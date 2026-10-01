package sh.sysl

/** The rule that a declaration may not name, in what it declares to whoever can reach it, a type
 * that does not reach as far (`reference/modules.md § Visibility`).
 *
 * A `private struct Point` beside a public `make() -> Point` would hand every module a value of a
 * type none of them may name: they could hold it, pass it on, and read its fields, and the one
 * thing they could not do is write the type down. That is a hole in the restriction rather than a
 * use of it, so the declaration is refused rather than left to leak — and refusing it now forbids
 * nothing a later rule would have had to keep allowing.
 *
 * The rule is about **naming**, so it runs over what was written rather than over resolved types. A
 * type parameter, `Self`, and the built-in scalars are names that stand for no declaration; a name
 * the declaring file may not reach at all is a complaint the resolution that built the signature has
 * already made, and is passed over here rather than reported twice.
 *
 * An `impl` block is deliberately outside it. Its members' signatures are the trait's — a mismatch
 * is refused as non-conformance — so a type leaking through one leaks through the trait, which is
 * where it is reported. Implementing a *public* trait for a *private* type is not a leak either: the
 * promise is public, and the type it was made for stays as unnameable as it was.
 */
trait SignatureVisibility extends TypeResolution {

  /** How far a declaration may be named from, which is the one thing this rule compares.
   *
   * A file-private declaration carries the module its file contributes to as well as the file, since
   * a module-scoped type covers it exactly when that module encloses this one.
   */
  protected enum Reach:
    case Everywhere
    case Subtree(module: String)
    case OneFile(file: Source, module: String)

  /** How far one declaration reaches, read off what its modifier recorded. A declaration with no
   * entry was unmarked, and a `private` in a context with no file behind it — the library's, or one
   * the compiler synthesized — restricts nothing there is anywhere to restrict it to.
   */
  protected def reachOf(key: String): Reach = declAccess.get(key) match
    case Some(Access(_, Some(m)))    => Reach.Subtree(m)
    case Some(Access(Some(f), None)) => Reach.OneFile(f, scopeFor(key).module)
    case _                           => Reach.Everywhere

  /** Whether everything that may name a declaration of reach `inner` may also name one of `outer` —
   * which is the whole of the question a leak asks.
   *
   * A module-scoped declaration covers another exactly when its subtree contains the other's, and
   * `private[M]` may only name an enclosing module (`reference/modules.md § Visibility`), so every
   * reach is a contiguous region and containment settles it. A file covers only itself, compared by
   * identity rather than by name because two files of one project may be called the same thing.
   */
  protected def covers(outer: Reach, inner: Reach): Boolean = (outer, inner) match
    case (Reach.Everywhere, _)                      => true
    case (_, Reach.Everywhere)                      => false
    case (Reach.OneFile(a, _), Reach.OneFile(b, _)) => a eq b
    case (Reach.OneFile(_, _), _)                   => false
    case (Reach.Subtree(m), Reach.Subtree(n))       => within(m, n)
    case (Reach.Subtree(m), Reach.OneFile(_, n))    => within(m, n)

  private def within(outer: String, inner: String): Boolean =
    inner == outer || inner.startsWith(s"$outer.")

  /** Resolves every type a **trait's** members name, which nothing else asks for.
   *
   * A struct's or an enum's members are lowered to functions, and a free function's signature is
   * resolved when it is hoisted — so a name that stands for nothing is reported at the declaration
   * in all three. A trait's members are lowered nowhere: they are a promise, and the only other
   * thing that reads them is the conformance check an `impl` runs. Without this pass a trait
   * nothing implements could promise a type that does not exist, and nobody would be told.
   *
   * `Self` and the trait's own parameters stand in for themselves, which is exactly what the
   * signature means — it holds for every implementing type, so what can be checked here is the
   * names, not what any one of them turns out to be. The conformance check still resolves both
   * sides against a concrete `Self`, and this takes nothing away from it.
   */
  protected def checkTraitSignatures(): Unit =
    for (key, t) <- traitDecls.toList do
      // **In a sandbox of its own, and it is not tidiness** — it is the rule `checkAbstractLayouts`
      // already states for the same reason. A `Type.Abstract` is identified by its *name*, so this
      // trait's `T` and any other declaration's `T` are one type as far as a cache key is concerned;
      // resolving `Option[T]` here registers an `Option` instantiated at a stand-in carrying **this
      // trait's** bounds, and every later walk asking for `Option[T]` is handed that one.
      //
      // What that looked like: `trait Sink[T]` promising an `Option[T]` left the library's
      // `impl[T: Eq] Eq for Option[T]` being walked at an *unbounded* `T`, and the block was told
      // its own body assumed what its own bounds promise. The trait was unrelated to the impl in
      // every way but the letter its parameter is spelled with.
      //
      // Nothing is lost by dropping it: this pass resolves names in order to report the ones that
      // stand for nothing, and every instantiation it makes is one a real use makes again.
      sandboxed(inDecl(key) {
        // The parameters are read here rather than outside, because a bound on one of them names a
        // trait in the terms of the file that wrote it: `[T: Scale]` under an `import` means the
        // imported trait, and read from anywhere else means nothing at all.
        // `Self` carries **this trait** as its bound, which is what lets a member's result be
        // `Self::Item`: a projection is read off a bound, and the one thing known about `Self` in a
        // trait's own declaration is that it implements the trait being declared. Everywhere else
        // `Self` stands in for itself with nothing promised, which is all those places need.
        val own   = abstractSubst(t.tparams, t.bounds, t.tvalues)
        val subst = own + (selfName -> Type.Abstract(selfName, List(Type.Bound(key, t.tparams.map(own)))))

        for m <- t.methods do
          recover(())(at(m.pos) {
            // A member's **own** parameters stand beside the trait's, exactly as they do on an
            // inherent member: the trait declares what the type is generic over and the member
            // declares what a call to it is, and its signature may name either. Resolving under the
            // trait's alone reported the member's as unknown types, which read as a mistake in the
            // trait rather than as a scope this walk had not been given.
            val here = subst ++ abstractSubst(m.tparams, m.bounds, m.tvalues, m.tpacks, subst)

            for p <- m.params do resolveType(p.typ, here)
            m.retType.foreach(resolveReturn(_, here))
            // And the rules about where a `va_list` may stand, which an implementation would meet
            // when its member is lowered — a promise nothing keeps has no lowering to meet them at.
            // The receiver stands in the list because it is a parameter once the member is lowered,
            // and it is what a `...` on a method anchors its tail on.
            val recv = m.recvMode.map(_ => Param("self", NamedType(selfName, Nil))).toList

            checkSignatureRules(s"${qn(key)}.${m.name}", recv ::: m.params, m.retType, m.variadic)
          })
      })

  /** Reports every type a declaration exposes that does not reach as far as the declaration does.
   *
   * It runs once every declaration is registered, because the question is about two of them at a
   * time and either may be written further down the file than the other.
   */
  protected def checkExposedTypes(): Unit = {
    for (key, d) <- structDecls.toList do
      val own = d.tparams.toSet

      expose(key, own, d.bounds, defaults(d.tdefaults))
      for f <- d.fields do exposeField(key, own, f)
      for m <- d.members do exposeMember(key, own, m)

    for (key, d) <- enumDecls.toList do
      val own     = d.tparams.toSet
      val payload =
        for v <- d.variants; f <- v.fields
        yield (s"the '${f.name}' of variant '${v.name}'", f.typ, f.pos)

      expose(key, own, d.bounds, payload ::: defaults(d.tdefaults))
      for m <- d.members do exposeMember(key, own, m)

    for (key, d) <- traitDecls.toList do
      val own = d.tparams.toSet

      expose(key, own, d.bounds, defaults(d.tdefaults), d.supers)
      for m <- d.methods do exposeMember(key, own, m)

    // An `extern` is here too: it is registered as a function, and a symbol the linker resolves is
    // no less able to hand back a type the caller cannot name.
    for (key, d) <- funcDecls.toList do
      expose(key, d.tparams.toSet, d.bounds, signature(d.params, d.retType))

    // The three declarations that are a **name and one type**: a `const`, a module-level `val`, and
    // an `extern` variable. Each is the same hole a public function returning a private type is,
    // reached by a shorter route — a module that may write the name holds a value whose type it
    // cannot write, which is what §2 calls a hole in the restriction rather than a use of it.
    //
    // A `val`'s type is an `Option` only because a *local* infers one; a module-level `val` has been
    // held to stating it, so an absent one is a declaration already reported and there is nothing
    // here to compare.
    //
    // The `const` line has nothing to catch **today** and is here because the rule is one rule: a
    // constant is held to being a scalar (`reference/modules.md § const — a value`), and every scalar is a builtin with no declaration
    // to restrict — so a constant naming a type anyone could make private is refused a step earlier.
    // Stating it here anyway is what keeps widening what a constant may hold from silently reopening
    // the hole that `val` and `extern` had, which is the mistake this whole block is fixing.
    for (key, d) <- constDecls.toList do
      expose(key, Set.empty, Map.empty, List(("its type", d.typ, d.typ.pos)))

    for (key, d) <- valDecls.toList; t <- d.typ do
      expose(key, Set.empty, Map.empty, List(("its type", t, t.pos)))

    for (key, d) <- externVarDecls.toList do
      expose(key, Set.empty, Map.empty, List(("its type", d.typ, d.typ.pos)))
  }

  /** A **default** is exposed as surely as a field is, and less obviously: it is the one part of a
    * signature a use does not write, so a caller that leaves the argument out ends up holding
    * whatever the default named — a type they could not have written and cannot name. A default
    * naming one of the declaration's own parameters names nothing anyone has to reach, which is
    * what `skip` already covers.
    */
  private def defaults(tdefaults: Map[String, TypeRef]): List[(String, TypeRef, Option[Pos])] =
    tdefaults.toList.sortBy(_._1).map((tp, ref) => (s"the default for '$tp'", ref, ref.pos))

  /** One top-level declaration, which is as visible as its own modifier made it. */
  private def expose(
      key: String,
      tparams: Set[String],
      bounds: Map[String, List[BoundRef]],
      parts: List[(String, TypeRef, Option[Pos])],
      supers: List[BoundRef] = Nil,
  ): Unit = exposeIn(key, Modules.split(key)._2, reachOf(key), tparams, bounds, parts, supers)

  /** One member of a type or of a trait, asked at **its own** reach (`reference/modules.md §
   * Visibility`).
   *
   * That is the type's where the member said nothing, since an unmarked member inherits it, and
   * narrower where it restricted itself — so a `private` helper method may name a `private` type
   * that the public method beside it may not.
   */
  private def exposeMember(owner: String, tparams: Set[String], m: MethodDecl): Unit =
    exposeIn(
      owner,
      s"${Modules.split(owner)._2}.${m.name}",
      reachOf(memberAccessKey(owner, m.name)),
      tparams ++ m.tparams + selfName,
      m.bounds,
      signature(m.params, m.retType),
    )

  /** One field, which carries a modifier for the same reason a member does and is asked the same
   * question: a caller who can read the field can hold whatever type it named.
   */
  private def exposeField(owner: String, tparams: Set[String], f: Param): Unit =
    exposeIn(
      owner,
      s"${Modules.split(owner)._2}.${f.name}",
      reachOf(memberAccessKey(owner, f.name)),
      tparams,
      Map.empty,
      List(("its type", f.typ, f.pos)),
    )

  private def signature(params: List[Param], ret: Option[TypeRef]): List[(String, TypeRef, Option[Pos])] =
    params.map(p => (s"parameter '${p.name}'", p.typ, p.pos)) ::: ret.toList.map(r => ("its result", r, r.pos))

  /** The comparison itself, made in the terms the declaration was written in — the names in its
   * signature mean what they meant there, exactly as its body's do.
   */
  private def exposeIn(
      scope: String,
      label: String,
      reach: Reach,
      skip: Set[String],
      bounds: Map[String, List[BoundRef]],
      parts: List[(String, TypeRef, Option[Pos])],
      supers: List[BoundRef] = Nil,
  ): Unit = reach match
    // A file-private declaration reaches one file, and every type it names is visible there or the
    // signature would not have resolved — so there is nothing left for this to find.
    case _: Reach.OneFile => ()
    case _ =>
      inDecl(scope) {
        for (what, ref, pos) <- parts; (written, at1) <- namesIn(ref, skip); key <- namedDecl(written) do
          at(at1.orElse(pos))(leak(label, reach, what, key))

        // A bound is part of what a caller must satisfy, so a trait it cannot name leaves it unable
        // to say what the declaration asks of it.
        for (tp, refs) <- bounds; b <- refs if !skip(b.name); key <- quietly(traitKey(b.name)) do
          at(b.pos)(leak(label, reach, s"the bound on '$tp'", key))

        // A **required** trait is the sharpest case of the same rule: implementing the trait means
        // implementing that one as well, so a requirement the implementer cannot name leaves the
        // trait unimplementable from outside — and the diagnostic at the `impl` would tell them to
        // write a block naming a trait they may not.
        for b <- supers; key <- quietly(traitKey(b.name)) do
          at(b.pos)(leak(label, reach, "the trait it requires", key))
      }

  private def leak(label: String, reach: Reach, what: String, key: String): Unit =
    if !covers(reachOf(key), reach) then
      recover(())(err(s"'$label' is ${reachPhrase(reach)}, but $what names '${qn(key)}', which is " +
        s"${restriction(key)} — a declaration may not be more visible than the types it names"))

  /** How far the declaration itself reaches, as a diagnostic says it. `restriction` says the other
   * half — how far the type it named does — and the two together are the whole complaint.
   */
  private def reachPhrase(reach: Reach): String = reach match
    case Reach.Everywhere    => "public"
    case Reach.Subtree(m)    => s"visible throughout module '$m'"
    case Reach.OneFile(f, _) => s"private to '${f.name}'"

  /** Every name a written type mentions, with where it was written, less the names that stand for
   * something other than a declaration here — the enclosing declaration's type parameters, and
   * `Self`.
   */
  private def namesIn(t: TypeRef, skip: Set[String]): List[(String, Option[Pos])] = t match
    case n @ NamedType(name, args) =>
      (if skip(name) then Nil else List((name, n.pos))) ::: args.flatMap(namesIn(_, skip))
    case PtrType(inner)     => namesIn(inner, skip)
    case RefType(inner, _)  => namesIn(inner, skip)
    case WeakType(inner)    => namesIn(inner, skip)
    case ArrayType(_, elem, _) => namesIn(elem, skip)
    case VectorType(_, elem)   => namesIn(elem, skip)
    // A value argument names no declaration, so a signature can expose nothing through one — the
    // same reason an array's length is not walked one line up.
    case _: ValueArgType     => Nil
    case VolatileType(inner) => namesIn(inner, skip)
    case TupleType(parts, _) => parts.flatMap(namesIn(_, skip))
    // A pack names a parameter of the declaration being checked, never a declaration of its own, so
    // a signature can expose nothing through one.
    case _: PackType         => Nil
    // A callable mentions the library's call trait, which is public and is nobody's to hide; what a
    // signature can expose through one is its parameters and its result, so those are what is walked.
    case f: FnType           => (f.params :+ f.ret).flatMap(namesIn(_, skip))
    // A function pointer names no declaration of its own, so what a signature can expose through one
    // is what it is called with — the same walk, for the same reason.
    case CFnType(ps, r)      => (ps :+ r).flatMap(namesIn(_, skip))
    // A projection exposes its **subject**, which is the name a signature actually spells. What the
    // projection resolves to is chosen by an implementation rather than written here, and is held to
    // the same rule where that implementation supplies it.
    case AssocType(base, _)  => namesIn(base, skip)
    // An object's binding is written in the signature, so what it names is exposed by it — unlike a
    // projection, where the implementation rather than this signature chooses.
    case AssocArgType(_, t)  => namesIn(t, skip)
    // A `some` result names no declaration: the bound is the trait, whose own reach is checked where
    // the bound resolves, and the concrete type is the implementation's.
    case _: SomeType         => Nil

  /** The declaration a name in a type position stands for: a struct, an enum, or — behind a memory
   * mode, where a trait object writes one — a trait.
   */
  private def namedDecl(written: String): Option[String] = quietly(typeKey(written).orElse(traitKey(written)))

  /** A resolution made for the sake of the comparison, which is not the place a name that resolves
   * to nothing is worth reporting from: a scalar and a type parameter answer `None` here, and a name
   * the file may not reach was reported where the signature was resolved.
   */
  private def quietly(lookup: => Option[String]): Option[String] =
    try lookup
    catch
      case _: AnalyzerError => None
      case _: Poisoned      => None
}
