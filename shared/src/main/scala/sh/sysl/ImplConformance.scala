package sh.sysl

import scala.collection.mutable

/** Whether an `impl` block supplies what the trait it names asked for, and what to build from what
 * it did supply.
 *
 * Conformance is **nominal and exact**: the type must supply every method the trait declares without
 * a default, with a matching kind, receiver, parameter list and result, and no method the trait does
 * not declare at all. Nothing is inferred from a shape that happens to line up, which is what makes
 * a useful diagnostic possible — a mismatch names the trait it fails to satisfy and the member it
 * fails at, rather than reporting that no implementation was found.
 *
 * The defaults are the other half of the same walk. A member the trait supplied a body for is one
 * the block did not have to write, so the pass that finds what is missing also yields what is
 * inherited, and each inherited one is synthesised into an ordinary function under the implementing
 * type's own name. A property is no different: its declaration form already carries a body, so the
 * only question was whether the receiver it never spells becomes a `self` parameter. It does.
 */
trait ImplConformance extends MemberLowering {

  /** Verifies that `impl` supplies the members `tr` declares, each with an identical resolved
   * signature, and yields the **defaults it inherits** — the members it did not write and does not
   * have to, because the trait supplied a body for them.
   *
   * A member the trait declares without a default and the block leaves out, a member the trait does
   * not declare at all, or a mismatched kind, receiver, parameter, or result is reported against the
   * trait it fails to satisfy.
   */
  protected def checkConformance(
      tr: TraitDecl,
      impl: ImplDecl,
      home: MemberHome,
      sig: Map[String, Type],
  ): List[MethodDecl] = {
    val declared  = tr.methods.map(_.name).toSet
    val inherited = mutable.ListBuffer.empty[MethodDecl]
    val shown     = traitShown(impl)

    for tm <- tr.methods do
      impl.methods.find(_.name == tm.name) match
        case Some(im) =>
          checkSignature(home.label, shown, tm, im, sig, scopeFor(tr.name))
          checkOverrideMarking(shown, tm, im)
        case None if tm.body.nonEmpty => inherited += tm
        case None =>
          err(s"'${home.label}' does not implement '$shown': ${kind(tm)} " +
            s"'${DeclParser.sourceName(tm.name)}' is missing")

    for im <- impl.methods do
      if !declared.contains(im.name) then
        err(s"trait '$shown' declares no ${kind(im)} '${DeclParser.sourceName(im.name)}', so this " +
          "'impl' cannot define it")

    inherited.toList
  }

  /** Whether a member the block wrote says `override` exactly when it is one (`reference/traits.md § Replacing a default says override`).
   *
   * The trait's side is the whole of the test, and it is one question: does the member the block
   * wrote **replace a body**, or does it **answer a requirement**? A trait member with a body is a
   * default the block is replacing, and one without is a promise the block is keeping — so the
   * keyword is required for the first and refused for the second.
   *
   * The point is what an `impl` block reads like from the outside. Without the keyword a member that
   * replaces a default and one that supplies a requirement are written identically, and telling them
   * apart means opening the trait; with it, the block says which each of its members is doing. It
   * costs almost nothing, because an implementation content with a default writes no member at all.
   */
  protected def checkOverrideMarking(traitName: String, tm: MethodDecl, im: MethodDecl): Unit =
    // Reported at the member rather than at the block, which is where the word is missing or spare —
    // an `impl` of a trait with several defaults would otherwise put every one of these on its
    // opening line and leave the reader to work out which member was meant.
    at(im.pos) {
    val written = DeclParser.sourceName(tm.name)
    // The words to write, which for a setter are not its name: `override set count(x)` is the whole
    // member, and telling a reader to say `override count` would send them to the getter.
    val say     = if DeclParser.isSetter(tm.name) then s"set $written" else written

    if tm.body.nonEmpty && !im.overrides then
      err(s"trait '$traitName' supplies a body for ${kind(tm)} '$written', so writing one here " +
        s"replaces it — say 'override $say', or leave the member out to keep the trait's")
    else if tm.body.isEmpty && im.overrides then
      err(s"trait '$traitName' declares ${kind(tm)} '$written' without a body, so this member " +
        "supplies what the trait asked for rather than replacing anything — 'override' says a body " +
        "was replaced")
    }

  /** What a diagnostic calls one member of a trait. The three kinds are told apart by shape rather
   * than by a keyword, so a message that names the wrong one sends the reader looking for the wrong
   * mistake.
   */
  protected def kind(m: MethodDecl): String =
    if DeclParser.isSetter(m.name) then "setter for property"
    else if m.isProperty then "property"
    else if m.receiver.isEmpty then "associated function"
    else "method"

  /** Compares one implementing method against the trait's signature: same receiver mode, same
   * parameter types in order, and the same result.
   *
   * Both sides are resolved with `Self` bound to the implementing type, which is what makes a
   * signature written with `Self` and one written with the concrete name the same signature
   * (`reference/traits.md § Declaring a trait`) — the comparison is between *resolved* types, so it
   * does not matter which spelling either side chose.
   */
  /** The trait an `impl` is for, as a message spells it — which for a call trait is the arrow the
   * block was written with, not the arity-carrying declaration behind it (`reference/types.md §
   * Function types`).
   */
  protected def traitShown(impl: ImplDecl): String =
    if Type.Fn.isCall(impl.traitName) && impl.traitArgs.nonEmpty then
      s"Fn(${impl.traitArgs.init.map(_.show).mkString(", ")}) -> ${impl.traitArgs.last.show}"
    else qn(impl.traitName)

  protected def checkSignature(
      forType: String,
      traitName: String,
      tm: MethodDecl,
      im: MethodDecl,
      self: Map[String, Type],
      traitScope: Scope,
  ): Unit = {
    // A message names the member the way it was **written**: a setter is filed under a name holding
    // `$`, which is not sysl and must never reach a reader.
    val name = DeclParser.sourceName(im.name)

    // Which *kind* of member it is comes first, because two of the three kinds have no receiver
    // between them: a property and an associated function both answer `None`, so the receiver
    // comparison below would let one stand for the other without ever noticing.
    if tm.isProperty != im.isProperty then
      if tm.isProperty then
        err(s"'$name' is a property of trait '$traitName', so an implementation writes it as " +
          s"'$name -> …' with no parameter list")
      else
        err(s"'$name' is a method of trait '$traitName', so an implementation writes it with a " +
          "parameter list — a property has none")
    if tm.receiver != im.receiver then
      err(s"method '$name' of 'impl $traitName for $forType' takes a different receiver than " +
        "the trait declares")
    // A member of an `impl` is the trait's member supplied, so its shape is the trait's — including
    // how many types of its own it is generic over.
    if tm.tparams.length != im.tparams.length then
      err(s"${kind(im)} '$name' of 'impl $traitName for $forType' declares " +
        s"${quantity(im.tparams.length, "type parameter")}, but trait '$traitName' declares " +
        s"${tm.tparams.length}")
    if tm.params.length != im.params.length then
      err(s"method '$name' of 'impl $traitName for $forType' takes ${im.params.length} " +
        s"parameters, but the trait declares ${tm.params.length}")
    // A `...` is part of what a caller may write, so an implementation that has one where the trait
    // has none — or the other way about — is a different promise, not a wider one.
    if tm.variadic != im.variadic then
      err(s"method '$name' of 'impl $traitName for $forType' " +
        s"${if im.variadic then "takes" else "does not take"} a '...', but trait '$traitName' " +
        s"declares ${if tm.variadic then "one" else "none"}")

    // **A member's own type parameters stand for one another across the two signatures, by
    // position.** The trait's `map[U]` and the block's `map[V]` are the same promise — the letters
    // are each file's own, and what corresponds is the order they were declared in. So one set of
    // stand-ins is built from the trait's declaration and *both* sides are resolved against it, the
    // block's under its own spelling. `Type.Abstract` is identified by its name, so a parameter
    // renamed this way compares equal to the one it stands for, which is exactly what the two
    // signatures have to agree about.
    //
    // Resolving without them reported the member's own parameters as unknown types — a message
    // about the trait, in a walk the trait had nothing wrong with.
    // `self` is the outer scope here — the trait's own parameters, and `Self` bound to the
    // implementing type — because a member's bound may name one of them: a bare-arrow parameter on
    // `trait Mapping[T]` desugars to a bound of `Fn(T) -> U`, whose `T` is the trait's.
    val stand    = abstractSubst(tm.tparams, tm.bounds, tm.tvalues, tm.tpacks, self)
    val traitOwn = self ++ stand
    val implOwn  = self ++ im.tparams.zip(tm.tparams.map(stand)).toMap

    // The two signatures were written in two files — the trait's and the block's — so each side is
    // resolved where it was written, under that file's module and its imports. A `Point` in the
    // trait's `-> Point` is the trait module's `Point`, whether or not the implementing module has
    // one of its own.
    for (tp, ip) <- tm.params.zip(im.params) do
      val want = inScope(traitScope)(resolveType(tp.typ, traitOwn))
      val got  = resolveType(ip.typ, implOwn)
      if want != got then
        err(s"parameter '${ip.name}' of method '$name' is ${show(got)}, but trait '$traitName' " +
          s"declares ${show(want)}")

    val want = inScope(traitScope)(tm.retType.map(resolveReturn(_, traitOwn)).getOrElse(Type.Unit))
    val got  = im.retType.map(resolveReturn(_, implOwn)).getOrElse(Type.Unit)
    if want != got then
      err(s"method '$name' returns ${show(got)}, but trait '$traitName' declares ${show(want)}")
  }

  /** Builds the function a member lowers to: the receiver becomes an ordinary first parameter
   * named `self`, carrying the memory mode its sigil asked for. A property's receiver is an
   * implicit by-value read; an associated function has no receiver. On a generic type the self
   * type is the type applied to its own parameters (`Box[T]`, not `Box`), and the lowered function
   * inherits those parameters, so instantiating it at a concrete `Box[int]` substitutes `T` in the
   * receiver, the result, and the body alike.
   *
   * A member's **own** parameters follow the type's, in that order and never interleaved, because
   * the two are fixed from different places and the order is what lets them be: a call reads the
   * type's off the receiver and appends the ones it solved, so the same positional substitution
   * serves a member that adds parameters and one that adds none.
   *
   * Which of them stand for a **value** and which for a **list of types** comes from both sides for
   * the same reason the names do. A block declares the ones its subject is generic over and a member
   * declares its own, and the lowered function is generic over the two together — so a member taking
   * `[..A]` is walked with a pack standing at a pack, rather than at one opaque type whose parts
   * nothing could reach.
   */
  protected def synthesize(home: MemberHome, m: MethodDecl): FuncDecl = {
    val name = s"${home.symbol}.${m.name}${home.alt}"

    FuncDecl(
      name,
      home.tparams ::: m.tparams,
      receiverParam(m, selfRefFor(name, home)).toList ::: m.params,
      m.retType,
      m.body,
      home.bounds ++ m.bounds,
      m.variadic,
      tvalues = home.tvalues ++ m.tvalues,
      tpacks = home.tpacks ++ m.tpacks,
      // The three annotations a member may carry are about its **parameters**, so they cross to the
      // lowered function unchanged — the receiver is prepended in front of them and the names they
      // gave still name the same parameters. Nothing here resolves one: `@crossing` and the frames
      // are checked against the signature later, exactly as a free function's are.
      reads = m.reads,
      writes = m.writes,
      crossing = m.crossing,
      // `@noinline`, `@inline` and `@cold` are about the function a member lowers to, which is this
      // one, so they cross unchanged as well — and they reach every instantiation of a generic
      // member for the reason the signature does: the declaration is what is instantiated.
      noinline = m.noinline,
      cold = m.cold,
      inline = m.inline,
    ).setPos(m.pos)
  }

  /** The receiver's type as a member's lowered signature carries it.
   *
   * A member written here names it as the block did. An **inherited default** is read in the trait's
   * terms (`defaultHome`), because that is where its body and the rest of its signature came from —
   * and the subject is the one thing in it that was not written there, so a copy naming it as the
   * block spelled it would have that spelling looked for in the trait's module. The copy therefore
   * names it `Self`, which is what the trait itself calls the implementing type, and the binding for
   * `Self` is made where the subject really was written.
   */
  protected def selfRefFor(name: String, home: MemberHome): TypeRef =
    if defaultOrigin.contains(name) then NamedType(selfName) else home.selfRef

  /** The `self` parameter a member's receiver becomes, at the self type it is a member of. A
   * property's receiver is an implicit by-value read; an associated function has none.
   */
  protected def receiverParam(m: MethodDecl, selfRef: TypeRef): Option[Param] = m.recvMode.map {
    case RecvMode.ByValue     => Param("self", selfRef)
    case RecvMode.ByPtr       => Param("self", PtrType(selfRef))
    case RecvMode.ByRef(sync) => Param("self", RefType(selfRef, sync))
  }
}
