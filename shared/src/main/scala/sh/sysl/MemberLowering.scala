package sh.sysl

import scala.collection.mutable

/** What a declared member becomes, and the description of *where it lives* that says so.
 *
 * Every member in the language ends up as an ordinary function under a mangled name, whether it was
 * written in a type's own body, in an `impl` block, or as a trait's default. `MemberHome` is what
 * makes that one path rather than three: it carries everything a member's signature resolves against
 * that a call does not have to supply — the key its owner is filed under, the receiver's type as
 * written, the type parameters the signature may mention and in the order the implementing type
 * applies them, what `Self` means, and what a diagnostic should call the thing.
 *
 * Collecting those into one value is what lets `hoistMemberList` be written once. A member from a
 * struct body and a member from a generic `impl` for a composed type differ in every one of those
 * fields and in nothing else, so the lowering reads the home and does not care which it was.
 */
object MemberLowering {

  /** The name [[MemberLowering.callBounds]] gives the type parameter it adds for a parameter that
   * wrote a bare arrow — `f: A -> B` becoming `[F: Fn(A) -> B](f: F)`.
   *
   * It holds a `$`, which no source name may, so a program can neither collide with one nor be
   * mistaken for one. The predicate exists because the rewrite is **lossy in the one place that
   * matters**: after it, a parameter that a reader wrote as a callable is an ordinary named type,
   * and a rule keyed on the written type — a trailing block's, which has to decide between a
   * collection and a callable — can no longer see what was written. Asking the name is what gives
   * it back.
   */
  def callBoundName(n: Int): String = s"${Modules.sep}F$n"

  def isCallBound(name: String): Boolean = name.startsWith(s"${Modules.sep}F")
}

trait MemberLowering extends TypeResolution {

  /** Four answers the lowering needs from the traits mixed in after it. Each belongs to a later
   * layer, and each is reached by the one walk that lowers every member — which is exactly why they
   * are named here: the coupling runs in this direction only.
   */

  /** Whether a member's own type parameters collide with names already spoken for. */
  protected def checkBoundNames(name: String, bounds: Map[String, List[BoundRef]]): Unit

  /** Whether a `@crossing` above a member names parameters it actually has (card `0313`). */
  protected def checkCrossingNames(f: FuncDecl): Unit

  /** Turning a trait's default into an ordinary function under the implementing type's name. */
  protected def synthesize(home: MemberHome, m: MethodDecl): FuncDecl

  /** Where a default's body was written, and how a shape like `*Trait` is described to a reader. */
  protected def defaultHome(member: String): Option[Scope]
  protected def everyShape(head: String): String

  /** Everything hoisting a list of members needs to know about the type they belong to, whichever
   * declaration form brought them — a struct or enum body, or an `impl` block.
   *
   *   - `key` is what the members are filed under. For a block matching a **shape** it is the shape
   *     itself (`[]`), which is what a lookup falls back to when the type it is asked about has no
   *     members of its own.
   *   - `label` is what a diagnostic calls the type, which is the key everywhere the key is
   *     something a programmer would recognise — and the subject as written where it is not, since
   *     `[]` names nothing and `[]T` names what the block covers.
   *   - `symbol` is the same type mangled, which is what the lowered functions are *named*. The two
   *     differ only for a type that has no name of its own: `[]int` is a fine key and an impossible
   *     LLVM symbol, so its members are emitted under `slice.int`.
   *   - `head` is the shape these members share a namespace with, where they have one — the shape
   *     itself for a block matching one, and its own shape for a composed type written out in full.
   *     A type's members are one namespace whatever brought them, and this is what carries that rule
   *     across the two ways a composed type can come by one.
   *   - `selfRef` is the receiver's type as written, so a `self` parameter needs no reconstructing.
   *   - `tparams` are the parameters a member's signature may mention — a generic type's own, or a
   * generic `impl`'s, **in the order the implementing type applies them**, so that instantiating a
   * member from a receiver's type arguments substitutes them positionally. `bounds` is what was
   * asked of them where they were declared, and it is what the members may assume. `tvalues` says
   * which of them stand for **values** (`reference/generics.md § A parameter may stand for a
   * value`) and at what type, since a member's body reads one of those as the constant it is rather
   * than as an opaque type.
   *   - `taken` are the names already spoken for inside the body (a struct's fields, an enum's
   *     variants), and `noun` what a diagnostic calls one of those.
   *   - `self` is what `Self` means inside these members, empty where the answer waits for an
   *     instantiation (a generic type's members) or means nothing at all.
   *   - `outer` is what the **trait's** own type parameters mean, for the members an `impl` of a
   *     generic trait brings. They are fixed by the block rather than by anything a call does, so
   *     unlike `tparams` they are answers rather than questions, and a member's signature and body
   *     read them exactly as they read `Self`.
   *   - `alt` distinguishes the members of a second implementation of one trait from the first's.
   *     It is empty everywhere else, so a type's own body and its first `impl` file their members
   *     under exactly the names they were written with.
   *   - `fromTrait` is the trait an `impl` block is keeping, and `None` for a type's own body. It is
   *     what a use site's scope is asked about, since a trait's member is reachable only where the
   *     trait can be named.
   *   - `overrides` is whether the block said `override` (`reference/traits.md § override — when the overlap is deliberate`). What it turns off is the
   *     one-name-per-shape rule below: an override gives a written-out type a member the shape it
   *     belongs to also has, and that is what an override *is* rather than a collision — the two are
   *     the same trait's member, and `memberKey` asks the type's own key before the shape's, so one
   *     name still reaches one member.
   */
  protected case class MemberHome(
      key: String,
      label: String,
      symbol: String,
      head: Option[String],
      selfRef: TypeRef,
      tparams: List[String],
      bounds: Map[String, List[BoundRef]],
      taken: Set[String],
      noun: String,
      self: Map[String, Type],
      outer: Map[String, Type] = Map.empty,
      alt: String = "",
      fromTrait: Option[String] = None,
      overrides: Boolean = false,
      tvalues: Map[String, TypeRef] = Map.empty,
      /** Which of `tparams` stand for a **list** of types (`reference/generics.md § A parameter may
        * stand for a list of types`) — carried for the reason `tvalues` is, so a member's body is
        * walked with the pack standing at what a pack stands at rather than at one `Abstract` that
        * has no parts for `self.i` to reach.
        */
      tpacks: Set[String] = Set.empty,
  ) {

    /** Everything a member's signature resolves against that a call does not supply: the trait's
     * arguments, and `Self` where it is already known.
     */
    def fixed: Map[String, Type] = outer ++ self

    /** Whether this block is the one matching the shape, rather than one of the types it covers.
     * A composed type written out in full is never spelled as its own shape, so the keys settle it.
     */
    def shaped: Boolean = head.contains(key)
  }

  /** The instantiation of a non-generic struct or enum, which was made before any member was
   * hoisted. Read from the table rather than resolved again, so a type whose own declaration was
   * already reported does not report it a second time on the way to its members — the members are
   * still hoisted, with `Self` simply unbound.
   */
  protected def concrete(name: String): Option[Type] =
    structInsts.get(name).orElse(enumInsts.get(name))

  /** Whether a member's key is a name a call could be written through, which is what a member with
   * **no receiver** needs and what one with a receiver never asks for.
   *
   * Every declared type has one, and so does every built-in that a program spells: `u32.bits()` and
   * `real.epsilon()` reach an `impl`'s associated function exactly as `Box.of(…)` reaches a struct's.
   * The test is that the key is the canonical name of the type it resolves to, so an alias reaches
   * the one implementation rather than registering a second — `int` and `i32` are one key already,
   * and this asks the same question one layer up.
   */
  protected def reachableName(key: String): Boolean =
    nominal(key).isDefined || constrainedDecls.contains(key) ||
      scalarType(key).exists(t => Type.show(t) == key)

  protected def nominal(name: String): Option[(List[String], Set[String], String)] =
    structDecls
      .get(name)
      .map(s => (s.tparams, s.fields.map(_.name).toSet, "field"))
      .orElse(enumDecls.get(name).map(e => (e.tparams, e.variants.map(_.name).toSet, "variant")))

  /** Lowers a list of members to functions, shared by a type's own body and an `impl` block. Each
   * member is registered under (type, name) and synthesized to a `Type.member` function; a member
   * of a concrete type is type-checked eagerly, while a member of a generic type waits for a
   * concrete instantiation.
   *
   * The declarations come back so that a caller with something further to do with them — a generic
   * `impl`, whose members are checked at their definition — needs no second walk to find them.
   */
  /** Turns each bare-arrow parameter into the bounded type parameter it is sugar for
   * (`reference/types.md § Function types`).
   *
   * `map(self, f: A -> B)` is `map[$F: Fn(A) -> B](self, f: $F)`, and the rewrite happens at the
   * declaration so that *everything* after it — resolving the signature, inferring the argument,
   * monomorphizing the body, emitting the direct call — is the generic machinery that already
   * exists. The parameter is a bound and not a type, which is what makes the closure inlinable and
   * the call direct, and it is the whole reason the arrow is available in a parameter and nowhere
   * else.
   *
   * The added name holds a `$`, so nothing a program can write collides with it and a diagnostic
   * that has to name one reads as the synthetic thing it is. `None` where no parameter wrote an
   * arrow, so a declaration that did not use the sugar is left exactly as it was.
   */
  protected def callBounds(
      tparams: List[String],
      params: List[Param],
  ): Option[(List[String], List[Param], Map[String, List[BoundRef]])] = {
    val added = mutable.ListBuffer.empty[(String, List[BoundRef])]

    val rewritten = params.map { p =>
      p.typ match
        case a: FnType if a.bare =>
          checkFnArity(a)

          val tp = MemberLowering.callBoundName(tparams.length + added.length)

          added += ((tp, List(BoundRef(Type.Fn.base(a.params.length), a.params :+ a.ret).setPos(a.pos))))
          p.copy(typ = NamedType(tp).setPos(a.pos)).setPos(p.pos)
        case _ => p
    }

    Option.when(added.nonEmpty)((tparams ::: added.map(_._1).toList, rewritten, added.toMap))
  }

  /** [[callBounds]] applied to each member of a list, which is what a **declaration** wants: a
   * member's bare-arrow parameters are sugar wherever the member was written, so the rewrite belongs
   * where the declaration is registered rather than where its members are hoisted.
   *
   * **It is idempotent**, which is what lets a list be lowered at registration and hoisted later
   * without asking whether it has been through here already: a rewritten parameter is a
   * `NamedType`, and `callBounds` looks only for a bare `FnType`.
   */
  protected def loweredMembers(members: List[MethodDecl]): List[MethodDecl] =
    members.map(m =>
      callBounds(m.tparams, m.params).fold(m) { (tps, ps, bs) =>
        m.copy(tparams = tps, params = ps, bounds = m.bounds ++ bs).setPos(m.pos)
      },
    )

  protected def hoistMemberList(
      home: MemberHome,
      members: List[MethodDecl],
      out: mutable.ListBuffer[FuncDecl],
  ): List[FuncDecl] = {
    val lowered = mutable.ListBuffer.empty[FuncDecl]

    // An `impl` block need not restate the property it is writing: the trait declared it, and a
    // block supplying only the setter is supplying exactly what the trait left open.
    val inherited = home.fromTrait.toList.flatMap(tr => traitDecls.get(tr).toList.flatMap(_.methods))

    for original <- pairSetters(members, home.label, inherited) do
      val m = callBounds(original.tparams, original.params).fold(original) { (tps, ps, bs) =>
        original.copy(tparams = tps, params = ps, bounds = original.bounds ++ bs).setPos(original.pos)
      }

      currentPos = m.pos.orElse(currentPos)

      // A member's parameters and its type's are two lists that end up in one signature, so a name
      // used by both would leave the lowered function with two parameters of that name and nothing
      // to say which one a `T` in the body meant.
      for tp <- m.tparams if home.tparams.contains(tp) do
        err(s"'${home.label}' already declares a type parameter '$tp', so member '${m.name}' " +
          "cannot declare one of that name")

      checkBoundNames(s"${home.label}.${m.name}", m.bounds)

      // An associated function is reached by naming its type — `Box.of(…)`, `u32.bits()` — so the
      // type needs a name a call can be written through. A composed type has none: `[]int` is a
      // type an `impl` may be written for and not something that can stand in call position, and a
      // block matching a shape is for a whole family at once. Such a member would be registered and
      // never callable, which is worth saying at the declaration.
      if m.isAssociated && !reachableName(home.key) then
        err(s"'${m.name}' has no receiver, and '${home.label}' is not a name a call could reach it " +
          "through — give it a 'self' parameter")

      // The name this member is actually filed under. It is the one it was written with everywhere
      // but a second implementation of one trait, whose members would otherwise collide with the
      // first's — and the collisions asked about below are still asked about the *written* name,
      // since that is the one a program spells.
      //
      // A name some **other trait** already gave the type is not a collision, and the block was
      // given a suffix of its own above so that it is not one here either: the two members are told
      // apart by which trait is in scope at the use (`reference/modules.md § Visibility`). What
      // still collides is a name the type's **own** body has, which is reachable wherever the type
      // is and so has no scope to be told apart by.
      val filed = m.name + home.alt

      // Both messages name the member the way it was **written**, which for a setter is the
      // property it writes rather than the name it is filed under — nothing a reader could type
      // holds a `$`, and telling them one would be 0139 in a fresh place.
      val written = DeclParser.sourceName(m.name)

      if memberDecls.contains((home.key, filed)) then
        if DeclParser.isSetter(m.name) then
          err(s"type '${home.label}' already has a setter for '$written'")
        else err(s"type '${home.label}' already has a member named '$written'")
      if home.taken.contains(m.name) then
        err(s"type '${home.label}' has both a ${home.noun} and a member named '$written'")

      // A composed type's members and its shape's are one namespace, so that a name reaches one
      // member however the type came by it. Both directions are asked, since a file may write the
      // shape before the types it covers or after them. A second implementation's members are left
      // out: the name they are filed under is one no other block can have written.
      //
      // An **override** is left out too, and it is the one case where sharing the name is the point:
      // it supplies the same trait's members for one type of the shape, so a `display` here and a
      // `display` on every slice are one member seen at two levels of generality rather than two a
      // call would have to choose between. `memberKey` asks the type's own key first, which is what
      // makes the more specific one the answer.
      for h <- home.head if home.alt.isEmpty && !home.overrides do
        if home.shaped then
          for written <- composedMembers.get((h, m.name)) do
            err(s"'$written' already has a member named '${m.name}', and this 'impl' would give " +
              s"${everyShape(h)} one — a call on a '$written' could mean either")
        else
          if memberDecls.contains((h, m.name)) then
            err(s"${everyShape(h)} already has a member named '${m.name}', so '${home.label}' " +
              "cannot declare one of its own")
          composedMembers((h, m.name)) = home.label

      for ty <- home.self.get(selfName) do
        // A built-in's catalog methods are the compiler's (`reference/expressions.md § Operator
        // dispatch`), and member lookup would find a member of the same name first — so an `impl`
        // of some *other* trait may not quietly take `5.add` over from the `Add` the type is
        // already a member of.
        // A **second** implementation is left out, exactly as it is from the composed-member check
        // above and for the same reason: its members carry a suffix, so `real.mul.2` is a name the
        // built-in `mul` was never under and cannot be hidden by. That is what lets a scalar sit on
        // the left of an operator — `impl Mul[Point, Point] for real` (card `0385`) — while a block
        // that would take the plain name over is refused as it always was.
        for tr <- CoreTraits.declaring(m.name) if home.alt.isEmpty && CoreTraits.builtin(tr, ty) do
          err(s"'${m.name}' is how '$tr' is implemented for ${show(ty)}, and the compiler provides " +
            s"that — a member of this name would hide it")

        // `len` and `bytes` are the same situation one step further out: a member of a built-in that
        // the compiler supplies, reached ahead of the member table rather than through it.
        if builtinMember(ty, m.name) then
          err(s"'${m.name}' is a member the compiler provides for ${show(ty)} — a member of this " +
            "name would hide it")

      // A shape block never reaches the loop above, because `Self` is not a type until an
      // instantiation says what its element is. Asking the shape instead is what closes the one
      // spelling that got through: every written-out sequence was refused a `len` and every slice
      // at once was not.
      for h <- home.head if home.shaped && builtinShapeMember(h, m.name) do
        err(s"'${m.name}' is a member the compiler provides for ${everyShape(h)} — a member of " +
          "this name would hide it")

      memberDecls((home.key, filed)) = m

      // Which trait this member arrived with, for the members an `impl` brought. A type's own body
      // records nothing, and that absence is what makes its members reachable wherever the type is.
      for tr <- home.fromTrait do memberTrait((home.key, filed)) = tr

      // **A destructor has no caller in the source**, which is the one thing that makes it
      // different from every other member here: what calls it is the release hook the emitter
      // builds, and that is decided by a payload type at a site with no name in it. So nothing
      // would mark it reached, and pruning would drop the body while the hook still named it — a
      // link error against a symbol no line of the program mentions. Marked at the declaration
      // instead (`reference/memory.md § A destructor`).
      if filed == "drop" && home.fromTrait.contains(Library.key("Drop")) then dropsDeclared += home.key

      // A name a program spells that now reaches more than one member is recorded as reaching all of
      // them, so a call has the whole set to answer from. Both reasons a member is filed under
      // something other than its written name come through here — a second implementation of one
      // trait, and a second trait declaring a name the first already used.
      //
      // The bare name joins the set only if something was actually **filed** under it. A suffix is
      // taken by the whole block, so a member the suffixed block alone declares — a default the
      // trait supplied and the first trait never had — has no bare-named sibling, and listing one
      // would put a name in the set that nothing can be looked up under.
      if filed != m.name then
        val prior = memberAlts.getOrElse(
          (home.key, m.name),
          if memberDecls.contains((home.key, m.name)) then List(m.name) else Nil,
        )

        memberAlts((home.key, m.name)) = prior :+ filed

      val fd = synthesize(home, m)

      // A member is filed under the type it belongs to, which an `impl` in another module may have
      // named — and an inherited default's body is the trait's source, wherever the trait is. Both
      // resolve their names where they were written, so that is what is recorded here.
      declScope(fd.name) = defaultHome(fd.name).getOrElse(currentScope)
      // Declared here, though: the copy is this block's member, whichever module wrote its body.
      declOwner(fd.name) = currentScope.module

      lowered += fd

      // A signature mentioning a type parameter — the type's own, or the member's — has no meaning
      // until something fixes it, so the member is kept as written and made real per call.
      if fd.tparams.nonEmpty then
        genericMembers((home.key, filed)) = fd
        // On a generic type `Self` is the type applied to its own parameters, which is not a type
        // yet. The reference is what waits, and every substitution that fixes the parameters fixes
        // it too; on a concrete type it is already the answer and resolves to the same thing.
        genericSelf(fd.name) = (home.selfRef, currentScope)
        if home.outer.nonEmpty then genericOuter(fd.name) = home.outer
      else
        // The signature is resolved *before* the member joins the list of bodies to analyze, so a
        // type in it that does not resolve leaves the member out rather than half in: what the walk
        // takes off that list it looks the signature up for, and a member on it with none is a
        // crash where a diagnostic belongs.
        val signature =
          (fd.params.map(p => (p.name, resolveType(p.typ, home.fixed))),
           fd.retType.map(resolveReturn(_, home.fixed)).getOrElse(Type.Unit))

        out += fd
        if home.fixed.nonEmpty then memberSelf(fd.name) = home.fixed
        funcInsts(fd.name) = signature
        // Which function this member became, for the passes that read the member table and have no
        // receiver type to build the name from. Recorded in this branch alone: the signature is
        // what it is for, and a generic member has none until a call fixes its parameters.
        memberFuncs((home.key, filed)) = fd.name

        // The destructor is the one member nothing in the source reaches, so nothing else would
        // mark it (`reference/memory.md § A destructor`). Marked here, where the lowered name
        // exists.
        if filed == "drop" && home.fromTrait.contains(Library.key("Drop")) then funcsUsed += fd.name

      // A member is a function with a receiver in front, so the rules a signature is held to are
      // the same ones — asked of the *lowered* form, where the receiver is a parameter like any
      // other, which is what lets `add(self, ...)` anchor its tail on `self` while a receiverless
      // `make(...)` has nothing to anchor on. Asked after the member is registered, so a mistake
      // here does not also erase the member it is about.
      recover(())(at(m.pos)(checkSignatureRules(fd.name, fd.params, fd.retType, fd.variadic)))
      // A member's `@crossing` names its own parameters, and the lowered form is where those have
      // become an ordinary list — so the question is asked here for the same reason the signature
      // rules are, and answers about the same names a reader wrote.
      recover(())(at(m.pos)(checkCrossingNames(fd)))
      recover(())(at(m.pos)(checkValueParamArithmetic(
        fd.tvalues.keySet, fd.params.map(_.typ) ::: fd.retType.toList, fd.tparams.toSet, fd.tpacks)))

    lowered.toList
  }
}
