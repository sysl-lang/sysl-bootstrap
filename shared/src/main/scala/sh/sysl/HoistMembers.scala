package sh.sysl

import scala.collection.mutable

/** Registering a type's **members**, and the `impl` blocks that give a type someone else's.
 *
 * A member is lowered to a function declaration under a mangled name, so a call on a value
 * resolves through the ordinary member path with no dispatch machinery of its own — and a
 * trait's methods become the implementing type's by the same route, which is what makes
 * `p.show()` one lookup whether `show` was written on `Point` or on a trait `Point` implements.
 *
 * `MemberHome` is what the two paths share: where a member belongs, what its `Self` is, and
 * which of the block's own parameters the subject binds. Both `hoistMembers` (a type's own) and
 * `hoistImpl` (a trait's, filed under the type) build one and hand it to `hoistMemberList`.
 */
trait HoistMembers extends HoistImpl {

  /** Records a type's members and lowers each to a function declaration under the mangled name
   * `Type.member`, whose signature is registered so calls resolve like ordinary ones.
   *
   * A member of a concrete type is hoisted eagerly, so an uncalled member is still type-checked at
   * its definition. A member of a *generic* type cannot be: its signature mentions the type's
   * parameters, which have no meaning until a call fixes them, so it is stored generic in
   * `genericMembers` and instantiated on demand at each call site. A method reads those arguments
   * off its receiver; an **associated function** has no receiver, so its call infers them from what
   * it is passed and from the type the context expects, exactly as a call to a generic free function
   * does. Members that introduce their own type parameters wait on later work and are rejected with
   * a clear diagnostic rather than silently mishandled.
   *
   * A generic type's members are handed to the definition-time pass of `reference/generics.md §
   * Bounds` all the same. What they may assume of the type's parameters is what the type asks of
   * them — nothing, where it asks nothing — and that is a rule a body can be held to before
   * anything instantiates it, exactly as a bounded generic function's body is.
   */
  protected def hoistMembers(tname: String, members: List[MethodDecl], out: mutable.ListBuffer[FuncDecl]): Unit = {
    val (tparams, taken, noun) = nominal(tname).get
    val bounds                 = nominalBounds(tname)

    checkBoundNames(tname, bounds)

    // A member of a concrete type may write `Self` for the type it is a member of, exactly as an
    // `impl`'s method may. A member of a *generic* one has its `Self` bound one step later, at each
    // instantiation, since `Box[T]` is not a type until `T` is one — `genericSelf` is where the
    // reference waits for that.
    val self = if tparams.nonEmpty then Map.empty else concrete(tname).fold(Map.empty[String, Type])(selfBinding)

    val lowered = hoistMemberList(
      MemberHome(
        tname,
        qn(tname),
        tname,
        None,
        NamedType(tname, tparams.map(NamedType(_, Nil))),
        tparams,
        bounds,
        taken,
        noun,
        self,
        // Which of those parameters stand for **values** rather than types, for the same reason an
        // `impl`'s members are told: the definition-time pass walks a member's body with the
        // parameters standing in for themselves, and a value parameter stands at a value. Without
        // it the `N` of `struct Buf[const N: usize]` names nothing inside the type's own methods.
        tvalues = nominalValues(tname),
      ),
      members,
      out,
    )

    abstractMembers ++= lowered.filter(_.tparams.nonEmpty)
  }


  /** Every trait default, as the generic function each one is: one type parameter, `Self`, bounded
   * by the trait that declared it.
   *
   * That is what a default body *means* — it may assume of its receiver exactly what the trait
   * promises, and nothing else — so writing it down this way is what lets the definition-time pass
   * of `reference/generics.md § Bounds` check it once, at the trait, with the machinery a bounded
   * generic already uses. The declarations exist only for that walk; the body a program runs is the
   * copy `hoistImpl` makes for each implementing type.
   *
   * A **property** with a body is a default like any other, and needs nothing said about it here: its
   * declaration form already carries a body, so the only question was whether the trait was allowed
   * to write one, and the receiver it never spelled becomes a `self` parameter the same way.
   */
  protected def traitDefaults: List[FuncDecl] =
    for
      tr <- traitDecls.values.toList
      m  <- tr.methods if m.body.nonEmpty
    yield defaultAt(tr, FuncDecl(
      s"${tr.name}.${m.name}",
      // The member's **own** parameters stand beside the trait's, for the reason the trait's stand
      // beside `Self`: they are as unknown inside the body as either, and a body naming one is
      // ordinary. Leaving them out reported the member's own parameters as unknown types, in a walk
      // whose whole job is to check that body once.
      selfName :: (tr.tparams ::: m.tparams),
      receiverParam(m, NamedType(selfName, Nil)).toList ::: m.params,
      m.retType,
      m.body,
      // A generic trait's default is generic over the trait's parameters too — they are as unknown
      // inside the body as `Self` is — and what `Self` promises is the trait *applied* to them,
      // which is the one promise every implementation of it makes.
      bounds = tr.bounds ++ m.bounds +
        (selfName -> List(BoundRef(tr.name, tr.tparams.map(NamedType(_, Nil))))),
      variadic = m.variadic,
      tvalues = m.tvalues,
      tpacks = m.tpacks,
      noinline = m.noinline,
      cold = m.cold,
      inline = m.inline,
    ).setPos(m.pos))

  /** Records that a default is read **in its trait's terms**, and hands it back.
   *
   * These declarations are synthesized rather than hoisted, so nothing has filed where they were
   * written — and `scopeFor` answers for a key it does not know with the module the key names and
   * **no imports at all**. A default calling anything its file imported was therefore undefined the
   * moment the definition-time pass looked at it, and the pass drops what it cannot resolve, so the
   * body was silently not checked. It is the same fact `MemberLowering` records for the copy made
   * per implementing type, which is why the copies resolved what the original could not.
   */
  private def defaultAt(tr: TraitDecl, fd: FuncDecl): FuncDecl = {
    declScope(fd.name) = scopeFor(tr.name)
    fd
  }
  /** Checks that every bound a declaration writes names a trait and applies it to as many arguments
   * as it declares, whichever declaration form wrote it — a function, a struct, an enum, a trait. A
   * bound is a trait and nothing else (`reference/generics.md § Bounds`), so a name that is a
   * struct, a scalar, or nothing at all is reported here rather than silently promising something
   * no type could ever be held to.
   *
   * It runs in a pass after every type is registered, so a bound may name a trait declared further
   * down the file. What the *arguments* are is left to `resolveBound`, which is reached wherever the
   * substitution that gives them meaning exists; the arity is answerable here and worth saying at
   * the declaration rather than at whatever first applied it.
   */
  protected def checkBoundNames(name: String, bounds: Map[String, List[BoundRef]]): Unit =
    for (tp, traits) <- bounds; tr <- traits do
      // The whole check points at the bound rather than at the declaration carrying it, because
      // that is the text that is wrong. It also means a bound naming something the file may not
      // reach is reported at one place — this and the definition-time walk both resolve the name,
      // and two identical complaints about one bound are one mistake reported twice.
      at(tr.pos) {
        traitKey(tr.name).map(traitDecls) match
          case None       => err(s"the bound on '$tp' in '$name' names '${tr.name}', which is not a trait")
          case Some(decl) =>
            checkTraitArity(tr.name, decl.tparams, decl.tdefaults, tr.args.map(_ => Type.Unknown))
      }
}
