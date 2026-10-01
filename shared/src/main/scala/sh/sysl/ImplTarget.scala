package sh.sysl

/** Which type an `impl` block is for, and what its shape gives it.
 *
 * Split out of `HoistImpl`, which lowers the block once these are answered. It is the larger half
 * and the one with the vocabulary: a written subject may be a name, an applied generic, a memory
 * mode over either, or a shape like `*Trait` standing for every type at once, and each answers to a
 * different member home.
 *
 * The diagnostics are here because they are inseparable from the resolution — a coherence failure
 * has to name *whose* trait and *whose* type in the words the declaration used, so the phrasing is
 * built from the written reference rather than from the resolved type, and the functions that build
 * it read the same reference the resolution does.
 */
trait ImplTarget extends ImplConformance {

  /** Which promise an `impl` block supplies: the trait, at the arguments this block writes for it.
   *
   * An argument may name one of the block's own parameters — `impl[T] Index[usize, T] for Buf[T]`
   * says a `Buf` of anything is read by a `usize` and gives back whatever it holds. That is one
   * promise per instantiation, exactly as a defaulted argument list on a generic subject is: a
   * `Buf[int]` implements `Index[usize, int]` and nothing else.
   *
   * Nothing has to be checked here for that to hold, because a generic block's parameters are
   * already exactly the arguments of the type it is for — `implArgs` and `shapeArgs` require each
   * one to appear in the subject and to appear once. So a parameter an argument can name is one the
   * subject settles, and the open case those checks refuse (`impl[V, T] From[T] for Wrapper[V]`,
   * where nothing would ever fix `T`) never reaches this far.
   *
   * What makes it safe beyond that is that a **generic type** admits no more specific block: an
   * `impl` for one covers every instantiation, and `impl Index[usize, int] for Buf[int]` is refused
   * outright (`implTarget`). So one block per (trait-at-arguments, generic type) still holds, and a
   * parameter the subject settles is a key that matches one thing per subject rather than many.
   * `override` (`reference/traits.md § override — when the overlap is deliberate`) does not reach
   * this: what it relaxes is a *shape* overlapping a type written
   * out in full, which is a second key rather than a second block under this one.
   */
  protected def implBound(impl: ImplDecl, tr: TraitDecl): (Type.Bound, List[Type]) = {
    checkTraitArity(qn(impl.traitName), tr.tparams, tr.tdefaults, impl.traitArgs.map(_ => Type.Unknown))

    // The block's parameters resolve to themselves so that naming one here is caught as the thing it
    // is, rather than reported as an unknown type — which would send the reader looking for a
    // declaration rather than at the argument they meant to fix.
    val declared = abstractSubst(impl.tparams, impl.bounds, impl.tvalues, impl.tpacks)

    // **Sandboxed for the same reason the subject below is: an `Abstract` is its name.** A written
    // argument may be a generic type applied to this block's own parameters — `Mul[Vector[T], T]`,
    // the shape a dot product has — and resolving it instantiates `Vector` at a stand-in called `T`.
    // That instantiation keys on the letter, so left registered it is handed to the next declaration
    // whose parameter is also spelled `T`, along with the *fields* it resolved: a `Vector[T]` holding
    // a `&Buf[T]` registers `Buf` at this block's `T`, and `sysl.container.Heap[T: Ord]` then reads
    // its own elements back at a stand-in bounded by something else entirely. The diagnostic lands in
    // the library, names `Ord`, and has nothing to do with the block that caused it.
    //
    // A **value** argument is read as the trait's parameter declares it (`traitArgs`), and one that
    // is a value parameter of this block — `impl[const M: usize] Bytes[M] for Bits[M]` — stands in
    // for itself, as a type parameter does: the block is filed once, and each subject's own `M`
    // replaces the stand-in, where the zero a generic body is checked at would file it at zero.
    def own(ref: TypeRef): Option[Type] = ref match
      case NamedType(n, Nil) if impl.tvalues.contains(n)      => Some(Type.Abstract(n, Nil))
      case ValueArgType(Ident(n)) if impl.tvalues.contains(n) => Some(Type.Abstract(n, Nil))
      case _                                                  => None

    def read() =
      traitArgs(Some(impl.traitName), impl.traitArgs, declared).zip(impl.traitArgs).zipWithIndex.map {
        case ((t, ref), i) if tr.tvalues.contains(tr.tparams.lift(i).getOrElse("")) => own(ref).getOrElse(t)
        case ((t, _), _)                                                            => t
      }

    val written = if impl.tparams.isEmpty then read() else sandboxed(read())

    val subject = sandboxed(resolveType(impl.forType, declared))

    // What the block leaves out the trait supplies, and `Self` in one of those defaults is the type
    // this block implements the trait for — which is what makes `impl Mul for Point` the
    // `impl Mul[Point] for Point` it reads as.
    val args =
      if written.length == tr.tparams.length then written
      else withDefaults(impl.traitName, tr.tparams, tr.tdefaults, written, selfBinding(subject))

    // Asked of what the block **wrote**, not of what the defaults filled in. A conditional block's
    // subject is the type applied to its own parameters, so a default of `Self` names them by
    // construction — `impl[T: Named] Pairable for Box[T]` supplies `Pairable[Box[T]]`, which is one
    // implementation and not a family of them, and only an argument the author left open is.
    for tp <- impl.tparams if tr.tparams.contains(tp) do
      err(s"trait '${qn(impl.traitName)}' already declares a type parameter '$tp', so this 'impl' " +
        "cannot declare one of that name")

    deferredBounds(impl.traitName, tr.tparams, tr.bounds, args)

    (Type.Bound(impl.traitName, args), written)
  }

  /** What a signature written inside these members resolves under.
   *
   * A concrete `impl` binds only `Self`, to the one type it is for. A generic one binds its own
   * parameters to themselves — the opaque stand-in of `reference/generics.md § Bounds` — and `Self`
   * to the type applied to them, so `-> Self` and `-> Box[T]` are the one signature conformance
   * compares, exactly as `-> Self` and `-> Point` are on a concrete implementation.
   *
   * The trait's own parameters are bound either way, since the block fixed them: a method written in
   * the trait's `T` and one written in the type that `T` is are the same signature.
   */
  protected def signatures(home: MemberHome): Map[String, Type] =
    if home.tparams.isEmpty then home.outer ++ home.self
    else
      val abstracts = abstractSubst(home.tparams, home.bounds, home.tvalues, home.tpacks)

      settledOuter(home.outer, abstracts) ++ abstracts + (selfName -> resolveType(home.selfRef, abstracts))

  /** The type an `impl` is for, and where its members belong.
   *
   * A trait may be implemented for **any** type it makes sense to name: built-ins included — a
   * language whose `Show` cannot cover `int` has a `Show` no library can use — the composed types
   * too, so `impl Display for []int` says how a slice of ints renders, a **generic** struct or enum
   * as a whole, which is what an `impl` with type parameters of its own is for, and a composed
   * **shape**, which is that same block written for a type that has no name to be generic over. A
   * struct or an enum brings its own fields or variants for a member to collide with; nothing else
   * has any.
   *
   * Two shapes are refused outright, each because an `impl` for it would be about nothing. A
   * **memory mode** is a way of holding a type rather than a type, and a member call already sees
   * through one. A **trait object** has forgotten which type it holds, which is the one thing an
   * `impl` is written about.
   */
  protected def implTarget(impl: ImplDecl): (Type, MemberHome) = {
    val ref = impl.forType

    ref match
      // A name declaring a struct or an enum is the one shape that may be generic, so it is settled
      // here rather than by resolving a reference whose arguments are the block's own parameters.
      // Its key is the name it was declared under, which stands even where the declaration itself
      // failed to resolve and there is no instantiation to read one off.
      case NamedType(written, argRefs) if typeKey(written).exists(k => nominal(k).isDefined) =>
        val key                    = typeKey(written).get
        val (tparams, taken, noun) = nominal(key).get

        if tparams.isEmpty then
          if impl.tparams.nonEmpty then
            err(s"'$written' takes no type arguments, so an 'impl' for it has nothing to be generic over")
          if argRefs.nonEmpty then err(s"type '$written' does not take type arguments")

          val ty = concrete(key).getOrElse(Type.Unknown)

          (ty, MemberHome(key, qn(key), key, None, ref, Nil, Map.empty, taken, noun, selfBinding(ty)))
        else
          val order = implArgs(impl, written, tparams)

          // A generic type has no one instantiation to be, and nothing that reaches this needs one:
          // an implementation covers every `Box` at once, and each member is made real per receiver.
          //
          // Which of the block's parameters stand for **values** travels with them, exactly as it
          // does for a block matching a shape. Without it the definition-time pass walks the members
          // with the name of a value parameter standing for nothing at all — `impl[const M: Mode]
          // Display for Run[M]` reading `M` in a body — and the complaint it makes was invisible
          // only because that pass used to drop everything it said about a name.
          (Type.Unknown,
           MemberHome(key, qn(key), key, None, ref, order, impl.bounds, taken, noun, Map.empty,
             tvalues = impl.tvalues, tpacks = impl.tpacks))

      case NamedType(n, Nil) if n == selfName =>
        err("'Self' is the type an 'impl' is for, so it cannot also be the type it names")
      case NamedType(n, Nil) if n == neverName =>
        err("'never' has no values, so nothing can be implemented for it")
      // Both valueless types are named here rather than left to `resolveType`, which refuses them
      // for standing anywhere but a result — true, and beside the point when what the block asks
      // is whether the type can behave.
      case NamedType(n, Nil) if n == unitName =>
        err("'unit' has one value and no behaviour — a trait for it would say nothing")

      case _ =>
        // The block's parameters stand in for themselves while the subject is resolved, so a shape
        // written with one comes back as the shape it is — `[]T` as a slice of something — rather
        // than through a complaint about a name that means nothing outside this block.
        val abstracts = abstractSubst(impl.tparams, impl.bounds, impl.tvalues, impl.tpacks)

        // Resolved rather than taken as written, so the key is the type's one canonical name and
        // two spellings of one type are one implementation.
        val ty = resolveType(ref, abstracts)

        ty match
          case t if Type.erased(t) =>
            err(s"'${show(t)}' has forgotten which type it holds, and an 'impl' says how one " +
              "particular type behaves — so it is written for that type, not for an object over it")
          case Type.Ptr(inner)       => err(modeIsNotAType("*", inner))
          case Type.Ref(inner, sync) => err(modeIsNotAType(if sync then "&sync " else "&", inner))
          // The third mode, refused for the same reason with a sharper consequence: a member call
          // reaches through a `*T` and a `&T`, and through a `weak T` it reaches nothing at all, so
          // a member written here would have no way to be called.
          case Type.Weak(inner) =>
            err(s"'weak ${show(inner)}' is a way of holding a ${show(inner)} rather than a type of " +
              s"its own — and nothing goes through one but 'get()', so a member written here could " +
              s"never be called. Write the 'impl' for ${show(inner)}, which 'get()' hands back")
          case Type.VaList => err("a va_list is an ABI primitive, not something to implement a trait for")
          // A **blanket** block, whose subject is the block's own parameter: it covers every type
          // that meets the parameter's bound. Allowed only where that bound is one the compiler
          // closes (`CoreTraits.closed`), which is what keeps the set of covered types fixed — and
          // so what lets one block stand for all of it without any type being able to join later.
          case a: Type.Abstract if closedBound(a).isDefined => ()
          // Any other parameter stands for any type at all, so the block would be saying how every
          // type behaves, which is what a trait's own defaults are for.
          case a: Type.Abstract =>
            err(s"'${a.name}' is a type parameter of this 'impl', so it stands for every type at " +
              s"once — an 'impl' says how one kind of type behaves. A bound the compiler closes is " +
              s"the exception, since it names a family rather than every type")
          case _ =>

        val blanket = ty match
          case a: Type.Abstract => closedBound(a)
          case _                => None

        val head = blanket.map(blanketKey).orElse(implShape(impl, ty))

        // Recorded where it is decided, since this is the only place that knows the block was a
        // blanket: the key alone cannot be read back out of the table it is filed in.
        for b <- blanket do blanketBounds(blanketKey(b)) = b

        if impl.tparams.isEmpty then
          (ty,
           MemberHome(ownerKey(ty), show(ty), Type.memberSymbol(ty), head, ref, Nil, Map.empty, Set.empty, "field",
             selfBinding(ty)))
        else
          val shape = head.getOrElse(notGeneric(ref))
          val order = blanket.fold(shapeArgs(impl, ty, shape))(_ => blanketArgs(impl, ty))

          // Like a generic type's, the members are made real per receiver — so there is no one
          // instantiation to be, and the shape rather than any type of it is what they are filed
          // under. The symbol drops the arguments the same way: `slice.show` instantiated at `int`
          // is `slice.show.int`, which the written `[]int`'s `slice.int.show` cannot collide with.
          (Type.Unknown,
           MemberHome(shape, ref.show, blanket.fold(shapeSymbol(ty, shape))(blanketSymbol), head, ref, order,
             impl.bounds, Set.empty, "field", Map.empty, tvalues = impl.tvalues, tpacks = impl.tpacks))
  }

  /** The **closed** bound a type parameter carries, where it carries one — which is what makes a
   * block written for the bare parameter a blanket rather than the refusal above.
   *
   * A parameter may be bounded by more than one trait, and only the closed one names the family the
   * block covers; the rest are what its body may assume, exactly as on any other generic. So this
   * finds rather than requires, and a parameter with two closed bounds is refused where the second
   * would have widened what the block stands for.
   */
  protected def closedBound(a: Type.Abstract): Option[String] = {
    val closed = a.bounds.map(_.name).filter(n => Library.spelling(n).exists(CoreTraits.closed))

    if closed.length > 1 then
      err(s"'${a.name}' is bounded by ${conjoin(closed.map(n => s"'${qn(n)}'"))}, and each of those " +
        s"names a family — so it is not one family this 'impl' covers, and there is no type that is " +
        s"in both")

    closed.headOption
  }

  /** The symbol a blanket's members are emitted under, which the instantiation's own arguments are
   * appended to — `bound.sysl$Integer.display.int`, arrived at the way `slice.int` is.
   *
   * It has to be spelled out rather than left to `shapeSymbol`, whose fallback mangles the subject:
   * a type parameter mangles as **its own name**, so two blankets would both be emitted under `T`
   * and collide with each other for no reason a reader could see.
   *
   * The bound is named by its **key** rather than its spelling, which is what every other emitted
   * name does and is load-bearing for the same reason: nothing about the symbol should change if
   * the trait moves between the library's files, and nothing a program declares should be able to
   * land on it.
   */
  protected def blanketSymbol(boundTrait: String): String = s"bound.$boundTrait"

  /** A blanket's one type parameter, which is the subject itself.
   *
   * `shapeArgs`' rule and `implArgs`' rule, at the one place where the type applies exactly one
   * argument: every parameter the block declares must be fixed by the subject, and a blanket's
   * subject is a single parameter — so a block declaring a second has one nothing would ever fix.
   */
  protected def blanketArgs(impl: ImplDecl, ty: Type): List[String] = {
    val name = ty.asInstanceOf[Type.Abstract].name

    if impl.tparams != List(name) then
      err(s"'${(impl.tparams.toSet - name).mkString("', '")}' is declared by this 'impl' but does " +
        s"not appear in '${impl.forType.show}', so nothing would ever fix it")

    impl.tparams
  }

  /** Where an `impl` may be written (`reference/traits.md § Where an impl may live`): **the module
   * that declares the trait, or one that declares a type the block names** — in the subject or among
   * the trait's own arguments — and nowhere else.
   *
   * An `impl` is unnamed, so resolving a bound means *searching* for one — and this is the rule that
   * bounds the search to two modules, both of which anything naming the trait and the type already
   * depends on. What it forbids is the case with no home, a foreign trait for a foreign type, where
   * two unrelated modules could each supply a different implementation and no rule picks one.
   *
   * **The arguments count as of card `0385`, and until then the rule looked at the subject alone.**
   * A trait that takes arguments is a family of promises, and a block naming a local type in one of
   * them is as anchored as one naming it in the subject — the module that declares `Point` is the
   * only module that would ever write `impl Mul[Point, Point] for real`, and it is exactly the block
   * a scalar on the *left* of an operator needs. Refusing it made `z * 2.0` legal and `2.0 * z` a
   * thing nobody could write anywhere, including in `sysl.math.complex`, which is the library's own
   * module for the type it is about. Rust widened the same rule for the same reason and to the same
   * shape (RFC 2451), which is how `num-complex` has `2.0 * z`.
   *
   * **What still has no home is unchanged, and that is what says coherence was not switched off.**
   * A block naming nothing local anywhere — `impl Mul[real, real] for real` in a program — is
   * refused exactly as before, and so is one whose only local name is the trait's, since the trait's
   * *declarer* was always the other way in. Rust's further restriction on uncovered parameters has
   * nothing to bite on here: an `impl` for a bare type parameter is already refused outright unless
   * the parameter carries a bound the compiler closes, so the blanket that restriction exists to
   * forbid cannot be written in the first place.
   *
   * The library is a module of its own for this purpose even though its declarations are keyed under
   * the root like a rootless program's, so which file a declaration came from is what decides rather
   * than the key — `Stdlib.owns`. A program at the project root is therefore as foreign to `Eq` as
   * any named module is.
   */
  protected def checkCoherence(impl: ImplDecl, label: String): Unit = {
    val home     = if libraryOwns(impl, currentModule) then None else Some(currentModule)
    val declarer = declaringModule(impl.traitName)
    val subject  = subjectHomes(impl.forType) ++ impl.traitArgs.flatMap(subjectHomes)

    if home != declarer && !subject(home) then
      err(s"an 'impl' may be written only in the module that declares the trait or in one that " +
        s"declares a type the block names, and '${qn(impl.traitName)}' ${whose(declarer)} " +
        s"while ${subjectPhrase(subject, label, impl.traitArgs.nonEmpty)} — so this one has no home. A trait of your own, or " +
        "a type of your own in what it is written for or in the trait's arguments, gives it one")
  }

  /** What the block names, and where those names live.
   *
   * **It names the subject where the block wrote no trait arguments, and the block where it did**,
   * which is card `0385`'s widening carried into the message. A trait taking no arguments has
   * nothing but the subject to name, so "nothing in `[]int` is declared outside the library" is the
   * whole truth and is the more useful half of it — the reader is looking at that type. A trait
   * that *does* take arguments has two places a local name could have gone, and a phrase naming
   * only the `for` type would send a reader to the one the fix is less likely to be in.
   */
  protected def subjectPhrase(homes: Set[Option[String]], label: String, args: Boolean): String = {
    val what = if args then "this block names" else s"in '$label'"

    if homes == Set(None) then s"nothing $what is declared outside the library"
    else if args then s"it names only what ${homes.toList.map(whose).sorted.mkString(" and ")}"
    else s"'$label' names only what ${homes.toList.map(whose).sorted.mkString(" and ")}"
  }

  /** Every module the **subject** of an `impl` belongs to: its own where it is a declared type, and
   * every one its parts belong to where it is composed (`reference/traits.md § Where an impl may
   * live`).
   *
   * A composed type is the module's when anything named in it is — `[]Point` belongs where `Point`
   * does, `[]int` to nobody. Without that a module could not so much as print a slice of its own
   * struct, and the compiler's own advice ("write an `impl Display for []Point`") would name a block
   * the rule then refused.
   *
   * It reads the subject **as written** rather than as resolved, because a generic subject has no
   * one instantiation to be: `implTarget` answers `Unknown` for `Box[T]`, and the arguments — which
   * are exactly what may carry a local type — would be lost with it.
   */
  protected def subjectHomes(ref: TypeRef): Set[Option[String]] = ref match
    case NamedType(written, args) =>
      Set(quietly(typeKey(written)).flatMap(declaringModule)) ++ args.flatMap(subjectHomes)
    case PtrType(inner)      => subjectHomes(inner)
    case RefType(inner, _)   => subjectHomes(inner)
    case WeakType(inner)     => subjectHomes(inner)
    case ArrayType(_, elem, _) => subjectHomes(elem)
    case VectorType(_, elem)   => subjectHomes(elem)
    // A value names no type, so it declares nothing and gives a block no home. `Buf[4]` is this
    // module's for whatever reason `Buf` is, and the `4` adds nothing either way.
    case _: ValueArgType     => Set(None)
    case VolatileType(inner) => subjectHomes(inner)
    case TupleType(parts, _) => Set(None) ++ parts.flatMap(subjectHomes)
    // A pack is the block's own parameter, which is not a local type and so is no home — the same
    // answer `impl[T: Display] Display for []T` gets for its `T` (`reference/traits.md § Where an
    // impl may live`).
    case _: PackType         => Set(None)
    case f: FnType           => subjectHomes(f.asTrait)
    // A function pointer belongs to no module — its parts may, so they are what is asked.
    case CFnType(ps, r)      => Set(None) ++ (ps :+ r).flatMap(subjectHomes)
    // A projection's home is its subject's: `T::Item` is written wherever `T` is, and the type it
    // names belongs to whoever implemented the trait rather than to whoever wrote this.
    case AssocType(base, _)  => subjectHomes(base)
    // An object's binding names a type like any other, and that type is where it lives.
    case AssocArgType(_, t)  => subjectHomes(t)
    // A `some` result is never an `impl`'s subject — it stands in a member's result.
    case _: SomeType         => Set(None)

  /** A resolution made only to ask where a name lives, which is not the place a name that resolves
   * to nothing is worth reporting from — a scalar and a block's own type parameter both answer
   * `None` here, and a name the file may not reach was reported where the subject was resolved.
   */
  protected def quietly(lookup: => Option[String]): Option[String] =
    try lookup
    catch
      case _: AnalyzerError => None
      case _: Poisoned      => None

  /** What a diagnostic says about where a declaration lives. The library's own answers "the
   * library's" rather than naming the mechanism that supplies it — that is a fact about the
   * compiler, and a reader told it goes looking for something they cannot write. The library is a
   * module they *can* name and import, so that is what a diagnostic offers them.
   */
  protected def whose(module: Option[String]): String = module match
    case None                         => "is the library's"
    case Some(m) if m == Modules.root => "is declared at the project root"
    case Some(m)                      => s"is declared in module '$m'"

  /** What a diagnostic calls every type of a shape at once. */
  protected def everyShape(head: String): String =
    if head == "[]" then "every slice"
    // Every array at *any* length, which is the block whose length is a value parameter — as against
    // `[3]`, every array of three, which the branch at the end of this reads off the key.
    else if head == Type.Array.shape then "every array"
    // A blanket covers a family the compiler names, and the family's own name is what a reader
    // knows it by — there is no shape to describe, so the bound is the description.
    else if head.startsWith("@bound:") then s"every '${qn(head.drop("@bound:".length))}'"
    // Every tuple at *any* arity, which is the block whose parts are a pack — as against `(,)`,
    // every pair, which the branch after this reads off the key (`reference/generics.md § A
    // parameter may stand for a list of types`).
    else if head == Type.Tuple.pack then "every tuple"
    else if head.startsWith("(") then s"every tuple of ${head.count(_ == ',') + 1} parts"
    else s"every array of ${head.drop(1).dropRight(1)}"

  /** The symbol a shape's members are emitted under, which is the mangling of the types it covers
   * with the arguments left off — so an instantiation appends them and arrives back where the type
   * written out in full would have started.
   */
  protected def shapeSymbol(t: Type, shape: String): String = t match
    case _: Type.Slice => "slice"
    // A block covering every length leaves the length off with the element type, since both are
    // arguments an instantiation appends: `arr.c3.int.display` is where a `[3]int` arrives.
    case _: Type.Array if shape == Type.Array.shape => "arr"
    case Type.Array(n, _)                           => s"arr$n"
    // A block covering every arity leaves the arity off with the parts, since the pack is the one
    // argument an instantiation appends: `tuple.pk2.int.string.display` is where a pair arrives.
    case _: Type.Tuple if shape == Type.Tuple.pack  => "tuple"
    case t: Type.Tuple                              => s"tuple${t.targs.length}"
    case other                                      => Type.mangle(other)

  /** Why a block declaring type parameters has nothing to apply them to: the subject is a type that
   * takes no arguments and has no shape either, so there is nothing for the parameters to fix.
   *
   * A composed type does have a shape, and matching it is what `shapeArgs` checks instead. What is
   * left here is a name — and by the time one reaches this it has already resolved, so a type that
   * does not exist or a trait was reported as itself rather than through a complaint about the
   * parameters, which would send the reader looking at the one part of the line written correctly.
   */
  protected def notGeneric(ref: TypeRef): Nothing = ref match
    case NamedType(n, _) =>
      err(s"'$n' takes no type arguments, so an 'impl' for it has nothing to be generic over")
    case _ =>
      err(s"'${ref.show}' has no shape for an 'impl' to match — a slice and an array do, and a " +
        "generic struct or enum has a name of its own to be generic over")

  /** The block's type parameters **in the order the implementing type applies them**, which is what
   * lets a member be instantiated from a receiver's own type arguments by position.
   *
   * An `impl` covers a generic type as a whole, so its subject must be that type applied to the
   * block's parameters and nothing else: each argument one parameter, each parameter used once, all
   * of them spoken for. Anything narrower — `Box[int]`, `Pair[T, int]` — is an implementation for
   * *some* instantiations, which is a second implementation for a key that holds one.
   */
  protected def implArgs(impl: ImplDecl, written: String, tparams: List[String]): List[String] = {
    val declared = impl.tparams.toSet
    val args     = impl.forType match
      case NamedType(_, as) => as
      case _                => Nil
    // `qn`, because the advice is meant to be typed. A library trait's internal name carries the
    // module separator it is stored under — `sysl$Mul` — and a reader copying that gets a name the
    // lexer cannot read, which is worse than no advice at all.
    val spelled = s"impl[${tparams.mkString(", ")}] ${qn(impl.traitName)} for $written[${tparams.mkString(", ")}]"

    if impl.tparams.isEmpty then
      err(s"'$written' is generic, so an 'impl' for it covers every instantiation at once — " +
        s"write '$spelled'")
    if args.length != tparams.length then
      err(s"'$written' takes ${quantity(tparams.length, "type argument")}, but " +
        s"${supplied(args.length, "type argument")}")

    val names = args.map {
      case NamedType(n, Nil) if declared(n) => n
      case other =>
        err(s"'${other.show}' fixes an argument of '$written' to one type, and an 'impl' covers " +
          s"the whole of a generic type — write one of the block's own parameters here")
    }

    if names.distinct.length != names.length then
      err(s"each argument of '$written' takes a type parameter of its own, and this 'impl' " +
        s"names '${names.diff(names.distinct).head}' twice")
    if declared != names.toSet then
      err(s"'${(declared -- names).mkString("', '")}' is declared by this 'impl' but does not " +
        s"appear in '${impl.forType.show}', so nothing would ever fix it")

    names
  }

  /** The shape key **this block** is filed under, which the subject as *written* decides rather than
   * the type it resolved to.
   *
   * The two part company for exactly one subject, and that is the whole reason this is not simply
   * `shapeOwners(ty).head`: a value parameter stands at zero for the walk that checks the block's
   * body (`reference/generics.md § A parameter may stand for a value`), so `[N]T` and `[0]T`
   * resolve to the same array and only the syntax says which was written. A length that is one of
   * the block's own value parameters covers every array; every other length covers the arrays of
   * that one length, under the per-length key it has always had.
   */
  protected def implShape(impl: ImplDecl, ty: Type): Option[String] =
    if lengthParam(impl).isDefined then Some(Type.Array.shape)
    // `(..A)` covers every tuple at every arity, and the subject it resolved to is an ordinary pair
    // — a pack stands at two types for the walk that checks the body (`reference/generics.md § A
    // parameter may stand for a list of types`) — so only the syntax says which was written.
    // Exactly the reason a length gets the branch above.
    else if packParam(impl).isDefined then Some(Type.Tuple.pack)
    else shapeOwners(ty).headOption.map(_._1)

  /** The block's own **pack** parameter, where the subject is `(..A)` and `A` is one — which is what
   * makes the block cover every arity rather than the two its stand-ins happen to resolve to.
   *
   * It asks `tpacks` rather than `tparams`, so a plain type parameter written as `(..T)` is not
   * quietly read as this; that subject is refused by the pack's own resolution, which is where a
   * name that stands for one type standing where a list belongs is worth reporting.
   */
  protected def packParam(impl: ImplDecl): Option[String] = impl.forType match
    case TupleType(List(PackType(n)), _) if impl.tpacks.contains(n) => Some(n)
    case _                                                          => None

  /** The block's own **value** parameter standing for an array's length, where the subject is an
   * array whose length was written as one — which is what makes the block cover every length rather
   * than the one it named.
   *
   * It asks `tvalues` rather than `tparams`, so a *type* parameter written where a length belongs is
   * not quietly read as this. That subject is refused by the length's own resolution, which is where
   * a type standing in an expression position is worth reporting.
   */
  protected def lengthParam(impl: ImplDecl): Option[String] = impl.forType match
    case ArrayType(Some(Ident(n)), _, _) if impl.tvalues.contains(n) => Some(n)
    case _                                                           => None

  /** The same, for a **shape**: the block's parameters in the order the shape applies them, which
   * for a slice is the one element type and for an array covering every length is the length and
   * then the element.
   *
   * The rule is `implArgs`' rule, and it is the same rule because the reason is the same. A shape is
   * one key, so a block that fixed part of what it matched would be implementing the trait for
   * *some* of the types the shape covers — a second implementation for a key that holds one.
   */
  protected def shapeArgs(impl: ImplDecl, ty: Type, shape: String): List[String] = {
    val declared = impl.tparams.toSet
    // A block covering every tuple applies **one** argument, the pack, and reads it off what was
    // written for the reason a length is read that way: the pack stands at two types for this walk,
    // so the subject is a pair and no longer knows which parameter made it one
    // (`reference/generics.md § A parameter may stand for a list of types`).
    val matched  =
      if packParam(impl).isDefined then Nil
      else
        shapeOwners(ty).head._2.map {
          case Type.Abstract(n, _) if declared(n) => n
          case other =>
            err(s"'${show(other)}' fixes the element type, and an 'impl' with type parameters covers " +
              s"${everyShape(shape)} — write one of the block's own parameters here")
        }

    // A block covering every array applies **two** arguments, the length before the element, and the
    // length is read from what was written rather than from what it resolved to: a value parameter
    // stands at zero for this walk, so the subject no longer knows which parameter put it there.
    val names = lengthParam(impl).toList ::: packParam(impl).toList ::: matched

    if declared != names.toSet then
      err(s"'${(declared -- names).mkString("', '")}' is declared by this 'impl' but does not " +
        s"appear in '${impl.forType.show}', so nothing would ever fix it")

    names
  }

  protected def modeIsNotAType(sigil: String, inner: Type): String =
    s"'$sigil${show(inner)}' is a way of holding a ${show(inner)} rather than a type of its own — " +
      s"write the 'impl' for ${show(inner)}, which a member call reaches through one '${sigil.trim}' to find"
}
