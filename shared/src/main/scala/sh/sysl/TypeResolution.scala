package sh.sysl

import scala.collection.mutable

/** Resolving a `TypeRef` to a `Type`, instantiating generic structs and enums on demand, and the
 * generics machinery that solves a declaration's type arguments from a call's argument types.
 *
 * A named type is memoized on its display name, and an instantiation is registered *before* its
 * fields are resolved, so a type that reaches itself through a `*T` / `&T` / `[]T` finds the
 * in-progress object and the recursion terminates; `cycleCheck` is what rejects a cycle that has
 * no indirection to break it.
 */
trait TypeResolution extends GenericInstantiation, Aliasing, WrittenTypes, ConstrainedTypes {

  /** Resolves a type in the current function's substitution — the identity map outside a
   * generic instantiation.
   */
  protected def rt(t: TypeRef): Type = resolveType(t, tsubst)

  /** Resolves a type reference under `subst`, which maps the enclosing declaration's type
   * parameters to the arguments it was instantiated with.
   */
  protected def resolveType(t: TypeRef, subst: Map[String, Type]): Type =
    at(t.pos)(resolveTypeAt(t, subst))

  /** The name of the bottom type. It is a predeclared identifier rather than a reserved word, like
   * every other type name, and it is deliberately *not* one of the scalars: there is no value of
   * it, so it is not something a variable, a field, or a type argument can be.
   */
  protected val neverName = "never"

  /** The name of the type an expression run only for its effect has. It *is* a scalar, because a
   * function may declare it as its result and a block may have it, but it is no more a layout than
   * `never` is: its one value occupies nothing, so there is nothing for a parameter, a field, an
   * element, or a type argument to hold.
   */
  protected val unitName = "unit"

  /** The name of the type a trait is being implemented for (`reference/traits.md § Declaring a
   * trait`).
   *
   * It is a substitution key rather than a type of its own: a trait declaration and an `impl` are
   * both resolved with `Self` bound to the implementing type, so `add(self, rhs: Self) -> Self` and
   * `add(self, rhs: Point) -> Point` are the one signature conformance compares. Outside those two
   * places it is bound to nothing and resolving it says so.
   *
   * Distinct from the lowercase receiver `self`, which is the *value*; this is its type.
   */
  protected val selfName = "Self"

  /** The substitution that gives `Self` its meaning, for the one type it currently stands for. */
  protected def selfBinding(t: Type): Map[String, Type] = Map(selfName -> t)

  /** Whether every declared type's fields are in, which is when a `&sync T` can be held to what
   * `06` asks of `T`. It is false while the file is still being hoisted for the reason
   * `implsHoisted` is: a type that reaches itself through a `&sync` field is being resolved at the
   * moment its own field list is still empty, so asking then would find nothing in the way of
   * anything.
   */
  protected var typesHoisted: Boolean = false

  /** The pointees of `&sync T` types written before that question could be answered, each with the
   * position to report against. Drained once, as soon as the answer is available.
   */
  protected val sharedChecks = mutable.ListBuffer.empty[(Type, Option[Pos])]

  /** Holds a `&sync T` to `06`'s rule about what `T` may contain — now if the types are all in, and
   * after hoisting if they are not.
   */
  protected def checkShared(inner: Type): Unit =
    if typesHoisted then Sharing.complaint(inner).foreach(err)
    else sharedChecks += ((inner, currentPos))

  /** `Self` standing in for itself, for the checks a trait's own declaration can run before any type
   * has implemented it. A requirement's arguments have to resolve for the declaration to be checked
   * at all, and here the implementing type is precisely what is not yet known — so it is treated as
   * the one unknown type it is, which is enough to compare two requirements of the same trait.
   */
  protected def abstractSelf: Type = Type.Abstract(selfName, Nil)

  /** `subst`, extended with whatever `Self` means inside `fname` — which is a question only a
   * member of a *generic* type leaves open.
   *
   * A concrete type's members had their `Self` resolved at hoist. A generic type's could not be:
   * `Box[T]` is not a type until `T` is one, so what was kept is the reference, and resolving it
   * under the very substitution that fixed the parameters is what fixes `Self` alongside them.
   * Both places that build such a substitution — an instantiation and the definition-time pass —
   * come through here, so `Self` means the same thing in a checked body and in a run one.
   */
  protected def withSelf(fname: String, subst: Map[String, Type]): Map[String, Type] =
    // A trait's **value** parameter fixed by one of the block's own — `impl[const M: usize]
    // Bytes[M] for Bits[M]` — is whatever that parameter is here: zero for the walk that checks the
    // body, the subject's own `M` at an instantiation. A body reads it as a value only once it is one.
    settledOuter(genericOuter.getOrElse(fname, Map.empty), subst) ++
      genericSelf.get(fname).fold(subst) { (ref, scope) =>
        // Read where the subject was written, which for an inherited default is the `impl` block
        // rather than the trait the rest of the declaration came from. The substitution itself is
        // resolved types and means the same thing anywhere.
        subst + (selfName -> inScope(scope)(resolveType(ref, subst)))
      }

  /** A block's trait arguments with each of the block's own **value** parameters replaced by what
    * `subst` makes it — the stand-in it was filed under is a name, and a length has to be a value.
    */
  protected def settledOuter(outer: Map[String, Type], subst: Map[String, Type]): Map[String, Type] =
    outer.map {
      case (k, Type.Abstract(n, _)) if subst.get(n).exists(_.isInstanceOf[Type.ConstArg]) => k -> subst(n)
      case kv                                                                            => kv
    }

  /** Rewrites `Self` in a written type reference to the reference it stands for.
   *
   * `withSelf` above is the same idea one stage later, on resolved types; this one is needed where
   * a signature is read *before* anything is resolved — inferring a generic associated function's
   * type arguments, where `-> Self` has to be seen as the `Box[T]` it means for `T` to be solved
   * from the expected type. Everywhere else the resolved binding is enough.
   */
  protected def spellSelf(ref: TypeRef, selfRef: TypeRef): TypeRef = ref match
    case NamedType(n, Nil) if n == selfName => selfRef
    case NamedType(n, args)                 => NamedType(n, args.map(spellSelf(_, selfRef)))
    case PtrType(inner)                     => PtrType(spellSelf(inner, selfRef))
    case RefType(inner, sync)               => RefType(spellSelf(inner, selfRef), sync)
    case WeakType(inner)                    => WeakType(spellSelf(inner, selfRef))
    case ArrayType(len, elem, ro)           => ArrayType(len, spellSelf(elem, selfRef), ro)
    case VectorType(lanes, elem)            => VectorType(lanes, spellSelf(elem, selfRef))
    // A value argument names no type, so there is no `Self` in it to spell.
    case v: ValueArgType                    => v
    case VolatileType(inner)                => VolatileType(spellSelf(inner, selfRef))
    case TupleType(parts, r)                => TupleType(parts.map(spellSelf(_, selfRef)), r)
    // A pack names a parameter of the block it is declared by, which `Self` never is.
    case p: PackType                        => p
    case f: FnType =>
      f.copy(params = f.params.map(spellSelf(_, selfRef)), ret = spellSelf(f.ret, selfRef))
    case CFnType(params, ret) => CFnType(params.map(spellSelf(_, selfRef)), spellSelf(ret, selfRef))
    case AssocType(base, m)   => AssocType(spellSelf(base, selfRef), m)
    case AssocArgType(n, t)   => AssocArgType(n, spellSelf(t, selfRef))
    // A `some` result names no type, so there is no `Self` in it to spell.
    case s: SomeType          => s

  /** The same rewrite over a declaration's **bounds**, whose arguments are written types exactly as
   * its parameters are.
   *
   * A bound is where the arrow sugar puts what the author wrote in a parameter list
   * (`MemberLowering.callBounds`), so `f: Self::Item -> N` leaves `Self` in a bound and nowhere
   * else — and a call reads that bound to say what the closure standing there takes. Spelling the
   * parameters and leaving the bounds alone therefore looks complete and is not: a member inherited
   * from a trait's default keeps the word `Self`, deliberately, so that its signature goes on being
   * read in the trait's own module, and the one place a call could not put the implementing type
   * back was the half nothing rewrote.
   */
  protected def spellSelfBounds(
      bounds: Map[String, List[BoundRef]],
      spell: TypeRef => TypeRef,
  ): Map[String, List[BoundRef]] =
    bounds.map((tp, refs) => tp -> refs.map(b => BoundRef(b.name, b.args.map(spell)).setPos(b.pos)))

  protected def resolveBound(b: BoundRef, subst: Map[String, Type]): Type.Bound = at(b.pos) {
    val key     = traitKey(b.name)
    val written = traitArgs(key, b.args, subst)
    var args    = written

    for k <- key; decl <- traitDecls.get(k) do
      checkTraitArity(b.name, decl.tparams, decl.tdefaults, written)
      // What the bound leaves out the trait supplies, and `Self` in one of those defaults is
      // whatever is being asked to implement the trait — the parameter the bound belongs to, which
      // is what the substitution being resolved under already carries.
      args = withDefaults(k, decl.tparams, decl.tdefaults, written, subst.filter(_._1 == selfName))
      deferredBounds(k, decl.tparams, decl.bounds, args)

    // A bound naming no trait at all keeps the name as written, so `checkBoundNames` reports it in
    // the words it was spelled with rather than this producing a second complaint about a key.
    Type.Bound(key.getOrElse(b.name), args)
  }

  /** How many arguments a declaration has to be given: the parameters it declares, less the ones
   * carrying a default. A default stands in only for an argument that was not written, so the
   * parameters without one are exactly what a use is obliged to supply.
   *
   * The defaults are a suffix — `checkTypeDefaults` refuses any other arrangement at the
   * declaration — so a count is enough here and no position has to be reasoned about.
   */
  protected def leastArgs(tparams: List[String], tdefaults: Map[String, TypeRef]): Int =
    tparams.count(!tdefaults.contains(_))

  /** The declarations whose defaults are being filled right now, so one that leads back to itself is
    * caught rather than recursed into. The set is small and short-lived: a fill nests only where one
    * default names another generic declaration that has defaults of its own.
    */
  private val filling = mutable.Set.empty[String]


  /** A trait applied to the wrong number of arguments, said in the words a trait deserves — the
   * type-level message would send the reader looking for a struct of that name.
   */
  protected def checkTraitArity(
      name: String,
      tparams: List[String],
      tdefaults: Map[String, TypeRef],
      targs: List[Type],
  ): Unit =
    if targs.length > tparams.length || targs.length < leastArgs(tparams, tdefaults) then
      if tparams.isEmpty then err(s"trait '$name' does not take type arguments")
      else
        err(s"trait '$name' ${arityPhrase(tparams, tdefaults)}, " +
          s"but ${supplied(targs.length, "type argument")}")

  /** The arguments a generic declaration was applied to, with the ones it was not given taken from
   * the defaults it declares (`reference/generics.md § A parameter may carry a default`).
   *
   * They are filled left to right, each resolved under the arguments already fixed, so a later
   * default may name an earlier parameter and `[T, U = T]` means what it reads as. `self` carries
   * what `Self` stands for where there is such a thing — the implementing type at an `impl`, the
   * parameter itself at a bound — and is empty where there is not, which is what leaves a `Self`
   * default at a trait object reported rather than silently taken as something else.
   *
   * A default is written in the file that declares it, so it is resolved in **that** file's terms
   * rather than the applying one's: `[A = Heap]` names the `Heap` its author could see, whether or
   * not the program applying it can see one at all.
   */
  protected def withDefaults(
      key: String,
      tparams: List[String],
      tdefaults: Map[String, TypeRef],
      targs: List[Type],
      self: Map[String, Type],
  ): List[Type] =
    if targs.length >= tparams.length || tdefaults.isEmpty then targs
    else if !filling.add(key) then
      // A default that leads back to the declaration it belongs to would fill forever, since each
      // arrival applies the declaration to fewer arguments than it declares and so asks for the
      // defaults again. Reported at the use, because a chain of them is a property of the
      // declarations together rather than of any one of them.
      err(s"filling a type argument of '${qn(key)}' from its default leads back to '${qn(key)}' — a " +
        "default cannot stand in for a type that is still being worked out")
    else
      try
        inDecl(key) {
          val filled = mutable.ListBuffer.from(targs)
          // Where the caller knows what `Self` is it says so. Where it does not, the implementing
          // type is precisely what is not yet known, and the default stands for the one unknown type
          // it is — which is enough to compare two requirements of the same trait, and is how
          // `trait Word: Mul` resolves before anything implements `Word`. The one place that is not
          // good enough is erasure, and `traitObject` refuses it there in its own words.
          val here = (if self.contains(selfName) then self else self ++ selfBinding(abstractSelf))

          // A value parameter's default is the argument a use would have written there, folded
          // under the same arguments a type default is resolved under — so `[const M: usize,
          // const N: usize = M]` reads the `M` this use fixed.
          val values = nominalValues(key)

          for tp <- tparams.drop(targs.length) do
            val fixed = here ++ tparams.zip(filled)
            filled += tdefaults.get(tp).fold(Type.Unknown) { ref =>
              values.get(tp) match
                case Some(vt) =>
                  valueArg(ref, recover(Type.Unknown)(resolveType(vt, Map.empty)), fixed, s"the default for '$tp'")
                case None => resolveType(ref, fixed)
            }

          filled.toList
        }
      finally filling -= key

  /** A declaration's type parameters as its own body sees them: each standing in for itself, and
   * carrying what the declaration asked of it (`reference/generics.md § Bounds`).
   *
   * The bounds are resolved with the *siblings* standing in for themselves too, which is what lets
   * `f[T: Iter[U], U: Display]` know that what `T`'s iterator yields is something printable. A bound
   * that reaches back around to the parameter it belongs to is broken by dropping that parameter's
   * own bounds one level in — the promise stays, and only the walk stops.
   *
   * **It must be called in the terms of the declaration whose parameters these are** — under an
   * `inDecl` or an `inScope` for it — because a bound names a trait the way anything else does, and
   * what a short name means is what the *declaring* file imported. Read from anywhere else the bound
   * silently keeps the name as written, and the parameter goes on carrying a promise that no
   * conformance check can match against the same bound resolved properly.
   *
   * **`outer` is what the declaration these parameters belong to sits inside**, and a member's
   * bounds need it: a member declares its own parameters beside its *owner's*, so a bound written on
   * one of them may name the owner's — which is not an exotic case but the ordinary one, since
   * `map(self, f: T -> U)` on a `trait Mapping[T]` desugars to exactly that. Resolved without it the
   * bound reports the owner's parameter as an unknown type, blaming the trait for a scope the walk
   * had not been given. Empty for a declaration that sits inside nothing, which is every free
   * function and every type.
   */
  protected def abstractSubst(
      tparams: List[String],
      bounds: Map[String, List[BoundRef]],
      values: Map[String, TypeRef] = Map.empty,
      packs: Set[String] = Set.empty,
      outer: Map[String, Type] = Map.empty,
  ): Map[String, Type] = {
    def build(tp: String, seen: Set[String]): Type.Abstract = named(tp, tp, seen)

    /** The stand-in for one parameter, under a name that may differ from it — which is what a pack's
      * members need, since two of them stand for one parameter and two types that compared equal
      * would defeat the walk they exist for (`Abstract` is identified by its name).
      */
    def named(as: String, tp: String, seen: Set[String]): Type.Abstract =
      Type.Abstract(
        as,
        if seen(tp) then Nil
        else
          val refs = bounds.getOrElse(tp, Nil)
          // **Only the parameters this bound actually names.** Standing up the others is not merely
          // wasted work: building a sibling's stand-in resolves that sibling's *own* bounds, and it
          // does so with `tp` in `seen` — which is to say with `tp` stripped of its bounds, since
          // that is what breaks the walk back around. So a sibling bound naming `tp` was resolved
          // against a `tp` that promised nothing, and a projection off it (`tp::Item`) was refused
          // as though the parameter carried no bound at all. The refusal was collateral: it came
          // from building a stand-in the bound in hand never mentioned.
          val needed = tparams.filter(p => p == tp || refs.exists(_.args.exists(mentions(_, Set(p)))))
          val inner: Map[String, Type] = needed.map(p => p -> build(p, seen + tp)).toMap
          // A bound asks something of the parameter it is written on, so `Self` inside it — written
          // there, or arriving from a default — is that parameter. It is taken from `inner`, whose
          // entry for it is already the bound-free stand-in that breaks the walk back around.
          //
          // **A bound the arrow sugar added is the exception, and it is the whole of the exception.**
          // Its arguments were not written on it: they were written in a *parameter list*, where
          // `Self` is the receiver's type, and `callBounds` moved them here. Rebinding `Self` to the
          // synthesized parameter reinterprets what the author wrote — `f: Self::Item -> N` became a
          // projection off `$F1`, and the diagnostic named `$F1` as the thing lacking a bound.
          val here =
            if MemberLowering.isCallBound(tp) then outer ++ inner
            else outer ++ inner ++ selfBinding(inner(tp))
          refs.map(b => recorded(Type.Bound(b.name, Nil))(resolveBound(b, here))),
      )

    // A **value parameter** stands in as a zero rather than as an `Abstract`
    // (`reference/generics.md § A parameter may stand for a value`). It is not a type, so nothing
    // may ask what it implements — and standing at a value is what lets the one walk that checks
    // the generic body read `[N]T` as an array and `N` as a `usize` without a second mechanism.
    // Zero is the same placeholder `[sizeof(T)]u8` already resolves to for this walk, whose tree is
    // discarded; every real length is built per instantiation.
    //
    // A **pack** stands at **two** types (`reference/generics.md § A parameter may stand for a list
    // of types`), each carrying the pack's own bounds, because an unrolled loop has no fixed length
    // for this walk to run at. Two rather than one is what makes the walk worth doing: the body of
    // `for const` is one piece of source repeated, so a copy that checks at one position checks at
    // every position — and at two the *between* is covered as well, which is where a separator is
    // emitted and where a body that only works on the first part gives itself away. Two is also the
    // smallest tuple there is (`reference/types.md § Tuples`), so the shape being checked is one
    // that really exists.
    //
    // The members are named with a `#`, which no identifier may hold, so nothing a program writes
    // can collide with one and a diagnostic naming `A#0` is plainly the compiler's own stand-in.
    tparams.map(tp =>
      tp -> (
        if packs(tp) then Type.Pack(List(named(s"$tp#0", tp, Set.empty), named(s"$tp#1", tp, Set.empty)))
        else
          values.get(tp).fold[Type](build(tp, Set.empty))(tr =>
            Type.ConstArg(0, recover(Type.Unknown)(resolveType(tr, Map.empty))))
      )).toMap
  }

  /** Resolves a **result** type, which is the one position the two valueless types may appear in —
   * a function's, a member's, or an `extern`'s declared result, saying that it hands back nothing
   * (`unit`) or does not return at all (`never`). Everywhere else a type is resolved through
   * `resolveType`, which rejects both.
   */
  protected def resolveReturn(t: TypeRef, subst: Map[String, Type]): Type = t match
    case NamedType(n, Nil) if n == neverName && !subst.contains(n) => Type.Never
    case NamedType(n, Nil) if n == unitName && !subst.contains(n)  => Type.Unit
    // A result list is a property of *this* position and of no other, which is why it is read here
    // rather than by `resolveType` — one that reached anywhere else is a mistake, and says so.
    case TupleType(parts, true) => Type.Results(tupleType(parts.map(resolveType(_, subst))))
    case _                      => resolveType(t, subst)

  /** Resolves a type in one of the three positions a `volatile` qualifier may stand: the pointee of
   * a `*T`, an element, and a struct field (`reference/memory.md § Device memory`).
   *
   * What the three have in common is that the storage being named is **somebody else's** — a device's
   * registers, reached through an address the program was handed. Everywhere else a type is resolved
   * through `resolveType`, whose own `VolatileType` case says why the qualifier means nothing there.
   */
  protected def resolveQualified(t: TypeRef, subst: Map[String, Type]): Type = t match
    case VolatileType(inner) => at(t.pos)(Type.Volatile(volatileScalar(resolveType(inner, subst))))
    case other               => resolveType(other, subst)

  /** Holds what a `volatile` was written on to a scalar or a raw pointer.
   *
   * The qualifier says the compiler must emit exactly the accesses the source wrote, which is a
   * promise about **one** load and **one** store — so it is only meaningful where an access is one
   * instruction. An aggregate is qualified by qualifying the fields of it that are registers, which
   * is what every device header does and what lets a shadow field opt out; anything counted is
   * refused outright, since a retain that may not be elided is not a request anybody can act on.
   */
  private def volatileScalar(t: Type): Type = Type.underlying(t) match
    // A constrained subtype is the claim that a value **has been checked** (`reference/errors.md §
    // Where a constraint is checked`), and a register holds whatever the device put there. Reading
    // one at such a type would hand back that claim unchecked, through a field selection that looks
    // like any other — and the `ptr_cast` that made the pointer is too far away to read as the
    // licence for it. Declaring the register at the base and converting what comes back puts the
    // check where the value arrives, which is one written conversion and the whole of the fix.
    case _ if constrains(t) =>
      err(s"'volatile ${show(t)}' is not a type: a register holds whatever the device put in it, and " +
        s"${show(t)} is the claim that a value has been checked. Declare the register at " +
        s"${show(Type.underlying(t))} and convert what you read")
    case _: Type.Integer | _: Type.Floating | Type.Char | Type.Bool => t
    // A **simple** enum is its underlying integer and nothing else — `llvm` delegates to it — so
    // reading one is the single load the qualifier promises. It is also the spelling a mode field
    // wants (`reference/types.md § Structs`), which is the reason this is here rather than left to
    // the catch-all: refusing it would mean a register's mode had to be declared as the raw `u3`
    // the enum exists to name.
    case e: Type.Enum if e.simple => t
    // A **data** enum is a tag beside a payload, so touching one is as many accesses as the payload
    // has words however the source writes it — the one promise the qualifier makes, and the one it
    // could not keep here.
    case _: Type.Enum =>
      err(s"'volatile ${show(t)}' is not a type: '${show(t)}' carries a payload beside its tag, so " +
        "touching one is more than the single access the qualifier promises. A register field that " +
        "is a set of named values is a simple enum, which is one integer")
    // A trait object is two words, so an access to one is two accesses however it is written — which
    // is the one promise the qualifier makes, and the one it could not keep here. It is also nothing
    // a device could have put there: what the second word points at is a method table this compiler
    // emitted.
    case _ if Type.erased(t) =>
      err(s"'volatile ${show(t)}' is not a type: an object over a trait is a pair of words, so " +
        "touching one is two accesses whatever the source says — and the table beside the value is " +
        "this program's, not a device's")
    case _: Type.Ptr | _: Type.CFn                                  => t
    case _: Type.Ref | _: Type.Weak | _: Type.View =>
      err(s"'volatile ${show(t)}' is not a type: the qualifier says every access is emitted exactly " +
        "as written, and a counted value's accesses come with retains and releases that no rule " +
        "here could hold still — device memory is reached with a raw pointer")
    case _: Type.Struct =>
      err(s"'volatile ${show(t)}' is not a type: a register block is qualified one field at a time, " +
        s"so write 'volatile' on the fields of '${show(t)}' that are registers — which is also what " +
        "lets a shadow field in the middle of one stay ordinary")
    case _ =>
      err(s"'volatile ${show(t)}' is not a type: 'volatile' qualifies a scalar or a raw pointer, " +
        "since it promises the one load or the one store the source wrote and nothing else")

  /** Whether a type constrains which **values** it has, as against merely having an identity of its
   * own. A bare `new` derivation is nominal and asserts nothing about a value (`reference/errors.md
   * § new is what makes it a type`), so there is nothing for a register to arrive holding that the
   * type would have promised was checked.
   */
  private def constrains(t: Type): Boolean = t match
    case c: Type.Constrained => c.lo.isDefined || c.hi.isDefined || c.predFn.isDefined || constrains(c.base)
    case _                   => false

  /** Whether the type about to be resolved is what a `*` points at, rather than a value in its own
   * right. It is the one thing an `opaque` struct may be outside the module declaring it
   * (`reference/ffi.md § opaque`).
   *
   * Set for exactly **one** level and cleared the moment any resolution begins, which is what keeps
   * it honest under nesting: `*Outer` sets it, `Outer`'s own by-value field clears it before
   * resolving, so a layout reached through a pointer is not itself excused by that pointer. The
   * `indirection` counter next door answers a different question — whether a *cycle* is finite — and
   * is deliberately not reused, since it stays raised for the whole subtree.
   */
  private var pointee = false

  private def asPointee(resolve: => Type): Type = {
    pointee = true
    try resolve
    finally pointee = false
  }

  private def resolveTypeAt(t: TypeRef, subst: Map[String, Type]): Type = {
    val behindPointer = pointee
    pointee = false

    resolveShape(t, subst, behindPointer)
  }

  private def resolveShape(t: TypeRef, subst: Map[String, Type], behindPointer: Boolean): Type = t match
    case PtrType(inner) =>
      traitObject(inner, subst, "*")
        .fold(Type.Ptr(addressable(asPointee(underIndirection(resolveQualified(inner, subst))), "'*'")))(Type.Ptr.apply)

    // Everywhere but a field, an element, and a pointee, what is being named is a **value** — what a
    // binding holds, what a parameter receives, what a call hands back — and a value read out of a
    // volatile place is an ordinary value. There is nothing left for the qualifier to say by then,
    // so writing it there would read as a promise about accesses that have already happened.
    case VolatileType(inner) =>
      err(s"'volatile ${inner.show}' is the type of *storage*, and this is a value — what a read of a " +
        s"volatile place hands back is an ordinary '${inner.show}'. The qualifier goes where the " +
        s"storage is named: a struct field, an element, or the pointee of a '*T', as " +
        s"'*volatile ${inner.show}'")
    // An atomic reference promises that a second domain may hold the object, which is a promise
    // about everything the object holds (`reference/memory.md § Crossing a concurrency domain`). A
    // trait object is the one shape whose contents are not known here — the type it forgot is
    // settled where a value is erased into one, and that is where it is asked.
    case RefType(inner, sync) =>
      val t = traitObject(inner, subst, "&")
        .fold(Type.Ref(addressable(underIndirection(resolveType(inner, subst)), "'&'"), sync))(Type.Ref(_, sync))

      if sync && !t.inner.isInstanceOf[Type.Trait] then checkShared(t.inner)

      t

    case WeakType(inner) =>
      traitObject(inner, subst, "weak")
        .fold(Type.Weak(addressable(underIndirection(resolveType(inner, subst)), "'weak'")))(Type.Weak.apply)

    // An array holds its elements, so it is no indirection at all and a type cannot contain an
    // array of itself. A slice only points at them, so it breaks a cycle exactly as `*T` does.
    case ArrayType(None, elem, ro) =>
      Type.Slice(addressable(underIndirection(resolveQualified(elem, subst)), "a slice"), readOnly = ro)
    // A bound is a compile-time constant, which a `const` is and a call is not (`reference/modules.md § const — a value`).
    //
    // **The substitution reaches the bound as well as the element**, so a length may measure the
    // block's own type parameter: `[sizeof(T) * 3 + 1]u8` is a buffer sized for whatever `T` turns
    // out to be, which is what a generic renderer needs and what no fixed number can express.
    //
    // A bound over a parameter has no value during the walk that checks the generic body itself,
    // because there is no `T` yet — and that is not a mistake to report, since the body is analyzed
    // again for each instantiation with the parameter bound to a real type. The array stands at
    // length zero for that one walk; every array the program actually gets is built by the later
    // pass, where the measurement answers.
    case ArrayType(Some(len), elem, _) =>
      checkLengthArithmetic(len, subst)
      checkLengthNotAType(len, subst)

      val n = constInt(len, subst) match
        case Some(v) if v >= 0 && v.isValidInt => v.toInt
        case Some(v)                           => err(s"an array cannot have $v elements")
        case None if awaitsInstantiation(len, subst) => 0
        case None => err("an array length must be a constant — a literal, or a 'const' naming one")
      Type.Array(n, addressable(resolveQualified(elem, subst), "an array"))

    // `<N>T` — a vector. The lane count is a compile-time constant on the same terms an array's
    // length is, and reaches the same substitution, so a kernel generic over its width writes
    // `<W>f32` against its own `[const W: usize]` and is instantiated once per width.
    case VectorType(lanes, elem) =>
      checkLengthArithmetic(lanes, subst)
      checkLengthNotAType(lanes, subst)

      val resolved = resolveQualified(elem, subst)

      // A lane is a scalar, and the reason is LLVM's rather than a choice: `<4 x {float, float}>`
      // is not a type, so an aggregate lane has no representation to lower to. The abstract case is
      // let through untouched — during the walk that checks a generic body there is no `T` yet, and
      // the instantiation walk is where a real type arrives to be held to this.
      if !Type.mentionsAbstract(resolved) && !Type.Vector.lanes(resolved) then
        err(s"a vector's lanes are scalars, and ${show(resolved)} is not one — an integer, a float, " +
          s"'bool' or 'char' may be a lane, and an aggregate has no lane-wise arithmetic to give")

      val n = constInt(lanes, subst) match
        case Some(v) if v > 0 && v.isValidInt => v.toInt
        case Some(v) if v == 0 =>
          err("a vector of no lanes holds nothing and computes nothing — an array may be empty " +
            "because it is storage, and a register cannot be")
        case Some(v)                                   => err(s"a vector cannot have $v lanes")
        // One lane rather than the zero an array stands at, because this stand-in is *lowered* on
        // the walk that checks a generic body and `<0 x float>` is not a type LLVM has. Every
        // vector the program actually gets is built by the instantiation pass, where the width
        // answers.
        case None if awaitsInstantiation(lanes, subst) => 1
        case None => err("a vector's lane count must be a constant — a literal, or a 'const' naming one")

      Type.Vector(n, resolved)

    // `(..A)` — the tuple of a pack (`reference/generics.md § A parameter may stand for a list of
    // types`). The pack is looked up rather than resolved, because what stands for it in the
    // substitution is already the list of parts: for the walk that checks a generic body that is
    // the two stand-ins, and for an instantiation it is the parts the subject matched. Either way
    // the tuple built here is an ordinary one, so nothing downstream of this point knows a pack was
    // written.
    case TupleType(List(PackType(n)), _) =>
      subst.get(n) match
        case Some(Type.Pack(elems)) => tupleType(elems.map(substituted))
        case Some(other) =>
          err(s"'..$n' is written as a type pack, and '$n' stands for ${show(other)} — a pack is " +
            "declared '[..A]' and a single type '[A]'")
        case None =>
          err(s"'..$n' names no type pack — a pack is declared in the parameter list, as '[..$n]'")

    // A tuple holds its parts the way a struct holds its fields, so a part is resolved exactly as a
    // field is — a `unit` part included, which the layout skips and the parts after it shift past.
    case TupleType(parts, false) => tupleType(parts.map(resolveType(_, subst)))

    // A pack outside the one place it may be written. `(..A)` is caught by the case above, so
    // reaching here means a bare `..A` stood where a type belongs.
    case PackType(n) =>
      err(s"'..$n' is a type pack and not a type — the only place one may be written is inside a " +
        s"tuple, as '(..$n)'")

    // C's function pointer, which is a type wherever any other is: it is one word, it is copied by
    // being copied, and nothing about it is counted. Its parts are held to exactly what an `extern`
    // holds its own to, which is nothing — what crosses the boundary is the programmer's business
    // (`reference/ffi.md § extern — a declaration with no body`), and a signature written here is the same kind of promise the `*` already is.
    case CFnType(params, ret) =>
      Type.CFn(params.map(resolveType(_, subst)), resolveReturn(ret, subst))

    // A callable is not a type: it is a trait, and a trait stands where a type does only behind a
    // mode sigil. The bare arrow is the sugar a **parameter** may use, and it never reaches here —
    // a parameter reads it before resolving one — so what is left to say is that a slot needing a
    // concrete type needs the box the `&` denotes (`reference/types.md § Function types`).
    case f: FnType =>
      checkFnArity(f)

      if f.bare then
        err(s"'${f.show}' is a callable a function is passed and does not keep, which only a " +
          s"parameter may be — anywhere a concrete type is required, a callable is boxed, so " +
          s"write '&Fn(${f.params.map(_.show).mkString(", ")}) -> ${f.ret.show}'")
      else
        err(s"'${f.show}' is a trait, and a trait is a type only behind a mode — write " +
          s"'&${f.show}' for the boxed callable a concrete slot takes")

    // A result list reaches here only where a *type* was asked for, which is everywhere but a
    // function's result — and the fix is the type that has the same parts.
    case TupleType(parts, true) =>
      err(s"'${parts.map(_.show).mkString(", ")}' is a result list, which a signature has and a " +
        s"value does not — write '(${parts.map(_.show).mkString(", ")})' for the type")

    // A written value argument that reached here is one in a position where no parameter is a value
    // parameter — `int[4]`, or an argument of a type whose declaration takes types. The declaration
    // is what decides, and `resolveArgs` has already read it, so what is left is to say so.
    case ValueArgType(_) =>
      err("a value stands here, and this argument is a type — a value argument belongs where the " +
        "declaration wrote 'const', and nowhere else")

    // `T::Item` — the associated type, read off whatever the subject turned out to be. A subject
    // still standing in for itself leaves the projection abstract, carrying the bounds the trait
    // declared for it; a concrete one is answered by the implementation that supplies it.
    case AssocType(base, member) => assocType(resolveType(base, subst), member)

    // `Name = T` reached a position where the brackets belong to something other than a trait behind
    // a mode. Only a trait object leaves an associated type for a bracket to fix — a generic type's
    // arguments are all written where it is used, so there is nothing there for a name to pick out.
    case AssocArgType(a, _) =>
      err(s"'$a = …' fixes an associated type, which only a trait object has to fix — it belongs " +
        s"inside the brackets of a '*Trait' or a '&Trait', and a type's arguments are written in " +
        s"order without names")

    // A `some` result that reached resolution is one written where the inference behind it has
    // nothing to read. `resolveShape` is reached from a *type* position, which `coreType` already
    // refuses one in — so what arrives here is a **result** in the two places a result is not an
    // `impl` block's member: a free function, and the trait's own declaration.
    case SomeType(bounds) =>
      err(s"'some ${bounds.map(_.show).mkString(" + ")}' says the type is read off the body of a " +
        s"member that supplies a trait's associated type, so it stands only in an 'impl' block — a " +
        s"trait writes 'type Name: ${bounds.head.show}' among its members and gives the member the " +
        s"result 'Self::Name'")

    case NamedType(n, argRefs) =>
      if argRefs.isEmpty && subst.contains(n) then substituted(subst(n))
      else
        val targs = resolveArgs(n, argRefs, subst)
        scalarType(n) match
          case Some(s) => plain(n, targs, s)
          // A declared type is named in this module's terms — its own, or a module's it names in
          // full (`reference/modules.md § Imports`) — so what the tables are asked for is the key
          // that resolves to.
          case None =>
            typeKey(n) match
              case Some(key) if structDecls.contains(key) =>
                if !behindPointer then checkLayoutKnown(key, n)
                instantiateStruct(key, targs)
              // An alias whose base is a declared type was already followed to it by `typeKey`, so
              // what reaches here is one standing for something with no key of its own — a scalar, a
              // pointer, an array, a callable. It resolves to that, and to nothing wrapping it: an
              // alias is a second spelling and not a second type.
              case Some(key) if plainAlias(key) =>
                if targs.nonEmpty then err(s"'$n' is an alias and takes no type arguments")
                resolveAlias(key)
              case Some(key) if constrainedDecls.contains(key) =>
                if targs.nonEmpty then err(s"'$n' is a constrained subtype and takes no type arguments")
                resolveConstrained(key)
              case Some(key)                              => instantiateEnum(key, targs)
              case None if n == neverName =>
                err("'never' is the type of an expression that does not finish, so it can only be a result type")
              case None if n == selfName =>
                err("'Self' names the type a trait is implemented for, so it is only meaningful inside " +
                  "a 'trait' or an 'impl'")
              // A trait describes what a value can do, not how one is laid out, so it stands where a
              // type is asked for only behind a sigil — and the two ways of reaching it are named here,
              // since which one a program wants is the whole of the choice it has to make.
              case None if traitKey(n).isDefined =>
                err(s"'$n' is a trait, so it describes behaviour rather than a layout — write '*$n' or " +
                  s"'&$n' for a trait object, or bound a type parameter with '[T: $n]'")
              case None => unresolvedErr(s"unknown type '$n'")

  /** A written argument list, resolved **against the declaration whose parameters they fill** —
   * because which argument is a type and which is a value is the declaration's fact and nothing the
   * grammar can see.
   *
   * `Buf[4]` and `Buf[int]` are the same shape to a parser, and so are `Buf[N]` and `Box[T]`: a bare
   * name is a type as far as the syntax goes. So a name standing in a value position is read here as
   * the expression it is, which is what lets one value parameter be passed straight to another —
   * `Buf[N]` inside a block declaring `const N`. Rust resolves a bare path in a const-argument
   * position the same way, for the same reason.
   *
   * An argument that could not be a type at all arrives as a `ValueArgType` and is folded. One in a
   * position the declaration made a *type* parameter is refused by `resolveShape`, which is where
   * every other "this is not a type" is said.
   */
  private def resolveArgs(n: String, argRefs: List[TypeRef], subst: Map[String, Type]): List[Type] = {
    val values = typeKey(n).fold(Map.empty[String, TypeRef])(nominalValues)

    if values.isEmpty then argRefs.map(ref => underTypeArg(resolveType(ref, subst)))
    else
      val tparams = typeKey(n).fold(List.empty[String])(nominalTparams)

      argsAgainst(tparams, values, argRefs, subst, ref => underTypeArg(resolveType(ref, subst)))
  }

  /** Written arguments read against the parameters they stand at: a value where the declaration
   * wrote `const`, folded to the constant that goes in the identity, and `typeArg` everywhere else.
   */
  private def argsAgainst(
      tparams: List[String],
      values: Map[String, TypeRef],
      argRefs: List[TypeRef],
      subst: Map[String, Type],
      typeArg: TypeRef => Type,
  ): List[Type] =
    argRefs.zipWithIndex.map { (ref, i) =>
      tparams.lift(i).filter(values.contains) match
        case Some(tp) => valueArg(ref, recover(Type.Unknown)(resolveType(values(tp), Map.empty)), subst)
        case None     => typeArg(ref)
    }

  /** A **trait's** written arguments, read the way a generic type's are — so `FromBytes[4]` applies
   * the trait to the value 4 at its `const N: usize`, and `FromBytes[u8]` there is refused in the
   * words a struct's value parameter refuses a type with (`reference/generics.md § A parameter may
   * stand for a value`). Every place that applies a trait — a bound, an `impl`, an object — reads
   * its arguments here, so a value argument is one constant wherever the trait is written.
   */
  protected def traitArgs(key: Option[String], refs: List[TypeRef], subst: Map[String, Type]): List[Type] =
    key.flatMap(traitDecls.get) match
      case Some(d) if d.tvalues.nonEmpty => argsAgainst(d.tparams, d.tvalues, refs, subst, resolveType(_, subst))
      case _                             => refs.map(resolveType(_, subst))

  /** One **value** argument: the expression it was written as, folded to the constant that goes in
   * the type's identity.
   *
   * A value parameter of the *enclosing* declaration stands at zero here for the walk that checks a
   * generic body, exactly as an array length written over one does — there is no argument yet, and
   * the tree that walk builds is discarded.
   */
  protected def valueArg(ref: TypeRef, ty: Type, subst: Map[String, Type], what: String = "this argument"): Type = {
    val written = ref match
      case ValueArgType(e)      => Some(e)
      case NamedType(name, Nil) => Some(Ident(name))
      case _                    => None

    written match
      case None =>
        err(s"$what stands where the declaration wrote 'const', so a value belongs here " +
          s"rather than a type — one of ${show(ty)}")
      case Some(e) =>
        constArgValue(e, subst).orElse(variantTag(e, ty)) match
          // The value goes into a type's identity at the parameter's declared width, so one that
          // does not fit it is refused in the words a `const` that does not fit is — rather than
          // keyed at a number the parameter could never hold.
          case Some(v) =>
            Type.underlying(ty) match
              case i: Type.Integer if !Type.fits(v, i) => err(s"$what does not fit ${show(i)}: $v")
              case _                                   => Type.ConstArg(v, ty)
          case None if awaitsInstantiation(e, subst) => Type.ConstArg(0, ty)
          // A name that turns out to be a **type** is the likely mistake in this position, and it
          // gets its own sentence: `Buf[int]` reads as an argument list of types until the
          // declaration says otherwise, so the reader is told which of the two this slot is.
          case None =>
            e match
              case Ident(n) if typeKey(n).isDefined || scalarType(n).isDefined =>
                err(s"'$n' is a type, and this argument stands where the declaration wrote " +
                  s"'const' — so a value of ${show(ty)} belongs here")
              case _ =>
                err("a value argument must be a constant — a literal, or a 'const' naming one")
  }

  /** The tag a **simple enum's variant** stands for, where the value parameter's declared type is
   * that enum (`reference/generics.md § A parameter may stand for a value`).
   *
   * A simple enum's value *is* its identity — there is nothing else telling two of its variants
   * apart (`09`) — so its tag is exactly the number a type's identity wants. It is read off the
   * instantiated enum rather than recomputed from the declaration, so an explicit discriminant and
   * the gap after it are the ones the rest of the compiler already agreed on.
   *
   * A **data** enum needs no case and gets none: its variants carry values, so a number does not
   * tell two of them apart and there is no identity to be made of one.
   */
  private def variantTag(e: Expr, ty: Type): Option[BigInt] = (e, ty) match
    case (Ident(n), en: Type.Enum) if en.simple =>
      variantKey(n)
        .filter(k => variantOwnerList(k).contains(en.base))
        .flatMap(_ => en.variant(n.split('.').last))
        .map(v => BigInt(v.tag))
    case _ => None

  /** The `Type.Constrained` a subtype name stands for, built once and cached. Building resolves the
   * base, evaluates the `within` bounds to constants, and validates them — an out-of-range or
   * inverted bound is caught here, at the declaration, rather than at any use.
   */
  private def traitObject(inner: TypeRef, subst: Map[String, Type], sigil: String): Option[Type.Trait] =
    inner match
      // A callable behind a mode is a trait object of the call trait, and everything that makes one
      // — object safety, the arguments, the table — is the same question asked of the same shape.
      case f: FnType =>
        checkFnArity(f)
        at(f.pos)(traitObject(f.asTrait, subst, sigil))
      case NamedType(n, argRefs) if traitKey(n).isDefined && !(argRefs.isEmpty && subst.contains(n)) =>
        val key  = traitKey(n).get
        val decl = traitDecls(key)

        // The brackets carry two different things now, and they are separated before anything is
        // resolved: an ordinary argument fills one of the trait's own parameters, and a `Name = T`
        // fixes an associated type the implementation would otherwise have chosen (`02`).
        val (bindRefs, posRefs) = argRefs.partition(_.isInstanceOf[AssocArgType])

        at(inner.pos) {
          val declared = objectAssocs(key)

          // **A bare associated-type name is the mistake to catch before resolution**, because after
          // it the reader is told there is no type of that name — which is true and is about the
          // wrong thing. `&Seq[Item]` is somebody one `=` short of the form they meant.
          for case NamedType(a, Nil) <- posRefs if declared.exists(_.name == a) do
            err(s"'$a' is an associated type of '${qn(key)}', which an object fixes by name rather " +
              s"than by position — write '$sigil${qn(key)}[$a = …]'")

          val written = traitArgs(Some(key), posRefs, subst)

          // **The one unambiguous case is written without the name**, and it is the common one: a
          // trait with no parameters of its own and exactly one associated type has only one thing a
          // bare argument could mean, so `*Iterate[string]` is `*Iterate[Item = string]`. Where the
          // trait has parameters too, a bare argument means one of *those* and the associated type
          // has to be named — which is what keeps `*Index[usize]` readable once `Index` carries an
          // element of its own.
          val sugared =
            Option.when(decl.tparams.isEmpty && declared.length == 1 && bindRefs.isEmpty && written.length == 1)(
              (declared.head.name, written.head))

          val positional = if sugared.isDefined then Nil else written

          // **A bare argument too many, on a trait that declares associated types.** The arity
          // refusal is right about the count and says the wrong thing about the cause — "does not
          // take type arguments" reads as a trait with nothing open, and this one has something
          // open that is simply not fixed by position.
          if sugared.isEmpty && declared.nonEmpty && positional.length > decl.tparams.length then
            val names   = declared.map(_.name)
            val fixes   = names.map(a => s"$a = …").mkString(", ")
            val whole   = (decl.tparams.map(_ => "…") ::: names.map(a => s"$a = …")).mkString(", ")
            val owns    =
              if decl.tparams.isEmpty then s"'${qn(key)}' takes no type arguments of its own"
              else s"'${qn(key)}' takes ${quantity(decl.tparams.length, "type argument")} of its own"

            // The tail says why the short form did not apply, and there are two different reasons:
            // the trait has more than one thing open, or it has exactly one and was given the wrong
            // number of arguments. A reader in the second case has the form right and the count
            // wrong, and telling them about ambiguity would be telling them about somebody else's
            // mistake.
            val why =
              if decl.tparams.isEmpty && names.length == 1 then
                s"One bare argument is the short form for '$fixes', and ${quantity(positional.length, "argument")} " +
                  s"${if positional.length == 1 then "is" else "are"} not"
              else
                s"A bare argument is the short form only for a trait with no parameters of its own and " +
                  s"exactly one associated type, so here it would not say which"

            err(s"$owns, and what it leaves for an object to fix is " +
              s"${if names.length == 1 then "the associated type" else "the associated types"} " +
              s"${names.mkString("'", "', '", "'")} — an object fixes one by name, so write " +
              s"'$sigil${qn(key)}[$whole]'. $why")

          checkTraitArity(n, decl.tparams, decl.tdefaults, positional)

          val args = withDefaults(key, decl.tparams, decl.tdefaults, positional, Map.empty)

          val assocs = sugared.toList ::: bindRefs.collect { case AssocArgType(a, ref) =>
            (a, at(ref.pos)(resolveType(ref, subst)))
          }

          checkAssocArgs(key, declared, assocs, sigil, args)
          deferredBounds(key, decl.tparams, decl.bounds, args)
          // A `Self` that arrived from one of *this* trait's own defaults is left to the check
          // below, because there the fix is a spelling: writing the argument out. That now includes
          // the operator catalog, which it did not when an operator's result was fixed to `Self`
          // and no argument could rescue it — `&Mul[real, real]` is a formable object
          // (`library/core.md § Walking a type of your own`).
          checkObjectSafe(key, args, sigil, decl.tparams.drop(positional.length).toSet, assocs.map(_._1).toSet)

          // An object has forgotten which type it holds, so a default that names one has nothing to
          // stand for, and writing the argument is the fix.
          for tp <- decl.tparams.drop(positional.length); ref <- decl.tdefaults.get(tp) if mentionsSelf(ref) do
            err(s"'$tp' defaults to '${ref.show}', which names the type implementing '${qn(key)}', " +
              "and an object has forgotten which type it holds — write the argument")

          Some(Type.Trait(key, args, assocs))
        }
      case _ => None

  /** Whether a trait may be erased into an object at all, and what to say when it may not (`02`).
   *
   * An erased value has forgotten its type, so a method may promise nothing that depends on knowing
   * it. `Self` may stand for the receiver and nowhere else: a second `Self` would have to be the
   * same forgotten type as the first, which is exactly the fact a trait object no longer has, and a
   * `Self` result has no size to hand back. That excludes every trait in the operator catalog —
   * `add(self, rhs: Self) -> Self` first among them — which is why those traits are for bounds.
   *
   * An associated function has no receiver to dispatch on. A **property** does — it just never spells
   * one — so a trait that asks for a property is as object-safe as one that asks for a method, and
   * the slot it gets is the same slot. And `&self` asks for its receiver to be inside a
   * reference-counted box, which only the counted object has: `&Trait` carries one, so it accepts
   * such a method, and `*Trait` points straight at a value and does not.
   */
  /** Every associated type an object over this trait has to account for: the trait's own, and those
   * of every trait it requires.
   *
   * A required trait's associated type is as much a hole in a slot's signature as the trait's own —
   * the required members are slots in the same table — so the object binds it in the same brackets
   * and under the same name. It can be one flat list because a type may implement at most one trait
   * declaring an associated type of any given name (`02`), which is the rule that lets a projection
   * be written without naming its trait.
   */
  protected def objectAssocs(name: String, args: List[Type] = Nil): List[AssocDecl] =
    traitClosure(Type.Bound(name, args)).flatMap(b => traitDecls.get(b.name).toList.flatMap(_.assocs)).distinctBy(_.name)

  /** Holds the `Name = T` clauses of a trait object's brackets to the associated types the trait
   * actually declares: each one named once, each one declared, and each one meeting the bounds the
   * trait asked of it.
   *
   * What it does *not* check is that the value erased into the object agrees — that is the erasure's
   * question, asked where the concrete type is known, and `TraitObjects.erase` asks it.
   */
  protected def checkAssocArgs(
      name: String,
      declared: List[AssocDecl],
      supplied: List[(String, Type)],
      sigil: String,
      args: List[Type],
  ): Unit = {
    val obj = s"'$sigil${Type.qualified(qn(name), args)}'"

    for (a, _) <- supplied if !declared.exists(_.name == a) do
      if declared.isEmpty then
        err(s"'${qn(name)}' declares no associated types, so there is nothing in $obj for '$a' to " +
          s"fix — a trait declares one with 'type $a: …' among its members")
      else
        err(s"'${qn(name)}' declares no associated type '$a' — it declares " +
          declared.map(_.name).mkString("'", "', '", "'"))

    for (a, _) <- supplied.groupBy(_._1).collect { case (a, more) if more.length > 1 => (a, more) } do
      err(s"$obj fixes '$a' more than once, and a type has one of each")

    // The bounds are the trait's promise about whatever supplies the type, and an object supplying
    // one directly is held to them exactly as an `impl` is — otherwise a slot's signature would
    // typecheck against a type the member's body was never licensed to use.
    //
    // **Deferred, for the reason every other applied bound is deferred**: a signature is resolved
    // while the file is still being hoisted, so the `impl` that makes the supplied type conform may
    // not be registered yet. Asked here and now, `&Seq[Item = int]` in a program that also writes
    // `impl Render for int` is refused against a table still being filled.
    val bound = declared.flatMap(d => supplied.find(_._1 == d.name).map((d, _)))

    deferredBounds(
      qn(name),
      bound.map(_._1.name),
      bound.map((d, _) => d.name -> d.bounds).toMap,
      bound.map(_._2._2),
      noun = "associated type",
    )
  }

  protected def checkObjectSafe(
      name: String,
      args: List[Type],
      sigil: String,
      defaulted: Set[String] = Set.empty,
      fixed: Set[String] = Set.empty,
  ): Unit = {
    val obj = s"'$sigil${Type.qualified(qn(name), args)}'"

    // **An associated type is what erasure spends**, and it is said here rather than left to the
    // `Self` rule below because the reader did not write a second `Self` to be told about: the
    // projection in the member's result is a function of the very type an object has forgotten, so
    // the slot's signature would differ per implementing type. The trait that declared it is named,
    // which for a required trait is not the one the object was written as.
    for b <- traitClosure(Type.Bound(name, args)); d <- traitDecls.get(b.name); a <- d.assocs if !fixed(a.name) do
      err(s"'${qn(b.name)}' declares the associated type '${a.name}', whose meaning is the " +
        s"implementing type's — an erased value has forgotten which type that is, so there is no " +
        s"$obj to form. Say which it is and there is: write '$sigil${qn(name)}[${a.name} = …]', or " +
        s"take a bound instead, which keeps the type and so keeps the answer: '[T: ${qn(b.name)}]'")

    // A trait offers what it requires as well as what it declares, so a required trait that cannot
    // be erased makes the trait that required it unerasable too — and the diagnostic names the one
    // the member came from, which for a required trait is not the one the object was written as.
    for (from, m) <- traitMembers(Type.Bound(name, args)) do
      val shown = qn(from.name)

      if m.recvMode.isEmpty then
        err(s"'$shown' declares the associated function '${m.name}', which has no receiver to " +
          s"dispatch on — so there is no $obj to form")
      if m.receiver.exists(_.isInstanceOf[RecvMode.ByRef]) && sigil == "*" then
        err(s"'${m.name}' of '$shown' takes '&self', so it needs its receiver inside a " +
          s"reference-counted box — $obj points straight at a value, so write '&${qn(name)}' instead")
      // A `Self` reaches a signature two ways and both have to be looked for. It may be **written**
      // there — `other: *Self` — or it may arrive through one of the trait's own **arguments**, which
      // is how the operator catalog carries it: `add(self, rhs: Rhs) -> Out` mentions no `Self` at
      // all, and `Add`'s two defaults *are* `Self`, so a bound written bare hands both parameters the
      // very type the object has forgotten. Reading only what was written would let `trait Word: Add`
      // erase and give the object a slot whose argument type differs per implementing type.
      // A parameter this application left to a default is not reported here: the caller says which
      // those are, and reports them itself in the words that name the fix.
      val viaArg = paramsBoundToSelf(from) -- (if from.name == name then defaulted else Set.empty)

      if (m.params.map(_.typ) ++ m.retType).exists(t => mentionsAny(t, viaArg + selfName, fixed)) then
        err(s"'${m.name}' of '$shown' mentions 'Self' away from its receiver, and an erased value " +
          s"has forgotten which type that is — so there is no $obj to form")
      // A variadic call names the callee's whole function type, which is how it says where the
      // declared parameters stop; a table slot is one word and names nothing. A bound still reaches
      // such a method, because that call knows which function it is reaching.
      if m.variadic then
        err(s"'${m.name}' of '$shown' takes a '...', and a call to one names the whole function " +
          s"type it is reaching — a slot in a method table is a word and names none, so there is " +
          s"no $obj to form")
  }

  /** Holds a written callable to an arity the library declares a call trait for.
   *
   * The limit is the library's and not the language's, so what it says is where to go next: a
   * callable this wide is a signature nobody reads, and a struct of the arguments names them.
   */
  protected def checkFnArity(f: FnType): Unit =
    if f.params.length > Type.Fn.maxArity then
      at(f.pos)(err(s"a callable takes up to ${Type.Fn.maxArity} parameters and this one takes " +
        s"${f.params.length} — a call this wide reads better with the arguments named, so pass a " +
        "struct of them"))

  /** Whether a written type names `Self` anywhere inside it. */
  protected def mentionsSelf(t: TypeRef): Boolean = mentionsAny(t, Set(selfName))

  /** Whether a written type names any of `names` anywhere inside it. `Self` is the one that matters
   * for erasure, and a trait *parameter* matters exactly when `Self` is what was passed for it.
   */
  protected def mentionsAny(t: TypeRef, names: Set[String], settled: Set[String] = Set.empty): Boolean = t match
    case NamedType(n, args)  => names(n) || args.exists(mentionsAny(_, names, settled))
    case PtrType(i)          => mentionsAny(i, names, settled)
    case RefType(i, _)       => mentionsAny(i, names, settled)
    case WeakType(i)         => mentionsAny(i, names, settled)
    case ArrayType(_, e, _)  => mentionsAny(e, names, settled)
    case VectorType(_, e)    => mentionsAny(e, names, settled)
    // The question this answers is about erasure — whether a **type** is named — and a value
    // argument names none, so it can hold no forgotten one.
    case _: ValueArgType     => false
    case VolatileType(i)     => mentionsAny(i, names, settled)
    case TupleType(parts, _) => parts.exists(mentionsAny(_, names, settled))
    // A pack names one of the block's own parameters, so it is named here exactly as `T` would be.
    case PackType(n)         => names(n)
    case f: FnType           => mentionsAny(f.asTrait, names, settled)
    case CFnType(ps, r)      => ps.exists(mentionsAny(_, names, settled)) || mentionsAny(r, names, settled)
    // **`Self::Body` the object fixed mentions nothing it has forgotten.** That is the whole of what
    // `settled` is for: the projection names a type the object type wrote down, so the slot's
    // signature is the same for every implementing type and there is a table to point at.
    case AssocType(NamedType(n, Nil), member) if n == selfName && settled(member) => false
    // Otherwise `Self::Body` mentions `Self`, which is why a trait declaring an associated type
    // cannot be erased unless the object says which type it is. `checkObjectSafe` says so in those
    // words before this answer is reached, so that the reader is told about the associated type
    // rather than about a `Self` they did not spell twice.
    case AssocType(base, _)  => mentionsAny(base, names, settled)
    // What an object fixes an associated type *to* is an ordinary type and is read as one: a
    // `*Sink[Item = Self]` names the forgotten type as surely as a parameter of that type would.
    case AssocArgType(_, t)  => mentionsAny(t, names, settled)
    case _: SomeType         => false

  /** Which of a trait's own type parameters were given `Self` at this application — the parameters a
   * member may name and so mention the forgotten type without ever spelling it.
   */
  protected def paramsBoundToSelf(b: Type.Bound): Set[String] =
    traitDecls.get(b.name).toList
      .flatMap(_.tparams.zip(b.args))
      .collect { case (tp, Type.Abstract(n, _)) if n == selfName => tp }
      .toSet

  /** Resolves the pointee of a `*T` / `&T`, which is one level further from the layout of
   * whatever type is currently being laid out.
   */
  /** Holds a type to being one that can be *pointed at* or laid out in a row.
   *
   * A zero-sized type may be a field, a parameter, or a binding — none of those needs an address —
   * but a `&T`, a `*T`, a slice, and an array all reach their contents through one, and there is
   * nothing to reach. An array is the sharper case: every element would be at the same address, so
   * a bounds check would be the only thing `a[i]` did.
   */
  protected def addressable(t: Type, what: String): Type =
    if Type.zeroSized(t) then
      err(s"${show(t)} occupies no storage, so there is nothing for $what to point at")
    else t

  protected def underIndirection(resolve: => Type): Type = {
    indirection += 1
    try resolve
    finally indirection -= 1
  }

  /** Whether reaching an in-progress instantiation again is a legal cycle. It is exactly when
   * at least one indirection was crossed on the way back to it: a `Node` holding a `*Node` is
   * pointer-sized, while a `Node` holding a `Node` by value has no finite size.
   *
   * **A type argument is not an answer to this question and is left alone.** `Buf[Node]` reaches
   * `Node` while `Node` is still being resolved, and whether that is containment depends on how
   * `Buf` uses its parameter: `Buf` holds a `[]T`, which is an indirection, and a `Wrap` holding a
   * bare `T` is not. So the argument's own resolution says nothing, and the question is asked again
   * at every *use* of the substitution inside the instantiation — where the indirection count is the
   * one that matters. `substituted` is the other half of this, and it is the half that carries the
   * weight: **the deferral is only sound because something asks later**, over what the argument
   * reaches rather than over the argument itself.
   *
   * **What is excused is an argument list entered AFTER the type in question, not merely being
   * inside one.** Both counts are therefore compared against where the type was entered rather than
   * against zero, and each catches what the other cannot:
   *
   *   - Resolving `Buf[B]` instantiates `B` inside the argument position, so `B`'s own `a: A` is on
   *     a path that left `A` through an argument list. `A` was entered outside it, the counts
   *     differ, and the cycle is left alone — whichever of the two was declared first, which is what
   *     a mutual cycle through a `Buf` needs and did not get.
   *   - `B` reaching **itself** is a different matter: it was entered inside that same argument list,
   *     so the counts agree and it is condemned exactly as it would be anywhere else. An argument
   *     position is an excuse for the path that came through it and never for what a type does to
   *     itself — and nothing downstream would catch it, since a generic that does not use its
   *     parameter in a field never substitutes it.
   */
  protected def cycleCheck(key: String): Unit = {
    val at = resolving(key)

    if indirection <= at.indirection && typeArgDepth <= at.typeArgs then
      err(s"type '${qn(key)}' contains itself, so it has no finite size")
  }

  /** A type argument's resolution, which reaching an in-progress type inside does not condemn. */
  protected def underTypeArg[A](resolve: => A): A = {
    typeArgDepth += 1
    try resolve
    finally typeArgDepth -= 1
  }

  /** What a type parameter stands for, asked the containment question the argument position
   * deferred: a `T` used **by value** here is the containing type held by value, whatever
   * indirection the parameter was written behind.
   *
   * **The question is about what the argument REACHES, not about what it is.** Asking only whether
   * the argument is itself in progress answers a mutual cycle wrong, because the other type on the
   * cycle has usually finished by the time the generic's field substitutes it: `Wrap[B]` over
   * `struct B { a: A }` holds `B` by value and so holds `A` by value, and `B` is memoized as
   * complete long before `Wrap`'s `x: T` is reached. So this walks the argument's layout by value
   * and puts every type on it that is still being resolved to `cycleCheck` — every one and not the
   * first, for the reason `reachedByValue` gives.
   */
  private def substituted(t: Type): Type = {
    if resolving.nonEmpty then
      val found = mutable.LinkedHashSet.empty[String]

      reachedByValue(t, mutable.Set.empty, found)
      found.foreach(cycleCheck)

    t
  }

  /** Every still-resolving type a layout reaches **without crossing an indirection**, which is the
   * whole of what makes a cycle infinite.
   *
   * A struct's fields, an enum variant's payload, a tuple's parts, a fixed array's elements and a
   * constrained type's base are all held; a `*T`, a `&T`, a `weak T` and a slice all point, so the
   * walk stops at them exactly as `underIndirection` counts them.
   *
   * **Every one of them, rather than the first**, because they are not interchangeable: `resolving`
   * records the indirection each type was *entered* at, and a walk can reach two that were entered
   * at different ones. `cycleCheck` condemns only where the count here has not risen above that
   * depth, so stopping at the first would let a genuine cycle through whenever a shallower type
   * happened to sit earlier in the field order — the pointer that excuses one of them says nothing
   * about the other.
   *
   * It terminates on its own — a type still being resolved has no fields yet, and one that has them
   * was finite when it got them — and carries `seen` so that a shape reached twice by two fields is
   * walked once.
   */
  private def reachedByValue(t: Type, seen: mutable.Set[String], found: mutable.Set[String]): Unit = t match
    case s: Type.Struct =>
      val key = Type.instanceKey(s.base, s.targs)

      if resolving.contains(key) then found += key
      else if seen.add(key) then s.fields.foreach((_, f) => reachedByValue(f, seen, found))
    case e: Type.Enum =>
      val key = Type.instanceKey(e.base, e.targs)

      if resolving.contains(key) then found += key
      else if seen.add(key) then e.variants.foreach(_.fields.foreach((_, f) => reachedByValue(f, seen, found)))
    case Type.Array(_, elem) => reachedByValue(elem, seen, found)
    case c: Type.Constrained => reachedByValue(c.base, seen, found)
    case _                   => ()

  /** The rules a declared signature must satisfy whichever declaration form it came from, checked
   * after the name is registered so a failure reports the mistake without also erasing the
   * declaration it is about.
   */
  protected def checkSignatureRules(
      name: String,
      params: List[Param],
      ret: Option[TypeRef],
      variadic: Boolean,
      foreign: Boolean = false,
  ): Unit = {
    // C reads a variadic call's arguments relative to the last named parameter, so there has to be
    // one; `f(...)` is not a callable declaration in any C either.
    if variadic && params.isEmpty then err(s"'$name' needs at least one named parameter before '...'")
    checkRestPosition(name, params, variadic, foreign)
    checkVaListPositions(name, params, ret, foreign)
  }

  /** Where a `...T` parameter may stand, which is last and only last
   * (`reference/declarations.md § A parameter may collect the rest of the call`).
   *
   * **It collects everything the call has left**, so a parameter after it could never be given
   * anything: the arguments that would have been its are already inside the slice. That is the same
   * rule Go, Swift, Java and C# each arrived at, and it is a fact about what "the rest" means rather
   * than a limitation of the packing.
   *
   * **It may not stand beside the C ellipsis**, which is a different feature wearing the same three
   * characters: `...` with no name and no type is the foreign tail, walked with `va_arg`, and a
   * declaration carrying both would have two tails and one call.
   *
   * **And an `extern` may not carry one.** The packing happens at the call and hands over a sysl
   * slice — three words with an owner in them — which is not something a C function was compiled to
   * read. What a C variadic takes is the other form.
   */
  private def checkRestPosition(name: String, params: List[Param], variadic: Boolean, foreign: Boolean): Unit =
    for p <- params.find(_.rest) do
      at(p.pos) {
        if foreign then
          err(s"'$name' is an 'extern', and '${p.name}: ...T' hands over a sysl slice — which is " +
            "three words with an owner in them, and not what a C function was compiled to read. " +
            "C's own tail is the bare '...' with no name and no type")
        else if variadic then
          err(s"'$name' has two tails: '${p.name}: ...T' collects the rest of the call into a " +
            "slice, and '...' is C's ellipsis, walked with 'va_arg'. A call has one tail, so a " +
            "declaration may write one of them")
        else if params.last ne p then
          err(s"'${p.name}' collects the rest of the call, so nothing can follow it — the " +
            s"arguments that would be '${params.last.name}' are already inside it")
      }

  /** Where a `va_list` may stand in a signature (`reference/ffi.md § Variadic functions`).
   *
   * In a **sysl** signature a walk is handed on **by address**: `*va_list` is the parameter type,
   * and `&ap` is what the caller writes. A bare `va_list` parameter is refused because a parameter
   * is a by-value binding (`reference/declarations.md § Functions`) and a copy of a walk is not a
   * walk — advancing it would advance nothing the caller could see, which is the one thing the form
   * exists to do.
   *
   * An **`extern`** transcribes a C header, so it is written in C's spellings and both are allowed:
   * `va_list` is C's by-value parameter, the one `vprintf` takes, and `*va_list` is C's `va_list
   * *`, the one a function that must advance its caller's own walk takes. What a call actually
   * hands the first of those is a different thing on every target and is `TVaPass`'s business
   * (`getting-started/cli.md § targets`); the *call* writes `&ap` for either, because the address
   * is the only thing sysl has and it is what both are formed from.
   *
   * **Returning a bare `va_list` is refused everywhere**, foreign or not: sysl has no by-value
   * `va_list` at all — the type names the storage a walk lives in — so there would be nothing to
   * put the result in. A `*va_list` return is an ordinary pointer and is allowed.
   */
  private def checkVaListPositions(
      name: String,
      params: List[Param],
      ret: Option[TypeRef],
      foreign: Boolean,
  ): Unit = {
    def isVaList(t: TypeRef) = t match
      case NamedType(n, Nil) => scalarType(n).contains(Type.VaList)
      case _                 => false

    for p <- params if !foreign do
      if isVaList(p.typ) then
        at(p.pos)(err(s"a va_list is a parameter as '*va_list', not as 'va_list' — a parameter is a " +
          s"by-value binding, and a copy of a walk advances nothing '$name''s caller can see, so " +
          "the walk is handed over by address and the call writes '&ap'"))

    for r <- ret do
      if isVaList(r) then
        at(r.pos)(err(s"a va_list cannot be returned from '$name' — the type names the storage a " +
          "walk lives in, and there is no value of it to hand back"))
  }

  /** The type a foreign parameter has once it is a slot in a signature.
   *
   * Only one written type changes: a C `va_list` parameter takes the address of the walk, since
   * that is what sysl has to give and what every target's answer is formed from. So an argument is
   * checked as a `*va_list` whichever of C's two spellings the header used, and the difference
   * between them — whether the callee receives the walk or a pointer to it — is carried by
   * `TVaPass` rather than by the parameter's type.
   */
  protected def foreignParam(t: Type): Type = if t == Type.VaList then Type.Ptr(Type.VaList) else t

  /** Which of a foreign declaration's parameters were written as C's by-value `va_list`, and so
   * need what a call passes converted to the target's ABI (`getting-started/cli.md § targets`).
   */
  protected def foreignVaByValue(e: ExternDecl): Set[Int] =
    e.params.zipWithIndex.collect {
      case (Param(_, NamedType(n, Nil), _, _, _, _), i) if scalarType(n).contains(Type.VaList) => i
    }.toSet

}
