package sh.sysl

import scala.collection.mutable

/** The generic machinery: building a concrete `Type` from a name and its type arguments, and
 * solving those arguments back out of a call's argument types.
 *
 * The two directions are one subject. Instantiation is the forward direction — given `Buf` and
 * `[u8]`, lay out the struct with `T` bound to `u8` — and unification is the inverse, recovering the
 * binding from what a call was actually handed so the forward direction can then run. Everything
 * that makes the forward direction correct is what makes the inverse decidable, which is why they
 * are read together rather than a file apart.
 *
 * An instantiation is registered **before** its fields are resolved, so a type that reaches itself
 * through a `*T` / `&T` / `[]T` finds the in-progress object and the recursion terminates. That is
 * also why a cycle with no indirection to break it is a separate check rather than a stack overflow.
 */
trait GenericInstantiation extends ConstFolding {

  /** Two answers about a generic declaration that `TypeResolution` gives, and which the arity check
   * below needs before that trait is mixed in: how few arguments a parameter list will accept once
   * its defaults are counted, and whether a type reached itself with no indirection to break it.
   */
  protected def leastArgs(tparams: List[String], tdefaults: Map[String, TypeRef]): Int
  protected def cycleCheck(key: String): Unit

  /** How an arity is described when it is wrong -- "1 type argument", "between 1 and 3". Shared with
   * the trait-arity check, which asks the same question of a trait's parameter list.
   */
  protected def arityPhrase(tparams: List[String], tdefaults: Map[String, TypeRef]): String = {
    val least = leastArgs(tparams, tdefaults)

    if least == tparams.length then s"takes ${quantity(tparams.length, "type argument")}"
    else s"takes between $least and ${tparams.length} type arguments"
  }

  /** Resolves a scalar type name: the named primitives and friendly aliases, or one of the
   * systematic `iN` / `uN` / `fN` width spellings.
   */
  protected def scalarType(name: String): Option[Type] =
    Type.scalars.get(name).orElse(widthType(name))

  /** `i5`, `u12`, `f32` — a family letter followed by a width. The integer family is open,
   * so it is recognised by shape rather than listed; a width the back end cannot lower is a
   * diagnostic, not an unknown name.
   *
   * **The integer limit is LLVM's own, `2^23 - 1`**, because the two reasons this once stopped at
   * 128 were both checked and neither survived:
   *
   *   - *"a division at a wider width has no compiler-rt routine behind it"* — it needs none.
   *     `udiv i256`, `udiv i1024` and `mul i8192` all compile with **no undefined symbols at all**;
   *     the back end expands a wide division inline rather than calling out to one.
   *   - *"the decimal rendering is written once at the widest width there is"* — it is written
   *     **per width**, on demand: `ScalarEmitter` asks `StringEmitter.intName(bits)` for a renderer
   *     and gets one generated at that width, with its buffer sized from the width itself.
   *
   * So the ceiling is the back end's and nothing else's. It is still not a statement about what
   * `reference/types.md § Integers are an open family` permits — the maximum the *language* allows
   * is that chapter's open question, and a width this large is a thing the machine can hold rather
   * than a thing a program should want.
   *
   * **Two known costs at the extreme, accepted deliberately rather than guarded against.** A width
   * near the ceiling makes `digitCapacity` evaluate `2^bits` as a `BigInt` and take its decimal
   * length — millions of digits, at compile time, once per width a program actually renders. And a
   * value that wide is a megabyte travelling by value. Neither is reachable by accident: nothing
   * writes `i8388607` without meaning to.
   */
  protected def widthType(name: String): Option[Type] = {
    val digits = name.drop(1)

    // A family letter followed by digits and nothing else. Anything that is not that shape is not
    // this family's business and may be a name a program declared — `i5x` is an ordinary identifier.
    if name.length < 2 || !"iuf".contains(name.head) then None
    else if !digits.forall(c => c >= '0' && c <= '9') then None
    // **Once the shape matches, the name belongs to the family and a bad width is a diagnostic** —
    // never a fall-through to "unknown type", which sends a reader looking for a missing
    // declaration when what is wrong is the number they wrote. `scalarType` consults this before it
    // consults the declared types, so the shape is reserved either way; saying so is the whole
    // difference between the two messages.
    else if digits.head == '0' then
      if digits.forall(_ == '0') then
        err(s"'$name' has no bits — the width families start at 1, and a single bit is " +
          s"'${name.head}1'")
      else err(s"'$name' has a leading zero — a width is written plainly, as '${name.head}" +
        s"${digits.dropWhile(_ == '0')}'")
    else
      val bits = digits.toIntOption.getOrElse(err(s"'$name' is far wider than anything can hold"))

      if name.head == 'f' then
        if bits == 16 || bits == 32 || bits == 64 then Some(Type.Floating(bits))
        else if bits == 128 then err("'f128' is not lowered yet — the widest float is 'f64'")
        // `bf16` is named rather than measured — it is `f16`'s width and not its format — so it is
        // not a width this shape could have produced, and the message names it where a reader
        // reaching for a sixteen-bit float would look.
        else err(s"'$name' is not an IEEE floating-point width; they are f16, f32, and f64, and " +
          "the other sixteen-bit format is 'bf16'")
      else if bits >= 1 && bits <= Type.MaxIntegerBits then
        Some(Type.Integer(bits, signed = name.head == 'i'))
      else err(s"'$name' is wider than the ${Type.MaxIntegerBits} bits the back end lowers")
  }

  protected def plain(name: String, targs: List[Type], ty: Type): Type =
    if targs.nonEmpty then err(s"type '$name' does not take type arguments") else ty

  protected def checkArity(
      name: String,
      tparams: List[String],
      tdefaults: Map[String, TypeRef],
      targs: List[Type],
  ): Unit =
    if targs.length > tparams.length || targs.length < leastArgs(tparams, tdefaults) then
      if tparams.isEmpty then err(s"type '${qn(name)}' does not take type arguments")
      else
        err(s"type '${qn(name)}' ${arityPhrase(tparams, tdefaults)}, " +
          s"but ${supplied(targs.length, "type argument")}")

  /** Instantiates a struct for one set of type arguments, memoized on the display name. The
   * instantiation is registered *before* its fields are resolved, so a field that points back
   * at the struct finds it and the recursion terminates; `cycleCheck` is what rejects the cycle
   * that has no indirection to break it.
   */
  protected def instantiateStruct(name: String, targs: List[Type]): Type.Struct =
    inDecl(name)(instantiateStructIn(name, targs))

  private def instantiateStructIn(name: String, written: List[Type]): Type.Struct = {
    if brokenDecls(name) then poisoned()

    val decl = structDecls(name)
    checkArity(name, decl.tparams, decl.tdefaults, written)

    // Filled before anything is keyed on it, so `Buf[int]` and `Buf[int, Heap]` are one
    // instantiation rather than two that happen to have the same fields. And made nameless for the
    // same reason: `Buf[H]` over a `c type` or an exported alias of `u64` *is* `Buf[u64]`.
    val targs = withDefaults(name, decl.tparams, decl.tdefaults, written, Map.empty).map(Type.nameless)

    checkTypeBounds(name, decl.tparams, targs)
    val key = Type.instanceKey(name, targs)

    structInsts.get(key) match
      case Some(s) => s
      case None if inProgress.contains(key) =>
        cycleCheck(key)
        inProgress(key).asInstanceOf[Type.Struct]
      case None =>
        val s = new Type.Struct(name, targs)
        inProgress(key) = s
        resolving(key) = Entered(indirection, typeArgDepth)
        val subst = decl.tparams.zip(targs).toMap

        // A field whose type does not resolve is recorded and taken as unknown, so the struct
        // still has the shape the programmer wrote: the right fields, in the right order, with
        // the right count. Abandoning the instantiation instead would leave every later mention
        // of the type reporting something about a struct that never finished being built.
        //
        // The `finally` is what keeps the resolver's own bookkeeping honest whatever happens
        // here: an entry left in `inProgress` would make the next mention of this type look
        // like a cycle, which is a diagnostic about nothing at all.
        try
          s.fields = decl.fields.map(f => (f.name, recover(Type.Unknown)(resolveQualified(f.typ, subst))))
          s.packed = decl.packed
          s.opaque = decl.opaque
          s.minAlign = decl.alignment.flatMap(a => recover(Option.empty[Int])(alignBound(decl.name, a)))
          // The declared name where `@export` carried no string, which is the reading it has on a
          // function. It is the **bare** name rather than the key: a key is `module$Name`, and being
          // rid of the module path is the whole of what the attribute is written for.
          s.cname = decl.cname.map(e => e.symbol.getOrElse(Modules.bare(decl.name)))
          recover(())(checkBitfields(s))
        finally
          resolving -= key
          inProgress -= key

        structInsts(key) = s
        s
  }

  /** The one thing a **bitfield struct** may not hold, refused where the struct is built rather
   * than where one of its fields is later read (`reference/types.md § Structs`, `Bitfields`).
   *
   * A `@packed` struct with a field narrower than a byte is one integer, and the refusal follows
   * from that sentence rather than from a policy laid over it: a field that does not lower to an
   * integer has no bit range to occupy, and the alternative — packing a pointer into the container —
   * would mean an `inttoptr` round trip that loses the provenance the back end reasons about.
   * Nesting is the answer, and it costs nothing: an outer `@packed` struct lays a bitfield struct
   * out as an ordinary field of its size.
   *
   * **`volatile` on a bitfield used to be refused here and is not any more.** It means a volatile
   * access of the **container**: one volatile load to read a field, and one volatile load plus one
   * volatile store to write one, which is what C does with `volatile unsigned x : 3`. The hazard
   * that refusal was protecting is real and is now the driver author's to carry — a write to a
   * clear-on-read or write-1-to-clear register reads it first — but refusing it left the hardware
   * register this whole feature was asked for unable to use it, since `volatile` qualifies scalar
   * storage and so the struct could not carry the qualifier either.
   */
  private def checkBitfields(s: Type.Struct): Unit = {
    val stored = s.stored

    // Nothing is said about a struct one of whose fields did not resolve: it has been complained
    // about once already, and `Unknown` is not an integer, so every field after it would be too.
    if s.packed && !stored.exists((_, t) => Type.underlying(t) == Type.Unknown) &&
      stored.exists((_, t) => Bitfields.width(t).exists(_ % 8 != 0))
    then
      for (name, ty) <- stored do
        if Bitfields.width(ty).isEmpty then
          err(s"'${s.name}.$name' is ${show(ty)}, and '${s.name}' is '@packed' with a field narrower " +
            s"than a byte — so it is one integer, and every field of it has to be one too. Put the " +
            s"narrow fields in a '@packed' struct of their own and hold that here: it lays out " +
            s"identically and leaves ${show(ty)} what it is")
  }

  /** `@align(n)` folded to the boundary it names, with the two things that are not alignments
   * refused here rather than left to produce a layout nobody asked for.
   *
   * A **power of two** is what an alignment is, in the ABI and in LLVM both: an address is aligned
   * by having low bits clear, so a boundary of six is not a weaker claim than eight but an
   * unsatisfiable one. And a **non-constant** is refused because layout is fixed at compile time —
   * `reference/types.md § Structs` makes it part of the module's interface, which a value computed
   * at run time could not be.
   *
   * Whether it is *above* the natural alignment is not asked here: the floor is applied by taking
   * the larger of the two, so a struct that asks for less than its fields need simply keeps what
   * they need. That is a redundant annotation rather than a wrong one, and refusing it would make
   * `@align(8)` an error on a struct that happens to hold a pointer today and legal again tomorrow.
   */
  protected def alignBound(name: String, bound: Expr): Option[Int] =
    fold(bound) match
      case Some(IntLit(n, _)) if n > 0 && (n & (n - 1)) == 0 && n <= (1 << 29) => Some(n.toInt)
      case Some(IntLit(n, _)) =>
        err(s"'@align($n)' on '$name' is not an alignment — a boundary is a power of two, since an " +
          "address is aligned by having low bits clear")
      case _ =>
        err(s"'@align' on '$name' needs a constant — a literal, a 'const', or the arithmetic over " +
          "them. A layout is fixed while compiling and is part of what this module publishes, so " +
          "it cannot wait on a value")

  /** The one canonical tuple over these parts, registered so codegen lays its aggregate down.
   *
   * It is memoized beside the struct instantiations because it *is* one — `(int, string)` written
   * twice is one type, for the same reason `Box[int]` written twice is, and the field list is a
   * consequence of the parts rather than something a second instantiation could disagree about.
   */
  protected def tupleType(written: List[Type]): Type.Tuple =
    // Nameless for `instantiateStruct`'s reason: `(H, int)` over a second name for `u64` is `(u64, int)`.
    val parts = written.map(Type.nameless)
    // A tuple over a type **parameter** is not registered, and that is not an optimization. The key
    // is the type's spelling, and a parameter spells the same whatever an `impl` asked of it — so a
    // registered `(A, B)` would be handed back to the next block that wrote one, carrying the first
    // block's bounds. Nothing lays a value out at one of these, so nothing needs it kept.
    if parts.exists(Type.mentionsAbstract) then new Type.Tuple(parts)
    else
      val key = Type.qualified(Type.Tuple.base(parts.length), parts)

      structInsts.get(key) match
        case Some(t: Type.Tuple) => t
        case _ =>
          val t = new Type.Tuple(parts)

          structInsts(key) = t
          t

  /** A type with type parameters replaced by what a particular instantiation was made with — the
   * substitution `resolveType` performs on a *reference*, performed on a type that is already
   * resolved.
   *
   * It exists for one question: what an `impl` written with its own parameters promises about a
   * subject those parameters are bound in. `impl[T] Index[usize, T] for Buf[T]` stored `T` as a
   * trait argument, and asking what a `Buf[int]` implements means putting `int` there. Nothing
   * unresolved is reachable from here, so this is a walk rather than a resolution — a named type is
   * rebuilt through its instantiator so the result is the one canonical instantiation, and anything
   * with no parameter inside it comes back as itself.
   */
  protected def substParams(t: Type, subst: Map[String, Type]): Type =
    if subst.isEmpty then t
    else
      t match
        case a: Type.Abstract => subst.getOrElse(a.name, projected(a, subst))
        case n: Type.Named if n.targs.isEmpty => n
        case t: Type.Tuple    => tupleType(t.targs.map(substParams(_, subst)))
        case n: Type.Struct   => instantiateStruct(n.base, n.targs.map(substParams(_, subst)))
        case n: Type.Enum     => instantiateEnum(n.base, n.targs.map(substParams(_, subst)))
        case Type.Ptr(inner)      => Type.Ptr(substParams(inner, subst))
        case Type.Ref(inner, syn) => Type.Ref(substParams(inner, subst), syn)
        case Type.Weak(inner)     => Type.Weak(substParams(inner, subst))
        case Type.Array(n, elem)  => Type.Array(n, substParams(elem, subst))
        case Type.Slice(elem, ro) => Type.Slice(substParams(elem, subst), ro)
        case Type.CFn(ps, r)      => Type.CFn(ps.map(substParams(_, subst)), substParams(r, subst))
        case other                => other

  /** A **projection** whose subject the substitution moved: `V::Item` with `V` bound to `Buf[int]`
   * is `Buf[int]::Item`, which the implementation then says is `int`.
   *
   * This is the one place that knows a `Type.Abstract`'s name may have parts, and it is what makes
   * a projection cost no new type. Everything else — comparing two types, mangling one, refusing a
   * layout for one, caching an instantiation — sees an opaque parameter with an unusual name and
   * treats it exactly as it treats `T`.
   *
   * A subject the substitution does not move is left alone, which covers the ordinary case of a body
   * being checked at its own definition. And a projection the tables cannot answer is left abstract
   * rather than reported: the bound that licensed it was checked where it was written, so an
   * unanswerable one here means an error has already been raised somewhere with a position worth
   * printing.
   */
  private def projected(a: Type.Abstract, subst: Map[String, Type]): Type = {
    val cut = a.name.lastIndexOf("::")

    if cut < 0 then a
    else
      val base   = Type.Abstract(a.name.substring(0, cut), Nil)
      val member = a.name.substring(cut + 2)
      val moved  = substParams(base, subst)

      if moved == base then a else assocTypeOpt(moved, member).getOrElse(a)
  }

  /** Instantiates an enum for one set of type arguments. All-dataless variants make a *simple*
   * enum (integer constants, auto-incrementing from an optional explicit `= value`); any
   * data-carrying variant makes a *data* enum, whose variants take sequential tags and whose
   * payload-bearing variants each claim a slot in the aggregate.
   */
  protected def instantiateEnum(name: String, targs: List[Type]): Type.Enum =
    inDecl(name)(instantiateEnumIn(name, targs))

  private def instantiateEnumIn(name: String, written: List[Type]): Type.Enum = {
    if brokenDecls(name) then poisoned()

    val decl = enumDecls(name)
    checkArity(name, decl.tparams, decl.tdefaults, written)

    val targs = withDefaults(name, decl.tparams, decl.tdefaults, written, Map.empty).map(Type.nameless)

    checkTypeBounds(name, decl.tparams, targs)
    val key = Type.instanceKey(name, targs)

    enumInsts.get(key) match
      case Some(en) => en
      case None if inProgress.contains(key) =>
        cycleCheck(key)
        inProgress(key).asInstanceOf[Type.Enum]
      case None =>
        val en = new Type.Enum(name, targs)
        en.simple = decl.variants.forall(_.fields.isEmpty)
        inProgress(key) = en
        resolving(key) = Entered(indirection, typeArgDepth)

        // The `: iN` annotation pins a simple enum's storage; it is meaningless on a generic or
        // data enum, so those reject it rather than silently ignore it. Where present, every
        // discriminant is range-checked against it, which is the whole point of pinning the type.
        en.underlying = decl.underlying match
          case None => Type.Int
          case Some(ref) =>
            if !en.simple then
              err(s"only a simple enum has an underlying integer type — '${qn(name)}' carries data")
            resolveType(ref, Map.empty) match
              case i: Type.Integer => i
              case other           => err(s"an enum's underlying type must be an integer, not ${show(other)}")

        val subst    = decl.tparams.zip(targs).toMap
        var nextTag = 0
        try
          en.variants = decl.variants.map { v =>
            if en.simple then
              def fitting(n: BigInt): Int =
                if !Type.fits(n, en.underlying) then
                  err(s"discriminant $n of variant '${v.name}' does not fit ${show(en.underlying)}")
                n.toInt
              val tag = v.value match
                // A discriminant is any compile-time integer, so a negative one under a signed
                // underlying type folds from the unary negation the parser gives, and a `const` may
                // stand where a literal does (`reference/modules.md § const — a value`).
                case Some(e) =>
                  fitting(constInt(e).getOrElse(
                    err(s"the value of variant '${v.name}' must be a constant integer")))
                case None => fitting(nextTag)
              nextTag = tag + 1
              Type.EnumVariant(v.name, tag, Nil, false)
            else
              if v.value.isDefined then
                err(s"variant '${v.name}' carries data, so it cannot also have an explicit value")
              val tag    = nextTag; nextTag += 1
              val fields = v.fields.map(f => (f.name, recover(Type.Unknown)(resolveType(f.typ, subst))))
              Type.EnumVariant(v.name, tag, fields, fields.nonEmpty)
          }

          // A simple enum's value *is* its identity — there is nothing else to tell two variants
          // apart — so two names for one value leave the language unable to keep its own promises:
          // `Pos` and `Val` stop being inverses, the second variant's `match` arm can never run,
          // and `Image` lowers to a switch with a repeated case that the assembler rejects outright.
          // A data enum cannot reach this, since its tags are assigned in order and it refuses an
          // explicit value. Naming a value twice on purpose is what a `const` is for.
          if en.simple then
            val seen = mutable.Map.empty[Int, String]
            for v <- en.variants do
              seen.get(v.tag) match
                case Some(first) =>
                  err(
                    s"variants '$first' and '${v.name}' both stand for ${v.tag}, and a simple enum's "
                      + "values are the whole of what tells its variants apart — a second name for one "
                      + "value is a 'const'",
                  )
                case None => seen(v.tag) = v.name
        catch
          // Nothing a *simple* enum's variants read depends on the type arguments: a discriminant is
          // a constant and the underlying type is fixed at the declaration. So a mistake found here
          // belongs to the declaration rather than to this instantiation of it, and marking it as
          // such is what keeps a generic one — which has no eager instantiation to be judged at —
          // from repeating the same complaint at every use.
          //
          // Only a pass that *reports* may mark: the definition-time walk of a generic body raises
          // errors and drops them (`reference/generics.md § Bounds`), so marking from there would
          // retire the declaration before anything had told the reader about it.
          case e: AnalyzerError if en.simple && !abstractPass =>
            brokenDecls += name
            throw e
        finally
          resolving -= key
          inProgress -= key

        enumInsts(key) = en
        en
  }

  /** The call trait a value implements, where it implements one — supplied by `Closures`, which is
   * where a closure's own is registered.
   *
   * Declared here because `solve` reads a type argument back off a callable argument and this trait
   * sits above the one that knows how: the alternative is threading the answer through every call
   * site, which would put the knowledge of what a callable is into three places that do not need it.
   */
  protected def callableOf(t: Type): Option[Type.Bound]

  /** Matches a declaration's type reference against an actual type, binding whatever type
   * parameters it determines. Deliberately lenient: a structural mismatch simply leaves the
   * parameter unbound, and the argument is type-checked properly against the instantiated
   * signature afterwards, where the message can name both types.
   *
   * The actual type is matched nameless, so an exported `type R = *Vm` is read as the `*Vm` it is
   * by a `*T` parameter, and what a parameter binds is never a second name for a type.
   */
  protected def unify(
      ref: TypeRef,
      actual: Type,
      tparams: Set[String],
      sub: mutable.Map[String, Type],
  ): Unit = unifyNameless(ref, Type.nameless(actual), tparams, sub)

  private def unifyNameless(
      ref: TypeRef,
      actual: Type,
      tparams: Set[String],
      sub: mutable.Map[String, Type],
  ): Unit = ref match
    case PtrType(inner) =>
      actual match
        case Type.Ptr(t) => unify(inner, t, tparams, sub)
        case _           => ()
    // A `&T` parameter also accepts a bare `T`, which the call boxes, so the payload type is
    // matched against either shape.
    case RefType(inner, sync) =>
      actual match
        case Type.Ref(t, s) if s == sync => unify(inner, t, tparams, sub)
        case _: Type.Ref                 => ()
        case t                           => unify(inner, t, tparams, sub)
    // A `weak T` parameter also accepts a `&T`, which the call weakens, so the payload type is
    // matched against either shape — the same leniency `&T` gets one case up, for the same reason.
    case WeakType(inner) =>
      actual match
        case Type.Weak(t)  => unify(inner, t, tparams, sub)
        case Type.Ref(t, _) => unify(inner, t, tparams, sub)
        case _             => ()
    // Whether the view refuses writes is not matched here, for the reason `&T` and `weak T` above
    // do not match their own modes: this is where a type *parameter* is read off an argument, and
    // whether the argument may stand in the parameter's place is the coercion's question. Refusing
    // here would report a missing instantiation for what is really a write into read-only elements.
    case ArrayType(None, elem, _) =>
      actual match
        case Type.Slice(e, _) => unify(elem, e, tparams, sub)
        // **An array is matched too, because an array coerces to a slice at the call.** A `[]T`
        // parameter handed a `[3]int` is an ordinary call — a non-generic `[]const int` parameter
        // takes `[1, 2, 3]` and always has — so inference not following the coercion left `T`
        // unbound and answered a perfectly good call with "cannot infer the type argument". The
        // length is the array's own and binds nothing here; only the element is read.
        //
        // This is the leniency `&T` and `weak T` are given four cases up, for the same reason: each
        // matches the shapes a call will convert *to* its own, so that what the reader wrote is what
        // gets read. Following a coercion inference already permits is not widening what compiles —
        // the argument is still checked against the instantiated signature afterwards.
        case Type.Array(_, e) => unify(elem, e, tparams, sub)
        case _                => ()
    // The **length binds a value parameter** the way the element binds a type one
    // (`reference/generics.md § A parameter may stand for a value`): a `[N]T` parameter handed a
    // `[3]int` reads 3 off the argument's type, which is where the length already lives. Only a
    // bare name is read — a length written as arithmetic over a parameter, `[N + 1]T`, is refused
    // at resolution rather than solved here, since inverting an expression is the type-level
    // arithmetic that section excludes.
    case ArrayType(Some(len), elem, _) =>
      actual match
        case Type.Array(n, e) =>
          len match
            case Ident(v) if tparams(v) => sub.getOrElseUpdate(v, Type.ConstArg(n, Type.usize))
            case _                      => ()

          unify(elem, e, tparams, sub)
        case _ => ()
    // A vector's lane count binds a value parameter exactly as an array's length does, and this is
    // the case the whole feature turns on: `solve[const W: usize](vn: <W>f32)` handed a `<8>f32`
    // reads 8 off the argument and instantiates the kernel at eight lanes, with no width written at
    // the call. Nothing else lets one body serve every machine.
    case VectorType(lanes, elem) =>
      actual match
        case Type.Vector(n, e) =>
          lanes match
            case Ident(v) if tparams(v) => sub.getOrElseUpdate(v, Type.ConstArg(n, Type.usize))
            case _                      => ()

          unify(elem, e, tparams, sub)
        case _ => ()
    // A value argument written out fixes nothing. `Buf[4]` handed a `Buf[4]` has nothing to solve,
    // and handed anything else is a mismatch the instantiated signature reports in both types'
    // terms — which is where every other structural disagreement is reported.
    case _: ValueArgType     => ()
    case VolatileType(inner) => unify(inner, Type.unqualified(actual), tparams, sub)
    // `(..A)` binds the pack to **every** part at once, at whatever arity the argument has — which
    // is the whole of the inference this feature needs (`reference/generics.md § A parameter may
    // stand for a list of types`). It is `[N]T` reading a length off an argument one kind up:
    // nothing is written at the call, and a tuple of three parts and a tuple of five each solve the
    // one parameter.
    case TupleType(List(PackType(n)), _) =>
      actual match
        case t: Type.Tuple if tparams(n) => sub(n) = Type.Pack(t.targs)
        case _                           => ()
    case TupleType(parts, _) =>
      actual match
        case t: Type.Tuple if t.targs.length == parts.length =>
          parts.zip(t.targs).foreach { case (r, a) => unify(r, a, tparams, sub) }
        case _ => ()
    // A bare pack stands where no type does, so nothing here can be matched against it. Its own
    // resolution is what reports that, in the terms of what was written.
    case _: PackType => ()
    // A callable's parameters and result are matched through the trait they name, so a `&Fn(A) -> R`
    // parameter binds `A` and `R` from a `&Fn(int) -> bool` argument exactly as any other applied
    // trait binds its arguments. A closure's own type is a struct that says nothing about either, so
    // what settles a *bare* arrow's parameters is the call's own inference and not this.
    // **A closure arrives as its own struct, not as the call trait its parameter names**, so the
    // signature is asked of the implementation rather than matched against the type. That is what
    // lets `convert[T, U](x: T, f: &Fn(T) -> U)` read `U` off the closure's body — the boxed
    // spelling's half of what a bound's arguments do for the arrow's, and the only thing in the
    // call that knows what `U` is.
    case f: FnType =>
      callableOf(actual).filter(_.args.length == f.params.length + 1) match
        case Some(b) =>
          f.params.zip(b.args).foreach((r, a) => unify(r, a, tparams, sub))
          unify(f.ret, b.args.last, tparams, sub)
        case None => unify(f.asTrait, actual, tparams, sub)
    // A function pointer's parts bind exactly as a tuple's do — position by position, and only
    // against another one of the same width, since nothing else has parts to read.
    case CFnType(ps, r) =>
      actual match
        case Type.CFn(as, ar) if as.length == ps.length =>
          ps.zip(as).foreach((pr, a) => unify(pr, a, tparams, sub))
          unify(r, ar, tparams, sub)
        case _ => ()
    // A projection binds nothing, and that is the same rule a trait gets three lines down rather
    // than a limitation: solving `T` backwards from `T::Item` would need the implementation table
    // read in reverse, and more than one type can have the same associated type. The subject is
    // solved from wherever else it appears, and the projection then follows from it.
    case _: AssocType => ()
    // An object's binding is written rather than solved: it is part of the object *type*, so a
    // parameter named there is fixed by whoever wrote it and there is nothing to read backwards.
    case _: AssocArgType => ()
    // A `some` result stands in an `impl` block's member, which nothing calls generically.
    case _: SomeType  => ()
    // A trait never binds a type parameter. `f[T](p: *T)` handed a `*Writer` would otherwise
    // instantiate at a type with no layout, and the body could then write `var v: T` for a value
    // that cannot exist; leaving it unsolved reports the inference failure instead. A qualifier is
    // dropped on the way into a parameter, so `f[T](xs: []T)` handed a `[]volatile u32` solves `T`
    // as `u32` — and then the argument does not agree with the `[]u32` that instantiation asks for,
    // which is the message worth reading. Binding `T` to the qualified type instead would let a
    // generic body promise accesses it cannot promise: the loads and stores it emits are its own,
    // not the ones the caller wrote (`reference/memory.md § Device memory`).
    case NamedType(n, Nil) if tparams(n) =>
      if !sub.contains(n) && !actual.isInstanceOf[Type.Trait] then sub(n) = Type.unqualified(actual)
    // The reference is the declaration's, written in the declaration's terms, so the name it uses
    // is matched against the resolved type by the key it names here — a `Pair[T]` parameter is its
    // own module's `Pair`, whichever module the call that supplied the argument was written in.
    case NamedType(n, argRefs) =>
      val key = typeKey(n)

      actual match
        case s: Type.Struct if key.contains(s.base) && s.targs.length == argRefs.length =>
          argRefs.zip(s.targs).foreach { case (r, t) => unify(r, t, tparams, sub) }
        case e: Type.Enum if key.contains(e.base) && e.targs.length == argRefs.length =>
          argRefs.zip(e.targs).foreach { case (r, t) => unify(r, t, tparams, sub) }
        // A trait applied to arguments binds them the way a generic type does — what a trait cannot
        // do is bind a parameter to *itself*, which is the case above.
        case t: Type.Trait if traitKey(n).contains(t.name) && t.args.length == argRefs.length =>
          argRefs.zip(t.args).foreach { case (r, a) => unify(r, a, tparams, sub) }
        case _ => ()

  /** Solves a generic declaration's type arguments from the argument types, falling back to
   * the expected type of the whole expression for parameters the arguments do not determine.
   *
   * `soft` marks the arguments that are bare literals, and they are consulted **last**. A literal
   * has no type of its own (`01`) — it takes one from where it appears — so it is the weakest thing
   * in the room to conclude a type parameter from, and letting it go first is what made
   * `pick(1, 2, 250u8)` fix `T = int` and then reject the argument that actually knew. The order is
   * the operand rule's, one level up: what is already a type settles the parameter, and the
   * literals take what it settled to. They are still consulted, because with nothing else to go on
   * `id(7)` must remain an `int` rather than an inference failure.
   *
   * `known` carries the parameters that are not being inferred at all because something already
   * says what they are — a type's own arguments, read off the type an associated function was
   * reached through. Nothing infers over one: `unify` writes only where the map is silent, so a
   * seeded parameter stands however the arguments and the expected type read.
   */
  protected def solve(
      what: String,
      tparams: List[String],
      paramRefs: List[TypeRef],
      argTys: List[Type],
      resultRef: Option[TypeRef],
      expected: Option[Type],
      soft: List[Boolean] = Nil,
      bounds: Map[String, List[BoundRef]] = Map.empty,
      known: Map[String, Type] = Map.empty,
  ): List[Type] = {
    val sub   = mutable.LinkedHashMap.empty[String, Type]
    val tps   = tparams.toSet
    val pairs = paramRefs.zip(argTys).zip(soft.padTo(paramRefs.length, false))

    for (tp, t) <- known if tps(tp) do sub(tp) = t

    for ((r, t), adaptable) <- pairs if !adaptable do unify(r, t, tps, sub)
    if sub.size < tparams.length then
      for r <- resultRef; e <- expected do unify(r, e, tps, sub)
    if sub.size < tparams.length then
      for ((r, t), adaptable) <- pairs if adaptable do unify(r, t, tps, sub)
    // **What a callable argument yields is read back off the closure**, which is the other half of
    // `callBound` handing one an open result. The parameter itself has by now been solved to the
    // closure's own struct, and that struct implements the call trait its body determined — so the
    // bound's arguments, written in the declaration's terms, are matched against the ones the
    // closure turned out to have. `collect[T, U](xs: []const T, f: T -> U)` reads `U` here and
    // nowhere else, because nothing but the closure knows it.
    //
    // Last, and only while something is still missing, for the reason the literals go last: this
    // concludes a type parameter from a value that took its own shape from the context, so anything
    // that knew independently has already spoken.
    if sub.size < tparams.length then
      for
        (tp, bs) <- bounds
        t        <- sub.get(tp)
        ref      <- bs.find(b => Type.Fn.isCall(b.name))
        actual   <- callableOf(t)
        if ref.args.length == actual.args.length
        (r, a)   <- ref.args.zip(actual.args)
      do unify(r, a, tps, sub)

    // A parameter left unsolved because what would have solved it could not itself be worked out is
    // a consequence, not a mistake. `f() -> Result[unit, IoError] = Ok(())` with `IoError` never
    // imported has already been told that the name is unknown, with the caret under the name; the
    // return type it recovers to is `Result[unit, <unknown>]`, which then leaves `E` unsolved. Left
    // to report, that says "annotate the expected type" about a return type the reader annotated as
    // fully as they could, and points at the expression rather than at the word to change.
    //
    // Suppressing is safe by construction rather than by judgement: `Type.Unknown` exists only where
    // an error was already recorded, so there is no input that reaches here poisoned and silent.
    def unsolvable = expected.exists(Type.mentionsUnknown) || argTys.exists(Type.mentionsUnknown)

    tparams.map(tp =>
      sub.getOrElse(
        tp,
        if unsolvable then poisoned()
        else err(s"cannot infer the type argument '$tp' of '$what' here — annotate the expected type"),
      ),
    )
  }

  /** Whether a type has a zero value, which is what a declaration with no initializer starts
   * at. A reference has none — it always points at a live object — and neither does an enum,
   * whose zeroed tag names no variant in particular.
   */
  protected def hasZero(t: Type): Boolean = t match
    case _: Type.Integer | _: Type.Floating | Type.Char | Type.Bool | _: Type.Ptr => true
    // A `va_list` has no meaningful starting value — `va_start` is what makes it usable — but
    // `var ap: va_list` with no initializer is exactly how one is declared, so it zeroes like any
    // other slot and `va_start` overwrites whatever was there.
    case Type.VaList => true
    // A zeroed view owns nothing and names no elements, which is exactly the empty slice — and,
    // for a string, the empty string, which is well-formed UTF-8 the way anything empty is.
    case _: Type.View        => true
    // A weak reference that never had an object and one whose object is gone are the same state,
    // and a program can already reach the second — so the zeroed slot is not a value it would
    // otherwise be spared, it is the value `None` writes and `get()` reads back (`03`). This is the
    // one place `weak T` parts company with `&T`, which has no zero because there is no such thing
    // as a reference to nothing.
    case _: Type.Weak        => true
    // A zero-sized type has exactly one value, so it is trivially its own zero — there is nothing
    // to produce and nowhere to put it. Without this a struct would lose its zero value by gaining
    // a field that costs nothing, which is the opposite of what zero-sized means.
    case t if Type.zeroSized(t) => true
    case Type.Array(_, elem)    => hasZero(elem)
    // Every lane zeroed, which a lane always has: a lane is a scalar, and the four scalar kinds are
    // the first line of this match. `var v: <4>f32` is therefore the ordinary declaration it looks
    // like — and so is a `[2]<4>f32`, which is the case that found this missing.
    case Type.Vector(_, elem)   => hasZero(elem)
    case s: Type.Struct         => s.fields.forall(f => hasZero(f._2))
    case _                      => false
}
