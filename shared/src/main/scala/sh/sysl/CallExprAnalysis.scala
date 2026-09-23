package sh.sysl

/** A call, and the forms written like one.
 *
 * Split out of `ExprAnalysis`, whose expression dispatch calls in here. It is the largest run of
 * arms in that dispatch by a wide margin, and the one three separate features landed in this month.
 *
 * **The arms keep the order they had, and the order is the specification.** Several are correct only
 * because an earlier one has already claimed the shape — a name that is a type, a name that is a
 * local holding a callable, and a name that is a function all arrive as `Call(Ident(n), args)`, and
 * which of them it is, is decided by the sequence of guards rather than by any one of them. Nothing
 * here may be reordered, and a new arm goes where the run it belongs to already is.
 *
 * `LayoutOf` and `OffsetOf` are here because they sit inside that run: `sizeof(T)` is written like a
 * call and read like one, and the only thing separating it from the forms above it is that the
 * parser has already taken its operand as a type.
 */
trait CallExprAnalysis extends ExprCoercion with MemberExprAnalysis with RawStorage with Atomics {

  /** A call, or one of the two call-shaped forms, at the position the dispatch found it.
   *
   * `expected` is what the context wants, which a construction, a variant with a payload it does not
   * mention, and a generic call whose result alone is generic all need; the rest ignore it.
   */
  protected def callExpr(expr: Call | LayoutOf | OffsetOf, expected: Option[Type]): TExpr = expr match
    // The forms the compiler resolves by name rather than by looking a function up: `print` and
    // its two rendering companions, which are temporary and leave once a `Display` trait can carry
    // them, and the five primitives no sysl body could implement — the unchecked byte-to-string
    // conversion and the four a variadic body needs — which stay. What each one means is in
    // `SpecialForms`; the dispatch is here so it reads in the order the match tries.
    case Call(Ident("print"), args)                         => printCall(args)
    case Call(Ident("str"), args)                           => strCall(args)
    case Call(Ident("format"), List(argExpr, StrLit(spec))) => formatCall(argExpr, spec)
    case Call(Ident("str_cast"), args)                      => strCast(args)
    case Call(Ident("str_alias"), args)                     => strAlias(args)
    case Call(Ident("va_start"), args)                      => vaStart(args)
    case Call(Ident("va_end"), args)                        => vaEnd(args)
    case Call(Ident("va_arg"), args)                        => vaArg(args, expected)
    case Call(Ident("va_copy"), args)                       => vaCopy(args)
    case Call(Ident("ptr_cast"), args)                      => ptrCast(args, expected)

    // The atomic tier, which is the raw one — an address, values, and an ordering the call spells
    // out. The fence is separate because it reaches no address at all (`library/sync.md § A fence has no wrapper, and the omission is deliberate`).
    // Unlike the forms above, these stand aside for a declaration of the same name — nine names is
    // too much of a program's vocabulary to take outright (`Atomics.unclaimed`).
    case Call(Ident("atomic_fence"), args) if atomicFenceForm => atomicFence(args)
    case Call(Ident(name), args) if atomicForm(name)          => atomicCall(name, args)

    // `sizeof(T)` / `alignof(T)` / `offsetof(T, f)` — the parser has already read the operand as a
    // type, which is what separates these from every form above: they are syntax rather than a name
    // the analyzer knows.
    case LayoutOf(what, tr)                                 => layoutOf(what, tr)
    case OffsetOf(tr, field)                                => offsetOf(tr, field)

    // `old(e)` is a contextual keyword read only while an `ensure` is being analyzed; the guard is
    // what lets `old` stay an ordinary name outside a postcondition.
    case Call(Ident("old"), args) if oldBuf.isDefined       => oldCall(args)

    // A conversion is written with call syntax, so a type name in call position is one — a built-in,
    // or a **type parameter**, which every instantiation replaces with a type this same form would
    // have accepted written out. That is what makes the two directions symmetric: `u8(x)` where `x`
    // is a `T` was always ordinary code, checked once the width is concrete, and `T(b)` is the same
    // check at the same moment.
    //
    // A **parameter is asked about first**, ahead of every declaration table below, because it is
    // the nearer binding: `var y: T` inside a `[T]` body already means the parameter whatever else
    // is called `T`, and a name cannot mean the parameter in type position and a declaration in
    // call position. A built-in is asked about only where no declaration claims the name, which is
    // where it was asked before.
    //
    // **A declared FUNCTION claims the name too, which it did not until `funcInScope` was asked**,
    // and the asymmetry was a hole rather than a design: a declared *type* of one of these names
    // already won here, so `unit`, `int` and `byte` were words a module could take for a type and
    // not for a function. What a reader got instead was a diagnostic about a conversion they had
    // not written — *"a 'unit' conversion takes exactly one value"* for a nullary `unit()` — which
    // names a built-in they were not reaching for and sends them to count arguments. The conversion
    // is reachable in any case wherever the name is not a declaration's: that is what `u8(x)` is,
    // and a module that takes the name still spells it `u8` in every other file.
    case Call(Ident(name), args) if lookupOpt(name).isEmpty &&
        (tsubst.contains(name) ||
          (typeKey(name).isEmpty && !funcInScope(name) && scalarType(name).isDefined)) =>
      convertAt(typeNamed(name).get, name, args)

    // A bare variant name in call position — `Circle(3)` — with the enum taken from the expected
    // type, exactly as `Ident` above takes it for a nullary one.
    //
    // **A TYPE of the same name is asked about first**, which is what the guard is for. The two are
    // in different namespaces — a variant is a value name and a type is a type name — so a module
    // may declare both, and only the *call* has to choose between them. It chooses the way a bare
    // variant is resolved everywhere else: the expected type decides where it names the variant's
    // enum, and the type wins where it does not.
    //
    // **The asymmetry is the argument, rather than a preference for types.** A variant always has
    // the qualified `Enum.Variant` spelling, so standing aside costs it nothing it cannot get back;
    // a struct constructor is named by the struct's own name and has no second spelling at all. The
    // arm used to come first unguarded, which left such a struct impossible to construct by any
    // spelling — found from `box2d`, whose `ShapeKind` names five of the shapes it also declares
    // (card `0220`).
    //
    // **IT SITS ABOVE THE CONVERSION ARMS, AND THAT IS CARD `0295`.** `0220` put the guard on a
    // *struct* and left this arm below them, so an **alias** of a variant's name never reached it:
    // `type Eval = Result[Value, Signal]` beside a `StmtKind.Eval` made `stmt(…, Eval(target))` a
    // cast from an integer, refused for carrying data, at a call whose parameter already said
    // `StmtKind`. The expected type was there and nothing asked it. Being a *type* is what puts a
    // name here, so every kind of type is asked the same question in the same place.
    //
    // **The type is asked about with `typeInScope` rather than `typeKey`**, because this is the
    // compiler asking itself a question rather than resolving a name a file wrote — `typeKey` would
    // raise on a candidate the site may not name, and would file a module dependency for a
    // declaration the program never reached.
    //
    // **The key is `variantKeyFor` rather than `variantKey`, so that the expected type is consulted
    // across modules as well as within one** (card `0370`). A module declaring its own `Ok` made
    // every `Result` in it unwritable, because a bare name resolves to this module before the
    // library and nothing asked what the site wanted.
    case Call(Ident(name), args)
        if lookupOpt(name).isEmpty && variantKeyFor(name, expected).isDefined &&
          (!typeInScope(name) || variantEnumExpected(variantKeyFor(name, expected).get, expected).isDefined) =>
      constructVariant(variantKeyFor(name, expected).get, args, expected)

    // A constrained subtype's name in call position wraps a base value into the subtype, checking it
    // — `Age(n)`, `Meters(3.0)`. Unlike an implicit produce site, the cast is written, so it applies
    // even where the base would not flow in on its own.
    case Call(Ident(name), args) if lookupOpt(name).isEmpty && typeKey(name).exists(constrainedDecls.contains) =>
      constrainedCast(typeKey(name).get, args)

    // A **type parameter** in the same position, which reaches `Min` and `Max` and is told what the
    // rest are asked on. It comes first for the reason it does in `typeAttrExpr`: a body's own
    // parameter shadows anything a surrounding scope declares under that name.
    case Call(TypeAttr(Ident(name), attr), args) if lookupOpt(name).isEmpty && tsubst.contains(name) =>
      parameterAttr(name, tsubst(name), attr, args)

    // `T::Attr(x)` — a type attribute that takes an argument (`Valid`, `Succ`, `Pred`).
    case Call(TypeAttr(Ident(name), attr), args) if lookupOpt(name).isEmpty && typeKey(name).isDefined =>
      typeAttr(typeKey(name).get, attr, args)

    // An integer's attributes take no argument, so this reaches `integerAttr` only to be refused
    // there by name — which is a better answer than the generic "not callable" this would fall to.
    case Call(TypeAttr(Ident(name), attr), args) if lookupOpt(name).isEmpty && builtinInteger(name).isDefined =>
      integerAttr(builtinInteger(name).get, name, attr, args)

    // A struct's name in call position is its positional constructor. The variant arm above has
    // already stood aside or claimed the name, so nothing here has to ask about variants.
    case Call(Ident(name), args) if lookupOpt(name).isEmpty && typeKey(name).exists(structDecls.contains) =>
      constructStruct(typeKey(name).get, args, expected)

    // A simple enum's name in call position is a checked cast from an integer — `Color(n)` traps
    // on a value that is not a declared discriminant. Told from a data enum, which has no integer
    // to reinterpret, and from a struct constructor, which the arm above already claimed.
    case Call(Ident(name), args) if lookupOpt(name).isEmpty && typeKey(name).exists(enumDecls.contains) =>
      enumFromInt(typeKey(name).get, args)

    // A local that is callable is called, and it wins over a declaration of the same name for the
    // reason the nearest binding always does. It is asked whether it is callable rather than merely
    // whether it is a local, so a name that shadows a function with something uncallable still
    // reaches the function — which is what it did before there were closures, and no program that
    // relied on it is silently rerouted. A local holding C's function pointer, called through it.
    // It comes before the callable one because a `*extern` implements no call trait: there is no
    // receiver to pass and no table to read, only an address and the signature its type carried
    // (`reference/ffi.md § A function's address`).
    case Call(Ident(name), args) if lookupOpt(name).exists((_, t) => cfnOf(t).isDefined) =>
      callThroughAddress(analyzeExpr(Ident(name).setPos(expr.pos)), args)

    case Call(Ident(name), args) if lookupOpt(name).exists((_, t) => callableOf(t).isDefined) =>
      callCallable(analyzeExpr(Ident(name).setPos(expr.pos)), args, expected)

    // A nested function of an enclosing block (`reference/declarations.md`), which shadows a
    // top-level one of the same name for the reason the nearest binding always wins. The
    // environment travels as the receiver, so a sibling call and a recursive call are the same
    // call.
    case Call(Ident(name), args) if lookupOpt(name).isEmpty && nestedFuncs.contains(name) =>
      callNested(nestedFuncs(name), name, args)

    // A nested function of this block whose environment does not exist yet, for one of two reasons —
    // and they are different mistakes, so they get different sentences (`0224`).
    case Call(Ident(name), _) if lookupOpt(name).isEmpty && pendingNested.exists(_.name == name) =>
      // The group is waiting on a binding written below this call. The functions themselves are
      // above it, so "declared below this call" would be flatly false — what is below is the data.
      //
      // **All of them wait, not only the one that reads it**, and that is forced rather than
      // conservative: a block's nested functions share **one** environment, which is what lets them
      // call each other in either order, so there is nothing to pass to any of them until it is
      // built.
      if awaitingNeeds then
        val waiting = pendingNeeds.toList.sorted
        val which   = waiting.map(n => s"'$n'").mkString(", ")
        val it      = if waiting.length == 1 then "it" else "them"

        err(s"'$name' cannot be called here — the nested functions of this block share one " +
          s"environment, and it is not built until everything they read is bound. $which " +
          s"${if waiting.length == 1 then "is" else "are"} bound below this call: move the call " +
          s"below $it, or move $it above the functions")
      // The ordinary case: the call is written above the functions themselves.
      else
        err(s"'$name' is declared below this call — the nested functions of a block share an " +
          "environment formed where the first of them is written, so they may be called from there on")

    // One belonging to a body this one is written inside. A body reaches its own group and no
    // further, because what it would have to carry to reach further is the frame around it.
    case Call(Ident(name), _) if lookupOpt(name).isEmpty && outerNested(name) =>
      err(s"'$name' is a nested function of the body around this one, which reaches its own nested " +
        "functions and its own captures and no further — a top-level function is what several " +
        "bodies share")

    case Call(Ident(name), args) if funcKey(name).isDefined =>
      callOverloaded(funcKey(name).get, args, expected)

    // A name that is neither a local nor a function, holding a function pointer — a module-level
    // `val` is the one that reaches here, since it is resolved by neither of the two lookups above.
    // The general case further down would have taken it, but the complaint about an undefined
    // function comes first: a call head that is a *name* never gets that far (`reference/ffi.md § A
    // function's address`).
    case Call(Ident(name), args)
        if lookupOpt(name).isEmpty && probe(analyzeExpr(Ident(name).setPos(expr.pos)))
          .exists(t => cfnOf(t.ty).isDefined) =>
      callThroughAddress(analyzeExpr(Ident(name).setPos(expr.pos)), args)

    // The same, for a name holding a **callable** rather than an address. Module storage may hold one
    // (`reference/modules.md § val — a thing`), which is what a binding keeping a callback does — so `pending(n)` has to mean what
    // it would mean if `pending` were a local, and the general case below is unreachable from here
    // because the complaint about an undefined function comes first.
    case Call(Ident(name), args)
        if lookupOpt(name).isEmpty && probe(analyzeExpr(Ident(name).setPos(expr.pos)))
          .exists(t => callableOf(t.ty).isDefined) =>
      callCallable(analyzeExpr(Ident(name).setPos(expr.pos)), args, expected)

    // A local that is not callable, called anyway, is a different mistake from a name that stands
    // for nothing — the name was found, and what it holds is not a thing a call reaches.
    case Call(Ident(name), _) if lookupOpt(name).isDefined =>
      err(s"'$name' is ${show(lookupOpt(name).get._2)} and is not callable — a callable is a " +
        "closure, or a value of a type that implements the call trait")

    case Call(Ident(name), _) =>
      err(s"undefined function '$name'")

    // A member reached through the module it belongs to (`reference/modules.md § Imports`): the
    // chain is rewritten with the module folded into the name it qualifies, and what is left is the
    // ordinary form — a call, a construction, an associated function — resolved exactly as one
    // written unqualified is.
    case Call(callee, args) if throughModule(callee).isDefined =>
      analyzeValueAt(Call(throughModule(callee).get, args).setPos(expr.pos), expected)

    // `.Circle(5)`, `.make(2)` — the forms below with the type's own name left off, resolved
    // against what the context expects (`reference/expressions.md § A leading dot`).
    case Call(ImplicitMember(f), args) => implicitCall(f, args, expected)

    // Reached through the enum name: `Color.try(n)` is the fallible constructor; otherwise a
    // data-carrying variant `Shape.Circle(5)`, the qualified form of the bare `Circle(5)`, or an
    // associated function the enum declares, which resolves exactly as a struct's does.
    case Call(Field(Ident(written), mname), args)
        if lookupOpt(written).isEmpty && typeKey(written).exists(enumDecls.contains) =>
      val tname = typeKey(written).get

      if mname == "try" then enumTry(tname, args)
      else if enumDecls(tname).variants.exists(_.name == mname) then
        constructVariant(Modules.qualify(Modules.moduleOf(tname), mname), args, expected, Some(tname))
      else if memberDecls.contains((tname, mname)) then callAssociated(tname, mname, args, expected)
      else err(s"enum '${qn(tname)}' has no variant or associated function '$mname'")

    // `Type.name(…)` — an associated function, told from the positional constructor `Type(…)` by
    // the member selected from the type name rather than the bare name applied.
    case Call(Field(Ident(written), mname), args)
        if lookupOpt(written).isEmpty && typeKey(written).exists(structDecls.contains) =>
      callAssociated(typeKey(written).get, mname, args, expected)

    case Call(Field(Ident(written), mname), args)
        if lookupOpt(written).isEmpty && typeKey(written).exists(constrainedDecls.contains) =>
      val n = typeKey(written).get

      // A constrained subtype is a name a call reaches, so an `impl` for one may carry an associated
      // function exactly as a struct's may. Everything else selected from the name is one of the
      // mistakes `constrainedMember` has words for.
      //
      // **An alias is not one of those and is answered by its base** (`reference/errors.md §
      // Constrained types` — a transparent alias is the same type as its base), which is what the
      // *read* form already does one line into `constrainedMember`. Without it here the call fell
      // through to the read's complaint, and for an alias to a type with a written `impl` that
      // complaint is *"call it with 'F.zero()'"* under a line already reading `F.zero()`. An alias
      // to a **declared** type never arrives: those are followed at the key by `aliasedKey`, so
      // only one naming a scalar, a pointer, an array or a callable reaches this.
      if plainAlias(n) then callTypeAssociated(resolveAlias(n), written, mname, args, expected)
      else if memberDecls.get((n, mname)).exists(_.recvMode.isEmpty) then callAssociated(n, mname, args, expected)
      else constrainedMember(n, written, mname)

    case Call(Field(Ident(written), mname), _)
        if lookupOpt(written).isEmpty && typeKey(written).isEmpty && traitKey(written).isDefined =>
      traitMember(traitKey(written).get, mname)

    // `T.f(…)` and `real.f(…)` — an associated function reached through a type that is not one of
    // the declaration tables above: a type parameter, the `Self` a member's body is analyzed under,
    // or a built-in an `impl` was written for. It is the only way a bound says anything about the
    // type rather than about a value of it (`reference/traits.md § Reaching a trait's members
    // without a value`).
    case Call(Field(Ident(written), mname), args)
        if lookupOpt(written).isEmpty && typeKey(written).isEmpty && traitKey(written).isEmpty &&
          typeNamed(written).isDefined =>
      callTypeAssociated(typeNamed(written).get, written, mname, args, expected)

    // `Maybe[int].Just(1)` — the arguments written on the type a variant or an associated function is
    // selected *from*, which is the same rule one step to the left of the constructor below. Nothing
    // read it as a type, so the walk analyzed the brackets as an ordinary subscript and reported the
    // type's own name undefined: the one reading guaranteed not to help, since the name is defined
    // and is a type. Both spellings arrive as different nodes, one argument as an `Index` and a list
    // as a `TypeArgs`.
    case Call(Field(Index(Ident(written), targ), sel), args) if genericTypeName(written) =>
      typeArgsAtSelection(written, List(targ), sel, args)

    case Call(Field(TypeArgs(Ident(written), targs), sel), args) if genericTypeName(written) =>
      typeArgsAtSelection(written, targs, sel, args)

    case Call(Field(recv, mname), args) =>
      callMethod(recv, mname, args, expected)

    // `f[T](…)` — type arguments written at a call (`reference/generics.md § [] means type
    // application in a type, indexing in an expression`). The list and a subscript are one grammar,
    // so what tells them apart is not the parser: the name is resolved, and a **function** is not a
    // thing that can be indexed, so there is no second reading of the brackets to protect. That is
    // the discrimination `&f[T]` already made in order to refuse this by name, turned from a
    // refusal into a solve.
    //
    // The name has to be a declaration and nothing nearer: a local shadowing one is an ordinary
    // indexed value called through, and reading its author's subscript as a type argument would be
    // worse than any message. That is the same shadowing test every call form above makes. It is
    // tested on being a *function* rather than a generic one, so `plain[i32](3)` is owed the message
    // that `plain` has no type arguments rather than a general complaint about callables.
    //
    // Both spellings arrive, and as different nodes: one argument is an ordinary `Index` and a list
    // is a `TypeArgs`, which is the split `&f[T]` against `&f[A, B]` already lives with.
    case Call(Index(Ident(name), targ), args) if lookupOpt(name).isEmpty && funcKey(name).isDefined =>
      callOverloaded(funcKey(name).get, args, expected, List(targ))

    case Call(TypeArgs(Ident(name), targs), args) if lookupOpt(name).isEmpty && funcKey(name).isDefined =>
      callOverloaded(funcKey(name).get, args, expected, targs)

    // The same, written **qualified**. A module path is folded into the name by `qualifiedFunc`,
    // exactly as it is at an address, so `mod.f[T](x)` resolves wherever the unqualified spelling
    // does rather than falling to the general complaint about a callee that is not a name.
    case Call(Index(e, targ), args) if qualifiedFunc(e).isDefined =>
      callOverloaded(qualifiedFunc(e).get._2, args, expected, List(targ))

    case Call(TypeArgs(e, targs), args) if qualifiedFunc(e).isDefined =>
      callOverloaded(qualifiedFunc(e).get._2, args, expected, targs)

    // `x.m[T](…)` — the same list on a **method**, and the one head where the second reading is
    // live: `x.handlers[i](…)` is a field holding a table of callables, indexed and called, which
    // is an ordinary thing to write. So the discrimination is stricter than the free function's — the
    // receiver is analyzed and asked whether it declares a *method* of that name, rather than a name
    // being looked for among the declarations of every type in the program. A field wins, because a
    // field is what the subscript would have been reaching into.
    case Call(Index(Field(recv, mname), targ), args) if methodWritten(recv, mname) =>
      callMethod(recv, mname, args, expected, List(targ))

    case Call(TypeArgs(Field(recv, mname), targs), args) if methodWritten(recv, mname) =>
      callMethod(recv, mname, args, expected, targs)

    // `Pair[K, V](…)` — the same list at a **constructor**, which is the other half of the call head.
    // A type applied to arguments is what a type argument list *is* everywhere else in the language,
    // so this is the spelling with the least to learn, and it means what the annotation means: the
    // instantiation is fixed and the arguments are checked against it rather than solving it.
    //
    // Both spellings arrive, and they arrive as different nodes: one argument is an ordinary `Index`
    // and a list is a `TypeArgs`, which is the split `&f[T]` against `&f[A, B]` already lives with.
    // The shadowing test is every other call form's — a local standing over the type's name is an
    // ordinary indexed value, and reading its author's subscript as a type argument would be worse
    // than any message.
    // Two cases rather than one alternative, because a pattern alternative may bind no variable.
    case Call(Index(Ident(written), targ), args) if genericTypeName(written) =>
      constructWritten(written, List(targ), args)

    case Call(TypeArgs(Ident(written), targs), args) if genericTypeName(written) =>
      constructWritten(written, targs, args)

    // A special form written with type arguments. **`va_arg[int](ap)` is the one this is for**, and
    // `reference/ffi.md § Variadic functions` named it as the strongest case for the syntax:
    // everywhere else the annotation that stands in is a word on a binding that was going to be
    // written anyway, and a variadic body reading its tail straight into `print` has no binding at
    // all. `ptr_cast[T](p)` is the same shape from the raw tier — both take their type from what
    // receives the value, so writing it here is writing it where the value is made.
    //
    // The rest take none, for the reason a non-generic function takes none: there is nothing for an
    // argument to be an argument *of*.
    case Call(Index(Ident(name), targ), args) if lookupOpt(name).isEmpty && specialFormNames(name) =>
      val written = Some(rt(typeArgWritten(targ, atCall = true)))

      name match
        case "va_arg"   => vaArg(args, written)
        case "ptr_cast" => ptrCast(args, written)
        case _          => err(s"'$name' takes no type arguments")

    // Anything that *is* a callable may be called, wherever it was read from — an element of an
    // array of them, a part of a tuple, a container's item (`reference/types.md § Function types`).
    // The head of a call is looked at rather than required to be a name, and only what turns out
    // not to be callable is refused. A function pointer read from wherever one was kept — a
    // struct's field, an element of a table of handlers, what another call handed back
    // (`reference/ffi.md § A function's address`).
    case Call(callee, args) if probe(analyzeExpr(callee)).exists(t => cfnOf(t.ty).isDefined) =>
      callThroughAddress(analyzeExpr(callee), args)

    case Call(callee, args) if probe(analyzeExpr(callee)).exists(t => callableOf(t.ty).isDefined) =>
      callCallable(analyzeExpr(callee), args, expected)

    case Call(_, _) =>
      err("the thing being called must be a name, or something whose type says it is callable")

  /** Whether a written name is a **generic** nominal type — a struct's or an enum's — and is not
   * standing behind something nearer.
   *
   * The shadowing test is the one every call form makes: a local holding a value of that name is an
   * ordinary subscript, and its author never wrote a type argument to be told about.
   */
  protected def genericTypeName(written: String): Boolean =
    lookupOpt(written).isEmpty && typeKey(written).exists(k => nominalTparams(k).nonEmpty)

  /** Whether `recv.mname` names a **declared method** of the receiver's own type, which is what
   * decides that a bracket after it is a type-argument list rather than a subscript.
   *
   * It is the strictest of the four guards, and it has to be: a field may hold an array of callables,
   * so `x.handlers[i](…)` is a reading the language already gives and this must not take. Asking the
   * receiver settles it — a field is not a member — where asking whether *any* type in the program
   * declares a generic member of that name, which is what the refusal this replaces did, would have
   * answered yes for a field whose name some unrelated type happened to share.
   *
   * A receiver reached through a bound, a trait object or a weak reference is left out: each has a
   * dispatch of its own that never sees a written list, so a guard that admitted one would accept
   * the brackets and silently drop them.
   */
  private def methodWritten(recv: Expr, mname: String): Boolean =
    probe(analyzeExpr(recv)).map(t => receiverType(t.ty)).exists {
      case _: Type.Abstract | _: Type.Trait | _: Type.Weak => false
      case rty =>
        val (base, _) = memberKey(rty, mname)

        memberDecls.get((base, mname)).exists(_.receiver.isDefined)
    }

  /** `Pair[K, V](…)` — a construction whose instantiation is written rather than inferred.
   *
   * It is the annotation's meaning moved to the constructor: the type is resolved from the name and
   * the arguments in the brackets, and the ordinary construction is then asked for exactly that
   * type. So `Pair[int, real](1, 2)` and `var p: Pair[int, real] = Pair(1, 2)` build the same value
   * and refuse the same mistakes, and a literal in the arguments is read at the parameter the
   * written instantiation gave it rather than at its own default.
   *
   * An **enum** reaches here too, since a name applied to arguments is one grammar — and a bare enum
   * name is not a constructor at all, so what it is owed is the sentence about variants rather than
   * a type it cannot build.
   */
  private def constructWritten(written: String, targs: List[Expr], args: List[Expr]): TExpr = {
    val ty = rt(NamedType(written, targs.map(typeArgWritten(_, atCall = true))))

    typeKey(written) match
      case Some(k) if structDecls.contains(k) => constructStruct(written, args, Some(ty))
      case _ =>
        err(s"'$written' is an enum, so it is not built by calling its name — a variant is what " +
          s"carries a value, as '$written[…].Name(…)'")
  }

  /** The same list one step to the left: written on the type something is selected *from*, which is
   * what a reader writes to say which instantiation a **variant** belongs to — `Maybe[int].Just(1)`,
   * and `Maybe[int].Nothing` with nothing called at all.
   *
   * A variant is a construction of the type it belongs to, so the written arguments mean here what
   * they mean at a constructor: the instantiation is fixed and the payload is checked against it.
   *
   * **An associated function is not that**, and keeps the refusal. Its instantiation is solved from
   * the call — the type's parameters and its own arrive in one list and are read together
   * (`reference/generics.md § Inference is bidirectional`) — so honouring the brackets would mean
   * settling half of that list and solving the rest, which is a different question from the one
   * this form asks. The annotation on the binding reaches it, and unlike the corner a call head
   * could not reach, it is always there: an associated function has a result, and the result is
   * what its type arguments are read off.
   */
  protected def typeArgsAtSelection(
      written: String,
      targs: List[Expr],
      sel: String,
      args: List[Expr],
  ): TExpr = {
    val tname = typeKey(written).get

    if enumDecls.get(tname).exists(_.variants.exists(_.name == sel)) then
      constructVariant(Modules.qualify(Modules.moduleOf(tname), sel), args,
        Some(rt(NamedType(written, targs.map(typeArgWritten(_, atCall = true))))), Some(tname))
    else
      err(s"'$written' cannot be given type arguments where '$sel' is selected from it; write the " +
        s"type on what receives the result — 'var x: $written[…] = …' — and select '$sel' from the " +
        s"plain name")
  }

  /** `old(e)` — the value `e` had at function entry. It is analyzed in the entry scope the `ensure`
   * runs in (parameters, but no body locals, which are not in scope yet), then recorded in the
   * `old` buffer so codegen snapshots it before the body runs. The position it took is what the
   * postcondition reads back.
   */
  protected def oldCall(args: List[Expr]): TExpr = {
    val e = args match
      case List(one) => one
      case _         => err(s"'old' takes exactly one argument, but got ${args.length}")

    val te = analyzeExpr(e, None)
    if te.ty == Type.Unit then err("'old' needs a value to remember, but its argument is unit")

    val buf = oldBuf.get
    val idx = buf.length
    buf += te
    TOld(idx, te.ty)
  }
}
