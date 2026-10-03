package sh.sysl

/** What an expression means, and which expressions denote a **place**.
 *
 * This is the dispatch every other analysis trait calls back into — `analyzeExpr` is declared
 * abstract in `AnalyzerBase` precisely so that a call, a pattern, or a statement can analyze its
 * parts without knowing which of the forms below they turn out to be. The forms that need a table
 * or a machinery of their own are delegated: a call goes to `CallAnalysis`, a `match` to
 * `PatternAnalysis`, a block to `StmtAnalysis`, and the handful the compiler resolves by name to
 * `SpecialForms`.
 *
 * **Four groups of forms live in traits of their own**, and the dispatch below sends each group
 * whole: a call and the two forms written like one to `CallExprAnalysis`, reading a member to
 * `MemberExprAnalysis`, building or indexing a sequence to `CollectionExprAnalysis`, and control
 * flow used as an expression to `ControlFlowExprAnalysis`. Each group's own arms keep the order they
 * had, which is what makes the split safe to read: an arm can only ever be shadowed by an arm
 * matching the same node type, and every group is a set of node types nothing else here matches.
 * That is the rule to keep a new group to, and it is the only thing standing between a split of this
 * match and a silent change of meaning. What more than one group needs is in `ExprSupport`.
 *
 * **The expected type is a layer rather than a group**, and it is `ExprCoercion`: `analyzeExpr`
 * consults it before the dispatch below runs and again on what comes back.
 *
 * What stays is the dispatch itself and the forms with nowhere else to be: literals, names,
 * operators, assignment, and places. Places are here rather than beside assignment because what
 * makes an expression a place is a property of the expression: a local, a dereference, a field of
 * either, and an element of a slice have an address, and anything computed does not.
 */
trait ExprAnalysis
    extends CallExprAnalysis
    with MemberExprAnalysis
    with CollectionExprAnalysis
    with ControlFlowExprAnalysis
    with RawStorage
    with Atomics {

  // --- expressions ---------------------------------------------------------------------

  protected def analyzeBool(e: Expr): TExpr = {
    val t = analyzeExpr(e, Some(Type.Bool))
    if t.ty != Type.Bool then err(s"condition must be bool, got ${show(t.ty)}")
    t
  }

  /** Analyzes an expression. `expected` is the type the context wants, used where the
   * expression cannot determine its own type arguments — a bare `None`, an `Ok(v)` whose error
   * type is not mentioned by its argument, a generic call whose result alone is generic — and
   * where it decides that a value belongs on the heap.
   *
   * A context expecting `&T` asks the expression for a `T` and boxes what comes back, so
   * writing the ordinary construction is the whole spelling of an allocation. An expression
   * that is already a `&T` passes through untouched.
   */
  protected def analyzeExpr(expr: Expr, expected: Option[Type], discarded: Boolean): TExpr = {
    // Whether *this* expression is one of the three places a result list may stand
    // (`reference/declarations.md § Several results`). Taking the flag before anything below runs
    // is what confines it to one expression: every subexpression is analyzed through this same
    // funnel and sees it already spent.
    val allowed = multiOk
    multiOk = false

    // A discarded expression is by definition one nothing was expected of, so there is no context
    // to push down and the conversion cases below have nothing to say — what the flag carries is
    // the *absence* of a consumer, which only the branching forms act on.
    val raw = at(expr.pos)(
      if discarded then analyzeValue(expr, None, discarded = true) else analyzeExpected(expr, expected),
    ).setPos(expr.pos)

    // A result list is unwrapped here into the tuple its parts lay out as, so nothing downstream —
    // no other analysis, no pass, no emitter — ever holds one. Where a list is *not* allowed, this
    // is the one place that knows it, and it is also the place with the whole expression to name.
    val t = raw.ty match
      case r: Type.Results if allowed => retyped(raw, r.parts)
      case r: Type.Results =>
        at(expr.pos)(err(s"this yields ${quantity(r.parts.targs.length, "result")}, and one value is " +
          "wanted here — a result list is taken apart by a binding or an assignment that names " +
          "every one of them"))
      case _ => raw

    // A value whose type could not be worked out — a name whose declaration failed, a field of a
    // type that did not resolve, a call to a function with an unusable signature — abandons this
    // statement quietly. The mistake was reported where it was made, and every consequence of it
    // reported as well would bury the one diagnostic worth reading.
    if t.ty == Type.Unknown then poisoned()

    t
  }

  /** Analyzes one expression in a place a **result list** may stand (`reference/declarations.md §
   * Several results`): the right side of a binding, the right side of a multiple assignment, and
   * the result of a function whose own declared result is a list. The permission covers this
   * expression and nothing inside it.
   */
  protected def analyzeMulti(expr: Expr, expected: Option[Type] = None): TExpr = {
    multiOk = true

    try analyzeExpr(expr, expected)
    finally multiOk = false
  }

  /** The same call, typed at the tuple its result list lays out as. Only a call can carry a result
   * list — nothing else reads a signature — so those are the two shapes there are.
   */
  private def retyped(t: TExpr, parts: Type.Tuple): TExpr = t match
    case c: TCall  => c.copy(ty = parts, results = true).setPos(t.pos)
    case c: TVCall => c.copy(ty = parts, results = true).setPos(t.pos)
    case other     => sys.error(s"a result list arrived on a ${other.getClass.getSimpleName}")

  /** What a reserved identifier stands for, folded into the use as the literal it names
   * (`ReservedNames`).
   *
   * The location three of them report — and the place whose package `__NAME__` and `__VERSION__`
   * read — is
   * `reportedPos`, which is the node's own place everywhere
   * except while a parameter's default is being filled in — there it is the **call**, because a
   * default stands exactly where the argument would have been written (`reference/declarations.md §
   * Default parameters and named arguments`). That one substitution is the whole mechanism behind a
   * checking function that names its caller's line without any caller having written one down, and
   * it is why sysl needs no `#[track_caller]`: the call-site behaviour falls out of what a default
   * already was.
   *
   * `__LINE__` and `__COLUMN__` go through `intLiteral`, so each takes the integer type its context
   * asks for and is range-checked like any other literal — a parameter declared `i32` gets an `i32`,
   * and one declared `u8` is told where a line number will not fit rather than wrapping.
   */
  private def builtin(name: String, expected: Option[Type]): TExpr = {
    def where: Pos = reportedPos.getOrElse(
      err(s"'$name' reports where it is written, and this is a node with no place in any file"))

    name match
      case "__FILE__" => TStrLit(where.source.name)
      case "__LINE__" => intLiteral(BigInt(where.line), None, expected)
      // The column of the **file**, which is not the one the lexer counted when the text it lexed had
      // its left margin taken off: a literate program's code sits four columns in (`Source`).
      // `Pos.location` adds the offset back for the same reason, and the two have to agree — a
      // diagnostic and a program that disagreed about one place would be worse than either alone.
      case "__COLUMN__" => intLiteral(BigInt(where.col + where.source.columnOffset), None, expected)
      // **Empty outside any body, rather than an error.** A module's storage is filled before any
      // function runs, so there is genuinely no function to name there — and refusing it would also
      // refuse a *default* of `__FUNCTION__`, which is checked once at its declaration where there
      // is no caller yet and is the one place this is most worth writing. An empty string is the
      // honest answer to "which function is this"; a stale one would not be, and was the bug.
      case "__FUNCTION__" => TStrLit(currentFunctionName)
      case "__DATE__" => TStrLit(ReservedNames.date(ReservedNames.stamp))
      case "__TIME__" => TStrLit(ReservedNames.time(ReservedNames.stamp))
      // **The package of the place it reports, as `__FILE__` is that place's file.** Written in a
      // body it is the package that holds the text; written as a default it is the package of the
      // call the default is filled at — so a library's `version: string = __VERSION__` answers the
      // version of whichever program called it, which is the one a caller writing it would mean.
      case "__VERSION__" => TStrLit(packageVersion(reportedPos))
      // The package's `name`, by the same road: a library's `tag: string = __NAME__` default names
      // whichever package called it, so a logging call made from an application is tagged with it.
      case "__NAME__" => TStrLit(packageName(reportedPos))
      case _          => err(ReservedNames.unknown(name))
  }

  protected def analyzeValueAt(expr: Expr, expected: Option[Type], discarded: Boolean): TExpr = expr match
    case IntLit(v, suffix)   => intLiteral(v, suffix, expected)
    case FloatLit(t, suffix) => floatLiteral(t, suffix, expected)
    case CharLit(cp)         => TIntLit(cp, Type.Char)
    case StrLit(s)           => TStrLit(s)
    // A C callee finds the end by the terminator, so an interior NUL would hide everything written
    // after it. Refused outright rather than silently truncated — an ordinary `"a\0b"` is unaffected,
    // since carrying a length is exactly what lets it hold one.
    case CStrLit(s) =>
      if s.indexOf(0) >= 0 then
        err("a C string ends at its first NUL, so it cannot contain one — the bytes after it could never be read")
      TCStrLit(s)
    case BoolLit(b)          => TBoolLit(b)
    case UnitLit()           => TUnitLit()

    case NullLit() =>
      expected match
        case Some(p: Type.Ptr) => TNullLit(p)
        // The null callback, which several C interfaces read as "there is none, use the default" —
        // `signal(SIG_DFL)`, an `atexit` slot never filled, a `*_set_callback(0)`. It is an address
        // like any other and the same word says it is absent.
        case Some(c: Type.CFn) => TNullLit(c)
        case Some(other)       => err(s"'null' is a raw pointer, and ${show(other)} was expected here")
        case None              => err("'null' takes its type from its context, and there is none here")

    // A minus and the literal it precedes are one unit for the range check, so a signed type's
    // minimum is writable even though its magnitude overflows the positive range.
    case Unary("-", IntLit(v, suffix))   => intLiteral(-v, suffix, expected)
    case Unary("-", FloatLit(t, suffix)) => floatLiteral("-" + t, suffix, expected)

    // A reserved identifier is the compiler's to answer and no scope is consulted: the shape may not
    // be declared at all (`ReservedNames`), so there is nothing a lookup could find and nothing that
    // could shadow one. That is the difference between these and `result` below, which is a
    // *contextual* keyword precisely because an ordinary binding of that name is allowed to win.
    case Ident(name) if ReservedNames.shaped(name) => builtin(name, expected)

    // `result` is a contextual keyword: it names the returned value inside an `ensure`, but a
    // real binding of that name (a parameter or local) still shadows it, so the lookup comes first.
    case Ident("result") if lookupOpt("result").isEmpty =>
      ensureResultTy match
        case Some(ty) => TResult(ty)
        case None     => err("'result' is only meaningful inside an 'ensure' of a value-returning function")

    // A weak reference whose object is gone and one that never had an object are the same state,
    // so the empty weak reference is spelled the way that state reads everywhere else: `None`, the
    // very thing `get()` will hand back for it (`03`).
    case Ident("None") if lookupOpt("None").isEmpty && expected.exists(_.isInstanceOf[Type.Weak]) =>
      TZero(expected.get)

    // A declared function standing where a callable is wanted is one, with nothing captured
    // (`reference/expressions.md § Closures`). It is asked for only where the context says a
    // callable, so a bare function name anywhere else is still the mistake it was.
    case Ident(name)
        if lookupOpt(name).isEmpty && !ownValueName(name) && funcKey(name).isDefined &&
          expected.flatMap(callableSignature).isDefined =>
      val (ptypes, result) = expected.flatMap(callableSignature).get

      functionAsCallable(name, ptypes, result, expr.pos)

    // A nested function is **called** where it is written and is not a value
    // (`reference/declarations.md`). Its environment is a row of addresses into the frame it was
    // declared in, which is sound exactly because nothing can carry it out of that frame — and a
    // callable value is a way of carrying it.
    case Ident(name)
        if lookupOpt(name).isEmpty && (nestedFuncs.contains(name) || outerNested(name)) =>
      err(s"'$name' is a nested function, so it is called where it is written rather than passed — " +
        "its environment is the frame it was declared in, and a callable value is a way of carrying " +
        s"it out. Something that has to be passed is a closure of its own: 'var $name = x -> …'")

    // A declared function named where nothing wants a callable. The name is not undefined — the
    // declaration is right above — and saying so sends the reader hunting for a typo instead of at
    // what is really missing: a context that says which call trait to build the function into
    // (`§5`, `§6`).
    //
    // **The other thing it may be is an address**, and the two are deliberately different spellings:
    // a bare name is the capture-free closure and `&name` is the address of code compiled to C's
    // convention (`§6a`). Where the context asks for one of those outright — a `pthread_create`, a
    // `qsort`, any interface that calls back — the missing `&` is the whole of the mistake, so the
    // message names it rather than the two callable forms the reader did not want.
    case Ident(name) if lookupOpt(name).isEmpty && !ownValueName(name) && funcKey(name).isDefined =>
      // **The name is quoted through `qn`, because it may be a key rather than what anybody wrote.**
      // A qualified path is folded into one name before it reaches here (`throughModule`), so a
      // reader who wrote `c.less` was being shown `c$less` — and then told to write `'&c$less'`,
      // which carries the module separator and is not sysl. `qn` is where every message naming a
      // declaration by its key turns it back into the path a reader would type.
      val shown = qn(name)

      err(
        if expected.exists(t => cfnOf(t).isDefined) then
          s"'$shown' is a function, and what is wanted here is the address of one — write '&$shown'. " +
            "A bare name is the capture-free closure, which has no address a C interface could call"
        else
          s"'$shown' is a function, and a function becomes a value only where a callable is wanted — " +
            "a bare-arrow parameter, or a '&Fn' where a concrete type is required. Nothing here asks " +
            s"for one; where the address of code is what is wanted, that is written '&$shown'",
      )

    /** A **value parameter** (`reference/generics.md § A parameter may stand for a value`), folded
     * into its use exactly as a declared constant is — which is what it is, a `const` whose value
     * the instantiation supplied. The substitution holds a `ConstArg` for it wherever the body is
     * walked: the real argument at an instantiation, and a zero placeholder during the walk that
     * checks the generic body, where there is no argument yet and the tree built is discarded. An
     * array length written `[sizeof(T)]u8` already stands at zero for that same walk and for the
     * same reason.
     *
     * **A local of the same name still wins**, which is why the scope is asked first: a parameter is
     * the outermost binding of its name, not the only one.
     */
    case Ident(name)
        if lookupOpt(name).isEmpty && tsubst.get(name).exists(_.isInstanceOf[Type.ConstArg]) =>
      val c = tsubst(name).asInstanceOf[Type.ConstArg]

      c.ty match
        // A **simple enum's** argument travels as its tag, and what the body wants back is the
        // variant — so the name is handed to the ordinary variant path rather than reconstructed
        // here, and everything a written `Fast` gets (its type, its scope, its exhaustiveness)
        // follows from that.
        case en: Type.Enum =>
          en.variants.find(_.tag == c.value.toInt) match
            case Some(v) => analyzeExpr(Ident(v.name), Some(en))
            case None    => err(s"'$name' stands for no variant of ${show(en)}")
        case ty => analyzeExpr(constArgLiteral(c), Some(ty))

    case Ident(name) =>
      lookupOpt(name) match
        // A by-name parameter is read by **calling** it: the caller wrote an expression and the
        // compiler made it a nullary closure, so naming it here is where that closure runs. Doing it
        // at the read is what gives the feature its defining behaviour — each use is an evaluation,
        // because each use is a call — and it costs nothing else, since a call on a callable is the
        // ordinary method call `callCallable` already performs.
        case Some((u, ty)) if byNameLocals(u) =>
          callCallable(capturedFields.getOrElse(u, TLoad(u, ty)), Nil, expected)
        // A captured name is a name the scope knows and the frame does not hold: what it reaches is
        // the field of the closure the body is now a member of (`reference/expressions.md §
        // Closures`).
        case Some((u, ty)) => capturedFields.getOrElse(u, TLoad(u, ty))
        case None =>
          // `variantKeyFor` rather than `variantKey`: the expected type picks the enum across
          // modules as well as within one, so a bare `None` still means `Option`'s in a module that
          // declares a `None` of its own (card `0370`).
          variantKeyFor(name, expected) match
            case Some(key) => constructVariant(key, Nil, expected)
            case None =>
              // A constant is folded into its use and analyzed as the literal it stands for, at the
              // type it was declared with rather than the one the context asked for. That is what
              // makes it behave like the value it names: `const n: usize = 5` used where an `int`
              // belongs is the same mismatch a `usize` variable would be, not a silent adaptation.
              constKey(name) match
                case Some(key) => analyzeExpr(constLiteral(key), Some(constType(key)))
                // A `val` is the other half of that: nothing is folded, because it is storage. The
                // name reaches the storage itself, which is why it can be indexed and iterated.
                case None =>
                  globalKey(name) match
                    case Some(key) => TGlobal(key, globalType(key), globalWritable(key))
                    // An `extern` variable is storage too, so it becomes the same node — under the
                    // **symbol** rather than the key, since what it names is what the linker
                    // supplies, and writable, since the storage is not this program's to promise
                    // anything about (`reference/ffi.md § An extern also declares a variable`).
                    case None if externVarKey(name).isDefined =>
                      val key = externVarKey(name).get

                      externVarsUsed += key
                      TGlobal(externVarDecls(key).symbol, externVarType(key), writable = true)
                    // A name the block binds is a different mistake from one that stands for
                    // nothing, and the difference is what the reader has to fix.
                    case None if blockDeclares.contains(name) => notYetBound(name)
                    case None => unresolvedErr(s"undefined name '${qn(name)}'")

    case Binary(op @ ("&&" | "||"), l, r) =>
      TLogical(op, analyzeBool(l), analyzeBool(r))

    case Binary(op, l, r) =>
      val List(tl0, provisional) = analyzeOperands(List(l, r), expected.filter(Type.computesNumerically))
      val (tl, tr)               = balanceLanes(tl0, operandRhs(op, tl0, r, provisional))

      operatorCall(op, tl, tr).getOrElse(produced(TBinary(op, tl, tr, arithType(op, tl.ty, tr.ty, tr.pos))))

    // The operand's *base* decides which operators there are — a subtype narrows which values a type
    // has, never which operations it has — so the match reads through it and the node is typed by
    // `unaryType`, which keeps a derived result in its own type.
    case Unary("-", e) =>
      val t = analyzeExpr(e, expected.filter(Type.computesNumerically))
      prefixCall("-", t).getOrElse(Type.opSubject(t.ty) match
        case i: Type.Integer if i.signed => produced(TUnary("-", t, unaryType(t.ty)))
        case _: Type.Floating            => produced(TUnary("-", t, unaryType(t.ty)))
        case _: Type.Integer => err(s"unary '-' is not defined for the unsigned type ${show(t.ty)}")
        case _               => err(s"unary '-' is not defined for ${show(t.ty)}"))

    case Unary("!", e) =>
      TUnary("!", analyzeBool(e), Type.Bool)

    case Unary("~", e) =>
      val t = analyzeExpr(e, expected.filter(Type.computesNumerically))
      prefixCall("~", t).getOrElse(Type.opSubject(t.ty) match
        case _: Type.Integer => produced(TUnary("~", t, unaryType(t.ty)))
        case _               => err(s"unary '~' is not defined for ${show(t.ty)}"))

    // Address-of yields a *raw* pointer: a place lives in a frame or inside another object, so
    // there is no refcount to take a share of. Reaching a `&T` means being handed one.
    //
    // The one place it is refused is a place inside a struct whose invariant reads it: the pointer
    // would be typed below the promise, and `reference/errors.md § Struct invariants` is discharged
    // by naming the struct. A function is not a place — nothing holds it, and there is no slot to
    // point at — so its address is taken here rather than by the walk below, which asks for one
    // (`reference/ffi.md § A function's address`). A local shadowing the name is an ordinary value
    // and keeps the ordinary reading. The expected type is handed on because it is what settles a
    // *generic* function's arguments: there is no written form for them here, and a `*extern` being
    // asked for already fixes the signature (`reference/ffi.md § A function's address`).
    case Unary("&", Ident(name)) if lookupOpt(name).isEmpty && !ownValueName(name) && funcKey(name).isDefined =>
      functionAddress(name, funcKey(name).get, expected)

    // `&f[T]` and `&f[A, B]` — the address of an *instantiation*, with the arguments written out
    // (`reference/ffi.md § A function's address`). This is the one position in the language where
    // type arguments are written rather than inferred, and what earns it is the shape every C
    // callback has: the interface fixes the signature to untyped pointers, so a trampoline mentions
    // its own type parameter nowhere and there is nothing for the expected type to solve.
    //
    // **The grammar gives this the same shape as `&xs[i]`, and the discrimination is here rather
    // than there.** The name has to be a generic declaration and nothing nearer: a local shadowing
    // one is an ordinary indexed value, and reading its author's subscript as a type argument would
    // be worse than any message. That is the same shadowing test every call form above makes.
    // The test is that the name is a *function*, not that it is a generic one: a function cannot be
    // indexed, so there is no second reading to protect, and `&plain[i32]` is owed the message that
    // `plain` has no type arguments rather than a general complaint about callables.
    case Unary("&", Index(Ident(name), targ)) if lookupOpt(name).isEmpty && funcKey(name).isDefined =>
      functionAddress(name, funcKey(name).get, expected, List(targ))

    // More than one thing in the brackets was never an index, so this needs no shadowing test to be
    // sure of the reading — only to say something useful about a name that is not a generic
    // function, which `functionAddress` does with the declaration in hand.
    case Unary("&", TypeArgs(Ident(name), targs)) if lookupOpt(name).isEmpty && funcKey(name).isDefined =>
      functionAddress(name, funcKey(name).get, expected, targs)

    // The same three, written **qualified**. A function is not a place, so an address is taken from
    // the name above rather than by the walk below — and that walk is the only thing a module path is
    // folded into the name by (`throughModule`). So a qualified spelling reached neither: it fell
    // through to the walk, which found a function where it wanted storage and said so, quoting the
    // key it had by then rewritten the path into.
    //
    // Which is the shape `0104` had at a constant expression, and the answer is that one's: a name
    // means the declaration rather than a spelling of it, so the qualified form resolves wherever the
    // unqualified one does. `qualifiedFunc` hands back the spelling that was written beside the key,
    // because the message a refused address prints has to be something a reader can type.
    case Unary("&", e) if qualifiedFunc(e).isDefined =>
      val (written, key) = qualifiedFunc(e).get

      functionAddress(written, key, expected)

    case Unary("&", Index(e, targ)) if qualifiedFunc(e).isDefined =>
      val (written, key) = qualifiedFunc(e).get

      functionAddress(written, key, expected, List(targ))

    case Unary("&", TypeArgs(e, targs)) if qualifiedFunc(e).isDefined =>
      val (written, key) = qualifiedFunc(e).get

      functionAddress(written, key, expected, targs)

    // The same node anywhere else. A subscript takes one index, so what was written is a
    // type-argument list — and `reference/ffi.md § A function's address` is the only place one may
    // be written, which is what this says rather than complaining that a comma was unexpected.
    case TypeArgs(_, args) =>
      err(s"a subscript takes one index, and ${args.length} were written — a list of types in " +
        "brackets is a type-argument list, and the only place one is written is at an address, as " +
        "'&f[A, B]'. Everywhere else a generic's arguments are inferred, from the arguments at a " +
        "call or from the type the result is read into")

    // A nested function's environment is the frame it was declared in
    // (`reference/declarations.md`), and an address is a way of carrying it out of that frame — the
    // same reason the name is not a value either.
    case Unary("&", Ident(name)) if lookupOpt(name).isEmpty && (nestedFuncs.contains(name) || outerNested(name)) =>
      err(s"'$name' is a nested function, so it has no address to take — what would have to travel " +
        "beside the address is the frame it reads, and a '*extern' is one word. A top-level " +
        "function is what has an address")

    case Unary("&", e) =>
      val t = analyzeExpr(e)

      // **A name is asking for THAT thing's address, so it is answered rather than given a copy.**
      // The line is what the reader wrote and not what it turned out to be, which is exactly what a
      // diagnostic is for: `&t.f` at a property and `&t.g` at a field are one spelling, and handing
      // the first a pointer into a copy nothing else can see would make the two silently different.
      // A constant is the same shape — `&capacity` reads as storage and there is none. Everything
      // below is written as something computed, and asks for storage rather than for an address.
      if isPlace(t) || named(e) then
        val place = requirePlace(t, e, "'&'", writes = false)
        checkAddressable(place)
        // The address of a register is an address *of a register*, so the qualifier travels with it
        // and every access through the result stays an access to a device (`reference/memory.md §
        // Device memory`).
        TAddrOf(place, Type.Ptr(place.placeTy))
      // Something computed has no address of its own, so one is made for it: the value is written
      // into a hidden local of this scope and what comes back is that slot's address (`TTempAddr`).
      // The pointer is then exactly as good as one taken of a `var` the program wrote itself, which
      // is what `03` already says about every `*T`.
      else
        // A value nothing occupies has no storage to point at, and a pointer to it could only ever
        // be read through to produce nothing — so the address is refused rather than handed back
        // aimed at a slot of no bytes.
        if Type.noValue(t.ty) then
          err(s"${show(t.ty)} is not a value, so there is nothing to make an address of")

        // An opaque type's shape is the C side's, so there is no slot to lay down for one here —
        // the same reason `*c` is refused where `c` itself was fine (`reference/ffi.md § opaque`).
        Type.underlying(t.ty) match
          case s: Type.Struct => checkLayoutKnown(s.base, s.name)
          case _              => ()

        TTempAddr(t, Type.Ptr(t.ty))

    case Unary("*", e) =>
      val t = analyzeExpr(e)
      Type.pointee(t.ty) match
        case Some(inner) =>
          // Reading through the pointer produces the **value**, which is the one thing an opaque
          // type has no shape for out here — so `*c` is refused where `c` itself was fine
          // (`reference/ffi.md § opaque`).
          Type.underlying(inner) match
            case s: Type.Struct => checkLayoutKnown(s.base, s.name)
            case _              => ()

          TDeref(t, Type.unqualified(inner))
        // A trait object points somewhere, but it has forgotten what is there, so there is no type
        // to read out — its methods are the whole of what it still offers.
        case None if Type.erased(t.ty) =>
          err(s"a ${show(t.ty)} has forgotten what it points at, so there is no value to read " +
            "through it — call one of the trait's methods instead")
        case None                       => err(s"'*' needs a pointer or a reference, not ${show(t.ty)}")

    case Unary(op, _) =>
      err(s"unary '$op' is not supported yet")

    case PreIncDec(op, target)  => incDec(op, target, pre = true)
    case PostIncDec(op, target) => incDec(op, target, pre = false)

    // Each link of the chain is resolved on its own — an instruction where the operand type has
    // one, the method its `Eq`/`Ord` supplies otherwise (`reference/expressions.md § Operator dispatch`) — so a chain of user types reads
    // and behaves exactly as a chain of scalars does, sharing each middle operand between the two
    // comparisons that use it.
    case Compare(operands, ops) =>
      val ts = analyzeOperands(operands, None)

      if ts.exists(t => Type.repr(t.ty).isInstanceOf[Type.Vector]) then vecCompare(ts, ops)
      else compareChain(ts, ops.indices.map(i => compareLink(ops(i), ts(i), ts(i + 1))).toList)

    // A parameter's default, spliced in where the argument was not written
    // (`reference/declarations.md § Default parameters and named arguments`). It is analyzed in the
    // declaration's own terms and with nothing local in scope, which is what makes it mean the same
    // thing from every module that calls the function. Its declaration's type parameters are the
    // ones the call settled, which the call hands over through `withDefaultTypes`.
    case d @ DefaultArg(owner, e) =>
      atCallSite(d.pos)(at(e.pos)(filling(e.pos)(inDefault(owner)(inDefaultTypes(analyzeExpr(e, expected))))))

    // Argument binding replaces every one of these before a call's arguments are looked at, so one
    // arriving here was written where nothing is being called by name — in an array literal, on the
    // right of an operator, or at a call through something that carries no parameter names.
    case NamedArg(name, _) =>
      err(s"'$name = …' names an argument, and this is not a call to a declaration that names its " +
        s"parameters — write the value on its own, or '($name = …)' for the assignment")

    // Argument binding replaces this one too, and it needs the same thing to replace it against: a
    // `...T` parameter for the slice to be handed to. One arriving here was written somewhere no
    // parameter collects anything, where it says nothing at all.
    case _: Spread =>
      err("'...' says an argument is already the slice a '...T' parameter collects, and nothing " +
        "here collects one — it stands in a call's argument list and nowhere else")

    // Argument binding replaces every one of these too, and for the same reason it needs a
    // parameter to replace it against: a block is an array of its lines at a collection parameter
    // and a closure over them at a callable one, and a callee with no parameters at all — a value
    // being called through, a variadic's tail — has neither to offer.
    case _: BlockArg =>
      err("a trailing block stands at a parameter of the declaration being called, and this call " +
        "reaches nothing that declares any — write the argument in the parentheses instead")

    // `b[i] = v` on a type with no elements of its own is `IndexSet`, and it is a call rather than a
    // store because a trait's method gives back a value and never an address — so there is no place
    // for the ordinary path to write through, and the trait says as much by taking the value.
    case Assign("=", Index(receiver, index), value) if indexes(Library.key("IndexSet"), receiver) =>
      callMethod(receiver, "index_set", List(index, value), None)

    // The compound forms would have to read the element and write it back, which means evaluating
    // the receiver and the index twice — and a container's subscript is a call, so twice is twice
    // the calls. Written out, the program says that itself.
    case Assign(op, Index(receiver, index), _) if indexes(Library.key("IndexSet"), receiver) =>
      err(s"'$op' on an element read through '${qn(Library.key("Index"))}' would evaluate the " +
        s"receiver and the index twice — write it out as 'b[i] = b[i] ${op.dropRight(1)} …'")

    // `p.count = v` where `count` is a settable property is a **call**, exactly as `b[i] = v` on a
    // container is (`library/core.md § Walking a type of your own`, `reference/expressions.md § Assignment`): a property computes
    // rather than naming storage, so there is no place for a store to write through, and the setter
    // takes the value instead.
    case Assign("=", Field(receiver, name), value) if settable(receiver, name) =>
      checkNotOwnSetter(receiver, name)
      callMethod(receiver, DeclParser.setterName(name), List(value), None)

    // The compound forms, which is where a property parts company with `IndexSet`. `library/core.md
    // § Walking a type of your own` refuses `b[i] += v` because the receiver *and the index* would
    // each be evaluated twice; a property has no index, so taking the receiver's address once is
    // the whole of what the form needs — and it is the line the feature exists for, `count += 1`
    // rather than the two calls written out.
    //
    // It is desugared into source rather than built here, so every rule arrives through its ordinary
    // spelling: `&` refuses a receiver with no address, the setter's own `*self` refuses a `val`,
    // and the arithmetic is whatever `+` means for that type. The temporary holds the **address**,
    // since a copy of the receiver would be written and thrown away.
    case a @ Assign(op, Field(receiver, name), value) if settable(receiver, name) =>
      // Asked of the receiver as **written**, and before the desugaring below replaces it with the
      // temporary holding its address — after that there is no `self` left to recognize.
      checkNotOwnSetter(receiver, name)

      if !addressable(receiver) then
        err(s"'$op' reads '$name' and writes it back, so it needs a receiver it can reach twice — " +
          "this one is computed, and has no address. Bind it to a 'var' first")

      val tmp                    = s"${Modules.sep}recv"
      def here[T <: Positioned](e: T): T = e.setPos(a.pos)
      def through: Expr          = here(Field(here(Unary("*", here(Ident(tmp)))), name))

      analyzeExpr(
        here(Block(List[Stmt](
          here(ValDecl(tmp, None, here(Unary("&", receiver)))),
          here(ExprStmt(here(Assign("=", through, here(Binary(op.dropRight(1), through, value)))))),
        ))),
        expected,
      )

    // A property with no setter. The ordinary path would report an expression with no address, which
    // is true and is not what the reader needs to know: what is missing is the member, and the
    // sentence that says so also says what to write.
    case Assign(op, Field(receiver, name), _) if readOnlyProperty(receiver, name).isDefined =>
      val p   = readOnlyProperty(receiver, name).get
      val msg = s"'$name' is a property of '${p.of}', which computes rather than naming storage, " +
        s"so ${if op == "=" then "there is nothing to assign through" else s"'$op' has nothing to write back"} " +
        s"— write 'set $name(…)' ${p.where} to give it a setter"

      if p.viaBound then boundErr(msg) else err(msg)

    case Assign("=", target, value) =>
      val place = analyzePlace(target, "assignment")
      val tv    = analyzeExpr(value, Some(place.ty))
      // A diverging value is no value to store, so it is rejected here rather than agreeing the way
      // a `never` does where one really may stand — as the value a `return` or a branch yields.
      if tv.ty == Type.Never || disagree(tv.ty, place.ty) then
        err(s"cannot assign ${show(tv.ty)} to ${describe(target)} of type ${show(place.ty)}")
      withInvCheck(place, TStore(place, tv, place.ty))

    // `p += q` on a type whose `Add` is a real implementation updates the place from the value it
    // already read, exactly as the scalar form does — the dispatch travels with the node rather
    // than becoming a call tree that would read the place twice.
    case Assign(op, target, value) =>
      val place  = analyzePlace(target, s"'$op'")
      val binSym = op.dropRight(1)
      // `v += 1.0` is `v = v + 1.0`, so a scalar splats here exactly as it does in the binary form.
      // The two spellings reach the same instruction and have to agree about it, which is the rule
      // this whole branch is written around.
      val tv     = balanceLanes(TZero(place.ty), analyzeExpr(value, updateExpected(binSym, place.ty)))._2
      val d      = updateDispatch(binSym, place, tv)

      // What has to hold is that the result can be stored back. A constrained place is the one case
      // where the arithmetic's type and the place's legitimately differ — a transparent subtype
      // computes at its base — so the test is on the representation the two share, and what the
      // difference costs is the check `constraintOf` asks for. `disagree` is that comparison plus
      // the suppression a poisoned type wants: a place whose type could not be worked out has been
      // complained about once already, and saying its `+=` changes a type is a second complaint
      // about the consequence.
      if d.isEmpty && disagree(arithType(binSym, place.ty, tv.ty, tv.pos), place.ty) then
        err(s"'$op' would change the type of ${describe(target)}")

      withInvCheck(place, TUpdate(place, op, tv, place.ty, d, constraintOf(place.ty)))

    // A call, and the two forms written like one — `sizeof(T)` and `offsetof(T, f)`, whose
    // operand the parser has already read as a type. The largest run of arms there is, and the
    // one whose order carries the most: `CallExprAnalysis`.
    case e @ (_: Call | _: LayoutOf | _: OffsetOf) => callExpr(e, expected)

    // A member read is one form with three readings — a field, a property, or an attribute of a
    // type's own name — and each reading's mistakes want their own words. `MemberExprAnalysis`.
    // The same selection with nothing called — `Maybe[int].Nothing`, a variant that carries no
    // payload. It reaches `fieldExpr` rather than any call form, so it needs the case said again
    // here; without it the reader gets the same `undefined name` about a type that is declared.
    case Field(Index(h, targ), sel) if genericTypeHead(h).isDefined =>
      typeArgsAtSelection(genericTypeHead(h).get, List(targ), sel, Nil)

    case Field(TypeArgs(h, targs), sel) if genericTypeHead(h).isDefined =>
      typeArgsAtSelection(genericTypeHead(h).get, targs, sel, Nil)

    case e: Field    => fieldExpr(e, expected)
    case e: TypeAttr => typeAttrExpr(e)

    // `.red` — the same selection with the type's own name left off, taken from what the context
    // expects (`reference/expressions.md § A leading dot`). The call form is up with the other
    // call arms, since a call is matched before its callee is looked at.
    case ImplicitMember(f) => implicitMember(f, expected)

    // `base with { bg = ACCENT }` — the value again with some fields changed, which is a member
    // form because every rule it obeys is an assignment's. `MemberExprAnalysis`.
    case w: WithExpr => withExpr(w, expected)
    // Building a sequence and reaching into one, which share the question of how many elements
    // there are and whether this index is one of them. `CollectionExprAnalysis`.
    case e @ (_: ArrayLit | _: ArrayFill | _: Index) => sequenceExpr(e, expected)
    // Control flow that yields a value (`reference/statements.md`), and the forms that carry
    // several at once. `ControlFlowExprAnalysis`.
    case e @ (_: IfExpr | _: MatchExpr | _: While | _: DoWhile | _: Loop | _: CFor | _: For |
        _: ConstFor | _: Quantifier | _: TryExpr | _: RangeExpr | _: ResultList | _: Lambda |
        _: Tuple | _: Block) =>
      controlExpr(e, expected, discarded)

    // Reached only where an `is` was written somewhere a condition's terms are not read one by one:
    // under `||` or `!`, in a `match` guard, in a `require`, on the right of an `=`, as an
    // argument. The rule is about the binding rather than the test — a `bool` would be harmless,
    // but a name bound where the reader cannot see which paths reach it is not
    // (`reference/expressions.md § is — a pattern where a condition is wanted`).
    case _: IsPattern =>
      err("'is' tests a pattern in the condition of an 'if' or a 'while', and nowhere else — its " +
        "binding is live from here to the end of the condition and through the branch that " +
        "condition guards, and there is no such branch here. Chain it with '&&', or write 'match'")


  /** `++`/`--` — a step of one, which the base decides the existence of and a constrained place
   * then has to accept: the new value is checked between the addition and the store, so a counter
   * declared over a range stops at the end of it rather than walking off.
   *
   * It is a write of one field when its place is one, so it owes a struct's `invariant` the same
   * re-check the compound form owes — `s.lo++` can break `lo <= hi` exactly as `s.lo += 1` can.
   */
  private def incDec(op: String, target: Expr, pre: Boolean): TExpr = {
    val place = analyzePlace(target, s"'$op'")

    Type.underlying(place.ty) match
      case _: Type.Integer => withInvCheck(place, TIncDec(place, op, pre, place.ty, constraintOf(place.ty)))
      case _               => err(s"'$op' is not defined for ${show(place.ty)}")
  }

  /** A comparison where either side is a vector: one link, and a mask rather than a `bool`.
   *
   * **A chain is refused rather than lowered.** `a < b < c` on scalars is two comparisons joined by
   * `&&`, and `&&` short-circuits — which is a thing no register does, since every lane is computed
   * either way. Reading the chain as a lane-wise `&` would give it the shape of the scalar spelling
   * and a different meaning, so the reader is told to write the `&` themselves and see it.
   */
  private def vecCompare(ts: List[TExpr], ops: List[String]): TExpr = {
    if ts.length > 2 then
      err("a comparison chain joins its links with '&&', which short-circuits and so has no " +
        "lane-wise form — compare two vectors at a time and combine the masks with '&'")

    val (l, r) = balanceLanes(ts.head, ts(1))
    val op     = ops.head

    if Type.repr(l.ty) != Type.repr(r.ty) then
      err(s"cannot compare ${show(l.ty)} with ${show(r.ty)}")

    val lane = Type.repr(l.ty) match
      case v: Type.Vector => Type.underlying(v.elem)
      case other          => other

    val equality = op == "==" || op == "!="

    // A mask is `<N>bool`, which is what makes `(a < b).select(x, y)` and `(a < b) & (c < d)`
    // ordinary values rather than a comparison's private business. `bool` lanes have equality and
    // no ordering, exactly as a scalar `bool` does.
    lane match
      case _: Type.Integer | _: Type.Floating =>
      case Type.Bool if equality              =>
      case _ => err(s"'$op' is not defined for ${show(l.ty)}")

    val n = Type.repr(l.ty).asInstanceOf[Type.Vector].length

    TVecCompare(op, l, r, Type.Vector(n, Type.Bool))
  }

  /** A comparison chain, checked link by link. A link the machine performs directly needs its
   * operands to agree and the type to have the comparison being asked of it — equality reaches
   * further than ordering (`01`); a link a trait supplies had both checked against the trait's own
   * signature when `compareLink` resolved it.
   */
  protected def compareChain(ts: List[TExpr], cmps: List[TCmp]): TExpr = {
    for i <- cmps.indices if cmps(i).dispatch.isEmpty do
      val op       = cmps(i).op
      val (a, b)   = (ts(i), ts(i + 1))
      val equality = op == "==" || op == "!="
      // Operands agree on their *representation*, which is the reading `arithType` takes: a
      // transparent subtype is the same type as its base (`reference/errors.md § Constrained
      // types`), so `a < n` between an `Age` and an `int` is one comparison of two integers, while
      // a derived subtype is its own representation and so still compares only with itself.
      if Type.repr(a.ty) != Type.repr(b.ty) then err(s"cannot compare ${show(a.ty)} with ${show(b.ty)}")
      if !(if equality then Type.isEquatable(a.ty) else Type.isOrdered(a.ty)) then
        err(s"'$op' is not defined for ${show(a.ty)}")

    TCompare(ts, cmps)
  }

  // --- places --------------------------------------------------------------------------

  /** Whether a typed expression denotes a **place** — something with an address, which can be
   * assigned through and pointed at. A local, a dereference, an element, and a field of any of
   * them are places; anything computed (a call result, an arithmetic result, a freshly built
   * struct) is not.
   *
   * The element is the case with a condition on it, and the arm below says which way each falls.
   */
  protected def isPlace(t: TExpr): Boolean = t match
    case _: TLoad           => true
    case _: TGlobal         => true
    case _: TDeref          => true
    case TField(recv, _, _) => isPlace(recv)
    // A slice's elements live wherever its owner keeps them, so they have an address even when
    // the slice itself is a temporary. An array's elements are the array, so they do not.
    case TIndex(recv, _, _) =>
      recv.ty match
        case _: Type.Slice => true
        case Type.Str      => false
        // What a pointer names is somewhere else, so its elements have an address whether or not
        // the pointer itself is a place — the same reason a slice's do, and the same reason `*p`
        // is a place.
        case _: Type.Ptr   => true
        case _             => isPlace(recv)
    case _ => false

  /** Analyzes something that must be a place — an assignment target or the operand of `&`.
   *
   * `writes` separates the two. Both need an address, but only one of them *is* a write: `&` hands
   * back a `*T`, and what a program does with a raw pointer is the unsafe tier's business by the
   * rule `03` states outright. So a place that may be addressed but not assigned to says no here
   * and nothing there.
   */
  protected def analyzePlace(target: Expr, what: String, writes: Boolean = true): TExpr =
    requirePlace(analyzeExpr(target), target, what, writes)

  /** The same demand made of an expression **already analyzed**, for a form that had to see the type
   * before it knew whether a place was what it wanted at all.
   */
  protected def requirePlace(t: TExpr, target: Expr, what: String, writes: Boolean = true): TExpr = {
    // A **captured** name reaches storage the walk below cannot see the binding of — a field of the
    // environment, or the frame slot one points at — so being written once is asked of the name it
    // was declared under rather than of the expression it turned into (`reference/expressions.md §
    // Closures`).
    target match
      case Ident(n) if lookupOpt(n).exists((u, _) => readOnlyLocals(u)) =>
        err(writtenOnce(lookupOpt(n).map(_._1), what))
      case _ =>

    t match
      // A string is immutable, and it is worth saying so rather than reporting the absence of an
      // address: writing one byte of UTF-8 is how a string stops being UTF-8.
      case TIndex(recv, _, _) if recv.ty == Type.Str =>
        err("a string is immutable, so its bytes have no address to write through")

      // `s.bytes` reinterprets the same three words as a `[]const u8` (`04`), so its elements are
      // the string's own storage and assigning to one is the line above by another route — with a
      // literal's bytes in read-only memory, a segfault out of a program containing no `*T` at all.
      case TIndex(_: TBytes, _, _) if writes =>
        err("a string is immutable, and 'bytes' views the string's own storage rather than a copy " +
          "of it — so writing through one is writing the string. Bytes you may write are bytes of " +
          "your own: copy them into a '[]u8' first")

      // Any other element of a read-only view: the view bound to a name, passed to a function, or
      // sliced again, which is what the arm above cannot reach.
      //
      // Only the *write* is refused. `&` is left alone here, unlike on the `val` itself (`13`), and
      // the difference is which tier the reader is in: `&v[0]` is a `*T` the moment it is written,
      // and `03` says in as many words that the guarantees stop there and that this is how a view
      // reaches a C function taking a pointer and a length. `printf("%.*s")` is exactly that call
      // and so is `memchr`, which is what `sysl.io`'s own `find_byte` is. Refusing it would leave
      // a type that cannot do the job it was added for, while buying nothing: `*T` is greppable, and
      // a program that has none still cannot reach these bytes.
      case TIndex(recv, _, _) if writes && Type.readOnlyView(recv.ty) && recv.ty != Type.Str =>
        err(s"this element belongs to a '${show(recv.ty)}', which views elements it may not write, " +
          "so there is nothing to assign through. Elements you may write are elements of your own: " +
          s"copy them into a '[]${show(Type.element(recv.ty).getOrElse(Type.Unknown))}' first" +
          constSelfNote)

      case _ =>
        // The enumeration names all four kinds rather than the three it used to: an element is a
        // place too, and leaving it out told a reader of this line that `&buf[0]` — which this
        // chapter's own pointer-difference example writes — was not something they could take.
        if !isPlace(t) then
          err(s"$what needs a variable, a field, an element, or a dereference — something with an address")
        // A `val` has an address, which is the whole difference between it and a `const` — what it
        // does not have is a writable one. `&` is refused along with assignment because a `*T` is a
        // licence to write, and handing one out would make the promise unkeepable one step away
        // from where it was written.
        if readOnly(t) then err(writtenOnce(rootLocal(t), what))

    // A live `ref` stands on storage, and an assignment that would release it leaves the name aimed
    // at freed memory (`reference/memory.md § ref — a name for a place`). Only a write is asked
    // about: `&` produces a pointer and takes nothing away, so the storage a ref found is exactly
    // where it was.
    if writes then checkRefGuards(t)

    t
  }


  /** Refuses a setter that writes the property it is defining, which calls itself.
   *
   * The mirror of the read `MemberExprAnalysis` refuses, and it is asked of the receiver as the
   * program **wrote** it rather than of the analyzed one: a compound form is rewritten into a
   * temporary holding the receiver's address before anything is analyzed, so `self` has to be
   * recognized while it is still there.
   *
   * Reading `self.count` inside `set count` is left alone, and deliberately: that calls the
   * *getter*, which is a different member and terminates. Only the write comes back here.
   */
  protected def checkNotOwnSetter(receiver: Expr, name: String): Unit =
    receiver match
      case Ident("self") | Unary("*", Ident("self")) if enclosingMember == DeclParser.setterName(name) =>
        err(s"'$name' writes the property it is defining, so it calls itself — a setter is what " +
          "writing the property means, and there is nothing further in for it to reach. What a body " +
          "like this means to write is the field it is in front of")
      case _ =>

  /** One level of automatic dereference, so a field is selected through a `*T` or a `&T`
   * exactly as it is on the value itself. One level only: reaching through a `**T` is written.
   */
  protected def autoDeref(t: TExpr): TExpr =
    Type.pointee(t.ty) match
      case Some(inner) => TDeref(t, Type.unqualified(inner))
      case None        => t

  /** How a diagnostic names an assignment target. */
  protected def describe(target: Expr): String = target match
    case Ident(n)      => s"'$n'"
    case Field(_, f)   => s"field '$f'"
    case Unary("*", _) => "the place it points at"
    case Index(_, _)   => "this element"
    case _             => "this place"
}
