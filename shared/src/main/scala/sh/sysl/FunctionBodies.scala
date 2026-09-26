package sh.sysl

import scala.collection.mutable

/** What a function body is checked against, and how one is analyzed inside another.
 *
 * Running a body is what the driver is draining towards, so this is where the drain arrives. Three
 * entries reach it and they differ only in where the signature comes from: an instantiation's is
 * registered in `funcInsts`, the definition-time pass of `reference/generics.md § Bounds` resolves
 * its own, and a closure's comes from the context that asked for a callable.
 *
 * That last one is why `analyzeNested` saves and restores the whole of the per-function state. A
 * closure's body is written in the middle of its enclosing function and has to be analyzed there —
 * that is where the names it captures are in scope — but every piece of state a body is analyzed
 * against belongs to the function being interrupted. Putting it aside and giving it back is the
 * whole of what makes a body nested inside another body possible.
 */
trait FunctionBodies extends ModuleStorage {

  protected def analyzeFuncBody(name: String, f: FuncDecl, subst: Map[String, Type]): TFunc =
    // A body means what it meant where it was written: an `impl` in one module for a type in
    // another lowers members keyed under the type, and a trait's default is copied into every
    // implementing type, so which module a name in it resolves against travels with the body.
    inDecl(f.name)(at(f.pos)(analyzeFuncBodyAt(name, f, subst)))

  private def analyzeFuncBodyAt(name: String, f: FuncDecl, subst: Map[String, Type]): TFunc = {
    val (params, rtype) = funcInsts(name)
    val saved           = inConstSelf

    // Set for the body of a slice shape block made real at the read-only view, and cleared after —
    // saved and restored rather than assigned, because a body may be analyzed inside another one
    // (`analyzeNested`) and a closure written in a writable instance must not inherit the note.
    inConstSelf = constSelfInsts.contains(name)

    try analyzeBodyWith(name, f, subst, params, rtype)
    finally inConstSelf = saved
  }

  /** Analyzes a body **inside** the analysis of another one, and gives back what it yields as well
   * as what it lowered to.
   *
   * A closure's body is written in the middle of its enclosing function and has to be analyzed
   * there, because that is where the names it captures are still in scope and where a diagnostic
   * about it belongs. But a body is analyzed against per-function state — the scope stack, the
   * unique-name set, the declared result, the loops a `break` may name — and every one of those
   * belongs to the function being interrupted. So the state is put aside and given back, which is
   * the whole of what makes a body nested inside another body possible.
   *
   * **The result type comes from the body**, not from a signature, which is what lets a closure be
   * written with nothing to infer a result from (`reference/expressions.md § Closures`): the
   * parameters come from the context asking for a callable and the result comes from what the body
   * does.
   */
  protected def analyzeNested(
      name: String,
      params: List[(String, Type)],
      declaredResult: Option[Type],
      body: List[Stmt],
      environment: Option[Environment] = None,
      siblings: Map[String, Nested] = Map.empty,
      variadic: Boolean = false,
  ): (TFunc, Type) = {
    // A nested body is declared by whatever declared the body it is written in.
    val savedOwner    = currentOwner
    val owner         = if currentOwner.nonEmpty then currentOwner else currentModule
    val savedScopes   = scopes
    val savedUsed     = used.toSet
    val savedReadOnly = readOnlyLocals.toSet
    val savedPattern  = patternLocals.toSet
    val savedRefs     = refPlaces.toMap
    val savedGuards   = refGuards
    val savedImports  = importStack
    val savedLoops    = loops
    val savedEnsure   = ensureResultTy
    val savedOld      = oldBuf
    val savedMulti    = multiOk
    val savedRet      = retTy
    val savedRetList  = retIsList
    val savedVariadic = variadicFn
    val savedCaptures = capturedFields
    val savedNested   = nestedFuncs
    val savedPending  = pendingNested
    val savedOuter    = outerNested
    val savedDeclares = blockDeclares

    // A closure has no name a reader wrote, so `__FUNCTION__` in one names the function it is
    // written in — which means carrying the enclosing name across the reset rather than letting the
    // body see the empty state that reset establishes. Restored below with everything else, since
    // this walk interrupts a function that is still going.
    val savedFuncName = currentFunctionName
    val savedMember   = currentMemberName

    // And whether that function is one a test build keeps, for the same reason: a closure written
    // inside a test is scaffolding exactly as the test is, and nothing about the lowered body says
    // so once the reset has cleared where it came from.
    val savedInTest = inTestBody

    try
      resetFunction()
      currentFunctionName = savedFuncName
      currentMemberName = savedMember
      currentOwner = owner
      inTestBody = savedInTest
      retTy = declaredResult.getOrElse(Type.Unknown)
      retIsList = false
      // A nested function states its own signature, so a `...` on one is its own tail to walk; a
      // closure literal has no way to write one, and is handed `false`
      // (`reference/declarations.md`, §9).
      variadicFn = variadic

      // The receiver comes first, so the closure's own environment is the argument every call
      // already passes and the parameters after it are the ones a program wrote.
      val receiver = environment.map(e => ("self", Type.Ptr(e.struct)))
      val tparams  = (receiver.toList ::: params).map { case (n, t) => (declare(n, t), t) }

      for e <- environment do
        val self = TLoad("self", Type.Ptr(e.struct))

        capturedFields = e.names.zipWithIndex.map { (n, i) =>
          val stored = e.struct.fields(i)._2
          val field  = TField(TDeref(self, e.struct), e.struct.slot(i), stored)

          // A by-reference environment holds the address of the frame's own variable, so what the
          // name reaches is the variable itself — one dereference further, and a place, which is
          // what lets a nested function assign to it.
          val (ty, read) = stored match
            case Type.Ptr(inner) if e.byReference => (inner, TDeref(field, inner))
            case _                                => (stored, field)

          (if e.fixed(n) then declareReadOnly(n, ty) else declare(n, ty)) -> read
        }.toMap
      // A sibling is in scope throughout the group, so a body may call one written below it — which
      // is the half of `reference/declarations.md` that makes mutual recursion work. What a body
      // does *not* reach is a nested function of the frame around it, and remembering their names
      // is what lets that be said rather than reported as a name that stands for nothing.
      nestedFuncs = siblings
      outerNested = (savedNested.keySet ++ savedOuter) -- siblings.keySet
      // The block around this body is where a name it could not capture would have been bound, so
      // its bindings are what a "declared below this" message is measured against.
      blockDeclares = savedDeclares

      // A body written like a function's is one, so its leading contract clauses are its own
      // (`16`). They are analyzed *after* it rather than before, because an `ensure` names `result`
      // and a closure's result is what its body turned out to yield — which is the one thing a
      // declared function knows in advance and this does not.
      val (contracts, rest) = body.span { case _: Require | _: Ensure => true; case _ => false }

      // Neither a closure nor a nested function takes a `variant` yet, and the two are refused
      // together because this is the one path both are analyzed on. The measure is checked at the
      // *call*, out of the arguments it supplies (`reference/verification.md § variant on a
      // function`), and neither of these is reached by a call of that shape: a closure goes through
      // `Fn`, and a nested function's calls carry its captured environment as a receiver the check
      // would have to account for. `reference/verification.md § variant on a function`.
      body.collectFirst { case v: Variant => v }.foreach { v =>
        at(v.pos)(err("a 'variant' is a top-level function's — the measure is checked where a call " +
          "to the same body is written, and neither a closure, which is reached through 'Fn', nor " +
          "a function nested in another, whose calls carry a captured environment, is reached that " +
          "way"))
      }

      // A closure whose result the context did not fix is analyzed with nothing expected, so what
      // its body yields is what it yields — the only reading under which `x -> x * 2` has a result
      // at all.
      val tbody = declaredResult match
        case Some(Type.Unit) => analyzeValueBlock(rest, None, discarded = true)
        case want            => analyzeValueBlock(rest, want)

      val result = declaredResult.getOrElse(if tbody.result.isDefined then tbody.ty else Type.Unit)

      if declaredResult.exists(r => r != Type.Unit && tbody.result.isDefined && disagree(tbody.ty, r)) then
        err(s"this closure should yield ${show(declaredResult.get)}, but its body yields ${show(tbody.ty)}")

      val (requires, ensures, olds, _) = analyzeContracts(result, contracts)

      // **Internal, always.** Nothing outside this compilation can name a closure or a function
      // nested in a body (`reference/types.md § Function types`), so the symbol has no reason to
      // leave the object file — and leaving it there is what a per-compilation counter for a name
      // makes unsafe. Two units that each lowered a fourth closure both call it `$closure4.call`,
      // and with external linkage the linker is free to resolve one unit's call to the other unit's
      // body: a different environment layout under a different body, which is a wrong answer rather
      // than a failure to link.
      (TFunc(name, tparams, result, tbody, variadic, requires, ensures, olds, internal = true,
             module = owner),
       result)
    finally
      currentFunctionName = savedFuncName
      currentMemberName = savedMember
      currentOwner = savedOwner
      inTestBody = savedInTest
      scopes = savedScopes
      used.clear(); used ++= savedUsed
      readOnlyLocals.clear(); readOnlyLocals ++= savedReadOnly
      patternLocals.clear(); patternLocals ++= savedPattern
      refPlaces.clear(); refPlaces ++= savedRefs
      refGuards = savedGuards
      importStack = savedImports
      loops = savedLoops
      ensureResultTy = savedEnsure
      oldBuf = savedOld
      multiOk = savedMulti
      retTy = savedRet
      retIsList = savedRetList
      variadicFn = savedVariadic
      capturedFields = savedCaptures
      nestedFuncs = savedNested
      pendingNested = savedPending
      outerNested = savedOuter
      blockDeclares = savedDeclares
  }

  /** Analyzes one body against a signature it is handed, rather than one looked up in `funcInsts`.
   * An instantiation's signature is registered there; the definition-time pass of
   * `reference/generics.md § Bounds` resolves its own, since a generic declaration has no entry
   * until something instantiates it.
   */
  protected def analyzeBodyWith(
      name: String,
      f: FuncDecl,
      subst: Map[String, Type],
      params: List[(String, Type)],
      declaredResult: Type,
  ): TFunc = {
    resetFunction()
    // Set from the *declaration* rather than from `name`, which is the instantiation's mangled key:
    // `__FUNCTION__` reports what a reader wrote, and one written function is one name however many
    // times a generic was lowered.
    currentFunctionName = Modules.bare(f.name)
    // Whole, for the reason `currentMemberName` carries: the split above cuts a setter's name in the
    // wrong place, and which member this body is has to be answerable.
    currentMemberName = f.name
    // Who declared it is read off the declaration too, so an instantiation belongs to the module of
    // the generic it was made from rather than to whichever module's call asked for it.
    currentOwner = ownerOf(f.name)
    // Read off the declaration for the same reason, and off the declaration's own name rather than
    // off `name`: an instantiation of a generic written in a test file is scaffolding exactly as the
    // generic is, and its mangled key is in no table that remembers which file wrote it.
    inTestBody = f.test.isDefined || testOnlyDecls(f.name)
    // A member's body sees `Self` alongside whatever type parameters it was instantiated with, so
    // the one substitution answers both questions and nothing downstream has to know the difference.
    tsubst = subst ++ memberSelf.getOrElse(name, Map.empty)
    // What the signature asked of those parameters, kept beside what they became: an instantiated
    // body still has to say which trait's member a name on a type parameter meant.
    tbounds = f.bounds
    // And which of the body's own names hold one of them, for the members reached through a value
    // rather than through the parameter's name. Only a parameter written as the bare type parameter
    // qualifies: a `Box[T]` is a `Box`, and what its members mean is the `Box`'s question.
    pbounds = f.params.collect { case Param(n, NamedType(w, Nil), _, _, _, _) if f.bounds.contains(w) => n -> w }.toMap

    // A result list is the signature's, and the body produces the tuple its parts lay out as — so
    // the body is analyzed against that tuple, with the list itself recorded beside it as the one
    // thing the tuple does not say: how the result is written, and what may stand in it.
    val rtype = declaredResult match
      case r: Type.Results => r.parts
      case other           => other

    retIsList = declaredResult.isInstanceOf[Type.Results]
    retTy = rtype
    variadicFn = f.variadic
    val tparams = params.map { case (n, t) => (declare(n, t), t) }
    // Which of those uniqued names came from a by-name parameter, so a read of one becomes the call
    // the sugar promises (`reference/declarations.md § Default parameters and named arguments`).
    // Matched by written name rather than by position, since what `params` holds is not always the
    // declaration's list unchanged.
    val byNameWritten = f.params.filter(_.byName).map(_.name).toSet
    byNameLocals =
      if byNameWritten.isEmpty then Set.empty
      else params.zip(tparams).collect { case ((n, _), (u, _)) if byNameWritten(n) => u }.toSet
    val (contracts, rest) =
      f.body.span { case _: Require | _: Ensure | _: Variant => true; case _ => false }
    val (requires, ensures, olds, variant) = analyzeContracts(rtype, contracts)

    // A function owing no value is where statement position starts: its body block is the outermost
    // one written for effect, and every `if` and `match` that ends it inherits that.
    val tbody =
      if rtype == Type.Unit then analyzeValueBlock(rest, None, discarded = true)
      else analyzeValueBlock(rest, Some(rtype))

    if rtype != Type.Unit && tbody.result.isDefined && disagree(tbody.ty, rtype) then
      // A `where` predicate and a struct `invariant` lower to synthesised `-> bool` functions, so a
      // non-bool clause surfaces here. Their names are internal, so the mistake is reported as what
      // the user wrote — a condition that is not a `bool` — rather than as a return-type mismatch.
      if f.name.endsWith("$pred") then
        err(s"a 'where' predicate must be a 'bool', but this one is ${show(tbody.ty)}")
      else if f.name.endsWith("$inv") then
        err(s"an 'invariant' must be a 'bool', but this one is ${show(tbody.ty)}")
      else
        err(s"function '${f.name}' should return ${show(declaredResult)}, but its body yields " +
          s"${show(tbody.ty)}")

    // Both keys are asked because an instantiation does not carry the declaration's. `name` is what
    // this body is emitted as — `demo$amplify.int` for a generic — and `f.name` is the declaration
    // it came from, which is what `declAccess` was keyed by. They are the same key for an ordinary
    // function, and only the second answers for an instantiation; a symbol is file-private if either
    // says so, since both name the one declaration.
    //
    // **And an instantiation whose name carries a closure's is internal on the same ground as the
    // closure itself.** `sysl.time$resolve.$closure4` is a symbol only the unit that lowered that
    // closure can mean anything by, since the number in it is that unit's counter. Advertised as a
    // library's precompiled function it is worse than useless: a program declares it instead of
    // building its own, and the artifact's copy then calls back into a `$closure4.call` the
    // *program* defined for a closure of its own (card `0229`).
    //
    // Asked of the emitted name rather than of `tsubst`, so that a closure reached through a
    // composite argument — a `Buf` of them, a pack — is covered by the one rule: whatever the
    // argument was, the closure's own base is a segment of the name it produced.
    TFunc(name, tparams, rtype, tbody, f.variadic, requires, ensures, olds,
      fileLocal(name) || fileLocal(f.name) || Closures.mentioned(name),
      f.conv, f.tailrec, variant, f.pure, f.ghost,
      frameSymbols(f.reads, "reads"), frameSymbols(f.writes, "writes"),
      // `@export` becomes a symbol here, where the declared name is still in hand. An unwritten one
      // is the function's **bare** name: the module path is what mangling adds, and suppressing the
      // mangling is the whole of what the attribute does.
      f.exported.map(_.symbol.getOrElse(Modules.bare(f.name))),
      // `@section` travels as written: the name is the target's spelling, so there is nothing here to
      // resolve it against and nothing downstream that would rather have it in another form.
      f.section,
      // All three reach the emitted `define` line and nothing else. They travel per **instantiation**
      // rather than per declaration, which is what a generic marked with one of them means: every
      // copy the program asks for carries the mark the declaration was written with.
      f.noinline, f.cold, f.inline, currentOwner)
      // **The declaration's own position travels with it**, which is what lets the checks that run
      // on the *typed* tree point somewhere. `Exports.check` and `TailCalls.check` both complain
      // about a whole function rather than about an expression inside one, and until this they
      // complained about it from nowhere — no file, no line, and nothing for an editor to
      // underline. A nested function and a closure are stamped nowhere and need no stamp: neither
      // can be exported, and `@tailrec` is a declaration's attribute.
      .setPos(f.pos)
  }

  /** Typechecks the leading `require`/`ensure` clauses. Both conditions must be `bool`. `result`
   * and `old(e)` are only in scope inside an `ensure` — `result` also only when the function
   * returns a value — and the `old` expressions are collected so codegen can snapshot them at
   * entry.
   */
  private def analyzeContracts(
      rtype: Type,
      clauses: List[Stmt],
  ): (List[(TExpr, Option[String])], List[(TExpr, Option[String])], List[TExpr], Option[TExpr]) = {
    val requires = mutable.ListBuffer.empty[(TExpr, Option[String])]
    val ensures  = mutable.ListBuffer.empty[(TExpr, Option[String])]
    val olds     = mutable.ListBuffer.empty[TExpr]
    var variant  = Option.empty[TExpr]

    for c <- clauses do
      c match
        case Require(cond, msg) =>
          requires += ((analyzeBool(cond), msg))
        case Ensure(cond, msg) =>
          ensureResultTy = if rtype == Type.Unit then None else Some(rtype)
          oldBuf = Some(olds)
          val tc = analyzeBool(cond)
          oldBuf = None
          ensureResultTy = None
          ensures += ((tc, msg))
        case Variant(e) =>
          at(c.pos) {
            if variant.isDefined then
              err("a function declares one 'variant' — a measure is the thing that decreases, and " +
                "two of them say nothing about which")
            val te = analyzeExpr(e)

            te.ty match
              case _: Type.Integer =>
              case other           => err(s"a 'variant' is an integer measure, not ${show(other)}")

            // `reference/verification.md § variant on a function` says the measure reads the
            // parameters and nothing else, and **scoping is what enforces it** rather than a rule
            // of this pass: the clause is analyzed before the body, in a scope holding the
            // parameters alone, so a name from the body is undefined here and says so. That is what
            // makes the check local — the arguments at a self-call are what the parameters are
            // about to become, so the "next" measure is this expression over them and nothing has
            // to travel with the call.
            variant = Some(te)
          }
        case _ => // span guarantees only Require/Ensure/Variant reach here
    (requires.toList, ensures.toList, olds.toList, variant)
  }

  /** The parameter list of a slice shape block made real at the read-only view: `self` at the view
   * the receiver actually had, and everything after it untouched.
   *
   * Only the receiver moves. A member declaring a `[]T` **parameter** asked for one that may be
   * written, and the read-only receiver says nothing about what its arguments are allowed to be —
   * `eq(self, rhs: Self)` is the one that reads as if it should follow, and it does, because it
   * names `Self` rather than the shape.
   */
  private def constReceiver(params: List[(String, Type)]): List[(String, Type)] = params match
    case (name, ty) :: rest if name == "self" => (name, constSelfType(ty)) :: rest
    case other                                => other

  /** A receiver's type with its view made read-only, through whatever mode the sigil asked for —
   * `[]T`, `*[]T`. A receiver that is not a slice at all is unchanged, so this is safe to apply to
   * the head of any parameter list.
   */
  private def constSelfType(t: Type): Type = t match
    case Type.Ptr(inner) => Type.Ptr(constSelfType(inner))
    case other           => Type.constView(other)

  protected def instantiateFunc(f: FuncDecl, solved: List[Type], constSelf: Boolean = false): String = {
    // Nameless for `instantiateStruct`'s reason: an instantiation at a `c type` or an exported alias
    // is the instantiation at what it names, and must be one body with one signature — the mangled
    // name below already says so, and the signature filed under it has to agree.
    val targs = solved.map(Type.nameless)

    // The tag is empty for every instantiation a value is ever made at, so an emitted symbol is
    // exactly what it always was. It is not empty for a **stand-in**, which is its name and nothing
    // else — so `Buf.at.T` under one declaration's `[T: Ord]` and under another's `[T: Display]`
    // would be one entry, and the second declaration's body would read the first one's signature
    // back. The instantiation is definition-time and never emitted, so the name is free to say which
    // walk it belongs to (`Type.standInTag`).
    //
    // `constSelf` is the slice shape's second instantiation (`Type.Slice.shape`): the same block,
    // made real at the read-only view because that is what the receiver was. It is a different name
    // because it is a different **body** — the machine code is identical and only one of the two had
    // its writes checked, which is the reason `Type.mangle` carries the bit through everywhere else.
    val name = Type.mangled(if constSelf then Type.Slice.constOwner(f.name) else f.name, targs) +
      targs.map(Type.standInTag).mkString

    // Recorded here because this is the one place a generic becomes a function, and the name it
    // becomes cannot be read back to say so: what tells `lib$twice.Loud` from a member of a type
    // called `Loud` is that this line made it. What reads it is `NoAlloc`, which holds a module to
    // the bodies it wrote rather than to the ones a caller's type argument chose for it.
    if f.tparams.nonEmpty then
      genericInsts += name
      // And what it was made from, for every instantiation rather than only the definition-time
      // one below — a call reaching an editor as a mangled name has nothing else left pointing at
      // the declaration a reader would want opened.
      funcOrigin(name) = f.name
      // And where the *definition-time* pass made one, what it was made from — the instantiation
      // itself is about to be thrown away with the rest of that walk, so its name is the only thing
      // left pointing at the generic that was called.
      if abstractPass then abstractInsts(name) = f.name

    // The signature being made real is the declaration's, so it is resolved in the declaration's
    // module however far from it the call that asked for this instantiation was written.
    if constSelf then constSelfInsts += name

    if !funcInsts.contains(name) then
      inDecl(f.name) {
        val plain = withSelf(f.name, f.tparams.zip(targs).toMap)
        // `Self` and the `self` parameter are moved to the read-only view together, and they have to
        // be: the block writes its receiver as `[]T` rather than as `Self`, so the parameter is not
        // reached by rebinding the name, and a member answering `-> Self` would otherwise hand back a
        // view the receiver never was.
        val subst = if constSelf then plain.updatedWith(selfName)(_.map(Type.constView)) else plain
        val params = f.params.map(p => (p.name, resolveType(p.typ, subst)))
        funcInsts(name) =
          (if constSelf then constReceiver(params) else params,
           f.retType.map(resolveReturn(_, subst)).getOrElse(Type.Unit))
        pending.enqueue((name, f, subst))
      }

    name
  }
}
