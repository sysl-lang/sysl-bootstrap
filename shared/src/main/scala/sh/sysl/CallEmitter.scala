package sh.sysl

import ir.{Access, Arg, BinOp, ICmp, Inst, LType, Val}

/** The call seam, and writing a value where it is going to live.
 *
 * What a `call` names is not simply the callee's sysl name. An `extern` may have been given a link
 * name, the program's own `main` is renamed out of the way of the entry point the platform starts,
 * a variadic callee needs its whole function type rather than just its result, and a foreign one is
 * lowered under the convention the other side was compiled against rather than sysl's own
 * (`ForeignEmitter`). Four questions, one answer each, asked once here instead of at every call
 * site.
 *
 * The into-writers sit with them because the two are one mechanism rather than two. A **large**
 * aggregate (`layout.indirect`) is built, copied and returned through memory, so `genOwnedInto` and
 * `genBorrowedInto` take the address a value is wanted at and write it there instead of handing
 * back a register the caller then stores — the difference between a struct literal that is fourteen
 * `insertvalue` instructions over multi-kilobyte SSA values and one that is fourteen stores. A call
 * is where that meets the seam from both directions: a large *argument* is staged into a slot with
 * `genOwnedInto`, and a large *result* lands in storage named in front of the argument list. They
 * call each other, so they are one trait.
 *
 * Everything smaller goes on being a value. The into-writers still accept one — a field of a large
 * struct is usually a small one, and the recursion has to bottom out somewhere — and for those they
 * emit exactly what the caller would have emitted itself.
 */
trait CallEmitter extends ControlFlowEmitter with VtableEmitter with WriterEmitter with StaticEmitter
    with ForeignEmitter with ContractEmitter with EnumAttrEmitter {
  /** The typed program being lowered, which the seam below reads to learn what a name resolves to. */
  protected val program: TProgram

  /** The arguments of a call, as an LLVM argument list.
   *
   * Every one is evaluated, in the order it was written, because the *effect* of an argument is
   * owed whatever its type — but a zero-sized one is then not passed, since the callee has no
   * parameter to receive it. That keeps the two sides of the call agreeing with `genFunction`,
   * which drops the same parameters from the signature.
   */
  protected def argList(args: List[TExpr]): List[Arg] = formatArgs(args.map(argValue(_)))

  /** The functions whose bodies can release nothing at all (`BorrowedParams`), so a caller handing
   * one of them a **place** needs no count of its own for the length of the call.
   *
   * This is the one place the callee-side rule is still asked about at a call site, and what it buys
   * is that a leaf accessor reading `f(s.v)` stays as free as it has always been.
   */
  private lazy val inertCallees: Set[String] =
    program.funcs.filter(BorrowedParams.borrows).map(_.name).toSet

  /** Whether a call to this name asks nothing of its caller — the callee cannot reach a release, so
   * whatever holds the argument's count goes on holding it.
   */
  protected def inert(name: String): Boolean = inertCallees(name)

  /** One argument, evaluated, in the form the callee receives it — or `None` where it is zero-sized
   * and there is nothing to hand over.
   *
   * A large one is handed over as the address of storage the caller holds — its own where the
   * argument is a place, a slot made here where it is not — which is the `Left`. The callee copies at
   * entry either way, so the copy the by-value convention promises still happens; it just happens
   * once, in memory, instead of as a multi-kilobyte value crossing the call.
   *
   * **This is where the caller discharges its side of the convention** (`CallOwnership`): the callee
   * reads the value through a count somebody is holding for the length of the call, and where that
   * somebody is not already this frame — a field, a global, a local whose address got out — a count
   * is taken here.
   *
   * What it is taken *at* is the only difference between the two forms, and both are the same idea:
   * hand the callee a **snapshot**, because the storage the argument came out of is storage the call
   * is free to overwrite. A small one's loaded value already is one, so the count goes on it and
   * comes back with the statement's other temporaries. A large one never becomes a register at all,
   * so the snapshot is a slot of this frame's, written with counts taken and released where the
   * scope holding it ends — and the callee's entry copy then reads from there rather than from the
   * caller's own field.
   *
   * The value is kept beside its type rather than formatted straight away because a self-call needs
   * the values themselves: `reference/verification.md § variant on a function`'s measure is
   * evaluated over the arguments, and reaching them by evaluating the arguments a second time would
   * run whatever they do twice.
   */
  protected def argValue(a: TExpr, calleeInert: Boolean = false)
      : Option[(Type, Either[ir.Val, ir.Val])] = {
    val guarantee = containsRef(a.ty) && !calleeInert &&
      !CallOwnership.held(a, callerExposed ++ promoted)

    if layout.indirect(a.ty) then
      if guarantee then
        val slot = emitAlloca(freshReg(), a.ty.lty)

        genOwnedInto(slot, a)
        ownAt(slot, a.ty)
        Some((a.ty, Left(slot)))
      else Some((a.ty, Left(address(a))))
    else
      val v = genExpr(a)

      if guarantee then
        retainValue(a.ty, v)
        ownTemp(v, a.ty)

      Option.unless(Type.zeroSized(a.ty))((a.ty, Right(v)))
  }

  protected def formatArgs(vals: List[Option[(Type, Either[ir.Val, ir.Val])]]): List[Arg] =
    vals.flatten.map {
      case (_, Left(addr)) => Arg(LType.Ptr, addr)
      case (ty, Right(v))  => Arg(ty.lty, v)
    }

  /** A call the function makes to itself as the last thing it does, lowered as a jump back to its
   * own entry rather than a second frame (`TailCalls`).
   *
   * **The order is the whole of the correctness, and it is the order a `return` already uses.** Each
   * argument is computed while the frame is still whole — it may well read the very slots about to
   * be overwritten — and a count is taken for it there, so what it names cannot reach zero when the
   * old bindings let go. Only then does the frame give up everything it holds, and only then do the
   * new values land. Written the other way round, `go(next, list)` would free the list at the moment
   * the parameter holding it was reassigned and pass the second argument a dangling reference.
   *
   * The jump is a `br` and the block ends there, which is all any caller needs to know: this returns
   * no register because there is no value — the call does not come back, and everything the emitters
   * would have gone on to lay down is dropped by `emit`, exactly as it is after a `-> never` call.
   */
  protected def genTailSelfCall(args: List[TExpr]): ir.Val = {
    // A large argument is staged in a slot of its own rather than a register, because that is how a
    // large value moves at all here. The staging slot is what makes it safe as well: `address` on an
    // argument that is simply a parameter hands back that parameter's own slot, so writing straight
    // through would have the first argument's landing change what the second one reads.
    val staged =
      tailParams.zip(args).map { case ((_, ty), a) =>
        if Type.zeroSized(ty) then { genExpr(a); None }
        else if layout.indirect(ty) then
          val slot = emitAlloca(freshReg(), ty.lty)

          genOwnedInto(slot, a)
          Some(Left(slot))
        else
          val v = genExpr(a)

          retainValue(ty, v)
          Some(Right(v))
      }

    // The measure is checked while the frame is still whole, since it reads the parameters and the
    // jump below is about to overwrite them. A tail call survives this where an `ensure` does not
    // (`reference/errors.md § Contracts on a function`): the check happens *before* the call, and a
    // tail call's problem is that it never returns.
    if checksVariant(selfName) then
      genVariantAtCall(tailParams.zip(staged).map {
        case ((_, ty), Some(Left(slot))) => Some((ty, Left(slot)))
        case ((_, ty), Some(Right(v)))   => Some((ty, Right(v)))
        case _                           => None
      })

    releaseAll()

    for case ((name, ty), Some(s)) <- tailParams.zip(staged) do
      s match
        // The count came with the bytes and stays with them: what the staging slot took is now the
        // parameter's, so nothing is retained here and nothing released.
        case Left(slot) =>
          emitMemcpy(Val.Reg(s"$name.addr"), slot, layout.size(ty), layout.align(ty))

        case Right(v) => emit(Inst.Store(ty.lty, v, Val.Reg(s"$name.addr"), Access.Plain))

    emitTerm(Inst.Br(tailTarget.get))
    Val.Nothing
  }

  /** `become f(…)` — the call is emitted with `musttail`, and the `ret` it requires goes with it
   * (`TailJumps`).
   *
   * **The order is `genTailSelfCall`'s, for the same reason and one step further.** Each argument is
   * computed while the frame is still whole; only then does the frame give up everything it holds;
   * only then does the call happen. What the self-jump does after that is store into its own
   * parameter slots and branch, because the frame is being reused; what this does is hand the
   * arguments to a **different** function, whose frame replaces this one.
   *
   * **Nothing is retained here and that is a rule rather than an omission.** A callee takes its own
   * count on every parameter at entry (`Codegen.genFunction`), so a retain on this side would be a
   * second count nobody releases. What makes the release below safe without one is `TailJumps`'
   * restriction: no parameter carries a count at all, so there is nothing in the frame the release
   * could free that an argument still names.
   *
   * **`musttail` and the `ret` are one instruction as far as a reader is concerned** — LLVM refuses
   * a `musttail` that is not immediately followed by a return of its own result — so both are
   * emitted here rather than left to whatever comes next.
   */
  protected def genBecome(call: TExpr): Unit = {
    val (what, callee, args) =
      call match
        case TCall(name, as, ty, _) =>
          val (form, fn) = calleeParts(name, ty)

          (form, fn: ir.Val, as)
        case TCallPtr(fn, as, _, ty) => (syslResult(ty), genExpr(fn), as)
        case other                   => sys.error(s"'become' reached codegen with ${other.getClass.getSimpleName}")

    val staged = formatArgs(args.map(argValue(_)))

    releaseAll()

    if Type.noValue(call.ty) then
      emit(Inst.Call(None, what.ret, callee, staged, what.retAttrs, what.whole, Nil, mustTail = true))
      emitTerm(Inst.Ret(None, None))
    else
      val r = freshReg()

      emit(Inst.Call(Some(r), what.ret, callee, staged, what.retAttrs, what.whole, Nil, mustTail = true))
      emitTerm(Inst.Ret(Some(call.ty.lty), Some(r)))
  }

  /** Every callee declared with a `...`, foreign or sysl's own, mapped to the LLVM function type a
   * call to it must name: result type, declared parameter types, ellipsis.
   */
  private val variadics: Map[String, ir.FnType] =
    val fromExterns = program.externs.filter(_.variadic)
                             .map(e => e.name -> foreignSignature(e.retTy, e.params, variadic = true))
    val fromFuncs   = program.funcs.filter(_.variadic).map { f =>
      val params = syslSret(f.retTy).toList ++
        Type.stored(f.params).map(p => ir.Param(syslParamLty(p._2)))
      val result = syslResult(f.retTy)

      f.name -> ir.FnType(result.ret, params, variadic = true, result.retAttrs)
    }

    (fromExterns ++ fromFuncs).toMap

  /** The `extern`s a call may resolve to, so a foreign call is lowered by what the other side's
   * convention asks for rather than by what sysl's own would be (`ForeignEmitter`).
   */
  protected val foreigns: Map[String, TExtern] = program.externs.map(e => e.name -> e).toMap

  /** The symbol a called name resolves to, which differs from the name for an `extern` given a link
   * name and for the program's own `main`. Everything else is emitted under its own name.
   *
   * `main` is renamed because the emitted entry point *is* `@main`: the platform starts the program
   * there, and a sysl function of that name would be a second definition of one symbol. The reserved
   * name it takes instead holds two separators, which no key can (`Modules.qualify` writes one), so it
   * cannot collide with anything a program or a module could be called.
   *
   * **`@export` is not in here, and used to be.** It was written as the same substitution an
   * `extern`'s link name is, pointing the other way — which made the exported symbol a *rename* of
   * the definition, so a C caller reached a body lowered by sysl's convention rather than by its own
   * (`ExportThunk`). The exported name now belongs to the thunk in front of the definition, and the
   * definition keeps its mangled key, which is what a sysl caller resolves and what the thunk calls.
   */
  protected val entrySymbol = s"${Modules.sep}${Modules.sep}main"

  /** The exported symbol of each function that has one, which is the **thunk's** rather than the
   * definition's (`ExportThunk`).
   */
  private val exportSymbols: Map[String, String] =
    program.funcs.collect { case f if f.exported.isDefined => f.name -> f.exported.get }.toMap

  /** **A symbol C has claimed is not available to a sysl definition.**
   *
   * A key is normally `module$name`, which no exported symbol can be — `ExportCheck.cIdentifier`
   * holds one to letters, digits and `_`, and `Modules.sep` is none of those. A function in the
   * **root** module has no such qualification, so its key *is* the bare name, and an export under
   * its own name there claims the very symbol the definition would be emitted under. What that
   * produces is two definitions of one symbol, reported by clang as an invalid redefinition of a
   * function nobody wrote twice.
   *
   * **Renaming the definition rather than the thunk keeps the promise the right way round**: the
   * exported symbol is the one somebody outside has written down, and a mangled key is this
   * compiler's own business. Which of the two moves is the whole of the decision here.
   *
   * A function that is not itself exported is covered too, since a `@export("add")` elsewhere in the
   * program claims `add` whoever else wanted it.
   */
  private val displaced: Set[String] = exportSymbols.values.toSet

  private val symbols: Map[String, String] =
    program.externs.collect { case e if e.symbol != e.name => e.name -> e.symbol }.toMap ++
      program.funcs.collect {
        case f if displaced(f.name) => f.name -> s"${f.name}${Modules.sep}${Modules.sep}sysl"
      }.toMap ++
      program.entry.map(_.func -> entrySymbol)

  /** What a definition and every call to it name. */
  protected def symbolOf(name: String): String = symbols.getOrElse(name, name)

  /** The symbol an **address** of this function names — the one entry point that is callable under
   * the machine's C convention, since that is the only thing a `*extern` may hold
   * (`reference/ffi.md § A function's address`).
   *
   * For an ordinary sysl function the two are the same and this is `symbolOf`: its signature is all
   * scalars, or the address would have been refused (`FuncAddress`), and a scalar crosses as itself.
   * For an exported one they are not — the definition keeps sysl's lowering and the thunk in front
   * of it is what C can call — so the address is the thunk's, which is what makes `&f` on an
   * exported function mean anything.
   */
  protected def entryOf(name: String): String = exportSymbols.getOrElse(name, symbolOf(name))

  /** The two things a `call` names, kept apart because that is how the instruction carries them:
   * **what** the call names, and the symbol it names.
   *
   * For an ordinary function the first is the result type, which is all LLVM needs; for a variadic
   * one it is the callee's *whole* function type, because the argument list alone does not say where
   * the declared parameters stop and the ellipsis begins.
   */
  protected def calleeParts(name: String, ty: Type): (CallForm, ir.Val.Global) =
    val symbol = symbolOf(name)
    // A foreign result may be named by a type the sysl signature never mentions — a coerced
    // aggregate, or `void` where the value comes back through an out-parameter. A sysl result may
    // be `void` for the second of those reasons alone.
    val result = if foreigns.contains(name) then foreignResult(ty) else syslResult(ty)

    (result.copy(whole = variadics.get(name)), ir.Val.Global(symbol))

  /** Emits a call from sysl to sysl and hands back the register holding its result.
   *
   * `dest`, where there is one, is the storage a **large** result is to land in — the caller's own,
   * named in front of every argument, so the value is never an LLVM value at either end. A caller
   * with nowhere to put one makes a slot here and reads the value back out of it, which is correct
   * and is exactly the shape `genInto` exists to save the callers that *do* have somewhere.
   */
  protected def genSyslCall(what: CallForm, callee: ir.Val, argVals: List[Arg], ty: Type,
                            dest: Option[ir.Val]): ir.Val =
    syslSret(ty) match
      case Some(_) =>
        val slot = dest.getOrElse(emitAlloca(freshReg(), ty.lty))
        val out  = Arg(LType.Ptr, slot, sretAttrs(ty.lty, layout.align(ty)))

        emit(what.call(None, callee, out :: argVals))

        if dest.isDefined then Val.Nothing
        else
          val r = freshReg(); emit(Inst.Load(r, ty.lty, slot, Access.Plain))
          ownTemp(r, ty)

      case None if Type.noValue(ty) =>
        emit(what.call(None, callee, argVals))
        if ty == Type.Never then emitTerm(Inst.Unreachable)
        Val.Nothing

      case None =>
        val r = freshReg()

        emit(what.call(Some(r), callee, argVals))
        ownTemp(r, ty)

  // --- writing a value where it is going to live ----------------------------------------

  /** Writes `e`'s value into `dest` and leaves the destination **owning** it: a count taken for
   * every reference inside, which is what a slot that will later release them needs.
   */
  protected def genOwnedInto(dest: ir.Val, e: TExpr): Unit =
    if !layout.indirect(e.ty) then
      val v = genExpr(e)

      retainValue(e.ty, v)
      emit(Inst.Store(e.ty.lty, v, dest, Access.Plain))
    else
      e match
        // A tail self-call needs no destination: the jump keeps the frame, so the `sret` pointer the
        // caller handed in is still the one the return that eventually happens will write through.
        // `dest` here *is* that pointer, and passing it on would be writing the result of a call that
        // is not being made.
        case c: TCall if isTailCall(c) => genTailSelfCall(c.args)

        // A result already arrives with its count taken, and the out-pointer is what says where.
        // This is the case the whole mechanism is for: `val k = kernel()` writes the callee's work
        // straight into `k`'s slot, and no `%struct.Kernel` is ever an LLVM value.
        case TCall(name, args, ty, _) if !foreigns.contains(name) =>
          val staged = args.map(argValue(_, inert(name)))

          if checksVariant(name) then genVariantAtCall(staged)
          val (what, callee) = calleeParts(name, ty)

          genSyslCall(what, callee, formatArgs(staged), ty, Some(dest))

          // A large result never becomes an LLVM value, so `result` has no register to name here and
          // a clause mentioning it is left alone; one written about the parameters alone is repeated
          // exactly as it is on the register path.
          assumeEnsures(name, args, None)

        case _ =>
          genBorrowedInto(dest, e)
          retainAt(e.ty, dest)

  /** Writes `e`'s value into `dest` without taking a count for anything in it — what lands there is
   * borrowed, exactly as the register `genExpr` hands back is.
   *
   * A **call** is deliberately not special-cased here. Its result arrives owned, and giving it this
   * destination would leave a count in a place with nothing registered to release it; sending it
   * through `genExpr` instead costs the whole-value load this file exists to avoid, but only where
   * a large result is nested inside a literal that is itself being built in place, which is rare
   * and is what the code did before any of this.
   */
  protected def genBorrowedInto(dest: ir.Val, e: TExpr): Unit = e match
    // A bitfield struct has one slot however many fields were written, so there is nothing to build
    // field by field: the container is assembled as a value and stored once (`Bitfields`).
    case TStructNew(struct, args) if Bitfields.of(struct).isDefined =>
      val ranges = Bitfields.of(struct).get
      val vals   = args.zipWithIndex.map((a, i) => (genExpr(a), struct.fields(i)._2))
      val c      = buildBits(ranges, vals.collect { case (v, ft) if !Type.zeroSized(ft) => v })

      emit(Inst.Store(containerLty(ranges), c, dest, Access.Plain))

    case TStructNew(struct, args) =>
      for (a, i) <- args.zipWithIndex if !Type.zeroSized(struct.fields(i)._2) do
        val p = freshReg()

        emit(Inst.Gep(p, struct.lty, dest,
                      List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(struct.slot(i))))))
        genBorrowedInto(p, a)

    case TArrayLit(elems, arrayTy) =>
      for (el, i) <- elems.zipWithIndex do
        val p = freshReg()

        emit(Inst.Gep(p, arrayTy.elem.lty, dest, List(Arg(wordLty, Val.Int(i)))))
        genBorrowedInto(p, el)

    // The tag, and then the variant's own fields written into the region every variant shares.
    // Reaching the region by address is what the value form has to use a stack slot for anyway —
    // a union has no `insertvalue` — so this is the shorter of the two lowerings as well.
    case TEnumNew(en, variant, args) if !en.simple =>
      emit(Inst.Store(i32, Val.Int(variant.tag), dest, Access.Plain))

      if variant.carries then
        val base = payloadPtr(en, dest)

        for (a, i) <- args.zipWithIndex if !Type.zeroSized(variant.fields(i)._2) do
          val p = freshReg()

          emit(Inst.Gep(p, en.payloadLty(variant), base,
                        List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(variant.slot(i))))))
          genBorrowedInto(p, a)

    // The value is generated once, above the loop, exactly as the value form does — every element
    // is a copy of that one evaluation. It is generated even where there are no elements, because
    // one evaluation is what the form promises and an empty array does not take that back.
    case TArrayFill(value, arrayTy) =>
      val v = genExpr(value)

      if arrayTy.length > 0 then
        fillLoop(dest, arrayTy) { at =>
          emit(Inst.Store(arrayTy.elem.lty, v, at, Access.Plain))
        }

    // A copy from one place to another is a copy of bytes. Reading the value out first would make
    // a first-class aggregate of it for the length of one instruction, which is the whole cost.
    case place if hasAddress(place) =>
      val src = address(place)

      emitMemcpy(dest, src, layout.size(e.ty), layout.align(e.ty))

    case _ =>
      val v = genExpr(e)

      emit(Inst.Store(e.ty.lty, v, dest, Access.Plain))

  /** `e` built where a value of it is wanted, for a caller that asked for a register rather than
   * offering somewhere to put one. The load is the very thing the destination forms avoid, so this
   * is the fallback and not the path anything hot takes.
   */
  protected def throughSlot(e: TExpr): ir.Val = {
    val slot = emitAlloca(freshReg(), e.ty.lty)

    genBorrowedInto(slot, e)

    val r = freshReg(); emit(Inst.Load(r, e.ty.lty, slot, Access.Plain)); r
  }

  /** Runs `each` once per element of an array laid down at `base`, with the element's address. */
  private def fillLoop(base: ir.Val, arrayTy: Type.Array)(each: ir.Val => Unit): Unit = {
    val i     = emitAlloca(freshReg(), wordLty)
    val condL = freshLabel("fill.test")
    val bodyL = freshLabel("fill.elem")
    val endL  = freshLabel("fill.done")

    emit(Inst.Store(wordLty, Val.Int(0), i, Access.Plain))
    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    val iv   = freshReg(); emit(Inst.Load(iv, wordLty, i, Access.Plain))
    val more = freshReg()

    emit(Inst.IntCmp(more, ICmp.Ult, wordLty, iv, Val.Int(arrayTy.length)))
    emitTerm(Inst.CondBr(more, bodyL, endL))
    emitLabel(bodyL)
    val ep = freshReg()

    emit(Inst.Gep(ep, arrayTy.elem.lty, base, List(Arg(wordLty, iv))))
    each(ep)
    val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, wordLty, iv, Val.Int(1)))
    emit(Inst.Store(wordLty, nxt, i, Access.Plain))
    emitTerm(Inst.Br(condL))
    emitLabel(endL)
  }


  /** `expr?` — on success the payload becomes the expression's value; on failure the function
   * returns immediately with the failure re-wrapped in its own return type, carrying the error
   * payload across unchanged.
   */
  protected def genTry(
      operand: TExpr,
      ok: Type.EnumVariant,
      fail: Type.EnumVariant,
      retEnum: Type.Enum,
      retFail: Type.EnumVariant,
      convert: Option[String] = None,
  ): Val = {
    val en = operand.ty.asInstanceOf[Type.Enum]
    val v  = genExpr(operand)

    val tag  = freshReg(); emit(Inst.Extract(tag, en.lty, v, List(0)))
    val isOk = freshReg(); emit(Inst.IntCmp(isOk, ICmp.Eq, i32, tag, Val.Int(ok.tag)))

    val okL   = freshLabel("try.ok")
    val failL = freshLabel("try.fail")
    emitTerm(Inst.CondBr(isOk, okL, failL))

    emitLabel(failL)
    // **The conversion happens here and nowhere else**, which is why `TTry` carries a name rather
    // than a node: what it converts is the failure payload, and the payload exists only inside this
    // branch. Everything else about the early return is unchanged, so a `?` that converts and one
    // that does not lower to the same shape with one call in the middle
    // (`reference/errors.md § A ? converts through From`).
    val failed = convert match
      case None =>
        val f = enumValue(retEnum, retFail, payloadFields(en, fail, v))
        // The payload was borrowed out of the operand, so the value leaving the function takes a
        // count of its own.
        retainValue(retEnum, f)
        f
      case Some(fn) =>
        // **A converted failure arrives owned and is NOT retained again.** The conversion is a call,
        // and a call's result is a share of its own — so the count the early return carries out is
        // the one `from` already took. Retaining here would be the second of two and nothing would
        // ever release it.
        enumValue(retEnum, retFail,
          List(genConversion(fn, payloadFields(en, fail, v).head, en.targs(1), retEnum.targs(1))))

    // **A large result leaves through the caller's storage, and `?` is a `return` like any other.**
    // The ABI has already made this function `void` and given it an `sret` out-parameter
    // (`Codegen.genIndirectReturn`), so handing the value back directly emits a `ret` of an
    // aggregate out of a `void` function — IR that LLVM refuses, in a temporary file the driver
    // deletes, naming `void` and so reading as a fault in the C toolchain rather than in the sysl
    // that was written. The function's own final `return` was always lowered correctly; this early
    // one is the path that had no case for it.
    if layout.indirect(retEnum) then
      emit(Inst.Store(retEnum.lty, failed, sretParam, Access.Plain))
      releaseAll()
      emitTerm(Inst.Ret(None, None))
    else
      releaseAll()
      emitTerm(Inst.Ret(Some(retEnum.lty), Some(failed)))

    emitLabel(okL)
    payloadFields(en, ok, v).head
  }

  /** One call to a conversion function whose argument is a value already in hand.
   *
   * The ordinary call path takes typed **expressions**, so it can ask each argument for its address
   * where the ABI wants one. Here the argument is a register, so a value the ABI passes indirectly
   * has to be given storage first — which is the whole of what this does beyond the call itself.
   */
  private def genConversion(fn: String, arg: Val, from: Type, to: Type): Val = {
    val passed =
      if !layout.indirect(from) then Arg(from.lty, arg)
      else
        val slot = emitAlloca(freshReg(), from.lty)
        emit(Inst.Store(from.lty, arg, slot, Access.Plain))
        Arg(LType.Ptr, slot)

    val (what, callee) = calleeParts(fn, to)

    // **Emitted rather than sent through `genSyslCall`, because the result must NOT be a temp.** A
    // temp is released where the surrounding statement ends, and this call happens inside the branch
    // that returns — so the release would be emitted in a block the value does not reach, which LLVM
    // refuses outright with *"Instruction does not dominate all uses"*. The count the call took is
    // exactly the one the early return carries out, which is why `genTry` does not retain it either.
    syslSret(to) match
      case Some(_) =>
        val slot = emitAlloca(freshReg(), to.lty)
        val out  = Arg(LType.Ptr, slot, sretAttrs(to.lty, layout.align(to)))

        emit(what.call(None, callee, out :: List(passed)))

        val r = freshReg(); emit(Inst.Load(r, to.lty, slot, Access.Plain)); r

      case None =>
        val r = freshReg(); emit(what.call(Some(r), callee, List(passed))); r
  }
}
