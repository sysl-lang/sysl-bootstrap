package sh.sysl

import ir.{Access, Arg, CastOp, FCmp, Inst, LType, Val}

/** Everything that traps when a value turns out not to be what it was promised to be: a function's
 * `require` and `ensure` clauses (`reference/errors.md § What the type's own name offers: ::
 * attributes`), a constrained subtype's `within` range and `where` predicate (`reference/errors.md
 * § Where a constraint is checked`), and a struct's `invariant` (`reference/errors.md § Struct
 * invariants`).
 *
 * They live together because they are one mechanism wearing four names. Each one evaluates a
 * condition and traps on false, and each one has to leave nothing behind on the checked path — a
 * condition may allocate on its way to a `bool`, and the trap is not a place that releases anything.
 * So every check opens a temporary region and closes it before the branch, which is the detail that
 * would otherwise have to be right in four places.
 *
 * What a check does *not* do is decide anything. Which clauses exist, what they mean, and whether
 * they are satisfiable are all settled by the time the tree arrives; a range here is two comparisons
 * against constants the analyzer already folded, and a predicate is an ordinary call to a function it
 * already synthesised.
 */
trait ContractEmitter extends ArcEmitter with ScalarEmitter {

  /** The postconditions of the function being emitted, checked before every return, and the SSA
   * value `result` denotes while one of them is being lowered.
   *
   * They are per-function state like every other register counter, so they are cleared with the rest
   * rather than left for the next function to inherit — `@main` declares no postconditions and must
   * not be handed the last function's.
   */
  protected var ensures: List[(TExpr, Option[String])] = Nil
  protected var resultSSA: Option[Val]                 = None

  /** What the function being emitted is called, what its parameters are, and the `variant` it
   * declared — the three things a self-call needs to check the measure (`reference/verification.md
   * § variant on a function`).
   */
  protected var selfName: String                 = ""
  protected var selfParams: List[(String, Type)] = Nil
  protected var selfVariant: Option[TExpr]       = None

  /** The locals of the body being emitted whose value a **call it makes** could change under it —
   * what `ContractAssume.exposed` finds, and the one thing an argument has to be clear of before a
   * postcondition may be written in terms of it.
   *
   * It defaults to `ContractAssume.unknown`, which substitutes nothing, so a body that never says
   * what it exposes gives up the optimization rather than claiming something it has not checked.
   */
  protected var callerExposed: Set[String] = ContractAssume.unknown

  override protected def startFunction(): Unit = {
    super.startFunction()
    ensures = Nil
    resultSSA = None
    selfName = ""
    selfParams = Nil
    selfVariant = None
    callerExposed = ContractAssume.unknown
  }

  /** Whether a call to `name` is the self-call a `variant` is checked at. */
  protected def checksVariant(name: String): Boolean = selfVariant.isDefined && name == selfName

  /** The measure check at a direct recursive call (`reference/verification.md § variant on a
   * function`).
   *
   * **It reads no state and threads nothing through the call**, which is what
   * `reference/verification.md § variant on a function`'s restriction to the parameters buys. The
   * arguments about to be passed are the values the parameters are about to hold, so the "next"
   * measure is this same expression evaluated with those values in the parameters' own slots: take
   * the measure as it stands, put the arguments in, take it again, put the parameters back, and
   * compare. Nothing is retained or released across the swap — the slots end holding exactly what
   * they held — so the ownership bookkeeping is untouched.
   *
   * `staged` is aligned with `selfParams` and carries what each argument came out as, which is the
   * shape both call paths already produce: an address for a large value, a register for the rest,
   * and nothing at all for a zero-sized parameter.
   */
  protected def genVariantAtCall(staged: List[Option[(Type, Either[Val, Val])]]): Unit = {
    val variant = selfVariant.get

    pushTemps()
    val cur = genExpr(variant)
    popTemps()

    // Every save is laid down before any argument lands, since a measure over two parameters must
    // see both of the call's values and neither of the frame's.
    val saved =
      for case ((name, ty), Some((_, arg))) <- selfParams.zip(staged) yield
        val keep = emitAlloca(freshReg(), ty.lty)

        arg match
          case Left(addr) =>
            memcpy(keep, Val.Reg(s"$name.addr"), ty)
            memcpy(Val.Reg(s"$name.addr"), addr, ty)
          case Right(v) =>
            val old  = freshReg()
            val addr = Val.Reg(s"$name.addr")

            emit(Inst.Load(old, ty.lty, addr, Access.Plain))
            emit(Inst.Store(ty.lty, old, keep, Access.Plain))
            emit(Inst.Store(ty.lty, v, addr, Access.Plain))
        (name, ty, keep)

    pushTemps()
    val nxt = genExpr(variant)
    popTemps()

    for (name, ty, keep) <- saved do
      if layout.indirect(ty) then memcpy(Val.Reg(s"$name.addr"), keep, ty)
      else
        val back = freshReg(); emit(Inst.Load(back, ty.lty, keep, Access.Plain))
        emit(Inst.Store(ty.lty, back, Val.Reg(s"$name.addr"), Access.Plain))

    val ok = freshReg()

    emit(Inst.IntCmp(ok, intPred("<", variant.ty), variant.ty.lty, nxt, cur))
    trapUnless(ok, "variant")
  }

  private def memcpy(dst: Val, src: Val, ty: Type): Unit = {
    usesMemcpy = true
    emitMemcpy(dst, src, layout.size(ty), layout.align(ty))
  }

  /** Emits a contract clause as a trap-on-false check, discarding any temporaries the condition
   * allocated before the trap so the checked path stays leak-free.
   */
  protected def emitContract(cond: TExpr, kind: String): Unit = {
    pushTemps()
    val ok = genExpr(cond)
    popTemps()
    trapUnless(ok, kind)
  }

  /** Runs every postcondition with `result` bound to the value about to be returned. */
  protected def emitEnsures(result: Option[Val]): Unit =
    if ensures.nonEmpty then
      resultSSA = result
      for (cond, _) <- ensures do emitContract(cond, "ensure")
      resultSSA = None

  /** Whether a clause is a proof obligation rather than something to lay down: it names a ghost
   * function, which will not be there (`reference/verification.md § @ghost — what costs nothing to
   * say`).
   *
   * **It is the one switch a contract has, and everything that follows a contract is wired to it.**
   * A clause that is not ghostly is checked at every return and an `assume` may repeat it; a clause
   * that is ghostly is not checked at all, so nothing may claim it. There is no build mode, flag or
   * optimization level that strips a check sysl emitted — a contract is `§1` of
   * `reference/verification.md`, one program with one meaning — and if one is ever added, the assume
   * has to be taken out by the same switch or a caller will be told something no longer established.
   */
  protected def ghostly(x: Any): Boolean = ghostFuncs.nonEmpty && Ghost.mentions(x, ghostFuncs)

  /** The ghost functions of this program, which nothing emitted may name. */
  protected lazy val ghostFuncs: Set[String] =
    program.funcs.filter(_.ghost).map(_.name).toSet

  /** Every function that declares a postcondition worth repeating at a call, by the name a call
   * site writes, each with the parameters its own body may have rebound before the check ran
   * (`ContractAssume.rebound`). A `@ghost` function is not in here because nothing executable calls
   * one.
   *
   * **The rebinding is worked out once per function rather than once per call site**, which is what
   * keeps the feature off the compiler's clock: it is a walk of a whole body, and the library has
   * call sites in the hundreds.
   */
  private lazy val contracted: Map[String, (TFunc, Set[String])] =
    program.funcs.filter(f => f.ensures.nonEmpty && !f.ghost)
      .map(f => f.name -> (f, ContractAssume.rebound(f.body))).toMap

  /** **What a caller is told a call established** — an `llvm.assume` per postcondition, laid down
   * after the call has returned (`reference/verification.md § What the optimizer is told`).
   *
   * **Without this a contract stops at the callee's own frame.** `grow` promises storage for what
   * was asked for and traps if it did not deliver, and the store on the line after the call still
   * carries a bounds test — because LLVM sees a call to a `@noinline` function and then a subscript,
   * and nothing connects them. Repeating the promise as a fact is the connection, and it costs the
   * program nothing at run time: `llvm.assume` emits no code.
   *
   * **Three things make repeating it sound, and all three are load-bearing:**
   *
   *  - The callee checks the same condition **before every return**. `ensure` is checked at each of
   *    them (`Codegen.genFunction`), and the two ways a function can leave without reaching one —
   *    a tail call and a `become` — are both refused outright on a function that has an `ensures`
   *    (`TailCalls`, `TailJumps`). So control arriving here is control that passed the check.
   *  - Nothing strips that check. See `ghostly` above: the only clause that does not run is one
   *    naming a ghost function, and those are skipped here by the same test rather than by a
   *    second one that could drift from it.
   *  - The clause reads the same values in both places, which is `ContractAssume`'s half — and the
   *    caller's side of that is `callerExposed`, since an argument the call itself can change is
   *    not a name the promise was made about.
   *
   * A clause that cannot be rewritten, or whose lowering turns out to do anything at all, is simply
   * not repeated — the program is then exactly what it was before, which is the failure mode this
   * is allowed to have.
   */
  protected def assumeEnsures(name: String, args: List[TExpr], result: Option[Val]): Unit =
    for
      (f, written) <- contracted.get(name).toList
      (clause, _)  <- f.ensures
      if !ghostly(clause)
      ported <- ContractAssume.atCall(clause, f.params, args, written, result.isDefined,
                                      callerExposed ++ promoted)
    do
      val saved = resultSSA

      resultSSA = result
      tryPure {
        pushTemps()
        val ok = genExpr(ported)

        popTemps()
        ok
      } match
        case Some(ok) if ok != Val.Nothing =>
          usesAssume = true
          emit(Inst.Call(None, LType.Void, Val.Global(Llvm.assume.name), List(Arg(i1, ok))))
        case _ => ()
      resultSSA = saved

  /** Every struct's synthesised `invariant` function by the name it is emitted under — one per
   * instantiation, for a generic struct — for the assume below to read its clause out of.
   */
  private lazy val invariantFuncs: Map[String, TFunc] =
    program.funcs.filter(_.name.contains("$inv")).map(f => f.name -> f).toMap

  /** **What a member is told about its receiver** — the struct's `invariant`, laid down as an
   * `llvm.assume` on entry to every function whose receiver is a struct that carries clauses
   * (`reference/verification.md § What the optimizer is told`).
   *
   * A clause is checked at every write of the struct, at its construction and at its zero, so every
   * value of the type a member can be handed is one that passed the check: control reaching a
   * member is control that got past the trap, exactly as it is after a callee's `ensure`. What
   * the fact buys is a test the member does not have to make twice — `Buf.at` compares the index
   * with `count` to panic with the length, and the slice under it compares it with `elems.len`;
   * told `count <= elems.len`, the second compare is implied by the first and folds.
   *
   * **What it rests on is the checking, so what the checking cannot reach is outside it**, as it is
   * for every other guarantee about memory reached through a raw pointer
   * (`reference/memory.md`): bytes written into the struct through a `*T` of another type, or by
   * C, are the writer's promise to keep.
   *
   * Only a clause that lowers to arithmetic and comparisons over the receiver's fields is repeated,
   * by the test `assumeEnsures` uses (`tryPure`) — one that calls something, divides, or allocates
   * is left as the check it already is.
   */
  protected def assumeReceiverInvariant(f: TFunc): Unit =
    f.params.headOption match
      case Some(("self", ty)) =>
        val (recv, target) = Type.unqualified(ty) match
          case s: Type.Struct => (Some(TLoad("self", ty)), Some(s))
          case p: Type.Ptr =>
            Type.pointee(p).map(Type.unqualified) match
              case Some(s: Type.Struct) => (Some(TDeref(TLoad("self", ty), s)), Some(s))
              case _                    => (None, None)
          case _ => (None, None)

        for
          r   <- recv
          s   <- target
          inv <- invariantFuncs.get(Type.mangled(s"${s.base}$$inv", s.targs))
          if inv.body.stmts.isEmpty && inv.params.length == s.fields.length
          clause <- inv.body.result
          if !ghostly(clause)
          ported <- ContractAssume.substitute(clause,
                      inv.params.map(_._1).zip(s.fields.zipWithIndex.map { case ((_, ft), i) =>
                        TField(r, i, ft)
                      }).toMap)
        do
          tryPure {
            pushTemps()
            val ok = genExpr(ported)

            popTemps()
            ok
          } match
            case Some(ok) if ok != Val.Nothing =>
              usesAssume = true
              emit(Inst.Call(None, LType.Void, Val.Global(Llvm.assume.name), List(Arg(i1, ok))))
            case _ => ()
      case _ => ()

  /** Sets up a `variant`'s two slots at the point the loop is entered (`reference/verification.md §
   * invariant and variant on a loop`), then emits the loop.
   *
   * The `armed` flag starts false, which is what lets the first iteration pass with nothing to
   * compare against. It is stored **here** rather than in the function's prologue because a loop
   * nested inside another is entered many times: a flag armed once per call would compare the second
   * entry's first measure against the first entry's last, and trap on a loop that was decreasing
   * perfectly well.
   */
  protected def genCheckedLoop(slot: String, varTy: Type, loop: TExpr): Val = {
    emitAlloca(Val.Reg(s"$slot.prev"), varTy.lty)
    emitAlloca(Val.Reg(s"$slot.armed"), i1)
    emit(Inst.Store(LType.I(1), Val.Int(0), Val.Reg(s"$slot.armed"), Access.Plain))
    genExpr(loop)
  }

  /** One iteration's `variant` check: measure, compare against the last one where there was a last
   * one, and store.
   *
   * The comparison is **strict** and it is signed or unsigned according to the measure's own type,
   * which `compareValue` reads off it — a `usize` counting down to zero is as ordinary a measure as
   * an `int` is, and comparing it as signed would be wrong at exactly the values it spends its time
   * near.
   */
  protected def genVariantCheck(v: TVariantCheck): Unit = {
    val TVariantCheck(slot, varTy, expr) = v
    val w = varTy.lty

    pushTemps()
    val cur = genExpr(expr)
    popTemps()

    val armedV = freshReg(); emit(Inst.Load(armedV, LType.I(1), Val.Reg(s"$slot.armed"), Access.Plain))
    val cmpL   = freshLabel("variant.cmp")
    val setL   = freshLabel("variant.set")

    emitTerm(Inst.CondBr(armedV, cmpL, setL))
    emitLabel(cmpL)
    val prev = freshReg(); emit(Inst.Load(prev, w, Val.Reg(s"$slot.prev"), Access.Plain))
    val ok   = freshReg(); emit(Inst.IntCmp(ok, intPred("<", varTy), w, cur, prev))
    trapUnless(ok, "variant")
    emitTerm(Inst.Br(setL))
    emitLabel(setL)
    emit(Inst.Store(w, cur, Val.Reg(s"$slot.prev"), Access.Plain))
    emit(Inst.Store(LType.I(1), Val.Int(1), Val.Reg(s"$slot.armed"), Access.Plain))
  }

  /** Emits the `within`-range checks for a value produced into a constrained subtype: a lower- and
   * upper-bound compare, each trapping on violation. Integer and `char` bounds compare at the base
   * width (unsigned for `char` and the unsigned integers, which `compareValue` reads off the type);
   * float bounds compare in double precision, widening a narrower value so one rendering of the
   * bound serves every float width.
   */
  private def emitRangeChecks(v: Val, c: Type.Constrained): Unit =
    Type.underlying(c.base) match
      case f: Type.Floating =>
        val wide =
          if f.bits == 64 then v
          else
            val r = freshReg()

            emit(Inst.Cast(r, CastOp.FPExt, f.lty, v, LType.F(64)))
            r
        for lo <- c.lo do trapUnless(fcmpConst(FCmp.Oge, wide, lo), "within")
        for hi <- c.hi do trapUnless(fcmpConst(if c.exclusiveHi then FCmp.Olt else FCmp.Ole, wide, hi), "within")
      case base =>
        for lo <- c.lo do trapUnless(compareValue(">=", base, v, Val.Int(lo.toBigInt)), "within")
        for hi <- c.hi do
          trapUnless(compareValue(if c.exclusiveHi then "<" else "<=", base, v, Val.Int(hi.toBigInt)),
                     "within")

  /** Everything a constrained subtype asks of a value: the `within` range, then the `where`
   * predicate — a synthesised `i1`-returning function over the base value, which traps exactly as
   * the range does when it answers false. Shared by the checking node and by the two forms that
   * compute and store in one step, so a value cannot reach a constrained slot by a path that tests
   * less of it than another.
   */
  protected def emitConstraintChecks(v: Val, c: Type.Constrained): Unit = {
    emitRangeChecks(v, c)

    for pf <- c.predFn do
      val r = freshReg()

      emit(Inst.Call(Some(r), i1, Val.Global(pf), List(Arg(Type.underlying(c.base).lty, v))))
      trapUnless(r, "where")
  }

  private def fcmpConst(pred: FCmp, wide: Val, bound: BigDecimal): Val = {
    val r = freshReg()

    emit(Inst.FloatCmp(r, pred, LType.F(64), wide, Val.float(bound.toDouble)))
    r
  }

  /** Checks a struct value against its `invariant` function: read each stored field out of the
   * aggregate `v`, call `invFn`, and trap on a false result. The fields are handed over exactly as
   * an ordinary call's arguments are — the callee borrows, so no count is taken here.
   */
  protected def emitInvCheck(v: Val, struct: Type.Struct, invFn: String): Unit =
    val container = Bitfields.of(struct).map { ranges =>
      val c = freshReg(); emit(Inst.Extract(c, struct.lty, v, List(0))); (ranges, c)
    }

    val args = struct.fields.zipWithIndex.collect {
      case ((_, ft), i) if !Type.zeroSized(ft) =>
        // A bitfield struct's fields are ranges of one container rather than slots of an aggregate,
        // so they are read out of it once it has been lifted out — which is the same one read
        // whether the invariant relates one field or all of them.
        val r = container match
          case Some((ranges, c)) => readBits(ranges, ranges(struct.slot(i)), c)
          case None =>
            val t = freshReg()

            emit(Inst.Extract(t, struct.lty, v, List(struct.slot(i))))
            t

        Arg(ft.lty, r)
    }
    val ok = freshReg()

    emit(Inst.Call(Some(ok), i1, Val.Global(invFn), args.toList))
    trapUnless(ok, "invariant")
}
