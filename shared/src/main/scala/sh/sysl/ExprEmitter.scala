package sh.sysl

import ir.{Access, Arg, BinOp, CastOp, ICmp, Inst, LType, Val}

/** The expression dispatch.
 *
 * This is the widest single `match` in the compiler and it stays one, because that is what it is:
 * every arm's outermost pattern is a node type, so the cases cannot shadow each other and the
 * compiler's exhaustiveness check is the guarantee that a new node has somewhere to go. Splitting
 * it across traits would trade that for a fall-through nobody could read.
 *
 * What the arms have in common is that none of them decides anything. A type is read off the node,
 * an instruction is selected from it, and the work that is more than an instruction has already
 * been lifted into a named method — `genSlice`, `genMatch`, `genTry`, `genForEach` on the traits
 * underneath, and the call seam and the arithmetic on `CallEmitter` and `ArithEmitter`. An arm that
 * is still long here is long because the *emitted code* is long, not because a decision is being
 * made in it.
 */
trait ExprEmitter extends ArithEmitter {

  /** Lowers an expression, returning the register or immediate holding its value (empty for a
   * unit-typed expression, whose value is never read).
   */
  protected def genExpr(expr: TExpr): Val = expr match
    case TIntLit(v, _) => Val.Int(v)
    case TStrLit(s)    => stringValue(s)
    // Every interned constant is already laid down NUL-terminated, so a C string is the same global
    // read as a plain pointer — the terminator the sysl string ignores is exactly what C reads by.
    case TCStrLit(s)   => stringGlobal(s)
    case TBoolLit(b)   => Val.Int(if b then 1 else 0)
    // A trait object is two words, so its null is a zeroed pair rather than a bare address.
    case TNullLit(ty)  => zero(ty)
    case TUnitLit()    => Val.Nothing
    case TZero(ty)     => zero(ty)

    // A large one is built where it is going to live and read back out only because this caller
    // asked for a value; a small one is the `insertvalue` chain it always was.
    case e @ TArrayLit(_, arrayTy) if layout.indirect(arrayTy) => throughSlot(e)

    case TArrayLit(elems, arrayTy) =>
      val vals = elems.map(genExpr)
      var acc: Val = Val.Zero
      for (v, i) <- vals.zipWithIndex do
        val r = freshReg()
        emit(Inst.Insert(r, arrayTy.lty, acc, arrayTy.elem.lty, v, List(i)))
        acc = r
      acc

    // A vector is built lane by lane with `insertelement`, which is the array literal's
    // `insertvalue` chain in the register file — and never through a slot, however wide it is: a
    // vector has no `layout.indirect` case because a value that is not storage cannot be built in
    // storage. LLVM folds a chain of constant lanes into a literal, so a splat of constants costs
    // nothing at run time.
    case TVectorLit(lanes, vecTy) =>
      val vals = lanes.map(genExpr)
      var acc: Val = Val.Zero
      for (v, i) <- vals.zipWithIndex do
        val r = freshReg()
        emit(Inst.InsertElement(r, vecTy.lty, acc, vecTy.elem.lty, v,
          Arg(i32, Val.Int(i))))
        acc = r
      acc

    // The splat, in LLVM's own idiom: put the value in lane zero and shuffle it across with an
    // all-zero mask. `shufflevector` with a zeroinitializer mask is the pattern every back end
    // recognises, and it lowers to one broadcast instruction where the machine has one.
    case TSplat(value, vecTy) =>
      val v   = genExpr(value)
      val one = freshReg()
      emit(Inst.InsertElement(one, vecTy.lty, Val.Poison, vecTy.elem.lty, v,
        Arg(i32, Val.Int(0))))
      val r = freshReg()
      emit(Inst.Shuffle(r, vecTy.lty, one, Val.Poison,
        Arg(LType.Vec(vecTy.length, i32), Val.Zero)))
      r

    // A lane-wise comparison is the scalar instruction at the register's width — `fcmp olt
    // <4 x float>` yields the `<4 x i1>` that is this language's `<4>bool`. The predicate is chosen
    // exactly as the scalar path chooses it, from the lane's own type and signedness.
    case TVecCompare(op, l, r, _) =>
      val lv    = genExpr(l)
      val rv    = genExpr(r)
      val vecTy = Type.repr(l.ty).asInstanceOf[Type.Vector]
      val lane  = Type.underlying(vecTy.elem)
      val res   = freshReg()

      emit(
        if lane.isInstanceOf[Type.Floating] then
          Inst.FloatCmp(res, floatPred(op), vecTy.lty, lv, rv)
        else Inst.IntCmp(res, intPred(op, lane), vecTy.lty, lv, rv))
      res

    // Both sides are evaluated and the mask picks between them, which is the whole difference from
    // an `if` and is why this is not one (`tast.scala`'s `TSelect`).
    case TSelect(mask, whenTrue, whenFalse, ty) =>
      val m = genExpr(mask)
      val a = genExpr(whenTrue)
      val b = genExpr(whenFalse)
      val r = freshReg()

      emit(Inst.Select(r, m, ty.lty, a, b, mask.ty.lty))
      r

    case TReduce(op, receiver, ty) =>
      val v     = genExpr(receiver)
      val vecTy = Type.repr(receiver.ty).asInstanceOf[Type.Vector]
      val name  = Llvm.reduce(op).at(vecTy.lty)
      val r     = freshReg()

      // **The float sum takes a starting accumulator and the others do not**, which is not a
      // symmetry LLVM chose lightly: floating addition is not associative, so the intrinsic makes
      // you say what to start from and whether the order may be changed. `-0.0` is the identity —
      // `0.0` would turn a sum of nothing but negative zeros positive — and `reassoc` is what
      // licenses the tree that makes this worth doing at all rather than a left fold in disguise.
      val (params, args, flags) =
        if op == "fadd" then
          (List(ir.Param(vecTy.elem.lty), ir.Param(vecTy.lty)),
           List(Arg(vecTy.elem.lty, Val.float(-0.0)), Arg(vecTy.lty, v)),
           List(ir.FastMath.Reassoc))
        else (List(ir.Param(vecTy.lty)), List(Arg(vecTy.lty, v)), Nil)

      satDecls += ir.FuncSig(name, ir.FnType(ty.lty, params))
      emit(Inst.Call(Some(r), ty.lty, Val.Global(name), args, fast = flags))
      r

    // One lane, read straight out of the register. There is no address and so no bounds test: the
    // index was held to a constant in range where it was written, which is what `TLane` is for.
    case TLane(receiver, lane, ty) =>
      val v = genExpr(receiver)
      val r = freshReg()

      emit(Inst.ExtractElement(r, Type.repr(receiver.ty).lty, v,
        Arg(i32, Val.Int(lane))))
      r

    // **The alignment is the element's, not the register's, and that is what a slice can promise.**
    // A `[]f32` says its elements are four-byte-aligned and says nothing whatever about where the
    // run begins — a slice of one is a slice of any of them. Claiming the vector's own alignment
    // would be a promise the type does not make, and an over-aligned `load` is not a slow load but
    // undefined behaviour the moment it is wrong. Every machine sysl targets has an unaligned
    // vector load that costs the same as the aligned one on aligned data, so the honest number is
    // also the free one.
    case TVecLoad(receiver, index, vecTy) =>
      val p = runAddr(receiver, index, vecTy.length)
      val r = freshReg()

      emit(Inst.Load(r, vecTy.lty, p, Access.Plain, Some(layout.align(vecTy.elem))))
      r

    case TVecStore(receiver, index, value) =>
      val vecTy = Type.repr(value.ty).asInstanceOf[Type.Vector]
      // The value first and the address second, which is the order `TStore` uses — this *is* an
      // assignment to a run of elements, and the two spellings must not differ in when a side
      // effect in the value happens relative to the bounds check on the run.
      val v = genExpr(value)
      val p = runAddr(receiver, index, vecTy.length)

      emit(Inst.Store(vecTy.lty, v, p, Access.Plain, Some(layout.align(vecTy.elem))))
      Val.Nothing

    // Built through memory with a loop rather than as an `insertvalue` chain, for the reason the
    // ARC walk gives: the count is a compile-time constant but it can be very large, and a repeat
    // count is where someone writes a large one on purpose. The value is generated once, above the
    // loop — every element is a copy of that one evaluation. Its references are borrowed here and
    // retained by whatever binds the array, whose ARC walk visits all n elements.
    case TArrayFill(value, arrayTy) if arrayTy.length == 0 =>
      genExpr(value); Val.Zero

    case e: TArrayFill => throughSlot(e)

    // The same two forms, sized and owned rather than laid out in a frame. Each element the buffer
    // takes is a share of its own — the box holds them until its hook lets them go — so the value
    // that lands in one is retained as it is stored, exactly as a slot that binds an array is.
    // An empty view has no storage to make, so it is the zero value of its own representation:
    // `{owner = null, data = null, len = 0}`. It is not `ownTemp`'d either — there is no box for a
    // release to let go of, and a null owner is what a view that owns nothing already carries. The
    // `TArrayFill` case above is the same observation about the length being zero, one form over.
    case TBufLit(Nil, _) => Val.Zero

    case TBufLit(elems, sliceTy) =>
      val vals       = elems.map(genExpr)
      val (box, data) = genBuffer(sliceTy.elem, Val.Int(vals.length))

      for (v, i) <- vals.zipWithIndex do
        retainValue(sliceTy.elem, v)
        val ep = freshReg(); emit(Inst.Gep(ep, sliceTy.elem.lty, data, List(Arg(wordLty, Val.Int(i)))))
        emit(Inst.Store(sliceTy.elem.lty, v, ep, Access.Plain))

      bufferView(sliceTy, box, data, Val.Int(vals.length))

    // The value is generated once, above the loop, which is what makes `[tick(); n]` one call
    // whose result lands in n places — the repeat's own rule (`07`), and the reason the count may
    // be zero and the call still happen.
    case TBufFill(value, count, sliceTy) =>
      val v           = genExpr(value)
      val n           = widenIndex(count)
      val (box, data) = genBuffer(sliceTy.elem, n)

      fillElements(sliceTy.elem, data, n, v)
      bufferView(sliceTy, box, data, n)

    case e @ TIndex(receiver, index, ty) =>
      val p = elementAddr(receiver, index)
      val r = freshReg(); emit(Inst.Load(r, ty.lty, p, access(e))); r

    // A whole array literal viewed, where the view **escapes**: the storage is the buffer form's
    // rather than the frame's, which is what an array literal written where a `[]T` is expected
    // already gets (`Escape.Promotions.temporaries`). Everything else about the node is unchanged —
    // it is the same elements at the same slice type, laid somewhere that outlives the frame.
    case slice @ TSlice(TArrayLit(elems, _), None, None, _, sliceTy: Type.Slice)
      if slice.pos.exists(promotedTemps) =>
      genExpr(TBufLit(elems, sliceTy))

    case slice @ TSlice(TArrayFill(value, Type.Array(n, _)), None, None, _, sliceTy: Type.Slice)
      if slice.pos.exists(promotedTemps) =>
      genExpr(TBufFill(value, TIntLit(n, Type.usize), sliceTy))

    // Any **other** whole temporary array whose view escapes: the value the frame just computed is
    // written into a buffer instead of into the slot it would otherwise have had, and the view
    // counts against that buffer. A call answering a `[N]T` is the shape this exists for — card
    // `0394` — and it is the same three steps a promoted *declaration* takes (`Codegen`'s
    // `TVarDecl` for a promoted array), which is what says this is a lowering and not a new idea.
    //
    // The two literal forms above are the same case done better: they are built straight into the
    // buffer rather than produced and copied, which is worth the two lines it costs. Everything
    // else has no such form, so the value is made and moved.
    //
    // The retain is the one that path takes and for the same reason: the value is still the
    // statement's to release, and the buffer is a second holder of whatever it counts.
    case slice @ TSlice(base, None, None, _, sliceTy: Type.Slice)
      if slice.pos.exists(promotedTemps) =>
      val Type.Array(n, elem) = base.ty: @unchecked
      val v                   = genExpr(base)
      val (box, data)         = genBuffer(elem, Val.Int(n))

      retainValue(base.ty, v)
      emit(Inst.Store(base.ty.lty, v, data, Access.Plain))
      bufferView(sliceTy, box, data, Val.Int(n))

    case TSlice(base, lo, hi, inclusive, sliceTy) =>
      genSlice(base, lo, hi, inclusive, sliceTy)

    case TLen(receiver, _) =>
      receiver.ty match
        case Type.Array(n, _) => genExpr(receiver); Val.Int(n)
        case w: Type.View =>
          val v = genExpr(receiver)
          val r = freshReg(); emit(Inst.Extract(r, w.lty, v, List(2))); r
        case other => sys.error(s"unreachable length of ${other.llvm}")

    // A string and a `[]u8` are the same three words, so looking at one as the other is nothing
    // at all — the guarantee that was given up is the analyzer's business, not the machine's.
    case TBytes(receiver) =>
      genExpr(receiver)

    // A narrower float is the `double` constant rounded to it, which folds away entirely.
    case TFloatLit(bits, ty) =>
      if ty == Type.Real then Val.Float(bits)
      else
        val r = freshReg()

        emit(Inst.Cast(r, CastOp.FPTrunc, LType.F(64), Val.Float(bits), ty.lty))
        r

    case TCast(operand, ty) =>
      // A constrained operand converts from its base representation — `f64(m)` reaches the double a
      // `Meters` is, `int(age)` the i32 an `Age` is.
      convert(Type.underlying(operand.ty), ty, genExpr(operand))

    case TConstrainedValid(value, c) =>
      val v    = genExpr(value)
      val base = Type.underlying(c.base)
      val geLo = compareValue(">=", base, v, Val.Int(c.lo.get.toBigInt))
      val leHi = compareValue(if c.exclusiveHi then "<" else "<=", base, v, Val.Int(c.hi.get.toBigInt))
      val r = freshReg(); emit(Inst.Bin(r, BinOp.And, i1, geLo, leHi)); r

    case TConstrainedStep(value, c, up, _) =>
      val v    = genExpr(value)
      val base = Type.underlying(c.base)
      val last = if c.exclusiveHi then c.hi.get - 1 else c.hi.get
      // `Succ` traps at `Last`, `Pred` at `First`; the value is otherwise one step along the base.
      if up then trapUnless(compareValue("<", base, v, Val.Int(last.toBigInt)), "succ")
      else trapUnless(compareValue(">", base, v, Val.Int(c.lo.get.toBigInt)), "pred")
      val r = freshReg()

      emit(Inst.Bin(r, if up then BinOp.Add else BinOp.Sub, base.lty, v, Val.Int(1)))
      r

    case TConstrainedCheck(value, target) =>
      val v = genExpr(value)
      emitConstraintChecks(v, target)
      v

    case TStructInvCheck(value, struct, invFn) =>
      val v = genExpr(value)
      emitInvCheck(v, struct, invFn)
      v

    case TRecheck(after, recv, struct, invFn) =>
      // The write or the call runs first (yielding the value the expression is), then the struct it
      // could have changed is re-read so the invariant sees the new fields.
      val v = genExpr(after)
      emitInvCheck(genExpr(recv), struct, invFn)
      v

    // Nothing is stored for a zero-sized binding, so there is nothing to read back.
    case TLoad(_, ty) if Type.zeroSized(ty) => Val.Nothing

    // The qualifier comes off the value's type for an ordinary local, whose slot is its own, and
    // off the recorded storage for a `ref`, whose slot is somebody else's and may be a register
    // (`reference/memory.md § ref — a name for a place`, `reference/memory.md § Device memory`).
    case TLoad(name, ty) =>
      val acc = accessOf(if refStorage.contains(name) then refStorage(name) else ty)
      val r   = freshReg()

      emit(Inst.Load(r, ty.lty, Val.Reg(s"$name.addr"), acc))
      r

    case TResult(_) =>
      resultSSA.getOrElse(sys.error("'result' lowered outside an ensure postcondition"))

    case TOld(index, ty) =>
      val r = freshReg()

      emit(Inst.Load(r, ty.lty, Val.Reg(s"old.$index.addr"), Access.Plain))
      r

    case TGlobal(symbol, ty, _) =>
      val r = freshReg()

      emit(Inst.Load(r, ty.lty, Val.Global(symbol), accessOf(ty)))
      r

    case e @ TDeref(operand, ty) =>
      val p = payloadAddr(operand)
      val r = freshReg(); emit(Inst.Load(r, ty.lty, p, access(e))); r

    case TAddrOf(place, _) =>
      address(place)

    // `o::Id` — the first word of the table the object points at (`VtableEmitter`). The table is a
    // constant global, so this is a load from read-only storage and nothing else.
    case TTypeId(receiver, _) =>
      val obj   = genExpr(receiver)
      val table = freshReg(); emit(Inst.Extract(table, LType.fat, obj, List(0)))
      val r     = freshReg(); emit(Inst.Load(r, wordLty, table, Access.Plain))
      r

    // `&value` — the storage a computed value has no address for. The slot is an ordinary alloca, so
    // it belongs to the **frame** rather than to the block that wrote it, and the count it takes is
    // the one a `var`'s slot takes: registered with the scope being emitted, released where that
    // scope ends. Everything about the pointer afterwards is what a `*T` always is.
    case TTempAddr(value, _) =>
      val slot = emitAlloca(freshReg(), value.ty.lty)

      genOwnedInto(slot, value)
      ownAt(slot, value.ty)
      slot

    case TBox(value, refTy) =>
      genBox(value, refTy)

    case TStore(place, value, ty) if Type.zeroSized(ty) =>
      genExpr(value)
      address(place)
      Val.Nothing

    // The three forms that write through a place go through `placeAddr`/`loadPlace`/`storePlace`
    // rather than through `address` and a `load`, because a **bitfield** is a range of a container
    // and has no address of its own (`Bitfields`). For every other place those three are the address
    // and the load and the store, unchanged.
    case TStore(place, value, ty) =>
      val v = genExpr(value)
      val p = placeAddr(place)
      storePlace(place, p, v)
      v

    case TUpdate(place, op, value, ty, dispatch, check) =>
      val p       = placeAddr(place)
      val cur     = loadPlace(place, p)
      val v       = genExpr(value)
      val updated = combine(op, ty, value.ty, dispatch, cur, v)

      for c <- check do emitConstraintChecks(updated, c)

      storePlace(place, p, updated, Some(cur))
      updated

    case TIncDec(place, op, pre, ty, check) =>
      val p   = placeAddr(place)
      val cur = loadPlace(place, p)
      val nv  = freshReg()

      emit(Inst.Bin(nv, if op == "++" then BinOp.Add else BinOp.Sub, ty.lty, cur,
        Val.Int(1)))

      for c <- check do emitConstraintChecks(nv, c)

      storePlace(place, p, nv, Some(cur))
      if pre then nv else cur

    case TStr(arg) =>
      // A string renders to itself, its count already the argument's; every other type renders
      // into a fresh buffer this statement owns and releases.
      val v = genStr(arg)
      if arg.ty == Type.Str then v else ownTemp(v, Type.Str)

    case TFormat(arg, spec) =>
      ownTemp(genFormat(arg, spec), Type.Str)

    // Nothing to emit: dropping the ability to write is a fact about the type and not about the
    // three words, which are the same three words either way.
    case TConstView(arg) => genExpr(arg)

    // The bytes are copied into a string that owns them rather than viewed in place: a `[]u8` can
    // be written through afterwards, and a `string` whose bytes could change is not one that was
    // ever validated. The same copy is what `str(x)` finishes through, so both spend one allocation.
    case TFromBytes(arg) =>
      heap = true
      val fn  = requestText("sysl.str.from_bytes")(StringEmitter.fromBytes)
      val v   = genExpr(arg)
      val p   = freshReg(); emit(Inst.Extract(p, arg.ty.lty, v, List(1)))
      val n   = freshReg(); emit(Inst.Extract(n, arg.ty.lty, v, List(2)))
      val r   = freshReg(); emit(Inst.Call(Some(r), Type.Str.lty, Val.Global(fn), List(Arg(LType.Ptr, p), Arg(wordLty, n))))
      ownTemp(r, Type.Str)

    // The raw tier's other direction: the same three words taken as a string, sharing the slice's
    // owner. Nothing to emit, exactly as for `s.bytes` — a string that is kept takes its own share
    // of that owner where it is stored, as any view does.
    case TStrView(arg) => genExpr(arg)

    // Rendering into a buffer: a zeroed stack slot becomes the sink, the value writes itself into
    // it, and what landed there is copied into a string the statement owns. The slot is re-zeroed
    // on every arrival rather than once, since an alloca is hoisted to the entry block and a render
    // inside a loop meets the same one each time round.
    case TRender(value, method, spec, vslot) =>
      heap = true
      requestText("sysl.str.from_bytes")(StringEmitter.fromBytes)

      val table = bufferTable()
      // **A large receiver crosses as the address of storage the caller holds**, exactly as it does
      // at any other call (`CallEmitter.argValue`) — a value past the indirect boundary is passed
      // and received through memory, so its parameter is a pointer. Producing it as a first-class
      // aggregate and handing that over instead put the struct's first word where the callee reads
      // an address, which is a `SIGSEGV` with nothing to read: `print(x)` went through an ordinary
      // call and worked, so a type could render all through development and die the first time it
      // was put in a string.
      val big   = layout.indirect(value.ty)
      val v     = if big then address(value) else genExpr(value)
      val s     = genExpr(spec)
      val slot  = emitAlloca(freshReg(), bufferLty)

      emit(Inst.Store(bufferLty, Val.Zero, slot, Access.Plain))
      val a = freshReg()

      emit(Inst.Insert(a, LType.fat, Val.Undef, LType.Ptr, Val.Global(table), List(0)))
      val w = freshReg(); emit(Inst.Insert(w, LType.fat, a, LType.Ptr, slot, List(1)))

      // A trait object renders through the table it carries, so the callee and the receiver both
      // come out of the value: the data word is the receiver a slot's entry expects, exactly as it
      // is for any other call through one.
      vslot match
        case Some(n) =>
          val vt   = freshReg(); emit(Inst.Extract(vt, LType.fat, v, List(0)))
          val data = freshReg(); emit(Inst.Extract(data, LType.fat, v, List(1)))
          val e    = freshReg(); emit(Inst.Gep(e, vtableLty, vt,
                       List(Arg(LType.I(32), Val.Int(0)), Arg(LType.I(32), Val.Int(1)),
                            Arg(wordLty, Val.Int(n)))))
          val fn   = freshReg(); emit(Inst.Load(fn, LType.Ptr, e, Access.Plain))

          emit(Inst.Call(None, LType.Void, fn, List(Arg(LType.Ptr, data), Arg(LType.fat, w), Arg(spec.ty.lty, s))))
        case None =>
          emit(Inst.Call(None, LType.Void, Val.Global(method),
            List(Arg(if big then LType.Ptr else value.ty.lty, v), Arg(LType.fat, w), Arg(spec.ty.lty, s))))

      val r = freshReg()
      emit(Inst.Call(Some(r), Type.Str.lty, Val.Global("sysl.w.buf.finish"), List(Arg(LType.Ptr, slot))))
      ownTemp(r, Type.Str)

    case TBinary(_, l, r, Type.Str) =>
      ownTemp(strConcat(genExpr(l), genExpr(r)), Type.Str)

    /** `p - q`, C's `ptrdiff_t`. The bytes between the two addresses, divided by the pointee's size
      * so the answer counts elements — the inverse of `&p[n]`, which strides by the same size.
      *
      * The divide is skipped for a one-byte pointee, which is not an optimization so much as the
      * common case: a `*u8` walk over bytes is what `memchr` and every other interior-pointer libc
      * function hands back, and `sdiv` by 1 would be noise in the emitted text a reader has to
      * discount.
      */
    case TBinary("-", l, r, _) if Type.underlying(l.ty).isInstanceOf[Type.Ptr] =>
      val stride = Type.underlying(l.ty) match
        case Type.Ptr(e) => layout.size(e)
        case _           => 1
      val (lv, rv) = (genExpr(l), genExpr(r))
      val (la, ra) = (freshReg(), freshReg())
      emit(Inst.Cast(la, CastOp.PtrToInt, LType.Ptr, lv, Type.isize.lty))
      emit(Inst.Cast(ra, CastOp.PtrToInt, LType.Ptr, rv, Type.isize.lty))
      val bytes = freshReg()
      emit(Inst.Bin(bytes, BinOp.Sub, Type.isize.lty, la, ra))
      if stride <= 1 then bytes
      else
        val n = freshReg()
        emit(Inst.Bin(n, BinOp.SDiv, Type.isize.lty, bytes, Val.Int(stride)))
        n

    case TBinary(op, l, r, _) =>
      // Arithmetic runs at the base representation — a derived type keeps its own identity in the
      // analyzer but is added, multiplied, and divided as the base it is laid out as. When an operand
      // was produced through a ranged type and the result could leave the base width, the operation
      // is overflow-detecting so a wrap cannot slip past the produce-site range check.
      val bt  = Type.underlying(l.ty)
      val lv  = genExpr(l)
      val rv0 = genExpr(r)
      // A shift's count may be any integer width, so it is brought to the shifted value's own here —
      // every instruction below takes two operands of one type. See `ScalarEmitter.shiftAmount`.
      val rv  = if op == "<<" || op == ">>" then shiftAmount(bt, r.ty, rv0) else rv0

      bt match
        case it: Type.Integer if op == "<<" && isRanged(l.ty)  => checkedShl(it, lv, rv)
        case it: Type.Integer if overflowChecked(op, l, r, it) => checkedArith(op, it, lv, rv)
        case _                                                 => arith(op, bt, lv, rv)

    // The vector cases come first, because `opSubject` reads through the register to the lane and
    // the two scalar guards below would otherwise never see one. `zeroinitializer` is the whole
    // vector's zero, so the integer form needs no splat written out.
    case TUnary("-", operand, ty) if Type.opSubject(ty).isInstanceOf[Type.Integer] && Type.repr(ty).isInstanceOf[Type.Vector] =>
      val v = genExpr(operand); val r = freshReg(); emit(Inst.Bin(r, BinOp.Sub, ty.lty, Val.Zero, v)); r
    case TUnary("-", operand, ty) if Type.underlying(ty).isInstanceOf[Type.Integer] =>
      val v = genExpr(operand); val r = freshReg(); emit(Inst.Bin(r, BinOp.Sub, ty.lty, Val.Int(0), v)); r
    case TUnary("-", operand, ty) if Type.opSubject(ty).isInstanceOf[Type.Floating] =>
      val v = genExpr(operand); val r = freshReg(); emit(Inst.Neg(r, ty.lty, v)); r
    case TUnary("!", operand, _) =>
      val v = genExpr(operand); val r = freshReg(); emit(Inst.Bin(r, BinOp.Xor, i1, v, Val.Bool(true))); r
    // A vector's complement flips every lane, and its all-ones constant is written `splat (iN -1)`
    // rather than as the bare `-1` a scalar takes.
    case TUnary("~", operand, ty) if Type.repr(ty).isInstanceOf[Type.Vector] =>
      val v    = genExpr(operand)
      val lane = Type.repr(ty).asInstanceOf[Type.Vector].elem
      val r    = freshReg()
      emit(Inst.Bin(r, BinOp.Xor, ty.lty, v, Val.Splat(lane.lty, Val.Int(-1))))
      r
    case TUnary("~", operand, ty) =>
      val v = genExpr(operand); val r = freshReg(); emit(Inst.Bin(r, BinOp.Xor, ty.lty, v, Val.Int(-1))); r
    case TUnary(op, _, _) =>
      sys.error(s"unreachable unary '$op'")

    // A fence orders the accesses around it and touches nothing, so there is no address to compute
    // and no value to hand back. `syncscope` is deliberately absent: the default is the whole
    // system, which is what a program sharing memory with another thread or a device needs.
    case TFence(ord) =>
      emit(Inst.Fence(Atomics.llvm(ord)))
      Val.Int(0)

    // One atomic access. `at` rather than the node's own type decides the width, since a store
    // answers nothing and would otherwise lower at `void`; the alignment is stated because LLVM
    // requires it on an atomic load or store and will not infer one.
    case TAtomic(op, addr, ops, ord, at, ty) =>
      val p    = genExpr(addr)
      val vs   = ops.map(genExpr)
      val ll   = at.lty
      val ordr = Atomics.llvm(ord)
      val acc  = Access.Atomic(ordr)
      val al   = Some(layout.align(at))

      op match
        case "atomic_load" =>
          val r = freshReg(); emit(Inst.Load(r, ll, p, acc, al)); r
        case "atomic_store" =>
          emit(Inst.Store(ll, vs.head, p, acc, al))
          Val.Int(0)
        // `cmpxchg` answers a pair — the value it found and whether it swapped — and what this hands
        // back is the value. A caller comparing it against what they expected learns the same thing
        // the flag would have told them, which is why the raw form has one result rather than a
        // tuple the library would immediately take apart.
        case "atomic_cas" =>
          val pair = freshReg()
          emit(Inst.CmpXchg(pair, p, ll, vs.head, vs(1), ordr))
          val r = freshReg()
          emit(Inst.Extract(r, LType.Struct(List(ll, i1)), pair, List(0)))
          r
        // The rest are one `atomicrmw`, which answers the value that was there *before* — the
        // property that makes an atomic increment usable as a ticket.
        case _ =>
          val kind = op.stripPrefix("atomic_") match
            case "swap" => ir.RmwOp.Xchg
            case "add"  => ir.RmwOp.Add
            case "sub"  => ir.RmwOp.Sub
            case "and"  => ir.RmwOp.And
            case "or"   => ir.RmwOp.Or
            case "xor"  => ir.RmwOp.Xor
            case other  => sys.error(s"unreachable atomic '$other'")
          val r = freshReg()
          emit(Inst.AtomicRmw(r, kind, p, ll, vs.head, ordr))
          r

    // The operand is read into a register once and the comparisons index off that, which is the
    // whole reason these are a node rather than a tree of the operators they mean
    // (`reference/expressions.md § Operator dispatch`).
    case TIntOp(op, operand, amount, width, ty) =>
      val v    = genExpr(operand)
      val n    = amount.map(genExpr)
      val bits = width.asInstanceOf[Type.Integer].bits
      val ll   = width.lty

      // The bits counted at the operand's own width, then resized to the `u32` the count is
      // answered in. A count is at most the width, so narrowing one from a type wider than 32 bits
      // cannot lose anything.
      def counted(base: String, of: Val, zeroFlag: Boolean = false) =
        resize(intrinsic(base, ll, List(of), zeroFlag), width, ty)

      // `~x`, which is what turns a count of zeroes into a count of ones. The trait has both pairs
      // because the complement is the caller's to get wrong, not because the machine has four
      // instructions.
      def complement = {
        val r = freshReg(); emit(Inst.Bin(r, BinOp.Xor, ll, v, Val.Int(-1))); r
      }

      op match
        // `x < 0 ? -x : x`, and the negation is the wrapping one the language's own `-` is — so at
        // the most negative value both answer that value, rather than the member disagreeing with
        // the operator beside it.
        case "abs" =>
          val neg = freshReg(); emit(Inst.Bin(neg, BinOp.Sub, ll, Val.Int(0), v))
          val lt  = freshReg(); emit(Inst.IntCmp(lt, ICmp.Slt, ll, v, Val.Int(0)))
          val r   = freshReg()
          emit(Inst.Select(r, lt, ll, neg, v))
          r
        // Two comparisons rather than a subtraction of them, because `(x > 0) - (x < 0)` would have
        // to widen both booleans to the operand's width first and says less about what it computes.
        case "signum" =>
          val pos = freshReg(); emit(Inst.IntCmp(pos, ICmp.Sgt, ll, v, Val.Int(0)))
          val neg = freshReg(); emit(Inst.IntCmp(neg, ICmp.Slt, ll, v, Val.Int(0)))
          val hi  = freshReg()
          emit(Inst.Select(hi, pos, ll, Val.Int(1), Val.Int(0)))
          val r = freshReg()
          emit(Inst.Select(r, neg, ll, Val.Int(-1), hi))
          r

        case "count_ones"  => counted("ctpop", v)
        case "count_zeros" => counted("ctpop", complement)

        // The `i1` the two counting intrinsics take says whether a zero operand is **poison**.
        // `false` is what makes zero answer the width instead, which is the answer the trait
        // documents and the only one that is the same number on every target.
        case "leading_zeros"  => counted("ctlz", v, zeroFlag = true)
        case "trailing_zeros" => counted("cttz", v, zeroFlag = true)
        case "leading_ones"   => counted("ctlz", complement, zeroFlag = true)
        case "trailing_ones"  => counted("cttz", complement, zeroFlag = true)

        case "reverse_bits" => intrinsic("bitreverse", ll, List(v))

        // A rotation is a funnel shift of the value with itself, which is how LLVM spells one: the
        // two halves are the same register, so the bits leaving the top arrive at the bottom.
        case "rotate_left"  => intrinsic("fshl", ll, List(v, v, rotateBy(n.get, amount.get.ty, bits)))
        case "rotate_right" => intrinsic("fshr", ll, List(v, v, rotateBy(n.get, amount.get.ty, bits)))

        case _ => sys.error(s"unreachable integer operation '$op'")

    // Short-circuit: `&&` evaluates its right side only when the left is true, `||` only when the
    // left is false — so a guard like `p != null && *p > 0` never runs the unsafe right side. The
    // result defaults to the left value and is overwritten by the right only when it is reached.
    case TLogical(op, l, r) =>
      val lv   = genExpr(l)
      val slot = emitAlloca(freshReg(), i1)
      emit(Inst.Store(i1, lv, slot, Access.Plain))
      val rhsL = freshLabel("sc.rhs")
      val endL = freshLabel("sc.end")
      if op == "&&" then emitTerm(Inst.CondBr(lv, rhsL, endL))
      else emitTerm(Inst.CondBr(lv, endL, rhsL))
      emitLabel(rhsL)
      // The right side gets its own temp region: anything it allocates is released before the
      // merge, and if the branch is skipped that code never runs at all.
      //
      // **And its own ownership region, for the second half of that sentence.** A slot registered
      // here is written only where the branch is taken, while the region it is registered with is
      // released on every path out of it — so a `&value` in a short-circuited operand would have
      // its release emitted where its store never happened. Opening a region of the branch's own
      // is what keeps the two on one path; where nothing is owned, which is nearly always, the pop
      // emits nothing at all.
      pushTemps()
      pushOwned()
      val rv = genExpr(r)
      emit(Inst.Store(i1, rv, slot, Access.Plain))
      popOwned()
      popTemps()
      emitTerm(Inst.Br(endL))
      emitLabel(endL)
      val res = freshReg(); emit(Inst.Load(res, i1, slot, Access.Plain)); res

    // One comparison has nothing to short-circuit, so it stays straight-line — which is what the
    // overwhelming majority of comparisons are.
    case TCompare(List(l, r), List(c)) =>
      val lv = genExpr(l)
      comparison(c, l.ty, lv, genExpr(r))

    // A **chain** short-circuits: `a < b < c` stops at the first comparison that fails, so a
    // side-effecting later operand does not run. That is what `01` specifies, and what writing the
    // chain out as `&&` already did.
    //
    // Each operand is still evaluated **exactly once**. Operand `k+1` is compared against operand
    // `k`, so the shared middle value has to be the same value both times — which is why this is
    // not a rewrite into `&&` over separate comparisons, and why the operands cannot simply be
    // evaluated where they are used.
    //
    // That sharing is also what makes the ownership bookkeeping interesting: an operand evaluated
    // in one block is used again in the next, so its temporaries outlive the block that made them
    // and cannot be released there. Each block therefore opens its own region, and the exits
    // **unwind them in reverse** — a path that leaves the chain early passes through exactly the
    // pops for the regions it entered, and no others. Where nothing is owned, which is the usual
    // case, every pop is empty and the ladder is branches alone.
    case TCompare(operands, cmps) =>
      val slot  = emitAlloca(freshReg(), i1)
      val exits = cmps.indices.map(_ => freshLabel("cmp.exit")).toList
      val endL  = freshLabel("cmp.end")

      var left: Val = Val.Nothing

      for k <- cmps.indices do
        pushTemps()
        // An ownership region per block for the same reason and unwound on the same ladder: a
        // `&value` in an operand the chain short-circuits past must not have its release emitted
        // where its store never ran.
        pushOwned()
        if k == 0 then left = genExpr(operands.head)
        val right = genExpr(operands(k + 1))
        val c     = comparison(cmps(k), operands(k).ty, left, right)

        emit(Inst.Store(i1, c, slot, Access.Plain))

        if k == cmps.length - 1 then emitTerm(Inst.Br(exits(k)))
        else
          val nextL = freshLabel("cmp.next")
          emitTerm(Inst.CondBr(c, nextL, exits(k)))
          emitLabel(nextL)

        left = right

      for k <- cmps.indices.reverse do
        emitLabel(exits(k))
        popOwned()
        popTemps()
        emitTerm(Inst.Br(if k == 0 then endL else exits(k - 1)))

      emitLabel(endL)
      val res = freshReg(); emit(Inst.Load(res, i1, slot, Access.Plain)); res

    case TSeq(exprs) =>
      exprs.foreach(genExpr); Val.Nothing

    // One copy of an unrolled `for const` (`reference/generics.md § A parameter may stand for a
    // list of types`), which is a block wherever it stands. A copy that yields nothing is emitted
    // for its effects, which is what every copy of a loop body does.
    case TBlockExpr(b) =>
      if Type.zeroSized(b.ty) then { genBlockVoid(b); Val.Nothing }
      else genBlockValue(b)

    // A function's address is the symbol its **C-callable** entry is defined under, which is a
    // constant — there is nothing to compute and nothing to load, the way there is for the address
    // of a variable. For an exported function that entry is the thunk rather than the definition
    // (`CallEmitter.entryOf`), since a `*extern` may only hold something C can call.
    case TFuncAddr(_, entry, _) => Val.Global(entryOf(entry))

    // A call through one goes out under C's convention, because that is what the type said was at
    // the other end. It reuses the foreign path entire: what a `call` names in front of an indirect
    // callee is the result type and then the value, exactly where a direct one names the symbol.
    case TCallPtr(callee, args, _, ty) =>
      genForeignCall(foreignResult(ty), genExpr(callee), args, ty)

    // The last thing a recursive function does, where it is a call to itself: a jump to its own
    // entry over the frame it already has (`TailCalls`).
    case c: TCall if isTailCall(c) => genTailSelfCall(c.args)

    // A call to a foreign function is lowered under the other side's convention rather than sysl's
    // own, which is a difference only an aggregate can see (`ForeignEmitter`).
    case TCall(name, args, ty, _) if foreigns.contains(name) =>
      val (what, callee) = calleeParts(name, ty)

      genForeignCall(what, callee, args, ty)

    // A call to something declared `-> never` does not come back, so the block ends at it: what
    // follows in the same block is unreachable and `emit` drops it, which is exactly why a
    // diverging arm needs no special handling anywhere else.
    case TCall(name, args, ty, _) =>
      val staged = args.map(argValue(_, inert(name)))

      // `reference/verification.md § variant on a function`: a call the compiler can see is a call
      // to the same body checks that the measure has gone down. It sits before the call rather than
      // inside the callee, which is what lets it be made out of values already in hand.
      if checksVariant(name) then genVariantAtCall(staged)
      val (what, callee) = calleeParts(name, ty)
      val r              = genSyslCall(what, callee, formatArgs(staged), ty, None)

      // What the callee promised, said again where the caller's own code can be folded against it
      // (`ContractEmitter.assumeEnsures`). It goes after the call because a postcondition is about
      // the state the call left behind.
      assumeEnsures(name, args, Option.when(r != Val.Nothing)(r))
      r

    // Erasing costs one word: the value goes on pointing where it pointed, and the table for the
    // type it is losing is a constant beside it. Nothing is retained — a counted object holds the
    // count its operand already had, which is what makes `f(&T)` and `f(&Trait)` the same handover.
    case TErase(operand, vtable, _) =>
      val d = genExpr(operand)
      val a = freshReg()

      emit(Inst.Insert(a, LType.fat, Val.Undef, LType.Ptr, Val.Global(vtable), List(0)))
      val b = freshReg(); emit(Inst.Insert(b, LType.fat, a, LType.Ptr, d, List(1)))
      b

    // A call whose callee is a word in the object's table rather than a name. The data word goes in
    // front of the declared arguments, which is the shape every slot was built to.
    case TVCall(receiver, slot, args, ty, _) =>
      val obj     = genExpr(receiver)
      val table   = freshReg(); emit(Inst.Extract(table, LType.fat, obj, List(0)))
      val data    = freshReg(); emit(Inst.Extract(data, LType.fat, obj, List(1)))
      val argVals = argList(args)
      val entry   = freshReg(); emit(Inst.Gep(entry, vtableLty, table,
                      List(Arg(LType.I(32), Val.Int(0)), Arg(LType.I(32), Val.Int(1)),
                           Arg(wordLty, Val.Int(slot)))))
      val fn      = freshReg(); emit(Inst.Load(fn, LType.Ptr, entry, Access.Plain))
      genSyslCall(syslResult(ty), fn, Arg(LType.Ptr, data) :: argVals, ty, None)

    // The tail walk is the ABI's, so all three are LLVM's own: two intrinsic calls and the one
    // instruction whose lowering every backend supplies for it.
    case TVaStart(ap) =>
      usesVarargs = true
      emit(Inst.Call(None, LType.Void, Val.Global(Llvm.vaStart.at(LType.Ptr)),
                     List(Arg(LType.Ptr, genExpr(ap)))))
      Val.Nothing

    case TVaEnd(ap) =>
      usesVarargs = true
      emit(Inst.Call(None, LType.Void, Val.Global(Llvm.vaEnd.at(LType.Ptr)),
                     List(Arg(LType.Ptr, genExpr(ap)))))
      Val.Nothing

    case TVaArg(ap, ty) =>
      val r = freshReg()
      emit(Inst.VaArg(r, genExpr(ap), ty.lty))
      r

    // The one place the C ABI is not the same on every machine, so the one place codegen reads the
    // target (`getting-started/cli.md § targets`). All three answers are a `ptr`, and all three
    // start from the address of the walk — what differs is whether the callee is handed that
    // address, the value in it, or the address of a copy of it.
    case TVaPass(ap) =>
      val addr = genExpr(ap)

      target.vaList match
        case VaListAbi.Address => addr

        case VaListAbi.Loaded =>
          val r = freshReg(); emit(Inst.Load(r, LType.Ptr, addr, Access.Plain)); r

        case VaListAbi.Copied =>
          val copy = emitAlloca(freshReg(), Type.VaList.lty)

          emitMemcpy(copy, addr, target.vaListBytes, 8)
          copy

    case TVaCopy(dst, src) =>
      usesVaCopy = true
      // Both addresses are produced before either is used, so a copy onto a list read out of the
      // same expression cannot see a half-written destination.
      val d = genExpr(dst)
      val s = genExpr(src)
      emit(Inst.Call(None, LType.Void, Val.Global(Llvm.vaCopy.at(LType.Ptr)),
                     List(Arg(LType.Ptr, d), Arg(LType.Ptr, s))))
      Val.Nothing

    case e @ TStructNew(struct, _) if layout.indirect(struct) => throughSlot(e)

    case e @ TEnumNew(en, _, _) if !en.simple && layout.indirect(en) => throughSlot(e)

    // A bitfield struct is one integer, so it is built by or-ing every field into place rather than
    // inserting each into a slot of its own — and the container then goes into the single slot the
    // emitted aggregate has (`Bitfields`). The arguments are still evaluated in written order, a
    // zero-sized one included: it contributes no bits and is not a reason to skip whatever computing
    // it does.
    case TStructNew(struct, args) if Bitfields.of(struct).isDefined =>
      val ranges = Bitfields.of(struct).get
      val vals   = args.zipWithIndex.map((a, i) => (genExpr(a), struct.fields(i)._2))
      val c      = buildBits(ranges, vals.collect { case (v, ft) if !Type.zeroSized(ft) => v })
      val r      = freshReg()

      emit(Inst.Insert(r, struct.lty, Val.Undef, containerLty(ranges), c, List(0)))
      r

    case TStructNew(struct, args) =>
      val vals = args.map(genExpr)
      var acc: Val = Val.Undef
      for (v, i) <- vals.zipWithIndex if !Type.zeroSized(struct.fields(i)._2) do
        val r = freshReg()
        emit(Inst.Insert(r, struct.lty, acc, struct.fields(i)._2.lty, v,
          List(struct.slot(i))))
        acc = r
      acc

    case TEnumNew(en, variant, args) =>
      enumValue(en, variant, args.map(genExpr))

    case TEnumFromInt(value, en) =>
      genEnumFromInt(value, en)

    case TEnumTry(value, en, optTy, some, none) =>
      genEnumTry(value, en, optTy, some, none)

    case TDowngrade(value, weakTy) =>
      genDowngrade(value, weakTy)

    case TUpgrade(value, optTy, some, none) =>
      genUpgrade(value, optTy, some, none)

    case TEnumAttr(kind, en, arg, _) =>
      genEnumAttr(kind, en, arg)

    case TTry(operand, ok, fail, retEnum, retFail, _, convert) =>
      genTry(operand, ok, fail, retEnum, retFail, convert)

    case TField(receiver, _, ty) if Type.zeroSized(ty) =>
      genExpr(receiver); Val.Nothing

    // A bitfield is read out of its container, and the container is reached the way the *receiver*
    // is: at its address where it has one, so that a field of a `volatile` register block is one
    // volatile load of the whole register — and out of the value otherwise. Which of those it is
    // has to be settled here rather than by the two cases below, because both of those take the
    // address of the **field**, and a bitfield has none (`reference/types.md § Structs`).
    case TField(receiver, index, _) if bitfieldOf(receiver.ty).isDefined =>
      val ranges = bitfieldOf(receiver.ty).get
      val ct     = containerLty(ranges)

      // The container is read at the receiver's address where the receiver is large enough to be
      // handed about through memory, and lifted out of its value otherwise — the same choice the two
      // cases below make, made here because both of those take the address of the **field** and a
      // bitfield has none.
      //
      // **A container holding any `volatile` field is read at its address whatever its size**, so
      // that reading a bitfield register is one volatile load of the register and not a load of the
      // struct followed by an `extractvalue` (`reference/types.md § Structs`). `volatile` is a
      // property of the container rather than of one range of it — every field of a bitfield struct
      // is bits of the same word — which is why the qualifier is asked of the receiver's storage
      // and not of the field.
      val acc = accessOf(receiver.placeTy)
      val c =
        if hasAddress(receiver) && (layout.indirect(receiver.ty) || acc != Access.Plain) then
          val p = address(receiver)
          val t = freshReg(); emit(Inst.Load(t, ct, p, acc)); t
        else
          val rv = genExpr(receiver)
          val t  = freshReg(); emit(Inst.Extract(t, receiver.ty.lty, rv, List(0))); t

      readBits(ranges, bitRange(receiver.ty, index).get, c)

    // A register is reached at its own address, because the ordinary lowering below would read the
    // whole block to get at one field of it — and reading a register block is not a way of reading
    // one register (`reference/memory.md § Device memory`).
    case e @ TField(receiver, _, ty) if Type.volatileIn(e.placeTy) && hasAddress(receiver) =>
      val p = address(e)
      val r = freshReg(); emit(Inst.Load(r, ty.lty, p, Access.Volatile)); r

    // So is a field of a **large** struct, for a reason that is arithmetic rather than hardware:
    // lifting one field out of a value means producing the whole value first, and for a receiver of
    // kilobytes that is a first-class aggregate emitted to read four bytes out of it.
    case e @ TField(receiver, _, ty) if layout.indirect(receiver.ty) && hasAddress(receiver) =>
      val p = address(e)
      val r = freshReg(); emit(Inst.Load(r, ty.lty, p, Access.Plain)); r

    case TField(receiver, index, ty) =>
      val rv = genExpr(receiver); val r = freshReg()
      emit(Inst.Extract(r, receiver.ty.lty, rv, List(fieldSlot(receiver.ty, index)))); r

    case TIf(cond, thenBlock, elseBlock, ty) =>
      genIf(cond, thenBlock, elseBlock, ty)

    case TMatch(scrutinee, arms, ty) =>
      genMatch(scrutinee, arms, ty)

    case w: TWhile   => genWhile(w)
    case d: TDoWhile => genDoWhile(d)
    case l: TLoop    => genLoop(l)
    case f: TCFor    => genCFor(f)
    case f: TFor     => genFor(f)
    case e: TForEach => genForEach(e)
    case i: TIterate => genIterate(i)
    case q: TQuantifier => genQuantifier(q)

    case TCheckedLoop(slot, varTy, loop) => genCheckedLoop(slot, varTy, loop)
}
