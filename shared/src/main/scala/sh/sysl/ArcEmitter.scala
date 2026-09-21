package sh.sysl

import ir.{Access, Arg, BinOp, CastOp, ICmp, Inst, LType, Val}

import scala.collection.mutable

/** Automatic reference counting: where a count is taken, where it is given back, and what the
 * heap object looks like while it is held.
 *
 * The placement is uniform rather than clever, because that is what makes it checkable — every
 * named slot holds one count, every temporary holds one until its region ends, and every
 * function retains its parameters and returns its result with a count already taken. Nothing
 * here elides a redundant pair; that is a later pass over the same placement.
 */
trait ArcEmitter extends Emitter {

  /** The typed program being lowered. Declared here as well as in `CallEmitter` because the release
   * hook has to ask it which payload types carry a destructor (`reference/memory.md § A
   * destructor`), and a hook is built from a type with no call site anywhere near it.
   */
  protected val program: TProgram

  /** Whether copying a value of this type has to touch a refcount. A raw pointer never does —
   * it is the mode that opts out of management — and a `&T` is a leaf, so a recursive type
   * cannot make this recur forever.
   */
  protected def containsRef(t: Type): Boolean = Type.containsCounted(t)

  /** Where a box's own contents begin, once the three header words — the strong count, the
   * destruction hook, and the weak count (`reference/memory.md § What a heap object costs`) — are
   * behind them. A buffer's element count sits here and its elements one further on.
   */
  protected val headerFields = 3

  /** The LLVM name of the box that holds a `T` on the heap: the two counts, the deallocation
   * hook, and the payload.
   */
  protected def boxName(payload: Type): String = boxLty(payload).render

  /** The same as a type rather than as its name. */
  protected def boxLty(payload: Type): LType.Named = {
    heap = true
    val n = s"%arc.${Type.mangle(payload)}"
    boxes.getOrElseUpdate(n, payload)
    LType.Named(n)
  }

  /** The box a counted trait object holds, which is its second word — the first is the method
   * table, a constant that owns nothing.
   */
  protected def erasedBox(v: Val): Val = {
    val b = freshReg(); emit(Inst.Extract(b, LType.fat, v, List(1))); b
  }

  /** The address a weak reference counts against: the box itself, or — behind a trait object —
   * the second of its two words.
   */
  private def weakBox(ty: Type.Weak, v: Val): Val =
    if ty.inner.isInstanceOf[Type.Trait] then erasedBox(v) else v

  /** Takes a share of everything a value refers to. A bare reference is one refcount; an
   * aggregate delegates to a per-type helper that walks its reference-carrying fields.
   */
  protected def retainValue(ty: Type, v: Val): Unit = ty match
    // A `&Trait` counts exactly as the `&T` it was made from does: the box carries its own
    // destructor, so letting go of one needs no more knowledge of the payload than this has.
    case Type.Ref(_: Type.Trait, sync) =>
      heap = true
      syncHeap ||= sync
      emit(Inst.Call(None, LType.Void, Val.Global(s"arc.retain${if sync then "_sync" else ""}"),
        List(Arg(LType.Ptr, erasedBox(v)))))

    case Type.Ref(_, sync) =>
      heap = true
      syncHeap ||= sync
      emit(Inst.Call(None, LType.Void, Val.Global(s"arc.retain${if sync then "_sync" else ""}"),
        List(Arg(LType.Ptr, v))))
    case w: Type.Weak =>
      weakHeap = true
      emit(Inst.Call(None, LType.Void, Val.Global("arc.weak_retain"),
        List(Arg(LType.Ptr, weakBox(w, v)))))

    case w: Type.View =>
      emit(Inst.Call(None, LType.Void, Val.Global("arc.retain_maybe"),
        List(Arg(LType.Ptr, owner(w, v)))))

    case t if containsRef(t) =>
      emit(Inst.Call(None, LType.Void, Val.Global(valueHelper(t, retain = true)),
        List(Arg(t.lty, v))))

    case _                   => ()

  /** Gives back a share of everything a value refers to. */
  protected def releaseValue(ty: Type, v: Val): Unit = ty match
    case Type.Ref(_: Type.Trait, sync) =>
      heap = true
      syncHeap ||= sync
      emit(Inst.Call(None, LType.Void, Val.Global(s"arc.release${if sync then "_sync" else ""}"),
        List(Arg(LType.Ptr, erasedBox(v)))))

    case Type.Ref(_, sync) =>
      heap = true
      syncHeap ||= sync
      emit(Inst.Call(None, LType.Void, Val.Global(s"arc.release${if sync then "_sync" else ""}"),
        List(Arg(LType.Ptr, v))))
    case w: Type.Weak =>
      weakHeap = true
      emit(Inst.Call(None, LType.Void, Val.Global("arc.weak_release"),
        List(Arg(LType.Ptr, weakBox(w, v)))))

    case w: Type.View =>
      emit(Inst.Call(None, LType.Void, Val.Global("arc.release_maybe"),
        List(Arg(LType.Ptr, owner(w, v)))))

    case t if containsRef(t) =>
      emit(Inst.Call(None, LType.Void, Val.Global(valueHelper(t, retain = false)),
        List(Arg(t.lty, v))))

    case _                   => ()

  /** Takes a share of everything the value **at an address** refers to, and gives one back.
   *
   * The pair exists for a type `layout.indirect` calls large, which never becomes a first-class
   * value: there is nothing to hand `retainValue`, and loading one so that there were would undo
   * the whole point of lowering it through memory. Everything smaller is read out and walked as
   * before, so the two forms agree on what they count — only on where they read it from.
   */
  protected def retainAt(ty: Type, p: Val): Unit = walkAt(ty, p, retain = true)

  protected def releaseAt(ty: Type, p: Val): Unit = walkAt(ty, p, retain = false)

  private def walkAt(ty: Type, p: Val, retain: Boolean): Unit =
    if containsRef(ty) then
      if layout.indirect(ty) then
        emit(Inst.Call(None, LType.Void, Val.Global(slotHelper(ty, retain)), List(Arg(LType.Ptr, p))))
      else
        val v = freshReg(); emit(Inst.Load(v, ty.lty, p, Access.Plain))
        if retain then retainValue(ty, v) else releaseValue(ty, v)

  /** The retain / release helper a large aggregate takes at its address. Same walk as the one over
   * a value, reaching each reference-carrying member with `getelementptr` instead of lifting it out
   * of an aggregate the caller would have had to materialize first.
   */
  private def slotHelper(ty: Type, retain: Boolean): String = {
    val name = s"arc.${if retain then "copy" else "dispose"}_at.${Type.mangle(ty)}"

    requestFunction(name)(helperSig(name, "p" -> LType.Ptr)) {
      walkSlot(ty, Val.Reg("p"), retain)
      emitTerm(Inst.Ret(None, None))
    }
  }

  private def walkSlot(ty: Type, p: Val, retain: Boolean): Unit = {
    def each(fields: List[(String, Type)], aggregate: LType, base: Val): Unit =
      for ((_, fty), i) <- fields.zipWithIndex if containsRef(fty) do
        val f = freshReg()

        emit(Inst.Gep(f, aggregate, base,
          List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(Type.slot(fields, i))))))
        walkAt(fty, f, retain)

    ty match
      case s: Type.Struct => each(s.fields, s.lty, p)

      case Type.Array(n, elem) =>
        val i = emitAlloca(freshReg(), wordLty)
        emit(Inst.Store(wordLty, Val.Int(0), i, Access.Plain))
        val condL = freshLabel("arc.each")
        val bodyL = freshLabel("arc.elem")
        val endL  = freshLabel("arc.done")
        emitTerm(Inst.Br(condL))
        emitLabel(condL)
        val iv   = freshReg(); emit(Inst.Load(iv, wordLty, i, Access.Plain))
        val more = freshReg(); emit(Inst.IntCmp(more, ICmp.Ult, wordLty, iv, Val.Int(n)))
        emitTerm(Inst.CondBr(more, bodyL, endL))
        emitLabel(bodyL)
        val ep = freshReg(); emit(Inst.Gep(ep, elem.lty, p, List(Arg(wordLty, iv))))
        walkAt(elem, ep, retain)
        val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, wordLty, iv, Val.Int(1)))
        emit(Inst.Store(wordLty, nxt, i, Access.Plain))
        emitTerm(Inst.Br(condL))
        emitLabel(endL)

      case e: Type.Enum =>
        val tag  = freshReg(); emit(Inst.Load(tag, i32, p, Access.Plain))
        val endL = freshLabel("arc.done")
        for variant <- e.variants if variant.fields.exists(f => containsRef(f._2)) do
          val hitL  = freshLabel("arc.variant")
          val nextL = freshLabel("arc.next")
          val is    = freshReg(); emit(Inst.IntCmp(is, ICmp.Eq, i32, tag, Val.Int(variant.tag)))
          emitTerm(Inst.CondBr(is, hitL, nextL))
          emitLabel(hitL)
          each(variant.fields, e.payloadLty(variant), payloadPtr(e, p))
          emitTerm(Inst.Br(endL))
          emitLabel(nextL)
        emitTerm(Inst.Br(endL))
        emitLabel(endL)

      case _ => ()
  }

  /** The owner word of a view — the reference that keeps its elements alive, or null when they
   * are static (every string literal), on a frame, or reached through a `*T`.
   */
  protected def owner(ty: Type.View, v: Val): Val = {
    heap = true
    maybeHeap = true
    val o = freshReg(); emit(Inst.Extract(o, ty.lty, v, List(0))); o
  }

  /** The function that destroys a box of this payload type: it lets go of whatever the payload
   * holds and then returns the storage. Installed in the box at construction, which is what
   * makes releasing a reference type-erased — a slice's owner has no static type to consult.
   */
  protected def dropFn(payload: Type): Val.Global = {
    // A type with a destructor needs a hook of its own even when nothing in it is counted
    // (`reference/memory.md § A destructor`): the walk has nothing to do and the `drop` still has
    // to be called, so the plain hook — which is shared by every payload that holds nothing —
    // cannot serve.
    val destructor = program.destructors.get(Type.mangle(payload))

    if !containsRef(payload) && destructor.isEmpty then plainDropFn
    else
      val m  = Type.mangle(payload)
      val bn = boxLty(payload)

      Val.Global(requestFunction(s"arc.drop.$m")(
        helperSig(s"arc.drop.$m", "p" -> LType.Ptr, "storage" -> i1)) {
          val give = freshLabel("arc.give")
          val over = freshLabel("arc.over")

          emitTerm(Inst.CondBr(Val.Reg("storage"), give, over))
          emitLabel(over)
          val pa = freshReg(); emit(Inst.Gep(pa, bn, Val.Reg("p"), List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))
          val v  = freshReg(); emit(Inst.Load(v, payload.lty, pa, Access.Plain))

          // **Before the walk, not after.** The destructor is handed the value as it stands, so it
          // may read a field to close what that field names; releasing first would hand it a value
          // whose references had already been let go of. It takes `self` **borrowed** — no count is
          // taken for the call — because the count is already zero and taking one would resurrect
          // the object into a second teardown.
          for d <- destructor do
            if layout.indirect(payload) then emit(Inst.Call(None, LType.Void, Val.Global(d), List(Arg(LType.Ptr, pa))))
            else emit(Inst.Call(None, LType.Void, Val.Global(d), List(Arg(payload.lty, v))))

          releaseValue(payload, v)
          emitTerm(Inst.Ret(None, None))
          emitLabel(give)
          emitFree()
          emitTerm(Inst.Ret(None, None))
      })
  }

  /** The hook for a box whose payload holds nothing: there is no walk to make, so the only phase
   * that does anything is the one that gives the storage back.
   *
   * One per module rather than one per payload type, because nothing in it depends on the payload —
   * which keeps the commonest box of all, the one holding a plain number, from costing a function.
   */
  protected def plainDropFn: Val.Global =
    Val.Global(requestFunction("arc.drop.plain")(
      helperSig("arc.drop.plain", "p" -> LType.Ptr, "storage" -> i1)) {
        val give = freshLabel("arc.give")
        val over = freshLabel("arc.over")

        emitTerm(Inst.CondBr(Val.Reg("storage"), give, over))
        emitLabel(over)
        emitTerm(Inst.Ret(None, None))
        emitLabel(give)
        emitFree()
        emitTerm(Inst.Ret(None, None))
    })

  /** The one place `free` is **called**. It sits inside a hook, so it reaches a module only where
   * that module builds a box — which is a module that has already called `malloc`, and therefore
   * has an allocator to give the bytes back to.
   *
   * The `declare` beside `malloc`'s stays where it was, under `heap`, and costs nothing: a
   * declaration nothing calls names no symbol in the object file. What the linker was complaining
   * about was the call, which is why the card counted calls rather than declarations.
   */
  private def emitFree(): Unit = emit(Inst.Call(None, LType.Void, Val.Global(freeSym), List(Arg(LType.Ptr, Val.Reg("p")))))

  /** The retain / release helper for an aggregate type, which walks the fields that carry
   * references. Emitted once per type rather than inlined, since a data enum needs a tag test
   * per reference-carrying variant.
   */
  private def valueHelper(ty: Type, retain: Boolean): String = {
    val name = s"arc.${if retain then "copy" else "dispose"}.${Type.mangle(ty)}"

    requestFunction(name)(helperSig(name, "v" -> ty.lty)) {
      walkValue(ty, Val.Reg("v"), retain)
      emitTerm(Inst.Ret(None, None))
    }
  }

  private def walkValue(ty: Type, v: Val, retain: Boolean): Unit = {
    def each(fields: List[(String, Type)], aggregate: LType, value: Val): Unit =
      // A zero-sized field holds no reference, so it is skipped by the filter already — but the
      // index has to be the slot it landed in rather than the one it was written at, or a field
      // after one would be walked at the wrong offset.
      for ((_, fty), i) <- fields.zipWithIndex if containsRef(fty) do
        val f = freshReg()
        emit(Inst.Extract(f, aggregate, value, List(Type.slot(fields, i))))
        if retain then retainValue(fty, f) else releaseValue(fty, f)

    ty match
      case s: Type.Struct => each(s.fields, s.lty, v)

      // An array is walked with a loop rather than an unrolled chain: the element count is a
      // compile-time constant, but it can be very large, and the code for one element is not.
      case Type.Array(n, elem) =>
        val buf = emitAlloca(freshReg(), ty.lty)
        emit(Inst.Store(ty.lty, v, buf, Access.Plain))
        val i = emitAlloca(freshReg(), wordLty)
        emit(Inst.Store(wordLty, Val.Int(0), i, Access.Plain))
        val condL = freshLabel("arc.each")
        val bodyL = freshLabel("arc.elem")
        val endL  = freshLabel("arc.done")
        emitTerm(Inst.Br(condL))
        emitLabel(condL)
        val iv   = freshReg(); emit(Inst.Load(iv, wordLty, i, Access.Plain))
        val more = freshReg(); emit(Inst.IntCmp(more, ICmp.Ult, wordLty, iv, Val.Int(n)))
        emitTerm(Inst.CondBr(more, bodyL, endL))
        emitLabel(bodyL)
        val ep = freshReg(); emit(Inst.Gep(ep, elem.lty, buf, List(Arg(wordLty, iv))))
        val ev = freshReg(); emit(Inst.Load(ev, elem.lty, ep, Access.Plain))
        if retain then retainValue(elem, ev) else releaseValue(elem, ev)
        val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, wordLty, iv, Val.Int(1)))
        emit(Inst.Store(wordLty, nxt, i, Access.Plain))
        emitTerm(Inst.Br(condL))
        emitLabel(endL)

      case e: Type.Enum =>
        val tag  = freshReg(); emit(Inst.Extract(tag, e.lty, v, List(0)))
        val endL = freshLabel("arc.done")
        for variant <- e.variants if variant.fields.exists(f => containsRef(f._2)) do
          val hitL  = freshLabel("arc.variant")
          val nextL = freshLabel("arc.next")
          val is    = freshReg(); emit(Inst.IntCmp(is, ICmp.Eq, i32, tag, Val.Int(variant.tag)))
          emitTerm(Inst.CondBr(is, hitL, nextL))
          emitLabel(hitL)
          val payload = enumPayload(e, variant, v)
          each(variant.fields, e.payloadLty(variant), payload)
          emitTerm(Inst.Br(endL))
          emitLabel(nextL)
        emitTerm(Inst.Br(endL))
        emitLabel(endL)

      case _ => ()
  }

  // --- allocation --------------------------------------------------------------------------

  /** Puts a value on the heap: one count for the reference this yields, the hook that will
   * destroy it, and a copy of the payload — whose own references the box now holds a share of.
   */
  protected def genBox(value: TExpr, refTy: Type.Ref): Val = {
    val inner = refTy.inner
    val bn    = boxLty(inner)
    // A large payload is written into the box rather than produced and then stored into it, for the
    // reason every other destination has: the value would be a first-class aggregate of kilobytes
    // for the length of one instruction. The address is not known until the box exists, so this is
    // the one destination that cannot be handed over before the expression runs.
    val v     = if layout.indirect(inner) then Val.Nothing else genExpr(value)

    val end  = freshReg(); emit(Inst.Gep(end, bn, Val.Null, List(Arg(i32, Val.Int(1)))))
    val size = freshReg(); emit(Inst.Cast(size, CastOp.PtrToInt, LType.Ptr, end, wordLty))
    val p    = freshReg(); emit(Inst.Call(Some(p), LType.Ptr, Val.Global(mallocSym), List(Arg(wordLty, size))))

    emit(Inst.Store(wordLty, Val.Int(1), p, Access.Plain))
    val hook = freshReg(); emit(Inst.Gep(hook, bn, p, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(1)))))
    emit(Inst.Store(LType.Ptr, dropFn(inner), hook, Access.Plain))
    // One weak share stands for every strong reference together, so the storage outlives the
    // object exactly as long as some weak reference is still asking about it (`03`).
    val wc = freshReg(); emit(Inst.Gep(wc, bn, p, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(2)))))
    emit(Inst.Store(wordLty, Val.Int(1), wc, Access.Plain))

    val slot = freshReg(); emit(Inst.Gep(slot, bn, p, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))

    if layout.indirect(inner) then genOwnedInto(slot, value)
    else
      retainValue(inner, v)
      emit(Inst.Store(inner.lty, v, slot, Access.Plain))

    ownTemp(p, refTy)
  }

  /** The box that holds elements the program sized while running: the refcount, the deallocation
   * hook, how many elements there are, and then the elements. The count has to be *in* the box
   * because the hook is what destroys them and the hook is reached with no static type in sight —
   * which is the same reason `07` gives for the hook existing at all.
   */
  protected def bufName(elem: Type): String = bufLty(elem).render

  /** The same as a type rather than as its name. */
  protected def bufLty(elem: Type): LType.Named = {
    heap = true
    val n = s"%arc.buf.${Type.mangle(elem)}"
    bufs.getOrElseUpdate(n, elem)
    LType.Named(n)
  }

  /** The function that destroys such a box: it lets go of each element it still holds and returns
   * the storage. Elements that hold nothing need no walk, so the plain hook is enough — it still
   * gives the bytes back, which is the phase every box needs whatever it holds.
   */
  protected def dropBufFn(elem: Type): Val.Global =
    if !containsRef(elem) then plainDropFn
    else
      val m  = Type.mangle(elem)
      val bn = bufLty(elem)

      Val.Global(requestFunction(s"arc.dropbuf.$m")(
        helperSig(s"arc.dropbuf.$m", "p" -> LType.Ptr, "storage" -> i1)) {
          val give = freshLabel("arc.give")
          val over = freshLabel("arc.over")

          emitTerm(Inst.CondBr(Val.Reg("storage"), give, over))
          emitLabel(over)
          val lenp = freshReg(); emit(Inst.Gep(lenp, bn, Val.Reg("p"), List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))
          val n    = freshReg(); emit(Inst.Load(n, wordLty, lenp, Access.Plain))
          val data = freshReg()

          emit(Inst.Gep(data, bn, Val.Reg("p"),
            List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields + 1)))))

          eachElement(elem, data, n) { ep =>
            val ev = freshReg(); emit(Inst.Load(ev, elem.lty, ep, Access.Plain))
            releaseValue(elem, ev)
          }
          emitTerm(Inst.Ret(None, None))
          emitLabel(give)
          emitFree()
          emitTerm(Inst.Ret(None, None))
      })

  /** Puts one value in every one of `n` slots, taking a share for each — the elements belong to the
   * buffer, and it is the hook above that eventually lets them go.
   */
  protected def fillElements(elem: Type, data: Val, n: Val, v: Val): Unit =
    eachElement(elem, data, n) { ep =>
      retainValue(elem, v)
      emit(Inst.Store(elem.lty, v, ep, Access.Plain))
    }

  /** Walks `n` elements starting at `data`, handing each element's address to `body`. A loop rather
   * than a straight line for the reason the ARC walk over an array gives: the count is exactly the
   * thing that may be large here.
   */
  protected def eachElement(elem: Type, data: Val, n: Val)(body: Val => Unit): Unit = {
    val i     = emitAlloca(freshReg(), wordLty)
    val condL = freshLabel("buf.test")
    val bodyL = freshLabel("buf.elem")
    val endL  = freshLabel("buf.done")

    emit(Inst.Store(wordLty, Val.Int(0), i, Access.Plain))
    emitTerm(Inst.Br(condL))
    emitLabel(condL)
    val iv   = freshReg(); emit(Inst.Load(iv, wordLty, i, Access.Plain))
    val more = freshReg(); emit(Inst.IntCmp(more, ICmp.Ult, wordLty, iv, n))
    emitTerm(Inst.CondBr(more, bodyL, endL))
    emitLabel(bodyL)
    val ep = freshReg(); emit(Inst.Gep(ep, elem.lty, data, List(Arg(wordLty, iv))))
    body(ep)
    val nxt = freshReg(); emit(Inst.Bin(nxt, BinOp.Add, wordLty, iv, Val.Int(1)))
    emit(Inst.Store(wordLty, nxt, i, Access.Plain))
    emitTerm(Inst.Br(condL))
    emitLabel(endL)
  }

  /** Where a pointer's pointee actually lives. A `*T` addresses its value directly; a `&T`
   * addresses the box, whose payload sits after the refcount and the deallocation hook.
   */
  protected def payloadAddr(operand: TExpr): Val = {
    val p = genExpr(operand)

    operand.ty match
      case Type.Ref(inner, _) =>
        val r = freshReg()

        emit(Inst.Gep(r, boxLty(inner), p,
          List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))
        r
      case _ => p
  }

  // --- ownership regions ---------------------------------------------------------------------

  protected def pushTemps(): Unit = tempStack = mutable.ListBuffer.empty[(Val, Type)] :: tempStack

  /** Records a value the expression owns outright — a fresh box, or a call result, which every
   * function returns with a count already taken.
   */
  protected def ownTemp(v: Val, ty: Type): Val = {
    if containsRef(ty) then tempStack.head += ((v, ty))
    v
  }

  /** Releases the innermost region's temporaries and pops it. */
  protected def popTemps(): Unit = {
    releaseTemps()
    tempStack = tempStack.tail
  }

  /** Emits the releases for the innermost region without popping it, and drops the region without
   * emitting them — the pair a caller needs when more than one edge leaves the region and each has
   * to let go for itself. An iterating loop is the case: what `next` gave back is released on the
   * way into the body and again on the way out of the loop, and those are different blocks.
   */
  protected def releaseTemps(): Unit = releaseValues(tempsHere)

  /** What the innermost temp region holds, for an edge that has to give those counts back somewhere
   * other than where the region is closed — a condition term that branches before it can release,
   * because the branch it guards retains out of the very value being held
   * (`reference/expressions.md § is — a pattern where a condition is wanted`).
   */
  protected def tempsHere: List[(Val, Type)] = tempStack.head.toList

  protected def releaseValues(vs: List[(Val, Type)]): Unit =
    for (v, ty) <- vs.reverse do releaseValue(ty, v)
  protected def dropTemps(): Unit    = tempStack = tempStack.tail

  /** Hands a statement to the scope being emitted. Nothing is emitted here — the `defer` itself
   * costs no instruction, and the statement is laid down at each edge that leaves the block.
   */
  protected def deferStmts(stmts: List[TStmt]): Unit = deferrals.head ++= stmts

  /** Runs one scope's deferred statements, last registered first.
   *
   * They go **before** that scope's releases, so every local the statement names is still alive
   * while it runs — including the one holding the resource it is closing, which is the whole point
   * of the form. Registration order is written order, so `reverse` is the LIFO `reference/memory.md
   * § Where defer sits` promises.
   */
  private def runDeferrals(scope: mutable.ListBuffer[TStmt]): Unit = scope.reverse.foreach(genStmt)

  protected def pushOwned(): Unit = {
    owned = mutable.ListBuffer.empty[(Val, Type)] :: owned
    deferrals = mutable.ListBuffer.empty[TStmt] :: deferrals
  }

  /** Records a slot that holds a count of its own, after it has been written. */
  protected def ownSlot(name: String, ty: Type): Unit = ownAt(Val.Reg(s"$name.addr"), ty)

  /** The same, for a slot with no name in the program — the storage a `&value` materializes. The
   * scope registered in is the innermost one *at the point the value is written*, which is what
   * makes the release land on a path the store dominates.
   */
  protected def ownAt(slot: Val, ty: Type): Unit =
    if containsRef(ty) then owned.head += ((slot, ty))

  /** Registers the buffer a promoted array lives in, so the scope that declared it gives back its
   * share on the way out. A buffer is what a `&T` points at, so it is registered as one and the
   * ordinary release path emits the `arc.release` — the only reason it needs a slot of its own is
   * that everything in `owned` is a place to load from rather than a value.
   *
   * The count is what makes promotion work: a view of the array retains this box when it is
   * returned or stored, so the release below takes the declaration's share and the storage outlives
   * the frame exactly as long as some view of it does.
   */
  protected def ownBox(name: String, box: Val, elem: Type): Unit = {
    emitAlloca(Val.Reg(s"$name.box"), LType.Ptr)
    emit(Inst.Store(LType.Ptr, box, Val.Reg(s"$name.box"), Access.Plain))
    owned.head += ((Val.Reg(s"$name.box"), Type.Ref(elem, false)))
  }

  /** Emits the releases for the innermost scope without popping it — the guard-failure path out
   * of a match arm, where the bindings have been made but the arm is not taken.
   */
  protected def releaseOwned(): Unit = releaseSlots(ownedHere)

  /** What the innermost scope holds, for a path that has to give those counts back somewhere other
   * than where the scope ends.
   *
   * A condition's `is` bindings are the case (`reference/expressions.md § is — a pattern where a
   * condition is wanted`): the scope is popped at the end of the branch the condition guards, but a
   * later term of the same condition may fail, and *that* edge leaves without ever reaching the
   * branch. Its releases are emitted after the success path has already popped, so the slots have
   * to be read out while the scope is still there.
   */
  protected def ownedHere: List[(Val, Type)] = owned.head.toList

  protected def releaseSlots(slots: List[(Val, Type)]): Unit =
    for (slot, ty) <- slots.reverse do
      val v = freshReg(); emit(Inst.Load(v, ty.lty, slot, Access.Plain))
      releaseValue(ty, v)

  protected def popOwned(): Unit = {
    runDeferrals(deferrals.head)
    releaseOwned()
    owned = owned.tail
    deferrals = deferrals.tail
  }

  /** Lets go of everything the function holds, for a `return` that leaves from the middle of
   * it. Nothing is popped: the block is terminated straight afterwards, so the scopes that
   * follow this one lexically emit their own releases into unreachable code and are dropped.
   */
  protected def releaseAll(): Unit = {
    for frame <- tempStack; (v, ty) <- frame.reverse do releaseValue(ty, v)

    // **Snapshot first, and it is load-bearing.** A deferred statement is emitted by `genStmt`, and
    // one containing an `if` or a `match` pushes and pops the very stacks this is walking — so the
    // scopes must be fixed before any of them runs. A lazy pairing here would read `owned` back
    // mid-walk and skip the releases of whatever it had moved past, which leaks rather than fails.
    val scopes = owned.zip(deferrals)

    // Outward, one scope at a time, each running what it deferred before giving up its counts — so
    // a statement deferred in an inner block still sees the outer block's locals, which are let go
    // only once the scope that owns them is itself being left.
    for (scope, ds) <- scopes do
      runDeferrals(ds)
      for (slot, ty) <- scope.reverse do
        val v = freshReg(); emit(Inst.Load(v, ty.lty, slot, Access.Plain))
        releaseValue(ty, v)
  }

  /** Lets go of everything accrued since a loop was entered, for a `break`/`continue` that leaves
   * from the middle of the body — the ownership analogue of `releaseAll`, but bounded to the
   * loop's regions (whose depths the `GenLoop` recorded) rather than the whole function. Nothing
   * is popped: the block terminates right after, so the lexically-following pops emit into
   * unreachable code and are dropped.
   */
  protected def releaseToDepth(ownedDepth: Int, tempDepth: Int): Unit = {
    for frame <- tempStack.take(tempStack.length - tempDepth); (v, ty) <- frame.reverse do releaseValue(ty, v)

    // Snapshotted before anything runs, for `releaseAll`'s reason. `zip` needs no matching `take`:
    // the two stacks are the same length, so pairing the bounded one against the whole of the other
    // stops where the bound does, and the pairs still line up because both are innermost-first.
    val scopes = owned.take(owned.length - ownedDepth).zip(deferrals)

    for (scope, ds) <- scopes do
      runDeferrals(ds)
      for (slot, ty) <- scope.reverse do
        val v = freshReg(); emit(Inst.Load(v, ty.lty, slot, Access.Plain))
        releaseValue(ty, v)
  }
}

object ArcEmitter {

  /** The one symbol a freestanding **port** may define to say what the running task's reaper
   * scratch is (`reference/memory.md § A destructor`).
   *
   * It is a C name rather than a sysl one because whoever knows the answer is a scheduler written in
   * C: under FreeRTOS it is `pvTaskGetThreadLocalStoragePointer`, under something else it is
   * whatever that runtime keeps per task. It answers with a pointer to storage of two words —
   * `struct { void *head; unsigned char draining; }` — which must live as long as the task and must
   * not be shared with another one.
   *
   * Emitted `weak` alongside a definition returning the module's own single slot, so a program with
   * no scheduler links with nothing supplied and behaves as it always has. Nothing checks that a
   * port's definition is honest; a slot two tasks share is the very defect this exists to let a port
   * avoid, and the compiler cannot see it.
   */
  val reaperSlot = "__sysl_arc_reaper"

  /** All of ARC that is the same for every type — which, because the destructor lives behind the
   * box's hook, is all of it but the hooks themselves.
   *
   * Taking a share needs no ordering: a count you already hold cannot reach zero underneath you,
   * so the atomic form is a relaxed increment. Giving one back publishes with release ordering
   * and acquires before destroying, so the thread that frees sees every other domain's writes.
   * The `_maybe` pair is for views — slices and strings — whose owner is null when the elements
   * are static, on a frame, or reached through a `*T`; the plain pair stays branch-free, since a
   * reference is non-null by construction.
   *
   * **Teardown is iterative, not recursive.** A destructor releases the references its payload
   * holds, so a chain of `&T` — a linked list, a degenerate tree — would otherwise recurse one C
   * stack frame per node and overflow the stack on a long one. Instead, when a count reaches zero
   * the object is pushed onto a worklist (reusing its now-dead count slot as the link) and the
   * *first* release to hit zero drains the list in a loop: each destructor it runs pushes more
   * work rather than recursing, so teardown depth is O(1) regardless of structure depth.
   *
   * **The worklist is per thread, because it is scratch space and not shared state.** Two threads
   * releasing the last reference to two unrelated `&sync` structures at the same moment both reach
   * `arc.reap`, and one list between them would have each overwrite the other's head — a queued
   * object dropped on the floor, or drained twice. Neither is a race the counts could prevent: the
   * counts are what got both threads here correctly. So the head and the flag travel together as one
   * **slot**, each thread drains its own, and that is what makes the flag mean what it says: it asks
   * whether *this* thread is already inside a drain further up its own stack.
   *
   * **Where there is no notion of a current thread, the slot is asked for rather than assumed.** A
   * target with thread-local storage reaches its own directly and this costs nothing. A freestanding
   * target has no thread pointer — asked for a `thread_local`, LLVM gives it the local-exec model,
   * whose offset is read from a register nothing on a bare machine has written — so it calls
   * `__sysl_arc_reaper`, defined `weak` here to return the one plain slot. A program with no
   * scheduler defines nothing and gets exactly the single-list behaviour it has always had; a port
   * that *does* schedule defines the symbol over this one and answers with storage belonging to the
   * running task.
   *
   * **That is delegation rather than a new dependency, because `&sync` on a bare target is already
   * the environment's.** Its counts are `atomicrmw`, which LLVM cannot lower without `ldrex`/`strex`
   * and turns into a call to `__atomic_fetch_add_4` the board must define — and on a two-core part
   * that definition needs a hardware spinlock, since masking interrupts covers one core. A runtime
   * that already asks the board how to add atomically may ask it what the current task is.
   *
   * The call is on the path taken when a strong count reaches zero, so it is once per object rather
   * than once per release, and a target with real thread-local storage does not make it at all.
   *
   * **The storage outlives the object when something weak still asks about it.** The strong count
   * reaching zero destroys the object — the payload's references are given back — but the bytes
   * come back only when the weak count follows, and the weak count holds one share on behalf of
   * every strong reference together (`reference/memory.md § What a heap object costs`). So a
   * `get()` on a dead object reads storage that is still there and finds a strong count of zero in
   * it.
   *
   * That share is also what makes the worklist safe. A queued object has its strong slot on loan
   * as the list's link, and a weak release arriving in that window cannot free it, because the
   * share the strong references left behind has not been given back yet — `arc.destroy` is what
   * gives it back, after the hook has run and the link is no longer needed. The slot is put back
   * to zero before the hook runs, so nothing ever reads a link where a count should be.
   */
  def core(target: Target): String = {
    val tls = if target.hasThreadLocalStorage then "thread_local " else ""

    // How `arc.reap` comes by the slot. With thread-local storage the answer is the symbol itself
    // and nothing is asked — a machine that knows what the current thread is has no reason to
    // delegate. Without it the slot is fetched, and the `weak` definition below is what makes a
    // program that supplies nothing behave exactly as it did before there was anything to supply.
    //
    // `weak` rather than a declaration, because the overwhelming case is the program that has no
    // scheduler: a declaration would leave every bare-metal link needing a definition of a symbol
    // its author has no use for. A port that schedules defines it and wins the link.
    val supplied = !target.hasThreadLocalStorage

    val supplier = if !supplied then "" else
      s"""
        |define weak ptr @$reaperSlot() {
        |entry:
        |  ret ptr @arc.reaper.self
        |}""".stripMargin

    val fetchSlot = if !supplied then "" else s"  %s = call ptr @$reaperSlot()\n"

    val slot = if supplied then Val.Reg("s") else "@arc.reaper.self"

    // The two counts are pointer-width. Nothing forces that — a reference count is not an address —
    // but the alternative is a fixed `i64`, which costs sixteen bytes of header on a machine whose
    // whole point is having very little memory. What *is* forced is that this agrees with the stores
    // the emitter makes into these fields, which is why both spell it the same way: a count written
    // as one width and read as another is not a type error anywhere, it is a leak or a double free.
    val word = target.word.llvm

    // The middle word is the object's **hook**, and it is asked to do two things at two different
    // moments: run over the contents when the strong count reaches zero, and give the storage back
    // when the weak count does. A second slot would have been symmetric and cost a pointer per
    // object on machines chosen for having very little memory, which is the argument the comment
    // above already makes about the counts — so the phase travels as an argument instead.
    //
    // The free is in the hook rather than in `arc.unshare` because `reference/modules.md §
    // Capabilities are a module property` says the free path goes through the object's own hook,
    // and because it is the only arrangement under which the two halves of that chapter are both
    // true: a module that allocates nothing emits no hook and so names no `free`, while one holding
    // a heap slice something else made frees it with the allocator that made it.
    //
    // **`arc.reap` is kept out of line and declared rare, and that is what makes a release cheap
    // wherever one is emitted.** Releasing is a decrement and a test; only the test succeeding
    // reaches the worklist and its drain loop. Left inlinable, that loop is copied into every
    // release site in the program — so a member holding a single release becomes too large for its
    // own caller to absorb, and a sequence's `push` is a call and a frame rather than a store. With
    // `noinline cold` the decrement and the branch stay where they were and everything after them
    // sits behind a call that is almost never made, which is the arrangement every counted-reference
    // runtime settles on.
    s"""%arc.header = type { $word, ptr, $word }
      |
      |%arc.reaper = type { ptr, i8 }
      |
      |@arc.reaper.self = internal ${tls}global %arc.reaper zeroinitializer
      |$supplier
      |
      |define private void @arc.retain(ptr %p) {
      |entry:
      |  %c = load $word, ptr %p
      |  %n = add $word %c, 1
      |  store $word %n, ptr %p
      |  ret void
      |}
      |
      |define private void @arc.destroy(ptr %p) {
      |entry:
      |  %h = getelementptr %arc.header, ptr %p, i32 0, i32 1
      |  %f = load ptr, ptr %h
      |  %none = icmp eq ptr %f, null
      |  br i1 %none, label %after, label %run
      |run:
      |  call void %f(ptr %p, i1 false)
      |  br label %after
      |after:
      |  call void @arc.unshare(ptr %p)
      |  ret void
      |}
      |
      |define private void @arc.unshare(ptr %p) {
      |entry:
      |  %w = getelementptr %arc.header, ptr %p, i32 0, i32 2
      |  %c = load $word, ptr %w
      |  %n = sub $word %c, 1
      |  store $word %n, ptr %w
      |  %z = icmp eq $word %n, 0
      |  br i1 %z, label %gone, label %kept
      |gone:
      |  %h = getelementptr %arc.header, ptr %p, i32 0, i32 1
      |  %f = load ptr, ptr %h
      |  %none = icmp eq ptr %f, null
      |  br i1 %none, label %kept, label %give
      |give:
      |  call void %f(ptr %p, i1 true)
      |  ret void
      |kept:
      |  ret void
      |}
      |
      |define private void @arc.reap(ptr %p) noinline cold {
      |entry:
      |$fetchSlot  %head = getelementptr %arc.reaper, ptr $slot, i32 0, i32 0
      |  %flag = getelementptr %arc.reaper, ptr $slot, i32 0, i32 1
      |  %w = load ptr, ptr %head
      |  store ptr %w, ptr %p
      |  store ptr %p, ptr %head
      |  %d = load i8, ptr %flag
      |  %in = icmp ne i8 %d, 0
      |  br i1 %in, label %done, label %drain
      |drain:
      |  store i8 1, ptr %flag
      |  br label %loop
      |loop:
      |  %q = load ptr, ptr %head
      |  %e = icmp eq ptr %q, null
      |  br i1 %e, label %finish, label %step
      |step:
      |  %next = load ptr, ptr %q
      |  store ptr %next, ptr %head
      |  store $word 0, ptr %q
      |  call void @arc.destroy(ptr %q)
      |  br label %loop
      |finish:
      |  store i8 0, ptr %flag
      |  ret void
      |done:
      |  ret void
      |}
      |
      |define private void @arc.release(ptr %p) {
      |entry:
      |  %c = load $word, ptr %p
      |  %n = sub $word %c, 1
      |  store $word %n, ptr %p
      |  %z = icmp eq $word %n, 0
      |  br i1 %z, label %reap, label %live
      |reap:
      |  call void @arc.reap(ptr %p)
      |  ret void
      |live:
      |  ret void
      |}
      |
      |""".stripMargin
  }

  /** The three functions a `weak T` needs, emitted only into a module that holds one.
   *
   * `upgrade` is the whole of what separates a weak reference from a dangling one: it reads the
   * strong count, and a zero there means the object has already been destroyed, so the caller is
   * handed nothing instead of an address. A live count is taken, which is why the answer is a
   * reference the caller owns rather than one it has to be careful with.
   */
  def weak(target: Target): String =
    val word = target.word.llvm

    s"""define private void @arc.weak_retain(ptr %p) {
      |entry:
      |  %empty = icmp eq ptr %p, null
      |  br i1 %empty, label %done, label %live
      |live:
      |  %w = getelementptr %arc.header, ptr %p, i32 0, i32 2
      |  %c = load $word, ptr %w
      |  %n = add $word %c, 1
      |  store $word %n, ptr %w
      |  ret void
      |done:
      |  ret void
      |}
      |
      |define private void @arc.weak_release(ptr %p) {
      |entry:
      |  %empty = icmp eq ptr %p, null
      |  br i1 %empty, label %done, label %live
      |live:
      |  call void @arc.unshare(ptr %p)
      |  ret void
      |done:
      |  ret void
      |}
      |
      |define private ptr @arc.upgrade(ptr %p) {
      |entry:
      |  %empty = icmp eq ptr %p, null
      |  br i1 %empty, label %gone, label %check
      |check:
      |  %c = load $word, ptr %p
      |  %z = icmp eq $word %c, 0
      |  br i1 %z, label %gone, label %live
      |live:
      |  %n = add $word %c, 1
      |  store $word %n, ptr %p
      |  ret ptr %p
      |gone:
      |  ret ptr null
      |}
      |
      |""".stripMargin

  /** The atomic pair, emitted only into a module that has a `&sync` in it. */
  def atomic(target: Target): String =
    val word = target.word.llvm

    s"""define private void @arc.retain_sync(ptr %p) {
      |entry:
      |  %o = atomicrmw add ptr %p, $word 1 monotonic
      |  ret void
      |}
      |
      |define private void @arc.release_sync(ptr %p) {
      |entry:
      |  %o = atomicrmw sub ptr %p, $word 1 release
      |  %z = icmp eq $word %o, 1
      |  br i1 %z, label %reap, label %live
      |reap:
      |  fence acquire
      |  call void @arc.reap(ptr %p)
      |  ret void
      |live:
      |  ret void
      |}
      |
      |""".stripMargin

  /** The null-tolerant pair, emitted only into a module that holds a view of something.
   *
   * Alone among the four blocks this one names no count, so it needs no width: it is a null check
   * and a delegation, and the two functions it calls are the ones that know how wide a count is.
   */
  val maybe: String =
    """define private void @arc.retain_maybe(ptr %p) {
      |entry:
      |  %null = icmp eq ptr %p, null
      |  br i1 %null, label %done, label %live
      |live:
      |  call void @arc.retain(ptr %p)
      |  ret void
      |done:
      |  ret void
      |}
      |
      |define private void @arc.release_maybe(ptr %p) {
      |entry:
      |  %null = icmp eq ptr %p, null
      |  br i1 %null, label %done, label %live
      |live:
      |  call void @arc.release(ptr %p)
      |  ret void
      |done:
      |  ret void
      |}
      |
      |""".stripMargin
}
