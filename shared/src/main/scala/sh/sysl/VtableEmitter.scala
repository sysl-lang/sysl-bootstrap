package sh.sysl

import ir.{Arg, Inst, LType, Val}

/** The method tables trait objects dispatch through (`02`).
 *
 * A table is a constant array of function pointers, one per method the trait declares, and every
 * entry has the same shape: the object's data word first, then the method's own parameters. That
 * uniformity is what lets a call site load a slot and call it knowing only the trait.
 *
 * Keeping it uniform is what the adapters here are for. An implementation may want its receiver by
 * value, behind a pointer, or behind a reference, and the data word addresses the value itself in a
 * raw object but the reference-counted box in a counted one — so between the word a call site has
 * and the receiver an implementation declared there may be a header to step over and a value to
 * load. Where the two already agree, the implementation goes into the slot directly.
 */
trait VtableEmitter extends ArcEmitter {

  /** One table's constant, with whatever adapters its slots need queued for emission.
   *
   * **The type's identity is the first word, and the slots follow it** (`TypeId`). That is what an
   * erased value carries `o::Id` in: the object's first word is this table's address, so one load
   * answers a question the value itself has forgotten. It costs a word per table — one per (trait,
   * implementing type, memory mode) — and nothing per value.
   *
   * The layout is a struct rather than a longer array of pointers with an `inttoptr` in front,
   * because that is what is actually there: a word and then function pointers. `VtableEmitter` and
   * the one call site that indexes a slot are the only two things that know it.
   */
  protected def genVtable(vt: TVtable): ir.Global = {
    val entries = vt.slots.map(s => Arg(LType.Ptr, Val.Global(slotFn(vt, s))))
    val slots   = LType.Arr(vt.slots.length, LType.Ptr)

    ir.Global(vt.name, constant = true, LType.Struct(List(wordLty, slots)),
              Some(Val.Agg(List(Arg(wordLty, Val.Int(vt.typeId)), Arg(slots, Val.Array(entries))))))
  }

  /** The function a slot holds: the implementation itself where its receiver already *is* the data
   * word — a `*self` method reached through a raw object, a `&self` method reached through a counted
   * one — and an adapter for everything else.
   */
  private def slotFn(vt: TVtable, slot: TVSlot): String = (slot.recv, vt.boxed) match
    case (RecvMode.ByPtr, false)   => slot.target
    case (RecvMode.ByRef(_), true) => slot.target
    case _                         => adapter(vt, slot)

  /** The large by-value parameters each function reads where its caller put them (`Codegen`). */
  protected def readsInPlace: Map[String, Set[String]]

  /** The name a method's receiver goes by, which is its first parameter's. */
  private def receiverOf(fn: String): Option[String] =
    program.funcs.find(_.name == fn).flatMap(_.params.headOption).map(_._1)

  private def adapter(vt: TVtable, slot: TVSlot): String = {
    val name    = s"vt.adapt.${if vt.boxed then "ref." else ""}${slot.target}"
    // The signature carries the extension a narrow result owes whoever called through the table;
    // the `ret` at the bottom names the type alone, a terminator taking no return attribute.
    val ret     = syslResult(slot.retTy)
    // A large result is written into the caller's storage, so the adapter neither receives it in a
    // register nor returns one — it forwards the out-pointer it was handed and returns nothing.
    val out     = syslSret(slot.retTy)
    // A zero-sized parameter is not in the implementation's signature, so it is not in the
    // adapter's either — the two have to agree, and the argument was never a word to forward.
    val forwarded = slot.params.zipWithIndex.filterNot((t, _) => Type.zeroSized(t))
    val declare   = forwarded.map((t, i) => ir.Param(syslParamLty(t), name = Some(Val.Reg(s"a$i"))))
    val pass      = forwarded.map { case (t, i) => Arg(syslParamLty(t), Val.Reg(s"a$i")) }

    requestFunction(name)(
      ir.FuncSig(name,
                 ir.FnType(ret.ret,
                           out.map(_.copy(name = Some(sretParam))).toList :::
                             ir.Param(LType.Ptr, name = Some(Val.Reg("d"))) :: declare,
                           retAttrs = ret.retAttrs),
                 ir.Linkage.Private)) {
        val payload =
          if !vt.boxed then Val.Reg("d")
          else
            val p = freshReg()
            emit(Inst.Gep(p, LType.Named(boxName(vt.forType)), Val.Reg("d"),
                          List(Arg(LType.I(32), Val.Int(0)), Arg(LType.I(32), Val.Int(headerFields)))))
            p

        // The receiver is *borrowed* here, not owned: the implementation retains its parameters on
        // entry and releases them on return, so handing it a value loaded out of the object leaves
        // the object's own count exactly where it was.
        // A large receiver the implementation reads in place (`readsInPlace`) is the object
        // itself unless a copy is made here, and the object is storage the call may change — so
        // the adapter stages the snapshot a caller would have, holding what it refers to for the
        // length of the call.
        val staged =
          if slot.recv == RecvMode.ByValue && layout.indirect(vt.forType) &&
             readsInPlace.get(slot.target).exists(ps => receiverOf(slot.target).exists(ps))
          then
            val copy = emitAlloca(freshReg(), vt.forType.lty)
            emitMemcpy(copy, payload, layout.size(vt.forType), layout.align(vt.forType))
            retainAt(vt.forType, copy)
            Some(copy)
          else None

        val self = slot.recv match
          // A large receiver is passed at its address like any other large argument.
          case RecvMode.ByValue if layout.indirect(vt.forType) => Arg(LType.Ptr, staged.getOrElse(payload))
          case RecvMode.ByValue =>
            val v = freshReg(); emit(Inst.Load(v, vt.forType.lty, payload, ir.Access.Plain))
            Arg(vt.forType.lty, v)
          case RecvMode.ByPtr => Arg(LType.Ptr, payload)
          // A `&self` method on a raw object is refused where the object's type is formed, and on a
          // counted one the implementation is named directly, so no adapter is ever built for it.
          case RecvMode.ByRef(_) => sys.error("unreachable adapter for a '&self' method")

        // The out-pointer is forwarded with its `sret` intact and its `noalias` dropped: the adapter
        // did not create the storage and cannot promise nothing else addresses it.
        val forward = out.map(o => Arg(LType.Ptr, sretParam,
                                       o.attrs.filterNot(_ == ir.Attr.NoAlias))).toList
        val call    = forward ::: self :: pass

        if ret.ret == LType.Void then
          emit(Inst.Call(None, LType.Void, Val.Global(slot.target), call))
          staged.foreach(releaseAt(vt.forType, _))
          emitTerm(Inst.Ret(None, None))
        else
          val r = freshReg()
          emit(ret.call(Some(r), Val.Global(slot.target), call))
          staged.foreach(releaseAt(vt.forType, _))
          emitTerm(Inst.Ret(Some(syslResultLty(slot.retTy)), Some(r)))
    }
  }
}
