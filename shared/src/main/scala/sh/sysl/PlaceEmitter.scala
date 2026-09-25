package sh.sysl

import ir.{Access, Arg, BinOp, CastOp, ICmp, Inst, LType, Val}

/** Addressing a place, and building the composite values that have one.
 *
 * Two things live here because they are the same subject seen from either end. A **place** is
 * something with an address — a local's slot, a dereference, a field or element of either — and
 * reaching one is a `getelementptr` chain rather than a read of the value it sits in, which is what
 * lets `s.f.g = v` write through without copying anything out. A **composite value** is what those
 * addresses point into: an array, a slice, an enum and its payload.
 *
 * Every element access is bounds-checked, and a slice carries the owner it borrows from, so making
 * one is also an ownership event rather than pointer arithmetic alone.
 */
trait PlaceEmitter extends ArcEmitter with ScalarEmitter {

  /** The marker that goes on the load or the store reaching a place, which is the whole of what
   * `volatile` costs (`reference/memory.md § Device memory`).
   *
   * It is asked of the place rather than of the value, because that is where the qualifier lives:
   * `regs.status` has type `u32` and sits in storage of type `volatile u32`, and the difference
   * between those two is exactly this word.
   */
  protected def vol(place: TExpr): String = access(place) match
    case Access.Volatile => " volatile"
    case _               => ""

  /** How an access to this place is reached, which is the same question the marker above answers
   * and the form an instruction carries.
   */
  protected def access(place: TExpr): Access = place match
    // A `ref` name carries no declaration of its own, so the qualifier comes from what the binding
    // found rather than from the node (`reference/memory.md § ref — a name for a place`).
    case TLoad(name, _) if refStorage.contains(name) => accessOf(refStorage(name))
    case _                                           => accessOf(place.placeTy)

  /** The same marker read off a type directly, for the paths that have the storage's type and not
   * the node it came from.
   */
  protected def qualifier(storage: Type): String =
    if Type.volatileIn(storage) then " volatile" else ""

  /** The same read off a type directly, as the instruction carries it. */
  protected def accessOf(storage: Type): Access =
    if Type.volatileIn(storage) then Access.Volatile else Access.Plain

  /** Whether `address` can walk to this node without giving it a slot of its own first — which is
   * every place, and nothing else.
   *
   * It exists because reading a field is lowered two ways. Ordinarily the receiver is produced as a
   * value and the field lifted out of it with `extractvalue`, which is one instruction and needs no
   * address. A **register** cannot be read that way: lifting one field out of a register block would
   * mean loading the whole block, and reading a status register is not how you find out what is in a
   * data register. So a volatile field is reached by walking to its address instead — which is only
   * possible when the receiver has one.
   */
  protected def hasAddress(e: TExpr): Boolean = e.isPlace

  /** The address of a place, as a `ptr` register or an existing slot name. Every place bottoms
   * out either in a local's stack slot or in a pointer the program already holds, so this walks
   * the field chain with `getelementptr` rather than reading values out with `extractvalue`.
   */
  protected def address(place: TExpr): Val = place match
    case TLoad(name, _)     => Val.Reg(s"$name.addr")
    // A `val`'s storage is the global itself, so its address needs no instruction to compute — it
    // is what makes indexing one reach into the table rather than copy it out first.
    case g: TGlobal         => Val.Global(g.symbol)
    case TDeref(operand, _) => payloadAddr(operand)
    // A zero-sized field occupies nothing, so it is wherever its receiver is: the address is never
    // read or written through, and handing back the receiver's keeps the walk to it — and whatever
    // that walk evaluates — exactly as it is for every other field.
    case TField(receiver, _, ty) if Type.zeroSized(ty) => address(receiver)

    // A **bitfield** has no address at all, and this says so rather than handing back the
    // container's — which is what a `getelementptr` to slot zero would be, and would be wrong for
    // every field but the first while looking right for all of them. Nothing reaches here:
    // `reference/types.md § Structs` refuses `&` on any packed field, and both the read and the
    // write of a bitfield go through its container by name (`Bitfields`).
    case TField(receiver, _, _) if bitfieldOf(receiver.ty).isDefined =>
      sys.error(s"unreachable address of a bitfield in ${receiver.ty.llvm}")

    case TField(receiver, index, _) =>
      val base = address(receiver)
      val r    = freshReg()
      emit(Inst.Gep(r, receiver.ty.lty, base,
        List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(fieldSlot(receiver.ty, index))))))
      r
    case TIndex(receiver, index, _) => elementAddr(receiver, index)

    // A computed value has no address of its own, so reaching into one means giving it a slot
    // first. The analyzer has already refused to *assign* through anything but a real place, so
    // this only ever happens on the way to a read.
    case other =>
      val slot = emitAlloca(freshReg(), other.ty.lty)
      // A large one is written into the slot rather than produced and then stored into it — the
      // slot is what it was going to end up in either way.
      if layout.indirect(other.ty) then genBorrowedInto(slot, other)
      else emit(Inst.Store(other.ty.lty, genExpr(other), slot, Access.Plain))
      slot

  /** Writes `v` into the place at `p`. A slot that holds a count takes one for the value arriving
   * and lets go of the one leaving, in that order, so assigning something to itself never briefly
   * drops the last count.
   */
  protected def storeInto(ty: Type, p: Val, v: Val, acc: Access = Access.Plain): Unit =
    if !containsRef(ty) then emit(Inst.Store(ty.lty, v, p, acc))
    // A large aggregate gives its old contents back at the address rather than out of a value read
    // for the purpose. The order is the same one the value form keeps and for the same reason: the
    // count for the arriving value is taken first, so assigning something to itself never briefly
    // drops the last one.
    else if layout.indirect(ty) && acc == Access.Plain then
      retainValue(ty, v)
      releaseAt(ty, p)
      emit(Inst.Store(ty.lty, v, p, Access.Plain))
    else
      val old = freshReg(); emit(Inst.Load(old, ty.lty, p, acc))

      retainValue(ty, v)
      emit(Inst.Store(ty.lty, v, p, acc))
      releaseValue(ty, old)

  /** `a, b = b, a` (`reference/expressions.md § Several places at once`), in the phases the form promises.
   *
   * Locating every place comes first, so an index that calls something calls it once and the calls
   * happen in written order. Then everything the statement reads is read — what each compound arm
   * finds in its place, and then the whole right side — and because no store has happened yet, every
   * operand of every arm sees the values the statement started with. The writes come left to right.
   *
   * **Every `invariant` re-check waits until all of them have landed**, and once per struct however
   * many of its fields were written. A struct whose invariant relates two fields is exactly what
   * this form is for, and checking after each field in turn would trap on the half-written state on
   * the way to a perfectly good one — `s.lo, s.hi = 6, 8` on a `Span` that was `1..5` would be
   * refused for the moment `lo` was `6` and `hi` still `5`.
   */
  protected def genMultiAssign(writes: List[TWrite]): Unit = {
    // A bitfield has no address of its own — `reference/types.md § Structs` refuses one outright —
    // so what `placeAddr` locates for one is the **container's**, which is where its
    // read-modify-write happens. Locating it in this phase rather than at the write is what keeps
    // the form's promise for a bitfield too: every place's own subexpressions are evaluated once,
    // and before anything is read.
    val addrs = writes.map(w => if Type.zeroSized(w.place.ty) then Val.Nothing else placeAddr(w.place))

    // Every read takes a count for the statement, which is what a form that reads everything before
    // it writes anything needs and a single assignment does not. Writing into the first place lets
    // go of what was there, and in a swap what was there is exactly the value the second arm is
    // holding — so without a count of its own that value can be freed between being read and being
    // stored. The statement's counts go back at the end of the statement, as every temporary's do.
    def held(v: Val, ty: Type): Val = {
      retainValue(ty, v)
      ownTemp(v, ty)
    }

    val curs = writes.zip(addrs).map { (w, p) =>
      if w.op == "=" || p == Val.Nothing then Val.Nothing else held(loadPlace(w.place, p), w.place.ty)
    }
    val vals = writes.map(w => held(genExpr(w.value), w.value.ty))

    for ((w, p), (cur, v)) <- writes.zip(addrs).zip(curs.zip(vals)) do
      if p != Val.Nothing then
        if w.op == "=" then storePlace(w.place, p, v)
        else
          val updated = combine(w.op, w.place.ty, w.value.ty, w.dispatch, cur, v)

          for c <- w.constraint do emitConstraintChecks(updated, c)

          storePlace(w.place, p, updated, Some(cur))

    for (recv, struct, invFn) <- writes.flatMap(_.check).distinct do
      emitInvCheck(genExpr(recv), struct, invFn)
  }

  // --- a place that is a bit range ------------------------------------------------------
  //
  // Four forms write through a place — `TStore`, `TUpdate`, `TIncDec` and each arm of a
  // `TMultiAssign` — and a bitfield changes the same three steps in all of them: what is located,
  // what a read of it costs, and what a write of it has to preserve. They are here so that a fifth
  // form cannot be added without meeting them, which is the failure this replaced: three of the four
  // reached `address` directly and got a `getelementptr` into a struct that has no such field.

  /** A place that is a bit range of a bitfield struct: the receiver holding the container, the
   * struct's ranges, and this field's one — or `None` for every other place, which is nearly all of
   * them (`Bitfields`).
   */
  protected def bitPlace(place: TExpr): Option[(TExpr, List[BitRange], BitRange)] = place match
    case TField(receiver, index, _) =>
      bitfieldOf(receiver.ty).map(ranges => (receiver, ranges, bitRange(receiver.ty, index).get))
    case _ => None

  /** Where a write to this place goes: a bitfield's **container**, since the field itself has no
   * address at all, and the place's own otherwise.
   */
  protected def placeAddr(place: TExpr): Val =
    address(bitPlace(place).map(_._1).getOrElse(place))

  /** What is in the place now — one load of the container and a shift for a bitfield.
   *
   * **One load, whatever the field's width**, which is what makes reading a bitfield register a
   * single bus read of the whole register rather than a sub-word access of part of it. The
   * qualifier is asked of the **receiver's storage** rather than of the field: every field of a
   * bitfield struct is bits of one word, so `volatile` on any of them qualifies the container they
   * share, which is what `Type.volatileIn` already answers by looking through a struct
   * (`reference/types.md § Structs`).
   */
  protected def loadPlace(place: TExpr, p: Val): Val = bitPlace(place) match
    case Some((recv, ranges, r)) => readBits(ranges, r, loadContainer(ranges, recv, p))
    case None =>
      val t = freshReg(); emit(Inst.Load(t, place.ty.lty, p, access(place))); t

  private def loadContainer(ranges: List[BitRange], receiver: TExpr, addr: Val): Val = {
    val t = freshReg()

    emit(Inst.Load(t, containerLty(ranges), addr, accessOf(receiver.placeTy)))
    t
  }

  /** Puts `v` in the place. `cur` is what a compound form already found there, so that the release
   * is of that value rather than of a second load — `TUpdate`'s arrangement, for its reason; `None`
   * is a plain assignment, which gives the old value up at the address instead.
   *
   * A bitfield needs neither: every field of a bitfield struct is an integer, so nothing there
   * carries a count. **Its container is re-read here** rather than reused from `loadPlace`, because
   * two arms of one statement may be two fields of one container and the second has to see what the
   * first left behind.
   */
  protected def storePlace(place: TExpr, p: Val, v: Val, cur: Option[Val] = None): Unit =
    bitPlace(place) match
      case Some((recv, ranges, r)) =>
        val acc = accessOf(recv.placeTy)
        val c   = loadContainer(ranges, recv, p)

        emit(Inst.Store(containerLty(ranges), writeBits(ranges, r, c, v), p, acc))

      case None =>
        val ty  = place.ty
        val acc = access(place)

        cur match
          case None => storeInto(ty, p, v, acc)
          case Some(old) =>
            if containsRef(ty) then
              retainValue(ty, v)
              emit(Inst.Store(ty.lty, v, p, acc))
              releaseValue(ty, old)
            else emit(Inst.Store(ty.lty, v, p, acc))

  /** Where a written field index lands in the emitted aggregate, once the zero-sized fields before
   * it are dropped.
   */
  protected def fieldSlot(recvTy: Type, index: Int): Int = recvTy match
    case s: Type.Struct => s.slot(index)
    case _              => index

  /** The address of one element, after checking that it exists. An array is indexed from its
   * own storage; a slice is indexed from the pointer it carries.
   */
  protected def elementAddr(receiver: TExpr, index: TExpr): Val = {
    // The length is what there is to check against, and a `*T` has none — so the pointer case
    // yields no length and the check is skipped. That is the whole difference between `p[i]` and
    // every other subscript, and it is `03`'s unchecked primitive doing what C's does.
    val (base, len, elem) = Type.unnamed(receiver.ty) match
      case Type.Array(n, e) => (address(receiver), Some(Val.Int(n)), e)
      case w: Type.View =>
        val v = genExpr(receiver)
        val p = freshReg(); emit(Inst.Extract(p, w.lty, v, List(1)))
        val l = freshReg(); emit(Inst.Extract(l, w.lty, v, List(2)))
        (p, Some(l), w.elem)
      case Type.Ptr(e) => (genExpr(receiver), None, e)
      case other       => sys.error(s"unreachable index into ${other.llvm}")

    val i = widenIndex(index)
    for l <- len do boundsCheck(i, l)
    val r = freshReg(); emit(Inst.Gep(r, elem.lty, base, List(Arg(wordLty, i)))); r
  }

  /** The address a run of `lanes` elements starts at, after checking that the whole run exists —
   * what `xs.load(i)` and `xs.store(i, v)` reach through.
   *
   * **The test is `i + lanes <= len` written so that it cannot overflow.** The obvious spelling
   * adds first and compares after, and on a `usize` an `i` near the top wraps to a small number
   * that passes — which is the one arithmetic mistake a bounds check must not make, since it turns
   * a check into a licence. Subtracting instead keeps both sides inside the range: the run fits
   * only if there are `lanes` elements at all *and* `i` is no further in than `len - lanes`. The
   * subtraction is evaluated either way and is meaningless when the first test fails, which costs
   * nothing — `and` is not a branch, and a wrapped `sub` is a defined value LLVM is happy to have
   * computed and then ignored.
   */
  protected def runAddr(receiver: TExpr, index: TExpr, lanes: Int): Val = {
    val (base, len, elem) = receiver.ty match
      case Type.Array(n, e) => (address(receiver), Val.Int(n), e)
      case w: Type.View =>
        val v = genExpr(receiver)
        val p = freshReg(); emit(Inst.Extract(p, w.lty, v, List(1)))
        val l = freshReg(); emit(Inst.Extract(l, w.lty, v, List(2)))
        (p, l, w.elem)
      case other => sys.error(s"unreachable run of ${other.llvm}")

    val i    = widenIndex(index)
    val room = freshReg(); emit(Inst.IntCmp(room, ICmp.Uge, wordLty, len, Val.Int(lanes)))
    val last = freshReg(); emit(Inst.Bin(last, BinOp.Sub, wordLty, len, Val.Int(lanes)))
    val here = freshReg(); emit(Inst.IntCmp(here, ICmp.Ule, wordLty, i, last))
    val ok   = freshReg(); emit(Inst.Bin(ok, BinOp.And, i1, room, here))

    trapUnless(ok, "bounds")

    val r = freshReg(); emit(Inst.Gep(r, elem.lty, base, List(Arg(wordLty, i)))); r
  }

  /** Takes a view of some of an array's, a slice's, or a string's elements. The base is evaluated
   * once and gives up three things — what keeps the elements alive, where the first of them is,
   * and how many there are — and the view is built by narrowing the last two and taking a share
   * of the first.
   */
  protected def genSlice(
      base: TExpr,
      lo: Option[TExpr],
      hi: Option[TExpr],
      inclusive: Boolean,
      sliceTy: Type.View,
  ): Val = {
    val elem = sliceTy.elem

    val (ownerV, first, len) = Type.unnamed(base.ty) match
      case Type.Ref(array @ Type.Array(n, _), _) =>
        val r = genExpr(base)
        val p = freshReg()

        emit(Inst.Gep(p, boxLty(array), r,
          List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))
        (r, p, Some(Val.Int(n)))
      case s: Type.View =>
        val v = genExpr(base)
        val o = freshReg(); emit(Inst.Extract(o, s.lty, v, List(0)))
        val p = freshReg(); emit(Inst.Extract(p, s.lty, v, List(1)))
        val l = freshReg(); emit(Inst.Extract(l, s.lty, v, List(2)))
        (o, p, Some(l))
      // Storage this frame owns, or a `*T` region. A frame-backed array has no owner, so counting
      // it is a no-op — unless the escape analysis moved it to the heap, in which case the buffer
      // it moved into is exactly what a view of it must count against, and the whole promotion
      // comes down to naming that buffer here instead of `null`. Nothing makes a `*T` region safe;
      // that is what `*T` is.
      // Storage this frame owns, storage inside a box the walk went through, or a `*T` region.
      case Type.Array(n, _) =>
        val (own, addr) = addressOwned(base)
        (own, addr, Some(Val.Int(n)))
      case Type.Ptr(Type.Array(n, _)) => (Val.Null, genExpr(base), Some(Val.Int(n)))
      // A `*T` region: no owner to count and no length to check the end against. The analyzer has
      // already insisted the end be written, since there is nothing here to supply one.
      case _: Type.Ptr                => (Val.Null, genExpr(base), None)
      case other                      => sys.error(s"unreachable slice of ${other.llvm}")

    val start = lo.map(widenIndex).getOrElse(Val.Int(0))

    // The check is on the half-open interval the view ends up naming. An inclusive high end
    // additionally has to name an element that exists, which is also what stops `hi + 1` from
    // wrapping past the end.
    val end = hi match
      case None => len.getOrElse(sys.error("unreachable open-ended slice of a pointer"))
      case Some(h) =>
        val v = widenIndex(h)
        if !inclusive then v
        else
          for l <- len do
            val within = freshReg(); emit(Inst.IntCmp(within, ICmp.Ult, wordLty, v, l))
            trapUnless(within, "bounds")
          val e = freshReg(); emit(Inst.Bin(e, BinOp.Add, wordLty, v, Val.Int(1))); e

    for l <- len if hi.isDefined && !inclusive do
      val fits = freshReg(); emit(Inst.IntCmp(fits, ICmp.Ule, wordLty, end, l))
      trapUnless(fits, "bounds")

    val ordered = freshReg(); emit(Inst.IntCmp(ordered, ICmp.Ule, wordLty, start, end))
    trapUnless(ordered, "bounds")

    // A substring has to be a string, so both ends must fall between characters. This runs after
    // the bounds checks, which is what makes reading the byte at either end safe.
    if sliceTy == Type.Str then
      // A string is a view and so always has one; only a `*T` region does not.
      val l = len.getOrElse(sys.error("unreachable string slice with no length"))

      trapUnless(strBoundary(first, l, start), "boundary")
      trapUnless(strBoundary(first, l, end), "boundary")

    val p = freshReg(); emit(Inst.Gep(p, elem.lty, first, List(Arg(wordLty, start))))
    val n = freshReg(); emit(Inst.Bin(n, BinOp.Sub, wordLty, end, start))

    emit(Inst.Call(None, LType.Void, Val.Global("arc.retain_maybe"), List(Arg(LType.Ptr, ownerV))))
    maybeHeap = true
    heap = true

    val withOwner = freshReg(); emit(Inst.Insert(withOwner, sliceTy.lty, Val.Zero, LType.Ptr, ownerV, List(0)))
    val withPtr   = freshReg(); emit(Inst.Insert(withPtr, sliceTy.lty, withOwner, LType.Ptr, p, List(1)))
    val whole     = freshReg(); emit(Inst.Insert(whole, sliceTy.lty, withPtr, wordLty, n, List(2)))

    ownTemp(whole, sliceTy)
  }

  /** Storage for `n` elements, with one count taken for whoever is about to view it. Yields where
   * the box is and where its elements start.
   *
   * A count the program computed is where the arithmetic can go wrong, so the size is built with
   * checked arithmetic: a count that would wrap traps rather than allocating something smaller than
   * the elements that are about to be written into it, and an allocation that fails traps rather
   * than handing back a null those elements are then stored through. Both are `reference/arrays.md
   * § Indexing`'s trap for `reference/arrays.md § Indexing`'s reason — the guarantee is that a
   * program with no `*T` in it cannot fault.
   */
  /** The buffer a promoted array lives in, or `null` for one that still lives in the frame.
   *
   * Only a plain local is ever promoted, which is the same shape the analysis names its roots by
   * (`05`): storage reached through a field or an index belongs to something else, and an array
   * parameter is the caller's layout, so neither is this body's to have moved.
   */
  protected def promotedOwner(base: TExpr): Val = base match
    case TLoad(name, _) if promotedBoxes.contains(name) => promotedBoxes(name)
    // A `ref` names storage it did not declare, so the box belongs to whatever its place was rooted
    // at (`reference/memory.md § ref — a name for a place`). Without this step a view taken through
    // a ref would own nothing, and the buffer the array was promoted into would be released while
    // the view still pointed into it — a promotion that silently undid itself.
    case TLoad(name, _) if refPlaceOf.contains(name)    => promotedOwner(refPlaceOf(name))
    case _: TLoad                                       => Val.Null
    case TIndex(r, _, _)                                => promotedOwner(r)
    case _                                              => Val.Null

  /** The dereference a place walk bottoms out at, following **receivers only**.
   *
   * A place is a chain of field and element steps over something with an address, and the thing at
   * the end of that chain is what decides who owns the storage. The index *expressions* along the
   * way are not part of the chain — `g.rows[p.i]` reaches `p` while it works out which row, and `p`
   * owns nothing here — so this follows the receiver of each step and nothing else.
   *
   * An element step is followed only through an array, because an array's elements are its own
   * storage. Through a slice they are somebody else's, and the slice already carries who.
   */
  private def spineRoot(place: TExpr): Option[TDeref] = place match
    case TField(r, _, _)                                => spineRoot(r)
    case TIndex(r, _, _) if r.ty.isInstanceOf[Type.Array] => spineRoot(r)
    case d: TDeref                                      => Some(d)
    case _                                              => None

  /** The address of an array place together with whatever keeps its storage alive: the box it lives
   * inside, the buffer a promotion moved it to, or nothing at all for storage on the frame.
   *
   * A place rooted at a **counted reference** is the box's, so the reference is evaluated once here
   * and the walk to the field continues from the payload it addresses — which is what lets a view of
   * a fixed array inside a `&Struct` name that box as its owner (`05`). Reaching for it structurally
   * rather than watching what the ordinary walk happened to compute is what keeps a `&Struct` inside
   * another `&Struct` honest: the box that owns the storage is the **innermost** one on the chain,
   * and the ordinary walk evaluates the outermost first.
   */
  protected def addressOwned(place: TExpr): (Val, Val) =
    spineRoot(place) match
      case Some(root @ TDeref(operand, inner)) if operand.ty.isInstanceOf[Type.Ref] =>
        val box = genExpr(operand)
        val at  = freshReg()

        emit(Inst.Gep(at, boxLty(inner), box,
          List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))
        (box, addressUnder(place, root, at))

      case _ => (promotedOwner(place), address(place))

  /** The rest of a place walk, once its root has been evaluated and reached. Every step is the one
   * `address` takes; what differs is only where the chain starts.
   */
  private def addressUnder(place: TExpr, root: TDeref, at: Val): Val = place match
    case p if p eq root                          => at
    case TField(r, _, ty) if Type.zeroSized(ty)  => addressUnder(r, root, at)

    case TField(r, index, _) =>
      val base = addressUnder(r, root, at)
      val x    = freshReg()
      emit(Inst.Gep(x, r.ty.lty, base,
        List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(fieldSlot(r.ty, index))))))
      x

    case TIndex(r, index, _) =>
      val Type.Array(n, elem) = r.ty: @unchecked
      val base = addressUnder(r, root, at)
      val i    = widenIndex(index)
      boundsCheck(i, Val.Int(n))
      val x = freshReg(); emit(Inst.Gep(x, elem.lty, base, List(Arg(wordLty, i))))
      x

    case other => address(other)

  protected def genBuffer(elem: Type, n: Val): (Val, Val) = {
    val bn = bufLty(elem)
    checked = true

    val e1   = freshReg(); emit(Inst.Gep(e1, elem.lty, Val.Null, List(Arg(LType.I(64), Val.Int(1)))))
    val esz  = freshReg(); emit(Inst.Cast(esz, CastOp.PtrToInt, LType.Ptr, e1, wordLty))
    val h1   = freshReg()

    emit(Inst.Gep(h1, bn, Val.Null, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields + 1)))))
    val hsz  = freshReg(); emit(Inst.Cast(hsz, CastOp.PtrToInt, LType.Ptr, h1, wordLty))

    // The overflow intrinsics carry their width in the **name** as well as in the signature, so a
    // size computed at the machine's own width has to name the matching overload — and getting that
    // wrong is not a type error LLVM would catch, it is a call to a function that does not exist.
    val pair  = LType.Struct(List(wordLty, i1))
    val mul   = freshReg()

    emit(Inst.Call(Some(mul), pair, Val.Global(Llvm.withOverflow("mul", signed = false).at(wordLty)),
      List(Arg(wordLty, n), Arg(wordLty, esz))))
    val bytes = freshReg(); emit(Inst.Extract(bytes, pair, mul, List(0)))
    val over1 = freshReg(); emit(Inst.Extract(over1, pair, mul, List(1)))
    val add   = freshReg()

    emit(Inst.Call(Some(add), pair, Val.Global(Llvm.withOverflow("add", signed = false).at(wordLty)),
      List(Arg(wordLty, bytes), Arg(wordLty, hsz))))
    val total = freshReg(); emit(Inst.Extract(total, pair, add, List(0)))
    val over2 = freshReg(); emit(Inst.Extract(over2, pair, add, List(1)))
    val over  = freshReg(); emit(Inst.Bin(over, BinOp.Or, i1, over1, over2))
    val fits  = freshReg(); emit(Inst.Bin(fits, BinOp.Xor, i1, over, Val.Bool(true)))
    trapUnless(fits, "size")

    val p   = freshReg(); emit(Inst.Call(Some(p), LType.Ptr, Val.Global(mallocSym), List(Arg(wordLty, total))))
    val got = freshReg(); emit(Inst.IntCmp(got, ICmp.Ne, LType.Ptr, p, Val.Null))
    trapUnless(got, "alloc")

    emit(Inst.Store(wordLty, Val.Int(1), p, Access.Plain))
    val hook = freshReg(); emit(Inst.Gep(hook, bn, p, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(1)))))
    emit(Inst.Store(LType.Ptr, dropBufFn(elem), hook, Access.Plain))
    val wc   = freshReg(); emit(Inst.Gep(wc, bn, p, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(2)))))
    emit(Inst.Store(wordLty, Val.Int(1), wc, Access.Plain))
    val lenp = freshReg(); emit(Inst.Gep(lenp, bn, p, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields)))))
    emit(Inst.Store(wordLty, n, lenp, Access.Plain))
    val data = freshReg()

    emit(Inst.Gep(data, bn, p,
      List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(headerFields + 1)))))

    (p, data)
  }

  /** The view of a whole buffer: the box keeps the elements alive, and the one count it was made
   * with is the count this view holds.
   */
  protected def bufferView(sliceTy: Type.Slice, box: Val, data: Val, n: Val): Val = {
    maybeHeap = true

    val withOwner = freshReg(); emit(Inst.Insert(withOwner, sliceTy.lty, Val.Zero, LType.Ptr, box, List(0)))
    val withPtr   = freshReg(); emit(Inst.Insert(withPtr, sliceTy.lty, withOwner, LType.Ptr, data, List(1)))
    val whole     = freshReg(); emit(Inst.Insert(whole, sliceTy.lty, withPtr, wordLty, n, List(2)))

    ownTemp(whole, sliceTy)
  }

  /** An index at **the machine's own width**, keeping its signedness so a negative one stays
   * negative through the widening and then fails the unsigned bounds test.
   *
   * It is the address width rather than a fixed sixty-four because what it is about to be compared
   * against is a length, and a length is a `usize`. Widening to a constant 64 was right for as long
   * as every target was — and produced an `icmp` between an `i64` and an `i32` the moment one was
   * not, which is what `CrossTargetBuildTests` caught.
   *
   * **A *wider* index is narrowed, and the test that makes that safe is emitted here rather than
   * refused in the analyzer.** No storage can hold more than `usize` elements, so an index that does
   * not fit in one names nothing — which makes it an ordinary out-of-bounds index and not a program
   * the compiler has to decline. Testing before the truncation is what keeps it honest: `2^64 + 5`
   * at 128 bits would arrive as 5 and pass a six-element check, so the fit is asked at the *index's*
   * width, where the value is still all there.
   *
   * It is read **unsigned** for that test, exactly as the bounds check below reads it, so a negative
   * index arrives as a very large one and fails — the same answer it gets on a machine where it
   * would have needed no narrowing at all.
   *
   * **Sixteen bits is what made this the ordinary case rather than an exotic one.** Until CRAFT
   * every target's address was as wide as an `int` or wider, so the only index this reached was a
   * `u128`, and refusing those cost nothing. On a machine with a 64 KiB address space `int` is
   * wider than `usize`, so `for i in 0..<4 do b[i] …` is the case — which `reference/arrays.md §
   * Indexing` names in as many words as the thing that must not need a conversion.
   */
  protected def widenIndex(index: TExpr): Val = Type.underlying(index.ty) match
    case i: Type.Integer if i.bits <= target.word.bits =>
      convert(i, Type.Integer(target.word.bits, i.signed), genExpr(index))

    case i: Type.Integer =>
      val v     = genExpr(index)
      val wide  = LType.I(i.bits)
      val limit = Val.Int((BigInt(1) << target.word.bits) - 1)
      val fits  = freshReg()

      emit(Inst.IntCmp(fits, ICmp.Ule, wide, v, limit))
      trapUnless(fits, "bounds")
      convert(i, Type.Integer(target.word.bits, i.signed), v)

    case other => sys.error(s"unreachable index of type ${other.llvm}")

  /** Traps unless `i` names an element that exists. The comparison is unsigned at the address width,
   * so a negative index arrives as a very large one and fails the same test.
   */
  protected def boundsCheck(i: Val, len: Val): Unit = {
    val ok = freshReg(); emit(Inst.IntCmp(ok, ICmp.Ult, wordLty, i, len))
    trapUnless(ok, "bounds")
  }

  /** Builds an enum value from already-lowered payload values: the tag, then the variant's payload
   * aggregate written into the region every variant shares.
   *
   * The region is a union, so the payload cannot be dropped in with `insertvalue` — an aggregate
   * value has no operation that reinterprets part of it as another type. It goes through a stack
   * slot instead: written at the variant's own type, read back at the enum's. A nullary variant
   * never touches the region and so needs no slot at all.
   */
  protected def enumValue(en: Type.Enum, variant: Type.EnumVariant, vals: List[Val]): Val =
    if en.simple then Val.Int(variant.tag)
    else if !variant.carries then
      val tagged = freshReg()
      emit(Inst.Insert(tagged, en.lty, Val.Undef, i32, Val.Int(variant.tag), List(0)))
      tagged
    else
      var payload: Val = Val.Undef
      for (v, i) <- vals.zipWithIndex if !Type.zeroSized(variant.fields(i)._2) do
        val r = freshReg()
        emit(Inst.Insert(r, en.payloadLty(variant), payload,
          variant.fields(i)._2.lty, v, List(variant.slot(i))))
        payload = r
      val slot = scratchSlot(en.lty)
      emit(Inst.Store(i32, Val.Int(variant.tag), slot, Access.Plain))
      val p = payloadPtr(en, slot)
      emit(Inst.Store(en.payloadLty(variant), payload, p, Access.Plain))
      val r = freshReg()
      emit(Inst.Load(r, en.lty, slot, Access.Plain))
      r

  /** Whether an integer `v` of type `vt` equals one of the enum's declared discriminants — the
   * membership test both integer-to-enum conversions share. Every comparison is done at 64 bits
   * so a wide source cannot alias a narrow discriminant, matching how the checked `char`
   * conversion widens before testing.
   */
  protected def enumMembership(en: Type.Enum, vt: Type.Integer, v: Val): Val = {
    val wide = convert(vt, Type.Integer(64, vt.signed), v)
    en.variants.map { variant =>
      val eq = freshReg(); emit(Inst.IntCmp(eq, ICmp.Eq, LType.I(64), wide, Val.Int(variant.tag)))
      eq
    }.reduceOption(orI1).getOrElse(no)
  }

  /** `Color(n)` — the checked cast. Traps unless `n` is a declared discriminant, then stores the
   * value at the enum's underlying width, which is the enum's representation.
   */
  protected def genEnumFromInt(value: TExpr, en: Type.Enum): Val = {
    // Through `repr`, because a transparent subtype *is* its base (`reference/errors.md §
    // Constrained types`) and the analyzer admits one here on exactly that ground — the value is
    // laid out as the base and converts as it.
    val vt = Type.repr(value.ty).asInstanceOf[Type.Integer]
    val v  = genExpr(value)
    trapUnless(enumMembership(en, vt, v), "enum")
    convert(vt, en.underlying, v)
  }

  /** `Color.try(n)` — the fallible constructor. The membership test picks the branch: a match
   * builds `Some(n as Color)`, a miss builds `None`. The result is an ordinary `Option[Color]`,
   * whose element has no refcount, so a merge slot needs no ownership bookkeeping.
   */
  protected def genEnumTry(value: TExpr, en: Type.Enum, optTy: Type.Enum,
                         some: Type.EnumVariant, none: Type.EnumVariant): Val = {
    val vt    = Type.repr(value.ty).asInstanceOf[Type.Integer]
    val v     = genExpr(value)
    val ok    = enumMembership(en, vt, v)
    val slot  = emitAlloca(freshReg(), optTy.lty)
    val someL = freshLabel("try.some")
    val noneL = freshLabel("try.none")
    val endL  = freshLabel("try.end")

    emitTerm(Inst.CondBr(ok, someL, noneL))
    emitLabel(someL)
    val ev = convert(vt, en.underlying, v)
    emit(Inst.Store(optTy.lty, enumValue(optTy, some, List(ev)), slot, Access.Plain))
    emitTerm(Inst.Br(endL))
    emitLabel(noneL)
    emit(Inst.Store(optTy.lty, enumValue(optTy, none, Nil), slot, Access.Plain))
    emitTerm(Inst.Br(endL))
    emitLabel(endL)
    val r = freshReg(); emit(Inst.Load(r, optTy.lty, slot, Access.Plain)); r
  }

  /** Weakens a reference: the same address, counted in the box's third word instead of its first
   * (`03`). The strong count is untouched, which is the whole point — the edge this makes keeps
   * nothing alive.
   *
   * The value is registered as an owned temporary so the region that produced it gives the weak
   * share back, exactly as it would for a reference. What ends up holding it long-term is the slot
   * it is stored into, which takes a share of its own when it is written.
   */
  protected def genDowngrade(value: TExpr, weakTy: Type.Weak): Val = {
    val v = genExpr(value)
    retainValue(weakTy, v)
    ownTemp(v, weakTy)
  }

  /** `w.get()` — asks the box whether the object is still there. A live one comes back with a count
   * taken for the caller, so what the `Some` carries is an ordinary owned reference; a dead one
   * gives a null address, which is the `None`.
   *
   * A trait object is rebuilt around the answer rather than reused: the method table it was carrying
   * is a constant that says nothing about whether the object is alive, so it is put back beside the
   * address only on the arm where there is one.
   */
  protected def genUpgrade(value: TExpr, optTy: Type.Enum, some: Type.EnumVariant,
                           none: Type.EnumVariant): Val = {
    weakHeap = true
    val fat  = value.ty.asInstanceOf[Type.Weak].inner.isInstanceOf[Type.Trait]
    val v    = genExpr(value)
    val addr = if fat then { val b = freshReg(); emit(Inst.Extract(b, LType.fat, v, List(1))); b } else v

    val got   = freshReg(); emit(Inst.Call(Some(got), LType.Ptr, Val.Global("arc.upgrade"), List(Arg(LType.Ptr, addr))))
    val live  = freshReg(); emit(Inst.IntCmp(live, ICmp.Ne, LType.Ptr, got, Val.Null))
    val slot  = emitAlloca(freshReg(), optTy.lty)
    val someL = freshLabel("weak.live")
    val noneL = freshLabel("weak.gone")
    val endL  = freshLabel("weak.end")

    emitTerm(Inst.CondBr(live, someL, noneL))
    emitLabel(someL)
    val strong =
      if !fat then got
      else
        val tbl = freshReg(); emit(Inst.Extract(tbl, LType.fat, v, List(0)))
        val f0  = freshReg(); emit(Inst.Insert(f0, LType.fat, Val.Undef, LType.Ptr, tbl, List(0)))
        val f1  = freshReg(); emit(Inst.Insert(f1, LType.fat, f0, LType.Ptr, got, List(1)))
        f1
    emit(Inst.Store(optTy.lty, enumValue(optTy, some, List(strong)), slot, Access.Plain))
    emitTerm(Inst.Br(endL))
    emitLabel(noneL)
    emit(Inst.Store(optTy.lty, enumValue(optTy, none, Nil), slot, Access.Plain))
    emitTerm(Inst.Br(endL))
    emitLabel(endL)
    val r = freshReg(); emit(Inst.Load(r, optTy.lty, slot, Access.Plain))
    ownTemp(r, optTy)
  }

  /** Reads every field of a variant's payload out of an enum value. */
  protected def payloadFields(en: Type.Enum, variant: Type.EnumVariant, value: Val): List[Val] =
    if !variant.carries then Nil
    else
      val p = enumPayload(en, variant, value)
      variant.fields.indices.map { i =>
        if Type.zeroSized(variant.fields(i)._2) then Val.Nothing
        else
          val f = freshReg()
          emit(Inst.Extract(f, en.payloadLty(variant), p, List(variant.slot(i))))
          f
      }.toList
}
