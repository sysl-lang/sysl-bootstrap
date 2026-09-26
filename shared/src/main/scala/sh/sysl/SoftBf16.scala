package sh.sysl

import sh.sysl.ir.*

import scala.collection.mutable

/** **`bf16` on a machine whose back end cannot convert it** — a module rewritten so that no `bfloat`
 * is left in it, for the targets `Target.bf16AsBits` names.
 *
 * `bf16` is a shorter `f32` (`reference/types.md § The two sixteen-bit formats`): its sixteen bits
 * are the top sixteen of a binary32, which is the whole of what this needs. Every other back end sysl
 * targets lowers a `bfloat` operation by widening it to `float`, computing, and rounding back, and
 * WebAssembly's does too — except that it has no way to select the two conversions themselves, so
 * the first `fpext` or `fptrunc` it meets stops the build with *"Cannot select: bf16_to_fp"*. Nothing
 * short of removing the type gets past it: a `bitcast` to `i16` and an `xor` of the sign bit is
 * folded straight back into an `fneg bfloat`, which fails the same way.
 *
 * **So a `bf16` is carried as an `i16` of its bits, and every operation on one is the widening the
 * design describes, written out.** The widening is exact and is a shift; the narrowing is binary32
 * rounded to the nearest `bf16`, ties to even, with a NaN kept a quiet NaN. That is the answer the
 * other back ends give for the same program, which is the claim `SoftBf16Tests` checks by running a
 * host program both ways.
 *
 * **Two conversions need more than the narrowing**, because they are where one rounding would
 * otherwise become two: a `real` and an integer wider than a binary32's significand. Each is first
 * rounded *to odd* at binary32 — truncated, with the lowest bit set if anything was lost — which is
 * what makes the second rounding the only one that counts. An arithmetic operation needs no such
 * care: a binary32 result of two `bf16` operands rounds to the same `bf16` either way.
 */
object SoftBf16 {

  private val Bf  = LType.F(16, brain = true)
  private val I1  = LType.I(1)
  private val I16 = LType.I(16)
  private val I32 = LType.I(32)
  private val F32 = LType.F(32)

  /** The module with every `bfloat` replaced by the `i16` of its bits. */
  def lower(m: Module): Module = {
    val added    = mutable.LinkedHashMap.empty[String, FuncSig]
    val replaced = mutable.Set.empty[String]

    def fn(f: Func): Func = new Lowering(added, replaced).func(f)

    val funcs   = m.funcs.map(fn)
    val thunks  = m.thunks.map(fn)
    val entry   = m.entry.map(fn)
    val init    = m.init.map(i => Initializer(fn(i.func), global(i.list)))
    val runtime = m.runtime.map {
      case Runtime.Emitted(f) => Runtime.Emitted(fn(f))
      case template           => template
    }

    val kept     = m.declares.filterNot(d => replaced(d.name)).map(sig)
    val declares = kept ++ added.values.filterNot(a => kept.exists(_.name == a.name))

    Module(m.triple, declares, m.structs.map(typeDef), m.enums.map(typeDef), m.boxes.map(typeDef),
           m.imports.map(sig), m.globals.map(global), runtime, funcs, thunks, entry,
           m.used.map(global), init)
  }

  /** The sixteen bits of the `bf16` nearest a `real`, ties to even — the narrowing below, at
   * compile time, for a constant. A constant the compiler wrote down at `bf16` is one exactly, so
   * this rounds nothing in practice; it is written as the general case so that it cannot be wrong.
   */
  private[sysl] def bitsOf(doubleBits: Long): Int = {
    val d = java.lang.Double.longBitsToDouble(doubleBits)
    val f = d.toFloat
    var u = java.lang.Float.floatToRawIntBits(f)

    if !d.isNaN && f.toDouble != d then
      if math.abs(f.toDouble) > math.abs(d) then u -= 1
      u |= 1

    if java.lang.Float.isNaN(f) then ((u >>> 16) | 0x40) & 0xffff
    else ((u + 0x7fff + ((u >>> 16) & 1)) >>> 16) & 0xffff
  }

  // ---- types and constants, which change spelling and nothing else ------------------------------

  private def ty(t: LType): LType = t match
    case LType.F(16, true) => I16
    case LType.Arr(n, e)   => LType.Arr(n, ty(e))
    case LType.Vec(n, e)   => LType.Vec(n, ty(e))
    case LType.Struct(fs)  => LType.Struct(fs.map(ty))
    case other             => other

  private def attr(a: Attr): Attr = a match
    case Attr.SRet(t)  => Attr.SRet(ty(t))
    case Attr.ByVal(t) => Attr.ByVal(ty(t))
    case other         => other

  private def param(p: Param): Param     = p.copy(ty = ty(p.ty), attrs = p.attrs.map(attr))
  private def fnType(f: FnType): FnType  = f.copy(ret = ty(f.ret), params = f.params.map(param),
                                                  retAttrs = f.retAttrs.map(attr))
  private def sig(s: FuncSig): FuncSig   = s.copy(ty = fnType(s.ty))
  private def typeDef(d: TypeDef): TypeDef = d.copy(fields = d.fields.map(ty))
  private def global(g: Global): Global  = g.copy(ty = ty(g.ty), value = g.value.map(value(_, g.ty)))
  private def arg(a: Arg): Arg           = Arg(ty(a.ty), value(a.value, a.ty), a.attrs.map(attr))

  /** A value of type `t`, respelled. Only a floating constant at `bf16` changes: it becomes its bits,
   * written signed so that it reads as an `i16` whatever the top bit is.
   */
  private def value(v: Val, t: LType): Val = (v, t) match
    case (Val.Float(bits), LType.F(16, true)) =>
      val b = bitsOf(bits)
      Val.Int(BigInt(if b >= 0x8000 then b - 0x10000 else b))
    case (Val.Splat(lane, x), _) => Val.Splat(ty(lane), value(x, lane))
    case (Val.Agg(fs), _)        => Val.Agg(fs.map(arg))
    case (Val.Array(es), _)      => Val.Array(es.map(arg))
    case _                       => v

  /** Whether `t` is a `bf16` a register holds — a scalar or a vector of them — which is what an
   * operation has to be rewritten for. A `bf16` inside an aggregate is only ever moved.
   */
  private def isBf(t: LType): Boolean = t match
    case LType.F(16, true)                => true
    case LType.Vec(_, LType.F(16, true))  => true
    case _                                => false

  /** `lane` in the shape of `t`: itself for a scalar, `<N x lane>` for a vector of `N`. */
  private def as(t: LType, lane: LType): LType = t match
    case LType.Vec(n, _) => LType.Vec(n, lane)
    case _               => lane

  /** An integer constant in the shape of `t`. */
  private def k(t: LType, c: BigInt): Val = t match
    case LType.Vec(_, lane) => Val.Splat(lane, Val.Int(c))
    case _                  => Val.Int(c)

  /** A floating constant in the shape of `t`, from a `real`'s bits. */
  private def kf(t: LType, bits: Long): Val = t match
    case LType.Vec(_, lane) => Val.Splat(lane, Val.Float(bits))
    case _                  => Val.Float(bits)

  private val inf = java.lang.Double.doubleToRawLongBits(Double.PositiveInfinity)

  /** One function's rewrite. The registers it adds are named `bf16.soft.N`, which nothing else
   * produces: a local's slot is `<name>.addr` and a temporary is `t<N>`.
   */
  private final class Lowering(added: mutable.LinkedHashMap[String, FuncSig], replaced: mutable.Set[String]) {

    private var n   = 0
    private val out = mutable.ListBuffer.empty[Inst]

    private def fresh(): Val = { n += 1; Val.Reg(s"bf16.soft.$n") }

    private def reg(dest: Option[Val]): Val = dest.getOrElse(fresh())

    def func(f: Func): Func =
      Func(sig(f.sig), f.blocks.map { b =>
        out.clear()
        b.instrs.foreach(inst)
        Block(b.label, out.toList, b.terminator.map(terminator))
      })

    private def terminator(i: Inst): Inst = i match
      case Inst.Ret(Some(t), v) => Inst.Ret(Some(ty(t)), v.map(value(_, t)))
      case other                => other

    // ---- the two conversions everything else is built from --------------------------------------

    /** A `bf16` (in the shape of `t`) as the binary32 it is the top half of — exact. */
    private def widen(v: Val, t: LType, dest: Option[Val] = None): Val = {
      val z = fresh(); out += Inst.Cast(z, CastOp.ZExt, as(t, I16), v, as(t, I32))
      val s = fresh(); out += Inst.Bin(s, BinOp.Shl, as(t, I32), z, k(as(t, I32), 16))
      val f = reg(dest); out += Inst.Cast(f, CastOp.BitCast, as(t, I32), s, as(t, F32))
      f
    }

    /** A binary32 rounded to the nearest `bf16`, ties to even. Adding `0x7fff` plus the lowest bit
     * that survives is the whole of round-to-nearest-even on the discarded half, and it carries into
     * the exponent exactly where the answer is the next binade or infinity. A NaN is the one input it
     * would get wrong — its payload could carry into the sign — so a NaN keeps its top bits and is
     * made quiet instead.
     */
    private def narrow(f: Val, t: LType, dest: Option[Val] = None): Val = {
      val w = as(t, I32)
      val u    = fresh(); out += Inst.Cast(u, CastOp.BitCast, as(t, F32), f, w)
      val hi   = fresh(); out += Inst.Bin(hi, BinOp.LShr, w, u, k(w, 16))
      val odd  = fresh(); out += Inst.Bin(odd, BinOp.And, w, hi, k(w, 1))
      val bias = fresh(); out += Inst.Bin(bias, BinOp.Add, w, odd, k(w, 0x7fff))
      val sum  = fresh(); out += Inst.Bin(sum, BinOp.Add, w, u, bias)
      val near = fresh(); out += Inst.Bin(near, BinOp.LShr, w, sum, k(w, 16))
      val nan  = fresh(); out += Inst.FloatCmp(nan, FCmp.Uno, as(t, F32), f, f)
      val quiet = fresh(); out += Inst.Bin(quiet, BinOp.Or, w, hi, k(w, 0x40))
      val pick = fresh(); out += Inst.Select(pick, nan, w, quiet, near, as(t, I1))
      val r = reg(dest); out += Inst.Cast(r, CastOp.Trunc, w, pick, as(t, I16))
      r
    }

    /** A binary32 rounded to odd: the truncation towards zero, with the lowest bit set when anything
     * was discarded. `rne` is the round-to-nearest answer and `away` says whether it rounded away
     * from zero, in which case the truncation is the representation one below — the bit patterns of
     * a sign-magnitude format count magnitudes, across binades and up to infinity alike.
     */
    private def toOdd(rne: Val, inexact: Val, away: Val, t: LType): Val = {
      val w = as(t, I32)
      val bits = fresh(); out += Inst.Cast(bits, CastOp.BitCast, as(t, F32), rne, w)
      val step = fresh(); out += Inst.Cast(step, CastOp.ZExt, as(t, I1), away, w)
      val down = fresh(); out += Inst.Bin(down, BinOp.Sub, w, bits, step)
      val jam  = fresh(); out += Inst.Bin(jam, BinOp.Or, w, down, k(w, 1))
      val pick = fresh(); out += Inst.Select(pick, inexact, w, jam, bits, as(t, I1))
      val r    = fresh(); out += Inst.Cast(r, CastOp.BitCast, w, pick, as(t, F32))
      r
    }

    /** A `real` to `bf16`, through binary32 rounded to odd so that only the last rounding counts. */
    private def fromReal(v: Val, from: LType, t: LType, dest: Option[Val]): Val = {
      val f    = fresh(); out += Inst.Cast(f, CastOp.FPTrunc, from, v, as(t, F32))
      val back = fresh(); out += Inst.Cast(back, CastOp.FPExt, as(t, F32), f, from)
      val lost = fresh(); out += Inst.FloatCmp(lost, FCmp.One, from, back, v)
      val gt   = fresh(); out += Inst.FloatCmp(gt, FCmp.Ogt, from, back, v)
      val neg  = fresh(); out += Inst.FloatCmp(neg, FCmp.Olt, from, v, kf(from, 0L))
      val away = fresh(); out += Inst.Bin(away, BinOp.Xor, as(t, I1), gt, neg)
      narrow(toOdd(f, lost, away, t), t, dest)
    }

    /** An integer to `bf16`. One that fits a binary32's significand converts exactly and is only
     * narrowed. A wider one keeps its top twenty-four bits with the rest jammed into the lowest —
     * rounding to odd, by hand — and is scaled back by the power of two it was shifted by. A
     * magnitude past `2^128` is past every `bf16` and is infinity.
     */
    private def fromInt(v: Val, from: LType, signed: Boolean, t: LType, dest: Option[Val]): Val = {
      val bits = from match
        case LType.Vec(_, LType.I(b)) => b
        case LType.I(b)               => b
        case other                    => sys.error(s"not an integer: ${other.render}")

      val op = if signed then CastOp.SIToFP else CastOp.UIToFP

      if bits <= 24 then
        val f = fresh(); out += Inst.Cast(f, op, from, v, as(t, F32))
        narrow(f, t, dest)
      else
        val c = as(t, I1)

        val (mag, neg) =
          if signed then
            val neg  = fresh(); out += Inst.IntCmp(neg, ICmp.Slt, from, v, k(from, 0))
            val flip = fresh(); out += Inst.Bin(flip, BinOp.Sub, from, k(from, 0), v)
            val mag  = fresh(); out += Inst.Select(mag, neg, from, flip, v, c)
            (mag, Some(neg))
          else (v, None)

        val ctlz = Llvm.bits("ctlz").at(from)
        added.getOrElseUpdate(ctlz, FuncSig(ctlz, FnType(from, List(Param(from), Param(I1)))))

        val lz     = fresh(); out += Inst.Call(Some(lz), from, Val.Global(ctlz), List(Arg(from, mag), Arg(I1, Val.Bool(false))))
        val length = fresh(); out += Inst.Bin(length, BinOp.Sub, from, k(from, bits), lz)
        val over   = fresh(); out += Inst.IntCmp(over, ICmp.Ugt, from, length, k(from, 24))
        val excess = fresh(); out += Inst.Bin(excess, BinOp.Sub, from, length, k(from, 24))
        val shift  = fresh(); out += Inst.Select(shift, over, from, excess, k(from, 0), c)
        val kept   = fresh(); out += Inst.Bin(kept, BinOp.LShr, from, mag, shift)
        val unit   = fresh(); out += Inst.Bin(unit, BinOp.Shl, from, k(from, 1), shift)
        val mask   = fresh(); out += Inst.Bin(mask, BinOp.Sub, from, unit, k(from, 1))
        val lost   = fresh(); out += Inst.Bin(lost, BinOp.And, from, mag, mask)
        val sticky = fresh(); out += Inst.IntCmp(sticky, ICmp.Ne, from, lost, k(from, 0))
        val stick  = fresh(); out += Inst.Cast(stick, CastOp.ZExt, c, sticky, from)
        val jammed = fresh(); out += Inst.Bin(jammed, BinOp.Or, from, kept, stick)

        val w = as(t, I32)

        def word(x: Val): Val =
          if bits == 32 then x
          else
            val r = fresh()
            out += Inst.Cast(r, if bits > 32 then CastOp.Trunc else CastOp.ZExt, from, x, w)
            r

        val top   = fresh(); out += Inst.Cast(top, CastOp.UIToFP, w, word(jammed), as(t, F32))
        val exp   = fresh(); out += Inst.Bin(exp, BinOp.Add, w, word(shift), k(w, 127))
        val field = fresh(); out += Inst.Bin(field, BinOp.Shl, w, exp, k(w, 23))
        val scale = fresh(); out += Inst.Cast(scale, CastOp.BitCast, w, field, as(t, F32))
        val prod  = fresh(); out += Inst.Bin(prod, BinOp.FMul, as(t, F32), top, scale)

        val ranged =
          if bits <= 128 then prod
          else
            val huge = fresh(); out += Inst.IntCmp(huge, ICmp.Ugt, from, length, k(from, 128))
            val r    = fresh(); out += Inst.Select(r, huge, as(t, F32), kf(as(t, F32), inf), prod, c)
            r

        val signedValue = neg match
          case Some(neg) =>
            val flipped = fresh(); out += Inst.Neg(flipped, as(t, F32), ranged)
            val r       = fresh(); out += Inst.Select(r, neg, as(t, F32), flipped, ranged, c)
            r
          case None => ranged

        narrow(signedValue, t, dest)
    }

    // ---- the instructions ---------------------------------------------------------------------

    private def inst(i: Inst): Unit = i match
      case Inst.Bin(d, op, t, a, b) if isBf(t) =>
        val r = fresh()
        out += Inst.Bin(r, op, as(t, F32), widen(value(a, t), t), widen(value(b, t), t))
        narrow(r, t, Some(d))

      // Negation is the sign bit, and nothing else: a NaN keeps its payload, which is what `fneg`
      // promises and what widening would also have kept.
      case Inst.Neg(d, t, v) if isBf(t) =>
        out += Inst.Bin(d, BinOp.Xor, ty(t), value(v, t), k(ty(t), -32768))

      case Inst.FloatCmp(d, p, t, a, b) if isBf(t) =>
        out += Inst.FloatCmp(d, p, as(t, F32), widen(value(a, t), t), widen(value(b, t), t))

      case Inst.Cast(d, op, from, v, to) if isBf(from) || isBf(to) => cast(d, op, from, value(v, from), to)

      case Inst.Call(d, ret, Val.Global(name), args, _, _, fast, _)
          if name.startsWith(Llvm.prefix) && (isBf(ret) || args.exists(a => isBf(a.ty))) =>
        intrinsic(d, ret, name, args, fast)

      case Inst.Call(d, ret, callee, args, retAttrs, calleeType, fast, tail) =>
        out += Inst.Call(d, ty(ret), callee, args.map(arg), retAttrs.map(attr), calleeType.map(fnType), fast, tail)

      case Inst.Alloca(d, t, align)           => out += Inst.Alloca(d, ty(t), align)
      case Inst.Load(d, t, p, acc, align)     => out += Inst.Load(d, ty(t), p, acc, align)
      case Inst.Store(t, v, p, acc, align)    => out += Inst.Store(ty(t), value(v, t), p, acc, align)
      case Inst.Gep(d, t, p, idx)             => out += Inst.Gep(d, ty(t), p, idx.map(arg))
      case Inst.Extract(d, t, agg, idx)       => out += Inst.Extract(d, ty(t), value(agg, t), idx)
      case Inst.Insert(d, t, agg, vt, v, idx) =>
        out += Inst.Insert(d, ty(t), value(agg, t), ty(vt), value(v, vt), idx)
      case Inst.Select(d, c, t, a, b, ct)     => out += Inst.Select(d, c, ty(t), value(a, t), value(b, t), ct)
      case Inst.InsertElement(d, t, vec, et, v, ix) =>
        out += Inst.InsertElement(d, ty(t), value(vec, t), ty(et), value(v, et), arg(ix))
      case Inst.ExtractElement(d, t, vec, ix) => out += Inst.ExtractElement(d, ty(t), value(vec, t), arg(ix))
      case Inst.Shuffle(d, t, a, b, mask)     => out += Inst.Shuffle(d, ty(t), value(a, t), value(b, t), arg(mask))
      case Inst.Asm(d, ret, text, cons, args) => out += Inst.Asm(d, ty(ret), text, cons, args.map(arg))
      case Inst.VaArg(d, list, t)             => out += Inst.VaArg(d, list, ty(t))
      case Inst.AtomicRmw(d, op, p, t, v, ord) => out += Inst.AtomicRmw(d, op, p, ty(t), value(v, t), ord)
      case Inst.CmpXchg(d, p, t, e, x, ord)   => out += Inst.CmpXchg(d, p, ty(t), value(e, t), value(x, t), ord)
      case other                              => out += other

    private def cast(d: Val, op: CastOp, from: LType, v: Val, to: LType): Unit = op match
      case CastOp.FPExt if as(to, F32) == to  => widen(v, from, Some(d))
      case CastOp.FPExt                       => out += Inst.Cast(d, CastOp.FPExt, as(from, F32), widen(v, from), to)
      case CastOp.FPTrunc if as(from, F32) == from => narrow(v, to, Some(d))
      case CastOp.FPTrunc                     => fromReal(v, from, to, Some(d))
      case CastOp.FPToSI | CastOp.FPToUI      => out += Inst.Cast(d, op, as(from, F32), widen(v, from), to)
      case CastOp.SIToFP                      => fromInt(v, from, signed = true, to, Some(d))
      case CastOp.UIToFP                      => fromInt(v, from, signed = false, to, Some(d))
      case _                                  => out += Inst.Cast(d, op, ty(from), v, ty(to))

    /** An intrinsic with a `bf16` in its signature, called at binary32 instead. Each of these gives
     * the same `bf16` computed that way as computed directly — they round once, or not at all — except
     * a sum, which rounds at every step and so is written out lane by lane.
     */
    private def intrinsic(d: Option[Val], ret: LType, name: String, args: List[Arg], fast: List[FastMath]): Unit = {
      replaced += name

      def declare(callee: String, r: LType, ps: List[LType]): Val.Global = {
        added.getOrElseUpdate(callee, FuncSig(callee, FnType(r, ps.map(Param(_)))))
        Val.Global(callee)
      }

      val saturating = List(true, false).map(Llvm.fptoiSat)
      val rounding   = List(Llvm.sqrt, Llvm.fabs, Llvm.floor, Llvm.ceil, Llvm.trunc, Llvm.round, Llvm.copysign)

      args match
        case List(a) if !isBf(ret) && saturating.exists(_.at(ret, a.ty) == name) =>
          val sat  = saturating.find(_.at(ret, a.ty) == name).get
          val wide = as(a.ty, F32)
          out += Inst.Call(d, ret, declare(sat.at(ret, wide), ret, List(wide)),
                           List(Arg(wide, widen(value(a.value, a.ty), a.ty))), fast = fast)

        case _ if isBf(ret) && args.forall(_.ty == ret) && rounding.exists(_.at(ret) == name) =>
          val op   = rounding.find(_.at(ret) == name).get
          val wide = as(ret, F32)
          val r    = fresh()
          out += Inst.Call(Some(r), wide, declare(op.at(wide), wide, args.map(_ => wide)),
                           args.map(a => Arg(wide, widen(value(a.value, a.ty), a.ty))), fast = fast)
          narrow(r, ret, d)

        case List(v @ Arg(LType.Vec(lanes, Bf), _, _)) if ret == Bf &&
            List("fmin", "fmax").exists(op => Llvm.reduce(op).at(v.ty) == name) =>
          val op   = List("fmin", "fmax").find(op => Llvm.reduce(op).at(v.ty) == name).get
          val wide = LType.Vec(lanes, F32)
          val r    = fresh()
          out += Inst.Call(Some(r), F32, declare(Llvm.reduce(op).at(wide), F32, List(wide)),
                           List(Arg(wide, widen(value(v.value, v.ty), v.ty))), fast = fast)
          narrow(r, Bf, d)

        case List(start, v @ Arg(LType.Vec(lanes, Bf), _, _)) if ret == Bf && Llvm.reduce("fadd").at(v.ty) == name =>
          val vec = value(v.value, v.ty)
          (0 until lanes).foldLeft(value(start.value, Bf)) { (acc, lane) =>
            val e = fresh(); out += Inst.ExtractElement(e, ty(v.ty), vec, Arg(I32, Val.Int(lane)))
            val s = fresh(); out += Inst.Bin(s, BinOp.FAdd, F32, widen(acc, Bf), widen(e, Bf))
            narrow(s, Bf, if lane == lanes - 1 then d else None)
          }
          ()

        case _ => sys.error(s"'$name' has no lowering for a bf16 carried as its bits")
    }
  }
}
