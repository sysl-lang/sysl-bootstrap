package sh.sysl

import ir.{Arg, BinOp, CastOp, FCmp, ICmp, Inst, LType, Val}

/** The scalar end of codegen: arithmetic, comparison, conversion, and printing.
 *
 * Everything here follows from `01-scalar-types-and-operators.md` — arithmetic wraps at the
 * operand's declared width and never promotes, ordering distinguishes signed from unsigned,
 * and every conversion between types is one the programmer wrote.
 *
 * The two operations that are not scalar at all — comparing and printing a `string` — are
 * dispatched from here because that is where an operator and a `print` argument arrive, but
 * what they do with the bytes belongs to `StringEmitter`.
 */
trait ScalarEmitter extends StringEmitter {

  /** Arithmetic wraps at the operand's declared width, which plain LLVM integer instructions
   * already do — the width is in the type, so no masking is needed even for an odd one.
   * Signedness picks between the division, remainder, and right-shift pairs.
   */
  protected def arith(op: String, ty: Type, lv: Val, rv: Val): Val = {
    // **A vector picks its instruction from the lane and emits at the whole register.** LLVM spells
    // both widths of the word with one mnemonic — `fadd <4 x float>` is the same opcode as `fadd
    // float` — so lane-wise arithmetic needs no operation of its own, only the type it is written
    // at. Integer `/` and `%` are the one pair that would need more than this, because their guards
    // below reduce a lane-wise comparison to a single `i1`; the analyzer refuses those on a vector,
    // so nothing reaches here needing a guard it cannot build.
    val lane = ty match
      case Type.Vector(_, elem) => Type.underlying(elem)
      case other                => other

    val instr = lane match
      case i: Type.Integer =>
        op match
          case "+"  => BinOp.Add
          case "-"  => BinOp.Sub
          case "*"  => BinOp.Mul
          case "/"  => if i.signed then BinOp.SDiv else BinOp.UDiv
          case "%"  => if i.signed then BinOp.SRem else BinOp.URem
          case "<<" => BinOp.Shl
          case ">>" => if i.signed then BinOp.AShr else BinOp.LShr
          case "&"  => BinOp.And
          case "|"  => BinOp.Or
          case "^"  => BinOp.Xor
          case _    => sys.error(s"unreachable arith '$op'")
      // A mask's lane is an `i1`, and the three bitwise instructions are the same ones an integer
      // takes — which is why this arrives here at all rather than needing a path of its own. Only a
      // *vector* of `bool` reaches it: a scalar `bool` has `&&` and `||`, which are control flow and
      // are lowered somewhere else entirely.
      case Type.Bool =>
        op match
          case "&" => BinOp.And
          case "|" => BinOp.Or
          case "^" => BinOp.Xor
          case _   => sys.error(s"unreachable mask arith '$op'")
      case _: Type.Floating =>
        op match
          case "+" => BinOp.FAdd
          case "-" => BinOp.FSub
          case "*" => BinOp.FMul
          case "/" => BinOp.FDiv
          case _   => sys.error(s"unreachable arith '$op'")
      case other => sys.error(s"unreachable arith on ${other.llvm}")

    // Integer division and remainder have two inputs LLVM leaves undefined and would miscompile: a
    // zero divisor always, and — for signed operands — INT_MIN by -1, whose true quotient overflows
    // the width. A zero divisor traps; signed `/` traps on that overflow; signed `%` returns its
    // defined zero by dividing by 1 in the one case -1 would make the remainder undefined, since
    // `a % -1` is 0 for every `a`. None of this touches raw multiplication or addition, which wrap.
    var divisor = rv
    ty match
      case i: Type.Integer if op == "/" || op == "%" =>
        val nz = freshReg(); emit(Inst.IntCmp(nz, ICmp.Ne, ty.lty, rv, Val.Int(0)))
        trapUnless(nz, "div")
        if i.signed then
          val neg1 = freshReg(); emit(Inst.IntCmp(neg1, ICmp.Eq, ty.lty, rv, Val.Int(-1)))
          if op == "/" then
            val min   = -(BigInt(2).pow(i.bits - 1))
            val isMin = freshReg(); emit(Inst.IntCmp(isMin, ICmp.Eq, ty.lty, lv, Val.Int(min)))
            val ovf   = freshReg(); emit(Inst.Bin(ovf, BinOp.And, i1, isMin, neg1))
            val ok    = freshReg(); emit(Inst.Bin(ok, BinOp.Xor, i1, ovf, Val.Bool(true)))
            trapUnless(ok, "overflow")
          else
            val safe = freshReg()

            emit(Inst.Select(safe, neg1, ty.lty, Val.Int(1), rv))
            divisor = safe
      case _ =>

    // A shift amount at or past the operand's width is undefined on every machine instruction this
    // lowers to, and LLVM propagates that as poison rather than as a number — so an over-shift did
    // not merely give a wrong answer, it gave a *different* one between two runs of one binary.
    // `01`'s rule is that raw integer arithmetic is total and defined, so the amount is bounded here
    // and the shift is defined at every amount. See `boundedShift`.
    lane match
      case i: Type.Integer if op == "<<" || op == ">>" =>
        boundedShift(ty, i, instr, lv, divisor)
      case _ =>
        val r = freshReg(); emit(Inst.Bin(r, instr, ty.lty, lv, divisor)); r
  }

  /** A shift whose amount may be anything, lowered so that every amount means something.
   *
   * **Shifting a value by its own width or more answers what shifting it all the way answers**: zero
   * for `<<` and for an unsigned `>>`, and the sign for a signed one, since an arithmetic right
   * shift fills from the top and a value shifted past its width is all sign. That is Go's rule and
   * Swift's, and it is the only one under which `x >> (w - n)` is total at `n == 0` — which is what
   * a routine reversing the low `n` bits of a word needs, and what `Bits.rotate_left` exists to work
   * around for rotation.
   *
   * **It is not the machine's rule and not C's.** The bare instruction on x86 and on AArch64 masks
   * the amount, so `x >> 64` would be `x >> 0`, which is `x` — an answer nobody means and the one
   * Java shipped. Masking is available to a caller who wants the raw instruction, under a name, if
   * one ever asks; it is not what an operator should do silently.
   *
   * The amount is clamped rather than the shift being emitted and thrown away, so **no poison is
   * built at all**: at `width - 1` the arithmetic shift is already the sign fill, which is why the
   * signed case needs nothing further. The two logical shifts then select their zero over the
   * clamped result. On a constant amount every instruction here folds away; on a variable one it is
   * a compare and a select.
   */
  protected def boundedShift(ty: Type, lane: Type.Integer, instr: BinOp, lv: Val, amt: Val): Val = {
    val bits    = lane.bits
    val condTy  = ty match
      case Type.Vector(n, _) => LType.Vec(n, i1)
      case _                 => i1
    val width   = constantAt(ty, bits)
    val highest = constantAt(ty, bits - 1)

    val tooBig = freshReg(); emit(Inst.IntCmp(tooBig, ICmp.Uge, ty.lty, amt, width))
    val safe   = freshReg(); emit(Inst.Select(safe, tooBig, ty.lty, highest, amt, condTy))
    val r      = freshReg(); emit(Inst.Bin(r, instr, ty.lty, lv, safe))

    // An arithmetic right shift by `width - 1` is already every bit of the sign, so the clamp alone
    // is the whole of the signed case. The other two have to say zero.
    if instr == BinOp.AShr then r
    else
      val out = freshReg(); emit(Inst.Select(out, tooBig, ty.lty, Val.Zero, r, condTy)); out
  }

  /** An integer constant at a type, splat across the lanes where that type is a vector.
   *
   * A scalar constant carries no width — the instruction names it — so a vector needs the value put
   * in lane zero and shuffled out, which is the same idiom `TSplat` emits and is recognised by every
   * back end.
   */
  protected def constantAt(ty: Type, value: BigInt): Val = ty match
    case v: Type.Vector =>
      val one = freshReg()
      emit(Inst.InsertElement(one, v.lty, Val.Poison, Type.underlying(v.elem).lty, Val.Int(value),
        Arg(i32, Val.Int(0))))
      val r = freshReg()
      emit(Inst.Shuffle(r, v.lty, one, Val.Poison, Arg(LType.Vec(v.length, i32), Val.Zero)))
      r
    case _ => Val.Int(value)

  /** Integer `+`, `-`, `*` where the operands' declared ranges allow a result the base width cannot
   * hold, so a plain instruction would wrap before the range check at the produce site could see it.
   * The overflow-detecting intrinsic traps on a representation overflow, so what the range check then
   * examines is the true result. Raw integer arithmetic is defined to wrap and never reaches here;
   * a range narrow enough that its results always fit the width stays on the plain path above.
   */
  protected def checkedArith(op: String, ty: Type.Integer, lv: Val, rv: Val): Val = {
    val name = op match
      case "+" => "add"
      case "-" => "sub"
      case "*" => "mul"
      case _   => sys.error(s"unreachable checkedArith '$op'")
    val fn = Llvm.withOverflow(name, ty.signed).at(ty.lty)

    // The intrinsic hands back the value and the overflow flag together, in an aggregate LLVM does
    // not name: `{ i32, i1 }`, which the declaration and the three uses below all take from here.
    val both = LType.Struct(List(ty.lty, i1))

    satDecls += ir.FuncSig(fn, ir.FnType(both, List(ir.Param(ty.lty), ir.Param(ty.lty))))

    val pair = freshReg()

    emit(Inst.Call(Some(pair), both, Val.Global(fn), List(Arg(ty.lty, lv), Arg(ty.lty, rv))))
    val v   = freshReg(); emit(Inst.Extract(v, both, pair, List(0)))
    val ovf = freshReg(); emit(Inst.Extract(ovf, both, pair, List(1)))
    val ok  = freshReg(); emit(Inst.Bin(ok, BinOp.Xor, i1, ovf, Val.Bool(true)))
    trapUnless(ok, "overflow")
    v
  }

  /** A left shift of a value produced through a ranged type. There is no overflow intrinsic for a
   * shift, so the check is direct: the amount must be below the width — the machine shift is
   * undefined at or above it — and shifting the result back must recover the value, since a bit
   * pushed out of the top is exactly the overflow. Raw shifts, the hot bit-manipulation path, are
   * left to wrap and never reach here.
   */
  protected def checkedShl(ty: Type.Integer, lv: Val, sh: Val): Val = {
    val amtOk = freshReg(); emit(Inst.IntCmp(amtOk, ICmp.Ult, ty.lty, sh, Val.Int(ty.bits)))
    trapUnless(amtOk, "overflow")

    val r    = freshReg(); emit(Inst.Bin(r, BinOp.Shl, ty.lty, lv, sh))
    val back = freshReg()

    emit(Inst.Bin(back, if ty.signed then BinOp.AShr else BinOp.LShr, ty.lty, r, sh))
    val ok = freshReg(); emit(Inst.IntCmp(ok, ICmp.Eq, ty.lty, back, lv))
    trapUnless(ok, "overflow")
    r
  }

  /** A shift's count, brought to the shifted value's own width.
   *
   * The count may be any integer type — it is a count rather than a value being combined with the
   * left, and `Literals.arithType` says so — while LLVM's `shl`, `lshr` and `ashr` each take two
   * operands of one type. So the conversion happens here rather than in a cast the reader had to
   * write for the compiler's benefit.
   *
   * A **narrower** count is extended, which preserves its value whichever way it is signed. A
   * **wider** one is clamped at the shifted width *before* it is truncated, and that order is the
   * whole of the care this needs: truncating first turns a count of 256 into a count of 0, so
   * `x << n` would answer `x` where every other over-shift answers zero. Clamping at the count's own
   * width and then truncating cannot, since the clamp value is the shifted width and always fits.
   *
   * A vector arrives unchanged: its count is lane-wise and is already the same register type, which
   * is why `arithType` does not relax the rule there.
   */
  protected def shiftAmount(ty: Type, amtTy: Type, amt: Val): Val =
    (Type.underlying(ty), Type.underlying(amtTy)) match
      case (to: Type.Integer, from: Type.Integer) if to.bits != from.bits =>
        if from.bits < to.bits then convert(from, to, amt)
        else
          val width  = Val.Int(to.bits)
          val tooBig = freshReg(); emit(Inst.IntCmp(tooBig, ICmp.Uge, from.lty, amt, width))
          val safe   = freshReg(); emit(Inst.Select(safe, tooBig, from.lty, width, amt))
          convert(from, to, safe)
      case _ => amt

  /** The `icmp` / `fcmp` predicate for an operator at a type. `char` compares by scalar
   * value, so it uses the unsigned predicates over its `i32` representation.
   */
  protected def predicate(op: String, ty: Type): String = ty match
    case _: Type.Floating => floatPred(op).render
    case other            => intPred(op, other).render

  /** The `icmp` predicate for an operator at a type. `char` compares by scalar value, so it uses the
   * unsigned predicates over its `i32` representation.
   */
  protected def intPred(op: String, ty: Type): ICmp = ty match
    // Equality only: a bool and an address have no ordering, so no signed/unsigned choice.
    case Type.Bool | _: Type.Ptr | _: Type.Ref | _: Type.CFn =>
      op match
        case "==" => ICmp.Eq; case "!=" => ICmp.Ne
        case _    => sys.error(s"unreachable compare '$op'")
    case Type.Char | Type.Integer(_, false, _) =>
      op match
        case "==" => ICmp.Eq;  case "!=" => ICmp.Ne
        case "<"  => ICmp.Ult; case ">"  => ICmp.Ugt
        case "<=" => ICmp.Ule; case ">=" => ICmp.Uge
        case _    => sys.error(s"unreachable compare '$op'")
    case _: Type.Integer =>
      op match
        case "==" => ICmp.Eq;  case "!=" => ICmp.Ne
        case "<"  => ICmp.Slt; case ">"  => ICmp.Sgt
        case "<=" => ICmp.Sle; case ">=" => ICmp.Sge
        case _    => sys.error(s"unreachable compare '$op'")
    case other => sys.error(s"unreachable compare on ${other.llvm}")

  /** The `fcmp` predicate. IEEE 754 makes `!=` the negation of `==`, and a `NaN` is equal to nothing
   * including itself — so `!=` is **unordered** or not equal (`une`), not ordered and not equal
   * (`one`). Every other float comparison is the ordered one, which is what makes all four of `==`,
   * `<`, `<=` and `>=` false at a `NaN` while `!=` is true.
   */
  protected def floatPred(op: String): FCmp = op match
    case "==" => FCmp.Oeq; case "!=" => FCmp.Une
    case "<"  => FCmp.Olt; case ">"  => FCmp.Ogt
    case "<=" => FCmp.Ole; case ">=" => FCmp.Oge
    case _    => sys.error(s"unreachable compare '$op'")

  protected def compareValue(op: String, base: Type, av: Val, bv: Val): Val = {
    // A constrained subtype is laid out as the type it narrows (`reference/errors.md § Constrained
    // types`), so it is compared as that one — its values are that type's values, and the range it
    // was declared with is checked where it is *produced* rather than where two of them are
    // ordered. Done here rather than at each caller because every caller wants it: an ordinary `n <
    // 6` arrives already reduced, and a pattern's test does not, which is what left `n match 1..6`
    // reaching a signedness question asked of a type that has no answer to it. A **simple** enum is
    // its discriminant — `Type.Enum.llvm` delegates to the storage integer, so the value in hand is
    // already one — and equality on it is that integer's compare. Read here for the same reason a
    // constrained subtype is: every caller wants it, and the signedness question has no answer
    // asked of the enum itself.
    val ty = Type.underlying(base) match
      case e: Type.Enum if e.simple => e.underlying
      case other                    => other

    // Two strings are ordered by their bytes, which is a call rather than an instruction; every
    // operator then reads the same -1 / 0 / 1 the way it would read a subtraction.
    if ty == Type.Str then
      val c = strCmp(av, bv)
      val r = freshReg()
      emit(Inst.IntCmp(r, intPred(op, Type.Int), i32, c, Val.Int(0)))
      r
    else
      val r = freshReg()

      ty match
        case _: Type.Floating => emit(Inst.FloatCmp(r, floatPred(op), ty.lty, av, bv))
        case _                => emit(Inst.IntCmp(r, intPred(op, ty), ty.lty, av, bv))

      r
  }

  // --- conversions ---------------------------------------------------------------------

  /** Lowers an explicit scalar conversion. Every case is a single LLVM cast, except the
   * partial `char(u)` — the one conversion that can fail, and so the one that checks.
   */
  protected def convert(from: Type, to: Type, v: Val): Val = (from, to) match
    case _ if from == to => v

    case (a: Type.Integer, b: Type.Integer) =>
      if b.bits == a.bits then v
      else if b.bits < a.bits then castOp(CastOp.Trunc, a, b, v)
      else castOp(if a.signed then CastOp.SExt else CastOp.ZExt, a, b, v)

    case (a: Type.Integer, b: Type.Floating)  => castOp(if a.signed then CastOp.SIToFP else CastOp.UIToFP, a, b, v)
    case (a: Type.Floating, b: Type.Integer)  => saturatingCast(a, b, v)
    // `f16` and `bf16` are the one pair of floating types neither of which is wider than the other,
    // and LLVM has no instruction that turns one into the other — `fptrunc` and `fpext` both
    // require the destination to differ in width. The conversion is a widening of one and a
    // narrowing of the other with `float` in between, which holds both exactly: binary32 has
    // binary16's precision and bfloat16's range, so the intermediate rounds nothing and the only
    // rounding is the one the destination was always going to do.
    case (a: Type.Floating, b: Type.Floating) if a.bits == b.bits =>
      convert(Type.Floating(32), b, convert(a, Type.Floating(32), v))

    case (a: Type.Floating, b: Type.Floating) => castOp(if b.bits > a.bits then CastOp.FPExt else CastOp.FPTrunc, a, b, v)

    case (Type.Char, b: Type.Integer) => convert(Type.Integer(32, signed = false), b, v)
    case (a: Type.Integer, Type.Char) => checkedChar(a, v)

    // A simple enum is stored at its underlying integer width, so converting it to another
    // integer is just that integer conversion; every enum value is already in range.
    case (e: Type.Enum, b: Type.Integer) => convert(e.underlying, b, v)

    // The raw tier (`reference/memory.md § Reinterpreting storage`). Two pointee types are the same
    // `ptr` under opaque pointers, so reading one as the other is nothing at all at this level —
    // which is exactly the claim the language is making about it.
    case (_: Type.Ptr, _: Type.Ptr)      => v
    case (a: Type.Ptr, b: Type.Integer)  => castOp(CastOp.PtrToInt, a, b, v)
    case (a: Type.Integer, b: Type.Ptr)  => castOp(CastOp.IntToPtr, a, b, v)

    // An address of code and an address of bytes are the same word, which is what makes `dlsym`
    // usable in one direction and a `*u8` callback table usable in the other (`reference/ffi.md § A
    // function's address`).
    case (_: Type.Ptr, _: Type.CFn)      => v
    case (_: Type.CFn, _: Type.Ptr)      => v
    case (_: Type.CFn, _: Type.CFn)      => v
    case (a: Type.CFn, b: Type.Integer)  => castOp(CastOp.PtrToInt, a, b, v)
    case (a: Type.Integer, b: Type.CFn)  => castOp(CastOp.IntToPtr, a, b, v)

    case _ => sys.error(s"unreachable conversion from ${from.llvm} to ${to.llvm}")

  private def castOp(instr: CastOp, from: Type, to: Type, v: Val): Val = {
    val r = freshReg(); emit(Inst.Cast(r, instr, from.lty, v, to.lty)); r
  }

  /** Float-to-integer, saturating. A plain `fptosi`/`fptoui` is poison when the source is out of
   * the target's range or is NaN, and what the hardware then does differs by target — so the same
   * program would print different numbers on different machines. The `llvm.fpto{s,u}i.sat`
   * intrinsics pin it down everywhere: out of range clamps to the type's minimum or maximum, and
   * NaN becomes zero. `int()` stays total; `char()` remains the one conversion that traps.
   */
  private def saturatingCast(from: Type.Floating, to: Type.Integer, v: Val): Val = {
    val name = Llvm.fptoiSat(to.signed).at(to.lty, from.lty)
    satDecls += ir.FuncSig(name, ir.FnType(to.lty, List(ir.Param(from.lty))))
    val r = freshReg()
    emit(Inst.Call(Some(r), to.lty, Val.Global(name), List(Arg(from.lty, v))))
    r
  }

  /** `char(u)` — a checked conversion. A Unicode scalar value is at most `0x10FFFF` and never
   * a surrogate; anything else traps, in the same runtime-safety category as a bounds check.
   * The test runs at 64 bits so a wide source cannot smuggle a value past it.
   */
  private def checkedChar(from: Type.Integer, v: Val): Val = {
    val wide     = convert(from, Type.Integer(64, from.signed), v)
    val i64      = LType.I(64)
    val inRange  = freshReg(); emit(Inst.IntCmp(inRange, ICmp.Ule, i64, wide, Val.Int(1114111)))
    val belowLow = freshReg(); emit(Inst.IntCmp(belowLow, ICmp.Ult, i64, wide, Val.Int(55296)))
    val aboveTop = freshReg(); emit(Inst.IntCmp(aboveTop, ICmp.Ugt, i64, wide, Val.Int(57343)))
    val scalar   = freshReg(); emit(Inst.Bin(scalar, BinOp.Or, i1, belowLow, aboveTop))
    val ok       = freshReg(); emit(Inst.Bin(ok, BinOp.And, i1, inRange, scalar))

    trapUnless(ok, "char")
    castOp(CastOp.Trunc, Type.Integer(64, from.signed), Type.Char, wide)
  }

  /** Traps unless a condition holds. This is the shape every runtime check takes: a compare, a
   * branch, `llvm.trap`, and an unreachable arm, with the checked path falling through.
   */
  protected def trapUnless(ok: Val, what: String): Unit = {
    traps = true

    val okL  = freshLabel(s"$what.ok")
    val badL = freshLabel(s"$what.bad")

    emitTerm(Inst.CondBr(ok, okL, badL))
    emitLabel(badL)
    emit(Inst.Call(None, LType.Void, Val.Global(Llvm.trap.name), Nil))
    emitTerm(Inst.Unreachable)
    emitLabel(okL)
  }

  /** `str(x)` — a value's string form. A `string` is returned unchanged, since it already is one;
   * every other type is rendered into a fresh owning buffer, so the result is an owned temporary
   * the enclosing statement releases. A `bool` renders to one of two immortal literals and needs
   * no allocation at all.
   */
  protected def genStr(arg: TExpr): Val = Type.underlying(arg.ty) match
    case Type.Str =>
      // Identity: the same value, its count already the argument's to manage.
      genExpr(arg)

    case Type.Bool =>
      boolStrs = true
      val v   = genExpr(arg)
      val ptr = freshReg()
      val len = freshReg()

      emit(Inst.Select(ptr, v, LType.Ptr, Val.Global(".true"), Val.Global(".false")))
      emit(Inst.Select(len, v, wordLty, Val.Int(4), Val.Int(5)))
      strView(Val.Null, ptr, len)

    case Type.Char =>
      charBuf = true
      heap = true
      requestText("sysl.str.from_bytes")(StringEmitter.fromBytes)
      val fn = requestText("sysl.str.char")(StringEmitter.char)
      val cp = genExpr(arg)
      val r  = freshReg()
      emit(Inst.Call(Some(r), Type.Str.lty, Val.Global(fn), List(Arg(i32, cp))))
      r

    // Rendered at a width that holds the value, which for anything past 64 bits is **the value's
    // own**. Up to 64 it goes through the renderer every program has always used — one instance,
    // shared, and the overwhelmingly common case.
    //
    // **Beyond that the width may not be clamped**, which is what this did while 128 was the
    // ceiling and every wider value was unreachable. Rendering an `i256` through a 128-bit renderer
    // would not be a lossy answer, it would be the wrong number: the conversion below narrows first
    // and the digits printed are of the truncated value. A renderer is generated per width instead,
    // and `StringEmitter.intName` gives each its own symbol.
    case i: Type.Integer =>
      heap = true
      requestText("sysl.str.from_bytes")(StringEmitter.fromBytes)
      val bits   = if i.bits > 64 then i.bits else 64
      val fn     = requestText(StringEmitter.intName(bits))(StringEmitter.int(bits))
      val wide   = convert(i, Type.Integer(bits, i.signed), genExpr(arg))
      val r = freshReg()

      emit(Inst.Call(Some(r), Type.Str.lty, Val.Global(fn),
                     List(Arg(LType.I(bits), wide), Arg(i1, Val.Int(if i.signed then 1 else 0)))))
      r

    case f: Type.Floating =>
      heap = true
      usesSnprintf = true
      val fn = requestText("sysl.str.float")(StringEmitter.float)
      val v  = convert(f, Type.Real, genExpr(arg))
      val r  = freshReg()
      emit(Inst.Call(Some(r), Type.Str.lty, Val.Global(fn), List(Arg(LType.F(64), v))))
      r

    case other => sys.error(s"unreachable str of ${other.llvm}")

  /** `format(x, spec)` — one value rendered through a printf specifier, into a fresh owning string.
   * A numeric value is widened and handed to `snprintf` with the C form of the specifier; a string
   * is copied NUL-terminated and handed to `snprintf` as a `%s`, so width, precision, and
   * justification are C's to apply. The result is always a fresh buffer this statement owns.
   */
  protected def genFormat(arg: TExpr, spec: String): Val = {
    heap = true
    usesSnprintf = true

    val c   = FormatSpec.conversion(spec)
    val fmt = stringGlobal(FormatSpec.cFormat(spec))
    val r   = freshReg()

    def call(fn: String, rest: List[Arg]): Unit =
      emit(Inst.Call(Some(r), Type.Str.lty, Val.Global(fn), Arg(LType.Ptr, fmt) :: rest))

    if FormatSpec.isStr(c) then
      val fn     = requestText("sysl.str.fmt_s")(StringEmitter.fmtStr)
      val (p, n) = strBytes(genExpr(arg))
      call(fn, List(Arg(LType.Ptr, p), Arg(wordLty, n)))
    else if FormatSpec.isFloat(c) then
      val fn = requestText("sysl.str.fmt_f")(StringEmitter.fmtFloat)
      val v  = convert(Type.underlying(arg.ty).asInstanceOf[Type.Floating], Type.Real, genExpr(arg))
      call(fn, List(Arg(LType.F(64), v)))
    else
      // A signed conversion widens by the value's own signedness, so a decimal keeps its value; an
      // unsigned one reads the bits as unsigned, so `%x` shows exactly the value's own width. Both
      // end at 64 bits and print through a `%ll…`.
      val fn = requestText("sysl.str.fmt_i")(StringEmitter.fmtInt)
      val i  = Type.underlying(arg.ty).asInstanceOf[Type.Integer]
      val v =
        if FormatSpec.isSignedInt(c) then convert(i, Type.Integer(64, i.signed), genExpr(arg))
        else
          val unsigned = convert(i, Type.Integer(i.bits, signed = false), genExpr(arg))
          convert(Type.Integer(i.bits, signed = false), Type.Integer(64, signed = false), unsigned)
      call(fn, List(Arg(LType.I(64), v)))

    r
  }

  /** Builds a string value from its three words. */
  private def strView(owner: Val, ptr: Val, len: Val): Val = {
    val str = Type.Str.lty
    val v0  = freshReg(); emit(Inst.Insert(v0, str, Val.Undef, LType.Ptr, owner, List(0)))
    val v1  = freshReg(); emit(Inst.Insert(v1, str, v0, LType.Ptr, ptr, List(1)))
    val v2  = freshReg(); emit(Inst.Insert(v2, str, v1, wordLty, len, List(2)))
    v2
  }
}

object ScalarEmitter {

  /** Encodes one Unicode scalar value as NUL-terminated UTF-8 into a caller-supplied
   * five-byte buffer, so printing a `char` is an ordinary `%s` argument alongside the rest.
   * Emitted only into modules that print one.
   */
  val utf8Encoder: String =
    """define private ptr @sysl.utf8(i32 %cp, ptr %buf) {
      |entry:
      |  %ascii = icmp ult i32 %cp, 128
      |  br i1 %ascii, label %one, label %wide
      |one:
      |  %a0 = trunc i32 %cp to i8
      |  store i8 %a0, ptr %buf
      |  %a1 = getelementptr i8, ptr %buf, i32 1
      |  store i8 0, ptr %a1
      |  ret ptr %buf
      |wide:
      |  %short = icmp ult i32 %cp, 2048
      |  br i1 %short, label %two, label %wider
      |two:
      |  %b0 = lshr i32 %cp, 6
      |  %b1 = or i32 %b0, 192
      |  %b2 = trunc i32 %b1 to i8
      |  store i8 %b2, ptr %buf
      |  %b3 = and i32 %cp, 63
      |  %b4 = or i32 %b3, 128
      |  %b5 = trunc i32 %b4 to i8
      |  %b6 = getelementptr i8, ptr %buf, i32 1
      |  store i8 %b5, ptr %b6
      |  %b7 = getelementptr i8, ptr %buf, i32 2
      |  store i8 0, ptr %b7
      |  ret ptr %buf
      |wider:
      |  %bmp = icmp ult i32 %cp, 65536
      |  br i1 %bmp, label %three, label %four
      |three:
      |  %c0 = lshr i32 %cp, 12
      |  %c1 = or i32 %c0, 224
      |  %c2 = trunc i32 %c1 to i8
      |  store i8 %c2, ptr %buf
      |  %c3 = lshr i32 %cp, 6
      |  %c4 = and i32 %c3, 63
      |  %c5 = or i32 %c4, 128
      |  %c6 = trunc i32 %c5 to i8
      |  %c7 = getelementptr i8, ptr %buf, i32 1
      |  store i8 %c6, ptr %c7
      |  %c8 = and i32 %cp, 63
      |  %c9 = or i32 %c8, 128
      |  %c10 = trunc i32 %c9 to i8
      |  %c11 = getelementptr i8, ptr %buf, i32 2
      |  store i8 %c10, ptr %c11
      |  %c12 = getelementptr i8, ptr %buf, i32 3
      |  store i8 0, ptr %c12
      |  ret ptr %buf
      |four:
      |  %d0 = lshr i32 %cp, 18
      |  %d1 = or i32 %d0, 240
      |  %d2 = trunc i32 %d1 to i8
      |  store i8 %d2, ptr %buf
      |  %d3 = lshr i32 %cp, 12
      |  %d4 = and i32 %d3, 63
      |  %d5 = or i32 %d4, 128
      |  %d6 = trunc i32 %d5 to i8
      |  %d7 = getelementptr i8, ptr %buf, i32 1
      |  store i8 %d6, ptr %d7
      |  %d8 = lshr i32 %cp, 6
      |  %d9 = and i32 %d8, 63
      |  %d10 = or i32 %d9, 128
      |  %d11 = trunc i32 %d10 to i8
      |  %d12 = getelementptr i8, ptr %buf, i32 2
      |  store i8 %d11, ptr %d12
      |  %d13 = and i32 %cp, 63
      |  %d14 = or i32 %d13, 128
      |  %d15 = trunc i32 %d14 to i8
      |  %d16 = getelementptr i8, ptr %buf, i32 3
      |  store i8 %d15, ptr %d16
      |  %d17 = getelementptr i8, ptr %buf, i32 4
      |  store i8 0, ptr %d17
      |  ret ptr %buf
      |}
      |
      |""".stripMargin
}
