package sh.sysl

import scala.collection.mutable

import ir.{Access, Arg, BinOp, CastOp, Inst, LType, Val}

/** **What a `call` names between the keyword and the callee**, which is the three fields of
 * `ir.Inst.Call` that describe the callee's type rather than its arguments.
 *
 * It is a bundle rather than three parameters because three functions hand it along — the call seam
 * works out what a name refers to (`CallEmitter.calleeParts`) and passes it to whichever of the two
 * emitters lowers the call. Splitting it into three would put the same triple in six signatures and
 * make a caller that filled two of them and forgot the third representable.
 *
 * `whole` is set only for a variadic callee, and then it is what LLVM reads; `ret` and `retAttrs`
 * hold the same answer either way, so `call` below cannot produce the two disagreeing.
 */
case class CallForm(ret: ir.LType, retAttrs: List[ir.Attr] = Nil,
                    whole: Option[ir.FnType] = None) {

  def call(dest: Option[ir.Val], callee: ir.Val, args: List[ir.Arg]): ir.Inst =
    ir.Inst.Call(dest, ret, callee, args, retAttrs, whole)
}

/** The substrate every part of codegen emits into: registers, basic blocks, the entry-block
 * prologue, module-level globals, and the queue of runtime helpers.
 *
 * This trait holds *how* IR is written down and none of what it says. The lowering rules live
 * in the traits and the class mixed on top of it — `ArcEmitter` for reference counting,
 * `ScalarEmitter` for arithmetic and printing, `Codegen` for the tree walk itself.
 */
trait Emitter {

  /** The machine this module is being emitted for (`getting-started/cli.md § targets`). Little
   * consults it directly: what does is the handful of places where the C ABI genuinely differs,
   * each of which says so where it reads this, plus the two derived values below that every emitter
   * uses without naming a target at all.
   */
  protected def target: Target

  /** The two C symbols this module's storage comes from and goes back to (`reference/packages.md §
   * One heap, and the package that names it`).
   *
   * A program has one pair, settled from the packages it is built from before any of this runs, so
   * nothing below decides it — every emitter that allocates or releases reads it here rather than
   * spelling `malloc`. Defaulted to libc's, which is what every program got before a package could say
   * otherwise and what a `Codegen` constructed without one still gets.
   */
  protected def allocator: Allocator = Allocator.c

  /** The allocating half, as it is written in IR — `@malloc`, or whatever the packages named. */
  protected def mallocSym: String = allocator.alloc

  /** The releasing half, the same way. */
  protected def freeSym: String = allocator.free

  /** Given for the same reason `Word` is: the runtime helpers are text templates on the companion
   * objects, so a helper that allocates needs the pair in scope without every caller threading it.
   */
  protected given Allocator = allocator

  /** How wide an address is, given to every `Type.llvm` in scope. A view's length is a `usize`, so
   * an emitter cannot write the LLVM form of a slice without it — and because there is no default
   * anywhere, an emitter that somehow had no target would not compile rather than quietly writing
   * a 64-bit type for a 32-bit machine.
   */
  protected given Word = target.word

  /** What this machine's types cost, which is the same question with the same one answer in it.
   * A `given` because `CAbi` asks for one, and an emitter has exactly one to give.
   */
  protected given layout: Layout = Layout(target)

  /** The LLVM integer exactly one address wide — `i64` or `i32` — which is what a **length, a size,
   * an index, or anything else that is a `usize`** must be spelled as.
   *
   * Reach for this rather than writing `i64`, and the test that says whether you got it right is
   * `CrossTargetBuildTests`: a module mixing the two is text like any other and only clang will say
   * so. A loop counter internal to a helper may honestly be either, since nothing outside sees it —
   * but it is compared against a length often enough that using this everywhere is both simpler and
   * harder to get wrong.
   */
  protected def word: String = wordLty.render

  /** The same, as a type rather than as its text — what a converted emitter names a length, a size
   * or an index with.
   */
  protected def wordLty: ir.LType = target.word.lty

  /** A method table's shape as a **call site** has to spell it: the type's identity, then the slots
   * (`VtableEmitter`).
   *
   * The slot array is written with **no length**, which is what lets one type serve every trait: a
   * `getelementptr` needs the aggregate only to compute an offset, and `[0 x ptr]` and `[n x ptr]`
   * begin at the same one. A call site knows the trait's slot number and not how many slots the
   * table has — the table it reaches is chosen at run time — so a length here would be a number it
   * could not supply.
   */
  protected def vtableLty: ir.LType = ir.LType.Struct(List(wordLty, ir.LType.Arr(0, ir.LType.Ptr)))

  protected val globals  = mutable.ListBuffer.empty[ir.Global]
  private var strId      = 0

  /** The emitted name of every struct that asked for a boundary of its own, and the boundary
   * (`reference/types.md § Structs`). Consulted wherever storage is created, which is the only
   * place an alignment can be *said* — LLVM's textual form gives a named type no alignment, so
   * `@align` has to be stamped onto each alloca and global rather than declared once with the type.
   *
   * Keyed by the **emitted** name rather than by the sysl type, because what a slot has is an
   * `ir.LType` and a declared struct is a `Named` — the sysl type it came from is exactly what
   * lowering discarded, and asking a slot to carry it back would undo that for the handful of types
   * this applies to at all.
   */
  protected val raisedAligns = mutable.Map.empty[String, Int]
  protected var boolStrs = false
  protected var charBuf  = false
  protected var traps    = false

  /** Whether any string operation renders through `snprintf` — a float `str`, or an `f"…"` format.
   * The single declaration then lives in the module header rather than in each helper.
   */
  protected var usesSnprintf = false

  /** Whether any variadic body walked its tail, which is what the two `llvm.va_*` intrinsics are
   * declared for. `va_arg` is an instruction and needs no declaration of its own.
   */
  protected var usesVarargs = false

  /** Whether anything duplicated a walk, which `llvm.va_copy` is declared for. It is asked
   * separately because a function handed a `*va_list` may copy one without ever starting one.
   */
  protected var usesVaCopy = false

  /** Whether anything copied a block of storage — today only a walk handed to a foreign function on
   * a target that passes `va_list` indirectly (`VaListAbi.Copied`), which is why a module built for
   * one machine declares `llvm.memcpy` and the same module built for another does not.
   */
  protected var usesMemcpy = false

  /** Whether anything told the optimizer what a contract established, which is what `llvm.assume` is
   * declared for (`ContractEmitter.assumeEnsures`). A module whose callees declare no postcondition
   * — or none that survives `tryPure` — never names it.
   */
  protected var usesAssume = false

  /** LLVM intrinsic `declare` lines the module turned out to need — the saturating float-to-integer
   * casts, the checked arithmetic, and the bit operations `sysl.math`'s `Bits` lowers to. Each is
   * declared once under its overload-mangled name, which is why this is a set rather than a flag:
   * the name carries the width, so a program using one member at three widths needs three
   * declarations and a flag could not tell them apart.
   */
  protected val satDecls = mutable.LinkedHashSet.empty[ir.FuncSig]

  /** Box layouts to declare, keyed by their LLVM name and held in the order they were first
   * needed — a box's payload type is always declared before it.
   */
  protected val boxes = mutable.LinkedHashMap.empty[String, Type]

  /** Buffer layouts to declare, keyed the same way. A buffer carries its element count, since the
   * elements it holds are only reachable from the hook that destroys them.
   */
  protected val bufs = mutable.LinkedHashMap.empty[String, Type]

  /** Whether the module sizes an allocation from something it computed, and so needs the checked
   * arithmetic that keeps a count from wrapping into a buffer too small for it.
   */
  protected var checked = false

  /** Runtime helpers (retain, release, destructors) to emit, generated on demand. Generating one
   * may ask for another, so they are queued rather than emitted inline.
   */
  private val requested            = mutable.HashSet.empty[String]
  protected val runtimeQueue = mutable.Queue.empty[() => ir.Runtime]

  /** Which parts of the ARC runtime the module turned out to need: the heap at all, the atomic
   * pair a `&sync` uses, the null-tolerant pair a slice's owner needs, and the weak trio.
   *
   * The weak *header word* is not on this list, because every box carries it whether or not
   * anything weakly refers to one (`reference/memory.md § What a heap object costs`) — what is
   * optional is the three functions that read it, and only a program holding a `weak T` calls
   * those.
   */
  protected var heap      = false
  protected var syncHeap  = false
  protected var maybeHeap = false
  protected var weakHeap  = false

  // Per-function emission state, reset at each function boundary.
  //
  // The body is a list of **basic blocks** rather than a run of text (`ir.Func`). It always was one:
  // the three writers below are the only ways anything reaches it, and `terminated` already means
  // *this block is closed* — what changes is that the structure now survives being written down,
  // which is what a second back end has to read.
  //
  // `prologue` is kept apart because a stack slot is hoisted to the top of the entry block from
  // wherever it was needed, so the two halves of that block are built at different times and are
  // joined only when the function is finished.
  private var prologue   = mutable.ListBuffer.empty[ir.Inst]
  private var blocks     = mutable.ListBuffer.empty[ir.Block]
  private var current    = mutable.ListBuffer.empty[ir.Inst]
  private var currentEnd: Option[ir.Inst] = None
  private var currentLbl = "entry"
  private var temp       = 0
  private var label      = 0
  private var terminated = false
  private var scratch    = mutable.HashMap.empty[ir.LType, ir.Val]

  /** Every label an emitted terminator names, which is what `emitLabel` asks to decide whether the
   * block it is opening can be reached at all.
   */
  private var reached = mutable.HashSet.empty[String]

  /** References this expression owns and must let go of. The stack mirrors the regions a value
   * may not escape: a statement, and each branch of an `if` or arm of a `match`, release their
   * own before control leaves them, so every release site dominates what it releases.
   */
  protected var tempStack: List[mutable.ListBuffer[(ir.Val, Type)]] = Nil

  /** Named slots — parameters, locals, pattern bindings — that hold a reference of their own,
   * innermost scope first. Each holds one count, taken when the slot is written and given back
   * when the scope ends or the function returns.
   */
  protected var owned: List[mutable.ListBuffer[(ir.Val, Type)]] = Nil

  /** What each scope has been asked to run on its way out — the `defer` stack (`reference/memory.md
   * § Where defer sits`), innermost first. It sits beside `owned` because it is pushed, popped and
   * unwound with it and never on its own, so the two are always the same length and one index
   * reaches both.
   */
  protected var deferrals: List[mutable.ListBuffer[TStmt]] = Nil

  /** The enclosing loops, innermost first, so a `break`/`continue` knows where to jump, what
   * result slot to store into, and how far to unwind the ownership regions on the way out. The
   * depths are the sizes of `owned`/`tempStack` at loop entry, so leaving releases exactly what
   * the body accrued.
   */
  protected case class GenLoop(breakL: String, continueL: String, slot: ir.Val, resultTy: Type,
                               ownedDepth: Int, tempDepth: Int)
  protected var genLoops: List[GenLoop] = Nil

  /** Where a tail self-call jumps, and which calls are the ones that jump (`TailCalls`).
   *
   * `tailTarget` is the label sitting just after the parameter slots are written, so the jump lands
   * where a fresh call would have landed: the preconditions are checked again and each `old(e)` is
   * snapshotted again, because a tail call *is* a call and those belong to the invocation rather
   * than to the frame. It is `None` in a function with no tail call, which is what keeps a label
   * out of every other function in the module.
   *
   * `tailCalls` is compared by identity, since two calls written alike are equal case classes
   * standing in different positions — only one of which the walk may have named.
   */
  protected var tailTarget: Option[String] = None
  protected var tailCalls: List[TCall]     = Nil

  /** The parameters a jump rebinds, in declaration order — every one of them, so that they line up
   * with the arguments a call was written with. A zero-sized one has no slot to write and is
   * skipped where the writing happens, not here.
   */
  protected var tailParams: List[(String, Type)] = Nil

  /** Whether this call is the one the walk named, rather than merely equal to it. */
  protected def isTailCall(c: TCall): Boolean = tailCalls.exists(_ eq c)

  /** Folding `and` / `or` over `i1`s, with the constant cases collapsed rather than emitted — a
   * pattern test that cannot fail contributes `"true"`, and ANDing that in would be an instruction
   * saying nothing. Both halves of codegen build conditions this way, so they live here.
   */
  protected def andI1(a: ir.Val, b: ir.Val): ir.Val =
    if a == yes then b
    else if b == yes then a
    else { val r = freshReg(); emit(Inst.Bin(r, BinOp.And, i1, a, b)); r }

  protected def orI1(a: ir.Val, b: ir.Val): ir.Val =
    if a == yes || b == yes then yes
    else { val r = freshReg(); emit(Inst.Bin(r, BinOp.Or, i1, a, b)); r }

  /** Negating an `i1`, with the same constant folded away — what `is not` does to the test its
   * pattern produced.
   */
  protected def notI1(a: ir.Val): ir.Val =
    if a == yes then no
    else { val r = freshReg(); emit(Inst.Bin(r, BinOp.Xor, i1, a, Val.Bool(true))); r }

  /** The two `i1` constants, which the folding above tests for by identity rather than by text —
   * a condition that cannot fail is one of these, and there is no third spelling of it.
   */
  protected val yes: ir.Val = Val.Bool(true)
  protected val no: ir.Val  = Val.Bool(false)

  // --- bit ranges of a bitfield struct's container ---------------------------------------
  //
  // A bitfield struct is one integer and its fields are ranges of it (`Bitfields`), so the two
  // things every emitter needs are here rather than in each of them: lifting a range out of a
  // container, and putting one back. Both halves of codegen reach a field — the one that reads a
  // value and the one that writes through a place — which is the reason the `i1` folding above
  // lives here too.

  /** The ranges of the bitfield struct this type is one of, through whatever qualifies it, or
   * `None` — which is every other type.
   */
  protected def bitfieldOf(t: Type): Option[List[BitRange]] = Type.underlying(t) match
    case s: Type.Struct => Bitfields.of(s)
    case _              => None

  /** The range the field **written** `index`th occupies, or `None` where the receiver is not a
   * bitfield struct.
   *
   * The two indices are not the same number, which is the whole reason this is a method: the ranges
   * are cut from `stored`, and a zero-sized field is not stored. `struct { a: u3, u: unit, b: u5 }`
   * writes `b` third and stores it second.
   */
  protected def bitRange(t: Type, index: Int): Option[BitRange] = Type.underlying(t) match
    case s: Type.Struct => Bitfields.of(s).map(_(s.slot(index)))
    case _              => None

  /** The container type a bitfield struct's fields are ranges of. */
  protected def containerLty(ranges: List[BitRange]): ir.LType = ir.LType.I(Bitfields.bits(ranges))

  protected def containerLlvm(ranges: List[BitRange]): String = containerLty(ranges).render

  /** A field's value widened to the container and shifted to where it belongs — the half of a write
   * that is about the range, before anything is said about what was there already.
   */
  protected def placeBits(ranges: List[BitRange], r: BitRange, v: ir.Val): ir.Val = {
    val ct = containerLty(ranges)

    val wide =
      if r.width == Bitfields.bits(ranges) then v
      else
        val t = freshReg(); emit(Inst.Cast(t, CastOp.ZExt, LType.I(r.width), v, ct)); t

    if r.offset == 0 then wide
    else { val t = freshReg(); emit(Inst.Bin(t, BinOp.Shl, ct, wide, Val.Int(r.offset))); t }
  }

  /** A whole container built from one value per field, which is what constructing a bitfield struct
   * is. It ors the placed ranges together rather than folding `writeBits` over a zero, because there
   * is nothing to preserve: every bit of the result is being written.
   */
  protected def buildBits(ranges: List[BitRange], vals: List[ir.Val]): ir.Val = {
    val ct = containerLty(ranges)

    ranges.zip(vals).map(placeBits(ranges, _, _)).reduceOption { (a, b) =>
      val t = freshReg(); emit(Inst.Bin(t, BinOp.Or, ct, a, b)); t
    }.getOrElse(Val.Int(0))
  }

  /** One range lifted out of the container `c`.
   *
   * **No sign fixup is needed and none is emitted**, which is worth saying because a C bitfield
   * needs one: an `i5` field *is* an LLVM `i5`, so truncating the shifted container to the field's
   * own width lands the two's-complement value already in place. The signedness is in the type
   * rather than in the extraction.
   */
  protected def readBits(ranges: List[BitRange], r: BitRange, c: ir.Val): ir.Val = {
    val ct     = containerLty(ranges)
    val shifted =
      if r.offset == 0 then c
      else
        val t = freshReg(); emit(Inst.Bin(t, BinOp.LShr, ct, c, Val.Int(r.offset))); t

    if r.width == Bitfields.bits(ranges) then shifted
    else
      val t = freshReg(); emit(Inst.Cast(t, CastOp.Trunc, ct, shifted, LType.I(r.width))); t
  }

  /** The container `c` with one range replaced by `v` — the read-modify-write a bitfield write is,
   * with the read already done by the caller so that the whole of a multi-field update is one load
   * and one store rather than one of each per field.
   */
  protected def writeBits(ranges: List[BitRange], r: BitRange, c: ir.Val, v: ir.Val): ir.Val = {
    val ct         = containerLty(ranges)
    val bits       = Bitfields.bits(ranges)
    val (_, clear) = Bitfields.mask(r, bits)
    val shifted    = placeBits(ranges, r, v)

    // A field occupying the whole container has nothing to preserve, so the mask and the or would
    // be two instructions saying `shifted` — and the constant one of them ands with is zero, which
    // reads as a bug rather than as an identity.
    if r.width == bits then shifted
    else
      val cleared = freshReg(); emit(Inst.Bin(cleared, BinOp.And, ct, c, Val.Int(clear)))
      val merged  = freshReg(); emit(Inst.Bin(merged, BinOp.Or, ct, cleared, shifted))
      merged
  }

  // --- how a sysl signature is spelled --------------------------------------------------
  //
  // Six places write down what a sysl function looks like to LLVM — its definition, a declaration
  // of one linked in from a library, a call, the whole function type a variadic call has to name,
  // a method table's adapter, and the `ret` itself. They agree because they all ask here.

  /** The out-pointer a **large** result comes back through, in front of every declared parameter.
   *
   * A result that fits in registers is returned as itself, as it always was. One that does not is
   * written straight into storage the caller supplies, so it is never a first-class LLVM value at
   * either end — which is the whole of what `layout.indirect` buys. It is the same `sret` the
   * foreign boundary has always used (`ForeignEmitter`), asked for the same reason on a call that
   * happens to have this compiler on both sides.
   */
  protected def syslSret(retTy: Type): Option[ir.Param] =
    Option.when(layout.indirect(retTy))(
      ir.Param(LType.Ptr, ir.Attr.NoAlias :: sretAttrs(retTy.lty, layout.align(retTy))))

  /** The out-pointer's attributes, less the `noalias` only a caller that made the storage may
   * claim: what is at the far end, and the boundary it sits on. Written once here because a `sret`
   * is stated four times over — on a definition, on a declaration, at the call, and on the adapter
   * that forwards one — and the four have to agree.
   */
  protected def sretAttrs(ty: LType, align: Int): List[ir.Attr] =
    List(ir.Attr.SRet(ty), ir.Attr.Align(align))

  /** What a sysl `define`, `declare` and `call` name as the result type.
   *
   * A **narrow** result carries the C convention's extension (`CAbi.extension`), which is the one
   * thing about a sysl signature that is not sysl's own answer. Widening a result is the *callee's*
   * obligation, and a definition cannot know who calls it: `@export` and `&f` hand this very symbol
   * to C, and a `bool` returned without the attribute would reach it as whatever was in the top of
   * the register. Stating it everywhere costs a sysl caller nothing — it does not ask for the
   * extension and is not harmed by receiving one — and it means no part of the compiler has to work
   * out which definitions C can reach.
   *
   * A **parameter** gets none, which is the same rule seen from the other side: widening an argument
   * is the caller's obligation, so it is stated at a foreign call (`ForeignEmitter`) and nowhere
   * else. Neither end of a sysl-to-sysl call claims it, so neither may rely on it.
   */
  protected def syslResult(retTy: Type): CallForm =
    CallForm(syslResultLty(retTy),
             if syslResultLty(retTy) == LType.Void then Nil else CAbi.extension(retTy, target))

  /** The result's **type**, with no attribute on it — what a `ret` instruction carries.
   *
   * A return attribute belongs to the signature: `define zeroext i1 @f()` states what the function
   * guarantees, and the terminator inside it just names the value's type. LLVM refuses `ret zeroext
   * i1 %x` outright, which is a parse error in the emitted module rather than anything a test of the
   * compiler's own types would notice — so the two are kept apart here rather than at each of the
   * places that needs one.
   */
  protected def syslResultLty(retTy: Type): ir.LType =
    if Type.noValue(retTy) || layout.indirect(retTy) then ir.LType.Void else retTy.lty

  /** How a parameter is declared. A **large** one arrives as the address of storage the caller
   * holds; the callee makes its own copy at entry, which is the copy it always made — the only
   * difference is that the value crosses the boundary in memory rather than in registers.
   */
  protected def syslParamLty(ty: Type): ir.LType = if layout.indirect(ty) then ir.LType.Ptr else ty.lty

  /** The name the out-pointer takes inside a function that has one. */
  protected val sretParam = Val.Reg("sret.out")

  // --- hooks provided by the Codegen class ---------------------------------------------
  //
  // The recursive entry points live in the class that walks the tree; the emitting traits call
  // back into them.

  /** Lowers an expression, returning the register or immediate holding its value. */
  protected def genExpr(expr: TExpr): ir.Val

  /** Lowers an expression into the storage at `dest`, leaving the destination owning what lands
   * there — a count taken for every reference inside (`CallEmitter`).
   */
  protected def genOwnedInto(dest: ir.Val, e: TExpr): Unit

  /** The same, taking no counts: what lands at `dest` is borrowed, exactly as the register
   * `genExpr` hands back is (`CallEmitter`).
   */
  protected def genBorrowedInto(dest: ir.Val, e: TExpr): Unit

  /** Lowers one statement for its effects. */
  protected def genStmt(stmt: TStmt): Unit

  /** The value a compound assignment stores: what its operator makes of the place's current value
   * and the value on the right, whether that is an instruction or a trait method
   * (`reference/expressions.md § Operator dispatch`).
   */
  protected def combine(op: String, ty: Type, valueTy: Type, dispatch: Option[TDispatch],
                        cur: ir.Val, v: ir.Val): ir.Val

  /** Traps unless a struct's `invariant` clauses hold of the value in `v` (`reference/errors.md § Struct invariants`). */
  protected def emitInvCheck(v: ir.Val, struct: Type.Struct, invFn: String): Unit

  /** Traps unless `v` satisfies everything the constrained subtype `c` asks of its values — its
   * `within` range and its `where` predicate (`reference/errors.md § Where a constraint is
   * checked`).
   */
  protected def emitConstraintChecks(v: ir.Val, c: Type.Constrained): Unit

  protected def startFunction(): Unit = {
    prologue = mutable.ListBuffer.empty
    blocks = mutable.ListBuffer.empty
    current = mutable.ListBuffer.empty
    currentEnd = None
    currentLbl = "entry"
    temp = 0
    label = 0
    terminated = false
    reached = mutable.HashSet.empty
    tempStack = Nil
    owned = Nil
    deferrals = Nil
    genLoops = Nil
    tailTarget = None
    tailCalls = Nil
    tailParams = Nil
    scratch = mutable.HashMap.empty
    promoted = Set.empty
    promotedBoxes = mutable.HashMap.empty
    refStorage = mutable.HashMap.empty
    refPlaceOf = mutable.HashMap.empty
  }

  /** The **storage** type each `ref` in this body names (`reference/memory.md § ref — a name for a
   * place`), which is not the same as the type of the values that come out of it.
   *
   * A qualifier lives on storage rather than on a value (`reference/memory.md § Device memory`),
   * and the ordinary way to read one is off the place — a field knows its own declaration. A ref's
   * uses are all a plain name, which has no declaration to consult, so the qualifier it found at
   * the binding is kept here and put back at each access. Without it a ref to a register would emit
   * an unmarked load and store: the same access the direct path marks `volatile`, silently free to
   * be reordered or dropped.
   *
   * Per function, like every other name-keyed table here, since a name is unique only within one.
   */
  protected var refStorage: mutable.HashMap[String, Type] = mutable.HashMap.empty

  /** The place each `ref` in this body names, for the walks that have to reach past the name to the
   * storage's **root** rather than to its address — which the address alone cannot tell them.
   *
   * `promotedOwner` is the one that needs it: a view records the box it borrows from, and a ref that
   * names a promoted array has to hand over that array's box rather than the null a name with no
   * declaration would otherwise give.
   */
  protected var refPlaceOf: mutable.HashMap[String, TExpr] = mutable.HashMap.empty

  /** The local arrays this body allocates on the heap rather than in its frame, decided by the
   * escape analysis (`05`), and the buffer each one ended up in.
   *
   * A promoted array keeps its `[N]T` type and only its storage moves, so `%name.addr` is the
   * buffer's data pointer instead of an `alloca` and every index, store and copy is emitted exactly
   * as it was. What the box is needed for is the one thing that differs: a slice of the array names
   * it as its owner, where a frame-backed array has none.
   */
  protected var promoted: Set[String]                 = Set.empty

  /** The **temporaries** given storage of their own, by the position of the slice node that escapes
   * (`Escape.Promotions.temporaries`). Whole-program rather than per-function, since a position
   * identifies one node in one source file.
   */
  protected var promotedTemps: Set[Pos]               = Set.empty
  protected var promotedBoxes: mutable.HashMap[String, ir.Val] = mutable.HashMap.empty

  /** One stack slot per LLVM type per function, for the type punning a union needs: a value goes
   * in written as one type and comes back out read as another. Sharing the slot is safe because
   * every use of it stores and immediately loads with nothing emitted in between, and it is worth
   * doing because a function that matches on an enum in a hundred places would otherwise carry a
   * hundred slots it uses one at a time.
   */
  protected def scratchSlot(ty: ir.LType): ir.Val =
    scratch.getOrElseUpdate(ty, emitAlloca(freshReg(), ty))

  /** The address of the payload region inside an enum sitting at `base`. */
  protected def payloadPtr(en: Type.Enum, base: ir.Val): ir.Val = {
    val r = freshReg()
    emit(Inst.Gep(r, en.lty, base, List(Arg(i32, Val.Int(0)), Arg(i32, Val.Int(1)))))
    r
  }

  /** Reads a variant's payload out of an enum value, at that variant's type.
   *
   * A union is read by putting the whole value somewhere and loading the part back at the type the
   * variant that wrote it used, since an aggregate value has no operation that reinterprets part of
   * itself. Reading a variant the value is not holding yields whatever the one it *is* holding left
   * there — which is what a pattern test does before it knows the tag, and the tag test beside it is
   * what makes the answer count.
   */
  protected def enumPayload(en: Type.Enum, variant: Type.EnumVariant, value: ir.Val): ir.Val = {
    val slot = scratchSlot(en.lty)

    emit(Inst.Store(en.lty, value, slot, Access.Plain))

    val p = payloadPtr(en, slot)
    val r = freshReg()

    emit(Inst.Load(r, en.payloadLty(variant), p, Access.Plain))
    r
  }

  /** The function just emitted: its header, and the blocks it is made of.
   *
   * The hoisted slots go at the front of the entry block, which is where they were always printed —
   * `emitAlloca` writes one where the function begins rather than where it was needed, so that a
   * slot inside a loop does not grow the stack on every iteration.
   */
  protected def finishFunc(sig: ir.FuncSig): ir.Func = {
    closeBlock()
    val all = blocks.toList

    ir.Func(sig, all.head.copy(instrs = prologue.toList ::: all.head.instrs) :: all.tail)
  }

  /** The same, written down. */
  protected def finishFunction(sig: ir.FuncSig): String = ir.Printer.func(finishFunc(sig))

  /** Files the block being built and starts one under `l`. The entry block is the one this begins
   * with, so a function whose body never labels anything still finishes with a block rather than
   * with nothing.
   */
  private def closeBlock(): Unit = {
    // A block still marked closed at the moment it is filed never had a terminator of its own,
    // because everything emitted into it was dropped — it is unreachable code, and `unreachable`
    // is what it says. A block that is *not* closed is one the emitters left unfinished, which is
    // a defect in them rather than something to paper over here, so it is filed as it stands.
    val end = currentEnd.orElse(Option.when(terminated)(ir.Inst.Unreachable))

    blocks += ir.Block(currentLbl, current.toList, end)
    current = mutable.ListBuffer.empty
    currentEnd = None
  }

  /** A register nothing else in this function uses. */
  protected def freshReg(): Val.Reg = { temp += 1; Val.Reg(s"t$temp") }

  /** The two widths written often enough here to be worth naming: a condition, and the index a
   * `getelementptr` steps with.
   */
  protected val i1: LType  = LType.I(1)
  protected val i32: LType = LType.I(32)

  protected def freshLabel(s: String): String = { label += 1; s"$s$label" }

  /** Emits a plain instruction, unless the current block is already terminated. */
  protected def emit(inst: ir.Inst): Unit = if !terminated then current += inst

  /** Whether control can reach the point being emitted — false once a terminator has closed the
   * block, until a label something branches to opens the next. Asked where emitting a whole region
   * would be wasted rather than wrong, since `emit` already drops what lands in a closed block.
   */
  protected def controlReaches: Boolean = !terminated

  /** Emits a stack slot into the function's entry block rather than where it is needed.
   * Every name is unique within a function, so hoisting is safe — and it keeps a slot inside
   * a loop from growing the stack on every iteration.
   *
   * `align` is a boundary the **declaration** asked for (`@align(n)` on a `var` or `val`), and it
   * wins over the one the type carries. It cannot ask for less: `@align` only ever raises, so the
   * larger of the two is what satisfies both claims, and one written on the declaration is by that
   * rule already at or above the type's.
   *
   * **A named slot is laid down once however many times its declaration is emitted.** The one
   * declaration emitted more than once is a `@threaded` loop's head, copied to the foot of every arm
   * (`genThreadedBody`), and every copy is meant to write the same slot — the arms read it by name.
   * A fresh register never repeats, so only a program name is looked for.
   */
  protected def emitAlloca(name: ir.Val, ty: ir.LType, align: Option[Int] = None): ir.Val = {
    val named = name match
      case ir.Val.Reg(n) => !n.matches("t[0-9]+")
      case _             => false
    val laid = named && prologue.exists {
      case ir.Inst.Alloca(n, _, _) => n == name
      case _                       => false
    }
    if !laid then prologue += ir.Inst.Alloca(name, ty, align.orElse(raisedAlign(ty)))
    name
  }

  /** The boundary the type this storage holds asked for, and nothing where it did not — LLVM's own
   * choice is the natural alignment, which is right for everything that made no claim.
   *
   * **An aggregate is searched rather than only asked about**, which is what makes `[8 x
   * %struct.Frame]` land on `Frame`'s boundary: a region of aligned elements begins where its first
   * element must, and that is what makes an aligned type usable as a buffer and not only as a single
   * value. The first declared struct in the type's own written order is the one that answers, which
   * is where the search stops.
   */
  private def raisedAlign(ty: ir.LType): Option[Int] = ty match
    case ir.LType.Named(n)     => raisedAligns.get(n)
    case ir.LType.Arr(_, elem) => raisedAlign(elem)
    case ir.LType.Vec(_, elem) => raisedAlign(elem)
    case ir.LType.Struct(fs)   => fs.iterator.map(raisedAlign).collectFirst { case Some(n) => n }
    case _                     => None

  /** A block of storage copied, at a boundary both ends are known to satisfy.
   *
   * The one call in the module the intrinsic is declared for, so the flag that declares it is set
   * here rather than at each site that copies — a caller that stops copying stops declaring it, and
   * one that starts brings the declaration with it.
   */
  protected def emitMemcpy(dst: ir.Val, src: ir.Val, bytes: Int, align: Int): Unit = {
    usesMemcpy = true
    emit(ir.Inst.Call(None, ir.LType.Void, ir.Val.Global(Llvm.memcpyName),
                      List(ir.Arg(ir.LType.Ptr, dst, List(ir.Attr.Align(align))),
                           ir.Arg(ir.LType.Ptr, src, List(ir.Attr.Align(align))),
                           ir.Arg(ir.LType.I(64), ir.Val.Int(bytes)),
                           ir.Arg(ir.LType.I(1), ir.Val.Bool(false)))))
  }

  /** Emits a block terminator (`br` / `ret` / `unreachable`), marks the block closed, and records
   * wherever it sends control — which is what makes the labels it names reachable.
   */
  protected def emitTerm(inst: ir.Inst): Unit =
    if !terminated then {
      currentEnd = Some(inst)
      terminated = true
      inst match
        case ir.Inst.Br(l)               => reached += l
        case ir.Inst.CondBr(_, t, f)     => reached += t; reached += f
        case ir.Inst.Switch(_, _, d, cs) => reached += d; reached ++= cs.map(_._2)
        case _                           =>
    }

  /** Opens a block under `l`.
   *
   * **A label nothing has branched to is unreachable, and emission stays suppressed through it.**
   * LLVM has no fall-through — a block is entered only by a terminator naming it — so a label
   * opened while no emitted branch mentions it can be reached at most by a back edge from the
   * region it opens, which is unreachable from the entry by the same argument.
   *
   * Without this the suppression that `emit` performs stops at the first label, and dead code is
   * emitted in pieces: the instructions in the closed block are dropped while the blocks *after*
   * it survive, still naming the registers that went with them. That is not a dead-code
   * inefficiency but invalid IR — clang rejects the module with `use of undefined value` — and it
   * needs both a divergence and a value whose lowering opens a block of its own, which is why it
   * survived so long. A slice repeat (`[0; 0]`) after an `exit` is the shape that found it.
   */
  protected def emitLabel(l: String): Unit = {
    closeBlock()
    currentLbl = l
    terminated = !reached.contains(l)
  }

  /** Emits `body`, and keeps what it emitted **only if every instruction of it is one the program
   * could equally not have run** — no call, no store, no allocation, no branch, no trap. On anything
   * else the emitted text is taken back out and the answer is `None`, leaving the function exactly as
   * it was down to the register counter.
   *
   * **This exists because a contract clause is ordinary sysl, and a clause read somewhere other than
   * where it was written may not be allowed to do anything** (`ContractEmitter.assumeEnsures`). A
   * postcondition may short-circuit an `and`, which opens a block; it may subscript, which traps; it
   * may build a temporary, which allocates and releases. None of those may happen a second time at a
   * caller, whose only business with the clause is to repeat a fact the callee has already checked.
   *
   * **The test is on the instructions rather than on the node kinds, which is what keeps it honest.**
   * A whitelist of the tree shapes that are safe is a second copy of what every lowering does, and it
   * goes stale the first time one of them grows a check — silently, and in the direction that emits an
   * `assume` for something the callee never established. What came out is the thing to read.
   */
  protected def tryPure[A](body: => A): Option[A] = {
    val hoisted = prologue.length
    val filed   = blocks.length
    val here    = current.toList
    val end     = currentEnd
    val lbl     = currentLbl
    val closed  = terminated
    val edges   = reached.toSet
    val regs    = temp
    val labels  = label
    val out     = body

    if !closed && !terminated && blocks.length == filed &&
      prologue.length == hoisted && current.view.drop(here.length).forall(effectFree)
    then Some(out)
    else {
      prologue.remove(hoisted, prologue.length - hoisted)
      blocks.remove(filed, blocks.length - filed)
      current.clear()
      current ++= here
      currentEnd = end
      currentLbl = lbl
      terminated = closed
      reached.clear()
      reached ++= edges
      temp = regs
      label = labels
      None
    }
  }

  /** Whether an instruction computes a value and does nothing else — what `tryPure` keeps.
   *
   * **Integer division and remainder are excluded although LLVM calls them side-effect free**,
   * because a zero divisor is undefined behaviour: emitting one at a call site introduces a way for
   * the program to be wrong that the source never wrote. Every other arithmetic instruction sysl
   * emits is total at every operand.
   */
  private def effectFree(inst: ir.Inst): Boolean = inst match
    case ir.Inst.Bin(_, op, _, _, _) =>
      op != ir.BinOp.SDiv && op != ir.BinOp.UDiv && op != ir.BinOp.SRem && op != ir.BinOp.URem
    case _: ir.Inst.Neg | _: ir.Inst.IntCmp | _: ir.Inst.FloatCmp | _: ir.Inst.Cast |
        _: ir.Inst.Load | _: ir.Inst.Gep | _: ir.Inst.Extract | _: ir.Inst.Insert |
        _: ir.Inst.Select | _: ir.Inst.ExtractElement | _: ir.Inst.InsertElement |
        _: ir.Inst.Shuffle =>
      true
    case _ => false

  /** A runtime helper's signature: `private`, `void`, and parameters named as the body reads them.
   *
   * Every helper the ownership runtime writes has this shape, which is not a coincidence — a helper
   * is reached only from emitted code, so nothing outside may name it, and it works through the
   * addresses it is handed rather than answering with anything.
   */
  protected def helperSig(name: String, params: (String, ir.LType)*): ir.FuncSig =
    ir.FuncSig(name,
               ir.FnType(LType.Void,
                         params.map((n, t) => ir.Param(t, name = Some(Val.Reg(n)))).toList),
               ir.Linkage.Private)

  /** Generates one whole function while another is in progress, which is how a runtime helper
   * gets written at the moment it is first asked for.
   */
  protected def inFunction(sig: ir.FuncSig)(gen: => Unit): ir.Func = {
    val saved =
      (prologue, temp, label, terminated, tempStack, owned, scratch, promoted, promotedBoxes, deferrals)
    // The blocks under construction are saved with the rest: a helper is a whole function emitted in
    // the middle of one, so what the interrupted function had built has to be there when it resumes.
    val savedBlocks = (blocks, current, currentEnd, currentLbl, reached)
    // A helper is emitted in the middle of whatever asked for it, and the asking function may be one
    // with a jump in it — so what says where that jump goes is put back too. Without this the helper's
    // reset would leave the interrupted function with no target and its remaining tail calls would be
    // emitted as ordinary ones, which is a miscompile only a body long enough to need a helper shows.
    val savedTail = (tailTarget, tailCalls, tailParams)

    startFunction()
    gen
    val built = finishFunc(sig)

    prologue = saved._1; temp = saved._2; label = saved._3
    terminated = saved._4; tempStack = saved._5; owned = saved._6; scratch = saved._7
    promoted = saved._8; promotedBoxes = saved._9; deferrals = saved._10
    blocks = savedBlocks._1; current = savedBlocks._2
    currentEnd = savedBlocks._3; currentLbl = savedBlocks._4; reached = savedBlocks._5
    tailTarget = savedTail._1; tailCalls = savedTail._2; tailParams = savedTail._3
    built
  }

  /** Queues a runtime helper for emission, once per name, and hands back the name — which is what
   * a caller wants, since it asked in order to call the thing.
   *
   * **Generating one may ask for another**, so it is a queue rather than a recursion, and the
   * generator is by-name all the way down: building the value eagerly here would emit a helper in
   * the middle of whatever asked for it.
   */
  private def enqueue(name: String)(gen: => ir.Runtime): String = {
    if requested.add(name) then runtimeQueue.enqueue(() => gen)
    name
  }

  /** A helper the emitters **generate**, which is therefore data like every other function. */
  protected def requestFunction(name: String)(sig: ir.FuncSig)(gen: => Unit): String =
    enqueue(name)(ir.Runtime.Emitted(inFunction(sig)(gen)))

  /** A helper written out by hand as LLVM, which is text and can only be **named** for anything
   * that is not LLVM (`ir.Runtime`).
   */
  protected def requestText(name: String)(gen: => String): String =
    enqueue(name)(ir.Runtime.Template(name, gen))

  // --- string interning ------------------------------------------------------------------

  protected def stringGlobal(s: String): ir.Val.Global = {
    strId += 1
    val name  = s".str$strId"
    val bytes = encode(s)

    globals += ir.Global(name, constant = true, ir.LType.Arr(bytes.length, ir.LType.I(8)),
                         Some(ir.Val.Bytes(bytes)))
    ir.Val.Global(name)
  }

  /** A string's bytes as an interned constant holds them: UTF-8, with the terminator a C caller
   * reads by — which a `string`, knowing its own length, has never had a use for.
   */
  private def encode(s: String): List[Byte] = s.getBytes("UTF-8").toList :+ 0.toByte
}
