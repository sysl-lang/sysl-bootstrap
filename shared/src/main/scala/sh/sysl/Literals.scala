package sh.sysl

/** The scalar leaves of expression analysis: how a literal takes its type, how operands that
 * must share a type reconcile a bare literal with a typed neighbour, explicit conversions, and
 * the result type of an arithmetic or bitwise operator. Nothing here widens, narrows, or
 * promotes on its own — every representation change is written by the programmer.
 */
trait Literals extends TypeResolution {

  /** The scalar type a context wants, seen through a vector: a position asking for `<4>f32` wants
   * `f32` out of a bare literal, which is then splatted into every lane.
   *
   * **This is what keeps `a * 2.0` free of a suffix and free of a construction**, and it is the same
   * reading `repr` already performs for a transparent subtype — the position's type is not what the
   * literal denotes, and both cases exist because a literal has no type of its own to defend. Without
   * it a float literal under a `<4>f32` falls all the way to `real` and is then refused for being
   * one, which reads as a missing conversion rather than as a lane count nobody wrote.
   */
  private def scalarWanted(t: Type): Type = Type.repr(t) match
    case Type.Vector(_, lane) => Type.repr(lane)
    case other                => other

  /** An integer literal takes its type from its suffix, else from the type the context
   * expects, else `int`. The default is never magnitude-dependent: a value too large for the
   * type it landed in is an error asking for a suffix, not a silent widening.
   *
   * **The expected type is consulted through `repr`**, which is the same identity `disagree` uses,
   * and the two have to be the same one or a literal is refused for having exactly the type the
   * position asked for. A qualifier says how a place is reached and never what is in it, so the value
   * a literal denotes is not `volatile` even where the field it is about to fill is: `Gpio(10, 0)`
   * against `input: volatile uint` read the expected type as no integer at all, fell back to `int`,
   * and was refused with *"'input' of 'Gpio' is volatile uint, but int was given"* — while
   * `regs.input = 10` had always been fine, because assignment reaches the field's type through
   * something that strips. A transparent constrained subtype is the same shape and was refused the
   * same way whenever its base was not `int`.
   */
  protected def intLiteral(value: BigInt, suffix: Option[String], expected: Option[Type]): TExpr = {
    val ty = suffix match
      case Some(s) =>
        scalarType(s) match
          case Some(i: Type.Integer) => i
          case _                     => err(s"'$s' is not an integer type")
      case None =>
        expected.map(scalarWanted) match
          case Some(i: Type.Integer) => i
          // A type **parameter**, during the definition-time pass of `reference/generics.md §
          // Bounds`. It is opaque, so there is no width to check against and no representation to
          // pick — but it is still the type the literal has, since what the instantiation makes of
          // `T` is what the literal is read as when the body is lowered. Taking `int` instead would
          // make the `1` in a `[T: Sub]` body's `x - 1` ask for `Sub[int]` where what the body
          // means is `T`'s own subtraction.
          case Some(a: Type.Abstract) => a
          case _                      => Type.Int

    ty match
      case i: Type.Integer if !Type.fits(value, i) => err(s"the literal $value does not fit ${show(i)}")
      case _                                       =>

    TIntLit(value, ty)
  }

  /** A float literal takes its type from its suffix, else from the expected type when that is
   * a float, else `real`. An integer literal never becomes a float on its own — writing `1`
   * where a `real` is wanted is a type error, not a silent conversion.
   *
   * Read through `repr` for the reason `intLiteral` above gives: a `volatile f32` field is a place,
   * and the value going into it is an `f32`.
   */
  protected def floatLiteral(text: String, suffix: Option[String], expected: Option[Type]): TExpr = {
    val ty = suffix match
      case Some(s) =>
        scalarType(s) match
          case Some(f: Type.Floating) => f
          case _                      => err(s"'$s' is not a floating-point type")
      case None =>
        expected.map(scalarWanted) match
          case Some(f: Type.Floating) => f
          case Some(a: Type.Abstract) => a
          case _                      => Type.Real

    TFloatLit(java.lang.Double.doubleToLongBits(text.toDouble), ty)
  }

  /** Analyzes operands that must share one type. A bare literal has no type of its own, so it
   * takes the type of a non-literal neighbour — which is what lets `n + 1` work for an `n` of
   * any width without the literal needing a suffix, and `p == null` work for any `*T`. The operands
   * that have a type are analyzed first precisely so it is available to the ones that do not.
   *
   * What it takes is the neighbour's **representation**, which for a transparent subtype is its
   * base: the operator it is about to be an operand of is the base's, so the literal is a base
   * value and has no range to satisfy. Reading `120` in `t + 120` as a `Temp` would refuse it for
   * not being a temperature when what has to be one is the sum — and the sum is checked where it is
   * stored. A derived subtype is its own representation, so a literal still may not stand beside
   * one without the cast `reference/errors.md § new is what makes it a type` asks for.
   *
   * **There are three tiers rather than two, and the middle one is `typedByPosition`.** A form whose
   * type the position supplies is not a literal — it may be a whole expression, and what it is owed
   * is a shape rather than a width — but it stands where one stands: it has nothing to offer the
   * operands beside it until something has told it what it is. So it is analyzed after everything
   * that knows its own type and before everything that knows none, which is what lets
   * `xs.load(i) * by` read its lane count off `by` while `out.store(i, xs.load(i))` stays refused —
   * nothing outside that load ever says a width there, and guessing one is the thing a run of memory
   * must never do.
   *
   * **The order is why this is a tier and not another case of `isLiteral`.** A *declared* member of
   * one of those names — `sysl.sync.Atomic.load`, which the builtin stands aside for — does have a
   * type of its own, and putting it in the same tier as the literals would leave `a.load(i) + 1` with
   * nothing in it that knows anything: the literal would fall to `int` where the member answers a
   * `u8`. In the middle tier it is still asked before the literals are, so it goes on supplying the
   * type it always did.
   */
  /** A last reading of an expression at the type its position turned out to have, taken only where
   * the reading it already has is about to be reported as a mismatch.
   *
   * The tiers above cover what the answer changes *outright* — a bare literal, an array standing
   * where a slice was asked for — and they are unconditional, because the node they replace is a
   * stand-in that must not reach the output. This covers the rest of the same shape: anything whose
   * type came out of an analysis with no expected type and would have come out differently had
   * there been one.
   *
   * `Some(3)` is the case that found it, at both of the two places a position is settled late. As an
   * argument at a `T` the other arguments solved to `Option[usize]` it reads as an `Option[int]`
   * alone, and `Option[int]` does not coerce to `Option[usize]`; as the right operand of an `==`
   * whose left is an `Option[usize]` it reads the same way and for the same reason. Either way the
   * call or the comparison is refused for a difference the reader never wrote.
   *
   * **Conditional on the disagreement, unlike the tiers above, and deliberately so.** A re-reading
   * that succeeds can only turn a refusal into what the same expression at a *written* type would
   * already have been; one that fails changes nothing and leaves the original complaint to whoever
   * was going to report it. So this cannot alter anything that resolves today, which is what makes
   * it safe to offer every remaining expression rather than a list of shapes somebody has to keep.
   */
  protected def reread(t: TExpr, src: Option[Expr], want: Type): TExpr =
    if !disagree(t.ty, want) then t
    else src.flatMap(e => attempt(analyzeExpr(e, Some(want)))).filterNot(r => disagree(r.ty, want)).getOrElse(t)

  /** Which of the operands the pair takes its type from: the first that stands on its own, and only
   * failing that the first that was read at all.
   *
   * **An adaptable operand is passed over rather than dropped**, which is the whole of the
   * difference between this and taking the head. `Some(3)` has a type — it is not a literal, so the
   * top tier reads it — and that type is worth exactly as much as the `int` inside it, which is to
   * say nothing when the operand beside it says `Option[usize]`. Taking it anyway settled the pair
   * on a type nobody wrote and then re-read the operand that was right, so `Some(3) == s` was
   * refused while `s == Some(3)` compiled.
   */
  private def firmest(operands: List[Expr], read: List[Option[TExpr]]): Option[Type] = {
    val pairs = operands.zip(read).collect { case (e, Some(t)) => (e, t) }
    val taken = pairs.find((e, t) => !adaptable(e, t)).orElse(pairs.headOption).map((_, t) => Type.repr(t.ty))

    taken.map(meetViews(pairs.map(_._2.ty)))
  }

  /** The two views of one slice, met at the read-only one — the only place a pair of operands has a
   * type to settle on that neither of them wrote.
   *
   * Everywhere else `firmest` takes an operand's type as it stands, and it can, because sysl has no
   * subtyping among concrete types to widen towards. A `[]T` and a `[]const T` are the exception the
   * language already made: they are one type with a bit, and the writable one is accepted wherever
   * the read-only one is wanted (`reference/arrays.md § []const T`). So the pair has a meeting point,
   * and taking the first operand's type instead made **which order it was written in** decide whether
   * it compiled — `c == a[..]` yes, `a[..] == c` no, for the same two values.
   *
   * The read-only view is the meeting point rather than a choice between them: it is the one either
   * side converts to, and it is the one a comparison needs, since comparing reads.
   */
  private def meetViews(types: List[Type])(taken: Type): Type = taken match
    case Type.Slice(elem, false) if types.contains(Type.Slice(elem, readOnly = true)) =>
      Type.Slice(elem, readOnly = true)
    case other => other

  /** Whether a value block's value is one of the forms that has **no type of its own** — the two
   * tiers `analyzeOperands` reads last, applied to a branch of an `if` or an arm of a `match`
   * instead of to an operand.
   *
   * A branch is a block, so what is asked about is the expression it ends in: `then 1` is a `1` and
   * nothing else, and a `1` is worth what the position it stands in says it is worth. Everything
   * else — a name, a call, a field, an arithmetic expression over any of them — knows what it is
   * before anything asks, and is therefore what the pair takes its type *from*.
   *
   * **The tiering is the existing one and that is the whole argument for it.** `n + 1` needs no
   * suffix, `p == null` works for any `*T`, `0..<xs.len` is a `usize` range: in every one of those
   * a bare literal beside something that knows takes what it is told. A branch is the same shape —
   * one of two positions that must agree on one type — and the only reason it did not read that way
   * is that a block is analyzed before anything compares it to its sibling.
   */
  protected def guessing(stmts: List[Stmt]): Boolean = stmts.lastOption match
    case Some(ExprStmt(e)) => isLiteral(e) || typedByPosition(e)
    case _                 => false

  /** What a branch or an arm **settles** for whichever of its siblings is guessing, or `None` where
   * it settles nothing.
   *
   * The three it declines are the three that are not an answer: `never` is a branch that does not
   * finish and constrains nothing, `unit` is a branch used for its effect — one of those makes the
   * whole form a statement — and `unknown` is a mistake already reported, which must not be handed
   * on to become a second complaint about a consequence.
   */
  protected def settles(t: TBlock): Option[Type] =
    Some(t.ty).filterNot(ty => ty == Type.Never || ty == Type.Unit || ty == Type.Unknown)

  protected def analyzeOperands(operands: List[Expr], expected: Option[Type]): List[TExpr] = {
    // **The top tier is allowed to come back empty**, which is what lets an operand with no type of
    // its own fall to the tier below rather than raising from the tier that has nothing to offer
    // it. `n == None` is the case: `None` is not a literal and not a load, so it is asked first —
    // and asking it alone is asking what an `Option` of nothing in particular holds. Held over, it
    // is read against what the *other* operand settled, which is the whole of what a comparison's
    // second side needs and exactly what `reference/expressions.md § Closures`'s held-back argument
    // already gets at a call.
    val own     = operands.map(e =>
      if isLiteral(e) || typedByPosition(e) then None else attempt(analyzeExpr(e, expected)))
    val settled = firmest(operands, own).orElse(expected)
    val told    = operands.zip(own).map((e, t) => t.orElse(Option.when(!isLiteral(e))(analyzeExpr(e, settled))))
    val ty      = firmest(operands, told).orElse(expected)

    // And the same last reading the tiers above earn: an operand whose type came out of the top
    // tier, before the other side had said anything, is read again at what the pair settled to.
    operands.zip(told).map {
      case (e, Some(t)) => ty.fold(t)(reread(t, Some(e), _))
      case (e, None)    => analyzeExpr(e, ty)
    }
  }

  /** Whether an expression is one whose type the position it sits in supplies, rather than one it
   * carries itself — the middle tier above.
   *
   * `xs.load(i)` and the implicit member `.red` are the set. The first is here for the reason
   * `MethodCalls`' `vectorMemory` gives: a slice has whatever length it has, so how many lanes a
   * run of it is read as is the *receiving* type's to say, and there is nothing in the receiver to
   * read it from.
   *
   * **The test is on the spelling, and it is allowed to be**, because the middle tier is not a
   * licence to skip anything — it moves an operand one place later in the order and hands it the
   * neighbour's type as the position's, which is what an expected type is for. So a receiver
   * declaring a `load` of its own is unharmed by matching here, while the alternative — analyzing
   * every operand speculatively to find out which of them can stand alone — would put a sandbox
   * around every binary operator in the language.
   */
  protected def typedByPosition(e: Expr): Boolean = e match
    case Call(Field(_, "load"), List(_)) => true
    // A **payload-free variant of a generic enum** — `None` above all. It is not a literal and it is
    // not a load, and until 0.0.82 it fell through both tiers to be analyzed alone, which is asking
    // what an `Option` of nothing in particular holds: the refusal was *"cannot infer the type
    // argument 'T' of 'None'"* at a site whose other branch said `Some(parse_expr(p))` (card `0294`
    // filed it from slate). It belongs here for precisely the reason `.red` does — the form has no
    // type at all until something says which type is wanted, and a sibling is something.
    //
    // **A variant that CARRIES something is deliberately not here.** `Some(3)` knows what it is
    // before anything asks, so it is what the pair takes its type *from* — read by the top tier and
    // then, where its argument was itself adaptable, re-read at what the pair settled. That is
    // `adaptable`'s job and it already worked; putting a carrying variant in this tier would take a
    // type away from the branch that had one.
    case Ident(name) if payloadFreeGenericVariant(name) => true
    // `.red` is the other one, and it is the reason the tier is worth having rather than a special
    // case: `c == .Red` is what a reader writes first, and the form has no type at all until
    // something says which type is wanted. The neighbour is what says it here, exactly as it says
    // what width a literal is.
    case _: ImplicitMember               => true
    case Call(_: ImplicitMember, _)      => true
    case _                               => false

  /** Whether a bare name is a variant that carries nothing, of an enum that takes type arguments.
   *
   * Both halves are needed and each rules out a case that must not be in the tier above. A variant
   * of a **non-generic** enum has a type the moment it is named — `Colour.Red` is a `Colour` — so it
   * is an answer rather than a question. One that **carries** something derives its arguments from
   * what it was given, which is what makes `Some(3)` a type its sibling can be read against.
   *
   * Asked with `variantKey`, so a spelling that reaches no variant from here is simply not one.
   */
  protected def payloadFreeGenericVariant(name: String): Boolean =
    variantKey(name).exists { key =>
      variantOwners.getOrElse(key, Nil).exists { own =>
        enumDecls.get(own).exists(d =>
          d.tparams.nonEmpty &&
            d.variants.find(v => LlvmName.guard(v.name) == Modules.split(key)._2).exists(_.fields.isEmpty))
      }
    }

  /** Whether an expression is a literal with no type of its own. A suffixed numeric literal
   * has already said what it is, so it counts as fixed rather than adaptable.
   */
  protected def isLiteral(e: Expr): Boolean = e match
    case IntLit(_, None) | FloatLit(_, None) => true
    case NullLit()                           => true
    case Unary("-", operand)                 => isLiteral(operand)
    case _                                   => false

  /** Whether an argument or operand is **adaptable**: the tier a bare literal is in, widened to the
   * things built out of nothing but bare literals.
   *
   * `Some(3)` is the case this exists for. It is not a literal — it is a call, and `isLiteral` is
   * syntactic — so it went in the first round and fixed `T = Option[int]`, a conclusion worth
   * exactly what the `int` inside it was worth and no more. The operand or argument that actually
   * knew was then the one re-read, and re-reading an `Option[usize]` at an `Option[int]` cannot
   * succeed. So `same(s, Some(3))` compiled and `same(Some(3), s)` did not, for a difference the
   * reader never wrote (card `0200`).
   *
   * **Both halves are needed and neither is enough alone**, which is what took the deciding.
   * `literalShaped` reads the *source*, and is what keeps a suffix load-bearing: `Some(3)` and
   * `Some(3int)` analyze to the same node at the same type, so only the spelling says that one of
   * them chose its width. `constructed` reads the *analyzed node*, and is what separates `Some(3)`
   * from `f(3)`: the two parse identically — `Call(Ident(…), List(IntLit(3)))` — and a function's
   * result has nothing to do with the literals handed to it, so nothing about an ordinary call
   * moves.
   *
   * **It is deliberately not cleverer than that.** `Some(f(3))` is literal-shaped and is marked
   * adaptable although its type came from `f`'s return. Consulted last it still settles whatever
   * nothing else settles, so nothing that compiled before stops compiling; the whole cost is that a
   * mismatch in a call carrying one is reported at the construction rather than at its neighbour.
   * Knowing which type arguments really came from `literalDefault` would need the analysis to
   * report it, which is a great deal of machinery for a caret's position.
   */
  protected def adaptable(e: Expr, t: TExpr): Boolean = e match
    // A filled default is asked about as what it wraps — it stands where the argument would have
    // been written (`reference/declarations.md § Default parameters and named arguments`).
    case DefaultArg(_, v) => adaptable(v, t)
    case _                => isLiteral(e) || literalShaped(e) && constructed(t)

  /** Whether an argument is a **default the call left out** whose value is a bare literal — the one
   * argument the caller did not write. It is adaptable like any literal, and it is consulted after
   * every adaptable argument the call *did* write, so it settles a type parameter only where nothing
   * else in the call or its context does.
   */
  protected def omittedLiteral(e: Expr): Boolean = e match
    case DefaultArg(_, v) => isLiteral(v) || omittedLiteral(v)
    case _                => false

  /** Whether an expression is written out of adaptable literals and nothing else — one of them, or a
   * construction over them, however deeply nested.
   *
   * A string, a character and a suffixed number all stop it, because each of those says what it is.
   */
  protected def literalShaped(e: Expr): Boolean = e match
    case _ if isLiteral(e)   => true
    case NamedArg(_, v)      => literalShaped(v)
    case Call(_, args)       => args.nonEmpty && args.forall(literalShaped)
    case ArrayLit(elems)     => elems.nonEmpty && elems.forall(literalShaped)
    case ArrayFill(v, _)     => literalShaped(v)
    case _                   => false

  /** Whether an analyzed expression is a **construction**: a value built here out of the parts
   * written beside it, rather than one something else answered with.
   *
   * The distinction is the type's provenance and not the node's kind. A construction's type
   * arguments are read off what it was handed, so literals inside it reach its type; anything
   * else — a call, a load, a field — has a type of its own that the literals in the expression
   * cannot have decided.
   */
  protected def constructed(t: TExpr): Boolean = t match
    case _: TEnumNew | _: TStructNew                            => true
    case _: TArrayLit | _: TArrayFill | _: TBufLit | _: TBufFill => true
    case _: TVectorLit                                          => true
    case _                                                      => false

  /** The type an adaptable literal falls back to, worked out from how it is written rather than
   * by analyzing it.
   *
   * Inference needs a type for every argument before it can decide what any parameter is, and a
   * literal at a parameter still being solved has nothing to give it but this default — while
   * *analyzing* the literal against that default is a range check it should not have to pass. A
   * `5000000000` headed for a `u64` is a fine argument, and asking it to be an `int` first is how
   * it would be refused for being one. So the fallback is read off the spelling, and the literal
   * is analyzed once, later, against the parameter the solution gave it.
   */
  protected def literalDefault(e: Expr): Option[Type] = e match
    case IntLit(_, None)     => Some(Type.Int)
    case FloatLit(_, None)   => Some(Type.Real)
    case Unary("-", operand) => literalDefault(operand)
    case _                   => None

  /** An explicit scalar conversion. Every pair that has a meaning is listed; nothing widens,
   * narrows, or changes representation without being written.
   *
   * `asked` is what the reader actually wrote, where that is not the target's own name: a conversion
   * into a transparent subtype is this conversion into its base, and a refusal naming the base would
   * be answering about a type the line does not mention.
   */
  protected def convert(t: TExpr, to: Type, asked: Option[String] = None): TExpr =
    if to == Type.Str then encode(t)
    else {
      // A written conversion is licensed to reach a constrained value's base representation, so the
      // source kind is read through `underlying`: `f64(m)` unwraps a derived `Meters`, `int(age)` an
      // `Age`. The target of a scalar conversion is always a plain scalar, so only the source strips.
      val allowed = (Type.underlying(t.ty), to) match
        case (_: Type.Integer, _: Type.Integer)   => true
        case (_: Type.Integer, _: Type.Floating)  => true
        case (_: Type.Floating, _: Type.Integer)  => true
        case (_: Type.Floating, _: Type.Floating) => true
        case (Type.Char, _: Type.Integer)         => true // total: every char is an integer
        case (_: Type.Integer, Type.Char)         => true // partial: traps on a non-scalar value
        case (Type.Char, Type.Char)               => true
        // Enum → integer is total: every enum value is one of its declared discriminants. Only a
        // simple enum has an integer value to give; a data enum is a tagged union, not a number.
        case (e: Type.Enum, _: Type.Integer) =>
          if !e.simple then
            err(s"only a simple enum converts to an integer — ${show(e)} carries data")
          true
        // A pointer → integer is total: an address *is* a number of `usize`'s width, so reading one
        // as that number loses nothing and yields a value nothing can dereference. It goes only
        // this way; making a pointer out of an integer is `ptr_cast` in the raw tier
        // (`reference/memory.md § Reinterpreting storage`). A pointer to a trait is two words
        // rather than an address, so it has no number.
        case (Type.Ptr(_: Type.Trait), _: Type.Integer) =>
          err("a pointer to a trait is two words — the address and the table of the type it was " +
            "erased from — so it is not a number")
        case (_: Type.Ptr, i: Type.Integer) =>
          if !i.pointerWidth then
            err(s"an address is read as 'usize' or 'isize' rather than as ${show(i)} — a fixed " +
              "width is not an address's width on every target")
          true
        // The address of a function is an address, and `ptr_cast` already accepts one — so refusing
        // it here made the number reachable in two steps and not in one, which is an inconsistency
        // rather than a position. `*extern` is a type of its own because there is no value at the
        // end of it to read through; that is about dereferencing, not about whether it has an
        // address. Card 0399.
        case (_: Type.CFn, i: Type.Integer) =>
          if !i.pointerWidth then
            err(s"an address is read as 'usize' or 'isize' rather than as ${show(i)} — a fixed " +
              "width is not an address's width on every target")
          true
        case _ => false

      if !allowed then
        err(asked match
          case Some(name) => s"cannot make $name from ${show(t.ty)}"
          case None       => s"cannot convert ${show(t.ty)} to ${show(to)}")

      TCast(t, to)
    }

  /** `string(c)` — one scalar value encoded as the bytes that spell it (`04`).
   *
   * It is the one conversion whose result is not a scalar, so it produces fresh bytes rather than a
   * reinterpretation of the ones already there, and it allocates where every other conversion does
   * not. The encoding is the one `str(c)` performs, which is why the two agree to the byte and why
   * this needs nothing of its own underneath.
   *
   * Nothing else converts. A number has a *rendering* rather than an encoding — there is no one
   * answer, as `f"…%x…"` shows — so it is sent to the form that renders, and a value already made of
   * bytes is sent to the form that validates them.
   */
  private def encode(t: TExpr): TExpr = Type.underlying(t.ty) match
    case Type.Char => TStr(t)
    case Type.Str  => err("a 'string' conversion encodes a char, and this value is already a string")
    case Type.Slice(Type.Byte, _) =>
      err("a 'string' conversion encodes a char — bytes are already text or are not, so " +
        s"'${Modules.show(Library.key("from_utf8"))}(b)' is what says which")
    case other =>
      err(s"a 'string' conversion encodes a char, and ${show(other)} is not one — 'str(x)' renders " +
        "a value as text")

  /** Widens a value to the one width a renderer takes, leaving one that is already there alone.
   * The library carries one rendering per *kind* rather than one per type, so every integer meets
   * `long` or `ulong` and every float meets `real` on the way in.
   */
  protected def widen(t: TExpr, to: Type): TExpr = if t.ty == to then t else TCast(t, to).setPos(t.pos)

  /** The result type of an arithmetic or bitwise binary operator. Operands must already have
   * the same type — there is no implicit promotion, so a mixed-width expression is an error
   * asking for a conversion rather than a silent widening. `+` on two strings concatenates,
   * allocating a fresh buffer; it is deliberately strict, so `s + 5` is an error asking for
   * interpolation rather than a silent `str(5)`.
   *
   * `rhs` is where the **right operand** was written, and it is where a mismatch is reported. An
   * expression's position is otherwise its start, which is what every other diagnostic here uses —
   * but a message naming two types and complaining about the second is one the reader answers by
   * editing that operand, so the caret goes under it. The dispatched path already did this
   * (`OperatorCalls.checkOperand`), and the two disagreed until this took a position: `1 + "x"`
   * pointed at the `1` while a `Box[int] + string` pointed at the `string`.
   *
   * Only the mismatch moves. "operator is not defined for" below is about the operator and the type
   * it was applied to rather than about one operand, so it keeps the expression's own position.
   *
   * **A shift is the exception, and it is the one operator where the rule above does not apply.**
   * `a + b` combines two values that have to agree on what they are; `x << n` asks for `x` shifted
   * `n` places, and `n`'s width has nothing to do with `x`'s. So the count may be any integer, the
   * result is the shifted value's own type, and the count is brought to that width by codegen
   * (`ScalarEmitter.shiftAmount`) rather than by a conversion the reader has to write. That is C's
   * rule, Rust's, Java's, Go's and Scala's; a `x << usize(n)` written for the compiler's benefit is
   * noise in exactly the code — a hash, a bitset, a decoder — where widths are already being
   * juggled. It reaches a **scalar** shift only: LLVM's vector `shl` takes one register type, and a
   * lane-wise count is a different question from this one.
   */
  protected def arithType(op: String, a: Type, b: Type, rhs: Option[Pos]): Type = {
    // Operands must agree on their *representation*: a transparent subtype meets its base and other
    // subtypes over it, while a derived type meets only itself (`repr` keeps it distinct). The
    // result is that shared representation — except two values of one derived type stay in it, since
    // arithmetic on a derived numeric yields the same derived numeric.
    val ra    = Type.repr(a)
    val shift = (op == "<<" || op == ">>") && Type.underlying(a).isInstanceOf[Type.Integer]
    if !shift && ra != Type.repr(b) then
      at(rhs)(err(s"'$op' needs matching types, got ${show(a)} and ${show(b)}"))
    // A shift's count is a count, so what it has to be is an integer rather than the shifted
    // value's own type. The message names it as a count for the same reason.
    if shift && !Type.underlying(b).isInstanceOf[Type.Integer] then
      at(rhs)(err(s"'$op' shifts by a count, and ${show(b)} is not an integer"))
    val result = a match
      case c: Type.Constrained if c.derived && (a == b || shift) => a
      case _                                                     => ra
    (Type.underlying(a), op) match
      case (_: Type.Integer, "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^") => result
      case (_: Type.Floating, "+" | "-" | "*" | "/")                                      => result
      case (Type.Str, "+")                                                                => result
      // C's `ptrdiff_t`: the one arithmetic two pointers have, and the only operator whose result is
      // neither operand's type nor their shared representation. It counts *elements*, so it is the
      // inverse of `&p[n]` (`03`) — which is why the pointee decides the stride and why two pointers
      // into different objects are the programmer's business, exactly as `p[i]` already is.
      case (Type.Ptr(_), "-")                                                             => Type.isize
      // **A vector's operators are its lane's, applied to every lane.** There is no promotion here
      // either, so the two registers agree on width and lane type before this is reached and the
      // result is that same type.
      //
      // A mask — `<N>bool` — has the bitwise three and nothing else, which is what makes
      // `(a < b) & (c < d)` the way two conditions are combined. `&&` is not among them and cannot
      // be: it short-circuits, and there is no such thing per lane.
      case (Type.Vector(_, lane), _) =>
        (Type.underlying(lane), op) match
          case (_: Type.Integer, "+" | "-" | "*" | "&" | "|" | "^" | "<<" | ">>") => result
          case (_: Type.Floating, "+" | "-" | "*" | "/")                          => result
          case (Type.Bool, "&" | "|" | "^")                                       => result
          // Integer division is refused rather than unimplemented, and the reason is worth the
          // sentence: the scalar form traps on a zero divisor and on the one signed overflow, and
          // both guards reduce a lane-wise comparison to a single `i1` — so a vector would either
          // drop the guard or trap for lanes that were fine. No machine sysl targets has an integer
          // vector divide anyway, so what looks like an omission costs a scalarized loop either way.
          case (_: Type.Integer, "/" | "%") =>
            err(s"'$op' is not defined on ${show(a)} — integer division traps per lane and a " +
              s"register traps as a whole, so divide the lanes in a loop over an array")
          case _ => err(s"operator '$op' is not defined for ${show(a)}")
      case _ => err(s"operator '$op' is not defined for ${show(a)}")
  }

  /** The type a unary operator yields at `a`, by the rule `arithType` uses for a binary one: a
   * transparent subtype's arithmetic happens at its base and yields the base, while a derived one's
   * happens at itself and yields itself (`reference/errors.md § A derivation inherits its base's
   * behaviour and may replace none of it`).
   */
  protected def unaryType(a: Type): Type = a match
    case c: Type.Constrained if c.derived => a
    case _                                => Type.repr(a)

  /** What a value has to satisfy to come to have type `t`, where `t` is a constrained subtype that
   * asks anything of its values. A subtype with neither a range nor a predicate — `type Stamp = new
   * i64`, which exists to be a distinct name — asks nothing, so producing one costs no instruction.
   *
   * This is the question `reference/errors.md § Where a constraint is checked` is about, asked of a
   * type rather than of a syntactic form: every site that gives a value this type consults it, so
   * the set of checked sites is the set of sites that produce one, by construction rather than by a
   * list somebody kept up to date.
   */
  protected def constraintOf(t: Type): Option[Type.Constrained] = t match
    case c: Type.Constrained if c.lo.nonEmpty || c.hi.nonEmpty || c.predFn.nonEmpty => Some(c)
    case _                                                                          => None

  /** An operation whose *result* is a constrained subtype, checked where it is made. Only a derived
   * subtype's own arithmetic produces one — a transparent subtype computes at its base, so its
   * results are base values and the check waits for the store that gives one the subtype again.
   *
   * Without this, `reference/errors.md § A derivation inherits its base's behaviour and may replace
   * none of it` (a derivation's operators produce itself) and `reference/errors.md § Where a
   * constraint is checked` (every produce site is checked) could not both be true: `Slot(199) +
   * Slot(1)` would be a `Slot` holding 200.
   */
  protected def produced(t: TExpr): TExpr = constraintOf(t.ty) match
    case Some(c) => TConstrainedCheck(t, c).setPos(t.pos)
    case None    => t

  /** Renders a decimal float literal as an LLVM hex double — the textual form that survives
   * the round-trip without losing bits.
   */
}
