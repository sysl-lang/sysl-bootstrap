package sh.sysl

import scala.collection.mutable

/** Constants, module-level `val`s, and the constant folder that gives a `const` its value.
 *
 * A constant is a scalar and nothing else (`reference/modules.md § const — a value`), which is not an arbitrary restriction but the
 * shape of what a constant expression can produce: there is no aggregate literal to fold to, and a
 * table would be storage rather than a value. That is what lets a constant be answered without any
 * of the type tables — registered in the first hoisting pass and already nameable from an array
 * bound in the second.
 *
 * The folder underneath is small on purpose. It evaluates what `reference/modules.md § const — a value` says a constant expression is
 * and nothing more, and it answers `None` rather than guessing, so the two positions that consume it
 * — an array bound and an enum discriminant — can report "not a constant expression" against the
 * expression the programmer actually wrote.
 *
 * A `val` is the other half and needs none of this: its type is **written** rather than inferred
 * (`reference/modules.md § Visibility`), so it is answered without looking at the initializer,
 * which is what lets one `val` be read from another's neighbourhood with no ordering between them —
 * exactly as two functions may call each other.
 */
trait ConstFolding extends ImportResolution {

  /** Resolving a written type, which a constant's declaration carries. Defined by `TypeResolution`,
   * which is mixed in after this: the two are mutually recursive, since an array bound is a constant
   * expression and a constant is declared with a type.
   */
  protected def resolveType(t: TypeRef, subst: Map[String, Type]): Type

  /** The same, in one of the positions that may carry a `volatile` qualifier — a struct field among
   * them, which is why this is visible from here.
   */
  protected def resolveQualified(t: TypeRef, subst: Map[String, Type]): Type

  /** A written type with any `volatile` taken off the front of it — for the one place that reads a
   * field list as a **parameter** list, where what travels is the value rather than the storage.
   */
  protected def unqualifiedRef(t: TypeRef): TypeRef = t match
    case VolatileType(inner) => unqualifiedRef(inner)
    case other               => other

  /** Recognising a scalar type name, which is all a constant may be declared as. */
  protected def scalarType(name: String): Option[Type]

  // --- constants -------------------------------------------------------------------------

  /** The key a written **constant** name resolves to (`reference/modules.md § const — a value`). */
  protected def constKey(written: String): Option[String] = resolveName(written)(constDecls.contains)

  /** A dotted path written as field reads, flattened back into the name it was written as — `c.limit`
   * from `Field(Ident("c"), "limit")`.
   *
   * A qualified name and a field read are the same shape after parsing, and which one is meant is
   * decided by what the name resolves to rather than by how it was written. So this hands back a
   * candidate spelling and nothing more; `None` is for a receiver that is an expression rather than a
   * name, which cannot be a module path however it is read.
   */
  protected def dottedName(e: Expr): Option[String] = e match
    case Ident(n)         => Some(n)
    case Field(recv, name) => dottedName(recv).map(r => s"$r.$name")
    case _                 => None

  /** The type a constant was declared with.
   *
   * A constant is a scalar and nothing else, which is not an arbitrary restriction but the shape of
   * what a constant expression can produce: there is no aggregate literal to fold to, and a table
   * would be storage rather than a value (`reference/modules.md § const — a value`). Resolving it needs none of the type tables,
   * which is what lets a constant be registered in the first hoisting pass and named from an array
   * bound in the second.
   *
   * **A transparent subtype of a scalar is a scalar**, and is admitted for the reason
   * `reference/errors.md § Constrained types` gives: without `new` such a type *is* its base, so
   * refusing it here was this rule reaching a case it was never about. What it buys is the case a
   * `c type` was built for — a typedef whose width the target decides, and the constants that have
   * to be that width — and, for a written subtype, a `within` range checked below against a value
   * that is already known.
   */
  protected def constType(key: String): Type = constTypes.getOrElseUpdate(key, {
    val decl = constDecls(key)

    inDecl(key)(resolveType(decl.typ, Map.empty)) match
      case t @ (_: Type.Integer | _: Type.Floating | Type.Bool | Type.Char | Type.Str) => t
      case c: Type.Constrained => at(decl.pos)(constrainedConst(c, qn(key)))

      // **An aggregate is named as STORAGE and pointed at `val`**, because a reader who has just
      // been told a constant is a scalar has no way to know that the same initializer is legal one
      // keyword away. It is: `ModuleStorage.isStatic` answers `true` for an array literal, an array
      // fill and a struct over static arguments, so `val nil: Uuid = Uuid(ZEROS)` is laid straight
      // into the object file and links on a freestanding target. The two spellings are not a
      // constancy disagreement — they are a value and storage, which is the distinction this says
      // out loud.
      case other @ (_: Type.Array | _: Type.Vector | _: Type.Struct) =>
        at(decl.pos)(err(s"a constant is a scalar, and ${show(other)} is storage — write " +
          s"'${qn(key)}' as a 'val', which is laid into the object file as constant data"))

      case other =>
        at(decl.pos)(err(s"a constant is a scalar, and ${show(other)} is not — '${qn(key)}'"))
  })

  /** A constrained type as a constant's declared type, or the reason it is not one.
   *
   * **`new` is refused because reaching a distinct type from its base is a written conversion**
   * (`reference/errors.md § new is what makes it a type`), in both directions and with no position
   * excused — and a constant is the literal it was written as, so there is nowhere on the line for
   * the conversion to go.
   *
   * **A `where` predicate is refused because there is no site to run it at.** A predicate is a sysl
   * function, checked where a value is *made* (`16`), and a constant is folded into every use rather
   * than made anywhere. Admitting one and never running it would be a check the declaration claims
   * and the program does not get. A `within` range is admitted for the opposite reason: it is a
   * comparison against a number the compiler already has in hand, which is exactly the `@assert`
   * somebody would otherwise write underneath.
   */
  private def constrainedConst(c: Type.Constrained, what: String): Type =
    if c.derived then
      err(s"'$what' is declared ${show(c)}, which is a 'new' type — reaching one from its base is a " +
        "written conversion, and a constant is the value it is written as, so there is nowhere here " +
        "to write it")
    else if c.predFn.isDefined then
      err(s"'$what' is declared ${show(c)}, whose 'where' predicate is checked where a value is " +
        "made — and a constant is folded into its uses rather than made anywhere, so the check has " +
        "no site. A 'within' range is settled here; a predicate cannot be")
    else c

  /** A constant's value, as the literal every use of it is folded to.
   *
   * Memoized, and guarded against a constant defined in terms of itself. The cycle is reported
   * once, at whichever of them the walk reached first, naming the loop in the order it was followed
   * — which is the same account `reference/modules.md § The module graph is acyclic` gives of a
   * cycle between modules.
   */
  protected def constLiteral(key: String): Expr = constLits.getOrElseUpdate(key, {
    val decl = constDecls(key)

    if constsInProgress(key) then
      val loop = constsInProgress.dropWhile(_ != key).map(qn).mkString(" → ")

      at(decl.pos)(err(s"constant '${qn(key)}' is defined in terms of itself: $loop → ${qn(key)}"))

    // A block is read by the grammar wherever a binding's `=` is, so it reaches a `const` too — and a
    // `const` is folded into its uses rather than run, so there is no point at which the statements
    // would happen. Said here rather than left to the sentence below, because a reader who has just
    // learnt the form from `val` is owed the rule and not a verdict.
    decl.value match
      case _: Block =>
        at(decl.value.pos)(err(s"'${qn(key)}' is a 'const', so its value is folded into every use of " +
          "it — a block is run, and there is nowhere here for it to run. Write a 'val', whose " +
          "initializer is code"))
      case _ => ()

    constsInProgress += key
    try
      // **The declared type is resolved BEFORE the value is folded, and the order is the whole of
      // what a reader sees.** An aggregate initializer folds to nothing, so with the fold first the
      // refusal was always *"the value of 'x' is not a constant expression"* — a claim about
      // constancy, about an expression a `val` three lines away lays into the object file as
      // constant data. `constType`'s own rule was the right answer and was unreachable, because the
      // fold failed before anything asked what the type was.
      val ty = constType(key)

      val value = inDecl(key)(fold(decl.value).getOrElse(
        at(decl.value.pos)(err(s"the value of '${qn(key)}' is not a constant expression"))))

      checkFits(value, ty, s"'${qn(key)}'", decl.pos)
      value
    finally constsInProgress -= key
  })

  /** Whether a folded value fits the type it was declared at. A constant is written with its type
   * (`reference/modules.md § const — a value`), so this is the one place the two meet, and a value that does not fit is the mistake
   * a suffix-less literal would otherwise make silently.
   */
  private def checkFits(value: Expr, ty: Type, what: String, pos: Option[Pos]): Unit = (value, ty) match
    case (_, c: Type.Constrained)                            => checkAdmits(value, c, what, pos)
    case (IntLit(v, _), i: Type.Integer) if !Type.fits(v, i) => at(pos)(err(s"$what does not fit ${show(i)}: $v"))
    case (IntLit(_, _), _: Type.Integer)                     => ()
    case (FloatLit(_, _), _: Type.Floating)                  => ()
    case (BoolLit(_), Type.Bool)                             => ()
    case (CharLit(_), Type.Char)                             => ()
    case (StrLit(_), Type.Str)                               => ()
    case _ => at(pos)(err(s"$what is declared ${show(ty)} but its value is ${literalKind(value)}"))

  /** Whether a folded value is one its **constrained** type admits: the base's own width first, and
   * then the `within` range.
   *
   * This is the check that makes a constrained constant worth writing. A `c const` measured from a
   * header is a number nobody chose — a `#define` in somebody else's tree, a config macro, a
   * `sizeof` — so a range on the type it is read into is a claim about the configuration the program
   * was built against, settled while compiling rather than trusted. A written `const` gets the same
   * check for the same reason, one step earlier than the run-time one its `val` would have had.
   *
   * A type with a `where` predicate never reaches here: `constrainedConst` refuses it at the
   * declaration, because a predicate is a function and this is a comparison.
   */
  private def checkAdmits(value: Expr, c: Type.Constrained, what: String, pos: Option[Pos]): Unit = {
    checkFits(value, Type.underlying(c), what, pos)

    val number = value match
      case IntLit(v, _)   => Some(BigDecimal(v))
      case FloatLit(t, _) => Some(BigDecimal(t))
      case CharLit(cp)    => Some(BigDecimal(cp))
      case _              => None

    for
      v  <- number
      lo <- c.lo
      hi <- c.hi
    do
      val under = if c.exclusiveHi then v < hi else v <= hi
      val top   = if c.exclusiveHi then s"under $hi" else hi.toString

      if v < lo || !under then
        at(pos)(err(s"$what is $v, which ${show(c)} does not admit — it holds $lo to $top"))
  }

  protected def literalKind(e: Expr): String = e match
    case _: IntLit   => "an integer"
    case _: FloatLit => "a float"
    case _: BoolLit  => "a boolean"
    case _: CharLit  => "a character"
    case _: StrLit   => "a string"
    case _           => "not a constant"

  /** The literal a bound **value parameter** stands for, which is decided by the type it was
   * declared with (`reference/generics.md § A parameter may stand for a value`).
   *
   * The argument travels as a `BigInt` because that is what a type's identity needs — something
   * that compares and mangles — and the declared type is what says how to read it back. A `bool`
   * whose parameter folded to an `IntLit` would be a `0` where the body wrote `B`, so the type is
   * not decoration here: it is the half of the pair that makes the number mean something.
   */
  protected def constArgLiteral(c: Type.ConstArg): Expr = c.ty match
    case Type.Bool => BoolLit(c.value != 0)
    case Type.Char => CharLit(c.value.toInt)
    case _         => IntLit(c.value, None)

  /** The other direction: the number a written value argument stands for, or `None` where it is not
   * a value an identity can be made of.
   *
   * The admissible set is `reference/generics.md § A parameter may stand for a value`'s and the
   * reason is one sentence — a value in a type's identity must compare and must mangle. An integer,
   * a `bool` and a `char` each do; a float does not (`NaN != NaN` would make a type unequal to
   * itself) and a string does not until two spellings of one text are one value.
   */
  protected def constArgValue(e: Expr, subst: Map[String, Type] = Map.empty): Option[BigInt] =
    fold(e, subst).collect {
      case IntLit(v, _)  => v
      case BoolLit(b)    => if b then BigInt(1) else BigInt(0)
      case CharLit(c)    => BigInt(c)
    }

  /** A compile-time integer, for the two positions where a literal was previously the only thing
   * accepted: an array bound and an enum discriminant (`reference/modules.md § const — a value`).
   */
  protected def constInt(e: Expr, subst: Map[String, Type] = Map.empty): Option[BigInt] =
    fold(e, subst).collect { case IntLit(v, _) => v }

  // --- module-level `val`s ---------------------------------------------------------------

  /** The key a written name for module storage resolves to — a `val` or a `var`.
   *
   * The two are one lookup because they are one namespace and one kind of thing: storage the module
   * owns, reached by name. Which of the two a key names decides only whether the storage may be
   * *written*, and that is asked separately, where it matters.
   */
  protected def globalKey(written: String): Option[String] =
    resolveName(written)(k => valDecls.contains(k) || staticVarDecls.contains(k))

  /** Whether a key names storage that may be written — a module `var` rather than a `val`. */
  protected def globalWritable(key: String): Boolean = staticVarDecls.contains(key)

  /** The type a module-level `val` was declared with.
   *
   * Written rather than inferred (`reference/modules.md § Visibility`), which is what lets this be
   * answered without looking at the initializer — so one `val` may be read from another's
   * neighbourhood with no ordering between them, exactly as two functions may call each other.
   */
  protected def globalType(key: String): Type = valTypes.getOrElseUpdate(key, {
    inDecl(key)(staticVarDecls.get(key).orElse(valDecls.get(key)) match
      case Some(v: VarDecl) => v.typ.map(resolveType(_, Map.empty)).getOrElse(Type.Unknown)
      case Some(v: ValDecl) => v.typ.map(resolveType(_, Map.empty)).getOrElse(Type.Unknown)
      case _                => Type.Unknown)
  })

  // --- `extern` variables -----------------------------------------------------------------

  /** The key a written **`extern` variable** name resolves to. */
  protected def externVarKey(written: String): Option[String] =
    resolveName(written)(externVarDecls.contains)

  /** The type an `extern` variable was declared with. Written for the reason a `val`'s is and one
   * more: there is no initializer to infer it from, because the storage was laid down elsewhere.
   */
  protected def externVarType(key: String): Type = externVarTypes.getOrElseUpdate(key, {
    val decl = externVarDecls(key)

    inDecl(key)(resolveType(decl.typ, Map.empty))
  })

  /** `@assert(cond)` / `@assert(cond, "why")` — settles the condition, and stops the compilation if
   * it is false.
   *
   * Three outcomes, and each is a different mistake to report. A condition that does not fold is
   * not a constant expression, and the reader has asked the compiler to settle something it cannot;
   * one that folds to a value that is not a `bool` is an assertion about nothing; and one that folds
   * to `false` is the assertion doing its job.
   *
   * The message is the reader's own where they wrote one, because they know what the number *means*
   * — that a struct matches its C counterpart, that a table is the size the protocol fixes — and the
   * expression alone says only that two numbers differ.
   *
   * **The substitution is what lets one be written inside a generic**, where the interesting facts
   * are per instantiation rather than per declaration: a body is analyzed once for each set of
   * arguments, so `sizeof(T)` in a condition has a width at every point the check runs. A module
   * file's asserts are declarations and pass nothing, having no parameters to substitute.
   *
   * The **fourth** outcome exists only inside a generic: a condition measuring a type that is still a
   * parameter does not fold *yet*, and the walk that checks the body before anything has instantiated
   * it is not the moment to complain. That is the same deferral `[sizeof(T)]u8` already gets, and it
   * is why a generic nothing calls carries an unchecked claim — there is nothing to check until
   * somebody chooses a `T`.
   */
  protected def checkAssert(a: AssertDecl, subst: Map[String, Type] = Map.empty): Unit =
    fold(a.cond, subst) match
      case Some(BoolLit(true))  => ()
      case Some(BoolLit(false)) =>
        err((a.message match
          case Some(m) => s"assertion failed: $m"
          case None    => "assertion failed") + comparedNote(a.cond, subst) + instantiationNote(subst))
      case Some(other) =>
        err(s"'@assert' takes a condition, and this is ${literalKind(other)} — an assertion is " +
          "something that can be true or false")
      case None if awaitsInstantiation(a.cond, subst) => ()
      case None =>
        err("'@assert' is settled while compiling, so its condition has to be a constant " +
          "expression — a literal, a 'const', 'sizeof', 'alignof', 'offsetof', or the arithmetic " +
          "and comparisons over them, and never a call")

  /** What a failed assertion adds when the body it is in was compiled against a substitution, and
   * nothing at all when it was not.
   *
   * The mistake is at the call that asked for `Slab[u8]` while the sentence explaining why is at the
   * declaration, so a report carrying only one of the two sends the reader to the wrong file. The
   * position is the condition's, which is the half that says *what* is wrong; naming the bindings is
   * the half that says which choice of types asked.
   *
   * `Self` is named alongside the type parameters rather than filtered out as the compiler's own
   * word. Which `Box` this is is exactly as much of the answer as which `T` it holds, and in an
   * inherited default body — one text shared by every implementing type — it is the *whole* answer.
   *
   * **It does not say "at this instantiation", though every early draft did.** A concrete type's
   * members carry a `Self` binding too, resolved once at hoist rather than per call, so that wording
   * announced an instantiation to a reader looking at a plain `struct Point` that nothing had
   * instantiated. Naming the bindings is true whichever way they were bound, and it is the part that
   * was carrying the information.
   */
  /** What a failed **comparison** adds: the value each side folded to.
   *
   * The compiler folded both sides to constants in order to decide the comparison, so it is holding
   * the number the reader needs at the moment it reports that the number is wrong. Throwing it away
   * left them to recover it the only way left — edit the literal, rebuild, repeat — which is a
   * bisection over builds for a fact that was in hand. That is worst in exactly the case the form
   * exists for: a mirrored C struct whose size moved reports that it is not 16 without saying it is
   * now 24, so *"did a field change width, or was one added"* cannot even be started from the
   * message.
   *
   * **A side the reader wrote as a literal is not named.** `sizeof(X) == 12` has the 12 on screen
   * already, and *"the right side is 12"* is a sentence that teaches nothing; where both sides are
   * computed — `sizeof(A) == sizeof(B)` — both are worth saying.
   *
   * **The expression is not re-rendered, deliberately.** The offending line is quoted with a caret
   * directly above this message, so the reader has the source text; what they lack is the number.
   * Printing the operand back would mean an `Expr` printer the tree does not have, and a partial one
   * lies about the shapes nobody thought of.
   *
   * Only a two-operand comparison reaches here. `fold` settles `Compare(List(l, r), List(op))` and
   * nothing longer, so a chained comparison is refused earlier as not a constant expression, and a
   * condition that is not a comparison at all has no operand worth naming — the thing that folded to
   * `false` *is* its only operand.
   */
  private def comparedNote(cond: Expr, subst: Map[String, Type]): String = cond match
    case Compare(List(l, r), List(_)) =>
      val left  = Option.when(!isWrittenLiteral(l))(fold(l, subst)).flatten.map(literalText)
      val right = Option.when(!isWrittenLiteral(r))(fold(r, subst)).flatten.map(literalText)

      (left, right) match
        case (Some(a), Some(b)) => s" — the left side is $a and the right side is $b"
        case (Some(a), None)    => s" — the left side is $a"
        case (None, Some(b))    => s" — the right side is $b"
        case (None, None)       => ""
    case _ => ""

  /** Whether an operand is a literal the reader **wrote**, rather than one folding produced.
   *
   * A negative number is written `-1` and parses as a unary minus over a literal, so it counts too —
   * otherwise the one shape a bound is most often compared against would be the one reported back.
   */
  private def isWrittenLiteral(e: Expr): Boolean = e match
    case _: IntLit | _: FloatLit | _: BoolLit | _: CharLit | _: StrLit => true
    case Unary("-", operand)                                          => isWrittenLiteral(operand)
    case _                                                            => false

  /** A folded literal as it should appear inside a sentence, spelled the way it would be written in
   * source — a `char` in single quotes and a `string` in double, so that a reader comparing the
   * sentence against their own line is looking at the same thing twice.
   */
  private def literalText(e: Expr): String = e match
    case IntLit(v, _)   => v.toString
    case FloatLit(t, _) => t
    case BoolLit(b)     => b.toString
    case CharLit(c)     => s"'${String(Character.toChars(c))}'"
    case StrLit(s)      => s"\"$s\""
    case other          => literalKind(other)

  private def instantiationNote(subst: Map[String, Type]): String =
    val bound = subst.toList.sortBy(_._1)

    if bound.isEmpty then ""
    else s" — where ${bound.map((n, t) => s"$n = ${show(t)}").mkString(", ")}"

  /** Folds a constant expression to the literal it denotes, or `None` where it is not one.
   *
   * The set is deliberately small and closed: literals, other constants, conversions, and the
   * unary and binary operators. There are no calls — a call in a constant expression is a request
   * for compile-time evaluation of arbitrary code, which is a language of its own — and a `string`
   * folds only from a literal, since `+` on strings allocates and a compile-time concatenation
   * would be a different operation wearing the same spelling.
   */
  protected def fold(e: Expr, subst: Map[String, Type] = Map.empty): Option[Expr] = e match
    case l: IntLit   => Some(l.copy(suffix = None))
    case l: FloatLit => Some(l.copy(suffix = None))
    case l: BoolLit  => Some(l)
    case l: CharLit  => Some(l)
    case l: StrLit   => Some(l)

    // A **value parameter** is asked before a declared constant, and shadows one of the same name
    // for the same reason a type parameter shadows a type: the nearer binding is the one written
    // where the name is (`reference/generics.md § A parameter may stand for a value`). During the
    // walk that checks a generic body there is no argument bound yet, so the name simply is not in
    // the substitution and falls through — `constInt`'s caller reads that as awaiting
    // instantiation, exactly as it already does for `sizeof(T)`.
    case Ident(n) =>
      subst.get(n).collect { case c: Type.ConstArg => constArgLiteral(c) }
        .orElse(constKey(n).map(k => constLiteral(k)))

    // `A.len` — how many types a **pack** stands for (`reference/generics.md § A parameter may
    // stand for a list of types`), which is a compile-time integer and folds as one. It is here as
    // well as in the analyzer because the range of a `for const` is read *before* anything is
    // analyzed: the loop has to know how many copies to make before it can make one. During the
    // walk that checks a generic body the pack stands at two, so this is 2 there.
    case Field(Ident(n), "len") =>
      subst.get(n).collect { case Type.Pack(elems) => IntLit(BigInt(elems.length), None) }

    // A **module-qualified** constant — `c.limit`. A `const` is registered under its dotted path and
    // `constKey` has always taken one, which is why the two positions that read a constant *as a
    // written name* — a match pattern, an ordinary expression — accepted this spelling from the
    // start. Only the folder did not: it matched a bare `Ident` and nothing else, so the very same
    // declaration was a constant when imported unqualified and not one when reached through its
    // module. A `Field` that names no constant answers `None` and is reported as whatever it is,
    // which is what an ordinary field read of a value has always been here.
    case f: Field => dottedName(f).flatMap(constKey).map(constLiteral)

    case Unary("-", operand) =>
      fold(operand, subst).collect {
        case IntLit(v, _)   => IntLit(-v, None)
        case FloatLit(t, _) => FloatLit((-t.toDouble).toString, None)
      }
    case Unary("!", operand) => fold(operand, subst).collect { case BoolLit(b) => BoolLit(!b) }
    case Unary("~", operand) => fold(operand, subst).collect { case IntLit(v, _) => IntLit(~v, None) }

    // A conversion is written, so what it does at compile time is what it does at run time: a
    // narrowing wraps and a float-to-integer truncates toward zero (`01`). Silently doing something
    // gentler here would make a constant mean one thing and the same expression written out mean
    // another.
    case Call(Ident(name), List(arg)) =>
      for
        target <- scalarType(name)
        value  <- fold(arg, subst)
        out    <- convert(value, target)
      yield out

    case Binary(op, l, r) => for (a <- fold(l, subst); b <- fold(r, subst); v <- binary(op, a, b)) yield v
    case Compare(List(l, r), List(op)) =>
      for (a <- fold(l, subst); b <- fold(r, subst); v <- binary(op, a, b)) yield v

    // `sizeof(T)` and `alignof(T)` are compile-time constants (`reference/memory.md §
    // Reinterpreting storage`), so they fold exactly as a literal does. That is what makes them
    // usable in the two positions this folder serves — an array bound and an enum discriminant — as
    // well as in a `const`, which is where a program names the block size a slab is laid out in.
    //
    // **The substitution is what lets the measured type be the caller's own parameter.** A generic
    // body is analyzed once per instantiation with its parameters bound to the concrete arguments
    // (`instantiateFunc`), so `sizeof(T)` inside one has a width at every point it is compiled —
    // and resolving it against an empty map instead would report the parameter as an unknown type,
    // which is a name the reader can see is declared right there.
    case LayoutOf(what, tr) => layoutBytes(what, resolveType(tr, subst)).map(n => IntLit(n, None))

    // `offsetof(T, field)` folds for the same reason and under the same deferral: a layout is fixed
    // while compiling, and a type parameter's is fixed at each instantiation rather than here.
    case OffsetOf(tr, field) => offsetBytes(resolveType(tr, subst), field).map(n => IntLit(n, None))

    // `T::Min` and `T::Max` on a built-in integer are constants for the same reason `sizeof` is:
    // the answer is a property of the type and is known once the type is. Folding them here is what
    // puts them in a `const` initializer, an `@assert` and an array bound — the positions this
    // folder serves, and the ones where a bound is most worth naming.
    //
    // A type **parameter** is not folded: `T::Max` inside a generic has no width until the body is
    // compiled for a concrete `T`, and `subst` is what supplies one. Falling through to `None` here
    // is the same deferral `sizeof(T)` gets, and `awaitsInstantiation` reports it the same way.
    case TypeAttr(Ident(name), attr) =>
      for
        i     <- substScalar(name, subst).collect { case i: Type.Integer => i }
        value <- attr match
                   case "Min" => Some(Type.minOf(i))
                   case "Max" => Some(Type.maxOf(i))
                   case _     => None
      yield IntLit(value, None)

    case _ => None

  /** The integer a name stands for while folding, looking through a generic's substitution first so
   * that `T::Max` in an instantiated body measures the argument rather than the parameter's name.
   */
  private def substScalar(name: String, subst: Map[String, Type]): Option[Type] =
    subst.get(name).map(Type.underlying).orElse(scalarType(name))

  /** The bytes `sizeof(T)` / `alignof(T)` answer with, or `None` where the type has no answer here.
   *
   * There are two ways to have no answer, and neither is a mistake to report. A **type parameter**
   * is concrete at every instantiation and stands in for itself only during the walk that reports
   * what a generic body's bounds do not license, so the measurement is not wrong there — it is not
   * being made yet. A **poisoned** type has already been complained about once, and saying its width
   * is unknown would be a second complaint about the same thing.
   */
  protected def layoutBytes(what: String, ty: Type): Option[Int] = Type.underlying(ty) match
    case _: Type.Abstract | Type.Unknown => None
    case t                               => Some(if what == "sizeof" then layout.size(t) else layout.align(t))

  /** The bytes `offsetof(T, field)` answers with.
   *
   * The two silent `None`s are exactly `layoutBytes`' — a parameter awaiting its instantiation, and a
   * type already complained about — and every other way to have no answer is a mistake worth its own
   * sentence rather than a fall through to "not a constant expression". A misspelled field is the one
   * that matters: the whole point of the form is to be told when a mirror and its original disagree,
   * so being told *nothing* about a name that is not there would be the failure in miniature.
   */
  protected def offsetBytes(ty: Type, field: String): Option[Int] = Type.underlying(ty) match
    case _: Type.Abstract | Type.Unknown => None
    case s: Type.Struct =>
      Bitfields.of(s).flatMap(_.find(_.name == field)) match
        case Some(r) =>
          err(s"'${show(s)}.$field' is a bitfield — it starts at bit ${r.offset} of the struct and is " +
            s"${r.width} bits wide, so it has no byte offset. 'offsetof' answers in bytes, and the byte " +
            s"it begins in is not where it is")
        case None => byteOffset(s, field)
    case other =>
      err(s"'offsetof' measures a field of a struct, and ${show(other)} is not one")

  /** The same measurement for a field that has one: a byte offset into an ordinary layout. */
  private def byteOffset(s: Type.Struct, field: String): Option[Int] =
    layout.fieldOffset(s, field) match
      case some @ Some(_) => some
      case None if s.fieldIndex(field) >= 0 =>
        err(s"'${show(s)}.$field' occupies no storage, so it has no offset — a zero-sized field " +
          "lands nowhere, and a C struct that is being mirrored has no member matching it either")
      case None =>
        err(s"'${show(s)}' has no field '$field'" + (
          if s.fields.isEmpty then "" else s" — it stores ${s.fields.map(f => s"'${f._1}'").mkString(", ")}"
        ))

  /** Whether a constant expression does not fold **yet** rather than not folding at all: it measures
   * a type that is still a parameter, and every instantiation will supply one that is not.
   *
   * The distinction is the whole of what separates a deferred bound from a mistake. `[sizeof(T)]u8`
   * inside a generic is a well-formed array whose length nobody can name until the body is compiled
   * for a particular `T`; `[n]u8` over a variable is a length that will never be a constant however
   * many times it is instantiated. Both fail to fold, and only the second is worth a diagnostic.
   *
   * It walks the same shapes `fold` does, since a bound may measure a type inside arithmetic —
   * `[sizeof(T) * 3 + 1]u8` is the shape a decimal-digit buffer wants, and none of its parts folds
   * on its own either.
   */
  protected def awaitsInstantiation(e: Expr, subst: Map[String, Type]): Boolean = e match
    case LayoutOf(_, tr) =>
      Type.underlying(resolveType(tr, subst)) match
        case _: Type.Abstract => true
        case _                => false
    case OffsetOf(tr, _) =>
      Type.underlying(resolveType(tr, subst)) match
        case _: Type.Abstract => true
        case _                => false
    // A **value parameter** during the walk that checks the generic body: it is bound to the same
    // `Abstract` stand-in a type parameter gets, so it does not fold and is not an error either
    // (`reference/generics.md § A parameter may stand for a value`). A name bound to a `ConstArg`
    // is not here, because that one folds.
    case Ident(n) =>
      subst.get(n).exists(_.isInstanceOf[Type.Abstract])
    case Unary(_, operand)             => awaitsInstantiation(operand, subst)
    case Binary(_, l, r)               => awaitsInstantiation(l, subst) || awaitsInstantiation(r, subst)
    case Compare(List(l, r), _)        => awaitsInstantiation(l, subst) || awaitsInstantiation(r, subst)
    case Call(Ident(_), List(arg))     => awaitsInstantiation(arg, subst)
    case _                             => false

  private def convert(value: Expr, target: Type): Option[Expr] = (value, target) match
    case (IntLit(v, _), i: Type.Integer) => Some(IntLit(Type.wrap(v, i), None))
    case (IntLit(v, _), _: Type.Floating) => Some(FloatLit(v.toDouble.toString, None))
    case (IntLit(v, _), Type.Char) if v >= 0 && v <= 0x10FFFF && !(v >= 0xD800 && v <= 0xDFFF) =>
      Some(CharLit(v.toInt))
    case (IntLit(v, _), Type.Char)        => err(s"$v is not a Unicode scalar value")
    case (FloatLit(t, _), i: Type.Integer) => Some(IntLit(Type.wrap(BigInt(t.toDouble.toLong), i), None))
    case (FloatLit(t, _), _: Type.Floating) => Some(FloatLit(t, None))
    case (CharLit(c), i: Type.Integer)    => Some(IntLit(Type.wrap(BigInt(c), i), None))
    case _                                 => None

  private def binary(op: String, l: Expr, r: Expr): Option[Expr] = (l, r) match
    case (IntLit(a, _), IntLit(b, _)) =>
      op match
        case "+"  => Some(IntLit(a + b, None))
        case "-"  => Some(IntLit(a - b, None))
        case "*"  => Some(IntLit(a * b, None))
        case "/"  => if b == 0 then err("a constant divided by zero") else Some(IntLit(a / b, None))
        case "%"  => if b == 0 then err("a constant divided by zero") else Some(IntLit(a % b, None))
        case "&"  => Some(IntLit(a & b, None))
        case "|"  => Some(IntLit(a | b, None))
        case "^"  => Some(IntLit(a ^ b, None))
        case "<<" => Some(IntLit(a << shiftBy(b), None))
        case ">>" => Some(IntLit(a >> shiftBy(b), None))
        case _    => compare(op, a.compare(b))
    case (FloatLit(a, _), FloatLit(b, _)) =>
      val (x, y) = (a.toDouble, b.toDouble)

      op match
        case "+" => Some(FloatLit((x + y).toString, None))
        case "-" => Some(FloatLit((x - y).toString, None))
        case "*" => Some(FloatLit((x * y).toString, None))
        case "/" => Some(FloatLit((x / y).toString, None))
        case _   => compare(op, x.compare(y))
    case (BoolLit(a), BoolLit(b)) =>
      op match
        case "&&" => Some(BoolLit(a && b))
        case "||" => Some(BoolLit(a || b))
        case "==" => Some(BoolLit(a == b))
        case "!=" => Some(BoolLit(a != b))
        case _    => None
    case (CharLit(a), CharLit(b)) => compare(op, a.compare(b))
    case (StrLit(a), StrLit(b)) =>
      op match
        case "==" => Some(BoolLit(a == b))
        case "!=" => Some(BoolLit(a != b))
        // Two string literals joined are a third string literal, which is the one operator on
        // strings that produces a value rather than a verdict — and at compile time it is text
        // beside text, with none of the allocating `sysl.str.concat` a run-time `+` lowers to. What
        // it is for is the long constant written in pieces: a table, a census, a usage message, one
        // line per source line so a reader can see the lines. Without it such a value could not be
        // a `const` at all and had to be a `val`, which is storage, and which no array bound or
        // `@assert` may name.
        case "+"  => Some(StrLit(a + b))
        case _    => None
    case _ => None

  /** A constant shift distance, as a number the fold can actually shift by.
   *
   * The ceiling is the widest integer the back end lowers rather than 64, because a shift past 64
   * is meaningful the moment a type is wider than that: `1 << 200` is an ordinary constant at
   * `u256`, and refusing it here would have made the fold narrower than the types it folds for. A
   * distance beyond every possible width is still refused — it cannot be a shift of anything, and
   * the `BigInt` it would produce is unbounded.
   */
  private def shiftBy(n: BigInt): Int =
    if n < 0 || n > Type.MaxIntegerBits then err(s"a constant shifted by $n places") else n.toInt

  private def compare(op: String, sign: Int): Option[Expr] = op match
    case "==" => Some(BoolLit(sign == 0))
    case "!=" => Some(BoolLit(sign != 0))
    case "<"  => Some(BoolLit(sign < 0))
    case "<=" => Some(BoolLit(sign <= 0))
    case ">"  => Some(BoolLit(sign > 0))
    case ">=" => Some(BoolLit(sign >= 0))
    case _    => None

  private val valTypes         = mutable.HashMap.empty[String, Type]
  private val externVarTypes   = mutable.HashMap.empty[String, Type]
  private val constTypes       = mutable.HashMap.empty[String, Type]
  private val constLits        = mutable.HashMap.empty[String, Expr]
  private val constsInProgress = mutable.LinkedHashSet.empty[String]
}
