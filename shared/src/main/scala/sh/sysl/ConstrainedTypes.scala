package sh.sysl

/** Building a constrained subtype from its declaration (`16`) — the `within` bounds folded to
 * constants, and the diagnostics for a bound that is not one.
 *
 * The bounds are their own area because what may stand in one is narrower than what may stand
 * anywhere else a constant is wanted: a literal, or a negated literal, of the base type. Saying
 * which kind of thing was written instead is most of what the code here does.
 */
trait ConstrainedTypes extends GenericInstantiation {

  /** Whether `key` names a plain **transparent alias** — `type Name = Existing`, with no `new`, no
   * `within` and no `where` — rather than a constrained subtype.
   *
   * An alias declares no type of its own: it is a second spelling for one that already exists, so
   * everything asked of the name is answered by the type it stands for. That is why the tables are
   * asked this question at all — a constrained subtype is a type and an alias is a *name*, and the
   * two want opposite handling from the same declaration form.
   *
   * A measured `c type` is written by the compiler in exactly this shape and is **not** an alias in
   * this sense: it is a distinct scalar whose width came from the C compiler, and the code below
   * builds it as one.
   *
   * **Nor is an exported one**, for the same reason seen from the header's side: `@export` asks a
   * generated header to spell the type by a name of its own, and a name that dissolved into its base
   * here would leave nothing there to spell. It is built as a transparent type with no constraint,
   * which is a `c type`'s shape — interchangeable with its base in both directions, as before.
   */
  protected def plainAlias(key: String): Boolean =
    constrainedDecls.get(key).exists(d =>
      !d.derived && d.range.isEmpty && d.pred.isEmpty && !d.fromC && d.cname.isEmpty)

  /** The key an alias ultimately stands for, following a chain of them, where what it names is a
   * declared type. A key that is not an alias, and one whose base is not a bare declared name — a
   * scalar, a pointer, an array, a callable — is its own answer.
   *
   * **The base is resolved in the ALIAS'S OWN SCOPE, which is the whole reason this is not a
   * substitution on the written name.** `type FRect = c.FRect` names `c` in the file that wrote the
   * alias; a file that uses `FRect` need not import `c` at all, and may well have its own `c`
   * meaning something else.
   *
   * A cycle — `type A = B` and `type B = A` — is walked at most as many steps as there are
   * declarations before giving up, so a program that writes one is refused rather than hanging.
   */
  override protected def followAlias(key: String): String = aliasedKey(key)

  protected def aliasedKey(key: String): String = {
    var seen = key
    var steps = 0

    while plainAlias(seen) && steps <= constrainedDecls.size do
      val next = inScope(scopeFor(seen)) {
        constrainedDecls(seen).base match
          case NamedType(n, Nil) => resolveName(n)(k => structDecls.contains(k) || enumDecls.contains(k) ||
            constrainedDecls.contains(k))
          case _                 => None
      }

      next match
        case Some(k) if k != seen => seen = k; steps += 1
        case _                    => return seen

    if steps > constrainedDecls.size then
      at(constrainedDecls(key).pos)(err(s"'${qn(key)}' is an alias for itself, through a chain of aliases"))

    seen
  }

  /** What a plain alias stands for, as a type. Only reached where `aliasedKey` could not answer —
   * the base is a scalar, a pointer, an array or a callable rather than a declared name — since a
   * declared one is followed at the key and never arrives here.
   */
  protected def resolveAlias(key: String): Type =
    inScope(scopeFor(key))(at(constrainedDecls(key).pos)(resolveType(constrainedDecls(key).base, Map.empty)))

  protected def resolveConstrained(key: String): Type.Constrained =
    constrainedInsts.getOrElseUpdate(key, buildConstrained(key))

  private def buildConstrained(key: String): Type.Constrained = {
    val d = constrainedDecls(key)

    at(d.pos) {
      // In the declaration's own scope, for `aliasedKey`'s reason: a pointer base names a struct in
      // the file that wrote the type, and the file using it need not import that struct's module.
      val base = inScope(scopeFor(key))(resolveType(d.base, Map.empty))

      // An exported name with no constraint is the one shape that may stand for more than a
      // number: what it asks for is a `typedef`, and C writes one for a pointer as readily as for
      // an integer. Anything C cannot spell that way is refused here, under the export's reading
      // rather than the subtype's, since the constraint sentence below would be about a question
      // nobody asked.
      val exportedAlias = d.cname.isDefined && !d.derived && d.range.isEmpty && d.pred.isEmpty

      if exportedAlias && !ExportCheck.typedefable(base) then
        err(s"'${qn(key)}' is exported to a C header as ${show(base)}, which C has no way to spell " +
          s"as a typedef — a header names an integer, a float, 'bool', 'char' or a pointer this way. " +
          ExportCheck.typedefAdvice(base))

      val scalar = Type.underlying(base) match
        case _: Type.Integer | _: Type.Floating | Type.Char => true
        // `bool` is a base for a measured `c type` and for nothing else, because C's `_Bool` is what
        // sysl's `bool` already is. Nothing is given up by admitting it: there is no range and no
        // predicate a `bool` could carry, so the one thing this allows is the alias itself — which is
        // also what an exported alias is.
        case Type.Bool                                      => d.fromC || exportedAlias
        case _                                              => exportedAlias
      if !scalar then
        err(s"a constrained subtype's base must be an integer, a float, or 'char', not ${show(base)}")

      val (lo, hi) = d.range match
        case Some(r) =>
          val loV = boundValue(r.lo, base)
          val hiV = boundValue(r.hi, base)
          val ordered = if r.exclusiveHi then loV < hiV else loV <= hiV
          if !ordered then
            err(s"the lower bound of '${qn(key)}' is above its upper bound")
          (Some(loV), Some(hiV))
        case None => (None, None)

      // A declaration with neither a range nor a predicate nor `new` is a plain alias, and an alias
      // declares no subtype — so nothing that wants one should have arrived here with its key.
      // Every caller either resolves the type expression, which branches on `plainAlias` first, or
      // reaches a key that `typeKey` has already followed past the alias. This is the backstop for
      // a route that grows later and forgets to: it says what is wrong rather than building a
      // `Constrained` with no constraint, which would be a subtype whose value set is everything.
      if lo.isEmpty && d.pred.isEmpty && !d.derived && !d.fromC && d.cname.isEmpty then
        err(s"'${qn(key)}' is a transparent alias and declares no subtype, so there is nothing here " +
          "to constrain")

      val predFn = if d.pred.isDefined then Some(predKey(key)) else None
      // Written bare, `@export` gives the header the declared name, as it does a struct's.
      val cname  = d.cname.map(e => e.symbol.getOrElse(Modules.bare(key)))
      Type.Constrained(key, base, d.derived, lo, hi, d.range.exists(_.exclusiveHi), predFn, cname)
    }
  }

  /** One `within` bound as a constant, checked against the base's kind: a `char` base takes a
   * character, an integer base an integer that fits its width, a float base any number. A bound of the
   * wrong kind, or an integer out of the base's range, is an error.
   *
   * The bound is **folded first**, so a `const` — or an expression over constants — stands wherever
   * a literal does, which is what makes `within 0..<max_tasks` and `[max_tasks]Task` one fact
   * rather than two (`reference/errors.md § Ranges`). Folding is the same `fold` an array bound and
   * an enum discriminant go through, so the three positions accept exactly the same expressions and
   * cannot drift apart. What does not fold is reported as not being a constant, at the bound,
   * rather than as a wrong *kind* — a name the program never declared is a different mistake from a
   * name that is not a number.
   */
  private def boundValue(e: Expr, base: Type): BigDecimal =
    fold(e) match
      case Some(folded) => boundLiteral(folded, base)
      case None =>
        err(s"a 'within' bound has to be a constant, and ${boundKind(e)} is not one — a literal, a " +
          "'const', or an expression over them")

  private def boundLiteral(e: Expr, base: Type): BigDecimal =
    Type.underlying(base) match
      case Type.Char =>
        e match
          case CharLit(cp) => BigDecimal(cp)
          case _           => err(s"a 'char' subtype needs character bounds, not ${boundKind(e)}")
      case i: Type.Integer =>
        val v = e match
          case IntLit(n, _)             => n
          case Unary("-", IntLit(n, _)) => -n
          case _                        => err(s"an integer subtype needs integer bounds, not ${boundKind(e)}")
        if !Type.fits(v, i) then err(s"the bound $v does not fit ${show(base)}")
        BigDecimal(v)
      case _ =>
        e match
          case IntLit(n, _)               => BigDecimal(n)
          case Unary("-", IntLit(n, _))   => BigDecimal(-n)
          case FloatLit(t, _)             => BigDecimal(t)
          case Unary("-", FloatLit(t, _)) => -BigDecimal(t)
          case _                          => err(s"a floating-point subtype needs numeric bounds, not ${boundKind(e)}")

  private def boundKind(e: Expr): String = e match
    case _: CharLit              => "a character"
    case _: FloatLit             => "a floating-point literal"
    case Unary("-", _: FloatLit) => "a floating-point literal"
    case _: IntLit               => "an integer"
    case Unary("-", _: IntLit)   => "an integer"
    case _: StrLit               => "a string"
    case _: BoolLit              => "a boolean"
    case Ident(n)                => s"'$n'"
    case _                       => "that"

  /** A trait named behind a memory-mode sigil, which is what makes the pointer a trait object
   * (`02`): `*Trait` raw and unmanaged, `&Trait` reference-counted. `None` for everything else,
   * including a type parameter that happens to be spelled like a trait — the substitution wins,
   * since that is what shadowing means everywhere else a name is resolved.
   */
}
