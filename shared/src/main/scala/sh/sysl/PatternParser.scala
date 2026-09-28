package sh.sysl

/** `match`, its arms, and the patterns they are written with.
 *
 * One area rather than two because a pattern has no other home: it is written in a `match` arm, in
 * an `is` condition, and in an irrefutable binding, and those must be the same grammar rather than
 * three that drift (`reference/expressions.md § is — a pattern where a condition is wanted`).
 */
trait PatternParser extends ExprParser {

  // --- match ---------------------------------------------------------------------------

  /** `scrutinee match` followed by an indented list of `pattern[, pattern…] [if guard] -> body`
   * arms — an expression yielding the taken arm's value.
   *
   * The keyword goes **after** the value, as in Scala, and for Scala's reason: a match is a
   * *transformation of the thing to its left*, so writing it there is what lets one feed another.
   * `a match … match …` reads in the order the values flow, where the prefix form would have made
   * the second one wrap the first and put the arms of each at a different distance from the value
   * they choose between.
   *
   * It is therefore repeated rather than optional, and left-associative — each arms block matches
   * whatever the chain has produced so far. An operand with no `match` after it is returned
   * unchanged, which is how this doubles as the fall-through from `expression` to `assignment`.
   */
  protected lazy val matchExpr: PackratParser[Expr] =
    assignment ~ rep(onNextLine(op("match")) ~>
      (newline ~> indent ~> skipNewlines ~> rep1sep(matchArm, newlines) <~ skipNewlines <~ dedent)) ^^ {
      case scrut ~ chain => chain.foldLeft(scrut)((e, arms) => MatchExpr(e, arms).setPos(e.pos))
    }

  /** One arm. The `->` separates a *pattern* from what to do when it matches, and `else` is not a
   * pattern — it is the fallback, and it takes its body directly, exactly as an `if`'s `else` does.
   *
   * The arrow after `else` is called out by name rather than left to fail as an expression, since
   * what it fails as is a body that does not start with a `->` and the position that reports is
   * the `match` several lines above.
   */
  protected lazy val matchArm: Parser[MatchArm] =
    at(
      op("else") ~> op("->") ~> err("the 'else' arm takes its body directly, with no '->' — 'else' names no pattern to separate one from") |
        op("else") ~> (suite | inlineBody) ^^ (b => MatchArm(List(WildcardPattern), None, b)) |
        rep1sep(pattern, op("|")) ~ opt(op("if") ~> expression) ~ (op("->") ~> (suite | inlineBody)) ^^ {
          case pats ~ guard ~ b => MatchArm(pats, guard, b)
        },
    )

  /** Patterns: scalar literals and ranges, the `_` wildcard, a positional destructuring
   * `V(sub…)` (an enum variant or a struct), a named struct destructuring `S{field: sub…}`, or a
   * bare name — which the analyzer reads as a nullary-variant pattern when it names a variant of
   * the scrutinee's enum, and as a binding otherwise. A name may be qualified wherever a variant
   * may be, which is every form: a nullary variant is spelled like a name and reached like one.
   */
  /** A pattern, with the **binding** form read first so that the name before an `@` is not taken as
   * a pattern in its own right. Everything after the `@` is an ordinary pattern, which is what makes
   * the form nest: `outer @ Wrap(inner @ Val(v))` is two of these.
   */
  override protected lazy val pattern: Parser[Pattern] =
    describe("a pattern")(bindPattern | unboundPattern)

  /** `n @ pat` — the value bound whole, and taken apart, in one pattern.
   *
   * The name is an `ident` rather than a `qualifiedName`: what a binding introduces is a local, and
   * a name with a dot in it is not a name a program can declare. A qualified one before an `@` is
   * therefore a mistake about what is being bound rather than a pattern the grammar could go on to
   * read, and it is answered as one.
   */
  protected lazy val bindPattern: Parser[Pattern] =
    (ident <~ op("@")) ~ unboundPattern ^^ { case n ~ p => BindPattern(n, p) } |
      (qualifiedName <~ guard(op("@"))) >> (n =>
        err(s"'$n' has a dot in it, and what a binding introduces is a local — write a name a " +
          "program can declare"))

  private lazy val unboundPattern: Parser[Pattern] =
    noImplicitMember |
      patternLit ~ (rangeOp ~ patternLit) ^^ { case lo ~ (inc ~ hi) => RangePattern(lo, hi, inc) } |
      structPattern |
      variantPattern |
      tuplePattern |
      wildcard ^^^ WildcardPattern |
      quotedRef ^^ EqPattern.apply |
      qualifiedName ^^ IdentPattern.apply |
      patternLit ^^ LitPattern.apply |
      reservedPatternName ^^ IdentPattern.apply

  /** A reserved word written where a pattern binds a name — `Some(loop) -> …`, `loop @ Some(x)`,
   * `Point{x, loop}` — refused as the word it is rather than as `a pattern expected`.
   *
   * It is the last alternative, so every pattern that may begin with a reserved word (`true`,
   * `false`) has already been read, and it excludes the two value words that are not patterns at
   * all (`null`, `self`), which are not names anybody meant to bind. The lookahead is what may follow
   * a sub-pattern, since `else` opens a match arm the same way a pattern does and is read above this.
   */
  private lazy val reservedPatternName: Parser[String] =
    not(op("null") | op("self")) ~> reservedName("a name a pattern binds", patternFollow)

  private lazy val patternFollow: Parser[Any] =
    op(")") | op(",") | op("}") | op("->") | op("|") | op("@") | op("if") | op("=") | op("in")

  /** A backtick-quoted name in pattern position, `` `limit` `` or `` sdl.`SCANCODE_A` `` — a
   * reference rather than a binding.
   *
   * It sits above the bare-name alternative because the two overlap: `ident` takes either spelling,
   * so a quoted name would otherwise be read as an `IdentPattern` and bind. What distinguishes them
   * is the *final* segment, since that is the one naming the thing — a qualifier is dropped by the
   * analyzer either way.
   */
  private lazy val quotedRef: Parser[String] =
    rep(ident <~ op(".")) ~ quotedIdent ^^ { case ps ~ n => (ps :+ n).mkString(".") }

  /** A leading dot written in a pattern, which the expression form has and a pattern does not need
   * (`reference/expressions.md § A leading dot`).
   *
   * A pattern is already matched *against* a type, and the analyzer reads a bare name against the
   * scrutinee's enum and drops any qualifier it was written with — so `red` here already means what
   * `.red` means one line up, and the dot would be a second spelling of the same thing. Refused by
   * name rather than left to "a pattern expected", which is what a reader arriving from the
   * expression form would otherwise get for writing the form they had just been taught.
   *
   * Nothing else in the grammar begins a pattern with a dot, so consuming one commits.
   */
  private lazy val noImplicitMember: Parser[Nothing] =
    op(".") ~> ident >> (n =>
      err(s"a pattern is matched against a type it already knows, so the variant is written '$n' " +
        "with no leading dot — the dot is for an expression, where the type comes from what the " +
        "context expects"))

  protected lazy val variantPattern: Parser[Pattern] =
    qualifiedName ~ (op("(") ~> commaList(pattern) <~ op(")")) ^^ { case n ~ ps => VariantPattern(n, ps) }

  /** `S{field: sub, other}` — a named struct pattern. A bare `field` is shorthand for
   * `field: field`, binding the field to a variable of the same name.
   */
  protected lazy val structPattern: Parser[Pattern] =
    qualifiedName ~ (op("{") ~> commaList(fieldPattern) <~ op("}")) ^^ { case n ~ fs => StructPattern(n, fs) }

  /** `(a, b)` — a tuple pattern. Two or more sub-patterns for the same reason the type takes two
   * or more parts, and a single one is refused where the type refuses it rather than being read as
   * a pattern in parentheses, which sysl has no use for.
   */
  protected lazy val tuplePattern: Parser[Pattern] =
    (op("(") ~> commaList1(pattern) <~ op(")")) >> {
      case List(_) => err("a tuple pattern matches two or more parts — one part is not a tuple")
      case parts   => success(TuplePattern(parts))
    }

  protected lazy val fieldPattern: Parser[(String, Pattern)] =
    (reservedName("a name a pattern binds", op(",") | op("}")) | ident) ~ opt(op(":") ~> pattern) ^^ {
      case n ~ p => (n, p.getOrElse(IdentPattern(n)))
    }

  /** A pattern literal: any scalar literal, or a negated numeric literal. */
  protected lazy val patternLit: Parser[Expr] =
    op("-") ~> (floatLit | intLit) ^^ (e => Unary("-", e)) |
      floatLit | intLit | charLit | strLit | boolLit
}
