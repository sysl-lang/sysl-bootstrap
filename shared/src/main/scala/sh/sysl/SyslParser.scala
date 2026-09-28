package sh.sysl

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

/** The sysl parser: a packrat combinator grammar over the materialized token list from
 * `SyslLexical`.
 *
 * The grammar is split by area, each area a trait: `SyslParserBase` (the token reader, position
 * stamping, terminals, and the crossings between areas), `ExprParser` (the precedence ladder and
 * the literals), `TypeParser` (types as written, and generic parameter lists), `DeclParser`
 * (functions, structs, enums, traits, impls), `AttrParser` (the annotations above a declaration),
 * `HeaderParser` (the file header and imports), `PatternParser` (`match` and patterns), `StmtParser`
 * (one statement, the bindings, the contract clauses, inline assembly) and `ControlFlowParser` (the
 * six forms written like a statement and read like an expression).
 *
 * What is left here is the statement *list* — which is the one rule `recovering` reaches — the
 * program rule, and the entry points.
 *
 * The `List[Token]` is the reversibility seam: a hand-written parser could later consume the same
 * tokens with no change to the lexer.
 *
 * Every rule that builds a node wraps itself in `at`, which stamps the node with the extent of
 * everything the rule consumed — and with where a diagnostic about it should point, which a rule may
 * choose for itself and which is otherwise the same thing. A parser is bound to one `Source`, so
 * both stamps are complete — file, line, and column — the moment the node exists.
 *
 * `recovering` says this instance is the **second** parser over a file the first one refused, and
 * nothing but `statements` reads it. A recovering instance skips the lines it cannot read instead of
 * stopping at the first, which is what an editor needs and what a build must not have: the tree it
 * yields is missing whatever it skipped. It is a separate instance rather than a flag flipped
 * part-way through because packrat memoization is per instance, and a cache filled by the
 * non-recovering pass would answer the recovering one.
 *
 * `origin` says the source is a piece of another file — an interpolation's hole — and where in it
 * that piece begins, so every position this parser stamps names the enclosing file and the place
 * the reader wrote the text rather than a line of the piece.
 */
class SyslParser(val source: Source, val recovering: Boolean = false,
                 override val origin: Option[SourceOrigin] = None) extends ControlFlowParser {

  /** A whole file's — or a whole block's — worth of statements, in one of two spellings.
   *
   * The ordinary one is the `repsep`, and it is what every file that parses goes through. The
   * recovering one is reached **only on a second pass over a file the grammar has already refused**
   * (`SyslParser.recoveredParse`), and it is what lets a file report more than the first thing wrong
   * with it.
   *
   * **Which spelling is in force is fixed for the whole parser instance, and that is the point.**
   * `suite` is `newline ~> indent ~> statements <~ dedent`, so this one rule stands under every
   * block in the language — including the blocks parsed speculatively as one alternative among
   * several (`body`, `inlineBody | suite`). A recovering block *succeeds* where it used to fail, and
   * that failure is exactly what hands the position to the next alternative: recovering
   * unconditionally would silently re-decide which construct a well-formed file parses as. Confined
   * to a second pass, it cannot, because a file that parses never reaches one.
   */
  protected lazy val statements: PackratParser[List[Stmt]] =
    if recovering then recoveringStatements
    else skipNewlines ~> repsep(statement, newlines) <~ skipNewlines

  /** [[statements]] on the recovering pass: a statement that will not parse has its refusal recorded
   * and its line skipped, and the statements after it are read anyway.
   *
   * The unit of recovery is a **line at this block's own depth**. `Indent` and `Dedent` are counted
   * across a skip so that it can never leave the block it began in, and never swallows the `Dedent`
   * that ends it — a skip that escaped would take the rest of an enclosing declaration with it and
   * report the damage somewhere the reader never looked.
   *
   * A failed statement is **dropped rather than replaced**. An error node in the tree would need a
   * case in `AstCodec` and a version with it, an arm in the analyzer and an arm in every walk, for
   * something that can never reach an artifact or codegen: a file with a parse error does not build.
   * What it costs is that a name the dropped line bound is undefined below it, which is why the
   * diagnostic path does not analyze a recovered tree — `Compiler.checked` reports these and stops.
   */
  private lazy val recoveringStatements: PackratParser[List[Stmt]] = memo(Parser { start =>
    val stmts = ListBuffer.empty[Stmt]
    var in    = blankLines(start)

    while (!endOfBlock(in)) {
      val before = mark(in)

      statement(in) match {
        // A statement is followed by a line break or by the end of its block. Anything else means
        // the grammar's longest match stopped part-way along the line — and **what it matched is
        // dropped with the rest of the line**, which is the one judgement call in here.
        //
        // The reason is that a partial match is not a smaller version of what the reader wrote, it
        // is a different program: `print(2 3)` matches `print` alone, and keeping that would put a
        // bare `print` in the tree as though the file called it with nothing. A missing statement is
        // an absence an editor can see; a fabricated one is an answer it cannot tell from a real
        // one. What is paid for it is the complete statement in `print(1) print(2)`, which goes too.
        case Success(s, next) if endOfBlock(next) || next.first == lexical.Newline =>
          stmts += s
          in = next
        case Success(_, next) =>
          note("newline expected", next)
          in = toNextLine(in)
        case ns: NoSuccess =>
          note(ns.msg, ns.next)
          in = toNextLine(in)
      }

      in = blankLines(in)

      // A skip with nowhere to go leaves the reader where it was, and a loop that does not advance
      // does not end. One token is always progress.
      if mark(in) == before then in = in.rest
    }

    Success(stmts.toList, in)
  })

  /** Where the reader is, as the pair that says whether anything was consumed. Two distinct tokens
   * never share a position, so equality here is "the reader did not move".
   */
  private def mark(in: Input): (Int, Int) = (in.pos.line, in.pos.column)

  /** That there are no more statements to read at this depth — the `Dedent` closing this block, or
   * the end of the file. Neither is consumed: `suite` is owed its `Dedent` and `phrase` its end.
   */
  private def endOfBlock(in: Input): Boolean =
    in.atEnd || in.first == lexical.Dedent || in.first == lexical.EOF

  private def blankLines(in: Input): Input = {
    var r = in

    while (!r.atEnd && r.first == lexical.Newline) r = r.rest

    r
  }

  /** Past the next line break at **this** depth, leaving the reader at the start of the next
   * statement of this block.
   *
   * The depth count is the whole of it: a `struct` header that will not parse is followed by the
   * indented body it was going to have, and skipping "to the next newline" without counting would
   * stop inside that body and try to read a field as a statement. Counting, the body goes with the
   * header it belongs to, and the `Dedent` that closes the *enclosing* block still stops the skip.
   *
   * **The block a line opens goes with it**, which is the second half of the same idea. A skip that
   * stopped at the line break would leave the reader on the `Indent` of a body whose header it has
   * just thrown away, and every line of that body would then be reported as a statement that is not
   * one. One unreadable `if` is worth one diagnostic, not one per line underneath it.
   */
  private def toNextLine(in: Input): Input = {
    var r     = in
    var depth = 0
    var going = true

    while (going && !r.atEnd)
      r.first match {
        case lexical.Indent => depth += 1; r = r.rest
        case lexical.Dedent =>
          if depth == 0 then going = false
          else { depth -= 1; r = r.rest }
        case lexical.Newline =>
          r = r.rest
          if depth == 0 then going = false
        case _ => r = r.rest
      }

    if !r.atEnd && r.first == lexical.Indent then pastBlock(r) else r
  }

  /** Past a balanced `Indent` … `Dedent`, and past the line break that closes it. */
  private def pastBlock(in: Input): Input = {
    var r     = in.rest
    var depth = 1

    while (depth > 0 && !r.atEnd) {
      r.first match {
        case lexical.Indent => depth += 1
        case lexical.Dedent => depth -= 1
        case _              => ()
      }

      r = r.rest
    }

    blankLines(r)
  }

  private def note(message: String, in: Input): Unit =
    skipped += Diagnostic(message, Some(SyslParser.failedAt(source, in.pos)))

  private val skipped = ListBuffer.empty[Diagnostic]

  /** What the recovering pass skipped, in source order and without repeats.
   *
   * The de-duplication is not tidiness. Packrat memoization means a position may be *reached* more
   * than once and answered from the cache, and an alternative that recorded a refusal before being
   * abandoned has still recorded it — so the same complaint about the same token can arrive twice
   * by two roads. A reader is owed it once.
   */
  def skippedProblems: List[Diagnostic] =
    skipped.toList
      .distinctBy(d => (d.message, d.pos.map(p => (p.line, p.col))))
      .sortBy(d => (d.pos.map(_.line).getOrElse(0), d.pos.map(_.col).getOrElse(0)))

  /** A file: an optional module header, the clauses that go with it, then its statements. A file
   * with no header contributes to the anonymous root module, which is what lets a one-file program
   * be written with no ceremony — and it may still narrow itself or name a library, since the root
   * module is a module like any other.
   */
  protected lazy val program: PackratParser[Program] =
    skipNewlines ~> reservedFuncHead ~> maybe(moduleHeader) >> { m =>
      // An attribute goes on a line of its own, which is what both `reference/modules.md §
      // Capabilities are a module property` and `reference/modules.md § Capabilities are a module
      // property` show and what keeps `module m
      // @no_alloc @requires(os)` from being a line anyone has to read. The exception is a file that
      // declares no module: the root module is a module like any other, and there is no header for
      // its attributes to sit below, so there they may open the file.
      val lead = if m.isDefined then success(List.empty[HeaderClause])
                 else maybe(headerAttr) ^^ (_.toList.flatten)

      lead ~ repeatedly(asOneToken(newlines ~> headerAttr)) ~ statements ^^ {
        case first ~ rest ~ body =>
          val clauses = first ::: rest.flatten

          Program(body, m,
                  clauses.collect { case c: CapabilityClause => c },
                  clauses.collect { case l: LinkClause => l },
                  source,
                  clauses.exists(_.isInstanceOf[TestsClause]),
                  clauses.collect { case i: IncludeClause => i },
                  // The lexer collected these while scanning, so they are read off it rather than
                  // parsed: a comment reaches no grammar rule, which is what keeps it trivia.
                  DocComments.of(source, lexical.docComments))
      }
    }

  // --- entry points --------------------------------------------------------------------

  /** Parses this parser's source as a single expression (used by the expression test tier). */
  def parseExpression: ParseResult[Expr] =
    phrase(expression <~ rep(newline))(reader(source.text))

  /** Parses this parser's source as a whole program. */
  def parseProgram: ParseResult[Program] =
    phrase(program)(reader(source.text))

  /** The first token the lexer could make nothing of, and where it sits.
   *
   * A lexical error is reported ahead of whatever the grammar made of the tokens around it, because
   * the parser's expectation is a *reaction* to the damage rather than a description of it: an
   * unterminated string literal is a token the grammar has no rule for, so the failure surfaces
   * wherever the longest partial match happened to stop — for `print("oops`, a complaint about the
   * argument list, having taken `print` alone as much as it could read. The lexer already knew the
   * answer and said so; this is what carries it out.
   */
  def firstLexicalError: Option[(String, Position)] =
    lexical.scanPositioned(source.text).collectFirst { case (lexical.ErrorToken(msg), at, _) => (msg, at) }
}

object SyslParser {

  /** Parses a program, returning either the AST or a rendered diagnostic. */
  def parse(src: String, name: String = "<input>"): Either[String, Program] =
    parse(Source(name, src))

  /** Parses a file for a target.
   *
   * **The target reaches the parser because conditional compilation does** (`Conditional`): the
   * lines of a branch this build is not for are blanked before the lexer is handed anything, so what
   * is parsed is already the file this target sees. Every other seam is unaffected — the gate is not
   * applied to an expression fragment or to a string interpolation's contents, neither of which is a
   * file, and a source with no directives in it is not even copied.
   *
   * **Tangling comes first, and the order is the whole of what makes the two features independent.**
   * A literate file's directives are written in its program, which is indented (`Literate`) — so
   * gating first would be gating a file whose `#if` lines are four columns in, and `Conditional`
   * would have to learn about a format that is none of its business. Tangled first, what the gate
   * receives is ordinary sysl and it behaves exactly as it does for a file that was never literate.
   */
  def parse(source: Source, target: Target): Either[String, Program] =
    checked(source, target).left.map(Diagnostic.report)

  /** The same parse, answering its refusal as **data** rather than as a paragraph.
   *
   * It is what the compiler itself uses, and `parse` is one line on top of it — a caller that wants
   * the text has always wanted exactly `Diagnostic.report`'s text, and the two spellings exist so
   * that adding this broke nothing. `api.Sysl.check` is the published end of this road.
   *
   * A file that will not parse is parsed a **second** time, recovering, so the list holds every
   * line the grammar could not read rather than only the first. The tree that second pass builds is
   * not returned here and is not compiled — it is missing whatever it skipped, so a name a skipped
   * line bound is undefined below it and analyzing it would invent a cascade. `recovered` is the
   * entry point for a caller that wants that tree anyway.
   */
  def checked(source: Source, target: Target = Target.default): Either[List[Diagnostic], Program] =
    Literate.tangled(source).left.map(List(_))
      .flatMap(Conditional.gated(_, target).left.map(List(_)))
      .flatMap(parsed)

  /** The same parse, keeping the **tree it managed to build** beside the diagnostics.
   *
   * This is what an editor asks for and what nothing in a build has any use for: a file being typed
   * in is a file that does not parse, and a server with no tree has nothing to answer hover,
   * go-to-definition or an expanding selection with. The tree is partial by construction — every
   * line the recovering pass skipped is simply not in it — which is exactly why this is a second
   * entry point rather than a wider return type on `checked`. A build must not be able to reach it
   * by accident.
   *
   * `None` means there is not even a partial tree: a lexical error, a literate or conditional file
   * that could not be prepared, or a recovering pass that itself failed.
   */
  def recovered(source: Source, target: Target = Target.default): (Option[Program], List[Diagnostic]) =
    Literate.tangled(source) match
      case Left(d)     => (None, List(d))
      case Right(text) =>
        Conditional.gated(text, target) match
          case Left(d)     => (None, List(d))
          case Right(gate) => recoveredParse(gate)

  /** The same, for the machine a caller that names none would get. Spelled as its own overload
   * rather than as a default argument because only one of these alternatives may carry defaults, and
   * that one is the `String` form the parser tests are written against.
   */
  def parse(source: Source): Either[String, Program] = parse(source, Target.default)

  private def parsed(source: Source): Either[List[Diagnostic], Program] =
    recoveredParse(source) match {
      // Only a file that parsed with nothing to say is handed on. A recovered tree has holes in it
      // where the lines it could not read used to be, and compiling one would be compiling a
      // program nobody wrote.
      case (Some(prog), Nil) => Right(prog)
      case (_, problems)     => Left(problems)
    }

  /** One parse, then — if it failed — a second, recovering one, answering with whatever tree came
   * out and every diagnostic either pass has to give.
   *
   * **The ordinary pass is unchanged and runs first, which is the whole of the safety argument.** A
   * file that parses is answered by it, token for token, and never reaches the recovering parser at
   * all; recovery cannot therefore change what any working program means.
   *
   * A **lexical** error short-circuits both. The token stream is damaged at that point, so skipping
   * lines through it would be reporting the grammar's confusion about tokens the lexer has already
   * explained — `firstLexicalError` says why it outranks the grammar's own complaint.
   *
   * **THE FIRST PASS'S DIAGNOSTIC LEADS, ALWAYS, AND RECOVERY ONLY APPENDS WHAT COMES AFTER IT.**
   * This is not a tie-break, it is the point at which recovery would otherwise make the compiler
   * worse. The ordinary grammar reports the *furthest* point it reached and names something the
   * reader could have written there — the whole subject of `ParseDiagnosticTests`. The recovering
   * loop knows only that a line would not parse, so where it stops matters much less: for
   * `print(1 2)` it says `newline expected` at the `1`, where the grammar says `')' expected` at the
   * `2`. Letting the second pass answer for the first mistake replaced a tuned message with a vague
   * one, in eleven cases the suite already pins.
   *
   * So what recovery adds is the mistakes *below* the first, which is exactly what it was for, and
   * anything it has to say at or before the first mistake is the same mistake reached by a worse
   * road. This can therefore never be quieter, or vaguer, than the parser was before it existed.
   */
  private def recoveredParse(source: Source): (Option[Program], List[Diagnostic]) = {
    val p = new SyslParser(source)

    p.parseProgram match {
      case p.Success(prog, _) => (Some(prog), Nil)
      case ns: p.NoSuccess =>
        val first = p.firstLexicalError match {
          case Some((msg, at)) => Diagnostic(msg, Some(Pos(source, at.line, at.column)))
          case None            => Diagnostic(ns.msg, Some(failedAt(source, ns.next.pos)))
        }

        if p.firstLexicalError.isDefined then (None, List(first))
        else {
          val r = new SyslParser(source, recovering = true)

          r.parseProgram match {
            case r.Success(prog, _) => (Some(prog), first :: r.skippedProblems.filter(below(first, _)))
            case _                  => (None, List(first))
          }
        }
    }
  }

  /** That `later` sits strictly further into the file than `first` — what decides whether a
   * recovered diagnostic is a *second* mistake or the first one described again.
   *
   * A diagnostic with no position is treated as not below anything: it cannot be shown to be a
   * separate mistake, and duplicating the one already reported is the worse of the two errors.
   */
  private def below(first: Diagnostic, later: Diagnostic): Boolean =
    (first.pos, later.pos) match
      case (Some(a), Some(b)) => b.line > a.line || (b.line == a.line && b.col > a.col)
      case _                  => false

  /** Where a parse failed. Running out of tokens leaves no position at all, and pointing at the
   * end of the last line is more use than pointing at nothing — an unclosed block is exactly the
   * case that reports there.
   *
   * A failure that stopped *at* a token carries that token's span, so `identifier expected`
   * underlines the token that is not one rather than its first character. Running out of input has
   * no token and so no extent — and the reader answers line zero for it deliberately
   * (`TokenPos.after`), which is what sends it to the last branch here.
   */
  protected def failedAt(source: Source, at: scala.util.parsing.input.Position): Pos = at match {
    case p: TokenPos if p.line > 0 => p.toPos
    case p if p.line > 0           => Pos(source, p.line, p.column)
    case _                         =>
      val last = math.max(1, source.lines.length)

      Pos(source, last, source.line(last).length + 1)
  }
}
