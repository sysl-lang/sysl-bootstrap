package sh.sysl

/** Source locations and the rendering of a diagnostic against them.
 *
 * The lexer has always known where every token is; what was missing was a way to carry that
 * knowledge as far as the pass that finds the mistake. A `Pos` is that carrier, and because it
 * holds the `Source` it came from, a diagnostic renders itself — no pass has to thread the
 * source text alongside the message just to quote a line.
 */

/** A named source text: the unit a `Pos` points into. Sources are compared by identity, so the
 * library and the user's file stay distinct even though a program is made of both, and a
 * diagnostic against a library declaration quotes the library's own file rather than the wrong one.
 *
 * `dir` is where the file sits, as the directory segments between the project root and it — so a
 * file at `std/fs/read.sysl` carries `["std", "fs"]` and one at the root carries `Nil`. A module is
 * a directory (`reference/modules.md`), which makes this the module the file's header has to agree
 * with, and it is the driver that knows it: `None` says the file was handed over with no project
 * around it, and the header is then the whole of what says which module the file is in.
 *
 * `columnOffset` is how many columns were taken off the front of every line to make this text, which
 * is zero for a file the compiler was handed as it was written and four for the program inside a
 * literate one (`Literate`). It is added back wherever a position is *reported*, so that a location
 * names the column of the file the reader has open rather than of the text the lexer saw.
 *
 * `features` is what the package this file belongs to has enabled, which `Conditional` turns into
 * `feature_<name>` symbols the file may be gated on. Empty for a file handed over with no package
 * graph around it, which is every compilation that resolves nothing.
 */
final class Source(val name: String, val text: String, val dir: Option[List[String]] = None,
                   val columnOffset: Int = 0, val features: Set[String] = Set.empty) {

  /** The same file, belonging to a package with these features enabled.
   *
   * The set travels on the file rather than on the compilation because a compilation holds the files
   * of several packages at once — a program's own, a dependency's, a `--lib` root's — and each of
   * them is gated against **its own** manifest (`Conditional.defined`). A parameter one level up
   * could not tell them apart, and one package's features leaking into another's source would gate
   * code in or out on the strength of a name its author never declared.
   */
  def enabling(names: Set[String]): Source =
    if names.isEmpty then this else new Source(name, text, dir, columnOffset, names)

  /** The text split into lines, kept for the one line a diagnostic quotes. Splitting with a
   * negative limit keeps a trailing empty line, so line numbers stay 1:1 with the file.
   */
  lazy val lines: Vector[String] = text.split("\n", -1).toVector

  /** Line `n`, 1-based, or the empty string when there is no such line — a position past the
   * end (an unexpected EOF) still renders, it just has nothing to quote.
   */
  def line(n: Int): String =
    if n >= 1 && n <= lines.length then lines(n - 1).stripSuffix("\r") else ""

  /** The offset into `text` at which each line begins, so that an offset can be turned into a line
   * and a column without walking the text.
   *
   * The lexer knows where a token *ends* as an offset and nothing else — it is what the character
   * reader beneath it counts — while everything that reports a position speaks in lines and
   * columns. One of the two has to be converted, and this is the cheap direction.
   */
  lazy val lineStarts: Vector[Int] =
    0 +: text.indices.view.filter(text.charAt(_) == '\n').map(_ + 1).toVector

  /** The line and the column, both 1-based, of an offset into `text`.
   *
   * An offset past the end answers the place just past the last character, which is where a token
   * that runs to the end of input ends.
   */
  def placeOf(offset: Int): (Int, Int) = {
    val at = math.max(0, math.min(offset, text.length))
    var lo = 0
    var hi = lineStarts.length - 1

    while lo < hi do
      val mid = (lo + hi + 1) / 2

      if lineStarts(mid) <= at then lo = mid else hi = mid - 1

    (lo + 1, at - lineStarts(lo) + 1)
  }

  override def toString: String = name
}

object Source {
  def apply(name: String, text: String): Source = new Source(name, text)

  /** A file the driver read out of a project, carrying the directory it was found in. */
  def apply(name: String, text: String, dir: List[String]): Source = new Source(name, text, Some(dir))
}

/** A span of a source file: where something starts, and where it ends.
 *
 * `line` and `col` are 1-based, as the lexer counts them, and `endLine`/`endCol` name the place
 * **just past** the last character — so the width of a span on one line is `endCol - col`, and a
 * span whose end equals its start has no extent at all. The exclusive end is what an editor
 * publishes and what makes the arithmetic come out without a `+ 1` at every use.
 *
 * **What a span covers depends on which of a node's two it is**, and `Positioned` is where that is
 * settled: a node's `pos` is where a complaint about it belongs, and its `extent` is the whole of
 * what it was parsed from. They agree for a node built from one token, which is most of what a
 * diagnostic points at, and part ways as soon as a rule anchors its node inside itself — `xs.foo(1)`
 * points at `foo` and covers all of `xs.foo(1)`.
 *
 * A position built from a line and a column alone — a literate file's margin, a conditional
 * directive, a parse that ran out of input — has no token to measure and carries no extent.
 */
final case class Pos(source: Source, line: Int, col: Int, endLine: Int, endCol: Int) {

  /** Where this is, as a compiler conventionally spells it: `file.sysl:12:7`.
   *
   * The column is the one in the **file**, which is not the one the lexer counted when the text it
   * lexed had its left margin removed (`Source.columnOffset`). An editor told to go to a location
   * goes to the file, so the file's column is the one worth giving.
   */
  def location: String = s"${source.name}:$line:${col + source.columnOffset}"

  /** This span carried out to `line`/`col`, or left alone where that would shorten it.
   *
   * Widening is the only direction, and the guard is what makes it safe to ask for an end that may
   * not be one. A rule that consumed nothing ends *before* it began, and one that consumed a single
   * token ends exactly where that token already does — so both leave the span as it stands rather
   * than collapse it to no extent at all.
   */
  def endingAt(line: Int, col: Int): Pos =
    if line > endLine || (line == endLine && col >= endCol) then copy(endLine = line, endCol = col)
    else this

  /** The message, the location, and the offending line with the token underlined:
   *
   * {{{
   * error: 'b' of 'add' is int, but string was given
   *  --> hello.sysl:7:14
   *   |
   * 7 | print(add(x, "two"))
   *   |              ^^^^^
   * }}}
   *
   * The underline's indent is built from the line's own leading characters with everything but a
   * tab replaced by a space, so it lands under the right column whichever mix of tabs and spaces
   * the line was written with.
   *
   * A span that runs past the end of its first line — a text block, an unterminated literal — is
   * underlined to the end of that line and no further: the quote shows one line, so an underline
   * longer than it would be pointing at nothing. A span with no extent gets a single caret, which
   * is what every diagnostic looked like before spans existed.
   */
  def render(msg: String, severity: Severity = Severity.Error): String = {
    val number = line.toString
    val gutter = " " * number.length
    val text   = source.line(line)
    val column = math.max(1, math.min(col, text.length + 1))
    val indent = text.take(column - 1).map(c => if c == '\t' then '\t' else ' ')
    val past   = if endLine == line then math.min(endCol, text.length + 1) else text.length + 1
    val width  = math.max(1, past - column)

    val label = severity match
      case Severity.Error   => "error"
      case Severity.Warning => "warning"

    List(
      s"$label: $msg",
      s"$gutter--> $location",
      s"$gutter |",
      s"$number | $text",
      s"$gutter | $indent${"^" * width}",
    ).mkString("\n")
  }
}

object Pos {

  /** A place rather than a span, for a caller that knows a line and a column and has no token to
   * measure. Its end is its start, so it renders with one caret.
   */
  def apply(source: Source, line: Int, col: Int): Pos = Pos(source, line, col, line, col)
}

/** Something that came from a place in a source file.
 *
 * The position is a **mutable field rather than a constructor parameter** on purpose: every AST
 * node is a case class, and a position in the constructor would put it into `equals`, which
 * would mean no two trees built from different files could ever compare equal and every
 * structural test would have to spell out positions it does not care about. Keeping it out of
 * the case-class signature leaves `Binary("+", a, b) == Binary("+", a, b)` true, exactly as the
 * parser tests assume.
 *
 * `setPos` **keeps the first position it is given**. Parsing builds bottom-up, so the innermost
 * rule to claim a node is the most specific one that could: a `.field` tail sets the position of
 * the dot, and the enclosing expression rule, which would have set the start of the whole
 * expression, then leaves it alone.
 */
trait Positioned {
  private var current: Option[Pos] = None
  private var parsed: Option[Pos]  = None

  /** Where a diagnostic about this node should point, which is **not** always where the node
   * begins. A grammar rule may anchor a node somewhere inside itself — a call at its callee, a
   * field selection at the member name rather than at the dot — because that is where the reader's
   * attention belongs when something is wrong with it.
   */
  def pos: Option[Pos] = current

  /** The whole of what this node was parsed from, from its first token to its last.
   *
   * This is the question an editor asks — which construct is the cursor inside, what does a
   * selection expand to, what does hovering cover — and it is a different question from where a
   * complaint belongs. `xs.foo(1)` points a diagnostic at `foo` and covers `xs.foo(1)`, and both
   * answers are right for what they are for.
   *
   * It falls back to `pos` for a node nothing parsed: the analyzer synthesizes plenty, and a
   * desugared node's extent is honestly the position it was given.
   */
  def extent: Option[Pos] = if parsed.isEmpty then current else parsed

  def setPos(p: Pos): this.type = {
    if current.isEmpty then current = Some(p)
    this
  }

  def setPos(p: Option[Pos]): this.type = {
    if current.isEmpty then current = p
    this
  }

  /** Records what the node was parsed from — the first such claim only, exactly as `setPos` keeps
   * the first position. An outer rule that merely passed the node through consumed the same tokens
   * and would say the same thing; one that consumed *more* built a node of its own, and this one is
   * a part of it rather than the whole.
   */
  def setExtent(p: Pos): this.type = {
    if parsed.isEmpty then parsed = Some(p)
    this
  }
}

/** One thing wrong with a program: what is wrong, and where.
 *
 * **A diagnostic is carried rather than rendered**, and that is the whole reason this is a type. A
 * pass that finds a mistake has the message and the position in hand, and until this existed it
 * turned them into text on that same line — so by the time anything else could look, the only thing
 * left was a paragraph. Rendering happens once, at the edge, in `render` and `report`.
 *
 * What that buys is a caller that wants to do something other than print: an editor wanting a range
 * to underline, a build tool wanting to group by file, a test wanting to assert on the position
 * without matching text. `api.Sysl.check` is the published form of it.
 *
 * `pos` is absent for a rule that fires away from any one node — a synthesized declaration, a
 * whole-program check — and that is a reason to say less rather than to say nothing.
 *
 * **`severity` arrived with the first positioned warning** (card `0372`), which is the condition
 * this comment used to name for adding it: until then every diagnostic here was an error, the
 * compiler's only two warnings were the driver's — written straight to standard error, carrying no
 * position — and a field with one inhabitant is a field a reader stops to wonder about.
 *
 * **A warning does not fail a compilation and an error does, and that is the whole of the
 * difference here.** Everything else about the two is the same: both carry a position, both are
 * ordered by it, both reach `api.Sysl.check` as data. What must not follow is a *level* — an
 * argument about which warnings are errors is one this compiler has no reason to have while it has
 * one warning.
 */
final case class Diagnostic(message: String, pos: Option[Pos], severity: Severity = Severity.Error) {

  /** This one on its own, as a reader sees it. */
  def rendered: String = Diagnostic.render(message, pos, severity)

  /** Whether this one stops the compilation. */
  def isError: Boolean = severity == Severity.Error
}

/** Whether a diagnostic stops the compilation.
 *
 * Two values and no ordering between them: a warning is not a lesser error, it is a different claim
 * — *this compiles and is probably not what you meant* — and the day it becomes worth promoting one
 * to an error is the day this grows a third thing to be, not a comparison.
 */
enum Severity:
  case Error, Warning

object Diagnostic {

  /** How many errors one compilation reports.
   *
   * Five is already more than anyone fixes before compiling again, and the further down a broken
   * file a diagnostic is, the likelier it is to be a consequence of one further up rather than a
   * mistake of its own. A wall of them is not more information; it is the first five with the
   * signal-to-noise falling off behind them.
   *
   * **It is the renderer's rule and not the list's.** A caller reading diagnostics as data wants
   * all of them — an editor underlines every mistake in the file, and five would leave the rest
   * unmarked — so nothing truncates until `report` does.
   */
  val limit: Int = 5

  /** A message rendered against a position when there is one, and on its own when there is not —
   * a synthesized node (the library's, a desugaring's) may have no place to point at, and that
   * is a reason to say less rather than to say nothing.
   */
  def render(msg: String, pos: Option[Pos], severity: Severity = Severity.Error): String = {
    val label = severity match
      case Severity.Error   => "error"
      case Severity.Warning => "warning"

    pos.map(_.render(msg, severity)).getOrElse(s"$label: $msg")
  }

  /** A note rather than a complaint: the location first, in the one-line form a build log wants,
   * since these are printed by the handful and not read one at a time.
   */
  def explain(msg: String, pos: Option[Pos]): String =
    pos.map(p => s"${p.location}: $msg").getOrElse(msg)

  /** Assembles the diagnostics of one compilation: at most `limit` of them, separated by a blank
   * line, with a closing count when there were more. The ones kept are the *first* — the list
   * arrives in source order, and an error early in a file is the one worth reading.
   */
  def report(all: List[Diagnostic]): String = {
    val shown = all.take(limit).map(_.rendered)

    if all.length <= limit then shown.mkString("\n\n")
    else (shown :+ s"showing the first $limit of ${all.length} errors").mkString("\n\n")
  }

  /** The order a reader reads a file in: by source, then by line, then by column. A diagnostic with
   * no position sorts last, since there is nowhere to file it.
   *
   * Every pass that collects more than one owes this, so it lives here rather than in whichever
   * pass happened to need it first.
   */
  def inSourceOrder(all: List[Diagnostic]): List[Diagnostic] =
    all.sortBy(d =>
      (d.pos.isEmpty, d.pos.map(_.source.name).getOrElse(""), d.pos.map(_.line).getOrElse(0),
       d.pos.map(_.col).getOrElse(0)),
    )
}
