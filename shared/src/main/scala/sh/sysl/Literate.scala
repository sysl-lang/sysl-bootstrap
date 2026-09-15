package sh.sysl

/** Literate sysl: a source file that is a **Markdown document whose indented blocks are the
 * program**.
 *
 * Knuth's argument for WEB was that a program is written to be read by a person and only
 * incidentally by a machine, and that the order and the explanation a reader needs are not the ones
 * a compiler needs. This is the same idea at a much smaller scale: there is no reordering and no
 * macro layer — the code appears in the order it runs in — and what a `.lsysl` file adds over a
 * `.sysl` file is room for prose between the blocks, in a format that is already readable unrendered
 * and already renderable by anything that reads Markdown.
 *
 * **The rule is the indent, and nothing else.** A line indented four spaces or more is program text;
 * every other line is prose and is not compiled. Nothing marks where a block begins or ends, there
 * is no name to declare and no chunk to reference, and a file with no prose in it is a `.sysl` file
 * with four spaces in front of every line.
 *
 * ```
 * The greeting
 * ------------
 *
 * A program is a list of statements, and this one is a list of length one.
 *
 *     print("Hello, sysl!")
 * ```
 *
 * **Two lines of prose are not two blocks.** Consecutive indented lines are one block whether or not
 * blank lines sit between them, so a function body split by a paragraph of explanation is still one
 * function — the prose is *inside* it, dedented to column zero, and the code resumes at the
 * indentation it left off at. That is what makes the format usable for anything longer than an
 * example: a fifty-line function can be explained a step at a time without being broken into
 * fifty-line-long comments.
 *
 * **A fenced block is prose.** ` ``` ` and `~~~` mark an illustration — an ASCII diagram, expected
 * output, a shell transcript, a snippet of some other language — and none of it is compiled however
 * it is indented. Code that is meant to run is indented; code that is meant to be looked at is
 * fenced. The two are never the same block, which is what lets a chapter show a wrong version beside
 * the right one.
 *
 * **What is under a bullet is prose.** An indented block inside a list item is part of that item —
 * the example the bullet is about — and the program is the blocks at the **top level** only. A list
 * runs until prose comes back to the margin, which a following paragraph or heading already does, so
 * an executable block never sits directly under a list without a line of prose between them.
 *
 * **A tab is refused.** The indent is measured in columns, and a tab has no width until something
 * decides what a tab stop is — so a file that mixes them means one thing to the compiler and another
 * in the reader's editor. Refusing is the only answer that cannot be quietly wrong.
 */
object Literate {

  /** The suffix that says a file is written this way. */
  val Extension: String = ".lsysl"

  /** How far a line has to be indented to be program text. Markdown's own threshold, so that what
   * this compiles and what a Markdown renderer sets in a code block are the same lines.
   */
  val Indent: Int = 4

  /** Whether a file of this name is literate. Asked of the name because that is what a driver, a
   * project walk, and a test fixture all have in common — there is nothing inside the text that says
   * which it is, deliberately: a `.sysl` file full of indented lines is an ordinary program whose
   * author likes whitespace, and it has to stay one.
   */
  def named(name: String): Boolean = name.endsWith(Extension)

  /** What one line of a literate file turned out to be. Every line is one or the other, and the
   * whole of what reading such a file amounts to is deciding which.
   *
   * The two answers carry different text on purpose. `Code` holds the line with the four columns
   * that made it program text already gone, because that is what a compiler is owed; `Prose` holds
   * the line exactly as it was written, because that is what a reader is owed.
   */
  private[sysl] enum Line {
    case Code(text: String)
    case Prose(text: String)
  }

  /** Each line of the file, said to be one or the other — or the first thing wrong with the file.
   *
   * **Two things read a literate file and they want opposite halves of it**, so the rules that
   * decide which half a line belongs to live here and neither of them carries a copy. What a fence
   * does, what an indent means, and what a list item swallows are subtle enough that a second
   * implementation would drift, and the drift would be a program that compiles as one thing and
   * renders as another.
   */
  private[sysl] def classify(source: Source): Either[Diagnostic, IndexedSeq[Line]] = {
    val lines = source.lines
    val out   = Array.fill[Line](lines.length)(Line.Prose(""))
    var fence = Option.empty[(String, Int)]
    var list  = -1
    var i     = 0

    while (i < lines.length) {
      val text = lines(i)

      fence match {
        // Inside an illustration. It ends at a line whose own fence is at least as long as the one
        // that opened it and made of the same character, which is Markdown's rule and matters
        // here for one reason: a fenced block may quote a fence, and the longer opener is how.
        case Some((open, _)) =>
          out(i) = Line.Prose(text)
          if closes(text, open) then fence = None

        case None =>
          val margin = text.takeWhile(c => c == ' ' || c == '\t')
          val bare   = text.drop(margin.length)

          // A tab is refused wherever it could have been meant as indentation, which is **before**
          // asking whether the line is deep enough to be code. Asked after, a tab-indented line
          // would answer "not four spaces", become prose, and be dropped from the program in
          // silence — the author's function quietly missing a statement. That is the failure the
          // rule exists to prevent, so the rule has to run before the decision it protects.
          if bare.nonEmpty && margin.contains('\t') then
            return Left(Diagnostic(
              "a tab in the indentation of a literate file — what makes a line program text is " +
                "four columns of indent, and a tab is as wide as whatever happens to be " +
                "displaying it, so this line is code in one editor and prose in another",
              Some(Pos(source, i + 1, margin.indexOf('\t') + 1))))
          else {
            // An open list item ends at the first line with content that is no further in than the
            // marker was. A blank line does not end one — a list may be written with air between
            // its items — so what closes a list is prose returning to the margin, which is what a
            // paragraph or a heading after a list already is.
            if list >= 0 && bare.nonEmpty && margin.length <= list then list = -1

            if list >= 0 then out(i) = Line.Prose(text)
            else if opensFence(bare) && margin.length < Indent then
              out(i) = Line.Prose(text)
              fence = Some((bare.takeWhile(_ == bare.head), i + 1))
            else if margin.length >= Indent then out(i) = Line.Code(text.drop(Indent))
            else {
              // Only asked where the line is too shallow to be program text, which is why a `-` or
              // a `*` opening a sysl line can never be read as a bullet: at four columns in, the
              // line was code before this was consulted.
              if marker(bare) then list = margin.length
              out(i) = Line.Prose(text)
            }
          }
      }

      i += 1
    }

    // Markdown lets an unclosed fence run to the end of the document, and for a document that is
    // the harmless reading. For a *program* it means every declaration below the opening fence is
    // quietly not compiled, and what the reader is told is that something further up is
    // incomplete — the missing half of their file never enters the story. Refused at the fence
    // that opened, since that is the line to go and look at.
    fence match
      case Some((_, line)) =>
        Left(Diagnostic(
          "this fence is never closed, so everything below it is an illustration and none of it " +
            "is compiled — close it, or indent the lines that are meant to run",
          Some(Pos(source, line, 1))))
      case None => Right(out.toIndexedSeq)
  }

  /** The program a literate file holds, as a `Source` the rest of the compiler cannot tell from an
   * ordinary one — or the first thing wrong with the file.
   *
   * **Prose lines are blanked rather than removed, and this is the whole of why positions still
   * work.** The result has exactly as many lines as the file, and each line of program text sits on
   * the line it was written on, so every position the lexer records is already the position in the
   * `.lsysl` file. Nothing downstream needs a mapping table, and no pass has to know that any of
   * this happened — the same trick `Conditional.gate` plays on a branch this build is not for, for
   * the same reason.
   *
   * The one thing that does move is the **column**, since the four spaces that made a line code are
   * not part of the program. The `Source` carries how far, and `Pos` is where it is added back, so a
   * location a reader is given is the one their editor agrees with.
   *
   * A file that is not literate comes back **unchanged and identical** — the same `Source` object,
   * not a copy of it, since a `Source` compares by identity (`Diagnostics`).
   */
  def tangle(source: Source): Either[String, Source] = tangled(source).left.map(_.rendered)

  /** The same, answering the refusal as **data**. A literate file's margin is a mistake in the file
   * like any other, and an editor showing one has a range to underline.
   */
  def tangled(source: Source): Either[Diagnostic, Source] =
    if !named(source.name) then Right(source)
    else
      classify(source).map { lines =>
        val body = lines.map {
          case Line.Code(text) => text
          case Line.Prose(_)   => ""
        }

        new Source(source.name, body.mkString("\n"), source.dir, Indent, source.features)
      }

  private def opensFence(bare: String): Boolean =
    bare.startsWith("```") || bare.startsWith("~~~")

  /** Whether a line begins a list item: a bullet, or a number and its punctuation, followed by a
   * space. What follows one is prose however far it is indented, so this is the question that keeps
   * an example written under a bullet out of the program.
   */
  private def marker(bare: String): Boolean = {
    val bullet  = bare.length > 1 && "-*+".contains(bare.head) && bare(1) == ' '
    val digits  = bare.takeWhile(_.isDigit)
    val ordered = digits.nonEmpty && bare.length > digits.length + 1 &&
      ".)".contains(bare(digits.length)) && bare(digits.length + 1) == ' '

    bullet || ordered
  }

  /** Whether a line closes the fence `open` opened: the same character, at least as many of them,
   * and nothing else on the line. Trailing spaces are allowed, since they are invisible and a file
   * that failed to close on account of one would be maddening.
   */
  private def closes(text: String, open: String): Boolean = {
    val bare = text.dropWhile(_ == ' ').reverse.dropWhile(c => c == ' ' || c == '\t').reverse

    bare.startsWith(open) && bare.forall(_ == open.head)
  }
}
