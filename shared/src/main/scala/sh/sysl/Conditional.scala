package sh.sysl

import scala.collection.mutable

/** Conditional compilation: the lines of a file that belong to the target being built for
 * (`reference/attributes.md § #if — gating lines before the lexer`).
 *
 * A systems language needs one file to be able to say two things, because the two machines it is
 * describing genuinely differ — a syscall number, a struct a header lays out differently, a symbol
 * one libc exports and the other does not. Everything else about a target is a fact the compiler
 * reads; this is the one place a *program* gets to read one.
 *
 * **The gating is by line, and it happens before the lexer sees anything.** A line inside a branch
 * this target is not being built for is replaced by an empty line — replaced rather than removed, so
 * every line below keeps the number it was written at and a diagnostic quotes the line it means. The
 * directive lines go the same way, which is what makes them cost nothing: after this runs, the file
 * is an ordinary sysl file that happens to have some blank lines in it, and nothing downstream knows
 * this ever happened.
 *
 * **Why lines rather than a construct wrapping declarations.** Rust spells this `#[cfg]`, an
 * attribute on an item, and can because Rust is brace-delimited: the attribute attaches to the item
 * without moving it. **Sysl is indentation-sensitive**, so the equivalent here would have to take an
 * indented block — and then adding or removing a platform gate would reindent everything inside it,
 * turning a one-line intent into a whole-body diff and putting the gate into the same visual channel
 * the language reads block structure out of. A flat marker at the margin disturbs neither.
 *
 * **The cost, stated rather than worked around: the inactive branch is never syntax-checked.** That
 * is C's cost too, and it is the price of gating text rather than trees. What is *not* given up is
 * the conditions themselves — every condition on every directive is parsed and checked here, in the
 * branch being taken and the ones being skipped alike, so a misspelled symbol in the Linux half is
 * caught by a macOS build. Beyond that, a per-target build is what finds a branch that has rotted,
 * and that is a thing to run rather than a thing to design around.
 */
object Conditional {

  /** Every symbol a condition may name, whatever the target.
   *
   * The set is **closed and derived**: it is the operating systems and processors the registry knows
   * (`Target`), plus the two facts worth a name of their own, and nothing a project can add to.
   * Derived so that a new `Os` extends it by existing, rather than by also being written down here
   * where the two could come to disagree.
   *
   * That it is closed is what lets a symbol nobody knows be refused, which is the whole reason to
   * have a set rather than a predicate: a misspelling that read as false would gate code out of the
   * build with nothing said, and silently missing code is the one failure this feature cannot have.
   */
  lazy val symbols: Set[String] =
    Os.values.map(osSymbol).toSet ++ Cpu.values.map(cpuSymbol).toSet ++ Set(Hosted, Posix, Bsd)

  /** The symbols that are **true** for a target. Everything in [[symbols]] and not in here is false;
   * everything in neither is a mistake.
   *
   * `Hosted` and `Posix` are read off `Target.inherentCapabilities` rather than recomputed here.
   * They are the same two facts a `c const` block is gated on, under a different vocabulary — this
   * side names a symbol a source line may test, that side names a capability a module may require —
   * and a machine that is hosted for one and bare for the other is a bug nobody would find by
   * reading either file alone.
   */
  def defined(target: Target, features: Set[String] = Set.empty): Set[String] =
    osDefined(target.os) + cpuSymbol(target.cpu) ++ features.map(featureSymbol)

  /** The symbol a package's feature is named by in a condition: `server` is `feature_server`.
   *
   * The prefix is what keeps the two vocabularies apart. A target's symbols are the compiler's and
   * are closed; a feature's are the manifest's, so a package could otherwise declare a feature
   * called `linux` and gate on a word meaning something else entirely.
   */
  def featureSymbol(name: String): String = FeaturePrefix + name

  /** Whether a condition's word names a feature rather than a fact about the machine.
   *
   * **This family is open where [[symbols]] is closed**, which is the one place the closed-set
   * argument above does not reach: a feature exists because a manifest says so, and the manifest
   * that says so is the one belonging to the file being gated. So `feature_x` in a package that
   * declares no `x` is **false** rather than refused — the same answer a declared feature nobody
   * turned on gives, and the answer a consumer choosing features expects.
   */
  def isFeatureSymbol(s: String): Boolean = s.startsWith(FeaturePrefix)

  private val FeaturePrefix = "feature_"

  /** The symbols an **operating system alone** settles, true for it and false for every other.
   *
   * This is the half a directory selects on (`reference/modules.md § Platform selection`), and the
   * reason it is a function of `Os` rather than of a whole target: the walk that reads a tree has
   * an operating system and no processor, so a selector may name anything answerable from one and
   * nothing that is not. Every one of these is also a `#if` symbol, because [[defined]] is this
   * plus the processor — the two places a source file can name a machine share a vocabulary, and
   * here is where that stops being a claim and becomes an equation.
   */
  def osDefined(os: Os): Set[String] =
    val machine = os.inherentCapabilities

    Set(osSymbol(os)) ++
      Option.when(machine(Capability.Os))(Hosted) ++
      Option.when(machine(Capability.Posix))(Posix) ++
      Option.when(os.bsd)(Bsd)

  /** Every symbol a **directory** may select on — the operating systems, plus the two environment
   * facts that are answerable without a processor.
   *
   * Closed, and closed for the same reason [[symbols]] is: a misspelled folder that read as false
   * would gate source out of a build with nothing said. A processor symbol is deliberately not here
   * — a directory is chosen by a walk that has no processor to ask, and source that varies by one is
   * what `#if` and the C preprocessor are for.
   */
  lazy val directorySymbols: Set[String] =
    Os.values.map(osSymbol).toSet ++ Set(Hosted, Posix, Bsd)

  /** The source as this target sees it, or the first thing wrong with its directives.
   *
   * A file with no directives comes back **unchanged and identical** — the same `Source` object, not
   * a copy of it. A `Source` compares by identity (`Diagnostics`), and a compilation that gated
   * nothing should be the compilation it would have been before any of this existed.
   */
  def gate(source: Source, target: Target): Either[String, Source] =
    gated(source, target).left.map(_.rendered)

  /** The same, answering the refusal as **data** — what `SyslParser.checked` and through it
   * `api.Sysl.check` need, since a directive's mistake is a mistake in the file like any other and
   * an editor has a range to underline for it.
   */
  def gated(source: Source, target: Target): Either[Diagnostic, Source] = {
    val lines   = source.lines
    val out     = lines.toArray
    val on      = defined(target, source.features)
    var stack   = List.empty[Frame]
    var touched = false
    var failed  = Option.empty[Diagnostic]
    var i       = 0

    // `stack` holds the open groups, innermost first. A line survives when every one of them is in
    // its taken branch: an `#if` inside a branch that was not taken selects nothing at all, however
    // its own condition reads.
    def active: Boolean = stack.forall(_.taken)

    def fail(line: Int, col: Int, msg: String): Unit =
      failed = Some(Diagnostic(msg, Some(Pos(source, line, col))))

    while (i < lines.length && failed.isEmpty) {
      val text = lines(i)
      val line = i + 1

      directiveOf(text) match {
        case Some((word, at)) =>
          touched = true
          out(i) = ""

          word match {
            case "if" =>
              condition(source, line, text, at, on) match
                case Left(err)    => failed = Some(err)
                case Right(value) => stack ::= Frame(line, active && value, value)

            case "elif" =>
              stack match
                case Nil =>
                  fail(line, 1, "'#elif' has no '#if' above it to be an alternative to")
                case f :: _ if f.elseLine > 0 =>
                  fail(line, 1, "'#elif' comes after the '#else' on line " +
                    s"${f.elseLine}, which is already the alternative to everything above it")
                case f :: rest =>
                  condition(source, line, text, at, on) match
                    case Left(err) => failed = Some(err)
                    case Right(value) =>
                      // `chosen` is what makes the branches exclusive rather than merely conditional:
                      // it says a branch of this group has already been taken, so no later one is,
                      // however its own condition reads.
                      stack = f.copy(taken = rest.forall(_.taken) && !f.chosen && value,
                                     chosen = f.chosen || value) :: rest

            case "else" =>
              trailing(source, line, text, at) match
                case Some(err) => failed = Some(err)
                case None =>
                  stack match
                    case Nil =>
                      fail(line, 1, "'#else' has no '#if' above it to be an alternative to")
                    case f :: _ if f.elseLine > 0 =>
                      fail(line, 1, s"this group already has an '#else', on line ${f.elseLine}")
                    case f :: rest =>
                      stack = f.copy(taken = rest.forall(_.taken) && !f.chosen, chosen = true,
                                     elseLine = line) :: rest

            case "endif" =>
              trailing(source, line, text, at) match
                case Some(err) => failed = Some(err)
                case None =>
                  stack match
                    case Nil       => fail(line, 1, "'#endif' has no '#if' above it to close")
                    case _ :: rest => stack = rest

            case other =>
              fail(line, 1, notWords(other))
          }

        case None =>
          if !active then { out(i) = ""; touched = true }
      }

      i += 1
    }

    // Reported at the `#if` rather than at the end of the file, because the `#if` is what the reader
    // has to go and look at: an unterminated group is a missing `#endif`, and with several groups
    // open the end of the file says nothing about which one is missing it. The outermost is named
    // because it is the one whose `#endif` would have to come last.
    if failed.isEmpty then
      stack.lastOption.foreach(f => fail(f.line, 1, "this '#if' is never closed — add an '#endif'"))

    failed match
      case Some(err)        => Left(err)
      case None if !touched => Right(source)
      case None             => Right(new Source(source.name, out.mkString("\n"), source.dir, source.columnOffset,
                                       source.features))
  }

  /** One open `#if` group.
   *
   * `taken` is whether *this* branch's lines are the ones being kept, and it already accounts for
   * the groups enclosing this one — so the whole stack being `taken` is exactly the condition for a
   * line to survive. `chosen` is whether any branch of the group has been taken yet, which is what
   * an `#elif` and an `#else` consult. `elseLine` is where this group's `#else` is, or `0` for a
   * group that has not reached one; a group has at most one, and it has to be last.
   */
  private case class Frame(line: Int, taken: Boolean, chosen: Boolean, elseLine: Int = 0)

  /** What an operating system is called where a *source file* names one.
   *
   * Visible to the package because `Project`'s `__<os>__` directories select on the same vocabulary
   * (`reference/modules.md § Platform selection`), and they have to be the same words: `#if linux`
   * and `__linux__/` are one reader's one idea, and two spellings of it would be a thing to look up
   * rather than a thing to know.
   */
  private[sysl] def osSymbol(os: Os): String = os match
    case Os.MacOS        => "macos"
    case Os.Linux        => "linux"
    case Os.Windows      => "windows"
    case Os.Freestanding => "freestanding"
    // `android` and not `linux`, though there is a Linux kernel under it: what a source file gating
    // on a system is asking about is the libc and the libraries, and Bionic is neither glibc's set
    // of `-l` names nor its headers. A program wanting both says `#if linux || android`, which is
    // the same shape `posix` exists to shorten.
    case Os.Android      => "android"
    // `wasi` and not `wasm`: the processor is what `#if wasm32` answers, and this is the *system* —
    // wasi-libc and a table of imports the host supplies. A module built for `wasm32-unknown-unknown`
    // is the same processor and answers `freestanding`, which is the distinction that matters to a
    // file choosing an implementation.
    case Os.Wasi         => "wasi"

  private def cpuSymbol(cpu: Cpu): String = cpu.symbol

  /** True on every target that has an operating system under it, which is what code gating on a
   * platform is usually really asking: whether there is a libc there to call at all.
   */
  private val Hosted = "hosted"

  /** True where the system conventions are POSIX's, which is the name for the commonest disjunction.
   * `#if linux || macos` still says the same thing and is still allowed — this is a shorter way to
   * write it, not a replacement for being able to.
   */
  private val Posix = "posix"

  /** A libc family rather than a capability, which is why it is read off `Os.bsd` and not off
   * `inherentCapabilities` — a BSD and a glibc system are both `posix`, and every one of the
   * library's OS gates is about the difference between them rather than about what the machine can
   * do. `Os.bsd` carries the argument and the list of what it costs to get wrong.
   *
   * It is in `directorySymbols` as well as here, because the two vocabularies are one equation: a
   * `#if bsd` and a `__bsd__` folder have to mean the same thing or the file that reads one is
   * gated differently from the folder that holds the other.
   */
  private val Bsd = "bsd"

  private val Words = Set("if", "elif", "else", "endif")

  /** The C spellings sysl does not have, kept so that writing one is told what to write instead.
   * Without this they fall through to the lexer as a stray `#` and a name, and the reader is left
   * with a syntax error about a token rather than with the sentence they need.
   */
  private lazy val notWords: Map[String, String] = Map(
    "ifdef"  -> ("sysl has no '#ifdef' — a symbol is a fact about the target rather than something a " +
      "build defines, so the test is written '#if <symbol>'"),
    "ifndef" -> "sysl has no '#ifndef' — write '#if !<symbol>'",
    "elseif" -> "sysl spells this '#elif'",
    "define" -> ("sysl has no '#define' — the symbols a condition may name are fixed by the target: " +
      symbols.toList.sorted.mkString(", ")),
    "undef"  -> ("sysl has no '#undef' — the symbols a condition may name are fixed by the target: " +
      symbols.toList.sorted.mkString(", ")),
  )

  /** The directive a line carries, as the word and the 1-based column its argument would start at,
   * or `None` for an ordinary line.
   *
   * **A directive sits at the margin, column 1, and that is a rule rather than a convention.** It is
   * what keeps the gate out of the channel the language reads block structure from: an indented
   * `#if` would look like it takes part in an indentation it has nothing to do with, and a reader
   * would have to know that the line disappears before anything counts columns. It is also how C is
   * written.
   *
   * What it is **not** is the thing that tells a directive from an annotation. That is the sigil:
   * `#` gates lines, `@` says something about a declaration. The margin could not do the job, since
   * an annotation on a `module` header sits at column 1 too — the declaration it is on is there.
   */
  private def directiveOf(text: String): Option[(String, Int)] =
    if !text.startsWith("#") then None
    else {
      val word = text.drop(1).takeWhile(_.isLetter)

      // A word this does not know is left alone rather than refused, and is answered further in —
      // by the grammar, which has the `#` as a token and a sentence to say about it. Refusing here
      // would be refusing before the lexer has run, so the report would have a line and no column.
      // Something that reads as a name carrying on past the word is left alone for the same reason.
      if (Words(word) || notWords.contains(word)) &&
        !text.drop(1 + word.length).headOption.exists(c => c.isDigit || c == '_')
      then Some((word, 2 + word.length))
      else None
    }

  /** What remains of a directive line after its word, with a trailing `//` comment dropped.
   *
   * A comment earns its place on these specifically: `#endif // linux` is how the reader of a long
   * gated region is told which group just closed. There is nothing else on such a line for a `//` to
   * be part of — a condition has no strings in it — so finding one is the whole of the work.
   */
  private def argument(text: String, at: Int): String = {
    val rest = text.drop(at - 1)

    rest.indexOf("//") match
      case -1 => rest
      case n  => rest.take(n)
  }

  /** Refuses anything written after `#else` or `#endif`, neither of which takes an argument.
   *
   * Left unchecked, `#endif linux` — C's old habit, and legal there — would read as an `#endif` with
   * the word quietly discarded, so the spelling most likely to arrive out of muscle memory would be
   * the one thing here that failed silently.
   */
  private def trailing(source: Source, line: Int, text: String, at: Int): Option[Diagnostic] = {
    val rest = argument(text, at)
    val n    = rest.indexWhere(!_.isWhitespace)

    Option.when(n >= 0)(
      Diagnostic(
        s"'#${text.drop(1).takeWhile(_.isLetter)}' takes nothing after it, and '${rest.trim}' is here",
        Some(Pos(source, line, at + n))))
  }

  /** Whether a directive's condition holds for the symbols given, or what is wrong with it.
   *
   * **The condition is evaluated whether or not its branch could be taken**, so that a mistake in the
   * half of a file this build is not compiling is still this build's problem. That is the one part of
   * an inactive branch this pass checks, and it is the part most worth checking: the mistakes a
   * condition can hold are exactly the ones that would otherwise gate code out with nothing said.
   *
   * `&&` and `||` are here although old sysl banned them. In a flat line-marker scheme the only other
   * way to write a disjunction is to nest the groups, which reads far worse than `#if linux || macos`
   * for the sake of a boolean evaluator over a set of strings.
   */
  private def condition(source: Source, line: Int, text: String, at: Int,
                        on: Set[String]): Either[Diagnostic, Boolean] = {
    val arg  = argument(text, at)
    val word = text.drop(1).takeWhile(_.isLetter)

    def err(col: Int, msg: String): Either[Diagnostic, Boolean] =
      Left(Diagnostic(msg, Some(Pos(source, line, col))))

    // A target's *name* is the thing a reader most plausibly reaches for and the one thing that
    // cannot be a symbol: it has a `-` in it, which no identifier carries. Left to the tokenizer they
    // are told a `-` is not an operator, which is true and no help.
    val named = arg.trim

    if Target.all.exists(_.name == named) then
      return err(at + arg.indexWhere(!_.isWhitespace),
        s"'$named' is a target's name rather than something a condition asks about — a condition asks " +
          "about one fact of the machine at a time, so this is written '" + named.replace("-", " && ") + "'")

    val toks   = mutable.ListBuffer.empty[(String, String, Int)]
    var lexErr = Option.empty[Diagnostic]
    var i      = 0

    while (i < arg.length && lexErr.isEmpty) {
      val c   = arg(i)
      val col = at + i

      if c.isWhitespace then i += 1
      else if c.isLetter || c == '_' then {
        val s = arg.drop(i).takeWhile(ch => ch.isLetterOrDigit || ch == '_')

        toks += (("name", s, col))
        i += s.length
      } else if arg.startsWith("&&", i) then { toks += (("&&", "&&", col)); i += 2 }
      else if arg.startsWith("||", i) then { toks += (("||", "||", col)); i += 2 }
      else if c == '!' || c == '(' || c == ')' then { toks += ((c.toString, c.toString, col)); i += 1 }
      else
        lexErr = Some(Diagnostic(
          s"a condition is built from symbols, '!', '&&', '||' and parentheses, and '$c' is none of them",
          Some(Pos(source, line, col))))
    }

    var p = 0

    def peek: Option[(String, String, Int)] = toks.lift(p)

    // Where the condition ran out, for the errors that are about something missing rather than
    // something written: past the last token there is, or at the argument itself when there are none.
    def endCol: Int = toks.lastOption.map((_, t, c) => c + t.length).getOrElse(at)

    def atom: Either[Diagnostic, Boolean] = peek match
      case Some(("name", s, col)) =>
        p += 1
        if symbols(s) || isFeatureSymbol(s) then Right(on(s))
        else
          err(col, s"'$s' is not something a target says about itself — sysl knows " +
            symbols.toList.sorted.mkString(", ") +
            s". A feature this package's manifest declares is named '${featureSymbol(s)}'")
      case Some(("(", _, _)) =>
        p += 1
        or.flatMap(v =>
          peek match
            case Some((")", _, _)) => p += 1; Right(v)
            case Some((_, t, col)) => err(col, s"this '(' wants a ')' before '$t'")
            case None              => err(endCol, "this '(' is never closed"))
      case Some((_, t, col)) => err(col, s"a condition needs a symbol here, and '$t' is what is written")
      case None              => err(endCol, s"'#$word' needs a condition after it")

    def unary: Either[Diagnostic, Boolean] = peek match
      case Some(("!", _, _)) => p += 1; unary.map(!_)
      case _                 => atom

    // Both sides are evaluated even where the result is already settled — this is a checker as much
    // as an evaluator, and short-circuiting would let a misspelled symbol through whenever the other
    // half of the condition happened to decide the answer.
    def and: Either[Diagnostic, Boolean] = {
      var acc = unary

      while (peek.exists(_._1 == "&&") && acc.isRight) {
        p += 1
        acc = for a <- acc; b <- unary yield a && b
      }
      acc
    }

    def or: Either[Diagnostic, Boolean] = {
      var acc = and

      while (peek.exists(_._1 == "||") && acc.isRight) {
        p += 1
        acc = for a <- acc; b <- and yield a || b
      }
      acc
    }

    lexErr match
      case Some(e) => Left(e)
      case None =>
        or.flatMap(v =>
          peek match
            case None              => Right(v)
            case Some((_, t, col)) => err(col, s"the condition is complete before '$t'"))
  }
}
