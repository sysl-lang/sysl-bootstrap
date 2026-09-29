package sh.sysl

/** How a module's name and the name of a declaration in it make the one key every table is keyed by
 * (`reference/modules.md`, `reference/modules.md § Imports`).
 *
 * A declaration belongs to a module, and two modules may each declare a `read`, so the name a
 * table holds has to say which module's it is. The alternative — a module dimension on every
 * table — would have touched every lookup in the analyzer to say something that only ever
 * travels with the name, so the name carries it instead.
 *
 * **The separator is not a dot**, and that is the whole of the design here. A dot already
 * separates a type from its member (`Point.dist`) and a generic function from the arguments it
 * was instantiated at (`f.int`), so a dotted module prefix would let `geom.Point.dist` mean
 * either a member of `geom`'s `Point` or a function `dist` of a module `geom.Point`. Two
 * declarations answering to one key is a miscompile rather than a diagnostic, so the two
 * separators are kept apart: a module name holds no `$`, and neither does an identifier, so
 * splitting at the first one always recovers the module a key belongs to.
 *
 * `$` is also a character an LLVM symbol may contain unquoted, which is what lets a key be the
 * emitted name as it always was — nothing between the analyzer and codegen has to mangle.
 */
object Modules {

  /** The character between a module's name and the name of a declaration in it. */
  val sep: Char = '$'

  /** The name of the **anonymous root module**, whose name is the empty path
   * (`reference/modules.md`). A declaration in it is keyed by its own name alone, which is why a
   * program of one headerless file is keyed exactly as it was before modules existed.
   */
  val root: String = ""

  /** The key a declaration named `name` in `module` is filed under.
   *
   * **The name is guarded here and NOT made LLVM-safe here**, and the split is what
   * `reference/lexical.md § Identifiers` costs: a name may be written in any script and a quoted
   * one may hold a space, so a key that had been made LLVM-safe would no longer spell the name the
   * programmer wrote — and the analyzer compares a key's tail against a declared name in several
   * places, and diagnostics print one. `LlvmName.guard` marks a `$` and touches nothing else;
   * `LlvmName.safe` finishes the job at the emitter, where a name becomes IR text.
   *
   * **That is what keeps `split` working.** The module a key belongs to is everything before its
   * first `$`, so the one character a name may not contribute raw is `$` — which the ordinary
   * identifier grammar cannot produce and only a quoted name can. A module path holds no quoted
   * segment, refused where a module path is read, so the first `$` in a key is always the one this
   * function put there.
   */
  def qualify(module: String, name: String): String =
    if module.isEmpty then LlvmName.guard(name) else s"$module$sep${LlvmName.guard(name)}"

  /** The module a key belongs to, and everything after it — which for a member or an
   * instantiation is itself a dotted name (`Point.dist`, `f.int`) and is left as one.
   */
  def split(key: String): (String, String) = {
    val i = key.indexOf(sep.toInt)

    if i < 0 then (root, key) else (key.take(i), key.drop(i + 1))
  }

  /** The module a key belongs to. */
  def moduleOf(key: String): String = split(key)._1

  /** The declared name in a key, with the module it belongs to taken off. */
  def bare(key: String): String = split(key)._2

  /** A key as a diagnostic spells it: the separator read back as the dot a programmer writes.
   *
   * Two keys can share a spelling — `geom`'s `Point.dist` and `geom.Point`'s `dist` both read as
   * `geom.Point.dist` — which is a cost paid in the one place it is affordable. A message is read
   * by someone looking at the line it points at; a table is not.
   */
  def show(key: String): String = key.replace(sep, '.')

  /** A key with the segments the compiler added to it taken off — an overload's number and a
   * file-private slot — so that what is left is the qualified name a file actually wrote.
   *
   * They come off in turn because a private slot can carry an overload suffix: a file that declares
   * one spelling twice, privately, while a sibling file holds the plain key, has its second
   * declaration at `m$pick.private1.1`.
   */
  def spelled(key: String): String = {
    val cut  = key.lastIndexOf('.')
    val last = key.drop(cut + 1)
    val slot = last.forall(_.isDigit) ||
      (last.startsWith("private") && last.drop("private".length).forall(_.isDigit) &&
        last.length > "private".length)

    if cut > 0 && cut < key.length - 1 && slot then spelled(key.take(cut)) else key
  }

  /** The declaration a key names, as the source that declared it spells it: `m$P.get` is `P.get`,
   * `m$hid.private1` is `hid`, and a `$` a quoted name contributed (`LlvmName.guard`) is read back.
   */
  def declared(key: String): String = unguard(bare(spelled(key)))

  /** A message with every key it quotes spelled as the declaration it names (`declared`).
   *
   * **This is the one place a key becomes display text**, and it sits in `Diagnostic`'s constructor,
   * so every complaint the compiler makes passes through it on its way to a reader — rendered, or
   * read as data through `api.Sysl.check`. A message is written by interpolating whatever name is
   * in hand, and in the analyzer the name in hand is nearly always the key a table holds, so a site
   * that forgot to take the module off printed `'m$use'`: a name nobody can write, and one that
   * names the compiler's bookkeeping rather than the program. Fixing each site would leave the next
   * one written to make the same mistake.
   *
   * **What counts as a key is narrow on purpose**: a span between two single quotes that begins
   * with a module path — identifier segments joined by dots — followed by the separator and a
   * letter or an underscore. So `'$'`, `'$name'` and `'{'` are untouched, a qualified spelling
   * a site chose with `show` has no `$` left to find, and a quoted name that was guarded carries
   * `$24`, whose digit is not a name's first character.
   */
  def readable(message: String): String =
    if !message.contains(sep) then message
    else {
      val out = new StringBuilder
      var i   = 0

      while i < message.length do
        val close = if message(i) == '\'' then message.indexOf('\'', i + 1) else -1

        if close > i && isKey(message.substring(i + 1, close)) then
          out += '\'' ++= declared(message.substring(i + 1, close)) += '\''
          i = close + 1
        else
          out += message(i)
          i += 1

      out.toString
    }

  private def isNameStart(c: Char): Boolean = c.isLetter || c == '_'

  private def isNamePart(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  /** Whether a quoted span is a key: a module path, the separator, and a name after it. */
  private def isKey(s: String): Boolean = {
    val i = s.indexOf(sep.toInt)

    i > 0 && i < s.length - 1 && isNameStart(s(i + 1)) && !s.contains('\n') && {
      val segments = s.take(i).split('.')

      segments.forall(seg => seg.nonEmpty && isNameStart(seg.head) && seg.forall(isNamePart))
    }
  }

  /** A name with the `$` marks `LlvmName.guard` wrote read back as the `$` a quoted name held. */
  private def unguard(name: String): String = name.replace(s"${sep}24", sep.toString)
}
