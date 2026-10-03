package sh.sysl

/** Identifiers the language answers for itself: `__FILE__`, `__LINE__` and the rest.
 *
 * **The shape is reserved, not the seven names.** An identifier that begins and ends with `__` and
 * holds nothing but capitals and underscores in between belongs to the compiler, and a declaration
 * may not take one. That rule is the feature; the built-ins are what currently occupies the space it
 * keeps clear. Reserving the shape up front is what makes every later addition non-breaking — a
 * new built-in cannot collide with a name somebody already declared, because the shape was never
 * theirs to declare.
 *
 * It is the same decision the `#if` vocabulary made (`getting-started/cli.md § targets`): a closed
 * set, and a name outside it is an **error rather than false**, because a misspelling that quietly
 * meant nothing is the one failure C has here and the one worth not repeating. C reserves this
 * space too and diagnoses nothing in it, so `__FILE_` — one underscore short — is a name C will
 * happily let you declare and then collide with. Here it is either the shape or it is not, and the
 * shape is refused outright.
 *
 * **These are not reserved words.** They lex as ordinary identifiers and are resolved by the
 * analyzer, exactly as `int`, `usize` and `f32` are (`SyslLexical`). That is what keeps the space
 * cheap to extend: a new built-in is an entry in `builtins` below, and it owes nothing to the
 * lexer's `reserved` set, to the reserved-word table the site's `lexical.md` states a *count* for,
 * or to the highlighting grammar `GrammarTests` reconciles against that set.
 *
 * **The restriction is on sysl identifiers and not on the C names an `extern` links to.** C's own
 * reserved space is exactly where a libc lives — `library/sysl/fs/error.sysl` links `"__errno_location"`
 * and `"__error"` — so the *string* in an `extern` is untouched by any of this. What is checked is
 * the name the declaration binds in sysl, which in that file is `errno_location`.
 */
object ReservedNames {

  /** Whether `name` is the compiler's to answer: `__`, capitals and underscores, `__`.
   *
   * The middle may be empty, so `____` has the shape and is simply not a built-in — which is the
   * right answer rather than a special case, since the point of the shape is to be refused whether
   * or not anything currently occupies it. Three underscores do not have it: the two markers may not
   * overlap, so four characters is the shortest that does.
   *
   * Deliberately ASCII: `isUpper` is true of letters in scripts that have no bearing on a rule whose
   * whole job is to be recognizable on sight, and a name is either obviously the compiler's or it is
   * the author's.
   */
  def shaped(name: String): Boolean =
    name.length >= 4 && name.startsWith("__") && name.endsWith("__") &&
      name.substring(2, name.length - 2).forall(c => (c >= 'A' && c <= 'Z') || c == '_')

  /** The built-ins, in the order a diagnostic lists them — location first, since that is what they
   * are for, and the build stamp last, since it is the one with a cost attached.
   */
  val builtins: List[String] =
    List("__FILE__", "__LINE__", "__COLUMN__", "__FUNCTION__", "__DATE__", "__TIME__", "__VERSION__")

  /** What `__VERSION__` says in a file that belongs to no package — a lone `sysl run x.sysl`, or
   * anything else compiled with no `package.hocon` at its project root.
   */
  def versionNoPackage(file: String): String =
    s"'__VERSION__' reads the 'version' in package.hocon, and '$file' is not part of a package — " +
      "there is no package.hocon at the root of its project"

  /** What `__VERSION__` says where the file's package has a manifest that states no `version`. */
  def versionUnstated(manifest: String): String =
    s"'__VERSION__' reads the 'version' in package.hocon, and $manifest states none — " +
      "add 'version = \"…\"' to it"

  /** A node with no place in any file has no package either. */
  val versionNowhere: String =
    "'__VERSION__' reads the 'version' of the package a file belongs to, and this is a node with no place in any file"

  /** What to say to a declaration that tried to take one of these names.
   *
   * It names the *shape* rather than the seven, because the mistake is not "you picked a built-in" —
   * a reader who wrote `__MY_FLAG__` picked nothing — and being told which names are taken would
   * send them looking for a collision that is not there.
   */
  def refuseDeclaration(name: String, what: String): String =
    s"'$name' begins and ends with '__', which is reserved for names the compiler answers for " +
      s"itself — so it is not a name a $what may take. Everything of that shape belongs to the " +
      s"language, whether or not it means anything yet, which is what lets a later release add one " +
      s"without breaking a program that guessed it. Write '${suggest(name)}' instead"

  /** The same name with the markers taken off, which is nearly always what was meant. An unwrapping
   * that leaves nothing usable falls back to something that is at least a name.
   */
  private def suggest(name: String): String = {
    val bare = name.substring(2, name.length - 2).stripPrefix("_").stripSuffix("_").toLowerCase

    if bare.isEmpty then "a name of your own" else bare
  }

  /** What to say to a use of a name that has the shape and is not one of the seven. */
  def unknown(name: String): String =
    s"there is no built-in called '$name' — the shape '__…__' is the language's, and what it " +
      s"currently holds is ${builtins.mkString(", ")}"

  /** Every name a top-level declaration binds that has the reserved shape, with what kind of thing
   * tried to take it and where it was written.
   *
   * **Parameters of anything with a body are deliberately absent.** They are bound as locals when
   * the body is analyzed, and `Scoping.declare` refuses them there — reporting from both places
   * would give one mistake two diagnostics. What is here instead is every name no binding pass will
   * ever see: a declaration's own name, a type parameter, a field, a variant, and the parameters of
   * an `extern` or a bodiless trait method, which are written and never bound.
   *
   * The pass is over the untyped tree and runs before hoisting, so a program that names a
   * declaration `__FILE__` is told about *that* rather than about the eleven consequences of a
   * declaration the rest of the compiler could not register.
   */
  def declaredIn(stmt: Stmt): List[(String, String, Option[Pos])] = {
    def named(name: String, what: String, pos: Option[Pos]): List[(String, String, Option[Pos])] =
      if shaped(name) then List((name, what, pos)) else Nil

    def tparams(ns: List[String], pos: Option[Pos]): List[(String, String, Option[Pos])] =
      ns.flatMap(named(_, "type parameter", pos))

    def params(ps: List[Param]): List[(String, String, Option[Pos])] =
      ps.flatMap(p => named(p.name, "parameter", p.pos))

    def method(m: MethodDecl): List[(String, String, Option[Pos])] =
      named(m.name, "method", m.pos) ::: tparams(m.tparams, m.pos) :::
        (if m.body.isEmpty then params(m.params) else Nil)

    stmt match
      case f: FuncDecl =>
        named(f.name, "function", f.pos) ::: tparams(f.tparams, f.pos) :::
          (if f.body.isEmpty then params(f.params) else Nil)
      case e: ExternDecl    => named(e.name, "declaration", e.pos) ::: params(e.params)
      case e: ExternVarDecl => named(e.name, "declaration", e.pos)
      case v: ValDecl       => named(v.name, "'val'", v.pos)
      case v: VarDecl       => named(v.name, "'var'", v.pos)
      case c: ConstDecl     => named(c.name, "'const'", c.pos)
      case t: TypeDecl      => named(t.name, "type", t.pos)
      case s: StructDecl =>
        named(s.name, "type", s.pos) ::: tparams(s.tparams, s.pos) :::
          s.fields.flatMap(f => named(f.name, "field", f.pos)) ::: s.members.flatMap(method)
      case e: EnumDecl =>
        named(e.name, "type", e.pos) ::: tparams(e.tparams, e.pos) :::
          e.variants.flatMap(v => named(v.name, "variant", v.pos) ::: params(v.fields)) :::
          e.members.flatMap(method)
      case t: TraitDecl =>
        named(t.name, "trait", t.pos) ::: tparams(t.tparams, t.pos) ::: t.methods.flatMap(method)
      case i: ImplDecl => i.methods.flatMap(method)
      // An alias binds a name without declaring anything, and binds it through the import tables
      // rather than through `Scoping.declare` — so this is the only pass that can see it. Left out,
      // `import sysl.text as __TEXT__` would sit in the space the shape exists to keep clear, which
      // is the one outcome the whole rule is for.
      // A wildcard binds no name of its own, so only its selectors and the bare-path form are asked.
      case i: ImportDecl =>
        (if i.wildcard then Nil else named(i.bound, "import", i.pos)) :::
          i.selectors.flatMap(s => named(s.bound, "import", s.pos))
      // `static` says where the storage lives and nothing about the name, so the declaration inside
      // it is checked exactly as it would have been written on its own (`reference/modules.md § Where a program starts`).
      case s: StaticDecl => declaredIn(s.inner)
      case _             => Nil
  }

  /** `__DATE__`, in C's format: `Mmm dd yyyy`, with the day **space**-padded rather than zero-padded.
   *
   * The format is C's because this is the name C gave it, and a reader who knows the name knows the
   * shape that comes out of it; inventing a tidier one would make the familiar name a false friend.
   *
   * **UTC, not local time.** The compiler cross-builds to three platforms and only one of them has a
   * timezone database worth relying on, so a local answer would be right on the JVM and a guess
   * elsewhere. A stamp that means the same thing wherever the build ran is also the more useful of
   * the two: it is read to identify a build, not to know what the author had for lunch.
   */
  def date(millis: Long): String = {
    val (y, m, d) = civil(Math.floorDiv(millis, 86400000L))
    val month     = List("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")(m - 1)

    f"$month ${d.toString}%2s $y"
  }

  /** `__TIME__`, in C's format: `hh:mm:ss`, zero-padded, and UTC for the same reason `date` is. */
  def time(millis: Long): String = {
    val secs = Math.floorMod(Math.floorDiv(millis, 1000L), 86400L)

    f"${secs / 3600}%02d:${secs / 60 % 60}%02d:${secs % 60}%02d"
  }

  /** The civil date a count of days since 1970-01-01 lands on, as Howard Hinnant's `civil_from_days`
   * computes it: shift the era to start in March so the leap day is last and the month lengths
   * repeat on a 153-day cycle, then shift back.
   *
   * Written out rather than taken from a date library because the compiler builds on three platforms
   * and `java.time` is whole on exactly one of them. `Math.floorDiv` is what makes it correct for a
   * timestamp before 1970, which nothing will produce and which is cheaper to get right than to
   * document as unsupported.
   */
  private def civil(days: Long): (Long, Int, Int) = {
    val z    = days + 719468
    val era  = Math.floorDiv(z, 146097L)
    val doe  = z - era * 146097
    val yoe  = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val doy  = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp   = (5 * doy + 2) / 153
    val d    = (doy - (153 * mp + 2) / 5 + 1).toInt
    val m    = (mp + (if mp < 10 then 3 else -9)).toInt

    (yoe + era * 400 + (if m <= 2 then 1 else 0), m, d)
  }

  /** The moment this compilation started, which every `__DATE__` and `__TIME__` in it reports.
   *
   * One stamp per process, so a program with two `__TIME__`s in it cannot disagree with itself about
   * when it was built — which a per-use reading would allow across a second boundary, rarely, and
   * therefore in a way nobody would reproduce.
   */
  lazy val stamp: Long = System.currentTimeMillis()
}
