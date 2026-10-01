package sh.sysl.doc

import sh.sysl.*

/** A declaration written back out as the source line a reader wants to see.
 *
 * **This renders the AST rather than slicing the source**, which is a choice with a cost worth
 * knowing. Slicing would be perfect fidelity for free — it is literally what the author typed — and
 * it was rejected because a signature is not a quotation. What a reference page wants is one line
 * with the parameters and the result on it, normalized, with the body, the annotations, the
 * visibility keyword and the author's line breaks all gone; recovering that from source text means
 * finding where a signature ends, which for an indented language is the same work as parsing it
 * again. Rendering the tree is the shorter road and the one scaladoc and rustdoc both take.
 *
 * **What it renders is what was WRITTEN, not what was resolved.** A `TypeRef` is the syntax — the
 * `usize` a parameter declared, not the `Type` the analyzer settled it to — and that is the right
 * side of the line for documentation. An alias should read as the alias its author chose, a `Self`
 * should stay `Self`, and a defaulted type parameter should show the default rather than an
 * instantiation. It also means this runs on a parsed program and needs nothing analyzed, which is
 * what lets a module documented from a package that does not build still produce a page.
 *
 * **An expression in a type position is rendered conservatively.** Only the forms that can actually
 * stand there — a literal, a name, a qualified name, arithmetic over those — are spelled out, and
 * anything else answers `…`. That is a deliberate floor rather than an omission: an array bound or a
 * default is occasionally an arbitrary constant expression, and a documentation tool that guessed
 * wrong about one would print something false with no way for a reader to tell.
 */
object Signature {

  /** A written type, as source text.
   *
   * Every case of `TypeRef` is here and the ordering follows the declarations in `astTypes.scala`,
   * so a type added there fails this exhaustively rather than silently rendering as something else.
   */
  def typeText(t: TypeRef): String = t match
    case NamedType(name, Nil)  => name
    case NamedType(name, args) => s"$name[${args.map(typeText).mkString(", ")}]"
    case ValueArgType(v)       => exprText(v)
    case PtrType(inner)        => s"*${typeText(inner)}"
    // `sync` sits after the sigil rather than in front of the type, because it qualifies the
    // reference and not the referent — the same reason `const` sits after the brackets below.
    case RefType(inner, sync)  => if sync then s"&sync ${typeText(inner)}" else s"&${typeText(inner)}"
    case WeakType(inner)       => s"weak ${typeText(inner)}"

    case ArrayType(length, elem, readOnly) =>
      val bound = length.map(exprText).getOrElse("")
      val const = if readOnly then "const " else ""

      s"[$bound]$const${typeText(elem)}"

    case VectorType(lanes, elem) => s"<${exprText(lanes)}>${typeText(elem)}"
    case VolatileType(inner)     => s"volatile ${typeText(inner)}"
    case TupleType(parts, _)     => s"(${parts.map(typeText).mkString(", ")})"
    case PackType(name)          => s"..$name"

    // A callable has two spellings that mean one thing, and `bare` is which was written. Rendering
    // the one the author chose matters more here than anywhere else in this file: the bare arrow is
    // the form a parameter uses, so normalizing it to `Fn(...)` would make every callback in the
    // library read as something a caller cannot write in that position.
    case FnType(params, ret, bare) =>
      val ps = params.map(typeText).mkString(", ")

      if bare && params.length == 1 then s"$ps -> ${typeText(ret)}"
      else if bare then s"($ps) -> ${typeText(ret)}"
      else s"Fn($ps) -> ${typeText(ret)}"

    case CFnType(params, ret) => s"*extern(${params.map(typeText).mkString(", ")}) -> ${typeText(ret)}"
    case AssocType(base, member) => s"${typeText(base)}::$member"
    case AssocArgType(name, typ) => s"$name = ${typeText(typ)}"
    case SomeType(bounds)        => s"some ${bounds.map(boundText).mkString(" + ")}"

  /** A trait as a bound names it — `Display`, or `From[int]`. */
  def boundText(b: BoundRef): String =
    if b.args.isEmpty then b.name else s"${b.name}[${b.args.map(typeText).mkString(", ")}]"

  /** An expression standing in a type position, or as a parameter's default.
   *
   * **The fallback is `…` and it is load bearing.** Everything below is a form that can really turn
   * up in an array bound, a lane count, a value argument or a default; anything else is a constant
   * expression this has no business paraphrasing. Printing an ellipsis says "there is a value here
   * and it is not shown", which a reader can act on. Printing a guess says something false.
   */
  def exprText(e: Expr): String = e match
    case IntLit(value, suffix)  => s"$value${suffix.getOrElse("")}"
    case FloatLit(text, suffix) => s"$text${suffix.getOrElse("")}"
    case BoolLit(value)         => value.toString
    case StrLit(value)          => "\"" + strText(value) + "\""
    case CharLit(cp)            => s"'${charText(cp)}'"
    case NullLit()              => "null"
    case UnitLit()              => "()"
    case Ident(name)            => name
    case Field(recv, name)      => s"${exprText(recv)}.$name"
    case Unary(op, operand)     => s"$op${operandText(operand, PrefixPower, right = false)}"
    case Binary(op, l, r)       =>
      val p = infixPower(op)

      s"${operandText(l, p, right = false)} $op ${operandText(r, p, right = true)}"
    case Tuple(elements)        => s"(${elements.map(exprText).mkString(", ")})"
    case Call(callee, args)     => s"${exprText(callee)}(${args.map(exprText).mkString(", ")})"
    case ArrayLit(elements)     => s"[${elements.map(exprText).mkString(", ")}]"
    case _                      => "…"

  /** A character literal's interior, with the escapes a reader would have had to write.
   *
   * Rendering the raw codepoint would put a literal newline inside quotes on the page, which is both
   * wrong as sysl and invisible as prose.
   */
  private def charText(cp: Int): String = escaped(cp, '\'')

  /** One operand of an operator binding at `p`, parenthesized where the parser would otherwise read
   * it differently.
   *
   * The tree records no parentheses, so they are put back from the operators' binding powers: an
   * operand binding more loosely than the operator it sits under is parenthesized, and so is a right
   * operand binding exactly as tightly, every infix operator here grouping to the left. Written
   * bare, `-(3 * 4)` printed as `-3 * 4` and `(a + b) * 2` as `a + b * 2` — each a different value.
   */
  private def operandText(e: Expr, p: Int, right: Boolean): String = e match
    case Binary(op, _, _) =>
      val q = infixPower(op)

      if q < p || (right && q == p) then s"(${exprText(e)})" else exprText(e)
    case _ => exprText(e)

  /** How tightly a prefix operator holds its operand: above every infix operator. */
  private val PrefixPower = 70

  /** How tightly an infix operator binds, as `ExprParser`'s ladder reads it, loosest first. */
  private def infixPower(op: String): Int = op match
    case "||"                                   => 10
    case "&&"                                   => 20
    case "==" | "!=" | "<" | "<=" | ">" | ">=" => 30
    case "|"                                    => 40
    case "^"                                    => 42
    case "&"                                    => 44
    case "+" | "-"                              => 50
    case "*" | "/" | "%" | "<<" | ">>"          => 60
    case _                                      => 0

  /** A string literal's interior, escaped as it would have been written: `"a\"b"`, `"\n"`.
   *
   * The literal's value is what the lexer decoded, so a default of `"\n"` holds a real newline, and
   * printing it back unescaped broke the signature across two lines of the fence.
   */
  private def strText(value: String): String =
    value.codePoints.toArray.map(escaped(_, '"')).mkString

  /** One codepoint inside a literal quoted by `quote`: the quote itself, a backslash and the control
   * characters take the escape the lexer reads; everything else is itself.
   */
  private def escaped(cp: Int, quote: Char): String = cp match
    case 0x0A => "\\n"
    case 0x0D => "\\r"
    case 0x09 => "\\t"
    case 0x00 => "\\0"
    case 0x5C => "\\\\"
    case c if c == quote => s"\\$quote"
    case c if c < 0x20 || c == 0x7F => f"\\u{$c%x}"
    case c    => String.valueOf(Character.toChars(c))

  /** A declaration's type-parameter list, `[...]`, or empty where there is none.
   *
   * The three kinds share one list because they share one argument position, so they are separated
   * here by what the declaration recorded about each: `tvalues` marks a value parameter and gives
   * its type, `tpacks` marks a pack, and anything in neither is an ordinary type parameter. Bounds
   * and defaults hang off whichever it turned out to be.
   */
  def tparamsText(
      tparams: List[String],
      bounds: Map[String, List[BoundRef]],
      tvalues: Map[String, TypeRef] = Map.empty,
      tpacks: Set[String] = Set.empty,
      tdefaults: Map[String, TypeRef] = Map.empty,
  ): String =
    if tparams.isEmpty then ""
    else
      val parts = tparams.map { p =>
        val head =
          if tvalues.contains(p) then s"const $p: ${typeText(tvalues(p))}"
          else if tpacks(p) then s"..$p"
          else p

        // A value parameter states its type where a type parameter states its bounds, so the two
        // never both apply — but a pack takes bounds like any other, which is why this is appended
        // rather than folded into the branch above.
        val bound =
          bounds.get(p).filter(_.nonEmpty) match
            case Some(bs) if !tvalues.contains(p) => s": ${bs.map(boundText).mkString(" + ")}"
            case _                                => ""

        val default = tdefaults.get(p).map(d => s" = ${typeText(d)}").getOrElse("")

        s"$head$bound$default"
      }

      s"[${parts.mkString(", ")}]"

  /** One parameter, as written: `xs: []const u8`, `n: usize = 0`, `f: -> int`, `xs: ...int`.
   *
   * **Two of those are properties of the PARAMETER rather than of its type**, so both are spelled
   * here and not in `typeText` — which is the same split the AST makes and the reason this takes a
   * `Param` rather than a `TypeRef`. A by-name parameter's `-> T` is one; a rest parameter's `...T`
   * is the other, and it matters more, because a rest parameter's `typ` is already the `[]const T`
   * its body sees. Rendering that would document a signature nobody can call: the caller writes
   * `total(1, 2, 3)`, not a slice.
   */
  def paramText(p: Param): String =
    val arrow   = if p.byName then "-> " else ""
    val default = p.default.map(d => s" = ${exprText(d)}").getOrElse("")

    s"${p.name}: $arrow${paramTypeText(p)}$default"

  /** A parameter's type as the **caller** meets it, which for a rest parameter is not the type the
   * body sees. The fallback is the plain rendering, so a `rest` whose type is somehow not the
   * read-only view the parser builds prints something true rather than nothing.
   */
  private def paramTypeText(p: Param): String = (p.rest, p.typ) match
    case (true, ArrayType(None, elem, true)) => s"...${typeText(elem)}"
    // A by-name `m: -> int` is parsed to the thunk type `() -> int` its body sees; the caller
    // writes an `int`, so the declaration's form is what goes on the page.
    case (_, FnType(Nil, ret, true)) if p.byName => typeText(ret)
    case _                                   => typeText(p.typ)

  /** The parameter list with its parentheses, variadic marker included. */
  private def paramsText(params: List[Param], variadic: Boolean): String =
    val ps = params.map(paramText)

    s"(${(if variadic then ps :+ "..." else ps).mkString(", ")})"

  /** The result clause, which is absent rather than `-> ()` when nothing is returned.
   *
   * sysl writes a procedure with no arrow at all, so rendering the unit type would document a
   * spelling the language does not have.
   */
  private def retText(ret: Option[TypeRef]): String =
    ret.map(r => s" -> ${typeText(r)}").getOrElse("")

  /** A free function: `split_once[T](s: []const T, sep: T) -> Option[usize]`. */
  def func(f: FuncDecl): String =
    val tps = tparamsText(f.tparams, f.bounds, f.tvalues, f.tpacks, f.tdefaults)

    s"${f.name}$tps${paramsText(f.params, f.variadic)}${retText(f.retType)}"

  /** A member, with its receiver written the way the declaration wrote it.
   *
   * An associated function has no receiver and no `self`, which is a real distinction to a caller —
   * it is reached through the type rather than through a value — so the absence is rendered by
   * simply having no first parameter, exactly as the source does.
   */
  def method(m: MethodDecl): String =
    val tps = tparamsText(m.tparams, m.bounds, m.tvalues, Set.empty, m.tdefaults)
    val ps  = m.params.map(paramText)

    val all =
      m.recvMode match
        case Some(mode) => recvText(mode) :: ps
        case None       => ps

    val args = if m.variadic then all :+ "..." else all

    // A property is called without parentheses, so writing an empty pair would document a call the
    // language refuses. An instance property keeps its receiver, which is what says it is not an
    // associated value; a **static** one has none, so the word is the only thing that says a reader
    // writes `Type.name` rather than `value.name` — and that is the whole of what a caller needs.
    if m.isProperty && m.params.isEmpty then
      s"${if m.isStatic then "static " else ""}${m.name}$tps${retText(m.retType)}"
    else s"${m.name}$tps(${args.mkString(", ")})${retText(m.retType)}"

  /** How the receiver is spelled for each mode. */
  private def recvText(mode: RecvMode): String = mode match
    case RecvMode.ByValue      => "self"
    case RecvMode.ByPtr        => "*self"
    case RecvMode.ByRef(true)  => "&sync self"
    case RecvMode.ByRef(false) => "&self"

  /** A struct's head and its fields, as a block.
   *
   * The fields are included because a reader working out what a value costs needs to know how many
   * there are and what they hold — and because a struct with no members would otherwise render as a
   * single line saying nothing. A `private` field is included for the same reason and marked, since
   * "there is state here you cannot reach" is information rather than noise.
   */
  def struct(s: StructDecl): String =
    val tps  = tparamsText(s.tparams, s.bounds, s.tvalues, Set.empty, s.tdefaults)
    val head = s"${if s.opaque then "opaque " else ""}struct ${s.name}$tps"

    // AN OPAQUE STRUCT IS ITS HEAD ALONE. Its layout is withheld from every module but its own
    // (`reference/ffi.md § opaque`), so its fields are nothing a reader of the API may use — and it
    // may declare no body at all, which is the C-handle case and why it needs no `end` either.
    //
    // A STRUCT WITH NO FIELDS CARRIES ITS `end`, and that marker is required rather than optional —
    // one of the four cases the org's end-marker rule names. It is the only thing distinguishing a
    // deliberately empty body from one whose author forgot to indent it, and the compiler refuses
    // the bare head in as many words: "'struct Full' declares no fields — a struct's body is
    // indented under it, a type with no fields needs an 'end'".
    //
    // So rendering the head alone puts something on the page that is not sysl. Found by compiling
    // the juicerapi demo's blocks; `Stdout`, `Stderr` and `TtyWriter` are the real cases in the
    // library.
    if s.opaque then head
    else if s.fields.isEmpty then s"$head\nend ${s.name}"
    else s"$head\n${s.fields.map(f => s"    ${visText(f.vis)}${paramText(f)}").mkString("\n")}"

  /** The visibility a declaration was written with, followed by a space, or nothing where public. */
  def visText(vis: Visibility): String = vis match
    case Visibility.Public         => ""
    case Visibility.File           => "private "
    case Visibility.Scoped(module) => s"private[$module] "

  /** `type Name = [new] Base [within lo..hi] [where predicate]`, as it was written. */
  def typeDecl(t: TypeDecl): String =
    val derived = if t.derived then "new " else ""
    val range   = t.range.map { r =>
      s" within ${exprText(r.lo)}${if r.exclusiveHi then "..<" else ".."}${exprText(r.hi)}"
    }
    val pred = t.pred.map(p => s" where ${exprText(p)}")

    s"type ${t.name} = $derived${typeText(t.base)}${range.getOrElse("")}${pred.getOrElse("")}"

  /** An `extern` function, as a caller reaches it. The C symbol it links against is left out: it is
   * the binding's business, not the caller's, and it differs between platforms (`__stdoutp`).
   */
  def externFunc(e: ExternDecl): String =
    s"extern ${e.name}${paramsText(e.params, e.variadic)}${retText(e.retType)}"

  /** An `extern` variable: a C global, named and typed, its link symbol left out likewise. */
  def externVar(e: ExternVarDecl): String = s"extern ${e.name}: ${typeText(e.typ)}"

  /** A module-level `val`: its type where one was written, and its value as a constant's is. */
  def valDecl(v: ValDecl): String =
    s"val ${v.name}${v.typ.map(t => s": ${typeText(t)}").getOrElse("")} = ${exprText(v.value)}"

  /** An enum's head and its variants, payloads included. */
  def enumDecl(e: EnumDecl): String =
    val underlying = e.underlying.map(u => s": ${typeText(u)}").getOrElse("")
    val head =
      s"enum ${e.name}${tparamsText(e.tparams, e.bounds, e.tvalues, Set.empty, e.tdefaults)}$underlying"

    if e.variants.isEmpty then head
    else s"$head\n${e.variants.map(v => s"    ${variantText(v)}").mkString("\n")}"

  /** One variant: `None`, `Some(value: T)`, or `Red = 3` on a simple enum that pinned its constants.
   *
   * **A payload's fields are always named**, because the grammar parses them with the same
   * `fieldParam` a struct's fields use — there is no positional `Some(T)` in sysl. So there is no
   * unnamed case to render, and a generator that invented one would be documenting a spelling the
   * language refuses.
   */
  def variantText(v: EnumVariantDecl): String =
    if v.fields.nonEmpty then s"${v.name}(${v.fields.map(paramText).mkString(", ")})"
    else v.value.map(e => s"${v.name} = ${exprText(e)}").getOrElse(v.name)

  /** A trait's head, its associated types and its members. */
  def traitDecl(t: TraitDecl): String =
    val supers = if t.supers.isEmpty then "" else s": ${t.supers.map(boundText).mkString(" + ")}"
    val head   = s"trait ${t.name}${tparamsText(t.tparams, t.bounds, Map.empty, Set.empty, t.tdefaults)}$supers"
    val assocs = t.assocs.map(a => s"    type ${a.name}${assocBoundText(a)}")
    val ms     = t.methods.map(m => s"    ${method(m)}")
    val body   = assocs ::: ms

    if body.isEmpty then head else s"$head\n${body.mkString("\n")}"

  /** An associated type's bound clause, where it declared one. */
  private def assocBoundText(a: AssocDecl): String =
    if a.bounds.isEmpty then "" else s": ${a.bounds.map(boundText).mkString(" + ")}"

  /** A module constant: `const capacity: usize = 512`. */
  def const(c: ConstDecl): String = s"const ${c.name}: ${typeText(c.typ)} = ${exprText(c.value)}"

  /** An `impl` block's head — the trait, its arguments, and the type it is for.
   *
   * The methods are deliberately absent. An `impl` is documented as *the fact that this type has
   * this trait*, and its members' signatures are the trait's, already written where the trait is —
   * repeating them under every implementation is how a reference page becomes unreadable.
   */
  def implHead(i: ImplDecl): String =
    val tps  = tparamsText(i.tparams, i.bounds, i.tvalues, Set.empty, i.tdefaults)
    val args = if i.traitArgs.isEmpty then "" else s"[${i.traitArgs.map(typeText).mkString(", ")}]"

    s"impl$tps ${i.traitName}$args for ${typeText(i.forType)}"
}
