package sh.sysl

/** Holds a definition marked `@export` to what a C caller can actually name and call
 * (`reference/ffi.md § @export`).
 *
 * **`@export` is `extern` read the other way**, and that symmetry is what decides every rule here.
 * An `extern` names a symbol the linker has and states the signature the other side published; an
 * `@export` publishes a symbol and states the signature C may call it at. Neither invents a shape the
 * other end could not spell, so what this pass refuses is exactly what C has no way to say.
 *
 * **A declaration pass, not part of the body walk**, for the reason `ConventionCheck` gives: every
 * rule is about the *signature*, and a signature exists whether or not a body is ever analyzed. A
 * generic marked `@export` has no instantiation and so no body to walk, and it is exactly as wrong as
 * one that does.
 *
 * **The refusals are the point, and they are cheap because of where they land.** A restriction that
 * fired while somebody wrote ordinary sysl would be a language that keeps saying no. These fire only
 * on a definition the author marked, in the boundary layer they wrote on purpose — the same facade a
 * Scala program exposing itself to Java or JS grows — so the diagnostic reads as a reminder of what
 * the boundary is for rather than as an obstruction. There are simply some things a function cannot
 * be and stay C-callable, and saying which, where somebody tries, is the whole of the work.
 *
 * **What is here is what a declaration answers on its own**, which turns out to be everything about
 * the *shape* of the function and nothing about its types. Two kinds of rule are in `Exports`
 * instead, and both for the same reason — they need something a declaration does not have:
 *
 *   - **the parameter and result types**, because resolving a short name needs the scope the
 *     declaration sits in, and a `P` written inside `module demo` means `demo.P` only to a walk that
 *     is standing there. The typed tree has already answered that, so asking it again here would be
 *     re-deriving a resolution that exists — and getting it wrong for exactly the modules a real
 *     binding is written in;
 *   - **whether an export reaches a computed module `val`**, which is a question about the call
 *     graph and so exists only once every body has been analyzed.
 */
trait ExportCheck extends TypeResolution {

  /** Holds every declaration carrying `@export` — a free function, and a struct naming itself for a
   * generated header.
   *
   * **A free function and a struct are the whole of it, and that is the grammar's doing rather than
   * a choice made here.** `SyslParser.attributedDecl` reads attributes at statement position only,
   * so no attribute reaches a struct member, an enum member or an `impl` member — `@test` and
   * `@pure` are as unavailable there as this one. A rule refusing `@export` on a method would
   * therefore be a rule nothing could reach, and the refusal a reader actually meets comes from the
   * parser.
   */
  protected def checkExports(): Unit = {
    for
      f <- funcDecls.values.toList
      e <- f.exported
    do recover(())(at(e.pos)(checkExport(e, f)))

    for
      s <- structDecls.values.toList
      e <- s.cname
    do recover(())(at(e.pos)(checkStructExport(e, s)))

    for
      (key, t) <- constrainedDecls.toList
      e        <- t.cname
    do recover(())(at(e.pos)(checkTypeExport(e, key, t)))
  }

  /** Refuses a `type` a header could not declare as a `typedef` — the struct's rules for its name,
   * and building the type, which is where a base C cannot spell is refused. Building it here rather
   * than waiting for a use is what makes the refusal unconditional: a published name is a claim
   * whether or not anything in this module happens to write it.
   */
  private def checkTypeExport(e: ExportAttr, key: String, t: TypeDecl): Unit = {
    for n <- e.symbol if !ExportCheck.cIdentifier(n) do
      err(s"'$n' is not a name C can declare — a type's name in a header is a C identifier, so it " +
        "is a letter or '_' followed by letters, digits and '_'")

    if e.symbol.isEmpty && !ExportCheck.cIdentifier(Modules.bare(key)) then
      err(s"'${qn(key)}' is not a name C can declare, so naming it after itself would put a " +
        "spelling in the header no C declaration could use — name it instead, '@export(\"...\")'")

    if t.vis != Visibility.Public then
      err(s"'${qn(key)}' is private, so no exported function may name it — an export is public, " +
        "and a declaration may not be more visible than the types it names. No header can carry " +
        "this type at all, so there is no name in one for it to take")

    at(t.pos)(resolveConstrained(key))
  }

  /** Refuses a C name a header could not declare, or a struct that has no one name to give.
   *
   * **The rules are the function's, arrived at from a different direction.** A symbol and a type
   * name are both spellings a C reader writes, so what C can name is the same question — but a
   * `typedef` has no linkage, so `private` is not the contradiction here that it is on a function.
   * It is refused anyway, and for a reason worth reading: the visibility rule already stops a public
   * declaration naming a type less visible than itself, and an exported function is public. So no
   * private struct can appear in any signature a header carries, and naming one for a header is
   * naming it for a file it can never be in.
   */
  private def checkStructExport(e: ExportAttr, s: StructDecl): Unit = {
    for n <- e.symbol if !ExportCheck.cIdentifier(n) do
      err(s"'$n' is not a name C can declare — a type's name in a header is a C identifier, so it " +
        "is a letter or '_' followed by letters, digits and '_'")

    // Checked against the declared name, since that is what an unwritten one becomes. A
    // backtick-quoted declaration (`09`) is the case: it may hold anything the reader wanted.
    if e.symbol.isEmpty && !ExportCheck.cIdentifier(Modules.bare(s.name)) then
      err(s"'${qn(s.name)}' is not a name C can declare, so naming it after itself would put a " +
        "spelling in the header no C declaration could use — name it instead, '@export(\"...\")'")

    // Not the function's reason — a type name has no linkage to contradict — but the same answer,
    // reached through the visibility rule instead: a public declaration may not name a type less
    // visible than itself, and an exported function is public. So a private struct appears in no
    // signature a header carries, and this would be naming it for a file it can never be in.
    if s.vis != Visibility.Public then
      err(s"'${qn(s.name)}' is private, so no exported function may name it — an export is public, " +
        "and a declaration may not be more visible than the types it names. No header can carry " +
        "this type at all, so there is no name in one for it to take")

    // Generic is refused for the reason an exported generic function is, and the argument is even
    // shorter here: every instantiation is a struct of its own, so one written name would be
    // claimed by all of them at once.
    if s.tparams.nonEmpty then
      err(s"a header names one type at one shape, so '${qn(s.name)}' cannot be generic — each " +
        "instantiation is a struct of its own and they would all claim this one name. Wrap the " +
        "instantiation the header carries in a struct of its own and name that")
  }

  /** Refuses an export whose symbol C could not name, or whose function C could not call. */
  private def checkExport(e: ExportAttr, f: FuncDecl): Unit = {
    for s <- e.symbol if !ExportCheck.cIdentifier(s) do
      err(s"'$s' is not a name C can call — an exported symbol is a C identifier, so it is a " +
        "letter or '_' followed by letters, digits and '_'")

    // **An overload exported under its own name is refused here**, and it is worth catching by name
    // rather than leaving to the rule below. An overload's key carries a numbered segment, which is
    // not a C identifier — so the check below already refuses it, with a message quoting a spelling
    // the source does not contain (`reference/declarations.md § Overloading`). C has no overloading
    // at all: two exports of one name are two definitions of one symbol, which is a link error
    // rather than anything a reader of either line could act on.
    if e.symbol.isEmpty && overloadKeys(overloadPlain(f.name)).length > 1 then
      err(s"'${qn(f.name)}' is declared more than once, and C has no overloading — two exports of " +
        "one name would be two definitions of one symbol. Give each export a symbol of its own, " +
        "'@export(\"...\")'")

    // Checked against the *declared* name, since that is what an unwritten symbol becomes. A
    // backtick-quoted name (`09`) is the case: it may hold anything the reader wanted, and the
    // escape that makes it an LLVM label is not something a C header could declare.
    if e.symbol.isEmpty && !ExportCheck.cIdentifier(Modules.bare(overloadPlain(f.name))) then
      err(s"'${qn(f.name)}' is not a name C can call, so exporting it under its own name " +
        "would publish a symbol no C declaration could spell — name the symbol instead, " +
        "'@export(\"...\")'")

    // Generic is refused before anything is resolved, for `ConventionCheck`'s reason: a type
    // parameter has nothing to resolve *to* out here, and asking what `T` is would be asking a
    // question the declaration has already made meaningless.
    if f.tparams.nonEmpty then
      err(s"an exported symbol is one function at one signature, so '${f.name}' cannot be generic — " +
        "there would be no way to say which instantiation the linker holds. Export a wrapper per " +
        "instantiation, each naming its own symbol")

    // `private` emits the symbol `internal` (`reference/modules.md § Visibility`), which is the
    // promise that every caller is inside this module. An export is the opposite promise, and a
    // definition cannot make both.
    if f.vis != Visibility.Public then
      err(s"'${Modules.show(f.name)}' is private, which promises every caller is inside its own " +
        "module — and an export promises the opposite. A definition cannot make both claims")

    if f.ghost then
      err(s"'${Modules.show(f.name)}' is '@ghost', so it is erased before codegen and there is no " +
        "symbol for C to link against")

    if f.test.isDefined then
      err(s"'${Modules.show(f.name)}' is a '@test', which only 'sysl test' builds — an exported " +
        "symbol has to be in the artifact a C project links, and a test is not")

    // A variadic sysl definition reads its tail through `va_start`/`va_arg`, which is C's own
    // mechanism — but the *promotions* a C caller applies to the tail are the caller's, and nothing
    // here has seen that caller's prototype. `extern` is safe the other way round because the
    // published prototype is what the declaration transcribes.
    if f.variadic then
      err(s"'${Modules.show(f.name)}' is variadic, and what a C caller promotes into the tail is " +
        "decided by the prototype it compiled against rather than by this declaration — take a " +
        "'va_list' parameter instead, which states the same thing and is what C's own 'v' " +
        "variants do")

  }
}

object ExportCheck {

  /** The sentence every signature refusal ends with, written once so the two cannot drift apart. */
  val spellable: String =
    "an integer, a float, a 'bool', a 'char', a pointer, a function pointer, a simple enum, or a " +
      "struct built out of those"

  /** Whether a type crosses to C **as itself**, which is a narrower question than whether it has a
   * meaning over there.
   *
   * A scalar and a pointer are one register on every machine sysl lowers for, and the answer needs
   * nothing decided. **An aggregate of them is here too**, and used not to be: each ABI says which
   * registers a struct arrives in and LLVM applies no rule of its own, so what an aggregate needs is
   * that somebody apply the convention — which is exactly what `CAbi` is, and what an exported
   * function's entry now goes through (`ExportThunk`). Refusing one was the honest answer while
   * `@export` was a rename and nothing else; it is the wrong answer now that the entry is lowered
   * the way the target's document says, and it was what stopped a binding exporting the callbacks a
   * C library asks for — the shape every one of them is written in.
   *
   * What is still refused is what has no C **declaration** at all, whatever convention is applied to
   * it. A slice and a `string` are two words with a length in the second, a `&T` is a counted box
   * whose header C would have to know the shape of, and a trait object is a pair with a method
   * table. Each is a real sysl value and none of them is something a header could spell.
   *
   * **An aggregate is asked about its fields, and that is what keeps the two rules apart.** A struct
   * of scalars is a struct C declares; a struct with a `&T` in it is a counted box with a coat on,
   * and passing it would hand C a refcount to look after. Recurring is also what keeps ownership out
   * of the thunk entirely: every type that reaches it is plain data, so the boundary takes and
   * releases nothing.
   */
  def crosses(t: Type): Boolean = Type.repr(t) match
    // **An array is not a parameter or a result in C at all**, though it is a perfectly good field.
    // C decays an array parameter to a pointer, so there is no prototype that passes one by value —
    // which makes this the one place the two positions differ, and the reason there are two
    // functions here rather than one. A struct with an array in it is how C says this, and it
    // classifies identically.
    case _: Type.Array => false
    case other         => carried(other)

  /** The same question at a **field**, which admits the one thing a signature does not.
   *
   * The split is C's rather than sysl's: `int32_t xs[3]` is a member and is not a parameter.
   * Everything else answers the same in both positions, so this is also where the recursion lives —
   * an aggregate is asked about what it carries, and a signature asks this of its own type.
   */
  def carried(t: Type): Boolean = Type.repr(t) match
    case _: Type.Integer | _: Type.Floating => true
    case Type.Bool | Type.Char              => true
    case _: Type.CFn                        => true
    // An **erased** pointer is a trait object — the value beside its method table — so it is a pair
    // of words rather than the one word every other pointer is, and the `Trait` advice is the one
    // that applies to it. It reached here as a plain pointer until 0137, and was lowered as a pair
    // that C had no declaration for.
    case p: Type.Ptr                        => !Type.erased(p)
    // A constrained subtype is its base with a check at the point a value is *made*, so a derived
    // one crosses exactly as its base does — nothing about the representation differs, and the
    // C side is not making the value.
    case c: Type.Constrained                => carried(c.base)
    case s: Type.Struct                     => s.stored.forall((_, f) => carried(f))
    case a: Type.Array                      => carried(a.elem)
    // A simple enum **is** its underlying integer (`EnumAttrEmitter`), so it crosses as one. A data
    // enum is a tag beside a union of payloads sysl laid out, which is not what a C union is.
    case e: Type.Enum                       => e.simple
    case _                                  => false

  /** The one line of help the refusal can honestly give, which differs by what was written. Anything
   * with no specific advice gets none rather than a guess.
   */
  def advice(t: Type): String = Type.repr(t) match
    case _: Type.Slice | Type.Str =>
      "A slice is an address and a length, so C takes them as two parameters — a pointer and a " +
        "'usize' — which is the shape its own string and buffer functions already have"
    case _: Type.Ref | _: Type.Weak =>
      "A counted reference has a header C would have to know the layout of. Hand out a raw pointer " +
        "and keep the counted one on this side"
    case _: Type.Trait =>
      "A trait object is a value and a method table together. Export one function per " +
        "implementation, or take a function pointer"
    case p: Type.Ptr if Type.erased(p) =>
      "A pointer to a trait is the value and its method table together, so it is two words rather " +
        "than the one every other pointer is. Point at the concrete type instead"
    // An aggregate itself crosses now, so arriving here means one of its **fields** does not — and
    // naming the field is the whole of what the reader needs, since the declaration they are looking
    // at does not mention it.
    case s: Type.Struct =>
      s.stored.find((_, f) => !carried(f)) match
        case Some((name, f)) =>
          s"'$name' of ${Type.show(t)} is ${Type.show(f)}, which is what C has no declaration for — " +
            s"the aggregate around it is fine. ${advice(f)}"
        case None => ""
    // An array is refused for a reason of its own — it is a field in C and never a parameter — so
    // the advice depends on which of the two it is. An element C cannot spell is the reader's
    // problem; an element it can means the array itself is the only thing in the way.
    case a: Type.Array if !carried(a.elem) =>
      s"its element is ${Type.show(a.elem)}, which is what C has no declaration for — the array " +
        s"around it is fine. ${advice(a.elem)}"
    case _: Type.Array =>
      "C decays an array parameter to a pointer, so there is no prototype that passes one by " +
        "value. Wrap it in a struct, which is how C hands an array over as a value and lays out " +
        "identically, or take a pointer to its first element and a length"
    case e: Type.Enum if !e.simple =>
      "A data enum is a tag beside a union of the payloads sysl laid out, which is not the shape a " +
        "C union has. Export one function per variant, or take the payload as its own type"
    case _ => ""

  /** Whether an exported `type` may be declared as a C `typedef` of `t` (`reference/ffi.md § Naming
   * a type`).
   *
   * **A scalar or a pointer, and nothing with a layout.** An aggregate has a name of its own in the
   * header already — `@export` on the struct chooses it — so a second name for it would be a
   * `typedef` the header had no reason to write; a slice, a counted reference and a data enum are
   * the shapes no C declaration spells at all. A function pointer is a pointer, and an alias of an
   * alias is asked about what it stands for.
   */
  def typedefable(t: Type): Boolean = t match
    case c: Type.Constrained => typedefable(c.base)
    case _ =>
      Type.repr(t) match
        case _: Type.Integer | _: Type.Floating | Type.Bool | Type.Char => true
        case c: Type.Constrained                                         => typedefable(c.base)
        case p: Type.Ptr                                                 => !Type.erased(p)
        case _: Type.CFn                                                 => true
        case _                                                           => false

  /** What to write instead of a `typedef` C could not spell — there is always something. */
  def typedefAdvice(t: Type): String = Type.repr(t) match
    case _: Type.Struct =>
      "Put '@export(\"...\")' above the struct itself, which names it in the header, or export a " +
        "pointer to it"
    case e: Type.Enum if e.simple =>
      "A simple enum is spelled as the integer it is, so export an alias of that integer"
    case _: Type.Array =>
      "Wrap the array in a struct and name that, which is how C hands one over as a value"
    case other =>
      val a = advice(other)
      if a.nonEmpty then a else "Export an alias of a scalar or a pointer"

  /** Whether a string is a name C could declare. ISO C also reserves a leading underscore at file
   * scope, which is a rule about *which* names a program may take rather than about what is a name —
   * so it is left to the author, exactly as C leaves it.
   */
  def cIdentifier(s: String): Boolean =
    s.nonEmpty && (s.head.isLetter && s.head < 128 || s.head == '_') &&
      s.forall(c => c < 128 && (c.isLetterOrDigit || c == '_'))
}
