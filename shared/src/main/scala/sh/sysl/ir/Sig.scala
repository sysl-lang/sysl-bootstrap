package sh.sysl
package ir

/** **A signature, as data** — what a function answers with, what it takes, and what the convention
 * attaches to each of those.
 *
 * This is the half of the IR that stayed text longest, and the reason is worth stating: a body is
 * made of instructions and an instruction is obviously a node, while a signature *looks* like one
 * string. It is not. `define internal x86_intrcc zeroext i1 @f(ptr byval(%struct.Frame) %a0)` holds
 * a linkage, a calling convention, a return attribute, a result type, a symbol, and a parameter
 * carrying an attribute that names another type — six decisions made in five different files, joined
 * with `+` at the end. A back end handed the string has to take all six apart again.
 */

/** A **parameter attribute**, which is what a calling convention adds to a parameter beyond its type.
 *
 * There are seven, and the number is a measurement rather than a design: these are every attribute
 * the compiler emits anywhere, read off the tree rather than off LLVM's manual, which lists dozens
 * sysl has no use for. An eighth belongs here the day something emits one.
 *
 * **The distinction that matters is that an attribute is not storage.** `sret(%struct.Point)` names
 * a type and the parameter is still a `ptr`: what the attribute says is what is at the far end and
 * whose job it is to put it there. So a type and its attributes are kept apart here rather than run
 * together into one "declared" string, which is what made `noalias` removable only by
 * `replace("noalias ", "")` in the one place an adapter has to drop it.
 */
enum Attr {

  /** The caller's storage for a result too big to come back in registers. The callee writes through
   * it and the declared result is `void`.
   */
  case SRet(ty: LType)

  /** An aggregate the **caller** copies onto its own stack, which is System V's answer and
   * WebAssembly's. The rest hand over a real pointer, and marking one `byval` there is a different
   * call — see `CAbi.stackCopy`.
   */
  case ByVal(ty: LType)

  /** The boundary the storage a pointer parameter addresses sits on. */
  case Align(n: Int)

  /** The boundary the **argument slot itself** sits on, which is a different claim from `Align` and
   * is why it is a case of its own: AAPCS64 away from Darwin hands an array-shaped aggregate over in
   * a slot aligned to eight whatever the elements are.
   */
  case AlignStack(n: Int)

  /** Nothing else addresses this storage. It is true of an `sret` slot the caller just made and
   * false of one forwarded through an adapter, which is the whole of why it is droppable.
   */
  case NoAlias

  /** The two halves of `CAbi.extension` — whoever hands a narrow scalar over widens it first, and
   * which of the two it is follows the type's own signedness everywhere but RISC-V 64.
   */
  case ZeroExt
  case SignExt

  def render: String = this match
    case SRet(ty)     => s"sret(${ty.render})"
    case ByVal(ty)    => s"byval(${ty.render})"
    case Align(n)     => s"align $n"
    case AlignStack(n) => s"alignstack($n)"
    case NoAlias      => "noalias"
    case ZeroExt      => "zeroext"
    case SignExt      => "signext"

  override def toString: String = render
}

object Attr {

  /** Several attributes as they are written together — in the order given, because that is the order
   * LLVM's own grammar puts them in and the order the conventions were measured writing them.
   */
  def text(attrs: List[Attr]): String = attrs.map(_.render).mkString(" ")
}

/** One declared parameter: its type, whatever the convention attaches, and the name a **definition**
 * gives it.
 *
 * `name` is `None` in a `declare`, which is the whole difference between the two forms — a
 * declaration says what a call has to supply and has no body to refer to it from. Modelling it as an
 * absent name rather than as two types is what lets one signature be printed either way, which is
 * exactly what `Codegen` needs: a library's own functions are declared from the shape their
 * *definition* would have had.
 */
case class Param(ty: LType, attrs: List[Attr] = Nil, name: Option[Val] = None) {

  def render: String =
    (ty.render :: attrs.map(_.render) ::: name.map(_.render).toList).mkString(" ")

  override def toString: String = render
}

/** **A function's type** — what it answers with and what it takes.
 *
 * It exists apart from `FuncSig` because a variadic call has to name one: the argument list alone
 * does not say where the declared parameters stop and the ellipsis begins, so `call i32 (ptr, ...)
 * @printf(…)` states the whole type at the call site. That is the same information a `declare` holds
 * less the symbol, which is why it is one type here rather than two.
 */
case class FnType(ret: LType, params: List[Param], variadic: Boolean = false,
                  retAttrs: List[Attr] = Nil) {

  /** The result as a signature writes it: the attributes in front of the type, which is where LLVM
   * puts them in a `define`, a `declare` and a `call` alike. A `ret` instruction takes none, and
   * names the type alone.
   */
  def result: String = (retAttrs.map(_.render) :+ ret.render).mkString(" ")

  /** The parameters between their parentheses, ellipsis included where there is one. */
  def paramList: String =
    (params.map(_.render) ::: (if variadic then List("...") else Nil)).mkString(", ")

  def render: String = s"$result ($paramList)"

  override def toString: String = render
}

/** How visible a symbol is outside the module that defines it.
 *
 * A function and a global draw from the same set and do not use the same part of it: a `define`
 * writes nothing for the default, where a global has to say `external` outright, and `appending` is
 * a global's only. That is LLVM's asymmetry rather than one chosen here, and one enum with a note
 * beats two that would have to agree.
 */
enum Linkage {

  /** External: the default, and what everything a linker has to find carries. Written by a
   * `define` as nothing at all — `declare` and `define` already say which side of the link this is.
   */
  case Default

  /** Visible nowhere, and renameable — what a runtime helper and a string constant take. */
  case Private

  /** Visible inside this module only. A file-private function's (`reference/modules.md §
   * Visibility`): every caller is in the module that defines it, so nothing outside may resolve it
   * and the linker may discard it.
   */
  case Internal

  /** Storage some other object file lays down, named here so this module can reach it. */
  case External

  /** Each module contributes its own and the linker concatenates them, which is what makes
   * `@llvm.used` a list rather than a name several modules fight over (`reference/attributes.md §
   * @section("...")`).
   */
  case Appending

  def prefix: String = this match
    case Default   => ""
    case Private   => "private "
    case Internal  => "internal "
    case External  => "external "
    case Appending => "appending "
}

/** **A whole signature**: the type, the symbol, and everything the module-level declaration says
 * about the function that a call does not need to know.
 *
 * One value prints as either form. `define` writes the linkage, the calling convention, the function
 * attributes and the section; `declare` writes none of them, because a declaration describes what a
 * caller must supply and those four are properties of the definition. Keeping them in one type is
 * what lets `Codegen` declare a precompiled function the way its *definition* would have been
 * spelled, which is not what `foreignSignature` would have produced and is the difference between a
 * working link and arguments passed the other way round.
 */
case class FuncSig(name: String, ty: FnType, linkage: Linkage = Linkage.Default,
                   cconv: Option[String] = None, attrs: List[(String, String)] = Nil,
                   section: Option[String] = None, bareAttrs: List[String] = Nil) {
  // `cconv` and `attrs` are the two fields here that are still LLVM's own spelling, and knowingly:
  // both carry an interrupt handler's declaration (`reference/ffi.md § interrupt`), which LLVM
  // writes two different ways — x86-64's is a calling convention and RISC-V's a function attribute
  // — and neither is a set the compiler closes, since a new target may bring its own. `Conventions`
  // is where they come from.
  //
  // `bareAttrs` is the third, and it is a **separate list because LLVM has two spellings** rather
  // than because the two mean different things. An enum attribute is a keyword — `noinline`,
  // `cold` — and a string attribute is a quoted pair; writing one where the other belongs is not
  // accepted, so which list a name goes in is the whole of the distinction and a single list of
  // pairs could not express it.


  /** The `define` line, without the brace the printer adds. */
  def define: String =
    val bare = bareAttrs.map(" " + _).mkString
    val fn   = attrs.map((k, v) => s""" "$k"="$v"""").mkString
    val sec  = section.map(s => s""" section "$s"""").getOrElse("")

    s"define ${linkage.prefix}${cconv.map(_ + " ").getOrElse("")}${ty.result} " +
      s"@${LlvmName.safe(name)}(${ty.paramList})$bare$fn$sec"

  /** The `declare` line. */
  def declare: String = s"declare ${ty.result} @${LlvmName.safe(name)}(${ty.paramList})"

  /** The symbol as an operand — what a call to this names. */
  def symbol: Val.Global = Val.Global(name)

  override def toString: String = define
}
