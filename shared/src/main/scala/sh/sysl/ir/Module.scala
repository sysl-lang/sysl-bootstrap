package sh.sysl
package ir

/** **A whole compiled module, as data** — and the thing a second back end is actually handed.
 *
 * Everything else in this package describes a piece: a type, an operand, an instruction, a
 * signature, a function. This is what codegen produces, and until it existed there was no way to
 * *get* the IR at all — `Codegen.gen` assembled the pieces into a `StringBuilder` and answered with
 * a `String`, so a consumer that was not LLVM had to parse the compiler's own output back into the
 * shapes the compiler had just finished deciding.
 *
 * **The fields are groups rather than one list, because the order is semantic and the grouping is
 * where the ordering rules live.** A named struct used before its `= type` line is opaque, and an
 * opaque type cannot be passed by value — so `imports` comes after `structs` and an `external
 * global` naming an aggregate comes after it too. Flattening these into one sequence would keep the
 * order and lose the reason for it.
 */
case class Module(triple: String, declares: List[FuncSig], structs: List[TypeDef],
                  enums: List[TypeDef], boxes: List[TypeDef], imports: List[FuncSig],
                  globals: List[Global], runtime: List[Runtime], funcs: List[Func],
                  thunks: List[Func], entry: Option[Func], used: Option[Global],
                  init: Option[Initializer] = None)

/** **What fills a module's computed storage where there is no entry point to fill it in** — the
 * constructor and the list that gets it called (`reference/modules.md § val — a thing`).
 *
 * The two are one value because neither is anything on its own. A function nothing registers is
 * dead code the linker discards; a registration naming no definition does not link. Carrying them
 * as a pair is what keeps a back end from being handed half of the mechanism.
 *
 * `list` is `@llvm.global_ctors`, whose element is LLVM's own `{ i32, ptr, ptr }` — a priority, the
 * function, and a global the entry is tied to the liveness of. It is `appending`, so each object
 * file contributes its own and the linker concatenates them rather than several modules fighting
 * over one name, exactly as `@llvm.used` does.
 */
case class Initializer(func: Func, list: Global)

/** `%struct.Point = type { i32, i32 }` — a name for an aggregate, at module level.
 *
 * It writes its own braces rather than borrowing `LType.Struct`'s, because a definition is not a
 * reference: `@packed` is `<{ }>`, which is a property of the declaration and has no spelling in a
 * type used anywhere else.
 */
case class TypeDef(name: String, fields: List[LType], packed: Boolean = false) {

  def render: String =
    val body =
      if packed then fields.map(_.render).mkString("<{ ", ", ", " }>")
      else fields.map(_.render).mkString("{ ", ", ", " }")

    s"${LlvmName.safeSigiled(name)} = type $body"

  override def toString: String = render
}

/** Module-level storage: a `val`'s, a string constant's, a method table's, or the declaration line
 * for storage the linker supplies.
 *
 * `value` is absent exactly where this module lays nothing down — an `external global`, which is a
 * declaration wearing the same syntax. Everything else carries a **constant expression**, which is
 * an `ir.Val` and not a rendered string: the whole point of the initializer being constant is that
 * it is data the object file holds rather than instructions somebody runs.
 */
case class Global(name: String, constant: Boolean, ty: LType, value: Option[Val] = None,
                  linkage: Linkage = Linkage.Private, section: Option[String] = None,
                  align: Option[Int] = None,
                  /** `thread_local` — one copy of this object per thread. It is written with **no
                    * model in parentheses**, which leaves the choice to the back end: `local-exec`
                    * where the object is in the executable itself, `initial-exec` or the general
                    * dynamic form where a shared library may be involved. The keyword's position in
                    * the grammar is after the linkage and before `global`/`constant`.
                    */
                  threadLocal: Boolean = false) {

  def render: String =
    val kind = if constant then "constant" else "global"
    val init = value.map(" " + _.render).getOrElse("")
    val sec  = section.map(s => s""", section "$s"""").getOrElse("")
    val at   = align.map(n => s", align $n").getOrElse("")
    val tls  = if threadLocal then "thread_local " else ""

    s"@${LlvmName.safe(name)} = ${linkage.prefix}$tls$kind ${ty.render}$init$sec$at"

  override def toString: String = render
}

/** **A function the module needs whose body did not come from sysl source** — the ownership
 * runtime, the string operations, the UTF-8 encoder.
 *
 * Two cases, and the split is the whole of what this type is for. Most of these are *generated*: a
 * destructor for a payload type, a vtable adapter, the retain and release helpers — built by the
 * ordinary emitters through `inFunction`, so they are `Func`s like any other and a consumer reads
 * them as data. The rest are **hand-written LLVM text**, and there is nothing to hand a back end
 * that is not LLVM except the name of what the function has to do.
 *
 * So a template carries its name, and the name is the request. A back end that cannot use the text
 * matches on `sysl.str.concat` and supplies its own; one that can, writes the text out. Before this
 * the two were indistinguishable — both were strings in a queue — and a module was a set of calls to
 * functions a non-LLVM consumer had no way to identify, let alone provide.
 */
enum Runtime {

  /** Generated by the emitters, and therefore data. */
  case Emitted(func: Func)

  /** Hand-written LLVM, named so that a back end which cannot read it knows what to write. */
  case Template(name: String, llvm: String)
}
