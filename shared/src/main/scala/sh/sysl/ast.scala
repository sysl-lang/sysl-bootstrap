package sh.sysl

/** The sysl abstract syntax tree.
 *
 * This is the *settled* surface only — the expression grammar of
 * `01-scalar-types-and-operators.md` plus the statement and declaration forms needed to
 * write and run real programs (functions, structs, loops). It grows as the language does;
 * nothing here is ported wholesale from the previous implementation, whose tree carried a
 * large amount of cut or deferred surface.
 *
 * The tree is *untyped*: the parser produces it structurally, and the analyzer
 * (`Analyzer`) turns it into a typed tree (`tast.scala`) that codegen consumes.
 *
 * Every node carries the source position the parser found it at (`Positioned`), which is what
 * lets a diagnostic quote the line and point at the column. The position is deliberately not a
 * constructor parameter, so structural equality is unaffected by it.
 *
 * **The tree spans five files, and which node is in which is not a matter of taste.** `Expr`,
 * `Stmt`, `Pattern` and `TypeRef` are each sealed, so each one's nodes have to sit together: this
 * file holds `Expr` and every expression — the loops and `match` among them, since those yield a
 * value — while `astPatterns.scala`, `astTypes.scala` and `astStmts.scala` hold the other three.
 * `astFile.scala` holds what is not a node at all: `Program`, and the clauses a file's header
 * carries.
 */

/** Every expression node is a **case class**, and saying so in the type is what lets a walk that has
 * nothing to say about any particular node read its children off the product rather than matching
 * arm by arm (`Aliasing.exprKids`). Such a walk is total by construction, which is the property that
 * matters: a node added below without a case of its own is descended into rather than silently
 * treated as a leaf.
 */
sealed trait Expr extends Positioned, Product

case class IntLit(value: BigInt, suffix: Option[String]) extends Expr
case class FloatLit(text: String, suffix: Option[String]) extends Expr
case class CharLit(codepoint: Int)                        extends Expr
case class StrLit(value: String)                          extends Expr

/** `c"…"` — a NUL-terminated C string, of type `*u8`. It is the shape a C interface reads a string
 * as, which a sysl `string` is not: that carries a length and no terminator (`04`).
 */
case class CStrLit(value: String) extends Expr
case class BoolLit(value: Boolean)                        extends Expr

/** `()` — the sole value of `unit`. */
case class UnitLit() extends Expr

/** `null` — the absent raw pointer. It has no type of its own and takes the `*T` its context
 * expects; there is no null in the safe subset, where an absent reference is `Option[&T]`.
 */
case class NullLit() extends Expr

case class Ident(name: String) extends Expr

/** A prefix operator: `-x`, `!b`, `~n`, `*p`, `&x`. */
case class Unary(op: String, operand: Expr) extends Expr

case class PreIncDec(op: String, operand: Expr)  extends Expr
case class PostIncDec(op: String, operand: Expr) extends Expr

case class Binary(op: String, left: Expr, right: Expr) extends Expr

/** A comparison chain: `a < b < c` holds `operands = [a, b, c]`, `ops = ["<", "<"]` and
 * means `a < b && b < c`. A single comparison is just the two-operand case.
 */
case class Compare(operands: List[Expr], ops: List[String]) extends Expr

/** `a..b` (inclusive) / `a..<b` (exclusive), with either end optionally open. */
case class RangeExpr(lo: Option[Expr], hi: Option[Expr], inclusive: Boolean) extends Expr

/** Assignment as an expression (`=` and the compound forms), right-associative. */
case class Assign(op: String, target: Expr, value: Expr) extends Expr

case class Call(callee: Expr, args: List[Expr]) extends Expr

/** `name = value` at a call — the argument stands at the parameter it names rather than at the one
 * its position would have given it (`reference/declarations.md § Default parameters and named
 * arguments`).
 *
 * It is an `Expr` so that it travels in `Call.args` beside the positional arguments it is mixed
 * with, and every call form binds its arguments before looking at any of them. Reaching
 * `analyzeExpr` means it was written somewhere no declaration is being called, which is what the
 * arm there reports.
 */
case class NamedArg(name: String, value: Expr) extends Expr

/** `xs...` in an argument list — the one argument that is **already** the slice a `...T` parameter
 * collects (`reference/declarations.md § A parameter may collect the rest of the call`).
 *
 * Without it a rest parameter could only ever be given values written out one by one, so a function
 * that forwards its own tail to another could not be written at all — which is the case Go added
 * the same spelling for. It is not an operator and not an expression anywhere else: it says how one
 * argument is bound, exactly as `name = value` beside it does, and the analyzer refuses it wherever
 * there is no rest parameter for it to be about.
 */
case class Spread(value: Expr) extends Expr

/** A parameter's default, spliced in at a call that left the argument out
 * (`reference/declarations.md § Default parameters and named arguments`).
 *
 * Synthesized by argument binding and never parsed, so it carries the position of the default as
 * written rather than one of its own.
 *
 * `owner` is the key of the declaration the default was written in, and is the scope it is analyzed
 * under: a default names what its own file names, wherever it is called from. It is absent for a
 * **nested** function, whose declaration has no key of its own and whose every call is inside the
 * body it was written in — so the scope already in force is the one it was written in.
 */
case class DefaultArg(owner: Option[String], value: Expr) extends Expr
case class Index(receiver: Expr, index: Expr)   extends Expr
case class Field(receiver: Expr, name: String)  extends Expr

/** `f[A, B]` — brackets holding *more than one* thing, which is never an index.
 *
 * A subscript takes one index, so a comma inside the brackets is what tells the two readings apart
 * without asking what the name is. It exists for the one place a list of types is written — `&f[A,
 * B]`, the address of an instantiation (`reference/ffi.md § A function's address`) — and every
 * other position refuses it.
 *
 * The single-argument form `&f[T]` has no comma to distinguish it and arrives as an `Index`, which
 * the analyzer re-reads where the name is a generic function. That asymmetry is deliberate: it is
 * the *analyzer* that decides between an index and a type-argument list, and this node only spares
 * it the cases the grammar could never have built.
 */
case class TypeArgs(receiver: Expr, args: List[Expr]) extends Expr

/** `T::Attr` — a type attribute (`reference/errors.md § What the type's own name offers: ::
 * attributes`, `reference/types.md § Enums`): metadata a type exposes under a name, with `::`
 * rather than `.` because it belongs to the type itself, not to a value of it. `Age::First`,
 * `Day::Succ(d)`. The receiver is a type name; a bare `Attr` reads a value, and `Attr(args)` is a
 * `Call` over this node, exactly as an enum's associated function is a `Call` over a `Field`.
 */
case class TypeAttr(receiver: Expr, attr: String) extends Expr

/** One field of a `with` clause: `bg = ACCENT`. Positioned at the **name**, so a field the struct
 * does not have is complained about where it was written rather than at the `with`.
 */
case class WithField(name: String, value: Expr) extends Positioned

/** `base with { bg = ACCENT }` — the value `base` again, with the fields named here changed
 * (`reference/expressions.md § with`).
 *
 * It is the two-statement form written as one expression, and that is the whole of the rule: a copy
 * of `base` is bound, each field is assigned to in the order written, and the copy is the value. So
 * a struct's invariant is rechecked, a private field is refused, a conversion happens, and a
 * settable property runs its setter — every one of them because the assignment it desugars to is an
 * ordinary assignment, and none of them because this form said anything about it.
 *
 * **The base must be a struct value**, which is a rule this node has to state rather than inherit:
 * `p with { … }` where `p` is a `&Style` would bind another reference to the same object and write
 * through it, changing what every other holder sees. That is the one reading the desugaring gets
 * wrong, so it is refused by name.
 */
case class WithExpr(base: Expr, fields: List[WithField]) extends Expr

/** `.red`, `.Circle(3)`, `.make(2)` — a member selected from the type the context **expects**,
 * with the type's own name left off (`reference/expressions.md § A leading dot`).
 *
 * It is the qualified form with the qualifier dropped and nothing more: `.red` means what
 * `Colour.red` means wherever a `Colour` is what is wanted, so it reaches a variant, an enum's
 * `try`, and an associated function — exactly what the written qualifier reaches. What supplies the
 * qualifier is the expected type, so a position with no expectation has nothing to resolve against
 * and the analyzer says so rather than reporting an undefined name.
 *
 * `.name(args)` is a `Call` over this node, the way `Type.name(args)` is a `Call` over a `Field`.
 */
case class ImplicitMember(name: String) extends Expr

/** `sizeof(T)` / `alignof(T)` — how much storage a type occupies and what it must be aligned to
 * (`reference/memory.md § Reinterpreting storage`).
 *
 * The operand is a **type**, which is why this is a node of its own rather than a `Call` the analyzer
 * recognizes by name: an argument list holds values, and the whole type grammar is written here —
 * `sizeof(*Node)`, `sizeof([16]u8)`, `sizeof((int, real))`. `op` is the word that was written, so one
 * node carries both and the two never drift apart.
 */
case class LayoutOf(op: String, typ: TypeRef) extends Expr

/** `offsetof(T, field)` — where a field starts inside the struct it is written in, in bytes
 * (`reference/memory.md § Reinterpreting storage`).
 *
 * The other half of what `@assert` needs to hold a mirrored C struct to its original
 * (`reference/attributes.md § @assert — a condition settled while compiling`). `sizeof` pins the
 * total, which catches a field that changed width or one that was added; it says nothing about
 * **order**, so two same-width fields transposed in the mirror leave the size right and every read
 * wrong. This is what turns that into a refusal.
 *
 * It is not a `LayoutOf` with a third field because its operands are of two kinds — a type and then a
 * name — and folding it asks a different question of `Layout`.
 */
case class OffsetOf(typ: TypeRef, field: String) extends Expr

/** The postfix `?` error-propagation operator. */
case class TryExpr(expr: Expr) extends Expr

case class Tuple(elements: List[Expr]) extends Expr

/** One of a closure literal's parameters. Its type is written only where nothing else can supply
 * one (`reference/expressions.md § Closures`), so the annotation is optional here in a way a
 * declared function's never is.
 */
case class LambdaParam(name: String, typ: Option[TypeRef]) extends Positioned

/** `x -> x + 1` — a closure literal (`reference/expressions.md § Closures`). The body is a
 * statement list for the same reason a function's is: an indented block's trailing expression is
 * its value, and the `= expr` short form is that list with one statement in it.
 *
 * `implicitParams` is set on the one closure a program does not write the parameters of: the
 * [[BlockArg]] a trailing block becomes at a callable parameter, whose single parameter is bound as
 * `it` (`reference/expressions.md § A trailing block`). It says *name the parameters from the arity*
 * rather than saying how many there are, because the place that builds the closure cannot yet know:
 * argument binding is asked of the parameter's type **as written**, and a bare arrow has already
 * been rewritten into a bounded type parameter carrying no arity. `analyzeLambda` is where the
 * expected signature arrives, and is where the name is given.
 */
case class Lambda(params: List[LambdaParam], body: List[Stmt], implicitParams: Boolean = false)
    extends Expr

/** The indented block a call may write after a `:`, standing at one of its parameters.
 *
 * It is **neutral about what it is**, and it has to be: the parser reads the block before anything
 * has resolved the callee, so it does not know whether the parameter the block stands at wants a
 * collection or a callable. Argument binding is where a parameter and its argument are first known
 * to be a pair, so that is where this becomes an [[ArrayLit]] of the block's lines or a [[Lambda]]
 * over them, and nothing downstream ever sees one.
 *
 * The body is a statement list for the reason a [[Lambda]]'s is: an indented block is what was
 * written, and the collection reading is the case that additionally requires every line to be an
 * expression.
 */
case class BlockArg(body: List[Stmt]) extends Expr

/** `[a, b, c]` — an array literal, whose length is how many elements were written. An empty
 * one has no element type of its own and takes it from the context.
 */
case class ArrayLit(elements: List[Expr]) extends Expr
case class ArrayFill(value: Expr, count: Expr) extends Expr

/** An indented block standing where a value is wanted — the block a binding's `=` introduces
 * (`reference/lexical.md § An unbracketed line continues after an operator`).
 *
 * It is the same thing a function body and a branch already are: a statement list whose trailing
 * expression is its value, `never` where it ends in a jump, and `unit` where it ends in neither. What
 * makes it a node of its own is that a binding has nowhere else to put one — `if`, `match` and the
 * loops each hold their blocks inside the node the block belongs to, and `val x =` has no such node.
 *
 * **A block of one expression is that expression**, collapsed by the parser rather than represented:
 * `val X: int =` with `42` under it is the constant tree it looks like, not a computed initializer
 * wrapping one. The scope a block opens is what a single expression has no use for, so nothing is
 * lost by not building it.
 */
case class Block(stmts: List[Stmt]) extends Expr

/** `if cond then a else b` as an **expression**: it yields the value of the taken branch.
 * In statement position the `else` may be omitted and the whole thing has type `unit`.
 * Each branch is a statement list whose trailing expression is the branch's value.
 */
case class IfExpr(cond: Expr, thenBody: List[Stmt], elseBody: Option[List[Stmt]]) extends Expr

/** `scrutinee match` with indented arms — an **expression** yielding the taken arm's value
 * (or `unit` in statement position). Arms are tried top to bottom.
 */
case class MatchExpr(scrutinee: Expr, arms: List[MatchArm]) extends Expr

/** `subject is Pat` / `subject is not Pat` — a pattern tested where a condition is wanted
 * (`reference/expressions.md § is — a pattern where a condition is wanted`). It yields a `bool`
 * and, in the un-negated form, binds whatever the pattern names for the rest of the condition and
 * the branch the condition guards.
 *
 * `patterns` holds the `|`-alternatives an arm's left side may hold, and under the same rule: they
 * share one answer, so none of them may bind.
 *
 * Its position is checked rather than its type: it is a term of an `if`'s or a `while`'s condition
 * and nowhere else, so that the reach of a binding is one sentence long. Everywhere else the
 * analyzer refuses it, which is why this stays an ordinary `Expr` — the grammar has one place to put
 * it and one rule to give it back.
 */
case class IsPattern(subject: Expr, patterns: List[Pattern], negated: Boolean) extends Expr

/** `a, b` where a function's own result list is what is being produced (`reference/declarations.md
 * § Several results`) — the callee's side of the form. It is never a value: the analyzer accepts it
 * only where the enclosing function's declared result is a list, and builds the aggregate the
 * caller takes apart.
 */
case class ResultList(values: List[Expr]) extends Expr

/** `['label] while cond body [else elseBody]` as an **expression**. A `break expr` in the body
 * makes `expr` the loop's value; the optional `else` runs on normal completion (the condition
 * became false with no break) and its trailing expression is the loop's value on that path. With
 * no `else`, normal completion yields `unit`, so a value-carrying `break` needs an `else` to give
 * a matching value when the loop finishes on its own. An optional `label` names the loop so a
 * `break`/`continue` in a nested loop can reach it.
 */
case class While(label: Option[String], cond: Expr, body: List[Stmt], elseBody: Option[List[Stmt]]) extends Expr

/** `['label] do body while cond [else elseBody]` — the post-test loop, whose body runs once before
 * anything is asked.
 *
 * The value rules are `while`'s exactly: a `break expr` carries the loop's value, and the `else`
 * runs on normal completion, which here means the test at the foot finally failed. What it carries
 * that `while` cannot is the same thing the three-clause `for` carries — a `continue` runs the
 * **test** rather than restarting the body blind. The shape a program reaches for otherwise is
 * `loop` with `if !cond then break` at the foot, and that rewrite is not this loop: a `continue`
 * added to it jumps over the test and never leaves.
 *
 * `cond` is an ordinary boolean rather than a condition's term list, for the reason the three-clause
 * `for`'s is: a test at the foot of the body has no branch after it for an `is` binding to be read
 * from.
 */
case class DoWhile(label: Option[String], body: List[Stmt], cond: Expr, elseBody: Option[List[Stmt]]) extends Expr

/** `['label] loop body` — a loop with no condition, which runs until something leaves it.
 *
 * It is `while true` with the part that was never a question taken out, and it carries one thing
 * that spelling cannot: a loop nothing breaks out of does not finish, so its type is `never` and
 * the code after it is unreachable. There is no `else`, because `else` runs on normal completion
 * and this loop has none — the value it yields is the value its `break`s carry, and `unit` where
 * they carry none.
 *
 * `threaded` is `@threaded` written above it (`reference/attributes.md § @threaded`): the body ends
 * in a `match` over an enum, and each arm of that match runs the body's head again and dispatches
 * from where it stands, rather than every arm branching back to one shared dispatch.
 */
case class Loop(label: Option[String], body: List[Stmt], threaded: Boolean = false) extends Expr

/** `['label] for name in iter body [else elseBody]` — an **expression** with the same
 * `break`/`else` value rules as `while`. `iter` is a range for now (`a..b`, `a..<b`).
 */
case class For(label: Option[String], name: String, iter: Expr, body: List[Stmt], elseBody: Option[List[Stmt]])
    extends Expr

/** `for const name in iter body` — the loop the compiler **unrolls** (`reference/generics.md § A
 * parameter may stand for a list of types`).
 *
 * `iter` is a range whose ends are compile-time constants, and the body is repeated once per value
 * with `name` folded in as that value. The copies are type-checked *separately*, which is the whole
 * point: `self.0` and `self.1` have different types and one written line covers both.
 *
 * It carries no label and no `else`, and neither is an omission. There is no loop at run time for a
 * `break` to leave or for an `else` to run after — what the analyzer produces is the copies in a
 * block, so nothing downstream of it ever meets this node.
 */
case class ConstFor(name: String, iter: Expr, body: List[Stmt]) extends Expr

object ConstFor {

  /** How many copies one `for const` may be unrolled into.
   *
   * A bound rather than no bound, because the cost is in the *emitted program* and not in a number
   * a reader can see: `for const i in 0..<100000` is one line and a hundred thousand copies of
   * whatever is under it. The limit is generous against what the feature is for — a tuple wide
   * enough to reach it is one nobody should be writing (`reference/types.md § Tuples`) — and a loop
   * that genuinely counts that high is the ordinary `for`, which costs one copy however far it
   * goes.
   */
  val maxCopies: Int = 64
}

/** `['label] for init; cond; step` — the three-clause loop, written without parentheses as Go
 * writes it, since every other header in the language is parenthesis-free.
 *
 * Each clause may be empty. It is an **expression** with the same `break`/`else` rules as `while`,
 * whose `else` runs when the condition turns false. What it carries that `while` cannot is the
 * **step**: `continue` runs it before testing again, which is the whole reason a counted loop
 * written as a `while` is a bug waiting for its first `continue`. A binding introduced by the init
 * is scoped to the loop — the condition, the step, the body, and the `else`.
 */
case class CFor(
    label: Option[String],
    init: Option[Stmt],
    cond: Option[Expr],
    step: Option[Stmt],
    body: List[Stmt],
    elseBody: Option[List[Stmt]],
) extends Expr

/** `for all i in 0..<n do P(i)` / `for some i in 0..<n do P(i)` — a quantifier over an integer
 * range, yielding a `bool` (`reference/verification.md § for all and for some`).
 *
 * `universal` tells the two apart. The bound name is visible only inside `pred`, and `iter` is a
 * range expression — the same `RangeExpr` a counted `for` takes, so the two forms cannot drift
 * apart over what a range is.
 */
case class Quantifier(universal: Boolean, name: String, iter: Expr, pred: Expr) extends Expr
