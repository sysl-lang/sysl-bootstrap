package sh.sysl

import scala.collection.mutable

/** Turning a program **body** into a program.
 *
 * A caller holding sysl source as a *string* rather than as a file — a documentation page's code
 * block, a test's inline program — has the statements a program runs and nowhere to put them. This
 * supplies the `main` they belong to, so the string may be written as the thing being shown rather
 * than as a whole program wrapped around it.
 *
 * **The split is structural and could not be textual**, which is the whole reason this exists rather
 * than each caller doing it with string operations. At a file's top level `f()` is a call and
 * `f() -> int = x` is a declaration, and nothing about the characters before the newline separates
 * them — the grammar does, by whether a block follows. So the source is parsed first and partitioned
 * by node, and a caller that cannot parse cannot do this correctly.
 */
object Bodies {

  /** Whether a top-level statement is a **declaration** — hoisted, order-free, and not part of what
   * the program runs.
   *
   * This answers **which file the program starts in**: the one carrying something this is false of.
   * A file holding only a table of constants and the functions that read it declares a lot and runs
   * nothing, and must not thereby become the file the program starts in.
   *
   * It is also what `wrapBody` moves, for the same reason — the statements a body carries are the
   * ones a `main` has to be built around.
   *
   * **A `var` is deliberately not one of these**, because a binding with an initializer is something
   * that runs and a file carrying one really may be a body. Whether it is *this* program's body is a
   * question about the other files too, so it is asked in `ModuleFiles.entryFile` rather than here —
   * see `isTopLevelBinding` below.
   */
  def isDeclaration(s: Stmt): Boolean = s match
    case _: FuncDecl | _: StructDecl | _: EnumDecl | _: TraitDecl | _: ImplDecl | _: ExternDecl |
        _: ExternVarDecl | _: ImportDecl | _: ConstDecl | _: ValDecl | _: TypeDecl | _: StaticDecl |
        // An `@assert` runs nothing: it is settled while compiling and emits no code, so a file
        // holding one is still a file of pure declarations and does not become the entry file.
        _: AssertDecl =>
      true
    case _ => false

  /** Whether a top-level statement is a **binding** — the one kind that may be either a body's local
   * or its module's storage, depending on what the rest of the program looks like (`reference/modules.md § Where a program starts`).
   *
   * Everything else settles which file the program starts in on its own: a `print` runs and a
   * `struct` does not, whatever else the program contains. A `var` is the one form that does not,
   * because both readings are coherent — a local of the body it was written in, or storage the
   * module owns — and nothing in the line itself chooses between them. `ModuleFiles.entryFile`
   * chooses, by asking whether any file carries a statement that is not one of these.
   */
  def isTopLevelBinding(s: Stmt): Boolean = s.isInstanceOf[VarDecl]

  /** Whether a declaration belongs to the **module** even when it is written in the file the program
   * starts in.
   *
   * The file that carries the statements is a **body**, so what it declares is local to that body:
   * a `val` there is a stack local initialized where it is written, a `var` there is an ordinary
   * mutable local, and a function there is a nested function (`reference/declarations.md`) that
   * captures the locals above it.
   *
   * What stays behind is everything with **no runtime identity**: a type names a shape, a `const` is
   * folded into its uses before anything runs, an `extern` names storage somebody else owns, an
   * `import` binds a name for the file. None of them occupies storage or captures anything, and a
   * body has nothing to make them local *to* — `StmtAnalysis` refuses every one of them in a block.
   *
   * This is narrower than `isDeclaration` on purpose, and the two questions really are different: the
   * one above asks which file runs, and is asked of every file; this one asks what that file's own
   * declarations mean, and is asked only of it.
   */
  def isModuleMember(s: Stmt): Boolean = s match
    case _: StructDecl | _: EnumDecl | _: TraitDecl | _: ImplDecl | _: ExternDecl | _: ExternVarDecl |
        _: ImportDecl | _: ConstDecl | _: TypeDecl | _: StaticDecl =>
      true
    case _ => false

  /** Which of the entry file's functions belong to its **body** rather than to its module — the ones
   * that read something the body binds, and the ones that call those.
   *
   * A function written at the top of the entry file is a nested function only if it has an
   * environment to be nested *in*. `reference/declarations.md` says as much outright: one that
   * captures nothing and does not escape "is an ordinary static function with a private name". So
   * the three things a nested function cannot be — generic, addressable, a value — are consequences
   * of holding a frame, not of where the declaration was written, and charging them to a function
   * that holds no frame would be charging the common case for the rare one. A comparison handed to
   * `qsort` is the shape that makes this concrete: it reads nothing, and refusing its address on
   * the ground that "what would have to travel beside the address is the frame it reads" names a
   * frame that does not exist.
   *
   * **Capture is transitive through a sibling call**, which is what the fixpoint is for: the nested
   * functions of a block share one environment (`reference/declarations.md`), so a function calling
   * one that captures needs that environment too and is part of the group whether or not it reads
   * anything itself.
   *
   * This is purely syntactic, and has to be: it decides what the hoisting passes are *given*, so it
   * runs before any scope exists to resolve a name against. Over-approximating is therefore the safe
   * direction, and shadowing is honoured so that a helper whose own parameter is spelled like one of
   * the body's bindings is not dragged in by the spelling.
   */
  def capturing(body: List[Stmt]): Set[String] = {
    // What the body binds is what a function could be reading. A `static` one is the module's, so it
    // is not among them — which is the whole of what the modifier does.
    val bindings = body.flatMap {
      case VarDecl(n, _, _, _, _, _, _) => List(n)
      case ValDecl(n, _, _, _, _, _)    => List(n)
      case RefDecl(n, _)        => List(n)
      case MultiDecl(ns, _, _)  => ns
      case PatternDecl(p, _, _) => patternNames(p)
      case _                    => Nil
    }.toSet

    // `main` is never one of them, however it is written. It is not a name the program calls but the
    // one the *platform* calls, so it has no caller inside the body to have formed an environment for
    // it — and a `main` that read one of the body's bindings would have quietly stopped being the
    // program's entry point rather than being told it could not have them.
    val funcs = body.collect { case f: FuncDecl if f.name != "main" => f }
    val free  = funcs.map(f => f.name -> freeNames(f)).toMap
    var group = funcs.filter(f => free(f.name).exists(bindings)).map(_.name).toSet
    var more  = true

    while more do
      val next = group ++ funcs.filter(f => free(f.name).exists(group)).map(_.name)

      more = next.size > group.size
      group = next

    group
  }

  /** Every name `f` reads that `f` does not bind — its parameters, its locals, and everything a
   * nested block of its own introduces are all shadows, exactly as they are anywhere else.
   *
   * A binding is in scope for what comes *after* it, so a declaration's initializer is walked outside
   * the shadow it casts: `var n = n` reads the outer `n`.
   */
  private def freeNames(f: FuncDecl): Set[String] = {
    val found = mutable.LinkedHashSet.empty[String]

    def walk(node: Any, bound: Set[String]): Set[String] = node match
      case Ident(n) =>
        if !bound(n) then found += n
        bound
      case VarDecl(n, _, init, _, _, _, _) => init.foreach(walk(_, bound)); bound + n
      case ValDecl(n, _, v, _, _, _)       => walk(v, bound); bound + n
      case RefDecl(n, p)         => walk(p, bound); bound + n
      case ConstDecl(n, _, v, _) => walk(v, bound); bound + n
      case MultiDecl(ns, _, vs)  => vs.foreach(walk(_, bound)); bound ++ ns
      case PatternDecl(p, _, v)  => walk(v, bound); bound ++ patternNames(p)
      case g: FuncDecl           => walk(g.body, bound ++ g.params.map(_.name)); bound
      case Lambda(ps, b, _)      => walk(b, bound ++ ps.map(_.name)); bound
      case For(_, n, it, b, e) =>
        walk(it, bound)
        walk(b, bound + n)
        e.foreach(walk(_, bound))
        bound
      case Quantifier(_, n, it, p) =>
        walk(it, bound)
        walk(p, bound + n)
        bound
      case MatchArm(ps, guard, b) =>
        val inArm = bound ++ ps.flatMap(patternNames)

        // What the patterns *read* — the backticked names, which reference rather than bind (`09`).
        // A walk of its own rather than the general one, which would also descend into a
        // `LitPattern`'s expression and could call a name there free: a change to every match arm
        // rather than to the form that is new.
        for n <- ps.flatMap(patternReads) do if !bound(n) then found += n

        guard.foreach(walk(_, inArm))
        walk(b, inArm)
        bound
      case stmts: List[?] => stmts.foldLeft(bound)((b, s) => walk(s, b))
      case p: Product     => p.productIterator.foreach(walk(_, bound)); bound
      case _              => bound

    walk(f.body, f.params.map(_.name).toSet)
    found.toSet
  }

  /** Every name a pattern **reads** — the backticked ones, which reference something already
   * declared rather than binding it (`09`). Almost always empty, which is the point: every other
   * form binds a name or holds a literal.
   */
  private def patternReads(p: Pattern): List[String] = p match
    case EqPattern(n)          => List(n)
    case BindPattern(_, inner) => patternReads(inner)
    case VariantPattern(_, ps) => ps.flatMap(patternReads)
    case TuplePattern(ps)      => ps.flatMap(patternReads)
    case StructPattern(_, fs)  => fs.flatMap((_, sub) => patternReads(sub))
    case _                     => Nil

  /** Every name a pattern binds, which shadows for the arm it introduces. */
  private def patternNames(p: Pattern): List[String] = p match
    case IdentPattern(n)       => List(n)
    case BindPattern(n, inner) => n :: patternNames(inner)
    case VariantPattern(_, ps) => ps.flatMap(patternNames)
    case TuplePattern(ps)      => ps.flatMap(patternNames)
    case StructPattern(_, fs)  => fs.flatMap((_, sub) => patternNames(sub))
    case _                     => Nil

  /** The declarations of `u`, plus a `main` holding whatever else it carried.
   *
   * A unit that carries no statements is returned as it stands, so this is safe to apply to a whole
   * program, to a library, or to a body, and a caller needs to know which it has only if it cares.
   *
   * Where the unit **already declares a `main`**, the statements go to the front of that function's
   * body rather than into a second one. That is the order they already had — a program's top-level
   * statements are its initialization and run before the `main` it declared — so a body that mixes
   * the two keeps its meaning rather than becoming a duplicate-`main` error.
   */
  def wrapBody(u: Program): Program = {
    // The same split `ProgramWalk` makes, because a body *is* the top level of a file the program
    // starts in and wrapping it must not change what it means. So a plain `val` moves into the `main`
    // as the local it was, a helper that reads one moves with it, and only what belongs to the module
    // stays outside.
    //
    // Where a declaration said `static`, the modifier has done its work by being read here and the
    // wrapper leaves the plain declaration behind: once the statements are inside a `main`, the file
    // has no body for anything to belong to instead, and the modifier would then be refused for
    // saying nothing.
    val captures       = capturing(u.body)
    val (decls, stmts) = u.body.partition {
      case f: FuncDecl => !captures(f.name)
      case other       => isModuleMember(other)
    }
    val plain = decls.map {
      case StaticDecl(inner) => inner
      case other             => other
    }

    // A unit carrying no statements is **not** a body, and is returned exactly as it stands. That
    // guard is what makes this safe to apply file by file: only one file of a program is its body, so
    // a sibling holding a table and the functions that read it must not have its `val` read as a
    // local and carried into a `main` of its own.
    if u.body.forall(isDeclaration) then u
    else
      plain.indexWhere { case f: FuncDecl => f.name == "main"; case _ => false } match
        case -1 =>
          val main = FuncDecl("main", Nil, Nil, None, stmts).setPos(stmts.head.pos)

          u.copy(body = plain :+ main)
        case i =>
          val existing = plain(i).asInstanceOf[FuncDecl]

          u.copy(body = plain.updated(i, existing.copy(body = stmts ::: existing.body).setPos(existing.pos)))
  }

  /** Parses one body and wraps it, which is the whole of what a caller holding a string needs. */
  def parseBody(source: Source, target: Target = Target.default): Either[String, Program] =
    SyslParser.parse(source, target).map(wrapBody)
}
