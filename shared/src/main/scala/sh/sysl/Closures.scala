package sh.sysl

import scala.collection.mutable

/** A nested function declared in a block (`reference/declarations.md`): the lowered name it is
 * called by, and the environment its call is passed.
 *
 * The environment is a `*Env` expression rather than a name, because the same nested function is
 * reached two ways — from the block that declared it, where it is the address of the local holding
 * the environment, and from inside a sibling's body, where it is the receiver that body already
 * has. That is what makes mutual recursion a call on the receiver rather than a capture.
 */
case class Nested(fname: String, env: TExpr, variadic: Boolean = false, params: List[Param] = Nil)

/** The environment a body reads its captures out of (`reference/expressions.md § Closures`, `§5a`).
 *
 * `byReference` is the one difference between the two things that have one. A **closure literal**
 * captures by value, because it may outlive the frame it was written in: its fields hold copies and
 * shares, and `§7` says so. A **nested function** captures by address, because it may not — nothing
 * outside the body can name one, so it cannot be stored or returned, and a body of statements that
 * could not assign to the variable it was written beside would not be worth the name it has.
 */
case class Environment(
    struct: Type.Struct,
    names: List[String],
    byReference: Boolean,
    fixed: Set[String],
)

/** Closures (`reference/expressions.md § Closures`–`§8`): the arrow literal, what it captures, and
 * the type it inhabits.
 *
 * **A closure is a struct and an `impl`, and that is the whole reduction.** The struct's fields are
 * the variables the body names from the scope it was written in; the `impl` is of the call trait for
 * the closure's arity (`Fn2[A, B, R]`, spelled `Fn(A, B) -> R`), and its one member is the body with
 * those names reaching the fields instead of the locals. Everything downstream then already works
 * and is not asked to learn anything new: layout, ARC, monomorphization, trait objects, and the
 * static and dynamic halves of `§6` are the two a bound and a trait object already were.
 *
 * What is genuinely new is here and is only this — deciding which names a body captures, and binding
 * them inside the body so that reading one reaches the field. **Nothing rewrites the body**: a
 * capture is declared in the closure's own scope like any other name, with a note saying where it
 * lives, so shadowing, assignment, and a closure inside a closure are the ordinary cases of rules
 * that already hold rather than three more things for the walk to get right.
 */
object Closures {

  /** What every closure struct's base name begins with. It holds a `$`, which no identifier and no
   * module name may, so nothing a program can write collides with one — the same reason a tuple's
   * base holds one (`reference/types.md § Tuples`).
   */
  private val prefix = s"${Modules.sep}closure"

  /** And what the environment shared by one block of nested functions begins with, for the same
   * reason (`reference/declarations.md`).
   */
  private val envPrefix = s"${Modules.sep}env"

  /** The base name of the struct one closure literal lowers to. */
  def base(n: Int): String = s"$prefix$n"

  /** The base name of the struct one block of nested functions shares. */
  def envBase(n: Int): String = s"$envPrefix$n"

  /** Whether a type is the struct behind a closure literal — something a program wrote and did not
   * name. What a reader is told about one has to say "closure" and "captures", never the name the
   * compiler filed it under (`reference/types.md § Function types`).
   */
  def literal(t: Type): Boolean = t match
    case s: Type.Struct => s.base.startsWith(prefix)
    case _              => false

  /** Whether a symbol is one of the functions a closure literal lowered to — its `call` body, or one
   * of the hooks the struct carries. Asked where a diagnostic would otherwise name it: the compiler's
   * own filing is not something a reader wrote and not something they can go and look at.
   */
  def symbol(name: String): Boolean = name.startsWith(prefix)

  /** The name a trailing block's one parameter is bound as, where the callable it stands at takes
   * one (`reference/expressions.md § A trailing block`).
   *
   * It is an ordinary parameter of the closure and not a keyword: it is not reserved, it shadows an
   * outer name of its own accord, and a block that has no use for the value simply never writes it.
   * Kotlin's, and for Kotlin's reason — the block already *is* a closure, so this only names what it
   * was passed. What it deliberately does not do is extend: two parameters have no positional
   * spelling here, because `$` is [[Modules.sep]] and no source name may hold one.
   */
  val it: String = "it"

  /** Whether a function's name is one the **lowering** made up rather than one a reader wrote — a
   * closure's body or a nested function's.
   *
   * Wider than `symbol` by the nested-function case, and asked where the two are the same thing: a
   * diagnostic naming one of these names nothing the program contains, and a rule stated over what a
   * reader wrote has nothing to say about it.
   */
  def lowered(name: String): Boolean = name.startsWith(prefix) || name.startsWith(envPrefix)

  /** Whether a symbol's name **carries** a closure's rather than being one: `$closure4.call` is one,
   * and so is `sysl.time$resolve.$closure4`, the instantiation of a bare-arrow parameter made at it.
   *
   * The distinction from `lowered` is where the closure's name sits, and it matters because both are
   * subject to the same rule: a closure's name is a counter in the compilation that lowered it, so
   * two units each lowering a fourth closure produce the same name for two different things. Every
   * symbol answering this is therefore emitted `internal` and may not cross a link — which is what
   * `StdArtifactTests` holds the standard module's own artifact to.
   *
   * **Segment by segment rather than as a substring**, because the prefix begins with the module
   * separator and a module's own declaration called `closure4` is keyed `foo$closure4`. A mangled
   * name joins its parts with `.` (`Type.mangled`), and a closure's base is a part in its own right,
   * so a real one always starts a segment and that one never does.
   */
  def mentioned(name: String): Boolean =
    name.split('.').exists(part => part.startsWith(prefix) || part.startsWith(envPrefix))

  /** Whether a lowered closure body ever **writes** through the environment it was handed.
   *
   * A closure captures by value (`reference/expressions.md § Closures`), so its captures are fields
   * of its own struct and a write to one is a store reaching `self`. That makes this the question
   * `call`'s receiver mode is really about: a body that only reads its captures needs the address
   * for nothing but reading, and asking a caller for a place it may write is asking for something
   * the body will not use.
   *
   * **Asked of the typed tree, for the reason `Purity`'s question is** — a write is a *node*, and
   * the four below are all of them, so a fifth added later is visibly absent from this list rather
   * than silently missing from a condition spread across the analyzer.
   *
   * A closure nested inside this one is lowered separately and carries its own `self`, so its body
   * is not walked here and its writes are its own.
   */
  def writesEnvironment(body: Any): Boolean = {
    def reachesSelf(place: TExpr): Boolean = place match
      case TLoad("self", _)      => true
      case TDeref(inner, _)      => reachesSelf(inner)
      case TField(recv, _, _)    => reachesSelf(recv)
      case TIndex(recv, _, _)    => reachesSelf(recv)
      case TSlice(base, _, _, _, _) => reachesSelf(base)
      case _                     => false

    def walk(x: Any): Boolean = x match
      case _: Type                     => false
      case TStore(place, _, _)         => reachesSelf(place)
      case TUpdate(place, _, _, _, _, _) => reachesSelf(place)
      case TIncDec(place, _, _, _, _)  => reachesSelf(place)
      case TVecStore(recv, _, _)       => reachesSelf(recv)
      case xs: Iterable[?]             => xs.exists(walk)
      case p: Product                  => p.productIterator.exists(walk)
      case _                           => false

    walk(body)
  }
}

trait Closures extends CallAnalysis {

  /** How many closures have been lowered, which is what makes each one's name its own. */
  private var closureCount = 0

  /** How many blocks of nested functions have been lowered, likewise. */
  private var environmentCount = 0


  /** A closure literal (`reference/expressions.md § Closures`), and the closure a trailing block
   * became.
   *
   * The parameter types come from the context asking for a callable and the result comes from the
   * body — never the other way round, so a closure is analyzed once and what it yields is what it
   * yields.
   *
   * **A block wrote no parameter list, so this is where it gets one.** `implicitParams` turns the
   * check below inside out for that case: what the context asks for *names* the parameters rather
   * than being compared against names the reader chose.
   */
  protected def analyzeLambda(l: Lambda, expected: Option[Type]): TExpr = {
    val want = expected.flatMap(callableSignature)

    val params = if l.implicitParams then blockParams(l, want.map(_._1)) else l.params

    for (ws, _) <- want if ws.length != params.length do
      if l.implicitParams then
        err(s"a trailing block binds the one value it is passed as '${Closures.it}', so it stands " +
          s"at a callable taking one or none — and what this one is being used as takes " +
          s"${ws.length}. Write it as a closure literal, which names its parameters: " +
          s"'(${(1 to ws.length).map(i => ('a' + i - 1).toChar).mkString(", ")}) -> …'")
      else
        err(s"this closure takes ${quantity(params.length, "parameter")}, and what it is being " +
          s"used as takes ${ws.length}")

    // An annotation is read where one is written and the context supplies the rest. A parameter with
    // neither is the one shape a closure cannot be analyzed at all, and it is reported against the
    // parameter rather than against the literal, since that is where the answer would go.
    //
    // A placeholder's parameter is named by the compiler (`reference/expressions.md § _ — a
    // parameter with the name left out`), so the advice that fits a written one — annotate it —
    // names something the program is not able to write. What it is told instead is the form that
    // has somewhere to put the annotation.
    //
    // **The type position is an ellipsis and not a `T`.** These two were the only diagnostics in
    // the tree quoting a spelling with a metavariable in it, and the place this one fires is
    // exactly where that bites: a call into a generic member, whose own signature very likely calls
    // something `T`, so a reader copying the advice writes a name that means nothing at the call.
    // `…` cannot be copied and says the same thing.
    val ptypes = params.zipWithIndex.map { (p, i) =>
      p.typ.map(resolveType(_, tsubst))
        .orElse(want.flatMap((ws, _) => ws.lift(i)))
        .getOrElse(at(p.pos)(err(
          if Placeholders.isPlaceholder(p.name) then
            "this '_' has no type here — nothing says what the closure it stands in takes, so " +
              "write that closure with a named parameter: '(x: …) -> …'"
          else
            s"'${p.name}' has no type here — nothing says what this closure takes, so write it: " +
              s"'(${p.name}: …) -> …'",
        )))
    }

    lowerClosure(params.map(_.name), ptypes, want.flatMap(_._2), l.body, l.pos)
  }

  /** What a trailing block's closure calls its parameters, which is the arity it stands at and
   * nothing the block wrote (`reference/expressions.md § A trailing block`).
   *
   * One parameter is [[Closures.it]]. **None is the empty list, and so is anything else** — a block
   * cannot name two, and handing back nothing is what lets the arity check above say so in the
   * sentence a reader who wrote no parameters needs, rather than in the one that counts parameters
   * they did not write.
   *
   * Nothing said what the block stands at where `want` is empty, and the empty list is right there
   * too: the closure is analyzed with no parameters, exactly as one written `() -> …` would be, and
   * whatever refuses that refuses this.
   */
  private def blockParams(l: Lambda, want: Option[List[Type]]): List[LambdaParam] =
    if want.exists(_.length == 1) then List(LambdaParam(Closures.it, None).setPos(l.pos)) else Nil

  /** Builds the struct, the implementation and the body of one closure, and yields the struct value
   * that *is* the closure — its captures, in the order the fields hold them.
   */
  protected def lowerClosure(
      names: List[String],
      ptypes: List[Type],
      result: Option[Type],
      body: List[Stmt],
      pos: Option[Pos],
  ): TExpr = {
    val captured = captures(body, names.toSet)

    refuseRefCaptures(captured)

    val fields   = captured.map(n => (n, lookupOpt(n).get._2))

    val struct = Type.Struct(Closures.base(closureCount), Nil)

    closureCount += 1
    struct.fields = fields
    structInsts(struct.base) = struct

    val name = s"${Type.mangle(struct)}.call"

    // A closure written inside a test is the test's, and is scaffolding on both counts the header
    // says (`reference/attributes.md § @tests — a file of scaffolding`): it may name what the test file declared, and every build that drops the
    // test drops it too. Recorded before the body is analyzed, so that a closure nested inside this
    // one is judged against a set this one is already in.
    if inTestBody then testOnlyDecls += name

    val (func, ret) = at(pos)(analyzeNested(name, names.zip(ptypes), result, body,
      Some(Environment(struct, captured, byReference = false, fixed(captured)))))

    closureFuncs += func
    funcInsts(name) = (func.params.map((n, t) => (n, t)), ret)

    // Asked once, here, where the body has just been analyzed and before anything can call it.
    // What it decides is whether a `val` holding this closure may be called (`DeclTables`), which is
    // a property of the body rather than of any call site — so a closure that only reads answers the
    // same at every one of them.
    if !Closures.writesEnvironment(func.body) then readOnlyClosures += struct.base

    registerCallTrait(struct, ptypes, ret, pos)

    // Every capture is read where the closure is *formed* (`reference/expressions.md § Closures`),
    // so a value is copied in and a `&T` takes a share — which is what the ordinary field-by-field
    // construction of a struct already does, and the reason capture needed no rule of its own.
    TStructNew(struct, captured.map(n => analyzeExpr(Ident(n).setPos(pos)))).setPos(pos)
  }

  /** The nested functions of one block, lowered together (`reference/declarations.md`).
   *
   * **They share one environment**, and that is the whole design. A nested function *is* a closure —
   * capture follows §7 and representation follows §8, both unchanged — but the block's functions are
   * members of one struct rather than one struct each, which is what makes the two halves of §5a's
   * rule both true at once: every name is in scope throughout the block, so two may call each other,
   * while what may be *captured* is settled at the position the group is written.
   *
   * A sibling call and a recursive call are then the same thing — a call on the receiver the body
   * already has — so neither takes a share of anything, and §5a's reference cycle never forms.
   *
   * The environment is built where the first of them is written, which is also where the captures
   * are read. Everything a nested function names from around it must therefore be declared above
   * that point, and the diagnostic for a name declared below says so.
   */
  /** Which of `candidates` the group's bodies read — the block's own bindings a nested function
   * needs, whether or not they have been bound yet (`0224`).
   *
   * This is what decides **where** the group is lowered. `captures` normally asks the scope whether
   * a name is an outer one, and a binding written further down the block is not in it yet; asking
   * about the block's *whole* binding set instead is what lets a nested function read something
   * written below it. The environment is then built after the last of them, so every address it
   * holds is of a slot whose declaration has run.
   *
   * A name the group's own functions bind, and a parameter of one, is excluded by `captures`
   * itself, so what comes back is only what the group reaches outward for.
   */
  protected def groupNeeds(group: List[FuncDecl], candidates: Set[String]): Set[String] = {
    val bound = group.map(_.name).toSet

    group.flatMap(f => captures(f.body, bound ++ f.params.map(_.name), candidates)).toSet
  }

  protected def lowerNestedGroup(group: List[FuncDecl]): List[TStmt] = {
    val names = group.map(_.name)

    for f <- group do
      at(f.pos) {
        if f.vis != Visibility.Public then
          err(s"'${f.name}' is declared inside a function body, so nothing outside can name it and " +
            "there is nothing for a visibility modifier to restrict")
        if names.count(_ == f.name) > 1 then err(s"'${f.name}' is already declared in this block")
        if f.tparams.nonEmpty then
          err(s"'${f.name}' is declared inside a function body and cannot be generic — the type " +
            "arguments would have nowhere to come from, since nothing outside the body calls it")
        // A nested function states its own signature (`reference/declarations.md`), so it is held
        // to the same rules as any other — including where a `va_list` may stand and what a `...`
        // must have before it.
        checkSignatureRules(f.name, f.params, f.retType, f.variadic)
      }

    // A sibling is reached through the shared receiver rather than captured, so the names of the
    // group are bound here and are not free in each other's bodies.
    val bound    = names.toSet
    val captured = group.flatMap(f => captures(f.body, bound ++ f.params.map(_.name))).distinct

    refuseRefCaptures(captured)

    // The field holds the **address** of the enclosing variable, not a copy of it — see
    // `Environment`. That is what makes a nested function assigning to one of the block's locals
    // assign to the block's local, which is the thing the form exists for.
    val fields = captured.map(n => (n, Type.Ptr(lookupOpt(n).get._2)))
    val env    = Type.Struct(Closures.envBase(environmentCount), Nil)

    environmentCount += 1
    env.fields = fields
    structInsts(env.base) = env

    val local = declare(Closures.envBase(environmentCount - 1), env)
    val here  = TAddrOf(TLoad(local, env), Type.Ptr(env))
    val self  = TLoad("self", Type.Ptr(env))

    // Every signature is registered before any body is analyzed, which is what lets one call
    // another whichever order they are written in. A nested function states its parameters and its
    // result (`reference/declarations.md`), so there is nothing here to infer and nothing to wait
    // for.
    val lowered = group.map { f =>
      val fname = s"${Type.mangle(env)}.${f.name}"

      // A nested function is a closure by another spelling, so it is the test's on the same terms.
      if inTestBody then testOnlyDecls += fname

      funcInsts(fname) = (
        ("self", Type.Ptr(env)) :: f.params.map(p => (p.name, resolveType(p.typ, tsubst))),
        f.retType.map(resolveReturn(_, tsubst)).getOrElse(Type.Unit),
      )
      (f, fname)
    }

    val inside = lowered.map((f, fname) => f.name -> Nested(fname, self, f.variadic, f.params)).toMap
    val outer  = lowered.map((f, fname) => f.name -> Nested(fname, here, f.variadic, f.params)).toMap

    // The names are bound **before** any body is analyzed, so a body that does not analyze leaves
    // the group callable and the block is told about its one mistake rather than about that one and
    // an undefined name at every call.
    nestedFuncs = nestedFuncs ++ outer

    for (f, fname) <- lowered do
      val (params, result) = funcInsts(fname)

      recover(())(at(f.pos) {
        val (func, _) = analyzeNested(fname, params.tail, Some(result), f.body,
          Some(Environment(env, captured, byReference = true, fixed(captured))), inside, f.variadic)

        closureFuncs += func
      })

    // The environment holds addresses, so nothing in it is counted and nothing is copied: it is a
    // row of pointers into the frame it was built in, which is sound exactly because a nested
    // function cannot leave that frame.
    // Read through the ordinary analysis rather than off the scope, so a name this block itself
    // captured from further out is the field it reaches there, and the address taken is of the one
    // variable rather than of a copy of it.
    val addresses = captured.map { n =>
      val place = analyzeExpr(Ident(n))

      TAddrOf(place, Type.Ptr(place.ty))
    }

    List(TVarDecl(local, env, TStructNew(env, addresses)))
  }

  /** Which of a body's captures were bound by something written once, so that naming one inside the
   * body reaches a `val` and not a variable that happens to hold the same value.
   */
  private def fixed(captured: List[String]): Set[String] =
    captured.filter(n => lookupOpt(n).exists((u, _) => readOnlyLocals(u))).toSet

  /** Refuses a capture of a `ref`, which is the one construct that would carry one out of its block
   * (`reference/memory.md § ref — a name for a place`).
   *
   * A ref is a **declaration and never a type**, and the whole of what that buys is that the compiler
   * still holds the place the name stands for, in the body that wrote it. A capture is exactly the
   * thing that breaks it: the name would be read from an environment, in a body that has no place to
   * walk outward through, and an escaping closure would carry the address past the block whose
   * storage it names — the dangle the binding rule spends its effort preventing at the other end.
   *
   * It is refused rather than made to work by capturing the place instead, because that would be a
   * second, hidden meaning for one word: `ref e = xs[i]` inside the closure would re-walk the path at
   * whatever `i` had become, which is the call-by-name reading the chapter rejects.
   */
  protected def refuseRefCaptures(captured: List[String]): Unit =
    for
      n         <- captured
      (unique, _) <- lookupOpt(n)
      if refPlaces.contains(unique)
    do
      err(s"'$n' is a 'ref', which names storage for as long as its own block and no longer — so it " +
        "cannot be captured, since a closure may outlive that block. Capture the place it names, or " +
        "read the value into a 'val' and capture that")

  /** `inner(a)` — a call to a nested function, which is a call to its lowered name with the shared
   * environment in front of the arguments it was written with.
   */
  protected def callNested(n: Nested, written: String, args: List[Expr]): TExpr = {
    val (params, result) = funcInsts(n.fname)

    // A nested function is a declaration with named parameters like any other, so it takes both a
    // name at the call and a default (`reference/declarations.md § Default parameters and named
    // arguments`). Its default carries no owning key: every call to one is inside the body it was
    // written in, so the terms already in force are its own — and `bindArgs` empties the locals,
    // which is what keeps a default from reading a capture.
    val bound = bindArgs(s"'$written'", None, n.params, args, n.variadic)

    checkArity(s"'$written'", params.length - 1, n.variadic, bound.length)

    // The environment holds the first slot, so a tail begins one past where a free function's does
    // — the same arithmetic a method's receiver makes.
    val (declared, tail) = bound.splitAt(params.length - 1)
    val supplied2        = declared.zip(params.tail).map((a, p) => analyzeExpr(a, Some(p._2)))

    funcsUsed += n.fname
    TCall(n.fname, checkArgs(written, params, declared, Some(n.env :: supplied2)) ::: tail.map(variadicArg(_)), result)
  }

  /** `xs.map(square)` — a declared function where a callable is wanted (`reference/expressions.md §
   * Closures`).
   *
   * **A named function is the capture-free closure**, so it is one: the same struct with no fields,
   * whose `call` is a call to the function. There is no function-pointer type beside the call trait
   * and no wrapper for a program to write, and the degenerate case costs nothing at run time —
   * an empty environment is an empty struct.
   */
  protected def functionAsCallable(written: String, ptypes: List[Type], result: Option[Type], pos: Option[Pos])
      : TExpr = {
    // The parameters are named where no program can name them, so nothing the function's own body
    // reaches is shadowed by one of them.
    val names = ptypes.indices.map(i => s"${Modules.sep}a$i").toList
    val call  = Call(Ident(written).setPos(pos), names.map(n => Ident(n).setPos(pos))).setPos(pos)

    lowerClosure(names, ptypes, result, List(ExprStmt(call).setPos(pos)), pos)
  }

  /** Files the closure's struct as an implementation of the call trait for its arity, so a bound
   * over `Fn(A) -> R` is met by it and a `&Fn(A) -> R` may be built out of it.
   *
   * The block is synthetic and there is no source `impl` behind it, which is exactly right: a
   * closure implements the call trait by being one, and nothing about the block is a promise a
   * program made and could have made differently.
   */
  private def registerCallTrait(struct: Type.Struct, ptypes: List[Type], ret: Type, pos: Option[Pos]): Unit = {
    val trName = traitKey(Type.Fn.base(ptypes.length)).getOrElse(
      at(pos)(err(s"a callable of ${quantity(ptypes.length, "parameter")} has no call trait")))

    val impl = ImplDecl(trName, NamedType(struct.base), Nil)
    val args = ptypes :+ ret

    traitImpls((trName, struct.base)) =
      // A closure's call trait declares no associated type, and could not: the shape is synthesized
      // here rather than written, so there is nothing for an implementation to have chosen.
      List(TraitImpl(impl, args, Type.Bound(trName, args).key, "", Nil, None, currentScope,
                     Map.empty, Map.empty))

    // The member is registered as the ordinary method it is, so calling a closure is a method call
    // and needs no path of its own — its parameters are the signature `funcInsts` already holds, so
    // what is recorded here is the shape a call reads: a method, taking its receiver by address.
    memberDecls((struct.base, "call")) =
      MethodDecl("call", Some(RecvMode.ByPtr), isProperty = false, Nil, Nil, None, Nil)
  }

  /** The call trait a value of this type implements, where it implements one — which is what makes
   * it a thing that can be called.
   *
   * The four shapes are the ones `§6` and `§8` produce: a bounded type parameter (a bare arrow's
   * sugar, monomorphized), a trait object behind either mode (the boxed callable), and a concrete
   * type with an implementation — which is a closure's own struct after monomorphization, and is
   * also any type a program writes an implementation of the call trait for.
   */
  protected def callableOf(t: Type): Option[Type.Bound] = receiverType(t) match
    case a: Type.Abstract => a.bounds.find(b => Type.Fn.parts(b.name, b.args).isDefined)
    case tr: Type.Trait   => Option.when(Type.Fn.parts(tr.name, tr.args).isDefined)(tr.bound)
    case named: Type.Named =>
      (0 to Type.Fn.maxArity).view
        .flatMap(n => traitKey(Type.Fn.base(n)))
        .flatMap(name => implsOf(name, named.base).map(ti => Type.Bound(name, ti.written)))
        .headOption
    case _ => None

  /** `f(args)` where `f` is a value rather than a name the program declared (`reference/types.md §
   * Function types`).
   *
   * It is a call to the call trait's one member, so it is the ordinary method call it looks like —
   * which is what makes the bare-arrow parameter a direct call and the `&Fn` field an indirect one
   * with nothing here choosing between them. The receiver decides, exactly as it does everywhere.
   */
  protected def callCallable(recv: TExpr, args: List[Expr], expected: Option[Type]): TExpr =
    callMethodOn(recv, "call", args, expected)

  /** `b.on_click(7)` — a **field** holding a callable, called through the selection that reads it.
   *
   * A field is only reached this way where no method of that name exists, so a type never loses a
   * method to a field: the two share a spelling and the method wins, which is the order every other
   * lookup already takes. It is what makes `struct Button` with an `on_click: &Fn(int) -> unit` a
   * usable shape rather than one whose field has to be read into a local before it can be called.
   */
  protected def callableField(
      rty: Type,
      name: String,
      recv: TExpr,
      args: List[Expr],
      expected: Option[Type],
  ): Option[TExpr] = rty match
    case s: Type.Struct =>
      s.fieldType(name).filter(t => callableOf(t).isDefined || cfnOf(t).isDefined).map { fty =>
        checkFieldVisible(s.base, name)
        val field = TField(autoDeref(recv), s.slot(s.fieldIndex(name)), fty)

        // A field holding C's function pointer is called through in the same position and by the
        // same spelling; what differs is only that there is no receiver to hand over
        // (`reference/ffi.md § A function's address`).
        if cfnOf(fty).isDefined then callThroughAddress(field, args)
        else callCallable(field, args, expected)
      }
    case _ => None

  /** The signature a type asking for a callable asks for: what it takes, and what it yields where
   * the type says so.
   *
   * The three shapes are the three `§6` names — a bounded type parameter (which is what the bare
   * arrow a parameter wrote became), a trait object (`&Fn(A) -> R`), and the trait itself, which is
   * what a bound's argument list reads as. A result is `None` where the context genuinely does not
   * fix one, which is not the same as fixing it to `unit`.
   */
  protected def callableSignature(t: Type): Option[(List[Type], Option[Type])] = t match
    case Type.Ref(tr: Type.Trait, _) => fnParts(tr)
    case Type.Ptr(tr: Type.Trait)    => fnParts(tr)
    case tr: Type.Trait              => fnParts(tr)
    case a: Type.Abstract            => a.bounds.flatMap(b => fnParts(Type.Trait(b.name, b.args))).headOption
    case _                           => None

  private def fnParts(tr: Type.Trait): Option[(List[Type], Option[Type])] =
    Type.Fn.parts(tr.name, tr.args).map((ps, r) => (ps, Option.unless(r == Type.Unknown)(r)))

  /** The names a body reads from the scope it was written in, in the order it first reads them.
   *
   * This is a walk over what was *written* rather than over what it resolved to, which is what lets
   * it run before the body is analyzed — and it has to, since the body cannot be analyzed until the
   * type holding its captures exists. A name is a capture when the enclosing scope has one and the
   * body did not declare it: a parameter, a local, or a loop variable of the body's own shadows the
   * outer name exactly as it would anywhere else, and a name that resolves to a declaration rather
   * than to a local is reached the way any other function reaches it.
   */
  private def captures(body: List[Stmt], bound: Set[String],
                       outer: String => Boolean = n => lookupOpt(n).isDefined): List[String] = {
    val found = mutable.LinkedHashSet.empty[String]

    def walk(node: Any, bound: Set[String]): Set[String] = node match
      case Ident(n) =>
        if !bound(n) && outer(n) then found += n
        bound
      // A binding is in scope for what comes *after* it, so the shadow starts at the declaration and
      // the initializer is still read outside it — `var n = n` captures the outer `n`.
      case VarDecl(n, _, init, _, _, _, _) => init.foreach(walk(_, bound)); bound + n
      case ValDecl(n, _, v, _, _, _)       => walk(v, bound); bound + n
      case RefDecl(n, p)         => walk(p, bound); bound + n
      case ConstDecl(n, _, v, _) => walk(v, bound); bound + n
      case f: FuncDecl => scoped(f.body, bound ++ f.params.map(_.name)); bound
      // A closure inside this one captures from further out through this one, so what it reads is
      // read here too — which is what makes capture reach through a nesting
      // (`reference/declarations.md`).
      case Lambda(ps, b, _) => scoped(b, bound ++ ps.map(_.name)); bound
      case For(_, n, it, b, e) =>
        walk(it, bound)
        scoped(b, bound + n)
        e.foreach(scoped(_, bound))
        bound
      // A quantifier binds its name over the predicate and nowhere else, so an outer name of the
      // same spelling is not captured by a clause that only shadows it (`reference/verification.md
      // § for all and for some`).
      case Quantifier(_, n, it, p) =>
        walk(it, bound)
        walk(p, bound + n)
        bound
      case MatchArm(ps, guard, b) =>
        val inArm = bound ++ ps.flatMap(patternNames)

        // What the patterns *read*, which until quoted names existed was nothing. Collected by a
        // walk of its own rather than by turning the general one loose on the patterns: that would
        // also descend into a `LitPattern`'s expression and could call a name there a capture,
        // which is a change to every match arm in the language rather than to the new form.
        //
        // Resolved against what was bound *before* the arm, since the quoting says the name is
        // something already declared — so a pattern that both binds `a` and references `` `a` ``
        // reads the outer one.
        for n <- ps.flatMap(patternReads) do
          if !bound(n) && outer(n) then found += n

        guard.foreach(walk(_, inArm))
        scoped(b, inArm)
        bound
      // A statement list threads its bindings along, since each declaration is in scope for the ones
      // after it; anything else is walked for its parts with the same bindings throughout.
      case stmts: List[?] => stmts.foldLeft(bound)((b, s) => walk(s, b))
      case p: Product     => p.productIterator.foreach(walk(_, bound)); bound
      case _              => bound

    // A block is its own scope, so what it declares is gone at its end and nothing after it sees one.
    def scoped(stmts: List[Stmt], bound: Set[String]): Unit = walk(stmts, bound)

    scoped(body, bound)
    found.toList
  }

  /** Every name a pattern **reads** — the backticked ones, which reference something already
   * declared rather than binding it (`09`).
   *
   * It is almost always empty, and that is the point of having it: every other pattern form binds a
   * name or holds a literal, so this is exactly the set the new form added and nothing else changes
   * meaning.
   */
  private def patternReads(p: Pattern): List[String] = p match
    case EqPattern(n)          => List(n)
    case BindPattern(_, inner) => patternReads(inner)
    case VariantPattern(_, ps) => ps.flatMap(patternReads)
    case TuplePattern(ps)      => ps.flatMap(patternReads)
    case StructPattern(_, fs)  => fs.flatMap((_, sub) => patternReads(sub))
    case _                     => Nil

  /** Every name a pattern binds, which shadows inside the arm it introduces. */
  private def patternNames(p: Pattern): List[String] = p match
    case IdentPattern(n)       => List(n)
    case BindPattern(n, inner) => n :: patternNames(inner)
    case VariantPattern(_, ps) => ps.flatMap(patternNames)
    case TuplePattern(ps)      => ps.flatMap(patternNames)
    case StructPattern(_, fs)  => fs.flatMap((_, sub) => patternNames(sub))
    case _                     => Nil
}
