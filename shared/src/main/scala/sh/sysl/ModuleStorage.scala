package sh.sysl

/** The storage a module lays down — its `val`s, its `var`s, and the `extern` variables it names.
 *
 * All three are read in a state belonging to no function, and that is the point rather than an
 * implementation detail: an initializer here is a *module member's* expression, so the locals of
 * whichever function first mentioned the name must not be in scope while it is read.
 *
 * What separates them is which question is asked of what. A `val`'s release rule is asked of the
 * **value**, because a `val` is forever what it was given and a constant tree was never built; a
 * `var`'s is asked of the **type**, because it may be given something else tomorrow; and an
 * `extern`'s is asked of neither, because the storage is somebody else's. `isStatic` is the test
 * all of that turns on, and `reference/modules.md § val — a thing`'s initialization order is the other thing it decides.
 */
trait ModuleStorage extends ModuleFiles {
  // --- module-level `val`s --------------------------------------------------------------

  /** Analyzes one module-level `val` to the storage it lays down.
   *
   * The initializer is read at the declared type, so an element written `0x428a2f98` in a `[64]u32`
   * needs no suffix — the same courtesy a `const` gets from writing its type, for the same reason.
   *
   * What the initializer *is* decides how the storage gets filled: a constant tree is written into
   * the object file, and anything else becomes code that runs once before the program's own
   * statements. It also decides one of the two things checked here, since whether a counted type
   * may be held at all is a question about the value rather than about the type.
   */
  protected def analyzeVal(key: String): TVal = inDecl(key)(at(valDecls(key).pos) {
    val decl = valDecls(key)
    val ty   = globalType(key)

    // Storage a test file declared is scaffolding, and so is anything its initializer lowers to —
    // the same rule a body gets, asked here because an initializer is analyzed outside every body.
    inTestBody = testOnlyDecls(key)

    val init = analyzeExpr(decl.value, Some(ty))

    if disagree(init.ty, ty) then
      err(s"cannot initialize '${qn(key)}': declared ${show(ty)} but the value is ${show(init.ty)}")

    checkNoMaterialized(init)

    val static = isStatic(init)

    checkVal(ty, key)
    TVal(key, ty, Some(init), !static, align = boundaryOf(key, decl.align), section = decl.section)
  })

  /** Refuses `&value` in a module initializer, which is the one place the form has nowhere to put
   * what it makes.
   *
   * `&Rect(2)` writes the value into a hidden local of the scope it stands in — and an initializer
   * here belongs to no scope that outlives it: what runs is a prologue, so the slot would be that
   * prologue's frame and the pointer would be stale by the program's first statement. Storage that
   * lasts the run is what module storage *is*, so the fix is to give the value a name of its own,
   * which is a second declaration rather than something the compiler can invent.
   *
   * A closure written in an initializer is lowered to a function of its own before this walk sees
   * anything, so a `&value` inside one is out of reach here and is right where it stands — the slot
   * is that function's frame, exactly as in any other body.
   */
  private def checkNoMaterialized(t: TExpr): Unit = t match
    case a: TTempAddr =>
      at(a.pos)(err("'&' in front of a value writes it into a hidden local of the scope the '&' " +
        "stands in, and module storage has no such scope — the slot would belong to the " +
        "initializer's own frame, which is gone by the program's first statement. Give the value a " +
        "name that lasts the run: declare it beside this one, and take the address of that"))
    case _ => TreeWalk.children(t).foreach(checkNoMaterialized)

  /** `@align(n)` above module storage, folded to the boundary it named — the same fold a struct's
   * and a local's go through, reported against the name as a reader wrote it rather than against the
   * key the module qualified it into.
   */
  private def boundaryOf(key: String, align: Option[Expr]): Option[Int] =
    align.flatMap(a => recover(Option.empty[Int])(alignBound(qn(key), a)))

  /** One module `var`: module storage the program may write (`reference/modules.md § val — a thing`), written `static var` in the
   * file the program starts in and plain `var` in any other.
   *
   * Three things separate it from the `val` above, and each is what the word `var` already means.
   *
   * **Its initializer may be absent**, which a `val`'s may not: a variable with no value is still a
   * complete declaration of storage, and the type's zero is what it starts at. That is the cheapest
   * form and the one an arena wants — `zeroinitializer` and no store at all.
   *
   * **It may hold a counted value, and the last one it holds is never released.** Every assignment
   * *during* the run has a perfectly good line to write a release on — its own store, which
   * `PlaceEmitter.storeInto` already emits — so the only release with nowhere to go is the one at
   * exit, and never taking it is what a static *is*. A destructor takes the same ruling, and for
   * the same reason (`reference/memory.md § A destructor`).
   *
   * **What is refused instead is a type with no zero and no initializer**, which is a narrower rule
   * about a different thing, and is `hasZero` — the same question a **local** with no initializer is
   * already held to. A `&T` has no zero because there is no such thing as a reference to nothing, so
   * zeroed storage of one is not a reference and the first assignment through it would release
   * whatever address zero is. A `string` and a slice zero to the empty one, whose owner word is null.
   *
   * **It is `writable`**, which is what `TGlobal` carries to every read of the name so that an
   * assignment through it is allowed and a `@pure` function reading it is not
   * (`reference/verification.md § @pure`).
   *
   * **`@thread_local` adds one rule and takes nothing away**, and `checkPerThread` below is it.
   */
  protected def analyzeStaticVar(key: String): TVal = inDecl(key)(at(staticVarDecls(key).pos) {
    val decl = staticVarDecls(key)
    val ty   = globalType(key)

    inTestBody = testOnlyDecls(key)

    val init = decl.init.map { e =>
      val t = analyzeExpr(e, Some(ty))

      if disagree(t.ty, ty) then
        err(s"cannot initialize '${qn(key)}': declared ${show(ty)} but the value is ${show(t.ty)}")

      checkNoMaterialized(t)
      t
    }

    if Type.zeroSized(ty) then
      err(s"'${qn(key)}' cannot be module storage: ${show(ty)} has no representation, so there is " +
        "nothing for the storage to be")
    else if init.isEmpty && !hasZero(ty) then
      err(s"'${qn(key)}' needs a value: storage with no initializer starts at its type's zero, and " +
        s"${show(ty)} has none — the same rule a local with no initializer is held to. A 'string' " +
        "and a slice both start empty and need no value written; a reference and an enum need one")

    if decl.threadLocal then checkPerThread(key, init)

    TVal(key, ty, init, computed = init.exists(!isStatic(_)), writable = true,
      align = boundaryOf(key, decl.align), section = decl.section,
      threadLocal = decl.threadLocal)
  })

  /** The two things `@thread_local` asks that a plain module `var` does not
   * (`reference/attributes.md § @thread_local`).
   *
   * **The initializer has to be a constant**, and that is C's rule for the same reason rather than
   * a borrowing of it: every thread's copy is made from one image the object file carries, and
   * there is no per-thread prologue for anything else to run in. A program's `main` runs once on
   * one thread; a thread started later never passes through it, so an initializer that was code
   * would fill exactly one copy and leave every other thread reading zeroes. LLVM has no lazy form
   * to fall back on either — a `thread_local global` takes a constant initializer and nothing else.
   *
   * **And the target has to have thread-local storage at all.** `Target.hasThreadLocalStorage` is
   * the same question the ARC reaper's slot asks, and the reason it is asked rather than left to the
   * back end is written out there: every target LLVM knows *accepts* the keyword, and a freestanding
   * one silently gets the local-exec model, whose offset is read from a register nothing on a bare
   * machine has written. So the alternative to this refusal is not a slower program, it is one that
   * reads a wild address.
   */
  private def checkPerThread(key: String, init: Option[TExpr]): Unit =
    if !target.hasThreadLocalStorage then
      err(s"'${qn(key)}' cannot be '@thread_local' on '${target.name}': there is no loader and no " +
        "libc on this target, so nothing lays a thread's storage down and nothing writes the thread " +
        "pointer the offset would be read from. Declare it as a plain module 'var' and let the " +
        "port's scheduler answer for which task is running, which is what the ownership runtime's " +
        "reaper slot does")
    else if init.exists(!isStatic(_)) then
      err(s"'${qn(key)}' is '@thread_local', so its value has to be one the compiler can write " +
        "down: every thread gets a copy of the initial value, and the copies are made from a single " +
        "image in the object file rather than by code that runs per thread. A literal, 'null', a " +
        "'const' and the arrays and structs built from them are all values; a call is not. Leave the " +
        "value off to start every thread at the type's zero, and do the work in each thread instead")

  /** Holds a module-level `val` to something a name can stand for (`reference/modules.md § val — a thing`).
   *
   * **A counted value is admissible, and is never released.** The rule this replaced refused one
   * unless its initializer was a constant tree, on the ground that a module `val` exists for the
   * whole run and so has no line to write a release on. That is true and is not a reason: the
   * release it names is the one at exit, which is the release a static is *defined* by not taking.
   * `val greeting: string = str(n)` and `val log: &File = open(path)` are both storage the program
   * holds until it ends, which is what the declaration said.
   *
   * What the value being a constant tree still decides is where the storage is *filled* — in the
   * object file, or by the prologue — and `isStatic` remains the test for that. It is no longer also
   * the test for whether a count may be held.
   *
   * A raw pointer and the address of a function own nothing at the far end, so they were never part
   * of this question either way.
   *
   * Read-only *at every depth* is deliberately **not** what this checks, though it once was. That
   * promise is about the storage the declaration lays down, and it is kept where it is made: `k[0]
   * = …` and `&k[0]` are both refused. It was never a promise about what a value *inside* the
   * storage addresses — a `*T` reached through one is the raw tier, where the language guarantees
   * nothing, and slicing a `val` and writing `&v[0]` already produces one on purpose
   * (`reference/arrays.md § []const T — a view that may not be written`). Refusing a `val` at
   * pointer type declined one route to what another route grants.
   */
  private def checkVal(ty: Type, key: String): Unit =
    // The whole difference between a `val` and a `const` is that a `val` has an address (`reference/modules.md § val — a thing`),
    // and a value with no representation has nothing to put one on. The initializer would still run,
    // which is the only thing such a declaration could have been for — and a statement says that
    // without pretending there is storage.
    if Type.zeroSized(ty) then
      err(s"'${qn(key)}' cannot be a 'val': a ${show(ty)} value occupies nothing, so there is no " +
        "storage for the name to stand for — write the call as a statement instead")

  // --- `extern` variables ----------------------------------------------------------------

  /** Holds one `extern` variable's declared type to being something a symbol could stand for.
   *
   * There is one rule and it is the `val`'s first one, for the same reason: a symbol is an address,
   * and a value with no representation has nothing to put one at. The `val`'s *second* rule — that
   * it counts nothing — is deliberately not applied here. It exists to keep a `val` from owning a
   * count it can never release, and an `extern` variable owns nothing: the storage is the other
   * side's, and so is whatever releasing it would mean.
   */
  protected def checkExternVar(key: String): Unit =
    val ty = externVarType(key)

    if Type.zeroSized(ty) then
      err(s"'${qn(key)}' cannot be an 'extern' variable: a ${show(ty)} value occupies nothing, so " +
        "there is no storage for the linker to resolve the name to")

  // `hasZero` is `GenericInstantiation`'s, and asking it here is the point rather than a saving: it
  // is what a **local** declared with no initializer is already held to, so a module `var` and a
  // local `var` now answer the same question about the same types. A second predicate would have
  // been a second answer to drift from the first.

  /** Whether an initializer is a value the object file can carry as it stands: numbers, string
   * literals, and the arrays and structs built from them. Everything else is code, which is what
   * `reference/modules.md § val — a thing`'s initialization order is about — this is the test that decides which of the two a
   * declaration is. It is also what `checkVal` asks to decide whether a counted type may be held,
   * since a value that was never built owes no release.
   */
  private def isStatic(t: TExpr): Boolean = t match
    case _: TIntLit | _: TFloatLit | _: TBoolLit => true

    // A literal's bytes are already in the object file and its owner word is null, so the three
    // words a `string` is are complete before anything runs (`04`). Nothing is allocated to make
    // one, which is why a module with no allocator may hold a table of them.
    case _: TStrLit => true

    case TArrayLit(elems, _)  => elems.forall(isStatic)
    case TArrayFill(value, _) => isStatic(value)

    // Fields laid side by side are a constant tree exactly when the fields are — the same rule the
    // array above follows, and the reason a table of `{name, code}` pairs lands in read-only data
    // rather than being filled in by stores. A struct whose `invariant` has to run is wrapped in a
    // `TStructInvCheck` and never reaches this, which is `reference/modules.md § val — a thing`'s rule that a value having to be
    // checked is code however it looks.
    case TStructNew(_, args) => args.forall(isStatic)

    // An address the datasheet gives as a number is a constant like any other, so `ptr_cast` over one
    // stays in this category rather than becoming code. It matters where it is used: a register block
    // reached before anything has run is exactly what a freestanding program wants, and an ordered
    // initializer would be a store to make before the storage could be read.
    case _: TNullLit                                      => true
    case TCast(_: TIntLit, (_: Type.Ptr) | (_: Type.CFn)) => true

    // A variant that carries nothing is a tag and a region nothing reads, so it is laid into the
    // object file exactly as a struct of constants is. `None` is what this is for and is why it
    // matters: a module-level slot that starts out unset — a sink nobody has installed, a handle
    // nobody has opened — is `Option[T] = None`, and without this it was an *initializer*, which is
    // what shuts a module out of a freestanding `@export` (`reference/modules.md § val — a thing`).
    //
    // **A variant that carries something is not admitted**, even where its arguments are constants:
    // the payload region is a union written as an array, so filling one means knowing which bytes a
    // variant's fields land on, and that is `CallEmitter`'s store-through-a-pointer rather than
    // anything this file could spell.
    case TEnumNew(_, variant, _) if !variant.carries => true

    case _ => false
}
