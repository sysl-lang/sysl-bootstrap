package sh.sysl

import scala.collection.mutable

/** What a program can still arrive at once it has started: the `val`s an expression ends up reading,
 * and the functions it ends up calling.
 *
 * One walk answers both, because they are the same question asked of different roots. `reference/modules.md § val — a thing` asks it
 * of a single `val`'s initializer — which storage has to be filled before this one — and `prune` asks
 * it of the whole program at once, so that a declaration nothing can reach costs the output nothing.
 *
 * **Only emission is filtered, never analysis.** A function nobody calls is analyzed and reported
 * exactly as one that is called, which is why this runs as its own pass over a program the analyzer
 * has already finished with, and after the checks that come between. A mistake is a mistake whether
 * or not the program would have run the line; what being unreachable costs a declaration is its place
 * in the output, which is the only thing it was costing the program.
 *
 * Two rules make the walk honest, and are worth stating here rather than at each use.
 *
 * **It over-approximates, never under.** Where a call's target is decided at run time, every function
 * it could land in is taken: a slot of a method table is answered with what every table for that
 * trait put there. The whole program is compiled at once, so that set is known even though the choice
 * is not. An over-approximation costs a function that is never called; an under-approximation costs a
 * call to a function that was never written, which is not a trade.
 *
 * **It descends through the shape of the tree rather than a case per node**, and deliberately: a node
 * added later is followed without anyone remembering to come back here, where a match with a
 * catch-all would silently stop descending into the new one's children. Only the nodes that *name*
 * something get a case, since a name is a `String` like any other and the shape cannot tell one from
 * a variable's or an operator's.
 */
object Reachability {

  /** The same program with everything unreachable dropped.
   *
   * The roots are the places the program can start from: the statements it runs, the `main` it runs
   * after those, the initializers that fill its storage before either, and the method tables a trait
   * object dispatches through — a table is a constant the program can read a function out of, so what
   * it points at is reachable whatever can be proved about the calls themselves.
   *
   * The tables and the types are left alone. A table is reached by erasing a value into a trait
   * object, and a type is emitted for its layout rather than for anything that runs, so neither is
   * what this pass is about; both are their own question.
   *
   * `own` names the modules this compilation is **building** rather than being handed, and what it
   * decides is which exports are roots — `exporting` says why. `None` is what a compilation with no
   * dependencies means: everything here is the program's own.
   */
  def prune(program: TProgram, own: Option[Set[String]] = None): TProgram = {
    val live = walk(program, entryPoints(program, own)).live

    program.copy(
      externs = program.externs.filter(e => live(e.name)),
      funcs = program.funcs.filter(f => live(f.name)),
    )
  }

  /** What one walk of a program answered: everything it can arrive at, and the **suppliers** it had
   * to add roots for to get there.
   */
  private case class Walked(live: Set[String], suppliers: Set[String])

  /** The walk, and the one root kind that cannot be decided before it has run.
   *
   * **An `@export` whose symbol answers an `extern` the program actually calls is a root**
   * (`reference/ffi.md § A module may supply another module's extern`). It is the fifth kind, and it
   * is the only one that is not a property of the function alone: the other four are decided by what
   * a declaration carries, so `entryPoints` can answer them before anything is walked, while this one
   * is decided by what the *rest of the program* turned out to need.
   *
   * **What it is for is a seam.** A capability-free module declares a symbol and defines nothing —
   * `sysl.time`'s clock is the worked example — and whoever links the program defines it: the
   * standard library on a host, a package binding an RTC chip on a board. The consumer imports the
   * *declaring* module and calls `now()`; it never names the supplier, which is the whole point,
   * since naming one is naming the chip. So `contributing`'s question — did the program reach the
   * supplier's module? — answers **no** for precisely the case the seam exists to serve, and would
   * prune the definition the program is about to fail to link without.
   *
   * **The narrowness is what keeps `0111` intact.** The rule this does not weaken is that a
   * dependency's root reaches a consumer only where the consumer reaches its module, and the case
   * that cost was a package carrying a test application whose `@export("main")` fought the
   * consumer's own. Nothing declares `extern "main"`, so no live extern symbol answers to it and the
   * supplier rule never fires there. A supplier is kept exactly when something in the program is
   * already going to ask the linker for that symbol — which is the accounting an archive member gets,
   * arrived at from the other side.
   *
   * **It is a fixpoint because a supplier has a body**, and that body may call an extern of its own
   * that some further module supplies. Both sets only ever grow and both are bounded by the program's
   * own declarations, so it settles; in practice one extra round does it, and the round after that is
   * what proves nothing more was found.
   */
  private def walk(program: TProgram, entries: List[TFunc]): Walked = {
    val bySymbol = program.funcs.filter(_.exported.isDefined).groupBy(_.exported.get)
    val extra    = mutable.LinkedHashSet.empty[TFunc]

    var live = Set.empty[String]
    var more = true

    while more do
      val roots = List(program.main, program.vals, program.vtables, program.entry, entries, extra.toList)

      live = reachedFrom(roots, program.funcs, program.vtables).calls ++
        entries.map(_.name) ++ extra.map(_.name)

      val wanted = program.externs.collect { case e if live(e.name) => e.symbol }.toSet

      more = wanted.toList.flatMap(s => bySymbol.getOrElse(s, Nil)).foldLeft(false)((grew, f) => extra.add(f) || grew)

    Walked(live, extra.map(_.name).toSet)
  }

  /** The functions that are roots because **nothing in the program names them**, whatever the walk
   * starts from.
   *
   * The four below are one idea told four times, and they are gathered here rather than written
   * where a walk begins because there is more than one such place: a program's walk starts at what it
   * runs, and a test build's starts at its tests (`Tests.only`). A list of entry kinds that lived
   * beside one of those would be a list the other did not have — which is exactly what happened, and
   * cost a test build the ability to link a package that had a destructor or an export.
   *
   * **An interrupt handler is a root**, and it is the one kind of function that can never be
   * anything else: no program calls it — `reference/ffi.md § interrupt` refuses that outright — so
   * a walk starting from what the program *runs* cannot reach it. Dropping one would leave the
   * vector table pointing at nothing, which is a fault at the worst available moment. It is entered
   * by the processor, and that is exactly what an entry point is.
   *
   * Its **body** is walked with the others, so whatever a handler calls survives because the handler
   * does. Only its own name has to be added by hand, since nothing names it.
   *
   * **An `@export`ed function is a root for the same reason** (`reference/ffi.md § @export`).
   * Nothing inside the program need ever call it — the whole point is that something outside the
   * program will, and this compilation cannot see that caller any more than it can see the
   * processor. A build with no entry point at all is the case that makes this load bearing: every
   * root above is absent there, so an export that were not one would prune the artifact down to
   * nothing.
   *
   * **A `@section` definition is a root for the third version of it** (`reference/attributes.md §
   * @section("...")`). What finds it is a linker script gathering a named section, which is no more
   * visible to this walk than the processor or the C caller is — a `.ramfunc` copied into RAM by a
   * startup routine, or a boot entry the image is laid out around. Placing a definition somewhere
   * and then dropping it for want of a caller is the one outcome the attribute was written to
   * prevent, and keeping it costs a function nobody calls, which is the trade every other kind here
   * makes.
   *
   * **A destructor is a root for the fourth version of the same reason** (`reference/memory.md § A
   * destructor`). What calls it is the release hook the emitter builds, and that is not a tree this
   * walk can see — it is generated from a payload type at the moment a box of that type is let go
   * of. No reachable body names one, so pruning it would leave the hook calling a symbol nothing
   * defined, and the failure would be at the link, against a name no line of the program contains.
   *
   * **A root the program did not WRITE counts only where the program reaches its module**, and that
   * is the one qualification on any of the four — the same qualification on each of them, since a
   * rule told per kind turns into a rule about which attributes a function carries *together*.
   * `contributing` is the whole of it, and **the standard library is inside it**: it is handed to a
   * compilation exactly as a `--lib` root is, so the first `impl Drop` in `library/` would otherwise
   * have been emitted into every program that links it.
   *
   * **There is a FIFTH kind and it is not here, because it cannot be**: a function supplying a symbol
   * some `extern` the program calls has declared. That one is decided by what the walk found rather
   * than by what the declaration carries, so it lives in `walk`, which is also where the argument for
   * it is — including why it does not reopen what `contributing` closed.
   */
  def entryPoints(program: TProgram, own: Option[Set[String]] = None): List[TFunc] = {
    val contributes = contributing(program, own)
    val drops       = program.destructors.values.toSet

    program.funcs.filter { f =>
      val kind = f.conv.isDefined || f.exported.isDefined || f.section.isDefined || drops(f.name)

      kind && contributes(f.name)
    }
  }

  /** The exports this compilation is answerable for: the ones it will emit, and the ones every pass
   * that reads the emitted program's symbol table should read (`Exports.check`).
   *
   * The same rule `entryPoints` applies, narrowed to the one kind, because the check is about symbols
   * rather than about emission: a symbol two declarations claim is a refusal, and one of them being
   * in a module this program never reaches means there is only one claimant.
   */
  def exports(program: TProgram, own: Option[Set[String]]): List[TFunc] = {
    val contributes = contributing(program, own)
    val supplies    = walk(program, entryPoints(program, own)).suppliers

    program.funcs.filter(f => f.exported.isDefined && (contributes(f.name) || supplies(f.name)))
  }

  /** Whether the module a function is in contributes roots to this compilation at all.
   *
   * **A handed source root is compiled whole rather than by what the program imports**, so every
   * module of every `--lib` root, every fetched package **and the standard library** is in this tree
   * whether or not anything reaches it. For an ordinary declaration that costs nothing — `prune`
   * drops what no body names — but every kind above is precisely a declaration no body names, so an
   * unconditional root put an unimported module's contribution into the consumer's output. What that
   * cost was a **package carrying its own program**: a test application's `@export("main")` reached
   * every consumer, and the two `main`s fought at the link.
   *
   * **The library is one of the handed roots and was the last to be told so.** It travels as its own
   * `Stdlib` rather than in the units, so a caller working `own` out from the presence of *other*
   * libraries left it unqualified — and a library module nothing reaches then put its contribution
   * into every program in existence. Measured with an `impl Drop` on `sysl.fs`'s shared state and a
   * program whose whole body is `print(1)`: the destructor is emitted, and on a freestanding target
   * it brings a `declare` for `fclose` with it, out of a module whose own header requires `os`.
   *
   * **The rule is about provenance and it is one rule for all four kinds**, which is what makes it
   * hold. Told per kind it would have to be told about *pairs* as well, since a root left
   * unconditional keeps whatever else the same function carries: an `@export` that is also
   * `@section`-placed, or that is also a handler, would be emitted for the second reason and land its
   * C symbol in the consumer anyway — measured, and it put two `define @main`s in one module.
   *
   * **The destructor is the kind that could under-prune, and coherence is why it cannot.** Over-
   * pruning the other three costs a symbol nobody asked for; over-pruning a destructor is a *link*
   * error, since the release hook the emitter builds calls a name no line of the program contains.
   * What makes it safe is `reference/traits.md § Where an impl may live`: an `impl Drop for T` may
   * live only in the module declaring `Drop` — the library's — or in one declaring a type named in
   * `T`. So the hook's module is always one that instantiating `T` had to name, and a reachable
   * instantiation always carries an edge to it.
   *
   * **That argument was written for a package and holds word for word for the library**, with one
   * residue worth stating rather than legislating against: the library *may* add to `Drop`'s own
   * module, since it is the module the trait is declared in. An `impl Drop for File` written in
   * `sysl` rather than beside `File` in `sysl.fs` is therefore unconditional again — every program
   * reaches `sysl`. That is the author's choice, the natural spelling is beside the type, and a rule
   * about where in the library an impl may sit would be a worse rule than coherence's.
   *
   * **What the qualification costs is that a consumer wanting a package's handler or placed
   * definition has to name its module**, and `import` is how — that is a reference like any other,
   * and it says in the consumer's own source what it is asking for. A vector table slot and a
   * RAM-resident `.ramfunc` region are the scarcest things on the parts those attributes exist for,
   * so gaining every unimported module's silently is the worse way to be wrong.
   *
   * The referring is asked of `TProgram.moduleDeps` — the graph name resolution built, which records
   * an `import` as readily as a call. That is deliberately coarser than the function-level walk
   * beside it: a module holding nothing but an ISR handler and an exported C entry has no function
   * anything calls, and a rule asking whether the program *called* something there would drop exactly
   * the case these attributes exist for.
   *
   * **The transitive closure and not the direct edges**, because a package reached through another
   * package is reached: `own` is where the walk starts, and it follows the graph out.
   *
   * `own` is `None` where a caller has nothing to say, and the answer is then that every module is
   * the program's own. **No compilation a driver makes takes that any more** — the standard library
   * is always handed, so `Compiler.ownModules` always answers — and it is left meaningful for the
   * in-tree callers that legitimately have no program to name. **A supplied file with no module
   * header is in the root module and is therefore treated as the program's own**, since the root
   * module is the one name two trees can share — the answer that keeps a symbol is the safe one to
   * be wrong with.
   */
  private def contributing(program: TProgram, own: Option[Set[String]]): String => Boolean = {
    val reached = reachedModules(program, own)

    name => reached.forall(_(Modules.moduleOf(name)))
  }

  /** Which modules a root in counts, or `None` where every module's does. */
  private def reachedModules(program: TProgram, own: Option[Set[String]]): Option[Set[String]] =
    own.map { ours =>
      val reached = mutable.HashSet.from(ours + Modules.root)
      val queue   = mutable.Queue.from(reached)

      while queue.nonEmpty do
        for to <- program.moduleDeps.getOrElse(queue.dequeue(), Set.empty) if reached.add(to) do
          queue += to

      reached.toSet
    }

  /** What a set of trees reaches: every `val` read and every function called, following each call
   * into the body it lands in.
   *
   * A plain reachability walk rather than a fixpoint. Recursion among the callees is fine — what is
   * being accumulated is a set, and a function already visited adds nothing a second time.
   */
  def reachedFrom(
      roots: List[Any],
      funcs: List[TFunc],
      vtables: List[TVtable],
      written: Boolean = false,
  ): Refs = {
    val byName = funcs.map(f => f.name -> f).toMap
    val vals   = mutable.HashSet.empty[String]
    val called = mutable.HashSet.empty[String]
    val erased = mutable.HashSet.empty[String]
    val sites  = mutable.HashSet.empty[(String, Int)]
    val queue  = mutable.Queue.empty[String]

    def take(r: Refs): Unit =
      vals ++= r.vals
      erased ++= r.erased
      sites ++= r.dynamicSites
      for c <- r.calls if called.add(c) do queue += c

    def drain(): Unit =
      while queue.nonEmpty do for f <- byName.get(queue.dequeue()) do take(summarize(f, vtables, written))

    roots.foreach(r => take(summarize(r, vtables, written)))
    drain()

    // Under `written` a dynamic site is answered against the tables this walk saw a value erased
    // into, and an erasure can turn up in a body reached only through an earlier answer -- so
    // answering opens sites and sites open answers. Repeat until neither grows.
    //
    // It terminates because both sets only ever gain, and both are bounded by the program: the
    // tables it declares and the (trait, slot) pairs its bodies name. In practice one pass settles
    // it, since a body that erases a value into a table almost always calls through it in the same
    // breath -- the loop is here for the case that does not, which is a sink made in one function
    // and written to in another.
    if written then
      var growing = true

      while growing do
        val before = called.size

        for (traitName, slot) <- sites.toList; t <- vtables if t.name != "" && erased(t.name) && t.traitName == traitName
            s <- t.slots.lift(slot) if called.add(s.target)
        do queue += s.target

        drain()
        growing = called.size != before

    Refs(vals.toSet, called.toSet, erased.toSet, sites.toSet)
  }

  /** What one tree names, without following any of it: the `val`s read out of it and the functions
   * it can call.
   *
   * The last two are empty unless the walk was asked for `written`, and are what that mode needs to
   * reach its answer: the tables the trees themselves erased a value into, and the dynamic calls
   * left unresolved so that they can be answered once the tables are all known.
   */
  case class Refs(
      vals: Set[String],
      calls: Set[String],
      erased: Set[String] = Set.empty,
      dynamicSites: Set[(String, Int)] = Set.empty,
  )

  /** `written` asks a narrower question than the default, and only one caller wants it: **what does
   * this code reach through tables it put a value into itself?**
   *
   * The difference is a dynamic call whose receiver came from somewhere else. Answering it with every
   * table for the trait is right for emission — a table is a constant the program can read a function
   * out of, so a definition it points at has to survive — and wrong for a *capability*, which
   * `reference/modules.md § Capabilities are a module property` states over what a module's own
   * source does. `reference/modules.md § A generic answers for what it wrote, not for what its caller
   * chose` already says this for a type parameter: an allocator-free module may call `s.put(msg)`
   * through a bound whose `impl` allocates, because it never saw the type. A trait object is the same
   * borrowing one level along — a `*Writer` parameter is a choice its caller made — and the two now
   * answer alike.
   *
   * **What a module is still held to is the erasure it wrote.** `TErase` names the table, so a body
   * that makes a growable sink and hands it over as a `*Writer` is answered with that sink's
   * implementation and nothing else. This is sharper than the walk it replaces in both directions:
   * narrower, because the tables somebody else's code built are not the module's business, and no
   * looser, because the case that would otherwise escape — making an allocating sink and reaching it
   * through a trait object — is exactly the case where the erasure is in the module's own tree.
   */
  /** The functions this tree runs **itself** — what it calls, whose address it takes, and the
   * implementations behind every table it erased a value into — without following any of them.
   *
   * It is the `written` reading of one tree, which is the capability question's: a dynamic call
   * through a trait object some other body built is that body's choice, while a table this tree
   * filled is code it handed over to be run.
   */
  def calledBy(root: Any, vtables: List[TVtable]): Set[String] = {
    val refs   = summarize(root, vtables, written = true)
    val filled = vtables.filter(t => refs.erased(t.name)).flatMap(_.slots.map(_.target))

    refs.calls ++ filled
  }

  private def summarize(root: Any, vtables: List[TVtable], written: Boolean): Refs = {
    val vals    = mutable.HashSet.empty[String]
    val calls   = mutable.HashSet.empty[String]
    val erased  = mutable.HashSet.empty[String]
    val pending = mutable.HashSet.empty[(String, Int)]

    // Every table for a trait supplies one function per slot, so a call at a slot can be answered
    // with the functions every implementation of that trait put there. Under `written` the site is
    // recorded instead and answered later, against the tables this walk saw a value erased into --
    // which are not all known until the walk has finished.
    def dynamic(recvTy: Type, slot: Int): Unit =
      val name = recvTy match
        case Type.Ptr(Type.Trait(n, _, _))    => Some(n)
        case Type.Ref(Type.Trait(n, _, _), _) => Some(n)
        case _                             => None

      for n <- name do
        if written then pending += (n -> slot)
        else for t <- vtables if t.traitName == n; s <- t.slots.lift(slot) do calls += s.target

    // A `Type` is where the descent stops — it holds no expression, and a recursive one would
    // otherwise be walked forever. The one function name that lives inside a type rather than beside
    // it is a constrained subtype's `where` predicate, which is why the node checking against one
    // reads it out here instead of leaving it to the descent.
    def scan(x: Any): Unit = x match
      case _: Type            => ()
      case g: TGlobal         => vals += g.symbol
      case d: TDispatch       => calls += d.name
      case c: TCall           => calls += c.name; c.args.foreach(scan)
      // Taking a function's address is a use of it exactly as calling it is, and the only one whose
      // caller is not in this program: what happens to the address afterwards is C's business, so
      // there is nothing else that could keep the definition from being dropped.
      case a: TFuncAddr       => calls += a.name
      case p: TCallPtr        => scan(p.callee); p.args.foreach(scan)
      // A `?` that widens the callee's error carries the conversion as a **name** rather than as a
      // node, for the reason `TRecheck`'s predicate does: what it converts is the failure payload,
      // which exists only inside the branch the emitter builds. So the shape cannot tell it from any
      // other string and it is read out here — without which the call is emitted and the definition
      // is pruned, which clang reports as a use of an undefined value.
      case t: TTry            => t.convert.foreach(calls += _); scan(t.operand)
      case s: TStructInvCheck => calls += s.invFn; scan(s.value)
      case r: TRecheck        => calls += r.invFn; scan(r.after); scan(r.recv)
      // A multi-assignment's arm carries its re-check as data rather than as a node, so the
      // predicate's name is a `String` the shape cannot tell from any other and has to be read out
      // here — exactly as `TRecheck`'s is, for the same reason.
      case w: TWrite =>
        w.check.foreach((recv, _, invFn) => { calls += invFn; scan(recv) })
        w.constraint.flatMap(_.predFn).foreach(calls += _)
        scan(w.place); scan(w.value); w.dispatch.foreach(scan)
      case c: TConstrainedCheck =>
        c.target.predFn.foreach(calls += _)
        scan(c.value)
      // A compound assignment and an increment carry their constraint as a `Type`, which the case
      // above skips, so the predicate they call is named here. **No program observes this today**,
      // and the reason is worth writing down rather than discovering twice: a value can only come to
      // have a `where`-carrying subtype through a site that checks it, so whatever produced the first
      // one has already named the predicate — measured by removing these two lines, which leaves the
      // suite green. They are here because the alternative to naming a function held as data is a
      // call to a definition that was pruned, and that failure is a link error rather than a test.
      case u: TUpdate =>
        u.check.flatMap(_.predFn).foreach(calls += _)
        scan(u.place); scan(u.value); u.dispatch.foreach(scan)
      case i: TIncDec =>
        i.check.flatMap(_.predFn).foreach(calls += _)
        scan(i.place)
      // The entry point names its `main` and the conversion that makes its arguments, neither of
      // which any tree calls: what calls them is the wrapper codegen lays down around them.
      case e: TEntry =>
        calls += e.func
        e.argsFn.foreach(calls += _)
        e.resultFn.foreach(calls += _)
      // Erasing a value into a trait object is where a body says which implementation it is putting
      // behind the trait. The name is the table's, which is what a dynamic site is answered against
      // under `written`; the default walk has no use for it and pays a set insertion.
      case e: TErase  => erased += e.vtable; scan(e.operand)
      case s: TVSlot  => calls += s.target
      case r: TRender =>
        r.slot match
          case Some(slot) => dynamic(r.value.ty, slot)
          case None       => calls += r.method
        scan(r.value); scan(r.spec)
      case v: TVCall =>
        dynamic(v.receiver.ty, v.slot)
        scan(v.receiver); v.args.foreach(scan)
      case xs: Iterable[?] => xs.foreach(scan)
      case p: Product      => p.productIterator.foreach(scan)
      case _               => ()

    scan(root)
    Refs(vals.toSet, calls.toSet, erased.toSet, pending.toSet)
  }
}
