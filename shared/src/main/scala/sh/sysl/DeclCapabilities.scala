package sh.sysl

/** What a declaration that wrote `@needs(...)` costs whoever reaches it
 * (`reference/modules.md § A declaration may name what reaching it needs`).
 *
 * **A module is a coarse unit for this question, and that is the whole reason this pass exists.**
 * `@requires(heap)` written on `sysl` would say something false about most of it, so a library that
 * wanted the granularity had to split modules — letting an annotation decide the library's shape.
 * `@needs` is the finer form: it is written on the declaration, and the cost is charged to whoever
 * calls it rather than to whoever holds it.
 *
 * **The declaration it was built for is `extern`.** Every other declaration has a body the compiler
 * reads, so `NoAlloc` finds an allocation by looking; an `extern` is a name and a signature, and a
 * module that gave up an environment capability could reach `open()` straight through one. Nothing
 * but the declaration itself can close that, which is why the annotation is additive rather than a
 * narrowing — a program that writes none is refused exactly what it was refused before.
 *
 * **Two things this deliberately does not do.** It does not make a module's own clause finer: a
 * `@requires(os)` file header still says the module cannot be built without an OS, checked once
 * against the target, and `@needs` adds to that floor rather than replacing it. And it does not
 * infer: a declaration that needs a capability and does not say so is not found here, because the
 * only declaration that cannot be read is exactly the one with nothing to read.
 *
 * The reachability walk and the descent to the smallest guilty sub-tree are `Reaches`, shared with
 * `NoAlloc` — the two checks differ in their seed set and their sentence and in nothing else.
 */
trait DeclCapabilities extends NoAlloc {

  /** Refuses a `@needs(...)` naming something that is not a capability, and reports every reference
   * from a module that does not have what the declaration it reaches requires.
   *
   * It runs beside `checkNoAlloc`, over the same finished tree and for the same reason: what reaches
   * what is a property of the typed program rather than of any one place in the analyzer.
   */
  protected def checkDeclCapabilities(
      funcs: List[TFunc],
      vals: List[TVal],
      vtables: List[TVtable],
      main: List[TStmt],
      mainModule: String,
      testFuncs: Set[String],
  ): Unit = {
    checkNeedsNames()

    val needed = declaredNeeds

    // Everything a test build keeps and every other build drops, built exactly as `checkNoAlloc`
    // builds it and for the same reason: a `@tests` file may take back what the module gave up,
    // because the module's clause is a promise about what *ships* and the file does not ship
    // (`reference/modules.md § A @tests file states its own capabilities`). Asking the module's
    // clause here would refuse a test that took `os` back in order to reach a real filesystem —
    // which is the one thing a `@tests` clause is for.
    // Lazily, for the reason the guard below is written: `testOnlyDecls` is every declaration a test
    // build keeps, so building this set is work a compilation with no `@needs` anywhere should not
    // be charged for.
    lazy val scaffolding = testOnlyDecls.toSet ++ testFuncs

    // Almost every compilation writes no `@needs` at all, and the walk below reads every body — so
    // one with nothing to ask should pay nothing for it.
    if needed.nonEmpty then
      // One walk per capability rather than one per declaration: what a reader has to change is the
      // reference, and a reference refused for two capabilities at once is one line with one fix.
      for cap <- Capability.core.filter(c => needed.values.exists(_.contains(c))) do
        val seeds = new Reaches(funcs, vtables, needed.collect { case (k, cs) if cs(cap) => k }.toSet)

        def check(x: Any, module: String, isScaffolding: Boolean): Unit =
          if lacks(module, cap, isScaffolding) then
            seeds.blame(x) { (pos, who) =>
              recover(())(at(pos)(err(s"this reaches '${Modules.show(who)}', which needs '$cap', " +
                s"and ${why(module, cap, isScaffolding)}")))
            }

        // A generic is answered for by the body it was written as, exactly as `NoAlloc` answers it:
        // an instantiation belongs to whoever chose the type, in a module of their own, and is
        // reported at *their* call.
        // A body whose own declaration needs the capability is left alone for it: what that body
        // reaches is charged to whoever reaches the declaration, and they are checked at their call.
        // Asked of its module instead, a program's own `@needs(os)` function on a machine with no
        // OS would be refused for doing exactly what its annotation says it does.
        for f <- funcs if !genericInsts(f.name) && !bodyCovers.get(f.name).exists(_(cap)) do
          check(f.body, Modules.moduleOf(f.name), scaffolding(f.name))
        for v <- vals; init <- v.init do
          check(init, Modules.moduleOf(v.symbol), scaffolding(v.symbol))
        // The entry file's statements are never scaffolding: a `@tests` file has no body to run,
        // and a program's own top-level code is what every build keeps.
        check(main, mainModule, isScaffolding = false)
  }

  /** Every declaration that wrote `@needs(...)`, as the capabilities it needs with their
   * implications folded in — a declaration needing `posix` needs `os`, whether or not it said so.
   *
   * Both tables are read because an `extern` is in both: `Hoisting` files a `FuncDecl` shim for one
   * so that a call resolves like any other, and the `ExternDecl` beside it is what codegen reads.
   * Reading the shim alone would be enough today and would break silently the first time the two
   * stop being written together.
   */
  private def declaredNeeds: Map[String, Set[String]] = {
    val pairs =
      funcDecls.toList.map(kv => kv._1 -> kv._2.needs) ++
        externDecls.toList.map(kv => kv._1 -> kv._2.needs)

    pairs
      .filter(_._2.nonEmpty)
      .groupMapReduce(_._1)(_._2.filter(Capability.implies.contains).flatMap(Capability.closure).toSet)(_ ++ _)
      .filter(_._2.nonEmpty)
  }

  /** Refuses a `@needs` naming nothing, said the way `Capabilities.checkClause` says it of a file
   * header — one vocabulary, one message, whichever position the mistake was written in.
   *
   * `alloc` is the one worth its own sentence: it is what a module *does*, and a `@needs` names the
   * facility, so somebody who wrote `@needs(alloc)` beside the `@no_alloc` they had just read meant
   * `@needs(heap)`.
   */
  private def checkNeedsNames(): Unit =
    for
      decl <- funcDecls.values.toList
      cap  <- decl.needs if !Capability.implies.contains(cap)
    do
      recover(())(at(decl.pos) {
        if Capability.narrowedBy.contains(cap) then
          err(s"'$cap' is what a module does, not something a machine has — a '@needs' names the " +
            s"facility, so this is '@needs(${Capability.narrowedBy(cap)})'")
        else
          err(s"no capability is called '$cap' — the set is " +
            Capability.core.map(n => s"'$n'").mkString(", "))
      })

  /** Whether `module` is without `cap` — because it gave the capability up, or because the target it
   * is being built for never had it.
   *
   * The two halves are one question, exactly as they are in `NoAlloc` and `GatedModules`: a module's
   * effective set is the target's intersected with its own narrowing, so a capability is out of
   * reach whichever of the two removed it. The library is exempt from the target's half for the
   * reason it is exempt there — its files are compiled into every program, so applying it would
   * report a mistake in source this compilation's author did not write.
   */
  private def lacks(module: String, cap: String, scaffolding: Boolean): Boolean =
    if cap == Capability.Heap then
      if scaffolding then noAllocTests(module) else noAlloc(module)
    else
      narrowing(module, scaffolding).contains(cap) ||
        (!targetProvides(cap) && ownModule(module) && !std.carries(module))

  /** Which clause governs a declaration: the module's, or its tests'.
   *
   * **Only the module's half moves. The target's does not**, which is why the caller intersects
   * this with `targetProvides` rather than this answering both — the asymmetry is `noAllocTests`'s
   * and is the honest part of it. A `@tests` file may take back what the module gave up, because
   * the module's clause is a promise about what ships. It cannot take back what the machine never
   * had, so a test reaching `@needs(os)` is still refused on a target with no OS whatever its file
   * says.
   */
  private def narrowing(module: String, scaffolding: Boolean): Map[String, Option[Pos]] =
    if scaffolding then testNarrows(module) else moduleNarrows.getOrElse(module, Map.empty)

  /** Which of the two put the capability out of reach, said the way the diagnostic needs it. A
   * reader told their module "declared 'no os'" when no file of it says any such thing would go
   * looking for a clause that is not there.
   *
   * **And a test refused under its own `@tests` file's narrowing has a third answer**, told apart
   * the way `NoAlloc.because` tells it apart: a `@tests` file writing nothing inherits its module's
   * entry, pos and all, so an entry carrying a *different* position is one that file wrote. Naming
   * the module there sends a reader to delete a line that is not in it.
   */
  private def why(module: String, cap: String, scaffolding: Boolean): String =
    val ours   = testNarrows(module).get(cap)
    val theirs = moduleNarrows.get(module).flatMap(_.get(cap))
    val word   = Capability.narrowWord(cap)

    // Not `<module>'s '@tests' file`, which renders as `'sys''s` for every module that has a name —
    // `here` quotes what it answers.
    if scaffolding && ours.isDefined && ours != theirs then
      s"the '@tests' file of ${here(module)} declared '@no_$word'"
    else if (if scaffolding then ours else theirs).isDefined then
      s"${here(module)} declared '@no_$word'"
    else s"'${target.name}' does not provide it — a target's capabilities are what " +
      s"'${PackageConfig.FileName}' declares, so either this reference cannot be made on this " +
      "machine or the config is understating it"

  /** How a module refers to itself in a diagnostic. The root module has no name to print, and a
   * program's own files are in it, so the common case reads as a sentence.
   */
  private def here(module: String): String =
    if module.isEmpty then "this module" else s"'$module'"
}
