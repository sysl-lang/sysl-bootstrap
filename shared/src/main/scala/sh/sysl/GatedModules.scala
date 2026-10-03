package sh.sysl

import scala.collection.mutable

/** What a module that gave up an environment capability may not reach (`reference/modules.md §
 * Capabilities are a module property`, `reference/modules.md § Capabilities are a module
 * property`).
 *
 * **The question is asked of the module graph, and that is what makes it a different pass from
 * `NoAlloc`.** `alloc` changes what the *language* allows, so it is checked at each construction
 * that makes heap storage and at each call that arrives at one — it has to be, because the standard
 * module is one module and half of it allocates, so a rule stated over modules would refuse every
 * `no alloc` module that printed anything. `os` and `posix` gate **which modules exist**, which is a
 * statement about a module and not about any declaration in it: a program either may name `sysl.fs`
 * or may not, and every declaration in that module is equally out of reach.
 *
 * So the edge is the unit here, and the diagnostic lands at the reference that made it — the place a
 * reader has to change — rather than at the clause, which is where it would land if the check were
 * about the module's own text.
 *
 * **A requirement is transitive.** A module that gave up `os` may not reach one that requires it
 * *through* a third, since what the third offers is only reachable because the gated module is:
 * what `reference/modules.md § Capabilities are a module property` asks for is that "the whole
 * transitive graph must fit within the target's capabilities", and this is that rule for the half
 * of it a module states about itself.
 */
trait GatedModules extends AnalyzerBase {

  /** Reports every reference into a module that needs an environment capability the reaching module
   * does not have — **either because it gave the capability up, or because the target never had it**.
   *
   * The two halves are one check because they are one rule: `reference/modules.md § The target's
   * half needs no clause at all` says a module's effective set is `target ∩ narrowing`, so a
   * capability is out of reach whichever of the two removed it, and the edge that reached it is the
   * line a reader has to change either way. Only the sentence differs, since only one of them names
   * something the reader wrote.
   *
   * It runs after the module graph is settled and held to being acyclic, for both of the reasons
   * that pass does: an edge is made by a reference and a reference may be anywhere a body is, and
   * the transitive walk below terminates because the graph has no cycles.
   */
  protected def checkGatedModules(): Unit = {
    val narrowed = moduleNarrows.view
      .mapValues(_.keySet & Capability.environment)
      .filter(_._2.nonEmpty)
      .toMap

    // The ceiling half of the two-level rule, which is the machine's rather than any module's: what
    // the target does not provide is out of reach for every module of the program, with no clause
    // written anywhere. It is the same treatment `NoAlloc` gives a target with no heap, and for the
    // same reason — `reference/modules.md § Capabilities are a module property` says the whole
    // transitive graph must fit within the target's set, and a module that inherits the target's
    // capabilities by default inherits their absence too.
    //
    // **Reported here at the reference rather than at the required module's own clause**, which is
    // the whole of what makes it answerable. That clause is in a file the program's author did not
    // write: the standard module's `sysl.fs`, or a package's one POSIX module. Refusing there makes a
    // library unusable on a machine because of a module the program never names; refusing here
    // refuses exactly the programs that reach one.
    //
    // The library's own modules are left out for the reason `NoAlloc` leaves them out: they are
    // compiled into every program, so an edge inside the library would report a mistake in source
    // nobody in this compilation can change. What is being asked is whether the *program* reaches a
    // gated module, and that edge starts in a module of the program's.
    val absent = Capability.environment.filterNot(targetProvides)

    // Nothing narrowed anything and the target has everything, which is almost every compilation: the
    // walk below reads every edge, and one with no question to ask should pay nothing at all for it.
    if narrowed.nonEmpty || absent.nonEmpty then
      diesMemo.clear()
      instanceIndex = null

      val needed = requirements()

      for
        ((from, to), made) <- edgeUses.toList
        uses = effective(made)
        given_up = givenUp(from) if given_up.nonEmpty
        // What a reference written inside a `@needs(...)` declaration reaches is out of reach of its
        // callers for what that declaration names, and `DeclCapabilities` refuses them at the call —
        // so for those capabilities the reference itself is not this check's to refuse. The first
        // reference that IS refused is the one reported, so the caret lands on a line to change.
        //
        // A value that can die is reported ahead of the rest: the destructor is the code it costs,
        // and the place it is held is the line to change, where the import that let it be named is
        // only what made that line possible.
        refused = uses.filter((u, _) => (given_up & (needed.getOrElse(to, Set.empty) -- u.covers)).nonEmpty)
        (use, pos) <- refused.find(_._1.named.isDefined).orElse(refused.headOption)
        // The least of them by name where a reference is refused for more than one reason, so the
        // message does not vary between runs with the iteration order of a set.
        cap <- (given_up & (needed.getOrElse(to, Set.empty) -- use.covers)).toList.sorted.headOption
      do
        // Moved outright rather than through `at`, which would leave the cursor wherever the
        // finished walk left it when an edge carries no position of its own.
        currentPos = pos

        // A clause the reader wrote is the better half of the answer where there is one, so it is
        // preferred over the target's: told both, somebody would go and change the config.
        val why =
          if narrowed.get(from).exists(_.contains(cap)) then
            s"${here(from)} declared 'no $cap' — an environment capability gates which modules " +
              "exist, so a module that gave one up may not reach one that needs it"
          else
            s"'${target.name}' does not provide it — a target's capabilities are what " +
              s"'${PackageConfig.FileName}' declares, so either this reference cannot be made on " +
              "this machine or the config is understating it"

        val what = use.named.fold("this")(k => s"a '${Modules.show(k)}' can die here, and its destructor")

        recover(())(err(s"$what reaches '$to', which requires '$cap', and $why"))
  }

  /** Charges every body for the code it **runs** in another module — the methods it calls, the
   * functions whose address it takes, and the implementations behind each table it erased a value
   * into (`reference/modules.md § A type costs what it runs`).
   *
   * **A free function is charged where its name is resolved, and a method is not**: `e.code()` names
   * no module at all, the receiver's type does, and naming a type charges nothing. So what a body
   * runs is read here, off the typed tree, where every call has been settled to the function it
   * lands in. The use is recorded beside the ones resolution made and never in the module graph,
   * since a method a body calls is not a dependency the graph is held acyclic over.
   *
   * **A generic instantiation is left out**, for the reason `DeclCapabilities` leaves it out: its type
   * arguments were chosen by whoever instantiated it, in a module of their own, and charging the
   * generic's module for them would refuse every program that used it.
   */
  protected def chargeCalls(
      funcs: List[TFunc],
      vals: List[TVal],
      vtables: List[TVtable],
      main: List[TStmt],
      mainModule: String,
      scaffolding: String => Boolean,
  ): Unit =
    if gateAsked then
      def charge(tree: Any, from: String, covers: Set[String], test: Boolean): Unit =
        for
          to <- Reachability.calledBy(tree, vtables).map(Modules.moduleOf)
          if to != from && to != Modules.root && moduleNames(to)
        do
          val use  = EdgeUse(covers, test)
          val made = edgeUses.getOrElseUpdate((from, to), mutable.LinkedHashMap.empty)

          // Only a kind of use the edge has not seen is worth placing: the first of a kind is what
          // a refusal points at, and resolution has usually recorded the call's own name already.
          if !made.contains(use) then made(use) = site(tree, to, vtables)

      for f <- funcs if !genericInsts(f.name) do
        charge(f.body, Modules.moduleOf(f.name), bodyCovers.getOrElse(f.name, Set.empty), scaffolding(f.name))
      for v <- vals; init <- v.init do
        charge(init, Modules.moduleOf(v.symbol), Set.empty, scaffolding(v.symbol))
      charge(main, mainModule, Set.empty, test = false)

  /** The smallest part of `tree` that still runs code in `to`, which is where a refusal's caret goes
   * — the call, rather than the body around it.
   */
  private def site(tree: Any, to: String, vtables: List[TVtable]): Option[Pos] =
    Reaches.parts(tree).find(Reachability.calledBy(_, vtables).exists(Modules.moduleOf(_) == to)) match
      case Some(part) => site(part, to, vtables).orElse(Reaches.position(tree))
      case None       => Reaches.position(tree)

  /** Whether any module of this compilation has an environment capability out of reach — which is
   * almost never, and is what lets both passes here cost nothing when it is not.
   */
  private def gateAsked: Boolean =
    moduleNarrows.values.exists(c => (c.keySet & Capability.environment).nonEmpty) ||
      Capability.environment.exists(c => !targetProvides(c))

  /** The environment capabilities `module` may not reach: what its own clause gave up, plus — for a
   * module of the program's own — whatever the target does not provide. The library's modules are
   * left out of the second half for the reason `checkGatedModules` gives.
   */
  private def givenUp(module: String): Set[String] =
    (moduleNarrows.getOrElse(module, Map.empty).keySet & Capability.environment) ++
      (if ownModule(module) && !std.carries(module) then Capability.environment.filterNot(targetProvides)
       else Set.empty)

  /** Whether a reference from the module being walked into `module` is one `checkGatedModules` will
   * refuse on the strength of `module`'s own clause.
   *
   * **A name such a reference fails to find is not worth a diagnostic of its own.** A module that
   * requires what this machine lacks may have had its body dropped before analysis — `CProbe` keeps
   * the header of a file whose `c const` it could not measure and nothing else — so everything it
   * declares is missing here for the reason the gate reports. Saying the name is undefined as well
   * would send the reader looking for a typo in a module that is exactly as they wrote it, and the
   * refusal at the same reference already tells them what to change.
   */
  protected def gatedAway(module: String): Boolean =
    val needs = moduleRequires.get(module).map(_.keySet).getOrElse(Set.empty) & Capability.environment

    module != currentModule && needs.nonEmpty && (givenUp(currentModule) & needs).nonEmpty

  /** How a module refers to itself in a diagnostic. The root module has no name to print, and a
   * program's own files are in it, so the common case reads as a sentence rather than as an empty
   * pair of quotes.
   */
  private def here(module: String): String =
    if module.isEmpty then "this module" else s"'$module'"

  /** What each module needs an operating system for, its own clause plus everything it reaches.
   *
   * A module that requires nothing itself still requires whatever it depends on requires — that is
   * the whole of what makes the answer transitive, and it is why a program cannot get at `sysl.fs`
   * by going through something else that does.
   *
   * The walk is memoized per module, so a diamond costs one visit rather than one per path. A module
   * already on the path answers empty rather than recurring, which is only reachable in a program
   * whose module graph has a cycle — already a diagnostic of its own, and the degraded answer here
   * is an under-approximation, so nothing is refused that a correct answer would have allowed.
   */
  private def requirements(): collection.Map[String, Set[String]] = {
    val out  = mutable.HashMap.empty[String, Set[String]]
    // Over every edge a use was recorded on rather than over the module graph alone: a method a body
    // calls is charged without being a dependency the graph is held acyclic over (`charges`).
    val deps = edgeUses.keys.toList.groupMap(_._1)(_._2)

    def of(m: String, path: Set[String]): Set[String] =
      out.get(m) match
        case Some(found)     => found
        case None if path(m) => Set.empty
        case None            =>
          val own   = moduleRequires.get(m).map(_.keySet).getOrElse(Set.empty) & Capability.environment
          val below = deps.getOrElse(m, Nil).flatMap(d => charged(m, d, of(d, path + m))).toSet
          val all   = own ++ below

          out(m) = all
          all

    for m <- deps.keySet ++ moduleRequires.keySet do of(m, Set.empty)
    out
  }

  /** Which of what `to` requires the edge from `from` passes on to `from` itself — and so to every
   * module that reaches `from`.
   *
   * **Two kinds of reference pass nothing on.** One written in **scaffolding** — a `@tests` file, a
   * test — is dropped by every build but `sysl test`, so what it reaches is not something the module
   * ships (`reference/modules.md § A @tests file states its own capabilities`); counting it would
   * refuse a board program over a library's test fixtures. And one written inside a declaration that
   * said `@needs(...)` passes on nothing that declaration names, because that is charged to whoever
   * calls it (`reference/modules.md § A declaration may name what reaching it needs`) — counting it
   * would make one `@needs(os)` function cost a `@no_os` importer the whole module, which is the
   * granularity the annotation exists to give.
   */
  private def charged(from: String, to: String, requires: Set[String]): Set[String] =
    edgeUses.get((from, to)) match
      case None       => requires
      case Some(uses) =>
        effective(uses).map(_._1).filterNot(_.scaffolding).flatMap(u => requires -- u.covers).toSet

  /** One edge's uses as the capability question reads them: a type **named** and nothing more is
   * charged nothing, and each `import` is charged by what the module's references along the edge are
   * charged with.
   *
   * **Naming a type runs nothing** (`reference/modules.md § A type costs what it runs`). A field, a
   * variant's payload, a parameter or a result of a type from a gated module is a shape, and the
   * module's requirement is the cost of its code — which a program pays where it calls a method
   * (`chargeCalls`) and where a value of the type can **die**, since a destructor runs there. So a
   * named use stands for nothing unless the type it named carries a destructor somewhere in what it
   * holds; then it is a reference like any other, at the place the type was named.
   *
   * **An import is charged by its uses** (`reference/modules.md § A declaration may name what
   * reaching it needs`): a file that imports `sysl.fs.write_bytes` for its one `@needs(os)` function
   * has said what that function may write, not that the module reads files. So an import covers
   * whatever every shipping reference along the same edge covers — nothing, the moment one of them
   * sits in an unannotated declaration, which then charges the module at the import as it always
   * did. An import whose every use only names a type is charged nothing, as those uses are; one
   * nothing references at all is charged as written, since there is no use to read it by.
   */
  private def effective(uses: collection.Map[EdgeUse, Option[Pos]]): List[(EdgeUse, Option[Pos])] = {
    val all      = Capability.environment
    val shipping = uses.keys.filter(u => !u.imported && !u.scaffolding).toList
    val (free, run) = shipping.partition(u => u.named.exists(k => !dies(k)))
    val lifted =
      if run.nonEmpty then run.map(_.covers).reduce(_ & _)
      else if free.nonEmpty then all
      else Set.empty[String]

    uses.toList.map { (u, p) =>
      if u.imported then (u.copy(covers = u.covers ++ lifted), p)
      else if u.named.exists(k => !dies(k)) then (u.copy(covers = u.covers ++ all), p)
      else (u, p)
    }
  }

  /** Whether a value of the type `key` names can **die** running code: whether it, or anything it
   * holds, has a destructor (`reference/memory.md § A destructor`).
   *
   * Asked of what the program instantiated, because what a type holds is decided per instantiation —
   * `Holder[T]` holds whatever `T` was chosen — and a type nothing instantiated holds nothing a
   * program could release. **What it holds is followed through `&T`, a slice and an array**, each of
   * which releases what it refers to; **a `*T` and a `weak T` stop it**, owning nothing. A trait and
   * an alias are answered by what they stand for: a trait is a set of methods, which are charged
   * where called, and an alias is the type it names — followed by key where it names a declared
   * type, and resolved where it names an application of one (`type M = Option[&Handle]`), which
   * has no key of its own to follow.
   */
  private def dies(key: String): Boolean =
    diesMemo.getOrElseUpdate(key, {
      val types = variantOwners.getOrElse(key, List(followAlias(key)))

      types.exists(k => dropsDeclared(k) || instances.getOrElse(k, Nil).exists(holdsDrop(_, Set.empty))) ||
        aliasedType(key).exists(holdsDrop(_, Set.empty))
    })

  // Both asked only once the program's instantiations are settled, by `checkGatedModules`, which
  // empties the memo first; the index is what keeps the question from being a scan per use.
  private val diesMemo = mutable.HashMap.empty[String, Boolean]

  private def instances: Map[String, List[Type]] =
    if instanceIndex == null then
      val made = structInsts.values.toList.map(s => s.base -> (s: Type)) :::
        enumInsts.values.toList.map(e => e.base -> (e: Type))

      instanceIndex = made.groupMap(_._1)(_._2)
    instanceIndex

  private var instanceIndex: Map[String, List[Type]] = null

  private def holdsDrop(t: Type, seen: Set[String]): Boolean = t match
    case _: Type.Ptr | _: Type.Weak => false
    case Type.Ref(inner, _)         => holdsDrop(inner, seen)
    case Type.Volatile(inner)       => holdsDrop(inner, seen)
    case Type.Slice(elem, _)        => holdsDrop(elem, seen)
    case Type.Array(_, elem)        => holdsDrop(elem, seen)
    case s: Type.Struct if !seen(s.name) =>
      val fields = structInsts.get(s.name).map(_.fields).getOrElse(s.fields)

      dropsDeclared(s.base) || (fields.map(_._2) ::: s.targs).exists(holdsDrop(_, seen + s.name))
    case e: Type.Enum if !seen(e.name) =>
      val variants = enumInsts.get(e.name).map(_.variants).getOrElse(e.variants)

      dropsDeclared(e.base) ||
        (variants.flatMap(_.fields.map(_._2)) ::: e.targs).exists(holdsDrop(_, seen + e.name))
    case _ => false
}
