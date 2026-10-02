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
      val needed = requirements()

      for
        ((from, to), made) <- edgeUses.toList
        uses = effective(made)
        given_up = givenUp(from) if given_up.nonEmpty
        // What a reference written inside a `@needs(...)` declaration reaches is out of reach of its
        // callers for what that declaration names, and `DeclCapabilities` refuses them at the call —
        // so for those capabilities the reference itself is not this check's to refuse. The first
        // reference that IS refused is the one reported, so the caret lands on a line to change.
        (use, pos) <- uses.find((u, _) => (given_up & (needed.getOrElse(to, Set.empty) -- u.covers)).nonEmpty)
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

        recover(())(err(s"this reaches '$to', which requires '$cap', and $why"))
  }

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
    val deps = moduleEdges.keys.toList.groupMap(_._1)(_._2)

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

  /** One edge's uses with each `import` charged by what the module's references along the edge are
   * charged with.
   *
   * **An import is charged by its uses** (`reference/modules.md § A declaration may name what
   * reaching it needs`): a file that imports `sysl.fs.write_bytes` for its one `@needs(os)` function
   * has said what that function may write, not that the module reads files. So an import covers
   * whatever every shipping reference along the same edge covers — nothing, the moment one of them
   * sits in an unannotated declaration, which then charges the module at the import as it always
   * did. An import nothing references is charged as written, since there is no use to read it by.
   */
  private def effective(uses: collection.Map[EdgeUse, Option[Pos]]): List[(EdgeUse, Option[Pos])] = {
    val made   = uses.keys.filter(u => !u.imported && !u.scaffolding).map(_.covers)
    val lifted = made.reduceOption(_ & _).getOrElse(Set.empty)

    uses.toList.map((u, p) => if u.imported then (u.copy(covers = u.covers ++ lifted), p) else (u, p))
  }
}
