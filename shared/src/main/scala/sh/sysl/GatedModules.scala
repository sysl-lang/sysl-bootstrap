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
        ((from, to), pos) <- moduleEdges.toList
        given_up = givenUp(from) if given_up.nonEmpty
        // The least of them by name where a reference is refused for more than one reason, so the
        // message does not vary between runs with the iteration order of a set.
        cap <- (given_up & needed.getOrElse(to, Set.empty)).toList.sorted.headOption
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
          val below = deps.getOrElse(m, Nil).flatMap(of(_, path + m)).toSet
          val all   = own ++ below

          out(m) = all
          all

    for m <- deps.keySet ++ moduleRequires.keySet do of(m, Set.empty)
    out
  }
}
