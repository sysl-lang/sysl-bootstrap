package sh.sysl

/** Which package each file came from, and what the names at the head of its import lines mean
 * (`reference/packages.md § What a dependency's modules are called`).
 *
 * ==Identity is two-layered, and this is the layer that is not identity==
 *
 * A module's name is its directory path relative to the project root (`reference/modules.md`),
 * which makes every module name **local and relative** — so a fetched `json` and a project's own
 * `json` are the same name with no domain in the path to separate them. Go does not have this
 * problem because its import path *is* the module name and is globally unique; sysl's rule is the
 * opposite one.
 *
 * The answer is to keep names local and give each package a **canonical prefix** taken from its
 * coordinate, so a module `json` in `github.com/e/sysl-json` is really
 * `github.com.e.sysl-json.json`. That name is what `Modules.qualify` keys tables by and therefore
 * what `reference/modules.md § Separate compilation` mangles into every symbol, which is the
 * property that makes two consumers naming one package differently still link one copy of it. What
 * a *file* writes stays short: the prefix is added on the way in, and the leading segment of a
 * written path is read back through this table.
 *
 * **The mount is never the identity.** If one project mounted a package as `json` and another as
 * `ejson`, a mangler keyed off the local name would emit `json$Parser.next` in one build and
 * `ejson$Parser.next` in the other — the same source producing different symbols, which makes a
 * shared artifact cache worthless and two consumers of one package unlinkable into one program.
 *
 * The empty prefix is the project being built. Its modules keep the names they were written with, so
 * a program with no dependencies goes through all of this unchanged — which is the point, and is why
 * `none` is what every existing caller gets.
 */
case class Packages(of: Map[Source, String] = Map.empty, imports: Map[String, Map[String, String]] = Map.empty) {

  /** The canonical prefix of the package a file belongs to, or the empty one for the project. */
  def prefixOf(file: Source): String = of.getOrElse(file, "")

  /** What a written module path means, in a file of the package `prefix`.
   *
   * Per-package, which is the whole point: a dependency's own source says `json` and means whatever
   * *it* calls `json`, while a consumer of that dependency says `json` and may mean something else.
   * Both arrive at one canonical name without ever having to agree on a spelling.
   *
   * **The whole path is offered and not its first segment**, because what a package binds is a
   * module path — `sh.sysl.table` for one namespaced by reverse DNS, whose `sh/` holds no source
   * and is therefore no module of its own (`reference/modules.md`). A single-segment lookup could
   * only ever have found `sh`, which every such package would claim and none of them declares.
   *
   * The match is by longest key, so a name reaches the most specific package that offers it and
   * anything under that name comes with it: `sh.sysl.table.sub` is answered by `sh.sysl.table` and
   * keeps its tail. At most one key can match at all — resolution refuses two packages whose paths
   * nest — so the longest is a tie-break that never has a tie to break, and is written that way so
   * that a bug in the refusal is a wrong answer rather than an arbitrary one.
   */
  def mounted(prefix: String, written: String): Option[String] =
    imports.get(prefix).flatMap { table =>
      table.keys.filter(k => written == k || written.startsWith(s"$k."))
        .maxByOption(_.length)
        .map(k => table(k) + written.drop(k.length))
    }

  /** Whether there is anything here at all — asked before any of the work below is done, since a
   * program with no dependencies is the ordinary case.
   */
  def isEmpty: Boolean = of.isEmpty && imports.isEmpty
}

/** One package's declared need of a header it includes and does not carry (`reference/packages.md §
 * Capabilities`).
 *
 * `who` is the package's coordinate rather than its module name, because that is what a consumer
 * wrote in their `dependencies` block and therefore what they can go and look at. `why` is the
 * package's own prose and is quoted rather than summarised.
 *
 * `env` is the environment variable the ecosystem conventionally keeps this path in, where there is
 * one — `PICO_SDK_PATH` for the pico-sdk. **A variable's NAME is not a path**: it is the same on
 * every machine in the world, which is what makes it a property of the package rather than of
 * whoever is building it, and is why the manifest may carry it when it may not carry a directory.
 */
case class HeaderNeed(who: String, name: String, why: String, env: Option[String] = None)

/** One package's declared need of an **installed library**, under the name `pkg-config` files it as
 * (`reference/packages.md § Capabilities`).
 *
 * The same shape as `HeaderNeed` and deliberately not the same type. A header requirement is answered
 * by a directory somebody types; this one is answered by asking the machine, and the two produce
 * different refusals — one says *say where these headers are*, the other says *install this library,
 * or say where it is*. Folding them together would mean one message that has to hedge about which it
 * is talking about.
 */
case class LibNeed(who: String, module: String, why: String)

/** What a project's `dependencies` block brings to one compilation.
 *
 * Five things and not one, because a package reaches a build by more than one road. Its `.sysl` is
 * **more modules** and joins the compilation's own; the table keeps its module names apart from
 * every other package's; its directory is a tree whose C is compiled and linked beside the
 * program's (`reference/ffi.md § A library may carry C`, `NativeSources`); what that C needs to
 * *find* is something only the consumer can supply, so the package states it and the driver checks
 * it; and it may name the pair of C functions the whole program allocates through.
 *
 * The third is here because it was once missing. A package's sysl compiled and its C was dropped
 * with no warning, so a build against any package carrying a shim ended at the linker naming
 * symbols that package's own C defines — which `reference/packages.md § No build scripts, ever` had
 * already promised would not happen, on the grounds that sysl compiles a package's C declaratively
 * and needs no build script to do it.
 *
 * `defines` is what each package declared for its **own carried C** (`reference/packages.md § No
 * build scripts, ever`), already keyed by the path the file will be compiled from: a package's
 * manifest names it relative to the package root, and where that root is on this machine is known
 * here and nowhere downstream.
 *
 * `allocators` is carried as the **declarations** rather than as the answer, because the project's
 * own is not here: the root is not a fetched package, and `Allocator.choose` has to see every claim
 * at once to say which wins or that two disagree. That is the same shape as `needs`, which is folded
 * together with the project's own headers at the one place holding both.
 */
case class PackageSources(sources: List[Source], packages: Packages, roots: List[String],
                          needs: List[HeaderNeed] = Nil,
                          libs: List[LibNeed] = Nil,
                          allocators: List[(String, Allocator)] = Nil,
                          defines: Map[String, List[String]] = Map.empty,
                          /** The module paths **as a file writes them** that reach a
                            * `dev_dependencies` package, and so may be imported only from this
                            * project's tests (`Tests.checkDevImports`).
                            *
                            * Written paths rather than canonical prefixes, because an import line
                            * carries neither the coordinate nor its dots: a package mounted from
                            * `github.com/sysl-lang/quickjs` is imported as `sh.sysl.quickjs`, and a
                            * coordinate is not even spellable as a path — `sysl-lang` has a hyphen
                            * in it. The translation between the two is `Packages.mounted`, and it
                            * is done once here rather than at every import.
                            *
                            * Empty on every build but `sysl test`, because that is the only one
                            * that resolves a dev dependency at all.
                            */
                          devModules: Set[String] = Set.empty,
                          /** The features the **root project** has enabled, which its own files may
                            * be gated on as `feature_<name>` symbols (`Conditional`).
                            *
                            * It rides here because a fetched package's files are stamped where they
                            * are read and the root's are read before anything is resolved — so this
                            * is the resolver's answer travelling back to the one caller that has to
                            * apply it itself. Empty for a build that resolves nothing.
                            */
                          rootFeatures: Set[String] = Set.empty)

object PackageSources {

  /** A project with no dependencies, which is the ordinary case and costs nothing. */
  val none: PackageSources = PackageSources(Nil, Packages.none, Nil)
}

object Packages {

  /** No packages: one project, its own modules, nothing fetched. */
  val none: Packages = Packages()

  /** The canonical prefix and a module's own name, joined — and the module's own name alone where
   * the package is the project being built.
   */
  def qualify(prefix: String, module: String): String =
    if prefix.isEmpty then module
    else if module.isEmpty then prefix
    else s"$prefix.$module"
}
