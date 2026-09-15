package sh.sysl

import io.github.edadma.cross_platform.*

/** One package of a resolved graph: where its source is, what it says about itself, and what the
 * names in *its* import lines mean.
 *
 * `imports` is per-package and that is the whole of `reference/packages.md § What a dependency's
 * modules are called`'s two-layer identity. A package's own source says `json.Parser` and means
 * whatever *it* calls `json`; a consumer of that package says `json.Parser` and may mean something
 * else entirely. Both resolve through their own table to one canonical name, so the two never have
 * to agree on a spelling and always agree on which code they linked.
 */
case class ResolvedPackage(
    canonical: String,
    root: String,
    config: PackageConfig,
    imports: Map[String, String],
    version: Option[Version] = None,
) {

  /** Whether this is the project being built rather than something it depends on. The root project
   * has no coordinate, and its modules keep the names they were written with.
   */
  def isRoot: Boolean = canonical.isEmpty
}

/** The dependency graph, resolved (`reference/packages.md § Which version you get`).
 *
 * Minimal Version Selection: the version chosen for a package is **the highest minimum anybody
 * asked for**, not the newest that exists. It is a walk taking a maximum rather than a search, which
 * is what makes it a few hundred lines instead of a solver, and it gives three properties that fall
 * out of the arithmetic rather than out of cleverness — adding a dependency cannot silently upgrade
 * an unrelated one, the answer is a pure function of the manifests reachable from this one, and
 * upgrading is an edit to a file rather than a command that walks everything forward.
 *
 * ==Selecting and materializing are two passes, and they have to be==
 *
 * A floor can rise *after* the package it belongs to has been reached: a dependency named 1.2.0 and
 * something further down the graph named 1.4.0, so what was read first is not what gets built. One
 * pass that resolved names as it walked would have built the import table of a version it then
 * stopped using. So the first pass reads manifests and does nothing but raise floors, and the second
 * pass runs over the answer, by which time every version is final.
 */
object Resolve {

  /** One manifest asking for one coordinate at one floor, kept so that `sysl deps` can say who
   * wanted what (`reference/packages.md § Which version you get`).
   *
   * `asker` is named the way a reader can go and look it up: a fetched package by its coordinate, a
   * path dependency by its label, and the root by whatever its manifest calls itself. Not the
   * friendly `package.name` a diagnostic uses — a note about one package can afford that, and a
   * listing of the whole graph is exactly where two packages both calling themselves `json` stop
   * being distinguishable.
   */
  case class Claim(asker: String, version: Version)

  /** Everything the build needs from the package system: the packages in a stable order, the claims
   * that decided their versions, and the `sysl.sum` that should be on disk afterwards.
   */
  case class Graph(packages: List[ResolvedPackage], claims: Map[String, List[Claim]], sums: Sums,
                   sumsChanged: Boolean)

  /** What the selecting pass carries: the version floors, who asked for them, the manifests read so
   * far, the packages that are directories rather than coordinates, and the sums as they stand.
   */
  private case class State(
      selected: Map[String, Version] = Map.empty,
      claims: Map[String, List[Claim]] = Map.empty,
      manifests: Map[(String, Version), (String, PackageConfig)] = Map.empty,
      locals: Map[String, (String, PackageConfig)] = Map.empty,
      sums: Sums = Sums.empty,
      changed: Boolean = false,
  ) {

    /** Every requirement raises a floor and nothing ever lowers one, which is the whole of MVS —
     * and every one of them is written down on the way past, whether or not it won.
     *
     * **This is the only place holding both halves at once.** Downstream has the answer and not the
     * question: `selected` keeps a version with no memory of where it came from, and a version that
     * was passed over has its manifest dropped by `materialize` on purpose. So a claim recomputed
     * from the finished graph can only ever find the one that won, which is the half nobody needed
     * to be told.
     *
     * Keyed on the **slashed** coordinate, as `selected` is, so the two are joined by one key.
     */
    def demanding(asker: String, deps: List[Dependency]): State =
      deps.foldLeft(this) { (s, dep) =>
        dep.origin match
          case Origin.Git(coordinate, version) =>
            val noted = s.copy(claims = s.claims.updatedWith(coordinate)(was =>
              Some(was.getOrElse(Nil) :+ Claim(asker, version))))

            if noted.selected.get(coordinate).forall(_ < version) then
              noted.copy(selected = noted.selected + (coordinate -> version))
            else noted
          case _ => s
      }

    /** A package's hash written into the sums where no line covered it yet.
     *
     * Asked against what is held rather than against how the package arrived, so that a project
     * depending on something another project already fetched still records what it got. A line that
     * is already there and already agrees is not a change, which is what keeps a build from
     * rewriting the file every time.
     */
    def withFetch(dep: Dependency, got: Fetch.Fetched): State =
      (got.hash, dep.origin) match
        case (Some(hash), Origin.Git(coordinate, version)) if !sums.hashOf(coordinate, version).contains(hash) =>
          copy(sums = sums.recording(coordinate, version, hash), changed = true)
        case _ => this
  }

  /** Resolves the graph rooted at `root`, fetching whatever this machine has not got.
   *
   * `cache` is passed in rather than found here so that a test can resolve a whole graph without
   * writing into the machine's own package cache — the same reason `PackageConfig.read` takes text
   * rather than a path.
   *
   * `sharing` names the directories whose modules are in the root's **own name space** rather than
   * under a prefix of their own: the `--lib` source roots, whose files are filed under the empty
   * prefix because that is what the flag means. They are not packages of this graph and contribute no
   * dependencies of their own here — what they contribute is *names already taken*, which is the only
   * thing the collision rule below can be asked about them. Left out, a root's module and a
   * dependency's module could both claim one name and the local one would quietly win, which is the
   * silent winner `§ 9` exists to refuse.
   */
  def graph(root: String, config: PackageConfig, sums: Sums, cache: String,
            sharing: List[String] = Nil): Either[String, Graph] =
    for
      withLocals <- readLocals(config.dependencies, State(sums = sums), cache)
      settled    <- select(withLocals.demanding(rootName(config), config.dependencies), cache)
      rootTable  <- tableOf(PackageConfig.FileName, root :: sharing, config.dependencies, settled)
      packages   <- materialize(settled)
      tables     <- collect(packages)(p => tableOf(owner(p), List(p.root), p.config.dependencies, settled)
                      .map(t => p.copy(imports = t)))
      _          <- checkFloors(tables)
    yield Graph(ResolvedPackage("", root, config, rootTable) :: tables, settled.claims, settled.sums,
                settled.changed)

  /** What the root calls itself when it is the one asking, since it has no coordinate to be named by.
   *
   * A manifest need not name its package, and a listing that then attributed a claim to the empty
   * string would read as though nobody had asked for it — which is the opposite of what the claim
   * says.
   */
  private def rootName(config: PackageConfig): String = config.name.getOrElse("this project")

  /** Every dependency's stated floor against the compiler in hand (`reference/packages.md`).
   *
   * **This is the whole point of the field**, and it is why the check is here rather than left to
   * whatever fails later: a package using something the language grew fails somewhere *inside*
   * itself, with a diagnostic pointing at a line in a tree the consumer did not write and nothing to
   * say the compiler is what is wrong. The manifest is what turns that into one sentence naming the
   * package, the floor and the compiler.
   *
   * The **root** is not checked here — `readPackageConfig` does it, which is the funnel every
   * command's own config comes through, and checking it twice would report it twice.
   *
   * A version this compiler cannot read as three numbers is its own, and the answer to that is to
   * make no claim rather than to refuse a build over it.
   */
  private def checkFloors(packages: List[ResolvedPackage]): Either[String, Unit] =
    Version.ofCompiler(BuildInfo.version) match
      case None => Right(())
      case Some(compiler) =>
        collect(packages)(p => p.config.checkFloor(named(p), compiler)).map(_ => ())

  /** A package as the person who has to act on the message thinks of it — the name it calls itself
   * and the tag they wrote, rather than the dotted canonical form the resolver keys on.
   *
   * `owner` is the other spelling and stays where it is: it labels a *manifest* being read, where
   * the coordinate is the thing being quoted back. This labels a package being refused, where what
   * a reader needs is what to go and look at.
   */
  private def named(p: ResolvedPackage): String = {
    val name = p.config.name.getOrElse(p.canonical)

    p.version.map(v => s"package $name ${v.tag}").getOrElse(s"package $name")
  }

  private def owner(p: ResolvedPackage): String =
    p.version.map(v => s"${p.canonical} ${v.tag}").getOrElse(p.canonical)

  /** The path dependencies of the **root** manifest, which are not part of version selection.
   *
   * A directory beside the consumer has no version to select and no coordinate to be identified by,
   * so it takes no part in MVS and is simply read. A *dependency* may not have one: a relative path
   * in a fetched package would be relative to a checkout that only exists on the machine that made
   * it, which is a build that works for its author and nobody else.
   */
  private def readLocals(deps: List[Dependency], state: State, cache: String): Either[String, State] =
    deps.foldLeft(Right(state): Either[String, State]) { (acc, dep) =>
      dep.origin match
        case Origin.Git(_, _) => acc
        case Origin.Local(_) =>
          for
            before <- acc
            got    <- Fetch.ensure(dep, before.sums, cache)
            theirs <- configOf(got.root)
            _      <- Either.cond(theirs.dependencies.forall(isGit), (),
                        s"'${dep.label}' is a path dependency that itself has a path dependency — a " +
                          "relative path only means something in the project it was written in")
          yield before.copy(locals = before.locals + (dep.canonical -> (got.root, theirs)))
                      .demanding(dep.label, theirs.dependencies)
    }

  private def isGit(dep: Dependency): Boolean = dep.origin match
    case Origin.Git(_, _) => true
    case Origin.Local(_)  => false

  /** The selecting pass: read the manifest of every floor, raise whatever it demands, repeat.
   *
   * It terminates because a floor only rises and a coordinate has finitely many versions, so each
   * step either reads a pair nothing has read or finds none left and stops.
   */
  private def select(state: State, cache: String): Either[String, State] =
    state.selected.toList.sortBy(_._1).find(pair => !state.manifests.contains(pair)) match
      case None => Right(state)
      case Some(pair @ (coordinate, version)) =>
        val dep = Dependency(coordinate, Origin.Git(coordinate, version))

        for
          got    <- Fetch.ensure(dep, state.sums, cache)
          theirs <- configOf(got.root)
          _      <- Either.cond(theirs.dependencies.forall(isGit), (),
                      s"$coordinate ${version.tag} has a path dependency, which only means something " +
                        "in the project it was written in")
          next    = state.withFetch(dep, got)
                      .copy(manifests = state.manifests + (pair -> (got.root, theirs)))
                      .demanding(coordinate, theirs.dependencies)
          settled <- select(next, cache)
        yield settled

  /** Every package the build will read, at the version selection settled on.
   *
   * The manifests map holds every version that was *looked at*, which is more than what is used —
   * a floor that rose left the lower one behind. Reading the answer off `selected` rather than off
   * the manifests is what drops those.
   */
  private def materialize(state: State): Either[String, List[ResolvedPackage]] = {
    val fromGit = state.selected.toList.sortBy(_._1).map { (coordinate, version) =>
      val (dir, config) = state.manifests((coordinate, version))

      ResolvedPackage(coordinate.replace('/', '.'), dir, config, Map.empty, Some(version))
    }

    val fromPath = state.locals.toList.sortBy(_._1).map { (canonical, entry) =>
      ResolvedPackage(canonical, entry._1, entry._2, Map.empty)
    }

    Right(fromGit ::: fromPath)
  }

  /** One package's import table: what each name it writes at the head of an import line means.
   *
   * Built after selection, so a dependency that named 1.2.0 while something else named 1.4.0 gets
   * the manifest of the version that is actually being built — which is where its *name* comes from
   * when no mount overrides it, and is exactly the thing one pass would have got wrong.
   */
  private def tableOf(owner: String, ownerRoots: List[String], deps: List[Dependency], state: State)
      : Either[String, Map[String, String]] = {
    val declared = deps.map(_.canonical).toSet

    for
      direct   <- declaredTable(owner, ownerRoots, deps, state)
      indirect <- inherited(deps, state)
      whole    <- indirect.foldLeft(Right(direct): Either[String, Table]) { (acc, dep) =>
                    for
                      table <- acc
                      dir   <- theirRoot(dep, state)
                      added <- bindings(dep.copy(mount = None), dir).left.map(e => s"$owner: $e")
                      done  <- added.toList.sortBy(_._1).foldLeft(Right(table): Either[String, Table]) {
                                 (t, binding) =>
                                   t.flatMap(inherit(owner, ownerRoots, _, binding, dep, declared))
                               }
                    yield done
                  }
    yield whole.map((k, v) => k -> v._1)
  }

  /** Which package put each name in a table, carried beside the name it resolves to and dropped at
   * the end. The collision test is about *packages* — two of them wanting one name — and the target
   * alone cannot answer that, since a mount stores the coordinate where an unmounted binding stores
   * the coordinate and the module both.
   */
  private type Table = Map[String, (String, String)]

  /** The names a manifest's own `dependencies` block binds, which is what it wrote down. */
  private def declaredTable(owner: String, ownerRoots: List[String], deps: List[Dependency],
                            state: State): Either[String, Table] =
    deps.foldLeft(Right(Map.empty): Either[String, Table]) { (acc, dep) =>
      for
        table <- acc
        dir   <- theirRoot(dep, state)
        added <- bindings(dep, dir).left.map(e => s"$owner: $e")
        _     <- collect(added.keys.toList.sorted)(local => noCollision(owner, ownerRoots, table, local, dep))
      yield table ++ added.map((k, v) => k -> (v, dep.canonical))
    }

  /** Every package reachable through the ones a manifest named, minus those it named itself
   * (`reference/packages.md § What a dependency's modules are called`).
   *
   * ==Imports are transitive, and a manifest names what it *takes* rather than what it sees==
   *
   * A dependency's public surface is made of its own dependencies' types: `syslui-sdl` hands out a
   * `&Fn() -> &View` and `View` is syslUI's, so a consumer that could not name syslUI could not call
   * the one function that package exists for. Requiring it to be declared anyway is asking for a
   * line that says nothing a build could not work out — and the four-coordinate manifest that came
   * of it is what made this change: a demo naming a driver had to name the driver's dependencies,
   * and their dependencies, to compile.
   *
   * **The cost is stated rather than hidden**: a program may import through a package that never
   * promised to keep depending on what it depends on, so a library dropping one of its own
   * dependencies can break a consumer that never named it. Every language with a class path has this
   * and it has not been what people complain about; what they complain about is the ceremony.
   *
   * **Breadth first, so that nearer packages are offered a name first**, which is what makes the
   * precedence below decidable at all.
   */
  private def inherited(deps: List[Dependency], state: State): Either[String, List[Dependency]] = {
    val declared = deps.map(_.canonical).toSet

    def walk(queue: List[Dependency], seen: Set[String], out: List[Dependency])
        : Either[String, List[Dependency]] =
      queue match
        case Nil                                => Right(out)
        case dep :: rest if seen(dep.canonical) => walk(rest, seen, out)
        case dep :: rest =>
          for
            theirs <- theirConfig(dep, state)
            done   <- walk(rest ::: theirs.dependencies, seen + dep.canonical,
                        if declared(dep.canonical) then out else out :+ dep)
          yield done

    walk(deps, Set.empty, Nil)
  }

  /** One name a package further down the graph offers, placed or passed over.
   *
   * **Three levels of precedence, and only a tie inside one of them is refused.** The consumer's own
   * modules win, then what its manifest declared, then what came in through something else — and two
   * packages at the *same* level wanting one name is the collision `§ 9` refuses rather than resolves.
   *
   * **A name nobody asked for never takes one somebody wrote.** A project with its own `json/`, or a
   * dependency it mounted as `json`, keeps that name however many packages three levels down offer a
   * `json` of their own: passing over is not the silent winner the section refuses, because the name
   * that won is the one in front of the person reading the manifest. Refusing here would mean a
   * project's own module names could be broken by a package it has never heard of.
   */
  private def inherit(owner: String, ownerRoots: List[String], table: Table,
                      binding: (String, String), dep: Dependency,
                      declared: Set[String]): Either[String, Table] = {
    val (local, target) = binding

    table.find((name, _) => overlap(name, local)) match
      case Some((_, (_, other))) if other == dep.canonical => Right(table)
      case Some((_, (_, other))) if declared(other)        => Right(table)

      case Some((name, (_, other))) =>
        Left(s"$owner: '$local' and '$name' cannot both be imported — ${sameLibrary(other, dep.canonical)}, " +
          s"and both arrived through something else this project depends on. Name one of them in " +
          s"${PackageConfig.FileName} with a 'mount' to say which name it takes here")

      case None =>
        if ownerRoots.exists(r => Project.modules(r).exists(overlap(_, local))) then Right(table)
        else Right(table + (local -> (target, dep.canonical)))
  }

  /** The manifest of the version of `dep` that is being built, read off the selection rather than
   * off the edge that asked for it — the same rule `theirRoot` follows, for the same reason.
   */
  private def theirConfig(dep: Dependency, state: State): Either[String, PackageConfig] =
    dep.origin match
      case Origin.Local(_) =>
        state.locals.get(dep.canonical).map(_._2).toRight(s"'${dep.label}' was never read")
      case Origin.Git(coordinate, _) =>
        for
          version <- state.selected.get(coordinate).toRight(s"'$coordinate' was never selected")
          entry   <- state.manifests.get((coordinate, version)).toRight(s"'$coordinate' was never read")
        yield entry._2

  /** What a dependency binds in the manifest that named it (`§ 9`).
   *
   * **With no mount, a package's modules come in under their own names.** That is what makes the
   * mount optional in the sense the section argues for: mandatory mounting was rejected because it
   * "would make every project's import lines differ from the library's own documentation", and an
   * import line only matches the documentation if `sqlite.open` is what a consumer writes — which it
   * is not if every name is silently prefixed by something. The section's own collision example says
   * the same thing from the other side: it is a project with a `json/` directory meeting "a
   * dependency preferring `json`", and `json` there is a module name.
   *
   * **A name here is a module path and not a directory**, which is what `Project.modules` settles and
   * is the difference between this working and not. A package namespaced by reverse DNS offers
   * `sh.sysl.table`; its `sh/` and `sh/sysl/` hold no source, so neither is a module and neither is a
   * name it can be asked for. Binding the top-level directory instead made every namespaced package
   * claim `sh`, so any two of them refused to resolve together — with the convention `§ 9` recommends
   * being what guaranteed it, and the mount it sent people to being the tax that section refuses to
   * levy.
   *
   * **A mount hangs the whole package under one segment**, which is the escape hatch for exactly the
   * case the collision example describes: mount it as `ejson` and its `json` is reached as
   * `ejson.json`, leaving the project's own `json` alone. It is unchanged by any of the above — a
   * mount is a name the consumer chose, so there is no tree to read it off.
   *
   * `package.name` is deliberately not consulted. It names the *package*, which is a unit of
   * distribution, and what an import line writes is a *module*, which is a unit of code — sqlite3's
   * package is called `sqlite3` and its module is `sqlite`, and a consumer reaching for the second
   * should not have to say the first.
   */
  private def bindings(dep: Dependency, dir: String): Either[String, Map[String, String]] =
    dep.mount match
      case Some(mount) => Right(Map(mount -> dep.canonical))
      case None =>
        val modules = Project.modules(dir)

        if modules.isEmpty then
          Left(s"'${dep.label}' has no modules — a package is a tree of directories holding source, " +
            "each of them a module, and there is nothing here to import")
        else Right(modules.map(m => m -> Packages.qualify(dep.canonical, m)).toMap)

  /** Where the version of `dep` that is being built has its source, which for a git dependency is the
   * version selection settled on rather than the one this edge asked for.
   */
  private def theirRoot(dep: Dependency, state: State): Either[String, String] =
    dep.origin match
      case Origin.Local(_) =>
        state.locals.get(dep.canonical).map(_._1).toRight(s"'${dep.label}' was never read")
      case Origin.Git(coordinate, _) =>
        for
          version <- state.selected.get(coordinate).toRight(s"'$coordinate' was never selected")
          entry   <- state.manifests.get((coordinate, version)).toRight(s"'$coordinate' was never read")
        yield entry._1

  /** The two ways one module name can be claimed twice, both refused rather than resolved by order.
   *
   * This is `§ 9`'s load-bearing rule: a collision is an error and never a silent winner. The JVM
   * classpath is the counter-example the chapter names — the same package in two jars is decided by
   * jar order, quietly, and what results is a program somebody has to bisect to understand.
   *
   * **What is compared is the module path**, so two packages that both namespace themselves under
   * `sh.sysl` and offer `sh.sysl.sqlite` and `sh.sysl.linenoise` do not collide, because no import
   * line can be read as either. The names they share are `sh` and `sh.sysl`, and neither package
   * declares those — a directory holding no source is not a module (`reference/modules.md`).
   *
   * **`ownerRoots` is a list because the consumer's name space can be more than one directory.** Its
   * head is the consumer itself; the rest are the `--lib` source roots, whose modules are filed under
   * the consumer's prefix and are therefore names just as taken. One of those colliding used to be the
   * silent winner this rule refuses everywhere else — the local module answered and the dependency's
   * was unreachable, with nothing said.
   */
  private def noCollision(owner: String, ownerRoots: List[String], table: Map[String, (String, String)],
                          local: String, dep: Dependency): Either[String, Unit] =
    table.find((name, _) => overlap(name, local)) match
      case Some((name, (_, other))) if other != dep.canonical =>
        Left(s"$owner: '$local' and '$name' cannot both be imported — ${sameLibrary(other, dep.canonical)}. " +
          s"Give one of them a 'mount' in ${PackageConfig.FileName} to say which name it takes here")

      case _ =>
        // The consumer's own modules are in the same name space, and a project with a `json`
        // directory at its root taking a dependency that offers `json` is the common case rather
        // than an exotic one. Refused in the same words, because it is the same collision.
        ownerRoots.find(r => Project.modules(r).exists(overlap(_, local))) match
          case None => Right(())
          case Some(r) =>
            // Named as the reader gave it where it is a source root, and "this project" where it is
            // the consumer itself — the same choice the header requirements make, and for the same
            // reason: what is quoted back is the thing they typed and can go and look at.
            val whose = if r == ownerRoots.head then "this project" else s"the source root '$r'"

            Left(s"$owner: '$local' is both a module of $whose and one ${dep.canonical} offers — " +
              "give the dependency a 'mount' to say what it is called here")

  /** What to say about two packages claiming one module name, which is not always the same thing.
   *
   * **Two major versions of one library is the case worth naming, and transitive imports made it
   * likely.** `§ 4` copies Go's rule that a major above the first rides in the coordinate, so
   * `github.com/e/json` and `github.com/e/json/v2` are two *different* packages to selection — MVS
   * cannot fold them together, and it should not: they are allowed to be incompatible, which is what
   * the suffix says. What they are not allowed to do is quietly both answer `json`, and their module
   * names are identical because a module's name is its directory.
   *
   * A consumer that named neither of them — which is now possible, since a package reached through
   * another is importable — otherwise gets a message about a name collision between two coordinates
   * it has never typed, with no hint that they are one library at two versions.
   *
   * The mount is still the answer where somebody genuinely wants both, so the advice does not
   * change; what changes is that the reader is told what they are looking at.
   */
  private def sameLibrary(a: String, b: String): String = {
    val (one, two) = (a.replace('.', '/'), b.replace('.', '/'))

    if Dependency.withoutMajor(one) == Dependency.withoutMajor(two) && one != two then
      s"$a and $b are two major versions of one library and both are in this graph, and their " +
        "modules have the same names"
    else s"$a and $b claim the same module"
  }

  /** Whether two module paths claim ground the other needs: the same name, or one a path **inside**
   * the other.
   *
   * The nesting half is what keeps the table unambiguous rather than merely unique. A package
   * offering `sh.sysl` and another offering `sh.sysl.table` share no name, but `sh.sysl.table` read
   * as an import answers to both — and picking the longer would be the silent winner this whole rule
   * exists to refuse. So it is a collision, and it is one the reverse-DNS convention makes very
   * unlikely, since a package that namespaces itself puts nothing at `sh/sysl/` for the shorter name
   * to come from.
   */
  private def overlap(a: String, b: String): Boolean =
    a == b || a.startsWith(s"$b.") || b.startsWith(s"$a.")

  /** A package's manifest, where it has one. A package with no file is a package that said nothing,
   * exactly as `§ 1` has it for a project.
   */
  private def configOf(root: String): Either[String, PackageConfig] = {
    val path = s"$root/${PackageConfig.FileName}"

    if !isFile(path) then Right(PackageConfig.empty)
    else
      try
        PackageConfig.read(readFile(path)).left.map(e => s"$root: $e").map { config =>
          // A **dependency's** unknown keys are worth saying more than a project's, because this is
          // the case the manifest cannot report for itself: a package written against a newer sysl
          // than the one in hand, being built by the older one. The root says whose manifest it is,
          // since the reader did not write this file.
          config.warnings.foreach(w => Console.err.println(s"$root: $w"))
          config
        }
      catch case e: Exception => Left(s"cannot read $path: ${e.getMessage}")
  }

  /** Every item, or the first thing wrong — the same shape the config parser uses, and for the same
   * reason: these are not independent failures, and a list of them describes one broken graph
   * several times over.
   */
  private def collect[A, B](items: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    items.foldLeft(Right(Nil): Either[String, List[B]]) { (acc, item) =>
      for
        done <- acc
        one  <- f(item)
      yield done :+ one
    }
}
