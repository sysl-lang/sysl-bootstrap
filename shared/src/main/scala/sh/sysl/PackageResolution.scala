package sh.sysl

import io.github.edadma.cross_platform.*

// The driver's half of the package system (`reference/packages.md`): the manifest read off the
// root, the dependencies fetched and version-selected, and the `sysl.sum` checked and written back.
// Everything below resolution is `Resolve`'s and `Fetch`'s.

/** What this compilation depends on, brought onto this machine and turned into what it needs from
 * them (`reference/packages.md § Dependencies`, `§ 5`, `§ 9`).
 *
 * A dependency reaches a compilation the way a `--lib` source tree does — as **more modules**, and
 * as a tree whose C is compiled beside the program's (`reference/ffi.md § A library may carry C`).
 * What is new beside those is the `Packages` table: each fetched package's files are filed under a
 * canonical prefix taken from its coordinate, and the names its import lines write are read back
 * through the manifest that named it.
 *
 * ==A `--lib` source root's own `dependencies` are here too, and used not to be==
 *
 * A package reached as a source root is the same package it is by coordinate, so what it depends on is
 * a property of *it* rather than of the road it arrived by — the argument `§ 13` makes about the
 * allocator, and the reason `§ 8`'s header requirements are read from a root as well. Handed the same
 * directory as a path, the driver read its `.sysl` and nothing else, so the dependency was neither
 * fetched nor mentioned: what a caller got was the unresolved-name cascade that follows a package
 * whose imports point at code nobody brought, none of it naming the package or the flag.
 *
 * **One graph rather than one per root**, which is what makes selection mean anything: MVS takes its
 * maximum over every claim at once (`§ 5`), so a coordinate two roots share resolves to one copy at one
 * version rather than to two the linker would have to choose between. The roots' entries are folded
 * into the list the project's own are read from, so `demanding`, `select` and the root's import table
 * cover them with nothing added.
 *
 * **Their bindings land in the root table**, which follows from where their source lands rather than
 * being a choice: a `--lib` root's files are filed under the empty prefix, the project's own name
 * space, because that is what `--lib` means. So the names its manifest binds are names the project's
 * own files can write too — no more true of a dependency's name than it always was of the root's own
 * modules.
 *
 * A `path` dependency of a root is resolved exactly as the project's own is, by the same
 * `Fetch.ensure`. Reading it against the root it was written in would be a *second* meaning for one
 * field, and whether a relative path should be read against a project root rather than the working
 * directory is the same question on both roads.
 *
 * `sysl.sum` is written back where a package was fetched that no line covered, so the first build
 * after adding a dependency records what it got and every build after that is checked against it.
 * Writing it is not fatal if it fails: a read-only checkout should still build, and the alternative
 * is refusing to compile over a file that exists to be compared against next time.
 */
private def dependencies(cfg: Config, project: PackageConfig, roots: List[String], os: Os)
    : Either[String, PackageSources] =
  for
    fromRoots <- libDependencies(roots)
    dev        = devDependencies(cfg, project)
    declared   = project.dependencies ::: dev ::: fromRoots
    got       <- if declared.isEmpty then Right(PackageSources.none)
                 else resolveDependencies(cfg, project.copy(dependencies = declared), roots, os)
  yield got.copy(devModules = devModules(got, dev))

/** Resolving a non-empty dependency list against this machine's cache, and recording what it got.
 *
 * The source roots are handed over as well as their dependencies, and for a different purpose: their
 * modules sit in the project's own name space, so they are names a dependency may not also claim
 * (`§ 9`), and nothing in a manifest tells `Resolve` they are there.
 */
private def resolveDependencies(cfg: Config, project: PackageConfig, roots: List[String], os: Os)
    : Either[String, PackageSources] =
  for
    graph <- resolvedGraph(cfg, project, roots)
    files <- collectPackages(graph, os)
  yield
    reportRaised(project, graph)
    files

/** The graph alone, which is what a build and `sysl deps` have in common.
 *
 * Split out so the command reads the same graph the build resolves rather than one composed to look
 * like it — a listing that disagreed with the compilation standing beside it would be worse than no
 * listing. `collectPackages` and the note are the build's own half and stay above.
 *
 * **Writing the sums back is part of resolving rather than part of building.** A line is recorded
 * because a package was fetched, and both callers fetch; leaving it to the build would mean the
 * first `sysl deps` after adding a dependency downloaded a package and recorded nothing about what
 * it got.
 */
private def resolvedGraph(cfg: Config, project: PackageConfig, roots: List[String])
    : Either[String, Resolve.Graph] = {
  val root = projectRoot(cfg.file)

  for
    cache <- Fetch.cacheRoot(root)
    sums  <- readSums(root)
    graph <- Resolve.graph(root, project, sums, cache, roots, featureRequest(cfg),
               testing = cfg.command == "test")
  yield
    if graph.sumsChanged then writeSums(root, graph.sums)
    graph
}

/** The root's feature request, as the command line stated it.
 *
 * It is a straight reading of three flags, and the one thing worth saying is what is *not* here:
 * whether `sysl test` widens the request to every feature is decided in `FeatureResolution` off the
 * `testing` flag beside this, because that rule is about `default` and `allFeatures` interacting and
 * belongs with the code that knows what those mean.
 */
private def featureRequest(cfg: Config): FeatureRequest =
  FeatureRequest(cfg.features, cfg.noDefaultFeatures, cfg.allFeatures)

/** Say so when the build is against a **higher** version than this project asked for
 * (`reference/packages.md § Which version you get`).
 *
 * **Selection is silent by design and this is the one case worth breaking that for.** MVS raises
 * floors constantly — that is what it is — and a line per raise would be a wall of them, every one
 * about a package nobody typed. What is different here is that the version came from *this
 * manifest*: somebody wrote `0.2.0`, the build used `0.2.1`, and nothing in the file they are
 * reading says so.
 *
 * It matters more now that a package reached through another is importable (`§ 9`), because the
 * thing that raised the floor may be a package this project never named at all — so the note names
 * who asked, found by looking for a manifest in the graph that wanted exactly what was selected.
 *
 * **A note rather than a refusal.** The higher version is the right answer and the build is correct;
 * what a reader wants is to know it happened, so that a manifest saying `0.2.0` while every build
 * uses `0.2.1` is something they can go and fix rather than something they discover from a bug.
 */
private def reportRaised(project: PackageConfig, graph: Resolve.Graph): Unit =
  for
    dep      <- project.dependencies
    asked    <- dep.origin match
                  case Origin.Git(_, version) => Some(version)
                  case Origin.Local(_)        => None
    resolved <- graph.packages.find(p => p.canonical == dep.canonical).flatMap(_.version)
    if asked < resolved
  do
    val who = graph.packages
      .filterNot(_.isRoot)
      .filter(_.config.dependencies.exists(d =>
        d.canonical == dep.canonical && (d.origin match
          case Origin.Git(_, v) => v == resolved
          case Origin.Local(_)  => false)))
      .map(p => p.config.name.getOrElse(p.canonical))

    // Named as the manifest writes them — `0.2.0` and not `v0.2.0`, since what a reader is being
    // asked to compare this against is the line they typed, and the tag is the repository's spelling
    // rather than theirs.
    val asking = who.distinct.sorted

    val because = asking match
      case Nil        => ""
      case one :: Nil => s", which $one asks for"
      case many       => s", which ${many.init.mkString(", ")} and ${many.last} ask for"

    Console.err.println(s"sysl: note: '${dep.label}' is named at $asked and the build " +
      s"selected $resolved$because")

/** Which module paths a file of this project would have to write to reach a dev dependency.
 *
 * **The translation from a coordinate to something an import line can say**, and it has to happen
 * somewhere: `Dependency.canonical` is `github.com.sysl-lang.quickjs`, which is not a module path
 * and is not even spellable as one — a hyphen is not an identifier character. What a file writes is
 * the mount, `sh.sysl.quickjs`, and the table that relates the two is the root's own row of
 * `Packages.imports`.
 *
 * The root's row is the right one because the check is about the root's files. A dependency's own
 * files resolve their imports through their own row, and a dev dependency is not in anybody's graph
 * but this project's.
 */
private def devModules(got: PackageSources, dev: List[Dependency]): Set[String] = {
  val prefixes = dev.map(_.canonical).toSet

  got.packages.imports.getOrElse("", Map.empty).collect {
    case (written, canonical) if prefixes.exists(p => canonical == p || canonical.startsWith(s"$p.")) =>
      written
  }.toSet
}

/** The root project's `dev_dependencies`, when this build is the one they are for.
 *
 * **`sysl test` and nothing else** (`reference/packages.md § Dependencies a test alone needs`). A
 * `dev_dependencies` entry is imported only from a `@tests` file or a `@test` function, and every
 * other build drops that source before analysis (`Tests.stripSource`) — so on a `build`, a
 * `build-lib` or a `build-c` there is nothing left that could name one, and resolving it would fetch
 * and compile a package the compilation cannot refer to. Cargo draws the line in the same place:
 * `cargo build` does not build a dev-dependency and `cargo test` does.
 *
 * **The root project's only**, which is what prunes them from a consumer's graph. A fetched package
 * is walked through its own `dependencies`, and nothing anywhere reads a *dependency's*
 * `devDependencies` — so the pruning is an omission rather than a filter, and a package added to the
 * graph tomorrow gets it for free. A `--lib` source root is somebody else's package for this
 * purpose: its tests are not the ones being run.
 */
private def devDependencies(cfg: Config, project: PackageConfig): List[Dependency] =
  if cfg.command == "test" then project.devDependencies else Nil

/** What each `--lib` source root says it depends on (`reference/packages.md § What a project is
 * called`).
 *
 * A root with no manifest depends on nothing, exactly as for the header requirements and the allocator
 * beside it, and one whose manifest will not parse stops the build rather than being skipped — the
 * same three answers `libHeaderNeeds` and `libAllocators` give, off the same read.
 */
private def libDependencies(roots: List[String]): Either[String, List[Dependency]] =
  roots.foldLeft[Either[String, List[Dependency]]](Right(Nil)) { (acc, root) =>
    for
      seen   <- acc
      config <- readPackageConfig(root)
    yield seen ::: config.dependencies
  }

/** Each fetched package's source, filed under the canonical prefix that keeps its module names
 * apart from every other package's — and each one's directory, which is a tree the C walk visits.
 */
private def collectPackages(graph: Resolve.Graph, os: Os): Either[String, PackageSources] = {
  val fetched = graph.packages.filterNot(_.isRoot)

  try
    val each = fetched.map(p => p -> Project.collect(p.root, Some(os)))

    each.find(_._2.isEmpty) match
      case Some((p, _)) => Left(s"'${p.canonical}' holds no sysl source files")
      case None =>
        val owned = each.flatMap((p, sources) => sources.map(_ -> p.canonical)).toMap

        // Keyed by where each package's C actually is, which is what the compilation will name it
        // — and walked only for a package that declared something, so a build depending on packages
        // that use no macros reads no C here at all.
        val declared = fetched.filter(_.config.defines.nonEmpty).foldLeft[
            Either[String, Map[String, List[String]]]](Right(Map.empty)) { (acc, p) =>
          for
            seen <- acc
            mine <- p.config.carriedDefines(Project.cSources(p.root, Some(os)).map(_.name))
              .left.map(err => s"'${p.canonical}': $err")
          yield seen ++ mine
        }

        declared.flatMap(defines => Right(PackageSources(
          each.flatMap(_._2),
          Packages(owned, graph.packages.map(p => p.canonical -> p.imports).toMap),
          fetched.map(_.root),
          fetched.flatMap(p =>
            p.config.headers.toList.sortBy(_._1).map((name, h) => HeaderNeed(p.canonical, name, h.why, h.env))),
          fetched.flatMap(p =>
            p.config.pkgConfig.toList.sortBy(_._1).map((mod, why) => LibNeed(p.canonical, mod, why))),
          fetched.flatMap(p => p.config.allocator.map(p.canonical -> _)),
          defines,
        )))
  // A malformed per-OS directory is a mistake in the package rather than a package that would not
  // read (`reference/modules.md § Platform selection`), and the message names the directory and
  // lists the operating systems there are — so wrapping it in "cannot read a package" would bury
  // the only part worth having.
  catch
    case e: SelectionError => Left(e.getMessage)
    case e: Exception      => Left(s"cannot read a package: ${e.getMessage}")
}

private def readSums(root: String): Either[String, Sums] = {
  val path = s"$root/${Sums.FileName}"

  if !isFile(path) then Right(Sums.empty)
  else
    try Sums.read(readFile(path))
    catch case e: Exception => Left(s"cannot read $path: ${e.getMessage}")
}

private def writeSums(root: String, sums: Sums): Unit =
  try writeFile(s"$root/${Sums.FileName}", sums.render)
  catch
    case e: Exception =>
      Console.err.println(s"warning: cannot write ${Sums.FileName}: ${e.getMessage}")

/** The project root: the directory the driver was given, or the one holding the file it was given.
 *
 * `reference/modules.md` settles that the driver is *given* a root rather than discovering one, so
 * this never searches upward — a build that walked up would depend on directories above the one
 * named.
 */
private def projectRoot(file: String): String =
  if isDirectory(file) then file
  else
    val slash = math.max(file.lastIndexOf('/'), file.lastIndexOf('\\'))

    if slash >= 0 then file.substring(0, slash) else "."

/** The package an **example** belongs to: the root two levels above a program that sits in a
 * package's `examples/` directory, or nothing (`reference/packages.md § A package may carry
 * examples`).
 *
 * **This is the half that makes the feature worth having.** Excluding `examples/` from the library
 * build is what lets a package carry one at all; without this, building it means knowing to write
 * `--lib ..`, and a demo you have to know a flag to build is half an answer — which is what card
 * `0194 h` says a package format that cannot carry a demo already is.
 *
 * **It is not an upward search, and the distinction is the whole of why it is allowed here.**
 * `readPackageConfig` looks beside the sources on purpose, because a search that walked upward would
 * make a build depend on directories above the one named. This looks at exactly two levels and only
 * where the intervening one is literally `examples` — a rule about the shape of a path rather than a
 * hunt, so a reader can apply it by looking, and nothing outside a package's own tree can be picked
 * up by accident.
 *
 * The manifest has to be there. A directory called `examples` beside a tree that is not a package is
 * somebody's ordinary folder, and answering with its parent would compile a program against a
 * library nobody claimed.
 */
private[sysl] def owningPackage(file: String): Option[String] = {
  val root = projectRoot(file)

  // Two shapes reach here and both are ordinary: `examples/demo.sysl`, whose root is the `examples`
  // directory itself, and `examples/demo/`, whose root is a directory inside it.
  val pkg =
    if Project.basename(root) == Project.ExamplesDir then Project.parentOf(root)
    else Project.parentOf(root).filter(p => Project.basename(p) == Project.ExamplesDir)
           .flatMap(Project.parentOf)

  pkg.filter(p => isFile(s"$p/${PackageConfig.FileName}"))
}

/** The project config, read from the root this invocation was given (`reference/packages.md`).
 *
 * **A missing file is not an error.** A single-file program has no config and wants none, so what
 * comes back is the empty one — the same shape `reference/modules.md` gives the anonymous root
 * module. A file that is *there* and will not read is a different thing entirely and stops the
 * build: somebody wrote it, and building while ignoring it would be building something other than
 * what they asked for.
 *
 * The file is looked for beside the sources rather than searched for upwards.
 * `reference/modules.md` settles that the driver is *given* a root rather than discovering one, and
 * a search that walked upward would make a build depend on directories above the one named.
 */
private def readPackageConfig(file: String): Either[String, PackageConfig] = {
  val path = s"${projectRoot(file)}/${PackageConfig.FileName}"

  if !isFile(path) then Right(PackageConfig.empty)
  else
    try
      for
        config <- PackageConfig.read(readFile(path))
        // Checked here because this is the one funnel every command's root config comes through, so
        // `build`, `run`, `test`, `build-c` and `build-lib` are all held to the floor by one line. A
        // dependency's floor is checked where its manifest is read, in `Resolve.graph`.
        _ <- Version.ofCompiler(BuildInfo.version)
               .map(config.checkFloor("this project", _))
               .getOrElse(Right(()))
      yield config
    catch case e: Exception => Left(s"cannot read $path: ${e.getMessage}")
}

/** `sysl deps` — the resolved graph, and who asked for each version (`reference/packages.md § Which
 * version you get`).
 *
 * ==What the command is for==
 *
 * A package reached through another is importable as of 0.0.73 (`§ 9`), so a program can be built
 * against packages its own manifest never names — and until this command there was no way to answer
 * *what am I compiling against* from the files in front of you. Selection is silent by design and is
 * right to be: MVS raises floors constantly, and a line per raise would be a wall of them. What was
 * missing is somewhere to go and **ask**.
 *
 * It answers with the claims rather than only the outcome, which is the half nothing else can
 * reconstruct: `reportRaised` above scans the graph for a manifest that wanted what was selected, and
 * the manifest of a version that was *passed over* is dropped by `materialize` on purpose. So the
 * claim that lost — the whole reason a version is higher than somebody expected — exists only because
 * `Resolve.State.demanding` wrote it down on the way past.
 *
 * ==What it prints==
 *
 * {{{
 * app 0.1.0
 *
 * github.com/e/a    1.0.0
 *     app asks for 1.0.0
 * github.com/e/buf  1.4.0  (raised)
 *     github.com/e/a asks for 1.2.0
 *     github.com/e/b asks for 1.4.0
 * }}}
 *
 * `(raised)` marks a coordinate some claim on which is **below** what was selected. That is a wider
 * net than the note `reportRaised` prints, deliberately: the note fires only when the *root's own*
 * manifest was overtaken, because that is the one line a reader can go and edit, and this catches a
 * dependency overtaken by its sibling as well.
 *
 * A path dependency takes no part in selection and has no version, so it prints its directory
 * instead — which is the only thing that says which tree it is.
 */
/** `sysl vendor` — every package this project depends on, put beside the manifest.
 *
 * **The whole of it is making the directory and then resolving.** `Fetch.cacheRoot` prefers a
 * `vendor/` beside the manifest over the machine's cache, so once the directory exists the ordinary
 * resolution fetches into it, and every later build reads from it and asks the network nothing.
 * There is no copy step and no second layout: vendoring is the cache moved into the project.
 *
 * A dependency that is a **path** is not vendored and cannot be. It is a directory somebody is
 * editing beside this one — `§ 6` keeps no sum for it precisely because it is expected to change —
 * so copying it would freeze the thing whose whole purpose is not to be frozen. It is named in what
 * this prints, so that a reader is told rather than left to notice.
 */
private def vendorAll(cfg: Config, project: PackageConfig, roots: List[String]): Int = {
  val root = projectRoot(cfg.file)
  val dir  = s"$root/${Project.VendorDir}"

  try Project.makeDirectories(dir)
  catch case e: Exception => return fail(s"cannot make '$dir': ${e.getMessage}")

  val listed =
    for
      fromRoots <- libDependencies(roots)
      graph     <- resolvedGraph(cfg, project.copy(dependencies = project.dependencies ::: fromRoots),
                     roots)
    yield graph

  listed match
    case Left(err) => fail(err)
    case Right(graph) =>
      val vendored = graph.packages.filterNot(_.isRoot).filter(_.version.isDefined)
      val local    = project.dependencies.count(d => d.origin match
        case Origin.Local(_) => true
        case _               => false)

      stdout(s"vendored ${vendored.length} package${if vendored.length == 1 then "" else "s"} " +
        s"into ${Project.VendorDir}/\n")

      if local > 0 then
        stdout(s"\n$local path dependenc${if local == 1 then "y is" else "ies are"} not vendored — a " +
          "path is a directory you are editing, and freezing a copy of it is the one thing it is " +
          "there not to do\n")

      0
}

private def showDeps(cfg: Config, project: PackageConfig, roots: List[String]): Int = {
  val listed =
    for
      fromRoots <- libDependencies(roots)
      // **`deps` lists what a project takes, so it shows the dev half too** -- unlike a build,
      // which resolves them only for `test`. A reader asking what this project depends on wants
      // the whole answer with the parts labelled, and the label is what says a consumer will not
      // fetch that one.
      graph     <- resolvedGraph(cfg, project.copy(dependencies =
                     project.dependencies ::: project.devDependencies ::: fromRoots), roots)
    yield graph

  listed match
    case Left(err)    => fail(err)
    case Right(graph) => printGraph(project, graph)
}

/** The graph as a reader sees it, once it has resolved. */
private def printGraph(project: PackageConfig, graph: Resolve.Graph): Int = {
  val depended = graph.packages.filterNot(_.isRoot)
  val spelling = coordinates(graph)
  val devOnly  = project.devDependencies.map(_.canonical).toSet --
                   project.dependencies.map(_.canonical).toSet

  stdout(s"${project.name.getOrElse("this project")}${project.version.fold("")(v => s" $v")}\n")

  if depended.isEmpty then
    stdout("\nthis project depends on nothing\n")
    return 0

  def named(p: ResolvedPackage): String = spelling.getOrElse(p.canonical, p.canonical)

  // Padded against the whole listing rather than per line, so the versions form a column a reader can
  // run an eye down — the same reason `sysl targets` measures its own names first.
  val width = depended.map(named(_).length).max

  stdout("\n")

  for p <- depended do
    val claims = graph.claims.getOrElse(named(p), Nil)
    // A claim below the selection is what makes this coordinate worth stopping at, and it is asked of
    // the claims rather than of the note above, which only ever sees the root's own manifest.
    val raised = p.version.exists(v => claims.exists(_.version < v))
    // A path dependency has no version to print, so it prints the directory instead — which is the
    // only thing that says which tree it is.
    val at     = p.version.map(_.toString).getOrElse(p.root)

    // Marked rather than listed apart, because what a reader is checking is usually one package
    // and the question is which kind it is -- and a package reached *through* a dev dependency is
    // in the same position without being named in either block, so a second section would have
    // nowhere to put it.
    val kind   = if devOnly.contains(p.canonical) then "  (dev)" else ""

    stdout(s"${named(p).padTo(width, ' ')}  $at$kind${if raised then "  (raised)" else ""}\n")

    // In the order the walk asked, which is the root's manifest first and then breadth first through
    // the graph — so the line a reader is most likely to be able to edit is the one at the top.
    for claim <- claims do stdout(s"    ${claim.asker} asks for ${claim.version}\n")

  0
}

/** Each package's canonical name back to the coordinate a manifest actually wrote.
 *
 * **It cannot be computed from the canonical name, which is the trap worth naming.** `canonical` is
 * the coordinate with its slashes replaced by dots, and a coordinate is full of dots already, so
 * `github.com.e.buf` un-replaced is `github/com/e/buf` and names nothing. The claims are keyed on the
 * real coordinate, and every fetched package has at least one claim — that is how it reached
 * `selected` at all — so the keys are the whole answer. A path dependency is absent and keeps its
 * label, which is what `Dependency.canonical` gives it and is the only name it has.
 */
private def coordinates(graph: Resolve.Graph): Map[String, String] =
  graph.claims.keys.map(c => c.replace('/', '.') -> c).toMap
