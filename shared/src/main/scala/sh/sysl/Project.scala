package sh.sysl

import java.io.IOException

import io.github.edadma.cross_platform.*

/** Reading a project off the filesystem: which files one invocation compiles, and what each one's
 * location says about the module it belongs to (`reference/modules.md`).
 *
 * A module is a directory and its name is that directory's path **relative to the project root**, so
 * the root is the one thing a caller has to supply and everything else follows from where a file was
 * found. It lives apart from the driver because the driver is not the only caller: a test that
 * compiles a program written on disk asks the same question, and asking it a second way would let
 * the two disagree about what a project is.
 */
/** What a malformed per-OS directory raises (`reference/modules.md § Platform selection`).
 *
 * It has a type of its own because one caller — `Project.modules` — deliberately tolerates a
 * directory it cannot read, and a mistake in the tree must not be swallowed by that tolerance. There
 * is no source position to hang it on: the mistake is a *name on disk*, so the message names the
 * directory and the driver reports it as it reports any other unreadable tree.
 */
final class SelectionError(message: String) extends Exception(message)

object Project {

  /** The source files one invocation compiles.
   *
   * Pointing at a **directory** makes it the root and compiles the whole tree beneath it: the files
   * directly in it are the anonymous root module, and each sub-directory is a module named by the
   * path down to it. Each file carries the segments it was found under, which is what the compiler
   * holds its `module` header to.
   *
   * Naming a single **file** compiles that file alone, as the root module with nothing else in it.
   *
   * `os` is which operating system's per-OS directories to take (`reference/modules.md § Platform
   * selection`), and it has **no default** on purpose: a walk that guessed would drop half a tree
   * with nothing said, which is the one failure mode this whole axis has. `None` takes every one of
   * them, which is what a command that reads a tree rather than compiling it wants — see [[Every]].
   */
  def collect(path: String, os: Option[Os]): List[Source] =
    if isDirectory(path) then walk(path, Nil, sysl, os, None)
    else List(Source(path, readFile(path), Nil))

  /** The selection a command that **renders** a tree makes: every per-OS directory, because there is
   * no target to choose one with and no compilation for the extra files to collide in.
   *
   * `weave` and `tangle` are the callers. Both are deliberately above target selection — a package's
   * prose is worth reading on a machine that could not build it — and both keep working here for the
   * same reason: `weave` renders a document, where two implementations of one function are two things
   * worth reading, and `tangle` writes the tree back out with its shape intact, `__<os>__` directories
   * and all.
   */
  val Every: Option[Os] = None

  /** The two suffixes a program may be written under: the ordinary one, and the literate one whose
   * program is the indented part of a Markdown document (`Literate`). A directory may hold both, and
   * which a file is decides nothing beyond how its text is read — the module it belongs to is where
   * it sits, as it is for every other file.
   */
  private val sysl: List[String] = List(".sysl", Literate.Extension)

  /** The one directory name a project root gives up: `examples/`, which holds **programs** rather
   * than part of the tree they are written against (`reference/packages.md § A package may carry
   * examples`).
   *
   * Everything under a project root compiles *into* it, which is what made a package unable to carry
   * a demo at all: `build-lib` refused `examples/demo.sysl` because a file with no `module` header
   * is the anonymous root module wherever it sits, and a library may not have one. So every binding
   * in the org kept its example as a fenced block in a README that nothing compiles — an example
   * that rots, which is what card `0194 h` was filed about.
   *
   * **The exclusion is at the root and nowhere else**, so `sh/sysl/thing/examples/` is an ordinary
   * module named `sh.sysl.thing.examples`. One name, one place, and a package that wants the word
   * for a module still has it.
   *
   * It is unconditional rather than asked of the manifest: a rule the *format* states is one a
   * reader can apply by looking at the tree, and making it depend on whether a `package.hocon`
   * happens to be there would mean the same directory is source in one project and not in another.
   */
  val ExamplesDir = "examples"

  /** A project's own copy of what it depends on, which `sysl vendor` fills and a build reads instead
   * of the machine's cache.
   *
   * **It is skipped for exactly `examples`' reason, one line below**: it sits at the root and it is
   * full of `.sysl`, so a walk that took it would compile every dependency *as this project's own
   * modules* — which fails on the first file, saying that `sh.sysl.json` sits in
   * `vendor.github.com.sysl-lang.json.@v0.1.2.sh.sysl.json`. A dependency is reached as a package,
   * through the resolver, and never as a directory that happens to be here.
   */
  val VendorDir = "vendor"

  /** The C files of a source tree, which a `.sysl` file reaches by `extern` and the build compiles
   * alongside it (`reference/ffi.md § A library may carry C`).
   *
   * The same walk as `collect` and deliberately so: a C file belongs to the directory it was found
   * in exactly as a sysl file does, which is what lets its object be named after a path that is
   * unique across the tree.
   *
   * **C belongs to a module or to the tree's own root, so a directory that is neither contributes
   * none of its own.** The walk still descends through such a directory, since a module may sit any
   * depth below one — what it does not do is take the C sitting *in* it. That is
   * `getting-started/cli.md § The subcommands` step 1 read for the other half of the walk: a
   * directory containing sources is a module, and `modules` below already applies the same rule to
   * the sysl. The two disagreeing is how a project came to compile C nobody wrote for it — `cmake
   * -B build` puts a build directory *inside* the project, and a Zephyr build fills it with
   * generated C meant for a different compiler.
   *
   * The cost, which is accepted rather than fixed: a vendored C library laid out in sub-directories
   * of its own loses the ones holding no sysl, and the signal is a link error naming the symbols.
   * Every binding in the org puts its C flat beside the module that declares it, so the rule is the
   * house pattern rather than a new constraint on it; a package needing the nested form makes the
   * directory a module by putting the `.sysl` that declares those `extern`s in it.
   *
   * It is a *separate* call rather than a second list out of one walk because the two answers are
   * wanted at different moments: the sysl decides whether there is anything to compile at all, and
   * the C is not looked at until a compilation that got that far is about to link. Which trees are
   * asked is `NativeSources`' — every tree the compilation walked, not only a library's.
   *
   * Naming a single file is not offered, and gets `Nil` rather than an error. Naming a file
   * compiles that file alone (`reference/modules.md`), so there is no tree for C to have travelled
   * with — and a lone C file is not a program.
   *
   * **The per-OS directories of `reference/modules.md § Platform selection` matter most here**,
   * because C is what they exist for: a module binds one system's header in a `.c` under
   * `__linux__/` and another's under `__macos__/`, and neither file is compiled — or read — on a
   * target it was not written for.
   */
  def cSources(path: String, os: Option[Os]): List[Source] =
    if isDirectory(path) then walkModules(path, Nil, List(".c"), os, None) else Nil

  /** The directories `cSources` walked past that a reader is likely to have meant it to take: ones
   * holding C and no sysl, so they are not modules and their `.c` files are compiled by nothing.
   *
   * **This exists because the skip is silent, and that silence cost an investigation.** The rule is
   * the one `walkModules` states and is deliberate — a directory the sysl walk claimed nothing from
   * was never part of the tree, and taking C out of it would compile whatever a build directory or a
   * vendored library happened to leave there. What was missing is any word of it: a `c/` holding a
   * bare `side.c` produced no `compile:` line, no note, and then a link error naming a symbol, which
   * reads as a defect in the compiler rather than as a file it never looked at. Card `0323` was
   * filed against `sysl build` on exactly that reading, and the premise was false — the root module
   * supplies C like any other, once the directory is a module.
   *
   * **Only a directory holding `.c` is reported**, which is what keeps this from being noise: an
   * empty directory, a `docs/`, a `.git` are all walked past too and none of them was meant to be
   * compiled. A `.c` sitting somewhere nothing will read is a file somebody wrote for this build.
   *
   * The answer is the directories themselves, so the caller decides what to say and about which
   * trees — a dependency's stray `.c` is its author's to fix and not something the consumer can act
   * on.
   */
  def skippedC(path: String, os: Option[Os]): List[String] =
    if isDirectory(path) then skippedUnder(path, Nil, os, None) else Nil

  /** The modules a tree offers to something outside it: the shallowest directories under `root`
   * that hold source, as dotted paths (`reference/modules.md`).
   *
   * **A directory holding no source is not a module**, which is the same rule `walk` applies and is
   * why this lives here rather than beside its caller. A package namespaced by reverse DNS puts its
   * source at `sh/sysl/table/`, so `sh` and `sh/sysl` hold nothing and the module it offers is
   * `sh.sysl.table` — one name, three segments. Reading the top-level *directories* instead would
   * answer `sh` for that package and for every other one namespaced the same way, which is not a
   * name any of them declares and is the same name for all of them.
   *
   * **Shallowest and not every module**, because what a binding has to cover is a name and everything
   * under it: a consumer writing `sh.sysl.table.sub` is answered by the entry for `sh.sysl.table`,
   * and an entry of its own would say the same thing twice. It also keeps the guarantee the collision
   * check rests on — no path here is inside another, so at most one can answer any written path.
   *
   * A file sitting directly in `root` belongs to the anonymous root module, which has no name to
   * import and so is not offered.
   */
  /** **Every per-OS directory is taken here, whatever machine is asking.** What a package *offers* is
   * a property of the package rather than of the build consuming it, so a name that came and went
   * with the target would make a dependency's mount resolve on one machine and not on another — and
   * the collision check below it would be checking a different table each time. Taking every one is
   * also the safe direction for that check: it can only find more collisions, never fewer.
   */
  def modules(root: String): Set[String] = {
    def under(path: String, dir: List[String], within: Option[String]): List[String] = {
      // A directory that will not list is nothing to offer and never was — a dependency's root may
      // be anything on disk. A *malformed* selector is a different thing entirely and travels: it is
      // a mistake somebody made in the tree, and a package that silently offered nothing because of
      // one would be the exact failure `selected` exists to refuse.
      val (files, subs) =
        try contents(path, Every, within)
        catch
          case e: SelectionError => throw e
          case _: Exception      => (Nil, Nil)

      if dir.nonEmpty && files.exists(f => sysl.exists(f.endsWith)) then List(dir.mkString("."))
      else
        outside(dir, subs).filterNot((d, _) => basename(d).startsWith("."))
          .flatMap((d, w) => under(d, dir :+ basename(d), w))
    }

    under(root, Nil, None).toSet
  }

  /** One directory of the project: its own files of the wanted kinds, then the sub-directories under
   * it. A directory holding no source is not a module and contributes nothing; it is still walked,
   * since modules further down are reached through it.
   */
  private def walk(path: String, dir: List[String], exts: List[String], os: Option[Os],
                   within: Option[String]): List[Source] = {
    val (files, subs) = contents(path, os, within)
    val here          = files.filter(f => exts.exists(f.endsWith)).map(f => Source(f, readFile(f), dir))

    here ::: outside(dir, subs).flatMap((sub, w) => walk(sub, dir :+ basename(sub), exts, os, w))
  }

  /** A directory's sub-directories with the root's `examples/` left out. The `dir.isEmpty` test is
   * what makes it the *root's* one: everywhere else the name is ordinary.
   */
  private def outside(dir: List[String], subs: List[(String, Option[String])])
      : List[(String, Option[String])] =
    if dir.nonEmpty then subs
    else subs.filterNot((d, _) => basename(d) == ExamplesDir || basename(d) == VendorDir)

  /** `walk`, taking a directory's own files only where that directory is a **module** — where it
   * holds a sysl file of its own — or is the tree's own root. Sub-directories are still descended
   * into, because a module may sit any depth below a directory that holds nothing.
   *
   * This is what `cSources` wants and `collect` does not: the sysl walk finds the modules, so a
   * directory it takes nothing from has by definition contributed nothing, while the C walk would
   * otherwise take files out of a directory the project never claimed.
   *
   * **The root is exempt because the root is the tree rather than a directory in it**, which is what
   * `LibraryBuildCliTests` and `PackageBuildTests` mean by *"as well as beside a module"*: a package
   * namespaced by reverse DNS has no sysl at its root, and the C belonging to no single module goes
   * there. `dir` is empty at exactly one place and that is the place.
   */
  private def walkModules(path: String, dir: List[String], exts: List[String], os: Option[Os],
                          within: Option[String]): List[Source] = {
    val (files, subs) = contents(path, os, within)
    val mine          = dir.isEmpty || files.exists(f => sysl.exists(f.endsWith))
    val here          = if mine then files.filter(f => exts.exists(f.endsWith)).map(f => Source(f, readFile(f), dir))
                        else Nil

    here ::: outside(dir, subs).flatMap((sub, w) => walkModules(sub, dir :+ basename(sub), exts, os, w))
  }

  /** `walkModules`' own descent, answering with the directories it takes **nothing** from that hold
   * C — see `skippedC`. It is the same walk deliberately: a rule stated twice is a rule that will
   * disagree with itself, and what this reports has to be exactly what the other one skipped.
   */
  private def skippedUnder(path: String, dir: List[String], os: Option[Os],
                           within: Option[String]): List[String] = {
    val (files, subs) = contents(path, os, within)
    val mine          = dir.isEmpty || files.exists(f => sysl.exists(f.endsWith))
    val here          = if !mine && files.exists(_.endsWith(".c")) then List(path) else Nil

    here ::: outside(dir, subs).flatMap((sub, w) => skippedUnder(sub, dir :+ basename(sub), os, w))
  }

  /** One directory's contents with per-OS selection already applied: the files that belong to it, and
   * the sub-directories below it paired with whichever `__<os>__` directory each is inside.
   *
   * **This is the whole of the mechanism, and it is one function on purpose.** All three walks
   * above split a listing into files and sub-directories and then apply a rule of their own; giving
   * them a listing that has already had the selection folded into it means every one of those rules
   * — the shallowest-module rule, `walkModules`' *is this a module*, the `dir` segments a `Source`
   * carries — goes on being written once and goes on being true. A directory's files are its own
   * plus the selected `__<os>__` child's, and its sub-directories are its own plus that child's:
   * the folder disappears, which is exactly what `reference/modules.md § Platform selection` says
   * it does.
   *
   * The pairing is what carries the nesting refusal down. A sub-directory found inside a selected
   * folder is under it however deep it goes, so `__linux__/fs/__macos__/` is refused for the same
   * reason `__linux__/__macos__/` is, rather than quietly selecting nothing forever.
   */
  private def contents(path: String, os: Option[Os], within: Option[String])
      : (List[String], List[(String, Option[String])]) = {
    val entries            = listFiles(path).toList.sorted
    val (dirs, files)      = entries.partition(isDirectory)
    val (selectors, plain) = dirs.partition(d => marked(basename(d)))

    // Every selector is validated, not only the ones this target takes — a misspelling in the Linux
    // half is a mistake a macOS build should report, exactly as `Conditional` checks the conditions
    // on branches it is not taking. What is *not* looked at is the inside of a folder this target
    // did not select, which `reference/modules.md § Platform selection` states outright: an
    // unselected tree is never read.
    val taken = selectors.filter { d =>
      val named = selects(basename(d), within)

      os.forall(o => named.exists(Conditional.osDefined(o)))
    }

    val (nestedFiles, nestedSubs) = taken.map(d => contents(d, os, Some(basename(d)))).unzip

    // **Two selectors that both answer, each holding a file of the same name.** It could not happen
    // while a selector named exactly one operating system — no machine is two of those — and it can
    // now that one may name a family: `__posix__` and `__macos__` are both true on macOS. What it
    // produces is two files of one name, which is a duplicate symbol at the link or two declarations
    // of one function, reported a long way from the directories that caused it.
    //
    // **The name colliding is the fault, not the two folders answering.** `__hosted__` and
    // `__posix__` are both true of every POSIX machine and are a perfectly good pair while what they
    // hold is different — one is *needs an operating system*, the other *needs POSIX*, and a module
    // may want to say both. Refusing every overlap would have forbidden that to catch a collision
    // this catches exactly.
    //
    // Asked only where a machine was named. With none there is no such thing as answering for it, and
    // taking every selector is what `Project.modules` is doing on purpose.
    if os.isDefined && taken.length > 1 then
      val byName = nestedFiles.flatten.groupBy(basename).filter(_._2.length > 1)

      for (name, paths) <- byName.toList.sortBy(_._1) do
        throw SelectionError(s"'$name' is in more than one directory that selects source for this " +
          s"machine — ${paths.map(p => s"'${basename(parentOf(p).getOrElse(p))}'").sorted.mkString(" and ")}. " +
          "A selector may name a family, so two of them can answer at once and both files would be " +
          "taken. Give them different names, or name the machines so that at most one matches")

    (files ::: nestedFiles.flatten,
     plain.map(_ -> within) ::: nestedSubs.flatten)
  }

  /** Whether a directory's name has the shape that says *this selects rather than names*.
   *
   * The shape is matched before the spelling so that a name which was **meant** to select and is
   * misspelled is an error rather than an ordinary directory — a `__linx__` read as a module would
   * compile nothing on any target and be reported, eventually, as a missing function.
   */
  private def marked(name: String): Boolean =
    name.length > 4 && name.startsWith("__") && name.endsWith("__")

  /** Which machines a selector directory names, or the mistake it is.
   *
   * **A selector names one or more symbols, separated by commas, and is taken when ANY of them holds
   * for the target** — so `__macos__` selects one operating system, `__macos,linux__` either of two,
   * and `__posix__` whichever of them POSIX means. One rule covers all three, and it is `#if`'s rule:
   * the vocabulary is `Conditional.directorySymbols` and the test is `Conditional.osDefined`, so a
   * new operating system reaches this by existing and the two places a source file can name a
   * machine cannot come to disagree.
   *
   * **Prefer the name that says why over the list that says which.** `__posix__` and
   * `__macos,linux__` select the same two machines today and are not the same claim: which operating
   * systems are POSIX is written in exactly one place (`Os.inherentCapabilities`), so the first
   * derives from it and the second copies it. Add a third POSIX system and every `__posix__` folder
   * covers it untouched while every `__macos,linux__` folder silently does not. The list earns its
   * keep on a set no capability names — macOS and Windows but not Linux — which is a reason to have
   * the form rather than to reach for it.
   *
   * A **processor** is not nameable here and that is the one deliberate hole: this walk has an
   * operating system and nothing else to ask. Source that varies by processor is `#if`'s, or the C
   * preprocessor's inside a `.c`.
   */
  private def selects(name: String, within: Option[String]): List[String] = {
    for outer <- within do
      throw SelectionError(s"'$name' sits inside '$outer', and both select source for a machine — " +
        "an unselected directory is never read, so nothing inside one could ever be taken. Write " +
        "them beside each other, or use '#if' inside a sysl file or the C preprocessor inside a " +
        "'.c'")

    val written = name.stripPrefix("__").stripSuffix("__").split(",", -1).toList.map(_.trim)

    // Each element is checked, not just the first bad one's folder, so a reader fixing a four-way
    // selector is told which quarter of it is wrong rather than being handed the whole name back.
    for symbol <- written do
      if !Conditional.directorySymbols(symbol) then
        throw SelectionError(s"'$symbol', in the directory '$name', names no machine this compiler " +
          "selects source for. A selector names operating systems and the two facts that hold " +
          s"without naming one, separated by commas: ${Conditional.directorySymbols.toList.sorted.mkString(", ")}. " +
          "A processor is not among them — that is what '#if' is for")

    written
  }

  /** What a per-OS directory is called for a given operating system. */
  private[sysl] def spelling(os: Os): String = s"__${Conditional.osSymbol(os)}__"

  /** The last segment of a path, whichever separator the platform wrote it with. */
  def basename(path: String): String = {
    val slash = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))

    if slash >= 0 then path.substring(slash + 1) else path
  }

  /** What a project rooted at this path is *called*, which is not the same question as what the
   * caller typed to reach it.
   *
   * `.`, `..`, a trailing separator and a path that doubles back through `..` all name a directory
   * whose name is somewhere other than the end of the string, so the segments are resolved against
   * the working directory and the last one that survives is the answer. `basename` cannot do this
   * and should not try: it answers about the *text*, which is what its other callers want.
   *
   * A path that resolves to the root has no last segment and so has no name; `a.out` is the same
   * fallback the driver has always used for a name it could not work out.
   */
  def nameOf(path: String): String = segmentsOf(path).lastOption.getOrElse("a.out")

  /** The same path written from the root, with `.` and `..` resolved away.
   *
   * **It exists because a path that is only ever *read* and one that is *written down* want
   * different things**, and the difference is invisible until the second kind appears. A `-I` or a
   * `-L` is consumed by the command it is on, so the working directory it is relative to is the one
   * that is current — nothing can go wrong. An **rpath** is consumed by the dynamic loader, later,
   * in whatever directory the program is eventually run from, so a relative one names a place
   * nobody meant. `--link-path ../build` would bake `../build` into the executable and resolve it
   * against the *user's* cwd at launch.
   *
   * Purely textual, deliberately: no `realpath`, so a symlink is left as the caller wrote it. What
   * this is for is making a path independent of *when* it is read, and resolving symlinks would
   * additionally make it independent of what the filesystem does later — a different promise, and
   * one a build has no business making on somebody's behalf.
   *
   * `base` is where a relative `path` is read from — the process's working directory by default,
   * which is right for anything typed on a command line, but wrong for a path a *manifest* wrote,
   * which means wherever that manifest sits rather than wherever the build happens to be invoked from
   * (`PackageConfig.resolvingLocalPaths`). `base` may itself be relative — a project root is not
   * always given as an absolute path — so it is grounded in the working directory first, the same
   * way a bare `path` would be. Because the answer always starts with `/`, resolving an
   * already-absolute result again — against a different `base` — is a no-op rather than a second
   * join, which is what makes it safe to call more than once on the same dependency.
   */
  def absolute(path: String, base: String = getCurrentDirectory): String =
    s"/${segmentsOf(path, base).mkString("/")}"

  private def segmentsOf(path: String, base: String = getCurrentDirectory): List[String] = {
    val rootedBase = if base.startsWith("/") || base.startsWith("\\") then base
                     else s"$getCurrentDirectory/$base"
    val rooted      = if path.startsWith("/") || path.startsWith("\\") then path
                       else s"$rootedBase/$path"

    rooted.split("[/\\\\]").foldLeft(List.empty[String]) {
      case (segments, "" | ".") => segments
      case (segments, "..")     => segments.dropRight(1)
      case (segments, segment)  => segments :+ segment
    }
  }

  /** The directory an output path sits in, where it names one — the default standard-module path
   * does, and it is a directory a fresh clone has never had, so writing the artifact has to make it.
   */
  def parentOf(path: String): Option[String] = {
    val slash = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))

    Option.when(slash > 0)(path.substring(0, slash))
  }

  /** Makes one directory, tolerating one that something else has just made.
   *
   * **`createDirectory` refuses a path that is already there, and no caller here wants that
   * refusal**: they want the directory to exist, not to have been the one that made it. So the test
   * after the failure is exactly that test — a failure that leaves no directory behind is still
   * raised, carrying the filesystem's own message, which is what keeps an unwritable path from
   * reaching the linker as somewhere to put an executable.
   *
   * The refusal is not one exception but two, and the second is the one that matters: a directory
   * that exists and is *empty* is refused differently from one that exists and has been written
   * into. A tolerance written against the first alone passes its own test and still fails in the
   * field, because the field is `makeDirectories` below, where the directory already there is one
   * another compilation has already put its artifact in.
   */
  def makeDirectory(dir: String): Unit =
    try createDirectory(dir)
    catch case e: IOException => if !isDirectory(dir) then throw e

  /** Makes a directory and everything above it, tolerating a compilation racing for the same one.
   *
   * **`createDirectories` decides what is missing before it makes any of it, so it is not safe to
   * call twice at once** — and every caller of this is writing somewhere shared, which is to say
   * somewhere two invocations can be pointed at once. Two compilations starting against a cold
   * artifact cache both find the cache directory missing, both walk up to make it, and the one that
   * loses calls `createDirectory` on a directory the winner has made *and already written the
   * artifact into*.
   *
   * What that produces is a complaint that a directory is not empty — which it is not, precisely
   * because the other compilation got there first. Nothing about it reads as a race, and it lands as
   * a failed build on whichever invocation was second. The same window is open on a fetched
   * package's directory and on any output directory two builds are pointed at, which is an ordinary
   * parallel make.
   *
   * **The walk is written out rather than delegated because the tolerance has to be at the leaf.**
   * The window is between the check that says a directory is missing and the call that makes it, and
   * only the call that loses it is in a position to know that losing it is not an error.
   */
  def makeDirectories(dir: String): Unit =
    if !isDirectory(dir) then
      parentOf(dir).foreach(makeDirectories)
      makeDirectory(dir)

  /** Removes a temporary file, whether or not it is there.
   *
   * Cleanup runs on the paths that failed, and those are exactly the paths where the file may never
   * have been created: `createTempFile` reserves a name, and it is the toolchain that writes to it. A
   * `deleteFile` on a link that failed therefore threw, and the stack trace replaced the linker's own
   * message — the one thing the user needed to see.
   */
  def discard(path: String): Unit =
    try deleteFile(path)
    catch case _: Exception => ()
}
