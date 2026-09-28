package sh.sysl

import io.github.edadma.cross_platform.*

/** The library the compiler is installed with: the standard module `sysl` that every program is
 * compiled against, and the submodules beneath it (`library/_index.md`).
 *
 * **`sysl` is the auto-imported part and not the whole of it.** `library/sysl` is a tree, so a
 * directory under it is a submodule by `reference/modules.md`'s ordinary rule, and only the
 * standard module's names are the ones every file gets for free. That is what a submodule is for:
 * what a program cannot avoid needing goes in `sysl`, and what it should have to ask for goes
 * below.
 *
 * What a program starts with is a *module*, not a set of declarations threaded in beside it. It was
 * the latter once: a `Prelude` of sysl source held in a string inside the compiler, keyed under the
 * anonymous root module. Its declarations were drained into this one a surface at a time, both
 * mechanisms live throughout, so that there was no commit at which neither worked. A switch would
 * have put every unqualified name in every program onto a path nothing had ever exercised, where a
 * single hole fails every test at once with nothing to bisect; a move put **one** declaration onto
 * that path, so what broke named what was wrong.
 *
 * **The source is real files, under `library/sysl`, and the compiler reads them off disk.** That is what
 * the string inside the compiler could never be: a literal has no other form, while these are files
 * a driver reads exactly as it reads a user's library — which is what `sysl build-lib` is pointed
 * at, and what makes the library's own source something a reader can open, edit, and rebuild
 * against.
 *
 * They were carried *inside* the binary once, generated into a `StdSource` object by `build.sbt`.
 * That bought a guarantee — a compilation could not fail to find its library — and it was bootstrap
 * scaffolding rather than the design: a library nobody can open is not one anybody can learn from,
 * and a compiler that cannot be pointed at an edited one cannot have its library worked on at all.
 * What replaces the guarantee is `root` below, and above all the diagnostic it fails with, which
 * names every place it looked.
 *
 * **Every real toolchain resolves this way.** `rustc` computes a sysroot from its own location,
 * `clang` finds its resource directory from its own, `zig` its `library/`. None of them carries its
 * standard library in the executable, and none of them asks the user to set a variable.
 *
 * **What a given compilation was handed is a different question**, and it is `Stdlib` that answers it:
 * a program may be compiled against a `.syslib` whose trees never went through the parser, and every
 * question about which declarations are the library's has to be answered over the one it actually
 * got. This file is only the copy that is always available.
 *
 * **It is more than one file, and that is load-bearing rather than tidiness.** `Display.display`
 * names `Writer`, which is declared in the other one: a module's members are one set however many
 * files they came from (`reference/modules.md § The module graph is acyclic`), so neither file
 * imports the other and the order they are read in decides nothing. A library that could only ever
 * be one file would not be a library.
 */
object Std {

  /** The installed library cannot be made into a standard module: its directory holds no source,
   * a file of it does not parse, or a `c const` of it cannot be measured.
   *
   * **It is raised where the library is read and caught in `Stdlib.resolve`, which answers it as a
   * refusal like any other.** The readers sit under memo tables and behind signatures that answer a
   * `Stdlib` rather than an `Either`, and every road a compiling command takes to them goes through
   * `resolve` — so one catch there turns what used to reach the user as a stack trace into
   * `sysl: error:` and an exit status of 1. A caller embedding the compiler that asks
   * `Stdlib.fromSource` directly gets this exception, and its message is the same sentence.
   */
  final class Unusable(message: String) extends Exception(message)

  /** The **auto-imported** module, and the root of the library's tree: every other module the
   * library carries is below it. A constant so that nothing has to parse to ask which module the
   * free names are in; which modules there are in total is `Library.modules`, read off the headers.
   */
  val module: String = "sysl"

  /** Where the library's source is, or why it could not be found — resolved once, and the one thing
   * about the installation the rest of the compiler asks.
   *
   * The order is the order the answers are *trusted* in. An explicit `SYSL_LIB` is the user
   * overriding everything and is taken at its word; the compiler's own location is the installed
   * case and is what makes an install self-contained; the working directory is the development one,
   * where the compiler is being run out of a checkout.
   *
   * `SYSL_LIB` is an escape hatch and not the mechanism. A toolchain that needed a variable set
   * before it would work would be one nobody could install, so nothing on the ordinary path reads
   * it — it is here for a broken install and for working on the library itself, and it is the only
   * one of the three that reports its own failure rather than falling through, because someone who
   * set it is owed the truth about what they set.
   */
  lazy val root: Either[String, String] = rootOf(envVar("SYSL_LIB"), executablePath, isDirectory)

  /** The resolution itself, over its three inputs rather than over the machine it is running on —
   * the variable, this compiler's own location, and what counts as a directory.
   *
   * **Taking them as parameters is what makes every branch testable.** Read ambiently, the only case
   * a suite could reach is whichever one happens to hold on the machine it is running on, and the
   * others would be asserted by reading. That is the mistake `cross_platform.cacheRoot` was written
   * to undo, where the one test for a named root was cancelled on every run.
   */
  private[sysl] def rootOf(named: Option[String], exe: Option[String],
                           isDir: String => Boolean): Either[String, String] = {
    // A root is one that **holds the standard module**, not merely one that exists. Both spellings
    // are ordinary directory names — a C project has a `lib`, and so does half of everything else —
    // so an installed compiler standing in someone's source tree would otherwise take theirs for its
    // own and fail somewhere far from the cause. Asking for `<root>/sysl` is one `isDirectory` and
    // it makes the search skip what it should skip.
    def holdsLibrary(root: String): Boolean = isDir(s"$root/$module")

    named match
      case Some(path) =>
        Either.cond(holdsLibrary(path), path,
          s"SYSL_LIB names '$path', which does not hold a '$module' directory — it should be the " +
            s"library root, which is the directory *above* '$module' rather than '$module' itself")
      case None => candidates(exe).map(_._1).find(holdsLibrary).toRight(missing(exe))
  }

  /** The places a library root is looked for, in order, each with the reason it is a candidate — the
   * reasons being what the diagnostic below is made of.
   *
   * The installed answer is `<prefix>/share/sysl/library`, reached from the binary's own resolved
   * path by going up out of `bin`. That is a plain Unix prefix layout and is exactly what Homebrew's
   * `pkgshare` is, so an installed sysl finds its library with nothing configured and two installs
   * of different versions each find their own.
   *
   * The relative ones are the development tree. `../` and `../../` are there because the compiler's
   * own tests do not all run from the repository root, and because a developer running the driver
   * from a subdirectory of a checkout is the same case.
   *
   * **`lib` is tried after `library` everywhere, and it is not deprecation politeness.** The
   * directory was called `lib` until the name was changed for being one a reader takes for a build
   * output; what did not change is that copies of it are on disk in other repositories and inside
   * every compiler already installed. `sysl.sh` unpacks one from a release tarball and points
   * `SYSL_LIB` at it; a checkout of any age has one in the tree. Dropping the old spelling would
   * have made a rename in one repository an outage in eleven, so both are searched and the new one
   * wins wherever a tree has been moved over.
   */
  private[sysl] def candidates(exe: Option[String]): List[(String, String)] = {
    val prefixes = exe.flatMap(path => Project.parentOf(path).flatMap(Project.parentOf)).toList

    Names.flatMap(name => prefixes.map(prefix => s"$prefix/share/sysl/$name" -> "beside this compiler")) :::
      Names.flatMap(name => List(name, s"../$name", s"../../$name"))
        .map(_ -> "from the working directory")
  }

  /** What the directory holding the library is called, in the order a search trusts them. */
  private val Names: List[String] = List("library", "lib")

  /** What a compilation with no library to compile against says.
   *
   * **This is what replaces a guarantee**, so it is written as the thing a reader can act on rather
   * than as a report that something failed: every path that was tried, in the order they were tried,
   * with what each one was — followed by the two ways out. A compiler that cannot find its standard
   * library is broken, and the difference between a broken installation and a mystery is this list.
   */
  private def missing(exe: Option[String]): String =
    s"cannot find the standard module's source, which every program is compiled against. Looked " +
      s"for a library root holding '$module' at:\n" +
      candidates(exe).map((path, why) => s"  $path ($why)").mkString("\n") +
      "\n\nA sysl installed from a package has it beside the binary; one run out of a checkout " +
      "finds it in the tree. Set SYSL_LIB to the library root to name it outright."

  /** The library's files, sorted by their place in it rather than by where they were found — so that
   * what a compilation sees depends on the library and not on the order a directory listed, or on
   * which of the paths above the root was reached by.
   *
   * Each carries the directory it sits in below the root, which is the module its header has to
   * agree with (`reference/modules.md`). They are read by the same walk that reads a user's
   * library, because they *are* a library: `sysl build-lib library --std` is pointed at this same
   * tree and gets these same values.
   *
   * **A function of the operating system rather than a `lazy val`, because a library is a tree and
   * a tree is a per-target answer** (`reference/modules.md § Platform selection`). The library
   * binds one system's `readdir` under `__linux__/` and another's under `__macos__/`, so what "the
   * library's files" are is a question with a machine in it — the same shape [[parsed]] already had
   * for the same reason one layer up.
   *
   * **Memoized, and that is a correctness requirement rather than a saving.** A `Source` compares by
   * **identity** (`Diagnostics`), and `Stdlib.owns` — which decides whether an unreached declaration
   * may be dropped — asks whether a tree's `Source` *is* one of the library's. Re-reading the files
   * makes new objects, so a second ask would answer about a different library that happens to hold
   * the same bytes. The `lazy val` this replaced gave that for nothing; a function has to say it.
   *
   * A map rather than a single slot, because there are four operating systems and a run asks about
   * one or two. What it holds is file *text*, which is a fraction of what [[parsed]]'s trees cost —
   * that one is bounded to a single target for a reason, and this one does not need to be.
   */
  def sources(os: Os): List[Source] = read.synchronized(read.getOrElseUpdate(os, collect(os)))

  private val read = collection.mutable.Map.empty[Os, List[Source]]

  private def collect(os: Os): List[Source] = root match
    case Right(dir) => collectAt(dir, os)
    case Left(err)  => throw Unusable(err)

  /** The library's files under `dir`, for one operating system — [[sources]] without the memo and
   * without the search, so a test can hand it a directory of its own.
   */
  private[sysl] def collectAt(dir: String, os: Os): List[Source] =
    val found = Project.collect(dir, Some(os)).sortBy(place)

    // A directory that answers the search and holds nothing readable is a *different* failure from
    // not finding one, and it has to say so: every program would otherwise fail at its first free
    // name — `undefined function 'print'` — which points at the program rather than at the empty
    // library that caused it. Reachable through a truncated install, or a `SYSL_LIB` naming a root
    // whose `sysl` directory is empty.
    if found.isEmpty then throw Unusable(s"the library at $dir holds no sysl source files")

    found.map(named)

  /** A library file under the name a **diagnostic** should call it: the library root's own name and
   * the file's place below it, never the path it was read from.
   *
   * **A message naming a library file has to read the same on every machine.** The library moves
   * with the installation — `/opt/homebrew/Cellar/sysl/0.0.3/share/sysl/library` on one machine,
   * `library` in a checkout, wherever `SYSL_LIB` says on a third — and a diagnostic that quoted that
   * path would be noise on the first, different on the second, and unquotable by the documentation
   * on all of them. Which is how this was found: two library pages quote
   * `library/sysl/thread/mutex.sysl` in a refusal about a private field, and the executable-docs
   * suite failed the moment the name became absolute.
   *
   * **The prefix is a constant and not the root's own basename**, which is the second half of the
   * same rule and was learned the harder way. Read off the directory, the name said `lib` in a
   * checkout, `lib` under an install — and `mine` for anyone who pointed `SYSL_LIB` at
   * `/tmp/mine`. That last one was always wrong and went unnoticed because nobody does it; what made
   * it matter is the directory being renamed, after which the *same* file was `library/sysl/print.sysl`
   * out of a moved tree and `library/sysl/print.sysl` out of a compiler installed the week before. A
   * diagnostic that a page quotes cannot depend on which of those the reader has.
   *
   * A program's own files keep the path they were given, and should: those a reader can open, and
   * the whole point of a diagnostic pointing at one is that they go and look.
   */
  private[sysl] def named(s: Source): Source =
    Source(s"$Prefix/${place(s)}", s.text, s.dir.getOrElse(Nil))

  /** What a library file is called in a diagnostic, whatever directory it was read out of. */
  private[sysl] val Prefix: String = "library"

  /** A file's place in the library: the module directories it sits under, then its own name. The
   * same key the fingerprint sorts and hashes by, and for the same reason — it is what is true of a
   * file wherever the library was found, while its path is not.
   */
  private def place(s: Source): String =
    (s.dir.getOrElse(Nil) :+ Project.basename(s.name)).mkString("/")

  /** What the library's source amounts to, for telling a prebuilt artifact built from *this* library
   * from one built from a different version of it.
   *
   * The artifact is built separately from the compiler that consumes it, and the two can therefore
   * fall out of step: build one, edit `library/sysl`, and every compilation after that would be against
   * a standard module that is not the one in the tree. Nothing else would notice — a stale artifact
   * decodes perfectly and links perfectly, it is just the wrong library. This is what an artifact is
   * held to on the way in ([[Stdlib.read]]).
   *
   * **Rebuilding the compiler has nothing to do with it, and that is what reading the library off
   * disk bought.** While the source was generated into the binary there were two copies and two
   * ways to be stale — an artifact behind the carried source, and carried source behind the tree —
   * and the second needed a test of its own to catch. Now an edit to `library/sysl` moves this the next
   * time the compiler runs, the artifact keyed by it is a different file, and the drift the second
   * copy made possible cannot occur.
   *
   * **The C is fingerprinted with the sysl and not apart from it**, which is what
   * [[LibraryArtifact.build]] does on the other side of the comparison — a shim is as much the
   * library's source as a module is, and an artifact that did not change when one was edited is a
   * stale artifact nothing would notice was stale. It cost nothing while the library carried no C
   * and would have been a silent mismatch the day it did.
   */
  def fingerprint(os: Os): String = LibraryArtifact.fingerprint(files(os))

  /** The same hash, over a **named** root rather than over the one this object resolved.
   *
   * `build-lib --std` compiles the tree it was pointed at, which need not be the tree a compilation
   * on this machine would resolve — [[candidates]] tries the installed library before the working
   * directory, so an installed sysl run inside a checkout resolves the installed one while being
   * handed the checkout's. Naming the artifact with [[fingerprint]] there put the bytes of one
   * library under the key of another, which is precisely what the key exists to prevent.
   *
   * **It agrees with [[fingerprint]] by construction where the root is the same**, and that is a
   * property of the hash rather than a thing to keep in step:
   * [[LibraryArtifact.fingerprint]] reduces each file to its `place` and its text and **sorts by
   * `place` itself**, so neither the order the files arrive in nor the renaming [[named]] applies
   * can reach it. Nothing here has to reproduce `collect`'s bookkeeping to get the same answer.
   *
   * Read off the **directory** rather than off whatever a caller had already collected: `build-lib`
   * strips a library's `@tests` files before analysis, and the standard module's fingerprint is over
   * its files including them.
   */
  def fingerprintOf(dir: String, os: Os): String =
    LibraryArtifact.fingerprint(Project.collect(dir, Some(os)) ::: Project.cSources(dir, Some(os)))

  /** Every file the library is made of, which is what the fingerprint is over: its sysl and its C.
   *
   * **It has a name of its own so that nobody has to remember the `:::`.** This fingerprint is
   * compared against one computed somewhere else — inside [[LibraryArtifact.build]], from whatever
   * that call was handed — and the only way the two can disagree is by one side forgetting a kind of
   * file. A name makes that a thing to pass rather than a thing to assemble, and the tests that
   * assert properties of the hash say the same word the production path does.
   */
  def files(os: Os): List[Source] = sources(os) ::: cSources(os)

  /** The C the library carries, for the operating system being built for.
   *
   * **It is empty on a target whose `__<os>__` directory the library does not have, and that is the
   * feature rather than a gap.** A shim calling `readdir` cannot exist on a freestanding target and
   * must not be compiled there; a directory that selects is how it comes to be absent, without a
   * condition written anywhere and without the file having to compile.
   *
   * Read through `Project.cSources`, which is the same walk every other tree's C is found by — the
   * library is a library (`reference/modules.md § Separate compilation`), and the one thing that
   * used to be true of it and of nothing else was that nobody ever looked here.
   */
  def cSources(os: Os): List[Source] = readC.synchronized(readC.getOrElseUpdate(os, root match
    case Right(dir) => Project.cSources(dir, Some(os)).sortBy(place)
    case Left(_)    => Nil))

  private val readC = collection.mutable.Map.empty[Os, List[Source]]

  /** The parsed standard module, **for a target** — and the trees of **one** target are kept, not
   * every target's.
   *
   * The library is sysl source like any other and may gate on the machine it is being built for
   * (`Conditional`), so which trees it comes to is a question with a target in it. Two targets may
   * therefore see two different standard modules — that is the point of the feature — and the answer
   * is memoized because this is on the path of every compilation that has no artifact to read
   * instead.
   *
   * **What is memoized is the LAST target asked for, and that bound is the whole point.** The
   * paragraph this replaces argued that the table should not be built from the registry up front
   * because *"a run compiles for one target and would pay for ten"* — which is true of a run, and
   * stopped being true of the **suite** the moment a test iterated `Target.all`. Filling it lazily
   * costs exactly what filling it eagerly costs, only later: `AbiAgainstClangTests` walks every
   * supported target, so it arrived at ten parsed standard modules held for the life of the process,
   * and a Scala Native test agent that had run it could not then run anything else.
   *
   * A single slot keeps every bit of the benefit for the access pattern the comment describes — a
   * loop over one target hits every time — and costs a re-parse only where the caller alternates,
   * which is the case that was never meant to be cached anyway.
   */
  def parsed(target: Target): List[Program] =
    cache.synchronized {
      cache.get(target) match
        case Some(programs) => programs
        case None =>
          val programs = parsedFrom(sources(target.os), target)

          cache.clear()
          cache(target) = programs
          programs
    }

  /** The library's files parsed for a target — [[parsed]] without the memo, so a test can hand it
   * files of its own. Every file is parsed and all of their diagnostics are reported together,
   * since two broken files are two things for whoever is repairing the library to find.
   */
  private[sysl] def parsedFrom(files: List[Source], target: Target): List[Program] =
    val results = files.map(SyslParser.checked(_, target))

    results.collect { case Left(found) => found }.flatten match
      case Nil   => results.collect { case Right(p) => p }
      case found => throw Unusable(unparsed(found))

  /** What a library that does not parse is reported as: one sentence saying whose fault it is, and
   * the parser's own diagnostics under it, located as they would be for any file.
   *
   * **The directory is named because the diagnostics cannot name it.** A library file is reported
   * as `library/sysl/…` whatever it was read from ([[named]] says why), which is right for a message
   * about the library and leaves no way to find the file when the library is not the one installed
   * — a `SYSL_LIB` pointed at an edited copy is exactly the case this refusal is for.
   *
   * **It stands on its own rather than inside `Stdlib.rebuildFailure`**, whose advice is to build
   * the artifact with `build-lib library --std` or to compile the source in with `--no-std-lib`. Both
   * parse this same library, so both fail the same way, and a sentence recommending them would send
   * the reader round a loop.
   */
  private def unparsed(found: List[Diagnostic]): String =
    val where = root.fold(_ => "", dir => s" at $dir")

    s"the standard module does not parse, so nothing can be compiled against it — the mistake is " +
      s"in the library$where, not in the program\n${Diagnostic.report(found)}"

  /** **Locked, and it is not decoration.** This was a `lazy val` before it took a target, and a
   * `lazy val` is initialized exactly once however many threads reach it. A bare mutable `Map` is
   * not: two compilations for two targets, running at once, can be inside it together and leave the
   * table itself broken — which is not a wrong answer but a corrupted one, and it would show up as
   * something unrelated much later. The suite compiles for several targets from several threads, so
   * this is a live case rather than a hypothetical one.
   */
  private val cache = collection.mutable.Map.empty[Target, List[Program]]

  /** How many targets' trees are held. Exists so a test can pin the bound the comment above claims —
   * a memory property has no other observable surface, and this one regressed a whole release.
   */
  private[sysl] def cachedTargets: Int = cache.synchronized(cache.size)

  /** Whether a second ask for one target answers from memory, decided **without letting go of the
   * lock between the two asks**.
   *
   * A test cannot settle this by calling `parsed` twice and comparing, because only one target's
   * trees are kept: another suite asking for a different target in between clears the slot, and the
   * second ask reparses through no fault of the memo. The suites run concurrently and several sweep
   * every target, so that is an ordinary interleaving rather than a rare one — it reproduces against
   * `CrossTargetBuildTests` and `TargetTests` every time.
   *
   * Holding the lock across both is what makes the question answerable at all. It asks the memo
   * exactly what it promises: that a caller staying on one target parses once.
   */
  private[sysl] def memoAnswersTwice(target: Target): Boolean =
    cache.synchronized { parsed(target) eq parsed(target) }

  /** What the standard module **declares**, which is not everything under `library/`.
   *
   * The library's own `@tests` files are scaffolding for `sysl test --std` and are dropped by every
   * other build, so a name only a test writes is not a name the standard module declares — and
   * anything asking this question wants the shipping surface. `Stdlib.fromSource` strips the same
   * way, so the two agree by construction rather than by coincidence.
   */
  def decls(target: Target): List[Stmt] = Tests.stripSource(parsed(target)).flatMap(_.body)
}
