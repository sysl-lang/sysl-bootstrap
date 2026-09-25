package sh.sysl

import io.github.edadma.cross_platform.*

/** Glue between the pure compiler and an installed LLVM toolchain: it writes the generated IR
 * to a temporary `.ll`, links it with `clang`, and runs the result. All filesystem and process
 * access goes through `cross_platform`, so this works on every backend (JVM/JS/Native).
 *
 * **Two external tools are required: a `clang`, and — to build a library — an `llvm-ar`.** The
 * second is not a preference for LLVM's archiver over the platform's. A `.syslib` is an `ar` archive
 * (`LibraryArtifact`) whose members are objects for the machine it was *built for*, which is not
 * necessarily the machine it was built on, and a platform archiver only understands its own format:
 * asked to archive an ELF object on macOS, the system `ar` exits 0, prints a `ranlib` warning, and
 * writes an archive with the member **missing**. A cross-built library would be silently empty.
 * `llvm-ar` indexes every format, so one archiver covers every target.
 */
/** Where on **this machine** the toolchain should look for things it was not told the location of:
 * libraries a `@link` directive named, and headers a carried `.c` includes.
 *
 * ==This is the host's question, and `reference/ffi.md § @link` answered the target's==
 *
 * `reference/ffi.md § @link` is emphatic that a directive names a library and never a flag, because
 * *what a name becomes on a command line* is a property of the machine being built **for** — `-lm`
 * is right on ELF and wrong on Darwin. That decision stands and nothing here touches it. What it
 * never addressed is *where the library sits on the machine being built* **on**, which is a
 * different question with a different owner: the target decides the name-to-flag mapping, the host
 * decides the search path.
 *
 * So this belongs to the driver and must not become an attribute. A path written into a module's
 * header would be one machine's directory layout hard-coded into portable source, which is the
 * exact failure `reference/ffi.md § @link` refuses `-l` spellings to avoid.
 *
 * ==Why it is a flag when clang already reads the environment==
 *
 * `LIBRARY_PATH` and `CPATH` work already and always did: sysl execs clang, clang inherits the
 * environment, and a developer who exports them gets a build that links. Measured, not assumed —
 * without them `ld: library 'probe' not found`, with them the program runs.
 *
 * That is not a reason to leave this out; it is the reason to put it in. **A build that works only
 * because of one developer's shell is a build nobody else can reproduce**, and it fails for the next
 * person with a diagnostic that names a library rather than the setting they are missing. The flag is
 * where that fact gets written down, in the project or the command, alongside every other thing a
 * build needs. The environment keeps working and is the right tool for *this machine, always*; the
 * flag is the right one for *this build, wherever it runs*.
 *
 * ==Both halves, because half is unusable==
 *
 * A binding to a library outside the default path needs its headers to compile and its archive to
 * link, and neither is reachable without a flag — verified both ways on this machine. Shipping only
 * `link` would leave every such binding exactly as unbuildable as before, failing one step earlier.
 *
 * Nothing is guessed at. `/opt/homebrew/lib` is the obvious candidate to add by default and is not
 * added, for `Toolchain.provided`'s reason: a compiler that ruled on where a platform keeps its
 * libraries would be wrong about a machine nobody here has, and the cost of being wrong is a link
 * that fails somewhere the author cannot reach.
 */
case class SearchPaths(link: List[String] = Nil, include: List[String] = Nil,
                       defines: List[String] = Nil,
                       probed: List[String] = Nil, probedLibs: List[String] = Nil,
                       carried: Map[String, List[String]] = Map.empty,
                       /** `--cc` — the clang to build with, instead of searching for one
                         * (`findClang`'s `named`).
                         *
                         * ==Why the compiler's NAME rides with the search paths==
                         *
                         * It is the same class of fact as the rest of this record: what *this build*
                         * needs from *this machine's* toolchain, written down in the command rather
                         * than picked up from the environment — which is the argument the header
                         * makes for `--link-path` and holds unchanged here.
                         *
                         * The practical half is that this record is already carried to every place
                         * that reaches clang. A parameter of its own would have to be added beside
                         * every existing `paths` parameter and then remembered at every call, and
                         * the flag that is honoured in some of those places and not others is
                         * exactly the half-measure card `0197` says to avoid.
                         *
                         * **The one path that carries no `SearchPaths` is the standard module's own
                         * rebuild**, which is why `Stdlib.writeArtifact` takes it as an explicit
                         * argument. A `--cc` that stopped applying the moment the library was
                         * rebuilt would fail later than the flag, and blame the library.
                         */
                       cc: Option[String] = None,
                       /** The libraries `link` asked for statically, each `-l` name mapped to the
                         * archive that replaces it on the link line (`StaticLink`, `LinkMode`).
                         *
                         * Here for `cc`'s reason: this record already reaches the one place a link
                         * line is written, and it is part of the run cache's key as it stands, so a
                         * binary linked one way is never handed back to a run that asked the other.
                         */
                       archives: Map[String, String] = Map.empty,
                       /** Every `pkg_config` module the build declared — the project's own, each
                         * fetched package's and each `--lib` root's — in the order they were asked.
                         *
                         * `probedLibs` holds what those modules *answered*, which is what a link
                         * this compiler runs needs; the names are what a link somebody else runs
                         * needs. A `build-c` archive is linked by a C project's own toolchain, and
                         * the one thing that project can be told that stays true on its machine is
                         * which modules to ask its `pkg-config` for. A library a package binds through
                         * its manifest alone, with no `@link` in its source, reaches the link line
                         * only through here, so it is the whole of why the list is kept.
                         */
                       modules: List[String] = Nil) {

  /** What the linker is told, as clang spells it — `--link-path`'s directories, then any a probe
   * answered with. Joined rather than passed as two arguments, which is how `-L` has been written
   * since cc and what a reader comparing this line against a hand-run clang expects to see.
   *
   * **A probe's `-L` belongs here rather than at the end, because `-L` is order-sensitive on GNU ld**
   * — it does not apply to a `-l` that appeared before it. A `@link("cairo")` directive puts `-lcairo`
   * on the line above the objects, so a probed `-L` arriving after it would leave that `-l`
   * unresolvable on any Linux where the library is outside the default prefix. macOS's `ld64` gathers
   * every `-L` before resolving and does not care, which is exactly why this cannot be caught here.
   */
  def linkFlags: List[String] = link.map(d => s"-L$d") ::: probedLibs.filter(_.startsWith("-L"))

  /** The rest of what a probe answered — the `-l` for the library itself, and the `-Wl,-rpath` that
   * decides whether a dynamically-linked program finds it at run time.
   *
   * This half goes at the **end** of the link line: a `-l` placed above the objects that need it is
   * discarded by any linker resolving an archive in one pass. The rpath's position is immaterial and
   * it travels with its own `-l` rather than being separated for no gain.
   */
  def probedLinkFlags: List[String] = probedLibs.filterNot(_.startsWith("-L"))

  def includeFlags: List[String] = include.map(d => s"-I$d") ::: probed

  /** The macros the C is compiled with, as clang spells them — `-DNAME` or `-DNAME=value`.
   *
   * **A header path is not enough to compile a header**, which is the whole reason this is here
   * beside `include`. A C project of any size configures its own headers with macros, and one that
   * has not been given them does not miss quietly: pico-sdk's `pico/cyw43_arch.h` answers a build
   * with no `CYW43_LWIP` by `#error`ing on the spot, saying it cannot tell which architecture
   * variant is meant. So a shim reachable at its path and compiled without the definitions its
   * project builds with is a shim that fails one step *after* the include path was fixed.
   *
   * Nothing is inferred and nothing is defaulted, for the reason `link` gives about `/opt/homebrew`:
   * what a project defines is the project's, and a compiler guessing at it would be wrong somewhere
   * nobody here can reach.
   */
  def defineFlags: List[String] = defines.map(d => s"-D$d")

  /** The macros one **carried** C file is compiled with: the build's own, then whatever the package
   * that carries the file declared for it in `defines` (`reference/packages.md § No build scripts,
   * ever`).
   *
   * ==Why the package's come second==
   *
   * A later `-D` wins, so a `--define` on the command line can be overridden by the package and not
   * the other way about. That is the right way round for the same reason the block exists at all:
   * the options are the author's decision and the consumer is not meant to be choosing them, so a
   * build that happens to define one of a package's own macros must not silently reconfigure it. A
   * consumer with a real reason to override is editing a vendored copy, which is a decision they
   * have made rather than one made for them.
   */
  def defineFlagsFor(source: String): List[String] =
    (defines ::: carried.getOrElse(source, Nil)).map(d => s"-D$d")

  /** The macros a `c const` block's probe translation unit is read under: the build's own, then the
   * union of what the carried C **in the same directory** is compiled with.
   *
   * ==A probe has no path of its own, and cannot be left out==
   *
   * `CProbe` synthesises the translation unit it measures, so there is nothing in the package for a
   * `defines` key to have named — and a probe left reading the package's headers under their
   * defaults while the object beside it was compiled under the options is not a small
   * inconsistency. Every option worth setting is one that changes a struct's size or deletes a
   * declaration: measured that way, miniz's `sizeof(tdefl_compressor)` comes back 319,352 where the
   * object holds 167,800, and the constant is wrong by 151,552 bytes with nothing in the build to
   * say so. It is the same defect `termbox2`'s `options.h` was written to prevent, arrived at from
   * the other direction.
   *
   * **The directory is the unit because that is how a binding is laid out** — the implementation,
   * the shim and the `c.sysl` in one `c/` directory, all reading one header. Two C files there
   * compiled with different macros give their union, which is a shape to avoid rather than to lean
   * on: a probe cannot be measured under two disagreeing configurations at once, and nothing here
   * can tell which of them its block meant.
   */
  def probeFlagsFor(unit: String): List[String] = {
    val dir = Project.parentOf(unit)

    val inherited = carried.toList.sortBy(_._1)
      .collect { case (path, macros) if Project.parentOf(path) == dir => macros }
      .flatten.distinct

    (defines ::: inherited).map(d => s"-D$d")
  }
}

object SearchPaths {

  /** Nothing but the toolchain's own defaults — every build that binds only what the platform ships,
   * which is every one in this repository.
   */
  val none: SearchPaths = SearchPaths()

  /** `--include-path` written as `<name>=<dir>`, which satisfies a package's declared header
   * requirement (`reference/packages.md § Capabilities`), or nothing where it is the ordinary bare
   * directory.
   *
   * ==Why a bare path cannot be mistaken for a named one==
   *
   * The two forms share a flag, so the split has to be decidable by looking rather than by guessing.
   * A name is what `PackageConfig.isPkgConfigName` allows — letters, digits, `_`, `-`, `.` and `+`,
   * starting with a letter — so a directory read as one would have to hold an `=`, be a single
   * segment with no separator before it, and have a non-empty remainder. Where the text before the
   * first `=` is not a name the whole string is a directory, which is what an absolute path, a
   * relative one and `.` all are.
   *
   * **The widest of the two requirement kinds, because the flag serves both.** A `headers` name is
   * the narrower `isHeaderName`, since its author chooses it; a `pkg_config` name may hold a dot or
   * a `+` because `pkg-config` chose it — `yaml-0.1`, `glib-2.0`, `gtk+-3.0`. A flag that read only
   * the narrow spelling would leave those requirements declarable and unanswerable, which is worse
   * than either rule alone: the refusal a build stops on tells the reader to type exactly this.
   *
   * **The name is not checked against any requirement here**, and a consumer satisfying one no
   * package declared is not an error: the directory reaches the C compiler exactly as the bare form
   * does. Refusing it would fail a build over a package's omission, and the flag would be the only
   * way to work around that omission.
   */
  def namedInclude(text: String): Option[(String, String)] =
    text.indexOf('=') match
      case -1 => None
      case at =>
        val name = text.take(at)
        val dir  = text.drop(at + 1)

        if dir.nonEmpty && PackageConfig.isPkgConfigName(name) then Some(name -> dir) else None
}

object Toolchain {

  /** Whether a `clang` capable of consuming textual LLVM IR is on the PATH. Tests that link
   * and run gate on this so they skip cleanly on a machine without a toolchain.
   */
  lazy val clangAvailable: Boolean =
    exec(Seq("clang", "--version")).exitCode == 0

  /** Whether a Why3 is on the PATH, for `sysl prove` (`reference/verification.md § sysl prove`).
   * The proof tests gate on it so they skip cleanly on a machine without one.
   *
   * Looked for by name only. Why3 installs through opam and has no Homebrew formula, so on a machine
   * that has one it is opam's switch that puts it on the PATH — which is the thing to say in the
   * diagnostic rather than a directory to go guessing at.
   */
  lazy val why3Available: Boolean = runs("why3")

  /** Which provers to try, in order, by the name Why3 files them under. Alt-Ergo first because it is
   * the one Why3 ships alongside; Z3 next, since Homebrew has a formula for it and opam does not.
   */
  private val provers = List("Alt-Ergo", "Z3", "CVC5", "CVC4")

  /** The prover to name on the command line, as `Name,Version` — which is the spelling Why3 wants
   * when a configuration holds several.
   *
   * **A bare name is not enough and the failure says so rather than falling back**: with three
   * Alt-Ergo entries configured (plain, bitvector, counterexample) `-P Alt-Ergo` is refused as
   * ambiguous. The alternatives are the parenthesized ones, and they are exactly what is skipped
   * here — the plain entry is the one that answers an ordinary goal.
   */
  private lazy val prover: Option[String] =
    if !why3Available then None
    else
      val listed =
        exec(Seq("why3", "config", "list-provers")).stdout.linesIterator
          .map(_.trim)
          .filter(l => l.nonEmpty && !l.contains('('))
          .toList

      provers.iterator
        .flatMap(p => listed.find(_.startsWith(p + " ")).map(_.replaceFirst(" ", ",")))
        .nextOption()

  /** Whether Why3 has a prover to discharge anything with. A machine may have Why3 and no prover at
   * all, which is a different failure from having neither and is worth telling apart.
   */
  lazy val why3HasProver: Boolean = prover.isDefined

  /** Runs Why3 over a WhyML module, answering what it said (`reference/verification.md § sysl
   * prove`).
   *
   * `prove` is the batch subcommand: it splits each verification condition into goals, runs the
   * configured provers on them, and reports one line per goal. The exit status is what says whether
   * everything was discharged, and the output is what says which goal was not.
   */
  def why3Prove(mlw: String, timeoutSeconds: Int = 5): Either[String, (Int, String)] =
    if !why3Available then
      Left("cannot find why3, which proving needs — it installs through opam ('opam install why3') " +
        "and has no Homebrew formula, so a shell that has not run 'eval $(opam env)' will not see " +
        "one that is installed. A prover is wanted too: 'opam install alt-ergo', or Homebrew's z3, " +
        "either of which why3 finds once 'why3 config detect' has run")
    else
      val file = createTempFile("sysl-", ".mlw")

      writeFile(file, mlw)

      // A prover has to be named for a time limit to mean anything, and naming one is what makes
      // this a *discharge* rather than a well-formedness check.
      val args = prover.toList.flatMap(p => List("--timelimit", timeoutSeconds.toString, "-P", p))
      val result = exec(List("why3", "prove") ::: args ::: List(file))

      deleteFile(file)
      Right((result.exitCode, result.stdout + result.stderr))

  /** What optimization a build asks for when nothing named one — the level, as clang spells it after
   * the `-O`.
   *
   * **Nothing was passed here at all until a program miscompiled.** `sysl.crypto`'s SHA-2, then a guide program, handed a 184-byte
   * struct to a function **by value** and the callee received zeros, from correct IR, on a build
   * that differed from a working one by a single `zext` feeding an `llvm.fshr`. `-O0` was where it
   * lived: the same module at `-O1` ran correctly, and the case reduced to a hand-edited `.ll` with
   * none of sysl's own code in the edit. The shape of it is a FastISel fallback losing a live value
   * — a path that exists only because `-O0` selects instructions the fast way and falls back to the
   * full selector for whatever it cannot handle.
   *
   * **So the default is not a workaround for that one bug.** It is the decision to stop building on
   * the least-exercised path in the back end. `-O0` is the mode a compiler's own suite covers least
   * and the one where instruction selection is a different algorithm; a language whose programs were
   * correct only at `-O0` would have the problem the wrong way round.
   *
   * `1` rather than `2` because what is wanted is the ordinary pipeline and not the aggressive one.
   * `-O1` is where the standard selector and the everyday simplifications are, which is the whole of
   * what this is for; it is also where a debugger still finds its footing and where compile time is
   * not something a program has to be big before noticing. `--optimize` names another.
   */
  val defaultOptimization = "1"

  /** The levels every clang has, which is what a **manifest** may name.
   *
   * ==Why a written-down set exists at all, when the flag needs none==
   *
   * `--optimize` passes whatever was written and lets clang rule on it, which is right for something
   * somebody is typing: the build they are watching stops in the same second, with the authority on
   * clang's levels saying so in its own words. `fast` and `g` are clang's too and a person reaching
   * for one gets it.
   *
   * A manifest is the other case. It is written once and read on every later build, by consumers who
   * did not write it and by `sysl run` replaying a cached binary — so the same mistake surfaces from
   * inside clang, at a remove, naming no file and no key. A manifest is refused when it is read
   * instead (`PackageConfig`), and the price of refusing early is that the accepted set has to be
   * stated here rather than deferred.
   *
   * **So it is the six levels every clang has**, and deliberately not `fast` (which clang has been
   * retiring) or `g` (which is about debugging rather than about what a release is built at). A
   * project that needs one of those names it on the command line, where clang answers for itself.
   */
  val levels = List("0", "1", "2", "3", "s", "z")

  /** How a build may ask for **link-time optimization**, which is what `--lto` and a manifest's
    * `lto` key take (`Pipeline`).
    *
    * ==Why sysl has anything to say about it when it emits one module already==
    *
    * A whole sysl program is lowered into **one** LLVM module, so the inlining that LTO exists to
    * give a C project across its translation units is already available to sysl's own code at `-O2`
    * — measured on slate, whose 541,000-line module is the whole interpreter. What is *not* in that
    * module is everything reached over the FFI: the C a package vendors, the `__posix__` shims
    * beside the standard library, and whatever a `@link` names. Those are separate objects compiled
    * by a separate clang, and LTO is the only thing that lets a call into one of them be inlined.
    *
    * So this is not a second way of asking for what `-O` already does. It is the flag that decides
    * whether the seam between sysl and its C is an optimization barrier.
    *
    * **Two modes and no more**, for the reason `levels` gives about a manifest: `thin` keeps a
    * per-module summary and links at something close to an ordinary link's cost, and `full` merges
    * every module into one and pays for it. `-flto` on its own is clang's spelling of `full`, and is
    * deliberately not offered — a key read by a consumer who did not write it is better for saying
    * which one it meant.
    */
  val ltoModes = List("thin", "full")

  /** The flag a level is passed as. A level is whatever was written — `0`, `2`, `s`, `z`, `fast` are
   * all clang's — and one clang does not have is clang's to complain about, since it is the
   * authority on its own levels and would say so better than a list here could.
   */
  private def flag(level: String) = s"-O$level"

  /** Extra flags spliced into every clang the build drives, read from `SYSL_EXTRA_CFLAGS`.
   *
   * **A build sysl drives is several clang invocations** — the module's IR, each C file a package
   * vendors, and the link — so a flag that has to be on all of them cannot be passed to one of them
   * by hand. `-fsanitize=address` is the case this exists for: given to the link alone it brings in
   * a runtime that instruments nothing, and given to a C file alone it instruments the binding and
   * not the program.
   *
   * It is an environment variable rather than an option because it is a *developer's* seam — a way
   * to reach the toolchain sysl is already driving — and not a thing a project should be able to
   * write down. Nothing here validates it: whatever it says goes to clang, and clang answers for it.
   */
  private[sysl] def extraFlags: List[String] =
    variable("SYSL_EXTRA_CFLAGS").toList.flatMap(_.split(" ").toList.filter(_.nonEmpty))

  /** The environment a build reads, which is the process's unless a caller has said otherwise.
   *
   * **A suite cannot set an environment variable**, and what card `0415` is about is precisely what
   * happens when one changes between two runs — so without a seam here the fix would be asserted on
   * a helper rather than on the thing that was broken. `RunCache.usingCache` is the same shape and
   * exists for the same reason.
   *
   * **Per thread**, matching `RunCache`'s: suites run beside each other driving one compiler, and a
   * process-wide override would reach a neighbour's build.
   *
   * An installed override **replaces** the process environment rather than adding to it, so a test
   * asking about an unset variable gets an unset variable however the developer's shell is
   * configured. That is the difference between a hermetic test and one that passes here.
   */
  private val overriddenEnv = new ThreadLocal[Option[Map[String, String]]] {
    override def initialValue(): Option[Map[String, String]] = None
  }

  private def variable(name: String): Option[String] =
    overriddenEnv.get.map(_.get(name)).getOrElse(envVar(name))

  private[sysl] def usingEnvironment[T](vars: Map[String, String])(body: => T): T = {
    val saved = overriddenEnv.get

    overriddenEnv.set(Some(vars))
    try body
    finally overriddenEnv.set(saved)
  }

  /** Every environment variable that can change the bytes a build produces, as `NAME=value` for the
   * ones that are set — what `RunCache`'s key has to carry so that two environments cannot share an
   * entry (card `0415`).
   *
   * **The rest of that key is settled by the command line and the source tree, and this is the half
   * that is not.** It was missing, and the failure was as bad as a cache failure gets: a run under
   * `SYSL_EXTRA_CFLAGS="-fsanitize=address"` over an unchanged tree was handed back the
   * **uninstrumented** binary the previous ordinary run had left in the slot, ran it, and reported
   * green. So the one workflow the org file prescribes for checking a binding — *set the variable,
   * re-run `sysl test .`* — answered about a binary the variable had never reached.
   *
   * **A stale answer is the only failure a cache can have, and a sanitizer is where it costs most**,
   * because a sanitizer's whole output is an absence. A wrong number would have been noticed.
   *
   * The list is what this file reads and nothing wider:
   *
   *   - **`SYSL_EXTRA_CFLAGS`** — spliced into every clang, and it also decides `sanitizerAttrs`,
   *     so it changes the *IR* as well as the command line.
   *   - **the toolchain locators** — `WASI_SDK_PATH` and the four Android variables. They name which
   *     clang and which sysroot a cross build gets, which is a different compiler producing
   *     different bytes.
   *
   * **`SYSL_LIB` is deliberately absent, and leaving it out is the more correct answer rather than
   * an omission.** It names where the library source *is*, and the key already carries that
   * library's **fingerprint** — its contents. Adding the path would make two identical trees at two
   * paths miss each other's entry for no gain, which is a cache that rebuilds more and proves
   * nothing extra. `SYSL_NO_CACHE` is absent for a different reason: it turns the cache off rather
   * than selecting an entry in it.
   *
   * A variable that is not set contributes nothing, so an ordinary build's key is exactly what it
   * was before this existed and no entry anybody has is invalidated by adding it.
   */
  private[sysl] def buildEnvironment: List[String] =
    List("SYSL_EXTRA_CFLAGS", "WASI_SDK_PATH", "ANDROID_NDK_ROOT", "ANDROID_NDK_HOME",
         "ANDROID_HOME", "ANDROID_SDK_ROOT")
      .flatMap(name => variable(name).map(v => s"$name=$v"))

  /** The LLVM function attribute each `-fsanitize=` name needs on the IR sysl generates.
   *
   * **This table is the whole of what sysl reads out of the extra flags** — everything else is
   * passed to clang, which answers for it. A sanitizer is in here when it works by instrumenting
   * functions that carry an attribute, and a frontend is what normally adds one.
   *
   * **UndefinedBehaviorSanitizer is deliberately absent, and its absence is the interesting entry.**
   * UBSan has no such attribute: clang implements it by emitting the checks and their
   * `__ubsan_handle_*` calls into the IR as it generates it, so there is nothing to switch on
   * afterwards. `-fsanitize=undefined` over a sysl build therefore covers the C a package vendors
   * and none of the sysl, and no marking here could change that. It is written down because the
   * failure is silent in exactly the way this whole mechanism exists to prevent.
   */
  private val SanitizerAttrs = Map(
    "address"   -> "sanitize_address",
    "hwaddress" -> "sanitize_hwaddress",
    "memory"    -> "sanitize_memory",
    "thread"    -> "sanitize_thread",
  )

  /** The sanitizers those flags asked for, by their `-fsanitize=` name.
   *
   * One flag may name several — `-fsanitize=address,undefined` is the ordinary way to ask for two —
   * so the list is split rather than matched whole. **`-fno-sanitize=` is not an ask**, which is
   * why the prefix is anchored: a substring test reads a suppression as a request and marks the IR
   * for a sanitizer the build has just turned off.
   */
  private[sysl] def sanitizersAsked(flags: List[String] = extraFlags): List[String] =
    flags.filter(_.startsWith("-fsanitize="))
      .flatMap(_.stripPrefix("-fsanitize=").split(",").toList)
      .map(_.trim).filter(_.nonEmpty).distinct

  /** The attributes every generated function needs, for what was asked. Sorted so that the IR a
   * build writes is the same from one run to the next.
   *
   * The flags are a parameter so that a test can ask about a build it is not running in: they are
   * read from the environment, which a suite cannot set for itself.
   */
  private[sysl] def sanitizerAttrs(flags: List[String] = extraFlags): List[String] =
    sanitizersAsked(flags).flatMap(SanitizerAttrs.get).distinct.sorted

  /** The IR with every function marked for the sanitizers the extra flags asked for.
   *
   * **LLVM instruments only functions carrying the right attribute, and it is a *frontend* that
   * adds one** — so IR handed to clang as a `.ll` links against the sanitizer's runtime and is
   * instrumented nowhere. That reads as a clean run and is a sanitizer that never looked, which is
   * the worst of the three possible outcomes; marking the functions here is what makes the answer
   * mean something.
   *
   * It was `sanitize_address` alone until the same hole was found one sanitizer along: a program
   * running sysl on another thread was checked with ThreadSanitizer, which reported nothing because
   * it had been given nothing to look at. `nm -u <binary> | grep -c "tsan_read\|tsan_write"`
   * answered zero, against three for the same race written in C. The attributes are a table now so
   * that the next one is an entry rather than a repeat of that afternoon.
   *
   * The attribute group is numbered because LLVM's syntax takes nothing else, and the number is one
   * nothing else emits — sysl's IR carries no attribute groups of its own.
   */
  private[sysl] def sanitized(ir: String, attrs: List[String] = sanitizerAttrs()): String =
    if attrs.isEmpty then ir
    else
      val marked = ir.linesIterator
        .map(l => if l.startsWith("define ") && l.endsWith(" {") then l.dropRight(1) + "#99 {" else l)
        .mkString("\n")

      marked + s"\n\nattributes #99 = { ${attrs.mkString(" ")} }\n"

  /** Where an `llvm-ar` is looked for, in order, when none was named.
   *
   * The PATH first, since a toolchain installed to be used is on it. Then the places a package
   * manager puts an LLVM that it deliberately keeps *off* the PATH — Homebrew does this so that its
   * LLVM does not shadow the system clang, which means the common case on a Mac is that `llvm-ar` is
   * installed, works, and is invisible to `which`. Finding it there is the difference between a
   * toolchain that works out of the box and one that starts with an error message.
   */
  private val arCandidates: List[String] =
    List(
      "llvm-ar",
      "/opt/homebrew/opt/llvm/bin/llvm-ar",
      "/usr/local/opt/llvm/bin/llvm-ar",
    )

  /** The archiver to use, or why there is none.
   *
   * A named one is **not** searched for and not fallen back from: someone who wrote down which
   * archiver to use is owed an error when it is not there, rather than a library quietly built with
   * a different one.
   */
  def findAr(named: Option[String]): Either[String, String] = named match
    case Some(path) =>
      Either.cond(runs(path), path, s"cannot run '$path' — --ar names the llvm-ar to build libraries with")
    case None =>
      searched.toRight(
        "cannot find llvm-ar, which building a library needs — install LLVM, or name one with --ar. " +
          "The platform's own 'ar' is not a substitute: it indexes only its own object format, and " +
          "drops the members of a library built for another machine without failing")

  /** The search, made once. Each candidate costs a process to rule out, and nothing about the answer
   * changes between two builds in one run — of which a test suite does many.
   */
  private lazy val searched: Option[String] = arCandidates.find(runs)

  /** Where a `clang` is looked for, in the order tried. The same list as `arCandidates` and for the
   * same reason: Homebrew keeps its LLVM off the PATH so as not to shadow the system clang, so on a
   * Mac the capable compiler is commonly installed, working, and invisible to `which`.
   */
  private val clangCandidates: List[String] =
    List(
      "clang",
      "/opt/homebrew/opt/llvm/bin/clang",
      "/usr/local/opt/llvm/bin/clang",
    )

  /** The back ends a clang has, as `-print-targets` lists them — or an empty set if it cannot be run
   * at all, which the caller tells apart by there being no candidate that answers.
   *
   * Cached per path because it is a process, and a test suite asks this of the same clang thousands
   * of times.
   */
  private val backendCache = collection.mutable.Map.empty[String, Set[String]]

  /** The back ends a *named* clang has, for a caller that chose the compiler rather than searching
   * for one. `AbiAgainstClangTests` is the case: an oracle asks one clang about every target, so it
   * needs the same question `findBackendClang` asks internally without the search in front of it.
   */
  def backendsOf(path: String): Set[String] = backends(path)

  private def backends(path: String): Set[String] = backendCache.getOrElseUpdate(path, {
    val r = exec(Seq(path, "-print-targets"))

    if r.exitCode != 0 then Set.empty
    // Each line is `    name  - Description`, and the name is the first word.
    else r.stdout.linesIterator.map(_.trim.takeWhile(!_.isWhitespace)).filter(_.nonEmpty).toSet
  })

  /** A clang that can **emit code** for `target`, or why there is none — which is a weaker question
   * than `findClang`'s and is deliberately kept separate from it.
   *
   * **The PATH's clang is preferred and is nearly always the answer** — a host build must not quietly
   * change compiler because some other LLVM happens to be installed. What sends the search further is
   * a target the PATH clang has no back end for, which on a Mac is every RISC-V one: Apple's clang is
   * trimmed to Apple's processors, so it can parse `-target riscv32-unknown-elf` and then fail to
   * produce an object for it. That failure is not a compiler bug and reads exactly like one.
   *
   * What this answers for is turning **IR into an object**, and a `.ll` names its own triple and
   * includes no header, so nothing outside the back end is needed to lower it. Compiling a `.c` or
   * linking a program needs the target's headers and its libraries as well, and that is the question
   * `findClang` asks — the two part company on Android and nowhere else so far.
   */
  def findBackendClang(target: Target): Either[String, String] =
    clangCandidates.find(p => runs(p) && backends(p).contains(target.cpu.backend)).toRight(
      clangCandidates.find(runs) match
        case Some(found) =>
          s"no clang here has the '${target.cpu.backend}' back end that '${target.name}' needs. " +
            s"'$found' registers ${shown(backends(found))} — a vendor's clang is often trimmed to " +
            "that vendor's processors. Install LLVM ('brew install llvm'), or name one with --cc"
        case None =>
          "cannot find clang, which building needs — install LLVM, or name one with --cc")

  /** The clang to **build** for `target` with, or why there is none: one that has the back end *and*
   * can reach the machine's headers and libraries.
   *
   * ==Having the back end is not having the toolchain, and Android is where that first bites==
   *
   * Every target until Android was one the host's own clang could serve outright, so this and
   * `findBackendClang` were one function and nothing noticed. A host build compiles against the
   * machine's own headers; a freestanding one includes nothing at all; RISC-V sends the search on to
   * Homebrew's LLVM, which is still a whole toolchain for the triple.
   *
   * **Android has a sysroot of its own and no clang outside the NDK carries it.** Apple's clang has
   * `aarch64`, so a search by back end picks it and it then cannot find `<dirent.h>` — which reads as
   * a broken standard library rather than as the wrong compiler, and is the reason this is a separate
   * question rather than a fallback. The NDK's clang needs no `--sysroot` flag from us: since r19 it
   * resolves one relative to its own path, so naming the binary is the whole of what is required.
   *
   * A named one is **not** searched past and not fallen back from, by the rule `findAr` states: a
   * person who wrote down which compiler to use is owed an error rather than a different one.
   */
  def findClang(target: Target, named: Option[String] = None): Either[String, String] = named match
    case Some(path) =>
      if !runs(path) then Left(s"cannot run '$path' — --cc names the clang to build with")
      else
        Either.cond(backends(path).contains(target.cpu.backend), path,
          s"'$path' has no '${target.cpu.backend}' back end, so it cannot build for " +
            s"'${target.name}' — it registers ${shown(backends(path))}")
    case None if target.os == Os.Android => androidClang
    case None if target.os == Os.Wasi    => wasiClang
    case None                            => findBackendClang(target)

  private def shown(bs: Set[String]): String =
    if bs.isEmpty then "none" else bs.toList.sorted.mkString(", ")

  private def runs(path: String): Boolean =
    try exec(Seq(path, "--version")).exitCode == 0
    catch case _: Exception => false

  /** wasi-sdk's clang, found from the environment, or the sentence saying what to set.
   *
   * **The same shape as the NDK's, for the same reason.** wasi-sdk is clang, wasi-libc and a sysroot
   * in one download that is installed nowhere in particular, and a clang picked merely for having
   * the `wasm32` back end carries no sysroot at all — so it succeeds at the search and fails at the
   * first `#include`. Homebrew's clang is exactly that here, which makes this the `dirent.h` case
   * with a different header name.
   *
   * **Its clang self-resolves the sysroot for a `wasm32-wasip1` triple**, checked rather than
   * assumed — a `clang.cfg` beside the binary points at `share/wasi-sysroot` — so naming the binary
   * is the whole of it and there is no `--sysroot` for sysl to pass.
   */
  private def wasiClang: Either[String, String] =
    wasiClangIn(variable("WASI_SDK_PATH"))
      .flatMap(cc => Either.cond(runs(cc), cc, s"cannot run '$cc', which is wasi-sdk's clang"))

  /** The path resolution behind `wasiClang`, taking its directory rather than reading it, so that
   * what it decides can be asserted on a machine with no wasi-sdk on it.
   */
  private[sysl] def wasiClangIn(sdkRoot: Option[String]): Either[String, String] =
    sdkRoot match
      case None =>
        Left("building for WASI needs wasi-sdk's own clang, and nothing here says where it is — a " +
          "clang picked for having the wasm32 back end carries no sysroot, so it fails at the first " +
          "'#include'. Set WASI_SDK_PATH to the directory holding 'bin/clang' " +
          "(github.com/WebAssembly/wasi-sdk/releases)")
      case Some(sdk) =>
        val cc = s"$sdk/bin/clang"

        if !isFile(cc) then
          // The path rather than the variable: what somebody set is in the message, so a WASI_SDK_PATH
          // pointing one directory too high or too low is visible rather than described.
          Left(s"'$sdk' holds no 'bin/clang' — WASI_SDK_PATH names the wasi-sdk directory itself, " +
            "the one with 'bin' and 'share' in it")
        else Right(cc)

  /** The NDK's clang, found from the environment, or the sentence saying what to set.
   *
   * **The environment is the only place it is looked for, and where the environment is silent this
   * refuses rather than guessing.** An NDK is not installed anywhere in particular — it sits under
   * whichever SDK directory the person chose — so a compiler that went hunting through home
   * directories would be right on the machine it was written on and wrong afterwards, and the way it
   * would be wrong is by finding *an* NDK rather than none. A refusal naming the variable is a
   * sentence somebody can act on; a silently chosen toolchain is not.
   *
   * `ANDROID_NDK_ROOT`/`ANDROID_NDK_HOME` name an NDK outright and win, which is what a standalone
   * install has. Otherwise the SDK is named and the newest `ndk/<version>` under it is taken.
   *
   * **`ANDROID_HOME` is the SDK's current name and `ANDROID_SDK_ROOT` is DEPRECATED, so the first is
   * what the refusal tells anybody to set.** Google has swapped these twice — `ANDROID_HOME`
   * originally, `ANDROID_SDK_ROOT` in between, and `ANDROID_HOME` again now, with the tools checking
   * the two agree where both are set. Both are read here because both are in wide use and a machine
   * that has one working must go on working; only the *advice* is opinionated, and it names the one
   * that is not deprecated.
   */
  private def androidClang: Either[String, String] =
    androidClangIn(variable("ANDROID_NDK_ROOT").orElse(variable("ANDROID_NDK_HOME")),
                   variable("ANDROID_HOME").orElse(variable("ANDROID_SDK_ROOT")))
      .flatMap(cc => Either.cond(runs(cc), cc, s"cannot run '$cc', which is the NDK's clang"))

  /** The path resolution behind `androidClang`, taking its two directories rather than reading them,
   * so that what it decides can be asserted on a machine with no NDK on it.
   */
  private[sysl] def androidClangIn(ndkRoot: Option[String], sdkRoot: Option[String])
      : Either[String, String] =
    ndkRoot match
      case Some(ndk) => clangUnderNdk(ndk)
      case None =>
        sdkRoot match
          case None =>
            // `ANDROID_HOME` and not `ANDROID_SDK_ROOT`: the second is deprecated, and telling
            // somebody to export a deprecated variable is advice that ages into a second problem.
            // Both are still *read* — see `androidClang` — so a machine already set up either way
            // goes on working, and only what this sentence recommends is opinionated.
            Left("building for Android needs the NDK's own clang, and nothing here says where it is " +
              "— no clang outside the NDK carries Bionic's headers, so one picked for having the " +
              "back end fails at the first '#include'. Set ANDROID_HOME to the Android SDK " +
              "(the directory holding 'ndk/'), or ANDROID_NDK_ROOT to one NDK directly")
          case Some(sdk) =>
            val installed = s"$sdk/ndk"

            if !isDirectory(installed) then
              // **The path and not the variable, because two spellings reach here and this cannot
              // tell which one was set.** Naming `ANDROID_SDK_ROOT` at somebody who set
              // `ANDROID_HOME` sends them to look at a variable that is empty and correct — the same
              // class of misdirection as the `dirent.h` diagnostic this whole search exists to
              // replace. The directory is in the message, so the setting that produced it is clear.
              Left(s"the Android SDK at '$sdk' holds no 'ndk' directory — the NDK is a separate " +
                "download, installed from Android Studio's SDK Manager under SDK Tools as " +
                "'NDK (Side by side)'")
            else
              // Several may be installed side by side, which is what the directory is named for, and
              // the newest is the one to take. Sorted on the version read as *numbers* — `9.x` sorts
              // above `30.x` as text, which is the whole reason this is not a plain `.sorted`.
              listFiles(installed).toList.filter(isDirectory).sortBy(d => versionKey(Project.basename(d)))
                .lastOption match
                case Some(newest) => clangUnderNdk(newest)
                case None =>
                  Left(s"'$installed' holds no NDK — the directory is there and empty, so an install " +
                    "was started and did not finish")

  /** The clang inside one NDK, or why that directory is not one.
   *
   * **The host directory is listed rather than computed, and that is not laziness.** It is named for
   * the platform the toolchain was *built* for, which on an Apple Silicon Mac is still
   * `darwin-x86_64` — the clang inside is a universal binary with a real arm64 slice and runs
   * natively, and the path has simply never been renamed. Anything that spells the directory from
   * the host's own architecture finds nothing on the machine it most needs to work on.
   */
  private def clangUnderNdk(ndk: String): Either[String, String] = {
    val prebuilt = s"$ndk/toolchains/llvm/prebuilt"

    if !isDirectory(prebuilt) then
      Left(s"'$ndk' is not an Android NDK — it has no 'toolchains/llvm/prebuilt' in it")
    else
      listFiles(prebuilt).toList.filter(isDirectory).sorted.headOption match
        case None => Left(s"'$prebuilt' holds no toolchain for any host")
        case Some(host) =>
          val cc = s"$host/bin/clang"

          Either.cond(isFile(cc), cc, s"'$ndk' has no clang at '$cc'")
  }

  /** A dotted version as something sortable, each numeric part widened so that the comparison is by
   * value rather than by first digit. A part that is not a number is left as it is, which puts it
   * after every number rather than throwing.
   */
  private def versionKey(name: String): String =
    name.split('.').map(p => if p.nonEmpty && p.forall(_.isDigit) then "0" * (12 - p.length) + p else p)
      .mkString(".")

  /** Objects gathered into an archive the linker takes as it is.
   *
   * `r` replaces, `c` creates without remarking on it, and `s` writes the symbol index — the index
   * being the whole reason this is a subprocess rather than thirty lines of `Ar`: building one means
   * reading the symbol table of every member, in whichever object format the target uses.
   *
   * The output is removed first because `r` *merges* into an archive that is already there. Rebuilding
   * a library whose sources had shrunk would otherwise keep the members of the previous build, and
   * they would still resolve, so nothing would go wrong until the stale definition was the one linked.
   */
  def archive(objects: List[String], out: String, ar: String): Either[String, Unit] = {
    try if exists(out) then deleteFile(out)
    catch case e: Exception => return Left(s"cannot replace $out: ${e.getMessage}")

    val result = exec(ar :: "rcs" :: out :: objects)

    if result.exitCode == 0 then Right(())
    else Left(s"$ar failed (exit ${result.exitCode}):\n${result.stderr.trim}")
  }

  /** Links an IR module into a native executable at `exe`, for a given machine.
   *
   * The triple goes on the command line as well as in the module because the two answer different
   * questions: the module's says what the code *is*, and the driver's decides which linker and
   * which system libraries it is linked against. Passing it is therefore what makes naming a
   * cross target fail honestly at the link rather than silently produce a host binary.
   *
   * `-Wno-override-module` is the one warning suppressed, and only because sysl states a target
   * *family* — `arm64-apple-macosx` — where the driver refines it to the installed SDK's version.
   * With `--target` passed, the only override that can still happen is that refinement.
   */
  def build(ir: String, exe: String, target: Target = Target.default,
            archives: List[String] = Nil, level: String = defaultOptimization,
            links: List[String] = Nil, objects: List[String] = Nil,
            paths: SearchPaths = SearchPaths.none, verbose: Boolean = false,
            pipeline: Pipeline = Pipeline.none): Either[String, Unit] = {
    findClang(target, paths.cc).flatMap { cc =>
      val ll = createTempFile("sysl-", ".ll")
      writeFile(ll, sanitized(ir))

      val command = linkCommand(ll, archives, exe, target, level, links, objects, paths, cc, pipeline)

      if verbose then trace(s"link: ${command.mkString(" ")}")

      val result = exec(command)
      deleteFile(ll)

      if result.exitCode == 0 then Right(())
      else Left(s"$cc failed (exit ${result.exitCode}):\n${result.stderr.trim}")
    }
  }

  /** The whole of what is handed to the driver to link one program, as a list, so that what a target
   * decides about it can be asserted without a machine of that kind to link on.
   *
   * The order is the linker's and not a style: an archive goes **after** the module that calls into
   * it, because the scan is left to right and a member is pulled in only to resolve a symbol already
   * undefined — an archive listed first would be scanned before anything needed it and contribute
   * nothing. The system libraries go after both for exactly the same reason, since the library's own
   * object is one of the things that calls them.
   *
   * `objects` is the C a source tree carried (`NativeSources`), and it sits between the two for the
   * same reason again: an object is linked whether or not anything needed it, so its own calls are
   * undefined symbols by the time the archives and the `-l`s are scanned. A shim placed after them
   * would have nothing left to resolve `sqlite3_open` against.
   *
   * The `-L`s go **before** everything they could affect (`SearchPaths`). A search path is not an
   * input being scanned in turn — it is where the scan looks — so putting it first is what makes the
   * line read the way a hand-run clang would be written, and leaves no question about whether a
   * directory named late reaches a library named early.
   *
   * A library `link` asked for statically is its archive's **path** in place of its `-l`, in the same
   * position (`StaticLink`): an archive is scanned exactly where the `-l` would have been, and the
   * path is the only spelling macOS's `ld` does not resolve to the `.dylib` beside it.
   */
  private[sysl] def linkCommand(ll: String, archives: List[String], exe: String, target: Target,
                                level: String = defaultOptimization,
                                links: List[String] = Nil, objects: List[String] = Nil,
                                paths: SearchPaths = SearchPaths.none,
                                cc: String = "clang",
                                pipeline: Pipeline = Pipeline.none): List[String] =
    List(cc, s"--target=${target.triple}", "-Wno-override-module", flag(level)) :::
      pipeline.flags ::: extraFlags :::
      machineFlags(target) ::: linkerFlags(target) ::: deadStrip(target) :::
      paths.linkFlags ::: rpathFlags(target, paths) ::: List(ll) ::: objects ::: archives :::
      StaticLink.rewrite(libraryFlags(links, target) ::: paths.probedLinkFlags, paths.archives) :::
      List("-o", exe)

  /** A run-time search path for each directory `--link-path` named, so that a shared library found
   * at link time is found again at load time (card `0414`).
   *
   * ==Why the flag implies this, when `-L` does not==
   *
   * **Neither clang nor rustc adds an rpath from a search-path flag, and that was measured rather
   * than assumed** — a `libfoo.dylib` whose install name is `@rpath/libfoo.dylib`, linked with `-L`
   * alone, links cleanly under both and then dies at startup with `Library not loaded ... no
   * LC_RPATH's found`. rustc has an opt-in for it (`-C rpath`, off by default) and clang has none.
   * So the analogy argues the other way and is worth stating: this is a place sysl deliberately
   * does something a C or Rust driver does not.
   *
   * **The reason it is right here is that `--link-path` is not `-L`.** `-L` is one flag of a
   * command line the writer is composing, next to the `-Wl,-rpath` they would add themselves; sysl
   * composes the whole line and the writer never sees it, so a directory named to sysl is the only
   * thing they get to say about where the library is. Left at `-L` the flag answers half the
   * question and the other half is unreachable — a sysl program cannot pass a raw linker flag —
   * which is what card `0414` was filed on: `sysl-lang/webview` could be linked and could not be
   * run, and the fix had to be made in the *Homebrew formula* by overriding the install name.
   *
   * A `pkg_config` probe's directories are deliberately **not** here. A probe answers with its own
   * `-Wl,-rpath` where the library wants one, which `probedLinkFlags` carries through untouched —
   * so adding a second, guessed one would override a decision the library's own `.pc` file had
   * already made.
   *
   * Absolute, via `Project.absolute`, because an rpath is read by the loader in whatever directory
   * the program is run from rather than the one it was built in.
   *
   * **Not on a freestanding target, and that is about meaning rather than about breakage.** Both
   * `wasm-ld` and `ld.lld` accept `-rpath` for a bare-metal link without complaint — checked, since
   * the guard would otherwise read as defensive — and what they write is a run-time search path for
   * a dynamic loader that does not exist on such a machine. There is nothing to find it, so there
   * is nothing to say.
   */
  private def rpathFlags(target: Target, paths: SearchPaths): List[String] =
    if target.os == Os.Freestanding then Nil
    else paths.link.map(d => s"-Wl,-rpath,${Project.absolute(d)}")

  /** What a target needs said to the **linker** beyond its triple, which today is WebAssembly's and
   * nobody else's.
   *
   * `wasm-ld` is not a variation on `ld` and the driver's defaults for it are a hosted program's:
   * without `-nostdlib` the link opens with `crt1.o`, `-lc` and a wasm `libclang_rt.builtins.a`, none
   * of which exists for `wasm32-unknown-unknown` and the first of which is what the error names — so
   * the failure reads as a broken installation rather than as a freestanding target having no libc.
   *
   * **`--entry=main` is the load-bearing half, and the reason is a link that SUCCEEDS.** The obvious
   * spelling is `--no-entry`, since a wasm module has no `_start`; pair it with `--gc-sections` — as
   * every link here does — and nothing is reachable from anywhere, so the linker drops the entire
   * program and reports success. Measured: 278 bytes, no `main`, exit 0. Naming `main` as the entry
   * keeps it and everything it reaches, and exports it under that name for an embedder to call, which
   * is the whole of what running a wasm module means.
   *
   * The failure that replaces it is the honest one this page describes for every freestanding target:
   * a program that prints fails at the link naming `putchar`, because nothing on a bare target
   * defines it.
   *
   * **NONE of it applies to `wasm32-wasi`, and both halves would break it.** WASI is the same
   * processor with a libc under it: wasi-libc supplies `_start`, which is what a runtime calls and
   * which calls `main` itself — so `-nostdlib` drops the very libc that makes the row worth having,
   * and naming `main` as the entry replaces the `_start` a runtime is looking for. The condition is
   * therefore the *operating system* and not the processor, which is the distinction the row exists
   * to draw.
   */
  private def linkerFlags(target: Target): List[String] =
    if target.cpu == Cpu.Wasm32 && target.os == Os.Freestanding then
      List("-nostdlib", "-Wl,--entry=main")
    else Nil

  /** What the machine needs said to clang **beyond its triple**, on every command line that produces
   * code for it or reads a header as it.
   *
   * There is one such thing today and it is the floating-point unit. A triple names an architecture
   * and a calling convention, and on Arm those two leave the *presence* of the unit unstated: the
   * `eabi` suffix was only ever a statement about where arguments travel, so what is left is clang's
   * default for the architecture. `-mfpu=` is the missing half of the sentence, and **both answers
   * are given, because leaving one of them to the default left it to the toolchain.**
   *
   *   - `-mfpu=none`, for `noFpu`, does the two things a board without a unit needs: the back end
   *     stops selecting VFP instructions, and `__ARM_FP` goes away, which is what a header guarding
   *     on `__FPU_PRESENT` reads;
   *   - `-mfpu=<`the row's `fpu>`, for a board that has one, says which. Without it the answer was
   *     whatever clang thought: `thumbv8m.main-none-eabi` defines `__ARM_FP 0xe` under Apple clang 21
   *     and Homebrew clang 22 and defines nothing at all under apt.llvm.org's clang 20, so
   *     `thumb-freestanding-softfp` compiled a multiply to `vmul.f32` on one machine and to
   *     `__aeabi_fmul` on the other, and the Linux CI was red on it for two releases.
   *
   * Exactly one of the two is passed for a target, which `TargetTests` pins by refusing a row that
   * claims both.
   *
   * **`-mfloat-abi` goes with it, because `soft` OVERRIDES `-mfpu` and is what a bare `eabi` triple
   * defaults to on some clangs.** Measured: `-mfloat-abi=soft -mfpu=fpv5-sp-d16` defines no
   * `__ARM_FP` at all — the convention wins — so naming the unit alone did not fix
   * `thumb-freestanding-softfp` on the CI's clang 20, which had defaulted the triple to `soft` where
   * Apple's clang 21 defaults it to `softfp`. The row's own `softFloat` says which one it means, and
   * `softfp` is precisely *a unit, used, with arguments in core registers*. So a Thumb row states the
   * pair — `soft`+`none`, `softfp`+its unit, or `hard`+its unit — and none of the three is left for a
   * default to decide. The two hard rows are unmoved by saying it: their assembly is byte-identical
   * with `-mfloat-abi=hard` and without it, as the `soft` rows' is with `-mfloat-abi=soft`.
   *
   * **It goes on all four command lines, not only the two that emit instructions.** A package's C and
   * a `c const` probe are compiled *as* the target, and CMSIS's own
   * `#error "Compiler generates FPU instructions for a device without an FPU"` refuses them at the
   * `#include`, one step before anything has been lowered.
   *
   * **It is Arm's flag because Arm is where the triple is silent.** RISC-V says it in the triple —
   * `riscv32-unknown-elf` is RV32IMAC and has no F extension to turn off — so `noFpu` is true there
   * and nothing is passed. Armv6-M and Armv7-M are that same case on the Arm side, their triples
   * defining no `__ARM_FP` at all; the flag is passed for them anyway rather than being conditioned on
   * which Thumb it is, because it was checked to be accepted and to change nothing there, and one rule
   * that is uniformly true beats two that have to be kept in step.
   */
  private[sysl] def machineFlags(target: Target): List[String] =
    if target.cpu != Cpu.Thumb then Nil
    else if target.noFpu then List("-mfloat-abi=soft", "-mfpu=none")
    else
      target.fpu.toList.flatMap: unit =>
        List(if target.softFloat then "-mfloat-abi=softfp" else "-mfloat-abi=hard", s"-mfpu=$unit")

  /** What a build's link directives (`reference/ffi.md § @link`) become on **this** target's
   * command line.
   *
   * A directive names a library and never a flag, so this is the whole of the translation, and it is
   * a decision per operating system written out per case rather than a default that some target
   * falls into. Three things can happen to a name, and only two of them look alike from outside:
   *
   *   - **The target has it, separately.** `-lm` on ELF. The ordinary case, and the one every name
   *     the compiler has never heard of takes.
   *   - **The target already links what holds it.** Darwin keeps libc and libm in `libSystem` and
   *     Windows keeps them in the CRT, both of which the driver passes unasked, so naming either
   *     adds nothing. `-lm` on a Mac is not merely redundant — there is no `libm.dylib` to find.
   *   - **The target does not have it at all.** A freestanding build has no libc, so nothing can be
   *     passed for one. A program that then calls `sqrt` fails at the link naming `sqrt`, which is
   *     the honest report: the missing thing is the function, and no `-l` would have supplied it.
   *
   * The last two both emit nothing and are written apart anyway, because they are different facts
   * and a target added to the registry has to answer them separately. `hosted` is what tells them
   * apart: it is the C runtime's own family that a freestanding target is missing, and a directive
   * naming anything else — a driver's own archive, say — is passed through on every target.
   *
   * **This replaced a list the driver carried.** Until `reference/ffi.md § @link` existed, `-lm`
   * was appended to every ELF link whether or not the program touched mathematics, because the
   * compiler had no way to be told and `sysl.math` had no way to say. It says so itself now, in
   * `library/sysl/sys/math.sysl`, and the decision left here is the only part that was ever the
   * driver's: where a named library lives on the machine being built for.
   */
  private[sysl] def libraryFlags(links: List[String], target: Target): List[String] =
    links.filter(name => !provided(target.os).contains(name)).map(name => s"-l$name")

  /** The libraries a target needs no flag for — because something the driver already links holds
   * them, or because the target has none of them to link.
   *
   * The set is deliberately small: it holds what the compiler can *defend* rather than every library
   * a platform might bundle. `c` and `m` are here because the standard module itself needs them and
   * because their placement genuinely differs across the four. Guessing about `pthread` or `dl` would
   * mean guessing wrong for some platform nobody here has, and the cost of being wrong is a link that
   * fails on a machine the author cannot reach — so an unknown name is passed through, where a wrong
   * `-l` at least names itself in the error.
   */
  private def provided(os: Os): Set[String] =
    os match
      // libSystem and the CRT carry both, and the driver links them without being asked.
      case Os.MacOS | Os.Windows => Set("c", "m")
      // clang links libc itself; the mathematics is a file of its own.
      //
      // Android is here rather than beside Darwin because Bionic keeps them apart the same way, and
      // that was measured rather than assumed: an NDK link of a program calling `tgamma` fails on an
      // undefined symbol without `-lm`. (`sqrt` alone is not the test — it lowers to an instruction
      // and links either way, which is the shape of the mistake this line would otherwise make.)
      case Os.Linux | Os.Android => Set("c")
      // wasi-libc carries both and the driver links them, measured the way Android's was: a program
      // calling `tgamma` links and runs under `wasm32-wasip1` with no `-lm` at all. (`sqrt` alone is
      // not the test — it lowers to an instruction and links either way.) `-lm` is *accepted*, since
      // wasi-libc ships an empty archive of that name, which is why the answer had to be measured
      // rather than read off whether the flag was refused.
      case Os.Wasi => Set("c", "m")
      // No libc exists here, so there is nothing to pass for one.
      case Os.Freestanding => Set("c", "m")

  /** Ask the linker to drop what nothing reaches.
   *
   * **This is the second of two mechanisms, and it is not made redundant by the first.** Because a
   * `.syslib` is an archive, the linker already pulls in only the members that resolve something
   * undefined — but a member is pulled *entire*, and one member holds many definitions. Dead-striping
   * is what removes the rest of a member that was pulled in for one function. Without it a program
   * whose whole text is `print(1)` carried all 61 of the standard module's symbols, the reader and
   * the string builder and the hashes among them, none of which it can reach.
   *
   * The two spellings are not interchangeable. Mach-O resolves at **atom** granularity, so
   * `-dead_strip` needs nothing from the compile step. ELF resolves at **section** granularity and
   * `--gc-sections` can therefore only drop a function that was given a section of its own, which is
   * what `-ffunction-sections` in `compileObject` is for — the two halves are one mechanism and
   * changing either alone silently stops working.
   */
  private def deadStrip(target: Target): List[String] =
    target.os match
      case Os.MacOS => List("-Wl,-dead_strip")
      case _        => List("-Wl,--gc-sections")

  /** Assembles an IR module into a relocatable object file — the ahead-of-time half of a library.
   *
   * `-c` is the whole difference from `build`: nothing is linked, so a module with no `main` and
   * with unresolved calls into the program that will use it is exactly what is wanted.
   *
   * `-ffunction-sections`/`-fdata-sections` give every definition a section of its own, which is
   * what lets the linker drop the ones a program never reaches (`deadStrip`). It is the *library*
   * side of that pair and useless without the linker side, and on Mach-O it is redundant rather than
   * wrong — atoms are already per-definition there. Passed unconditionally so that an artifact built
   * on one host still strips when linked for another.
   *
   * `named` says which compiler to use instead of searching for one, and is `findClang`'s parameter
   * of the same name reached from here. **It is on this one of the four because assembling IR is the
   * step that needs no sysroot** — a `.ll` includes nothing — so a caller asking *what does sysl's
   * output lower to for this machine* can be answered by any clang with the back end, on a machine
   * that could not build a whole program for that target at all. `findBackendClang` finds one.
   */
  def compileObject(ir: String, obj: String, target: Target = Target.default,
                    level: String = defaultOptimization,
                    named: Option[String] = None,
                    pipeline: Pipeline = Pipeline.none): Either[String, Unit] = {
    findClang(target, named).flatMap { cc =>
      val ll = createTempFile("sysl-", ".ll")
      writeFile(ll, sanitized(ir))

      val result = exec(Seq(cc, s"--target=${target.triple}", "-Wno-override-module", flag(level)) ++
        pipeline.flags ++ extraFlags ++ machineFlags(target) ++
        Seq("-ffunction-sections", "-fdata-sections", "-c", ll, "-o", obj))
      deleteFile(ll)

      if result.exitCode == 0 then Right(())
      else Left(s"$cc failed (exit ${result.exitCode}):\n${result.stderr.trim}")
    }
  }

  /** Compiles one of a library's C files into a relocatable object, to be archived beside the
   * object the library's own sysl compiled to (`reference/ffi.md § A library may carry C`).
   *
   * **This is what makes a binding to a C library writable at all.** A caller-allocated type like
   * `regex_t` has a size and an alignment that only the target's own headers know, and a macro like
   * `REG_EXTENDED` has no symbol to link against; both are reachable from C and from nothing else.
   * A few lines of C beside the binding turn each of them into an ordinary function the sysl side
   * declares with `extern` — so the numbers come from the headers rather than from a transcription
   * that is right until the platform changes.
   *
   * The same section flags as `compileObject`, for the same reason: a shim a program never calls is
   * dropped by the linker only if it was given a section of its own. A library that binds forty
   * functions should not cost a program that calls one of them the other thirty-nine.
   *
   * `-Wno-override-module` is *not* passed. It exists for a `.ll` that states a target family the
   * driver then refines, and a `.c` has no module statement to override — passing it here would be
   * suppressing a warning that cannot arise.
   *
   * `paths.include` is where a header the shim `#include`s is looked for beyond the toolchain's own
   * directories (`SearchPaths`). This is the half of that setting the *link* flag cannot stand in
   * for: a binding to a library outside the default prefix fails here, at the `#include`, one step
   * before anything gets as far as a `-l`.
   *
   * `paths.defines` is the half the *include* flag cannot stand in for, and it is a step later
   * again: a header found at its path and compiled without the macros its project configures it with
   * is a header that refuses on its own terms. `SearchPaths.defineFlags` has the worked example.
   *
   * **`-fPIC` where the target says so (`Target.positionIndependent`), and its absence was silent.**
   * A package's C has ordinary C globals, which are preemptible — so without the flag clang emits an
   * absolute-page reference to the symbol and the shared link refuses it. That is a *link* error in
   * somebody else's build system, naming a symbol from a vendored library:
   * `relocation R_AARCH64_ADR_PREL_PG_HI21 cannot be used against symbol 'b2AssertHandler';
   * recompile with -fPIC`. It reads as a broken package and is not one.
   *
   * **This does not contradict `getting-started/cli.md § targets`'s finding that sysl needs no
   * relocation model.** That was measured on sysl's *own* object, whose globals are all
   * `Linkage.Private` and therefore not preemptible. Carried C is the half that finding did not
   * cover.
   *
   * `named` says which compiler to use instead of searching for one, exactly as `compileObject`'s
   * does and for the same narrow reason: a C file that includes nothing needs no sysroot, so a test
   * asking what a flag does to an object can be answered by any clang with the back end.
   *
   * `-fshort-enums` where the target says so (`Target.shortEnums`), because a package's C is being
   * compiled to link against a C project that sysl did not start. Clang and GNU's `arm-none-eabi`
   * default opposite ways on the same triple, so passing nothing means whichever this clang prefers —
   * and a linker that has to reconcile the two says `uses 32-bit enums yet the output is to use
   * variable-size enums; use of enum values across objects may fail` and carries on. It is the same
   * class of fact as the float ABI: a convention of the build sysl is joining, and therefore not
   * sysl's to pick.
   */
  def compileC(source: String, obj: String, target: Target = Target.default,
               level: String = defaultOptimization,
               paths: SearchPaths = SearchPaths.none, verbose: Boolean = false,
               named: Option[String] = None,
               pipeline: Pipeline = Pipeline.none): Either[String, Unit] = {
    findClang(target, named orElse paths.cc).flatMap { cc =>
      val command = Seq(cc, s"--target=${target.triple}", flag(level)) ++ pipeline.flags ++
        extraFlags ++ machineFlags(target) ++
        Option.when(target.shortEnums)("-fshort-enums") ++
        Option.when(target.positionIndependent)("-fPIC") ++ paths.defineFlagsFor(source) ++
        paths.includeFlags ++
        Seq("-ffunction-sections", "-fdata-sections", "-c", source, "-o", obj)

      if verbose then trace(s"compile: ${command.mkString(" ")}")

      val result = exec(command)

      if result.exitCode == 0 then Right(())
      // The file first, because a build compiles many and which one failed is the thing to act on —
      // and the compiler after it, so that a diagnostic the *driver* provoked (a bad `-O`, a triple
      // it will not take) reads as the tool's report rather than as sysl's own. The link path names
      // it for the same reason.
      else Left(s"$source did not compile ($cc, exit ${result.exitCode}):\n${result.stderr.trim}")
    }
  }

  /** Compiles, links, and runs a source program, returning its exit code and captured
   * stdout — the end-to-end path the run-it test tier exercises.
   *
   * There is one target here and not two: a program is built for the machine it is about to be run
   * on, so a cross target has nothing to run the result with and is not offered.
   */
  def compileAndRun(source: String, name: String = "<input>",
                    args: List[String] = Nil): Either[String, (Int, String)] =
    runIr(Compiler.compiled(List(Source(name, source))), args)

  /** The same, for the files of one program. */
  def compileAndRun(sources: List[Source]): Either[String, (Int, String)] =
    runIr(Compiler.compiled(sources), Nil)

  /** The same, for a program compiled **against a library** whose modules arrive as trees rather
   * than as source — an `AstCodec` artifact, decoded (`Compiler.compileWith`).
   */
  def compileAndRun(sources: List[Source], libraries: List[Program]): Either[String, (Int, String)] =
    runIr(Compiler.compiledWith(sources, libraries), Nil)

  /** The same, compiled against a **prebuilt standard module** rather than the copy the compiler
   * carries: the trees arrive decoded, the symbols its object half already defines are declared
   * instead of emitted a second time, and the archive holding them is handed to the linker.
   *
   * This is the shape an ordinary `sysl build` has — it is what `.sysl/std.syslib` is *for* — and
   * the caller supplies the artifact because building one belongs to whoever can decide how often it
   * is worth doing. Given none, this is the compilation the other overloads perform.
   */
  def compileAndRun(sources: List[Source], libraries: List[Program], args: List[String],
                    std: Option[Stdlib], precompiled: Set[String], archives: List[String])
      : Either[String, (Int, String)] =
    runIr(Compiler.compiledWith(sources, libraries, Target.default, precompiled, std), args, archives)

  /** The same, with the program's two output streams kept apart. */
  def compileAndRunFully(sources: List[Source], libraries: List[Program], args: List[String],
                         std: Option[Stdlib], precompiled: Set[String], archives: List[String])
      : Either[String, (Int, String, String)] =
    runIrFully(Compiler.compiledWith(sources, libraries, Target.default, precompiled, std), args, archives)

  /** `args` are the words the program is started with, which reach it exactly as they would from a
   * shell: the executable's own path arrives ahead of them as the zeroth, since that is what the
   * platform passes and not something this could withhold.
   *
   * The whole `Compiled` is taken rather than the IR alone because what a program links against
   * comes back beside its module and nowhere else (`reference/ffi.md § @link`). Taking the IR was
   * enough only while the driver carried one hardcoded library for every build.
   */
  private[sysl] def runIr(compiled: Either[String, Compiled], args: List[String],
                          archives: List[String] = Nil,
                          level: String = defaultOptimization): Either[String, (Int, String)] =
    runIrFully(compiled, args, archives, level).map { case (code, out, _) => (code, out) }

  /** The same, keeping the two streams apart.
   *
   * Almost everything that runs a program wants its output and does not care which descriptor it
   * came out of, which is what `runIr` above is for. What needs the distinction is anything
   * asserting about a *diagnostic*: `sysl.args` puts a usage error on standard error and its
   * `--help` on standard output precisely so the two can be redirected apart, and a test that
   * concatenated them could not tell whether that had happened.
   */
  private[sysl] def runIrFully(compiled: Either[String, Compiled], args: List[String],
                               archives: List[String] = Nil,
                               // **The level is a parameter because one feature's whole claim is
                               // about it.** `become` promises a tail call LLVM has to make, and at
                               // `-O1` the sibling-call pass makes one anyway — so a test that ran
                               // only at the default would pass on a compiler that had never emitted
                               // `musttail` at all.
                               level: String = defaultOptimization)
      : Either[String, (Int, String, String)] =
    compiled.flatMap { c =>
      val exe = createTempFile("sysl-", "")

      libraryObjects.flatMap { objects =>
        build(c.ir, exe, Target.default, archives, level, c.links, objects).map { _ =>
          val result = exec(exe :: args)
          deleteFile(exe)
          (result.exitCode, result.stdout, result.stderr)
        }
      }
    }

  /** The standard library's own C, compiled for this machine **once for the whole process** and put
   * on every link this runs.
   *
   * **It is here because this is the link, and there turned out to be three of them.** The driver
   * adds the library's tree to `NativeSources` (`Main`), `StdSelfTests` compiles it for its own
   * link, and then a guide program calling `sysl.posix.time.monotonic` failed to resolve the clock
   * shim through a *third* path nobody had thought about. Every one of those is "link a
   * program that was compiled against the library from source", so the knowledge belongs at the link
   * rather than at each caller, where the next harness would have missed it too.
   *
   * **Objects rather than an archive, and it costs nothing where they are not wanted.** An object is
   * linked whether or not anything needed it, so a program reaching none of the library's C carries
   * a few hundred unused bytes; an archive would be pulled on demand but would need `ar`, which this
   * path has no other reason to require. Where the standard module arrived as an **artifact** its
   * shims are archive members already, and a member is pulled only to resolve a symbol still
   * undefined — so the object here wins and the member is simply never reached, rather than the two
   * colliding.
   *
   * Compiled once because the alternative is once per test, and there are thousands of them.
   */
  private lazy val libraryObjects: Either[String, List[String]] =
    NativeSources.of(Std.root.toOption.toList, Target.default.os) match
      case Nil   => Right(Nil)
      case trees => NativeSources.build(trees, Target.default).map(_.objects)
}
