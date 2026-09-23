package sh.sysl

import io.github.edadma.cross_platform.*

/** The standard module **this** compilation is compiled against, and the one place that says where
 * its trees came from.
 *
 * Every program is compiled against the library (`reference/modules.md § Separate compilation`),
 * and there has been one way to be handed it: `Std.parsed` — the files under `library/sysl`, read
 * off disk and parsed. That is what a *source* dependence on the standard module is, and it is what
 * the artifact exists to end: every program re-derives every signature in the standard module from
 * text before it can check its own first line.
 *
 * A `.syslib` carries those same trees already decoded, and — for the half with nothing left to
 * monomorphize — the object code to link against rather than emit a second copy of. Which of the two
 * a compilation gets is this value.
 *
 * **It is a parameter rather than an ambient fact**, for the reason a `Target` is: that a library is
 * installed beside the compiler is not the same claim as this compilation being compiled against
 * it. Making it a parameter is also what lets the two be run side by side and their
 * results compared, which is the only way to establish the thing the artifact has to be true for —
 * that it *means* what the source means.
 *
 * **Only three questions turn on which standard module it is**, which is why this is a small type. What trees
 * the library contributes, which sources are its rather than the program's, and which declarations
 * are its. Everything else the compiler knows about the library — the module's name, the key a
 * declaration is filed under, the spelling a key renders as — is true of the standard module however
 * it arrived, and lives in `Library`.
 */
final class Stdlib(val units: List[Program]) {

  /** The files these trees came from. A `Source` compares by identity (`Diagnostics`), so this is
   * an identity set: a user file that happened to be called `library/sysl/display.sysl` is not one of
   * these, and neither is the decoded copy of a file an artifact carries under the same name.
   */
  private val own: Set[Source] = units.map(_.source).toSet

  /** The modules these trees declare — the library's own, as *this* compilation was handed it.
   *
   * A program may name any of them and may declare none of them, and both are questions about what
   * is actually there: a compilation given a stand-in library has whatever that one declares, and
   * one given none has nothing to collide with at all.
   */
  lazy val modules: List[String] = units.map(Compiler.moduleOf).distinct.sorted

  /** Whether `module` is one of the library's own here. */
  def carries(module: String): Boolean = modules.contains(module)

  /** The library's files, as units for the walk to read on the same terms as the program's.
   *
   * They *are* files of modules, so what a name in one of them resolves against is what its own
   * header and its own imports say — not one scope standing in for the whole library. That was
   * exact while the library was a single module importing nothing, and stops being so the moment it
   * has more than one: a file of `sysl.sys` naming `Buf` is naming another module's declaration, and
   * has to say so the way any other file would.
   *
   * `building` is the modules this compilation is **producing** rather than being supplied with,
   * which is how the library's own source gets compiled at all: pointed at `lib`, the compiler would
   * otherwise hand a copy of `sysl` to the files that declare it, and every name in them would be
   * declared twice. It is per file rather than all-or-nothing so that the answer stays right for a
   * build producing some of the library's modules and not others.
   */
  def contributed(building: Set[String]): List[Program] =
    units.filterNot(u => building(Compiler.moduleOf(u)))

  /** Whether a declaration is the library's rather than the program's.
   *
   * Asked of the **position** rather than of the module, though the two now agree: a `Source` is a
   * stronger answer than a name, since a user file that happened to sit at `library/sysl/display.sysl`
   * is not one of these.
   */
  def owns(decl: Positioned): Boolean = decl.pos.exists(p => own(p.source))

  /** Every declaration the library carries. */
  def decls: List[Stmt] = units.flatMap(_.body)
}

object Stdlib {

  /** Which standard module a compilation is to be given.
   *
   * The three ways are not variations on a setting — they are three different sources, and which one
   * a compilation used decides what its diagnostics can say and whether anything is linked. They are
   * a type rather than a set of flags so that a caller cannot ask for two at once: naming an
   * artifact *and* refusing all of them is a command line with no reading, and it is refused by the
   * driver today with a message saying exactly that.
   */
  enum Choice {

    /** Compile the library's source into the program. What `--no-std-lib` asks for, and what
     * `build-lib --std` has to use, since it is the command that produces the artifact.
     */
    case FromSource

    /** A prebuilt artifact somebody named. Never rebuilt and never fallen back from — someone who
     * wrote down which artifact to use is owed the truth about that one.
     */
    case Artifact(path: String)

    /** The path both ends agree on, built there when what is there is not one. `search` overrides
     * where that is; `None` means the default, which is keyed by a fingerprint of the library and so
     * cannot be computed until the library has been found.
     */
    case Default(search: Option[String] = None)
  }

  /** A standard module a compilation can be given: the trees to compile against, the symbols its
   * object half already defines, and the archive to link them from.
   *
   * The archive is `None` where there is nothing to link — the library was compiled in rather than
   * linked, so its bodies are in the program's own IR.
   */
  case class Resolved(std: Stdlib, precompiled: Set[String], archive: Option[String])

  /** The standard module for a target, however the caller wants it found.
   *
   * **This is the entry point anything embedding the compiler wants**, and it is here rather than in
   * the driver because it is not a property of a command line: a test suite, a documentation
   * harness and `sysl build` all need the same answer to the same question, and answering it three
   * ways is how the three drift.
   *
   * Every branch reads the library's source — two compile it and the third checks a prebuilt
   * artifact against a fingerprint of it — so a compiler that cannot find its library fails here,
   * once, with the diagnostic naming where it looked, rather than at whichever branch happened to
   * touch `Std.sources` first, where it would arrive as an exception.
   */
  def resolve(choice: Choice, target: Target, allocator: Allocator = Allocator.c,
              cc: Option[String] = None, level: String = Toolchain.defaultOptimization,
              pipeline: Pipeline = Pipeline.none): Either[String, Resolved] =
    Std.root.flatMap: _ =>
      choice match
        case Choice.FromSource      => Right(Resolved(fromSource(target, cc), Set.empty, None))
        case Choice.Artifact(named) => load(named, target, allocator)
        case Choice.Default(search) =>
          // **The level and the pipeline are in the default path**, so a build at another level or
          // with LTO resolves — and, the first time, builds — an archive of its own rather than
          // linking one compiled for somebody else's flags (`LibraryArtifact.codegen`).
          val path = search.getOrElse(LibraryArtifact.stdDefault(target, allocator, level = level,
                                                                 pipeline = pipeline))

          resolved.synchronized {
            resolved.get((path, target, allocator)) match
              case Some(answer) => answer
              case None         =>
                val answer = found(path, target, allocator, cc, level, pipeline)

                resolved.clear()
                resolved((path, target, allocator)) = answer
                answer
          }

  /** The default path's answer, kept — for the reason `fromSource`'s copy is kept, and with the same
   * lock: a caller that resolves repeatedly would otherwise decode an unchanged artifact each time,
   * and a suite that compiles hundreds of programs is exactly such a caller.
   *
   * **Only the default path is memoized, and the key is what makes that safe.** The path is
   * `<cache>/sysl/<fingerprint>/std.syslib`, so a change to the library changes the *path* rather
   * than the file — a stale answer under one key is not reachable. A **named** artifact is
   * deliberately not kept: someone who wrote down which artifact to use may be rebuilding it, and
   * answering from memory would hand them the one they replaced.
   *
   * **One answer is kept, not one per key**, and this is the largest of the three single-slot memos
   * because a `Resolved` carries a whole decoded `Stdlib`. The key has a target in it and the path
   * has the target in it too, so a test walking `Target.all` produced an entry per target and held
   * every one of them.
   */
  private val resolved =
    collection.mutable.Map.empty[(String, Target, Allocator), Either[String, Resolved]]

  /** How many answers are held, so a test can pin the bound. See `Std.cachedTargets`. */
  private[sysl] def cachedResolutions: Int = resolved.synchronized(resolved.size)

  /** The monitor the memo above is guarded by.
   *
   * **It is exposed so that a test asserting two resolutions share an instance can make its own
   * precondition true.** One answer is kept and a miss clears it, so *any* resolution of a different
   * key between two calls evicts the first — and in a parallel suite run that is a property of the
   * scheduler rather than of `resolve`. A test that does not hold this fails a few runs in a hundred
   * and passes the rest, which is worse than one that fails always. The monitor is reentrant, so
   * `resolve`'s own `synchronized` nests inside a caller already holding it.
   */
  private[sysl] def memo: AnyRef = resolved

  /** The standard module at the path both ends agree on, **built there when what is there is not
   * one.**
   *
   * The artifact is a *derived* file: it is object code for one machine, it is not committed, and
   * every byte of it is computed from the library source the compiler already carries. So the states
   * it can be found in — absent after a clone or a fresh worktree, and stale after any change to the
   * tree encoding or the container — are not questions to put to whoever ran the compiler. They have
   * one answer, the compiler can produce it in well under a second, and it is the same answer every
   * time.
   *
   * **This is not the silent fallback the design refuses**, and the distinction is the whole of why
   * it is allowed. What is refused is compiling against a *different* standard module than the one
   * asked for — answering "I could not find your library" by quietly using another. A rebuild
   * answers with **this** library: the sources are the ones the compiler carries, `read` holds the
   * result to `Std.fingerprint` on the way back in, and a program compiled after it is compiled
   * against exactly what it would have been compiled against had the artifact been there. Nothing is
   * substituted, so there is nothing for a reader to be misled about.
   *
   * It says so on stderr rather than doing it invisibly. The work is a second of someone's time and
   * the line is what makes a slow first build explicable instead of mysterious.
   *
   * **A rebuild that cannot happen reports the original problem, not its own.** Without a toolchain
   * there is no artifact to make, and what the reader needs then is the sentence naming the command
   * and the flag — the same one they would have got before — with the reason the compiler could not
   * do it for them appended.
   *
   * **Except where the reason is that there is no compiler at all, and then those two suggestions are
   * worse than nothing.** Both need the very thing that is missing: `build-lib --std` reaches the same
   * toolchain, and `--no-std-lib` compiles the library's source into the program and still has to link
   * it. Offering them buries the one sentence that would fix the problem inside two that cannot,
   * wrapped in a complaint about the standard module — which is exactly the misreading Android's
   * missing NDK produces, since a toolchain fault there already looks like a broken library.
   */
  private def found(path: String, target: Target, allocator: Allocator,
                    cc: Option[String], level: String, pipeline: Pipeline): Either[String, Resolved] = {
    val already = if isFile(path) then load(path, target, allocator) else Left(s"$path does not exist")

    already match
      case Right(got) => Right(got)
      case Left(why) =>
        Console.err.println(s"building the standard module at $path ($why)")

        writeArtifact(path, target, allocator = allocator, cc = cc, level = level, pipeline = pipeline) match
          case Right(_)  => load(path, target, allocator)
          case Left(err) => Left(rebuildFailure(err, Toolchain.findClang(target, cc)))
  }

  /** What to say when the rebuild above did not happen: the toolchain's own sentence where there is
   * no compiler for this target, and the two suggestions otherwise.
   *
   * **The toolchain is asked rather than inferred from the message.** What makes the advice wrong is
   * that this machine cannot compile for this target at all, which is `findClang`'s question — not
   * something to recognize by the shape of a string, which would go on matching after the wording
   * moved and would misclassify any other failure that happened to mention a compiler.
   *
   * Separate from `found` so that both branches can be asserted on a machine where every registered
   * target does have a clang, which is every developer machine and the CI runner.
   */
  private[sysl] def rebuildFailure(err: String, toolchain: Either[String, String]): String =
    toolchain match
      case Left(noToolchain) => noToolchain
      case Right(_) =>
        s"cannot find or build the standard module — build it with 'sysl build-lib library --std', " +
          s"or pass --no-std-lib to compile against the copy built into the compiler ($err)"

  /** A prebuilt standard module read back: the trees to compile against, the symbols its object half
   * already defines, and the archive to link that half from — which is the file itself, since it is
   * already the archive the linker wants.
   *
   * Every way this can go wrong — a file that is not ours, one built by another sysl, one built from
   * other sources than the compiler carries, a tree that will not decode — is a failure of the same
   * kind as not finding it at all, and is reported rather than worked around. A standard module that
   * cannot be read is not a standard module.
   */
  private def load(path: String, target: Target, allocator: Allocator): Either[String, Resolved] = {
    val bytes =
      try readBytes(path)
      catch case e: Exception => return Left(s"cannot read $path: ${e.getMessage}")

    LibraryArtifact.metadataOf(path, bytes).flatMap(meta =>
      read(path, meta, target, allocator).map((std, symbols) => Resolved(std, symbols, Some(path))))
  }

  /** The library **parsed from its source**, as a given target sees it — the standard module the
   * long way round, and what an unusable artifact at the default path is rebuilt *from* rather than
   * replaced by (`Main.foundStd`). Reached directly only by `--no-std-lib`.
   *
   * The source is `Std.sources`, read off disk from wherever this compiler's library is installed
   * (`Std.root`). It was generated into the binary once and this was called `embedded`; the name
   * went with the mechanism, because a compilation that reads files can fail to find them and one
   * that unpacked a string could not.
   *
   * The target is a parameter for the reason it is one everywhere else, and here it is not merely
   * consistency: the library may gate on the machine (`Conditional`), so a copy parsed for one
   * target is not the library another target has.
   */
  /** **One target's copy is kept**, for the reason `Std.parsed` keeps one — a caller loops over a
   * target rather than alternating, and a test that iterates `Target.all` otherwise leaves every
   * target's standard module resident for the life of the process.
   *
   * **The library's own tests are stripped before anybody compiles against it**, which is what
   * `@tests` promises and what this path was quietly not doing. `Std.parsed` reads every file in
   * `library/`, `tests.sysl` included; handed to a compilation as the standard module, those files are
   * ordinary declarations — nameable, and worse, *instantiable*. A generic named only by a test
   * helper was being monomorphized into every program that compiled against the source std, so the
   * library shipped instantiations no caller had asked for.
   *
   * `LibraryArtifact` already drops them (`Tests.stripSource`, and
   * `reference/attributes.md § What is dropped, and when` on why the drop has to come *before*
   * analysis rather than after), so this was also the one difference between the two ways a
   * standard module reaches a compilation — which is exactly what `StdArtifactTests` compares. It
   * surfaced as an emitted-type *order* difference rather than a missing type, because the leaked
   * instantiation was one some later library function asks for anyway: it simply arrived earlier on
   * the path that could see the tests. A test file that named a type nothing else used would have
   * been a plain divergence instead.
   */
  /** **The library's `c const` blocks are measured here too, and this was the third call site to need
   * saying so.** `Analyzer.analyze` lowers a program's blocks and `LibraryArtifact.build` lowers a
   * library's on the way into an artifact; this is the path that reads the library *as source*, and
   * without the lowering a measured constant is not a constant — so an `@assert` over one is refused
   * for not being a constant expression, and the refusal names a library file the program's author
   * did not write. `AstCodec` already says a block reaching it means a path skipped `CProbe.lower`;
   * this is the same rule one layer earlier.
   *
   * **The strip comes first**, so a `@tests` file is never probed: it is dropped either way, and
   * asking the C compiler about a file nothing will compile is a clang invocation for nothing. The
   * units that survive are lowered identically to the artifact path's, which is what keeps the two
   * comparable — `StdArtifactTests` is what would notice if they were not.
   *
   * A probe that fails is `Std.parsed`'s kind of failure rather than a diagnostic: the standard
   * module is the compiler's own, so a header it cannot read is a broken installation and not
   * something a program did.
   */
  def fromSource(target: Target, cc: Option[String] = None): Stdlib =
    cache.synchronized {
      cache.get((target, cc)) match
        case Some(std) => std
        case None =>
          val units = CProbe.lower(Tests.stripSource(Std.parsed(target)), target,
                                   SearchPaths(cc = cc)) match
            case Right(lowered) => lowered
            case Left(e)        => sys.error(s"the standard module's 'c const' could not be measured: $e")

          val std = new Stdlib(units)

          cache.clear()
          cache((target, cc)) = std
          std
    }

  /** Locked for the reason `Std.parsed`'s is: this was a `lazy val`, which is initialized once
   * however many threads reach it, and a bare mutable `Map` is not.
   */
  private val cache = collection.mutable.Map.empty[(Target, Option[String]), Stdlib]

  /** Whether a second ask for one target answers from memory, decided without releasing the lock
   * between the two asks — `Std.memoAnswersTwice`'s reason, for the slot one layer up.
   */
  private[sysl] def memoAnswersTwice(target: Target): Boolean =
    cache.synchronized { fromSource(target) eq fromSource(target) }

  /** A standard module read out of the metadata half of an artifact (`LibraryArtifact`).
   *
   * The trees arrive already decoded, which is the whole point, and nothing downstream can tell
   * them from parsed ones — that is what `AstCodec` is for, and what the equivalence test holds it
   * to. The symbols the artifact's object half already defines come back beside it: they are what a
   * program **declares** rather than defines a second time, and they belong to the caller that has
   * a linker to hand them to.
   *
   * **An artifact built from a different `library/sysl` is refused here**, which is the one check a
   * consumer of the *standard* module can make and a consumer of any other library cannot: the
   * compiler has the source this was supposed to be built from, so it can say whether it was
   * (`Std.fingerprint`). Without it the artifact is built separately and can drift — and a stale one
   * decodes and links perfectly well, it is simply the wrong library, which is the worst way for
   * this to fail. Refusing puts it on the path a corrupt one already takes: at the default path it is
   * rebuilt from the library's own source, and where `--std-lib` named it the refusal stands.
   */
  def read(name: String, metadata: String, target: Target, allocator: Allocator = Allocator.c)
      : Either[String, (Stdlib, Set[String])] =
    LibraryArtifact.read(name, metadata, target, allocator).flatMap((units, precompiled, source) =>
      if source == Std.fingerprint(target.os) then Right((new Stdlib(units), precompiled))
      else
        Left(s"$name was built from a different standard module than this compiler's — " +
          "rebuild it with 'sysl build-lib <library root> --std'"))

  /** The standard module compiled to an artifact at `out`, from **this** compiler's library source —
   * the other end of `read`, and what an unusable artifact at the default path is rebuilt by.
   *
   * The sources are `Std.sources` and not the `library/` in whatever tree the command was run from,
   * which is what makes this dependable: `read` checks a decoded artifact against `Std.fingerprint`,
   * the fingerprint of exactly these files, so building from them is the one thing guaranteed to
   * produce an artifact this compiler will accept. Point it at another library and you get an
   * artifact it refuses, which is `build-lib --std`'s business rather than this routine's.
   *
   * No C files are gathered, and none can be missed: `reference/ffi.md § A library may carry C`
   * lets a library carry `.c` beside its modules, and the standard module carries none. It could
   * not — a `.c` under `library/sysl` would be hashed into the fingerprint of an artifact built by
   * `build-lib`, while `Std.sources` collects only sysl files, so every artifact built from the
   * tree would fail the check it is read back through.
   *
   * `ar` names the archiver where it is somewhere a search would not look, exactly as `--ar` does;
   * given none, the search runs.
   *
   * **The archive is assembled under another name and renamed onto `out`**, which is what makes the
   * default path safe to share. Two compilations of the same library compute the same fingerprint
   * and so name the same artifact, and there is nothing keeping them apart: separate worktrees at
   * one commit, or two runs of the same suite. `ar` truncates its output before it writes, so a
   * build that wrote in place would leave every concurrent reader a file that is briefly absent and
   * then briefly half an archive. Published by rename, a reader sees the whole of the previous
   * artifact or the whole of the new one — and a build that fails leaves the one that was already
   * there, rather than deleting a working artifact on its way to not producing one.
   */
  def writeArtifact(out: String, target: Target, ar: Option[String] = None,
                    allocator: Allocator = Allocator.c,
                    cc: Option[String] = None, level: String = Toolchain.defaultOptimization,
                    pipeline: Pipeline = Pipeline.none): Either[String, Unit] =
    for
      archiver <- Toolchain.findAr(ar)
      built    <- LibraryArtifact.build(Std.sources(target.os), target, LibraryArtifact.std,
                                        Some(fromSource(target, cc)), native = Std.cSources(target.os),
                                        paths = SearchPaths(cc = cc), allocator = allocator)
      _        <- {
                    val staging  = createTempDirectory("sysl-std-")
                    val code     = s"$staging/${LibraryArtifact.codeMember}"
                    val metadata = s"$staging/${LibraryArtifact.metadataMember}"

                    // **The library's own C, one member each** (`reference/ffi.md § A library may
                    // carry C`), exactly as `build-lib` stages a library's. `build` is handed the
                    // same files but only *fingerprints* them — archiving is the caller's, and this
                    // caller had nothing to archive until the library carried C. What that cost was
                    // an artifact the compiler builds for itself on a machine with a cold cache,
                    // holding `sysl.fs$entries` and not the shim it calls: everything compiled, and
                    // the *program* failed to link on a symbol from the standard library.
                    val objects = Std.cSources(target.os)
                      .map(s => s -> s"$staging/${LibraryArtifact.nativeMember(s)}")

                    Project.parentOf(out).foreach(Project.makeDirectories)

                    // Beside its destination rather than in the staging directory, because a rename
                    // is only a rename within one filesystem and the system's temporary directory
                    // need not be on the same one as the cache. The unique part of the name is the
                    // staging directory's own, which the system has already made unique — two
                    // builds racing for one artifact must not agree on a pending name either, or
                    // one of them would publish the other's half-written archive.
                    val pending = s"$out.${Project.basename(staging)}"

                    val outcome =
                      for
                        // **The code at the build's own level, with its own pipeline** — under LTO
                        // that makes it bitcode, which the link then optimizes together with the
                        // program instead of taking as a finished object.
                        _ <- Toolchain.compileObject(built._1, code, target, level, cc, pipeline)
                        // **The metadata is never bitcode.** It is read back by finding its marker in
                        // the member's bytes (`LibraryArtifact.metadataOf`), and a bitstream does not
                        // keep a string constant's bytes contiguous — so it is an ordinary object
                        // whatever the code beside it is.
                        _ <- Toolchain.compileObject(LibraryArtifact.metadataIr(built._2, target), metadata,
                                                     target, named = cc)
                        // Each C file is its own member, so the linker pulls a shim in the way it
                        // pulls anything else in: because something left its symbol undefined.
                        _ <- objects.foldLeft[Either[String, Unit]](Right(()))((so_far, entry) =>
                               so_far.flatMap(_ =>
                                 Toolchain.compileC(entry._1.name, entry._2, target, level,
                                                    named = cc, pipeline = pipeline)))
                        _ <- Toolchain.archive(code :: metadata :: objects.map(_._2), pending, archiver)
                        _ <- publish(pending, out)
                      yield ()

                    (code :: metadata :: objects.map(_._2) ::: List(staging, pending))
                      .foreach(Project.discard)
                    outcome
                  }
    yield ()

  /** The rename itself, as an answer rather than an exception: everything else in the build reports
   * what went wrong by returning it, and a full disk or a read-only cache directory is the ordinary
   * way this step fails.
   */
  private def publish(pending: String, out: String): Either[String, Unit] =
    try Right(moveFile(pending, out))
    catch case e: Exception => Left(s"cannot put the standard module at $out: ${e.getMessage}")
}
