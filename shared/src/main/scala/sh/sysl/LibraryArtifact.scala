package sh.sysl

import io.github.edadma.cross_platform.{cacheDirectory, isFile, readBytes}

import java.nio.charset.StandardCharsets.UTF_8

/** A library as one **file**: the half that could be compiled ahead of time as an object file, and
 * the half that could not as a tree (`reference/modules.md § Separate compilation`).
 *
 * **The split is the whole design, and it is the one Rust's `.rlib` makes.** A declaration with no
 * type parameters is compiled once, by whoever ships the library, and a program that calls it just
 * links against the result. A **generic** has nothing to compile until a caller fixes its type
 * arguments, so it travels as the tree it was parsed into and is monomorphized in the consuming
 * program. Rust carries MIR in `lib.rmeta` for exactly this reason; we carry the AST, which is the
 * same decision one stage earlier.
 *
 * The metadata carries **every** declaration, not only the generic ones — a call into the
 * precompiled half still has to be type-checked against a signature, and the tree is where the
 * signature is. What the symbol list adds is which of those the consumer must *declare* rather than
 * emit a second time.
 *
 * **The container is a real `ar` archive**, which is what an `.rlib` is and for the same reason: the
 * linker already knows how to read one, so the object half needs no unwrapping and — the part a
 * container of our own could not buy — a member is pulled in only to resolve a symbol something has
 * left undefined. A library the program barely touches costs the linker the members it touched.
 *
 * The metadata rides inside it the way Rust's does: **wrapped in a real object file**, as one
 * `private` constant in a section of its own. That detail is not decoration. A raw member is
 * *silently dropped* by macOS's `ranlib`, and forcing the index off only moves the failure to the
 * linker, which reads every member and refuses with `archive member 'lib.smeta' not a mach-o file`.
 * Wrapped, the member is an object like any other; being `private` it exports no symbol, so nothing
 * ever gives the linker a reason to pull it in, and it costs the output nothing.
 *
 * **Reading it back needs no object-file parser.** The member is found by scanning for a magic and
 * the payload follows its own length — so the compiler reads metadata for targets whose object
 * format it could not begin to parse, which is every target it cross-compiles to.
 *
 * An artifact is **for one machine**, exactly as an `.rlib` is, because half of it is compiled code.
 * The tree half would travel anywhere; the object half is what pins it.
 */
object LibraryArtifact {

  /** What a library artifact is called. */
  val extension: String = ".syslib"

  /** What the metadata half is called where it stands alone. `rmeta` does the same job under the same
   * reasoning.
   */
  val metadataName: String = "smeta"

  /** The two members an artifact is made of: the metadata, and the compiled half.
   *
   * Named rather than left as whatever the temporary files happened to be called, so that an artifact
   * holds the same names wherever it was built — and so that `ar t` shows a reader something they can
   * make sense of. `metadataOf` prefers the metadata member by name, which is what keeps the marker
   * from being found in a library whose own source text happened to spell it out.
   *
   * The `.o` on both is not dressing. Each member **is** an object file, the metadata one included,
   * and that is exactly the property that makes the container survive the platform tools.
   *
   * **THE TWO MEMBERS DIFFER FOR DIFFERENT REASONS, SO COMPARE THEM SEPARATELY — a whole-container
   * `cmp` answers the wrong question and answers it confidently.** The metadata half embeds the
   * **absolute path of every source file**, so two artifacts built from the same sources in
   * different directories are **not** byte-identical and never were. Measured: one library built
   * from two worktrees of identical sources gave containers 288 bytes apart, entirely in this member
   * and in the archive symbol table whose offsets it moved, while the compiled half was identical
   * byte for byte. 97 embedded paths and a directory name three characters shorter is 291 bytes of
   * payload, with alignment absorbing the difference.
   *
   * So a difference here says where the artifact was built, and a difference in the compiled half
   * says the emitted code changed. Only the second is evidence about the compiler. To remove the
   * variable rather than reason about it, point both compilers at **one** source directory: same
   * paths, and any remaining difference is real.
   */
  val metadataMember: String = s"sysl.$metadataName.o"

  val codeMember: String = "sysl.code.o"

  /** The container's version, refused on mismatch. It is not `AstCodec.Version`: the layout here and
   * the tree encoding inside it change for different reasons, and either alone makes an artifact
   * unreadable, so each says so on its own.
   *
   * **It also covers the object code, which is why the ARC hook's shape moved it to 4.** A box's
   * hook now takes the phase it is being called for (`ArcEmitter.dropFn`), so a member compiled
   * before that installs a one-argument function where the runtime around it calls a two-argument
   * one. Nothing about that is a type error to a linker, and what it produces is a wrong call at the
   * moment an object is destroyed — the least observable place a mismatch could land.
   *
   * **5 adds the allocator the object half was built for.** An artifact from 4 records no pair, and a
   * reader that assumed libc's would be assuming the very thing it has to check — so the whole
   * container is refused instead, which is what a version is for.
   *
   * **A CHANGE TO `Type.mangleOne` BELONGS IN THAT LIST TOO, and it is the one likeliest to be
   * forgotten**, because both bumps above were changes to the *shape* of a call while this one is a
   * change to a **name**. Nothing bounds a mangled symbol today — the mangling is a structural
   * recursion with no threshold and no hash — so if one is ever added, or the scheme is otherwise
   * altered, an artifact built before it defines symbols the reader no longer asks for.
   *
   * It is worth being honest about how this differs from the two bumps above, so the analogy is not
   * over-read: those were **silent**, and a naming mismatch is **loud** — the link fails on an
   * undefined symbol rather than calling the wrong thing. The bump is still owed, for the same
   * reason and with less at stake.
   *
   * **6 is such a change.** A constrained type that checks its values now mangles under its own name
   * rather than its base's, so a library instantiating a generic at one defines `Buf.at.Small` where
   * an artifact from 5 defined `Buf.at.ulong`.
   */
  val Version: Int = 6

  /** The separator both byte formats here lean on: between the fields of a fingerprint, and around
   * the metadata marker.
   *
   * Built rather than written as a literal, and that is not fussiness. A NUL in the source makes the
   * *file* binary — `git diff` refuses to show it, and `grep` goes quiet on it — which costs every
   * future reader of this file far more than the constant is worth. It is the same character either
   * way; only the spelling differs.
   */
  private val Nul = 0.toChar.toString

  /** A fingerprint of the source a library was built from, carried in the artifact so that a
   * consumer can tell one built from *these* files from one built from different ones.
   *
   * **It is over the files' contents and their places in the library, never the paths they were
   * found at.** The same library is read from wherever it is installed — an installed compiler finds
   * it under its own prefix, a checkout finds it at `library/`, and `build-lib --std` is pointed at
   * whichever root the person running it typed. Those are all the same library and have to
   * fingerprint the same, or the guard this exists for would fire on every artifact ever built and
   * no two machines could share one. Sorting is for the same reason: a directory listing decides
   * nothing.
   *
   * A file's place is its **directory below the root** and its name, and not the name alone. A
   * library is a tree, so two of its modules may each hold a `read.sysl` — and keying by the name
   * would give them one key between them, at which point the sort is deciding by input order what
   * two files with one name hash as, and the same library fingerprints two ways depending on the
   * order it was read in. Which is precisely the guard failing open: a stale artifact that hashed
   * the other way would be accepted.
   *
   * FNV-1a rather than a cryptographic digest, because the question is *did these files change*, not
   * *did someone forge them* — and a `MessageDigest` is not on every platform this cross-builds to.
   * 64 bits, finished through `fmix64` and rendered to a fixed width, so the metadata's first line
   * is one shape whatever the hash comes out as.
   */
  def fingerprint(sources: List[Source]): String = {
    var h = 0xcbf29ce484222325L

    def mix(s: String): Unit =
      for c <- s do
        h = (h ^ c.toLong) * 0x100000001b3L

    def place(s: Source): String = (s.dir.getOrElse(Nil) :+ Project.basename(s.name)).mkString("/")

    for s <- sources.map(s => (place(s), s.text)).sortBy(_._1) do
      mix(s._1)
      mix(Nul)
      mix(s._2)
      mix(Nul)

    f"${avalanche(h)}%016x"
  }

  /** The `fmix64` finalizer, which is what makes a small edit change the whole fingerprint rather
   * than a corner of it.
   *
   * FNV-1a's own diffusion is uneven — its multiply carries a change upward but never back down, so
   * the low bits of the result lean on the low bits of the input in a way a mixing step is meant to
   * break. It costs three shifts and two multiplies once per library, which is nothing against
   * reading the files.
   *
   * This is MurmurHash3's finalizer as written there, **both rounds**. One round alone diffuses most
   * of the way and is the easy place to stop; the second is what its constant was chosen with.
   */
  private def avalanche(h: Long): Long = {
    var x = h

    x ^= x >>> 33
    x *= 0xff51afd7ed558ccdL
    x ^= x >>> 33
    x *= 0xc4ceb9fe1a85ec53L
    x ^= x >>> 33
    x
  }

  private val Magic = "syslib"

  /** Where a prebuilt standard module lives when nobody says otherwise: what `build-lib --std`
   * writes with no `-o`, and what a compilation looks for having been given no `--std-lib`.
   *
   * One path for both ends is the point — it is what makes the artifact something built once and then
   * never thought about again, rather than a file whose location has to be carried around on every
   * command line.
   *
   * Not committed, and deliberately: the object half is built for one machine while sysl
   * cross-compiles, and a second of build is a poor trade for a binary in the tree that must be
   * regenerated by hand on every edit to the library.
   *
   * **In the user's cache, keyed by the library it was built from.** This was `./.sysl/std.syslib`,
   * which is right for someone working *in a clone* — a worktree has its own `library/sysl` and therefore
   * its own artifact — and wrong for an installed compiler, whose library sits beside it in one place
   * however many directories it is run from. There the same 900KB artifact would be rebuilt and
   * stored once per directory anyone ever ran `sysl` in, and `sysl run notes.sysl` would leave a
   * `.sysl/` in whatever folder it was typed in.
   *
   * **The key names both the library and the compiler, because the artifact is a function of both.**
   * `Std.fingerprint` is over the library's file contents and their places in the tree, so a build
   * with an edited `library/sysl` gets a different path rather than a stale hit. That half alone is not
   * enough, and the way it is not enough is quiet: an artifact is *compiled* code, so a release that
   * changes what the library lowers to while touching none of its source produces different bytes at
   * an identical fingerprint. The upgrade then reads back the artifact its predecessor built and the
   * change does not take.
   *
   * That is not hypothetical — it is what 0.0.6 was about to do. 0.0.5 emitted a definition of
   * `sysl$stdout` into the object half that a program also emits for itself, which a Mach-O link
   * accepts and an ELF link refuses; the fix changed the compiler and left `library/sysl` byte-identical,
   * so every machine that had already run 0.0.5 would have gone on linking against the broken
   * artifact and seen nothing change.
   *
   * The version is the compiler's own, so two releases never share an entry and a rebuild costs the
   * second it takes. `Stdlib.read`'s fingerprint check still stands behind the library half; nothing
   * stands behind the compiler half but this, because an artifact does not record what built it.
   *
   * What this does *not* separate is two builds of the same version — a development tree, where the
   * compiler changes under a constant `BuildInfo.version`. Nothing in the cache can distinguish those
   * without hashing the compiler itself, and `--no-core-lib` is the answer there.
   *
   * Nothing here is ever evicted. Every distinct library leaves an artifact behind, which is what a
   * cache directory is for and why this belongs in one — the platform's own housekeeping knows to
   * look there, and everything under it is derived, so removing the lot costs a rebuild and nothing
   * else.
   *
   * **`./.sysl` remains the answer where there is no cache directory to be had** — a machine with no
   * home, which is a real state for a build container. The compilation then behaves exactly as it did
   * before, rather than failing over somewhere nobody can write.
   *
   * **The key names the TARGET too, and leaving it out cost a rebuild on every alternation.** An
   * artifact is object code for one machine and a tree parsed as that machine sees it (`Conditional`),
   * which is why `read` refuses one built for another target by name. Sharing one path between them
   * therefore never produced a wrong answer — it produced a *rebuild*: the cross build refused the
   * host's artifact and overwrote it, the next host build refused that one and overwrote it back, and
   * each announced itself on stderr in the words of a fault. Keying by target makes them separate
   * entries that both stay warm, which is the whole point of a cache keyed by anything.
   *
   * **And the ALLOCATOR, for exactly that reason.** The object half calls the pair by name, so a
   * program that allocates through `pvPortMalloc` and one that allocates through `malloc` need
   * different artifacts — and `read` refuses the wrong one, which without a key of its own is the
   * rebuild-and-overwrite loop above rather than a diagnostic. It costs nothing to add: the key
   * already names the compiler version, so every entry is new in the release this ships in.
   *
   * **`fingerprint` is what the caller COMPILED, and it is not the same question as what the caller
   * RESOLVED.** A compilation names the library it found and leaves this empty, which asks `Std`;
   * `build-lib --std` names the tree it was pointed at, which need not be that library at all. They
   * were one call before, so `build-lib` in a checkout with an installed sysl wrote the checkout's
   * library under the installed library's key — the one thing a fingerprint in a cache key exists to
   * stop.
   *
   * **And HOW it was compiled — the level and whatever `Pipeline` asked of LLVM (`codegen`).** The
   * archive is object code, and object code is what `-O` and `-flto` change: before the key named
   * them, every build linked whichever archive the first build on the machine had made, which was
   * the default level's, so a `-O3` build and an LTO build both linked an `-O1` standard module
   * that took no part in link-time optimization — and said nothing, since the link succeeded.
   */
  def stdDefault(target: Target, allocator: Allocator = Allocator.c,
                 fingerprint: Option[String] = None,
                 level: String = Toolchain.defaultOptimization,
                 pipeline: Pipeline = Pipeline.none): String =
    cacheDirectory
      .map(c => s"$c/sysl/${BuildInfo.version}-${fingerprint.getOrElse(Std.fingerprint(target.os))}" +
        s"-${target.name}-${allocator.alloc}-${allocator.free}-${codegen(level, pipeline)}/std$extension")
      .getOrElse(stdLocal)

  /** The part of the standard module's key that says how its object code was made: the level, then
   * each lever `pipeline` pulls — `O2`, `O3-lto-thin`, `O2-lto-thin-puse-<hash>`.
   *
   * **A profile is keyed by what it SAYS, not by where it is.** `--profile-use` names a file, and
   * the ordinary way of working is to merge a new profile over the old one at the same path — so a
   * key over the path would hand the second build an archive optimized for the first profile. The
   * file's bytes are hashed instead, which is a read of one file per build against a rebuild of the
   * whole standard module. A file that is not there hashes as its path alone; clang then refuses it,
   * in its own words, when the archive is built.
   *
   * `--profile-generate` is keyed by its directory, because the directory is what the instrumented
   * objects are told to write into.
   */
  def codegen(level: String, pipeline: Pipeline): String =
    s"O$level" +
      pipeline.lto.map(mode => s"-lto-$mode").getOrElse("") +
      pipeline.profileGenerate.map(dir => s"-pgen-${digest(Project.absolute(dir).getBytes(UTF_8))}")
        .getOrElse("") +
      pipeline.profileUse.map { file =>
        val where = Project.absolute(file)
        val body  =
          try if isFile(where) then readBytes(where) else Array.emptyByteArray
          catch case _: Exception => Array.emptyByteArray

        s"-puse-${digest(where.getBytes(UTF_8) ++ body)}"
      }.getOrElse("")

  /** FNV-1a over bytes, finished as `fingerprint` finishes — for a key segment rather than a
   * library, so sixteen hex digits and no framing.
   */
  private def digest(bytes: Array[Byte]): String = {
    var h = 0xcbf29ce484222325L

    for b <- bytes do
      h = (h ^ (b & 0xffL)) * 0x100000001b3L

    f"${avalanche(h)}%016x"
  }

  /** The project-local artifact path, which is what `stdDefault` was before it moved to the cache and
   * what it falls back to where a machine has no cache directory.
   *
   * It is **not** keyed by target, because it is one file in one project rather than a cache with
   * room for entries: a machine without a cache directory gets the behaviour it had before the cache
   * existed, including a rebuild when the target changes.
   */
  val stdLocal: String = s".sysl/std$extension"

  /** Whether a path names an artifact rather than a source tree. This is what lets `--lib` take
   * either without a second flag — how a library shipped is not something a program that depends on
   * it should have to write down.
   */
  def isArtifact(path: String): Boolean = path.endsWith(extension)

  /** What a library compiles to: the object code for its determined half, and the metadata a
   * consuming compilation reads.
   *
   * Every file is parsed before any is rejected, so one syntax error does not hide the rest — the
   * same rule the ordinary compilation follows.
   *
   * **The library is analyzed before anything is written.** A library that does not check is broken
   * once, by whoever built it; without this the artifact ships anyway and every program that links
   * against it is handed a diagnostic pointing into somebody else's source. `main` is optional
   * (`reference/modules.md § Where a program starts`), so a library having none is not a complaint.
   */
  /** **A `c const` is lowered here, before anything else touches the trees**, which is what puts the
   * measured value into the artifact rather than the C expression that produced it. A consumer then
   * needs neither a clang nor the library's headers to read the constant — and could not honestly be
   * given the expression anyway, since an artifact is built for one target and re-evaluating it
   * somewhere else would be answering a different question under the same name.
   */
  /** **`libraries` and `libraryTrees` are what this library is built ON** — the `--lib` source
   * roots and `.syslib`s of `reference/ffi.md § A library may carry C`, which reach a library
   * exactly as they reach a program. They are kept apart from `sources` for one reason and it
   * decides the artifact's correctness: what the object half **defines** is the modules this tree
   * declares, and everything else is declared. Folded in with the library's own files they would be
   * *its* modules, so the artifact would carry a second copy of its dependency's compiled half —
   * which builds, archives, and fails as a duplicate symbol at whatever program later links both.
   */
  def build(sources: List[Source], target: Target = Target.default, building: Set[String] = Set.empty,
            std: Option[Stdlib] = None, native: List[Source] = Nil,
            paths: SearchPaths = SearchPaths.none, libraries: List[Source] = Nil,
            libraryTrees: List[Program] = Nil, allocator: Allocator = Allocator.c)
      : Either[String, (String, String)] = {
    val parsed = (sources ::: libraries).map(SyslParser.parse(_, target))

    parsed.collect { case Left(e) => e } match
      case errs if errs.nonEmpty => Left(errs.mkString("\n"))
      case _ =>
        val (own, supplied) = parsed.collect { case Right(p) => p }.splitAt(sources.length)

        // Both halves are lowered, and with the same search paths. A `--lib` source root is a
        // library like any other and may hold a `c const` of its own; lowering it here rather than
        // inside the analyzer is what gets `--include-path` to the C compiler that has to answer it.
        // Only the library's own units go on to the metadata below — a dependency's constants are
        // its own artifact's to ship.
        CProbe.lower(supplied, target, paths).left.map(_.rendered).flatMap(carriedSource =>
        CProbe.lower(own, target, paths).left.map(_.rendered).flatMap(units =>
        rootless(units) match
          case Some(err) => Left(err)
          case None =>
            // The C files are fingerprinted with the sysl ones and not apart from them. A library's
            // shims are as much its source as its modules are, and an artifact that did not change
            // when one of them was edited is a stale artifact nothing would notice was stale.
            //
            // **The fingerprint is over every file, test files included**, while the metadata below
            // is over the shipping ones — and the two are allowed to disagree because a fingerprint
            // is a question about the *source a library was built from*, which is what decides
            // whether an artifact is stale. `Stdlib.read` compares it against `Std.fingerprint`,
            // computed the same way over the same tree, so editing a library's tests correctly
            // invalidates the artifact even though nothing in it would have changed.
            //
            // Only the metadata is filtered, and it is filtered because that is what a consumer
            // reads: the ASTs travel so a generic can be instantiated in the program that fixes its
            // type arguments, and shipping a test file's would leave a package's scaffolding
            // nameable by everything that links it.
            //
            // The compiled half needs no filtering here because `compileLibrary` never analyzed the
            // tests in the first place (`Tests.stripSource`). That is the one build whose drop comes
            // *before* the analysis, and the reason is that analyzing a test body creates generic
            // instantiations, which are ordinary library functions afterwards — so a filter applied
            // here, however thorough, would be too late to keep them out.
            Compiler.compileLibrary(units, target, building, std, libraryTrees ::: carriedSource,
                                    allocator)
              .map((ir, compiled) =>
                // **`stripSource` rather than `filterNot(_.testOnly)`**, so that what an artifact
                // carries is exactly what `Stdlib.fromSource` carries. The two differ on one shape: a
                // `@test` written inside an *ordinary* file, which `testOnly` does not see because the
                // file itself is not scaffolding. Any difference between them is a difference between
                // the two ways a standard module reaches a compilation, which is the one thing
                // `StdArtifactTests` exists to refuse.
                (ir, metadata(Tests.stripSource(units), compiled,
                              fingerprint(sources ::: native), target, allocator)))))
  }

  /** What one of a library's C files is called inside the archive.
   *
   * Named after the path it was found at, with the directories kept, because a member name has to be
   * unique across the whole library and a basename is not: two modules may each hold a `util.c`, and
   * `ar r` **replaces by name**, so the second would silently evict the first and the library would
   * ship missing whatever only the first defined.
   */
  def nativeMember(source: Source): String =
    (source.dir.getOrElse(Nil) :+ Project.basename(source.name)).mkString(".").stripSuffix(".c") + ".o"

  /** Why a library's members cannot all live in one archive, if they cannot.
   *
   * Two ways it can happen, and both would otherwise produce an artifact that builds and is wrong
   * rather than a build that fails. Two C files can map to one member name only by sitting at the
   * same path, which cannot happen — but a directory named `sysl` holding a `code.c` maps to the
   * name the library's own compiled half uses, and that one evicts the code the whole library is.
   */
  def collisions(native: List[Source]): Option[String] = {
    val reserved = Set(codeMember, metadataMember)
    val named    = native.map(s => nativeMember(s) -> s.name)
    val clashing = named.filter((member, _) => reserved(member))
    val repeated = named.groupBy(_._1).collect { case (m, ss) if ss.length > 1 => (m, ss.map(_._2)) }

    if clashing.nonEmpty then
      Some(clashing.map((m, file) =>
        s"$file would be archived as '$m', which is the name the library's own compiled half uses — " +
          "rename it, or move it out of a directory called 'sysl'").mkString("\n"))
    else if repeated.nonEmpty then
      Some(repeated.map((m, files) =>
        s"${files.mkString(" and ")} would both be archived as '$m'").mkString("\n"))
    else None
  }

  /** Why a library may not sit in the anonymous root module.
   *
   * Two reasons, and either alone would be enough. A library is reached by **naming** its module
   * (`reference/modules.md § Imports`), and the root module has no name — so a declaration there is
   * one no program that depends on the library could write. And the root module is where a
   * headerless *program's* declarations go, so the two would share a key space: the library's
   * `double` and the program's would be one name, and which of them a call meant would depend on
   * what else was linked.
   *
   * It is also what makes the split above exact. Everything the compiler supplied is keyed outside
   * the library's own modules, and that stops being true the moment a library is allowed to declare
   * one of them.
   */
  private def rootless(units: List[Program]): Option[String] =
    units.find(u => Compiler.moduleOf(u) == Modules.root).map(u =>
      s"a library is reached by naming its module, and ${u.source.name} is in none — " +
        "put the library's files in a directory under the root it is built from")

  /** The one library whose source the compiler also carries: its own standard module.
   *
   * Building it is the only compilation that is allowed to declare a module the library supplies,
   * and it is what `sysl build-lib --std` passes. Everything else about the build is the same —
   * which is the claim worth making, since a standard library that needed its own toolchain would
   * not be evidence that the toolchain works.
   */
  def std: Set[String] = Library.modules.toSet

  /** The metadata blob: the machine it was built for, the source fingerprint, how many symbols the
   * object half defines, those symbols, then the tree.
   *
   * **The target is recorded because the tree half is now a per-target answer** (`Conditional`): a
   * library may gate on the machine, so two artifacts built from one source are two different sets of
   * declarations. Before that the trees were the same whatever the artifact was built for, and the
   * only target-specific part was the object half — which a linker refuses on its own. A tree
   * mismatch has nothing to refuse it, and would surface as a diagnostic about a missing name in
   * somebody else's library.
   */
  private def metadata(units: List[Program], compiled: Set[String], source: String,
                       target: Target, allocator: Allocator): String = {
    val names = compiled.toList.sorted

    (target.name :: s"${allocator.alloc} ${allocator.free}" :: source ::
      names.length.toString :: names).mkString("", "\n", "\n") + AstCodec.encode(units)
  }

  /** The byte string the metadata is found by, inside an object file nothing else can read.
   *
   * NUL-delimited rather than plain text because the only bytes of an artifact a user chooses are the
   * compiled half's string literals, and a library whose own source held the words `SYSL-METADATA`
   * would otherwise be a library whose metadata could not be found. A NUL is not something a literal
   * arrives at by being written; it has to be escaped deliberately, and the whole sequence with it.
   */
  private val Marker = s"${Nul}SYSL-METADATA$Nul"

  /** The metadata framed for burial in an object file: the marker, the format version and the
   * payload's length in bytes, then the payload.
   *
   * The length is in **bytes** and not characters, and this is the whole reason the frame exists: the
   * payload carries the library's own source text, which is UTF-8, and the reader has an object file's
   * worth of unrelated bytes on either side of it. Counting characters would end the metadata
   * somewhere inside the text.
   */
  def frame(meta: String): Array[Byte] =
    framed(s"$Magic $Version ${meta.getBytes(UTF_8).length}", meta)

  /** A frame built from a header given whole, which is how a test reaches the ones `frame` will not
   * produce — a length that overruns, a version from another compiler, a header that is not one.
   *
   * The marker comes from here rather than being spelled out again in the suite. Re-spelling it would
   * leave those fixtures describing a container nothing writes the moment the real one changed, and
   * they would go on passing: a reader that rejects a frame it has never seen rejects a fixture that
   * is merely out of date in exactly the same way.
   */
  private[sysl] def framed(header: String, body: String = ""): Array[Byte] =
    (Marker + header).getBytes(UTF_8) ++ Array[Byte](0) ++ body.getBytes(UTF_8)

  /** The IR for an object file whose only content is the framed metadata.
   *
   * Going through `clang` is what makes this one piece of code rather than a Mach-O writer, an ELF
   * writer and a COFF writer — the compiler already requires a clang that can lower for the target,
   * and lowering a constant array into a named section is the least it can be asked to do.
   *
   * `private` is load-bearing twice over. It exports no symbol, so the linker never has a reason to
   * pull this member out of the archive; and a symbol here would be one every artifact defined, which
   * is a duplicate the moment two of them are linked into one program. `llvm.compiler.used` is what
   * keeps a global nothing refers to from being optimized away without putting a symbol back: it is
   * dropped before the object is written, so it holds the constant through the optimizer and leaves
   * nothing behind for the linker to see.
   */
  def metadataIr(meta: String, target: Target): String = {
    val blob     = frame(meta)
    val escaped  = blob.map(b => f"\\${b & 0xff}%02x").mkString
    // Named rather than written out, so this hand-built IR reaches the same registry the emitters
    // do — it is the one place in the compiler that writes a module's text instead of building it.
    val held     = Llvm.compilerUsed.name
    val metadata = Llvm.metadataSection.name

    s"""@sysl.metadata = private constant [${blob.length} x i8] c"$escaped", section "${section(target)}"
       |@$held = appending global [1 x ptr] [ptr @sysl.metadata], section "$metadata"
       |""".stripMargin
  }

  /** Where the metadata sits in the object that carries it. Mach-O names a section by its segment as
   * well, and the others do not; nothing reads it back by name, so this only has to be a section name
   * the assembler for the target accepts and nothing else claims.
   */
  private def section(target: Target): String =
    target.os match
      case Os.MacOS => "__DATA,__sysl_meta"
      case _        => ".sysl_meta"

  /** The metadata read back out of an artifact, or why the file is not one of ours.
   *
   * **The member we named is looked at first, and every member after it.** Each half of that earns
   * its place. Preferring the name is what stops the marker being found somewhere it was never put:
   * the compiled half holds the library's own string literals, and a library whose source spelled the
   * marker out would otherwise have its metadata read out of the middle of its own text. Falling back
   * to a scan is what survives an archiver that recorded the name differently than we wrote it —
   * which both long-name conventions have been seen to do.
   *
   * `Ar` parses the names properly rather than leaning on the scan to cover for it. A reader that
   * worked only because it ignored a bad parse is a reader that stops working for its own reasons.
   */
  def metadataOf(name: String, archive: Array[Byte]): Either[String, String] =
    Ar.members(archive).left.map(why => s"$name is not a sysl library: $why").flatMap { members =>
      val marker       = Marker.getBytes(UTF_8)
      val (ours, rest) = members.partition(_.name == metadataMember)

      (ours ::: rest).iterator.map(m => (m.body, indexOfSlice(m.body, marker))).collectFirst {
        case (body, at) if at >= 0 => unframe(name, body.drop(at + marker.length))
      }.getOrElse(Left(s"$name is not a sysl library: it carries no metadata"))
    }

  /** The framed payload, checked and decoded. */
  private def unframe(name: String, framed: Array[Byte]): Either[String, String] = {
    val end    = framed.indexOf(0.toByte)
    val header = if end < 0 then "" else new String(framed.take(end), UTF_8)

    header.split(' ') match
      case Array(Magic, version, length) =>
        (version.toIntOption, length.toIntOption) match
          case (Some(v), _) if v != Version =>
            Left(s"$name was built by a different sysl (library format $v, this one reads $Version) " +
              "— rebuild it with 'sysl build-lib'")
          case (Some(_), Some(len)) if len >= 0 && end + 1 + len <= framed.length =>
            Right(new String(framed.slice(end + 1, end + 1 + len), UTF_8))
          case _ => Left(s"$name is a truncated sysl library")
      case _ => Left(s"$name is not a readable sysl library: its metadata header is damaged")
  }

  /** Where one byte string starts inside another, or `-1`. Written out because `Array` offers
   * `indexOfSlice` only through a conversion that copies, and this runs over every member of an
   * archive that is mostly compiled code.
   */
  private def indexOfSlice(haystack: Array[Byte], needle: Array[Byte]): Int = {
    var at = 0

    while at + needle.length <= haystack.length do {
      var i = 0

      while i < needle.length && haystack(at + i) == needle(i) do i += 1

      if i == needle.length then return at

      at += 1
    }

    -1
  }

  /** The modules an artifact carries, the symbols its object half defines, and the fingerprint of
   * the source it was built from.
   *
   * **An artifact built for another machine is refused here**, and refused rather than handed back
   * for the caller to check, because every caller wants the same rule — the same reason
   * `Target.named` refuses a target it cannot lower for. The tree half is a per-target answer since
   * a library may gate on the machine (`Conditional`), and the wrong one decodes perfectly: it is
   * simply a different set of declarations, which is the worst way for this to fail. The object half
   * would be refused by the linker eventually, in a message about object formats that says nothing
   * about which library or why.
   *
   * The fingerprint is handed back rather than checked, because only the caller knows what it should
   * be: a library's own source is not something the compiler has, while the standard module's is
   * exactly what it carries. `Stdlib.read` is where that comparison belongs.
   *
   * **An artifact built for another allocator is refused here too**, and for a sharper reason than the
   * target: a mismatched target is refused by the linker eventually, and a mismatched allocator is
   * refused by nothing at all. Both pairs resolve, the link succeeds, and the program hands one
   * heap's storage back to another every time an object this artifact allocated is released. Checked
   * against a *name* rather than by inspecting the object half, because that is what a program can be
   * told to do about it — rebuild the library.
   */
  def read(name: String, meta: String, target: Target, allocator: Allocator = Allocator.c)
      : Either[String, (List[Program], Set[String], String)] = {
    val firstEnd = meta.indexOf('\n')
    val built    = if firstEnd >= 0 then meta.take(firstEnd) else ""
    val afterTgt = meta.drop(firstEnd + 1)

    val pairEnd = afterTgt.indexOf('\n')
    val pair    = if pairEnd >= 0 then afterTgt.take(pairEnd) else ""
    val rest0   = afterTgt.drop(pairEnd + 1)

    val declared = pair.split(' ') match
      case Array(a, f) if Allocator.isSymbol(a) && Allocator.isSymbol(f) => Some(Allocator(a, f))
      case _                                                            => None

    val header  = rest0.indexOf('\n')
    val source  = Option.when(header > 0)(rest0.take(header))
    val rest    = rest0.drop(header + 1)
    val newline = rest.indexOf('\n')
    val count   = Option.when(newline >= 0)(rest.take(newline)).flatMap(_.trim.toIntOption).filter(_ >= 0)

    (source, count, declared) match
      case (Some(fingerprint), Some(n), Some(pair)) if built == target.name && pair == allocator =>
        val body  = rest.drop(newline + 1)
        val lines = body.linesWithSeparators.take(n).toList
        val tree  = body.drop(lines.map(_.length).sum)

        AstCodec.decode(tree).left
          .map(e => s"$name is not a readable sysl library: $e")
          .map((_, lines.map(_.stripLineEnd).toSet, fingerprint))

      case (Some(_), Some(_), Some(_)) if built != target.name =>
        Left(s"$name was built for $built and this is a build for ${target.name} — " +
          s"rebuild it with --target ${target.name}")

      case (Some(_), Some(_), Some(pair)) =>
        Left(s"$name allocates through ${pair.alloc} / ${pair.free} and this program allocates " +
          s"through ${allocator.alloc} / ${allocator.free} — a program has one heap, so a library " +
          "compiled against another cannot be linked into it. Rebuild the library against this one")

      case _ => Left(s"$name is not a readable sysl library: its metadata header is missing")
  }
}
