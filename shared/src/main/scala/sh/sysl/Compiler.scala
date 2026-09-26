package sh.sysl

/** The pure front-to-IR pipeline: a program's source to an LLVM IR module, or the first error.
 *
 * Everything here is platform-independent and cross-compiles to JVM, JS, and Native — the
 * parts that touch the filesystem and invoke a toolchain live in the JVM CLI.
 *
 * A compilation is **for** a target (`getting-started/cli.md § targets`), which is a parameter and
 * not an ambient fact: the machine the compiler is running on reaches this only as the default
 * `Target.default` picks, and a caller that names one is building for it whether or not it could
 * run the result.
 *
 * A compilation also allocates from **one** pair of C functions (`reference/ffi.md § interrupt`),
 * and that pair is a parameter for the same reason: it is settled by the packages a program depends
 * on, which is a fact the driver holds and no pass here could work out. `Allocator.c` is libc's,
 * which is what a program depending on nothing that says otherwise gets.
 */
/** What one compilation produced: the module, whatever the driver may want to tell the user about
 * it, and what the result has to be linked against.
 *
 * `links` is here rather than inside the IR because it is not a property of the code — it is a
 * property of the *build*, and the only pass that can answer it is the one holding every unit that
 * went in, the standard module and any decoded library included. A driver that had the IR alone
 * could not work it out by reading it: an `extern` says which symbol it wants and never which
 * library has it, which is the whole reason `reference/ffi.md § @link` exists.
 */
/** `exports` is the lowered tree's exported functions, which is what a C header is written from
 * (`reference/ffi.md § @export`). It is carried here rather than recomputed because the two would
 * then be two answers to one question: a header naming a function the object does not define, or
 * missing one it does, is exactly the failure a C project cannot diagnose — it links, and calls
 * something that is not there.
 */
/** `module` is the compilation's result **as data** (`ir.Module`) and `ir` is that written down.
 *
 * **The constructor is private and the factory below is the only way to build one**, so the text is
 * always the printing of the data beside it. A driver hands `ir` to clang and a back end that is
 * not LLVM reads `module`; the point of the two being one value is that they cannot be two answers.
 */
case class Compiled private (ir: String, notes: List[String], links: List[String],
                             exports: List[TFunc], module: sh.sysl.ir.Module,
                             warnings: List[Diagnostic])

object Compiled {

  def apply(module: sh.sysl.ir.Module, notes: List[String], links: List[String],
            exports: List[TFunc], warnings: List[Diagnostic]): Compiled =
    Compiled(sh.sysl.ir.Printer.module(module), notes, links, exports, module, warnings)

  def apply(module: sh.sysl.ir.Module, notes: List[String], links: List[String]): Compiled =
    Compiled(sh.sysl.ir.Printer.module(module), notes, links, Nil, module, Nil)
}

object Compiler {

  /** Compiles source text to an LLVM IR module, or to the first error as a rendered diagnostic.
   * `name` is what a diagnostic calls the source — a path from the driver, a placeholder from a
   * test — and it is carried by every position the front end records.
   */
  def compileToLlvm(source: String, name: String = "<input>", target: Target = Target.default,
                    allocator: Allocator = Allocator.c)
      : Either[String, String] =
    compile(List(Source(name, source)), target, allocator)

  /** Compiles the files of one program, however many modules they make up. Each file says which
   * module it contributes to, the files of one module share a single scope
   * (`reference/modules.md`), and a module reaches another's members by naming them in full
   * (`reference/modules.md § Imports`) — so the order the files are handed over in decides nothing
   * but which one a diagnostic is reported against first.
   */
  def compile(sources: List[Source], target: Target = Target.default,
              allocator: Allocator = Allocator.c): Either[String, String] =
    compiled(sources, target, allocator).map(_.ir)

  /** The same compilation, starting from trees that are **already parsed**.
   *
   * This is the seam a library artifact enters through (`AstCodec`): a decoded tree is the tree the
   * parser would have produced, so everything from here on is unchanged and no pass has to know
   * whether a declaration was read from source or from an artifact.
   */
  def compileTrees(units: List[Program], target: Target = Target.default,
                   allocator: Allocator = Allocator.c): Either[String, String] =
    rendered(analyzed(units, target, Set.empty, Stdlib.fromSource(target), allocator = allocator,
      own = ownModules(units))).map(_.ir)

  /** Compiles a program **against a library**: the library's modules are compiled alongside it, and
   * the program reaches them by the ordinary module rules (`reference/modules.md § Imports`) — a
   * full path, or an `import`.
   *
   * The library arrives as trees rather than as source because that is what it will be: an
   * `AstCodec` artifact, decoded. Nothing downstream distinguishes the two, which is the property
   * worth having — a library is not a second kind of input, it is more modules.
   *
   * A library declares no statements of its own; the one file that carries a program's statements
   * (`reference/modules.md § Where a program starts`) is still the program's, and a library that carried any would be reported by the same
   * rule that reports two of them.
   */
  def compileWith(sources: List[Source], libraries: List[Program],
                  target: Target = Target.default,
                  allocator: Allocator = Allocator.c): Either[String, String] =
    compiledWith(sources, libraries, target, allocator = allocator).map(_.ir)

  /** The same compilation against a library, keeping the notes the driver may want to show. This is
   * the one the CLI takes, so that a program linked against a library reports its heap promotions
   * exactly as one compiled alone does.
   */
  /** `librarySources` is a library given as **source** — a `--lib` root or a fetched package — which
   * is more modules exactly as `libraries` is, and is separate from `sources` for one reason: which
   * modules are the program's own is only knowable here, and `Reachability.contributing` needs it.
   *
   * Concatenated in front, which is the order the driver used when it did this itself, so which file
   * a diagnostic is reported against first does not move.
   */
  def compiledWith(sources: List[Source], libraries: List[Program], target: Target = Target.default,
                   precompiled: Set[String] = Set.empty, std: Option[Stdlib] = None,
                   provides: Set[String] = Capability.core.toSet,
                   packages: Packages = Packages.none, entryPoint: Boolean = true,
                   paths: SearchPaths = SearchPaths.none, allocator: Allocator = Allocator.c,
                   librarySources: List[Source] = Nil)
      : Either[String, Compiled] = {
    val supplied = librarySources.map(SyslParser.checked(_, target))
    val parsed   = sources.map(SyslParser.checked(_, target))

    rendered((supplied ::: parsed).collect { case Left(e) => e } match
      case Nil =>
        treesChecked(parsed.collect { case Right(p) => p },
          libraries ::: supplied.collect { case Right(p) => p }, target, precompiled, std,
          provides, packages, entryPoint, paths, allocator)
      case errs => Left(errs.flatten))
  }

  /** The same compilation from the program's own **already-parsed** units.
   *
   * `compiledWith` is this with a parse in front of it, and the two are one method split rather than
   * two paths: a caller that had to change the trees between parsing and compiling — to supply the
   * `main` a body has no room for (`Bodies`) — would otherwise have had nowhere to stand.
   */
  def compiledTrees(units: List[Program], libraries: List[Program] = Nil,
                    target: Target = Target.default, precompiled: Set[String] = Set.empty,
                    std: Option[Stdlib] = None, provides: Set[String] = Capability.core.toSet,
                    packages: Packages = Packages.none, entryPoint: Boolean = true,
                    paths: SearchPaths = SearchPaths.none, allocator: Allocator = Allocator.c)
      : Either[String, Compiled] =
    rendered(treesChecked(units, libraries, target, precompiled, std, provides, packages,
      entryPoint, paths, allocator))

  /** `compiledTrees`, answering its refusal as data. It is the shared body rather than a second
   * road: `compiledTrees` is this plus `Diagnostic.report`, and `compiledWith` is a parse in front
   * of it.
   */
  private def treesChecked(units: List[Program], libraries: List[Program], target: Target,
                           precompiled: Set[String], std: Option[Stdlib], provides: Set[String],
                           packages: Packages, entryPoint: Boolean, paths: SearchPaths,
                           allocator: Allocator)
      : Either[List[Diagnostic], Compiled] =
    analyzed(libraries ::: units, target, precompiled, carried(std, target), provides, packages,
      entryPoint, paths, allocator, ownModules(units))

  /** Which modules this compilation is **building** rather than being handed.
   *
   * **The standard module is handed exactly as a `--lib` root is**, and that is the whole of why
   * this no longer asks whether anything else was. It arrives as a `Stdlib` rather than in `units`,
   * so a set computed from the presence of *other* libraries left every compilation without one
   * saying `None` — and `None` means every module here is the program's own, which made a library
   * declaration nothing reaches an unconditional root. `Reachability.contributing` states the rule
   * this feeds and the reason it is one rule for all four kinds.
   *
   * What that cost, before this: a `sysl.fs` destructor is emitted into `print(1)`, and on a
   * freestanding target it brings a `declare` for `fclose` with it — from a module whose own header
   * requires `os`, which that target does not provide. The same shape `Capabilities` closed for a
   * `--lib` module's `requires`, one road over.
   *
   * Read off the module headers, which is what `compileLibrary` and `typedWith` already do — the
   * directory has to agree and the analyzer is what holds it to that, so a header is the answer
   * before anything is analyzed.
   *
   * It stays an `Option` because `None` is still meaningful to everything downstream — it is what a
   * caller with nothing to say passes, and `Reachability.prune` and `Analyzer.analyze` both default
   * to it. Nothing on a driver's path takes that default any more.
   */
  private def ownModules(units: List[Program]): Option[Set[String]] =
    Some(units.map(moduleOf).toSet)

  /** The same compilation stopped at the **typed tree**, which is what `sysl prove` reads
   * (`reference/verification.md § sysl prove`).
   *
   * It stops before pruning and before lowering, and both matter. A function nothing calls is still
   * one the program declared and still one somebody may want proved; and a `@ghost` declaration is
   * dropped from the emitted module by design (`reference/verification.md § @ghost — what costs
   * nothing to say`), so a proof run reading the lowered tree would have lost exactly the
   * predicates the specification is written in.
   *
   * **`paths` is here for the reason it is on `compiledWith`, and it used to be missing.** Stopping
   * before lowering does not stop before *analysis*, and a `c const` block is evaluated by running
   * the C compiler over the file's `@include`s during analysis (`CProbe`). So a proof run over a
   * project binding an installed library needs the include paths exactly as a build does — without
   * them it failed with `'uv.h' file not found` on a project that builds, and `--include-path` was
   * ignored rather than insufficient, since there was nothing here for it to reach (card `0325`).
   */
  def typedWith(sources: List[Source], libraries: List[Program], target: Target = Target.default,
                std: Option[Stdlib] = None, provides: Set[String] = Capability.core.toSet,
                paths: SearchPaths = SearchPaths.none)
      : Either[String, (TProgram, Set[String])] = rendered {
    val parsed = sources.map(SyslParser.checked(_, target))

    parsed.collect { case Left(e) => e } match
      case Nil =>
        val mine = parsed.collect { case Right(p) => p }

        // **Which modules are the program's own travels back beside the tree**, because nothing in
        // the tree says. `TProgram.mainModule` names the file that carries the statements, and a
        // module of pure declarations carries none — so a proof run over a library-shaped file would
        // have found nothing to translate. The sources given are what the reader meant by "this
        // module", and they are only known here — which is the same fact the analyzer is handed, for
        // the same reason.
        Analyzer.analyze(libraries ::: mine, std = carried(std, target), target = target,
                         provides = provides, paths = paths, own = ownModules(mine))
          .map((_, mine.map(moduleOf).toSet))
      case errs => Left(errs.flatten)
  }

  /** The standard module a compilation was handed, or the copy the compiler carries **for the target
   * it is building for**.
   *
   * Spelled as an option rather than as a defaulted parameter because the fallback depends on
   * `target`, and a default argument cannot read another parameter of its own list. Which is the
   * honest shape anyway: "none was given" and "this particular one was" are different facts, and
   * only the first has an answer that varies with the machine.
   */
  private def carried(std: Option[Stdlib], target: Target): Stdlib = std.getOrElse(Stdlib.fromSource(target))

  /** The same compilation, keeping the notes the driver may want to show — currently the heap
   * promotions, for `--explain-escapes` (`05`). Separate from `compile` so that the ordinary path
   * has nothing extra to ignore.
   */
  def compiled(sources: List[Source], target: Target = Target.default,
               allocator: Allocator = Allocator.c)
      : Either[String, Compiled] = rendered(checked(sources, target, allocator))

  /** The same compilation, answering **every diagnostic as data** rather than as a paragraph.
   *
   * This is the compiler's structured entry point, and `api.Sysl.check` is one step on top of it.
   * A caller here gets the whole list — the five-diagnostic limit is `Diagnostic.report`'s rule and
   * not the list's, because an editor underlining a file wants every mistake in it and five would
   * leave the rest unmarked.
   */
  def checked(sources: List[Source], target: Target = Target.default,
              allocator: Allocator = Allocator.c)
      : Either[List[Diagnostic], Compiled] = {
    val parsed = sources.map(SyslParser.checked(_, target))

    // Every file is parsed before any is rejected, so a syntax error in one does not hide the
    // syntax errors in the rest — the same reason the analyzer reports every mistake it finds.
    parsed.collect { case Left(e) => e } match
      case Nil =>
        val mine = parsed.collect { case Right(p) => p }

        analyzed(mine, target, Set.empty, Stdlib.fromSource(target), allocator = allocator,
          own = ownModules(mine))
      case errs => Left(errs.flatten)
  }

  /** The same compilation as a **test build**: the IR whose entry point dispatches to one `@test`
   * function by name, and the tests it can be asked for (`getting-started/cli.md § test`).
   *
   * This is the one compilation that keeps the tests and drops the program — `Tests.only` says why —
   * and the tests come back beside the IR because the runner needs both: the binary to execute and
   * the list to execute it for. Working the list out twice, once here and once by reading the tree
   * again, is how the two come to disagree about what a test is called.
   *
   * `building` names the modules this compilation is **producing** rather than being handed, exactly
   * as `compileLibrary` means it. Without it a tree that declares `sysl` or one of its submodules is
   * read as a program *adding to* the library and collides with every declaration it holds, which is
   * what testing the standard library from its own source amounts to. It is the caller that says so
   * and never inferred, for the reason `ModuleFiles.checkLibraryModules` gives: a build that guessed
   * would turn a crisp refusal into a link-time collision.
   *
   * **`paths` is not optional in practice, whatever its default says.** A `c const` block is
   * evaluated by the C compiler, so a tree whose `@include` names a header outside the toolchain's own
   * search path cannot be analyzed without it — and a test build is a compilation like any other. It
   * was missing here until 0.0.46, which made `test` the one subcommand that could not compile a
   * package built on `c const`: the driver printed every `--include-path` it had been given and then
   * analyzed without them. The default stays because a dozen in-tree callers legitimately have no
   * paths to give.
   */
  def compileTests(sources: List[Source], libraries: List[Program], target: Target = Target.default,
                   precompiled: Set[String] = Set.empty, std: Option[Stdlib] = None,
                   building: Set[String] = Set.empty, paths: SearchPaths = SearchPaths.none,
                   allocator: Allocator = Allocator.c, librarySources: List[Source] = Nil,
                   devModules: Set[String] = Set.empty)
      : Either[String, (Compiled, List[TTest])] = rendered {
    val supplied = librarySources.map(SyslParser.checked(_, target))
    val parsed   = sources.map(SyslParser.checked(_, target))

    (supplied ::: parsed).collect { case Left(e) => e } match
      case errs if errs.nonEmpty => Left(errs.flatten)
      case _ =>
        val mine   = parsed.collect { case Right(p) => p }
        val handed = libraries ::: supplied.collect { case Right(p) => p }
        val units  = handed ::: mine
        val whole  = carried(std, target)

        for
          // **Before the analysis, because it is about what a *consumer* would compile.** A dev
          // dependency is in scope for the whole of this build, so an ordinary module importing one
          // type-checks here and is refused for everybody else (`Tests.checkDevImports`).
          _        <- Tests.checkDevImports(mine, devModules)
          typed    <- Analyzer.analyze(units, building, whole, target, paths = paths,
                        own = ownModules(mine))
          promoted <- Escape.check(typed)
          _        <- TailCalls.check(typed)
          _        <- TailJumps.check(typed, target)

          // **The same check `analyzed` runs, against the tree *this* build emits.** It was missing
          // here outright, which made every rule about an exported symbol silent under `sysl test` —
          // the loop a package's author actually runs. A source `sysl build` refused compiled and
          // ran, so `private`, `@ghost`, variadic, a symbol C could not name, two exports claiming
          // one symbol, and an export reaching computed module storage were all unreported there.
          //
          // `Tests.only` rather than `Tests.strip`, and that is the whole of the difference from
          // `analyzed`. The question is about the emitted program's symbol table, so what to read is
          // whatever *this* compilation emits — and a test build is the one build where a `@test`
          // file's `@export` is a definition rather than something dropped.
          _        <- Exports.check(Tests.only(typed, ownModules(mine)), target, ownModules(mine))
        yield
          val kept = Tests.only(typed, ownModules(mine))

          // A test binary is linked like any other, so it needs the same libraries. It is a
          // different compilation from the one above rather than a variant of it, which is why the
          // collection is repeated here instead of shared: what the two keep differs, and only what
          // they link is the same.
          (Compiled(Codegen.module(kept.copy(precompiled = precompiled), promoted, target, allocator),
                    promoted.explanations,
                    LinkDirectives.required(units ::: whole.units),
                    Nil, kept.warnings),
           kept.tests)
  }

  /** A **library** lowered on its own: the IR for everything in it that could be compiled ahead of
   * time, and the names of those functions.
   *
   * The split needs no analysis of its own, which is the pleasant part. A generic declaration is
   * kept untyped and monomorphized on demand, so analyzing a library alone yields typed functions
   * for exactly the declarations that were already determined — a generic nothing called is simply
   * not here. Those are what an object file can hold; the generics travel as trees and are compiled
   * in whatever program fixes their type arguments.
   *
   * Nothing is pruned. A program is lowered from `main` outwards because anything it cannot reach is
   * dead, but a library has no `main` and every public declaration is a potential entry — so all of
   * them are emitted, and it is the *linker* that discards what a given program never calls.
   *
   * **A library defines its own declarations and nobody else's.** The compilation is handed the
   * shipped library too — a library that prints reaches `printi` and `putbytes` exactly as a program
   * does — and emitting *those* would put a copy of the printing surface in every artifact, so two
   * libraries that both print could not be linked into one program. They are declared here and
   * defined in the consuming program, which compiles the shipped library anyway and reaches them
   * through the very body that called for them.
   *
   * Which is which is read off the **declaring module** each typed function records (`TFunc.module`),
   * never off its name. A library owns the functions its modules declared: its own top-level
   * functions, the members of every `impl` it wrote — a built-in type's included, so `char.is_digit`
   * is `sysl.text.ascii`'s though its key names no module — every instantiation of a generic it
   * defines, and every closure written in one of those bodies. A generic of the shipped library's
   * monomorphized for this one's sake belongs to the shipped library. It is exact only because a
   * library may not sit in the anonymous root module, which `LibraryArtifact.build` is what refuses.
   *
   * `building` names the shipped library's own modules where *this* is the compilation producing
   * them, so that the compiler does not supply the files it is being asked to compile.
   *
   * `libraries` is every **other** library this one is built on — the `--lib` roots and `.syslib`s
   * of `reference/ffi.md § A library may carry C`, already parsed. They join the compilation the
   * way the shipped library does and are governed by the paragraph above with nothing added: their
   * modules are not this tree's, so what they declare is declared here and defined in whatever
   * program links them both.
   */
  /** **The object half is built for one allocator**, exactly as it is built for one target: an
   * allocating function compiled here calls the pair by name, and a program that frees through a
   * different pair would be giving one heap's storage back to another. That is why an artifact
   * records the pair it was built with and `LibraryArtifact.read` refuses a mismatch — the alternative
   * is a link that succeeds and a heap that is quietly wrong.
   */
  def compileLibrary(units: List[Program], target: Target = Target.default, building: Set[String] = Set.empty,
                     std: Option[Stdlib] = None, libraries: List[Program] = Nil,
                     allocator: Allocator = Allocator.c)
      : Either[String, (String, Set[String])] = rendered {
    for
      // A library ships no tests. They are the library author's, they run against the sources rather
      // than against the artifact, and emitting them would put a function nothing can call into every
      // program that links it — with the helpers only it reaches dragged in behind.
      //
      // **Before the analysis rather than after it**, which is the one place a library differs from
      // every other build (`Tests.stripSource`). Analyzing a test body is enough to change what the
      // artifact holds: it monomorphizes whatever generics the test names, and an instantiation is an
      // ordinary function afterwards — so a strip made on the typed tree removes the test and ships
      // everything it caused.
      //
      // Nothing runs `strip` afterwards, and nothing needs to: conditional compilation is gating by
      // line and is over before the lexer, so a declaration is either at a file's top level or is
      // not a declaration. There is no branch a `@test` could be hiding inside for the typed pass to
      // catch, and a second removal that can never find anything reads as though there were.
      typed    <- Analyzer.analyze(Tests.stripSource(libraries ::: units), building,
                                   carried(std, target), target,
                                   own = ownModules(units))
      promoted <- Escape.check(typed)
      _        <- TailCalls.check(typed)
      _        <- TailJumps.check(typed, target)
    yield
      val mine = units.map(moduleOf).toSet

      // **A library owns the functions its modules declared, and the symbol name is not where that
      // is read.** Each typed function records the module that declared it (`TFunc.module`), so a
      // member of a built-in type — `char.is_digit`, `real.nan`, `constslice.byte.last_index_of_byte`
      // — is its `impl`'s module's, though its key names none, and a closure is the module's whose
      // body it was written in, though its key begins with `$`. Owned functions are emitted with
      // ordinary linkage and advertised below like any other, so a program linking the artifact
      // declares them and links against this copy.
      //
      // **Ownership and linkage are separate facts.** Both units call their fourth closure
      // `$closure4.call`, so a closure body — and every instantiation made at one — is `internal`
      // (`TFunc.internal`): the library's copy and the program's are two symbols, and neither is
      // advertised, since `determined` below leaves out whatever is internal.
      val (own, supplied) = typed.funcs.partition(f => mine(f.module))

      // A function that reads a module-level `val` is left out of the precompiled half, and this is
      // the honest boundary of what separate compilation reaches today: the storage for a `val` is
      // initialized by the entry point, and a library has none. Such a function is compiled in the
      // program instead, where the initialization it depends on actually happens. Everything else —
      // which is most of a library — is compiled once, here.
      //
      // **`build-c` took the other road for the same problem and this one stays as it is**, which is
      // what `TProgram.cArtifact` exists to keep apart (card `0263`). An archive is linked by a C
      // project that supplies its own `main`, so nothing is ever going to fill its storage and it
      // registers a constructor that does. A `.syslib` is linked by a sysl program, which has an
      // entry point and fills the storage there — so deferring is not a workaround here, it is the
      // cheaper answer, and a constructor emitted alongside would fill a copy nothing reads.
      //
      // **It has to be left out of what is EMITTED and not only out of what is advertised**, and
      // getting that wrong is a duplicate definition rather than a missing one. The program compiles
      // its own copy because nothing advertised it; if this object file defined it too, both
      // definitions reach the linker. `sysl.stdout` is the case that found this — it returns a trait
      // object built from a module-level `val`, so it is the one library function a program emits for
      // itself, and it was being emitted here as well.
      //
      // **A Mach-O link accepts that and an ELF link refuses it**, which is why it survived a suite
      // that runs on one of the two: `ld64` takes the first definition, `ld` reports `multiple
      // definition of 'sysl$stdout'` and stops. Nothing about the bug was macOS-specific — only the
      // consequence was.
      val (deferred, here) =
        own.partition(f => Reachability.reachedFrom(List(f), typed.funcs, typed.vtables).vals.nonEmpty)

      // **An `internal` deferred function is dropped outright rather than declared**, because the
      // emitter defines every `internal` function it is handed, precompiled or not — that is how a
      // program carries its own copy of a library's private one. Handed to it here, a file-private
      // function or a closure that reads a module `val` would be defined in the library after all,
      // naming deferred callees this object never defines — harmless only while the linker strips
      // the dead copy. Every caller of a deferred function is deferred too, so nothing emitted here
      // names it.
      val dropped = deferred.filter(_.internal).map(_.name).toSet
      val funcs   = typed.funcs.filterNot(f => dropped(f.name))

      val ir =
        Codegen.generate(
          typed.copy(funcs = funcs, entryPoint = false,
                     precompiled = (supplied ::: deferred).map(_.name).toSet),
          promoted, target, allocator)

      // **What is advertised is what the linker can reach**, so a file-private declaration is left
      // out however ordinary it looks here. Its symbol is emitted `internal` (`reference/modules.md
      // § Visibility`), which is a promise that every caller is in this module — and a program told
      // the artifact holds it would declare it, call it, and find nothing at the link. Left out,
      // the program compiles a copy of its own from the tree the artifact carries, which is what it
      // does with a generic.
      //
      // An `internal` function is still *emitted* above, unlike a deferred one: internal linkage is
      // what keeps the program's copy and this one from being one symbol, so there is no collision
      // for them to have.
      val determined = here.filter(f => !f.internal)

      (ir, determined.map(_.name).toSet)
  }

  /** The module a file contributes to, as its header says. The directory has to agree, and the
   * analyzer is what holds it to that; before anything is analyzed the header is what there is.
   */
  private[sysl] def moduleOf(u: Program): String = u.module.map(_.show).getOrElse(Modules.root)

  /** Every pass that reads a whole typed program runs before anything is dropped from it: a
   * declaration nothing can reach is still one the program declared, and checking it is what makes a
   * mistake in it a mistake at all. Pruning is therefore the last thing that happens to the tree, and
   * the only thing between the checks and the lowering.
   *
   * `own` has no default, unlike everything else optional here: it is what qualifies the roots a
   * handed module contributes (`Reachability.contributing`), the standard library is handed to every
   * one of these, and a default of `None` would be the answer that emits an unreached library
   * declaration into the program. Each caller says which modules are its own.
   */
  /** A structured outcome as the string-returning entry points answer it.
   *
   * Every public method here kept its `Either[String, ?]` signature when diagnostics became data,
   * because a caller wanting text has always wanted exactly this text — and because one of them is
   * read by `sysl.sh`'s `DocsTests`, in the other repository. `checked` is the one that answers the
   * list; the rest are this on top of it.
   */
  private def rendered[A](outcome: Either[List[Diagnostic], A]): Either[String, A] =
    outcome.left.map(Diagnostic.report)

  private def analyzed(units: List[Program], target: Target, precompiled: Set[String],
                       std: Stdlib, provides: Set[String] = Capability.core.toSet,
                       packages: Packages = Packages.none, entryPoint: Boolean = true,
                       paths: SearchPaths = SearchPaths.none, allocator: Allocator,
                       own: Option[Set[String]])
      : Either[List[Diagnostic], Compiled] =
    for
      // **`entryPoint` reaches the analyzer and not only the emitter.** It has always decided
      // whether a `main` is written; it also has to decide whether a lone top-level `var` is read as
      // a body's local, because a body nothing emits has no locals for it to be one of — see
      // `ModuleFiles.entryFile`.
      walked   <- Analyzer.analyze(units, std = std, target = target, provides = provides,
                    packages = packages, paths = paths, own = own, entryPoint = entryPoint)

      // **The analyzer takes the answer and does not record it, so the tree it hands back says
      // `true` whatever this build is.** That was invisible for as long as `Codegen` was the only
      // thing that asked — the `copy` below the `yield` set it on the way past — and it stopped
      // being invisible the moment a *check* needed it: `Exports.storage` reads it to ask whether
      // anything will fill this artifact's module storage, and read off the analyzer's default it
      // is asking about a program that is not being built. Set it once, here, so every pass below
      // sees the build it is actually part of.
      //
      // **`cArtifact` is `!entryPoint` HERE and only here**, because this path is reached with no
      // entry point by exactly one thing: `build-c` and the `emit-header` beside it (`Main.cLibrary`).
      // `build-lib` is the other build with no entry point and it does not come through here — see
      // `compileLibrary`, and `TProgram.cArtifact` for why the two must not share an answer.
      typed     = walked.copy(entryPoint = entryPoint, cArtifact = !entryPoint)
      promoted <- Escape.check(typed)
      _        <- TailCalls.check(typed)
      _        <- TailJumps.check(typed, target)

      // **`Exports.check` reads the tree this build is going to emit, which is the stripped one.**
      // Reading `typed` instead made an `@export` in a `@tests` file a definition in every build,
      // and its most visible effect was that a *package* could not carry a test-only export at all:
      // any consumer defining that symbol — as it must, when the symbol is one the package's C
      // demands of its application — was refused, and the diagnostic named the package's own test
      // file as the other definition. Only one of the two was ever emitted.
      //
      // `Escape` and `TailCalls` above are deliberately not moved with it. Both are about whether a
      // body is *correct*, and a `@test` that does not compile is an error in a build that would
      // never have run it — which is the whole of what makes it safe to leave a test beside the code
      // it tests. `Exports` is the odd one out because it asks a question about the emitted
      // program's symbol table rather than about a body.
      _        <- Exports.check(Tests.strip(typed), target, own)
    yield
      // Pruning still runs, and still from `main`: a library function this program never calls is
      // dropped from the tree exactly as before. What `precompiled` changes is only what happens to
      // the ones it *does* call — declared rather than defined, with the body coming from the
      // library's object file at link time.
      //
      // The tests go first, and they go **after** the analysis above rather than instead of it: a
      // `@test` that does not compile is an error in a build that would never have run it, which is
      // what makes it safe to leave one beside the code it tests (`Tests`).
      val pruned = Reachability.prune(Tests.strip(typed), own)

      // The std's units are asked as well as the program's, and this is what makes an artifact's
      // directives mean anything: the standard module arrives as `Stdlib` rather than in `units`,
      // so a collection that read only the latter would drop every directive the library ships
      // with. `entryPoint = false` is what makes a C-callable artifact possible at all
      // (`reference/ffi.md § @export`): the module is emitted with no `main`, so the C project
      // supplies its own and this object is something its linker takes rather than something that
      // wanted to be a program. It is the same switch a library build has always used, reached from
      // a second command.
      Compiled(Codegen.module(pruned.copy(precompiled = precompiled, entryPoint = entryPoint),
                              promoted, target, allocator),
               promoted.explanations,
               LinkDirectives.required(units ::: std.units),
               pruned.funcs.filter(_.exported.isDefined),
               pruned.warnings)
}
