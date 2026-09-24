package sh.sysl

/** What a *file* contributes to the module, and which file the program starts in.
 *
 * A module is a directory (`reference/modules.md`), so a header and a location are two statements
 * of one fact and have to agree. That is the first thing this answers. The second is `reference/modules.md § Where a program starts`'s: a
 * declaration is hoisted and belongs to its module, but a statement *runs*, and running happens in
 * an order that neither a set of files nor a graph of modules supplies — so one file carries the
 * statements the program runs, and everything about choosing it is here.
 *
 * Both are settled before a single name is resolved, which is why they sit below the driver rather
 * than inside it: what a file contributes decides what there is to hoist.
 */
trait ModuleFiles
    extends Hoisting
    with StmtAnalysis
    with SignatureVisibility
    with ModuleGraph
    with Capabilities
    with LinkRequirements
    with ConventionCheck
    with ExportCheck
    with NoAlloc
    with DeclCapabilities
    with Purity
    with Frames
    with TestScope
    with Ghost
    with GatedModules
    with InitOrder
    with DefaultParams {

  /** The module a file contributes to: what its header says, or the **anonymous root module** when
   * it declares none (`reference/modules.md`) — under the canonical prefix of the package it came
   * from (`reference/packages.md § What a dependency's modules are called`).
   *
   * The prefix is what keeps a dependency's `json` and this project's `json` apart, and it is added
   * *here* rather than being written in the file so that a package's source is the same source
   * whoever depends on it. A compilation with no dependencies has no prefixes, so this is the
   * header's own name and nothing has changed.
   */
  protected def moduleOf(u: Program): String =
    Packages.qualify(packages.prefixOf(u.source), u.module.map(_.show).getOrElse(Modules.root))

  /** A module is a directory, and its name is that directory's path relative to the project root
   * (`reference/modules.md`), so a file's header has to agree with where the file sits.
   *
   * The location is the driver's to know, and it hands it over on the `Source`. A file handed to
   * the compiler with no project around it carries none, and its header is then the whole of what
   * says which module it is in — which is how a single file, and the tests that compile a handful
   * of them directly, go on working with no project to be measured against.
   *
   * Checking the header against the *location* subsumes checking the files of a directory against
   * each other: they are each held to the same derived name, so a file that was edited without its
   * siblings is reported on its own line rather than as a disagreement with whichever sibling
   * happened to be read first.
   */
  protected def checkLocations(): Unit =
    for u <- units; dir <- u.source.dir do
      // Both sides are read **relative to the package's own root**, which is what lets a fetched
      // package be checked by the rule it was written under: its files say `module json` and sit in
      // `json/`, and the canonical prefix that keeps it apart from this project's `json` is added
      // above both of them rather than to one. `reference/modules.md` is a rule about a package,
      // not about a machine's directory layout, and a dependency would fail it for the wrong reason
      // otherwise.
      val expected = dir.mkString(".")
      val declared = u.module.map(_.show).getOrElse(Modules.root)

      if declared != expected then
        val theirs = if declared.isEmpty then "declares no module" else s"declares '$declared'"
        val here   = if expected.isEmpty then "sits at the project root" else s"sits in '$expected'"

        recover(())(at(u.module.flatMap(_.pos).orElse(u.body.headOption.flatMap(_.pos))) {
          err(s"${u.source.name} $theirs, but it $here — a module is a directory, so the two must agree")
        })

  /** Refuses a file claiming a module the library already carries.
   *
   * A module's declarations are one set however many files they came from (`reference/modules.md`),
   * so a program with a `sysl` directory of its own would not be writing a module beside the
   * standard one — it would be adding to it, sharing its key space, and shadowing whatever name it
   * happened to reuse. There is nothing in the file that distinguishes that from a mistake, and the
   * mistake is the far likelier reading, so it is a diagnostic rather than a silent merge.
   *
   * Unless the compilation is what **produces** that module, which is the one reading under which
   * the files are not adding to the library but are the library. Nothing infers it: a build says so,
   * because a build that guessed would turn this crisp refusal into a link-time collision.
   */
  protected def checkLibraryModules(): Unit =
    for u <- units; name = moduleOf(u) if std.carries(name) && !building.contains(name) do
      recover(())(at(u.module.flatMap(_.pos).orElse(u.body.headOption.flatMap(_.pos))) {
        err(s"'$name' is the module every program is compiled against, so ${u.source.name} cannot " +
          "declare it — its declarations would join the library's rather than sit beside them")
      })

  /** The file the program starts in, if it has one.
   *
   * A declaration is hoisted and belongs to the module it was written in, but an executable
   * statement runs, and running happens in an order. Files have none — a module's members are one
   * unordered set (`reference/modules.md § The module graph is acyclic`) — and neither do modules,
   * which are a graph rather than a sequence. So **one file of the program carries the statements
   * it runs**, and a second that carries any is a mistake rather than an ordering to be guessed at.
   *
   * An `import` is not executable: it binds a name for the file that wrote it and runs nothing, so a
   * file may import whatever it likes without becoming the file the program starts in. Neither is a
   * `const`, a `val`, or a function — all three are declarations, and a file carrying a table and
   * the functions that read it must not thereby become the file the program starts in.
   *
   * **A top-level `var` is the one form that cannot answer on its own, which is why this is two
   * passes rather than one filter** (`reference/modules.md § Where a program starts`). It is a binding with an initializer, so a file
   * carrying nothing else really is a body and its `var` really is a local — and it is equally
   * readable as storage the module owns, which is what it has to be in a file that names a module.
   * Nothing in the line chooses. What chooses is the rest of the program: **a file carrying a
   * statement that is not a binding is a beginning, and where one exists every other file's
   * top-level `var`s are the module's.** So the two readings never compete, and a program that has
   * no such file falls back to the second pass, where a lone binding is a body after all — which is
   * what keeps a one-file `var n = 1` meaning what it has always meant.
   *
   * **That fallback reaches only a file with no `module` header**, because a file that names a module
   * has no body for a binding to belong to instead (`reference/modules.md § Where a program starts`). Nothing else here needs the condition:
   * the first pass is about what a file *runs*, and a `print` runs whatever the header says.
   *
   * A program with none of either is a complete program that does nothing, which is what a tree of
   * pure declarations should compile to: a library is not an error.
   */
  /** Whether this compilation emits a beginning at all.
    *
    * False for `build-c` and `build-lib`, where the artifact is something else's to start: the C
    * project supplies its own `main`, and a library is linked into a program that has one. It is what
    * the fallback below asks before reading a lone `var` as a body's local, since a body that is
    * never emitted has no locals.
    */
  protected def hasEntryPoint: Boolean

  protected def entryFile(files: List[(Program, Scope)]): Option[(Program, Scope)] = {
    def declaresMain(u: Program) = u.body.exists { case d: FuncDecl => d.name == "main"; case _ => false }

    def carries(u: Program, what: Stmt => Boolean) = u.body.exists(s => !Bodies.isDeclaration(s) && what(s))

    // Reported against the first thing in `u` that made it a rival, which is the line the reader has
    // to move — not the file's first line, and not the winner's.
    def refuse(first: Program, u: Program, what: Stmt => Boolean): Unit =
      for s <- u.body.find(s => !Bodies.isDeclaration(s) && what(s)) do
        recover(())(at(s.pos) {
          err(s"${first.source.name} already carries the statements this program runs, so " +
            s"${u.source.name} may hold declarations only")
        })

    // A statement that is not a binding: one file may carry these, and a second is the mistake this
    // reports.
    files.filter((u, _) => carries(u, !Bodies.isTopLevelBinding(_))) match
      case (first, s) :: others =>
        for (u, _) <- others do refuse(first, u, !Bodies.isTopLevelBinding(_))
        Some((first, s))

      // Nothing runs, so the bindings decide — and only where one file carries them, and only where
      // that file could have had a body at all. **A file with a `module` header could not**: `reference/modules.md § Where a program starts`
      // says everything it declares is the module's already, so there is nothing there for a `var` to
      // belong to *instead*, and reading one as a local would be reading the header off the file.
      //
      // Without that condition a library was its own beginning. A package is one module, in files
      // that all name it, and the moment one of them held a `var` it was chosen here — after which
      // every function touching that `var` was a nested function of a body that does not exist, and
      // was refused for the things a nested function may not be: `private`, and generic. The module
      // compiled when a program imported it, because a real entry file won the pass above, and was
      // refused by `build-lib`, where there is no program. That is the whole of what a library build
      // is — files and no beginning — so the fallback had to be told which files could be one.
      //
      // Several is **not** the two-beginnings mistake: a program in which nothing runs has no
      // beginning for a second one to compete with, so each of those files is holding module storage
      // and the whole thing is a library that does nothing. Picking a winner among them would be
      // arbitrary, and would make one file's bindings local and invisible to the rest for no reason a
      // reader could see.
      //
      // **And only where this compilation HAS a beginning.** `build-c` and `build-lib` emit no
      // `main` — the C project or the program linking the artifact supplies its own — so there is no
      // body for a `var` to be a local of, and inventing one is not a smaller mistake than picking
      // the wrong file. What it cost: the chosen file's functions became *nested* functions of a body
      // that is never emitted, which renames them into an environment (`$env0.setit`) and drops the
      // `@export` on the way — so an archive came out with no entry point at all, one warning saying
      // the module exports nothing, and no error anywhere. Card `0167`.
      //
      // This is the same fix the `module` header condition below is, arrived at from the other side:
      // that one says a file which names a module cannot be a body, this one says a compilation with
      // no beginning has no body for any file to be.
      case Nil if !hasEntryPoint => None

      // **And only where the program has not already written its beginning as a `main`.** The
      // fallback exists to keep a one-file script's `var n = 1` a local of the script, but a `main` is
      // the other way of writing where a program starts, and once one is declared there is no body
      // left for a lone binding to belong to: the `var` is storage the module owns, exactly as a `val`
      // beside it is, and exactly as it is in a file that names a module. Reading it as a body instead
      // left the most ordinary program there is — a counter, the functions that bump it, a `main`
      // calling them — with no spelling that compiled: refused as a second beginning over its own
      // `main`, with every function reading the counter nested out of `main`'s reach and reported as
      // undefined, while `static`, the word for asking for the module, was refused for saying nothing
      // in a file with no body.
      case Nil if files.exists((u, _) => declaresMain(u)) => None

      case Nil =>
        files.filter((u, _) => u.module.isEmpty && carries(u, Bodies.isTopLevelBinding)) match
          case one :: Nil => Some(one)
          case _          => None
  }

  /** The statements that become the program's entry point, and the module they are read in.
   *
   * The entry file is a **body**, so this is everything in it that is not a module member —
   * its statements, and also the `val`s and functions it declares, which belong to that body rather
   * than to the module (`Bodies.isModuleMember`). They arrive in **source order**, which is what
   * makes a `val` there an ordinary local: it is initialized where it is written, rather than before
   * every statement in the file the way a module member's is.
   */
  protected def entryPoint(entry: Option[(Program, Scope)], captures: Set[String]): (Scope, List[Stmt]) =
    entry match
      case None => (Scope.root, Nil)
      case Some((u, s)) =>
        (s, u.body.filterNot {
          case f: FuncDecl => !captures(f.name)
          case other       => Bodies.isModuleMember(other)
        })
}
