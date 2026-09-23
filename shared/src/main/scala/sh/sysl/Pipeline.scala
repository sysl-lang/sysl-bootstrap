package sh.sysl

/** What a build asks of LLVM **beyond the level** — link-time optimization, and a profile to be
  * written or read — carried as one value rather than as three parameters.
  *
  * ==Why it is a value and not more parameters==
  *
  * A build sysl drives is four clang invocations: the module's IR, each C file a package vendors,
  * the standard module's own rebuild, and the link. Everything here has to reach **all** of them or
  * it does the wrong thing quietly rather than loudly — `-flto=thin` on the module alone leaves the
  * C a package carries as ordinary objects, so the cross-language inlining that is the whole point
  * of asking for it never happens and the build still succeeds. `-fprofile-generate` is worse: given
  * to the compile and not the link, the program will not link at all, and given to the link and not
  * the compile it links a profile runtime that counts nothing and writes an empty `.profraw`.
  *
  * So the shape that fits is the one `SearchPaths` already has — a value that every invocation is
  * handed — and the reason it is worth a file rather than three more defaulted parameters is that
  * three defaulted parameters on five functions is fifteen places for one of them to be forgotten.
  *
  * ==What each one is==
  *
  *   - `lto` is `thin` or `full`, and it is the one of the three that is a property of the
  *     **project**: a manifest may state it (`PackageConfig.lto`) the way it states its level.
  *     `thin` keeps a summary index per module and is near-incremental; `full` merges everything
  *     into one module and is the slower, more thorough one.
  *   - `profileGenerate` is the directory the instrumented binary writes its counters into, as
  *     `<dir>/default_<image>.profraw` — one file per run, so a training set of many programs
  *     merges into one profile without any of them overwriting another.
  *   - `profileUse` is the **indexed** profile a build reads, which is what `llvm-profdata merge`
  *     writes and not what the program wrote. Neither of the two is a manifest key: a profile
  *     describes a measurement somebody made on one machine, and a path to it in a file a consumer
  *     reads is a path that is wrong for everybody but its author.
  *
  * Generating and using are refused together at the command line (`Cli`), because clang takes both
  * and then quietly instruments the build it was asked to optimize.
  */
case class Pipeline(
    lto: Option[String] = None,
    profileGenerate: Option[String] = None,
    profileUse: Option[String] = None,
) {

  /** The flags, in the order clang's own documentation writes them. Every invocation gets the whole
    * list: there is no flag here that belongs to the compile and not the link, which is the point
    * of the type.
    *
    * The profile directory is made absolute because the instrumented program writes to it when it
    * **runs**, from whatever directory it is run in, and a relative path would scatter a training
    * set across as many directories as the training set has working directories.
    */
  def flags: List[String] =
    lto.map(mode => s"-flto=$mode").toList :::
      profileGenerate.map(dir => s"-fprofile-generate=${Project.absolute(dir)}").toList :::
      profileUse.map(file => s"-fprofile-use=${Project.absolute(file)}").toList

  /** Nothing asked for, which is what an ordinary build is. */
  def isEmpty: Boolean = lto.isEmpty && profileGenerate.isEmpty && profileUse.isEmpty

  /** What `RunCache`'s key carries, so that two builds differing only in what is asked for here
    * cannot share a cache slot (`RunCache`).
    *
    * **The flags themselves, rather than a rendering written twice.** A lever added to this type and
    * forgotten in the key is a `sysl run` handed back a binary built with a different optimizer, and
    * the one failure a cache can have is a stale answer — so the key is read off the same list the
    * build is, and a new lever joins both or neither.
    */
  def key: String = if isEmpty then "" else flags.mkString(" ")
}

object Pipeline {

  /** Nothing asked for. */
  val none: Pipeline = Pipeline()
}
