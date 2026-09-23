package sh.sysl

import io.github.edadma.cross_platform.*

/** The C a compilation carries, compiled for the same target and put on the link line
 * (`reference/ffi.md § A library may carry C`).
 *
 * ==C travels with a source tree, whatever that tree is to the compilation==
 *
 * `reference/ffi.md § A library may carry C` settles that a `.c` dropped anywhere in a tree is
 * compiled with it and nothing declares it, and the walk that finds the sysl already visits every
 * directory. What that leaves open is *which* trees a compilation walks, and the answer is all of
 * them: the project's own, a source tree named with `--lib`, and every package a `dependencies`
 * block brought in. Each is a directory of sysl files that reached this compilation as modules, and
 * a C file beside them is as much a part of one as of another.
 *
 * **A dependency's C is the case this exists for.** `reference/packages.md § No build scripts,
 * ever` refuses build scripts on the grounds that sysl already compiles a package's C declaratively
 * — which was true of a package built into a `.syslib` and false of one brought in as source, where
 * the sysl compiled and the C was dropped with no warning at all. What arrived was a linker naming
 * symbols the package's own C defines, from a build whose every other part had worked.
 *
 * ==Objects, not an archive==
 *
 * A `build-lib` archives its C beside the library's own object because an artifact is one file and
 * has to be. A compilation has no such constraint: the objects go on the command line as objects,
 * which is simpler and also more correct — an archive member is pulled in only to resolve a symbol
 * already undefined, and a shim whose whole job is to be called through a table nothing has
 * mentioned yet would not be pulled in at all.
 *
 * ==Each tree gets a directory of its own==
 *
 * `LibraryArtifact.nativeMember` names an object after the path it was found at rather than its
 * basename, because two modules may each hold a `util.c`. Across several trees even that is not
 * unique — two packages may each hold a `net/util.c` — so each tree is staged into a numbered
 * directory of its own and the names inside it stay the readable ones. Nothing has to be checked for
 * a collision, because the shape makes one impossible.
 */
object NativeSources {

  /** What the C of one compilation became: the objects the link is given, and everything to remove
   * once it is over. The scratch list is ordered so that a directory comes after what is in it.
   */
  case class Built(objects: List[String], scratch: List[String])

  /** No C anywhere — the ordinary case, and what a command that never links is handed. */
  val none: Built = Built(Nil, Nil)

  /** The C files of every tree a compilation walks, one list per tree, with the trees that hold none
   * dropped.
   *
   * A root that names a **file** rather than a directory contributes nothing, which is
   * `Project.cSources`'s own rule and the right one here too: naming a file compiles that file
   * alone (`reference/modules.md`), so there is no tree for C to have travelled with.
   *
   * **A tree may be named once.** Nothing deduplicates here and nothing needs to: a tree named twice
   * declares its sysl twice, which the analyzer refuses long before a link — so a repeat that reached
   * this far would be one the sysl walk never saw, and there is no such thing. What *did* arrive
   * twice is the standard library, and it is kept out at the source (`Main`, `stdTree`) rather than
   * filtered here, because the answer there is that a `--std` build has no separate library to add.
   */
  def of(roots: List[String], os: Os): List[List[Source]] =
    roots.map(Project.cSources(_, Some(os))).filter(_.nonEmpty)

  /** Compiles them, or says which one did not compile.
   *
   * A failure removes what it had already written before returning: the objects are temporaries, and
   * the caller that reports the error has no reason to know they existed.
   */
  def build(trees: List[List[Source]], target: Target = Target.default,
            level: String = Toolchain.defaultOptimization,
            paths: SearchPaths = SearchPaths.none, verbose: Boolean = false,
            pipeline: Pipeline = Pipeline.none): Either[String, Built] =
    if trees.isEmpty then Right(none)
    else
      val staging = createTempDirectory("sysl-c-")
      val staged = trees.zipWithIndex.map { (sources, i) =>
        val dir = s"$staging/$i"

        // The plain call rather than `Project.makeDirectories`, which tolerates a directory
        // something else has made: this one is under a temporary directory of this process's own,
        // so nothing can be racing it and a name already taken would be a mistake in the index
        // above rather than a neighbour to make room for.
        createDirectories(dir)
        (dir, sources.map(s => s -> s"$dir/${LibraryArtifact.nativeMember(s)}"))
      }

      val named   = staged.flatMap(_._2)
      val scratch = named.map(_._2) ::: staged.map(_._1) ::: List(staging)

      named.foldLeft[Either[String, Unit]](Right(()))((so_far, entry) =>
        so_far.flatMap(_ =>
          Toolchain.compileC(entry._1.name, entry._2, target, level, paths, verbose,
                             pipeline = pipeline))) match
        case Left(err) => scratch.foreach(Project.discard); Left(err)
        case Right(_)  => Right(Built(named.map(_._2), scratch))
}
