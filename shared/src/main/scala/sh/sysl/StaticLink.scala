package sh.sysl

/** How the libraries a build found through `pkg-config` are linked — the root manifest's `link` key,
 * or `--link` (`reference/packages.md § Linking a library statically`).
 *
 * ==Why this is a choice at all==
 *
 * A program that links a dozen installed libraries dynamically pays for loading them every time it
 * starts: `dyld` maps each one, binds its symbols and runs its initializers before the first line of
 * `main`. For a short-lived program that is most of its run. Linking the same libraries as archives
 * moves the cost to the link, once.
 *
 * ==Why an archive is named by its path==
 *
 * `-l<name>` cannot be made to mean the archive. macOS's `ld` takes a `.dylib` over a `.a` it finds
 * in the same directory, and Homebrew installs both side by side, so the only spelling that reaches
 * the archive is the file itself. The same spelling works unchanged on Linux, which is why there is
 * one mechanism rather than a `-Bstatic` bracket there and a path here.
 *
 * ==The root's and nobody else's==
 *
 * For `optimization`'s reason: how the program is linked is decided once, for the whole program, and a
 * package that could decide it would be deciding for code it never sees.
 */
enum LinkMode {

  /** Every library dynamically, which is what a build does when nothing says otherwise. */
  case Dynamic

  /** Every library a `pkg_config` requirement names, anywhere in the build, from its archive **where
   * it has one**.
   *
   * A library installed with no archive — the system's own `sqlite3` in `/usr/lib` is the usual one —
   * is linked dynamically, with a trace line saying so, rather than refused: nobody named it, so
   * nobody asked for it in particular, and "every library that can be" is the only reading of
   * `static` a build over a dozen libraries can actually satisfy. Naming a library in `Only` is what
   * says *this one must come from its archive*, and there a missing one is still refused
   * (`StaticLink.archives`).
   */
  case Static

  /** The libraries these `pkg_config` names answer to from their archives, and the rest as usual.
   *
   * The names are the manifests' own — `libuv`, `openssl` — rather than the `-l` spellings, because
   * those are what a reader writing the key can see; `PkgConfig` says why the two cannot be derived
   * from each other.
   */
  case Only(modules: List[String])

  /** Whether the library `pkg-config` files as `module` is to be linked from its archive. */
  def marks(module: String): Boolean = this match
    case Dynamic       => false
    case Static        => true
    case Only(modules) => modules.contains(module)

  /** How this is written, both in a key and in a trace — and what the run cache is keyed over. */
  def spelling: String = this match
    case Dynamic       => "dynamic"
    case Static        => "static"
    case Only(modules) => modules.mkString(",")
}

object LinkMode {

  /** `--link`'s argument: `static`, `dynamic`, or a comma-separated list of `pkg_config` names.
   *
   * An empty name is refused rather than dropped, since `--link libuv,` was typed by somebody who
   * meant a second name and did not write it.
   */
  def parse(text: String): Either[String, LinkMode] = text match
    case "static"  => Right(Static)
    case "dynamic" => Right(Dynamic)
    case _ =>
      val names = text.split(",", -1).toList.map(_.trim)

      if names.exists(_.isEmpty) then
        Left(s"'--link $text' names an empty library — it is 'static', 'dynamic', or the " +
          "pkg_config names of the libraries to link statically, separated by commas")
      else Right(Only(names.distinct))
}

/** What `pkg-config` answered for a library that is to be linked from its archive
 * (`PkgConfig.queryStatic`).
 *
 * @param direct what `--libs` printed — the libraries a program linking this one names itself, and so
 *               the ones that must have an archive
 * @param static what `--libs --static` printed — the same plus what those libraries link privately,
 *               which an archive does not carry and the link line therefore has to
 * @param libdir the `.pc`'s own `libdir`, which is where the archive is when `pkg-config` printed no
 *               `-L` for it (it leaves out directories the linker searches by default)
 */
case class StaticAnswer(direct: List[String], static: List[String], libdir: Option[String])

/** The link-line half of `LinkMode`: which `-l` becomes which archive. A pure function over what
 * `pkg-config` answered and what is on disk, so that the rule can be asserted without either.
 */
object StaticLink {

  /** Libraries that are never taken from an archive, whatever the build asked.
   *
   * They are the platform's: the C library, its mathematics and threads, and the C++ runtime. Linking
   * one statically is not a speed-up but a different program — on macOS `libSystem` is the only
   * supported way into the kernel, and on Linux a static `libc` beside a dynamic loader is the
   * classic way to get two copies of `malloc`. A `-framework` is two tokens and never an `-l`, so it
   * cannot match here at all.
   */
  val System: Set[String] =
    Set("System", "c", "m", "pthread", "dl", "rt", "util", "resolv", "c++", "c++abi", "stdc++",
        "gcc", "gcc_s", "objc")

  /** The `-l` names in a list of linker flags, in order, less the platform's own. */
  private def libs(flags: List[String]): List[String] =
    flags.collect { case f if f.startsWith("-l") && f.length > 2 => f.drop(2) }.filterNot(System.contains)

  /** Where an archive may be: every `-L` the answer printed, then the `.pc`'s `libdir`, then
   * `--link-path`'s directories — the same places the linker would have looked for the `-l`.
   */
  def searched(answer: StaticAnswer, linkPaths: List[String]): List[String] =
    (answer.static.collect { case f if f.startsWith("-L") && f.length > 2 => f.drop(2) } :::
      answer.libdir.toList ::: linkPaths).distinct

  /** Each library of `module` that has an archive, mapped to that archive's path — or the sentence
   * the build stops on.
   *
   * A library `--libs` names is one the program links **directly**, so it must have an archive: a
   * build that asked for it statically and quietly got the `.dylib` would be the very build the key
   * exists to prevent, and nothing on the command line would say so. A library only `--static`
   * added is one of those libraries' own dependencies, which the reader did not name and may well be
   * a system library installed without an archive, so it stays an `-l` where there is none.
   *
   * **`required` is whether the reader named this library**, which is `LinkMode.Only` and not
   * `LinkMode.Static`. Where they did not, a direct library with no archive is linked dynamically like
   * a private one (`kept` names it for the trace) — and where *none* of the direct libraries has one,
   * the answer is empty, so the library is linked exactly as a dynamic build would link it rather than
   * with an archive of some private dependency and the `.dylib` of the library itself.
   */
  def archives(module: String, answer: StaticAnswer, linkPaths: List[String],
               exists: String => Boolean, required: Boolean = true): Either[String, Map[String, String]] = {
    val dirs   = searched(answer, linkPaths)
    val direct = libs(answer.direct).toSet

    libs(answer.static).distinct.foldLeft[Either[String, Map[String, String]]](Right(Map.empty)) {
      (acc, lib) =>
        acc.flatMap { found =>
          dirs.map(d => s"$d/lib$lib.a").find(exists) match
            case Some(archive)                    => Right(found + (lib -> archive))
            case None if direct(lib) && required  => Left(missing(module, lib, dirs))
            case None                             => Right(found)
        }
    }.map(found => if direct.isEmpty || direct.exists(found.contains) then found else Map.empty)
  }

  /** The libraries `module`'s program links directly that `archives` found no archive for, and which
   * are therefore linked dynamically — what `LinkMode.Static` traces rather than refuses.
   */
  def kept(answer: StaticAnswer, found: Map[String, String]): List[String] =
    libs(answer.direct).distinct.filterNot(found.contains)

  /** The refusal for a library that was asked for statically and has no archive. */
  def missing(module: String, lib: String, dirs: List[String]): String =
    s"'$module' is to be linked statically, and there is no 'lib$lib.a' to link it from — ${where(dirs)}. " +
      s"Install its static archive there, or leave '$module' out of 'link' to link it dynamically"

  /** The trace line for a library `link = "static"` found no archive of, and linked dynamically. */
  def keptNote(module: String, lib: String, dirs: List[String]): String =
    s"static: '$module' has no 'lib$lib.a', so -l$lib is linked dynamically — ${where(dirs)}"

  private def where(dirs: List[String]): String =
    if dirs.isEmpty then "pkg-config named no directory to look in"
    else s"looked in ${dirs.mkString(", ")}"

  /** A `link` list's names that no `pkg_config` requirement **in this build** answers to — the ones
   * that are either misspelt or gated off, and so the only ones worth widening the question for.
   */
  def unmatched(mode: LinkMode, declared: Set[String]): List[String] =
    mode match
      case LinkMode.Only(names) => names.filterNot(declared.contains)
      case _                    => Nil

  /** A `link` list's names that a requirement answers to only in a build with more features on —
   * `could` is every `pkg_config` name the build could reach with **all** of them on.
   *
   * Left out, with a trace, rather than refused: the list is written once for every build of the
   * project, and a build with `--no-default-features` that does not link `lmdb` has nothing to link
   * statically and nothing wrong with it.
   */
  def gated(mode: LinkMode, declared: Set[String], could: Set[String]): List[String] =
    unmatched(mode, declared).filter(could.contains)

  /** A `link` list's name that no `pkg_config` requirement answers to — in this build, or in the build
   * with every feature on (`could`).
   *
   * Refused rather than ignored, because a misspelt `libvu` would otherwise build and link libuv
   * dynamically with nothing to say the key did nothing. The names offered are every one the build
   * could link, feature on or off, since a reader correcting the spelling of a gated name is looking
   * for it among them.
   */
  def unknown(mode: LinkMode, declared: Set[String], supplied: Set[String],
              could: Set[String] = Set.empty): Option[String] =
    mode match
      case LinkMode.Only(names) =>
        val known = declared ++ could

        names.find(!known.contains(_)).map { name =>
          s"'link' names '$name', and no pkg_config requirement in this build is called that — the " +
            "names are the ones the manifests write under 'requires.pkg_config'" +
            (if known.isEmpty then "" else s", which here are ${known.toList.sorted.mkString(", ")}")
        }.orElse(names.find(supplied.contains).map { name =>
          s"'link' names '$name', which '--include-path $name=<dir>' answered, so pkg-config was not " +
            "asked which archive it is — link it statically by naming the archive's path to the " +
            "linker yourself, or leave it out of 'link'"
        })
      case _ => None

  /** A link line with every `-l` that has an archive replaced by that archive's path.
   *
   * A library is usually named twice — by the module's `@link` and again in what `pkg-config`
   * printed — so an archive is kept at its **last** place only: a linker scanning once takes from
   * an archive only what is already undefined, and the later place is after everything that could
   * need it.
   */
  def rewrite(flags: List[String], archives: Map[String, String]): List[String] =
    if archives.isEmpty then flags
    else
      val paths    = archives.values.toSet
      val replaced = flags.map(f => if f.startsWith("-l") then archives.getOrElse(f.drop(2), f) else f)

      replaced.zipWithIndex.collect {
        case (f, i) if !paths(f) || replaced.lastIndexOf(f) == i => f
      }
}
