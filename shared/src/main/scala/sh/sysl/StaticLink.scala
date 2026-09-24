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

  /** Every library a `pkg_config` requirement names, anywhere in the build, from its archive. */
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
   */
  def archives(module: String, answer: StaticAnswer, linkPaths: List[String],
               exists: String => Boolean): Either[String, Map[String, String]] = {
    val dirs   = searched(answer, linkPaths)
    val direct = libs(answer.direct).toSet

    libs(answer.static).distinct.foldLeft[Either[String, Map[String, String]]](Right(Map.empty)) {
      (acc, lib) =>
        acc.flatMap { found =>
          dirs.map(d => s"$d/lib$lib.a").find(exists) match
            case Some(archive)            => Right(found + (lib -> archive))
            case None if direct(lib)      => Left(missing(module, lib, dirs))
            case None                     => Right(found)
        }
    }
  }

  /** The refusal for a library that was asked for statically and has no archive. */
  def missing(module: String, lib: String, dirs: List[String]): String =
    val where =
      if dirs.isEmpty then "pkg-config named no directory to look in"
      else s"looked in ${dirs.mkString(", ")}"

    s"'$module' is to be linked statically, and there is no 'lib$lib.a' to link it from — $where. " +
      s"Install its static archive there, or leave '$module' out of 'link' to link it dynamically"

  /** A `link` list's names that no `pkg_config` requirement in this build answers to.
   *
   * Refused rather than ignored, because a misspelt `libvu` would otherwise build and link libuv
   * dynamically with nothing to say the key did nothing.
   */
  def unknown(mode: LinkMode, declared: Set[String], supplied: Set[String]): Option[String] =
    mode match
      case LinkMode.Only(names) =>
        names.find(!declared.contains(_)).map { name =>
          s"'link' names '$name', and no pkg_config requirement in this build is called that — the " +
            "names are the ones the manifests write under 'requires.pkg_config'" +
            (if declared.isEmpty then "" else s", which here are ${declared.toList.sorted.mkString(", ")}")
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
