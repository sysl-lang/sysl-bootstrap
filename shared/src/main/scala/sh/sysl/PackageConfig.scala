package sh.sysl

import io.github.edadma.hocon.{ConfigBoolean, ConfigNumber, ConfigObject, ConfigString, ConfigValue, Hocon, HoconException}

/** What a target provides, as the project says rather than as the registry knows
 * (`reference/packages.md § What a project is called`).
 *
 * A `triple` here declares a machine the registry does not have; omitted, the block adds
 * capabilities to a registry entry of the same name. The split is the line `getting-started/cli.md
 * § targets` draws: **capabilities are policy and the ABI is not**, so a project may say what its
 * kernel target can do and may not say how a call to it is made.
 */
case class TargetConfig(triple: Option[String], capabilities: Map[String, Boolean])

/** The pair of C symbols a program's storage comes from and goes back to (`reference/packages.md §
 * One heap, and the package that names it`).
 *
 * A program has **one** of these, and `03` is why: whoever holds the last reference is what frees,
 * so two heaps in one program would mean a box whose payload cannot be given back by the code that
 * outlived it. What varies is not how many there are but which pair they are — libc's on a hosted
 * machine, `pvPortMalloc` / `vPortFree` under FreeRTOS, an arena's on a target that has one.
 *
 * The two symbols must have `malloc`'s and `free`'s signatures. **Nothing here can check that**, for
 * the same reason nothing checks an `extern` against the header it names: the declaration is a claim
 * about code the compiler does not have. It is the same trade `@link` already makes.
 */
case class Allocator(alloc: String, free: String)

object Allocator {

  /** What a program gets when no package says otherwise — and every program before this existed. */
  val c: Allocator = Allocator("malloc", "free")

  /** Whether a name is one a C symbol could have.
   *
   * Checked because the string reaches LLVM IR as `@$name`, where a space or a sigil would produce a
   * module that will not parse — and the failure would arrive as a syntax error inside generated text
   * rather than as a complaint about the line somebody wrote.
   */
  def isSymbol(name: String): Boolean =
    name.nonEmpty && (name.head.isLetter || name.head == '_') &&
      name.forall(c => c.isLetterOrDigit || c == '_')

  /** Which allocator a program built from these packages uses, or what is wrong with the answer.
   *
   * Each entry is a package's name and what it declared; the root project's name is whatever the
   * caller calls it, because a conflict has to be reported in words a reader can act on.
   *
   * **Two packages declaring the same pair is not a conflict** — it is one fact stated twice, which is
   * what happens as soon as a package and a driver built on it both know which kernel they are for.
   * Two packages declaring *different* pairs is refused, naming both, because there is no answer: the
   * program cannot have two heaps and nothing here can rank one claim above the other.
   *
   * **No declaration is not an error and never becomes one.** It is libc's pair, which is what every
   * program compiled before this feature got, so nothing that exists today changes.
   */
  def choose(declared: List[(String, Allocator)]): Either[String, Allocator] =
    declared.map(_._2).distinct match
      case Nil          => Right(c)
      case List(single) => Right(single)
      case several =>
        val who = declared
          .groupBy(_._2)
          .toList
          .sortBy(p => several.indexOf(p._1))
          .map { (a, packages) =>
            // The verb agrees, because one package claiming a pair is the ordinary case and 'freertos'
            // name pvPortMalloc reads as a mistake in the compiler rather than one in the project.
            val names = packages.map(_._1).sorted
            val verb  = if names.length == 1 then "names" else "name"

            s"'${names.mkString("' and '")}' $verb ${a.alloc} / ${a.free}"
          }

        Left("two packages name different allocators, and a program has one heap — " +
          who.mkString("; ") + ". Drop one of the declarations, or depend on only one of them")
}

/** The project config — `package.hocon` at the project root (`reference/packages.md`).
 *
 * The file is **optional**, and a project without one is not a lesser project: the defaults are the
 * root the driver was given, the machine the compiler is running on, and a target that provides
 * everything. That is the same shape `reference/modules.md` gives the anonymous root module — the
 * bare case is the general one with nothing filled in — and it is what keeps `sysl run hello.sysl`
 * free of ceremony.
 *
 * Only the part of `reference/packages.md` that has something to enforce is read. `package` and
 * `requires` are parsed and checked so that a file naming them is held to spelling them correctly,
 * and `dependencies` says what to fetch and what to call it (`§ 2`, `§ 3`).
 */
/** What one `requires { headers { … } }` entry declares: the prose a consumer is shown, and the
 * environment variable the ecosystem conventionally keeps the path in.
 *
 * The bare-string form of the entry is this with `env = None`, so a manifest written before this
 * existed means exactly what it always meant.
 */
case class HeaderReq(why: String, env: Option[String] = None)

case class PackageConfig(
    name: Option[String] = None,
    version: Option[String] = None,
    defaultTarget: Option[String] = None,
    targets: Map[String, TargetConfig] = Map.empty,
    capabilities: Map[String, Boolean] = Map.empty,
    requires: Set[String] = Set.empty,
    headers: Map[String, HeaderReq] = Map.empty,
    pkgConfig: Map[String, String] = Map.empty,
    dependencies: List[Dependency] = Nil,
    /** What this package's own tests need and its consumers do not
      * (`reference/packages.md § Dependencies a test alone needs`).
      *
      * Read exactly as `dependencies` is and resolved the same way — the difference is entirely in
      * who resolves them. They join the graph for this project's `sysl test`, and they are **not**
      * walked when this package is somebody's dependency, so a consumer neither fetches nor builds
      * them. `Tests.stripSource` already removes the source that would import one, so what this
      * adds is that the package is not fetched either.
      */
    devDependencies: List[Dependency] = Nil,
    /** The `features` block: each feature, and the optional dependencies and other features it turns
      * on (`PackageFeatures`).
      *
      * In the order the file declares them, which is the order anything listing them should use.
      * `default` is an ordinary entry here rather than a field of its own — it is the feature a
      * consumer gets when it asks for none, and nothing about reading the block treats it specially.
      */
    features: Map[String, List[String]] = Map.empty,
    allocator: Option[Allocator] = None,
    defines: Map[String, List[String]] = Map.empty,
    sysl: Option[Version] = None,
    /** Keys this compiler did not recognize, as sentences to print — one per key, in the order the
      * file writes them (`unknownKeys`).
      *
      * Carried rather than printed from the read, because reading a manifest is a function from text
      * to a value and a test asks it directly. The driver prints these.
      */
    warnings: List[String] = Nil,
) {

  /** Refuses to build where the compiler in hand is older than the floor this manifest states
   * (`reference/packages.md`).
   *
   * **The whole of what the field buys is this sentence.** A package that uses something the language
   * grew builds or does not depending on what the consumer happens to have installed, and when it
   * does not, the diagnostic points at a line inside somebody else's package with nothing to say the
   * compiler is the problem. `sdl3` v0.2.6 is the live example: it writes a bare `None` as a method
   * default, which needs 0.0.62, and a 0.0.61 consumer gets a type-inference error inside
   * `video.sysl`.
   *
   * `who` is what the reader has to act on — a package's name and version, or the project itself —
   * because the manifest at fault is usually not one they wrote.
   *
   * **An interim compiler satisfies the floor its numbers reach**, which is why the comparison is
   * against `Version.ofCompiler`: `0.0.66-fcf4e33a` is dev heading for 0.0.66 and has everything
   * 0.0.65 shipped. Cargo makes the same ruling for a nightly toolchain against `rust-version`.
   *
   * **An older compiler cannot report this at all**, and nothing here can change that: it does not
   * know the key, so it reads the manifest, ignores the field, and fails wherever it was going to
   * fail. The field starts paying from the release that understands it, exactly as `rust-version`
   * did.
   */
  def checkFloor(who: String, compiler: Version): Either[String, Unit] = sysl match
    case Some(floor) if compiler < floor =>
      Left(s"$who cannot be built because it requires sysl $floor or newer, while the compiler in " +
        s"hand is $compiler")
    case _ => Right(())

  /** Every `path` dependency, read against the directory that holds **this** manifest.
   *
   * `readOrigin` keeps a `path` exactly as the manifest wrote it, because reading is a pure function
   * of text with no filesystem in the way (`PackageConfig.read`'s own docstring). So a relative path
   * means nothing on its own — it has to be joined to wherever this particular `package.hocon` was
   * found, which only the reader of the file knows. Calling this once, right after the read, is what
   * makes the directory in `Origin.Local` unambiguous from then on: a package read again as somebody
   * else's dependency, with a different `dir`, resolves its own paths against its own directory,
   * never against the directory of whichever manifest happened to read it first.
   */
  def resolvingLocalPaths(dir: String): PackageConfig =
    copy(dependencies = dependencies.map(_.resolvedAgainst(dir)),
         devDependencies = devDependencies.map(_.resolvedAgainst(dir)))

  /** The macros one carried C file is compiled with, as clang spells them, or nothing.
   *
   * `path` is relative to the package root and written with `/`, which is how the manifest names it.
   */
  def definesFor(path: String): List[String] = defines.getOrElse(path, Nil)

  /** Every macro this package declared, keyed by the path its file was **found** at rather than by
   * the one the manifest wrote (`SearchPaths.carried`).
   *
   * ==Why the walk decides the key and the root does not==
   *
   * A manifest names `sh/sysl/miniz/c/miniz.c`, and what reaches clang is whatever the source walk
   * produced — which is absolute and canonical whatever the reader typed, so `sysl test .` inside a
   * package yields `/private/tmp/…/sh/sysl/miniz/c/miniz.c` for a root of `.`. Joining the root to
   * the relative path gives `./sh/sysl/miniz/c/miniz.c`, which is the same file and not the same
   * string, and the macros then reach nothing. Worse, they reach nothing *quietly*: the C compiles
   * under its defaults and only a `c const` measuring a configured struct notices.
   *
   * Absolutizing the root instead would fix the `.` and not the rest of it — a symlinked path is
   * canonicalised by the walk and not by the working directory, so `/tmp` and `/private/tmp` would
   * still be two strings for one file on this machine.
   *
   * So the declared path is matched against the files the walk actually returned, within this
   * package's own tree. `found` is that list. A path matching none of them is a mistake in the
   * manifest rather than something to pass over, and it is the one mistake here that is otherwise
   * invisible: every other way of getting a `defines` block wrong is refused when the file is read.
   */
  def carriedDefines(found: List[String]): Either[String, Map[String, List[String]]] =
    PackageConfig.collect(defines.toList.sortBy(_._1)) { (declared, macros) =>
      found.find(path => path == declared || path.endsWith(s"/$declared")) match
        case Some(path) => Right(path -> macros)
        case None =>
          Left(s"${PackageConfig.FileName}: 'defines.\"$declared\"' names a file this package does " +
            "not carry — the block configures the C the package itself holds, and there is no such " +
            "C file in this tree")
    }.map(_.toMap)

  /** The capabilities `target` provides.
   *
   * **A capability the file does not mention is provided**, which is the direction that cannot
   * silently take something away. The compiler's prior is that a machine can do everything — that is
   * what every target did before there was a file to say otherwise — so what a config records is
   * what a machine *cannot* do. It also composes the way the source clause does: `@no_alloc` narrows,
   * and so does `heap = false`.
   *
   * **The top-level block is the project's own policy and a target block layers over it**, which is
   * the order the two are written in and the only one that reads sensibly: a project says what it is
   * building — *this thing has no heap* — and then names the one machine where that is not so. Keyed
   * only by target, the statement could not be made at all for a target the registry already has,
   * since a block would then be a machine the project was redefining rather than a policy it was
   * declaring.
   *
   * Whether a heap exists is a **project engineering decision**, which is the whole reason this is
   * here rather than derived: `getting-started/cli.md § targets` deliberately carries no
   * capabilities, because a target's capabilities are exactly the part a project has an opinion
   * about (`reference/packages.md § What a project is called`).
   */
  def provides(target: String): Set[String] = {
    val perTarget = targets.get(target).map(_.capabilities).getOrElse(Map.empty)

    Capability.core.toSet.filter(c => perTarget.getOrElse(c, capabilities.getOrElse(c, true)))
  }
}

object PackageConfig {

  /** The one name the driver looks for. */
  val FileName = "package.hocon"

  /** The sub-block of `requires` that names headers rather than capabilities. */
  val HeadersKey = "headers"

  /** The sub-block of `requires` that names libraries this machine is asked about by `pkg-config`
   * (`reference/packages.md § Capabilities`).
   */
  val PkgConfigKey = "pkg_config"

  /** Whether a header requirement may be called this.
   *
   * A name reaches a command line as `--include-path <name>=<dir>`, so it must be tellable from a
   * directory by looking — which is what `SearchPaths.namedInclude` does with it, and why the rule
   * lives here rather than being written twice. Refused rather than escaped: a name holding a
   * separator or an `=` would be one the flag could not carry, and the failure would land on the
   * consumer typing a command they were given rather than on the package that chose the name.
   */
  def isHeaderName(name: String): Boolean =
    name.nonEmpty && name.head.isLetter && name.forall(c => c.isLetterOrDigit || c == '_' || c == '-')

  /** Whether a `pkg_config` requirement may be called this.
   *
   * Wider than `isHeaderName` by a dot and a `+`, and the widening is not a relaxation — it is the
   * recognition that **this name is not the package author's to choose**. A `headers` name is
   * invented by whoever writes the manifest, so holding it to a plain word costs nobody anything.
   * A `.pc` name is what `pkg-config` answers to, and a great many of the world's libraries file
   * under one with a version in it: libyaml is `yaml-0.1` on macOS, Debian and Arch alike, glib is
   * `glib-2.0`, GTK is `gtk+-3.0`. Refusing those is refusing to bind the library at all.
   *
   * What the flag needs is unchanged and is what still bounds this: the name reaches a command line
   * as `--include-path <name>=<dir>`, so it may hold neither a separator nor an `=` — which is what
   * keeps `SearchPaths.namedInclude` able to tell a name from a directory by looking. A dot and a
   * `+` are in neither of those, which is why they cost nothing to allow.
   */
  def isPkgConfigName(name: String): Boolean =
    name.nonEmpty && name.head.isLetter &&
      name.forall(c => c.isLetterOrDigit || c == '_' || c == '-' || c == '.' || c == '+')

  /** A project that said nothing, which is what a missing file means. */
  val empty: PackageConfig = PackageConfig()

  /** Reads the file's text, or says what is wrong with it in one line.
   *
   * It takes **text rather than a path** so that the whole of it runs on every platform with no
   * filesystem in the way: finding the file is the driver's, and what the file
   * means is here, where a test can ask about it directly.
   */
  def read(text: String): Either[String, PackageConfig] =
    try
      val root = Hocon.parse(text).root

      // Read off the parsed tree rather than through the dotted-path getters, which split on '.'
      // without regard for quoting — so `targets.aarch64-kernel` resolves and `targets."a.b"` does
      // not, and a target whose name carried a dot would be looked for in a nesting nobody wrote.
      // The structure is what the file means; the path API is a convenience that does not fit here.
      for
        pkg     <- block(root, "package")
        _       <- checkName(pkg.flatMap(string(_, "name")))
        floor   <- readFloor(pkg)
        targets <- readTargets(root)
        project <- readCapabilityFlags(block2(root, "capabilities"), "capabilities")
        needed  <- readCapabilityFlags(capabilitiesOf(block2(root, "requires")), "requires")
        _       <- checkNotNarrowing(needed)
        headers <- readHeaders(block2(root, "requires"))
        pkgs    <- readPkgConfig(block2(root, "requires"))
        deps    <- readDependencies(root)
        dev     <- readDependencies(root, "dev_dependencies")
        _       <- notInBoth(deps, dev)
        feats   <- PackageFeatures.read(root)
        _       <- PackageFeatures.check(feats, deps)
        alloc   <- readAllocator(root)
        defs    <- readDefines(root)
      yield PackageConfig(
        name = pkg.flatMap(string(_, "name")),
        version = pkg.flatMap(string(_, "version")),
        sysl = floor,
        defaultTarget = block2(root, "targets").flatMap(string(_, "default")),
        targets = targets,
        capabilities = project.toMap,
        requires = needed.collect { case (name, true) => name }.toSet.flatMap(Capability.closure),
        headers = headers,
        pkgConfig = pkgs,
        dependencies = deps,
        devDependencies = dev,
        features = feats,
        allocator = alloc,
        defines = defs,
        warnings = unknownKeys(root, TopLevelKeys, "") :::
          pkg.toList.flatMap(unknownKeys(_, PackageKeys, "package.")),
      )
    catch
      // Every way HOCON can be wrong arrives as one of these, and the driver wants a line rather
      // than a stack trace. The message is the library's own, which already says where it was.
      case e: HoconException => Left(s"$FileName: ${e.getMessage}")

  /** Refuses `requires { … = false }`, which parsed cleanly and was then thrown away.
   *
   * `requires` is what a package **needs of its host** (`reference/packages.md § Capabilities`), so
   * a `false` there says nothing: a package does not need a facility *not* to exist. It was
   * collected with `collect { case (name, true) => name }` and silently dropped, which is worse
   * than a refusal — the file then reads as though the project had said something, and it is the
   * spelling somebody reaches for first when what they mean is *this project has no heap*.
   *
   * The two blocks are named in the message because they are the two directions, and somebody who
   * wrote one of them wanted the other.
   */
  private def checkNotNarrowing(needed: Map[String, Boolean]): Either[String, Unit] =
    needed.collectFirst { case (name, false) => name } match
      case None => Right(())
      case Some(name) =>
        Left(s"$FileName: 'requires { $name = false }' says nothing — 'requires' is what this package " +
          s"needs its host to have. To say the machine has no $name, write it in the project's own " +
          s"'capabilities { $name = false }'; to say a module does not use it, write " +
          s"'@no_${Capability.narrowWord(name)}' in that module's files")

  /** A package's name is what a directory project's output is called, so it reaches the filesystem
   * and has to be a single path segment.
   *
   * Refused rather than quietly ignored, and refused rather than sanitized: a file saying
   * `name = "build/tool"` was written by somebody who expected an answer, and both of the silent
   * options — falling back to the directory name, or flattening the separator away — write a
   * different executable from the one they asked for and say nothing about it. `.` and `..` are here
   * for the same reason: each is a legal segment that names a directory rather than a file, so the
   * link would fail at the far end with a message about a path rather than about this line.
   */
  /** `package.sysl` — the oldest compiler this package is known to build with
   * (`reference/packages.md`).
   *
   * It is a **floor** rather than a range, and it is three numbers like every other version here, so
   * `Version.parse` is what reads it and a pre-release spelling is refused along with everything else
   * that is not a version. That is the right refusal: what a package states is the release it needs,
   * and an interim is not something anybody else can install.
   *
   * The message names the field, because the version is a string in a file rather than something the
   * reader can see the shape of from the surrounding line.
   */
  private def readFloor(pkg: Option[ConfigObject]): Either[String, Option[Version]] =
    pkg.flatMap(string(_, "sysl")) match
      case None => Right(None)
      case Some(text) =>
        Version.parse(text).left.map(e => s"$FileName: 'package.sysl' names the oldest compiler " +
          s"this package builds with, and $e").map(Some(_))

  private def checkName(name: Option[String]): Either[String, Unit] = name match
    case None => Right(())
    case Some(n) =>
      if n.isEmpty then Left(s"$FileName: 'package.name' is empty")
      else if n == "." || n == ".." || n.exists(c => c == '/' || c == '\\') then
        Left(s"$FileName: 'package.name' is what this project's output is called, so '$n' cannot " +
          "be one — it names a path rather than a name")
      else Right(())

  /** A named sub-object, refusing a key that holds something else. */
  private def block(o: ConfigObject, key: String): Either[String, Option[ConfigObject]] =
    o.fields.get(key) match
      case None                    => Right(None)
      case Some(sub: ConfigObject) => Right(Some(sub))
      case Some(_)                 => Left(s"$FileName: '$key' is not a block")

  /** The same, where a non-object has already been refused or does not matter. */
  private def block2(o: ConfigObject, key: String): Option[ConfigObject] =
    o.fields.get(key).collect { case sub: ConfigObject => sub }

  /** A string field, or nothing where the key is absent or holds something else. */
  private def string(o: ConfigObject, key: String): Option[String] =
    o.fields.get(key).collect { case ConfigString(v) => v }

  /** Each named block under `targets`, less the `default` key, which names one rather than being
   * one.
   */
  private def readTargets(root: ConfigObject): Either[String, Map[String, TargetConfig]] =
    block2(root, "targets") match
      case None => Right(Map.empty)
      case Some(targets) =>
        val blocks = targets.fields.toList.filter(_._1 != "default").sortBy(_._1)

        collect(blocks) {
          case (name, sub: ConfigObject) =>
            readCapabilityFlags(block2(sub, "capabilities"), s"targets.$name.capabilities")
              .map(caps => name -> TargetConfig(string(sub, "triple"), caps))
          // `targets.default = "x"` is a string beside the blocks, and anything else that is not a
          // block is the same mistake: a name where a description of a machine was expected.
          case (name, _) => Left(s"$FileName: 'targets.$name' is not a block describing a target")
        }.map(_.toMap)

  /** A block of capability names, each true or false — a target's `capabilities` and the package's
   * own `requires` have the same shape and the same mistakes.
   *
   * A name that is not a capability is refused rather than ignored. A file that says `treads = false`
   * was written by somebody who believes they have turned something off, and the whole value of the
   * block is that they have.
   */
  private def readCapabilityFlags(section: Option[ConfigObject], where: String)
      : Either[String, Map[String, Boolean]] =
    section match
      case None => Right(Map.empty)
      case Some(caps) =>
        collect(caps.fields.toList.sortBy(_._1)) { (rawName, value) =>
          // **A module's word is accepted here and mapped**, which is a transitional allowance rather
          // than a second spelling: `alloc` names what a module does and `heap` names the facility, and
          // the config wants the facility. What makes it worth carrying is that a **tag is immutable**
          // — every package in the org is fetched at a pinned version whose `package.hocon` says
          // `requires { alloc = true }` and always will, and `Resolve.configOf` validates a fetched
          // dependency's file exactly as it validates the project's own. Refusing the old word outright
          // would stop every pinned dependency resolving on the day this shipped, and re-tagging cannot
          // fix a consumer that has not also bumped its pin.
          //
          // `heap` is the documented name and the one to write. This goes when the org has been swept.
          val name = Capability.narrowedBy.getOrElse(rawName, rawName)

          if !Capability.implies.contains(name) then
            Left(s"$FileName: '$rawName' in '$where' is not a capability — " +
              s"the set is ${Capability.core.map(n => s"'$n'").mkString(", ")}")
          else
            value match
              case ConfigBoolean(on) => Right(name -> on)
              case _                 => Left(s"$FileName: '$where.$name' must be true or false")
        }.map(_.toMap)

  /** `requires` less its `headers` sub-block, which is a requirement of a different kind and is read
   * by `readHeaders`.
   */
  private def capabilitiesOf(section: Option[ConfigObject]): Option[ConfigObject] =
    section.map(caps => ConfigObject(caps.fields - HeadersKey - PkgConfigKey))

  /** The `headers` sub-block of `requires` — the C headers this package's own C includes and does
   * not carry, each under a name the consumer satisfies with `--include-path <name>=<dir>`
   * (`reference/packages.md § Capabilities`).
   *
   * ==A name, never a path — and, since 0391, the NAME of a variable==
   *
   * `reference/ffi.md § @link` refuses a path here in as many words: the file is committed and
   * describes the *package*, and where a prefix lives on somebody's laptop is not a property of the
   * package. That half is unchanged and is the whole of why an `env` is allowed.
   *
   * **This paragraph used to refuse an environment variable too**, on the grounds that a build
   * reading the consumer's shell is one that works for whoever wrote it. What that missed is that a
   * variable's *name* is not a path: `PICO_SDK_PATH` is the same string on every machine in the
   * world, it is what the pico-sdk's own CMake reads, and this file already named it in prose inside
   * the `note`. Carrying it as data mechanises a sentence that was written down anyway.
   *
   * It also cannot turn a working build into a differently-working one, which is the objection that
   * would have bitten: an `env` is consulted **only** where no `--include-path` supplied the name,
   * and where it is unset the build stops exactly as it did before. So it converts a refusal into a
   * success and never one success into another.
   *
   * ==The value is the reason, and it is what makes the refusal worth having==
   *
   * A name alone would tell a consumer that something called `lwip` is unsatisfied and leave them to
   * guess what that is and where it lives. The string is quoted back at them instead, so the build
   * that stops says what to go and find. It is prose for a person and nothing reads it as data.
   */
  private def readHeaders(section: Option[ConfigObject]): Either[String, Map[String, HeaderReq]] =
    section.flatMap(s => block2(s, HeadersKey)) match
      case None => Right(Map.empty)
      case Some(headers) =>
        collect(headers.fields.toList.sortBy(_._1)) { (name, value) =>
          value match
            case ConfigString(why) if why.trim.nonEmpty =>
              checkHeaderName(name).map(_ => name -> HeaderReq(why, None))
            case ConfigString(_) =>
              Left(s"$FileName: 'requires.$HeadersKey.$name' says nothing about what it needs — the " +
                "text is quoted back at whoever has to supply the path, so an empty one helps nobody")
            case o: ConfigObject => readHeaderObject(name, o)
            case _ =>
              Left(s"$FileName: 'requires.$HeadersKey.$name' must be a string saying what these " +
                "headers are and where they come from, or a block with a 'note' and the 'env' the " +
                "path conventionally lives in")
        }.map(_.toMap)

  /** The long form of a `headers` entry: the prose, plus the environment variable the ecosystem
   * conventionally keeps this path in.
   *
   * **`note` is required and `env` is optional**, which is the way round that keeps the refusal
   * worth reading: a consumer who has neither the variable set nor a flag to hand is shown the
   * prose, and a block that supplied only a variable name would leave them with nothing to go and
   * look for on a machine where it is unset.
   */
  private def readHeaderObject(name: String, o: ConfigObject): Either[String, (String, HeaderReq)] = {
    val where = s"'requires.$HeadersKey.$name'"

    def str(key: String): Either[String, Option[String]] = o.fields.toMap.get(key) match
      case None                                     => Right(None)
      case Some(ConfigString(v)) if v.trim.nonEmpty => Right(Some(v))
      case Some(ConfigString(_))                    => Left(s"$FileName: $where's '$key' is empty")
      case Some(_)                                  => Left(s"$FileName: $where's '$key' must be a string")

    val known = Set("note", "env")

    for
      _    <- o.fields.map(_._1).find(!known.contains(_))
                .map(k => Left(s"$FileName: $where has no '$k' — a headers block takes 'note' and 'env'"))
                .getOrElse(Right(()))
      note <- str("note")
      env  <- str("env")
      _    <- checkHeaderName(name)
      n    <- note.toRight(s"$FileName: $where needs a 'note' saying what these headers are and " +
                "where they come from — it is what a consumer without the variable set is shown")
      _    <- env.filter(!isEnvName(_))
                .map(e => Left(s"$FileName: $where's 'env' is '$e', which is not the name of an " +
                  "environment variable — letters, digits and '_', not starting with a digit"))
                .getOrElse(Right(()))
    yield name -> HeaderReq(n, env)
  }

  /** Whether a string is the name of an environment variable rather than a path or a value.
   *
   * Checked for the same reason a `headers` name and a `.pc` name are: what the manifest carries has
   * to be tellable from what it must never carry. A directory holds a separator and a variable's
   * name does not, so the refusal can say which mistake was made.
   */
  private def isEnvName(s: String): Boolean =
    s.nonEmpty && !s.head.isDigit && s.forall(c => c.isLetterOrDigit || c == '_')

  /** The `pkg_config` sub-block of `requires` — the installed libraries this package binds, each
   * under the name `pkg-config` files it as (`reference/packages.md § Capabilities`).
   *
   * ==Both halves of one declaration==
   *
   * A `headers` requirement is answered by `--include-path` and a `@link` directive by
   * `--link-path`, and a package binding an installed library needs both: its headers to compile and
   * its archive to link. They are one requirement rather than two because they are one fact — *this
   * machine must have SDL3* — and a consumer who answered one of them has not got a build.
   *
   * So this is a requirement kind beside `headers` rather than a field on it. The value is prose for
   * a person, for `readHeaders`' reason: it is quoted back at whoever has to install the library, and
   * it is the only part of the refusal nothing in the compiler could have written.
   *
   * ==A `.pc` name, which is not the `@link` name==
   *
   * The name is what `pkg-config` answers to, and `PkgConfig` says why it cannot be derived from
   * anything already in the package: sdl3 writes `@link("SDL3")` and files as `sdl3`.
   *
   * The name is checked by `isPkgConfigName` rather than by `isHeaderName`, and the two differ by a
   * dot and a `+` for one reason: a `headers` name is invented by the package's author and a `.pc`
   * name is not. libyaml files as `yaml-0.1` everywhere it is packaged, glib as `glib-2.0`, GTK as
   * `gtk+-3.0` — so a rule that refuses a dot is a rule that refuses to bind them. What both
   * predicates still guarantee is the flag: a consumer overrides this exactly as they answer a
   * header requirement, with `--include-path <name>=<dir>`, and neither a dot nor a `+` is anything
   * that flag could not carry.
   */
  private def readPkgConfig(section: Option[ConfigObject]): Either[String, Map[String, String]] =
    section.flatMap(s => block2(s, PkgConfigKey)) match
      case None => Right(Map.empty)
      case Some(mods) =>
        collect(mods.fields.toList.sortBy(_._1)) { (name, value) =>
          value match
            case ConfigString(why) if why.trim.nonEmpty => checkPkgConfigName(name).map(_ => name -> why)
            case ConfigString(_) =>
              Left(s"$FileName: 'requires.$PkgConfigKey.$name' says nothing about what it needs — the " +
                "text is quoted back at whoever has to install the library, so an empty one helps " +
                "nobody")
            case _ =>
              Left(s"$FileName: 'requires.$PkgConfigKey.$name' must be a string saying what this " +
                "library is and how it is installed")
        }.map(_.toMap)

  /** The keys an `allocator` block has, both required. */
  private val AllocatorKeys = Set("alloc", "free")

  /** The `allocator` block — the pair of C symbols this package's storage comes from
   * (`reference/packages.md § One heap, and the package that names it`).
   *
   * ==Why a package and not a target==
   *
   * The allocator is a fact about the software a program is built *on*, not about the machine:
   * `thumbv7em` does not imply FreeRTOS, two RTOSes on one chip want different pairs, and a
   * bare-metal program on that same chip wants libc's. A target-level answer would need a target
   * per RTOS, which `getting-started/cli.md § targets` already declined to do for a float variant.
   *
   * ==Both keys, or neither==
   *
   * Half a pair is refused rather than filled in from libc. A file naming `alloc` and not `free` would
   * otherwise get storage from one heap and give it back to another, which is the single worst outcome
   * available here and would present as corruption a long way from this line.
   *
   * An unknown key is refused for the reason `readDependency` refuses one: somebody who wrote
   * `malloc = "pvPortMalloc"` believes they have said something, and ignoring the key would leave them
   * believing it while the program went on calling libc.
   */
  private def readAllocator(root: ConfigObject): Either[String, Option[Allocator]] =
    block(root, "allocator").flatMap {
      case None => Right(None)
      case Some(sub) =>
        def unknown: Option[String] = sub.fields.keys.toList.sorted.find(!AllocatorKeys.contains(_))

        def symbol(key: String): Either[String, String] =
          sub.fields.get(key) match
            case Some(ConfigString(v)) if Allocator.isSymbol(v) => Right(v)
            case Some(ConfigString(v)) =>
              Left(s"$FileName: 'allocator.$key' is '$v', which is not a name a C function can have")
            case Some(_) => Left(s"$FileName: 'allocator.$key' must be the name of a C function")
            case None =>
              Left(s"$FileName: 'allocator' names no '$key' — a program takes its storage from one " +
                "pair and gives it back to the same one, so both halves are said or neither is")

        for
          _ <- unknown.toLeft(()).left.map(k => s"$FileName: 'allocator.$k' is not something an " +
                 s"allocator says — the keys are 'alloc' and 'free'")
          a <- symbol("alloc")
          f <- symbol("free")
          _ <- if a == f then Left(s"$FileName: 'allocator' names '$a' for both halves") else Right(())
        yield Some(Allocator(a, f))
    }

  /** The `defines` block: the macros a package's **own carried C** is compiled with
   * (`reference/packages.md § No build scripts, ever`).
   *
   * ==Why a package says this and a consumer does not==
   *
   * A vendored C library almost always has compile-time options, and they are the *author's*
   * decision: which of miniz's four `MINIZ_NO_*` switches are set decides what the package is, and a
   * consumer of it should no more be typing them than choosing its warning flags. Before this block
   * the only way to set one was `--define` on the consumer's command line, which put a package's
   * internal configuration in the hands of everybody who depended on it — or drove the author to
   * edit the vendored source, which is a fork of upstream carried forever.
   *
   * ==Bundled C only==
   *
   * A key is the path of a `.c` file **this package carries**, relative to the package root and
   * written with `/`. It is not a general flags channel: it cannot reach an installed library's
   * headers, cannot add an include path, and cannot pass anything that is not a macro. `--define`
   * remains what a *build* says, and applies to every C compilation in it; this is what a *package*
   * says, and applies to the file it names.
   *
   * ==What a value means==
   *
   * `true` is a bare `-DNAME`, which is what a C option tested with `#ifdef` wants. Anything else
   * scalar is `-DNAME=value`, which is what one tested with `#if` wants. **`false` is refused**: a
   * reader would have to guess between "do not define this" and `-DNAME=0`, and those differ under
   * `#ifdef`. Whichever was meant can be said exactly — omit the line, or write `0`.
   */
  private def readDefines(root: ConfigObject): Either[String, Map[String, List[String]]] =
    block2(root, "defines") match
      case None => Right(Map.empty)
      case Some(defines) =>
        val read = collect(defines.fields.toList.sortBy(_._1)) {
          case (key, sub: ConfigObject) =>
            for
              paths  <- expand(key)
              _      <- collect(paths)(checkCSource)
              macros <- collect(sub.fields.toList.sortBy(_._1))(macroOf(key, _, _))
            yield paths.map(_ -> macros)
          case (key, _) =>
            Left(s"$FileName: 'defines.\"$key\"' is not a block of macros — a key is the C this " +
              "package carries and what follows it is what that C is compiled with")
        }

        read.map(_.flatten).flatMap { pairs =>
          // A file configured from two blocks has no sensible merge: the later would silently win,
          // which is the one outcome nobody could have intended by writing both.
          pairs.map(_._1).diff(pairs.map(_._1).distinct).distinct.sorted match
            case Nil => Right(pairs.toMap)
            case twice =>
              Left(s"$FileName: 'defines' configures ${twice.map(p => s"'$p'").mkString(" and ")} " +
                "from more than one block — a file is compiled once, so it is said in one place")
        }

  /** A key's alternatives, as a shell writes them: `a/{x,y}.c` is `a/x.c` and `a/y.c`, and several
   * groups give the cross product.
   *
   * ==Why braces and not a glob==
   *
   * A `*` would pick up a `.c` added later without anybody deciding, and these are macros that
   * change a struct's layout — a file joining the set by accident is the exact failure this block
   * exists to prevent, and it would fail the way the others do, silently. A brace still names every
   * file it configures; it only says the shared part once, which is the whole difference between
   * this and repeating the list.
   *
   * That matters most where a package's C **must** agree: miniz's implementation and its shim read
   * one header under five options, and two copies of that list is how they start to drift.
   *
   * **Nesting is refused rather than supported.** `{a,{b,c}}` is a second grammar to learn and
   * expands to what a flat list already says.
   */
  private[sysl] def expand(key: String): Either[String, List[String]] = {
    val open = key.count(_ == '{')

    if open == 0 then (if key.contains('}') then unbalanced(key) else Right(List(key)))
    else
      // Left to right, one group at a time, each alternative carried through the rest — so several
      // groups multiply out and the order is the one a reader scanning the key would predict.
      val start = key.indexOf('{')
      val end   = key.indexOf('}', start)

      if end < 0 then unbalanced(key)
      else
        val group = key.substring(start + 1, end)

        if group.contains('{') then
          Left(s"$FileName: 'defines.\"$key\"' nests one group of alternatives inside another, " +
            "which says nothing a flat list does not")
        else
          val parts = group.split(",", -1).toList

          if parts.exists(_.isEmpty) then
            Left(s"$FileName: 'defines.\"$key\"' has an empty alternative — whichever was meant " +
              "can be written out, and a group naming nothing configures nothing")
          else
            collect(parts)(part => expand(key.substring(0, start) + part + key.substring(end + 1)))
              .map(_.flatten)
  }

  private def unbalanced(key: String): Either[String, List[String]] =
    Left(s"$FileName: 'defines.\"$key\"' has an unbalanced brace — alternatives are written " +
      "'{one,two}', as a shell writes them")

  /** One `NAME = value` line, as clang spells it. */
  private def macroOf(path: String, name: String, value: ConfigValue): Either[String, String] =
    if !isMacroName(name) then
      Left(s"$FileName: 'defines.\"$path\".$name' is not a name a C macro can have — letters, " +
        "digits and '_', not starting with a digit")
    else
      value match
        case ConfigBoolean(true)  => Right(name)
        case ConfigBoolean(false) =>
          Left(s"$FileName: 'defines.\"$path\".$name' is false, which does not say which of two " +
            "things is meant — leave the line out to not define it, or write '0' to define it as " +
            "zero, which '#ifdef' still sees")
        case ConfigString(v) => Right(s"$name=$v")
        case ConfigNumber(v) => Right(s"$name=$v")
        case _ =>
          Left(s"$FileName: 'defines.\"$path\".$name' must be true or a scalar — a macro is " +
            "text the C preprocessor substitutes, so there is nothing a block or a list could mean")

  /** Whether a `defines` key names a C file this package could be carrying.
   *
   * The three refusals are the three ways the path could name something that is not this package's
   * to configure: an absolute path is a file on the author's machine, a `..` segment climbs out of
   * the package, and an extension that is not `.c` is either a typo or an attempt to configure a
   * header — which is not a translation unit and is compiled by nothing.
   */
  private def checkCSource(path: String): Either[String, Unit] =
    if !path.endsWith(".c") then
      Left(s"$FileName: 'defines.\"$path\"' does not name a '.c' file — a macro is given to a " +
        "translation unit, and only the C this package carries is one it compiles")
    else if path.startsWith("/") then
      Left(s"$FileName: 'defines.\"$path\"' is an absolute path — a key is relative to the " +
        "package root, since the package is the same tree wherever it was fetched to")
    else if path.split("/").contains("..") then
      Left(s"$FileName: 'defines.\"$path\"' climbs out of the package with '..' — a package " +
        "configures the C it carries and nothing else")
    else Right(())

  /** The directory part of a manifest path, or `""` for a file at the package root.
   *
   * Written here rather than taken from `Project`, because a manifest path is not a path on this
   * machine: it is always relative, always written with `/`, and means the same thing on every
   * platform the package is fetched to.
   */
  def parentOf(path: String): String =
    path.lastIndexOf('/') match
      case -1 => ""
      case i  => path.substring(0, i)

  /** Whether a name is one the C preprocessor would take. */
  def isMacroName(name: String): Boolean =
    name.nonEmpty && !name.head.isDigit && name.forall(c => c.isLetterOrDigit || c == '_')

  private def checkHeaderName(name: String): Either[String, Unit] =
    if isHeaderName(name) then Right(())
    else
      Left(s"$FileName: '$name' is not usable as a header requirement's name — it is written on a " +
        "command line as '--include-path <name>=<dir>', so it must be letters, digits, '_' and '-', " +
        "starting with a letter")

  /** The same check for a `.pc` name, which says what a `pkg-config` module may be called rather
   * than what this file's author may call something — so the refusal names the flag and stops
   * there, instead of suggesting a better name for something the author did not name.
   */
  private def checkPkgConfigName(name: String): Either[String, Unit] =
    if isPkgConfigName(name) then Right(())
    else
      Left(s"$FileName: '$name' is not usable as a pkg-config module's name — it is written on a " +
        "command line as '--include-path <name>=<dir>', so it must be letters, digits, '_', '-', " +
        "'.' and '+', starting with a letter")

  /** The `dependencies` block: what to fetch, at what version, and what a consumer may rename it to
   * (`reference/packages.md § Dependencies`,
   * `reference/packages.md § What a dependency's modules are called`).
   *
   * Each entry is checked here rather than at the fetch, because every mistake below is one the file
   * makes on its own — a coordinate with a scheme on the front, a major version that disagrees with
   * the path it rides in — and finding them needs neither the network nor the package. A build that
   * cannot mean anything should stop before it clones a repository to discover that.
   */
  private def readDependencies(root: ConfigObject, block: String = "dependencies")
      : Either[String, List[Dependency]] =
    block2(root, block) match
      case None => Right(Nil)
      case Some(deps) =>
        for
          entries <- collect(deps.fields.toList.sortBy(_._1)) {
            case (label, sub: ConfigObject) => readDependency(label, sub, block)
            case (label, _) => Left(s"$FileName: '$block.$label' is not a block — a dependency " +
              "is a git coordinate and a version, or a path")
          }
          _ <- unique(entries)
        yield entries

  /** Refuses a package named by both blocks.
   *
   * The two say different things about the same package — one that a consumer gets it, the other
   * that a consumer does not — and there is no reading under which both are meant. It is the
   * judgement `readOrigin` makes about a `git` beside a `path`: a closed vocabulary, where the
   * writer believes they have said one thing and a build that quietly picked one of them would let
   * them go on believing it.
   */
  private def notInBoth(deps: List[Dependency], dev: List[Dependency]): Either[String, Unit] =
    dev.map(_.label).toSet.intersect(deps.map(_.label).toSet).toList.sorted match
      case Nil => Right(())
      case both =>
        Left(s"$FileName: ${both.map(l => s"'$l'").mkString(", ")} " +
          s"${if both.length == 1 then "is" else "are"} named by both 'dependencies' and " +
          "'dev_dependencies' — a dependency reaches this package's consumers or it does not, and " +
          "it cannot do both. Name it once, in 'dependencies' if anything a consumer compiles " +
          "imports it")

  /** One entry, held to a shape in which every field is decided by the others.
   *
   * The unknown-key refusal is the same judgement `requires` makes about a misspelled capability: a
   * file saying `versoin = "1.0.0"` was written by somebody who believes they have pinned a version,
   * and ignoring the key would resolve whatever the repository's default branch happens to be while
   * they went on believing it.
   */
  private def readDependency(label: String, sub: ConfigObject, block: String)
      : Either[String, Dependency] = {
    val where = s"$block.$label"

    def unknown: Option[String] =
      sub.fields.keys.toList.sorted.find(!DependencyKeys.contains(_))

    for
      _      <- unknown.toLeft(()).left.map(k => s"$FileName: '$where.$k' is not something a " +
                  s"dependency says — the keys are ${DependencyKeys.toList.sorted.map(n => s"'$n'").mkString(", ")}")
      mount  <- readMount(sub, where)
      origin <- readOrigin(sub, where)
      opt    <- PackageFeatures.flag(sub, "optional", where, orElse = false)
      want   <- PackageFeatures.names(sub, "features", where)
      byDflt <- PackageFeatures.flag(sub, "default_features", where, orElse = true)
    yield Dependency(label, origin, mount, opt, want, byDflt)
  }

  /** `features` and `default_features` here are about the package being *depended on* — which of
   * its features this entry asks for — and they are accepted as written without being resolved,
   * because resolving one means reading that package's own manifest and nothing has fetched it yet.
   */
  private val DependencyKeys =
    Set("git", "version", "path", "mount", "optional", "features", "default_features")

  /** The blocks this compiler knows at the top level of a manifest, and the fields it knows inside
   * `package`.
   *
   * Both were **silently ignored** when unrecognized, which is the state card `0194 d` is about:
   * whatever 0.1.0's parser does with a key it does not know is the compatibility floor every later
   * manifest has to clear.
   */
  private val TopLevelKeys =
    Set("package", "targets", "capabilities", "requires", "dependencies", "dev_dependencies",
        "features", "allocator", "defines")

  private val PackageKeys = Set("name", "version", "sysl")

  /** Says what this compiler did not understand, and goes on.
   *
   * ==Warn and ignore, which is the middle of three==
   *
   * **Refusing** is the strictest and it is what the format could never undo: no key could ever be
   * added without breaking every released compiler, and fifty-one repositories in the org already
   * pin coordinates that a newer manifest would have to reach. **Ignoring silently** is what this
   * replaces, and its failure is a manifest that looks configured and is not — a mistyped
   * `dependencis` block does nothing and says nothing.
   *
   * Warning keeps the format extensible in the only direction that matters — a compiler older than
   * a key still builds the project — while a typo is still something somebody is told about. The
   * warning names the key and the file, or it is noise.
   *
   * **This does not soften the two refusals inside `dependencies` and `allocator`**, which are
   * argued where they stand and are about a *closed* vocabulary rather than a growing one: somebody
   * who wrote `versoin = "1.0.0"` believes they have pinned a version, and a build that resolved the
   * default branch while they believed it is a worse outcome than a stopped one.
   */
  private def unknownKeys(obj: ConfigObject, known: Set[String], prefix: String): List[String] =
    obj.fields.keys.toList.filterNot(known.contains)
      .map(k => s"$FileName: '$prefix$k' is not something this sysl understands, and is ignored — " +
        "either it is a misspelling, or it was added by a newer compiler")

  /** Where the source comes from — exactly one of the two, with the fields that one of them takes. */
  private def readOrigin(sub: ConfigObject, where: String): Either[String, Origin] =
    (string(sub, "git"), string(sub, "path")) match
      case (Some(_), Some(_)) =>
        Left(s"$FileName: '$where' names both a 'git' coordinate and a 'path' — a dependency is " +
          "fetched or it is beside you, and it cannot be both")

      case (None, None) =>
        Left(s"$FileName: '$where' names neither a 'git' coordinate nor a 'path'")

      case (None, Some(dir)) =>
        // A path dependency is whatever is in that directory right now, so a version beside one is
        // not a weaker promise than it looks — it is a promise nothing could keep.
        if sub.fields.contains("version") then
          Left(s"$FileName: '$where' names a 'path' and a 'version' — a path dependency is the " +
            "directory as it is now, and there is nothing to resolve a version against")
        else if dir.isEmpty then Left(s"$FileName: '$where.path' is empty")
        else Right(Origin.Local(dir))

      case (Some(coordinate), None) =>
        for
          text    <- string(sub, "version").toRight(s"$FileName: '$where' names a 'git' coordinate " +
                       "and no 'version' — a coordinate says which repository and a version says which of it")
          version <- Version.parse(text).left.map(e => s"$FileName: '$where.version': $e")
          _       <- checkCoordinate(coordinate, where)
          _       <- checkMajor(coordinate, version, where)
        yield Origin.Git(coordinate, version)

  /** What a coordinate is: a host and a path under it, as `git` would be given, and **not a URL**.
   *
   * The scheme is refused rather than accepted-and-stripped because the coordinate is *identity*
   * (`§ 9`) — two spellings of one package would mangle to two module names and link as two
   * incompatible copies, which is a link error a long way from the line that caused it.
   */
  private def checkCoordinate(coordinate: String, where: String): Either[String, Unit] =
    if coordinate.contains("://") then
      Left(s"$FileName: '$where.git' is a URL — a coordinate is written as 'github.com/you/thing', " +
        "since it is the package's identity and not the way it is fetched")
    else if coordinate.startsWith("/") || coordinate.endsWith("/") || coordinate.contains("//") then
      Left(s"$FileName: '$where.git' is not a coordinate — 'github.com/you/thing' is the shape")
    else if !coordinate.contains('/') then
      Left(s"$FileName: '$where.git' names a host and nothing under it")
    else Right(())

  /** `§ 4`'s rule that the major version rides in the path, checked both ways.
   *
   * Both halves matter and they catch opposite mistakes. A suffix that disagrees with the version
   * is a manifest asking for one package and naming another. A major of 2 or more with *no* suffix
   * is the mistake `/vN` exists to prevent: it would put two incompatible majors under one module
   * name, and `reference/modules.md § Separate compilation` mangles a module name into every
   * symbol, so the two would collide at the linker rather than anywhere a diagnostic could reach.
   */
  private def checkMajor(coordinate: String, version: Version, where: String): Either[String, Unit] =
    Dependency.majorSuffix(coordinate) match
      case Some(n) if n != version.major =>
        Left(s"$FileName: '$where' asks for $version from a coordinate ending '/v$n' — a major " +
          s"version is part of the coordinate, so '/v$n' holds ${n}.x and nothing else")

      case None if version.major >= 2 =>
        Left(s"$FileName: '$where' asks for $version from a coordinate with no '/v${version.major}' " +
          "— from the second major version on, the major rides in the coordinate, because two of " +
          "them are two packages rather than two versions of one")

      case _ => Right(())

  /** The root name a consumer may put a dependency under, which has to be a name a module could
   * have: it becomes the first segment of every import line that reaches the package (`§ 9`).
   */
  private def readMount(sub: ConfigObject, where: String): Either[String, Option[String]] =
    string(sub, "mount") match
      case None => Right(None)
      case Some(name) =>
        val legal = name.nonEmpty && (name.head.isLetter || name.head == '_') &&
          name.forall(c => c.isLetterOrDigit || c == '_')

        if legal then Right(Some(name))
        else Left(s"$FileName: '$where.mount' is '$name', which is not a name a module can have — " +
          "a mount becomes the first segment of an import line")

  /** The two ways one manifest can name one thing twice.
   *
   * A repeated **coordinate** is refused because `§ 4` gives a module one version: two entries for
   * one package are two answers to a question that has one, and taking either silently would make
   * which one depend on the order a file was read in. A repeated **mount** is the collision `§ 9`
   * exists to refuse, caught here in the one case where the file alone is enough to see it.
   */
  private def unique(entries: List[Dependency]): Either[String, Unit] = {
    def repeated[A](of: Dependency => Option[A]): Option[(A, List[Dependency])] =
      entries.groupBy(of).collectFirst { case (Some(key), group) if group.length > 1 => (key, group) }

    repeated { d =>
      d.origin match
        case Origin.Git(coordinate, _) => Some(coordinate)
        case _                         => None
    } match
      case Some((coordinate, group)) =>
        return Left(s"$FileName: '$coordinate' is named by ${group.map(d => s"'${d.label}'").mkString(" and ")} " +
          "— one package is one dependency, and a module gets one version of it")
      case None => ()

    repeated(_.mount) match
      case Some((mount, group)) =>
        Left(s"$FileName: ${group.map(d => s"'${d.label}'").mkString(" and ")} are both mounted as " +
          s"'$mount' — two packages cannot share a root name")
      case None => Right(())
  }

  /** Every item, or the **first** thing wrong, by the order the caller put them in.
   *
   * One message rather than all of them: a config file is short, its mistakes are usually one
   * mistake, and the alternative is a driver that prints a list before it has read a line of sysl.
   */
  private[sysl] def collect[A, B](items: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    items.foldLeft(Right(Nil): Either[String, List[B]]) { (acc, item) =>
      for
        done <- acc
        one  <- f(item)
      yield done :+ one
    }
}
