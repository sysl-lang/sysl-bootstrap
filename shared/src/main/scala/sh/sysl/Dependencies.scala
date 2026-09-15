package sh.sysl

/** A version, as `reference/packages.md § The major version rides in the coordinate` fixes them:
 * semver, three numbers, nothing else.
 *
 * Pre-release and build metadata are refused rather than ignored. Minimal Version Selection takes a
 * maximum over what the manifests ask for (`§ 5`), so every version has to be comparable to every
 * other by one total order — and semver's pre-release ordering is the part of the specification that
 * is least agreed on and most surprising. Nothing needs it yet, and admitting it later is an
 * addition; admitting it now and getting the order wrong is a resolution that silently picks the
 * wrong package.
 */
case class Version(major: Int, minor: Int, patch: Int) extends Ordered[Version] {

  def compare(that: Version): Int =
    if major != that.major then major - that.major
    else if minor != that.minor then minor - that.minor
    else patch - that.patch

  override def toString: String = s"$major.$minor.$patch"

  /** The git tag this version is published under, which is semver's own convention and Go's. */
  def tag: String = s"v$this"
}

object Version {

  /** **The compiler's own version**, which is the one version in the system that may carry a suffix.
   *
   * An interim is stamped `0.0.66-fcf4e33a` — the next patch, plus the commit it was built from — so
   * `parse` refuses it, correctly: nothing may *depend* on an interim, since nobody else can install
   * one. What a comparison against a package's floor wants is the numbers, and an interim satisfies
   * whatever they reach: `0.0.66-fcf4e33a` is dev heading for 0.0.66 and has everything 0.0.65
   * shipped. Cargo makes the same ruling for a nightly toolchain against `rust-version`.
   *
   * Nothing is refused here. A version the compiler cannot read is its own, so the answer is to make
   * no claim about it rather than to fail a build over it — `checkFloor` is then never the thing that
   * stops anybody.
   */
  def ofCompiler(text: String): Option[Version] = parse(text.takeWhile(_ != '-')).toOption

  def parse(text: String): Either[String, Version] = {
    val parts = text.split("\\.", -1)

    def number(s: String): Option[Int] =
      // Not `toIntOption`: that admits a sign and a leading zero, and `01.2.3` written by hand is a
      // typo rather than a version — semver refuses one for the same reason, since `01` and `1`
      // comparing equal would give one version two spellings. Held to digits, to a single leading
      // zero only where the number *is* zero, and to a length no version needs.
      Option.when(s.nonEmpty && s.length <= 9 && s.forall(_.isDigit) && (s.length == 1 || s.head != '0'))(s.toInt)

    if parts.length != 3 then
      Left(s"'$text' is not a version — a version is three numbers, as in '1.4.2'")
    else
      (number(parts(0)), number(parts(1)), number(parts(2))) match
        case (Some(a), Some(b), Some(c)) => Right(Version(a, b, c))
        case _ => Left(s"'$text' is not a version — a version is three numbers, as in '1.4.2'")
  }
}

/** Where a dependency's source comes from (`reference/packages.md § Dependencies`).
 *
 * The two are not variations of one shape: a git dependency always has a version, because that is
 * what a coordinate resolves *at*, and a path dependency never does, because the directory is
 * whatever it is on disk right now. Making them one case class with two optional halves would put
 * that rule in a validation pass instead of in the type, where every later reader would have to
 * re-derive it.
 */
enum Origin {

  /** A repository and the version to read it at. `coordinate` carries the `/vN` suffix where there
   * is one, because `§ 4` makes a major version *part of the identity* rather than a decoration on
   * it — `github.com/e/json` and `github.com/e/json/v2` are two packages that may both be linked.
   */
  case Git(coordinate: String, version: Version)

  /** A directory beside the consumer, for a package being developed alongside what uses it. */
  case Local(dir: String)
}

/** One entry of the `dependencies` block (`reference/packages.md § What a project is called`).
 *
 * `label` is the key it was written under. It is **not** the module name the code will use: `§ 9`
 * gives the root name to the dependency itself, so that a consumer's import lines match the
 * library's own documentation, and `mount` is what a consumer writes when two dependencies want one
 * name. The label is what diagnostics and `sysl deps` call this entry, and it has to be unique for
 * that reason alone.
 *
 * ==The three fields a feature decides==
 *
 * `optional` says this entry is not fetched or built unless a feature of **this** package names it,
 * so it is a claim about the package being described. `features` and `defaultFeatures` are claims
 * about the package being *depended on* — which of its features this consumer wants — and they are
 * read here and resolved against that package's own manifest elsewhere, since nothing at this point
 * has fetched it.
 *
 * `defaultFeatures` is true where the entry says nothing, because a dependency written the ordinary
 * way gets the package its author meant to ship; `optional` is false for the mirror-image reason,
 * since a dependency written the ordinary way is one the build takes.
 */
case class Dependency(
    label: String,
    origin: Origin,
    mount: Option[String] = None,
    optional: Boolean = false,
    features: List[String] = Nil,
    defaultFeatures: Boolean = true,
) {

  /** The identity `§ 9` mangles, caches and resolves by — the coordinate, with the separators a
   * module name is allowed to hold.
   *
   * A local dependency has no coordinate to be canonical, so it answers with its label. That is
   * honest rather than convenient: a package with no coordinate genuinely has no identity beyond
   * this project, which is `§ Open a`'s whole subject and is why a local dependency's name cannot be
   * relied on to be the same name anywhere else.
   */
  def canonical: String = origin match
    case Origin.Git(coordinate, _) => coordinate.replace('/', '.')
    case Origin.Local(_)           => label
}

object Dependency {

  /** How a coordinate becomes something `git` can clone.
   *
   * The `/vN` suffix is identity and not location (`§ 4`) — there is no branch or directory called
   * `v2` in the repository — so it comes off before the URL is built. HTTPS rather than SSH because
   * a public package should be fetchable by someone who has no account anywhere.
   */
  def cloneUrl(coordinate: String): String = s"https://${withoutMajor(coordinate)}.git"

  /** The coordinate with its `/vN` suffix removed, which is the repository itself. */
  def withoutMajor(coordinate: String): String =
    majorSuffix(coordinate) match
      case Some(_) => coordinate.take(coordinate.lastIndexOf('/'))
      case None    => coordinate

  /** The major version a coordinate declares in its path, where it declares one. `v0` and `v1` are
   * not suffixes: `§ 4` copies Go's rule that the first two majors ride in the bare path, so a
   * `github.com/e/json/v1` would be a second spelling of a package that already has one.
   */
  def majorSuffix(coordinate: String): Option[Int] = {
    val last = coordinate.drop(coordinate.lastIndexOf('/') + 1)

    Option.when(last.length >= 2 && last.head == 'v' && last.tail.forall(_.isDigit))(last.tail.toInt)
      .filter(_ >= 2)
  }
}
