package sh.sysl

import io.github.edadma.cross_platform.*

/** Getting a package's source onto this machine (`reference/packages.md § Dependencies`).
 *
 * A coordinate is a git repository and a version is a tag on it, so fetching is a clone at a tag and
 * nothing more — there is no registry to ask, no account to have, and no service that has to be up.
 * What that costs is the two things `§ 3` states rather than glosses: there is nowhere central to
 * search, and a repository that disappears takes its package with it. Vendoring answers the second.
 *
 * ==The cache is content-addressed by promise, not by hash==
 *
 * A fetched package lands under `<cache>/sysl/pkg/<coordinate>/@v<version>/`, which is one directory
 * per package and version and is shared by every project on the machine. Beside it sits a file
 * holding the tree hash that was computed when it was written, so a project whose `sysl.sum` covers
 * a package that some *other* project fetched first is still checked, and is checked without walking
 * the tree again. Trusting a populated cache blindly is the hole that would make `sysl.sum` a
 * formality on any machine with more than one project on it.
 *
 * The hash file is a **sibling** of the package directory rather than a file inside it, because
 * anything inside would be part of what the tree hash covers and the hash cannot cover itself.
 */
object Fetch {

  /** Where a package's source is once it is here, and where the clone is checked against. */
  case class Fetched(dep: Dependency, root: String, hash: Option[String])

  /** The directory every fetched package lives under.
   *
   * Under the cache rather than in the project, because a package at a version is the same bytes for
   * every project on the machine and the alternative is N copies of one library. This is the same
   * reasoning, and the same root, as the standard module's prebuilt artifact.
   */
  def cacheRoot(projectRoot: String = ""): Either[String, String] =
    override_.map(Right(_)).getOrElse {
      val vendored = s"$projectRoot/${Project.VendorDir}"

      if projectRoot.nonEmpty && isDirectory(vendored) then Right(vendored)
      else
        cacheDirectory.map(c => s"$c/sysl/pkg").toRight(
          "cannot find a cache directory to fetch packages into — set XDG_CACHE_HOME, or vendor the " +
            "dependencies so that nothing has to be fetched")
    }


  private var override_ : Option[String] = None

  /** Runs `body` against a cache somewhere else — **for tests only**.
   *
   * A suite that drives the whole driver has no other way to keep its packages out of the machine's
   * own cache, and putting them there would make a test's answer depend on what had been built
   * before it. The same shape and the same caveat as `AutoImport.including`: it is process-global,
   * so only one suite may use it, and that suite's tests must not run in parallel with each other.
   */
  private[sysl] def usingCache[T](path: String)(body: => T): T = {
    val saved = override_

    override_ = Some(path)
    try body
    finally override_ = saved
  }

  /** Where this coordinate at this version sits, fetched or not. */
  def directory(cache: String, coordinate: String, version: Version): String =
    s"$cache/$coordinate/@${version.tag}"

  /** The package's source on disk, cloning it if this machine has not got it, and checked against
   * `sums` either way.
   *
   * Returns **the hash however it was arrived at** — computed from the clone, or read from what was
   * recorded beside a package already here. Returning it only for a fresh clone would have meant
   * that a project depending on a package some other project had already fetched wrote no
   * `sysl.sum` line at all, and so was never checked afterwards. Whether the line is *new* is the
   * caller's question, and it can only be answered against the sums it holds.
   */
  def ensure(dep: Dependency, sums: Sums, cache: String): Either[String, Fetched] = dep.origin match
    case Origin.Local(dir) =>
      // Nothing to fetch and nothing to check: `§ 6` keeps no entry for a directory that is expected
      // to change under you, which is the whole point of developing a package beside its consumer.
      if isDirectory(dir) then Right(Fetched(dep, dir, None))
      else Left(s"'${dep.label}' is a path dependency, and '$dir' is not a directory")

    case Origin.Git(coordinate, version) =>
      val dir = directory(cache, coordinate, version)

      if isDirectory(dir) then verify(dep, coordinate, version, dir, sums).map(Fetched(dep, dir, _))
      else clone(dep, coordinate, version, dir, sums).map(hash => Fetched(dep, dir, Some(hash)))

  /** A cache entry checked against `sysl.sum`, by way of the hash recorded when it was written.
   *
   * The tree is not walked again. What is being asked is whether *this* project's record agrees with
   * what was verified when the directory was populated, and that question is answered by two strings
   * — walking the tree would answer a different and weaker question, since a cache someone had
   * edited would simply hash to whatever it now holds.
   */
  private def verify(dep: Dependency, coordinate: String, version: Version, dir: String,
                     sums: Sums): Either[String, Option[String]] =
    (sums.hashOf(coordinate, version), recorded(dir)) match
      case (None, got) => Right(got)

      case (Some(want), Some(got)) if want == got => Right(Some(got))

      case (Some(want), Some(got)) =>
        Left(mismatch(dep, coordinate, version, want, got))

      // A directory with no hash beside it was not written by this compiler — an interrupted fetch,
      // or something a person put there. It is not evidence about anything, so it is refused rather
      // than trusted or silently replaced.
      case (Some(_), None) =>
        Left(s"'$dir' holds ${coordinate} ${version.tag} but nothing recording what it hashed to, so " +
          s"${Sums.FileName} cannot be checked against it — remove that directory and build again")

  /** Clones at the tag, checks what arrived, and only then puts it where the build will read it.
   *
   * The clone goes to a sibling directory and is moved into place (`Publish`), so nothing ever
   * reads a directory that is half a package. It is a sibling rather than a temporary somewhere else
   * so that the move is a rename within one filesystem instead of a copy that can fail halfway.
   *
   * **The sibling's name is this fetch's own.** It was a fixed `<dir>.partial`, which two fetches of
   * one package at one version — two builds starting at once on a cold cache — both cleared and
   * cloned into, each deleting the other's clone under it. Now each clones apart, and the one that
   * finishes second finds a whole package already in place, discards its copy and carries on.
   */
  private def clone(dep: Dependency, coordinate: String, version: Version, dir: String,
                    sums: Sums): Either[String, String] = {
    val partial = Publish.pending(dir)
    val url     = Dependency.cloneUrl(coordinate)

    try
      Project.parentOf(dir).foreach(Project.makeDirectories)

      Console.err.println(s"fetching $coordinate ${version.tag}")

      // `--depth 1` at the tag: a package is read at one version and its history is not part of what
      // is compiled, so fetching the rest of it is bytes nobody asks for. It also keeps the clone out
      // of the tree hash's way, since `.git` is excluded from that either way.
      val result = exec(Seq("git", "clone", "--quiet", "--depth", "1", "--branch", version.tag,
        url, partial))

      if result.exitCode != 0 then
        removeTree(partial)
        return Left(s"cannot fetch $coordinate ${version.tag} from $url:\n${result.stderr.trim}")

      val fetched =
        for
          hash <- Hashing.treeHash(partial)
          _    <- sums.hashOf(coordinate, version) match
                    case Some(want) if want != hash => Left(mismatch(dep, coordinate, version, want, hash))
                    case _ => Right(())
          // The hash first, so a directory is never there without what vouches for it — a reader
          // finding one without the other refuses it (`verify`).
          _    <- Publish.text(s"$dir.hash", s"$hash\n")
          _    <- Publish.directory(partial, dir)
        yield hash

      // Whichever step refused, the clone is this fetch's alone and nobody else will clear it.
      if fetched.isLeft then removeTree(partial)
      fetched
    catch
      case e: Exception =>
        removeTree(partial)
        Left(s"cannot fetch $coordinate ${version.tag}: ${e.getMessage}")
  }

  /** The one diagnostic that says the fetch was not the package that was promised.
   *
   * It says what to do about it, because the honest answer depends on something the compiler cannot
   * know: either the upstream moved and the record should follow it, or the record is right and what
   * arrived is not the package.
   */
  private def mismatch(dep: Dependency, coordinate: String, version: Version, want: String,
                       got: String): String =
    s"'${dep.label}' does not hash to what ${Sums.FileName} records for $coordinate ${version.tag}\n" +
      s"  recorded: $want\n" +
      s"  fetched:  $got\n" +
      "A version's content is not supposed to change. Either the tag was moved and the recorded " +
      "line should be replaced deliberately, or what was fetched is not the package that was published."

  /** The hash written beside a cached package when it was fetched, where there is one. */
  private def recorded(dir: String): Option[String] =
    Option.when(isFile(s"$dir.hash"))(readFile(s"$dir.hash").trim).filter(_.nonEmpty)

  /** Removes a directory and everything under it, whether or not it is there.
   *
   * `cross_platform` deletes one entry at a time, and a clone is a tree — most of it `.git`. Used on
   * the failure paths, where the thing being removed is a partial fetch this run made and nothing
   * else has seen.
   */
  private[sysl] def removeTree(path: String): Unit =
    try
      if isDirectory(path) then
        listFiles(path).foreach(removeTree)
        deleteFile(path)
      else if exists(path) then deleteFile(path)
    catch case _: Exception => ()
}
