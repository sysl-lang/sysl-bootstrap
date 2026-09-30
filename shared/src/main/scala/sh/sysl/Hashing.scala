package sh.sysl

import io.github.edadma.cross_platform.*

/** The content hash a fetched package is held to (`reference/packages.md § sysl.sum`).
 *
 * `sysl.sum` records what a package's source *was* when it was first resolved, and every fetch
 * after that is checked against it. What that protects against is the class of change a version
 * number cannot describe — a tag moved to point somewhere else, a repository rewritten, a mirror
 * serving something other than what the author published. Minimal Version Selection has already
 * made the *selection* deterministic (`reference/packages.md § Which version you get`), so there is
 * nothing here about which version was chosen; this is only ever about whether the bytes are the
 * ones that were promised.
 *
 * **This is not `LibraryArtifact.fingerprint` and must not become it.** That one answers "did my own
 * library change, so is the cached artifact stale", where FNV-1a is the right tool and a
 * cryptographic hash would be waste. This one answers "is somebody else's tree the one it claims to
 * be", which is a question with an adversary in it, and a hash an adversary can find a collision in
 * answers it wrongly. The two never share code.
 *
 * ==The definition is independent of the tool that computes it==
 *
 * The digest itself is computed by the platform's own utility rather than in Scala, which is the
 * same arrangement the compiler already has with `clang`, `llvm-ar` and `why3` — and a fetch needs
 * `git` in any case, so a package build already depends on tools that are not this program. What
 * that must never leak into is the *definition*: a hash written into somebody's `sysl.sum` is a
 * promise that outlives whatever computed it, so the tree hash below is specified over a canonical
 * listing and not over "whatever the utility printed". Replacing the shell-out with an
 * implementation in Scala therefore changes nothing anyone has committed, which is the property that
 * makes borrowing the utility safe rather than expedient.
 */
object Hashing {

  /** How a digest is written where one is recorded, so that a file naming an algorithm can be read
   * by a later version that offers more than one.
   */
  val Prefix: String = "sha256:"

  /** The tree hash of a package rooted at `root`: **`SHA-256` of the canonical listing**.
   *
   * The listing is one line per file, `<lowercase hex digest><two spaces><relative path>`, newline
   * terminated, sorted by the relative path — which is the shape Go's `h1:` uses and is chosen for
   * the same reason: it is a total order over content that no filesystem detail can perturb. The
   * paths are relative to `root` and separated by `/` whatever the platform writes, so a package
   * hashes to the same value wherever it was unpacked and on whichever machine.
   *
   * **What is in it is every file except a `.git` anywhere in the path.** The repository's own
   * bookkeeping is not the package: it holds timestamps, packing choices and remote names that
   * differ between two clones of the very same commit, so including it would make the hash of an
   * honest fetch depend on how it was fetched.
   */
  def treeHash(root: String): Either[String, String] =
    for
      paths  <- files(root)
      digest <- listingHash(root, paths)
    yield s"$Prefix$digest"

  /** Every file under `root` that the hash covers, as absolute paths in the order the listing wants
   * — which is the order of their *relative* paths, since that is what the definition sorts by and
   * the two orders are not the same once a root's own name is in front of them.
   */
  private[sysl] def files(root: String): Either[String, List[String]] = {
    def walk(path: String): List[String] = {
      val entries = listDirectory(path).toList

      entries.filter(isFile) ::: entries.filter(isDirectory)
        .filter(d => Project.basename(d) != ".git")
        .flatMap(walk)
    }

    val found = try walk(root)
    catch case e: Exception => return Left(s"cannot read $root: ${IoFailure.describe(e)}")

    // A name holding a newline would be indistinguishable from two entries once the listing is a
    // text file, and the digest utility's own output has the same problem one step earlier. Refused
    // rather than escaped: nothing legitimate is being turned away, and an escaping rule is a second
    // thing the definition would have to pin forever.
    found.find(_.contains('\n')) match
      case Some(bad) => Left(s"cannot hash '$bad' — a file name may not hold a newline")
      case None      => Right(found.sortBy(relative(root, _)))
  }

  /** A file's path as the listing spells it: relative to the package root, `/` separated. */
  private[sysl] def relative(root: String, path: String): String = {
    val normal = path.replace('\\', '/')
    val base   = root.replace('\\', '/').stripSuffix("/")

    if normal.startsWith(s"$base/") then normal.drop(base.length + 1) else normal
  }

  /** The listing built and hashed, which is the whole of the definition in one place.
   *
   * The listing goes to a temporary file rather than down a pipe because `exec` hands a process its
   * arguments and not its input, and because the file is what makes the definition checkable by
   * hand: the same two commands run in a shell produce the same number.
   */
  private def listingHash(root: String, paths: List[String]): Either[String, String] =
    for
      digests <- digestsOf(paths)
      listing = paths.zip(digests).map((p, d) => s"$d  ${relative(root, p)}\n").mkString
      hash    <- digestOfText(listing)
    yield hash

  /** The digest of each of `paths`, in the order given.
   *
   * **Answered by position rather than by parsing the name back.** Both utilities print the path
   * they were handed, and both mark the mode they read it in — so the name in the output is a thing
   * with a format of its own, while the order is not. Reading only the first field and holding the
   * count to the input's is the arrangement with less to go wrong, and a utility that reordered its
   * output would be caught by the count rather than silently mismatching a digest to a file.
   */
  private[sysl] def digestsOf(paths: List[String]): Either[String, List[String]] =
    if paths.isEmpty then Right(Nil)
    else
      for
        command <- digestCommand
        // In batches, because the whole of a package goes on one command line and a large one would
        // otherwise be refused by the system rather than by anything here. The batch boundary cannot
        // affect the answer: each file is hashed alone whichever call it lands in.
        batches <- collect(paths.grouped(BatchSize).toList)(batch => run(command, batch))
      yield batches.flatten

  /** How many files go on one command line. Well inside every system's argument limit at any path
   * length a package will have, and large enough that a package is one or two calls.
   */
  private val BatchSize = 256

  private def run(command: List[String], batch: List[String]): Either[String, List[String]] = {
    val result =
      try exec(command ::: batch)
      catch case e: Exception => return Left(s"cannot run '${command.head}': ${IoFailure.describe(e)}")

    if result.exitCode != 0 then
      return Left(s"'${command.head}' failed (exit ${result.exitCode}):\n${result.stderr.trim}")

    val digests = result.stdout.linesIterator.filter(_.nonEmpty).map(_.takeWhile(_ != ' ')).toList

    if digests.length != batch.length then
      Left(s"'${command.head}' answered for ${digests.length} files where ${batch.length} were asked")
    else if digests.exists(d => d.length != 64 || !d.forall(hex)) then
      Left(s"'${command.head}' printed something that is not a SHA-256 digest")
    else Right(digests.map(_.toLowerCase))
  }

  private def hex(c: Char): Boolean = c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  /** The digest of a string, by way of a file, since the utilities read files. */
  private def digestOfText(text: String): Either[String, String] = {
    val file = createTempFile("sysl-sum-", "")

    try
      writeFile(file, text)
      digestsOf(List(file)).map(_.head)
    catch case e: Exception => Left(s"cannot hash: ${IoFailure.describe(e)}")
    finally Project.discard(file)
  }

  /** The utility this machine hashes with, chosen by what the machine is rather than by trying each
   * in turn. Both print the same `<digest><two spaces><name>` line, which is why one reader serves
   * both, and each is part of its system rather than something to install: `shasum` ships with
   * macOS, and `sha256sum` is in coreutils and in busybox, so a minimal image has it too.
   */
  private def digestCommand: Either[String, List[String]] = Target.host.map(_.os) match
    case Some(Os.MacOS) => Right(List("shasum", "-a", "256"))
    case Some(Os.Linux) => Right(List("sha256sum"))
    case _ =>
      Left(s"cannot hash a package on ${Target.hostMachineShown} — verifying what was fetched needs " +
        "a SHA-256 utility, and this machine is not one this compiler knows how to find one on")

  /** Every item, or the first thing wrong — the same shape and the same reasoning as
   * `PackageConfig`'s, and for the same reason: one message, because these are not independent
   * failures and a list of them describes one broken fetch several times over.
   */
  private def collect[A, B](items: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    items.foldLeft(Right(Nil): Either[String, List[B]]) { (acc, item) =>
      for
        done <- acc
        one  <- f(item)
      yield done :+ one
    }
}
