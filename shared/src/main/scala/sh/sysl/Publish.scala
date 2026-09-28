package sh.sysl

import io.github.edadma.cross_platform.*

import java.util.concurrent.atomic.AtomicLong

/** Putting a file or a directory where other processes may be reading it, so that none of them ever
 * sees half of one.
 *
 * Everything the compiler keeps in the user's cache — the standard module, a `sysl run` binary, the
 * test list beside it, a fetched package — sits at a path every compilation of the same input
 * computes, and nothing keeps two such compilations apart: two worktrees at one commit, two suites
 * driving one compiler, a build and an editor's check. Writing at that path directly hands every
 * concurrent reader whatever the writer has got to so far — an executable that is half its bytes, or
 * a test list that decodes cleanly to fewer tests than the suite has.
 *
 * So the thing is written under a **pending** name and then **renamed** onto its place. A rename
 * within one directory is one step for the filesystem: a reader finds the whole of the previous entry
 * or the whole of the new one, never a mixture, and never a moment with nothing there. The pending
 * name is beside the destination rather than in the system's temporary directory because a rename is
 * only a rename within one filesystem, and the cache need not be on the same one as `/tmp`.
 *
 * **Two writers must never agree on a pending name**, or one of them publishes the other's
 * half-written file. The name carries a token drawn once per process and a counter taken once per
 * call, so two processes differ in the first and two threads of one process in the second.
 */
object Publish {

  /** Drawn once per process. Random rather than a process id, which not every platform this runs on
   * will say — and 64 random bits collide between two live processes about as often as a disk
   * returns the wrong sector.
   */
  private val token = java.lang.Long.toHexString(new scala.util.Random().nextLong())

  private val counter = new AtomicLong

  /** A name beside `target` that no other call — in this process or any other — will answer. The
   * caller writes there and then hands both names to [[file]] or [[directory]].
   */
  def pending(target: String): String = s"$target.pending-$token-${counter.incrementAndGet()}"

  /** `pending`, renamed onto `target`, replacing whatever was there. On failure the pending file is
   * removed, so a full disk or a read-only directory leaves nothing behind but the previous entry.
   * The executable bit travels with the rename, so a binary linked under the pending name is still
   * one when it arrives.
   */
  def file(pending: String, target: String): Either[String, Unit] =
    try Right(moveFile(pending, target))
    catch
      case e: Exception =>
        Project.discard(pending)
        Left(s"cannot put '$target' in place: ${IoFailure.describe(e)}")

  /** `text` written to `target` by way of a pending name, so a reader never sees a prefix of it. */
  def text(target: String, text: String): Either[String, Unit] = {
    val staged = pending(target)

    try
      writeFile(staged, text)
      file(staged, target)
    catch
      case e: Exception =>
        Project.discard(staged)
        Left(s"cannot write '$target': ${IoFailure.describe(e)}")
  }

  /** A directory built at `pending`, renamed onto `target` — unless another writer got there first.
   *
   * **Losing that race is success, not failure.** Two fetches of one package at one version are
   * building the same thing, and the one that finished first has already put a whole directory
   * there; a rename refuses to replace a directory that has anything in it, so the winner is never
   * disturbed. The loser removes its own copy and answers as though it had written the one that is
   * there.
   */
  def directory(pending: String, target: String): Either[String, Unit] =
    try Right(moveFile(pending, target))
    catch
      case e: Exception =>
        Fetch.removeTree(pending)

        if isDirectory(target) then Right(())
        else Left(s"cannot put '$target' in place: ${IoFailure.describe(e)}")
}
