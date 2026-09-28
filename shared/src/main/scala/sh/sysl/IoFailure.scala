package sh.sysl

import java.io.IOException

import io.github.edadma.cross_platform.*

/** A failure of the filesystem, said in words, rather than an exception escaping to the user.
 *
 * **Every path the compiler reads or writes is somebody else's choice** — `-o`, `--header`, the
 * project root, `--lib`, `SYSL_LIB`, the cache directory `XDG_CACHE_HOME` or `HOME` leads to, a
 * manifest `sysl add` rewrites — so a directory that is really a file, a parent nobody may write, or a
 * cache on a read-only disk is an ordinary answer to give rather than a defect in the compiler. What
 * the platform's IO layer hands back for those is a `java.io.IOException`, often one whose message
 * is nothing but the path (`FileAlreadyExistsException: p4/main.sysl`) or nothing at all (Scala
 * Native's `createDirectory` refuses an unwritable parent with a bare `IOException`).
 *
 * So there are two halves, and they are the only two places this is decided:
 *
 *   - `describe` turns such an exception into what was wrong with which path, for every place that
 *     already catches one and for the boundary below.
 *   - **The boundary is `execute`** (`Main.scala`): whatever IO failure reaches it from any command
 *     becomes `sysl: error: …` and exit status 1. Only an IO failure is caught there — anything else
 *     escaping is a compiler bug and is left to say so with its stack trace.
 *
 * Where the exception cannot say what went wrong, the one call that knows the operation raises a
 * `Refusal` carrying the whole sentence: `Project.makeDirectory` is that call for every directory the
 * compiler makes, which is where nearly all of these arrive from.
 *
 * **The `java.nio.file` exceptions are read per platform** (`ioFailureOf`, `fileSystemFailure` in
 * each `platform` directory), because Scala.js has no `java.nio.file` and no
 * `UncheckedIOException` — this file may name `IOException` and nothing narrower.
 */
private[sysl] object IoFailure {

  /** An IO failure whose message is already the sentence a user should read. It is an
   * `IOException`, so every caller that tolerated the filesystem's own refusal tolerates this one.
   */
  final class Refusal(message: String) extends IOException(message)

  /** The IO failure a throwable is, if it is one — including one wrapped to cross a stream. */
  def unapply(e: Throwable): Option[IOException] = ioFailureOf(e)

  /** What was wrong, naming the path the failure was about wherever the exception carries one. */
  def describe(e: Throwable): String = e match
    case r: Refusal => r.getMessage
    case _ =>
      ioFailureOf(e) match
        case Some(inner) if inner ne e => describe(inner)
        case _ =>
          fileSystemFailure(e).getOrElse {
            // A stream's refusal reads `<path> (<reason>)`, on the JVM and on Native alike.
            Option(e.getMessage) match
              case Some(Parenthesised(path, why)) => s"'$path': ${lower(why)}"
              case _                              => plain(e)
          }

  /** The refusal for a directory that could not be made, worked out from what is there.
   *
   * **The question is asked of the filesystem rather than of the exception**, because the exception
   * differs by platform for the same cause — the JVM names an unwritable parent
   * `AccessDeniedException`, Native throws an `IOException` with no message — and what a user needs is
   * the cause.
   */
  def cannotMake(dir: String, cause: IOException): Refusal = {
    val parent = Project.parentOf(dir)

    val why =
      if exists(dir) then "a file is already there, and a directory is needed"
      else
        parent match
          case Some(p) if exists(p) && !isDirectory(p)     => s"'$p' is a file, and a directory is needed"
          case Some(p) if isDirectory(p) && !isWritable(p) => s"'$p' is not writable"
          case None if !isWritable(".")                    => "the current directory is not writable"
          case _                                           => describe(cause)

    new Refusal(s"cannot make the directory '$dir': $why")
  }

  private val Parenthesised = """(?s)(.*) \((.+)\)""".r

  private[sysl] def lower(s: String): String =
    if s.length > 1 && s(0).isUpper && s(1).isLower then s"${s(0).toLower}${s.substring(1)}" else s

  private def plain(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(s"the filesystem refused it (${e.getClass.getSimpleName})")
}
