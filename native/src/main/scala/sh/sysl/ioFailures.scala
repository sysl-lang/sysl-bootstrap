package sh.sysl

import java.io.{IOException, UncheckedIOException}
import java.nio.file.{AccessDeniedException, DirectoryNotEmptyException, FileAlreadyExistsException,
  FileSystemException, NoSuchFileException, NotDirectoryException}

// The `java.nio.file` half of `IoFailure`, which Scala.js cannot name. The JVM and Native
// directories each hold this file word for word, since both platforms raise the same classes.

/** The IO failure a throwable is, if it is one — an `UncheckedIOException` is how a stream carries
 * one out of a lambda, so it counts as its cause.
 */
private[sysl] def ioFailureOf(e: Throwable): Option[IOException] = e match
  case io: IOException         => Some(io)
  case u: UncheckedIOException => Option(u.getCause)
  case _                       => None

/** What a `java.nio.file` refusal was about, naming its path; `None` for anything else. */
private[sysl] def fileSystemFailure(e: Throwable): Option[String] = e match
  case x: FileAlreadyExistsException => Some(s"'${refusedPath(x)}' already exists")
  case x: NoSuchFileException        => Some(s"'${refusedPath(x)}' does not exist")
  case x: NotDirectoryException      => Some(s"'${refusedPath(x)}' is not a directory")
  case x: AccessDeniedException      => Some(s"'${refusedPath(x)}': permission denied")
  case x: DirectoryNotEmptyException => Some(s"'${refusedPath(x)}' is a directory that is not empty")
  case x: FileSystemException =>
    val why = Option(x.getReason).filter(_.nonEmpty).map(IoFailure.lower).getOrElse("the filesystem refused it")
    Some(s"'${refusedPath(x)}': $why")
  case _ => None

private def refusedPath(x: FileSystemException): String = Option(x.getFile).getOrElse("")
