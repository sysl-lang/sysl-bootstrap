package sh.sysl

import java.io.IOException

// `IoFailure`'s platform half. Scala.js has no `java.nio.file` and no `UncheckedIOException`, so an
// IO failure here is an `IOException` and there is no narrower refusal to read.

private[sysl] def ioFailureOf(e: Throwable): Option[IOException] = e match
  case io: IOException => Some(io)
  case _               => None

private[sysl] def fileSystemFailure(e: Throwable): Option[String] = None
