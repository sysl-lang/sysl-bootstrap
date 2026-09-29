package sh.sysl

/** Which compiler this is, for a cache key: the version, and which build of that version.
 *
 * **The version alone names a release and not a compiler.** A development tree holds
 * `BuildInfo.version` constant while the compiler under it changes, so a cache keyed on the version
 * hands a newer build whatever an older build of the same version left there — a binary `sysl run`
 * replays without compiling (`RunCache`), or a standard module every program links
 * (`LibraryArtifact.stdDefault`). Nothing announces it: the program simply behaves the way the
 * compiler before the change made it behave. That is how the 0.0.150 release gate came to report a
 * dependency's test from a fixture built before the fix it asserted.
 *
 * `BuildInfo.build` is a digest of the compiler's own sources and build definition, made by
 * `build.sbt` when the compiler is compiled, so it moves exactly when something that could change
 * what the compiler emits does, and is fixed for a released binary.
 *
 * Spelled `<version>+<build>`, the form a semantic version gives build metadata.
 */
object CompilerIdentity {

  /** **Per thread**, the same shape `RunCache.usingCache` has: a suite standing in for another build
   * runs beside suites driving the real one, and a process-wide override would move their keys too.
   */
  private val override_ = new ThreadLocal[Option[String]] {
    override def initialValue(): Option[String] = None
  }

  def current: String = override_.get.getOrElse(s"${BuildInfo.version}+${BuildInfo.build}")

  /** `body`, run as though this were the compiler `id` names — which is how a test puts an entry in a
   * cache that another build of this version made.
   */
  private[sysl] def as[T](id: String)(body: => T): T = {
    val saved = override_.get

    override_.set(Some(id))
    try body
    finally override_.set(saved)
  }
}
