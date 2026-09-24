package sh.sysl

import io.github.edadma.cross_platform.*

/** What `pkg-config` answered for one module: the flags its headers are compiled with, and the flags
 * its library is linked with.
 *
 * Kept as the tokens the tool printed rather than parsed into directories, which is deliberate.
 * `--cflags` is not only `-I`: SDL prints `-D_THREAD_SAFE` on some platforms and cairo's expands to
 * twelve include directories across five projects, and `--libs` carries `-L`, `-l` and the
 * `-Wl,-rpath` that decides whether a dynamically-linked program finds its library at *run* time.
 * Reading `-I` out and dropping the rest would answer the easy half of the question and silently lose
 * the half that is hard to diagnose.
 */
case class PkgConfigAnswer(cflags: List[String], ldflags: List[String])

/** The one machine-side question a package is allowed to ask: **where does this library live here?**
 *
 * ==Why this does not reopen `reference/packages.md § No build scripts, ever`==
 *
 * `§ 7` refuses build scripts, and the reason it gives is that a package must not run code on the
 * consumer's machine. Nothing here does. The package contributes a **name** — `sdl3` — and the
 * compiler asks a well-known tool what that name means on this machine, in exactly the way it already
 * asks `clang` what a `c const` measures to. What a package may say is unchanged from `§ 8`: *which*
 * library it needs, never where it is.
 *
 * ==Why the name is declared and not derived==
 *
 * Both available derivations are wrong. From the `@link` directive: the sdl3 package writes
 * `@link("SDL3")` and the module is `sdl3`, so it fails on case, and there is no rule that recovers
 * it — `-lSDL3` and `sdl3.pc` are two naming conventions that happen to share a word. From the
 * `headers` requirement's name: a name that happened to match some `.pc` file on this box would
 * satisfy a requirement nobody answered, which is the accident `§ 8` exists to prevent, and it would
 * do so on the machine of whoever built it and nowhere else.
 *
 * ==Absent is not failed==
 *
 * A machine with no `pkg-config`, or one whose `pkg-config` has never heard of the module, is a
 * machine exactly where it was before this existed: the declared requirement goes unanswered and the
 * build stops with the sentence `§ 8` already wrote, plus one naming the module that was looked for.
 * The probe can make a build succeed that would have failed; it can never make one fail that would
 * have succeeded.
 */
object PkgConfig {

  /** The tool, by name only. `pkg-config` on this machine is Homebrew's `pkgconf` wearing the
   * traditional name, and the two are compatible in the two queries asked here — so looking for a
   * second spelling would be looking for a program nobody has under a name nobody types.
   */
  private val Tool = "pkg-config"

  /** Whether the tool is on the PATH at all, asked once.
   *
   * Separate from a module query because it is the difference between two diagnostics: *this machine
   * has no pkg-config* is something the reader can act on with a `brew install`, where *pkg-config
   * does not know 'sdl3'* is about a library that is not installed. Answering both with the second
   * sentence would send somebody looking for a library they already have.
   */
  lazy val available: Boolean =
    try exec(Seq(Tool, "--version")).exitCode == 0
    catch case _: Throwable => false

  private var answers: Map[String, Either[String, PkgConfigAnswer]] = Map.empty

  /** What this machine says about `module`, or why it could not say.
   *
   * Memoized because the same module is asked for once per compilation unit that includes its
   * headers, and the answer cannot change inside one run. It is a process-lifetime cache rather than
   * a build one for the same reason `Toolchain.clangAvailable` is: what is installed does not move
   * while a compiler runs.
   */
  def query(module: String): Either[String, PkgConfigAnswer] =
    answers.get(module) match
      case Some(cached) => cached
      case None =>
        val answer = ask(module)

        answers += module -> answer
        answer

  private def ask(module: String): Either[String, PkgConfigAnswer] =
    if !available then
      Left(s"there is no '$Tool' on this machine to ask")
    else
      try
        val exists = exec(Seq(Tool, "--exists", module))

        if exists.exitCode != 0 then Left(s"'$Tool' knows no '$module'")
        else
          for
            c <- flags(module, "--cflags")
            l <- flags(module, "--libs")
          yield PkgConfigAnswer(c, l)
      catch case e: Throwable => Left(s"'$Tool' could not be run: ${e.getMessage}")

  /** What a library that is to be linked from its archive needs besides `query`'s answer
   * (`StaticLink`, `LinkMode`).
   *
   * `--static` adds the libraries this one links **privately** — which a shared library carries
   * inside itself and an archive does not, so they have to be on the link line. `libdir` is asked
   * because `pkg-config` leaves out a `-L` for a directory the linker searches anyway, and that is
   * exactly where the archive then is. Asked only after `query` has answered, so a module that does
   * not exist has already been refused with that sentence.
   */
  def queryStatic(module: String): Either[String, StaticAnswer] =
    for
      plain  <- query(module)
      static <- try flags(module, "--static", "--libs")
                catch case e: Throwable => Left(s"'$Tool' could not be run: ${e.getMessage}")
      libdir <- try flags(module, "--variable=libdir")
                catch case e: Throwable => Left(s"'$Tool' could not be run: ${e.getMessage}")
    yield StaticAnswer(plain.ldflags, static, libdir.headOption)

  /** One query's output, split the way a shell would split it.
   *
   * **A path holding a space is not handled and is refused rather than mangled.** pkg-config quotes
   * such a path in its output, and a splitter that ignored the quoting would hand clang two arguments
   * that are each half a directory — which fails as a missing header, naming neither the space nor
   * this line. The reader is told to say the path themselves, which is a flag that takes it whole.
   */
  private def flags(module: String, what: String*): Either[String, List[String]] = {
    val result = exec(Seq(Tool) ++ what ++ Seq(module))

    if result.exitCode != 0 then
      Left(s"'$Tool ${what.mkString(" ")} $module' failed: ${result.stderr.trim}")
    else if result.stdout.exists(c => c == '"' || c == '\'') then
      Left(s"'$Tool' answered for '$module' with a quoted path, which is one holding a space — say " +
        "where it is on the command line instead, which takes a path whole")
    else Right(result.stdout.split("\\s+").toList.filter(_.nonEmpty))
  }
}
