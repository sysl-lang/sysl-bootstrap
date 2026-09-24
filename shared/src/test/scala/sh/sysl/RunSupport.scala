package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Shared helper for the Tier-2 run suites: compile a program all the way to a native binary,
 * run it, and return its stdout. Needs an LLVM toolchain, so it cancels cleanly (rather than
 * failing) when `clang` is absent.
 */
trait RunSupport extends Matchers { this: Assertions =>

  /** Every run-tier compilation, in one place, against the **prebuilt** standard module where the
   * toolchain can build one.
   *
   * That is the compilation an ordinary `sysl build` performs: the library's determined half is
   * linked from the artifact rather than emitted into this program and handed to clang again. Absent
   * a toolchain there is no artifact and this is the compilation the suite has always done — which is
   * the case the `assume` above each helper has already cancelled for.
   *
   * It goes through `Stdlib.resolve`, which is the call the driver itself makes, rather than through
   * a routine of the suite's own — so a test is compiled against the artifact an ordinary build
   * would find, at the path an ordinary build would find it, and a change to how one is made cannot
   * reach the compiler without reaching the suite.
   */
  protected def prebuiltStd: Option[Stdlib.Resolved] =
    Stdlib.resolve(Stdlib.Choice.Default(), Target.default).toOption

  private def compiled(sources: List[Source], args: List[String],
                       level: String = Toolchain.defaultOptimization): Either[String, (Int, String)] =
    prebuiltStd match {
      case Some(Stdlib.Resolved(std, precompiled, Some(archive))) =>
        Toolchain.runIr(Compiler.compiledWith(sources, Nil, Target.default, precompiled, Some(std)),
                        args, List(archive), level)
      case _ =>
        Toolchain.runIr(Compiler.compiledWith(sources, Nil, Target.default, Set.empty, None),
                        args, Nil, level)
    }

  /** The program, run. `optimize` is the level clang is given, and it defaults to the one every
   * other run in the suite uses — what asks for another is a claim that is *about* the level, which
   * `BecomeTests` is: a guaranteed tail call has to hold where nothing is eliminated by luck.
   */
  protected def run(src: String, optimize: String = Toolchain.defaultOptimization): String = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(List(Source("<input>", src)), Nil, optimize) match {
      case Right((0, out))    => out
      case Right((code, out)) => fail(s"program exited with $code:\n$out")
      case Left(err)          => fail(err)
    }
  }

  /** The same, started with arguments — what a `main(args: []string)` is handed. The zeroth is the
   * executable's own path, which the platform supplies and no test can predict, so a test asserts
   * about `args[1..]` or about the count.
   */
  protected def runWith(src: String, args: String*): String = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(List(Source("<input>", src)), args.toList) match {
      case Right((0, out))    => out
      case Right((code, out)) => fail(s"program exited with $code:\n$out")
      case Left(err)          => fail(err)
    }
  }

  /** The same, given bytes for the program to read: they are written to a temporary file whose path
   * arrives as `args[1]`, and the file is removed however the run ends.
   *
   * It is a file rather than standard input because the harness hands a compiled program a **closed**
   * one — `cross_platform`'s `exec` offers no way to write to a child — so a test that wants real
   * bytes to arrive passes a path and lets the program open it. That exercises everything above the
   * file descriptor, which is all of the reading surface; only the choice of descriptor is left out.
   */
  protected def runReading(src: String, input: String): String = {
    assume(Toolchain.clangAvailable, "clang not available")

    val path = createTempFile("sysl-input-", ".txt")

    writeFile(path, input)

    try runWith(src, path)
    finally deleteFile(path)
  }

  /** Compile a program and run it **attached to a pseudo-terminal**, with `input` typed at it.
   *
   * **This is the only helper that can reach code gated on `is_tty`, and it exists because a whole
   * facility was otherwise being merged unexercised.** Every other run here hands the program a
   * closed standard input, so `sysl.posix.tty.raw` takes its early exit and returns `false` — which
   * makes the *refusing* branch the only one a suite could see, and the succeeding branch, the one
   * that changes a real terminal's settings, unreachable. It is not a small branch: it spawns a
   * shell, and getting it wrong once turned a Ctrl-C into a deadlock rather than a tidy exit.
   *
   * `script` is what supplies the terminal. Its arguments differ between the BSD one macOS ships and
   * util-linux's, so the host decides which spelling to use — and a host with no `script` cancels the
   * test rather than failing it, exactly as a missing `clang` does.
   *
   * **The input is delayed, and that is not superstition.** `script` writes whatever it is given to
   * the terminal as soon as it has one, while the program is still starting; anything arriving before
   * the program calls `stty` is handled by the line discipline that is still in canonical mode, and
   * is then discarded when the mode changes. A person typing cannot lose that race and a pipe always
   * wins it, so the sleep is what makes the harness behave like the situation being tested.
   *
   * The output carries `script`'s own noise — a `^D`, and the terminal's echo of the input while it
   * was still cooked — so a caller should assert with `include` rather than against the whole string.
   */
  protected def runOnTty(src: String, input: String, after: String = ""): String = {
    assume(Toolchain.clangAvailable, "clang not available")
    assume(exec(List("sh", "-c", "command -v script")).exitCode == 0, "script not available")

    val exe   = createTempFile("sysl-tty-", "")
    val typed = createTempFile("sysl-typed-", ".txt")
    // What runs *inside* the terminal, as a file rather than as a quoted argument: `script`'s two
    // dialects disagree about how a command is passed, and nesting quotes through both of them is a
    // way to be wrong on one platform only. `after` is how a test asks a question of the terminal
    // once the program has left it — whether the mode was put back, which is the claim that matters
    // most and cannot be asked from inside a program that has exited.
    val session = createTempFile("sysl-session-", ".sh")

    writeFile(typed, input)
    writeFile(session, s"$exe\n$after\n")

    val compiled = prebuiltStd match {
      case Some(Stdlib.Resolved(std, precompiled, Some(archive))) =>
        (Compiler.compiledWith(List(Source("<input>", src)), Nil, Target.default, precompiled,
                               Some(std)), List(archive))
      case _ =>
        (Compiler.compiledWith(List(Source("<input>", src)), Nil, Target.default, Set.empty, None), Nil)
    }

    try
      compiled._1.flatMap(c => Toolchain.build(c.ir, exe, Target.default, compiled._2,
                                               Toolchain.defaultOptimization, c.links)) match {
        case Left(err) => fail(err)
        case Right(_)  =>
          // BSD `script` takes the command as trailing words and the typescript first; util-linux
          // takes the command through `-c` and the typescript last. Neither accepts the other's form.
          val under =
            if Target.default.name.contains("macos") then s"script -q /dev/null sh $session"
            else s"script -qc 'sh $session' /dev/null"

          // **The sleep is inside the subshell on purpose.** Written as `sleep 1; cat … | script …`
          // it delays the whole pipeline, so the program starts *after* the wait and the input is
          // still queued ahead of its `stty` — the exact race the delay exists to avoid, reinstated
          // by precedence. Grouped, `script` starts at once and the typing arrives a second later.
          exec(List("sh", "-c", s"( sleep 1; cat $typed ) | $under")).stdout
      }
    finally
      deleteFile(exe)
      deleteFile(typed)
      deleteFile(session)
  }

  /** A program's whole outcome — its status, its standard output and its standard error, kept
   * apart — for the runs where which stream something came out of is the thing being asserted.
   *
   * `run` and `runWith` above fail a program that did not exit cleanly, which is right for almost
   * everything and wrong for a command-line parser: stopping with a status and a diagnostic is
   * behaviour it is *for*, and a helper that treated it as a failure could not test it. Nothing is
   * asserted here; the caller reads the three parts and says what it expects of each.
   */
  protected case class Outcome(status: Int, out: String, err: String)

  protected def outcomeOf(src: String, args: String*): Outcome = {
    assume(Toolchain.clangAvailable, "clang not available")

    val sources = List(Source("<input>", src))
    val result = prebuiltStd match {
      case Some(Stdlib.Resolved(std, precompiled, Some(archive))) =>
        Toolchain.compileAndRunFully(sources, Nil, args.toList, Some(std), precompiled, List(archive))
      case _ =>
        Toolchain.compileAndRunFully(sources, Nil, args.toList, None, Set.empty, Nil)
    }

    result match {
      case Right((status, out, err)) => Outcome(status, out, err)
      case Left(e)                   => fail(e)
    }
  }

  /** The same, for a program written as several files. */
  protected def runOf(fs: (String, String)*): String =
    ran(fs.toList.map { case (name, text) => Source(name, text) })

  /** The same, for a project whose files sit in directories — each one written as the dotted path
   * the driver derives from where the file was found, with `""` for the project root.
   */
  protected def runIn(fs: (String, String, String)*): String =
    ran(fs.toList.map { case (dir, name, text) =>
      Source(name, text, if dir.isEmpty then Nil else dir.split('.').toList)
    })

  private def ran(sources: List[Source]): String = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(sources, Nil) match {
      case Right((0, out))    => out
      case Right((code, out)) => fail(s"program exited with $code:\n$out")
      case Left(err)          => fail(err)
    }
  }

  /** Asserts that a program stops itself rather than running past a failed check. Every runtime
   * check traps, so what is observable is the exit status, not the output.
   */
  protected def exits(src: String): Unit = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(List(Source("<input>", src)), Nil) match {
      case Right((code, _)) => code should not be 0
      case Left(err)        => fail(err)
    }
  }

  /** The same, for a program written as several files. */
  protected def exitsOf(fs: (String, String)*): Unit = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(fs.toList.map { case (name, text) => Source(name, text) }, Nil) match {
      case Right((code, _)) => code should not be 0
      case Left(err)        => fail(err)
    }
  }

  /** Asserts the exact status a program exits with, for the one thing that can choose it: a call to
   * `exit`. `exits` above only asks whether the status was non-zero, which is all a trap has to say.
   */
  protected def exitsWith(src: String, code: Int): Unit = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(List(Source("<input>", src)), Nil) match {
      case Right((c, _)) => c shouldBe code
      case Left(err)     => fail(err)
    }
  }

  /** Asserts that a program stops itself *and says why* — the shape of a panic, as against the
   * silent trap `exits` checks for. The diagnostic reaches the pipe because the panic leaves
   * through `exit`, which flushes what was written; `abort` would not have to.
   */
  protected def panics(src: String, message: String): Unit = {
    assume(Toolchain.clangAvailable, "clang not available")

    compiled(List(Source("<input>", src)), Nil) match {
      case Right((0, out)) => fail(s"expected the program to stop, but it exited cleanly:\n$out")
      case Right((_, out)) => out should include(message)
      case Left(err)       => fail(err)
    }
  }
}
