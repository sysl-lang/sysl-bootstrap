package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Shared helpers for the Tier-1 codegen suites: assert on the emitted LLVM IR *text* without
 * running it. The IR is just a string, so these are pure and cross-platform.
 */
trait CodegenSupport extends Matchers { this: Assertions =>

  /** How a diagnostic spells a library declaration — the key it resolves to, rendered.
   *
   * A diagnostic names a moved declaration by the path that reaches it, so a bound on the standard module
   * catalog reads `T: sysl.Add`. Reading the spelling off `Library` rather than writing it out is
   * what keeps these assertions honest across a move: the library is being drained into the
   * standard module a surface at a time, and an expectation with the name baked in either fails for
   * a reason that has nothing to do with what it is testing, or — if it is a negative — quietly
   * stops testing anything at all.
   *
   * **This is the rendered form, and an emitted symbol is not.** A key separates with `$` and this
   * shows it with `.`, so an assertion about IR wants `Library.key` directly — `@sysl$args_of`, not
   * `@sysl.args_of`. Reaching for this one there fails in a way that looks like the move went wrong.
   */
  protected def lib(name: String): String = Modules.show(Library.key(name))

  /** A library key for use inside `include regex`, with the module separator escaped.
   *
   * `Modules.sep` is `$`, which a regex reads as an end-of-input anchor rather than as a character —
   * so `sysl$printi` unescaped matches nothing at all, and the assertion fails as though the symbol
   * were missing. Every `include regex` naming a library symbol goes through this.
   */
  protected def keyRe(name: String): String = Library.key(name).replace("$", "\\$")

  /** The IR for a program that must compile. */
  protected def ir(src: String): String =
    Compiler.compileToLlvm(src) match {
      case Right(out) => out
      case Left(e)    => fail(e)
    }

  /** The environment structs of the closures **this program** declared, in index order, with the
   * library's own filtered out.
   *
   * **Never assert `%struct.$closure0`.** The counter runs over the whole compilation and `library/` is
   * lowered first, so a program's own closures begin at whatever index the library leaves free. That
   * index moved by four the day `sysl.slices` arrived, four of its functions passing
   * `(a, b) -> a < b` to a bare-arrow parameter — and five assertions broke for a reason that had
   * nothing to do with what they were testing.
   *
   * **The silent direction is the worse one, and it happened at the same time.** An assertion that
   * `%struct.$closure0` merely *exists* went on passing while testing nothing at all, because the
   * library now supplies one. That is the failure this file's own opening docstring warns about, in
   * its exact words: an expectation with the name baked in either fails for an unrelated reason, or
   * "quietly stops testing anything at all".
   *
   * Asking which environments are *this program's* is what all of these tests always meant, and it
   * cannot drift again: a closure added to `library/` changes the baseline and the difference, equally.
   */
  protected def envs(out: String): List[String] = {
    val defs = envDefs(out)

    (defs.keySet -- libraryEnvs).toList.sortBy(_.drop("$closure".length).toInt).map(defs)
  }

  /** Every closure environment in some IR, as name to field list. `type {  }` yields `""`. */
  private def envDefs(out: String): Map[String, String] =
    """%struct\.(\$closure\d+) = type \{ ?(.*?) ?\}""".r
      .findAllMatchIn(out)
      .map(m => m.group(1) -> m.group(2))
      .toMap

  /** The closures the library lowers before a program's own, measured from a program that declares
   * none. Compiled once — it is the slowest thing here, and every call wants the same answer.
   */
  private lazy val libraryEnvs: Set[String] = envDefs(ir("print(1)\n")).keySet

  /** The IR a real **optimizer** leaves of a program, rather than the IR the compiler emitted.
   *
   * Some claims are only about what survives `-O2`: that a definition is still a definition, that a
   * call is still a call. Nothing in the unoptimized text can answer one of those — every function
   * is there and every call is made — so the question has to be put to clang, exactly as an ABI
   * question is put to it rather than remembered.
   *
   * The IR goes through a `.ll` file so clang reads it as IR rather than as C, and the result comes
   * back on stdout because there is nothing here that wants a file. A machine with no clang
   * **cancels** the case by name rather than passing it.
   */
  protected def optimizedIr(src: String, level: String = "2"): String = {
    if !Toolchain.clangAvailable then cancel("clang is not installed, so there is no optimizer to ask")

    val path = createTempFile("sysl-optimized-", ".ll")

    try {
      writeFile(path, ir(src))

      val r = exec(Seq("clang", s"-O$level", "-S", "-emit-llvm", "-o", "-", path))

      withClue(s"clang refused the emitted IR:\n${r.stderr}")(r.exitCode shouldBe 0)
      r.stdout
    } finally try deleteFile(path) catch case _: Exception => ()
  }

  /** Whether some IR defines a function whose symbol holds `name` — the question "did this survive
   * the optimizer", asked without spelling a mangled symbol out.
   */
  protected def defines(out: String, name: String): Boolean =
    out.linesIterator.exists(l => l.startsWith("define") && l.contains(name))

  /** The `define` line of the function whose symbol holds `name`, for an assertion about what the
   * line says rather than about whether it is there.
   */
  protected def defineLine(out: String, name: String): String =
    out.linesIterator.find(l => l.startsWith("define") && l.contains(name))
      .getOrElse(fail(s"nothing defines a function named '$name':\n$out"))

  /** The IR for a program built for a machine other than this one (`getting-started/cli.md §
   * targets`). Reading the text is the whole of what a cross-target test can do — there is nothing
   * here to run the result on — and it is enough, because what a target decides is what
   * instructions come out.
   */
  protected def irFor(target: Target, src: String): String =
    Compiler.compileToLlvm(src, "<input>", target) match {
      case Right(out) => out
      case Left(e)    => fail(e)
    }

  /** The error message for a program that must be rejected when built for another machine. A
   * program can be wrong for one target and right for another (`Conditional`), so the rejection is
   * as much a per-target answer as the IR is.
   */
  protected def errFor(target: Target, src: String): String =
    Compiler.compileToLlvm(src, "<input>", target) match {
      case Right(out) => fail(s"expected an error, got:\n$out")
      case Left(e)    => e
    }

  /** The error message for a program that must be rejected. */
  protected def err(src: String): String =
    Compiler.compileToLlvm(src) match {
      case Right(out) => fail(s"expected an error, got:\n$out")
      case Left(e)    => e
    }

  /** Several files, named so a diagnostic about one can be told from a diagnostic about another.
   * `"a.sysl" -> "…"` reads at a call site the way a small directory looks.
   *
   * They carry no location, which is what a file handed to the compiler with no project around it
   * has: the module each contributes to is whatever its header says, and there is nothing for that
   * header to be held against.
   */
  protected def files(fs: (String, String)*): List[Source] =
    fs.toList.map { case (name, text) => Source(name, text) }

  /** The same files as a **project**: each one paired with the directory it sits in, written as the
   * dotted path a driver walking the tree would have derived. `""` is the project root.
   */
  protected def project(fs: (String, String, String)*): List[Source] =
    fs.toList.map { case (dir, name, text) =>
      Source(name, text, if dir.isEmpty then Nil else dir.split('.').toList)
    }

  /** The IR for a program of several files, all of which must compile. */
  protected def irOf(fs: (String, String)*): String = compiled(files(fs*))

  /** The error message for a program of several files that must be rejected. */
  protected def errOf(fs: (String, String)*): String = rejected(files(fs*))

  /** The IR for a project laid out across directories. */
  protected def irIn(fs: (String, String, String)*): String = compiled(project(fs*))

  /** The error message for a project laid out across directories that must be rejected. */
  protected def errIn(fs: (String, String, String)*): String = rejected(project(fs*))

  /** A **stand-in standard module**, written by the test rather than read from `library/sysl`.
   *
   * Some questions are about the library's *position* rather than about what it happens to declare —
   * that a program reaches its members without importing them, that it is searched after a file's
   * imports, that what it keeps to itself is out of reach. Asking those of the real library means
   * the answer changes whenever the library does, and a question it has no case for cannot be asked
   * at all: nothing in `library/sysl` is private, so nothing there can show what a private member of the
   * library does.
   *
   * The trees go in as the `Stdlib` and **not** as units of the compilation, which is the same way the
   * embedded library arrives: a `Stdlib` contributes its own declarations to the walk, and a file that
   * were also a unit would be a program declaring `module sysl` — which is refused, and rightly.
   *
   * A program compiled this way has **only** what the stand-in declares, `print` included, so these
   * fixtures state everything they use.
   */
  protected def standIn(fs: (String, String)*): (List[Program], Stdlib) =
    standInTree(fs.map { case (name, text) => (Std.module, name, text) }*)

  /** The same, for a library that is a **tree**: each file paired with the module it sits in,
   * written as the dotted path a driver walking `lib` would have derived (`Project.walk`).
   *
   * A library with more than one module is not something the real one can stand for while it has
   * only the standard module, and it is the case every question about a submodule is about — that
   * its names do not arrive unasked-for, that a program may reach it by naming it, that what it
   * keeps to itself it keeps from the rest of the library too.
   */
  protected def standInTree(fs: (String, String, String)*): (List[Program], Stdlib) = {
    val units = fs.toList.map { case (module, name, text) =>
      SyslParser.parse(Source(name, text, module.split('.').toList)) match {
        case Right(p) => p
        case Left(e)  => fail(s"the stand-in standard module does not parse: $e")
      }
    }

    (units, new Stdlib(units))
  }

  /** The IR for a program compiled against a stand-in standard module, which must compile. */
  protected def irAgainst(lib: (String, String)*)(fs: (String, String)*): String =
    resultOf(standIn(lib*)._2, fs) match {
      case Right(out) => out
      case Left(e)    => fail(e)
    }

  /** The error for a program compiled against a stand-in standard module, which must be rejected. */
  protected def errAgainst(lib: (String, String)*)(fs: (String, String)*): String =
    resultOf(standIn(lib*)._2, fs) match {
      case Right(out) => fail(s"expected an error, got:\n$out")
      case Left(e)    => e
    }

  /** The IR for a program compiled against a stand-in library of several modules. */
  protected def irAgainstTree(lib: (String, String, String)*)(fs: (String, String)*): String =
    resultOf(standInTree(lib*)._2, fs) match {
      case Right(out) => out
      case Left(e)    => fail(e)
    }

  /** The error for a program compiled against a stand-in library of several modules. */
  protected def errAgainstTree(lib: (String, String, String)*)(fs: (String, String)*): String =
    resultOf(standInTree(lib*)._2, fs) match {
      case Right(out) => fail(s"expected an error, got:\n$out")
      case Left(e)    => e
    }

  private def resultOf(std: Stdlib, fs: Seq[(String, String)]): Either[String, String] =
    Compiler.compiledWith(files(fs*), Nil, Target.default, Set.empty, Some(std)).map(_._1)

  private def compiled(sources: List[Source]): String =
    Compiler.compile(sources) match {
      case Right(out) => out
      case Left(e)    => fail(e)
    }

  private def rejected(sources: List[Source]): String =
    Compiler.compile(sources) match {
      case Right(out) => fail(s"expected an error, got:\n$out")
      case Left(e)    => e
    }

  /** One emitted function, for a test that counts instructions rather than looking for one.
   *
   * A whole-module count is not what such a test means: the module also holds the ARC runtime and
   * whatever library functions the program reached, and either can grow without the lowering under
   * test having changed.
   */
  protected def defineOf(out: String, name: String): String =
    val header = (l: String) => l.startsWith("define") && l.contains(s"@$name(")
    val body   = out.linesIterator.dropWhile(!header(_)).takeWhile(_ != "}")

    body.mkString("\n")

  /** Just `main`, which is where a top-level statement lands. */
  protected def mainOf(out: String): String = defineOf(out, "main")

  /** The IR of `main` alone. */
  protected def irMain(src: String): String = mainOf(ir(src))

  /** Every temporary a function reads without ever writing — the one thing an emitter can produce
   * that is not merely wrong but *unassembleable*, and which nothing here would otherwise see.
   *
   * `freshReg` names temporaries `%tN` and nothing else does, so a `%tN` with no `%tN =` in the
   * same function is a use of a value the function never defined; clang answers it with `use of
   * undefined value` and no test that only reads the text would notice.
   *
   * **Per function rather than per module, which is the whole difficulty.** The counter restarts at
   * each function, so `%t17` exists in most of them — a module-wide census finds a definition for
   * every use and reports nothing, however broken the block it was read in. That is exactly how the
   * defect behind card 0165 reached a release.
   */
  protected def undefinedRegisters(out: String): List[String] = {
    val reg  = """%(t\d+)\b""".r
    val defn = """^\s*%(t\d+) = """.r

    functions(out).flatMap { body =>
      val defined = body.iterator.flatMap(l => defn.findPrefixMatchOf(l).map(_.group(1))).toSet

      body.iterator.flatMap(reg.findAllMatchIn(_).map(_.group(1))).filterNot(defined).toList
    }
  }

  /** Asserts that a program's IR reads no temporary it does not define. */
  protected def definesEveryRegisterItReads(src: String): Unit =
    undefinedRegisters(ir(src)) shouldBe empty

  /** The body of each function in a module, as lines — `define` down to the closing brace. */
  private def functions(out: String): List[List[String]] = {
    val lines = out.linesIterator.toList

    lines.zipWithIndex.collect { case (l, i) if l.startsWith("define") => i }
      .map(i => lines.drop(i).takeWhile(_ != "}"))
  }
}
