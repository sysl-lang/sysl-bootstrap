package sh.sysl

import io.github.edadma.cross_platform.*

/** `__VERSION__`: the `version` in the `package.hocon` of the package whose file it is written in
 * (`reference/lexical.md § Reserved identifiers`).
 *
 * The load-bearing case is the dependency's. `__FILE__` is per file and needs no manifest at all, so
 * nothing else in the tree says which package a file belongs to — a version taken from the
 * compilation's root would pass the happy path here and hand every library its consumer's version.
 */
class VersionBuiltinTests extends PackageCacheSupport {

  /** A project at a fresh root: this manifest, and these files beside it. */
  private def tree(config: Option[String], files: (String, String)*): String = {
    val root = createTempDirectory("sysl-version-")

    config.foreach(c => writeFile(s"$root/${PackageConfig.FileName}", c))

    for (rel, body) <- files do
      Project.parentOf(s"$root/$rel").foreach(createDirectories)
      writeFile(s"$root/$rel", body)

    root
  }

  /** A package at version `version` declaring one module, `module`, holding `text`. */
  private def library(name: String, version: String, module: String, text: String): String =
    tree(Some(manifest(name, version)), s"$module/$module.sysl" -> s"module $module\n\n$text\n")

  private def drive(file: String, libs: List[String]): (Int, String, String) = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = file, libs = libs))))

    (status, out.toString, notes.toString)
  }

  private def run(file: String, libs: List[String] = Nil): String = {
    val (status, out, notes) = drive(file, libs)

    if status != 0 then fail(s"the driver exited with $status:\n$out$notes")

    out
  }

  private def refused(file: String): String = {
    val (status, _, notes) = drive(file, Nil)

    if status == 0 then fail("expected a refusal, got a build")

    notes
  }

  "__VERSION__ is the version the program's own package.hocon states" in {
    val root = tree(Some(manifest("app", "1.2.3")), "main.sysl" -> "print(__VERSION__)\n")

    run(root) shouldBe "1.2.3\n"
  }

  "a dependency's __VERSION__ is its own version, not its consumer's" in {
    val lib  = library("ver-lib", "4.5.6", "verlib", "version() -> string = __VERSION__")
    val root = tree(Some(manifest("app", "0.1.0", s"""v { path = "$lib" }""")),
                    "main.sysl" -> "print(verlib.version())\nprint(__VERSION__)\n")

    run(root) shouldBe "4.5.6\n0.1.0\n"
  }

  // `__FILE__` and `__LINE__` in a default report the caller, because a default stands where the
  // argument would have been written. A version is a property of the package holding the text, so
  // this one does not follow them across: the library's default answers the library's version.
  "and a library's default of __VERSION__ is still the library's, though __FILE__ there is the caller's" in {
    val lib  = library("ver-lib", "4.5.6", "verlib",
                       "stamp(v: string = __VERSION__, f: string = __FILE__) -> string = v + \" \" + f")
    val root = tree(Some(manifest("app", "0.1.0", s"""v { path = "$lib" }""")),
                    "main.sysl" -> "print(verlib.stamp())\n")

    val printed = run(root)

    printed should startWith("4.5.6 ")
    printed should include("main.sysl")
  }

  "a --lib source root with a manifest answers with that manifest's version" in {
    val lib  = library("ver-lib", "7.8.9", "verlib", "version() -> string = __VERSION__")
    val root = tree(Some(manifest("app", "0.1.0")), "main.sysl" -> "print(verlib.version())\n")

    run(root, List(lib)) shouldBe "7.8.9\n"
  }

  "it folds to a constant, so a module's 'const' and 'val' may be initialized with it" in {
    val lib  = library("ver-lib", "2.0.1", "verlib",
                       "const VERSION: string = __VERSION__\nval ALSO: string = __VERSION__")
    val root = tree(Some(manifest("app", "0.1.0", s"""v { path = "$lib" }""")),
                    "main.sysl" -> "print(verlib.VERSION)\nprint(verlib.ALSO)\n")

    run(root) shouldBe "2.0.1\n2.0.1\n"
  }

  "a file that is not part of any package is refused at the use" in {
    val root = tree(None, "lone.sysl" -> "print(__VERSION__)\n")

    val said = refused(s"$root/lone.sysl")

    said should include(
      "'__VERSION__' is the 'version' in package.hocon, and this file is not part of a package — " +
        "no package.hocon stands at the root of its tree")
    said should include(s"--> $root/lone.sysl:1:7")
  }

  // The constant folder reaches `__VERSION__` by a road of its own, so it owes the same refusal
  // rather than a bare "not a constant expression" that hides which thing is missing.
  "and a 'const' of it in such a file is refused the same way, not as a non-constant" in {
    val root = tree(None, "lone.sysl" -> "const V: string = __VERSION__\nprint(V)\n")

    refused(s"$root/lone.sysl") should include(
      "'__VERSION__' is the 'version' in package.hocon, and this file is not part of a package")
  }

  "a manifest that states no version is refused, naming the manifest" in {
    val root = tree(Some("package { name = \"app\" }\n"), "main.sysl" -> "print(__VERSION__)\n")

    refused(root) should include(
      s"'__VERSION__' is the 'version' in package.hocon, and ${Project.absolute(root)}/package.hocon declares none")
  }
}
