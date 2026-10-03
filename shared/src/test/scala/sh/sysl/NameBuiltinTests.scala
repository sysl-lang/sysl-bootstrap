package sh.sysl

import io.github.edadma.cross_platform.*

/** `__NAME__`: the `name` in the `package.hocon` of the package whose file it is written in
 * (`reference/lexical.md § Reserved identifiers`) — or, as a parameter's default, of the package the
 * call filling it is in. It is `__VERSION__`'s twin and travels by the same road, so these cases
 * mirror `VersionBuiltinTests` one for one.
 *
 * The motivating use is a log tag — `logcat(__NAME__)` — so the default cases are the load-bearing
 * ones: a logging library's `tag: string = __NAME__` has to name the application that called it.
 */
class NameBuiltinTests extends PackageCacheSupport {

  /** A project at a fresh root: this manifest, and these files beside it. */
  private def tree(config: Option[String], files: (String, String)*): String = {
    val root = createTempDirectory("sysl-name-")

    config.foreach(c => writeFile(s"$root/${PackageConfig.FileName}", c))

    for (rel, body) <- files do
      Project.parentOf(s"$root/$rel").foreach(createDirectories)
      writeFile(s"$root/$rel", body)

    root
  }

  /** A package called `name` declaring one module, `module`, holding `text`. */
  private def library(name: String, module: String, text: String): String =
    tree(Some(manifest(name, "1.0.0")), s"$module/$module.sysl" -> s"module $module\n\n$text\n")

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

  "__NAME__ is the name the program's own package.hocon states" in {
    val root = tree(Some(manifest("logger-app", "1.2.3")), "main.sysl" -> "print(__NAME__)\n")

    run(root) shouldBe "logger-app\n"
  }

  "a dependency's __NAME__ is its own name, not its consumer's" in {
    val lib  = library("name-lib", "namelib", "own() -> string = __NAME__")
    val root = tree(Some(manifest("app", "0.1.0", s"""n { path = "$lib" }""")),
                    "main.sysl" -> "print(namelib.own())\nprint(__NAME__)\n")

    run(root) shouldBe "name-lib\napp\n"
  }

  // The logcat case: a library's default of `__NAME__` tags the message with the caller's package,
  // as `__FILE__` in the same default names the caller's file.
  "a library's default of __NAME__ is the caller's name, as __FILE__ there is the caller's file" in {
    val lib  = library("name-lib", "namelib",
                       "tag(t: string = __NAME__, f: string = __FILE__) -> string = t + \" \" + f")
    val root = tree(Some(manifest("app", "0.1.0", s"""n { path = "$lib" }""")),
                    "main.sysl" -> "print(namelib.tag())\n")

    val printed = run(root)

    printed should startWith("app ")
    printed should include("main.sysl")
  }

  "a default called from inside its own package answers that package's name" in {
    val lib  = library("name-lib", "namelib",
                       "tag(t: string = __NAME__) -> string = t\n\nown() -> string = tag()")
    val root = tree(Some(manifest("app", "0.1.0", s"""n { path = "$lib" }""")),
                    "main.sysl" -> "print(namelib.own())\nprint(namelib.tag())\n")

    run(root) shouldBe "name-lib\napp\n"
  }

  "a default filled at a call in a second dependency answers the second dependency's name" in {
    val lib  = library("name-lib", "namelib", "tag(t: string = __NAME__) -> string = t")
    val mid  = library("name-mid", "midlib", "mid() -> string = namelib.tag()")
    val root = tree(Some(manifest("app", "0.1.0")), "main.sysl" -> "print(midlib.mid())\n")

    run(root, List(lib, mid)) shouldBe "name-mid\n"
  }

  "a --lib source root with a manifest answers with that manifest's name" in {
    val lib  = library("name-lib", "namelib", "own() -> string = __NAME__")
    val root = tree(Some(manifest("app", "0.1.0")), "main.sysl" -> "print(namelib.own())\n")

    run(root, List(lib)) shouldBe "name-lib\n"
  }

  "it folds to a constant, so a module's 'const' and 'val' may be initialized with it" in {
    val lib  = library("name-lib", "namelib", "const NAME: string = __NAME__\nval ALSO: string = __NAME__")
    val root = tree(Some(manifest("app", "0.1.0", s"""n { path = "$lib" }""")),
                    "main.sysl" -> "print(namelib.NAME)\nprint(namelib.ALSO)\n")

    run(root) shouldBe "name-lib\nname-lib\n"
  }

  "a file that is not part of any package is refused at the use" in {
    val root = tree(None, "lone.sysl" -> "print(__NAME__)\n")

    val said = refused(s"$root/lone.sysl")

    said should include(
      "'__NAME__' is the 'name' in package.hocon, and this file is not part of a package — " +
        "no package.hocon stands at the root of its tree")
    said should include(s"--> $root/lone.sysl:1:7")
  }

  "and a 'const' of it in such a file is refused the same way, not as a non-constant" in {
    val root = tree(None, "lone.sysl" -> "const N: string = __NAME__\nprint(N)\n")

    refused(s"$root/lone.sysl") should include(
      "'__NAME__' is the 'name' in package.hocon, and this file is not part of a package")
  }

  "a manifest that states no name is refused, naming the manifest" in {
    val root = tree(Some("package { version = \"1.0.0\" }\n"), "main.sysl" -> "print(__NAME__)\n")

    refused(root) should include(
      s"'__NAME__' is the 'name' in package.hocon, and ${Project.absolute(root)}/package.hocon declares none")
  }

  // The two read different fields and neither depends on the other being there.
  "a manifest with a name and no version answers __NAME__ and still refuses __VERSION__" in {
    val named = tree(Some("package { name = \"app\" }\n"), "main.sysl" -> "print(__NAME__)\n")
    val both  = tree(Some("package { name = \"app\" }\n"), "main.sysl" -> "print(__NAME__)\nprint(__VERSION__)\n")

    run(named) shouldBe "app\n"
    refused(both) should include("'__VERSION__' is the 'version' in package.hocon")
  }

  "a declaration may not take the name, since it is a built-in of the reserved shape" in {
    val root = tree(Some(manifest("app", "0.1.0")), "main.sysl" -> "val __NAME__ = 1\nprint(__NAME__)\n")

    refused(root) should include("'__NAME__' begins and ends with '__'")
  }
}
