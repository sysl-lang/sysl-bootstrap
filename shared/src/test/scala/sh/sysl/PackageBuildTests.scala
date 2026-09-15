package sh.sysl

import io.github.edadma.cross_platform.*

/** A program built against what its `package.hocon` depends on — the whole of the package system
 * from the outside (`reference/packages.md`).
 *
 * This is the seam that matters: the suites either side of it check the resolver's answers and the
 * hash's definition, and none of them says that a `sysl run` over a project with a `dependencies`
 * block compiles the fetched code, links it, and prints the right number. Every case here does that
 * or refuses to.
 *
 * The dependencies are `path` ones, so nothing reaches the network — and the code path from
 * resolution onwards is the same one a git dependency takes, since `§ 3` puts a fetched package in
 * a directory and everything after that is a directory.
 */
class PackageBuildTests extends PackageCacheSupport {

  /** A package on disk: a manifest, one module in a directory of its own, and whatever else it
   * carries — which for the suite below is C, written at a path relative to the package root.
   *
   * `module` may be a path, which is how a package namespaced by reverse DNS is laid out —
   * `sh/sysl/table` declares the single module `sh.sysl.table` and leaves `sh` and `sh/sysl` holding
   * no source, and therefore declaring nothing.
   */
  private def packageOf(name: String, module: String, text: String, deps: String = "",
                        files: (String, String)*): String =
    packageSaying("", name, module, text, deps, files*)

  /** The same, plus whatever else the manifest says — a `defines` block, for the suite below. */
  private def packageSaying(extra: String, name: String, module: String, text: String,
                            deps: String, files: (String, String)*): String = {
    val root = createTempDirectory("sysl-pkg-")
    val leaf = Project.basename(module)

    writeFile(s"$root/${PackageConfig.FileName}", manifest(name, "1.0.0", deps) + extra)
    createDirectories(s"$root/$module")
    writeFile(s"$root/$module/$leaf.sysl", s"module ${module.replace('/', '.')}\n\n$text\n")

    for (path, body) <- files do
      Project.parentOf(s"$root/$path").foreach(createDirectories)
      writeFile(s"$root/$path", body)

    root
  }

  /** A project with a program in it and the dependencies its manifest names. */
  private def app(program: String, deps: String): String = {
    val root = createTempDirectory("sysl-app-")

    writeFile(s"$root/${PackageConfig.FileName}",
      s"""package { name = "app", version = "0.1.0" }
         |dependencies { $deps }
         |""".stripMargin)
    writeFile(s"$root/main.sysl", program)
    root
  }

  /** The driver run against a cache of this test's own, so that what it fetches from and what the
   * machine has built before are two different things.
   */
  private def withCache[T](cache: String)(body: => T): T = Fetch.usingCache(cache)(body)

  /** A plain source root of the kind `--lib` names: modules and no manifest, so its files join the
   * compilation under the names they wrote rather than under a canonical prefix.
   */
  private def libRoot(modules: String*): String = {
    val root = createTempDirectory("sysl-lib-")

    for m <- modules do
      val dir = m.replace('.', '/')

      createDirectories(s"$root/$dir")
      writeFile(s"$root/$dir/${Project.basename(dir)}.sysl", s"module $m\n\nunused() -> int = 0\n")

    root
  }

  private def run(root: String, libs: List[String] = Nil): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root, libs = libs))))

    if status != 0 then fail(s"the driver exited with $status:\n${out.toString}${notes.toString}")

    out.toString
  }

  private def refused(root: String, libs: List[String] = Nil): String = {
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(Discarded)(
      Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root, libs = libs))))

    if status == 0 then fail(s"expected a refusal, got a build")

    notes.toString
  }

  "a program calls a package's function" in {
    val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")

    run(app("""print(geom.double(21))""", s"""g { path = "$geom" }""")) shouldBe "42\n"
  }

  // The import line is the one the library's own documentation would show, which is `§ 9`'s whole
  // argument for making the mount optional.
  "and may import from it rather than qualifying every use" in {
    val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")

    run(app(
      """import geom.double
        |
        |print(double(21))""".stripMargin, s"""g { path = "$geom" }""")) shouldBe "42\n"
  }

  "a mount puts the package under a name of the consumer's choosing" in {
    val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")

    run(app("""print(shapes.geom.double(21))""",
      s"""g { path = "$geom", mount = "shapes" }""")) shouldBe "42\n"
  }

  // The point of the canonical prefix: the project's own `geom` and the dependency's `geom` are two
  // modules, and each program means the one it wrote.
  "a project's own module may share a name with a mounted package's" in {
    val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")
    val root = app("""print(geom.triple(14) + shapes.geom.double(0))""",
      s"""g { path = "$geom", mount = "shapes" }""")

    createDirectories(s"$root/geom")
    writeFile(s"$root/geom/geom.sysl", "module geom\n\ntriple(n: int) -> int = n * 3\n")

    run(root) shouldBe "42\n"
  }

  "a package whose own modules call each other" in {
    val root = createTempDirectory("sysl-pkg-two-")

    writeFile(s"$root/${PackageConfig.FileName}", manifest("two", "1.0.0"))
    createDirectories(s"$root/inner")
    createDirectories(s"$root/outer")
    writeFile(s"$root/inner/inner.sysl", "module inner\n\ndouble(n: int) -> int = n * 2\n")
    // Written with the package's own short name, which is what makes a package's source read the
    // same whoever depends on it.
    writeFile(s"$root/outer/outer.sysl",
      "module outer\n\nimport inner.double\n\nquadruple(n: int) -> int = double(double(n))\n")

    run(app("""print(outer.quadruple(10))""", s"""t { path = "$root" }""")) shouldBe "40\n"
  }

  // A package's table is its own, so `middle` reaches `bottom` because *its* manifest names it —
  // and would not reach it merely because the project did. That is the two-layer identity from the
  // inside: the consumer here never writes the word `bottom` at all.
  "a package that depends on a package" in {
    val cache = emptyCache()
    val at    = published(cache, "github.com/e/bottom", Version(1, 0, 0), manifest("bottom", "1.0.0"))

    createDirectories(s"$at/bottom")
    writeFile(s"$at/bottom/bottom.sysl", "module bottom\n\ndouble(n: int) -> int = n * 2\n")

    val middle = createTempDirectory("sysl-pkg-mid-")

    writeFile(s"$middle/${PackageConfig.FileName}",
      manifest("middle", "1.0.0", """b { git = "github.com/e/bottom", version = "1.0.0" }"""))
    createDirectories(s"$middle/middle")
    writeFile(s"$middle/middle/middle.sysl",
      "module middle\n\nquadruple(n: int) -> int = bottom.double(bottom.double(n))\n")

    withCache(cache)(run(app("""print(middle.quadruple(10))""", s"""m { path = "$middle" }"""))) shouldBe
      "40\n"
  }

  // **And it reaches what its dependency reaches**, which is `§ 9`'s transitivity end to end: the
  // consumer names `middle` and writes `bottom.double` without ever naming `bottom`. This test
  // asserted the refusal until 0.0.73, and the sentence it asserted is the one the section retired.
  "and the consumer reaches what its dependency reaches" in {
    val cache = emptyCache()
    val at    = published(cache, "github.com/e/bottom", Version(1, 0, 0), manifest("bottom", "1.0.0"))

    createDirectories(s"$at/bottom")
    writeFile(s"$at/bottom/bottom.sysl", "module bottom\n\ndouble(n: int) -> int = n * 2\n")

    val middle = createTempDirectory("sysl-pkg-mid2-")

    writeFile(s"$middle/${PackageConfig.FileName}",
      manifest("middle", "1.0.0", """b { git = "github.com/e/bottom", version = "1.0.0" }"""))
    createDirectories(s"$middle/middle")
    writeFile(s"$middle/middle/middle.sysl",
      "module middle\n\nquadruple(n: int) -> int = bottom.double(bottom.double(n))\n")

    withCache(cache)(run(app("""print(bottom.double(21))""", s"""m { path = "$middle" }"""))) shouldBe
      "42\n"
  }

  "two packages wanting one name" - {

    "is refused rather than resolved by whichever was read first" in {
      val one = packageOf("one", "json", "tag() -> int = 1")
      val two = packageOf("two", "json", "tag() -> int = 2")

      refused(app("""print(json.tag())""",
        s"""a { path = "$one" }, b { path = "$two" }""")) should include("claim the same module")
    }

    "and a mount settles it" in {
      val one = packageOf("one", "json", "tag() -> int = 40")
      val two = packageOf("two", "json", "tag() -> int = 2")

      run(app("""print(json.tag() + other.json.tag())""",
        s"""a { path = "$one" }, b { path = "$two", mount = "other" }""")) shouldBe "42\n"
    }

    // The common case rather than an exotic one, and the reason the check cannot be only about
    // dependencies disagreeing with each other.
    "including one the project already uses for a module of its own" in {
      val one  = packageOf("one", "json", "tag() -> int = 1")
      val root = app("""print(json.tag())""", s"""a { path = "$one" }""")

      createDirectories(s"$root/json")
      writeFile(s"$root/json/json.sysl", "module json\n\nmine() -> int = 2\n")

      refused(root) should include("is both a module of this project")
    }
  }

  /** The convention `reference/packages.md § What a dependency's modules are called` recommends,
   * from the consuming side — and the case it was recommended *for*.
   *
   * A package that namespaces itself by reverse DNS puts its source at `sh/sysl/<name>/`, so `sh`
   * and `sh/sysl` hold none and neither is a module it declares. Binding the top-level **directory**
   * made every such package claim the one name `sh`, so any two of them refused to resolve together
   * and a project could depend on at most one — with the convention that was supposed to make
   * collisions "close to never" being exactly what guaranteed one.
   *
   * Nothing in the suite could have caught it: every other package here declares a bare module at
   * its root, which is the one shape the old rule got right.
   */
  "packages namespaced under a shared prefix" - {

    "are all reachable at once, under the names their own documentation shows" in {
      val sqlite = packageOf("sqlite3", "sh/sysl/sqlite", "answer() -> int = 40")
      val table  = packageOf("table", "sh/sysl/table", "answer() -> int = 2")

      run(app("""print(sh.sysl.sqlite.answer() + sh.sysl.table.answer())""",
        s"""s { path = "$sqlite" }, t { path = "$table" }""")) shouldBe "42\n"
    }

    "and are imported without a mount, which is the whole point of the convention" in {
      val sqlite = packageOf("sqlite3", "sh/sysl/sqlite", "double(n: int) -> int = n * 2")
      val table  = packageOf("table", "sh/sysl/table", "half(n: int) -> int = n / 2")

      run(app(
        """import sh.sysl.sqlite.double
          |import sh.sysl.table.half
          |
          |print(double(half(42)))""".stripMargin,
        s"""s { path = "$sqlite" }, t { path = "$table" }""")) shouldBe "42\n"
    }

    // A binding covers the module it names and everything under it, so a package needs one entry
    // however deep its tree goes.
    "a module below a bound one comes with it" in {
      val deep = packageOf("deep", "sh/sysl/outer", "double(n: int) -> int = n * 2")

      createDirectories(s"$deep/sh/sysl/outer/inner")
      writeFile(s"$deep/sh/sysl/outer/inner/inner.sysl",
        "module sh.sysl.outer.inner\n\ntriple(n: int) -> int = n * 3\n")

      run(app("""print(sh.sysl.outer.double(15) + sh.sysl.outer.inner.triple(4))""",
        s"""d { path = "$deep" }""")) shouldBe "42\n"
    }

    /** A namespaced package's own modules reaching each other — the *other* half of `packagePath`,
     * where the path is the package's own tree rather than a dependency's binding.
     *
     * "a package whose own modules call each other" above covers the same branch with **bare** names,
     * where the head and the whole name are one thing — so the segment that travels past the head is
     * invisible there, and a package with two namespaced modules is the shape that would have caught
     * it. No package in the org has two yet, which is exactly why it is written down here.
     */
    "a namespaced package's own modules reach each other by import" in {
      val root = createTempDirectory("sysl-pkg-ns-")

      writeFile(s"$root/${PackageConfig.FileName}", manifest("ns", "1.0.0"))
      createDirectories(s"$root/sh/sysl/inner")
      createDirectories(s"$root/sh/sysl/outer")
      writeFile(s"$root/sh/sysl/inner/inner.sysl",
        "module sh.sysl.inner\n\ndouble(n: int) -> int = n * 2\n")
      writeFile(s"$root/sh/sysl/outer/outer.sysl",
        "module sh.sysl.outer\n\nimport sh.sysl.inner.double\n\n" +
          "quadruple(n: int) -> int = double(double(n))\n")

      run(app("""print(sh.sysl.outer.quadruple(10))""", s"""n { path = "$root" }""")) shouldBe "40\n"
    }

    // And by qualified reference, which takes a different route: an import goes through `inPackage`
    // and a reference through `throughModule`, and it was `throughModule` that asked with the head
    // alone.
    "and by qualified reference" in {
      val root = createTempDirectory("sysl-pkg-ns2-")

      writeFile(s"$root/${PackageConfig.FileName}", manifest("ns", "1.0.0"))
      createDirectories(s"$root/sh/sysl/inner")
      createDirectories(s"$root/sh/sysl/outer")
      writeFile(s"$root/sh/sysl/inner/inner.sysl",
        "module sh.sysl.inner\n\ndouble(n: int) -> int = n * 2\n")
      writeFile(s"$root/sh/sysl/outer/outer.sysl",
        "module sh.sysl.outer\n\ntriple(n: int) -> int = sh.sysl.inner.double(n) + n\n")

      run(app("""print(sh.sysl.outer.triple(14))""", s"""n { path = "$root" }""")) shouldBe "42\n"
    }

    /** Both of those again, with an unrelated source root in the compilation that begins with the
     * same segment — which is what broke them.
     *
     * Whether a segment begins a module was asked of **every** module being compiled, so a `--lib`
     * root supplying `sh.sysl.other` made `sh` name one; the package's own path was then left
     * unqualified and its sibling module was unreachable from its own source. The package was
     * right and had not changed. What answered was a tree it has never heard of.
     *
     * It is the ordinary workflow rather than a corner. Developing an untagged package beside a
     * project that depends on a released one is what `--lib` is for, and `§ 9`'s convention makes
     * every package in an org share the segment — so this is guaranteed rather than unlucky, in
     * the same way the top-level-directory binding above was.
     */
    "and neither is taken over by an unrelated source root sharing the segment" - {
      def namespaced(outer: String): String = {
        val root = createTempDirectory("sysl-pkg-ns3-")

        writeFile(s"$root/${PackageConfig.FileName}", manifest("ns", "1.0.0"))
        createDirectories(s"$root/sh/sysl/inner")
        createDirectories(s"$root/sh/sysl/outer")
        writeFile(s"$root/sh/sysl/inner/inner.sysl",
          "module sh.sysl.inner\n\ndouble(n: int) -> int = n * 2\n")
        writeFile(s"$root/sh/sysl/outer/outer.sysl", s"module sh.sysl.outer\n\n$outer\n")

        root
      }

      "by import" in {
        val root = namespaced("import sh.sysl.inner.double\n\nquadruple(n: int) -> int = double(double(n))")

        run(app("""print(sh.sysl.outer.quadruple(10))""", s"""n { path = "$root" }"""),
          List(libRoot("sh.sysl.other"))) shouldBe "40\n"
      }

      "by qualified reference" in {
        val root = namespaced("triple(n: int) -> int = sh.sysl.inner.double(n) + n")

        run(app("""print(sh.sysl.outer.triple(14))""", s"""n { path = "$root" }"""),
          List(libRoot("sh.sysl.other"))) shouldBe "42\n"
      }
    }

    // The mount is untouched by any of it: a name the consumer chose has no tree to be read off, so
    // it still hangs the whole package under one segment.
    "and a mount still hangs the whole tree under one segment" in {
      val table = packageOf("table", "sh/sysl/table", "answer() -> int = 42")

      run(app("""print(tb.sh.sysl.table.answer())""",
        s"""t { path = "$table", mount = "tb" }""")) shouldBe "42\n"
    }
  }

  "a name the package does not declare is refused where it is written" in {
    val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")

    refused(app("""print(geom.triple(21))""", s"""g { path = "$geom" }""")) should include("triple")
  }

  /** `reference/ffi.md § A library may carry C` from the consuming side: a package carries C, and a
   * build against it compiles that C and links it.
   *
   * **This is the seam a whole release shipped without.** A package's `.sysl` compiled, its `.c`
   * was dropped with nothing said, and the build ended at the linker naming symbols the package's
   * own C defines — which made every package with a shim unusable as a dependency, and a shim is
   * what a binding to a real library *is*. `reference/packages.md § No build scripts, ever` had
   * already promised the opposite in as many words, refusing build scripts on the grounds that sysl
   * compiles a package's C declaratively.
   *
   * Nothing else in the suite could have caught it: every package above is pure sysl, so the walk
   * that never looked for C was never asked a question it could get wrong.
   */
  "a package's C" - {

    // The discriminating shape: seven is a number only the C knows, and the multiplication is only
    // the sysl's. A build that dropped the C cannot print this, and neither can one that linked the
    // C and lost the sysl.
    val shim = "int geom_seven(void) { return 7; }\n"

    val calling =
      """extern "geom_seven" c_seven() -> int
        |
        |seven_times(n: int) -> int = c_seven() * n""".stripMargin

    "is compiled and linked beside its sysl" in {
      val geom = packageOf("geom-lib", "geom", calling, "", "geom/shim.c" -> shim)

      run(app("""print(geom.seven_times(6))""", s"""g { path = "$geom" }""")) shouldBe "42\n"
    }

    // `reference/ffi.md § A library may carry C` says *anywhere* in the tree, and the package root
    // is the one place that declares no module — so a walk gathering C only where it found sysl
    // would skip it.
    "is found at the package root as well as beside a module" in {
      val geom = packageOf("geom-lib", "geom", calling, "", "shim.c" -> shim)

      run(app("""print(geom.seven_times(6))""", s"""g { path = "$geom" }""")) shouldBe "42\n"
    }

    // The trap the fix had to answer. Each package is staged into a directory of its own, so two
    // files at one relative path stay two objects — where one shared staging area would have the
    // second overwrite the first and the program lose whatever only the first defined.
    "does not collide with another package's at the same relative path" in {
      val one = packageOf("one", "left", """extern "one_util" c() -> int
                                           |
                                           |value() -> int = c()""".stripMargin,
        "", "left/util.c" -> "int one_util(void) { return 40; }\n")

      val two = packageOf("two", "left", """extern "two_util" c() -> int
                                           |
                                           |value() -> int = c()""".stripMargin,
        "", "left/util.c" -> "int two_util(void) { return 2; }\n")

      run(app("""print(left.value() + other.left.value())""",
        s"""a { path = "$one" }, b { path = "$two", mount = "other" }""")) shouldBe "42\n"
    }

    // Every package the resolver selected is a tree, not only the ones the project named — so a
    // binding two levels down is compiled on the same footing as one the manifest mentions. The
    // consumer here never writes the word `bottom`.
    "is compiled for a package the project never named itself" in {
      val cache = emptyCache()
      val at    = published(cache, "github.com/e/bottom", Version(1, 0, 0), manifest("bottom", "1.0.0"))

      createDirectories(s"$at/bottom")
      writeFile(s"$at/bottom/bottom.sysl",
        "module bottom\n\nextern \"bottom_seven\" c() -> int\n\nseven() -> int = c()\n")
      writeFile(s"$at/bottom/shim.c", "int bottom_seven(void) { return 7; }\n")

      val middle = createTempDirectory("sysl-pkg-mid3-")

      writeFile(s"$middle/${PackageConfig.FileName}",
        manifest("middle", "1.0.0", """b { git = "github.com/e/bottom", version = "1.0.0" }"""))
      createDirectories(s"$middle/middle")
      writeFile(s"$middle/middle/middle.sysl",
        "module middle\n\nsix_sevens() -> int = bottom.seven() * 6\n")

      withCache(cache)(run(app("""print(middle.six_sevens())""", s"""m { path = "$middle" }"""))) shouldBe
        "42\n"
    }

    // The other half of a binding: the C reaches its own library through a `@link` directive, and
    // the directive is written in the package's header where nothing but the package could know it.
    // Named after a library no machine has, so the observation is the same on every platform — the
    // name reached a command line, which is all `reference/ffi.md § @link` claims.
    "carries its module's link directive to the command line" in {
      val geom = packageOf("geom-lib", "geom",
        "@link(\"sysl-no-such-library\")\n\nvalue() -> int = 42")

      refused(app("""print(geom.value())""", s"""g { path = "$geom" }""")) should
        include("sysl-no-such-library")
    }

    // The error path. A shim that will not compile is the package author's mistake and the consumer
    // is the one who meets it, so the message has to name the file rather than report that a link
    // went wrong somewhere.
    "that will not compile stops the build, naming the file" in {
      val geom = packageOf("geom-lib", "geom", calling, "", "geom/shim.c" -> "int geom_seven(void) { return\n")

      refused(app("""print(geom.seven_times(6))""", s"""g { path = "$geom" }""")) should
        include("shim.c")
    }
  }

  /** **A `.c` the walk goes past, named rather than left silent.**
   *
   * A directory holding C and no sysl is not a module, so its files belong to no compilation. That
   * rule is deliberate — a directory the sysl walk claimed nothing from was never part of the tree,
   * and taking C out of it would compile whatever a `build/` or a vendored library happened to leave
   * there — and it said nothing whatever: no `compile:` line, no note, and then a link error naming
   * a symbol.
   *
   * **Card `0323` was filed against `sysl build` on exactly that reading**, claiming the root
   * module's `c/` was never compiled while a dependency's was. The premise was false: `Main` passes
   * the root first, nothing on the path distinguishes it, and the card's own reduction compiles once
   * the directory is made a module. What was real is that the two cases are indistinguishable from
   * outside, and this is what tells them apart.
   */
  "a directory of C that is not a module" - {

    /** A project whose own tree carries the files named, so the warning is about something its
     * author can change — which is why a dependency's stray `.c` is deliberately not reported.
     */
    def carrying(files: (String, String)*): String = {
      val root = createTempDirectory("sysl-stray-c-")

      writeFile(s"$root/${PackageConfig.FileName}", """package { name = "app", version = "0.1.0" }""")
      writeFile(s"$root/main.sysl", "print(thing.answer())")
      createDirectories(s"$root/thing")
      writeFile(s"$root/thing/thing.sysl", "module thing\n\nanswer() -> int = 42\n")

      for (path, body) <- files do
        Project.parentOf(s"$root/$path").foreach(createDirectories)
        writeFile(s"$root/$path", body)

      root
    }

    /** A run that had to succeed, with what it said on the way. The build working is half the
     * observation here: the skip is not an error and must not become one.
     */
    def noted(root: String): String = {
      val notes  = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(
        Console.withErr(notes)(sh.sysl.execute(Config(command = "run", file = root))))

      withClue(notes.toString)(status shouldBe 0)
      notes.toString
    }

    "is named, with the one-file remedy in the sentence" in {
      val notes = noted(carrying("thing/c/side.c" -> "int side_answer(void) { return 7; }\n"))

      notes should include("holds C and no sysl")
      notes should include("compiled by nothing")
      notes should include("declares those 'extern's")
    }

    // The remedy the warning names, run rather than described: one `.sysl` in the directory makes it
    // a module, and the C is compiled and linked like any other. This is the card's own reduction,
    // repaired.
    "and compiles once that directory holds the sysl declaring its externs" in {
      val root = carrying(
        "thing/c/side.c"   -> "int side_answer(void) { return 7; }\n",
        "thing/c/c.sysl"   -> "module thing.c\n\nextern \"side_answer\" side() -> int\n",
        "thing/thing.sysl" -> "module thing\n\nimport thing.c\n\nanswer() -> int = c.side()\n",
      )

      run(root) shouldBe "7\n"
      noted(root) should not include "holds C and no sysl"
    }

    // Noise is what would make this worth turning off, so the rule is narrow: only a directory
    // actually holding `.c` is reported. Every tree has directories the walk takes nothing from.
    "while a directory holding no C at all is passed over in silence, as everything else is" in {
      noted(carrying("thing/notes/README.md" -> "nothing to compile here\n")) should
        not include "holds C and no sysl"
    }
  }

  "a project with no dependencies block is untouched by any of this" in {
    val root = createTempDirectory("sysl-plain-")

    writeFile(s"$root/${PackageConfig.FileName}", """package { name = "app", version = "0.1.0" }""")
    writeFile(s"$root/main.sysl", "print(42)")

    run(root) shouldBe "42\n"
  }

  /** A **`--lib` source root** that is itself a package, and the `dependencies` block it declares.
   *
   * A package reached this way is the same package it is by coordinate, so what it depends on is a
   * property of *it* rather than of the road it arrived by — the argument `§ 13` makes about the
   * allocator and `§ 8` about the headers its C needs. It was the one road that read neither: the
   * driver took the root's `.sysl` and nothing else, so the block was neither fetched nor mentioned,
   * and what a caller got was the unresolved-name cascade that follows code whose imports point at a
   * package nobody brought — none of it naming the package or the flag that would have answered.
   *
   * It is the loop for developing a package against an unreleased dependency, which is why it cost
   * something rather than merely being inconsistent: the companion had to be tagged before anything
   * could be tried against it.
   */
  "a --lib source root's own dependencies" - {

    /** A source root that is a package: a manifest naming what it depends on, and a module whose code
     * reaches it.
     */
    def declaring(deps: String, body: String, module: String = "mid"): String = {
      val root = createTempDirectory("sysl-lib-deps-")

      writeFile(s"$root/${PackageConfig.FileName}", manifest(module, "1.0.0", deps))
      createDirectories(s"$root/$module")
      writeFile(s"$root/$module/$module.sysl", s"module $module\n\n$body\n")
      root
    }

    /** A program with **no manifest of its own**, so the only `dependencies` block in the build is the
     * root's. That is what makes each case below say something about the root rather than about a
     * project that would have fetched the package anyway.
     */
    def program(text: String): String = {
      val root = createTempDirectory("sysl-lib-deps-app-")

      writeFile(s"$root/main.sysl", text)
      s"$root/main.sysl"
    }

    /** A package published at a version, whose one function answers with a number naming that
     * version — which is how the cases below say *which* copy was selected rather than only that one
     * was.
     */
    def bottom(cache: String, version: Version, stamp: Int): Unit = {
      val at = published(cache, "github.com/e/bottom", version, manifest("bottom", version.toString))

      createDirectories(s"$at/bottom")
      writeFile(s"$at/bottom/bottom.sysl", s"module bottom\n\nstamp() -> int = $stamp\n")
    }

    "are fetched, so its own code reaches what it declared" in {
      val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")
      val root = declaring(s"""g { path = "$geom" }""",
        "quadruple(n: int) -> int = geom.double(geom.double(n))")

      run(program("print(mid.quadruple(10))"), List(root)) shouldBe "40\n"
    }

    // A `--lib` root's manifest is written in that root's own directory, so a relative `path` in it
    // means a directory beside the root -- never beside the project that happens to name the root
    // with `--lib`, and never wherever the process was launched from. `geom-lib` sits beside `mid`
    // here and nowhere near `program`'s own directory, which is what makes this the case the root
    // project's own relative-path resolution could not have gotten right by accident.
    "and a relative one is read against ITS OWN directory, never the project's" in {
      val parent = createTempDirectory("sysl-lib-deps-rel-")
      val geom    = s"$parent/geom-lib"

      createDirectories(geom)
      writeFile(s"$geom/${PackageConfig.FileName}", manifest("geom-lib", "1.0.0"))
      createDirectories(s"$geom/geom")
      writeFile(s"$geom/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")

      val root = s"$parent/mid"

      createDirectories(root)
      writeFile(s"$root/${PackageConfig.FileName}",
        manifest("mid", "1.0.0", """g { path = "../geom-lib" }"""))
      createDirectories(s"$root/mid")
      writeFile(s"$root/mid/mid.sysl",
        "module mid\n\nquadruple(n: int) -> int = geom.double(geom.double(n))\n")

      run(program("print(mid.quadruple(10))"), List(root)) shouldBe "40\n"
    }

    "and a coordinate is fetched, not only a directory already on this machine" in {
      val cache = emptyCache()

      bottom(cache, Version(1, 0, 0), 100)

      val root = declaring("""b { git = "github.com/e/bottom", version = "1.0.0" }""",
        "answer() -> int = bottom.stamp()")

      withCache(cache)(run(program("print(mid.answer())"), List(root))) shouldBe "100\n"
    }

    /** One graph rather than one per road, which is what makes selection mean anything: MVS takes its
     * maximum over every claim at once (`§ 5`), so the project's floor and the root's are read
     * together and one copy is built at the higher of them.
     *
     * Resolved a road at a time, this program would hold two versions of one module — and `§ 4` is
     * about exactly what that costs, since the two emit *the same symbol names* for different code.
     * The sum is what says the floor rose for the project's own use too: 240 rather than 220.
     */
    "and are version-selected together with the project's own claims" in {
      val cache = emptyCache()

      bottom(cache, Version(1, 0, 0), 100)
      bottom(cache, Version(1, 2, 0), 120)

      val root = declaring("""b { git = "github.com/e/bottom", version = "1.2.0" }""",
        "answer() -> int = bottom.stamp()")

      withCache(cache)(run(app("""print(mid.answer() + bottom.stamp())""",
        """b { git = "github.com/e/bottom", version = "1.0.0" }"""), List(root))) shouldBe "240\n"
    }

    "and two roots naming one coordinate get one copy of it, at the higher version" in {
      val cache = emptyCache()

      bottom(cache, Version(1, 0, 0), 100)
      bottom(cache, Version(1, 4, 0), 140)

      val low  = declaring("""b { git = "github.com/e/bottom", version = "1.0.0" }""",
        "value() -> int = bottom.stamp()", module = "low")
      val high = declaring("""b { git = "github.com/e/bottom", version = "1.4.0" }""",
        "value() -> int = bottom.stamp()", module = "high")

      withCache(cache)(run(program("print(low.value() + high.value())"), List(low, high))) shouldBe
        "280\n"
    }

    /** What a package declares is charged to the build that reached it, whichever road reached it — so
     * `§ 8`'s header requirement and `§ 13`'s allocator arrive with a package a *root* named, without
     * either needing an answer of its own for this road. Both are refusals, which is the shape a
     * declaration has that a suite watching a program's output can see.
     */
    "and what one of them declares is charged to the build that reached it" - {

      "its header requirement, named rather than left to clang" in {
        val cache = emptyCache()
        val at    = published(cache, "github.com/e/probing", Version(1, 0, 0),
          """package { name = "probing", version = "1.0.0" }
            |requires { headers { probe = "the probe library's headers" } }
            |""".stripMargin)

        createDirectories(s"$at/probing")
        writeFile(s"$at/probing/probing.sysl", "module probing\n\nvalue() -> int = 42\n")

        val root = declaring("""p { git = "github.com/e/probing", version = "1.0.0" }""",
          "answer() -> int = probing.value()")

        withCache(cache)(refused(program("print(mid.answer())"), List(root))) should
          include("--include-path probe=")
      }

      "and its allocator, which cannot then disagree with one the project names" in {
        val cache = emptyCache()
        val at    = published(cache, "github.com/e/heapy", Version(1, 0, 0),
          """package { name = "heapy", version = "1.0.0" }
            |allocator { alloc = "pvPortMalloc", free = "vPortFree" }
            |""".stripMargin)

        createDirectories(s"$at/heapy")
        writeFile(s"$at/heapy/heapy.sysl", "module heapy\n\nvalue() -> int = 42\n")

        val root     = declaring("""h { git = "github.com/e/heapy", version = "1.0.0" }""",
          "answer() -> int = heapy.value()")
        val consumer = createTempDirectory("sysl-lib-deps-heap-")

        writeFile(s"$consumer/${PackageConfig.FileName}",
          """package { name = "app", version = "0.1.0" }
            |allocator { alloc = "kmalloc", free = "kfree" }
            |""".stripMargin)
        writeFile(s"$consumer/main.sysl", "print(mid.answer())")

        withCache(cache)(refused(consumer, List(root))) should include("pvPortMalloc")
      }
    }

    // A source root need not be a package at all, which is most of what `--lib` is for, and one with
    // no manifest depends on nothing. Nothing may be fetched on its account and nothing may break.
    "while a root with no manifest depends on nothing" in {
      run(program("print(41 + 1)"), List(libRoot("sh.sysl.other"))) shouldBe "42\n"
    }

    /** The collision this made reachable, and the one thing the first draft of the fetch got wrong.
     *
     * A root's modules are filed under the project's prefix, so a dependency of that root offering a
     * name the root itself declares is two modules claiming one name — `§ 9`'s collision, and the
     * refusal was checked against the *project's* directories only. What happened instead was the
     * silent winner that rule exists to refuse: the root's own module answered, the dependency's was
     * unreachable, and the build was green.
     */
    "and a dependency claiming a name the root itself declares is refused" in {
      val cache = emptyCache()
      val at    = published(cache, "github.com/e/mid", Version(1, 0, 0), manifest("mid", "1.0.0"))

      createDirectories(s"$at/mid")
      writeFile(s"$at/mid/mid.sysl", "module mid\n\nstamp() -> int = 7\n")

      val root  = declaring("""m { git = "github.com/e/mid", version = "1.0.0" }""",
        "answer() -> int = 42")
      val notes = withCache(cache)(refused(program("print(mid.answer())"), List(root)))

      notes should include("is both a module of the source root")
      notes should include(root)
      notes should include("'mount'")
    }

    // And a mount settles it, exactly as it settles every other collision — the escape hatch has to
    // be reachable from this road too, or the refusal above is a wall rather than a diagnostic.
    "while a mount settles that, as it does every other collision" in {
      val cache = emptyCache()
      val at    = published(cache, "github.com/e/mid", Version(1, 0, 0), manifest("mid", "1.0.0"))

      createDirectories(s"$at/mid")
      writeFile(s"$at/mid/mid.sysl", "module mid\n\nstamp() -> int = 7\n")

      val root = declaring(
        """m { git = "github.com/e/mid", version = "1.0.0", mount = "theirs" }""",
        "answer() -> int = theirs.mid.stamp() * 6")

      withCache(cache)(run(program("print(mid.answer())"), List(root))) shouldBe "42\n"
    }

    /** The accepted cost, pinned so that it is deliberate rather than discovered. A `--lib` root's
     * files are filed under the **project's own** prefix — that is what `--lib` means — so the import
     * table its manifest binds into is the project's, and a name only the root declared is a name the
     * program can write. It is no more true of a dependency's name than it always was of the root's
     * own modules.
     */
    "and the program may write a name only the root's manifest bound" in {
      val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")
      val root = declaring(s"""g { path = "$geom" }""", "unused() -> int = 0")

      run(program("print(geom.double(21))"), List(root)) shouldBe "42\n"
    }
  }

  "sysl.sum" - {

    // A path dependency is expected to change under you, so nothing records what it hashed to and
    // the file is never written for one.
    "is not written for a project whose dependencies are all paths" in {
      val geom = packageOf("geom-lib", "geom", "double(n: int) -> int = n * 2")
      val root = app("""print(geom.double(21))""", s"""g { path = "$geom" }""")

      run(root)
      isFile(s"$root/${Sums.FileName}") shouldBe false
    }

    // Written on the first build, from a package this project did not itself fetch — which is the
    // case that would otherwise leave a project with no record and so no check.
    "records what a git dependency hashed to, on the first build" in {
      val cache = emptyCache()
      val at    = published(cache, "github.com/e/geom", Version(1, 0, 0), manifest("geom-lib", "1.0.0"))

      createDirectories(s"$at/geom")
      writeFile(s"$at/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")

      val hash = record(cache, "github.com/e/geom", Version(1, 0, 0))
      val root = app("""print(geom.double(21))""",
        """g { git = "github.com/e/geom", version = "1.0.0" }""")

      withCache(cache)(run(root)) shouldBe "42\n"
      readFile(s"$root/${Sums.FileName}") shouldBe s"github.com/e/geom v1.0.0 $hash\n"
    }

    "and a build whose record already agrees leaves the file alone" in {
      val cache = emptyCache()
      val at    = published(cache, "github.com/e/geom", Version(1, 0, 0), manifest("geom-lib", "1.0.0"))

      createDirectories(s"$at/geom")
      writeFile(s"$at/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")

      val hash = record(cache, "github.com/e/geom", Version(1, 0, 0))
      val root = app("""print(geom.double(21))""",
        """g { git = "github.com/e/geom", version = "1.0.0" }""")

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/geom v1.0.0 $hash\n")

      val before = lastModified(s"$root/${Sums.FileName}")

      withCache(cache)(run(root)) shouldBe "42\n"
      lastModified(s"$root/${Sums.FileName}") shouldBe before
    }

    // The refusal the file exists for: what is on disk is not what was promised.
    "refuses a build whose record disagrees with what is there" in {
      val cache = emptyCache()
      val at    = published(cache, "github.com/e/geom", Version(1, 0, 0), manifest("geom-lib", "1.0.0"))

      createDirectories(s"$at/geom")
      writeFile(s"$at/geom/geom.sysl", "module geom\n\ndouble(n: int) -> int = n * 2\n")
      record(cache, "github.com/e/geom", Version(1, 0, 0))

      val root = app("""print(geom.double(21))""",
        """g { git = "github.com/e/geom", version = "1.0.0" }""")

      writeFile(s"$root/${Sums.FileName}", s"github.com/e/geom v1.0.0 ${Hashing.Prefix}${"a" * 64}\n")

      withCache(cache)(refused(root)) should include("does not hash to what sysl.sum records")
    }
  }

  /* The header both translation units read, written the way a vendored C library is: an option
     * with a default, so that a probe measuring it under the defaults gets a *different answer*
     * rather than failing to compile. That is what makes the probe case below a real test.
     */
  private val header =
    """#ifndef SIZED_H
      |#define SIZED_H
      |#ifndef WIDE
      |#define WIDE 0
      |#endif
      |#if WIDE
      |typedef struct { char bytes[64]; } state;
      |#else
      |typedef struct { char bytes[8]; } state;
      |#endif
      |int sized_value(void);
      |#endif
      |""".stripMargin

  private val impl =
    """#include "sized.h"
      |#ifdef DOUBLED
      |int sized_value(void) { return 42; }
      |#else
      |int sized_value(void) { return 1; }
      |#endif
      |""".stripMargin

  /** `reference/packages.md § No build scripts, ever`'s `defines` block: the macros a package's own
   * carried C is compiled with.
   *
   * Every case here is end to end — the package is built and the program is run — because what the
   * feature claims is about a clang command line, and the only thing that can check a clang command
   * line is clang.
   */
  "a package's 'defines'" - {

    "reach the C file the block names" in {
      val pkg = packageSaying(
        """defines { "geom/shim.c" { DOUBLED = true } }""",
        "geom-lib", "geom",
        """extern "sized_value" c() -> int
          |
          |value() -> int = c()""".stripMargin,
        "", "geom/shim.c" -> impl, "geom/sized.h" -> header)

      run(app("""print(geom.value())""", s"""g { path = "$pkg" }""")) shouldBe "42\n"
    }

    "do not reach a C file the block does not name" in {
      val pkg = packageSaying(
        """defines { "other.c" { DOUBLED = true } }""",
        "geom-lib", "geom",
        """extern "sized_value" c() -> int
          |
          |value() -> int = c()""".stripMargin,
        "", "geom/shim.c" -> impl, "geom/sized.h" -> header,
        "other.c" -> "int other_unused(void) { return 0; }\n")

      run(app("""print(geom.value())""", s"""g { path = "$pkg" }""")) shouldBe "1\n"
    }

    "take a value, not only a bare name" in {
      val pkg = packageSaying(
        """defines { "geom/shim.c" { DOUBLED = 1 } }""",
        "geom-lib", "geom",
        """extern "sized_value" c() -> int
          |
          |value() -> int = c()""".stripMargin,
        "", "geom/shim.c" -> impl, "geom/sized.h" -> header)

      run(app("""print(geom.value())""", s"""g { path = "$pkg" }""")) shouldBe "42\n"
    }

    /* **The case the feature would be silently wrong without.** A `c const` block is measured from
     * a translation unit the compiler synthesises, which no `defines` key can name — so it inherits
     * from the C in its own directory. Measured under the header's defaults `state` is 8 bytes;
     * under `WIDE` it is 64, and both compile. A probe left out of the block reports 8 for a package
     * whose object file was built with the 64-byte layout, and nothing anywhere says so.
     */
    "reach the probe a 'c const' block is measured from" in {
      val pkg = packageSaying(
        """defines { "geom/shim.c" { WIDE = 1 } }""",
        "geom-lib", "geom",
        """@include("sized.h")
          |
          |c const
          |    SIZE: usize = "sizeof(state)"
          |
          |size() -> usize = SIZE""".stripMargin,
        "", "geom/shim.c" -> impl, "geom/sized.h" -> header)

      run(app("""print(geom.size())""", s"""g { path = "$pkg" }""")) shouldBe "64\n"
    }

    /* And the other half of that rule: inheritance is from the probe's **own** directory, so a
     * package configuring C somewhere else in its tree does not silently reconfigure this one.
     */
    "do not reach a probe in a different directory" in {
      val pkg = packageSaying(
        """defines { "other.c" { WIDE = 1 } }""",
        "geom-lib", "geom",
        """@include("sized.h")
          |
          |c const
          |    SIZE: usize = "sizeof(state)"
          |
          |size() -> usize = SIZE""".stripMargin,
        "", "geom/sized.h" -> header,
        "other.c" -> "int other_unused(void) { return 0; }\n")

      run(app("""print(geom.size())""", s"""g { path = "$pkg" }""")) shouldBe "8\n"
    }

    /* The one mistake a `defines` block can make that reading the manifest cannot catch. Everything
     * else is refused at parse time; a path that is merely wrong would otherwise leave the macros
     * reaching nothing, the C compiling under its defaults, and the build looking fine.
     *
     * It also catches naming C the walk does not collect at all -- a directory holding no sysl is
     * not a module, so its C is not compiled and a block configuring it is configuring nothing.
     */
    "naming a file the package does not carry stops the build" in {
      val pkg = packageSaying(
        """defines { "geom/typo.c" { DOUBLED = true } }""",
        "geom-lib", "geom",
        """extern "sized_value" c() -> int
          |
          |value() -> int = c()""".stripMargin,
        "", "geom/shim.c" -> impl, "geom/sized.h" -> header)

      val e = refused(app("""print(geom.value())""", s"""g { path = "$pkg" }"""))

      e should include("names a file this package does not carry")
    }

    /* Braces are what keep two files that must share a configuration from carrying two copies of
     * it. Both C files here read the same header and both need the same macro; the key says it once
     * and the build has to reach both.
     */
    "a braced key configures every file it names" in {
      val second =
        """#include "sized.h"
          |#ifdef DOUBLED
          |int sized_other(void) { return 800; }
          |#else
          |int sized_other(void) { return 0; }
          |#endif
          |""".stripMargin

      val pkg = packageSaying(
        """defines { "geom/{shim,other}.c" { DOUBLED = true } }""",
        "geom-lib", "geom",
        """extern "sized_value" c() -> int
          |extern "sized_other" d() -> int
          |
          |value() -> int = c() + d()""".stripMargin,
        "", "geom/shim.c" -> impl, "geom/other.c" -> second, "geom/sized.h" -> header)

      run(app("""print(geom.value())""", s"""g { path = "$pkg" }""")) shouldBe "842\n"
    }

    /* A macro is one package's business. Two packages carrying C compiled with the same option name
     * and different values is not a conflict, because neither one is a build-wide setting.
     */
    "are scoped to the package that declared them" in {
      val one = packageSaying(
        """defines { "left/util.c" { DOUBLED = true } }""",
        "one", "left",
        """extern "one_util" c() -> int
          |
          |value() -> int = c()""".stripMargin,
        "", "left/util.c" ->
          """#ifdef DOUBLED
            |int one_util(void) { return 40; }
            |#else
            |int one_util(void) { return 0; }
            |#endif
            |""".stripMargin)

      val two = packageOf("two", "left",
        """extern "two_util" c() -> int
          |
          |value() -> int = c()""".stripMargin,
        "", "left/util.c" ->
          """#ifdef DOUBLED
            |int two_util(void) { return 0; }
            |#else
            |int two_util(void) { return 2; }
            |#endif
            |""".stripMargin)

      run(app("""print(left.value() + other.left.value())""",
        s"""a { path = "$one" }, b { path = "$two", mount = "other" }""")) shouldBe "42\n"
    }
  }

  /** The executable takes the package's name, so a project named after its own module directory has
    * two things wanting one name — and it is the obvious layout rather than an exotic one: name the
    * package for the language and put the language in a directory of that name.
    *
    * Left to the linker it surfaced as `ld: open() failed, errno=21 (Is a directory)`, from a tool
    * the reader never invoked, naming neither the package nor the directory. The condition is known
    * long before clang is.
    *
    * **`run` never meets it**, since that writes to a temporary file — so a project can carry the
    * collision indefinitely and meet it the first time somebody asks for a binary.
    */
  "an executable that would be written over a directory" - {

    /** `build` rather than `run`, into a working directory of the test's own, since the default
      * output path is relative to where the project is.
      */
    def built(root: String): (Int, String) = {
      val notes  = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(
        Console.withErr(notes)(sh.sysl.execute(Config(command = "build", file = root))))

      (status, notes.toString)
    }

    /** A project whose package name is also the name of a module directory in it, which is what
      * makes the two collide.
      */
    def colliding(name: String): String = {
      val root = createTempDirectory("sysl-collide-")

      writeFile(s"$root/${PackageConfig.FileName}",
        s"""package { name = "$name", version = "0.1.0" }\n""")
      createDirectories(s"$root/$name")
      writeFile(s"$root/$name/$name.sysl", s"module $name\n\nvalue() -> int = 7\n")
      writeFile(s"$root/main.sysl", s"print($name.value())\n")
      root
    }

    "is refused, naming both names and what to do about it" in {
      val (status, said) = built(colliding("slate"))

      status should not be 0
      said should include("executable named 'slate'")
      said should include("Rename the package or the directory")
    }

    // The message is different when the path was written by hand, because the advice is: there is
    // no package name to rename, and the reader chose the path.
    "and says so differently when '-o' named the directory" in {
      val root   = colliding("slate")
      val notes  = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(Console.withErr(notes)(
        sh.sysl.execute(Config(command = "build", file = root, output = Some(s"$root/slate")))))

      status should not be 0
      notes.toString should include("names a directory")
    }

    // The other half of the threshold: a project whose directory does not share the package's name
    // builds, which is what says the refusal is about the collision and not about a module directory
    // being there at all.
    "while a package named something else builds" in {
      val root = colliding("slate")
      val out  = s"$root/slate-bin"
      val notes = new java.io.ByteArrayOutputStream
      val status = Console.withOut(Discarded)(Console.withErr(notes)(
        sh.sysl.execute(Config(command = "build", file = root, output = Some(out)))))

      withClue(notes.toString)(status shouldBe 0)
    }
  }

}
