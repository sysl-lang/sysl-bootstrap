package sh.sysl

import io.github.edadma.cross_platform.*

/** `sysl build-lib` — producing an artifact, driven through the driver itself.
 *
 * The compiler's own API cannot reach any of this. What a built container holds, what a library
 * carrying C source does to the object half, whether a temporary object survives the run — all of it
 * lives in `execute`, and a test that called `Compiler` and `Toolchain` directly would be
 * re-implementing the driver and pinning its own arrangement rather than the one a user meets.
 *
 * Which artifact a *compilation* is then given is the other half, and it is `LibraryCliTests`.
 */
class LibraryBuildCliTests extends LibraryCliSupport {

  "build-lib" - {

    "writes an artifact that carries both halves" in {
      val out   = artifact()
      val bytes = readBytes(out)

      LibraryArtifact.metadataOf(out, bytes) match
        case Right(meta) => meta should include("demo$double")
        case Left(err)   => fail(err)

      // The compiled half is not read back through any of our own code, so the check that it is
      // there has to be made against the container: a member of the name it was archived under, with
      // something in it. An artifact whose object half went missing would still decode perfectly and
      // fail at the link of every program that used it.
      Ar.members(bytes) match
        case Right(members) =>
          members.find(_.name == LibraryArtifact.codeMember).map(_.body.length).getOrElse(0) should be > 0
        case Left(why) => fail(why)
    }

    "names the artifact after the root when no output is given, and writes it inside it" in {
      // The default matters because it is what a reader gets when they follow the help text, and
      // an extension that did not match `--lib`'s test would make the two halves disagree.
      //
      // **Inside the root, not beside the caller.** The name is the root's, so writing it into the
      // working directory made the artifact's path depend on where the build was started — and for
      // `sysl build-lib .` the name was `.`, which produced `..syslib`: a hidden file, in somebody
      // else's directory, named after nothing. `OutputPathTests` holds the whole rule; this is the
      // half of it a library build meets.
      val root     = libraryRoot()
      val expected = s"$root/${Project.basename(root)}${LibraryArtifact.extension}"

      succeeds(Config(command = "build-lib", file = root))

      isFile(expected) shouldBe true
      deleteFile(expected)
    }

    "refuses a root holding no source rather than writing an empty artifact" in {
      refused(Config(command = "build-lib", file = createTempDirectory("sysl-cli-empty-")))
    }

    "takes the archiver it is told to use" in {
      // Worth pinning both ways round. That a named archiver is *used* is what the failing case below
      // cannot show on its own — an option that was read and then ignored would refuse a bad path
      // exactly as loudly while quietly building every library with something else.
      val ar = Toolchain.findAr(None) match
        case Right(path) => path
        case Left(why)   => cancel(why)

      val out = createTempFile("sysl-cli-named-ar-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = libraryRoot(), output = Some(out), ar = Some(ar)))

      LibraryArtifact.metadataOf(out, readBytes(out)) should matchPattern { case Right(_) => }
    }

    // **The metadata is found by its marker in the member's bytes**, and under LTO clang writes
    // bitcode, whose bitstream does not keep a string constant's bytes where a byte search finds
    // them. So the metadata member is an ordinary object whatever the code beside it is, and a
    // library built for an LTO project is still one the compiler can read.
    "builds under LTO into an artifact whose metadata still reads back" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val out = createTempFile("sysl-cli-lto-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = libraryRoot(), output = Some(out), lto = Some("thin")))

      LibraryArtifact.metadataOf(out, readBytes(out)) should matchPattern { case Right(_) => }
    }

    "refuses an archiver it cannot run rather than searching for another" in {
      // Someone who wrote down which archiver to use is owed the error. Falling back would build the
      // library with a different tool than the one asked for and say nothing about it — and the whole
      // reason to name one is a machine where the one that would be found is the wrong one.
      val out = createTempFile("sysl-cli-bad-ar-", LibraryArtifact.extension)
      val (status, notes) =
        diagnostics(Config(command = "build-lib", file = libraryRoot(), output = Some(out),
          ar = Some(s"${createTempDirectory("sysl-cli-noar-")}/llvm-ar")))

      status should not be 0
      notes should include("--ar")
    }

    "refuses a library that does not check, and writes nothing" in {
      val root = createTempDirectory("sysl-cli-bad-lib-")

      createDirectory(s"$root/demo")
      writeFile(s"$root/demo/lib.sysl", "module demo\n\nf() -> int = \"no\"\n")

      val out = s"$root/out${LibraryArtifact.extension}"

      refused(Config(command = "build-lib", file = root, output = Some(out)))
      isFile(out) shouldBe false
    }

    /* The members are staged in a directory of their own so they can be named, and that directory is
     * the one thing this command writes outside the artifact it was asked for. One per invocation
     * that is never removed is a leak nothing would report: a temporary directory is nobody's to
     * notice, and a machine that builds libraries all day fills up quietly.
     *
     * **Asked of this invocation's directory, by name.** The obvious test — count the `sysl-lib-`
     * entries in the system temp directory before and after — is a race against every other build on
     * the machine, three of the suites here included, since they create temporaries under that same
     * prefix and sbt runs six of them at once. It failed exactly that way during another branch's
     * gate, reporting `127 was not equal to 128`: two numbers one apart, which is what a single
     * leaked temporary would look like and is why it cost an afternoon to disbelieve. `--verbose`
     * names the directory, which leaves no shared namespace to be raced on.
     */
    "removes the directory it staged the members in" in {
      val out = createTempFile("sysl-cli-staged-", LibraryArtifact.extension)
      val (status, notes) =
        diagnostics(Config(command = "build-lib", file = libraryRoot(), output = Some(out), verbose = true))

      withClue(notes)(status shouldBe 0)
      exists(staged(notes)) shouldBe false
    }

    "and removes it when the build fails, which is where a leak would come from" in {
      assume(Toolchain.clangAvailable, "clang not available")

      // Cleanup runs before the outcome is examined, and this is the path that makes that worth
      // doing: a build that succeeded had every reason to tidy up after itself, while one that gave
      // up partway is where a directory gets left behind. The C is staged and compiled after the
      // module's own object, so the build reaches the staging directory and then fails.
      val out = createTempFile("sysl-cli-staged-bad-", LibraryArtifact.extension)
      val (status, notes) =
        diagnostics(Config(command = "build-lib", file = rootWithC("demo", library, "shim.c" -> "not C at all\n"),
          output = Some(out), verbose = true))

      status should not be 0
      exists(staged(notes)) shouldBe false
    }
  }

  /** The staging directory a verbose `build-lib` said it was using. A run that announced none never
   * got as far as staging, which makes the assertion above vacuous rather than satisfied — so it is
   * a failure here rather than a path that does not exist.
   */
  private def staged(notes: String): String = {
    val marker = "members staged in "

    notes.linesIterator.collectFirst {
      case line if line.contains(marker) => line.substring(line.indexOf(marker) + marker.length).trim
    }.getOrElse(fail(s"the build never said where it staged its members:\n$notes"))
  }

  /** A library built **on** another one, which is `--lib` at `build-lib` (`reference/ffi.md § A
   * library may carry C`). The org's case is `sdl3-ttf`, whose `Font` renders to an `sdl3`
   * `Surface`: without that library's declarations it does not compile, and until this worked the
   * flag was read off the command line and dropped.
   *
   * What `build-lib` still does not do is *fetch*. A `dependencies` block is a coordinate to resolve
   * over the network, and a command whose whole job is to compile one tree into an artifact for one
   * machine should not be the thing that goes looking — so the block is refused rather than acted on,
   * and the flag that answers it is named.
   */
  "a library built on another library" - {

    val dependent =
      """module skin
        |
        |import extra.triple
        |
        |sixfold(n: int) -> int = triple(triple(n))
        |""".stripMargin

    "takes one named as a source root" in {
      succeeds(Config(command = "build-lib", file = rootOf("skin", dependent),
        output = Some(createTempFile("sysl-cli-skin-", LibraryArtifact.extension)),
        libs = List(rootOf("extra", other))))
    }

    "takes one named as an artifact, which is the other thing --lib accepts" in {
      succeeds(Config(command = "build-lib", file = rootOf("skin", dependent),
        output = Some(createTempFile("sysl-cli-skin-", LibraryArtifact.extension)),
        libs = List(artifactOf(rootOf("extra", other)))))
    }

    "and cannot be built without it" in {
      // The pair is what says the flag was *used*. A `--lib` accepted and dropped refuses a bad path
      // exactly as loudly while compiling every library against nothing, which is what it did.
      val (status, notes) =
        diagnostics(Config(command = "build-lib", file = rootOf("skin", dependent),
          output = Some(createTempFile("sysl-cli-skin-", LibraryArtifact.extension))))

      status should not be 0
      notes should include("no module is called 'extra.triple'")
    }

    "and a program links the pair and runs" in {
      // The end of the chain, and the only place the split between the two artifacts is observable:
      // each half is compiled once, by whoever built it, and the program calls across the seam.
      val base = artifactOf(rootOf("extra", other))
      val out  = createTempFile("sysl-cli-skin-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = rootOf("skin", dependent), output = Some(out),
        libs = List(base)))

      ran(Config(command = "run", file = program("print(skin.sixfold(2))"),
        libs = List(out, base))) shouldBe "18\n"
    }

    "refuses a package whose dependencies it would have to fetch, and says what to write instead" in {
      val root = rootOf("skin", dependent)

      writeFile(s"$root/${PackageConfig.FileName}",
        "package { name = \"skin\" }\n\ndependencies {\n  extra { git = \"example.com/extra\", version = \"1.0.0\" }\n}\n")

      val (status, notes) =
        diagnostics(Config(command = "build-lib", file = root,
          output = Some(createTempFile("sysl-cli-skin-", LibraryArtifact.extension))))

      status should not be 0
      notes should include("'extra'")
      notes should include("--lib")
    }
  }

  "a library carrying C" - {

    /* A C file beside a library's sysl, compiled with it and archived into the same artifact
     * (`reference/ffi.md § A library may carry C`). It is what makes a binding to a real C library
     * writable: `sizeof(regex_t)`, the value of a macro like `REG_EXTENDED`, an anonymous union —
     * each is reachable from C and from nothing else, and a few lines of C turn each into an
     * ordinary function `extern` can declare.
     *
     * The sysl side needs nothing new. That is the claim these tests are really pinning: the whole
     * feature lives in the build, and a shim is reached by the `extern` that was already there. */

    val shim = "int demo_seven(void) { return 7; }\n"

    val usingShim =
      """module demo
        |
        |extern "demo_seven" c_seven() -> int
        |
        |seven_times(n: int) -> int = c_seven() * n
        |""".stripMargin

    def fingerprintOf(out: String): String =
      LibraryArtifact.metadataOf(out, readBytes(out)).flatMap(LibraryArtifact.read(out, _, Target.default)) match
        case Right((_, _, fingerprint)) => fingerprint
        case Left(err)                  => fail(err)

    "is archived as a member of its own, named after where it was found" in {
      val out = artifactOf(rootWithC("demo", usingShim, "shim.c" -> shim))

      Ar.members(readBytes(out)) match
        case Right(members) =>
          // The directory is kept in the name, which is what makes it unique across the library.
          members.find(_.name == "demo.shim.o").map(_.body.length).getOrElse(0) should be > 0
        case Left(why) => fail(why)
    }

    "and a program calling through the library reaches it" in {
      assume(Toolchain.clangAvailable, "clang not available")

      // The sharp shape, and not merely a program calling C directly: `demo$seven_times` lives in
      // the library's own compiled member and leaves `demo_seven` undefined, so the linker has to
      // resolve one member of the archive from another. An artifact that carried the shim but did
      // not index it would compile, link the first member, and fail here.
      ran(Config(command = "run", file = program("print(demo.seven_times(3))"),
        libs = List(artifactOf(rootWithC("demo", usingShim, "shim.c" -> shim))))) shouldBe "21\n"
    }

    "which is what makes a caller-allocated C type bindable" in {
      assume(Toolchain.clangAvailable, "clang not available")

      // The case the feature exists for. `regex_t` has to be allocated by the caller and its size is
      // known only to the target's own headers — 32 bytes here, 64 under glibc — so a sysl program
      // has no way to spell the storage. The shim allocates it, and the sysl side never learns the
      // size at all: it holds an opaque `*u8` and the numbers stay where they are checked.
      //
      // `REG_EXTENDED` is the same problem in miniature. It is a `#define`, so it has no symbol to
      // link against and nothing but C can read it.
      val regexShim =
        """#include <regex.h>
          |#include <stdlib.h>
          |
          |void *demo_regex_new(void) { return malloc(sizeof(regex_t)); }
          |
          |int demo_regex_compile(void *re, const char *pattern) {
          |    return regcomp((regex_t *)re, pattern, REG_EXTENDED);
          |}
          |
          |int demo_regex_matches(void *re, const char *s) {
          |    return regexec((regex_t *)re, s, 0, NULL, 0) == 0;
          |}
          |
          |void demo_regex_free(void *re) {
          |    regfree((regex_t *)re);
          |    free(re);
          |}
          |""".stripMargin

      val binding =
        """module rx
          |
          |extern "demo_regex_new" regex_new() -> *u8
          |extern "demo_regex_compile" regex_compile(re: *u8, pattern: *u8) -> int
          |extern "demo_regex_matches" regex_matches(re: *u8, s: *u8) -> int
          |extern "demo_regex_free" regex_free(re: *u8)
          |""".stripMargin

      val prog =
        """var re = rx.regex_new()
          |print(rx.regex_compile(re, c"^a+b$") == 0)
          |print(rx.regex_matches(re, c"aaab") == 1)
          |print(rx.regex_matches(re, c"xyz") == 1)
          |rx.regex_free(re)
          |""".stripMargin

      // Discriminating on purpose: a match, and a non-match of the same pattern. A binding that
      // returned a constant, or one whose `regex_t` was too small to survive being written into,
      // would pass on the first line and fail on one of the others.
      ran(Config(command = "run", file = program(prog),
        libs = List(artifactOf(rootWithC("rx", binding, "regex.c" -> regexShim))))) shouldBe
        "true\ntrue\nfalse\n"
    }

    "is found at the root of the tree as well as beside a module" in {
      // `reference/ffi.md § A library may carry C` says *anywhere* in the tree, and the root is the
      // one place that is not a module's directory — nothing there declares a module, so a walk
      // that gathered C only where it had found sysl would skip it. The member takes the bare name,
      // there being no directory to carry.
      assume(Toolchain.clangAvailable, "clang not available")

      val root = rootWithC("demo", """module demo
                                     |
                                     |extern "demo_root" c_root() -> int
                                     |
                                     |from_root() -> int = c_root()
                                     |""".stripMargin)

      writeFile(s"$root/shared.c", "int demo_root(void) { return 13; }\n")

      val out = artifactOf(root)

      Ar.members(readBytes(out)) match
        case Right(members) => members.map(_.name) should contain("shared.o")
        case Left(why)      => fail(why)

      ran(Config(command = "run", file = program("print(demo.from_root())"), libs = List(out))) shouldBe "13\n"
    }

    "and two modules may each hold a file of the same name" in {
      // `ar r` replaces by name, so a member name built from the basename alone would have the
      // second of these silently evict the first — and the library would ship missing whatever only
      // the first defined. Nothing else in the suite would notice.
      val root = createTempDirectory("sysl-cli-two-c-")

      for (module, symbol, value) <- List(("one", "one_util", 1), ("two", "two_util", 2)) do {
        createDirectory(s"$root/$module")
        writeFile(s"$root/$module/lib.sysl",
          s"""module $module
             |
             |extern "$symbol" util() -> int
             |""".stripMargin)
        writeFile(s"$root/$module/util.c", s"int $symbol(void) { return $value; }\n")
      }

      val out = createTempFile("sysl-cli-two-c-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = root, output = Some(out)))

      Ar.members(readBytes(out)) match
        case Right(members) => members.map(_.name) should contain allOf ("one.util.o", "two.util.o")
        case Left(why)      => fail(why)

      assume(Toolchain.clangAvailable, "clang not available")

      // And both still resolve, which is the part the member names are in aid of.
      ran(Config(command = "run", file = program("print(one.util() + two.util())"),
        libs = List(out))) shouldBe "3\n"
    }

    // The collision this used to reach — a `sysl/code.c` taking the name the library's own compiled
    // half uses — is no longer reachable from a tree like this one, because `reference/ffi.md § A
    // library may carry C` takes C only from a module or the root and a bare `sysl/` is neither.
    // The guard itself is not dead: a `build-lib --std` walks a tree whose modules genuinely are
    // `sysl/…`, which is the only place the name can now be produced, and `LibraryArtifactTests`
    // pins it on `collisions` directly.
    "while a C file in a directory that declares no module is skipped rather than archived" in {
      val root = createTempDirectory("sysl-cli-clash-")

      createDirectory(s"$root/demo")
      writeFile(s"$root/demo/lib.sysl", library)
      createDirectory(s"$root/sysl")
      writeFile(s"$root/sysl/code.c", "int f(void) { return 0; }\n")

      val out = createTempFile("sysl-cli-clash-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = root, output = Some(out)))

      assume(Toolchain.clangAvailable, "clang not available")

      // The member name is the point rather than the member count: had the stray C been archived it
      // would have taken `sysl.code.o` and evicted the library's own compiled half, leaving an
      // artifact that reads back perfectly and links nothing. A program running off it says it did
      // not, which no assertion about names can — both spellings are the same string.
      ran(Config(command = "run", file = program("print(demo.double(21))"),
        libs = List(out))) shouldBe "42\n"
    }

    "a C file that does not compile stops the build and names itself" in {
      // The error path. Without the file in the message the user is handed clang's complaint about a
      // path under a temporary directory, with nothing saying which of their sources it came from.
      val root = rootWithC("demo", library, "broken.c" -> "int oops(void) { return \n")
      val out  = createTempFile("sysl-cli-bad-c-", LibraryArtifact.extension)

      deleteFile(out)

      val (status, notes) = diagnostics(Config(command = "build-lib", file = root, output = Some(out)))

      status should not be 0
      notes should include("broken.c")
      isFile(out) shouldBe false
    }

    "and the binding it makes possible really matches, at offsets nothing could have guessed" in {
      assume(Toolchain.clangAvailable, "clang not available")

      // A binding built the way a real one is: the C holds everything only a header knows — the size
      // of a `regex_t`, `REG_EXTENDED`, the layout of a `regmatch_t` — and the sysl side holds an
      // opaque pointer and four integers.
      //
      // Every case below is chosen so that only real POSIX matching gives the number. A test whose
      // matches all began at 0 would be satisfied by a binding that answered `0..len` to anything
      // that matched at all, which is the coincidence worth ruling out: here a match starts and ends
      // mid-string, a bounded repeat has to stop at its ceiling, an anchor refuses, a class is
      // negated, case matters, a group carries a quantifier, and one match is empty.
      //
      // The offsets are POSIX and portable. The message for the bad pattern is the C library's own,
      // worded differently by BSD and glibc, so only its presence is pinned.
      val regexShim =
        """#include <regex.h>
          |#include <stdlib.h>
          |
          |void *probe_rx_new(void) { return calloc(1, sizeof(regex_t)); }
          |
          |void probe_rx_free(void *re) {
          |    if (re) { regfree((regex_t *)re); free(re); }
          |}
          |
          |int probe_rx_compile(void *re, const char *pattern) {
          |    return regcomp((regex_t *)re, pattern, REG_EXTENDED);
          |}
          |
          |int probe_rx_exec(void *re, const char *s, long *so, long *eo) {
          |    regmatch_t m;
          |
          |    if (regexec((regex_t *)re, s, 1, &m, 0) != 0) return 0;
          |
          |    *so = (long)m.rm_so;
          |    *eo = (long)m.rm_eo;
          |
          |    return 1;
          |}
          |
          |void probe_rx_error(void *re, int code, char *buf, unsigned long n) {
          |    regerror(code, (regex_t *)re, buf, (size_t)n);
          |}
          |""".stripMargin

      val binding =
        """module rx
          |
          |import sysl.text.{cstring, from_cstring}
          |
          |extern "probe_rx_new" c_new() -> *u8
          |extern "probe_rx_free" c_free(re: *u8)
          |extern "probe_rx_compile" c_compile(re: *u8, pattern: *u8) -> int
          |extern "probe_rx_exec" c_exec(re: *u8, s: *u8, so: *i64, eo: *i64) -> int
          |extern "probe_rx_error" c_error(re: *u8, code: int, buf: *u8, n: u64)
          |
          |struct Match
          |    start: usize
          |    end: usize
          |
          |struct Regex
          |    handle: *u8
          |
          |    find(self, s: string) -> Option[Match]
          |        val subject = cstring(s)
          |
          |        var so: i64 = 0i64
          |        var eo: i64 = 0i64
          |
          |        if c_exec(self.handle, subject.ptr, &so, &eo) == 1 then
          |            Some(Match(usize(so), usize(eo)))
          |        else
          |            None
          |    end find
          |
          |    free(self) = c_free(self.handle)
          |end Regex
          |
          |compile(pattern: string) -> Result[Regex, string]
          |    val re   = c_new()
          |    val p    = cstring(pattern)
          |    val code = c_compile(re, p.ptr)
          |
          |    if code == 0 then
          |        Ok(Regex(re))
          |    else
          |        var buf: [256]u8
          |
          |        c_error(re, code, &buf[0], 256u64)
          |        c_free(re)
          |        Err(from_cstring(&buf[0]).unwrap_or("not a pattern"))
          |end compile
          |""".stripMargin

      val out = artifactOf(rootWithC("rx", binding, "regex.c" -> regexShim))

      val prog =
        """import rx.*
          |
          |show(pat: string, s: string) =
          |    compile(pat) match
          |        Ok(re) ->
          |            re.find(s) match
          |                Some(m) -> print(f"${m.start}..${m.end}")
          |                None -> print("none")
          |            re.free()
          |
          |        Err(why) -> print(f"error: ${why}")
          |
          |show("[0-9]+", "abc123def")
          |show("cat|dog", "hotdog stand")
          |show("a{2,3}", "aaaa")
          |show("^abc$", "xabcx")
          |show("^abc$", "abc")
          |show("[^aeiou]+", "aeixyzou")
          |show("ABC", "xxabcxx")
          |show("(ab)+", "zzababab!!")
          |show("x*", "yyy")
          |show("a{3,1}", "aaa")
          |""".stripMargin

      val lines = ran(Config(command = "run", file = program(prog), libs = List(out))).linesIterator.toList

      lines.take(9) shouldBe
        List("3..6", "3..6", "0..3", "none", "0..3", "3..6", "none", "2..8", "0..0")

      lines(9) should startWith("error: ")
      lines(9).length should be > "error: ".length
    }

    "and editing only the C changes what the artifact fingerprints as" in {
      // A library's shims are as much its source as its modules are. An artifact that did not change
      // when one of them was edited is a stale artifact nothing would notice was stale — which is
      // exactly the failure `Stdlib.read`'s fingerprint check exists to catch for the sysl half.
      val before = fingerprintOf(artifactOf(rootWithC("demo", usingShim, "shim.c" -> shim)))
      val after =
        fingerprintOf(artifactOf(rootWithC("demo", usingShim,
          "shim.c" -> "int demo_seven(void) { return 8; }\n")))

      before should not be after
    }
  }
  /** A closure lowered inside a **generic** body carries that body's type parameters in its own
    * signature, and no value at run time has such a type — so it must never reach a backend.
    *
    * **A library is the only place it can escape**, which is why this sat undetected. In a program the
    * enclosing generic is instantiated and a concrete copy is emitted beside the abstract one, which
    * nothing then asks about; `compileLibrary` strips `@tests` *before* analysis, so in a library
    * nothing instantiates the generic at all and the abstract closure is the only copy there is. It
    * crashed rather than diagnosing — `IllegalStateException: the type parameter 'T' reached codegen`
    * out of `Type.Abstract.llvm` — which is never a correct answer to legal input.
    *
    * The shape is `sysl.slices`' own, and `reference/types.md § Function types` names a comparator
    * passed to a sort as the bare arrow's motivating case, so it is one the language invites rather
    * than an exotic corner.
    */
  "a closure written inside a generic, which nothing in the library instantiates" - {

    "is dropped rather than reaching a backend that has no layout for its type parameter" in {
      val root = rootOf("cmp",
        """module cmp
          |
          |sorted[T: Ord](xs: []const T) -> bool = sorted_by(xs, (a, b) -> a < b)
          |
          |sorted_by[T](xs: []const T, lt: (T, T) -> bool) -> bool
          |    for i in 1..<xs.len
          |        if lt(xs[i], xs[i - 1]) then return false
          |
          |    true
          |""".stripMargin)

      val out = createTempFile("sysl-cli-generic-closure-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = root, output = Some(out)))

      // And what came out is a readable artifact carrying the module, not merely a command that
      // exited zero.
      LibraryArtifact.metadataOf(out, readBytes(out)).flatMap(LibraryArtifact.read(out, _, Target.default)) match
        case Right((trees, _, _)) => trees.flatMap(_.module.map(_.show)) shouldBe List("cmp")
        case Left(err)            => fail(err)

      deleteFile(out)
    }
  }

  "build-lib --std" - {

    "builds the standard module, which nothing else may declare" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val out = createTempFile("sysl-cli-std-", LibraryArtifact.extension)

      succeeds(Config(command = "build-lib", file = StdRoot.root.get, output = Some(out), std = true))

      LibraryArtifact.metadataOf(out, readBytes(out)).flatMap(LibraryArtifact.read(out, _, Target.default)) match
        case Right((trees, syms, fingerprint)) =>
          // Every symbol is one of the library's own modules'. A library defines its own
          // declarations and nobody else's, and the standard module library is the one place that rule is under
          // the most pressure, since the whole of the rest of the library is what it was compiled
          // against. `sysl.args` is in here as well as `sysl`, which is the point of building the
          // whole tree rather than the standard module alone.
          //
          // A member of a built-in type is keyed under the type and names no module — `char.is_digit`
          // is `sysl.text`'s by its `impl`, which the coherence rule puts nowhere else — so those are
          // the library's too, and advertised; a closure's key names none either, and is never here.
          // A blanket implementation's members are keyed under `bound.<trait>` for the same reason —
          // the implementing type is every type meeting the bound — and are the `impl`'s module's.
          def keyedElsewhere(s: String): Boolean =
            val m = Modules.moduleOf(s)
            m.nonEmpty && !Library.modules.contains(m) && !s.startsWith("bound.")

          syms should not be empty
          syms.filter(keyedElsewhere) shouldBe empty
          syms should contain("char.is_digit")
          syms.filter(Closures.mentioned) shouldBe empty
          syms.map(Modules.moduleOf).size should be > 1
          trees.flatMap(_.module.map(_.show)).distinct.sorted shouldBe Library.modules

          // And it fingerprints as the library the compiler carries, though this one was walked off
          // disk and named by where it was found while the carried copy is named by where the
          // generator read it. If the fingerprint were over paths rather than contents, the guard in
          // `Stdlib.read` would reject every artifact the documented command produces.
          fingerprint shouldBe Std.fingerprint(Target.default.os)
        case Left(err) => fail(err)

      deleteFile(out)
    }

    /** **Building a tree that is not the one this machine resolves is allowed, and says so.**
     * `Std.candidates` tries the installed library before the working directory, so an installed
     * sysl run inside a checkout resolves the installed library while being handed the checkout's —
     * and the artifact it writes is keyed to the tree it compiled, which nothing here will read.
     *
     * Reported rather than refused: the command is mostly run as a type-check of a library being
     * worked on, and a refusal would stop it doing the one thing it was wanted for.
     */
    "says so when the tree given is not the library a compilation here resolves" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val copy = createTempDirectory("sysl-cli-other-")
      val out  = createTempFile("sysl-cli-std-", LibraryArtifact.extension)

      try
        copyTree(StdRoot.root.get, copy)
        writeFile(s"$copy/sysl/marker.sysl", "module sysl\n\nprivate[sysl] a_marker() -> int = 1\n")

        val (status, notes) = diagnostics(Config(command = "build-lib", file = copy, output = Some(out), std = true))

        withClue(notes)(status shouldBe 0)
        notes should include("not the library a compilation here resolves")
        notes should include(StdRoot.root.get)
      finally
        discardTree(copy)
        deleteFile(out)
    }

    // And the ordinary case says nothing, which is the half that keeps the note worth reading.
    "and says nothing when it is" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val out = createTempFile("sysl-cli-std-", LibraryArtifact.extension)

      val (status, notes) =
        diagnostics(Config(command = "build-lib", file = StdRoot.root.get, output = Some(out), std = true))

      withClue(notes)(status shouldBe 0)
      notes should not include "not the library a compilation here resolves"

      deleteFile(out)
    }

    "refuses to build the standard module against a prebuilt copy of itself" in {
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      // The one combination that cannot mean anything: the declarations being compiled are the ones
      // the artifact holds. Refused before the artifact is even read — ignoring it would leave a
      // command line reading as though it were used, which is the failure worth preventing.
      refused(Config(command = "build-lib", file = StdRoot.root.get, std = true,
        stdLib = Some(s"${createTempDirectory("sysl-cli-std-")}/any${LibraryArtifact.extension}")))
    }

    "and without it the same root is refused, because the module is the library's" in {
      // The refusal is the ordinary one every program gets, and keeping it is the point: inferring
      // the mode from the module names in the tree would turn a clear diagnostic into an artifact
      // that builds and then collides with the built-in copy at whatever link tried to use it.
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val out = createTempFile("sysl-cli-std-", LibraryArtifact.extension)

      deleteFile(out)
      refused(Config(command = "build-lib", file = StdRoot.root.get, output = Some(out)))
      isFile(out) shouldBe false
    }
  }
}
