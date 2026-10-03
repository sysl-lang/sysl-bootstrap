package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `sysl run` keeps what it built, so running the same program twice costs the second one nothing
 * (card `0309`, `RunCache`).
 *
 * **What a cache has to be right about is the MISS, not the hit.** A hit that should have been a
 * miss is a stale binary — a program that runs the code it had before the edit, silently — which is
 * worse than any amount of slowness. So most of what is below is about the key changing: every input
 * that reaches the bytes is perturbed in turn and the entry count is what says whether the key
 * noticed.
 *
 * The cache directory is this suite's own, which is what makes counting entries meaningful at all.
 */
class RunCacheTests extends AnyFreeSpec with Matchers {

  private def withCache[T](body: String => T): T = {
    val cache = createTempDirectory("sysl-runcache-")

    Fetch.usingCache(cache)(RunCache.usingCache(cache)(body(cache)))
  }

  /** The driver, run against a cache of the test's own. `Fetch.usingCache` moves the package cache
   * and `RunCache.usingCache` moves this one, so nothing here reaches the developer's.
   */
  private def ran(cfg: Config): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(Console.withErr(notes)(sh.sysl.execute(cfg)))

    if status != 0 then fail(s"the driver exited with $status:\n${out.toString}${notes.toString}")

    out.toString
  }

  /** A report with its durations removed, so that two of them can be compared. */
  private def untimed(report: String): String = report.replaceAll("[0-9]+ms", "<ms>")

  private def entries(cache: String): Int =
    if isDirectory(s"$cache/sysl/run") then listFiles(s"$cache/sysl/run").length else 0

  private def program(text: String): String = {
    val root = createTempDirectory("sysl-run-")

    writeFile(s"$root/main.sysl", text)
    root
  }

  "the same program run twice is built once" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 1

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 1
    }
  }

  // `__NAME__` and `__VERSION__` fold the manifest into the program, and the manifest is not a source
  // file — so a key over the sources alone replayed the old binary after either was changed.
  "a manifest whose version or name changed is a different program, though no source file moved" in
    withCache { cache =>
      {
        val root = program("print(__NAME__, __VERSION__)\n")

        def stating(name: String, version: String): String = {
          writeFile(s"$root/${PackageConfig.FileName}", s"""package { name = "$name", version = "$version" }\n""")
          ran(Config(command = "run", file = root))
        }

        stating("app", "1.0.0") shouldBe "app 1.0.0\n"
        stating("app", "1.0.1") shouldBe "app 1.0.1\n"
        stating("tool", "1.0.1") shouldBe "tool 1.0.1\n"
        entries(cache) shouldBe 3

        stating("app", "1.0.0") shouldBe "app 1.0.0\n"
        entries(cache) shouldBe 3
      }
    }

  "and the arguments it is given are not part of the key, which is the point" in withCache { cache =>
    {
      val root = program("""main(args: []string)
                           |    print(args[1])
                           |""".stripMargin)

      ran(Config(command = "run", file = root, programArgs = List("a"))) shouldBe "a\n"
      ran(Config(command = "run", file = root, programArgs = List("b"))) shouldBe "b\n"

      entries(cache) shouldBe 1
    }
  }

  "an edit is a different program" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"

      writeFile(s"$root/main.sysl", """print(21 * 3)""")

      ran(Config(command = "run", file = root)) shouldBe "63\n"
      entries(cache) shouldBe 2
    }
  }

  /** **Another build of the same version is another compiler** (`CompilerIdentity`). A development
   * tree changes the compiler under one `BuildInfo.version`, and keyed on the version alone a build
   * replayed the binary an earlier build had cached for an identical program.
   */
  "a different build of this version is a different compiler, and the same build is the same" in withCache { cache =>
    {
      val root  = program("""print(21 * 2)""")
      val other = s"${BuildInfo.version}+0123456789abcdef"

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 1

      CompilerIdentity.as(other)(ran(Config(command = "run", file = root))) shouldBe "42\n"
      entries(cache) shouldBe 2

      CompilerIdentity.as(other)(ran(Config(command = "run", file = root))) shouldBe "42\n"
      CompilerIdentity.as(CompilerIdentity.current)(ran(Config(command = "run", file = root))) shouldBe "42\n"
      entries(cache) shouldBe 2
    }
  }

  "and what another build cached is not replayed, whatever it holds" in withCache { cache =>
    {
      val root  = program("""print(21 * 2)""")
      val other = s"${BuildInfo.version}+0123456789abcdef"

      CompilerIdentity.as(other)(ran(Config(command = "run", file = root))) shouldBe "42\n"

      // What an older build left: here a program printing something else entirely, standing in
      // for a binary that build's lowering made.
      val slot = listFiles(s"$cache/sysl/run").head

      writeFile(slot, "#!/bin/sh\necho stale\n")
      CompilerIdentity.as(other)(ran(Config(command = "run", file = root))) shouldBe "stale\n"

      ran(Config(command = "run", file = root)) shouldBe "42\n"
    }
  }

  /** **A comment is an edit.** The key is over the file's text rather than over anything the parser
   * decided, which is the conservative direction: a key that tried to see through a comment would be
   * a key that had to be right about what a comment is.
   */
  "including one the program's behaviour does not depend on" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"

      writeFile(s"$root/main.sysl", "// a note\nprint(21 * 2)")

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 2
    }
  }

  "and so is a second file beside it, which the first one's text says nothing about" in withCache { cache =>
    {
      val root = program("""import m.*
                           |
                           |print(double(21))
                           |""".stripMargin)

      createDirectories(s"$root/m")
      writeFile(s"$root/m/m.sysl", "module m\n\ndouble(n: int) -> int = n * 2\n")

      ran(Config(command = "run", file = root)) shouldBe "42\n"

      writeFile(s"$root/m/m.sysl", "module m\n\ndouble(n: int) -> int = n * 3\n")

      ran(Config(command = "run", file = root)) shouldBe "63\n"
      entries(cache) shouldBe 2
    }
  }

  "the optimization level changes what is emitted, so it changes the key" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      ran(Config(command = "run", file = root, optimize = Some("2"))) shouldBe "42\n"

      entries(cache) shouldBe 2
    }
  }

  /** The escape hatch, which is for working **on the compiler** — where the version in the key
   * stands still while the bytes it produces do not.
   */
  "'SYSL_NO_CACHE' builds every time and keeps nothing" in withCache { cache =>
    {
      RunCache.disabledFor {
        val root = program("""print(21 * 2)""")

        ran(Config(command = "run", file = root)) shouldBe "42\n"
        ran(Config(command = "run", file = root)) shouldBe "42\n"

        entries(cache) shouldBe 0
      }
    }
  }

  /** `sysl test` is the same shape and the case the card says the cost is felt in most often — a
   * suite that recompiles on every run. It needs **two** things from the cache, because the
   * executable does not carry what to call in it, so the sidecar is written beside it.
   */
  "a test suite run twice is built once, and the report is the same either way" in withCache { cache =>
    {
      val root = program("""@test("two doubled is four")
                           |doubling() =
                           |    assert(2 * 2 == 4)
                           |
                           |@test
                           |adding() =
                           |    assert(1 + 1 == 2)
                           |""".stripMargin)

      val first = ran(Config(command = "test", file = root))

      entries(cache) shouldBe 2

      val second = ran(Config(command = "test", file = root))

      // Compared with the **timings taken out**, which are the one part of a report that is a clock
      // rather than a fact: two runs of one suite differ by a millisecond and are the same report.
      untimed(second) shouldBe untimed(first)
      first should include("two doubled is four")
      first should include("2 passed")
      entries(cache) shouldBe 2
    }
  }

  /** **The fingerprint is over each file's place in its tree, so two identical trees in two
   * directories fingerprint alike** — and a test build's report heads each file with its full path.
   * The key has to carry the paths as well, or the second tree replays the first one's build and
   * reports the other directory as its own.
   */
  "an identical tree in another directory is another program, and reports its own path" in withCache { cache =>
    {
      val text  = """@test
                    |four() =
                    |    assert(2 * 2 == 4)
                    |""".stripMargin
      val one   = program(text)
      val two   = program(text)
      val first = ran(Config(command = "test", file = one))

      first should include(s"$one/main.sysl")

      val second = ran(Config(command = "test", file = two))

      second should include(s"$two/main.sysl")
      second should not include one
      entries(cache) shouldBe 4
    }
  }

  /** The filter picks from the list rather than deciding what is compiled, so it is not in the key —
   * and a cached run has to apply it exactly as a fresh one does, which is why `rerun` is the tail
   * of `run` rather than a second implementation.
   */
  "and a filter is applied to a cached suite as it is to a fresh one" in withCache { cache =>
    {
      val root = program("""@test
                           |doubling() =
                           |    assert(2 * 2 == 4)
                           |
                           |@test
                           |adding() =
                           |    assert(1 + 1 == 2)
                           |""".stripMargin)

      ran(Config(command = "test", file = root)) should include("2 passed")

      val filtered = ran(Config(command = "test", file = root, filter = Some("doubling")))

      filtered should include("1 passed")
      filtered should not include "adding"
      entries(cache) shouldBe 2
    }
  }

  "a 'run' and a 'test' of one tree are two entries, since they are two builds" in withCache { cache =>
    {
      val root = program("""print(21 * 2)
                           |
                           |@test
                           |arithmetic() =
                           |    assert(1 + 1 == 2)
                           |""".stripMargin)

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      ran(Config(command = "test", file = root)) should include("1 passed")

      entries(cache) shouldBe 3
    }
  }

  /** **The environment that reaches the toolchain is part of the key** (card `0415`).
   *
   * Everything else in the key is settled by the command line and the source tree; this was the half
   * that was not, and it was missing. The org file's own recipe for checking a binding is *set
   * `SYSL_EXTRA_CFLAGS="-fsanitize=address"`, re-run `sysl test .`* — over an unchanged tree that
   * replayed the **uninstrumented** binary the previous ordinary run had left in the slot, and
   * reported green having looked at nothing.
   *
   * **A sanitizer is the worst place a cache can be stale, because its whole output is an absence.**
   * A wrong number would have been noticed on sight; a clean run is what a clean run looks like.
   *
   * `-fno-omit-frame-pointer` stands in for the sanitizer here: it reaches every clang the build
   * drives exactly as `-fsanitize=address` does, and costs the suite no runtime to link.
   *
   * **`-g` was the obvious stand-in and it is the wrong one**, which is worth a sentence because the
   * failure looks like the fix being broken: on Darwin the driver runs `dsymutil` after the link, so
   * a `-g` build writes a `prog.dSYM` **beside the executable in the cache slot** and the entry count
   * comes back one too high. Anything that counts what a build left on disk wants a flag that leaves
   * one file.
   */
  "the extra clang flags" - {

    "are part of the key, so a run under a new value builds again" in withCache { cache =>
      {
        val root = program("""print(21 * 2)""")

        ran(Config(command = "run", file = root)) shouldBe "42\n"
        entries(cache) shouldBe 1

        Toolchain.usingEnvironment(Map("SYSL_EXTRA_CFLAGS" -> "-fno-omit-frame-pointer")) {
          ran(Config(command = "run", file = root)) shouldBe "42\n"
        }

        entries(cache) shouldBe 2
      }
    }

    // The key is over the flags' *value*, not over the fact that an environment was consulted — so
    // the second run of a sanitizer build is still free, which is what makes the fix affordable.
    "and the same value twice is still one build" in withCache { cache =>
      {
        val root = program("""print(21 * 2)""")

        Toolchain.usingEnvironment(Map("SYSL_EXTRA_CFLAGS" -> "-fno-omit-frame-pointer")) {
          ran(Config(command = "run", file = root)) shouldBe "42\n"
          ran(Config(command = "run", file = root)) shouldBe "42\n"
        }

        entries(cache) shouldBe 1
      }
    }

    // A variable that is not set contributes nothing, so adding this to the key invalidated no entry
    // anybody already had — the ordinary build's key is exactly what it was.
    "contribute nothing to the key when nothing is set" in {
      Toolchain.usingEnvironment(Map.empty)(Toolchain.buildEnvironment) shouldBe empty
    }

    "and are named with their value, so two settings cannot share an entry" in {
      Toolchain.usingEnvironment(Map("SYSL_EXTRA_CFLAGS" -> "-fno-omit-frame-pointer"))(Toolchain.buildEnvironment)
        .shouldBe(List("SYSL_EXTRA_CFLAGS=-fno-omit-frame-pointer"))
    }

    /** **`SYSL_LIB` is deliberately not in it, and that is the more correct answer rather than an
     * omission.** It names where the library source *is*, and the key already carries that library's
     * fingerprint — its contents. Including the path would make two identical trees at two paths
     * miss each other's entry for no gain.
     */
    "and 'SYSL_LIB' is not among them, because the library's contents are already in the key" in {
      Toolchain.usingEnvironment(Map("SYSL_LIB" -> "/somewhere"))(Toolchain.buildEnvironment) shouldBe empty
    }

    // The four Android variables and WASI's name a cross toolchain — a different compiler and a
    // different sysroot, so different bytes from the same source.
    "and a cross toolchain's location is in the key too, since it decides which compiler answers" in {
      Toolchain.usingEnvironment(Map("ANDROID_HOME" -> "/sdk"))(Toolchain.buildEnvironment)
        .shouldBe(List("ANDROID_HOME=/sdk"))
      Toolchain.usingEnvironment(Map("WASI_SDK_PATH" -> "/wasi"))(Toolchain.buildEnvironment)
        .shouldBe(List("WASI_SDK_PATH=/wasi"))
    }
  }

  /** `build` writes a binary somebody named and is expected to have built it; `build-c` and
   * `build-lib` write artifacts for somebody else's toolchain. None of them is something a reader
   * would want quietly skipped, so none of them consults this.
   */
  "no other command keeps anything" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")
      val out  = createTempDirectory("sysl-out-")

      ran(Config(command = "build", file = root, output = Some(s"$out/app")))
      entries(cache) shouldBe 0
    }
  }

  "the test sidecar carries a suite back exactly as it went in" - {
    // The sidecar is how a cached suite reaches the runner: the binary alone says which tests exist,
    // and nothing in it says what to call them or which hooks bracket them. A field that did not
    // survive the round trip is a cached run behaving differently from a fresh one, which is the one
    // thing a cache may never do.
    val suite = List(
      TTest("m$plain", "a test with no hooks around it", false, None, "m.sysl", 3),
      TTest("m$traps", "one that should trap", true, Some("past the end"), "m.sysl", 9,
            THooks(setup = Some(THook(HookKind.Setup, "m$up", "m.sysl", 12)),
                   teardownAll = Some(THook(HookKind.TeardownAll, "m$halt", "m.sysl", 20)))),
      TTest("m$waits", "one that is ignored", false, None, "m.sysl", 30,
            ignored = Some("the parser drops the second arm")),
    )

    "every field comes back" in {
      RunCache.decode(RunCache.encode(suite)) shouldBe Some(suite)
    }

    // A cached suite that forgot a test was ignored would *run* it — the one difference between a
    // cached run and a fresh one the report could not hide.
    "an ignored test comes back ignored, with its reason, and the others come back not ignored" in {
      RunCache.decode(RunCache.encode(suite)).get.map(_.ignored) shouldBe
        List(None, None, Some("the parser drops the second arm"))
    }

    // A sidecar written before `ignore` existed has two fewer fields per line; reading it as a suite
    // with nothing ignored would be a guess, and a rebuild is the answer that cannot be wrong.
    "a sidecar written before 'ignore' existed is no cache at all" in {
      val older = RunCache.encode(suite.take(1)).split("\u0000", -1).dropRight(2).mkString("\u0000")

      RunCache.decode(older) shouldBe None
    }

    "a test with no hooks comes back with none" in {
      RunCache.decode(RunCache.encode(suite)).get.head.hooks shouldBe THooks()
    }

    // A sidecar written before the hooks existed has fewer fields per line, and the read is what has
    // to notice: `None` is a rebuild, and a rebuild is the designed answer.
    "a line of the wrong shape is no cache at all" in {
      RunCache.decode(List("m$plain", "a test", "false", "", "0", "m.sysl", "3").mkString("\u0000")) shouldBe None
    }
  }

  /** Two runs of one program compute one slot, so what lands there has to arrive whole: a binary
   * linked straight into the slot could be executed half-written by the other run, and a test list
   * written in place decodes cleanly to fewer tests than the suite has. `Publish` is how every cache
   * write arrives, and these are its promises.
   */
  "publishing into the cache" - {
    "a pending name is beside its target, and no two calls share one" in {
      val dir    = createTempDirectory("sysl-publish-")
      val target = s"$dir/slot"
      val names  = List.fill(200)(Publish.pending(target))

      names.flatMap(Project.parentOf).distinct shouldBe List(dir)
      names.foreach(n => Project.basename(n) should startWith("slot."))
      names.distinct.length shouldBe names.length
    }

    "and none shared between threads either" in {
      val target = s"${createTempDirectory("sysl-publish-")}/slot"
      val names  = new java.util.concurrent.ConcurrentLinkedQueue[String]
      val pool   = List.fill(8)(new Thread(() => for _ <- 1 to 250 do names.add(Publish.pending(target))))

      pool.foreach(_.start())
      pool.foreach(_.join())
      names.size shouldBe 2000
      names.toArray.distinct.length shouldBe 2000
    }

    "a published file is exactly what was written, and nothing is left beside it" in {
      val dir = createTempDirectory("sysl-publish-")

      Publish.text(s"$dir/slot.tests", "one\ntwo\n") shouldBe Right(())
      readFile(s"$dir/slot.tests") shouldBe "one\ntwo\n"
      listFiles(dir).map(Project.basename).toList shouldBe List("slot.tests")
    }

    "publishing over an entry replaces the whole of it" in {
      val dir = createTempDirectory("sysl-publish-")

      Publish.text(s"$dir/slot", "a much longer first version of the entry\n")
      Publish.text(s"$dir/slot", "short\n") shouldBe Right(())
      readFile(s"$dir/slot") shouldBe "short\n"
      listFiles(dir).length shouldBe 1
    }

    "a rename keeps the executable bit the linker gave the pending file" in {
      val dir     = createTempDirectory("sysl-publish-")
      val pending = Publish.pending(s"$dir/app")

      writeFile(pending, "#!/bin/sh\necho kept\n")
      exec(Seq("chmod", "+x", pending)).exitCode shouldBe 0
      Publish.file(pending, s"$dir/app") shouldBe Right(())
      isExecutable(s"$dir/app") shouldBe true
      exists(pending) shouldBe false
    }

    "a publish that cannot land leaves no pending file behind" in {
      val dir     = createTempDirectory("sysl-publish-")
      val pending = Publish.pending(s"$dir/app")

      writeFile(pending, "x")
      // A directory with something in it cannot be replaced by a file, so the rename refuses.
      createDirectories(s"$dir/app/occupied")
      Publish.file(pending, s"$dir/app").isLeft shouldBe true
      exists(pending) shouldBe false
      listFiles(dir).map(Project.basename).toList shouldBe List("app")
    }

    "a text that cannot be written leaves nothing either" in {
      val dir = createTempDirectory("sysl-publish-")

      Publish.text(s"$dir/no/such/directory/slot", "x").isLeft shouldBe true
      listFiles(dir).length shouldBe 0
    }

    "a reader racing a writer sees one whole version or the other, never a prefix" in {
      val target  = s"${createTempDirectory("sysl-publish-")}/slot.tests"
      val short   = "s\n" * 10
      val long    = "l\n" * 200000
      val seen    = new java.util.concurrent.ConcurrentLinkedQueue[String]
      val writing = new java.util.concurrent.atomic.AtomicBoolean(true)

      Publish.text(target, short)

      val writer = new Thread(() =>
        for i <- 1 to 40 do Publish.text(target, if i % 2 == 0 then short else long)
        writing.set(false))
      val reader = new Thread(() =>
        while writing.get do
          val text = readFile(target)
          if text != short && text != long then seen.add(s"${text.length} bytes"))

      writer.start(); reader.start()
      writer.join(); reader.join()
      seen.toArray.toList shouldBe Nil
    }

    // `RunCache`'s redirect alone: this test reaches no `Fetch` call, so there is nothing for
    // `Fetch.usingCache` to move here.
    val cache = createTempDirectory("sysl-runcache-")

    "a kept test build puts the binary in its slot and the list beside it" in RunCache.usingCache(cache) {
      val key     = "k"
      val slot    = RunCache.reserve(key).get
      val linked  = Publish.pending(slot)
      val suite   = List(TTest("m$t", "a test", false, None, "m.sysl", 1))

      writeFile(linked, "binary")
      RunCache.keep(key, linked, slot, Some(suite)) shouldBe Right(())
      readFile(slot) shouldBe "binary"
      RunCache.tests(key).map(p => RunCache.decode(readFile(p))) shouldBe Some(Some(suite))
      listFiles(s"$cache/sysl/run").map(Project.basename).toList.sorted shouldBe List("k", "k.tests")
    }
  }

}
