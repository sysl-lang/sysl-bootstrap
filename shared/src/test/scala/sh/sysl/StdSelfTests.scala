package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The standard library's **own** `@test` functions, run as part of this suite
 * (`reference/attributes.md § @test — a function with a caller nothing else has`).
 *
 * `library/` is packed with tests written in sysl, and they are the right place for what is true of the
 * *library* — that a `Buf` grows geometrically, that `truncate` keeps its storage. This is the one
 * test that makes them part of the gate: without it they would be a thing somebody could run rather
 * than a thing that runs, and a library test that broke would break nothing.
 *
 * **What stays in Scala is what a sysl test cannot honestly assert**, and the line is not a matter of
 * taste. A `@test` runs code the compiler under test produced and asserts with `assert_eq`, which
 * that same compiler produced — so a miscompiled `==` makes the assertion pass. Everything asserting
 * a *language* claim therefore stays where the expectation is written in another language and
 * compared by another runtime: the whole of `err*` (a program is refused, with this diagnostic),
 * the whole of `ir*` (this is the IR), and every run-tier test whose subject is a construct rather
 * than a library function.
 *
 * The gain is not only tidiness. A run-tier Scala test compiles, links and executes a program of its
 * own; these compile the library **once** and then cost a process each, which is why sixteen of them
 * run in the time one `RunSupport.run` takes to reach `clang`.
 */
class StdSelfTests extends AnyFreeSpec with Matchers {

  /** Below this, assume something has gone wrong rather than that the library got quieter.
   *
   * `TestRunner` treats a tree with no tests as a success and says so on stderr — correctly, since a
   * program is allowed to have none. That makes "the library's tests all passed" true of a `library/`
   * whose test files had been deleted, which is exactly the silent-green this test exists to
   * prevent. A floor is the cheap guard: it needs no maintenance as tests are added, and it fails
   * loudly if a whole file stops being collected.
   *
   * **Raise it when a batch adds a module's worth of tests**, or it stops doing its job: a floor far
   * below the real count still passes with a whole file missing, which is the one thing it is for.
   * Raised from 60 to 120 when `sysl.slices`, `sysl.encoding` and `sysl.rand` took the library from
   * 89 collected tests to 136, and to 155 when the UTF-8 encoder, `Buf.insert` and the sub-second
   * durations took it to 163, and to 165 when the duration unit properties and the arithmetic
   * nothing had been exercising took it to 169, and to 195 when the line editor and the two
   * in-memory stream types took it to 201, and to 220 when `sysl.posix.time` took it to 227.
   *
   * That raise closed a drift as much as it made room: the tree had reached 222 while the floor
   * still read 195, so a twenty-test file could have stopped being collected with nothing said. The
   * gap this guard wants is the handful the entries above all left, not the two dozen it had grown.
   * Raised again to 224 when `sysl.fs` got a `tests.sysl` and took the tree to 231, and to 234
   * when `sysl.posix.tty` got one and took it to 234, and to 236 when `sysl.fs` gained the two
   * `size` tests — those raises leave no slack on purpose, because raising to exactly the new count
   * is what proves a new file or test is collected at all rather than silently skipped. Raised to
   * **317** when `sysl.container` arrived: five modules with a `tests.sysl` each — `map`, `set`,
   * `deque`, `heap` and `list` — plus the line editor's history cap, which is the first test of a
   * bound that module always had and nothing reached.
   *
   * Raised to **330** when the zone surface arrived: `sysl.time`'s `resolve` — seven tests against a
   * synthetic zone, needing no host — and `sysl.posix.time`'s four against whatever zone the machine
   * running them is set to. Exactly the new count again, for the reason above.
   *
   * Raised to **343** when the zone database arrived: `sysl.time.tzif` decodes a TZif file and gets
   * seven, `sysl.env` four, and `sysl.posix.time` two more for reading a real zone off the host.
   * `tzif`'s carry their zone as **bytes**, so they assert the format rather than the machine — the
   * only tests in the library that would pass on a target with no filesystem at all.
   *
   * **`sysl.fs`'s are the first tests that depend on the library's own C**, so this floor now guards
   * a second thing: a build that stopped compiling or stopped linking the shim under
   * `library/sysl/fs/__<os>__` fails outright rather than quietly collecting fewer tests.
   *
   * Raised to **433** when `sysl.math.matrix` arrived with 43 of its own. That raise also closed a
   * drift of the kind this comment has recorded once before and which had grown larger than it: the
   * tree was collecting 390 while the floor still read 343, so a whole module's worth could have
   * stopped being found with nothing said. The number below is the tree's exact count again, which
   * is the only setting that proves a new file is collected rather than silently skipped.
   *
   * Raised to **439** when `sysl.text`'s parse family gained its `[]const u8` forms — six tests,
   * which is what a facility rather than a module costs.
   *
   * Raised to **457** when `sysl.process` arrived with 18 of its own. Like `sysl.fs`'s, they depend
   * on the library's own C — the shim under `library/sysl/process/__posix__` is what decodes how a
   * child ended — so they guard the same second thing that comment names. They also guard something
   * no other test in the tree does: that `fork` and `execvp` reach a real program and come back,
   * which cannot be asserted without starting one.
   *
   * Raised to **482** when `Chars` stopped trusting `char_width` over bytes nobody validated -- nine
   * tests for the walk, the count, `peek`, `char_indices`' offsets, and the agreement between all of
   * them and `from_utf8_lossy`, which is the only thing that says three readers of one table read it
   * the same way.
   *
   * **Nine of those sixteen were the drift again, and it is the third time this comment has recorded
   * it.** The tree was collecting 473 while the floor still read 457, so a module's worth could have
   * stopped being found with nothing said -- which is the exact failure the number exists to catch,
   * and it cannot catch it from below. **Read the count rather than adding to the last one**: set
   * the floor absurdly high for one run and the failure names it (`482 was not greater than or equal
   * to 99999`), which is one run and settles it. Adding your own tests to a number nobody measured
   * carries the drift forward, which is how it got here twice before.
   *
   * Raised to **487** when `Vector[T]` and `Matrix[T]` stopped gathering their renderings: two of
   * those are the specifier split and the agreement between a matrix and its own rows, and **three
   * were drift again** -- the tree was at 485 against a floor of 482, from `Complex[F]`'s rendering
   * tests the day before. That is the fourth time, and it was found by the measurement this comment
   * prescribes rather than by adding two to the number above it.
   *
   * **LOWERED to 442 when `sysl.math.matrix` left the library**, which is the first time this number
   * has gone down and the one direction the guard cannot check for itself: a floor that is too high
   * fails loudly and a floor left too high after a removal fails *every* run, so the temptation is to
   * drop it far enough to be safe and stop thinking. That is the drift this comment has recorded four
   * times, arrived at from the other side.
   *
   * So it is measured, not subtracted: 45 tests left with the module, `grep -rh "^@test(" library
   * --include="*.sysl" | wc -l` reads **442**, and the gap of two is the same handful the raises
   * above kept. The module is `sh.sysl.linalg` now -- a leaf nothing else in `library/` imported, and
   * the only domain in a library otherwise made of the platform and of what all code touches.
   *
   * Raised to **448**, and with no slack this time, when `sysl.slices` gained `align_up` and
   * `is_aligned` with six tests: the measurement above reads 448, and taking the number exactly is
   * what proves the six are collected rather than merely that nothing vanished. The gap the entry
   * above kept was the residue of a removal, not a target to hold.
   *
   * Raised to **457**, again with no slack, when `Buf[T]` and `[]T` became `Eq` and a `Buf` became
   * `Display`: nine tests, four for the slice and the array in the root module's own file and five
   * for the buffer. The measurement above reads 457 and the runner agrees, which is what says the
   * nine are collected rather than that nothing vanished.
   *
   * Raised to **467** when `sysl.io` gained `read_all`, `read_all_text` and `read_exact` — the
   * whole-stream reads the surface was missing — with eight tests in the module's **first** test
   * file of its own. Every one of them is really about a reader that answers *short*, which is what
   * a pipe with one write in it, a socket and a terminal all do, and is the case a hand-rolled loop
   * gets wrong; `bytes_reader_at_most` is the fixture that poses it.
   *
   * **Two of the ten were drift again, and that is the fifth time.** The measurement read 467 with
   * eight added, so the tree was at 459 against a floor of 457 — from the two the `Buf`/`Eq` entry
   * above says it took exactly. Taken exactly again here.
   *
   * Raised to **479** when `Option` and `Result` gained the transforming combinators — `map`,
   * `and_then`, `or_else`, `unwrap_or_else` on both, `filter` and `ok_or` on the option, `map_err`,
   * `ok` and `err` on the result. Twelve tests, each asserting **both** variants, because a
   * combinator that is right about the present case and wrong about the absent one is exactly the
   * shape that survives a casual test. Measured, no slack.
   *
   * Raised to **486** when `sysl.posix.threads` gained `Channel[T]` and its first `tests.sysl` —
   * seven, six of them single-threaded facts about the ring and the closing rule, and one that
   * really does put twenty values through a ring of two on another thread. Measured with
   * `grep -rh "^@test(" library --include="*.sysl" | wc -l`, which counts the parenthesis so that
   * the file-level `@tests` above each of them is not counted as one.
   *
   * Raised to **513** when `sysl.path` arrived with 23 of its own and `sysl.container`'s map and set
   * gained 4 for the table that is not made until the first insert. `sysl.path` is the first module
   * in the library whose tests could run on a target with **no filesystem at all** — every one of
   * them is a claim about a string — which is the property that made it a module rather than more of
   * `sysl.fs`. Measured, no slack.
   *
   * Raised to **544** when `sysl.fs` gained the working set it was missing: one reading of an entry
   * (`metadata`, `link_metadata`, `set_permissions`), symbolic and hard links, `canonicalize`,
   * `make_dir_all`/`remove_dir_all`, `copy_file`, `truncate` and a temporary directory. Thirty-one
   * tests, and they guard the same second thing the `sysl.fs` and `sysl.process` entries above name,
   * for a **third** shim: `meta.c` is what knows where `st_size` sits, so a wrong offset shows up
   * here as a size that is not ten rather than as a build that failed. Measured, no slack.
   *
   * Raised to **545** for one more in `sysl.path`: resolving an import against the file that named
   * it, which is the shape a module loader actually writes and which arrived as four assertions from
   * an outside consumer rather than from taste.
   *
   * Raised to **549** for `sysl.time.checked_date`, which refuses a date the calendar does not have
   * where `date_at` answers with a day count. Four cases, and two of them assert `date_at`'s
   * *unchecked* answer, since what the pair is for is that the walk keeps its arithmetic. Measured,
   * no slack.
   *
   * Raised to **554** for `display_real_shortest`, a `real` at the shortest precision that reads
   * back equal. Five cases, and they sit in `sysl.text`'s file rather than the root module's for two
   * reasons: the property is a **round trip** and the other half of it is `parse_real`, and the root
   * module may not import a submodule at all, tests included -- `library/sysl` declares `sysl`, so
   * `import sysl.buf.byte_sink` in a `@tests` file beside it is refused as a cycle. Measured, no
   * slack.
   *
   * Raised to **561** for the platform constants and the four directory conventions: four cases in
   * the root module's file asserting that `os()` and `cpu()` answer what a `#if` on the same machine
   * gates on, and three in `sysl.fs`'s asserting what can be said about a directory whose path is the
   * running machine's rather than this file's. The compiler-side half is `PlatformRegistryTests`,
   * which is derived from the registry and checks the machines a program on one machine cannot see.
   * Measured, no slack.
   *
   * Raised to **566** for `capture`'s `stderr`, which collects the child's other stream into
   * `Output.err`. Five cases, and every one of them asserts something about `text` as well: a
   * capture that quietly merged the two streams would satisfy any test that only ever read `err`.
   * Measured, no slack.
   *
   * Raised to **582** when `sysl.posix.net` arrived with 16 of its own. Like `sysl.fs`'s and
   * `sysl.process`'s they depend on the library's own C -- the shim under
   * `library/sysl/posix/net/__posix__` owns every layout and every constant -- and they guard
   * something nothing else in the tree does: that a socket is made, bound, listened on, connected
   * to, accepted and talked through, in one thread, over the loopback. A `connect` to a listening
   * socket succeeds as soon as the kernel queues it, which is what makes that single-threaded and so
   * makes it something a suite can contain.
   *
   * Raised to **598** when `sysl.fs` gained a directory walk: eight for `walk` itself -- the order
   * both ways, a link reported and not followed, pruning, and a walk of a plain file -- and six for
   * `copy_dir_all`, which is the walk's first caller outside the module that declares it. **Two of
   * the sixteen were drift**, measured the way the paragraph above prescribes rather than added to
   * the number below it, which is the fifth time that has paid for itself.
   *
   * Raised to **603** when `sysl.slices` gained `copy` and `copy_exact` (card `0368`): five cases,
   * covering every length relation and both overlap directions at each of the two declarations, plus
   * `copy_exact`'s three answers. **No drift this time** -- the measurement read 598 before the
   * cases were written, which is the first run since this comment began to find the number already
   * right, and is what the previous entry's discipline was for.
   *
   * **What these cases cannot say is which declaration answered**, since the byte one and the
   * generic one are specified to behave identically and do. That claim is `SliceCopyTests`, in
   * Scala, over the IR -- the split this file's own docstring prescribes, arrived at from the side
   * it does not name: not a language claim a sysl test would compile into its own assertion, but a
   * claim about *which code was emitted*, which no program can observe about itself.
   *
   * Raised to **608** when `sysl.posix.rand` gained `entropy_from_os` (card `0373`): four cases for
   * it and one for `seed_from_os`, which had been covered by **nothing** — that module had no test
   * file at all until this needed one, which is the sort of hole a floor cannot see and only a
   * reader can.
   *
   * Raised to **616** when `sysl.crypto` gained SHA-1 (card `0363`): seven cases — the published
   * FIPS 180-4 messages, the padding boundaries, a million bytes in thousand-byte pieces, RFC 2202's
   * keyed-hash vectors, RFC 6455 §1.3's worked WebSocket handshake, the streaming and hasher-copying
   * claims, and what the hasher refuses. **One of the seven was drift**, measured before the cases
   * were written rather than added to the number above.
   *
   * The handshake case is the one worth naming, because it is the only reason SHA-1 is in the
   * standard module at all: RFC 6455 publishes a key and the accept value a server must answer with,
   * and a browser will not open a socket against any other answer. Every other digest in that module
   * is there to be chosen; this one is there because a wire protocol names it.
   *
   * Raised to **649** when `sysl.container.ring` arrived: fourteen cases for the bounded ring the
   * `guide/ring` program was retired into, which is the module `Channel` now keeps its values in
   * rather than in four fields of its own.
   *
   * **Nineteen of the thirty-three were drift, and that is the sixth time this comment has recorded
   * it** — the tree was at 635 against a floor of 616 before a line of the ring was written, which is
   * most of a module's worth going unnoticed. Measured with the command this comment prescribes
   * rather than added to the number above it.
   *
   * Raised to **675** when `sysl.unicode` arrived with 21 of its own and `sysl.text`'s one case
   * about ASCII case mapping became three: two about Unicode case mapping, and one contrasting it
   * with folding, which is the file that can name both. Like `sysl.fs`'s and
   * `sysl.posix.net`'s they depend on the library's own C, and on far more of it than any of those:
   * the whole Unicode Character Database is vendored beside the module, so a build that stopped
   * compiling `utf8proc.c` or stopped linking it fails here rather than quietly collecting fewer
   * tests. **The drift was three**, measured before a case was written -- 652 against a floor of
   * 649 -- and taken exactly, so what the number now proves is that all twenty-three new cases are
   * collected.
   *
   * Raised to **765** for a batch of five modules: `sysl.log` (14), `sysl.math.bigint` (20),
   * `sysl.math.decimal` (18), `sysl.encoding`'s UUIDs (11), `sysl.path`'s globs (16), `sysl.fs`'s
   * pattern filter (4) and `sysl.text`'s Unicode trims, fold comparisons and cluster widths (12).
   * The runner collected **770** with the floor at 675, so the whole of the difference is these
   * ninety-five and the drift was **zero** -- which is what makes taking a five-case margin here a
   * margin rather than a guess.
   *
   * Raised to **787** for the start offsets and the sub-linear search: nine in `sysl.text` — the
   * `_from` family at every edge, the overlapping counts on both sides of the threshold where the
   * naive scan hands over to Horspool, and a differential run of two thousand random haystacks
   * against a scan written out longhand in the test file — and two in `sysl.slices`. Measured with
   * the `grep` above before a case was written: it read **776** against a floor of 765, so the drift
   * was the eleven the margin above was, and the number here is the tree's exact count again.
   *
   * Raised to **824** for `sysl.math.bigint`'s bit operations, conversions, floored division,
   * number theory and machine-sized operands (32 cases) and `sysl.math`'s checked and overflowing
   * arithmetic (5). The runner collected **791** before a case was written, against a floor of 787,
   * so the drift was four and the margin here is the same four -- 828 collected, 824 required.
   */
  private val floor = 824

  /** The library, compiled as a **test build of itself**.
   *
   * `building` is what says the tree in front of the compiler *is* the standard module rather than a
   * program compiled against one — the same word `build-lib --std` uses, and the same set. Without
   * it every declaration in `library/` collides with the copy the compiler supplies, which is what
   * `sysl test library` did before `--std` existed.
   */
  private def libraryTests(): List[TestRunner.Outcome] = {
    assume(Toolchain.clangAvailable, "clang not available")

    val root = StdRoot.root

    assume(root.isDefined, "the library is not reachable from the working directory")

    val target = Target.default

    val Stdlib.Resolved(std, precompiled, _) =
      Stdlib.resolve(Stdlib.Choice.FromSource, target) match {
        case Right(c)  => c
        case Left(err) => fail(s"no standard module to compile against:\n$err")
      }

    val (built, tests) =
      Compiler.compileTests(Project.collect(root.get, Some(target.os)), Nil, target, precompiled, Some(std),
                            LibraryArtifact.std) match {
        case Right(result) => result
        case Left(err)     => fail(s"the standard library did not compile as a test build:\n$err")
      }

    // **The library's own C** (`reference/ffi.md § A library may carry C`), which it may carry
    // exactly as any other tree may — the shim under `library/sysl/fs/__<os>__` is what answers
    // `entries`, and `sysl.fs`'s own `@test`s call it. The driver does this for a real compilation
    // off `Stdlib.Resolved`'s answer about whether an artifact supplied the standard module; here
    // the tree is being compiled from source by construction, so it is unconditional.
    val native =
      NativeSources.build(NativeSources.of(List(root.get), target.os), target) match {
        case Left(err)    => fail(s"the standard library's C did not compile:\n$err")
        case Right(built) => built
      }

    val exe = createTempFile("sysl-std-test-", "")

    try
      Toolchain.build(built.ir, exe, target, Nil, links = built.links, objects = native.objects) match {
        case Left(err) => fail(s"the standard library's test build did not link:\n$err")
        case Right(_)  => TestRunner.execute(exe, tests, TestRunner.Options())
      }
    finally
      native.scratch.foreach(Project.discard)

      try deleteFile(exe)
      catch case _: Exception => ()
  }

  /** Run once. The compile is the slow half and both assertions below want the same outcomes, so
   * paying for it twice would double the cost of this file for nothing.
   */
  private lazy val outcomes: List[TestRunner.Outcome] = libraryTests()

  "the standard library's own tests" - {

    "all pass" in {
      outcomes.filterNot(_.passed) match {
        case Nil => succeed
        case bad =>
          fail(bad.map(o => s"${o.test.display}: ${o.detail.getOrElse("")}\n${o.output}").mkString("\n"))
      }
    }

    // Separate from the assertion above, because they fail for different reasons and a reader wants
    // to know which. "A library test broke" is a defect in the library; "the tests stopped being
    // found" is a defect in the collection, and it would otherwise read as everything passing.
    s"number at least $floor, so an empty run cannot read as a green one" in {
      outcomes.length should be >= floor
    }
  }
}
