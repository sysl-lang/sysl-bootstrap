package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** The capability annotations of `reference/modules.md § Capabilities are a module property` and
 * `reference/modules.md § Capabilities are a module property`: `@no_alloc` narrowing a module below
 * what its target offers, and `@requires(alloc)` declaring what it cannot be built without.
 *
 * It is a property of the **module** written on each of its **files**, which is where most of the
 * rules here come from: the files have to agree, it has a place in the file, and the module's own set
 * is what every construction in it is measured against.
 *
 * What `@no_alloc` refuses is *making* heap storage, never holding it — a distinction the suite
 * checks from both sides, since a capability that refused a reference it was handed would make the
 * allocator-free subset useless for the kernel code it exists for.
 */
class CapabilityClauseTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  // The whole reason the clauses are attributes rather than grammar. An attribute's name arrives as
  // an ordinary identifier, so nothing here is reserved — and it is the code that *provides* a
  // capability that wants the words most: the slab allocator that reported this change calls its
  // own function `alloc`, and could not while the clause was the two words `no alloc`.
  "the words the clauses used to spend are ordinary names" - {

    "a function may be called 'alloc', which is what an allocator calls its own" in {
      run("alloc(n: int) -> int = n + 1\n\nprint(alloc(6))\n") shouldBe "7\n"
    }

    "'no', 'requires' and 'link' too" in {
      run("no(x: int) -> int = x\nrequires(x: int) -> int = x * 2\nlink(x: int) -> int = x + 3\n\n" +
        "print(no(1), requires(2), link(4))\n") shouldBe "1 4 7\n"
    }

    "a field may be called 'link', which is what the slab guide threads its free list through" in {
      run("struct Block\n    link: *Block\n    alloc: int\n\nvar b = Block(null, 9)\nprint(b.alloc)\n") shouldBe "9\n"
    }

    "and a local, in a module that gave the capability up" in {
      run("@no_alloc\n\nf() -> int\n    val alloc = 3\n    val requires = 4\n\n    alloc + requires\n\nprint(f())\n") shouldBe "7\n"
    }
  }

  "the clause is read in the file's header" - {

    "'no alloc' beside a module header" in {
      irOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf(p: *int) -> int = *p\n",
        "main.sysl" -> "var n = 7\nprint(thing.f(&n))") should include("define")
    }

    "'requires heap', the other direction" in {
      runOf("thing/a.sysl" -> "module thing\n@requires(heap)\n\nf() -> &int = 1\n",
        "main.sysl" -> "print(*thing.f())") shouldBe "1\n"
    }

    "a file with no module header may narrow, since the root module is a module" in {
      run("@no_alloc\n\nf(p: *int) -> int = *p\n\nvar n = 4\nprint(f(&n))\n") shouldBe "4\n"
    }

    "several clauses on their own lines" in {
      irOf("thing/a.sysl" -> "module thing\n@no_alloc\n@requires(os)\n\nf() -> int = 1\n",
        "main.sysl" -> "print(thing.f())") should include("define")
    }

    "an attribute on the header's own line is refused, since each takes a line" in {
      err("module m @no_alloc\n\nf() -> int = 1\n") should include("belongs in the file's header")
    }

    "and a clause written below the statements says where it belongs" in {
      val e = err("f() -> int = 1\n@no_alloc\n")

      e should include("belongs in the file's header")
      e should include("directly after 'module'")
      e shouldNot include("newline expected")
    }

    /** The advice above is good for the case it was written for and false for a clause that is
     * already where it belongs and simply will not parse.
     *
     * **A repetition ends when its element fails**, so a `@requires(` that never closed used to end
     * the header, hand the line to the statement grammar, and be told by the message above to move
     * to the header — on line 1, which *is* the header, with nowhere to move it to. What the reader
     * is owed is the complaint `headerAttr` had already made and that was being thrown away.
     */
    "a malformed one is refused where it is malformed, rather than told to move" in {
      val e = err("@requires(\nprint(1)\n")

      e should include("')' expected")
      e shouldNot include("belongs in the file's header")
    }

    "the same under a module header, which is the other way a file opens" in {
      val e = err("module m\n@requires(\nf() -> int = 1\n")

      e should include("')' expected")
      e shouldNot include("belongs in the file's header")
    }

    // The other attribute with an argument list, so the commit is pinned at the vocabulary rather
    // than at one word of it.
    "and a '@link' whose parenthesis never closed" in {
      val e = err("@link(\"z\"\n\nf() -> int = 1\n")

      e should include("')' expected")
      e shouldNot include("belongs in the file's header")
    }
  }

  "a clause that names nothing, or claims something unchecked, is refused" - {

    "a name that is not a capability" in {
      val e = err("@requires(sockets)\n\nf() -> int = 1\n")

      e should include("no capability is called 'sockets'")
      e should include("'heap', 'os', 'posix'")
    }

    // The narrowing form carries the name in the attribute's own word, so an unknown one is a
    // spelling the analyzer has to reject rather than a parse failure — which is what keeps one
    // message about an unknown capability instead of two that differ by which form was written.
    "and the same, spelled the narrowing way" in {
      err("@no_sockets\n\nf() -> int = 1\n") should include("no capability is called 'sockets'")
    }

    "the same capability declared twice" in {
      err("@no_alloc\n@no_alloc\n\nf() -> int = 1\n") should include("'alloc' is declared twice")
    }

    // Written in the two vocabularies, because that is what the pair looks like: the module promises
    // it does not allocate and then says it cannot be built without a heap. Grouping the clauses by
    // what was *typed* would have let this through, since the words differ.
    "and a capability both given up and required" in {
      val e = err("@no_alloc\n@requires(heap)\n\nf() -> int = 1\n")

      e should include("'heap' is both given up and required here")
      e should include("'@no_alloc' and 'requires heap' are the two directions of one capability")
    }

    /* The refusal that used to sit here refuses nothing now, and that is the state it was built to
     * reach: a narrowing is refused while it would enforce nothing, and allowed once it would. `os`
     * and `posix` left the list the day a module requiring one existed — so all three are narrowings
     * today. What each of them then *means* is checked against the module graph, in `ThreadTests`
     * and `FsTests`.
     *
     * There were four of them until `sysl.thread` became `sysl.posix.threads`. `threads` was
     * removed rather than renamed, because a module built on pthreads requires `posix` and the
     * compiler tracks nothing about schedulers; `ThreadTests` is where the refusal of the old
     * spelling is pinned. */
    "while every capability there is may now be given up, since each of them gates something" in {
      run("@no_alloc\n@no_os\n@no_posix\n\nprint(1 + 1)\n") shouldBe "2\n"
    }
  }

  "the files of one module state the same clause" - {

    "a file that dropped it is reported against one that kept it" in {
      val e = errOf(
        "a.sysl" -> "module thing\n@no_alloc\n\nf() -> int = 1\n",
        "b.sysl" -> "module thing\n\ng() -> int = 2\n",
        "main.sysl" -> "print(thing.f())",
      )

      e should include("declare different capabilities")
      e should include("'@no_alloc'")
      e should include("none")
    }

    "and two files stating it are not" in {
      runOf(
        "a.sysl" -> "module thing\n@no_alloc\n\nf() -> int = 1\n",
        "b.sysl" -> "module thing\n@no_alloc\n\ng() -> int = 2\n",
        "main.sysl" -> "print(thing.f() + thing.g())",
      ) shouldBe "3\n"
    }
  }

  /** The one exception to the rule above, and the reason it is not a hole in it: a `@tests` file is
   * dropped by every build but `sysl test`, so the module's clause — which is a promise about what a
   * program linking this module may do — was never a promise about that file.
   *
   * Held to one set, the clause was unavailable to precisely the modules that would want it. The
   * standard library measured it: the one module carrying `@no_alloc` was the one module with no
   * `tests.sysl`, because testing an allocation-free primitive means rendering what it produced and
   * rendering allocates. `sysl.crypto` is what took the clause once this existed.
   */
  "a '@tests' file states what the module's tests need" - {

    "it need not repeat the module's clause, which is what used to be refused" in {
      runOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> int = 1\n",
        "thing/tests.sysl" -> "module thing\n@tests\n\nhelper() -> int = 2\n",
        "main.sysl" -> "print(thing.f())",
      ) shouldBe "1\n"
    }

    "and it may take back what the module gave up, since what it declares does not ship" in {
      runOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> int = 1\n",
        "thing/tests.sysl" -> "module thing\n@tests\n@requires(heap)\n\nboxed() -> &int = 1\n",
        "main.sysl" -> "print(thing.f())",
      ) shouldBe "1\n"
    }

    "while the same construction in a file that ships is refused as it always was" in {
      errOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> &int = 1\n",
        "thing/tests.sysl" -> "module thing\n@tests\n@requires(heap)\n\nboxed() -> &int = 1\n",
        "main.sysl" -> "print(1)",
      ) should include("a reference needs an allocator, and this module declared '@no_alloc'")
    }

    // The silent case is the common one and it has to keep meaning what it meant: every `@tests`
    // file in the standard library repeats its module's clause today, and none of them changes
    // meaning by dropping the line. A file states its own only where it says something.
    "a test file that says nothing is held to its module's clause" in {
      errOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> int = 1\n",
        "thing/tests.sysl" -> "module thing\n@tests\n\nboxed() -> &int = 1\n",
        "main.sysl" -> "print(thing.f())",
      ) should include("a reference needs an allocator, and this module declared '@no_alloc'")
    }

    "and one that narrows on its own is held to that, where its module narrowed nothing" in {
      errOf(
        "thing/a.sysl" -> "module thing\n\nf() -> &int = 1\n",
        "thing/tests.sysl" -> "module thing\n@tests\n@no_alloc\n\nboxed() -> &int = 1\n",
        "main.sysl" -> "print(*thing.f())",
      ) should include("a reference needs an allocator, and this module's '@tests' file declared '@no_alloc'")
    }

    // The module's own set is read off a file that ships. It used to be read off whichever file the
    // group happened to hold first, which is source order — so a module whose test file sorted ahead
    // of its code would have recorded the scaffolding's clause as the module's.
    "the module's clause is read off a file that ships, whatever order the files arrive in" in {
      errOf(
        "thing/a.sysl" -> "module thing\n@tests\n@requires(heap)\n\nboxed() -> &int = 1\n",
        "thing/b.sysl" -> "module thing\n@no_alloc\n\nf() -> &int = 1\n",
        "main.sysl" -> "print(1)",
      ) should include("a reference needs an allocator, and this module declared '@no_alloc'")
    }

    // The scaffolding is held to agreeing with itself for the reason the shipping files are held to
    // agreeing with each other: a module has one answer to what its tests may do.
    "two '@tests' files of one module state one thing between them" in {
      val e = errOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> int = 1\n",
        "thing/t1.sysl" -> "module thing\n@tests\n@requires(heap)\n\nhelper() -> int = 2\n",
        "thing/t2.sysl" -> "module thing\n@tests\n\nother() -> int = 3\n",
        "main.sysl" -> "print(thing.f())",
      )

      e should include("declare different capabilities")
      e should include("its '@tests' files state one thing between them")
    }

    // A test is scaffolding wherever it is written, and `reference/attributes.md § @tests — a file of scaffolding` asks for one written beside
    // what it tests. Answering to the module while the `@tests` file beside it answered to
    // something else would put a seam through the middle of one module's tests.
    "a '@test' function in an ordinary file answers to the tests' clause too" in {
      runOf(
        "thing/a.sysl" -> ("module thing\n@no_alloc\n\nf() -> int = 1\n\n@test\n" +
          "beside_what_it_tests() =\n    val r: &int = 1\n    assert(*r == 1)\n"),
        "thing/tests.sysl" -> "module thing\n@tests\n@requires(heap)\n\nhelper() -> int = 2\n",
        "main.sysl" -> "print(thing.f())",
      ) shouldBe "1\n"
    }

    "and is refused with the module where no '@tests' file took the allocator back" in {
      errOf(
        "thing/a.sysl" -> ("module thing\n@no_alloc\n\nf() -> int = 1\n\n@test\n" +
          "beside_what_it_tests() =\n    val r: &int = 1\n    assert(*r == 1)\n"),
        "main.sysl" -> "print(thing.f())",
      ) should include("a reference needs an allocator, and this module declared '@no_alloc'")
    }
  }

  /** Which of the standard library's modules an allocator-free program may actually reach.
   *
   * The clause's worth is that it holds you to the promise while you develop on a machine that has
   * a heap; a module that is allocation-free and does not say so is one that quietly acquires an
   * allocation on some future Tuesday. **Seven modules said nothing until the sweep**, and until a
   * `@tests` file could state its own capabilities they could not: testing an allocation-free
   * primitive means rendering what it produced, and rendering allocates.
   *
   * These are asserted from a program that gave the allocator up, which is the surface a reader
   * cares about — that the call compiles there, rather than that a header carries a word.
   */
  "an allocator-free program reaches the modules that declared themselves so" - {

    "'sysl.math', whose comparisons and integer arithmetic allocate nowhere" in {
      run("@no_alloc\n\nimport sysl.math.{max, min, gcd}\n\nprint(max(3, 7), min(3, 7), gcd(12, 18))\n") shouldBe
        "7 3 6\n"
    }

    "'sysl.slices', which works in storage its caller already owns" in {
      run("@no_alloc\n\nimport sysl.slices.{reverse, index_of}\n\n" +
        "var a: [3]int = [1, 2, 3]\nreverse(a[0..<3])\nprint(a[0], index_of(a[0..<3], 1))\n") shouldBe
        "3 Some(2)\n"
    }

    "'sysl.rand', whose generator is a value rather than a handle" in {
      run("@no_alloc\n\nimport sysl.rand.rng\n\nvar g = rng(42, 54)\nprint(g.next_u32() != 0)\n") shouldBe "true\n"
    }

    "and 'sysl.sync', which a lock-free structure on a board is built out of" in {
      run("@no_alloc\n\nimport sysl.sync.Atomic\n\nvar a = Atomic(7)\nprint(a.load())\n") shouldBe "7\n"
    }

    // Probed while sweeping the library, because the sweep's refusals made it look otherwise and the
    // guess would have shaped the whole result. Rendering *into the sink* allocates nothing — it is
    // building a string first and handing that over that does — so the constraint is on how a
    // `display` is written rather than on where it may live.
    "an 'impl Display' may live in an allocator-free module, since writing into the sink makes nothing" in {
      run("@no_alloc\n\nstruct Tag\n    n: int\n\nimpl Display for Tag\n" +
        "    display(self, out: *Writer, fmt: FormatSpec) = display_str(\"tag\", out, fmt)\n\nprint(Tag(1))\n") shouldBe
        "tag\n"
    }

    "including one whose text is chosen by a match over literals, which are static rather than made" in {
      run("@no_alloc\n\nenum Fail\n    NotSquare\n    Singular\n\nimpl Display for Fail\n" +
        "    display(self, out: *Writer, fmt: FormatSpec)\n" +
        "        var text = self match\n" +
        "            NotSquare -> \"not square\"\n" +
        "            Singular -> \"singular\"\n\n" +
        "        display_str(text, out, fmt)\n    end display\n\nprint(Fail.Singular)\n") shouldBe "singular\n"
    }

    /** **And they keep passing when the program links an allocating `Writer`, which is the point.**
     * `sysl.buf`'s `ByteSink` is a `Writer` too, so importing it puts an allocating implementation
     * into the method table a `*Writer` call goes through — and which implementation is behind a
     * parameter is the *caller's* choice, made in a module of their own.
     *
     * `reference/modules.md § A generic answers for what it wrote, not for what its caller chose`
     * states exactly this for a type parameter: an allocator-free module may call `s.put(msg)`
     * through a bound whose `impl` allocates. A trait object is the same borrowing one level along,
     * and the two now answer alike.
     *
     * Before they did not, and the cost was that the same `display` — unchanged, byte for byte — was
     * legal or refused according to what the *program* imported. No `Display` in `library/` could
     * carry the clause, because the library build compiles `sysl.buf` alongside everything else.
     */
    "and linking an allocating 'Writer' leaves it alone, since which one is behind a '*Writer' is the caller's" in {
      irOf(
        "mark/a.sysl" ->
          """module mark
            |@no_alloc
            |
            |struct Mark
            |    n: int
            |
            |impl Display for Mark
            |    display(self, out: *Writer, fmt: FormatSpec) = display_str("mark", out, fmt)
            |""".stripMargin,
        "main.sysl" ->
          """import mark.Mark
            |import sysl.buf.byte_sink
            |
            |var sink = byte_sink()
            |var out: *Writer = &sink
            |
            |Mark(3).display(out, FormatSpec(0, -1, false))
            |putbytes(sink.text())
            |""".stripMargin,
      ) should include("mark")
    }

    /** **The other half, and the one that makes the relaxation a narrowing rather than a hole: a
     * module is still answerable for the erasure it wrote itself.**
     *
     * `TErase` names the table, so a body that puts a value behind a trait object has said which
     * implementation it is reaching, in its own tree, and the walk follows that one. Only a trait
     * object that *arrived* is somebody else's choice.
     *
     * The allocating `Writer` has to live in a module without the clause for this to test what it
     * says it tests. Written inside `mark` its `write` would be refused where it stands, by the
     * direct walk, and the case would pass without the erasure being consulted at all — which is
     * also why `sysl.buf`'s own `byte_sink` cannot play this part: `ByteSink` holds a `&Buf[u8]`, so
     * making one allocates before any table is reached.
     */
    "but a module that erased the value itself is still answerable for what it reaches" in {
      errOf(
        "loud/a.sysl" ->
          """module loud
            |
            |struct Loud
            |    n: int
            |
            |impl Fallible for Loud
            |
            |impl Writer for Loud
            |    write(*self, bytes: []const u8)
            |        var kept: &int = 1
            |""".stripMargin,
        "mark/a.sysl" ->
          """module mark
            |@no_alloc
            |
            |import loud.Loud
            |
            |say()
            |    var sink = Loud(0)
            |    var out: *Writer = &sink
            |
            |    display_str("mark", out, FormatSpec(0, -1, false))
            |""".stripMargin,
        "main.sysl" -> "mark.say()\n",
      ) should include("which makes heap storage, and this module declared '@no_alloc'")
    }
  }

  "'no alloc' refuses every construction that makes heap storage" - {

    "a reference" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> &int = 1\n",
        "main.sysl" -> "print(*thing.f())") should
        include("a reference needs an allocator, and this module declared '@no_alloc'")
    }

    "a boxed trait object, which is a reference underneath" in {
      errOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |trait Shape
            |    sides(self) -> int
            |
            |struct Square
            |    side: int
            |
            |impl Shape for Square
            |    sides(self) -> int = 4
            |
            |f() -> &Shape = Square(1)
            |""".stripMargin,
        "main.sysl" -> "print(thing.f().sides())",
      ) should include("a reference needs an allocator")
    }

    "a slice with storage of its own" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> []int\n    var xs: []int = [1, 2, 3]\n    xs\n",
        "main.sysl" -> "print(thing.f()[0])") should
        include("a slice with storage of its own needs an allocator")
    }

    "a repeated slice, the other way one is made" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf(n: int) -> []int\n    var xs: []int = [0; n]\n    xs\n",
        "main.sysl" -> "print(thing.f(3)[0])") should
        include("a slice with storage of its own needs an allocator")
    }

    "two strings joined" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> string = \"a\" + \"b\"\n",
        "main.sysl" -> "print(thing.f())") should
        include("the string two strings join into needs an allocator")
    }

    "a value rendered as a string" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf(n: int) -> string = str(n)\n",
        "main.sysl" -> "print(thing.f(1))") should
        include("the string a value renders as needs an allocator")
    }

    "and an interpolation, which is the same thing spelled differently" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nf(n: int) -> string = s\"n=$n\"\n",
        "main.sysl" -> "print(thing.f(1))") should include("needs an allocator")
    }

  }

  "'no alloc' leaves the whole no-alloc subset alone" - {

    /** Printing a slice, which is the reason its `Display` writes elements straight through rather
     * than gathering them. Gathering would have been the shorter block, and it would have put an
     * allocation on the one path an embedded target actually takes — where the point of printing
     * here is that it needs none.
     *
     * **Only the unpadded form is reachable under the clause**, and not because of anything slices
     * do: a width can only be written in an interpolation, and an interpolation joins strings, which
     * is refused here on its own account. So the width path is pinned in `DisplayRunTests` instead.
     * That does not make the counting sink idle — it is what keeps the width working *without*
     * putting a buffer in the impl, which would have cost this test its subject.
     */
    "printing a slice" in {
      runOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |show(xs: []int) = print(xs)
            |""".stripMargin,
        "main.sysl" ->
          """var a: [3]int = [4, 5, 6]
            |thing.show(a[0..<3])
            |""".stripMargin,
      ) shouldBe "[4, 5, 6]\n"
    }

    "raw pointers, fixed arrays, and views of them" in {
      runOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |sum(xs: []int) -> int
            |    var total = 0
            |    for x in xs do total += x
            |    total
            |
            |first(p: *int) -> int = *p
            |""".stripMargin,
        "main.sysl" ->
          """var a: [3]int = [4, 5, 6]
            |print(thing.sum(a[0..<3]))
            |print(thing.first(&a[0]))
            |""".stripMargin,
      ) shouldBe "15\n4\n"
    }

    "holding, passing and releasing a reference made somewhere else" in {
      runOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |struct Held
            |    r: &int
            |
            |keep(r: &int) -> Held = Held(r)
            |
            |read(h: Held) -> int = *h.r
            |""".stripMargin,
        "main.sysl" ->
          """var r: &int = 9
            |var h = thing.keep(r)
            |print(thing.read(h))
            |""".stripMargin,
      ) shouldBe "9\n"
    }

    "and a string literal, which is static rather than made" in {
      runOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nname() -> string = \"kernel\"\n",
        "main.sysl" -> "print(thing.name())") shouldBe "kernel\n"
    }

    // **Rendering an integer, at every width there is** — which is `Display`'s own promise and was
    // not true of the widest values until the blanket `impl` replaced the routing that chose between
    // three renderers. Above 128 bits the compiler used to fall back through `str`, so a module
    // declaring it makes no storage could print a `u32` and not a `u256`: the values needing the
    // most care were the ones it could not have. Pinned at four widths spanning both signednesses
    // and all three of the old ranges.
    "an integer of any width, which is what the blanket buys a module with no allocator" in {
      for width <- List("u8", "i64", "u128", "i256") do
        // Called from `main`, so the instantiation is live: an uncalled generic emits nothing, and
        // the assertion would then be about a module the compiler had thrown away.
        irOf(
          "thing/a.sysl" ->
            s"module thing\n@no_alloc\n\nf(v: $width, out: *Writer) = v.display(out, FormatSpec(0, -1, false))\n",
          "main.sysl" -> s"thing.f(7$width, stdout())\n",
        ) should include(s"call void @bound.${Library.key("Integer")}.display.")
    }

    // `reference/modules.md § Capabilities are a module property` lists this among the no-alloc
    // subset by name: the allocator's own building blocks. Nothing follows a call out of the
    // language, so what a C function does with the storage it hands back is not this compiler's
    // question — which is what makes an allocator writable in a module that has none.
    "the malloc a module provides itself, which is an extern like any other" in {
      irOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |extern malloc(n: usize) -> *u8
            |extern free(p: *u8)
            |
            |take(n: usize) -> *u8 = malloc(n)
            |
            |give(p: *u8) = free(p)
            |""".stripMargin,
        "main.sysl" -> "var p = thing.take(8usize)\nthing.give(p)\n",
      ) should include("declare ptr @malloc")
    }

    // Also listed by name: a closure that does not outlive its scope is inlined and boxes nothing.
    "a closure that does not escape, which is inlined rather than boxed" in {
      runOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |twice(f: int -> int, n: int) -> int = f(f(n))
            |
            |four(n: int) -> int = twice(x -> x + 2, n)
            |""".stripMargin,
        "main.sysl" -> "print(thing.four(1))",
      ) shouldBe "5\n"
    }

    // The other half of "holding is not making": a slice whose storage is on the heap was made by
    // somebody with an allocator, and reading it needs none.
    "a heap-backed slice made elsewhere and read here" in {
      runOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nfirst(xs: []int) -> int = xs[0]\n",
        "main.sysl" -> "var xs: []int = [7, 8, 9]\nprint(thing.first(xs))",
      ) shouldBe "7\n"
    }

    // `Display` promises rendering costs no allocation so that a module without an allocator can
    // still log, and that promise used to stop at 64 bits: anything wider reached its renderer as
    // the digits `str` writes, which is a heap string, so a value this module could hold was one it
    // could not print. The renderers work the digits out against a frame-local buffer instead, and
    // the slice is safe to hand over because a `Writer` borrows what it is given rather than keeping
    // it.
    "rendering an integer wider than 64 bits into a sink" in {
      irOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |unsigned(v: u128, out: *Writer) = v.display(out, FormatSpec(0, -1, false))
            |
            |signed(v: i128, out: *Writer) = v.display(out, FormatSpec(0, -1, false))
            |""".stripMargin,
        "main.sysl" -> "print(1)",
      ) should include("define")
    }
  }

  "the rule reaches every place a module's code can be written" - {

    // A `val` counts nothing, so it cannot *be* a string — but its initializer is code like any
    // other, and joining two strings to measure the result allocates just the same.
    "a module-level val's initializer" in {
      errOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nval width: usize = (\"a\" + \"b\").bytes.len\n",
        "main.sysl" -> "print(thing.width)") should include("needs an allocator")
    }

    // …while the `val` a driver module actually wants is admitted, which matters because a driver is
    // the module most likely to declare this clause. A constant address is a constant tree
    // (`reference/modules.md § Where a program starts`): it runs nothing at all, so there is nothing here for an allocator to be reached by.
    "but a register block named at file scope runs nothing, so it is admitted" in {
      irOf(
        "thing/a.sysl" ->
          "module thing\n@no_alloc\n\nconst UART: usize = 0x1000\nval regs: *u32 = ptr_cast(UART)\n",
        "main.sysl" -> "print(usize(thing.regs))",
      ) should include("@thing$regs = private constant ptr inttoptr (i64 4096 to ptr)")
    }

    "a method of a trait implementation" in {
      errOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |trait Named
            |    name(self) -> string
            |
            |struct Box
            |    n: int
            |
            |impl Named for Box
            |    name(self) -> string = str(self.n)
            |""".stripMargin,
        "main.sysl" -> "print(thing.Box(1).name())",
      ) should include("needs an allocator")
    }

    // A generic body is checked once with its parameters standing in for themselves and again at
    // each instantiation, and the diagnostic is wanted exactly once — from the instantiation, since
    // that is the tree that would have been emitted.
    "and a generic, at the instantiation that would have allocated" in {
      val e = errOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nhold[T](v: T) -> &T = v\n",
        "main.sysl" -> "print(*thing.hold(5))",
      )

      e should include("a reference needs an allocator")
      e.linesIterator.count(_.contains("needs an allocator")) shouldBe 1
    }
  }

  "the promotion a module cannot make is reported rather than made silently" - {

    "a slice of a local array that outlives the frame" in {
      val e = errOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\nf() -> []int\n    var a: [3]int = [1, 2, 3]\n    a[0..<3]\n",
        "main.sysl" -> "print(thing.f()[0])",
      )

      e should include("would move to the heap")
      e should include("this module declared '@no_alloc'")
    }

    // Promotion is decided after the walk, so the exemption has to be carried onto the typed tree
    // rather than answered where the clause was read. A `@tests` file that took the allocator back
    // took it back for this too, or the one allocation no expression spells would be the one thing
    // its tests still could not do.
    "the same body in a '@tests' file whose module gave the allocator up is promoted" in {
      runOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\ng() -> int = 1\n",
        "thing/tests.sysl" -> ("module thing\n@tests\n@requires(heap)\n\n" +
          "f() -> []int\n    var a: [3]int = [1, 2, 3]\n    a[0..<3]\n"),
        "main.sysl" -> "print(thing.g())",
      ) shouldBe "1\n"
    }

    "while one whose '@tests' file said nothing is refused with its module" in {
      val e = errOf(
        "thing/a.sysl" -> "module thing\n@no_alloc\n\ng() -> int = 1\n",
        "thing/tests.sysl" -> ("module thing\n@tests\n\n" +
          "f() -> []int\n    var a: [3]int = [1, 2, 3]\n    a[0..<3]\n"),
        "main.sysl" -> "print(thing.g())",
      )

      e should include("would move to the heap")
      e should include("this module declared '@no_alloc'")
    }

    "and the same body in a module that kept its allocator is promoted with no diagnostic at all" in {
      runOf(
        "thing/a.sysl" -> "module thing\n\nf() -> []int\n    var a: [3]int = [1, 2, 3]\n    a[0..<3]\n",
        "main.sysl" -> "print(thing.f()[0])",
      ) shouldBe "1\n"
    }
  }

  "what the guarantee is worth is readable in the output" - {

    // The claim is about **calls**, not about the declaration. `malloc` and `free` are declared
    // together with the ARC helpers, which an allocator-free program still emits: it holds slices,
    // and retaining one it was handed is what those helpers are. Reaching the `free` inside them
    // takes storage that was made, and this program makes none.
    "no function of an allocator-free program calls the allocator" in {
      val out = ir(
        """@no_alloc
          |
          |sum(p: *int, n: usize) -> int
          |    var total = 0
          |    var i = 0usize
          |    while i < n do
          |        total += p[i]
          |        i += 1
          |    total
          |
          |var a: [3]int = [4, 5, 6]
          |print(sum(&a[0], 3))
          |""".stripMargin,
      )

      out shouldNot include("call ptr @malloc")
      out should include("declare i64 @sysl_stdout_write(ptr, i64)")
    }

    // A call out of the module is the other half of `reference/modules.md § Capabilities are a
    // module property`, and `line_text` is what it catches: the module's own text makes nothing,
    // and the library function it calls validates bytes into a fresh `string`. Without the
    // reachability half this compiles, and the output holds one real call to the allocator in a
    // module that declared it would make none.
    "a call into a function that allocates is refused too" in {
      val e = errOf(
        "thing/a.sysl" ->
          "module thing\n@no_alloc\n\nimport sysl.io.line_text\n\ntext(b: []const u8) -> string = line_text(b)\n",
        "main.sysl" -> "var bytes: [2]u8 = [104u8, 105u8]\nprint(thing.text(bytes[0..<2]))\n",
      )

      e should include("which makes heap storage")
      e should include("may only call what is allocator-free itself")
    }

    /** **Every** call site that reaches an allocator, not merely the first. A body with two of them
      * has two things wrong with it and two places to change, and reporting one at a time makes the
      * reader fix, recompile, and be told about the next — which is worse than what the *direct*
      * allocation walk gives for the same mistake one step nearer.
      *
      * The two statements below reach the same allocator by two different routes, so a walk that
      * stopped at the first reaching child would say one thing and leave the second line standing.
      */
    "and every call that reaches one is named, not just the first" in {
      val e = errOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |import sysl.io.line_text
            |
            |two(a: []const u8, b: []const u8) -> usize
            |    var x = line_text(a)
            |    var y = line_text(b)
            |    x.len + y.len
            |""".stripMargin,
        "main.sysl" -> ("var bytes: [2]u8 = [104u8, 105u8]\n" +
          "print(thing.two(bytes[0..<2], bytes[0..<2]))\n"),
      )

      // One diagnostic per site, each caret on its own call rather than twice on the body — which is
      // what says the descent branched and then narrowed each branch to the smallest node.
      e.linesIterator.count(_.contains("which makes heap storage")) shouldBe 2
      e should include("thing/a.sysl:7:13")
      e should include("thing/a.sysl:8:13")
    }

    // The counterpart, and the reason the branching descent is not simply "report every node": one
    // call reached through a chain of nodes is still one thing to change, so the descent stops at the
    // smallest sub-tree that answers rather than reporting each node on the way down to it.
    "while one call reached through several nodes is still one diagnostic" in {
      val e = errOf(
        "thing/a.sysl" ->
          """module thing
            |@no_alloc
            |
            |import sysl.io.line_text
            |
            |one(a: []const u8) -> usize = line_text(a).len + 1
            |""".stripMargin,
        "main.sysl" -> ("var bytes: [2]u8 = [104u8, 105u8]\n" +
          "print(thing.one(bytes[0..<2]))\n"),
      )

      e.linesIterator.count(_.contains("which makes heap storage")) shouldBe 1
    }

    // The same rule the other way: printing is reached constantly from allocator-free code and does
    // not allocate, so it is not refused. A capability that refused `print` would be one no kernel
    // could carry.
    "and printing, which an allocator-free module does constantly, is not" in {
      runOf("thing/a.sysl" -> "module thing\n@no_alloc\n\nsay(n: int)\n    print(\"n is\", n)\n",
        "main.sysl" -> "thing.say(3)") shouldBe "n is 3\n"
    }

    "and the same program without the clause is not obliged to say so" in {
      run(
        """sum(p: *int, n: usize) -> int
          |    var total = 0
          |    var i = 0usize
          |    while i < n do
          |        total += p[i]
          |        i += 1
          |    total
          |
          |var a: [3]int = [4, 5, 6]
          |print(sum(&a[0], 3))
          |""".stripMargin,
      ) shouldBe "15\n"
    }
  }

  "a module with no clause is unaffected" in {
    runOf("thing/a.sysl" -> "module thing\n\nf() -> int = 1\n", "main.sysl" -> "print(thing.f())") shouldBe "1\n"
  }

  /** `no os` and `no posix`, which gate **which modules exist** rather than what the language allows
   * — so they are asked of the module graph where `no alloc` is asked of each construction.
   *
   * `sysl.fs` is what makes any of this checkable: it is the first module in the library to declare
   * `requires os`, and until it existed the narrowing was refused because it would have enforced
   * nothing.
   */
  "'no os' gives up the modules an operating system gates" - {

    "a module that gave it up may not reach one that requires it" in {
      val e = err("@no_os\n\nimport sysl.fs.exists\n\nprint(exists(\"x\"))\n")

      e should include("which requires 'os'")
      e should include("this module declared 'no os'")
      e should include("a module that gave one up may not reach one that needs it")
    }

    // A qualified path needs no import (`reference/modules.md § Imports`), so a rule stated over
    // imports would have missed this entirely — which is why the graph is the reference graph and
    // not the import graph.
    "nor by a qualified path, which needs no import at all" in {
      err("@no_os\n\nprint(sysl.fs.exists(\"x\"))\n") should include("which requires 'os'")
    }

    "the import alone is enough, before anything is named through it" in {
      err("@no_os\n\nimport sysl.fs.*\n\nprint(1)\n") should include("'sysl.fs', which requires 'os'")
    }

    "a named module is named in the message rather than called 'this module'" in {
      errOf(
        "thing/a.sysl" -> "module thing\n@no_os\n\nf() -> bool = sysl.fs.exists(\"x\")\n",
        "main.sysl"    -> "print(thing.f())",
      ) should include("'thing' declared 'no os'")
    }

    // Giving up `os` gives up `posix` with it, since POSIX needs an operating system under it — the
    // implication runs the opposite way from the one `requires` follows.
    "and 'no posix' does not, since posix is what needs an os rather than the other way round" in {
      runOf("thing/a.sysl" -> "module thing\n@no_posix\n\nf() -> int = 1\n",
        "main.sysl" -> "print(thing.f())") shouldBe "1\n"
    }

    "a module that said nothing reaches it freely" in {
      run("import sysl.fs.exists\n\nprint(exists(\"/\"))\n") shouldBe "true\n"
    }

    /* The requirement is transitive: what a module reaches through a third is still what it
     * reaches. A rule stated only over direct edges would let a `no os` module get at the whole of
     * `sysl.fs` by writing one function in between. */
    "and may not reach one through a module that says nothing itself" in {
      val e = errOf(
        "helper/a.sysl" -> "module helper\n\nthere(p: string) -> bool = sysl.fs.exists(p)\n",
        "main.sysl"     -> "@no_os\n\nprint(helper.there(\"x\"))\n",
      )

      e should include("'helper'")
      e should include("this module declared 'no os'")
    }

    "the clause and the requirement may be written on the same module without contradiction" in {
      irOf("thing/a.sysl" -> "module thing\n@requires(os)\n\nf() -> int = 1\n",
        "main.sysl" -> "print(thing.f())") should include("define")
    }
  }
}
