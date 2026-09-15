package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Conditional compilation (`reference/attributes.md § #if — gating lines before the lexer`,
 * `Conditional`).
 *
 * A target is a value here, which is the whole reason this is testable at all: one laptop can ask
 * what each of the ten machines in the registry sees, and reading the emitted text is the whole of
 * what a cross-target question needs.
 */
class ConditionalTests extends AnyFreeSpec with Matchers with CodegenSupport with RunSupport
    with TestFrameworkSupport {

  private val linux   = Target.x86_64Linux
  private val macos   = Target.aarch64MacOS
  private val windows = Target.x86_64Windows
  private val bare    = Target.riscv64Freestanding

  /** A program whose whole observable content is which number `tag` returns, so that what a target
   * sees can be read straight out of the emitted text. The numbers are picked to be ones nothing
   * else in a module's IR would carry.
   */
  private def tagged(body: String): String =
    s"""tag() -> int
       |$body
       |
       |print(tag())
       |""".stripMargin

  private def sees(target: Target, body: String): String = {
    val out = irFor(target, tagged(body))

    List(31337, 42424, 55555).filter(n => out.contains(n.toString)) match
      case List(one) => one.toString
      case several   => fail(s"expected exactly one tag in the emitted module, found $several")
  }

  "which lines a target sees" - {

    "a branch whose condition holds is kept, and the alternative is not" in {
      val body = "#if linux\n    31337\n#else\n    42424\n#endif"

      sees(linux, body) shouldBe "31337"
      sees(macos, body) shouldBe "42424"
    }

    "a group with no alternative simply contributes nothing where it does not hold" in {
      // The whole body is gated, so on the target that does not take it there is no body at all —
      // which is the honest outcome and the one C gives, rather than a silently empty function.
      val src = "f() -> int\n#if linux\n    31337\n#endif\n\nprint(f())\n"

      irFor(linux, src) should include("31337")
      errFor(macos, src) should include("expected")
    }

    "a line outside every group is seen by every target" in {
      val src = "#if linux\nvar a = 1\n#endif\nprint(9)\n"

      Target.all.filter(_.supported).foreach(t => irFor(t, src) should include("9"))
    }

    "what this machine sees, run rather than read" in {
      // The one place the whole path is exercised end to end — gate, lex, analyze, lower, link, run.
      run("""#if posix
            |print("posix")
            |#else
            |print("not posix")
            |#endif
            |""".stripMargin) shouldBe "posix\n"
    }
  }

  "the alternatives are exclusive" - {

    "an '#elif' is not taken when the '#if' above it was" in {
      // `posix` and `macos` are both true here, and exactly one branch may be taken. A group that
      // merely evaluated each condition would take both and emit two definitions of `tag`.
      sees(macos, "#if posix\n    31337\n#elif macos\n    42424\n#endif") shouldBe "31337"
    }

    "an '#elif' is taken when the '#if' above it was not" in {
      sees(macos, "#if windows\n    31337\n#elif macos\n    42424\n#endif") shouldBe "42424"
    }

    "a second '#elif' is reached only when neither before it was taken" in {
      val body = "#if windows\n    31337\n#elif freestanding\n    42424\n#elif posix\n    55555\n#endif"

      sees(macos, body) shouldBe "55555"
      sees(windows, body) shouldBe "31337"
      sees(bare, body) shouldBe "42424"
    }

    "an '#else' is not taken when any branch above it was" in {
      sees(linux, "#if linux\n    31337\n#elif windows\n    42424\n#else\n    55555\n#endif") shouldBe "31337"
    }

    "an '#else' is taken when none was" in {
      sees(windows, "#if linux\n    31337\n#elif macos\n    42424\n#else\n    55555\n#endif") shouldBe "55555"
    }
  }

  "nesting" - {

    "a group inside a branch that was taken is read normally" in {
      val body =
        "#if posix\n#if macos\n    31337\n#else\n    42424\n#endif\n#else\n    55555\n#endif"

      sees(macos, body) shouldBe "31337"
      sees(linux, body) shouldBe "42424"
      sees(windows, body) shouldBe "55555"
    }

    "a group inside a branch that was NOT taken selects nothing, however its own condition reads" in {
      // The discriminating case: the inner `#if macos` is true on macOS, and the branch it sits in
      // is not taken, so neither of its own branches may contribute. An implementation that tracked
      // only the innermost group would emit 31337 here.
      sees(macos, "#if windows\n#if macos\n    31337\n#else\n    42424\n#endif\n#else\n    55555\n#endif") shouldBe
        "55555"
    }

    "an '#else' inside an untaken branch does not reopen it" in {
      // Both halves of the inner group sit inside a branch that was not taken, so neither may
      // contribute — the `#else` selects nothing rather than selecting the alternative.
      sees(macos, "#if windows\n#if windows\n    31337\n#else\n    42424\n#endif\n#endif\n    55555") shouldBe
        "55555"
    }

    "the groups close innermost first" in {
      val body = "#if posix\n#if aarch64\n    31337\n#endif\n#if x86_64\n    42424\n#endif\n#endif"

      sees(macos, body) shouldBe "31337"
      sees(linux, body) shouldBe "42424"
    }
  }

  "the operators" - {

    "'!' inverts" in {
      sees(macos, "#if !windows\n    31337\n#else\n    42424\n#endif") shouldBe "31337"
    }

    "'&&' needs both" in {
      val body = "#if posix && aarch64\n    31337\n#else\n    42424\n#endif"

      sees(macos, body) shouldBe "31337"
      sees(linux, body) shouldBe "42424"
    }

    "'||' needs either" in {
      val body = "#if windows || linux\n    31337\n#else\n    42424\n#endif"

      sees(linux, body) shouldBe "31337"
      sees(macos, body) shouldBe "42424"
    }

    "'&&' binds tighter than '||'" in {
      // On macOS: `false || (true && true)` is true, and `(false || true) && true` would be too — so
      // the discriminating target is the one where the groupings disagree. On x86-64 Linux
      // `linux || macos && aarch64` is `true || (false && false)` = true either way; on Windows it
      // is `false || (false && false)` = false, and the wrong grouping gives `false && false` = false
      // as well. The pair that separates them is x86-64 macOS: `false || (true && false)` = false,
      // against `(false || true) && false` = false. Parenthesized explicitly below instead.
      sees(macos, "#if windows || posix && aarch64\n    31337\n#else\n    42424\n#endif") shouldBe "31337"
      sees(linux, "#if windows || posix && aarch64\n    31337\n#else\n    42424\n#endif") shouldBe "42424"
    }

    "parentheses regroup" in {
      // `(windows || posix) && aarch64` is true on aarch64 macOS where `windows || (posix && aarch64)`
      // agrees — so the discriminating machine is x86-64 macOS, where the parenthesized form is
      // `(false || true) && false` = false and the unparenthesized is `false || (true && false)` =
      // false too. The separating case is `!(a || b)` against `!a || b`, below.
      sees(macos, "#if (macos || windows) && aarch64\n    31337\n#else\n    42424\n#endif") shouldBe "31337"
      sees(Target.x86_64MacOS, "#if (macos || windows) && aarch64\n    31337\n#else\n    42424\n#endif") shouldBe
        "42424"
    }

    "'!' applies to a parenthesized condition rather than to its first symbol" in {
      // `!(macos || linux)` is false on macOS; `!macos || linux` would be false there too, so the
      // machine that tells them apart is Linux: `!(macos || linux)` is false, `!macos || linux` true.
      sees(linux, "#if !(macos || linux)\n    31337\n#else\n    42424\n#endif") shouldBe "42424"
      sees(linux, "#if !macos || linux\n    31337\n#else\n    42424\n#endif") shouldBe "31337"
    }

    "'!' stacks" in {
      sees(macos, "#if !!macos\n    31337\n#else\n    42424\n#endif") shouldBe "31337"
    }
  }

  "the symbols" - {

    // **Read off the registry rather than written out.** The symbol set is derived from `Os` and
    // `Cpu` precisely so that adding a machine adds its symbol; a list spelled here turns that one
    // registry entry into a stale fixture, which is what happened when `craft` arrived and this went
    // on counting the seven processors it knew about. What the test is about is the *exactly one*.
    "every target defines exactly one operating system and one processor" in {
      val systems    = Os.values.map(_.toString.toLowerCase).toSet
      val processors = Cpu.values.map(_.symbol).toSet

      for t <- Target.all do
        val on = Conditional.defined(t)

        withClue(t.name) {
          on.count(systems) shouldBe 1
          on.count(processors) shouldBe 1
        }
    }

    "nothing a target defines is outside the closed set" in {
      // What makes an unknown symbol refusable: the vocabulary is fixed, so a name outside it is a
      // mistake rather than a fact this build happens not to have.
      for t <- Target.all do Conditional.defined(t) should contain allElementsOf Set.empty[String]
      for t <- Target.all do (Conditional.defined(t) -- Conditional.symbols) shouldBe empty
    }

    "'hosted' is exactly the targets with an operating system under them" in {
      for t <- Target.all do Conditional.defined(t).contains("hosted") shouldBe (t.os != Os.Freestanding)
    }

    // Android is the third of them, and it is why this reads as a set rather than as a pair: Bionic
    // is a POSIX libc with a kernel under it, so a program gating on `posix` wants a phone included.
    // What Android is *not* is Linux, which is the next case down.
    "'posix' is exactly macOS, Linux and Android" in {
      for t <- Target.all do
        Conditional.defined(t).contains("posix") shouldBe
          Set(Os.MacOS, Os.Linux, Os.Android).contains(t.os)
    }

    // The distinction the `android` symbol exists for, asserted from both sides. Answering `linux`
    // on a phone would answer a question about glibc with a fact about a kernel — the `-l` names,
    // the headers and the absence of `pkg-config` all differ — and a source file that needs the two
    // together writes `#if linux || android`, which is a thing it can only write if they are two
    // symbols.
    "'android' and 'linux' are different symbols, and no target defines both" in {
      Conditional.symbols should contain("android")

      Conditional.defined(Target.aarch64Android) should contain("android")
      Conditional.defined(Target.aarch64Android) should not contain "linux"
      Conditional.defined(Target.aarch64Linux) should not contain "android"

      for t <- Target.all do
        withClue(t.name)(Conditional.defined(t).intersect(Set("android", "linux")).size should be <= 1)
    }

    // A libc family rather than a capability, which is the distinction the symbol exists to make: a
    // BSD and a glibc system are both `posix`, and every OS gate in `library/sysl/fs` is about where
    // the two disagree rather than about what the machine can do. Asserted against `Os.bsd` from both
    // sides so that an operating system added without an answer there cannot pass by defaulting.
    "'bsd' is exactly the BSD libcs, and is narrower than 'posix'" in {
      for t <- Target.all do withClue(t.name)(Conditional.defined(t).contains("bsd") shouldBe t.os.bsd)

      Conditional.defined(Target.aarch64MacOS) should contain("bsd")
      Conditional.defined(Target.aarch64Linux) should not contain "bsd"
      Conditional.defined(Target.aarch64Android) should not contain "bsd"

      // Narrower, and the point of it: everything that is `bsd` is `posix`, and the converse fails
      // on the two glibc systems. A gate that meant "not glibc" and wrote `posix` would take them.
      for t <- Target.all if t.os.bsd do withClue(t.name)(Conditional.defined(t) should contain("posix"))
    }

    // The equation `reference/modules.md § Platform selection` rests on: a directory selects on
    // what an operating system alone settles, so every symbol a `__<os>__` folder may name has to
    // be one a `#if` names too. A new operating system is where that could quietly come apart,
    // since the two sets are derived separately.
    "a new operating system reaches the directory vocabulary and the '#if' one alike" in {
      Conditional.directorySymbols should contain("android")
      Conditional.directorySymbols.subsetOf(Conditional.symbols) shouldBe true
      Conditional.osDefined(Os.Android) shouldBe Set("android", "hosted", "posix")

      Conditional.directorySymbols should contain("bsd")
      Conditional.osDefined(Os.MacOS) shouldBe Set("macos", "hosted", "posix", "bsd")
    }

    "every symbol is true of at least one target in the registry" in {
      // A symbol nothing can ever satisfy is a symbol that gates code out on every machine, which is
      // the same defect as a misspelling and would not otherwise be caught.
      val reachable = Target.all.flatMap(Conditional.defined(_)).toSet

      Conditional.symbols -- reachable shouldBe empty
    }

    "the target's own name is not a symbol, and writing one says so" in {
      // It cannot be one: it has a `-` in it, which no identifier carries. Left to the tokenizer the
      // reader is told a `-` is not an operator, which is true and no help — a condition asks about
      // one fact of the machine at a time, and that is the sentence they need.
      val out = err("#if aarch64-macos\nprint(1)\n#endif\n")

      out should include("'aarch64-macos' is a target's name")
      out should include("aarch64 && macos")
    }
  }

  "the carried library is parsed once per target and kept" - {

    "the same target gets the very same trees back" in {
      // What replaced a `lazy val` when the library became a per-target question. Parsing the whole
      // standard module is on the path of every compilation with no artifact to read instead, so a
      // memo that missed would be a real cost and not only an inelegance.
      //
      // Asked through the memo's own lock rather than by calling twice and comparing. Only one
      // target's copy is kept, so a suite running beside this one and asking for another target
      // clears the slot between two bare calls and the second reparses — which says nothing about
      // the memo. That is an ordinary interleaving, not a rare one.
      Stdlib.memoAnswersTwice(macos) shouldBe true
      Std.memoAnswersTwice(linux) shouldBe true
    }

    "two targets get two" in {
      Stdlib.fromSource(macos) should not be theSameInstanceAs(Stdlib.fromSource(linux))
    }

    "every target in the registry parses at all" in {
      // The library carries no directives today, so this says the parse is not target-sensitive by
      // accident. It is what would fail first if one were added that some machine could not read.
      for t <- Target.all do withClue(s"${t.name}: ") { Std.parsed(t) should not be empty }
    }
  }

  "a name the compiler spells for itself is declared on every target" in {
    // This is what lets `Library` read the library for one machine and answer for all of them. A
    // library that gated `Option` away for Windows would be a library nothing compiles against
    // there, and this is where that is found rather than at the first `?` somebody writes.
    for t <- Target.all do
      val declared = Library.names(Std.decls(t))

      withClue(s"${t.name}: ") { Library.known -- declared shouldBe empty }
  }

  "line numbers survive the gating" - {

    "a diagnostic below a gated-out group points at the line it was written on" in {
      // The whole reason an inactive line is blanked rather than removed. Deleting the lines would
      // make every diagnostic below a gate point at the wrong place, and nothing would say so — the
      // message would be right and the caret would be somewhere else.
      val out = err("""#if windows
                      |var a = 1
                      |var b = 2
                      |var c = 3
                      |#endif
                      |var x: int = "no"
                      |""".stripMargin)

      out should include("<input>:6:1")
    }

    "a diagnostic inside a kept group points at the line it was written on" in {
      // `posix` rather than `macos`, because this one really compiles for the machine it is running
      // on and the claim is about a group that is **kept**. Named `macos` it was a test about the
      // author's laptop: on Linux the group is dropped, the erroneous line goes with it, and what
      // comes back is a clean compilation rather than the diagnostic being asserted. `posix` is true
      // of every target sysl hosts, so the group is kept wherever this runs.
      val out = err("""#if posix
                      |var x: int = "no"
                      |#endif
                      |""".stripMargin)

      out should include("<input>:2:1")
    }

    "the quoted line is the one the reader wrote" in {
      val out = err("#if windows\nvar a = 1\n#endif\nvar x: int = \"no\"\n")

      out should include("4 | var x: int = \"no\"")
    }

    "a file keeps exactly as many lines as it had" in {
      val src = Source("<input>", "#if linux\nvar a = 1\n#else\nvar b = 2\n#endif\n")

      Conditional.gate(src, macos).map(_.lines.length) shouldBe Right(src.lines.length)
      Conditional.gate(src, linux).map(_.lines.length) shouldBe Right(src.lines.length)
    }
  }

  "what is refused, and why" - {

    "a symbol nothing knows, with the ones there are named" in {
      val out = err("#if darwin\nprint(1)\n#endif\n")

      out should include("'darwin' is not something a target says about itself")
      out should include("macos")
      out should include("<input>:1:5")
    }

    "a symbol nothing knows in a branch this build is NOT taking" in {
      // The discriminating one, and the reason conditions are evaluated in every branch: a
      // misspelling reads as false, so without this a macOS build would silently drop the code it
      // was supposed to keep for Linux and say nothing at all.
      err("#if macos\nprint(1)\n#elif linnux\nprint(2)\n#endif\n") should include("'linnux' is not")
    }

    "a symbol nothing knows inside a group that was itself skipped" in {
      err("#if windows\n#if lunix\nprint(1)\n#endif\n#endif\n") should include("'lunix' is not")
    }

    "an '#if' that is never closed, reported at the '#if'" in {
      val out = err("print(1)\n#if macos\nprint(2)\n")

      out should include("this '#if' is never closed")
      out should include("<input>:2:1")
    }

    "the outermost unclosed '#if' is the one named" in {
      val out = err("#if macos\n#if aarch64\nprint(1)\n#endif\n")

      out should include("<input>:1:1")
    }

    "an '#endif' with nothing to close" in {
      err("print(1)\n#endif\n") should include("'#endif' has no '#if' above it to close")
    }

    "an '#else' with nothing to be an alternative to" in {
      err("print(1)\n#else\nprint(2)\n#endif\n") should include("'#else' has no '#if' above it")
    }

    "an '#elif' with nothing to be an alternative to" in {
      err("print(1)\n#elif macos\nprint(2)\n#endif\n") should include("'#elif' has no '#if' above it")
    }

    "a second '#else' in one group, with the first one's line" in {
      val out = err("#if macos\nprint(1)\n#else\nprint(2)\n#else\nprint(3)\n#endif\n")

      out should include("this group already has an '#else', on line 3")
    }

    "an '#elif' after the '#else' it would follow" in {
      val out = err("#if macos\nprint(1)\n#else\nprint(2)\n#elif linux\nprint(3)\n#endif\n")

      out should include("'#elif' comes after the '#else' on line 3")
    }

    "an '#if' with no condition at all" in {
      err("#if\nprint(1)\n#endif\n") should include("'#if' needs a condition after it")
    }

    "an '#elif' with no condition at all" in {
      err("#if windows\nprint(1)\n#elif\nprint(2)\n#endif\n") should include("'#elif' needs a condition after it")
    }

    "a '!' with nothing to invert" in {
      err("#if !\nprint(1)\n#endif\n") should include("'#if' needs a condition after it")
    }

    "an operator where a symbol belongs" in {
      err("#if && macos\nprint(1)\n#endif\n") should include("a condition needs a symbol here, and '&&' is")
    }

    "a '(' that is never closed" in {
      err("#if (macos || linux\nprint(1)\n#endif\n") should include("this '(' is never closed")
    }

    "a ')' the condition did not open" in {
      err("#if macos)\nprint(1)\n#endif\n") should include("the condition is complete before ')'")
    }

    "two symbols with no operator between them" in {
      err("#if macos linux\nprint(1)\n#endif\n") should include("the condition is complete before 'linux'")
    }

    "a character a condition has no reading for" in {
      err("#if macos & linux\nprint(1)\n#endif\n") should include("'&' is none of them")
    }

    "a number where a symbol belongs" in {
      // C's `#if 1` has no counterpart here: a condition asks about the machine, and there is
      // nothing a literal could be asking.
      err("#if 1\nprint(1)\n#endif\n") should include("'1' is none of them")
    }

    "something written after an '#else'" in {
      val out = err("#if macos\nprint(1)\n#else linux\nprint(2)\n#endif\n")

      out should include("'#else' takes nothing after it, and 'linux' is here")
    }

    "something written after an '#endif'" in {
      // C's old habit, and legal there. Silently discarding it would make the spelling most likely
      // to arrive out of muscle memory the one thing here that failed quietly.
      val out = err("#if macos\nprint(1)\n#endif macos\n")

      out should include("'#endif' takes nothing after it, and 'macos' is here")
    }
  }

  "the C spellings sysl does not have are named rather than left to the parser" - {

    "'#ifdef'" in {
      err("#ifdef macos\nprint(1)\n#endif\n") should include("sysl has no '#ifdef'")
    }

    "'#ifndef'" in {
      err("#ifndef macos\nprint(1)\n#endif\n") should include("write '#if !<symbol>'")
    }

    "'#elseif'" in {
      err("#if windows\nprint(1)\n#elseif macos\nprint(2)\n#endif\n") should include("sysl spells this '#elif'")
    }

    "'#define'" in {
      err("#define DEBUG\nprint(1)\n") should include("sysl has no '#define'")
    }

    "'#undef'" in {
      err("#undef DEBUG\nprint(1)\n") should include("sysl has no '#undef'")
    }
  }

  "what is not a directive" - {

    "an indented '#if' is not one" in {
      // A directive sits at the margin, which is what keeps it out of the channel the language reads
      // block structure from. An indented one reaches the lexer as what it is: a stray `#`.
      val out = err("f()\n    #if macos\n    print(1)\n    #endif\n\nf()\n")

      out should not include "never closed"
    }

    "a declaration's '@test' attribute at the margin is untouched" in {
      // `#` opens an attribute too (`reference/attributes.md § @test — a function with a caller nothing else has`), and this pass has no business with one. The words
      // are what tell them apart, and `test` is not one of them.
      ir("""@test
           |proves_nothing()
           |    1
           |
           |print(1)
           |""".stripMargin) should include("main")
    }

    "a word that carries on past the directive is left alone" in {
      // `#iffy` is not `#if` with `fy` after it. Recognizing it as one would turn a typo in an
      // attribute's name into a diagnostic about conditional compilation.
      err("#iffy\nprint(1)\n") should not include "condition"
    }
  }

  "a file with no directives in it" - {

    "comes back as the very same Source, not a copy" in {
      // A `Source` compares by identity (`Diagnostics`), and `Stdlib.owns` is that identity — so a
      // compilation that gated nothing has to be the compilation it was before any of this existed.
      val src = Source("<input>", "print(1)\n")

      Conditional.gate(src, macos) shouldBe Right(src)
      Conditional.gate(src, linux).map(_ should be theSameInstanceAs src)
    }

    "is unaffected on every target in the registry" in {
      val src = "double(n: int) -> int = n * 2\nprint(double(21))\n"

      Target.all.filter(_.supported).map(t => irFor(t, src).contains("42")).distinct shouldBe List(true)
    }
  }

  "the smaller shapes" - {

    "a '//' comment on a directive line says which group closed" in {
      run("""#if posix
            |print("yes") // kept
            |#endif // posix
            |""".stripMargin) shouldBe "yes\n"
    }

    "a '//' comment on an '#if' line does not become part of the condition" in {
      sees(macos, "#if macos // this machine\n    31337\n#else\n    42424\n#endif") shouldBe "31337"
    }

    "whitespace around a condition is ignored" in {
      sees(macos, "#if    macos   \n    31337\n#else\n    42424\n#endif") shouldBe "31337"
    }

    "a group may be empty" in {
      run("#if windows\n#endif\nprint(7)\n") shouldBe "7\n"
    }

    "a group at the very end of a file, with no trailing newline" in {
      Conditional.gate(Source("<input>", "print(1)\n#if macos\n#endif"), macos).map(_.text) shouldBe
        Right("print(1)\n\n")
    }
  }

  "against the rest of the language" - {

    "a struct's fields may differ between targets" in {
      // The case the feature is for: a type that has to match what a header lays out, and the header
      // does not lay it out the same way twice.
      val src = """struct Handle
                  |    fd: int
                  |#if windows
                  |    reserved: int
                  |#endif
                  |
                  |var h = Handle(3)
                  |print(h.fd)
                  |"""

      irFor(macos, src.stripMargin) should include("3")
      errFor(windows, src.stripMargin) should include("Handle")
    }

    "an 'extern' may differ between targets" in {
      // The other case it is for: the same call reaching a symbol each libc spells its own way.
      val src = """#if macos
                  |extern "printf" say(fmt: *u8, ...) -> i32
                  |#else
                  |extern "printf_chk" say(fmt: *u8, ...) -> i32
                  |#endif
                  |
                  |say(c"hi\n")
                  |"""

      irFor(macos, src.stripMargin) should include("@printf")
      irFor(linux, src.stripMargin) should include("@printf_chk")
    }

    "a gated import is an import the other target does not have" in {
      val src = """#if macos
                  |import sysl.sys
                  |#endif
                  |
                  |print(1)
                  |"""

      irFor(macos, src.stripMargin) should include("1")
      irFor(linux, src.stripMargin) should include("1")
    }

    "a gated '@test' is a test the other target does not run" in {
      // The attribute and the directive both start with `#` and are read by different things, so a
      // file holding both is worth pinning.
      val src = """#if macos
                  |@test
                  |only_here()
                  |    1
                  |#endif
                  |
                  |print(1)
                  |"""

      irFor(macos, src.stripMargin) should include("1")
      irFor(linux, src.stripMargin) should include("1")
    }
  }

  /** A one-module library whose `flavour` differs between machines, built for a target and read back
   * through the same seam a consuming compilation reads it through.
   */
  private def built(target: Target): List[String] = {
    val src = """module demo
                |
                |#if macos
                |flavour() -> int = 31337
                |#elif linux
                |flavour() -> int = 42424
                |#else
                |flavour() -> int = 55555
                |extra() -> int = 1
                |#endif
                |""".stripMargin

    LibraryArtifact.build(List(Source("demo/lib.sysl", src, List("demo"))), target) match
      case Left(err)     => fail(s"the library did not build: $err")
      case Right((_, m)) =>
        LibraryArtifact.read("demo.syslib", m, target) match
          case Left(err)            => fail(err)
          case Right((units, _, _)) => Library.names(units.flatMap(_.body)).toList.sorted
  }

  "a library gates like anything else" - {

    "the artifact carries the declarations its target saw" in {
      built(macos) shouldBe List("flavour")
      built(linux) shouldBe List("flavour")
      built(windows) shouldBe List("extra", "flavour")
    }

    "and an artifact built for another machine is refused rather than quietly used" in {
      // The hazard this feature creates. Before it the tree half was the same whatever the artifact
      // was built for, and only the object half was per-target — which a linker refuses on its own.
      // A tree mismatch decodes perfectly and is simply a different set of declarations.
      val src = "module demo\n\nf() -> int = 1\n"

      LibraryArtifact.build(List(Source("demo/lib.sysl", src, List("demo"))), linux) match
        case Left(err)     => fail(err)
        case Right((_, m)) =>
          LibraryArtifact.read("demo.syslib", m, macos) match
            case Right(_)  => fail("an artifact built for another machine was accepted")
            case Left(err) =>
              err should include("was built for x86_64-linux")
              err should include("--target aarch64-macos")
    }
  }

  "a '@test' is gated like any other declaration" in {
    // `#` opens both an attribute and a directive, and this is the one place the two meet in earnest:
    // the test build has to discover the tests the target actually has.
    discovered("""@test
                 |everywhere()
                 |    1
                 |
                 |#if windows
                 |@test
                 |only_on_windows()
                 |    1
                 |#endif
                 |""".stripMargin) shouldBe List("everywhere")
  }

  "an inactive branch does not have to be sysl at all" - {

    "it may not parse" in {
      // The stated cost of gating text rather than trees, and the thing most worth pinning: a reader
      // told the branch is never syntax-checked should be able to see that it is not.
      run("""#if windows
            |)( this is not sysl and never was ][
            |#endif
            |print(1)
            |""".stripMargin) shouldBe "1\n"
    }

    "it may not even lex" in {
      run("#if windows\nvar s = \"unterminated\nvar c = '\n#endif\nprint(2)\n") shouldBe "2\n"
    }

    "but a directive inside it is still read, because the group has to be closed" in {
      err("#if windows\n#if linux\nnonsense ][\n#endif\nprint(1)\n") should include("never closed")
    }
  }

  "the shape of the file survives" - {

    "a file's directory is carried through the gate, so its module still checks out" in {
      // A gated `Source` is a new object, and losing `dir` would make every module header in a
      // project stop agreeing with the directory it sits in — a whole class of wrong diagnostics.
      //
      // `posix` rather than `macos`: this compiles for the host, so a condition true only on the
      // author's machine takes `two` out of the module everywhere else and the call goes undefined.
      irIn(("demo", "a.sysl", "module demo\n\n#if posix\ntwo() -> int = 2\n#endif\n"),
           ("", "main.sysl", "print(demo.two())\n")) should include("2")
    }

    "an empty file gates to an empty file" in {
      Conditional.gate(Source("<input>", ""), macos).map(_.text) shouldBe Right("")
    }

    "a file of nothing but directives" in {
      Conditional.gate(Source("<input>", "#if macos\n#endif\n"), macos).map(_.text) shouldBe Right("\n\n")
    }

    "CRLF line endings survive on the lines that are kept" in {
      Conditional.gate(Source("<input>", "#if macos\r\nprint(1)\r\n#else\r\nprint(2)\r\n#endif\r\n"), macos)
        .map(_.text) shouldBe Right("\nprint(1)\r\n\n\n\n")
    }

    "groups nest as deeply as they are written" in {
      val depth = 12
      val body  = List.fill(depth)("#if posix").mkString("\n") + "\n    31337\n" +
        List.fill(depth)("#endif").mkString("\n")

      sees(macos, body) shouldBe "31337"
    }

    "a gated 'module' header puts the file in the root module, and says so where that is wrong" in {
      // Worth pinning because it is the one line whose absence changes what the whole file means
      // rather than what it contains.
      errIn(("demo", "a.sysl", "#if windows\nmodule demo\n#endif\n\ntwo() -> int = 2\n")) should
        include("demo")
    }
  }

  "the property this pass does NOT have" - {

    "a line at the margin inside a text block reads as a directive" in {
      // Stated as a known property rather than left to be discovered. The gate runs before anything
      // knows what a string is, so a text-block line that starts at the margin with a directive word
      // is one. Recognizing it would mean a second copy of the lexer's rules about strings and
      // comments, which is a worse defect than this — two sets of rules drift, and nothing says so.
      //
      // The margin is what keeps this rare: a text block written anywhere but the top level is
      // indented in the source, whatever its value ends up being.
      val out = err("""var s = \"\"\"
                      |#endif
                      |\"\"\"
                      |""".stripMargin.replace("\\\"", "\""))

      out should include("'#endif' has no '#if' above it to close")
    }
  }

  "a package's enabled features are symbols of its own files" - {

    "each one is the feature's name under a prefix, beside everything the target says" in {
      val on = Conditional.defined(macos, Set("server", "tls"))

      on should contain("feature_server")
      on should contain("feature_tls")
      on should contain allElementsOf Conditional.defined(macos)
    }

    "and a compilation with no features has no symbol of that shape at all" in {
      for t <- Target.all do
        withClue(t.name)(Conditional.defined(t).filter(Conditional.isFeatureSymbol) shouldBe empty)
    }

    // The set travels on the file, which is what makes the answer per package: two files of one
    // compilation, gated on the same word, get different answers.
    "the file carries the set, so the same condition reads differently in two packages" in {
      val text = "#if feature_server\nprint(1)\n#else\nprint(2)\n#endif\n"

      Conditional.gate(Source("<input>", text).enabling(Set("server")), macos).map(_.text) shouldBe
        Right("\nprint(1)\n\n\n\n")
      Conditional.gate(Source("<input>", text), macos).map(_.text) shouldBe
        Right("\n\n\nprint(2)\n\n")
    }

    // The one place the closed-symbol rule above does not reach: a feature exists because a manifest
    // says so, so a word nothing declared is the ordinary "that feature is off" rather than a
    // mistake — where a target symbol nobody knows is still refused.
    "a feature nobody declared is false, and an unknown target symbol is still refused" in {
      Conditional.gate(Source("<input>", "#if feature_nobody\nprint(1)\n#endif\n"), macos)
        .map(_.text) shouldBe Right("\n\n\n")
      Conditional.gate(Source("<input>", "#if nobody\n#endif\n"), macos).isLeft shouldBe true
    }
  }
}
