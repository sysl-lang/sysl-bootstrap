package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A **file-private** name is scoped to its file (`reference/modules.md § A file-private name is
 * scoped to its file`).
 *
 * `private` in sysl is private to the file, and until 2026-08-27 it restricted the *reach* of a name
 * without restricting the *namespace*: a second file declaring its own `Limit` was refused as a
 * duplicate of a name it could not have named anyway. That defeated the thing file-privacy is for —
 * the reason to keep a helper to its file is that `Limit`, `helper`, `check` are local matters — so
 * a module of any size grew `MaxCallDepth` beside `MaxHashDepth` for two bounds that were each one
 * file's business. Rust, C and Go all scope the name as well as the reach.
 *
 * **And a private name SHADOWS a public one of the same spelling in another file**, which until
 * 0.0.151 was refused as a duplicate. It is C's `static` in one translation unit beside an `extern`
 * of that name in another: inside the declaring file its own declaration answers, and every other
 * file — and every importer — sees only the public one. What stays refused is a second declaration
 * of one spelling inside a single file, whatever the two visibilities, and two public declarations
 * of one spelling anywhere in a module.
 */
class FilePrivateNameTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  "two files may each keep a name to themselves" - {

    "a constant" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |from_one() -> int = Limit
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private const Limit: int = 2
           |from_two() -> int = Limit
           |""".stripMargin),
      ) shouldBe "1 2\n"
    }

    // The card's own case: two files of a module each bounding how deep something walks, each
    // wanting to call the bound what it is.
    "a function, which is the case the card was filed from" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private helper() -> int = 1
           |from_one() -> int = helper()
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private helper() -> int = 2
           |from_two() -> int = helper()
           |""".stripMargin),
      ) shouldBe "1 2\n"
    }

    "a module 'val'" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private val base: int = 10
           |from_one() -> int = base
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private val base: int = 20
           |from_two() -> int = base
           |""".stripMargin),
      ) shouldBe "10 20\n"
    }

    "a struct" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private struct Cell
           |    n: int
           |from_one() -> int = Cell(1).n
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private struct Cell
           |    s: int
           |    t: int
           |from_two() -> int = Cell(2, 3).t
           |""".stripMargin),
      ) shouldBe "1 3\n"
    }

    // Each file's own declaration is what its bodies see — the assertion the two above would still
    // pass if both files somehow shared one declaration, so it is made on its own.
    "and each file names its own, not the other's" in {
      runIn(
        ("", "main.sysl", "print(m.one_says(), m.two_says())"),
        ("m", "one.sysl",
         """module m
           |private const Which: int = 111
           |one_says() -> int = Which
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private const Which: int = 222
           |two_says() -> int = Which
           |""".stripMargin),
      ) shouldBe "111 222\n"
    }
  }

  /** The rule the user chose on 2026-09-28: a file-private declaration shadows a public one of its
   * spelling in another file of the module, C's `static` beside an `extern`.
   *
   * Every case is written with the private file both **before** and **after** the public one where
   * the order could matter, because the plain key is claimed at hoisting and the files are hoisted in
   * the order they arrive — a private declaration hoisted first must still leave the key to the
   * public one that every other file resolves.
   */
  "a private name shadows a public one of its spelling in another file" - {

    def helpers(privateFirst: Boolean) = {
      val pub =
        ("m", "pub.sysl",
         """module m
           |helper() -> int = 1
           |from_pub() -> int = helper()
           |""".stripMargin)
      val priv =
        ("m", "priv.sysl",
         """module m
           |private helper() -> int = 2
           |from_priv() -> int = helper()
           |""".stripMargin)
      val third =
        ("m", "third.sysl",
         """module m
           |from_third() -> int = helper()
           |""".stripMargin)
      val main = ("", "main.sysl", "print(m.from_pub(), m.from_priv(), m.from_third(), m.helper())")

      if privateFirst then List(main, priv, pub, third) else List(main, pub, priv, third)
    }

    // (1) The same kind, the same signature: exactly the pair that used to be "already declared".
    "a function of the same signature: its own file calls the private one, every other file the public" in {
      runIn(helpers(privateFirst = false)*) shouldBe "1 2 1 1\n"
    }

    "and the same whichever of the two files is hoisted first" in {
      runIn(helpers(privateFirst = true)*) shouldBe "1 2 1 1\n"
    }

    // (2)
    "a type: the private file's 'T' is its own, a third file's is the public one" in {
      for privateFirst <- List(false, true) do
        val pub =
          ("m", "b.sysl",
           """module m
             |struct T
             |    s: string
             |from_b() -> string = T("public").s
             |""".stripMargin)
        val priv =
          ("m", "a.sysl",
           """module m
             |private struct T
             |    n: int
             |    k: int
             |from_a() -> int = T(3, 4).k
             |""".stripMargin)
        val third =
          ("m", "c.sysl",
           """module m
             |from_c() -> string = T("third").s
             |""".stripMargin)
        val main = ("", "main.sysl", "print(m.from_a(), m.from_b(), m.from_c(), m.T(\"main\").s)")
        val order = if privateFirst then List(main, priv, pub, third) else List(main, pub, third, priv)

        withClue(s"private file first: $privateFirst") {
          runIn(order*) shouldBe "4 public third main\n"
        }
    }

    // (3) Every kind of value: a constant, a module 'val', module storage, and an 'extern' variable.
    "a constant" in {
      runIn(
        ("", "main.sysl", "print(m.from_a(), m.from_b(), m.Limit)"),
        ("m", "a.sysl",
         """module m
           |private const Limit: int = 1
           |from_a() -> int = Limit
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |const Limit: int = 2
           |from_b() -> int = Limit
           |""".stripMargin),
      ) shouldBe "1 2 2\n"
    }

    "a module 'val'" in {
      runIn(
        ("", "main.sysl", "print(m.from_a(), m.from_b(), m.base)"),
        ("m", "a.sysl",
         """module m
           |private val base: int = 10
           |from_a() -> int = base
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |val base: int = 20
           |from_b() -> int = base
           |""".stripMargin),
      ) shouldBe "10 20 20\n"
    }

    // Storage is where two declarations sharing one key would show: a write through one name read
    // back through the other.
    "module storage, which is two cells rather than one" in {
      runIn(
        ("", "main.sysl", "m.bump_a()\nm.bump_a()\nm.bump_b()\nprint(m.read_a(), m.read_b(), m.count)"),
        ("m", "a.sysl",
         """module m
           |private var count: int = 100
           |bump_a()
           |    count += 1
           |read_a() -> int = count
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |var count: int = 0
           |bump_b()
           |    count += 1
           |read_b() -> int = count
           |""".stripMargin),
      ) shouldBe "102 1 1\n"
    }

    // 'optind' is libc's, an 'int' starting at 1 on every platform this runs on.
    "an 'extern' variable" in {
      runIn(
        ("", "main.sysl", "print(m.from_a(), m.from_b())"),
        ("m", "a.sysl",
         """module m
           |private extern "optind" idx: i32
           |from_a() -> i32 = idx
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |val idx: i32 = 7
           |from_b() -> i32 = idx
           |""".stripMargin),
      ) shouldBe "1 7\n"
    }

    // Public is not the only reach wider than a file: a 'private[m]' declaration is one every file of
    // the module may write, so a bare 'private' one shadows it the same way.
    "a 'private[m]' declaration counts as wider, and is shadowed the same way" in {
      runIn(
        ("", "main.sysl", "print(m.from_a(), m.from_b())"),
        ("m", "a.sysl",
         """module m
           |private const Limit: int = 1
           |from_a() -> int = Limit
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |private[m] const Limit: int = 2
           |from_b() -> int = Limit
           |""".stripMargin),
      ) shouldBe "1 2\n"
    }

    // (4) A private declaration is never exported, so an importer's name is the public one whether it
    // spells the path out or imports the name.
    "an importer from another module sees only the public one" in {
      runIn(
        ("", "main.sysl", "import m.helper\n\nprint(helper(), m.helper(), m.from_a())"),
        ("m", "a.sysl",
         """module m
           |private helper() -> string = "private"
           |from_a() -> string = helper()
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |helper() -> string = "public"
           |""".stripMargin),
      ) shouldBe "public public private\n"
    }

    // A generic is resolved in the scope of the file that declared it, wherever it is instantiated —
    // so a body naming 'helper' means the public one even when the file instantiating it has a
    // private 'helper' of its own.
    "a generic from another file resolves its body in its own file, not in the caller's" in {
      runIn(
        ("", "main.sysl", "print(m.from_a())"),
        ("m", "a.sysl",
         """module m
           |private helper() -> int = 100
           |private struct Box
           |    tag: string
           |from_a() -> string = s"${twice(0)} ${boxed(0).n} ${helper()} ${Box("mine").tag}"
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |helper() -> int = 1
           |struct Box
           |    n: int
           |twice[T](x: T) -> int = helper() + helper()
           |boxed[T](x: T) -> Box = Box(5)
           |""".stripMargin),
      ) shouldBe "2 5 100 mine\n"
    }

    // The escape hatch: the module's own path resolves the plain key directly, so the file that
    // shadowed a name can still reach the public declaration when it means to.
    "and the private file reaches the public one by the module's own path" in {
      runIn(
        ("", "main.sysl", "print(m.from_a())"),
        ("m", "a.sysl",
         """module m
           |private helper() -> int = 2
           |from_a() -> string = s"${helper()} ${m.helper()}"
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |helper() -> int = 1
           |""".stripMargin),
      ) shouldBe "2 1\n"
    }

    // (8) Both are emitted, under two symbols — the private one at its numbered slot and with
    // internal linkage, since every caller it has is in its own file.
    "and both functions are emitted, each under a symbol of its own" in {
      val ir = irIn(
        ("", "main.sysl", "print(m.from_a(), m.helper())"),
        ("m", "a.sysl",
         """module m
           |private helper() -> int = 2
           |from_a() -> int = helper()
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |helper() -> int = 1
           |""".stripMargin),
      )

      ir should include regex """define [^\n]*@"?m\$helper"?\("""
      ir should include regex """define internal [^\n]*@"?m\$helper\.private1"?\("""
      ir should include regex """call [^\n]*@"?m\$helper\.private1"?\("""
    }
  }

  /** Which declaration a bare name means inside the private file, when the public one is a different
   * overload or a different kind.
   *
   * **Between two functions, a private declaration shadows only the public overloads a call could not
   * tell from it** — the pairs `reference/declarations.md § Overloading` refuses inside one file —
   * and joins the rest as an overload set, which is what a private overload beside a public set of
   * another signature has always done. That was measured before it was chosen: the self-hosted
   * compiler and slate hold 34 such pairs between them (`held`, `joined`, `declares`, `complain`…),
   * every one a file calling both its own private overload and the module's public one, and hiding
   * the public set wholesale would have broken each. **Any other pairing hides the whole spelling**:
   * a private constant beside a public function of its name has nothing a call could tell apart.
   */
  "the private file's spelling, against a public one of another signature or kind" - {

    // What the private file always saw, kept: the pair differs in its parameters, so a call tells
    // them apart and neither shadows the other.
    "a private overload of another signature joins the public set in its own file" in {
      runIn(
        ("", "main.sysl", "print(m.use_b(), m.use_b_int())"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n + 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |use_b() -> string = skip_line("x")
           |use_b_int() -> int = skip_line(1)
           |""".stripMargin),
      ) shouldBe "x 2\n"
    }

    // Both cases at once, and from the other file order: the private '(n: int)' shadows its public
    // twin, and the public '(b: bool)' — which no private declaration could be confused with — is
    // still called from the private file.
    "and shadows exactly the public overloads a call could not tell from it" in {
      runIn(
        ("", "main.sysl", "print(m.from_priv(), m.from_pub(), m.flip(true))"),
        ("m", "priv.sysl",
         """module m
           |private flip(n: int) -> int = n * 100
           |from_priv() -> string = s"${flip(1)} ${flip(false)}"
           |""".stripMargin),
        ("m", "pub.sysl",
         """module m
           |flip(n: int) -> int = -n
           |flip(b: bool) -> bool = !b
           |from_pub() -> int = flip(1)
           |""".stripMargin),
      ) shouldBe "100 true -1 false\n"
    }

    // A difference behind a default is the other pair the overloading rule refuses: a one-argument
    // call fits both, so the private declaration shadows the public one rather than colliding. The
    // private one is the one carrying the default on purpose — were the public one still a
    // candidate, the tie-break preferring a call that needs no default would choose it.
    "a public overload a default makes indistinguishable is shadowed too" in {
      runIn(
        ("", "main.sysl", "print(m.from_priv(), m.g(1))"),
        ("m", "priv.sysl",
         """module m
           |private g(x: int, y: int = 0) -> string = "private"
           |from_priv() -> string = g(1)
           |""".stripMargin),
        ("m", "pub.sysl",
         """module m
           |g(x: int) -> string = "public"
           |""".stripMargin),
      ) shouldBe "private public\n"
    }

    "and the module's path reaches the public set from there" in {
      runIn(
        ("", "main.sysl", "print(m.use_b(), m.use_b_int())"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n + 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |use_b() -> string = skip_line("x")
           |use_b_int() -> int = m.skip_line(1)
           |""".stripMargin),
      ) shouldBe "x 2\n"
    }

    "a private constant beside a public function of its name" in {
      runIn(
        ("", "main.sysl", "print(m.from_a(), m.size(), m.from_b())"),
        ("m", "a.sysl",
         """module m
           |private const size: int = 3
           |from_a() -> int = size + m.size()
           |""".stripMargin),
        ("m", "b.sysl",
         """module m
           |size() -> int = 40
           |from_b() -> int = size()
           |""".stripMargin),
      ) shouldBe "43 40 40\n"
    }
  }

  "what is still refused" - {

    // (7) One file, one spelling, twice — whatever the two visibilities.
    "one file declaring the same private name twice, which is the ordinary duplicate" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |private const Limit: int = 2
           |""".stripMargin),
      ) should include("already declared")
    }

    "and one file declaring a name both privately and publicly" in {
      for (first, second) <- List(("private ", ""), ("", "private ")) do
        withClue(s"'${first}const' then '${second}const': ") {
          errIn(
            ("", "main.sysl", "print(1)"),
            ("m", "one.sysl",
             s"""module m
                |${first}const Limit: int = 1
                |${second}const Limit: int = 2
                |""".stripMargin),
            ("m", "two.sysl",
             """module m
               |const Other: int = 3
               |""".stripMargin),
          ) should include("constant 'Limit' is already declared")
        }
    }

    // A public declaration in a third file is no licence for one file to declare the spelling twice.
    "and the same in a file whose name another file also declares publicly" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private helper() -> int = 1
           |helper() -> int = 2
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |helper() -> int = 3
           |""".stripMargin),
      ) should include("function 'helper' is already declared")
    }

    // (6) Two public declarations are one name both files may write, so they are still a duplicate.
    "two public declarations of one name in two files" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |const Limit: int = 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |const Limit: int = 2
           |""".stripMargin),
      ) should include("constant 'Limit' is already declared")
    }

    // Told once, at the second declaration. Both stay registered so each body is still walked, and a
    // call to the name used to be reported again as "ambiguous" between the two — the same mistake,
    // at a line that did not make it.
    "and two public functions of one signature, reported once rather than again at each call" in {
      val message = errIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "one.sysl",
         """module m
           |helper() -> int = 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |helper() -> int = 2
           |use() -> int = helper()
           |""".stripMargin),
      )

      message should include("function 'helper' is already declared")
      message should not include "ambiguous"
    }

    // Reach is untouched by any of this: a name scoped to its file is still unreachable from
    // outside it, which is what `private` was always buying.
    "and a sibling file still cannot name the other's private declaration" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |borrow() -> int = Limit
           |""".stripMargin),
      ) should include("Limit")
    }
  }

  /** A function name stands for every declaration of it, so reach has to be asked of the whole
   * overload set (`reference/modules.md`: *"A name a file may not reach is not a candidate for
   * it"*).
   *
   * From every file but its own the private declaration is not a candidate at all: it may not take a
   * call, may not make one ambiguous, and may not stand in the way of the public declaration being
   * found.
   */
  "a private declaration is no candidate outside its own file" - {

    "the public one takes the call its own file makes" in {
      runIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n + 1
           |use() -> int = skip_line(1)
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |""".stripMargin),
      ) shouldBe "2\n"
    }

    // The defect this suite's section was written from: the sibling's private declaration took a
    // call that no declaration the file can name would have taken, and compiled.
    "and a call the public one does not take is refused rather than reaching the private one" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n
           |use() -> string = skip_line("x")
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |""".stripMargin),
      ) should include("is int, but string was given")
    }

    // The other direction, and the one that refused a program outright once: the private declaration
    // was written first, and every other file was told the *public* name was private to a file it
    // had never heard of.
    "and a public declaration is found from a third file though a private one was written first" in {
      runIn(
        ("", "main.sysl", "print(m.use_c())"),
        ("m", "one.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |use_a() -> int = skip_line(1)
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |skip_line(s: string) -> string = s
           |""".stripMargin),
        ("m", "three.sysl",
         """module m
           |use_c() -> string = skip_line("y")
           |""".stripMargin),
      ) shouldBe "y\n"
    }

    // A private slot holds every declaration one file made of the contended spelling, so it carries
    // an overload set of its own. Filing them all under the slot itself dropped every one but the
    // last, and the call the dropped declaration took was then reported against the survivor.
    "and one file may overload the name it keeps to itself" in {
      runIn(
        ("", "main.sysl", "print(m.use_b(), m.use_b2())"),
        ("m", "one.sysl",
         """module m
           |private skip_line(b: bool) -> bool = b
           |use_a() -> bool = skip_line(true)
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |private skip_line(s: string) -> string = s
           |use_b() -> string = skip_line("x")
           |use_b2() -> int = skip_line(3)
           |""".stripMargin),
      ) shouldBe "x 3\n"
    }

    "and two of them a call could not tell apart are refused, under the name as written" in {
      val message = errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private skip_line(b: bool) -> bool = b
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |private skip_line(n: int) -> string = "x"
           |""".stripMargin),
      )

      message should include("'skip_line' is already declared")
      // The slot is the compiler's answer to a contended spelling, and no reader wrote it.
      message should not include "private1"
    }

    // The same, with a public declaration of the spelling in a third file: the private file's own
    // pair is still told apart among themselves, and the public one is in no set with them.
    "and the same beside a public declaration, which is in no set with them" in {
      val message = errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |private skip_line(n: int) -> string = "x"
           |""".stripMargin),
      )

      message should include("'skip_line' is already declared")
      message should not include "private1"
    }
  }
}
