package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec

/** `c type` — the sysl type a **C typedef** turns out to be (`reference/ffi.md § A library may
 * carry C`).
 *
 * Every assertion here that says how wide something is reads that width from the C compiler as well,
 * in the same probe, and asserts the two **agree**. That is deliberate and it is the whole point of
 * the feature: a test saying `Tick` is `u64` against a literal `u64` would be the transcription this
 * exists to abolish, written one layer up — and it would fail on the next machine, which is exactly
 * the failure `c type` is for. `CConstTests` is the value half of the same claim and the same rule.
 *
 * The oracle for a signedness is C's own `((T)-1) < 0`, and for a width C's own `sizeof(T)`. Both are
 * measured beside the type, so a case says "sysl agreed with C here" rather than "sysl said 8".
 */
class CTypeTests extends AnyFreeSpec with CodegenSupport with RunSupport with ParseSupport {

  "the block parses" - {
    "as its types, each a name and a C type in quotes" in {
      prog("c type\n    Tick = \"TickType_t\"\n    Stack = \"configSTACK_DEPTH_TYPE\"\n") shouldBe
        List(CTypeBlock(List(
          CTypeDecl("Tick", "TickType_t"),
          CTypeDecl("Stack", "configSTACK_DEPTH_TYPE"))))
    }

    /** The modifier is written on the header and governs the lines under it, exactly as a `c const`
      * block's does — it is the one place there is to write one.
      */
    "and a visibility on the header reaches every type in it" in {
      prog("private c type\n    Tick = \"TickType_t\"\n") shouldBe
        List(CTypeBlock(List(CTypeDecl("Tick", "TickType_t", vis = Visibility.File))))
    }

    /** `c` stays an ordinary name, which the keyword after it is what makes safe. `c const` pins the
      * same claim; this pins that a second keyword did not take the word away.
      */
    "and 'c' is still an ordinary name" in {
      run("var c = 3\nc += 1\nprint(str(c))\n") shouldBe "4\n"
    }

    "a block with nothing under it is refused with the form" in {
      progError("c type\n") should include("'c type' is followed by its types")
    }
  }

  /** The measurement itself, asked of C twice over: what sysl resolved the typedef to, against what
    * C says its width and its signedness are.
    */
  "the type is the one the C compiler describes" - {
    for (c, header) <- List(
        "char"               -> None,
        "unsigned long"      -> None,
        "short"              -> None,
        "long long"          -> None,
        "size_t"             -> Some("<stddef.h>"),
        "int_least16_t"      -> Some("<stdint.h>"),
        "wchar_t"            -> Some("<stddef.h>"),
      )
    do
      s"'$c' resolves to the integer C's own 'sizeof' and sign test describe" in {
        val (width, signed, resolved) = measured(c, header)

        resolved shouldBe (if signed then s"i${width * 8}" else s"u${width * 8}")
      }
  }

  /** A typedef is a second name for one integer and nothing else, so a value flows both ways with no
    * cast — which is what makes an `extern` written against one an ordinary declaration.
    */
  "a measured type is used as the integer it is" - {
    "a signed one takes a negative value" in {
      run("""c type
            |    S = "signed char"
            |
            |val x: S = -1
            |
            |print(str(x))
            |""".stripMargin) shouldBe "-1\n"
    }

    "and arithmetic on it needs no cast, because it is that integer" in {
      run("""@include("<stddef.h>")
            |
            |c type
            |    Size = "size_t"
            |
            |var total: Size = 0
            |
            |for i in 0..<4
            |    total += 2
            |
            |print(str(total))
            |""".stripMargin) shouldBe "8\n"
    }

    /** The case the card was filed for: the width reaches a **signature**, where getting it wrong is
      * not a size mismatch anything can see. The claim is agreement again — the type sysl gave the
      * parameter is as wide as C says the typedef is.
      */
    "and it may be a parameter of an extern, which is what it exists for" in {
      run("""@include("<string.h>")
            |
            |c const
            |    W: usize = "sizeof(size_t)"
            |
            |c type
            |    Size = "size_t"
            |
            |@assert(sizeof(Size) == W, "a 'c type' must be as wide as C says the typedef is")
            |
            |extern "strlen" c_strlen(s: *u8) -> Size
            |
            |print(str(sizeof(Size) == W))
            |""".stripMargin) shouldBe "true\n"
    }

    /** **Its bounds are the measured integer's**, asked of C in the same program so that the claim
      * is agreement rather than transcription. A `c type` is the one transparent subtype carrying
      * no `within` range at all, so this is the case that says `reference/errors.md § Constrained
      * types` holds for the attributes too: a measured `size_t` *is* the integer it turned out to
      * be, and asking its maximum asks about that integer.
      */
    "and its maximum is the maximum of the integer C said it is" in {
      run("""@include("<stddef.h>")
            |
            |c const
            |    W: usize = "sizeof(size_t)"
            |
            |c type
            |    Size = "size_t"
            |
            |print(str(u128(Size::Max) == (1u128 << u128(W * 8)) - 1), str(Size::Min == 0))
            |""".stripMargin) shouldBe "true true\n"
    }

    /** The same bound through a **type parameter**, which is the route a library takes. `10` solves a
      * parameter to the type that was *written*, so the annotation on the binding puts `Size` in `T`
      * and the answer is `Size`'s — the measured integer's, again.
      */
    "and a generic solved from it answers the same bound" in {
      run("""@include("<stddef.h>")
            |
            |c const
            |    W: usize = "sizeof(size_t)"
            |
            |c type
            |    Size = "size_t"
            |
            |widest[T]() -> T = T::Max
            |
            |val m: Size = widest()
            |
            |print(str(u128(m) == (1u128 << u128(W * 8)) - 1))
            |""".stripMargin) shouldBe "true\n"
    }

    /** The measured type reaches C in the other direction too. An `@export`ed function taking one is
      * an ordinary export — `ExportCheck.crosses` reads a constrained subtype as its base — and the
      * header spells the integer rather than the C name it came from, which is right: the typedef
      * was measured for *this* target, and re-spelling it would hand the consumer a name whose width
      * their own headers decide again.
      */
    "and an exported function spells it in its header as the integer it is" in {
      val (width, signed, _) = measured("size_t", Some("<stddef.h>"))
      val src =
        """module demo
          |@include("<stddef.h>")
          |
          |c type
          |    Size = "size_t"
          |
          |@export
          |take(n: Size) -> Size = n
          |""".stripMargin
      val exports = Compiler.compiled(List(Source("<input>", src))) match
        case Right(c) => c.exports
        case Left(e)  => fail(s"the export did not compile: $e")

      CHeader.render(exports, "demo") should
        include(s"${if signed then "" else "u"}int${width * 8}_t take(")
    }

    /** **A value computed in sysl reaches the type through the type's own name**, which is the only
      * spelling that can be portable: the measured base is `size_t` here and something else on the
      * next target, so `u64(n)` would be this machine's answer written into the program. `usize` is
      * a distinct type from whatever C measured, so nothing arrives at the base by itself — and a
      * length, a `sizeof` and any arithmetic over them are exactly the values a binding has to hand
      * back to C.
      *
      * Filed as `0132` from the FreeRTOS binding, where the kernel's stack depth is a
      * `configSTACK_DEPTH_TYPE` and what has to go in it is a slice's `len`.
      */
    "and a value computed in sysl converts into it under its own name" in {
      run("""@include("<stddef.h>")
            |
            |c type
            |    Size = "size_t"
            |
            |take(n: Size) -> Size = n
            |
            |var xs: [3]u32 = [0, 0, 0]
            |
            |print(str(take(Size(xs.len)) + Size(sizeof(u32))))
            |""".stripMargin) shouldBe "7\n"
    }

    /** The conversion is *written*, and this is what says so. A width that differs by target is the
      * whole reason the type exists, so a `usize` sliding into one unannounced would be a narrowing
      * that is silent here and lossy under a 16-bit typedef — the mistake `c type` was built to
      * catch.
      */
    "while an unwritten one is still refused" in {
      val e = err("""@include("<stddef.h>")
                             |
                             |c type
                             |    Size = "size_t"
                             |
                             |take(n: Size) -> Size = n
                             |
                             |var n: usize = 3
                             |
                             |print(str(take(n)))
                             |""".stripMargin)

      e should include("Size")
      e should include("usize")
    }

    /** `_Bool` is the one answer that is not an integer and is still resolved, because sysl's `bool`
      * is what C means by it — `CAbi` already crosses one as a single unsigned byte.
      */
    "and a C '_Bool' arrives as sysl's 'bool'" in {
      run("""c type
            |    Flag = "_Bool"
            |
            |val yes: Flag = true
            |
            |print(str(yes))
            |""".stripMargin) shouldBe "true\n"
    }
  }

  /** **The claim the whole mechanism rests on**: the answer is the *target's*, not this machine's.
    * Nothing runs, so nothing here could have been right by accident — `long` is eight bytes on this
    * machine and four on the 32-bit ones, and the array the type sizes says which the compiler
    * believed.
    */
  "the answer is the target's rather than the host's" - {
    val src =
      """c type
        |    Long = "long"
        |
        |var xs: [1]Long = [0]
        |
        |print(str(xs.len))
        |""".stripMargin

    for (name, bits) <- List(Target.default.name -> 64, "thumbv7em-freestanding" -> 32) do
      s"$name lays the array out as i$bits" in {
        val target = Target.named(name).getOrElse(cancel(s"no target named '$name'"))

        Toolchain.findClang(target).getOrElse(cancel(s"no clang here has a back end for ${target.name}"))

        irFor(target, src) should include(s"[1 x i$bits]")
      }
  }

  /** A vendored header is where a binding's typedefs actually live, and an enum is the shape whose
    * signedness nobody can predict — C leaves it to the implementation, which is the reason to ask.
    */
  "a typedef out of a header beside the module is measured like any other" - {
    "including an enum, whose signedness is the C compiler's choice" in {
      val dir = createTempDirectory("sysl-ctype-pkg-")

      try
        writeFile(s"$dir/levels.h",
          "typedef enum { LOW, HIGH } level_t;\ntypedef enum { COLD = -1, HOT = 1 } trend_t;\n")

        for (c, name) <- List("level_t" -> "Level", "trend_t" -> "Trend") do
          val (width, signed, resolved) = measured(c, Some("levels.h"), Some(dir.toString), name)

          resolved shouldBe (if signed then s"i${width * 8}" else s"u${width * 8}")
      finally Project.discard(s"$dir/levels.h")
    }
  }

  /** One file, one probe — a `c const` block and a `c type` block are one question put to the C
    * compiler, which is why they are blocks at all. What is asserted is that both answers come back
    * from the file that wrote them; the cost claim itself is `CProbe`'s and has nothing to observe.
    */
  "a file writing both blocks is measured once" in {
    run("""@include("<limits.h>")
          |
          |c const
          |    BITS: u32 = "CHAR_BIT"
          |
          |c type
          |    Small = "short"
          |
          |val x: Small = 3
          |
          |print(str(BITS) + " " + str(x))
          |""".stripMargin) shouldBe "8 3\n"
  }

  /** The edges, each found by asking what a second occurrence or another file does. */
  "the edges" - {
    /** Two blocks are consumed **in order** rather than looked up by name, so a bug here does not
      * refuse anything — it hands one line another line's answer, which compiles and is wrong. The
      * two types are of different widths precisely so that a swap is visible.
      */
    "two blocks in one file each keep their own answer" in {
      run("""c type
            |    Wide = "long long"
            |
            |c type
            |    Narrow = "signed char"
            |
            |print(str(sizeof(Wide) > sizeof(Narrow)))
            |""".stripMargin) shouldBe "true\n"
    }

    /** The same again with the blocks interleaved the other way round, since the two kinds are
      * replaced from two queues walking one body.
      */
    "a 'c type' before a 'c const' is measured as readily as one after it" in {
      run("""@include("<limits.h>")
            |
            |c type
            |    Small = "short"
            |
            |c const
            |    BITS: u32 = "CHAR_BIT"
            |
            |val x: Small = 3
            |
            |print(str(BITS) + " " + str(x))
            |""".stripMargin) shouldBe "8 3\n"
    }

    /** **The reason a `c type` declares a name rather than being substituted through its own file.**
      * A module is written across files, and a binding puts its typedefs in one of them; a type that
      * existed only in the file that measured it would be a surprise nothing else in the language
      * has, and would make the block useless for the arrangement it was built for.
      */
    for (how, main) <- List(
        "by the qualified path" -> "val n: ticks.Size = 41\n\nprint(str(n + 1))\n",
        "and by an import"      -> "import ticks.Size\n\nval n: Size = 41\n\nprint(str(n + 1))\n",
      )
    do
      s"a type measured in one file of a module is a type in the next file, $how" in {
        runIn(
          ("ticks", "ticks.sysl",
            """module ticks
              |@include("<stddef.h>")
              |
              |c type
              |    Size = "size_t"
              |""".stripMargin),
          ("", "main.sysl", main),
        ) shouldBe "42\n"
      }

    /** And the visibility written on the header is the ordinary one, enforced where every other
      * declaration's is — the block does not invent a rule of its own.
      */
    "a 'private' one does not leave its file" in {
      val message = errIn(
        ("ticks", "ticks.sysl",
          """module ticks
            |@include("<stddef.h>")
            |
            |private c type
            |    Size = "size_t"
            |""".stripMargin),
        ("", "main.sysl",
          """val n: ticks.Size = 1
            |
            |print(str(n))
            |""".stripMargin),
      )

      message should not be empty
    }

    /** A measured type shares the type namespace, which is what makes a collision with a struct the
      * duplicate it is rather than a shadowing nobody wrote.
      */
    "a name a struct already has is refused" in {
      err("""struct Size
            |    n: usize
            |
            |c type
            |    Size = "size_t"
            |
            |print(1)
            |""".stripMargin) should not be empty
    }
  }

  /** **The motivating shape is a package, not a program**, so the artifact path is the one that has
    * to work. A library is measured when it is *built* and ships the resolved type, which is what
    * lets a program link a binding without the library's headers — and is the only honest
    * arrangement anyway, since an artifact is built for one target.
    */
  "a library ships the measured type rather than the C name" - {
    val source = List(Source("demo/lib.sysl",
      """module demo
        |@include("<stddef.h>")
        |
        |c type
        |    Size = "size_t"
        |
        |zero() -> Size = 0
        |""".stripMargin, List("demo")))

    lazy val trees: List[Program] =
      LibraryArtifact.build(source) match
        case Left(e) => fail(s"the library did not build: $e")
        case Right((_, meta)) =>
          LibraryArtifact.read("demo.syslib", meta, Target.default) match
            case Left(e)              => fail(s"the metadata did not read back: $e")
            case Right((units, _, _)) => units

    "the artifact carries a type declaration naming a sysl integer" in {
      val bases =
        trees.flatMap(_.body).collect { case TypeDecl("Size", NamedType(b, _), _, _, _, _, true, _) => b }

      bases.map(_.head) shouldBe List('u')
    }

    /** Compiled against the **decoded** trees, which is the path a package takes: the program never
      * sees `<stddef.h>` and never invokes a C compiler, because the question was settled when the
      * library was built.
      */
    "and a program uses it with no header and no C compiler of its own" in {
      Compiler.compiledWith(List(Source("main.sysl", "print(str(demo.zero()))\n")), trees) match
        case Left(e)  => fail(s"the program did not compile against the artifact: $e")
        case Right(c) => c.ir should include("main")
    }
  }

  "what it refuses" - {
    /** A pointer, a float, a struct and an array all reach `_Generic`'s `default` arm, which is what
      * makes the refusal a sysl diagnostic naming the type rather than a C error naming a file the
      * programmer never wrote.
      */
    for c <- List("double", "void *", "struct { int a; }") do
      s"'$c', which is not an integer and has an answer of its own in sysl" in {
        val message = err(s"""c type
                             |    T = "$c"
                             |
                             |print(1)
                             |""".stripMargin)

        message should include("is not an integer type")
        message should include(c)
      }

    "a C type that does not exist, in the C compiler's own words" in {
      val message = err("""c type
                          |    T = "no_such_type_at_all_t"
                          |
                          |print(1)
                          |""".stripMargin)

      message should include("the C compiler refused")
      message should include("'c type' block")
    }

    "a header that is not there" in {
      err("""@include("no_such_header_at_all.h")
            |
            |c type
            |    T = "int"
            |
            |print(1)
            |""".stripMargin) should include("no_such_header_at_all.h")
    }

    "an unsigned type still refuses a negative value, since it is that integer" in {
      err("""c type
            |    U = "unsigned char"
            |
            |val x: U = -1
            |
            |print(str(x))
            |""".stripMargin) should not be empty
    }

    "a block inside a body, which has no file's headers to be compiled against" in {
      err("""f() -> int
            |    c type
            |        T = "int"
            |    1
            |
            |print(str(f()))
            |""".stripMargin) should include("'c type' block is declared at the top level")
    }

    "and a name declared twice is refused as the duplicate it is" in {
      err("""c type
            |    T = "int"
            |
            |c type
            |    T = "long"
            |
            |print(1)
            |""".stripMargin) should not be empty
    }

    /** A hand-written `type` with no constraint is a transparent **alias** and compiles, which it did
      * not until the deferral `16` carried was closed. The two forms are spelled almost identically
      * and mean different things, so both are asserted here rather than only the one this file is
      * about: `c type` declares a scalar whose *width* the C compiler answered for, and `type` with
      * no constraint declares no type at all.
      */
    "while a hand-written alias with no constraint is now a transparent alias" in {
      run("""type Tick = u32
            |
            |val x: Tick = 3
            |
            |print(str(x))
            |""".stripMargin) shouldBe "3\n"
    }
  }

  /** A compilation with no block never asks for a clang, which `CConstTests` pins for the value
    * half; this is the same claim for a file whose only block is a `c type`.
    */
  "a file with neither block leaves its units untouched" in {
    val units = List(SyslParser.parse(Source("<input>", "print(1)\n"), Target.default).toOption.get)

    CProbe.lower(units, Target.default) shouldBe Right(units)
  }

  /** What C says about a type, and what sysl made of it: the width, the signedness, and the name of
    * the integer the typedef lowered to — all three out of one probe, so that a case can assert they
    * agree instead of asserting a number.
    */
  private def measured(c: String, header: Option[String], dir: Option[String] = None,
                       name: String = "T"): (Int, Boolean, String) = {
    val include = header.map(h => s"""@include("$h")\n""").getOrElse("")
    val src =
      s"""$include
         |c const
         |    WIDTH: usize = "sizeof($c)"
         |    SIGNED: i32 = "((($c)-1) < 0)"
         |
         |c type
         |    $name = "$c"
         |""".stripMargin
    val file = dir.map(d => s"$d/probe.sysl").getOrElse("<input>")
    val unit = SyslParser.parse(Source(file, src), Target.default) match
      case Left(e)  => fail(s"the probe source did not parse: $e")
      case Right(u) => u

    CProbe.lower(List(unit), Target.default) match
      case Left(e) => fail(s"'$c' was not measured: $e")
      case Right(units) =>
        val body     = units.head.body
        val consts   = body.collect { case ConstDecl(n, _, IntLit(v, _), _) => n -> v }.toMap
        val resolved = body.collect { case TypeDecl(n, NamedType(b, _), _, _, _, _, true, _) if n == name => b }

        (consts("WIDTH").toInt, consts("SIGNED") != BigInt(0), resolved.head)
    }
}
