package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A diagnostic names a declaration as the source spells it, never as the key a table holds it by.
 *
 * A declaration in a module is filed under `<module>$<name>` (`Modules.qualify`), and a message is
 * written by interpolating whatever name is in hand — in the analyzer, nearly always that key. So a
 * module file's mistakes were reported against `'m$use'`, `'m$hid'` and `'m$P.get'`: names nobody
 * can write, which a reader has to decode before they can find the line. `Modules.readable` reads
 * every quoted key back in `Diagnostic`'s constructor, which is the one place all of them pass.
 */
class DiagnosticNameTests extends AnyFreeSpec with Matchers with CodegenSupport {

  /** The first line of the report — the message itself, without the source excerpt under it. */
  private def headline(fs: (String, String, String)*): String = errIn(fs*).linesIterator.next()

  "a module file's declarations are named as they are written" - {

    "a function" in {
      headline(
        ("", "main.sysl", "print(1)"),
        ("m", "m.sysl",
         """module m
           |use() -> string = 1
           |""".stripMargin),
      ) shouldBe "error: function 'use' should return string, but its body yields int"
    }

    "a private function" in {
      headline(
        ("", "main.sysl", "print(1)"),
        ("m", "m.sysl",
         """module m
           |private hid() -> string = 2
           |""".stripMargin),
      ) shouldBe "error: function 'hid' should return string, but its body yields int"
    }

    // A private declaration of a spelling another file of the module declares publicly is filed
    // under a slot of its own, `m$hid.private1`, and the slot is the compiler's, not the reader's.
    "a private function that shadows a sibling file's public one" in {
      headline(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |hid() -> int = 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private hid() -> string = 2
           |""".stripMargin),
      ) shouldBe "error: function 'hid' should return string, but its body yields int"
    }

    "a method" in {
      headline(
        ("", "main.sysl", "print(1)"),
        ("m", "m.sysl",
         """module m
           |struct P
           |    x: int
           |
           |    get(self) -> string = self.x
           |""".stripMargin),
      ) shouldBe "error: function 'P.get' should return string, but its body yields int"
    }

    "a type a method call is refused on" in {
      headline(
        ("", "main.sysl", "print(1)"),
        ("m", "m.sysl",
         """module m
           |struct P
           |    x: int
           |
           |f() -> int = P(1).nope()
           |""".stripMargin),
      ) shouldBe "error: type 'P' has no method 'nope'"
    }

    "a generic function marked '@export'" in {
      headline(
        ("", "main.sysl", "print(1)"),
        ("m", "m.sysl",
         """module m
           |@export
           |gen[T](x: T) -> T = x
           |""".stripMargin),
      ) should startWith("error: an exported symbol is one function at one signature, so 'gen' " +
        "cannot be generic")
    }
  }

  // The four the site quotes, each of which printed a library key before.
  "the library's declarations are named the same way" - {

    "an argument of a generic method's instantiation" in {
      err("import sysl.buf.{Buf, buf}\nvar b: Buf[int] = buf()\nb.push(\"x\")").linesIterator.next() shouldBe
        "error: 'v' of 'Buf.push.int' is int, but string was given"
    }

    "an argument of a method" in {
      err("import sysl.text.str_builder\nvar b = str_builder()\nvar k: int = 42\nb.push_int(k)")
        .linesIterator.next() shouldBe "error: 'n' of 'StrBuilder.push_int' is long, but int was given"
    }

    "a property called as a method" in {
      err("import sysl.text.str_builder\nvar b = str_builder()\nb.push(\"hello\")\nprint(b.len())")
        .linesIterator.next() shouldBe
        "error: 'len' is a property of 'StrBuilder' — read it as 'value.len', without '()'"
    }

    "a method a type does not have" in {
      err("import sysl.fs.open\nvar f = open(\"/tmp/x.txt\")\nf.close()").linesIterator.next() shouldBe
        "error: type 'Result' has no method 'close'"
    }
  }

  "the root module's declarations were never keyed with a module, and read the same" in {
    err("use() -> string = 1\nprint(1)").linesIterator.next() shouldBe
      "error: function 'use' should return string, but its body yields int"
  }

  "reading a message back" - {

    "takes the module and the compiler's segments off a quoted key" in {
      Modules.readable("'m$use' and 'a.b$P.get' and 'm$hid.private1' and 'm$pick.private1.1'") shouldBe
        "'use' and 'P.get' and 'hid' and 'pick'"
    }

    "reads back a '$' a quoted name contributed" in {
      Modules.readable(s"'m$$price$$24total'") shouldBe "'price$total'"
    }

    "leaves alone what is not a key" in {
      val untouched = List(
        "expected a name or '{' after '$'",
        "'$name' is an interpolation",
        "'m.use' is spelled with its module",
        "'price 24$' is not a module path",
        "'a b$c' is not a module path either",
        "'m$' names nothing after the separator",
        "'m$24x' begins with a guard mark, not a name",
        "an unclosed 'm$use",
      )

      for msg <- untouched do withClue(msg)(Modules.readable(msg) shouldBe msg)
    }

    "is what a diagnostic carries as data, not only what it renders" in {
      Diagnostic("function 'm$use' is wrong", None).message shouldBe "function 'use' is wrong"
    }
  }
}
