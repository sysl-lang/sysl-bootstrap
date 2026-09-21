package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Every way of writing `@noinline` or `@cold` that means nothing, and the sentence each gets
 * (`reference/attributes.md § @noinline and @cold`).
 *
 * The two mark a **definition**, so the refusals divide by what there is to be a definition of: a
 * declaration of another kind has none, an `extern` has one the linker supplies, and `@ghost` takes
 * the definition away outright. `InliningAttrTests` is the other half.
 */
class InliningAttrErrorTests extends AnyFreeSpec with CodegenSupport {

  "neither takes an argument" - {

    // The parenthesis is answered rather than left unread, for a hook's reason: the word has been
    // read, so there is nothing else the line could have been, and leaving the `(` sends the
    // statement rule on to refuse the perfectly ordinary declaration below it.
    "'@noinline(3)'" in {
      err("@noinline(3)\nf() -> int = 1\n") should
        include("'@noinline' takes no arguments — whether a call stays a call is the whole of what it says")
    }

    "'@cold(1)'" in {
      err("@cold(1)\nf() -> int = 1\n") should
        include("'@cold' takes no arguments — how rare a rare path is is not a number a program has")
    }

    // An empty pair is refused by the same rule and for the same reason: there is no form that
    // takes parentheses, so `()` is not a shorter argument list but a different annotation.
    "and an empty pair is not the shorter form of either" in {
      err("@noinline()\nf() -> int = 1\n") should include("'@noinline' takes no arguments")
      err("@cold()\nf() -> int = 1\n") should include("'@cold' takes no arguments")
    }
  }

  "one written twice says nothing the once does not" in {
    err("@noinline\n@noinline\nf() -> int = 1\n") should
      include("'@noinline' is written twice above one declaration")
    err("@cold\n@cold\nf() -> int = 1\n") should
      include("'@cold' is written twice above one declaration")
  }

  "they mark a function, and only a function" - {

    "not a struct" in {
      err("@noinline\nstruct S\n    a: int\n") should include("an annotation marks a function")
    }

    "not a binding" in {
      err("@cold\nstatic var n: int = 1\nprint(n)\n") should include("an annotation marks a function")
    }

    // An `extern` is a name and a signature: the definition is in somebody else's object file, and
    // a `declare` line carries no function attributes at all — so the mark would be a request made
    // of a compilation that never sees the body.
    "and not an 'extern', whose definition is not here" in {
      err("@noinline\nextern exit(code: int)\nprint(1)\n") should
        include("an annotation marks a function")
    }
  }

  // The one contradiction among the marks rather than among the kinds of declaration: `@ghost`
  // erases the function before anything is emitted, so there is no definition left to keep out of
  // line or to place away from the hot path.
  "'@ghost' leaves nothing for either to be about" - {

    "with '@noinline'" in {
      val e = err("@ghost\n@noinline\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n")

      e should include("'@noinline' tell the optimizer about a definition, and '@ghost' means there is none")
      e should include("erased before anything is emitted")
    }

    "with '@cold'" in {
      err("@ghost\n@cold\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n") should
        include("'@cold' tell the optimizer about a definition")
    }

    "and with both, which the sentence names together" in {
      err("@ghost\n@noinline\n@cold\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n") should
        include("'@noinline' and '@cold' tell the optimizer about a definition")
    }
  }

  // The list a misspelling is answered with has to hold them, or the reader is told the word they
  // reached for is not an annotation while the language has it.
  "a misspelling is answered with a list that holds them" in {
    val e = err("@noinlne\nf() -> int = 1\n")

    e should include("'@noinline'")
    e should include("'@cold'")
  }

  // A member may carry them — they are about the function it lowers to — so the sentence that
  // refuses the rest has to say so, or it sends a reader to write a wrapper they do not need.
  "and the member refusal names them among what a member may carry" in {
    err("""struct S
          |    v: int
          |
          |    @tailrec
          |    take(self, n: int) -> int = n
          |
          |print(1)
          |""".stripMargin) should include("'@noinline' and '@cold'")
  }
}
