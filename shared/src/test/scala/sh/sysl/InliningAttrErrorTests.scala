package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Every way of writing `@noinline`, `@inline` or `@cold` that means nothing, and the sentence each
 * gets (`reference/attributes.md § @noinline, @inline and @cold`).
 *
 * The three mark a **definition**, so most of the refusals divide by what there is to be a
 * definition of: a declaration of another kind has none, an `extern` has one the linker supplies,
 * and `@ghost` takes the definition away outright. The one that does not is `@noinline` beside
 * `@inline`, where the definition is in no doubt and the two requests about it are opposite.
 * `InliningAttrTests` is the other half.
 */
class InliningAttrErrorTests extends AnyFreeSpec with CodegenSupport {

  "none of the three takes an argument" - {

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

    // The one of the three where a number is the tempting thing to write, because every other
    // language's version of it has one somewhere. `inlinehint` raises what the inliner is willing
    // to spend and the amount is LLVM's, so there is nothing here for a program to name.
    "'@inline(200)'" in {
      err("@inline(200)\nf() -> int = 1\n") should
        include("'@inline' takes no arguments — it raises what the inliner will spend on this " +
          "definition rather than naming a number")
    }

    // An empty pair is refused by the same rule and for the same reason: there is no form that
    // takes parentheses, so `()` is not a shorter argument list but a different annotation.
    "and an empty pair is not the shorter form of any of them" in {
      err("@noinline()\nf() -> int = 1\n") should include("'@noinline' takes no arguments")
      err("@inline()\nf() -> int = 1\n") should include("'@inline' takes no arguments")
      err("@cold()\nf() -> int = 1\n") should include("'@cold' takes no arguments")
    }
  }

  "one written twice says nothing the once does not" in {
    err("@noinline\n@noinline\nf() -> int = 1\n") should
      include("'@noinline' is written twice above one declaration")
    err("@inline\n@inline\nf() -> int = 1\n") should
      include("'@inline' is written twice above one declaration")
    err("@cold\n@cold\nf() -> int = 1\n") should
      include("'@cold' is written twice above one declaration")
  }

  // The contradiction among the three, and the only refusal here that is not about there being no
  // definition: one forbids inlining and the other asks for it, so a definition carrying both is
  // held to neither.
  "'@noinline' beside '@inline' asks for opposite things" - {

    "written either way round" in {
      err("@noinline\n@inline\nf() -> int = 1\n") should
        include("'@noinline' forbids inlining and '@inline' asks for it, so they contradict above " +
          "one declaration")
      err("@inline\n@noinline\nf() -> int = 1\n") should
        include("so they contradict above one declaration")
    }

    // And the sentence sends the reader to the thing they probably meant, since "keep it out of the
    // hot path" and "do not inline it" are two requests that get confused for one.
    "and the sentence names '@cold', which is the different thing they may have meant" in {
      err("@noinline\n@inline\nf() -> int = 1\n") should
        include("'@cold' beside it if it is also reached rarely")
    }
  }

  "they mark a function, and only a function" - {

    "not a struct" in {
      err("@noinline\nstruct S\n    a: int\n") should include("an annotation marks a function")
    }

    "not a binding" in {
      err("@cold\nstatic var n: int = 1\nprint(n)\n") should include("an annotation marks a function")
    }

    "and not a struct with '@inline' above it either" in {
      err("@inline\nstruct S\n    a: int\n") should include("an annotation marks a function")
    }

    // An `extern` is a name and a signature: the definition is in somebody else's object file, and
    // a `declare` line carries no function attributes at all — so the mark would be a request made
    // of a compilation that never sees the body.
    "and not an 'extern', whose definition is not here" in {
      err("@noinline\nextern exit(code: int)\nprint(1)\n") should
        include("an annotation marks a function")
    }
  }

  // `@ghost` erases the function before anything is emitted, so there is no definition left to keep
  // out of line, to absorb into a caller, or to place away from the hot path.
  "'@ghost' leaves nothing for any of them to be about" - {

    "with '@noinline'" in {
      val e = err("@ghost\n@noinline\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n")

      e should include("'@noinline' tell the optimizer about a definition, and '@ghost' means there is none")
      e should include("erased before anything is emitted")
    }

    "with '@cold'" in {
      err("@ghost\n@cold\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n") should
        include("'@cold' tell the optimizer about a definition")
    }

    "with '@inline'" in {
      val e = err("@ghost\n@inline\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n")

      e should include("'@inline' tell the optimizer about a definition")
      e should include("to absorb into a caller")
    }

    "and with two of them, which the sentence names together" in {
      err("@ghost\n@noinline\n@cold\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n") should
        include("'@noinline' and '@cold' tell the optimizer about a definition")
      err("@ghost\n@inline\n@cold\nsorted(n: int) -> bool = n > 0\n\nprint(1)\n") should
        include("'@inline' and '@cold' tell the optimizer about a definition")
    }
  }

  // The list a misspelling is answered with has to hold them, or the reader is told the word they
  // reached for is not an annotation while the language has it.
  "a misspelling is answered with a list that holds them" in {
    val e = err("@noinlne\nf() -> int = 1\n")

    e should include("'@noinline'")
    e should include("'@inline'")
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
          |""".stripMargin) should include("'@noinline', '@inline' and '@cold'")
  }
}
