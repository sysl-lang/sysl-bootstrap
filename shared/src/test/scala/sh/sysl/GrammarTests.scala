package sh.sysl

import io.github.edadma.highlighter.Highlighter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The syntax-highlighting grammar, checked against the lexer it is supposed to describe.
 *
 * `grammars/sysl.tmLanguage.json` is a second, hand-written statement of which words are keywords,
 * and nothing makes the two agree. A word added to `SyslLexical.reserved` is a keyword the compiler
 * honours and every renderer sets as an ordinary identifier — which looks like a line that simply has
 * little to highlight rather than like a fault, so it survives being looked at. `ref` did exactly
 * that: reserved, taught in the tour, and unstyled on the published page.
 *
 * **This test lives here because `SyslLexical` does.** The grammar was in `sysl.sh` until `weave`
 * needed it, and the arrangement had the test able to see the grammar but not the lexer it is a claim
 * about — it worked only because the site's suite happened to depend on the compiler. The site now
 * fetches the grammar the same way its CI already fetches the library.
 *
 * **What is checked is the compiled-in constant rather than the file**, so what the assertions are
 * about is the grammar that actually ships inside `weave` — a file read off disk could be right while
 * a stale generated constant shipped beside it.
 *
 * Both directions are checked against the grammar's **`keyword` section only**. The predeclared
 * scalars are styled from a section of their own and are deliberately *not* reserved words — that is
 * what lets the open `iN` / `uN` families need no lexical support — so a check spanning the whole
 * file would have to allow them and would stop meaning anything.
 */
class GrammarTests extends AnyFreeSpec with Matchers {

  /** One entry of the grammar's repository, as text. */
  private def section(name: String): String = {
    val text  = Grammar.sysl
    val start = text.indexOf(s"\"$name\": {")

    withClue(s"the grammar has no '$name' section\n") { start should be >= 0 }

    val end = text.indexOf("\n    },", start)

    end should be > start

    text.substring(start, end)
  }

  /** Every regex that section styles with, concatenated, with the regex escapes blanked out.
    *
    * Blanking matters: the patterns arrive still escaped as they appear in the JSON, so `self` is
    * written `\\bself\\b` and is preceded by the *letter* `b`. Any word-boundary reasoning done
    * against the raw text is reasoning about backslashes, and quietly finds nothing.
    *
    * `comment` and `name` fields are left out, because the comment beside a keyword group nearly
    * always names the very words in it — which is exactly how this kind of test passes by accident.
    */
  private def styledIn(sectionText: String): Set[String] = {
    val field = """"(?:match|begin|end)"\s*:\s*"((?:[^"\\]|\\.)*)"""".r
    val text = field
      .findAllMatchIn(sectionText)
      .map(_.group(1))
      .mkString(" ")
      .replaceAll("""\\\\.""", " ")

    """[a-z_]{2,}""".r.findAllMatchIn(text).map(_.matched).toSet
  }

  private lazy val asKeyword: Set[String] = styledIn(section("keyword"))

  /** `true`, `false` and `null` are reserved words that the grammar styles as constants rather than
    * as keywords, which is the right call — they are values, and colouring them like `if` would say
    * something false about them. So a reserved word is satisfied by either section.
    */
  private lazy val asConstant: Set[String] = styledIn(section("constant"))

  /** Words the grammar styles as keywords without the lexer reserving them. They are keywords only
    * where the grammar expects one and ordinary identifiers everywhere else, so `SyslLexical` never
    * sees them. Adding to this set is a decision about the language, not a way to fix a red test.
    *
    * **`opaque` and `derives` are here because the parser reads them with `softWord`**, in
    * `DeclParser` — so the decision was taken when they were written, and the grammar catching up
    * with it (`71436553`) is what left this list behind. That commit brought `grammars/` into line
    * with the site's copy and turned this half of the reconciliation red on dev; the words are soft
    * keywords in fact, so the list is what was wrong.
    *
    * **`become` joined them in 0.0.86 and the same thing happened again**, which is what makes this
    * a pattern rather than one stale line. `StmtParser.becomeStmt` reads the word with `softWord`,
    * so it is soft by construction — a variable and a function may both still be called `become`.
    * The grammar was taught to colour it and this list was not, and the two are in different files
    * *and different repositories*: `sysl.sh` carries a copy of the grammar with a `soft` set of its
    * own, so a word can be right in three places and wrong in the fourth. Adding a soft keyword
    * means editing the grammar here, this set, and the site's `GrammarTests` — and CI diffs the two
    * grammar files against each other, so the copy is the half that announces itself.
    */
  private val soft =
    Set("is", "not", "invariant", "new", "within", "where", "with", "opaque", "derives", "become")

  "the highlighting grammar" - {

    "is the one the compiler carries" in {
      // The constant is generated from the file by the build, so an edit to the grammar that never
      // reached a rebuild would otherwise be invisible to everything below.
      Grammar.sysl should include("source.sysl")
      Grammar.sysl should include("\"keyword\": {")
    }

    "styles every word the lexer reserves" in {
      val styled  = asKeyword ++ asConstant
      val missing = new SyslLexical().reserved.toList.sorted.filterNot(styled)

      withClue(s"reserved by SyslLexical but unstyled by the grammar: ${missing.mkString(", ")}\n") {
        missing shouldBe empty
      }
    }

    // The disagreement half. Without it the test above would pass on a grammar whose keyword section
    // had been replaced by one alternation listing every lowercase word imaginable — which would
    // style correctly and prove nothing — and it is also what notices a word going the other way,
    // when the lexer stops reserving something the grammar goes on colouring.
    "and styles nothing else, beyond the soft keywords" in {
      val unknown = (asKeyword -- new SyslLexical().reserved -- soft).toList.sorted

      withClue(s"styled as a keyword by the grammar but neither reserved nor soft: " +
        s"${unknown.mkString(", ")}\n") {
        unknown shouldBe empty
      }
    }

    // What `weave` actually does with it. The reconciliation above is about the word list; this is
    // about the file being a grammar at all, which no amount of agreement with the lexer implies.
    "and parses as one" in {
      io.github.edadma.highlighter.Highlighter.fromJson(Grammar.sysl) match
        case Left(err) => fail(s"the grammar does not parse: $err")
        case Right(highlighter) =>
          highlighter.highlight("val x = 1") should include("""<span class="hl-keyword">val</span>""")
    }
    // **The reconciliation above is about WORDS. The grammar also carries identifier PATTERNS, and
    // nothing checked those at all** — so an ASCII character class in a language whose identifiers
    // are Unicode's letters (`reference/lexical.md § Identifiers`) was invisible here, and what it
    // produces is a page where `struct Círculo` renders as an unstyled word. That reads as a line
    // with little to highlight rather than as a fault, which is the same failure the reserved-word
    // half exists for, one construct over.
    //
    // **Asked of the renderer rather than of a regex engine**, which is the only way the question is
    // well posed: a TextMate grammar is matched by Oniguruma (`io.github.edadma.oniguruma`, through
    // `highlighter`), and that is what `Weave` uses here and what juicer uses on the site. Compiling
    // these patterns with some *other* engine tests that engine — its `\b`, its Unicode classes, its
    // lookaround support — and says nothing about what a reader sees.
    //
    // It also makes the claim end-to-end: what is asserted is the SCOPE a name is styled with, which
    // is the thing the page shows, rather than that some pattern matched some substring.
    // `Grammar.sysl` rather than the file on disk: it is what `build.sbt` embeds and therefore what
    // `Weave` actually renders with, and a generated copy that had drifted from its source is a
    // different defect from this one.
    def styled(code: String, name: String): List[String] =
      Highlighter.fromJson(Grammar.sysl) match
        case Left(why) => fail(s"the grammar did not load, so nothing could be styled: $why")
        case Right(h)  =>
          // Trimmed, because a capture may take the space in front of the name with it — `end`'s
          // second group is `\s+<name>` — and the scope is still the one on that name.
          h.tokens(code).flatten.filter(_.text.trim == name).flatMap(_.scopes).distinct

    // **THE ONE THAT WOULD HAVE CAUGHT THE WORST OF THIS, AND WHICH NOTHING ASKED.** A pattern the
    // engine cannot compile does not stop the grammar loading: `fromJson` still answers `Right`, the
    // pattern is dropped, and everything it would have styled comes out bare. So a grammar that
    // highlights **nothing** loads exactly as cleanly as one that works, and the only record is a
    // list nobody read.
    //
    // Written after doing precisely that: widening the identifier classes to `\p{Nd}` and `\p{Lu}`
    // — which `java.util.regex` accepts and this engine does not — silently unstyled every
    // declaration, call and type in `sysl weave` and on the whole site.
    "loads with every pattern compiled, since one that does not is dropped in silence" in {
      Highlighter.fromJson(Grammar.sysl) match
        case Left(why) => fail(s"the grammar did not load at all: $why")
        case Right(h)  =>
          withClue(s"patterns the engine refused:\n  ${h.loadWarnings.mkString("\n  ")}\n") {
            h.loadWarnings shouldBe empty
          }
    }

    "styles a declaration whose name is not ASCII, and a name whose tail is not" in {
      // The WHOLE name has to be the token: an ASCII class matches `struct C` and stops at the
      // accent, which styles one letter and leaves the rest bare. Matching the exact text is what
      // makes that a failure rather than a pass — checked by reverting each widened class.
      styled("struct Círculo", "Círculo") should contain("entity.name.type.sysl")
      styled("end Círculo", "Círculo") should contain("entity.name.type.sysl")
      styled("área(ancho: real) -> real = ancho", "área") should contain("entity.name.function.sysl")
      styled("val c: Círculo = q", "Círculo") should contain("entity.name.type.sysl")
      styled("val x = area(3.0)", "area") should contain("entity.name.function.call.sysl")
    }

    /** **A combining mark continues a name in the lexer, so it has to continue one here** — `café`
      * spelled `e` + U+0301 is one identifier, and a class without `\p{M}` stops at the mark and
      * styles `Cafe` while leaving the accent and everything after it bare.
      */
    "styles a name carrying a combining mark as one token" in {
      val decomposed = "Cafés"
      styled(s"struct $decomposed", decomposed) should contain("entity.name.type.sysl")
      styled(s"val c: $decomposed = q", decomposed) should contain("entity.name.type.sysl")
      styled(s"café(x: int) -> int = x", "café") should contain("entity.name.function.sysl")
      styled("val x = café(3)", "café") should contain("entity.name.function.call.sysl")
    }

    /** **A name whose FIRST letter is not ASCII is styled where the rule names a declaration and not
      * where it depends on case**, and that is a limit of the engine rather than a decision.
      *
      * Two of the grammar's rules are about capitalisation — a capitalised name is a type, a
      * lowercase one at a call is a function — and this Oniguruma port supports the general Unicode
      * categories (`\p{L}`, `\p{N}`) and not the subcategories, so there is no way to write "an
      * uppercase letter" that it will compile. `\p{Lu}` and `\p{Ll}` are refused, and a refused
      * pattern is silently dropped.
      *
      * So the case-dependent rules keep an ASCII first character and take the Unicode class for
      * everything after it, which is why `Círculo` styles and `Ómnibus` does not. The rules that
      * name a declaration — `struct X`, `end X`, a function at the head of a line — have no such
      * dependency and are Unicode throughout.
      *
      * Pinned rather than left as an absence, so that whoever adds the subcategories to the engine
      * finds the case that says what changes.
      */
    "and does not style one whose first letter is not ASCII where the rule is about case" in {
      styled("val x = área(3.0)", "área") should not contain "entity.name.function.call.sysl"
      styled("val c: Ómnibus = q", "Ómnibus") should not contain "entity.name.type.sysl"
    }

    /** **A KEYWORD INSIDE AN IDENTIFIER IS NOT A KEYWORD, WHICH STOPPED BEING TRUE THE DAY A NAME
      * COULD HOLD A LETTER OUTSIDE ASCII.**
      *
      * Every `\bword\b` in this grammar depends on what the engine thinks a word character is, and
      * Oniguruma keeps two answers: `\w` is ASCII and `\b` is encoding-aware. The port tied `\b` to
      * its ASCII `\w`, so a boundary fired wherever the script changed *inside* a word — `síif`
      * rendering `if` as a keyword, `realíssimo` rendering `real` as a primitive type — on the whole
      * docs site and in everything `sysl weave` writes.
      *
      * Fixed upstream in `io.github.edadma:oniguruma` 0.0.5 (`UCDProperty.BoundaryWord`, `L | M | N
      * | Pc`), which reaches here through `highlighter` 0.0.11. Pinned here as well as there because
      * this is where the symptom is, and because the pin is what would notice a downgrade.
      */
    "and does not find a keyword inside a name that merely contains one" in {
      // **Asked over the WHOLE LINE rather than over the name's own token, because with the defect
      // present there IS no token for the name** — `síif` arrives split into `sí` and `if`, so a
      // filter on the exact text answers empty and `should not contain` passes having compared
      // nothing. The first version of this case did exactly that and was green against the broken
      // engine; it is the same trap as asserting that some pattern matched, one level in.
      def scopes(code: String): List[String] =
        Highlighter.fromJson(Grammar.sysl) match
          case Left(why) => fail(s"the grammar did not load: $why")
          case Right(h)  => h.tokens(code).flatten.flatMap(_.scopes).distinct

      scopes("val síif = 1") should not contain "keyword.control.sysl"
      scopes("val realíssimo = 1") should not contain "support.type.primitive.sysl"
      scopes("var estáreal = 3") should not contain "support.type.primitive.sysl"

      // The keyword itself is still a keyword, so none of the above can pass by styling nothing.
      scopes("val x = 1") should contain("keyword.declaration.sysl")
      scopes("val x: real = 1.0") should contain("support.type.primitive.sysl")
      scopes("if x then 1 else 2") should contain("keyword.control.sysl")
    }

    // The other direction, so the classes above cannot be widened into styling anything at all: a
    // digit still does not begin a name, in any script. `source.sysl` is on every token, so what is
    // asserted is the absence of the *type* scope rather than of all scopes.
    "and still refuses to style a name beginning with a digit as a type" in {
      styled("struct 3café", "3café") should not contain "entity.name.type.sysl"
    }
  }
}
