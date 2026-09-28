package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** An identifier may be written in any script (`reference/lexical.md § Identifiers`).
  *
  * **The rule is one predicate: a name begins with `_` or anything Unicode calls a letter, and
  * continues with that, a digit or a combining mark.** Not a list of ranges — a table of accepted blocks is a thing
  * somebody has to extend every time a script is asked for, and the first extension request would
  * arrive from whoever was left out of the last one.
  *
  * **What it is for is readability rather than internationalisation.** A language whose identifiers
  * are ASCII asks everybody who does not think in English to transliterate their own vocabulary, and
  * the words that suffer are the domain ones — the names a reader most needs to recognise. Go, Java,
  * Scala and C# all took this road.
  *
  * **The symbol half was expected to need nothing and needed a rearrangement**, which is the fact
  * worth carrying. The encoding was already there — a quoted name could hold anything, so `$XX` /
  * `$uXXXXXX` was written years before this — but it was applied at `Modules.qualify`, where a
  * *key* is built, and a key is read back as a name in several places and printed in diagnostics.
  * That was invisible while every bare identifier was ASCII, because the encoding was then the
  * identity on all but a handful of quoted names.
  *
  * So the two jobs were split (`LlvmName`): a key is **guarded**, which marks a `$` alone so that
  * `Modules.split` still finds the module separator, and IR text is made **safe** at the five
  * renderings that write a name. **Three defects fell out of that and all three were reachable in
  * released 0.0.88 through a quoted name** — a method whose IR would not parse, a module segment
  * likewise, and a variant that crashed the compiler. `QuotedIdentTests` holds the cases from the
  * other side; the accents were a second vehicle rather than the cause.
  *
  * A name that was previously reachable only by quoting is now reachable by writing it, and it
  * lowers to exactly the symbol the quoted form always did.
  */
class UnicodeIdentTests extends AnyFreeSpec with Matchers with CodegenSupport with RunSupport {

  private def withLexer(body: SyslLexical => Any): Unit = { body(new SyslLexical); () }

  extension (l: SyslLexical) {

    def bare(src: String): List[Any] =
      l.scan(src).filterNot(t => t == l.Newline || t == l.Indent || t == l.Dedent)

    def bad(src: String): String = l.bare(src).head.toString
  }

  "the token" - {

    "a Latin letter with an accent on it" in withLexer { l =>
      l.bare("café") shouldBe List(l.Identifier("café"))
      l.bare("año") shouldBe List(l.Identifier("año"))
      l.bare("größe") shouldBe List(l.Identifier("größe"))
      l.bare("ação") shouldBe List(l.Identifier("ação"))
    }

    // The rule is Unicode's own answer rather than a Latin-1 carve-out, so it is worth pinning a
    // script that has nothing to do with the request that prompted it.
    "and any other script's letters, on the same rule" in withLexer { l =>
      l.bare("μ") shouldBe List(l.Identifier("μ"))
      l.bare("Ελλάδα") shouldBe List(l.Identifier("Ελλάδα"))
      l.bare("Москва") shouldBe List(l.Identifier("Москва"))
      l.bare("名前") shouldBe List(l.Identifier("名前"))
    }

    "an accented letter may begin a name, since it is a letter" in withLexer { l =>
      l.bare("área") shouldBe List(l.Identifier("área"))
      l.bare("Ómnibus") shouldBe List(l.Identifier("Ómnibus"))
    }

    // The declaration is what a caller has to spell, so the two forms are two names rather than one
    // spelling of it. This is the case UAX #31's normalization would fold together and this rule
    // deliberately does not. Both must still BE names: this case once passed by asserting only that
    // the decomposed spelling was not the precomposed identifier, which a lexer refusing the mark
    // outright satisfied just as well.
    "a precomposed letter and a base plus a combining mark are different names" in withLexer { l =>
      val precomposed = "café"
      val decomposed  = "café"

      l.bare(precomposed) shouldBe List(l.Identifier(precomposed))
      l.bare(decomposed) shouldBe List(l.Identifier(decomposed))
      precomposed should not be decomposed
    }

    // A mark is part of the letter before it (Unicode category M: Mn, Mc, Me). Most of the world's
    // scripts write their words with them, so a rule of letters alone refuses nearly every word in
    // Devanagari — 'ि' and 'ी' are spacing marks (Mc), '्' a non-spacing one (Mn).
    "a combining mark of any kind continues a name" in withLexer { l =>
      l.bare("हिन्दी") shouldBe List(l.Identifier("हिन्दी"))
      l.bare("x́") shouldBe List(l.Identifier("x́"))
      l.bare("a⃝") shouldBe List(l.Identifier("a⃝"))
      l.bare("_́") shouldBe List(l.Identifier("_́"))
    }

    // A mark with nothing before it has nothing to combine with, so it may not begin a name — and it
    // draws on top of whatever precedes it, so the message has to say what the eye cannot.
    "a combining mark may not begin a name, and the refusal says why" in withLexer { l =>
      l.bad("́a") should include("illegal character: U+0301 is a combining mark")
      l.bad("िx") should include("may not begin a name")
    }

    // The same claim from the other side: nothing normalizes the decomposed spelling into the
    // precomposed identifier on the way through the lexer.
    "and no normalization folds the decomposed spelling into the precomposed one" in withLexer { l =>
      l.bare("café") should not be List(l.Identifier("café"))
    }

    "a digit still may not begin one" in withLexer { l =>
      l.bare("3café") should not be List(l.Identifier("3café"))
    }

    // A name written in a script with its own digits may use them; the *literal* grammar is
    // deliberately unmoved, since a literal is a value the arithmetic has to read.
    "a non-ASCII digit may continue a name and may not begin one" in withLexer { l =>
      l.bare("caf٣") shouldBe List(l.Identifier("caf٣"))
      l.bare("٣") should not be List(l.Identifier("٣"))
    }

    // A `Char` is one UTF-16 unit, so a letter above U+FFFF arrives as a surrogate pair and
    // `Character.isLetter` answers false for either half. Every living script's letters are inside
    // the BMP, CJK Unified Ideographs included; what is outside is the historic scripts and the CJK
    // extension planes. Asserted rather than left to be discovered, because the diagnostic says
    // "illegal character" and gives no hint that the plane is what decided it.
    "a letter above the BMP is refused, and that edge is real" in withLexer { l =>
      l.bad("𠮷") should include("illegal character")
    }

    "an operator is still not a letter" in withLexer { l =>
      l.bad("×") should include("illegal character")
      l.bad("÷") should include("illegal character")
    }
  }

  "a name in any script is an ordinary name" - {

    "declared, read and assigned" in {
      val src =
        """var año = 3
          |año += 4
          |print(año)""".stripMargin

      run(src) shouldBe "7\n"
    }

    "as a function and its parameters" in {
      val src =
        """área(ancho: real, altura: real) -> real = ancho * altura
          |print(área(3.0, 4.0))""".stripMargin

      run(src) shouldBe "12\n"
    }

    "as a struct, its fields and its methods" in {
      val src =
        """struct Círculo
          |    radio: real
          |
          |    área(self) -> real = 3.0 * self.radio * self.radio
          |
          |val c = Círculo(2.0)
          |
          |print(c.radio, c.área())""".stripMargin

      run(src) shouldBe "2 12\n"
    }

    "as an enum and its variants" in {
      val src =
        """enum Estación
          |    Otoño
          |    Invierno
          |
          |val e = Estación.Otoño
          |
          |print(e == Otoño, e == Invierno)""".stripMargin

      run(src) shouldBe "true false\n"
    }

    // The one that exercises the symbol rather than the scope: module storage is laid into the
    // object file under a mangled name, so this is what says the encoding carries a bare identifier
    // as well as it carried a quoted one.
    "as module storage, which is what reaches the emitted symbol" in {
      val src =
        """module contabilidad
          |
          |var contador: int = 0
          |
          |incrementar(cuánto: int) -> int
          |    contador += cuánto
          |    contador""".stripMargin

      runOf(
        "contabilidad/c.sysl" -> src,
        "main.sysl"           -> "import contabilidad.incrementar\n\nprint(incrementar(5), incrementar(37))",
      ) shouldBe "5 42\n"
    }

    // A bare name and the quoted form of the same name were two spellings with two lowerings until
    // the identifier grammar caught up; now they are one name written two ways, which is what makes
    // the quoted form's job purely "a name the grammar still cannot reach".
    "and the quoted form of the same name is the same name" in {
      val src =
        """var café = 3
          |`café` += 1
          |print(café)""".stripMargin

      run(src) shouldBe "4\n"
    }

    // `reference/lexical.md § Identifiers`: the precomposed and the decomposed `café` are different
    // identifiers, so one scope may bind both and each read finds its own.
    "the precomposed and decomposed spellings bind two names in one scope" in {
      val src =
        "val café = 1\nval café = 2\nprint(café, café)"

      run(src) shouldBe "1 2\n"
    }

    "a name in a script written with marks, declared and called" in {
      val src =
        "हिन्दी(x: int) -> int = x + 1\nprint(हिन्दी(41))"

      run(src) shouldBe "42\n"
    }

    "a combining mark beginning a name is refused in a program" in {
      err("val x = ́a\nprint(x)") should include("U+0301 is a combining mark")
    }

    "across a module boundary, imported by name" in {
      runOf(
        "geometría/a.sysl" -> "module geometría\n\nárea(x: int) -> int = x + 1",
        "main.sysl"        -> "import geometría.área\n\nprint(área(41))",
      ) shouldBe "42\n"
    }

    "through a nested module path, every segment of which is accented" in {
      runOf(
        "geometría/básica/a.sysl" -> "module geometría.básica\n\nárea(x: int) -> int = x * 2",
        "main.sysl"               -> "import geometría.básica.área\n\nprint(área(21))",
      ) shouldBe "42\n"
    }
  }

  /** The composition sites, one case each.
   *
   * **A name reaches IR text through four renderings and no others** — a function's `define`, a
   * function's `declare`, a global's definition, and a value naming one — plus the sigilled name of
   * a declared LLVM type. `LlvmName.safe` sits at those five and nowhere earlier, so what these
   * cases are really asking is whether a shape composes its name somewhere those five do not see.
   *
   * They are written as one case per *mechanism* rather than per feature, because the mechanism is
   * what decides: a generic's mangled name, a vtable's slot, a variant's payload type, a closure's
   * environment, a destructor's hook, an ownership box. Each of those builds a symbol by
   * concatenation from a user's name, and each was unreachable while a bare identifier was ASCII.
   */
  "a name in any script survives every place a symbol is composed" - {

    // The mangled name of an instantiation carries the type argument, so this is the one that says
    // a generic's symbol is composed from a name and still lands somewhere `safe` can see.
    "a generic instantiated at a type whose name is not ASCII" in {
      val src =
        """struct Estación
          |    día: int
          |
          |primero[T](xs: []const T) -> T = xs[0]
          |
          |val es = [Estación(4), Estación(9)]
          |
          |print(primero(es).día)""".stripMargin

      run(src) shouldBe "4\n"
    }

    // A trait's method reaches its implementation through a vtable slot, which is a global whose
    // initializer names each function — so the name travels as a `Val.Global` rather than as a
    // `define`, and the two renderings are separate lines of `LlvmName`'s.
    "a trait, its impl and a dynamic call through the slot" in {
      val src =
        """trait Área
          |    área(self) -> int
          |
          |struct Círculo
          |    radio: int
          |
          |impl Área for Círculo
          |    área(self) -> int = 3 * self.radio * self.radio
          |
          |describir(f: &Área) -> int = f.área()
          |
          |print(describir(Círculo(2)))""".stripMargin

      run(src) shouldBe "12\n"
    }

    // **The payload type of a variant is a declared LLVM type named `%<enum>.<variant>`**, composed
    // in `Type.scala` from the mangled enum and the variant's own name — which is the one site a
    // nullary variant never reaches, so the case above it would have passed with this broken.
    "an enum variant that carries a payload, which names a type of its own" in {
      val src =
        """enum Medida
          |    Ancho(valor: int)
          |    Altura(valor: int)
          |
          |val m = Ancho(7)
          |
          |print(m match
          |    Ancho(n) -> n
          |    Altura(n) -> n * 100)""".stripMargin

      run(src) shouldBe "7\n"
    }

    // A closure's environment is a struct the compiler names, and a captured local's name is a
    // field of it — so a body local written in another script reaches a second naming path.
    "a closure capturing a local whose name is not ASCII" in {
      val src =
        """aplicar(f: () -> int) -> int = f()
          |
          |val año = 40
          |
          |print(aplicar(() -> año + 2))""".stripMargin

      run(src) shouldBe "42\n"
    }

    // A destructor is reached by a hook the emitter builds rather than by anything the source
    // writes, and the hook's name is composed from the type's — `arc.drop.<mangled>`.
    //
    // **The receiver has to be a `&T` for the hook to exist at all**, which is a fact about `Drop`
    // rather than about names: a plain value's scope end runs nothing, checked against an ASCII
    // control before this fixture was believed. Written as a `val` in a function so the release is
    // at the dedent and the ordering is what the assertion is about.
    "a destructor, whose hook the emitter names for the type" in {
      val src =
        """struct Cuentaañ
          |    valor: int
          |
          |impl Drop for Cuentaañ
          |    drop(self) = print("suelto", self.valor)
          |
          |hacer()
          |    val c: &Cuentaañ = Cuentaañ(5)
          |    print("hecho", c.valor)
          |
          |hacer()
          |print("fin")""".stripMargin

      run(src) shouldBe "hecho 5\nsuelto 5\nfin\n"
    }

    // `Display` is the rendering path, and an associated function is the receiverless half of
    // member lowering — a member is named `<type symbol>.<member>` either way, and only a receiver
    // decides which of the two lookups finds it.
    "an impl of Display, and an associated function beside it" in {
      val src =
        """struct Número
          |    valor: int
          |
          |    cero() -> Número = Número(0)
          |
          |impl Display for Número
          |    display(self, out: *Writer, fmt: FormatSpec)
          |        "№".display(out, fmt)
          |        self.valor.display(out, fmt)
          |
          |print(Número(8), Número.cero())""".stripMargin

      run(src) shouldBe "№8 №0\n"
    }

    // A `&T` is a counted box, and the retain and release the emitter inserts are named for the
    // type they are about — a third composition site, reached only by a value that is boxed.
    "a boxed value of a type whose name is not ASCII" in {
      val src =
        """struct Árbol
          |    peso: int
          |
          |val a: &Árbol = Árbol(11)
          |val b = a
          |
          |print(a.peso + b.peso)""".stripMargin

      run(src) shouldBe "22\n"
    }

    // **A `declare` line is the one rendering a whole-program compile never reaches**, since
    // everything it needs it also defines. What reaches it is separate compilation: a library built
    // against another states the dependency's half rather than emitting a second copy of it
    // (`LibraryArtifactTests`, "declares the dependency's compiled half"), and that statement is a
    // `declare` carrying a name somebody else wrote.
    //
    // Written here rather than left to a fixture that could not reach it: with the encoding removed
    // from `Sig.declare` alone, every other case in this suite stays green.
    "a dependency's compiled half, declared rather than defined, across an artifact" in {
      val base =
        """module geometría
          |
          |área(x: int) -> int = x + 1
          |""".stripMargin

      val piel =
        """module piel
          |
          |import geometría.área
          |
          |doble(x: int) -> int = área(área(x))
          |""".stripMargin

      val baseSource = List(Source("geometría/lib.sysl", base, List("geometría")))
      val pielSource = List(Source("piel/lib.sysl", piel, List("piel")))

      val ir =
        LibraryArtifact.build(pielSource, Target.default, Set.empty, None, Nil, SearchPaths.none,
          baseSource, Nil) match
          case Left(err)     => fail(s"the dependent did not build: $err")
          case Right((t, _)) => t

      def has(kind: String, symbol: String): Boolean =
        ir.linesIterator.exists(l => l.startsWith(s"$kind ") && l.contains(s"@$symbol("))

      // The dependency is stated and not copied, and the name it is stated under is the encoded
      // one — `geometr$eda$$e1rea`, which is what the definition in the other artifact is called.
      // Both halves of the module key needed the encoding, which is the shape a single-module
      // program never produces.
      has("declare", "geometr$eda$$e1rea") shouldBe true
      has("define", "geometr$eda$$e1rea") shouldBe false
      has("define", "piel$doble") shouldBe true
    }

    // Module storage is laid into the object file under its own global, and an accented module
    // makes both halves of `<module>$<name>` need the encoding rather than only the tail. This is
    // the case the `geometría$$e1rea` failure came from, one indirection further in.
    "module storage in an accented module, read from another module" in {
      runOf(
        "contabilidad/c.sysl" ->
          """module contabilidad
            |
            |var contador: int = 0
            |
            |incrementar(cuánto: int) -> int
            |    contador += cuánto
            |    contador""".stripMargin,
        "main.sysl" ->
          "import contabilidad.incrementar\n\nprint(incrementar(5), incrementar(37))",
      ) shouldBe "5 42\n"
    }
  }
}
