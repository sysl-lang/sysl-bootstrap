package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-2 runtime behavior of members declared on an `enum`: methods, properties, and associated
 * functions on simple and data enums alike, on generic enums, and reached through a trait `impl`.
 *
 * An enum receiver differs from a struct one in what it lowers to — a simple enum is an integer, a
 * data enum a tagged aggregate — so each receiver mode is checked against both, and the
 * `Option`/`Result` members the library now carries are exercised as the first real users of the
 * whole path.
 */
class EnumMemberRunTests extends AnyFreeSpec with RunSupport {

  "a value receiver on a data enum matches on itself" in {
    val src =
      """enum Shape
        |    Circle(r: int)
        |    Square(side: int)
        |    area(self) -> int = self match
        |        Circle(r) -> 3 * r * r
        |        Square(s) -> s * s
        |var c = Circle(2)
        |var q = Square(5)
        |print(c.area(), q.area())""".stripMargin

    run(src) shouldBe "12 25\n"
  }

  // A simple enum's receiver is the integer it lowers to, and a nullary variant is a value like
  // any other, so a method is reached straight off the variant name.
  "a value receiver on a simple enum reads its discriminant" in {
    val src =
      """enum Color: u8
        |    Red = 1
        |    Green = 2
        |    next(self) -> int = int(self) + 1
        |var g = Green
        |print(Red.next(), g.next())""".stripMargin

    run(src) shouldBe "2 3\n"
  }

  "a pointer receiver replaces a simple enum in place" in {
    val src =
      """enum Step
        |    Zero
        |    One
        |    advance(*self)
        |        *self = One
        |    n(self) -> int = self match
        |        Zero -> 0
        |        One -> 1
        |var s = Zero
        |print(s.n())
        |s.advance()
        |print(s.n())""".stripMargin

    run(src) shouldBe "0\n1\n"
  }

  // Overwriting a data enum through `*self` replaces a tagged aggregate whose old payload owns a
  // string, so the release of what was there has to happen before the new value lands.
  "a pointer receiver replaces a data enum whose payload owns a string" in {
    val src =
      """enum Box
        |    Full(name: string)
        |    Empty
        |    label(self) -> string = self match
        |        Full(n) -> n
        |        Empty -> "empty"
        |    clear(*self)
        |        *self = Empty
        |var b = Full("held")
        |print(b.label())
        |b.clear()
        |print(b.label())""".stripMargin

    run(src) shouldBe "held\nempty\n"
  }

  "a reference receiver reads the shared heap enum" in {
    val src =
      """enum Box
        |    Full(name: string)
        |    Empty
        |    peek(&self) -> string = *self match
        |        Full(n) -> n
        |        Empty -> "none"
        |var h: &Box = Full("heaped")
        |print(h.peek())""".stripMargin

    run(src) shouldBe "heaped\n"
  }

  "a computed property on an enum reads with no parentheses" in {
    val src =
      """enum Shape
        |    Circle(r: int)
        |    Square(side: int)
        |    area(self) -> int = self match
        |        Circle(r) -> 3 * r * r
        |        Square(s) -> s * s
        |    doubled -> int = self.area() * 2
        |var c = Circle(2)
        |print(c.doubled)""".stripMargin

    run(src) shouldBe "24\n"
  }

  "an associated function on an enum is called through the type name" in {
    val src =
      """enum Shape
        |    Circle(r: int)
        |    Square(side: int)
        |    unit() -> Shape = Square(1)
        |    area(self) -> int = self match
        |        Circle(r) -> 3 * r * r
        |        Square(s) -> s * s
        |print(Shape.unit().area())""".stripMargin

    run(src) shouldBe "1\n"
  }

  // A member is an ordinary function with `self` first, so it recurses like one: each step
  // constructs a fresh receiver and calls back into the same monomorphized body.
  "a method on an enum recurses through a value it constructs" in {
    val src =
      """enum Nat
        |    Lit(n: int)
        |    countdown(self) -> int = self match
        |        Lit(n) -> if n <= 0 then 0 else Lit(n - 1).countdown() + n
        |print(Lit(4).countdown())""".stripMargin

    run(src) shouldBe "10\n"
  }

  "generic enums" - {
    "a method is instantiated from the receiver's type arguments" in {
      val src =
        """enum Maybe[T]
          |    Just(value: T)
          |    Nothing
          |    or_else(self, d: T) -> T = self match
          |        Just(v) -> v
          |        Nothing -> d
          |var a: Maybe[int] = Just(7)
          |var b: Maybe[int] = Nothing
          |var c: Maybe[string] = Just("hi")
          |var d: Maybe[string] = Nothing
          |print(a.or_else(0), b.or_else(-1))
          |print(c.or_else("none"), d.or_else("gone"))""".stripMargin

      run(src) shouldBe "7 -1\nhi gone\n"
    }

    "a property on a generic enum reads with no parentheses" in {
      val src =
        """enum Maybe[T]
          |    Just(value: T)
          |    Nothing
          |    tag -> int = self match
          |        Just(_) -> 1
          |        Nothing -> 0
          |var a: Maybe[real] = Just(1.5)
          |var b: Maybe[real] = Nothing
          |print(a.tag, b.tag)""".stripMargin

      run(src) shouldBe "1 0\n"
    }

    // Two element types are two independent monomorphized functions, exactly as for a generic
    // struct's members: Maybe.get.int and Maybe.get.real. Only the member's own definitions are
    // counted; the rest of the module is the ARC runtime and the renderers `print` reached.
    "the same generic enum method is monomorphized once per element type" in {
      val out = Compiler.compileToLlvm(
        """enum Maybe[T]
          |    Just(value: T)
          |    Nothing
          |    get(self, d: T) -> T = self match
          |        Just(v) -> v
          |        Nothing -> d
          |var a: Maybe[int] = Just(1)
          |var b: Maybe[real] = Just(2.5)
          |print(a.get(0), b.get(0.0))""".stripMargin
      )

      out.map(_.linesIterator.count(l => l.startsWith("define") && l.contains("@Maybe.get."))) shouldBe Right(2)
    }
  }

  "traits" - {
    "an impl for an enum makes its methods inherent" in {
      val src =
        """trait Show
          |    show(self) -> int
          |enum Dim
          |    Small
          |    Large
          |impl Show for Dim
          |    show(self) -> int = self match
          |        Small -> 1
          |        Large -> 100
          |var d = Large
          |print(d.show(), Small.show())""".stripMargin

      run(src) shouldBe "100 1\n"
    }

    // A trait bound is satisfied by an enum's `impl` the same way a struct's satisfies it, so the
    // one bounded generic serves both kinds.
    "an enum satisfies a trait bound alongside a struct" in {
      val src =
        """trait Show
          |    show(self) -> int
          |enum Dim
          |    Small
          |    Large
          |struct Fixed
          |    v: int
          |impl Show for Dim
          |    show(self) -> int = self match
          |        Small -> 1
          |        Large -> 100
          |impl Show for Fixed
          |    show(self) -> int = self.v
          |render[T: Show](x: T) -> int = x.show()
          |print(render(Large), render(Fixed(7)))""".stripMargin

      run(src) shouldBe "100 7\n"
    }
  }

  "the library's Option and Result" - {
    "Option answers whether it holds a value, and falls back when it does not" in {
      val src =
        """half(n: int) -> Option[int]
          |    if n % 2 == 0 then Some(n / 2) else None
          |var a = half(10)
          |var b = half(7)
          |print(a.is_some(), a.is_none(), b.is_some(), b.is_none())
          |print(a.unwrap_or(-1), b.unwrap_or(-1))""".stripMargin

      run(src) shouldBe "true false false true\n5 -1\n"
    }

    "Result answers which side it holds, and falls back on the error" in {
      val src =
        """parse(ok: bool) -> Result[int, string]
          |    if ok then Ok(9) else Err("bad")
          |var r = parse(true)
          |var e = parse(false)
          |print(r.is_ok(), r.is_err(), e.is_ok(), e.is_err())
          |print(r.unwrap_or(0), e.unwrap_or(0))""".stripMargin

      run(src) shouldBe "true false false true\n9 0\n"
    }

    "the fallible enum constructor's Option is asked the same questions" in {
      val src =
        """enum Color: u8
          |    Red = 1
          |    Green = 2
          |print(Color.try(2).is_some(), Color.try(9).is_some())""".stripMargin

      run(src) shouldBe "true false\n"
    }

    "a string payload survives unwrap_or on both sides" in {
      val src =
        """pick(n: int) -> Option[string]
          |    if n > 0 then Some("yes") else None
          |print(pick(1).unwrap_or("fallback"), pick(0).unwrap_or("fallback"))""".stripMargin

      run(src) shouldBe "yes fallback\n"
    }

    // unwrap_or on an Option[&T] hands out one of two references and drops the other, so over a
    // long loop every Inner must be freed exactly once — a leak grows RSS, a double-free crashes.
    // Peak RSS was separately confirmed flat at 3,000,000 iterations.
    // total = sum of i%4 over 500000 = 750000.
    "unwrap_or on an Option of references neither leaks nor frees twice" in {
      val src =
        """struct Inner
          |    v: int
          |grab(seed: int) -> &Inner
          |    var o: Option[&Inner] = Some(Inner(seed))
          |    o.unwrap_or(Inner(0))
          |var i = 0
          |var total = 0
          |while i < 500000
          |    var g = grab(i % 4)
          |    total += g.v
          |    i++
          |print(total)""".stripMargin

      run(src) shouldBe "750000\n"
    }

    // The discarded side is the one that owns the reference: when the Option is None the default
    // is handed back and nothing else was ever allocated, so the count still balances.
    "unwrap_or falling back to a reference neither leaks nor frees twice" in {
      val src =
        """struct Inner
          |    v: int
          |grab(seed: int) -> &Inner
          |    var o: Option[&Inner] = None
          |    o.unwrap_or(Inner(seed))
          |var i = 0
          |var total = 0
          |while i < 500000
          |    var g = grab(i % 4)
          |    total += g.v
          |    i++
          |print(total)""".stripMargin

      run(src) shouldBe "750000\n"
    }

    // A member calling another member of the same generic enum — is_none is written as the negation
    // of is_some — instantiates the second one from the first's own substitution.
    "a library member calling another member instantiates it too" in {
      val src =
        """var a: Option[real] = Some(0.5)
          |var b: Option[real] = None
          |print(a.is_none(), b.is_none())""".stripMargin

      run(src) shouldBe "false true\n"
    }

    // Reaching the other side. `unwrap`, `expect` and `unwrap_or` are all about the `Ok` half, so a
    // program interested in *why* something failed had to write the match out — which every guide
    // program checking what a function refuses did.
    "Result hands over the error the same way it hands over the value" in {
      val src =
        """parse(ok: bool) -> Result[int, string]
          |    if ok then Ok(7) else Err("not a number")
          |
          |print(parse(false).unwrap_err())
          |print(parse(false).expect_err("a refusal"))""".stripMargin

      run(src) shouldBe "not a number\nnot a number\n"
    }

    "an error payload of a type of its own comes back whole" in {
      val src =
        """enum Fault
          |    Empty
          |    TooLong
          |
          |    describe(self) -> string = self match
          |        Empty -> "nothing at all"
          |        TooLong -> "too long"
          |end Fault
          |
          |check(n: int) -> Result[int, Fault] = if n == 0 then Err(Empty) else Err(TooLong)
          |
          |print(check(0).unwrap_err().describe(), check(1).unwrap_err().describe())""".stripMargin

      run(src) shouldBe "nothing at all too long\n"
    }

    "unwrap_err on a value that succeeded stops the program" in {
      panics(
        """var r: Result[int, string] = Ok(1)
          |print(r.unwrap_err())""".stripMargin,
        "unwrap_err of an Ok value",
      )
    }

    "expect_err says what was expected instead" in {
      panics(
        """var r: Result[int, string] = Ok(1)
          |print(r.expect_err("a refusal"))""".stripMargin,
        "a refusal",
      )
    }

    // The error side owns what it holds exactly as the value side does, so reaching for it in a
    // loop neither leaks nor frees twice.
    "an error carrying a reference is reached without disturbing its count" in {
      val src =
        """struct Inner
          |    n: int
          |
          |var total = 0
          |var i = 0
          |
          |while i < 20000
          |    var r: Result[int, &Inner] = Err(Inner(i))
          |    total += r.unwrap_err().n
          |    i += 1
          |
          |print(total)""".stripMargin

      run(src) shouldBe "199990000\n"
    }

    // Nothing is emitted for a member no program calls: the two library enums are generic, so their
    // members exist only once an instantiation asks for one. A top-level library *function* is
    // dropped the same way, by reachability — printing an int reaches three of them and none of the
    // rest, so the whole printing surface does not land in every program.
    //
    // A **non-generic** library type's members are held back by the same reachability, which is
    // what lets the library carry `ByteSink` at all: eager members would have put its three, and
    // the two `Buf[u8]` members they reach, into every program that prints a number.
    //
    // The set is exact rather than a `should contain`, which is the only form that can say a fourth
    // definition did *not* appear.
    "an unused library declaration costs nothing in the output" in {
      val out = Compiler.compileToLlvm("print(1)")
      val defined = out.map(
        _.linesIterator.filter(l => l.startsWith("define") && !l.startsWith("define private"))
          .map(_.dropWhile(_ != '@').takeWhile(_ != '('))
          .toSet,
      )

      // `printc` arrives because the desugaring reaches it for a `char`, `encode_utf8` because
      // `printc` reaches it, and the two digit writers because `printi` reaches them — one library
      // function calling another is ordinary, and what the exact set is for is saying that nothing
      // *else* arrived.
      defined shouldBe Right(
        List("printi", "printc", "putbytes", "encode_utf8", "digits_long", "digits_ulong")
          .map(n => s"@${Library.key(n)}").toSet + "@main")
    }

    // And the other half of that bargain: a member the program *does* reach has to arrive, whether
    // it is called on a value, read as a property, or found through a trait object.
    "a library member a program calls is emitted" in {
      val out = Compiler.compileToLlvm(
        """import sysl.buf.byte_sink
          |
          |var g = byte_sink()
          |var w: *Writer = &g
          |display_str("x", w, FormatSpec(0, -1, false))
          |print(g.text().len)""".stripMargin,
      )
      val defined = out.map(_.linesIterator.filter(_.startsWith("define")).mkString("\n"))

      defined.getOrElse("") should include(s"@${Library.key("ByteSink")}.text")
      defined.getOrElse("") should include(s"@${Library.key("ByteSink")}.write")
    }
  }
}
