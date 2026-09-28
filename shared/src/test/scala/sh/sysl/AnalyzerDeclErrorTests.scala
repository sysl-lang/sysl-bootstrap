package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Diagnostics for declarations and their construction: function-call arity, struct fields and
 * construction, enums and their variants, generic type-argument arity and inference, and the
 * conversions between a simple enum and an integer.
 */
class AnalyzerDeclErrorTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  "a call with the wrong number of arguments" in {
    err("add(a: int, b: int) -> int = a + b\nprint(add(1))") should include("takes 2 arguments")
  }

  "reading an unknown struct field" in {
    err("struct P\n    x: int\nvar p = P(1)\nprint(p.y)") should include("no field or property 'y'")
  }

  "reading a field of a non-struct" in {
    err("var x = 5\nprint(x.foo)") should include("cannot read field 'foo'")
  }

  "constructing a struct with the wrong arity" in {
    err("struct P\n    x: int\n    y: int\nvar p = P(1)") should include("has 2 fields")
  }

  "enums" - {
    "a match on a data enum must cover every variant" in {
      val src = "enum Shape\n    Circle(r: int)\n    Empty\nf(s: Shape) -> int\n    s match\n        Empty -> 0"
      err(src) should include("not exhaustive")
      err(src) should include("Circle")
    }

    "constructing a variant with the wrong arity" in {
      err("enum Shape\n    Circle(r: int)\n    Empty\nvar s = Circle(1, 2)") should
        include("has 1 field, but 2 values were given")
    }

    "a nullary variant cannot take arguments" in {
      err("enum Shape\n    Circle(r: int)\n    Empty\nvar s = Empty(1)") should
        include("carries nothing, so it is written as a name on its own")
    }

    "matching an unknown variant" in {
      val src = "enum Shape\n    Circle(r: int)\n    Empty\nf(s: Shape) -> int\n    s match\n        Square(x) -> 0\n        else 1"
      err(src) should include("no variant 'Square'")
    }

    // Two enums sharing a variant name is legal as of `reference/types.md § Enums`'s namespacing
    // rule — what is refused is a *use* of the shared name with nothing to say which was meant.
    // `VariantNamespaceTests` covers the rest of it.
    "a variant name shared by two enums is ambiguous where nothing settles it" in {
      err("enum A\n    X\n    Y\nenum B\n    X\n    Z\nvar v = X\nprint(1)") should
        include("'X' is a variant of 'A' and 'B'")
    }

    "an enum value cannot be printed" in {
      err("enum Color\n    Red\n    Green\nprint(Red)") should include("cannot print")
    }
  }

  "generics" - {
    "the wrong number of type arguments" in {
      err("struct Box[T]\n    value: T\nf(b: Box[int, real])\n    print(1)") should
        include("takes 1 type argument, but 2 type arguments were given")
    }

    "type arguments on a type that takes none" in {
      err("var x: int[real] = 1") should include("does not take type arguments")
    }

    "a type argument that nothing determines" in {
      err("var x = None") should include("cannot infer the type argument")
    }

    // The `"two"` is what the parameter turned out to be, since a written type outranks a literal
    // in inference; so the complaint is about the `1`, which is the argument that could have been
    // anything and was not.
    "an argument that does not match the instantiated parameter" in {
      err("""pair[T](a: T, b: T) -> T = a
            |var x = pair(1, "two")
            |""".stripMargin) should include("'a' of 'pair' is string, but int was given")
    }

    "a type that contains itself has no finite size" in {
      err("struct Node\n    next: Node\nvar n = Node(n)") should include("contains itself")
    }

    // **A type argument is not containment, and what decides is how the generic uses it.** Reaching
    // an in-progress type while resolving `Wrap[Node]`'s argument says nothing on its own — the
    // question is asked again where the substitution is *used*, which for a bare `T` field is by
    // value at no indirection at all. So this is still refused, and the caret lands on the field
    // that holds it rather than on the argument list that mentioned it.
    "a generic that holds its parameter by value still contains what it is given" in {
      err("""struct Wrap[T]
            |    x: T
            |struct Node
            |    w: Wrap[Node]
            |var n: Node
            |""".stripMargin) should include("type 'Node' contains itself")
    }

    // The same question the other way round is `StructRunTests`' "a struct whose children are a
    // growable sequence of itself", which is a run test because compiling it is only half the claim.

    // `reference/types.md § Enums` says the same of a variant, and it has to be caught *here*
    // rather than at layout: the model that sizes a data enum's payload region walks its variants,
    // so a type that reached itself would be an unbounded walk in the compiler instead of a
    // diagnostic.
    "nor does a variant that holds its own enum" in {
      err("""enum List
            |    Cons(head: int, tail: List)
            |    Nil
            |var l: List = Nil
            |""".stripMargin) should include("type 'List' contains itself")
    }
  }

  "enum ↔ integer conversion" - {
    "converting a data enum to an integer is rejected" in {
      err(
        """enum Shape
          |    Circle(radius: int)
          |    Empty
          |var s = Circle(3)
          |print(int(s))""".stripMargin
      ) should include("only a simple enum converts to an integer")
    }

    "a checked cast into a data enum is rejected" in {
      err(
        """enum Shape
          |    Circle(radius: int)
          |    Empty
          |var s = Shape(0)""".stripMargin
      ) should include("carries data")
    }

    "a checked cast on a generic enum is rejected" in {
      err(
        """enum Maybe[T]
          |    Just(value: T)
          |    Nothing
          |var m = Maybe(0)""".stripMargin
      ) should include("generic")
    }

    "a checked cast takes exactly one integer argument" in {
      err(
        """enum Color
          |    Red
          |    Green
          |var c = Color(1, 2)""".stripMargin
      ) should include("exactly one integer")
    }

    "a checked cast rejects a non-integer argument" in {
      err(
        """enum Color
          |    Red
          |    Green
          |var c = Color("red")""".stripMargin
      ) should include("converts an integer")
    }

    "the fallible constructor on a data enum is rejected" in {
      err(
        """enum Shape
          |    Circle(radius: int)
          |    Empty
          |var s = Shape.try(0)""".stripMargin
      ) should include("carries data")
    }

    "an unknown member on an enum is still reported, not swallowed by try" in {
      err(
        """enum Color
          |    Red
          |    Green
          |var c = Color.Bogus(1)""".stripMargin
      ) should include("enum 'Color' has no variant or associated function 'Bogus'")
    }

    "an underlying-type annotation on a generic enum is rejected" in {
      err(
        """enum Maybe[T]: u8
          |    Just(value: T)
          |    Nothing""".stripMargin
      ) should include("generic enum cannot pin an underlying type")
    }

    "an underlying-type annotation on a data enum is rejected" in {
      err(
        """enum Shape: u8
          |    Circle(radius: int)
          |    Empty""".stripMargin
      ) should include("only a simple enum has an underlying integer type")
    }

    "a non-integer underlying type is rejected" in {
      err(
        """enum Color: string
          |    Red
          |    Green""".stripMargin
      ) should include("underlying type must be an integer")
    }

    "a discriminant that overflows the underlying type is rejected" in {
      err(
        """enum Color: u8
          |    Red
          |    Big = 300""".stripMargin
      ) should include("does not fit")
    }

    "a negative discriminant under an unsigned underlying type is rejected" in {
      err(
        """enum Color: u8
          |    Red = -1""".stripMargin
      ) should include("does not fit")
    }

    "an auto-incremented discriminant that overflows the underlying type is rejected" in {
      err(
        """enum Small: u2
          |    A = 3
          |    B""".stripMargin
      ) should include("does not fit")
    }

    /** A simple enum's value is the whole of its identity, so two names for one value leave the
      * language unable to keep its own promises: `Pos` and `Val` stop being inverses, the second
      * variant's `match` arm can never run, and `Image` lowered to a `switch` with a repeated case,
      * which clang rejected — the program was accepted and then failed to assemble.
      */
    "two variants standing for one value" - {
      "are refused, and the message names both of them and the value" in {
        val e = err(
          """enum Colour: int
            |    Red = 1
            |    Green = 1
            |    Blue = 2""".stripMargin
        )

        e should include("'Red' and 'Green'")
        e should include("both stand for 1")
        e should include("a 'const'")
      }

      // The collision an explicit value cannot be read off the line it is written on: `Green` takes
      // the value after `Red`, and `Blue` says that same number out loud two lines later.
      "including where an auto-incremented value lands on an explicit one" in {
        err(
          """enum Colour: int
            |    Red = 1
            |    Green
            |    Blue = 2""".stripMargin
        ) should include("'Green' and 'Blue' both stand for 2")
      }

      // Distinctness is the whole of the rule: nothing about order or sign is being asked for. The
      // negative control — that such an enum still runs and still answers correctly — is in
      // `EnumAttrRunTests`, where a run can show the values rather than only their acceptance.

      // A data enum's tags are handed out in order and it refuses an explicit value outright, so it
      // has no way to reach the collision — pinned here so that giving it one would have to say so.
      "and a data enum cannot reach the case at all" in {
        err(
          """enum Shape
            |    Circle(r: int)
            |    Empty = 0""".stripMargin
        ) should include("cannot also have an explicit value")
      }

      // A declaration is judged whether or not anything reads it, exactly as a constant is — so the
      // enum nobody mentions is the one that shows the judging happens at the declaration and not on
      // the way to a use.
      "even where nothing in the program ever names the enum" in {
        err(
          """enum Unused: int
            |    Red = 1
            |    Green = 1
            |print("nothing mentions it")""".stripMargin
        ) should include("both stand for 1")
      }

      // A discriminant is any constant expression (`reference/modules.md § const — a value`), so the collision need not be visible in
      // the two lines that collide — it is the folded values that must differ, not their spellings.
      "and where one value reaches its number through a const" in {
        err(
          """const TOP: int = 1
            |enum Colour: int
            |    Red = TOP
            |    Green = 1""".stripMargin
        ) should include("'Red' and 'Green' both stand for 1")
      }
    }

    /** A generic enum has no eager instantiation to be judged at, so the first use is where it is
      * caught — but nothing a *simple* enum's variants read depends on the type arguments, so it is
      * still the declaration that is wrong and still reported once however many instantiations
      * follow.
      */
    "a generic simple enum with two variants standing for one value" in {
      def occurrences(haystack: String, needle: String): Int =
        haystack.sliding(needle.length).count(_ == needle)

      val e = err(
        """enum Pair[T]
          |    Left = 1
          |    Right = 1
          |var p: Pair[int] = Pair[int].Left
          |var q: Pair[bool] = Pair[bool].Left""".stripMargin
      )

      e should include("'Left' and 'Right' both stand for 1")
      occurrences(e, "both stand for") shouldBe 1
    }

    /** A declaration is judged once, but its name can be mentioned any number of times, and each
      * mention used to rebuild the type and raise the same complaint again at its own position. The
      * report also landed wherever the walk had last looked rather than at the declaration — for a
      * program whose only mistake was its first line, that was a trait in the library.
      */
    "a mistake in an enum declaration is reported once, at the declaration" - {
      def occurrences(haystack: String, needle: String): Int =
        haystack.sliding(needle.length).count(_ == needle)

      "however many times the type is named afterwards" in {
        val e = err(
          """enum Colour: int
            |    Red = 1
            |    Green = 1
            |print(Colour::Pos(Colour.Red))
            |print(Colour::Image(Colour.Green))
            |print(Colour::Pos(Colour.Red))""".stripMargin
        )

        occurrences(e, "both stand for") shouldBe 1
        e should include("1 | enum Colour: int")
      }

      "and the same holds for a discriminant that does not fit" in {
        val e = err(
          """enum Colour: u8
            |    Red = 1
            |    Green = 300
            |print(Colour::Pos(Colour.Red))
            |print(Colour::Pos(Colour.Red))""".stripMargin
        )

        occurrences(e, "does not fit") shouldBe 1
        e should include("1 | enum Colour: u8")
      }
    }
  }

  // A struct from another module is known to the analyzer by its key, whose `$` separates the
  // module from the name. Neither a construction nor its refusals may show that key: the call finds
  // the declaration through it, and a message names the struct the way a reader writes it.
  "constructing a struct declared in another module" - {
    val lib = "b.sysl" -> """|module b
                             |
                             |struct T
                             |    x: int
                             |
                             |struct G[A]
                             |    x: A
                             |    y: int
                             |
                             |mk() -> T = T("s")
                             |""".stripMargin

    val plain = "b.sysl" -> """|module b
                               |
                               |struct T
                               |    x: int
                               |
                               |struct G[A]
                               |    x: A
                               |    y: int
                               |""".stripMargin

    "a generic one imported by name is built at a written instantiation" in {
      runOf(
        "b.sysl"    -> """|module b
                          |
                          |struct G[T]
                          |    x: T
                          |""".stripMargin,
        "main.sysl" -> """|import b.G
                          |
                          |val g = G[int](1)
                          |val h = G[string]("two")
                          |print(g.x)
                          |print(h.x)
                          |""".stripMargin,
      ) shouldBe "1\ntwo\n"
    }

    "a field's mismatch names it qualified, reached through its module" in {
      errOf(plain, "main.sysl" -> "import b\n\nprint(b.T(\"s\").x)\n") should
        include("'x' of 'b.T' is int, but string was given")
    }

    "and the same when it was imported by name" in {
      errOf(plain, "main.sysl" -> "import b.T\n\nprint(T(\"s\").x)\n") should
        include("'x' of 'b.T' is int, but string was given")
    }

    "and the same where a field is given by name" in {
      errOf(plain, "main.sysl" -> "import b\n\nprint(b.T(x = \"s\").x)\n") should
        include("'x' of 'b.T' is int, but string was given")
    }

    "and the same inside the module that declares it" in {
      errOf(lib, "main.sysl" -> "import b\n\nprint(b.mk().x)\n") should
        include("'x' of 'b.T' is int, but string was given")
    }

    "a generic one's mismatch names it at its instantiation" in {
      errOf(plain, "main.sysl" -> "import b\n\nprint(b.G(1, \"s\").x)\n") should
        include("'y' of 'b.G[int]' is int, but string was given")
    }

    "no refusal shows the module separator" in {
      errOf(plain, "main.sysl" -> "import b\n\nprint(b.T(\"s\").x)\n") should not include "$"
    }
  }

  "a struct declared in the entry file is named bare" in {
    err("struct L\n    x: int\nprint(L(\"s\").x)") should include("'x' of 'L' is int, but string was given")
  }
}
