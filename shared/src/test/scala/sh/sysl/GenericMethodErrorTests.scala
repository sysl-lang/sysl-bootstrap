package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What a member with type parameters of its own may and may not say (`reference/generics.md §
 * Members and associated functions`).
 *
 * The two lists of parameters are the whole subject. What the *call* has to settle is the member's
 * own, so a parameter nothing at the call mentions is refused there; what the *declaration* has to
 * settle is which name belongs to which list, and what a body may assume of either. A trait
 * declares no generic method, so neither does an `impl`, and that is stated at the block rather
 * than left to fail somewhere further in.
 */
class GenericMethodErrorTests extends AnyFreeSpec with CodegenSupport {

  "the member's own parameters have to be inferable at the call" - {

    "a parameter the signature never mentions cannot be inferred" in {
      err(
        """struct Box[T]
          |    v: T
          |    count[U](self) -> int = 0
          |var b = Box(1)
          |print(b.count())""".stripMargin,
      ) should include("cannot infer the type argument 'U' of 'Box.count' here — annotate the expected type")
    }

    "one of several left unsettled is named on its own" in {
      err(
        """struct Box[T]
          |    v: T
          |    two[U, V](self, x: U) -> int = 0
          |var b = Box(1)
          |print(b.two(3))""".stripMargin,
      ) should include("cannot infer the type argument 'V' of 'Box.two'")
    }

    "a type with no parameters of its own is no different" in {
      err(
        """struct Counter
          |    n: int
          |    count[U](self) -> int = 0
          |var c = Counter(1)
          |print(c.count())""".stripMargin,
      ) should include("cannot infer the type argument 'U' of 'Counter.count'")
    }
  }

  "the instantiation the call resolves to is checked" - {

    "a bound the member wrote is reported in the member's name" in {
      err(
        """trait Rank
          |    rank(self) -> int
          |struct Box[T]
          |    v: T
          |    scored[U: Rank](self, x: U) -> int = x.rank()
          |var b = Box(1)
          |print(b.scored(2))""".stripMargin,
      ) should include("'Box.scored' requires its type parameter 'U' to implement 'Rank', but int does not")
    }

    "and so is one on an associated function of a type with no parameters" in {
      err(
        """trait Rank
          |    rank(self) -> int
          |struct C
          |    n: int
          |    of[U: Rank](x: U) -> int = x.rank()
          |print(C.of(1))""".stripMargin,
      ) should include("'C.of' requires its type parameter 'U' to implement 'Rank', but int does not")
    }

    "the type's own bound is still reported in the type's name" in {
      err(
        """trait Rank
          |    rank(self) -> int
          |struct Scored[T: Rank]
          |    n: int
          |    of[U](x: T, y: U) -> U = y
          |print(Scored.of(3, true))""".stripMargin,
      ) should include("'Scored' requires its type parameter 'T' to implement 'Rank', but int does not")
    }

    "the arity is checked before anything is inferred from the arguments" in {
      err(
        """struct Box[T]
          |    v: T
          |    one[U](self, x: U) -> U = x
          |var b = Box(1)
          |print(b.one(1, 2))""".stripMargin,
      ) should include("method 'Box.one' takes 1 argument, but 2 arguments were given")
    }

    "an argument disagreeing with the parameter the other one fixed is reported" in {
      err(
        """struct Box[T]
          |    v: T
          |    two[U](self, x: U, y: U) -> U = x
          |var b = Box(1)
          |print(b.two(1, "s"))""".stripMargin,
      ) should include("'x' of 'Box.two' is string, but int was given")
    }

    "a result the arguments already fixed is not bent to the annotation" in {
      err(
        """struct Box[T]
          |    v: T
          |    one[U](self, x: U) -> U = x
          |var b = Box(1)
          |var s: string = b.one(2)""".stripMargin,
      ) should include("string")
    }
  }

  "the body is checked at the definition, against the member's own bounds" - {

    "rendering an unbounded parameter names the bound that would license it" in {
      err(
        """struct Box[T]
          |    v: T
          |    describe[U](self, x: U) -> string = str(x)""".stripMargin,
      ) should include("'str' needs 'U: sysl.Display'")
    }

    "an operator on one does too" in {
      err(
        """struct Box[T]
          |    v: T
          |    twice[U](self, x: U) -> U = x + x""".stripMargin,
      ) should include(s"'+' needs 'U: ${lib("Add")}'")
    }

    "a type with no parameters of its own has its member checked all the same" in {
      err(
        """struct Counter
          |    n: int
          |    describe[U](self, x: U) -> string = str(x)""".stripMargin,
      ) should include("'str' needs 'U: sysl.Display'")
    }

    "what the type asks of its own parameter says nothing about the member's" in {
      err(
        """struct Box[T: Display]
          |    v: T
          |    describe[U](self, x: U) -> string = str(x)""".stripMargin,
      ) should include("'str' needs 'U: sysl.Display'")
    }

    "an unbounded parameter handed to a bounded type is refused at the definition" in {
      err(
        """trait Rank
          |    rank(self) -> int
          |struct Scored[T: Rank]
          |    n: int
          |struct Box[T]
          |    v: T
          |    keep[U](self, x: U) -> Scored[U] = Scored(0)""".stripMargin,
      ) should include("'Scored' requires its type parameter 'T' to implement 'Rank', but 'U' is not bounded by it")
    }
  }

  "the two lists of parameters stay apart" - {

    "a member may not spell one of its own the way the type spells one of its" in {
      err(
        """struct Box[T]
          |    v: T
          |    cast[T](self, x: T) -> T = x""".stripMargin,
      ) should include("'Box' already declares a type parameter 'T', so member 'cast' cannot declare one of that name")
    }

    "a bound on the member's own parameter still has to name a trait" in {
      err(
        """struct Box[T]
          |    v: T
          |    keep[U: Loud](self, x: U) -> U = x""".stripMargin,
      ) should include("the bound on 'U' in 'Box.keep' names 'Loud', which is not a trait")
    }

    "a property has no call to fix a parameter, so it may declare none" in {
      err(
        """struct Box[T]
          |    v: T
          |    size[U] -> int = 1""".stripMargin,
      ) should include("a property takes no type parameters")
    }

    /** A name and a type-parameter list are a method's head, so a line that goes on with anything
      * but `(` is a method missing its parameter list — not a property, which is only what `->`
      * would have made it. The struct used to call every such line a property with type
      * parameters while a trait said `'(' expected`; every kind of body now says the trait's thing
      * at the token after the `]`, and keeps the property refusal for the line that is one.
      */
    "a name and a type-parameter list are a method's head, whatever body holds them" - {
      val heads = List(
        "struct" -> "struct S\n    x: int\n",
        "enum"   -> "enum E\n    A\n",
        "trait"  -> "trait R\n    f(self) -> int\n",
      )
      // `y[T]` alone leaves nothing on its line, so its parameter list is missing at the next one.
      val notProperties = List("y[T] 5" -> "3:10", "y[T] = 1" -> "3:10", "y[T]: int" -> "3:9",
        "y[T]" -> "4:1")

      for (kind, head) <- heads; (line, at) <- notProperties do
        s"'$line' in $kind is a method missing its parameter list" in {
          val out = err(s"$head    $line\n")
          out should include("'(' expected")
          out should include(s":$at")
          out shouldNot include("a property takes no type parameters")
        }

      for (kind, head) <- heads; line <- List("y[T] -> int = 1", "y[T] -> int") do
        s"'$line' in $kind is a property, and is refused for its type parameters" in {
          err(s"$head    $line\n") should include("a property takes no type parameters")
        }

      "an 'impl' body reads it the same way" in {
        val src = "trait Tr\n    f(self) -> int\nstruct S\n    x: int\nimpl Tr for S\n    y[T] 5\n"
        val out = err(src)
        out should include("'(' expected")
        out should include(":6:10")
        err(src.replace("y[T] 5", "y[T] -> int = 1")) should include("a property takes no type parameters")
      }
    }
  }

  "an 'impl' supplies exactly the type parameters the trait's member declares" - {

    // A trait **may** declare one, and the member is left out of the table rather than keeping the
    // trait out of an object. What is refused is the one thing the table cannot carry: the call on
    // an erased value. Pinned here beside the arity rules below, since the two are what an `impl`
    // has to agree with.
    "the trait may declare one, and the object is what cannot reach it" in {
      val out = err(
        """trait Mapper
          |    tag(self) -> int
          |    map[U](self, x: U) -> U
          |struct P
          |    n: int
          |impl Mapper for P
          |    tag(self) -> int = self.n
          |    map[U](self, x: U) -> U = x
          |val o: &Mapper = P(1)
          |print(o.map(2))""".stripMargin,
      )

      out should include("'map' of 'Mapper' declares type parameters of its own")
      out should include("reach it through a bound")
    }

    "an 'impl' may not add parameters the trait did not declare" in {
      err(
        """trait Show
          |    show(self) -> int
          |struct P
          |    n: int
          |impl Show for P
          |    show[U](self) -> int = 1""".stripMargin,
      ) should include("method 'show' of 'impl Show for P' declares 1 type parameter, but trait 'Show' declares 0")
    }

    "nor may a generic 'impl'" in {
      err(
        """trait Show
          |    show(self) -> int
          |struct Box[T]
          |    v: T
          |impl[T] Show for Box[T]
          |    show[U](self) -> int = 1""".stripMargin,
      ) should include("method 'show' of 'impl Show for Box' declares 1 type parameter, but trait 'Show' declares 0")
    }

    "nor may one matching a shape" in {
      err(
        """trait Show
          |    show(self) -> int
          |impl[T] Show for []T
          |    show[U](self) -> int = 1""".stripMargin,
      ) should include("method 'show' of 'impl Show for []T' declares 1 type parameter, but trait 'Show' declares 0")
    }
  }
}
