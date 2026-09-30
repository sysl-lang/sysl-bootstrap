package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A type parameter that carries a default (`reference/generics.md § A parameter may carry a
 * default`): `trait Scale[R = Self]`, `struct Pair[A, B = A]`.
  *
  * The customer is the operator catalog. `library/core.md § Walking a type of your own` records
  * that a heterogeneous operand — `Complex * f64` — wants `Mul[Rhs]` rather than `Mul`, and that
  * the change cannot be made without defaults: every `impl Mul for Point` already written would
  * become `impl Mul[Point] for Point`, and `[T: Mul]` would stop saying what it says now. So the
  * case that matters most here is `Self` as the default, which is what makes those two spellings
  * mean the same thing.
  *
  * A default belongs only where arguments are **written**. sysl offers no call-site type arguments
  * at all (`reference/generics.md § [] means type application in a type, indexing in an
  * expression`), so a function's, a method's, and an `impl` block's parameters are solved rather
  * than supplied, and a default there is refused — the thing that would be useful in those three is
  * a fallback for an inference that found nothing, which is a different feature.
  */
class DefaultTypeParamTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** A trait whose parameter is the operand type, defaulting to the implementing type — the shape the
    * catalog's `Mul` is meant to take.
    */
  private val scale =
    """trait Scale[R = Self]
      |    scale(self, k: R) -> Self
      |struct P
      |    v: int
      |impl Scale for P
      |    scale(self, k: P) -> P = P(self.v * k.v)
      |""".stripMargin

  "what a default stands in for" - {
    "a trait applied to nothing takes its default" in {
      run(
        """trait Sink[T = int]
          |    put(self, x: T) -> int
          |struct C
          |    n: int
          |impl Sink for C
          |    put(self, x: int) -> int = self.n + x
          |f[T: Sink](c: T) -> int = c.put(5)
          |print(f(C(10)))""".stripMargin,
      ) shouldBe "15\n"
    }

    // The half the catalog is waiting on: an `impl` written with no arguments means the one written
    // with `Self`'s value, so nothing already in the language has to be respelled.
    "'Self' as a default is the implementing type at an impl" in {
      run(scale + "print(P(6).scale(P(7)).v)") shouldBe "42\n"
    }

    "and writing that argument out is the same implementation" in {
      run(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |struct P
          |    v: int
          |impl Scale[P] for P
          |    scale(self, k: P) -> P = P(self.v * k.v)
          |print(P(6).scale(P(7)).v)""".stripMargin,
      ) shouldBe "42\n"
    }

    // And the other half: a bound naming the trait bare asks for it at the bounded parameter, so
    // `[T: Scale]` keeps meaning what it meant before the trait took a parameter at all.
    "'Self' as a default is the parameter itself at a bound" in {
      run(scale + "f[T: Scale](a: T, b: T) -> T = a.scale(b)\nprint(f(P(6), P(7)).v)") shouldBe "42\n"
    }

    "a bound may still fix the argument to something else" in {
      run(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |struct P
          |    v: int
          |impl Scale[int] for P
          |    scale(self, k: int) -> P = P(self.v * k)
          |f[T: Scale[int]](a: T) -> T = a.scale(3)
          |print(f(P(14)).v)""".stripMargin,
      ) shouldBe "42\n"
    }

    "a struct takes its default" in {
      run(
        """struct Pair[A, B = A]
          |    x: A
          |    y: B
          |var p: Pair[int] = Pair(3, 4)
          |print(p.x + p.y)""".stripMargin,
      ) shouldBe "7\n"
    }

    "an enum takes its default" in {
      run(
        """enum Slot[T = int]
          |    Empty
          |    Full(value: T)
          |held(s: Slot) -> int = s match
          |    Full(v) -> v
          |    Empty -> 0
          |var s: Slot = Full(9)
          |print(held(s))""".stripMargin,
      ) shouldBe "9\n"
    }

    "a default may name a parameter written before it" in {
      run(
        """struct Pair[A, B = A]
          |    x: A
          |    y: B
          |var p: Pair[string] = Pair("a", "b")
          |print(p.x + p.y)""".stripMargin,
      ) shouldBe "ab\n"
    }

    "and may build a type out of one" in {
      run(
        """struct Holder[A, B = []A]
          |    one: A
          |    many: B
          |var xs = [1, 2, 3]
          |var h: Holder[int] = Holder(4, xs[0..<3])
          |print(h.one + h.many[2])""".stripMargin,
      ) shouldBe "7\n"
    }

    /** Filling happens before anything is keyed on the arguments, so the written-out and left-out
      * spellings are one instantiation rather than two that happen to have the same fields. Asserted
      * on the emitted module, since that is the only place two would show up as two.
      */
    "the filled and the written spelling are one instantiation" in {
      val out = ir(
        """struct Pair[A, B = A]
          |    x: A
          |    y: B
          |var p: Pair[int] = Pair(1, 2)
          |var q: Pair[int, int] = Pair(3, 4)
          |print(p.x + q.y)""".stripMargin,
      )

      out should include("%struct.Pair.int.int = type { i32, i32 }")
      out.linesIterator.count(_.startsWith("%struct.Pair")) shouldBe 1
    }

    "different fills are different types" in {
      val out = ir(
        """struct Pair[A, B = A]
          |    x: A
          |    y: B
          |var p: Pair[int] = Pair(1, 2)
          |var q: Pair[int, bool] = Pair(3, true)
          |print(p.x + q.x)""".stripMargin,
      )

      out should include("%struct.Pair.int.int = type { i32, i32 }")
      out should include("%struct.Pair.int.bool = type { i32, i1 }")
    }

    "a default may name a type declared below the one that defaults to it" in {
      run(
        """struct Holder[T = Later]
          |    v: T
          |struct Later
          |    n: int
          |var h: Holder = Holder(Later(7))
          |print(h.v.n)""".stripMargin,
      ) shouldBe "7\n"
    }
  }

  "what a requirement does with one" - {
    /** A required trait is applied like any other, so it takes the defaults too — and `Self` in one
      * of them is the type implementing the *requiring* trait, which is the same type all the way
      * down a chain of requirements.
      */
    "a required trait takes its default" in {
      run(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |trait Vector: Scale
          |    dim(self) -> int
          |struct P
          |    v: int
          |impl Scale for P
          |    scale(self, k: P) -> P = P(self.v * k.v)
          |impl Vector for P
          |    dim(self) -> int = 1
          |f[T: Vector](a: T, b: T) -> T = a.scale(b)
          |print(f(P(6), P(7)).v)""".stripMargin,
      ) shouldBe "42\n"
    }

    /** The filled argument has to be the one the `impl` was filed under, or the requirement would be
      * "satisfied" by an implementation at other arguments. `impl Scale[int] for P` does not answer a
      * requirement of `Scale[Self]`.
      */
    "an implementation at other arguments does not answer the requirement" in {
      err(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |trait Vector: Scale
          |    dim(self) -> int
          |struct P
          |    v: int
          |impl Scale[int] for P
          |    scale(self, k: int) -> P = P(self.v * k)
          |impl Vector for P
          |    dim(self) -> int = 1""".stripMargin,
      ) should include("requires 'Scale[P]'")
    }
  }

  "what the declaration has to satisfy" - {
    /** Arguments are written left to right, so a parameter with no default sitting behind one that
      * has could never be reached — leaving the earlier one out would leave nothing for the later one
      * to be written after.
      */
    "a defaulted parameter may not come before an undefaulted one" in {
      err("struct S[A = int, B]\n    x: A\n    y: B") should include(
        "'B' has no default and comes after 'A', which has one",
      )
    }

    "a default may not name a parameter fixed after it" in {
      err("struct S[A = B, B]\n    x: A\n    y: B") should include("'B' has no default and comes after 'A'")
    }

    "nor one fixed after it when the shape is otherwise legal" in {
      err("struct S[A, B = C, C = int]\n    x: A\n    y: B\n    z: C") should include(
        "the default for 'B' names 'C', which is fixed after it",
      )
    }

    "a default may not name its own parameter" in {
      err("struct S[A = A]\n    x: int") should include("the default for 'A' names 'A'")
    }

    "a struct has no 'Self' for a default to name" in {
      err("struct S[A = Self]\n    x: int") should include("'Self' is the type implementing a trait")
    }

    "and neither has an enum" in {
      err("enum E[A = Self]\n    Empty\n    Full(value: A)") should include(
        "'Self' is the type implementing a trait",
      )
    }

    "a default has to name a type" in {
      err("struct S[A = Nope]\n    x: A") should include("unknown type 'Nope'")
    }
  }

  "where a parameter is solved rather than written" - {
    /** The three refusals are one rule: sysl has no call-site type arguments
      * (`reference/generics.md § [] means type application in a type, indexing in an expression`),
      * so in none of these is there an argument list with a gap for a default to fill.
      */
    "a function may not default a type parameter" in {
      err("f[T = int](x: T) -> T = x\nprint(f(1))") should include(
        "'T' is a type parameter of the function 'f', whose type parameters are solved",
      )
    }

    "nor may a method" in {
      err("struct P\n    v: int\n    m[T = int](self, x: T) -> int = self.v") should include(
        "'T' is a type parameter of the method 'P.m'",
      )
    }

    /** A trait's method may be generic now, so the default on its parameter is the only thing left
      * wrong with it — which is the same refusal an inherent member's gets, one line up.
      */
    "a trait's method may be generic, and still may not default one" in {
      err("trait T\n    m[U = int](self, x: U) -> int") should include(
        "is a type parameter of the method",
      )
    }

    "nor may an 'impl' block" in {
      err(
        """trait Show
          |    show(self) -> string
          |struct Box[T]
          |    v: T
          |impl[T = int] Show for Box[T]
          |    show(self) -> string = "box"""".stripMargin,
      ) should include("is a type parameter of the 'impl' block")
    }
  }

  "what a use has to satisfy" - {
    "supplying more arguments than there are parameters" in {
      err("struct S[A, B = A]\n    x: A\n    y: B\nvar s: S[int, int, int] = S(1, 2)") should include(
        "type 'S' takes between 1 and 2 type arguments, but 3 type arguments were given",
      )
    }

    "supplying fewer than the ones with no default" in {
      err("struct S[A, B = A]\n    x: A\n    y: B\nvar s: S = S(1, 2)") should include(
        "type 'S' takes between 1 and 2 type arguments, but 0 type arguments were given",
      )
    }

    "a trait applied to too many is told the same way" in {
      err(
        """trait Sink[T = int]
          |    count(self) -> int
          |f[U: Sink[int, int]](x: U) -> int = x.count()""".stripMargin,
      ) should include("trait 'Sink' takes between 0 and 1 type arguments, but 2 type arguments were given")
    }

    /** A trait whose method names the parameter, applied to the wrong number of arguments, is worth
      * its own case: the arity is what is wrong, and the reader should not also be told that the
      * trait's own signature does not resolve. Written against a trait with **no** default so that
      * it says something about arity checking generally rather than about defaults.
      */
    "an arity mistake does not also report the trait's own signature" in {
      val out = err(
        """trait Sink[T]
          |    put(self, x: T) -> int
          |f[U: Sink[int, int]](x: U) -> int = x.put(1)""".stripMargin,
      )

      out should include("takes 1 type argument, but 2 type arguments were given")
      out should not include "unknown type 'T'"
    }

    /** The parameter's own bound is checked on the filled list by the deferral every other type
      * argument goes through, so a default that fails it is caught wherever the declaration is
      * applied — and against the `impl` blocks, which are registered after the declaration is read.
      */
    "a default that fails the parameter's bound is caught at the use" in {
      err(
        """trait Named
          |    name(self) -> string
          |struct Plain
          |    n: int
          |struct Box[T: Named = Plain]
          |    v: T
          |var b: Box = Box(Plain(1))""".stripMargin,
      ) should include("'Named'")
    }

    "and a fill that meets it is accepted" in {
      run(
        """trait Named
          |    name(self) -> string
          |struct Plain
          |    n: int
          |impl Named for Plain
          |    name(self) -> string = "plain"
          |struct Box[T: Named = Plain]
          |    v: T
          |var b: Box = Box(Plain(1))
          |print(b.v.name())""".stripMargin,
      ) shouldBe "plain\n"
    }
  }

  /** `reference/modules.md § Visibility` says a declaration may not be more visible than the types
    * it names. A default is one of those: it is not written at the use, so a caller that leaves the
    * argument out ends up holding a type the default named — and if that type does not reach them,
    * they are holding something they could not have written and cannot name.
    */
  "how far a default reaches" - {
    "a struct may not default to a type that reaches less far than it does" in {
      errIn(
        ("m", "m.sysl",
         """module m
           |private struct Secret
           |    n: int
           |struct Holder[T = Secret]
           |    v: T""".stripMargin),
        ("", "main.sysl", "import m.Holder\nprint(1)"),
      ) should include("'Holder' is public, but the default for 'T' names 'm.Secret', which is private")
    }

    "nor may a trait" in {
      errIn(
        ("m", "m.sysl",
         """module m
           |private struct Secret
           |    n: int
           |trait Sink[T = Secret]
           |    put(self, x: T) -> int""".stripMargin),
        ("", "main.sysl", "import m.Sink\nprint(1)"),
      ) should include("'Sink' is public, but the default for 'T' names 'm.Secret', which is private")
    }

    "nor an enum" in {
      errIn(
        ("m", "m.sysl",
         """module m
           |private struct Secret
           |    n: int
           |enum Slot[T = Secret]
           |    Empty
           |    Full(value: T)""".stripMargin),
        ("", "main.sysl", "import m.Slot\nprint(1)"),
      ) should include("'Slot' is public, but the default for 'T' names 'm.Secret', which is private")
    }

    // A default naming one of the declaration's *own* parameters names nothing anyone has to reach.
    "a default naming a sibling parameter reaches nowhere" in {
      runIn(
        ("m", "m.sysl", "module m\nstruct Pair[A, B = A]\n    x: A\n    y: B"),
        ("", "main.sysl", "import m.Pair\nvar p: Pair[int] = Pair(3, 4)\nprint(p.x + p.y)"),
      ) shouldBe "7\n"
    }

    /** A default is written where the declaration is, so it is resolved in **that** file's terms. A
      * user who cannot see the type the default names still gets it filled in — which is the whole
      * reason the rule above has to exist rather than the default simply failing to resolve.
      */
    "a default is resolved in the file that declares it" in {
      runIn(
        ("m", "m.sysl",
         """module m
           |struct Unit
           |    n: int
           |struct Holder[T = Unit]
           |    v: T
           |made() -> Holder = Holder(Unit(7))""".stripMargin),
        ("", "main.sysl", "import m.made\nprint(made().v.n)"),
      ) shouldBe "7\n"
    }
  }

  "one default reaching another" - {
    "a default may name a generic type that has defaults of its own" in {
      run(
        """struct Inner[U = int]
          |    n: U
          |struct Outer[T = Inner]
          |    v: T
          |var o: Outer = Outer(Inner(7))
          |print(o.v.n)""".stripMargin,
      ) shouldBe "7\n"
    }

    /** Each arrival applies the declaration to fewer arguments than it declares, so a default that
      * leads back to its own declaration would ask for the defaults again forever. Caught rather
      * than recursed into.
      */
    "a default may not lead back to its own declaration" in {
      err("struct S[T = S]\n    v: int\nvar s: S = S(1)") should include("leads back to 'S'")
    }

    "nor around a longer way" in {
      err(
        """struct A[T = B]
          |    v: int
          |struct B[U = A]
          |    w: int
          |var a: A = A(1)""".stripMargin,
      ) should include("leads back to")
    }

    // Applied in full, nothing is filled at all, so a type may name itself in a written argument
    // exactly as a field may — the indirection is what makes it finite, and that rule is unchanged.
    "a type fully applied inside its own default is not a cycle" in {
      run(
        """struct Node[T = int]
          |    v: T
          |    next: *Node[int]
          |var n: Node = Node(7, null)
          |print(n.v)""".stripMargin,
      ) shouldBe "7\n"
    }
  }

  "what the table does with one" - {
    /** The slot list a table is built from and the one a call site indexes by are the same walk, and
      * a defaulted requirement is where they could most easily disagree — one filling `Self`, the
      * other not. Pinned on the emitted table rather than on the program's output, since a
      * disagreement about order is exactly what running one call would fail to show.
      */
    "a defaulted requirement lays out one slot per member, in one order" in {
      val out = ir(
        """trait Tag[R = int]
          |    tag(self, k: R) -> int
          |trait Shown: Tag
          |    show(self) -> string
          |struct P
          |    v: int
          |impl Tag for P
          |    tag(self, k: int) -> int = self.v + k
          |impl Shown for P
          |    show(self) -> string = "p"
          |var o: &Shown = P(5)
          |print(o.tag(1))""".stripMargin,
      )

      out should include(
        "[2 x ptr] [ptr @vt.adapt.ref.P.tag, ptr @vt.adapt.ref.P.show]",
      )
    }

    "and the call reaches the required trait's member through it" in {
      run(
        """trait Tag[R = int]
          |    tag(self, k: R) -> int
          |trait Shown: Tag
          |    show(self) -> string
          |struct P
          |    v: int
          |impl Tag for P
          |    tag(self, k: int) -> int = self.v + k
          |impl Shown for P
          |    show(self) -> string = "p"
          |var o: &Shown = P(5)
          |print(o.tag(1))""".stripMargin,
      ) shouldBe "6\n"
    }

    /** Three deep, with `Self` at every step — the requirement is the implementing type all the way
      * down rather than being rebound to whatever declared the next link.
      */
    "'Self' carries the whole length of a chain of requirements" in {
      run(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |trait Vector: Scale
          |    dim(self) -> int
          |trait Space: Vector
          |    origin(self) -> int
          |struct P
          |    v: int
          |impl Scale for P
          |    scale(self, k: P) -> P = P(self.v * k.v)
          |impl Vector for P
          |    dim(self) -> int = 1
          |impl Space for P
          |    origin(self) -> int = 0
          |f[T: Space](a: T, b: T) -> T = a.scale(b)
          |print(f(P(6), P(7)).v)""".stripMargin,
      ) shouldBe "42\n"
    }
  }

  "what an implementation may be written for" - {
    "a shape rather than a named type still fills 'Self'" in {
      run(
        """trait Size[R = Self]
          |    size(self, other: R) -> int
          |impl Size for []int
          |    size(self, other: []int) -> int = self[0] + other[0]
          |var a = [1, 2, 3]
          |var b = [4, 5]
          |print(a[0..<3].size(b[0..<2]))""".stripMargin,
      ) shouldBe "5\n"
    }

    /** A conditional block's own parameters are solved, so its subject is the type applied to them —
      * and `Self` in the trait's default is that, not the bare declaration.
      */
    "a conditional implementation fills 'Self' with its own subject" in {
      run(
        """trait Named
          |    name(self) -> string
          |trait Pairable[R = Self]
          |    pair(self, other: R) -> string
          |struct Box[T]
          |    v: T
          |impl[T: Named] Named for Box[T]
          |    name(self) -> string = self.v.name()
          |impl[T: Named] Pairable for Box[T]
          |    pair(self, other: Box[T]) -> string = self.name() + other.name()
          |struct P
          |    n: string
          |impl Named for P
          |    name(self) -> string = self.n
          |print(Box(P("a")).pair(Box(P("b"))))""".stripMargin,
      ) shouldBe "ab\n"
    }
  }

  "what inference does with one" - {
    /** A default fills a **written** argument list. Inference solves from the arguments, and where it
      * reaches an answer that answer is the type — the default is not a preference that overrides
      * what the construction actually holds.
      */
    "a construction is inferred, not defaulted" in {
      run(
        """struct Pair[A, B = A]
          |    x: A
          |    y: B
          |var p = Pair(1, true)
          |print(p.y)""".stripMargin,
      ) shouldBe "true\n"
    }

    "and the annotation that leaves the argument out still fills it" in {
      err(
        """struct Pair[A, B = A]
          |    x: A
          |    y: B
          |var p: Pair[int] = Pair(1, true)
          |print(p.y)""".stripMargin,
      ) should include("bool")
    }
  }

  "one implementation per trait per type" - {
    /** `02`'s coherence rule reads the *filled* arguments, so the two spellings of one implementation
      * are the one implementation — and writing both is the duplicate it looks like, not two
      * implementations at different arguments.
      */
    "the bare and the written 'impl' are the same implementation" in {
      err(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |struct P
          |    v: int
          |impl Scale for P
          |    scale(self, k: P) -> P = P(self.v * k.v)
          |impl Scale[P] for P
          |    scale(self, k: P) -> P = P(self.v + k.v)""".stripMargin,
      ) should include("already implements")
    }

    "while two genuinely different arguments are two implementations" in {
      run(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> Self
          |struct P
          |    v: int
          |impl Scale for P
          |    scale(self, k: P) -> P = P(self.v * k.v)
          |f[T: Scale](a: T, b: T) -> T = a.scale(b)
          |struct Q
          |    v: int
          |impl Scale[int] for Q
          |    scale(self, k: int) -> Q = Q(self.v + k)
          |print(f(P(6), P(7)).v)""".stripMargin,
      ) shouldBe "42\n"
    }
  }

  "what erasure does with one" - {
    /** An object has forgotten which type it holds, so there is no `Self` for a default to stand for.
      * The argument has to be written out, and saying that here is better than letting `Self` resolve
      * to nothing further in.
      */
    "an object cannot take a default of 'Self'" in {
      err(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> int
          |f(x: &Scale) -> int = x.scale(x)""".stripMargin,
      ) should include("an object has forgotten which type it holds")
    }

    "with the argument written out it is an ordinary object" in {
      run(
        """trait Scale[R = Self]
          |    scale(self, k: R) -> int
          |struct P
          |    v: int
          |impl Scale[int] for P
          |    scale(self, k: int) -> int = self.v * k
          |f(x: &Scale[int]) -> int = x.scale(3)
          |var o: &Scale[int] = P(14)
          |print(f(o))""".stripMargin,
      ) shouldBe "42\n"
    }

    "a trait defaulting to something concrete erases without being written out" in {
      run(
        """trait Sink[T = int]
          |    put(self, x: T) -> int
          |struct C
          |    n: int
          |impl Sink for C
          |    put(self, x: int) -> int = self.n + x
          |f(s: &Sink) -> int = s.put(5)
          |var o: &Sink = C(10)
          |print(f(o))""".stripMargin,
      ) shouldBe "15\n"
    }
  }

  /** A **value** parameter may carry a default too, under the same rules: it is the argument a use
    * would have written in its place, so it fills the same gap, counts toward the same arity, sits in
    * the same suffix and is refused on the same three solved declarations.
    */
  "a value parameter's default" - {
    "a struct takes it" in {
      run(
        """struct Ring[T, const N: usize = 4]
          |    items: [N]T
          |val r: Ring[int] = Ring([1, 2, 3, 4])
          |print(r.items.len)""".stripMargin,
      ) shouldBe "4\n"
    }

    "an enum takes it" in {
      run(
        """enum Slot[const N: usize = 2]
          |    Empty
          |    Full(xs: [N]int)
          |held(s: Slot) -> usize = s match
          |    Full(xs) -> xs.len
          |    Empty -> 0
          |var s: Slot = Full([5, 6])
          |print(held(s))""".stripMargin,
      ) shouldBe "2\n"
    }

    "type and value defaults fill together" in {
      run(
        """struct Grid[T = int, const N: usize = 3]
          |    cells: [N]T
          |var g: Grid = Grid([1, 2, 3])
          |var h: Grid[bool] = Grid([true, false, true])
          |var k: Grid[bool, 2] = Grid([true, false])
          |print(g.cells.len, h.cells.len, k.cells.len)""".stripMargin,
      ) shouldBe "3 3 2\n"
    }

    "a default may name a value parameter written before it" in {
      run(
        """struct Pad[const M: usize, const N: usize = M * 2]
          |    a: [M]int
          |    b: [N]int
          |var p: Pad[2] = Pad([1, 2], [3, 4, 5, 6])
          |print(p.a.len, p.b.len)""".stripMargin,
      ) shouldBe "2 4\n"
    }

    /** Asserted on the emitted module, as the type-parameter case above is: two instantiations
      * would show up there as two struct types.
      */
    "the filled and the written spelling are one instantiation" in {
      val out = ir(
        """struct Ring[T, const N: usize = 4]
          |    items: [N]T
          |var p: Ring[int] = Ring([1, 2, 3, 4])
          |var q: Ring[int, 4] = Ring([5, 6, 7, 8])
          |print(p.items[0] + q.items[3])""".stripMargin,
      )

      out.linesIterator.count(_.startsWith("%struct.Ring")) shouldBe 1
    }

    "a defaulted value parameter may not come before an undefaulted one" in {
      err("struct S[const N: usize = 4, T]\n    x: [N]T") should include(
        "'T' has no default and comes after 'N', which has one",
      )
    }

    "a default may not name a value parameter fixed after it" in {
      err("struct S[const M: usize = N, const N: usize = 2]\n    x: [M]int") should include(
        "the default for 'M' names 'N', which is fixed after it",
      )
    }

    "a default has to fit the parameter's type" in {
      err("struct S[const N: u8 = 300]\n    x: int") should include(
        "the default for 'N' does not fit byte: 300",
      )
    }

    "and so does a written argument" in {
      err("struct S[const N: u8]\n    x: int\nvar s: S[300] = S(1)\nprint(s.x)") should include(
        "this argument does not fit byte: 300",
      )
    }

    "a default has to be a value" in {
      err("struct S[const N: usize = int]\n    x: [N]int") should include("'int' is a type")
    }

    "a function may not default one" in {
      err("total[const N: usize = 3](xs: [N]int) -> usize = N\nprint(total([1, 2]))") should include(
        "'N' is a value parameter of the function 'total', whose type parameters are solved from " +
          "what it is given rather than written where it is used — so '= 3' has nothing to stand in for",
      )
    }

    "nor may a method" in {
      err("struct P\n    v: int\n    m[const N: usize = 2](self, xs: [N]int) -> usize = N") should include(
        "'N' is a value parameter of the method 'P.m'",
      )
    }

    "nor may an 'impl' block" in {
      err(
        """trait Show
          |    show(self) -> string
          |struct Buf[const N: usize]
          |    v: [N]int
          |impl[const N: usize = 2] Show for Buf[N]
          |    show(self) -> string = "buf"""".stripMargin,
      ) should include("'N' is a value parameter of the 'impl' block")
    }
  }
}
