package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A parameter's default value, and an argument written at the parameter it names
 * (`reference/declarations.md § Default parameters and named arguments`).
 *
 * The two are one feature because they are one question — what a call may leave to the declaration
 * — and one implementation: both are resolved by `bindArgs` before any call form looks at its
 * arguments, so what the arity check, the generic solve, `checkArgs` and the emitter all receive is
 * the call written out in full. That is what the runs here are for. A test that only read the
 * emitted text would pin the binding and say nothing about whether the *program* means what the
 * chapter says it does — whether a default really is evaluated per call, whether reordering by name
 * really reaches the parameter named and not the one at that position.
 *
 * The collision with assignment is the other reason for the runs. `f(x = 1)` was a legal call
 * before this feature and meant something else; the tests that pin the new reading also pin the
 * escape from it, because a language that quietly changed what an existing line does would be worse
 * than one that never had the feature.
 */
class ArgumentTests
    extends AnyFreeSpec
    with RunSupport
    with CodegenSupport
    with ParseSupport
    with TestFrameworkSupport {

  "a default value" - {
    "stands where the argument was not written" in {
      run("""|greet(name: string, greeting: string = "hi") -> string = greeting + ", " + name
             |print(greet("ed"))
             |""".stripMargin) shouldBe "hi, ed\n"
    }

    "and steps aside where it was" in {
      run("""|greet(name: string, greeting: string = "hi") -> string = greeting + ", " + name
             |print(greet("ed", "hello"))
             |""".stripMargin) shouldBe "hello, ed\n"
    }

    // Several defaults, filled from the right, so what a call writes decides how many are taken. A
    // discriminating shape: three different values, so a fill that took the wrong one shows up.
    "fills from the right, however many the call stops short of" in {
      run("""|f(a: int, b: int = 20, c: int = 300) -> int = a + b + c
             |print(f(1))
             |print(f(1, 2))
             |print(f(1, 2, 3))
             |""".stripMargin) shouldBe "321\n303\n6\n"
    }

    // `reference/declarations.md § Default parameters and named arguments`: "a fresh call per call
    // site rather than one value computed once and shared". Asserted by defaulting to something
    // with a side effect and counting how often it happened.
    "is an expression evaluated at each call, not one value shared between them" in {
      run("""|import sysl.buf.{Buf, buf}
             |
             |grow(b: &Buf[int] = buf()) -> usize
             |    b.push(1)
             |    b.len()
             |
             |var shared: &Buf[int] = buf()
             |
             |print(grow())
             |print(grow())
             |print(grow(shared))
             |print(grow(shared))
             |""".stripMargin) shouldBe "1\n1\n1\n2\n"
    }

    "may name a module-level value the caller has never heard of" in {
      runOf(
        "conf.sysl" -> """|module conf
                          |
                          |val width: int = 40
                          |
                          |indent(depth: int, w: int = width) -> int = depth * w
                          |""".stripMargin,
        "main.sysl" -> """|import conf.indent
                          |
                          |print(indent(2))
                          |""".stripMargin,
      ) shouldBe "80\n"
    }

    "reaches a method" in {
      run("""|struct Box
             |    n: int
             |
             |    grown(self, by: int = 10) -> int = self.n + by
             |end Box
             |
             |var b = Box(5)
             |print(b.grown())
             |print(b.grown(1))
             |""".stripMargin) shouldBe "15\n6\n"
    }

    // `reference/declarations.md § Default parameters and named arguments`: a default "is written
    // in the declaration's terms", and the parameter's own type is the first of those terms. `None`
    // alone says what it is `None` *of* only if something tells it, and the parameter is what tells
    // it — so a member whose default was read against nothing could not take a `None` at all, while
    // the identical free function could.
    "is read at the parameter's type on a method, so a nullary variant needs no annotation" in {
      run("""|struct Box
             |    n: int
             |
             |    grown(self, by: Option[int] = None) -> int = by match
             |        Some(k) -> self.n + k
             |        None -> self.n
             |end Box
             |
             |var b = Box(5)
             |print(b.grown())
             |print(b.grown(Some(4)))
             |""".stripMargin) shouldBe "5\n9\n"
    }

    // The same on the receiverless kind, whose lowered parameter list has no `self` in front of it —
    // which is the offset the expected type is read at, so getting it wrong shows up here and
    // nowhere else.
    "and at the parameter's type on an associated function, which has no receiver in front" in {
      run("""|struct Box
             |    n: int
             |
             |    of(first: Option[int] = None, base: int = 2) -> Box = first match
             |        Some(k) -> Box(k + base)
             |        None -> Box(base)
             |end Box
             |
             |print(Box.of().n)
             |print(Box.of(Some(40)).n)
             |""".stripMargin) shouldBe "2\n42\n"
    }

    "reaches an associated function" in {
      run("""|struct Box
             |    n: int
             |
             |    of(n: int = 7) -> Box = Box(n)
             |end Box
             |
             |print(Box.of().n)
             |print(Box.of(2).n)
             |""".stripMargin) shouldBe "7\n2\n"
    }

    "reaches a nested function" in {
      run("""|work(base: int) -> int
             |    step(n: int, by: int = 4) -> int = n + by
             |    step(base) + step(base, 1)
             |
             |print(work(1))
             |""".stripMargin) shouldBe "7\n"
    }

    "and an 'extern', whose parameter names are sysl's to choose" in {
      run("""|extern abs(n: i32 = -5i32) -> i32
             |print(abs())
             |print(abs(-7i32))
             |""".stripMargin) shouldBe "5\n7\n"
    }

    "reaches a generic function, at the type the call fixes" in {
      run("""|second[T](a: T, b: T, take_first: bool = false) -> T = if take_first then a else b
             |print(second(1, 2))
             |print(second("x", "y", true))
             |""".stripMargin) shouldBe "2\nx\n"
    }

    // `reference/declarations.md § Default parameters and named arguments`: *"A default is read at
    // the type its parameter declares"* — and where that type is a parameter being solved, it is
    // read at what the ARGUMENTS solved it to. The omitted literal is not one of the call's
    // arguments, so it may not take part in the solve: before the fix it was analyzed bare as an
    // `int`, so `f(x)` refused a `1.0` default against the `real` that `x` settled, and `f(0.5)`
    // solved `T = int` from the default and refused the argument the caller actually wrote.
    "an omitted literal default is read at the type a variable argument settles" in {
      run("""|f[T](lo: T, step: T = 1.0) -> real = real(lo) + real(step)
             |val x: real = 0.5
             |print(f(x))
             |""".stripMargin) shouldBe "1.5\n"
    }

    "and at the type a written literal settles, which the default does not outvote" in {
      run("""|f[T](lo: T, step: T = 1.0) -> real = real(lo) + real(step)
             |print(f(0.5))
             |""".stripMargin) shouldBe "1.5\n"
    }

    "and at an integer the call settled, where it stays an integer" in {
      run("""|f[T: Add](lo: T, step: T = 1) -> T = lo + step
             |val n: u8 = 41
             |print(f(n))
             |print(f(10))
             |""".stripMargin) shouldBe "42\n11\n"
    }

    "while a written argument at the defaulted parameter still stands" in {
      run("""|f[T](lo: T, step: T = 1) -> real = real(lo) + real(step)
             |print(f(0.5, 0.25))
             |print(f(2, step = 3))
             |""".stripMargin) shouldBe "0.75\n5\n"
    }

    // A `u8` binding cannot take an `int`, so this compiling is the proof that the expected type
    // reached `T` ahead of the default's own spelling.
    "and it settles the parameter itself only where nothing the call or its context says" in {
      run("""|g[T](step: T = 1) -> T = step
             |print(g())
             |val b: u8 = g()
             |print(b)
             |""".stripMargin) shouldBe "1\n1\n"
    }

    "a generic struct's method reads its default at the receiver's argument" in {
      run("""|struct Box[T: Add]
             |    v: T
             |
             |    bumped(self, by: T = 1) -> T = self.v + by
             |end Box
             |
             |val b: u8 = 41
             |print(Box(b).bumped())
             |""".stripMargin) shouldBe "42\n"
    }

    // The literal rule is the ordinary one, so an integer literal is no `real` at a default either
    // (`reference/traits.md`: *"an integer literal is neither"*). The refusal is about the default,
    // and it names the argument the reader wrote nowhere — before the fix `f(0.5)` blamed `lo`.
    "an integer default is refused where the arguments settle 'T' to real" in {
      val said = err("""|f[T](lo: T, step: T = 1) -> real = real(lo) + real(step)
                        |print(f(0.5))
                        |""".stripMargin)
      said should include("'step' of 'f' was left to its default, and the default cannot be read at " +
        "real, the type the parameter has at this call")
      said should not include "'lo' of 'f'"
    }

    "and so is a float default where they settle it to int" in {
      err("""|f[T](lo: T, step: T = 1.5) -> T = lo
             |print(f(2))
             |""".stripMargin) should include(
        "'step' of 'f' was left to its default, and the default cannot be read at int, the type the " +
          "parameter has at this call")
    }

    // A literal the caller WROTE outranks one it left out, even where the left-out one stands first:
    // `b = 3` settles `T = int`, so it is the `1.5` default that is refused, never the `3`.
    "a written literal outranks a default that stands before it" in {
      val said = err("""|k[T](a: T = 1.5, b: T = 2) -> T = a
                        |print(k(b = 3))
                        |""".stripMargin)
      said should include("'a' of 'k' was left to its default, and the default cannot be read at int")
      said should not include "'b' of 'k'"
    }

    "and a generic struct's method's, at the receiver's argument" in {
      err("""|struct Box[T: Add]
             |    v: T
             |
             |    bumped(self, by: T = 1) -> T = self.v + by
             |end Box
             |
             |print(Box(0.5).bumped())
             |""".stripMargin) should include("'by' of ")
    }

    // `reference/declarations.md § Default parameters and named arguments`: a default stands
    // exactly where the argument would have been written, and at that position a closure literal
    // takes its parameter types from what is asking for it. The bare arrow is a **bounded type
    // parameter** (`reference/types.md § Function types`), so what says what `y` is here is the
    // bound — which is what makes this the one spelling meant for taking a closure that could not
    // default to one.
    "may be a closure literal at a parameter written with the bare arrow" in {
      run("""|apply(g: int -> int = y -> y * 2) -> int = g(21)
             |print(apply())
             |print(apply(x -> x + 1))
             |""".stripMargin) shouldBe "42\n22\n"
    }

    // The boxed spelling is a different road to the same place and broke in a different way: this
    // one was accepted where it was written and refused at the first call that took it. Both are
    // pinned because either alone would have looked fixed.
    "and at one written as a boxed callable, which a call fills rather than the declaration" in {
      run("""|apply(g: &Fn(int) -> int = y -> y * 2) -> int = g(21)
             |print(apply())
             |print(apply(x -> x + 1))
             |""".stripMargin) shouldBe "42\n22\n"
    }

    // `reference/expressions.md § _ — a parameter with the name left out`'s placeholder is the same
    // expression with the parameter unwritten, so it is fixed by the same thing and would be a
    // separate hole if it were not.
    "and may be written with the placeholder, which needs the same thing to say what it stands for" in {
      run("""|apply(g: int -> int = _ * 2) -> int = g(21)
             |print(apply())
             |""".stripMargin) shouldBe "42\n"
    }

    // The error path, and it is the declaration that reports it: nothing calls `apply`, so a
    // closure held to nothing would have been checked by nobody.
    "while a closure default that takes the wrong number of parameters is refused where it stands" in {
      err("""|apply(g: int -> int = (a: int, b: int) -> a + b) -> int = 1
             |print(1)
             |""".stripMargin) should include("this closure takes 2 parameters, and what it is being used as takes 1")
    }
  }

  // `reference/declarations.md § Default parameters and named arguments`: a default may name its
  // declaration's type parameters, which are part of the signature rather than local to it. It is
  // read at each call, at what the call solved them to from the arguments it wrote and the type it
  // is expected to have — and it plays no part in that solve.
  "a default naming its declaration's type parameters" - {
    "reaches an associated function, at whatever the call settles" in {
      run("""|f[T: Zero + Add](x: T, step: T = T.zero()) -> T = x + step
             |print(f(2.5))
             |print(f(7))
             |""".stripMargin) shouldBe "2.5\n7\n"
    }

    // `int(1.5)` is 1 and `real(1.5)` is 1.5, so the two lines can only agree with the expected
    // output if the conversion was made at each call's own `T`.
    "and a conversion written at the parameter's name" in {
      run("""|g[T: Add](x: T, half: T = T(1.5)) -> T = x + half
             |print(g(1.0))
             |print(g(3))
             |""".stripMargin) shouldBe "2.5\n4\n"
    }

    "at both float widths" in {
      run("""|import sysl.math.Float
             |
             |tuned[F: Float](f: F, a4: F = F(440.0)) -> F = f + a4
             |val h: f32 = 0.5
             |val d: f64 = 0.25
             |print(tuned(h))
             |print(tuned(d))
             |""".stripMargin) shouldBe "440.5\n440.25\n"
    }

    "and an explicit argument still stands in its place" in {
      run("""|f[T: Zero + Add](x: T, step: T = T.zero()) -> T = x + step
             |g[T: Add](x: T, half: T = T(1.5)) -> T = x + half
             |print(f(7, 3))
             |print(g(1.0, half = 0.25))
             |""".stripMargin) shouldBe "10\n1.25\n"
    }

    "a generic struct's method may name the struct's parameter" in {
      run("""|struct Acc[T: Zero + Add]
             |    v: T
             |
             |    plus(self, by: T = T.zero()) -> T = self.v + by
             |    mix[U: Add](self, u: U, extra: U = U(1.5), base: T = T(2.5)) -> U = u + extra
             |end Acc
             |
             |print(Acc(2.5).plus())
             |print(Acc(4).plus())
             |print(Acc(4).plus(5))
             |print(Acc(4).mix(1.0))
             |print(Acc(4).mix(1))
             |""".stripMargin) shouldBe "2.5\n4\n9\n2.5\n2\n"
    }

    "and an associated function of a generic type its own" in {
      run("""|struct Acc[T: Zero]
             |    v: T
             |
             |    start(v: T = T.zero()) -> Acc[T] = Acc(v)
             |end Acc
             |
             |val a: Acc[real] = Acc.start()
             |print(a.v)
             |print(Acc.start(3).v)
             |""".stripMargin) shouldBe "0\n3\n"
    }

    // The expected type is one of the two things that may settle `T`, and the default is not.
    "a 'T' only the expected type settles is read there" in {
      run("""|f[T: Zero](x: T = T.zero()) -> T = x
             |val n: u8 = f()
             |print(n)
             |""".stripMargin) shouldBe "0\n"
    }

    "while one nothing but the default could settle is refused as uninferable" in {
      err("""|f[T: Zero](x: T = T.zero()) -> T = x
             |print(f())
             |""".stripMargin) should include("cannot infer the type argument 'T' of 'f' here")
    }

    // The callee's `T`, never the caller's: `g` is generic over a `T` of its own, a `string` here,
    // and the default it fills for `f` is read at the `int` that `f(5)` settles.
    "the parameter is the callee's even inside a caller generic over a 'T' of its own" in {
      run("""|f[T: Zero + Add](x: T, y: T = T.zero()) -> T = x + y
             |g[T](t: T) -> int = f(5)
             |print(g("s"))
             |""".stripMargin) shouldBe "5\n"
    }

    // Read per call, and only by a call that leaves the argument out.
    "is evaluated only by a call that takes it, once each" in {
      run("""|loud[T](v: T) -> T
             |    print("filled")
             |    v
             |
             |f[T: Zero](x: T, y: T = loud(T.zero())) -> T = y
             |print(f(1, 2))
             |print(f(3))
             |print(f(4.5))
             |""".stripMargin) shouldBe "2\nfilled\n0\nfilled\n0\n"
    }

    "a default may still not name another parameter" in {
      err("""|f[T](x: T, y: T = x) -> T = y
             |print(f(1))
             |""".stripMargin) should include("undefined name 'x'")
    }

    // Held to `T`'s bounds where it is written, as a body would be — nothing calls `f` here.
    "a member the bounds do not promise is refused at the declaration" in {
      err("""|f[T](x: T, step: T = T.zero()) -> T = x
             |print(1)
             |""".stripMargin) should include("'zero' needs 'T: ")
    }
  }

  "a trait's default" - {
    // `reference/declarations.md § Default parameters and named arguments`: the trait's declaration
    // is what a call names, so the default is filled before the dispatch and means the same thing
    // either way. Both halves asserted, because a fill that happened after the slot lookup would
    // work through a known type and fail through an object.
    "is the same value through a trait object as through a known type" in {
      run("""|trait Volume
             |    loud(self, times: int = 3) -> int
             |
             |struct Horn
             |    n: int
             |
             |impl Volume for Horn
             |    loud(self, times: int) -> int = self.n * times
             |
             |shout(v: &Volume) -> int = v.loud()
             |
             |var h = Horn(2)
             |var boxed: &Volume = Horn(2)
             |print(h.loud())
             |print(shout(boxed))
             |""".stripMargin) shouldBe "6\n6\n"
    }

    // The other half of the same rule: the implementation supplies the body and the trait supplies
    // the default, so an `impl` writing one of its own is refused rather than silently ignored.
    "and an 'impl' block declares none of its own" in {
      err("""|trait Volume
             |    loud(self, times: int) -> int
             |
             |struct Horn
             |    n: int
             |
             |impl Volume for Horn
             |    loud(self, times: int = 3) -> int = self.n * times
             |
             |print(Horn(2).loud(1))
             |""".stripMargin) should include("a member of an 'impl' block declares no default")
    }
  }

  "what a default may not be" - {
    // The suffix rule, worded as `reference/generics.md § A parameter may carry a default` words
    // the identical rule about a type parameter's default.
    "a parameter with no default may not come after one that has" in {
      val e = err("""|f(a: int = 1, b: int) -> int = a + b
                     |print(f(1, 2))
                     |""".stripMargin)

      e should include("'b' has no default and comes after 'a', which has one")
      e should include("nothing could leave out 'a' and still supply 'b'")
    }

    // `reference/ffi.md § Variadic functions`: C reads the tail relative to the last named
    // argument, so an argument that might be the last parameter or might be the first of the tail
    // leaves nowhere for the tail to begin.
    "a variadic parameter list declares none" in {
      err("""|f(a: int, b: int = 2, ...) -> int = a
             |print(f(1))
             |""".stripMargin) should include("a parameter list with a tail declares no default")
    }

    // The rule that makes a default mean one thing: it is analyzed with nothing local in scope, so
    // a parameter is as undefined there as it is anywhere else outside a body.
    "a default may not name another parameter" in {
      err("""|f(n: int, m: int = n) -> int = n + m
             |print(f(1))
             |""".stripMargin) should include("undefined name 'n'")
    }

    // The same rule from the caller's side, and the one that matters more: without an emptied local
    // scope this would quietly compile and read the *caller's* `n`, which is 100 and not 1.
    "and may not find a caller's local of that name either" in {
      err("""|f(m: int = n) -> int = m
             |
             |go() -> int
             |    var n = 100
             |    f()
             |
             |print(go())
             |""".stripMargin) should include("undefined name 'n'")
    }

    // Checked at the declaration and not at the first call that takes it, which is the whole reason
    // the pass exists: nothing here calls `f`, and the mistake is still reported.
    "a default that is not the parameter's type is refused where it is written" in {
      err("""|f(s: string = 3) -> string = s
             |print(1)
             |""".stripMargin) should include("the default for 's'")
    }

    // The other half of reading a member's default at its parameter's type: what the type gives it
    // is also what it is held to. Nothing calls `grown` here either.
    "and a method's is refused there too, at the type its parameter declares" in {
      err("""|struct Box
             |    n: int
             |
             |    grown(self, by: string = 3) -> int = self.n
             |end Box
             |
             |print(1)
             |""".stripMargin) should include("the default for 'by'")
    }

    // `reference/modules.md § Visibility`, applied to the one part of a signature a call does not
    // write. Without this a caller in another module would have had `secret()` called on their
    // behalf.
    "a public declaration's default may not name something that reaches less far" in {
      errOf(
        "lib.sysl"  -> """|module lib
                          |
                          |private secret() -> int = 7
                          |
                          |f(n: int = secret()) -> int = n
                          |""".stripMargin,
        "main.sysl" -> """|import lib.f
                          |
                          |print(f())
                          |""".stripMargin,
      ) should include("does not reach as far as")
    }

    // And the control: the same shape with both declarations private is no leak at all, so it
    // compiles. Without this the test above would pass for a rule that refused every default.
    "while a private one naming a private one is no leak" in {
      runOf(
        "lib.sysl"  -> """|module lib
                          |
                          |private secret() -> int = 7
                          |
                          |private f(n: int = secret()) -> int = n
                          |
                          |show() -> int = f()
                          |""".stripMargin,
        "main.sysl" -> """|import lib.show
                          |
                          |print(show())
                          |""".stripMargin,
      ) shouldBe "7\n"
    }

    // The absence a closure shares with a `&Fn` call: no names travel with it, so there would be
    // nothing at the call to fill a default from.
    "a closure's parameter declares none" in {
      err("""|var f: &Fn(int) -> int = (x: int = 1) -> x
             |print(f(2))
             |""".stripMargin) should include("a closure's parameter declares no default")
    }

    "a field declares none" in {
      err("""|struct Point
             |    x: int = 0
             |    y: int
             |end Point
             |
             |print(Point(1, 2).x)
             |""".stripMargin) should include("a field declares no default")
    }
  }

  "an argument written by name" - {
    "reaches the parameter it names rather than the one at its position" in {
      run("""|div(top: int, bottom: int) -> int = top / bottom
             |print(div(bottom = 2, top = 10))
             |""".stripMargin) shouldBe "5\n"
    }

    "may follow positional arguments" in {
      run("""|clamp(v: int, lo: int, hi: int) -> int = if v < lo then lo else if v > hi then hi else v
             |print(clamp(50, hi = 10, lo = 0))
             |""".stripMargin) shouldBe "10\n"
    }

    // The two features meeting: a name is what lets a call skip a defaulted parameter and supply a
    // later one, which is the case neither feature can serve on its own.
    "is what lets a call skip a default and still write what comes after it" in {
      run("""|f(a: int, b: int = 20, c: int = 300) -> int = a + b + c
             |print(f(1, c = 3))
             |""".stripMargin) shouldBe "24\n"
    }

    "reaches a struct's constructor, whose fields are its parameters" in {
      run("""|struct Point
             |    x: int
             |    y: int
             |end Point
             |
             |var p = Point(y = 2, x = 1)
             |print(p.x)
             |print(p.y)
             |""".stripMargin) shouldBe "1\n2\n"
    }

    "reaches an enum variant's payload" in {
      run("""|enum Shape
             |    Rect(w: int, h: int)
             |
             |area(s: Shape) -> int
             |    s match
             |        Rect(w, h) -> w * h
             |
             |print(area(Rect(h = 2, w = 30)))
             |""".stripMargin) shouldBe "60\n"
    }

    "reaches a method" in {
      run("""|struct Span
             |    n: int
             |
             |    between(self, lo: int, hi: int) -> int = self.n + hi - lo
             |end Span
             |
             |print(Span(5).between(hi = 30, lo = 10))
             |""".stripMargin) shouldBe "25\n"
    }
  }

  "what a name at a call may not do" - {
    "come before a positional argument" in {
      err("""|f(a: int, b: int) -> int = a + b
             |print(f(a = 1, 2))
             |""".stripMargin) should include("comes after one written by name")
    }

    "name a parameter the declaration does not have" in {
      val e = err("""|f(a: int, b: int) -> int = a + b
                     |print(f(a = 1, c = 2))
                     |""".stripMargin)

      e should include("declares no parameter named 'c'")
      e should include("'a' and 'b'")
    }

    "name one twice" in {
      err("""|f(a: int, b: int) -> int = a + b
             |print(f(a = 1, a = 2))
             |""".stripMargin) should include("'a' is given twice")
    }

    "name one a positional argument already filled" in {
      err("""|f(a: int, b: int) -> int = a + b
             |print(f(1, a = 2))
             |""".stripMargin) should include("already given by position")
    }

    // A call through a `&Fn` carries types and no names (`reference/types.md § Function types`), so
    // there is nothing to match a name against and the message says that rather than "no such
    // parameter".
    "or be written at a call through a callable, which carries no names" in {
      err("""|apply(f: &Fn(int) -> int) -> int = f(n = 1)
             |
             |var c: &Fn(int) -> int = x -> x + 1
             |print(apply(c))
             |""".stripMargin) should include("names an argument")
    }

    // `*extern(A) -> R` is one machine word (`reference/ffi.md § A function's address`), so it
    // carries even less than a trait object does — there is no declaration anywhere behind it to
    // have named anything.
    "or at a call through a function's address" in {
      err("""|double(n: int) -> int = n * 2
             |
             |var f: *extern(int) -> int = &double
             |print(f(n = 5))
             |""".stripMargin) should include("names an argument")
    }
  }

  "the collision with assignment" - {
    // Before this feature `f(x = 1)` stored 1 into `x` and passed the stored value. The named
    // argument now wins, which is what the two halves below pin: the new reading, and the escape.
    "is decided for the named argument" in {
      run("""|f(x: int) -> int = x * 10
             |var x = 7
             |print(f(x = 1))
             |print(x)
             |""".stripMargin) shouldBe "10\n7\n"
    }

    "and parentheses still say the store was meant" in {
      run("""|f(x: int) -> int = x * 10
             |var x = 7
             |print(f((x = 1)))
             |print(x)
             |""".stripMargin) shouldBe "10\n1\n"
    }

    // Only a bare identifier before `=` is a name, so neither of these ever was a parameter and
    // both are the stores they always were.
    "while a store through a field or an element is untouched" in {
      run("""|struct Cell
             |    v: int
             |end Cell
             |
             |f(n: int) -> int = n
             |var c = Cell(0)
             |print(f(c.v = 5))
             |print(c.v)
             |""".stripMargin) shouldBe "5\n5\n"
    }
  }

  /** Cases the chapter says nothing about, which is why they are the ones that break: a default that
   * asks for itself, a name that arrives before inference has run, a list where both kinds of
   * default meet.
   */
  "the corners" - {
    // The one that can hang the compiler rather than refuse a program: filling a default calls the
    // function, which fills the default again. `reference/generics.md § A parameter may carry a
    // default` guards the type-level form of this with a set of what is being filled; the
    // value-level form needs the same guard or none at all.
    "a default that calls its own declaration is refused rather than recursed into" in {
      err("""|again(n: int = again()) -> int = n
             |print(again())
             |""".stripMargin) should include("filling this default calls something that asks for it again")
    }

    "and one that reaches itself through a second declaration is too" in {
      err("""|ping(n: int = pong()) -> int = n
             |pong(n: int = ping()) -> int = n
             |print(ping())
             |""".stripMargin) should include("filling this default calls something that asks for it again")
    }

    // Names are placed before the generic solve reads the arguments, so inference sees them in
    // declared order and not written order. Without that, `T` would be solved from whichever
    // argument happened to be written first.
    "a name reorders the arguments before inference reads them" in {
      run("""|pair[T](first: T, second: T) -> T = first
             |print(pair(second = 1, first = 2))
             |print(pair(second = "b", first = "a"))
             |""".stripMargin) shouldBe "2\na\n"
    }

    "a type parameter's default and a value parameter's meet without interfering" in {
      run("""|struct Pair[A, B = A]
             |    x: A
             |    y: B
             |
             |first[A, B](p: Pair[A, B], fallback: A = 0) -> A = p.x
             |
             |var p: Pair[int] = Pair(y = 2, x = 1)
             |print(first(p))
             |""".stripMargin) shouldBe "1\n"
    }

    "a default may itself be a call written with a name" in {
      run("""|span(lo: int, hi: int) -> int = hi - lo
             |width(n: int = span(hi = 10, lo = 4)) -> int = n
             |print(width())
             |print(width(1))
             |""".stripMargin) shouldBe "6\n1\n"
    }

    "a default may name a constant" in {
      run("""|const limit: int = 9
             |cap(n: int = limit) -> int = n
             |print(cap())
             |""".stripMargin) shouldBe "9\n"
    }

    "a declaration with no parameters says so when given a name" in {
      err("""|f() -> int = 1
             |print(f(a = 1))
             |""".stripMargin) should include("declares no parameter named 'a'")
    }

    "a named argument takes a trailing comma like any other" in {
      run("""|f(a: int, b: int) -> int = a * b
             |print(f(b = 2, a = 3,))
             |""".stripMargin) shouldBe "6\n"
    }

    "a nested function takes a name as well as a default" in {
      run("""|work(base: int) -> int
             |    step(n: int, by: int) -> int = n * 10 + by
             |    step(by = 3, n = base)
             |
             |print(work(1))
             |""".stripMargin) shouldBe "13\n"
    }

    // A zero-sized parameter is dropped from the emitted signature (`reference/declarations.md § Functions`), so a default for one
    // has to be filled and then have nothing left of it — the case where "fill it in" and "emit
    // nothing" have to agree.
    "a default of a zero-sized type is filled and then emitted as nothing" in {
      run("""|nothing() -> unit
             |    var ignored = 1
             |
             |f(u: unit = nothing(), n: int = 4) -> int = n
             |print(f())
             |""".stripMargin) shouldBe "4\n"
    }

    // The two defaults are filled independently, so one that reads the other is the forward
    // reference it looks like rather than something quietly ordered.
    "one default may not name the parameter another fills" in {
      err("""|f(a: int = 1, b: int = a) -> int = a + b
             |print(f())
             |""".stripMargin) should include("undefined name 'a'")
    }

    "a variadic method declares no default either, not only a free function" in {
      err("""|struct Log
             |    n: int
             |
             |    say(self, first: int, rest: int = 0, ...) -> int = first
             |
             |print(Log(1).say(2))
             |""".stripMargin) should include("a parameter list with a tail declares no default")
    }

    // `reference/modules.md § Visibility` through a member rather than a free function: the same
    // leak, one declaration form in.
    "a member's default is held to the same reach its type is" in {
      errOf(
        "lib.sysl"  -> """|module lib
                          |
                          |private secret() -> int = 7
                          |
                          |struct Box
                          |    n: int
                          |
                          |    grown(self, by: int = secret()) -> int = self.n + by
                          |""".stripMargin,
        "main.sysl" -> """|import lib.Box
                          |
                          |print(Box(1).grown())
                          |""".stripMargin,
      ) should include("does not reach as far as")
    }
  }

  /** What the chapters around this one claim, asked of the two new doors into a call rather than
   * assumed to be unaffected by them. A constructor that a name can now reach is a constructor two
   * other rules already had something to say about.
   */
  "what the neighbouring rules say once a name reaches a constructor" - {
    // `reference/modules.md § Visibility`: the positional constructor writes every field, so a
    // restricted one puts it out of reach. A name is a second way in, and the rule has to hold at
    // both.
    "a private field still puts the constructor out of reach" in {
      errOf(
        "lib.sysl"  -> """|module lib
                          |
                          |struct Point
                          |    private x: int
                          |    y: int
                          |""".stripMargin,
        "main.sysl" -> """|import lib.Point
                          |
                          |print(Point(y = 2, x = 1).y)
                          |""".stripMargin,
      ) should include("the constructor")
    }

    // `07`: a struct with `invariant` clauses is checked the moment it is built, and every
    // construction site flows through one place. Reordering by name must not route around it.
    "an invariant is still checked when the fields arrive out of order" in {
      exits("""|struct Span
               |    lo: int
               |    hi: int
               |    invariant lo <= hi
               |
               |var s = Span(hi = 1, lo = 9)
               |print(s.lo)
               |""".stripMargin)
    }

    "while the same fields in a satisfying order build" in {
      run("""|struct Span
             |    lo: int
             |    hi: int
             |    invariant lo <= hi
             |
             |var s = Span(hi = 9, lo = 1)
             |print(s.hi - s.lo)
             |""".stripMargin) shouldBe "8\n"
    }

    // `reference/ffi.md § Variadic functions`: a variadic's tail stands at no parameter, so nothing
    // in it has a name — and since a positional argument may not follow a named one, a call that
    // names anything has no tail left.
    "a variadic call may name its declared parameters only while it writes no tail" in {
      run("""|first(a: int, ...) -> int = a
             |print(first(a = 5))
             |""".stripMargin) shouldBe "5\n"

      err("""|first(a: int, ...) -> int = a
             |print(first(a = 5, 6))
             |""".stripMargin) should include("comes after one written by name")
    }

    // `reference/modules.md § val — a thing`: a `val` is laid down before any body runs, in a state belonging to no function. A
    // default filled there is analyzed in the callee's terms and not in that state's.
    "a module-level 'val' may be built by a call that takes a default" in {
      run("""|origin(x: int = 3, y: int = 4) -> int = x * 10 + y
             |
             |val here: int = origin(y = 9)
             |print(here)
             |""".stripMargin) shouldBe "39\n"
    }

    // `reference/traits.md § A default may assume exactly what its own trait declares`: a trait's
    // default *body* is copied to each implementing type. A parameter default on that same member
    // is a second thing being copied, and the two travel together.
    "a trait's default body and its parameter default are copied together" in {
      run("""|trait Step
             |    size(self) -> int
             |    walk(self, times: int = 2) -> int = self.size() * times
             |
             |struct Foot
             |    n: int
             |
             |impl Step for Foot
             |    size(self) -> int = self.n
             |
             |print(Foot(3).walk())
             |print(Foot(3).walk(5))
             |""".stripMargin) shouldBe "6\n15\n"
    }

    // `reference/attributes.md § What a test may be`: a `@test` function takes no parameters, because `sysl test` calls it with
    // nothing. A default looks like it should rescue that — every parameter has a value, so the
    // call could be written with none — and it does not, which is right: the runner's call is
    // emitted rather than analyzed, so there is nothing there to fill it.
    "a '@test' function's parameter is refused even carrying a default" in {
      err("""|@test
             |works(n: int = 7) -> unit
             |    assert(n == 7, "n")
             |""".stripMargin) should include("takes no parameters")
    }
  }

  "the shapes the parser reads" - {
    "a default is held on the parameter it was written after" in {
      prog("""|f(a: int, b: int = 2) -> int
              |    a
              |""".stripMargin) shouldBe
        List(FuncDecl(
          "f",
          Nil,
          List(
            Param("a", NamedType("int")),
            Param("b", NamedType("int"), default = Some(IntLit(2, None))),
          ),
          Some(NamedType("int")),
          List(ExprStmt(Ident("a"))),
        ))
    }

    "and a named argument is a node of its own, not an assignment" in {
      prog("f(a = 1)") shouldBe List(ExprStmt(Call(Ident("f"), List(NamedArg("a", IntLit(1, None))))))
    }

    "while a parenthesized one is still the assignment it reads as" in {
      prog("f((a = 1))") shouldBe
        List(ExprStmt(Call(Ident("f"), List(Assign("=", Ident("a"), IntLit(1, None))))))
    }
  }
}
