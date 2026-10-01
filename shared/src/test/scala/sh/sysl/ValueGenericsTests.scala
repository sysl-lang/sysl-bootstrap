package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Value generics (`reference/generics.md § A parameter may stand for a value`) — a parameter
 * standing for a **value** rather than a type, written `[const N: usize]`, which is what lets one
 * declaration cover every array length.
 */
class ValueGenericsTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "the parameter list" - {
    "reads a value parameter beside a type one" in {
      run("""f[const N: usize, T](xs: [N]T) -> usize = N
            |var a: [3]int = [1, 2, 3]
            |print(f(a))""".stripMargin) shouldBe "3\n"
    }

    /** A value parameter with no type is a reader who has understood the feature and mis-spelled it,
     * which is the case worth a sentence. Getting one required raising it where the `:` belongs
     * rather than after the whole form: two failures are ranked by how far each got, so an
     * alternative written after the form is beaten by the form's own failure further along the line.
     */
    "wants the type its argument must have" in {
      err("f[const N](xs: [N]int) = 0\nprint(1)") should include("a value parameter needs the type")
    }
  }

  "an array length" - {
    "is inferred from the argument" in {
      run("""len[const N: usize](xs: [N]int) -> usize = N
            |var a: [4]int = [1, 2, 3, 4]
            |print(len(a))""".stripMargin) shouldBe "4\n"
    }

    "is a constant inside the body, so it may be looped to" in {
      run("""total[const N: usize](xs: [N]int) -> int
            |    var t = 0
            |    for i in 0..<N do t = t + xs[i]
            |    t
            |var a: [3]int = [10, 20, 30]
            |print(total(a))""".stripMargin) shouldBe "60\n"
    }

    "makes two lengths two instantiations" in {
      run("""len[const N: usize](xs: [N]int) -> usize = N
            |var a: [2]int = [1, 2]
            |var b: [5]int = [1, 2, 3, 4, 5]
            |print(len(a))
            |print(len(b))""".stripMargin) shouldBe "2\n5\n"
    }
  }

  /** A **struct** carries value parameters too, and its arguments are written out rather than
   * inferred — which is the one place the grammar cannot tell which kind of argument it is reading.
   * `Buf[4]` and `Buf[int]` are one shape to a parser, so the declaration decides.
   */
  "a struct" - {
    "may carry a value parameter, written out at the use" in {
      run("""struct Buf[const N: usize]
            |    data: [N]byte
            |var b: Buf[4] = Buf([0u8, 0u8, 0u8, 0u8])
            |print(b.data.len)""".stripMargin) shouldBe "4\n"
    }

    // Two lengths are two types, which is the whole of what putting a value in a type identity
    // means: the layouts differ and neither stands where the other is wanted.
    "at two lengths is two types" in {
      run("""struct Buf[const N: usize]
            |    data: [N]byte
            |var a: Buf[2] = Buf([1u8, 2u8])
            |var b: Buf[4] = Buf([1u8, 2u8, 3u8, 4u8])
            |print(a.data.len + b.data.len)""".stripMargin) shouldBe "6\n"
    }

    "and does not accept one for the other" in {
      err("""struct Buf[const N: usize]
            |    data: [N]byte
            |var a: Buf[2] = Buf([1u8, 2u8])
            |var b: Buf[4] = a
            |print(b.data.len)""".stripMargin) should include("Buf[2]")
    }

    // A bare name in a value-argument position is read as the value it names, which is what lets one
    // declaration's parameter be passed straight to another's.
    "takes an enclosing block's value parameter as its argument" in {
      run("""struct Buf[const N: usize]
            |    data: [N]byte
            |wrap[const N: usize](xs: [N]byte) -> usize
            |    var b: Buf[N] = Buf(xs)
            |    b.data.len
            |var a: [3]byte = [1u8, 2u8, 3u8]
            |print(wrap(a))""".stripMargin) shouldBe "3\n"
    }

    // And a declared constant, since a value parameter is a `const` whose value the use supplies
    // and the two are the same kind of thing (`reference/modules.md § const — a value`).
    "and a declared constant" in {
      run("""const SIZE: usize = 3usize
            |struct Buf[const N: usize]
            |    data: [N]byte
            |var b: Buf[SIZE] = Buf([1u8, 2u8, 3u8])
            |print(b.data.len)""".stripMargin) shouldBe "3\n"
    }

    /** A **member's own** list, which is a different list from the type's: the type's are fixed by
     * the receiver and the member's are solved at the call. The two meet in the lowered function, so
     * the kinds have to come from both sides — a member's own `const` read as an ordinary type
     * parameter would stand at an opaque type and its body could not do arithmetic on it.
     */
    "a method declares one of its own, solved from the argument" in {
      run("""struct Sum
            |    seen: usize
            |    take[const N: usize](*self, xs: [N]int) -> usize
            |        self.seen = self.seen + N
            |        N
            |end Sum
            |var s = Sum(0usize)
            |print(s.take([1, 2, 3]))
            |print(s.take([4, 5]))
            |print(s.seen)""".stripMargin) shouldBe "3\n2\n5\n"
    }
  }

  /** `reference/generics.md § A parameter may stand for a value` admits **integers, `bool`, `char`
   * and enum values** on day one, for one reason: a value in a type's identity must compare and
   * must mangle, and each of those does. The argument travels as a number whichever it is, and the
   * parameter's declared type is what reads it back — which is why a `bool` parameter is a `bool`
   * in a body rather than the `0` it is stored as.
   *
   * Floats and strings are excluded by that same rule, and §9 says why: `NaN != NaN` would make a
   * type unequal to itself, and two spellings of one text are not one value until strings intern.
   */
  "the admissible types" - {
    "take a bool, written out" in {
      run("""struct Flag[const B: bool]
            |    v: int
            |var a: Flag[true] = Flag(1)
            |print(a.v)""".stripMargin) shouldBe "1\n"
    }

    // The half that a `BigInt` alone would have got wrong: read back in the body, `B` has to be the
    // `bool` it was declared, not the number it travelled as.
    "and reads it back as a bool, not as the number it travelled as" in {
      run("""struct Flag[const B: bool]
            |    v: int
            |impl[const B: bool] Display for Flag[B]
            |    display(self, out: *Writer, fmt: FormatSpec) =
            |        display_str(if B then "on" else "off", out, fmt)
            |var a: Flag[true] = Flag(1)
            |var b: Flag[false] = Flag(2)
            |print(a)
            |print(b)""".stripMargin) shouldBe "on\noff\n"
    }

    "a char" in {
      run("""struct Sep[const C: char]
            |    v: int
            |var a: Sep['x'] = Sep(1)
            |print(a.v)""".stripMargin) shouldBe "1\n"
    }

    /** A **simple** enum's value *is* its identity (`09`) — there is nothing else telling two of its
     * variants apart — so its tag is exactly the number a type's identity wants.
     */
    "and a simple enum's variant" in {
      run("""enum Mode
            |    Fast
            |    Slow
            |struct Run[const M: Mode]
            |    v: int
            |var a: Run[Fast] = Run(1)
            |print(a.v)""".stripMargin) shouldBe "1\n"
    }

    // Two variants are two types, and the diagnostic names them **as written**. A message reading
    // `Run[1]` would name something no program wrote — the reader would have to know an internal
    // tag to recognise their own type.
    "so two variants are two types, named the way they were written" in {
      val e = err("""enum Mode
                    |    Fast
                    |    Slow
                    |struct Run[const M: Mode]
                    |    v: int
                    |var a: Run[Fast] = Run(1)
                    |var b: Run[Slow] = a
                    |print(b.v)""".stripMargin)

      e should include("declared Run[Slow] but the value is Run[Fast]")
    }

    /** And the parameter is that enum inside a body, pinned by comparing it against a variant and
     * reading the answer.
     *
     * **This used to assert a refusal**, because a dataless enum had no `==` at all — so the claim
     * "`M` was read as a `Mode`" rested on the complaint naming `Mode`. A simple enum is now `Eq`
     * (card 0033), which makes the same claim answerable directly and much more strongly: the
     * program runs, and it prints the branch the argument chose.
     */
    "and inside a body the parameter is a value of that enum" in {
      run("""enum Mode
            |    Fast
            |    Slow
            |struct Run[const M: Mode]
            |    v: int
            |impl[const M: Mode] Display for Run[M]
            |    display(self, out: *Writer, fmt: FormatSpec) =
            |        display_str(if M == Fast then "y" else "n", out, fmt)
            |var a: Run[Fast] = Run(1)
            |var b: Run[Slow] = Run(2)
            |print(a, b)""".stripMargin) shouldBe "y n\n"
    }
  }

  /** A parameter's **kind** is the declaration's to state, and getting it wrong is refused rather
   * than guessed at. Both of these compiled silently before they were caught, which is the worst
   * outcome available: an array standing at a length nobody wrote.
   */
  "a type parameter where a value belongs" - {
    "is refused in a function's signature" in {
      err("""f[T](xs: [T]int) -> usize = 0
            |var a: [2]int = [1, 2]
            |print(f(a))""".stripMargin) should
        include("'T' is a type parameter, and an array's length is a value rather than a type")
    }

    /** The function case is caught at the **declaration** and has to be: `unify` reads a length off
     * the argument's type and binds `T` to the 2 it found there, so by the time the signature
     * resolves, `T` holds a value and is indistinguishable from one declared `const`.
     */
    "and the message names the spelling that was meant" in {
      err("""f[T](xs: [T]int) -> usize = 0
            |var a: [2]int = [1, 2]
            |print(f(a))""".stripMargin) should include("declared 'const T: usize'")
    }

    "and in an 'impl', where it silently became a block for '[0]T'" in {
      err("""trait Tag
            |    tag(self) -> string
            |impl[N, T] Tag for [N]T
            |    tag(self) -> string = "any"
            |var a: [2]int = [1, 2]
            |print(a.tag())""".stripMargin) should
        include("'N' is a type parameter, and an array's length is a value rather than a type")
    }

    // The other direction: a type written where the declaration wrote `const`. A type-argument list
    // reads as types until the declaration says otherwise, so this is the likely mistake and gets
    // its own sentence rather than the generic "must be a constant".
    "while a type written as a value argument says which of the two the slot is" in {
      err("""struct Buf[const N: usize]
            |    data: [N]byte
            |var b: Buf[int] = Buf([])
            |print(b.data.len)""".stripMargin) should
        include("'int' is a type, and this argument stands where the declaration wrote 'const'")
    }

    "and a value written where a type argument belongs says the same in reverse" in {
      err("""struct Box[T]
            |    v: T
            |var b: Box[4] = Box(1)
            |print(b.v)""".stripMargin) should
        include("a value stands here, and this argument is a type")
    }
  }

  "the ordinary cases that have to keep working" - {
    "two value parameters at once" in {
      run("""f[const A: usize, const B: usize](xs: [A]int, ys: [B]int) -> usize = A + B
            |var p: [2]int = [1, 2]
            |var q: [3]int = [1, 2, 3]
            |print(f(p, q))""".stripMargin) shouldBe "5\n"
    }

    // A parameter is the outermost binding of its name, not the only one.
    "a local of the same name shadows the parameter" in {
      run("""f[const N: usize](xs: [N]int) -> usize
            |    var N: usize = 99usize
            |    N
            |var a: [3]int = [1, 2, 3]
            |print(f(a))""".stripMargin) shouldBe "99\n"
    }

    // Zero is the placeholder the abstract walk stands a value parameter at, so an array that is
    // *really* empty is the one case where the placeholder and the answer coincide.
    "and the empty array reaches the library's block like any other" in {
      run("var a: [0]int = []\nprint(a)") shouldBe "[]\n"
    }

    // Refused at the argument itself now, since -1 is not a `usize` at all — before the array it
    // would have sized is ever laid out.
    "while a negative length is refused as it always was" in {
      err("""struct Buf[const N: usize]
            |    data: [N]byte
            |var b: Buf[-1] = Buf([])
            |print(b.data.len)""".stripMargin) should include("this argument does not fit usize: -1")
    }
  }

  /** The whole point of the feature: one `impl[const N: usize, T: Display] Display for [N]T` in the
   * library, so a fixed array prints the way every slice already does.
   *
   * It is what the coherence half bought. An array's length used to be part of its shape *key* —
   * `[3]` and `[4]` were two shapes — because no parameter could stand for a length; now it is an
   * argument to one shape, and a block may be generic over it.
   */
  "the motivating case" - {
    "one impl covers every array length" in {
      run("""var a: [3]int = [1, 2, 3]
            |var b: [2]int = [7, 8]
            |print(a)
            |print(b)""".stripMargin) shouldBe "[1, 2, 3]\n[7, 8]\n"
    }

    "reaches an element type the library never saw" in {
      run("""struct P
            |    x: int
            |impl Display for P
            |    display(self, out: *Writer, fmt: FormatSpec) = out.write(str(self.x).bytes)
            |var a: [2]P = [P(1), P(2)]
            |print(a)""".stripMargin) shouldBe "[1, 2]\n"
    }

    /** An `impl` on a **named** generic type, whose members read the value parameter — which is a
     * different `MemberHome` from the one a block matching a *shape* builds, and the two had to be
     * told separately which parameters stand for values.
     *
     * The assertion is the *run*, not a diagnostic, because what went wrong was silent: the members
     * were walked at their definition with the parameter's name standing for nothing, and the
     * complaint about that was dropped by the very pass making it. Nothing failed until an unrelated
     * change stopped the pass swallowing what it says about a name.
     */
    "an 'impl' on a named generic type reads its value parameter in a member" in {
      run("""struct Run[const N: usize]
            |    v: int
            |impl[const N: usize] Display for Run[N]
            |    display(self, out: *Writer, fmt: FormatSpec) =
            |        display_str(str(self.v) + "/" + str(N), out, fmt)
            |var a: Run[3] = Run(7)
            |var b: Run[5] = Run(8)
            |print(a)
            |print(b)""".stripMargin) shouldBe "7/3\n8/5\n"
    }

    /** And a type's **own** method reading its own value parameter, which is a third `MemberHome`
     * from a third file — the hole was in each of them separately, so each is pinned separately.
     */
    "a type's own method reads the type's value parameter" in {
      run("""struct Buf[const N: usize]
            |    used: usize
            |    room(self) -> usize = N - self.used
            |end Buf
            |var a: Buf[8] = Buf(3usize)
            |var b: Buf[2] = Buf(1usize)
            |print(a.room())
            |print(b.room())""".stripMargin) shouldBe "5\n1\n"
    }

    // The length reaches the *symbol*, so two lengths are two emitted bodies rather than one
    // compiled at whichever arrived first — the same thing `mangleOne` pins for a function.
    "renders an array of arrays, which is two lengths at once" in {
      run("""var a: [2][3]int = [[1, 2, 3], [4, 5, 6]]
            |print(a)""".stripMargin) shouldBe "[[1, 2, 3], [4, 5, 6]]\n"
    }

    /** A block written for one length is the more specific of the two and answers first, which is
     * `shapeOwners`' ordering — the same rule that makes `[]Point` beat `[]T` (`reference/traits.md § override — when the overlap is deliberate`),
     * one level up from where it used to apply.
     *
     * The trait is the program's own because the library's would put this against the orphan rule:
     * `Display` is the library's and nothing in `[2]T` is this module's, so neither block would
     * have a home. That refusal is unchanged by any of this, and `ImplShapeErrorTests` holds it.
     */
    "a block written for one length beats the one written for every length" in {
      run("""trait Tag
            |    tag(self) -> string
            |impl[const N: usize, T] Tag for [N]T
            |    tag(self) -> string = "any"
            |impl[T] Tag for [2]T
            |    tag(self) -> string = "pair"
            |var a: [2]int = [1, 2]
            |var b: [3]int = [1, 2, 3]
            |print(a.tag())
            |print(b.tag())""".stripMargin) shouldBe "pair\nany\n"
    }
  }

  "what is refused" - {
    /** `[N + 1]T` needs the compiler to decide when two *expressions* denote one length — that `N +
     * 1` and `1 + N` are one type — which is type-level arithmetic and a separate feature
     * (`reference/generics.md § A parameter may stand for a value`). Rust keeps it unstable for the
     * same reason long after const generics shipped.
     *
     * Refusing is not a limitation admitted reluctantly: left alone the length resolves to whatever
     * the placeholder made it, so the array is silently the wrong size.
     */
    "arithmetic on a length in a type" in {
      val e = err("""f[const N: usize](xs: [N]int) -> [N + 1]int = xs
                    |print(1)""".stripMargin)

      e should include("does arithmetic on 'N'")
      e should include("type-level arithmetic")
    }

    "including where the parameter is buried in the expression" in {
      err("""f[const N: usize](xs: [N]int) -> [2 * N]int = xs
            |print(1)""".stripMargin) should include("does arithmetic on 'N'")
    }

    /** The **parameter** position is why this is asked at the declaration rather than only where the
     * type gets built. Nothing unifies with `[N + 1]int`, so a call cannot solve for `N` and says
     * so — true, and about the wrong line. The call still says it, since errors are collected rather
     * than stopped at; what the declaration-time check buys is that the reader meets the cause
     * **first**, at the line that has to change.
     */
    "and in a parameter, which no call could ever be solved against" in {
      val e = err("""f[const N: usize](xs: [N + 1]int) -> usize = 0
                    |var a: [3]int = [1, 2, 3]
                    |print(f(a))""".stripMargin)

      e should include("does arithmetic on 'N'")
      e.indexOf("does arithmetic on 'N'") should be < e.indexOf("annotate the expected type")
    }

    // A member of an `impl` is the other declaration form carrying value parameters, and it is held
    // to the same rule — here in a local's type, which the resolution catches rather than the
    // declaration-time walk.
    "and in a type written inside a member's body" in {
      err("""trait Grow
            |    grow(self) -> usize
            |impl[const N: usize, T] Grow for [N]T
            |    grow(self) -> usize
            |        var b: [N * 2]int
            |        b.len
            |var a: [2]int = [1, 2]
            |print(a.grow())""".stripMargin) should include("does arithmetic on 'N'")
    }

    // And in a local of an ordinary function, which is the same catch one declaration form over.
    "and in a local's type inside a function" in {
      err("""f[const N: usize](xs: [N]int) -> usize
            |    var b: [N + 1]int
            |    b.len
            |var a: [3]int = [1, 2, 3]
            |print(f(a))""".stripMargin) should include("does arithmetic on 'N'")
    }

    /** A length measuring a **type** parameter is a different thing and stays legal: `sizeof(T)` is
     * a number the type argument fixes outright, so nothing has an equation to solve.
     */
    "while a length measuring a type parameter is untouched" in {
      run("""f[T](x: T) -> usize
            |    var buf: [sizeof(T) * 2 + 1]u8
            |    buf.len
            |print(f(1))""".stripMargin) shouldBe "9\n"
    }

    // A body may compute with `N` as freely as with any other `usize` — it is only a *type* that
    // may not carry the result.
    "and a body computes with the parameter freely" in {
      run("""f[const N: usize](xs: [N]int) -> usize = N * 2usize + 1usize
            |var a: [3]int = [1, 2, 3]
            |print(f(a))""".stripMargin) shouldBe "7\n"
    }
  }

  /** A **trait** may take a value parameter too, as Rust's traits take const generics: everything a
   * struct's value parameter does — declared, named at an `impl`, a bound and an object, defaulted,
   * held to its type — a trait's does in the same words.
   */
  "a trait's value parameter" - {
    val bytes = """trait Bytes[const N: usize]
                  |    bytes(self) -> [N]u8
                  |    size(self) -> usize = N
                  |struct W
                  |    x: u8
                  |""".stripMargin

    /** It was parsed and dropped, so `N` was read as a **type** parameter: `impl Sized[int]` compiled
     * and `impl Sized[4]` was refused as a value where a type belongs. A type there is the mistake
     * now, said in the words a struct's value parameter says it in.
     */
    "is a value, so a type written for it is refused" in {
      err("""trait Sized[const N: usize]
            |    size(self) -> usize
            |struct W
            |    x: int
            |impl Sized[int] for W
            |    size(self) -> usize = 3
            |print(1)""".stripMargin) should include(
        "'int' is a type, and this argument stands where the declaration wrote 'const'")
    }

    "is fixed by an impl, and the members read it in their signatures and bodies" in {
      run(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [4]u8 = [self.x, 0, 0, 0]
                    |val w = W(7)
                    |print(w.size())
                    |print(w.bytes()[0])
                    |print(w.bytes().len)""".stripMargin) shouldBe "4\n7\n4\n"
    }

    "and an impl's signature must agree with the value it fixed" in {
      err(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [2]u8 = [self.x, 0]
                    |print(1)""".stripMargin) should include("returns [2]byte, but trait 'Bytes' declares [4]byte")
    }

    "is named by a bound" in {
      run(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [4]u8 = [self.x, 0, 0, 0]
                    |f[T: Bytes[4]](x: T) -> usize = x.size() * 10
                    |print(f(W(1)))""".stripMargin) shouldBe "40\n"
    }

    "and a bound at another value is not met" in {
      err(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [4]u8 = [self.x, 0, 0, 0]
                    |f[T: Bytes[2]](x: T) -> usize = x.size()
                    |print(f(W(1)))""".stripMargin) should include("Bytes[2]")
    }

    "is solved from a use where a bound leaves it open" in {
      run(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [4]u8 = [self.x, 0, 0, 0]
                    |first[T: Bytes[N], const N: usize](x: T, pad: [N]u8) -> usize = x.size() + pad.len
                    |var p: [4]u8 = [0, 0, 0, 0]
                    |print(first(W(1), p))""".stripMargin) shouldBe "8\n"
    }

    /** Two values are two implementations, exactly as two type arguments are
     * (`MultipleImplementationTests`): an object's table points at the one its type names, and a
     * member taking no argument has nothing a direct call could pick by.
     */
    "at two values is two implementations, each object reaching its own" in {
      run("""trait Width[const N: usize]
            |    width(self) -> usize = N
            |struct X
            |    x: int
            |impl Width[2] for X
            |impl Width[4] for X
            |val two: &Width[2] = X(0)
            |val four: &Width[4] = X(0)
            |print(two.width())
            |print(four.width())""".stripMargin) shouldBe "2\n4\n"
    }

    "and a direct call with no argument to pick by is told so" in {
      err("""trait Width[const N: usize]
            |    width(self) -> usize = N
            |struct X
            |    x: int
            |impl Width[2] for X
            |impl Width[4] for X
            |print(X(0).width())""".stripMargin) should include(
        "'width' comes from 2 implementations of one trait on X, and the arguments do not say which was meant")
    }

    "but twice at one value is refused" in {
      err("""trait Width[const N: usize]
            |    width(self) -> usize = N
            |struct X
            |    x: int
            |impl Width[2] for X
            |impl Width[2] for X
            |print(1)""".stripMargin) should include("already implements")
    }

    "may be implemented by a generic impl abstracting over a value" in {
      run(bytes + """struct Bits[const M: usize]
                    |    raw: [M]u8
                    |impl[const M: usize] Bytes[M] for Bits[M]
                    |    bytes(self) -> [M]u8 = self.raw
                    |var r: [3]u8 = [9, 8, 7]
                    |val b = Bits(r)
                    |print(b.size())
                    |print(b.bytes()[2])""".stripMargin) shouldBe "3\n7\n"
    }

    "takes its default where an impl leaves it out" in {
      run("""trait Hash[const N: usize = 32]
            |    digest_len(self) -> usize = N
            |struct Sha
            |    x: int
            |impl Hash for Sha
            |f[T: Hash[32]](x: T) -> usize = x.digest_len()
            |print(f(Sha(0)))""".stripMargin) shouldBe "32\n"
    }

    "and a default may use an earlier value parameter" in {
      run("""trait Pair[const A: usize, const B: usize = A]
            |    total(self) -> usize = A + B
            |struct P
            |    x: int
            |impl Pair[3] for P
            |print(P(0).total())""".stripMargin) shouldBe "6\n"
    }

    "and a default that does not fit is refused at the trait" in {
      err("""trait Hash[const N: u8 = 300]
            |    digest_len(self) -> u8 = N
            |print(1)""".stripMargin) should include("the default for 'N' does not fit byte: 300")
    }

    "and a value argument that does not fit is refused" in {
      err("""trait Small[const N: u8]
            |    n(self) -> u8 = N
            |struct S
            |    x: int
            |impl Small[300] for S
            |print(1)""".stripMargin) should include("this argument does not fit byte: 300")
    }

    "is fixed by a trait object" in {
      run(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [4]u8 = [self.x, 0, 0, 0]
                    |val o: &Bytes[4] = W(5)
                    |print(o.size())
                    |print(o.bytes()[0])""".stripMargin) shouldBe "4\n5\n"
    }

    "and an object leaving it out takes the default" in {
      run("""trait Hash[const N: usize = 32]
            |    digest_len(self) -> usize = N
            |struct Sha
            |    x: int
            |impl Hash for Sha
            |val o: &Hash = Sha(0)
            |print(o.digest_len())""".stripMargin) shouldBe "32\n"
    }

    "or is refused where there is none" in {
      err(bytes + """impl Bytes[4] for W
                    |    bytes(self) -> [4]u8 = [self.x, 0, 0, 0]
                    |val o: &Bytes = W(5)
                    |print(1)""".stripMargin) should include("trait 'Bytes' takes 1 type argument")
    }
  }
}
