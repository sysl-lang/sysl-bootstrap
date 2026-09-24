package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** The exhaustive pass over closures (`reference/expressions.md § Closures`–`§8`): every kind of
 * thing that can be captured, every place a captured name can have come from, every position a
 * callable can stand in, and the way each of those goes wrong.
 *
 * What earns a test here is a case where the answer could plausibly differ from the one beside it —
 * a type whose capture costs a retain rather than a copy, a binding form the free-variable walk has
 * to know about, a position where the type is written rather than inferred.
 */
class ClosureEdgeTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** A one-argument caller, which most of these go through. */
  private val apply = "apply(f: int -> int, x: int) -> int = f(x)\n"

  "every kind of value can be captured" - {
    "a narrow integer, which is copied in at its own width" in {
      run(apply + """var b: u8 = 7
                    |
                    |print(apply(k -> k + int(b), 0))
                    |""".stripMargin) shouldBe "7\n"
    }

    "a wide integer" in {
      run(apply + """var big: i64 = 100i64
                    |
                    |print(apply(k -> k + int(big), 0))
                    |""".stripMargin) shouldBe "100\n"
    }

    "a float" in {
      run("""var fl = 1.5
            |var f: &Fn(int) -> string = k -> str(fl)
            |
            |print(f(0))
            |""".stripMargin) shouldBe "1.5\n"
    }

    "a bool and a char" in {
      run("""var bo = true
            |var ch = 'A'
            |var f: &Fn(int) -> string = k -> if bo then string(ch) else "no"
            |
            |print(f(0))
            |""".stripMargin) shouldBe "A\n"
    }

    "a string, which takes a share of the buffer rather than copying the bytes" in {
      run("""var s = "hi"
            |var f: &Fn(int) -> string = k -> s + "!"
            |
            |print(f(0))
            |""".stripMargin) shouldBe "hi!\n"
    }

    "a fixed array, which is a value and is copied whole" in {
      run(apply + """var arr = [1, 2, 3]
                    |
                    |print(apply(k -> arr[1], 0))
                    |""".stripMargin) shouldBe "2\n"
    }

    "a slice, which views what it viewed" in {
      run(apply + """var arr = [1, 2, 3]
                    |var sl = arr[..]
                    |
                    |print(apply(k -> sl[2], 0))
                    |""".stripMargin) shouldBe "3\n"
    }

    "a struct by value" in {
      run(apply + """struct P
                    |    x: int
                    |    y: int
                    |
                    |var st = P(3, 4)
                    |
                    |print(apply(k -> st.x + st.y, 0))
                    |""".stripMargin) shouldBe "7\n"
    }

    "a counted reference, which is retained" in {
      run(apply + """struct P
                    |    x: int
                    |    y: int
                    |
                    |var rf: &P = P(5, 6)
                    |
                    |print(apply(k -> rf.x + rf.y, 0))
                    |""".stripMargin) shouldBe "11\n"
    }

    "a weak reference, which is not" in {
      run("""struct P
            |    x: int
            |
            |var rf: &P = P(5)
            |var wk: weak P = rf
            |var f: &Fn(int) -> int = k -> wk.get() match
            |    Some(p) -> p.x
            |    None -> -1
            |
            |print(f(0))
            |""".stripMargin) shouldBe "5\n"
    }

    "a raw pointer, which is the unsafe tier and is unchanged by being captured" in {
      run(apply + """var arr = [1u8, 2u8, 3u8]
                    |var pt = &arr[0]
                    |
                    |print(apply(k -> int(pt[1]), 0))
                    |""".stripMargin) shouldBe "2\n"
    }

    "a simple enum and a data enum" in {
      run("""enum Color: u8
            |    Red = 1
            |    Blue = 2
            |
            |enum Shape
            |    Circle(r: int)
            |    Nothing
            |
            |var col = Color.Blue
            |var sh = Circle(9)
            |var f: &Fn(int) -> int = k -> sh match
            |    Circle(r) -> r + int(u8(col))
            |    Nothing -> 0
            |
            |print(f(0))
            |""".stripMargin) shouldBe "11\n"
    }

    "a tuple" in {
      run(apply + """var tp = (1, "two")
                    |
                    |print(apply(k -> tp.0, 0))
                    |""".stripMargin) shouldBe "1\n"
    }

    "another callable, so a closure may capture a closure" in {
      run("""var inner: &Fn(int) -> int = k -> k * 2
            |var outer: &Fn(int) -> int = k -> inner(k) + 1
            |
            |print(outer(5))
            |""".stripMargin) shouldBe "11\n"
    }

    "several at once, in the order the body first names them" in {
      val out = ir(apply + """var a = 1
                             |var b = 2
                             |var c = 3
                             |
                             |print(apply(k -> c + a, 0))
                             |""".stripMargin)

      // `b` is never named, so it is not a field; `c` comes first because it is named first.
      envs(out) shouldBe List("i32, i32")
    }
  }

  "every place a captured name can come from" - {
    "the enclosing function's parameter" in {
      run(apply + """outer(n: int) -> int = apply(k -> k + n, 1)
                    |
                    |print(outer(10))
                    |""".stripMargin) shouldBe "11\n"
    }

    "a local 'var'" in {
      run(apply + """var n = 10
                    |
                    |print(apply(k -> k + n, 1))
                    |""".stripMargin) shouldBe "11\n"
    }

    "a local 'val', which stays written-once inside the closure" in {
      err("""var f: &Fn(int) -> int = k ->
            |    val n = 1
            |    n = 2
            |    k
            |""".stripMargin) should include("a 'val' is written once")
    }

    "a 'for' loop variable" in {
      run(apply + """var total = 0
                    |
                    |for i in 0..<3 do total += apply(k -> k + i, 0)
                    |
                    |print(total)
                    |""".stripMargin) shouldBe "3\n"
    }

    "a match-arm binding" in {
      run(apply + """enum Shape
                    |    Circle(r: int)
                    |    Square(s: int)
                    |
                    |var sh = Circle(3)
                    |
                    |sh match
                    |    Circle(r) -> print(apply(k -> k + r, 1))
                    |    Square(s) -> print(s)
                    |""".stripMargin) shouldBe "4\n"
    }

    "'self' inside a method" in {
      run(apply + """struct Box2
                    |    v: int
                    |
                    |    twice(self) -> int = apply(x -> x + self.v, self.v)
                    |
                    |print(Box2(5).twice())
                    |""".stripMargin) shouldBe "10\n"
    }

    "a capture of the closure around it, which reaches through" in {
      run(apply + """var k = 100
                    |
                    |print(apply(x -> apply(y -> y + k, x), 5))
                    |""".stripMargin) shouldBe "105\n"
    }

    "a module-level declaration, which is reached and not captured" in {
      // A `const` and a top-level `val` are declarations rather than locals, so a body naming one
      // reaches it the way any other function does — the environment stays empty.
      val out = ir("""const LIMIT: int = 7
                     |
                     |apply(f: int -> int, x: int) -> int = f(x)
                     |
                     |print(apply(k -> k + LIMIT, 1))
                     |""".stripMargin)

      envs(out) shouldBe List("")
    }

    "a local declared inside the body itself, which is not a capture at all" in {
      val out = ir("""var f: &Fn(int) -> int = k ->
                     |    var own = 5
                     |    k + own
                     |
                     |print(f(1))
                     |""".stripMargin)

      envs(out) shouldBe List("")
    }
  }

  "every arity the library declares" - {
    "none through four" in {
      run("""zero(f: () -> int) -> int = f()
            |one(f: int -> int) -> int = f(1)
            |two(f: (int, int) -> int) -> int = f(1, 2)
            |three(f: (int, int, int) -> int) -> int = f(1, 2, 3)
            |four(f: (int, int, int, int) -> int) -> int = f(1, 2, 3, 4)
            |
            |print(zero(() -> 0), one(a -> a), two((a, b) -> a + b))
            |print(three((a, b, c) -> a + b + c), four((a, b, c, d) -> a + b + c + d))
            |""".stripMargin) shouldBe "0 1 3\n6 10\n"
    }

    "and five is where the library stops" in {
      err("""f(g: (int, int, int, int, int) -> int) -> int = 0
            |""".stripMargin) should include("takes up to 4 parameters")
    }
  }

  "every position a callable may stand in" - {
    "an element of an array of them" in {
      run("""var fs: [3]&Fn(int) -> int = [k -> k + 1, k -> k + 2, k -> k + 3]
            |
            |for i in 0..<3 do print(fs[i](10))
            |""".stripMargin) shouldBe "11\n12\n13\n"
    }

    "the payload of an enum" in {
      run("""var maybe: Option[&Fn(int) -> int] = Some(k -> k * 5)
            |
            |maybe match
            |    Some(g) -> print(g(4))
            |    None -> print("none")
            |""".stripMargin) shouldBe "20\n"
    }

    "a part of a tuple" in {
      run("""var pair: (int, &Fn(int) -> int) = (2, k -> k * 100)
            |
            |print(pair.0, (pair.1)(3))
            |""".stripMargin) shouldBe "2 300\n"
    }

    "an item of a container" in {
      run("""import sysl.buf.*
            |
            |var bag: Buf[&Fn(int) -> int] = buf()
            |
            |bag.push(k -> k * 7)
            |print(bag[0usize](3))
            |""".stripMargin) shouldBe "21\n"
    }

    // `reference/types.md § Function types` lists the result of another call among these, which is
    // the one that never passes through a name at all: the head of the outer call is the inner call
    // itself.
    "the result of another call, called straight away" in {
      run("""pick(which: bool) -> &Fn(int) -> int
            |    if which then x -> x + 1 else x -> x * 2
            |
            |print(pick(true)(10), pick(false)(10))
            |""".stripMargin) shouldBe "11 20\n"
    }

    "the result of a callable, so one may yield another" in {
      run("""mk() -> &Fn(int) -> &Fn(int) -> int
            |    a -> b -> a + b
            |
            |var outer = mk()
            |var inner = outer(10)
            |
            |print(inner(5))
            |""".stripMargin) shouldBe "15\n"
    }

    "both branches of an 'if', which meet at the written type" in {
      run("""var pick: &Fn(int) -> int = if true then k -> k + 1 else k -> k - 1
            |
            |print(pick(10))
            |""".stripMargin) shouldBe "11\n"
    }

    "and something that is not callable, called anyway, says so" in {
      err("""var xs = [1, 2, 3]
            |
            |print(xs[0](1))
            |""".stripMargin) should include("must be a name, or something whose type says it is callable")
    }
  }

  "what a closure composes with" - {
    "a trait implementation's method may write one" in {
      run(apply + """trait Runs
                    |    go(self) -> int
                    |
                    |struct Holder
                    |    n: int
                    |
                    |impl Runs for Holder
                    |    go(self) -> int
                    |        var k = self.n
                    |
                    |        apply(x -> x + k, 1)
                    |
                    |var h = Holder(41)
                    |
                    |print(h.go())
                    |""".stripMargin) shouldBe "42\n"
    }

    "a generic function may take one over its own parameter" in {
      run("""pairup[T](a: T, mk: int -> T) -> T = mk(0)
            |
            |print(pairup(7, k -> k + 1))
            |""".stripMargin) shouldBe "1\n"
    }

    // `reference/expressions.md § Closures` asked whether a closure written *inside* a generic body
    // — capturing a value whose type is the enclosing function's parameter — interacts with
    // monomorphization in a way the top-level cases do not. It does not: the closure is a struct
    // whose field has that type, so it is monomorphized with the function that holds it, once per
    // instantiation. Two instantiations here, and the second is a float, so a struct laid out for
    // the first would be measurably wrong.
    "a closure inside a generic body captures the parameter's own type" in {
      run("""bump[T: Add](x: T, by: T) -> T
            |    var g = (y: T) -> y + by
            |
            |    g(x)
            |
            |print(bump(5, 7), bump(1.5, 0.25))
            |""".stripMargin) shouldBe "12 1.75\n"
    }

    // The bound travels with it: `+` inside the body is dispatched through what the *enclosing*
    // declaration asked of `T`, which is the only place the requirement is written.
    "and the bound the enclosing declaration carries reaches the body" in {
      run("""shown[T: Display](x: T) -> string
            |    var f: &Fn(T) -> string = (v: T) -> str(v)
            |
            |    f(x)
            |
            |print(shown(42), shown(true), shown("s"))
            |""".stripMargin) shouldBe "42 true s\n"
    }

    "a boxed one may be returned out of a generic body" in {
      run("""adder[T: Add](by: T) -> &Fn(T) -> T = (y: T) -> y + by
            |
            |var i = adder(10)
            |var f = adder(0.5)
            |
            |print(i(1), f(1.25))
            |""".stripMargin) shouldBe "11 1.75\n"
    }

    // The other half of `reference/expressions.md § Closures` — a closure that is itself generic —
    // is a **grammar** fact and nothing more. It used to rest on a second one: a callable's type is
    // the library's `FnN` trait, and a trait member could not declare type parameters at all. That
    // reason is gone — `Fn1.call` could carry one now — so what is left is that an arrow has
    // nowhere to write them, which is where the refusal lands.
    "a closure of its own may not be generic, because an arrow has nowhere to declare one" in {
      // `[T]` reads as an index of `T` and `(x` as a call on the result, so the refusal lands on
      // the `:` that neither of those admits — the grammar has no reading in which the brackets
      // before an arrow declare anything.
      err("""var f = [T](x: T) -> x
            |""".stripMargin) should include("')' expected")
    }

    // The half that was holding it up, kept as its own case so the pair still reads: a trait member
    // declaring type parameters is ordinary now, and the arrow above is refused on its own account.
    "a trait member declaring one is no longer what stands in the way" in {
      run("""trait Maps
            |    over[T](self, x: T) -> T
            |struct P
            |    n: int
            |impl Maps for P
            |    over[T](self, x: T) -> T = x
            |print(P(1).over(7), P(1).over("s"))
            |""".stripMargin) shouldBe "7 s\n"
    }

    "'?' inside a body unwraps into the closure's own result" in {
      run("""find(n: int) -> Result[int, string]
            |    if n > 0 then Ok(n) else Err("no")
            |
            |var g: &Fn(int) -> Result[int, string] = k ->
            |    var v = find(k)?
            |    Ok(v * 2)
            |
            |g(3) match
            |    Ok(v) -> print("ok", v)
            |    Err(e) -> print("err", e)
            |
            |g(-1) match
            |    Ok(v) -> print("ok", v)
            |    Err(e) -> print("err", e)
            |""".stripMargin) shouldBe "ok 6\nerr no\n"
    }

    "an interpolated string reads the captures around it" in {
      run("""var name = "world"
            |var s: &Fn(int) -> string = k -> s"hello $name $k"
            |
            |print(s(3))
            |""".stripMargin) shouldBe "hello world 3\n"
    }

    "a boxed one holding a reference releases it with the box" in {
      // Twenty thousand closures, each holding a share of a fresh object and each dropped at the end
      // of its iteration. A capture that was retained and never released grows the heap instead.
      run("""struct Node
            |    v: int
            |
            |make(n: int) -> &Fn(int) -> int
            |    var node: &Node = Node(n)
            |
            |    x -> x + node.v
            |
            |var total = 0
            |
            |for i in 0..<20000
            |    var f = make(i)
            |    total += f(0)
            |
            |print(total)
            |""".stripMargin) shouldBe "199990000\n"
    }
  }

  /** `call` is `*self` on every arity of the call trait, because a closure captures by value and one
   * that counts has to write its own copy. That mode asks a call for a *place*, so a closure bound
   * with `val` used to be refused for being written once — a name only ever read, told it was meant
   * to change.
   *
   * A body that writes none of its captures emits no store, so the address the receiver takes is
   * used for reading and the refusal protected nothing. These hold both halves: the reading closure
   * is called on a `val`, and the counting one still is not.
   */
  "a 'val' holds a closure that only reads its captures" - {

    "one that captures nothing is called" in {
      run("""val double = (x: int) -> x * 2
            |
            |print(double(3))
            |""".stripMargin) shouldBe "6\n"
    }

    "one that reads a capture is called" in {
      run("""var k = 10
            |val add = (x: int) -> x + k
            |
            |print(add(3))
            |""".stripMargin) shouldBe "13\n"
    }

    // The case that filed the card, from `library/sysl/path/tests.sysl`: a closure named to be used
    // twice, never reassigned, and `var` was the only word that would take it.
    "one named to be used twice" in {
      run("""val twice = (s: string) -> s + s
            |
            |print(twice("ab"), twice("cd"))
            |""".stripMargin) shouldBe "abab cdcd\n"
    }

    // **The half that must keep failing.** A closure captures by value and carries its own mutable
    // copy, so calling this one writes through the receiver — which is exactly what `*self` is for,
    // and what a `val` cannot supply.
    "and one that WRITES a capture is still refused, naming the binding" in {
      err("""var n = 0
            |val bump = () ->
            |    n += 1
            |    n
            |
            |print(bump())
            |""".stripMargin) should include("write 'var bump'")
    }

    // The counting closure is the reason the mode is `*self` at all (`library/sysl/iter.sysl`), so
    // it is pinned here rather than left to the refusal above: the captures are the closure's own
    // copies, and the variable they were taken from does not move.
    "a counting closure still counts, and does not touch what it captured" in {
      run("""var n = 0
            |
            |var bump = () ->
            |    n += 1
            |    n
            |
            |print(bump(), bump(), bump(), n)
            |""".stripMargin) shouldBe "1 2 3 0\n"
    }

    // A boxed callable is a `&Fn`, dispatched on the trait's own declaration where `call` stays
    // `*self` for every closure alike — so this path is untouched by the relaxation and the box is
    // what supplies the place.
    "a boxed callable in a 'val' is unaffected" in {
      run("""val g: &Fn(int) -> int = (x) -> x * 2
            |
            |print(g(3))
            |""".stripMargin) shouldBe "6\n"
    }

    // **The case no spelling reached before.** `&Fn` is a box, so the annotation above is refused
    // where there is no allocator — which left a freestanding target with no way at all to bind a
    // closure to an immutable name and call it. The bare closure needs none.
    "and the reading closure is callable where there is no allocator" in {
      ir("""@no_alloc
           |
           |val double = (x: int) -> x * 2
           |
           |print(double(3))
           |""".stripMargin) should include("@main")
    }

    // **A `*self` method called ON a capture is admitted, and this is the boundary worth pinning.**
    // `writesEnvironment` looks for a store whose place reaches `self`, and `c.bump()` has none —
    // the store is inside `bump`'s own body, and what the closure writes is the receiver it passed.
    // So this is called on a `val`, and the answer is the by-value one the `var` form gives.
    "one calling a '*self' method on a capture is called, with the by-value answer" in {
      run("""struct Counter
            |    n: int
            |
            |    bump(*self) -> int
            |        self.n += 1
            |        self.n
            |
            |var c = Counter(0)
            |
            |val tick = () -> c.bump()
            |
            |print(tick(), tick(), c.n)
            |""".stripMargin) shouldBe "1 2 0\n"
    }

    /** **The storage that would have made the case above unsound cannot be written at all**, which
     * is what licenses admitting it rather than luck.
     *
     * `ValReceiverTests` names a module-level `val` as the one place where a write through a `val`
     * receiver aims a store into storage the emitted module marks `constant`. A closure cannot get
     * there by two independent refusals, and both are checked here rather than reasoned about: a
     * module-level `val` must state its type, and a bare closure's type is a struct with no
     * spellable name; and the only spellable one, `&Fn`, is a box whose `call` is the trait's, where
     * the mode is unchanged. Then a module-level closure cannot capture module storage either.
     */
    "and a module-level 'val' cannot hold one at all, which is why none of this reaches read-only storage" in {
      val counter = "struct Counter\n    n: int\n\n    bump(*self) -> int\n        self.n += 1\n" +
        "        self.n\n\nprivate var c = Counter(0)\n\n"

      errOf(
        "m/m.sysl"  -> ("module m\n\n" + counter + "val tick = () -> c.bump()\n"),
        "main.sysl" -> "print(m.tick())\n",
      ) should include("a module-level 'val' states its type")
    }

    // One that names its type reaches module storage as any function does — by name, not by capture,
    // since module storage is not a binding of any block. What it writes is the module's `var`, which
    // is writable storage, so nothing here aims a store at a `constant`. (This used to read as a
    // refusal — "undefined name 'c'" — only because an untyped module `var` was never registered, so
    // every use of it was reported as undefined after its own diagnostic.)
    "and one that names its type reaches module storage by name, which is writable" in {
      val counter = "struct Counter\n    n: int\n\n    bump(*self) -> int\n        self.n += 1\n" +
        "        self.n\n\nprivate var c: Counter = Counter(0)\n\npeek() -> int = c.n\n\n"

      runOf(
        "m/m.sysl"  -> ("module m\n\n" + counter + "val tick: &Fn() -> int = () -> c.bump()\n"),
        "main.sysl" -> "print(m.tick(), m.tick(), m.peek())\n",
      ) shouldBe "1 2 2\n"
    }

    // Untyped, the module `var` is refused for the type it did not state, and for nothing after it.
    "while an untyped one is refused for its type alone" in {
      val counter = "struct Counter\n    n: int\n\n    bump(*self) -> int\n        self.n += 1\n" +
        "        self.n\n\nprivate var c = Counter(0)\n\n"

      val e = errOf(
        "m/m.sysl"  -> ("module m\n\n" + counter + "val tick: &Fn() -> int = () -> c.bump()\n"),
        "main.sysl" -> "print(m.tick())\n",
      )

      e should include("'c' is module storage, and module storage states its type")
      e should not include "undefined name"
    }
  }

  "how a call on one goes wrong" - {
    "too few arguments" in {
      err("""var f: &Fn(int, int) -> int = (a, b) -> a + b
            |
            |print(f(1))
            |""".stripMargin) should include("this callable takes 2 arguments, but 1 argument was given")
    }

    "too many" in {
      err("""var f: &Fn(int) -> int = x -> x + 1
            |
            |print(f(1, 2))
            |""".stripMargin) should include("this callable takes 1 argument, but 2 arguments were given")
    }

    "an argument of the wrong type names its position, not the trait behind it" in {
      val message = err("""var f: &Fn(int) -> int = x -> x + 1
                          |
                          |print(f("no"))
                          |""".stripMargin)

      message should include("the 1st argument of this callable is int, but string was given")
      message should not include "Fn1"
    }

    "the same, at an inlined one" in {
      val message = err(apply + """print(apply(x -> x + 1, "no"))
                                  |""".stripMargin)

      message should not include "Fn1"
    }

    "a closure at a parameter that wanted an ordinary value" in {
      err("""take(n: int) -> int = n
            |
            |print(take(x -> x))
            |""".stripMargin) should include("'x' has no type here")
    }

    "a body naming something that does not exist" in {
      err(apply + """print(apply(k -> k + missing, 1))
                    |""".stripMargin) should include("undefined name 'missing'")
    }

    /** A closure standing where a bare function address is asked for, which is the case a program
     * meets at every C callback: `*extern(A) -> R` is one machine word (`reference/ffi.md § A
     * function's address`) and a closure is a struct with an environment, so there is nothing to
     * convert.
     *
     * **What the reader is told it gave has to be "a closure".** The struct is filed under a serial
     * number that runs over the whole compilation with `library/` lowered first, so the message used to
     * read *"but .closure4 was given"* — a name nothing in the program is called, nothing can be
     * grepped for, and whose digit moves whenever the standard library gains a closure literal of its
     * own. That is not hypothetical: `sysl.slices` arriving with four of them broke five assertions
     * here and turned a page of the site red at the next version bump.
     */
    "a closure where a bare function address is wanted is called a closure, not a number" in {
      // The parameters are written out, so the closure has a type of its own to be complained
      // about. Left bare it is refused a step earlier, for having nothing to read its parameter
      // types from — a good message, and not this one.
      val message = err("""take(f: *extern(int) -> int) -> int = f(1)
                          |
                          |print(take((x: int) -> x + 1))
                          |""".stripMargin)

      message should include("but a closure was given")
      message should not include regex ("""closure\d""")
    }
  }

  "the far corners" - {
    "five closures deep, each capturing through the last" in {
      run(apply + """print(apply(a -> apply(b -> apply(c -> apply(d -> apply(e ->
                    |    e + a + b + c + d, d), c), b), a), 1))
                    |""".stripMargin) shouldBe "5\n"
    }

    "one as a loop condition" in {
      run("""var i = 0
            |var go: &Fn(int) -> bool = k -> k < 3
            |
            |while go(i) do i = i + 1
            |
            |print(i)
            |""".stripMargin) shouldBe "3\n"
    }

    "no parameters and no result" in {
      run("""var noop: &Fn() -> unit = () -> print("noop")
            |
            |noop()
            |""".stripMargin) shouldBe "noop\n"
    }

    "behind a raw pointer rather than a reference" in {
      run("""struct Doubler
            |    k: int
            |
            |impl Fn(int) -> int for Doubler
            |    call(*self, a: int) -> int = a * self.k
            |
            |var d = Doubler(3)
            |var raw: *Fn(int) -> int = &d
            |
            |print(raw(5))
            |""".stripMargin) shouldBe "15\n"
    }

    "twenty thousand of them capturing a string, none of them leaked" in {
      run("""var name = "abc"
            |var total = 0
            |
            |for j in 0..<20000
            |    var f: &Fn(int) -> int = k -> k + int(name.len)
            |    total += f(0)
            |
            |print(total)
            |""".stripMargin) shouldBe "60000\n"
    }

    // Module storage holds a boxed callable, and this is the shape the rule was relaxed **for**:
    // every C interface that calls back takes an address and an opaque word, so a binding offering a
    // sysl closure has to keep it somewhere that outlives the call, and module storage is the only
    // storage that does. The count it takes is never given back, which is what a static is.
    "a module-level 'val' holds one, which is what lets a callback be kept" in {
      run("""static val greeter: &Fn(int) -> string = k -> s"hello $k"
            |print(greeter(1))
            |""".stripMargin) shouldBe "hello 1\n"
    }

    "two of them are not compared" in {
      err("""var f: &Fn(int) -> int = x -> x
            |var g: &Fn(int) -> int = x -> x
            |
            |print(f == g)
            |""".stripMargin) should include("'==' is not defined for &Fn(int) -> int")
    }

    "a type is callable at one arity, since its members are one namespace" in {
      // `Fn(int) -> int` and `Fn(int, int) -> int` are two traits, and each would give the type a
      // member named `call`. Two traits naming one member is ordinarily allowed, told apart by
      // which of them a file can name (`reference/modules.md § Visibility`) — but `call` is reached
      // through the **call syntax**, which names no trait, so nothing a program could write would
      // say which arity it meant. `callableOf` answers with the first it finds, which makes the
      // second one silent rather than ambiguous, and that is what is refused here.
      err("""struct Twin
            |    k: int
            |
            |impl Fn(int) -> int for Twin
            |    call(*self, a: int) -> int = a
            |
            |impl Fn(int, int) -> int for Twin
            |    call(*self, a: int, b: int) -> int = a + b
            |""".stripMargin) should include("already has a member named 'call'")
    }

    // The same rule from the other side: a trait of the program's own may not take the name either,
    // however ordinary that trait is. Two traits sharing a name are told apart by which is in scope,
    // and the call syntax is exactly the use that has no trait in it to be told by.
    "nor may a trait of the program's own take the name a call trait holds" in {
      err("""struct Twin
            |    k: int
            |
            |trait Mine
            |    call(*self, a: int) -> int
            |
            |impl Fn(int) -> int for Twin
            |    call(*self, a: int) -> int = a
            |
            |impl Mine for Twin
            |    call(*self, a: int) -> int = a + 1
            |""".stripMargin) should include("already has a member named 'call'")
    }

    "an implementation whose 'call' disagrees is told in the arrow it was written with" in {
      val message = err("""struct Wrong
                          |    k: int
                          |
                          |impl Fn(int) -> int for Wrong
                          |    call(*self, a: string) -> int = 0
                          |""".stripMargin)

      message should include("trait 'Fn(int) -> int' declares int")
      message should not include "Fn1"
    }
  }

  "what the representation must not quietly become" - {
    "two closures of one written shape are two types" in {
      val out = ir(apply + """print(apply(x -> x + 1, 1), apply(x -> x * 2, 1))
                             |""".stripMargin)

      // **Two environments, not one.** Both closures are written `x -> …` over no captures, so both
      // are empty structs — and the claim is that they are nonetheless two *types*, one specialising
      // `apply` each, rather than one shared representation.
      //
      // This pair used to assert that `$closure0` and `$closure1` existed, which stopped being a
      // claim about this program the moment the library began lowering closures of its own: both
      // names were then present whatever the program did, and the test passed while asserting
      // nothing. Counting *this program's* environments is the thing that was always meant.
      envs(out) shouldBe List("", "")
    }

    "an inlined callable is called directly and a boxed one through its table" in {
      val direct = ir(apply + "print(apply(x -> x + 1, 5))\n")
      val boxed  = ir("""var f: &Fn(int) -> int = x -> x + 1
                        |
                        |print(f(5))
                        |""".stripMargin)

      direct should not include "@vt."
      boxed should include("@vt.")
    }

    "a capture is read once, where the closure is formed, and not at each call" in {
      // Formed once and called three times: a closure that read its capture at the call would see
      // the assignment between the calls and print 1 2 3 rather than 0 0 0.
      run("""struct Held
            |    f: &Fn(int) -> int
            |
            |var n = 0
            |var h = Held(k -> n)
            |
            |n = 1
            |var a = h.f(0)
            |
            |n = 2
            |var b = h.f(0)
            |
            |n = 3
            |print(a, b, h.f(0))
            |""".stripMargin) shouldBe "0 0 0\n"
    }
  }

  /** What a generic call can and cannot tell a closure standing at one of its parameters.
   *
   * **The rule is that only what a closure TAKES has to be settled first.** A closure's parameters
   * come from the context and its result comes from its body, so a result the call is still trying
   * to work out is the ordinary case rather than an obstacle — and it is often the only thing that
   * knows: in `collect[T, U](xs: []const T, f: T -> U)` nothing but the closure can say what `U` is.
   * Waiting for it before reading the closure waits forever, and what came out was the closure being
   * blamed for parameters the declaration had already stated.
   *
   * **Both spellings of `reference/types.md § Function types` are tested against each other
   * throughout**, because they were not being asked the same question: the bare arrow becomes a
   * bounded type parameter and the boxed `&Fn` is a trait object, and only the first was ever
   * consulted for a signature.
   */
  "a generic call tells a closure what it takes" - {
    "the arrow spelling, with the result settled by another parameter" in {
      run("""apply[T](x: T, f: T -> T) -> T = f(x)
            |
            |print(apply(3, n -> n + 1))
            |""".stripMargin) shouldBe "4\n"
    }

    "the boxed spelling, likewise — and this one could not be read at all" in {
      run("""apply[T](x: T, f: &Fn(T) -> T) -> T = f(x)
            |
            |print(apply(3, n -> n + 1))
            |""".stripMargin) shouldBe "4\n"
    }

    "a result nothing else settles comes from the body, arrow spelling" in {
      run("""convert[T, U](x: T, f: T -> U) -> U = f(x)
            |
            |print(convert(3, n -> s"<${n}>"))
            |""".stripMargin) shouldBe "<3>\n"
    }

    "and boxed" in {
      run("""convert[T, U](x: T, f: &Fn(T) -> U) -> U = f(x)
            |
            |print(convert(3, n -> s"<${n}>"))
            |""".stripMargin) shouldBe "<3>\n"
    }

    // The shape the whole thing was found from: a helper of `map`'s signature, where the element
    // type comes from the collection and the result type comes from nowhere but the closure.
    "a map-shaped helper needs neither type argument written" in {
      run("""import sysl.buf.{Buf, buf}
            |
            |collect[T, U](xs: []const T, f: T -> U) -> Buf[U]
            |    var out: Buf[U] = buf()
            |
            |    for x in xs
            |        out.push(f(x))
            |
            |    out
            |end collect
            |
            |print(collect([1, 2, 3], n -> n * 10).view()[2])
            |""".stripMargin) shouldBe "30\n"
    }

    // **The boxed spelling here is now a choice rather than the only one that works.** This comment
    // used to record a wart: that the arrow's spelling could not take written type arguments at all,
    // because the desugaring adds a type parameter for the callable and `convert[int, string]` was
    // then two arguments where three were declared, refused naming a `$F2` the reader never wrote.
    // An explicit list now fills the parameters the *author* declared and the sugar's go on being
    // read off the closure, so both spellings take one. The arrow's is the case below.
    "a written type argument is obeyed rather than re-inferred" in {
      run("""convert[T, U](x: T, f: &Fn(T) -> U) -> U = f(x)
            |
            |print(convert[int, string](9, n -> s"{${n}}"))
            |""".stripMargin) shouldBe "{9}\n"
    }

    "and the arrow's spelling takes one too, naming only what was declared" in {
      run("""convert[T, U](x: T, f: T -> U) -> U = f(x)
            |
            |print(convert[int, string](9, n -> s"{${n}}"))
            |""".stripMargin) shouldBe "{9}\n"
    }

    // Two arrows, so two synthesized parameters — and the written list is still the two that were
    // declared. The count is what a reader miscounts against, so it is the assertion that matters.
    "several arrows are still two type arguments, not four" in {
      run("""chain[A, B](x: int, f: int -> A, g: A -> B) -> B = g(f(x))
            |
            |print(chain[int, int](3, n -> n * 2, n -> n + 1))
            |""".stripMargin) shouldBe "7\n"
    }

    // **The refusal names the author's parameters and their count**, which is the whole of what this
    // card was about: it used to say four and name two of them with a `$`.
    "and a wrong count is reported against those two" in {
      err("""chain[A, B](x: int, f: int -> A, g: A -> B) -> B = g(f(x))
            |
            |print(chain[int](3, n -> n * 2, n -> n + 1))
            |""".stripMargin) should include(
        "'chain' takes 2 type arguments ('A', 'B'), and 1 was written")
    }

    // The shape that made this worth fixing rather than documenting: a parameter reachable only
    // through a **generic** trait bound is not solved from the arguments, so the explicit list is the
    // only way in — and on a declaration taking an arrow there was no way to write it.
    "a parameter only a generic bound mentions can now be written out" in {
      run("""trait Has[T]
            |    get(self) -> T
            |
            |struct Box
            |    v: int
            |
            |impl Has[int] for Box
            |    get(self) -> int = self.v
            |
            |pick[B: Has[T], T, N](b: B, f: T -> N) -> N = f(b.get())
            |
            |print(pick[Box, int, int](Box(4), n -> n + 1))
            |""".stripMargin) shouldBe "5\n"
    }

    // The same call with nothing written is still refused, which is what says the test above
    // exercises the written path rather than passing because inference reached it anyway.
    "and that call is still refused with nothing written" in {
      err("""trait Has[T]
            |    get(self) -> T
            |
            |struct Box
            |    v: int
            |
            |impl Has[int] for Box
            |    get(self) -> int = self.v
            |
            |pick[B: Has[T], T, N](b: B, f: T -> N) -> N = f(b.get())
            |
            |print(pick(Box(4), n -> n + 1))
            |""".stripMargin) should include("nothing says what this closure takes")
    }

    "a member's own list names only what the member declared" in {
      run("""struct Wrap
            |    v: int
            |
            |    mapped[U](self, f: int -> U) -> U = f(self.v)
            |
            |print(Wrap(5).mapped[int](n -> n * 3))
            |""".stripMargin) shouldBe "15\n"
    }

    // **An address is the one position where the sugar's parameter cannot be answered**, there being
    // no arguments to read it off and no way to write it. Refused by name rather than by a count
    // naming a `$`, which is what it did before.
    "but an address says why it cannot be instantiated" in {
      err("""chain[A, B](x: int, f: int -> A, g: A -> B) -> B = g(f(x))
            |
            |val p = &chain[int, int]
            |
            |print(1)
            |""".stripMargin) should include("generic in the callable's own type")
    }

    "an annotated parameter is still read" in {
      run("""convert[T, U](x: T, f: T -> U) -> U = f(x)
            |
            |print(convert(3, (n: int) -> n * 2))
            |""".stripMargin) shouldBe "6\n"
    }

    // Settling the result elsewhere was the one arrangement that always worked, and it has to go on
    // working — it is the case that proved the diagnosis.
    "a result settled by another argument is unchanged" in {
      run("""seeded[T, U](xs: []const T, seed: U, f: T -> U) -> U = f(xs[0])
            |
            |print(seeded([7, 8], "", n -> s"[${n}]"))
            |""".stripMargin) shouldBe "[7]\n"
    }

    /** The case above settles `U` from an **ordinary** parameter. This settles it from an earlier
      * *arrow's result*, which is the same answer arriving by a different route — and it did not
      * arrive: the second closure was told nothing said what it took, on a call whose previous
      * argument had just said so.
      *
      * An argument's contribution to the substitution has to be visible to the arguments after it
      * whatever spelling produced it, or the order two callables are declared in changes whether a
      * call compiles.
      */
    "an earlier arrow's result settles what a later arrow takes" in {
      run("""apply_b[N](x: int, first: int -> N, again: N -> N) -> N = again(first(x))
            |
            |print(apply_b(3, n -> n * 2, n -> n + 1))
            |""".stripMargin) shouldBe "7\n"
    }

    /** The same call with **named functions**, which is the shape that cannot be worked around: a
      * callable that has to recurse cannot be a closure literal, so it cannot be annotated either.
      *
      * Its failure wore a different and worse message — *"'again' is a function, and a function
      * becomes a value only where a callable is wanted … Nothing here asks for one"* — on a
      * parameter declared `again: N -> N`, which is the first thing that sentence lists. It also
      * advised `&again`, the boxed spelling, which is a different program with an allocation in it.
      */
    "and a named function is accepted there, not told the position wants no callable" in {
      run("""apply_b[N](x: int, first: int -> N, again: N -> N) -> N = again(first(x))
            |
            |double(n: int) -> int = n * 2
            |bump(n: int) -> int = n + 1
            |
            |print(apply_b(3, double, bump))
            |""".stripMargin) shouldBe "7\n"
    }

    /** Three deep, so the fix is carrying the substitution forward rather than special-casing the
      * argument next door.
      */
    "and it carries through a third arrow" in {
      run("""chain[A, B](x: int, f: int -> A, g: A -> B, h: B -> B) -> B = h(g(f(x)))
            |
            |print(chain(3, n -> n * 2, n -> s"<${n}>", s -> s + "!"))
            |""".stripMargin) shouldBe "<6>!\n"
    }

    /** **And the order the two are declared in is not part of the answer.** These two declarations
      * differ in nothing but which parameter was written first: one arrow's result is what the
      * other takes, either way round.
      *
      * Reading the held-back arguments left to right settled that by declaration order — the
      * settling arrow first compiled, the same pair reversed did not — which is the shape
      * `two(&x, null)` and `two(null, &x)` are documented not to have. Each round now reads the
      * arguments that *can* be read and lets what they settle reach the rest.
      */
    "and neither order of two arrows decides whether the call compiles" in {
      run("""forward[T](f: int -> T, g: T -> int) -> int = g(f(0))
            |
            |print(forward(n -> n + 1, n -> n * 2))
            |""".stripMargin) shouldBe "2\n"

      run("""backward[T](g: T -> int, f: int -> T) -> int = g(f(0))
            |
            |print(backward(n -> n * 2, n -> n + 1))
            |""".stripMargin) shouldBe "2\n"
    }

    // **The refusal has to survive**, or the fix has traded a bad message for a wrong program. A
    // parameter type nothing determines is still a parameter type nothing determines.
    "a parameter nothing settles is still refused, and against the closure" in {
      err("""one[T](f: &Fn(T) -> T) -> int = 0
            |
            |print(one(n -> n))
            |""".stripMargin) should include("'n' has no type here")
    }

    // **The two spellings now refuse it the same way, and that is a correction rather than a
    // regression.** This asserted the opposite until the arrow's types were made visible to
    // `unsettleable`: `T` was mentioned only by the synthesized bound, so the call was told `T`
    // appears in neither the parameters nor the result of a function whose parameter is written
    // `f: T -> T`.
    //
    // What settles which message is better is that only one of them names a fix that exists. The old
    // one advised `one[…](…)` — and a declaration taking a bare arrow **cannot** be given written
    // type arguments at all, for the reason recorded above at *"a written type argument is obeyed
    // rather than re-inferred"*: the sugar's `$F` is counted among them and its argument would be a
    // closure's unspellable type. The closure's message advises annotating what it takes, and
    // `one((n: int) -> n)` compiles.
    "the arrow spelling refuses it against the closure, naming a fix that works" in {
      err("""one[T](f: T -> T) -> int = 0
            |
            |print(one(n -> n))
            |""".stripMargin) should include("'n' has no type here")
    }

    // The advice above, run — so the pair cannot come apart the way advice and program silently do.
    "and that annotation is what makes it compile" in {
      run("""one[T](f: T -> T) -> int = 0
            |
            |print(one((n: int) -> n))
            |""".stripMargin) shouldBe "0\n"
    }
  }

  /** What a closure's name is worth outside the compilation that made it, which is nothing —
   * card `0229`.
   *
   * The name is a counter, so two units that each lower a fourth closure both call it
   * `$closure4.call`, and an instantiation made at one carries that name too. With external linkage
   * the linker is then free to resolve one unit's call to the other unit's body: the same signature
   * over a different environment, which is a wrong answer rather than a failure to link. The
   * standard module shipped in 0.0.70 did exactly that, and `StdArtifactTests` holds its artifact to
   * the consequence; these pin the property itself, at the one place it is decided.
   */
  "a closure's symbol does not leave the object file" - {

    /* Read off the emitted lines rather than written out as a name, because the number in a closure's
     * name is a counter over the whole compilation — which is the very thing being pinned, and is
     * not something a test should have to predict. */

    def named(out: String, form: String): List[String] =
      out.linesIterator.filter(_.startsWith(form)).filter { line =>
        val at = line.indexOf('@')

        // The symbol alone, cut before the parameter list: a function *taking* a closure names that
        // struct in its parameters, and this is a question about the name it is called by.
        at >= 0 && Closures.mentioned(line.drop(at + 1).takeWhile(c => c != '(' && c != ' '))
      }.toList

    def allInternal(out: String): Unit = {
      val defined = named(out, "define")

      // Non-vacuous: the program below lowered one, so there is something to have an opinion about.
      defined should not be empty
      defined.filterNot(l => l.startsWith("define internal") || l.startsWith("define private")) shouldBe Nil

      // And nothing is left to the linker either. A declaration at one of these names is the shape
      // card `0229` shipped: a body that resolves to whatever other object file happens to define it.
      named(out, "declare") shouldBe Nil
    }

    "the body of a closure literal is internal" in {
      allInternal(ir("""var n = 2
                       |var f: &Fn(int) -> int = k -> k * n
                       |
                       |print(f(3))
                       |""".stripMargin))
    }

    "and so is the body of a nested function, whose name is made the same way" in {
      allInternal(ir("""outer() -> int
                       |    var n = 2
                       |    inner(k: int) -> int = k * n
                       |    inner(3)
                       |end outer
                       |
                       |print(outer())
                       |""".stripMargin))
    }

    "and so is an instantiation made at a closure, which carries the closure's name in its own" in {
      // The shape the card was found in: a bare-arrow parameter, so the call fixes it at the
      // caller's closure and monomorphizes a function whose own name carries the closure's.
      val out = ir("""apply(f: int -> int, x: int) -> int = f(x)
                     |
                     |var n = 2
                     |
                     |print(apply(k -> k * n, 3))
                     |""".stripMargin)

      allInternal(out)

      // Discriminating against the closure's own body: the instantiation is a *second* symbol named
      // after the closure, and it is the one that was being advertised across a link.
      named(out, "define").filterNot(_.contains("$closure")) shouldBe Nil
      named(out, "define").count(_.contains("@apply.")) shouldBe 1
    }

    "while an ordinary instantiation beside it keeps the linkage it always had" in {
      // Discriminating against all three: without this the rule could be "every instantiation is
      // internal", which would be a different and much larger change.
      val out = ir("""twice[T: Add[T]](x: T) -> T = x + x
                     |
                     |print(twice(3))
                     |""".stripMargin)

      val defined = out.linesIterator.filter(l => l.startsWith("define") && l.contains("@twice.int(")).toList

      defined should have length 1
      defined.head should not startWith "define internal"
      defined.head should not startWith "define private"
    }
  }
}
