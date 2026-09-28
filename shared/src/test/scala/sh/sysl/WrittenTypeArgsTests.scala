package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Type arguments written at a **call head** — `f[T](x)`, `Pair[K, V](…)`, `x.m[T](…)`,
 * `va_arg[int](ap)` (`reference/generics.md § [] means type application in a type, indexing in an
 * expression`).
 *
 * The list and a subscript are one grammar, which is what kept this deferred: `f[T](x)` and
 * `handlers[i](x)` are the same nodes, so the parser cannot tell a type argument from an index. What
 * settles it is not the parser but **name resolution** — a function is not a thing that can be
 * indexed, so where the head resolves to a declaration and nothing nearer, there is no second
 * reading of the brackets to protect. `&f[T]` had already been making exactly that distinction in
 * order to refuse this by name, so what changed was the action and not the decision.
 *
 * **What earned it is a signature neither direction of inference reaches.** A `[const W: usize]`
 * kernel that reads and writes through slices names its width in no parameter and answers `unit`, so
 * the arguments say nothing about `W` and there is no receiving type to annotate: it could not be
 * called from anywhere. That is the same asymmetry that earned `&f[T]` its exception, one position
 * over, and it is a class rather than a corner — a lane-wise `add` over three slices is the plainest
 * SIMD kernel there is.
 *
 * The one head that keeps a refusal is a **selection**: `Box[int].of(…)` reads its type's arguments
 * and its own from one solve, and settling half of that list is a different question. `Maybe[int]`'s
 * *variants* are here, because a variant is a construction of the type it belongs to.
 */
class WrittenTypeArgsTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  "a free function" - {
    "takes its type argument written out" in {
      run("""id[T](x: T) -> T = x
            |print(id[int](3), id[string]("s"))
            |""".stripMargin) shouldBe "3 s\n"
    }

    // What is written **settles** the instantiation rather than being checked against what the
    // arguments would have said: a bare `3` is an `int` on its own and lands in whichever copy the
    // brackets named.
    "and the written argument is what the copy is chosen by" in {
      run("""width[T](x: T) -> usize = sizeof(T)
            |print(width[u8](3), width[i64](3), width[real](1.0))
            |""".stripMargin) shouldBe "1 8 8\n"
    }

    "a value parameter is written the same way" in {
      run("""chunk[const N: usize]() -> usize = N
            |print(chunk[4](), chunk[8]())
            |""".stripMargin) shouldBe "4 8\n"
    }

    /** **The class this was built for.** Every parameter is a slice and the result is `unit`, so
      * `W` is in neither of `reference/generics.md § Inference is bidirectional`'s directions and
      * no annotation anywhere could have supplied it. Run at two widths from one body, which is
      * what says the brackets are read rather than ignored.
      */
    "a width-generic kernel with nothing but slices is callable" in {
      val src =
        """add[const W: usize](a: []const f32, b: []const f32, out: []f32)
          |    var i: usize = 0
          |    while i + W <= a.len
          |        val l: <W>f32 = a.load(i)
          |        val r: <W>f32 = b.load(i)
          |        out.store(i, l + r)
          |        i += W
          |
          |val xs: [12]f32 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0]
          |val ys: [12]f32 = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0, 120.0]
          |var four: [12]f32
          |var eight: [12]f32
          |
          |add[4](xs[..], ys[..], four[..])
          |add[8](xs[..], ys[..], eight[..])
          |print(four[0], four[11], eight[7], eight[11])
          |""".stripMargin

      // Twelve is three runs at four lanes and one at eight, so the tail the wider instantiation
      // leaves is what says the two bodies are different code rather than one guessed width.
      run(src) shouldBe "11 132 88 0\n"
    }

    // A function whose parameter is used only *inside* the body, which is the shape
    // `reference/generics.md § Writing the type arguments` named as well-formed and uncallable.
    "so is one whose parameter appears only in its body" in {
      run("""trait Width
            |    bits() -> usize
            |
            |impl Width for u32
            |    bits() -> usize = 32
            |
            |impl Width for u8
            |    bits() -> usize = 8
            |
            |describe[T: Width]() -> usize = T.bits()
            |print(describe[u32](), describe[u8]())
            |""".stripMargin) shouldBe "32 8\n"
    }

    // Written out is not written out of the bounds: the body was compiled against them, and an
    // unsatisfied one would otherwise surface as a missing method inside a copy nobody wrote.
    "a bound is checked on what was written" in {
      err("""struct Plain
            |    n: int
            |
            |show[T: Display](x: T) -> string = str(x)
            |var p = Plain(1)
            |print(show[Plain](p))
            |""".stripMargin) should include("Display")
    }

    "the count has to match" in {
      err("""id[T](x: T) -> T = x
            |print(id[int, string](3))
            |""".stripMargin) should include("takes 1 type argument")
    }

    // The test is that the name is a *function*, not that it is a generic one — a function cannot be
    // indexed, so there is no second reading, and this is owed a sentence about type arguments
    // rather than a general complaint about callables.
    "a function that is not generic says so" in {
      err("""plain(x: int) -> int = x
            |print(plain[int](3))
            |""".stripMargin) should include("'plain' is not generic")
    }

    // The shadowing test every call form makes: what is indexed here is the local, and its author
    // never wrote a type argument to be told about.
    "a local shadowing the name keeps the subscript" in {
      run("""id[T](x: T) -> T = x
            |var id: []*extern(int) -> int = [&double]
            |double(n: int) -> int = n * 2
            |print(id[0](3))
            |""".stripMargin) shouldBe "6\n"
    }

    /** The honest hole, and it is the same one `&f[T]` has: the brackets read the **expression**
      * grammar, so a type with no expression spelling cannot be written in them. A slice fails in
      * the parser rather than reaching the message, which is worth pinning so that a change to
      * either is noticed.
      */
    "a type the expression grammar cannot spell is refused" in {
      err("""count[T](xs: []T) -> usize = xs.len
            |var ns: [3]int = [1, 2, 3]
            |print(count[[]int](ns[..]))
            |""".stripMargin) should include("expected")
    }

    "a vector is one of them, and the annotation still reaches it" in {
      err("""first[T](v: T) -> T = v
            |print(first[<4>f32](1.0))
            |""".stripMargin) should include("expected")
    }
  }

  "a special form" - {
    /** `va_arg[int](ap)` is what `reference/ffi.md § Variadic functions` called the strongest case
      * for this syntax: everywhere else the annotation standing in is a word on a binding that was
      * going to be written anyway, and a variadic body reading its tail straight into `print` has
      * no binding at all.
      */
    "va_arg reads the type written on it" in {
      val src =
        """sum(n: int, ...) -> int
          |    var ap: va_list
          |    var total = 0
          |    va_start(ap)
          |    for i in 0..<n
          |        total += va_arg[int](&ap)
          |    va_end(ap)
          |    total
          |end sum
          |print(sum(3, 10, 20, 30))""".stripMargin

      run(src) shouldBe "60\n"
    }

    "and a form that has no type to be told is told that" in {
      err("print[int](1)") should include("'print' takes no type arguments")
    }
  }

  "a constructor" - {
    "takes the arguments written on the type's name" in {
      run("""struct Pair[K, V]
            |    key: K
            |    value: V
            |
            |var p = Pair[int, real](1, 2.5)
            |print(p.key, p.value)
            |""".stripMargin) shouldBe "1 2.5\n"
    }

    // It means what the annotation means, so the fields are checked against the instantiation the
    // brackets fixed rather than one solved from the arguments.
    "and the fields are checked against it" in {
      err("""struct Box[T]
            |    v: T
            |
            |var b = Box[real](1)
            |print(b.v)
            |""".stripMargin) should include("real")
    }

    // A bare enum name is not a constructor at all, so what it is owed is the sentence about
    // variants rather than a type it cannot build.
    "an enum name applied to arguments is not one" in {
      err("""enum Maybe[T]
            |    Just(v: T)
            |    Nothing
            |
            |var m = Maybe[int](1)
            |print(1)
            |""".stripMargin) should include("is an enum")
    }
  }

  "a variant selected from the type" - {
    "carries the instantiation written on the name" in {
      run("""enum Maybe[T]
            |    Just(v: T)
            |    Nothing
            |
            |val m = Maybe[int].Just(3)
            |val n = m match
            |    Just(v) -> v
            |    Nothing -> 0
            |print(n)
            |""".stripMargin) shouldBe "3\n"
    }

    "including one that carries nothing" in {
      run("""enum Maybe[T]
            |    Just(v: T)
            |    Nothing
            |
            |val m = Maybe[string].Nothing
            |val n = m match
            |    Just(v) -> v
            |    Nothing -> "none"
            |print(n)
            |""".stripMargin) shouldBe "none\n"
    }

    /** An associated function keeps the refusal, and the reason is not squeamishness: its type's
      * parameters and its own are solved from one list, so honouring the brackets would mean fixing
      * half of that list and solving the rest. The annotation reaches it and is always there, since
      * an associated function has a result for the arguments to be read off.
      */
    "an associated function is still asked for on the binding" in {
      err("""struct Box[T]
            |    v: T
            |    of(x: T) -> Self = Box(x)
            |
            |var b = Box[int].of(1)
            |print(b.v)
            |""".stripMargin) should include("select 'of' from the plain name")
    }
  }

  // Every head above, written through the module that declares it. A list is written "on a function,
  // qualified or not", on a constructor and on a variant (`reference/generics.md § Writing the type
  // arguments`), and a qualified name reaches whatever the unqualified one does
  // (`reference/modules.md § Imports`). The struct and the variant fell past every form that reads a
  // list as type arguments, to the complaint about a callee that is not a name.
  "written on a qualified name" - {
    val lib = "b.sysl" -> """|module b
                             |
                             |struct G[T]
                             |    x: T
                             |    y: string
                             |
                             |struct Pair[K, V]
                             |    key: K
                             |    value: V
                             |
                             |enum Maybe[T]
                             |    Just(v: T)
                             |    Nothing
                             |
                             |struct Box[T]
                             |    v: T
                             |    of(x: T) -> Self = Box(x)
                             |
                             |f[T](x: T) -> usize = sizeof(T)
                             |""".stripMargin

    "a constructor takes one argument" in {
      runOf(lib, "main.sysl" -> """|import b
                                   |
                                   |val g = b.G[int](1, "s")
                                   |print(g.x, g.y)
                                   |""".stripMargin) shouldBe "1 s\n"
    }

    "a constructor takes a list" in {
      runOf(lib, "main.sysl" -> """|import b
                                   |
                                   |val p = b.Pair[u8, real](7, 2.5)
                                   |print(p.key, p.value, sizeof(b.Pair[u8, real]))
                                   |""".stripMargin) shouldBe "7 2.5 16\n"
    }

    "a free function, which already did" in {
      runOf(lib, "main.sysl" -> """|import b
                                   |
                                   |print(b.f[u8](3), b.f[i64](3))
                                   |""".stripMargin) shouldBe "1 8\n"
    }

    "a variant carrying a payload" in {
      runOf(lib, "main.sysl" -> """|import b
                                   |
                                   |val m = b.Maybe[int].Just(3)
                                   |val n = m match
                                   |    b.Maybe.Just(v) -> v
                                   |    b.Maybe.Nothing -> 0
                                   |print(n)
                                   |""".stripMargin) shouldBe "3\n"
    }

    "a variant carrying nothing" in {
      runOf(lib, "main.sysl" -> """|import b
                                   |
                                   |val m = b.Maybe[string].Nothing
                                   |val n = m match
                                   |    b.Maybe.Just(v) -> v
                                   |    b.Maybe.Nothing -> "none"
                                   |print(n)
                                   |""".stripMargin) shouldBe "none\n"
    }

    "the fields are checked against it" in {
      errOf(lib, "main.sysl" -> "import b\n\nval g = b.G[real](1, \"s\")\nprint(g.x)\n") should
        include("'x' of 'b.G[real]' is real, but int was given")
    }

    // The refusals quote what the reader wrote, never the key with its module separator.
    "an enum name applied to arguments is refused by its written name" in {
      errOf(lib, "main.sysl" -> "import b\n\nval m = b.Maybe[int](1)\nprint(1)\n") should
        include("'b.Maybe' is an enum")
    }

    "an associated function is still asked for on the binding" in {
      errOf(lib, "main.sysl" -> "import b\n\nval x = b.Box[int].of(1)\nprint(x.v)\n") should
        include("'b.Box' cannot be given type arguments where 'of' is selected from it")
    }
  }

  "a method" - {
    "takes its own type argument written out" in {
      run("""struct Counter
            |    n: int
            |    pick[T](self, x: T) -> T = x
            |
            |var c = Counter(1)
            |print(c.pick[int](3), c.pick[string]("s"))
            |""".stripMargin) shouldBe "3 s\n"
    }

    // Only the member's own parameters are written. The receiver's arrived with the value and were
    // never a question, so the list is the member's length rather than the pair's.
    "on a generic receiver, only the member's own are written" in {
      run("""struct Box[T]
            |    v: T
            |    tag[U](self, u: U) -> usize = sizeof(T) + sizeof(U)
            |
            |var b = Box(1u8)
            |print(b.tag[i64](0), b.tag[u8](0))
            |""".stripMargin) shouldBe "9 2\n"
    }

    /** **The one head where the second reading is live**, and the guard is written for it: a field
      * may hold a table of callables, so `x.handlers[i](…)` is an ordinary thing to write and must
      * keep meaning what it always meant. Asking the receiver settles it, where asking whether any
      * type in the program declares a generic member of that name — which is what the refusal this
      * replaces did — would have answered yes for a field some unrelated type shared a name with.
      */
    "a field holding callables keeps its subscript" in {
      run("""struct Table
            |    fs: [2]*extern(int) -> int
            |
            |double(n: int) -> int = n * 2
            |triple(n: int) -> int = n * 3
            |
            |struct Other
            |    n: int
            |    fs[T](self, x: T) -> T = x
            |
            |var t = Table([&double, &triple])
            |print(t.fs[0](5), t.fs[1](5))
            |""".stripMargin) shouldBe "10 15\n"
    }

    "a method that is not generic says so" in {
      err("""struct Counter
            |    n: int
            |    plain(self, x: int) -> int = x
            |
            |var c = Counter(1)
            |print(c.plain[int](3))
            |""".stripMargin) should include("'plain' is not generic")
    }
  }
}
