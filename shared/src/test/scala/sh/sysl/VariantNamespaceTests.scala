package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `reference/types.md § Enums` — **a variant belongs to its enum rather than to the module it was
 * declared in.**
 *
 * Two enums may each name a variant `Circle`, and what a bare `Circle(3)` means is settled by the
 * type expected where it is written. Where nothing expects a type, the qualified `Shape.Circle`
 * spelling says which — the same dot-qualified form a pattern has always been able to carry.
 *
 * Until this landed, a variant name was unique across the whole module: the second enum to name a
 * `Circle` was refused at its declaration, and a module accumulating enums ran out of the ordinary
 * words. `Failed`, `Done`, `Empty` and `Invalid` were each usable once.
 */
class VariantNamespaceTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** Two enums with a variant name in common, and a second name apiece so that neither is reachable
   * only through the shared one.
   */
  private val two =
    """enum Shape
      |    Circle(r: int)
      |    Square(side: int)
      |enum Hole
      |    Circle(r: int)
      |    Slot(len: int)
      |""".stripMargin

  /** A struct and an enum variant of one name, which is what a call in a module declaring both has
   * to choose between (card `0220`).
   */
  private val both =
    """struct Segment
      |    a: int
      |    b: int
      |enum Kind
      |    Circle
      |    Segment
      |""".stripMargin

  "two enums in one module may share a variant name" - {
    "which was refused outright before" in {
      ir(two + "val s: Shape = Circle(1)\nprint(1)") should include("%enum.Shape")
    }

    "and each keeps its own storage" in {
      val out = ir(two + "val s: Shape = Circle(1)\nval h: Hole = Circle(2)\nprint(1)")

      out should include("%enum.Shape = type")
      out should include("%enum.Hole = type")
    }
  }

  "the expected type says which enum a bare name means" - {
    "at an annotated binding" in {
      run(two +
        """area(s: Shape) -> int
          |    s match
          |        Circle(r) -> r * 3
          |        Square(side) -> side * side
          |depth(h: Hole) -> int
          |    h match
          |        Circle(r) -> r + 100
          |        Slot(len) -> len + 200
          |val s: Shape = Circle(2)
          |val h: Hole = Circle(2)
          |print(area(s))
          |print(depth(h))
          |""".stripMargin) shouldBe "6\n102\n"
    }

    "at an argument, which supplies one without anything being annotated" in {
      run(two +
        """depth(h: Hole) -> int
          |    h match
          |        Circle(r) -> r + 100
          |        Slot(len) -> len + 200
          |print(depth(Circle(5)))
          |""".stripMargin) shouldBe "105\n"
    }

    "at a return, for a nullary variant" in {
      run("""enum Link
            |    Down
            |    Up
            |enum Door
            |    Down
            |    Open
            |state() -> Link = Down
            |name(l: Link) -> string
            |    l match
            |        Down -> "down"
            |        Up -> "up"
            |print(name(state()))
            |""".stripMargin) shouldBe "down\n"
    }
  }

  "a qualified name says which, where nothing else does" - {
    "carrying data" in {
      run(two +
        """area(s: Shape) -> int
          |    s match
          |        Circle(r) -> r * 3
          |        Square(side) -> side * side
          |var s = Shape.Circle(4)
          |print(area(s))
          |""".stripMargin) shouldBe "12\n"
    }

    "and nullary" in {
      run("""enum Link
            |    Down
            |    Up
            |enum Door
            |    Down
            |    Open
            |name(l: Link) -> string
            |    l match
            |        Down -> "down"
            |        Up -> "up"
            |var l = Link.Down
            |print(name(l))
            |""".stripMargin) shouldBe "down\n"
    }

    "picking the other one, from the same line written twice" in {
      run(two +
        """area(s: Shape) -> int
          |    s match
          |        Circle(r) -> r * 3
          |        Square(side) -> side * side
          |depth(h: Hole) -> int
          |    h match
          |        Circle(r) -> r + 100
          |        Slot(len) -> len + 200
          |print(area(Shape.Circle(2)))
          |print(depth(Hole.Circle(2)))
          |""".stripMargin) shouldBe "6\n102\n"
    }
  }

  "a bare name with two answers and nothing to choose by is refused" - {
    "naming both enums" in {
      val out = err(two + "var s = Circle(1)\nprint(1)")

      out should include("'Circle' is a variant of 'Shape' and 'Hole'")
    }

    "and showing the qualified form that fixes it" in {
      err(two + "var s = Circle(1)\nprint(1)") should include("qualify it, as 'Shape.Circle'")
    }

    "for a nullary variant too" in {
      err("""enum Link
            |    Down
            |    Up
            |enum Door
            |    Down
            |    Open
            |var l = Down
            |print(1)
            |""".stripMargin) should include("'Down' is a variant of 'Link' and 'Door'")
    }

    "listing all three where three enums answer" in {
      val out = err("""enum A
                      |    X
                      |    P
                      |enum B
                      |    X
                      |    Q
                      |enum C
                      |    X
                      |    R
                      |var v = X
                      |print(1)
                      |""".stripMargin)

      out should include("'X' is a variant of 'A', 'B' and 'C'")
    }
  }

  "one enum answering is not an ambiguity" in {
    err("enum Shape\n    Circle(r: int)\n    Square(s: int)\nvar s = Circle(1, 2)\nprint(1)") should
      include("has 1 field, but 2 values were given")
  }

  "a pattern is unaffected, because the scrutinee already says which enum" - {
    "bare, on either of them" in {
      run(two +
        """area(s: Shape) -> int
          |    s match
          |        Circle(r) -> r * 3
          |        Square(side) -> side * side
          |depth(h: Hole) -> int
          |    h match
          |        Circle(r) -> r + 100
          |        Slot(len) -> len + 200
          |print(area(Shape.Circle(1)) + depth(Hole.Circle(1)))
          |""".stripMargin) shouldBe "104\n"
    }

    "qualified, which the dotted form has always allowed" in {
      run(two +
        """area(s: Shape) -> int
          |    s match
          |        Shape.Circle(r) -> r * 3
          |        Shape.Square(side) -> side * side
          |print(area(Shape.Circle(3)))
          |""".stripMargin) shouldBe "9\n"
    }
  }

  // The asymmetry is the rule in one line: two variants of a name are told apart by the enum they
  // belong to, and a variant and a constant have nothing to be told apart *by*.
  "a variant still clashes with anything that is not a variant" - {
    "a constant declared after it" in {
      err("enum Colour\n    Red\nend Colour\nconst Red: int = 1") should
        include("'Red' is already used by enum 'Colour'")
    }

    "and one declared before it, which the enum reports" in {
      err("const Red: int = 1\nenum Colour\n    Red\nend Colour") should
        include("variant name 'Red' is already used by a constant")
    }

    "while a second enum in the same position is accepted" in {
      ir("enum Colour\n    Red\nend Colour\nenum Wine\n    Red\nend Wine\nprint(1)") should
        include("define i32 @main")
    }
  }

  "visibility follows the widest enum that offers the name" - {
    "so a public enum's variant survives a private one naming it too" in {
      runIn(
        ("shapes", "shapes.sysl",
          """module shapes
            |private enum Hidden
            |    Circle(r: int)
            |    Slot(len: int)
            |enum Shape
            |    Circle(r: int)
            |    Square(side: int)
            |area(s: Shape) -> int
            |    s match
            |        Circle(r) -> r * 3
            |        Square(side) -> side * side
            |""".stripMargin),
        ("", "main.sysl",
          """import shapes.*
            |print(area(Circle(2)))
            |""".stripMargin)) shouldBe "6\n"
    }

    "and the private one is not a candidate from outside, so the name is not ambiguous there" in {
      runIn(
        ("shapes", "shapes.sysl",
          """module shapes
            |private enum Hidden
            |    Circle(r: int)
            |    Slot(len: int)
            |enum Shape
            |    Circle(r: int)
            |    Square(side: int)
            |""".stripMargin),
        ("", "main.sysl",
          """import shapes.*
            |var s = Circle(7)
            |
            |s match
            |    Circle(r) -> print(r)
            |    Square(side) -> print(side)
            |""".stripMargin)) shouldBe "7\n"
    }
  }

  "generic enums are no different, since the expected type carries its arguments" - {
    "two of them sharing a variant name" in {
      run("""enum Box[T]
            |    Wrap(v: T)
            |    Bare
            |enum Sack[T]
            |    Wrap(v: T)
            |    Loose
            |unbox(b: Box[int]) -> int
            |    b match
            |        Wrap(v) -> v
            |        Bare -> 0
            |val b: Box[int] = Wrap(9)
            |print(unbox(b))
            |""".stripMargin) shouldBe "9\n"
    }

    "and qualified, where nothing expects one" in {
      run("""enum Box[T]
            |    Wrap(v: T)
            |    Bare
            |enum Sack[T]
            |    Wrap(v: T)
            |    Loose
            |unbox(b: Box[int]) -> int
            |    b match
            |        Wrap(v) -> v
            |        Bare -> 0
            |var b = Box.Wrap(4)
            |print(unbox(b))
            |""".stripMargin) shouldBe "4\n"
    }
  }

  "a shared name whose two variants differ in shape is still told apart by the expected type" in {
    run("""enum Reply
          |    Done(code: int)
          |    Waiting
          |enum Job
          |    Done
          |    Running(pid: int)
          |say(r: Reply) -> int
          |    r match
          |        Done(code) -> code
          |        Waiting -> -1
          |ran(j: Job) -> int
          |    j match
          |        Done -> 100
          |        Running(pid) -> pid
          |val r: Reply = Done(7)
          |val j: Job = Done
          |print(say(r))
          |print(ran(j))
          |""".stripMargin) shouldBe "7\n100\n"
  }

  // `reference/generics.md § A parameter may stand for a value` — a simple enum's variant stands
  // for its tag where a value parameter's declared type is that enum. The tag is read off the
  // *instantiated* enum, so it has to reach the right one of two.
  "a simple enum's variant as a value argument picks the right enum" in {
    run("""enum Level
          |    Low
          |    High
          |enum Tide
          |    Low
          |    Slack
          |struct Gauge[const L: Level]
          |    reading: int
          |at(g: Gauge[High]) -> int = g.reading
          |val g: Gauge[High] = Gauge(3)
          |print(at(g))
          |""".stripMargin) shouldBe "3\n"
  }

  // A program's own declaration outranks the library's *where nothing else decides*, which is what
  // makes this legal — and it was legal before, since `Option` is another module. Kept because a
  // reader meeting the new rule will ask whether it changed.
  //
  // **It is the expected type that outranks both**, which the block below this one is about: the
  // annotation here says `Maybe`, so the program's own is what it names, and the same program with
  // `val m: Option[int]` on that line would name the library's.
  "a program may name a variant the library also names" in {
    run("""enum Maybe
          |    Some(v: int)
          |    Nothing
          |unwrap(m: Maybe) -> int
          |    m match
          |        Some(v) -> v
          |        Nothing -> 0
          |val m: Maybe = Some(5)
          |print(unwrap(m))
          |""".stripMargin) shouldBe "5\n"
  }

  /** Card `0370`. The rule above crosses a **module** boundary, which it did not until this landed:
   * a key says which module, and `resolveName` answers with the file's own before it looks at the
   * library — so a module declaring an `Ok` of its own made every `Result` in it unwritable.
   *
   * The refusal named the wrong `Ok` and read as a defect in the call: *"variant 'Ok' carries
   * nothing, so it is written as a name on its own"*, over a line whose expected type has an `Ok`
   * that carries an `int`, with nothing in it mentioning the enum thirty lines above.
   *
   * Found writing `sysl-lang/libpq`, where `PQstatus` answers `CONNECTION_OK`/`CONNECTION_BAD` and
   * the enum over it wants the name every other language gives it. The workaround was to rename the
   * variant, and the cost was real: the collision is invisible until a `Result` is returned
   * somewhere else in the same file.
   */
  "the expected type reaches across modules, not only within one" - {

    // The card's own reduction, at all four of the prelude's names — `Result` and `Option` are what
    // every package returns, so these are the four words a module cannot afford to lose.
    val shadows =
      """enum Status
        |    Ok
        |    Bad
        |enum Outcome
        |    Err
        |    Some
        |    None
        |""".stripMargin

    "so a module declaring its own Ok may still return a Result" in {
      run(shadows +
        """f() -> Result[int, string] = Ok(1)
          |g() -> Result[int, string] = Err("no")
          |print(f().unwrap(), g().is_err())
          |""".stripMargin) shouldBe "1 true\n"
    }

    "and Some and None still mean Option's" in {
      run(shadows +
        """h() -> Option[int] = Some(2)
          |k() -> Option[int] = None
          |print(h().unwrap(), k().is_none())
          |""".stripMargin) shouldBe "2 true\n"
    }

    // The other direction, which is the half that would break if the expected type were ignored the
    // opposite way. A nullary local variant is the shape that has no arguments to be read from.
    "while the module's own still wins where its own type is expected" in {
      run(shadows +
        """s() -> Status = Ok
          |o() -> Outcome = None
          |print(s() == Ok, o() == None)
          |""".stripMargin) shouldBe "true true\n"
    }

    // The escape for a site whose expected type is the library's and whose meaning is not.
    "and the qualified spelling says which, as it does everywhere else" in {
      run(shadows +
        """want(x: Status) -> bool = x == Status.Ok
          |r() -> Result[int, string] = Ok(1)
          |print(want(Status.Ok), r().unwrap())
          |""".stripMargin) shouldBe "true 1\n"
    }

    // Nothing here weakens the ambiguity rule: two owners and no expected type is still refused, and
    // still names both enums.
    "and two owners with nothing to choose by are still refused" in {
      err("""enum A
            |    Thing
            |enum B
            |    Thing
            |f() = print(Thing)
            |""".stripMargin) should include("'Thing' is a variant of 'A' and 'B'")
    }

    // **The regression 0.0.91 shipped, and the reason the rule is worded as re-pointing.** Asking
    // the expected type *instead of* asking whether the name resolves at all made a bare variant
    // reachable with no import — and then let it beat a name that already meant something else.
    "and a name that resolves to something else is NOT re-pointed at a variant" in {
      // A module's own function against a distant enum's variant of the same name. On 0.0.91 the
      // call constructed the variant and the function was never reached: `DOT` became `VARIANT 1`,
      // silently. That is card `0220`'s finding across modules rather than within one.
      runIn(
        ("pal", "pal.sysl",
          """module pal
            |enum Shape
            |    Segment(n: int)
            |    Dot
            |describe(s: Shape) -> string = s match
            |    Segment(n) -> s"VARIANT ${n}"
            |    Dot -> "DOT"
            |""".stripMargin),
        ("app", "app.sysl",
          """module app
            |import pal.{Shape, describe}
            |Segment(n: int) -> Shape = Shape.Dot
            |go() -> string = describe(Segment(1))
            |""".stripMargin),
        ("", "main.sysl", "print(app.go())"),
      ) shouldBe "DOT\n"
    }

    // The other half of the same mistake: a variant the file never imported is not in scope, and
    // the expected type naming its enum does not put it there. `sysl.sh`'s `library/sync.md`
    // asserts exactly this refusal and is what caught it, one release too late.
    "and an unimported variant is still out of scope, whatever the expected type names" in {
      errIn(
        ("pal", "pal.sysl",
          """module pal
            |enum Mode
            |    Fast
            |    Slow
            |pick(m: Mode) -> int = m match
            |    Fast -> 1
            |    Slow -> 0
            |""".stripMargin),
        ("", "main.sysl", "print(pal.pick(Fast))"),
      ) should include("undefined name 'Fast'")
    }

    // Patterns were never affected — the scrutinee's type says which enum before a name is looked
    // at — and this says so with **nothing constructed**, which the first draft of it did not: a
    // program that builds its own scrutinee fails on the construction line and reports a green
    // pattern rule it never reached. The `Result` comes out of the library instead, so the only
    // shadowed names in the program are the two in the arms.
    "and a pattern was never affected, because the scrutinee already says which" in {
      run("import sysl.text.parse_int\n" + shadows +
        """f(s: string) -> int = parse_int(s) match
          |    Ok(v) -> v
          |    Err(_) -> -1
          |print(f("42"), f("x"))
          |""".stripMargin) shouldBe "42 -1\n"
    }
  }

  "a variant clashes with a module 'var' as it does with a constant" in {
    errIn(
      ("shapes", "shapes.sysl",
        """module shapes
          |var Red: int = 1
          |enum Colour
          |    Red
          |end Colour
          |""".stripMargin),
      ("", "main.sysl", "print(1)\n")) should include("variant name 'Red' is already used by a module 'var'")
  }

  // Card `0220`. A variant is a **value** name and a struct is a **type** name, so one module may
  // declare both — and only a *call* has to choose between them. It used to choose the variant
  // outright, which left the struct impossible to construct by any spelling at all: found writing
  // `box2d`, whose `ShapeKind` names five of the shapes the package also declares as structs.
  //
  // The struct wins where the expected type does not name the variant's enum, and the asymmetry is
  // the argument rather than a preference: a variant keeps `Enum.Variant`, and a struct constructor
  // has no second spelling.
  "a struct and a variant of one name are told apart in call position" - {
    "the struct is constructed by its own name, which was refused outright" in {
      run(both + "seg(x: int, y: int) -> Segment = Segment(x, y)\nprint(seg(3, 4).b)\n") shouldBe "4\n"
    }

    "with nothing expected at all, since the variant still has a qualified spelling" in {
      run(both + "var s = Segment(1, 2)\nprint(s.a)\n") shouldBe "1\n"
    }

    "while the variant wins wherever the expected type names its enum" in {
      run(both +
        """name(k: Kind) -> string
          |    k match
          |        Circle -> "circle"
          |        Segment -> "segment"
          |val k: Kind = Segment
          |print(name(k))
          |""".stripMargin) shouldBe "segment\n"
    }

    "and at an argument, which supplies the expected type without an annotation" in {
      run(both +
        """name(k: Kind) -> string
          |    k match
          |        Circle -> "circle"
          |        Segment -> "segment"
          |print(name(Segment))
          |""".stripMargin) shouldBe "segment\n"
    }

    "the qualified spelling reaches the variant from a call position too" in {
      run(both +
        """name(k: Kind) -> string
          |    k match
          |        Circle -> "circle"
          |        Segment -> "segment"
          |var k = Kind.Segment
          |print(name(k))
          |""".stripMargin) shouldBe "segment\n"
    }

    "and a struct with no same-named variant is unaffected" in {
      run("struct Point\n    x: int\n    y: int\nvar p = Point(1, 2)\nprint(p.y)\n") shouldBe "2\n"
    }

    // The shape `box2d` actually has: the colliding name is a **transparent alias** for a struct
    // declared elsewhere, not a struct declared here. The question is asked through `followAlias`
    // exactly as `typeKey` asks it, so the two cannot disagree about which arm claims the call.
    "and the struct may be reached through an alias, which is the case this came from" in {
      run("""struct RawSeg
            |    a: int
            |    b: int
            |type Segment = RawSeg
            |enum Kind
            |    Circle
            |    Segment
            |seg(x: int, y: int) -> Segment = Segment(x, y)
            |print(seg(3, 4).b)
            |""".stripMargin) shouldBe "4\n"
    }

    // The question "is there a struct of this name?" is the compiler's own, so it is asked
    // quietly: `typeKey` would report the restriction instead of answering, and a variant call
    // would be refused for naming a struct the program cannot see and did not write.
    "a struct of the same name that is out of reach is not a candidate, and does not refuse" in {
      runIn(
        ("shapes", "shapes.sysl",
          """module shapes
            |private struct Segment
            |    a: int
            |    b: int
            |enum Kind
            |    Circle
            |    Segment
            |name(k: Kind) -> string
            |    k match
            |        Circle -> "circle"
            |        Segment -> "segment"
            |""".stripMargin),
        ("", "main.sysl",
          """import shapes.*
            |print(name(Segment))
            |""".stripMargin)) shouldBe "segment\n"
    }

    // …and the quiet ask still searches the imports, so one that *is* in reach still wins.
    "while one that is in reach wins, from another module" in {
      runIn(
        ("shapes", "shapes.sysl",
          """module shapes
            |struct Segment
            |    a: int
            |    b: int
            |enum Kind
            |    Circle
            |    Segment
            |""".stripMargin),
        ("", "main.sysl",
          """import shapes.*
            |var s = Segment(3, 4)
            |print(s.b)
            |""".stripMargin)) shouldBe "4\n"
    }

    // `reference/modules.md § Visibility`: "A name a file may not reach is not a candidate for it."
    // A sibling file's `private struct` is out of reach for every other file of the module, so it
    // is passed over and the variant answers — it used to be reported as private instead.
    "a sibling file's private struct is not a candidate either" - {
      val sibling =
        ("shapes", "other.sysl",
          """module shapes
            |private struct Circle
            |    x: int
            |private dummy(c: Circle) -> int = c.x
            |""".stripMargin)

      def shapes(tail: String) =
        ("shapes", "shape.sysl",
          """module shapes
            |enum Shape
            |    Circle(r: int)
            |    Square(s: int)
            |area(s: Shape) -> int = s match
            |    Circle(r) -> r * r
            |    Square(x) -> x * x
            |""".stripMargin + tail)

      "where the expected type names the enum" in {
        runIn(sibling, shapes("unit_circle() -> Shape = Circle(1)\n"),
          ("", "main.sysl", "import shapes.*\nprint(area(unit_circle()))\n")) shouldBe "1\n"
      }

      "and where nothing is expected at all" in {
        runIn(sibling, shapes("two() -> int\n    val c = Circle(2)\n    area(c)\n"),
          ("", "main.sysl", "import shapes.*\nprint(two())\n")) shouldBe "4\n"
      }

      "and from an importing module, where the struct was never a candidate" in {
        runIn(sibling, shapes(""),
          ("", "main.sysl", "import shapes.*\nval s: Shape = Circle(3)\nprint(area(s), area(Circle(1)))\n")) shouldBe
          "9 1\n"
      }

      // Inside the file that declares the struct both are in reach, and the rule the rest of this
      // group pins still holds: the struct with nothing expected, the variant where the expected
      // type names its enum.
      "while in the declaring file both are candidates, and the ordinary rule chooses" in {
        runIn(
          ("shapes", "other.sysl",
            """module shapes
              |private struct Circle
              |    x: int
              |raw() -> int = Circle(7).x
              |wrapped() -> int = area(Circle(5))
              |""".stripMargin),
          shapes(""),
          ("", "main.sysl", "import shapes.*\nprint(raw(), wrapped())\n")) shouldBe "7 25\n"
      }
    }
  }

  // The advice used to be "write it as 'Segment'", printed under a line already reading
  // `Segment(x, y)` — the spelling it had just refused. Card `0220`'s second half.
  "a nullary variant given arguments is told to drop the parentheses" - {
    "at a construction" in {
      val message = err("""enum Kind
                          |    Circle
                          |    Flat
                          |val k: Kind = Circle(1)
                          |print(1)
                          |""".stripMargin)

      message should include("'Circle' carries nothing, so it is written as a name on its own")
      message should include("drop the parentheses and the 1 argument inside them")
    }

    "and at a pattern" in {
      val message = err("""enum Kind
                          |    Circle
                          |    Flat
                          |val k: Kind = Circle
                          |
                          |k match
                          |    Circle(r) -> print(r)
                          |    Flat -> print(0)
                          |""".stripMargin)

      message should include("'Circle' carries nothing, so it is matched as a name on its own")
      message should include("drop the parentheses and the 1 sub-pattern inside them")
    }
  }

  "the rule crosses a module boundary intact" in {
    runIn(
      ("shapes", "shapes.sysl",
        """module shapes
          |enum Shape
          |    Circle(r: int)
          |    Square(side: int)
          |enum Hole
          |    Circle(r: int)
          |    Slot(len: int)
          |""".stripMargin),
      ("", "main.sysl",
        """import shapes.*
          |val s: Shape = Circle(1)
          |val h: Hole = Circle(2)
          |
          |s match
          |    Circle(r) -> print(r)
          |    Square(x) -> print(x)
          |
          |h match
          |    Circle(r) -> print(r)
          |    Slot(l) -> print(l)
          |""".stripMargin)) shouldBe "1\n2\n"
  }

  // Card `0295`, which is `0220` one kind of type over. `0220` guarded the *struct* arm and left the
  // variant arm below the conversion arms, so an **alias** of a variant's name never reached it: a
  // `type Eval = Result[…]` beside a `StmtKind.Eval` read `Eval(target)` as a cast from an integer
  // and refused it for carrying data — at a call whose parameter already said `StmtKind`. The
  // expected type was there and nothing asked it. Found writing slate.
  "an alias and a variant of one name are told apart the same way" - {
    val aliased =
      """enum StmtKind
        |    Eval(n: int)
        |    Skip
        |type Eval = Result[int, string]
        |""".stripMargin

    "the variant wins where the expected type names its enum" in {
      run(aliased +
        """take(k: StmtKind) -> int = k match
          |    Eval(n) -> n
          |    Skip -> 0
          |print(take(Eval(3)))
          |""".stripMargin) shouldBe "3\n"
    }

    // And the alias keeps call position where nothing expects the enum, exactly as a struct does —
    // a plain alias's name is its base's name, so this is the `Result` conversion a reader wrote.
    "and the alias keeps it where nothing expects the enum" in {
      run(aliased + "val e: Eval = Ok(7)\nprint(e.unwrap())\n") shouldBe "7\n"
    }

    // The refusal that used to fire, kept as a refusal at a site that really has no expected type
    // naming the enum: it is the conversion being complained about, which is now correct.
    "with the old refusal still reached where the alias really is what was named" in {
      err(aliased + "print(Eval(3))") should include("carries data")
    }
  }

  // A module-qualified path is folded into the key its enum is filed under, and the node that comes
  // back is analyzed again — so a head that was already a key used to be read as a path a second
  // time. `sysl.text$ParseError` split at its dot is `sysl` and `text$ParseError`, `sysl` is a
  // module, and the variant was refused as `undefined name 'sysl.text.24ParseError'`. A program's
  // own `aa.text` never reproduced it, because no module is named `aa`; the standard library's
  // root module is what made every one of its enums unreachable this way.
  "a variant is reached through the module of a standard-library enum" - {
    val describe =
      """code(e: text.ParseError) -> int = e match
        |    Empty -> 0
        |    BadDigit(at) -> 100 + int(at)
        |    Overflow -> 2
        |    BadBase(b) -> 300 + b
        |""".stripMargin

    "carrying a payload, through the module's import" in {
      run("import sysl.text\n" + describe +
        """val e: text.ParseError = text.ParseError.BadBase(3)
          |print(code(e), code(text.ParseError.BadDigit(7)))
          |print(e)
          |""".stripMargin) shouldBe "303 107\n3 is not a base between 2 and 36\n"
    }

    "carrying nothing, through the module's import" in {
      run("import sysl.text\n" + describe +
        """val e: text.ParseError = text.ParseError.Overflow
          |print(code(e), code(text.ParseError.Empty))
          |""".stripMargin) shouldBe "2 0\n"
    }

    "spelled in full under a wildcard import of the module" in {
      run(
        """import sysl.text.*
          |val a: ParseError = sysl.text.ParseError.BadBase(5)
          |val b = sysl.text.ParseError.Overflow
          |print(a, b)
          |""".stripMargin) shouldBe "5 is not a base between 2 and 36 value too large for its type\n"
    }

    "of another module's enum too" in {
      run(
        """import sysl.time
          |val d = time.Weekday.Monday
          |print(d == time.Weekday.Monday, d == time.Weekday.Friday)
          |""".stripMargin) shouldBe "true false\n"
    }

    // The refusal for a name the enum genuinely lacks names the enum as a reader spells it, and
    // never the key it is filed under nor an escaped copy of one.
    "and a variant the enum lacks is named in the dotted spelling" in {
      for src <- List("text.ParseError.Missing(3)", "text.ParseError.Missing") do
        val out = err("import sysl.text\nval e: text.ParseError = " + src + "\nprint(e)\n")

        out should include("sysl.text.ParseError")
        out should include("Missing")
        out should not include "$"
        out should not include ".24"
    }
  }
}
