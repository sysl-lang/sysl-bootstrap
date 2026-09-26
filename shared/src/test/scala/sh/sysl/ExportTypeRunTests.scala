package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** An exported `type` from the sysl side (`reference/ffi.md § Naming a type`).
 *
 * `@export` keeps the type's name so a generated header can spell it, where an ordinary alias
 * dissolves into its base. That is the whole of the difference it is allowed to make: a program
 * using the name reads, writes, selects, indexes and calls through it exactly as it would through
 * what it stands for, and these run the program to prove it rather than inspecting the IR.
 */
class ExportTypeRunTests extends AnyFreeSpec with RunSupport {

  "an exported scalar mixes with its base in both directions, with no conversion written" in {
    run("""@export("value")
          |type Handle = u64
          |
          |bump(h: Handle) -> u64 = h + 1
          |
          |val a: u64 = bump(41)
          |val b: Handle = a
          |print(a, b * 2)
          |""".stripMargin) shouldBe "42 84\n"
  }

  "an exported pointer is selected through, indexed, dereferenced and handed on as the pointer it is" in {
    run("""struct Vm
          |    base: u64
          |    xs: [4]i32
          |
          |@export("vm_ref")
          |type VmRef = *Vm
          |
          |@export("bytes_t")
          |type Bytes = *u8
          |
          |plain(v: *Vm) -> u64 = v.base
          |
          |var vm: Vm = Vm(100, [1, 2, 3, 4])
          |val p: VmRef = &vm
          |p.base = p.base + 1
          |p.xs[2] = 30
          |print(p.base, p.xs[2], (*p).base, plain(p))
          |
          |var arr: [3]u8 = [7, 8, 9]
          |val b: Bytes = &arr[0]
          |b[1] = 80
          |print(b[1], arr[1])
          |val view = b[0..<3]
          |print(view.len, view[2])
          |""".stripMargin) shouldBe "101 30 101 101\n80 80\n3 9\n"
  }

  /** The name is visible to the header and to nothing that checks types: inside a generic's
    * arguments, behind a pointer and in what a type parameter is solved to, it is the base.
    * `Buf[H]` beside `Buf[u64]` is the case that found this, where the two shared one instantiation
    * keyed by the base and whichever was met first gave the other its signature — so both orders.
    */
  "an exported scalar is its base inside a generic's arguments and behind a pointer" - {
    val header = "import sysl.buf.{Buf, buf}\n\n@export(\"h_t\")\ntype H = u64\n\n"

    "a Buf of it beside a Buf of the base, the alias met first" in {
      run(header +
        """sum(b: *Buf[u64]) -> u64 = b.at(0) + b.at(1)
          |
          |main()
          |    var a: Buf[H] = buf()
          |    a.push(1)
          |    a.push(2)
          |    var b: Buf[u64] = buf()
          |    b.push(10)
          |    b.push(20)
          |    print(sum(&a), sum(&b), a.at(1) + b.at(0))
          |""".stripMargin) shouldBe "3 30 12\n"
    }

    "and the base met first" in {
      run(header +
        """sum(b: *Buf[H]) -> H = b.at(0) + b.at(1)
          |
          |main()
          |    var b: Buf[u64] = buf()
          |    b.push(10)
          |    b.push(20)
          |    var a: Buf[H] = buf()
          |    a.push(1)
          |    a.push(2)
          |    print(sum(&a), sum(&b), b.at(1) + a.at(0))
          |""".stripMargin) shouldBe "3 30 21\n"
    }

    "a pointer to it is taken from a place of the base, and handed back as one" in {
      run(header +
        """first(p: *H) -> H = p[0]
          |firstBase(p: *u64) -> u64 = p[0]
          |
          |main()
          |    var b: Buf[u64] = buf()
          |    b.push(2)
          |    val p: *H = &b.elems[0]
          |    val q: *u64 = p
          |    print(first(p) + b.at(0))
          |    p[0] = 5
          |    print(first(q), firstBase(p))
          |""".stripMargin) shouldBe "4\n5 5\n"
    }

    "a type parameter solved from it is solved to the base" in {
      run(header +
        """struct Vm
          |    base: u64
          |
          |@export("vm_ref")
          |type VmRef = *Vm
          |
          |load[T](p: *T) -> T = *p
          |same[T](a: T, b: T) -> T = b
          |
          |main()
          |    var vm: Vm = Vm(7)
          |    val r: VmRef = &vm
          |    val h: H = 3
          |    val n: u64 = 4
          |    print(load(r).base, same(h, n), same(n, h))
          |""".stripMargin) shouldBe "7 4 3\n"
    }
  }

  // The rule is about a name and nothing more: a narrowed type checks what is written into it, so a
  // pointer to one is not a pointer to its base — a write through the second would skip the check.
  "a narrowed type keeps its own identity behind a pointer" in {
    val src = """type Small = u64 where value < 100
                |
                |main()
                |    var x: u64 = 500
                |    val p: *Small = &x
                |    print(p[0])
                |""".stripMargin
    Compiler.compiled(List(Source("<input>", src))) match
      case Left(e)  => e should include("declared *Small but the value is *ulong")
      case Right(_) => fail("a *u64 was accepted as a *Small")
  }

  "an exported function pointer is called through as one" in {
    run("""@export("on_event")
          |type OnEvent = *extern(i32) -> i32
          |
          |@export
          |twice(n: i32) -> i32 = n * 2
          |
          |apply(f: OnEvent, n: i32) -> i32 = f(n)
          |
          |val f: OnEvent = &twice
          |print(f(21), apply(f, 5))
          |""".stripMargin) shouldBe "42 10\n"
  }
}
