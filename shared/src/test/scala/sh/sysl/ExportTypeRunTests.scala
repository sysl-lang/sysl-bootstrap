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
