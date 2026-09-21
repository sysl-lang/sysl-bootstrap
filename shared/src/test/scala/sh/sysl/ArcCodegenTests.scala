package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-1 lowering of automatic reference counting: the shape of a box, where the counts are
 * taken and given back, and that the atomic mode really is atomic. Placement is what makes ARC
 * correct, so it is checked in the emitted text rather than only through a program's output.
 */
class ArcCodegenTests extends AnyFreeSpec with CodegenSupport {

  private val point = "struct Point\n    x: int\n    y: int\n"

  // A payload the reaper's tests can hang a `&sync` off, kept apart from `point` so that changing
  // either one does not silently change what the other's assertions are counting.
  private val chain = "struct Node\n    v: int\n"

  // A monomorphized name carries its generic's **key**, so the library's `Option` appears here as
  // `sysl$Option` — and inside a regex the `$` has to be escaped, since it would otherwise read as
  // an end-of-input anchor and match nothing at all.
  private val opt   = Library.key("Option")
  private val optRe = keyRe("Option")

  "a box is the strong count, the deallocation hook, the weak count, and the payload" in {
    val out = ir(point + "var p: &Point = Point(1, 2)")

    out should include("%arc.Point = type { i64, ptr, i64, %struct.Point }")
    out should include("declare ptr @malloc(i64)")
    out should include("declare void @free(ptr)")
  }

  "a construction the context wants by reference goes on the heap" in {
    val out = ir(point + "var p: &Point = Point(1, 2)")

    out should include regex raw"%t\d+ = call ptr @malloc\(i64 %t\d+\)"
    out should include regex raw"store i64 1, ptr %t\d+"
    // A payload holding nothing has no contents to walk, and still has storage to give back — so the
    // hook is the plain one rather than none at all.
    out should include("store ptr @arc.drop.plain, ptr")
  }

  // The weak count starts at one because the strong references hold one share between them
  // (`reference/memory.md § What a heap object costs`), so the storage lives exactly until the
  // object and every weak reference to it are both gone.
  "and its weak count starts at the one share the strong references hold together" in {
    val out = ir(point + "var p: &Point = Point(1, 2)")

    out should include regex raw"%t\d+ = getelementptr %arc\.Point, ptr %t\d+, i32 0, i32 2\n  store i64 1, ptr %t\d+"
  }

  "the same construction with no expectation stays a value" in {
    val out = ir(point + "var p = Point(1, 2)")

    out should not include "@malloc"
    out should include("%p.addr = alloca %struct.Point")
  }

  "a local takes a count of its own and gives it back when its scope ends" in {
    val out = ir(point + "var p: &Point = Point(1, 2)\nprint(p.x)")

    out should include regex raw"call void @arc\.retain\(ptr %t\d+\)\n  store ptr %t\d+, ptr %p\.addr"
    out should include regex raw"load ptr, ptr %p\.addr\n  call void @arc\.release\(ptr %t\d+\)\n  ret i32 0"
  }

  "assignment retains the new reference before releasing the old one" in {
    val out = ir(point + "var p: &Point = Point(1, 2)\np = Point(3, 4)")
    val store =
      raw"load ptr, ptr %p\.addr\n  call void @arc\.retain\(ptr %t\d+\)\n" +
        raw"  store ptr %t\d+, ptr %p\.addr\n  call void @arc\.release\("

    out should include regex store
  }

  // A count of its own is what a function takes when it *can* release something, which is every
  // function but the leaf reader `BorrowedParams` describes — that one reads through the caller's
  // count instead, and `BorrowedParamTests` is where the difference is asserted. The result's count
  // is taken either way, because what comes back is the caller's to hold.
  "a function that can release owns its parameters and hands back its result with a count taken" in {
    val src = point + "keep(p: &Point) -> &Point\n    print(p.x)\n    p\n" +
      "var q: &Point = Point(1, 2)\nvar r = keep(q)"
    val out = ir(src)

    out should include("call void @arc.retain(ptr %p.param)")
    out should include regex raw"call void @arc\.release\(ptr %t\d+\)\n  ret ptr %t\d+"
  }

  "a destructor lets go of what the payload held, and leaves the storage to whoever is last" in {
    val src = "struct Node\n    value: int\n    next: Option[&Node]\nvar n: &Node = Node(1, None)"
    val out = ir(src)

    out should include("define private void @arc.drop.Node(ptr %p, i1 %storage) {")
    out should include("call void @arc.dispose.Node(%struct.Node %t2)")
    // The hook is asked which of its two jobs is wanted. Walking the payload and giving the storage
    // back happen at different moments — the strong count reaching zero and the weak count reaching
    // zero — so the walk still ends in `ret void` and the free is the other arm.
    out should include regex raw"call void @arc\.dispose\.Node[^\n]*\n  ret void"
    out should include("call void @free(ptr %p)")
    out should include("store ptr @arc.drop.Node, ptr")
  }

  "copying an aggregate takes a share of every reference inside it" in {
    val src = "struct Node\n    value: int\n    next: Option[&Node]\nvar n: &Node = Node(1, None)"
    val out = ir(src)

    out should include("define private void @arc.copy.Node(%struct.Node %v) {")
    out should include(s"call void @arc.copy.$opt.ref.Node(%enum.$opt.ref.Node %t1)")
  }

  // The tag is read straight off the value, since it is the aggregate's own first member; the
  // payload is not, because it shares one region with every other variant — so reaching it is a
  // store and a load back at this variant's type (`reference/types.md § Enums`).
  "a data enum walks only the variants that carry a reference, behind a tag test" in {
    val src = "struct Node\n    value: int\n    next: Option[&Node]\nvar n: &Node = Node(1, None)"
    val out = ir(src)

    out should include(s"define private void @arc.dispose.$opt.ref.Node(%enum.$opt.ref.Node %v) {")
    out should include regex raw"extractvalue %enum\.${optRe}\.ref\.Node %v, 0\n  %t\d+ = icmp eq i32 %t\d+, 0"
    out should include regex
      raw"getelementptr %enum\.${optRe}\.ref\.Node, ptr %t\d+, i32 0, i32 1\n" +
      raw"  %t\d+ = load %${optRe}\.ref\.Node\.Some, ptr %t\d+"
    out should include regex raw"extractvalue %${optRe}\.ref\.Node\.Some %t\d+, 0\n  call void @arc\.release\("
  }

  "an ordinary reference counts with a plain load and store" in {
    val out = ir(point + "var p: &Point = Point(1, 2)")

    out should include("define private void @arc.release(ptr %p) {")
    out should not include "atomicrmw sub"
  }

  "an atomic reference counts atomically and fences before it frees" in {
    val out = ir(point + "var p: &sync Point = Point(1, 2)")

    out should include("%o = atomicrmw add ptr %p, i64 1 monotonic")
    out should include("atomicrmw sub ptr %p, i64 1 release")
    // On the zero transition the atomic path fences, then hands the object to the iterative reaper
    // rather than recursing into its destructor.
    out should include regex raw"reap:\n  fence acquire"
    out should include("call void @arc.reap(ptr %p)")
  }

  "the two reference modes share one box layout, since atomicity belongs to the reference" in {
    val src = point + "var p: &Point = Point(1, 2)\nvar q: &sync Point = Point(3, 4)"
    val out = ir(src)

    out.linesIterator.count(_.startsWith("%arc.Point = type")) shouldBe 1
    out should include("define private void @arc.release(ptr %p) {")
    out should include("define private void @arc.release_sync(ptr %p) {")
  }

  "a field through a reference reaches past the header" in {
    val out = ir(point + "var p: &Point = Point(1, 2)\np.y = 9")

    out should include regex raw"getelementptr %arc\.Point, ptr %t\d+, i32 0, i32 2"
    out should include regex raw"getelementptr %struct\.Point, ptr %t\d+, i32 0, i32 1"
  }

  // Deliberately silent: `print` renders through a slice of a local buffer, and a slice carries an
  // owner slot, so making one turns the ARC runtime on even where the owner is statically null.
  // That is a codegen over-approximation rather than an allocation, and it is not what this test
  // is about.
  "a program that never allocates declares no allocator" in {
    ir("var n = 1\nn += 1") should not include "@malloc"
  }

  // The shape a program without a heap is built out of: a fixed table of task blocks, walked by
  // index, with its lists threaded through the blocks themselves. Handing the table to a callee as
  // a slice *declares* the ARC runtime — the over-approximation noted above, a retain of an owner
  // that is statically null — so what is asserted is that nothing ever calls the allocator.
  "nor does one that walks a fixed table by index ever call it" in {
    val out = ir("""struct Node
                   |    v: int
                   |    next: Option[u8]
                   |end Node
                   |sum(ns: []Node, first: Option[u8]) -> int
                   |    var total = 0
                   |    var cur = first
                   |
                   |    while cur.is_some()
                   |        var i = usize(cur.unwrap())
                   |
                   |        total += ns[i].v
                   |        cur = ns[i].next
                   |
                   |    total
                   |end sum
                   |var t: [3]Node = [Node(0, None); 3]
                   |t[0usize] = Node(10, Some(1u8))
                   |t[1usize] = Node(20, None)
                   |print(sum(t[..], Some(0u8)))
                   |""".stripMargin)

    out should not include "call ptr @malloc"
    out should not include "@arc.copy"
  }

  /** The reaper's worklist, which is scratch space a thread uses while it drains and not state two
   * threads share. Its being per-thread is what keeps a concurrent `&sync` drop from having one
   * thread overwrite the other's list head — and it is invisible from a program's output, so the
   * storage class is asserted in the text.
   */
  "the reaper's worklist" - {
    "is thread-local, along with the flag saying a drain is already running" in {
      val out = ir(chain + "var p: &sync Node = Node(1)")

      out should include("%arc.reaper = type { ptr, i8 }")
      out should include("@arc.reaper.self = internal thread_local global %arc.reaper zeroinitializer")
    }

    // A machine that knows what the current thread is has no reason to ask anybody: the slot is
    // reached by name, and the symbol a port would define is not emitted at all.
    "and a target with it asks nobody for the slot" in {
      val out = ir(chain + "var p: &sync Node = Node(1)")

      out should include("%head = getelementptr %arc.reaper, ptr @arc.reaper.self, i32 0, i32 0")
      out should not include ArcEmitter.reaperSlot
    }

    // The reaper is unchanged by where the slot came from: it still threads the list through the
    // dead count slot and still drains in a loop rather than recursing.
    "and the reaper reads and writes it exactly as it did" in {
      val out = ir(chain + "var p: &sync Node = Node(1)")

      out should include regex raw"%w = load ptr, ptr %head\n  store ptr %w, ptr %p"
      out should include("store ptr %p, ptr %head")
      out should include regex raw"drain:\n  store i8 1, ptr %flag"
    }

    /** A bare target cannot have the slot as a thread-local, and the reason is not that threads are
     * unlikely there: asked for one, LLVM gives a freestanding ELF target the **local-exec** model,
     * whose `:tprel_hi12:` offset is read from a thread pointer register nothing on a bare machine
     * has written. That links clean and reads a wild address.
     *
     * So it asks instead, and the answer is the environment's — which is the same arrangement
     * `&sync`'s counts are already under, since an `atomicrmw` with no `ldrex`/`strex` under it
     * becomes a call to an `__atomic_*` the board defines.
     */
    "is fetched where nothing has set thread-local storage up" in {
      val out = irFor(Target.aarch64Freestanding, chain + "var p: &sync Node = Node(1)")

      out should include(s"%s = call ptr @${ArcEmitter.reaperSlot}()")
      out should include("%head = getelementptr %arc.reaper, ptr %s, i32 0, i32 0")
      out should not include "thread_local"
    }

    // `weak`, so the overwhelming case — a bare-metal program with no scheduler at all — links with
    // nothing supplied and drains through the one slot exactly as it always has. A port that
    // schedules defines the symbol over this and wins the link.
    "and the program supplies its own answer weakly, so a port can win the link" in {
      val out = irFor(Target.aarch64Freestanding, chain + "var p: &sync Node = Node(1)")

      out should include(s"define weak ptr @${ArcEmitter.reaperSlot}()")
      out should include("@arc.reaper.self = internal global %arc.reaper zeroinitializer")
      out should include regex raw"entry:\n  ret ptr @arc\.reaper\.self"
    }

    // Stated over the whole registry rather than for the two targets above, so that a target added
    // later cannot be reaching for a thread pointer it has not got, or paying for a call it need
    // not make.
    "and every target either has the slot or asks for it, never both and never neither" in {
      for t <- Target.all if t.supported do
        withClue(t.name) {
          val out = irFor(t, chain + "var p: &sync Node = Node(1)")
          val tls = if t.hasThreadLocalStorage then "thread_local " else ""

          out should include(s"@arc.reaper.self = internal ${tls}global %arc.reaper zeroinitializer")
          out.contains(s"define weak ptr @${ArcEmitter.reaperSlot}()") shouldBe !t.hasThreadLocalStorage
        }
    }

    // That the loop still *works* once it threads its work through a `getelementptr` is
    // `RecursiveTeardownRunTests`, which drops a chain long enough that one C frame per node would
    // be the difference between working and not — including through the atomic release path, which
    // is the one this slot exists for. Read there rather than duplicated here.
  }
}
