package sh.sysl

/** The machine a program is compiled **for**.
 *
 * A target is the pair a systems language cannot do without — a processor and the operating system
 * conventions layered on it — together with the handful of facts the compiler has to know to emit
 * correct code for it. It is a value, not a global: an invocation names one, it is carried down to
 * codegen, and nothing consults the machine the compiler happens to be running on. That is what
 * lets one compiler build for a machine it is not, and it is why the host appears here only as the
 * **default** an invocation that names no target gets.
 *
 * The facts recorded are the ones something in the compiler reads today. A target is not a
 * description of a machine — it is the answer to the questions codegen asks — so the way to add a
 * field is to have something need it, and the way to add a target is to verify its answers against
 * a C compiler for the same triple rather than to reason about the ABI document.
 */
case class Target(
    name: String,
    triple: String,
    cpu: Cpu,
    os: Os,
    vaList: VaListAbi,
    vaListBytes: Int,
    softFloat: Boolean = false,
    /** Whether a C enumerated type here is the **smallest** type holding its values rather than
      * always an `int` — AAPCS's rule, which GNU's `arm-none-eabi` implements by defaulting to
      * `-fshort-enums`, and which clang does not default to for the same triple.
      *
      * Recorded for the reason `softFloat` is: sysl hands its triple to clang and is *joining* a C
      * build rather than starting one, so where the two disagree about a convention it is sysl that
      * follows. It reaches exactly one decision — the flag `Toolchain.compileC` passes for a
      * package's C — because nothing sysl itself emits has a C enum in it.
      *
      * **The disagreement was being reported all along and went unread.** Every link of a Wi-Fi demo
      * printed `uses 32-bit enums yet the output is to use variable-size enums; use of enum values
      * across objects may fail`, once per object built out of the package tree, for two days.
      * Nothing miscompiled only because nothing in `pico2` carries an enum across the boundary yet:
      * the first shim taking one by value, or a struct with an enum field, is where it stops being a
      * warning.
      */
    shortEnums: Boolean = false,
    /** Whether the machine has no floating-point unit at all, so no floating-point **instruction**
      * may be emitted for it.
      *
      * **It is a different claim from `softFloat`, and the two only sometimes give the same answer.**
      * `softFloat` is about the *convention* — where a `double` travels on the way into a call — and a
      * core can perfectly well have an FPU and pass arguments in core registers anyway, which is what
      * `-mfloat-abi=softfp` means and what pico-sdk builds. This is about the *hardware*: a Cortex-M3
      * has no such unit to have a convention about, and a Cortex-M4 on a board that leaves the FPU off
      * has one that must not be used.
      *
      * **It cannot be left to the triple, because the Arm triples do not say it.** Measured rather
      * than assumed: `clang --target=thumbv7em-none-eabi` compiles `a * b` to `vmul.f32` and defines
      * `__ARM_FP 0x6`, and `thumbv8m.main-none-eabi` gives `vmla.f32` and `__ARM_FP 0xe` — the `eabi`
      * suffix chose a calling convention and said nothing whatever about the presence of the unit. An
      * image built from either faults on a board that has not got one, and a header that checks —
      * CMSIS's *"Compiler generates FPU instructions for a device without an FPU"* — refuses to
      * compile at all.
      *
      * It reaches exactly one decision, the flag `Toolchain.machineFlags` puts on every clang command
      * line for this target, and that flag is Arm's because Arm is where the triple is silent. RISC-V
      * and Armv6-M answer `true` here honestly and need nothing said on their behalf: their triples
      * already describe a machine with no unit.
      */
    noFpu: Boolean = false,
    /** The floating-point unit the machine **has**, spelled the way clang's `-mfpu` spells it, for a
      * row that has one and a triple that does not settle it.
      *
      * **It is `noFpu`'s other half, and the pair exists because a default belongs to the
      * toolchain.** `noFpu` says the unit is absent and passes `-mfpu=none` to say so; saying nothing
      * in the other case left its *presence* to whatever clang defaults the triple to, and clangs
      * disagree. Measured on `thumbv8m.main-none-eabi` with no `-mfpu`: Apple clang 21 and Homebrew
      * clang 22 define `__ARM_FP 0xe`, and the apt.llvm.org clang 20 the Linux CI installs defines
      * nothing at all. So `thumb-freestanding-softfp` — whose whole point is that the unit is used
      * and only the convention is in core registers — was a hard-float target on one machine and a
      * soft one on the other, and the paired cases `FpuPresenceTests` writes for it failed on Linux
      * across two releases while passing here.
      *
      * **What is named is the silicon's unit and not the triple's default, which is a different
      * answer.** A Cortex-M33 has an `fpv5-sp-d16`: single precision, with no double-precision
      * arithmetic in hardware. clang's default for `thumbv8m.main` is the `fpv5-d16` above, which
      * claims a double unit the part has not got and lowers a `f64` multiply to `vmul.f64` — an
      * instruction that faults on the board, which is the failure these two fields exist to prevent
      * arrived at from the other side.
      *
      * **Naming the unit is half of it — the float ABI is the other half, because `soft` overrides
      * `-mfpu`.** `-mfloat-abi=soft -mfpu=fpv5-sp-d16` defines no `__ARM_FP` whatever: the convention
      * wins, and a clang that defaults a bare `eabi` triple to `soft` therefore ignored the unit this
      * field names. So `machineFlags` states both, and `softFloat` is what picks the convention —
      * `softfp` where it is true, which means exactly *a unit, used, with arguments in core
      * registers*.
      *
      * It reaches the one decision `noFpu` reaches, the flags `Toolchain.machineFlags` puts on every
      * clang command line for this target. A row leaves it empty where the triple already answers —
      * every hosted target, RISC-V, Armv6-M and Armv7-M — and no row sets it beside `noFpu`, which
      * `TargetTests` pins.
      */
    fpu: Option[String] = None,
) {

  /** How wide an address is, as a value that can be handed to the two places that need it — the
   * layout of anything holding a pointer or a length, and the LLVM type of a view (`Word`).
   */
  def word: Word = Word(cpu.bits)

  def pointerBits: Int = cpu.bits

  def pointerBytes: Int = cpu.bits / 8

  /** Whether floating-point values travel in floating-point registers. This is a question only
   * RISC-V answers two ways: a hosted RISC-V system is built for the D extension and passes them
   * there, and a bare-metal one is not and passes them as the integers they are laid out as. It is
   * recorded here rather than derived because sysl hands its triple to clang, so the two have to
   * make the same default assumption about the same triple or the call disagrees.
   *
   * It reaches exactly one decision: whether a small aggregate of floating members is **flattened**
   * into registers (`CAbi`). A scalar needs nothing here — the backend applies the convention to
   * those itself, from the same triple.
   */
  def hardFloat: Boolean = !softFloat

  /** Whether a `thread_local` global on this target reaches storage something has actually set up.
   *
   * Every target LLVM knows will *accept* the keyword, so this is not a question about the backend —
   * it is a question about what runs before `main`. A hosted system lays down a thread's storage as
   * part of starting it, and a bare one has nothing that would: there is no loader, no libc, and
   * nothing that writes the thread pointer.
   *
   * The reason it has to be asked rather than left to the backend is that the failure is silent.
   * Asked for a freestanding ELF target, LLVM upgrades an internal thread-local to the **local-exec**
   * model — a `:tprel_hi12:` offset from `TPIDR_EL0` on AArch64 — which links clean and reads
   * whatever the register happened to hold. A kernel that never set one up would get a wild address
   * out of the arc runtime rather than a diagnostic out of the linker.
   *
   * Nothing is lost by answering `false` there, because nothing on a bare target can spawn: the
   * threads a program gets from sysl come from `sysl.posix.threads`, which is `requires posix`. A kernel
   * that implements threads of its own and shares a reference across them is outside what the
   * compiler can see either way — it does not know that scheduler exists.
   */
  def hasThreadLocalStorage: Boolean = os != Os.Freestanding

  /** Whether something on this platform runs a module's initializers **before** the program's own
   * entry point — which is what decides whether an archive with no entry point of its own can fill
   * its module storage at all (`reference/modules.md § val — a thing`).
   *
   * A program lays its computed `val`s down at the top of `@main`, because the entry point is the
   * one place that certainly runs first and it is already written. `build-c` has no entry point to
   * put them in: the artifact is linked into a C project that supplies its own `main`. What answers
   * for it there is `@llvm.global_ctors`, which LLVM lowers to `.init_array` on ELF,
   * `__mod_init_func` on Mach-O and `.CRT$XCU` on COFF — so the spelling is the back end's problem
   * rather than one this compiler has to keep a table of.
   *
   * **A freestanding target answers `false`, and the reason is the same one
   * `hasThreadLocalStorage` gives: there is no loader.** Nothing walks `.init_array` on bare metal
   * unless the image's own start-up calls `__libc_init_array` — newlib's does and a hand-written
   * reset vector may not — so a constructor emitted there is a function that is never called, and
   * the storage it would have filled reads whatever the image left. That is a silent wrong answer,
   * which is why an export reaching computed storage is still refused for those targets
   * (`Exports.storage`) instead of being served by a constructor nothing runs.
   */
  def runsInitializers: Boolean = os != Os.Freestanding

  /** Whether a `bf16` reaches this machine's back end as the `i16` of its bits rather than as a
   * `bfloat` (`SoftBf16`).
   *
   * **WebAssembly is the one back end that cannot convert a `bfloat`.** Every other one sysl targets
   * widens a `bfloat` operation to `float` and rounds the answer back itself; LLVM's wasm back end
   * has no selection for either conversion, so the first one it meets — and the standard library
   * has one in every `Float for bf16` body — stops the build with *"Cannot select: bf16_to_fp"*.
   * Measured against Apple clang 21 and LLVM 23, at both wasm rows. Nothing about the language
   * changes: the widening is what the other back ends do, written out by the compiler instead.
   */
  def bf16AsBits: Boolean = cpu == Cpu.Wasm32

  /** The environment capabilities this machine can have **at all**, which is a different question
   * from what a project says it provides.
   *
   * `package.hocon` is the authority on what a target *offers* and is the one to ask nearly
   * everywhere — but its prior is that a machine can do everything, so a capability the file does not
   * mention comes back provided. That prior is right for policy and wrong for physics: a freestanding
   * target has no operating system whatever a config omits to say, and no `<regex.h>` for a probe to
   * include. Asking `provides` where the question is physical answers `true` for every target and
   * gates nothing, which is the trap this exists to close.
   *
   * Only the two environment capabilities have a physical half. `heap` is deliberately absent —
   * whether there is an allocator is an engineering decision about a machine that could have one
   * either way, which is what `package.hocon` is for.
   *
   * **`Conditional` derives its two facts from this**, so the compiler holds one answer to "is this
   * machine hosted" rather than two that can drift apart.
   *
   * **It is the operating system's answer and not the whole machine's**, which is why the work is
   * `Os`'s. A directory selects on what the walk that finds it knows, and that is an `Os` — so a
   * selector naming `posix` has to be answerable without a processor, and this is where that is
   * settled for both askers at once (`reference/modules.md § Platform selection`).
   */
  def inherentCapabilities: Set[String] = os.inherentCapabilities

  /** Whether the compiler can lower for this target at all. A target it knows and cannot lower for
   * is worth naming: the diagnostic then says what is missing instead of leaving the name unknown,
   * which reads as a typo.
   */
  def supported: Boolean = Cpu.buildable.contains(cpu)

  /** Whether there is a **clang** for this machine — which is a different question from whether sysl
   * can lower for it, and CRAFT is where the two part company.
   *
   * Every other row in the registry is a triple some installed clang accepts, so `build`, `run`,
   * `build-c`, `build-lib` and a `c const` probe all work by handing it one. CRAFT's back end is out
   * of tree and is an `llc` rather than a driver, and the machine has no libc, no object format and
   * no linker — so there is nothing for any of those to call. What sysl does for it is write the
   * LLVM; `llc -march=craft` and `craft as` are the reader's own two commands.
   *
   * It is asked rather than derived from `Os.Freestanding`, because every other freestanding row
   * has a perfectly good cross clang. The distinction is the toolchain's existence, not the
   * machine's bareness.
   */
  def buildsWithClang: Boolean = cpu != Cpu.Craft

  /** Whether a program for this machine is **loaded as a shared object**, which decides whether the C
   * a package carries has to be position-independent.
   *
   * **Android is the only row where it is true, and it is true of every Android program.** There is
   * no executable to run: `SDLActivity` — or any other host — `dlopen`s a `.so` and calls into it, so
   * everything sysl compiles for that machine ends up inside one.
   *
   * ==Why this is about the CARRIED C and not about sysl's own output==
   *
   * `getting-started/cli.md § targets` establishes that sysl's own objects need no relocation
   * model: every global the emitter writes is `Linkage.Private`, so it is not preemptible and
   * lowers to a PC-relative pair against a local section. That is still true and nothing here
   * changes it.
   *
   * **A package's vendored C is the other half, and it has ordinary C globals.** Those *are*
   * preemptible, so without `-fPIC` clang emits `ADR_PREL_PG_HI21` against the symbol itself and the
   * shared link refuses it — `relocation R_AARCH64_ADR_PREL_PG_HI21 cannot be used against symbol
   * 'b2AssertHandler'; recompile with -fPIC`. With the flag the same reference goes through the GOT
   * (`ADR_GOT_PAGE`), which is what a `.so` needs. Found building `sysl-lang/androidkit` against
   * `sh.sysl.box2d`, whose vendored Box2D has exactly such globals.
   *
   * **It is asked rather than derived from `os.inherentCapabilities`,** because it is not a property
   * of the operating system's services: Linux hosts shared libraries perfectly well and a sysl
   * program for it is an ordinary executable. What this records is the shape of the *output*, which
   * is a fact about the platform's convention rather than about its libc.
   *
   * Freestanding rows answer `false` and must: `-fPIC` on a bare-metal image adds a GOT nothing sets
   * up.
   */
  def positionIndependent: Boolean = os == Os.Android

  /** What to say to somebody who asked for a build sysl cannot drive — the sentence, once, so the
   * five subcommands that refuse do not each invent their own half of it.
   */
  def noToolchain: String =
    s"'$name' has no C toolchain — its LLVM back end is out of tree, and the machine has no libc, " +
      "no object format and no linker for sysl to drive. Write the LLVM with 'sysl emit-llvm " +
      s"--target $name', then 'llc -march=${cpu.backend}' and 'craft as'"

  /** Why not, where not — `None` for a target that builds.
   *
   * **It is here rather than at either reader because there are two of them**, and the same person
   * reads both minutes apart: `Target.named` refusing `--target x86-linux`, and `sysl targets`
   * annotating the row. A list that gives a different reason from the refusal is worse than a list
   * that gives none, and that is what this was — the row read *"32-bit — not yet supported"* after
   * two 32-bit targets had shipped, because it was a separate sentence with nothing to hold it to
   * the other.
   */
  def unsupported: Option[String] =
    Option.unless(supported)(s"no C calling convention has been measured for ${cpu.symbol}")
}

/** How wide an address is on the target, and the one fact about a machine that reaches into the
 * *types* the compiler writes rather than only into the calls it makes.
 *
 * Almost nothing about lowering depends on the target. A struct's members sit at the same offsets
 * everywhere, an `i32` is an `i32`, and the emitted module states its triple so LLVM derives the
 * data layout rather than sysl writing it down twice. **One thing is not like that**: a view is
 * `{ ptr, ptr, iN }` where `N` is the address width, because its length is a `usize` — so the LLVM
 * form of a slice, and of anything containing one, cannot be written without knowing the machine.
 *
 * It is a parameter rather than a global for the reason `getting-started/cli.md § targets` gives
 * for the target itself: a compiler that reads the width from somewhere ambient can be wrong about
 * it silently, and one that is handed it cannot compile at all until every caller has said which
 * machine it means.
 */
case class Word(bits: Int) {
  def bytes: Int = bits / 8

  /** The integer that is exactly one address wide — what `usize` and `isize` lower to, and what a
   * view's length is.
   */
  def lty: ir.LType = ir.LType.I(bits)

  def llvm: String = lty.render
}

/** The processor, and the two things about it the compiler asks: how wide an address is, and which
 * C calling convention its aggregates cross under (`CAbi`).
 */
enum Cpu(val bits: Int) {
  case Aarch64 extends Cpu(64)
  case X86_64  extends Cpu(64)
  case Riscv64 extends Cpu(64)
  case Riscv32 extends Cpu(32)
  case Thumb   extends Cpu(32)
  case Wasm32  extends Cpu(32)
  case X86     extends Cpu(32)

  /** CRAFT — *Compact RISC Architecture For Teaching*, and the first machine here narrower than a
    * `long`, an `int`, or the pointer every other row shares a width with.
    *
    * **Sixteen bits is the whole of what makes it different, and it reaches further than any other
    * field in this registry does.** `Word(16)` is what `usize` and `isize` become, so a view is
    * `{ ptr, ptr, i16 }` and a program's whole address space is the 64 KiB one length can name. An
    * `int` stays 32 bits and a `long` stays 64, because a width is the language's answer and not the
    * machine's — so on this target the ordinary arithmetic of an ordinary program is multi-word, and
    * the back end below expands it.
    *
    * There is deliberately no `Craft32`, and there never will be: the ISA's own positioning is that
    * *"the moment CRAFT grows to 32 bits, RISC-V does it better and the reason to exist evaporates"*.
    * The name carries no width for that reason — one machine, one row.
    */
  case Craft   extends Cpu(16)

  /** How a source file names this processor — in a `#if` condition and in an assembly arm alike.
   * One spelling for both, so a program that gates on a processor and one that writes instructions
   * for it are naming the same thing.
   *
   * `thumb` rather than `arm` because it is the instruction set, not the architecture family, that
   * an assembly arm has to be right about: a Cortex-M executes Thumb only, and an arm written for
   * A32 would assemble for a machine that cannot run it.
   */
  def symbol: String = this match
    case Aarch64 => "aarch64"
    case X86_64  => "x86_64"
    case Riscv64 => "riscv64"
    case Riscv32 => "riscv32"
    case Thumb   => "thumb"
    case Wasm32  => "wasm32"
    case X86     => "x86"
    case Craft   => "craft"

  /** The name LLVM registers this processor's back end under, which is what `clang -print-targets`
   * lists and so what says whether a given clang can produce objects for this target at all.
   *
   * **It is not `symbol`, and the difference is not cosmetic.** `symbol` is what a *program* writes
   * in a `#if` or an `asm` arm and is sysl's to choose; this is LLVM's, and LLVM spells the 64-bit
   * x86 back end `x86-64` where a source file says `x86_64`. Deriving one from the other would work
   * for six processors out of seven and fail for that one, silently, in the form of a clang that
   * looks incapable of a target it handles perfectly well.
   *
   * **It is also what says a target needs a clang the PATH may not have.** Apple's clang registers
   * eleven back ends and `wasm32` is not among them, so `Toolchain.findClang` falls through to
   * Homebrew's LLVM for that one exactly as it already does for RISC-V.
   */
  def backend: String = this match
    case Aarch64 => "aarch64"
    case X86_64  => "x86-64"
    case Riscv64 => "riscv64"
    case Riscv32 => "riscv32"
    case Thumb   => "thumb"
    case Wasm32  => "wasm32"
    case X86     => "x86"
    // Registered by an **out-of-tree** back end rather than by any clang somebody installs, so this
    // is the one name `Toolchain.findClang` never goes looking for: `craftFreestanding` is refused a
    // toolchain outright and reaches `llc` only by the reader's own hand.
    case Craft   => "craft"
}

object Cpu {

  /** The processors a target can actually be built for, which is what an exhaustive set of assembly
   * arms has to cover (`reference/inline-assembly.md § Every architecture needs an answer`).
   *
   * `x86` is nameable and not lowerable, and **the reason is no longer its width** — 32-bit targets
   * build. It is that nothing has measured i386's C calling convention against clang, which is what
   * `getting-started/cli.md § targets` says a target's answers have to come from. Requiring an
   * assembly arm for a processor no program can be built for would be requiring an answer to a
   * question nobody can ask; when the convention is measured it joins this list, and the arms that
   * do not cover it become errors, which is the work of supporting it.
   */
  def buildable: List[Cpu] = values.filterNot(_ == X86).toList
}

/** The system conventions the processor is used under. `Freestanding` is a target with no operating
 * system at all — a kernel or a bare-metal program — which is a real answer here rather than a
 * missing one: the ABI of a freestanding ELF target is fully specified, and it differs from a
 * hosted one of the same processor only where the OS is what fixed the convention.
 */
enum Os {
  case MacOS, Linux, Windows, Freestanding, Android, Wasi

  /** The environment capabilities a machine running this operating system has **at all** — the
   * physical half of the two-level rule, as against what `package.hocon` says a target offers.
   *
   * It lives here rather than on `Target` because it needs nothing else about the machine, and two
   * askers depend on that: `Conditional` gates a `#if` on it, and a `__<os>__` directory selects on
   * it during a walk that has an operating system and no processor. One answer, so a machine cannot
   * be hosted for one asker and bare for the other.
   *
   * **Android answers both the way Linux does, and it is a separate case anyway.** Bionic is a POSIX
   * libc and there is a kernel under it, so a program that gates on `posix` or `hosted` wants this
   * machine included. What it is *not* is glibc: the `-l` names differ, `pkg-config` does not exist,
   * and the graphics and logging a program reaches for are `libandroid`/`liblog` rather than
   * anything a desktop has. Answering `linux` to those questions would be answering them wrong, and
   * a symbol a source file can test is exactly where that has to be distinguishable.
   *
   * **WASI answers the way Windows does, and that rung was already occupied.** A preview1 module has
   * files, a clock, randomness, arguments and exit, so it is plainly not freestanding; it has no
   * fork, no sockets and no threads, so it is plainly not POSIX. That is hosted-but-not-POSIX, which
   * is what `Os.Windows` has always been — so this needs no capability of its own, and the two-level
   * model absorbs it with one more name in the `case` above.
   */
  def inherentCapabilities: Set[String] =
    Option.when(this != Os.Freestanding)(Capability.Os).toSet ++
      Option.when(this == Os.MacOS || this == Os.Linux || this == Os.Android)(Capability.Posix)

  /** Whether this is a **BSD**, which is a question about the libc rather than about the kernel and
   * is why it is not a capability (`Conditional.osDefined` turns it into the `bsd` symbol).
   *
   * **The library's OS gates were asking this and saying `macos`**, which was true while macOS was
   * the only BSD here and was never what any of them meant. Every one of the five is a place where
   * a BSD libc and glibc disagree and nothing about Darwin is involved: `__error` against
   * `__errno_location`, `ENOTEMPTY` at 66 against 39, `ENAMETOOLONG` at 63 against 36, and
   * `mode_t` at sixteen bits against thirty-two for both `mkdir` and `chmod`.
   *
   * **What that cost is a trap rather than untidiness, and only one of the five would have said
   * anything.** A FreeBSD added as a bare `Os` routes into the glibc branch at all five: the errno
   * accessor fails loudly at the link, and the other four are silent — a wrong `ENOTEMPTY` makes
   * `DirectoryNotEmpty` simply never match, and a `mode_t` at the wrong width appears to work on
   * most ABIs. So the symbol is corrected before the target exists rather than after, which is the
   * order that makes adding one a `case` here instead of an audit of five files.
   */
  def bsd: Boolean = this == Os.MacOS
}

/** What a call hands a C function whose parameter is a `va_list` (`reference/ffi.md § Variadic
 * functions`).
 *
 * C's `va_list` is a different type on every target and is passed differently on each, so the one
 * thing sysl has — the address of the walk's storage — reaches a foreign callee three ways. Each
 * was read off `clang -S -emit-llvm` for the triple rather than out of an ABI document:
 *
 *   - `Loaded` — the storage holds a single pointer-sized value and the call passes **that value**.
 *     Darwin arm64 (`va_list` is `char *`), Windows x64, and RISC-V.
 *   - `Address` — the storage is an array of one struct, which decays, so the call passes **the
 *     address of the storage itself**. x86-64 System V, hosted and freestanding alike.
 *   - `Copied` — the storage is a struct passed indirectly, so the call passes the address of a
 *     **fresh copy** of it. AAPCS64 everywhere but Darwin.
 *
 * The distinction is invisible in the emitted types — all three pass one `ptr` — which is exactly
 * why it has to be recorded: nothing downstream could recover it from the IR.
 *
 * **AAPCS32 needs no case of its own, and that was measured rather than assumed.** Its `va_list` is
 * `struct { void * }`, which clang declares as `[1 x i32]` because the aggregate rule reaches a
 * one-word struct — so it looks at first like a fourth answer. It is not: compiling a call both ways
 * for `thumbv8m.main-none-eabihf` gives the identical `ldr r1, [r1]`, because one word in a core
 * register is one word in a core register whichever type names it. `Loaded` already says exactly
 * that, and a fourth case would have been a distinction with no instruction behind it.
 */
enum VaListAbi {
  case Loaded, Address, Copied
}

object Target {

  val aarch64MacOS: Target =
    Target("aarch64-macos", "arm64-apple-macosx", Cpu.Aarch64, Os.MacOS, VaListAbi.Loaded, 8)

  val x86_64MacOS: Target =
    Target("x86_64-macos", "x86_64-apple-macosx", Cpu.X86_64, Os.MacOS, VaListAbi.Address, 24)

  val aarch64Linux: Target =
    Target("aarch64-linux", "aarch64-unknown-linux-gnu", Cpu.Aarch64, Os.Linux, VaListAbi.Copied, 32)

  val x86_64Linux: Target =
    Target("x86_64-linux", "x86_64-unknown-linux-gnu", Cpu.X86_64, Os.Linux, VaListAbi.Address, 24)

  val riscv64Linux: Target =
    Target("riscv64-linux", "riscv64-unknown-linux-gnu", Cpu.Riscv64, Os.Linux, VaListAbi.Loaded, 8)

  val x86_64Windows: Target =
    Target("x86_64-windows", "x86_64-pc-windows-msvc", Cpu.X86_64, Os.Windows, VaListAbi.Loaded, 8)

  /** A 64-bit Arm phone, which is the only Android worth a row: it is what every device sold since
   * 2019 runs and it is what the emulator runs on an Apple-silicon host, so one row serves both and
   * developing against the emulator costs no second build. `x86_64` would be for an Intel host or a
   * CI runner and `armeabi-v7a` for pre-2015 hardware; neither is a machine anybody here has.
   *
   * **Its ABI answers are `aarch64-linux`'s, and that is measured rather than inherited.** Compiling
   * a `va_start`/forward pair for this triple copies thirty-two bytes into a fresh `%struct.__va_list`
   * and passes its address, exactly as the GNU triple does — AAPCS64 is AAPCS64, and Bionic did not
   * vary it. What Bionic varies is everything above the ABI, which is why the `Os` is its own.
   *
   * **The API level belongs in the triple and a bare `aarch64-linux-android` is a trap.** With no
   * level, clang defines neither `__ANDROID_API__` nor `__ANDROID_MIN_SDK_VERSION__` — measured — so
   * the first Bionic header that guards a declaration on the level fails to compile, one step before
   * anything has been lowered. `24` is the floor chosen: the NDK's own range is 21 to 36
   * (`meta/platforms.json`), Vulkan starts at 24, and it is what a new Gradle project defaults to.
   * **The number here and the kit's `minSdk` are one fact stated twice** — a program compiled against
   * a higher level than the APK declares links and then fails to load on a device that has not got
   * the symbol.
   *
   * **The row needs no relocation-model field, which was the expected blocker and is not one.**
   * An archive linked into a `.so` must carry no **absolute** relocation in code, and sysl's objects
   * do not: every global it emits is `private`, so it is not preemptible and lowers to a PC-relative
   * pair against a local section, while its calls out to libc and to the standard module go through
   * a PLT. `CrossTargetBuildTests` measures this off the object, and its docstring carries the one
   * `ABS64` a correct program does have and why it is not this.
   */
  val aarch64Android: Target =
    Target("aarch64-android", "aarch64-linux-android24", Cpu.Aarch64, Os.Android, VaListAbi.Copied, 32)

  val aarch64Freestanding: Target =
    Target("aarch64-freestanding", "aarch64-none-elf", Cpu.Aarch64, Os.Freestanding, VaListAbi.Copied, 32)

  val x86_64Freestanding: Target =
    Target("x86_64-freestanding", "x86_64-unknown-none-elf", Cpu.X86_64, Os.Freestanding, VaListAbi.Address, 24)

  /** Bare-metal RISC-V is the one target with no floating-point registers to pass arguments in: the
   * hosted triple is built for the D extension and this one is not, which is clang's default for
   * each and so has to be sysl's.
   */
  val riscv64Freestanding: Target =
    Target("riscv64-freestanding", "riscv64-unknown-elf", Cpu.Riscv64, Os.Freestanding, VaListAbi.Loaded, 8,
      softFloat = true, noFpu = true)

  /** The RP2350's Arm personality: a Cortex-M33, which is Armv8-M Mainline and executes Thumb only.
   *
   * The triple carries the whole of what the ABI needs — `eabihf` selects the hard-float convention,
   * so arguments cross in VFP registers and `softFloat` stays false. `-mcpu=cortex-m33` refines
   * instruction selection to the exact core and changes nothing here; it is the sub-architecture
   * question this registry still leaves open.
   *
   * **The unit is named because the triple's default is neither stable across clangs nor right for
   * the part** — `fpv5-sp-d16` is the M33's, where clang defaults `thumbv8m.main` to a double-precision
   * `fpv5-d16` the silicon has not got. See `fpu`.
   */
  val thumbFreestanding: Target =
    Target("thumb-freestanding", "thumbv8m.main-none-eabihf", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, shortEnums = true, fpu = Some("fpv5-sp-d16"))

  /** The same core under the **other** float ABI, which is a sibling rather than a setting because
   * the two cannot link together: GNU ld refuses the mix outright, saying one object "uses VFP
   * register arguments" and the other "does not".
   *
   * It is here because the C project sysl joins picks the convention, and pico-sdk's default is this
   * one — so without it `@export` dictated the float ABI of every build it entered, and
   * `pico-scratch` had to set `PICO_HARD_FLOAT_ABI` to follow sysl rather than the other way round.
   *
   * **`softfp` is gcc's and pico-sdk's own spelling, and that is the whole argument for the name.**
   * Somebody handed that linker message searches their build system for the word in it. It is not
   * `soft`, which says something else: `-mfloat-abi=soft` means no FPU instructions at all, while
   * `softfp` means the unit is used and only the *calling convention* is in core registers — which is
   * exactly what `softFloat` records.
   *
   * **This is the row the unnamed unit broke, so it is the row `fpu` was added for.** Its triple says
   * only where arguments travel; whether there is a unit at all was clang's default, and one clang's
   * default is not another's. Naming `fpv5-sp-d16` is what makes "the unit is used" a fact about the
   * row rather than about the machine the compiler is running on.
   */
  val thumbFreestandingSoftfp: Target =
    Target("thumb-freestanding-softfp", "thumbv8m.main-none-eabi", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, softFloat = true, shortEnums = true, fpu = Some("fpv5-sp-d16"))

  /** The **third** row for that same Cortex-M33, and the one for a board whose FPU is simply not
   * there — an Armv8-M Mainline part built without one, or a build that gates it off, which is what
   * every MPS2 defconfig in Zephyr does.
   *
   * **`softfp` was not enough, and the difference is measurable rather than a nicety.**
   * `thumb-freestanding-softfp` carries the soft-float *ABI* and nothing more: it defines `__ARM_FP`
   * and compiles `a * b + 1.0f` to `vmla.f32`, because the unit is there to be used and only the
   * convention was asked about. So aiming that row at a
   * board with no unit produces an image that links, boots and takes a usage fault on the first
   * floating-point instruction — and a header that checks first refuses outright.
   *
   * **`soft` is gcc's own spelling for the distinction, which is the whole argument for the name.**
   * `-mfloat-abi=soft` means no FPU instructions at all where `-mfloat-abi=softfp` means the unit is
   * used and only the convention is in core registers. The registry's three v8m rows are that series
   * end to end: hard, `softfp`, `soft`.
   */
  val thumbFreestandingSoft: Target =
    Target("thumb-freestanding-soft", "thumbv8m.main-none-eabi", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, softFloat = true, shortEnums = true, noFpu = true)

  /** The **RP2040's** core: a Cortex-M0+, which is Armv6-M — Armv8-M's predecessor rather than a
   * variant of it, and a strictly smaller Thumb.
   *
   * **The name carries the sub-architecture where its two siblings above do not**, because this is a
   * different architecture and not a third float ABI. Armv6-M has no FPU to have a convention about,
   * so `softFloat` here is not the choice it is on the M33 — it is the only thing the core can do.
   *
   * What differs is the instruction set: no Thumb-2 to speak of, no hardware divide, and no
   * unaligned load or store. LLVM answers each of those with a libcall where it would emit an
   * instruction on the M33, and they come from the toolchain's own runtime exactly as
   * `__aeabi_ldivmod` already does for a `long` on any 32-bit board.
   *
   * **The one that does not come from the toolchain is atomics.** Armv6-M has no `ldrex`/`strex`, so
   * LLVM cannot lower an `atomicrmw` inline and calls `__atomic_fetch_add_4` instead — a symbol
   * nothing in a freestanding link defines. That reaches only `&sync T`, since `Codegen` emits the
   * atomic retain pair only for a program that has one, so an ordinary program on this target needs
   * nothing and links as it stands.
   *
   * A program that *does* share across the RP2040's **two** cores needs it, and the answer is the
   * board's rather than the language's. Disabling interrupts is the obvious implementation and is
   * the wrong one: `PRIMASK` is per-core, so it buys atomicity against this core's interrupts and
   * nothing at all against the other core — and a lost refcount update is a premature free, which
   * surfaces nowhere near the mistake. What the chip actually has is a hardware spinlock, which is
   * a thing about the board and must not be something the compiler knows. The package carries it.
   */
  val thumbv6mFreestanding: Target =
    Target("thumbv6m-freestanding", "thumbv6m-none-eabi", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, softFloat = true, shortEnums = true, noFpu = true)

  /** Armv7-M — the Cortex-M3, which is the core Zephyr's own documentation reaches for first
   * (`qemu_cortex_m3`, `mps2/an385`) and which the registry could not describe at all.
   *
   * **Armv6-M code runs on it and the headers do not survive the mismatch**, which is why aiming the
   * row below at this core is not an answer. A real project reads its own configuration rather than
   * the triple: `CONFIG_CPU_CORTEX_M3` says Armv7-M, so Zephyr's inline assembly uses `BASEPRI`,
   * while CMSIS reads `__ARM_ARCH_6M__` out of a v6-M triple and supplies the intrinsic set that has
   * no `__get_BASEPRI`. Neither side is wrong; they were told about two different machines.
   *
   * **It is the one new Thumb row that needs no flag**, because Armv7-M base has no floating-point
   * unit in the architecture and the triple says so — `thumbv7m-none-eabi` defines no `__ARM_FP` and
   * compiles `a * b` to `__aeabi_fmul` unasked. `noFpu` is set anyway, and is the fact rather than
   * the flag: it is true of the machine, and `machineFlags` passing `-mfpu=none` here changes
   * nothing, which was checked rather than assumed.
   *
   * Everything Armv6-M lacks, this core has: Thumb-2, a hardware divider, unaligned access, and
   * `ldrex`/`strex` — so an `atomicrmw` lowers inline and `&sync T` needs nothing from the board.
   */
  val thumbv7mFreestanding: Target =
    Target("thumbv7m-freestanding", "thumbv7m-none-eabi", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, softFloat = true, shortEnums = true, noFpu = true)

  /** The STM32s: Armv7E-M, which is where most of ST's parts land and which covers **two** boards
   * with different silicon — a Cortex-M4F (STM32G491RE) and a Cortex-M7 (STM32H753ZI).
   *
   * **One row serves both, and that is a measurement rather than an assumption.** The M7 has a
   * double-precision FPU and the M4F a single-precision one, so the two disagree about what they can
   * compute:
   *
   * {{{
   * cortex-m7:  +fp-armv8d16                 FPv5-D16, double
   * cortex-m4:  -fp-armv8d16 +vfp4d16sp      FPv4-SP-D16, single only
   * }}}
   *
   * Yet under `eabihf` both pass a `double` in `d0`/`d1` — checked by compiling a call and reading
   * the assembly. What differs is whether double *arithmetic* is inline or a libcall, and that is a
   * question about instruction selection rather than about the convention. So `softFloat` stays
   * false and one row is the honest answer for the pair.
   *
   * **The bare triple behaves as the conservative subset**, emitting `vldr d0` like the M4F and no
   * double-precision arithmetic. That is correct on both boards and leaves the H7's hardware double
   * unit unused. Naming the exact core with `-mcpu` is what would recover it, and this registry has
   * no field for a CPU — the sub-architecture question `thumbFreestanding` records as open. These
   * two parts are where it stops being hypothetical, since they are one triple and different
   * silicon.
   *
   * **`fpu` writes that subset down rather than inheriting it.** `fpv4-sp-d16` is the M4F's unit and
   * is what the bare triple already selects — the assembly for a float multiply, a double multiply
   * and a double load is byte-identical with the flag and without it — so naming it changes nothing
   * except that the answer is now the row's instead of the toolchain's.
   *
   * **Atomics need nothing from a board here**, unlike the RP2040 above: Armv7E-M has
   * `ldrex`/`strex`, so an `atomicrmw` lowers inline and an object built from one carries no
   * relocation against `__atomic_*` at all. `&sync T` works as it stands.
   */
  val thumbv7emFreestanding: Target =
    Target("thumbv7em-freestanding", "thumbv7em-none-eabihf", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, shortEnums = true, fpu = Some("fpv4-sp-d16"))

  /** The same Armv7E-M with the unit **absent** rather than merely unused — an STM32 F4 on a board
   * that leaves the FPU off, and `mps2/an386`, whose Zephyr defconfig sets no `CONFIG_FPU` and
   * silently drops an application's `CONFIG_FPU=y` because the SoC does not select `CPU_HAS_FPU`.
   *
   * The row beside it is hard-float and cannot serve here for two reasons at once — the convention is
   * wrong *and* the instructions are unavailable. The bare `thumbv7em-none-eabi` triple fixes only
   * the first: it still defines `__ARM_FP 0x6` and still selects `vmul.f32`, so what makes this row
   * different from that triple alone is `noFpu`.
   */
  val thumbv7emFreestandingSoft: Target =
    Target("thumbv7em-freestanding-soft", "thumbv7em-none-eabi", Cpu.Thumb, Os.Freestanding,
      VaListAbi.Loaded, 4, softFloat = true, shortEnums = true, noFpu = true)

  /** The RP2350's other personality: a Hazard3, which is RV32IMAC and has no F extension at all —
   * so, like bare-metal RISC-V at 64 bits, there are no floating registers to pass arguments in.
   */
  val riscv32Freestanding: Target =
    Target("riscv32-freestanding", "riscv32-unknown-elf", Cpu.Riscv32, Os.Freestanding,
      VaListAbi.Loaded, 4, softFloat = true, noFpu = true)

  /** WebAssembly, which is not a processor at all and is the first target here that is a **virtual**
   * machine — a stack machine with typed values, executed by a browser, by `wasmtime`, or by whatever
   * else embeds one.
   *
   * **`Freestanding` is the literal truth of `wasm32-unknown-unknown` rather than a convenience.**
   * The middle field of that triple is the vendor and the last is the operating system, and `unknown`
   * there means there is none: no libc, no loader, nothing that runs before an exported function is
   * called. Everything the `Os` answer decides is right for it in consequence — no `-l` is implied,
   * `hosted` and `posix` are both false, and `thread_local` gets `false`, which matters here because
   * the keyword is **accepted and silently meaningless**: measured, `@slot = internal thread_local
   * global i32 0` compiles to an ordinary data symbol.
   *
   * **`softFloat` is false and it is not the usual claim.** There are no registers on this machine to
   * pass anything in — but `f32` and `f64` are value types the instruction set has, a call takes and
   * returns them as themselves, and there is a `f64.add`. It reaches no decision here either way,
   * because the convention below never asks: an aggregate of floating members is not flattened on
   * this target any more than an aggregate of anything else is.
   *
   * **Atomics need nothing, unlike Armv6-M.** An `atomicrmw` for this triple lowers to a plain
   * read-modify-write with no undefined symbol, with and without `-matomics`, so a `&sync T` program
   * links as it stands. A wasm built for threads is a different machine — a shared memory and a real
   * `thread_local` — and would be a row of its own rather than a flag on this one.
   */
  val wasm32Freestanding: Target =
    Target("wasm32-freestanding", "wasm32-unknown-unknown", Cpu.Wasm32, Os.Freestanding,
      VaListAbi.Loaded, 4)

  /** WebAssembly against **WASI preview1** — the same machine as the row above with a libc under it.
   *
   * `wasm32-unknown-unknown` is the bare target: no libc, no convention for what the host supplies,
   * so a program with `requires { heap = true }` cannot link until somebody writes `malloc` by hand.
   * WASI is a standardised table of imports a module asks its host for by name, and **wasi-libc** is
   * a real libc built on it — musl above, those imports below. So this row is what makes an ordinary
   * sysl program run in a wasm runtime, and `sysl.fs`, `sysl.env` and `exit` have something real
   * underneath them.
   *
   * **The triple handed to clang is `wasm32-wasip1`**, which is the modern spelling; bare
   * `wasm32-wasi` is the deprecated alias, and the row keeps that name because it is what somebody
   * types and what the family is called.
   *
   * **preview1 rather than preview2, and the asymmetry is the whole reason.** preview1 is a flat
   * table of imports producing an ordinary core module — a target, and nothing in codegen changes.
   * preview2 is that rebuilt on the Component Model: WIT, worlds, resource handles, and an output
   * that is not a core module at all, which browsers do not run natively. That is a binding system
   * rather than a target. The standard preview1-to-preview2 adapter lifts a module into a component,
   * so this choice is reversible by tooling somebody else maintains.
   *
   * The machine is the row above's, so the ABI answers are too — one linear memory, four-byte
   * pointers, and a `va_list` that is a pointer to be loaded through.
   */
  val wasm32Wasi: Target =
    Target("wasm32-wasi", "wasm32-wasip1", Cpu.Wasm32, Os.Wasi, VaListAbi.Loaded, 4)

  /** CRAFT — a 16-bit load/store teaching machine with a 64 KiB virtual address space, and the first
   * row here that **no clang can build for**.
   *
   * Its back end lives out of tree, in the CRAFT repository, and is symlinked into an unmodified
   * `llvm-project` rather than forked into one — so what exists is an `llc` somebody built, not a
   * compiler driver anybody installs. There is no craft clang, no libc, no object format and **no
   * linker**: `craft as` reads one assembly file and resolves every label inside it. So sysl writes
   * the LLVM and stops, and `Toolchain` refuses this target rather than going looking for a driver
   * that is not there (`buildsWithClang`).
   *
   * **Everything a target usually records about a C call is unanswerable here, which is a fact
   * about the machine rather than a measurement nobody made.** `getting-started/cli.md § targets`
   * says an ABI answer comes from compiling the equivalent C and reading what clang did; there is
   * no C on the other side of any call on this machine, so there is nothing to agree with. The
   * fields below are therefore what the *LLVM back end* does, and `CAbi` says so where it is asked.
   *
   * `softFloat` and `noFpu` are both plainly true: there is no floating-point unit and no plan for
   * one, so every operation on a `float` or a `double` is a call to a runtime routine the back end
   * appends to the program that called it.
   *
   * The name carries no width because there is only ever going to be one CRAFT — see `Cpu.Craft`.
   */
  val craftFreestanding: Target =
    Target("craft-freestanding", "craft", Cpu.Craft, Os.Freestanding, VaListAbi.Loaded, 2,
      softFloat = true, noFpu = true)

  /** A target that is listed and cannot be built for. It is here rather than left out because the
   * limit is the compiler's and not the machine's, and a reader who names it deserves to be told
   * what is missing rather than told the name is unknown.
   *
   * **The limit is no longer its width.** 32-bit targets build; what i386 has not got is a C
   * calling convention measured against clang, which is the only way `getting-started/cli.md §
   * targets` allows one to be arrived at.
   */
  val x86Linux: Target =
    Target("x86-linux", "i386-unknown-linux-gnu", Cpu.X86, Os.Linux, VaListAbi.Address, 4)

  /** Every target the compiler knows, in the order `sysl targets` lists them. */
  val all: List[Target] =
    List(
      aarch64MacOS,
      x86_64MacOS,
      aarch64Linux,
      x86_64Linux,
      riscv64Linux,
      x86_64Windows,
      aarch64Android,
      aarch64Freestanding,
      x86_64Freestanding,
      riscv64Freestanding,
      thumbFreestanding,
      thumbFreestandingSoftfp,
      thumbFreestandingSoft,
      thumbv6mFreestanding,
      thumbv7mFreestanding,
      thumbv7emFreestanding,
      thumbv7emFreestandingSoft,
      riscv32Freestanding,
      wasm32Freestanding,
      wasm32Wasi,
      craftFreestanding,
      x86Linux,
    )

  private val byName = all.map(t => t.name -> t).toMap

  /** The target of a given name, or a complaint naming the ones there are. A target the compiler
   * knows and cannot lower for is refused here too, so no caller has to remember to ask.
   */
  def named(name: String): Either[String, Target] =
    byName.get(name) match
      case Some(t) if t.supported => Right(t)
      case Some(t) =>
        Left(s"sysl knows '$name' and cannot build for it: ${t.unsupported.get}")
      case None =>
        Left(s"unknown target '$name' — sysl knows ${all.map(_.name).mkString(", ")}")

  /** The registry name for a machine described the way a runtime describes itself, or `""` for one
   * the registry has no entry for.
   *
   * Each platform the compiler runs on spells the same machine differently — a JVM says `amd64`
   * where Node says `x64` and where Scala Native says `x86_64` — so the **asking** is per-platform
   * and the **answering** is here, once, where it can be tested without a machine of each kind to
   * run on.
   */
  def hostName(arch: String, os: String): String = {
    val cpu = arch.toLowerCase match
      case "aarch64" | "arm64"           => "aarch64"
      case "x86_64" | "amd64" | "x64"    => "x86_64"
      case "riscv64"                     => "riscv64"
      case _                             => ""
    val system = os.toLowerCase match
      case s if s.contains("mac") || s.contains("darwin") => "macos"
      case s if s.contains("linux")                       => "linux"
      case s if s.contains("win")                         => "windows"
      case _                                              => ""

    if cpu.isEmpty || system.isEmpty then "" else s"$cpu-$system"
  }

  /** This machine, when the registry knows it. The compiler asks its own platform what it is
   * running on — that is a question about the edge and `hostMachine` answers it there, per
   * platform — and the pair is turned into a target here.
   */
  val host: Option[Target] = byName.get(hostName(hostMachine._1, hostMachine._2)).filter(_.supported)

  /** This machine in the words its own runtime used, which is what `sysl targets` shows and what a
   * report about a machine sysl does not know has to carry: the mapping above can only be extended
   * by somebody who knows what the unrecognized half actually said.
   */
  def hostMachineShown: String = {
    val (arch, os) = hostMachine

    if arch.isEmpty && os.isEmpty then "not reported" else s"$arch / $os"
  }

  /** What the pure compiler API assumes when a caller names no target and the host is not one it
   * knows. The driver never reaches this — it reports the missing target and stops — so this is
   * the assumption a test or an embedding makes, and the machine sysl is developed on is the
   * honest one to make.
   */
  val default: Target = host.getOrElse(aarch64MacOS)
}
