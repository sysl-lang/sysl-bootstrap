package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@export` — a definition C can call, and the C header describing it (`reference/ffi.md §
 * @export`).
 *
 * **The assertions are on the emitted IR and on the generated header rather than on running
 * anything**, for `CallingConventionTests`' reason: what this feature produces is an artifact for a
 * *different* toolchain to consume, and there is no C project in this suite to consume it. What can
 * be checked here is the whole of what sysl decides — which symbol the definition is emitted under,
 * that the mangling is gone, that a build with no entry point keeps the exports alive, and every
 * refusal.
 */
class ExportTests extends AnyFreeSpec with CodegenSupport with TestFrameworkSupport {

  /** The exports of a compilation, which is the list a header is written from. */
  private def exportsOf(src: String): List[TFunc] =
    Compiler.compiled(List(Source("<input>", src))) match
      case Right(c) => c.exports
      case Left(e)  => fail(e)

  /** The header a module's exports produce. */
  private def headerFor(src: String): String = CHeader.render(exportsOf(src), "demo")

  "the symbol" - {

    "the exported symbol is the bare name, with no module path" in {
      ir("module demo\n\n@export\nadd(a: i32, b: i32) -> i32 = a + b\n") should
        include("define i32 @add(")
    }

    "and the symbol it names, where it names one" in {
      ir("module demo\n\n@export(\"mylib_add\")\nadd(a: i32, b: i32) -> i32 = a + b\n") should
        include("define i32 @mylib_add(")
    }

    /** **The symbol is a thunk in front of the definition rather than the definition renamed**, and
      * that is 0137: an export has to be lowered the way the machine's C convention says, which is
      * not what sysl does with its own definitions. The definition therefore keeps its mangled key
      * and the exported symbol is a function of its own that calls it (`ExportThunk`).
      *
      * A rename is what this was until then, and for a signature of scalars the two are
      * indistinguishable — which is why it survived. The difference shows the moment an aggregate is
      * in the signature, and `CExportAbiTests` is where that is run rather than read.
      */
    "and it is a thunk, so the definition is still there under its own key" in {
      val out = ir("module demo\n\n@export(\"mylib_add\")\nadd(a: i32, b: i32) -> i32 = a + b\n")

      out should include("define i32 @mylib_add(")
      out should include("define i32 @demo$add(")
      out should include("call i32 @demo$add(")
    }

    /** **The root module is where the thunk and the definition would claim one symbol**, and it is
      * the case the split above has to be careful about. A key is normally `module$name`, which no
      * exported symbol can be — a C identifier holds no `$` — but a function in the root module has
      * no qualification, so `@export` under its own name asks for exactly the name the definition
      * would take. What that produced was two `define`s of one symbol, which clang reports as an
      * invalid redefinition of a function nobody wrote twice.
      *
      * The **definition** is what moves, not the thunk: the exported symbol is what somebody outside
      * has written down, and a mangled key is the compiler's own business.
      */
    "and in the root module the definition moves aside, so the two do not collide" in {
      val out = ir("@export\nproduct(a: f32, b: f32) -> f32 = a * b\n\nprint(1)\n")

      out should include("define float @product(")
      out should include("define float @product$$sysl(")
      out should include("call float @product$$sysl(")
    }

    // The export is what a real C API needs, since its symbols carry a prefix the sysl side gets
    // from its module path — so the function stays `add` to everything inside sysl, and a sysl
    // caller reaches the definition rather than paying for a conversion at a call that never leaves
    // sysl.
    "a sysl caller reaches the definition, not the thunk" in {
      val out = ir("module demo\n\n@export(\"mylib_add\")\nadd(a: i32, b: i32) -> i32 = a + b\n\n" +
        "print(demo.add(1, 2))\n")

      out should include("call i32 @demo$add(")
    }

    "an unmangled export is not 'internal', so a linker outside can resolve it" in {
      ir("module demo\n\n@export\nadd(a: i32, b: i32) -> i32 = a + b\n") should
        include("define i32 @add(")
    }
  }

  "an export is a root for reachability, exactly as an interrupt handler is" - {

    // The load-bearing case: nothing in the program calls it, so a walk from what the program *runs*
    // cannot reach it, and a build with no entry point has no such walk to start.
    "a function nothing calls survives because it is exported" in {
      ir("module demo\n\n@export\nunused(a: i32) -> i32 = a + 1\n\nprint(1)\n") should
        include("define i32 @unused(")
    }

    "and so does what it calls" in {
      val out = ir("module demo\n\nhelper(a: i32) -> i32 = a * 2\n\n@export\n" +
        "doubled(a: i32) -> i32 = helper(a)\n\nprint(1)\n")

      out should include("define i32 @doubled(")
      out should include("@demo$helper(")
    }

    // A test build replaces the roots with the tests, so an export — which nothing in the program
    // names, that being the point of one — was reachable from nothing and went. It goes *quietly*,
    // unlike a destructor: there is no generated call site to be left dangling, so the suite links
    // and the package's own C is what finds out, which is the whole surface a `build-c` package has.
    "and a test build keeps it, though no test names it" in {
      testIr("module demo\n\n@export\nunused(a: i32) -> i32 = a + 1\n\n" +
        "@test(\"something else\")\nother() = assert_eq(1 + 1, 2)\n") should
        include("define i32 @unused(")
    }
  }

  "what C cannot spell is refused" - {

    "a slice parameter" in {
      val e = err("module demo\n\n@export\nsum(xs: []i32) -> i32 = 0\n")

      e should include("which C has no way to spell")
      e should include("a pointer and a 'usize'")
    }

    "a string parameter" in {
      err("module demo\n\n@export\nlen_of(s: string) -> i32 = 0\n") should
        include("which C has no way to spell")
    }

    /** A struct of scalars **is** a C declaration, so it is not refused and used to be (0137). What
      * an aggregate needed was never a refusal — it was for somebody to apply the convention, which
      * is what the exported entry now goes through (`ExportThunk`). `CExportAbiTests` is where the
      * bytes are checked; here it is only that the rule lets it past.
      */
    "a struct of scalars is not refused, since C declares one" in {
      ir("module demo\n\nstruct P\n    x: i32\n    y: i32\n@export\nf(p: P) -> i32 = p.x\n") should
        include("define i32 @f(")
    }

    "nor one returned" in {
      ir("module demo\n\nstruct P\n    x: i32\n@export\nf() -> P = P(1)\n") should
        include("@f(")
    }

    /** **But an aggregate is asked about its fields**, which is what keeps this apart from the rule
      * above it: the struct is a shape C has, and a `&T` inside it is a counted box that C would be
      * handed the refcount of. The refusal names the *field*, because the declaration the reader is
      * looking at does not mention it.
      */
    "a struct holding something C has no declaration for is refused, by field" in {
      val e = err("module demo\n\nstruct Node\n    x: i32\n\nstruct P\n    x: i32\n    n: &Node\n" +
        "@export\nf(p: P) -> i32 = p.x\n")

      e should include("'n' of demo.P is &demo.Node")
      e should include("Hand out a raw pointer")
    }

    "and so is an array of them" in {
      err("module demo\n\n@export\nf(xs: [3]string) -> i32 = 0\n") should
        include("its element is string")
    }

    /** A data enum is a tag beside a union sysl laid out, which is not the shape a C union has —
      * so it stays refused where a *simple* one, which is its underlying integer and nothing else,
      * does not.
      */
    "a data enum is refused, and a simple one is not" in {
      err("module demo\n\nenum E\n    A(n: i32)\n    B\n\n@export\nf(e: E) -> i32 = 0\n") should
        include("A data enum is a tag beside a union")

      ir("module demo\n\nenum Colour\n    Red\n    Green\n\n@export\nf(c: Colour) -> i32 = 0\n") should
        include("@f(")
    }

    // A pointer to a struct is fine, and it is the check that the rule is about the *shape* rather
    // than about the type being one the module declared, which a rule keyed on "is it named" would
    // have got wrong.
    "and a pointer to one is not refused either" in {
      ir("module demo\n\nstruct P\n    x: i32\n@export\nf(p: *P) -> i32 = p.x\n") should
        include("define i32 @f(")
    }

    /** A pointer to a **trait** is not one word: it is the value beside its method table, so it is a
      * pair, and it reached here as an ordinary pointer until 0137 — accepted, and lowered as
      * something no C header could declare.
      */
    "but a pointer to a trait is, since it is two words rather than one" in {
      val src = "module demo\n\ntrait Show\n    show(self) -> i32\n\n@export\nf(p: *Show) -> i32 = 0\n"

      err(src) should include("which C has no way to spell")
    }
  }

  "what a definition cannot be and stay C-callable" - {

    "generic" in {
      err("module demo\n\n@export\nid[T](x: T) -> T = x\n") should
        include("cannot be generic")
    }

    // Not an export rule at all: a member may carry only the three annotations that are about a
    // **parameter** (card `0313`), and `@export` is about a symbol. Pinned because it decides the
    // shape a binding is written in: the boundary layer is free functions, and a method is reached
    // by exporting one that takes the receiver. What the reader is told is `memberAttrs`' sentence,
    // and `MemberAttrErrorTests` is where that is pinned.
    "a member, which the grammar refuses before any rule here is reached" in {
      val src = "module demo\n\nstruct P\n    x: i32\n    @export\n    get(self) -> i32 = self.x\n"

      err(src) should include("the only annotations a member may carry are the ones about a parameter")
    }

    "private" in {
      err("module demo\n\n@export\nprivate add(a: i32, b: i32) -> i32 = a + b\n") should
        include("cannot make both claims")
    }

    "variadic" in {
      err("module demo\n\n@export\nf(n: i32, ...) -> i32 = n\n") should
        include("take a 'va_list' parameter instead")
    }

    "'@ghost', which is erased before there is a symbol at all" in {
      err("module demo\n\n@export\n@ghost\nf(a: i32) -> bool = a > 0\n") should
        include("erased before codegen")
    }

    "a symbol C could not name" in {
      err("module demo\n\n@export(\"my lib\")\nf(a: i32) -> i32 = a\n") should
        include("is not a name C can call")
    }

    "and a backticked name exported under itself, which is the same rule from the other end" in {
      err("module demo\n\n@export\n`add two`(a: i32) -> i32 = a + 2\n") should
        include("name the symbol instead")
    }

    "two definitions claiming one symbol" in {
      val src = "module demo\n\n@export(\"f\")\na(x: i32) -> i32 = x\n\n@export(\"f\")\nb(x: i32) -> i32 = x\n"

      err(src) should include("one symbol is one definition")
    }

    /** **But not one this build is about to discard.** The check reads the tree that is going to be
      * emitted, which is the one `Tests.strip` has been over — so an `@export` in a `@tests` file is
      * not a definition in an ordinary build, because it is not in an ordinary build at all.
      *
      * The case this was found in is a *package* rather than a program. A binding whose C calls back
      * into the application — a FreeRTOS kernel wants `vAssertCalled` and the idle task's storage from
      * whoever links it — has to define those symbols in order to test itself, and its `tests.sysl` is
      * the only place they belong. Counting them made every *consumer* of the package refuse, naming
      * the package's own test file as the other definition, and only one of the two was ever emitted.
      *
      * Both halves are asserted together on purpose. A case pinning only the first would go on passing
      * against a check that had been deleted outright.
      */
    "but a @tests file's export is not a second definition, while two real ones still are" in {
      val test =
        """module demo
          |
          |@tests
          |
          |@export("f")
          |theirs(x: i32) -> i32 = x
          |
          |@test
          |it_runs() = assert(1 == 1, "one")
          |""".stripMargin

      val program = "module demo\n\n@export(\"f\")\nmine(x: i32) -> i32 = x\n"

      Compiler.compiled(List(Source("tests.sysl", test), Source("demo.sysl", program))) match
        case Left(e)  => fail(s"a discarded export was counted as a definition: $e")
        case Right(c) => c.ir should include("define i32 @f(")

      // The same two exports with the `@tests` header gone are two definitions, and are refused.
      Compiler.compiled(List(Source("tests.sysl", test.replace("@tests\n\n", "")),
                             Source("demo.sysl", program))) match
        case Left(e)  => e should include("one symbol is one definition")
        case Right(_) => fail("two real exports of one symbol were accepted")
    }
  }

  /** A module compiled the way `build-c` compiles one: **no entry point of its own**, because the
   * artifact is linked into a C project that supplies the `main`. That is the whole of what
   * `Main.cLibrary` decides, and it is the shape every assertion about module storage is about.
   */
  private def archive(src: String, target: Target): Either[String, String] =
    Compiler.compiledWith(List(Source("<input>", src)), Nil, target, entryPoint = false).map(_.ir)

  /** The IR of such an archive, which must compile. */
  private def archiveIr(src: String, target: Target = Target.default): String =
    archive(src, target) match
      case Right(out) => out
      case Left(e)    => fail(e)

  /** Why such an archive was refused. */
  private def archiveErr(src: String, target: Target): String =
    archive(src, target) match
      case Right(out) => fail(s"expected an error, got:\n$out")
      case Left(e)    => e

  /** A freestanding target, which is the one case left that has nowhere to fill module storage. */
  private def bare: Target = Target.named("thumbv7em-freestanding").getOrElse(cancel("no such target"))

  "module storage" - {

    /** **A hosted archive fills its storage from a constructor** (card `0263`).
      *
      * The ruling this replaces was that a C project supplies its own `main`, so nothing runs to
      * fill storage a computed initializer would have written — and every build was refused on it,
      * whatever the target and whether or not it had an entry point of its own. What answers for it
      * now is `@llvm.global_ctors`, which LLVM lowers to `.init_array` on ELF, `__mod_init_func` on
      * Mach-O and `.CRT$XCU` on COFF: one spelling here, three where it matters.
      *
      * The workaround the refusal forced is what makes this worth pinning: a module reached through
      * an archive could hold no `Buf`, no closure and no counted value at module scope, so
      * `sysl-lang/skitter` stored four bare `int`s in place of a callback and `syslui` turned a
      * write counter into a bare `var`.
      */
    "a computed 'val' an export reaches is filled by a constructor on a hosted target" in {
      val src = "module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
        "@export\nbegin() -> i32 = start\n"
      val out = archiveIr(src)

      out should include("define internal void @sysl.module_init()")
      out should include(
        "@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] " +
          "[{ i32, ptr, ptr } { i32 65535, ptr @sysl.module_init, ptr null }]")
      // The storage really is filled there, rather than the constructor being an empty gesture.
      out should include("call i32 @demo$counter()")
    }

    /** **The constructor is what `@main` would have been**, so a build that has an entry point
      * still lays its `val`s down there and gets no second filling — which would take a second
      * count of every reference among them.
      */
    "while a build with an entry point of its own gets no constructor" in {
      val src = "module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
        "@export\nbegin() -> i32 = start\n"
      val out = ir(src)

      out should not include "@llvm.global_ctors"
      out should not include "@sysl.module_init"
    }

    /** **And a freestanding archive is still refused, because nothing there walks the list.** Bare
      * metal has no loader: `.init_array` runs only if the image's own start-up calls
      * `__libc_init_array`, which newlib's does and a hand-written reset vector may not — so a
      * constructor emitted there is a function nobody calls and the storage reads whatever the
      * image left.
      */
    "but a freestanding one is refused, and the diagnostic names the target" in {
      val src = "module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
        "@export\nbegin() -> i32 = start\n"
      val out = archiveErr(src, bare)

      out should include("module storage an initializer fills")
      out should include("thumbv7em-freestanding")
    }

    "reached transitively, which is what the walk is for" in {
      val src = "module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
        "inner() -> i32 = start\n\n@export\nbegin() -> i32 = inner()\n"

      archiveErr(src, bare) should include("module storage an initializer fills")
    }

    /** A module with nothing computed at module scope gets no constructor at all — an empty one
      * would be a symbol on every archive's pre-`main` list doing nothing.
      */
    "an archive with nothing computed carries no constructor" in {
      archiveIr("module demo\n\n@export\nbegin() -> i32 = 7\n") should not include "@llvm.global_ctors"
    }

    /** **A `.syslib` gets no constructor either, and that is the third answer rather than the
      * archive's read loosely** (card `0263`).
      *
      * A library artifact has no entry point, so `!entryPoint` would have caught it — and it must
      * not, because the thing that links a `.syslib` is a sysl **program**, which has one.
      * `Compiler.compileLibrary` already answers for this by leaving every function that reaches a
      * computed `val` out of the artifact and compiling it in the program instead, where the
      * initialization actually happens. A constructor here would fill a copy of the storage nothing
      * reads, and run the initializer a second time to do it.
      */
    "and a library artifact gets none, because the program that links it has an entry point" in {
      val src = SyslParser.parse(Source("lib.sysl",
        "module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
          "@export\nbegin() -> i32 = start\n", List("demo"))) match
        case Right(p) => p
        case Left(e)  => fail(e)

      Compiler.compileLibrary(List(src)) match
        case Right((out, _)) =>
          out should not include "@llvm.global_ctors"
          out should not include "@sysl.module_init"
        case Left(e) => fail(e)
    }

    // The other half of the ruling, and the reason it is not simply "no module storage": a constant
    // initializer is laid straight into the object file and nothing runs to fill it, which is the
    // rule C already has for static storage.
    "a 'val' whose initializer is constant data is fine" in {
      val src = "module demo\n\nval start: i32 = 7\n\n@export\nbegin() -> i32 = start\n"

      ir(src) should include("define i32 @begin(")
    }

    "and a 'const', which has no storage at all" in {
      val src = "module demo\n\nconst start: i32 = 7\n\n@export\nbegin() -> i32 = start\n"

      ir(src) should include("define i32 @begin(")
    }
  }

  "the generated header" - {

    "declares each export once, in a guard, with the C fixed-width names" in {
      val h = headerFor("module demo\n\n@export\nadd(a: i32, b: u8) -> i64 = 0i64\n")

      h should include("#ifndef SYSL_DEMO_H")
      h should include("#include <stdint.h>")
      h should include("int64_t add(int32_t a, uint8_t b);")
      h should include("#endif /* SYSL_DEMO_H */")
    }

    "spells a function taking nothing as C does, which is 'void' and not an empty list" in {
      headerFor("module demo\n\n@export\ntick() -> i32 = 0\n") should include("int32_t tick(void);")
    }

    "spells a function yielding nothing as 'void'" in {
      headerFor("module demo\n\n@export\ntick(a: i32)\n    print(a)\n") should include("void tick(int32_t a);")
    }

    // `never` and `unit` both spell as `void`, so the annotation is the only thing carrying the
    // difference across — that control does not come back, rather than merely that no value does.
    "annotates a function that does not return, which a bare 'void' would not say" in {
      val h = headerFor("module demo\n\n@export\nspin() -> never =\n    loop\n        print(1)\n")

      h should include("SYSL_NORETURN void spin(void);")
      h should include("#define SYSL_NORETURN _Noreturn")
      h should include("[[noreturn]]")
    }

    "and the macro is absent from a header with nothing that diverges" in {
      headerFor("module demo\n\n@export\ntick(a: i32)\n    print(a)\n") should not include "SYSL_NORETURN"
    }

    "spells a pointer as a pointer" in {
      headerFor("module demo\n\n@export\nhead(p: *u8) -> *u8 = p\n") should
        include("uint8_t * head(uint8_t * p);")
    }

    "spells a 'bool' as C's, which is why <stdbool.h> is included" in {
      headerFor("module demo\n\n@export\npositive(a: i32) -> bool = a > 0\n") should
        include("bool positive(int32_t a);")
    }

    "spells the two float widths as C's own names" in {
      val h = headerFor("module demo\n\n@export\nf(a: f32) -> f64 = f64(a)\n")

      h should include("double f(float a);")
    }

    /** **And the two sixteen-bit ones as theirs, which they have and which is neither `float` nor
      * each other's.** A header is read by a C compiler that will lay the call out from what it
      * says, so spelling an `f16` parameter `double` produces a header that compiles on both sides
      * and passes eight bytes where the callee reads two. That is the one failure a generated
      * header exists to make impossible, and it is invisible in the sysl half of the program.
      */
    "spells the two sixteen-bit formats as C's own names, which are not each other's" in {
      headerFor("module demo\n\n@export\nf(a: f16) -> f16 = a\n") should
        include("_Float16 f(_Float16 a);")
      headerFor("module demo\n\n@export\nf(a: bf16) -> bf16 = a\n") should
        include("__bf16 f(__bf16 a);")
    }

    // A `char` is a Unicode scalar value and four bytes wide, so C's `char` would be wrong by a
    // factor of four — which is a silently corrupt call rather than a compile error.
    "spells a 'char' as the 32-bit integer it is, never as C's 'char'" in {
      headerFor("module demo\n\n@export\nf(c: char) -> i32 = 0\n") should include("uint32_t c")
    }

    "names them in symbol order rather than declaration order, so the file is stable" in {
      val h = headerFor("module demo\n\n@export\nzeta() -> i32 = 0\n\n@export\nalpha() -> i32 = 1\n")

      h.indexOf("alpha") should be < h.indexOf("zeta")
    }

    "and a module exporting nothing says so, rather than being an empty file" in {
      headerFor("module demo\n\nadd(a: i32) -> i32 = a\n\nprint(1)\n") should
        include("exports nothing")
    }

    "wraps the declarations for C++, which is what a C header is expected to do" in {
      val h = headerFor("module demo\n\n@export\nf() -> i32 = 0\n")

      h should include("extern \"C\" {")
    }

    /** An aggregate is a spelling **and a definition**: a prototype naming `Id` is useless to a
      * consumer that has not been told what an `Id` is, and a by-value member is exactly the case a
      * forward declaration does not serve. So the header defines each one before it is used (0137).
      */
    "defines each struct it names, before the prototype that names it" in {
      val h = headerFor("module demo\n\nstruct Id\n    index1: i32\n    generation: u16\n\n" +
        "@export\nbump(a: Id) -> Id = a\n")

      h should include("typedef struct {")
      h should include("int32_t index1;")
      h should include("uint16_t generation;")
      h.indexOf("typedef struct") should be < h.indexOf("bump(")
    }

    "defines a struct a struct holds, before the struct that holds it" in {
      val h = headerFor("module demo\n\nstruct Inner\n    x: i32\n\nstruct Outer\n    a: Inner\n\n" +
        "@export\nf(o: Outer) -> i32 = o.a.x\n")

      h.indexOf("demo_Inner;") should be < h.indexOf("demo_Outer;")
    }

    "defines one named twice only once" in {
      val h = headerFor("module demo\n\nstruct Id\n    x: i32\n\n@export\nf(a: Id, b: Id) -> i32 = a.x\n")

      h.sliding("} demo_Id;".length).count(_ == "} demo_Id;") shouldBe 1
    }

    // A field's brackets go after the name, so a member is not its type followed by its name the way
    // a parameter is — which is the one place the declarator syntax shows through.
    "spells an array field with its brackets after the name" in {
      headerFor("module demo\n\nstruct Buf\n    bytes: [4]u8\n\n@export\nf(b: Buf) -> i32 = 0\n") should
        include("uint8_t bytes[4];")
    }

    // C's own `enum` has an implementation-defined width, so naming the integer is what states the
    // same contract the rest of the header states.
    "spells a simple enum as the integer it is" in {
      headerFor("module demo\n\nenum Colour: u8\n    Red\n    Green\n\n@export\nf(c: Colour) -> i32 = 0\n") should
        include("int32_t f(uint8_t c);")
    }
  }

  /** **The name a struct carries in the header, which used to be derived and nothing else** (0142).
    *
    * A function's symbol was given `@export("mylib_add")` because a C library's names are its own
    * and sysl's module path is doing that job already. Once an aggregate crossed (0137) a header
    * began carrying *type* names too, and those had no such control: the name is the mangled
    * instantiation with everything C would not accept replaced, which is `demo_Id` here and
    * `sh_sysl_box2d_c_Id` in a package. `@export` on the struct is that same rename read at the
    * other kind of declaration.
    *
    * **What the derived name was buying is uniqueness**, and a chosen one is a claim rather than a
    * derivation — so the collisions below are the price of the feature and are refused where two
    * exported symbols are.
    */
  "the name a struct carries in the header" - {

    "is the one the attribute chose" in {
      val h = headerFor("module demo\n\n@export(\"b2BodyId\")\nstruct Id\n    index1: i32\n\n" +
        "@export\nbump(a: Id) -> Id = a\n")

      h should include("} b2BodyId;")
      h should include("b2BodyId bump(b2BodyId a);")
      h should not include "demo_Id"
    }

    // The same reading `@export` has on a function, where the bare form publishes the declared name
    // rather than the mangled key — which is the whole of what a reader would expect it to mean.
    "and the bare form is the declared name, with the module path gone" in {
      val h = headerFor("module demo\n\n@export\nstruct Id\n    index1: i32\n\n" +
        "@export\nbump(a: Id) -> Id = a\n")

      h should include("} Id;")
      h should include("Id bump(Id a);")
    }

    "while a struct that names itself nothing keeps the derived name" in {
      headerFor("module demo\n\nstruct Id\n    index1: i32\n\n@export\nbump(a: Id) -> Id = a\n") should
        include("} demo_Id;")
    }

    // The name is used wherever the type is spelled, not only at the definition — a field, a
    // pointee and a function pointer's signature all go through `CHeader.cName`.
    "and it is what a field, a pointer and a callback spell too" in {
      val h = headerFor("module demo\n\n@export(\"b2Vec2\")\nstruct V\n    x: f32\n\n" +
        "struct Body\n    at: V\n\n@export\nshift(b: *Body, cb: *extern(V) -> unit) -> V = b.at\n")

      h should include("\tb2Vec2 at;")
      h should include("b2Vec2 shift(demo_Body * b, void (*cb)(b2Vec2));")
    }

    // A function pointer is a declarator, not a type followed by a name: the name goes inside the
    // `(*` … `)`, wherever the pointer stands. `void (*)(int32_t) f` is not C.
    "a function pointer puts the name inside its declarator, as a parameter, a field and a result" in {
      val h = headerFor("module demo\n\nstruct Sink\n    put: *extern(i32) -> unit\n    n: i32\n\n" +
        "@export\nemit(s: *Sink) = s.put(s.n)\n\n" +
        "@export\nhook(f: *extern(i32) -> unit) -> *extern(i32) -> unit = f\n")

      h should include("\tvoid (*put)(int32_t);")
      h should include("void (*hook(void (*f)(int32_t)))(int32_t);")
      h should not include "(*)"
    }

    // The layout pair and the name are three facts about one struct, so they compose — which is the
    // combination a mirrored C struct actually wants.
    "and it composes with the layout attributes, which describe the same declaration" in {
      val h = headerFor("module demo\n\n@packed\n@export(\"wire_hdr\")\nstruct H\n    a: u8\n    b: u32\n\n" +
        "@export\nf(h: H) -> u8 = h.a\n")

      h should include("} wire_hdr;")
      h should include("uint8_t f(wire_hdr h);")
    }

    // `@align` is the other half of that pair and is folded by a different line, so it is asserted
    // rather than assumed to follow from `@packed` working.
    "including the other one, which is folded separately" in {
      headerFor("module demo\n\n@align(16)\n@export(\"aligned_hdr\")\nstruct H\n    a: u32\n\n" +
        "@export\nf(h: H) -> u32 = h.a\n") should include("} aligned_hdr;")
    }

    // The rule that catches an annotation written twice is above the struct reading and so covers
    // it — two names for one type say nothing the one does, and worse could disagree.
    "and writing it twice is refused, as it is above a function" in {
      err("module demo\n\n@export(\"a\")\n@export(\"b\")\nstruct H\n    x: i32\n") should
        include("is written twice above one declaration")
    }

    /** **The chosen name reaches the header and nothing else.** The emitted aggregate keeps the
      * mangled name, which is what every other part of the compiler keys on, and C links nothing on
      * a type name — so this is a spelling for a reader rather than a fact anything depends on. It
      * is asserted because it is the cost the card accepted rather than fixed.
      */
    "and the emitted aggregate is untouched, since nothing links on a type name" in {
      ir("module demo\n\n@export(\"b2BodyId\")\nstruct Id\n    index1: i32\n\n" +
        "@export\nbump(a: Id) -> Id = a\n") should include("%struct.demo$Id")
    }

    /** **A private struct is refused, and not for the reason a private function is.** A `typedef`
      * has no linkage to contradict, so the function's argument does not reach — this was written
      * as an acceptance on the strength of that and the compiler said otherwise, which is the
      * finding. The visibility rule gets there first: a public declaration may not name a type less
      * visible than itself, an exported function is public, so a private struct is in no signature
      * a header carries and there is no name in one for it to take.
      */
    "a private struct is refused, because no export could name it in the first place" in {
      err("module demo\n\n@export(\"b2BodyId\")\nprivate struct Id\n    index1: i32\n") should
        include("No header can carry this type at all")
    }

    // The rule the one above defers to, asserted here so that the refusal and its reason cannot
    // drift apart: it is what makes a private struct unreachable from a header.
    "which is the rule that already stops a public export naming one" in {
      err("module demo\n\nprivate struct Id\n    index1: i32\n\n@export\nbump(a: Id) -> Id = a\n") should
        include("may not be more visible than the types it names")
    }

    "a name C could not declare is refused" in {
      err("module demo\n\n@export(\"2bad\")\nstruct Id\n    x: i32\n") should
        include("is not a name C can declare")
    }

    "and a backticked struct named after itself, which is the same rule from the other end" in {
      err("module demo\n\n@export\nstruct `an id`\n    x: i32\n") should
        include("name it instead")
    }

    // Every instantiation is a struct of its own, so one written name would be claimed by all of
    // them at once — the argument that refuses an exported generic function, one step shorter.
    "a generic struct is refused, because each instantiation would claim the one name" in {
      err("module demo\n\n@export(\"Box\")\nstruct Box[T]\n    v: T\n") should
        include("cannot be generic")
    }

    "two structs claiming one name are refused, naming both" in {
      val e = err("module demo\n\n@export(\"Handle\")\nstruct A\n    x: i32\n\n" +
        "@export(\"Handle\")\nstruct B\n    y: i32\n\n@export\nf(a: A, b: B) -> i32 = a.x\n")

      e should include("'Handle' is the C name of")
      e should include("'demo.A'")
      e should include("'demo.B'")
    }

    // The collision a reader would never look for: a chosen name landing on what another struct's
    // derived one already is. It is the same mistake and gets the same refusal.
    "and a chosen name landing on another struct's derived one is the same refusal" in {
      err("module demo\n\nstruct Id\n    x: i32\n\n@export(\"demo_Id\")\nstruct Other\n    y: i32\n\n" +
        "@export\nf(a: Id, b: Other) -> i32 = a.x\n") should include("'demo_Id' is the C name of")
    }

    /** **A type and a function are one namespace in C, and two in sysl.** At file scope a `typedef`
      * name and a function name are both ordinary identifiers, so `typedef struct { … } handle;`
      * beside `handle handle(…)` is one name declared twice — which nothing on the sysl side hints
      * at, since a struct and a function collide nowhere here.
      */
    "and a type claiming an exported function's symbol, which C has one namespace for" in {
      val e = err("module demo\n\n@export(\"handle\")\nstruct H\n    x: i32\n\n" +
        "@export(\"handle\")\nmake(n: i32) -> H = H(n)\n")

      e should include("the type 'demo.H'")
      e should include("the function 'demo.make'")
    }

    // And the same thing the other way round, where nobody chose anything: a function named after
    // what a struct's derived name happens to be.
    "including where the type's half of it was derived rather than chosen" in {
      err("module demo\n\nstruct Id\n    x: i32\n\n@export(\"demo_Id\")\nmake(n: i32) -> Id = Id(n)\n") should
        include("'demo_Id' is the C name of")
    }

    // Two functions claiming one symbol stay `duplicates`' case, whose sentence is about the linker
    // and two definitions — the better one there, and the reason this check needs a type in the group.
    "while two functions claiming one symbol keep the refusal that is about the linker" in {
      err("module demo\n\n@export(\"go\")\na() -> i32 = 1\n\n@export(\"go\")\nb() -> i32 = 2\n") should
        include("one symbol is one definition")
    }

    // Only what reaches the header claims a name, which is `CHeader.aggregates`' set rather than
    // every struct in the program: two types nobody exported cannot collide in a file neither is in.
    "while two agreeing names neither of which reaches the header are not a collision" in {
      headerFor("module demo\n\n@export(\"Handle\")\nstruct A\n    x: i32\n\n" +
        "@export(\"Handle\")\nstruct B\n    y: i32\n\n@export\nf(n: i32) -> i32 = n\n") should
        include("int32_t f(int32_t n);")
    }
  }

  /** `@export` on a `type` (`reference/ffi.md § Naming a type`): the header declares a `typedef`
    * and spells the type by that name wherever it appears, rather than dissolving it into its base.
    */
  "the name a type carries in the header" - {

    val slate =
      "module demo\n\n@export(\"slate_value\")\ntype Handle = u64\n\n" +
        "@export(\"slate_int\")\nint_of(n: i64) -> Handle = u64(n)\n"

    "is declared as a typedef of what it stands for, and the prototype uses it" in {
      val h = headerFor(slate)

      h should include("typedef uint64_t slate_value;\n")
      h should include("slate_value slate_int(int64_t n);")
      h.indexOf("typedef uint64_t slate_value;") should be < h.indexOf("slate_int(")
    }

    "and the bare form is the declared name, as a struct's is" in {
      val h = headerFor("module demo\n\n@export\ntype Handle = u64\n\n@export\nf(h: Handle) -> Handle = h\n")

      h should include("typedef uint64_t Handle;\n")
      h should include("Handle f(Handle h);")
    }

    // Exporting it changes what the header says and nothing a sysl caller sees: it is still
    // interchangeable with its base in both directions, with no conversion written.
    "and a sysl caller still mixes it with its base freely" in {
      ir("module demo\n\n@export(\"v\")\ntype Handle = u64\n\n@export\nf(h: Handle) -> u64 = h + 1\n\n" +
        "val a: u64 = f(41)\nval b: Handle = a\nprint(b)\n") should include("define")
    }

    "is what a struct field and a function pointer's parameter spell too" in {
      val h = headerFor("module demo\n\n@export(\"slate_value\")\ntype Handle = u64\n\n" +
        "@export(\"box\")\nstruct Box\n    v: Handle\n    cb: *extern(Handle) -> Handle\n\n" +
        "@export\nput(b: *Box, f: *extern(Handle, i32) -> unit)\n    f(b.v, 1)\n")

      h should include("\tslate_value v;")
      h should include("\tslate_value (*cb)(slate_value);")
      h should include("void put(box * b, void (*f)(slate_value, int32_t));")
      h.indexOf("typedef uint64_t slate_value;") should be < h.indexOf("typedef struct {")
    }

    // A pointer's `*` is part of what the name stands for, so a pointer to a named pointer is `h *`,
    // and a function pointer given a name takes no parentheses of its own where it is used.
    "a pointer and a function pointer are declared with their declarators inside the typedef" in {
      val h = headerFor("module demo\n\n@export(\"vm\")\nopaque struct Vm\n    n: int\n\n" +
        "@export(\"vm_ref\")\ntype VmRef = *Vm\n\n@export(\"on_event\")\ntype OnEvent = *extern(i32) -> i32\n\n" +
        "@export\nrun(v: VmRef, f: OnEvent, fs: *OnEvent) -> VmRef = v\n")

      h should include("typedef struct vm vm;\n")
      h should include("typedef vm * vm_ref;\n")
      h should include("typedef int32_t (*on_event)(int32_t);\n")
      h should include("vm_ref run(vm_ref v, on_event f, on_event * fs);")
      h.indexOf("typedef struct vm vm;") should be < h.indexOf("typedef vm * vm_ref;")
    }

    "while a type that is not exported is spelled as what it stands for" in {
      val h = headerFor("module demo\n\ntype Handle = u64\ntype Meters = new f64\ntype Slot = u8 within 0..<8\n\n" +
        "@export\nf(h: Handle, m: Meters, s: Slot) -> Handle = h\n")

      h should include("uint64_t f(uint64_t h, double m, uint8_t s);")
      h should not include "typedef uint64_t"
    }

    // A narrowed type is exported the same way: the header has no way to check the bound, and says
    // what the value is made of — which is all a C declaration ever says.
    "and a narrowed or derived type exports as its base, under its own name" in {
      val h = headerFor("module demo\n\n@export(\"slot\")\ntype Slot = u8 within 0..<8\n\n" +
        "@export(\"meters\")\ntype Meters = new f64\n\n@export\nf(s: Slot, m: Meters) -> Meters = m\n")

      h should include("typedef uint8_t slot;\n")
      h should include("typedef double meters;\n")
      h should include("meters f(slot s, meters m);")
    }

    "two aliases of one base are two typedefs, and a bare base is still the C name" in {
      val h = headerFor("module demo\n\n@export(\"value_a\")\ntype A = u64\n\n@export(\"value_b\")\ntype B = u64\n\n" +
        "@export\nf(a: A, b: B, n: u64) -> B = b\n")

      h should include("typedef uint64_t value_a;\n")
      h should include("typedef uint64_t value_b;\n")
      h should include("value_b f(value_a a, value_b b, uint64_t n);")
    }

    "an alias of an alias is declared after the one it names" in {
      val h = headerFor("module demo\n\n@export(\"base_t\")\ntype A = u32\n\n@export(\"top_t\")\ntype B = A\n\n" +
        "@export\nf(b: B) -> A = b\n")

      h should include("typedef uint32_t base_t;\n")
      h should include("typedef base_t top_t;\n")
      h should include("base_t f(top_t b);")
      h.indexOf("typedef uint32_t base_t;") should be < h.indexOf("typedef base_t top_t;")
    }

    "a type over a struct is refused, with the struct's own attribute named instead" in {
      val e = err("module demo\n\nstruct Point\n    x: i32\n\n@export(\"point_t\")\ntype P = Point\n")

      e should include("'demo.P' is exported to a C header as demo.Point, which C has no way to spell as a typedef")
      e should include("above the struct itself")
    }

    "and so are a slice and a counted reference, each with its own advice" in {
      err("module demo\n\n@export(\"bytes\")\ntype Bytes = []u8\n") should include("a pointer and a 'usize'")
      err("module demo\n\nstruct N\n    x: i32\n\n@export(\"nref\")\ntype R = &N\n") should
        include("Hand out a raw pointer")
    }

    "a type's C name is held to a function's rules" in {
      err("module demo\n\n@export(\"no-good\")\ntype H = u64\n") should include("'no-good' is not a name C can declare")
      err("module demo\n\n@export(\"h\")\nprivate type H = u64\n") should include("'demo.H' is private")
    }

    "and it claims its name against a function's symbol, which C has one namespace for" in {
      err("module demo\n\n@export(\"handle\")\ntype H = u64\n\n@export(\"handle\")\nmake(n: u64) -> H = n\n") should
        include("'handle' is the C name of the function 'demo.make' and the type 'demo.H'")
    }
  }

  /** **A test build is held to the same rules, and was held to none of them** (0140).
    *
    * `Compiler.compileTests` ran `Escape.check` and `TailCalls.check` and never `Exports.check`, so
    * every rule above was silent under `sysl test` — the loop a package's author actually runs. The
    * same source a `sysl build` refused compiled, linked and ran, reporting a pass.
    *
    * The tree it reads is `Tests.only` rather than `Tests.strip`, which is the whole of the
    * difference from an ordinary build: the question is about the symbol table *this* compilation
    * emits, and a test build is the one build where a `@test` file's `@export` is a definition.
    */
  "a test build is held to the export rules too" - {

    /** The helper reports a refusal by failing, so what is asserted is the diagnostic — which means
      * this case cannot use `testIr`.
      */
    def testErr(src: String): String =
      Compiler.compileTests(List(Source("<input>", src)), Nil) match {
        case Left(e)  => e
        case Right(_) => fail("a test build accepted an export every other build refuses")
      }

    "a private export, which is the two promises a definition cannot both make" in {
      testErr("module demo\n\n@export\nprivate add(a: i32) -> i32 = a\n\n" +
        "@test\nt() = assert(1 == 1, \"one\")\n") should include("cannot make both claims")
    }

    "a variadic one" in {
      testErr("module demo\n\n@export\nf(n: i32, ...) -> i32 = n\n\n" +
        "@test\nt() = assert(1 == 1, \"one\")\n") should include("take a 'va_list' parameter instead")
    }

    "a parameter C has no declaration for" in {
      testErr("module demo\n\n@export\nf(s: string) -> i32 = 0\n\n" +
        "@test\nt() = assert(1 == 1, \"one\")\n") should include("which C has no way to spell")
    }

    /** **A test build fills its own module storage, so this is one refusal it does NOT inherit**
      * (card `0263`).
      *
      * It used to, and the case was listed here for the reason every other one is: `sysl test` is
      * the loop a package author actually runs, and a rule it stays silent about is a rule reported
      * to nobody. What changed is that the rule stopped applying — `Codegen.genTestMain` lays the
      * computed `val`s down exactly as an ordinary entry point does, so a test binary reads filled
      * storage and always did. The refusal was over-broad rather than usefully early.
      *
      * The refusal that remains is a **freestanding archive**'s, which no test build can be: a test
      * runs on the machine that built it. `ExportTests`' own "module storage" section is where it
      * is asserted.
      */
    "while an export reaching computed module storage is fine, because a test build fills it" in {
      testIr("module demo\n\ncounter() -> i32 = 7\n\nval start: i32 = counter()\n\n" +
        "@export\nbegin() -> i32 = start\n\n@test\nt() = assert(1 == 1, \"one\")\n") should
        include("define i32 @begin(")
    }

    // And the export a test build legitimately has stays legitimate: a `@tests` file's `@export` is
    // a definition *here*, which is the whole reason the tree read is `Tests.only`.
    "while a @tests file's own export is fine, because this is the build that emits it" in {
      testIr("module demo\n\n@tests\n\n@export(\"probe\")\nprobe(a: i32) -> i32 = a\n\n" +
        "@test\nt() = assert(1 == 1, \"one\")\n") should include("define i32 @probe(")
    }
  }

  "the attribute itself" - {

    "a malformed argument is told what the form is" in {
      err("module demo\n\n@export(mylib_add)\nadd(a: i32) -> i32 = a\n") should
        include("names the C symbol as a string")
    }

    "and the unknown-annotation list mentions it, so it is discoverable" in {
      err("module demo\n\n@exprot\nadd(a: i32) -> i32 = a\n") should include("'@export'")
    }

    // `@export` marks two kinds of declaration and neither of them is a binding, so the refusal says
    // which two rather than leaving the grammar to complain about the word after it.
    "above a binding, which is neither of the two things it names" in {
      err("module demo\n\n@export(\"x\")\nvar count: i32 = 0\n") should
        include("names what C sees")
    }

    // The layout pair settles it: `@packed` cannot mark a function, so the pair together can only be
    // about a struct and the refusal need not offer the function reading at all.
    "and beside a layout attribute above one, where only the struct reading is left" in {
      err("module demo\n\n@packed\n@export(\"x\")\nvar count: i32 = 0\n") should
        include("together they mark a struct")
    }

    // A simple enum is spelled as its underlying integer, so there is no name in the header for the
    // attribute to be about — which is the mistake worth naming, since a simple enum does cross.
    "and above an enum, which is spelled as an integer and so has no name to choose" in {
      err("module demo\n\n@export(\"Colour\")\nenum Colour: u8\n    Red\n    Green\n") should
        include("no name in the header to choose")
    }

    // The mix that has always meant a function still does: `@export` composes with the function
    // attributes exactly as before, and the struct reading is reached only by the layout pair.
    "while an export beside a function attribute is still a function" in {
      ir("module demo\n\n@pure\n@export(\"mylib_add\")\nadd(a: i32, b: i32) -> i32 = a + b\n") should
        include("define i32 @mylib_add(")
    }
  }
}
