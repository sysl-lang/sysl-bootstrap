package sh.sysl

import io.github.edadma.cross_platform.*

/** `sysl build-c` and `sysl emit-header` — the archive and the header a C project is handed
 * (`reference/ffi.md § @export`).
 *
 * **The last test is the one that matters and the rest are its scaffolding.** Everything else in
 * this feature can be asserted by reading text sysl produced, which proves only that sysl agrees
 * with itself. What nothing else can answer is whether a **C compiler** accepts the header, whether
 * its linker resolves the symbol out of the archive, and whether the value that comes back is the
 * one the sysl function computed — so that test writes a C `main`, builds it with clang against both
 * artifacts, runs it, and reads the answer.
 *
 * It needs a clang that can link for this machine, which is the same thing every run-tier suite here
 * needs, so it is not a new dependency.
 */
class ExportCliTests extends LibraryCliSupport {

  /** A module whose whole purpose is to be called from outside, which is the shape a boundary layer
   * takes: free functions, scalars and pointers, no module storage that has to be computed.
   */
  private val boundary =
    """module mylib
      |
      |@export("mylib_add")
      |add(a: i32, b: i32) -> i32 = a + b
      |
      |@export("mylib_scale")
      |scale(a: i32, by: i32) -> i32 = a * by
      |
      |helper(n: i32) -> i32 = n + 1
      |""".stripMargin

  /** A module that reaches the standard library, which the one above never does.
   *
   * **Everything in `boundary` lowers to instructions**, so its archive stands alone whatever the
   * compilation did with the standard module — which is why a suite built entirely on it could be
   * green while every archive sysl wrote was unlinkable. One `print` is the difference: the object
   * then refers to `sysl$prints`, and whether that symbol is *in* the archive is the only question a
   * C project ever asks of this command.
   */
  private val talkative =
    """module mylib
      |
      |@export("mylib_greet")
      |greet(n: i32) =
      |    print("hello", n)
      |""".stripMargin

  private def built(text: String = boundary): (String, String) = {
    val root = rootOf("mylib", text)
    val out  = s"$root/libmylib.a"

    succeeds(Config(command = "build-c", file = root, output = Some(out)))
    (out, s"$out.h")
  }

  /** A **headerless** module that keeps state — which is what a boundary layer usually is, and which
    * silently exported nothing until card `0167` was fixed.
    *
    * `ModuleFiles.entryFile` reads a lone top-level `var` in a file with no `module` header as a
    * *body's local*, since that is what keeps a one-file `var n = 1` meaning what it always has in a
    * program. `build-c` emits no `main`, so there is no body — and the file was chosen anyway, its
    * functions became nested functions of a body nothing emits, and that renamed them into an
    * environment and dropped the `@export` on the way.
    *
    * **Both exports go, not just the one that touches the storage**, which is the part worth pinning:
    * a fix that only re-rooted the reaching function would still pass a one-export test.
    */
  private val stateful =
    """var counter: i32 = 0
      |
      |@export("bump_set")
      |bump_set(n: i32)
      |    counter = n
      |end bump_set
      |
      |@export("bump_get")
      |bump_get() -> i32 = counter
      |""".stripMargin

  "a header-less module that keeps state still exports, and exports all of it" in {
    val root = createTempDirectory("sysl-cli-state-")

    writeFile(s"$root/main.sysl", stateful)

    val out = s"$root/libstate.a"

    succeeds(Config(command = "build-c", file = root, output = Some(out)))

    val header = readFile(s"$out.h")

    withClue(header) {
      header should include("void bump_set(int32_t n);")
      header should include("int32_t bump_get(void);")
      header should not include "exports nothing"
    }
  }

  /** The same shape read the other way: the storage is what makes it interesting, so a build that
    * kept the exports and lost the `var` would pass the test above and be just as wrong.
    */
  "and the storage it keeps is the module's, reachable from both" in {
    val root = createTempDirectory("sysl-cli-state-")

    writeFile(s"$root/main.sysl", stateful)

    val out = s"$root/libstate.a"

    succeeds(Config(command = "build-c", file = root, output = Some(out)))

    // Both symbols are *defined* in the archive rather than merely declared in the header, which is
    // what the C project's linker will ask.
    val listed = exec(List("nm", out))

    withClue(listed.stdout) {
      listed.stdout should include("bump_set")
      listed.stdout should include("bump_get")
    }
  }

  "build-c writes an archive and a header beside it" in {
    val (archive, header) = built()

    exists(archive) shouldBe true
    exists(header) shouldBe true
  }

  "the header declares each export under the symbol it named" in {
    val (_, header) = built()
    val text        = readFile(header)

    text should include("int32_t mylib_add(int32_t a, int32_t b);")
    text should include("int32_t mylib_scale(int32_t a, int32_t by);")
  }

  "and does not declare what was not exported" in {
    readFile(built()._2) should not include "helper"
  }

  "--header puts it somewhere else" in {
    val root = rootOf("mylib", boundary)
    val out  = s"$root/libmylib.a"
    val hdr  = s"$root/include/mylib.h"

    succeeds(Config(command = "build-c", file = root, output = Some(out), header = Some(hdr)))
    exists(hdr) shouldBe true
  }

  "emit-header prints the same declarations without building anything" in {
    val root = rootOf("mylib", boundary)
    val out  = new java.io.ByteArrayOutputStream

    val status = Console.withOut(out)(sh.sysl.execute(Config(command = "emit-header", file = root,
      noStdLib = true)))

    status shouldBe 0
    out.toString should include("int32_t mylib_add(int32_t a, int32_t b);")
  }

  // A module with no entry point still has to compile, and this is the case that would have been
  // pruned to nothing if an export were not a reachability root.
  "a module with no statements and no 'main' builds, because the exports are the roots" in {
    val (archive, _) = built()

    exists(archive) shouldBe true
  }

  "a C program links against the archive and gets the answers sysl computed" in {
    val (archive, header) = built()
    val dir               = createTempDirectory("sysl-c-caller-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(void) {
         |    printf("%d %d\\n", mylib_add(2, 3), mylib_scale(4, 5));
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "5 20"
  }

  /** A project whose manifest asks for thin LTO — slate's own shape, which is how an archive of
    * LLVM bitcode reached a C project that only GNU ld was ever going to link.
    */
  private def builtUnderLto(): (String, String) = {
    val root = rootOf("mylib", boundary)
    val out  = s"$root/libmylib.a"

    writeFile(s"$root/${PackageConfig.FileName}",
      """package { name = "mylib", version = "0.1.0" }
        |lto = "thin"
        |""".stripMargin)

    succeeds(Config(command = "build-c", file = root, output = Some(out)))
    (out, s"$out.h")
  }

  /** Whether `bytes` begin as a native object this machine's linkers read without LLVM's plugin:
    * ELF, or 64-bit Mach-O in either byte order. Bitcode begins `BC 0xC0DE`, or `0x0B17C0DE` inside
    * Apple's wrapper, and is neither.
    */
  private def isNativeObject(bytes: Array[Byte]): Boolean = {
    val head = bytes.take(4).map(_ & 0xff).toList

    head == List(0x7f, 0x45, 0x4c, 0x46) || head == List(0xcf, 0xfa, 0xed, 0xfe) ||
      head == List(0xfe, 0xed, 0xfa, 0xcf)
  }

  // The manifest's `lto` is how the project's OWN link is done, and `build-c` does no link: the
  // archive is for "an existing C project", whose linker may be GNU ld, which refuses bitcode as
  // "file format not recognized". So the member is an object whatever the manifest says.
  "a project whose manifest asks for LTO still archives a native object, not bitcode" in {
    val ar = Toolchain.findAr(None) match
      case Right(found) => found
      case Left(why)    => cancel(why)

    val (archive, _) = builtUnderLto()
    val dir          = createTempDirectory("sysl-c-members-")
    val extract      = exec(Seq(ar, "x", s"--output=$dir", archive, LibraryArtifact.codeMember))

    withClue(extract.stderr)(extract.exitCode shouldBe 0)

    val member = s"$dir/${LibraryArtifact.codeMember}"

    withClue(s"the first bytes of $member: ${readBytes(member).take(4).map(b => f"${b & 0xff}%02x").mkString(" ")}")(
      isNativeObject(readBytes(member)) shouldBe true)
  }

  // The same archive through the system linker — no `-fuse-ld=lld`, which was the one link that
  // could read the bitcode the manifest's `lto` used to put here.
  /** A project whose manifest says nothing about LTO, archived with `--lto thin` on the command
    * line — the host that links with clang and lld and wants its link to optimize across the
    * boundary. The stderr is returned beside the paths, since the advice line is half the contract.
    */
  private def builtWithLtoFlag(): (String, String, String) = {
    val root = rootOf("mylib", boundary)
    val out  = s"$root/libmylib.a"

    writeFile(s"$root/${PackageConfig.FileName}",
      """package { name = "mylib", version = "0.1.0" }
        |""".stripMargin)

    val (status, notes) =
      diagnostics(Config(command = "build-c", file = root, output = Some(out), lto = Some("thin")))

    withClue(notes)(status shouldBe 0)
    (out, s"$out.h", notes)
  }

  /** Whether `bytes` begin as LLVM bitcode: the raw bitstream's `BC 0xC0DE`, or Apple's wrapper's
    * `0x0B17C0DE` little-endian.
    */
  private def isBitcode(bytes: Array[Byte]): Boolean = {
    val head = bytes.take(4).map(_ & 0xff).toList

    head == List(0x42, 0x43, 0xc0, 0xde) || head == List(0xde, 0xc0, 0x17, 0x0b)
  }

  "'--lto' on the build-c command line is parsed, and a mode clang does not have is refused" in {
    parseArgs(Seq("build-c", "/somewhere", "--lto", "thin")).map(_.lto) shouldBe Some(Some("thin"))
    parseArgs(Seq("build-c", "/somewhere", "--lto", "full")).map(_.lto) shouldBe Some(Some("full"))
    parseArgs(Seq("build-c", "/somewhere")).map(_.lto) shouldBe Some(None)
    Console.withErr(new java.io.ByteArrayOutputStream)(
      parseArgs(Seq("build-c", "/somewhere", "--lto", "medium"))) shouldBe None
  }

  "build-c's own help lists '--lto', saying the archive is native objects without it" in {
    val usage   = scopt.OParser.usage(parser)
    val section = usage.linesIterator
      .dropWhile(!_.startsWith("Command: build-c"))
      .drop(1)
      .takeWhile(!_.startsWith("Command: "))
      .mkString("\n")

    section should include("--lto")
    section should include("native objects")
  }

  // `--lto thin` asks for exactly what `sysl build` compiles under that mode, which is bitcode: the
  // flag is how a clang-and-lld host opts in, where the manifest's key never does.
  "a project archived with '--lto thin' holds bitcode, and build-c says how it must be linked" in {
    val ar = Toolchain.findAr(None) match
      case Right(found) => found
      case Left(why)    => cancel(why)

    val (archive, _, notes) = builtWithLtoFlag()
    val dir                 = createTempDirectory("sysl-c-bitcode-")
    val extract             = exec(Seq(ar, "x", s"--output=$dir", archive, LibraryArtifact.codeMember))

    withClue(extract.stderr)(extract.exitCode shouldBe 0)

    val member = s"$dir/${LibraryArtifact.codeMember}"

    withClue(s"the first bytes of $member: ${readBytes(member).take(4).map(b => f"${b & 0xff}%02x").mkString(" ")}")(
      isBitcode(readBytes(member)) shouldBe true)
    notes should include("sysl: this archive is LLVM bitcode: link it with clang and lld (-fuse-ld=lld)")
  }

  "and without the flag build-c gives no bitcode advice, whatever the manifest's 'lto' says" in {
    val root            = rootOf("mylib", boundary)
    val out             = s"$root/libmylib.a"

    writeFile(s"$root/${PackageConfig.FileName}",
      """package { name = "mylib", version = "0.1.0" }
        |lto = "thin"
        |""".stripMargin)

    val (status, notes) = diagnostics(Config(command = "build-c", file = root, output = Some(out)))

    withClue(notes)(status shouldBe 0)
    notes should not include "LLVM bitcode"
  }

  // The link the advice names. Skipped where this machine's clang cannot reach lld at all, which is
  // found out on a trivial program first so that a missing linker is never read as a bad archive.
  "a C program links against a '--lto thin' archive with clang and lld" in {
    val probeDir = createTempDirectory("sysl-c-lld-probe-")

    writeFile(s"$probeDir/p.c", "int main(void) { return 0; }\n")

    val probe = exec(Seq("clang", "-fuse-ld=lld", s"$probeDir/p.c", "-o", s"$probeDir/p"))

    assume(probe.exitCode == 0, s"clang cannot link with lld here: ${probe.stderr}")

    val (archive, header, _) = builtWithLtoFlag()
    val dir                  = createTempDirectory("sysl-c-lld-caller-")
    val source               = s"$dir/main.c"
    val exe                  = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(void) {
         |    printf("%d %d\\n", mylib_add(2, 3), mylib_scale(4, 5));
         |    return 0;
         |}
         |""".stripMargin)

    // The C side is compiled natively: what is under test is lld's link reading the archive's
    // bitcode, and `-flto` on `main.c` would also hand lld bitcode from whichever clang is first on
    // the PATH — Apple's, on a Mac, whose bitcode a Homebrew lld of another LLVM cannot codegen.
    val build = exec(Seq("clang", "-fuse-ld=lld", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "5 20"
  }

  "a C program links against an archive built under the manifest's LTO with the system linker" in {
    val (archive, header) = builtUnderLto()
    val dir               = createTempDirectory("sysl-c-lto-caller-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(void) {
         |    printf("%d %d\\n", mylib_add(2, 3), mylib_scale(4, 5));
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "5 20"
  }

  /** A module `pkg-config` on this machine knows about, when it knows about any — the first word of
    * `--list-all`'s first line, which is what makes the case below independent of what is installed.
    */
  private lazy val someModule: Option[String] =
    if !PkgConfig.available then None
    else
      val result = exec(Seq("pkg-config", "--list-all"))

      if result.exitCode != 0 then None
      else result.stdout.linesIterator.map(_.trim).find(_.nonEmpty).map(_.takeWhile(!_.isWhitespace))

  // A package binds an installed library through `pkg_config` in its manifest and need say `@link`
  // nowhere, so the directives alone are not the link line. slate's brotli, webp, zstd and hiredis
  // were all left off the list this way, and the C author met `Brotli*` as an undefined symbol.
  "the link advice names the pkg-config modules the manifest requires, which '@link' never sees" in {
    assume(someModule.isDefined)

    val module = someModule.get
    val root   = rootOf("mylib", boundary)

    writeFile(s"$root/${PackageConfig.FileName}",
      s"""package { name = "mylib", version = "0.1.0" }
         |requires { pkg_config { $module = "the library this test found installed" } }
         |""".stripMargin)

    val (status, notes) = diagnostics(Config(command = "build-c", file = root,
      output = Some(s"$root/libmylib.a")))

    withClue(notes)(status shouldBe 0)
    notes should include(s"pkg-config --libs $module")
  }

  /** A module handing an aggregate across under a name it chose, which is what a binding mirroring
    * a C library looks like: the type's spelling is the library's own rather than the mangled
    * instantiation `sh_sysl_box2d_c_Id` would give it (0142).
    */
  private val vectors =
    """module mylib
      |
      |@export("mylib_vec2")
      |struct Vec2
      |    x: i32
      |    y: i32
      |
      |@export("mylib_add_vec")
      |add(a: Vec2, b: Vec2) -> Vec2 = Vec2(a.x + b.x, a.y + b.y)
      |""".stripMargin

  "the header names a struct what its '@export' said, not what the module path derives" in {
    val text = readFile(built(vectors)._2)

    text should include("} mylib_vec2;")
    text should include("mylib_vec2 mylib_add_vec(mylib_vec2 a, mylib_vec2 b);")
    text should not include "mylib_Vec2"
  }

  /** **The name is only worth anything if a C compiler takes it**, which is this suite's whole
    * argument applied to the type half: everything above proves sysl agrees with itself. Here clang
    * declares a `mylib_vec2` of its own, fills it, hands it over by value and reads the result back.
    */
  "and a C program declares that struct by that name, fills it and gets sysl's answer back" in {
    val (archive, header) = built(vectors)
    val dir               = createTempDirectory("sysl-c-named-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(void) {
         |    mylib_vec2 a = { 1, 2 };
         |    mylib_vec2 b = { 10, 20 };
         |    mylib_vec2 c = mylib_add_vec(a, b);
         |    printf("%d %d\\n", c.x, c.y);
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "11 22"
  }

  /** An `opaque` struct is incomplete to everything outside its module (`reference/ffi.md § opaque`),
    * and a C caller is outside every module. So the header declares a tag with no body, and a field
    * with no C spelling at all — a `string` here — is never written, because nothing is laid out.
    */
  private val handles =
    """module mylib
      |
      |@export("mylib_handle")
      |opaque struct Handle
      |    name: string
      |    n: int
      |
      |@export("mylib_count")
      |count(h: *Handle) -> int = h.n
      |""".stripMargin

  "the header declares an 'opaque' struct incomplete, and lays out none of its fields" in {
    val text = readFile(built(handles)._2)

    text should include("typedef struct mylib_handle mylib_handle;\n")
    text should include("int32_t mylib_count(mylib_handle * h);")
    text should not include "no C spelling"
    text should not include "typedef struct {"
  }

  "and a C program holding only a pointer to that handle compiles and links against it" in {
    val (archive, header) = built(handles)
    val dir               = createTempDirectory("sysl-c-opaque-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(int argc, char **argv) {
         |    mylib_handle *h = NULL;
         |    if (argc > 99) printf("%d\\n", (int) mylib_count(h));
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", "-Werror", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)
  }

  /** An exported `type` is a `typedef` in the header (`reference/ffi.md § Naming a type`), used by
    * name in a prototype, a struct field, a function pointer's parameter, and — as a pointer — where
    * a handle is passed. clang under `-Werror` judges the header, and the run proves the values
    * crossed the boundary as what the names say they are.
    */
  private val aliases =
    """module mylib
      |
      |@export("slate_value")
      |type Handle = u64
      |
      |@export("slate_vm")
      |opaque struct Vm
      |    base: u64
      |
      |@export("slate_vm_ref")
      |type VmRef = *Vm
      |
      |@export("slate_pair")
      |struct Pair
      |    a: Handle
      |    each: *extern(Handle) -> Handle
      |
      |var vm: Vm = Vm(100)
      |
      |@export("slate_open")
      |open() -> VmRef = &vm
      |
      |@export("slate_int")
      |int_of(h: VmRef, n: i64) -> Handle = h.base + u64(n)
      |
      |@export("slate_apply")
      |apply(p: *Pair) -> Handle = p.each(p.a)
      |""".stripMargin

  "an exported 'type' is a typedef the header declares and every position spells" in {
    val text = readFile(built(aliases)._2)

    text should include("typedef uint64_t slate_value;\n")
    text should include("typedef slate_vm * slate_vm_ref;\n")
    text should include("slate_value slate_int(slate_vm_ref h, int64_t n);")
    text should include("\tslate_value a;")
    text should include("\tslate_value (*each)(slate_value);")
    text should not include "no C spelling"
  }

  "and a C program written against those names compiles under -Werror, links and runs" in {
    val (archive, header) = built(aliases)
    val dir               = createTempDirectory("sysl-c-typedef-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |static slate_value twice(slate_value v) { return v * 2; }
         |
         |int main(void) {
         |    slate_vm_ref vm = slate_open();
         |    slate_value v = slate_int(vm, 23);
         |    slate_pair p = { v, twice };
         |    printf("%llu %llu\\n", (unsigned long long) v, (unsigned long long) slate_apply(&p));
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", "-Werror", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "123 246"
  }

  /** A function pointer in all three places a C declarator can hold one — a parameter, a struct
    * field and a result — each of which the header once spelled `void (*)(int32_t) name`, which is
    * not C. clang under `-Werror` is the judge, and a callback it really calls is the proof.
    */
  private val callbacks =
    """module mylib
      |
      |@export("mylib_each")
      |each(xs: *i32, n: usize, f: *extern(i32) -> unit)
      |    for i in 0..<n do f(xs[i])
      |
      |@export("mylib_sink")
      |struct Sink
      |    put: *extern(i32) -> unit
      |    n: i32
      |
      |@export("mylib_emit")
      |emit(s: *Sink) = s.put(s.n)
      |
      |@export("mylib_hook")
      |hook(f: *extern(i32) -> unit) -> *extern(i32) -> unit = f
      |""".stripMargin

  "the header names a function pointer inside its declarator, as a parameter, a field and a result" in {
    val text = readFile(built(callbacks)._2)

    text should include("void mylib_each(int32_t * xs, uint64_t n, void (*f)(int32_t));")
    text should include("\tvoid (*put)(int32_t);")
    text should include("void (*mylib_hook(void (*f)(int32_t)))(int32_t);")
  }

  "and a C program passes, stores and gets back a real callback, under -Werror" in {
    val (archive, header) = built(callbacks)
    val dir               = createTempDirectory("sysl-c-callback-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |static int32_t total = 0;
         |
         |static void add(int32_t x) { total += x; }
         |
         |int main(void) {
         |    int32_t xs[3] = {1, 2, 3};
         |    mylib_each(xs, 3, add);
         |    mylib_sink s = { mylib_hook(add), 10 };
         |    mylib_emit(&s);
         |    printf("%d\\n", (int) total);
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", "-Werror", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "16"
  }

  "a C program links an export that uses the standard library, which is the case a '.syslib' cannot serve" in {
    val (archive, header) = built(talkative)
    val dir               = createTempDirectory("sysl-c-printing-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include "$header"
         |
         |int main(void) {
         |    mylib_greet(7);
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "hello 7"
  }

  /** **A module that keeps a `Buf` at module scope**, which is the shape card `0263` is about and
    * the one an archive could not hold at all until it landed.
    *
    * `squares` is *computed* storage: nothing about `fill()` is data an object file can carry, so
    * the value has to be built by something that runs. A program builds it at the top of `@main`;
    * an archive has no `@main`, and until `0263` the export reaching it was refused outright — so a
    * boundary layer linked into a C project could hold no `Buf`, no closure and no counted value,
    * however it was reached.
    */
  private val counted =
    """module mylib
      |
      |import sysl.buf.{Buf, buf}
      |
      |val squares: Buf[i32] = fill()
      |
      |fill() -> Buf[i32]
      |    var b: Buf[i32] = buf()
      |
      |    for i in 1..<4
      |        b.push(i32(i) * i32(i))
      |
      |    b
      |
      |@export("mylib_count")
      |count() -> i32 = i32(squares.len())
      |
      |@export("mylib_at")
      |at(i: i32) -> i32 = squares[usize(i)]
      |""".stripMargin

  /** **A closure at module scope, which is the shape the card was actually filed from.**
    *
    * `sysl-lang/skitter` wanted an application to register what happens when the system bars move —
    * `var sink: &Fn(int, int, int, int) -> unit` — and could not have one, so it stored four bare
    * `int`s and offered them to be pulled instead. This is that in miniature: a counted reference in
    * module storage, written by one export and called through another.
    *
    * It is a second module rather than another export on `counted` because it asks a different
    * question of the constructor. A `Buf` is storage the initializer *builds*; a `&Fn` is a
    * reference the initializer has to take a **count** of, which is the half of `genInitStore`
    * that an ordinary assignment does not do.
    */
  private val callback =
    """module mylib
      |
      |import sysl.buf.{Buf, buf}
      |
      |var seen: Buf[i32] = buf()
      |
      |var sink: &Fn(i32) -> unit = (n) -> seen.push(n * 2)
      |
      |@export("mylib_fire")
      |fire(n: i32) = sink(n)
      |
      |@export("mylib_last")
      |last() -> i32 = if seen.len() == 0 then -1 else seen[seen.len() - 1]
      |""".stripMargin

  "and a closure it holds at module scope, which is what the card was filed from" in {
    assume(Toolchain.clangAvailable, "clang not available")

    val (archive, header) = built(callback)
    val dir               = createTempDirectory("sysl-c-callback-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(void) {
         |    printf("%d ", mylib_last());
         |    mylib_fire(21);
         |    printf("%d\\n", mylib_last());
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "-1 42"
  }

  /** **The acceptance test for `0263`, and the only thing that can answer it.** Everything the
    * compiler decides here can be read off the IR — `ExportTests` does exactly that — and none of
    * it proves that a real loader calls the constructor before a real C `main`. This links the
    * archive with clang and asks the C side for numbers only a filled `Buf` has.
    *
    * `1 4 9` and not zeros: an archive whose constructor never ran answers a length of nothing, so
    * the count alone would catch it, and the elements say the storage holds what sysl put there
    * rather than merely something.
    */
  "a C program reads module storage a constructor filled, which an archive has no 'main' to fill" in {
    assume(Toolchain.clangAvailable, "clang not available")

    val (archive, header) = built(counted)
    val dir               = createTempDirectory("sysl-c-counted-")
    val source            = s"$dir/main.c"
    val exe               = s"$dir/caller"

    writeFile(source,
      s"""#include <stdio.h>
         |#include "$header"
         |
         |int main(void) {
         |    printf("%d", mylib_count());
         |    for (int i = 0; i < mylib_count(); i++) printf(" %d", mylib_at(i));
         |    printf("\\n");
         |    return 0;
         |}
         |""".stripMargin)

    val build = exec(Seq("clang", source, archive, "-o", exe))

    withClue(build.stderr)(build.exitCode shouldBe 0)

    val run = exec(Seq(exe))

    withClue(run.stderr)(run.exitCode shouldBe 0)
    run.stdout.trim shouldBe "3 1 4 9"
  }

  /** Seven is a number only the C knows and the multiplication is only the sysl's, so an archive that
   * dropped either object cannot answer 42 — and the one it drops is the one this section is about.
   */
  private val shim = "int demo_seven(void) { return 7; }\n"

  private val callingC =
    """module demo
      |
      |extern "demo_seven" c_seven() -> i32
      |
      |seven_times(n: i32) -> i32 = c_seven() * n
      |""".stripMargin

  private val exporting =
    """module mylib
      |
      |@export("mylib_answer")
      |answer() -> i32 = demo.seven_times(6)
      |""".stripMargin

  /** `reference/ffi.md § A library may carry C`'s table gives a source root named with `--lib` the
   * same answer as the project's own tree: its C is compiled and reaches the link. For this command
   * the link line is the archive, so "reaches the link" means "is a member".
   *
   * **The failure this pins was silent in both directions.** `build-c` walked the project's tree
   * alone, so a package carrying a shim built cleanly, wrote an archive, said nothing, and handed a
   * C project a wall of undefined references from a linker that had never heard of the flag the
   * omission came from. Nothing between the two ends could see it: the sysl compiled, the archive
   * existed, and `ar t` was the only place the absence was visible.
   */
  "the C of a '--lib' source tree" - {

    "is a member of the archive, exactly as the project's own is" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val lib  = rootWithC("demo", callingC, "shim.c" -> shim)
      val root = rootOf("mylib", exporting)
      val out  = s"$root/libmylib.a"

      succeeds(Config(command = "build-c", file = root, output = Some(out), libs = List(lib)))

      Ar.members(readBytes(out)) match
        case Right(members) => members.map(_.name) should contain("demo.shim.o")
        case Left(err)      => fail(err)
    }

    "and a C program links against it and gets the answer the shim computed" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val lib    = rootWithC("demo", callingC, "shim.c" -> shim)
      val root   = rootOf("mylib", exporting)
      val out    = s"$root/libmylib.a"
      val header = s"$out.h"

      succeeds(Config(command = "build-c", file = root, output = Some(out), libs = List(lib)))

      val dir    = createTempDirectory("sysl-c-shim-")
      val source = s"$dir/main.c"
      val exe    = s"$dir/caller"

      writeFile(source,
        s"""#include <stdio.h>
           |#include "$header"
           |
           |int main(void) {
           |    printf("%d\\n", mylib_answer());
           |    return 0;
           |}
           |""".stripMargin)

      val build = exec(Seq("clang", source, out, "-o", exe))

      withClue(build.stderr)(build.exitCode shouldBe 0)

      val run = exec(Seq(exe))

      withClue(run.stderr)(run.exitCode shouldBe 0)
      run.stdout.trim shouldBe "42"
    }

    // The row of the table that already worked, kept beside the one that did not so that a change
    // moving the collection cannot fix one end while quietly dropping the other.
    "beside the project's own, which lands under a name of its own" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val lib  = rootWithC("demo", callingC, "shim.c" -> shim)
      val root = rootWithC("mylib", exporting, "extra.c" -> "int mylib_unused(void) { return 0; }\n")
      val out  = s"$root/libmylib.a"

      succeeds(Config(command = "build-c", file = root, output = Some(out), libs = List(lib)))

      Ar.members(readBytes(out)) match
        case Right(members) => members.map(_.name) should contain allOf ("demo.shim.o", "mylib.extra.o")
        case Left(err)      => fail(err)
    }
  }

  /** The **standard library's** own C, which is the fourth tree and the one nothing used to look at.
   *
   * `build-c` compiles the standard module into what it writes — that is why `--std-lib` is refused
   * here — so a shim of the library's own is exactly as much this archive's business as the
   * project's is, and it fails in the same silent way: the sysl compiles, the archive is written, and
   * a C project's linker reports a symbol nobody in that project has ever heard of.
   *
   * The C only exists because it is under a per-OS directory (`reference/modules.md § Platform
   * selection`), so this is also what says the selection reaches this command and not only the ones
   * that link.
   */
  "the standard library's own C" - {

    "is a member of the archive, because build-c compiles the library into it" in {
      assume(Toolchain.clangAvailable, "clang not available")
      assume(StdRoot.root.isDefined, "the library is not reachable from the test working directory")

      val listing =
        """module mylib
          |
          |import sysl.fs.entries
          |
          |@export("mylib_count")
          |count() -> i32 = i32(entries("/tmp").unwrap().len())
          |""".stripMargin

      val root = rootOf("mylib", listing)
      val out  = s"$root/libmylib.a"

      succeeds(Config(command = "build-c", file = root, output = Some(out)))

      // `library/sysl/fs/__<os>__/dirent.c` is in the module `sysl.fs`, since the folder names
      // nothing — so the member is named for the module and the file, with no trace of which
      // operating system's copy it was.
      Ar.members(readBytes(out)) match
        case Right(members) => members.map(_.name) should contain("sysl.fs.dirent.o")
        case Left(err)      => fail(err)
    }
  }

  /** A `--lib` source root holding two modules: one the program uses, and one nothing names.
   *
   * Two are the whole point. A root with one module cannot tell "a dependency's exports are dropped"
   * from "a dependency's exports are kept", because whichever answer the rule gives, the one module
   * is reached — so the used module is the control and the spare one is the measurement.
   */
  private def twoModuleLib(spare: String): String = {
    val root = createTempDirectory("sysl-cli-deplib-")

    createDirectory(s"$root/used")
    createDirectory(s"$root/spare")
    writeFile(s"$root/used/lib.sysl", "module used\n\ndouble(n: i32) -> i32 = n * 2\n")
    writeFile(s"$root/spare/lib.sysl", spare)
    root
  }

  private val spareExport =
    """module spare
      |
      |@export("spare_thing")
      |thing() -> i32 = 42
      |""".stripMargin

  private val usingUsed =
    """module mylib
      |
      |@export("mylib_answer")
      |answer(n: i32) -> i32 = used.double(n)
      |""".stripMargin

  /** A handler taking nothing, which is the shape RISC-V requires, and an `extern` for it to call so
   * that the body reaches something a dropped handler would take with it.
   */
  private val handlerModule =
    """module spare
      |
      |extern "ack" ack()
      |
      |interrupt timer()
      |    ack()
      |""".stripMargin

  /** A module with a type of its own that has a destructor, **and a function that makes one** — the
   * second half is what puts the type in `structInsts` and so its hook in `program.destructors`.
   */
  private val droppingModule =
    """module spare
      |
      |struct Handle
      |    n: i32
      |
      |impl Drop for Handle
      |    drop(self) = print("gone", self.n)
      |
      |make() -> &Handle = Handle(1)
      |""".stripMargin

  /** An `@export` in a dependency is a root only where the program reaches its module (card 0111).
   *
   * **A dependency's source root is compiled whole rather than by what the program imports**, so
   * before this every module of every `--lib` root and every fetched package put its exports in the
   * consumer's archive. What that cost was a package carrying its own program: a test application's
   * `@export("main")` reached every consumer, and the two `main`s fought at the link — which is why
   * `sysl-lang/zephyr` had to put its suite in a second repository.
   */
  "an '@export' in a '--lib' source tree" - {

    "is dropped where nothing in the program reaches its module" in {
      val lib = twoModuleLib(spareExport)
      val ir  = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(lib)))

      symbols(ir, "define") should contain("mylib_answer")
      symbols(ir, "define") should not contain "spare_thing"
    }

    // The control: the same tree, the same flag, and the one thing that differs is that the program
    // names the module. Without this the test above passes for a compiler that dropped every export.
    "and is kept where the program does reach it" in {
      val program =
        """module mylib
          |
          |@export("mylib_answer")
          |answer(n: i32) -> i32 = used.double(n) + spare.thing()
          |""".stripMargin

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", program),
        libs = List(twoModuleLib(spareExport))))

      symbols(ir, "define") should contain("spare_thing")
    }

    // An `import` is a reference like any other — `AnalyzerBase.dependsOn` records one — and it is
    // what a module holding nothing but an exported C entry has instead of a call. A rule asking
    // whether the program *called* something there would drop exactly the case an export exists for.
    "including where the only reference is an 'import'" in {
      val program =
        """module mylib
          |
          |import spare
          |
          |@export("mylib_answer")
          |answer(n: i32) -> i32 = used.double(n)
          |""".stripMargin

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", program),
        libs = List(twoModuleLib(spareExport))))

      symbols(ir, "define") should contain("spare_thing")
    }

    // The header is written from the pruned tree, so it says the same thing the archive does — which
    // is the property `Compiled.exports` exists for: a header naming a function the object does not
    // define is the one failure a C project cannot diagnose.
    "and the header does not declare it either" in {
      val out = new java.io.ByteArrayOutputStream

      val status = Console.withOut(out)(sh.sysl.execute(Config(command = "emit-header",
        file = rootOf("mylib", usingUsed), libs = List(twoModuleLib(spareExport)), noStdLib = true)))

      status shouldBe 0
      out.toString should include("mylib_answer")
      out.toString should not include "spare_thing"
    }

    /** The case the card was filed for.
     *
     * **Counted rather than looked for**, because the program has a `main` of its own — the entry
     * point codegen lays down — so the symbol is present either way and its presence says nothing.
     * What the second one makes is a module with two `define`s of it, which is not a module at all.
     */
    "so a package carrying its own program no longer hands every consumer a second 'main'" in {
      val lib = twoModuleLib("module spare\n\n@export(\"main\")\nrun() -> i32 = 0\n")

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(lib)))

      symbols(ir, "define") should contain("mylib_answer")
      ir.linesIterator.count(l => l.startsWith("define ") && l.contains("@main(")) shouldBe 1
    }

    /** A root the program reaches only through another of its dependencies.
     *
     * The closure and not the direct edges, which is what makes a package built on a package work:
     * the program names `used`, `used` names `middle`, and an export in `middle` is as reachable as
     * one the program named itself.
     */
    "and a module reached only through another dependency is reached" in {
      val root = createTempDirectory("sysl-cli-deplib-")

      createDirectory(s"$root/used")
      createDirectory(s"$root/middle")
      writeFile(s"$root/used/lib.sysl", "module used\n\ndouble(n: i32) -> i32 = middle.step(n)\n")
      writeFile(s"$root/middle/lib.sysl",
        "module middle\n\n@export(\"middle_thing\")\nstep(n: i32) -> i32 = n * 2\n")

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(root)))

      symbols(ir, "define") should contain("middle_thing")
    }

    /** The edge has a direction, and this is the case that would pass with it read backwards.
     *
     * `spare` imports `used`, which the program does reach — so the two modules are joined, and a
     * rule asking whether they are *connected* rather than which way keeps an export nothing can
     * arrive at. What makes a module reachable is being pointed **at**.
     */
    "while a module that imports a reached one is not itself reached" in {
      val root = createTempDirectory("sysl-cli-deplib-")

      createDirectory(s"$root/used")
      createDirectory(s"$root/spare")
      writeFile(s"$root/used/lib.sysl", "module used\n\ndouble(n: i32) -> i32 = n * 2\n")
      writeFile(s"$root/spare/lib.sysl",
        "module spare\n\n@export(\"spare_thing\")\nthing() -> i32 = used.double(1)\n")

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(root)))

      symbols(ir, "define") should not contain "spare_thing"
    }

    // The rule qualifies a *dependency's* export and nothing else, so two of the program's own
    // claiming one symbol are refused exactly as before. Without this the fix above reads as
    // "duplicate exports stopped being checked".
    "while two of the program's own exports claiming one symbol are still refused" in {
      val program =
        """module mylib
          |
          |@export("mylib_answer")
          |answer() -> i32 = 1
          |
          |@export("mylib_answer")
          |other() -> i32 = 2
          |""".stripMargin

      val (status, notes) = diagnostics(Config(command = "emit-llvm", file = rootOf("mylib", program),
        libs = List(twoModuleLib(spareExport))))

      status should not be 0
      notes should include("'mylib_answer' is exported by")
    }
  }

  /** The other three unconditional roots, qualified the same way (card 0118).
   *
   * `Reachability.entryPoints` has four kinds and 0111 qualified one, which left the rule holding
   * only while an export carried no second attribute: a function that is *also* placed or *also* a
   * handler was kept for that reason and landed its C symbol in the consumer anyway. So the
   * qualification is about **provenance** and applies to all four — a module a dependency supplied
   * contributes nothing unless the program reaches it, and `import` is how a consumer asks for one.
   */
  "the other roots in a '--lib' source tree are qualified the same way" - {

    /** A `@section` definition placed in RAM by a linker script.
     *
     * **`@section` implies `@llvm.used` (card 0101), so nothing downstream will remove it either** —
     * a placed definition kept here is bytes in every consumer's image, on parts where the region it
     * asks for is the scarce thing the attribute exists to manage.
     */
    "a '@section' definition is dropped where nothing reaches its module" in {
      val lib = twoModuleLib("module spare\n\n@section(\".ramfunc\")\nplaced() -> i32 = 7\n")
      val ir  = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(lib)))

      symbols(ir, "define") should not contain "spare$placed"
      ir should not include ".ramfunc"
    }

    "and is kept where the program does reach it" in {
      val program =
        """module mylib
          |
          |@export("mylib_answer")
          |answer(n: i32) -> i32 = used.double(n) + spare.placed()
          |""".stripMargin

      val lib = twoModuleLib("module spare\n\n@section(\".ramfunc\")\nplaced() -> i32 = 7\n")
      val ir  = emitted(Config(command = "emit-llvm", file = rootOf("mylib", program),
        libs = List(lib)))

      symbols(ir, "define") should contain("spare$placed")
      ir should include(".ramfunc")
    }

    /** An interrupt handler, which is the kind with the loudest consequence: a package shipping one
     * for a peripheral the consumer never uses was putting it in the consumer's vector table, and two
     * packages shipping a handler for one vector is the `main` collision 0111 was filed for, a layer
     * down.
     *
     * Lowered for RISC-V, where the convention is a function attribute rather than a calling
     * convention — the host cannot be asked, since AArch64 has no interrupt attribute at all
     * (`CallingConventionTests`).
     */
    "an interrupt handler is dropped where nothing reaches its module" in {
      val lib = twoModuleLib(handlerModule)
      val ir  = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(lib), target = Some("riscv64-linux")))

      symbols(ir, "define") should not contain "spare$timer"
    }

    // An `import` and nothing else, which is what a consumer wanting a package's handler writes:
    // no function of that module is ever called, so a rule asking about calls would drop it.
    "and an 'import' is enough to keep it" in {
      val program =
        """module mylib
          |
          |import spare
          |
          |@export("mylib_answer")
          |answer(n: i32) -> i32 = used.double(n)
          |""".stripMargin

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", program),
        libs = List(twoModuleLib(handlerModule)), target = Some("riscv64-linux")))

      symbols(ir, "define") should contain("spare$timer")
    }

    /** A destructor — the kind the card guessed was already safe, on the reasoning that
     * `program.destructors` is built from the types the compilation laid out.
     *
     * **It is not**, and the reason is the same one behind every other kind here: a dependency's tree
     * is compiled *whole*, so a module that makes a value of its own type has that type instantiated
     * and its hook in the map, whether or not the program can reach either.
     */
    "a destructor is dropped where nothing reaches its module" in {
      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(twoModuleLib(droppingModule))))

      symbols(ir, "define") should not contain "spare$Handle.drop"
    }

    // The control that says the hook still survives for a type the program does use: a destructor
    // pruned where something can release a box of that payload is a link error against a name no
    // line of the program contains.
    "while one whose module the program reaches survives" in {
      val program =
        """module mylib
          |
          |@export("mylib_answer")
          |answer(n: i32) -> i32 = used.double(n) + spare.make().n
          |""".stripMargin

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", program),
        libs = List(twoModuleLib(droppingModule))))

      symbols(ir, "define") should contain("spare$Handle.drop")
    }

    /** The destructor is the kind whose over-pruning is a **link error** rather than a size cost —
     * the release hook the emitter builds calls a name no line of the program contains — so it is
     * the one asserted by building and running a program rather than by reading IR.
     *
     * The type is reached the way a package's types usually are: through another of its modules,
     * never named by the program. `moduleDeps` is the whole graph rather than the program's own
     * edges, so `used` naming `spare` is what carries it.
     */
    "a destructor reached only through another dependency module still runs" in {
      assume(Toolchain.clangAvailable, "clang not available")

      val root = createTempDirectory("sysl-cli-deplib-")

      createDirectory(s"$root/used")
      createDirectory(s"$root/spare")
      writeFile(s"$root/used/lib.sysl",
        "module used\n\nmake() -> &spare.Handle = spare.Handle(1)\n")
      writeFile(s"$root/spare/lib.sysl", droppingModule)

      val src =
        """hold()
          |    var h = used.make()
          |    print("held", h.n)
          |
          |hold()
          |print("out")
          |""".stripMargin

      ran(Config(command = "run", file = program(src), libs = List(root))) shouldBe
        "held 1\ngone 1\nout\n"
    }

    /** The measurement that decided the rule was about provenance rather than about kinds.
     *
     * Before this, an export in an unreached module was dropped *as an export* and kept anyway
     * because it was also placed — so 0111's fix held only for a function carrying one attribute.
     * Counted rather than looked for, exactly as 0111's is: the program has a `main` of its own.
     */
    "and an export kept alive by a second attribute no longer defeats the export rule" in {
      val lib = twoModuleLib(
        "module spare\n\n@section(\".ramfunc\")\n@export(\"main\")\nplaced() -> i32 = 7\n")

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(lib)))

      ir.linesIterator.count(l => l.startsWith("define ") && l.contains("@main(")) shouldBe 1
    }

    "the same for an export carried by a handler" in {
      val lib = twoModuleLib(
        "module spare\n\nextern \"ack\" ack()\n\n@export(\"SysTick_Handler\")\ninterrupt timer()\n" +
          "    ack()\n")

      val ir = emitted(Config(command = "emit-llvm", file = rootOf("mylib", usingUsed),
        libs = List(lib), target = Some("riscv64-linux")))

      symbols(ir, "define") should not contain "SysTick_Handler"
    }
  }

  // The flag has a reading everywhere else and none here, so it is refused rather than discarded:
  // taking it silently would produce the unlinkable archive this command exists not to write.
  "naming a prebuilt standard module is refused, since a C link line cannot carry one" in {
    val root = rootOf("mylib", boundary)

    val (status, notes) = diagnostics(Config(command = "build-c", file = root,
      output = Some(s"$root/libmylib.a"), stdLib = Some(s"$root/std.syslib")))

    status should not be 0
    notes should include("--std-lib has nothing to name here")
  }
}
