package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** A type costs what it runs (`reference/modules.md § A type costs what it runs`).
 *
 * A module that gave up `os` may **name** a type whose module requires it — in a field, in a
 * variant's payload, in a signature, as a type argument — because naming one runs nothing. What it
 * may not do is run that module's code: call a method of the type, or hold a value of it that can
 * **die**, since a destructor is code the type runs wherever that happens.
 *
 * The case that asked for it is a module whose error enum wraps `sysl.fs.IoError` for its one
 * `@needs(os)` function: the enum named the type, so the module was charged `os` and a `@no_os`
 * program importing anything else from it was refused at the import.
 */
class TypeChargingTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  private val plainMain = "main.sysl" -> "@no_os\n\nimport sys.plain\n\nprint(plain())\n"

  // The error enum wraps the gated type, and the one function that can produce it says so.
  private val wrapsError = "sys/a.sysl" -> ("module sys\n\nimport sysl.fs.{IoError, write_bytes}\n\n" +
    "enum E\n    W(e: IoError)\n\n@needs(os)\n" +
    "save(f: string) -> Result[unit, E] = write_bytes(f, \"x\".bytes).map_err((e) -> W(e))\n\n" +
    "plain() -> int = 3\n")

  "naming a gated type runs nothing, so it charges nothing" - {

    "a variant carrying one leaves the rest of its module importable" in {
      runOf(wrapsError, plainMain) shouldBe "3\n"
    }

    "and so does a signature naming one, though nothing annotates it" in {
      runOf("sys/a.sysl" -> "module sys\n\nkind(e: sysl.fs.IoError) -> int = 1\n\nplain() -> int = 3\n",
        plainMain) shouldBe "3\n"
    }

    "and so does a gated type as a type argument" in {
      runOf("sys/a.sysl" -> ("module sys\n\nimport sysl.fs.IoError\n\n" +
        "struct Log\n    last: Option[IoError]\n\nplain() -> int = 3\n"), plainMain) shouldBe "3\n"
    }

    "and so does constructing a value of one that has no destructor" in {
      runOf("main.sysl" -> ("@no_os\n\nimport sysl.fs.IoError\n\nval e: IoError = IoError.NotFound\n" +
        "print(e match\n    IoError.NotFound -> 1\n    _ -> 2)\n")) shouldBe "1\n"
    }

    "and so does a qualified path through the type, which names it the same way" in {
      runOf("main.sysl" -> ("@no_os\n\nval e = sysl.fs.IoError.NotFound\n" +
        "print(e match\n    sysl.fs.IoError.NotFound -> 1\n    _ -> 2)\n")) shouldBe "1\n"
    }

    "while calling the function that does run it is refused at the call" in {
      val e = errOf(wrapsError, "main.sysl" -> "@no_os\n\nimport sys.save\n\nprint(save(\"/tmp/x\").is_ok())\n")

      e should include("this reaches 'sys.save', which needs 'os', and this module declared '@no_os'")
      e should include("main.sysl:5:7")
      e shouldNot include("which requires 'os'")
    }

    "and calling one of the type's own methods is refused, since that runs its module's code" in {
      val e = errOf("main.sysl" -> ("@no_os\n\nimport sysl.fs.IoError\n\nval e: IoError = IoError.NotFound\n" +
        "print(e.code())\n"))

      // At the import, as for any module a shipping reference charges: it is what let the call name it.
      e should include("this reaches 'sysl.fs', which requires 'os', and this module declared 'no os'")
      e should include("main.sysl:3:1")
    }
  }

  "a type with a destructor costs its module wherever a value of it can die" - {

    // `sys` requires `os` through the destructor alone; `Plain` beside it runs nothing.
    val withDrop = "sys/a.sysl" -> ("module sys\n\nimport sysl.fs.write_bytes\n\n" +
      "struct Plain\n    n: int\n\nstruct Handle\n    n: int\n\n" +
      "impl Drop for Handle\n    drop(self)\n        val _ = write_bytes(\"/tmp/x\", \"x\".bytes)\n")

    "so a program holding one is refused where it holds it" in {
      val e = errOf(withDrop, "main.sysl" -> ("@no_os\n\nimport sys.Handle\n\n" +
        "val h: &Handle = Handle(1)\nprint(h.n)\n"))

      e should include("a 'sys.Handle' can die here, and its destructor reaches 'sys', which requires 'os'")
      e should include("main.sysl:5:")
    }

    "while a type beside it with none is free to hold" in {
      runOf(withDrop, "main.sysl" -> "@no_os\n\nimport sys.Plain\n\nval p = Plain(4)\nprint(p.n)\n") shouldBe "4\n"
    }

    // `box` never names `sys` anywhere a value of it could die but the field, and the program never
    // names `sys` at all: what reaches the destructor is the field, through the type that holds it.
    "and a type that holds one dies with it, so holding that is refused too" in {
      val holder = "box/a.sysl" -> "module box\n\nimport sys.Handle\n\nstruct Holder\n    h: &Handle\n"
      val e      = errOf(withDrop, holder,
        "main.sysl" -> "@no_os\n\nimport box.Holder\n\nsize(b: &Holder) -> int = 1\n\nprint(2)\n")

      e should include("a 'box.Holder' can die here, and its destructor reaches 'box', which requires 'os'")
      e should include("main.sysl:5:")
    }
  }
}
