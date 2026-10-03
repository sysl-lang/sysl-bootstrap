package sh.sysl

import io.github.edadma.cross_platform.*

/** `sysl.fs` names the libc symbols Android's Bionic actually has.
 *
 * Found linking a skitter app that wrote its preferences with `write_text_atomic`: the archive asked
 * for `__errno_location` (glibc's spelling; Bionic's is `__errno`) and `getentropy` (in Bionic only from
 * API 28, where `aarch64-android` is API 24). Both are a question about which symbol sysl declares, so
 * the cases read the IR an `emit-llvm` for the target writes — a `build-c` for Android would need an
 * NDK, and what is being asserted is sysl's own output.
 */
class LibraryAndroidLibcCliTests extends LibraryCliSupport {

  private def project(main: String): String = {
    val root = createTempDirectory("sysl-cli-bionic-")

    writeFile(s"$root/${PackageConfig.FileName}", "")
    writeFile(s"$root/main.sysl", main)
    root
  }

  private val publishes =
    """import sysl.fs.write_text_atomic
      |
      |@export("entry")
      |entry() -> i32
      |    write_text_atomic("prefs.txt", "x") match
      |        Ok(_) -> 0
      |        Err(_) -> 1
      |""".stripMargin

  "an atomic write for aarch64-android" - {
    lazy val ir = emitted(Config(command = "emit-llvm", file = project(publishes),
      target = Some("aarch64-android")))

    "reads errno through Bionic's __errno, not glibc's __errno_location" in {
      symbols(ir, "declare") should contain("__errno")
      ir should not include "__errno_location"
    }

    "draws its pending name's token from arc4random_buf, which Bionic has at API 24" in {
      symbols(ir, "declare") should contain("arc4random_buf")
      ir should not include "getentropy"
    }
  }

  // The other side of the branch: glibc has `arc4random_buf` only from 2.36, so a Linux host keeps
  // the call it always made.
  "an atomic write for x86_64-linux keeps getentropy and __errno_location" in {
    val ir = emitted(Config(command = "emit-llvm", file = project(publishes),
      target = Some("x86_64-linux")))

    symbols(ir, "declare") should contain allOf ("getentropy", "__errno_location")
    ir should not include "arc4random_buf"
  }

  // And the BSD side: Darwin's accessor is `__error`, ahead of the Android arm in the `#if`.
  "an atomic write for aarch64-macos keeps getentropy and __error" in {
    val ir = emitted(Config(command = "emit-llvm", file = project(publishes),
      target = Some("aarch64-macos")))

    symbols(ir, "declare") should contain allOf ("getentropy", "__error")
    ir should not include "__errno"
    ir should not include "arc4random_buf"
  }
}
