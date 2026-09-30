package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `listDirectory` is how the compiler reads a directory, and what it owes beyond the list is that it
 * gives back the descriptor it opened.
 *
 * **This is a test because the leak it replaced took the Linux CI down.** `cross_platform.listFiles`
 * leaves its `Files.list` stream for the garbage collector, which never closes one, so each call
 * held a descriptor until the process ended. A compilation walks every directory of every tree it
 * reads, and the suite compiles thousands of programs in one process — about an hour into a run, the
 * runner's limit was reached and every file operation after it failed with *"Too many open files"*:
 * 261 failures across every suite that ran afterwards, none of them about directories.
 *
 * The count is read off `/dev/fd`, which lists this process's open descriptors on both macOS and
 * Linux. Other suites open and close files beside this one, so the bound is loose on purpose: a leak
 * of one descriptor per call is thousands over it, and the noise is not.
 */
class DirectoryListingTests extends AnyFreeSpec with Matchers {

  private val calls = 3000

  private def openDescriptors: Int = listDirectory("/dev/fd").length

  "listing a directory" - {
    "answers its entries as absolute paths, sorted" in {
      val dir = createTempDirectory("sysl-listing-")

      writeFile(s"$dir/b.sysl", "")
      writeFile(s"$dir/a.sysl", "")
      createDirectory(s"$dir/c")

      listDirectory(dir).map(Project.basename) shouldBe Seq("a.sysl", "b.sysl", "c")
      listDirectory(dir).forall(_.startsWith("/")) shouldBe true
    }

    "refuses something that is not a directory" in {
      val dir = createTempDirectory("sysl-listing-")

      writeFile(s"$dir/f", "")
      an[IllegalArgumentException] should be thrownBy listDirectory(s"$dir/f")
    }

    "gives back the descriptor it opened, however many times it is asked" in {
      assume(isDirectory("/dev/fd"), "this machine has no /dev/fd to count descriptors with")

      val dir    = createTempDirectory("sysl-listing-")
      val before = openDescriptors

      for _ <- 1 to calls do listDirectory(dir)

      (openDescriptors - before) should be < (calls / 3)
    }
  }
}
