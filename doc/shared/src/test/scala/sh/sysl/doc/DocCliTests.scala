package sh.sysl.doc

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `sysl-doc`'s command line — argument parsing, and the exit code each path answers with.
 *
 * **The exit code is the point of this suite.** `DocCli.run` is the seam held apart from `@main` for
 * exactly the reason `sysl`'s `drive` is: `@main` calls `processExit`, so a test that went through it
 * would take the test runner down with it, and the number it was going to assert is the one thing a
 * caller in a shell script actually sees. It was wrong on the generate path and nothing else would
 * have noticed — the command printed its success line and exited 1.
 */
class DocCliTests extends AnyFreeSpec with Matchers {

  /** A throwaway output directory under the build's own target tree. */
  private def out(name: String): String = {
    val dir = s"target/doc-cli-tests/$name"

    sh.sysl.Project.discard(dir)
    dir
  }

  /** The smallest juicer site that renders a generated page, built from nothing.
   *
   * `api-module` and `api-index` are the `juicerapi` theme's layouts and juicer-core is a library
   * jar with no theme files in it, so the templates are supplied here — the smallest thing that
   * emits a body, since none of this is about the theme.
   */
  private def site(name: String): String = {
    val root = s"target/doc-cli-tests/$name"

    sh.sysl.Project.discard(root)
    sh.sysl.Project.makeDirectories(s"$root/content")
    sh.sysl.Project.makeDirectories(s"$root/layouts/_default")

    val template = "<html><body>{{ .content }}</body></html>\n"

    writeFile(s"$root/site.toml", "title = \"probe\"\n")
    writeFile(s"$root/content/_index.md", "---\ntitle: home\n---\n\nprobe\n")
    writeFile(s"$root/layouts/_default/folder.html", template)
    writeFile(s"$root/layouts/_default/api-module.html", template)
    writeFile(s"$root/layouts/_default/api-index.html", template)

    root
  }

  /** The HTML juicer wrote for one content page, wherever its URL layout put it.
   *
   * **Where juicer puts it depends on how the content is organised, so both spellings are tried.**
   * These pages are a *section* — `content/api/` with an `_index.md` — and come out under
   * `public/html/api/…`; a single loose page at `content/probe.md` comes out at
   * `public/probe/index.html` with no `html/` segment at all, measured by `SlugConformanceTests`
   * beside this file against the same `baseConfig`. So the segment is not something the base config
   * always does, and a helper that assumed either spelling would be wrong for the other suite.
   *
   * Asserting one hardcoded path made a working render look like a broken one for a round trip,
   * which is why this names what it looked for rather than throwing a `NoSuchFileException`.
   */
  private def rendered(root: String, page: String): String = {
    val candidates = List(s"$root/public/html/$page", s"$root/public/$page")

    candidates.find(isFile).map(readFile).getOrElse {
      fail(s"no rendered page for '$page' — looked at ${candidates.mkString(", ")}")
    }
  }

  "the argument parser" - {

    "defaults to the working directory and docs/api" in {
      val opts = DocCli.parse(Nil).toOption.get

      opts.dir shouldBe "."
      opts.out shouldBe "docs/api"
      opts.check shouldBe false
      opts.site shouldBe None
    }

    "takes the tree as a positional argument" in {
      DocCli.parse(List("library")).toOption.get.dir shouldBe "library"
    }

    "takes -o and --out alike" in {
      DocCli.parse(List("-o", "x")).toOption.get.out shouldBe "x"
      DocCli.parse(List("--out", "y")).toOption.get.out shouldBe "y"
    }

    "takes -n and --note alike, and has none by default" in {
      DocCli.parse(List("-n", "hello")).toOption.get.note shouldBe Some("hello")
      DocCli.parse(List("--note", "hello")).toOption.get.note shouldBe Some("hello")
      DocCli.parse(Nil).toOption.get.note shouldBe None
    }

    "names --note when its value is missing, as it does every other valued flag" in {
      // The missing-value list is written out by hand, so a flag added to the parser and not to
      // that list fails as "unknown option '--note'" — which reads as a flag that does not exist.
      DocCli.parse(List("--note")).left.toOption.get should include("'--note' needs a value")
    }

    "takes -w and --weight alike, and has none by default" in {
      DocCli.parse(List("-w", "45")).toOption.get.weight shouldBe Some(45)
      DocCli.parse(List("--weight", "45")).toOption.get.weight shouldBe Some(45)
      DocCli.parse(Nil).toOption.get.weight shouldBe None
    }

    "refuses a --weight that is not a number, and says what it got" in {
      // Carried as an Int rather than a string precisely so this can be refused. A frontmatter
      // `weight: nine` is not an error anywhere downstream — the site reads it as nothing and puts
      // the section wherever the unweighted default lands, which is the state this flag exists to
      // end. Failing here is the only place it can be noticed.
      val message = DocCli.parse(List("--weight", "nine")).left.toOption.get

      message should include("'--weight' needs a whole number")
      message should include("nine")
    }

    "names --weight when its value is missing, as it does every other valued flag" in {
      DocCli.parse(List("--weight")).left.toOption.get should include("'--weight' needs a value")
    }

    "takes --site, and has none by default" in {
      DocCli.parse(List("--site", "docs")).toOption.get.site shouldBe Some("docs")
      DocCli.parse(Nil).toOption.get.site shouldBe None
    }

    "names --site when its value is missing, as it does every other valued flag" in {
      DocCli.parse(List("--site")).left.toOption.get should include("'--site' needs a value")
    }

    "reads the flags that take no value" in {
      val opts = DocCli.parse(List("--private", "--check")).toOption.get

      opts.includePrivate shouldBe true
      opts.check shouldBe true
    }

    "names the flag when its value is missing, rather than complaining about the end of input" in {
      DocCli.parse(List("--out")).left.toOption.get should include("'--out' needs a value")
    }

    "refuses an unknown option" in {
      DocCli.parse(List("--nonsense")).left.toOption.get should include("unknown option '--nonsense'")
    }

    "answers the usage text for --help" in {
      DocCli.parse(List("--help")).left.toOption.get should startWith("sysl-doc — ")
    }
  }

  "the exit code" - {

    "is 0 when --help was asked for, because asking for help is not a mistake" in {
      DocCli.run(List("--help")) shouldBe 0
    }

    "is 1 for an unknown option" in {
      DocCli.run(List("--nonsense")) shouldBe 1
    }

    "is 1 for a tree that holds no sysl" in {
      DocCli.run(List("target/doc-cli-tests/nothing-here")) shouldBe 1
    }

    "is 0 after a successful generate" in {
      // The one that was wrong: the command wrote all 27 modules, printed its success line, and
      // exited 1. A shell script reading `$?` would have called a good run a failure.
      DocCli.run(List("library", "--out", out("generate"))) shouldBe 0
    }

    "is 0 when --check finds the pages up to date" in {
      val dir = out("check-fresh")

      DocCli.run(List("library", "--out", dir)) shouldBe 0
      DocCli.run(List("library", "--out", dir, "--check")) shouldBe 0
    }

    "is 1 when --check finds nothing generated at all" in {
      DocCli.run(List("library", "--out", out("check-missing"), "--check")) shouldBe 1
    }

    "is 1 when --check finds a page that has drifted" in {
      // The whole point of the flag: a doc comment edited without regenerating fails the build.
      val dir = out("check-stale")

      DocCli.run(List("library", "--out", dir)) shouldBe 0

      io.github.edadma.cross_platform.writeFile(s"$dir/sysl-text.md", "stale\n")

      DocCli.run(List("library", "--out", dir, "--check")) shouldBe 1
    }

    "is 1 when --check finds the page of a module that no longer exists" in {
      // A renamed or removed module leaves its page behind, documenting an API nobody can import.
      // A check that only compared the pages it would write could never see it.
      val dir = out("check-orphan")

      DocCli.run(List("library", "--out", dir)) shouldBe 0

      writeFile(s"$dir/sysl-gone.md", readFile(s"$dir/sysl-text.md"))

      DocCli.run(List("library", "--out", dir, "--check")) shouldBe 1
    }

    "regenerating removes that page, so the check it failed can pass" in {
      val dir = out("regen-orphan")

      DocCli.run(List("library", "--out", dir)) shouldBe 0

      writeFile(s"$dir/sysl-gone.md", readFile(s"$dir/sysl-text.md"))

      DocCli.run(List("library", "--out", dir)) shouldBe 0
      isFile(s"$dir/sysl-gone.md") shouldBe false
      DocCli.run(List("library", "--out", dir, "--check")) shouldBe 0
    }

    "a hand-written page beside the generated ones is neither stale nor removed" in {
      val dir = out("hand-written")

      DocCli.run(List("library", "--out", dir)) shouldBe 0

      writeFile(s"$dir/notes.md", "---\ntitle: notes\n---\n\nwritten by hand\n")

      DocCli.run(List("library", "--out", dir, "--check")) shouldBe 0
      DocCli.run(List("library", "--out", dir)) shouldBe 0
      isFile(s"$dir/notes.md") shouldBe true
    }
  }

  "a doc comment the compiler would refuse" - {

    "is reported in the compiler's words, and the pages are still written" in {
      val tree = "target/doc-cli-tests/param-tree"

      sh.sysl.Project.discard(tree)
      sh.sysl.Project.makeDirectories(tree)
      writeFile(s"$tree/q.sysl",
        "module demo.q\n\n/** Bits.\n * @param a bits\n * @param nope not one\n */\nbits(a: int) -> int = a\n")

      val err  = new java.io.ByteArrayOutputStream
      val dir  = out("param-out")
      val code = Console.withErr(err)(DocCli.run(List(tree, "--out", dir)))

      code shouldBe 0
      err.toString should include("q.sysl:")
      err.toString should include("warning: 'nope' is not a parameter of this declaration — it has a")
      readFile(s"$dir/demo-q.md") should not include "nope"
    }
  }

  /** The `--site` path: generate, then render with juicer, in one command.
   *
   * **Nothing exercised this before card `0290`, and that is why the defect it records existed.**
   * `sysl.sh` renders by invoking the juicer *CLI*, a separate artifact at its own version; `--site`
   * calls **juicer-core as a library**, at whatever `build.sbt` pins. Two renderers over the same
   * pages, and only the one nobody ships was covered.
   *
   * The last case is the one that bites. `MarkdownWriter` writes `slugStyle: github` into every
   * generated page's frontmatter — card `0258`'s mechanism for keeping generated anchors off the
   * site's default slugging — and **per-page `slugStyle` arrived in juicer 0.4.1** (`95ae3d0`,
   * *"Make slugStyle overridable per page, and cut 0.4.1"*). A juicer-core older than that ignores
   * the key, slugs with its default, and every link on every generated page lands at the top of the
   * right page with nothing complaining anywhere.
   *
   * **It is a different claim from `SlugConformanceTests`', one layer up.** That suite proves the two
   * *algorithms* agree, driving `SiteRenderer.build` over a page it built itself. This drives the
   * **command**, from argv to HTML, and so proves the wiring as well: that `--site` reaches the
   * renderer at all, with the pages `-o` just wrote, carrying the frontmatter that decides their ids.
   */
  "the --site path" - {

    "refuses pages written outside the site it was asked to build, naming both" in {
      // The guard exists because generating into one place and building another would otherwise
      // produce a site that silently lacks the pages — which looks like a theme problem.
      val root = site("site-outside")

      DocCli.run(List("library", "--out", "target/doc-cli-tests/elsewhere", "--site", root)) shouldBe 1
    }

    "is 0 when the pages are written inside the site and it builds" in {
      val root = site("site-builds")

      DocCli.run(List("library", "--out", s"$root/content/api", "--site", root)) shouldBe 0

      rendered(root, "api/index.html") should include("<html>")
    }

    "renders the generated anchors as the ids the pages link to" in {
      // Card 0290. A juicer-core that does not know the per-page `slugStyle` key renders these with
      // its default slugger — which keeps no underscore and collapses runs — so `from_utf8_lossy`
      // comes out as `from-utf8-lossy` while the page's own index links to `#from_utf8_lossy`. The
      // render still succeeds and the exit code is still 0, which is why the case above cannot see
      // it and this one has to read the HTML.
      val root = site("site-anchors")

      DocCli.run(List("library", "--out", s"$root/content/api", "--site", root)) shouldBe 0

      val html = rendered(root, "api/sysl-text/index.html")
      val ids  = """<h3[^>]*\bid="([^"]*)""".r.findAllMatchIn(html).map(_.group(1)).toList

      withClue("the renderer ignored the page's slugStyle — juicer-core older than 0.4.1: ") {
        ids should contain("from_utf8_lossy")
      }
    }
  }

  "reading a tree" - {

    "answers every module of the standard library" in {
      val modules = DocCli.read("library", includePrivate = false).toOption.get

      modules.map(_.name) should contain("sysl.text")
      modules.map(_.name) should contain("sysl.slices")
      modules.length should be >= 20
    }

    "refuses a directory that holds no sysl at all, naming it" in {
      DocCli.read("target/doc-cli-tests/absent", includePrivate = false)
        .left.toOption.get should include("holds no sysl source")
    }
  }
}
