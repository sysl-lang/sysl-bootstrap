package sh.sysl.doc

import io.github.edadma.cross_platform.*
import sh.sysl.*

/** `sysl-doc` — the API reference generator's command line.
 *
 * **A separate binary, reached as `sysl doc` through the git-style dispatch card 0257 built.** The
 * reason is the dependency profile rather than novelty: this links a static site generator, a
 * templating engine, an asset pipeline and a web server, and none of that belongs inside a systems
 * compiler. scaladoc sits beside scalac and rustdoc beside rustc for the same reason.
 *
 * **What it does in one line:** parse a tree, build the module model, write Markdown — and, if asked,
 * hand that Markdown to juicer to render as a site.
 *
 * **The Markdown is the artifact and the site is the step after it.** That ordering is the whole of
 * decision 2 on the card: a `docs/` folder of generated Markdown in a package repository is readable
 * on GitHub with no tooling, no hosting and no juicer, which is what makes this work for fifty
 * package repos rather than for the one site somebody hosts.
 */
object DocCli {

  /** Everything the command was asked for. */
  case class Options(
      dir: String = ".",
      out: String = "docs/api",
      title: String = "API reference",
      note: Option[String] = None,
      weight: Option[Int] = None,
      version: Option[String] = None,
      site: Option[String] = None,
      includePrivate: Boolean = false,
      check: Boolean = false,
  )

  private val Usage =
    """sysl-doc — API reference from declarations and their doc comments
      |
      |Usage: sysl-doc [options] [<dir>]
      |
      |  <dir>              the tree to document (default: the working directory)
      |
      |  -o, --out <dir>    where the Markdown goes (default: docs/api)
      |  -t, --title <text> the index page's title (default: "API reference")
      |  -n, --note <text>  a line of Markdown under the index title, above the table —
      |                     where a site links back to its hand-written pages
      |  -w, --weight <n>   the index page's place in the site's navigation. Omitted from
      |                     the frontmatter entirely when not given
      |  -V, --docversion <v>  the version being documented, shown on the pages
      |      --site <dir>   after writing, build the juicer site rooted at <dir>
      |                     (<dir> must be a juicer site, and -o must be inside it)
      |      --private      include file- and module-private declarations
      |      --check        write nothing; exit 1 if the committed Markdown is stale
      |  -h, --help         print this
      |
      |The Markdown is the artifact. It is readable in the repository with no tooling at
      |all, which is what --check exists to keep true: run it in CI and a doc comment
      |edited without regenerating fails the build.
      |""".stripMargin

  /** Parse the argument list. `Left` is a message and a exit code's worth of intent. */
  def parse(args: List[String]): Either[String, Options] = {
    def loop(rest: List[String], opts: Options): Either[String, Options] = rest match
      case Nil => Right(opts)

      case ("-h" | "--help") :: _ => Left(Usage)

      case ("-o" | "--out") :: v :: t          => loop(t, opts.copy(out = v))
      case ("-t" | "--title") :: v :: t        => loop(t, opts.copy(title = v))
      case ("-n" | "--note") :: v :: t         => loop(t, opts.copy(note = Some(v)))

      // Rejected here rather than carried as a string, so `--weight nine` fails saying so instead
      // of writing a frontmatter key the site reads as nothing and silently ignores.
      case ("-w" | "--weight") :: v :: t =>
        v.toIntOption match
          case Some(n) => loop(t, opts.copy(weight = Some(n)))
          case None    => Left(s"sysl-doc: '--weight' needs a whole number, got '$v'")
      case ("-V" | "--docversion") :: v :: t   => loop(t, opts.copy(version = Some(v)))
      case "--site" :: v :: t                  => loop(t, opts.copy(site = Some(v)))
      case "--private" :: t                    => loop(t, opts.copy(includePrivate = true))
      case "--check" :: t                      => loop(t, opts.copy(check = true))

      // A flag that takes a value and was given none. Saying which flag beats "unexpected end of
      // input", which is what a positional fallthrough would produce here.
      case (f @ ("-o" | "--out" | "-t" | "--title" | "-n" | "--note" | "-w" | "--weight" |
          "-V" | "--docversion" | "--site")) :: Nil =>
        Left(s"sysl-doc: '$f' needs a value after it")

      case a :: _ if a.startsWith("-") => Left(s"sysl-doc: unknown option '$a'\n\n$Usage")

      case a :: t => loop(t, opts.copy(dir = a))

    loop(args, Options())
  }

  /** Read a tree and answer its modules.
   *
   * **`SyslParser.checked` is the entry point, and reaching for `Literate.tangle` first is a trap
   * that costs a confusing hour.** A literate source's prose does have to be stripped before the
   * lexer sees it — that is the card's decision 1 — but `checked` already does it, along with the
   * conditional-compilation gate. Tangling here as well strips the indent a **second** time, and
   * what comes back is a file whose declarations have lost their block structure:
   *
   * ```
   * error: newline expected
   *   --> library/sysl/regex/ast.lsysl:42:9
   *    |
   * 42 | Star(inner: &Node)
   * ```
   *
   * Fifty-eight of those, none of which names indentation or literate anything.
   *
   * **The precise line decision 1 draws still holds**: a doc comment written *inside* the code
   * blocks survives tangling and is read, and only the un-indented narrative Markdown is dropped.
   *
   * **The consequence to accept rather than fix**: a literate module whose author explained
   * everything in prose produces bare signatures here. That is correct. They chose the essay form,
   * `weave` serves it, and a thin API page is the honest answer rather than a defect.
   */
  def read(dir: String, includePrivate: Boolean): Either[String, List[ApiModel.Module]] = {
    // `Project.collect` throws a `NoSuchFileException` on a directory that is not there rather than
    // answering an empty list, so the sentence below was unreachable for the commonest mistake there
    // is — a mistyped path. A stack trace is not an answer to that.
    val sources =
      try Project.collect(dir, None)
      catch { case _: Exception => Nil }

    if sources.isEmpty then Left(s"sysl-doc: $dir holds no sysl source")
    else
      val parsed = sources.map(SyslParser.checked(_))

      parsed.collectFirst { case Left(ds) => ds } match
        case Some(ds) => Left(ds.map(_.rendered).mkString("\n"))
        case None =>
          val units   = parsed.collect { case Right(p) => p }
          val modules = ApiModel.build(units, includePrivate)

          // A doc comment the compiler would refuse — a `@param` naming no parameter — is said here
          // in the compiler's own words, since the page leaves that row out and the author would
          // otherwise never learn why. A warning rather than a refusal: a tree that does not compile
          // still documents, which is the property the model is built from parsed units to keep.
          for
            u              <- units if !u.testOnly && u.module.isDefined
            (tag, message) <- DocComments.problems(u)
          do Console.err.println(s"${u.source.name}:${tag.line}: warning: $message")

          // A tree of programs rather than modules documents to nothing, and saying so beats writing
          // an empty index — the same call `weave` makes about a tree with no literate source in it.
          if modules.isEmpty then
            Left(s"sysl-doc: nothing in $dir declares a module, so there is no API to document — " +
              "a program's entry file is a body rather than an importable surface")
          else Right(modules)
  }

  /** Run the command. Answers the process exit code. */
  def run(args: List[String]): Int = parse(args) match
    case Left(message) =>
      // `--help` comes back the same way an error does and must not exit non-zero.
      if message.startsWith("sysl-doc — ") then { println(message); 0 }
      else { Console.err.println(message); 1 }

    case Right(opts) =>
      read(opts.dir, opts.includePrivate) match
        case Left(err) => Console.err.println(err); 1

        case Right(modules) =>
          val pages = MarkdownWriter.pages(modules, opts.title, opts.version, opts.note, opts.weight)

          if opts.check then check(opts.out, pages)
          else
            write(opts.out, pages, modules.length) match
              case 0    => opts.site.map(site => render(opts.out, site)).getOrElse(0)
              case code => code

  /** Write the pages, and report what was written. */
  private def write(out: String, pages: List[MarkdownWriter.Page], modules: Int): Int =
    // **The braces are load bearing and this is where that was learned.** Written as
    // `catch case e: Exception => println(…); 1`, the `; 1` is NOT the catch body's second statement
    // — it becomes a second statement of the METHOD body, so the whole method answers 1 whatever the
    // try produced. This command wrote all 27 modules, printed its success line and exited 1, with no
    // exception thrown and no error printed; nothing but reading the returned number finds it.
    try {
      Project.makeDirectories(out)
      pages.foreach(p => writeFile(s"$out/${p.path}", p.text))
      orphans(out, pages).foreach { f =>
        deleteFile(f)
        println(s"sysl-doc: removed $out/${f.replace('\\', '/').split('/').last}, whose module is gone")
      }
      println(s"sysl-doc: $modules module${if modules == 1 then "" else "s"} -> $out")
      0
    } catch {
      case e: Exception =>
        Console.err.println(s"sysl-doc: cannot write $out: ${e.getMessage}")
        1
    }

  /** Compare what would be written against what is there, and write nothing.
   *
   * **This is what makes committed documentation honest.** Decision 4 on the card is that a package's
   * generated pages are committed — which is what the whole GitHub-renders-Markdown argument rests on
   * — and committed generated files go stale. Run this in CI and a doc comment edited without
   * regenerating fails the build, which is the usual answer and the only one that works.
   *
   * It names every file that differs rather than stopping at the first, because the fix is one
   * command and a reader wants to know the size of what it will change.
   */
  private def check(out: String, pages: List[MarkdownWriter.Page]): Int = {
    val stale =
      pages.filter { p =>
        val path = s"$out/${p.path}"

        !isFile(path) || readFile(path) != p.text
      }.map(_.path)

    // A page left behind by a module that was renamed or removed is as stale as one that drifted:
    // it documents an API nobody can import, and a check that only compared the pages it would
    // write could never see it.
    val left = orphans(out, pages).map(f => f.replace('\\', '/').split('/').last)
    val all  = stale ::: left

    if all.isEmpty then { println(s"sysl-doc: $out is up to date"); 0 }
    else
      Console.err.println(s"sysl-doc: $out is stale — ${all.length} file(s) differ:")
      stale.foreach(p => Console.err.println(s"  $p"))
      left.foreach(p => Console.err.println(s"  $p (its module is gone)"))
      Console.err.println("\nRegenerate with 'sysl doc' and commit the result.")
      1
  }

  /** Render the generated Markdown as a site, with juicer.
   *
   * **Only reached when `--site` is given**, so the ordinary run writes Markdown and stops. The site
   * is a convenience for somebody who wants to read their own docs in a browser; the artifact people
   * consume is the Markdown, in the repository. See `SiteRenderer` for why this builds a site that
   * already exists rather than making one.
   */
  /** The module pages in `out` this run would not write — each one the page of a module that no
   * longer exists.
   *
   * **Only a page this generator wrote counts**, told by the `layout: api-module` line its
   * frontmatter always carries, so a hand-written page kept beside the generated ones is never
   * reported, and never removed by a regenerate.
   */
  private def orphans(out: String, pages: List[MarkdownWriter.Page]): List[String] = {
    val written = pages.map(_.path).toSet

    val files =
      try if isDirectory(out) then listDirectory(out).toList.sorted else Nil
      catch case _: Exception => Nil

    files.filter { f =>
      val name = f.replace('\\', '/').split('/').last

      name.endsWith(".md") && !written(name) && isFile(f) && generated(readFile(f))
    }
  }

  /** Whether a file's frontmatter says it is a generated module page. */
  private def generated(text: String): Boolean =
    text.startsWith("---\n") && text.split("\n---\n", 2).head.linesIterator.contains("layout: api-module")

  private def render(markdown: String, site: String): Int =
    try {
      SiteRenderer.build(markdown, site)
      println(s"sysl-doc: site -> $site")
      0
    } catch {
      case e: Exception =>
        Console.err.println(s"sysl-doc: cannot build the site: ${e.getMessage}")
        1
    }
}

@main def syslDoc(args: String*): Unit = {
  val code = DocCli.run(processArgs(args).toList)

  processExit(code)
}
