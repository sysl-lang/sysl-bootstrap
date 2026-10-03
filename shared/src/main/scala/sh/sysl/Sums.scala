package sh.sysl

/** `sysl.sum` — what each resolved package's source was when it was first seen
 * (`reference/packages.md § sysl.sum`).
 *
 * It sits beside `package.hocon`, is committed, and is checked on every fetch that populates the
 * cache. **It is not a lockfile**, and the difference is worth keeping straight: Minimal Version
 * Selection has already made the *version* a pure function of the manifests (`§ 5`), so there is
 * nothing here recording what was chosen. What this pins is *content* — a tag moved to point at
 * different commits, a repository rewritten, a mirror serving something other than what the author
 * published. A version number cannot describe any of those, because in all three the version number
 * is exactly what it was.
 *
 * A **local** dependency has no entry. A directory beside the consumer is expected to change — that
 * is what it is for — so a hash of it would be a promise broken by the next keystroke.
 *
 * The first fetch of a package that no entry covers is trusted and recorded, which is what `go.sum`
 * does and carries the same caveat: it establishes what was there, not that what was there was
 * right. Reviewing the diff a new entry makes is the part a person has to do.
 */
case class Sums(entries: Map[(String, Version), String]) {

  def hashOf(coordinate: String, version: Version): Option[String] =
    entries.get((coordinate, version))

  def recording(coordinate: String, version: Version, hash: String): Sums =
    Sums(entries + ((coordinate, version) -> hash))

  /** The file's text: one entry per line, sorted, so that two runs that resolved the same graph
   * write the same bytes and a diff shows what changed rather than how a map was iterated.
   */
  def render: String =
    entries.toList.sortBy((key, _) => (key._1, key._2)).map { case ((coordinate, version), hash) =>
      s"$coordinate ${version.tag} $hash\n"
    }.mkString
}

object Sums {

  val FileName: String = "sysl.sum"

  val empty: Sums = Sums(Map.empty)

  /** Reads the file, or says which line is wrong with it.
   *
   * Text in and a value out, with finding the file left to the driver — the same split
   * `PackageConfig` takes, and for the same reason: what the file *means* is then a pure function
   * that every platform runs identically and a test can ask about directly.
   *
   * **A line that cannot be read is an error rather than a line skipped.** This file exists to
   * refuse things; one that quietly ignored what it could not parse would answer "no entry covers
   * this package" for a package it was carrying an entry for, and trust something it had been told
   * to check.
   */
  def read(text: String): Either[String, Sums] = {
    val lines = text.linesIterator.zipWithIndex.filter((line, _) => line.trim.nonEmpty).toList

    val parsed = lines.map { (line, i) =>
      val at = s"$FileName:${i + 1}"

      line.trim.split("\\s+").toList match
        case List(coordinate, tag, hash) =>
          for
            _ <- Either.cond(tag.startsWith("v"), (), s"$at: '$tag' is not a version tag")
            v <- Version.parse(tag.drop(1)).left.map(e => s"$at: $e")
            _ <- Either.cond(isDigest(hash), (), s"$at: '$hash' is not a ${Hashing.Prefix} digest")
          yield (coordinate, v) -> hash

        case _ => Left(s"$at: a line is a coordinate, a version tag and a digest")
    }

    parsed.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None    =>
        val ok = parsed.collect { case Right(entry) => entry }

        // Two lines for one package disagreeing is a file that has been merged badly or edited by
        // hand, and there is no reading of it under which one of the two is the right answer.
        ok.groupBy(_._1).collectFirst { case (key, group) if group.map(_._2).distinct.length > 1 =>
          s"$FileName: '${key._1} ${key._2.tag}' is recorded with two different digests"
        }.toLeft(Sums(ok.toMap))
  }

  /** What `sysl tidy` leaves of a file: `text` as it stands, cut down to the packages in `needed`.
   *
   * `current` is the sums after resolving, so it holds every line `text` held plus one for anything
   * resolving had to fetch that no line covered. Answered as the new text, what was dropped and what
   * was added, the last two sorted the way the file is.
   *
   * **A line that stays is the line that was there, byte for byte and in its place**, so a file
   * nothing was added to is the old one with lines taken out — a diff of it shows removals and
   * nothing else. Where a line has to be added, the file is rendered whole instead, which is exactly
   * the file a build would have written on adding it; a build always writes the rendered form, so
   * for any file a build wrote the two agree on every line that stays.
   *
   * A file with nothing left in it is the empty string, and the caller deletes it rather than
   * writing it: a build that resolves no package never writes one, so no file at all is what a
   * project depending on nothing looks like.
   */
  def tidied(text: String, current: Sums, needed: Set[(String, Version)])
      : (String, List[(String, Version)], List[(String, Version)]) = {
    val lines = text.linesIterator.filter(_.trim.nonEmpty).toList

    def keyOf(line: String): Option[(String, Version)] =
      line.trim.split("\\s+").toList match
        case List(coordinate, tag, _) => Version.parse(tag.drop(1)).toOption.map(coordinate -> _)
        case _                        => None

    val had     = lines.flatMap(keyOf).toSet
    val removed = (had -- needed).toList.sortBy(key => (key._1, key._2))
    val added   = (needed -- had).filter(current.entries.contains).toList.sortBy(key => (key._1, key._2))

    val out =
      if added.isEmpty then lines.filter(l => keyOf(l).exists(needed.contains)).distinct.map(_ + "\n").mkString
      else Sums(current.entries.filter((key, _) => needed.contains(key))).render

    (out, removed, added)
  }

  private def isDigest(s: String): Boolean =
    s.startsWith(Hashing.Prefix) && {
      val hex = s.drop(Hashing.Prefix.length)

      hex.length == 64 && hex.forall(c => c.isDigit || (c >= 'a' && c <= 'f'))
    }
}
