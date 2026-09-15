package sh.sysl

import io.github.edadma.hocon.{ConfigArray, ConfigBoolean, ConfigObject, ConfigString, ConfigValue}

import scala.collection.immutable.ListMap

/** The `features` block of a manifest, and the four things a manifest can say wrongly about one.
 *
 * ==What a feature is==
 *
 * A feature is a name for a set of names: optional dependencies it turns on, and other features it
 * turns on in turn. `default` is an ordinary feature that happens to be the one a consumer gets when
 * it asks for nothing, so nothing here treats it specially — which is what keeps
 * `default_features = false` meaning exactly *do not turn that one on* rather than being a second
 * mechanism.
 *
 * A feature carries no capability clause and no `requires` of its own. What it selects is which
 * dependencies are in the graph; what those dependencies then ask of the host is the ordinary
 * question the manifest already answers.
 *
 * ==Why the reading and the checking are separate==
 *
 * The block is read off the file alone, and then checked against the dependencies the same file
 * declared, because three of the four refusals are about the *relationship* between the two blocks
 * and cannot be seen from either one. Keeping them apart also means a test can ask what a block
 * parses to without having to satisfy the cross-checks first.
 *
 * ==The four refusals==
 *
 *   - a feature names a dependency that is not `optional` — the dependency is already in the build,
 *     so the feature turns nothing on and somebody believes otherwise;
 *   - an optional dependency no feature names — nothing can ever reach it, so it is dead weight in
 *     the file rather than a package the project takes;
 *   - a feature names something that is neither a dependency label nor another feature — a typo,
 *     which would otherwise silently select nothing;
 *   - a cycle among features — following the implications never ends.
 *
 * The cross-checks are against `dependencies` only, not `dev_dependencies`: what a feature selects
 * is what a *consumer* of this package gets, and a dev dependency by construction never reaches one.
 */
object PackageFeatures {

  /** The declared features, in the order the file writes them.
   *
   * The order is kept because it is the order a diagnostic and `sysl deps` will list them in, and a
   * reader comparing a message against their own file should find them in the same sequence. The
   * HOCON parser hands back a `ListMap`, so this costs nothing beyond not sorting.
   */
  def read(root: ConfigObject): Either[String, Map[String, List[String]]] =
    root.fields.get("features") match
      case None => Right(ListMap.empty[String, List[String]])
      case Some(block: ConfigObject) =>
        PackageConfig
          .collect(block.fields.toList) { (name, value) =>
            memberList(s"features.$name", value).map(name -> _)
          }
          .map(pairs => ListMap.from(pairs))
      case Some(_) =>
        Left(s"${PackageConfig.FileName}: 'features' is not a block — it names each feature and the " +
          "things that feature turns on, as 'features { server = [llhttp] }'")

  /** Holds a `features` block to the `dependencies` block beside it. */
  def check(features: Map[String, List[String]], deps: List[Dependency]): Either[String, Unit] =
    for
      _ <- everyMemberResolves(features, deps)
      _ <- everyOptionalIsReached(features, deps)
      _ <- noCycles(features)
    yield ()

  /** A `true`/`false` field of a dependency entry, or the default where the entry is silent.
   *
   * A non-boolean is refused rather than warned about, for the reason `readDependency` refuses an
   * unknown key: somebody who wrote `optional = "yes"` believes they have said something, and a
   * build that ignored it would let them go on believing it.
   */
  private[sysl] def flag(sub: ConfigObject, key: String, where: String, orElse: Boolean)
      : Either[String, Boolean] =
    sub.fields.get(key) match
      case None                   => Right(orElse)
      case Some(ConfigBoolean(v)) => Right(v)
      case Some(_) =>
        Left(s"${PackageConfig.FileName}: '$where.$key' is not 'true' or 'false' — it is a yes or no " +
          "question, written without quotes")

  /** A list-of-names field of a dependency entry, or nothing where the entry is silent. */
  private[sysl] def names(sub: ConfigObject, key: String, where: String): Either[String, List[String]] =
    sub.fields.get(key) match
      case None        => Right(Nil)
      case Some(value) => memberList(s"$where.$key", value)

  /** One `[a, b]`, held to being a list of non-empty names.
   *
   * HOCON reads a bare `server` inside brackets as a string, so a feature list is written without
   * quotes and a quoted one means the same thing. What is refused is a list holding something that
   * is not a name at all — a number, a nested block — since neither could ever name a feature or a
   * dependency.
   */
  private def memberList(where: String, value: ConfigValue): Either[String, List[String]] =
    value match
      case ConfigArray(elements) =>
        PackageConfig.collect(elements) {
          case ConfigString(name) if name.trim.nonEmpty => Right(name.trim)
          case _ =>
            Left(s"${PackageConfig.FileName}: '$where' holds something that is not a name — a list " +
              "here names optional dependencies and other features, as '[llhttp, nghttp2]'")
        }
      case _ =>
        Left(s"${PackageConfig.FileName}: '$where' is not a list — what a feature turns on is written " +
          "in brackets, as '[llhttp, nghttp2]', even where there is one of it")

  /** Refusals (a) and (c): every name a feature lists is either another feature or an **optional**
   * dependency.
   *
   * The two are one walk because they are one question asked of one name, and splitting them would
   * read the same list twice to produce messages that differ only in which half of the answer was
   * missing.
   */
  private def everyMemberResolves(features: Map[String, List[String]], deps: List[Dependency])
      : Either[String, Unit] = {
    val byLabel = deps.map(d => d.label -> d).toMap

    PackageConfig
      .collect(features.toList) { (name, members) =>
        PackageConfig.collect(members) { member =>
          if features.contains(member) then Right(())
          else
            byLabel.get(member) match
              case Some(dep) if dep.optional => Right(())
              case Some(_) =>
                Left(s"${PackageConfig.FileName}: 'features.$name' names the dependency '$member', " +
                  "which is not optional — a feature turns an optional dependency on, and this one " +
                  "is taken whatever is asked for. Write 'optional = true' in that dependency's " +
                  "entry, or drop it from the feature")
              case None =>
                Left(s"${PackageConfig.FileName}: 'features.$name' names '$member', which is neither " +
                  "a dependency of this package nor a feature of it — a feature turns on things " +
                  s"this manifest declares, so '$member' would select nothing")
        }
      }
      .map(_ => ())
  }

  /** Refusal (b): an optional dependency no feature names.
   *
   * It is the direction that is silent otherwise. An unreachable optional dependency is fetched by
   * nothing and linked by nothing, so the project builds perfectly and the author is left believing
   * some feature turns it on — which is the same failure the unknown-key warning exists for, one
   * block over, and it is refused rather than warned about because the vocabulary is closed: every
   * name that could reach it is in this same file.
   */
  private def everyOptionalIsReached(features: Map[String, List[String]], deps: List[Dependency])
      : Either[String, Unit] = {
    val named = features.values.flatten.toSet

    deps.filter(d => d.optional && !named(d.label)).map(_.label) match
      case Nil => Right(())
      case unreached =>
        Left(s"${PackageConfig.FileName}: ${unreached.map(l => s"'$l'").mkString(", ")} " +
          s"${if unreached.length == 1 then "is an optional dependency" else "are optional dependencies"} " +
          "no feature names — an optional dependency is reached only by the feature that turns it on, " +
          "so nothing would ever build it. Name it in a 'features' entry, or drop its 'optional = true'")
  }

  /** Refusal (d): the implications have to reach an end.
   *
   * Walked from each feature in declaration order, so the cycle reported is the first one a reader
   * of the file would meet rather than whichever the map happened to hand over first. `done` keeps
   * the walk linear: a feature already proven to lead nowhere in a circle cannot start doing so.
   */
  private def noCycles(features: Map[String, List[String]]): Either[String, Unit] = {
    val done = scala.collection.mutable.Set.empty[String]

    def walk(name: String, path: List[String]): Either[String, Unit] =
      if path.contains(name) then Left(cycleMessage(path.dropWhile(_ != name) :+ name))
      else if done(name) then Right(())
      else
        PackageConfig
          .collect(features.getOrElse(name, Nil).filter(features.contains))(walk(_, path :+ name))
          .map { _ =>
            done += name
            ()
          }

    PackageConfig.collect(features.keys.toList)(walk(_, Nil)).map(_ => ())
  }

  private def cycleMessage(cycle: List[String]): String =
    val chain = cycle.tail.map(n => s"'$n'").mkString(s"'${cycle.head}' turns on ", ", which turns on ", "")

    s"${PackageConfig.FileName}: the features here turn each other on — $chain. A feature may imply " +
      "another, but following the implications has to reach an end"
}
