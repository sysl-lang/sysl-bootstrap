package sh.sysl

/** What the caller asked the root project for, before anything has been resolved.
 *
 * The three are separate rather than one value because they answer three different questions and a
 * caller may say any combination of them: `features` adds, `noDefaultFeatures` takes the one a
 * package ships with away, and `allFeatures` says that whatever the manifest declares is wanted. A
 * single "the features" list could not distinguish *I asked for nothing* from *I asked for none of
 * them*, which is exactly the distinction `default` turns on.
 */
case class FeatureRequest(
    features: List[String] = Nil,
    noDefaultFeatures: Boolean = false,
    allFeatures: Boolean = false,
) {

  /** Whether the caller said nothing at all, which is what lets a command decide for itself.
   *
   * `sysl test` on the root enables everything the manifest declares, because gated code that the
   * ordinary gate never compiles is gated code nobody is testing — but only where the caller has not
   * said otherwise, since somebody naming a feature explicitly is asking to test that configuration
   * rather than asking for the default to be widened.
   */
  def silent: Boolean = features.isEmpty && !noDefaultFeatures && !allFeatures
}

/** Which features of which package are on, worked out across the whole dependency graph.
 *
 * ==Unification is a union, and that is what makes it a fixpoint==
 *
 * A package reached by two consumers gets the features **both** of them asked for, because each is
 * building against a surface it believes is there and neither can be told it was wrong. So enabling
 * a feature can pull an optional dependency into the graph, that dependency is a consumer of its own
 * with its own requests, and those requests can enable a feature somewhere the first pass had
 * already settled. Nothing here can be decided in one walk.
 *
 * The answer is reached by running the resolution repeatedly against the enabled sets the previous
 * round produced, until a round changes nothing. It terminates because every round can only add —
 * to the packages in the graph, and to the features enabled on each — so the whole thing is a
 * monotone climb through a finite lattice. The bound below is not what makes it terminate; it is
 * what makes a defect in that reasoning fail loudly rather than spin.
 */
object FeatureResolution {

  /** How many rounds the climb is allowed before the resolution gives up and says so.
   *
   * Far above what any real graph needs — a round is only spent when one round's answer *changed*
   * the next one's, so the count is bounded by the depth at which a feature turns on a dependency
   * that turns on a feature, not by the size of the graph.
   */
  val Rounds: Int = 64

  /** The features a package's own manifest turns on, following feature-to-feature edges.
   *
   * Only names the manifest declares survive, so a member that is an optional dependency's label
   * rather than a feature drops out here and is read back by `active` instead. That keeps the
   * answer a set of *features*, which is what a consumer of this result is expecting to be handed.
   *
   * `depLabels` names this package's own dependency labels: a member of a feature's list that is
   * ALSO one of them names the dependency rather than a same-named feature, so it is never expanded
   * into its own feature edges — only `asked`, which always names features (a CLI request or a
   * consumer's own `features = […]`, never a member inside a `features { … }` list), is exempt from
   * this check.
   */
  def closure(declared: Map[String, List[String]], asked: Iterable[String],
             depLabels: Set[String] = Set.empty): Set[String] = {
    @annotation.tailrec
    def walk(queue: List[String], seen: Set[String]): Set[String] = queue match
      case Nil                                                  => seen
      case name :: rest if seen(name) || !declared.contains(name) => walk(rest, seen)
      case name :: rest =>
        walk(declared(name).filterNot(depLabels) ::: rest, seen + name)

    walk(asked.toList, Set.empty)
  }

  /** The root project's own enabled set, from what the caller asked for.
   *
   * `default` is an ordinary feature that happens to be the one asking for nothing gets, so it is
   * added to the asked-for list rather than handled by a branch of its own — and a manifest that
   * declares no `default` simply has nothing to add, which is why an absent one is not an error
   * while a misspelled explicit request is.
   */
  def rootEnabled(config: PackageConfig, request: FeatureRequest, testing: Boolean)
      : Either[String, Set[String]] =
    if request.allFeatures || (testing && request.silent) then Right(config.features.keySet.toSet)
    else
      request.features.find(!config.features.contains(_)) match
        case Some(unknown) =>
          Left(s"this project has no feature '$unknown' — a feature has to be declared in " +
            s"${PackageConfig.FileName}'s 'features' block before it can be asked for" +
            (if config.features.isEmpty then ", and this manifest declares none"
             else s". It declares ${config.features.keys.map(n => s"'$n'").mkString(", ")}"))
        case None =>
          val asked = request.features ::: (if request.noDefaultFeatures then Nil else List("default"))

          Right(closure(config.features, asked, config.dependencies.map(_.label).toSet))

  /** The dependencies of one package that are in the graph, given what that package has enabled.
   *
   * An entry that is not `optional` is always one of them; an optional one is there exactly when
   * some enabled feature names its **label**. The filtering happens before anything is fetched, so
   * an optional dependency nothing turned on is not downloaded, not checked for the libraries its
   * manifest requires, and contributes nothing to the link line — which is the whole of what
   * `optional` is for.
   */
  def active(config: PackageConfig, enabled: Set[String]): List[Dependency] = {
    val turnedOn = enabled.flatMap(config.features.getOrElse(_, Nil))

    config.dependencies.filter(dep => !dep.optional || turnedOn(dep.label))
  }

  /** What every package in a resolved graph has enabled, from the requests its consumers wrote.
   *
   * A dependency's set is the union over every consumer of the features that consumer named, plus
   * `default` unless **every** consumer turned it off. The asymmetry is deliberate and is the same
   * argument as the union: one consumer saying `default_features = false` is saying what *it* does
   * not need, and it does not get to take a feature away from a sibling that asked for the package
   * the ordinary way.
   *
   * The root is left out, since what it has enabled is the caller's answer rather than anybody's
   * request, and is supplied by `rootEnabled`.
   */
  def enabledFrom(packages: List[ResolvedPackage]): Either[String, Map[String, Set[String]]] = {
    val byName  = packages.map(p => p.canonical -> p).toMap
    val asked   = packages.flatMap(p => p.config.dependencies.map(consumer(p) -> _))
    val grouped = asked.groupBy(_._2.canonical)

    def unify(name: String, requests: List[(String, Dependency)]): Either[String, Set[String]] =
      byName.get(name) match
        case None => Right(Set.empty)
        case Some(target) =>
          val declared = target.config.features
          val unknown  = requests.flatMap((who, dep) =>
            dep.features.filter(!declared.contains(_)).map((who, dep, _)))

          unknown.headOption match
            case Some((who, dep, feature)) => Left(noSuchFeature(who, dep, feature, declared))
            case None                      =>
              val named   = requests.flatMap(_._2.features)
              val wantsUp = requests.exists(_._2.defaultFeatures)

              Right(closure(declared, named ::: (if wantsUp then List("default") else Nil),
                target.config.dependencies.map(_.label).toSet))

    collect(grouped.toList.sortBy(_._1))((name, requests) => unify(name, requests).map(name -> _))
      .map(_.toMap)
  }

  /** How a consumer is named in a diagnostic: a dependency by its coordinate, the root by itself. */
  private def consumer(p: ResolvedPackage): String =
    if p.isRoot then "this project" else p.canonical

  private def noSuchFeature(who: String, dep: Dependency, feature: String,
                            declared: Map[String, List[String]]): String =
    s"$who asks '${dep.label}' for the feature '$feature', which '${dep.canonical}' does not " +
      "declare — a consumer may only ask for a feature that package's own manifest names" +
      (if declared.isEmpty then ", and it declares none"
       else s". It declares ${declared.keys.map(n => s"'$n'").mkString(", ")}")

  private def collect[A, B](items: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    items.foldLeft(Right(Nil): Either[String, List[B]]) { (acc, item) =>
      for
        out  <- acc
        next <- f(item)
      yield out :+ next
    }
}
