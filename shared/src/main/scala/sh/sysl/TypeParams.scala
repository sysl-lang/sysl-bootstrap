package sh.sysl

/** What a `[T, U: Show, V = int]` list says, kept as one value because every generic declaration
  * parses the same list and hands all three parts to the node it builds. A parameter carrying
  * neither a bound nor a default is simply absent from both maps.
  *
  * **A parameter may stand for a value rather than a type** (`reference/generics.md § A parameter
  * may stand for a value`), written `[const N: usize]`. Those share `names` with the type
  * parameters, because they share one list, one namespace and one argument position — what marks
  * one out is an entry in `values` giving the type its argument must have. A value parameter
  * carries no bound (a bound is a trait, and a value does not implement one) and its default, where
  * it has one, is an expression rather than a type — kept in `defaults` beside the type
  * parameters' as the `ValueArgType` a use would have written, so every rule about defaults (the
  * arity, the suffix, the left-to-right fill) is one rule rather than two.
  */
case class TypeParams(
    names: List[String],
    bounds: Map[String, List[BoundRef]] = Map.empty,
    defaults: Map[String, TypeRef] = Map.empty,
    values: Map[String, TypeRef] = Map.empty,
    packs: Set[String] = Set.empty,
) {

  /** The same list with a `where` clause's bounds folded in.
    *
    * **A parameter may be bounded in both places and the two combine**, which is what every language
    * carrying the form does: `[T: Display] … where T: Eq` asks for both, in the order they were
    * written. Refusing the mixture would be a rule a reader has to be told rather than one they
    * would guess, and the clause exists for readability rather than to take the bracket's job away.
    *
    * A value parameter carries no bound — its `: usize` is the type its argument must have — so a
    * clause naming one is refused by `whereUnknown` below along with a name the declaration does not
    * have at all.
    */
  def withWhere(clause: List[WhereBound]): TypeParams =
    copy(bounds = clause.foldLeft(bounds) { (acc, w) =>
      acc.updated(w.name, acc.getOrElse(w.name, Nil) ::: w.bounds)
    })

  /** The first name a `where` clause bounds that this declaration cannot bound, and why — either it
    * declares no such parameter, or the one it declares stands for a value.
    *
    * Answered here rather than in the analyzer because the declaration's own parameter list is the
    * whole of what decides it, and the parser has both in hand at the moment the clause is read.
    */
  def whereUnknown(clause: List[WhereBound]): Option[String] = {
    val takes =
      if names.isEmpty then "it takes none"
      else "it takes " + names.map(n => s"'$n'").mkString(", ")

    clause.collectFirst {
      case w if values.contains(w.name) =>
        s"'${w.name}' stands for a value rather than a type, and a value implements no trait — " +
          "its 'const' declaration is where the type its argument must have is written"
      case w if !names.contains(w.name) =>
        s"this declaration has no type parameter '${w.name}', so a 'where' clause has nothing to " +
          s"bound — $takes"
    }
  }
}

object TypeParams {
  val none: TypeParams = TypeParams(Nil)
}

/** One `T: Display + Eq` of a `where` clause, before the clause is folded into a declaration's
  * bounds. Kept as its own shape rather than as a pair so that the fold below has something to name
  * when it reports a parameter the declaration does not have.
  */
case class WhereBound(name: String, bounds: List[BoundRef])

/** One entry of a `[…]` parameter list, before the list is folded into `TypeParams`. The two shapes
  * are kept apart here rather than in maps because the grammar reading them is what tells them
  * apart, and a fold that had to guess would be the ambiguity `const` exists to remove.
  */
sealed trait ParamSpec { def name: String }

case class TypeParamSpec(name: String, bounds: List[BoundRef], default: Option[TypeRef])
    extends ParamSpec

case class ValueParamSpec(name: String, typ: TypeRef, default: Option[Expr]) extends ParamSpec

/** `..A: Display` — a parameter standing for a **list** of types (`reference/generics.md § A
  * parameter may stand for a list of types`). Its bound distributes over the members, which is why
  * the bounds go in the same map a type parameter's do: everything downstream that asks what a name
  * is bounded by gets the same answer, and only the walk that matches a subject needs to know this
  * one is a pack.
  */
case class PackParamSpec(name: String, bounds: List[BoundRef]) extends ParamSpec
