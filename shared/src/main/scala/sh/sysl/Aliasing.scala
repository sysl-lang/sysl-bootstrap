package sh.sysl

import scala.collection.mutable

/** What a struct's `invariant` clauses (`reference/errors.md § Struct invariants`) demand of the
 * aliases a program makes.
 *
 * A clause is discharged by re-checking it at the write, and the write is found by walking outward
 * through the **place** being written — so the whole obligation rests on the place still naming the
 * struct. A pointer is where that runs out: `&o.a` is a `*Inner`, and an `Inner` knows nothing of
 * the `Outer` whose clause reads `a.n`, so a write through it breaks a promise nothing is left to
 * check. No amount of checking at the write closes this; it is a rule about **what may be aliased**.
 *
 * The rule restricts alias *creation* rather than alias *use*, which is what keeps it a local,
 * type-only question and not the borrow checker `03` rejects. An alias is safe when its type still
 * carries every promise over the memory it can reach, and the three ways that is arranged are here:
 * the walk that finds what a write owes (`invCheckFor`), the walk that finds what an alias would
 * sever (`severing`), and the clause's own read set, which is what makes the refusal precise enough
 * to leave `&o.c` alone when no clause mentions `c`.
 *
 * Nothing here costs a program that declares no invariants anything: every entry point answers
 * `Nil` or `false` before it looks at a clause.
 */
trait Aliasing extends RefBindings {

  /** Whether a struct type carries clauses this machinery has to honour. */
  protected def carriesInvariants(s: Type.Struct): Boolean =
    structDecls.get(s.base).exists(_.invariants.nonEmpty)

  /** The instantiations whose field types have been held to the read rule, so each is asked once. */
  private val readsChecked = mutable.Set.empty[String]

  /** The function a check of this struct calls — the synthesised `<Struct>$inv`, made real at the
   * struct's own type arguments where the struct is generic.
   *
   * **A generic struct's read rule is asked here rather than at its declaration**, because whether a
   * clause reads through something the struct does not own depends on what the parameters are: a
   * field of type `T` is the struct's own storage at `int` and a pointer's far side at `*Inner`. So
   * each instantiation's field types are held to the rule a non-generic struct's are held to where
   * it is declared, and the diagnostic is the same one, at the clause.
   */
  protected def invFnFor(s: Type.Struct): String =
    val decl = structDecls(s.base)
    val key  = invKey(s.base)

    if decl.tparams.isEmpty then key
    else
      if readsChecked.add(s.name) then checkInvariantReads(decl, s.fields.toMap)
      instantiateFunc(funcDecls(key), s.targs)

  /** The zero value a declaration with no initializer starts at, checked against every clause it
   * holds (`reference/errors.md § Struct invariants`).
   *
   * **A zero is a construction the program did not spell**, and a struct whose clause the zero does
   * not satisfy — `invariant lo < hi` — would otherwise begin its life broken with nothing having
   * checked it. That is a hole in the checking, and it is a hole in what the optimizer is told too:
   * a clause is assumed at the entry of the struct's members (`ContractEmitter`), which is sound only
   * if no value of the type ever got past a check. So each struct carrying clauses that lies inside
   * the zero — the type itself, a field of it, an element of an array — is checked once, at its own
   * zero. The check is of a constant and folds away wherever the zero satisfies it.
   */
  protected def checkedZero(ty: Type): TExpr =
    def owed(t: Type): List[Type.Struct] = Type.unqualified(t) match
      case s: Type.Struct =>
        (if carriesInvariants(s) then List(s) else Nil) ::: s.fields.flatMap(f => owed(f._2))
      case Type.Array(n, e) if n > 0 => owed(e)
      case _                         => Nil

    owed(ty).distinctBy(_.name) match
      case Nil => TZero(ty)
      case owes =>
        val checks = owes.map(s => TExprStmt(TStructInvCheck(TZero(s), s, invFnFor(s)).setPos(currentPos)))

        TBlockExpr(TBlock(checks, Some(TZero(ty)), ty)).setPos(currentPos)

  /** Whether an element reached by indexing a value of this type is on the far side of it — a
   * view's element, which has an identity of its own — rather than storage the value holds, as an
   * array's element is.
   */
  private def farSide(t: Type): Boolean = Type.underlying(Type.unqualified(t)).isInstanceOf[Type.View]

  /** Every struct a write through `place` obliges a re-check of: what to re-read, its type, and the
   * predicate to call, innermost first.
   *
   * The walk goes **outward through the whole place**, not just to the field's own receiver, because
   * an invariant may read through a field — `invariant a.n <= b` is a claim `o.a.n = 9` can break
   * just as `o.b = 0` can, and only the enclosing struct knows it. Nothing is owed by the parts of a
   * place that merely locate it, so an index contributes no check of its own and is walked through.
   */
  protected def invCheckFor(place: TExpr): List[(TExpr, Type.Struct, String)] =
    def owed(recv: TExpr): List[(TExpr, Type.Struct, String)] = recv.ty match
      case s: Type.Struct if carriesInvariants(s) => List((recv, s, invFnFor(s)))
      case _                                      => Nil

    place match
      case TField(recv, _, _) => owed(recv) ++ invCheckFor(recv)
      // A view's element is on its far side, which no clause may read (`checkInvariantReads`), so
      // writing one can break nothing the struct holding the view promised and is owed no
      // re-check. `self.elems[i] = v` in a buffer whose clause reads `elems.len` changes an element
      // and leaves the length where it was.
      case TIndex(recv, _, _) if farSide(recv.ty) => Nil
      case TIndex(recv, _, _)                     => invCheckFor(recv)
      // A `ref` name is a place written shorter (`reference/memory.md § ref — a name for a place`),
      // so the walk carries on through what it stands for. This is the whole of why a ref keeps the
      // checking a `*T` would have severed: there is still a place here to walk outward through,
      // and it is the one the program wrote.
      case TLoad(n, _) if refPlaces.contains(n) => invCheckFor(refPlaces(n))
      case _                                    => Nil

  /** The same walk asked the other question: which enclosing structs an alias of this place would be
   * typed **below**, each with the field path from that struct down to the place.
   *
   * The path is what makes the refusal answer about the clause rather than about the struct. It is
   * spelled as a clause spells it, so an index step contributes no name and an element stands for
   * its array — `g.items[0]` and `g.items[1]` are one path, since a clause reading the first says
   * nothing about which element a dynamic index would reach.
   */
  protected def severing(place: TExpr): List[(Type.Struct, List[String])] = {
    // A field is a *position* in the typed tree and a *name* in the clause, so the receiver's type is
    // what turns one into the other. Anything else under a field step is storage this walk cannot
    // spell, and it stops rather than guessing at a path.
    def walk(e: TExpr, below: List[String]): List[(Type.Struct, List[String])] = e match
      case TField(recv, i, _) =>
        recv.ty match
          case s: Type.Struct if i < s.fields.length =>
            val path = s.fields(i)._1 :: below
            (if carriesInvariants(s) then List((s, path)) else Nil) ::: walk(recv, path)
          case _ => Nil
      // An alias of a view's element is an alias of the far side, which no struct holding the view
      // owns — `&b.elems[0]` names storage no clause may read, so it is below no promise.
      case TIndex(recv, _, _) if farSide(recv.ty) => Nil
      case TIndex(recv, _, _)                     => walk(recv, below)
      // As in `invCheckFor`: a ref is a shorter spelling of the place it stands for, and the path a
      // clause would be told about is the one through that place.
      case TLoad(n, _) if refPlaces.contains(n) => walk(refPlaces(n), below)
      case _                                    => Nil

    walk(place, Nil)
  }

  /** The field paths a struct's clauses read, cached because a program that takes several addresses
   * inside one struct would otherwise re-walk the same clauses at each of them.
   */
  private val readSets = mutable.HashMap.empty[String, Set[List[String]]]

  protected def invReads(base: String): Set[List[String]] =
    readSets.getOrElseUpdate(base, structDecls.get(base).fold(Set.empty)(readsOf))

  /** Which of a struct's fields each clause reaches, as paths from the struct.
   *
   * A clause is an expression over the fields by name, so the paths are read straight off it: a name
   * that is a field roots one, a `.f` extends it, and an index is walked through for the same reason
   * the place walk above walks through one. An index *expression* is not part of the path but may
   * read fields of its own, so it is collected separately rather than dropped.
   *
   * A clause that hides an expression inside a **statement** — the body of an `if` used as a value, a
   * `match` arm, a lambda — is not walked, and the answer for the struct becomes every field it has.
   * Reading too much only ever refuses an alias that would have been allowed; reading too little
   * would allow one that severs a promise, which is the mistake this whole file exists to prevent.
   */
  private def readsOf(decl: StructDecl): Set[List[String]] = {
    val fields = decl.fields.map(_.name).toSet
    var opaque = false

    def collect(e: Expr): List[List[String]] = e match
      case _: Lambda | _: IfExpr | _: MatchExpr => opaque = true; Nil
      case _ =>
        clausePath(fields, e) match
          case Some(p) => p :: chainIndices(e).flatMap(collect)
          case None    => exprKids(e).flatMap(collect)

    val found = decl.invariants.flatMap(collect).toSet

    if opaque then fields.map(List(_)) else found
  }

  /** The path a clause expression names, when it is a place rooted at one of the struct's fields. */
  private def clausePath(fields: Set[String], e: Expr): Option[List[String]] = e match
    case Ident(n) if fields(n) => Some(List(n))
    case Field(r, n)           => clausePath(fields, r).map(_ :+ n)
    case Index(r, _)           => clausePath(fields, r)
    case _                     => None

  /** The index expressions along a place chain, which are read but are not part of the path. */
  private def chainIndices(e: Expr): List[Expr] = e match
    case Field(r, _) => chainIndices(r)
    case Index(r, i) => i :: chainIndices(r)
    case _           => Nil

  /** Every sub-expression a node holds, read off the case class rather than matched arm by arm —
   * the clause walks want a total descent and have nothing to say about any particular node.
   * Expressions reachable only through a *statement* are not here, which is what the walks above
   * fall back to reading everything for.
   *
   * There is no fallback for a node this cannot read, because `Expr` is a `Product` and every node
   * is therefore one. A walk with a fallback would answer "no children" for whatever it did not
   * recognize, which for a *clause* walk is the dangerous direction: an alias the clause really
   * makes would go unseen. Stating it in the type is what removes the case that could be wrong.
   */
  private def exprKids(e: Expr): List[Expr] =
    e.productIterator.toList.flatMap {
      case x: Expr         => List(x)
      case Some(x: Expr)   => List(x)
      case xs: Iterable[?] => xs.toList.collect { case x: Expr => x }
      case _               => Nil
    }

  /** Holds a clause to reading storage the struct **owns** (`reference/errors.md § Struct
   * invariants`).
   *
   * Everything else here restricts the aliases a program may make, and that only works while the
   * clause is a claim about the struct's own bytes. A clause that reads through a pointer, a
   * reference or a view is a claim about somebody else's — the far side of the hop has an identity of
   * its own, every other alias of it is out of this struct's sight, and a write through one breaks
   * the clause with nothing anywhere that could re-check it. So the clause is refused where it is
   * written rather than quietly meaning less than it says.
   *
   * A view's `len` is the exception, and it is not really one: the three words are stored in the
   * struct, so the length is the struct's own and the elements are not.
   *
   * A struct holding a **register** carries no invariant at all (`reference/memory.md § Device
   * memory`), and the rule is about the struct rather than about the clause for a reason worth
   * stating: a check is a call taking *every* field, so it reads the whole block however few fields
   * the clause names. On real hardware that is not a redundant read — reading a data register pops
   * a FIFO — so a clause over the shadow field beside the registers would make writing that field
   * an access to every one of them. There is nothing to keep the clause true either: a device
   * changes a register between the check and the instruction after it, with no alias anywhere for
   * `reference/errors.md § Struct invariants` to restrict.
   */
  protected def checkInvariantReads(decl: StructDecl, ftypes: Map[String, Type]): Unit = {
    val fields = decl.fields.map(_.name).toSet

    for (name, t) <- ftypes if Type.volatileIn(t) do
      err(s"'${decl.name}' holds the register '$name', so it carries no invariant — a check reads " +
        "every field of the struct, so one written over the ordinary fields beside a register would " +
        "make writing them an access to the device. And a device changes a register between the " +
        "check and the instruction after it, which is what no clause could hold. Check a register " +
        "where it is read")

    def indirect(t: Type): Boolean = t match
      case _: Type.Ptr | _: Type.Ref | _: Type.Weak | _: Type.View => true
      case _                                                       => false

    def refuse(t: Type, what: String): Unit =
      err(s"an invariant may only read storage the struct owns, and $what is read through " +
        s"${show(t)} — what is on the far side has an identity of its own, so another alias of it " +
        "could break the clause with nothing left to re-check it against. Hold the value in a field " +
        "of the struct, and state the invariant over that")

    /** The type a chain step lands on, refusing the step that leaves the struct's own storage. */
    def stepped(e: Expr): Option[Type] = e match
      case Ident(n) => ftypes.get(n)
      case Field(r, n) =>
        stepped(r).flatMap {
          case s: Type.Struct              => s.fields.find(_._1 == n).map(_._2)
          case _: Type.View if n == "len"  => None
          case t if indirect(t)            => refuse(t, s"'$n'"); None
          case _                           => None
        }
      case Index(r, _) =>
        stepped(r).flatMap {
          case Type.Array(_, elem) => Some(elem)
          case t if indirect(t)    => refuse(t, "an element"); None
          case _                   => None
        }
      case _ => None

    def walk(e: Expr): Unit =
      if clausePath(fields, e).isDefined then
        stepped(e)
        chainIndices(e).foreach(walk)
      else exprKids(e).foreach(walk)

    decl.invariants.foreach(e => recover(())(at(e.pos)(walk(e))))
  }

  /** Whether a write through an alias at `alias` could change what a clause reading `read` sees.
   *
   * Either path being a prefix of the other is enough, and the two directions are different
   * mistakes: an alias of `a` reaches the `a.n` a clause reads, and an alias of `a.n.x` changes the
   * `a.n` a clause reads whole. Equal paths are both at once.
   */
  private def overlaps(alias: List[String], read: List[String]): Boolean =
    alias.startsWith(read) || read.startsWith(alias)

  /** The first clause an alias of this place would put out of reach, if there is one. */
  private def severed(place: TExpr): Option[(Type.Struct, List[String])] =
    severing(place).flatMap { (s, path) =>
      invReads(s.base).find(overlaps(path, _)).map(read => (s, read))
    }.headOption

  /** `&place`, refused where the pointer it makes would be typed below a promise the place carries.
   *
   * `&o` is left alone — a `*Outer` names the struct, so a write through it is found by the ordinary
   * walk and checked. What is refused is the pointer that drops the name on the way out.
   */
  protected def checkAddressable(place: TExpr): Unit =
    checkPackedField(place)

    for (s, read) <- severed(place) do
      err(s"'&' here makes a '*${show(place.ty)}' pointing inside ${show(s)}, whose invariant reads " +
        s"'${read.mkString(".")}' — and a '*${show(place.ty)}' names no ${show(s)}, so a write through " +
        s"it would break the clause with nothing left to re-check it against. Take the address of the " +
        s"${show(s)} itself, which keeps the invariant in its type, or make the change through a method")

  /** `&` of a field inside a `@packed` struct, refused (`reference/types.md § Structs`).
   *
   * A packed field sits at its declared offset, which is very often not a multiple of its own
   * alignment — the whole point of the attribute. A `*u32` is a `*u32` wherever it came from, and
   * every later use of one is entitled to assume the address is aligned: the back end may widen a
   * load, choose an instruction that faults on a misaligned address, or hand it to a function that
   * does. So the pointer is not merely unusual, it is a promise the storage cannot keep, and the
   * mistake surfaces arbitrarily far from the `&` that made it.
   *
   * **Reading and writing the field are untouched**, because those go through the struct and the
   * back end knows the offset is unaligned; it is only the escaped address that loses that. That is
   * why this refuses the pointer rather than the packing.
   */
  private def checkPackedField(place: TExpr): Unit = place match
    case TField(receiver, _, ty) =>
      Type.underlying(receiver.ty) match
        case s: Type.Struct if s.packed =>
          err(s"'&' here makes a '*${show(ty)}' into ${show(s)}, which is '@packed' — its fields sit " +
            s"at their declared offsets, so this one need not be on a '${show(ty)}' boundary, and " +
            s"every use of a '*${show(ty)}' is entitled to assume that it is. Read or write the " +
            s"field through the struct, which is where the offset is known, or take the address of " +
            s"the ${show(s)} itself")
        case _ => ()
    case _ => ()

  /** The same refusal for the other way to reach storage that outlives the expression: a view of it
   * that may be written. A `[]const T` is left alone, since giving up the write is exactly what makes
   * the alias carry no promise it could break.
   */
  protected def checkSliceable(base: TExpr, view: Type): Unit =
    // Slicing a view makes a view of **its** storage, which is on the far side and no clause's to
    // read — `self.elems[..<self.count]` shares the buffer's elements and leaves its length alone.
    if !Type.readOnlyView(view) && !farSide(base.ty) then
      for (s, read) <- severed(base) do
        err(s"this view may be written, and it views storage inside ${show(s)}, whose invariant reads " +
          s"'${read.mkString(".")}' — a '${show(view)}' names no ${show(s)}, so a write through it would " +
          s"break the clause with nothing left to re-check it against. Take it as a '[]const " +
          s"${show(Type.element(view).getOrElse(Type.Unknown))}', which may not write, or make the " +
          s"change through a method")

  /** Re-checks, after a call, every invariant the receiver's place lies below (`reference/errors.md
   * § Struct invariants`).
   *
   * A `*self` method reached through a field — `o.a.bump()` — is the one severed place the language
   * still has, since there are no parameter modes (`reference/declarations.md § Functions`) and so
   * no other way to hand a callee somewhere to write without writing `&`. What makes it recoverable
   * is that the **call site** still knows the whole place: the receiver is `o.a`, `o` is right
   * there, and the clause can be re-run the moment the call returns. So the alias is allowed and
   * the promise is kept at the boundary instead of inside.
   *
   * Wrapping is by the same fold a write uses, so where a place lies below more than one struct the
   * innermost clause is the one that fires first.
   */
  protected def recheckAfter(recvArg: TExpr, call: TExpr): TExpr = recvArg match
    case TAddrOf(place, _) =>
      invCheckFor(place).foldLeft(call)((acc, c) => TRecheck(acc, c._1, c._2, c._3).setPos(call.pos))
    case _ => call

  /** The structs whose storage can lie **inside** one that carries clauses, reached by fields and by
   * array elements — the hops that keep the two in one object. A reference or a view is not one of
   * them: what is on the far side has an identity of its own and is not what an enclosing struct's
   * clause is a claim about.
   *
   * This is what keeps `SelfAlias` from being a rule about every method in every program. A struct
   * that is nobody's field cannot have its receiver be a severed place, so nothing its methods do
   * with `&self.f` can put a clause out of reach, and they are left alone. A program that declares no
   * invariants has an empty set here and pays nothing.
   */
  private def coveredStructs: Set[String] = {
    def inlineStructs(t: Type): List[Type.Struct] = t match
      case s: Type.Struct   => List(s)
      case Type.Array(_, e) => inlineStructs(e)
      case e: Type.Enum     => e.variants.flatMap(_.fields.map(_._2)).flatMap(inlineStructs)
      case _                => Nil

    val found = mutable.Set.empty[String]
    var work =
      structInsts.values.filter(carriesInvariants).toList.flatMap(_.fields.flatMap(f => inlineStructs(f._2)))

    while work.nonEmpty do
      val s = work.head

      work = work.tail
      if found.add(s.base) then work = s.fields.flatMap(f => inlineStructs(f._2)) ::: work

    found.toSet
  }

  /** Holds every `*self` method of such a struct to finishing with its receiver when it returns.
   *
   * Runs once, over the whole program, because the set above is only complete when every type the
   * program mentions has been instantiated. Each finding is reported and the walk goes on, so a body
   * with two escapes names both rather than the first.
   */
  protected def checkSelfAliasing(funcs: List[TFunc]): Unit = {
    val covered = coveredStructs

    if covered.nonEmpty then
      for f <- funcs do
        f.params.headOption match
          case Some(("self", Type.Ptr(s: Type.Struct))) if covered(s.base) =>
            for (msg, pos) <- SelfAlias.check(f) do recover(())(at(pos)(err(msg)))
          case _ =>
  }
}
