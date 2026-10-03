package sh.sysl

/** Which local of a function is built where its result is going, rather than built in a slot of its
 * own and copied there at the `return`.
 *
 * A large result is written into storage the caller supplies (`Emitter.syslSret`), and a call's
 * result is already built there. What this adds is the **local** that becomes the result:
 *
 * {{{
 * synth(score: Score, rate: u32) -> Result[Synth, MusicError]
 *     if rate == 0 then return Err(NoSampleRate)
 *     var s = Synth(score, rate, …)
 *     s.rewind()
 *     Ok(s)
 * }}}
 *
 * Without this `s` has a slot in the frame, and `Ok(s)` copies all of it into the caller's storage
 * — two copies of a six-kilobyte value alive at once, on a board whose whole stack is four. With it,
 * `s`'s storage *is* the payload of the caller's `Result`, so the `Ok` writes the tag and nothing
 * else.
 *
 * ==The rule==
 *
 * The local `s` of a function with a large result is built in place when all of these hold:
 *
 *   - it is a `var` or `val` declared **at the top level of the body**, with no alignment of its own,
 *     of a large type, and not an array the escape analysis moved to the heap;
 *   - the body's result is `s` itself, or `s` as one argument of a variant or struct being built —
 *     `Ok(s)`, `Some(s)`, `(s, n)`, nested to any depth — where no other argument mentions `s`;
 *   - every `return` after the declaration returns `s` at **the same position**. A `return` before
 *     it, or inside its initializer, returns anything at all, since `s` is not yet anything;
 *   - no `?` comes after the declaration, in a statement or in the result itself;
 *   - `s`'s address is never taken except as a **direct argument of a call** — `s.rewind()` on a
 *     `*self` method, `fill(&s)` — and no `ref` is bound into it and no view is sliced out of it
 *     except as a call's argument;
 *   - where its address *is* handed to a call, nothing runs between a `return` and the end of the
 *     function: no postcondition, and no release that could reach a destructor (`destructive`);
 *   - the body has no `defer`, no `asm`, and no self-call lowered as a jump.
 *
 * ==Why those are the conditions==
 *
 * The local and the result occupy one region, so the program may not be able to tell which one it
 * wrote. Every condition closes a way of telling:
 *
 *   - **A different value returned after `s` exists** would be written over `s`'s bytes while
 *     `s`'s counts are still owed, and its own arguments could read `s` part-way through being
 *     overwritten: `return Err(fault(s.n))` writes `Err`'s payload into the same bytes. So any
 *     such `return` keeps the copy.
 *   - **A `?` is a `return` of the failure**, written into the same storage (`genTry`), and the
 *     local is not registered for release — so a `?` failing after `s` exists would write the error
 *     over `s` with its counts still owed, and a destructor `s` reaches would never run. Any `?`
 *     after the declaration keeps the copy.
 *   - **Another argument mentioning `s`** could change it between the copy the source promises and
 *     the end of the construction: `(s, s.step())` returns the `s` from *before* the step.
 *   - **An address kept anywhere** could be written through after the value was written into the
 *     result, and that write would land in it. So an address may only go to a call, and even then
 *     the callee may have kept it: the only code that runs between the `return` and the function
 *     being left is the releases and the postcondition, so where neither can run any of the
 *     program's code, nothing can write through a kept pointer in that window. After the function
 *     is left such a pointer dangles under either lowering, which is `*T`'s own hazard.
 *   - **A `defer`** runs after the value is written and before the function is left, so a deferred
 *     write to `s` would reach the returned value only under this lowering.
 *   - **A jump to the top** re-runs the declaration with the result region already written.
 *
 * Ownership moves with the bytes: the local's counts become the result's, so the slot is never
 * registered for release and the `return` takes no count for it (`Codegen.genIndirectReturn`).
 * Every way out after the declaration is a `return` of `s`, which is what makes leaving it
 * unregistered the same as handing it over.
 */
object ReturnSlot {

  /** One step from a value's storage into a part of it. */
  enum Step:
    case Field(struct: Type.Struct, i: Int)
    case Payload(en: Type.Enum, variant: Type.EnumVariant, i: Int)

  /** The local built in place, and where in the result it sits. */
  case class Plan(name: String, path: List[Step])

  /** `runsCode` is `destructive` over the program being compiled: whether letting go of a value of a
   * type can run a destructor.
   */
  def of(f: TFunc, indirect: Type => Boolean, promoted: Set[String], runsCode: Type => Boolean): Option[Plan] =
    if !indirect(f.retTy) || TailCalls.of(f).nonEmpty || f.body.result.isEmpty then None
    else if forbidden(f.body) then None
    else
      val stmts = f.body.stmts

      stmts.zipWithIndex.iterator.flatMap {
        case (TVarDecl(name, ty, _, None), i) if indirect(ty) && !promoted(name) =>
          pathOf(f.body.result.get, name).filter { path =>
            val after = stmts.drop(i + 1)

            returns(after).forall(r => r.value.flatMap(pathOf(_, name)).contains(path)) &&
            !tries(after) && !tries(f.body.result.get) &&
            !escapes(after, name) && !escapes(f.body.result.get, name) &&
            (!lent(after, name) || quiet(f, runsCode))
          }.map(Plan(name, _))
        case _ => None
      }.nextOption()

  /** Whether the local's address is handed to any call at all. A callee may keep it, and nothing
   * stops a pointer outliving the call that was given it — so where one could exist, nothing may run
   * between the `return` writing the result and the function being left (`quiet`).
   */
  private def lent(x: Any, name: String): Boolean = x match
    case _: Type                  => false
    case TAddrOf(place, _)        => mentions(place, name)
    case TSlice(base, _, _, _, _) => mentions(base, name) || lent(base, name)
    case xs: Iterable[?]          => xs.exists(lent(_, name))
    case p: Product               => p.productIterator.exists(lent(_, name))
    case _                        => false

  /** Whether nothing of the program's runs between a `return` and the end of `f`: no postcondition,
   * and no value whose release could reach a destructor. What a release can reach is asked of every
   * type the body mentions, which covers every local, every binding and every temporary it holds,
   * the local in question included — a superset, and cheap, since a program with no destructor
   * answers no for all of them.
   */
  private def quiet(f: TFunc, runsCode: Type => Boolean): Boolean =
    f.ensures.isEmpty && !(types(f.body) ++ f.params.iterator.map(_._2)).exists(runsCode)

  private def types(x: Any): Iterator[Type] = x match
    case t: Type         => Iterator(t)
    case xs: Iterable[?] => xs.iterator.flatMap(types)
    case p: Product      => p.productIterator.flatMap(types)
    case _               => Iterator.empty

  /** Whether releasing a value of a type can run a destructor (`reference/memory.md § A
   * destructor`) — directly, through a box it lets go of, or through what that box's contents let go
   * of. A trait object or a closure answers yes where any type has one, its payload being a type
   * nothing here can see; so does a view whose owner could be a box of a type with a destructor
   * holding an array of its element, since a view keeps its backing alive whatever the backing is.
   */
  def destructive(program: TProgram): Type => Boolean = {
    val drops    = program.destructors.keySet
    val dropping = (program.structs ++ program.enums).filter(t => drops(Type.mangle(t)))

    def holdsArrayOf(t: Type, elem: Type, seen: Set[String]): Boolean = t match
      case Type.Array(_, e) => e == elem || holdsArrayOf(e, elem, seen)
      case s: Type.Struct   =>
        val k = Type.mangle(s)
        !seen(k) && s.fields.exists(f => holdsArrayOf(f._2, elem, seen + k))
      case e: Type.Enum     =>
        val k = Type.mangle(e)
        !seen(k) && e.variants.exists(_.fields.exists(f => holdsArrayOf(f._2, elem, seen + k)))
      case _                => false

    def go(t: Type, seen: Set[String]): Boolean = t match
      case Type.Ref(_: Type.Trait, _) => true
      case Type.Ref(inner, _)         => drops(Type.mangle(inner)) || go(inner, seen)
      case _: Type.Weak               => false
      case v: Type.View               => go(v.elem, seen) || dropping.exists(holdsArrayOf(_, v.elem, Set.empty))
      case Type.Array(_, e)           => go(e, seen)
      case Type.Volatile(inner)       => go(inner, seen)
      case c: Type.Constrained        => go(c.base, seen)
      case s: Type.Struct             =>
        val k = Type.mangle(s)
        !seen(k) && s.fields.exists(f => go(f._2, seen + k))
      case e: Type.Enum               =>
        val k = Type.mangle(e)
        !seen(k) && e.variants.exists(_.fields.exists(f => go(f._2, seen + k)))
      case _: Type.Trait | _: Type.Abstract => true
      case _                          => false

    // A program none of whose types has a destructor runs none, whatever it lets go of — a trait
    // object's payload is one of the types it made, and so is anything a view's owner holds.
    t => drops.nonEmpty && go(t, Set.empty)
  }

  /** Where `e` puts the local `name`, where `e` is `name` itself or a construction holding it at one
   * argument that no other argument mentions.
   */
  def pathOf(e: TExpr, name: String): Option[List[Step]] = e match
    case TLoad(`name`, _) => Some(Nil)

    case TStructNew(struct, args) if Bitfields.of(struct).isEmpty =>
      within(args, name).map((i, rest) => Step.Field(struct, i) :: rest)

    case TEnumNew(en, variant, args) if !en.simple && variant.carries =>
      within(args, name).map((i, rest) => Step.Payload(en, variant, i) :: rest)

    case _ => None

  private def within(args: List[TExpr], name: String): Option[(Int, List[Step])] = {
    val found = args.zipWithIndex.flatMap((a, i) => pathOf(a, name).map(p => (i, p)))

    found match
      case List((i, path)) if args.zipWithIndex.forall((a, j) => j == i || !mentions(a, name)) => Some((i, path))
      case _                                                                              => None
  }

  /** Whether anything in `x` reads or names the local. */
  def mentions(x: Any, name: String): Boolean = x match
    case _: Type          => false
    case TLoad(n, _)      => n == name
    case xs: Iterable[?]  => xs.exists(mentions(_, name))
    case p: Product       => p.productIterator.exists(mentions(_, name))
    case _                => false

  /** Every `return` in `x`, at any depth. */
  private def returns(x: Any): List[TReturn] = x match
    case _: Type         => Nil
    case r: TReturn      => r :: r.productIterator.flatMap(returns).toList
    case xs: Iterable[?] => xs.iterator.flatMap(returns).toList
    case p: Product      => p.productIterator.flatMap(returns).toList
    case _               => Nil

  /** Whether `x` holds a `?` at any depth — an early return that writes the result storage. */
  private def tries(x: Any): Boolean = x match
    case _: Type         => false
    case _: TTry         => true
    case xs: Iterable[?] => xs.exists(tries)
    case p: Product      => p.productIterator.exists(tries)
    case _               => false

  /** A body this lowering is not attempted for at all. */
  private def forbidden(x: Any): Boolean = x match
    case _: Type                  => false
    case _: TDefer | _: TAsm      => true
    case _: TBecome               => true
    case xs: Iterable[?]          => xs.exists(forbidden)
    case p: Product               => p.productIterator.exists(forbidden)
    case _                        => false

  /** Whether the local's storage is reachable by anything but its own name once the expression has
   * run: an address taken anywhere other than as a call's argument, a view sliced out of it on the
   * same terms, or a `ref` bound into it.
   */
  private def escapes(x: Any, name: String): Boolean = {
    def borrowed(a: TExpr): Boolean = a match
      case TAddrOf(place, _)          => within(place)
      case TSlice(base, lo, hi, _, _) => within(base) && !escapes(lo.toList ++ hi.toList, name)
      case _                          => false

    // The place itself is the local or a part of it reached by field, index or lane — which is what
    // makes the address the local's rather than one read out of it.
    def within(place: TExpr): Boolean = place match
      case TLoad(`name`, _)   => true
      case TField(r, _, _)    => within(r)
      case TIndex(r, i, _)    => within(r) && !escapes(i, name)
      case TLane(r, _, _)     => within(r)
      case _                  => !mentions(place, name)

    def args(as: List[TExpr]): Boolean = as.exists(a => !borrowed(a) && escapes(a, name))

    x match
      case _: Type                        => false
      case TCall(_, as, _, _)             => args(as)
      case TVCall(recv, _, as, _, _)      => escapes(recv, name) || args(as)
      case TCallPtr(callee, as, _, _)     => escapes(callee, name) || args(as)
      case TAddrOf(place, _)              => mentions(place, name)
      case TSlice(base, _, _, _, _)       => mentions(base, name)
      case r: TRefDecl                    => mentions(r.place, name)
      case xs: Iterable[?]                => xs.exists(escapes(_, name))
      case p: Product                     => p.productIterator.exists(escapes(_, name))
      case _                              => false
  }
}
