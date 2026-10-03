package sh.sysl

/** Calling a method, which is four different resolutions wearing one syntax.
 *
 * `x.m(a)` reaches its definition by a different route depending on what `x` is: a concrete type
 * looks the member up under its mangled name, a bounded type parameter finds it in one of its
 * bounds, a trait object reads it out of the table it carries, and a builtin — `str`, `hash`,
 * a weak reference's `get` — has no declaration at all and is recognised here. An overload set adds
 * a fifth step in front of the other four, since which member is meant depends on the arguments.
 *
 * **The choice is what this file is**, and it is made in one place: once made, each route ends in the
 * same free call `CallCore` already knows how to check — the receiver passed in the mode its `self`
 * sigil declared, and nothing else method-specific. What stays here beside the choice is the route
 * that dominates it, the concrete one: the member lookup, the overload set in front of it, a method
 * with type parameters of its own, and the builtins no declaration backs.
 *
 * Two routes are beside this rather than in it, because neither looks a member up under an owner
 * key: `VectorMethods` answers the six names a `<N>T` knows, and `AbstractMethods` resolves against
 * a type parameter's bounds or a trait object's table.
 */
trait MethodCalls extends FuncAddress with VectorMethods with AbstractMethods {

  /** `value.method(args)` — resolves `method` as an inherent member of the receiver's type and
   * calls the function it lowered to, passing the receiver as the first argument in whatever
   * memory mode the method's `self` asked for.
   *
   * The receiver may be of *any* type: every type has an owner key its members are filed under, so
   * `5.show()` takes this path exactly as `p.show()` does.
   *
   * A method with type parameters **of its own** is the one shape the receiver does not settle, and
   * `callGenericMethod` is where the rest of the answer comes from.
   */
  protected def callMethod(
      recv: Expr,
      mname: String,
      args: List[Expr],
      expected: Option[Type],
      writtenTargs: List[Expr] = Nil,
  ): TExpr =
    callMethodOn(analyzeExpr(recv), mname, args, expected, receiverBound(recv), writtenTargs)

  /** The traits the receiver's own **bound** promised, read off the expression as written.
   *
   * This is the one thing the analyzed receiver cannot be asked for. `x` of a `[T: Zero]` body has
   * become an `int` by the time an instantiation looks at it, so the question has to be put to the
   * source — where the name is still `x`, and `pbounds` still knows `x` was written as a `T`.
   */
  protected def receiverBound(recv: Expr): Set[String] = recv match
    case Ident(n) => pbounds.get(n).map(boundTraits).getOrElse(Set.empty)
    case _        => Set.empty

  /** The same, for a receiver already analyzed — what a form that had to look at the receiver's type
   * to know it was a method call uses, so the receiver is analyzed once and the analysis that
   * decided is the one that runs.
   */
  protected def callMethodOn(
      tr: TExpr,
      mname: String,
      args: List[Expr],
      expected: Option[Type],
      via: Set[String] = Set.empty,
      // Written at the call — `x.m[T](…)` (`reference/generics.md § [] means type application in a
      // type, indexing in an expression`). Only the last branch below can be reached with a list in
      // hand, because what routes a call here with one is a guard that has already found a
      // **declared method** of that name on the receiver's own type.
      writtenTargs: List[Expr] = Nil,
  ): TExpr = {
    receiverType(tr.ty) match
      case a: Type.Abstract => callBoundMethod(a, tr, mname, args)
      case t: Type.Trait    => callTraitObject(tr, t, mname, args)
      case w: Type.Weak     => weakGet(w, tr, mname, args)

      // `s.copy()` is the operation that stops a substring holding its parent buffer alive (`04`):
      // the bytes are copied into a string that owns them, so what the result keeps is its own.
      // Parentheses because it allocates and walks the bytes — `reference/declarations.md § A
      // property` puts the O(1) projections on the other side of that line, and this is the case
      // the line was drawn for.
      case Type.Str if mname == "copy" =>
        if args.nonEmpty then err("'copy' takes no arguments — it copies the string it is read off")
        TFromBytes(TBytes(tr))

      // The vector's own methods, which are the compiler's for the reason `Intrinsics`' table is
      // closed: each is one LLVM operation and none of them can be written in sysl. `select` is an
      // instruction, and the reductions are intrinsics overloaded on the register's whole type — so
      // an `impl` block would have nothing to put in its bodies.
      case v: Type.Vector if vectorMethods(mname) => vectorMethod(v, tr, mname, args)

      // The two that move lanes between a register and memory. They are the slice's rather than the
      // vector's because the receiver is what supplies the address and the length — a vector has
      // neither, which is the whole reason these had to exist.
      case rty @ (_: Type.Array | _: Type.Slice) if claimsLanes(rty, mname) =>
        vectorMemory(rty, autoDeref(tr), mname, args, expected)

      // A `*T` is a bare address, and `03`'s unchecked tier is where a program that has one already
      // is. What it does not have is a length, so there is nothing for a run to be checked against
      // — and a load that silently skipped the check would be the one vector operation that could
      // read past the end of an object while every other way of reaching those elements cannot.
      //
      // **Only where the pointee could be a lane**, which is what keeps this from answering for
      // every `*T` in the language. `sysl.sync.Atomic` declares `load` and `store` and is reached
      // through a `*self`, so a message about runs of elements would be the compiler explaining
      // vectors to somebody writing a retry loop — and worse, refusing the call.
      case _ if vectorMemoryMethods(mname) && pointsAtALane(tr.ty) =>
        err(s"a '${show(tr.ty)}' carries no length, so a run of its elements has nothing to be " +
          "checked against — take a slice of them first, with 'p[0..<n]'")

      // A string is a view of bytes that are valid UTF-8 (`04`), and a run of them is bytes rather
      // than lanes of anything the string promises — so the answer is where a program says it wants
      // the bytes, not a second spelling that quietly means the same.
      case Type.Str if claimsLanes(Type.Str, mname) =>
        err(if mname == "store" then
          "a string is immutable, so there is nothing to write lanes into"
        else
          "a string is a view of UTF-8 rather than of lanes — 's.bytes.load(i)' says that a run of " +
            "bytes is what is wanted, and is what a '<N>u8' is a run of")

      case rty =>
        val (base, _) = memberKey(rty, mname)
        val chosen    = pickOverload(rty, base, mname, args, via)
        val targs     = memberKey(rty, chosen)._2

        memberDecls.get((base, chosen)) match
          case Some(m) if m.receiver.isDefined && m.tparams.nonEmpty =>
            checkMemberVisible(base, chosen, m)
            callGenericMethod(genericMembers((base, chosen)), m, targs, tr,
              bindArgs(s"method '$base.$chosen'", Some(base), m.params, args, m.variadic), expected,
              writtenTargs, viewedConst(rty, base))
          case Some(m) if m.receiver.isDefined =>
            checkMemberVisible(base, chosen, m)

            // A list written on a member that has no parameters for it. The receiver's arguments are
            // not among them — they are the *type*'s and are already settled by the value in hand —
            // so this is the method's own emptiness rather than the type's.
            if writtenTargs.nonEmpty then
              err(s"'$chosen' is not generic, so it has no type arguments to write — the call is " +
                s"'$mname(…)'. The arguments of ${show(rty)} are the receiver's and are already " +
                "settled by the value it is read off")

            val fname = memberFuncName(rty, chosen)
            // **A member whose own signature did not resolve is registered and has no lowered
            // form**, which is `MemberLowering`'s deliberate order — the declaration is filed
            // before the signature is built, so that a mistake in the signature does not also erase
            // the member it is about. What that leaves is a call whose callee has nothing to be
            // checked against, and the mistake has already been reported at the declaration where
            // it belongs. So the call is abandoned in silence rather than complained about twice —
            // and rather than, as it did until this line, taking the compiler down with a missing
            // key. Which of the two happened used to depend on declaration order alone: a caller
            // written *above* its callee reached here before the signature failed to be recorded.
            val (params, rtype) = funcInsts.getOrElse(fname, poisoned())
            // A closure's `call` is not a method a program wrote, so a complaint about one names
            // the callable and the argument's position rather than the member behind it
            // (`reference/types.md § Function types`).
            val callable = mname == "call" && callableOf(rty).isDefined
            // Named by the **receiver**, not by the symbol the member is emitted under. The two
            // agree for a type with a name of its own and part company for everything reached
            // through a shape or a blanket — `bound.Integer.display.int` is a fine symbol and an
            // answer to a question nobody asked, where `int.display` is what the call site wrote.
            val shown = if callable then "this callable" else s"method '${show(rty)}.$chosen'"
            // The receiver is not among the parameters a call writes arguments for, so it is not
            // among the ones a name may reach either — `m.params` starts where the arguments do.
            val bound = bindArgs(shown, Some(base), m.params, args, m.variadic)

            checkArity(shown, params.length - 1, m.variadic, bound.length)
            // A member's tail begins where its declared parameters stop, and the receiver holds one
            // of the slots — so the cut is one past what a free function's would be.
            val (declared, tail) = bound.splitAt(params.length - 1)
            val recvArg  = buildReceiver(m.receiver.get, tr, mname)
            // A member of a generic type may default to something naming the type's parameters,
            // which the receiver has already settled.
            val types    = genericMembers.get((base, chosen)).fold(noDefaultTypes)(calleeTypes(_, targs))
            val restArgs = declared.zip(params.tail).map { case (a, (_, pty)) =>
              if a.isInstanceOf[DefaultArg] then withDefaultTypes(types)(analyzeExpr(a, Some(pty)))
              else analyzeExpr(a, Some(pty))
            }
            funcsUsed += fname

            val checked =
              checkArgs(if callable then shown else fname, params, declared,
                        Some(recvArg :: restArgs), callable, types)

            checkCrossings(m.crossing, shown, params, checked)
            recheckAfter(recvArg, TCall(fname, checked ::: tail.map(variadicArg(_)), rtype))
          // Neither of the two remaining kinds takes a receiver, and they are not the same mistake:
          // a property is this call with the parentheses dropped, an associated function is not
          // reached through a value at all.
          case Some(m) if m.isProperty =>
            err(s"'$mname' is a property of '$base' — read it as 'value.$mname', without '()'")
          case Some(_) => err(s"'$mname' is an associated function of '$base' — call it with '$base.$mname(…)'")
          case None =>
            builtinMethod(rty, mname, tr, args)
              .orElse(builtinNumeric(rty, mname, tr, args))
              .orElse(builtinHash(rty, mname, tr, args))
              .orElse(callableField(rty, mname, tr, args, expected))
              .getOrElse {
                // A call reaches through one level of indirection and only one, exactly as
                // selection does (`reference/declarations.md § Structs`). A receiver still carrying
                // a mode after that has more than the shorthand walks, and the complaint below
                // would name what is *left* — a type the reader never wrote — and report a missing
                // method, when the method is there and what stopped short is the reach.
                rty match
                  case _: Type.Ptr | _: Type.Ref =>
                    err(s"a method call reaches through one level of indirection and ${show(tr.ty)} " +
                      s"has more, so the rest is written: '(*x).$mname(…)' calls '$mname' on the " +
                      s"${show(rty)} it leaves")
                  // A member the compiler provides, written with parentheses it does not take. A
                  // property a program *declares* has said so since properties existed; a provided
                  // one fell through to the line below and denied the member outright — the one
                  // answer that is not true of it, since `len` is exactly what a slice has. Anything
                  // reaching here failed to resolve as a call, so the provided members that really
                  // are called — `copy`, and a weak reference's `get` — are already gone above.
                  case _ if builtinMember(rty, mname) =>
                    err(s"'$mname' is a property the compiler provides for '$base' — read it as " +
                      s"'value.$mname', without '()'")
                  case _ => err(s"type '$base' has no method '$mname'")
              }
  }

  /** Which of a type's members a written name means, where more than one implementation of one trait
   * gave the type a member of that name.
   *
   * A name that reaches one member — everything a program writes until a type implements a trait
   * twice — comes straight back, so the ordinary call is the lookup it always was. Where there are
   * several, the arguments decide: each candidate's parameters are compared against the types the
   * arguments already have, and exactly one candidate accepting them is the answer.
   *
   * This is **not** overloading, and the difference is that the answer is determined rather than
   * chosen. A trait's argument list is what tells its implementations apart, and a call carries the
   * argument list in the values it passes — so a call that does not determine one is a call whose
   * arguments name no implementation, which is reported rather than resolved by a preference rule.
   * The arguments are analyzed here with nothing expected of them, and a literal has no type of its
   * own to be matched by, so `c.mul(2)` where the candidates take a `Complex` and a `real` is one
   * of the calls that determines nothing (`reference/declarations.md § Structs`).
   */
  protected def pickOverload(
      owner: String,
      mname: String,
      args: List[Expr],
      subject: String,
      via: Set[String] = Set.empty,
      /** Whether the **compiler** supplies a member of this name for this type as well — which is a
        * candidate with no row in any table, since `5.0.mul(2.0)` is answered by the instruction
        * rather than by a block. It matters only where a block was written for a built-in at *other*
        * arguments, which coherence began to allow at card `0385`: without it that block is the one
        * candidate and takes every call, including the ones the machine has an instruction for.
        */
      provided: Boolean = false,
  )(
      params: String => Option[List[Type]],
  ): String =
    memberAlts.get((owner, mname)) match
      // One member of the name is still a member of *some* trait, and the rule is about the trait
      // rather than about there being a choice: a name a file cannot reach is not answered by there
      // being nothing else it might have meant.
      case None =>
        if reachable(owner, mname, via) then mname else outOfScope(owner, mname, List(mname), subject)
      case Some(everything) =>
        // **A bound answers ahead of everything else.** Inside a generic body `T.zero()` means the
        // `zero` the bound promised, and at an instantiation the parameter has become an ordinary
        // type whose table may hold several — so the traits the parameter was bounded by are what
        // the candidates are narrowed to first. Without this a body is refused for an ambiguity its
        // own signature already settled, and which the caller has no way to speak to.
        val all =
          if via.isEmpty then everything
          else
            val named = everything.filter(c => memberTrait.get((owner, c)).exists(via))

            if named.isEmpty then everything else named

        // **Scope decides before the arguments do**, because the two axes answer different
        // questions and only this one can answer its own. Two implementations of one trait differ
        // in their argument lists and are told apart by a call's values; two *different traits* may
        // declare the same name with the same parameters — `zero()` and `zero()` — and nothing in
        // the call could ever tell those apart. What tells them apart is that a trait's member is
        // reachable only where the trait can be named (`reference/modules.md § Visibility`), so a
        // file reaching one of them has said which by what it imported.
        val cands = all.filter(reachable(owner, _, via))

        // With a compiler-supplied member of this name there is always something left, and it is the
        // one no import can reach or fail to reach.
        if cands.isEmpty then
          if provided then return mname else outOfScope(owner, mname, all, subject)

        // One survivor is the whole answer, and it is the ordinary case once two libraries have each
        // implemented something for a built-in: the arguments are never consulted, which is what
        // makes a nullary `zero()` resolvable at all. It is **not** the whole answer where the
        // compiler supplies one too, since then there are two and only the arguments say which.
        if cands.length == 1 && !provided then return cands.head

        val supplied = args.map(probeType)
        val from = s"'$mname' comes from ${quantity(cands.length, "implementation")} of one trait on $subject"

        // Which candidate a call means is read off the arguments it wrote, in the order it wrote
        // them — so a name, which is exactly a refusal to say what an argument's position is, has
        // nothing here to choose between them. Said outright, because the alternative is the
        // "none of them takes (?, ?)" below, which points at the wrong thing.
        for n <- args.collectFirst { case n: NamedArg => n } do
          at(n.pos)(err(s"$from, and which is meant is read off the arguments as written — so an " +
            "argument given by name leaves nothing to tell them apart. Write them in declared order"))

        // A candidate that takes a `...` is answered for by its declared parameters alone: the tail
        // stands at none, so what it may be told apart by stops where they do (`reference/ffi.md §
        // Variadic functions`). A default widens the same count downwards, and for the same reason
        // — a call may stop where the defaults begin, so what is compared is the prefix the two
        // lists share.
        val fits = cands.filter { c =>
          params(c).exists { ps =>
            val counted =
              if variadicMember(owner, c) then supplied.length >= ps.length
              else supplied.length <= ps.length && supplied.length >= leastMember(owner, c, ps.length)

            counted && ps.zip(supplied).forall((p, s) => s.contains(p))
          }
        }

        if fits.length == 1 then return fits.head

        // No written block takes these arguments, and the compiler has a member of this name: that
        // is the ordinary call the instruction answers, and the plain name is what reaches it —
        // nothing is filed under it, so the built-in path below picks the call up as it always did.
        if fits.isEmpty && provided then return mname

        // Nothing has settled it. Which complaint that is depends on what is left standing: several
        // **traits** is a use that reached more than one of them, and no argument could have told
        // those apart — a `zero()` and a `zero()` take the same nothing. Several implementations of
        // one trait is the older situation, where the arguments were the question and did not
        // answer it. The set to describe is what the arguments left, or all of them where they left
        // none, since that is the set the reader has to choose from either way.
        val left   = if fits.isEmpty then cands else fits
        val traits = left.flatMap(c => memberTrait.get((owner, c))).distinct

        if traits.length > 1 then
          err(s"'$mname' on $subject comes from ${conjoin(traits.map(qn))}, and each is in scope " +
            "here — nothing in the call says which was meant")

        if fits.isEmpty then
          err(s"$from, and none of them takes (${supplied.map(_.fold("?")(show)).mkString(", ")}) — " +
            "write the argument at the type of the implementation that was meant")

        err(s"$from, and the arguments do not say which was meant")

  /** Whether a use site here can reach this member at all (`reference/modules.md § Visibility`).
   *
   * Three ways it can, and the first is why the table records provenance at all. A member the type's
   * **own** body declared has no trait to be gated by and is reachable wherever the type is. One an
   * `impl` brought is reachable where its trait can be **named** — and also wherever a **bound**
   * asked for that trait, since a signature naming a trait has said so at least as plainly as an
   * import would.
   */
  protected def reachable(owner: String, cand: String, via: Set[String]): Boolean =
    memberTrait.get((owner, cand)).forall(tr => via(tr) || traitInScope(tr))

  /** What to say where a type has the member under every spelling and the file may reach none of
   * them — which is the whole of what an import would have changed, so the message is the import.
   */
  protected def outOfScope(owner: String, mname: String, cands: List[String], subject: String): Nothing = {
    val all = cands.flatMap(c => memberTrait.get((owner, c))).distinct

    // A trait this file may not **name** is not one an import would reach, so advising the import
    // would send a reader after a line the compiler refuses. Where every candidate is out of reach
    // for that reason, the member is somebody else's business and saying so is the whole of it —
    // naming the trait would leak a name that cannot be written here either.
    val traits = all.filter(visible).map(qn)

    if traits.isEmpty then
      err(s"$subject has '$mname', but the trait declaring it is private to the module that wrote " +
        "it — the member is an implementation detail rather than something to import")
    else
      err(s"$subject has '$mname' from ${conjoin(traits)}, and " +
        (if traits.length == 1 then s"that trait is not in scope here — import it to reach the member"
         else "none of those traits is in scope here — import the one that was meant"))
  }

  /** Whether one of a type's members takes a `...`, asked of the member table rather than of the
   * lowered signature — a tail is not something a parameter list records.
   */
  private def variadicMember(owner: String, mname: String): Boolean =
    memberDecls.get((owner, mname)).exists(_.variadic)

  /** How few arguments one of a type's members may be called with — its parameters, less the ones a
   * default stands in for. Asked of the member table for the reason above: a default is not
   * something a lowered signature records, and `declared` is what to fall back on where the member
   * itself cannot be found.
   */
  private def leastMember(owner: String, mname: String, declared: Int): Int =
    memberDecls.get((owner, mname)).fold(declared)(m => m.params.count(_.default.isEmpty))

  /** `value.m(…)`, where the receiver's type names each candidate's instantiation. */
  protected def pickOverload(
      rty: Type,
      base: String,
      mname: String,
      args: List[Expr],
      via: Set[String],
  ): String =
    pickOverload(base, mname, args, show(rty), via,
      provided = CoreTraits.declaring(mname).exists(CoreTraits.builtin(_, rty)))(c =>
      probe(funcInsts(memberFuncName(rty, c))._1.tail.map(_._2)))

  /** `Type.f(…)` — an associated function, which has no receiver to drop off the front. */
  protected def pickAssociated(tname: String, mname: String, args: List[Expr], via: Set[String] = Set.empty): String =
    pickOverload(tname, mname, args, qn(tname), via)(c => funcInsts.get(s"$tname.$c").map(_._1.map(_._2)))

  /** The type an argument already has, with nothing expected of it and nothing said about whatever
   * goes wrong — the ordinary analysis that follows reports that, in the place it belongs.
   */
  private def probeType(e: Expr): Option[Type] = probe(analyzeExpr(e).ty)

  /** `value.m(…)` where `m` declares type parameters of its own, at the instantiation this call
   * resolves to.
   *
   * The two lists of parameters the lowered function carries are fixed from two different places,
   * which is the whole of what makes this its own path. The **type's** are already settled — the
   * receiver is a type, and a type has its arguments — so they are read off it and held to nothing
   * further here, having been checked where the receiver's type was made. The **member's own** are
   * solved from the arguments and from the type the context expects, exactly as a generic free
   * function's are, and held to the bounds the member wrote, reported in the member's name.
   *
   * Appending the solved arguments to the receiver's is what `synthesize` laid the two lists out in
   * that order for, so the substitution instantiating the function needs to know nothing about
   * which parameter came from where.
   */
  private def callGenericMethod(
      fd: FuncDecl,
      m: MethodDecl,
      ownerArgs: List[Type],
      recv: TExpr,
      args: List[Expr],
      expected: Option[Type],
      writtenTargs: List[Expr],
      constSelf: Boolean,
  ): TExpr = {
    val shown = qn(fd.name)

    checkArity(s"method '$shown'", fd.params.length - 1, m.variadic, args.length)

    // The tail stands at no parameter, so it is kept out of the inference as well as out of the
    // check: what solves the member's own type parameters is the arguments that have one to solve.
    val (passed, tail) = args.splitAt(fd.params.length - 1)
    val spell       = genericSelf.get(fd.name).fold((r: TypeRef) => r)((ref, _) => spellSelf(_, ref))
    val ptypes      = fd.params.tail.map(p => spell(p.typ))
    // The bounds go through it too, because a bare arrow's shape lives in one rather than in the
    // parameter — see `spellSelfBounds`.
    val mbounds     = spellSelfBounds(m.bounds, spell)
    // The receiver's arguments, handed to the inference as already answered. `synthesize` lays the
    // owner's parameters ahead of the member's own in one list, which is what makes the `zip` the
    // whole of the pairing — it stops at the shorter, and the shorter is always the owner's.
    //
    // Without this a parameter naming the *type*'s parameter is read as unsolved, and a closure
    // standing at one is held back for a type no argument was ever going to supply. The free
    // function of the same signature has no such gap, because there every parameter is answered by
    // an argument.
    val ownerSeed = fd.tparams.zip(ownerArgs).toMap

    // **Only the member's own parameters are written**, and they settle the call outright where they
    // are: the owner's arrived with the receiver and were never a question, and the solve below is
    // what the written list replaces rather than something it is checked against.
    //
    // A bare arrow among them is the sugar's rather than the author's (`CallCore.authored`), so a
    // member declared `map[U](f: T -> U)` is written `map[int]` and the callable's own parameter
    // goes on being read off the closure, which is where it has always come from.
    val mine    = authored(m.tparams)
    val written =
      if writtenTargs.isEmpty then Map.empty[String, Type]
      else mine.zip(writtenTypeArgs(m.name, mine, m.tvalues, m.tpacks, writtenTargs, atCall = true)).toMap

    val provisional =
      provisionalArgs(fd.name, fd.tparams, ptypes, passed, mbounds, ownerSeed ++ written,
        result = fd.retType.map(spell), expected = expected)

    val own =
      if writtenTargs.nonEmpty && mine.length == m.tparams.length then
        writtenTypeArgs(m.name, m.tparams, m.tvalues, m.tpacks, writtenTargs, atCall = true)
      else
        inDecl(fd.name)(solve(
          shown,
          m.tparams,
          ptypes,
          provisional.map(_.ty),
          fd.retType.map(spell),
          expected,
          passed.zip(provisional).map((a, t) => adaptable(a, t)),
          spellSelfBounds(fd.bounds, spell),
          written,
          passed.map(omittedLiteral),
          provisional.map(deferredDefault),
        ))

    // The member's own bounds, resolved with the receiver's arguments to hand. A bare arrow is
    // sugar for a bound (`MemberLowering.callBounds`), so `f: T -> U` on a member of a generic type
    // writes `Fn(T) -> U` — a bound naming the *owner's* parameter, which nothing in the member's
    // own list can answer. Reading `fd.bounds` here would sweep the owner's own bounds in as well
    // and report anything unmet a second time, having already been said where the receiver's type
    // was made.
    inDecl(fd.name)(checkParamBounds(shown, m.tparams, mbounds, own, ownerSeed))

    val name            = instantiateFunc(fd, ownerArgs ::: own, constSelf)
    val (params, rtype) = funcInsts(name)
    val recvArg         = buildReceiver(m.receiver.get, recv, m.name)

    val checked =
      checkArgs(shown, params, passed, Some(recvArg :: provisional), types = calleeTypes(fd, ownerArgs ::: own))

    checkCrossings(m.crossing, shown, params, checked)
    recheckAfter(recvArg, TCall(name, checked ::: tail.map(variadicArg(_)), rtype))
  }

  /** `k.hash()` — the mixing a built-in's `Hash` membership provides (`reference/expressions.md §
   * Operator dispatch`).
   *
   * `5.add(3)`'s sibling, and built the same way for the same reason: a built-in has no
   * `impl` block, so the lowering is a library function named here. Where `Display` writes into a
   * sink this returns a number, so what the widening is for is the law rather than the signature —
   * every integer, `char`, and `bool` reaches one mixer at 64 bits, so two values that compare
   * equal across widths hash equal too.
   */
  private def builtinHash(rty: Type, mname: String, recv: TExpr, args: List[Expr]): Option[TExpr] =
    for
      _           <- Option.when(mname == "hash")(traitDecls.get(Library.key("Hash"))).flatten
      (fname, to) <- CoreTraits.hash(rty)
    yield {
      val key = Library.key(fname)

      if args.nonEmpty then
        err(s"method 'Hash.hash' takes no arguments, but ${supplied(args.length, "argument")}")

      val self = widen(buildReceiver(RecvMode.ByValue, recv), to)

      funcsUsed += key
      TCall(key, List(self), Type.Integer(64, signed = false))
    }

  /** A member of a trait the compiler supplies membership for, but which is not an operator —
   * `n.abs()`, `n.count_ones()`, `n.rotate_left(3)` (`reference/expressions.md § Operator
   * dispatch`, `CoreTraits.numeric`).
   *
   * **Gated on the trait being in scope**, which is what keeps a compiler-provided membership from
   * being a way around `reference/modules.md § Visibility`. The two questions are different and
   * both have to be answered: a membership settles which *types* have the member, and scope settles
   * which *files* may write it. `Add` and `Display` are unaffected because they are in the standard
   * module, which every file auto-imports; `Signed` and `Bits` are in `sysl.math`, so a program
   * asks for one exactly as it asks for `Float`.
   *
   * **Every signature is read off the trait's own declaration**, the way `builtinMethod` reads
   * `Add`'s, rather than being restated here. That is what makes `rotate_left`'s amount, and the
   * `u32` a count answers, facts stated once — in the library source a reader can see — instead of
   * twice in places that could disagree. `Self` binds to the receiver's **underlying** type, so the
   * magnitude of a constrained subtype is an ordinary integer: a range is a promise about the values
   * that were put in, and nothing says a range that holds `-128` also holds `128`.
   */
  private def builtinNumeric(rty: Type, mname: String, recv: TExpr, args: List[Expr]): Option[TExpr] =
    for
      trName <- CoreTraits.numeric.get(mname)
      if CoreTraits.builtin(trName, rty)
      key = Library.key(trName)
      if traitInScope(key)
      decl <- traitDecls.get(key)
      m    <- decl.methods.find(_.name == mname)
      // Every one of these answers something, and the answer is where the result type comes from. A
      // declaration that stated none would leave nothing to build the node's type out of, so it is
      // matched here rather than assumed: the member then simply does not resolve, which reports
      // the ordinary missing-method complaint instead of failing inside the compiler.
      declared <- m.retType
    yield {
      val width = Type.underlying(rty)
      val self  = selfBinding(width)
      val (params, ret) = inDecl(decl.name) {
        (m.params.map(p => (p.name, resolveType(p.typ, self))), resolveType(declared, self))
      }

      val bound = bindArgs(s"method '$trName.$mname'", Some(decl.name), m.params, args)

      if bound.length != params.length then
        err(s"method '$trName.$mname' takes ${quantity(params.length, "argument")}, " +
          s"but ${supplied(bound.length, "argument")}")

      val recvd = buildReceiver(RecvMode.ByValue, recv)
      val ts    = checkArgs(s"$trName.$mname", params, bound, None)

      TIntOp(mname, recvd, ts.headOption, width, ret)
    }

  /** `int.zero()`, `T.one()` at a solved `T` — a member of a trait the compiler supplies membership
   * for, reached through the **type** because it has no receiver (`reference/expressions.md §
   * Operator dispatch`, `CoreTraits.constants`).
   *
   * This is `builtinNumeric`'s counterpart on the side where there is nothing to lower *from*. A
   * provided member with a receiver becomes an instruction on that receiver's value; one without
   * has no value in hand, so what it lowers to is the constant itself — which is why the number is
   * stated in the table rather than read off anything.
   *
   * The type is already solved by the time this is asked. A generic body's `T.zero()` is checked
   * against the trait's own signature while `T` is abstract (`callBoundAssociated`) and reaches
   * here only at the instantiation, where `T` is an integer and the literal it wants is `TIntLit`
   * at that width. So a `[T: Add + Zero]` accumulator lowers to the same `0` a concrete body would
   * have written, and the membership costs no call and no code.
   *
   * **Asked after the member table, exactly as the receiver-bearing ones are**, so a type that
   * declares a `zero` of its own is not shadowed by this. No integer can, but a written-out block
   * finding the compiler already there is `HoistImpl`'s complaint to make and not a call's.
   */
  protected def builtinAssociated(ty: Type, mname: String, args: List[Expr]): Option[TExpr] =
    builtinConstant(ty, mname, args).orElse(builtinMeasure(ty, mname, args))

  /** `T.width()` — a receiverless member whose answer is read off the type (`CoreTraits.measures`).
   *
   * It is `builtinConstant`'s sibling and differs from it in one place: the number. A `zero()` is
   * `0` whatever the width, so the table states it; a width is the width, so the table names the
   * trait and `CoreTraits.measure` answers. Everything either side of that — the scope gate, the
   * declaration being read rather than assumed, the refusal of arguments — is the same and is the
   * same for the same reasons.
   *
   * **The result type is the declaration's and not the subject's**, which is the one thing that
   * could not be shared with `zero()`. `Zero.zero()` answers `Self` and this answers `u32`, so the
   * literal is built at the type `trait Bits` wrote down — a width is a count of bit positions, and
   * building it at `Self` would make `u8.width()` a `u8` that cannot hold `256`.
   */
  private def builtinMeasure(ty: Type, mname: String, args: List[Expr]): Option[TExpr] =
    for
      trName <- CoreTraits.measures.get(mname)
      if CoreTraits.builtin(trName, ty)
      key = Library.key(trName)
      if traitInScope(key)
      decl <- traitDecls.get(key)
      m    <- decl.methods.find(_.name == mname)
      // Read rather than assumed, exactly as `builtinConstant` reads it: a trait that grew a
      // receiver or a parameter here would otherwise be answered with a number that ignored it.
      if m.receiver.isEmpty && !m.isProperty && m.params.isEmpty
      declared <- m.retType
      value    <- CoreTraits.measure(mname, ty)
    yield {
      if args.nonEmpty then
        err(s"associated function '$trName.$mname' takes no arguments, but " +
          s"${supplied(args.length, "argument")}")

      TIntLit(value, inDecl(decl.name)(resolveType(declared, selfBinding(Type.underlying(ty)))))
    }

  private def builtinConstant(ty: Type, mname: String, args: List[Expr]): Option[TExpr] =
    for
      (trName, value) <- CoreTraits.constants.get(mname)
      if CoreTraits.builtin(trName, ty)
      key = Library.key(trName)
      if traitInScope(key)
      decl <- traitDecls.get(key)
      m    <- decl.methods.find(_.name == mname)
      // The declaration is what says this is a receiverless member taking nothing, and it is read
      // rather than assumed: a trait that grew a parameter would otherwise be answered here with a
      // constant that ignored it, which is a wrong program rather than a refused one.
      if m.receiver.isEmpty && !m.isProperty && m.params.isEmpty
    yield {
      if args.nonEmpty then
        err(s"associated function '$trName.$mname' takes no arguments, but " +
          s"${supplied(args.length, "argument")}")

      TIntLit(value, ty)
    }

  /** `w.get()` — the one thing a `weak T` can be asked (`03`).
   *
   * It yields an ordinary `Option[&T]`, so everything downstream of it — matching, `unwrap`, `?` —
   * is the surface `Option` already has, and nothing about a weak reference reaches further into
   * the language than this call.
   */
  private def weakGet(w: Type.Weak, recv: TExpr, mname: String, args: List[Expr]): TExpr = {
    if mname != "get" then
      err(s"a ${show(w)} has no method '$mname' — a weak reference may be gone, so 'get()' is the " +
        s"only thing to ask one, and what it hands back is what has methods")
    if args.nonEmpty then err(s"'get' takes no arguments")

    val optTy = instantiateEnum(Library.key("Option"), List(w.strong))
    TUpgrade(recv, optTy, optTy.variant("Some").get, optTy.variant("None").get)
  }

  /** `5.add(3)`, `x.lt(y)` — a core-trait method on a type whose membership the compiler provides.
   *
   * A built-in has no `impl` block and so no lowered `int.add` to call; what it has is the operator
   * the trait method *means*, and that is what this builds. The result is the same tree the
   * operator itself would have produced, which is `reference/expressions.md § Operator dispatch`'s
   * promise that a membership changes no codegen — and it is what a monomorphized `[T: Add]` body
   * lands on once `T` is known to be a scalar.
   */
  private def builtinMethod(rty: Type, mname: String, recv: TExpr, args: List[Expr]): Option[TExpr] =
    for
      trName <- CoreTraits.declaring(mname)
      if CoreTraits.builtin(trName, rty)
      decl   <- traitDecls.get(Library.key(trName))
      m      <- decl.methods.find(_.name == mname)
    yield {
      // A built-in's membership is homogeneous (`reference/expressions.md § Operator dispatch`), so
      // the trait's own parameter is the receiver's own type: `5.add(3)` is the `Add[int]` an `int`
      // has, and it has no other.
      //
      // The signature is the **trait's**, so it is resolved in the trait's terms however far from it
      // the call was written — the same rule an instantiated function's signature follows. Reading
      // it here would let a program that declares a type the trait's signature names mean its own by
      // the word, and `Display.display`'s specifier is exactly such a name.
      val params = inDecl(decl.name) {
        m.params.map(p => (p.name, resolveType(p.typ, selfBinding(rty) ++ decl.tparams.map(_ -> rty))))
      }

      val bound = bindArgs(s"method '$trName.$mname'", Some(decl.name), m.params, args)

      if bound.length != params.length then
        err(s"method '$trName.$mname' takes ${quantity(params.length, "argument")}, " +
          s"but ${supplied(bound.length, "argument")}")

      val self             = buildReceiver(RecvMode.ByValue, recv)
      val ts               = checkArgs(s"$trName.$mname", params, bound, None)
      val (_, op, kind)    = CoreTraits.required(trName)

      kind match
        case CoreTraits.Kind.Arith   => produced(TBinary(op, self, ts.head, arithType(op, self.ty, ts.head.ty, ts.head.pos)))
        case CoreTraits.Kind.Compare => TCompare(List(self, ts.head), List(TCmp(op)))
        case CoreTraits.Kind.Prefix  => produced(TUnary(op, self, unaryType(self.ty)))
    }

}
