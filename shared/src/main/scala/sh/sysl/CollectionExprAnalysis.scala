package sh.sysl

/** The expressions that build a sequence or reach into one: an array literal, a repeat, a slice,
 * and a subscript.
 *
 * They belong together because they share one question — how many elements are there, and is this
 * index one of them. An array's count is part of its type and a view's is carried beside its
 * elements, so the same subscript is checked against a constant in one case and against a loaded
 * word in the other; a `*T` has neither and is checked against nothing, which is what `*T` is.
 *
 * Split out of `ExprAnalysis`, whose expression dispatch calls in here.
 */
trait CollectionExprAnalysis extends ExprSupport {

  /** An array form, a repeat, a slice, or a subscript. The parameter names the four so that the
   * match below is exhaustive over exactly what the dispatch sends here and nothing else.
   */
  protected def sequenceExpr(expr: ArrayLit | ArrayFill | Index, expected: Option[Type]): TExpr = expr match
    case ArrayLit(elems) =>
      val elemExp = expected.flatMap(elementWanted)
      // With nothing asked of it, the literal's elements are operands that must share one type, and
      // they settle it the way the two sides of an operator do: a bare literal takes the type of an
      // element that has one, wherever that element stands. So `[1usize, 2, 7]` and `[1, 2usize, 7]`
      // are both `[3]usize`, as `val n: usize = 2` is a `usize` — and `[1u8, 300]` is refused at the
      // `300` for not fitting, rather than for being an `int` nobody wrote. Where the position does
      // ask for an element type, every element is read at that type and nothing here decides it.
      val ts      = if elemExp.isEmpty then analyzeOperands(elems, None) else elems.map(analyzeExpr(_, elemExp))

      for t <- ts do
        if Type.noValue(t.ty) then err(s"an array cannot hold ${show(t.ty)} values")
        if t.ty != ts.head.ty then
          err(s"an array literal needs one element type, got ${show(ts.head.ty)} and ${show(t.ty)}")

      val elemTy = ts.headOption.map(_.ty).orElse(elemExp).getOrElse(
        err("an empty array literal takes its element type from its context, and there is none here"),
      )

      expected match
        // The buffer is storage this expression makes, so nobody else can be holding a writable
        // view of it and it takes whichever form was asked for. Under a `[]const T` that is a
        // buffer written by its own literal and never again.
        case Some(Type.Slice(_, ro)) => TBufLit(ts, Type.Slice(elemTy, ro))
        // The lane count is part of the type, so a literal that does not fill it is a mistake with
        // an exact number in it — unlike an array, whose length the literal is free to decide.
        case Some(v: Type.Vector) =>
          if ts.length != v.length then
            err(s"${show(v)} has ${v.length} lanes and this literal has ${ts.length} — a vector's " +
              s"width is part of its type, so the two have to agree")

          TVectorLit(ts, Type.Vector(v.length, elemTy))
        case _ => TArrayLit(ts, Type.Array(ts.length, elemTy))

    // `[v; n]` — the form for an array whose element type has no zero, or has one that is not the
    // wanted starting value. The value is evaluated **once** and copied into every element, which
    // is what makes `[f(); 8]` mean one call rather than eight.
    //
    // What is being asked for decides which of the two things this is (`reference/arrays.md §
    // Storage sized while running`). Under a `[N]T` the count is part of the type and so a
    // compile-time constant; under a `[]T` the length is not in the type at all, so the count is an
    // ordinary expression and the elements are storage of their own that the view owns.
    case ArrayFill(value, count) =>
      val elemExp = expected.flatMap(elementWanted)
      val tv      = analyzeExpr(value, elemExp)

      if Type.noValue(tv.ty) then err(s"an array cannot hold ${show(tv.ty)} values")

      expected match
        // `[v; n]` where a vector was asked for is the splat written out, and it is accepted for
        // the reason the array literal's spelling is: the reader has written the array form of a
        // thing a vector also has, and refusing it would send them to a different spelling for no
        // reason they could see. The count is still checked against the lane count, since a vector
        // that does not fill its width is the same mistake as a literal that does not.
        case Some(v: Type.Vector) =>
          constInt(count, tsubst) match
            case Some(n) if n == v.length =>
            case Some(n) =>
              err(s"${show(v)} has ${v.length} lanes and this repeat makes $n")
            case None =>
              err(s"${show(v)} has ${v.length} lanes, which is part of its type — so a repeat " +
                "filling one needs a constant count")

          TSplat(coerce(tv, v.elem), v)

        case Some(Type.Slice(_, ro)) =>
          val tc = analyzeExpr(count)

          // A count is an index's twin, so it takes a transparent subtype for the same reason one
          // does — and refuses a derived one for the same reason too. A count wider than an address
          // needs no rule of its own either: it reaches `widenIndex` like every index does, and that
          // traps on a value which does not fit rather than sizing storage by a number nobody wrote.
          val checked = Type.repr(tc.ty) match
            case _: Type.Integer => tc
            case _ => err(s"a repeat count is a number of elements, and ${show(tc.ty)} is not an integer")

          TBufFill(tv, checked, Type.Slice(tv.ty, ro))

        case _ =>
          val n = constInt(count, tsubst) match
            case Some(v) if v >= 0 && v.isValidInt => v.toInt
            case Some(v)                           => err(s"an array cannot have $v elements")
            case None =>
              err("an array's repeat count must be a constant, since it is the array's bound — a " +
                "literal, or a 'const' naming one. A count computed while running makes storage " +
                "instead, which is written where a '[]T' is expected")

          TArrayFill(tv, Type.Array(n, tv.ty))

    // A range subscript takes a view. The receiver is left *undereferenced* on purpose: for a
    // heap array the reference is both where the elements are and what keeps them alive, and
    // evaluating it once is what makes those the same object.
    case Index(receiver, RangeExpr(lo, hi, inclusive)) =>
      if !inclusive && hi.isEmpty then err("an open-ended slice is written 'a[lo..]'")

      // **An array literal is the one receiver with no type of its own**, so a view of one is the
      // one place the *view's* expectation has to reach through to the elements: `[1, "hi", true][..]`
      // at a `[]const &Display` is three erasures, and read with nothing expected it is three types
      // that are told they do not agree. Every other receiver is storage that already has a type,
      // and asking it for one would be the expectation overruling what is really there.
      val elemHint = (receiver, expected) match
        case (ArrayLit(es), Some(Type.Slice(elem, _))) => Some(Type.Array(es.length, elem))
        case _                                         => None

      val tr = analyzeExpr(receiver, elemHint)

      // A view of read-only storage is read-only, which is the whole of what it takes to make this
      // safe: the view records the property, so it carries it wherever it is bound or passed rather
      // than losing it at the end of the expression that made it. Slicing a `val` was refused for
      // exactly as long as there was no type to say that in.
      //
      // A `[]const T` the context asked for is answered directly, the way the two forms above answer
      // one, rather than by making a writable view and then giving the ability up. The result is the
      // same view either way; what it changes is that a writable one is never made, so asking for a
      // read-only view of storage no writable view may be taken of is the ordinary thing it reads as.
      val viewIsConst = readOnly(tr) || expected.exists(Type.readOnlyView)

      val elem = tr.ty match
        case Type.Ref(Type.Array(_, e), false) => e
        case Type.Ref(Type.Array(_, _), true) =>
          err("a slice does not record whether its owner's count is atomic, so a '&sync' array cannot be sliced")
        case w: Type.View               => w.elem
        case Type.Array(_, e)           => e
        case Type.Ptr(Type.Array(_, e)) => e

        // A view of a `*T` region — the shape every C function that fills a buffer hands back. The
        // pointer carries no length, so the end must be written and is taken on trust; that is the
        // same trust `*p` already asks for, and the resulting view owns nothing (`05`).
        case Type.Ptr(e) =>
          if hi.isEmpty then
            err(s"a ${show(tr.ty)} carries no length, so a view of one needs its end written — " +
              "'p[0..<n]' with the number of elements that are really there")
          e

        // A type read by a subscript through `Index` cannot be *sliced* through it, and
        // `library/core.md § Walking a type of your own` says why: the index would have to be a
        // range, and a range is not yet a type a program can name, so there is nothing for the
        // trait's argument to be. Somebody who has written an `Index` and is reaching for the
        // neighbouring form is owed that rather than "cannot slice", which reads as though their
        // type were the wrong shape for an operation that exists.
        case other if indexes(Library.key("Index"), other) =>
          err(s"${show(other)} is read by a subscript through '${qn(Library.key("Index"))}', but slicing " +
            "through the trait is not built: the index would have to be a range, and a range is not yet " +
            "a type a program can name")

        case other => err(s"cannot slice ${show(other)}")

      // Part of a string is a string, not a `[]u8` — the bytes between two character boundaries
      // are still well-formed UTF-8, which is what the check at those boundaries is for.
      //
      // Anything else is a slice, read-only when what it views is: `val` storage, or another view
      // that had already given the ability up. Re-slicing is where a bit like this is easiest to
      // drop, and dropping it would make `xs[1..]` the way around `xs`.
      val viewTy =
        if tr.ty == Type.Str then Type.Str
        else Type.Slice(elem, readOnly = viewIsConst || Type.readOnlyView(tr.ty))

      // A view that may be written is an alias like any other, so it is refused where a `&` would
      // be: over storage inside a struct whose invariant reads it (`reference/errors.md § Struct
      // invariants`).
      checkSliceable(tr, viewTy)
      TSlice(tr, lo.map(bound), hi.map(bound), inclusive, viewTy)

    case Index(receiver, index) =>
      val raw = analyzeExpr(receiver)

      // `p[i]` on a raw pointer is C's, unchecked: the pointer is a bare address, so the subscript
      // is the address arithmetic C spells the same way. It is read off the *undereferenced*
      // receiver — a pointer to an array keeps the checked form below, because that one has a
      // length in its type to check against.
      val tr = raw.ty match
        case Type.Ptr(_: Type.Array) => autoDeref(raw)
        case _: Type.Ptr             => raw
        case _                       => autoDeref(raw)

      Type.repr(tr.ty) match
        // A lane, which is read out of a register rather than through an address — so the index is
        // held to being a constant and checked here, where there is a number to name. See `TLane`.
        case v: Type.Vector =>
          // The index is analyzed for its *type* even though its value comes from `constInt`, so
          // that `v[k]` where `k` is a string is told it is not an integer rather than told it is
          // not a constant — which is true of it and is the second thing wrong with it.
          val ti = analyzeExpr(index, Some(Type.usize))

          if !Type.repr(ti.ty).isInstanceOf[Type.Integer] then
            at(index.pos)(err(s"an index must be an integer, not ${show(ti.ty)}"))

          constInt(index, tsubst) match
            case Some(n) if n >= 0 && n < v.length => TLane(tr, n.toInt, Type.unqualified(v.elem))
            case Some(n) =>
              at(index.pos)(err(s"${show(v)} has lanes 0 to ${v.length - 1}, and this is $n"))
            case None =>
              at(index.pos)(err(s"a lane index is part of the instruction that reads it, so it has " +
                s"to be a constant — a literal, or a 'const' naming one. To index ${show(v)} by a " +
                s"computed number, keep the values in a '[${v.length}]${Type.show(v.elem)}'"))

        case _ => indexNonVector(raw, tr, index, expected)

  /** Everything a subscript may be applied to that is not a vector — an array, a view, a `*T`, or a
   * type with an `Index` of its own. Split out only so the vector case above can stand beside it.
   *
   * `raw` is the receiver as written and `tr` the same one dereferenced; the trait path needs the
   * first, because a method is looked up on what the reader named.
   */
  private def indexNonVector(raw: TExpr, tr: TExpr, index: Expr, expected: Option[Type]): TExpr =
      Type.element(tr.ty) match
        case Some(elem) =>
          val ti = analyzeExpr(index, Some(Type.usize))

          // A transparent constrained subtype stands where its base does, so an `Index within 0..<n`
          // indexes without a cast. A derived one does not: `new` is nominal, and reaching the base
          // is exactly what a written conversion is for.
          Type.repr(ti.ty) match
            // The element's qualifier stays on the receiver's type and comes off the value read out
            // of it, exactly as a field's does (`reference/memory.md § Device memory`).
            case _: Type.Integer => TIndex(tr, ti, Type.unqualified(elem))
            // At the index's own position rather than the subscript's: the message is about what
            // was written between the brackets, and a caret on the `[` points one character to the
            // left of the thing being complained about.
            case other           => at(index.pos)(err(s"an index must be an integer, not ${show(other)}"))

        // A type with no elements of its own is indexed through `Index`, whose one method the
        // subscript *is* (`reference/expressions.md § Operator dispatch`). The index is not held to
        // being an integer here: what a container is read by is the trait's own argument, and a
        // type that indexes by something else is implementing a different `Index` rather than
        // misusing this one.
        case None if indexes(Library.key("Index"), tr.ty) => callMethodOn(raw, "index", List(index), expected)

        case None =>
          tr.ty match
            // A parameter has no implementation to find — `indexes` asks after one, and a bound is
            // not that — so the subscript goes the way every other use of a parameter goes: to the
            // member machinery, which answers from the bounds and complains through `boundErr` when
            // they license nothing. A subscript **is** `Index`'s one method
            // (`reference/expressions.md § Operator dispatch`), so this is the same question the
            // dot form asks, and `reference/generics.md § Bounds` puts indexing among what an
            // unbounded parameter may not do. Without this the definition-time pass dropped the
            // complaint and the reader met it at whatever first instantiated the body, against a
            // type the definition never named — which is the outcome §5 exists to prevent.
            case _: Type.Abstract => callMethodOn(raw, "index", List(index), expected)
            case other            => err(s"cannot index ${show(other)}")


  /** One end of a slice range: an index like any other, so any integer will do. */
  protected def bound(e: Expr): TExpr = {
    val t = analyzeExpr(e, Some(Type.usize))

    t.ty match
      case _: Type.Integer => t
      case other           => err(s"a slice bound must be an integer, not ${show(other)}")
  }

  /** Whether a subscript on this type reaches one of the two indexing traits, asked of a type that
   * has no elements of its own. A built-in's subscript is never this: an array, a slice and a
   * string are indexed by the compiler, and nothing a program writes competes with that.
   */
  protected def indexes(traitName: String, ty: Type): Boolean =
    implsOf(traitName, memberOwner(ty)._1).nonEmpty

  /** The same, asked of an assignment target before the statement has committed to being a store.
   * Whatever goes wrong while typing the receiver is left for the ordinary path to report, in the
   * place the programmer wrote it.
   */
  protected def indexes(traitName: String, receiver: Expr): Boolean =
    probe(autoDeref(analyzeExpr(receiver)).ty)
      .exists(t => Type.element(t).isEmpty && indexes(traitName, t))
}
