package sh.sysl

/** The call forms the compiler resolves by name.
 *
 * A call is normally a name looked up among the program's declarations. These ten are not: the
 * analyzer recognizes `print`, `str`, `format`, `str_cast`, `str_alias`, `va_start`, `va_end`,
 * `va_arg`, `va_copy`, and `ptr_cast` before it gets that far. Collecting them in one file is
 * deliberate — it is the whole of what the language knows that a program could not have told it, and
 * the list is meant to shrink.
 *
 * `ptr_cast` is the exception that is written elsewhere: it belongs to the raw tier alongside
 * `sizeof`, so its meaning is in `RawStorage` and only its *name* is registered here, which is what
 * the dispatch and the mistake-at-a-form diagnostic below both need.
 *
 * The four `va_*` forms belong here permanently. Each is an **ABI primitive** that no sysl body
 * could implement, in the same category as `sizeof`, so there is nothing to put in the library
 * (`12` §9). `str_cast` is permanent for the same reason from the other direction: every safe route
 * to a `string` carries the UTF-8 guarantee, so the one operation that sets it aside can only come
 * from underneath the language. **What is not permanent is the NAME a program reaches for**, and it
 * is no longer this one: `sysl.text.from_utf8_unchecked` is an ordinary library function whose body
 * is a call to this form, so the pair a reader compares — validating and not — sits together in the
 * module that owns strings. This is the raw tier's spelling, beside `ptr_cast`, and it reinterprets
 * for the same reason that one does. The other three are the temporary ones: all three are now
 * desugarings onto a `Display`, and what keeps them here is the **arity** — `print(a, b, c)` is
 * variadic and heterogeneous, and sysl has no overloading — together with the buffer `str` and
 * `format` render into, which is a growable byte array the library cannot yet name. The sink
 * `print` writes into is no longer one of the reasons: it is an ordinary library value now.
 */
/** The names the forms below answer to, in an object so that the import resolver can ask.
 *
 * It is read in two places that must not disagree: the analyzer's dispatch, and the refusal an
 * `import` gives when somebody guesses at a module for one of these. A form belongs to no module, so
 * that refusal has to say which of the two situations the reader is in rather than reporting a name
 * that does not exist — which is what it used to do, of a name that works one line below.
 */
object SpecialForms {

  val names: Set[String] =
    Set("print", "str", "format", "str_cast", "str_alias", "va_start", "va_end", "va_arg", "va_copy",
      "ptr_cast") ++
      Atomics.names
}

trait SpecialForms extends Closures {

  /** The names recognized here, so that a mistake written *at* one of them can name the form rather
   * than falling through to the general complaint about a callee. Nothing looks these up, so
   * nothing else would know they are forms at all.
   */
  protected val specialFormNames: Set[String] = SpecialForms.names

  /** `print(a, b, …)` — each value rendered by the library function its type reaches, a space
   * between and a newline at the end.
   *
   * This is a **desugaring**, not a builtin: the compiler knows a handful of *names* the way it
   * already knows `Option`'s variants, and implements no printing of its own. A scalar reaches the
   * library renderer for its width; everything else reaches its own `Display`, which is what
   * putting the seam at a name was for.
   *
   * The space and the newline go out as *characters* rather than one-character strings, so that
   * printing a number reaches nothing that allocates: a `string` is reference-counted, and a single
   * `prints(" ")` would pull the whole ARC runtime and an allocator into a program whose own code
   * never asks for either.
   */
  protected def printCall(args: List[Expr]): TExpr = {
    val parts = args.zipWithIndex.flatMap { case (a, i) =>
      val sep = if i == 0 then Nil else List(printChar(' '))

      sep :+ printOne(analyzeExpr(a))
    }

    TSeq(parts :+ printChar('\n'))
  }

  /** One value, rendered by the library function that takes its type.
   *
   * Every argument is widened to the one width its renderer takes — the integers to `long` or
   * `ulong`, the floats to `real` — so the library needs one function per *kind* rather than one
   * per type, which sysl has no overloading to hide anyway.
   *
   * A scalar keeps this direct path rather than going through its `Display` (`library/core.md §
   * Rendering to standard output`): the two render identically, and the one that does not build a
   * sink is the one to emit. Everything else writes itself into standard output through the trait.
   */
  private def printOne(t: TExpr): TExpr = t.ty match
    // A width past 64 bits has no printf conversion to be widened to, so it renders itself first
    // and goes out as the string that came back. That is the one scalar whose printing allocates,
    // and it allocates because C cannot be asked to do this one.
    case i: Type.Integer if i.bits > 64  => callLibrary("prints", TStr(t))
    case i: Type.Integer if i.signed => callLibrary("printi", widen(t, Type.Integer(64, signed = true)))
    case _: Type.Integer             => callLibrary("printu", widen(t, Type.Integer(64, signed = false)))
    case _: Type.Floating            => callLibrary("printr", widen(t, Type.Real))
    case Type.Bool                   => callLibrary("printb", t)
    case Type.Char                   => callLibrary("printc", t)
    case Type.Str                    => callLibrary("prints", t)
    case _ =>
      renderer(t, "print") match
        case (_, value, Some(slot)) => TVCall(value, slot, List(stdout(), plainSpec), Type.Unit)
        case (method, value, None)  => TCall(method, List(value, stdout(), plainSpec), Type.Unit)

  /** The separator and the terminator `print` puts around its values. */
  private def printChar(c: Char): TExpr = callLibrary("printc", TIntLit(c.toInt, Type.Char))

  /** A call to a library function, built from an already-analyzed argument. Recording the name is
   * what brings the function itself into the program: a library declaration nothing reaches is
   * neither analyzed nor emitted.
   *
   * `name` is the spelling the library declares it under, and the key it is filed as is
   * `Library`'s to say — this is the one place the compiler calls a function it named itself rather
   * than one a program wrote, so it is the one place that translation belongs.
   */
  protected def callLibrary(name: String, arg: TExpr): TExpr = {
    val key        = Library.key(name)
    val (_, rtype) = funcInsts(key)

    funcsUsed += key
    TCall(key, List(arg), rtype)
  }

  /** `str(x)` renders a value of a primitive type, and a `string` as itself. Anything else writes
   * itself into a buffer through its `Display`, and the bytes that land there become the string.
   */
  protected def strCall(args: List[Expr]): TExpr = {
    if args.length != 1 then err("str takes exactly one value")
    val t = analyzeExpr(args.head)

    t.ty match
      case _: Type.Integer | _: Type.Floating | Type.Bool | Type.Char | Type.Str => TStr(t)
      case _ =>
        val (method, value, slot) = renderer(t, "str")

        TRender(value, method, plainSpec, slot)
  }

  /** `str_cast(b)` — a `[]u8` taken as a `string` without looking at it.
   *
   * This is the whole of what the library's `from_utf8` cannot write for itself: it validates, and
   * then it needs somewhere to say "these bytes are a string now". `04` puts the unchecked form in
   * the `*T` tier deliberately — breaking the UTF-8 invariant breaks `char`'s downstream.
   *
   * **A program does not write this; it writes `sysl.text.from_utf8_unchecked`**, which is an
   * ordinary library function over this form and is where the long, greppable spelling lives now.
   * Keeping the primitive under a raw-tier name rather than the readable one is what lets the two
   * halves of the pair — validating and not — sit beside each other in `sysl.text`.
   *
   * A `string` argument is refused rather than passed through. It would be the identity, but the
   * only way to write one is to have gone looking for this function, and a program that reaches for
   * the unchecked conversion on a value that is already checked has misunderstood which direction it
   * is going.
   */
  protected def strCast(args: List[Expr]): TExpr = {
    if args.length != 1 then err("'str_cast' takes exactly one value, the bytes to take as a string")
    val t = analyzeExpr(args.head)

    Type.underlying(t.ty) match
      // Either view will do: making a string out of bytes reads them, and a `[]const u8` is the
      // shape `s.bytes` hands over, which is the commonest thing anyone validates.
      case Type.Slice(Type.Byte, _) => TFromBytes(t)
      // An array of bytes is a view of itself here, on the same terms every other position takes one
      // on. This form asks for a `[]u8` and is one of the places that asks, so an array reaching it
      // need be sliced by hand no more than an argument to a `[]u8` parameter does.
      case Type.Array(_, Type.Byte) => TFromBytes(coerce(t, Type.Slice(Type.Byte, readOnly = true)))
      case Type.Str =>
        err("'str_cast' makes a string out of bytes, and this value is already a string")
      case other =>
        err(s"'str_cast' takes a []u8, but the value has type ${show(other)}")
  }

  /** `str_alias(b)` — a `[]u8` taken as a `string` without looking at it **and without copying
   * it**: the string is the slice's three words, sharing its owner the way `s[a..b]` shares a
   * string's.
   *
   * `str_cast` copies because a `[]u8` can be written afterwards; this is the form for the program
   * that needs the string to see those writes — a buffer grown in place, whose text is read back as
   * a `string` at every step without paying for a copy of the whole of it each time. What it gives
   * up is therefore more than `str_cast` does: the caller also owes that nothing writes the viewed
   * bytes while the string is alive. **A program does not write this either; it writes
   * `sysl.text.str_view`**, which says so where a reader will look.
   *
   * The storage is still the compiler's business. A string outlives every frame, so a view of an
   * array the frame owns moves that array to the heap (`Escape`), and a view with no counted owner
   * — a `*T` region — is the programmer's problem exactly as every `*T` is.
   */
  protected def strAlias(args: List[Expr]): TExpr = {
    if args.length != 1 then err("'str_alias' takes exactly one value, the bytes to view as a string")
    val t = analyzeExpr(args.head)

    Type.underlying(t.ty) match
      case Type.Slice(Type.Byte, _) => TStrView(t)
      case Type.Array(_, Type.Byte) => TStrView(coerce(t, Type.Slice(Type.Byte, readOnly = true)))
      case Type.Str =>
        err("'str_alias' makes a string out of bytes, and this value is already a string")
      case other =>
        err(s"'str_alias' takes a []u8, but the value has type ${show(other)}")
  }

  /** `format(value, "%spec")` renders one value through a printf specifier. It is the desugaring of
   * an `f"…"` hole, so the specifier is always a literal here; the lexer has vetted its shape, and
   * what is left is checking the conversion against the value's type.
   *
   * A type that renders itself has no printf conversion, so `%s` is what reaches it — and the
   * specifier is handed on rather than applied, since only the implementation knows what a width
   * means for the text it is about to write. A **number** keeps the strict check: `%s` on an
   * integer stays the mistake it was, since `%d` and `%g` are what it is for and writing `%s`
   * instead is a confusion about which value is in the hole rather than a rendering choice.
   *
   * The line is drawn at *numbers* rather than at built-ins, and it has to be: every built-in now
   * renders through an `impl`, so conforming no longer tells a scalar from a program's struct. What
   * it tells apart is a type with a conversion of its own from one without — `bool` and `char` have
   * none, so `%s` is the only way to write them and refusing it would leave them unwritable.
   */
  protected def formatCall(argExpr: Expr, spec: String): TExpr = {
    val t = analyzeExpr(argExpr)
    val c = FormatSpec.conversion(spec)

    // A specifier is applied by `snprintf`, and C has no length modifier for an argument wider than
    // `long long` — so the one thing that cannot be honoured here is a width past 64 bits. It is
    // refused by name rather than silently narrowed, and the message names `str`, which renders such
    // a value without a specifier because the language rather than libc writes its digits.
    Type.underlying(t.ty) match
      case i: Type.Integer if i.bits > 64 && FormatSpec.isInt(c) =>
        err(s"'$spec' is applied by C's formatting, which has no conversion for the ${i.bits} bits " +
          s"of ${show(t.ty)} — render it with 'str' instead")
      case _ =>

    val ok =
      if FormatSpec.isInt(c) then t.ty.isInstanceOf[Type.Integer]
      else if FormatSpec.isFloat(c) then t.ty.isInstanceOf[Type.Floating]
      else t.ty == Type.Str

    if ok then TFormat(t, spec)
    else if FormatSpec.isStr(c) && rendersItself(t.ty) && !numericBuiltin(t.ty) then
      val (method, value, slot)   = renderer(t, "format")
      val (width, prec, left)     = FormatSpec.parts(spec)

      TRender(value, method, specValue(width, prec, left), slot)
    else err(s"format '$spec' expects ${FormatSpec.expects(c)}, but the value has type ${show(t.ty)}")
  }

  // --- rendering through Display ---------------------------------------------------------

  /** The `display` a value renders through, and the value in whatever width that renderer takes.
   *
   * The answers are `reference/expressions.md § Operator dispatch`'s one dispatch rule, applied to
   * rendering: a **built-in** reaches the library renderer its membership provides (`§5`), a **user
   * type** the member its `impl` produced, and a **bounded type parameter** the trait's own method,
   * since which implementation runs is monomorphization's to decide once a concrete type is known.
   * A **trait object** answers with a slot rather than a name, since which `display` runs is a word
   * in its table — and it has one exactly when the trait it was erased to **requires** `Display`
   * (`02`). That is what a supertrait buys the ordinary way of printing: without one, a value stops
   * being printable at the moment it is erased, which is the moment a program most wants to
   * describe what it is holding.
   *
   * `op` is the form that asked, so a type that renders no way at all is complained about in the
   * words the programmer wrote rather than in the machinery underneath them.
   */
  private def renderer(t: TExpr, op: String): (String, TExpr, Option[Int]) = {
    // A subtype renders exactly as its base does — a number or a character — so it reaches the
    // base's renderer rather than asking for a `Display` impl of its own. That is what a subtype is
    // for, and it is what keeps `type Percent = int :: 0..100` printing like the integer it is.
    //
    // **Unless it has said otherwise**, which `reference/traits.md § override — when the overlap is deliberate` made writable and nothing had to
    // before. A derived subtype has its base's memberships (`reference/errors.md § A derivation
    // inherits its base's behaviour and may replace none of it`), so the library's blanket over
    // every integer already covers it and a block of its own is an override — the one case where
    // the type has a renderer the base does not. Collapsing first would find the base's, which
    // would make that block compile and do nothing.
    checkWriterShape()

    if implsOf(displayTrait, ownerKey(t.ty)).nonEmpty then t.ty else Type.underlying(t.ty)
  } match
    case a: Type.Abstract =>
      if !satisfies(displayTrait, a) then boundErr(s"'$op' needs '${show(a)}: ${qn(displayTrait)}'")
      (displayMethod, t, None)

    case Type.Ptr(o: Type.Trait) => objectRenderer(t, o, op)
    case Type.Ref(o: Type.Trait, _) => objectRenderer(t, o, op)

    // Every other type renders through an `impl`, the built-ins included: the closed families have
    // one written per type and the integers are covered by the blanket block over `Integer`. There
    // is no compiler-picked renderer left to try first, which is what makes this one path.
    case ty =>
      if !conforms(displayTrait, ty) then
        // Naming the `impl` to write is only advice where one could be written at all: a memory
        // mode is the shape `02` refuses, so it is told what is true of it rather than pointed
        // at a block that would not compile. A generic type is written for as a whole, so the
        // advice names the block's own parameters rather than the arguments this value has. And
        // a type an implementation already covers is told what that implementation asked of it,
        // since writing a second one is exactly what it may not do.
        //
        // The trait is named by the **key**, never spelled: advice is the one place where being
        // shown a name that means something else is worse than being shown a long one. A program
        // with a `Display` of its own reaches the library's only by path, and telling it to write
        // `impl Display` would have it implement its own trait and be refused again for the same
        // reason.
        val tr  = qn(displayTrait)
        val fix = unmetBound(displayTrait, ty).getOrElse(ty match
          case n: Type.Named if n.targs.nonEmpty =>
            val tps = nominalTparams(n.base).mkString(", ")
            s"write an 'impl[$tps] $tr for ${qn(n.base)}[$tps]' to say how it renders"
          case n: Type.Named => s"write an 'impl $tr for ${show(n)}' to say how it renders"

          // An **array** no longer reaches any of this, and nothing here needs a case for one: the
          // library covers every array whose element renders, so `unmetBound` above answers first
          // and names the element that fails that block's condition — the same answer a slice gets,
          // and the part a reader can act on. What used to stand here was advice to take the
          // whole-array view, written when a length was part of an array's shape and no block could
          // be generic over it (`reference/generics.md § A parameter may stand for a value`).

          // A composed type is the module's when anything named in it is (`reference/traits.md §
          // Where an impl may live`), so the advice holds for a `[]Point` and is impossible for a
          // `[]int`: `Display` is the library's and nothing in `[]int` is this module's, so the
          // block named here is one `checkCoherence` refuses. Both diagnostics used to arrive in
          // the same run — the rule saying the `impl` has no home, and this line telling the reader
          // to write it.
          case _: Type.Array | _: Type.Slice if implementableHere(ty) =>
            s"write an 'impl $tr for ${show(ty)}' to say how it renders"

          case _: Type.Array | _: Type.Slice =>
            s"nothing renders a ${show(ty)}, and an 'impl $tr' for it has no home outside " +
              "the library — print the elements, or give them a type of your own to be held in"

          case _ => s"it does not implement '$tr'")
        val asked = if op == "print" then "cannot print" else "cannot make a string of"
        err(s"$asked a ${show(ty)} value — $fix")

      val fname = memberFuncName(ty, "display")

      funcsUsed += fname
      (fname, t, None)

  /** Whether an `impl` for this type could be written **here** — the coherence question
   * (`reference/traits.md § Where an impl may live`), asked of a resolved type rather than of a
   * written subject.
   *
   * `Display` belongs to the library, so the only thing that can give the block a home is a type of
   * this module's named somewhere in the subject. The walk goes through the composed shapes for that
   * reason: it is the *elements* that carry the licence, which is what makes `[]Point` writable and
   * `[]int` not.
   */
  private def implementableHere(ty: Type): Boolean = ty match
    case Type.Array(_, elem) => implementableHere(elem)
    case Type.Slice(elem, _) => implementableHere(elem)
    case n: Type.Named =>
      declaringModule(n.base).contains(currentModule) || n.targs.exists(implementableHere)
    case _ => false

  /** The `display` slot in a trait object's table, or why the object has none.
   *
   * The advice is what tells the two failures apart. A trait that could require `Display` and does
   * not is told to, since that is a one-word change to a declaration; the object type itself is not
   * the thing to change, and neither is the value, which very likely implements `Display` already
   * and lost it at the erasure.
   */
  private def objectRenderer(t: TExpr, o: Type.Trait, op: String): (String, TExpr, Option[Int]) =
    displaySlot(o) match
      case Some(slot) => (displayMethod, t, Some(slot))
      case None =>
        val asked = if op == "print" then "cannot print" else "cannot make a string of"

        err(s"$asked a ${show(t.ty)} value — an object offers what its trait declares and what that " +
          s"trait requires, so write 'trait ${qn(o.name)}: ${qn(displayTrait)}' to keep the rendering the value " +
          "had before it was erased")

  /** The library's `Display`, and the member of it every rendering goes through. Both are keys, so
   * they are asked of `Library` rather than spelled — everything below reaches the trait the library
   * declares and never one a program happens to have named the same.
   */
  private def displayTrait: String = Library.key("Display")

  private def displayMethod: String = s"$displayTrait.display"

  /** Which slot of a trait object's table holds `Display`'s renderer, if the trait requires it. */
  private def displaySlot(o: Type.Trait): Option[Int] =
    slottedMembers(o.bound).zipWithIndex.collectFirst {
      case ((from, m), slot) if from.name == displayTrait && m.name == "display" => slot
    }

  /** Whether a type renders through an `impl` rather than through a printf conversion — which is
   * the case `format` hands its specifier on for instead of applying it.
   */
  /** Whether the value is a number the language has a printf conversion for, which is what keeps
   * `%s` on one the mistake it is. A constrained subtype is asked at its base, since `16` gives it
   * the base's whole catalog and `%d` reaches it exactly as it reaches an `int`.
   */
  private def numericBuiltin(ty: Type): Boolean = Type.isNumeric(Type.underlying(ty))

  private def rendersItself(ty: Type): Boolean = ty match
    case a: Type.Abstract           => satisfies(displayTrait, a)
    case Type.Ptr(o: Type.Trait)    => displaySlot(o).isDefined
    case Type.Ref(o: Type.Trait, _) => displaySlot(o).isDefined
    case _                          => conforms(displayTrait, ty)

  /** The compiler's own writers are laid out by hand and reached by slot index, so `Writer`'s shape
   * is something the emitter depends on rather than merely reads. Checking it here turns a later
   * edit to the library into a failed build instead of a call through the wrong slot.
   */
  private def checkWriterShape(): Unit = {
    // The **flattened** list, which is what a table is and what every call site indexes into: a
    // trait offers what it requires before what it declares, so `Writer: Fallible` puts `failed`
    // in the first slot and `write` in the second. Reading the declaration alone would have missed
    // exactly the change that moved them.
    val offered = traitMembers(Type.Bound(Library.key("Writer"), Nil)).map(_._2.name)

    if offered != WriterEmitter.members then
      sys.error(s"the compiler's own writers are laid out as " +
        s"${WriterEmitter.members.mkString("'", "', '", "'")}, " +
        s"but 'Writer' offers ${offered.mkString("'", "', '", "'")}")
  }

  /** Standard output as a sink — an ordinary call to the library function that hands one out.
   *
   * There is nothing for the compiler to build here. The sink is a fieldless struct with an
   * `impl Writer` of its own, so the table is the one any erasure of that type produces and the
   * behaviour is library sysl; recording the name is all this does, since a library declaration
   * nothing reaches is neither analyzed nor emitted.
   */
  private def stdout(): TExpr = {
    val key        = Library.key("stdout")
    val (_, rtype) = funcInsts(key)

    funcsUsed += key
    TCall(key, Nil, rtype)
  }

  /** The specifier `print` and `str` hand a `Display`: no width, no precision, no justification,
   * since neither was written with one.
   */
  private def plainSpec: TExpr = specValue(0, -1, left = false)

  private def specValue(width: Int, prec: Int, left: Boolean): TExpr = {
    val s = instantiateStruct(Library.key("FormatSpec"), Nil)

    TStructNew(s, List(TIntLit(width, s.fields.head._2), TIntLit(prec, s.fields(1)._2), TBoolLit(left)))
  }

  /** `va_start(ap)` readies a variadic body's tail. C also names the last fixed parameter here;
   * sysl does not, because the function already knows which parameter that is and repeating it is a
   * chance to get it wrong.
   */
  protected def vaStart(args: List[Expr]): TExpr = {
    if !variadicFn then err("'va_start' is only allowed in a function declared with '...' — there is no tail here")
    TVaStart(vaList("va_start", args))
  }

  /** `va_end(ap)` finishes with one. */
  protected def vaEnd(args: List[Expr]): TExpr = TVaEnd(vaList("va_end", args))

  /** `va_copy(dst, src)` starts `dst` where `src` has reached, so the tail can be walked twice.
   *
   * It is what makes lending a walk usable: the callee advances the list it was handed, so a body
   * that wants to go on reading its own hands over a copy. C's rule that the destination must not
   * already be in use, and must be ended in its turn, is C's here too — nothing in a `va_list` says
   * which state it is in.
   */
  protected def vaCopy(args: List[Expr]): TExpr = {
    if args.length != 2 then err("'va_copy' takes two arguments, the va_list to start and the one to copy")
    TVaCopy(vaListArg("va_copy", args.head), vaListArg("va_copy", args(1)))
  }

  /** `va_arg(ap)` takes the next argument and advances.
   *
   * C writes the type as a second argument, which is not something a sysl expression can hold, so
   * it comes from the context the value is read into — the same place `None` and `Ok(5)` get
   * theirs. Nothing in the tail can confirm it, which is the unsafety C has here too.
   *
   * There is no `va_arg[T](ap)`: square brackets in an expression are indexing
   * (`reference/generics.md § [] means type application in a type, indexing in an expression`), and
   * call-site type arguments are refused language-wide (`reference/generics.md § Writing the type
   * arguments`). Somebody reaching for that spelling is told what to write instead rather than that
   * the callee is not a name.
   */
  protected def vaArg(args: List[Expr], expected: Option[Type]): TExpr = {
    val ap = vaList("va_arg", args)
    val ty = expected.getOrElse(
      err("'va_arg' reads the next argument as some type, and nothing here says which — " +
        "annotate the variable it is read into"),
    )

    TVaArg(ap, vaArgType(ty))
  }

  private def vaList(what: String, args: List[Expr]): TExpr = {
    if args.length != 1 then err(s"'$what' takes exactly one argument, the va_list it walks")
    vaListArg(what, args.head)
  }

  /** The `va_list` a form works on, as its address. Each works on the list itself rather than on a
   * copy — `va_arg` advances it — so what is handed over is where it lives.
   *
   * Two spellings reach that, and they are the same thing seen from the two sides of a call. A
   * `va_list` is a body's own walk, and its address is taken here exactly as `&ap` would take it. A
   * **`*va_list`** is a walk some other function lent (`reference/ffi.md § Variadic functions`,
   * *Handing a walk on*): it is already the address, so it is passed along as it stands, and it
   * need not be a place — a pointer arriving as a parameter, out of a struct, or straight from a
   * call all say the same thing.
   */
  private def vaListArg(what: String, arg: Expr): TExpr = {
    val t = analyzeExpr(arg)

    t.ty match
      case Type.Ptr(Type.VaList) => t
      case Type.VaList => TAddrOf(requirePlace(t, arg, s"'$what'", writes = false), Type.Ptr(Type.VaList))
      case other                 => err(s"'$what' needs a va_list, not ${show(other)}")
  }

  /** Checks that a type is one a variadic tail can be *read* as.
   *
   * Every argument was widened on the way in (`reference/ffi.md § The calling side`), so asking for a narrower type than the
   * promotion produced would read the wrong bytes — C's most common varargs mistake, and one worth
   * a diagnostic rather than a wrong answer. Nothing is lost: read it at the promoted width and
   * convert, which is what C's own callee has to do anyway.
   *
   * **This list and `CallCore.variadicArg`'s are one rule read in two directions**, so a type a
   * caller may write into a tail is a type the callee may name — including a `Type.CFn`, which is
   * how a C library's callback setter takes a function and is a pointer-width value like any other
   * once it is in the tail.
   */
  private def vaArgType(ty: Type): Type = ty match
    case i: Type.Integer if i.bits >= 32          => ty
    case f: Type.Floating if f.bits == 64         => ty
    case Type.Char | _: Type.Ptr | _: Type.CFn    => ty
    case i: Type.Integer =>
      err(s"a variadic argument is promoted to at least 32 bits, so it cannot be read as " +
        s"${show(i)} — read it as ${if i.signed then "'int'" else "'uint'"} and convert")
    case f: Type.Floating =>
      err(s"a variadic argument is promoted to double, so it cannot be read as ${show(f)} — " +
        "read it as 'real' and convert")
    case other =>
      err(s"a variadic argument cannot be read as ${show(other)} — it may be an integer, a real, " +
        "a char, a raw pointer, or the address of a function")
}
