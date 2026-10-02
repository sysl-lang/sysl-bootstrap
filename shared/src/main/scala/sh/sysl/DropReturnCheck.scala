package sh.sysl

/** A destructor that can never run, told at the declaration that makes it so (card `0372`).
 *
 * **`impl Drop for T` is dead code unless something hands back a `&T`.** A destructor fires when a
 * *box's* strong count reaches zero (`reference/memory.md § A destructor`), so a constructor
 * declared `handle() -> Result[Handle, Error]` leaks the resource on every call — and nothing about
 * the program looks wrong while it does. The compression, the parsing and the queries are all
 * correct; only the memory is not.
 *
 * **It was found leaking three shipped packages at once.** Measured over 50,000 iterations:
 * `sysl-lang/brotli` v0.1.0 leaked a `BrotliEncoderState` per `compress` — **26 GB**, against 4.8 MB
 * once fixed — `hiredis` a `redisReader` per reader, and `libpq` a `PGconn` per failed connect. All
 * three suites were green throughout, and **no test could have caught any of them**.
 *
 * Three things make it invisible and they compound:
 *
 *   1. a `Drop` block that is never reached is not an error, so the declaration reads as the
 *      resource being handled;
 *   2. a type annotation at the call site looks like a fix and is not — `val e: &Encoder =
 *      encoder()?` compiles whichever way the constructor is declared, because reading a value into
 *      a `&T` binding boxes a *copy*;
 *   3. the error paths need more than a boxed return, since nothing owns the handle when the
 *      `Result` is an `Err`.
 *
 * Only the first is a compiler's to answer, and this is that answer.
 *
 * **It is a warning rather than an error, and the reason is not timidity.** Returning a `Drop` type
 * by value is legal and stays legal: a caller may take the value apart, or hand it straight into
 * something that boxes it, and neither is a mistake. What the compiler can see is that the *usual*
 * reading of the declaration is wrong, which is exactly what a warning is for.
 */
trait DropReturnCheck extends TypeResolution {

  /** Every declaration whose return type carries a `Drop` type by value, warned about once.
   *
   * **It runs after the `impl` blocks are hoisted**, because `dropsDeclared` is filled as they are —
   * asked any earlier it would answer about whichever half of the program had been read.
   *
   * The check is over declared functions rather than over instantiated types: a generic constructor
   * nobody has called yet is exactly the one worth telling, and an instantiation-driven check would
   * say nothing about a package whose tests happen not to reach it.
   */
  protected def checkDropReturns(): Unit =
    for (key, decl) <- funcDecls.toList do
      // The `drop` a `Drop` block declares takes `self` by value and answers `unit`, so it never
      // trips this — but a *member* of the dropping type may legitimately hand one back, and the
      // block itself is not the place to complain about that either way.
      if !isDropMember(key) then
        for
          ret  <- decl.retType
          // Under the declaration's own `@needs(...)`: resolving the type records the module it is
          // in, and a signature is charged as the body is, to whoever calls the declaration.
          ty   <- inSignature(decl.needs)(resolvedQuietly(key, ret))
          held <- dropByValue(ty)
        do
          warn(
            s"'${qn(key)}' hands back '${qn(held)}' by value, and '${qn(held)}' has a 'Drop' — a " +
              s"destructor runs when a box's count reaches zero, so nothing here will ever call it. " +
              s"Return '&${Modules.split(held)._2}' instead, or take the resource apart before " +
              s"returning it",
            decl.pos,
          )

  /** This declaration's return type, resolved **in the terms it was written in** and saying nothing
   * whatever it finds.
   *
   * Two things are load bearing and each cost a run of the library to discover.
   *
   * **`inScope(declScope(key))`, because a short name means what the file that wrote it imported.**
   * This check runs from a whole-program pass with no scope of its own, so resolving
   * `long_option(…) -> Opt` against whatever the walk was last reading answered *"unknown type
   * 'Opt'"* — about a declaration one line below the `Opt` it names.
   *
   * **The complaints are taken back, not merely un-thrown.** `probe` catches what is *raised*, and
   * `recorded`, `reported` and `boundErr` put a complaint straight into the set and carry on with a
   * fallback — so a resolution that fails quietly leaves the reader told about a reading nobody
   * kept. The first run of this check reported **228 errors** in a library that compiles.
   */
  private def resolvedQuietly(key: String, ret: TypeRef): Option[Type] = {
    val saved  = complaints
    val answer = declScope.get(key).fold(probe(resolveType(ret, Map.empty)))(
      inScope(_)(probe(resolveType(ret, Map.empty))))

    restoreComplaints(saved)
    answer
  }

  /** Whether this key is the `drop` of a `Drop` block, which is the one member the rule is not
   * about.
   */
  private def isDropMember(key: String): Boolean =
    Modules.split(key)._2 == "drop" || key.endsWith(".drop")

  /** The first type carried **by value** in this one that declares a destructor, if any.
   *
   * **`&T` and `*T` both stop the walk, for opposite reasons.** A `&T` is the boxed form, which is
   * the whole answer the warning asks for; a raw pointer owns nothing and never did, so a binding
   * handing one back is making no claim about who frees it.
   *
   * A slice and an array stop it too. Their elements are not destructed either, which is a real
   * question and a different one — this rule is about a *constructor's* answer, and widening it to
   * containers would fire on every buffer of handles a program legitimately manages itself.
   */
  private def dropByValue(t: Type): Option[String] = t match
    case _: Type.Ref | _: Type.Ptr | _: Type.Weak => None
    case _: Type.Slice | _: Type.Array            => None
    case s: Type.Struct =>
      Option.when(dropsDeclared(s.base))(s.base).orElse(s.targs.flatMap(dropByValue).headOption)
    case e: Type.Enum =>
      Option.when(dropsDeclared(e.base))(e.base).orElse(e.targs.flatMap(dropByValue).headOption)
    case Type.Volatile(inner) => dropByValue(inner)
    case _                    => None
}
