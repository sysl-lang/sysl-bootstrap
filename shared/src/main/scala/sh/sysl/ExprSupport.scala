package sh.sysl

/** The few things the expression traits ask of each other.
 *
 * `ExprAnalysis` holds the dispatch and the three traits beside it hold groups of forms, so anything
 * more than one of them needs has to sit *above* all of them — which is what this is. It is
 * deliberately small: a helper belongs with the form that uses it, and only a helper with two
 * callers in different groups ends up here.
 *
 * `analyzeValueAt` is declared and not defined, for the reason `AnalyzerBase` declares `analyzeExpr`
 * that way: a form that rewrites itself into another form — a qualified name folded into the one name
 * its module keys it under — has to re-enter the dispatch, and the dispatch is below this. The same
 * declaration is what lets `ExprCoercion` sit *above* the dispatch and still reach it.
 */
trait ExprSupport extends SpecialForms with PatternAnalysis with StmtAnalysis {

  /** Analyzes an expression that has already been rewritten into another form, at the position the
   * form it came from had. `analyzeExpr` would set the rewritten node's own position, which a node
   * the parser never saw does not have.
   */
  protected def analyzeValueAt(expr: Expr, expected: Option[Type], discarded: Boolean = false): TExpr

  /** The same, at the expression's own position — the ordinary way in. `analyzeExpr` differs only in
   * that it consults the expected type first, which is what `ExprCoercion` is for.
   */
  protected def analyzeValue(expr: Expr, expected: Option[Type], discarded: Boolean = false): TExpr =
    at(expr.pos)(analyzeValueAt(expr, expected, discarded)).setPos(expr.pos)

  /** What an array form's elements should be analyzed as, given what the form itself is expected to
   * produce. A `string` is not on the list: its elements are bytes, but writing one is a validity
   * question rather than an arrangement of elements.
   */
  protected def elementWanted(want: Type): Option[Type] = want match
    case Type.Array(_, e)  => Some(e)
    case Type.Vector(_, e) => Some(e)
    case Type.Slice(e, _)  => Some(e)
    case _                 => None

  /** A reference written as a chain of plain names: `std.fs.read` is `["std", "fs", "read"]`.
   * `None` for anything else, since a chain interrupted by a call or a subscript is a value being
   * read from rather than a path being named.
   */
  protected def chain(e: Expr): Option[List[String]] = e match
    case Ident(n)    => Some(List(n))
    case Field(r, f) => chain(r).map(_ :+ f)
    case _           => None

  /** A reference reaching into a module, rewritten with the module folded into the name it
   * qualifies — `std.fs.read` becomes the one name `std.fs`'s `read` is keyed under — or `None`
   * where the chain names no module.
   *
   * That rewrite is the whole of what qualified access needs: what is left is `read(…)`,
   * `Point(…)`, `Shape.Circle(…)` — the ordinary forms, resolved by the cases that already handle
   * them, against tables that were keyed this way to begin with.
   *
   * Two rules decide it, and both are `reference/modules.md § Imports`'s. **A local binding shadows
   * a module name**, so a chain whose head is bound to a value is a field read and nothing else —
   * which is why this cannot be a pre-pass over the tree and has to be asked where the scopes are.
   * And the **longest** module prefix wins, so a module `a.b` is reached as one rather than as
   * `a`'s `b`.
   *
   * A head bound by an import is read as that import, which is what makes the `fs` of `import
   * std.fs` a prefix everywhere a written path is. Everything after that is `inPackage`'s to decide
   * — the package the file belongs to, the path as written, and the packages its manifest named, in
   * that order — which is what makes `sqlite.open` reach a dependency's module without an import
   * and without the coordinate it is really named under (`reference/packages.md § What a
   * dependency's modules are called`).
   *
   * **The package layer is asked with the whole chain and the import layer with its head**, and the
   * asymmetry is the two layers' own. An import binds one name, so only a head can answer to it. A
   * package binds a module *path* — `sh.sysl.table` for one namespaced by reverse DNS, since a
   * directory holding no source is no module (`reference/modules.md`) — so a head is not enough to
   * find it, and offering only the head is what made `sh.sysl.table.of(…)` read as a field of an
   * undefined `sh` while `import sh.sysl.table` beside it resolved.
   *
   * This used to hold a **second** copy of `inPackage`'s ordering, deciding on the head alone before
   * the package layer was reached at all. That is the shape the ordering fixed, and having it written
   * twice is why fixing it in one place left the other half of the same defect standing.
   */
  protected def throughModule(e: Expr): Option[Expr] =
    for
      written <- chain(e) if written.length > 1 && lookupOpt(written.head).isEmpty
      whole = written.mkString(".")
      path = importedModule(written.head).map(_.split('.').toList ::: written.tail)
               .getOrElse(inPackage(whole).split('.').toList)
      k <- (path.length - 1).to(1, -1).find(n => moduleNames(path.take(n).mkString(".")))
    yield
      val module = path.take(k).mkString(".")
      val rest   = path.drop(k)

      // The key this builds is spelled the way the compiler spells its own references, so resolving
      // it says nothing about which module wrote it — but *this* is a path a file wrote, in the
      // terms of the body being read, so the dependency it makes is recorded here
      // (`reference/modules.md § The module graph is acyclic`).
      dependsOn(module)
      rest.tail.foldLeft[Expr](Ident(Modules.qualify(module, rest.head)))((acc, n) => Field(acc, n))
        .setPos(e.pos)

  /** A **module-qualified function name**, flattened back into the spelling it was written as and
   * paired with the key it resolves to — `("c.less", "c$less")` from `Field(Ident("c"), "less")`.
   *
   * A function is not a place, so its address is taken from the name rather than by the walk that
   * looks for storage (`reference/ffi.md § A function's address`) — and that walk is the only thing
   * `throughModule` sits in front of. A qualified name therefore has to be recognised here instead,
   * which is the whole of why the unqualified spelling reached an address and the qualified one did
   * not.
   *
   * **The written spelling is kept beside the key because a diagnostic has to quote it.** A key
   * carries the module separator, which nothing in source may contain, so a message built from one
   * tells the reader to type something that is not sysl.
   *
   * `funcKey` is asked the whole dotted path, which it has always accepted: `reference/modules.md §
   * Imports`'s last step is a qualified one, and this is the same resolution an unqualified name
   * takes. **A local binding shadows a module name**, so the head is tested exactly as
   * `throughModule` tests it.
   */
  protected def qualifiedFunc(e: Expr): Option[(String, String)] =
    for
      segs <- chain(e) if segs.length > 1 && lookupOpt(segs.head).isEmpty
      written = segs.mkString(".")
      key <- funcKey(written)
    yield (written, key)

  /** Whether a place bottoms out in something bound by a `val` — either a module-level one or a
   * local. Reaching *into* one keeps the property: an element of a read-only array is read-only,
   * and so is a field of a read-only struct.
   */
  protected def readOnly(t: TExpr): Boolean = t match
    // An `extern` variable is the one global that is not one: the storage belongs to whoever laid it
    // down, and reaching it is the foreign seam rather than a promise this program made (`reference/ffi.md § extern — a declaration with no body`).
    case g: TGlobal         => !g.writable
    // A `ref` is read-only exactly when the storage it found is (`reference/memory.md § ref — a
    // name for a place`), which is what lets the property survive being given a shorter name:
    // reaching into a `val` keeps it, and a ref is a way of reaching in.
    case TLoad(name, _)     => readOnlyLocals(name) || refPlaces.get(name).exists(readOnly)
    case TField(recv, _, _) => readOnly(recv)
    // Only where the elements are the receiver's own storage. A slice's are somebody else's, and
    // whose they are is exactly what a slice does not record.
    case TIndex(recv, _, _) =>
      Type.unnamed(recv.ty) match
        case _: Type.View => false
        // A `val *T` fixes the address, not what is at it — exactly as `*p = v` through one is
        // already allowed. C's `T *const p` reads the same way.
        case _: Type.Ptr  => false
        case _            => readOnly(recv)
    case _ => false
}
