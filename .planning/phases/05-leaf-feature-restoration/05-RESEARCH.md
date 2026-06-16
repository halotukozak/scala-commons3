# Phase 5: Leaf feature restoration - Research

**Researched:** 2026-06-02
**Domain:** Scala 3 `inline` + `scala.quoted` macros for typeclass materialization (internal port from fork master)
**Confidence:** HIGH

## Summary

This is an internal macro-porting phase. There is **no third-party library question** — every API is owned by the project. The work is a 1:1 file-level translation from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<file>` into our single-source tree (`core/src/main/scala/com/avsystem/commons/misc/<file>`), replacing Phase-1 `???` stubs.

Two infrastructural surprises that the planner must internalise:

1. **The fork ports leaf macros via a centralised `MiscMacros` bundle**, not file-local impl objects. Every leaf in `misc/` mixes in a `*Macros` trait (e.g. `AnnotationOf extends AnnotationOfMacros`); the trait body is one inline given that splices into `MiscMacros.materializeX`. The whole macro implementation surface for Phase 5 lives in **one new file**: `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` — ported verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`. This is the central piece downstream consumers (planner) often miss.

2. **Phase 4's `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` are `'{ ??? }` staging stubs by design** (fork status — not "broken yet"). Leaves that DON'T touch them (TypeString, JavaClassName, AnnotationOf family, ApplierUnapplier, SealedUtils, ValueEnum) port to **fully real runtime behaviour**. Leaves that DO touch them (Delegation, and one Bidirectional verdict) inherit a **compile-passes-but-throws-NotImplementedError-at-runtime** state — fork itself ships in this exact state with the matching test marked `ignore`d. This is acceptable per the migration's "stage-and-iterate" cadence.

**Primary recommendation:** Port `MiscMacros.scala` as **slice 5.0 (foundation)** — a single 400-LOC file that owns every Phase-5 macro implementation — then port each leaf as a thin "mix-in `XMacros` + replace `???` impl" delta. Treat as **stacked**, not parallel, despite the CONTEXT decision. Rationale below.

## User Constraints (from CONTEXT.md)

### Locked Decisions

- **Slice strategy:** 7 parallel-safe PRs (one per leaf), each off `upstream/scala-3` tip stacked on `04-05-meta-annotations @ f04cec6f` until Phase 4 merges. Document base in PR body.
- **Method:** Crib verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<file>` per `[[feedback_crib_from_master]]`. Reconcile divergence to our single-source layout.
- **Bidirectional:** deprecate over restore. Port the `@deprecated` stub object from fork (uses `scala.compiletime.error`). No real macro impl.
- **Phase 4 `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` are `'{ ??? }` stubs** — leaves that call them inherit the runtime `???` failure mode. Matches fork's own staging. Document per slice.
- **Test un-wrapping policy:** un-wrap matching `<Feature>Test` per slice; runtime-dependent cases stay `ignore`d (or wrapped if fork keeps them wrapped).
- **Commit cadence per slice (NO squash):** `feat(scala-3,core): port <Feature>` + `test(scala-3,core): un-wrap <Feature>Test` + `docs(migration): record <Feature> port`.
- **PR conventions:** title `[Scala 3] port <Feature>`; `--draft` on open; milestone 1; body metadata block with Slice / Parallel-independent / Depends-on / Base-branch.

### Claude's Discretion

- Exact slice ordering (CONTEXT proposes LOC-ascending: 5.1 Bidirectional → 5.7 ValueEnum — refine per execution).
- Whether to merge slices into fewer PRs if fork-shape allows.
- Test pending vs wrapped strategy per case.
- `MetaMacros` stub-dependency disclosure verbiage in PR body.
- **Newly-surfaced discretion (this research):** how to handle the shared `MiscMacros.scala` file — see Architecture section. Recommendation: pull `MiscMacros.scala` into a **slice 5.0 foundation** that the other 6 leaves depend on, OR include the relevant fragment of `MiscMacros` inline with each leaf. Strongly recommend the former (single foundation slice).

### Deferred Ideas (OUT OF SCOPE)

- Real macro bodies for `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` — Phase 6.
- `GenCodec.materialize` — Phase 6.
- `MongoEntityCompanion` macros — Phase 9.
- RPC framework — Phase 7.
- Phase 4 PR merge (leaves stack on `04-05-meta-annotations` tip until then).
- `Sam.apply` (deleted in slice 2.5).
- `analyzer` module re-enable.

## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| TYPESTRING-01 | Restore `TypeString.materialize` (or `derived`/`given`) producing `new TypeString[T](TypeRepr.of[T].dealias.show)` | Fork file complete (40 LOC `materializeImpl`); template in §Code Examples |
| JAVACLASSNAME-01 | Restore `JavaClassName.materialize` (now `derived`) producing JVM FQN of T's class symbol | Fork file complete (`derivedImpl` with module-flag suffix handling); template in §Code Examples |
| ANNOTOF-01 | Restore `AnnotationOf.materialize` + 6 siblings (`OptAnnotationOf`, `AnnotationsOf`, `HasAnnotation`, `SelfAnnotation`, `SelfOptAnnotation`, `SelfAnnotations`) reading annotations from `TypeRepr.of[T].typeSymbol` with `AnnotationAggregate` expansion | Fork `MiscMacros.materializeAnnotationOf` + `annotsOfT` + `annotsOfSym` + `expandAggregates` (220 LOC) |
| APPLIERUNAPPLIER-01 | Restore `Applier`, `Unapplier`, `ApplierUnapplier` via `Mirror.ProductOf` typeclass derivation (not macro at all in fork) | Fork uses `given derived[T <: Product: Mirror.ProductOf]` — no quoted impl needed |
| DELEGATION-01 | Restore `Delegation.materializeDelegation` + `CurriedDelegation.apply` | Fork has `???` stub in `DelegationMacros` — port AS STUB (matches fork) |
| SEALEDUTILS-01 | Restore `SealedUtils.{instancesFor, caseObjects, caseObjectsFor}` + `SealedEnumCompanion.evidence` | Fork uses `compiletime.summonAll` + `compiletime.erasedValue` + `compiletime.summonFrom` + `scala.ValueOf` — **no quoted impl**, pure inline metaprogramming |
| VALUEENUM-01 | Restore `ValueEnumCompanion.valName` + `Ctx` machinery | Fork has full impl: `valNameImpl` uses `Symbol.spliceOwner.owner` to recover the enclosing `val` symbol |
| BIDIRECTIONAL-01 | Port as `@deprecated` stub with `scala.compiletime.error` body | Fork verbatim — 17 LOC, no macro |

## Standard Stack

### Core (internal APIs — no third-party)

| Module | API | Purpose | Why Standard |
|--------|-----|---------|--------------|
| `scala.quoted` (stdlib) | `Expr[T]`, `Type[T]`, `Quotes`, `quotes.reflect.*` | Quote/unquote, type-level reflection | The ONE Scala 3 macro API; replaces `c.universe` / `c.Expr` 1:1 |
| `scala.compiletime` (stdlib) | `summonInline`, `summonAll`, `summonFrom`, `erasedValue`, `error` | Compile-time tuple recursion + implicit summon | Scala 3 idiom for `Mirror`-based derivation (SealedUtils, ApplierUnapplier) |
| `scala.deriving.Mirror` (stdlib) | `Mirror.ProductOf[T]`, `Mirror.SumOf[T]`, `.MirroredElemTypes` | Structural inspection of products/sums | Replaces all "is this a case class / sealed trait" macros |
| `com.avsystem.commons.meta.MetaMacros` (Phase 4) | `valueImpl`, `lazyMetadataImpl`, `dummy` | Splice plumbing for materialize traits | Phase 4 staging stubs (`'{ ??? }`) — leaves don't call these directly except via inherited trait |
| `com.avsystem.commons.meta.MacroInstances` (Phase 4) | `materialize` inline given | Aggregates typeclass instances into a NamedTuple | Not used by Phase-5 leaves directly; here for completeness |
| `com.avsystem.commons.misc.MiscMacros` (**new in Phase 5**) | `materializeAnnotationOf`, `materializeSelfAnnotation`, etc. + helpers (`annotsOfT`, `expandAggregates`) | Centralised impl bundle for all Phase-5 quoted macros | Fork pattern — keep one file owning impls, leaf files declare only the `inline given` shells |

### Supporting

| API | Purpose | When to Use |
|-----|---------|-------------|
| `Symbol.spliceOwner` + `.owner` walk | Find enclosing class/val symbol | `Self*Annotation` (walk to enclosing class), `ValueEnum.valNameImpl` (walk to enclosing val def) |
| `TypeRepr.of[T].dealias.typeSymbol` | Resolve T to concrete class symbol | Every leaf — first line of every impl |
| `Type.show[T]` + `Printer.TypeReprShortCode` | Pretty-print types into source-form strings | `TypeString.materializeImpl` |
| `Flags.Module`, `Flags.Package` | Distinguish companion objects from classes | `JavaClassName.derivedImpl` (`$` suffix logic) |
| `report.errorAndAbort(msg, pos?)` | Compile-time error from a macro | Every error path; replaces Scala 2 `c.abort` |
| `Expr.ofList`, `Expr(value)` (lift) | Lift Scala values into `Expr` | List-of-annotations construction |
| `term.asExprOf[A]` | Cast a reflected `Term` to typed `Expr[A]` | Annotation reification |
| `RefiningAnnotation` (scala.annotation) | Marker for type-refining annotations | `HasAnnotation[A <: RefiningAnnotation, T]` bound |

### Alternatives Considered (rejected)

| Instead of | Could Use | Why rejected |
|------------|-----------|--------------|
| Internal `MiscMacros` bundle | Per-file impl objects (e.g. `TypeStringMacros` in `TypeString.scala`) | Diverges from fork shape — `[[feedback_crib_from_master]]` wins. Also: shared helpers (`annotsOfT`, `expandAggregates`) need to be shared across 6 leaves; bundling avoids duplication |
| Quoted impl for `ApplierUnapplier` / `SealedUtils` | TypeClass derivation via `Mirror` | Mirror IS what fork uses — better idiomatic Scala 3, simpler code, no Quotes needed at all for these two |
| Real `Delegation.materialize` quoted impl | Reuse fork pattern of `???` stub | Fork itself ships `???` for `Delegation` — porting the real impl is Phase 6+ scope. `[[feedback_crib_from_master]]` rules: match fork state |
| `Bidirectional` real port | `@deprecated` stub | Locked decision; fork shape matches deprecate-over-restore memory |

**Installation:** None — all APIs are stdlib or in-project.

**Version verification:**
```bash
sbt 'show scalaVersion'
# Expected: 3.8.2 (Phase 1 baseline, verified in project/Commons.scala:29 — `val scala3Version = "3.8.2"`)
```
Scala 3.8.2 (released October 2025 — within knowledge horizon) supports every required API: `inline given`, `transparent inline`, `Mirror`, `compiletime.{summonAll, summonFrom, summonInline, erasedValue, error}`, `Symbol.spliceOwner`, `RefiningAnnotation`, `opaque type`.

## Architecture Patterns

### Recommended Project Structure (post-Phase-5)

```
core/src/main/scala/com/avsystem/commons/misc/
├── MiscMacros.scala              # NEW — centralised impl bundle (cribbed verbatim from fork)
│                                 #     Contains: AnnotationOfMacros, OptAnnotationOfMacros, AnnotationsOfMacros,
│                                 #               SelfAnnotationMacros, SelfOptAnnotationMacros, SelfAnnotationsMacros,
│                                 #               DelegationMacros, DelegationApplyMacros (stubs)
│                                 #     + object MiscMacros containing materializeX impls + annotsOfT/expandAggregates
├── TypeString.scala              # Replace stub with `inline given [T] => TypeString[T] = ${ materializeImpl[T] }`
│                                 #   + JavaClassName (coupled) with `derivedImpl`
├── AnnotationOf.scala            # Replace 7 stubs with `object X extends XMacros {}` lines + opaque type HasAnnotation
├── ApplierUnapplier.scala        # Replace 3 stubs with `given derived` Mirror-based derivations
├── Bidirectional.scala           # Replace stub with @deprecated object + compiletime.error body
├── Delegation.scala              # Replace 2 stubs with `object Delegation extends DelegationMacros {}` (impl = ???)
├── SealedUtils.scala             # Replace 3 stubs with inline `compiletime.summonAll` / `erasedValue` recursion
└── ValueEnum.scala               # Replace `valName` stub with `${ MiscMacros.valNameImpl... }`
                                  #   (NOTE: fork puts `valNameImpl` at TOP LEVEL of ValueEnum.scala, not in MiscMacros)
```

### Pattern 1: Inline Given + Splice (canonical Phase-5 shape)

**What:** Companion object extends a `XMacros` trait; trait has `inline given [T] => X[T] = ${ MiscMacros.materializeX[T] }`; `MiscMacros.materializeX` is a `def materializeX[T: Type](using Quotes): Expr[X[T]]`.

**When to use:** Every typeclass that needs reflection (TypeString, JavaClassName, AnnotationOf family).

**Example (cribbed from fork `TypeString.scala`):**
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala
class TypeString[T](val value: String) extends AnyVal
object TypeString extends TypeStringCompat {
  inline given [T] => TypeString[T] = ${ materializeImpl[T] }     // <-- the leaf shell
  def of[T: TypeString]: String = TypeString[T].value
  def apply[T](using ts: TypeString[T]): TypeString[T] = ts

  private def materializeImpl[T: Type](using quotes: Quotes) = {  // <-- the splice impl
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val typeString = Expr(tpe.show(using Printer.TypeReprShortCode))
    '{ new TypeString[T]($typeString) }
  }
}
```

### Pattern 2: Trait-Based Bundle (used for AnnotationOf family)

**What:** A `XMacros` trait in `MiscMacros.scala` carries the `inline given`; the leaf file's companion `extends XMacros`.

**Why:** Allows sharing helpers (`annotsOfT`, `expandAggregates`, `enclosingClass`) across 6 macro implementations without code duplication.

**Example (cribbed from fork):**
```scala
// In MiscMacros.scala:
trait AnnotationOfMacros {
  inline given [A, T] => AnnotationOf[A, T] = ${ MiscMacros.materializeAnnotationOf[A, T] }
}
object MiscMacros {
  def materializeAnnotationOf[A: Type, T: Type](using quotes: Quotes): Expr[AnnotationOf[A, T]] = {
    import quotes.reflect.*
    annotsOfT[A, T].headOption match {
      case Some(annot) => '{ AnnotationOf[A, T](${ annot.asExprOf[A] }) }
      case None => report.errorAndAbort(s"${Type.show[T]} is not annotated with ${Type.show[A]}")
    }
  }
  private def annotsOfT[A: Type, T: Type](using quotes: Quotes): List[quotes.reflect.Term] = { ... }
}

// In AnnotationOf.scala:
object AnnotationOf extends AnnotationOfMacros {}
```

### Pattern 3: Mirror-Based Derivation (NO macro at all)

**What:** Use `Mirror.ProductOf` / `Mirror.SumOf` + `compiletime.summonAll` instead of writing a macro.

**When to use:** Anywhere the macro is just iterating tuple types or summoning per-element instances.

**Example (cribbed from fork `ApplierUnapplier.scala`):**
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala
object Applier {
  given derived[T <: Product: Mirror.ProductOf as m]: Applier[T] = rawValues =>
    m.fromTuple(Tuple.fromArray(rawValues.toArray).asInstanceOf[m.MirroredElemTypes])
}
object Unapplier {
  given derived[T <: Product]: Unapplier[T] = value => IArraySeq.unsafeWrapArray(value.productIterator.toArray)
}
object ApplierUnapplier {
  given derived[T: {Applier as applier, Unapplier as unapplier}]: ApplierUnapplier[T] = new ApplierUnapplier[T] {
    override def apply(rawValues: Seq[Any]): T = applier.apply(rawValues)
    override def unapply(value: T): Seq[Any] = unapplier.unapply(value)
  }
}
```

**Note Scala 3.8 syntax:** `[T: Mirror.ProductOf as m]` is the new "summon-into-alias" sugar (≡ `(using m: Mirror.ProductOf[T])`). `[T: {Applier as applier, Unapplier as unapplier}]` is the same for multi-typeclass context bounds. Verified supported in 3.8.2.

### Pattern 4: Pure Inline (no `${...}` at all)

**What:** Use `inline def` + `compiletime.{erasedValue, summonAll, summonFrom}` for tuple/sum iteration.

**When to use:** Sealed-trait enumeration helpers (SealedUtils.instancesFor / caseObjects).

**Example (cribbed from fork `SealedUtils.scala`):**
```scala
object SealedUtils {
  inline def instancesFor[TC[_], T: Mirror.SumOf as m]: List[TC[T]] =
    compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, TC]].toList.asInstanceOf[List[TC[T]]]

  inline def caseObjects[T: Mirror.SumOf as m]: List[T] =
    collectCaseObjects[T, m.MirroredElemTypes]

  inline private def collectCaseObjects[T, Tup <: Tuple]: List[T] = inline compiletime.erasedValue[Tup] match {
    case _: (h *: t) =>
      compiletime.summonFrom {
        case vo: scala.ValueOf[`h`] => vo.value.asInstanceOf[T] :: Nil
        case m: Mirror.SumOf[`h`]   => collectCaseObjects[T, m.MirroredElemTypes]
        case _ => Nil
      } ::: collectCaseObjects[T, t]
    case _: EmptyTuple => Nil
  }
}
```

### Pattern 5: Enclosing-Symbol Walk (ValueEnum, Self*Annotation)

**What:** Use `Symbol.spliceOwner` to walk up to the enclosing class or val def — recovers the "macro call site context" that Scala 2 macros got via `c.enclosingClass` / `c.internal.enclosingOwner`.

**Example (cribbed from fork `ValueEnum.scala`):**
```scala
def valNameImpl[T <: ValueEnum: Type, ValName: Type, Owner: Type](
  createValName: Expr[String => ValName],
)(using quotes: Quotes): Expr[ValName] = {
  import quotes.reflect.*

  def omitAnonClass(owner: Symbol): Symbol =
    if (owner.isDefDef && owner.name == "<init>" && owner.owner.name.contains("$anon"))
      owner.owner.owner
    else owner

  val owner = omitAnonClass(Symbol.spliceOwner.owner)
  val valid = owner.isTerm && owner.owner == TypeRepr.of[Owner].typeSymbol &&
    owner.isValDef && owner.flags.is(Flags.Final) && !owner.flags.is(Flags.Lazy) && /* ... */

  if (!valid) report.errorAndAbort("ValueEnum must be assigned to a public, final, non-lazy val ...")
  val name = Expr(owner.name)
  '{ $createValName.apply($name) }
}
```

### Anti-Patterns to Avoid

- **Hand-rolling annotation walking when fork has `expandAggregates`** — the fork's `MiscMacros.expandAggregates` already handles `AnnotationAggregate` substitution (constructor-parameter rewiring with `TreeMap.transformTerm`). Don't reimplement.
- **Using `transparent inline` for these leaves** — none of them returns a refined type. Plain `inline` is correct (matches fork). `transparent inline` is only used in fork for `HasAnnotation.check` / `HasAnnotation.get` which return `Option[A]` shapes.
- **Splitting per-leaf impl objects** — fork's pattern is one centralised `MiscMacros`. Diverging means double-maintenance of shared helpers.
- **Porting `Delegation` with a real quoted impl** — fork doesn't, and the test is `ignore`d. Match fork state.
- **Skipping `@publicInBinary` on private members touched from inline bodies** — Scala 3 requires it (Phase 4 precedent).

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Iterate sum-type children | Custom macro doing `TypeRepr.of[T].typeSymbol.children` | `Mirror.SumOf[T].MirroredElemTypes` + `compiletime.summonAll` / `erasedValue` recursion | Less reflection, faster compile, fork pattern |
| Iterate product-type fields | Custom macro doing `caseClassFieldSymbols` | `Mirror.ProductOf[T]` + `Tuple.fromArray` / `productIterator` | Same |
| Walk annotations including aggregates | New `expandAggregates` impl | Crib fork `MiscMacros.expandAggregates` verbatim (180 LOC, handles `AnnotationAggregate` constructor-param substitution via `TreeMap`) | Edge-case heavy; do not reinvent |
| Resolve enclosing val/class symbol | Manual tree walking | `Symbol.spliceOwner.owner` + flag inspection (`isValDef`, `Flags.Final`, `Flags.Lazy`) | One-liner |
| Reify a list of annotation terms | Manual `Expr.ofList(map(...))` | Same — that IS the idiom; just don't try to build `List` ASTs by hand |
| Pretty-print a type to source code | Custom typeRepr printer | `TypeRepr.of[T].show(using Printer.TypeReprShortCode)` | Stdlib; produces valid Scala source by spec |
| Implicit lookup with custom error message | Reinvent `@implicitNotFound` chain | `Implicits.search(TypeRepr.of[T])` + fork's `implicitNotFoundMessage[T]` lookup helper (in `MiscMacros`) | Already done in fork |

**Key insight:** All the heavy lifting (annotation aggregation, implicit-not-found extraction, enclosing-class walking) is already authored in `origin/master:.../misc/MiscMacros.scala`. Phase 5 is a port, not a redesign — the verb is "translate," never "build."

## Common Pitfalls

### Pitfall 1: Trying to access a leaf's macro impl from Phase-4 `MetaMacros`
**What goes wrong:** Confusing the centralised Phase-5 `MiscMacros` (misc package) with Phase-4 `MetaMacros` (meta package). They're different files.
**Why it happens:** Similar naming. Phase 4's `MetaMacros` owns `valueImpl`/`lazyMetadataImpl`/`dummy` (which stay `'{ ??? }` until Phase 6). Phase 5's `MiscMacros` owns the real leaf impls.
**How to avoid:** Two files, two packages: `com.avsystem.commons.meta.MetaMacros` (Phase 4, stubs) vs `com.avsystem.commons.misc.MiscMacros` (Phase 5, real impls).
**Warning signs:** Compiler reports `??? : Nothing` at a leaf call site instead of the real return type — you wired the leaf to the wrong `Macros` object.

### Pitfall 2: `inline given X` vs `inline def materialize`
**What goes wrong:** Fork uses `inline given [T] => TypeString[T] = ${ ... }` (the new "given with type-param" syntax). Old Scala 3 syntax was `inline given materialize[T]: TypeString[T] = ${...}`. Both work, but fork uses the former — match it.
**Why it happens:** Multiple valid Scala 3 syntaxes for the same idiom; mixing them blocks the per-file diff check.
**How to avoid:** When porting, copy the EXACT given declaration shape from `origin/master:<file>`. Don't "normalize" syntax.
**Warning signs:** `git diff origin/master:<file> <our-file>` shows differences in given-block form despite identical semantics.

### Pitfall 3: Forgetting that `Bidirectional` body is `compiletime.error`, not `???`
**What goes wrong:** Porting `Bidirectional.apply` with `???` keeps it compile-time-OK but runtime-failure. Fork uses `compiletime.error("...")` which fails AT COMPILE TIME — strictly better for downstream consumers.
**Why it happens:** Carrying over the Phase-1 `???` stub instead of cribbing fork's deprecated-stub shape.
**How to avoid:** Verbatim crib from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala`. It's 17 LOC.
**Warning signs:** Test sources reference `Bidirectional[...](...)` and compile — they shouldn't (fork drops the test entirely with `DROPPED:` comment).

### Pitfall 4: HasAnnotation's `opaque type` + `RefiningAnnotation` bound
**What goes wrong:** Fork redesigned `HasAnnotation` from a `final class HasAnnotation[A,T] private ()` (our current stub) to `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A` with `transparent inline def check` / `get` companion methods. Direct port changes the public API shape.
**Why it happens:** Our Phase-1 stub kept the Scala-2 class shape; fork moved to opaque type. The change is necessary — `transparent inline def get[A,T]: Option[A]` requires `A` typed through the opaque alias.
**How to avoid:** Document as source-compat break in MIGRATION.md §3. Verify no internal callers depend on `HasAnnotation.create[A,T]` factory (which the opaque type doesn't have).
**Warning signs:** Compile error `HasAnnotation.create not found` at any callsite — search before porting.

### Pitfall 5: `Symbol.spliceOwner` returns the splice's owner, not the macro caller's owner
**What goes wrong:** In `ValueEnum.valNameImpl`, fork does `Symbol.spliceOwner.owner` (note the `.owner`!), then walks past anon-class `<init>` wrappers. Missing the `.owner` puts you inside the splice's synthetic def, not the enclosing val.
**Why it happens:** Scala 3 docs (and intuition) name the API `spliceOwner` — easy to assume it returns the call-site context.
**How to avoid:** Crib fork's `omitAnonClass` + `Symbol.spliceOwner.owner` pattern verbatim.

### Pitfall 6: `materializeWith` annotation lookup blocked by Phase-4 stubs
**What goes wrong:** `MacroInstances` (Phase 4) uses `materializeWith(prefix, materializer)` annotation to override the materializer per-method. If a Phase-5 leaf is the prefix and Phase 4's `materialize` is still real-but-inert (it summons inline givens which are the leaf macros), the chain works only when both ends are ported.
**Why it happens:** Phase-4 / Phase-5 boundary is fuzzy: `MacroInstances.materialize` works via `summonInline` of each instance type — so each instance type's `inline given` must be the leaf's real macro.
**How to avoid:** Verify Phase 4's `MacroInstances` is the **inline given + summonInline** shape (it is — see fork `meta/MacroInstances.scala`), not a quoted impl that wants to invoke each leaf macro by name.
**Warning signs:** `compile passes; runtime: NotImplementedError at MacroInstances.apply` — means leaf inline given returned `???`. Solved by porting that leaf.

### Pitfall 7: GenCodec/GenKeyCodec given changes break downstream
**What goes wrong:** Fork's `TypeString` has `given GenKeyCodec[TypeString[T]]` per-type (parametric), whereas Phase-1 stubs have `implicit val keyCodec: GenKeyCodec[TypeString[_]]` (single instance for the existential). Different resolution semantics.
**Why it happens:** Slice 3.3 (`implicit→given`) already touched some of these — check current state before assuming.
**How to avoid:** Diff `origin/master:.../TypeString.scala` against our current `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` BEFORE porting. The TypeString slice may need an extra commit for the GenKeyCodec/GenCodec given reshape.
**Warning signs:** `summon[GenKeyCodec[TypeString[Foo]]]` fails to resolve at a Phase-6+ consumer.

### Pitfall 8: Underestimating ValueEnum SI-7046-style ordering trap
**What goes wrong:** `ValueEnumCompanion.values` collects via a builder mutated from `Ctx.register` during enum-value construction. Object init order under Scala 3's lazy-val semantics can differ from Scala 2.
**Why it happens:** The current Phase-1 stub already includes a comment about `lazy val` for `SealedEnumCompanion.values` (Scala 3 init-order fix). ValueEnum has a similar concern.
**How to avoid:** Match fork's exact field ordering and `synchronized` / `awaitingRegister` flag dance. Don't optimize.
**Warning signs:** `values.head.ordinal != 0` or `IllegalStateException("Cannot register ...")` at app startup.

## Code Examples

### Example 1: TypeString (the smallest reflection-using leaf)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala
class TypeString[T](val value: String) extends AnyVal { override def toString = value }
object TypeString extends TypeStringCompat {
  inline given [T] => TypeString[T] = ${ materializeImpl[T] }
  def of[T: TypeString]: String = TypeString[T].value
  def apply[T](using ts: TypeString[T]): TypeString[T] = ts

  given [T] => GenKeyCodec[TypeString[T]] =
    GenKeyCodec.create[TypeString[T]](new TypeString(_), _.value)
  given [T] => GenCodec[TypeString[T]] =
    GenCodec.createSimple[TypeString[T]](i => new TypeString(i.readString()), (o, ts) => o.writeString(ts.value))

  private def materializeImpl[T: Type](using quotes: Quotes) = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val typeString = Expr(tpe.show(using Printer.TypeReprShortCode))
    '{ new TypeString[T]($typeString) }
  }
}
```
**Notes:** Note the `Printer.TypeReprShortCode` — produces source-form strings (e.g. `"List[Int]"`). Note placement of `materializeImpl` inside the companion object — TypeString breaks the "all impls in MiscMacros" rule because its impl is trivial and one-shot. Phase 5 plan should keep this fork shape.

### Example 2: AnnotationOf via shared MiscMacros bundle
```scala
// In core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala (NEW):
trait AnnotationOfMacros {
  inline given [A, T] => AnnotationOf[A, T] = ${ MiscMacros.materializeAnnotationOf[A, T] }
}

object MiscMacros {
  def materializeAnnotationOf[A: Type, T: Type](using quotes: Quotes): Expr[AnnotationOf[A, T]] = {
    import quotes.reflect.*
    annotsOfT[A, T].headOption match {
      case Some(annot) => '{ AnnotationOf[A, T](${ annot.asExprOf[A] }) }
      case None => report.errorAndAbort(s"${Type.show[T]} is not annotated with ${Type.show[A]}")
    }
  }

  private def annotsOfT[A: Type, T: Type](using quotes: Quotes): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aSym = TypeRepr.of[A].typeSymbol
    expandAggregates(TypeRepr.of[T].dealias.typeSymbol.annotations).filter(_.tpe.typeSymbol == aSym)
  }
  // expandAggregates: 80 LOC, ports the Scala-2 AnnotationAggregate expansion via TreeMap.transformTerm
}

// In core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:
@implicitNotFound("${T} is not annotated with ${A}")
case class AnnotationOf[A, T](annot: A) extends AnyVal
object AnnotationOf extends AnnotationOfMacros {}
```

### Example 3: Pure inline (no quoted impl) — SealedUtils
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala
object SealedUtils {
  inline def instancesFor[TC[_], T: Mirror.SumOf as m]: List[TC[T]] =
    compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, TC]].toList.asInstanceOf[List[TC[T]]]

  inline def caseObjects[T: Mirror.SumOf as m]: List[T] =
    collectCaseObjects[T, m.MirroredElemTypes]

  inline private def collectCaseObjects[T, Tup <: Tuple]: List[T] =
    inline compiletime.erasedValue[Tup] match {
      case _: (h *: t) =>
        compiletime.summonFrom {
          case vo: scala.ValueOf[`h`]    => vo.value.asInstanceOf[T] :: Nil
          case m: Mirror.SumOf[`h`]      => collectCaseObjects[T, m.MirroredElemTypes]
          case _                         => Nil
        } ::: collectCaseObjects[T, t]
      case _: EmptyTuple => Nil
    }
}
```
**Notes:** No `${...}` splice. No `Quotes`. Just inline + `compiletime.{summonAll, summonFrom, erasedValue}`. This is the simplest macro-replacement idiom and works because Mirror does the reflection work. SealedUtils.caseObjectsFor (Phase-1 stub with `@explicitGenerics`) is REMOVED in fork — replaced by `caseObjects[T: Mirror.SumOf]`. Plan must verify no callers of `caseObjectsFor`.

### Example 4: Bidirectional deprecated stub (verbatim port)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala
@deprecated("Bidirectional macro not ported to Scala 3 — write the reversed PartialFunction manually.", since = "3.0.0")
object Bidirectional {
  inline def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) =
    scala.compiletime.error(
      "com.avsystem.commons.misc.Bidirectional has not been ported to Scala 3. Write the reversed PartialFunction manually.",
    )
}
```
**Notes:** `compiletime.error` fails at compile time at any call site — better than runtime `???`. The matching fork test file `BidirectionalTest.scala` is wrapped in a `/* @TodoScala3Migration DROPPED: ... */` block, NOT deleted.

### Example 5: ValueEnum's valNameImpl (top-level def style)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala (line ~155)
def valNameImpl[T <: ValueEnum: Type, ValName: Type, Owner: Type](
  createValName: Expr[String => ValName],
)(using quotes: Quotes): Expr[ValName] = {
  import quotes.reflect.*

  def omitAnonClass(owner: Symbol): Symbol =
    if (owner.isDefDef && owner.name == "<init>" && owner.owner.name.contains("$anon")) owner.owner.owner
    else owner

  extension (s: Symbol)
    def isPublic: Boolean =
      !s.flags.is(Flags.Protected) && !s.flags.is(Flags.Private) && !s.flags.is(Flags.PrivateLocal)

  val owner = omitAnonClass(Symbol.spliceOwner.owner)
  val valid = owner.isTerm && owner.owner == TypeRepr.of[Owner].typeSymbol && owner.isValDef &&
    owner.flags.is(Flags.Final) && !owner.flags.is(Flags.Lazy) && owner.isPublic &&
    owner.typeRef <:< TypeRepr.of[T]

  if (!valid) report.errorAndAbort(
    "ValueEnum must be assigned to a public, final, non-lazy val in its companion object " +
    "with explicit `Value` type annotation, e.g. `final val MyEnumValue: Value = new MyEnumClass",
  )
  val name = Expr(owner.name)
  '{ $createValName.apply($name) }
}
```
**Notes:** `valNameImpl` is a TOP-LEVEL `def` in the `ValueEnum.scala` file (sibling to `object ValueEnumCompanion`), NOT in `MiscMacros`. Match fork.

## State of the Art

| Old Approach (Scala 2 / pre-Phase-5 stub) | Current Approach (Scala 3 / Phase 5 port) | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `c.macro` / `c.Expr[T]` / `c.universe` | `inline def` / `${...}` splice / `Expr[T]` / `Type[T]` / `Quotes` / `quotes.reflect.*` | Scala 3.0+ | Total rewrite of impl bodies; signatures preserved |
| `c.weakTypeOf[T]` | `TypeRepr.of[T].dealias` | Scala 3.0+ | 1:1 mapping |
| Macro bundles via class `c: blackbox.Context` | Plain `def materializeX[T: Type](using Quotes): Expr[X[T]]` | Scala 3.0+ | Helpers become plain methods on `MiscMacros` |
| Whitebox macro (`scala.reflect.macros.whitebox.Context`) returning refined types | `transparent inline def` + quoted impl | Scala 3.0+ | Used by `HasAnnotation.check` / `get` |
| `implicit def materialize[T]: TC[T] = macro ...` | `inline given [T] => TC[T] = ${ materializeImpl[T] }` (NEW Scala 3.8 syntax) | Scala 3.7+ | Modern given syntax; replaces older `inline given materialize[T]: TC[T] = ${...}` |
| `c.abort(c.enclosingPosition, msg)` | `report.errorAndAbort(msg, pos?)` | Scala 3.0+ | 1:1 |
| `c.enclosingClass` / `c.internal.enclosingOwner` | `Symbol.spliceOwner.owner` + walk past anon `<init>` | Scala 3.0+ | More explicit; manual flag-based filtering required |
| Iterating annotations via `weakTypeOf[T].typeSymbol.annotations` | `TypeRepr.of[T].typeSymbol.annotations: List[Term]` | Scala 3.0+ | Returns reflected `Term` (annotation constructor application), not `Annotation` value |
| Reifying `Annotation` via `internal.gen.mkAnnotation` | `term.asExprOf[A]` on the annotation term itself | Scala 3.0+ | Simpler |
| `final class HasAnnotation[A,T] private ()` + factory `create[A,T]` | `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A` + companion `check`/`get` | Phase 5 (this phase) | **Public API reshape — document in MIGRATION.md §3** |

**Deprecated / outdated:**
- Custom `SealedEnumCompanion.evidence: this.type = this` line — fork has it commented out (kept as `//` in `SealedUtils.scala`); not needed because `summon[SealedEnumCompanion[T]]` via the companion's `apply` works directly.
- `caseObjectsFor[T]: List[T] = ???` (current Phase-1 stub) — fork removed it entirely; replaced by `caseObjects[T: Mirror.SumOf]`. Plan must check zero internal callers before deleting.
- `inferNonMacro` (Implicits family) — Scala 3 has no equivalent to Scala 2's "disable further macro expansion" knob; fork falls back to same path as `infer`. Out of Phase 5 scope (handled by Phase 2's `Implicits` slice — already done).

## Open Questions

1. **MiscMacros foundation as a separate slice (5.0) vs inlined into each leaf slice?**
   - What we know: fork keeps it as one `MiscMacros.scala` file (399 LOC). Six of seven leaves depend on it.
   - What's unclear: CONTEXT locked "7 parallel-safe PRs" — but every PR except Bidirectional would need to add identical fragments of `MiscMacros` if we don't extract a foundation slice.
   - Recommendation: Open `## User Constraints` allows Claude's discretion on "Whether to merge slices into fewer PRs if fork-shape allows." Strong recommendation: **add slice 5.0 = port `MiscMacros.scala` foundation** (≈400 LOC, depends only on Phase 4) as the first slice; remaining 6 slices stack on it (or branch off it). Reduces total surface area of touched code per leaf-PR; matches fork's single-source structure. Loses parallel-shippability for 6/7 PRs.
   - Alternative: per-leaf trait + per-leaf impl in same file (no shared `MiscMacros`). Diverges from fork shape. Reject.

2. **JavaClassName.derivedImpl: top-level vs inside companion?**
   - What we know: fork puts `derivedImpl` as a TOP-LEVEL `def` in `TypeString.scala` (lines 95-115), not inside `object JavaClassName` or in `MiscMacros`. `TypeString.materializeImpl` is INSIDE the companion.
   - What's unclear: Why the asymmetry? Likely `derivedImpl` is referenced from `JavaClassNameLowPriority` trait which is sibling to the companion.
   - Recommendation: Match fork exactly. Don't refactor.

3. **`HasAnnotation` shape change source-compat impact?**
   - What we know: Current stub uses `final class HasAnnotation[A,T] private ()` + `create` factory. Fork uses `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A` with no `create`.
   - What's unclear: Whether any code in our tree imports `HasAnnotation.create`. Need to `git grep 'HasAnnotation\.create'` before the AnnotationOf slice.
   - Recommendation: Plan-task includes pre-port grep + MIGRATION.md §3 entry for the API reshape + the `RefiningAnnotation` bound (tightening — may break Scala-2 `Annotation` callers).

4. **`SealedEnumCompanion.values` field shape: `lazy val ISeq[T]` (current) vs `def ISeq[T]` (fork) vs `def values: ISeq[T]`?**
   - What we know: Current stub has `lazy val values: ISeq[T]` (Phase-1 fix for Scala 3 lazy-override semantics). Fork has `def values: ISeq[T]` (no body — abstract).
   - What's unclear: Whether subclasses currently rely on `lazy val` (versus `def values = caseObjects`).
   - Recommendation: Diff per-subclass before flipping. Likely already-handled in Phase 1/2; verify during SealedUtils slice planning.

## Validation Architecture

### Test Framework
| Property | Value |
|----------|-------|
| Framework | ScalaTest 3.2.x (`org.scalatest.funsuite.AnyFunSuite`, `wordspec.AnyWordSpec`) |
| Config file | `project/Commons.scala` (libraryDependencies) |
| Quick run command | `sbt 'core/testOnly com.avsystem.commons.misc.*'` |
| Full suite command | `sbt commons-core/test` (then `commons-jvm/test` for cross-module) |

### Phase Requirements → Test Map
| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| TYPESTRING-01 | `TypeString.of[List[Int]]` produces `"List[Int]"` | unit | `sbt 'core/testOnly *.SharedExtensionsTest'` | partial — fork has `dcf60e5d` re-enable; covered by SharedExtensionsTest |
| JAVACLASSNAME-01 | `JavaClassName.of[Foo]` matches `classOf[Foo].getName` | unit | `sbt 'core/testOnly *.JavaClassNameTest'` | wrapped — fork test absent (was in `macros/JavaClassNameTest`, depends on `TestMacros` — out of scope) |
| ANNOTOF-01 | `AnnotationOf[awesome, Foo]` returns annotation; absent → compile fail | unit | `sbt 'core/testOnly *.AnnotationOfTest'` | wrapped — un-wrap during slice |
| APPLIERUNAPPLIER-01 | `Applier[Foo].apply(Seq(1, "x"))` reconstructs case class | unit | `sbt 'core/testOnly *.ApplierUnapplierTest'` | wrapped — un-wrap during slice (fork: `7085bd8f` re-enabled via Mirror) |
| DELEGATION-01 | Stub compiles; tests stay `ignore`d (fork matches) | manual-only | n/a | wrapped + fork keeps `ignore` |
| SEALEDUTILS-01 | `SealedUtils.caseObjects[Color]` returns all case objects | unit | `sbt 'core/testOnly *.SealedEnumTest *.NamedEnumTest'` | un-wrap during slice |
| VALUEENUM-01 | `Weekday.values` has correct ordinals and names | unit | `sbt 'core/testOnly *.ValueEnumTest'` | wrapped — un-wrap during slice |
| BIDIRECTIONAL-01 | Callers fail at compile time with deprecation + error message | compile-test (manual or pending) | n/a | fork drops the test entirely with `DROPPED:` block-comment |

### Sampling Rate
- **Per task commit:** `sbt 'core/testOnly com.avsystem.commons.misc.<Feature>Test'` (10-30s per leaf)
- **Per wave merge:** `sbt 'commons-core/compile ;commons-core/test ;scalafmtCheckAll'` (~2-5 min)
- **Phase gate:** Full suite green before `/gsd:verify-work` + `git grep '???' core/src/main/scala/com/avsystem/commons/misc/` shows only Delegation + DelegationApply stubs (which match fork)

### Wave 0 Gaps
- [ ] None — test infrastructure exists; per-slice un-wrap is the action, not authoring. Fork tests for each leaf are in `origin/master:core/src/test/scala-3/com/avsystem/commons/misc/<Feature>Test.scala` and can be cribbed if our wrapped version diverges from fork.

## Sources

### Primary (HIGH confidence)
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala` — 399 LOC, owns all leaf impls
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala` — TypeString + JavaClassName (full impl)
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala` — 7 trait + opaque type
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala` — Mirror-based
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala` — deprecated stub
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Delegation.scala` — stub trait mix-in
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala` — pure inline
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala` — top-level `valNameImpl`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala` — Phase 4 staging stubs (`'{ ??? }`)
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala` — Phase 4 aggregator
- `origin/master:core/src/test/scala-3/com/avsystem/commons/misc/<Feature>Test.scala` — fork tests for un-wrap target
- Scala 3 docs: https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html
- Scala 3 docs: https://docs.scala-lang.org/scala3/reference/contextual/derivation.html

### Secondary (MEDIUM confidence)
- Fork commit `31970ec7` — AnnotationOf/OptAnnotationOf/AnnotationsOf real impls
- Fork commit `24e801ec` — SelfAnnotation/SelfOptAnnotation/SelfAnnotations real impls
- Fork commit `7085bd8f` — ApplierUnapplierTest re-enable
- Fork commit `f5c0b17e` — deprecate Bidirectional
- Fork commit `3ec8c125` — SealedUtils uses `scala.ValueOf`

### Tertiary (LOW confidence)
- (none — every load-bearing claim is backed by a fork file directly inspected during this research)

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — every API is stdlib or fork-owned, all verified by reading the source files
- Architecture: HIGH — fork shape is fully documented above; planner can pattern-match
- Pitfalls: HIGH — pitfalls 1, 3, 4, 5, 7 surfaced from direct fork-source inspection (not inference)
- Open questions: MEDIUM — Q1 (MiscMacros foundation slicing) and Q3 (HasAnnotation API impact) need executor-time confirmation; Q4 needs a `git grep` check

**Research date:** 2026-06-02
**Valid until:** 2026-07-02 (30 days — stable Scala 3.8.2 + fork at frozen baseline; refresh if fork master advances or Scala 3.9 lands)
