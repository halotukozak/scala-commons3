# Phase 3: Scala 3 syntax modernization — Research

**Researched:** 2026-06-01
**Domain:** Scala 3 syntax migration (implicit class → extension, HKT `_` → `?`, implicit def/val → given)
**Confidence:** HIGH (fork-master shape verified; live inventory measured via `git grep`)

## Summary

Phase 3 is mechanical and well-bounded: fork `origin/master` already carries the exact transformations needed
for all three slices. The path mapping is hybrid — fork's `mongo/` is single-source (matches our tree 1:1),
fork's `core/` is still cross-build (`core/src/main/scala-3/X` → our `core/src/main/scala/X`). Most fork
commits are sub-20-file diffs, so per-slice PRs land comfortably small.

Live inventory after Phase 1/2:

| Idiom (live, main-source only) | Files | Notes |
|--------------------------------|-------|-------|
| `implicit class` | **1 file** (`core/.../serialization/GenCodec.scala`, 4 private value-class wrappers) | Most `implicit class` already converted to `class X` + `implicit def xOps` during Phase 2 stub-driven cascades |
| HKT `_`/`_,_` wildcards | **15 files** in `core/src/main/scala/` (live), plus broader presence under `mongo/` (~30 files per fork commit 848b8e9e) | False positives: kind-param declarations `K[_]` are intentionally kept; only applied positions get `?` |
| `implicit def`/`implicit val`/`(implicit …)` | **111 files** mentioning `implicit ` in main sources | Most live in interop modules (`jiop/`, `jsiop/`, `concurrent/`) and serialization root |

**Primary recommendation:** Execute three sequential PRs by **per-file `git show origin/master:<path>` copy
into our single-source tree**, then reconcile imports and re-apply our Phase-1/2 stubs. Each slice ends with
a path-mapped diff vs `origin/master` to verify shape parity.

## User Constraints (from CONTEXT.md)

### Locked Decisions

- **Three sequential PRs**, in order: 3.1 `implicit class → extension`, 3.2 HKT `_ → ?`, 3.3 `implicit def/val → given`.
- **Method = translate from fork master**, NOT `sbt-scala3-migrate` plugin. Plugin removed from ROADMAP.
- **Per-file copy approach:** `git show origin/master:core/src/main/scala-3/<path>.scala > core/src/main/scala/<path>.scala`,
  then reconcile (prune dropped-module imports, re-apply Phase 1/2 stubs/deletions, drop `@TodoScala3Migration`,
  `scalafmtAll`).
- **Per-slice acceptance gates** (locked):
  - 3.1: `git grep 'implicit class' core/src/main/scala/` → 0 hits
  - 3.2: `git grep -E '\[_(\s*,\s*_)*\]' core/src/main/scala/` → 0 hits in HKT-applied positions
    (kind-param declarations like `K[_]` left alone, false-positives filtered)
  - 3.3: `git grep -E '^\s*implicit (def|val)' core/src/main/scala/` → only documented exceptions
    (`OptArg.argToOptArg`, `SerializationMacros.fun2GenRef`)
- **Match fork master 1:1.** Named vs anonymous given, extension parameter naming, borderline cases —
  inherit fork's choice. No editorial polish.
- **PR conventions** (cross-cutting): `[Scala 3]` title prefix, milestone "Scala 3" (#1), draft on open,
  no `@nowarn`/`-Wconf`, no `.planning/` in commits, no GSD nomenclature.
- **MIGRATION.md §3 (source-compat)** updated per slice — `extension` vs `implicit class` call-site impact,
  `given` vs `implicit val` summon-vs-named impact.
- **Test sources:** include in same PR as their main-source counterpart when idiom appears AND the test
  file is NOT wrapped in `/* */`. Skip wrapped test files (their un-wrap belongs to Phase 4+ feature
  restoration).

### Claude's Discretion

- Exact diff hunk granularity per file (one commit per slice unless sub-area independent).
- Whether to split slice 3.3 if `39c047eb` proves too big — preference: keep as one PR per "three
  sequential" plan unless reviewer requests split.
- Re-run `scalafmtAll` between commits if formatting drifts.
- Verbatim copy of fork's explanatory comments for borderline kept-implicit cases.

### Deferred Ideas (OUT OF SCOPE)

- Optional braces / significant-indentation (fork `1ceab33a`) — defer to optional Phase 3.4.
- `@nowarn` removal — out of scope (no warnings to suppress on Phase 1 baseline).
- `sbt-scala3-migrate` plugin — superseded.
- Feature ports (meta derivation, GenCodec, RPC, etc.) — Phase 4+.
- Test-source un-wrapping (`/* */` removal) — happens in feature restoration phases.

## Phase Requirements

No `phase_req_ids` provided. Phase 3 success is measured by the **per-slice grep gates** from
CONTEXT.md/ROADMAP.md (replicated above under Locked Decisions). Cross-cutting requirements that
apply: WORKFLOW-01..05, PR-01..03, QUALITY-01..02.

## Standard Stack

No new library introduced. All work uses native Scala 3 language features.

| Feature | Scala 3 syntax | Replaces | Reference |
|---------|---------------|----------|-----------|
| Extension methods | `extension [A](a: A) { … }` | `implicit class XOps[A](…) extends AnyVal` | https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html |
| Given instances | `given Name: T = …` / `given T = …` (anonymous) | `implicit val name: T = …` | https://docs.scala-lang.org/scala3/reference/contextual/givens.html |
| Using clauses | `(using x: T)` | `(implicit x: T)` parameter lists | Scala 3 contextual ref |
| HKT wildcards | `F[?]` | `F[_]` (when applied, not declared) | https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html |
| Conversion givens | `given Conversion[A, B] = a => …` | `implicit def aToB(a: A): B = …` | Scala 3 contextual ref |

**Toolchain (already pinned, no change):** Scala 3.8.2, sbt with sbt-scalafmt; `scalafmtAll` normalizes
formatting drift between fork and our tree.

**Version verification:** N/A — no new dependencies. Compiler/scalafmt versions pinned by Phase 1
build infra.

## Architecture Patterns

### Path Mapping (CRITICAL — verified against fork tree)

Fork `origin/master` is **hybrid** layout. When copying:

| Our path (single-source) | Fork path | Verified via |
|--------------------------|-----------|--------------|
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/<X>.scala` | **same path** | `git ls-tree -r --name-only origin/master \| grep '^mongo'` — 100+ files, all single-source |
| `core/src/main/scala/com/avsystem/commons/<X>.scala` | `core/src/main/scala-3/com/avsystem/commons/<X>.scala` | Fork has 111 files in `scala-3/`, only 6 in `scala/` (shared cross-build helpers like `package.scala`) |
| `core/jvm/src/main/scala/…` | `core/jvm/src/main/scala/…` (single-source in fork too) | jvm-only modules already single-source |
| `core/js/src/main/scala/…` | `core/js/src/main/scala/…` | js-only modules single-source |
| `hocon/src/main/scala/…` | `hocon/src/main/scala/…` | single-source |

**The 6 fork `core/src/main/scala/` files are SHARED across 2.13+3 in fork's cross-build** — they should
NOT be cribbed from (they contain 2.13 macro code). Always crib from the `scala-3/` overlay variant when
it exists; otherwise read carefully.

### Slice 3.1 — `implicit class → extension`

**Fork commit:** `eef0edce` (mongo) + parts of `39c047eb` (core).

**Live inventory in our tree:**
- `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala` lines 400, 416, 429, 450 —
  4 `private implicit class … extends AnyVal` blocks (`IterableOps`, `PairIterableOps`,
  `ListInputOps`, `ObjectInputOps`).

**Fork pattern (verified from fork SharedExtensions and UpdateOperatorsDsl):**

```scala
// BEFORE
implicit class UniversalOps[A](private val a: A) extends AnyVal {
  def opt: Opt[A] = Opt(a)
  def |>[B](f: A => B): B = f(a)
}

// AFTER (fork master shape)
extension [A](a: A) {
  def opt: Opt[A] = Opt(a)
  inline def |>[B](inline f: A => B): B = f(a)
}
```

Note fork frequently adds `inline` to small forwarders. Slice 3.1 should **match fork's
inline/non-inline choice** verbatim; do not introduce or remove `inline` independently.

**Module sweep (slice 3.1 PR scope):**
- `core/src/main/scala/.../serialization/GenCodec.scala` — 4 private value-class extensions
- `mongo/jvm/src/main/scala/.../mongo/typed/MongoEntityCompanion.scala` — `macroDslExtensions`
  (per fork `eef0edce`)
- `mongo/jvm/src/main/scala/.../mongo/typed/UpdateOperatorsDsl.scala` — `ForCollection`
  (per fork `eef0edce`)
- `mongo/jvm/src/main/scala/.../mongo/typed/QueryOperatorsDsl.scala` — `ForCollection`
  (per fork `eef0edce`)
- `mongo/jvm/src/main/scala/.../mongo/reactive/ReactiveMongoExtensions.scala` —
  `PublisherOps` AnyVal → extension on Publisher[T] (per fork `848b8e9e`)
- Test sources matching these (if not wrapped — verify per file)

**Subtle pattern: `given Conversion` over `extension` when receiver inference needed.** Fork's
`UpdateOperatorsDsl.scala` keeps a `given Conversion[…]` (not `extension`) for HKT-receiver
DSLs because `extension` can't infer `C[T]` from named-argument calls like `push(sort = …)`.
Carry the verbatim explanatory comment fork added (we already showed it in tool output above).

### Slice 3.2 — HKT wildcards `_ → ?`

**Fork commit:** `848b8e9e` (35 mongo files; 132 ins / 125 del; commit message explicitly
says "All applied `[_]` type-argument positions → `[?]` via targeted line:col positions
extracted from compiler warnings, to avoid touching kind-param `K[_]` declarations or
value-level `_` placeholders").

**Live inventory in our tree (main sources only, `\[_(\s*,\s*_)*\]` pattern):**

| Module | File count | Notes |
|--------|-----------|-------|
| `core/src/main/scala/` | 15 files | `SharedExtensions.scala`, `collection/CollectionAliases.scala`, `di/{Component,Components}.scala`, `meta/{AdtMetadataCompanion,MetadataCompanion}.scala`, `misc/{SealedUtils,SelfInstance,TypeString,TypedMap}.scala`, `rpc/{AsRawReal,RPCFramework,RpcMetadataCompanion,StandardRPCFramework,rpcAnnotations}.scala`, `serialization/{FieldValues,GenCodec,HasGenCodec,InputOutput,wrappers,macroCodecs}.scala`, `serialization/cbor/{CborAdtMetadata,CborOutput}.scala`, `serialization/json/JsonStringOutput.scala`, `tuples/TupleDerivation.scala` |
| `mongo/jvm/src/main/scala/` | ~30 files | per fork `848b8e9e` diff stat |
| `hocon/src/main/scala/` | TBD | low count expected, verify during planning |
| `core/jvm/src/main/scala/` and `core/js/src/main/scala/` | TBD | likely few |

**False-positive patterns to filter (do NOT rewrite):**
- **Kind-param declarations**: `class Foo[F[_]]` / `trait Bar[K[_]]` / `def baz[M[_]: Monad]` —
  declaration positions. The `_` is the wildcard in the kind parameter declaration. Leave alone.
- **Value-level placeholders**: `(_, _)` in tuple deconstruction, `_ => …` in lambda — NOT a type
  position. Pattern is `[_]` (square-bracketed in type position), so most lambdas are safe, but
  pattern-match cases like `case (_, _) =>` could lex-match a broad regex.
- **Varargs `: _*`** — not the same syntactic form (already fork-rewritten to `*`).
- **Existential types** removed in Scala 3 — should already be gone from Phase 1 baseline.

**Detection method per fork:** compile with `-Werror`, take line:col positions from compiler
warnings. We may not have `-Werror` on yet; alternative: per-file visual diff vs fork.

**MIGRATION.md §3 impact for 3.2:** None. `F[?]` vs `F[_]` is parse-level only, no call-site
or downstream consumer effect.

### Slice 3.3 — `implicit def/val → given`

**Fork commits:** `39c047eb` (core; eliminates implicit keyword), `ebffde26` (core serialization/cbor),
`eef0edce` (mongo implicit→given+using), `8f70be80` (mongo BsonGenCodecs anonymous given), `848b8e9e`
(mongo remaining implicits + `(implicit X)` → `(using X)` sweep).

**Live inventory in our tree:**

| Path | Pattern count summary |
|------|----------------------|
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala` | 6+ `implicit def xxxOps(…)` (synthesized post-Phase-2 to keep value-class wrappers usable without `implicit class`) |
| `core/jvm/src/main/scala/.../jiop/{GuavaInterop,JOptionalUtils,JStreamUtils,Java8CollectionUtils,JavaTimeInterop,Scala*Stream}.scala` | ~40 `implicit def xxx2AsScala/2AsJava` Java interop conversions — pure Conversion candidates |
| `core/js/src/main/scala/.../jsiop/JsInterop.scala` | 3 `implicit def` (jsDate, undefOr, jsOpt) |
| `core/jvm/src/main/scala/.../concurrent/BlockingUtils.scala` | 2 `implicit def scheduler` |
| `core/src/main/scala/.../misc/{Implicits,OptArg}.scala` | borderline (see below) |
| `core/src/main/scala/.../serialization/SerializationMacros.scala` | borderline `fun2GenRef` |
| `mongo/**` | extensive — `(implicit X: T)` → `(using X: T)` sweep across 60+ files (fork `848b8e9e` + `eef0edce`) |
| `benchmark/jvm/src/main/scala/.../ser/{GenCodecBenchmarks,StreamInputOutputBenchmark}.scala` | 4 `implicit val/def` cosmetic |
| `hocon/src/main/scala/.../hocon/{ConfigCompanion,HTree}.scala` | inspect during planning |

**Fork pattern (verified from `BsonGenCodecs.scala`):**

```scala
// BEFORE
trait BsonGenCodecs {
  implicit def objectIdCodec: GenCodec[ObjectId] = BsonGenCodecs.objectIdCodec
  implicit def objectIdKeyCodec: GenKeyCodec[ObjectId] = BsonGenCodecs.objectIdKeyCodec
}
object BsonGenCodecs {
  implicit val objectIdCodec: GenCodec[ObjectId] = GenCodec.nullable(…)
}

// AFTER (fork master shape)
trait BsonGenCodecs {
  export BsonGenCodecs.given
}
object BsonGenCodecs {
  given GenCodec[ObjectId] = GenCodec.nullable(…)
  given GenKeyCodec[ObjectId] = GenKeyCodec.create(new ObjectId(_), _.toHexString)

  // Source-compat aliases for callers that previously referenced these by name.
  @deprecated("Use summon[GenCodec[ObjectId]]", since = "scala-3")
  def objectIdCodec: GenCodec[ObjectId] = summon
  @deprecated("Use summon[GenKeyCodec[ObjectId]]", since = "scala-3")
  def objectIdKeyCodec: GenKeyCodec[ObjectId] = summon
}
```

**Key fork patterns:**
- **Anonymous `given T = …`** (not named) for canonical codec/conversion instances.
- **`export X.given`** in trait when companion holds the canonical givens.
- **`@deprecated` named-def shims** for source-compat with callers that referenced by name.
- **`given Conversion[A, B] = a => …`** for `implicit def aToB(a: A): B = …` style.
- **`(implicit X: T)` → `(using X: T)`** for parameter lists, sed-applicable per fork `eef0edce`
  commit message.
- **`implicit object X extends Y` → `given X: Y with { … }`** (or `given Y with { … }` if anonymous).

### Borderline implicits — KEEP as `implicit` (verbatim from fork)

These are NON-NEGOTIABLE preservations. Copy fork's explanatory comments verbatim.

| Location | Why kept | Fork file |
|----------|----------|-----------|
| `OptArg.argToOptArg` in `core/src/main/scala/.../misc/OptArg.scala` | Polymorphic `Conversion[A, OptArg[A]]` generates a clashing erasure bridge — `A` and the `OptArg` value class both erase to `Object`. Keep `implicit def argToOptArg[A](value: A): OptArg[A] = OptArg(value)`. Fork commit `39c047eb` message documents this. | fork `scala-3/.../misc/OptArg.scala` |
| `SerializationMacros.fun2GenRef` in `core/src/main/scala/.../serialization/SerializationMacros.scala` | Body is a macro splice over `inline fun` argument — can't be expressed as non-inline `Conversion[S => T, GenRef[S, T]]`'s `apply`. Keep `inline implicit def fun2GenRef[S, T](inline fun: S => T): GenRef[S, T] = ${ SerializationMacros.genRefImpl[S, T]('fun) }`. Fork commit `ebffde26`. | fork `scala-3/.../serialization/SerializationMacros.scala` |

**Slice 3.3 acceptance gate must whitelist these two by name.**

### MIGRATION.md §3 (source-compat) deltas per slice

Each slice's PR updates `MIGRATION.md` with downstream call-site implications:

**3.1 (`implicit class → extension`):**
- Caller side typically transparent (extension methods are resolved by same name).
- BUT: explicit construction syntax (`new MyOps(value).method()`) breaks — `extension` has no
  corresponding type. Downstream code that referenced `XOps`/`MyOps` by name will not compile.
- Wherever the original `implicit class` was `private`, no public impact.

**3.2 (`F[_]` → `F[?]`):**
- No call-site impact. Pure type-argument applied-position syntax change.

**3.3 (`implicit def/val` → `given`):**
- Source-compat break for callers that referenced the implicit by name (e.g. `BsonGenCodecs.objectIdCodec`).
  Fork mitigates with `@deprecated def name: T = summon` shims — preserve these.
- `(implicit x: T)` callers using named-argument syntax (`foo()(x = myImplicit)`) need to become `using` —
  but Scala 3 accepts `using` syntax at call site, and unnamed-implicit call sites unchanged.
- `implicit object X extends Y` → `given X: Y with { … }` — name-referencing callers OK; `import X._`
  needs `import X.given` for given-import semantics.

### Project Layout

```
core/src/main/scala/          <-- target for slice copies from fork's scala-3/ overlay
core/jvm/src/main/scala/      <-- target for jvm-only fork files (same path)
core/js/src/main/scala/       <-- target for js-only fork files (same path)
mongo/jvm/src/main/scala/     <-- fork already single-source; direct path match
mongo/js/src/main/scala/      <-- direct path match
hocon/src/main/scala/         <-- direct path match
benchmark/jvm/src/main/scala/ <-- direct path match
```

Skip: `analyzer/` (commented out per Phase 1), `jetty/` (skipped), `spring/` (deleted),
`commons-macros/` (deleted), `core/src/main/scala-2/` and `core/src/main/scala-2.13/` (don't exist
post-pivot).

### Anti-Patterns to Avoid

- **DO NOT run `sbt-scala3-migrate` plugin.** Superseded; user-locked decision.
- **DO NOT rewrite from scratch.** Always crib from fork master per `[[feedback-crib-from-master]]`.
- **DO NOT mix slices.** 3.1 ≠ 3.2 ≠ 3.3; do not bundle.
- **DO NOT touch kind-param declaration `K[_]`** when sweeping 3.2.
- **DO NOT add `@nowarn`/`-Wconf`** to suppress new warnings — fix at source.
- **DO NOT introduce significant-indentation** (deferred to optional Phase 3.4).
- **DO NOT un-wrap `/* */` test files** in this phase.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Mechanical syntax rewrite | sed/awk/regex scripts ad-hoc | Per-file `git show origin/master:<path>` then reconcile | Fork already did the surgery; matching shape 1:1 is the user-locked decision |
| Conversion `implicit def aToB` | Custom `implicit conversion` machinery | `given Conversion[A, B] = a => …` | Scala 3 stdlib |
| Named implicit at companion | Hand-written shim chains | `@deprecated def name: T = summon` (fork pattern) | Preserves source-compat with named-import callers |
| Detect false-positive `[_]` | Custom AST walker | Manual diff vs fork `848b8e9e` + `-Werror` compiler warnings | Compiler already does the AST work |

**Key insight:** Fork master is a working oracle. Every divergence from fork = risk; every match =
free correctness.

## Common Pitfalls

### Pitfall 1: Cherry-pick fails on path mismatch
**What goes wrong:** `git cherry-pick 39c047eb` fails because fork's `core/src/main/scala-3/` doesn't
exist in our tree.
**Why:** Post-pivot we have single-source `core/src/main/scala/`.
**How to avoid:** Use `git show origin/master:core/src/main/scala-3/<path>` + redirect into `core/src/main/scala/<path>`.
**Warning signs:** Cherry-pick conflicts mentioning `scala-3/` paths.

### Pitfall 2: Re-introducing Phase-1 stub regressions
**What goes wrong:** Copying fork file blindly overwrites our Phase-2 `???` stubs (e.g. mongo macro
stubs `BsonRef.Creator.ref`, `DataRefDsl.{ref,as,is,isNot}`, `TypedMongoUtils.optionalizeFirstArg`,
`MongoEntityCompanion.ID` widening, `K[_]` → `K[Any]` widenings) — compile breaks immediately.
**Why:** Fork file is pre-stub; assumes full macro impls compile.
**How to avoid:** After `git show > file`, run `git diff` and **manually re-apply Phase-1/2 stubs**.
Always run `sbt commons-jvm/compile commons-js/compile Test/compile scalafmtCheckAll` after
each per-file copy.
**Warning signs:** Compile errors mentioning macro impls, `E#IDType`, `K[_]` arity.

### Pitfall 3: Sweeping kind-param declaration `K[_]` to `K[?]`
**What goes wrong:** `class TypedMap[K[_]]` → `class TypedMap[K[?]]` is a syntax error.
**Why:** `K[_]` here is a **kind-parameter declaration**, not an applied type — `_` is the wildcard for
the inner type position of the kind. Scala 3 still uses `_` in this position; `?` only replaces applied
wildcards.
**How to avoid:** Slice 3.2 only sweeps applied positions. Use compiler `-Werror` warnings or per-file
diff against fork `848b8e9e`. Cross-check: position must be inside an applied type (type-argument
position), not a type parameter declaration.
**Warning signs:** Compile error "wildcard imports are not allowed in type parameter position" — you
swept too aggressively.

### Pitfall 4: Conversion erasure bridge collision (OptArg)
**What goes wrong:** Naively converting `OptArg.argToOptArg[A](value: A): OptArg[A]` to
`given Conversion[A, OptArg[A]]` produces a JVM erasure clash with `OptArg`'s value-class erasure
(both `A` and `OptArg[A]` erase to `Object`, bridge method collides).
**How to avoid:** Whitelist this `implicit def` — keep verbatim. Slice 3.3 acceptance gate must
exclude it.
**Warning signs:** Linker error / duplicate method signature after build.

### Pitfall 5: Macro-splice-over-inline-arg can't be Conversion
**What goes wrong:** `inline implicit def fun2GenRef[S, T](inline fun: S => T): GenRef[S, T] =
  ${ SerializationMacros.genRefImpl[S, T]('fun) }` — converting to `given Conversion[S => T, GenRef[S, T]]`
fails because `Conversion.apply` is non-inline; the `'fun` quote in the macro splice loses `inline`-ness.
**How to avoid:** Keep verbatim with fork's documenting comment.
**Warning signs:** Compile error inside the macro splice.

### Pitfall 6: Extension body can't contain imports
**What goes wrong:** Mongo `UpdateOperatorsDsl.scala` and `QueryOperatorsDsl.scala` originally
had `implicit class ForCollection` with internal `import MongoUpdateOperator._`. Naive `extension`
conversion fails — extension body cannot contain imports.
**How to avoid:** Hoist imports **above** the `extension` block, as fork `eef0edce` did.
**Warning signs:** Parse error "expected `def` but found `import`".

### Pitfall 7: Named-arg call breaks pure `extension` on HKT receiver
**What goes wrong:** `extension [C[X] <: Iterable[X], T, R] (dsl: UpdateOperatorsDsl[C[T], R])
  def push(sort: ...) = …` — Scala 3 can't infer `C`/`T` from named-argument call like
`upd.push(sort = …)`.
**How to avoid:** Use `given … => Conversion[UpdateOperatorsDsl[C[T], R], ForCollection[C, T, R]]`
(fork's exact shape), not `extension`. Carry fork's explanatory comment verbatim.
**Warning signs:** Compile error "cannot infer type C/T at named-argument call site".

### Pitfall 8: Skipping the post-slice scalafmt
**What goes wrong:** Fork's `.scalafmt.conf` config may differ slightly from ours (especially around
the `1ceab33a` significant-indentation enable on fork that we defer). Copied files appear "drifted".
**How to avoid:** Run `sbt scalafmtAll` immediately after copying each file. If diff explodes, the
fork file used significant-indentation syntax — re-fetch and re-format.
**Warning signs:** `scalafmtCheckAll` flagging hundreds of lines after slice.

## Code Examples

Verified patterns from fork `origin/master`:

### Slice 3.1: implicit class AnyVal → extension

```scala
// Source: git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala
trait SharedExtensions {
  extension [A](a: A) {
    inline def |>[B](inline f: A => B): B = f(a)
    inline def applyIf[A0 >: A](inline predicate: A => Boolean)(inline f: A => A0): A0 =
      if (predicate(a)) f(a) else a
    @nowarn
    inline def discard: Unit = ()
    def option: Option[A] = Option(a)
    def opt: Opt[A] = Opt(a)
    def unboxedOpt[B](using unboxing: Unboxing[B, A]): Opt[B] = opt.map(unboxing.fun)
    inline def setup(inline code: A => Any): A = { code(a); a }
    inline def matchOpt[B](inline pf: PartialFunction[A, B]): Opt[B] = pf.applyOpt(a)
  }
}
```

### Slice 3.1: HKT receiver — use `given Conversion`, not `extension`

```scala
// Source: git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala
object UpdateOperatorsDsl {
  import MongoUpdateOperator._  // hoisted outside extension/given body

  // A `given Conversion` (not an `extension`) is used here so the higher-kinded `C[T]` is unified once,
  // at conversion time, against the receiver's `UpdateOperatorsDsl[C[T], R]` base type. Plain extension
  // methods fail to infer `C`/`T` from the receiver for named-argument calls such as `push(sort = ...)`.
  given [C[X] <: Iterable[X], T, R] => Conversion[UpdateOperatorsDsl[C[T], R], ForCollection[C, T, R]] =
    ForCollection(_)

  class ForCollection[C[X] <: Iterable[X], T, R](dsl: UpdateOperatorsDsl[C[T], R]) { … }
}
```

### Slice 3.3: anonymous given + deprecated shim

```scala
// Source: git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala
trait BsonGenCodecs {
  export BsonGenCodecs.given
}
object BsonGenCodecs {
  given TransparentWrapping[ObjectId, ObjectId] = TransparentWrapping.identity
  given GenCodec[ObjectId] = GenCodec.nullable(…)
  given GenKeyCodec[ObjectId] = GenKeyCodec.create(new ObjectId(_), _.toHexString)

  @deprecated("Use summon[GenCodec[ObjectId]]", since = "scala-3")
  def objectIdCodec: GenCodec[ObjectId] = summon
  @deprecated("Use summon[GenKeyCodec[ObjectId]]", since = "scala-3")
  def objectIdKeyCodec: GenKeyCodec[ObjectId] = summon
}
```

### Slice 3.3: borderline kept-implicits

```scala
// Source: git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala
// (paraphrased — see CONTEXT for full)
object OptArg {
  /** Kept as `implicit def` (not `given Conversion[A, OptArg[A]]`): a polymorphic Conversion would
    * generate a JVM erasure-bridge collision — `A` and the `OptArg` value class both erase to `Object`. */
  implicit def argToOptArg[A](value: A): OptArg[A] = OptArg(value)
}

// Source: git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/SerializationMacros.scala
object GenRef {
  // Kept as `inline implicit def` (not a `given Conversion`): the body is a macro splice over the
  // `inline fun` argument, which a `Conversion[S => T, GenRef[S, T]]`'s non-inline `apply` cannot carry.
  inline implicit def fun2GenRef[S, T](inline fun: S => T): GenRef[S, T] =
    ${ SerializationMacros.genRefImpl[S, T]('fun) }
}
```

### Slice 3.3: implicit param list → using

```scala
// BEFORE
def write[T](output: BsonValueOutput, value: T)(implicit codec: GenCodec[T]): Unit = …

// AFTER
def write[T](output: BsonValueOutput, value: T)(using codec: GenCodec[T]): Unit = …
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `implicit class XOps[A](…) extends AnyVal` | `extension [A] (a: A) { … }` | Scala 3.0 | No-runtime-cost, clearer syntax |
| `implicit def aToB(a: A): B = …` | `given Conversion[A, B] = a => …` | Scala 3.0 | Conversions explicit; `import scala.language.implicitConversions` still required for usage |
| `implicit val x: T = …` | `given T = …` (anonymous) or `given x: T = …` (named) | Scala 3.0 | Anonymous preferred for canonical instances |
| `(implicit x: T)` parameter list | `(using x: T)` | Scala 3.0 | Call sites can keep old syntax or move to `using` |
| `F[_]` (applied) | `F[?]` (applied) | Scala 3.0; `_` deprecated for applied positions | Pure syntax |
| `F[_]` (kind-param declaration) | `F[_]` (unchanged) | n/a | `_` is the wildcard inside a kind-param decl |
| `implicit object X extends Y` | `given X: Y with { … }` | Scala 3.0 | `import X.given` needed for `given`-import semantics |

**Deprecated/outdated:**
- `implicit class` — kept as legacy syntax, still compiles in Scala 3 but `extension` is canonical
- `_` applied wildcard — deprecated, emits `-Werror` warning
- Existential types (`T forSome { … }`) — removed in Scala 3 entirely

## Open Questions

1. **Slice 3.2 false-positive enumeration in `core/`**
   - What we know: 15 core files contain `[_]` / `[_,_]` patterns; fork commit `848b8e9e` covers
     mongo exclusively (35 files).
   - What's unclear: which core occurrences are kind-param declarations vs applied positions.
     Fork commit `39c047eb` / `ebffde26` may incidentally fix some applied positions in core.
   - Recommendation: planner generates per-file inventory during 3.2 plan creation
     (`git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala/**/*.scala'`), then classifies
     each line as kind-decl (skip) or applied (rewrite). Cross-check against
     `git show origin/master:core/src/main/scala-3/<file>` per file.

2. **Hocon module inventory**
   - What we know: 2 hocon files mention `implicit ` in main; no `implicit class` survives.
   - What's unclear: exact count of `implicit def/val` to rewrite in slice 3.3, and whether
     `(implicit X)` param lists exist.
   - Recommendation: planner runs `git grep -nE '\bimplicit\s+(def|val)' -- 'hocon/src/main/scala'`
     during 3.3 plan creation.

3. **Stacking strategy 3.1→3.2→3.3 — branches vs each-off-tip**
   - CONTEXT.md says "default: each off `scala-3`, merge in order 3.1 → 3.2 → 3.3".
   - Risk: if 3.1 lands before 3.2 starts, 3.2 must rebase. If they stack (3.2 off 3.1), 3.2
     CI is gated on 3.1 merging.
   - Recommendation: **each PR off `scala-3` tip** (upstream `1561d8dc`+ whatever's currently
     merged). Sequential ordering at merge time. If 3.1 hasn't merged when 3.2 is ready, 3.2
     branches off 3.1 locally and rebases on `scala-3` after 3.1 lands. Confirm with user
     during plan checkpoint.

4. **Significant-indentation alignment with fork**
   - Fork commit `1ceab33a` enables significant-indentation in scalafmt. We **defer** this to
     optional Phase 3.4 (CONTEXT.md `<deferred>`).
   - Risk: cribbed files use indented syntax → our `scalafmtCheckAll` (braces dialect) reformats
     to braces, producing visual divergence from fork.
   - Recommendation: accept the braces-reformatted output as our canonical post-slice form.
     Diff-comparison against fork remains semantically-equivalent.

5. **`(implicit X: T)` parameter-list sweep granularity**
   - Fork commit `eef0edce` says "All `(implicit X: T)` parameter lists → `(using X: T)` (sed-applied
     across mongo/)".
   - This is mechanical but **touches many files** (~60 mongo). Could explode slice 3.3 PR size.
   - Recommendation: bundle into slice 3.3, but commit separately per-module
     (`refactor(mongo): (implicit) → (using)` as own commit within the 3.3 PR) for reviewer
     legibility. If reviewer asks to split, spin off a 3.4.

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | ScalaTest 3.2.20 + ScalaCheck 1.19.0 (scalatestplus-scalacheck-1-16 3.2.14.0) |
| Config file | `project/Commons.scala` (sbt) |
| Quick run command | `sbt 'Test/compile'` (fast — already green from Phase 1 Plan 05) |
| Full suite command | `sbt 'compile ;Test/compile ;scalafmtCheckAll'` |

### Phase Requirements → Test Map

| Slice | Behavior | Test Type | Automated Command | File Exists? |
|-------|----------|-----------|-------------------|-------------|
| 3.1 | `git grep 'implicit class' core/src/main/scala/` → 0 hits | grep gate | `! git grep -q 'implicit class' -- 'core/src/main/scala'` | ✅ (built-in) |
| 3.1 | Compile + tests still green | smoke (compile) | `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` | ✅ |
| 3.2 | `git grep -E '\[_(, _)*\]' core/src/main/scala/` → only kind-decl positions | grep gate (manual filter) | `git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala' \| grep -v '^.*class.*\[.*\[_\]' && exit 1 \|\| exit 0` (approximate; verify each remaining line is a kind-param decl) | ✅ (built-in) |
| 3.2 | Compile + tests still green | smoke (compile) | `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` | ✅ |
| 3.3 | `git grep -E '^\s*implicit (def\|val)' core/src/main/scala/` → 2 whitelisted hits | grep gate (whitelisted) | `git grep -nE '^\s*(inline\s+)?implicit\s+(def\|val)' -- 'core/src/main/scala' 'mongo' 'hocon'` then manually verify only `OptArg.argToOptArg` and `SerializationMacros.fun2GenRef` remain | ✅ (built-in) |
| 3.3 | Compile + tests still green | smoke (compile) | `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` | ✅ |
| All | No new `@nowarn` / `-Wconf` | grep gate | `! git diff scala-3..HEAD -- '*.scala' \| grep -E '^\+.*(@nowarn\|-Wconf)'` | ✅ (built-in) |
| All | No `.planning/` in commits | grep gate | `! git log scala-3..HEAD --name-only \| grep -q '^\.planning/'` | ✅ (built-in) |
| All | No GSD nomenclature in commit messages | grep gate | `! git log scala-3..HEAD --pretty=%B \| grep -iE 'gsd\|phase \|plan-'` | ✅ (built-in) |
| All | Path-mapped diff parity with fork | manual review | `diff <(git show HEAD:<file>) <(git show origin/master:<fork-path>)` per file (semantic, not byte-equal due to scalafmt) | ✅ (built-in) |

### Sampling Rate

- **Per file copy:** `sbt 'commons-jvm/compile'` (~10-30s warm) — must stay green after each
  per-file `git show > file` reconciliation.
- **Per slice commit:** `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'`
- **Per slice PR pre-push:** Full suite green + grep gate green + `MIGRATION.md` §3 update committed.
- **Phase 3 gate (after all three slices merge):** Re-run full suite on `scala-3` tip; verify all
  three grep gates simultaneously green.

### Wave 0 Gaps

None — existing test infrastructure covers all phase requirements:
- Phase 1 Plan 05 left `Test/compile` green across all enabled modules.
- ScalaTest framework already in place via `Commons.scala` library dependencies.
- No new test files need creation; this phase is pure syntax rewrite — semantic behavior
  preserved means existing tests stay valid.
- All acceptance gates are `git grep` + `sbt compile`, both already available.

## Sources

### Primary (HIGH confidence)

- Fork `origin/master` commits (verified via `git show`):
  - `39c047eb` — implicit → given sweep in core (covers slice 3.3 most thoroughly)
  - `ebffde26` — finish implicit → given in core serialization/cbor
  - `eef0edce` — implicit val/def/class → given/using/extension in mongo (slices 3.1 + 3.3)
  - `8f70be80` — BsonGenCodecs anonymous given + deprecated named def
  - `848b8e9e` — clear -Werror warnings, eliminate remaining implicits + HKT `[_]` → `[?]`
- Fork `origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala`
  — verified extension block shape
- Fork `origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala`
  — verified anonymous-given + deprecated-shim pattern
- Fork `origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala`
  — verified `given Conversion` over `extension` for HKT-receiver pattern
- Scala 3 official docs:
  - https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html
  - https://docs.scala-lang.org/scala3/reference/contextual/givens.html
  - https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html
- `.planning/phases/03-scala-3-syntax-modernization/03-CONTEXT.md` — user decisions
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md`:
  - `feedback_crib_from_master.md` — translate, don't rewrite
  - `feedback_small_scoped_prs.md` — three small PRs
  - `feedback_pr_title_prefix.md` — `[Scala 3]` prefix
  - `feedback_fix_dont_suppress_warnings.md` — no `@nowarn`/`-Wconf`
  - `feedback_scala3_migrate_syntax.md` — SUPERSEDED by this CONTEXT
- `.planning/MIGRATION.md` — current §3 source-compat section
- Live inventory via `git grep` (verified 2026-06-01):
  - 1 file with live `implicit class` (GenCodec.scala, 4 private wrappers)
  - 15 core/src/main/scala files with HKT wildcards
  - 111 main-source files mentioning `implicit `

### Secondary (MEDIUM confidence)

- Fork commit `1ceab33a` (scalafmt significant-indentation) — referenced in CONTEXT as
  deferred to Phase 3.4. Not applied here.

### Tertiary (LOW confidence)

None — all critical claims are verified against fork source or `git grep`.

## Metadata

**Confidence breakdown:**
- Slice 3.1 scope (`implicit class` → `extension`): HIGH — only 1 file live; fork pattern verified
- Slice 3.2 scope (HKT wildcards): HIGH for mongo (fork `848b8e9e` lists 35 files explicitly);
  MEDIUM for core (15 files inventoried, kind-decl vs applied classification deferred to plan)
- Slice 3.3 scope (implicit → given): HIGH — fork commits cover both core and mongo exhaustively;
  borderline cases (OptArg, fun2GenRef) verbatim-documented
- Path mapping (fork → our tree): HIGH — verified via `git ls-tree` (mongo direct, core via
  `scala-3/` overlay)
- Borderline kept-implicits: HIGH — verbatim text and rationale fetched from fork
- MIGRATION.md §3 impact: HIGH — fork's `@deprecated def name: T = summon` pattern explicit
- Pitfalls: HIGH — pitfalls 4, 5, 6, 7 verified by fork's explanatory commit messages

**Research date:** 2026-06-01
**Valid until:** ~2026-07-01 (30 days; fork master is stable archive of completed work, no churn risk)

---

## Slice 3.4 — `@inline def` → `inline def`

**Added:** 2026-06-01 (CONTEXT.md slice 3.4 + fork-sweep mandate)

### Scope summary

Sweep all `@inline def` (Scala 2 inlining *hint* — a JVM optimizer suggestion the compiler may
ignore) and rewrite to `inline def` (Scala 3 *true* compile-time inlining — body is spliced at
call site, mandatory, can carry `inline` parameters that participate in compile-time evaluation).

Fork master rewrote this in six `perf(scala-3): inline …` commits. Live inventory in our tree
is **125 `@inline` hits across 7 files**, all confined to `core/src/main/scala/`.

### Live inventory (per file, verified 2026-06-01)

| File | `@inline` count | Fork commit(s) covering it |
|------|----------------:|----------------------------|
| `core/src/main/scala/com/avsystem/commons/misc/Opt.scala` | 34 | `5fafdbd7` |
| `core/src/main/scala/com/avsystem/commons/misc/NOpt.scala` | 32 | `5fafdbd7` |
| `core/src/main/scala/com/avsystem/commons/misc/OptRef.scala` | 29 | `5fafdbd7` |
| `core/src/main/scala/com/avsystem/commons/misc/OptArg.scala` | 22 | `5fafdbd7` |
| `core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala` | 5 | **NOT covered (fork preserves)** |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala` | 2 | **NOT covered (RPC dropped from scala-3)** |
| `core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala` | 1 | **NOT covered (fork preserves)** |
| **Total** | **125** | — |

**Critical finding:** Three of seven live files are NOT touched by the fork's inline rewrite.

- **CborInput.scala** (`@inline private def bits(off: Int): Long`): fork target keeps this
  `@inline` verbatim. Verified via `git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/cbor/CborInput.scala | grep @inline` → 1 hit, identical location/signature.
- **JsonStringInput.scala** (5 `@inline` hits — `read`, `isNext`, `isNextDigit`, `advance`,
  nested `update`): fork target keeps all 5 verbatim. Verified via same `git show` →
  identical 5 hits.
- **RPCFramework.scala** (2 `@inline def apply`): fork's RPC sources live under
  `core/src/main/scala-2.13/`, NOT in scala-3 overlay. Per Phase 1 RPC-disable decision,
  RPC is deferred/deprecated in scala-3; **leave as-is**, don't touch.

This means **slice 3.4 rewrite scope is 4 files / 117 hits** (Opt family), NOT all 7 / 125.

### Files NOT covered by inline commits but ALSO touched by SharedExtensions/jiop commits

Fork commits `33a5b792`, `ad505679`, `ee0be95e`, `a4ddad6b`, `580625a9` also rewrite:

| File | Fork commit | Our tree @inline count today |
|------|-------------|------------------------------:|
| `core/jvm/src/main/scala/.../jiop/ScalaJStream.scala` | `33a5b792`, `ad505679` | **0** (already inlined or removed) |
| `core/jvm/src/main/scala/.../jiop/ScalaJDoubleStream.scala` | `33a5b792`, `ad505679` | 0 |
| `core/jvm/src/main/scala/.../jiop/ScalaJLongStream.scala` | `33a5b792`, `ad505679` | 0 |
| `core/jvm/src/main/scala/.../jiop/ScalaJIntStream.scala` | `33a5b792`, `ee0be95e` | 0 |
| `core/jvm/src/main/scala/.../jiop/JStreamUtils.scala` | `33a5b792` | 0 |
| `core/jvm/src/main/scala/.../jiop/GuavaInterop.scala` | `5fafdbd7` | 0 |
| `core/jvm/src/main/scala/.../jiop/Java8CollectionUtils.scala` | `5fafdbd7` | 0 |
| `core/jvm/src/main/scala/.../jiop/JBasicUtils.scala` | `ad505679` | 0 |
| `core/jvm/src/main/scala/.../jiop/JFunctionUtils.scala` | `580625a9` | 0 |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala` | `a4ddad6b` | 0 |
| `core/jvm/src/main/scala/.../concurrent/TaskExtensions.scala` | `33a5b792` | 0 |

**These files have ZERO `@inline` hits in our tree today** — they were already cleaned up by
Phase 2 stub work or by upstream Scala 2 changes. Slice 3.4 should still **diff them against
fork to verify they match the fork's *target* `inline def` shape** (where applicable —
SharedExtensions in particular has 49 `inline def` and 43 `(inline ` parameter usages in fork
target). If our SharedExtensions lacks these `inline` keywords on functions identical to
fork's, that's a parity gap to close.

**Slice 3.4 acceptance** must cover BOTH:
1. Sweep `@inline` → `inline` in Opt family (117 hits, 4 files).
2. Verify SharedExtensions and jiop files match fork's `inline def` / `inline` parameter shape
   (additive sweep — add `inline` to defs/params where fork did, even though our tree has no
   `@inline` annotation to drive the change).

### Per-file fork commit mapping (final)

| Our file | Fork commit (canonical source for shape) | Our action |
|----------|------------------------------------------|------------|
| `core/.../misc/Opt.scala` | `5fafdbd7` | `git show origin/master:core/src/main/scala-3/.../misc/Opt.scala` → copy+reconcile |
| `core/.../misc/NOpt.scala` | `5fafdbd7` | same |
| `core/.../misc/OptArg.scala` | `5fafdbd7` | same; preserve `argToOptArg` `implicit def` (see slice 3.3 borderline) |
| `core/.../misc/OptRef.scala` | `5fafdbd7` | same |
| `core/.../SharedExtensions.scala` | `a4ddad6b` | crib `inline def` shape (45+ extension methods) |
| `core/jvm/.../jiop/ScalaJStream.scala` | `33a5b792` + `ad505679` | crib `inline def` shape |
| `core/jvm/.../jiop/ScalaJDoubleStream.scala` | `33a5b792` + `ad505679` | same |
| `core/jvm/.../jiop/ScalaJLongStream.scala` | `33a5b792` + `ad505679` | same |
| `core/jvm/.../jiop/ScalaJIntStream.scala` | `33a5b792` + `ee0be95e` | same |
| `core/jvm/.../jiop/JStreamUtils.scala` | `33a5b792` | same |
| `core/jvm/.../jiop/GuavaInterop.scala` | `5fafdbd7` | same |
| `core/jvm/.../jiop/Java8CollectionUtils.scala` | `5fafdbd7` | same |
| `core/jvm/.../jiop/JBasicUtils.scala` | `ad505679` | same |
| `core/jvm/.../jiop/JFunctionUtils.scala` | `580625a9` | same |
| `core/jvm/.../concurrent/TaskExtensions.scala` | `33a5b792` | same |
| `core/.../serialization/cbor/CborInput.scala` | — | **NO CHANGE** (1 `@inline` preserved verbatim) |
| `core/.../serialization/json/JsonStringInput.scala` | — | **NO CHANGE** (5 `@inline` preserved verbatim) |
| `core/.../rpc/RPCFramework.scala` | — | **NO CHANGE** (RPC dropped from scala-3) |

### Scala 2 `@inline` vs Scala 3 `inline` — semantics

| Aspect | Scala 2 `@inline def f = …` | Scala 3 `inline def f = …` |
|--------|-----------------------------|-----------------------------|
| Trigger | JVM optimizer hint; compiler MAY honor under `-opt:l:inline` | Mandatory; compiler MUST splice at every call site |
| Phase | Backend (post-typer optimizer pass) | Typer (rewriting AST before lowering) |
| Inline params (`inline x: T`) | N/A — Scala 2 has no such concept | Permitted; param expression is spliced verbatim at call site, not first-evaluated |
| By-name + inline params | N/A | `inline default: => B` permitted; body evaluation is also spliced |
| Method body restrictions | None (any body) | Body must be "inline-able": no `synchronized`, no `try`/`catch` with side-effecting handler, limited reflection. Most pure forwarders are fine. |
| Effect on call-site | Optional copy of body bytecode | Body AST transplanted into call-site AST (with hygienic renaming) |
| `transparent inline` | N/A | Optional: return type refined per call site (used in macros / typeclass dispatch); fork does NOT use this in slice 3.4 scope |
| `inline val` | N/A (`@inline` only applies to defs) | Permitted (compile-time constant); fork does NOT use in this scope |

**Implication for Opt family:** Most `@inline def` are 1-2 line forwarders like
`def map[B](f: A => B): Opt[B] = if (isEmpty) Opt.Empty else Opt(f(value))` — these inline
cleanly. Fork goes further: `(inline f: A => B)` makes the lambda inline at the call site too,
eliminating the `Function1` allocation entirely. This is the main *performance* motivation
behind the `perf(scala-3)` commit prefix.

### Edge cases where blind conversion would break

Based on inspecting fork's actual rewrites in `Opt.scala`, `NOpt.scala`, `OptRef.scala`,
`OptArg.scala`, `SharedExtensions.scala`:

1. **Recursive defs cannot be `inline`** — Scala 3 compiler errors on `inline def f = … f(…) …`
   with "Maximal number of successive inlines exceeded". Fork's resolution: leave such defs
   as plain `def`. Inventory check: Opt family has NO directly recursive defs (all forwarders),
   so this is a non-issue here. Manual review per file still required.
2. **Non-trivial bodies** — fork keeps `def apply[A](value: A | Null): Opt[A] = if (value != null) new Opt[A](value) else Opt.Empty`
   as a plain `def` (not `inline def`) because it's an object-method allocator, not a forwarder.
   Pattern: **only forwarders / wrappers get `inline`**; allocators, constructors, and methods
   with `new T(...)` allocation paths stay plain `def`.
3. **`def isEmpty`, `def isDefined`, `def get`** — fork keeps these as plain `def` in `Opt`
   companion (verified: lines 71-72 of fork's `Opt.scala`). One-line trivial accessors don't
   need `inline` per fork's convention.
4. **`def unapply`** — fork leaves Scala name-based extractors as plain `def` (not `inline def`).
5. **Extension methods inside `extension [A](a: A) { … }` blocks** — fork DOES use `inline def`
   inside extension blocks (49 instances in SharedExtensions target). Parameter lists can carry
   `inline` (43 sites in SharedExtensions). Slice 3.4 must replicate this.
6. **`@nowarn` on inline forwarders** — fork annotates `inline def discard: Unit = ()` with
   `@nowarn` (the inline causes a "unused" warning at every call site otherwise). Carry verbatim.
7. **`@inline private def` in CborInput / JsonStringInput** — fork DELIBERATELY does NOT
   convert these. Rationale (inferred from fork's silence): these are tight-loop parser hot
   paths where Scala 3 `inline def` would force every call site to re-parse the same body
   (binary bloat), and the JVM optimizer's `@inline` hint is preferred. Preserve.

### Test-source impact

Test files generally use Opt / OptArg / NOpt / OptRef but don't carry `@inline` themselves.
Slice 3.4 has **zero test-source rewrites**. Test compile must stay green to validate
inline-call-site correctness.

Verified: `git grep -nE '@inline' -- 'core/src/test/scala' 'mongo' 'hocon' 'benchmark'` → 0 hits.

### Acceptance gate

**Primary gate** (locked in CONTEXT.md):
```bash
git grep -nE '@inline' -- 'core/src/main/scala' \
  | grep -vE '(CborInput|JsonStringInput|RPCFramework)\.scala:' \
  | wc -l
# Expected: 0
```

**Whitelist** (3 files / 8 hits preserved verbatim):
- `core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala:56` (1 hit)
- `core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala:346,353,356,359,475` (5 hits)
- `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:44,52` (2 hits)

**Secondary gate** — fork-parity for non-`@inline` paths:
```bash
# Verify our SharedExtensions inline-def count matches fork
diff <(git show HEAD:core/src/main/scala/com/avsystem/commons/SharedExtensions.scala | grep -cE '\binline (def|val)\b') \
     <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala | grep -cE '\binline (def|val)\b')
# Expected: identical counts (~49)
```

Run analogous parity diff for each jiop file in scope.

**Compile gate:** `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` green.

**MIGRATION.md §3 entry for slice 3.4:**
- `@inline def` → `inline def` is a **source-compat break** for downstream callers that
  override these methods (Scala 3 `inline def` cannot be overridden — `final` is implicit).
  Subclassing breakage is the primary risk. Mitigation: most `@inline def`s in Opt family are
  on value classes (final by construction); no impact. Document any non-value-class targets
  (likely zero in this scope, but verify per file).
- Binary compat: Scala 3 `inline def` does NOT emit a method in bytecode → MiMa would flag.
  Phase 1 / Phase 2 already established `mimaPreviousArtifacts := Set.empty` so MiMa is off
  for scala-3 baseline. Re-confirm no MiMa gate flips here.

### `@inline val` check

`@inline` in Scala 2 applies only to **methods**. There's no `@inline val` in our tree:
```bash
git grep -nE '@inline\s+(private\s+)?val\b' -- 'core' 'mongo' 'hocon' 'benchmark'
# Expected: 0 hits
```
Verified 2026-06-01 — 0 hits.

### Slice 3.4 PR scope

| Item | Action |
|------|--------|
| PR title | `[Scala 3] @inline def → inline def` |
| PR draft on open | yes (per `[[feedback-pr-draft]]`) |
| Milestone | "Scala 3" (#1) |
| Files touched (main sources) | 4 Opt-family + 11 SharedExtensions/jiop parity diffs = **15 files** |
| Files touched (tests) | 0 |
| Files touched (docs) | `MIGRATION.md` §3 (source-compat — final-implied warning) |
| Branches off | `upstream/scala-3` tip (NOT stacked on 3.1/3.2/3.3) |
| Acceptance | primary grep gate (whitelist) + secondary parity diff + compile + scalafmt |

---

## Broader Fork Sweep — Candidate Future Slices

**Added:** 2026-06-01 (per CONTEXT.md sweep mandate)

Examined every `refactor|fix|perf|chore(scala-3)` commit on fork `origin/master` to identify
mechanical rewrite kinds NOT covered by slices 3.1–3.4. Each candidate below carries a brief
inventory + recommendation (in-scope as Phase 3 slice, or defer to Phase 4+ feature work).

### Candidate 3.5 — `compiletime.defered` → `compiletime.deferred` typo fix

**Fork commit:** `27d32741` — `fix(scala-3,mongo): compiletime.defered → compiletime.deferred typo` (3 files, 6 hits).

**Live inventory in our tree:**
```bash
git grep -nE 'compiletime\.defered' -- '*.scala'
# 0 hits
```

**Recommendation:** **DROP — non-applicable.** Our tree has zero `compiletime.defered`
references (the typo never landed here). The fork commit fixes a fork-local typo. No slice
needed.

### Candidate 3.6 — `implicit def aToB` → `given Conversion[A, B]` (general sweep beyond slice 3.3)

**Fork commit(s):** Subsumed by `eef0edce` and `39c047eb` (already mapped to slice 3.3).

**Recommendation:** **ABSORBED into slice 3.3.** Already in scope; called out as "Conversion
givens" pattern in slice 3.3's fork patterns section. No separate slice.

### Candidate 3.7 — Explicit-nulls patches

**Fork commit:** `b42ab037` — `fix(scala-3,mongo): explicit-nulls patches, BsonRefMacros inline, DataTypeDsl macro stubs` (10 mongo files, +37/-22 lines).

**Live inventory in our tree:**
```bash
git grep -nE '\.nn\b' -- 'core/src/main/scala' 'mongo' 'hocon' 'benchmark'
# 2 hits — extremely sparse
```

**What it does:** Adds `.nn` (post-fix non-null assertion) and `| Null` type annotations where
fork's compiler was run with `-Yexplicit-nulls`. Patches `getDefault().nn`, `field.asInstanceOf[T].nn`,
etc.

**Status of `-Yexplicit-nulls` in our tree:** Not enabled (verified via grep in
`project/Commons.scala` — no `-Yexplicit-nulls`/`-Yexplicit-null` flag).

**Recommendation:** **DEFER to Phase 4+ as opt-in slice.** Two paths:
1. If we don't enable `-Yexplicit-nulls`, the fork patches are moot (no compile errors to fix).
2. If we want to match fork's null-safety posture, enabling `-Yexplicit-nulls` is itself a
   semantic+source change deserving its own phase (NOT a Phase 3 mechanical syntax slice).
   This is feature-flag territory, not idiom-rewrite.

Out of scope for Phase 3.

### Candidate 3.8 — Custom `commons.misc.ValueOf` → `scala.ValueOf`

**Fork commit:** `3ec8c125` — `chore(scala-3): SimpleRawRef codec via derived + SealedUtils uses scala.ValueOf` (2 files, 4 lines).

**Live inventory in our tree:**
```bash
git grep -nE 'commons\.misc\.ValueOf' -- 'core/src/main/scala' 'mongo' 2>/dev/null | wc -l
# ~10 hits in HasGenCodec.scala and ValueOf.scala companion
```

Our `core/src/main/scala/com/avsystem/commons/misc/ValueOf.scala` is already marked
`@deprecated("Use scala.ValueOf[T] from the standard library …", "2.28.0")` (upstream
`de920b2a` and `8b0f6ea2` did the master-side deprecation pre-pivot — confirmed via
`git log --all -- core/src/main/scala/com/avsystem/commons/misc/ValueOf.scala`).

**Recommendation:** **DEFER to Phase 4+ feature work.** Reasons:
1. Touches HasGenCodec — depends on macro/derivation paths that are Phase-2-stubbed.
2. Not pure syntax; involves dropping a class.
3. Already deprecated; replace at leisure during feature port.

NOT a Phase 3 slice.

### Candidate 3.9 — Named tuples for `MacroInstances` + match type for `IDOf`

**Fork commit:** `cc59e249` — `fix(scala-3,mongo): MacroInstances→named-tuple, IDOf match type, optionalizeFirstArg overload resolution` (8 files, +66/-90 lines).

**Live inventory in our tree:** N/A — `MacroInstances` and `IDOf` are macro-derivation-adjacent
APIs heavily stubbed by Phase 2 (`MongoEntityCompanion.ID` widened, `MacroInstances` partial).

**Recommendation:** **DEFER to Phase 4+ (feature port).** Named tuples and match types are
**semantic** changes (new Scala 3 features replacing typeclass-dispatch shapes), not mechanical
syntax. Belongs to the macro/typeclass restoration phase, not Phase 3.

NOT a Phase 3 slice.

### Candidate 3.10 — `Flags.HasDefault` over name-matching default methods

**Fork commit:** `fab331ad` — `refactor(scala-3): use Flags.HasDefault instead of name-matching default methods` (1 file, +2/-7 lines).

**Live inventory in our tree:**
```bash
git grep -nE 'Flags\.HasDefault' -- 'core/src/main/scala'
# 0 hits
```

The relevant file (`SerializationMacros.scala`) is Phase-2-stubbed. The improvement assumes
the macro is restored.

**Recommendation:** **DEFER to Phase 4+ feature port.** Apply when restoring `SerializationMacros`
in the GenCodec derivation phase. Trivially small (1 line), can be folded into that PR.

NOT a Phase 3 slice.

### Candidate 3.11 — `export Companion.given` re-export pattern

**Fork commit:** Embedded in `8f70be80` (BsonGenCodecs) and used throughout.

**Live inventory in our tree:**
```bash
git grep -nE '^\s*export .*\.given' -- 'core' 'mongo'
# 0 hits
```

**Status:** This pattern emerges *as a consequence of* slice 3.3's `implicit def` →
`given` + `export X.given` rewrite. Already covered by slice 3.3 (BsonGenCodecs example in
existing research).

**Recommendation:** **ABSORBED into slice 3.3.** No separate slice.

### Candidate 3.12 — `Symbol.newClass` patterns (RPC scaffolding)

**Fork commits:** `1706cbfa`, `070a0e38` — `feat(scala-3,rpc): … via Symbol.newClass`.

**Live inventory in our tree:** N/A — RPC module deprecated/disabled in Phase 1. `Symbol.newClass`
usage exclusively inside fork's RPC scala-3 port, which we are NOT restoring at all (per
project memory: RPC deprecated, fork commit `7a84a2c2` marks all RPC API as `@deprecated`).

**Recommendation:** **DROP — out of project scope.** RPC is dead-ended; no slice.

### Candidate 3.13 — `made.*` re-export (annotations under `commons.serialization`)

**Fork commit:** `b4af735e` — `feat(scala-3): re-export made annotations under commons.serialization`.

**Live inventory in our tree:** Phase 1 already integrated `made.*` (per project memory:
"`made` integration already on branch"). Verify via `git grep -nE 'export made\.' -- 'core/src/main/scala'`
during planning.

**Recommendation:** **DEFER to feature-port phase.** This is API surface design (re-export
ergonomics), not idiom rewrite. Belongs with the `made` integration polish, not Phase 3.

### Candidate 3.14 — `@nowarn` on F-bound witness sites

**Fork commit:** `abb7fb37` — `test(scala-3): cleanup codec tests + F-bound nowarn witness`.

**Recommendation:** **DROP — violates `[[feedback-fix-dont-suppress-warnings]]`.** User-locked:
"no `@nowarn`/`-Wconf` added". If F-bound warnings appear during feature port, fix at source,
not via suppression. Not a Phase 3 slice.

### Candidate 3.15 — `scalafmt` significant-indentation enablement

**Fork commit:** `1ceab33a` — `style(scalafmt): enable significant-indentation, format more files`.

**Status:** Already covered by existing CONTEXT.md `<deferred>` section — explicitly deferred
to "optional Phase 3.4" (which we've now repurposed for `@inline → inline`). True
significant-indentation enablement is a **separate optional Phase 3.5** if appetite arises.

**Recommendation:** **DEFER to optional Phase 3.5+** (renumbered, since 3.4 is now `@inline`).
NOT in current Phase 3 scope.

### Candidate 3.16 — Drop local `TupleOps` in favor of `made.*` tuple helpers

**Fork commit:** `25d34852` — `chore(scala-3): drop local TupleOps in favor of made.* tuple helpers`.

**Recommendation:** **DEFER to feature-port phase** (tuple derivation work). Not a mechanical
syntax sweep; involves removing local code and rewiring callers to `made` API. NOT Phase 3.

### Candidate 3.17 — `import` reorganization / drop scala-2 macro imports

Across fork commits (`39c047eb`, `eef0edce`, `848b8e9e`), import statements are pruned of
references to deleted modules (`commons-macros`, RPC). Our Phase 1 already deleted these
modules.

**Recommendation:** **ABSORBED into slices 3.1 / 3.3** as part of the per-file copy-and-reconcile
step ("prune imports referencing dropped modules" — already in CONTEXT.md §"Preferred method").
No separate slice.

### Candidate 3.18 — `implicitly[T]` → `summon[T]`

**Live inventory in our tree:**
```bash
git grep -nE '\bimplicitly\[' -- 'core/src/main/scala' 'mongo' 'hocon' 'benchmark'
# 0 hits
```

**Status:** Already zero in our tree. Either upstream Scala 2 master never used `implicitly[T]`,
or it was cleaned by Phase 1/2. Per project memory `[[feedback-deprecate-over-restore]]`,
`summon[T]` is preferred but no migration needed if no `implicitly[T]` exists.

**Recommendation:** **DROP — non-applicable.** No slice needed.

### Candidate 3.19 — `implicit object X extends Y` → `given X: Y with { … }`

**Status:** Pattern explicitly covered in slice 3.3's "Key fork patterns" list (already in
research).

**Recommendation:** **ABSORBED into slice 3.3.** No separate slice.

### Summary table — sweep verdict

| ID | Pattern | Verdict | Reason |
|----|---------|---------|--------|
| 3.5 | `compiletime.defered` typo fix | DROP | 0 hits in our tree |
| 3.6 | `implicit def` Conversion givens | ABSORBED in 3.3 | already mapped |
| 3.7 | Explicit-nulls patches | DEFER Phase 4+ | needs `-Yexplicit-nulls` enable, not pure syntax |
| 3.8 | `commons.misc.ValueOf` → `scala.ValueOf` | DEFER Phase 4+ | feature/dep work, depends on macros |
| 3.9 | Named tuples + match types | DEFER Phase 4+ | semantic, macro-port territory |
| 3.10 | `Flags.HasDefault` | DEFER Phase 4+ | macro restoration |
| 3.11 | `export X.given` | ABSORBED in 3.3 | natural consequence of given sweep |
| 3.12 | `Symbol.newClass` (RPC) | DROP | RPC dead-ended |
| 3.13 | `made.*` re-export | DEFER feature-port | API surface design |
| 3.14 | `@nowarn` F-bound witness | DROP | violates project rule |
| 3.15 | significant-indentation | DEFER Phase 3.5+ | optional polish |
| 3.16 | drop local TupleOps | DEFER feature-port | not mechanical |
| 3.17 | import pruning (dropped modules) | ABSORBED in 3.1/3.3 | per CONTEXT reconcile step |
| 3.18 | `implicitly[T]` → `summon[T]` | DROP | 0 hits |
| 3.19 | `implicit object` → `given … with` | ABSORBED in 3.3 | already mapped |

**Net new slices identified by sweep:** ZERO (all candidates either absorbed, deferred, or
dropped). The four-slice plan (3.1 → 3.2 → 3.3 → 3.4) remains the complete Phase 3 scope.

**Confidence:** HIGH for inventory accuracy (each candidate verified via `git grep` against
live tree and `git show` against fork commits). MEDIUM for "feature port" deferrals (exact
phase number TBD by ROADMAP).

---

## Validation Architecture — Slice 3.4 additions

**Appended:** 2026-06-01

Extends existing Validation Architecture section with slice 3.4 gates.

### Slice 3.4 → Test Map (extension to existing table)

| Slice | Behavior | Test Type | Automated Command | File Exists? |
|-------|----------|-----------|-------------------|-------------|
| 3.4 | `git grep '@inline' core/src/main/scala/` → 0 hits OUTSIDE whitelist | grep gate (whitelisted) | `git grep -nE '@inline' -- 'core/src/main/scala' \| grep -vE '(CborInput\|JsonStringInput\|RPCFramework)\.scala:' \| wc -l` → expect 0 | ✅ (built-in) |
| 3.4 | Whitelist preserved verbatim (8 hits across 3 files) | grep parity | `[ "$(git grep -cE '@inline' -- 'core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala' 'core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala' 'core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala' \| awk -F: '{s+=$NF}END{print s}')" = "8" ]` | ✅ (built-in) |
| 3.4 | SharedExtensions inline-def count matches fork | parity diff | `[ "$(git grep -cE '\binline (def\|val)\b' -- 'core/src/main/scala/com/avsystem/commons/SharedExtensions.scala')" = "$(git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala \| grep -cE '\binline (def\|val)\b')" ]` | ✅ (built-in) |
| 3.4 | No `@inline val` introduced | grep gate | `[ "$(git grep -cE '@inline\s+(private\s+)?val\b' -- 'core' 'mongo' 'hocon' 'benchmark')" = "0" ]` | ✅ (built-in) |
| 3.4 | Compile + tests still green | smoke (compile) | `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` | ✅ |
| 3.4 | No new `@nowarn` introduced | grep gate | (same as other slices — already in master table) | ✅ |

### Sampling Rate (slice 3.4 specifics)

- **Per file copy:** `sbt 'commons-jvm/compile'` after each Opt-family + SharedExtensions + jiop
  file is reconciled. Inline rewrites can break call sites that previously took non-inline
  functions; per-file compile catches this early.
- **Per slice commit:** Full suite as for other slices.
- **Phase 3 gate (post-merge):** All four slice gates simultaneously green on `scala-3` tip.

### Wave 0 Gaps for slice 3.4

None. Same justification as existing Wave 0 Gaps section — no test files needed; pure syntax
rewrite with semantic preservation; existing test compile is sufficient.
