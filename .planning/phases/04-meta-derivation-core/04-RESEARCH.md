# Phase 4: meta/ derivation core — Research

**Researched:** 2026-06-01
**Domain:** Scala 3 inline + macro-quote derivation infrastructure (`com.avsystem.commons.meta`)
**Confidence:** HIGH for stack/structure, MEDIUM for `MetaMacros.lazyMetadataImpl` real body (fork ships a `???` placeholder), LOW for `MetaMacros.valueImpl` final body (also fork placeholder).

## Summary

The fork's `meta/` layer is **much smaller than expected** because the fork itself shipped most of the heavy macros as `???`-bodied placeholders in `MetaMacros`. The real Scala 3 work in Phase 4 is therefore:

1. **Verbatim structural ports** — `AllowDerivation`, `Fallback`, `OptionLike` (small reconcile), `metadata.scala` (pure ADT, no macros), `metaAnnotations.scala` (annotation hierarchy + `object infer extends InferMacros`), `AdtMetadataCompanion`, `MetadataCompanion`. Mechanical; small diffs.
2. **One real inline derivation:** `MacroInstances.materialize` — Scala 3 `inline given` + `compiletime.erasedValue`/`summonInline` over `NamedTuple.DropNames`. ~12 LOC, but architecturally pivotal (consumed by every codec/RPC companion downstream).
3. **`MetaMacros` quote scaffolding** — splice methods (`valueImpl`, `lazyMetadataImpl`, `dummy`) that fork ships as `'{ ??? }` — port the **scaffolding** to unblock callers (Phase 6/7), defer the real bodies. This is **explicit fork-shipped technical debt**, not our regression.
4. **Reconcile divergences** — our `OptionLike` carries `BaseOptionLike` + `@bincompat` shim that fork dropped; our `AdtMetadataCompanion` is currently `M[_]` while fork upgrades to `M[X] <: TypedMetadata[X]`. Both flips are public-API impacting.

**Primary recommendation:** Five-slice stacked PR series with sequential merge — the meta layer is small but tightly coupled and porting it in one shot makes review unmanageable. Start with leaves (`Fallback`/`AllowDerivation`/`OptionLike`/`metadata`), then `MetaMacros` scaffolding, then `MacroInstances`, then `MetadataCompanion`/`AdtMetadataCompanion`, then `metaAnnotations` (infer.value).

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

- **Translation method:** crib from `origin/master` fork files at `core/src/main/scala-3/com/avsystem/commons/meta/`. Method: `git show origin/master:core/src/main/scala-3/<path>` → copy to `core/src/main/scala/<path>`. Per [[feedback_crib_from_master]].
- **Slice strategy:** ~3-5 small PRs per [[feedback_small_scoped_prs]]. Fork file cluster groups:
  - Slice 4.1: Foundation — `AllowDerivation`, `Fallback`, `OptionLike` (reconcile), `metadata.scala`
  - Slice 4.2: `MacroInstances` materialization
  - Slice 4.3: `MetaMacros` + `MetadataCompanion.lazyMetadata`
  - Slice 4.4: `AdtMetadataCompanion` + `Bounded*`
  - Slice 4.5: `metaAnnotations.value`
- **`made.*` usage:** accept; current branch already pins `madeVersion = "0.1.1"` in `project/Commons.scala`. `metadata.scala` imports `made.*` + `made.annotation.*` (fork code).
- **Macro impl style:** Scala 3 `inline given` + `compiletime.summonInline`, no `made.*` wrapping at this layer.
- **Test coverage:** un-wrap `/* */` per slice.
- **`Implicits.infer` references:** none remaining (slice 3.5 deleted).

### Claude's Discretion

- Exact slice boundaries (4.1–4.5 above are starting point — refine per fork-commit granularity).
- Stack PRs vs each off `upstream/scala-3`. **Recommendation below: STACK.** Meta has tight internal coupling.
- Test re-enable batching (one per slice vs all at end). **Recommendation: per-slice.**
- `MetaMacros` private helper visibility.

### Deferred Ideas (OUT OF SCOPE)

- `GenCodec.materialize` (Phase 6)
- `ApplyUnapplyCodec.materialize` (Phase 6)
- `MongoEntityCompanion.materialize` (Phase 9)
- RPC `AsRaw`/`AsReal` materialization (Phase 7)
- `forSealedEnum` macro (Phase 6)
- `made` library version bump (phase-independent)
- `analyzer` module re-enable
- Cross-build re-introduction (never, per [[project_scala3_only_pivot]])
</user_constraints>

<phase_requirements>
## Phase Requirements

The phase predates the v2 requirements numbering scheme — REQUIREMENTS.md only covers Phase 1. Phase 4 work is governed by:

| ID | Description | Research Support |
|----|-------------|-----------------|
| META-CORE-01 *(new — to be added in REQUIREMENTS.md)* | Port `MacroInstances.materialize` to Scala 3 inline + named-tuple derivation | §"Fork File Inventory" + §"Code Examples (verbatim from fork)" |
| META-CORE-02 *(new)* | Replace `???` stubs in `AdtMetadataCompanion`, `MetadataCompanion`, `MacroInstances`, `metaAnnotations.infer.value` with fork's inline+splice impls | §"Per-Slice Recommendations" + §"Fork File Dependency Graph" |
| META-CORE-03 *(new)* | Add new fork file `AllowDerivation.scala` (does not exist in our tree) | §"Missing Files in Our Tree" |
| META-CORE-04 *(new)* | Add new fork file `MetaMacros.scala` (does not exist in our tree) | §"Missing Files in Our Tree" |
| META-CORE-05 *(new)* | Reconcile `metadata.scala` import block (`made.*`, `made.annotation.*`, `GenCodec.given`) | §"`made.*` Integration" |
| META-CORE-06 *(new)* | Reconcile `OptionLike.scala` — drop `BaseOptionLike` `@bincompat` shim or preserve per binary compat note | §"Divergences From Fork" |
| META-CORE-07 *(new)* | Un-wrap meta-dependent tests as each slice lands | §"Test Un-wrapping Plan" |
| QUALITY-01 / PR-01..03 / WORKFLOW-01..05 | (cross-cutting carry from Phase 1) | Existing |
| DOC-02 (MIGRATION.md update per PR) | Per [[feedback_migration_md_contract]] | Existing |
</phase_requirements>

## Fork File Inventory

All 9 fork files at `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/` examined verbatim. Sizes (approx LOC, blank/comments stripped where indicated):

| Fork file | Size | Status in our tree | Has macro quotes? | Imports `made.*`? |
|-----------|------|-------------------|--------------------|-------------------|
| `AllowDerivation.scala` | 7 LOC | **MISSING** | No | No |
| `Fallback.scala` | 1 case class | EXISTS (identical body) | No | No |
| `MacroInstances.scala` | 30 LOC, 1 inline given + 1 transparent inline | EXISTS (Phase-1 stub) | No (pure inline) | No |
| `MetaMacros.scala` | 46 LOC, 5 traits + 3 `???`-quote impls | **MISSING** | **Yes** (`scala.quoted.*`) | No |
| `MetadataCompanion.scala` | 36 LOC, 2 traits | EXISTS (Phase-1 stub) | No (delegates to `MetadataCompanionLazyMacros` in `MetaMacros`) | No |
| `AdtMetadataCompanion.scala` | 4 LOC (just trait stitching) | EXISTS (Phase-1 stub, different shape) | No | No |
| `OptionLike.scala` | 50 LOC | EXISTS (richer — `BaseOptionLike` + bincompat shim) | No | Yes (1 line: `given … => made.Default[O]`) |
| `metaAnnotations.scala` | ~330 LOC, ~30 annot classes | EXISTS (mostly identical, `infer.value` stubbed `???`) | No (delegates to `InferMacros` in `MetaMacros`) | No |
| `metadata.scala` | 235 LOC, pure ADT (`ParamFlags`/`MethodFlags`/`TypeFlags`/`ParamPosition`/`MethodPosition`/`DefaultValue`/`SourceOffset`/`SymbolSource`) | EXISTS | No | **Yes** (`import made.*; import made.annotation.*; import GenCodec.given`) |

### Missing Files in Our Tree

1. **`AllowDerivation.scala`** — 7 LOC marker, depended on by `MacroInstances.materializeInstances`. Trivial port; **must land in or before slice 4.2 (MacroInstances)**.
2. **`MetaMacros.scala`** — 46 LOC. Houses every `scala.quoted` splice landing. Defines six traits that the other meta files **extend**:
   - `InferMacros` (consumed by `object infer extends InferMacros`)
   - `AdtMetadataCompanionMacros[M[_]]`
   - `BoundedAdtMetadataCompanionMacros[Hi, Lo, M]`
   - `MetadataCompanionMacros[M[_]]` (defined but currently unused by `MetadataCompanion` proper)
   - `BoundedMetadataCompanionMacros[Hi, Lo, M]`
   - `MetadataCompanionLazyMacros[M, Lazy]` (consumed by `MetadataCompanion.Lazy`)
   - `BoundedMetadataCompanionLazyMacros[Hi, Lo, M, Lazy]`
   - Companion `object MetaMacros { def valueImpl, lazyMetadataImpl, dummy }`.

   **All three impl bodies are `'{ ??? }` placeholders in fork.** This is fork-shipped technical debt — the **scaffolding** is real (signatures, splice plumbing, `inline def`/`inline given` patterns) but the *generation logic* is unwritten upstream. This means slice 4.3 ports the scaffolding only; the actual `lazyMetadataImpl` reflection-based body lands in a later phase (or via separate spike).

## Fork File Dependency Graph

Topological order (leaves first, top of stack last):

```
Layer 0 (leaves — zero meta-internal deps):
  - Fallback.scala
  - AllowDerivation.scala
  - OptionLike.scala  (depends only on Opt/OptRef/OptArg/NOpt from misc — already ported)

Layer 1 (depends on Layer 0):
  - metadata.scala  (depends on serialization.{GenCodec, HasGenCodec, GenCodec.given} + made.*)
                    !! has out-of-meta deps; see notes
  - metaAnnotations.scala  (depends on InferMacros from MetaMacros → must land AFTER MetaMacros)

Layer 2 (depends on Layer 0–1):
  - MetaMacros.scala  (depends on nothing within meta; provides traits consumed by L3)
  - MacroInstances.scala  (depends on AllowDerivation + scala.NamedTuple)

Layer 3 (depends on Layer 2):
  - MetadataCompanion.scala  (depends on Fallback, MetaMacros.MetadataCompanionLazyMacros, ImplicitNotFound)
  - AdtMetadataCompanion.scala  (depends on MetadataCompanion + MetaMacros.AdtMetadataCompanionMacros + TypedMetadata)
```

**Critical cross-meta edges:**
- `metaAnnotations.scala::object infer extends InferMacros` → must land **after** `MetaMacros` (or simultaneously) because `InferMacros` lives in `MetaMacros.scala`.
- `AdtMetadataCompanion[M[X] <: TypedMetadata[X]]` references `TypedMetadata` from `metadata.scala`.
- `MetadataCompanion.Lazy.object Lazy extends MetadataCompanionLazyMacros[M, Lazy]` references `MetaMacros`.
- `MacroInstances.materializeInstances` writes `given AllowDerivation[h] = AllowDerivation.create` — hard dep on `AllowDerivation`.

**Out-of-meta deps `metadata.scala` introduces:**
- `import made.*` (Mirror/Default machinery)
- `import made.annotation.*` (`@transparent`, `@name`)
- `import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}`
- `import com.avsystem.commons.serialization.GenCodec.given`

The `HasGenCodec[X]` companion-base pattern is **already used by our serialization layer** (existing `HasGenCodec` ports are live per STATE Plan 03 line). So this dep is internal-and-green. ⚠️ `GenCodec.given` import may not resolve if `GenCodec` is still macroless-stubbed — verify before slice 4.1 lands.

## Per-Slice Recommendations

**Recommendation: STACK** the 5 slices linearly. Reasoning: tight internal coupling (Layer 2 → Layer 3 dep is unavoidable; `metaAnnotations.infer` extends `InferMacros` from `MetaMacros`). Stacking lets each slice review against the previous and avoids n-way merge conflicts on the same files.

### Slice 4.1 — Foundation (leaves) [SMALL]

**Branch base:** `upstream/scala-3` tip (fresh, not stacked).
**Branch name:** `04-01-meta-foundation`.
**Fork files ported:**
- `AllowDerivation.scala` (new file — copy verbatim from `origin/master`)
- `Fallback.scala` (already identical — likely no-op; verify and skip if so)
- `OptionLike.scala` (reconcile — see Divergences §)
- `metadata.scala` (verbatim; **gate on `GenCodec.given` resolving** — if `GenCodec` still doesn't export givens, demote `metadata.scala` to slice 4.4 or stub the `HasGenCodec[X]` companions to manual `GenCodec.materialize` calls)

**Scala 3 features used:** `case class … extends AnyVal`, `@transparent` (made annotation, not Scala 3), `given … extends`, `given [A] => OptionLike.Aux[…]` (context-function-style given).

**Tests un-wrapped:** None in this slice — leaves have no test counterparts that were Phase-1-wrapped (`AdtTaggingTest.scala` is **already live** — not wrapped — depends only on `metaAnnotations` types that compile fine in Phase 1 stub form).

**Pitfalls:**
- `OptionLike` divergence (our `BaseOptionLike` extends `OptionLike` with `type Value = A`; fork inlines this directly into `OptionLikeImpl`). Dropping `BaseOptionLike` is **public API**. → preserve our shape, add fork's `given … => made.Default[O]` line.
- `metadata.scala` has fork-only `@name("dupa")` on `ParamFlags.rawFlags` — looks like a fork debug artifact. **Strip during port.** (HIGH confidence — clearly wrong, `dupa` is Polish slang.)
- `@transparent` is `made.annotation.transparent` — verify `made 0.1.1` exports it (cellar check before commit).

**MIGRATION.md updates:** Strike 0 backlog rows (foundation files have no backlog entries except `Fallback`-related, which has none).

### Slice 4.2 — MacroInstances inline derivation [SMALL]

**Branch base:** `04-01-meta-foundation` (stacked).
**Branch name:** `04-02-macro-instances`.
**Fork files ported:**
- `MacroInstances.scala` — full rewrite from fork verbatim. Drop our Phase-1 `???` body.

**Scala 3 features used:** `inline given`, `transparent inline def`, `compiletime.erasedValue`, `compiletime.summonInline`, `scala.NamedTuple.{AnyNamedTuple, DropNames}`, `import implicits.given` (context import from singleton parameter), polymorphic match-types pattern `_: (h *: t)`.

**Scala 3.8.2 baseline support verification:**
- `inline given`: stable since 3.0. ✅
- `transparent inline`: stable since 3.0. ✅
- `compiletime.{erasedValue, summonInline}`: stable since 3.0. ✅
- `scala.NamedTuple`: **experimental in 3.3, stable in 3.6+, in stdlib in 3.7+**. Scala 3.8.2 ships them stable. ✅ MEDIUM confidence — verify `scala.NamedTuple.{AnyNamedTuple, DropNames}` are not gated behind `language.experimental.namedTuples` in 3.8.2 by running a probe (`scalac -release 3.8 -e 'val x: scala.NamedTuple.AnyNamedTuple = ???'` during Wave-0).
- `import implicits.given`: stable; same shape as `import obj.given` for selective wildcard given imports.

**Tests un-wrapped:**
- `core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala` — full file `/* */`-wrapped (line 5 onwards). **Gate on slice 4.4** (test exercises `AdtMetadataCompanion.materialize` for `Dep`, `Klass[T]` — needs the full stack). Keep wrapped in 4.2; un-wrap in 4.4 or 4.5.

**Pitfalls:**
- `materializeInstances[T <: Tuple]` recursive inline match — when `Instances` has many fields, can trip `-Xmax-inlines` (default 32 in Scala 3.4+, raised to 1000 in 3.6+). Verify our scalacOptions don't pin a low limit. If a downstream consumer hits the limit, raise via `-Xmax-inlines:64` (not via inline-suppression).
- `compiletime.summonInline[h]` produces a **deferred** error message; the original macro had richer not-found messages. Document this regression in MIGRATION.md §3.
- `(implicits, companion) => …` lambda **captures** `implicits` and uses `import implicits.given`; the captured value is a runtime value. Verify the fork's pattern actually allows `import` of a runtime-value path (per Scala 3 spec: `import path.given` requires `path` to be a stable value reference — `implicits` is a function param, which IS a stable identifier inside the lambda body). ✅ HIGH confidence per fork being live and downstream consumers (Phase 6 GenCodec on fork) working.

**MIGRATION.md updates:** Strike backlog row `core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala:47`. Add §3 entry: "MacroInstances.materialize is now an `inline given` — call sites need no change but error messages from failed implicit search are now Scala-3 standard (not the legacy detailed-trace)."

### Slice 4.3 — MetaMacros scaffolding + MetadataCompanion [SMALL]

**Branch base:** `04-02-macro-instances` (stacked).
**Branch name:** `04-03-meta-macros-scaffolding`.
**Fork files ported:**
- `MetaMacros.scala` (new file — verbatim)
- `MetadataCompanion.scala` (rewrite to fork shape: `given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real]`, `Lazy` companion extends `MetadataCompanionLazyMacros[M, Lazy]`)
- `BoundedMetadataCompanion` reshape (same — `Lazy` extends `BoundedMetadataCompanionLazyMacros`)

**Scala 3 features used:**
- `inline def materialize[T]: M[T] = ${ MetaMacros.dummy }` — `inline def` + `${ … }` macro splice. Stable. ✅
- `inline given lazyMetadata: [Real] => (metadata: M[Real]) => Lazy[Real] = ${ MetaMacros.lazyMetadataImpl }` — **inline-given with polymorphic context function type** + macro splice as RHS. ✅ 3.4+ syntax.
- `given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value` — same polymorphic context function pattern; same precedent as slice 3.3 anonymous-given pattern locked in STATE.

**Tests un-wrapped:** None (`MetaMacros.dummy` returns `'{ ??? }` → call sites compile but throw at runtime — no test should exercise this until slice 4.5 or later).

**Pitfalls:**
- **Fork ships `'{ ??? }` placeholders** in `valueImpl`, `lazyMetadataImpl`, `dummy`. **This is fork-shipped technical debt, not regression on our part.** Document explicitly in MIGRATION.md §1 (Will not migrate THIS PHASE — see Open Questions below): "MetaMacros.lazyMetadataImpl real body deferred — fork ships `???`; downstream consumers (GenCodec) will hit it at runtime."
- `def lazyMetadataImpl(using Quotes): Expr[Nothing] = '{ ??? }` — return type is `Expr[Nothing]` but spliced into context `Lazy[Real]`. Scala 3 allows `Nothing <:< T` in expressions, but the splice will compile only because `??? : Nothing` widens. Document.
- `def valueImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]` — fork uses `asInstanceOf` on `Expr` to bypass type check. **Smell** but verbatim port. Don't "fix" it.

**MIGRATION.md updates:** Strike `MetadataCompanion.scala:27` and `:58` backlog rows. Add §1 entry: "MetaMacros real bodies (lazyMetadataImpl, valueImpl, dummy) deferred — port scaffolding only per upstream fork state."

### Slice 4.4 — AdtMetadataCompanion bound shift [SMALL]

**Branch base:** `04-03-meta-macros-scaffolding` (stacked).
**Branch name:** `04-04-adt-metadata-companion`.
**Fork files ported:**
- `AdtMetadataCompanion.scala` — collapse our 4-method stub to fork's 2-line trait stitching: `trait AdtMetadataCompanion[M[X] <: TypedMetadata[X]] extends AdtMetadataCompanionMacros[M] with MetadataCompanion[M]` and similar for `Bounded`.

**Scala 3 features used:** Plain trait composition + inherited `inline def materialize` from `AdtMetadataCompanionMacros[M]` (defined in `MetaMacros.scala` slice 4.3).

**Tests un-wrapped:**
- `core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala` — **selectively** un-wrap. The file has `???`-stubbed `HasGenCodecStructure[T]` plus tests that exercise `AdtMetadataCompanion.materialize` on case classes. Since `materialize` here calls `${ MetaMacros.dummy }` → `'{ ??? }` → RUNTIME `???` only when the test actually runs. **Compile-time tests** are revivable; **runtime tests** must be `pending`-marked.

**Pitfalls:**
- **`M[_]` → `M[X] <: TypedMetadata[X]` is a binary-compatible-narrowing** of the type-param bound — old callers with `M extends TypedMetadata` still work; old callers with `M` not bound by TypedMetadata will fail to compile. Per [[feedback_migration_md_contract]], document in MIGRATION.md §3 + §4 (bincompat).
- Trait composition `extends AdtMetadataCompanionMacros[M] with MetadataCompanion[M]` — verify no `inline given` clashes with `given fromFallback` from `MetadataCompanion`. Both are `given M[T]` but for different shapes; should be safe.

**MIGRATION.md updates:** Strike 4 `AdtMetadataCompanion.scala` backlog rows. Add §3 entry on the bound tightening.

### Slice 4.5 — metaAnnotations.infer.value [SMALL]

**Branch base:** `04-04-adt-metadata-companion` (stacked).
**Branch name:** `04-05-meta-annotations-infer`.
**Fork files ported:**
- `metaAnnotations.scala` — single surgical edit: `object infer { def value[T]: T = ??? }` → `object infer extends InferMacros`. Everything else stays.

**Tests un-wrapped:**
- `MacroInstancesTest.scala` — full file, **gated on runtime behaviour of `${ MetaMacros.dummy }`**. Since fork ships `???` at runtime, only **compile-time** assertions pass. Mark runtime-exercising tests `pending` per `feedback_fix_dont_suppress_warnings.md` (use ScalaTest's `pending` mechanism, NOT `@nowarn`).
- `AdtMetadataTest.scala` — finish un-wrapping any remaining `/* */` blocks.

**Pitfalls:**
- `inline def value[T]: T = ${ MetaMacros.valueImpl[T] }` body returns `'{ ??? }.asInstanceOf[Expr[T]]` → **call sites of `infer.value` will throw `NotImplementedError` at runtime**. This is **the documented annotation-default-value pattern** — `infer.value` is meant to be replaced by the macro that consumes the annotation; if the annotation is consumed by a non-macro path the runtime `???` fires correctly as an error signal. ✅ Intentional.

**MIGRATION.md updates:** Strike `metaAnnotations.scala:193` backlog row. Add §3 entry: "infer.value is now an inline macro call; runtime semantics preserve the 'use only inside macro-consumed annotations' contract."

## Scala 3 Feature Inventory

| Feature | Required by | Scala 3.8.2 status | Confidence |
|---------|-------------|----------------------|------------|
| `inline given` | MacroInstances, MetaMacros | stable since 3.0 | HIGH |
| `transparent inline def` | MacroInstances.materializeInstances | stable since 3.0 | HIGH |
| `compiletime.erasedValue` | MacroInstances | stable since 3.0 | HIGH |
| `compiletime.summonInline` | MacroInstances | stable since 3.0 | HIGH |
| `scala.quoted.*` (`Expr`, `Type`, `Quotes`, `'{ … }`) | MetaMacros | stable since 3.0 | HIGH |
| `scala.NamedTuple.AnyNamedTuple`, `DropNames` | MacroInstances | **stable in stdlib since 3.7** | MEDIUM — verify with sbt probe; may still be `experimental` in 3.6.x but 3.8.2 ships stable |
| Polymorphic context-function type givens `[Real] => (x: A) => B` | MetadataCompanion.fromFallback, lazyMetadata, BoundedMetadataCompanion.* | stable since 3.4 | HIGH |
| Trait composition `extends A[M] with B[M]` | AdtMetadataCompanion | classic Scala — never broke | HIGH |
| `match`-type-style inline match on `compiletime.erasedValue[T]` | MacroInstances.materializeInstances | stable since 3.0 | HIGH |
| `import path.given` from runtime parameter | MacroInstances.materialize lambda body | stable; documented in Scala 3 reference | HIGH |

**No experimental flags required.** No need to add `-language:experimental.namedTuples` (verify in probe).

## `made.*` Integration

Our branch already pins `madeVersion = "0.1.1"` in `project/Commons.scala:30`. Fork's `meta/` layer uses `made.*` in only TWO call sites:

| Site | Usage | Risk |
|------|-------|------|
| `metadata.scala::import made.*; import made.annotation.*` | Provides `@transparent`, `@name` annotations on `ParamFlags`/`MethodFlags`/`TypeFlags`/`MethodPosition`/`ParamPosition`/`SourceOffset`/`SymbolSource` | LOW — annotations consumed by `HasGenCodec` macro; same pattern as existing `metadata.scala` Phase-1 stubs (verify before slice 4.1: `cellar get-external io.github.halotukozak:made_3:0.1.1 made.annotation.transparent`) |
| `OptionLike.scala::given [O] => (optionLike: OptionLike[O]) => made.Default[O] = () => optionLike.none` | Bridges `OptionLike` to `made.Default` (used by `made`-driven derivation in `HasGenCodec`) | LOW — single line, isolated |

**No new `made.*` integration depth** introduced by Phase 4 — meta layer brushes `made.*` lightly; the heavy `made.*` consumption is downstream (GenCodec in Phase 6).

**Cellar pre-flight commands** before slice 4.1:
```sh
cellar get-external io.github.halotukozak:made_3:0.1.1 made.annotation.transparent
cellar get-external io.github.halotukozak:made_3:0.1.1 made.annotation.name
cellar get-external io.github.halotukozak:made_3:0.1.1 made.Default
```

## Test Un-wrapping Plan

Located via `find core -path '*test*' -name '*.scala' | xargs grep -l 'MacroInstances\|AdtMetadataCompanion\|MetadataCompanion\|TypedMetadata'`.

**Tests live under `core/src/test/scala/...` (NOT `core/jvm/...`).** Phase 1 STATE Plan 05 wrapped them with `/* */` per-file.

| Test file | Wrapped? | Un-wrap in slice | Compile-time only? |
|-----------|----------|-------------------|---------------------|
| `core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala` | YES (whole file post-imports) | 4.5 | Mixed — runtime assertions need `pending` |
| `core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala` | YES (whole file post-imports) | 4.4 or 4.5 | Mixed — most are runtime equality checks; `pending` until real `lazyMetadataImpl` lands |
| `core/src/test/scala/com/avsystem/commons/meta/AdtTaggingTest.scala` | **NO — already live** | (none) | — |
| `core/src/test/scala/com/avsystem/commons/rpc/GenericMetadataTest.scala` | TBD — out-of-scope (Phase 7) | Phase 7 | — |
| `core/src/test/scala/com/avsystem/commons/rpc/NewRpcMetadataTest.scala` | TBD — out-of-scope (Phase 7) | Phase 7 | — |
| `core/src/test/scala/com/avsystem/commons/rpc/ApiReflectionTest.scala` | TBD — out-of-scope (Phase 7) | Phase 7 | — |
| `core/src/test/scala/com/avsystem/commons/serialization/CodecTestData.scala` | data fixture; check per file | Phase 6 | — |

**Batching rule:** un-wrap a test file in the slice where the **last** dependency lands. For `AdtMetadataTest.scala` that's slice 4.4. For `MacroInstancesTest.scala` it depends on full stack — slice 4.5. **Mark runtime-exercising assertions `pending`** until the real `MetaMacros.lazyMetadataImpl` body lands in a future phase.

## Common Pitfalls

### Pitfall 1: `scala.NamedTuple` availability
**What goes wrong:** Importing `scala.NamedTuple.{AnyNamedTuple, DropNames}` on Scala < 3.7 requires `import scala.language.experimental.namedTuples`. If our scalacOptions/build inadvertently downgrades or our 3.8.2 build hasn't promoted them to stable, slice 4.2 will fail to compile.
**Why it happens:** Named tuples were experimental for several Scala 3 versions; stabilization in 3.7+.
**How to avoid:** Wave-0 probe: paste `import scala.NamedTuple.AnyNamedTuple; val x: AnyNamedTuple = ???` into a scratch source under `core/src/main/scala/`, run `sbt commons-core/compile`. If it fails with experimental-flag error, add `-language:experimental.namedTuples` to scalacOptions. **MEDIUM confidence Scala 3.8.2 has them stable.**
**Warning signs:** Compile error mentioning `experimental.namedTuples`.

### Pitfall 2: `${ MetaMacros.dummy }` at non-macro-context call sites
**What goes wrong:** Caller writes `val x: M[Foo] = AdtMetadataCompanion.materialize[Foo]` → compiles, but runtime evaluation hits `???` and throws `NotImplementedError`.
**Why it happens:** Fork ships placeholder bodies; we port them verbatim per [[feedback_crib_from_master]]. Phase 6 will replace the bodies with real reflection.
**How to avoid:** Document loudly in MIGRATION.md §1 ("real body deferred to Phase 6"). Mark all runtime tests `pending`. Do NOT add `@deprecated` shims; do NOT hide the throw behind `try`.
**Warning signs:** `NotImplementedError` thrown at runtime in any downstream consumer.

### Pitfall 3: `OptionLike` `BaseOptionLike` shim drop
**What goes wrong:** Our `OptionLike.scala` has `@bincompat sealed trait BaseOptionLike[O, A] extends OptionLike[O] { type Value = A }` + a secondary constructor on `OptionLikeImpl` for binary compat. Fork drops both. If we drop them silently we break downstream binaries that compiled against our stub.
**Why it happens:** Fork is independent — never had to maintain bincompat with our Phase 1 stub.
**How to avoid:** **PRESERVE `BaseOptionLike`** in slice 4.1 even though fork drops it. Add the fork's `given … => made.Default[O]` line. Document the divergence in MIGRATION.md §3/§4.
**Warning signs:** MiMa failures on `BaseOptionLike` in downstream PR CI (if we re-enabled MiMa — deferred per Phase 11).

### Pitfall 4: `@name("dupa")` debug artifact in fork `ParamFlags`
**What goes wrong:** Copying fork's `metadata.scala` verbatim brings in `@name("dupa") rawFlags: Int` — a Polish slang debug artifact. Pollutes serialized output ("dupa" instead of "rawFlags" in JSON/BSON).
**Why it happens:** Fork developer left debug annotation.
**How to avoid:** **Strip during port** — `@name("dupa")` → no annotation. Document in slice 4.1 commit message.
**Warning signs:** Serialization tests show key `"dupa"` instead of `"rawFlags"`.

### Pitfall 5: `-Xmax-inlines` ceiling on `materializeInstances` recursion
**What goes wrong:** Downstream `Instances` traits with many methods (each becoming a Tuple element) trigger inline-depth limit.
**Why it happens:** `materializeInstances` is recursive over Tuple shape; each method = one recursion level.
**How to avoid:** Check `scalacOptions` for `-Xmax-inlines:` — Scala 3.6+ default is 1000, ample. If a downstream consumer (Phase 6) hits the limit, raise via `-Xmax-inlines:2000` (HIGH if you suspect it).
**Warning signs:** "Maximal number of successive inlines (32) exceeded" error.

### Pitfall 6: `import implicits.given` inside lambda body
**What goes wrong:** Fork writes `MacroInstances[Implicits, Instances] { (implicits, companion) => import implicits.given; … }`. If Scala 3 path-checker rejects function-parameter `implicits` as not stable, compile fails.
**Why it happens:** Path-stability rules can be finicky in inline context.
**How to avoid:** Verbatim port from fork — fork is live and consumed downstream, so the path is stable. No workaround needed; just don't paraphrase.
**Warning signs:** "Not a stable identifier" error pointing at `import implicits.given`.

### Pitfall 7: `GenCodec.given` import in `metadata.scala`
**What goes wrong:** `import com.avsystem.commons.serialization.GenCodec.given` requires `GenCodec` to export givens. If our Phase 1 stub `GenCodec` is bodyless (`???`), no givens exist → import is empty (legal but silent) OR import fails (if no member named `given` exists in the stub).
**Why it happens:** Cross-package dep introduced by `metadata.scala`.
**How to avoid:** Pre-flight before slice 4.1: `git grep -n 'given ' core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala`. If no givens exist, defer `metadata.scala` to slice 4.4 (after MetaMacros lands) and use direct `HasGenCodec[ParamFlags]` macro call instead (which already works on fork per STATE).
**Warning signs:** "value given is not a member of object GenCodec" OR silent no-op import.

### Pitfall 8: `case class … extends AnyVal` + `final` + `@publicInBinary`
**What goes wrong:** Slice 3.4 established `@publicInBinary` for inline-body-referenced private members. `MacroInstances.materializeInstances` references `EmptyTuple`, `AllowDerivation.create` — both already public. No additional `@publicInBinary` should be needed, but verify during compile.
**How to avoid:** Check for "private member referenced from inline" warnings post-compile in slice 4.2.

## Divergences From Fork

| File | Our tree | Fork | Decision |
|------|----------|------|----------|
| `OptionLike.scala` | `BaseOptionLike` shim + `@bincompat` ctor on `OptionLikeImpl` + `class infer` does NOT extend `InferMacros` (object body is just `def value[T]: T = ???`) | Drops `BaseOptionLike`; uses `given … => OptionLike.Aux[…]`; `object infer extends InferMacros` | Keep our shim shape; fold fork's `given … => made.Default[O]`; preserve `implicit def` style for now OR convert to `given` consistent with slice 3.3. **Decision: convert to fork's `given` form for newly added methods, KEEP existing `implicit def optionOptionLike`/etc. as `given` (already converted in slice 3.3 if not, verify).** |
| `metadata.scala` | Identical-shape minus the `import made.*` block and `@transparent`/`@name` annotations? **Verify** — our file may already be the fork shape (Phase 1 stubs preserved Scala 3 file content). | Has `import made.*; import made.annotation.*; import GenCodec.given` + `@transparent` on ADT case classes + `@name("dupa")` debug artifact | **Strip `@name("dupa")`**; preserve rest verbatim. |
| `AdtMetadataCompanion.scala` | `M[_]` + 4 stub methods (`materialize`, `fromApplyUnapplyProvider` × 2) | `M[X] <: TypedMetadata[X]` + zero methods (delegates via trait composition) | Adopt fork shape — bound tightens to `<: TypedMetadata[X]`. Document bincompat. |
| `MetadataCompanion.scala` | `M[_]` + body methods (`apply`, `fromFallback`, `Lazy { lazyMetadata, notFound }`) all returning `???` | Polymorphic context-function givens + `Lazy extends MetadataCompanionLazyMacros[M, Lazy]` | Adopt fork shape. |
| `MacroInstances.scala` | `trait MacroInstances[Implicits, Instances]` plain trait + `implicit def materialize: M[I, I] = ???` | `sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple](applyImpl: …)` + `inline given materialize` | Adopt fork shape. **`Instances` upper-bounded to `AnyNamedTuple` is API-breaking** for downstream that passed non-NamedTuple type args. Document. |
| `metaAnnotations.scala` | `object infer { def value[T]: T = ??? }` | `object infer extends InferMacros` | Single-line swap in slice 4.5. |

## Code Examples (verbatim from fork)

### MacroInstances.materialize + inline derivation (slice 4.2)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala
package com.avsystem.commons
package meta

import scala.NamedTuple.{AnyNamedTuple, DropNames}

sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple](applyImpl: (Implicits, Any) => Instances) {
  def apply(implicits: Implicits, companion: Any): Instances = applyImpl(implicits, companion)
}

object MacroInstances {
  inline given materialize[Implicits, Instances <: AnyNamedTuple]: MacroInstances[Implicits, Instances] =
    MacroInstances[Implicits, Instances] { (implicits, companion) =>
      import implicits.given
      materializeInstances[DropNames[Instances]].asInstanceOf[Instances]
    }

  transparent inline def materializeInstances[T <: Tuple]: T = inline compiletime.erasedValue[T] match {
    case _: EmptyTuple => EmptyTuple.asInstanceOf[T]
    case _: (h *: t) =>
      given AllowDerivation[h] = AllowDerivation.create
      (compiletime.summonInline[h] *: materializeInstances[t]).asInstanceOf[T]
  }

  final class materializeWith(prefix: Any, materializer: String = "materialize") extends StaticAnnotation
}
```

### MetaMacros scaffolding (slice 4.3)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala
package com.avsystem.commons.meta

import scala.quoted.*

trait InferMacros {
  inline def value[T]: T = ${ MetaMacros.valueImpl[T] }
}

trait AdtMetadataCompanionMacros[M[_]] {
  inline def materialize[T]: M[T] = ${ MetaMacros.dummy }
  inline given [T] => M[T] = materialize[T]
  inline def fromApplyUnapplyProvider[T](inline applyUnapplyProvider: Any): M[T] =
    ${ MetaMacros.dummy }
}

// (BoundedAdtMetadataCompanionMacros, MetadataCompanionMacros, BoundedMetadataCompanionMacros,
//  MetadataCompanionLazyMacros, BoundedMetadataCompanionLazyMacros — see fork file)

object MetaMacros {
  def valueImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def lazyMetadataImpl(using Quotes): Expr[Nothing] = '{ ??? }
  def dummy(using Quotes): Expr[Nothing] = '{ ??? }
}
```

### MetadataCompanion polymorphic-context-function givens (slice 4.3)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetadataCompanion.scala
trait MetadataCompanion[M[_]] {
  given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value
  final def apply[Real](using metadata: M[Real]): M[Real] = metadata
  final class Lazy[Real](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy extends MetadataCompanionLazyMacros[M, Lazy] {
    @implicitNotFound("#{forNotLazy}")
    given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
    def apply[Real](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)
  }
}
```

### AllowDerivation (slice 4.1)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AllowDerivation.scala
package com.avsystem.commons
package meta

sealed trait AllowDerivation[T]
object AllowDerivation {
  private val reusable = new AllowDerivation[Any] {}
  def create[T]: AllowDerivation[T] = reusable.asInstanceOf[AllowDerivation[T]]
}

object AllowRecursiveDerivation
```

### AdtMetadataCompanion bound-tightening (slice 4.4)
```scala
// Source: origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AdtMetadataCompanion.scala
trait AdtMetadataCompanion[M[X] <: TypedMetadata[X]]
  extends AdtMetadataCompanionMacros[M] with MetadataCompanion[M] {}

trait BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]]
  extends BoundedAdtMetadataCompanionMacros[Hi, Lo, M] with BoundedMetadataCompanion[Hi, Lo, M] {}
```

## State of the Art

| Old (Scala 2) | New (fork's Scala 3) | When changed | Impact |
|---------------|----------------------|--------------|--------|
| `whitebox.Context` + tree synthesis macros for `materialize` | `inline given` + `compiletime.erasedValue` + `summonInline` for shape derivation; `${ … }` macro splices for `lazyMetadata`/`value` reflection | Fork master, 2024-2025 commits | Error messages weaker; runtime impl ports landed gradually |
| `implicit def lazyMetadata[T](implicit m: M[T]): Lazy[T] = macro …` | `inline given lazyMetadata: [Real] => (metadata: M[Real]) => Lazy[Real] = ${ MetaMacros.lazyMetadataImpl }` | Same | Polymorphic context-function given is the modern idiom (slice 3.3 precedent in our tree) |
| `MacroInstances.materialize` materializing arbitrary `Instances` traits | `MacroInstances` constrained to `Instances <: AnyNamedTuple` | Same | Downstream `Instances` traits must be **named tuple aliases**, not classical traits. Phase 6 ports will need to reshape `XyzInstances`. **API break.** |

## Open Questions

1. **`MetaMacros.lazyMetadataImpl` real body — when does it land?**
   - What we know: fork ships `'{ ??? }` placeholder. Downstream `GenCodec.materialize` (Phase 6) needs the **real reflection-based body** that walks `Mirror.SumOf` / `Mirror.ProductOf` (or fork's equivalent via `made.*`).
   - What's unclear: whether fork's Phase 6 work introduces the real `lazyMetadataImpl` body or whether Phase 6 sidesteps `lazyMetadata` entirely by inlining all derivation through `made.Default`.
   - Recommendation: **defer real body to Phase 6 spike**; ship scaffolding only in Phase 4. Document loudly in MIGRATION.md §1 to set downstream expectations.

2. **`scala.NamedTuple.{AnyNamedTuple, DropNames}` stability on 3.8.2**
   - What we know: fork uses them un-flagged.
   - What's unclear: whether they're under `language.experimental.namedTuples` or stable in 3.8.2.
   - Recommendation: **Wave-0 probe** — drop a one-line `import scala.NamedTuple.AnyNamedTuple` into a scratch file, run `sbt commons-core/compile`. If it asks for the language flag, add it (cite Scala 3.8 release notes in commit msg).

3. **`OptionLike.BaseOptionLike` shim — drop or preserve?**
   - What we know: fork drops it. Our Phase 1 stub kept it (declared with `@bincompat`).
   - What's unclear: whether any external consumer (downstream library) references `BaseOptionLike` directly. Internal references in our tree: `OptionLikeImpl` extends it.
   - Recommendation: **preserve `BaseOptionLike`**, document divergence. Cost: ~6 LOC of additional type indirection; benefit: zero source-compat risk.

4. **`AdtMetadataCompanion[M[_]]` → `AdtMetadataCompanion[M[X] <: TypedMetadata[X]]` bound tightening**
   - What we know: this is API-breaking for downstream that built `M[_]`-shaped metadata classes not extending `TypedMetadata`.
   - What's unclear: does any AVSystem-internal consumer pass an `M` that doesn't extend `TypedMetadata`? Quick grep needed in Phase 6/7 prep.
   - Recommendation: **accept the break** (fork shipped it; our migration target IS the fork shape). Document in MIGRATION.md §3 with a "rationale: enables `inline given [T] => M[T] = materialize[T]` in `AdtMetadataCompanionMacros` which requires the bound" footnote.

5. **`MacroInstances[Implicits, Instances]` → `Instances <: AnyNamedTuple` API break**
   - What we know: fork's `MacroInstances` constrains `Instances` to `AnyNamedTuple`.
   - What's unclear: how many Phase-6/7 consumer call sites pass classical-trait `Instances` (vs named-tuple). Each one needs reshape.
   - Recommendation: **accept the break in Phase 4 RESEARCH** (foundation must match fork). Cost surfaces in Phase 6 ports — each `Instances` trait becomes a named-tuple type alias.

## Validation Architecture

Per CONTEXT, project has `.planning/config.json` — `nyquist_validation` setting not visible from this scope. Treating as enabled.

### Test Framework
| Property | Value |
|----------|-------|
| Framework | ScalaTest (`AnyFunSuite`) — per `core/src/test/scala/.../AdtTaggingTest.scala` import |
| Config file | sbt-managed (no explicit pytest-style config); see `Test / scalacOptions` block in `project/Commons.scala` |
| Quick run command | `sbt 'commons-core/testOnly *MacroInstancesTest *AdtMetadataTest *AdtTaggingTest'` |
| Full suite command | `sbt 'commons-core/test'` |
| Compile-only gate | `sbt 'commons-core/Test/compile'` (cheap, catches most Phase 4 regressions because most tests are macro-shape compile-time tests) |

### Phase Requirements → Test Map
| Req | Behavior | Test Type | Automated Command | File Exists? |
|-----|----------|-----------|-------------------|-------------|
| META-CORE-01 (MacroInstances.materialize) | `inline given` resolves for `Instances <: AnyNamedTuple` | compile-time | `sbt 'commons-core/Test/compile'` | ✅ MacroInstancesTest exists, wrapped |
| META-CORE-02 (stubs → fork impls) | All meta call sites compile after un-wrap | compile-time | `sbt 'commons-core/Test/compile'` | ✅ AdtMetadataTest exists, wrapped |
| META-CORE-03 (AllowDerivation) | Type appears in `MacroInstances.materializeInstances` expansion | compile-time | same | ❌ Wave 0 if we want a direct unit test (not required) |
| META-CORE-04 (MetaMacros scaffolding) | `${ MetaMacros.dummy }` splices compile | compile-time | `sbt 'commons-core/compile'` | ❌ Existing test files cover transitively |
| META-CORE-05 (`made.*` integration metadata.scala) | `HasGenCodec[ParamFlags]` etc. compile | compile-time | `sbt 'commons-core/compile'` | — |
| META-CORE-06 (OptionLike reconcile) | All `OptionLike` instances resolve | compile-time + runtime | `sbt 'commons-core/testOnly *OptionLike*'` | ⚠️ Check — may not exist as standalone test |
| META-CORE-07 (test un-wrap) | Un-wrapped tests pass (or are `pending`-marked) | runtime | `sbt 'commons-core/testOnly *MacroInstancesTest *AdtMetadataTest'` | ✅ |

### Sampling Rate
- **Per slice commit:** `sbt 'commons-core/Test/compile ;scalafmtCheckAll'` (~30 s warm)
- **Per slice push:** `sbt 'commons-core/test ;commons-core/Test/compile ;scalafmtCheckAll'` (~3–5 min)
- **Phase gate (before final slice PR opens):** `sbt 'commons-jvm/test ;commons-js/compile ;scalafmtCheckAll'`

### Wave 0 Gaps
- [ ] Scala 3.8.2 `scala.NamedTuple` stability probe — drop scratch file, compile, document.
- [ ] Verify `GenCodec.given` import resolves before slice 4.1 starts (or defer `metadata.scala` to a later slice).
- [ ] Cellar pre-flight: `cellar get-external io.github.halotukozak:made_3:0.1.1 made.annotation.transparent` + `made.annotation.name` + `made.Default`.
- [ ] No `pytest`-style standalone test config needed — sbt + ScalaTest sufficient.
- [ ] **No new test files to create** — un-wrap existing wrapped files per slice.

## Sources

### Primary (HIGH confidence)
- `git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/*.scala` — all 9 fork files read verbatim.
- `git log --oneline origin/master -- core/src/main/scala-3/com/avsystem/commons/meta/` — 5 commits surface; most recent `39c047eb` (eliminate implicit keyword + remove RPC) post-dates the meta file shape locks in `80f82c62`/`de303a17`.
- `/Users/bkozak/IdeaProjects/scala-commons3/core/src/main/scala/com/avsystem/commons/meta/*.scala` — all 7 our-tree files read.
- `/Users/bkozak/IdeaProjects/scala-commons3/MIGRATION.md` — backlog rows for meta files surveyed (8 rows: 4 `AdtMetadataCompanion`, 1 `MacroInstances`, 2 `MetadataCompanion`, 1 `metaAnnotations.scala:193`).
- `/Users/bkozak/IdeaProjects/scala-commons3/.planning/STATE.md` — slice 3.3 anonymous-given precedent; `madeVersion = 0.1.1` confirmed.

### Secondary (MEDIUM confidence)
- Scala 3 reference (docs.scala-lang.org) — for `inline given`, `compiletime.erasedValue`, `compiletime.summonInline`, `scala.quoted.*` patterns (training-data anchored).
- `scala.NamedTuple` stability in 3.7+/3.8.2 — MEDIUM until Wave-0 probe.

### Tertiary (LOW confidence)
- Bound-tightening API-break impact on AVSystem-internal consumers — depends on Phase 6/7 call-site survey not yet conducted.

## Metadata

**Confidence breakdown:**
- Fork file inventory + dependency DAG: HIGH — all 9 files read verbatim, deps grep-verified.
- Scala 3 feature support on 3.8.2: HIGH (all features except NamedTuple), MEDIUM (NamedTuple stability — needs Wave-0 probe).
- Slice boundaries (5 slices): HIGH — driven by Layer-N dep order; alternatives possible (e.g., merge 4.1+4.2 if Wave-0 probes green and `metadata.scala` GenCodec.given is clean).
- Common pitfalls (8 listed): HIGH for #1-#5 (mechanical observation), MEDIUM for #6-#8 (depend on downstream context).
- `made.*` integration: HIGH — only 2 touchpoints (1 import block, 1 `given` line).
- Test un-wrap plan: HIGH — file presence + wrap-state verified by `head`/`grep`.

**Research date:** 2026-06-01
**Valid until:** 2026-07-01 (30 days — fork meta shape stable since commit `80f82c62`; Scala 3.8.2 stable release).
