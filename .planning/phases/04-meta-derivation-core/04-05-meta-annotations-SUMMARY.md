---
phase: 04-meta-derivation-core
plan: 05
subsystem: meta-derivation
tags: [scala-3, meta, metaAnnotations, infer-macro, phase-4-closure, slice-4.5]
requires:
  - 04-03-meta-macros (InferMacros trait, MetaMacros.valueImpl placeholder)
  - 04-02-macro-instances (Instances <: AnyNamedTuple bound)
  - 04-04-adt-metadata-companion (M[X] <: TypedMetadata[X] bound)
provides:
  - "metaAnnotations.object infer extends InferMacros — inline def value[T] splice"
  - "MacroInstancesTest.scala un-wrapped (named-tuple Instances reshape)"
  - "Phase 4 closure — all 9 fork meta files ported"
affects:
  - All call sites of `infer.value` (now route through MetaMacros.valueImpl splice; runtime still ??? per fork)
tech-stack:
  added: []
  patterns: [inline-given-macro-splice, named-tuple-instances, surgical-edit]
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
    - core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala
    - MIGRATION.md
decisions:
  - "Surgical edit per fork: object infer { def value[T]: T = ??? } -> object infer extends InferMacros (1 file, 1 hunk, 4 lines diff)"
  - "MacroInstancesTest un-wrap diverges from fork (fork ships file as TodoScala3Migration DISABLED) — compile-check sections that survive Phase 4 bounds; wrap ComplexInstancesTest + AnnotationReferringToEnclosingObjectTest pending Phase 6"
  - "Phase 4 closure gate scope: commons-core/clean+compile+Test/compile+scalafmtCheckAll (mongo/hocon/core-js multi-module failures are PRE-EXISTING on base branch — slice 4.2 bound + JS made.* classpath gaps — out of Phase 4 scope per [[feedback_small_scoped_prs]])"
  - "PR NOT opened per user directive (push only — Task 4 PR steps in plan skipped)"
metrics:
  duration: "~10 minutes (compile + scalafmt + investigation cycles)"
  tasks: 4 (Task 4 reduced to push-only)
  files: 3
  commits: 3
  completed: 2026-06-02
---

# Phase 4 Plan 05: metaAnnotations Port + Phase 4 Closure Summary

Surgical swap landed `object infer extends InferMacros` in `metaAnnotations.scala` (wiring `infer.value` to the macro splice scaffolding from slice 4.3) and un-wrapped the `MacroInstancesTest.scala` named-tuple-compatible sections, closing the Phase 4 stack with all 9 fork meta files ported under documented divergences.

## What Shipped

- **Source port (Task 1):** `core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala` — single-hunk swap (4 lines removed, 1 added). Phase-1 stub `object infer { def value[T]: T = ??? }` → fork shape `object infer extends InferMacros`. `InferMacros` (slice 4.3 `MetaMacros.scala`) provides `inline def value[T]: T = ${ MetaMacros.valueImpl[T] }`; the splice body still ships `'{ ??? }` per fork (Phase 6 deferral), so runtime ??? in non-macro callers remains intentional.
- **Test un-wrap (Task 2):** `MacroInstancesTest.scala` — lifted Phase-1 `/* */` envelope. `MultipleImplicitImportsTest.HasGenCodecUsingAB` reshaped from `MacroInstances[..., () => GenCodec[T]]` to `MacroInstances[..., (codec: GenCodec[T])]` (named-tuple bound from slice 4.2); call site `.apply()` → `.codec`. `ComplexInstancesTest` + `AnnotationReferringToEnclosingObjectTest` stay wrapped (deferred to Phase 6 — see deviations).
- **MIGRATION.md (Task 3):** new §3 `core — meta metaAnnotations (slice 4.5)` block + new `core — meta` Phase 4 closure table covering all 9 fork meta files. Backlog rows removed: `metaAnnotations.scala:193` (resolved), `MacroInstances.scala:47` (resolved by slice 4.2 inline given); `MacroInstancesTest.scala:4` row split into two new rows for remaining wrapped sub-objects (lines 16 + 62).
- **Branch push (Task 4 reduced):** `origin/04-05-meta-annotations` set up. PR explicitly NOT opened per user directive. Three commits stacked on `04-04-adt-metadata-companion@20cf9fd0`.

## Commits

| Hash       | Type | Message                                                                 |
| ---------- | ---- | ----------------------------------------------------------------------- |
| `78c4fd79` | feat | port metaAnnotations infer macro (object infer extends InferMacros)     |
| `c5c416be` | test | un-wrap MacroInstancesTest (named-tuple Instances aliases)              |
| `f04cec6f` | docs | record metaAnnotations port + close Phase 4 stack                       |

## Acceptance Gates (all green within Phase 4 scope)

- `sbt commons-core/clean` exit 0
- `sbt commons-core/compile` exit 0
- `sbt commons-core/Test/compile` exit 0
- `sbt scalafmtCheckAll` exit 0
- `sbt 'commons-core/testOnly *MacroInstancesTest'` exit 0 (no test methods to run; compile-only check)
- `grep -c 'object infer extends InferMacros' core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala` → 1
- `grep -c 'def value\[T\]: T = ???' core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala` → 0
- Surgical edit verified: only the `object infer` block changed (1 hunk, 5 LOC delta — well under the plan's <15 LOC threshold)
- 0 new `@nowarn` / `-Wconf` in `git diff upstream/scala-3..HEAD -- '*.scala'`
- 0 `.planning/` paths in `git log --name-only upstream/scala-3..HEAD`
- 0 new `@nowarn` / `@ignore` added to test files

## Deviations from Plan

### [Rule 3 - Out of Scope] Phase 4 closure gate cannot be full multi-module

- **Found during:** Task 3 PART B (closure gate)
- **Issue:** Plan demanded `sbt clean compile + Test/compile + scalafmtCheckAll` exit 0 across **all enabled modules**. Verification on the base branch `20cf9fd0` (slice 4.4 HEAD) shows pre-existing compile failures in `commons-mongo` (`MongoEntityCompanion`, `MongoPolyDataCompanion`), `commons-hocon` (`ConfigCompanion`), and `commons-core-js` (`OptionLike.scala:71`, `metadata.scala:4`).
- **Root causes (pre-existing, not introduced by slice 4.5):**
  1. Slice 4.2's `Instances <: AnyNamedTuple` bound — mongo's `MongoPolyAdtInstances[D]` / `MongoEntityInstances[E]` / hocon's `ConfigCompanion` instance bundles are classical traits that violate the bound.
  2. JS-side `made.*` classpath gaps — slice 4.1's import block in `metadata.scala` and OptionLike's `made.Default[O]` bridge don't resolve under Scala.js compile context.
- **Decision:** Per [[feedback_small_scoped_prs]] and the scope boundary in execute-plan rules (only auto-fix issues DIRECTLY caused by current task's changes), do NOT attempt to fix these in Phase 4. They are tracked for Phase 5+ (JS made.* classpath) and Phase 7/9 (mongo/hocon reshape). Gate scope reduced to `commons-core/*` (matching all prior slice gates in this phase). Honest scope recorded in MIGRATION.md.
- **Files modified:** MIGRATION.md
- **Commit:** `f04cec6f`

### [Rule 1 - Plan vs Reality] MacroInstancesTest has no ScalaTest methods + two sub-objects un-fixable in Phase 4

- **Found during:** Task 2
- **Issue:** Plan acceptance required `grep -c 'pending' MacroInstancesTest.scala >= 1`. The file contains no `test("...")` blocks — only compile-time type-shape declarations. The Phase-1 `/* */` was a compile-protection wrap, not test-method gating (same pattern as slice 4.4 `AdtMetadataTest` deviation). Additionally, two of three sub-objects depend on Phase 6 work:
  - `ComplexInstancesTest.ComplexInstances[T]` mixes `val` / `var` / `def` + a method with implicit-parametered list — cannot be expressed as a named-tuple type alias (slice 4.2's `Instances <: AnyNamedTuple` bound).
  - `AnnotationReferringToEnclosingObjectTest` uses `infer.value` as a default-arg of an annotation parameter; with `MetaMacros.valueImpl = '{ ??? }` (slice 4.3 stub), the splice infers `T = Nothing` at the annotation call site, breaking case-class default-arg resolution: `Found: GenCodec[Nothing]; Required: GenCodec[Rec]`.
- **Decision:** Un-wrap the one section that survives Phase 4 bounds (`MultipleImplicitImportsTest` with named-tuple reshape), keep the other two wrapped under explicit `TODO[scala3-port]` headers pointing at Phase 6. No `pending` introduced (no test methods). No `@nowarn` / `@ignore`. Documented in MIGRATION.md. This is a deliberate divergence from the fork, which ships the entire file as `TodoScala3Migration DISABLED` for Scala 3 — we compile-check the surviving shapes per the plan's intent.
- **Files modified:** `core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala`, MIGRATION.md
- **Commit:** `c5c416be`, `f04cec6f`

### [User Directive] Task 4 PR steps skipped

- **Found during:** Task 4
- **Issue:** Plan Task 4 specified `gh pr create --draft ...` + milestone assignment + stack verification across all 5 Phase 4 PRs. User prompt explicitly: "**DO NOT OPEN PR** (user directive). Push to `origin/04-05-meta-annotations`."
- **Decision:** Push branch only. No `gh pr create`, no milestone, no stack-verification gh queries.
- **Files modified:** none
- **Commit:** n/a (push-only)

## Deferred Issues / Backlog

- **`ComplexInstancesTest` reshape** — Phase 6 (real MetaMacros bodies + GenCodec.materialize); tracked at `MacroInstancesTest.scala:16` in backlog.
- **`AnnotationReferringToEnclosingObjectTest` reshape** — Phase 6 (real `MetaMacros.valueImpl` body that walks annotation owner tree); tracked at `MacroInstancesTest.scala:62` in backlog.
- **Mongo / hocon `Instances` reshape** — Phase 7/9 (`MongoEntityCompanion`, `MongoPolyDataCompanion`, `ConfigCompanion` named-tuple migration).
- **JS-side `made.*` classpath** — Phase 5+ (`OptionLike.scala:71`, `metadata.scala:4` Not Found Errors under Scala.js compile context).
- **Phase 4 PR stack** — not opened per user directive. When ready, all five branches `04-01-foundation` → `04-05-meta-annotations` need draft PRs with milestone 1, `[Scala 3]` prefix, sequential `Depends on` chain.

## Phase 4 Closure Status

All 9 fork files at `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/` are now ported in our tree:

| File                         | Slice | Status                                                                              |
| ---------------------------- | ----- | ----------------------------------------------------------------------------------- |
| `AllowDerivation.scala`      | 4.1   | Verbatim                                                                            |
| `Fallback.scala`             | 4.1   | Verbatim                                                                            |
| `metadata.scala`             | 4.1   | Diverged — strips `@name("dupa")` fork-debug noise                                  |
| `OptionLike.scala`           | 4.1   | Diverged — preserves `BaseOptionLike` `@bincompat` shim                             |
| `MacroInstances.scala`       | 4.2   | Verbatim (new `Instances <: AnyNamedTuple` bound)                                   |
| `MetaMacros.scala`           | 4.3   | Verbatim (`'{ ??? }` bodies for `valueImpl`/`lazyMetadataImpl`/`dummy` per fork)    |
| `MetadataCompanion.scala`    | 4.3   | Verbatim                                                                            |
| `AdtMetadataCompanion.scala` | 4.4   | Verbatim (bound tightened to `M[X] <: TypedMetadata[X]`)                            |
| `metaAnnotations.scala`      | 4.5   | Verbatim (`object infer extends InferMacros` swap)                                  |

Scaffolding-only closure: Real macro bodies for `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` and downstream callers (`GenCodec.materialize`, `AdtMetadataCompanion.materialize` runtime path) remain deferred to Phase 6 per the fork-shipped staging.

## Self-Check: PASSED

- metaAnnotations.scala: FOUND — `grep -c 'object infer extends InferMacros' .../metaAnnotations.scala` → 1
- MacroInstancesTest.scala: FOUND — `/* */` top-of-file envelope lifted; `MultipleImplicitImportsTest` un-wrapped with named-tuple reshape
- MIGRATION.md: FOUND — §3 slice 4.5 entry present (`grep -c 'slice 4.5' MIGRATION.md` ≥ 1); `metaAnnotations.scala:193` backlog row removed (→ 0)
- Commit `78c4fd79`: FOUND
- Commit `c5c416be`: FOUND
- Commit `f04cec6f`: FOUND
- Branch `origin/04-05-meta-annotations`: FOUND (push output confirmed `* [new branch] 04-05-meta-annotations -> 04-05-meta-annotations`)
- PR NOT opened per user directive: CONFIRMED (no `gh pr create` executed)
