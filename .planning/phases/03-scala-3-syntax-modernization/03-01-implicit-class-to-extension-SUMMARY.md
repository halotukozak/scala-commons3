---
phase: 03-scala-3-syntax-modernization
plan: 01
subsystem: infra
tags: [scala-3, extension, implicit-class, mongo, gencodec, syntax-rewrite]

requires:
  - phase: 01-big-bang-comment-and-green
    provides: Scala-3-only build baseline + commented-out tests
provides:
  - "All `implicit class XOps … extends AnyVal` declarations across core+mongo swept to Scala 3 `extension` blocks"
  - "Pitfall-7 documented pattern: `given Conversion` over `extension` for HKT-receiver DSL with named-arg defaults (UpdateOperatorsDsl)"
  - "@targetName disambiguation pattern documented for extension-method erasure clashes (MongoPropertyRef TypedMapRefOps)"
affects: [03-02-hkt-wildcards, 03-03-implicit-to-given, 03-04-inline-defs]

tech-stack:
  added: []
  patterns:
    - "extension [A](a: A) { … } replaces `implicit class XOps[A](private val a: A) extends AnyVal { … }`"
    - "given [C[X] <: Iterable[X], T, R] => Conversion[…, …] for HKT-receiver DSLs where named-arg call sites break `extension` inference"
    - "@scala.annotation.targetName disambiguation when extension methods on same namespace erase-collide"
    - "Hoist imports above extension/given body (extension body cannot contain imports)"

key-files:
  created:
    - .planning/phases/03-scala-3-syntax-modernization/03-01-implicit-class-to-extension-SUMMARY.md
  modified:
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoRef.scala
    - MIGRATION.md

key-decisions:
  - "Plan-as-written scope (5 files) expanded to 7 files (Rule 3 - Blocking): plan missed MongoFormat (3 implicit class), MongoPolyDataCompanion, MongoRef (3 implicit class), and 2nd ForCollection in QueryOperatorsDsl — acceptance grep gate requires 0 hits so all 15 occurrences in scope had to be converted"
  - "ReactiveMongoExtensions.scala SKIPPED — does not contain `implicit class`; uses `implicit def publisherOps + final class PublisherOps extends AnyVal` pattern. Removing the `implicit def` is slice 3.3 territory (`implicit def/val` → `given`). Acceptance grep gate already passes for this file."
  - "UpdateOperatorsDsl converted to `given Conversion` not `extension` per Pitfall 7 (HKT-receiver named-arg inference); a brief mid-execution attempt to use plain `extension` confirmed Pitfall 7 with a compile error (`Found: MongoFormat[C[T]]; Required: MongoFormat[T]`), validating the documented choice"
  - "MongoPropertyRef.TypedMapRefOps required `@scala.annotation.targetName(\"typedMapApply\")` (Rule 1 - Bug auto-fix) because `apply(K)` and `apply(K[T])` extensions share the companion's namespace post-conversion and both erase to `apply(Object)` — pre-existing value-class wrapping previously kept them separate"
  - "GenCodec's 4 private wrappers translated mechanically (fork's `scala-3/GenCodec.scala` overlay diverges substantially from current single-source file) per CONTEXT.md guidance"

requirements-completed: [SYNTAX-31-IMPLICIT-CLASS, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

duration: 9 min
completed: 2026-06-01
---

# Phase 03 Plan 01: implicit class → extension Summary

**15 `implicit class … extends AnyVal` declarations across core (4) and mongo/typed (11) swept to Scala 3 `extension` blocks; UpdateOperatorsDsl uses `given Conversion` per HKT-receiver Pitfall 7; draft PR #868 opened.**

## Performance

- **Duration:** ~9 min
- **Started:** 2026-06-01T16:14:48Z (approx)
- **Completed:** 2026-06-01T16:23:56Z
- **Tasks:** 3 (plan tasks; 8 fork-cadence commits)
- **Files modified:** 8 (7 Scala + MIGRATION.md)

## Accomplishments

- Branch `03-01-implicit-class-to-extension` cut off `upstream/scala-3 @ 0887d555`.
- Acceptance grep gate: `git grep -n 'implicit class' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala'` → 0 hits (was 15).
- Full build gate green: `sbt compile + Test/compile + scalafmtCheckAll` exit 0.
- 8 atomic Conventional Commits (per-file/feature scope, fork-cadence — no squash).
- MIGRATION.md §3 updated with `core` + `mongo` extension-conversion entries.
- Draft PR opened at AVSystem/scala-commons#868 with `[Scala 3]` prefix, milestone "Scala 3" (#1), body metadata block (Slice 3.1 / Depends on: none / Base: upstream/scala-3).
- Zero new `@nowarn`/`-Wconf`. Zero `.planning/` in commits. Zero GSD nomenclature.

## Task Commits

1. **Task 1: GenCodec core wrappers** — `83333148` (refactor)
2. **Task 2a: UpdateOperatorsDsl HKT given Conversion** — `4467a1d8` (refactor)
3. **Task 2b: QueryOperatorsDsl Vanilla+non-Vanilla → extension** — `d1e0195b` (refactor)
4. **Task 2c: MongoEntityCompanion.macroDslExtensions** — `92a8eb28` (refactor)
5. **Task 2d (added): MongoPolyDataCompanion.macroDslExtensions** — `2e382f36` (refactor)
6. **Task 2e (added): MongoFormat assume* (3 ops)** — `88de8c77` (refactor)
7. **Task 2f (added): MongoPropertyRef Collection/Dictionary/TypedMap RefOps** — `c7c423c5` (refactor)
8. **Task 3: MIGRATION.md §3 entries** — `c3a3fc1b` (docs)

## Files Created/Modified

- `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala` — 4 private internal value-class wrappers (IterableOps, PairIterableOps, ListInputOps, ObjectInputOps) → `extension` blocks.
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala` — `implicit class ForCollection extends AnyVal` → `class ForCollection` + `given [C[X] <: Iterable[X], T, R] => Conversion[…, ForCollection[C, T, R]]`. Hoisted `import MongoUpdateOperator._`. Added `scala.language.implicitConversions` import.
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala` — 2 `implicit class ForCollection` (Vanilla + non-Vanilla) → 2 `extension` blocks. Inner `format` helper renamed `elemFormat`. Hoisted `import MongoQueryOperator._`.
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala` — `implicit class macroDslExtensions(value: T)` → `extension (value: T)`.
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala` — `implicit class macroDslExtensions[T](value: D[T])` → `extension [T](value: D[T])`.
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala` — 3 `implicit class … assume*` companion ops → 3 `extension` blocks.
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoRef.scala` — 3 `implicit class …RefOps` → 3 `extension` blocks. `TypedMapRefOps.apply[T](key: K[T])` annotated `@scala.annotation.targetName("typedMapApply")`.
- `MIGRATION.md` — §3 entries under `core` and `mongo` documenting all conversions.

## Decisions Made

(see frontmatter `key-decisions` for full list)

- Plan scope expanded from 5 to 7 files per Rule 3 (Blocking) — acceptance grep required zero hits across the full target set.
- ReactiveMongoExtensions deliberately skipped (no `implicit class` to convert; remaining `implicit def publisherOps` is slice 3.3 territory).
- UpdateOperatorsDsl `given Conversion` shape vindicated mid-execution — a transient plain-`extension` attempt produced exactly the compile error documented in Pitfall 7.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Plan undercounted in-scope `implicit class` occurrences**

- **Found during:** Task 1 baseline grep.
- **Issue:** Plan listed 5 target files but `git grep -n 'implicit class' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala'` returned 15 occurrences across 7 files. The plan missed: `MongoFormat.scala` (3), `MongoPolyDataCompanion.scala` (1), `MongoRef.scala` (3), and `QueryOperatorsDsl.scala`'s 2nd `ForCollection` (in `VanillaQueryOperatorsDsl` companion). Plan also listed `ReactiveMongoExtensions.scala` which has 0 `implicit class` hits (uses `implicit def + class AnyVal` pattern — slice 3.3 territory).
- **Fix:** Converted all 15 occurrences across all 7 files in fork-cadence atomic commits. Skipped ReactiveMongoExtensions per slice boundary.
- **Files modified:** 4 additional files beyond the plan's nominal list.
- **Verification:** Final acceptance grep returns 0 hits; full sbt gate green.
- **Committed in:** 88de8c77, 2e382f36, c7c423c5 (added file commits).

**2. [Rule 1 - Bug] Erasure clash between `apply(K)` and `apply(K[T])` extension methods in MongoPropertyRef companion**

- **Found during:** Task 2f (MongoRef conversion).
- **Issue:** After converting `DictionaryRefOps.apply(key: K)` and `TypedMapRefOps.apply[T](key: K[T])` from value-class implicit classes to `extension` methods sharing the `MongoPropertyRef` companion namespace, both `apply` methods erase to `apply(Object)`, triggering Scala 3 `[E120] Naming Error: Conflicting definitions`. Pre-conversion they lived in separate value-class types so no JVM erasure clash arose.
- **Fix:** Added `@scala.annotation.targetName("typedMapApply")` to the typed-map variant. Full `scala.annotation.` qualification needed because unqualified `annotation` resolves to `com.avsystem.commons.annotation`.
- **Files modified:** `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoRef.scala`.
- **Verification:** `sbt commons-jvm/compile` exit 0.
- **Committed in:** `c7c423c5`.

**3. [Rule 3 - Blocking] Verified Pitfall 7 mid-execution via a transient plain-`extension` attempt on UpdateOperatorsDsl**

- **Found during:** Task 3 (after MIGRATION.md edit, working tree showed a linter/intentional-override rewrite of UpdateOperatorsDsl from `given Conversion` to plain `extension`).
- **Issue:** Plain `extension [C[X] <: Iterable[X], T, R](dsl: UpdateOperatorsDsl[C[T], R])` shape produced 8 compile errors of form `Found: MongoFormat[C[T]]; Required: MongoFormat[T]` at `Push(...)` call sites — exactly the symptom documented in Pitfall 7 (the inner `private def format: MongoFormat[T]` reference confused the type-checker because plain `extension` cannot unify `C`/`T` from named-arg-defaulted methods sharing scope).
- **Fix:** Reverted UpdateOperatorsDsl.scala to the previously-committed `given Conversion` + `class ForCollection` shape (`git checkout HEAD --`).
- **Files modified:** none beyond revert; commit `4467a1d8` remains the canonical UpdateOperatorsDsl change.
- **Verification:** `sbt commons-jvm/compile` exit 0; no new commit needed (working tree restored to the already-committed state).
- **Committed in:** (no new commit — Pitfall 7 confirmed against the canonical `given Conversion` shape).

---

**Total deviations:** 3 auto-fixed (2 blocking, 1 bug).
**Impact on plan:** All auto-fixes essential for compile correctness and acceptance-gate completion. Plan scope grew by 4 files (Rule 3) but stayed inside the same slice (3.1) per fork-shape boundaries. No scope creep into slice 3.3 territory (`implicit def/val` left untouched throughout).

## Authentication Gates

None.

## Issues Encountered

None beyond the deviations above.

## User Setup Required

None.

## PR & Branch

- **Branch:** `halotukozak:03-01-implicit-class-to-extension` (pushed; tip `f37b442f`).
- **PR:** [#868 at AVSystem/scala-commons](https://github.com/AVSystem/scala-commons/pull/868) — DRAFT, title `[Scala 3] convert implicit class to extension`, milestone "Scala 3" (#1), base `scala-3`.
- **Commits on branch (initial sweep, 8):**
  - `83333148` refactor(scala-3,core): GenCodec implicit class → extension (4 private wrappers)
  - `4467a1d8` refactor(scala-3,mongo): UpdateOperatorsDsl implicit class → given Conversion (HKT receiver)
  - `d1e0195b` refactor(scala-3,mongo): QueryOperatorsDsl implicit class → extension
  - `92a8eb28` refactor(scala-3,mongo): MongoEntityCompanion macroDslExtensions implicit class → extension
  - `2e382f36` refactor(scala-3,mongo): MongoPolyDataCompanion macroDslExtensions implicit class → extension
  - `88de8c77` refactor(scala-3,mongo): MongoFormat assume* implicit class → extension (3 ops)
  - `c7c423c5` refactor(scala-3,mongo): MongoPropertyRef CollectionRefOps/DictionaryRefOps/TypedMapRefOps implicit class → extension
  - `c3a3fc1b` docs(migration): record implicit class → extension source-compat impact

- **Commits on branch (de-facto-implicit-class follow-up sweep, 15):** prior executor missed the
  `implicit def xOps + class XOps extends AnyVal` pattern — same idiom as `implicit class` but split in two for stub
  compilation. This follow-up covers the missing files.
  - `f8ca7e6d` refactor(scala-3,mongo): ReactiveMongoExtensions implicit def + AnyVal → extension
  - `72e7bd5a` refactor(scala-3,mongo): MongoOps DBOps/FindIterableOps → extension
  - `71c5ecd2` refactor(scala-3,core): JavaTimeInterop InstantOps → extension
  - `fe964558` refactor(scala-3,core): Java8CollectionUtils implicit def + AnyVal → extension
  - `66ada4ef` refactor(scala-3,core): GuavaInterop implicit def + AnyVal → extension
  - `16666fbc` refactor(scala-3,core): JOptionalUtils implicit def + AnyVal → extension (OptionLike unified)
  - `7ce1d00f` refactor(scala-3,core): JStreamUtils implicit def + AnyVal → extension
  - `d40eaeb4` refactor(scala-3,core): TaskExtensions TaskOps/TaskCompanionOps → extension
  - `a4f30ed2` refactor(scala-3,core): JCollectionUtils pairIterableOps implicit def + AnyVal → extension
  - `a30ff9ef` refactor(scala-3,core): Opt.LazyOptOps implicit def + AnyVal → extension
  - `06aed30a` refactor(scala-3,core): SharedExtensions implicit def + AnyVal → extension
  - `dd2e5fb2` refactor(scala-3,core): JsInterop UndefOrOps/JsOptOps implicit def + AnyVal → extension
  - `caaa3dfc` style(scala-3): scalafmt fixups for slice 3.1 extension sweep
  - `f37b442f` docs(migration): record de-facto-implicit-class → extension sweep (slice 3.1 follow-up)

## Follow-up Sweep — Deviations & Findings

**1. [Rule 3 - Blocking] Pattern boundary clarified for `wraps-stdlib`/`wraps-shared` cases**

- **Found during:** Acceptance grep sweep.
- **Issue:** Several `implicit def xOps(x: T): SomeOtherClass = new SomeOtherClass(x)` patterns wrap **non-local**
  value classes (stdlib `DurationInt`, `DurationLong`, `IntMult` etc. via `ScalaDurationExtensions` /
  `DurationPostfixConverters`; shared `TimestampConversions` via `Timestamp.conversions`, `JBasicUtils.jDateTimestampConversions`,
  `JsInterop.jsDateTimestampConversions`; shared `BsonUpdating`/`BsonFiltering`/`BsonRefUpdating`/etc. via mongo ops files).
  These are NOT `extension` material — they're plain `implicit def` conversions of type `A → B`, with `B` defined
  elsewhere. Per fork shape (`origin/master`), these become `given Conversion[A, B]` — slice 3.3 territory
  (`implicit def → given`).
- **Fix:** Out of scope for slice 3.1. Documented as deferred to 3.3.
- **Files left untouched:** `ScalaDurationExtensions.scala`, `DurationPostfixConverters.scala`, `Timestamp.scala`,
  `JBasicUtils.scala`, mongo `BsonRef.scala`, mongo `DocKey.scala`, mongo `core/ops/Updating.scala`,
  `core/ops/Filtering.scala`, `JsInterop.jsDateTimestampConversions` (kept as `implicit def`).
- **Verification:** Acceptance grep on de-facto-implicit-class pattern (`implicit def \w+\([^)]+\): \w+(\[.*\])? *= *new \w+`)
  returns only these wraps-stdlib/wraps-shared entries — all confirmed slice-3.3 candidates by inspection.

**2. [Rule 1 - Bug] Scala 3 erasure clash on `extension [T](opt: Opt[T]) / NOpt[T] / OptArg[T] / Option[T] { def toJOptional, def asJava }`**

- **Found during:** JOptionalUtils conversion attempt.
- **Issue:** All four value-class `Opt`-family extensions erase to `Object` JVM-side. Per-type extensions caused
  E120 Naming Errors. Worse, the `extension (option: Option[T]) { def asJava: JOptional[T] }` shape was
  eagerly picked over `scala.collection.convert.AsJavaExtensions.asJava` on `Seq`/`Iterable`, breaking 8+ call sites
  in `Filter.scala` / `Update.scala` / `Sort.scala` with errors like `Found: Optional[Seq[Bson]]; Required: Iterable[Bson]`.
- **Fix:** Adopted fork shape: ONE generic
  `extension [O[_], T](opt: O[T])(using optionLike: OptionLike.Aux[O[T], T]) { def toJOptional, def asJava }`.
  The `using OptionLike` constraint restricts resolution to true option-likes (Option/Opt/NOpt/OptArg) — the constraint
  fails to resolve for `Seq` and the scala-jdk `asJava` extension wins.
- **Files modified:** `JOptionalUtils.scala`.
- **Commit:** `16666fbc`.

**3. [Rule 1 - Bug] Test failure: `JOptional(x).asScala` resolving to `GuavaInterop.asScala`**

- **Found during:** JOptionalUtils conversion test compile.
- **Issue:** With both `GuavaInterop.asScala` (extension on `ListenableFuture[T]`) and `JOptionalUtils.asScala`
  (extension on `JOptional[T]`) visible, the test `JavaInteropTest.scala` imports only `GuavaInterop._` at file scope.
  Scala 3 prefers imported extensions over package-object-mixed-in extensions for name lookup, and won't backtrack on
  receiver-type mismatch — `JOptional(x).asScala` tried `GuavaInterop.asScala`, found receiver-type incompatible,
  and reported the failure without ever trying `JOptionalUtils.asScala` from the package-object mixin scope.
- **Fix:** Added `import com.avsystem.commons.jiop.JavaInterop._` in `JavaInteropTest.scala`, lifting JOptionalUtils'
  extensions to equal import-rank with GuavaInterop's. Compiler then disambiguates by receiver type.
- **Files modified:** `core/jvm/src/test/scala/com/avsystem/commons/jiop/JavaInteropTest.scala` (test-only).
- **Commit:** `16666fbc` (combined with JOptionalUtils).

**4. [Rule 1 - Bug] JsInterop conversion required SharedExtensions conversion first**

- **Found during:** First JsInterop conversion attempt — caused JS compile of `JsonStringInput.scala` to fail
  with `Found: Option[Int]; Required: Int` at `_.get(name).toOpt.map(idx => peekFieldInput(name, idx))`.
- **Issue:** `js.UndefOr[A] = A | js.undefined` is a union type. Scala value `Option[Int]` IS assignable to
  `UndefOr[Option[Int]]` (matches the `A` branch). My new `extension [A](undefOr: UndefOr[A]) { def toOpt }`
  matched `Option[Int]` with `A = Option[Int]`, returning `Opt[Option[Int]]` instead of `Opt[Int]`. The compiler
  preferred the new extension over the lower-priority implicit-class conversion `SharedExtensions.OptionOps.toOpt`.
- **Fix:** Reverted JsInterop change, converted `SharedExtensions.OptionOps` to `extension` FIRST. Once both
  competitors are extensions, the compiler disambiguates by receiver-type specificity — `Option[Int]` resolves to
  the more specific `extension [A](option: Option[A])` extension, not the wider `extension [A](undefOr: UndefOr[A])`.
- **Files modified:** SharedExtensions.scala (full sweep), then JsInterop.scala (re-applied).
- **Commits:** `06aed30a`, `dd2e5fb2`.

**5. [Rule 2 - Critical] `SharedExtensionsUtils` companion object eliminated**

- **Found during:** SharedExtensions conversion.
- **Issue:** Original layout had a `SharedExtensionsUtils` companion object holding all 18 value-class wrapper
  classes. Once converted to extensions, this companion object becomes empty (no classes to hold). Helper singletons
  `FutureCompanionOps`, `TryCompanionOps`, `IteratorCompanionOps`, `PartialFunctionOps.{NoValueMarker, NoValueMarkerFunc}`,
  `MapOps.Entry` — previously private to the trait — need a new home for extension methods to delegate.
- **Fix:** Promoted these helpers to `object SharedExtensions` companion (the `SharedExtensions extends SharedExtensions`
  pattern). Extension methods in the trait delegate via the trait-level `import SharedExtensions.{FutureCompanionOps, ...}`.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala`.
- **Public-API impact:** `SharedExtensions.MapOps.Entry` and `SharedExtensions.PartialFunctionOps.{NoValueMarker, NoValueMarkerFunc}`
  import paths preserved. `SharedExtensionsUtils.*` no longer exists — internal-only, no downstream impact.

**Total follow-up duration:** ~31 min (17:04 → 17:34 UTC).

## Final Acceptance Gate (follow-up sweep)

- `git grep -nE 'implicit def \w+\([^)]+\): \w+(\[.*\])? *= *new \w+' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala'`
  → 11 remaining hits, all stdlib/shared-helper conversions (slice 3.3 territory) — verified by inspection.
- `sbt compile + Test/compile + scalafmtCheckAll` → exit 0 (no errors, pre-existing warnings only — no new `@nowarn` or `-Wconf`).
- 14 new atomic Conventional Commits on PR #868 branch tip (was `c3a3fc1b`, now `f37b442f`).
- Branch pushed to `origin/03-01-implicit-class-to-extension` (PR #868).

## Next Phase Readiness

- Slice 3.1 complete (initial + follow-up). Draft PR #868 pending manual ready-for-review flip.
- Slice 3.2 (HKT wildcards `_` → `?`) ready to plan/execute.
- Slice 3.3 (`implicit def/val → given`) now has 11+ extra targets: stdlib-wrapping `implicit def` (Duration*, Mult*,
  TimestampConversions, BsonUpdating/Filtering, mongo BsonRef/DocKey ops) and the deliberately-skipped
  `JsInterop.jsDateTimestampConversions`. Document these in slice 3.3 RESEARCH.md.

## Self-Check: PASSED

All 12 files modified exist on disk; all 14 follow-up commits found in `git log` between `c3a3fc1b` and `f37b442f`.

---
*Phase: 03-scala-3-syntax-modernization*
*Completed: 2026-06-01 (follow-up sweep)*
