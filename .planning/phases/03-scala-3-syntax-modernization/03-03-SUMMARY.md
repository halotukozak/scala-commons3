---
phase: 03-scala-3-syntax-modernization
plan: 03
subsystem: serialization,mongo,hocon,core
tags: [scala3, implicit-to-given, using, slice-3.3, source-compat]
dependency-graph:
  requires: []
  provides:
    - "anonymous-given pattern for typeclass instances across core/mongo/hocon/benchmark"
    - "BsonGenCodecs.export-given + @deprecated shim pattern (fork 8f70be80)"
    - "(implicit X: T) → (using X: T) sweep across mongo (fork eef0edce)"
    - "OptArg.argToOptArg preservation with verbatim erasure-bridge comment"
  affects:
    - "downstream callers of named-imported typeclass instances (must switch to summon[T] or import X.given)"
    - "named-arg call sites of mongo methods (must update foo(x = …) to foo(using x = …))"
    - "import X._ callers of KeyGetter implicit objects (must switch to import X.given)"
tech-stack:
  added: []
  patterns:
    - "anonymous given T = …"
    - "given X: T with { … } (replaces implicit object X extends T)"
    - "export X.given (trait exports given from companion)"
    - "@deprecated def name: T = summon (source-compat shim)"
    - "(using X: T) parameter lists"
key-files:
  created: []
  modified:
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/EntityIdMode.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/ObjectIdWrapperCompanion.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/Filter.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/core/ops/KeyGetter.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoTypedKey.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoCollection.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/core/ops/BsonRefKeyElementHandling.scala
    - core/src/main/scala/com/avsystem/commons/misc/OptArg.scala
    - core/src/main/scala/com/avsystem/commons/misc/BoxingUnboxing.scala
    - core/src/main/scala/com/avsystem/commons/misc/TypedMap.scala
    - core/src/main/scala/com/avsystem/commons/misc/Timestamp.scala
    - core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala
    - core/src/main/scala/com/avsystem/commons/misc/ValueOf.scala
    - core/src/main/scala/com/avsystem/commons/misc/TypeString.scala
    - core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala
    - core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala
    - core/src/main/scala/com/avsystem/commons/misc/Delegation.scala
    - core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala
    - core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala
    - core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala
    - core/src/main/scala/com/avsystem/commons/misc/SelfInstance.scala
    - core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
    - core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
    - core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/GenObjectCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/HasGenCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/SerializationName.scala
    - core/src/main/scala/com/avsystem/commons/serialization/TransparentWrapperCompanion.scala
    - core/src/main/scala/com/avsystem/commons/serialization/TupleGenCodecs.scala
    - core/src/main/scala/com/avsystem/commons/serialization/cbor/CborOptimizedCodecs.scala
    - core/src/main/scala/com/avsystem/commons/serialization/cbor/CborAdtMetadata.scala
    - core/src/main/scala/com/avsystem/commons/serialization/cbor/RawCbor.scala
    - core/src/main/scala/com/avsystem/commons/serialization/json/WrappedJson.scala
    - core/src/main/scala/com/avsystem/commons/tuples/TupleDerivation.scala
    - core/src/main/scala/com/avsystem/commons/concurrent/executionContexts.scala
    - core/src/main/scala/com/avsystem/commons/di/Component.scala
    - core/src/main/scala/com/avsystem/commons/di/Components.scala
    - core/jvm/src/main/scala/com/avsystem/commons/concurrent/BlockingUtils.scala
    - core/js/src/main/scala/com/avsystem/commons/serialization/nativejs/NativeFormatOptions.scala
    - hocon/src/main/scala/com/avsystem/commons/hocon/ConfigCompanion.scala
    - hocon/src/main/scala/com/avsystem/commons/hocon/HTree.scala
    - benchmark/jvm/src/main/scala/com/avsystem/commons/ser/GenCodecBenchmarks.scala
    - benchmark/jvm/src/main/scala/com/avsystem/commons/ser/StreamInputOutputBenchmark.scala
    - mongo/jvm/src/main/scala/** (19 files in (implicit)→(using) sweep)
    - MIGRATION.md
decisions:
  - "Branch `03-03-implicit-to-given` cut FRESH off `upstream/scala-3@0887d555` (NOT stacked on PR #868 or #869) per phase methodology."
  - "Three borderline preservations: OptArg.argToOptArg, GenRef.fun2GenRef (Phase-2 stub), and RunNowEC/RunInQueueEC.Implicits.executionContext (wildcard-import idiom)."
  - "Anonymous `given T = …` for canonical typeclass instances; named `given foo: T = …` only when fork chose named or when needed (e.g. when carriers may reference by name)."
  - "BsonGenCodecs follows fork 8f70be80 pattern: trait `export BsonGenCodecs.given` + object anonymous-givens + `@deprecated def name: T = summon` shims for every previously-named accessor."
  - "Extension-shim `implicit def Xops(…)` cohort deliberately preserved (SharedExtensions, jiop, jsiop, BsonRef ops, DocKey ops, MongoRef refOps, etc.) — these belong to slice 3.1 (PR #868) which converts them to `extension` blocks AND deletes the conversion."
  - "Acceptance gate `0 implicit hits except 2 documented exceptions` NOT met — 136 hits remain, all are either slice-3.1 extension-shims, slice-3.5 Implicits.scala territory, RPC (out of scope per project rules), or the three documented preservations. Post-rebase against PR #868 + PR #867 these will resolve."
metrics:
  duration: "~65 min (50 initial + 15 fork-shape drift fix)"
  completed: "2026-06-01"
  commits: 10
  files-touched: 84
---

# Phase 3 Plan 03: implicit def/val → given Summary

Slice 3.3 of Phase 3 — rewrite `implicit def` / `implicit val` / `implicit object` to Scala 3 `given` declarations and `(implicit X: T)` parameter lists to `(using X: T)` across core (serialization, cbor, meta, misc, tuples), mongo (BsonGenCodecs anonymous-given + 19-file using sweep + typeclass instances), hocon (HoconGenCodecs codecs), benchmark, with three borderline preservations carrying verbatim fork explanatory comments.

## One-liner

Slice 3.3 — anonymous-given typeclass-instance pattern with `@deprecated def = summon` shims, mongo `(implicit X) → (using X)` sweep across 19 files, BsonGenCodecs.export-given pattern, OptArg.argToOptArg + GenRef.fun2GenRef + RunNowEC.Implicits.executionContext preserved with rationale.

## Execution

Branch `03-03-implicit-to-given` cut off `upstream/scala-3@0887d555` (Phase 01 big-bang merge tip — NOT stacked on slice 3.1 PR #868 or slice 3.2 PR #869 per phase methodology). Seven atomic fork-cadence commits:

| # | Hash | Title |
| - | ---- | ----- |
| 1 | `78d5f3a3` | `refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + @deprecated shims` |
| 2 | `0525e127` | `refactor(scala-3,mongo): (implicit X: T) → (using X: T) parameter list sweep` |
| 3 | `0b176a0c` | `fix(scala-3): preserve OptArg.argToOptArg implicit (erasure-bridge collision)` |
| 4 | `71905a38` | `refactor(scala-3,mongo): residual implicit val/def → given for typeclass instances` |
| 5 | `3ceb8b59` | `refactor(scala-3,core): implicit val/def → given for typeclass instances + materialize stubs` |
| 6 | `ab689a24` | `refactor(scala-3): implicit val/def → given for hocon, benchmark, residuals + scalafmt` |
| 7 | `890d9630` | `docs(migration): record implicit → given + (implicit X) → (using X) source-compat impact` |
| 8 | `003be078` | `refactor(scala-3,core): OptArg int conversions → given Conversion (fork shape match)` |
| 9 | `2638ca12` | `refactor(scala-3,core): GenCodec residual implicit lazy val → given (fork shape match)` |
| 10 | `e9b8e132` | `refactor(scala-3,core): residual implicit val/def → given (fork shape match)` |
| 11 | `d4c32809` | `refactor(scala-3,core): @deprecated def shims for renamed BoxingUnboxing givens` |
| 12 | `04c051c8` | `refactor(scala-3,core): @deprecated def shims for renamed GenKeyCodec primitive givens` |
| 13 | `b803002a` | `docs(migration): record @deprecated def shims for BoxingUnboxing + GenKeyCodec` |

## Fork-Shape Drift Fix (post-hoc 2026-06-01)

Post-hoc drift sweep identified files where fork shape diverged from original commits (the initial sweep's regex `implicit\s+(def|val)` missed `implicit lazy val`). Three follow-up commits brought slice-3.3 files in line with fork:

### Per-file given/implicit counts (post-fix vs fork)

| File | ours given | ours impl | fork given | fork impl | notes |
| --- | --- | --- | --- | --- | --- |
| core/.../misc/OptArg.scala | 2 | 1 | 3 | 1 | fork-only `given [A] => Default[OptArg[A]]` requires `made` on core-js classpath — out of slice 3.3 scope (build-config change). argToOptArg preserved. |
| core/.../serialization/GenCodec.scala | 61 | 0 | 47 | 0 | ours has 14 more (Phase-2 retained named-codec accessors like `arrayCodec`, `bseqCodec`, `iseqCodec`, etc. consumed by macroCodecs.scala by NAME). |
| core/.../serialization/GenRef.scala | 2 | 1 | n/a | n/a | fun2GenRef preserved (macro splice); codec/SimpleRawRef.codec converted to given. |
| core/.../serialization/cbor/CborAdtMetadata.scala | 2 | 0 | n/a | n/a | HasCborCodec/HasCborCodecWithDeps codec → given. |
| core/.../misc/SealedUtils.scala | 2 | 0 | n/a | n/a | codec/keyCodec → given. |
| core/.../misc/TypedMap.scala | 1 | 0 | 1 | 0 | pairToEntry → given Conversion. |
| core/.../misc/ValueEnum.scala | 2 | 0 | n/a | n/a | valName/enumCtx → given; AbstractValueEnum (implicit val) → (using val). |
| core/jvm/.../concurrent/ObservableBlockingIterator.scala | 0 | 0 | n/a | n/a | (implicit val scheduler) → (using val scheduler). |
| core/.../di/Components.scala | 2 | 1 | n/a | n/a | autoComponent preserved with rationale (by-name + macro-stub; non-inline given Conversion cannot carry by-name). |
| mongo/jvm/.../typed/MongoFormat.scala | 9 | 0 | 11 | 0 | gap = 2 inner givens inside `typedMapFormat` body (fork-stylistic; our impl passes them positionally to `TypedMap.typedMapCodec`, functionally equivalent). |

### Documented preservations (4 total)

1. `OptArg.argToOptArg` — erasure-bridge collision
2. `GenRef.fun2GenRef` — macro splice over inline arg (Phase-2 stub)
3. `RunNowEC/RunInQueueEC.{Implicits.executionContext, executionContext}` — wildcard-import idiom, source-compat
4. `Components.autoComponent` — by-name parameter + macro-stub body (added 2026-06-01 post-hoc)

### Verification

- `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0 (all green)
- No new `@nowarn`/`-Wconf` introduced
- Sweep grep `git grep -nE 'implicit (lazy )?(val|def)' -- core/src/main/scala core/jvm/src/main/scala core/js/src/main/scala mongo hocon benchmark` filtered for non-test/non-rpc/non-Implicits/non-extension-shim files yields only the 4 documented preservations + comment-only false positives.

All gates green: `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0; no new `@nowarn`/`-Wconf` (`git diff upstream/scala-3..HEAD '*.scala' | grep '^+.*(@nowarn|-Wconf)'` empty); no `.planning/` in commits; no GSD nomenclature in commit messages.

Pushed `halotukozak:03-03-implicit-to-given @ 890d9630`. Opened AVSystem/scala-commons PR **#870** draft, base `scala-3`, milestone Scala 3 (#1) via `gh api PATCH /repos/AVSystem/scala-commons/issues/870 -f milestone=1`, title `[Scala 3] implicit def/val → given`. PR body metadata block: `Slice 3.3 / Merge order 3.1 → 3.2 → 3.3 → 3.4 / Depends on #869 / Base branch upstream/scala-3 (not stacked)`. PR left OPEN for manual maintainer merge per global rules.

## Named-Import Source-Compat Shim Sweep (post-hoc 2026-06-01)

Per user directive 2026-06-01: preserve named defs as `@deprecated def NAME: T = summon` shims so downstream callers using named-import lookup don't break post-anonymous-given conversion. Inventory of the 234 `implicit val/def` names removed by PR #870:

| Bucket | Count | Disposition |
| --- | --- | --- |
| Named `given NAME: T = …` preserved on branch | 174 | Already covered by existing shape |
| `@deprecated def NAME: T = summon` already present (BsonGenCodecs done in `78d5f3a3`) | 17 | Already covered |
| Implicit kept (Rule 4 preservations) | 4 | `autoComponent`, `forOptional`, `optionalRefOps`, `transparentRefOps` |
| `given Conversion[A,B]` (per skip-list rule: conversions out of scope) | 3 | `intToOptArgLong`, `intToOptArgDouble`, `pairToEntry` |
| `Filter.Limitations.CanCompare` nested object members (fork-shape match: anonymous, no shim) | 4 | `date`, `int32`, `int64`, `double` |
| **Shims ADDED in this sweep** | **32** | BoxingUnboxing (14) + GenKeyCodec (18) |

Three follow-up commits on PR #870:

| # | Hash | Title |
| - | ---- | ----- |
| 11 | `d4c32809` | `refactor(scala-3,core): @deprecated def shims for renamed BoxingUnboxing givens` |
| 12 | `04c051c8` | `refactor(scala-3,core): @deprecated def shims for renamed GenKeyCodec primitive givens` |
| 13 | `b803002a` | `docs(migration): record @deprecated def shims for BoxingUnboxing + GenKeyCodec` |

### Shim additions

**BoxingUnboxing.scala** — 14 shims (7 Boxing + 7 Unboxing): `BooleanBoxing`, `ByteBoxing`, `ShortBoxing`, `IntBoxing`, `LongBoxing`, `FloatBoxing`, `DoubleBoxing`, `BooleanUnboxing`, `ByteUnboxing`, `ShortUnboxing`, `IntUnboxing`, `LongUnboxing`, `FloatUnboxing`, `DoubleUnboxing`. Each: `@deprecated("Use summon[Boxing[X,Y]]", "scala-3-port") def NAME: Boxing[X, Y] = summon`.

**GenKeyCodec.scala** — 18 shims for primitive KeyCodecs: `BooleanKeyCodec`, `CharKeyCodec`, `ByteKeyCodec`, `ShortKeyCodec`, `IntKeyCodec`, `LongKeyCodec`, `BigIntKeyCodec`, `JBooleanKeyCodec`, `JCharacterKeyCodec`, `JByteKeyCodec`, `JShortKeyCodec`, `JIntKeyCodec`, `JLongKeyCodec`, `JBigIntegerKeyCodec`, `StringKeyCodec`, `SymbolKeyCodec`, `TimestampKeyCodec`, `BytesKeyCodec`.

### Verification

- `sbt commons-core/compile`, `Test/compile`, `scalafmtCheckAll` all exit 0 (verified in `/tmp/sc3-clone-303` clone — local worktree hit jgit `NoWorkTreeException` from `sbt-git` plugin, irrelevant to actual compile)
- No new `@nowarn`/`-Wconf`
- 32 shims emit `@deprecated` warnings at call sites — intentional, points callers to `summon[T]`
- PR #870 verified updated: `gh pr view 870 --repo AVSystem/scala-commons` → `headRefOid=b803002a`, still draft, title intact

## Deviations from Plan

### Auto-fixed Issues (Rule 1 / Rule 3)

**1. [Rule 3 - Blocking] `BsonGenCodecs.objectIdCodec` named-accessor callers**

- **Found during:** Task 2a (BsonGenCodecs rewrite)
- **Issue:** `EntityIdMode.scala` line 23 + `ObjectIdWrapperCompanion.scala` line 26 referenced `BsonGenCodecs.objectIdCodec` as a value. After the rewrite these became `@deprecated` shims emitting deprecation warnings.
- **Fix:** Switched both callers to `summon[GenCodec[ObjectId]]` and added `import com.avsystem.commons.mongo.BsonGenCodecs.given` to bring the anonymous given into scope.
- **Commit:** `78d5f3a3`

**2. [Rule 3 - Blocking] `this(rawCollection)(meta)` call site after `(implicit) → (using)` sweep**

- **Found during:** Task 2b (using sweep)
- **Issue:** `TypedMongoCollection.scala` `@bincompat` constructor `this(rawCollection)(meta)` referenced the primary constructor whose second arg-list is now `(using meta: …)`. The original call-site didn't have `using` keyword.
- **Fix:** Added `using` keyword: `this(rawCollection)(using meta)`.
- **Commit:** `0525e127`

**3. [Rule 3 - Blocking] `import meta.format._` no longer picks up `dataClassTag` given**

- **Found during:** Task 2c (residual mongo typeclass conversions)
- **Issue:** `MongoAdtFormat.codec` and `dataClassTag` converted from `implicit def` to `given` in the trait declaration. The internal call site `TypedMongoCollection.mkNativeCollection` uses `import meta.format._` to bring `dataClassTag` into implicit scope — wildcard `_` import does NOT capture givens.
- **Fix:** Switched to `import meta.format.{given, _}` AND replaced bare `classTag.runtimeClass` with `summon[ClassTag[E]].runtimeClass`.
- **Commit:** `71905a38`

**4. [Rule 3 - Blocking] `GenCodec.fromTransparentWrapping(arg1, arg2)` callers after conversion to `given`**

- **Found during:** Task 1 (core typeclass conversions)
- **Issue:** `EntityIdMode.scala` and `ObjectIdWrapperCompanion.scala` invoked `GenCodec.fromTransparentWrapping(idWrapping, summon[GenCodec[ObjectId]])` as a regular method. After conversion to `given`, Scala 3 reports "given instance fromTransparentWrapping in object GenCodec does not take more parameters".
- **Fix:** Added `using` keyword at call sites: `GenCodec.fromTransparentWrapping(using idWrapping, summon[GenCodec[ObjectId]])`.
- **Commit:** `3ceb8b59`

**5. [Rule 3 - Blocking + Source-compat revert] `RunNowEC.Implicits.executionContext` test breakage**

- **Found during:** Task 1 final gate (`sbt Test/compile`)
- **Issue:** After converting `RunNowEC.Implicits.executionContext` from `implicit val` to `given`, `Test/compile` failed for `SharedExtensionsTest.scala` — tests use `import RunNowEC.Implicits._` (wildcard import) which does NOT capture givens. Compiler explicitly says "Note: given instance executionContext in object Implicits was not considered because it was not imported with `import given`."
- **Fix:** Reverted just this `Implicits.executionContext` pair (`RunNowEC` + `RunInQueueEC`) back to `implicit val`. The wildcard-import-into-`Implicits`-object idiom is the public API contract — converting silently breaks downstream callers and our own tests. Documented in MIGRATION.md as a deliberate preservation alongside the `OptArg.argToOptArg` + `GenRef.fun2GenRef` borderline list.
- **Commit:** `ab689a24` (kept as `implicit val`)

### Scope Deviations

**Plan acceptance gate not met — 136 `implicit val/def` hits remain.** The plan's success criterion was `git grep '^\s*(inline\s+)?implicit\s+(def|val)' → exactly 2 documented exceptions`. This cannot be met without conflicting with slice 3.1's territory:

- **~50 hits in `core/src/main/scala/SharedExtensions.scala`, `core/jvm/.../jiop/*`, `core/js/.../jsiop/*`, `core/.../concurrent/TaskExtensions/DurationPostfixConverters`, `core/.../misc/ScalaDurationExtensions/Opt/NOpt/OptRef/Timestamp.conversions`, `core/.../jiop/JBasicUtils/JCollectionUtils/JFactory`** are `implicit def Xops(x: T): XOps = new XOps(x)` — these are the conversion shims for `implicit class` value-class wrappers, which slice 3.1 (PR #868) rewrites to `extension` blocks AND deletes the conversion entirely. Per phase methodology, this slice MUST NOT convert them. After PR #868 merges and this branch rebases, they will disappear.
- **~14 hits in `mongo/jvm/.../BsonRef.scala`, `DocKey.scala`, `Filtering.scala`, `Updating.scala`, `MongoOps.scala`, `MongoRef.scala` (optionalRefOps/transparentRefOps), `QueryOperatorsDsl.scala` (forOptional)** — same extension-shim pattern, slice 3.1 territory.
- **~3 hits in `core/.../misc/Implicits.scala`** — `Implicits` object is being deleted outright by slice 3.5 PR #867. Out of scope per project memory rule.
- **~20 hits in `core/.../rpc/*`** — fork commit `39c047eb` explicitly REMOVED the RPC module from scala-3 ("remove RPC module from scala-3"). Per CONTEXT.md the rpc module is out of scope for this phase entirely.
- **3 documented preservations** — `OptArg.argToOptArg`, `GenRef.fun2GenRef` (Phase-2 stub), `RunNowEC/RunInQueueEC.Implicits.executionContext`.

All these are documented in MIGRATION.md §3 (slice 3.3 entries under core/mongo/hocon) so downstream consumers understand the scope.

## Authentication Gates

None — pure mechanical syntax rewrite, no external API or auth needed.

## Verification

- `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0 (all three modules + tests compile clean under Scala 3.8.2 -Werror)
- `git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` empty (no new suppressions)
- `git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning/'` returns 0
- `git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'` empty
- PR #870 verified via `gh pr view 870 --repo AVSystem/scala-commons --json isDraft,title,milestone,baseRefName`: `{isDraft: true, title: "[Scala 3] implicit def/val → given", milestone: "Scala 3", base: "scala-3"}`.

## Self-Check: PASSED

- All 7 commits verified present via `git log --oneline -10` from `upstream/scala-3..HEAD`
- All 76 modified files staged and committed (verified via `git diff --stat upstream/scala-3..HEAD`)
- BsonGenCodecs uses `export BsonGenCodecs.given` + anonymous-given + `@deprecated def name: T = summon` shim pattern as required
- OptArg.argToOptArg PRESERVED with verbatim fork comment (erasure-bridge collision rationale)
- MIGRATION.md §3 has comprehensive core + mongo + hocon entries for slice 3.3
- PR #870 open at AVSystem: draft, `[Scala 3]` title, milestone Scala 3 (#1), base `scala-3`, body metadata block with `Slice 3.3 / Merge order / Depends on #869 / Base branch upstream/scala-3`

## History Cleanup (2026-06-01)

User directive: light cleanup — fold drift-fix follow-ups into their logical parents.

**First pass (slice-3.3 only):** 13 → 7 commits.

**Second pass (post 3.6-form sweep, 2026-06-01):** 19 → 14 commits. Tree state preserved (verified via `diff /tmp/before-303.diff /tmp/after-303.diff` → empty). `sbt clean; commons-core/compile; commons-core/Test/compile; scalafmtCheckAll` exit 0 post-rewrite.

### New commit history (PR #870)

| #  | Hash       | Title                                                                                                          |
| -- | ---------- | -------------------------------------------------------------------------------------------------------------- |
| 1  | `120a7d27` | refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + @deprecated shims                  |
| 2  | `5e1a83c7` | refactor(scala-3,mongo): (implicit X: T) → (using X: T) parameter list sweep                                   |
| 3  | `e0d42cbd` | fix(scala-3): preserve OptArg.argToOptArg implicit (erasure-bridge collision)                                  |
| 4  | `87df9ee3` | refactor(scala-3,mongo): residual implicit val/def → given for typeclass instances                             |
| 5  | `38b0c762` | refactor(scala-3,core): implicit val/def → given for typeclass instances + materialize stubs                   |
| 6  | `8e484e28` | refactor(scala-3): implicit val/def → given for hocon, benchmark, residuals + fork-shape drift + @deprecated shims |
| 7  | `e921a8f0` | docs(migration): record implicit → given + (implicit X) → (using X) source-compat impact                       |
| 8  | `cf28f296` | refactor(scala-3): bump @deprecated since to 3.0.0                                                             |
| 9  | `a2bdaae0` | refactor(scala-3,core): GenCodec primitive givens → anonymous + @deprecated shims                              |
| 10 | `442087e1` | refactor(scala-3,core): BoxingUnboxing givens → Scala 3.6 named context-function form                          |
| 11 | `41bf942e` | refactor(scala-3,core): GenKeyCodec givens → Scala 3.6 named context-function form                             |
| 12 | `f080b1c4` | refactor(scala-3,core): SealedUtils + TypedMap givens → Scala 3.6 named context-function form                  |
| 13 | `c716bc5f` | refactor(scala-3,core): GenCodec givens → named Scala 3.6 form, drop @deprecated shims                         |
| 14 | `4128fd3b` | refactor(scala-3,mongo): MongoFormat + BsonGenCodecs givens → Scala 3.6 named context-function form            |

Folds applied:
- `a1aa3336` (GenCodec named primitive shims) + `3095655d` (GenCodec all primitive anonymous + macroCodecs mangled names) → consolidated into **#9** `a2bdaae0`.
- `13fd0446` (BoxingUnboxing 3.6 anonymous) + `5cb86024` (BoxingUnboxing named 3.6, drop shims) → consolidated into **#10** `442087e1`.
- `fd640d4b` (GenKeyCodec 3.6 anonymous) + `721d4081` (GenKeyCodec named 3.6, drop shims) → consolidated into **#11** `41bf942e`.
- `3140b3ec` (SealedUtils+TypedMap 3.6 anonymous) + `a9920429` (SealedUtils+TypedMap named 3.6) → consolidated into **#12** `f080b1c4`.
- `868063f0` (MongoFormat 3.6 anonymous) + `b593a58c` (MongoFormat+BsonGenCodecs named 3.6, drop shims) → consolidated into **#14** `4128fd3b`.

Reorders are safe: each 3.6-form pair touches a disjoint file set from the others, so moving the "named 3.6" follow-ups adjacent to their "anonymous 3.6" predecessors produces no merge conflicts. The original ordering of commits 1–8 is preserved (notably `8e484e28` heavily modifies GenCodec, so the GenCodec primitive fold #9 cannot move ahead of it without conflicting — left in place).

Backup tag: `backup-before-cleanup/03-03-implicit-to-given-1780348258`. Force-pushed with `--force-with-lease` to `halotukozak:03-03-implicit-to-given` (PR #870). Tip moved from `b593a58c` → `4128fd3b`.
