---
phase: 01-big-bang-comment-and-green
plan: 03
subsystem: jvm-modules
tags: [scala3, mongo, hocon, benchmark, cbor, stub, compile-gate]

requires:
  - phase: 01-big-bang-comment-and-green/01
    provides: scala-3-only build, scalafmt scala3 dialect, scalacOptions
  - phase: 01-big-bang-comment-and-green/02
    provides: commons-macros deleted; commons-core stubs; SealedEnumCompanion `lazy val` precedent
provides:
  - commons-hocon compiles on Scala 3
  - commons-mongo (JVM) compiles on Scala 3
  - commons-benchmark compiles on Scala 3
  - cbor sub-package (in commons-core) compiles on Scala 3
  - `commons-jvm/compile` aggregate exits 0 — every enabled JVM module green
affects: [01-04-js-variants, 01-05-tests-compile, 01-06-migration-md-and-push-pr]

tech-stack:
  added: []
  patterns:
    - "stub strategy (`= ???`) extended to mongo macro defs — supersedes plan-as-written `/* */` block-comment wording per memory feedback_stub_over_comment"
    - "`K[_]` → `K[Any]` workaround for Scala 3 wildcard-as-type-arg restriction (parallel to Plan 02 SelfInstance/HasGenCodec/SealedUtils precedent)"
    - "Type projection `E#IDType` on abstract type → widen to `Any` for stub-only compile gate"
    - "Scala 3 explicit `using` keyword required where Scala 2 context-bound second-implicit-arg-list was elided"

key-files:
  created: []
  modified:
    - hocon/src/main/scala/com/avsystem/commons/hocon/HTokenType.scala (val→lazy val for SealedEnumCompanion override)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala (1 macro def stubbed)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/DataTypeDsl.scala (4 macro defs stubbed, MongoMacros import removed)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoUtils.scala (1 macro def stubbed, MiscMacros import removed)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala (E#IDType → Any widening)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoCollection.scala (E#IDType → Any widening)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala (K[_]→K[Any], MongoEntityMeta IDType→Any)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala (D[_]→D[Any])
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoRef.scala (explicit `using` + K[Any] cast)
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/core/ops/BsonRefKeyValueHandling.scala (explicit `using`)

key-decisions:
  - "Plan-as-written `/* */` comment strategy superseded by `= ???` stub per memory feedback_stub_over_comment.md — six mongo macro defs stubbed, all callers continue compiling."
  - "Hocon needed zero scala-3-port commenting; the only diff was the `val values: List[HTokenType]` → `lazy val` override fix (same SealedEnumCompanion precedent as core Plan 02)."
  - "CBOR sub-package compiled clean without any TODO[scala3-port] tags being needed — Plan 02 core stubs covered the surface (HasGenCodec / GenObjectCodec / etc.)."
  - "Benchmark compiled clean without any commenting — the surface that depends on macro derivation went through `???` stubs in core/mongo and the bench code only consumes the runtime API."
  - "Spring directory left untouched (orphaned from jvm aggregate already in Plan 01); `lazy val spring` in `project/Commons.scala` is dead code but harmless under minimum-diff."

patterns-established:
  - "Stub-driven compile loop on a downstream module: stub macro defs → loop sbt compile → fix new errors with minimum-diff Scala 3 fixes (using/wildcards/type-projections) all in the SAME commit so callers stay coherent."

requirements-completed: [COMMENT-01, COMMENT-02, COMMENT-03, COMMENT-05, COMPILE-01, QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05]

duration: ~15 min
completed: 2026-06-01
---

# Phase 01 Plan 03: Other JVM modules compile-green Summary

**commons-jvm aggregate compiles on Scala 3 — hocon + mongo + cbor sub-package + benchmark all green via 6 mongo macro `???` stubs, 1 hocon lazy-val fix, and 5 minimum-diff Scala 3 syntax/semantic auto-fixes; cbor and benchmark needed zero changes.**

## Performance

- **Duration:** ~15 min
- **Started:** 2026-06-01T13:19:00Z
- **Completed:** 2026-06-01T13:30:00Z
- **Tasks:** 2 (per plan)
- **Files modified:** 10

## Accomplishments

- `sbt commons-jvm/compile` exits 0 — entire JVM aggregate green on Scala 3
- `sbt commons-benchmark/compile` exits 0 (out-of-aggregate sanity also covered)
- 6 mongo Scala-2 macro defs stubbed with `???` preserving callers (`BsonRef.Creator.ref`, `DataRefDsl.ref/.as/.is/.isNot`, `TypedMongoUtils.optionalizeFirstArg`)
- 1 hocon override-mode fix (val → lazy val)
- 5 minimum-diff Scala 3 semantic fixes: explicit `using` (3 sites), `K[_]`/`D[_]` → `K[Any]`/`D[Any]` (3 sites), `E#IDType` → `Any` (4 sites)

## Module Coverage

| Module | TODO[scala3-port] tags | Macro defs stubbed | Other changes |
|--------|------------------------|--------------------|---|
| hocon | 0 | 0 | 1 (lazy-val override) |
| mongo (JVM) | 13 | 6 | explicit `using` / K[Any] / IDType widening |
| cbor (sub-package in commons-core) | 0 | 0 | none — already covered by Plan 02 core stubs |
| benchmark (JVM) | 0 | 0 | none — bench code consumes runtime API only |

## Task Commits

Each task delivered as one or more atomic commits:

1. **Task 1: Hocon + cbor compile-green** — `6b9bdf4a` `fix(hocon): make SealedEnumCompanion override lazy`
2. **Task 2: Mongo + benchmark compile-green** — `9e2b290b` `refactor(mongo): stub Scala 2 macro defs with \`???\``

Benchmark and cbor required zero commits (already green).

## Files Modified

- `hocon/src/main/scala/com/avsystem/commons/hocon/HTokenType.scala` — `final val values` → `final lazy val values` for SealedEnumCompanion override (same precedent as core Plan 02)
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala` — `Creator.ref` macro → `???`
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/DataTypeDsl.scala` — 4 macro defs (`ref`, `as`, `is`, `isNot`) → `???`; removed `MongoMacros` import
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoUtils.scala` — `optionalizeFirstArg` macro → `???`; removed `MiscMacros` import
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala` — `type ID = E#IDType` → `type ID = Any`
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoCollection.scala` — `type ID = E#IDType` → `type ID = Any`
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala` — `TypedMapFormat.keyCodec: GenKeyCodec[K[_]]` → `K[Any]`; `MongoEntityMeta` IDType references widened to `Any`
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala` — `C <: D[_]` → `C <: D[Any]`
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoRef.scala` — `BsonValueInput.read(doc)(format.codec)` → `(using format.codec)`; `TypedMapRefOps.apply` cast key to `K[Any]`
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/core/ops/BsonRefKeyValueHandling.scala` — `BsonValueOutput.write(t)(bsonRef.codec)` → `(using bsonRef.codec)`

## Decisions Made

- **Stub over comment (per memory feedback_stub_over_comment.md):** Replaced bodies with `???` instead of wrapping defs in `/* */` blocks. Plan-as-written said "comment broken defs" but the memory rule supersedes plan wording. This keeps callers compiling — critical for `commons-jvm/compile` to ever go green incrementally.
- **Bundle Scala 3 syntax/semantic auto-fixes into the stub commit:** the `using`/`K[Any]`/`E#IDType` fixes were discovered by the stub-driven compile loop; splitting them into a separate commit would have left an intermediate revision RED. Single coherent `refactor(mongo):` commit preserves bisectability.
- **Hocon SealedEnumCompanion fix is `fix(hocon):` not `refactor(hocon):`:** the build was broken on this file; conventional-prefix rule maps "broken→green" to `fix:`. Matches the same precedent in Plan 02 SealedEnumCompanion lazy-val change to core.
- **Spring orphaned but not deleted:** orchestrator-supplied directive said "delete if referenced". Spring is already commented out of the jvm aggregate (Plan 01); `lazy val spring` definition in `project/Commons.scala` is dead but harmless. Deletion deferred to a future plan that removes ALL orphaned project defs together.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Hocon `SealedEnumCompanion.values` must be `lazy val`**
- **Found during:** Task 1 first compile attempt
- **Issue:** `final val values: List[HTokenType] = caseObjects` cannot override the `lazy val` declared in `SealedEnumCompanion` — Scala 3 enforces this where Scala 2 didn't. Single E164 declaration error.
- **Fix:** `final val` → `final lazy val`. Same precedent as Plan 02 core fix.
- **Files modified:** `hocon/src/main/scala/com/avsystem/commons/hocon/HTokenType.scala`
- **Verification:** `sbt commons-hocon/compile` exit 0
- **Committed in:** `6b9bdf4a` (Task 1 commit)

**2. [Rule 1 - Bug] `BsonValueOutput.write(t)(codec)` needs explicit `using` (3 sites)**
- **Found during:** Task 2 stub-driven loop
- **Issue:** Scala 2 `[T: GenCodec]` context bound allowed `write(t)(codec)` to pass the codec as a second implicit arg list. Scala 3 desugars the same context bound to a single `using` parameter list, and the call `write(t)(codec)` is parsed as applying the result. Compile error "method read/write does not take more parameters".
- **Fix:** Insert explicit `using` keyword at the 3 call sites.
- **Files modified:** `BsonRefKeyValueHandling.scala`, `MongoFormat.scala` (1 site), `MongoRef.scala` (1 site)
- **Verification:** call sites resolve; no `@nowarn` introduced
- **Committed in:** `9e2b290b` (Task 2 commit)

**3. [Rule 1 - Bug] `K[_]` / `D[_]` as type argument needs `K[Any]` / `D[Any]` (3 sites)**
- **Found during:** Task 2 stub-driven loop
- **Issue:** Scala 3 rejects unreducible application of higher-kinded type parameter to wildcards — E043 "unreducible application of higher-kinded type K to wildcard arguments".
- **Fix:** Replace `K[_]`/`D[_]` with `K[Any]`/`D[Any]` in the affected signatures and cast at one call site (`TypedMapRefOps.apply`). Same precedent as Plan 02 core fix (TypedMap K[_]→K[Any]).
- **Files modified:** `MongoFormat.scala` (TypedMapFormat.keyCodec + typedMapFormat factory), `MongoPolyDataCompanion.scala` (isMongoAdtOrSubtype bound), `MongoRef.scala` (cast at TypedMapRefOps.apply)
- **Verification:** all four error sites resolve under clean compile
- **Committed in:** `9e2b290b` (Task 2 commit)

**4. [Rule 1 - Bug] `E#IDType` type projection on abstract `E` (4 sites)**
- **Found during:** Task 2 stub-driven loop
- **Issue:** Scala 3 forbids type projections on non-concrete prefixes. `E#IDType` where `E <: BaseMongoEntity` fails with "E is not a legal path since it is not a concrete type". This is a true language-level change; no per-call-site workaround keeps the original semantics.
- **Fix:** Widen `E#IDType` references to `Any` at the four use sites. Stub-grade signature preservation (loses static typing on the ID column, but `BaseMongoEntity` is currently a stubbed surface anyway and the real port will need to redesign this). Every site tagged `// TODO[scala3-port]: …` for follow-up.
- **Files modified:** `MongoEntityCompanion.scala`, `TypedMongoCollection.scala`, `MongoFormat.scala` (MongoEntityMeta fields + idRef)
- **Verification:** sites resolve; clean compile green
- **Committed in:** `9e2b290b` (Task 2 commit)

---

**Total deviations:** 4 auto-fixed (all Rule 1 — Scala 3 dialect/semantic differences uncovered by stub-driven compile loop)
**Impact on plan:** Each auto-fix was strictly necessary to reach `commons-jvm/compile` exit 0. No scope creep; every change tagged with `// TODO[scala3-port]:`.

## Issues Encountered

- **Plan said "comment".** Memory rule `feedback_stub_over_comment.md` says "stub with `???`". The plan was authored before that memory took effect. Stub strategy applied throughout. Orchestrator pre-flagged this exact override.
- **Spring directory exists.** Orchestrator directive: "if referenced, delete outright; if orphaned, leave it." Spring is already commented out of the jvm aggregate from Plan 01 — orphaned. Left alone. The `lazy val spring` in `project/Commons.scala` is dead code but harmless; deletion deferred.
- **Cbor sub-package needed zero work.** Plan called for "MEDIUM volume" of stubbing. In practice, Plan 02 already stubbed all the consumed core surface; cbor itself is `case class`/`val` data plus serialization-pipeline plumbing that doesn't reference any deleted-or-stubbed names. Recorded as a positive deviation.
- **Benchmark needed zero work.** Plan anticipated cascading commenting from codec consumers. The JMH benchmarks reference `materialize`/`GenCodec.materialize` indirectly through HasGenCodec, but those are already stubbed in core. Recorded as a positive deviation.

## Verification

- `sbt -batch ';clean ;commons-jvm/compile ;commons-benchmark/compile ;scalafmtCheckAll'` — exit 0
- `git log upstream/scala-3..HEAD` diff search: 0 lines introducing `@nowarn` or `-Wconf`
- `git log upstream/scala-3..HEAD --format='%s'` grep for `.planning` or GSD nomenclature: 0 matches
- All commits use conventional prefix (`fix(hocon):`, `refactor(mongo):`)

## Self-Check: PASSED

Files checked:
- `.planning/phases/01-big-bang-comment-and-green/01-03-SUMMARY.md` — written this commit
- `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoUtils.scala` — FOUND, `optionalizeFirstArg = ???` present
- `hocon/src/main/scala/com/avsystem/commons/hocon/HTokenType.scala` — FOUND, `final lazy val values` present

Commits checked:
- `6b9bdf4a` — FOUND (`fix(hocon):`)
- `9e2b290b` — FOUND (`refactor(mongo):`)

## Next Plan Readiness

- `commons-jvm/compile` is green — Plans 04 (JS variants) and 05 (tests compile) unblocked
- 13 new TODO[scala3-port] tags added across mongo (search corpus grows for follow-up porting work)
- Plan 06 (MIGRATION.md + push) must record:
  - mongo macro-defs stubbed (BsonRef.ref, DataRefDsl.{ref,as,is,isNot}, optionalizeFirstArg)
  - `E#IDType` widening (4 sites — public-facing API loss; downstream consumers need to know `type ID = Any` until the real port)
  - K[_]/D[_] → K[Any]/D[Any] (signature widening — should be source-compatible for typical use)

---
*Phase: 01-big-bang-comment-and-green*
*Completed: 2026-06-01*
