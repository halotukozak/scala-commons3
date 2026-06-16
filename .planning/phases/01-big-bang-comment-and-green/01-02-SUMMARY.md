---
phase: 01-big-bang-comment-and-green
plan: 02
subsystem: core
tags: [scala3, macros, stub, build, core]

requires:
  - phase: 01-big-bang-comment-and-green/01
    provides: Scala-3-only build (scalacOptions, scalafmt dialect, made 0.1.1 on core)
provides:
  - "commons-macros module deleted from build and working tree"
  - "commons-core compiles green on Scala 3"
  - "100 \\`TODO[scala3-port]\\` tags marking stubbed defs for future port"
  - "All scala-2 macro defs replaced with `= ???` stubs preserving signatures"
affects: [01-big-bang-comment-and-green/03 mongo, 01-big-bang-comment-and-green/04 hocon/cbor, 01-big-bang-comment-and-green/05 benchmark, 01-big-bang-comment-and-green/06 polish]

tech-stack:
  added: []
  patterns:
    - "`= ???` stub strategy (preserve signatures, defer real port via TODO tag)"
    - "`// TODO[scala3-port]: name (Scala 2 macro def) (S/M/L)` tag convention"
    - "Minimum-diff Scala 3 syntactic fixes when stub uncovered pre-existing 3.x incompatibilities"

key-files:
  created: []
  modified:
    - build.sbt (no change — root unaffected)
    - project/Commons.scala (drop commons-macros project, drop dependsOn(macros), drop unidoc filter entry)
    - 39 core sources (stub macro defs + ancillary Scala 3 fixes)
  deleted:
    - macros/** (28 scala-2 macro source files + entire module directory)

key-decisions:
  - "DELETE commons-macros outright (per project-deletable-modules memory rule, supersedes plan-as-written commenting strategy)"
  - "STUB broken defs in core with \\`= ???\\` (per feedback-stub-over-comment memory rule, supersedes /* */ block-commenting from prior commits)"
  - "Replaced existing block-commented macro defs (commits 561a95d5 and 92907dab) with stubs in 5 atomic refactor commits"
  - "Tag-suffix grammar locked: `// TODO[scala3-port]: name (Scala 2 macro def) (S/M/L)`"
  - "Stub return-type widenings tracked: SealedUtils.instancesFor List[TC[_ <: T]] -> List[TC[T]] (HKT wildcard rejected); Components.optEmptyComponent/noneComponent/sequenceOpt/sequenceOption stubbed entirely (depend on stubbed singleton/component macros)"
  - "Drop Comparable[Timestamp] from class Timestamp extends AnyVal (Scala 3 forbids AnyVal extending Object-derived traits) — TODO tag added"
  - "TypedMap.K[_] -> K[Any] at type-arg positions; .asInstanceOf casts at use sites — Scala 3 forbids HKT wildcard application"
  - "Tag companion: explicit `override def unapply(t: Tag): Option[Int]` — Scala 3 AnyVal case class synthetic unapply returns wrapper, not Option"

patterns-established:
  - "Stub macro-def with `= ???`: one TODO tag per def, signature intact, no `// format: off`"
  - "Prune `import com.avsystem.commons.macros.*` aggressively (module deleted)"
  - "When a stub's return type cannot be expressed in Scala 3 (HKT wildcards), widen with explicit TODO note"
  - "Stubbing exposes pre-existing Scala 3 issues unrelated to macros — apply minimum-diff fix and mark with TODO"

requirements-completed: [COMMENT-01, COMMENT-02, COMMENT-03, COMMENT-05, COMPILE-01, QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05]

duration: ~35 min
completed: 2026-06-01
---

# Phase 01 Plan 02: Macros-and-Core Summary

**commons-macros module deleted outright; commons-core compiles green on Scala 3 with 100 `???`-stubbed Scala 2 macro defs awaiting future port.**

## Performance

- **Duration:** ~35 min
- **Started:** 2026-06-01T12:35:00Z (approx)
- **Completed:** 2026-06-01T13:10:00Z
- **Tasks:** 2 (per plan)
- **Files modified:** 39 (1 build, 38 core) + 28 deleted

## Accomplishments

- `commons-macros` module deleted from sbt aggregate (28 macro source files + project/Commons.scala wiring); `sbt projects` no longer lists `commons-macros`.
- `commons-core/compile` exits 0 on Scala 3.8.2 (was ~12 errors at branch base after prior commenting commits; before this plan started: 114+ errors per Phase 5 baseline).
- 100 `// TODO[scala3-port]:` tags in `core/src/main/scala` mark every stubbed def — grep-friendly map for follow-up porting waves.
- `scalafmtCheckAll` exit 0.
- Zero new `@nowarn`/`-Wconf` introduced (per memory rule `feedback_fix_dont_suppress_warnings`).

## Task Commits

Task 1 (Delete commons-macros module):
1. `1eda26b2` — `build: drop commons-macros module (deleted from Scala 3 port)` (29 files changed: project/Commons.scala + git rm of macros/**)

Task 2 (Stub broken defs in commons-core), 5 atomic subpackage commits:
2. `a8adca09` — `refactor(core): stub Scala 2 macro defs in serialization with \`???\`` (10 files: GenCodec, GenKeyCodec, GenObjectCodec, GenRef, HasGenCodec, TupleGenCodecs, macroCodecs, whenAbsent, cbor/CborAdtMetadata, cbor/definitions)
3. `897c950d` — `refactor(core): stub Scala 2 macro defs in rpc with \`???\`` (5 files)
4. `f5dbf915` — `refactor(core): stub Scala 2 macro defs in meta with \`???\`` (4 files)
5. `07c93df5` — `refactor(core): stub Scala 2 macro defs in misc with \`???\`` (15 files)
6. `78330755` — `refactor(core): stub remaining Scala 2 macro defs with \`???\`` (5 files: SharedExtensions, AnnotationAggregate, positioned, CloseableIterator, Components)

## Files Created/Modified

**Deleted:** entire `macros/` directory (28 .scala sources + module config).

**Modified:**
- `project/Commons.scala` — drop `lazy val macros`, drop `.dependsOn(macros)` from `core` and `core-js`, drop `macros` from `jvm` aggregate and `unidoc` filter
- 38 core sources — stub macro defs + ancillary Scala 3 minimum-diff fixes

## Decisions Made

User directive 2026-06-01 SUPERSEDED the plan-as-written (which still said "comment"):
1. `commons-macros` module: **DELETE** (not stub, not comment) — per `project_deletable_modules.md`.
2. Broken defs in `commons-core`: **STUB with `???`** — per `feedback_stub_over_comment.md`. Block-commenting (the prior approach in commits `561a95d5` and `92907dab`) removed defs from the namespace, breaking every caller.

This plan superseded both prior approaches with `= ???` stubs.

## Deviations from Plan

### Auto-fixed Issues (Rule 1 / Rule 3 — minimum-diff fixes to make compile green)

Stubbing macros uncovered pre-existing Scala 3 incompatibilities unrelated to the macro deletion itself. All fixed per Rule 1 (bug fix) or Rule 3 (blocking).

**1. [Rule 3 — Blocking] CloseableIterator overload erasure collision**
- **Found during:** Task 2, Pass 2 (compile-driven iteration)
- **Issue:** `def apply[T](it: JIterator[T] with AutoCloseable)` and `def apply[T](it: Iterator[T] with AutoCloseable)` erase to `apply(AutoCloseable)` in Scala 3; conflicting definitions.
- **Fix:** Added `@targetName("applyScalaCloseable")` on the Scala-Iterator overload.
- **Files modified:** core/src/main/scala/com/avsystem/commons/collection/CloseableIterator.scala
- **Verification:** `commons-core/compile` exit 0.
- **Committed in:** `78330755`

**2. [Rule 1 — Bug] Components.optEmptyComponent et al. produced ambiguous-implicit cascade after stubbing**
- **Found during:** Task 2, Pass 2
- **Issue:** Stubbed `def singleton/component(implicit sourceInfo)` couldn't resolve `SourceInfo` because the trait's `ambiguousArbitraryComponent1/2` (declared null implicits for divergent-implicit prevention) were being considered by Scala 3 implicit search as candidates for `SourceInfo`. Original macros side-stepped this via macro expansion.
- **Fix:** Stub `optEmptyComponent/noneComponent/sequenceOpt/sequenceOption` with `???` too — they all depend on the stubbed singleton/component macros anyway and will be re-implemented as a unit when the di subsystem is ported.
- **Files modified:** core/src/main/scala/com/avsystem/commons/di/Components.scala
- **Committed in:** `78330755`

**3. [Rule 1 — Bug] TypedMap `K[_]` existential application**
- **Found during:** Task 2, Pass 2
- **Issue:** Scala 3 rejects applying a higher-kinded type parameter `K` to wildcard `_` or `?` in type-argument position ("unreducible application of higher-kinded type K to wildcard arguments", E043).
- **Fix:** Narrow type-arg `K[_]`→`K[Any]` in type positions; add `.asInstanceOf[K[Any]]` casts at use sites. Type-param-declaration `[K[_]]` left intact. Loses some compile-time precision but preserves observable behavior.
- **Files modified:** core/src/main/scala/com/avsystem/commons/misc/TypedMap.scala
- **Committed in:** `07c93df5`

**4. [Rule 1 — Bug] SelfInstance.instance type widening**
- **Found during:** Task 2, Pass 2
- **Issue:** Same E043 as above for `case class SelfInstance[C[_]](instance: C[_])`.
- **Fix:** `C[_]` → `C[Any]` with TODO note.
- **Committed in:** `07c93df5`

**5. [Rule 1 — Bug] HasGenCodec.wildcardCodec same E043**
- **Fix:** `GenCodec[C[_]]` → `GenCodec[C[Any]]` (lossless: the asInstanceOf casts in the body never needed the precision).
- **Committed in:** `a8adca09`

**6. [Rule 1 — Bug] Timestamp AnyVal extending Comparable[Timestamp]**
- **Found during:** Task 2, Pass 2
- **Issue:** Scala 3 forbids `AnyVal` extending traits whose superclass isn't `Any` ("illegal trait inheritance"). `Comparable` derives from `Object`.
- **Fix:** Drop `with Comparable[Timestamp]` from the class signature; keep the `compareTo(o: Timestamp): Int` method (used by the existing `implicit val ordering`). TODO tag added.
- **Committed in:** `07c93df5`

**7. [Rule 1 — Bug] SealedEnumCompanion.values lazy override**
- **Found during:** Task 2, Pass 2
- **Issue:** `ValueEnumCompanion.values` is `final lazy val`, overriding a non-lazy `val values: ISeq[T]` in `SealedEnumCompanion`. Scala 3 disallows lazy overriding non-lazy.
- **Fix:** Declare base as `lazy val values: ISeq[T]` (still abstract in trait body).
- **Committed in:** `07c93df5`

**8. [Rule 1 — Bug] SealedUtils.instancesFor HKT wildcard return type**
- **Issue:** `List[TC[_ <: T]]` triggers E043.
- **Fix:** Widen to `List[TC[T]]` with TODO note (precision loss is acceptable since the def is `???` anyway).
- **Committed in:** `07c93df5`

**9. [Rule 1 — Bug] Tag (cbor) AnyVal case class unapply mismatch**
- **Found during:** Task 2, Pass 2
- **Issue:** `case class Tag(value: Int) extends AnyVal`; Scala 3 synthesizes `unapply(t: Tag): Tag` (returns the wrapper itself), not `Option[Int]`. `TransparentWrapperCompanion[Int, Tag]` requires abstract `def unapply(t: T): Option[R]`.
- **Fix:** Add explicit `override def unapply(t: Tag): Option[Int] = Some(t.value)` to `object Tag`.
- **Files modified:** core/src/main/scala/com/avsystem/commons/serialization/cbor/definitions.scala
- **Committed in:** `a8adca09`

**10. [Rule 1 — Bug] GenCodec.bseqCodec/iseqCodec explicit context-bound passing**
- **Issue:** `seqCodec[BSeq, T](GenCodec[T], implicitly[Factory[T, List[T]]])` — Scala 3.4+ requires `using` keyword for explicit context-bound argument passing.
- **Fix:** `(GenCodec[T], ...)` → `(using GenCodec[T], ...)`. Same for `iseqCodec` and for `CborAdtMetadata.rawKey` writeRawCbor calls.
- **Committed in:** `a8adca09`

---

**Total deviations:** 10 minimum-diff Scala 3 syntax/semantic fixes.
**Impact on plan:** All necessary to land `commons-core/compile` exit 0. None expand scope — each is the smallest change that unblocks compilation, with TODO tags or comment notes preserving intent for future polish.

## Issues Encountered

- Branch had two prior "comment" commits (`561a95d5` macros, `92907dab` core defs) that this plan SUPERSEDED. Approach: build on top with new stub commits rather than rewriting history (preserves auditability of the strategy pivot).
- WIP working-tree edits from a prior session were stashed at start, dropped at end (superseded by the per-plan implementation).

## Notable Surviving Areas in commons-core

What compiles on Scala 3 without stubbing (the "what works" baseline):
- All collection/extension API (SharedExtensions, IteratorOps, etc.) — pure Scala stdlib usage.
- Manual `GenCodec` instances (BooleanCodec, IntCodec, all primitive/String/Bytes/UUID/Timestamp codecs in GenCodec.scala bottom half).
- All GenKeyCodec primitive instances.
- Input/Output/SimpleInput/SimpleOutput infrastructure.
- JSON serialization (JsonStringInput/JsonStringOutput).
- CBOR low-level infrastructure (writers, readers, Major).
- Concurrent helpers (Async, RunNowEC, Awaitable).
- Most jiop helpers.
- Opt/NOpt/OptRef/OptArg "made wiring primitives" from Phase 4.
- Made integration cherry-picks already on branch.

What is `???`-stubbed (the "deferred port" list, ≥100 tags):
- All `materialize[T]` / `materializeForApi[T]` for GenCodec, GenObjectCodec, GenKeyCodec, RpcMetadata, AsRaw, AsReal, AsRawReal.
- All RPCFramework macros (materializeAsRaw/AsReal/AsRawReal/Metadata/FullInfo, plus per-trait asRealRPC/asRawRPC/metadata in RPCCompanion).
- Components di macros (component/asyncComponent/singleton/asyncSingleton/reifyAllSingletons/autoComponent + dependents optEmptyComponent/noneComponent/sequenceOpt/sequenceOption).
- meta macros: MacroInstances.materialize, lazyMetadata, Adt/RpcMetadataCompanion.materialize, infer.value.
- misc macros: AnnotationOf/OptAnnotationOf/AnnotationsOf/HasAnnotation/SelfAnnotation*/Applier/Unapplier/ApplierUnapplier, Bidirectional.apply, Delegation, Implicits.infer, Sam.apply, SamCompanion.apply, SealedUtils.caseObjectsFor/instancesFor/caseObjects, SelfInstance.materialize, SimpleClassName.materialize, SourceInfo.here, TypeString.materialize, JavaClassName.materialize, ValueEnumCompanion.valName.
- serialization macros: GenCodec.{materialize, fromApplyUnapplyProvider, applyUnapplyCodec, fromJavaBuilder, forSealedEnum, materializeRecursively, materializeImplicitly}, GenObjectCodec.{materialize, fromApplyUnapplyProvider}, GenKeyCodec.{forSealedEnum, forTransparentWrapper}, GenRef.{ref, fun2GenRef}, RawRef.Creator.ref, ApplyUnapplyCodec.materialize, mkTupleCodec, whenAbsent.value.
- annotation.AnnotationAggregate.reifyAggregated, annotation.positioned.here.
- SharedExtensions debug macros (showAst, showRawAst, etc.).
- RpcUtils.compilationError.

## Files Where Return Type Was Forced to Widen

Tracked for future plans:
- `misc/SealedUtils.scala`: `instancesFor[TC[_], T]: List[TC[T]]` (was `List[TC[_ <: T]]`).
- `misc/SelfInstance.scala`: `instance: C[Any]` (was `C[_]`).
- `misc/TypedMap.scala`: `K[Any]` at all type-arg positions (was `K[_]`). Restoring K[_] existential requires Scala 3 type-lambda refactor.
- `serialization/HasGenCodec.scala`: `wildcardCodec: GenCodec[C[Any]]` (was `C[_]`).

## Note for Plan 06

MIGRATION.md `## Will Not Migrate` section must list `commons-macros` with the rationale "pure Scala 2 macro infrastructure (`c.universe`, blackbox/whitebox); no Scala 3 analogue worth porting in v1.0 milestone — replaced with `= ???` stubs in commons-core preserving signatures."

## Next Phase Readiness

- `commons-core` compile-green on Scala 3 → all downstream modules (mongo, hocon, cbor, benchmark) can begin their Big-Bang stubbing waves (plans 03–05).
- 100 TODO tags act as a backlog for the eventual "implement Scala 3 derivation" wave (likely v1.1+ milestone).
- No blockers for plans 03/04/05.

## Self-Check: PASSED

Verified:
- `macros/` directory deleted (FS check + `git status` clean).
- All 6 task commits exist on `01-big-bang` branch (verified via `git log`).
- `commons-core/compile` exit 0 (verified).
- `scalafmtCheckAll` exit 0 (verified).
- 100 TODO[scala3-port] tags in core (verified via `git grep -cE`).
- 0 new `@nowarn`/`-Wconf` (verified via `git diff upstream/scala-3..HEAD | grep -cE`).
- No `.planning/` in any of the 6 commits' diffs (verified).
- No GSD nomenclature in commit messages (verified).

---
*Phase: 01-big-bang-comment-and-green*
*Plan: 02*
*Completed: 2026-06-01*
