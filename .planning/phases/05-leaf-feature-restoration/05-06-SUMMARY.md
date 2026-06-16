---
phase: 05-leaf-feature-restoration
plan: 06
subsystem: core/misc
tags: [scala-3, sealed-enum, mirror, inline-derivation, compat-shift]
requires: [04-05-meta-annotations]
provides:
  - "SealedUtils.instancesFor[TC, T: Mirror.SumOf]"
  - "SealedUtils.caseObjects[T: Mirror.SumOf]"
  - "SealedEnumCompanion.caseObjects (inline)"
  - "NamedEnumCompanion.given GenKeyCodec / GenCodec"
  - "OrderedEnum.given Ordering"
affects:
  - "core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala"
  - "core/src/test/scala/com/avsystem/commons/misc/SealedEnumTest.scala"
  - "core/src/test/scala/com/avsystem/commons/misc/NamedEnumTest.scala"
  - "core/src/test/scala/com/avsystem/commons/rpc/Tag.scala"
  - "MIGRATION.md"
tech-stack:
  added:
    - "scala.compiletime.{summonAll, summonFrom, erasedValue}"
    - "scala.deriving.Mirror.SumOf"
    - "scala.ValueOf (case-object summoning)"
  patterns:
    - "Pure-inline tuple recursion over Mirror.MirroredElemTypes"
    - "summonFrom with ValueOf | Mirror.SumOf | _ tri-branch"
key-files:
  created: []
  modified:
    - "core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala"
    - "core/src/test/scala/com/avsystem/commons/misc/SealedEnumTest.scala"
    - "core/src/test/scala/com/avsystem/commons/rpc/Tag.scala"
    - "MIGRATION.md"
decisions:
  - "Kept `given evidence: this.type = this` commented out (fork shape) — uncommenting triggers Mirror.Sum self-type clash on every SealedEnumCompanion subclass"
  - "SealedEnumCompanion.values widened from lazy val (current) to def (fork shape); subclasses with `override lazy val values` still compile"
  - "Used `GenCodec.nullableSimple` instead of fork's `createSimple` (T <: NamedEnum extends Serializable is AnyRef so nullableSimple is the correct overload)"
  - "Added explicit imports for scala.compiletime + scala.deriving.Mirror since CommonAliases.scala doesn't yet re-export them"
  - "Provided explicit SourceInfo instances per case object in SealedEnumTest to work around the Scala-2-macro stub of SourceInfo.here (Phase-6 restoration)"
  - "rpc/Tag.scala caller switched from named `codec` member to `summon[GenCodec[Tag[?]]]` — required by new anonymous given in NamedEnumCompanion"
  - "Compat traits (OrderedEnumCompat, NamedEnumCompanionCompat) NOT ported in this slice — own future slice once the rest of fork compat.scala is batched"
metrics:
  duration: "~10 min"
  tasks_completed: 3
  files_modified: 4
  commits: 3
  tests_passing: "6/6 (SealedEnumTest + NamedEnumTest)"
  completed: "2026-06-02"
---

# Phase 5 Plan 6: SealedUtils Port (Pure Inline + Mirror.SumOf) Summary

Ported `SealedUtils.scala` verbatim from fork (with explicit imports added for `scala.compiletime` and `scala.deriving.Mirror`), replacing three Scala-2-macro `???` stubs (`caseObjectsFor`, `caseObjects`, `instancesFor`) with pure-inline derivation over `Mirror.SumOf.MirroredElemTypes` using `compiletime.{summonAll, summonFrom, erasedValue}` + `scala.ValueOf`. `caseObjectsFor` removed entirely (zero internal callers per pre-port audit). SealedEnumTest + NamedEnumTest un-wrapped and green (6/6).

## What Built

- `SealedUtils.instancesFor[TC[_], T: Mirror.SumOf]` — compile-time typeclass-instance summoning via `summonAll[Tuple.Map[m.MirroredElemTypes, TC]]`.
- `SealedUtils.caseObjects[T: Mirror.SumOf]` — recursive tuple walk via `inline private def collectCaseObjects[T, Tup <: Tuple]` using `summonFrom` to pick between `ValueOf` (case object), `Mirror.SumOf` (nested sum), or `_` (skip).
- `SealedEnumCompanion.caseObjects` — `inline protected def` delegating to `SealedUtils.caseObjects[T]`.
- `NamedEnumCompanion` — anonymous `given GenKeyCodec[T]` + `given GenCodec[T]` (was `implicit lazy val keyCodec` / `codec`).
- `OrderedEnum.ordering` — anonymous `given [T <: OrderedEnum] => Ordering[T]` (was `implicit def`).
- Test changes: `SealedEnumTest` provides explicit `SourceInfo` instances per case object (SourceInfo.here stub deferred to Phase 6); `rpc/Tag.scala` switched from named-codec member to `summon[GenCodec[Tag[?]]]`.

## Commits

- `ecebf6cb feat(scala-3,core): port SealedUtils (pure inline + Mirror.SumOf)`
- `e012eea8 test(scala-3,core): un-wrap SealedEnumTest + NamedEnumTest`
- `08980368 docs(migration): record SealedUtils port + caseObjectsFor removal`

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 — Bug] GenCodec.createSimple wrong signature**
- **Found during:** Task 1 (initial verbatim port)
- **Issue:** Fork uses `GenCodec.createSimple(read, write)` (2-arg) but our current `createSimple` mandates a third `allowNull: Boolean` argument; signatures diverged from fork.
- **Fix:** Switched to `GenCodec.nullableSimple[T <: AnyRef]` — semantically equivalent for `T <: NamedEnum extends Serializable` (AnyRef), preserves prior behavior.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala`
- **Commit:** `ecebf6cb`

**2. [Rule 3 — Blocking] `given evidence: this.type = this` triggers Mirror.Sum self-type clash**
- **Found during:** Task 2 (test compile)
- **Issue:** With `given evidence: this.type = this` uncommented, every companion object extending `SealedEnumCompanion[T]` errors with E058 `illegal inheritance: self type X.type does not conform to self type scala.deriving.Mirror.Sum`. Root cause: the companion is auto-derived as `Mirror.Sum` AND simultaneously provides a same-type self-given, which clashes with `Mirror.SumOf[T]`'s self-type bound.
- **Fix:** Kept the line commented (matches fork — verified against `origin/master:core/src/main/scala-3/.../SealedUtils.scala` which has the same line commented).
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala`
- **Commit:** `e012eea8`

**3. [Rule 3 — Blocking] rpc/Tag.scala references removed `codec` member**
- **Found during:** Task 2 (test compile)
- **Issue:** `core/src/test/scala/com/avsystem/commons/rpc/Tag.scala:19` referenced `codec` — the old named `implicit lazy val codec` member of `NamedEnumCompanion`. New shape has anonymous `given GenCodec[T]`, so the member name is gone.
- **Fix:** Replaced `codec.asInstanceOf[GenCodec[Tag[T]]]` with `summon[GenCodec[Tag[?]]].asInstanceOf[GenCodec[Tag[T]]]`.
- **Files modified:** `core/src/test/scala/com/avsystem/commons/rpc/Tag.scala`
- **Commit:** `e012eea8`

**4. [Rule 3 — Blocking] SealedEnumTest fails because SourceInfo.here is still a Phase-6 stub**
- **Found during:** Task 2 (test runtime)
- **Issue:** `OrderedEnum`'s `SomeEnum(implicit val sourceInfo: SourceInfo)` triggers `SourceInfo.here = ???` at object initialization (every case object), failing both SealedEnumTest tests.
- **Fix:** Provided four explicit `SourceInfo` instances (`si1..si4` with monotonically increasing offsets) per case object, preserving the `OrderedEnum` declaration-order semantics while side-stepping the macro stub. Fork ships this same test but only because fork has the SourceInfo macro implemented (Phase 6 in our backlog).
- **Files modified:** `core/src/test/scala/com/avsystem/commons/misc/SealedEnumTest.scala`
- **Commit:** `e012eea8`

### Intentional Divergences from Plan

- **No PR opened.** Override clause in execution prompt: "Do NOT open a GitHub PR. Push branch to `origin`, skip `gh pr create`." Branch pushed to `origin/05-06-sealed-utils` only.
- **Compat traits NOT ported.** The fork's `SealedUtils.scala` references `extends OrderedEnumCompat` and `with NamedEnumCompanionCompat[T]`. Those traits live in fork's `compat.scala` (pure `@deprecated` wrappers). Porting `compat.scala` is its own slice; we omit the `extends`/`with` here.
- **`SealedEnumCompanion.values` shape.** Plan flagged Open Question 4 (lazy val vs def). Resolved to use fork's `def values: ISeq[T]`; existing subclasses with `override lazy val values: ... = caseObjects` (3 in core, 1 in hocon) continue to compile because Scala 3 allows lazy-val override of `def`. No subclass changes needed.

## Acceptance Criteria Status

- [x] `! git grep -nE 'caseObjectsFor' -- '*.scala'` — zero hits
- [x] `compiletime.summonAll` present in SealedUtils.scala
- [x] `Mirror.SumOf` present in SealedUtils.scala
- [x] `scala.ValueOf` present (via `scala.ValueOf` in `summonFrom`)
- [x] no `???` in SealedUtils.scala
- [x] `sbt commons-core/compile` exit 0
- [x] `sbt scalafmtCheckAll` exit 0
- [x] `sbt 'commons-core/testOnly *.SealedEnumTest *.NamedEnumTest'` exit 0, 6/6 passing
- [x] MIGRATION.md slice 5.6 entry + §1 caseObjectsFor row + backlog rows removed
- [x] 3 atomic commits (feat + test + docs) per plan
- [x] Branch pushed to `origin/05-06-sealed-utils`

## Self-Check: PASSED

- Files exist: SealedUtils.scala, SealedEnumTest.scala, NamedEnumTest.scala, Tag.scala, MIGRATION.md, 05-06-SUMMARY.md
- Commits exist: ecebf6cb, e012eea8, 08980368
- Branch pushed to origin: `origin/05-06-sealed-utils`
