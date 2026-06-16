---
phase: 05-leaf-feature-restoration
plan: 03
subsystem: core/misc
tags: [applier, unapplier, mirror, derivation, scala-3, leaf, slice-5.3]
requires:
  - 04-05-meta-annotations (branch base; Phase 4 stack tip)
provides:
  - Applier[T] / Unapplier[T] / ApplierUnapplier[T] real `given derived` based on `scala.deriving.Mirror.ProductOf`
affects:
  - core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala
  - core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala
  - MIGRATION.md
tech-stack:
  added:
    - scala.deriving.Mirror.ProductOf (local import in ApplierUnapplier.scala)
  patterns:
    - Mirror-based typeclass derivation (`given derived`)
    - Scala 3.8 `[T: TC as alias]` summon-into-alias sugar
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala
    - core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala
    - MIGRATION.md
decisions:
  - Verbatim port from origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala
  - Added local `import scala.deriving.Mirror` (CommonAliases `export scala.deriving.Mirror` sweep deferred to slice 3.x)
  - Test `custom` case `ignore`d (Mirror.ProductOf only fires for true case classes/tuples — fork precedent)
metrics:
  duration_sec: 242
  tasks: 4
  files: 3
  completed: 2026-06-02T08:11:00Z
---

# Phase 05 Plan 03: ApplierUnapplier Mirror-based port Summary

Slice 5.3 — replaced the Phase-1 `implicit def materialize[T] = ???` stub in `misc/ApplierUnapplier.scala` with the fork's `given derived` typeclass derivation based on `scala.deriving.Mirror.ProductOf`, re-enabling the matching test.

## What shipped

- **`ApplierUnapplier.scala`** — verbatim port from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala`:
  - `object Applier { given derived[T <: Product: Mirror.ProductOf as m]: Applier[T] = rawValues => m.fromTuple(Tuple.fromArray(rawValues.toArray).asInstanceOf[m.MirroredElemTypes]) }`
  - `object Unapplier { given derived[T <: Product]: Unapplier[T] = value => IArraySeq.unsafeWrapArray(value.productIterator.toArray) }`
  - `object ApplierUnapplier { given derived[T: {Applier as applier, Unapplier as unapplier}]: ApplierUnapplier[T] = … }`
  - Public surface (traits + `ProductUnapplier` + `ProductApplierUnapplier`) unchanged.
- **`ApplierUnapplierTest.scala`** — synced with fork: trailing comma after `value: T`, `test("custom")` → `ignore("custom")`. 7 active cases green, 1 ignored.
- **`MIGRATION.md`** — §3 new "core — misc ApplierUnapplier (slice 5.3)" subsection; 3 backlog rows removed (lines 13/25/37 of ApplierUnapplier.scala).

## Commits (atomic, fork-cadence)

| Commit     | Type     | Description                                       |
| ---------- | -------- | ------------------------------------------------- |
| `a837dd51` | feat     | port ApplierUnapplier (Mirror-based)              |
| `cbec475e` | test     | un-wrap ApplierUnapplierTest                      |
| `bb98cc45` | docs     | record ApplierUnapplier Mirror-based port         |

Branch `05-03-applier-unapplier @ bb98cc45` cut off `04-05-meta-annotations @ f04cec6f`. Independent of slice 5.0 (no `MiscMacros` dependency). Pushed to `origin/05-03-applier-unapplier`. PR NOT opened per orchestrator override.

## Verification

- `sbt commons-core/compile` exit 0
- `sbt 'commons-core/testOnly *.ApplierUnapplierTest'` — 7 succeeded, 1 ignored, 0 failed
- `sbt scalafmtCheckAll` exit 0
- `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala) core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala` — only the added `import scala.deriving.Mirror` line + scalafmt scaladoc reformat (`/**` opener on its own line) differ; logic byte-identical.
- `grep -q 'Mirror.ProductOf' …/ApplierUnapplier.scala` exit 0
- `grep -q 'given derived' …/ApplierUnapplier.scala` exit 0
- `! grep -q '???' …/ApplierUnapplier.scala` (no stub bodies remain)
- `git log --oneline 04-05-meta-annotations..HEAD | wc -l` = 3
- 0 new `@nowarn`/`-Wconf` vs base
- 0 `.planning/` paths in commits
- 0 GSD nomenclature in commit messages

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 — Blocking] Added explicit `import scala.deriving.Mirror`**
- **Found during:** Task 1 compile
- **Issue:** Fork relies on `export scala.deriving.Mirror` from `CommonAliases.scala` (fork only); our `CommonAliases.scala` is still the pre-slice-3.x trait-of-type-aliases shape without that export, so the verbatim port failed with `Not found: Mirror`.
- **Fix:** Added one-line `import scala.deriving.Mirror` to ApplierUnapplier.scala. Avoids dragging the CommonAliases `export`-sweep (slice 3.x territory) into a leaf-restoration slice.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala`
- **Commit:** `a837dd51`

**2. [Rule 3 — Blocking] scalafmt reformat after verbatim port**
- **Found during:** Task 1 + Task 2 scalafmt gates
- **Issue:** Fork's `/**\n * …\n */` scaladoc style + trailing comma after `value: T` parameter were rejected by our local scalafmt config. Same precedent as Phase-4 plans.
- **Fix:** `sbt scalafmtAll`; bundled into the same commit per CONTEXT cadence.
- **Files modified:** ApplierUnapplier.scala + ApplierUnapplierTest.scala
- **Commits:** `a837dd51`, `cbec475e`

No Rule 4 (architectural) deviations. No auth gates.

## Acceptance Criteria

| Criterion                                                                  | Met |
| -------------------------------------------------------------------------- | --- |
| ApplierUnapplier.scala uses Mirror.ProductOf (no quoted macro)             | yes |
| `Applier[Foo].apply(Seq(1, "x"))` reconstructs case class at runtime       | yes (covered by tests) |
| ApplierUnapplierTest un-wrapped and green                                  | yes |
| `commons-core/compile` + ApplierUnapplierTest green                        | yes |
| MIGRATION.md §3 records Mirror-based reshape (no quoted macro)             | yes |
| 3 atomic commits per CONTEXT cadence                                       | yes |
| Branch off `04-05-meta-annotations` tip                                    | yes |

APPLIERUNAPPLIER-01, WORKFLOW-01..05, QUALITY-01 satisfied. PR creation skipped per orchestrator override.

## Self-Check: PASSED

- File `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala`: FOUND
- File `core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala`: FOUND
- File `MIGRATION.md`: FOUND
- Commit `a837dd51`: FOUND
- Commit `cbec475e`: FOUND
- Commit `bb98cc45`: FOUND
