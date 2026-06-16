---
phase: 05-leaf-feature-restoration
plan: 01-bidirectional-deprecate
subsystem: core/misc
tags: [deprecate-over-restore, leaf-feature, compiletime-error]
requirements_completed: [BIDIRECTIONAL-01]
dependency_graph:
  requires: [04-05-meta-annotations (base branch)]
  provides: ["misc.Bidirectional deprecated stub (callers fail at compile time)"]
  affects: []
tech_stack:
  added: []
  patterns:
    - "deprecate-over-restore via @deprecated + scala.compiletime.error inline body"
    - "test DROPPED-wrap: comment body, keep empty class for package layout"
key_files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala
    - core/src/test/scala/com/avsystem/commons/misc/BidirectionalTest.scala
    - MIGRATION.md
decisions:
  - "Wrapped BidirectionalTest with /* @TodoScala3Migration DROPPED: ... */ (fork removed the file outright; we preserve the file with an empty class to keep package layout — file deletion can come in a later mechanical sweep)"
  - "Body uses scala.compiletime.error (not ???) so callers fail at COMPILE time, satisfying BIDIRECTIONAL-01 fail-fast contract"
  - "Branch cut off 04-05-meta-annotations (Phase 4 tip @ f04cec6f) — independent of slice 5.0 (no MiscMacros dependency)"
metrics:
  duration_min: 2
  completed: 2026-06-02T07:54Z
  tasks_total: 3
  tasks_executed: 2
  checkpoints: 1
  files_changed: 3
---

# Phase 05 Plan 01: Bidirectional Deprecate Summary

Deprecated `misc/Bidirectional` via `@deprecated` object + `inline def apply` body of `scala.compiletime.error(...)` — every call site now fails at compile time with a migration message. Verbatim port from fork (modulo local scalafmt of scaladoc block + trailing comma); 17 LOC → 16 LOC after local formatter.

## What Shipped

- **`core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala`** (modified): Phase-1 `def apply ... = ???` stub replaced by `@deprecated(...,  since = "3.0.0")` object whose `inline def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A])` body is `scala.compiletime.error("…has not been ported to Scala 3. Write the reversed PartialFunction manually.")`. Matches `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala` modulo local scalafmt.
- **`core/src/test/scala/com/avsystem/commons/misc/BidirectionalTest.scala`** (modified): Test bodies wrapped in `/* @TodoScala3Migration DROPPED: ... */`. The previous `Bidirectional[Int,String] { … }` call sites would have hit the new `compiletime.error`; per fork pattern (fork removed the file entirely) we keep an empty `class BidirectionalTest extends AnyWordSpec with Matchers` to preserve package layout.
- **`MIGRATION.md`** (modified): Two edits in one commit — §1 (Will Not Migrate) gained a `misc/Bidirectional` row documenting the deprecate-over-restore decision and the `compiletime.error` fail-fast contract; backlog table lost the stale `Bidirectional.scala:6 apply (Scala 2 macro def)` TODO row per [[feedback_migration_md_contract]] (entry resolved).

## Commits

| #   | Hash       | Subject                                                                    | Files                                                    |
| --- | ---------- | -------------------------------------------------------------------------- | -------------------------------------------------------- |
| 1   | `c2c8d6fb` | feat(scala-3,core): deprecate Bidirectional (compiletime.error body)       | Bidirectional.scala (+ BidirectionalTest.scala DROPPED-wrap) |
| 2   | `5a9ddcab` | docs(migration): record Bidirectional deprecation                          | MIGRATION.md                                             |

Branch tip: `5a9ddcab` on `05-01-bidirectional-deprecate`, pushed to `origin/05-01-bidirectional-deprecate`. PR NOT opened (orchestrator override — batch PR creation under user supervision later).

## Acceptance Gates

- `grep -q '@deprecated' core/.../Bidirectional.scala` ✅
- `grep -q 'since = "3.0.0"' core/.../Bidirectional.scala` ✅
- `grep -q 'scala.compiletime.error' core/.../Bidirectional.scala` ✅
- `! grep -q '???' core/.../Bidirectional.scala` ✅ (no `???` body)
- `wc -l core/.../Bidirectional.scala` = 16 (range 15-20) ✅
- `diff origin/master:.../Bidirectional.scala vs ours` shows only scalafmt-cosmetic diffs (scaladoc-asterisk indent + trailing comma) ✅
- `sbt commons-core/compile` exit 0 ✅
- `sbt scalafmtCheckAll` exit 0 ✅
- `grep -q 'Bidirectional' MIGRATION.md` ✅ (1 hit, §1 row)
- `! grep -qE 'TODO\[scala3-port\].*Bidirectional' MIGRATION.md` ✅ (backlog row removed)

Branch hygiene against `04-05-meta-annotations`:
- 2 commits, both Conventional Commits ✅
- Only `Bidirectional.scala` + `BidirectionalTest.scala` + `MIGRATION.md` modified ✅
- 0 `.planning/` or GSD nomenclature in commit messages ✅
- 0 new `@nowarn` / `-Wconf` lines vs base ✅

## Deviations from Plan

**1. [Rule 3 - Blocker] scalafmt-driven byte diff against fork**
- **Found during:** Task 1 verification (`sbt scalafmtCheckAll`)
- **Issue:** Verbatim port from `origin/master` failed local scalafmt — fork uses a different scaladoc indentation/comma style.
- **Fix:** Ran `sbt commons-core/scalafmt` to apply our dialect. Plan acceptance criterion already allows "byte-identical (modulo scalafmt)" — preserved per minimum-diff intent. Diff against fork is purely cosmetic (scaladoc asterisk alignment + one trailing comma).
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala`
- **Commit:** included in `c2c8d6fb`

**2. [Rule 2 - Critical functionality] Wrapped BidirectionalTest call sites**
- **Found during:** Task 1 caller audit
- **Issue:** Phase 1 big-bang did NOT wrap `BidirectionalTest.scala`; lines 10 and 26 still called `Bidirectional[Int,String] { … }` against the `def apply = ???` stub (which compiled because the body type-checks under `???`). The new `inline def` with `scala.compiletime.error` would have failed `Test/compile`.
- **Fix:** Wrapped class body in `/* @TodoScala3Migration DROPPED: ... */` block, kept empty class to preserve package layout (fork removed the file entirely; minimum-diff vs Phase 1 keeps the file).
- **Files modified:** `core/src/test/scala/com/avsystem/commons/misc/BidirectionalTest.scala`
- **Commit:** bundled into `c2c8d6fb` per plan Task 1 action point 4

**3. [Orchestrator Override] PR creation skipped**
- Plan Task 3 called for `gh pr create --draft …`; orchestrator override said push only, no PR. Branch pushed to `origin/05-01-bidirectional-deprecate`. PR opening deferred to a later batched user-supervised step.

## Authentication Gates

None.

## Deferred Issues

None — plan completed within scope.

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` exists ✅
- `core/src/test/scala/com/avsystem/commons/misc/BidirectionalTest.scala` exists ✅
- `MIGRATION.md` exists ✅
- Commit `c2c8d6fb` in git log ✅
- Commit `5a9ddcab` in git log ✅
- Branch `05-01-bidirectional-deprecate` pushed to origin ✅
