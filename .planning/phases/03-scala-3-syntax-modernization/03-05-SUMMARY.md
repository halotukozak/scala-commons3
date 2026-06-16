---
phase: 03-scala-3-syntax-modernization
plan: 05
subsystem: core/misc
tags: [scala-3, refactor, delete, summon, ImplicitNotFound]
requires: []
provides:
  - "com.avsystem.commons.misc.ImplicitNotFound moved to own file (sealed trait + companion preserved)"
affects:
  - "com.avsystem.commons.misc.Implicits object removed — downstream callers must switch to scala.compiletime.summon[T]"
tech_stack:
  added: []
  patterns: ["use scala.compiletime.summon[T] over project-internal infer helpers"]
key_files:
  created:
    - "core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala"
    - "MIGRATION.md (§1 entry added — no new file, extended)"
  modified:
    - "MIGRATION.md"
  deleted:
    - "core/src/main/scala/com/avsystem/commons/misc/Implicits.scala"
decisions:
  - "Delete outright (not deprecate) per user directive 2026-06-01 — overrides fork commit 50272b26"
  - "Extract ImplicitNotFound to its own file before deletion to keep separable concerns separable"
metrics:
  duration_minutes: 6
  tasks_completed: 3
  files_touched: 3
  commits: 3
  completed: "2026-06-01"
---

# Phase 3 Plan 05: Delete Implicits Object Summary

One-liner: Deleted `com.avsystem.commons.misc.Implicits` object outright (0 callers; covered by Scala 3 `summon[T]`) and extracted the still-useful `ImplicitNotFound` sealed trait + companion to its own file.

## Outcome

- `Implicits.scala` deleted via `git rm`.
- `ImplicitNotFound.scala` created (sealed trait + companion preserved verbatim).
- `MIGRATION.md` §1 (Will not migrate) extended with `com.avsystem.commons.misc.Implicits` entry pointing to `scala.compiletime.summon[T]`.
- 3 atomic Conventional Commits, no squash, branched off `upstream/scala-3` tip (`0887d555`).
- Draft PR #867 opened at AVSystem/scala-commons, milestone "Scala 3" (#1), `[Scala 3]` prefix, body metadata block per slice-3.5 contract.

## Commits

| # | SHA | Subject |
|---|-----|---------|
| 1 | `9c653bcb` | `refactor(scala-3,core): extract ImplicitNotFound to its own file` |
| 2 | `699424c7` | `refactor(scala-3,core): delete Implicits object (covered by summon[T])` |
| 3 | `4091d42a` | `docs(migration): record Implicits object removal` |

## Gates Run

| Gate | Result |
|------|--------|
| `git grep -nE '\bImplicits\.' -- '*.scala'` | 0 hits |
| `git ls-files .../Implicits.scala \| wc -l` | 0 (deleted) |
| `git ls-files .../ImplicitNotFound.scala \| wc -l` | 1 (created) |
| `sbt 'compile ;Test/compile ;scalafmtCheckAll'` | exit 0 |
| No new `@nowarn` / `-Wconf` in diff | 0 matches |
| No `.planning/` in commit diffs | 0 matches |
| No GSD nomenclature in commit messages | 0 matches |

## PR

- **URL:** https://github.com/AVSystem/scala-commons/pull/867
- **State:** Draft (open) — user will flip ready manually
- **Base:** `AVSystem/scala-commons:scala-3`
- **Head:** `halotukozak:03-05-delete-implicits-object`
- **Title:** `[Scala 3] delete Implicits object (covered by summon[T])`
- **Milestone:** Scala 3 (#1)
- **Metadata block:** Slice 3.5 / Independent / Depends on: none / Base branch: upstream/scala-3

## Deviations from Plan

None — plan executed exactly as written. All 3 tasks completed in order; all acceptance criteria satisfied on first attempt.

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala` FOUND on disk and in HEAD.
- `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala` MISSING (correctly — deleted).
- All 3 commits (`9c653bcb`, `699424c7`, `4091d42a`) present in `git log upstream/scala-3..HEAD`.
- PR #867 verified: draft=true, milestone=1, title prefix `[Scala 3]`.
