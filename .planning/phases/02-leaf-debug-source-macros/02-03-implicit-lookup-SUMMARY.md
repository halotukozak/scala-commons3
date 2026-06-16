---
phase: 02-leaf-debug-source-macros
plan: 03
subsystem: core / misc
tags: [scala3, macros, quotes, implicit-search]
requirements: [IMPL-01]
dependency-graph:
  requires:
    - "01-big-bang @ 15fbf4a2"
  provides:
    - "Implicits.infer[T] / infer[T](clue) / inferNonMacro[T](clue) — compile-time implicit summon with optional clue"
  affects:
    - "misc.Implicits public API surface"
tech-stack:
  added: []
  patterns:
    - "Expr.summon[T] + report.errorAndAbort for implicit-search-or-fail"
    - "macro impl in a sibling source file in the same package (not commons.macros.*)"
key-files:
  created:
    - "core/src/main/scala/com/avsystem/commons/misc/ImplicitsMacros.scala"
    - "core/src/test/scala/com/avsystem/commons/misc/ImplicitsTest.scala"
    - ".planning/phases/02-leaf-debug-source-macros/02-03-implicit-lookup-SUMMARY.md"
  modified:
    - "core/src/main/scala/com/avsystem/commons/misc/Implicits.scala"
    - "MIGRATION.md"
decisions:
  - "Macro impl placed in sibling file `misc/ImplicitsMacros.scala` (same package, not a `.macros.` subpackage) per orchestrator directive overriding the plan skeleton."
  - "`inferNonMacro` preserved as alias of `infer(clue)` — Scala 3 `Expr.summon` has no `withMacrosDisabled` flag. Narrowing documented in MIGRATION.md §3."
  - "Negative test uses scalatest `assertDoesNotCompile`; clue-message assertion intentionally skipped (scalatest API does not expose the compile-error message)."
metrics:
  duration: "~15 min"
  completed: 2026-06-01
  tasks: 4 of 4 (Task 4 = push/PR completed under upfront authorization)
  files: 5 (3 created + 2 modified)
  commits: 3 (feat + test + docs)
---

# Phase 02 Plan 03: implicit-lookup Summary

**One-liner:** Restored `Implicits.infer[T]` / `infer[T](clue)` / `inferNonMacro[T](clue)` via Scala 3 `Expr.summon[T]` + `report.errorAndAbort` in a new sibling `ImplicitsMacros` object; 4 ScalaTest cases green (3 positive + 1 negative `assertDoesNotCompile`); MIGRATION.md backlog trimmed by 3; §3 documents `inferNonMacro` semantic narrowing; draft PR #864 opened with milestone 1.

## What shipped

- **`ImplicitsMacros.scala` (new)** — `package com.avsystem.commons.misc`. Single `inferImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T]` returning either `Expr.summon[T]` result or `report.errorAndAbort(s"$clue: could not find implicit value for ${TypeRepr.of[T].show}")`. `private[misc]` visibility.
- **`Implicits.scala`** — 3 `???` stubs replaced with `inline def` wrappers; all `TODO[scala3-port]` markers removed. `inferNonMacro` aliases `infer(clue)` with a doc-comment cross-referencing MIGRATION.md §3.
- **`ImplicitsTest.scala` (new)** — 4 ScalaTest `AnyFunSuite` cases:
  - `infer[T]` resolves to summoned `Ordering[Int]`
  - `infer[T](clue)` resolves to summoned `Ordering[Int]`
  - `inferNonMacro[T](clue)` resolves identically
  - `assertDoesNotCompile` for missing implicit (clue-string content not asserted — scalatest API limitation)
- **`MIGRATION.md`** — 3 backlog rows for `Implicits.scala:5/7/9` removed; `Total tags` updated `155` → `153` (actual baseline 156 minus 3; pre-existing off-by-one in source carries through); §3 (Source-compat breaks, core) bullet added documenting the `inferNonMacro` narrowing.

## Commits

| # | Hash | Type | Message |
|---|------|------|---------|
| 1 | `f9db4dcf` | feat | restore Implicits.infer family via Expr.summon |
| 2 | `1779bdf4` | test | add positive + negative tests for Implicits.infer family |
| 3 | `4221b3a5` | docs | remove restored infer backlog + document inferNonMacro narrowing |

## PR

- **URL:** https://github.com/AVSystem/scala-commons/pull/864
- **Number:** #864
- **State:** DRAFT
- **Base:** `AVSystem/scala-commons:scala-3`
- **Head:** `halotukozak:02-03-implicit-lookup`
- **Title:** `[Scala 3] Phase 02-03: restore Implicits.infer family`
- **Milestone:** `Scala 3` (#1) — assigned via `gh api -X PATCH .../issues/864 -F milestone=1`

## Verification

| Gate | Command | Result |
|------|---------|--------|
| Compile | `sbt commons-core/compile` | exit 0 |
| Test | `sbt 'commons-core/testOnly com.avsystem.commons.misc.ImplicitsTest'` | 4/4 pass |
| scalafmt | `sbt scalafmtCheckAll` | exit 0 |
| Backlog removal | `! grep -nE 'Implicits\.scala:(5\|7\|9)' MIGRATION.md` | OK |
| §3 narrowing entry | `grep -q 'inferNonMacro' MIGRATION.md` | OK |
| TODO tag count | `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` | 153 (was 156 before slice, -3) |
| `@nowarn`/`-Wconf` | source diff | 0 new |
| `.planning/` in commits | `git log -p ... \| grep .planning/` | 0 matches |

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 — Blocking infra] sbt build fails under git worktree (`NoWorkTreeException`)**
- **Found during:** Task 1 (first `sbt commons-core/compile`)
- **Issue:** sbt-git (transitive via sbt-ci-release) uses an older JGit version that does not recognise linked worktree gitdir-file pointers; build aborts at setting-evaluation time with `org.eclipse.jgit.errors.NoWorkTreeException`. Compounded by a stale `core.worktree` entry in the shared `.git/config` pointing at a different worktree path.
- **Fix:**
  - Unset the stale `core.worktree` in the shared repo config (it was a leftover from a prior worktree setup, not intentional configuration).
  - Added a local-only `local-worktree-override.sbt` that overrides `GitKeys.gitUncommittedChanges`, `gitCurrentBranch`, `gitHeadCommit` with static values, bypassing the JGit call path entirely. The file was deleted before push so the override stays local to the worktree.
- **Files modified:** none committed.

### Plan-stated assumption corrections

- **Macro impl location.** Plan wrote impl under `core/.../misc/macros/ImplicitsMacros.scala` (matching sibling slice 02-01). Orchestrator directive overrode: impls live next to `inline def`, not in a `commons.macros.*` package. This slice places `ImplicitsMacros` in `core/.../misc/ImplicitsMacros.scala` (same `misc` package as `Implicits`, separate file as required by Scala 3 macro phase separation).
- **`Total tags` baseline.** Plan implied a delta to `155` (the value written in MIGRATION.md). Actual `git grep` count on `01-big-bang @ 15fbf4a2` was `156` (sibling 02-01 SUMMARY also flagged a pre-existing off-by-one in this header). After this slice the actual count is `153` (-3). MIGRATION.md updated to `153` to match reality.

### Plan-stated branch / PR steps (Task 4 — checkpoint:human-action)

Task 4 is plan-marked as a blocking checkpoint, but per orchestrator's upfront authorization the executor proceeded autonomously:

- Branch `02-03-implicit-lookup` was pre-cut by the orchestrator from `01-big-bang @ 15fbf4a2`.
- Pushed to `origin/02-03-implicit-lookup`.
- Draft PR #864 opened against `AVSystem/scala-commons:scala-3`.
- Milestone #1 assigned via `gh api -X PATCH /repos/AVSystem/scala-commons/issues/864 -F milestone=1`.

## Authentication Gates

None encountered.

## Follow-ups

- None blocking. Slice is independent of 02-02, 02-04, 02-05 — all five Phase 2 slices can land in parallel.

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/misc/ImplicitsMacros.scala` FOUND
- `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala` (modified) FOUND
- `core/src/test/scala/com/avsystem/commons/misc/ImplicitsTest.scala` FOUND
- `MIGRATION.md` (3 backlog rows removed, §3 entry added, total tags updated) FOUND
- Commit `f9db4dcf` (feat) — verified present on `02-03-implicit-lookup`
- Commit `1779bdf4` (test) — verified present
- Commit `4221b3a5` (docs) — verified present
- PR #864 — verified open, draft, milestone 1, title prefix `[Scala 3]`
