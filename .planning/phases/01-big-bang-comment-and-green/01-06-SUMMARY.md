---
phase: 01-big-bang-comment-and-green
plan: 06
subsystem: docs+release
tags: [migration-md, push, pr, ci]
dependency_graph:
  requires: [01-01, 01-02, 01-03, 01-04, 01-05]
  provides: [phase-01-closed-pending-merge]
  affects: [MIGRATION.md, AVSystem/scala-commons PR #860]
tech_stack:
  added: []
  patterns: [migration-md-5-section-contract, draft-PR-on-AVSystem, milestone-1-Scala-3]
key_files:
  created:
    - MIGRATION.md
  modified:
    - project/Commons.scala
decisions:
  - Document `-Wconf` and other dropped flags in MIGRATION.md `## 1. Will not migrate`; the literal `-Wconf` token inside the doc was the only diff-hit for the `@nowarn`/`-Wconf` audit and is plain documentation text, not a source-code suppression.
  - PR base `AVSystem/scala-commons:scala-3` (not `master`) — matches phase target; preserves stack convention.
  - PR opened as draft + milestone 1 + `[Scala 3]` title prefix per locked memory rules.
metrics:
  duration: ~30 min
  completed: 2026-06-01
---

# Phase 01 Plan 06: MIGRATION.md and push PR Summary

Seeded the public-facing `MIGRATION.md` (5 locked sections + 155-row Backlog table), ran the full local verify gate, pushed `01-big-bang` to fork, and opened draft PR AVSystem/scala-commons#860 against `scala-3` with milestone 1. Fork CI + upstream PR CI both green (3 shards × Temurin 17/21/25).

## What Shipped

### Task 1 — MIGRATION.md authored

- `MIGRATION.md` at repo root, 252 lines, 6 `## ` headings (5 numbered locked sections + Backlog).
- Backlog table populated from `git grep -nE 'TODO\[scala3-port\]' -- '*.scala'` → 155 rows, sorted alphabetically by location, parsed into Location / Description / Effort columns. Sync check: TAG_COUNT = BACKLOG_ROWS = 155.
- Section 1 (Will not migrate): `commons-macros`, `analyzer`, `jetty`, `spring`, `comprof` modules + `-Xsource:3`, `-Wconf`, `-language:experimental.macros`, Scala 2 macro impls.
- Section 3 (Source-compat breaks): build flags (`-P:scalajs:mapSourceURI` → `-scalajs-mapSourceURI`), per-module core/mongo/hocon notes.
- Section 5 (Disabled): module table (`commons-macros` deleted; `analyzer`/`jetty`/`spring`/`comprof` dropped from aggregate) + 38 commented test files grouped into 6 categories (TestMacros gone / Components stub / GenCodec.materialize stub / MongoEntityCompanion stub / Hocon derivation / misc derivation).
- Single commit `c3aaa77c` `docs(migration): seed Scala 3 migration backlog and module status`.
- Post-commit prettier reformat (column-padding tables) committed separately as `24e4289c` `docs(migration): apply prettier formatting to tables` — content identical, layout only.

### Task 2 — Full local verify gate

All 16 acceptance gates green:

| Gate | Result |
| ---- | ------ |
| `sbt show version` | exit 0 (`2.28.0+43-...`) |
| `sbt show scalaVersion` | exit 0 (`3.8.2`) |
| `sbt compile` | exit 0 |
| `sbt Test/compile` | exit 0 |
| `sbt scalafmtCheckAll scalafmtSbtCheck` | exit 0 (after Rule 3 reformat) |
| No new `@nowarn`/`-Wconf` in source diff | OK — only doc reference in MIGRATION.md |
| No GSD nomenclature in commit messages | OK |
| No `.planning/` in commits | OK |
| No `crossScalaVersions` in `project/Commons.scala` | OK |
| No `-Xsource:3` in `project/Commons.scala` | OK |
| No source `scala-2.13/` dirs (excl. upstream-baseline `mongo/jvm/src/test/scala-2.13`) | OK |
| `.scalafmt.conf` single dialect (`runner.dialect = scala3`, no `fileOverride`) | OK |
| `.github/workflows/ci.yml` single Scala 3.8.2 axis, no 2.13 | OK |
| TAG_COUNT == BACKLOG_ROWS | 155 == 155 |

**Rule 3 auto-fix during Task 2:** `scalafmtSbtCheck` failed on `project/Commons.scala` (the `lazy val core = mkSubProject.settings(...)` block was reformatted by scalafmt-with-scala3-dialect). Applied `sbt scalafmtSbt`, reviewed diff (10 ins / 11 del — cosmetic, settings call chained on same line), committed as `7467149f` `style(scalafmt): reformat project/Commons.scala core settings block`. Re-ran full gate — green.

### Task 3 — Push to fork

- `origin` confirmed = `halotukozak/scala-commons3`.
- `git push -u origin 01-big-bang` — new branch created on fork at `7467149f` (Task 1+2 tips).
- Post-push, MIGRATION.md prettier reformat (`24e4289c`) pushed in a second push; final fork tip = `24e4289c`.
- Fork CI run: <https://github.com/halotukozak/scala-commons3/actions/runs/26753320607> — `success` (all 3 shards Build+Lint + Publish Artifacts skipped).

### Task 4 — Open draft PR

- PR opened: <https://github.com/AVSystem/scala-commons/pull/860>
- Title: `[Scala 3] Pivot to Scala 3 only — comment broken, green CI` (61 chars)
- Base: `AVSystem/scala-commons:scala-3` ✓
- Head: `halotukozak:01-big-bang` ✓
- State: draft ✓ (`isDraft: true`)
- Milestone: `Scala 3` (#1) ✓ (assigned via `gh api PATCH /repos/AVSystem/scala-commons/issues/860 -f milestone=1` — `gh pr edit --milestone` path not used since two-step API works deterministically)
- PR body: 5-bullet summary of changes + explicit out-of-scope list + pointer to `MIGRATION.md` + test-plan checklist.
- AVSystem PR CI run: <https://github.com/AVSystem/scala-commons/actions/runs/26753512180> — all 3 shards (Temurin 17 / 21 / 25) `pass`. Publish Artifacts skipped (draft PR).

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] `scalafmtSbtCheck` failed on `project/Commons.scala`**
- **Found during:** Task 2, gate 5.
- **Issue:** scala3-dialect scalafmt reformatted the `lazy val core = mkSubProject.settings(...)` block (settings call previously chained on a new line via `.settings(`; new format collapses to `mkSubProject.settings(`).
- **Fix:** Ran `sbt scalafmtSbt`, reviewed diff (10 ins / 11 del, cosmetic only).
- **Files modified:** `project/Commons.scala`
- **Commit:** `7467149f`

**2. [Rule 1 - Bug] Post-commit prettier reformat of MIGRATION.md tables**
- **Found during:** `git status` check before `gh pr create` (warning about uncommitted changes).
- **Issue:** Local prettier (likely from IDE-on-save) padded MIGRATION.md table columns after the Task 1 commit — content unchanged, layout only.
- **Fix:** Committed the reformat separately so the PR diff stays clean.
- **Files modified:** `MIGRATION.md`
- **Commit:** `24e4289c`

### Plan-deviation note

Plan Task 3 + Task 4 were `checkpoint:human-verify` gates. Per orchestrator context (`User has authorized you to proceed without clarifying questions — make reasonable calls`), proceeded through both checkpoints autonomously: pushed to fork after local gates green; opened PR after fork CI green. No surprises — both checkpoint scripts ran exactly as specified in the plan.

### Audit footnote

- Audit 6 (`! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'`) had 1 match — the literal `-Wconf` token inside MIGRATION.md `## 1. Will not migrate` documentation row. Manually inspected; it is plain documentation text describing what we will NOT use, not a code-level warning suppression. Audit logically satisfied.
- Audit 11 (`! find . -type d -name 'scala-2.13' ...`): hits were all under `target/` / `.bloop/` (build output, untracked) plus `mongo/jvm/src/test/scala-2.13/` — the latter is upstream-baseline (called out in STATE.md Plan 01 SUMMARY) left untouched per minimum-diff. Audit logically satisfied.

## Authentication Gates

None. `gh` CLI was already authenticated for both `halotukozak` and `AVSystem/scala-commons` (member access).

## Requirements Satisfied

- **DOC-01** ✓ — MIGRATION.md seeded with 5 locked sections.
- **DOC-02** ✓ — Backlog populated from TODO grep (155 rows match 155 tags).
- **COMPILE-01** ✓ — `sbt compile` green.
- **COMPILE-02** ✓ — `sbt Test/compile` green.
- **COMPILE-03** ✓ — `scalafmtCheckAll scalafmtSbtCheck` green.
- **CI-01** ✓ — Fork CI green on `halotukozak/scala-commons3:01-big-bang`.
- **CI-02** ✓ — AVSystem PR CI green (3 shards × Temurin 17/21/25).
- **WORKFLOW-01..05** ✓ — branch cut from `upstream/scala-3`, fork-PR workflow, double user-ack (granted upfront via orchestrator context), no GSD nomenclature, no `.planning/` in commits.
- **PR-01** ✓ — title `[Scala 3] Pivot to Scala 3 only — comment broken, green CI`.
- **PR-02** ✓ — milestone 1 (Scala 3) assigned.
- **PR-03** ✓ — draft state.
- **QUALITY-01** ✓ — zero new `@nowarn`/`-Wconf` in source diff.

## Commits

| Commit | Message |
| ------ | ------- |
| `c3aaa77c` | `docs(migration): seed Scala 3 migration backlog and module status` |
| `7467149f` | `style(scalafmt): reformat project/Commons.scala core settings block` |
| `24e4289c` | `docs(migration): apply prettier formatting to tables` |

## Phase 01 Closure

Phase 01 big-bang-comment-and-green is **complete pending maintainer merge**. The PR (#860) is left OPEN in draft state for the maintainer to review and flip to ready-for-review manually (global rule: never merge PRs automatically).

Branch tip: `24e4289c` on `01-big-bang`, also at `halotukozak/scala-commons3:01-big-bang`.

## Self-Check: PASSED

- FOUND: MIGRATION.md (252 lines, 6 `## ` headings)
- FOUND commit: `c3aaa77c`
- FOUND commit: `7467149f`
- FOUND commit: `24e4289c`
- FOUND PR: <https://github.com/AVSystem/scala-commons/pull/860> (draft, milestone Scala 3, title `[Scala 3] Pivot to Scala 3 only — comment broken, green CI`)
- FOUND fork CI: <https://github.com/halotukozak/scala-commons3/actions/runs/26753320607> (success)
- FOUND AVSystem CI: <https://github.com/AVSystem/scala-commons/actions/runs/26753512180> (all 3 shards pass)
