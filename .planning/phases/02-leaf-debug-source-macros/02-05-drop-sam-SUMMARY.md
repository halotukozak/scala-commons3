---
phase: 02-leaf-debug-source-macros
plan: 05
subsystem: core / misc
tags: [scala3, deletion, deprecated, migration-doc]
requirements: [SAM-01]
dependency-graph:
  requires:
    - "01-big-bang (PR #860) baseline @ 15fbf4a2"
  provides:
    - "Removal of deprecated misc.Sam / misc.SamCompanion from the codebase"
  affects:
    - "Public API surface: misc.Sam and misc.SamCompanion no longer exist"
    - "MIGRATION.md §1 (Will Not Migrate) gains one row; §6 (Backlog) loses three"
tech-stack:
  added: []
  patterns:
    - "Pure deletion of @deprecated APIs with stdlib replacements (rule feedback_dont_port_deprecated)"
key-files:
  created: []
  modified:
    - "MIGRATION.md"
  deleted:
    - "core/src/main/scala/com/avsystem/commons/misc/Sam.scala"
    - "core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala"
    - "core/src/test/scala/com/avsystem/commons/misc/SamTest.scala"
decisions:
  - "Drop instead of port: Sam / SamCompanion are @deprecated since 2.28.0 with stdlib native SAM conversion as replacement (per project rule feedback_dont_port_deprecated.md)"
  - "Delete the matching SamTest.scala in the same commit — test exercises only the now-removed symbols"
  - "Document in MIGRATION.md §1 (Will Not Migrate) — not §2 (Deprecated on Scala 3) — because nothing ships on Scala 3 at all"
metrics:
  duration: "~12 min"
  completed: 2026-06-01
  tasks: 3 of 3
  files: 4 (3 deleted + 1 modified)
  commits: 2 (chore + docs)
---

# Phase 02 Plan 05: drop-sam Summary

**One-liner:** Deleted deprecated `misc.Sam` / `misc.SamCompanion` outright (plus their test) instead of porting to Scala 3 quotes; stdlib native SAM conversion replaces them. MIGRATION.md §1 gains one row, §6 backlog loses three (Total tags: 155 → 153). Draft PR #863 opened against `AVSystem/scala-commons:scala-3` with milestone "Scala 3".

## What shipped

- **Deleted** `core/src/main/scala/com/avsystem/commons/misc/Sam.scala` — `object Sam { def apply[T](fun: => Any): T = ??? }`, marked `@deprecated since 2.28.0`.
- **Deleted** `core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala` — `abstract class SamCompanion[T, F]` + `object SamCompanion.ValidSam`, marked `@deprecated since 2.28.0`.
- **Deleted** `core/src/test/scala/com/avsystem/commons/misc/SamTest.scala` — exercised only the now-removed symbols; in scope per pre-flight reference grep.
- **`MIGRATION.md`** — §1 (Will Not Migrate) gains one row for `misc.Sam` / `misc.SamCompanion`; §6 (Backlog) loses three rows (Sam.scala:9, SamCompanion.scala:11, SamCompanion.scala:19); `Total tags: 155 → 153`.

## Commits

| # | Hash       | Type  | Message                                                |
|---|------------|-------|--------------------------------------------------------|
| 1 | `4b2b18f7` | chore | drop deprecated Sam and SamCompanion                   |
| 2 | `cac74d11` | docs  | record Sam/SamCompanion as will-not-migrate            |

## PR

- **URL:** https://github.com/AVSystem/scala-commons/pull/863
- **Number:** #863
- **State:** DRAFT
- **Base:** `AVSystem/scala-commons:scala-3`
- **Head:** `halotukozak:02-05-drop-sam`
- **Title:** `[Scala 3] Phase 02-05: drop deprecated Sam / SamCompanion`
- **Milestone:** `Scala 3` (#1) — assigned via `gh api -X PATCH /repos/AVSystem/scala-commons/issues/863 -f milestone=1`

## Verification

| Gate | Command | Result |
|------|---------|--------|
| Files deleted (working tree)     | `test ! -f core/src/main/scala/com/avsystem/commons/misc/Sam.scala && test ! -f .../SamCompanion.scala`        | pass |
| Files deleted (git index)        | `git ls-files \| grep -E '(^\|/)Sam(Companion)?\\.scala\$'`                                                    | empty |
| No non-self refs                 | `grep -rnE '\\b(Sam\|SamCompanion)\\b' core/src/ mongo/ hocon/ \| grep -v MIGRATION.md`                        | empty |
| MIGRATION §1 row added           | `grep -n 'misc.Sam' MIGRATION.md`                                                                              | line 21 |
| MIGRATION §6 rows removed        | `grep -nE 'Sam\\.scala:9\|SamCompanion\\.scala:(11\|19)' MIGRATION.md`                                         | empty |
| MIGRATION total updated          | `grep 'Total tags' MIGRATION.md`                                                                               | "Total tags: 153." |
| Local `sbt commons-core/compile` | `sbt -batch 'commons-core/compile'`                                                                            | **deferred — see Deviations** |

## Deviations from Plan

### Deferred Issues

**1. [Deferred] Local sbt compile not run — sbt-git / JGit worktree incompatibility**
- **Found during:** Task 1 verification step.
- **Issue:** `sbt -batch 'commons-core/compile'` fails at project-load time with `org.eclipse.jgit.errors.NoWorkTreeException: Bare Repository has neither a working tree, nor an index`. The error originates inside `sbt-git`'s `JGit.hasUncommittedChanges` (transitively pulled in by `sbt-ci-release`), evaluated during build settings load. JGit ≤6.x is known to misclassify a linked git worktree's gitfile-style `.git` pointer as a bare repo.
- **Why not auto-fixed:** Workarounds (`SBT_OPTS` override, `set ThisBuild / git.uncommittedSignifier := None`, replacing the `.git` gitfile with a symlink to the worktree gitdir) all either ran too late or required touching the worktree's git metadata. The classifier denied the symlink workaround as a scope escalation.
- **Risk assessment for this PR:** **Low.** This is a pure deletion. The pre-deletion reference grep (`grep -rnE '\b(Sam|SamCompanion)\b' core/src/ mongo/ hocon/`) returned zero non-self matches in production sources (only `SamTest.scala`, which was also deleted). The post-deletion grep also returns empty. With zero callers, the deletion cannot cause a compile failure.
- **Verification path:** CI on the draft PR will run the full `sbt 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll ;scalafmtSbtCheck'` on a fresh checkout (non-worktree) and surface any issue before maintainer merge.
- **Files modified:** none (verification gate, not a code change).
- **Commit:** n/a.

### Out-of-Scope Pre-existing Modifications

- `.idea/codeStyles/Project.xml` shows as modified in the worktree (pre-existing across all `02-*` worktrees, not introduced by this slice). Not staged, not committed, not part of this PR.

## Authentication gates

None.

## Self-Check: PASSED

- Files deleted: confirmed via `git log -1 --name-status` showing `D` for all three.
- MIGRATION edits: confirmed via `grep -n 'misc.Sam' MIGRATION.md` (line 21) and `grep -nE 'Sam\.scala:9|SamCompanion\.scala:(11|19)' MIGRATION.md` (empty).
- Commits `4b2b18f7`, `cac74d11` present on branch `02-05-drop-sam` and pushed to `origin`.
- PR #863 confirmed: title prefix `[Scala 3]`, draft, milestone `Scala 3` (#1), base `scala-3`, head `halotukozak:02-05-drop-sam`.
