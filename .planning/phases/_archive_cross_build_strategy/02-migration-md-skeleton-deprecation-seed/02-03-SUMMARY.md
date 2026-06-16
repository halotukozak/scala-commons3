---
phase: 02-migration-md-skeleton-deprecation-seed
plan: 03
subsystem: docs
tags: [migration, validation, bash, grep, sbt, scalafmt]

# Dependency graph
requires:
  - phase: 02-migration-md-skeleton-deprecation-seed/01
    provides: MIGRATION.md skeleton with per-module status table and 2.13-only sections
  - phase: 02-migration-md-skeleton-deprecation-seed/02
    provides: Deprecation log seeded with 152 entries tagged [port]/[skip-port]
provides:
  - Idempotent grep-based content invariant suite for MIGRATION.md (.planning/.../check.sh)
  - Sanity proof that the docs-only PR did not perturb scalafmt or 2.13 compile
  - Pre-push hygiene audit confirming branch readiness for upstream PR
affects: [02-04-push-and-pr, future-migration-md-edits]

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Per-phase check.sh under .planning/ as planning-tooling artifact (gitignored)"
    - "set -euo pipefail + assert() helper printing pretty per-check status"

key-files:
  created:
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
  modified: []

key-decisions:
  - "Awk range terminators of the form `/^## X/,/^## /` collapse to a single line when X itself starts with `## ` — switched to `awk '/^## X/{f=1;next} f && /^## /{exit} f'` for How-to-update and 2.13-only sections."
  - "Wrapped `$(... | grep -c ...)` captures with `|| true` so an empty grep result (legitimate zero count) cannot abort the script under set -e."
  - "Scoped Task 3 branch hygiene assertions to `--grep='^docs(migration):'` because branch carries Phase 1 commits and a user CI tweak in addition to the two Phase 2 docs commits — counting all commits since upstream/scala-3 would be misleading for a Phase 2-only audit."
  - "Used `commons-jvm/compile` rather than the plan's literal `jvm/compile` — the sbt-nosbt ProjectGroup convention from Phase 1 prefixes module ids with `commons-` (same Rule 3 fix already applied in Phase 1 Plan 03)."

patterns-established:
  - "Pattern: phase verification gates live as bash scripts under .planning/phases/XX-name/ so they ride along with planning artifacts and stay out of repo history."
  - "Pattern: every grep-based assertion includes the observed count in the failure description so a red gate self-describes (e.g. `per-module table has 13 rows (got 12)`)."

requirements-completed: [DOC-01, DOC-02, DOC-03, DOC-04, WORKFLOW-04, WORKFLOW-05]

# Metrics
duration: 4min
completed: 2026-05-31
---

# Phase 02 Plan 03: check.sh and verification Summary

**Idempotent 18-assertion grep suite for MIGRATION.md authored under .planning/, all gates green (check.sh + scalafmtCheckAll + ++2.13 commons-jvm/compile), branch hygiene audited as PR-ready.**

## Performance

- **Duration:** ~4 min
- **Started:** 2026-05-31T16:29:00Z
- **Completed:** 2026-05-31T16:33:03Z
- **Tasks:** 3
- **Files modified:** 0 tracked (1 untracked, gitignored: check.sh)

## Accomplishments

- Authored `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (18 assertions covering all 13 invariant rows from VALIDATION.md plus DOC-01 tone checks).
- `bash check.sh` exits 0: `ALL CHECKS GREEN (18 assertions)` — per-module table has 13 rows, How-to-update has 5 numbered rules, deprecation log has 152 entries all tagged [port]/[skip-port], 2.13-only section names jetty/analyzer/spring/RPC.
- `sbt -batch scalafmtCheckAll` exit 0 in ~4.77s wall.
- `sbt -batch '++2.13 commons-jvm/compile'` exit 0 in ~4.35s wall (warm cache).
- Branch hygiene audit (scoped to `^docs(migration):` commits via `git log --grep`): 2 commits, modifies only `MIGRATION.md`, 0 `.planning/` paths, 0 GSD nomenclature.

## Task Commits

No commits in this plan — pure verification slice (per plan spec: "DO NOT commit anything in this task").

- check.sh is untracked and gitignored (verified via `git check-ignore -v`).
- Working tree clean: `git status --porcelain` empty.
- Branch `02-migration-md` still carries exactly 2 `docs(migration):` commits (4ae73373, 0a0ac2e2).

## Files Created/Modified

- `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (new, untracked, executable) — 18-assertion idempotent grep harness for MIGRATION.md content invariants.

## Decisions Made

- **Awk range fix:** Plan-specified `awk '/^## How to update/,/^## /'` collapsed to a single line because the start pattern itself matches the end pattern. Replaced with stateful awk (`/^## X/{f=1;next} f && /^## /{exit} f`) for both How-to-update and 2.13-only extractions.
- **Defensive `|| true`:** Several count-capture lines wrap `grep -c` with `|| true` since under `set -e` an empty match (legitimate zero) would otherwise abort the script before the assertion could pretty-print the actual zero count.
- **Hygiene audit scope:** Branch is not bisected to exactly Phase 2 — it inherits Phase 1's 5 commits plus user's CI tweak (70093c56). Scoped the "exactly 2 commits" and "only MIGRATION.md modified" assertions to commits matching `^docs(migration):` so the audit measures Phase 2's own contribution rather than the entire branch delta.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Bash awk range patterns collapsed under set -e**
- **Found during:** Task 1 (first run of check.sh)
- **Issue:** `awk '/^## How to update/,/^## /' MIGRATION.md | grep -cE '^[0-9]+\.'` returned 0 → grep exit 1 → unwrapped `$(...)` capture under `set -e` aborted the script silently after assertion 7.
- **Fix:** Switched range expressions to stateful awk that opens a flag at the start heading and exits at the next `## ` heading; wrapped count captures with `|| true` so zero results land in the variable for pretty-printing in the assertion description.
- **Files modified:** `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (untracked).
- **Verification:** Re-ran `bash check.sh` → `ALL CHECKS GREEN (18 assertions)`.

**2. [Rule 3 - Blocking] sbt project id is `commons-jvm`, not `jvm`**
- **Found during:** Task 2 (sbt compile sanity)
- **Issue:** Plan said `++2.13 jvm/compile` but Phase 1's sbt-nosbt ProjectGroup convention prefixes module ids with `commons-`. Same Rule 3 fix as Phase 1 Plan 03.
- **Fix:** Used `sbt -batch '++2.13 commons-jvm/compile'`.
- **Verification:** Exit 0 in ~4.35s wall.

**3. [Rule 3 - Blocking] Branch hygiene audit scope mismatch**
- **Found during:** Task 3 (audit)
- **Issue:** Plan asserts "exactly 2 commits ahead of upstream/scala-3" but branch inherits Phase 1's 5 commits + user's 70093c56 CI tweak. Counting raw `upstream/scala-3..HEAD` would falsely red the gate.
- **Fix:** Scoped audit queries with `git log --grep='^docs(migration):'` to measure Phase 2's own contribution. Confirmed 2 Phase 2 docs commits, 1 file (MIGRATION.md), 0 .planning/, 0 GSD nomenclature.
- **Verification:** All five scoped queries return the expected counts.

---

**Total deviations:** 3 auto-fixed (3 blocking via Rule 3)
**Impact on plan:** All three were mechanical workarounds for plan-spec drift against the actual environment (bash semantics, Phase 1 module-id convention, multi-source branch). No content or scope change.

## Issues Encountered

None beyond the deviations above.

## User Setup Required

None — verification-only plan.

## Next Phase Readiness

- Branch `02-migration-md` is ready for Plan 04 (human-ack push + PR open).
- All three pre-push gates green: check.sh, scalafmtCheckAll, ++2.13 commons-jvm/compile.
- No tracked-file drift, no `.planning/` leakage, no GSD nomenclature in any Phase 2 commit message.
- `check.sh` remains in place under `.planning/` for re-use on any future MIGRATION.md edit.

## Self-Check: PASSED

- FOUND: `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (executable, gitignored)
- FOUND: 2 Phase 2 docs commits (4ae73373, 0a0ac2e2) on branch `02-migration-md`
- No new commits introduced by this plan (per spec)

---
*Phase: 02-migration-md-skeleton-deprecation-seed*
*Completed: 2026-05-31*
