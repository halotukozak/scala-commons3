---
phase: 02-migration-md-skeleton-deprecation-seed
plan: 03
type: execute
wave: 3
depends_on: [01, 02]
files_modified: []
autonomous: true
commit_docs: false
requirements: [DOC-01, DOC-02, DOC-03, DOC-04, WORKFLOW-04, WORKFLOW-05]
must_haves:
  truths:
    - "`.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` exists with `set -euo pipefail` and asserts all grep checks from VALIDATION.md"
    - "The check script is NOT committed (lives under `.planning/`, ignored)"
    - "Running `bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` exits 0"
    - "`sbt -batch scalafmtCheckAll` exits 0 — docs PR did not perturb Scala formatting"
    - "`sbt -batch '++2.13 jvm/compile'` exits 0 — docs PR did not perturb build"
    - "Branch `02-migration-md` carries exactly 2 commits ahead of `upstream/scala-3` (from Plans 01 + 02)"
    - "`git diff upstream/scala-3..HEAD --name-only` lists exactly `MIGRATION.md`"
    - "No `.planning/` paths in any commit on the branch (REQ WORKFLOW-05)"
    - "No GSD nomenclature in any commit message on the branch (REQ WORKFLOW-04)"
  artifacts:
    - path: .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
      provides: "Idempotent grep-based validation suite for the MIGRATION.md content"
      contains: "set -euo pipefail"
      min_lines: 40
  key_links:
    - from: ".planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh"
      to: "MIGRATION.md content invariants"
      via: "grep / awk assertions covering DOC-01, DOC-03, DOC-04 from VALIDATION.md"
      pattern: "grep"
    - from: "check.sh exit status"
      to: "phase verification gate"
      via: "exit 0 == green; any non-zero == fail with named assertion"
      pattern: "exit"
---

<objective>
Author `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` — an idempotent bash script that asserts every grep-based content invariant of `MIGRATION.md` listed in `02-VALIDATION.md`. Run it. Run `sbt -batch scalafmtCheckAll` and `sbt -batch '++2.13 jvm/compile'` as belt-and-suspenders sanity checks proving the docs PR did not perturb the build. The check script lives under `.planning/` and is intentionally NOT committed (REQ WORKFLOW-05). No new commits on `02-migration-md` in this plan.

Purpose: Closes Wave 0 of `02-VALIDATION.md`, gives downstream phases a reusable content harness for any future MIGRATION.md edits, and provides the green-light signal that Plan 04 (push + PR) requires before user ack.

Output: Local `check.sh` script (uncommitted, under `.planning/`). All three verification commands exit 0. No changes to tracked files; no new commits on the branch.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-VALIDATION.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-01-SUMMARY.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-02-SUMMARY.md
</context>

<interfaces>
Assertions to encode in `check.sh` (one per VALIDATION.md row, plus PR-hygiene checks from CONTEXT.md):

| # | Requirement | Assertion |
|---|-------------|-----------|
| 1 | DOC-01 | `test -f MIGRATION.md` |
| 2 | DOC-01 | `head -1 MIGRATION.md` equals `# Scala 3 Migration Status` |
| 3 | DOC-01 | All four `## ` sections present (How to update, Per-module status, 2.13-only modules, Deprecation log) |
| 4 | DOC-01 | Per-module status table has all 13 rows |
| 5 | DOC-02 (codified) | `## How to update` has ≥ 5 numbered rules |
| 6 | DOC-03 | `## Deprecation log` section non-empty (≥ 100 entries) |
| 7 | DOC-03 | Seed command `git grep -n '@deprecated' master -- '*.scala'` documented verbatim in the doc |
| 8 | DOC-03 | Every deprecation log entry tagged `[port]` or `[skip-port]` |
| 9 | DOC-03 | `### core/` and `### mongo/` subheadings present |
| 10 | DOC-04 | `## 2.13-only modules` section names jetty, analyzer, spring, RPC |
| 11 | WORKFLOW-04 | No GSD vocabulary in MIGRATION.md (`gsd`, `wave`, `phase [0-9]`, `RESEARCH.md`, `PLAN.md`, `CONTEXT.md`) |
| 12 | WORKFLOW-05 | No `.planning/` references in MIGRATION.md |
| 13 | DOC-01 (tone) | No emoji `✓` and no first-person plural `\bwe\b` / `\bour\b` |

Script protocol:
- `#!/usr/bin/env bash`
- `set -euo pipefail`
- Run from any working directory — script `cd`s to the repo root via `git rev-parse --show-toplevel` at the top.
- Each assertion: pretty-print `→ check: <description>` before, and `   ✓ ok` after, OR fail with `   ✗ FAIL: <description>` and `exit 1`. Use a small bash `assert` helper.
- Final line on success: `ALL CHECKS GREEN (<N> assertions)`.
- Exit 0 on success; non-zero with named failure on first failure.

This is a `.planning/` artifact — it MUST NOT be committed. Verify after writing it that `git status --porcelain` shows the script under `.planning/...` and that `.gitignore` (or top-level `.planning/` ignore) keeps it untracked.
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Write check.sh under .planning/phases/02-.../ with all 13 assertions; verify it is gitignored</name>
  <files>.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh</files>
  <read_first>
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-VALIDATION.md §"Per-Task Verification Map" (all rows)
    - MIGRATION.md (post-Plan-02 state — to know the literal content the script asserts against)
    - .gitignore (repo root) — confirm `.planning/` is listed so the script is automatically untracked
  </read_first>
  <action>
    Working dir: `/Users/bkozak/IdeaProjects/scala-commons3`.

    Step 1 — verify `.planning/` is gitignored (it must be — per PROJECT.md / STATE.md):

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git check-ignore -v .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
        # MUST print a `.gitignore:N:.planning/  .planning/...` line (proof of ignore). Exit code 0.

    If `git check-ignore` exits non-zero, `.planning/` is NOT ignored — STOP and surface to the user (the project rule "no .planning/ in commits" is broken at the .gitignore level, which is a Phase 0 invariant failure).

    Step 2 — write `check.sh` at `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` with this exact content:

        #!/usr/bin/env bash
        # Phase 2 — MIGRATION.md content invariants.
        # NOT committed (lives under .planning/, which is gitignored).
        # Idempotent. Exits 0 on full green, non-zero on first failure with named assertion.

        set -euo pipefail

        cd "$(git rev-parse --show-toplevel)"

        PASS=0
        FAIL=0

        assert() {
          local desc="$1"; shift
          printf '→ check: %s\n' "$desc"
          if "$@" >/dev/null 2>&1; then
            printf '   ok\n'
            PASS=$((PASS+1))
          else
            printf '   FAIL: %s\n' "$desc" >&2
            FAIL=$((FAIL+1))
            exit 1
          fi
        }

        # 1. DOC-01: file exists at repo root
        assert "MIGRATION.md exists at repo root" test -f MIGRATION.md

        # 2. DOC-01: H1 is exactly `# Scala 3 Migration Status`
        assert "H1 is '# Scala 3 Migration Status'" bash -c 'head -1 MIGRATION.md | grep -Fxq "# Scala 3 Migration Status"'

        # 3. DOC-01: all four ## sections present
        assert "section '## How to update' present"      grep -Fxq '## How to update' MIGRATION.md
        assert "section '## Per-module status' present"  grep -Fxq '## Per-module status' MIGRATION.md
        assert "section '## 2.13-only modules' present"  grep -Fxq '## 2.13-only modules' MIGRATION.md
        assert "section '## Deprecation log' present"    grep -Fxq '## Deprecation log' MIGRATION.md

        # 4. DOC-01: per-module status table has all 13 rows
        ROW_COUNT=$(awk '/^## Per-module status/,/^## 2/' MIGRATION.md \
          | grep -cE '^\| (macros|made|core|hocon|mongo|mongo-js|core-js|benchmark3|jetty|analyzer|spring|RPC|cbor) ')
        assert "per-module table has 13 rows (got $ROW_COUNT)" test "$ROW_COUNT" -eq 13

        # 5. DOC-02 (codified): ## How to update enumerates ≥ 5 numbered rules
        RULE_COUNT=$(awk '/^## How to update/,/^## /' MIGRATION.md | grep -cE '^[0-9]+\.')
        assert "How to update has ≥ 5 numbered rules (got $RULE_COUNT)" test "$RULE_COUNT" -ge 5

        # 6. DOC-03: ## Deprecation log has ≥ 100 entries
        DEP_COUNT=$(awk '/^## Deprecation log/,0' MIGRATION.md | grep -cE '^[^:]+:[0-9]+ — ')
        assert "deprecation log has ≥ 100 entries (got $DEP_COUNT)" test "$DEP_COUNT" -ge 100

        # 7. DOC-03: seed command documented verbatim
        assert "seed command documented verbatim" grep -F "git grep -n '@deprecated' master -- '*.scala'" MIGRATION.md

        # 8. DOC-03: every deprecation entry tagged [port] or [skip-port]
        UNTAGGED=$(awk '/^## Deprecation log/,0' MIGRATION.md | grep -E '^[^:]+:[0-9]+ — ' | grep -vcE '\[(port|skip-port)\]$' || true)
        assert "every deprecation entry tagged (untagged=$UNTAGGED)" test "$UNTAGGED" -eq 0

        # 9. DOC-03: ### core/ and ### mongo/ subheadings present
        assert "### core/ subheading present"  grep -Fxq '### core/' MIGRATION.md
        assert "### mongo/ subheading present" grep -Fxq '### mongo/' MIGRATION.md

        # 10. DOC-04: 2.13-only section names all four
        TARGET_COUNT=$(awk '/^## 2.13-only modules/,/^## Deprecation/' MIGRATION.md \
          | grep -Eo '\b(jetty|analyzer|spring|RPC)\b' | sort -u | wc -l | tr -d ' ')
        assert "2.13-only section names jetty/analyzer/spring/RPC (got $TARGET_COUNT/4)" test "$TARGET_COUNT" -eq 4

        # 11. WORKFLOW-04: no GSD vocabulary
        assert "no GSD vocabulary in MIGRATION.md" bash -c '! grep -iE "\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b" MIGRATION.md'

        # 12. WORKFLOW-05: no .planning/ references
        assert "no .planning/ references in MIGRATION.md" bash -c '! grep -F ".planning/" MIGRATION.md'

        # 13. DOC-01 (tone): no ✓ emoji and no first-person plural
        assert "no ✓ emoji in MIGRATION.md" bash -c '! grep -F "✓" MIGRATION.md'
        assert "no first-person plural in MIGRATION.md" bash -c '! grep -E "\b(we|our)\b" MIGRATION.md'

        printf '\nALL CHECKS GREEN (%d assertions)\n' "$PASS"

    Step 3 — make the script executable and run it:

        chmod +x .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
        bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
        # MUST exit 0 with "ALL CHECKS GREEN (<N> assertions)" on final line

    Step 4 — re-verify the script is untracked / ignored:

        git status --porcelain | grep -F '.planning/' || true
        # SHOULD print NOTHING (any `.planning/` paths must be ignored — they should not appear in git status)
        git check-ignore -v .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
        # MUST exit 0

    Rationale: the check script supports downstream phases too. Future MIGRATION.md edits (status flips, deprecation log appends from later PRs) can re-run this script to validate structural invariants. It lives under `.planning/` exactly because it's planning-tooling, not a deliverable.

    DO NOT commit anything in this task. No tracked files change.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; test -x .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh &amp;&amp; bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh &amp;&amp; git check-ignore -v .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh &amp;&amp; [ -z "$(git status --porcelain | grep -F '.planning/')" ]</automated>
  </verify>
  <acceptance_criteria>
    - `test -x /Users/bkozak/IdeaProjects/scala-commons3/.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` exits 0 (executable).
    - First line of script is `#!/usr/bin/env bash`.
    - Script contains `set -euo pipefail`.
    - `bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` exits 0 with final line matching `^ALL CHECKS GREEN \([0-9]+ assertions\)$`.
    - At least 15 `assert` invocations in the script — `grep -cE '^assert ' .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` is ≥ 15.
    - Script is gitignored — `git check-ignore -v .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` exits 0.
    - `git status --porcelain | grep -F '.planning/'` produces no output (no `.planning/` paths surface as tracked changes).
    - Branch still has exactly 2 commits ahead of `upstream/scala-3` — `git log upstream/scala-3..HEAD --oneline | wc -l` prints `2` (no new commits).
  </acceptance_criteria>
  <done>`check.sh` exists, executable, asserts ≥ 15 invariants, exits 0, gitignored. No tracked-file changes, no new commits.</done>
</task>

<task type="auto">
  <name>Task 2: Sanity-check the build is not perturbed — scalafmtCheckAll + ++2.13 jvm/compile</name>
  <files>(none — read-only sbt invocations)</files>
  <read_first>
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-VALIDATION.md §"Per-Task Verification Map" (final two rows — sbt sanity)
  </read_first>
  <action>
    Working dir: `/Users/bkozak/IdeaProjects/scala-commons3`.

    Phase 2 is documentation-only. Neither MIGRATION.md nor `.planning/check.sh` should perturb sbt — but a smoke test catches accidental disasters (e.g., file written to wrong path with the same name as a Scala source, scalafmt config edited, build.sbt typo). Run from project root:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        sbt -batch scalafmtCheckAll
        # MUST exit 0. Phase 2 touches no `.scala` files; scalafmt result should equal the post-Phase-1 baseline.

        cd /Users/bkozak/IdeaProjects/scala-commons3
        sbt -batch '++2.13 jvm/compile'
        # MUST exit 0. Confirms the build still compiles on Scala 2.13. We pick 2.13 (not Scala 3) because:
        # - Phase 1 may not be merged onto upstream/scala-3 yet, so the Scala 3 side has nothing to compile beyond empty jars
        # - 2.13 carries the full existing codebase and is the strictest smoke test
        # - this matches VALIDATION.md row "Repo still compiles (smoke)"

    Expected outcomes:
    - `scalafmtCheckAll` exits 0 with no diff (file list unchanged).
    - `++2.13 jvm/compile` exits 0. First invocation may take ~3 min (cold sbt + warm compile); subsequent runs are seconds.

    If `scalafmtCheckAll` fails, the failure must be in a file Phase 2 did not touch — most likely an inherited issue from `upstream/scala-3` HEAD. Surface to the user with the failing file list; do NOT attempt to fix (this is not Phase 2's responsibility).

    If `++2.13 jvm/compile` fails on the master tree (which Phase 2's branch base extends with one docs file), the failure exists in upstream and is not introduced by Phase 2. Surface and STOP.

    No commits. No file writes. Read-only sbt sanity gate.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch scalafmtCheckAll &amp;&amp; sbt -batch '++2.13 jvm/compile'</automated>
  </verify>
  <acceptance_criteria>
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch scalafmtCheckAll` exits 0.
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++2.13 jvm/compile'` exits 0.
    - No tracked-file changes: `git status --porcelain` shows no `M`/`A`/`D` lines for tracked files (the `.planning/check.sh` from Task 1 stays ignored).
    - Branch commit count unchanged — `git log upstream/scala-3..HEAD --oneline | wc -l` prints `2`.
  </acceptance_criteria>
  <done>scalafmt clean; ++2.13 jvm/compile green; no tracked-file drift; branch ready for Plan 04 (push + PR).</done>
</task>

<task type="auto">
  <name>Task 3: Final branch hygiene audit — no .planning/, no GSD nomenclature, exactly MIGRATION.md modified</name>
  <files>(none — read-only audit)</files>
  <read_first>
    - .planning/ROADMAP.md §"Per-PR Workflow" steps 1-7 (what should be true on the branch before push)
    - .planning/REQUIREMENTS.md §WORKFLOW-04, §WORKFLOW-05
  </read_first>
  <action>
    Working dir: `/Users/bkozak/IdeaProjects/scala-commons3`.

    Final pre-push audit. All assertions below are over `git log upstream/scala-3..HEAD`. Plan 04 (push + PR) will fail with a noisy diagnostic if any of these are red — better to surface here.

        cd /Users/bkozak/IdeaProjects/scala-commons3

        # 1. Branch position
        git rev-parse --abbrev-ref HEAD    # MUST be `02-migration-md`

        # 2. Commit count
        git log upstream/scala-3..HEAD --oneline | wc -l    # MUST be 2

        # 3. Modified files across all branch commits
        git diff upstream/scala-3..HEAD --name-only    # MUST be exactly `MIGRATION.md`

        # 4. No .planning/ paths in any commit
        git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning' || true    # MUST be 0

        # 5. No GSD nomenclature in any commit message
        git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' || true    # MUST exit 1 / print nothing

        # 6. Commit subjects start with `docs(migration):`
        git log upstream/scala-3..HEAD --format=%s | grep -vcE '^docs\(migration\):' || true    # MUST be 0

        # 7. Working tree clean (no uncommitted tracked-file changes)
        git status --porcelain | grep -vF '.planning/' | wc -l    # MUST be 0 (any `.planning/` lines were already filtered by git's ignore — this line is paranoia)

        # 8. Check script still green
        bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh

    Cross-reference: assertions 1-7 here mirror Plan 1's Plan 03 verification pattern but tailored for a docs-only slice (1 file modified, 2 commits, `docs(migration):` prefix).

    Surface a one-line summary on success. Example:

        Branch `02-migration-md` ready for push:
          - 2 commits ahead of upstream/scala-3
          - 1 file modified: MIGRATION.md
          - 0 .planning/ paths in branch history
          - 0 commits with GSD nomenclature
          - check.sh: ALL CHECKS GREEN (16 assertions)

    No commits. No file writes.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; git rev-parse --abbrev-ref HEAD | grep -Fxq '02-migration-md' &amp;&amp; [ "$(git log upstream/scala-3..HEAD --oneline | wc -l)" -eq 2 ] &amp;&amp; [ "$(git diff upstream/scala-3..HEAD --name-only | sort -u | tr -d '[:space:]')" = "MIGRATION.md" ] &amp;&amp; [ "$(git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning' || true)" -eq 0 ] &amp;&amp; ! git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; [ "$(git log upstream/scala-3..HEAD --format=%s | grep -vcE '^docs\(migration\):' || true)" -eq 0 ] &amp;&amp; bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh</automated>
  </verify>
  <acceptance_criteria>
    - Current branch is `02-migration-md` — `git rev-parse --abbrev-ref HEAD` prints exactly `02-migration-md`.
    - Branch has exactly 2 commits ahead of `upstream/scala-3` — `git log upstream/scala-3..HEAD --oneline | wc -l` prints `2`.
    - Branch modifies exactly one file (`MIGRATION.md`) — `git diff upstream/scala-3..HEAD --name-only | sort -u | tr -d '[:space:]'` prints `MIGRATION.md`.
    - No `.planning/` paths in any commit on the branch (REQ WORKFLOW-05) — `git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0`.
    - No GSD nomenclature in any commit message (REQ WORKFLOW-04) — `git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - Every commit message starts with `docs(migration):` — `git log upstream/scala-3..HEAD --format=%s | grep -vcE '^docs\(migration\):'` prints `0`.
    - `check.sh` still exits 0.
  </acceptance_criteria>
  <done>Branch hygiene audit green: correct branch, 2 commits, 1 file (MIGRATION.md), no `.planning/` leakage, no GSD nomenclature, all commits prefixed `docs(migration):`, check.sh green. Branch is ready for Plan 04 (human-ack push + PR).</done>
</task>

</tasks>

<verification>
After Plan 03 completes:

1. `bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` exits 0.
2. `sbt -batch scalafmtCheckAll` exits 0.
3. `sbt -batch '++2.13 jvm/compile'` exits 0.
4. `git log upstream/scala-3..HEAD --oneline | wc -l` prints `2`.
5. `git diff upstream/scala-3..HEAD --name-only` prints exactly `MIGRATION.md`.
6. `git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0`.
7. `git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
8. No tracked-file drift; `.planning/check.sh` exists and is gitignored.
</verification>

<success_criteria>
- `check.sh` exists, exits 0 with ≥ 15 assertions, gitignored (REQ DOC-01..04 + WORKFLOW-04..05 enforcement).
- `sbt scalafmtCheckAll` clean; `sbt '++2.13 jvm/compile'` green — docs PR did not perturb build.
- Branch carries exactly 2 commits and modifies exactly `MIGRATION.md`.
- No `.planning/` paths or GSD nomenclature anywhere in branch history.
- No new commits in this plan — pure verification slice.
</success_criteria>

<output>
After completion, create `.planning/phases/02-migration-md-skeleton-deprecation-seed/02-03-SUMMARY.md` capturing:
- Final tail of `bash check.sh` output (last 5 lines, including "ALL CHECKS GREEN (N assertions)")
- `scalafmtCheckAll` and `++2.13 jvm/compile` exit statuses + wall-clock times
- Branch hygiene audit summary (commit count, modified files, .planning/ count, GSD count)
- Confirmation that `check.sh` is gitignored
</output>
