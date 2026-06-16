---
phase: 03-macros-scala-3-stub
plan: 02
type: execute
wave: 2
depends_on: [01]
files_modified: []
autonomous: false
commit_docs: false
requirements: [WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, QUALITY-01]
must_haves:
  truths:
    - "Pre-push sanity gates pass: `sbt scalafmtCheckAll` green; `sbt '++3 macros/compile'` green; `sbt '++3 core/compile'` green; `sbt '++2.13 jvm/compile'` green"
    - "User explicitly acks before `git push origin 03-macros-scala-3-stub`"
    - "Branch `03-macros-scala-3-stub` pushed to `origin` (the fork `halotukozak/scala-commons3`)"
    - "GitHub Actions CI on the pushed branch reports `success` (5-gate matrix from Phase 1)"
    - "User explicitly acks before `gh pr create`"
    - "PR opened against `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02)"
    - "PR title and body contain no GSD nomenclature; no `.planning/` references"
    - "PR is left OPEN — Claude does NOT merge (global rule)"
  artifacts:
    - path: "(remote) origin/03-macros-scala-3-stub"
      provides: "Pushed branch carrying the macros Scala 3 stub + MIGRATION.md flip"
      contains: "macros/src/main/scala-3/"
    - path: "(remote) PR on AVSystem/scala-commons"
      provides: "Open PR against the scala-3 branch"
      contains: "build(macros)"
  key_links:
    - from: "local 03-macros-scala-3-stub branch"
      to: "origin/03-macros-scala-3-stub"
      via: "git push -u origin 03-macros-scala-3-stub"
      pattern: "03-macros-scala-3-stub"
    - from: "origin/03-macros-scala-3-stub"
      to: "AVSystem/scala-commons PR against scala-3"
      via: "gh pr create --repo AVSystem/scala-commons --base scala-3 --head halotukozak:03-macros-scala-3-stub"
      pattern: "scala-3"
---

<objective>
Gate Phase 3 behind two explicit user acks: (1) before `git push -u origin 03-macros-scala-3-stub`, (2) before `gh pr create --repo AVSystem/scala-commons --base scala-3`. Pre-push sanity (scalafmt + targeted compiles) must pass locally; CI must report `success` on the pushed branch before the second ack is requested. Claude opens the PR — Claude does NOT merge.

Purpose: Satisfies REQ WORKFLOW-01..05 — branch off upstream, push to fork, target upstream `scala-3` branch, user-ack gates, no GSD nomenclature, no `.planning/` in diff. Mirrors Phase 2 Plan 04 pattern (which itself mirrors Phase 1 Plan 03 tasks 4–7).

Output: `origin/03-macros-scala-3-stub` pushed, CI green, open PR URL recorded against `AVSystem/scala-commons:scala-3`. PR left for maintainer manual merge.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/ROADMAP.md
@.planning/phases/03-macros-scala-3-stub/03-CONTEXT.md
@.planning/phases/03-macros-scala-3-stub/03-01-SUMMARY.md
</context>

<interfaces>
Git remotes (verified during Phase 2 planning; unchanged):
- `origin` = `https://github.com/halotukozak/scala-commons3.git` (the fork, push target)
- `upstream` = `https://github.com/AVSystem/scala-commons.git` (PR target)

Fork owner string for `gh pr create --head <owner>:<branch>`: `halotukozak`.

CI gate: 5-gate matrix from Phase 1 (`+jvm/test`, `+jvm2/test`, `+js/test`, `++2.13 mimaReportBinaryIssues`, `scalafmtCheckAll`) on Java 17. For a near-empty `scala-3` source tree + a MIGRATION.md notes-column edit, all 5 gates should pass identically to upstream's baseline — modulo the new empty macros_3.jar artifact passing through the build. If CI fails for non-code reasons (flaky MongoDB setup, transient network), retry once via `gh run rerun`.

PR target: `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02).

Manual-merge rule: per global `~/.claude/CLAUDE.md` ("Never merge PRs or MRs automatically") and project ROADMAP §"Per-PR Workflow" step 12 ("Maintainer merges manually — Claude never merges"). Claude opens the PR; the maintainer merges.

Local sanity gate (Task 2): the FULL 5-gate `make ci` equivalent is overkill for a docs+source-dir-only PR. The minimal-but-sufficient local gate is:
- `sbt scalafmtCheckAll` (cheap; covers any fallback `.scala` file)
- `sbt '++3 macros/compile'` (this phase's primary cross-build assertion)
- `sbt '++3 core/compile'` (downstream resolution sanity)
- `sbt '++2.13 jvm/compile'` (2.13 side regression guard)

The full matrix runs on GitHub Actions; local pre-push is the cheap subset that catches obvious breakage.
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Pre-push local sanity gate — scalafmt + targeted compiles</name>
  <files>(none — sbt invocations)</files>
  <read_first>
    - .planning/phases/03-macros-scala-3-stub/03-01-SUMMARY.md (confirm Plan 01 outcomes)
  </read_first>
  <action>
    Run the local minimum-but-sufficient sanity gate before requesting the push ack:

        cd /Users/bkozak/IdeaProjects/scala-commons3

        # Branch sanity
        git rev-parse --abbrev-ref HEAD                # MUST print `03-macros-scala-3-stub`
        git status --porcelain                          # MUST be empty
        [ "$(git log upstream/scala-3..HEAD --oneline | wc -l | tr -d ' ')" = "2" ] && echo OK || { echo "ERROR: expected 2 commits"; exit 2; }

        # Compile gates
        sbt -batch scalafmtCheckAll
        sbt -batch '++3 macros/compile'
        sbt -batch '++3 core/compile'
        sbt -batch '++2.13 jvm/compile'

    Each command MUST exit 0. If any fails:
    1. STOP. Do NOT proceed to the push ack.
    2. Surface the failing command + last 40 lines of output to the user.
    3. If the failure is in `scalafmtCheckAll` and was caused by the fallback `package.scala` file, run `sbt scalafmtAll`, amend the build commit (`git commit --amend --no-edit`), and re-run the gate. Only amend if amending the immediately-preceding commit is unambiguous; if both commits are present and either could be at fault, STOP and surface.
    4. For any other failure, STOP and surface; do not attempt automated remediation.

    No-leakage spot check (cheap belt-and-suspenders before push):

        git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning' | grep -Fxq 0    # MUST exit 0
        git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' && { echo 'ERROR: GSD nomenclature'; exit 3; } || true
        git diff upstream/scala-3..HEAD | grep -E '^\+.*(@nowarn|-Wconf)' && { echo 'ERROR: new warning suppression'; exit 4; } || true
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; git rev-parse --abbrev-ref HEAD | grep -Fxq '03-macros-scala-3-stub' &amp;&amp; [ -z "$(git status --porcelain)" ] &amp;&amp; [ "$(git log upstream/scala-3..HEAD --oneline | wc -l | tr -d ' ')" = "2" ] &amp;&amp; sbt -batch scalafmtCheckAll &amp;&amp; sbt -batch '++3 macros/compile' &amp;&amp; sbt -batch '++3 core/compile' &amp;&amp; sbt -batch '++2.13 jvm/compile' &amp;&amp; [ "$(git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning')" = "0" ] &amp;&amp; ! git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; ! git diff upstream/scala-3..HEAD | grep -E '^\+.*(@nowarn|-Wconf)'</automated>
  </verify>
  <acceptance_criteria>
    - Current branch is `03-macros-scala-3-stub`; working tree clean; 2 commits ahead of `upstream/scala-3`.
    - `sbt -batch scalafmtCheckAll` exits 0.
    - `sbt -batch '++3 macros/compile'` exits 0.
    - `sbt -batch '++3 core/compile'` exits 0.
    - `sbt -batch '++2.13 jvm/compile'` exits 0.
    - No `.planning/` paths in branch history; no GSD nomenclature in any commit message; no new `@nowarn`/`-Wconf` introduced.
  </acceptance_criteria>
  <done>Local sanity gate green; branch ready to surface to user for push ack.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 2: User ack before push to origin</name>
  <files>(none — gate)</files>
  <what-built>
    Local branch `03-macros-scala-3-stub` on `/Users/bkozak/IdeaProjects/scala-commons3` carries the complete Phase 3 slice on top of `upstream/scala-3`:
    - Commit 1 (Plan 01 Task 2): `build(macros): add empty scala-3 source tree for cross-build stub` — creates `macros/src/main/scala-3/` (anchored by `.gitkeep` OR fallback placeholder `package.scala`).
    - Commit 2 (Plan 01 Task 3): `docs(migration): mark macros scala-3 stub as landed` — updates `MIGRATION.md` `macros` row notes column.

    Pre-push sanity (asserted in Task 1):
    - 2 commits ahead of `upstream/scala-3`
    - 2 files modified: `macros/src/main/scala-3/{.gitkeep|.../package.scala}` and `MIGRATION.md`
    - 0 `.planning/` paths in any commit
    - 0 GSD-vocabulary occurrences in any commit message
    - 0 new `@nowarn` / `-Wconf` introduced (REQ QUALITY-01)
    - `sbt scalafmtCheckAll`: green
    - `sbt '++3 macros/compile'`: green
    - `sbt '++3 core/compile'`: green
    - `sbt '++2.13 jvm/compile'`: green
  </what-built>
  <how-to-verify>
    1. Inspect the branch:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       git log upstream/scala-3..HEAD --oneline
       git diff upstream/scala-3..HEAD --stat
       git diff upstream/scala-3..HEAD -- MIGRATION.md
       ```
       Confirm: 2 commits; 2 files touched (the macros/scala-3/ anchor + MIGRATION.md); no `.planning/` paths.

    2. Verify the macros stub strategy chosen:
       ```
       ls -la /Users/bkozak/IdeaProjects/scala-commons3/macros/src/main/scala-3
       find /Users/bkozak/IdeaProjects/scala-commons3/macros/src/main/scala-3 -type f
       ```
       Confirm: either a single `.gitkeep` (preferred) or a single `com/avsystem/commons/macros/package.scala` (fallback). If `package.scala` is present, read it — content should be exactly two non-blank lines: `package com.avsystem.commons` and `package object macros` (no implementations, no `inline`/`given`/`quotes` imports). Plan 01's SUMMARY records which path was taken.

    3. Verify the MIGRATION.md row update is the minimum diff (just the notes column):
       ```
       grep -E '^\| macros \|' /Users/bkozak/IdeaProjects/scala-commons3/MIGRATION.md
       ```
       Expected:
       ```
       | macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty (empty scala-3/ source tree). |
       ```

    4. (Optional, slow) Re-run the local 4-command sanity gate yourself:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       sbt -batch scalafmtCheckAll
       sbt -batch '++3 macros/compile'
       sbt -batch '++3 core/compile'
       sbt -batch '++2.13 jvm/compile'
       ```
       All MUST exit 0.

    5. When satisfied, ack the push. After your ack, Claude will run:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       git push -u origin 03-macros-scala-3-stub
       ```
       Push target: your fork `origin` = `halotukozak/scala-commons3`. Push target is NOT `upstream`.
  </how-to-verify>
  <action>Pause execution. Present the verification steps above to the user and wait for their reply. Do NOT push to origin until the user responds with `ack push`.</action>
  <verify><automated>echo 'checkpoint — requires human ack'</automated></verify>
  <done>User has replied with `ack push` (or amended-and-acked). Branch ready to push.</done>
  <resume-signal>Reply `ack push` to authorize `git push -u origin 03-macros-scala-3-stub`. Reply with issue list to block.</resume-signal>
</task>

<task type="auto">
  <name>Task 3: Push branch to origin (after Task 2 ack); wait for CI green</name>
  <files>(none — git/gh plumbing)</files>
  <read_first>(none)</read_first>
  <action>
    Only run after Task 2 ack. From project root:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git push -u origin 03-macros-scala-3-stub

    Then wait for GitHub Actions to start on the pushed branch (typically <30s). Poll status:

        gh run list --branch 03-macros-scala-3-stub --limit 5

    The CI run should appear with status `queued` then `in_progress`. Do NOT proceed to Task 4 until at least one workflow run completes with conclusion `success`.

    Watch the latest run:

        gh run watch $(gh run list --branch 03-macros-scala-3-stub --limit 1 --json databaseId --jq '.[0].databaseId')

    If conclusion is `failure`:
    1. Surface the failed workflow URL via `gh run view --log-failed`.
    2. Diagnose. For a stub-only PR the most likely failure modes are: (a) MongoDB setup timeout (transient), (b) Node.js mirror flake (transient), (c) scalafmt drift from a fallback `package.scala` (real — fix locally, force-push, restart). Anything else (compile errors on Scala 3, MiMa surprises on 2.13) is unexpected — surface to user.
    3. If diagnosis suggests transient flake: rerun once with `gh run rerun <run-id>` and re-watch.
    4. If failure repeats or has a genuine cause: STOP and surface to user; do NOT request the PR ack on a red CI.

    If conclusion is `success`, proceed to Task 4.

    Per REQ WORKFLOW-03: NO PR is opened in this task. PR creation requires a second user ack in Task 4.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; git ls-remote --heads origin 03-macros-scala-3-stub | grep -q 03-macros-scala-3-stub &amp;&amp; gh run list --branch 03-macros-scala-3-stub --limit 1 --json conclusion --jq '.[0].conclusion' | grep -Fxq 'success' &amp;&amp; [ "$(gh pr list --head 03-macros-scala-3-stub --state open --json number --jq 'length')" -eq 0 ]</automated>
  </verify>
  <acceptance_criteria>
    - `git ls-remote --heads origin 03-macros-scala-3-stub` prints a non-empty line — branch exists on origin.
    - `gh run list --branch 03-macros-scala-3-stub --limit 1 --json conclusion --jq '.[0].conclusion'` prints exactly `success`.
    - No PR has been opened yet: `gh pr list --head 03-macros-scala-3-stub --state open --json number --jq 'length'` prints `0` (REQ WORKFLOW-03 — wait for second ack).
    - Pushed branch HEAD matches local: `git rev-parse origin/03-macros-scala-3-stub` equals `git rev-parse HEAD`.
  </acceptance_criteria>
  <done>Branch pushed to `origin/03-macros-scala-3-stub`; GitHub Actions reports `success`; no PR opened yet.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: User ack before PR open</name>
  <files>(none — gate)</files>
  <what-built>
    `origin/03-macros-scala-3-stub` carries 2 commits on top of `upstream/scala-3` (`build(macros):` + `docs(migration):`), GitHub Actions CI green on the 5-gate matrix. Ready to open a PR against `AVSystem/scala-commons:scala-3`.
  </what-built>
  <how-to-verify>
    1. Inspect the pushed branch on GitHub:
       - Open `https://github.com/halotukozak/scala-commons3/tree/03-macros-scala-3-stub`.
       - Confirm green CI badge / latest run status.
       - Confirm 2 commits visible.
       - Confirm commit subjects are `build(macros):` and `docs(migration):`; no GSD nomenclature in subject or body.
       - Confirm the diff against `AVSystem/scala-commons:scala-3` shows exactly 2 files: the new `macros/src/main/scala-3/` anchor and `MIGRATION.md`.
       - Confirm NO `.planning/` paths in diff.

    2. Spot-check the macros stub artifact in GitHub UI: navigate to `macros/src/main/scala-3/` on the branch view; confirm only the chosen anchor file (`.gitkeep` OR `package.scala`) is present, no other files.

    3. Draft PR title and body (suggested — confirm or amend before ack):
       - Title: `Add empty macros/src/main/scala-3/ for Scala 3 cross-build stub`
       - Body summary: introduces the Scala 3 source tree for the `macros` module so `dependsOn(macros)` resolves on Scala 3 with an (intentionally) empty jar; whitebox macro impls remain Scala 2.13-only; MIGRATION.md `macros` row notes column updated to reflect the landing.

    4. When satisfied, ack the PR open. After your ack, Claude will run:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:03-macros-scala-3-stub \
         --title '...' \
         --body '...'
       ```
       PR target: `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02). PR is opened ONLY — maintainer merges manually.
  </how-to-verify>
  <action>Pause execution. Present the verification steps above to the user, surface the GitHub Actions run URL, and wait for the user's reply. Do NOT open the PR until the user responds with `ack pr` (with optional amended title/body).</action>
  <verify><automated>echo 'checkpoint — requires human ack'</automated></verify>
  <done>User has replied with `ack pr`. Title and body locked in for Task 5.</done>
  <resume-signal>Reply `ack pr` (with optional title/body text or amendments) to authorize `gh pr create`. Reply with issue list to block.</resume-signal>
</task>

<task type="auto">
  <name>Task 5: Open PR against AVSystem/scala-commons:scala-3 (after Task 4 ack); do NOT merge</name>
  <files>(none — gh API call)</files>
  <read_first>(none)</read_first>
  <action>
    Only run after Task 4 ack. The user may amend the title/body during the ack — use their values exactly; otherwise use the defaults below.

    Default title:

        Add empty macros/src/main/scala-3/ for Scala 3 cross-build stub

    Default body (multi-line via HEREDOC):

        Adds an empty `macros/src/main/scala-3/` source tree so the `macros` sbt module cross-builds under Scala 3. Downstream modules that `dependsOn(macros)` (notably `core`, `core-js`, `mongo`, `hocon`) now resolve a valid `macros_3` artifact and can compile on Scala 3 without inheriting the whitebox / `scala.reflect`-based macro implementations.

        ## What this PR adds

        - **`macros/src/main/scala-3/.gitkeep`** (or, as fallback if sbt rejects an empty source set, a placeholder `macros/src/main/scala-3/com/avsystem/commons/macros/package.scala` declaring an empty `package object macros`). The chosen anchor lands the directory in git; sbt's `unmanagedSourceDirectories` resolves it lazily via the existing `scala-${binaryVersion}` convention. No `build.sbt` changes required — the `macros` module already declares `crossScalaVersions := Seq(scala3Version, scala2Version)`.
        - **`MIGRATION.md`** — `macros` row notes column updated to reflect the landed stub.

        ## What this PR does NOT add

        - No Scala 3 reimplementation of any macro. The Scala 3 jar is intentionally empty (or holds only an empty `package object macros`). Per-call-site replacements (likely via `inline` / `given` / `quotes`) land in later module-port PRs (`core` baseline, `cbor` cleanup, etc.).
        - No changes to `macros/src/main/scala-2.13/`. The whitebox 2.13 macros are untouched and remain the source of truth for Scala 2.13 builds.
        - No `build.sbt` / `project/Commons.scala` / `.scalafmt.conf` / `.github/workflows/ci.yml` edits.

        ## Verification

        - `sbt '++3 macros/compile'` — green (Scala 3 cross-build foothold established).
        - `sbt '++3 core/compile'` — green (downstream `dependsOn(macros)` resolves on Scala 3).
        - `sbt '++2.13 macros/compile'` — green (no regression on Scala 2.13 whitebox macros).
        - `sbt scalafmtCheckAll` — green.
        - CI matrix from upstream `scala-3` (5 gate commands on Java 17) passes green on the head branch.

    Run from project root:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        gh pr create \
          --repo AVSystem/scala-commons \
          --base scala-3 \
          --head halotukozak:03-macros-scala-3-stub \
          --title 'Add empty macros/src/main/scala-3/ for Scala 3 cross-build stub' \
          --body "$(cat <<'PR_BODY_EOF'
        ...body content above, verbatim...
        PR_BODY_EOF
        )"

    DO NOT auto-merge. Per global rule (`~/.claude/CLAUDE.md`): "Never merge PRs or MRs automatically. Create the PR/MR and leave it for manual review and merge." Per project ROADMAP §"Per-PR Workflow" step 12: "Maintainer merges manually — Claude never merges."

    Capture the PR URL for the SUMMARY:

        PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head 03-macros-scala-3-stub --state open --json number --jq '.[0].number')
        PR_URL=$(gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json url --jq '.url')
        echo "PR_URL=$PR_URL"
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head 03-macros-scala-3-stub --state open --json url --jq '.[0].url' | grep -E '^https://github\.com/AVSystem/scala-commons/pull/[0-9]+$' &amp;&amp; [ "$(gh pr list --repo AVSystem/scala-commons --head 03-macros-scala-3-stub --state open --json number --jq 'length')" -eq 1 ] &amp;&amp; PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head 03-macros-scala-3-stub --state open --json number --jq '.[0].number') &amp;&amp; gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json baseRefName --jq '.baseRefName' | grep -Fxq 'scala-3' &amp;&amp; gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json state --jq '.state' | grep -Fxq 'OPEN' &amp;&amp; ! gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json title --jq '.title' | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; ! gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json body --jq '.body' | grep -iE 'gsd|phase [0-9]|plan-phase|\.planning/'</automated>
  </verify>
  <acceptance_criteria>
    - Exactly one open PR with head `03-macros-scala-3-stub` against `AVSystem/scala-commons` — `gh pr list --repo AVSystem/scala-commons --head 03-macros-scala-3-stub --state open --json number --jq 'length'` prints `1`.
    - PR base is `scala-3` — `gh pr view --repo AVSystem/scala-commons <num> --json baseRefName --jq '.baseRefName'` prints `scala-3`.
    - PR state is `OPEN` (not merged) — `gh pr view ... --json state --jq '.state'` prints `OPEN`.
    - PR title contains no GSD nomenclature — `gh pr view ... --json title --jq '.title' | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - PR body contains no GSD nomenclature AND no `.planning/` reference — `gh pr view ... --json body --jq '.body' | grep -iE 'gsd|phase [0-9]|plan-phase|\.planning/'` exits 1.
    - PR URL recorded for SUMMARY.
  </acceptance_criteria>
  <done>PR opened against `AVSystem/scala-commons:scala-3` with title/body confirmed by user; PR remains OPEN for maintainer manual merge; URL recorded in SUMMARY.</done>
</task>

</tasks>

<verification>
After Plan 02 completes:

1. `git ls-remote --heads origin 03-macros-scala-3-stub` prints a non-empty line (branch pushed).
2. `gh run list --branch 03-macros-scala-3-stub --limit 1 --json conclusion --jq '.[0].conclusion'` prints `success`.
3. `gh pr list --repo AVSystem/scala-commons --head 03-macros-scala-3-stub --state open --json number --jq 'length'` prints `1`.
4. PR base is `scala-3`; PR state is `OPEN` (not merged).
5. No GSD nomenclature in PR title or body (REQ WORKFLOW-04).
6. No `.planning/` reference in PR title, body, or diff (REQ WORKFLOW-05).
</verification>

<success_criteria>
- Local pre-push sanity gate green: scalafmt + 3 targeted compiles.
- User explicitly acked the push (REQ WORKFLOW-03).
- Branch pushed to fork `origin/03-macros-scala-3-stub` (REQ WORKFLOW-01).
- GitHub Actions CI green on the pushed branch.
- User explicitly acked the PR open (REQ WORKFLOW-03).
- PR opened against `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02).
- PR left OPEN — Claude does NOT merge (global rule).
- No GSD nomenclature anywhere (REQ WORKFLOW-04); no `.planning/` paths anywhere (REQ WORKFLOW-05); no new `@nowarn`/`-Wconf` introduced (REQ QUALITY-01).
</success_criteria>

<output>
After completion, create `.planning/phases/03-macros-scala-3-stub/03-02-SUMMARY.md` capturing:
- Pushed branch HEAD SHA on `origin/03-macros-scala-3-stub`
- GitHub Actions run URL + conclusion
- PR URL + number + base + head + state
- Confirmation that PR title and body contain no GSD nomenclature and no `.planning/` references
- Confirmation that PR is OPEN (not merged)
- Final note: branch + PR await maintainer manual merge per global rule
</output>
</content>
</invoke>