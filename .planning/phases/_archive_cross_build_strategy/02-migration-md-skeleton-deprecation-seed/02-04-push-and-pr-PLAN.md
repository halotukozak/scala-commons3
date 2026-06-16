---
phase: 02-migration-md-skeleton-deprecation-seed
plan: 04
type: execute
wave: 4
depends_on: [01, 02, 03]
files_modified: []
autonomous: false
commit_docs: false
requirements: [WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05]
must_haves:
  truths:
    - "User explicitly acks before `git push origin 02-migration-md`"
    - "Branch `02-migration-md` pushed to `origin` (the fork `halotukozak/scala-commons3`)"
    - "GitHub Actions CI on the pushed branch reports `success` (5-gate matrix from Phase 1 still passes — docs PR shouldn't perturb anything)"
    - "User explicitly acks before `gh pr create`"
    - "PR opened against `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02)"
    - "PR title and body contain no GSD nomenclature"
    - "PR is left OPEN — Claude does NOT merge (global rule)"
  artifacts:
    - path: "(remote) origin/02-migration-md"
      provides: "Pushed branch carrying the MIGRATION.md skeleton + deprecation seed"
      contains: "MIGRATION.md"
    - path: "(remote) PR on AVSystem/scala-commons"
      provides: "Open PR against the scala-3 branch"
      contains: "docs(migration)"
  key_links:
    - from: "local 02-migration-md branch"
      to: "origin/02-migration-md"
      via: "git push -u origin 02-migration-md"
      pattern: "02-migration-md"
    - from: "origin/02-migration-md"
      to: "AVSystem/scala-commons PR against scala-3"
      via: "gh pr create --repo AVSystem/scala-commons --base scala-3"
      pattern: "scala-3"
---

<objective>
Gate Phase 2 behind two explicit user acks: (1) before `git push -u origin 02-migration-md`, (2) before `gh pr create --repo AVSystem/scala-commons --base scala-3`. CI must report `success` on the pushed branch before the second ack is requested. Claude opens the PR — Claude does NOT merge.

Purpose: Satisfies REQ WORKFLOW-01..05 — branch off upstream, push to fork, target upstream `scala-3` branch, user-ack gates, no GSD nomenclature, no `.planning/` in diff. Mirrors Phase 1 Plan 03's tasks 4-7 pattern.

Output: `origin/02-migration-md` pushed, CI green, open PR URL recorded against `AVSystem/scala-commons:scala-3`. PR left for maintainer manual merge.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/ROADMAP.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-01-SUMMARY.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-02-SUMMARY.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-03-SUMMARY.md
</context>

<interfaces>
Git remotes (verified at planning time):
- `origin` = `https://github.com/halotukozak/scala-commons3.git` (the fork, push target)
- `upstream` = `https://github.com/AVSystem/scala-commons.git` (PR target)

Fork owner string for the `gh pr create --head <owner>:<branch>` argument: `halotukozak` (derived from origin URL: `github.com/halotukozak/scala-commons3.git` → owner segment `halotukozak`).

Note: the fork repo name is `scala-commons3` while upstream is `scala-commons`. The `gh pr create --head <owner>:<branch>` form addresses the BRANCH on `<owner>`'s fork — GitHub matches it against the head repo automatically when the fork is registered as a fork of `AVSystem/scala-commons` on GitHub's side. If gh complains that the head fork is not registered as a fork on GitHub, surface to user — the project metadata on GitHub needs the fork relationship configured.

CI gate: the 5-gate matrix from Phase 1 Plan 03 runs on every push. For a docs-only PR, all 5 commands should pass identically to upstream's baseline. If CI fails for non-docs reasons (flaky MongoDB setup, transient network), retry once via `gh run rerun`.

PR target: `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02).

Manual-merge rule: per global `~/.claude/CLAUDE.md` ("Never merge PRs or MRs automatically") and project ROADMAP §"Per-PR Workflow" step 12 ("Maintainer merges manually — Claude never merges"). Claude opens the PR; the maintainer merges.
</interfaces>

<tasks>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 1: User ack before push to origin</name>
  <files>(none — gate)</files>
  <what-built>
    Local branch `02-migration-md` on `/Users/bkozak/IdeaProjects/scala-commons3` carries the complete Phase 2 documentation slice on top of `upstream/scala-3`:
    - Commit 1 (Plan 01): `docs(migration): add MIGRATION.md skeleton with per-module status and 2.13-only sections` — creates `MIGRATION.md` with H1, How-to-update (5 rules), Per-module status (13 rows), 2.13-only modules (jetty/analyzer/spring/RPC rationale), empty Deprecation log heading.
    - Commit 2 (Plan 02): `docs(migration): seed deprecation log from @deprecated scan of master` — populates `## Deprecation log` with `git grep` output, master SHA cite, `### core/` and `### mongo/` subheadings, ~152 tagged entries.

    Branch hygiene (asserted in Plan 03):
    - 2 commits ahead of `upstream/scala-3`
    - 1 file modified: `MIGRATION.md`
    - 0 `.planning/` paths in any commit
    - 0 GSD-vocabulary occurrences in any commit message
    - check.sh content invariants: ALL GREEN
    - `sbt scalafmtCheckAll`: green
    - `sbt '++2.13 jvm/compile'`: green
  </what-built>
  <how-to-verify>
    1. Inspect the branch:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       git log upstream/scala-3..HEAD --oneline
       git diff upstream/scala-3..HEAD --stat
       git diff upstream/scala-3..HEAD -- MIGRATION.md | head -80
       ```
       Confirm: 2 commits, 1 file (MIGRATION.md), expected diff content, no `.planning/` paths.

    2. Read MIGRATION.md end-to-end (tone check — locked decisions from CONTEXT.md):
       ```
       less /Users/bkozak/IdeaProjects/scala-commons3/MIGRATION.md
       ```
       Confirm:
       - H1 is `# Scala 3 Migration Status`
       - 5 numbered rules under `## How to update`
       - 13-row status table
       - 4 rationale paragraphs under `## 2.13-only modules` (jetty/analyzer/spring/RPC)
       - Deprecation log seeded with `### core/` and `### mongo/` blocks, each line `path:line — symbol — "msg" [port|skip-port]`
       - No emoji, no `✓`, no "we"/"our", no GSD vocabulary

    3. Re-run the content invariants harness:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh
       ```
       MUST end with `ALL CHECKS GREEN (<N> assertions)`.

    4. Re-run build sanity:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       sbt -batch scalafmtCheckAll
       sbt -batch '++2.13 jvm/compile'
       ```
       Both MUST exit 0.

    5. When satisfied, ack the push. After your ack, Claude will run:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       git push -u origin 02-migration-md
       ```
       Push target: your fork `origin` = `halotukozak/scala-commons3`. Push target is NOT `upstream`.
  </how-to-verify>
  <action>Pause execution. Present the verification steps above to the user and wait for their reply. Do NOT push to origin until the user responds with `ack push`.</action>
  <verify><automated>echo 'checkpoint — requires human ack'</automated></verify>
  <done>User has replied with `ack push` (or amended-and-acked). Branch ready to push.</done>
  <resume-signal>Reply `ack push` to authorize `git push -u origin 02-migration-md`. Reply with issue list to block.</resume-signal>
</task>

<task type="auto">
  <name>Task 2: Push branch to origin (after Task 1 ack); wait for CI green</name>
  <files>(none — git/gh plumbing)</files>
  <read_first>(none)</read_first>
  <action>
    Only run after Task 1 ack. From project root:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git push -u origin 02-migration-md

    Then wait for GitHub Actions to start on the pushed branch (typically <30s). Poll status:

        gh run list --branch 02-migration-md --limit 5

    The CI run should appear with status `queued` then `in_progress`. Do NOT proceed to Task 3 until at least one workflow run completes with conclusion `success`.

    Watch the latest run:

        gh run watch $(gh run list --branch 02-migration-md --limit 1 --json databaseId --jq '.[0].databaseId')

    If conclusion is `failure`:
    1. Surface the failed workflow URL via `gh run view --log-failed`.
    2. Diagnose. For a docs-only PR, almost any failure is infrastructure-flake — MongoDB setup timeout, Node.js mirror, transient network. Genuine code failure is extremely unlikely.
    3. If diagnosis suggests transient flake: rerun once with `gh run rerun <run-id>` and re-watch.
    4. If failure repeats or has a genuine cause: STOP and surface to user; do NOT request the PR ack on a red CI.

    If conclusion is `success`, proceed to Task 3.

    Per REQ WORKFLOW-03: NO PR is opened in this task. PR creation requires a second user ack in Task 3.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; git ls-remote --heads origin 02-migration-md | grep -q 02-migration-md &amp;&amp; gh run list --branch 02-migration-md --limit 1 --json conclusion --jq '.[0].conclusion' | grep -Fxq 'success' &amp;&amp; [ "$(gh pr list --head 02-migration-md --state open --json number --jq 'length')" -eq 0 ]</automated>
  </verify>
  <acceptance_criteria>
    - `git ls-remote --heads origin 02-migration-md` prints a non-empty line — branch exists on origin.
    - `gh run list --branch 02-migration-md --limit 1 --json conclusion --jq '.[0].conclusion'` prints exactly `success`.
    - No PR has been opened: `gh pr list --head 02-migration-md --state open --json number --jq 'length'` prints `0` (REQ WORKFLOW-03 — wait for second ack).
    - Pushed branch HEAD matches local: `git rev-parse origin/02-migration-md` equals `git rev-parse HEAD`.
  </acceptance_criteria>
  <done>Branch pushed to `origin/02-migration-md`; GitHub Actions reports `success`; no PR opened yet.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 3: User ack before PR open</name>
  <files>(none — gate)</files>
  <what-built>
    `origin/02-migration-md` carries 2 `docs(migration):` commits on top of `upstream/scala-3`, GitHub Actions CI green on the 5-gate matrix. Ready to open a PR against `AVSystem/scala-commons:scala-3`.
  </what-built>
  <how-to-verify>
    1. Inspect the pushed branch on GitHub:
       - Open `https://github.com/halotukozak/scala-commons3/tree/02-migration-md` (your fork view).
       - Confirm green CI badge / latest run status.
       - Confirm 2 commits visible.
       - Confirm commit subjects are `docs(migration):` prefixed; no GSD nomenclature in subject or body.
       - Confirm the diff against `AVSystem/scala-commons:scala-3` is exactly `MIGRATION.md` (one file added).
       - Confirm NO `.planning/` paths in diff.

    2. (Optional) Render MIGRATION.md in GitHub's UI — tables, fenced blocks, and section headings should render cleanly. Spot-check that the deprecation log code blocks are not too wide (no horizontal scroll surprises) and that the `[port]`/`[skip-port]` tags are legible.

    3. Draft PR title and body (suggested — confirm or amend before ack):
       - Title: `Add MIGRATION.md tracking doc with per-module status and deprecation log seed`
       - Body summary: introduces the top-level MIGRATION.md, explains the per-module status table conventions, documents the 2.13-only carveouts, captures the seed grep + tagging policy.

    4. When satisfied, ack the PR open. After your ack, Claude will run:
       ```
       cd /Users/bkozak/IdeaProjects/scala-commons3
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:02-migration-md \
         --title '...' \
         --body '...'
       ```
       PR target: `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02). PR is opened ONLY — maintainer merges manually.
  </how-to-verify>
  <action>Pause execution. Present the verification steps above to the user, surface the GitHub Actions run URL, and wait for the user's reply. Do NOT open the PR until the user responds with `ack pr` (with optional amended title/body).</action>
  <verify><automated>echo 'checkpoint — requires human ack'</automated></verify>
  <done>User has replied with `ack pr`. Title and body locked in for Task 4.</done>
  <resume-signal>Reply `ack pr` (with optional title/body text or amendments) to authorize `gh pr create`. Reply with issue list to block.</resume-signal>
</task>

<task type="auto">
  <name>Task 4: Open PR against AVSystem/scala-commons:scala-3 (after Task 3 ack); do NOT merge</name>
  <files>(none — gh API call)</files>
  <read_first>(none)</read_first>
  <action>
    Only run after Task 3 ack. The user may amend the title/body during the ack — use their values exactly; otherwise use the defaults below.

    Default title:

        Add MIGRATION.md tracking doc with per-module status and deprecation log seed

    Default body (multi-line via HEREDOC):

        Introduces a single top-level `MIGRATION.md` to track Scala 3 cross-compile status of each module, surface the modules that remain 2.13-only, and seed a deprecation log from the `@deprecated` annotations present in fork `master`. Documentation only — no source or build changes in this PR.

        ## What this PR adds

        - **`## Per-module status` table** — one row per logical module (macros, made, core, hocon, mongo, mongo-js, core-js, benchmark3, jetty, analyzer, spring, RPC, cbor) with single-token status (`cross` / `stub` / `2.13-only` / `pending` / `wip`) and explicit MiMa / Tasty-MiMa state. Initial state reflects the post-Phase-1 build infrastructure (jvm/jvm2/js aggregates from `project/Commons.scala`).
        - **`## 2.13-only modules`** — rationale paragraphs for `jetty` (servlet/RPC churn, lives under `jvm2`), `analyzer` (Scala 2 compiler-plugin against `scala.tools.nsc` internals), `spring` (upstream-deprecated; no port planned), `RPC` (logical concern under `core`; macro-stack-dependent).
        - **`## Deprecation log`** — seeded from `git grep -n '@deprecated' master -- '*.scala'`. Each entry is `path:line — symbol — "msg" [port|skip-port]`. `[skip-port]` lines have stdlib / language-feature replacements and are dropped during the port; `[port]` lines reference internal replacements and require migration work.
        - **`## How to update`** — five-line contract codifying how every subsequent PR should keep this doc current (flip the row, append discoveries, MiMa columns prove themselves).

        ## What this PR does NOT add

        - No Scala source changes.
        - No `build.sbt` / `project/Commons.scala` / `.scalafmt.conf` / `.github/workflows/ci.yml` edits.
        - No README hero badge or cross-link from README to MIGRATION.md (defer to follow-up if useful).
        - No annotated symbol→replacement table for the deprecation log (deferred to a later cbor MiMa cleanup PR).

        ## Verification

        - All status-table rows match the active `lazy val` declarations in `build.sbt` plus the documented logical groupings (`made`, `RPC`, `cbor`).
        - Deprecation log entries reproduce verbatim by running the documented seed command on a checkout that has fork `master` fetched.
        - `sbt scalafmtCheckAll` and `sbt '++2.13 jvm/compile'` are no-ops for this PR — the only changed file is `MIGRATION.md`.
        - CI matrix from upstream `scala-3` (5 gate commands on Java 17) passes green on the head branch.

    Run from project root:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        gh pr create \
          --repo AVSystem/scala-commons \
          --base scala-3 \
          --head halotukozak:02-migration-md \
          --title 'Add MIGRATION.md tracking doc with per-module status and deprecation log seed' \
          --body "$(cat <<'PR_BODY_EOF'
        ...body content above, verbatim...
        PR_BODY_EOF
        )"

    DO NOT auto-merge. Per global rule (`~/.claude/CLAUDE.md`): "Never merge PRs or MRs automatically. Create the PR/MR and leave it for manual review and merge." Per project ROADMAP §"Per-PR Workflow" step 12: "Maintainer merges manually — Claude never merges."

    Capture the PR URL for the SUMMARY:

        PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head 02-migration-md --state open --json number --jq '.[0].number')
        PR_URL=$(gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json url --jq '.url')
        echo "PR_URL=$PR_URL"
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head 02-migration-md --state open --json url --jq '.[0].url' | grep -E '^https://github\.com/AVSystem/scala-commons/pull/[0-9]+$' &amp;&amp; [ "$(gh pr list --repo AVSystem/scala-commons --head 02-migration-md --state open --json number --jq 'length')" -eq 1 ] &amp;&amp; PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head 02-migration-md --state open --json number --jq '.[0].number') &amp;&amp; gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json baseRefName --jq '.baseRefName' | grep -Fxq 'scala-3' &amp;&amp; gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json state --jq '.state' | grep -Fxq 'OPEN' &amp;&amp; ! gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json title --jq '.title' | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; ! gh pr view --repo AVSystem/scala-commons "$PR_NUM" --json body --jq '.body' | grep -iE 'gsd|phase [0-9]|plan-phase|\.planning/'</automated>
  </verify>
  <acceptance_criteria>
    - Exactly one open PR with head `02-migration-md` against `AVSystem/scala-commons` — `gh pr list --repo AVSystem/scala-commons --head 02-migration-md --state open --json number --jq 'length'` prints `1`.
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
After Plan 04 completes:

1. `git ls-remote --heads origin 02-migration-md` prints a non-empty line (branch pushed).
2. `gh run list --branch 02-migration-md --limit 1 --json conclusion --jq '.[0].conclusion'` prints `success`.
3. `gh pr list --repo AVSystem/scala-commons --head 02-migration-md --state open --json number --jq 'length'` prints `1`.
4. PR base is `scala-3`; PR state is `OPEN` (not merged).
5. No GSD nomenclature in PR title or body (REQ WORKFLOW-04).
6. No `.planning/` reference in PR title, body, or diff (REQ WORKFLOW-05).
</verification>

<success_criteria>
- User explicitly acked the push (REQ WORKFLOW-03).
- Branch pushed to fork `origin/02-migration-md` (REQ WORKFLOW-01 — branch off upstream, push to fork).
- GitHub Actions CI green on the pushed branch.
- User explicitly acked the PR open (REQ WORKFLOW-03).
- PR opened against `AVSystem/scala-commons:scala-3` (REQ WORKFLOW-02).
- PR left OPEN — Claude does NOT merge (global rule).
- No GSD nomenclature anywhere (REQ WORKFLOW-04); no `.planning/` paths anywhere (REQ WORKFLOW-05).
</success_criteria>

<output>
After completion, create `.planning/phases/02-migration-md-skeleton-deprecation-seed/02-04-SUMMARY.md` capturing:
- Pushed branch HEAD SHA on `origin/02-migration-md`
- GitHub Actions run URL + conclusion
- PR URL + number + base + head + state
- Confirmation that PR title and body contain no GSD nomenclature and no `.planning/` references
- Confirmation that PR is OPEN (not merged)
- Final note: branch + PR await maintainer manual merge per global rule
</output>
