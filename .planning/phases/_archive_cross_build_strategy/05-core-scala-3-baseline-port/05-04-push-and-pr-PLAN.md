---
phase: 05-core-scala-3-baseline-port
plan: 04
type: execute
wave: 4
depends_on: ["05-03"]
files_modified: []
autonomous: false
commit_docs: false
requirements: [WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05]

must_haves:
  truths:
    - "Branch 05-core-scala-3-baseline-port pushed to AVSystem/scala-commons (NOT fork) with -u upstream tracking"
    - "User explicitly approved the push BEFORE git push ran"
    - "PR opened against AVSystem/scala-commons:04-made-integration (cascading on PR #859 — NOT scala-3 directly)"
    - "PR opened with --draft flag (memory rule feedback_pr_draft.md)"
    - "PR title starts with `[Scala 3]` prefix (memory rule feedback_pr_title_prefix.md)"
    - "PR assigned milestone number 1 ('Scala 3') via gh api PATCH (memory rule feedback_pr_milestone.md)"
    - "User explicitly approved the PR open BEFORE gh pr create ran"
    - "CI green on the pushed branch"
    - "Claude does NOT merge the PR — manual merge only (global rule)"
  artifacts:
    - path: "(remote) AVSystem/scala-commons branch 05-core-scala-3-baseline-port"
      provides: "Phase 5 branch pushed for review"
    - path: "(remote) AVSystem/scala-commons PR (number TBD)"
      provides: "Cascading PR onto PR #859"
  key_links:
    - from: "AVSystem/scala-commons:05-core-scala-3-baseline-port"
      to: "AVSystem/scala-commons:04-made-integration"
      via: "PR base branch"
      pattern: "base.*04-made-integration"
---

<objective>
Push the Phase 5 branch to AVSystem upstream and open a cascading draft PR onto PR #859 (`04-made-integration`). Two blocking human-verify checkpoints (push, PR) per workflow contract.

Plan-level autonomy: FALSE. This plan contains two `checkpoint:human-verify` gates that block until user approves.

Output: branch pushed to AVSystem; PR opened in draft state with `[Scala 3]` prefix and milestone 1; CI confirmed green; PR URL recorded in SUMMARY.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/05-core-scala-3-baseline-port/05-03-sanity-gate-and-migration-flip-SUMMARY.md
@.planning/phases/04-made-integration/04-04-push-and-pr-SUMMARY.md
@~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md

<workflow_rules>
Memory rules MUST hold:
- `feedback_pr_title_prefix.md`: PR title MUST start with `[Scala 3]`. Use `gh api PATCH /repos/AVSystem/scala-commons/pulls/&lt;num&gt;` if `gh pr edit --title` fails on Projects-classic GraphQL.
- `feedback_pr_milestone.md`: PR MUST get milestone 1 (`Scala 3`) via `gh api PATCH /repos/AVSystem/scala-commons/issues/&lt;num&gt; -f milestone=1`.
- `feedback_pr_draft.md`: PR opened with `--draft`. User flips to ready-for-review manually.

Project rules:
- `WORKFLOW-02`: PR base is `AVSystem/scala-commons:04-made-integration` (cascading on PR #859). NOT `:scala-3`.
- `WORKFLOW-03`: Two user acks — before push, before PR open.
- `WORKFLOW-04`: No GSD nomenclature in PR title/body.
- `WORKFLOW-05`: `.planning/` not in PR diff (regression-check after push).
- Global: Claude never merges; never force-pushes to master/main; never updates git config.
</workflow_rules>

<remote_layout>
- `origin` typically points at the fork (halotukozak/scala-commons3 or similar).
- `AVSystem` (the upstream remote) is what we push to per Phase 4 precedent. Verify with `git remote -v` before pushing.
- If only `origin` exists and points at AVSystem, push to `origin` instead — the precedent in Phase 4 SUMMARY clarifies.
</remote_layout>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Pre-push sanity (re-run 5 gates + clean working tree assertion)</name>
  <files>(no edits)</files>
  <action>
Same 5-gate matrix as Plan 05-03 Task 1. ALL must be GREEN immediately before the push checkpoint:

```sh
git status                                # MUST be clean
git rev-parse --abbrev-ref HEAD           # MUST be 05-core-scala-3-baseline-port
sbt '++2.13.18 commons-core/compile' '++3.8.2 commons-core/compile' '++3.8.2 commons-macros/compile' scalafmtCheckAll
git diff 04-made-integration..HEAD | grep -E '(@nowarn|-Wconf)' &amp;&amp; { echo "QUALITY-01 violation"; exit 1; } || true
git log --oneline 04-made-integration..HEAD | grep -i -E '(gsd|planning)' &amp;&amp; { echo "WORKFLOW-04 violation"; exit 1; } || true
git diff 04-made-integration..HEAD --name-only | grep -E '^\.planning/' &amp;&amp; { echo "WORKFLOW-05 violation"; exit 1; } || true
git remote -v                             # confirm AVSystem remote exists; if absent fall back to origin per Phase 4 precedent
```
  </action>
  <verify>
    <automated>git diff --quiet &amp;&amp; git rev-parse --abbrev-ref HEAD | grep -qx 05-core-scala-3-baseline-port &amp;&amp; ! git diff 04-made-integration..HEAD --name-only | grep -qE '^\.planning/'</automated>
  </verify>
  <done>
- Working tree clean. Branch correct. 5 gates green. No memory-rule / workflow-rule violations in commit history.
  </done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 2: CHECKPOINT — push approval</name>
  <files>(none — verification checkpoint)</files>
  <action>BLOCKING checkpoint. Report the branch tip SHA, the commit list (`git log --oneline 04-made-integration..HEAD`), and a diff stat to the user, then WAIT for explicit `approved` reply before running Task 3 (the push).</action>
  <what-built>
Branch `05-core-scala-3-baseline-port` ready to push to AVSystem upstream. Tip: (executor will fill in branch tip SHA from `git rev-parse HEAD` before requesting approval). Cascades on Phase 4 PR #859 (base `04-made-integration`).

Pre-push gates:
- ++2.13.18 commons-core/compile: GREEN
- ++3.8.2 commons-core/compile: GREEN (Phase 5 goal achieved)
- ++3.8.2 commons-macros/compile: GREEN
- scalafmtCheckAll: GREEN
- No @nowarn / -Wconf / ??? introduced
- No GSD nomenclature in commit subjects
- .planning/ not in any diff
  </what-built>
  <how-to-verify>
1. Confirm executor reported correct branch tip SHA against `git log --oneline -1`.
2. Confirm executor reported a sensible commit list (`git log --oneline 04-made-integration..HEAD`) — relocation commit + scala-3 port commits + MIGRATION.md flip.
3. Optionally inspect a sample of the diff: `git diff 04-made-integration..HEAD --stat | tail`.
4. If satisfied, reply with `approved` (or equivalent). If not, describe the blocker.
  </how-to-verify>
  <resume-signal>Reply `approved` to proceed to push, or describe required changes.</resume-signal>
  <verify>User reply text contains `approved` (case-insensitive). If user describes changes instead, halt and address before retrying.</verify>
  <done>User has explicitly approved the push.</done>
</task>

<task type="auto">
  <name>Task 3: Push to AVSystem and confirm CI green</name>
  <files>(remote-only — no local edits)</files>
  <action>
1. Identify upstream remote name:
   ```sh
   git remote -v | grep AVSystem | head -1 | awk '{print $1}'   # expect 'AVSystem'
   ```
   If empty: use `origin` per Phase 4 SUMMARY's confirmation that `origin` may point at AVSystem in this clone.
2. Push with tracking:
   ```sh
   git push -u &lt;remote&gt; 05-core-scala-3-baseline-port
   ```
3. Capture the pushed SHA: `git rev-parse HEAD`.
4. Wait for CI to start and complete. Watch with `gh`:
   ```sh
   gh run watch --repo AVSystem/scala-commons -b 05-core-scala-3-baseline-port || true
   gh run list --repo AVSystem/scala-commons -b 05-core-scala-3-baseline-port --limit 1
   ```
5. If CI fails: STOP. Report failure. Do NOT proceed to PR open. (Memory rule: fix at source, not via `@nowarn`.)
6. Record CI run URL.
  </action>
  <verify>
    <automated>git ls-remote AVSystem 05-core-scala-3-baseline-port 2>/dev/null | grep -q 05-core-scala-3-baseline-port || git ls-remote origin 05-core-scala-3-baseline-port 2>/dev/null | grep -q 05-core-scala-3-baseline-port</automated>
  </verify>
  <done>
- Branch present on AVSystem (or origin if origin == AVSystem).
- CI run completed GREEN.
- Pushed SHA matches local tip.
  </done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: CHECKPOINT — PR open approval</name>
  <files>(none — verification checkpoint)</files>
  <action>BLOCKING checkpoint. Report the pushed AVSystem branch SHA, CI run URL + green status, proposed PR title, and proposed PR body to the user. WAIT for explicit `approved` reply (or revisions) before running Task 5 (`gh pr create`).</action>
  <what-built>
Branch pushed to AVSystem; CI is GREEN. Ready to open the draft PR:
- Base: `04-made-integration` (cascading on PR #859)
- Head: `05-core-scala-3-baseline-port`
- Title prefix: `[Scala 3]`
- Milestone: 1 (`Scala 3`)
- State: draft

Executor will report:
- Pushed branch SHA on AVSystem (matches local tip)
- CI run URL + green status
- Proposed PR title
- Proposed PR body (summary of relocation + cherry-pick + MIGRATION flip; CORE-01 + CORE-02 satisfied; cbor / tests / given-using sweep deferred to later phases)
  </what-built>
  <how-to-verify>
1. Confirm CI is green at the reported URL.
2. Read the proposed PR title — confirm `[Scala 3]` prefix.
3. Read the proposed PR body — confirm:
   - No GSD nomenclature.
   - No `.planning/` references.
   - Mentions cascading on PR #859.
   - Clear scope summary (what landed, what's deferred).
4. Reply `approved` to authorize PR open, or describe required body/title changes.
  </how-to-verify>
  <resume-signal>Reply `approved` to open the draft PR, or provide title/body revisions.</resume-signal>
  <verify>User reply text contains `approved` (case-insensitive). If user supplies revisions, incorporate before retrying.</verify>
  <done>User has explicitly approved the PR open.</done>
</task>

<task type="auto">
  <name>Task 5: Open draft PR, prefix title, set milestone</name>
  <files>(remote PR creation — no local edits)</files>
  <action>
1. Open the PR (draft, cascading base):
   ```sh
   gh pr create \
     --repo AVSystem/scala-commons \
     --base 04-made-integration \
     --head 05-core-scala-3-baseline-port \
     --draft \
     --title "[Scala 3] core: Scala 3 baseline port" \
     --body "$(cat &lt;&lt;'EOF'
## Summary

Establishes the Scala 3 source baseline for `core`. After this PR, `++3.8.2 commons-core/compile` is GREEN for the first time.

Two structural changes:
1. **Relocate Scala-2-only sources** — files containing `= macro` defs, whitebox macro impls, the RPC framework, and deprecated 2.13 APIs (`Sam`, `SamCompanion`, etc.) move from `core/src/main/scala/` to `core/src/main/scala-2.13/`. Git history preserved via `git mv`.
2. **Cherry-pick Scala 3 sources** — the Scala 3 baseline (annotations, collection, concurrent, jiop, derivation, meta, misc sans `compat.scala`, serialization sans `cbor/`, tuples, json) lands under `core/src/main/scala-3/`. Entry points include `GenCodec`, `GenKeyCodec`, `GenObjectCodec` (the CORE-01 typeclass derivation surface).

Cascades on #859 (`made` integration). Base intentionally set to `04-made-integration`, not `scala-3`.

## What's NOT in this PR

Deferred to later PRs (see `MIGRATION.md`):
- `core/src/main/scala-3/com/avsystem/commons/serialization/cbor/**` — cbor codec port (separate sub-package; pairs with MiMa work).
- `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala` — content is deprecated + references deferred types; not ported.
- Tests on Scala 3 — comes in a follow-up PR.
- Full `implicit` → `given`/`using`/`extension` sweep — comes in a follow-up PR.

## Verification

- `++2.13.18 commons-core/compile` — GREEN (regression guard)
- `++3.8.2 commons-core/compile` — GREEN (THE goal)
- `++3.8.2 commons-macros/compile` — GREEN (stub guard)
- `scalafmtCheckAll` — GREEN
- No `@nowarn` / `-Wconf` introduced
- No `???` stubs in non-test source
- `MIGRATION.md` `core` row updated to `cross` + Notes refreshed

## MIGRATION.md

`core` row: `pending` → `cross` on the 3.x column; Notes column rewritten to `Scala 3 compile-only baseline; tests + given/using sweep pending`.
EOF
)"
   ```
2. Capture the PR number from the URL `gh pr create` prints. Store as `PR_NUM`.
3. Confirm the title prefix landed (memory rule). If `gh pr edit --title` is needed, fall back to:
   ```sh
   gh api PATCH /repos/AVSystem/scala-commons/pulls/$PR_NUM -f title="[Scala 3] core: Scala 3 baseline port"
   ```
4. Set milestone 1 (memory rule):
   ```sh
   gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
   ```
5. Confirm PR state is draft:
   ```sh
   gh pr view $PR_NUM --repo AVSystem/scala-commons --json isDraft,title,milestone,baseRefName
   ```
   Expected: `isDraft: true`, `title: starts with [Scala 3]`, `milestone.number: 1`, `baseRefName: 04-made-integration`.
6. Confirm CI on the PR is GREEN:
   ```sh
   gh pr checks $PR_NUM --repo AVSystem/scala-commons
   ```

DO NOT merge. DO NOT mark as ready-for-review. The user flips draft → ready manually (memory rule `feedback_pr_draft.md`).
  </action>
  <verify>
    <automated>gh pr view "$(gh pr list --repo AVSystem/scala-commons --head 05-core-scala-3-baseline-port --json number --jq '.[0].number')" --repo AVSystem/scala-commons --json isDraft,title,baseRefName,milestone --jq 'select(.isDraft == true and (.title | startswith("[Scala 3]")) and .baseRefName == "04-made-integration" and .milestone.number == 1)' | grep -q .</automated>
  </verify>
  <done>
- Draft PR open on AVSystem/scala-commons, head=`05-core-scala-3-baseline-port`, base=`04-made-integration`.
- Title starts with `[Scala 3]`.
- Milestone number 1.
- CI green on the PR.
- PR URL recorded in the SUMMARY.
- PR NOT merged, NOT marked ready-for-review.
  </done>
</task>

</tasks>

<verification>
- Push: branch present on AVSystem (or origin if `origin == AVSystem`) at the expected tip SHA.
- CI: green on the pushed branch AND on the PR (they reference the same head).
- PR: draft + `[Scala 3]` title prefix + milestone 1 + base `04-made-integration`.
- Body: no GSD nomenclature, no `.planning/` paths.
- Claude did NOT merge.
</verification>

<success_criteria>
- WORKFLOW-01 / 02 / 03 / 04 / 05 satisfied for Phase 5.
- All 3 memory rules (`feedback_pr_title_prefix.md`, `feedback_pr_milestone.md`, `feedback_pr_draft.md`) honored.
- Cascading PR stack continues: Phase 5 PR depends on PR #859 depends on PR #858 depends on PR #2 depends on PR #1.
- Phase 5 is COMPLETE pending manual merge by maintainer.
</success_criteria>

<output>
After completion, create `.planning/phases/05-core-scala-3-baseline-port/05-04-push-and-pr-SUMMARY.md`. Record:
- Pushed branch SHA on AVSystem.
- CI run URL + final status.
- PR number + URL.
- PR title, base, milestone, draft status (confirmed via `gh pr view --json`).
- Full Phase 5 commit list from `git log --oneline 04-made-integration..HEAD`.
- Total time (push checkpoint → PR open checkpoint).
- Notes on any fallback paths invoked (e.g., `gh api PATCH` instead of `gh pr edit`, `origin` instead of `AVSystem`).
</output>
