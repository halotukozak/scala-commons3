---
phase: 04-made-integration
plan: 04
type: execute
wave: 4
depends_on: ["04-03"]
files_modified: []
autonomous: false
requirements:
  - WORKFLOW-02
  - WORKFLOW-03
  - WORKFLOW-04
  - WORKFLOW-05
must_haves:
  truths:
    - "User explicitly acks before `git push origin 04-made-integration`"
    - "GitHub Actions on the pushed branch reports green for the 5-gate matrix on Java 17"
    - "User explicitly acks before `gh pr create`"
    - "PR is opened against base `AVSystem/scala-commons:scala-3` (NOT against `origin/master` or `origin/scala-3`)"
    - "PR title and body contain no GSD/Claude nomenclature (WORKFLOW-04)"
    - "PR is left OPEN — Claude never merges (per ~/.claude/CLAUDE.md global rule and OUT_OF_SCOPE row 'Auto-merging PRs')"
  artifacts:
    - path: "(remote) origin/04-made-integration"
      provides: "Pushed branch reachable from GitHub Actions"
    - path: "(GitHub PR against AVSystem/scala-commons:scala-3)"
      provides: "Reviewable PR left OPEN for manual merge"
  key_links:
    - from: "Local branch `04-made-integration`"
      to: "`origin/04-made-integration` (fork remote)"
      via: "human-acked `git push origin`"
      pattern: "push origin 04-made-integration"
    - from: "`origin/04-made-integration`"
      to: "`AVSystem/scala-commons:scala-3` PR"
      via: "human-acked `gh pr create --repo AVSystem/scala-commons --base scala-3`"
      pattern: "gh pr create.*--base scala-3.*--repo AVSystem"
---

<objective>
Two human-acked workflow gates: push the branch, then open the PR. PR is left OPEN — Claude never merges. This plan is the formalized WORKFLOW-02/03/04/05 enforcement for Phase 4.

Purpose: Per `~/.claude/CLAUDE.md` and REQUIREMENTS.md WORKFLOW contract, ack-before-push and ack-before-PR are non-negotiable. CI must be green on the pushed branch before PR open.

Output: An OPEN PR on `AVSystem/scala-commons` targeting base `scala-3`, awaiting maintainer manual merge.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/REQUIREMENTS.md
@.planning/PROJECT.md
@.planning/phases/04-made-integration/04-CONTEXT.md
@.planning/phases/04-made-integration/04-01-SUMMARY.md
@.planning/phases/04-made-integration/04-02-SUMMARY.md
@.planning/phases/04-made-integration/04-03-SUMMARY.md

**Global rules to honor** (from `~/.claude/CLAUDE.md`):
- Never push directly to master/main — we push to `04-made-integration`, fine.
- Never merge PRs automatically — gate enforced below.
- Before every commit, run linter + tests — already done in Plan 03.
</context>

<tasks>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 1: Human-ack gate — review pre-push state, then push to origin</name>
  <files>(no files modified — human review gate; push happens in Task 2 after ack)</files>
  <action>Present the <what-built> and <how-to-verify> blocks below to the user. PAUSE execution until the user replies with `approved-push` (or describes blockers). DO NOT run `git push` in this task — Task 2 owns the push, conditional on the ack received here.</action>
  <what-built>
    Branch `04-made-integration` cut off `upstream/scala-3` with 4 commits:
    - `build(made): pin madeVersion to 0.1.0`
    - `feat(core/scala-3): port Opt/NOpt/OptArg/OptRef with made.Default givens`
    - `feat(core/scala-3): port madeAnnotationAliases re-exports`
    - `docs(migration): record made integration at 0.1.0; core wiring primitives ported`

    Full local 5-gate suite green per Plan 03. Diff ~6 files: `build.sbt`, `core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala`, `core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala`, `MIGRATION.md`.
  </what-built>
  <how-to-verify>
    Run these to inspect the pre-push state:

    ```sh
    # 1. Branch + commits
    git rev-parse --abbrev-ref HEAD          # → 04-made-integration
    git log upstream/scala-3..HEAD --oneline # → 4 commits, conventional prefixes, no GSD nomenclature

    # 2. Diff scope
    git diff upstream/scala-3 --stat         # → ~6 files (1 build, 4 misc, 1 aliases, 1 MIGRATION.md)

    # 3. No .planning/ leakage
    git log upstream/scala-3..HEAD --name-only --pretty=format: | sort -u | grep -E '^\.planning/' && echo BLOCKER || echo OK
    # → OK

    # 4. No SNAPSHOT
    grep -RnE '\-SNAPSHOT' build.sbt project/ || echo "OK: no SNAPSHOT"

    # 5. Re-confirm 5-gate suite (optional — Plan 03 already ran it)
    # sbt -batch '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll
    ```

    If all checks pass and you approve, type `approved-push`.

    After approval, Claude runs:
    ```sh
    git push -u origin 04-made-integration
    ```
    (Push goes to `origin` = the user's fork `halotukozak/scala-commons3`, NEVER to `upstream`. Per `~/.claude/CLAUDE.md` and PROJECT.md, the fork is the staging remote for upstream PRs.)

    DO NOT push if you find a blocker. Reply with the issue instead.
  </how-to-verify>
  <verify>
    <automated>MANUAL — checkpoint:human-verify gate. Resume-signal `approved-push` (or override) is the verification.</automated>
  </verify>
  <done>User replied with `approved-push` (push authorized) OR described a blocker (push aborted, surface to user).</done>
  <resume-signal>Type `approved-push` to authorize the push, or describe blockers.</resume-signal>
</task>

<task type="auto">
  <name>Task 2: Push branch and wait for GitHub Actions to report green</name>
  <files>(no local files modified — remote push only)</files>
  <action>
**Only proceed if the Task 1 human-verify gate returned `approved-push`.**

**Step 1 — Push:**
```sh
git push -u origin 04-made-integration
```

This pushes to `origin` (the user's fork). NEVER push to `upstream` (AVSystem) — PROJECT.md and global CLAUDE.md both forbid it.

**Step 2 — Wait for CI to start, then poll status:**
```sh
sleep 10                                  # give GH a moment to register the push
gh run list --branch 04-made-integration --limit 5
```

Find the most-recent run for this branch. Then watch it:
```sh
gh run watch --exit-status            # waits for completion, exits non-zero if any job failed
```

If `gh run watch` exits non-zero:
- Capture the failure with `gh run view --log-failed`.
- Surface to user — DO NOT proceed to PR open.
- Common fixes: scalafmt drift in upstream/scala-3 reformat baseline; environmental difference between local and CI (Plan 03's full 5-gate gate should have caught most of these).

**Step 3 — Record the CI run URL** for the PR body:
```sh
gh run list --branch 04-made-integration --limit 1 --json url --jq '.[0].url'
```
  </action>
  <verify>
    <automated>git ls-remote origin 04-made-integration | grep -q 04-made-integration && gh run list --branch 04-made-integration --limit 1 --json conclusion --jq '.[0].conclusion' | grep -q '^success$'</automated>
  </verify>
  <done>Branch is pushed to `origin/04-made-integration`; the most-recent CI run on the branch has `conclusion: success`; CI run URL recorded.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 3: Human-ack gate — review pushed branch + green CI, then open PR</name>
  <files>(no files modified — human review gate; PR opens in Task 4 after ack)</files>
  <action>Present the <what-built> and <how-to-verify> blocks below to the user. PAUSE execution until the user replies with `approved-pr` (with or without title/body overrides) or describes blockers. DO NOT run `gh pr create` in this task — Task 4 owns it.</action>
  <what-built>
    `origin/04-made-integration` pushed, GitHub Actions CI green on the 5-gate matrix (Java 17). Next action: open PR against `AVSystem/scala-commons:scala-3`.

    Proposed PR title:
      `Phase 4: made integration — pin to 0.1.0 + port Opt/NOpt/OptArg/OptRef wiring`

    Proposed PR body (no GSD nomenclature):
      ```
      ## Summary
      - Pin `madeVersion` to the published `0.1.0` release (was `0.1.1-SNAPSHOT` on the fork).
      - Port Scala 3 wiring primitives: `Opt.scala`, `NOpt.scala`, `OptArg.scala`, `OptRef.scala`
        under `core/src/main/scala-3/com/avsystem/commons/misc/` — each companion provides
        a `given Default[Wrapper[A]]` backed by `made.Default`.
      - Port `madeAnnotationAliases.scala` re-exporting `made.annotation.{generated, name,
        optionalParam, transparent, whenAbsent}` and `made.TransparentWrapping`.
      - Drop deprecated `OptCompat` / `NOptCompat` / `OptRefCompat` traits from companions —
        their single `opt2Iterable` shim is superseded by the in-companion `given
        Conversion[…, Iterable[…]]`.
      - Update `MIGRATION.md`: new `made` row; `core` row Notes appended.

      ## Out of scope (deferred)
      - `compat.scala` is NOT ported — its remaining traits reference deferred
        `GenCodec` / `GenKeyCodec` types (future PR).
      - Full `GenCodec` / `GenObjectCodec` / `GenKeyCodec` / `GenRef` / `HasGenCodec` derivation
        surface (future PRs).
      - `cbor` / `mongo` derivation refresh (future PRs).

      ## Validation
      Local 5-gate suite green: `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll`.
      GitHub Actions CI: <URL from Task 2 Step 3>.
      ```
  </what-built>
  <how-to-verify>
    1. Open the pushed branch on GitHub and inspect the diff:
       ```sh
       gh browse --branch 04-made-integration
       # or:
       echo "https://github.com/halotukozak/scala-commons3/tree/04-made-integration"
       ```
       Confirm the diff matches expectations: ~6 files changed, no `.planning/`, no SNAPSHOT.

    2. Inspect the CI run:
       ```sh
       gh run view  # interactive; pick the latest 04-made-integration run
       ```
       Confirm all 5 gates green.

    3. Approve PR title and body (or paste replacements).

    If approved, type `approved-pr`. Claude then runs:
    ```sh
    gh pr create \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:04-made-integration \
      --title "Phase 4: made integration — pin to 0.1.0 + port Opt/NOpt/OptArg/OptRef wiring" \
      --body "$BODY_FROM_ABOVE"
    ```

    Critical guards (Claude double-checks before running):
    - `--repo` is `AVSystem/scala-commons` (NOT `halotukozak/scala-commons3`).
    - `--base` is `scala-3` (NOT `master` or `main`).
    - `--head` is `halotukozak:04-made-integration` (forked PR).
    - PR title/body contains zero occurrences of: `gsd`, `Claude`, `claude`, `GSD`, `phase plan`, `must_haves`, etc. (WORKFLOW-04).
    - Claude DOES NOT add `--merge` / `--squash` / `--rebase` flags. PR is left OPEN for manual merge by the AVSystem maintainer.

    DO NOT open the PR if you find a blocker. Reply with the issue instead.
  </how-to-verify>
  <verify>
    <automated>MANUAL — checkpoint:human-verify gate. Resume-signal `approved-pr` (or override) is the verification.</automated>
  </verify>
  <done>User replied with `approved-pr` (PR open authorized; optional title/body overrides recorded) OR described a blocker (PR open aborted, surface to user).</done>
  <resume-signal>Type `approved-pr` to authorize PR open (or `approved-pr title: ... body: ...` with overrides), or describe blockers.</resume-signal>
</task>

<task type="auto">
  <name>Task 4: Open PR against AVSystem/scala-commons:scala-3 and report URL</name>
  <files>(no local files modified — gh API call only)</files>
  <action>
**Only proceed if Task 3 returned `approved-pr` (or `approved-pr` with overrides).**

**Step 1 — Open the PR:**
```sh
gh pr create \
  --repo AVSystem/scala-commons \
  --base scala-3 \
  --head halotukozak:04-made-integration \
  --title "Phase 4: made integration — pin to 0.1.0 + port Opt/NOpt/OptArg/OptRef wiring" \
  --body "$(cat <<'EOF'
## Summary
- Pin `madeVersion` to the published `0.1.0` release (was `0.1.1-SNAPSHOT` on the fork).
- Port Scala 3 wiring primitives: `Opt.scala`, `NOpt.scala`, `OptArg.scala`, `OptRef.scala`
  under `core/src/main/scala-3/com/avsystem/commons/misc/` — each companion provides
  a `given Default[Wrapper[A]]` backed by `made.Default`.
- Port `madeAnnotationAliases.scala` re-exporting `made.annotation.{generated, name,
  optionalParam, transparent, whenAbsent}` and `made.TransparentWrapping`.
- Drop deprecated `OptCompat` / `NOptCompat` / `OptRefCompat` traits from companions —
  their single `opt2Iterable` shim is superseded by the in-companion `given
  Conversion[…, Iterable[…]]`.
- Update `MIGRATION.md`: new `made` row; `core` row Notes appended.

## Out of scope (deferred)
- `compat.scala` is NOT ported — its remaining traits reference deferred
  `GenCodec` / `GenKeyCodec` types (future PR).
- Full `GenCodec` / `GenObjectCodec` / `GenKeyCodec` / `GenRef` / `HasGenCodec` derivation
  surface (future PRs).
- `cbor` / `mongo` derivation refresh (future PRs).

## Validation
Local 5-gate suite green: `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll`.
EOF
)"
```

(If the user provided title/body overrides in their `approved-pr` reply, use those instead. Be conservative — preserve the "no GSD nomenclature" rule.)

**Step 2 — Capture the PR URL:**
```sh
PR_URL=$(gh pr view --repo AVSystem/scala-commons --head halotukozak:04-made-integration --json url --jq '.url')
echo "$PR_URL"
```

**Step 3 — DO NOT merge.** Per `~/.claude/CLAUDE.md`: "Never merge PRs or MRs automatically." Per REQUIREMENTS.md OUT_OF_SCOPE: "Auto-merging PRs — Global rule — manual merge only." The maintainer at AVSystem will merge manually.

**Step 4 — Final WORKFLOW-04 grep on the live PR title+body** (defense in depth):
```sh
gh pr view "$PR_URL" --json title,body --jq '.title + " " + .body' | grep -iE '\b(gsd|claude|phase plan|must_haves)\b' && { echo BLOCKER; exit 1; } || echo "OK: no GSD nomenclature on PR"
```

**Step 5 — Update STATE.md** (only file allowed to be modified — and it's gitignored, so no commit):
```sh
# .planning/STATE.md is gitignored — safe to edit without commit
```
Mark Phase 4 as `Pending PR review` in `current_focus` / `Next` sections. (Use Read + Edit tools.)
  </action>
  <verify>
    <automated>gh pr view --repo AVSystem/scala-commons --head halotukozak:04-made-integration --json state,baseRefName --jq '"\(.state) \(.baseRefName)"' | grep -qE '^OPEN scala-3$'</automated>
  </verify>
  <done>PR is OPEN on `AVSystem/scala-commons` with base `scala-3`; PR URL recorded; no GSD nomenclature in title or body; PR NOT merged; STATE.md updated locally (no commit — gitignored).</done>
</task>

</tasks>

<verification>
End-of-phase gate:

```sh
# PR exists, OPEN, correct base
gh pr view --repo AVSystem/scala-commons --head halotukozak:04-made-integration --json state,baseRefName,title,body | jq .
# state: OPEN
# baseRefName: scala-3
# title contains no "gsd"/"claude"
# body contains no "gsd"/"claude"

# CI on the pushed branch green
gh run list --branch 04-made-integration --limit 1 --json conclusion --jq '.[0].conclusion'
# → success

# Local branch matches origin
git status --porcelain   # → empty
git rev-parse 04-made-integration == $(git rev-parse origin/04-made-integration)
```
</verification>

<success_criteria>
- [ ] Human ack received before push (`approved-push`)
- [ ] `git push -u origin 04-made-integration` succeeded
- [ ] GitHub Actions CI on the branch reports `success`
- [ ] Human ack received before PR open (`approved-pr`)
- [ ] PR opened with `--repo AVSystem/scala-commons --base scala-3 --head halotukozak:04-made-integration`
- [ ] PR title and body contain no GSD/Claude nomenclature
- [ ] PR is OPEN — not merged
- [ ] STATE.md locally reflects "Phase 4 — PR open, awaiting maintainer merge"
</success_criteria>

<output>
After completion, create `.planning/phases/04-made-integration/04-04-SUMMARY.md` documenting:
- Push timestamp + remote URL
- CI run URL and final conclusion
- PR URL
- Final PR title + body (as actually opened)
- Confirmation PR is OPEN and NOT merged
- Updated STATE.md snippet
</output>
</content>
