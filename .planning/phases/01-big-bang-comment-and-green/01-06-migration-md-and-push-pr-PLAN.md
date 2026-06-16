---
phase: 01-big-bang-comment-and-green
plan: 06
type: execute
wave: 6
depends_on: [05]
files_modified:
  - MIGRATION.md
autonomous: false
commit_docs: false
requirements: [DOC-01, DOC-02, COMPILE-01, COMPILE-02, COMPILE-03, CI-01, CI-02, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

must_haves:
  truths:
    - "MIGRATION.md at repo root has all 5 locked sections seeded"
    - "Full local 5-gate green: sbt show version + scalafmtCheckAll + compile + Test/compile + grep audits"
    - "Fork CI green on push to halotukozak/scala-commons3 01-big-bang"
    - "Draft PR open at AVSystem/scala-commons base=scala-3 head=halotukozak:01-big-bang"
    - "PR title starts with [Scala 3]; milestone = 1 (Scala 3); state = draft"
    - "AVSystem PR CI green"
  artifacts:
    - path: "MIGRATION.md"
      provides: "5-section migration contract; backlog from TODO[scala3-port] grep"
      contains: "## 1. Will not migrate"
  key_links:
    - from: "git grep -nE 'TODO\\[scala3-port\\]'"
      to: "MIGRATION.md ## Backlog"
      via: "backlog table populated from grep output"
      pattern: "TODO\\[scala3-port\\]"
    - from: "halotukozak/scala-commons3 push"
      to: "AVSystem/scala-commons PR #N"
      via: "gh pr create --draft --base scala-3 --title '[Scala 3] ...'"
      pattern: "\\[Scala 3\\]"
---

<objective>
Seed `MIGRATION.md` (5 locked sections) from grep over Plans 01-05's TODO tags + disabled modules + dropped scalac options. Run full local verify gate. Push branch to fork. Open draft PR against `AVSystem/scala-commons:scala-3` with required title prefix and milestone.

Purpose: Public-facing migration contract (`MIGRATION.md`) is the deliverable that lets downstream readers understand "what changed in this PR". The push + PR steps are the final acceptance — Phase 1 isn't done until CI is green on both fork and AVSystem PR.

Output: Committed `MIGRATION.md`. Pushed branch. Draft PR open. CI green twice (fork, then PR).
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/PROJECT.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/STATE.md
@.planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md
@.planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md
@.planning/phases/01-big-bang-comment-and-green/01-VALIDATION.md
@.planning/phases/01-big-bang-comment-and-green/01-01-SUMMARY.md
@.planning/phases/01-big-bang-comment-and-green/01-02-SUMMARY.md
@.planning/phases/01-big-bang-comment-and-green/01-03-SUMMARY.md
@.planning/phases/01-big-bang-comment-and-green/01-04-SUMMARY.md
@.planning/phases/01-big-bang-comment-and-green/01-05-SUMMARY.md

<interfaces>
MIGRATION.md 5-section contract (LOCKED, per memory `[[feedback-migration-md-contract]]` + CONTEXT):

1. **Will not migrate** — symbols/modules dropped entirely. Rationale per entry.
2. **Deprecated on Scala 3** — kept behind `@deprecated`. `since` + `replaceWith` pointers. (Empty/sparse in Phase 1 — populated as features are restored.)
3. **Source-compat breaks** — every place downstream `import`/call site changes. Per-module grouping.
4. **Binary-compat breaks** — empty for Phase 1 (no Scala 3 baseline yet). Activated later.
5. **Disabled tests / modules** — what's not running + why + restore-PR pointer if known.

Plus a **## Backlog** section (DOC-02) populated from `git grep -nE 'TODO\[scala3-port\]'` — table of location / description / effort.

PR conventions (LOCKED, from memory + CONTEXT):
- Title: `[Scala 3] Pivot to Scala 3 only — comment broken, green CI` (≤70 chars total)
- Base: `AVSystem/scala-commons:scala-3` (NOT `master`)
- Head: `halotukozak:01-big-bang`
- Draft on open (`gh pr create --draft`)
- Milestone: `Scala 3` (#1) — assigned via `gh api PATCH /repos/AVSystem/scala-commons/issues/<num> -f milestone=1` after `gh pr create` (memory rule notes `gh pr edit` may fail on Projects-classic GraphQL)

User ack required TWICE: before push, before `gh pr create` (WORKFLOW-03).
</interfaces>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Author MIGRATION.md with all 5 sections + backlog from TODO grep</name>
  <files>MIGRATION.md</files>
  <read_first>
    - .planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md ("MIGRATION.md 5-Section Skeleton")
    - .planning/phases/01-big-bang-comment-and-green/01-01-SUMMARY.md (scalac options dropped, modules dropped from aggregate)
    - .planning/phases/01-big-bang-comment-and-green/01-02-SUMMARY.md (per-module TODO counts in core/macros)
    - .planning/phases/01-big-bang-comment-and-green/01-03-SUMMARY.md (hocon/mongo/cbor/benchmark)
    - .planning/phases/01-big-bang-comment-and-green/01-04-SUMMARY.md (JS variants)
    - .planning/phases/01-big-bang-comment-and-green/01-05-SUMMARY.md (test sources)
    - Run `git grep -nE 'TODO\[scala3-port\]' -- '*.scala' | wc -l` for total backlog count.
  </read_first>
  <action>
    Create `MIGRATION.md` at repo root. Structure (LOCKED order; section headings exactly as below):

    ```markdown
    # Scala 3 Migration

    This document tracks the state of the Scala 3 migration of `scala-commons`.
    The codebase has pivoted to Scala 3 only. Features that didn't compile on Scala 3
    are commented out under `// TODO[scala3-port]:` tags and listed in the backlog at
    the bottom of this file. Restoration ships incrementally per feature area.

    ## 1. Will not migrate

    | Symbol/module | Rationale |
    |---------------|-----------|
    | `analyzer` module | Scala 2 compiler plugin; would need full Scala 3 plugin rewrite. Restored only if user demand surfaces. |
    | `jetty` module | Deprecated upstream; ee10 servlet wrapper. Out of scope until/unless restored. |
    | `spring` module | Deprecated upstream; spring-context wiring. Out of scope until/unless restored. |
    | `comprof` module | scalac-profiling plugin is Scala 2 only. Retire pending Scala 3 alternative. |
    | `-Xsource:3` flag | Obsolete — we ARE Scala 3 now. |
    | `-Wconf` blocks | Memory rule: fix warnings at source, not via suppression. |
    | `-language:experimental.macros` | Scala 3 uses inline + quotes; flag is a no-op. |
    | scala-2 macro impls (`c.universe`, whitebox/blackbox) | Replaced by Scala 3 quotes/inline during feature-area restoration. |

    ## 2. Deprecated on Scala 3

    *Populated as features are restored and intentionally kept behind `@deprecated` to ease downstream migration.*

    | Symbol | Since | Replacement |
    |--------|-------|-------------|
    | *(empty in Phase 1)* | | |

    ## 3. Source-compat breaks

    *Every downstream import/call site that changes vs the last Scala 2.13 release. Populated per restoration PR.*

    ### core
    - *(empty — no features restored yet)*

    ### mongo
    - *(empty)*

    ### hocon
    - *(empty)*

    ## 4. Binary-compat breaks

    *Empty in Phase 1 — no Scala 3 baseline released yet. MiMa activation deferred to the first `_3` release tag.*

    ## 5. Disabled tests / modules

    ### Modules (dropped from `commons-jvm` aggregate)
    - `analyzer` — Scala 2 compiler plugin. Restore: dedicated phase (effort L).
    - `jetty` — ee10 servlet wrapper. Restore: dedicated phase (effort M).
    - `spring` — spring-context wiring. Restore: dedicated phase (effort S).
    - `comprof` — scalac-profiling Scala 2 only. Restore: TBD (effort M).

    ### Test sources (commented per-file)
    *Generated count: <N> test classes commented across <M> files. Full list in Backlog below — filter rows where Location starts with `*/src/test/`.*

    ## Backlog

    *Auto-derived from `git grep -nE 'TODO\[scala3-port\]'` on this PR's tip.*

    | Location | Description | Effort |
    |----------|-------------|--------|
    | <grep output, one row per tag, parsed into 3 columns> | | |
    ```

    **Backlog table population:**
    1. Run: `git grep -nE 'TODO\[scala3-port\]' -- '*.scala' > /tmp/todo-raw.txt`
    2. For each line `path:line:    // TODO[scala3-port]: <description> (<effort>)`:
       - Location: `path:line` (relative path)
       - Description: the text between `port]:` and the optional ` (S|M|L)` suffix
       - Effort: `S`, `M`, `L`, or empty if no suffix
    3. Emit one markdown table row per tag.
    4. Sort by Location (alphabetical by path).
    5. Sanity: backlog row count = total tag count from grep.

    Memory-rule audit before commit:
    - No `.planning/` paths in diff.
    - No GSD nomenclature in commit message.
    - Conventional `docs:` prefix.

    Commit: `git add MIGRATION.md && git commit -m "docs(migration): seed Scala 3 migration backlog and module status"`.
  </action>
  <verify>
    <automated>test -f MIGRATION.md && test "$(grep -c '^## ' MIGRATION.md)" -ge 5 && grep -q '^## 1\. Will not migrate' MIGRATION.md && grep -q '^## 2\. Deprecated on Scala 3' MIGRATION.md && grep -q '^## 3\. Source-compat breaks' MIGRATION.md && grep -q '^## 4\. Binary-compat breaks' MIGRATION.md && grep -q '^## 5\. Disabled tests / modules' MIGRATION.md && grep -q '^## Backlog' MIGRATION.md && test "$(awk '/^## Backlog/,EOF' MIGRATION.md | grep -cE '^\|.*\|.*\|')" -gt 1</automated>
  </verify>
  <done>
    `MIGRATION.md` at repo root has all 5 numbered sections (Will not migrate / Deprecated / Source-compat / Bincompat / Disabled) plus a populated Backlog section. Backlog row count matches `TODO[scala3-port]` grep output. Single `docs(migration):` commit landed.
  </done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Full local 5-gate verify + hygiene audit</name>
  <files>(no file modifications — verification only)</files>
  <read_first>
    - .planning/phases/01-big-bang-comment-and-green/01-VALIDATION.md (full gate list)
    - .planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md (acceptance gates locked)
  </read_first>
  <action>
    Run every Phase 1 acceptance gate locally. ALL must exit 0 / produce expected output. If any gate fails, STOP and report — do not proceed to the push/PR checkpoints.

    **Compile gates:**
    1. `sbt -batch 'show version'` — exit 0
    2. `sbt -batch 'show scalaVersion'` — exit 0, output contains `3.8.2`
    3. `sbt -batch compile` — exit 0
    4. `sbt -batch Test/compile` — exit 0
    5. `sbt -batch scalafmtCheckAll scalafmtSbtCheck` — exit 0

    **Hygiene grep audits (vs upstream/scala-3 baseline):**
    6. `! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'` — zero new warning suppressions (QUALITY-01)
    7. `! git log upstream/scala-3..HEAD --pretty=%B | grep -qiE '(GSD|get shit done|phase[ -][0-9])'` — no GSD nomenclature in commit messages (WORKFLOW-04)
    8. `! git diff --name-only upstream/scala-3..HEAD | grep -q '^\.planning/'` — no .planning/ files committed (WORKFLOW-05)
    9. `! grep -q 'crossScalaVersions' project/Commons.scala` (BUILD-01)
    10. `! grep -q '\-Xsource:3' project/Commons.scala` (BUILD-02)
    11. `! find . -type d -name 'scala-2.13' -not -path './.planning/*' -not -path './.git/*' | grep -q .` (BUILD-03)
    12. `grep -q 'runner.dialect = scala3' .scalafmt.conf && ! grep -q 'fileOverride' .scalafmt.conf` (BUILD-04)
    13. `grep -q '3.8.2' .github/workflows/ci.yml && ! grep -q '2.13' .github/workflows/ci.yml` (BUILD-05)

    **TODO/MIGRATION sync check:**
    14. `git grep -cE 'TODO\[scala3-port\]' -- '*.scala' | awk -F: '{s+=$2} END {print s}'` — capture as `TAG_COUNT`
    15. `awk '/^## Backlog/,EOF' MIGRATION.md | grep -cE '^\|.*\|.*\|.*\|$' | head -1` — capture as `BACKLOG_ROWS` (minus the header row)
    16. Confirm BACKLOG_ROWS ≈ TAG_COUNT (±2 for header/separator counting nuance).

    Capture all results in a stdout log. If anything fails, abort with a clear error message stating which gate.

    No commit — verification only.
  </action>
  <verify>
    <automated>sbt -batch ';show version ;show scalaVersion ;scalafmtCheckAll ;scalafmtSbtCheck ;compile ;Test/compile' 2>&1 | tail -10 | grep -qE 'success' && ! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)' && ! git log upstream/scala-3..HEAD --pretty=%B | grep -qiE '(GSD|get shit done|phase[ -][0-9])' && ! git diff --name-only upstream/scala-3..HEAD | grep -q '^\.planning/' && ! grep -q 'crossScalaVersions' project/Commons.scala && ! grep -q 'Xsource:3' project/Commons.scala && grep -q 'runner.dialect = scala3' .scalafmt.conf && ! grep -q 'fileOverride' .scalafmt.conf && grep -q '3.8.2' .github/workflows/ci.yml && ! grep -q '2.13' .github/workflows/ci.yml</automated>
  </verify>
  <done>
    All 16 gates pass: build loads, compile + Test/compile + scalafmt all green; no warning suppressions, no GSD nomenclature, no .planning/ commits, no crossScalaVersions, no -Xsource:3, no scala-2.13/ dirs; .scalafmt.conf single dialect; ci.yml single Scala 3 axis; MIGRATION.md backlog in sync with TODO tags.
  </done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 3: User ack — push to halotukozak/scala-commons3</name>
  <what-built>
    Branch `01-big-bang` ready for push to fork. Contents: full Phase 1 (build pivot, macros stubbed, core+hocon+mongo+cbor+benchmark commented, JS variants commented, tests commented, MIGRATION.md seeded). All 16 local gates green.

    Push command Claude will run after ack:
    ```
    git push -u origin 01-big-bang
    ```
    (`origin` is assumed to be `halotukozak/scala-commons3` — Claude verifies with `git remote -v` before push.)
  </what-built>
  <how-to-verify>
    1. Review commit log: `git log --oneline upstream/scala-3..HEAD` — sanity-check messages, no GSD nomenclature.
    2. Review file count delta: `git diff --stat upstream/scala-3..HEAD | tail -1`.
    3. Review MIGRATION.md: `head -100 MIGRATION.md` — 5 sections present, backlog populated.
    4. Optionally spot-check a few commented files: `git grep -nE 'TODO\[scala3-port\]' core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala 2>/dev/null | head` (or similar).
    5. After push, fork CI runs: `gh run watch --repo halotukozak/scala-commons3` should be green within 10-15 min on warm caches (per VALIDATION estimate; CI is 3 shards × ~6-10 min cold).
  </how-to-verify>
  <resume-signal>
    Type "approved push" or describe any concerns. On approval, Claude runs `git push -u origin 01-big-bang` and waits for fork CI green (`gh run watch`) before continuing to Task 4. If push fails or CI red, STOP and report — do not proceed.
  </resume-signal>
  <files>(no source files; git push only)</files>
  <action>
    Wait for explicit user ack (resume-signal). On approval:
    1. `git remote -v` — confirm `origin` is halotukozak/scala-commons3.
    2. `git push -u origin 01-big-bang`.
    3. Wait for fork CI: `gh run watch --repo halotukozak/scala-commons3` until exit 0.
    4. If push or CI fails, STOP and report — do NOT proceed to Task 4.
  </action>
  <verify>
    <automated>gh run list --repo halotukozak/scala-commons3 --branch 01-big-bang --limit 1 --json conclusion -q '.[0].conclusion' | grep -q success</automated>
  </verify>
  <done>
    User approved. Branch pushed to `halotukozak/scala-commons3:01-big-bang`. Fork CI run completed successfully (3 shards: Scala 3 × Temurin 17/21/25).
  </done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: User ack — open draft PR at AVSystem/scala-commons</name>
  <what-built>
    Fork CI green on `halotukozak/scala-commons3:01-big-bang`. Ready to open the draft PR upstream.

    Commands Claude will run after ack:
    ```
    gh pr create \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:01-big-bang \
      --draft \
      --title "[Scala 3] Pivot to Scala 3 only — comment broken, green CI" \
      --body-file /tmp/pr-body.md

    # then assign milestone 1 (memory rule — gh api PATCH because gh pr edit may fail)
    PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:01-big-bang --json number -q '.[0].number')
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/${PR_NUM}" -f milestone=1
    ```

    PR body (`/tmp/pr-body.md`) Claude will author from CONTEXT/RESEARCH/SUMMARYs — concise: what changed (5 bullets), what's NOT in scope (comment-restoration is Phase 2+), pointer to MIGRATION.md for backlog. No GSD nomenclature. No `.planning/` references.
  </what-built>
  <how-to-verify>
    1. Confirm fork CI is actually green (final shard finished): `gh run list --repo halotukozak/scala-commons3 --branch 01-big-bang --limit 3`.
    2. Confirm upstream remote: `gh repo view AVSystem/scala-commons | grep -i 'default branch\|scala-3'` — `scala-3` branch must exist on AVSystem.
    3. Review the draft PR body Claude proposes (Claude will print it before opening).
    4. After PR is open, verify:
       - Title starts with `[Scala 3]`
       - State = Draft
       - Milestone = `Scala 3` (#1)
       - Base = `scala-3`
       - Head = `halotukozak:01-big-bang`
       - PR CI runs against AVSystem — wait for green (`gh pr checks <num> --repo AVSystem/scala-commons --watch`).
    5. PR stays open for manual maintainer review — never merge automatically (global rule).
  </how-to-verify>
  <resume-signal>
    Type "approved PR" or describe concerns. On approval, Claude runs `gh pr create` + `gh api PATCH` for milestone, then `gh pr checks --watch` until PR CI green. On any failure (PR creation rejected, milestone assignment failed, CI red), STOP and report.

    Phase 1 is complete when AVSystem PR CI is green AND PR is open in draft with milestone 1.
  </resume-signal>
  <files>(no source files; gh CLI operations only)</files>
  <action>
    Wait for explicit user ack (resume-signal). On approval:
    1. Author PR body at /tmp/pr-body.md from CONTEXT/RESEARCH/SUMMARYs — concise (5 bullets), pointer to MIGRATION.md, no GSD nomenclature, no .planning/ references. Print to stdout for user to see before `gh pr create`.
    2. `gh pr create --repo AVSystem/scala-commons --base scala-3 --head halotukozak:01-big-bang --draft --title "[Scala 3] Pivot to Scala 3 only — comment broken, green CI" --body-file /tmp/pr-body.md`.
    3. Capture PR number: `PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:01-big-bang --json number -q '.[0].number')`.
    4. Assign milestone 1: `gh api -X PATCH "/repos/AVSystem/scala-commons/issues/${PR_NUM}" -f milestone=1` (memory rule — gh pr edit may fail on Projects-classic GraphQL).
    5. Wait for PR CI: `gh pr checks ${PR_NUM} --repo AVSystem/scala-commons --watch` until exit 0.
    6. Do NOT merge the PR — leave open for manual maintainer review (global rule).
    7. On any failure (PR creation rejected, milestone assignment failed, CI red), STOP and report.
  </action>
  <verify>
    <automated>PR_NUM=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:01-big-bang --json number,isDraft,title,milestone -q '.[0] | select(.isDraft==true and (.title|startswith("[Scala 3]")) and (.milestone.number==1)) | .number'); test -n "$PR_NUM" && gh pr checks "$PR_NUM" --repo AVSystem/scala-commons --json state -q '.[].state' | grep -qE '(SUCCESS|PASS)' && ! gh pr checks "$PR_NUM" --repo AVSystem/scala-commons --json state -q '.[].state' | grep -qE '(FAILURE|FAIL|ERROR)'</automated>
  </verify>
  <done>
    User approved. Draft PR open at AVSystem/scala-commons base=scala-3 head=halotukozak:01-big-bang. Title starts with `[Scala 3]`. Milestone = Scala 3 (#1). State = draft. AVSystem PR CI green. PR left open for manual maintainer merge. Phase 1 closed.
  </done>
</task>

</tasks>

<verification>
- `MIGRATION.md` exists, ≥5 `## ` headings, all 5 locked sections by exact title, Backlog populated.
- `sbt -batch 'show version'` exit 0.
- `sbt -batch compile` exit 0.
- `sbt -batch Test/compile` exit 0.
- `sbt -batch scalafmtCheckAll scalafmtSbtCheck` exit 0.
- `! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'`
- `! git log upstream/scala-3..HEAD --pretty=%B | grep -qiE '(GSD|get shit done|phase[ -][0-9])'`
- `! git diff --name-only upstream/scala-3..HEAD | grep -q '^\.planning/'`
- Fork CI green on `halotukozak:01-big-bang`.
- AVSystem PR open: title `[Scala 3] ...`, base `scala-3`, head `halotukozak:01-big-bang`, draft, milestone 1.
- AVSystem PR CI green.
</verification>

<success_criteria>
1. MIGRATION.md at root, 5 locked sections + Backlog populated from TODO grep (DOC-01, DOC-02).
2. All compile gates green (COMPILE-01, COMPILE-02, COMPILE-03).
3. All hygiene grep audits pass (QUALITY-01, WORKFLOW-04, WORKFLOW-05).
4. Branch pushed to fork; fork CI green (CI-01).
5. Draft PR open at AVSystem/scala-commons base=scala-3 head=halotukozak:01-big-bang (WORKFLOW-02).
6. PR title prefixed `[Scala 3]`, milestone 1, draft state (PR-01, PR-02, PR-03).
7. AVSystem PR CI green (CI-02).
8. User ack obtained before push AND before PR open (WORKFLOW-03).
</success_criteria>

<output>
After completion, create `.planning/phases/01-big-bang-comment-and-green/01-06-SUMMARY.md`:
- Backlog row count vs TODO tag count (sync check)
- Final list of dropped/commented modules (will-not-migrate + disabled sections)
- Push commit hash + fork CI run URL
- PR number + URL + CI run URL
- Confirmation: DOC-01..02, COMPILE-01..03, CI-01..02, WORKFLOW-01..05, PR-01..03, QUALITY-01..02 ALL satisfied
- Phase 1 closure: roadmap entry can be marked complete pending maintainer merge (which is out of scope per global rule "never merge PRs automatically").
</output>
