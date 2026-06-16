---
phase: 04-made-integration
plan: 03
type: execute
wave: 3
depends_on: ["04-01", "04-02"]
files_modified:
  - MIGRATION.md
autonomous: true
requirements:
  - MADE-01
  - DOC-02
  - QUALITY-01
  - WORKFLOW-05
must_haves:
  truths:
    - "`sbt '++3 core/compile'` is green"
    - "`sbt '++3 core-js/compile'` is green"
    - "`sbt '++2.13 core/compile'` is green (no regression on the 2.13 side)"
    - "`sbt scalafmtCheckAll` is green"
    - "Full 5-gate CI suite (`'+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll`) is green"
    - "No new `@nowarn` or `-Wconf` introduced in the ported files (QUALITY-01)"
    - "MIGRATION.md has a new `made` row and the `core` row Notes column appended"
    - "`.planning/` is NOT staged in any commit on the branch (WORKFLOW-05)"
  artifacts:
    - path: "MIGRATION.md"
      provides: "Per-module status table updated with `made` row + `core` Notes append"
      contains: "| made |"
  key_links:
    - from: "MIGRATION.md per-module status table"
      to: "Phase 4 PR's source diff"
      via: "DOC-02 same-PR update rule"
      pattern: "made.*0\\.1\\.0|wiring primitives ported"
---

<objective>
Run the full sanity gate (compile permutations + scalafmt + QUALITY-01 grep + 5-gate suite + WORKFLOW-05 `.planning/` hygiene) on the ported branch, then flip MIGRATION.md in the same PR per DOC-02 — add a `made` row and append `core` row Notes column. All edits committed.

Purpose: Single consolidated validation + docs-flip plan. The sanity gate IS Phase 4's primary acceptance proof (no tests added — VALIDATION.md "compile-only"). MIGRATION.md flip belongs in the same PR per DOC-02 contract.

Output: Branch `04-made-integration` is fully green against the 5-gate suite, MIGRATION.md reflects the new state, one new commit (`docs(migration):` prefix), ready for human-ack push (Plan 04).
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/REQUIREMENTS.md
@.planning/phases/04-made-integration/04-CONTEXT.md
@.planning/phases/04-made-integration/04-RESEARCH.md
@.planning/phases/04-made-integration/04-VALIDATION.md
@.planning/phases/04-made-integration/04-01-SUMMARY.md
@.planning/phases/04-made-integration/04-02-SUMMARY.md
@MIGRATION.md

<interfaces>
<!-- Expected MIGRATION.md state at start of this plan (per Phase 2 ROADMAP): -->
<!-- A repo-root MIGRATION.md exists with a per-module status table that includes a `core` row -->
<!-- and a "## Deprecation log" section. Phase 2 should have established this. -->
<!-- This plan ADDS one row (`made`) and APPENDS the `core` row Notes column. -->
<!-- Exact table format and columns: read MIGRATION.md at top of Task 1 — DO NOT assume schema. -->
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Full sanity gate — 5 sbt commands + QUALITY-01 grep + WORKFLOW-05 hygiene</name>
  <files>(no files modified — read-only validation)</files>
  <action>
**Step 1 — Confirm branch state:**
```sh
git rev-parse --abbrev-ref HEAD          # → 04-made-integration
git status --porcelain                    # → empty (Plans 01 & 02 committed cleanly)
git log upstream/scala-3..HEAD --oneline  # → at least 3 commits (1 build bump + 2 ports)
```

**Step 2 — Scalafmt gate:**
```sh
sbt -batch scalafmtCheckAll
```
Expected: exit 0. If it fails, run `sbt -batch scalafmtAll` and inspect the diff. If the diff only touches the 5 ported files and the resulting `scalafmtCheckAll` is green, amend Plan 02's port commit (NOT the build bump commit) by adding a separate `style(scalafmt):` commit on top — do NOT use `git commit --amend` (per CLAUDE.md `Git Safety Protocol`).

If `scalafmtCheckAll` reports diffs in files OUTSIDE the Phase 4 scope (i.e., files this branch did not touch), STOP and surface to user — upstream/scala-3 baseline drift, not Phase 4's problem.

**Step 3 — Compile permutations (the 3 hard gates from VALIDATION.md):**
```sh
sbt -batch '++3 core/compile'
sbt -batch '++3 core-js/compile'
sbt -batch '++2.13 core/compile'
```
All three MUST exit 0.

Pitfalls (from RESEARCH.md):
- "Pitfall 1: `compat.scala` GenCodec leakage" — failure mode: `not found: type GenCodec` / `object serialization is not a member of …`. Fix: re-check Plan 02 Task 2 — `compat.scala` must NOT exist on this branch.
- "Pitfall 3: `made.Default` SAM" — failure mode: `expression of type () => AnyRef does not conform to type Default[AnyRef]`. Fix: confirm `madeVersion = "0.1.0"` is in `build.sbt` (Plan 01).
- "Pitfall 4: cross-build noise on 2.13" — failure mode: 2.13 compile fails after Scala-3-only edits. Should not happen here (Phase 1 routed `scala-3/` files only to Scala 3 builds), but if it does, surface to user.

**Step 4 — QUALITY-01 grep (no new `@nowarn` / `-Wconf` in the 5 ported files):**
```sh
! grep -RnE '@nowarn|-Wconf' \
  core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala \
  core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
```
Must produce no output.

**Step 5 — Full 5-gate CI suite (per REQ INFRA-07 / Phase 1 contract):**
```sh
sbt -batch '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll
```
This is the same suite CI will run. MUST exit 0. Note: Phase 4 ships no new tests, so `+jvm/test` etc. compile-and-run whatever already exists on `upstream/scala-3` plus the Scala 3 wiring.

**Step 6 — WORKFLOW-05 hygiene gate — `.planning/` MUST NOT be staged in any commit on the branch:**
```sh
git log upstream/scala-3..HEAD --name-only --pretty=format: | sort -u | grep -E '^\.planning/' && { echo "BLOCKER: .planning/ in commit"; exit 1; } || echo OK
```
If the grep finds anything (i.e., the `&&` branch fires), surface the violating commit to the user — `.planning/` is gitignored project state, never committed (REQUIREMENTS.md WORKFLOW-05). If `.planning/` is not gitignored, fix by:
```sh
echo ".planning/" >> .gitignore  # only if not already there
```
…then surface to user — this is unexpected at Phase 4 (Phase 1 should have set it).

**Step 7 — Diff scope check (manual-only per VALIDATION.md, but record the size for the SUMMARY):**
```sh
git diff upstream/scala-3 --stat
```
Expected: ~6 files changed (1 build.sbt + 5 source files). If many more files, something has gone wrong — surface to user.
  </action>
  <verify>
    <automated>sbt -batch '++3 core/compile' && sbt -batch '++3 core-js/compile' && sbt -batch '++2.13 core/compile' && sbt -batch scalafmtCheckAll && ! grep -RqE '@nowarn|-Wconf' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala && ! git log upstream/scala-3..HEAD --name-only --pretty=format: | grep -qE '^\.planning/'</automated>
  </verify>
  <done>All three compile permutations green, scalafmt green, no `@nowarn`/`-Wconf` in ported files, `.planning/` not in any commit, 5-gate suite green. Diff scope recorded for SUMMARY.</done>
</task>

<task type="auto">
  <name>Task 2: Flip MIGRATION.md — add `made` row + append `core` row Notes</name>
  <files>MIGRATION.md</files>
  <action>
**Step 1 — Read current MIGRATION.md** to understand the exact table schema (column count, headers, separators). Phase 2 authored this file; this plan must respect whatever schema landed:

```sh
cat MIGRATION.md
```

Per Phase 2 ROADMAP success criterion 1: "per-module status table" with rows for `macros, made, core, hocon, mongo, mongo-js, core-js, benchmark3, jetty, analyzer, spring, RPC, cbor`. (Phase 2 should already include a `made` row — BUT per CONTEXT.md decision: "Add new row above `core` row: `made | n/a | cross | n/a | n/a | external dep at 0.1.0, Scala 3 only`.")

**Conditional logic:**

- **If `made` row ALREADY EXISTS in MIGRATION.md** (Phase 2 anticipated it): UPDATE the `made` row's status/notes columns to reflect "external dep at 0.1.0, Scala 3 only".
- **If `made` row DOES NOT EXIST**: INSERT it above the `core` row using the same column schema as the surrounding rows.

**Per the CONTEXT.md decisions section:**
- `made` row content: `external dep at 0.1.0, Scala 3 only` (status: `n/a`/`cross`/whatever matches the schema's "external dependency" semantics; `core` Scala 3 only).
- `core` row Notes column APPENDED with: `made wiring primitives ported; full derivation pending`.
- `core` row Status column: KEEP as `wip` (full port still incomplete — derivation pending Phase 5).

**Step 2 — Make the edits using the Edit tool on MIGRATION.md.**

Concrete edit pattern (adjust to actual schema discovered in Step 1):

If the existing schema is `| module | 2.13 | 3 | tests | mima | Notes |`:
- `made` row: `| made | n/a | external | n/a | n/a | dep pinned at 0.1.0; Scala 3 only |`
- `core` row Notes: append `; made wiring primitives ported (Default givens for Opt/NOpt/OptArg/OptRef + annotation re-exports); full derivation pending`

Be conservative — match the surrounding rows' tone and length exactly.

**Step 3 — Verify the edits:**
```sh
grep -nE '^\| made \|' MIGRATION.md          # → exactly one match
grep -n 'made wiring primitives ported' MIGRATION.md  # → one match in the core row
```

**Step 4 — Re-run scalafmtCheckAll** (MIGRATION.md is not Scala, but the gate should still be green from Task 1 — confirm nothing else moved):
```sh
sbt -batch scalafmtCheckAll
```

**Step 5 — Commit:**
```sh
git add MIGRATION.md
git commit -m "docs(migration): record made integration at 0.1.0; core wiring primitives ported"
```

No GSD nomenclature in the commit message. `.planning/` MUST NOT be in the staged set (re-verify):
```sh
git diff --cached --name-only | grep -E '^\.planning/' && { echo BLOCKER; exit 1; } || echo OK
```
  </action>
  <verify>
    <automated>grep -qE '^\| made \|' MIGRATION.md && grep -q 'made wiring primitives ported' MIGRATION.md && ! git diff --cached --name-only | grep -qE '^\.planning/' && git log -1 --pretty=%s | grep -q 'docs(migration)'</automated>
  </verify>
  <done>MIGRATION.md has a single `made` row plus the `core` row Notes append; commit landed with `docs(migration):` prefix; `.planning/` not staged.</done>
</task>

</tasks>

<verification>
End-of-plan gate (must be green before Plan 04 push gate):

```sh
# All sbt gates green
sbt -batch '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll

# Repo hygiene
! git log upstream/scala-3..HEAD --name-only --pretty=format: | grep -qE '^\.planning/'
! grep -RqE '\-SNAPSHOT' build.sbt project/

# Doc state
grep -qE '^\| made \|' MIGRATION.md
grep -q 'made wiring primitives ported' MIGRATION.md

# Commit hygiene — count and prefixes
git log upstream/scala-3..HEAD --oneline   # → 4 commits: build(made), feat(core/scala-3) x2, docs(migration)
git log upstream/scala-3..HEAD --pretty=%s | grep -iE 'gsd|claude' && { echo BLOCKER; exit 1; } || echo OK  # WORKFLOW-04
```
</verification>

<success_criteria>
- [ ] `sbt -batch '++3 core/compile'`, `'++3 core-js/compile'`, `'++2.13 core/compile'` all exit 0
- [ ] `sbt -batch scalafmtCheckAll` exits 0
- [ ] 5-gate suite `'+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll` exits 0
- [ ] No `@nowarn` / `-Wconf` in the 5 ported files (QUALITY-01)
- [ ] `.planning/` not in any commit on the branch (WORKFLOW-05)
- [ ] No `-SNAPSHOT` anywhere in `build.sbt` or `project/`
- [ ] MIGRATION.md `made` row exists; `core` row Notes appended with "made wiring primitives ported"
- [ ] Commit prefixes are conventional (`build:`, `feat:`, `docs:`); zero GSD nomenclature
</success_criteria>

<output>
After completion, create `.planning/phases/04-made-integration/04-03-SUMMARY.md` documenting:
- All sbt gate exit codes (must be 0)
- `git log upstream/scala-3..HEAD --oneline` output (the full PR commit list)
- `git diff upstream/scala-3 --stat` output (the diff scope — should be ~6 files)
- Confirmation `.planning/` not in any commit
- Confirmation no GSD nomenclature in any commit message
- Final MIGRATION.md `made` row text and `core` row Notes append text
</output>
</content>
