---
phase: 05-core-scala-3-baseline-port
plan: 03
type: execute
wave: 3
depends_on: ["05-02"]
files_modified:
  - MIGRATION.md
autonomous: true
commit_docs: false
requirements: [DOC-02, CORE-02, QUALITY-01]

must_haves:
  truths:
    - "Full 5-gate sanity pass (2.13 core compile, 3.8.2 core compile, 3.8.2 macros compile, scalafmtCheckAll, no-new-@nowarn grep) all GREEN"
    - "MIGRATION.md `core` row Status flipped from `pending` to `cross`"
    - "MIGRATION.md `core` row Notes column appended with: `Scala 3 compile-only baseline; tests + given/using sweep pending`"
    - "No source-file changes in this commit (MIGRATION.md only)"
  artifacts:
    - path: "MIGRATION.md"
      provides: "Updated per-module status table reflecting core cross-compile achievement"
      contains: "Scala 3 compile-only baseline"
  key_links:
    - from: "MIGRATION.md core row"
      to: "Phase 5 cherry-pick result"
      via: "status column"
      pattern: "core \\|.*\\| cross"
---

<objective>
Re-run the full local 5-gate sanity matrix to prove Phase 5's work is stable across all dimensions, then update MIGRATION.md's `core` row to reflect the achievement. Single doc-only commit.

This plan exists separately from 05-02 so that (a) source-port commits stay clean and (b) the MIGRATION.md flip is reviewable as a single doc change in the PR.

Output: 5 sanity gates green; one MIGRATION.md commit on branch; no source changes.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/05-core-scala-3-baseline-port/05-CONTEXT.md
@.planning/phases/05-core-scala-3-baseline-port/05-02-cherry-pick-scala-3-sources-SUMMARY.md
@MIGRATION.md

<current_core_row>
Current state of the `core` row in MIGRATION.md (from Phase 4):

```
| core | cross | pending | green | pending | Cross-compile target; tests still pending on Scala 3. made wiring primitives ported; full derivation pending. |
```

Target state after Phase 5:

```
| core | cross | cross | green | pending | Scala 3 compile-only baseline; tests + given/using sweep pending. |
```

Diff:
- 3.x column: `pending` → `cross`
- Notes column: full rewrite — drop "made wiring primitives ported; full derivation pending"; replace with "Scala 3 compile-only baseline; tests + given/using sweep pending".
</current_core_row>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Run 5-gate sanity matrix</name>
  <files>(no edits — verification only)</files>
  <action>
Run each gate. ALL five MUST exit 0. If any RED, STOP and report; do not proceed to Task 2.

```sh
# Gate 1: Scala 2.13 commons-core regression
sbt '++2.13.18 commons-core/compile'

# Gate 2: Scala 3 commons-core — THE Phase 5 goal
sbt '++3.8.2 commons-core/compile'

# Gate 3: Scala 3 commons-macros (Phase 3 stub protection)
sbt '++3.8.2 commons-macros/compile'

# Gate 4: scalafmt
sbt scalafmtCheckAll

# Gate 5: No-introductions grep — memory rule QUALITY-01
git diff 04-made-integration..HEAD | grep -E '(@nowarn|-Wconf|^\+.*\?\?\?)' &amp;&amp; { echo "VIOLATION: nowarn/Wconf/??? introduced"; exit 1; } || echo "Gate 5 GREEN"
```

Record exit codes in execution log. Working tree MUST be clean before running these.
  </action>
  <verify>
    <automated>sbt '++2.13.18 commons-core/compile' '++3.8.2 commons-core/compile' '++3.8.2 commons-macros/compile' scalafmtCheckAll &gt;/dev/null 2>&amp;1 &amp;&amp; ! git diff 04-made-integration..HEAD | grep -qE '(@nowarn|-Wconf)'</automated>
  </verify>
  <done>
- All 5 gates GREEN.
- Working tree clean (no incidental changes during compile/format).
  </done>
</task>

<task type="auto">
  <name>Task 2: Flip MIGRATION.md core row and commit</name>
  <files>MIGRATION.md</files>
  <action>
1. Read `MIGRATION.md`. Locate the `| core |` row in the per-module status table.
2. Replace it verbatim:
   - FROM: `| core | cross | pending | green | pending | Cross-compile target; tests still pending on Scala 3. made wiring primitives ported; full derivation pending. |`
   - TO:   `| core | cross | cross | green | pending | Scala 3 compile-only baseline; tests + given/using sweep pending. |`
3. No other edits to MIGRATION.md.
4. Diff sanity check: `git diff MIGRATION.md` should show exactly 1 deletion + 1 addition on the core row.
5. Commit:
   ```sh
   git add MIGRATION.md
   git commit -m "docs(migration): flip core row to cross — Scala 3 compile-only baseline landed" \
     -m "core/src/main/scala-3/ now contains the Scala 3 baseline (annotations, collection, concurrent," \
     -m "jiop, derivation, meta, misc sans compat, serialization sans cbor, tuples). ++3.8.2 commons-core/compile" \
     -m "exits 0. Tests and given/using sweep remain pending (later phases)."
   ```
6. Confirm `.planning/` not in commit: `git diff HEAD~1 HEAD --name-only | grep -q '^\.planning/' &amp;&amp; exit 1 || true`.

NO GSD nomenclature. NO `.planning/` paths.
  </action>
  <verify>
    <automated>git log --oneline -1 | grep -q 'docs(migration): flip core row' &amp;&amp; grep -qE '^\| core \| cross \| cross \| green \| pending \| Scala 3 compile-only baseline' MIGRATION.md &amp;&amp; ! git diff HEAD~1 HEAD --name-only | grep -q '^\.planning/'</automated>
  </verify>
  <done>
- MIGRATION.md `core` row reflects Scala 3 = `cross` and updated Notes.
- New commit on branch with `docs(migration): flip core row to cross` subject.
- `.planning/` not in diff.
- No source files changed in this commit.
  </done>
</task>

</tasks>

<verification>
- All 5 sanity gates still GREEN after the MIGRATION.md commit (re-run if any doubt).
- `git log --oneline 04-made-integration..HEAD` shows the full Phase 5 commit train, ending in `docs(migration): flip core row to cross — Scala 3 compile-only baseline landed`.
- `git diff HEAD~1 HEAD --stat` shows `MIGRATION.md | 2 +-` (or similar) — exactly one file changed in the last commit.
</verification>

<success_criteria>
- 5-gate matrix GREEN.
- MIGRATION.md core row updated.
- One doc-only commit on branch.
- Branch ready for Plan 05-04 (push + PR).
</success_criteria>

<output>
After completion, create `.planning/phases/05-core-scala-3-baseline-port/05-03-sanity-gate-and-migration-flip-SUMMARY.md`. Record:
- All 5 gate results.
- MIGRATION.md commit SHA.
- Branch tip SHA.
- Full Phase 5 commit list (`git log --oneline 04-made-integration..HEAD`).
</output>
