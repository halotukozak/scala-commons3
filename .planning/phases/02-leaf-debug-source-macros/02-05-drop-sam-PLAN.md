---
phase: 02-leaf-debug-source-macros
plan: 05
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/Sam.scala
  - core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala
  - MIGRATION.md
autonomous: false
requirements: [SAM-01]

must_haves:
  truths:
    - "`core/src/main/scala/com/avsystem/commons/misc/Sam.scala` no longer exists"
    - "`core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala` no longer exists"
    - "No source file in `core/` references `Sam` or `SamCompanion` symbols (other than incidental string matches in comments / unrelated identifiers)"
    - "MIGRATION.md §1 (Will Not Migrate) lists `Sam` / `SamCompanion` with rationale"
    - "MIGRATION.md §6 Backlog no longer lists Sam.scala:9, SamCompanion.scala:11, SamCompanion.scala:19"
    - "`sbt commons-core/compile` exits 0 after deletion"
  artifacts:
    - path: "MIGRATION.md"
      provides: "§1 entry documenting Sam/SamCompanion as will-not-migrate (deprecated upstream; native SAM conversion replaces)"
      contains: "Sam / SamCompanion"
  key_links: []
---

<objective>
Drop the deprecated `Sam` + `SamCompanion` slice instead of porting. Both are `@deprecated since 2.28.0` with stdlib native SAM conversion as replacement, per memory rule `feedback_dont_port_deprecated.md`.

Purpose: Honors the project rule "don't port deprecated APIs with stdlib replacements". Reduces Phase 2 surface; reviewer cognitive load minimal (deletion + doc only — no Scala 3 impl to review).
Output: Two files deleted + MIGRATION.md §1 entry + §6 backlog rows removed + draft PR.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/phases/02-leaf-debug-source-macros/02-CONTEXT.md
@.planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Pitfall #6: Sam / SamCompanion are deprecated")
@.planning/phases/02-leaf-debug-source-macros/02-VALIDATION.md
@MIGRATION.md
</context>

<tasks>

<task type="auto">
  <name>Task 1: Delete Sam.scala + SamCompanion.scala + grep for references</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/Sam.scala, core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/misc/Sam.scala (current file — confirm only deprecated symbols)
    - core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala (current file — confirm only deprecated symbols)
    - Output of `grep -rnE '\b(Sam|SamCompanion)\b' core/src/ 2>/dev/null` to see if anything else uses them
    - MIGRATION.md (§1 Will Not Migrate table; §6 Backlog rows for Sam.scala:9, SamCompanion.scala:11, SamCompanion.scala:19)
  </read_first>
  <action>
    Step A — Run reference check:
    ```bash
    grep -rnE '\b(Sam|SamCompanion)\b' core/src/ 2>/dev/null
    ```
    If any non-self reference exists (i.e. another `core/` file imports or uses these symbols), STOP and report — slice cannot ship as a clean delete. (Expected: zero non-self references; both files were already stubbed in Phase 1 and nothing depends on them.)

    Step B — Delete both files:
    ```bash
    git rm core/src/main/scala/com/avsystem/commons/misc/Sam.scala
    git rm core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala
    ```

    Step C — Verify compile:
    ```bash
    sbt -batch 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll'
    ```

    Step D — Check if any test file references `Sam` / `SamCompanion` (commented out from Phase 1):
    ```bash
    grep -rnE '\b(Sam|SamCompanion)\b' core/src/test/ mongo/src/ hocon/src/ 2>/dev/null
    ```
    If a test file references them (likely a commented-out test that was already disabled in Phase 1), include that file's removal or stub-cleanup in this commit.

    Commit message: `chore(core): drop deprecated Sam and SamCompanion`
  </action>
  <verify>
    <automated>! test -f core/src/main/scala/com/avsystem/commons/misc/Sam.scala && ! test -f core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala && sbt -batch 'commons-core/compile' && ! grep -rlE '\b(Sam|SamCompanion)\b' core/src/ mongo/ hocon/ 2>/dev/null | grep -vE 'Sam\.scala|SamCompanion\.scala|MIGRATION\.md'</automated>
  </verify>
  <done>Both files removed from working tree and from git index; commons-core/compile exit 0; no broken references in core/test/mongo/hocon sources.</done>
</task>

<task type="auto">
  <name>Task 2: Update MIGRATION.md (§1 add entry, §6 remove rows)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (§1 Will Not Migrate table; §6 Backlog rows for Sam.scala:9, SamCompanion.scala:11, SamCompanion.scala:19; "Total tags: N" header)
  </read_first>
  <action>
    Step A — Add a new row to §1 (Will Not Migrate) table:

    | Symbol/module | Rationale |
    |---------------|-----------|
    | `misc.Sam` / `misc.SamCompanion` | `@deprecated since 2.28.0` with stdlib native SAM conversion as replacement. Project rule `feedback_dont_port_deprecated.md` says skip @deprecated APIs that have stdlib replacements. Deleted outright in Phase 2. |

    Step B — Delete the 3 §6 Backlog rows for:
    - `core/src/main/scala/com/avsystem/commons/misc/Sam.scala:9`
    - `core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala:11`
    - `core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala:19`

    Step C — Update the "Total tags: N" line to match `git grep -c 'TODO\[scala3-port\]' -- '*.scala'`.

    Commit message: `docs(migration): record Sam/SamCompanion as will-not-migrate`
  </action>
  <verify>
    <automated>grep -q 'Sam' MIGRATION.md && ! grep -nE 'Sam\.scala:9|SamCompanion\.scala:(11|19)' MIGRATION.md</automated>
  </verify>
  <done>§1 has the new row; §6 backlog rows removed; Total tags updated.</done>
</task>

<task type="checkpoint:human-action" gate="blocking">
  <name>Task 3: Push branch + open draft PR</name>
  <what-built>drop-sam slice: 2 source files deleted + MIGRATION.md §1 entry + §6 trim. 2 commits.</what-built>
  <action>
    ```bash
    git checkout -b 02-drop-sam 01-big-bang
    # ensure Task 1 + Task 2 commits are on this branch
    sbt -batch 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll ;scalafmtSbtCheck'
    git push -u origin 02-drop-sam

    PR_URL=$(gh pr create --draft \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:02-drop-sam \
      --title "[Scala 3] Phase 02 drop-sam: remove deprecated Sam and SamCompanion" \
      --body "Drops deprecated Sam / SamCompanion (deprecated since 2.28.0; stdlib SAM conversion replaces). Per project rule feedback_dont_port_deprecated.md: skip @deprecated APIs that have stdlib replacements. MIGRATION.md §1 (Will Not Migrate) updated; §6 backlog trims 3 rows.")
    echo "PR: $PR_URL"
    PR_NUM=$(echo "$PR_URL" | grep -oE '[0-9]+$')
    gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
    ```
  </action>
  <resume-signal>Type "approved" once PR draft is open + milestone assigned, or describe blockers.</resume-signal>
</task>

</tasks>

<verification>
- `sbt -batch 'commons-core/compile ;commons-core/Test/compile'` exit 0.
- `sbt -batch 'scalafmtCheckAll'` exit 0.
- `grep -rnE '\b(Sam|SamCompanion)\b' core/src/main/` returns zero relevant hits.
- `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` decreased by 3.
- PR draft, milestone 1, title prefix `[Scala 3]`.
</verification>

<success_criteria>
- Sam.scala + SamCompanion.scala deleted.
- MIGRATION.md §1 documents the rationale.
- MIGRATION.md §6 backlog reflects 3-row removal.
- Draft PR open with correct title prefix + milestone.
- No new `@nowarn` / `-Wconf` / GSD nomenclature / `.planning/` paths.
</success_criteria>

<output>
After completion, create `.planning/phases/02-leaf-debug-source-macros/02-05-SUMMARY.md` with: PR URL, 2 commit hashes (chore + docs), removed MIGRATION row count (3 + §1 added entry), post-slice TODO grep count, any deviations.
</output>
