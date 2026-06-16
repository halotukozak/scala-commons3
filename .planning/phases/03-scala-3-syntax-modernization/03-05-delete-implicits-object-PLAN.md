---
phase: 03-scala-3-syntax-modernization
plan: 05
type: execute
# NOTE on wave/depends_on: PRs in Phase 3 are NOT stacked — each branches off
# `upstream/scala-3` tip. Slice 3.5 is the standalone parallel-safe slice; it
# touches files no other slice touches and can be opened + merged in parallel
# with 3.1/3.2/3.3/3.4. Frontmatter expresses no git-topology dependencies.
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/Implicits.scala
  - core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala
  - MIGRATION.md
autonomous: true
requirements: [SYNTAX-33-IMPLICIT-TO-GIVEN, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

must_haves:
  truths:
    - "core/src/main/scala/com/avsystem/commons/misc/Implicits.scala deleted (0 git ls-files hits)"
    - "core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala created (sealed trait + companion)"
    - "git grep -nE '\\bImplicits\\.' -- '*.scala' returns 0 hits (no remaining callers)"
    - "sbt compile + Test/compile + scalafmtCheckAll all green"
    - "MIGRATION.md §1 (removed APIs) references Implicits removal pointing to scala.compiletime.summon"
    - "PR open off upstream/scala-3 tip, draft, [Scala 3] prefix, milestone 1"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala"
      provides: "sealed trait ImplicitNotFound + companion (extracted from former Implicits.scala)"
    - path: "MIGRATION.md"
      provides: "§1 entry — com.avsystem.commons.misc.Implicits object removed (use scala.compiletime.summon)"
  key_links:
    - from: "ImplicitNotFound.scala"
      to: "ImplicitNotFound users (custom error message machinery)"
      via: "import com.avsystem.commons.misc.ImplicitNotFound"
      pattern: "ImplicitNotFound"
---

<objective>
Slice 3.5 of Phase 3: delete the `com.avsystem.commons.misc.Implicits` object outright (covered by Scala 3 `summon[T]`) and extract the still-useful `ImplicitNotFound` sealed trait + companion into its own file.

Purpose: User directive 2026-06-01 — `Implicits.infer/inferNonMacro` were Scala 2 macro helpers; Scala 3 `summon[T]` + `@implicitNotFound` on `using` params replace the use case. 0 callers in tree. Standalone parallel-safe slice (no overlap with 3.1/3.2/3.3/3.4 file sets) — can land any time independently.

Output:
- `Implicits.scala` deleted entirely
- `ImplicitNotFound.scala` new file (sealed trait + companion)
- MIGRATION.md §1 (removed APIs) entry
- 2-3 Conventional Commits
- Draft PR `[Scala 3] delete Implicits object (covered by summon[T])` off `upstream/scala-3`, milestone 1
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/MIGRATION.md
@.planning/phases/03-scala-3-syntax-modernization/03-CONTEXT.md
@.planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md
@~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md

<interfaces>
Current state of `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala`:
- Contains `object Implicits { def infer[T]: T = ???; def inferNonMacro[T]: T = ??? }` (Scala 2 macro helpers, Phase-1 stubbed).
- Contains `sealed trait ImplicitNotFound[T]` + companion (custom @implicitNotFound error-message machinery, still useful).

Two distinct concerns living in one file — slice 3.5 separates them:
- `object Implicits` → DELETE (covered by `scala.compiletime.summon`).
- `sealed trait ImplicitNotFound` + companion → EXTRACT to `ImplicitNotFound.scala` (keep verbatim).

Callers check:
```bash
git grep -nE '\bImplicits\.' -- '*.scala'           # expect 0 hits (object users)
git grep -nE '\bImplicitNotFound\b' -- '*.scala'    # may have hits (kept artifact)
```

Per CONTEXT.md `### Implicits object — delete, do NOT deprecate` user directive 2026-06-01: outright delete, do NOT deprecate, do NOT restore (fork commit `50272b26` is intentionally NOT followed here).
</interfaces>

**PR conventions (non-negotiable, from MEMORY.md):**
- Branch base: `upstream/scala-3` tip — NOT stacked.
- PR title: `[Scala 3] delete Implicits object (covered by summon[T])`
- PR body metadata block:
  ```
  **Slice:** 3.5 of Phase 3 (Scala 3 syntax modernization)
  **Merge order:** Independent — can land any time (no overlap with 3.1/3.2/3.3/3.4)
  **Depends on:** none
  **Base branch:** upstream/scala-3
  ```
- `--draft` on open. Milestone 1. No GSD nomenclature. No `.planning/` in commits. No new `@nowarn`/`-Wconf`.

**Commit cadence:**
- `refactor(scala-3,core): extract ImplicitNotFound to its own file`
- `refactor(scala-3,core): delete Implicits object (covered by summon[T])`
- `docs(migration): record Implicits object removal`

</context>

<tasks>

<task type="auto">
  <name>Task 1: Branch + extract ImplicitNotFound to its own file</name>
  <files>
    core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala
    core/src/main/scala/com/avsystem/commons/misc/Implicits.scala
  </files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/misc/Implicits.scala (locate `sealed trait ImplicitNotFound` + companion; locate `object Implicits`)
    - .planning/phases/03-scala-3-syntax-modernization/03-CONTEXT.md `### Implicits object — delete, do NOT deprecate`
    - `git grep -nE '\bImplicitNotFound\b' -- '*.scala'` — caller inventory
    - `git grep -nE '\bImplicits\.' -- '*.scala'` — verify 0 caller hits before delete
  </read_first>
  <action>
    1. Cut new branch: `git fetch upstream && git checkout -b 03-05-delete-implicits-object upstream/scala-3`.
    2. Verify 0 callers of `object Implicits`:
       ```bash
       git grep -nE '\bImplicits\.' -- '*.scala'
       # Expected: 0 hits
       ```
       If hits exist, STOP and report — directive premise (0 callers) is violated.
    3. Open `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala`. Identify exact lines for:
       - `object Implicits { … }` block
       - `sealed trait ImplicitNotFound[T]` + companion (`object ImplicitNotFound { … }`)
       - Package declaration + imports needed by ImplicitNotFound
    4. Create `core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala`:
       - Same package: `package com.avsystem.commons.misc`
       - Carry only the imports used by `ImplicitNotFound` trait+companion (drop any used only by `object Implicits`).
       - Paste `sealed trait ImplicitNotFound[T]` + companion verbatim.
    5. Run `sbt commons-jvm/compile ;commons-js/compile` — must be exit 0 (file extracted, original still present, so duplicate symbol error possible: defer to Task 2).
       - **Workaround for this task only:** Remove the `sealed trait ImplicitNotFound` + companion FROM `Implicits.scala` in the same task so compile is clean.
    6. Re-read `Implicits.scala` — should now contain ONLY `object Implicits { … }` plus package/imports.
    7. `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    8. Commit: `git add core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && git commit -m "refactor(scala-3,core): extract ImplicitNotFound to its own file"` body: `Separates the still-useful ImplicitNotFound sealed trait + companion from the to-be-deleted Implicits object. Pure extraction — semantics unchanged.`
  </action>
  <verify>
    <automated>test -f core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala && sbt 'commons-jvm/compile ;commons-js/compile'</automated>
  </verify>
  <acceptance_criteria>
    - `core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala` exists with `sealed trait ImplicitNotFound[T]` + companion.
    - `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala` still exists but no longer contains `ImplicitNotFound` (only `object Implicits`).
    - `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - `git grep -nE '\bImplicitNotFound\b' -- 'core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala'` ≥ 1 (defined).
    - `git grep -nE '\bImplicitNotFound\b' -- 'core/src/main/scala/com/avsystem/commons/misc/Implicits.scala'` = 0 (removed from old location).
    - Single commit: `refactor(scala-3,core): extract ImplicitNotFound to its own file`.
  </acceptance_criteria>
  <done>
    ImplicitNotFound moved to its own file; original Implicits.scala now contains only the to-be-deleted `object Implicits`; compile + tests + scalafmt green.
  </done>
</task>

<task type="auto">
  <name>Task 2: Delete Implicits.scala entirely</name>
  <files>
    core/src/main/scala/com/avsystem/commons/misc/Implicits.scala
  </files>
  <read_first>
    - Current state of `Implicits.scala` (post-Task 1 — should only contain `object Implicits`)
    - `git grep -nE '\bImplicits\.' -- '*.scala'` — re-verify 0 callers
  </read_first>
  <action>
    1. Re-verify 0 callers:
       ```bash
       git grep -nE '\bImplicits\.' -- '*.scala'
       # Expected: 0 hits
       ```
    2. Delete the file:
       ```bash
       git rm core/src/main/scala/com/avsystem/commons/misc/Implicits.scala
       ```
    3. Confirm file gone:
       ```bash
       test ! -f core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && echo "deleted"
       ```
    4. `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` exit 0.
    5. Commit: `git commit -m "refactor(scala-3,core): delete Implicits object (covered by summon[T])"` body: `Implicits.infer / inferNonMacro were Scala 2 macro helpers. Scala 3 summon[T] + @implicitNotFound on using params replace the use case. 0 callers in tree (git grep verified). ImplicitNotFound moved to its own file in prior commit. User directive 2026-06-01: outright delete, do NOT deprecate (overrides fork commit 50272b26).`
  </action>
  <verify>
    <automated>! test -f core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `git ls-files core/src/main/scala/com/avsystem/commons/misc/Implicits.scala | wc -l` → 0 (file deleted).
    - `git ls-files core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala | wc -l` → 1 (file kept).
    - `git grep -nE '\bImplicits\.' -- '*.scala'` → 0 hits.
    - `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - Single commit: `refactor(scala-3,core): delete Implicits object (covered by summon[T])`.
  </acceptance_criteria>
  <done>
    Implicits.scala gone from tree; ImplicitNotFound.scala remains; compile + tests + scalafmt green; 2 commits total on branch.
  </done>
</task>

<task type="auto">
  <name>Task 3: MIGRATION.md §1 update + open draft PR</name>
  <files>
    MIGRATION.md
  </files>
  <read_first>
    - MIGRATION.md current §1 (Will Not Migrate / Removed APIs) structure
    - ~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md (PR rules — title prefix, draft, milestone)
  </read_first>
  <action>
    1. Open MIGRATION.md, locate `## 1.` section (Removed APIs / Will Not Migrate).
    2. Add new entry:
       - `com.avsystem.commons.misc.Implicits` (object with `infer` / `inferNonMacro`) — REMOVED. Use `scala.compiletime.summon[T]` instead. `ImplicitNotFound` sealed trait kept (now lives in `com.avsystem.commons.misc.ImplicitNotFound`, same package, no caller-visible change).
    3. Commit: `docs(migration): record Implicits object removal`.
    4. Final gate sweep before push:
       - `sbt 'compile ;Test/compile ;scalafmtCheckAll'` exit 0
       - `git ls-files core/src/main/scala/com/avsystem/commons/misc/Implicits.scala | wc -l` → 0
       - `git ls-files core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala | wc -l` → 1
       - `git grep -nE '\bImplicits\.' -- '*.scala'` → 0 hits
       - `! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'` (no GSD nomenclature)
       - `! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'` (no .planning/)
       - `! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` (no new suppressions)
    5. Push: `git push -u origin 03-05-delete-implicits-object`.
    6. Open draft PR via `gh`:
       ```bash
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:03-05-delete-implicits-object \
         --draft \
         --title "[Scala 3] delete Implicits object (covered by summon[T])" \
         --body "$(cat <<'EOF'
       Deletes `com.avsystem.commons.misc.Implicits` object outright (was Scala 2 macro helpers `infer` / `inferNonMacro`, both Phase-1 stubbed). Scala 3 `scala.compiletime.summon[T]` + `@implicitNotFound` on `using` params replace the use case. 0 callers in tree.

       `ImplicitNotFound` sealed trait + companion — separate concern (custom error-message machinery) — extracted to its own file `com/avsystem/commons/misc/ImplicitNotFound.scala`. Same package; no caller-visible change.

       ## Scope
       - DELETE: `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala`
       - CREATE: `core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala` (extracted verbatim)
       - MIGRATION.md §1 entry

       ## Acceptance
       - `git ls-files .../Implicits.scala | wc -l` → 0
       - `git ls-files .../ImplicitNotFound.scala | wc -l` → 1
       - `git grep -nE '\bImplicits\.' -- '*.scala'` → 0 hits
       - `sbt compile + Test/compile + scalafmtCheckAll` green

       User directive 2026-06-01: outright delete, do NOT deprecate (overrides fork commit `50272b26 fix(scala-3): Implicits.infer real impl`).

       **Slice:** 3.5 of Phase 3 (Scala 3 syntax modernization)
       **Merge order:** Independent — can land any time (no overlap with 3.1/3.2/3.3/3.4)
       **Depends on:** none
       **Base branch:** upstream/scala-3
       EOF
       )"
       ```
    7. Capture PR number from `gh pr create` output (note: not a placeholder — use the actual number returned). Assign milestone:
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/<PR_NUM_FROM_PREV_STEP> -f milestone=1
       ```
    8. Verify PR shape:
       ```bash
       gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone --jq '.isDraft and (.title | startswith("[Scala 3]")) and (.milestone.number == 1)'
       # Expected: true
       ```
  </action>
  <verify>
    <automated>gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone --jq '.isDraft and (.title | startswith("[Scala 3]")) and (.milestone.number == 1)'</automated>
  </verify>
  <acceptance_criteria>
    - MIGRATION.md §1 has new sub-entry for `com.avsystem.commons.misc.Implicits` removal pointing to `scala.compiletime.summon`.
    - PR opened at AVSystem/scala-commons with base `scala-3`, head `halotukozak:03-05-delete-implicits-object`.
    - PR is draft (`isDraft: true`), title prefix `[Scala 3]`, milestone "Scala 3" (#1).
    - PR body contains the four-line metadata block (Slice 3.5 / Independent / none / upstream/scala-3).
    - Branch has 3 commits total: extract + delete + docs(migration). No squash. No GSD nomenclature.
    - Final full gate `sbt 'compile ;Test/compile ;scalafmtCheckAll'` green.
  </acceptance_criteria>
  <done>
    Draft PR open at AVSystem/scala-commons with `[Scala 3]` title prefix, milestone 1, base `scala-3`, body metadata complete. Slice 3.5 ready for parallel review alongside 3.1/3.2/3.3/3.4.
  </done>
</task>

</tasks>

<verification>
Phase-level checks for slice 3.5:

```bash
# 1. File system state
git ls-files core/src/main/scala/com/avsystem/commons/misc/Implicits.scala | wc -l       # 0
git ls-files core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala | wc -l # 1

# 2. No callers
git grep -nE '\bImplicits\.' -- '*.scala'   # 0 hits

# 3. Compile + tests + scalafmt
sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0

# 4. No new @nowarn / -Wconf
! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'

# 5. No .planning/ in commits
! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'

# 6. No GSD nomenclature
! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'

# 7. PR conventions
gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone
# Expect: isDraft=true, title starts with "[Scala 3]", milestone.number=1
```
</verification>

<success_criteria>
- `Implicits.scala` deleted; `ImplicitNotFound.scala` created.
- `git grep -nE '\bImplicits\.' -- '*.scala'` → 0 hits.
- `sbt compile + Test/compile + scalafmtCheckAll` exits 0.
- 3 commits on branch (extract + delete + docs); no squash.
- MIGRATION.md §1 updated.
- Draft PR at AVSystem/scala-commons: base=`scala-3`, draft, `[Scala 3]` title, milestone 1, body metadata block with `Slice 3.5 / Independent / none / upstream/scala-3`.
- No new `@nowarn` / `-Wconf`. No `.planning/` in commits. No GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/03-scala-3-syntax-modernization/03-05-SUMMARY.md` documenting:
- File delete + create confirmations
- Commits landed (sha + subject)
- Final caller-grep result (0 hits)
- PR URL + number
</output>
