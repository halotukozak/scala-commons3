---
phase: 05-leaf-feature-restoration
plan: 02
type: execute
wave: 1
depends_on: ["05-00-miscmacros-foundation"]
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/Delegation.scala
  - core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala
  - MIGRATION.md
autonomous: false
requirements:
  - DELEGATION-01
must_haves:
  truths:
    - "Delegation.scala matches fork verbatim — companion `extends DelegationMacros` (trait defined in MiscMacros.scala from slice 5.0)"
    - "Delegation.materializeDelegation body remains `???` (matches fork staging)"
    - "DelegationTest stays `ignore`d (matches fork)"
    - "`commons-core/compile` green; runtime path still throws NotImplementedError (acceptable per CONTEXT)"
    - "MIGRATION.md §3 records Delegation as Phase-5 port with Phase-6 real-impl deferral"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/Delegation.scala"
      provides: "Delegation trait + companion + CurriedDelegation, all extending Macros traits"
      contains: "extends DelegationMacros"
    - path: "MIGRATION.md"
      provides: "Delegation port + Phase-6 deferral entry"
      contains: "Delegation"
  key_links:
    - from: "Delegation.scala companion"
      to: "MiscMacros.scala DelegationMacros trait"
      via: "extends DelegationMacros"
      pattern: "extends DelegationMacros"
    - from: "Delegation.CurriedDelegation"
      to: "MiscMacros.scala DelegationApplyMacros[B] trait"
      via: "extends DelegationApplyMacros[B]"
      pattern: "extends DelegationApplyMacros"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala` verbatim from fork (21 LOC). Per CONTEXT locked decision + research finding: fork keeps `???` body for `materializeDelegation` — matching test stays `ignore`d. We do the SAME.

Purpose: Slice 5.2 — DELEGATION-01 (stub-only port). Per [[feedback_crib_from_master]]: match fork state, including the staging stubs. Real macro impl is Phase 6+ scope.

Output:
- Updated `Delegation.scala` (verbatim port — companion extends `DelegationMacros` trait from slice 5.0's MiscMacros.scala)
- DelegationTest stays wrapped/ignored
- MIGRATION.md §3 entry noting Phase-6 deferral
- 2 atomic commits (feat + docs) — own PR, base = `05-00-miscmacros-foundation` (stacked on slice 5.0)
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md
@.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md
@.planning/phases/05-leaf-feature-restoration/05-VALIDATION.md

<interfaces>
<!-- Full fork Delegation.scala (21 LOC, verbatim) -->
package com.avsystem.commons
package misc

/**
 * A typeclass which witnesses that type `A` can be wrapped into trait or abstract class `B`
 */
trait Delegation[A, B] {
  def delegate(a: A): B
}

object Delegation extends DelegationMacros {
  /** Provides following syntax: Delegation[TargetType](value) */
  def apply[B] = new CurriedDelegation[B]
  class CurriedDelegation[B] extends DelegationApplyMacros[B]
}

<!-- DelegationMacros + DelegationApplyMacros traits live in MiscMacros.scala (slice 5.0) -->
<!-- Per Phase 4 staging: `materializeDelegation` body is `???` — runtime NotImplementedError -->
</interfaces>

<branch_strategy>
Branch off `05-00-miscmacros-foundation` tip (stacked on slice 5.0). PR base = `05-00-miscmacros-foundation` until that PR merges. Note: slice 5.0 lives on `origin` (which IS the halotukozak fork — `upstream` = AVSystem).
  git fetch origin && git checkout --track origin/05-00-miscmacros-foundation 2>/dev/null || true
  git checkout -b 05-02-delegation-stub 05-00-miscmacros-foundation
</branch_strategy>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Port Delegation.scala verbatim from fork; un-wrap DelegationTest with `ignore` matching fork</name>
  <files>
    core/src/main/scala/com/avsystem/commons/misc/Delegation.scala
    core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala
  </files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Summary point 2 + Don't Hand-Roll table row 3 + Common Pitfall 6)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md` (Delegation decision: stub-only port)
    - Current file: `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala`
    - Fork source: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Delegation.scala`
    - Fork test (to mirror `ignore`d state): `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/DelegationTest.scala`
    - Current test: `cat core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala`
    - Verify slice 5.0 traits exist: `grep -E 'trait Delegation(Macros|ApplyMacros)' core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala`
  </read_first>
  <action>
    Verbatim port + matching test state:

    1. Branch (slice 5.0 base on origin (= halotukozak fork)):
       `git fetch origin`
       `git checkout --track origin/05-00-miscmacros-foundation 2>/dev/null || git checkout 05-00-miscmacros-foundation`
       `git checkout -b 05-02-delegation-stub 05-00-miscmacros-foundation`
    2. Overwrite Delegation.scala:
       `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Delegation.scala > core/src/main/scala/com/avsystem/commons/misc/Delegation.scala`
    3. Compare test states. If current `DelegationTest.scala` is wrapped in `/* ... */` from Phase 1 big-bang AND fork's `DelegationTest.scala` has `.ignore` markers (not wrapped): un-wrap our test BUT add `.ignore` to every test case so behaviour matches fork. Crib exact shape: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/DelegationTest.scala > core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala`.
    4. Run `sbt commons-core/compile` + `sbt 'commons-core/testOnly *.DelegationTest'`. EXPECTED: compile green, test execution reports all cases `ignore`d (0 ran, 0 failed, N ignored).
    5. `sbt scalafmtCheckAll` — if it complains, `scalafmtAll` and reconfirm minimal diff is whitespace-only.
    6. Commit (single atomic): `feat(scala-3,core): port Delegation (stub matches fork)` with body referencing fork file + noting `materializeDelegation` remains `???` per fork staging. Test changes bundled into this commit because they are purely the un-wrap-to-ignored state shift (no behaviour added).

       Per CONTEXT cadence: usually 2 commits (feat + test). Here test un-wrap is a no-op state-mirror — bundling is appropriate. Document choice in commit body.
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/compile ;commons-core/testOnly *.DelegationTest ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - Diff vs fork (`diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Delegation.scala) core/src/main/scala/com/avsystem/commons/misc/Delegation.scala`) is minimal — whitespace/scalafmt normalization only. Treat this as documentation, not a hard gate (scalafmt may reflow).
    - `grep -q 'extends DelegationMacros' core/src/main/scala/com/avsystem/commons/misc/Delegation.scala` exits 0
    - `grep -q 'extends DelegationApplyMacros' core/src/main/scala/com/avsystem/commons/misc/Delegation.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/Delegation.scala` (no `???` in Delegation.scala itself; `???` lives in MiscMacros.scala from slice 5.0)
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch 'commons-core/testOnly *.DelegationTest'` exits 0 (ignored cases pass / 0 failures)
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): port Delegation`
  </acceptance_criteria>
  <done>Delegation.scala verbatim port; test matches fork `ignore`d state; single feat commit on branch.</done>
</task>

<task type="auto">
  <name>Task 2: Update MIGRATION.md §3 (Delegation port + Phase-6 deferral)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md` (find §3 + backlog)
  </read_first>
  <action>
    Two edits in one commit:

    1. §3 (Source-compat / per-slice notes), `core` row append:
       ```
       | misc/Delegation | n/a | n/a | Phase-5 slice 5.2: ported verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Delegation.scala`. `materializeDelegation` body remains `???` (lives in MiscMacros.scala from slice 5.0) — runtime NotImplementedError per fork staging. Matching `DelegationTest` is `ignore`d. Real impl deferred to Phase 6+. |
       ```

    2. Backlog table: remove any `Delegation` rows from Phase-1 `TODO[scala3-port]` seeding per BACKLOG-02.

    Commit: `docs(migration): record Delegation stub port` (separate atomic commit).
  </action>
  <verify>
    <automated>grep -q 'Phase-5 slice 5.2' MIGRATION.md &amp;&amp; grep -q 'Delegation' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'slice 5.2' MIGRATION.md` exits 0
    - `grep -q 'Delegation' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record Delegation stub port$`
  </acceptance_criteria>
  <done>MIGRATION.md updated; second atomic commit on branch.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 3: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-02-delegation-stub` with 2 commits. Delegation port matches fork; tests `ignore`d. DELEGATION-01 satisfied.</what-built>
  <how-to-verify>
    Pre-push:
    1. `git log --oneline 05-00-miscmacros-foundation..HEAD` shows exactly 2 commits
    2. `git diff 05-00-miscmacros-foundation..HEAD --stat` shows Delegation.scala + DelegationTest.scala + MIGRATION.md only
    3. `sbt 'commons-core/compile ;commons-core/testOnly *.DelegationTest ;scalafmtCheckAll'` exit 0
    4. `! git diff 05-00-miscmacros-foundation..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`

    Then:
    ```
    git push -u origin 05-02-delegation-stub
    gh pr create --draft --base 05-00-miscmacros-foundation --head halotukozak:05-02-delegation-stub \
      --title "[Scala 3] port Delegation (stub matches fork)" \
      --body "Slice 5.2 / Depends on: slice 5.0 (MiscMacros foundation) — PR #<5.0-PR> / Base branch: 05-00-miscmacros-foundation / Requirement: DELEGATION-01 / Note: materializeDelegation body is `???` per fork staging; real impl deferred to Phase 6+; DelegationTest stays `ignore`d"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- Delegation.scala byte-identical to fork (modulo scalafmt)
- DelegationTest stays `ignore`d
- `commons-core/compile` green
- PR draft, prefix, milestone 1, base = `05-00-miscmacros-foundation`
</verification>

<success_criteria>
- DELEGATION-01 satisfied (compile passes, runtime NotImplementedError matches fork)
- Delegation.scala matches fork verbatim
- MIGRATION.md records port + deferral
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-02-SUMMARY.md`
</output>
