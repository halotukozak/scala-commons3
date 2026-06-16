---
phase: 05-leaf-feature-restoration
plan: 06
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala
  - core/src/test/scala/com/avsystem/commons/misc/SealedEnumTest.scala
  - core/src/test/scala/com/avsystem/commons/misc/NamedEnumTest.scala
  - MIGRATION.md
autonomous: false
requirements:
  - SEALEDUTILS-01
must_haves:
  truths:
    - "SealedUtils.scala uses pure inline + compiletime.{summonAll, summonFrom, erasedValue} + Mirror.SumOf (no quoted impl) — matches fork"
    - "SealedUtils.caseObjectsFor is REMOVED (zero internal callers per pre-port audit — only the stub itself)"
    - "`SealedUtils.caseObjects[Color]` returns all case objects at runtime"
    - "SealedEnumTest + NamedEnumTest un-wrapped and green"
    - "MIGRATION.md §3 records pure-inline reshape + caseObjectsFor removal"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala"
      provides: "instancesFor, caseObjects + SealedEnumCompanion.evidence (via scala.ValueOf)"
      contains: "compiletime.summonAll"
  key_links:
    - from: "SealedUtils.scala inline defs"
      to: "scala.deriving.Mirror.SumOf + scala.compiletime"
      via: "inline def + Tuple.Map + summonFrom"
      pattern: "Mirror\\.SumOf"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala` verbatim from fork (185 LOC). Per research Pattern 4 (Pure Inline — no `${...}` splice): uses `compiletime.{summonAll, summonFrom, erasedValue}` + `Mirror.SumOf` + `scala.ValueOf` (per fork commit `3ec8c125`). Independent of MiscMacros.scala (no quoted impl).

Pre-port audit (per RESEARCH State-of-the-Art note): `caseObjectsFor` REMOVED in fork — verify zero internal callers (confirmed during planning: only the stub itself uses the name).

Output:
- Updated `SealedUtils.scala` (pure inline)
- Un-wrapped `SealedEnumTest.scala` + `NamedEnumTest.scala`
- MIGRATION.md §3 entry
- 3 atomic commits (feat + test + docs) — own PR, independent base
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
<!-- Fork SealedUtils.scala — Pattern 4 (pure inline, no quoted) -->
object SealedUtils {
  inline def instancesFor[TC[_], T: Mirror.SumOf as m]: List[TC[T]] =
    compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, TC]].toList.asInstanceOf[List[TC[T]]]

  inline def caseObjects[T: Mirror.SumOf as m]: List[T] =
    collectCaseObjects[T, m.MirroredElemTypes]

  inline private def collectCaseObjects[T, Tup <: Tuple]: List[T] = inline compiletime.erasedValue[Tup] match {
    case _: (h *: t) =>
      compiletime.summonFrom {
        case vo: scala.ValueOf[`h`]    => vo.value.asInstanceOf[T] :: Nil
        case m: Mirror.SumOf[`h`]      => collectCaseObjects[T, m.MirroredElemTypes]
        case _                         => Nil
      } ::: collectCaseObjects[T, t]
    case _: EmptyTuple => Nil
  }
}
<!-- NOTE: caseObjectsFor[T] is REMOVED in fork — current stub:
       core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala:10:  def caseObjectsFor[T]: List[T] = ???
     is the ONLY hit per pre-port audit. Safe to delete. -->
</interfaces>

<branch_strategy>
Branch off `04-05-meta-annotations @ f04cec6f`. Independent.
  git fetch origin upstream && git checkout -b 05-06-sealed-utils 04-05-meta-annotations
</branch_strategy>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Pre-port audit + verbatim port of SealedUtils.scala</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Pattern 4 — Pure Inline; Code Examples Ex 3; State-of-the-Art `caseObjectsFor` REMOVED note; Open Question 4 — SealedEnumCompanion.values shape; commit 3ec8c125)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md`
    - Current file: `cat core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala`
    - Fork source: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala`
    - SealedEnumCompanion subclasses (Open Question 4): `git grep -nE 'extends SealedEnumCompanion\\[' -- '*.scala'`
  </read_first>
  <action>
    1. **Pre-port grep audit:**
       ```
       git grep -nE 'caseObjectsFor' -- '*.scala'         # expect: only line 10 of SealedUtils.scala itself
       git grep -nE 'extends SealedEnumCompanion\\[' -- '*.scala' | head  # gather subclasses for Open Question 4 check
       ```
       Verify `caseObjectsFor` grep returns only the SealedUtils.scala stub line. If any external caller appears at execution time — STOP and document; the removal becomes a real bincompat break needing a deprecation cycle decision.
    2. Branch: `git checkout -b 05-06-sealed-utils 04-05-meta-annotations`
    3. Overwrite: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala > core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala`
    4. Per Open Question 4: review subclasses of `SealedEnumCompanion` for `lazy val values` vs `def values` override mismatches. Phase 1 Plan 2 SUMMARY notes a `lazy val` Rule-1 fix for 3 specific subclasses (Tag, NamedEnumTest, SealedEnumTest). If the fork shape requires `def values` and our subclasses override with `lazy val` — adjust subclasses per [[feedback_crib_from_master]] to match fork pattern. Document in commit body.
    5. Run `sbt commons-core/compile`. EXPECTED: green. If downstream consumer breaks, fix consumer (NOT revert leaf).
    6. `sbt scalafmtCheckAll` + auto-fix.
    7. Commit: `feat(scala-3,core): port SealedUtils (pure inline + Mirror.SumOf)` with body referencing fork file + commit `3ec8c125` (scala.ValueOf) + `caseObjectsFor` removal noted.
  </action>
  <verify>
    <automated>! git grep -nE 'caseObjectsFor' -- '*.scala' &amp;&amp; grep -q 'compiletime.summonAll' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala &amp;&amp; grep -q 'Mirror.SumOf' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `! git grep -nE 'caseObjectsFor' -- '*.scala'` (zero hits — entirely removed)
    - `grep -q 'compiletime.summonAll' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala` exits 0
    - `grep -q 'Mirror.SumOf' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala` exits 0
    - `grep -q 'scala.ValueOf' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala` (no stub bodies)
    - `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala) core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala` shows minimal diff
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): port SealedUtils`
  </acceptance_criteria>
  <done>SealedUtils.scala matches fork; caseObjectsFor removed; compile green.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Un-wrap SealedEnumTest + NamedEnumTest</name>
  <files>
    core/src/test/scala/com/avsystem/commons/misc/SealedEnumTest.scala
    core/src/test/scala/com/avsystem/commons/misc/NamedEnumTest.scala
  </files>
  <read_first>
    - Current tests: both files; check Phase-1 wrap state
    - Fork tests: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/SealedEnumTest.scala` (NamedEnumTest may not exist in fork)
  </read_first>
  <action>
    Per VALIDATION.md SEALEDUTILS-01 row: both tests are the verify target.

    1. SealedEnumTest: prefer overwrite from fork if available, else surgical un-wrap.
    2. NamedEnumTest: fork may not have it — surgical un-wrap of our existing file.
    3. Run `sbt 'commons-core/testOnly *.SealedEnumTest *.NamedEnumTest'`. EXPECTED: green; cases requiring Phase-6 → `.ignore`.
    4. Commit: `test(scala-3,core): un-wrap SealedEnumTest + NamedEnumTest`.
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/testOnly *.SealedEnumTest *.NamedEnumTest'</automated>
  </verify>
  <acceptance_criteria>
    - `! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/SealedEnumTest.scala`
    - `! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/NamedEnumTest.scala`
    - `sbt -batch 'commons-core/testOnly *.SealedEnumTest *.NamedEnumTest'` exits 0 with 0 failures
    - `git log -1 --pretty=%s` matches `^test\\(scala-3,core\\): un-wrap SealedEnumTest`
  </acceptance_criteria>
  <done>Both tests un-wrapped + green; second atomic commit.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 3: Update MIGRATION.md §3 + §1 (SealedUtils port + caseObjectsFor removal)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md`
  </read_first>
  <action>
    Two-entry update:

    1. §3 `core` row append:
       ```
       | misc/SealedUtils | n/a | source-compat (caseObjectsFor removed) | Phase-5 slice 5.6: ported from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala`. Pure inline (no quoted macro) — uses `compiletime.{summonAll, summonFrom, erasedValue}` + `Mirror.SumOf` + `scala.ValueOf` per fork commit 3ec8c125. SealedEnumTest + NamedEnumTest re-enabled. |
       ```

    2. §1 (Will Not Migrate) row append:
       ```
       | misc/SealedUtils.caseObjectsFor | Removed in fork — replaced by `caseObjects[T: Mirror.SumOf]`. Zero internal callers per pre-port audit. |
       ```

    3. Backlog: remove `caseObjectsFor` and any `SealedUtils` rows.

    Commit: `docs(migration): record SealedUtils port + caseObjectsFor removal`.
  </action>
  <verify>
    <automated>grep -q 'slice 5.6' MIGRATION.md &amp;&amp; grep -q 'caseObjectsFor' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'slice 5.6' MIGRATION.md` exits 0
    - `grep -q 'SealedUtils' MIGRATION.md` exits 0
    - `grep -q 'caseObjectsFor' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record SealedUtils port`
  </acceptance_criteria>
  <done>MIGRATION.md updated; third atomic commit.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-06-sealed-utils` with 3 commits. SEALEDUTILS-01 satisfied via pure-inline derivation; caseObjectsFor removed.</what-built>
  <how-to-verify>
    Pre-push:
    1. `git log --oneline 04-05-meta-annotations..HEAD | wc -l` == 3
    2. `sbt 'commons-core/compile ;commons-core/testOnly *.SealedEnumTest *.NamedEnumTest ;scalafmtCheckAll'` exit 0
    3. `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`
    4. `! git grep -nE 'caseObjectsFor' -- '*.scala'`
    5. `! git grep '???' core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala`

    Then:
    ```
    git push -u origin 05-06-sealed-utils
    gh pr create --draft --base 04-05-meta-annotations --head halotukozak:05-06-sealed-utils \
      --title "[Scala 3] port SealedUtils (pure inline)" \
      --body "Slice 5.6 / Parallel — independent (pure inline, no MiscMacros dep) / Depends on: none / Base branch: 04-05-meta-annotations / Requirement: SEALEDUTILS-01 / Note: caseObjectsFor removed per fork (zero internal callers); SealedUtils.caseObjects[T: Mirror.SumOf] replaces it; uses scala.ValueOf per fork 3ec8c125"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- SealedUtils.scala matches fork; pure inline; caseObjectsFor gone
- SealedEnumTest + NamedEnumTest green
- MIGRATION.md §3 + §1 updated
- Draft PR, prefix, milestone 1
</verification>

<success_criteria>
- SEALEDUTILS-01 satisfied
- `SealedUtils.caseObjects[Color]` returns all case objects
- caseObjectsFor entirely removed; bincompat documented
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-06-SUMMARY.md`
</output>
