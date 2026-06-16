---
phase: 05-leaf-feature-restoration
plan: 07
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala
  - core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala
  - MIGRATION.md
  - .planning/phases/05-leaf-feature-restoration/05-VALIDATION.md
autonomous: false
requirements:
  - VALUEENUM-01
must_haves:
  truths:
    - "ValueEnum.scala matches fork verbatim — `valNameImpl` is TOP-LEVEL `def` (NOT in MiscMacros) per fork shape + Open Question 2"
    - "`Weekday.values` has correct ordinals and names at runtime (per VALIDATION row VALUEENUM-01)"
    - "ValueEnumTest un-wrapped + green"
    - "Object init order matches fork (synchronized + awaitingRegister flag dance preserved per Pitfall 8)"
    - "MIGRATION.md §3 records port"
    - "After full suite (`commons-core/compile ;commons-core/test ;scalafmtCheckAll`) green, 05-VALIDATION.md frontmatter flags `nyquist_compliant` + `wave_0_complete` are flipped to `true` and sign-off checklist ticked"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala"
      provides: "ValueEnum + ValueEnumCompanion + Ctx machinery + top-level valNameImpl"
      contains: "Symbol.spliceOwner.owner"
  key_links:
    - from: "ValueEnum.scala companion `valName` inline def"
      to: "top-level `valNameImpl` (same file)"
      via: "${ valNameImpl[T, ValName, Owner]('createValName) }"
      pattern: "valNameImpl"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala` verbatim from fork (173 LOC). Per RESEARCH Open Question 2 + Code Examples Ex 5: `valNameImpl` is a TOP-LEVEL `def` in this file (NOT in `MiscMacros.scala`) — Pattern 5 (Enclosing-Symbol Walk via `Symbol.spliceOwner.owner`). Independent of slice 5.0.

Pitfall 5 (Symbol.spliceOwner.owner — must `.owner`, not just `spliceOwner`) and Pitfall 8 (SI-7046-style init-order trap) are real risks — verbatim crib avoids both.

Output:
- Updated `ValueEnum.scala` (verbatim port)
- Un-wrapped `ValueEnumTest.scala`
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
<!-- Fork ValueEnum.scala valNameImpl (TOP LEVEL — Open Question 2, Pattern 5, Pitfall 5) -->
def valNameImpl[T <: ValueEnum: Type, ValName: Type, Owner: Type](
  createValName: Expr[String => ValName],
)(using quotes: Quotes): Expr[ValName] = {
  import quotes.reflect.*

  def omitAnonClass(owner: Symbol): Symbol =
    if (owner.isDefDef && owner.name == "<init>" && owner.owner.name.contains("$anon")) owner.owner.owner
    else owner

  extension (s: Symbol) def isPublic: Boolean = !s.flags.is(Flags.Protected) && !s.flags.is(Flags.Private) && !s.flags.is(Flags.PrivateLocal)

  val owner = omitAnonClass(Symbol.spliceOwner.owner)
  val valid = owner.isTerm && owner.owner == TypeRepr.of[Owner].typeSymbol && owner.isValDef &&
    owner.flags.is(Flags.Final) && !owner.flags.is(Flags.Lazy) && owner.isPublic &&
    owner.typeRef <:< TypeRepr.of[T]

  if (!valid) report.errorAndAbort(
    "ValueEnum must be assigned to a public, final, non-lazy val in its companion object ...",
  )
  val name = Expr(owner.name)
  '{ $createValName.apply($name) }
}
</interfaces>

<branch_strategy>
Branch off `04-05-meta-annotations @ f04cec6f`. Independent.
  git fetch origin upstream && git checkout -b 05-07-value-enum 04-05-meta-annotations
</branch_strategy>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Port ValueEnum.scala verbatim from fork (top-level valNameImpl)</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Pattern 5 — Enclosing-Symbol Walk; Code Examples Ex 5; Open Question 2; Pitfall 5; Pitfall 8 — SI-7046 init-order trap)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md`
    - Current file: `cat core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala`
    - Fork source: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala`
    - Confirm `valNameImpl` is top-level in fork (not in MiscMacros.scala): `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala | grep -c 'valNameImpl'` should report 0
  </read_first>
  <action>
    1. Branch: `git checkout -b 05-07-value-enum 04-05-meta-annotations`
    2. Overwrite: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala > core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala`
    3. Per Pitfall 5: verify `Symbol.spliceOwner.owner` (with the `.owner`!) is present — NOT just `Symbol.spliceOwner`. Per Pitfall 8: preserve fork's exact `synchronized` / `awaitingRegister` flag dance + `lazy val` semantics around `values` collection (Ctx.register mechanism). DO NOT optimize.
    4. Run `sbt commons-core/compile`. EXPECTED: green. ValueEnum has no MiscMacros dep so slice-5.0 status is irrelevant.
    5. `sbt scalafmtCheckAll` + auto-fix.
    6. Commit: `feat(scala-3,core): port ValueEnum (top-level valNameImpl)` with body referencing fork file + noting Pattern 5 + Pitfalls 5 + 8.
  </action>
  <verify>
    <automated>grep -q 'def valNameImpl' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala &amp;&amp; grep -q 'Symbol.spliceOwner.owner' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'def valNameImpl' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala` exits 0
    - `grep -q 'Symbol.spliceOwner.owner' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala` exits 0
    - `grep -q 'omitAnonClass' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala`
    - `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala) core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala` shows minimal diff
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): port ValueEnum`
  </acceptance_criteria>
  <done>ValueEnum.scala matches fork; compile green; one feat commit.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Un-wrap ValueEnumTest</name>
  <files>core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala</files>
  <read_first>
    - Current: `cat core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala`
    - Fork: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/ValueEnumTest.scala`
  </read_first>
  <action>
    1. Prefer wholesale overwrite from fork if no out-of-scope deps; else surgical un-wrap.
    2. Run `sbt 'commons-core/testOnly *.ValueEnumTest'`. EXPECTED: green; Phase-6-dependent cases → `.ignore`.
    3. Per Pitfall 8: if `values.head.ordinal != 0` or `IllegalStateException("Cannot register ...")` appears at app startup during test — that means our `lazy val` / `synchronized` shape diverges from fork. Re-verify Task 1's verbatim port. DO NOT band-aid by reordering test cases.
    4. Commit: `test(scala-3,core): un-wrap ValueEnumTest`.
  </action>
  <verify>
    <automated>! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala &amp;&amp; sbt -batch 'commons-core/testOnly *.ValueEnumTest'</automated>
  </verify>
  <acceptance_criteria>
    - `! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala`
    - `sbt -batch 'commons-core/testOnly *.ValueEnumTest'` exits 0 with 0 failures
    - `git log -1 --pretty=%s` matches `^test\\(scala-3,core\\): un-wrap ValueEnumTest$`
  </acceptance_criteria>
  <done>ValueEnumTest un-wrapped + green; second atomic commit.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 3: Update MIGRATION.md §3 (ValueEnum port)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md`
  </read_first>
  <action>
    §3 `core` row append:
    ```
    | misc/ValueEnum | n/a | n/a | Phase-5 slice 5.7: ported from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala`. Top-level `valNameImpl` (NOT in MiscMacros — Pattern 5 enclosing-symbol walk via `Symbol.spliceOwner.owner`). Ctx machinery + synchronized/awaitingRegister flag dance preserved verbatim (Pitfall 8 — SI-7046-style init-order). ValueEnumTest re-enabled. |
    ```
    Backlog: remove ValueEnum rows.

    Commit: `docs(migration): record ValueEnum port`.
  </action>
  <verify>
    <automated>grep -q 'slice 5.7' MIGRATION.md &amp;&amp; grep -q 'ValueEnum' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'slice 5.7' MIGRATION.md` exits 0
    - `grep -q 'ValueEnum' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record ValueEnum port$`
  </acceptance_criteria>
  <done>MIGRATION.md updated; third atomic commit.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-07-value-enum` with 3 commits. VALUEENUM-01 satisfied — Weekday.values has correct ordinals + names at runtime; verbatim fork port preserves init-order semantics.</what-built>
  <how-to-verify>
    Pre-push:
    1. `git log --oneline 04-05-meta-annotations..HEAD | wc -l` == 3
    2. `sbt 'commons-core/compile ;commons-core/testOnly *.ValueEnumTest ;scalafmtCheckAll'` exit 0
    3. `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`
    4. `! git grep '???' core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala`

    Then:
    ```
    git push -u origin 05-07-value-enum
    gh pr create --draft --base 04-05-meta-annotations --head halotukozak:05-07-value-enum \
      --title "[Scala 3] port ValueEnum (top-level valNameImpl)" \
      --body "Slice 5.7 / Parallel — independent (top-level valNameImpl, NOT in MiscMacros) / Depends on: none / Base branch: 04-05-meta-annotations / Requirement: VALUEENUM-01 / Note: Pattern 5 enclosing-symbol walk via Symbol.spliceOwner.owner; Ctx init-order machinery preserved verbatim (Pitfall 8)"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone, or describe issues.</resume-signal>
</task>

<task type="auto">
  <name>Task 5: Phase 5 sign-off — full suite green + flip VALIDATION.md sign-off flags</name>
  <files>.planning/phases/05-leaf-feature-restoration/05-VALIDATION.md</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-VALIDATION.md` (frontmatter flags + sign-off checklist at bottom)
  </read_first>
  <action>
    This is the phase-gate. Must run AFTER all slice PRs (5.0 - 5.7) are merged/stacked locally and present on this branch tip — i.e. when the executor is sitting on the last slice with all prior slices merged in.

    1. Run the full phase suite:
       `sbt -batch 'commons-core/compile ;commons-core/test ;scalafmtCheckAll'`
       EXPECTED: exit 0 across compile + test + scalafmt.
    2. Sanity-check that `???` only appears where fork keeps it (Delegation stub family):
       `git grep '???' core/src/main/scala/com/avsystem/commons/misc/`
       Expected matches: only `Delegation`/`DelegationApply`-related lines from MiscMacros.scala (slice 5.0).
    3. Update `.planning/phases/05-leaf-feature-restoration/05-VALIDATION.md`:
       - Frontmatter: flip `nyquist_compliant: false` → `true` and `wave_0_complete: false` → `true`.
       - Sign-Off section at bottom: tick all checkboxes (`- [ ]` → `- [x]`).
       - Change `**Approval:** pending` → `**Approval:** signed-off YYYY-MM-DD` (use today's date).
    4. Commit: `docs(phase-5): sign off VALIDATION.md (full suite green)`.
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/compile ;commons-core/test ;scalafmtCheckAll' &amp;&amp; grep -q 'nyquist_compliant: true' .planning/phases/05-leaf-feature-restoration/05-VALIDATION.md &amp;&amp; grep -q 'wave_0_complete: true' .planning/phases/05-leaf-feature-restoration/05-VALIDATION.md &amp;&amp; ! grep -qE '^- \[ \]' .planning/phases/05-leaf-feature-restoration/05-VALIDATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `sbt -batch 'commons-core/compile ;commons-core/test ;scalafmtCheckAll'` exits 0
    - `grep -q 'nyquist_compliant: true' .planning/phases/05-leaf-feature-restoration/05-VALIDATION.md` exits 0
    - `grep -q 'wave_0_complete: true' .planning/phases/05-leaf-feature-restoration/05-VALIDATION.md` exits 0
    - All sign-off checkboxes in VALIDATION.md are `- [x]` (no remaining `- [ ]` in the Sign-Off section)
    - `**Approval:** signed-off` line present
    - `git log -1 --pretty=%s` matches `^docs\\(phase-5\\): sign off VALIDATION.md`
  </acceptance_criteria>
  <done>Phase-5 VALIDATION.md signed off; full suite green; sign-off flags flipped.</done>
</task>

</tasks>

<verification>
- ValueEnum.scala matches fork; top-level valNameImpl present
- ValueEnumTest green
- MIGRATION.md updated
- Draft PR, prefix, milestone 1
</verification>

<success_criteria>
- VALUEENUM-01 satisfied
- Weekday.values correct ordinals + names
- Init-order semantics match fork (no IllegalStateException)
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-07-SUMMARY.md`
</output>
