---
phase: 05-leaf-feature-restoration
plan: 03
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala
  - core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala
  - MIGRATION.md
autonomous: false
requirements:
  - APPLIERUNAPPLIER-01
must_haves:
  truths:
    - "ApplierUnapplier.scala uses Mirror.ProductOf (no quoted macro impl) — matches fork"
    - "`Applier[Foo].apply(Seq(1, \"x\"))` reconstructs a case class Foo at runtime"
    - "ApplierUnapplierTest is un-wrapped and green"
    - "`commons-core/compile` + ApplierUnapplierTest both green"
    - "MIGRATION.md §3 records the Mirror-based reshape (no quoted macro)"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala"
      provides: "Applier / Unapplier / ApplierUnapplier traits + Mirror-based given derived"
      contains: "Mirror.ProductOf"
    - path: "core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala"
      provides: "Unwrapped test exercising ApplierUnapplier via Mirror"
      contains: "ApplierUnapplier"
  key_links:
    - from: "ApplierUnapplier.scala given derived"
      to: "scala.deriving.Mirror"
      via: "given derived[T <: Product: Mirror.ProductOf as m]"
      pattern: "Mirror\\.(ProductOf|SumOf)"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala` verbatim from fork (42 LOC). Fork uses `Mirror.ProductOf`-based `given derived` — NO quoted macro impl. Independent of MiscMacros.scala (slice 5.0).

Per [[feedback_crib_from_master]] + research Pattern 3 (Mirror-Based Derivation): crib fork shape. Un-wrap matching test per CONTEXT (fork re-enabled at `7085bd8f`).

Output:
- Updated `ApplierUnapplier.scala` (Mirror-based)
- Un-wrapped `ApplierUnapplierTest.scala`
- MIGRATION.md §3 entry
- 3 atomic commits per CONTEXT cadence (feat + test + docs)
- Own PR base `04-05-meta-annotations` (independent — no MiscMacros dep)
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
<!-- Fork ApplierUnapplier.scala (key portion — verbatim crib) -->
object Applier {
  given derived[T <: Product: Mirror.ProductOf as m]: Applier[T] = rawValues =>
    m.fromTuple(Tuple.fromArray(rawValues.toArray).asInstanceOf[m.MirroredElemTypes])
}
object Unapplier {
  given derived[T <: Product]: Unapplier[T] = value => IArraySeq.unsafeWrapArray(value.productIterator.toArray)
}
object ApplierUnapplier {
  given derived[T: {Applier as applier, Unapplier as unapplier}]: ApplierUnapplier[T] = new ApplierUnapplier[T] {
    override def apply(rawValues: Seq[Any]): T = applier.apply(rawValues)
    override def unapply(value: T): Seq[Any] = unapplier.unapply(value)
  }
}
<!-- Pattern 3 in RESEARCH.md: Scala 3.8 `[T: TC as alias]` summon-into-alias sugar -->
</interfaces>

<branch_strategy>
Branch off `04-05-meta-annotations @ f04cec6f`. Independent of slice 5.0.
  git fetch origin upstream && git checkout -b 05-03-applier-unapplier 04-05-meta-annotations
</branch_strategy>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Port ApplierUnapplier.scala from fork (Mirror-based given derived)</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Architecture Pattern 3 — Mirror-Based Derivation; Code Examples not enumerated for AU but shape in interfaces above; Open Question 4 re: SealedEnumCompanion not relevant here)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md`
    - Current file: `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala`
    - Fork source: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala`
    - Callers in tree: `git grep -nE '\\b(Applier|Unapplier|ApplierUnapplier)\\b' -- '*.scala' | grep -v misc/ApplierUnapplier`
  </read_first>
  <action>
    Verbatim port:

    1. Cut branch: `git checkout -b 05-03-applier-unapplier 04-05-meta-annotations`
    2. Overwrite: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala > core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala`
    3. Reconcile imports if needed (single-source layout — same package). Fork uses `import scala.collection.immutable.ArraySeq.ofRef` etc; mirror exactly.
    4. Verify Scala 3.8 `[T: TC as alias]` sugar compiles (research confirms 3.8.2 supports it; verified Phase 4 precedent).
    5. Audit `materialize` callers (if any remain from Phase-1 stub shape e.g. `ApplierUnapplier.materialize`): switch to `summon[ApplierUnapplier[T]]` via fork's `given derived` resolution. Document any non-trivial caller migration in the commit body.
    6. Run `sbt commons-core/compile`. EXPECTED: green.
    7. `sbt scalafmtCheckAll`; auto-fix via `scalafmtAll` if needed (Rule 3).
    8. Commit: `feat(scala-3,core): port ApplierUnapplier (Mirror-based)` with body referencing fork file + `7085bd8f` test re-enable commit.

    Per Common Pitfall 7 — verify no `implicit val` for the typeclass exists in our tree that would shadow the new `given derived`. Pre-emptive grep: `git grep -E 'implicit (val|def).*ApplierUnapplier' core/src/main/scala/`.
  </action>
  <verify>
    <automated>diff &lt;(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala) core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'Mirror.ProductOf' core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala` exits 0
    - `grep -q 'given derived' core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala`
    - `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala) core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala` shows minimal/zero diff
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): port ApplierUnapplier`
  </acceptance_criteria>
  <done>ApplierUnapplier.scala matches fork; compile green; one feat commit.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Un-wrap ApplierUnapplierTest from Phase-1 big-bang wrap</name>
  <files>core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala</files>
  <read_first>
    - Current test: `cat core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala` (find Phase-1 `/* ... */` wrap)
    - Fork test (target shape): `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/ApplierUnapplierTest.scala`
  </read_first>
  <action>
    Un-wrap:

    1. Diff our wrapped version vs fork to understand if cases need extension/modification: if fork has additional Mirror-specific assertions, prefer overwriting with fork file (`git show origin/master:... > core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala`); otherwise just remove the `/* ... */` wrap (and `// TODO[scala3-port]:` line above) leaving body intact.
    2. Run `sbt 'commons-core/testOnly *.ApplierUnapplierTest'`. EXPECTED: all green. If any case requires Phase 6+ machinery, mark that single case `.ignore` and document in commit body — DO NOT re-wrap the whole file.
    3. Run `sbt scalafmtCheckAll`.
    4. Commit: `test(scala-3,core): un-wrap ApplierUnapplierTest` per CONTEXT cadence.
  </action>
  <verify>
    <automated>! grep -E '^\\s*/\\*\\s*TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala &amp;&amp; sbt -batch 'commons-core/testOnly *.ApplierUnapplierTest'</automated>
  </verify>
  <acceptance_criteria>
    - `! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala`
    - `! grep -qE '^/\\*' core/src/test/scala/com/avsystem/commons/misc/ApplierUnapplierTest.scala | head -1` (no leading block comment wrap)
    - `sbt -batch 'commons-core/testOnly *.ApplierUnapplierTest'` exits 0 with 0 failures
    - `git log -1 --pretty=%s` matches `^test\\(scala-3,core\\): un-wrap ApplierUnapplierTest$`
  </acceptance_criteria>
  <done>Test un-wrapped, all cases green; second atomic commit on branch.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 3: Update MIGRATION.md §3 (ApplierUnapplier Mirror-based reshape)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md`
  </read_first>
  <action>
    §3 `core` row append:
    ```
    | misc/ApplierUnapplier | n/a | source-compat | Phase-5 slice 5.3: ported from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala`. Reshape from macro `materialize` → Mirror-based `given derived[T <: Product: Mirror.ProductOf as m]`. Public API preserved (Applier/Unapplier/ApplierUnapplier traits unchanged); resolution mechanism changed from macro to typeclass derivation. ApplierUnapplierTest re-enabled per fork commit 7085bd8f. |
    ```
    Backlog: remove ApplierUnapplier rows.

    Commit: `docs(migration): record ApplierUnapplier Mirror-based port`.
  </action>
  <verify>
    <automated>grep -q 'slice 5.3' MIGRATION.md &amp;&amp; grep -q 'ApplierUnapplier' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'slice 5.3' MIGRATION.md` exits 0
    - `grep -q 'ApplierUnapplier' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record ApplierUnapplier Mirror-based port$`
  </acceptance_criteria>
  <done>MIGRATION.md updated; third atomic commit on branch.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-03-applier-unapplier` with 3 commits (feat + test + docs). APPLIERUNAPPLIER-01 satisfied via Mirror-based derivation.</what-built>
  <how-to-verify>
    Pre-push:
    1. `git log --oneline 04-05-meta-annotations..HEAD | wc -l` == 3
    2. `sbt 'commons-core/compile ;commons-core/testOnly *.ApplierUnapplierTest ;scalafmtCheckAll'` exit 0
    3. `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`

    Then:
    ```
    git push -u origin 05-03-applier-unapplier
    gh pr create --draft --base 04-05-meta-annotations --head halotukozak:05-03-applier-unapplier \
      --title "[Scala 3] port ApplierUnapplier (Mirror-based)" \
      --body "Slice 5.3 / Parallel — independent (no MiscMacros dep) / Depends on: none / Base branch: 04-05-meta-annotations / Requirement: APPLIERUNAPPLIER-01 / Note: reshape macro→Mirror.ProductOf-based derivation; preserved public traits; test re-enabled per fork 7085bd8f"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- ApplierUnapplier.scala matches fork (Mirror-based)
- ApplierUnapplierTest green
- MIGRATION.md updated
- Draft PR, prefix, milestone 1
</verification>

<success_criteria>
- APPLIERUNAPPLIER-01 satisfied — runtime `Applier[Foo].apply(...)` reconstructs case class via Mirror
- 3 atomic commits per CONTEXT cadence
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-03-SUMMARY.md`
</output>
