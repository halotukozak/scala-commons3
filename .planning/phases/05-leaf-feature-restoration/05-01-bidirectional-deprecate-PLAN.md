---
phase: 05-leaf-feature-restoration
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala
  - MIGRATION.md
autonomous: false
requirements:
  - BIDIRECTIONAL-01
must_haves:
  truths:
    - "Any caller of `Bidirectional.apply(...)` fails at COMPILE time (not runtime)"
    - "`Bidirectional` object carries `@deprecated` with fork's exact since-version + message"
    - "`commons-core/compile` is green; no `???` body remains in Bidirectional.scala"
    - "MIGRATION.md §1 (Will Not Migrate) or §6 (deprecated) records Bidirectional as deprecate-not-port"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala"
      provides: "Deprecated stub object with compiletime.error body"
      contains: "scala.compiletime.error"
    - path: "MIGRATION.md"
      provides: "Bidirectional deprecated-stub entry"
      contains: "Bidirectional"
  key_links:
    - from: "Bidirectional.apply call sites (none in tree)"
      to: "scala.compiletime.error"
      via: "inline def body"
      pattern: "scala\\.compiletime\\.error"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` verbatim from fork (17 LOC). Per CONTEXT locked decision: deprecate-over-restore per [[feedback_deprecate_over_restore]]. NO real macro impl. Body is `scala.compiletime.error(...)` — fails at COMPILE time at any call site.

Purpose: Slice 5.1 — smallest leaf. Establishes the deprecate-over-restore pattern that closes the leaf family. Per CONTEXT no test file (fork dropped `BidirectionalTest.scala` with `@TodoScala3Migration DROPPED:` block-comment).

Output:
- Updated `Bidirectional.scala` (verbatim port from fork)
- MIGRATION.md entry under §1 (Will Not Migrate) or §6 (deprecated log)
- 2 atomic commits (feat + docs) — own PR, base `04-05-meta-annotations` (NOT stacked on slice 5.0; independent)
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
<!-- Full fork content (17 LOC — copy verbatim) -->
package com.avsystem.commons
package misc

/**
 * Creates reversed partial function.
 *
 * @deprecated
 *   The Scala 2 `Bidirectional` macro has not been ported to Scala 3 and will not be. Implement a reversed partial
 *   function manually instead of relying on this stub, which always fails at use site.
 */
@deprecated("Bidirectional macro not ported to Scala 3 — write the reversed PartialFunction manually.", since = "3.0.0")
object Bidirectional {
  inline def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) =
    scala.compiletime.error(
      "com.avsystem.commons.misc.Bidirectional has not been ported to Scala 3. Write the reversed PartialFunction manually.",
    )
}
</interfaces>

<branch_strategy>
Branch off `04-05-meta-annotations @ f04cec6f`. Independent of slice 5.0.
  git fetch origin && git checkout -b 05-01-bidirectional-deprecate 04-05-meta-annotations
</branch_strategy>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Replace Bidirectional.scala with verbatim fork content</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Code Examples Ex 4, Common Pitfall 3)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md` (Bidirectional decision)
    - Current file: `core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` (read to see Phase-1 stub state)
    - Fork source (verbatim): `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala`
    - Audit callers: `git grep -n 'Bidirectional\\b' -- '*.scala'` to ensure no caller breaks (any call site becomes a compile error — that's the contract, but inventory must be known)
  </read_first>
  <action>
    Verbatim port:

    1. Cut branch: `git fetch origin upstream && git checkout -b 05-01-bidirectional-deprecate 04-05-meta-annotations`
    2. Overwrite file:
       `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala > core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala`
    3. Check for callers: `git grep -n 'Bidirectional[^a-zA-Z]' -- '*.scala'`. Expect zero non-self hits (Phase 1 commenting wrapped any callers). Document inventory in commit body.
    4. Check for test file: `ls core/src/test/scala/com/avsystem/commons/misc/BidirectionalTest.scala`. If exists (Phase 1 wrapped it but kept file): inspect — if it has any non-wrapped `Bidirectional(...)` calls, they will now break compile. Per fork's "DROPPED" pattern: wrap remaining test cases under `/* @TodoScala3Migration DROPPED: ... */` and commit as part of feat (no separate test commit since there is no test to un-wrap per CONTEXT).
    5. Run `sbt commons-core/compile` + `sbt scalafmtCheckAll`. EXPECTED green.
    6. Commit: `feat(scala-3,core): deprecate Bidirectional (compiletime.error body)` with body: "Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala. Per [[feedback_deprecate_over_restore]] — no real port; callers fail at compile time."

    Per Pitfall 3: body MUST be `scala.compiletime.error(...)`, NOT `???`. Verify via grep.
  </action>
  <verify>
    <automated>grep -q 'scala.compiletime.error' core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala &amp;&amp; ! grep -q '\\?\\?\\?' core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q '@deprecated' core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` exits 0
    - `grep -q 'since = "3.0.0"' core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` exits 0
    - `grep -q 'scala.compiletime.error' core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` (no `???` body)
    - `wc -l core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` between 15 and 20 lines
    - `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala) core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` shows zero differences (true verbatim)
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): deprecate Bidirectional`
  </acceptance_criteria>
  <done>Bidirectional.scala is verbatim fork content; one feat commit on branch.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Update MIGRATION.md (§1 Will Not Migrate + remove TODO[scala3-port] backlog entry)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md` (find §1 "Will Not Migrate" section and the backlog table; verify a `Bidirectional` row exists in backlog from Phase 1 big-bang)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md`
  </read_first>
  <action>
    Two edits in one commit:

    1. Under §1 (Will Not Migrate), append:
       ```
       | misc/Bidirectional | Scala-2 macro replaced by `@deprecated` object with `scala.compiletime.error` body. Callers fail at compile time with migration instructions. Fork commit `f5c0b17e`. |
       ```

    2. In backlog table, REMOVE any row matching `Bidirectional` (Phase-1 big-bang seeded `TODO[scala3-port]: Bidirectional` — those are now resolved per [[feedback_migration_md_contract]] + ROADMAP BACKLOG-02).

    Commit: `docs(migration): record Bidirectional deprecation` (separate atomic commit; NO squash per CONTEXT cadence).
  </action>
  <verify>
    <automated>grep -q 'misc/Bidirectional' MIGRATION.md &amp;&amp; ! grep -E '^\\| .* Bidirectional .* \\(.\\)' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'Bidirectional' MIGRATION.md` exits 0
    - `grep -c 'Bidirectional' MIGRATION.md` >= 1
    - `! grep -qE 'TODO\\[scala3-port\\].*Bidirectional' MIGRATION.md` (backlog entry removed)
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record Bidirectional deprecation$`
  </acceptance_criteria>
  <done>MIGRATION.md updated; second atomic commit on branch.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 3: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-01-bidirectional-deprecate` with 2 commits. Bidirectional is now a deprecated stub with `compiletime.error` body matching fork verbatim. BIDIRECTIONAL-01 satisfied.</what-built>
  <how-to-verify>
    Pre-push verification (per WORKFLOW-03 + global rule):
    1. `git log --oneline 04-05-meta-annotations..HEAD` shows exactly 2 commits, both Conventional Commits
    2. `git diff 04-05-meta-annotations..HEAD --stat` shows only Bidirectional.scala + MIGRATION.md
    3. `sbt 'commons-core/compile ;scalafmtCheckAll'` exit 0
    4. `! git log 04-05-meta-annotations..HEAD --pretty=%B | grep -E '(\\.planning/|GSD|get-shit-done)'`
    5. `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`
    6. `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala) core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala` shows zero diff

    Then:
    ```
    git push -u origin 05-01-bidirectional-deprecate
    gh pr create --draft --base 04-05-meta-annotations --head halotukozak:05-01-bidirectional-deprecate \
      --title "[Scala 3] deprecate Bidirectional (compiletime.error body)" \
      --body "Slice 5.1 / Parallel — independent (no MiscMacros dep) / Depends on: none / Base branch: 04-05-meta-annotations / Requirement: BIDIRECTIONAL-01"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone confirmed, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- `sbt commons-core/compile` exits 0
- Bidirectional.scala is byte-identical (modulo scalafmt) to fork master
- MIGRATION.md §1 contains Bidirectional row
- PR draft, `[Scala 3]` prefix, milestone 1
</verification>

<success_criteria>
- BIDIRECTIONAL-01 requirement satisfied (callers fail at compile time)
- Bidirectional.scala matches fork verbatim
- MIGRATION.md updated per [[feedback_migration_md_contract]]
- Draft PR open
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-01-SUMMARY.md`
</output>
