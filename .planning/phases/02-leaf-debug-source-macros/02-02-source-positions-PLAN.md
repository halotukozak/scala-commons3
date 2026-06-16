---
phase: 02-leaf-debug-source-macros
plan: 02
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/annotation/positioned.scala
  - core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala
  - core/src/main/scala/com/avsystem/commons/annotation/macros/PositionedMacros.scala
  - core/src/main/scala/com/avsystem/commons/misc/macros/SourceInfoMacros.scala
  - core/src/test/scala/com/avsystem/commons/annotation/PositionedTest.scala
  - core/src/test/scala/com/avsystem/commons/misc/SourceInfoTest.scala
  - MIGRATION.md
autonomous: false
requirements: [POS-01, POS-02]

must_haves:
  truths:
    - "`positioned.here` returns a positive `Int` source offset of the call site"
    - "Two adjacent `positioned.here` calls yield distinct ints"
    - "`SourceInfo.here` resolves implicitly; the resulting record has line > 0, non-empty filePath, non-empty lineContent"
    - "`summon[SourceInfo]` succeeds at any call site"
    - "MIGRATION.md backlog no longer lists positioned.scala:12 or SourceInfo.scala:28"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/annotation/macros/PositionedMacros.scala"
      provides: "Scala 3 quoted impl for positioned.here"
      contains: "object PositionedMacros"
    - path: "core/src/main/scala/com/avsystem/commons/misc/macros/SourceInfoMacros.scala"
      provides: "Scala 3 quoted impl for SourceInfo.here"
      contains: "object SourceInfoMacros"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/annotation/positioned.scala"
      to: "core/src/main/scala/com/avsystem/commons/annotation/macros/PositionedMacros.scala"
      via: "${ PositionedMacros.posPointImpl }"
      pattern: "PositionedMacros\\.posPointImpl"
    - from: "core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala"
      to: "core/src/main/scala/com/avsystem/commons/misc/macros/SourceInfoMacros.scala"
      via: "${ SourceInfoMacros.sourceInfoImpl }"
      pattern: "SourceInfoMacros\\.sourceInfoImpl"
---

<objective>
Restore `annotation.positioned.here: Int` (call-site offset) and `misc.SourceInfo.here: SourceInfo` (implicit source-info record). Both use `quotes.reflect.Position.ofMacroExpansion`.

Purpose: Position macros are leaf — no dependencies on other Phase 2 slices. Validates `Position.ofMacroExpansion` + symbol owner-chain walk.
Output: Working `positioned.here` + `SourceInfo.here` + smoke tests + MIGRATION.md trim + draft PR.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/phases/02-leaf-debug-source-macros/02-CONTEXT.md
@.planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md
@.planning/phases/02-leaf-debug-source-macros/02-VALIDATION.md
@MIGRATION.md
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Implement PositionedMacros + SourceInfoMacros + replace stubs</name>
  <files>core/src/main/scala/com/avsystem/commons/annotation/positioned.scala, core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala, core/src/main/scala/com/avsystem/commons/annotation/macros/PositionedMacros.scala, core/src/main/scala/com/avsystem/commons/misc/macros/SourceInfoMacros.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/annotation/positioned.scala (stub at line 12)
    - core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala (stub at line 28)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Slice 2: source-positions")
    - Cribbing source (fork master — working Scala 3 draft impls):
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/annotation/positioned.scala`
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SourceInfo.scala`
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala` (impl helpers if used)
      - Test reference: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/SourceInfoTest.scala`
    - MIGRATION.md (§6 Backlog rows for positioned.scala:12, SourceInfo.scala:28)
  </read_first>
  <behavior>
    - `positioned.here` returns `Position.ofMacroExpansion.start: Int` at the call site.
    - `SourceInfo.here` returns populated SourceInfo with line >= 1, nonEmpty filePath, nonEmpty lineContent.
    - Two `positioned.here` invocations on adjacent lines yield strictly different Int offsets.
  </behavior>
  <action>
    Step A — Create `core/src/main/scala/com/avsystem/commons/annotation/macros/PositionedMacros.scala`:

    ```scala
    package com.avsystem.commons.annotation.macros

    import scala.quoted.*

    object PositionedMacros:
      def posPointImpl(using Quotes): Expr[Int] =
        import quotes.reflect.*
        Expr(Position.ofMacroExpansion.start)
    ```

    Step B — In `positioned.scala`, replace the TODO + `def here: Int = ???` with:

    ```scala
      inline def here: Int = ${ macros.PositionedMacros.posPointImpl }
    ```

    Step C — Create `core/src/main/scala/com/avsystem/commons/misc/macros/SourceInfoMacros.scala`. Use RESEARCH.md §"Slice 2" skeleton verbatim. Key points: `line = p.startLine + 1` (1-based), `column = p.startColumn + 1`, enclosing chain via `LazyList.iterate(Symbol.spliceOwner)(_.owner).takeWhile(s => s != Symbol.noSymbol && !s.isPackageDef).map(_.name).toList`. Splice all 7 fields and the enclosing list via `${ Expr(v) }` into a single `'{ SourceInfo(...) }` quote.

    Step D — In `SourceInfo.scala`, replace the TODO + `implicit def here: SourceInfo = ???` with:

    ```scala
      inline implicit def here: SourceInfo = ${ macros.SourceInfoMacros.sourceInfoImpl }
    ```

    (Preserve `implicit def` form per Pitfall #4 — minimum-diff for downstream summons.)

    Step E — Verify compile + scalafmt.

    Commit message: `feat(core): restore positioned.here and SourceInfo.here via Scala 3 quotes`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/compile' && grep -q 'inline def here' core/src/main/scala/com/avsystem/commons/annotation/positioned.scala && grep -q 'inline implicit def here' core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala && sbt -batch 'scalafmtCheckAll'</automated>
  </verify>
  <done>Both inline defs present; both Macros objects exist with quoted impl; commons-core/compile exit 0; TODO tags removed from positioned.scala and SourceInfo.scala; scalafmtCheckAll exit 0.</done>
</task>

<task type="auto" tdd="true">
  <name>Task 2: Add smoke tests for positioned.here and SourceInfo.here</name>
  <files>core/src/test/scala/com/avsystem/commons/annotation/PositionedTest.scala, core/src/test/scala/com/avsystem/commons/misc/SourceInfoTest.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/annotation/positioned.scala (post-Task-1)
    - core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala (post-Task-1)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Wave 0 Gaps" — source-positions smoke shape)
    - Any existing test in `core/src/test/scala/com/avsystem/commons/` to mirror ScalaTest style
  </read_first>
  <behavior>
    - PositionedTest: two adjacent `positioned.here` calls yield two distinct positive ints.
    - SourceInfoTest: `val si: SourceInfo = summon[SourceInfo]`; assert `si.line > 0`, `si.filePath endsWith "SourceInfoTest.scala"`, `si.lineContent.contains("summon")` (or `here`).
  </behavior>
  <action>
    Create `core/src/test/scala/com/avsystem/commons/annotation/PositionedTest.scala` — ScalaTest `AnyFunSuite`. Single test "positioned.here yields distinct offsets at distinct call sites":
    ```scala
    val a = positioned.here
    val b = positioned.here
    assert(a > 0 && b > 0 && a != b)
    ```

    Create `core/src/test/scala/com/avsystem/commons/misc/SourceInfoTest.scala` — ScalaTest `AnyFunSuite`. Single test "SourceInfo.here populates fields":
    ```scala
    val si: SourceInfo = summon[SourceInfo]
    assert(si.line > 0)
    assert(si.filePath.endsWith("SourceInfoTest.scala"))
    assert(si.fileName == "SourceInfoTest.scala")
    assert(si.lineContent.nonEmpty)
    ```

    Run both tests; both must pass.

    Commit message: `test(core): add smoke tests for positioned.here and SourceInfo.here`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/testOnly com.avsystem.commons.annotation.PositionedTest com.avsystem.commons.misc.SourceInfoTest'</automated>
  </verify>
  <done>Both test files compile and pass; commons-core/Test/compile exit 0.</done>
</task>

<task type="auto">
  <name>Task 3: Remove restored backlog rows from MIGRATION.md</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (§6 Backlog rows for positioned.scala:12, SourceInfo.scala:28; "Total tags: N" header)
    - Output of `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` post-Task-1
  </read_first>
  <action>
    Delete the 2 backlog rows in MIGRATION.md (§ Backlog) for:
    - `core/src/main/scala/com/avsystem/commons/annotation/positioned.scala:12` (here)
    - `core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala:28` (SourceInfo.here)

    Update the "Total tags: N" line to match `git grep -c 'TODO\[scala3-port\]' -- '*.scala'`.

    Commit message: `docs(migration): remove restored source-position backlog entries`
  </action>
  <verify>
    <automated>! grep -nE 'positioned\.scala:12|SourceInfo\.scala:28' MIGRATION.md</automated>
  </verify>
  <done>2 backlog rows removed; Total tags line updated; MIGRATION.md still valid markdown.</done>
</task>

<task type="checkpoint:human-action" gate="blocking">
  <name>Task 4: Push branch + open draft PR</name>
  <what-built>source-positions slice: PositionedMacros + SourceInfoMacros + inline defs + 2 smoke tests + MIGRATION.md trim.</what-built>
  <action>
    ```bash
    git checkout -b 02-source-positions 01-big-bang
    # ensure Task 1/2/3 commits are on this branch
    sbt -batch 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll ;scalafmtSbtCheck'
    git push -u origin 02-source-positions

    PR_URL=$(gh pr create --draft \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:02-source-positions \
      --title "[Scala 3] Phase 02 source-positions: restore positioned.here + SourceInfo.here" \
      --body "Restores positioned.here (Int call-site offset) and SourceInfo.here (implicit SourceInfo) via Scala 3 inline + scala.quoted (Position.ofMacroExpansion + symbol owner-chain walk via Symbol.spliceOwner). Removes 2 backlog rows from MIGRATION.md.")
    echo "PR: $PR_URL"
    PR_NUM=$(echo "$PR_URL" | grep -oE '[0-9]+$')
    gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
    ```

    Expected: push OK; PR draft, title prefixed `[Scala 3]`, base `scala-3`, milestone "Scala 3" (#1).
  </action>
  <resume-signal>Type "approved" once PR draft is open + milestone assigned, or describe blockers.</resume-signal>
</task>

</tasks>

<verification>
- `sbt -batch 'commons-core/compile ;commons-core/Test/compile'` exit 0.
- `sbt -batch 'commons-core/testOnly com.avsystem.commons.annotation.PositionedTest com.avsystem.commons.misc.SourceInfoTest'` exit 0.
- `sbt -batch 'scalafmtCheckAll'` exit 0.
- `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` decreased by 2 vs `01-big-bang` tip.
- PR draft, milestone 1, title prefix `[Scala 3]`.
</verification>

<success_criteria>
- `positioned.here` + `SourceInfo.here` both quoted-macro-backed.
- Smoke tests pass.
- MIGRATION.md backlog reflects removal.
- Draft PR open.
- No new `@nowarn` / `-Wconf` / GSD nomenclature / `.planning/` paths.
</success_criteria>

<output>
After completion, create `.planning/phases/02-leaf-debug-source-macros/02-02-SUMMARY.md` with: PR URL, 3 commit hashes, removed MIGRATION row count (2), post-slice `git grep -c 'TODO[scala3-port]'` count, any deviations.
</output>
