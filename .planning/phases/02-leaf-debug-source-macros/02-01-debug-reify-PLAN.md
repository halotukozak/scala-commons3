---
phase: 02-leaf-debug-source-macros
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
  - core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala
  - core/src/test/scala/com/avsystem/commons/SharedExtensionsShowTest.scala
  - MIGRATION.md
autonomous: false
requirements: [DEBUG-01, DEBUG-02]

must_haves:
  truths:
    - "`UniversalOps#showAst` / `showRawAst` / `showSymbol` / `showSymbolFullName` / `showType` / `showRawType` / `showTypeSymbol` / `showTypeSymbolFullName` compile and at runtime return the receiver unchanged"
    - "`sourceCode` returns the literal source text of the receiver expression"
    - "`withSourceCode` returns `(receiver, sourceText)`"
    - "Compile-time call to a `show*` macro emits a `report.info` message (not a halt)"
    - "MIGRATION.md backlog no longer lists SharedExtensions.scala:129..147"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala"
      provides: "Scala 3 quoted impls for the 10 debug/reify macros"
      contains: "object ShowMacros"
    - path: "core/src/main/scala/com/avsystem/commons/SharedExtensions.scala"
      provides: "inline def show* / sourceCode / withSourceCode wrappers"
      contains: "inline def showAst"
    - path: "core/src/test/scala/com/avsystem/commons/SharedExtensionsShowTest.scala"
      provides: "Smoke test for show* family + sourceCode + withSourceCode"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/SharedExtensions.scala"
      to: "core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala"
      via: "${ macros.ShowMacros.<name>Impl[A]('a) }"
      pattern: "ShowMacros\\.show"
---

<objective>
Restore the 10 debug/reify macros on `UniversalOps[A]` in `SharedExtensions.scala` (`showAst`, `showRawAst`, `showSymbol`, `showSymbolFullName`, `showType`, `showRawType`, `showTypeSymbol`, `showTypeSymbolFullName`, `sourceCode`, `withSourceCode`) using Scala 3 `inline def` + `scala.quoted` macros.

Purpose: First "real" Scala 3 quoted-macro slice. Validates the per-slice PR pattern for Phase 2+ restoration.
Output: Working show* family + matching smoke test + MIGRATION.md backlog rows removed + draft PR opened.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/phases/02-leaf-debug-source-macros/02-CONTEXT.md
@.planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md
@.planning/phases/02-leaf-debug-source-macros/02-VALIDATION.md
@MIGRATION.md

<interfaces>
<!-- Receiver pattern: macros live inside `class UniversalOps[A](private val a: A) extends AnyVal` -->
<!-- Scala 3 idiom: pass the receiver as Expr[A] via '{a} splice -->

In core/src/main/scala/com/avsystem/commons/SharedExtensions.scala (current stubs, lines 129-147):
```scala
// inside class UniversalOps[A](private val a: A) extends AnyVal { ... }
def showAst: A = ???                  // line 129 (preceded by TODO[scala3-port])
def showRawAst: A = ???               // line 131
def showSymbol: A = ???               // line 133
def showSymbolFullName: A = ???       // line 135
def showType: A = ???                 // line 137
def showRawType: A = ???              // line 139
def showTypeSymbol: A = ???           // line 141
def showTypeSymbolFullName: A = ???   // line 143
def sourceCode: String = ???          // line 145
def withSourceCode: (A, String) = ??? // line 147
```
</interfaces>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Implement ShowMacros + replace stubs in SharedExtensions</name>
  <files>core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala, core/src/main/scala/com/avsystem/commons/SharedExtensions.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/SharedExtensions.scala (lines 120-160 — current stubs in `UniversalOps[A]`)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Slice 1: debug-reify" — full Scala 3 skeleton at lines 234-310 in RESEARCH.md)
    - .planning/phases/02-leaf-debug-source-macros/02-VALIDATION.md (§ "Observable Signals")
    - Cribbing source (fork master — working Scala 3 draft impl):
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala` (Scala 3 port of show*/sourceCode/withSourceCode — translate directly)
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala` (impl helpers if used)
      - Test reference: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/SharedExtensionsTest.scala`
    - MIGRATION.md (§6 Backlog rows 129-147 for SharedExtensions.scala)
  </read_first>
  <behavior>
    - Test 1: `42.showAst` at runtime returns `42` (receiver pass-through).
    - Test 2: `"hello".sourceCode` at runtime returns the literal string `"\"hello\""` (i.e. `Expr(Position.sourceCode)`).
    - Test 3: `(1 + 2).withSourceCode` returns `(3, "1 + 2")`.
    - Test 4: A `show*` macro emits a `report.info` (compile completes; no abort).
    - Test 5: `case class Foo(x: Int); Foo(7).showType` runtime returns `Foo(7)`; compile-time prints "Foo" via info channel.
  </behavior>
  <action>
    Step A — Create `core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala`. Package `com.avsystem.commons.macros`. Verbatim skeleton from RESEARCH.md §"Slice 1" (lines 239-295 in RESEARCH.md). Exactly 10 `def *Impl[A: Type](a: Expr[A])(using Quotes): Expr[A]` methods plus `sourceCodeImpl` (returns `Expr[String]`) and `withSourceCodeImpl` (returns `Expr[(A, String)]`).

    Use `report.info` (NOT `report.error`) for the 8 `show*` reporters — per orchestrator directive open question #3. Add a one-line comment `// report.info: print + proceed (Scala 2 used c.error as a hack — Scala 3 has a proper info channel)` above the first `report.info` call.

    For `sourceCodeImpl`: use `a.asTerm.pos.sourceCode.getOrElse(report.errorAndAbort("source code unavailable at this position", a.asTerm.pos))` and wrap with `Expr(...)`.

    For `withSourceCodeImpl`: implement as `'{ ($a, ${ sourceCodeImpl[A](a) }) }`.

    Step B — Modify `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala`. Inside `class UniversalOps[A](private val a: A) extends AnyVal`, replace the 10 `// TODO[scala3-port]: ...` + `def <name>: T = ???` blocks at lines 128-147 with:

    ```scala
    inline def showAst: A                  = ${ macros.ShowMacros.showAstImpl[A]('a) }
    inline def showRawAst: A               = ${ macros.ShowMacros.showRawAstImpl[A]('a) }
    inline def showSymbol: A               = ${ macros.ShowMacros.showSymbolImpl[A]('a) }
    inline def showSymbolFullName: A       = ${ macros.ShowMacros.showSymbolFullNameImpl[A]('a) }
    inline def showType: A                 = ${ macros.ShowMacros.showTypeImpl[A]('a) }
    inline def showRawType: A              = ${ macros.ShowMacros.showRawTypeImpl[A]('a) }
    inline def showTypeSymbol: A           = ${ macros.ShowMacros.showTypeSymbolImpl[A]('a) }
    inline def showTypeSymbolFullName: A   = ${ macros.ShowMacros.showTypeSymbolFullNameImpl[A]('a) }
    inline def sourceCode: String          = ${ macros.ShowMacros.sourceCodeImpl[A]('a) }
    inline def withSourceCode: (A, String) = ${ macros.ShowMacros.withSourceCodeImpl[A]('a) }
    ```

    Remove all 10 `// TODO[scala3-port]: ...` lines (drop the tags entirely; restoration = removal).

    Step C — Verify compile + scalafmt.

    Commit message: `feat(core): restore show*/sourceCode debug macros via Scala 3 quotes`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/compile' && grep -q 'inline def showAst' core/src/main/scala/com/avsystem/commons/SharedExtensions.scala && grep -q 'def showAstImpl\[A: Type\]' core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala && ! grep -nE 'TODO\[scala3-port\]: show|TODO\[scala3-port\]: sourceCode|TODO\[scala3-port\]: withSourceCode' core/src/main/scala/com/avsystem/commons/SharedExtensions.scala && sbt -batch 'scalafmtCheckAll'</automated>
  </verify>
  <done>10 `inline def` wrappers present in SharedExtensions.scala; `ShowMacros` impl file exists with all 10 `*Impl` methods; commons-core/compile exit 0; zero TODO[scala3-port] tags remain on the show*/sourceCode lines; scalafmtCheckAll exit 0.</done>
</task>

<task type="auto" tdd="true">
  <name>Task 2: Add smoke test for show* + sourceCode + withSourceCode</name>
  <files>core/src/test/scala/com/avsystem/commons/SharedExtensionsShowTest.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/SharedExtensions.scala (post-Task-1 inline defs)
    - core/src/test/scala/com/avsystem/commons/SharedExtensionsTest.scala (if exists — match style; otherwise model on any sibling test in `core/src/test/scala/com/avsystem/commons/`)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Wave 0 Gaps" — debug-reify smoke shape)
    - .planning/phases/02-leaf-debug-source-macros/02-VALIDATION.md (§ "Observable Signals" debug-reify row)
  </read_first>
  <behavior>
    - showAst: assert `42.showAst == 42`.
    - showType: assert `"x".showType == "x"`.
    - sourceCode: assert `(1 + 2).sourceCode == "1 + 2"`.
    - withSourceCode: assert `(1 + 2).withSourceCode == (3, "1 + 2")`.
    - All 8 show* methods invoked at least once for pass-through coverage (single `*Test.scala` class with one ScalaTest fun-suite block).
  </behavior>
  <action>
    Create `core/src/test/scala/com/avsystem/commons/SharedExtensionsShowTest.scala`. Use ScalaTest `AnyFunSuite` (the dominant style in this repo — verify by skimming any existing test). Single test class `SharedExtensionsShowTest`. One test block per behavior listed above. No external fixtures.

    Verify the test file compiles AND passes:
    ```
    sbt -batch 'commons-core/Test/compile' && sbt -batch 'commons-core/testOnly com.avsystem.commons.SharedExtensionsShowTest'
    ```

    Commit message: `test(core): add smoke test for show*/sourceCode debug macros`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/testOnly com.avsystem.commons.SharedExtensionsShowTest'</automated>
  </verify>
  <done>Test file exists, compiles, passes; SharedExtensionsShowTest reports 4+ green tests; commons-core/Test/compile exit 0.</done>
</task>

<task type="auto">
  <name>Task 3: Remove restored backlog rows from MIGRATION.md</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (full file — particularly §6 Backlog header line "Total tags: 155" and rows for SharedExtensions.scala:129..147)
    - Output of `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` (post-Task-1 — should be 145, i.e. 155 - 10)
  </read_first>
  <action>
    Delete the 10 backlog rows in MIGRATION.md (§ Backlog table) for:
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:129` (showAst)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:131` (showRawAst)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:133` (showSymbol)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:135` (showSymbolFullName)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:137` (showType)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:139` (showRawType)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:141` (showTypeSymbol)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:143` (showTypeSymbolFullName)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:145` (sourceCode)
    - `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:147` (withSourceCode)

    Update the "Total tags: N" line in the §6 Backlog header to match the new TAG count from `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` (expected 145).

    Commit message: `docs(migration): remove restored show*/sourceCode backlog entries`
  </action>
  <verify>
    <automated>! grep -nE 'SharedExtensions\.scala:(129|131|133|135|137|139|141|143|145|147)' MIGRATION.md && grep -qE 'Total tags: [0-9]+' MIGRATION.md</automated>
  </verify>
  <done>10 backlog rows removed; Total tags count reflects current `git grep -c` for `TODO[scala3-port]`; MIGRATION.md still parses as valid markdown.</done>
</task>

<task type="checkpoint:human-action" gate="blocking">
  <name>Task 4: Push branch + open draft PR</name>
  <what-built>Completed slice: ShowMacros + SharedExtensions inline defs + smoke test + MIGRATION.md trim. Three commits on a new branch off `01-big-bang`.</what-built>
  <action>
    Run these exact commands (in order). User must ack between push and PR open per WORKFLOW-03.

    ```bash
    # 1) Create branch off 01-big-bang (NOT off scala-3 directly; PR #860 is the in-flight base)
    git checkout -b 02-debug-reify 01-big-bang
    # cherry-pick or rebase: branch should already point at Task 1/2/3 commits if they were authored on 02-debug-reify

    # 2) Verify full local gate before push
    sbt -batch 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll ;scalafmtSbtCheck'

    # 3) Push to fork
    git push -u origin 02-debug-reify

    # 4) Open draft PR against AVSystem/scala-commons base scala-3 (or 01-big-bang if #860 not yet merged)
    PR_URL=$(gh pr create --draft \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:02-debug-reify \
      --title "[Scala 3] Phase 02 debug-reify: restore show*/sourceCode macros" \
      --body "Restores the 10 SharedExtensions debug/reify macros (showAst, showRawAst, showSymbol, showSymbolFullName, showType, showRawType, showTypeSymbol, showTypeSymbolFullName, sourceCode, withSourceCode) via Scala 3 inline + scala.quoted. show* macros now use report.info (was c.error in Scala 2 — info is the proper channel). Removes 10 backlog rows from MIGRATION.md.")
    echo "PR: $PR_URL"

    # 5) Extract PR number and assign milestone 1 (memory rule — gh pr edit -m fails on classic Projects)
    PR_NUM=$(echo "$PR_URL" | grep -oE '[0-9]+$')
    gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
    ```

    Expected:
    - Push exits 0.
    - PR created in DRAFT state, base `scala-3`, title prefixed `[Scala 3]`, milestone "Scala 3" (#1).
    - CI starts on fork + PR.
  </action>
  <resume-signal>Type "approved" once PR is open + milestone assigned + CI started, or describe blockers.</resume-signal>
</task>

</tasks>

<verification>
- `sbt -batch 'commons-core/compile ;commons-core/Test/compile'` exit 0.
- `sbt -batch 'commons-core/testOnly com.avsystem.commons.SharedExtensionsShowTest'` exit 0.
- `sbt -batch 'scalafmtCheckAll ;scalafmtSbtCheck'` exit 0.
- `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` decreased by 10 (155 → 145) vs `01-big-bang` tip.
- PR draft open at AVSystem/scala-commons, milestone 1, title prefix `[Scala 3]`.
</verification>

<success_criteria>
- All 10 SharedExtensions debug macros are working `inline def` wrappers around `ShowMacros.*Impl` quoted impls.
- Smoke test passes (4+ assertions).
- MIGRATION.md backlog reflects removal.
- Draft PR is open with correct title prefix + milestone.
- No new `@nowarn` / `-Wconf` / GSD nomenclature.
- No `.planning/` paths in any commit.
</success_criteria>

<output>
After completion, create `.planning/phases/02-leaf-debug-source-macros/02-01-SUMMARY.md` including:
- PR URL + number + draft status
- 3 commit hashes (feat / test / docs)
- Removed MIGRATION.md row count (10)
- Post-slice `git grep -c 'TODO[scala3-port]' -- '*.scala'` count
- Any deviations from plan
</output>
