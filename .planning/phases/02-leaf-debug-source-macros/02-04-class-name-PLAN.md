---
phase: 02-leaf-debug-source-macros
plan: 04
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala
  - core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala
  - core/src/test/scala/com/avsystem/commons/misc/SimpleClassNameTest.scala
  - MIGRATION.md
autonomous: false
requirements: [CLS-01]

must_haves:
  truths:
    - "`SimpleClassName.materialize[String]` resolves to a `SimpleClassName[String]` whose `name == \"String\"`"
    - "`SimpleClassName.of[List[Int]]` returns `\"List\"`"
    - "`SimpleClassName.of[MyCaseClass]` returns the simple class name"
    - "Materializing for a non-class type (e.g. a type alias of a primitive or a structural type) produces a compile error"
    - "MIGRATION.md backlog no longer lists SimpleClassName.scala:8"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala"
      provides: "Scala 3 quoted impl for SimpleClassName.materialize"
      contains: "object SimpleClassNameMacros"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala"
      to: "core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala"
      via: "${ SimpleClassNameMacros.materializeImpl[T] }"
      pattern: "SimpleClassNameMacros\\.materializeImpl"
---

<objective>
Restore `misc.SimpleClassName.materialize[T]` using `TypeRepr.of[T].dealias.typeSymbol.name`.

Purpose: Smallest "real" derivation-style macro. Validates `TypeRepr.of[T]` + symbol-name extraction. Unblocks Phase 3 `TypeString` restoration (deferred).
Output: Working `SimpleClassName.materialize` + smoke test + MIGRATION.md trim + draft PR.
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
  <name>Task 1: Implement SimpleClassNameMacros + replace stub</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala, core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala (current stub at line 8)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Slice 4: class-name" — full skeleton)
    - Cribbing source (fork master — working Scala 3 draft impl):
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SimpleClassName.scala`
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala` (impl helpers — `materializeSimpleClassName`)
    - MIGRATION.md (§6 Backlog row for SimpleClassName.scala:8)
  </read_first>
  <behavior>
    - `SimpleClassName.materialize[String].name == "String"`.
    - `SimpleClassName.of[List[Int]] == "List"` (uses dealias to peel `[Int]` and lift to typeSymbol).
    - `SimpleClassName.of[case class Foo]` returns `"Foo"`.
    - Compile failure for non-class types (e.g. structural type / refinement) — `sym.isClassDef` guard.
  </behavior>
  <action>
    Step A — Create `core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala`:

    ```scala
    package com.avsystem.commons.misc.macros

    import com.avsystem.commons.misc.SimpleClassName
    import scala.quoted.*

    object SimpleClassNameMacros:
      def materializeImpl[T: Type](using Quotes): Expr[SimpleClassName[T]] =
        import quotes.reflect.*
        val tpe = TypeRepr.of[T].dealias
        val sym = tpe.typeSymbol
        if !sym.isClassDef then
          report.errorAndAbort(s"${tpe.show} does not represent a regular class")
        val name = sym.name
        '{ SimpleClassName[T](${ Expr(name) }) }
    ```

    Step B — In `core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala`, replace the TODO + `implicit def materialize[T]: SimpleClassName[T] = ???` with:

    ```scala
      inline implicit def materialize[T]: SimpleClassName[T] = ${ macros.SimpleClassNameMacros.materializeImpl[T] }
    ```

    (Preserve `implicit def` form — minimum-diff for downstream summons.)

    Step C — Verify compile + scalafmt.

    Commit message: `feat(core): restore SimpleClassName.materialize via Scala 3 quotes`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/compile' && grep -q 'inline implicit def materialize' core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala && grep -q 'def materializeImpl' core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala && sbt -batch 'scalafmtCheckAll'</automated>
  </verify>
  <done>inline impl def present; macros file exists; compile exit 0; TODO tag removed; scalafmtCheckAll exit 0.</done>
</task>

<task type="auto" tdd="true">
  <name>Task 2: Add smoke test for SimpleClassName</name>
  <files>core/src/test/scala/com/avsystem/commons/misc/SimpleClassNameTest.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala (post-Task-1)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Wave 0 Gaps" class-name row)
  </read_first>
  <behavior>
    - `SimpleClassName.of[String] == "String"`.
    - `SimpleClassName.of[List[Int]] == "List"`.
    - `SimpleClassName.of[SimpleClassNameTest.Foo] == "Foo"` (nested case class).
  </behavior>
  <action>
    Create `core/src/test/scala/com/avsystem/commons/misc/SimpleClassNameTest.scala`. ScalaTest `AnyFunSuite`. Define companion `object SimpleClassNameTest { case class Foo(x: Int) }`. Tests:

    ```scala
    test("String"):  assert(SimpleClassName.of[String] == "String")
    test("List[Int]"):  assert(SimpleClassName.of[List[Int]] == "List")
    test("nested case class"):  assert(SimpleClassName.of[SimpleClassNameTest.Foo] == "Foo")
    ```

    Run; tests must pass.

    Commit message: `test(core): add smoke test for SimpleClassName.materialize`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/testOnly com.avsystem.commons.misc.SimpleClassNameTest'</automated>
  </verify>
  <done>3 tests pass; Test/compile exit 0.</done>
</task>

<task type="auto">
  <name>Task 3: Remove restored backlog row from MIGRATION.md</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (§6 Backlog row for SimpleClassName.scala:8; "Total tags: N" header)
  </read_first>
  <action>
    Delete the 1 backlog row in §6 for `core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala:8`.

    Update "Total tags: N" line.

    Commit message: `docs(migration): remove restored SimpleClassName backlog entry`
  </action>
  <verify>
    <automated>! grep -nE 'SimpleClassName\.scala:8' MIGRATION.md</automated>
  </verify>
  <done>1 row removed; Total tags updated.</done>
</task>

<task type="checkpoint:human-action" gate="blocking">
  <name>Task 4: Push branch + open draft PR</name>
  <what-built>class-name slice: SimpleClassNameMacros + inline impl def + 3 smoke tests + MIGRATION.md trim.</what-built>
  <action>
    ```bash
    git checkout -b 02-class-name 01-big-bang
    sbt -batch 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll ;scalafmtSbtCheck'
    git push -u origin 02-class-name

    PR_URL=$(gh pr create --draft \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:02-class-name \
      --title "[Scala 3] Phase 02 class-name: restore SimpleClassName.materialize" \
      --body "Restores SimpleClassName.materialize[T] via Scala 3 inline implicit def + TypeRepr.of[T].dealias.typeSymbol.name. Removes 1 backlog row from MIGRATION.md.")
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
- `sbt -batch 'commons-core/testOnly com.avsystem.commons.misc.SimpleClassNameTest'` exit 0.
- `sbt -batch 'scalafmtCheckAll'` exit 0.
- `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` decreased by 1.
- PR draft, milestone 1, title prefix `[Scala 3]`.
</verification>

<success_criteria>
- `SimpleClassName.materialize` is a working Scala 3 macro.
- Smoke tests pass.
- MIGRATION.md backlog reflects removal.
- Draft PR open.
- No new `@nowarn` / `-Wconf` / GSD nomenclature / `.planning/` paths.
</success_criteria>

<output>
After completion, create `.planning/phases/02-leaf-debug-source-macros/02-04-SUMMARY.md` with: PR URL, 3 commit hashes, removed MIGRATION row count (1), post-slice TODO grep count, any deviations.
</output>
