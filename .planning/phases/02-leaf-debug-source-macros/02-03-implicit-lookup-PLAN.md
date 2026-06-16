---
phase: 02-leaf-debug-source-macros
plan: 03
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/Implicits.scala
  - core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala
  - core/src/test/scala/com/avsystem/commons/misc/ImplicitsTest.scala
  - MIGRATION.md
autonomous: false
requirements: [IMPL-01]

must_haves:
  truths:
    - "`Implicits.infer[T]` succeeds when an implicit `T` exists; returns the summoned value"
    - "`Implicits.infer[T](clue)` succeeds when an implicit exists; fails compile with message containing `clue` when absent"
    - "`Implicits.inferNonMacro[T](clue)` is preserved as an alias with narrowed semantics (collapses to `infer(clue)` — Scala 3 has no `withMacrosDisabled` equivalent on `Expr.summon`)"
    - "MIGRATION.md backlog no longer lists Implicits.scala:5..9"
    - "MIGRATION.md §3 (Source-compat breaks) documents the `inferNonMacro` semantic narrowing"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala"
      provides: "Scala 3 quoted impl for infer family"
      contains: "object ImplicitsMacros"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/misc/Implicits.scala"
      to: "core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala"
      via: "${ ImplicitsMacros.inferImpl[T]('clue) }"
      pattern: "ImplicitsMacros\\.inferImpl"
---

<objective>
Restore `misc.Implicits.infer[T]` / `infer[T](clue)` / `inferNonMacro[T](clue)` using `Expr.summon[T]` + `report.errorAndAbort`.

Purpose: Validates `Expr.summon` usage + compile-time error reporting + the "narrowed-semantics alias" documentation pattern in MIGRATION.md.
Output: Working `infer` family + ImplicitsTest (positive + negative) + MIGRATION.md trim + §3 narrowing entry + draft PR.
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
  <name>Task 1: Implement ImplicitsMacros + replace stubs in Implicits.scala</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/Implicits.scala, core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/misc/Implicits.scala (current 3 stubs at lines 5/7/9)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Slice 3: implicit-lookup" — full skeleton)
    - Cribbing source (fork master — working Scala 3 draft impl):
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Implicits.scala` (Scala 3 port of infer family)
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala` (impl helpers if used)
    - MIGRATION.md (§6 Backlog rows for Implicits.scala:5..9; §3 for the narrowing entry)
  </read_first>
  <behavior>
    - `Implicits.infer[Ordering[Int]]` returns `summon[Ordering[Int]]` (positive — implicit exists).
    - `Implicits.infer[NotProvided]("clue text")` produces a compile error containing "clue text" (negative).
    - `Implicits.inferNonMacro[Ordering[Int]]("any")` returns `summon[Ordering[Int]]` (alias, same behaviour as infer(clue)).
    - All three forms type-check.
  </behavior>
  <action>
    Step A — Create `core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala`:

    ```scala
    package com.avsystem.commons.misc.macros

    import scala.quoted.*

    object ImplicitsMacros:
      def inferImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] =
        import quotes.reflect.*
        Expr.summon[T] match
          case Some(e) => e
          case None =>
            val clueStr = clue.value.getOrElse("")
            val prefix = if clueStr.nonEmpty then s"$clueStr: " else ""
            report.errorAndAbort(s"${prefix}could not find implicit value for ${TypeRepr.of[T].show}")
    ```

    Step B — Replace the entire body of `object Implicits` in `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala` with:

    ```scala
    object Implicits {
      inline def infer[T]: T = ${ macros.ImplicitsMacros.inferImpl[T]('{ "" }) }
      inline def infer[T](clue: String): T = ${ macros.ImplicitsMacros.inferImpl[T]('clue) }
      // inferNonMacro: Scala 3 `Expr.summon` has no `withMacrosDisabled` flag.
      // Preserved as alias for source-compat; semantically equivalent to `infer(clue)`.
      // See MIGRATION.md §3 for the narrowing.
      inline def inferNonMacro[T](clue: String): T = ${ macros.ImplicitsMacros.inferImpl[T]('clue) }
    }
    ```

    Drop the 3 `// TODO[scala3-port]: ...` lines.

    Step C — Verify compile + scalafmt.

    Commit message: `feat(core): restore Implicits.infer family via Expr.summon`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/compile' && grep -q 'inline def infer' core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && grep -q 'inline def inferNonMacro' core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && grep -q 'def inferImpl' core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala && sbt -batch 'scalafmtCheckAll'</automated>
  </verify>
  <done>All 3 inline defs present; ImplicitsMacros exists; compile exit 0; TODO tags removed; scalafmtCheckAll exit 0.</done>
</task>

<task type="auto" tdd="true">
  <name>Task 2: Add positive + negative tests for infer family</name>
  <files>core/src/test/scala/com/avsystem/commons/misc/ImplicitsTest.scala</files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/misc/Implicits.scala (post-Task-1)
    - .planning/phases/02-leaf-debug-source-macros/02-RESEARCH.md (§ "Wave 0 Gaps" implicit-lookup row)
    - Any existing test using `compiletime.testing.typeCheckErrors` or `scalatest`'s `assertDoesNotCompile` — search via `grep -rn 'assertDoesNotCompile\|typeCheckErrors' core/src/test/scala 2>/dev/null`
  </read_first>
  <behavior>
    - Positive: `Implicits.infer[Ordering[Int]]` returns a non-null Ordering[Int].
    - Positive: given a `given Foo` in test scope, `Implicits.infer[Foo]("clue")` returns that instance.
    - Negative: `Implicits.infer[NoSuchType]("special-clue-text")` fails to compile with message containing `special-clue-text`. Use `scalatest` `assertDoesNotCompile` (the canonical project pattern — also works for Scala 3) or `compiletime.testing.typeCheckErrors` if available.
    - Positive: `Implicits.inferNonMacro[Ordering[Int]]("any")` behaves identically to `infer(clue)`.
  </behavior>
  <action>
    Create `core/src/test/scala/com/avsystem/commons/misc/ImplicitsTest.scala`. ScalaTest `AnyFunSuite`. Define a local type `case class Foo()` with no implicit instance for the negative test. Use `assertDoesNotCompile("""Implicits.infer[Foo]("special-clue-text")""")` — confirms compile failure. If asserting on the error MESSAGE content is needed, fall back to `assertCompiles("""object _x { Implicits.infer[Ordering[Int]] }""")` for positive and `assertDoesNotCompile` for negative (scalatest's `assertDoesNotCompile` does not expose the message — accept this limitation; the compile-failure signal alone is the contract).

    Skeleton:
    ```scala
    package com.avsystem.commons.misc

    import com.avsystem.commons.misc.Implicits
    import org.scalatest.funsuite.AnyFunSuite

    class ImplicitsTest extends AnyFunSuite:
      class NoImplicit

      test("infer[Ordering[Int]] resolves"):
        assert(Implicits.infer[Ordering[Int]] eq summon[Ordering[Int]])

      test("infer(clue) resolves when implicit exists"):
        assert(Implicits.infer[Ordering[Int]]("ord clue") eq summon[Ordering[Int]])

      test("inferNonMacro behaves identically to infer(clue)"):
        assert(Implicits.inferNonMacro[Ordering[Int]]("any") eq summon[Ordering[Int]])

      test("infer fails to compile when no implicit"):
        assertDoesNotCompile("""com.avsystem.commons.misc.Implicits.infer[ImplicitsTest#NoImplicit]("special-clue-text")""")
    ```

    Run; tests must pass.

    Commit message: `test(core): add positive + negative tests for Implicits.infer family`
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/testOnly com.avsystem.commons.misc.ImplicitsTest'</automated>
  </verify>
  <done>4 ScalaTest cases pass; file compiles; Test/compile exit 0.</done>
</task>

<task type="auto">
  <name>Task 3: Remove backlog rows + add §3 narrowing entry to MIGRATION.md</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (§3 Source-compat breaks → core subsection; §6 Backlog rows for Implicits.scala:5/7/9; "Total tags: N" header)
  </read_first>
  <action>
    Step A — Delete the 3 backlog rows in §6 Backlog for:
    - `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:5` (infer)
    - `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:7` (infer(clue))
    - `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:9` (inferNonMacro)

    Step B — Update the "Total tags: N" line in §6 header.

    Step C — Add a new bullet to §3 (Source-compat breaks) → core subsection:

    > - `Implicits.inferNonMacro[T](clue)` is preserved as an alias of `Implicits.infer[T](clue)` — Scala 3's `Expr.summon[T]` has no `withMacrosDisabled` equivalent, so the "skip macro implicits" semantic of the Scala 2 impl no longer applies. Downstream callers relying on the macro-disabled lookup must implement it manually.

    Commit message: `docs(migration): remove restored infer backlog + document inferNonMacro narrowing`
  </action>
  <verify>
    <automated>! grep -nE 'Implicits\.scala:(5|7|9)' MIGRATION.md && grep -q 'inferNonMacro' MIGRATION.md</automated>
  </verify>
  <done>3 backlog rows removed; §3 narrowing bullet present; Total tags updated.</done>
</task>

<task type="checkpoint:human-action" gate="blocking">
  <name>Task 4: Push branch + open draft PR</name>
  <what-built>implicit-lookup slice: ImplicitsMacros + 3 inline defs + 4 ScalaTest cases (positive + negative) + MIGRATION.md backlog trim + §3 narrowing entry.</what-built>
  <action>
    ```bash
    git checkout -b 02-implicit-lookup 01-big-bang
    sbt -batch 'commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll ;scalafmtSbtCheck'
    git push -u origin 02-implicit-lookup

    PR_URL=$(gh pr create --draft \
      --repo AVSystem/scala-commons \
      --base scala-3 \
      --head halotukozak:02-implicit-lookup \
      --title "[Scala 3] Phase 02 implicit-lookup: restore Implicits.infer family" \
      --body "Restores Implicits.infer[T] / infer[T](clue) / inferNonMacro[T](clue) using Expr.summon + report.errorAndAbort. inferNonMacro semantically narrows to infer(clue) — Scala 3 Expr.summon has no withMacrosDisabled flag — documented in MIGRATION.md §3. Removes 3 backlog rows.")
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
- `sbt -batch 'commons-core/testOnly com.avsystem.commons.misc.ImplicitsTest'` exit 0.
- `sbt -batch 'scalafmtCheckAll'` exit 0.
- `git grep -c 'TODO\[scala3-port\]' -- '*.scala'` decreased by 3.
- PR draft, milestone 1, title prefix `[Scala 3]`.
</verification>

<success_criteria>
- `Implicits.infer` family is working Scala 3 macros.
- Negative test asserts compile-time failure on missing implicit.
- MIGRATION.md §3 documents the `inferNonMacro` narrowing.
- Draft PR open with correct title prefix + milestone.
- No new `@nowarn` / `-Wconf` / GSD nomenclature / `.planning/` in any commit.
</success_criteria>

<output>
After completion, create `.planning/phases/02-leaf-debug-source-macros/02-03-SUMMARY.md` with: PR URL, 3 commit hashes, removed MIGRATION row count (3), §3 narrowing entry text, post-slice TODO grep count, any deviations.
</output>
