---
phase: 05-leaf-feature-restoration
plan: 05
type: execute
wave: 1
depends_on: ["05-00-miscmacros-foundation"]
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala
  - core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala
  - MIGRATION.md
autonomous: false
requirements:
  - ANNOTOF-01
must_haves:
  truths:
    - "AnnotationOf.scala has 7 case-class leaves (AnnotationOf, OptAnnotationOf, AnnotationsOf, HasAnnotation, SelfAnnotation, SelfOptAnnotation, SelfAnnotations); each companion `extends XMacros` trait from MiscMacros.scala (slice 5.0)"
    - "HasAnnotation is now `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A` (reshape from final-class form per Pitfall 4)"
    - "`AnnotationOf[awesome, Foo]` returns annotation; absent → compile fail (via report.errorAndAbort)"
    - "AnnotationOfTest un-wrapped + green"
    - "Zero callers of removed `HasAnnotation.create[A,T]` factory (verified pre-port via grep)"
    - "MIGRATION.md §3 records reshape (HasAnnotation opaque type + RefiningAnnotation bound)"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala"
      provides: "7 case-class leaves + companions extending their respective Macros traits from MiscMacros.scala"
      contains: "extends AnnotationOfMacros"
    - path: "core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala"
      provides: "Unwrapped test exercising all 7 leaves"
      contains: "AnnotationOf"
  key_links:
    - from: "AnnotationOf.scala companions"
      to: "MiscMacros.scala XMacros traits (slice 5.0)"
      via: "extends XMacros"
      pattern: "extends (AnnotationOf|OptAnnotationOf|AnnotationsOf|SelfAnnotation|SelfOptAnnotation|SelfAnnotations)Macros"
    - from: "HasAnnotation opaque type"
      to: "scala.annotation.RefiningAnnotation"
      via: "type bound A <: RefiningAnnotation"
      pattern: "RefiningAnnotation"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala` verbatim from fork (116 LOC, 7 leaves coupled in one file). Each companion `extends XMacros` from `MiscMacros.scala` (slice 5.0). `HasAnnotation` undergoes API reshape from `final class HasAnnotation[A,T] private ()` → `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A` (Pitfall 4 — documented breaking change).

Pre-port: audit `HasAnnotation.create` callsites (zero confirmed via `git grep` during planning — re-verify at execution time). Un-wrap AnnotationOfTest per fork commits `31970ec7` + `24e801ec`.

Output:
- Updated `AnnotationOf.scala` (covers 7 leaves)
- Un-wrapped `AnnotationOfTest.scala`
- MIGRATION.md §3 + §4 (bincompat) entries for HasAnnotation reshape
- 3 atomic commits (feat + test + docs) — own PR, base = `05-00-miscmacros-foundation` (stacked on slice 5.0)
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
<!-- Fork AnnotationOf.scala header (verbatim crib) -->
package com.avsystem.commons
package misc

import scala.annotation.{implicitNotFound, RefiningAnnotation}

@implicitNotFound("${T} is not annotated with ${A}")
case class AnnotationOf[A, T](annot: A) extends AnyVal
object AnnotationOf extends AnnotationOfMacros {}

case class OptAnnotationOf[A, T](annotOpt: Opt[A])
object OptAnnotationOf extends OptAnnotationOfMacros {}

case class AnnotationsOf[A, T](annots: List[A]) extends AnyVal
object AnnotationsOf extends AnnotationsOfMacros {}

opaque type HasAnnotation[A <: RefiningAnnotation, T] = A
object HasAnnotation { /* transparent inline def check / get companion methods */ }

case class SelfAnnotation[A](annot: A) extends AnyVal
object SelfAnnotation extends SelfAnnotationMacros {}

case class SelfOptAnnotation[A](annotOpt: Opt[A])
object SelfOptAnnotation extends SelfOptAnnotationMacros {}

case class SelfAnnotations[A](annots: List[A]) extends AnyVal
object SelfAnnotations extends SelfAnnotationsMacros {}

<!-- Macros traits + materializeX impls live in MiscMacros.scala (slice 5.0) -->
</interfaces>

<branch_strategy>
Branch off `05-00-miscmacros-foundation` tip (stacked on slice 5.0). Note: slice 5.0 lives on `origin` (which IS the halotukozak fork — `upstream` = AVSystem).
  git fetch origin && git checkout --track origin/05-00-miscmacros-foundation 2>/dev/null || true
  git checkout -b 05-05-annotation-of-family 05-00-miscmacros-foundation
</branch_strategy>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Pre-port audit + verbatim port of AnnotationOf.scala (7 leaves)</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Pattern 2 — Trait-Based Bundle; Code Examples Ex 2; Pitfall 4 — HasAnnotation reshape; Open Question 3; commits 31970ec7 + 24e801ec)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md`
    - Current file: `cat core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala`
    - Fork source: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala`
    - Verify slice 5.0 traits exist: `grep -cE 'trait (AnnotationOf|OptAnnotationOf|AnnotationsOf|SelfAnnotation|SelfOptAnnotation|SelfAnnotations)Macros' core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` should return 6
  </read_first>
  <action>
    1. **Pre-port grep audit** (Pitfall 4 / Open Question 3 — MUST run before overwriting):
       ```
       git grep -nE 'HasAnnotation\\.create' -- '*.scala'
       git grep -nE 'HasAnnotation\\b' -- '*.scala' | grep -v misc/AnnotationOf
       ```
       Expected: zero `HasAnnotation.create` hits (verified during planning). If any appear at execution time — STOP and document for user; the opaque-type reshape removes that factory.
    2. Branch (slice 5.0 base on origin (= halotukozak fork)):
       `git fetch origin`
       `git checkout --track origin/05-00-miscmacros-foundation 2>/dev/null || git checkout 05-00-miscmacros-foundation`
       `git checkout -b 05-05-annotation-of-family 05-00-miscmacros-foundation`
    3. Verify slice 5.0 traits land in MiscMacros.scala — see read_first grep.
    4. Overwrite: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala > core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala`
    5. Per Pitfall 2: preserve fork's exact `inline given [A, T] => ...` syntax — DO NOT normalize.
    6. Run `sbt commons-core/compile`. EXPECTED: green. If a downstream consumer (e.g. `meta/AdtMetadataCompanion`, `serialization/macroCodecs`) breaks on `HasAnnotation` shape — adjust the consumer to use `HasAnnotation.check[A,T]` / `HasAnnotation.get[A,T]` per fork. Document in commit body.
    7. `sbt scalafmtCheckAll` + auto-fix if needed.
    8. Commit: `feat(scala-3,core): port AnnotationOf family (7 leaves, opaque HasAnnotation)` with body referencing fork file + fork commits `31970ec7` + `24e801ec` + noting HasAnnotation API reshape per Pitfall 4.
  </action>
  <verify>
    <automated>! git grep -nE 'HasAnnotation\\.create' -- '*.scala' &amp;&amp; grep -q 'extends AnnotationOfMacros' core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala &amp;&amp; grep -q 'opaque type HasAnnotation' core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `! git grep -nE 'HasAnnotation\\.create' -- '*.scala'` (zero hits)
    - `grep -cE 'extends (AnnotationOf|OptAnnotationOf|AnnotationsOf|SelfAnnotation|SelfOptAnnotation|SelfAnnotations)Macros' core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala` == 6
    - `grep -q 'opaque type HasAnnotation\\[A <: RefiningAnnotation' core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala`
    - `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala) core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala` shows minimal diff (whitespace only)
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): port AnnotationOf family`
  </acceptance_criteria>
  <done>AnnotationOf.scala verbatim port; 6 companions wired to slice-5.0 traits; HasAnnotation opaque-type reshape live.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Un-wrap AnnotationOfTest</name>
  <files>core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala</files>
  <read_first>
    - Current test: `cat core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala`
    - Fork test: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/AnnotationOfTest.scala`
  </read_first>
  <action>
    1. Diff current vs fork. Likely current is Phase-1 wrapped. Prefer wholesale overwrite from fork if no out-of-scope deps:
       `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/AnnotationOfTest.scala > core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala`
    2. Run `sbt 'commons-core/testOnly *.AnnotationOfTest'`. EXPECTED: green.
    3. Cases that require Phase-6 `MetaMacros.valueImpl` real body → mark `.ignore` per fork shape.
    4. Commit: `test(scala-3,core): un-wrap AnnotationOfTest`.
  </action>
  <verify>
    <automated>! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala &amp;&amp; sbt -batch 'commons-core/testOnly *.AnnotationOfTest'</automated>
  </verify>
  <acceptance_criteria>
    - `! grep -qE 'TODO\\[scala3-port\\]' core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala`
    - `sbt -batch 'commons-core/testOnly *.AnnotationOfTest'` exits 0 with 0 failures
    - `git log -1 --pretty=%s` matches `^test\\(scala-3,core\\): un-wrap AnnotationOfTest$`
  </acceptance_criteria>
  <done>Test un-wrapped, green; second atomic commit.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 3: Update MIGRATION.md §3 + §4 (AnnotationOf family + HasAnnotation reshape)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md` (find §3 + §4 bincompat)
  </read_first>
  <action>
    Two-entry update:

    1. §3 `core` row append:
       ```
       | misc/AnnotationOf family | n/a | source-compat (HasAnnotation reshape) | Phase-5 slice 5.5: ported 7 leaves from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala`. Companions extend Macros traits from MiscMacros.scala (slice 5.0). Fork commits 31970ec7 (AnnotationOf/Opt/Annotations real impls) + 24e801ec (Self* real impls). HasAnnotation reshaped: `final class HasAnnotation[A,T] private ()` → `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A`. Removes `HasAnnotation.create[A,T]` factory (zero internal callers per pre-port audit). |
       ```

    2. §4 (bincompat) row append:
       ```
       | misc.HasAnnotation | bincompat-break | opaque type replacing final class private ctor; type bound `A <: RefiningAnnotation` tightened. Downstream consumers must use `HasAnnotation.check[A,T]` / `HasAnnotation.get[A,T]` companion methods. |
       ```

    3. Backlog: remove all 7 leaf rows from Phase-1 `TODO[scala3-port]` seeding.

    Commit: `docs(migration): record AnnotationOf family port + HasAnnotation reshape`.
  </action>
  <verify>
    <automated>grep -q 'slice 5.5' MIGRATION.md &amp;&amp; grep -q 'opaque type' MIGRATION.md &amp;&amp; grep -q 'RefiningAnnotation' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'slice 5.5' MIGRATION.md` exits 0
    - `grep -q 'AnnotationOf' MIGRATION.md` exits 0
    - `grep -q 'opaque type' MIGRATION.md` exits 0
    - `grep -q 'RefiningAnnotation' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record AnnotationOf family port`
  </acceptance_criteria>
  <done>MIGRATION.md updated with §3 + §4 entries; third atomic commit.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-05-annotation-of-family` with 3 commits. ANNOTOF-01 satisfied via 7-leaf coupled port; HasAnnotation reshape live with documented bincompat break.</what-built>
  <how-to-verify>
    Pre-push:
    1. `git log --oneline 05-00-miscmacros-foundation..HEAD | wc -l` == 3
    2. `sbt 'commons-core/compile ;commons-core/testOnly *.AnnotationOfTest ;scalafmtCheckAll'` exit 0
    3. `! git diff 05-00-miscmacros-foundation..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`
    4. `! git grep '???' core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala`
    5. `! git grep 'HasAnnotation\\.create' -- '*.scala'`

    Then:
    ```
    git push -u origin 05-05-annotation-of-family
    gh pr create --draft --base 05-00-miscmacros-foundation --head halotukozak:05-05-annotation-of-family \
      --title "[Scala 3] port AnnotationOf family (7 leaves, opaque HasAnnotation)" \
      --body "Slice 5.5 / Depends on: slice 5.0 (MiscMacros foundation) — PR #<5.0-PR> / Base branch: 05-00-miscmacros-foundation / Requirement: ANNOTOF-01 / Note: HasAnnotation API reshape (final class → opaque type with RefiningAnnotation bound); zero internal callers of removed `create` factory; bincompat break documented in MIGRATION.md §4"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- AnnotationOf.scala matches fork; 7 leaves wired to slice-5.0 traits
- AnnotationOfTest green
- MIGRATION.md §3 + §4 updated
- Zero HasAnnotation.create callers
- Draft PR, prefix, milestone 1
</verification>

<success_criteria>
- ANNOTOF-01 satisfied — `AnnotationOf[awesome, Foo]` resolves at compile; absent fails with `report.errorAndAbort`
- HasAnnotation opaque-type reshape live with documented bincompat
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-05-SUMMARY.md`
</output>
