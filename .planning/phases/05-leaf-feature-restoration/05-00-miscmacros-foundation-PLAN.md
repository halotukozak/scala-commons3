---
phase: 05-leaf-feature-restoration
plan: 00
type: execute
wave: 0
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala
  - MIGRATION.md
autonomous: false
requirements: []
must_haves:
  truths:
    - "core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala exists with verbatim fork content (≈310 LOC)"
    - "sbt commons-core/compile exits 0 with MiscMacros file present (no callers yet — file is self-contained)"
    - "MIGRATION.md records new MiscMacros.scala foundation file under §3"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala"
      provides: "AnnotationOfMacros / OptAnnotationOfMacros / AnnotationsOfMacros / SelfAnnotationMacros / SelfOptAnnotationMacros / SelfAnnotationsMacros / SimpleClassNameMacros / SourceInfoMacros / ImplicitsMacros / SelfInstanceMacros / DelegationMacros / DelegationApplyMacros traits + object MiscMacros with materializeX defs + annotsOfT/expandAggregates helpers"
      contains: "object MiscMacros"
    - path: "MIGRATION.md"
      provides: "§3 entry recording foundation file port"
      contains: "MiscMacros"
  key_links:
    - from: "MiscMacros.scala"
      to: "Phase 4 meta/MetaMacros (still '{ ??? }' stubs)"
      via: "ImplicitsMacros calls MiscMacros.inferImpl which may indirectly depend on Phase 4"
      pattern: "scala.quoted.Quotes"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` VERBATIM from fork master (`origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`, ≈310 LOC). This file is the centralised impl bundle that 2 of 7 leaf slices (AnnotationOf family, Delegation) depend on.

Purpose: Foundation slice 5.0 — establish the shared macro impl module so downstream leaves (5.5 AnnotationOf, 5.2 Delegation) can declare only thin `object X extends XMacros {}` shells. Matches fork shape per [[feedback_crib_from_master]]. CONTEXT explicitly allows Claude's Discretion on "Whether to merge slices into fewer PRs if fork-shape allows" — research strongly recommends this foundation slice.

Output:
- `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` (new file)
- MIGRATION.md §3 entry
- 2 atomic commits (feat + docs) — own PR, base `04-05-meta-annotations`
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md
@.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md
@.planning/phases/05-leaf-feature-restoration/05-VALIDATION.md

<interfaces>
<!-- Fork's MiscMacros.scala header (extracted via `git show origin/master:...`) -->
package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.TodoScala3Migration
import scala.quoted.*

trait AnnotationOfMacros { inline given [A, T] => AnnotationOf[A, T] = ${ MiscMacros.materializeAnnotationOf[A, T] } }
trait OptAnnotationOfMacros { inline given [A, T] => OptAnnotationOf[A, T] = ${ MiscMacros.materializeOptAnnotationOf[A, T] } }
trait AnnotationsOfMacros { inline given [A, T] => AnnotationsOf[A, T] = ${ MiscMacros.materializeAnnotationsOf[A, T] } }
trait SelfAnnotationMacros { inline given [A] => SelfAnnotation[A] = ${ MiscMacros.materializeSelfAnnotation[A] } }
trait SelfOptAnnotationMacros { inline given [A] => SelfOptAnnotation[A] = ${ MiscMacros.materializeSelfOptAnnotation[A] } }
trait SelfAnnotationsMacros { inline given [A] => SelfAnnotations[A] = ${ MiscMacros.materializeSelfAnnotations[A] } }
trait SimpleClassNameMacros { inline given [T] => SimpleClassName[T] = ${ MiscMacros.materializeSimpleClassName[T] } }
trait SourceInfoMacros { inline given SourceInfo = ${ MiscMacros.materializeSourceInfo } }

@TodoScala3Migration("Implicits.infer family — need real implicit-search quoted impl")
trait ImplicitsMacros { ... }

@TodoScala3Migration("SelfInstance.materialize is a stub")
trait SelfInstanceMacros { inline given [C[_]] => SelfInstance[C] = ??? }

@TodoScala3Migration("Delegation.materializeDelegation is a stub — DelegationTest is `ignore`d")
trait DelegationMacros { ... }
trait DelegationApplyMacros[B] { ... }

object MiscMacros { /* materializeX defs + annotsOfT + expandAggregates helpers */ }
</interfaces>

<branch_strategy>
Branch off `04-05-meta-annotations @ f04cec6f` (Phase 4 final tip). Per CONTEXT: leaves stack on Phase 4 tip until Phase 4 merges. PR base = `04-05-meta-annotations` (or `scala-3` if Phase 4 has merged by execution time — check `gh pr view 870` etc.).

Cut command:
  git fetch origin && git checkout -b 05-00-miscmacros-foundation origin/04-05-meta-annotations
</branch_strategy>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut branch, port MiscMacros.scala verbatim from fork</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (sections: Architecture Patterns, Code Examples Ex 2, Standard Stack, Common Pitfalls 1+2+4+5+7)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md` (locked decisions)
    - Fork source (full read): `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala` (≈310 LOC)
    - Current state of Phase 4 tip: `git log --oneline 04-05-meta-annotations | head -5`
    - Existing tree under `core/src/main/scala/com/avsystem/commons/misc/` (to confirm `MiscMacros.scala` does NOT already exist): `ls core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` should fail
  </read_first>
  <action>
    Verbatim port. NO redesign.

    Steps:
    1. Verify branch base: `git fetch origin upstream && git checkout -b 05-00-miscmacros-foundation 04-05-meta-annotations`. Confirm HEAD = `f04cec6f`.
    2. Extract fork file to disk:
       `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala > core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala`
    3. Run `sbt scalafmtCheckAll` — if it fails, run `sbt scalafmtAll` per [[feedback_scala3_migrate_syntax]] precedent (Rule 3 auto-fix); confirm result still matches fork shape modulo whitespace (`diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala) core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` shows only whitespace).
    4. Run `sbt commons-core/compile`. EXPECTED: GREEN. MiscMacros.scala has no callers yet (leaves bring callers in waves 1+), so compile should be unaffected by addition — file is self-contained, depends only on `scala.quoted`, `scala.compiletime`, existing misc/* leaf type definitions (AnnotationOf etc. which are case classes at this point — already present in tree as `???`-stub companions).
    5. If compile errors appear referencing types like `RpcMacros`, `MetaMacros`, etc. that this file imports: investigate, but DO NOT fix by editing MiscMacros — instead update `<known_issues>` of summary. Fork compiles cleanly so any divergence indicates our tree differs from fork; resolve by aligning the missing dependency, NOT by editing the verbatim port.
    6. Commit: `feat(scala-3,core): port MiscMacros foundation bundle` with body referencing `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`.

    Per [[feedback_crib_from_master]]: do NOT rewrite. Preserve fork's exact `inline given [T] => ...` syntax (Pitfall 2). Preserve `@TodoScala3Migration` annotations verbatim (Pitfall 1 — flags ImplicitsMacros / SelfInstanceMacros / DelegationMacros as known staging stubs). Preserve `@publicInBinary` on any private members (Pitfall noted in research's anti-patterns).
  </action>
  <verify>
    <automated>test -f core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `test -f core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` exits 0
    - `wc -l core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` reports between 280 and 350 lines (fork is 310)
    - `grep -q 'object MiscMacros' core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` exits 0
    - `grep -q 'trait AnnotationOfMacros' core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` exits 0
    - `grep -q 'trait DelegationMacros' core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` exits 0
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git diff origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala -- | grep -vE '^(diff|index|---|\+\+\+|@@| )' | grep -vE '^[+-]\s*$'` shows minimal diff (whitespace + scalafmt only)
    - Commit message regex: `^feat\(scala-3,core\): port MiscMacros foundation bundle`
  </acceptance_criteria>
  <done>MiscMacros.scala exists in tree as verbatim port; `commons-core/compile` green; one `feat(...)` commit on branch.</done>
</task>

<task type="auto">
  <name>Task 2: Update MIGRATION.md §3 with MiscMacros foundation entry</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md` (find §3 section header)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md` (commit-cadence + MIGRATION conventions)
  </read_first>
  <action>
    Per [[feedback_migration_md_contract]]: every slice updates MIGRATION.md.

    Append under §3 (source-compat / per-slice notes), section `core`:

    ```
    | MiscMacros foundation | n/a | n/a | Phase-5 slice 5.0: ported `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` (~310 LOC) verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`. Foundation for slices 5.5 (AnnotationOf) + 5.2 (Delegation). Other leaves do not depend on this file. `ImplicitsMacros` / `SelfInstanceMacros` / `DelegationMacros` traits carry `@TodoScala3Migration` markers — runtime `???` matches fork staging. |
    ```

    Exact section header to find: `## 3.` or `## Source-compat` — match the existing format already in MIGRATION.md (read first to determine).

    Commit: `docs(migration): record MiscMacros foundation port` (separate atomic commit, NO squash per CONTEXT cadence).
  </action>
  <verify>
    <automated>grep -q 'MiscMacros foundation' MIGRATION.md &amp;&amp; git log --oneline -2 | head -1 | grep -q 'docs(migration): record MiscMacros foundation port'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'MiscMacros foundation' MIGRATION.md` exits 0
    - `grep -q 'slice 5.0' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\(migration\): record MiscMacros foundation port$`
    - `git diff HEAD~1 HEAD -- MIGRATION.md | grep -c '^+'` >= 1
  </acceptance_criteria>
  <done>MIGRATION.md updated with §3 entry; second atomic commit on branch.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 3: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-00-miscmacros-foundation` with 2 commits: (1) `feat(scala-3,core): port MiscMacros foundation bundle`, (2) `docs(migration): record MiscMacros foundation port`. `commons-core/compile` + `scalafmtCheckAll` green.</what-built>
  <how-to-verify>
    Per global rule + memory [[feedback_pr_draft]] + [[feedback_pr_title_prefix]] + [[feedback_pr_milestone]] + WORKFLOW-03: user ack required before push and before PR open.

    Verification:
    1. `git log --oneline 04-05-meta-annotations..HEAD` shows exactly 2 commits, both Conventional Commits
    2. `git diff 04-05-meta-annotations..HEAD --stat` shows only MiscMacros.scala (new) + MIGRATION.md (modified)
    3. `sbt 'commons-core/compile ;scalafmtCheckAll'` exit 0
    4. `! git log 04-05-meta-annotations..HEAD --pretty=%B | grep -E '(\.planning/|GSD|get-shit-done)'` (no leakage)
    5. `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` (QUALITY-01 no new suppressions)

    Then push + open PR:
    ```
    git push -u origin 05-00-miscmacros-foundation
    gh pr create --draft --base 04-05-meta-annotations --head halotukozak:05-00-miscmacros-foundation \
      --title "[Scala 3] port MiscMacros foundation bundle" \
      --body "Slice 5.0 (foundation) / Phase 5 leaf-feature-restoration / Depends on: #<phase-4-final-PR> / Base branch: 04-05-meta-annotations (stack-on-phase-4 until Phase 4 merges) / Required by: slices 5.2 (Delegation), 5.5 (AnnotationOf family)"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone assignment confirmed, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- `sbt commons-core/compile` exits 0 on branch tip
- `sbt scalafmtCheckAll` exits 0
- `git log --oneline 04-05-meta-annotations..HEAD | wc -l` = 2
- PR draft, `[Scala 3]` prefix, milestone 1
- `git grep '???' core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` may show stub bodies inside `DelegationMacros` / `ImplicitsMacros` / `SelfInstanceMacros` — matches fork staging (verify equal count to fork)
</verification>

<success_criteria>
- MiscMacros.scala exists in tree, verbatim from fork
- `commons-core/compile` green
- MIGRATION.md §3 mentions slice 5.0 foundation port
- Draft PR open with `[Scala 3]` prefix, milestone 1, base = `04-05-meta-annotations`
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-00-SUMMARY.md`
</output>
