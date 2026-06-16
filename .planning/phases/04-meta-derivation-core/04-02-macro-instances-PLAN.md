---
phase: 04-meta-derivation-core
plan: 02
type: execute
wave: 2
depends_on:
  - 04-01-foundation
files_modified:
  - core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
  - MIGRATION.md
autonomous: true
requirements:
  - META-CORE-01
  - META-CORE-02
  - QUALITY-01
  - PR-01
  - PR-02
  - PR-03
  - WORKFLOW-01
  - WORKFLOW-02
  - WORKFLOW-03
  - WORKFLOW-04
  - WORKFLOW-05
  - DOC-02
must_haves:
  truths:
    - "MacroInstances.scala is rewritten from fork verbatim — sealed class + inline given materialize + transparent inline def materializeInstances + materializeWith annotation"
    - "`Instances <: AnyNamedTuple` bound applied (shape shift — API-breaking; documented in MIGRATION.md §3)"
    - "AllowDerivation.create + scala.NamedTuple.{AnyNamedTuple, DropNames} + compiletime.{erasedValue, summonInline} all referenced correctly"
    - "MacroInstancesTest.scala remains wrapped (un-wrap deferred to slice 4.5 per RESEARCH §Test Un-wrapping Plan)"
    - "sbt commons-core/compile + commons-core/Test/compile + scalafmtCheckAll exit 0"
    - "No new @nowarn/-Wconf"
    - "MIGRATION.md §3 documents (a) Instances <: AnyNamedTuple shape shift, (b) inline-given materialize replacing classical implicit def"
    - "Draft PR opened against base = 04-01-foundation branch (stacked) with [Scala 3] prefix + milestone 1 + body metadata referencing slice 4.1 PR"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala"
      provides: "MacroInstances sealed class + inline given materialize[Implicits, Instances <: AnyNamedTuple] + transparent inline def materializeInstances[T <: Tuple] + materializeWith annotation"
      contains: "inline given materialize"
    - path: "MIGRATION.md"
      provides: "§3 entries for slice 4.2 — Instances <: AnyNamedTuple bound + inline-given materialize"
      contains: "AnyNamedTuple"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala"
      to: "AllowDerivation.create"
      via: "given AllowDerivation[h] = AllowDerivation.create"
      pattern: "AllowDerivation\\.create"
    - from: "core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala"
      to: "scala.NamedTuple.{AnyNamedTuple, DropNames}"
      via: "import scala.NamedTuple"
      pattern: "scala\\.NamedTuple"
    - from: "core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala"
      to: "compiletime.{erasedValue, summonInline}"
      via: "inline compiletime.erasedValue[T] match"
      pattern: "compiletime\\.(erasedValue|summonInline)"
---

<objective>
Slice 4.2 — Replace Phase-1 `???` body of `MacroInstances.scala` with the fork's Scala 3 `inline given` + named-tuple
+ `compiletime.erasedValue`/`summonInline` derivation. ~30 LOC port — small but architecturally pivotal: every
downstream codec/RPC companion in Phases 6+ summons `MacroInstances.materialize`.

Purpose: Unlock inline-driven companion-implicits materialization. This is the only slice in Phase 4 that ships a
REAL macro implementation — slices 4.3/4.5 ship fork-staged `???` placeholders.

Output: Rewritten `MacroInstances.scala` matching fork verbatim (with `Instances <: AnyNamedTuple` API-break),
MIGRATION.md §3 entries, branch `04-02-macro-instances` stacked on `04-01-foundation`, draft PR opened.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/04-meta-derivation-core/04-CONTEXT.md
@.planning/phases/04-meta-derivation-core/04-RESEARCH.md
@.planning/phases/04-meta-derivation-core/04-01-foundation-PLAN.md
@core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
@core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala
@MIGRATION.md

<interfaces>
<!-- Fork canonical source — executor MUST git show before porting. -->
<!-- Verbatim from RESEARCH §"Code Examples": -->

```scala
// origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala
package com.avsystem.commons
package meta

import scala.NamedTuple.{AnyNamedTuple, DropNames}

sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple](applyImpl: (Implicits, Any) => Instances) {
  def apply(implicits: Implicits, companion: Any): Instances = applyImpl(implicits, companion)
}

object MacroInstances {
  inline given materialize[Implicits, Instances <: AnyNamedTuple]: MacroInstances[Implicits, Instances] =
    MacroInstances[Implicits, Instances] { (implicits, companion) =>
      import implicits.given
      materializeInstances[DropNames[Instances]].asInstanceOf[Instances]
    }

  transparent inline def materializeInstances[T <: Tuple]: T = inline compiletime.erasedValue[T] match {
    case _: EmptyTuple => EmptyTuple.asInstanceOf[T]
    case _: (h *: t) =>
      given AllowDerivation[h] = AllowDerivation.create
      (compiletime.summonInline[h] *: materializeInstances[t]).asInstanceOf[T]
  }

  final class materializeWith(prefix: Any, materializer: String = "materialize") extends StaticAnnotation
}
```

Dependencies (must already be on the branch from slice 4.1):
- `AllowDerivation` (from 04-01-foundation)
- `scala.NamedTuple.{AnyNamedTuple, DropNames}` (stdlib — verified stable by slice 4.1 Wave-0 probe)
- `compiletime.{erasedValue, summonInline}` (stdlib — stable since 3.0)
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut branch 04-02-macro-instances off 04-01-foundation; port MacroInstances.scala verbatim from fork</name>
  <read_first>
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala
    - core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala (current Phase-1 ??? stub)
    - core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala (confirm slice 4.1 landed it)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Code Examples — MacroInstances", Pitfalls 1/5/6/8)
  </read_first>
  <action>
    1. Verify branch base — slice 4.1 tip must be present locally:
       ```bash
       git fetch origin
       git checkout 04-01-foundation  # confirm exists
       git log -1 --oneline  # note SHA
       git checkout -b 04-02-macro-instances 04-01-foundation
       ```

    2. Confirm AllowDerivation present on branch:
       ```bash
       test -f core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala  # exit 0
       grep -c 'def create\[T\]' core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala  # ≥ 1
       ```

    3. Port MacroInstances.scala verbatim from fork:
       ```bash
       git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala \
         > core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
       ```

    4. Verify content matches verbatim block in `<interfaces>` above — key signatures must be present:
       ```bash
       grep -c 'sealed class MacroInstances\[Implicits, Instances <: AnyNamedTuple\]' core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala  # 1
       grep -c 'inline given materialize\[Implicits, Instances <: AnyNamedTuple\]' core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala  # 1
       grep -c 'transparent inline def materializeInstances\[T <: Tuple\]' core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala  # 1
       grep -c 'compiletime.erasedValue\[T\]' core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala  # 1
       grep -c 'compiletime.summonInline\[h\]' core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala  # 1
       grep -c 'class materializeWith' core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala  # 1
       ```

    5. Compile gate:
       ```bash
       sbt commons-core/compile
       sbt commons-core/Test/compile
       sbt scalafmtCheckAll
       ```
       All MUST exit 0. If `scala.NamedTuple` import fails (Pitfall 1 — slice 4.1 Wave-0 probe should have caught this,
       but re-verify): add `-language:experimental.namedTuples` to scalacOptions in `project/Commons.scala` AS A
       SEPARATE COMMIT in this slice, NOT as part of the MacroInstances port commit.

    6. Commit (fork-cadence — atomic port):
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
       git commit -m "feat(scala-3,core): port MacroInstances (inline given + named-tuple materialization)

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala.

Replaces Phase-1 ??? stub with:
- sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple]
- inline given materialize using NamedTuple.DropNames over the Instances tuple
- transparent inline def materializeInstances recursing over Tuple shape via
  compiletime.erasedValue / summonInline
- final class materializeWith StaticAnnotation marker

API shape shift: Instances upper-bounded to AnyNamedTuple (was unbounded in our
Phase-1 stub). Downstream Instances traits in Phases 6/7 must be reshaped to
named-tuple type aliases. Documented in MIGRATION.md §3 (separate commit)."
       ```
  </action>
  <verify>
    <automated>sbt commons-core/compile && sbt commons-core/Test/compile</automated>
  </verify>
  <acceptance_criteria>
    - File matches fork verbatim — all 6 grep checks return 1
    - `sbt commons-core/compile` exit 0
    - `sbt commons-core/Test/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
    - `git diff HEAD~1 HEAD -- core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala` shows replacement of stub body
  </acceptance_criteria>
  <done>
    MacroInstances.scala matches fork verbatim with all 6 key signatures present. Compile + test-compile + scalafmt green.
    Single fork-cadence commit added to `04-02-macro-instances`.
  </done>
</task>

<task type="auto">
  <name>Task 2: MIGRATION.md §3 — document Instances <: AnyNamedTuple shape shift + inline-given materialize</name>
  <read_first>
    - MIGRATION.md (§3 source-compat)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Per-Slice Recommendations" 4.2 MIGRATION.md updates + §"State of the Art" table)
  </read_first>
  <action>
    Edit `MIGRATION.md` §3 — append under `### core — meta foundation (slice 4.1)` a new sub-section:

    ```markdown
    ### core — meta MacroInstances (slice 4.2)

    - `meta/MacroInstances` now constrains `Instances <: AnyNamedTuple` (was unbounded in our Phase 1 stub).
      Downstream `XyzInstances` types passed as the second type parameter must be **named-tuple type aliases** —
      classical-trait instance bundles will fail to compile.
      Example reshape pattern:
      ```scala
      // before: trait FooInstances { def codec: GenCodec[Foo]; def meta: GenMetadata[Foo] }
      // after:  type FooInstances = (codec: GenCodec[Foo], meta: GenMetadata[Foo])
      ```
    - `MacroInstances.materialize` is now an `inline given` (was classical `implicit def` stub) — call sites need
      no change but error messages from failed implicit search are now Scala-3 standard (not the legacy detailed
      tree-printing trace from the Scala 2 macro).
    - `MacroInstances` is now `sealed class` (was open trait stub) — downstream subclassing prohibited.
    ```

    Commit (separate fork-cadence commit):
    ```bash
    git add MIGRATION.md
    git commit -m "docs(migration): record MacroInstances API shape shift (slice 4.2)

§3 source-compat additions:
- Instances <: AnyNamedTuple bound — named-tuple aliases required for downstream Instances
- inline given materialize replaces classical implicit def (error message regression noted)
- sealed class MacroInstances prevents downstream subclassing

Phase 4 slice 4.2 of 4.1→4.2→4.3→4.4→4.5 stacked PR chain."
    ```
  </action>
  <verify>
    <automated>grep -c 'slice 4.2' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'slice 4.2' MIGRATION.md` ≥ 1
    - `grep -c 'AnyNamedTuple' MIGRATION.md` ≥ 1
    - `grep -c 'inline given materialize' MIGRATION.md` ≥ 1
    - Commit subject starts with `docs(migration):`
  </acceptance_criteria>
  <done>
    MIGRATION.md §3 documents the Instances <: AnyNamedTuple bound + inline-given replacement + sealed-class shape.
    Committed under `docs(migration):` prefix as a separate fork-cadence commit.
  </done>
</task>

<task type="auto">
  <name>Task 3: Final acceptance gate + push 04-02-macro-instances + open stacked draft PR</name>
  <read_first>
    - ~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md (PR rules)
    - .planning/phases/04-meta-derivation-core/04-01-foundation-PLAN.md (slice 4.1 PR number — needed for "Depends on" body line)
  </read_first>
  <action>
    1. Acceptance gate (per phase_specific_constraints 4.2):
       ```bash
       sbt commons-core/compile        # exit 0
       sbt commons-core/Test/compile   # exit 0
       sbt scalafmtCheckAll            # exit 0
       ```

    2. Fork-shape parity — confirm key signatures byte-identical to fork:
       ```bash
       diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala) \
            core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
       # MUST exit 0 (or only whitespace differences — scalafmt may reformat indentation)
       ```

    3. Inline-given resolution sanity probe — confirm `materialize` resolves at call site. Drop a scratch test
       file under `core/src/test/scala/_MacroInstancesResolve.scala`:
       ```scala
       package com.avsystem.commons.meta
       object _MacroInstancesResolve {
         type X = (a: Int, b: String)
         summon[MacroInstances[Unit, X]]  // compile-only sanity: must resolve
       }
       ```
       Then:
       ```bash
       sbt commons-core/Test/compile  # must compile
       rm core/src/test/scala/_MacroInstancesResolve.scala
       ```
       If the probe fails → MacroInstances.materialize is broken; debug before push.

    4. No new @nowarn/-Wconf:
       ```bash
       git diff 04-01-foundation..HEAD -- '*.scala' | grep -cE '^\+.*(@nowarn|-Wconf)'
       # MUST be 0
       ```

    5. No `.planning/`:
       ```bash
       git log --name-only 04-01-foundation..HEAD | grep -c '^\.planning/'  # 0
       ```

    6. Push:
       ```bash
       git push -u origin 04-02-macro-instances
       ```

    7. Look up slice 4.1 PR number to populate "Depends on":
       ```bash
       SLICE_41_PR=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:04-01-foundation --json number --jq '.[0].number')
       echo "Slice 4.1 PR: #$SLICE_41_PR"
       ```

    8. Open draft PR (base = 04-01-foundation branch on halotukozak fork OR upstream/scala-3 if 4.1 already merged):
       ```bash
       # If 04-01-foundation PR still open, use it as the base (stacked review)
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:04-02-macro-instances \
         --draft \
         --title "[Scala 3] port MacroInstances (inline given + named-tuple materialization)" \
         --body "$(cat <<EOF
**Slice:** 4.2 of Phase 4 (meta/ derivation core)
**Merge order:** 4.1 → 4.2 → 4.3 → 4.4 → 4.5
**Depends on:** #${SLICE_41_PR}
**Base branch:** 04-01-foundation (stacked) — re-base on upstream/scala-3 if slice 4.1 merges first

## Summary
Replaces Phase-1 \`???\` stub of \`MacroInstances.scala\` with fork's Scala 3 \`inline given\` derivation:

- \`sealed class MacroInstances[Implicits, Instances <: AnyNamedTuple]\`
- \`inline given materialize\` using \`NamedTuple.DropNames\` over the Instances tuple
- \`transparent inline def materializeInstances\` recursing over Tuple shape via \`compiletime.erasedValue\` / \`summonInline\`
- \`final class materializeWith\` StaticAnnotation marker (consumed by Phases 6/7)

## API shape shifts (MIGRATION.md §3)
- \`Instances <: AnyNamedTuple\` bound — downstream \`XyzInstances\` types must become named-tuple aliases
- \`inline given materialize\` replaces classical \`implicit def\` — implicit-search error messages now Scala-3 standard
- \`sealed class MacroInstances\` — downstream subclassing prohibited

## Acceptance
- \`sbt commons-core/compile\` exit 0
- \`sbt commons-core/Test/compile\` exit 0
- \`sbt scalafmtCheckAll\` exit 0
- 0 new \`@nowarn\`/\`-Wconf\`
- Inline-given resolution sanity probe green (summon[MacroInstances[Unit, (a: Int, b: String)]])

Translated from \`origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala\` per [[feedback_crib_from_master]].
EOF
)"
       ```

    9. Capture PR number, set milestone 1:
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
       ```

    10. Verify PR state:
        ```bash
        gh pr view $PR_NUM --repo AVSystem/scala-commons --json title,isDraft,milestone,baseRefName
        ```
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head halotukozak:04-02-macro-instances --json number,isDraft | grep -c '"isDraft": true'</automated>
  </verify>
  <acceptance_criteria>
    - `sbt commons-core/compile` + `Test/compile` + `scalafmtCheckAll` all exit 0
    - `diff` fork vs our file → empty (or whitespace-only)
    - Inline-given resolution probe green
    - 0 new `@nowarn`/`-Wconf`
    - 0 `.planning/` in diff
    - Branch pushed; draft PR open with `[Scala 3]` prefix, milestone 1, body cites slice 4.1 PR number as "Depends on"
  </acceptance_criteria>
  <done>
    Slice 4.2 PR open against upstream/scala-3 with stacked-review metadata. Slice 4.3 will branch off `04-02-macro-instances`.
  </done>
</task>

</tasks>

<verification>
- MacroInstances.scala matches fork verbatim (key signatures grep-confirmed)
- `Instances <: AnyNamedTuple` bound is present
- `sbt commons-core/compile + Test/compile + scalafmtCheckAll` exit 0
- Inline-given materialize resolves at call site (probe)
- 0 new `@nowarn`/`-Wconf` vs slice 4.1
- 0 `.planning/` in commits
- MIGRATION.md §3 documents 3 shape shifts
- Draft PR open on AVSystem/scala-commons with body metadata + Depends on slice 4.1 PR
</verification>

<success_criteria>
Slice 4.2 succeeds when:
1. `MacroInstances.materialize` inline given resolves for `Instances <: AnyNamedTuple`
2. `transparent inline def materializeInstances` compiles + recurses correctly over Tuple shape
3. MIGRATION.md §3 records the API shape shifts (Instances bound, inline-given, sealed-class)
4. Draft PR open against `upstream/scala-3` with body declaring slice 4.1 PR as Depends on
</success_criteria>

<output>
After completion, create `.planning/phases/04-meta-derivation-core/04-02-SUMMARY.md`
</output>
