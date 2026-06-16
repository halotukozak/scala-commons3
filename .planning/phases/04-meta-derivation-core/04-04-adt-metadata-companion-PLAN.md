---
phase: 04-meta-derivation-core
plan: 04
type: execute
wave: 4
depends_on:
  - 04-03-meta-macros
files_modified:
  - core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
  - core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala
  - MIGRATION.md
autonomous: true
requirements:
  - META-CORE-02
  - META-CORE-07
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
    - "AdtMetadataCompanion[M[X] <: TypedMetadata[X]] bound TIGHTENED from M[_] (API-break — fork shape)"
    - "BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] extends BoundedAdtMetadataCompanionMacros[Hi, Lo, M] with BoundedMetadataCompanion[Hi, Lo, M]"
    - "AdtMetadataCompanion file collapses from 4-method stub to 2-line trait composition per fork"
    - "AdtMetadataTest.scala selectively un-wrapped — compile-time assertions live; runtime tests that exercise lazyMetadataImpl ??? marked `pending` (NOT @nowarn, NOT @ignore)"
    - "sbt commons-core/compile + commons-core/Test/compile + scalafmtCheckAll exit 0"
    - "No new @nowarn/-Wconf"
    - "MIGRATION.md §3 documents M[X] <: TypedMetadata[X] bound tightening (bincompat-narrowing) + §4 bincompat entry"
    - "Draft PR opened stacked on 04-03-meta-macros with [Scala 3] prefix + milestone 1 + Depends on slice 4.3 PR"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala"
      provides: "AdtMetadataCompanion[M[X] <: TypedMetadata[X]] + BoundedAdtMetadataCompanion trait composition"
      contains: "M[X] <: TypedMetadata[X]"
    - path: "core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala"
      provides: "Un-wrapped tests (compile-time live, runtime pending)"
      contains: "AdtMetadataTest"
    - path: "MIGRATION.md"
      provides: "§3 + §4 entries on bound tightening"
      contains: "TypedMetadata"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala"
      to: "core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala"
      via: "extends AdtMetadataCompanionMacros[M] with MetadataCompanion[M]"
      pattern: "AdtMetadataCompanionMacros"
    - from: "core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala"
      to: "core/src/main/scala/com/avsystem/commons/meta/metadata.scala"
      via: "type bound M[X] <: TypedMetadata[X]"
      pattern: "TypedMetadata"
---

<objective>
Slice 4.4 — Collapse `AdtMetadataCompanion.scala` from our 4-method Phase-1 stub (with `M[_]` bound) to fork's
2-line trait composition with bound tightened to `M[X] <: TypedMetadata[X]`. Same for `BoundedAdtMetadataCompanion`.
Un-wrap `AdtMetadataTest.scala` selectively — compile-time assertions live, runtime assertions marked `pending`
because they exercise `MetaMacros.dummy` ??? body.

Purpose: Land the ADT-metadata derivation entry point. The bound tightening is the only API-break in this slice
(documented in MIGRATION.md §3 + §4).

Output: 2 source files (1 rewritten, 1 test un-wrapped), MIGRATION.md updates, draft PR stacked on slice 4.3.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/04-meta-derivation-core/04-CONTEXT.md
@.planning/phases/04-meta-derivation-core/04-RESEARCH.md
@core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
@core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
@core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
@core/src/main/scala/com/avsystem/commons/meta/metadata.scala
@core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala
@MIGRATION.md

<interfaces>
```scala
// origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AdtMetadataCompanion.scala — verbatim
package com.avsystem.commons
package meta

trait AdtMetadataCompanion[M[X] <: TypedMetadata[X]]
  extends AdtMetadataCompanionMacros[M] with MetadataCompanion[M] {}

trait BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]]
  extends BoundedAdtMetadataCompanionMacros[Hi, Lo, M] with BoundedMetadataCompanion[Hi, Lo, M] {}
```

Dependencies (must be on branch from prior slices):
- AdtMetadataCompanionMacros[M[_]] — from MetaMacros.scala (slice 4.3)
- BoundedAdtMetadataCompanionMacros[Hi, Lo, M] — from MetaMacros.scala (slice 4.3)
- MetadataCompanion[M[_]] — from MetadataCompanion.scala (slice 4.3)
- BoundedMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] — from MetadataCompanion.scala (slice 4.3)
- TypedMetadata — from metadata.scala (slice 4.1)
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut 04-04-adt-metadata-companion off 04-03-meta-macros; rewrite AdtMetadataCompanion.scala</name>
  <read_first>
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AdtMetadataCompanion.scala
    - core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala (current Phase-1 stub — 4 methods + M[_])
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Code Examples — AdtMetadataCompanion", Open Question 4)
  </read_first>
  <action>
    1. Branch:
       ```bash
       git checkout 04-03-meta-macros
       git checkout -b 04-04-adt-metadata-companion
       ```

    2. Verify upstream slice 4.3 artifacts present:
       ```bash
       grep -c 'trait AdtMetadataCompanionMacros' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # ≥ 1
       grep -c 'trait BoundedAdtMetadataCompanionMacros' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # ≥ 1
       grep -c 'trait MetadataCompanion\[M\[_\]\]' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # ≥ 1
       grep -c 'trait BoundedMetadataCompanion' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # ≥ 1
       grep -c 'trait TypedMetadata' core/src/main/scala/com/avsystem/commons/meta/metadata.scala  # ≥ 1
       ```

    3. Port AdtMetadataCompanion.scala verbatim from fork:
       ```bash
       git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AdtMetadataCompanion.scala \
         > core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
       ```

    4. Verify shape:
       ```bash
       grep -c 'trait AdtMetadataCompanion\[M\[X\] <: TypedMetadata\[X\]\]' core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala  # 1
       grep -c 'extends AdtMetadataCompanionMacros\[M\] with MetadataCompanion\[M\]' core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala  # 1
       grep -c 'trait BoundedAdtMetadataCompanion\[Hi, Lo <: Hi, M\[_ >: Lo <: Hi\]\]' core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala  # 1
       grep -c 'extends BoundedAdtMetadataCompanionMacros\[Hi, Lo, M\] with BoundedMetadataCompanion\[Hi, Lo, M\]' core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala  # 1
       # File should be VERY small (~6 lines content + package decl)
       wc -l core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala  # ≤ 15 LOC
       ```

    5. Compile gate — verify the bound `M[X] <: TypedMetadata[X]` is accepted + trait composition resolves:
       ```bash
       sbt commons-core/compile  # exit 0
       sbt commons-core/Test/compile  # exit 0 (some tests may now fail compile if they declared M not extending TypedMetadata — see Task 2)
       sbt scalafmtCheckAll  # exit 0
       ```
       If compile fails because some downstream consumer in our tree declares an `M extends MetadataCompanion[?]`
       without extending `TypedMetadata` → that's the documented API break; fix the consumer (likely a test in the
       next task) or comment the broken consumer with `// TODO[scala3-port]: bound tightened in slice 4.4` and
       document in MIGRATION.md backlog.

    6. Commit:
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
       git commit -m "feat(scala-3,core): port AdtMetadataCompanion + BoundedAdtMetadataCompanion

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AdtMetadataCompanion.scala.

Collapses our Phase-1 4-method stub into fork's 2-line trait composition:
- trait AdtMetadataCompanion[M[X] <: TypedMetadata[X]] extends
    AdtMetadataCompanionMacros[M] with MetadataCompanion[M]
- trait BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] extends
    BoundedAdtMetadataCompanionMacros[Hi, Lo, M] with BoundedMetadataCompanion[Hi, Lo, M]

API shape shift: bound tightened from M[_] to M[X] <: TypedMetadata[X]. Documented
in MIGRATION.md §3 (source-compat) + §4 (bincompat) — separate commit.

Rationale (fork): enables inline given [T] => M[T] = materialize[T] in
AdtMetadataCompanionMacros — the bound is required for the inherited inline given
to resolve correctly."
       ```
  </action>
  <verify>
    <automated>sbt commons-core/compile && sbt commons-core/Test/compile</automated>
  </verify>
  <acceptance_criteria>
    - All 4 grep checks return 1
    - File ≤ 15 LOC
    - `sbt commons-core/compile` exit 0
    - `sbt commons-core/Test/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
  </acceptance_criteria>
  <done>
    AdtMetadataCompanion.scala matches fork verbatim with `M[X] <: TypedMetadata[X]` bound + trait composition.
  </done>
</task>

<task type="auto">
  <name>Task 2: Selectively un-wrap AdtMetadataTest.scala — compile-time assertions live, runtime pending</name>
  <read_first>
    - core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala (current — wrapped in Phase 1 per STATE Plan 05)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Test Un-wrapping Plan" + Pitfall 2)
    - ~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md (feedback_fix_dont_suppress_warnings — pending, NOT @nowarn)
  </read_first>
  <action>
    1. Inspect current wrap state:
       ```bash
       head -20 core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala
       grep -nE '^\s*/\*|\*/\s*$' core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala | head -20
       ```
       Expect: `package` + `import` lines live, then `/*` wrapping the rest, `*/` at end. Per Phase 1 Plan 05 STATE.

    2. Un-wrap the whole file (remove the `/* */` envelope):
       - Delete the opening `/*` line (likely right after imports + the TODO[scala3-port] tag)
       - Delete the closing `*/` line at file end
       - Do NOT remove the `// TODO[scala3-port]:` comment — keep it pointing at remaining runtime gaps

    3. For each test method (`test("...") { ... }` or `it should ...`):
       - If the test body uses ONLY type-level / implicit-resolution checks (e.g. `summon[FooMetadata[Bar]]`,
         `materialize[Foo]` at type position) → LEAVE LIVE
       - If the test body calls `AdtMetadataCompanion.materialize` AT RUNTIME (e.g. extracts fields from
         a returned `M[T]`) → the underlying `${ MetaMacros.dummy }` returns `'{ ??? }` → throws
         `NotImplementedError`. Mark these `pending`:
         ```scala
         test("structural derivation roundtrip") {
           pending  // MetaMacros.dummy ships '{ ??? } per fork; real reflection body in Phase 6
           // original assertions below...
         }
         ```
         Do NOT use `@nowarn`, `@ignore`, or comment-out — `pending` is the ScalaTest idiomatic deferral.

    4. Compile gate:
       ```bash
       sbt commons-core/Test/compile  # exit 0
       sbt scalafmtCheckAll  # exit 0
       ```

    5. Test run (catch any tests that accidentally invoke runtime macro paths without pending):
       ```bash
       sbt 'commons-core/testOnly *AdtMetadataTest' 2>&1 | tail -40
       ```
       Expect: all live tests pass; pending tests are listed as `pending` in output (NOT failed).
       If any test fails with `NotImplementedError` → add `pending` to that test.

    6. Commit:
       ```bash
       git add core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala
       git commit -m "test(scala-3,core): re-enable AdtMetadataTest (runtime cases pending)

Un-wrapped /* */ envelope from Phase 1 big-bang.

Test classification:
- Compile-time / implicit-resolution checks → live
- Runtime AdtMetadataCompanion.materialize calls → marked \`pending\` because
  MetaMacros.dummy ships '{ ??? } per fork (slice 4.3 Task 1). Real reflection
  body lands in Phase 6 with GenCodec derivation.

Per [[feedback_fix_dont_suppress_warnings]]: NO @nowarn / @ignore — pending is
the idiomatic deferral signal."
       ```
  </action>
  <verify>
    <automated>sbt 'commons-core/testOnly *AdtMetadataTest'</automated>
  </verify>
  <acceptance_criteria>
    - File no longer has `/* */` envelope wrapping the bulk
    - `grep -c 'pending' core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala` ≥ 1 (at least one runtime case deferred)
    - `grep -c '@nowarn\|@ignore' core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala` → 0
    - `sbt commons-core/Test/compile` exit 0
    - `sbt 'commons-core/testOnly *AdtMetadataTest'` exit 0 (live tests pass; pending tests reported as pending)
  </acceptance_criteria>
  <done>
    AdtMetadataTest.scala live; runtime macro-exercising tests use `pending`. No suppression annotations introduced.
  </done>
</task>

<task type="auto">
  <name>Task 3: MIGRATION.md updates — §3 bound tightening (source-compat) + §4 bincompat-narrowing</name>
  <read_first>
    - MIGRATION.md
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Per-Slice Recommendations" 4.4 MIGRATION.md updates + §"Divergences From Fork")
  </read_first>
  <action>
    Edit `MIGRATION.md`:

    1. §3 (Source-compat) — append:
       ```markdown
       ### core — meta AdtMetadataCompanion (slice 4.4)

       - `meta/AdtMetadataCompanion[M[_]]` → `meta/AdtMetadataCompanion[M[X] <: TypedMetadata[X]]` —
         type parameter bound TIGHTENED. Downstream consumers that built `M` not extending `TypedMetadata`
         will fail to compile. **Rationale (fork):** enables `inline given [T] => M[T] = materialize[T]`
         in `AdtMetadataCompanionMacros[M]` — the bound is required for the inherited inline given to
         resolve correctly.
       - File body collapses from 4-method stub (`materialize`, `fromApplyUnapplyProvider` × 2) to 2-line
         trait composition. The 4 methods are now inherited from `AdtMetadataCompanionMacros[M]` (in
         `MetaMacros.scala`, slice 4.3).
       - `BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]]` mirror — same collapse, same
         inheritance pattern.
       - Backlog rows for 4 `AdtMetadataCompanion.scala` Phase-1 TODOs removed.
       ```

    2. §4 (Bincompat) — add entry (or create §4 if not yet present):
       ```markdown
       ### core — slice 4.4 bincompat-narrowing

       - `AdtMetadataCompanion[M[_]]` → `AdtMetadataCompanion[M[X] <: TypedMetadata[X]]` is a
         **bound-tightening narrowing**. Old binaries compiled against `M extends TypedMetadata` keep
         working; old binaries with `M` NOT extending `TypedMetadata` fail to resolve the trait. MiMa
         will flag this once MiMa re-enables in Phase 11.
       ```

    3. Commit:
       ```bash
       git add MIGRATION.md
       git commit -m "docs(migration): record AdtMetadataCompanion bound tightening (slice 4.4)

§3 source-compat: M[_] → M[X] <: TypedMetadata[X] bound on AdtMetadataCompanion
+ BoundedAdtMetadataCompanion. Body collapses to 2-line trait composition (4
methods inherited from AdtMetadataCompanionMacros in MetaMacros.scala).

§4 bincompat-narrowing: bound tightening will be MiMa-flagged in Phase 11.

Phase 4 slice 4.4 of 4.1→4.2→4.3→4.4→4.5 stacked PR chain."
       ```
  </action>
  <verify>
    <automated>grep -c 'slice 4.4' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'slice 4.4' MIGRATION.md` ≥ 1
    - `grep -c 'TypedMetadata\[X\]' MIGRATION.md` ≥ 1
    - `grep -c 'bound tightening\|bound-tightening' MIGRATION.md` ≥ 1
    - §4 bincompat section exists with bound-tightening entry
  </acceptance_criteria>
  <done>
    MIGRATION.md §3 + §4 document the bound tightening + bincompat-narrowing.
  </done>
</task>

<task type="auto">
  <name>Task 4: Final acceptance gate + push 04-04-adt-metadata-companion + open stacked draft PR</name>
  <read_first>
    - .planning/phases/04-meta-derivation-core/04-03-meta-macros-PLAN.md (slice 4.3 PR number for Depends on)
  </read_first>
  <action>
    1. Acceptance gate:
       ```bash
       sbt commons-core/compile && sbt commons-core/Test/compile && sbt scalafmtCheckAll
       ```

    2. AdtMetadataCompanion shape parity probe — drop scratch test:
       ```scala
       // core/src/test/scala/_AdtMetadataShapeProbe.scala
       package com.avsystem.commons.meta
       trait MyAdtMeta[X] extends TypedMetadata[X]
       object MyAdtMetaCompanion extends AdtMetadataCompanion[MyAdtMeta]
       object _AdtMetadataShapeProbe {
         val _: MyAdtMetaCompanion.type = MyAdtMetaCompanion
         // negative check: a non-TypedMetadata M should fail — verified by removing this scratch
       }
       ```
       ```bash
       sbt commons-core/Test/compile  # must compile
       rm core/src/test/scala/_AdtMetadataShapeProbe.scala
       ```

    3. No new @nowarn/-Wconf:
       ```bash
       git diff 04-03-meta-macros..HEAD -- '*.scala' | grep -cE '^\+.*(@nowarn|-Wconf)'  # 0
       ```

    4. No `.planning/`:
       ```bash
       git log --name-only 04-03-meta-macros..HEAD | grep -c '^\.planning/'  # 0
       ```

    5. Push:
       ```bash
       git push -u origin 04-04-adt-metadata-companion
       ```

    6. Look up slice 4.3 PR:
       ```bash
       SLICE_43_PR=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:04-03-meta-macros --json number --jq '.[0].number')
       ```

    7. Open draft PR:
       ```bash
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:04-04-adt-metadata-companion \
         --draft \
         --title "[Scala 3] port AdtMetadataCompanion (M[X] <: TypedMetadata[X] bound)" \
         --body "$(cat <<EOF
**Slice:** 4.4 of Phase 4 (meta/ derivation core)
**Merge order:** 4.1 → 4.2 → 4.3 → 4.4 → 4.5
**Depends on:** #${SLICE_43_PR}
**Base branch:** 04-03-meta-macros (stacked) — re-base on upstream/scala-3 when prior slices merge

## Summary
- \`AdtMetadataCompanion[M[_]]\` → \`AdtMetadataCompanion[M[X] <: TypedMetadata[X]]\` — type parameter bound TIGHTENED per fork shape. **API-break for downstream that built M not extending TypedMetadata.**
- File body collapses from 4-method stub to 2-line trait composition — methods inherited from \`AdtMetadataCompanionMacros[M]\` (slice 4.3) + \`MetadataCompanion[M]\` (slice 4.3).
- \`BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]]\` mirror.
- \`AdtMetadataTest.scala\` un-wrapped — compile-time assertions live; runtime cases marked \`pending\` (MetaMacros.dummy ships \`'{ ??? }\` per fork).

## MIGRATION.md
- §3 source-compat: bound tightening + rationale (enables inline given inheritance)
- §4 bincompat: bound-tightening narrowing (will be MiMa-flagged in Phase 11)

## Acceptance
- \`sbt commons-core/compile + Test/compile + scalafmtCheckAll\` exit 0
- \`sbt 'commons-core/testOnly *AdtMetadataTest'\` exit 0 (live + pending)
- 0 new \`@nowarn\`/\`-Wconf\`
- 0 \`@nowarn\`/\`@ignore\` added to AdtMetadataTest (only \`pending\` per [[feedback_fix_dont_suppress_warnings]])
- Shape parity probe green (\`trait MyAdtMeta[X] extends TypedMetadata[X]\` resolves)
EOF
)"
       ```

    8. Set milestone:
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
       ```
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head halotukozak:04-04-adt-metadata-companion --json isDraft | grep -c '"isDraft": true'</automated>
  </verify>
  <acceptance_criteria>
    - All compile gates exit 0
    - Shape parity probe green
    - AdtMetadataTest passes (live + pending)
    - 0 new @nowarn/-Wconf
    - 0 .planning/ in diff
    - Draft PR open with stacked metadata block citing slice 4.3 PR
  </acceptance_criteria>
  <done>
    Slice 4.4 PR open. Slice 4.5 branches off `04-04-adt-metadata-companion`.
  </done>
</task>

</tasks>

<verification>
- AdtMetadataCompanion.scala matches fork — M[X] <: TypedMetadata[X] bound + 2-line trait composition
- BoundedAdtMetadataCompanion mirror present
- AdtMetadataTest un-wrapped; runtime cases use `pending`
- sbt commons-core/compile + Test/compile + scalafmtCheckAll exit 0
- 0 new @nowarn/-Wconf; 0 @nowarn/@ignore added to test
- MIGRATION.md §3 + §4 document bound tightening
- Draft PR open stacked on slice 4.3
</verification>

<success_criteria>
Slice 4.4 succeeds when:
1. AdtMetadataCompanion[M[X] <: TypedMetadata[X]] shape matches fork (parity probe)
2. AdtMetadataTest runs live (compile-time) + pending (runtime macro-deferred)
3. MIGRATION.md §3 + §4 document bound tightening
4. Draft PR opened with stacked metadata
</success_criteria>

<output>
After completion, create `.planning/phases/04-meta-derivation-core/04-04-SUMMARY.md`
</output>
