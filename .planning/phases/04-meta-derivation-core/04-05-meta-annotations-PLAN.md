---
phase: 04-meta-derivation-core
plan: 05
type: execute
wave: 5
depends_on:
  - 04-04-adt-metadata-companion
files_modified:
  - core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
  - core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala
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
    - "metaAnnotations.scala — `object infer { def value[T]: T = ??? }` swapped to `object infer extends InferMacros` (single-line surgical edit; everything else stays)"
    - "MacroInstancesTest.scala un-wrapped — compile-time tests live; runtime tests that exercise MetaMacros ??? marked `pending`"
    - "Final phase gate: `sbt compile + Test/compile + scalafmtCheckAll` exit 0 across all enabled modules (not just commons-core — entire phase gate)"
    - "No new @nowarn/-Wconf vs upstream/scala-3 across entire Phase 4 stack (slices 4.1–4.5 combined)"
    - "MIGRATION.md §3 documents infer.value inline-macro semantics + backlog row for metaAnnotations.scala:193 removed"
    - "Draft PR opened stacked on 04-04-adt-metadata-companion with [Scala 3] prefix + milestone 1 + Depends on slice 4.4 PR"
    - "Phase 4 closure recorded — full stack 4.1–4.5 PRs open, all draft, all milestone 1, all body-metadata-compliant"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala"
      provides: "Annotation hierarchy + object infer extends InferMacros"
      contains: "object infer extends InferMacros"
    - path: "core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala"
      provides: "Un-wrapped tests (compile-time live, runtime pending)"
      contains: "MacroInstancesTest"
    - path: "MIGRATION.md"
      provides: "§3 infer.value inline-macro entry + backlog row removed"
      contains: "infer.value"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala"
      to: "core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala"
      via: "object infer extends InferMacros"
      pattern: "extends InferMacros"
---

<objective>
Slice 4.5 — Final slice. Single-line surgical edit on `metaAnnotations.scala`: swap `object infer { def value[T]: T = ??? }`
to `object infer extends InferMacros`. Un-wrap `MacroInstancesTest.scala` (last remaining wrapped meta test). Close
Phase 4 with full-stack gate.

Purpose: Wire `metaAnnotations.infer.value` to the macro splice scaffolding from slice 4.3 (via `InferMacros` trait
in `MetaMacros.scala`). Although the underlying `MetaMacros.valueImpl` still ships `'{ ??? }` (fork debt), the
inline-call shape is now in place — Phase 6 will land the real body without touching call sites.

Output: 2 source files (1 surgical swap, 1 test un-wrap), MIGRATION.md update, draft PR stacked on slice 4.4,
Phase 4 closed.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/04-meta-derivation-core/04-CONTEXT.md
@.planning/phases/04-meta-derivation-core/04-RESEARCH.md
@core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
@core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
@core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala
@MIGRATION.md

<interfaces>
```scala
// origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metaAnnotations.scala — single change vs our tree
// (everything else in the ~330 LOC file matches our tree per RESEARCH Inventory)

// OUR TREE (Phase 1 stub at ~line 193):
object infer {
  def value[T]: T = ???
}

// FORK (slice 4.5 target):
object infer extends InferMacros
```

Dependency (must be on branch from slice 4.3):
- `InferMacros` trait — in `MetaMacros.scala` — provides `inline def value[T]: T = ${ MetaMacros.valueImpl[T] }`
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut 04-05-meta-annotations off 04-04-adt-metadata-companion; swap `object infer` to extend InferMacros</name>
  <read_first>
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metaAnnotations.scala (only need lines around `object infer`)
    - core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala (find current `object infer` at ~line 193)
    - core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala (verify `trait InferMacros` present from slice 4.3)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Per-Slice Recommendations" 4.5, Pitfall — intentional runtime ???)
  </read_first>
  <action>
    1. Branch:
       ```bash
       git checkout 04-04-adt-metadata-companion
       git checkout -b 04-05-meta-annotations
       ```

    2. Verify slice 4.3 InferMacros present:
       ```bash
       grep -c 'trait InferMacros' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # 1
       grep -c 'inline def value\[T\]' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # 1
       ```

    3. Locate current `object infer` in metaAnnotations.scala:
       ```bash
       grep -nE '^\s*object infer' core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
       ```

    4. Single-line surgical edit — swap `object infer { def value[T]: T = ??? }` (may span 3 lines) to
       `object infer extends InferMacros`. Use Edit tool with exact-match text.

       Before:
       ```scala
       object infer {
         def value[T]: T = ???
       }
       ```
       After:
       ```scala
       object infer extends InferMacros
       ```

       Verify nothing else changed in the file:
       ```bash
       git diff core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
       # MUST show exactly one hunk affecting `object infer` block — no other content
       ```

    5. Compile gate:
       ```bash
       sbt commons-core/compile  # exit 0 — InferMacros provides `inline def value[T]: T = ${ MetaMacros.valueImpl[T] }`
       sbt commons-core/Test/compile  # exit 0
       sbt scalafmtCheckAll  # exit 0
       ```

    6. Commit:
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
       git commit -m "feat(scala-3,core): port metaAnnotations real impls

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metaAnnotations.scala.

Surgical edit: object infer { def value[T]: T = ??? } → object infer extends InferMacros
(InferMacros provides inline def value[T]: T = \${ MetaMacros.valueImpl[T] } — landed
in slice 4.3 MetaMacros scaffolding).

NOTE: MetaMacros.valueImpl still ships '{ ??? } per fork (slice 4.3 Task 1). Call
sites of infer.value compile but throw NotImplementedError at runtime — INTENTIONAL
per the documented annotation-default-value pattern: infer.value is meant to be
consumed by annotation-processing macros (GenCodec in Phase 6); if a non-macro
caller invokes it, the runtime ??? fires correctly as an error signal."
       ```
  </action>
  <verify>
    <automated>sbt commons-core/compile && sbt commons-core/Test/compile</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'object infer extends InferMacros' core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala` → 1
    - `grep -c 'def value\[T\]: T = ???' core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala` → 0 (old stub gone)
    - `git diff HEAD~1 HEAD -- core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala | wc -l` < 15 (surgical edit only)
    - `sbt commons-core/compile + Test/compile + scalafmtCheckAll` exit 0
  </acceptance_criteria>
  <done>
    metaAnnotations.scala `object infer` extends `InferMacros` from slice 4.3; nothing else changed in the file.
  </done>
</task>

<task type="auto">
  <name>Task 2: Un-wrap MacroInstancesTest.scala — compile-time live, runtime pending</name>
  <read_first>
    - core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala (current — wrapped per Phase 1 Plan 05)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Test Un-wrapping Plan" — MacroInstancesTest in slice 4.5)
  </read_first>
  <action>
    1. Inspect current wrap state:
       ```bash
       head -20 core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala
       grep -nE '^\s*/\*|\*/\s*$' core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala
       ```

    2. Un-wrap `/* */` envelope.

    3. Per-test classification (same rules as slice 4.4 AdtMetadataTest):
       - Compile-time / implicit-resolution checks → live (with the new `Instances <: AnyNamedTuple` bound from slice 4.2, classical-trait `Instances` will fail to compile — REWRITE those Instances declarations as named-tuple type aliases per slice 4.2 MIGRATION.md guidance)
       - Runtime calls into `MacroInstances.materialize(implicits, companion).field` that exercise the body → these go through `materializeInstances` which is REAL (slice 4.2 inline impl) — but if the test passes a companion that triggers `AdtMetadataCompanion.materialize` → that's still `???`. Mark such tests `pending`.

    4. Compile gate:
       ```bash
       sbt commons-core/Test/compile  # exit 0
       sbt scalafmtCheckAll  # exit 0
       ```

    5. Test run:
       ```bash
       sbt 'commons-core/testOnly *MacroInstancesTest' 2>&1 | tail -40
       ```
       Live tests pass; pending tests reported as pending.

    6. Commit:
       ```bash
       git add core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala
       git commit -m "test(scala-3,core): re-enable MacroInstancesTest (runtime macro-deferred cases pending)

Un-wrapped /* */ envelope from Phase 1 big-bang.

Reshape: classical-trait Instances declarations rewritten as named-tuple type
aliases per slice 4.2 Instances <: AnyNamedTuple bound (MIGRATION.md §3).

Pending: tests that route through AdtMetadataCompanion.materialize hit
MetaMacros.dummy ??? per fork — deferred to Phase 6. Per
[[feedback_fix_dont_suppress_warnings]]: pending, NOT @nowarn / @ignore."
       ```
  </action>
  <verify>
    <automated>sbt 'commons-core/testOnly *MacroInstancesTest'</automated>
  </verify>
  <acceptance_criteria>
    - File no longer has `/* */` envelope
    - `grep -c '@nowarn\|@ignore' core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala` → 0
    - `grep -c 'pending' core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala` ≥ 1 (some test deferred)
    - `sbt 'commons-core/testOnly *MacroInstancesTest'` exit 0
  </acceptance_criteria>
  <done>
    MacroInstancesTest live with `Instances` reshapes; runtime macro-deferred cases use `pending`.
  </done>
</task>

<task type="auto">
  <name>Task 3: Update MIGRATION.md + run Phase 4 closure gate</name>
  <read_first>
    - MIGRATION.md (find `metaAnnotations.scala:193` backlog row)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Per-Slice Recommendations" 4.5 MIGRATION.md updates, §"Validation Architecture" — Sampling Rate)
    - .planning/phases/04-meta-derivation-core/04-VALIDATION.md (per-slice 4.5 gates + full-phase gate)
  </read_first>
  <action>
    PART A — MIGRATION.md updates:

    1. §3 (Source-compat) — append:
       ```markdown
       ### core — meta metaAnnotations (slice 4.5)

       - `meta/metaAnnotations.object infer` is now `extends InferMacros` (was: `{ def value[T]: T = ??? }`).
         `inline def value[T]: T = ${ MetaMacros.valueImpl[T] }` — runtime semantics preserve the "use only inside
         macro-consumed annotations" contract: `MetaMacros.valueImpl` ships `'{ ??? }` per fork (slice 4.3), so
         non-macro callers of `infer.value` will throw `NotImplementedError`. This is **intentional** —
         `infer.value` is meant to be replaced by the macro that consumes the surrounding annotation; if it
         survives to runtime, the throw is a correct error signal.
       ```

    2. Backlog table — remove the `metaAnnotations.scala:193` row (Phase-1 TODO resolved):
       ```bash
       grep -n 'metaAnnotations.scala:193' MIGRATION.md
       # Use Edit tool to remove the exact row
       ```

    3. Commit MIGRATION.md:
       ```bash
       git add MIGRATION.md
       git commit -m "docs(migration): record meta/ derivation core port (API shape shifts)

§3 source-compat: metaAnnotations.infer now extends InferMacros (slice 4.5).
inline def value[T] splice landed; underlying MetaMacros.valueImpl body still '{ ??? }
per fork (slice 4.3 — Phase 6 deferral). Runtime ??? in non-macro callers is
intentional.

Backlog cleanup: metaAnnotations.scala:193 TODO[scala3-port] tag resolved.

Phase 4 final slice (4.5 of 4.1→4.2→4.3→4.4→4.5 stacked PR chain)."
       ```

    PART B — Phase 4 closure gate (broader than per-slice gates — exercises all modules that depend on meta/):

    1. Full module gate:
       ```bash
       sbt clean
       sbt compile           # all enabled modules — exit 0
       sbt Test/compile      # all enabled test sources — exit 0
       sbt scalafmtCheckAll  # exit 0
       ```

       If any module breaks → likely a downstream consumer depending on the old `M[_]` shape (slice 4.4 bound) or
       old classical-trait `Instances` (slice 4.2 bound). Fix:
       - Reshape downstream `M` to extend `TypedMetadata`
       - Reshape downstream `Instances` traits to named-tuple type aliases
       - If a module is too broken to fix in Phase 4 (e.g. rpc per phase boundary), add `// TODO[scala3-port]:
         bound shifted in Phase 4 — reshape in Phase 7/9` and DOCUMENT in MIGRATION.md backlog
       - Each fix is a separate fork-cadence commit on this branch

    2. No new `@nowarn` / `-Wconf`:
       ```bash
       git diff upstream/scala-3..HEAD -- '*.scala' | grep -cE '^\+.*(@nowarn|-Wconf)'  # 0
       ```

    3. No `.planning/`:
       ```bash
       git log --name-only upstream/scala-3..HEAD | grep -c '^\.planning/'  # 0
       ```

    4. Fork-shape parity across all 9 ported files:
       ```bash
       for f in AllowDerivation Fallback OptionLike metadata MacroInstances MetaMacros MetadataCompanion AdtMetadataCompanion metaAnnotations; do
         echo "=== $f ==="
         forkCount=$(git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/$f.scala 2>/dev/null | grep -cE '^(sealed|case|object|trait|class|given|inline|def) ')
         ourCount=$(grep -cE '^(sealed|case|object|trait|class|given|inline|def) ' core/src/main/scala/com/avsystem/commons/meta/$f.scala)
         echo "fork=$forkCount ours=$ourCount"
       done
       ```
       Expected: counts match (or differ by exactly the documented divergences — OptionLike's BaseOptionLike shim;
       metaAnnotations has same count, just `object infer extends InferMacros` shape).
  </action>
  <verify>
    <automated>grep -c 'slice 4.5' MIGRATION.md && sbt compile && sbt Test/compile && sbt scalafmtCheckAll</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'slice 4.5' MIGRATION.md` ≥ 1
    - `grep -c 'metaAnnotations.scala:193' MIGRATION.md` → 0 (backlog row removed)
    - `grep -c 'infer.value\|extends InferMacros' MIGRATION.md` ≥ 1
    - `sbt compile` exit 0 across all enabled modules
    - `sbt Test/compile` exit 0 across all enabled test sources
    - `sbt scalafmtCheckAll` exit 0
    - 0 new `@nowarn` / `-Wconf` vs upstream/scala-3 across all of Phase 4 (slices 4.1–4.5 combined)
    - 0 `.planning/` paths in any commit on Phase 4 branches
    - Fork-shape parity confirmed per per-file grep counts
  </acceptance_criteria>
  <done>
    MIGRATION.md §3 documents infer.value inline-macro shape + Phase-1 backlog row removed.
    Phase 4 full gate green. All 9 fork files ported (5 verbatim + 4 with documented divergences).
  </done>
</task>

<task type="auto">
  <name>Task 4: Push 04-05-meta-annotations + open final stacked draft PR + record Phase 4 closure</name>
  <read_first>
    - .planning/phases/04-meta-derivation-core/04-04-adt-metadata-companion-PLAN.md (slice 4.4 PR number)
  </read_first>
  <action>
    1. Push:
       ```bash
       git push -u origin 04-05-meta-annotations
       ```

    2. Look up slice 4.4 PR:
       ```bash
       SLICE_44_PR=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:04-04-adt-metadata-companion --json number --jq '.[0].number')
       ```

    3. Open draft PR:
       ```bash
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:04-05-meta-annotations \
         --draft \
         --title "[Scala 3] port metaAnnotations.infer (extends InferMacros)" \
         --body "$(cat <<EOF
**Slice:** 4.5 of Phase 4 (meta/ derivation core)
**Merge order:** 4.1 → 4.2 → 4.3 → 4.4 → 4.5
**Depends on:** #${SLICE_44_PR}
**Base branch:** 04-04-adt-metadata-companion (stacked) — re-base on upstream/scala-3 when prior slices merge

## Summary
Single-line surgical edit on \`metaAnnotations.scala\`:
- \`object infer { def value[T]: T = ??? }\` → \`object infer extends InferMacros\`

\`InferMacros\` (slice 4.3 MetaMacros) provides \`inline def value[T]: T = \${ MetaMacros.valueImpl[T] }\`. The macro splice body still ships \`'{ ??? }\` per fork; runtime semantics preserve the "use only inside macro-consumed annotations" contract.

\`MacroInstancesTest.scala\` un-wrapped — last remaining wrapped meta test from Phase 1 big-bang. Runtime macro-deferred cases use \`pending\`.

## MIGRATION.md
- §3 source-compat: infer.value now inline macro call
- Backlog row \`metaAnnotations.scala:193\` removed

## Phase 4 closure
This PR closes the Phase 4 stack (4.1 → 4.2 → 4.3 → 4.4 → 4.5). All 9 fork files at \`origin/master:core/src/main/scala-3/com/avsystem/commons/meta/\` are now ported (5 verbatim + 4 with documented divergences in MIGRATION.md §3/§4).

\`MetaMacros.{valueImpl, lazyMetadataImpl, dummy}\` real reflection bodies remain deferred to Phase 6 per fork-shipped staging (documented in MIGRATION.md §1).

## Acceptance (full phase gate)
- \`sbt compile + Test/compile + scalafmtCheckAll\` exit 0 across all enabled modules
- 0 new \`@nowarn\`/\`-Wconf\` across all of Phase 4
- 0 \`.planning/\` in any commit
- Fork-shape parity confirmed per per-file grep counts

Translated from \`origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metaAnnotations.scala\` per [[feedback_crib_from_master]].
EOF
)"
       ```

    4. Set milestone:
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
       ```

    5. Verify Phase 4 PR stack — all 5 should be draft, milestone 1, [Scala 3] prefix:
       ```bash
       for slice in 01-foundation 02-macro-instances 03-meta-macros 04-adt-metadata-companion 05-meta-annotations; do
         echo "=== $slice ==="
         gh pr list --repo AVSystem/scala-commons --head halotukozak:04-$slice \
           --json number,title,isDraft,milestone --jq '.[0] | {num: .number, title: .title, draft: .isDraft, milestone: .milestone.number}'
       done
       ```
       Expected: 5 entries, all `draft=true`, all `milestone=1`, all titles starting with `[Scala 3]`.

    6. Phase 4 closure recorded — orchestrator will pick this up via STATE.md update.
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head halotukozak:04-05-meta-annotations --json isDraft | grep -c '"isDraft": true'</automated>
  </verify>
  <acceptance_criteria>
    - Branch `04-05-meta-annotations` pushed to fork
    - Draft PR open against `upstream/scala-3` with body metadata block citing slice 4.4 PR as Depends on
    - Milestone 1 assigned
    - All 5 Phase 4 PRs visible in stack — each draft, milestone 1, `[Scala 3]` title prefix
    - Phase 4 PR chain complete and ready for sequential maintainer merge (4.1 → 4.2 → 4.3 → 4.4 → 4.5)
  </acceptance_criteria>
  <done>
    Phase 4 closed under Claude scope. Five stacked draft PRs open, all body-metadata-compliant, awaiting manual maintainer review.
  </done>
</task>

</tasks>

<verification>
- metaAnnotations.scala: `object infer extends InferMacros` (surgical swap)
- MacroInstancesTest.scala un-wrapped with `pending` markers for runtime macro-deferred cases
- Phase gate: sbt compile + Test/compile + scalafmtCheckAll exit 0 across all enabled modules
- 0 new @nowarn/-Wconf across entire Phase 4 stack
- MIGRATION.md §3 documents infer.value semantics; Phase-1 backlog row metaAnnotations.scala:193 removed
- All 5 Phase 4 PRs open, draft, milestone 1, [Scala 3] prefix, body metadata block compliant
- Slice 4.5 PR opened stacked on 04-04-adt-metadata-companion with Depends on slice 4.4 PR
</verification>

<success_criteria>
Slice 4.5 / Phase 4 succeeds when:
1. `object infer extends InferMacros` (single-line swap committed)
2. `MacroInstancesTest` un-wrapped + runtime macro cases pending (NOT @nowarn / @ignore)
3. Full phase gate: `sbt compile + Test/compile + scalafmtCheckAll` exit 0 across all enabled modules
4. All 5 Phase 4 PRs (4.1–4.5) open as drafts on AVSystem/scala-commons, milestone 1, [Scala 3] prefix, sequential body-metadata Depends-on chain
5. MIGRATION.md final state reflects: §1 MetaMacros bodies deferred to Phase 6, §3 entries for slices 4.1/4.2/4.3/4.4/4.5, §4 bincompat narrowing on slice 4.4, Phase-1 backlog rows for ported meta/* TODOs removed
</success_criteria>

<output>
After completion, create `.planning/phases/04-meta-derivation-core/04-05-SUMMARY.md`
</output>
