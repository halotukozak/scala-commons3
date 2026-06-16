---
phase: 04-meta-derivation-core
plan: 03
type: execute
wave: 3
depends_on:
  - 04-02-macro-instances
files_modified:
  - core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
  - core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
  - MIGRATION.md
autonomous: true
requirements:
  - META-CORE-02
  - META-CORE-04
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
    - "MetaMacros.scala (new file) exists with 7 traits (InferMacros, AdtMetadataCompanionMacros, BoundedAdtMetadataCompanionMacros, MetadataCompanionMacros, BoundedMetadataCompanionMacros, MetadataCompanionLazyMacros, BoundedMetadataCompanionLazyMacros) + object MetaMacros with valueImpl/lazyMetadataImpl/dummy"
    - "All three macro impl bodies (valueImpl, lazyMetadataImpl, dummy) retain fork's '{ ??? } placeholders — real bodies deferred to Phase 6"
    - "MetadataCompanion.scala rewritten to fork shape — given fromFallback [Real] => (fallback: Fallback[M[Real]]) => M[Real]; Lazy object extends MetadataCompanionLazyMacros[M, Lazy]; BoundedMetadataCompanion likewise"
    - "sbt commons-core/compile + commons-core/Test/compile + scalafmtCheckAll exit 0"
    - "MetadataCompanion.lazyMetadata macro splice compiles (body remains '{ ??? } per fork)"
    - "No new @nowarn/-Wconf"
    - "MIGRATION.md §1 declares Will-Not-Migrate-THIS-PHASE: MetaMacros real bodies (deferred to Phase 6) + §3 MetadataCompanion API shape shift"
    - "Draft PR opened against base = 04-02-macro-instances branch (stacked) with [Scala 3] prefix + milestone 1 + body cites slice 4.2 PR as Depends on"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala"
      provides: "7 macro-trait scaffolds + object MetaMacros companion with ??? splice bodies"
      contains: "trait InferMacros"
    - path: "core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala"
      provides: "MetadataCompanion[M[_]] + BoundedMetadataCompanion + Lazy nested companion"
      contains: "given fromFallback"
    - path: "MIGRATION.md"
      provides: "§1 MetaMacros bodies deferred entry + §3 MetadataCompanion API shape shift"
      contains: "MetaMacros"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala"
      to: "core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala"
      via: "object Lazy extends MetadataCompanionLazyMacros[M, Lazy]"
      pattern: "MetadataCompanionLazyMacros"
    - from: "core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala"
      to: "core/src/main/scala/com/avsystem/commons/meta/Fallback.scala"
      via: "given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real]"
      pattern: "Fallback\\[M\\[Real\\]\\]"
    - from: "core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala"
      to: "core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala"
      via: "given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]]"
      pattern: "ImplicitNotFound"
    - from: "core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala"
      to: "scala.quoted.*"
      via: "import scala.quoted.*"
      pattern: "scala\\.quoted"
---

<objective>
Slice 4.3 — Port `MetaMacros.scala` (new file, 7 macro-trait scaffolds + companion object with three `'{ ??? }`
placeholder splice impls) AND rewrite `MetadataCompanion.scala` to fork's polymorphic-context-function-given shape.

Purpose: Land the macro-quote SCAFFOLDING that slices 4.4 (AdtMetadataCompanion) and 4.5 (metaAnnotations.infer)
build on top of. Real macro reflection bodies are explicitly deferred to Phase 6 per fork-shipped technical debt
(documented in MIGRATION.md §1).

Output: 2 source files (1 new, 1 rewritten), MIGRATION.md updates (§1 deferral + §3 shape shift), branch
`04-03-meta-macros` stacked on `04-02-macro-instances`, draft PR opened.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/04-meta-derivation-core/04-CONTEXT.md
@.planning/phases/04-meta-derivation-core/04-RESEARCH.md
@core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
@core/src/main/scala/com/avsystem/commons/meta/Fallback.scala
@core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala
@MIGRATION.md

<interfaces>
<!-- Fork canonical sources — git show before each port. -->

```scala
// origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala — verbatim
package com.avsystem.commons.meta

import scala.quoted.*

trait InferMacros {
  inline def value[T]: T = ${ MetaMacros.valueImpl[T] }
}

trait AdtMetadataCompanionMacros[M[_]] {
  inline def materialize[T]: M[T] = ${ MetaMacros.dummy }
  inline given [T] => M[T] = materialize[T]
  inline def fromApplyUnapplyProvider[T](inline applyUnapplyProvider: Any): M[T] =
    ${ MetaMacros.dummy }
}

// BoundedAdtMetadataCompanionMacros[Hi, Lo, M], MetadataCompanionMacros[M], BoundedMetadataCompanionMacros[Hi, Lo, M],
// MetadataCompanionLazyMacros[M, Lazy], BoundedMetadataCompanionLazyMacros[Hi, Lo, M, Lazy] — see fork file

object MetaMacros {
  def valueImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def lazyMetadataImpl(using Quotes): Expr[Nothing] = '{ ??? }
  def dummy(using Quotes): Expr[Nothing] = '{ ??? }
}
```

```scala
// origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetadataCompanion.scala — verbatim shape
trait MetadataCompanion[M[_]] {
  given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value
  final def apply[Real](using metadata: M[Real]): M[Real] = metadata
  final class Lazy[Real](metadata: => M[Real]) {
    lazy val value: M[Real] = metadata
  }
  object Lazy extends MetadataCompanionLazyMacros[M, Lazy] {
    @implicitNotFound("#{forNotLazy}")
    given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]] =
      ImplicitNotFound()
    def apply[Real](metadata: => M[Real]): Lazy[Real] = new Lazy(metadata)
  }
}

trait BoundedMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] { /* analogous shape */ }
```
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut 04-03-meta-macros off 04-02-macro-instances; port MetaMacros.scala verbatim from fork</name>
  <read_first>
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala (~46 LOC)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Missing Files in Our Tree" #2, §"Per-Slice Recommendations" 4.3, Pitfalls — fork ships ??? bodies)
  </read_first>
  <action>
    1. Branch:
       ```bash
       git checkout 04-02-macro-instances
       git checkout -b 04-03-meta-macros
       ```

    2. Port MetaMacros.scala verbatim:
       ```bash
       git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala \
         > core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
       ```

    3. Verify ALL 7 traits + object MetaMacros present:
       ```bash
       grep -cE '^trait (InferMacros|AdtMetadataCompanionMacros|BoundedAdtMetadataCompanionMacros|MetadataCompanionMacros|BoundedMetadataCompanionMacros|MetadataCompanionLazyMacros|BoundedMetadataCompanionLazyMacros)' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
       # MUST be 7
       grep -c '^object MetaMacros' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # 1
       grep -c "'{ ??? }" core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # ≥ 3
       grep -c 'def valueImpl' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # 1
       grep -c 'def lazyMetadataImpl' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # 1
       grep -c 'def dummy' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # 1
       ```

    4. Compile (this slice introduces `import scala.quoted.*` for the first time — verify):
       ```bash
       sbt commons-core/compile  # exit 0
       sbt scalafmtCheckAll      # exit 0
       ```

    5. Commit (fork cadence — explicit note about ??? bodies):
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
       git commit -m "feat(scala-3,core): port MetaMacros (scaffolding — bodies ??? per fork, real impl deferred to Phase 6)

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala.

7 macro-trait scaffolds:
- InferMacros (consumed by metaAnnotations.infer in slice 4.5)
- AdtMetadataCompanionMacros[M] (consumed by AdtMetadataCompanion in slice 4.4)
- BoundedAdtMetadataCompanionMacros[Hi, Lo, M]
- MetadataCompanionMacros[M] (defined; currently no consumer)
- BoundedMetadataCompanionMacros[Hi, Lo, M]
- MetadataCompanionLazyMacros[M, Lazy] (consumed by MetadataCompanion.Lazy)
- BoundedMetadataCompanionLazyMacros[Hi, Lo, M, Lazy]

Companion object MetaMacros ships three '{ ??? } placeholder splice impls verbatim
from fork:
- valueImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
- lazyMetadataImpl(using Quotes): Expr[Nothing] = '{ ??? }
- dummy(using Quotes): Expr[Nothing] = '{ ??? }

Fork-shipped technical debt — real reflection-based bodies deferred to Phase 6
(see MIGRATION.md §1 entry — separate commit). Call sites of downstream
materialize APIs compile but throw NotImplementedError at runtime; this is
intentional until Phase 6 lands GenCodec derivation."
       ```
  </action>
  <verify>
    <automated>sbt commons-core/compile</automated>
  </verify>
  <acceptance_criteria>
    - 7 macro traits + object MetaMacros + 3 ??? bodies present (greps above all match required counts)
    - `sbt commons-core/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
    - Commit subject mentions "??? per fork, real impl deferred to Phase 6"
  </acceptance_criteria>
  <done>
    MetaMacros.scala new file landed verbatim from fork (including the ??? bodies). Compile + scalafmt green.
  </done>
</task>

<task type="auto">
  <name>Task 2: Rewrite MetadataCompanion.scala to fork's polymorphic-context-function-given shape</name>
  <read_first>
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetadataCompanion.scala (~36 LOC)
    - core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala (current Phase-1 stub)
    - core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala (verify exists — was extracted in slice 3.5 per STATE)
    - core/src/main/scala/com/avsystem/commons/meta/Fallback.scala (must exist from slice 4.1)
    - core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala (must exist from Task 1)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Code Examples — MetadataCompanion", Pitfalls 1/2)
  </read_first>
  <action>
    1. Verify dependencies present on branch:
       ```bash
       test -f core/src/main/scala/com/avsystem/commons/meta/Fallback.scala
       test -f core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
       test -f core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala
       grep -c 'MetadataCompanionLazyMacros' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # ≥ 1
       grep -c 'BoundedMetadataCompanionLazyMacros' core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala  # ≥ 1
       ```

    2. Port MetadataCompanion.scala verbatim from fork:
       ```bash
       git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetadataCompanion.scala \
         > core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
       ```

    3. Verify required signatures (per fork shape):
       ```bash
       grep -c 'trait MetadataCompanion\[M\[_\]\]' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # 1
       grep -cE 'given fromFallback:.*\[Real\] => .*Fallback\[M\[Real\]\].* => M\[Real\]' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # 1
       grep -c 'final class Lazy\[Real\]' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # 1
       grep -c 'object Lazy extends MetadataCompanionLazyMacros\[M, Lazy\]' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # 1
       grep -c 'trait BoundedMetadataCompanion\[Hi, Lo <: Hi, M\[_ >: Lo <: Hi\]\]' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # 1
       grep -c 'BoundedMetadataCompanionLazyMacros' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # ≥ 1
       grep -c '@implicitNotFound' core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala  # ≥ 1
       ```

    4. Compile gate — confirm slice-3.3-precedent polymorphic-context-function-givens still parse:
       ```bash
       sbt commons-core/compile        # exit 0
       sbt commons-core/Test/compile   # exit 0
       sbt scalafmtCheckAll            # exit 0
       ```
       If compile fails due to `ImplicitNotFound` import path or `@implicitNotFound` annotation:
       - Check imports — fork may import `scala.annotation.implicitNotFound` and our `ImplicitNotFound` (different things)
       - Adjust imports to match fork verbatim; do NOT delete the `@implicitNotFound` annotation

    5. Commit:
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
       git commit -m "feat(scala-3,core): port MetadataCompanion + BoundedMetadataCompanion

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetadataCompanion.scala.

Replaces Phase-1 ??? stub with fork's polymorphic-context-function-given shape:
- given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value
- final class Lazy[Real](metadata: => M[Real]) — lazy reference container
- object Lazy extends MetadataCompanionLazyMacros[M, Lazy] — inline-given derivation entry
- given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]]
  with @implicitNotFound message interpolation
- trait BoundedMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] mirror shape

Macro splice MetaMacros.lazyMetadataImpl still ships '{ ??? } per fork (slice 4.3
Task 1) — call sites compile but throw at runtime until Phase 6 lands the real
reflection body."
       ```
  </action>
  <verify>
    <automated>sbt commons-core/compile && sbt commons-core/Test/compile</automated>
  </verify>
  <acceptance_criteria>
    - All 7 grep checks above return required counts
    - `sbt commons-core/compile` exit 0
    - `sbt commons-core/Test/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
    - File diff against fork is byte-exact or whitespace-only
  </acceptance_criteria>
  <done>
    MetadataCompanion.scala matches fork verbatim with fromFallback given, Lazy nested companion, BoundedMetadataCompanion mirror, and @implicitNotFound notFound given.
  </done>
</task>

<task type="auto">
  <name>Task 3: MIGRATION.md updates — §1 MetaMacros bodies deferred + §3 MetadataCompanion shape shift</name>
  <read_first>
    - MIGRATION.md
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Per-Slice Recommendations" 4.3 MIGRATION.md updates, Pitfall 2)
  </read_first>
  <action>
    Edit `MIGRATION.md`:

    1. §1 (Will Not Migrate) — add entry pinning the fork-shipped ??? bodies (this is "will not migrate IN THIS PHASE"; consider it a temporary entry pending Phase 6):
       ```markdown
       | `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` real bodies | Fork ships `'{ ??? }` placeholders; the macro-quote SCAFFOLDING is ported in Phase 4 (slice 4.3) but real reflection-based derivation bodies are deferred to Phase 6. Downstream call sites of `MetadataCompanion.Lazy.lazyMetadata` / `AdtMetadataCompanion.materialize` / `metaAnnotations.infer.value` compile cleanly but throw `NotImplementedError` at runtime. **Intentional** — these APIs are meant to be inlined-away by upstream macros (GenCodec in Phase 6); if a runtime caller hits the `???`, it's a usage bug, not a regression. |
       ```

    2. §3 (Source-compat) — append under `### core — meta MacroInstances (slice 4.2)`:
       ```markdown
       ### core — meta MetaMacros + MetadataCompanion (slice 4.3)

       - **NEW FILE** `meta/MetaMacros.scala` — 7 macro-trait scaffolds + object MetaMacros companion. Imports `scala.quoted.*` for the first time in the meta layer. Three splice impls (`valueImpl`, `lazyMetadataImpl`, `dummy`) ship `'{ ??? }` bodies per fork (see §1 entry).
       - `meta/MetadataCompanion` reshaped:
         - `def fromFallback[T](fallback: => Fallback[M[T]]): M[T]` → `given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real] = fallback.value` (polymorphic context-function given; same idiom as slice 3.3 precedent)
         - `Lazy` nested companion now `extends MetadataCompanionLazyMacros[M, Lazy]` — inherits `inline given lazyMetadata` macro splice
         - Added `given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]]` with `@implicitNotFound("#{forNotLazy}")` interpolation
         - `trait BoundedMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]]` mirror shape added (Phase 1 stub had bound + body methods returning `???`; reshaped to match fork)
       - Backlog rows for `MetadataCompanion.scala:27` and `:58` removed (Phase-1 TODO tags resolved).
       ```

    3. Commit:
       ```bash
       git add MIGRATION.md
       git commit -m "docs(migration): record MetaMacros scaffolding + MetadataCompanion shape (slice 4.3)

§1 Will-Not-Migrate-THIS-PHASE: MetaMacros real bodies (valueImpl, lazyMetadataImpl, dummy)
deferred to Phase 6. Scaffolding ported; bodies remain '{ ??? } per fork.

§3 source-compat: MetadataCompanion polymorphic context-function givens (fromFallback,
notFound), Lazy extends MetadataCompanionLazyMacros, BoundedMetadataCompanion mirror.

Phase 4 slice 4.3 of 4.1→4.2→4.3→4.4→4.5 stacked PR chain."
       ```
  </action>
  <verify>
    <automated>grep -c 'slice 4.3' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'slice 4.3' MIGRATION.md` ≥ 1
    - `grep -c 'MetaMacros' MIGRATION.md` ≥ 2 (§1 + §3 entries)
    - `grep -c 'lazyMetadataImpl' MIGRATION.md` ≥ 1
    - `grep -c 'fromFallback' MIGRATION.md` ≥ 1
  </acceptance_criteria>
  <done>
    MIGRATION.md §1 declares MetaMacros bodies deferred; §3 documents MetadataCompanion reshape.
  </done>
</task>

<task type="auto">
  <name>Task 4: Final acceptance gate + push 04-03-meta-macros + open stacked draft PR</name>
  <read_first>
    - .planning/phases/04-meta-derivation-core/04-02-macro-instances-PLAN.md (slice 4.2 PR number for "Depends on")
  </read_first>
  <action>
    1. Acceptance gate:
       ```bash
       sbt commons-core/compile && sbt commons-core/Test/compile && sbt scalafmtCheckAll
       ```

    2. Lazy-metadata splice compile sanity probe — drop scratch:
       ```scala
       // core/src/test/scala/_LazyMetadataSpliceProbe.scala
       package com.avsystem.commons.meta
       trait MyMeta[T]
       object MyMetaCompanion extends MetadataCompanion[MyMeta]
       object _LazyMetadataSpliceProbe {
         summon[MyMetaCompanion.Lazy[String] => Unit]  // compile-only — body throws at runtime, intentional
       }
       ```
       ```bash
       sbt commons-core/Test/compile  # must compile
       rm core/src/test/scala/_LazyMetadataSpliceProbe.scala
       ```

    3. No new @nowarn/-Wconf:
       ```bash
       git diff 04-02-macro-instances..HEAD -- '*.scala' | grep -cE '^\+.*(@nowarn|-Wconf)'  # 0
       ```

    4. No `.planning/`:
       ```bash
       git log --name-only 04-02-macro-instances..HEAD | grep -c '^\.planning/'  # 0
       ```

    5. Push:
       ```bash
       git push -u origin 04-03-meta-macros
       ```

    6. Look up slice 4.2 PR:
       ```bash
       SLICE_42_PR=$(gh pr list --repo AVSystem/scala-commons --head halotukozak:04-02-macro-instances --json number --jq '.[0].number')
       ```

    7. Open draft PR:
       ```bash
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:04-03-meta-macros \
         --draft \
         --title "[Scala 3] port MetaMacros + MetadataCompanion (scaffolding, bodies deferred)" \
         --body "$(cat <<EOF
**Slice:** 4.3 of Phase 4 (meta/ derivation core)
**Merge order:** 4.1 → 4.2 → 4.3 → 4.4 → 4.5
**Depends on:** #${SLICE_42_PR}
**Base branch:** 04-02-macro-instances (stacked) — re-base on upstream/scala-3 when prior slices merge

## Summary
- **NEW FILE** \`meta/MetaMacros.scala\` (verbatim from fork) — 7 macro-trait scaffolds + object MetaMacros companion with three \`'{ ??? }\` splice impls. **Real bodies deferred to Phase 6 per fork-shipped staging.**
- \`meta/MetadataCompanion.scala\` rewritten to fork's polymorphic-context-function-given shape — \`given fromFallback\`, \`Lazy extends MetadataCompanionLazyMacros\`, \`given notFound\` with \`@implicitNotFound\`, \`BoundedMetadataCompanion\` mirror.

## Fork-shipped technical debt
\`MetaMacros.{valueImpl, lazyMetadataImpl, dummy}\` bodies remain \`'{ ??? }\` placeholders per upstream fork. Downstream call sites of \`Lazy.lazyMetadata\` / \`AdtMetadataCompanion.materialize\` / \`infer.value\` (slices 4.4/4.5) compile but throw \`NotImplementedError\` at runtime. **Intentional** — these APIs are inlined-away by upstream macros at consumer sites; runtime hits are usage bugs.

## MIGRATION.md
- §1 Will-Not-Migrate-THIS-PHASE: MetaMacros real bodies (deferred to Phase 6)
- §3: MetadataCompanion polymorphic context-function givens + Lazy shape

## Acceptance
- \`sbt commons-core/compile + Test/compile + scalafmtCheckAll\` exit 0
- 0 new \`@nowarn\`/\`-Wconf\`
- Lazy-metadata splice compile probe green
EOF
)"
       ```

    8. Set milestone 1:
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
       ```
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head halotukozak:04-03-meta-macros --json isDraft | grep -c '"isDraft": true'</automated>
  </verify>
  <acceptance_criteria>
    - All compile gates exit 0
    - Lazy-metadata splice probe green
    - 0 new @nowarn/-Wconf
    - 0 .planning/ in commits
    - Draft PR open with [Scala 3] prefix, milestone 1, body cites slice 4.2 PR as Depends on
  </acceptance_criteria>
  <done>
    Slice 4.3 PR open. Slice 4.4 branches off `04-03-meta-macros`.
  </done>
</task>

</tasks>

<verification>
- MetaMacros.scala new file with 7 traits + 3 ??? splice impls
- MetadataCompanion.scala rewritten — fromFallback, Lazy, BoundedMetadataCompanion, notFound
- sbt commons-core/compile + Test/compile + scalafmtCheckAll exit 0
- 0 new @nowarn/-Wconf
- MIGRATION.md §1 deferral + §3 shape shift documented
- Draft PR open on AVSystem/scala-commons stacked on slice 4.2
</verification>

<success_criteria>
Slice 4.3 succeeds when:
1. MetaMacros.scala compiles (with ??? bodies preserved per fork)
2. MetadataCompanion.lazyMetadata macro splice compiles (body throws at runtime — intentional)
3. MIGRATION.md §1 declares MetaMacros bodies deferred + §3 documents shape shift
4. Draft PR opened against upstream/scala-3 with metadata block citing slice 4.2 PR
</success_criteria>

<output>
After completion, create `.planning/phases/04-meta-derivation-core/04-03-SUMMARY.md`
</output>
