---
phase: 04-meta-derivation-core
plan: 03
subsystem: core/meta
tags: [scala-3, meta-macros, macro-quotes, polymorphic-context-function-given, fork-deferred-bodies]
dependency-graph:
  requires: [04-01-foundation, 04-02-macro-instances]
  provides:
    - MetaMacros.scala scaffolding (7 macro-trait stubs + object MetaMacros with '{ ??? } splice impls)
    - MetadataCompanion + BoundedMetadataCompanion fork-shape (fromFallback given, Lazy extends MetadataCompanionLazyMacros, notFound given)
  affects: [04-04-adt-metadata-companion, 04-05-meta-annotations, phase-6-gencodec-derivation]
tech-stack:
  added: [scala.quoted.*, Expr[T], Quotes, '{ ??? } macro-quote placeholder, polymorphic context-function given]
  patterns: [fork-staged deferred bodies (real impl → Phase 6), inline-given macro splice (lazyMetadata), @implicitNotFound message interpolation]
key-files:
  created:
    - core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala
  modified:
    - core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
    - MIGRATION.md
key-decisions:
  - "Port MetaMacros.scala verbatim from fork — including the three '{ ??? } splice bodies (valueImpl/lazyMetadataImpl/dummy). Real reflection bodies deferred to Phase 6 per fork-staging. Documented in MIGRATION.md §1."
  - "ImplicitNotFound is already provided by core/src/main/scala/com/avsystem/commons/misc/Implicits.scala (sealed trait + ImplicitNotFound() apply); no separate file extraction needed. Existing import com.avsystem.commons.misc.ImplicitNotFound resolves."
  - "MetadataCompanion reshape: replace Phase-1 implicit def / classical-trait shape with fork's polymorphic-context-function givens (given fromFallback, given notFound) and Lazy companion extending MetadataCompanionLazyMacros. Same idiom as slice 3.3 precedent."
  - "scalafmt reformat of MetadataCompanion (10 ins / 24 del) folded into the port commit — fork's brace/indent style differed from local config."
  - "Lazy-metadata splice compile-only probe redesigned to actually summon Lazy[String] (rather than nonsensical Lazy[String] => Unit from plan's draft). Probe confirmed inline-given lazyMetadata splice resolves via MetadataCompanionLazyMacros."
patterns-established:
  - "Fork-staged deferred bodies: macro-quote SCAFFOLDING ships now ('{ ??? }), real reflection bodies land in Phase 6. Downstream call sites compile but throw NotImplementedError at runtime — intentional; APIs are inlined-away at consumer sites."
  - "Polymorphic context-function given idiom: `given name: [T] => (ev: A[T]) => B[T] = body` (slice 3.3 precedent, now applied to fromFallback + notFound)."
requirements-completed:
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
metrics:
  duration: 4 min
  completed: 2026-06-02
  commits: 3
---

# Phase 4 Plan 03: MetaMacros + MetadataCompanion Summary

**MetaMacros.scala scaffolding (7 macro-trait stubs + object MetaMacros with `'{ ??? }` splice bodies) plus MetadataCompanion rewritten to fork's polymorphic-context-function-given shape — real macro reflection deferred to Phase 6 per fork-staging.**

## Performance

- **Duration:** 4 min
- **Started:** 2026-06-01T22:43:27Z
- **Completed:** 2026-06-01T22:48:01Z
- **Tasks:** 4 (Task 1-3 produced commits; Task 4 was acceptance gate + push)
- **Files modified:** 3 (1 new, 2 edited)

## Accomplishments

- **NEW FILE** `core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala` — verbatim port from fork. 7 macro-trait scaffolds (InferMacros, AdtMetadataCompanionMacros[M], BoundedAdtMetadataCompanionMacros[Hi,Lo,M], MetadataCompanionMacros[M], BoundedMetadataCompanionMacros[Hi,Lo,M], MetadataCompanionLazyMacros[M,Lazy], BoundedMetadataCompanionLazyMacros[Hi,Lo,M,Lazy]) + companion object with three `'{ ??? }` splice impls (`valueImpl[T:Type](using Quotes): Expr[T]`, `lazyMetadataImpl(using Quotes): Expr[Nothing]`, `dummy(using Quotes): Expr[Nothing]`). First `import scala.quoted.*` in the meta layer.
- `core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala` rewritten verbatim from fork — `given fromFallback: [Real] => (fallback: Fallback[M[Real]]) => M[Real]`, `Lazy extends MetadataCompanionLazyMacros[M, Lazy]`, `given notFound: [T] => (forNotLazy: ImplicitNotFound[M[T]]) => ImplicitNotFound[Lazy[T]]` with `@implicitNotFound("#{forNotLazy}")`, plus `BoundedMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]]` mirror.
- MIGRATION.md §1 declares `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` real bodies deferred to Phase 6. §3 documents `meta/MetaMacros + MetadataCompanion (slice 4.3)` reshape. Phase-1 backlog rows for `MetadataCompanion.scala:27` and `:58` removed.

## Task Commits

Fork-cadence, NO squash:

1. **Task 1: Port MetaMacros.scala verbatim from fork** — `b8d887c6` (feat)
2. **Task 2: Rewrite MetadataCompanion.scala to fork shape** — `a573683e` (feat)
3. **Task 3: MIGRATION.md §1 deferral + §3 shape shift** — `78700cef` (docs)

**Plan metadata:** (this SUMMARY) — committed in main repo `.planning/` (gitignored on worktree).

Branch `04-03-meta-macros` pushed to `origin/04-03-meta-macros` stacked on `origin/04-02-macro-instances`.

## Acceptance Gates (all green)

- `sbt commons-core/compile` → exit 0
- `sbt commons-core/Test/compile` → exit 0
- `sbt scalafmtCheckAll` → exit 0
- `grep -c "'{ ??? }" MetaMacros.scala` → 3 (≥3 required)
- 7/7 traits + 1 object MetaMacros + 3 def bodies (valueImpl, lazyMetadataImpl, dummy) present
- MetadataCompanion grep parity (trait/given/Lazy/BoundedMetadataCompanion/etc.) → 7/7 match
- Lazy-metadata splice compile probe → green (probe deleted post-verify; confirmed `MetadataCompanionLazyMacros.lazyMetadata` inline-given splice resolves)
- `git diff 04-02-macro-instances..HEAD | grep -cE '^\+.*(@nowarn|-Wconf)'` → 0 (no new suppression)
- `git log --name-only 04-02-macro-instances..HEAD | grep -c '^\.planning/'` → 0 (no planning files in commits)
- MIGRATION.md: `slice 4.3` ×2, `MetaMacros` ×3, `lazyMetadataImpl` ×2, `fromFallback` ×2

## Files Created/Modified

- `core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala` (NEW, 49 LOC) — macro-quote scaffolding
- `core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala` (-24/+10) — fork-shape givens + Lazy companion
- `MIGRATION.md` (+18/-2) — §1 deferral entry + §3 shape-shift section, Phase-1 backlog rows resolved

## Decisions Made

See frontmatter `key-decisions`. Highlights:

- Verbatim fork port (incl. `'{ ??? }` bodies) — bodies are fork-staged deferred to Phase 6.
- `ImplicitNotFound` resolves via existing `misc/Implicits.scala` — no separate extraction.
- scalafmt reformat folded into Task 2 commit (no separate style commit).
- Lazy-metadata splice probe redesigned (plan's draft was nonsensical `Lazy[String] => Unit`); new probe successfully verified `MetadataCompanionLazyMacros.lazyMetadata` inline-given resolution.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking issue] scalafmt reformat of MetadataCompanion**

- **Found during:** Task 2 `sbt scalafmtCheckAll` gate (post-port).
- **Issue:** Fork's `MetadataCompanion.scala` uses different brace/indent style than local `.scalafmt.conf`. Check failed: "1 files must be formatted".
- **Fix:** Ran `sbt scalafmtAll`. Diff: -24/+10 LOC (style-only). Re-ran check → green.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala`
- **Verification:** `sbt scalafmtCheckAll` exit 0; recompile (`sbt commons-core/compile && commons-core/Test/compile`) exit 0.
- **Committed in:** `a573683e` (Task 2 commit — fmt folded in).

**2. [Rule 3 - Blocking issue] Redesign lazy-metadata splice probe**

- **Found during:** Task 4 step 2 (probe compile-sanity).
- **Issue:** Plan's draft probe `summon[MyMetaCompanion.Lazy[String] => Unit]` asked for a function value, not the splice — failed with "No given instance of MyMetaCompanion.Lazy[String] => Unit". This was a typo in the plan, not a regression.
- **Fix:** Rewrote probe to `summon[MyMetaCompanion.Lazy[String]]` with `given dummy: [T] => MyMeta[T]` in scope + `import Lazy.given` — successfully exercises `MetadataCompanionLazyMacros.lazyMetadata` inline-given splice resolution.
- **Files modified:** `core/src/test/scala/_LazyMetadataSpliceProbe.scala` (transient; deleted post-verify).
- **Verification:** `sbt commons-core/Test/compile` exit 0; file deleted; final test compile re-confirmed green.
- **Committed in:** N/A (probe file never committed — created/deleted within Task 4).

### Plan steps skipped

**PR creation (Task 4 steps 6-8)** — Per user directive in this prompt's `<objective>` ("DO NOT OPEN PR ... NO `gh pr create`"). `gh pr list` lookup, `gh pr create`, `gh api PATCH /repos/.../milestone` all skipped. Branch pushed to `origin/04-03-meta-macros`; PR-opening deferred to user.

---

**Total deviations:** 2 auto-fixed (1 scalafmt reformat, 1 probe redesign) + 1 plan-step skip (PR creation per user directive).
**Impact on plan:** All auto-fixes were execution-environment glue, not behavioral. No scope creep; all acceptance criteria met verbatim.

## Issues Encountered

None — clean execution. Compile green on first attempt of both source files; scalafmt one-shot fixed via `scalafmtAll`.

## Authentication Gates

None occurred during this slice.

## Next Phase Readiness

- **Slice 4.4 (AdtMetadataCompanion)** can branch off `04-03-meta-macros`. Consumes `AdtMetadataCompanionMacros[M]` + `BoundedAdtMetadataCompanionMacros[Hi,Lo,M]` traits from `MetaMacros.scala`.
- **Slice 4.5 (metaAnnotations.infer)** consumes `InferMacros` trait from `MetaMacros.scala`.
- **Phase 6** owes the real reflection bodies for `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` (currently `'{ ??? }`). Downstream `materialize` / `Lazy.lazyMetadata` / `infer.value` call sites compile but throw `NotImplementedError` at runtime; this is intentional fork-staging.
- No blockers, no carry-over TODOs beyond Phase-6 deferral.

## Self-Check

- `core/src/main/scala/com/avsystem/commons/meta/MetaMacros.scala` — FOUND (new, 49 LOC, 3 `'{ ??? }` bodies present)
- `core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala` — FOUND (modified; fromFallback + Lazy.notFound givens present)
- `MIGRATION.md` — FOUND (modified; slice 4.3 ×2, MetaMacros ×3, lazyMetadataImpl ×2, fromFallback ×2)
- Commit `b8d887c6` — FOUND in `git log 04-02-macro-instances..HEAD`
- Commit `a573683e` — FOUND in `git log 04-02-macro-instances..HEAD`
- Commit `78700cef` — FOUND in `git log 04-02-macro-instances..HEAD`
- Branch `04-03-meta-macros` pushed to `origin` — confirmed (`* [new branch] 04-03-meta-macros -> 04-03-meta-macros`)
- Probe file `_LazyMetadataSpliceProbe.scala` — REMOVED (transient, post-verify)

## Self-Check: PASSED

---
*Phase: 04-meta-derivation-core*
*Completed: 2026-06-02*
