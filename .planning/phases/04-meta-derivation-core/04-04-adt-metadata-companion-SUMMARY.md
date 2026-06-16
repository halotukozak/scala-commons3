---
phase: 04-meta-derivation-core
plan: 04
subsystem: meta-derivation
tags: [scala-3, meta, adt-metadata, bound-tightening, slice-4.4]
requires:
  - 04-03-meta-macros (AdtMetadataCompanionMacros, BoundedAdtMetadataCompanionMacros, MetadataCompanion, BoundedMetadataCompanion, MetaMacros)
  - 04-01-foundation (TypedMetadata)
provides:
  - "AdtMetadataCompanion[M[X] <: TypedMetadata[X]] — 2-line trait composition"
  - "BoundedAdtMetadataCompanion[Hi, Lo <: Hi, M[_ >: Lo <: Hi]] — fork-mirror"
affects:
  - GenUnionInfo / GenCaseInfo (extend AdtMetadataCompanion — already TypedMetadata-bound)
tech-stack:
  added: []
  patterns: [trait-composition, bound-tightening]
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
    - core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala
    - MIGRATION.md
decisions:
  - "Bound tightened from M[_] to M[X] <: TypedMetadata[X] per fork — required for inline given inheritance from AdtMetadataCompanionMacros"
  - "Selectively un-wrap AdtMetadataTest: GenStructure ADT hierarchy lives; HasGenCodecStructure + its consumers stay wrapped (deferred to Phase 6 / NamedTuple Instances reshape)"
  - "Omit fork's `materialize[Option[String]]` companion-init on GenUnorderedUnion — MetaMacros.dummy still ships '{ ??? } per slice 4.3; would throw NotImplementedError at object init"
  - "Preserve fork verbatim comment `// cannot share code with AdtMetadataCompanion because of binary compatibility problems, must copy`"
  - "PR NOT opened per user directive (push only)"
metrics:
  duration: "~5 minutes (compile + scalafmt cycles)"
  tasks: 4
  files: 3
  commits: 3
  completed: 2026-06-02
---

# Phase 4 Plan 04: AdtMetadataCompanion Port + Bound Tightening Summary

Collapsed `AdtMetadataCompanion.scala` from Phase-1 4-method stub (`M[_]`) to fork's 2-line trait composition with bound tightened to `M[X] <: TypedMetadata[X]`; mirror collapse for `BoundedAdtMetadataCompanion`.

## What Shipped

- **Source port (Task 1):** `core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala` rewritten verbatim from `origin/master:core/src/main/scala-3/...`. File shrinks from 39 LOC stub to 28 LOC (mostly scaladoc). Bound `M[X] <: TypedMetadata[X]` accepted. "must copy" fork comment preserved.
- **Test selective un-wrap (Task 2):** `AdtMetadataTest.scala` — un-wrapped the GenStructure ADT hierarchy (10 case classes / sealed traits, all extend `TypedMetadata` → satisfy new bound). Kept `HasGenCodecStructure` + 6 dependent declarations wrapped pending Phase 6 (slice 4.2 `MacroInstances` reshape requires `Instances <: AnyNamedTuple`).
- **MIGRATION.md (Task 3):** §3 source-compat entry `core — meta AdtMetadataCompanion (slice 4.4)`; §4 bincompat-narrowing entry; 4 Backlog rows for `AdtMetadataCompanion.scala` Phase-1 TODOs removed.
- **Branch push (Task 4):** `origin/04-04-adt-metadata-companion` set up; PR explicitly NOT opened per user directive.

## Commits

| Hash       | Type     | Message                                                                |
|------------|----------|------------------------------------------------------------------------|
| `3e501709` | feat     | port AdtMetadataCompanion + BoundedAdtMetadataCompanion                |
| `a5fdb132` | test     | selectively un-wrap AdtMetadataTest                                    |
| `20cf9fd0` | docs     | record AdtMetadataCompanion bound tightening (slice 4.4) in MIGRATION  |

## Acceptance Gates (all green)

- `sbt commons-core/compile` exit 0
- `sbt commons-core/Test/compile` exit 0
- `sbt scalafmtCheckAll` exit 0
- `sbt 'commons-core/testOnly *AdtMetadataTest'` exit 0 (no test methods; declarations compile)
- Shape parity probe `trait MyAdtMeta[X] extends TypedMetadata[X]; object MyAdtMetaCompanion extends AdtMetadataCompanion[MyAdtMeta]` compiled green (scratch file added + removed)
- 0 new `@nowarn` / `-Wconf` in diff vs `04-03-meta-macros`
- 0 `@nowarn` / `@ignore` added to test
- 0 `.planning/` paths in commit diff

## Deviations from Plan

### [Rule 1 - Plan vs Reality] AdtMetadataTest has no ScalaTest methods

- **Found during:** Task 2
- **Issue:** Plan Task 2 acceptance required `grep -c 'pending' AdtMetadataTest.scala >= 1`, but the file contains only test-data ADT declarations — no `test("...")` or `it should ...` blocks. The Phase-1 `/* */` wrap was a compile-protection envelope around `HasGenCodecStructure` consumers, not test-method gating.
- **Decision:** Selectively un-wrap the bound-satisfying declarations (the GenStructure ADT hierarchy — all extend `TypedMetadata`). Keep `HasGenCodecStructure` + its 6 consumers wrapped (their dependency on classical-trait `MacroInstances[Unit, GenCodecStructure[T]]` is incompatible with slice 4.2's `Instances <: AnyNamedTuple` bound; deferred to Phase 6). No `pending` required because there are no test methods to defer.
- **Files modified:** `core/src/test/scala/com/avsystem/commons/misc/AdtMetadataTest.scala`
- **Commit:** `a5fdb132`

### [Rule 3 - Divergence from fork] Omitted `materialize[Option[String]]` companion-init

- **Found during:** Task 2
- **Issue:** Fork's `object GenUnorderedUnion` body contains `materialize[Option[String]]` at init. With `MetaMacros.dummy = '{ ??? }` (slice 4.3 placeholder), this would throw `NotImplementedError` whenever the companion is first touched.
- **Decision:** Omit the call; document in inline comment + MIGRATION.md note. Restore in Phase 6 alongside `MetaMacros.dummy` real body.
- **Files modified:** same as above.
- **Commit:** `a5fdb132`

## Deferred Issues / Backlog

None new. The 4 `AdtMetadataCompanion.scala` Phase-1 TODOs are resolved (removed from Backlog table). `HasGenCodecStructure` consumer wrap is tracked by existing slice 4.2 MIGRATION.md note.

## Self-Check: PASSED

- AdtMetadataCompanion.scala: FOUND (M[X] <: TypedMetadata[X] bound verified)
- AdtMetadataTest.scala: FOUND (selective un-wrap applied)
- MIGRATION.md: FOUND (§3 + §4 + Backlog updated)
- Commit `3e501709`: FOUND
- Commit `a5fdb132`: FOUND
- Commit `20cf9fd0`: FOUND
- Branch pushed: `origin/04-04-adt-metadata-companion` confirmed by push output
