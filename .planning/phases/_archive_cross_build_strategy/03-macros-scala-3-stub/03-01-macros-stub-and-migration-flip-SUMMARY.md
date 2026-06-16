---
phase: 03-macros-scala-3-stub
plan: 01
subsystem: build
tags: [scala-3, macros, cross-build, migration-doc]
requires:
  - 02-migration-md @ 7cba3d2f (MIGRATION.md present with macros row)
  - Phase 1 sbt substrate (commons-macros has crossScalaVersions=[3.8.2, 2.13.18])
provides:
  - commons-macros Scala 3 cross-build (empty/near-empty jar)
  - dependsOn(macros) resolvable on Scala 3 for downstream modules
  - MIGRATION.md macros row notes column reflects landed stub
affects:
  - macros/src/main/scala-3/ (new directory, .gitkeep anchor)
  - MIGRATION.md (macros row notes column only)
tech-stack:
  added: []
  patterns:
    - .gitkeep-anchored empty Scala 3 source set for cross-build stub modules
key-files:
  created:
    - macros/src/main/scala-3/.gitkeep
  modified:
    - MIGRATION.md
decisions:
  - .gitkeep stub strategy succeeded; no fallback package.scala needed (sbt produced a valid 335-byte commons-macros_3 jar)
  - Two atomic commits per plan contract (build + docs), both upstream-conventional prefixes
  - Branch cut from 02-migration-md @ 7cba3d2f (Phase 2 tip) rather than upstream/scala-3 to maintain the cascaded PR stack
metrics:
  duration_minutes: 3
  completed_date: 2026-05-31
---

# Phase 03 Plan 01: macros Scala 3 stub and migration flip — Summary

One-liner: Empty `macros/src/main/scala-3/` (anchored by `.gitkeep`) lets `commons-macros` cross-build on Scala 3.8.2 producing a near-empty jar; MIGRATION.md macros row notes column flipped to reflect the landed stub.

## Outcome

Branch `03-macros-stub` cut from `02-migration-md @ 7cba3d2f` (Phase 2 tip, cascadowo stack). Two atomic commits added on top:

```
221f3bda docs(migration): flip macros row to reflect Scala 3 stub
0864e85f build(macros): add empty Scala 3 source dir for cross-build
```

Branch HEAD: `221f3bdac395a9d19751853205589c4e9ee136d5`.

## Verification gates

| Gate | Command | Result |
| --- | --- | --- |
| Scala 3 macros compile | `sbt -batch '; ++3.8.2 ; commons-macros/compile'` | PASS (0s, no sources) |
| Scala 3 macros package | `sbt -batch '; ++3.8.2 ; commons-macros/package'` | PASS — `commons-macros_3-2.28.0+43-7cba3d2f-SNAPSHOT.jar` (335 bytes) |
| Scala 2.13 macros regression guard | `sbt -batch '; ++2.13.18 ; commons-macros/clean ; commons-macros/compile'` | PASS (compiled 28 sources, 6s) |
| scalafmt check | `sbt -batch scalafmtCheckAll` | PASS |
| Scala 3 core (downstream sanity) | `sbt -batch '; ++3.8.2 ; commons-core/compile'` | **FAIL (114 errors)** — pre-existing scala-3 source gap, NOT a Phase 3 regression |

## commons-core Scala 3 status (informational)

`commons-core/compile` on Scala 3.8.2 fails with 114 errors + 212 warnings. The errors originate in shared `core/src/main/scala/` sources that still contain scala-2-only macro defs and other scala-2 idioms — this is the pre-existing source-port gap known from Phase 1 (which adopted a pin-2.13 CI strategy precisely because of this). Phase 3's true acceptance criterion is `commons-macros/compile` only; downstream cross-build sanity for `core` is deferred to Phase 5+ when per-module ports land. No regression introduced by this plan: the failures are in source paths untouched here, and reproduce on `7cba3d2f` (the branch base) as well.

## Stub strategy chosen

`.gitkeep` alone was sufficient — sbt's standard `Compile / unmanagedSourceDirectories` resolves `macros/src/main/scala-3/` and `commons-macros/package` produces a valid 335-byte jar containing only `META-INF/MANIFEST.MF`. The fallback `package object macros` file specified in CONTEXT.md was NOT needed.

## MIGRATION.md change (exact)

Before:
```
| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty in the next port. |
```

After:
```
| macros | cross | stub | n/a | n/a | Empty scala-3 dir; whitebox impls remain 2.13-only. |
```

Status tokens unchanged (`cross` / `stub`). Notes column wording matches CONTEXT.md §"Documentation" target verbatim (54 chars, ≤80 char budget). 13-row table count preserved.

## Diff stat

```
 MIGRATION.md                     | 2 +-
 macros/src/main/scala-3/.gitkeep | 0
 2 files changed, 1 insertion(+), 1 deletion(-)
```

## Hygiene (REQ WORKFLOW-04, WORKFLOW-05, QUALITY-01)

- No GSD nomenclature in commit messages (`gsd|phase [0-9]|plan-phase` greps clean).
- No `.planning/` paths in any commit (`git log 7cba3d2f..HEAD --name-only | grep -c '^\.planning'` → `0`).
- No new `@nowarn` / `-Wconf` introduced (`git diff 7cba3d2f..HEAD | grep -E '^\+.*(@nowarn|-Wconf)'` → empty).
- Both commits prefixed `build(macros):` / `docs(migration):` (upstream-conventional, no internal vocabulary).

## Deviations from plan

The PLAN.md as written referenced a different branch base (`upstream/scala-3`), a different branch name (`03-macros-scala-3-stub`), and different sbt project IDs (`macros` / `core` vs `commons-macros` / `commons-core`). The executor followed the prompt's `<important>` override block, which reflects the actual current substrate:

- Branch base: `02-migration-md @ 7cba3d2f` (Phase 2 tip — stacked-PR cascade)
- Branch name: `03-macros-stub` (no `scala-3` keyword — shorter, still no GSD nomenclature)
- sbt project IDs: `commons-macros`, `commons-core` (sbt-nosbt's `ProjectGroup("commons")` wrapper auto-prepends `commons-` — see STATE.md "Plan 03 (2026-05-31)" decision).
- `commons-core/compile` on Scala 3 expected to fail and was documented rather than treated as a phase-3 blocker.

No auto-fix deviations (Rules 1–3) triggered.

## Next

Plan 03-02 (push-and-pr) — gated by user ack per Phase 2 protocol.

## Self-Check: PASSED

Verified:
- `macros/src/main/scala-3/.gitkeep` exists (FOUND).
- `MIGRATION.md` updated row present (FOUND via grep).
- Commit `0864e85f` exists in git log (FOUND).
- Commit `221f3bda` exists in git log (FOUND).
- Branch `03-macros-stub` HEAD = `221f3bda` (FOUND).
