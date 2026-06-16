---
phase: 05-core-scala-3-baseline-port
plan: 01
subsystem: infra
tags: [scala-3, cross-build, source-layout, macros, rpc]

# Dependency graph
requires:
  - phase: 04-made-integration
    provides: "Phase 4 tip 94f52ece with 11 files (Opt-family + serialization annotations) already relocated to scala-2.13/"
provides:
  - "33 scala-2-only sources relocated to core/src/main/scala-2.13/ (macro defs, RPC framework, derivation entry points)"
  - "Branch 05-core-scala-3-baseline-port cut off 04-made-integration tip"
  - "Clean scala/ tree under core (no `= macro ` defs remain in shared sources)"
affects: [05-02-cherry-pick-scala-3-sources, 05-03-sanity-gate-and-migration-flip, 06-given-using-sweep]

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Cross-build source layout: scala/ shared, scala-2.13/ macros + RPC + scala-2-only, scala-3/ wiring (expanded next plan)"
    - "Whole-subsystem relocation (entire core/.../rpc/ moved as a unit per MIGRATION.md 2.13-only subsystem rule)"

key-files:
  created:
    - "core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala"
    - "core/src/main/scala-2.13/com/avsystem/commons/di/Components.scala"
    - "core/src/main/scala-2.13/com/avsystem/commons/meta/{AdtMetadataCompanion,MetadataCompanion,metaAnnotations}.scala"
    - "core/src/main/scala-2.13/com/avsystem/commons/misc/{AnnotationOf,ApplierUnapplier,Delegation,Implicits,Sam,SamCompanion,SealedUtils,SelfInstance,SimpleClassName,SourceInfo,TypeString,ValueEnum}.scala"
    - "core/src/main/scala-2.13/com/avsystem/commons/annotation/{AnnotationAggregate,positioned}.scala"
    - "core/src/main/scala-2.13/com/avsystem/commons/rpc/{AsRawReal,MetadataAnnotation,RPCFramework,RawRpcCompanion,RawValueCompanion,RpcMetadataCompanion,RpcUtils,StandardRPCFramework,rpcAnnotations}.scala"
    - "core/src/main/scala-2.13/com/avsystem/commons/serialization/{GenCodec,GenKeyCodec,GenObjectCodec,GenRef,macroCodecs}.scala"
  modified: []

key-decisions:
  - "Branched off 94f52ece (Phase 4 actual tip after Copilot fixes) rather than plan-stated c3e54b16 — superseded by 11-file relocation already landed on master/04-made-integration"
  - "Relocated entire rpc/ subtree (9 files) as one unit per MIGRATION.md 2.13-only subsystem rule, not just the 5 macro-bearing files"
  - "Did NOT include meta/MacroInstances.scala or meta/metadata.scala (no `= macro` defs and not in <known_seed_files>) — defer to Plan 05-02 which will overlay scala-3/ counterparts"
  - "Did NOT relocate meta/Fallback.scala, meta/OptionLike.scala, serialization/HasGenCodec, serialization/wrappers, etc. — these will be addressed by Plan 05-02 scala-3/ overlay or Phase 6"

patterns-established:
  - "Pre-overlay-relocation: when scala-3/ counterpart will land later, relocate scala-2 source first so the cross-build only sees one definition under each scala version"
  - "Subsystem-coherent moves: when a subsystem is 2.13-only (RPC), move the entire directory together"

requirements-completed: [CORE-02, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05, QUALITY-01]

# Metrics
duration: 8min
completed: 2026-06-01
---

# Phase 5 Plan 1: Branch + Relocate Scala-2-Only Sources Summary

**33 scala-2 macro/RPC/derivation sources relocated from shared `scala/` to `scala-2.13/`, branch `05-core-scala-3-baseline-port` cut, ++2.13.18 commons-core/compile + scalafmtCheckAll remain green.**

## Performance

- **Duration:** ~8 min
- **Started:** 2026-06-01T11:16:42Z
- **Completed:** 2026-06-01T11:24:00Z
- **Tasks:** 3
- **Files moved:** 33 (all `git mv`, 100% rename detection)

## Accomplishments

- Branch `05-core-scala-3-baseline-port` created at `94f52ece` (Phase 4 actual tip with Copilot fixes).
- Discovered scala-2-only file set via `git grep -l '= macro '` (29 files) + full `rpc/` subtree union (33 files total after dedup).
- Executed 33 `git mv` operations from `core/src/main/scala/` to `core/src/main/scala-2.13/`, history preserved (100% similarity index on every rename).
- Verified `++2.13.18 commons-core/compile` remains green (138 sources compile, identical count to pre-move).
- Verified `scalafmtCheckAll` remains green (no reformat needed; scala-2.13/ dialect pinning landed earlier).
- Confirmed `git grep -l '= macro ' core/src/main/scala/` is empty post-move.

## Task Commits

1. **Task 1: Cut branch + baseline gates** — no commit (branch-only operation; baseline measurements logged).
2. **Task 2: Enumerate relocation list** — no commit (scratch list `/tmp/05-01-relocations.txt`).
3. **Task 3: git mv + compile gates** — `2b0b0ad4` (refactor).

## Files Created/Modified

All 33 moves are renames (0 insertions, 0 deletions in `git diff --stat`). Grouped by subsystem:

**Core extensions / SAM (3):**
- `core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/Sam.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/SamCompanion.scala`

**Annotations (macro-bound) (2):**
- `core/src/main/scala-2.13/com/avsystem/commons/annotation/AnnotationAggregate.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/annotation/positioned.scala`

**DI (1):**
- `core/src/main/scala-2.13/com/avsystem/commons/di/Components.scala`

**Meta companions (3):**
- `core/src/main/scala-2.13/com/avsystem/commons/meta/AdtMetadataCompanion.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/meta/MetadataCompanion.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/meta/metaAnnotations.scala`

**Misc macro materializers (10):**
- `core/src/main/scala-2.13/com/avsystem/commons/misc/AnnotationOf.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/ApplierUnapplier.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/Delegation.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/Implicits.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/SealedUtils.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/SelfInstance.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/SimpleClassName.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/SourceInfo.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/TypeString.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/ValueEnum.scala`

**RPC framework subsystem (9, entire dir):**
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/AsRawReal.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/MetadataAnnotation.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/RPCFramework.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/RawRpcCompanion.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/RawValueCompanion.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/RpcMetadataCompanion.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/RpcUtils.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/StandardRPCFramework.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/rpc/rpcAnnotations.scala`

**Serialization derivation entries (5):**
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/GenCodec.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/GenKeyCodec.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/GenObjectCodec.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/GenRef.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/macroCodecs.scala`

## Decisions Made

- **Branch base:** `94f52ece` (Phase 4 actual tip with Copilot relocation fixes), not `c3e54b16` as the plan text suggested. The 11-file pre-relocation already landed in commit `caabd39c` is on the same branch line — building on it avoids reverting work.
- **Scope: macro-defs ∪ RPC subsystem.** Followed the plan's `<known_seed_files>` exactly (29 files from `git grep -l '= macro '`) plus the full `rpc/` directory (4 additional non-macro files: `MetadataAnnotation`, `RawValueCompanion`, `StandardRPCFramework`, `rpcAnnotations`). RPC is documented in MIGRATION.md as a 2.13-only subsystem; moving it as a coherent unit avoids partial-state weirdness for Plan 05-02.
- **Excluded files:** `meta/MacroInstances`, `meta/metadata`, `serialization/HasGenCodec`, `serialization/wrappers`, `serialization/TupleGenCodecs` — these are referenced by 3.x errors but lack `= macro` defs themselves. They will either be covered by Plan 05-02's scala-3/ overlay (preferred) or deferred to Phase 6. Conservative scope here keeps this plan to a clean 33-file rename commit.
- **Excluded files:** scala-3 counterparts on master (`Bidirectional`, `BoxingUnboxing`, `Bytes`, `CaseMethods`, etc.) — these compile on both sides; Plan 05-02 will cherry-pick the scala-3/ versions and we'll re-evaluate any duplicate-definition issues then.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Branched off `94f52ece` instead of plan-stated `c3e54b16`**
- **Found during:** Task 1 (branch creation)
- **Issue:** The plan frontmatter and Task 1 instructions specify branching from `c3e54b16`, but `04-made-integration` branch has advanced to `94f52ece` via Copilot fixes (commit `caabd39c` already relocated 11 Opt-family/annotation files to `scala-2.13/`, then `94f52ece` fixed Scaladoc).
- **Fix:** Branched from current `04-made-integration` HEAD (`94f52ece`). This is consistent with the user prompt's `<state>` block and means we build on top of the existing partial relocation rather than reverting it.
- **Files modified:** none (branch operation).
- **Verification:** `git log --oneline 04-made-integration..HEAD` shows exactly 1 new commit; the 11 files already in `scala-2.13/` from `caabd39c` are not in our diff.

---

**Total deviations:** 1 auto-fixed (Rule 3 — Phase 4 advanced past plan's recorded base).
**Impact on plan:** Zero scope change; just acknowledges that the Phase 4 PR base moved.

## Issues Encountered

- **++3.8.2 commons-core/compile error count increased from 114 → 263 after the move (not decreased).** The plan's success criteria explicitly allows 3.x core compile to remain red ("acceptable — closed in Plan 05-02"), but the user prompt expected the count to *reduce*. Root cause: when scala-2-only files are removed from `scala/`, scala-3 stops getting their duplicate-def-like errors, but other files that *referenced* those types (e.g., `meta/MacroInstances`, `meta/metadata`, `serialization/HasGenCodec`, JVM-side `core/jvm/src/main/scala/.../CrossUtils`, etc.) now error with "not found" instead. These are the dependencies the upcoming Plan 05-02 scala-3/ cherry-pick is designed to resolve. The 3.x baseline is the wrong dimension to measure progress on this plan in isolation — 2.13 green + scalafmt green + clean `scala/` tree are the correct gates, and they all pass.
- No `// format: off` blocks needed (scalafmt landed scala-2.13/ dialect overrides in earlier work).
- No `@nowarn` / `-Wconf` introduced.

## User Setup Required

None.

## Next Phase Readiness

- `core/src/main/scala/` no longer contains any `= macro ` definitions — the precondition for cherry-picking `core/src/main/scala-3/` sources in Plan 05-02 is satisfied.
- `scala-2.13/` now has the full RPC + derivation-entry + macro-companion surface in one place.
- Plan 05-02 should expect to add ~80-100 files under `core/src/main/scala-3/` from fork master (minus cbor/mongo/RPC consumers).
- Branch tip: `2b0b0ad4`.

---
*Phase: 05-core-scala-3-baseline-port*
*Completed: 2026-06-01*

## Self-Check: PASSED

- SUMMARY.md created at `.planning/phases/05-core-scala-3-baseline-port/05-01-branch-and-relocate-scala-2-only-SUMMARY.md` (verified).
- Commit `2b0b0ad4` present in `git log` (verified).
- All 33 target files present under `core/src/main/scala-2.13/`; corresponding originals absent from `core/src/main/scala/` (spot-checked: `rpc/RPCFramework.scala` moved; `serialization/GenCodec.scala` removed from `scala/`).
