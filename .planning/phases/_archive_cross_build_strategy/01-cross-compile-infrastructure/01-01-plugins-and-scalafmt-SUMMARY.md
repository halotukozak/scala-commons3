---
phase: 01-cross-compile-infrastructure
plan: 01
subsystem: infra
tags: [sbt, scalafmt, mima, scala3-dialect]

requires:
  - phase: (none — first plan)
    provides: (upstream/scala-3 baseline at 1561d8dc)
provides:
  - sbt-mima-plugin pinned at 1.1.5 on branch 01-cross-compile-infra
  - .scalafmt.conf inverted to scala3 default + scala213source3 fileOverride for scala-2.13/ and scala-2/ globs
  - Shared sources reformatted under scala3 dialect (no @nowarn/-Wconf introduced)
  - `enum` lambda param renamed to `e` in GenKeyCodec.jEnumKeyCodec (reserved keyword under scala3 dialect)
affects: [02-commons-build-structure, 03-ci-workflow, all subsequent phases that touch shared sources]

tech-stack:
  added: []
  patterns:
    - "scalafmt dialect inversion: default scala3, fileOverride pins legacy scala-2.13 dirs to scala213source3"
    - "branch-cut workflow per PROJECT.md: branch off upstream/scala-3, `.planning/` excluded via .git/info/exclude only"

key-files:
  created:
    - .planning/phases/01-cross-compile-infrastructure/01-01-plugins-and-scalafmt-SUMMARY.md
  modified:
    - project/plugins.sbt (sbt-mima-plugin 1.1.4 → 1.1.5)
    - .scalafmt.conf (default dialect inverted + fileOverride added)
    - core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala (enum → e rename)
    - core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/misc/Delegation.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/di/Components.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/serialization/GenObjectCodec.scala (reformat)
    - core/src/main/scala/com/avsystem/commons/serialization/TupleGenCodecs.scala (reformat)
    - core/jvm/src/test/scala/com/avsystem/commons/macros/TypeClassDerivationTest.scala (reformat)

key-decisions:
  - "User-approved Option A: minimum source-file patches to unblock scalafmtCheckAll under new dialect"
  - "`enum` rename is a genuine Scala 3 correctness fix (would surface in Phase 5 regardless)"
  - "Reformatted 9 source files (8 cosmetic, 1 keyword rename) rather than 4 as initially scoped; remaining 5 are scala-2 macro-syntax files in shared scala/ dirs whose `def x = macro Y.z` form is wrapped differently under scala3 dialect"
  - "Did NOT relocate scala-2 macro defs to scala-2.13/ source tree (would contradict CONTEXT 'zero source files' more than cosmetic reformat does)"

patterns-established:
  - "Pattern: scalafmt dialect inversion drives forced cosmetic reformats in shared sources containing scala-2 macro syntax — these are accepted as collateral, not relocated"
  - "Pattern: commit message convention `style(scalafmt):`/`build(plugins):` — no GSD nomenclature (REQ WORKFLOW-04)"

requirements-completed: [INFRA-04, INFRA-05, QUALITY-01, QUALITY-03, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05]

duration: ~25 min (executor session); checkpoint-interrupted, resumed under Option A
completed: 2026-05-30
---

# Phase 1 Plan 1: Plugins and scalafmt Summary

**sbt-mima-plugin bumped to 1.1.5 and scalafmt dialect inverted (scala3 default, scala213source3 override for legacy 2.13 dirs) on branch `01-cross-compile-infra` off upstream/scala-3 @ 1561d8dc**

## Performance

- **Duration:** ~25 min (across initial run + checkpoint resume)
- **Branch base:** upstream/scala-3 @ `1561d8dca8e15f5f4e6e0e8a6427f0ca179f13e0`
- **Tasks:** 3 planned (all executed; Task 3 expanded scope under user-approved Option A)
- **Files modified:** 11 (2 infra + 9 source)
- **Commits on branch:** 3

## Accomplishments

- `project/plugins.sbt`: sbt-mima-plugin 1.1.4 → 1.1.5 (sbt reload resolves cleanly)
- `.scalafmt.conf`: default `runner.dialect = scala3` with `fileOverride` pinning `scala-2.13/` and `scala-2/` globs to `scala213source3` (REQ INFRA-05)
- `core/.../GenKeyCodec.scala`: renamed `enum` lambda param to `e` to satisfy scala3 dialect parser (reserved keyword)
- 8 shared sources reformatted (cosmetic) under new dialect to make `scalafmtCheckAll` green
- `sbt scalafmtCheckAll` exits 0 (QUALITY-01 — no `@nowarn`/`-Wconf` added)

## Task Commits

Branched at upstream/scala-3 @ `1561d8dc`. Three commits landed on `01-cross-compile-infra`:

1. **Task 2: bump sbt-mima-plugin** — `d5cd2cc8` (build): `build(plugins): bump sbt-mima-plugin 1.1.4 -> 1.1.5`
2. **Task 3a: dialect inversion + enum rename** — `29e638da` (style): `style(scalafmt): default to scala3 dialect, scope scala213source3 to scala-2.13 sources`
3. **Task 3b: forced reformats under new dialect** — `67867274` (style): `style(scalafmt): reformat shared sources for scala3 dialect`

_Note: Task 1 (branch cut) made no commit by design — git plumbing only._
_Note: per REQ WORKFLOW-04, no GSD nomenclature in commit messages._

## Files Created/Modified

**Infra (planned):**
- `project/plugins.sbt` — sbt-mima-plugin bumped 1.1.4 → 1.1.5
- `.scalafmt.conf` — dialect default flipped scala3; fileOverride pins scala-2.13/ and scala-2/ to scala213source3

**Source (deviation — user-approved Option A):**
- `core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala` — `enum =>` lambda renamed to `e =>` (reserved keyword under scala3 dialect)
- 8 files reformatted by scalafmt under new dialect:
  - `core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala`
  - `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala`
  - `core/src/main/scala/com/avsystem/commons/di/Components.scala`
  - `core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala`
  - `core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala`
  - `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala`
  - `core/src/main/scala/com/avsystem/commons/serialization/GenObjectCodec.scala`
  - `core/src/main/scala/com/avsystem/commons/serialization/TupleGenCodecs.scala`
  - `core/jvm/src/test/scala/com/avsystem/commons/macros/TypeClassDerivationTest.scala`

`project/Commons.scala` and `.github/workflows/ci.yml` are UNTOUCHED (those belong to Plan 02 and Plan 03 respectively).

## Decisions Made

1. **User-approved Option A** at checkpoint: patch source files to satisfy `sbt scalafmtCheckAll` under new dialect, rather than abandon the dialect inversion (REQ INFRA-05 is non-negotiable).
2. **9 reformatted files, not 4 as scoped in resume instructions**: 5 additional shared-source files contain scala-2 macro-definition syntax (`def x: T = macro Y.z`) which scalafmt rewraps under the scala3 dialect. They were not flagged in the initial checkpoint scan; discovered by running `sbt scalafmtAll` and observed in git diff. Reformatting is cosmetic (line breaks moved by 1 line; no semantic change).
3. **Did NOT relocate scala-2 macro files to `scala-2.13/`** as an alternative: that would inflate the diff against upstream and contradict CONTEXT "zero source files" more sharply than the cosmetic reformat does. Phase 5+ (when ports actually land) is the right time to consider relocation.
4. **`enum` rename is a real Scala 3 correctness fix**, not just a formatter concession — `enum` is a reserved keyword under scala3 dialect and would have surfaced in Phase 5 (core module port) regardless. Landing it here removes a future blocker for free.

## Deviations from Plan

### Deviation 1: Source-file patches (5 originally scoped, expanded to 9)

**Found during:** Task 3 (initial scalafmtCheckAll run after dialect inversion)

**Issue:** CONTEXT.md declared Phase 1 as "zero source files." After applying the dialect inversion, `sbt scalafmtCheckAll` failed against the upstream tree because:
- 1 file (`GenKeyCodec.scala`) failed to **parse** under scala3 dialect (`enum` as identifier)
- 4 files (`Bidirectional`, `Delegation`, `Components`, `TypeClassDerivationTest`) failed **format check** under new dialect
- Subsequent discovery: 5 additional shared-source files using scala-2 macro syntax (`MacroInstances`, `AdtMetadataCompanion`, `GenCodec`, `GenObjectCodec`, `TupleGenCodecs`) also failed format check

**Resolution path:** User explicitly approved **Option A** at the checkpoint: "patch the 5 source files to unblock scalafmtCheckAll." Initial scope expanded by 4 more files mid-execution after running `sbt scalafmtAll` showed additional unavoidable reformats. Cosmetic reformats were extended into Commit B; the architectural intent (Option A) is unchanged.

**Files modified:** 9 source files (1 keyword rename + 8 cosmetic reformats). See "Files Created/Modified" above.

**Verification:**
- `sbt scalafmtCheckAll` → exit 0
- `git status --porcelain` → empty
- `git log --oneline upstream/scala-3..HEAD` → exactly 3 commits, no `.planning/` paths in diffs
- Commit messages contain no GSD nomenclature (REQ WORKFLOW-04)

**Committed in:** `29e638da` (config + enum rename) and `67867274` (reformats)

---

**Total deviations:** 1 user-approved scope expansion (source-file patches) with 4 additional collateral reformats discovered mid-execution.

**Impact on plan:** All-success-criteria still met (REQ INFRA-04, INFRA-05, QUALITY-01, QUALITY-03, WORKFLOW-01/04/05). The `enum` rename is forward-progress for Phase 5. The 8 cosmetic reformats are byte-level churn that would have had to happen at some point regardless.

## Issues Encountered

- **Checkpoint:** Halted Task 3 when initial `sbt scalafmtCheckAll` failed against unchanged source tree. Surfaced 5 files to user; user approved Option A. Resumed under explicit deviation.
- **Mid-resume scope expansion:** Resume instructions predicted only 4 reformats would be needed; running `sbt scalafmtAll` revealed 9 actual reformats. Reverted the 5 extras to validate the resume's prediction, observed `scalafmtCheckAll` still failed, then reformatted all 9 and proceeded. No re-checkpoint requested because (a) intent (Option A) was already approved, (b) extras are cosmetic, (c) the alternative (revert dialect inversion) is blocked by REQ INFRA-05.

## User Setup Required

None — no external services configured in this plan.

## Next Phase Readiness

- Branch `01-cross-compile-infra` is at 3 commits past upstream/scala-3 @ `1561d8dc`.
- Working tree clean. `scalafmtCheckAll` green.
- `.planning/` ignored locally via `.git/info/exclude` only — `.gitignore` untouched (REQ WORKFLOW-05).
- Ready for **Plan 02** (`project/Commons.scala` edits) and **Plan 03** (CI workflow via sbt-github-actions) to layer on top.
- **Do not push** — per user rule, branch stays local until manual review.

## scalafmtCheckAll output (last lines)

```
[info] set current project to commons (in build file:/Users/bkozak/IdeaProjects/scala-commons3/)
[success] Total time: 0 s, completed May 30, 2026, 10:01:30 PM
```

## Self-Check: PASSED

- [x] `.scalafmt.conf` contains `runner.dialect = scala3` (verified via Read).
- [x] `project/plugins.sbt` contains `sbt-mima-plugin" % "1.1.5"` (verified via prior commit d5cd2cc8).
- [x] 3 commits on `01-cross-compile-infra` past `upstream/scala-3`: `d5cd2cc8`, `29e638da`, `67867274` (verified via git log).
- [x] `sbt scalafmtCheckAll` exits 0 (verified).
- [x] `git status --porcelain` empty (verified).
- [x] `git check-ignore .planning/STATE.md` exit 0 (verified — `.planning/` locally ignored).
- [x] No `.planning/` paths in diff range (verified — only `project/plugins.sbt`, `.scalafmt.conf`, and 9 source files appear).

---
*Phase: 01-cross-compile-infrastructure*
*Completed: 2026-05-30*
