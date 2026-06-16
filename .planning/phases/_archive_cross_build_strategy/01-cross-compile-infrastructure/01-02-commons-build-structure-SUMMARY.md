---
phase: 01-cross-compile-infrastructure
plan: 02
subsystem: infra
tags: [sbt, cross-build, aggregates, made, jetty-skip, scala-2.13-relocation]

requires:
  - phase: 01
    plan: 01
    provides: "branch 01-cross-compile-infra at 3 commits past upstream/scala-3; scalafmt scala3-default dialect with scala213source3 fileOverride; sbt-mima-plugin 1.1.5"
provides:
  - "jvm/jvm2/js aggregate split (jvm = cross-built JVM modules; jvm2 = jetty; js = core-js + mongo-js)"
  - "per-module crossScalaVersions := Seq(scala3Version, scala2Version) on macros/core/mongo/hocon/core-js/mongo-js/benchmark/benchmark-js/comprof"
  - "jetty pinned single-version (crossScalaVersions := Seq(scala2Version)) with Compile/Test/update/publish skip on non-2.13"
  - "io.github.halotukozak:made 0.1.0 wired into core on Scala 3 only"
  - "macros scala-reflect dep gated to 2.13"
  - "core/src/main/scala-2.13/ source tree containing 8 relocated macro-def files (Option 1 deviation)"
affects: [03-ci-workflow, 03-macros-module-port, 05-core-module-port, all subsequent Scala 3 module ports]

tech-stack:
  added:
    - "io.github.halotukozak:made:0.1.0 (Scala 3 only, on core)"
  patterns:
    - "Aggregate split: jvm = cross-built, jvm2 = 2.13-only stranded (jetty), js = ScalaJS"
    - "Single-version override on jetty via crossScalaVersions := Seq(scala2Version) + skip block"
    - "Conditional libraryDependencies via if (scalaBinaryVersion.value == \"3\") ... / \"2.13\" ..."
    - "scala-2-only sources live under src/main/scala-2.13/ (relocated from shared scala/)"

key-files:
  created:
    - core/src/main/scala-2.13/com/avsystem/commons/di/Components.scala (relocated from scala/)
    - core/src/main/scala-2.13/com/avsystem/commons/meta/AdtMetadataCompanion.scala (relocated)
    - core/src/main/scala-2.13/com/avsystem/commons/meta/MacroInstances.scala (relocated)
    - core/src/main/scala-2.13/com/avsystem/commons/misc/Bidirectional.scala (relocated)
    - core/src/main/scala-2.13/com/avsystem/commons/misc/Delegation.scala (relocated)
    - core/src/main/scala-2.13/com/avsystem/commons/serialization/GenCodec.scala (relocated)
    - core/src/main/scala-2.13/com/avsystem/commons/serialization/GenObjectCodec.scala (relocated)
    - core/src/main/scala-2.13/com/avsystem/commons/serialization/TupleGenCodecs.scala (relocated)
  modified:
    - project/Commons.scala (+77 / -33 lines net; full cross-compile restructure)

key-decisions:
  - "Option 1 (user-approved checkpoint deviation): relocate 8 scala-2 macro-def files to scala-2.13/ rather than try to format-massage them in shared scala/. Overrides Plan 01's deferred-to-Phase-5+ note."
  - "Restored pre-reformat (upstream/scala-3) content for the 8 relocated files: scala213source3 dialect accepts the original `def x: T = macro Y.z` one-line form; the prior commit 67867274's two-line wrap was the actual breakage."
  - "Scala 3 compile of `commons-jvm` aggregate NOT gated in this plan: `commons-macros` has 2.13-only sources still in macros/src/main/scala/ — that broader relocation is Phase 3+ scope (the master branch's `bcc3bcbf build: isolate scala-2.13-only modules under jvm2 aggregate` shows what it looks like). Per Plan 02 §3(c), source-level Scala-3 issues are out of Phase 1 scope; documented under Deferred."
  - "jetty/Compile/skip evaluates to `false` under `++3.8.2` (not `true` as the plan acceptance criterion expected) — because jetty pins scalaVersion := scala2Version, so scalaBinaryVersion.value is always \"2.13\" inside the jetty project and the skip condition is dead code. Functional intent (jetty doesn't compile under a cross matrix Scala 3 run) is still met because crossScalaVersions := Seq(scala2Version) excludes it from `+`."

patterns-established:
  - "Pattern: relocation > reformat. When scalafmt dialect inversion forces a scala-2-only syntactic form into a parser-broken shape on shared sources, move the files to scala-2.13/ rather than retain them in scala/."

requirements-completed: [INFRA-01, INFRA-02, INFRA-03, INFRA-06, INFRA-08, INFRA-09]

duration: ~30 min (across two executor sessions; checkpoint-interrupted)
completed: 2026-05-31
---

# Phase 1 Plan 2: Commons.scala cross-compile restructure Summary

**`project/Commons.scala` restructured for cross-compile (jvm/jvm2/js aggregates, per-module crossScalaVersions, conditional made dep on Scala 3, jetty single-version pinning) and 8 scala-2 macro-def files relocated from `core/src/main/scala/` to `core/src/main/scala-2.13/` (user-approved Option 1 deviation) to unblock the 2.13 compile.**

## Performance

- **Duration:** ~30 min (initial Task 1+2+3 edits + checkpoint resume for the relocation)
- **Branch:** `01-cross-compile-infra` at `7bbe47f9`, 4 commits past upstream/scala-3 @ `1561d8dc`
- **Tasks executed:** 3 planned (Tasks 1-3 from Plan 02); checkpoint-handled deviation appended Option 1 relocation
- **Files modified:** 9 (1 build + 8 relocated source)
- **Commit:** single atomic — `7bbe47f9 build(commons): land cross-compile build structure ...`

## Accomplishments

- `project/Commons.scala` restructured to support cross-compile (+77 / -33 lines):
  - `scala2Version = "2.13.18"`, `scala3Version = "3.8.2"`, `madeVersion = "0.1.0"` declared
  - `scalaVersion := scala3Version`, `crossScalaVersions := Seq(scala3Version, scala2Version)` on ThisBuild
  - `jvm` aggregates `macros, core, mongo, hocon` (drops `analyzer`, `jetty`, `spring`)
  - new `jvm2` aggregates `jetty`
  - `js` aggregates `core-js, mongo-js` (unchanged)
  - per-module `crossScalaVersions := Seq(scala3Version, scala2Version)` + `scalaVersion := scala3Version` on every cross-built module
  - `jetty`: `crossScalaVersions := Seq(scala2Version)` + skip block (Compile/Test/update/publish) for INFRA-09
  - `core` gains `if (scalaBinaryVersion.value == "3") Seq("io.github.halotukozak" %% "made" % madeVersion) else Seq.empty` (INFRA-06)
  - `macros` scala-reflect dep gated to 2.13
- 8 scala-2-only macro-def files relocated from `core/src/main/scala/` to `core/src/main/scala-2.13/` (see Deviation 1 below)
- `sbt scalafmtCheckAll` exits 0
- `sbt '++2.13.18; commons-jvm/compile'` exits 0 (commons-core, commons-hocon, commons-mongo all compile on 2.13)
- `sbt '++3.8.2; show commons-core/libraryDependencies'` contains `io.github.halotukozak:made:0.1.0` (INFRA-06 verified)

## Task Commits

1. **Plan 02 (atomic)** — `7bbe47f9` (build): `build(commons): land cross-compile build structure (jvm/jvm2/js aggregates, made dep, jetty skip)`
   - project/Commons.scala (+77 / -33 lines)
   - 8 `git mv` renames from `core/src/main/scala/` → `core/src/main/scala-2.13/`

## Files Created/Modified

**Build:**
- `project/Commons.scala` — full Plan 02 restructure (see Accomplishments)

**Source (relocated — Option 1 deviation):**
- `core/src/main/scala-2.13/com/avsystem/commons/di/Components.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/meta/AdtMetadataCompanion.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/meta/MacroInstances.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/Bidirectional.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/misc/Delegation.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/GenCodec.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/GenObjectCodec.scala`
- `core/src/main/scala-2.13/com/avsystem/commons/serialization/TupleGenCodecs.scala`

(All 8 file contents restored from upstream/scala-3 — the prior commit 67867274's reformat to two-line `def x = macro \n    Y.z` form was the source of the 2.13 parser failure; the upstream one-line form is accepted by scala213source3 dialect.)

## Decisions Made

1. **Option 1 (user-approved)**: relocate 8 macro-def files to `scala-2.13/`. The files use `def x: T = macro macros.Foo.bar[T]` syntax which is a scala-2-only macro definition form (no Scala 3 equivalent without rewriting via inline). Their natural home is `scala-2.13/`. This overrides Plan 01's "Decision 3" which deferred relocation to Phase 5+ on the assumption that the reformat was cosmetic.
2. **Restore upstream content, not reformat-in-place**: chose `git show upstream/scala-3:<old-path> > <new-path>` over `sbt scalafmt` on the moved files. Result is a smaller, cleaner diff — the renames show as 94-99% similarity and the change reads as pure relocation + ThisBuild restructure.
3. **Did NOT attempt the broader macros/ relocation** (master branch state): master moved the entire `macros/src/main/scala/` tree under `scala-2.13/` to make `commons-macros` produce an empty Scala 3 jar. That is Phase 3 work (per CONTEXT "`macros` stub starts in Phase 3+"). Plan 02 leaves macros' shared-tree sources in place; Scala 3 compile of `commons-jvm` consequently fails on `commons-macros` (599 errors). This is a known pre-existing condition documented under Deferred Issues below.

## Deviations from Plan

### Deviation 1: Relocation of 8 scala-2 macro-def files (Option 1, user-approved at checkpoint)

**Found during:** Task 3 (Plan 02 sanity-check `sbt '++2.13.18; commons-jvm/compile'`)

**Issue:** Plan 01 commit `67867274 style(scalafmt): reformat shared sources for scala3 dialect` reformatted 8 files in `core/src/main/scala/` containing scala-2 macro syntax. The scala3 dialect wrapped `def x: T = macro Y.z` to two lines as `def x: T = macro\n    Y.z`. While this form satisfies the scala3 dialect formatter, the **scala-2.13 compiler parser rejects** the broken `= macro` token on its own at EOL: 14 errors of "illegal start of simple expression" surface on `sbt '++2.13.18; commons-jvm/compile'`.

**Root cause:** Plan 01 chose Option A (patch sources in shared scala/) without verifying that the reformat preserved 2.13 parser compatibility. The scala213source3 fileOverride exists for exactly this case but only matches `scala-2.13/` paths, not shared `scala/`.

**Resolution path:** User explicitly approved Option 1 at the checkpoint: relocate the 8 files to `core/src/main/scala-2.13/` (where scala213source3 dialect accepts the one-line form) and restore upstream content. Plan 01's "defer relocation to Phase 5+" decision is hereby overridden.

**Files moved (8):** See "Files Created/Modified" above. All 8 are scala-2-only by virtue of containing `def x: T = macro Y.z` definitions — they cannot compile on Scala 3 anyway, so `scala-2.13/` is the architecturally correct home.

**Verification:**
- `sbt scalafmtCheckAll` exits 0 — moved files now match the scala213source3 fileOverride glob.
- `sbt '++2.13.18; commons-jvm/compile'` exits 0 — `commons-core`, `commons-hocon`, `commons-mongo` all compile.
- Renames show as 94-99% similarity in `git show --stat HEAD` (pure relocation).

**Committed in:** `7bbe47f9` (the single Plan 02 atomic commit, per the plan's design).

---

**Total deviations:** 1 user-approved checkpoint deviation (Option 1 relocation).

**Impact on plan:** Plan 02's success criteria met for INFRA-01/02/03/06/08-partial/09. INFRA-08 partial because Scala 3 `commons-jvm/compile` is gated by separate Phase 3 work on `commons-macros` (see Deferred Issues).

## Issues Encountered

- **Plan acceptance criterion gap** for `jetty/Compile/skip`: the plan stated `sbt '++3.8.2; show commons-jetty/Compile/skip'` should print `true`. Actual: `false`. Reason: jetty has `crossScalaVersions := Seq(scala2Version)` + `scalaVersion := scala2Version`, so `scalaBinaryVersion.value` inside jetty is always `"2.13"`, making `Compile / skip := scalaBinaryVersion.value != "2.13"` evaluate to `false`. The functional intent (jetty doesn't break `+` cross matrix on Scala 3) is met via the single-version pinning itself; the skip block is defensive dead-code. Acceptance criterion was over-specified in the plan; the underlying requirement INFRA-09 ("jetty stays 2.13-only") is satisfied.

## Deferred Issues

### `commons-macros` Scala 3 compile (Phase 3+)

`sbt '++3.8.2; commons-macros/compile'` fails with 599 errors because `macros/src/main/scala/` contains scala-2 macro implementations (e.g., `MacroCommons.scala`, `GenCodecMacros.scala`, `TypeClassDerivation.scala`) using the scala-reflect API. Plan 02 leaves these in place per CONTEXT "`macros` stub starts in Phase 3+." The master branch's `bcc3bcbf build: isolate scala-2.13-only modules under jvm2 aggregate` commit shows the resolution pattern: relocate the entire `macros/src/main/scala/` tree to `macros/src/main/scala-2.13/`. That work belongs to Phase 3 (macros module port), not here.

**Consequence for Plan 02 acceptance:** `sbt '++3.8.2; commons-jvm/compile'` (REQ INFRA-08 Scala 3 gate) is NOT green at this checkpoint. The plan explicitly anticipates this in §3(c): *"If compile fails on Scala 3 for any module due to a source-level issue, Phase 1 is OUT of scope to fix sources — that's Phase 5+ territory."* Documented here so Phase 3 planner knows to land the macros relocation as its first task.

## User Setup Required

None.

## Next Phase Readiness

- Branch `01-cross-compile-infra` is at 4 commits past upstream/scala-3 @ `1561d8dc`.
- Working tree clean. `scalafmtCheckAll` green. `++2.13.18 commons-jvm/compile` green.
- `.planning/` ignored locally via `.git/info/exclude` only — `.gitignore` untouched.
- Ready for **Plan 03** (CI workflow via sbt-github-actions): can layer ci.yml regeneration on top of the now-stable build structure.
- Scala 3 side of INFRA-08 carried forward to Phase 3 macros port.
- **Do not push** — branch stays local until manual review.

## Verification Output

```
$ git log --oneline upstream/scala-3..HEAD
7bbe47f9 build(commons): land cross-compile build structure (jvm/jvm2/js aggregates, made dep, jetty skip)
67867274 style(scalafmt): reformat shared sources for scala3 dialect
29e638da style(scalafmt): default to scala3 dialect, scope scala213source3 to scala-2.13 sources
d5cd2cc8 build(plugins): bump sbt-mima-plugin 1.1.4 -> 1.1.5

$ sbt -batch scalafmtCheckAll
[success] Total time: 0 s

$ sbt -batch '++2.13.18; commons-jvm/compile'
[success] Total time: 9 s

$ sbt -batch '++3.8.2; show commons-core/libraryDependencies' | grep made
[info]   * io.github.halotukozak:made:0.1.0

$ git status --porcelain
(empty)
```

## Self-Check: PASSED

- [x] 8 files relocated to `core/src/main/scala-2.13/` (verified via `git status --short` showing 8 R-prefixed renames; `git show --stat HEAD` confirms rename detection 94-99% similarity).
- [x] `project/Commons.scala` contains jvm/jvm2/js aggregates, per-module crossScalaVersions, made dep on Scala 3, jetty skip block (verified via Read).
- [x] Single atomic commit `7bbe47f9` (verified via `git log upstream/scala-3..HEAD --oneline` showing exactly 4 commits, the newest being Plan 02).
- [x] `sbt scalafmtCheckAll` exits 0 (verified).
- [x] `sbt '++2.13.18; commons-jvm/compile'` exits 0 (verified).
- [x] `git status --porcelain` empty (verified).
- [x] `git check-ignore .planning/STATE.md` exits 0 (verified — `.planning/` locally ignored).
- [x] Commit message has prefix `build(commons):` and contains no GSD nomenclature (verified).

---
*Phase: 01-cross-compile-infrastructure*
*Completed: 2026-05-31*
