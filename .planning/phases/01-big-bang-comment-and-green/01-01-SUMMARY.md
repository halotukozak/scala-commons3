---
phase: 01-big-bang-comment-and-green
plan: 01
subsystem: build-infra
tags: [build, scala3, scalafmt, ci, infrastructure]
dependency-graph:
  requires:
    - upstream/scala-3 @ 1561d8dc
  provides:
    - 01-big-bang branch with Scala 3 only build
    - Single-axis CI for Scala 3.8.2 x Temurin 17/21/25
    - scalafmt single scala3 dialect (no fileOverride)
  affects:
    - project/Commons.scala
    - .scalafmt.conf
    - .github/workflows/ci.yml
    - core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala (enum->e rename)
tech-stack:
  added:
    - made 0.1.1 (unconditional on core)
  patterns:
    - aggregate-level disable (commented entries in jvm aggregate)
    - single-axis CI matrix (no crossScalaVersions)
    - minimum-diff scalacOptions migration
key-files:
  created: []
  modified:
    - project/Commons.scala
    - build.sbt (verified clean stub, no change needed)
    - .scalafmt.conf
    - .github/workflows/ci.yml
    - core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala
    - core/jvm/src/test/scala/com/avsystem/commons/macros/TypeClassDerivationTest.scala
    - core/src/main/scala/com/avsystem/commons/di/Components.scala
    - core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
    - core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala
    - core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala
    - core/src/main/scala/com/avsystem/commons/misc/Delegation.scala
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/GenObjectCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/TupleGenCodecs.scala
decisions:
  - "Branch 01-big-bang cut at upstream/scala-3 @ 1561d8dc (matches plan baseline; no advancement since RESEARCH)"
  - "made 0.1.1 (not 0.1.0) - plan-locked madeVersion bump after Phase 5 work confirmed pivot strategy"
  - "Dropped analyzer/jetty/spring from jvm aggregate via comment-out (declarations preserved per RESEARCH minimum-diff pattern); jvm aggregate now {macros, core, mongo, hocon}"
  - "scalafmt enum->e rename in GenKeyCodec.scala absorbed into style commit (Rule 1 auto-fix; pre-existing keyword collision under scala3 dialect)"
  - "Three commits delivered (build / style / ci) per plan locked strategy for review clarity"
metrics:
  duration_min: 5
  completed_date: "2026-06-01"
  tasks: 3
  commits: 3
---

# Phase 01 Plan 01: build-infra-pivot Summary

Scala 3 only build pivot: scaffolded `01-big-bang` from `upstream/scala-3 @ 1561d8dc`, migrated `project/Commons.scala` to single Scala 3 axis (drop `crossScalaVersions`, migrate scalac options), flipped `.scalafmt.conf` to single `scala3` dialect, regenerated `ci.yml` for single Scala 3 axis x Temurin 17/21/25.

## Branch + Baseline

- Cut from `upstream/scala-3 @ 1561d8dca8e15f5f4e6e0e8a6427f0ca179f13e0` (matches research baseline `1561d8dc`).
- Branch tip after plan: `2e5e22e0` (3 commits ahead of `upstream/scala-3`).
- Tree clean; not pushed.

## Commits

| # | Hash       | Type   | Message                                                            |
| - | ---------- | ------ | ------------------------------------------------------------------ |
| 1 | `a4cb99e2` | build  | pivot to Scala 3 only, migrate scalac options                      |
| 2 | `f00976bd` | style  | switch to single scala3 dialect (+ enum->e rename in GenKeyCodec)  |
| 3 | `2e5e22e0` | ci     | regenerate workflow for single Scala 3 axis on Java 17/21/25       |

## Scalac Options: Before → After

| Flag                                       | Before (2.13)   | After (3.8.2)   |
| ------------------------------------------ | --------------- | --------------- |
| `-encoding utf-8`                          | kept            | kept            |
| `-Yrangepos`                               | present         | dropped         |
| `-explaintypes`                            | present         | renamed         |
| `-explain-types`                           | absent          | added (Scala 3) |
| `-feature`                                 | kept            | kept            |
| `-deprecation`                             | kept            | kept            |
| `-unchecked`                               | kept            | kept            |
| `-language:implicitConversions`            | kept            | kept            |
| `-language:existentials`                   | kept            | kept            |
| `-language:dynamics`                       | kept            | kept            |
| `-language:experimental.macros`            | present         | dropped         |
| `-language:higherKinds`                    | kept            | kept            |
| `-Xfatal-warnings`                         | present         | dropped (deferred via commented -Werror) |
| `-Xsource:3`                               | present         | dropped         |
| `-Xlint:-missing-interpolator,...`         | present         | dropped         |
| `-Ycache-plugin-class-loader:last-modified`| present         | dropped         |
| `-Ycache-macro-class-loader:last-modified` | present         | dropped         |
| `if (scalaBinaryVersion == "2.13") Seq(-Xnon-strict-patmat-analysis, ...)` | present | block deleted   |
| `// "-Werror"`                             | absent          | added as TODO   |
| `unidoc / scalacOptions += "-Ymacro-expand:none"` | present  | dropped         |

## Aggregate Membership: Before → After

`jvm` aggregate:
- Before: `analyzer, macros, core, jetty, mongo, hocon, spring`
- After:  `macros, core, mongo, hocon` (analyzer/jetty/spring commented out with `// TODO[scala3-port]: ... (effort)` markers; declarations preserved)

`js` aggregate: `core-js, mongo-js` (unchanged).

`root` aggregate: `jvm, js` (unchanged).

`jvm2` and `benchmark`/`benchmark-js`/`comprof` were not in upstream `jvm` aggregate; no change.

## ci.yml Matrix: Before → After

| Field                | Before                       | After                                 |
| -------------------- | ---------------------------- | ------------------------------------- |
| Scala                | `[2.13.18]` (single)         | `[3.8.2]`                             |
| Java                 | `[temurin@17, 21, 25]`       | `[temurin@17, 21, 25]` (unchanged)    |
| Build step           | compile + test/compile       | compile + test/compile + scalafmtCheckAll + scalafmtSbtCheck (folded) |
| Added jobs           | `mima`, `scalafmt`           | none (folded into main build step)    |
| MongoDB/Node preamble| present                      | present (unchanged)                   |

`ci.yml` shrank from 99 lines to ~11 lines net (4 insertions, 92 deletions in regen commit).

## Source Directory Helper

`mkSourceDirs(base, scalaBinary, conf)` → `mkSourceDirs(base, conf)`; dropped the `scala-$scalaBinary` rung. Call sites in `sourceDirsSettings` updated.

## Dependency Changes

- `madeVersion = "0.1.1"` (unconditional on `core`).
- Dropped `scala-reflect` from `macros` (Scala 2 only).
- Dropped `if (scalaBinaryVersion.value == "3") ... made ...` guards (we are Scala 3 only).
- `analyzer`'s `scala-compiler` dep kept on the `lazy val` (declaration preserved; no longer aggregated).

## Deviations from Plan

### Rule 1 - Bug: `enum` keyword collision in GenKeyCodec.scala (Task 3)

- **Found during:** Task 3 `scalafmtAll` after dialect flip.
- **Issue:** `enum => enum.name()` lambda parameter uses `enum` which is a Scala 3 reserved keyword; scala3-dialect parser rejected it.
- **Fix:** Renamed lambda parameter `enum` → `e` in `GenKeyCodec.scala:90`.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala`.
- **Commit:** `f00976bd` (folded into the style/scalafmt dialect commit per the locked single-style-commit strategy; this isn't pure reformatting but is part of the dialect transition and inseparable from making `scalafmtCheckAll` green).
- **Precedent:** Identical fix appears in STATE.md "Plan 01 (2026-05-30)" — pre-existing known issue from prior pivot work.

### Plan verify expectation mismatch (Task 1, non-blocking)

- **Found during:** Task 1 verify step `find . -type d -name 'scala-2.13' -not -path './.planning/*'`.
- **Issue:** Plan asserted "MUST return empty" but upstream `scala-3 @ 1561d8dc` carries one tracked source directory: `mongo/jvm/src/test/scala-2.13/` (two test files: `MongoPolyDataTest.scala`, `PolyDataWithCustomImplicits.scala`). Additional build-artifact `scala-2.13/` directories exist under `target/` and `.bloop/` but are not source paths.
- **Disposition:** Not a regression — pre-existing upstream condition; minimum-diff principle wins. Test directory left untouched (will be addressed in Plan 05 tests phase if it surfaces).
- **No fix applied.**

### Scalafmt absorbed more files than plan anticipated (Task 3, expected per RESEARCH Pitfall 5)

- **Files reformatted by dialect flip:** 12 files (`.scalafmt.conf` + `project/Commons.scala` + 10 `.scala` sources under `core/`).
- **Disposition:** Expected per plan ("If this rewrites files outside `project/` and `.scalafmt.conf` ... that's expected"). Single style commit per locked strategy.

## Pre-Commit Audits (all green)

- `git diff upstream/scala-3 -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'`: empty (QUALITY-01).
- `git diff -- .planning/`: empty (WORKFLOW-05).
- Commit messages use `build:` / `style(scalafmt):` / `ci:` prefixes; no GSD nomenclature (WORKFLOW-04).
- `sbt -batch 'show scalaVersion'` → `3.8.2` (build loads).
- `sbt -batch scalafmtCheckAll` → success (verified post-Commit A and after final state).
- `sbt -batch scalafmtSbtCheck` → success.

## Requirements Satisfied

BUILD-01, BUILD-02, BUILD-03, BUILD-04, BUILD-05, QUALITY-01, QUALITY-02, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05.

## Self-Check: PASSED

- Branch `01-big-bang` exists, tip `2e5e22e0`, 3 commits ahead of `upstream/scala-3`.
- Commit hashes verified in `git log`: `a4cb99e2`, `f00976bd`, `2e5e22e0`.
- File modifications verified: `project/Commons.scala`, `.scalafmt.conf`, `.github/workflows/ci.yml` all changed vs upstream.
- `grep` audits green: no `crossScalaVersions`, no `-Xsource:3`, no `-Wconf`, no `fileOverride`, no `Scala213Source3`, no `2.13` in ci.yml.
- `sbt show scalaVersion` returns `3.8.2`.
- `sbt scalafmtCheckAll` exits 0.
