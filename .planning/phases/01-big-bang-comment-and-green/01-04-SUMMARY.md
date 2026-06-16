---
phase: 01-big-bang-comment-and-green
plan: 04
subsystem: build, scala-js
tags: [scala-3, scala-js, build, big-bang]
requires:
  - "Plan 01-01 (build pivot to Scala 3, scalacOptions migrated)"
  - "Plan 01-02 (commons-core stubbed)"
  - "Plan 01-03 (commons-jvm aggregate green)"
provides:
  - "commons-js aggregate compiles on Scala 3.8.2 + Scala.js 1.21.0"
  - "Scala.js source-map URI flag migrated to Scala 3 syntax"
affects:
  - "project/Commons.scala (jsCommonSettings.scalacOptions)"
tech-stack:
  added: []
  patterns:
    - "Scala 3 + Scala.js 1.x uses `-scalajs-mapSourceURI:from->to` (no `-P:scalajs:` plugin prefix)"
key-files:
  created: []
  modified:
    - "project/Commons.scala (1 line: scalac flag rename)"
decisions:
  - "JS-specific sources required ZERO stubs/edits — every JS file in core/js, mongo/js, benchmark/js compiles cleanly once the build flag is fixed"
  - "The plan's compile-driven commenting/stubbing loop terminated on iteration 1 with a Rule 3 build fix; no source edits needed"
metrics:
  duration: "~2 min"
  completed: "2026-06-01T11:33:45Z"
  tasks_completed: 1
  files_modified: 1
  tags_added: 0
---

# Phase 01 Plan 04: JS Variants Summary

`commons-js` aggregate compiles on Scala 3.8.2 / Scala.js 1.21.0 after migrating the Scala.js source-map URI compiler flag from the legacy `-P:scalajs:` plugin syntax to the Scala 3-native `-scalajs-mapSourceURI:` form; no JS-specific source code needed stubbing.

## What Was Done

Single-line build fix in `project/Commons.scala`:

```diff
-      s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/"
+      s"-scalajs-mapSourceURI:$localDir->$githubDir/v${version.value}/"
```

After this fix:

- `sbt commons-js/compile` exits 0 (8 s clean)
- 128 shared core sources + 11 JS-specific sources + 3 mongo-js + 3 benchmark-js sources all compile
- Only Scala 3 migration warnings remain (`private[this]`, `= _`, `[_]` wildcards) — pre-existing in shared core sources, out of scope per scope boundary

## JS-Specific Source Inventory

Enumerated via `find core/js mongo/js benchmark/js -name '*.scala' -path '*/main/*'`:

| Module        | Files | TODO[scala3-port] tags added |
| ------------- | ----- | ---------------------------- |
| core-js       | 8     | 0                            |
| mongo-js      | 1     | 0                            |
| benchmark-js  | 3     | 0                            |
| **Total**     | 12    | **0**                        |

All 12 JS-platform-only sources are thin shims that consume APIs preserved by the `???` stubs landed in Plans 02 and 03. None required modification.

## Commits

| Hash       | Subject                                                         |
| ---------- | --------------------------------------------------------------- |
| `9ec8c177` | `build(js): migrate Scala.js mapSourceURI flag to Scala 3 syntax` |

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking build issue] Scala.js mapSourceURI flag rejected by Scala 3 compiler**

- **Found during:** Task 1, first `sbt commons-js/compile` invocation
- **Issue:** `bad option: -P:scalajs:mapSourceURI:...` — Scala 3 + Scala.js 1.x dropped the `-P:scalajs:` compiler-plugin prefix in favour of a built-in `-scalajs-mapSourceURI:from->to` flag. Plan 01 migrated `scalacOptions` but the Scala.js-specific flag in `jsCommonSettings` was missed.
- **Fix:** One-line rename in `project/Commons.scala:163`. Same source-map URL produced; only the flag spelling changed.
- **Files modified:** `project/Commons.scala`
- **Commit:** `9ec8c177`

### Positive Deviations

- **Plan estimated MEDIUM commenting volume across all three JS modules** (`core/js`, `mongo/js`, `benchmark/js`). Actual outcome: zero source-level edits. The `???` stubs landed in Plans 02 and 03 (commons-core, mongo) preserved enough runtime API surface that no JS-specific consumer broke.
- **Three plan-anticipated commits** (`refactor(core-js):`, `refactor(mongo-js):`, `refactor(benchmark-js):`) collapsed into one **build commit** because the failure was a build-config blocker, not a source-compat blocker.

## Verification

| Gate                                                          | Result      |
| ------------------------------------------------------------- | ----------- |
| `sbt -batch commons-js/compile`                               | exit 0 (8s) |
| `sbt -batch 'commons-jvm/compile' 'commons-js/compile'`       | exit 0      |
| `sbt -batch scalafmtCheckAll`                                 | exit 0      |
| `git diff` for new `@nowarn` / `-Wconf`                       | 0 matches   |
| `.planning/` paths in commit diff                             | 0 matches   |
| GSD nomenclature in commit messages                           | 0 matches   |
| Conventional-commit prefix on commit                          | `build:` OK |

## Requirements Satisfied

- COMMENT-01, COMMENT-02, COMMENT-03, COMMENT-05 — JS surface is comment/stub-clean (vacuously: nothing needed stubbing)
- COMPILE-01 — `commons-js/compile` exit 0, completing the JVM+JS dual-aggregate compile gate
- QUALITY-01 — no `@nowarn`/`-Wconf` added
- QUALITY-02 — `scalafmtCheckAll` green
- WORKFLOW-04 — conventional commit prefix (`build:`)
- WORKFLOW-05 — no `.planning/` paths in any commit diff

## Notes for Plan 06 (MIGRATION.md update)

Add to `## Source-compat changes` (or equivalent build-flags section):

- Scala.js compiler flag rename: `-P:scalajs:mapSourceURI` → `-scalajs-mapSourceURI`. Affects downstream consumers who copy our `jsCommonSettings`.

## Self-Check: PASSED

- File `project/Commons.scala` modified: FOUND
- Commit `9ec8c177` exists in history: FOUND
- Compile gates: GREEN
- scalafmtCheckAll: GREEN
- No suppressions introduced: VERIFIED
