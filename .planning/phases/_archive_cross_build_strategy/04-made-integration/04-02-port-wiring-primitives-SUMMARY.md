---
phase: 04-made-integration
plan: 02
subsystem: core
tags: [core, scala-3, made, wiring-primitives, deprecated-skip, scope-revision]
requires:
  - "branch 04-made-integration @ bf8e961a (Plan 01 tip — madeVersion pinned to 0.1.0)"
  - "made 0.1.0 on classpath (build wiring from Phase 1 Plan 02 + Plan 04-01)"
provides:
  - "Scala 3 sources for Opt/NOpt/OptArg/OptRef with `given Default[…]` instances"
  - "Top-level `export made.annotation.*` + `export made.TransparentWrapping` in `com.avsystem.commons.serialization`"
  - "Deprecated `OptCompat`/`NOptCompat`/`OptRefCompat` traits intentionally dropped (replaced by in-companion `given Conversion[…, Iterable[…]]`)"
affects:
  - "core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala (NEW)"
  - "core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala (NEW)"
tech-stack:
  added:
    - "made.Default given instances for the four Opt wrapper value-classes (Scala 3 only)"
  patterns:
    - "Minimum-island porting — drop `*Compat` mixin shims, in-companion `given Conversion` supersedes deprecated `opt2Iterable`"
    - "Cross-compile source layout via `scala-3/` source dir (set up in Phase 1 Plan 02)"
key-files:
  created:
    - core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala
    - core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala
    - core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala
    - core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala
    - core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
  modified: []
decisions:
  - "User-approved Option A after scope explosion check: ship ONLY the 5 cherry-picked wiring-primitive files; Scala 3 `commons-core/compile` green-gate WITHDRAWN from this plan"
  - "Dropped `extends OptCompat`/`NOptCompat`/`OptRefCompat` clauses per memory rule `feedback_dont_port_deprecated.md` (the deprecated `opt2Iterable` shims are superseded by in-companion `given Conversion[…, Iterable[…]]`)"
  - "`compat.scala` NOT ported — references deferred-Phase-5 types (`GenCodec`, `GenKeyCodec`) and its three relevant traits hold only deprecated APIs"
  - "Scala 3 `commons-core/compile` remains RED (~136 errors) — this is the status quo from BEFORE this plan plus a small number of new dup-def errors caused by the cherry-pick; full Scala 3 commons-core green-up DEFERRED to a future plan (Phase 5 CORE-02 / source organization)"
  - "Sanity-gate auto-fix (Rule 1): scalafmt rejected the verbatim-cherry-picked files; ran `scalafmtAll` and committed the reformat separately as `style(scalafmt):` so the `scalafmtCheckAll` regression gate stays green"
metrics:
  duration: "~35 min (including reset-revert + sanity gates + scope-revision SUMMARY)"
  tasks_completed: "2 of 2 (as written) — but plan SCOPE revised mid-execution; see deviations"
  files_modified: 5
  completed: "2026-05-31T22:57:00Z"
---

# Phase 4 Plan 02: Port Wiring Primitives Summary

**One-liner:** Cherry-picked 5 Scala 3 wiring-primitive source files (`Opt`/`NOpt`/`OptArg`/`OptRef` + `madeAnnotationAliases`) from `origin/master`, dropped the three deprecated `*Compat` mixin clauses, and intentionally LEFT `commons-core/compile` on Scala 3 broken (deferred to a future phase after a user-driven scope reduction).

## What Happened

### Task 1 — Port the four `misc/Opt*.scala` files

`git show origin/master:<path>` written into the branch working tree for each of `Opt.scala`, `NOpt.scala`, `OptArg.scala`, `OptRef.scala`. Then:

- `Opt.scala`: `object Opt extends OptCompat {` → `object Opt {`
- `NOpt.scala`: `object NOpt extends NOptCompat {` → `object NOpt {`
- `OptRef.scala`: `object OptRef extends OptRefCompat {` → `object OptRef {`
- `OptArg.scala`: unchanged from `origin/master` (its companion never extended a `*Compat` trait)

All four files import `made.Default`. None import any of `GenCodec`/`GenKeyCodec`/`GenObjectCodec`/`GenRef`/`HasGenCodec` (Phase 5 types).

### Task 2 — Port `madeAnnotationAliases.scala` and confirm `compat.scala` is NOT created

`madeAnnotationAliases.scala` copied verbatim from `origin/master`; contains exactly 6 `export made.*` lines (5 annotations + `TransparentWrapping`). `compat.scala` deliberately NOT created on this branch.

### Final shape committed (Commit A — `66fb1158`)

```
core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala                  | 180 +++
core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala                   | 192 +++
core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala                | 143 +++
core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala                | 155 +++
core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala | 15 +
5 files changed, 685 insertions(+)
```

### Sanity-gate auto-fix (Commit B-prime — `7e3a3035`)

`scalafmtCheckAll` failed on the 5 cherry-picked files because the current scalafmt dialect / fileOverride config (Phase 1 settling) reflows them differently than `origin/master` had. Ran `scalafmtAll`; 5 sources reformatted; committed separately as `style(scalafmt): reformat ported scala-3 wiring primitives`. Re-ran `scalafmtCheckAll` → green. Re-ran 2.13 `commons-core/compile` → green (0 errors).

## Deviations from Plan

### Major: scope revision after architectural-gate check (Rule 4)

The PLAN as originally written had only the 2 tasks documented above (port 5 files, drop `*Compat` clauses). Mid-execution, the user added a `++3 commons-core/compile` GREEN gate ("ship Scala 3 working"). Pursuing that gate required relocating ~34 scala-2 macro source files from `core/src/main/scala/` into `core/src/main/scala-2.13/` (mass `git mv`). This was staged as a draft Commit B.

The scope explosion check flagged that the relocation:
- touches files completely unrelated to Plan 04-02's `made` wiring objective,
- duplicates work that belongs to Phase 5 (CORE-01 / source-tree organization),
- conflicts with `minimum-island` principle established in `04-CONTEXT.md`.

User reversed the decision (Option A): **revert all 34 staged moves; keep ONLY Commit A**; accept the Scala 3 `commons-core` red status as deferred. Action taken: `git reset --hard HEAD` to drop the unstaged work cleanly back to `66fb1158`.

### Auto-fixed Issues

**1. [Rule 1 — Sanity gate regression] scalafmt failed on the 5 cherry-picked files**

- **Found during:** Final sanity gate (`scalafmtCheckAll`)
- **Issue:** 5 files cherry-picked verbatim from `origin/master` don't satisfy the current scalafmt dialect/fileOverride config that Phase 1 settled on
- **Fix:** Ran `sbt scalafmtAll`; 5 sources reformatted; invariants re-verified (no `extends *Compat`, all 4 `import made.Default`, no deferred-type imports, 6 `export made.*` lines preserved)
- **Files modified:** the same 5 ported files
- **Commit:** `7e3a3035` (`style(scalafmt): reformat ported scala-3 wiring primitives`)
- **Note:** This deviates from the user's verbatim "Branch tip = 66fb1158" success criterion. The other success criterion ("`sbt scalafmtCheckAll` exits 0") could not be satisfied without writing this commit. The auto-fix took priority; the branch tip is therefore `7e3a3035`, with Commit A (`66fb1158`) immediately below it.

## Compile State

| Build               | Result | Notes                                                                                                                                          |
| ------------------- | ------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `++2.13.18 commons-core/compile` | GREEN  | 0 errors, `done compiling`. 951 .class files in `core/target/scala-2.13/classes/`. 2.13 unaffected by the cherry-pick (files live in `scala-3/`). |
| `scalafmtCheckAll`               | GREEN  | After the format auto-fix above.                                                                                                               |
| `++3.8.2 commons-core/compile`   | RED (deferred) | ~136 errors. Status quo from before this plan + a small number of dup-def errors from the cherry-pick. Explicitly accepted scope. To be resolved in a future plan (Phase 5 CORE-02 / source-tree organization). |

## Requirement Coverage

- **MADE-01 — Wire `made` integration on Scala 3:** Partially satisfied. The 4 `Opt*` wrappers' `given Default[…]` instances and the `made.annotation` aliases are now on the branch. Remaining work (`GenCodec`/`GenObjectCodec`/`HasGenCodec` derivation surfaces) stays in Phase 5+.
- **DEPR-01 — Skip deprecated symbols with stdlib/library replacements:** Satisfied for this scope. Three `*Compat` mixin clauses dropped; `compat.scala` not ported.

## Memory Rules Honored

- `feedback_dont_port_deprecated.md` — `OptCompat`/`NOptCompat`/`OptRefCompat` and their `opt2Iterable` shims skipped; the in-companion `given Conversion[Opt[A], Iterable[A]]` (etc.) supersedes them.
- `feedback_fix_dont_suppress_warnings.md` — No new `@nowarn` or `-Wconf` introduced.

## Commits Added

| SHA        | Subject                                                                            |
| ---------- | ---------------------------------------------------------------------------------- |
| `66fb1158` | `feat(core): port made-based Opt/NOpt/OptArg/OptRef wiring primitives to Scala 3` |
| `7e3a3035` | `style(scalafmt): reformat ported scala-3 wiring primitives`                       |

(Plan 04-01 tip `bf8e961a` is the immediate predecessor.)

## Deferred / Follow-Up

- Scala 3 `commons-core/compile` green-up — see Phase 5 (CORE-02 / source-tree organization). Will likely entail the 34-file relocation of scala-2 macro-def sources from `scala/` into `scala-2.13/` that was reverted from this plan, plus the dup-def cleanup the cherry-pick introduced.
- `compat.scala` — never to be ported in its fork-master shape (deprecated content + deferred-type imports). Any non-deprecated piece can be re-derived in Phase 5+ once `GenCodec*` is available.
- Full `made` annotation alias surface (beyond the 6 currently re-exported) — backlog, driven by consumer code as later phases land it.

## Self-Check: PASSED

- `core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala` — FOUND
- `core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala` — FOUND
- `core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala` — FOUND
- `core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala` — FOUND
- `core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala` — FOUND
- `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala` — ABSENT (as required)
- Commit `66fb1158` — FOUND in `git log`
- Commit `7e3a3035` — FOUND in `git log`
- `++2.13.18 commons-core/compile` — exit 0, 0 errors
- `scalafmtCheckAll` — exit 0
- No `extends *Compat` clauses in the 4 misc/ files
- All 4 misc/ files `import made.Default`
- `madeAnnotationAliases.scala` has exactly 6 `^export made\.` lines
- Working tree clean before SUMMARY write
