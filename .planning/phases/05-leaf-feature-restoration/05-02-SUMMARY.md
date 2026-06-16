---
phase: 05-leaf-feature-restoration
plan: 02
subsystem: core/misc
tags: [scala-3, deprecation, leaf-feature, compiletime-error]
requires: [04-05-meta-annotations]
provides:
  - "@deprecated Delegation object with compiletime.error bodies — fail-fast at COMPILE time on every call site"
  - "DELEGATION-01 closed via deprecate-over-restore (no real port, no macro)"
affects:
  - core/src/main/scala/com/avsystem/commons/misc/Delegation.scala
  - core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala
  - MIGRATION.md
tech-stack:
  added: []
  patterns:
    - "@deprecated object + inline given / inline def with `scala.compiletime.error(...)` body"
    - "test file wrapped in `/* @TodoScala3Migration DROPPED: ... */` with empty class shell"
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/Delegation.scala
    - core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala
    - MIGRATION.md
decisions:
  - "Override PLAN-as-written (`???` runtime stub matching fork) with deprecate-over-restore pattern: `@deprecated` object whose `materializeDelegation` (given) and `CurriedDelegation.apply` both have `scala.compiletime.error(...)` bodies. Strictly better fail-fast contract than runtime `NotImplementedError` — call sites break at compile time. Same pattern as slice 5.1 Bidirectional."
  - "Branched off `04-05-meta-annotations` (Phase 4 tip), NOT `05-00-miscmacros-foundation`. The deprecated stub does not extend `DelegationMacros` / `DelegationApplyMacros` traits, so the slice-5.0 dependency drops away."
  - "DelegationTest wrapped in `/* @TodoScala3Migration DROPPED: ... */` with empty class shell — mirrors slice 5.1 BidirectionalTest treatment. Fork drops the test file outright; we preserve an empty wrapper for package layout / minimum-diff."
  - "PR NOT opened per orchestrator override (batch PR creation under user supervision later). Branch pushed to `origin/05-02-delegation-deprecate`."
metrics:
  duration_minutes: 4
  completed_date: "2026-06-02"
  commits: 2
  files_changed: 3
---

# Phase 05 Plan 02: Delegation Deprecate Summary

Deprecate `misc/Delegation` with `compiletime.error` bodies on both the materializer given and `CurriedDelegation.apply` — every call site fails at compile time with a migration message.

## What Changed

`core/src/main/scala/com/avsystem/commons/misc/Delegation.scala` flipped from the Phase-1 big-bang `???` runtime stub to a `@deprecated(..., since = "3.0.0")` object whose `materializeDelegation` (given) and `CurriedDelegation.apply` both carry `scala.compiletime.error(...)` bodies. The `Delegation[A, B]` trait surface and the `Delegation[B]` curried-apply entry point are preserved verbatim — only the macro-materialized members are deprecated.

`core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala` wrapped its full body in `/* @TodoScala3Migration DROPPED: ... */` with an empty `class DelegationTest extends AnyFunSuite` shell — the live `Delegation[Destination[Double]](source)` call would otherwise have hit the new `compiletime.error`. Fork (origin/master) drops this test file entirely; we keep an empty wrapper for package layout / minimum-diff. Mirrors slice 5.1 BidirectionalTest handling.

`MIGRATION.md` §2 (Deprecated on Scala 3) gained the `com.avsystem.commons.misc.Delegation` row pointing at the manual `new Delegation[A, B] { ... }` replacement. Backlog table lost the two stale Delegation rows seeded from Phase-1 `TODO[scala3-port]` markers.

## Scope Override vs PLAN

The plan as written said "match fork — keep `???` stub" and stack on slice 5.0 (`05-00-miscmacros-foundation`) to inherit `DelegationMacros` / `DelegationApplyMacros` traits. **Override (user decision):**

1. Use the `compiletime.error` deprecation pattern (same shape as slice 5.1 Bidirectional). COMPILE-time failure beats runtime `NotImplementedError`.
2. Branch off `04-05-meta-annotations` directly — the deprecated stub doesn't extend `DelegationMacros` / `DelegationApplyMacros`, so the slice-5.0 dependency drops.
3. Branch renamed `05-02-delegation-stub` → `05-02-delegation-deprecate` to reflect the new shape.
4. Push to `origin` (halotukozak fork). No `gh pr create` — batch PR creation deferred to user.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] scalafmt rejected verbatim Write**
- **Found during:** Task 1 verification (`sbt scalafmtCheckAll`)
- **Issue:** Trailing comma after the closing `compiletime.error(...)` argument and a stray comma inside the inner string lists triggered scalafmt format violations on `Delegation.scala`.
- **Fix:** `sbt scalafmtAll` — cosmetic reflow only; no semantic change.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala`
- **Commit:** folded into `7fad5b5f`

No Rule 1 (bug) or Rule 2 (missing critical functionality) fixes were required. The pattern is mechanical translation of the slice-5.1 Bidirectional shape onto the Delegation API surface.

## Commits

| # | Hash       | Subject                                                          |
|---|------------|------------------------------------------------------------------|
| 1 | `7fad5b5f` | feat(scala-3,core): deprecate Delegation (compiletime.error body) |
| 2 | `6b12d4f6` | docs(migration): record Delegation deprecation                    |

Branch `05-02-delegation-deprecate` pushed to `origin @ 6b12d4f6`. NOT pushed to AVSystem upstream. NO PR opened (per orchestrator override).

## Verification Results

| Gate                                                | Result |
|-----------------------------------------------------|--------|
| `sbt commons-core/compile`                          | green  |
| `sbt scalafmtCheckAll`                              | green  |
| `grep -q '@deprecated' Delegation.scala`            | match  |
| `grep -q 'since = "3.0.0"' Delegation.scala`        | match  |
| `grep -q 'scala.compiletime.error' Delegation.scala`| match  |
| `! grep -q '???' Delegation.scala`                  | absent |
| 0 new `@nowarn` / `-Wconf` vs base                  | 0      |
| 0 `.planning/` paths in commit diffs                | 0      |
| 0 GSD nomenclature in commit messages               | 0      |
| Branch pushed to `origin/05-02-delegation-deprecate`| ok     |

## Requirements Satisfied

- **DELEGATION-01** — Delegation deprecated via `compiletime.error` bodies (deprecate-over-restore variant).
- **WORKFLOW-01..05**, **QUALITY-01** — atomic commits, no `@nowarn`/`-Wconf`, no `.planning/` in tree, no GSD nomenclature, Conventional Commits format.

## Pattern Established

Slices 5.1 (Bidirectional) and 5.2 (Delegation) are now a coherent family: `@deprecated` object + `inline` member with `scala.compiletime.error(...)` body. Any future leaf where the Scala 2 macro is uneconomic to port AND stdlib has no clean replacement should use this shape. Compile-time failure with a migration message is the contract.

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala` — FOUND
- `core/src/test/scala/com/avsystem/commons/misc/DelegationTest.scala` — FOUND
- `MIGRATION.md` — FOUND
- Commit `7fad5b5f` — FOUND
- Commit `6b12d4f6` — FOUND
- Branch `05-02-delegation-deprecate` on `origin` — FOUND
