---
phase: 05-leaf-feature-restoration
plan: 07
slug: value-enum
status: complete
subsystem: core/misc
tags: [scala-3, value-enum, macro-port, enclosing-symbol-walk, pattern-5]
requires: [04-05-meta-annotations]
provides: [VALUEENUM-01]
affects:
  - core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala
  - core/src/main/scala/com/avsystem/commons/misc/ValueEnumCompanionCompat.scala
  - core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala
  - MIGRATION.md
tech-stack:
  added:
    - scala.quoted (Expr/Quotes/Type — local import; CommonAliases lacks fork's export)
  patterns:
    - "Pattern 5 — enclosing-symbol walk via Symbol.spliceOwner.owner + omitAnonClass"
    - "Top-level macro def (NOT in MiscMacros) — splice owner must be the companion val site"
    - "synchronized + awaitingRegister flag dance + lazy val values (Pitfall 8 init-order)"
    - "extract single compat trait — avoid dragging unported feature compat surface"
key-files:
  created:
    - core/src/main/scala/com/avsystem/commons/misc/ValueEnumCompanionCompat.scala
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala
    - core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala
    - MIGRATION.md
decisions:
  - "Extract ValueEnumCompanionCompat to its own file (not in compat.scala). Fork bundles many compat traits in one file but most reference unported features (Boxing/Opt/Timestamp/TypeString/JavaClassName/NamedEnumCompanion/OrderedEnum); extracting just the ValueEnum-relevant trait avoids cascading compile errors."
  - "Phase 5 sign-off DEFERRED — full suite has 39 test failures from sibling slices not present on this branch. Slice 5.7 is one of the parallel branches; sign-off requires all 5.0-5.7 stacked/merged onto a single branch first."
metrics:
  duration_minutes: ~10
  tasks_completed: 3
  tasks_total: 5
  commits: 3
  completed_date: "2026-06-02"
---

# Phase 5 Plan 07: ValueEnum Summary

Verbatim port of `misc/ValueEnum.scala` (173 LOC) including the top-level `valNameImpl` macro using Pattern 5 enclosing-symbol walk via `Symbol.spliceOwner.owner` + `omitAnonClass`; Ctx registration init-order machinery preserved verbatim (Pitfall 8 cleared via runtime `ValueEnumTest`).

## Objective

Restore the `ValueEnum` `final val` introspection contract on Scala 3 — companion `inline given ValName` invokes the top-level `valNameImpl` macro, which walks `Symbol.spliceOwner.owner` (with `omitAnonClass` to skip the `new SomeEnum {}` anonymous-class case) to find the enclosing `val` and check it is `public + final + non-lazy + has explicit Value type ascription`. Closes VALUEENUM-01.

## What Was Built

**Three atomic commits on branch `05-07-value-enum` (off `04-05-meta-annotations @ f04cec6f`):**

| # | Commit     | Subject                                                |
| - | ---------- | ------------------------------------------------------ |
| 1 | `162d2a73` | feat(scala-3,core): port ValueEnum (top-level valNameImpl) |
| 2 | `7b5052f7` | test(scala-3,core): un-wrap ValueEnumTest              |
| 3 | `feb8f424` | docs(migration): record ValueEnum port                 |

### Task 1 — Port `ValueEnum.scala` verbatim (`162d2a73`)

- Overwrite from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala`.
- Top-level `def valNameImpl[T <: ValueEnum: Type, ValName: Type, Owner: Type]` — NOT in `MiscMacros.scala` per fork shape (confirmed: `grep -c valNameImpl MiscMacros.scala` = 0 in fork).
- Uses `Symbol.spliceOwner.owner` (Pitfall 5: `.owner` is required because the splice owner is the synthetic `inline def`, not the enclosing companion val).
- `omitAnonClass` walk handles `new SomeValueEnum {}` (anonymous-class init `<init>` in `$anon`) by walking up two extra owners to the companion val.
- `Ctx` registration mechanism preserved verbatim (Pitfall 8 — SI-7046 init-order trap):
  - `lazy val values` synchronized block + `awaitingRegister`/`finished` flags
  - `Ctx` constructor throws if `awaitingRegister` is true (prevents double-register)
  - `register` throws if `finished` (too late) / `registered` (double-register)
- `AbstractValueEnum` constructor flipped from `(implicit ...)` to `(using ...)` per fork.
- `implicit final val ordering: Ordering[T]` → `given Ordering[T]`. `implicit final def ordered(value: T): Ordered[T]` dropped (fork removed it — `Ordered.orderingToOrdered` is implicitly available via `summon[Ordering[T]]`).
- Removed Phase-1 stub `protected[this] implicit def valName: ValName = ???`.

**Rule-3 auto-fixes:**

1. Local `import scala.quoted.{Expr, Quotes, Type}` — `CommonAliases.scala` on this branch lacks the fork's `export scala.quoted.*` line (matches slice 5.3 ApplierUnapplier + slice 5.5 AnnotationOf precedents).
2. Bundled new `ValueEnumCompanionCompat.scala` (single trait, single `@deprecated lazy val ordering = summon`). Fork keeps this in `misc/compat.scala` alongside many other compat traits (Boxing/Opt/OptRef/Timestamp/TypeString/JavaClassName/NamedEnumCompanion/OrderedEnum). Wholesale `compat.scala` import would drag in `summon[Boxing[Boolean, JBoolean]]` etc — every one of those `summon` calls would fail because the corresponding given instances live on unported feature surface. Extracting just the relevant trait is the minimum-diff move.

### Task 2 — Un-wrap `ValueEnumTest` (`7b5052f7`)

- Overwrite from `origin/master:core/src/test/scala-3/com/avsystem/commons/misc/ValueEnumTest.scala` (byte-identical).
- `test("value enum test")` green: validates `values == List(One, Two, Three, Four, Five_?)`, ordinals `0..4`, names match `final val` declaration names.
- `ignore("enum constant member validation")` stays `ignore`d per fork — `assertCompiles` / `assertDoesNotCompile` of the macro's compile-time-error contract is hard to reproduce in Scala 3's toolbox infrastructure.

**Runtime validation cleared Pitfall 8:** `Weekday.values` (here named `SomeValueEnum.values`) produces correct ordinals and names, no `IllegalStateException("Cannot collect enum values - one of the created contexts didn't register a value yet")` at app startup → confirms the synchronized + awaitingRegister + lazy val orchestration matches fork semantics.

### Task 3 — Update `MIGRATION.md` (`feb8f424`)

- New `### core — misc ValueEnum (slice 5.7)` subsection in §3 documenting the verbatim port + `valNameImpl` placement + Ctx machinery preservation + `(implicit) → (using)` flip + `Ordering` deprecation shape + new compat-trait file rationale.
- Removed stale Backlog row `ValueEnum.scala:125 ValueEnumCompanion.valName (Scala 2 macro def)` per [[feedback_migration_md_contract]].

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 — Blocking] Bundled new `ValueEnumCompanionCompat.scala`**

- **Found during:** Task 1 compile
- **Issue:** Fork's `ValueEnumCompanion[T] extends NamedEnumCompanion[T] with ValueEnumCompanionCompat[T]` references a trait that lives in `origin/master:.../misc/compat.scala`. Our tree has no `compat.scala` and importing it wholesale would cascade compile errors across ~10 unported feature compat traits.
- **Fix:** Created `misc/ValueEnumCompanionCompat.scala` as a standalone file containing only the ValueEnum-relevant trait body (`@deprecated lazy val ordering: Ordering[T] = summon`).
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/ValueEnumCompanionCompat.scala` (new).
- **Commit:** `162d2a73`

**2. [Rule 3 — Blocking] Local `import scala.quoted.{Expr, Quotes, Type}`**

- **Found during:** Task 1 compile
- **Issue:** Top-level `valNameImpl` references `Expr`/`Quotes`/`Type` directly. Fork relies on `CommonAliases.scala` `export scala.quoted.*` which is not on this branch.
- **Fix:** Added local import — matches slice 5.3 ApplierUnapplier + slice 5.5 AnnotationOf precedents.
- **Commit:** `162d2a73`

### Scope Deviations

**1. Task 4 (PR open) — superseded by orchestrator override**

The orchestrator override block in the spawn prompt explicitly states: "Do NOT open a GitHub PR. Push branch to `origin`, skip `gh pr create`." Branch pushed to `origin/05-07-value-enum @ feb8f424`; no PR opened. Matches the prior-slice cadence (every slice in this phase deferred PR opening to a batch under user supervision).

**2. Task 5 (VALIDATION.md sign-off) — DEFERRED**

Per plan: "If any test fails, surface in SUMMARY and do NOT flip flags." Full suite (`sbt -batch 'commons-core/compile ;commons-core/test ;scalafmtCheckAll'`) ran 159 tests, 120 succeeded, 39 failed, 2 canceled, 2 ignored, 3 suites aborted.

Failing test suites are all sibling-slice surface that is not present on this branch:

| Failing suite                                       | Slice that lands the dep |
| --------------------------------------------------- | ------------------------ |
| `BidirectionalTest`                                 | 5.1 (Bidirectional)      |
| `DelegationTest`                                    | 5.2 (Delegation)         |
| `ApplierUnapplierTest`                              | 5.3 (ApplierUnapplier)   |
| `AnnotationOfTest`                                  | 5.5 (AnnotationOf family) |
| `SealedEnumTest`, `NamedEnumTest`                   | 5.6 (SealedUtils)        |
| `SamTest`, `SourceInfoTest`, etc.                   | other Phase-5 surface or Phase-6 |
| `GenCodecErrorsTest`, `GenericMetadataTest`, etc.   | Phase-6 (serialization restoration) |

The plan explicitly states "Must run AFTER all slice PRs (5.0 - 5.7) are merged/stacked locally and present on this branch tip". Slices 5.0–5.6 live on parallel branches per the documented small-scoped-PR strategy ([[feedback_small_scoped_prs]] + [[feedback_parallel_migration]]). They are not stacked.

**`05-VALIDATION.md` flags NOT flipped.** `nyquist_compliant: false` and `wave_0_complete: false` stay. Sign-off checklist NOT ticked. Sign-off must happen on a future branch where all of 5.0–5.7 are stacked/merged.

## Phase-5 Slice Status Snapshot (post-5.7)

| Slice | Branch                          | Tip        | Tests                           | Status        |
| ----- | ------------------------------- | ---------- | ------------------------------- | ------------- |
| 5.0   | `05-00-miscmacros-foundation`   | `c45c95d6` | compile-only                    | PR-pending    |
| 5.1   | `05-01-bidirectional-deprecate` | `5a9ddcab` | compile-only (test wrap)        | PR-pending    |
| 5.2   | `05-02-delegation-deprecate`    | `6b12d4f6` | compile-only (test wrap)        | PR-pending    |
| 5.3   | `05-03-applier-unapplier`       | `bb98cc45` | `ApplierUnapplierTest` green    | PR-pending    |
| 5.4   | `05-04-typestring-javaclassname`| —          | (independent)                   | (assume done) |
| 5.5   | `05-05-annotation-of-family`    | `f6d27424` | `AnnotationOfTest` green        | PR-pending    |
| 5.6   | `05-06-sealed-utils`            | `08980368` | `SealedEnumTest`+`NamedEnumTest` green | PR-pending |
| **5.7** | **`05-07-value-enum`**        | **`feb8f424`** | **`ValueEnumTest` green** | **PR-pending** |

## Verification Run-Down

- `grep -q 'def valNameImpl' ValueEnum.scala` → 0 (✅ present at line 130)
- `grep -q 'Symbol.spliceOwner.owner' ValueEnum.scala` → 0 (✅ Pitfall 5 cleared)
- `grep -q 'omitAnonClass' ValueEnum.scala` → 0 (✅ anonymous-class case handled)
- `! grep -q '???' ValueEnum.scala` → 0 (✅ no Phase-1 stubs remain)
- `diff origin/master:.../ValueEnum.scala vs ours` → only scalafmt comment-style + the added `import scala.quoted.{Expr, Quotes, Type}`
- `sbt -batch 'commons-core/compile'` → exit 0 (84 unrelated `_`/`private[this]` warnings, all pre-existing)
- `sbt -batch 'scalafmtCheckAll'` → exit 0
- `sbt -batch 'commons-core/testOnly *.ValueEnumTest'` → 1 succeeded + 1 ignored, 0 failed
- `git log --oneline 04-05-meta-annotations..HEAD | wc -l` → 3 ✅
- `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` → 0 ✅
- 0 `.planning/` changes in commits ✅
- 0 GSD nomenclature in commit messages ✅
- Pushed `origin/05-07-value-enum @ feb8f424` ✅
- PR NOT opened (per orchestrator override) ✅

## Deferred Issues

None for slice 5.7 itself. Phase-5 sign-off deferred to a future "stack-all-slices" branch (see Scope Deviation 2 above).

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala`: FOUND
- `core/src/main/scala/com/avsystem/commons/misc/ValueEnumCompanionCompat.scala`: FOUND
- `core/src/test/scala/com/avsystem/commons/misc/ValueEnumTest.scala`: FOUND
- Commit `162d2a73`: FOUND
- Commit `7b5052f7`: FOUND
- Commit `feb8f424`: FOUND
