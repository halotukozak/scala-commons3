---
phase: 05-leaf-feature-restoration
plan: 04
subsystem: core/misc
tags: [scala-3, macro, typestring, javaclassname, leaf-restoration]
requires: [04-05-meta-annotations]
provides:
  - TYPESTRING-01 (TypeString.of[T] materialized via Printer.TypeReprShortCode)
  - JAVACLASSNAME-01 (JavaClassName.of[T] == classOf[T].getName)
affects:
  - downstream callers of GenKeyCodec[TypeString[_]] / GenCodec[TypeString[_]] — now per-T given (no in-tree callers affected)
tech-stack:
  added:
    - scala.quoted.* macro DSL usage in misc/TypeString.scala
  patterns:
    - Pattern 1: inline given + companion-local Quotes splice (materializeImpl)
    - Top-level derivedImpl (for JavaClassName.derived in JavaClassNameLowPriority)
key-files:
  created:
    - core/src/main/scala/com/avsystem/commons/misc/compat.scala (TypeStringCompat + JavaClassNameCompat only)
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/TypeString.scala
    - core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala
    - MIGRATION.md
decisions:
  - "Scoped compat.scala to TypeString/JavaClassName traits only — other fork compat traits (Boxing, Opt, Timestamp, etc.) deferred to their owning leaves to keep slice 5.4 minimal."
  - "Swapped GenCodec.createSimple (2-arg) → GenCodec.nonNullSimple — current tree lacks the 2-arg overload; semantically identical for these non-null value-class codecs."
  - "Added explicit `import scala.quoted.*` — fork file does not include it (must rely on undiscovered alias); tree has no such alias."
  - "Added TypeString.of / JavaClassName.of smoke tests to SharedExtensionsTest — fork commit dcf60e5d does NOT include these in fork's SharedExtensionsTest, but VALIDATION.md requires runtime smoke for both leaves."
metrics:
  duration_seconds: 424
  tasks_completed: 3
  files_modified: 4
  commits: 3
  completed: 2026-06-02
---

# Phase 5 Plan 04: TypeString + JavaClassName Restoration Summary

Port fork-verbatim `misc/TypeString.scala` (covers both TypeString and JavaClassName in one file) — companion-local `materializeImpl` uses `Printer.TypeReprShortCode`; top-level `derivedImpl` powers `JavaClassName.derived` from the `JavaClassNameLowPriority` trait. Switches Phase-1 stub's existential `GenKeyCodec[TypeString[_]]` to per-T `given [T] => GenKeyCodec[TypeString[T]]` per Pitfall 7.

## Tasks Executed

| Task | Name | Commit | Notes |
| ---- | ---- | ------ | ----- |
| 1 | Port TypeString.scala + minimal compat.scala | `32185107` | Verbatim port + import + nonNullSimple swap |
| 2 | Un-wrap SharedExtensionsTest TypeString/JavaClassName smoke | `06fbe666` | Added 2 smoke tests (fork SharedExtensionsTest itself lacks them) |
| 3 | MIGRATION.md slice 5.4 entry | `b2d376e0` | §3 entry + removed 2 obsolete will-not-migrate rows |

## Verification

- `sbt -batch 'commons-core/compile'` → green
- `sbt -batch 'commons-core/testOnly *.SharedExtensionsTest -- -z TypeString -z JavaClassName'` → 2/2 pass
- `sbt -batch scalafmtCheckAll` → green
- No new `@nowarn` / `-Wconf` introduced vs base
- No `???` in `misc/TypeString.scala`
- Diff vs `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala` minimal: `import scala.quoted.*` addition + 2× `createSimple` → `nonNullSimple` substitutions.

## Deviations from Plan

### Rule 3 — Auto-fix blocking issues

**1. Added `import scala.quoted.*` to ported TypeString.scala**
- **Found during:** Task 1 (compile after fork-verbatim overwrite)
- **Issue:** Fork file uses `Type[T]`, `Quotes`, `Expr` but does not import `scala.quoted.*` (presumably auto-aliased in fork via another file not present in the current tree).
- **Fix:** Added explicit `import scala.quoted.*`.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`
- **Commit:** `32185107`

**2. Swapped `GenCodec.createSimple` 2-arg → `GenCodec.nonNullSimple`**
- **Found during:** Task 1 (compile)
- **Issue:** Fork uses `GenCodec.createSimple(readFun, writeFun)` but the current tree's `createSimple` requires an explicit `allowNull: Boolean` (no 2-arg overload).
- **Fix:** Used `GenCodec.nonNullSimple` — semantically identical for these non-null value-class codecs (`new TypeString(_)` / `new JavaClassName(_)` cannot be null).
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`
- **Commit:** `32185107`

**3. Created scoped `misc/compat.scala` with only TypeStringCompat + JavaClassNameCompat**
- **Found during:** Task 1 (fork code references `TypeStringCompat` and `JavaClassNameCompat` traits)
- **Issue:** Fork's `compat.scala` doesn't exist in tree; ported file won't compile without these traits.
- **Fix:** Created `compat.scala` containing only the two needed traits (per-leaf scope — other compat traits deferred to their owning leaves per minimum-scope principle).
- **Files modified:** new file
- **Commit:** `32185107`

**4. Adjusted TypeString smoke assertion**
- **Found during:** Task 2 (test run)
- **Issue:** Initial test asserted `TypeString.of[String => Int] == "String => Int"`. Actual output from `Printer.TypeReprShortCode` is `"Function1[String, Int]"` (no arrow desugaring).
- **Fix:** Updated assertion to match actual behaviour; documented in MIGRATION.md.
- **Files modified:** `SharedExtensionsTest.scala`
- **Commit:** `06fbe666`

### Out-of-scope items (deferred)

`SharedExtensionsTest` has 2 pre-existing failures unrelated to TypeString/JavaClassName: `sourceCode` and `withSourceCode` both throw `NotImplementedError` because the underlying extension methods are `???` stubs (source-position macro removed pending its own slice). Logged in `deferred-items.md`.

## Authentication Gates

None.

## Branch

`05-04-typestring-javaclassname` cut from `04-05-meta-annotations` @ `f04cec6f` (NOT from `05-03-applier-unapplier` — slice 5.4 is independent of slice 5.0/5.3 per branch_strategy in PLAN). Pushed to `origin` (halotukozak fork). No PR opened per execution overrides.

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`: FOUND, contains `inline given`, `Printer.TypeReprShortCode`, `def derivedImpl`, no `???`
- `core/src/main/scala/com/avsystem/commons/misc/compat.scala`: FOUND
- `core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala`: FOUND, contains `JavaClassName.of`
- `MIGRATION.md`: FOUND, contains `slice 5.4`
- Commits `32185107`, `06fbe666`, `b2d376e0`: FOUND in `git log`
- Branch pushed to `origin/05-04-typestring-javaclassname`: confirmed
