---
phase: 05-leaf-feature-restoration
plan: 05
subsystem: core/misc
tags: [scala-3, annotation, opaque-type, RefiningAnnotation, leaf-restoration]
provides:
  - "core/misc/AnnotationOf.scala — 7 live leaves (AnnotationOf, OptAnnotationOf, AnnotationsOf, HasAnnotation, SelfAnnotation, SelfOptAnnotation, SelfAnnotations)"
  - "HasAnnotation opaque-type API (check / get inline quoted impls)"
requires:
  - "04-05-meta-annotations (base — Phase 4 stack tip)"
affects:
  - "downstream callers using HasAnnotation.create (none in tree; bincompat-break for external consumers)"
  - "downstream callers of *.materialize[A,T] (migrate to summon[*[A,T]])"
tech-stack:
  added: ["scala.quoted (local import)", "scala.annotation.RefiningAnnotation (type bound)"]
  patterns:
    - "file-local quoted impls — `inline given` splices straight into sibling `private def ...Impl[…](using Quotes): Expr[…]` defined in the same companion-adjacent scope"
    - "file-local `private object AnnotationOfMacros` holds shared reflection plumbing (annotsOfT / annotsOfSym / enclosingClass / expandAggregates + helpers); no central `MiscMacros.scala` bundle and no `extends *Macros` trait-shells"
    - "opaque type with bounded type param replacing final-class private ctor"
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala
    - core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala
    - MIGRATION.md
decisions:
  - "REWORK (2026-06-02): dropped slice 5.0 (`MiscMacros.scala`) foundation entirely per [[feedback_no_central_miscmacros]]. Branch rebased onto `04-05-meta-annotations` tip; AnnotationOf family ships as a standalone leaf with no external macro bundle dependency."
  - "Inlined each `materialize*Impl` as `private def` inside its companion object, splicing through `inline given`. Shared reflection plumbing factored into a file-local `private object AnnotationOfMacros` (annotsOfT / annotsOfSym / enclosingClass / expandAggregates + collectArgs / rebuildAnnot / substituteRefs)."
  - "HasAnnotation reshape preserved: final class HasAnnotation[A, T] private () → opaque type HasAnnotation[A <: RefiningAnnotation, T] = A. Removes `create[A, T]` factory (zero internal callers per pre-port grep audit). Tightened type bound is a documented source-compat + bincompat break."
  - "Local `import scala.quoted.{Expr, Quotes, Type}` in AnnotationOf.scala (Rule 3) instead of dragging a CommonAliases `export scala.quoted.*` sweep into this leaf slice."
  - "No PR opened — per [[feedback_never_auto_open_pr]]; branch operations only. Force-pushed to `origin/05-05-annotation-of-family`."
metrics:
  duration_minutes: 5
  completed: 2026-06-02T11:52Z
  commits: 3
  tasks_completed: 4
  files_modified: 3
requirements_satisfied:
  - ANNOTOF-01
---

# Phase 5 Plan 5: AnnotationOf Family Port Summary (Reworked)

Slice 5.5 — ports `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala`
from fork (7 leaves coupled in one file). **Self-contained:** each companion
declares its own `inline given` splicing into a sibling
`private def …Impl[…](using Quotes): Expr[…]` defined in the same file. Shared
reflection plumbing factored into a file-local `private object AnnotationOfMacros`.
No central `MiscMacros.scala` bundle; no `extends *Macros` trait-shells.

`HasAnnotation` undergoes API reshape from `final class HasAnnotation[A, T] private ()`
to `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A`.

## Commits (post-rework)

| Hash       | Type   | Summary                                                             |
| ---------- | ------ | ------------------------------------------------------------------- |
| `0dd2397d` | feat   | port AnnotationOf family (per-file impls, opaque HasAnnotation reshape) |
| `12e31fa8` | test   | un-wrap AnnotationOfTest                                            |
| `9ad3daa4` | docs   | record AnnotationOf family port + HasAnnotation reshape (MIGRATION) |

**Pre-rework history discarded:** the previous 5 commits (`599a30a9`/`c45c95d6`
slice 5.0 MiscMacros foundation, plus `ce555e2b`/`5fabadad`/`f6d27424` slice 5.5
on top of it) have been replaced by these 3. Branch was reset to
`04-05-meta-annotations` (`f04cec6f`) and the rework cherry-built on top.

## Rework rationale

User feedback `[[feedback_no_central_miscmacros]]`: per-file macro impls only;
no shared `MiscMacros.scala` bundle. The previous incarnation stuck a 310-LOC
`MiscMacros.scala` foundation under `AnnotationOf.scala` with six `*Macros`
trait shells. The reworked branch:

- **Deletes** `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala`
  (no longer created in this slice).
- **Deletes** `core/src/main/scala/com/avsystem/commons/annotation/TodoScala3Migration.scala`
  (no longer needed — only `MiscMacros.scala` was using it).
- **Reverts** the `SourceInfo.scala` `hereImpl` addition (no longer needed — only
  `MiscMacros.scala` was calling it). The public `implicit def here = ???` surface
  stays as-is for slice 5.6 to port properly.
- **Drops** all six `*Macros` trait shells. Each companion gets its own
  `inline given` + sibling `private def …Impl` directly in `AnnotationOf.scala`.

## Pre-port audits (all clean)

- `git grep -nE 'HasAnnotation\.create' -- '*.scala'` → 0 hits
- `git grep -nE 'HasAnnotation\b' -- '*.scala' | grep -v misc/AnnotationOf` → 0 hits
- `git grep -l 'com.avsystem.commons.meta' -- core/.../AnnotationOf*.scala` → 0 hits (no meta deps)
- `git grep -l 'MiscMacros' -- core/src/` → 0 hits post-rework (only the file itself, which is gone)

## What changed

- **AnnotationOf.scala**: 7 case-class leaves; each companion declares its own
  `inline given [...] => X[...] = ${ materializeXImpl[...] }` splicing into a
  sibling `private def materializeXImpl[...](using quotes: Quotes): Expr[X[...]]`.
  `HasAnnotation` opaque type with `transparent inline def check[A, T]` /
  `get[A, T]` companion methods (quoted impls). Phase-1 `implicit def materialize[...]: X = ???`
  stubs removed (×7). File-local `private object AnnotationOfMacros` holds
  shared reflection plumbing (`annotsOfT` / `annotsOfSym` / `enclosingClass` /
  `expandAggregates` + `collectArgs` / `rebuildAnnot` / `substituteRefs`),
  cribbed verbatim per-method from
  `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`.
- **AnnotationOfTest.scala**: `AnnotationOf.materialize[A, T]` →
  `summon[AnnotationOf[A, T]]`; `SelfAnnotations[genann[_]]` → `SelfAnnotations[genann[?]]`.
  3 cases green (aggregate with generic / self annotations / annotation with varargs).
- **MIGRATION.md**: §3 entry rewritten for slice 5.5 (no slice 5.0 entry); §4
  bincompat-break entry preserved; 7 stale Backlog rows removed. Slice 5.0
  entry (`misc MiscMacros foundation` + `TodoScala3Migration` + `SourceInfo hereImpl`)
  is **not** in this branch's diff because slice 5.0 was discarded.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Local `import scala.quoted.*`**

- **Found during:** compile
- **Issue:** Fork's quoted impls reference `Quotes`, `Type`, `Expr` directly.
  Our `CommonAliases.scala` on this branch base does not `export scala.quoted.*`
  (fork does).
- **Fix:** Added `import scala.quoted.{Expr, Quotes, Type}` at the top of
  `AnnotationOf.scala`.
- **Files modified:** `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala`
- **Commit:** `0dd2397d`

**2. [Rule 3 - Blocking] Named `quotes` parameter on quoted impl defs**

- **Found during:** compile (after initial `using Quotes` translation)
- **Issue:** Each `private def …Impl[…](using Quotes): Expr[…]` did
  `import quotes.reflect.*` referring to an unnamed implicit. Scala 3 requires
  the parameter be named when its path-dependent members are referenced.
- **Fix:** Renamed every `using Quotes` to `using quotes: Quotes` across the
  six per-companion impls + the helpers in `AnnotationOfMacros`.
- **Commit:** `0dd2397d` (same as Fix 1)

### Process deviations

- **Rework branch in place:** force-push to `origin/05-05-annotation-of-family`
  rewrites the 5 stale commits with the 3 reworked ones. No PR backed the old
  state; no `upstream` push.
- **No PR opened:** Per `[[feedback_never_auto_open_pr]]`. Branch-only operation.

## Verification gates (all green)

- `sbt -batch 'commons-core/compile ;commons-core/testOnly *.AnnotationOfTest ;scalafmtCheckAll'` exit 0
  - `AnnotationOfTest`: 3 succeeded, 0 failed, 0 ignored
  - `scalafmtCheckAll`: clean
- `git grep -l 'MiscMacros\|XMacros' -- core/src/main/scala/` → 0 hits
- `git grep -l 'com.avsystem.commons.meta' -- core/.../AnnotationOf*` → 0 hits
- 3 atomic commits (Conventional Commits)

## Self-Check: PASSED

- File `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala` FOUND (no `extends XMacros`; no `MiscMacros` refs)
- File `core/src/test/scala/com/avsystem/commons/misc/AnnotationOfTest.scala` FOUND
- File `MIGRATION.md` FOUND (slice 5.5 + bincompat sections present)
- Commit `0dd2397d` FOUND
- Commit `12e31fa8` FOUND
- Commit `9ad3daa4` FOUND
- Branch force-pushed to `origin/05-05-annotation-of-family`
- No `MiscMacros.scala` in tree
- No `gh pr` calls made
- No `upstream` pushes
