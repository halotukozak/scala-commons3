---
phase: 05-leaf-feature-restoration
plan: 00
subsystem: core/misc
tags: [scala-3-port, macros, sourceinfo, annotation, slice-5.0]
dependency-graph:
  requires:
    - Phase 4 final (04-05-meta-annotations @ f04cec6f)
  provides:
    - misc.SourceInfo.here (real Quotes-based macro splice)
    - misc.SourceInfo.hereImpl (private[misc])
    - annotation.TodoScala3Migration @StaticAnnotation
  affects:
    - all later slices that wish to grep-mark unported macro stubs via @TodoScala3Migration
    - any caller of `SourceInfo.here` (was `???`, now real macro)
tech-stack:
  added: []
  patterns:
    - "per-file Scala 3 macro impl colocated with its public surface (no central bundle)"
    - "inline implicit def + ${ ... } splice idiom for materialized typeclass-like givens"
    - "@TodoScala3Migration marker for grep-able tracking of unported macro bodies"
key-files:
  created:
    - core/src/main/scala/com/avsystem/commons/annotation/TodoScala3Migration.scala
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala
    - MIGRATION.md
decisions:
  - "Dropped central MiscMacros.scala bundle (310 LOC) per user feedback — macro impls live per-file with their public API, not in a shared file"
  - "SourceInfo.here wired now (not deferred to slice 5.6) — hereImpl body was already needed; wiring `here = ${ hereImpl }` is one-line"
  - "TodoScala3Migration annotation retained as standalone — already useful as marker for staged stubs in other slices"
metrics:
  duration: ~8 min (incl. rework)
  completed: 2026-06-02
---

# Phase 5 Plan 00: SourceInfo macro port + TodoScala3Migration marker Summary

Slice 5.0 (reworked): real Scala 3 macro port of `SourceInfo.here` using `Quotes`-based `hereImpl`, plus a standalone `@TodoScala3Migration` annotation marker reused by later slices. Centralised `MiscMacros.scala` bundle from the initial pass was dropped per user feedback.

## What Shipped

Two atomic commits on branch `05-00-sourceinfo-port` (renamed from `05-00-miscmacros-foundation`, off `04-05-meta-annotations @ f04cec6f`):

| Commit     | Type | Message                                                                                       | Files                                                                                                                |
| ---------- | ---- | --------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| `050e6911` | feat | `feat(scala-3,core): port SourceInfo.here macro + add TodoScala3Migration marker`             | `misc/SourceInfo.scala` (wire `here` + add `hereImpl`), `annotation/TodoScala3Migration.scala` (new)                 |
| `7221ca39` | docs | `docs(migration): record SourceInfo port + TodoScala3Migration marker`                        | `MIGRATION.md` (reframed slice 5.0 entry — replaces dropped "MiscMacros foundation" wording)                         |

Branch tip: `7221ca39` on `05-00-sourceinfo-port`. Pushed to `origin/05-00-sourceinfo-port`.

## Rework Notes (relative to original 599a30a9/c45c95d6 pass)

- **Dropped:** `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` (310 LOC verbatim port). User feedback: macro impls go per-file with their public API; no shared bundle.
- **Upgraded:** `SourceInfo.here` flipped from `implicit def here: SourceInfo = ???` to `inline implicit def here: SourceInfo = ${ hereImpl }`. The `hereImpl` body itself is unchanged from the first pass (was already present; only the public wiring changed).
- **Kept as-is:** `TodoScala3Migration.scala` (still useful as marker for stub bodies in later slices).
- **MIGRATION.md:** entry reframed — now lands "SourceInfo macro port + TodoScala3Migration annotation" instead of "MiscMacros foundation bundle." Table reduced from 3 rows to 2.

## Deviations from Plan

None — rework executed exactly as specified in the user's reframing brief.

## Verification

- `sbt commons-core/compile` → exit 0 (~5s incremental)
- `sbt scalafmtCheckAll` → exit 0 (one auto-reformat applied to TodoScala3Migration.scala, then re-verified)
- `git log --oneline 04-05-meta-annotations..HEAD | wc -l` → 2
- `git diff 04-05-meta-annotations..HEAD --stat` → 3 files: `MIGRATION.md` (+16/-19), `annotation/TodoScala3Migration.scala` (+N new), `misc/SourceInfo.scala` (+29)
- `MiscMacros.scala` no longer present in tree or branch diff
- No `gh pr` mutating calls made
- No `upstream` pushes made
- Force-push to `origin/05-00-sourceinfo-port` succeeded
- Old remote branch `origin/05-00-miscmacros-foundation` NOT auto-deleted (PR #876 still points at it — user retargets manually)

## Known Issues / Deferred

- PR #876 still points at `05-00-miscmacros-foundation` on origin and carries the old title/description. User needs to manually retarget PR #876 to `05-00-sourceinfo-port` (or close it and open a new PR) and update title/description to reflect new scope. Per [[feedback_never_auto_open_pr]] — no `gh pr edit` / `gh pr close` invoked.
- Old branch `origin/05-00-miscmacros-foundation` left in place — safe to delete only AFTER PR #876 head ref is updated.

## Self-Check: PASSED

Files claimed:
- `core/src/main/scala/com/avsystem/commons/annotation/TodoScala3Migration.scala` → FOUND
- `core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala` → FOUND (modified, `here` wired to `${ hereImpl }`)
- `MIGRATION.md` → FOUND (modified, entry reframed)
- `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` → ABSENT (correctly deleted)

Commits claimed:
- `050e6911` → FOUND
- `7221ca39` → FOUND

Branch claimed: `05-00-sourceinfo-port` → FOUND locally + on `origin`
