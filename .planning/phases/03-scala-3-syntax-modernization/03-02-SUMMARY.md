---
phase: 03-scala-3-syntax-modernization
plan: 02
subsystem: scala-3-syntax
tags: [scala-3, hkt-wildcards, syntax, refactor]
requires:
  - upstream/scala-3@0887d555 (post-Phase-1 baseline)
provides:
  - applied-position HKT wildcards rewritten across core + mongo
  - kind-parameter declarations preserved per Pitfall 3
affects:
  - core/src/main/scala (8 files)
  - mongo/jvm/src/main/scala (4 files)
  - MIGRATION.md §3 core + mongo
tech-stack:
  added: []
  patterns: [applied-vs-kind-decl-classification, fork-cadence-commits]
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/serialization/FieldValues.scala
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
    - core/src/main/scala/com/avsystem/commons/serialization/InputOutput.scala
    - core/src/main/scala/com/avsystem/commons/serialization/wrappers.scala
    - core/src/main/scala/com/avsystem/commons/serialization/macroCodecs.scala
    - core/src/main/scala/com/avsystem/commons/serialization/cbor/CborOutput.scala
    - core/src/main/scala/com/avsystem/commons/serialization/cbor/CborAdtMetadata.scala
    - core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringOutput.scala
    - core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala
    - core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala
    - core/src/main/scala/com/avsystem/commons/rpc/StandardRPCFramework.scala
    - core/src/main/scala/com/avsystem/commons/misc/TypeString.scala
    - core/src/main/scala/com/avsystem/commons/misc/TypedMap.scala
    - core/src/main/scala/com/avsystem/commons/di/Component.scala
    - core/src/main/scala/com/avsystem/commons/di/Components.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonInputOutput.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/FilterDocBuilder.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala
    - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoCollection.scala
    - MIGRATION.md
decisions:
  - "Per-file applied-vs-kind-decl classification — no blind sed sweep"
  - "Kind-parameter declarations (class/trait/def/case class/type param brackets) preserved as `_`"
  - "Applied positions (type arguments at use sites) rewritten to `?`"
  - "SharedExtensions, CollectionAliases, AdtMetadataCompanion, MetadataCompanion, SealedUtils, SelfInstance, RpcMetadataCompanion, rpcAnnotations, TupleDerivation, HasGenCodec had ZERO applied positions — all hits were kind-decls or scaladoc, so no edits applied"
  - "Test sources out of scope per plan files-list (`main/scala` only)"
metrics:
  duration: ~10 min
  completed: 2026-06-01
---

# Phase 3 Plan 02: HKT wildcards `_ → ?` (applied positions) Summary

Slice 3.2 of Phase 3 (Scala 3 syntax modernization) — rewrote applied-position
HKT wildcards `[_]` / `[_, _]` to `[?]` / `[?, ?]` across in-scope core + mongo
sources, preserving kind-parameter declaration positions verbatim per Scala 3
language rules (Pitfall 3).

## Commits (6, fork cadence)

| # | Hash | Subject |
|---|------|---------|
| 1 | `f5f2ce48` | `refactor(scala-3,core): F[_] → F[?] in applied positions (serialization)` |
| 2 | `87fe1659` | `refactor(scala-3,core): F[_] → F[?] in applied positions (rpc)` |
| 3 | `e8e4d2e9` | `refactor(scala-3,core): F[_] → F[?] in applied positions (misc + di)` |
| 4 | `45b83ecb` | `refactor(scala-3,mongo): F[_] → F[?] in applied positions (sweep)` |
| 5 | `e1905e59` | `docs(migration): record HKT wildcard tightening (type-level only, no source-compat)` |
| 6 | `6b1a6e1b` | `refactor(scala-3,mongo): tighten HKT wildcards with bounds in applied positions` |

## Additional sweep (commit 6, 2026-06-01)

User reported remaining sites missed in first pass (notably `Foo[E, _]`,
`Class[_ <: X]` bound-wildcard patterns, and `BsonRef[_, T]` / `DocKey[_, _]`
shapes). Searched with extended regex `\[_\s*<:|\[_\s*>:|\[_,|\[_\]|, ?_\]|, ?_ ?,`
across in-scope sources. 18 mongo files modified (50 rewrites). Core was clean
(all `[_]` remaining are kind-declarations or scaladoc).

### Additional rewrite counts

| File | Rewrites |
|------|----------|
| mongo/Filter.scala | 11 (DocKey[A, _], DocKey[_, _], DocKey[_, _ <: BsonArray]) |
| mongo/Sort.scala | 2 (DocKey[_, _]*) |
| mongo/Update.scala | 3 (DocKey[A, _], DocKey[_, _]) |
| mongo/core/ops/BsonRefFiltering.scala | 1 (BsonRef[_, T]) |
| mongo/core/ops/BsonRefIterableFiltering.scala | 1 (BsonRef[_, C[E]]) |
| mongo/core/ops/BsonRefIterableUpdating.scala | 1 (BsonRef[_, C[E]]) |
| mongo/core/ops/BsonRefKeyHandling.scala | 1 (BsonRef[_, T]) |
| mongo/core/ops/BsonRefSorting.scala | 1 (BsonRef[_, T]) |
| mongo/core/ops/BsonRefUpdating.scala | 1 (BsonRef[_, T]) |
| mongo/core/ops/KeyGetter.scala | 4 (BsonRef[_, _], DocKey[_, _]) |
| mongo/typed/FilterDocBuilder.scala | 1 (MongoRef[_, _]) |
| mongo/typed/MongoFilter.scala | 1 (MongoRef[E, _]) |
| mongo/typed/MongoIndex.scala | 4 (MongoPropertyRef[E, _]) |
| mongo/typed/MongoOrder.scala | 8 (MongoPropertyRef[E, _]) |
| mongo/typed/MongoProjection.scala | 7 (MongoRef[E, _], MongoPropertyRef[E, _], MongoToplevelRef[E, _]) |
| mongo/typed/MongoRef.scala | 1 (Set[MongoRef[E, _]]) |
| mongo/typed/MongoUpdate.scala | 1 (Vector[PropertyUpdate[E, _]]) |
| mongo/typed/ProjectionZippers.scala | 2 (Seq[MongoProjection[E, _]], Set[MongoRef[E, _]]) |

### Fork-shape comparison

Per-file `ours_undr` vs `fork_undr` counts done before changes. Fork canonical
shape (`origin/master` paths mapped from `scala-3/` → `scala/`) used as
target. After sweep, every applied-position underscore tightened to `?` to
match fork.

### Preserved type-patterns (mirror fork)

Fork master keeps `_` in `case x: T[_, _]` type-pattern positions (vs
applied-position type ascriptions). This sweep matches that convention —
the following type-patterns intentionally retain `_`:

- `MongoQueryOperator.scala:37-39` — `case f: Size[_, _]`, `ElemMatch[_, _]`, `All[_, t]`
- `MongoUpdate.scala:42,140,142,144` — `case uae: UpdateArrayElements[_, _]`, `PropertyUpdate[E, _]` patterns
- `MongoUpdateOperator.scala:25,33,36,39,42` — `case push: Push[_, ct]` etc.
- `MongoRef.scala:197,200,227,230` — nested `case FieldRef(_: MongoToplevelRef[_, _], ...)` constructor extractor patterns
- `MongoFormat.scala:317` — `case fieldRef: MongoRef.FieldRef[E, _, T]`

Rationale: fork master's canonical shape preserved `_` in these positions;
type-pattern `_` binds an anonymous type variable scoped to the case, distinct
from existential `?`.

## Per-Module Rewrite Counts

| Module | Files modified | Applied-position rewrites |
|--------|----------------|----------------------------|
| core/serialization | 8 | 14 rewrites (Array[GenCodec[?]], Array[Class[?]], Opt[cborKey[?]], Opt[cborDiscriminator[?]], List[Case[?]], Array[GenCodec.OOOFieldsObjectCodec[?]], List[Field[?]], List[CborKeyInfo[?]], Array[OOOFieldsObjectCodec[?]], InputMetadata[?], BIterable[?], Transformed[?, ?]) |
| core/rpc | 3 | 8 rewrites (Iterator[?], List[ParamMetadata[?]] ×4, Map[String, FunctionSignature[?]], Map[String, GetterSignature[?]] ×2) |
| core/misc + core/di | 4 | ~15 rewrites (GenKeyCodec[TypeString[?]], GenCodec[TypeString[?]], GenKeyCodec[JavaClassName[?]], GenCodec[JavaClassName[?]], Entry[K, ?]*, Component[?] ×7, AtomicReference[?], MHashMap[Component[?], ...] ×2, MHashSet[Component[?]], AtomicReference[Future[?]]) |
| mongo | 4 | 17 rewrites (InputMetadata[?], MongoQueryOperator[?], MongoFilter[?], List[Case[?]] ×2, Map[Class[?], Case[?]], Map[Class[?], UnionFormat[?]], MHashMap[Class[?], (SealedParent[?], MListBuffer[Case[?]])], cse: Case[?]/codec: GenObjectCodec[?], List[SealedParent[?]] ×3, Opt[Field[?]] ×3, List[Field[?]], Map[String, Field[?]], MongoCollection[?] ×4) |
| MIGRATION.md | 1 | 2 doc entries (core + mongo) |

## Per-File Applied-vs-Kind-Decl Classification (key examples)

### core/serialization
- `FieldValues.scala:15` — `Array[GenCodec[?]]` (applied) — REWRITTEN
- `GenCodec.scala:334-335` — `GenCodec[?]`, `Transformed[?, ?]` (applied) — REWRITTEN
- `HasGenCodec.scala:83/89/97/109/115/123/135/142` — `trait/abstract class X[C[_]]` (kind-decl) — PRESERVED
- `InputOutput.scala:77,171` — `InputMetadata[?]`, `BIterable[?]` (applied) — REWRITTEN
- `wrappers.scala:71` — `InputMetadata[?]` (applied) — REWRITTEN
- `macroCodecs.scala:27,133,152,155,180,188,189,389` — `Array[X[?]]` (applied) — REWRITTEN
- `cbor/CborAdtMetadata.scala:80,89,91,109,140,142,178,226` — applied positions REWRITTEN; lines 258,265 (`trait/abstract class X[C[_]]`) PRESERVED
- `cbor/CborOutput.scala:175`, `json/JsonStringOutput.scala:126` — `InputMetadata[?]` (applied) — REWRITTEN

### core/rpc
- `AsRawReal.scala:114,117` — `def materialize[M[_], Real]` (kind-decl) — PRESERVED
- `AsRawReal.scala:122` — `Iterator[?]` (applied) — REWRITTEN
- `RPCFramework.scala:61` — `List[ParamMetadata[?]]` (applied) — REWRITTEN
- `RpcMetadataCompanion.scala:15,23` — kind-decl — PRESERVED
- `StandardRPCFramework.scala:19,37,61,78,79,94` — applied — REWRITTEN
- `rpcAnnotations.scala:56` — scaladoc — UNCHANGED

### core/misc + core/di + core/collection + core/meta + core/tuples
- `SealedUtils.scala:14`, `SelfInstance.scala:5,8` — kind-decl — PRESERVED
- `TypeString.scala:34,35,37,38,83,84,86,87` — `GenKeyCodec/GenCodec[TypeString|JavaClassName[?]]` (applied) — REWRITTEN
- `TypedMap.scala:44,73,75,78,81 (K[_]),87,91` — kind-decl — PRESERVED; `81 (Entry[K, ?]*)` (applied) — REWRITTEN
- `Component.scala:11,14,49,64,69,75,109,192,206-208,221,223` — `Component[?]`, `AtomicReference[?]`, `MHashMap[Component[?], ...]`, `MHashSet[Component[?]]` (applied) — REWRITTEN
- `Components.scala:32` — `AtomicReference[Future[?]]` (applied) — REWRITTEN
- `CollectionAliases.scala:116` — `+Col[_]` (kind-decl) — PRESERVED
- `AdtMetadataCompanion.scala:14`, `MetadataCompanion.scala:15` — kind-decl — PRESERVED
- `TupleDerivation.scala:7` — kind-decl — PRESERVED
- `SharedExtensions.scala:699` — `drainTo[C[_]]` (kind-decl) — PRESERVED

### mongo
- `BsonInputOutput.scala:114` — `InputMetadata[?]` (applied) — REWRITTEN
- `FilterDocBuilder.scala:39,65` — `MongoQueryOperator[?]`, `MongoFilter[?]` (applied) — REWRITTEN
- `MongoFormat.scala:78,108,149` — `TypedMapFormat[K[_]]`, `typedMapFormat[K[_]]`, `typedMapFormatOps[K[_]]` (kind-decl) — PRESERVED
- `MongoFormat.scala:178,181,184,185,209,231,285,287,297,298,306,309,329,336` — applied positions — REWRITTEN
- `MongoPolyDataCompanion.scala:11,22,59` — kind-decl — PRESERVED
- `MongoRef.scala:269` — `TypedMapRefOps[E, K[_]]` (kind-decl) — PRESERVED
- `MongoTypedKey.scala:8` — kind-decl — PRESERVED
- `TypedMongoCollection.scala:27,387,390,396` — `MongoCollection[?]` (applied) — REWRITTEN

## Deviations from Plan

### Positive: SharedExtensions had zero applied positions

The plan called for a separate `Commit D: SharedExtensions` commit. The only `[_]` in
`SharedExtensions.scala:699` is `def drainTo[C[_]]` — a kind-parameter declaration that
must be PRESERVED per Pitfall 3. There were no applied-position rewrites needed, so no
SharedExtensions commit was created. Final commit count: 5 (not 6) — 3 core clusters
(serialization, rpc, misc+di) + 1 mongo + 1 docs.

### Positive: no hocon work needed

`git grep -nE '\[_(\s*,\s*_)*\]' -- 'hocon/src/main/scala'` returned 0 hits — no hocon
applied-position commit needed (plan noted "0-1" possibility).

### Positive: no core/jvm or core/js work needed

Same grep returned 0 hits on `core/jvm/src/main/scala` and `core/js/src/main/scala`.

No auto-fixes (Rules 1-3) applied — pure mechanical syntax sweep with no compile-cascade
side effects. No `@nowarn`/`-Wconf` introduced.

## Verification Gates (all green)

```
sbt 'compile ;Test/compile ;scalafmtCheckAll'     → exit 0 (warnings only — pre-existing)
git grep [_(\s*,\s*_)*]                            → 48 remaining, all kind-decls or scaladoc
git diff upstream/scala-3..HEAD | grep @nowarn      → 0
git log upstream/scala-3..HEAD --name-only | .planning/  → 0
git log upstream/scala-3..HEAD --pretty=%B | gsd|phase   → 0
```

## PR

**URL:** https://github.com/AVSystem/scala-commons/pull/869
**State:** draft
**Title:** `[Scala 3] tighten HKT wildcards (_ → ?)`
**Milestone:** Scala 3 (#1)
**Base:** `scala-3`
**Head:** `halotukozak:03-02-hkt-wildcards`
**Depends on:** #868 (slice 3.1)
**Commits:** 5 (fork cadence — do not squash)

## Self-Check: PASSED

- Branch `03-02-hkt-wildcards` cut off `upstream/scala-3@0887d555` (verified `git log`)
- 5 commits on branch (verified `git log upstream/scala-3..HEAD --oneline`)
- 12 source files + MIGRATION.md modified (verified `git diff --stat upstream/scala-3..HEAD`)
- PR #869 open at AVSystem/scala-commons, draft, milestone 1, base scala-3 (verified via gh)
- All acceptance gates pass
