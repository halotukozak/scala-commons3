---
phase: 03
plan: 06
subsystem: core
tags: [scala-3, export, aliases, syntax-modernization]
requires: []
provides: [CommonAliases-export, MiscAliases-export, CollectionAliases-export]
affects: [downstream-bincompat-forwarders]
tech-stack:
  added: [scala-3-export-clauses]
  patterns: [type-and-companion → export]
key-files:
  modified:
    - core/src/main/scala/com/avsystem/commons/CommonAliases.scala
    - core/src/main/scala/com/avsystem/commons/misc/MiscAliases.scala
    - core/src/main/scala/com/avsystem/commons/collection/CollectionAliases.scala
    - MIGRATION.md
decisions:
  - "User directive 2026-06-01: consolidate type+def/val alias pairs into Scala 3 export clauses across all three alias traits, even where halotukozak fork (origin/master) keeps the pair shape (CollectionAliases)."
  - "MColBuilder retained as plain type alias — its RHS scm.Builder[Elem, Col[Elem]] is a higher-kinded substitution, not a pure rename, so export does not apply."
  - "Single-import export clauses use braceless syntax (export pkg.X as Y) per user follow-up. Multi-import exports keep braces (none in scope after refactor)."
metrics:
  duration: ~25min
  completed: 2026-06-01
---

# Phase 3 Plan 6: Aliases to Export Summary

Convert `type X = Y` + `final def/val X: Y.type = Y` pairs across `CommonAliases`, `MiscAliases`, `CollectionAliases` to Scala 3 `export` clauses. Drops redundant declaration counts by ~50% in the alias traits; preserves source-level API; bytecode forwarder layout changes (relevant once MiMa is activated for the Scala 3 baseline).

## Conversion Inventory

### CommonAliases (`core/src/main/scala/com/avsystem/commons/CommonAliases.scala`)

11 entries, all Case 1/3 (no rename, `type + final val` pair):

| From | To |
|---|---|
| `type Try[+T] = scala.util.Try[T]; final val Try = scala.util.Try` | `export scala.util.Try` |
| `type Success[+T] = ...; final val Success = ...` | `export scala.util.Success` |
| `type Failure[+T] = ...; final val Failure = ...` | `export scala.util.Failure` |
| `type Future[+T] = ...; final val Future = ...` | `export scala.concurrent.Future` |
| `type Promise[T] = ...; final val Promise = ...` | `export scala.concurrent.Promise` |
| `type ExecutionContext = ...; final val ExecutionContext = ...` | `export scala.concurrent.ExecutionContext` |
| `final val NonFatal = ...` (term-only) | `export scala.util.control.NonFatal` |
| `type ClassTag[T] = ...; final val ClassTag = ...` | `export scala.reflect.ClassTag` |
| `final def classTag[T: ClassTag] = scala.reflect.classTag[T]` | `export scala.reflect.classTag` |
| `type Annotation = scala.annotation.Annotation` | `export scala.annotation.Annotation` |
| `type StaticAnnotation = ...` | `export scala.annotation.StaticAnnotation` |

### MiscAliases (`core/src/main/scala/com/avsystem/commons/misc/MiscAliases.scala`)

4 entries, all Case 3:

| From | To |
|---|---|
| `type Opt[+A] = ...; final val Opt = ...` | `export com.avsystem.commons.misc.Opt` |
| `type OptArg[+A] = ...; final val OptArg = ...` | `export com.avsystem.commons.misc.OptArg` |
| `type NOpt[+A] = ...; final val NOpt = ...` | `export com.avsystem.commons.misc.NOpt` |
| `type OptRef[+A >: Null] = ...; final val OptRef = ...` | `export com.avsystem.commons.misc.OptRef` |

### CollectionAliases (`core/src/main/scala/com/avsystem/commons/collection/CollectionAliases.scala`)

~38 entries, all Case 2 (rename via `as`); braceless syntax:

| Group | Sample conversion |
|---|---|
| Iterable family (B/I/M) | `export sc.Iterable as BIterable` |
| Seq family | `export sc.Seq as BSeq` / `export sci.Seq as ISeq` / `export scm.Seq as MSeq` |
| IndexedSeq family | `export sc.IndexedSeq as BIndexedSeq` (+I/M) |
| ArraySeq | `export sci.ArraySeq as IArraySeq` / `export scm.ArraySeq as MArraySeq` |
| LinearSeq | `export sc.LinearSeq as BLinearSeq` / `export sci.LinearSeq as ILinearSeq` |
| Queue | `export sci.Queue as IQueue` / `export scm.Queue as MQueue` |
| Set family | `export sc.Set as BSet` (+I/M) |
| HashSet | `export sci.HashSet as IHashSet` (+M) |
| SortedSet | `export sc.SortedSet as BSortedSet` (+I/M) |
| TreeSet | `export sci.TreeSet as ITreeSet` (+M) |
| BitSet | `export sc.BitSet as BBitSet` (+I/M) |
| LinkedHashSet | `export scm.LinkedHashSet as MLinkedHashSet` |
| Map family | `export sc.Map as BMap` (+I/M) |
| HashMap | `export sci.HashMap as IHashMap` (+M) |
| LinkedHashMap | `export scm.LinkedHashMap as MLinkedHashMap` |
| ListMap | `export sci.ListMap as IListMap` |
| SortedMap | `export sc.SortedMap as BSortedMap` (+I) |
| TreeMap | `export sci.TreeMap as ITreeMap` / `export scm.TreeMap as MTreeMap` |
| Buffer | `export scm.Buffer as MBuffer` |
| Builder | `export scm.Builder as MBuilder` |
| ListBuffer | `export scm.ListBuffer as MListBuffer` |
| ArrayBuffer | `export scm.ArrayBuffer as MArrayBuffer` |

**Retained as plain `type`:** `MColBuilder[Elem, +Col[_]] = MBuilder[Elem, Col[Elem]]` — RHS is HK substitution, not a symbol rename. Refined to reference the just-exported `MBuilder` for consistency.

**Also modernized:** `import scala.{collection => sc}` → `import scala.collection as sc` (Scala 3 import syntax for single rename).

### Files NOT touched (no `type + final def/val` pair found)

- `core/src/main/scala/com/avsystem/commons/jiop/JBasicUtils.scala` — only `type X = Y` (no companion alias)
- `core/src/main/scala/com/avsystem/commons/jiop/JCollectionUtils.scala` — only `type X = Y`
- `core/jvm/src/main/scala/com/avsystem/commons/jiop/JFunctionUtils.scala` — only `type X = Y`

The strict `final (val|def) X: Y.type = Y` grep returned **0 candidates outside** of CollectionAliases, confirming the type+companion pairs were concentrated in the three alias traits already touched.

## Fork-shape comparison

| File | Fork (`origin/master`) shape | Our shape | Drift? |
|---|---|---|---|
| `CommonAliases` | `export Y` clauses | `export Y` clauses | none — mirror exactly |
| `MiscAliases` | `export Y` clauses | `export Y` clauses | none — mirror exactly |
| `CollectionAliases` | `type X = Y; final def X: Y.type = Y` pairs | `export Y as X` braceless clauses | **drift** — per user directive 2026-06-01 |

Documented in PR body and commit message of the CollectionAliases commit.

## Acceptance gate

```bash
$ git grep -nE 'final (val|def) [A-Za-z]+:.*\.type =' -- '*.scala'
(zero hits)

$ sbt compile Test/compile scalafmtCheckAll
[success] (all green)
```

## Commits

| Hash | Subject |
|---|---|
| `5070a6ad` | refactor(scala-3,core): CommonAliases type+val → export |
| `2a120ec7` | refactor(scala-3,core): MiscAliases type+val → export |
| `527cfd98` | refactor(scala-3,core): CollectionAliases type+def → export with rename |
| `e5e2b5fc` | refactor(scala-3,core): MColBuilder RHS uses MBuilder alias |
| `ec2770f7` | docs(migration): record type+def alias → export consolidation |

## PR

https://github.com/AVSystem/scala-commons/pull/872 — `[Scala 3] consolidate type+val aliases to export`, draft, milestone "Scala 3" (number 1), base `scala-3`.

## Deviations from Plan

**1. [Rule 3 - Blocking] Worktree `.git` symlink trick caused `git status` to surface `.git-file.bak`.**
- Created `.git/info/exclude` in worktree-local info dir and also in the worktree's metadata dir under the common `.git/worktrees/<wt>/info/exclude`. The exclude was correctly applied but `git status` still flagged the file (likely cache-related — investigation deferred). Did not commit the backup file (stage-specific-files-only policy).

**2. [Rule 1 - Bug] Original `CollectionAliases` had a tiny inconsistency on `ISet`:** `final def ISet: Set.type = sci.Set` (type ascription `Set.type` but RHS value `sci.Set`). The export form `export sci.Set as ISet` resolves consistently to `sci.Set` for both type and term. Trivial improvement; no behavior change.

**3. User-driven polish:**
- After CollectionAliases first draft, scalafmt linter rewrote one `export sc.{Iterable as BIterable}` to braceless `export sc.Iterable as BIterable`. User confirmed: apply braceless to ALL single-import exports.
- User asked about `MBuilder`/`MColBuilder` after seeing the first CollectionAliases draft: refactored `MBuilder` to `export scm.Builder as MBuilder` and updated `MColBuilder` RHS to reference the just-exported `MBuilder`.

## Self-Check: PASSED

- `core/src/main/scala/com/avsystem/commons/CommonAliases.scala` — FOUND
- `core/src/main/scala/com/avsystem/commons/misc/MiscAliases.scala` — FOUND
- `core/src/main/scala/com/avsystem/commons/collection/CollectionAliases.scala` — FOUND
- `MIGRATION.md` § core — updated entry present
- All 5 commits present on `origin/03-06-aliases-to-export`
- PR #872 created, draft, milestone 1
