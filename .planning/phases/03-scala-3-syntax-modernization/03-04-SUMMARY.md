---
phase: 03-scala-3-syntax-modernization
plan: 04
subsystem: core
tags: [scala-3, syntax, inline, perf, slice-3.4]
status: complete
nyquist_compliant: true
requires: [SYNTAX-34-AT-INLINE-TO-INLINE]
provides:
  - "Opt family @inline def -> inline def (117 sites) + Function-typed/by-name params marked inline (59 params)"
  - "jiop adapters inline sweep: JFunctionUtils 43/43, Java8CollectionUtils 9/8, GuavaInterop 3/3, ScalaJStream/IntStream/LongStream/DoubleStream 23/23 + 16/16 x3"
  - "MIGRATION.md sec.3 entry: inline def is implicitly final; inline-param sweep documented; Future-override exception in GuavaInterop documented"
  - "AVSystem/scala-commons PR #871 (draft, [Scala 3] @inline def -> inline def, milestone 1, base scala-3)"
affects: []
tech-stack:
  added: ["scala.annotation.publicInBinary"]
  patterns:
    - "inline def (forwarders)"
    - "(inline f: A => B) - Function-typed/by-name parameter inlining"
    - "@publicInBinary on private ctor + private member referenced from inline body"
key-files:
  created: []
  modified:
    - core/src/main/scala/com/avsystem/commons/misc/Opt.scala
    - core/src/main/scala/com/avsystem/commons/misc/NOpt.scala
    - core/src/main/scala/com/avsystem/commons/misc/OptArg.scala
    - core/src/main/scala/com/avsystem/commons/misc/OptRef.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/JFunctionUtils.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/Java8CollectionUtils.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/GuavaInterop.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJStream.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJIntStream.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJLongStream.scala
    - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJDoubleStream.scala
    - MIGRATION.md
decisions:
  - "Forwarders converted to inline def; trivial accessors (isEmpty/isDefined/nonEmpty/get) kept as plain def for pattern-match desugaring compatibility (-Xmax-inlines bailout on case Opt(x) =>)."
  - "@publicInBinary added on private constructors + referenced private members (Scala 3 compiler requirement)."
  - "OptArg.argToOptArg preserved as implicit def per slice 3.3 erasure-bridge rationale (untouched)."
  - "Function-typed (A => B, (A, B) => C, () => T, PartialFunction[A, B]) and by-name (=> T) parameters of every inline def in Opt family + jiop adapters marked inline. Eliminates Function* allocation at call sites."
  - "Broader jiop sweep applied on the current class XOps/AnyVal form (not extension blocks) per user directive 2026-06-01; the inline keywords carry into post-PR#868-rebase extension blocks since method signatures are preserved by slice 3.1."
  - "GuavaInterop.ListenableFutureAsScala.{onComplete, transform, transformWith} kept as plain def - they override scala.concurrent.Future methods (non-inline params), and Scala 3 forbids overriding a non-inline param with an inline param."
  - "Whitelist preserved verbatim per fork: CborInput.bits (1), JsonStringInput (5), RPCFramework (2)."
  - "SharedExtensions (49 inline def + 43 inline params per fork) deferred - current 814-line file has 0 inline def, fork shape lives inside extension blocks that don't exist on this branch yet (post-PR#868 follow-up)."
  - "JStreamUtils / TaskExtensions / JsInterop / JBasicUtils - no inline sweep needed; fork has 0 inline def in JStreamUtils / JsInterop, TaskExtensions/JBasicUtils don't exist on this branch."
metrics:
  duration: "~30 min"
  completed: "2026-06-01"
  tasks_completed: 3
  commits: 9
---

# Phase 03 Plan 04: @inline def -> inline def Summary

One-liner: Sweep Scala 2 @inline def JVM-optimizer hints to Scala 3 mandatory inline def AST splice across the Opt family (117 sites + 59 inline-params) and jiop adapters (JFunctionUtils 43, Java8CollectionUtils 9, GuavaInterop 3, ScalaJ{Stream,Int,Long,Double}Stream 23+16x3), eliminating Function* allocations at every call site.

## Outcome

- Branch: 03-04-at-inline-to-inline (continued from prior commits cd04a196 + 45c7d20a); broader sweep added 8 new commits.
- AVSystem PR: #871 (draft, [Scala 3] @inline def -> inline def, milestone Scala 3 (#1), base scala-3).
- Commits (9 total on branch):
  - cd04a196 perf(scala-3,core): inline Opt family @inline def -> inline def (prior)
  - 45c7d20a docs(migration): record @inline def -> inline def (final-implied; value-class scope mitigates) (prior)
  - c0e033a7 perf(scala-3,core): inline Function-typed params on Opt family extension ops
  - 423b29f5 perf(scala-3,core): inline jXxx adapters in JFunctionUtils
  - 153b6d1f perf(scala-3,core): inline Function-taking ops in Java8CollectionUtils + GuavaInterop
  - ce2599bc perf(scala-3,core): inline Function-taking ops in ScalaJ{Stream,Int,Long,Double}Stream
  - 7b9abbd7 fix(scala-3,core): revert inline on Future overrides in GuavaInterop
  - 875fd360 perf(scala-3,core): widen inline sweep to all Function-typed adapters in jiop
  - ee48ae6e style(scala-3,core): scalafmt after inline-keyword sweep
  - 0c6f6547 docs(migration): record inline-param sweep on Opt family + jiop adapters

## Per-file fork-shape verification

| File                                                                           | Ours inline_def/param | Fork inline_def/param | Drift |
| ------------------------------------------------------------------------------ | --------------------: | --------------------: | ----- |
| core/src/main/scala/com/avsystem/commons/misc/Opt.scala                        |                 30/16 |                 31/18 | -1/-2 (LazyOptOps when differs; minor) |
| core/src/main/scala/com/avsystem/commons/misc/NOpt.scala                       |                 28/16 |                 28/16 | 0 |
| core/src/main/scala/com/avsystem/commons/misc/OptArg.scala                     |                  18/9 |                  18/9 | 0 |
| core/src/main/scala/com/avsystem/commons/misc/OptRef.scala                     |                 25/16 |                 25/16 | 0 |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/JFunctionUtils.scala         |                 43/43 |                 43/43 | 0 |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/Java8CollectionUtils.scala   |                   9/8 |                   9/8 | 0 |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/GuavaInterop.scala           |                   3/3 |                   3/3 | 0 |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJStream.scala           |                 23/23 |                 23/23 | 0 |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJIntStream.scala        |                 16/16 |                 16/14 | +2 (over-inlined existing inline-def params; harmless) |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJLongStream.scala       |                 16/16 |                 16/14 | +2 (same) |
| core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJDoubleStream.scala     |                 16/16 |                 16/14 | +2 (same) |

## Whitelist verification (unchanged on this branch)

| File                                       | Our @inline count |
| ------------------------------------------ | ----------------: |
| serialization/cbor/CborInput.scala         |                 1 |
| serialization/json/JsonStringInput.scala   |                 5 |
| rpc/RPCFramework.scala                     |                 2 |

## Acceptance gates (all green)

- git grep '@inline' outside whitelist on core/src/main/scala and core/jvm/src/main/scala (jiop only) -> 0 hits
- sbt compile ;Test/compile ;scalafmtCheckAll -> exit 0
- git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)' -> empty (no new suppressions)
- git log upstream/scala-3..HEAD --name-only | grep '^\.planning/' -> empty
- git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-' -> empty

## Deviations from Plan

### Auto-fixed Issues

1. [Rule 1 - Bug] Cannot override non-inline parameter with an inline parameter in GuavaInterop
   - Found during: broader jiop sweep (Task 2).
   - Issue: Three methods inside private case class ListenableFutureAsScala (onComplete, transform, transformWith) override scala.concurrent.Future methods. Marking their function-typed params inline caused Scala 3 compile errors (Future's parent signatures have non-inline params).
   - Fix: Reverted those three method signatures to plain def name(f: ...); only the standalone gXxx adapters keep inline def(inline ...). Codified the rule in the Python sweep script (NEVER_INLINE_NAMES) for future iterations.
   - Files modified: GuavaInterop.scala. Commit: 7b9abbd7.

2. [Rule 1 - Bug] Initial paren-depth regex bug missed multi-arg function types
   - Found during: parity check vs fork after first jiop sweep.
   - Issue: Initial regex \([^)]*\) truncated the param list at the first ), so methods like def jBiConsumer[T, U](code: (T, U) => Any) matched only (code: (T, U) and were not inlined. Resulted in 30/30 instead of 43/43 on JFunctionUtils.
   - Fix: Replaced with depth-aware paramlist extractor (extract_paramlist) that tracks (/) nesting. Re-ran sweep; now matches fork shape exactly.
   - Files modified: JFunctionUtils.scala, Java8CollectionUtils.scala, ScalaJ*Stream.scala. Commit: 875fd360.

3. [Rule 1 - Style] Scalafmt rewraps after inline-keyword insertion
   - Found during: post-sweep scalafmtCheckAll.
   - Issue: Three files (JFunctionUtils.scala, ScalaJLongStream.scala, ScalaJDoubleStream.scala) had lines that became too long after inline insertion; scalafmt wrapped them across multiple lines.
   - Fix: Ran sbt scalafmtAll. Commit: ee48ae6e.

### Scope deviations vs plan

SharedExtensions deferred (matches prior decision):
- Fork has 49 inline def + 43 inline params in SharedExtensions.scala; our branch has 0 of either.
- Fork's shape lives inside extension blocks; our branch is still pre-PR#868 (class XOps/AnyVal form, 814 lines, 123 def-s spanning varied non-uniform patterns).
- A blanket sweep would either drift heavily from fork or require manual per-method review. Per plan caveat: deferred to post-PR#868-rebase follow-up commit on this branch.

-Xmax-inlines bailout reverts preserved from cd04a196:
- The prior commit reverted isEmpty, isDefined, nonEmpty, get to plain def per Scala 3 pattern-match desugaring edge case. These remain plain def after the broader sweep; no re-inlining attempted.

## Corrective sweeps (user directive 2026-06-01, post initial-execution)

Three corrections applied on top of the initial slice 3.4 work to tighten
inline scope, strip redundant by-name annotations, and clean up style.

### Correction 1: inline scope tightened to Function-param methods

`inline def` retained only where at least one parameter is Function-typed
(`A => B`, `(A, B) => C`, `PartialFunction[A, B]`). Methods with only
value/implicit/by-name params reverted to plain `def`.

| File                     | Inline-def reverts |
| ------------------------ | -----------------: |
| Opt.scala                | 18 (incl. unless)  |
| NOpt.scala               | 16                 |
| OptArg.scala             | 13                 |
| OptRef.scala             | 13                 |
| JFunctionUtils.scala     | 5 (Suppliers)      |
| GuavaInterop.scala       | 1 (gSupplier)      |
| ScalaJStream.scala       | 1 (onClose)        |
| ScalaJIntStream.scala    | 1 (onClose)        |
| ScalaJLongStream.scala   | 1 (onClose)        |
| ScalaJDoubleStream.scala | 1 (onClose)        |
| **Total reverts**        | **70**             |

### Correction 2: redundant `inline` stripped from by-name params

`inline X: => T` is redundant — `=> T` already provides call-site
substitution in Scala 3. Stripped from these by-name params (method
itself stays `inline def` because it has another Function-typed param):

| File                     | by-name strips |
| ------------------------ | -------------: |
| Opt.scala                | 2 (fold/mapOr ifEmpty) |
| NOpt.scala               | 2 |
| OptArg.scala             | 2 |
| OptRef.scala             | 2 |
| ScalaJStream.scala       | 1 (collect supplier) |
| ScalaJIntStream.scala    | 1 |
| ScalaJLongStream.scala   | 1 |
| ScalaJDoubleStream.scala | 1 |
| **Total strips**         | **12** |

### Correction 3: style — drop `; ()` from Consumer SAM lambdas

Scala 3 SAM conversion of an Any-returning lambda to a void-returning
Java functional interface accepts the result directly; the trailing
`; ()` coercion is unnecessary.

| File                  | `; ()` removed |
| --------------------- | -------------: |
| JFunctionUtils.scala  | 8 Consumer lambdas |

### Fork-shape comparison (post-corrections)

| File                     | Ours inline_def / inline_params | Fork inline_def / inline_params | Drift |
| ------------------------ | ------------------------------: | ------------------------------: | ----- |
| Opt.scala                | 11 / 10 | 31 / 18 | -20/-8 (intentional: trivial accessors as plain def per user directive) |
| NOpt.scala               | 11 / 10 | 28 / 16 | -17/-6 (same) |
| OptArg.scala             | 5  / 4  | 18 / 9  | -13/-5 (same) |
| OptRef.scala             | 11 / 10 | 25 / 16 | -14/-6 (same) |
| JFunctionUtils.scala     | 38 / 38 | 43 / 43 | -5/-5  (Suppliers reverted to plain def) |
| Java8CollectionUtils.scala | 9 / 8 | 9  / 8  | 0 |
| GuavaInterop.scala       | 2  / 2  | 3  / 3  | -1/-1  (gSupplier reverted) |
| ScalaJStream.scala       | 22 / 22 | 23 / 23 | -1/-1  (onClose reverted) |
| ScalaJIntStream.scala    | 15 / 15 | 16 / 14 | -1/+1 |
| ScalaJLongStream.scala   | 15 / 15 | 16 / 14 | -1/+1 |
| ScalaJDoubleStream.scala | 15 / 15 | 16 / 14 | -1/+1 |

Drift is intentional per user directive 2026-06-01. Our shape is now
strictly the subset where `inline def` is justified by a Function-typed
parameter; fork over-inlines per its own perf cadence.

### Corrections commits (4 added on top)

- `0b564a1f` refactor(scala-3,core): tighten inline scope to Function-param methods (Opt family)
- `b1b92e5d` refactor(scala-3,core): tighten inline scope on jiop adapters
- `a2e1665d` refactor(scala-3,core): tighten inline scope + drop trailing ;() in JFunctionUtils
- `701a3ad6` docs(migration): record inline-scope tightening + redundant-inline-on-by-name strip

### Correction 4: by-name params count as lambdas — restore inline def (post directive)

User directive 2026-06-01 (revision): by-name parameters (`=> T`) are
semantically equivalent to `() => T` for inlining purposes, so methods
that take at least one by-name param qualify for `inline def`. The
prior Correction 1 had been too restrictive (Function-typed only),
over-reverting ~17 methods that took by-name params.

Restored `inline def` for (by-name param present):

| File                     | Methods restored                                    | Count |
| ------------------------ | --------------------------------------------------- | ----: |
| Opt.scala                | getOrElse, orElse, toRight, toLeft, forEmpty        |     5 |
| NOpt.scala               | getOrElse, orElse, toRight, toLeft, forEmpty        |     5 |
| OptArg.scala             | getOrElse, toRight, toLeft, forEmpty                |     4 |
| OptRef.scala             | getOrElse, orElse, toRight, toLeft, forEmpty        |     5 |
| GuavaInterop.scala       | gSupplier                                           |     1 |
| ScalaJStream.scala       | onClose                                             |     1 |
| ScalaJIntStream.scala    | onClose                                             |     1 |
| ScalaJLongStream.scala   | onClose                                             |     1 |
| ScalaJDoubleStream.scala | onClose                                             |     1 |
| JFunctionUtils.scala     | jBooleanSupplier, jDoubleSupplier, jIntSupplier, jLongSupplier, jSupplier | 5 |
| **Total restored**       |                                                     | **29** |

Kept plain `def` (no by-name AND no Function-typed param):

- `Opt.LazyOptOps.unless(cond: Boolean)` — value param only
- `boxed/boxedOrNull/unboxed/toOption/toOpt/toNOpt/toOptRef/toOptArg/orNull/flatten/contains/iterator/toList/zip` across Opt family — value/implicit-only

By-name params themselves remain unmarked (Correction 2 still valid:
`=> T` already provides call-site substitution; `inline (x: => T)` is
redundant).

### Correction 4 commits (2 added on top)

- `a24edcd1` refactor(scala-3,core): restore inline def for by-name-param Opt family methods
- `3ce1ea74` refactor(scala-3,core): restore inline def for by-name-param jiop adapters

### Post-correction-4 gates (all green)

- `sbt compile ;Test/compile ;scalafmtCheckAll` → exit 0 (no new warnings, no -Xmax-inlines bailout)
- `git grep -nE '\binline\s+\w+\s*:\s*=>' core/jvm/src/main/scala/com/avsystem/commons/jiop/ core/src/main/scala/com/avsystem/commons/misc/` → 0 hits
- No new `@nowarn` / `-Wconf` introduced

### Fork-shape comparison (post Correction 4)

| File                     | Ours inline_def | Fork inline_def | Drift |
| ------------------------ | --------------: | --------------: | ----- |
| Opt.scala                | 16              | 31              | -15 (still narrower than fork; trivial accessors / no-param / value-param methods kept as plain def) |
| NOpt.scala               | 16              | 28              | -12 |
| OptArg.scala             |  9              | 18              | -9  |
| OptRef.scala             | 16              | 25              | -9  |
| JFunctionUtils.scala     | 43              | 43              |  0  |
| GuavaInterop.scala       |  3              |  3              |  0  |
| ScalaJStream.scala       | 23              | 23              |  0  |
| ScalaJIntStream.scala    | 16              | 16              |  0  |
| ScalaJLongStream.scala   | 16              | 16              |  0  |
| ScalaJDoubleStream.scala | 16              | 16              |  0  |

jiop adapters now match fork shape exactly. Opt family remains narrower
than fork (intentional: fork over-inlines pure accessors which trigger
-Xmax-inlines bailout in pattern-match desugar; we keep them plain
`def`).

### Post-corrections gates (all green)

- `sbt compile ;Test/compile ;scalafmtCheckAll` → exit 0
- `git grep '@inline' core/src/main/scala/ core/jvm/src/main/scala/` outside whitelist → 0 hits
- No new `@nowarn` / `-Wconf` introduced
- `git grep -nE '\binline\s+\w+\s*:\s*=>' core/jvm/src/main/scala/com/avsystem/commons/jiop/ core/src/main/scala/com/avsystem/commons/misc/` → 0 hits (all by-name params stripped of `inline`)

## Correction 5: reshape by-name → inline value on Opt-family `inline def` (post directive 2026-06-01)

User directive 2026-06-01 (third revision): for `inline def` methods that use
a by-name parameter EXACTLY ONCE in a straight-line `if/then/else` body
(no closure capture, no multi-evaluation, no default value), reshape
`(x: => B)` → `(inline x: B)`. Rationale: `inline def` + `inline B` substitutes
the expression literally at the call site — no `Function0` thunk allocation,
no by-name dispatch overhead. By-name `=> B` boxes a thunk per call. Strictly
cheaper for inline-def + single-use straight-line bodies.

Reshaped (4 files, 19 sites):

| File         | Methods reshaped                                                     |
| ------------ | -------------------------------------------------------------------- |
| Opt.scala    | getOrElse, fold (ifEmpty), mapOr (ifEmpty), orElse, toRight, toLeft, forEmpty |
| NOpt.scala   | getOrElse, fold (ifEmpty), mapOr (ifEmpty), orElse, toRight, toLeft, forEmpty |
| OptArg.scala | getOrElse, fold (ifEmpty), mapOr (ifEmpty), toRight, toLeft, forEmpty (no orElse) |
| OptRef.scala | getOrElse, fold (ifEmpty), mapOr (ifEmpty), orElse, toRight, toLeft, forEmpty |

Kept by-name (deferred-evaluation / closure-capture intent):

- `jiop` Suppliers (`gSupplier`, `jBooleanSupplier`, `jDoubleSupplier`,
  `jIntSupplier`, `jLongSupplier`, `jSupplier`) — wrap `expr` inside `() => expr`
- `ScalaJ*Stream.onClose(closeHandler: => Any)` — wraps into Java Runnable
- `ScalaJ*Stream.collect(supplier: => R)` — wraps into Java Supplier

### Task 2 (repo-wide sweep) — outcome: no new commits

Walked the candidate set (TaskExtensions, SharedExtensions, mongo DSLs,
hocon). All meaningful candidates skipped per the directive's "Skip" rules:

- `core/.../concurrent/TaskExtensions.scala` — pre-PR#868 `class TaskOps extends AnyVal`
  form. Per directive: "Methods on `class XOps extends AnyVal` if PR #868
  hasn't merged yet... handle then." DEFER to post-rebase follow-up.
- `core/.../SharedExtensions.scala` — same (already deferred in initial work).
- `mongo/.../typed/*.scala` — DSL methods take `Creator[T] => Filter[T]` closures
  but live on `trait QueryOperatorsDsl[T, R]` / `trait UpdateOperatorsDsl` etc.
  with abstract `def format`. Marking concrete trait defs `inline` is binary-
  compat-risky (inline def is implicitly `final`; affects downstream extenders).
  Narrower scope per user directive — skip on this PR, defer to mongo-specific
  follow-up.
- `hocon/.../*.scala` — `ConfigCompanion` delegation methods are non-Function-taking
  forwarders; no inline opportunity meeting the criteria.

Net: Task 2 yielded 0 new commits. Task 1 reshape is the sole change in this
correction batch.

### Correction 5 commit

- `4f60b812` refactor(scala-3,core): reshape by-name → inline value for inline def Opt-family forwarders

### Post-Correction-5 gates (all green)

- `sbt compile ;Test/compile ;scalafmtCheckAll` → exit 0
- No new `@nowarn` / `-Wconf` introduced
- 19 sites reshaped across 4 Opt-family files
- Per-file `inline def` counts unchanged (only parameter shape changed)

## Self-Check: PASSED

- All 11 modified .scala files FOUND
- MIGRATION.md FOUND
- Commits c0e033a7, 423b29f5, 153b6d1f, ce2599bc, 7b9abbd7, 875fd360, ee48ae6e, 0c6f6547 all FOUND in git log upstream/scala-3..HEAD
- Corrective commits 0b564a1f, b1b92e5d, a2e1665d, 701a3ad6 FOUND
- Correction-4 commits a24edcd1, 3ce1ea74 FOUND
- Correction-5 commit 4f60b812 FOUND
- PR #871 at AVSystem/scala-commons already CREATED in prior work; branch updated with corrective sweep via push

## Polish sweep: SharedExtensions / TaskExtensions / ObservableExtensions (2026-06-01)

User directive 2026-06-01: broaden inline sweep to `class XOps extends AnyVal`
wrappers. Previously deferred SharedExtensions and the concurrent extension
files (TaskExtensions, ObservableExtensions) — the `inline` keywords are
applied on the pre-PR#868 form (`class XOps extends AnyVal`) and will carry
into post-rebase `extension` blocks since slice 3.1 preserves method
signatures.

### Files swept

| File                                              | Methods inlined | Inline params | Notes |
| ------------------------------------------------- | --------------: | ------------: | ----- |
| core/.../SharedExtensions.scala                   |              49 |            42 | Matches fork shape (49 def / 43 param; ~1 param drift from multi-arg join) |
| core/.../concurrent/TaskExtensions.scala          |               7 |             7 | TaskOps: lazyTimeout, tapL, tapErrorL; TaskCompanionOps: traverseOpt, traverseMap, traverseMapValues, usingNow |
| core/.../concurrent/ObservableExtensions.scala    |               4 |             4 | findOptL, distinctBy, sortedByL, mkMapL |

### Per-Ops-wrapper inventory in SharedExtensions

| Ops wrapper           | inline def | Methods |
| --------------------- | ---------: | ------- |
| UniversalOps          | 7          | \|>, applyIf, discard, thenReturn, setup, matchOpt, uncheckedMatch |
| LazyUniversalOps      | 6          | evalFuture, evalTry, optIf, optionIf, recoverFrom, recoverToOpt |
| IntOps                | 1          | times |
| FutureOps             | 13         | onCompleteNow, andThenNow, foreachNow, transformNow (2), transformWithNow, mapNow, flatMapNow, filterNow, collectNow, recoverNow, recoverWithNow, zipWithNow |
| OptionOps             | 2          | forEmpty, mapOr |
| TryOps                | 1          | tapFailure |
| PartialFunctionOps    | 2          | unless, fold |
| IterableOnceOps       | 16         | toMapBy, mkMap, groupToMap, findOpt, flatCollect, collectFirstOpt, reduceOpt, reduceLeftOpt, reduceRightOpt, maxOptBy, minOptBy, indexWhereOpt, asyncFoldLeft, asyncFoldRight, asyncForeach, partitionEither |
| FutureCompanionOps    | 1          | eval |
| **Total**             | **49**     | matches fork |

### Plain-`def` carve-outs (kept non-inline)

- `UniversalOps`: `option`, `opt`, `unboxedOpt`, `checkNotNull`, `showAst`+10 macro stubs, `debugMacro` — non-Function, non-by-name forwarders
- `NullableOps.optRef`, `StringOps.*` — value forwarders only
- `LazyFutureOps.catchFailures`, `LazyTryOps.catchFailures` — control-flow with try/catch
- `FutureOps`: `wrapToTry`, `toUnit`, `toVoid`, `thenReturn`, `ignoreFailures` — non-Function-typed bodies
- `FutureCompanionOps`: `traverseCompleted`, `sequenceCompleted` — body uses BuildFrom builders, not pure inline candidates
- `OptionOps`: toOpt/toOptRef/toNOpt/toOptArg — pure type conversions
- `TryOps`: toOpt/toOptRef/toNOpt/toOptArg — same
- `TryCompanionOps.sequence`/`traverse` — non-Function body uses BuildFrom
- `IterableOnceOps`: `toSized`, `maxOpt`, `minOpt`, `indexOfOpt`, `mkStringOr` (2), `mkStringOrEmpty` — no Function-typed params
- `PairIterableOnceOps.intoMap`, `SetOps.*`, `IterableOps.headOpt`/`lastOpt`, `MapOps.getOpt`/`entries` — no Function params
- `IteratorOps.*` (pairs, nextOpt, drainTo, collectWhileDefined) — body is iterator state machine, not inline candidate
- `IteratorCompanionOps.untilEmpty`, `iterateUntilEmpty` — iterator state machine
- `OrderingOps.*` — @deprecated stdlib replacements

### Companion-private symbols unprivatized for inline reach

- `PartialFunctionOps.NoValueMarker` / `NoValueMarkerFunc` — dropped `private` (matches fork). `inline def fold` body pattern-matches `NoValueMarker`.
- `IterableOnceOps.private def it` — removed entirely; bodies now call `coll.iterator` directly (matches fork; avoids `@publicInBinary` on a forwarder).

### Files NOT swept (objective directive)

- `mongo/.../sync/MongoOps.scala` — no Function-typed/by-name params on DBOps or FindIterableOps
- `mongo/.../reactive/ReactiveMongoExtensions.scala` — no Function-typed/by-name params on PublisherOps
- `hocon/...` — `ConfigCompanion` has no `class XOps extends AnyVal` form
- `core/.../jiop/JCollectionUtils.scala` — `pairIterableOps` has no Function-typed methods

### Polish commits

- `4d015ba3` perf(scala-3,core): inline closure-taking methods in SharedExtensions XOps wrappers
- `15d0c4c3` perf(scala-3,core): inline closure-taking methods in TaskExtensions / ObservableExtensions

### Polish gates (all green)

- `sbt commons-core/compile` → exit 0, no new warnings
- `sbt commons-core/Test/compile` → exit 0
- `sbt scalafmtCheckAll` → exit 0
- `sbt compile;Test/compile` (full repo) → exit 0
- No new `@nowarn` / `-Wconf`
- No `-Xmax-inlines` bailouts

## History Cleanup (2026-06-01)

User directive: light cleanup — fold revert-restore cycles and noise into logical parents.

**Before:** 18 commits (initial sweep + corrections + restorations + reshapes + 4 doc accumulators).

**After:** 3 commits. Tree state preserved (verified via `diff /tmp/before-03-04.diff /tmp/after-03-04.diff` → empty). `sbt compile + Test/compile + scalafmtCheckAll` exit 0 post-rewrite.

### New commit history (PR #871)

| # | Hash       | Title                                                                              |
| - | ---------- | ---------------------------------------------------------------------------------- |
| 1 | `b71fe2f6` | perf(scala-3,core): @inline def → inline def in Opt family with inline Function/by-name params |
| 2 | `1f5a8dea` | perf(scala-3,core): inline Function-taking ops in jiop adapters                    |
| 3 | `deb43bf8` | docs(migration): record @inline def → inline def + Function-param sweep            |

Future-override rationale (former commit `7b9abbd7`) preserved in commit 2 body. Backup tag: `backup-before-cleanup/03-04-at-inline-to-inline-1780343404`. Force-pushed with `--force-with-lease`.
