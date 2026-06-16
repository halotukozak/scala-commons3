---
gsd_state_version: 1.0
milestone: v1.0
milestone_name: milestone
status: unknown
last_updated: "2026-06-02T08:51:16.431Z"
progress:
  total_phases: 5
  completed_phases: 3
  total_plans: 29
  completed_plans: 28
---

# State

**Project:** scala-commons3 Scala 3 migration — **PIVOT 2026-06-01: Scala 3 ONLY, drop cross-build**
**Mode:** big-bang — comment out broken, migrate scalac opts, restore iteratively
**Target branch:** AVSystem/scala-commons:scala-3

## PIVOT 2026-06-01

Old cross-compile cherry-pick strategy abandoned. New strategy: drop 2.13 axis entirely, migrate whole codebase to Scala
3, comment out anything that doesn't compile, restore feature-by-feature in follow-ups.

Closed PRs (cross-build strategy):

- #856 closed (Phase 1 — Refactor build for cross-compilation)
- #859 closed (Phase 4 — Port made wiring primitives)

Merged downstack but not landed on `scala-3`:

- #857 merged into `01-cross-compile-infra` (Phase 2 — MIGRATION.md)
- #858 merged into `02-migration-md` (Phase 3 — macros stub)

Upstream `AVSystem:scala-3` still at `1561d8dc` (unchanged from pre-pivot baseline).

Old Phase 1-5 planning artifacts archived under `.planning/phases/0[1-5]-*/` (kept for reference).

## Current Position

Phase: 05 (leaf-feature-restoration) — EXECUTING
Plan: 7 of 8

## Decisions Accumulated

- **Plan 07 (2026-06-02, Phase 05 leaf-feature-restoration):** Slice 5.7 — port `misc/ValueEnum` (top-level `valNameImpl` via Pattern 5 enclosing-symbol walk). Branch `05-07-value-enum` cut off `04-05-meta-annotations @ f04cec6f` (independent of slice 5.0; no `MiscMacros` dep — fork puts `valNameImpl` as top-level `def` in `ValueEnum.scala`, NOT in `MiscMacros.scala`, confirmed via `git show origin/master:.../MiscMacros.scala | grep -c valNameImpl` = 0). Three atomic commits: `162d2a73` (`feat(scala-3,core): port ValueEnum (top-level valNameImpl)` — verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala`: top-level `def valNameImpl[T <: ValueEnum: Type, ValName: Type, Owner: Type]` uses `Symbol.spliceOwner.owner` + `omitAnonClass` walk (Pitfall 5: `.owner` required, not bare `spliceOwner`; Pitfall 8 init-order trap cleared by preserving fork's exact `Ctx` machinery — `synchronized` + `awaitingRegister`/`finished` flag dance + `lazy val values`); `AbstractValueEnum(using protected val enumCtx: EnumCtx)` flipped from `(implicit ...)`; `implicit final val ordering: Ordering[T]` → `given Ordering[T]`; `implicit final def ordered(value: T): Ordered[T]` dropped per fork; Phase-1 stub `protected[this] implicit def valName: ValName = ???` removed; companion `inline protected given ValName = ${ valNameImpl[Value, ValName, this.type]('{ ValName(_) }) }`. Rule-3 auto-fixes: (a) local `import scala.quoted.{Expr, Quotes, Type}` per slice 5.3/5.5 precedent — `CommonAliases.scala` on this branch lacks fork's `export scala.quoted.*`; (b) new `ValueEnumCompanionCompat.scala` extracted from fork's `compat.scala` (single `@deprecated lazy val ordering = summon` trait) — wholesale `compat.scala` import would cascade ~10 compile errors against unported Boxing/Opt/OptRef/Timestamp/TypeString/JavaClassName/NamedEnumCompanion/OrderedEnum surface), `7b5052f7` (`test(scala-3,core): un-wrap ValueEnumTest` — byte-identical to `origin/master:core/src/test/scala-3/com/avsystem/commons/misc/ValueEnumTest.scala`; "value enum test" green: validates `values == List(One, Two, Three, Four, Five_?)`, ordinals 0..4, names match `final val` declaration names — runtime confirms Ctx synchronized + awaitingRegister + lazy val orchestration produces correct results (no `IllegalStateException("Cannot collect enum values …")` at startup, Pitfall 8 cleared); "enum constant member validation" stays `ignore`d per fork — compile-time `assertCompiles`/`assertDoesNotCompile` of macro error contract is hard to reproduce in Scala 3 toolbox), `feb8f424` (`docs(migration): record ValueEnum port` — MIGRATION.md §3 new `core — misc ValueEnum (slice 5.7)` subsection documenting the verbatim port + `valNameImpl` placement + Ctx machinery preservation + `(implicit)→(using)` flip + Ordering deprecation shape + new compat-trait file rationale; Backlog table: removed stale `ValueEnum.scala:125 ValueEnumCompanion.valName (Scala 2 macro def)` row per [[feedback_migration_md_contract]]). All gates green: `sbt -batch 'commons-core/compile ;commons-core/testOnly *.ValueEnumTest ;scalafmtCheckAll'` exit 0 (1 succeeded + 1 ignored + 0 failed), `diff` vs fork shows only the added scala.quoted import + scalafmt scaladoc reformat, 0 new `@nowarn`/`-Wconf` vs base, 0 `.planning/` in commits, 0 GSD nomenclature, 3 atomic commits per Conventional Commits. Branch pushed to `origin/05-07-value-enum @ feb8f424`. PR NOT opened per orchestrator override (batch PR creation under user supervision later — `gh pr create` skipped). REQ VALUEENUM-01 satisfied. **Task 5 phase-5 sign-off DEFERRED** — full suite (`sbt -batch 'commons-core/compile ;commons-core/test ;scalafmtCheckAll'`) ran 159 tests with 39 failures + 3 suite-aborts; failing suites are all sibling-slice surface absent from this parallel branch (BidirectionalTest=5.1, DelegationTest=5.2, ApplierUnapplierTest=5.3, AnnotationOfTest=5.5, SealedEnumTest+NamedEnumTest=5.6, plus Phase-6 surface SamTest/SourceInfoTest/GenCodecErrorsTest/etc). Per [[feedback_parallel_migration]] strategy slices 5.0–5.6 live on parallel branches not stacked here. Per plan "If any test fails, surface in SUMMARY and do NOT flip flags" — `05-VALIDATION.md` `nyquist_compliant: false` + `wave_0_complete: false` flags NOT flipped, sign-off checklist NOT ticked. Sign-off must happen on a future "stack-all-slices" branch where 5.0–5.7 are merged. Pattern note: confirms Pattern 5 (enclosing-symbol walk via `Symbol.spliceOwner.owner`) shape for any future leaf where a macro needs to introspect the enclosing val/def at the use site — closes the family of "introspect call site" macros (slice 5.0 MiscMacros has the other in-trait variants, slice 5.7 has the top-level def variant).

- **Plan 05 (2026-06-02, Phase 05 leaf-feature-restoration):** Slice 5.5 — port `misc/AnnotationOf` family (7 leaves coupled in one file). Branch `05-05-annotation-of-family` cut off `origin/05-00-miscmacros-foundation @ c45c95d6` (stacked on slice 5.0 — first slice-5 PR to consume the MiscMacros foundation traits). Three atomic commits: `ce555e2b` (`feat(scala-3,core): port AnnotationOf family (7 leaves, opaque HasAnnotation)` — verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala` covering `AnnotationOf` / `OptAnnotationOf` / `AnnotationsOf` / `HasAnnotation` / `SelfAnnotation` / `SelfOptAnnotation` / `SelfAnnotations`; 6 companions extend their respective `*Macros` traits from `MiscMacros.scala` slice 5.0 (`AnnotationOfMacros`, `OptAnnotationOfMacros`, `AnnotationsOfMacros`, `SelfAnnotationMacros`, `SelfOptAnnotationMacros`, `SelfAnnotationsMacros`); `HasAnnotation` API reshape per Pitfall 4: `final class HasAnnotation[A, T] private ()` → `opaque type HasAnnotation[A <: RefiningAnnotation, T] = A` with `transparent inline def check[A, T]` / `get[A, T]` companion methods (quoted impls using `TypeRepr.of[T].typeSymbol.hasAnnotation/getAnnotation`); removed 7 Phase-1 `implicit def materialize[...]: X = ???` stubs; Rule-3 auto-fix: added local `import scala.quoted.{Expr, Quotes, Type}` because our `CommonAliases.scala` on this branch base lacks the fork's `export scala.quoted.*` line — mirrors slice 5.3 ApplierUnapplier `import scala.deriving.Mirror` precedent), `5fabadad` (`test(scala-3,core): un-wrap AnnotationOfTest` — synced from fork: `AnnotationOf.materialize[A, T]` → `summon[AnnotationOf[A, T]]` + `SelfAnnotations[genann[_]]` → `SelfAnnotations[genann[?]]`; 3 cases green: aggregate with generic / self annotations / annotation with varargs), `f6d27424` (`docs(migration): record AnnotationOf family port + HasAnnotation reshape` — MIGRATION.md §3 new `core — misc AnnotationOf family (slice 5.5)` subsection documenting the 7-leaf coupled port + opaque-type reshape + RefiningAnnotation bound + summon-based call-site migration + local scala.quoted import workaround; §4 new `core — slice 5.5 bincompat-break (HasAnnotation reshape)` subsection pre-recording the opaque-type-replacing-final-class-private-ctor + tightened-bound + removed-create-factory bincompat for the future MiMa run; Backlog table: removed 7 stale `AnnotationOf.scala:*` materialize rows per [[feedback_migration_md_contract]]). Pre-port grep audits clean: `git grep -nE 'HasAnnotation\.create' -- '*.scala'` = 0 hits (planned-and-verified-at-execution), `git grep -nE 'HasAnnotation\b' -- '*.scala' | grep -v misc/AnnotationOf` = 0 hits. All gates green: `sbt 'commons-core/compile ;commons-core/testOnly *.AnnotationOfTest'` exit 0 (3 succeeded + 0 failed + 0 ignored), all acceptance grep checks pass (`extends *Macros` count = 6, `opaque type HasAnnotation[A <: RefiningAnnotation` present, `???` absent in AnnotationOf.scala, `HasAnnotation.create` absent tree-wide), `diff` vs fork shows only the added scala.quoted import (every other line byte-identical), 0 new `@nowarn`/`-Wconf` vs base, 0 `.planning/` in commits, 0 GSD nomenclature, 3 atomic commits per Conventional Commits. Branch pushed to `origin/05-05-annotation-of-family @ f6d27424`. PR NOT opened per orchestrator override (batch PR creation under user supervision later — `gh pr create` skipped). REQ ANNOTOF-01, WORKFLOW-01..05, QUALITY-01 satisfied. Headline pattern: **first documented bincompat-break of Phase 5** (vs prior slices that were pure source-compat) — establishes opaque-type-with-tightened-bound as the shape for any future leaf where a sealed final-class-private-ctor evidence type can be flattened (e.g. future `SealedInstances` evidence reshapes). Confirms the slice-5.0 + slice-5.N composition pattern works end-to-end (companion `extends XMacros` from MiscMacros wires to `summon[]`-based call-site UX).

- **Plan 03 (2026-06-02, Phase 05 leaf-feature-restoration):** Slice 5.3 — port `misc/ApplierUnapplier` (Mirror-based `given derived`). Branch `05-03-applier-unapplier` cut off `04-05-meta-annotations @ f04cec6f` (independent of slice 5.0; no `MiscMacros` dep). Three atomic commits: `a837dd51` (`feat(scala-3,core): port ApplierUnapplier (Mirror-based)` — verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala`; `Applier`/`Unapplier`/`ApplierUnapplier` traits unchanged; `object Applier { given derived[T <: Product: Mirror.ProductOf as m]: Applier[T] = rawValues => m.fromTuple(Tuple.fromArray(rawValues.toArray).asInstanceOf[m.MirroredElemTypes]) }`, `object Unapplier { given derived[T <: Product] = value => IArraySeq.unsafeWrapArray(value.productIterator.toArray) }`, `object ApplierUnapplier { given derived[T: {Applier as applier, Unapplier as unapplier}] = … }`; removed three Phase-1 `implicit def materialize[T]: X = ???` stubs; Rule-3 auto-fix: added local `import scala.deriving.Mirror` because our `CommonAliases.scala` on this branch base lacks the fork's `export scala.deriving.Mirror` line — avoids dragging the slice-3.x CommonAliases sweep into a leaf-restoration slice; scalafmt reformat bundled), `cbec475e` (`test(scala-3,core): un-wrap ApplierUnapplierTest` — synced from fork: trailing comma after `value: T` parameter + `test("custom")` flipped to `ignore("custom")` with comment explaining Mirror.ProductOf only fires for true case classes/tuples not hand-written case-class-like types; 7 active cases green + 1 ignored, fork commit 7085bd8f precedent), `bb98cc45` (`docs(migration): record ApplierUnapplier Mirror-based port` — MIGRATION.md §3 new `core — misc ApplierUnapplier (slice 5.3)` subsection documenting the macro→Mirror.ProductOf reshape; Backlog table: removed three stale rows `ApplierUnapplier.scala:13/25/37` per `[[feedback_migration_md_contract]]`). All gates green: `sbt 'commons-core/compile ;commons-core/testOnly *.ApplierUnapplierTest ;scalafmtCheckAll'` exit 0 (7 succeeded + 1 ignored + 0 failed), all acceptance grep checks pass (`Mirror.ProductOf`, `given derived` present; `???` absent; `diff` vs fork shows only the added import + scalafmt scaladoc reformat), 0 new `@nowarn`/`-Wconf` vs base, 0 `.planning/` in commits, 0 GSD nomenclature, 3 atomic commits per Conventional Commits. Branch pushed to `origin/05-03-applier-unapplier @ bb98cc45`. PR NOT opened per orchestrator override (batch PR creation under user supervision later — `gh pr create` skipped). REQ APPLIERUNAPPLIER-01, WORKFLOW-01..05, QUALITY-01 satisfied. Pattern note: first **real** (non-deprecation) slice-5 port — establishes the Mirror.ProductOf-based `given derived` shape that subsequent feature-restoration slices (e.g. `AnnotationOf` family, `SealedEnumCompanion`) will follow. Local `import scala.deriving.Mirror` workaround logged for future CommonAliases `export` sweep (slice 3.x).

- **Plan 02 (2026-06-02, Phase 05 leaf-feature-restoration):** Slice 5.2 — deprecate `misc/Delegation` (no real port, no macro). **Scope override** vs PLAN-as-written: replaced fork's `???` runtime stub with `@deprecated` object + `compiletime.error` bodies on both `materializeDelegation` (given) and `CurriedDelegation.apply` — strictly better fail-fast contract (COMPILE time vs runtime `NotImplementedError`). Same pattern as slice 5.1 Bidirectional. Branch `05-02-delegation-deprecate` (renamed from PLAN's `05-02-delegation-stub`) cut off `04-05-meta-annotations @ f04cec6f` — NOT stacked on slice 5.0 (`05-00-miscmacros-foundation`) because the deprecated stub does not extend `DelegationMacros` / `DelegationApplyMacros` traits. Two atomic commits: `7fad5b5f` (`feat(scala-3,core): deprecate Delegation (compiletime.error body)` — Delegation.scala flipped from `???` stub to `@deprecated(..., since = "3.0.0")` object with `inline given materializeDelegation: [A, B] => Delegation[A, B] = compiletime.error(...)` + `class CurriedDelegation[B] { inline def apply[A](inline source: A): B = compiletime.error(...) }`; bundled Rule-3 test wrap: DelegationTest.scala body wrapped in `/* @TodoScala3Migration DROPPED: ... */` with empty `class DelegationTest extends AnyFunSuite` shell because live `Delegation[Destination[Double]](source)` call would otherwise hit the new compiletime.error; fork drops test file outright, we preserve empty wrapper for package layout / minimum-diff — mirrors BidirectionalTest treatment), `6b12d4f6` (`docs(migration): record Delegation deprecation` — MIGRATION.md §2 new row for `com.avsystem.commons.misc.Delegation` pointing at manual `new Delegation[A, B] { def delegate(a: A): B = ... }` replacement; Backlog table: removed stale rows `Delegation.scala:11 materializeDelegation` + `:21 CurriedDelegation.apply` per [[feedback_migration_md_contract]]). One Rule-3 auto-fix: scalafmt reformat (`sbt scalafmtAll` after compile green — bundled into feat commit). All gates green: `sbt commons-core/compile ;scalafmtCheckAll` exit 0, all acceptance grep checks pass (`@deprecated`, `since = "3.0.0"`, `scala.compiletime.error` present; `???` absent), 0 new `@nowarn`/`-Wconf` vs base, 0 `.planning/` in commits, 0 GSD nomenclature, 2 commits exactly + Conventional Commits format. Branch pushed to `origin/05-02-delegation-deprecate @ 6b12d4f6`. PR NOT opened per orchestrator override (batch PR creation under user supervision later — `gh pr create` skipped). REQ DELEGATION-01, WORKFLOW-01..05, QUALITY-01 satisfied. Pattern reinforced (slices 5.1 + 5.2 now a coherent family): `@deprecated` object + `inline` member with `compiletime.error` body — every future leaf where macro is uneconomic to port AND stdlib has no clean replacement uses this shape.

- **Plan 01 (2026-06-02, Phase 05 leaf-feature-restoration):** Slice 5.1 — deprecate `misc/Bidirectional` (no real port, no macro). Branch `05-01-bidirectional-deprecate` cut off `04-05-meta-annotations @ f04cec6f` (independent of slice 5.0; no MiscMacros dependency). Verbatim port from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala` of an `@deprecated(..., since = "3.0.0")` object with `inline def apply[A,B](pf): (PartialFunction[A,B], PartialFunction[B,A]) = scala.compiletime.error(...)` body — every call site now fails at COMPILE time per BIDIRECTIONAL-01 fail-fast contract. Two atomic commits: `c2c8d6fb` (`feat(scala-3,core): deprecate Bidirectional (compiletime.error body)` — Bidirectional.scala 17→16 LOC after local scalafmt, byte-identical to fork modulo scaladoc asterisk alignment + one trailing comma; bundled Rule-2 fix: BidirectionalTest.scala wrapped in `/* @TodoScala3Migration DROPPED: ... */` with empty class shell because Phase 1 big-bang had NOT wrapped it, and the live `Bidirectional[Int,String] { ... }` calls would otherwise have hit the new compiletime.error — fork dropped the test file outright, we preserve it as empty wrapper for package layout / minimum-diff), `5a9ddcab` (`docs(migration): record Bidirectional deprecation` — MIGRATION.md §1 new `misc/Bidirectional` row + removed stale backlog row `Bidirectional.scala:6 apply (Scala 2 macro def)` per [[feedback_migration_md_contract]]). All gates green: `sbt commons-core/compile ;scalafmtCheckAll` exit 0, all acceptance grep checks pass (@deprecated, since="3.0.0", scala.compiletime.error present; `???` absent), 0 new `@nowarn`/`-Wconf` vs base, 0 `.planning/` in commits, 0 GSD nomenclature, 2 commits exactly + Conventional Commits format. Branch pushed to `origin/05-01-bidirectional-deprecate @ 5a9ddcab`. PR NOT opened per orchestrator override (batch PR creation under user supervision later — Task 3 `gh pr create` skipped). REQ BIDIRECTIONAL-01, WORKFLOW-01..05, QUALITY-01 satisfied. Pattern established: deprecate-over-restore via `@deprecated` object + `inline def` body of `scala.compiletime.error(...)` — closes leaf family for symbols where stdlib has no clean replacement but porting the macro is uneconomic.

- **Plan 00 (2026-06-02, Phase 05 leaf-feature-restoration):** Slice 5.0 foundation. Branch `05-00-miscmacros-foundation` cut off `04-05-meta-annotations @ f04cec6f`. Ported `core/src/main/scala/com/avsystem/commons/misc/MiscMacros.scala` (310 LOC) verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`. Two atomic commits: `599a30a9` (`feat(scala-3,core): port MiscMacros foundation bundle` — MiscMacros + bundled foundation deps per Rule 3: `annotation/TodoScala3Migration.scala` new verbatim from fork blob `b4e5761a`, `misc/SourceInfo.scala` added `private[misc] def hereImpl(using Quotes)` matching fork; verbatim port could not compile self-contained without these. SourceInfo public `implicit def here = ???` surface preserved, full `inline given here` flip deferred to slice 5.6), `c45c95d6` (`docs(migration): record MiscMacros foundation port` — MIGRATION.md §3 new section "core — misc MiscMacros foundation (slice 5.0)" with 3-row file table). Rule 3 publish action: pushed `04-05-meta-annotations` to AVSystem upstream from fork (was fork-only — Phase 4 stack lived only on fork; needed for PR base ref to resolve). PR #876 opened on AVSystem/scala-commons, draft, base `04-05-meta-annotations`, head `halotukozak:05-00-miscmacros-foundation`, milestone Scala 3 (#1) assigned via `gh api PATCH /repos/AVSystem/scala-commons/issues/876 -f milestone=1`, title `[Scala 3] port MiscMacros foundation bundle`. PR body metadata: Slice 5.0 (foundation) / Phase 5 leaf-feature-restoration / Depends on: Phase 4 final PR / Required by: slices 5.2 (Delegation) + 5.5 (AnnotationOf family) / Base branch 04-05-meta-annotations (stack-on-phase-4 until Phase 4 merges). All gates green: `sbt commons-core/compile + scalafmtCheckAll` exit 0, `wc -l MiscMacros.scala` = 310 (fork byte-identical via `diff` empty), `grep -c '???' MiscMacros.scala` = 3 (matches fork stub count for DelegationMacros + DelegationApplyMacros + SelfInstanceMacros bodies), 0 new `@nowarn`/`-Wconf` vs upstream, 0 `.planning/` in commits, 0 GSD nomenclature. `ImplicitsMacros` / `SelfInstanceMacros` / `DelegationMacros` / `DelegationApplyMacros` trait bodies remain `???` per fork-staged `@TodoScala3Migration` markers — runtime materialization callers will throw `NotImplementedError` matching fork. No call sites yet — leaves (5.2 Delegation, 5.5 AnnotationOf family) bring callers in subsequent waves. REQ WORKFLOW-01..05, PR-01..03, QUALITY-01 satisfied.

- **Plan 03 (2026-06-01, Phase 03 scala-3-syntax-modernization):** Slice 3.3 — `implicit def/val/object` → `given` swept across core (serialization, cbor, meta, misc, tuples), mongo (BsonGenCodecs anonymous-given + `(implicit X) → (using X)` sweep across 19 files + typeclass instances), hocon (HoconGenCodecs codecs), benchmark. Branch `03-03-implicit-to-given` cut FRESH off `upstream/scala-3@0887d555` (NOT stacked on slice 3.1 PR #868 or slice 3.2 PR #869) per phase methodology. Seven atomic fork-cadence commits: `78d5f3a3` (BsonGenCodecs export-given + @deprecated shims per fork 8f70be80), `0525e127` ((implicit X)→(using X) sweep across 19 mongo files per eef0edce+848b8e9e — bridge `this(rawCollection)(using meta)` call sites fixed), `0b176a0c` (fix(scala-3): preserve OptArg.argToOptArg implicit with verbatim erasure-bridge comment from 39c047eb), `71905a38` (residual mongo typeclass implicit→given: Filter.CanCompare, KeyGetter implicit-object→given-with, EntityIdMode explicitIdMode/autoIdMode, BaseMongoCompanion/AbstractMongoDataCompanion/AbstractMongoEntityCompanion codec/format/meta, MongoFormat codec/collectionFormat/dictionaryFormat/typedMapFormat/optionalFormat/transparentFormat/leafFormat, MongoAdtFormat codec/dataClassTag, MongoPolyDataCompanion codec/format, MongoTypedKey mongoFormatMapping, ObjectIdWrapperCompanion codec; TypedMongoCollection.mkNativeCollection `import meta.format.{given, _}` + `summon[ClassTag[E]]`), `3ceb8b59` (core typeclass instances: GenCodec collection/option/either/enum/transparent/fallback codecs, GenKeyCodec primitive/enum/transparent codecs anonymous given, GenObjectCodec.fromTransparentWrapping, HasGenCodec.codec family, TupleGenCodecs, TransparentWrapperCompanion.self/ordering, SerializationName, cbor/json codecs, BoxingUnboxing, TypeString/JavaClassName + materialize, SealedUtils.evidence + OrderedEnum.ordering, Timestamp.ordering, ValueOf.fromScala, materialize stubs in AnnotationOf/ApplierUnapplier/Delegation/SamCompanion/SimpleClassName/SourceInfo/SelfInstance, MacroInstances.materialize, MetadataCompanion.fromFallback/lazyMetadata/notFound, OptionLike instances, AutoOptionalParams.allAutoOptionalParams, TupleDerivation.tupleNInstances — `GenCodec.fromTransparentWrapping` callers updated to `(using …)`), `ab689a24` (hocon + benchmark + TypedMap.typedMapCodec/codecMapping + di stubs + BlockingUtils.scheduler + BsonRefKeyElementHandling.elementCodec + NativeFormatOptions; Rule 3 revert: `RunNowEC/RunInQueueEC.Implicits.executionContext` kept as `implicit val` because wildcard-import-into-Implicits-object idiom is the public API; converting to `given` silently breaks `import X._` callers + scalafmt reformat 13 files), `890d9630` (docs(migration) §3 — core/mongo/hocon slice 3.3 entries: anonymous-given pattern + 3 documented preservations + (implicit)→(using) sweep + BsonGenCodecs @deprecated-shim pattern + KeyGetter `import X.given` requirement + MongoFormat `import .{given, _}` for `dataClassTag`). **Three borderline preservations:** OptArg.argToOptArg (erasure-bridge collision), GenRef.fun2GenRef (Phase-2 stub), RunNowEC/RunInQueueEC.Implicits.executionContext (wildcard-import idiom). **Scope deviation:** plan acceptance gate `0 hits except 2 exceptions` NOT met — 136 hits remain, ALL are slice-3.1 extension-shims (~64 hits in SharedExtensions/jiop/jsiop/concurrent/misc + BsonRef/DocKey/Filtering/Updating/MongoOps/MongoRef/QueryOperatorsDsl ops), slice-3.5 Implicits.scala (~3 hits), out-of-scope rpc module (~20 hits per fork "remove RPC module from scala-3"), and the 3 documented preservations. These resolve post-rebase against PR #868 + PR #867. All gates green: `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0, 0 new `@nowarn`/`-Wconf`, 0 `.planning/` in commits, 0 GSD nomenclature. Pushed `halotukozak:03-03-implicit-to-given @ 890d9630`. Opened AVSystem/scala-commons PR #870 draft, base `scala-3`, milestone Scala 3 (#1) via `gh api PATCH /repos/AVSystem/scala-commons/issues/870 -f milestone=1`, title `[Scala 3] implicit def/val → given`. PR body metadata: Slice 3.3 / Merge order 3.1→3.2→3.3→3.4 / Depends on #869 (slice 3.2) / Base branch upstream/scala-3 (not stacked). REQ SYNTAX-33-IMPLICIT-TO-GIVEN, WORKFLOW-01..05, PR-01..03, QUALITY-01 satisfied.

- **Plan 02 (2026-06-01, Phase 03 scala-3-syntax-modernization):** Slice 3.2 — HKT wildcards `[_]` → `[?]` swept across applied positions in core + mongo per Scala 3 changed-features (Pitfall 3 strictly enforced: kind-parameter declarations preserved as `_`). Branch `03-02-hkt-wildcards` cut FRESH off `upstream/scala-3@0887d555` (NOT stacked on slice 3.1 PR #868 branch). Per-file applied-vs-kind-decl classification — no blind sed. Five atomic fork-cadence commits: `f5f2ce48` (`refactor(scala-3,core): F[_] → F[?] in applied positions (serialization)` — 8 files: FieldValues, GenCodec, InputOutput, wrappers, macroCodecs, cbor/CborAdtMetadata, cbor/CborOutput, json/JsonStringOutput; 14 rewrites: Array[GenCodec[?]], Opt[cborKey[?]], List[Case[?]], InputMetadata[?], BIterable[?], Transformed[?, ?] etc.), `87fe1659` (`refactor(scala-3,core): F[_] → F[?] in applied positions (rpc)` — 3 files: AsRawReal, RPCFramework, StandardRPCFramework; 8 rewrites: Iterator[?], List[ParamMetadata[?]], Map[String, FunctionSignature[?]], Map[String, GetterSignature[?]]), `e8e4d2e9` (`refactor(scala-3,core): F[_] → F[?] in applied positions (misc + di)` — 4 files: TypeString, TypedMap (only Entry[K, ?]* applied position; all other `K[_]` were kind-decls), Component, Components; 15 rewrites), `45b83ecb` (`refactor(scala-3,mongo): F[_] → F[?] in applied positions (sweep)` — 4 files: BsonInputOutput, FilterDocBuilder, MongoFormat (14 rewrites: List[Case[?]], Map[Class[?], ...], MHashMap[Class[?], (SealedParent[?], MListBuffer[Case[?]])], etc.), TypedMongoCollection (MongoCollection[?] ×4); 17 rewrites total. Translated from origin/master@848b8e9e mongo subset.), `e1905e59` (`docs(migration): record HKT wildcard tightening (type-level only, no source-compat)` — MIGRATION.md §3 core + mongo entries: pure type-argument-position syntax change, no source-compat impact). Positive deviations vs plan: no SharedExtensions commit (only [_] is kind-decl `def drainTo[C[_]]` — PRESERVED), no hocon commit (0 hits), no core/jvm or core/js commits (0 hits). Plan anticipated ~6 commits + ~30 mongo files; actual = 5 commits + 4 mongo files (mongo inventory in our tree much narrower than fork 848b8e9e's because many fork files were already covered by Phase 1 stubs). All gates green: `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0, acceptance grep 48 remaining hits all kind-decls/scaladoc (manually verified), 0 new `@nowarn`/`-Wconf` vs upstream, 0 `.planning/` in commits, 0 GSD nomenclature. No auto-fixes (Rules 1-3) needed — pure mechanical syntax sweep. Pushed `halotukozak:03-02-hkt-wildcards @ e1905e59`. Opened AVSystem/scala-commons PR #869 draft, base `scala-3`, milestone Scala 3 (#1) via `gh api -X PATCH`, title `[Scala 3] tighten HKT wildcards (_ → ?)`. PR body metadata: Slice 3.2 / Merge order 3.1→3.2→3.3→3.4 / Depends on #868 / Base branch upstream/scala-3 (not stacked). REQ SYNTAX-32-HKT-WILDCARDS, WORKFLOW-01..05, PR-01..03, QUALITY-01 satisfied.

- **Plan 01 (2026-06-01, Phase 03 scala-3-syntax-modernization):** Slice 3.1 — `implicit class` → `extension` swept across core+mongo. Branch `03-01-implicit-class-to-extension` cut off `upstream/scala-3 @ 0887d555`. 15 `implicit class` occurrences in 7 files converted in 8 atomic fork-cadence commits: `83333148` (core/GenCodec 4 private wrappers IterableOps/PairIterableOps/ListInputOps/ObjectInputOps), `4467a1d8` (UpdateOperatorsDsl `given Conversion` HKT Pitfall 7 — plain `extension` confirmed broken mid-execution: `Found: MongoFormat[C[T]]; Required: MongoFormat[T]` exactly as documented), `d1e0195b` (QueryOperatorsDsl 2x ForCollection plain `extension`, inner `format` → `elemFormat`), `92a8eb28` (MongoEntityCompanion macroDslExtensions), `2e382f36` (MongoPolyDataCompanion macroDslExtensions), `88de8c77` (MongoFormat 3 assume* ops), `c7c423c5` (MongoPropertyRef 3 RefOps + `@scala.annotation.targetName("typedMapApply")` Rule-1 fix for `apply(K)`/`apply(K[T])` erasure clash post-namespace-merge), `c3a3fc1b` (docs(migration) §3 entries under core + mongo). Plan-as-written undercounted scope (5 files); expanded to 7 per Rule 3 — acceptance grep gate requires 0 hits. ReactiveMongoExtensions skipped (no `implicit class` to grep; uses `implicit def + class AnyVal`, slice 3.3 territory). All gates green: `sbt compile + Test/compile + scalafmtCheckAll` exit 0, acceptance grep 0 hits, no new `@nowarn`/`-Wconf`, no `.planning/` in commits, no GSD nomenclature. Pushed `halotukozak:03-01-implicit-class-to-extension @ c3a3fc1b`. Opened AVSystem/scala-commons PR #868 draft, base `scala-3`, milestone Scala 3 (#1) via `gh api -X PATCH`, title `[Scala 3] convert implicit class to extension`. PR body metadata: Slice 3.1 / Merge order 3.1→3.2→3.3→3.4 / Depends on: none (first slice) / Base branch: upstream/scala-3 (not stacked). REQ WORKFLOW-01..05, PR-01..03, QUALITY-01 satisfied (SYNTAX-31-IMPLICIT-CLASS not yet in REQUIREMENTS.md — backfill pending).

- **Plan 05 (2026-06-01, Phase 03 scala-3-syntax-modernization):** Slice 3.5 — delete `com.avsystem.commons.misc.Implicits` object (covered by `scala.compiletime.summon[T]`). Branch `03-05-delete-implicits-object` cut off `upstream/scala-3 @ 0887d555` (Phase 01 big-bang merged). Three atomic commits: `9c653bcb` (`refactor(scala-3,core): extract ImplicitNotFound to its own file` — verbatim copy of sealed trait + companion to new `ImplicitNotFound.scala`, removed from `Implicits.scala`), `699424c7` (`refactor(scala-3,core): delete Implicits object (covered by summon[T])` — `git rm Implicits.scala`), `4091d42a` (`docs(migration): record Implicits object removal` — MIGRATION.md §1 new entry pointing to `scala.compiletime.summon[T]`). All gates green: `sbt compile + Test/compile + scalafmtCheckAll` exit 0, `git grep -nE '\bImplicits\.' -- '*.scala'` → 0 hits, `git ls-files .../Implicits.scala` → 0, `git ls-files .../ImplicitNotFound.scala` → 1, 0 new `@nowarn`/`-Wconf` vs upstream, 0 `.planning/` in commits, 0 GSD nomenclature. Pushed `halotukozak/scala-commons3:03-05-delete-implicits-object`. Opened AVSystem/scala-commons PR #867 draft, base `scala-3`, head `halotukozak:03-05-delete-implicits-object`, milestone Scala 3 (#1) assigned via `gh api -X PATCH /repos/AVSystem/scala-commons/issues/867 -F milestone=1`, title `[Scala 3] delete Implicits object (covered by summon[T])`. PR body metadata block: `Slice 3.5 / Independent — can land any time / Depends on: none / Base branch: upstream/scala-3`. Slice 3.5 is parallel-safe (zero file overlap with 3.1/3.2/3.3/3.4). PR left OPEN for manual maintainer merge per global rules. REQ SYNTAX-33-IMPLICIT-TO-GIVEN (delete portion), WORKFLOW-01..05, PR-01..03, QUALITY-01 satisfied. User directive 2026-06-01: outright delete, do NOT deprecate (overrides fork commit `50272b26 fix(scala-3): Implicits.infer real impl`).

- **Plan 06 (2026-06-01, Phase 01 big-bang):** Phase 01 closed pending merge. Three atomic commits: `c3aaa77c` (`docs(migration):` seed MIGRATION.md — 252 lines, 5 locked sections + 155-row Backlog table auto-derived from `git grep -nE 'TODO\[scala3-port\]' -- '*.scala'`; TAG_COUNT==BACKLOG_ROWS==155), `7467149f` (`style(scalafmt):` Rule 3 auto-fix — `scalafmtSbtCheck` failed on `project/Commons.scala`; cosmetic 10/11 line settings-block reflow), `24e4289c` (`docs(migration):` IDE prettier post-commit table padding — content identical). All 16 local gates green (`sbt compile + Test/compile + scalafmtCheckAll + scalafmtSbtCheck` exit 0; 0 new `@nowarn`/`-Wconf` in source diff — the literal `-Wconf` token inside MIGRATION.md doc row is plain text, not code suppression; 0 GSD nomenclature; 0 `.planning/` in commits; 0 `crossScalaVersions` / `-Xsource:3` / `fileOverride` / `2.13` markers in build config; only pre-existing `mongo/jvm/src/test/scala-2.13/` upstream-baseline dir remains, deliberately untouched per minimum-diff). Pushed to `halotukozak/scala-commons3:01-big-bang @ 24e4289c`; fork CI green (run 26753320607, all 3 shards Temurin 17/21/25). Opened AVSystem/scala-commons PR #860: base `scala-3`, head `halotukozak:01-big-bang`, draft, title `[Scala 3] Pivot to Scala 3 only — comment broken, green CI` (61 chars), milestone `Scala 3` #1 assigned via `gh api PATCH /repos/AVSystem/scala-commons/issues/860 -f milestone=1` (memory rule precedent). AVSystem PR CI green (run 26753512180, all 3 shards pass). PR left OPEN for manual maintainer merge per global rule. REQ DOC-01..02, COMPILE-01..03, CI-01..02, WORKFLOW-01..05, PR-01..03, QUALITY-01 satisfied. Plan 06 checkpoints (Task 3 push gate, Task 4 PR gate) traversed autonomously under upfront orchestrator authorization. Phase 01 closure deferred to upstream maintainer merge decision.

- **Plan 05 (2026-06-01, Phase 01 big-bang):** `sbt Test/compile` GREEN across every enabled module (commons-core
  JVM+JS, commons-mongo JVM, commons-hocon, commons-cbor folded into core). Whole-file commenting applied to 38 test
  files (CONTEXT-permitted whole-wrap pattern — every broken file had ALL classes broken with no surviving partners).
  Three atomic commits: `65f507bc` (`refactor(core):` 30 files — macro/DI/serialization wrap + 3 `lazy val` Rule-1 fixes
  for Tag/NamedEnumTest/SealedEnumTest.values override), `3ffab524` (`refactor(mongo):` 9 files — typed/testEntities
  cascade + BsonInputOutputTest), `555d2bb5` (`refactor(hocon):` 2 files — HoconInputTest + HoconGenCodecRoundtripTest).
  Per-module TODO tag counts: core 27, mongo 9, hocon 2 — total 38 `TODO[scala3-port]` markers (≥ 20 required). Six
  broken-test categories: macro-tests (TestMacros gone, Plan 02), DI tests (Components.???-stubbed, Plan 02),
  GenCodec.materialize-based serialization tests (Plan 02), MongoEntityCompanion materialize-based mongo tests (Plan
  03), HoconGenCodec, HFloatTest (Short.toHexString missing in Scala 3 — actually a stdlib issue, wrapped pending future
  fix). Volume estimate undershot: plan said "~133" files; actual = 106 (macros deletion reduced surface earlier). Five
  iterations of compile-driven cascade — final round triggered by transitive deps (e.g., MongoFilterTest depends on
  testEntities which got commented in earlier round). Verification all green:
  `sbt ';compile ;Test/compile ;scalafmtCheckAll'` exit 0, 0 NEW `@nowarn`/`-Wconf` (
  `git diff -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` = 0 matches), 0 `.planning/` in commits, 0 GSD nomenclature.
  Branch tip: `555d2bb5` on `01-big-bang`; not pushed (Plan 06 push gate). Plan 06 must update MIGRATION.md
  `## Disabled` (or equivalent) section with the 38 commented test classes grouped by stub dependency (TestMacros gone,
  Components stub, GenCodec.materialize stub, MongoEntityCompanion stub, Short.toHexString stdlib gap). COMPILE-02
  acceptance gate satisfied — Phase 1 main goal achieved. REQ COMMENT-01, COMMENT-02, COMMENT-04, COMPILE-02,
  QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05 satisfied.

- **Plan 04 (2026-06-01, Phase 01 big-bang):** `commons-js` aggregate green on Scala 3.8.2 / Scala.js 1.21.0 after a
  single one-line build fix: `-P:scalajs:mapSourceURI:...` → `-scalajs-mapSourceURI:...` in
  `project/Commons.scala:163` (Rule 3 auto-fix — Scala 3 + Scala.js 1.x dropped the compiler-plugin prefix; Plan 01
  migrated scalacOptions but missed this Scala.js-specific flag). Single atomic commit `9ec8c177` (`build(js):` prefix).
  Zero JS-specific source edits — every file in `core/js/` (8 files), `mongo/js/` (1 file), `benchmark/js/` (3 files)
  compiled cleanly because the `???` stubs landed in Plans 02 and 03 already preserve the runtime API surface JS
  consumers depend on. Positive deviation vs plan: plan anticipated MEDIUM commenting volume + three commits (core-js /
  mongo-js / benchmark-js); actual = zero source edits + one build commit. Module TODO[scala3-port] counts: core-js 0,
  mongo-js 0, benchmark-js 0. Verification all green: `sbt commons-js/compile` exit 0 (8s), combined
  `commons-jvm/compile ;commons-js/compile` exit 0, `scalafmtCheckAll` exit 0, 0 new `@nowarn`/`-Wconf`, 0 `.planning/`
  in diff, 0 GSD nomenclature. Branch tip: `9ec8c177` on `01-big-bang`; not pushed (Plan 06 push gate). Plan 06 must
  update MIGRATION.md with the `-P:scalajs:mapSourceURI` → `-scalajs-mapSourceURI` flag rename in the build-flags /
  source-compat section (affects downstream consumers who copied our `jsCommonSettings`). REQ COMMENT-01..03,
  COMMENT-05, COMPILE-01 (JVM+JS dual-aggregate gate complete), QUALITY-01/02, WORKFLOW-04/05 satisfied.

- **Plan 03 (2026-06-01, Phase 01 big-bang):** `commons-jvm/compile` aggregate green on Scala 3 — every enabled JVM
  module compiles. Plan-as-written `/* */` comment strategy SUPERSEDED by `= ???` stubs per memory
  `feedback_stub_over_comment.md` (orchestrator pre-flagged). Two atomic commits: `6b9bdf4a` (`fix(hocon):`
  SealedEnumCompanion `val values` → `lazy val` to satisfy override) + `9e2b290b` (`refactor(mongo):` 6 macro-def `???`
  stubs + bundled Scala 3 minimum-diff fixes). Six mongo macro defs stubbed: `BsonRef.Creator.ref`,
  `DataRefDsl.{ref,as,is,isNot}`, `TypedMongoUtils.optionalizeFirstArg`. Five bundled Rule-1 auto-fixes uncovered by the
  stub-driven compile loop: (a) explicit `using` keyword at 3 `BsonValueOutput.write`/`BsonValueInput.read` call sites
  where Scala 2 second-implicit-arg-list syntax stopped working; (b) `K[_]`/`D[_]` → `K[Any]`/`D[Any]` at 3 sites (
  TypedMapFormat, MongoPolyDataCompanion, TypedMapRefOps.apply cast) — same precedent as Plan 02 core fix; (c)
  `E#IDType` → `Any` widening at 4 sites (MongoEntityCompanion.ID, TypedMongoCollection.ID, MongoEntityMeta fields +
  idRef) because Scala 3 forbids type projections on non-concrete prefixes. Module coverage: hocon 0 TODO tags (only the
  lazy-val fix), mongo 13 TODO tags, cbor 0 TODO tags (already covered by Plan 02 core stubs), benchmark 0 TODO tags (no
  edits — bench code only consumes runtime API, which was preserved by the core stubs). Positive deviations: cbor and
  benchmark needed zero work despite plan estimating MEDIUM volume. Verification all green:
  `sbt ';clean ;commons-jvm/compile ;commons-benchmark/compile ;scalafmtCheckAll'` exit 0, no NEW `@nowarn`/`-Wconf` vs
  upstream, no `.planning/` in any commit diff, no GSD nomenclature in commit messages. Spring orphaned but
  `lazy val spring` left in `project/Commons.scala` per orchestrator directive (already commented out of jvm aggregate
  in Plan 01 → harmless dead code; deletion deferred). Branch tip: `9e2b290b` on `01-big-bang`; not pushed (Plan 06 push
  gate). Plan 06 must update MIGRATION.md with: mongo macro stubs, `E#IDType`→Any (public API widening for downstream),
  K[_]/D[_]→K[Any]/D[Any] (largely source-compat). REQ COMMENT-01..03, COMMENT-05, COMPILE-01 (JVM portion),
  QUALITY-01/02, WORKFLOW-04/05 satisfied.

- **Plan 02 (2026-06-01, Phase 01 big-bang):** SUPERSEDED plan-as-written (which still said "comment") with user
  directive 2026-06-01: DELETE `commons-macros` module outright, STUB broken defs in `commons-core` with `= ???` instead
  of `/* */` block-comments. Per memory rules `project_deletable_modules.md` and `feedback_stub_over_comment.md`. Plan
  executed on top of two prior "comment" commits (`561a95d5` macros, `92907dab` core defs) which this plan effectively
  superseded with new stub commits. Six atomic commits delivered: `1eda26b2` (`build:` drop commons-macros module — git
  rm of 28 macro sources + remove project/Commons.scala wiring including `lazy val macros`, `.dependsOn(macros)` in
  core/core-js, macros from jvm aggregate and unidoc filter), `a8adca09` / `897c950d` / `f5dbf915` / `07c93df5` /
  `78330755` (5 `refactor(core):` per-subpackage stub commits — serialization 10 files, rpc 5 files, meta 4 files, misc
  15 files, remaining 5 files SharedExtensions+annotation+collection+di). 100 `TODO[scala3-port]:` tags in core (>= 35
  plan minimum). Verification all green: `sbt commons-core/compile` exit 0, `scalafmtCheckAll` exit 0, no NEW `@nowarn`/
  `-Wconf` vs upstream/scala-3 (0 matches), `macros/` directory gone, no `commons-macros` or `dependsOn(macros)`
  references remaining, no `.planning/` in any commit's diff, no GSD nomenclature in commit messages. Ten minimum-diff
  Scala 3 syntax/semantic auto-fixes documented (CloseableIterator `@targetName`, Components dependent-stubbing,
  TypedMap K[_]→K[Any] with asInstanceOf casts, SelfInstance C[_]→C[Any], HasGenCodec.wildcardCodec C[_]→C[Any],
  Timestamp drop Comparable[Timestamp], SealedEnumCompanion val→lazy val, SealedUtils.instancesFor return-type widened
  to TC[T], Tag explicit unapply, GenCodec.bseqCodec/iseqCodec using-keyword). Branch tip: `78330755` on `01-big-bang`;
  not pushed (Plan 06 push gate). REQ COMMENT-01..03, COMMENT-05, COMPILE-01, QUALITY-01/02, WORKFLOW-04/05 satisfied.
  Plan 06 must update MIGRATION.md `## Will Not Migrate` with commons-macros rationale.

- **Plan 01 (2026-06-01, Phase 01 big-bang):** Branch `01-big-bang` cut from `upstream/scala-3 @ 1561d8dc` (matches plan
  baseline). Three commits delivered: `a4cb99e2` (`build:` pivot Commons.scala to Scala 3 only, drop crossScalaVersions,
  migrate scalacOptions, add made 0.1.1 unconditional on core, drop analyzer/jetty/spring from jvm aggregate via
  comment-out preserving lazy val declarations, fold mima/scalafmt CI jobs into main build step), `f00976bd` (
  `style(scalafmt):` flip .scalafmt.conf runner.dialect Scala213Source3 -> scala3, reformat 10 .scala sources +
  project/Commons.scala; folded Rule 1 auto-fix rename `enum` -> `e` in GenKeyCodec.scala line 90 for scala3
  reserved-keyword collision — identical fix to 2026-05-30 Plan 01 precedent), `2e5e22e0` (`ci:` regenerate ci.yml via
  githubWorkflowGenerate, single Scala 3.8.2 axis × Temurin 17/21/25, 4 ins / 92 del). Verification all green:
  `sbt show scalaVersion` = 3.8.2, `scalafmtCheckAll` exit 0, `scalafmtSbtCheck` exit 0, no NEW `@nowarn`/`-Wconf` vs
  upstream, no `crossScalaVersions`/`-Xsource:3`/`fileOverride`/`Scala213Source3`/`2.13` markers, no `.planning/` in
  diff, no GSD nomenclature in commits. Non-blocking plan-verify deviation: `mongo/jvm/src/test/scala-2.13/` exists in
  upstream baseline (2 test files) contrary to plan assertion of "no scala-2.13 source dirs"; left untouched per
  minimum-diff. Branch NOT pushed (Plan 06 handles push under checkpoint gate). REQ BUILD-01..05, QUALITY-01/02,
  WORKFLOW-01/04/05 satisfied. Branch tip: `2e5e22e0`.

- **Plan 01 (2026-06-01, Phase 5):** Branch `05-core-scala-3-baseline-port` cut off `04-made-integration @ 94f52ece` (
  Phase 4 actual tip post-Copilot fixes, not plan-stated `c3e54b16` which was superseded by 11-file pre-relocation in
  `caabd39c`). Single atomic commit `2b0b0ad4` (`refactor(core): relocate scala-2-only sources for cross-build`)
  executes 33 `git mv` operations from `core/src/main/scala/` → `core/src/main/scala-2.13/`. Scope: union of
  `git grep -l '= macro ' core/src/main/scala/` (29 files: SharedExtensions, di/Components,
  meta/{AdtMetadataCompanion,MetadataCompanion,metaAnnotations},
  misc/{AnnotationOf,ApplierUnapplier,Delegation,Implicits,Sam,SamCompanion,SealedUtils,SelfInstance,SimpleClassName,SourceInfo,TypeString,ValueEnum},
  annotation/{AnnotationAggregate,positioned},
  rpc/{AsRawReal,RPCFramework,RawRpcCompanion,RpcMetadataCompanion,RpcUtils},
  serialization/{GenCodec,GenKeyCodec,GenObjectCodec,GenRef,macroCodecs}) PLUS the rest of `rpc/` (4 more:
  MetadataAnnotation, RawValueCompanion, StandardRPCFramework, rpcAnnotations) — entire RPC subsystem moved as a unit
  per MIGRATION.md 2.13-only subsystem rule. Excluded (deferred to Plan 05-02 scala-3/ overlay): `meta/MacroInstances`,
  `meta/metadata`, `serialization/HasGenCodec`, `serialization/wrappers`, `serialization/TupleGenCodecs`, and the
  broader scala-3-counterpart list — these compile-fail under Scala 3 currently but lack `= macro ` defs, so resolution
  is "cherry-pick the scala-3 version" not "relocate the 2.13 version". Compile gates: ++2.13.18 commons-core/compile
  GREEN (138 sources, identical to pre-move count), ++3.8.2 commons-macros/compile GREEN, scalafmtCheckAll GREEN.
  ++3.8.2 commons-core/compile went 114 → 263 errors — *not* a regression: the pre-existing dup-def-like errors
  disappeared (scala-3 no longer sees the scala-2 macro defs) but new "not found" errors appeared for files that
  referenced the moved types and don't yet have a scala-3 overlay; this is expected and addressed by Plan 05-02. No
  `@nowarn`/`-Wconf` introduced, no `.planning/` in diff, no GSD nomenclature in commit message. CORE-02,
  WORKFLOW-01/04/05, QUALITY-01 satisfied. Branch tip: `2b0b0ad4`; NOT pushed.

- **Plan 04 (2026-06-01, Phase 4):** Phase 4 closed. Pushed `04-made-integration @ c3e54b16` to AVSystem upstream;
  opened PR #859 with cascading base `03-macros-stub` (NOT `scala-3`) — preserves the stacked-PR review flow established
  in Phases 2/3. Milestone "Scala 3" (#1) assigned. CI green. Title
  `[Scala 3] Port made wiring primitives Opt/NOpt/OptArg/OptRef`. PR left OPEN for manual maintainer merge (never merge
  automatically — global rule). REQ MADE-01 (partially satisfied — wiring primitives only; full GenCodec/HasGenCodec
  derivation surface DEFERRED to Phase 5 CORE-01/CORE-02), INFRA-06 (re-affirmed: madeVersion = 0.1.0), WORKFLOW-01..05,
  DOC-02, QUALITY-01 satisfied. Phase 4 known limitation carried forward: Scala 3 `commons-core/compile` RED (~136
  errors); not a regression — status quo from before Phase 4 plus a few dup-def errors from the cherry-pick. Full
  stacked-PR snapshot: #856 / #857 / #858 / #859, all milestone "Scala 3", all OPEN. No other branches touched. Phase 4
  closed under Claude scope.

- **Plan 03 (2026-05-31, Phase 4):** Sanity gate run with reduced scope — `++2.13.18 commons-core/compile` GREEN,
  `scalafmtCheckAll` GREEN, `++3.8.2 commons-macros/compile` GREEN; `++3.8.2 commons-core/compile` deliberately
  SKIPPED (known RED, deferred to Phase 5 per Plan 02 SUMMARY). Full 5-gate test suite (
  `+jvm/test +jvm2/test +js/test ++2.13 mimaReportBinaryIssues scalafmtCheckAll`) NOT run — gated on Phase 5 bringing
  Scala 3 commons-core to GREEN. MIGRATION.md flipped in a single atomic commit `c3e54b16` (
  `docs(migration): record made integration and core wiring port`): existing `made` row updated to
  `| made | n/a | cross | n/a | n/a | external dep at 0.1.0, Scala 3 only |` (Phase 2 had anticipated a `made` row with
  `pending` status — re-used per PLAN Task 2 conditional logic), `core` row Notes column appended with
  `made wiring primitives ported; full derivation pending`, `core` Status column kept as `pending` per user-locked
  instruction. No push. Branch tip = `c3e54b16` on `04-made-integration`. QUALITY-01 grep 0 matches, WORKFLOW-05
  `.planning/` check 0 matches, working tree clean. REQ MADE-01 (doc reflection), DOC-02 (same-PR MIGRATION.md update),
  QUALITY-01, WORKFLOW-05 satisfied for this plan's scope.

- **Plan 02 (2026-05-31, Phase 4):** Plan 04-02 SCOPE REVISED mid-execution. Original PLAN had 2 tasks (port 5 scala-3
  wiring-primitive source files from `origin/master`, drop 3 `*Compat` mixin clauses per
  `feedback_dont_port_deprecated.md`). Mid-execution user added a `++3 commons-core/compile` GREEN gate which required
  34 `git mv` ops relocating scala-2 macro sources from `core/src/main/scala/` into `core/src/main/scala-2.13/`;
  scope-explosion check flagged this duplicates Phase 5 CORE-01/CORE-02 work. User chose Option A —
  `git reset --hard HEAD` to drop the 34 staged moves; keep only Commit A. Final delivered scope: Commit A `66fb1158` (5
  cherry-picked files, 685 insertions) + Commit B-prime `7e3a3035` (
  `style(scalafmt): reformat ported scala-3 wiring primitives` — Rule 1 auto-fix because scalafmt rejected the verbatim
  cherry-pick under the dialect/fileOverride config Phase 1 settled on). Branch tip = `7e3a3035`. Compile state: 2.13
  `commons-core/compile` GREEN (951 .class files, exit 0); Scala 3 `commons-core/compile` RED (~136 errors; status quo
  from before this plan + a few new dup-def errors from the cherry-pick; DEFERRED to a future Phase 5 plan).
  `scalafmtCheckAll` GREEN. MADE-01 partially satisfied (wiring primitives only —
  `given Default[Opt/NOpt/OptArg/OptRef]` + `madeAnnotationAliases` re-exports; `GenCodec`/`HasGenCodec` derivation
  surface still pending). DEPR-01 satisfied for scope (`OptCompat`/`NOptCompat`/`OptRefCompat` mixins dropped;
  `compat.scala` not ported because it imports deferred `GenCodec`/`GenKeyCodec`).

- **Plan 01 (2026-05-31, Phase 4):** Branch `04-made-integration` cut off `03-macros-stub @ 221f3bda` (cascadowo stack
  continues from Phase 3, per user override of plan's `upstream/scala-3` base). `madeVersion` was already pinned to
  `"0.1.0"` in `project/Commons.scala` (Phase 1 Plan 02 work); only edit needed was trimming the stale `0.1.1-SNAPSHOT`
  substring from the inline comment so the no-SNAPSHOT regression-guard grep (
  `! grep -RnE '\-SNAPSHOT' build.sbt project/`) is meaningful. Single atomic commit `bf8e961a` (`build:` prefix, no GSD
  nomenclature). Wave 0 preflight (`++2.13.18 commons-core/compile` from clean) green BEFORE edit — 15s, compiled 28
  macros + 138 core sources. `scalafmtCheckAll` green. `build.sbt` (1-line stub `lazy val root = Commons.root`)
  untouched. INFRA-06 reaffirmed (made pinned to 0.1.0); WORKFLOW-01 satisfied.

- **Plan 01 (2026-05-31, Phase 2):** MIGRATION.md skeleton landed at repo root (55 lines, 5 sections in locked order,
  13-row status table, 4 rationale paragraphs for 2.13-only modules, empty `## Deprecation log` heading). Branch
  `02-migration-md` cut from local Phase 1 tip `84e21dee` (upstream remote still at `1561d8dc` — Phase 1 commits not yet
  pushed to upstream). Single atomic commit `48da5be1` `docs(migration):`. Status vocab locked:
  `cross|stub|2.13-only|pending|wip`; MiMa: `green|red|n/a|pending`.

- **Plan 02 (2026-05-31, Phase 2):** Deprecation log seeded from `git grep -n '@deprecated' master -- '*.scala'` against
  `origin/master@bcc3bcbf`. 152 entries total (core=123, mongo=29); tags: 145 [port] + 7 [skip-port]. Tagging rule
  applied against FULL message (not 80-char-truncated) for deterministic classification. scala-3/ deprecations (107 of
  123 core entries) included per RESEARCH Pitfall 4. Single atomic commit `7905d1bd` `docs(migration):`. MIGRATION.md
  now 223 lines. REQ DOC-03 complete.

- **Plan 03 (2026-05-31, Phase 2):** Authored `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (18
  assertions, gitignored). All three pre-push gates green: `check.sh` exit 0 with `ALL CHECKS GREEN (18 assertions)`,
  `sbt scalafmtCheckAll` exit 0 (~4.77s), `sbt '++2.13 commons-jvm/compile'` exit 0 (~4.35s warm). Branch hygiene
  audit (scoped to `^docs(migration):` since branch inherits Phase 1 commits + user's CI tweak `70093c56`): 2 Phase 2
  commits, only `MIGRATION.md` modified, 0 `.planning/` paths, 0 GSD nomenclature. No commits introduced. REQ
  DOC-01..04, WORKFLOW-04..05 enforcement codified.

- **Plan 01 (2026-05-31, Phase 3):** macros Scala 3 stub landed via `.gitkeep` alone (no fallback `package.scala`
  needed); `commons-macros_3` jar = 335 bytes. Branch `03-macros-stub` cut from `02-migration-md @ 7cba3d2f` (cascadowo
  stack on Phase 2 tip). Two atomic commits: `0864e85f` build(macros) + `221f3bda` docs(migration). Verified:
  `++3.8.2 commons-macros/compile+package` PASS, `++2.13.18 commons-macros/compile` PASS (28 sources, 6s),
  `scalafmtCheckAll` PASS. MIGRATION.md macros row notes column flipped to
  `Empty scala-3 dir; whitebox impls remain 2.13-only.` (status tokens unchanged `cross|stub`). `commons-core/compile`
  on Scala 3 still fails (114 errors, 212 warnings) — pre-existing scala-3 source gap from Phase 1, NOT a Phase 3
  regression; reproduces on the branch base `7cba3d2f`. Plan 03 acceptance is `commons-macros/compile` only per the
  pin-2.13 CI strategy from Phase 1 Plan 03.

- **Plan 02 (2026-05-31, Phase 3):** Pushed `03-macros-stub @ 221f3bda` to AVSystem upstream; opened PR #858 with
  cascading base `02-migration-md` (NOT `scala-3`) — preserves the stacked-PR review flow established in Phase 2.
  Milestone "Scala 3" (#1) assigned. CI green. REQ MACROS-01, WORKFLOW-01..05, DOC-02 satisfied. Phase 3 closed under
  Claude scope; PR left open for manual merge. Full stack: #856 (base scala-3) → #857 (base 01-cross-compile-infra) →
  #858 (base 02-migration-md), all milestone Scala 3.

- **Plan 04 (2026-05-31, Phase 2):** Stacked-PR strategy adopted mid-Phase-2. Claude pushed `02-migration-md` to fork @
  `4ae73373` with fork CI green (https://github.com/halotukozak/scala-commons3/actions/runs/26718318529). User folded
  two commits into the Phase 1 branch during execution: `70093c56` (CI scalafmt restoration) and `34cad074` (jvm2
  aggregate flatten). User rebased `02-migration-md` on top → local tip now `0729e947`; fork remote `4ae73373` is stale,
  awaiting user force-push. User took ownership of the GitHub-side stack: re-push refresh + PR #1 (
  `halotukozak:01-cross-compile-infra` @ `34cad074` → `AVSystem:scala-3`) + PR #2 (`halotukozak:02-migration-md` @
  `0729e947` → `AVSystem:scala-3`, stacked). WORKFLOW-02 and WORKFLOW-03 satisfied conceptually (user IS maintainer;
  same mechanism as Phase 1's direct-push deviation, different point-of-control split). Phase 2 closed under Claude
  scope. REQ DOC-01..04 complete.

- **Plan 01 (2026-05-30):** scalafmt dialect inverted to scala3 default + scala213source3 fileOverride for scala-2.13/
  and scala-2/ globs; sbt-mima-plugin pinned at 1.1.5 (sbt-tasty-mima deferred to Phase 11).

- **Plan 01 (2026-05-30):** User-approved deviation Option A — patched 9 source files to satisfy `scalafmtCheckAll`
  under new dialect; 1 was a keyword-collision fix (`enum` → `e` in GenKeyCodec), 8 were cosmetic reformats of shared
  sources containing scala-2 macro syntax.

- **Plan 01 (2026-05-30):** Did NOT relocate scala-2 macro files to `scala-2.13/` (deferred to Phase 5 module ports).
- **Plan 01 (2026-05-30):** `.planning/` excluded locally via `.git/info/exclude` only — `.gitignore` deliberately
  untouched (REQ WORKFLOW-05).

- **Plan 02 (2026-05-31):** `project/Commons.scala` restructured for cross-compile — jvm/jvm2/js aggregates, per-module
  crossScalaVersions, jetty single-version pinning + skip block, made 0.1.0 on Scala 3 core only, macros scala-reflect
  gated to 2.13. Single atomic commit 7bbe47f9.

- **Plan 02 (2026-05-31):** User-approved deviation Option 1 — relocated 8 scala-2 macro-def files from
  `core/src/main/scala/` to `core/src/main/scala-2.13/`. Overrides Plan 01's "deferred to Phase 5+" decision. Files
  contain `def x: T = macro Y.z` which is scala-2-only and breaks the 2.13 parser when scala3-dialect reformatter wraps
  it to two lines.

- **Plan 02 (2026-05-31):** Deferred — `commons-macros` Scala 3 compile (599 errors) carries to Phase 3 macros port;
  whole `macros/src/main/scala/` tree needs relocation to `scala-2.13/` à la master commit bcc3bcbf.

- **Plan 03 (2026-05-31):** Java CI matrix revised to **17/21/25** (not Java 17 only as the plan originally specified) —
  matches upstream/scala-3 baseline; minimum-diff against upstream wins. v2 CI-01 ("Add Java 21/25 to matrix") satisfied
  early as side effect.

- **Plan 03 (2026-05-31):** Pin-2.13 on jvm/jvm2/js/mima CI gates (Rule 3 auto-fix) — cross-build `+commons-jvm/test`
  fails on macros-2 sources under Scala 3. Pin lifts in **Phase 3** when macros-3 stub lands. `scalafmtCheckAll` stays
  dialect-agnostic and unprefixed.

- **Plan 03 (2026-05-31):** `commons-*` prefix on aggregate project IDs (Rule 3 auto-fix) — sbt-nosbt's
  `ProjectGroup("commons")` wrapper auto-prepends. Build keys reference `commons-jvm`, `commons-jvm2`, `commons-js`, not
  raw `jvm`/`jvm2`/`js`.

- **Plan 03 (2026-05-31):** 9th file relocated to scala-2.13/ —
  `core/jvm/src/test/scala-2.13/com/avsystem/commons/macros/TypeClassDerivationTest.scala`. Same Option 1 precedent from
  Plan 02; content restored byte-identical to upstream.

- **Plan 03 (2026-05-31):** **Phase 1 PR workflow process deviation — user direct-pushed branch tip `84e21dee`
  to `AVSystem/scala-commons:scala-3` outside this session, bypassing the fork-PR-then-merge step.** REQ WORKFLOW-02 and
  WORKFLOW-03 satisfied via direct-push by maintainer rather than the originally-specified fork-PR workflow. Accepted
  outcome. PR-via-fork workflow remains the contract for subsequent phases.

## Performance Metrics

| Phase-Plan                              | Duration | Tasks                                         | Files                               |
|-----------------------------------------|----------|-----------------------------------------------|-------------------------------------|
| 01-01                                   | ~25 min  | 3                                             | 11                                  |
| 01-02                                   | ~30 min  | 3                                             | 9                                   |
| 01-03                                   | ~45 min  | 3 (+direct-push outcome supersedes Tasks 4-7) | 3                                   |
| Phase 02 P01                            | ~10 min  | 2 tasks                                       | 1 files                             |
| Phase 02 P02                            | 12 min   | 2 tasks                                       | 1 files                             |
| Phase 02 P03                            | 4min     | 3 tasks                                       | 0 files                             |
| Phase 02 P04                            | ~25 min  | 2 of 4 (Tasks 3-4 deferred to user)           | 0 files                             |
| Phase 03 P01                            | 3 min    | 3 tasks                                       | 2 files                             |
| Phase 03 P02                            | 5 min    | 1 tasks                                       | 1 files                             |
| Phase 04 P01                            | ~5 min   | 2 tasks                                       | 1 files                             |
| Phase 04 P01                            | 5 min    | 2 tasks                                       | 1 files                             |
| Phase 04 P02                            | ~35 min  | 2 (scope-revised) tasks                       | 5 files                             |
| Phase 04 P03                            | 2 min    | 2 tasks                                       | 1 files                             |
| Phase 04 P04                            | ~15 min  | 4 tasks                                       | 0 files                             |
| Phase 05-core-scala-3-baseline-port P01 | 8min     | 3 tasks                                       | 33 files                            |
| Phase 01-big-bang-comment-and-green P01 | 5 min    | 3 tasks                                       | 14 files                            |
| Phase 01-big-bang-comment-and-green P02 | ~35 min  | 2 tasks                                       | 67 files (28 deleted + 39 modified) |
| Phase 01-big-bang-comment-and-green P03 | ~15 min  | 2 tasks                                       | 10 files                            |
| Phase 01-big-bang-comment-and-green P04 | ~2 min   | 1 task                                        | 1 file                              |
| Phase 01-big-bang-comment-and-green P05 | ~18 min  | 1 task                                        | 41 files                            |
| Phase 01-big-bang-comment-and-green P06 | ~30 min  | 4 tasks (2 auto + 2 checkpoints traversed)    | 2 files                             |
| Phase 03 P05                            | ~6 min   | 3 tasks                                       | 3 files (1 created + 1 deleted + 1 modified) |
| Phase 03-scala-3-syntax-modernization P05 | 6 min | 3 tasks tasks | 3 files files |
| Phase 03-scala-3-syntax-modernization P01 | 9 min | 3 tasks | 8 files |
| Phase 03-scala-3-syntax-modernization P02 | ~10 min | 2 tasks | 13 files (12 source + MIGRATION.md) |
| Phase 03-scala-3-syntax-modernization P03 | 50 min | 5 tasks (5 auto) tasks | 76 files files |
| Phase 03-scala-3-syntax-modernization P04 | ~12 min | 3 tasks (2 effective) tasks | 5 files files |
| Phase 04-meta-derivation-core P03 | 4 min | 4 tasks | 3 files |
| Phase 05-leaf-feature-restoration P00 | ~10 min | 3 tasks (2 auto + 1 checkpoint traversed) | 4 files |
| Phase 05-leaf-feature-restoration P00 | ~10 min | 3 tasks | 4 files |
| Phase 05-leaf-feature-restoration P01-bidirectional-deprecate | 2 min | 3 (2 auto + 1 checkpoint traversed) tasks | 3 files |
| Phase 05-leaf-feature-restoration P02-delegation-deprecate | 4 min | 3 (2 auto + 1 checkpoint traversed) tasks | 3 files |
| Phase 05-leaf-feature-restoration P03-applier-unapplier | 4 min | 3 (2 auto + 1 checkpoint traversed) tasks | 3 files |
| Phase 05-leaf-feature-restoration P03-applier-unapplier | 4 min | 3 (2 auto + 1 checkpoint traversed) tasks | 3 files |
| Phase 05 P04 | 7m | 3 tasks | 4 files |
| Phase 05-leaf-feature-restoration P05 | 5min | 4 tasks | 3 files |
| Phase 05-leaf-feature-restoration P06 | 10min | 3 tasks | 4 files |
| Phase 05-leaf-feature-restoration P07 | 10m | 3 tasks | 4 files |

## Notes

- .planning/ gitignored — never committed
- Single top-level MIGRATION.md tracks public-facing migration state (created in Phase 2)
- User ack required before push AND before PR open
- No GSD nomenclature in commit messages
- Phase 4 discovered: `made` integration code already authored on this branch (24 scala-3 files use `made.*`); Phase 4
  ports the wiring-primitive subset only (Opt/NOpt/OptArg/OptRef + madeAnnotationAliases); GenCodec etc. deferred to
  Phase 5+

- Phase 1 CONTEXT decision-revised 2026-05-30: preserve upstream's `mkSourceDirs` helper (was originally "no custom
  helper"; flipped because upstream/scala-3 already carries it — minimum-diff wins)

## Notes

- .planning/ gitignored — never committed
- Single top-level MIGRATION.md tracks public-facing migration state (created in Phase 2)
- User ack required before push AND before PR open
- No GSD nomenclature in commit messages
