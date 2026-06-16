# Roadmap: scala-commons → Scala 3 (big-bang)

**Created:** 2026-06-01 (post-pivot)
**Strategy:** Single big-bang PR — comment everything broken + green CI. Then PR-per-feature restoration.
**Target branch:** `AVSystem/scala-commons:scala-3`
**Source of truth:** fork `master` (reference scala-3 implementations to crib from during restoration)
**PR strategy:** stacked chain. Phase 1 = single big PR. Phase 2+ = one feature per PR, stacked.

## Core Value

Phase 1: codebase compiles on Scala 3 with everything broken commented out. Green CI. Cut-over done.
Phase 2+: each PR un-comments one feature area + ships new Scala 3 implementation. Iterative restoration.

## Phases

### Phase 1: Big bang — comment broken, green CI

**Single PR.** Cut from `upstream/scala-3 @ 1561d8dc`.

**In scope (everything in this single PR):**
- Build infra pivot: `scalaVersion := scala3Version`, drop `crossScalaVersions`, migrate scalac options, archive `scala-2.13/` source dirs, simplify `.scalafmt.conf` (single scala3 dialect).
- CI: single Scala 3 axis × Java 17/21/25 (3 shards).
- Comment out per file every block that doesn't compile on Scala 3:
  - `macros/` whitebox impls
  - `core/` derivation, GenCodec, RPC, anything using scala-2 macros / `c.universe`
  - `hocon`, `mongo`, `cbor` — comment broken
  - `jetty`, `analyzer`, `spring`, RPC modules — disable (`Compile/skip := true`) lub comment in place
  - test sources — per-file commenting
- Every commented section tagged `// TODO[scala3-port]: <feature>` for grep-based backlog.
- Fresh `MIGRATION.md` at root: every TODO listed, grouped by module, with rough S/M/L effort.
- CI green: `sbt compile` + `sbt Test/compile` exits 0.

**Success Criteria:**
1. `sbt 'show version'` exits 0
2. `sbt compile` exits 0 (every enabled module compiles)
3. `sbt Test/compile` exits 0
4. `sbt scalafmtCheckAll` exits 0
5. CI green on AVSystem PR (3 shards: Scala 3 × Temurin 17/21/25)
6. `MIGRATION.md` lists every commented feature with module + effort estimate
7. PR draft, `[Scala 3]` prefix, milestone "Scala 3" (#1)

**Plans:** 6 plans

Plans:
- [ ] 01-01-build-infra-pivot-PLAN.md — Cut 01-big-bang branch; rewrite Commons.scala (scalaVersion 3.8.2, drop crossScalaVersions, migrate scalac options, drop jvm2); simplify .scalafmt.conf to single scala3 dialect; regenerate ci.yml (Scala 3 × Temurin 17/21/25).
- [ ] 01-02-macros-and-core-PLAN.md — Neuter commons-macros (empty Scala 3 stub via /* */ wrap) then iterate per-file commenting of commons-core until ++3 commons-core/compile is green.
- [ ] 01-03-other-jvm-modules-PLAN.md — Comment broken defs in hocon, cbor sub-package, mongo (JVM), benchmark until sbt commons-jvm/compile is green.
- [ ] 01-04-js-variants-PLAN.md — Comment broken JS sources (core-js, mongo-js, benchmark-js) until sbt commons-js/compile is green.
- [x] 01-05-tests-compile-PLAN.md — Per-file commenting of broken test classes across all modules until sbt Test/compile is green. (3 commits: `65f507bc`/`3ffab524`/`555d2bb5`; 38 files commented; COMPILE-02 gate satisfied)
- [ ] 01-06-migration-md-and-push-pr-PLAN.md — Author MIGRATION.md (5 locked sections + backlog from TODO grep); run full local 5-gate verify; push to fork; open draft PR at AVSystem/scala-commons (two human-verify gates).

### Phase 2: Leaf debug/source macros — parallel slices

**Goal:** Restore independent leaf macros (no internal deps) as a fan-out of small parallel PRs off `01-big-bang`. Each slice ships as its own draft PR — reviewer cognitive load low, parallel landing rate high.

**In scope (one PR each, parallel-shippable):**
- `SharedExtensions.show*` family — 10 debug macros (`showAst`, `showRawAst`, `showSymbol`, `showSymbolFullName`, `showType`, `showRawType`, `showTypeSymbol`, `showTypeSymbolFullName`, `sourceCode`, `withSourceCode`). Single PR — tightly coupled file.
- `annotation/positioned.here` — source-position annotation helper.
- `misc/SourceInfo.here` — implicit source position.
- `misc/Implicits.infer` / `infer(clue)` / `inferNonMacro` — implicit summon helpers.
- `misc/SimpleClassName.materialize` — class name string.
- `misc/Sam.apply` / `SamCompanion.apply` / `isValidSam` — SAM type companion (single PR — coupled).

**Out of scope:** anything depending on these (`TypeString` uses `SimpleClassName` — defer to Phase 3). `meta/` derivation core (Phase 3 foundation). `AnnotationOf`, `ApplierUnapplier` (Phase 3, depend on metadata machinery).

**Pattern per PR:**
1. Branch off `01-big-bang` (or its merged base on `scala-3` once #860 lands).
2. Replace `???` stub with Scala 3 `inline` + `scala.quoted` implementation.
3. Restore matching test class if commented (per-file uncomment from Phase 1's `/* */` wraps).
4. Update MIGRATION.md backlog (remove restored entries; bump TODO count).
5. CI green.
6. Push + draft PR + milestone "Scala 3".

**Success Criteria:**
1. Each restored macro: original Scala 2 signature preserved, Scala 3 impl works.
2. Restored tests pass.
3. `MIGRATION.md` backlog reflects restoration (entries removed).
4. Each PR draft, `[Scala 3]` prefix, milestone 1.
5. No regressions in main compile/Test/compile gates.

**Plans:** 5 parallel-shippable slices (one PR each).

Plans:
- [ ] 02-01-debug-reify-PLAN.md — Restore SharedExtensions show*/sourceCode/withSourceCode (10 macros) via Scala 3 quotes + smoke test + MIGRATION trim + draft PR.
- [ ] 02-02-source-positions-PLAN.md — Restore positioned.here + SourceInfo.here via Position.ofMacroExpansion + smoke tests + MIGRATION trim + draft PR.
- [ ] 02-03-implicit-lookup-PLAN.md — Restore Implicits.infer / infer(clue) / inferNonMacro via Expr.summon + positive/negative tests + MIGRATION §3 narrowing entry + draft PR.
- [ ] 02-04-class-name-PLAN.md — Restore SimpleClassName.materialize via TypeRepr.of[T].dealias.typeSymbol.name + smoke test + MIGRATION trim + draft PR.
- [ ] 02-05-drop-sam-PLAN.md — Delete deprecated Sam.scala + SamCompanion.scala (per don't-port-deprecated rule) + MIGRATION §1 (Will Not Migrate) entry + §6 trim + draft PR.

### Phase 3: Scala 3 syntax modernization — 4 sequential PRs + 1 standalone parallel PR

**Goal:** Sweep Scala 2 idioms out of the codebase in favor of native Scala 3 syntax. Land as **four narrow-scope PRs in order**, each doing ONE rewrite kind. Translate from halotukozak's fork master (which has these transformations already merged) — NOT via `sbt-scala3-migrate` plugin. Each PR branches off `upstream/scala-3` tip — NOT stacked; merge order enforced via PR body metadata.

**Four slices (sequential, single-purpose):**
1. **3.1 `implicit class` → `extension`** — value-class extensions become extension blocks (+ `given Conversion` for HKT receivers).
2. **3.2 HKT wildcards `_` → `?`** — applied-position `F[_]` → `F[?]`; kind-decls preserved.
3. **3.3 `implicit def/val` → `given`** — implicit definitions → given declarations; `(implicit X)` → `(using X)`; 2 borderline preservations (OptArg.argToOptArg, SerializationMacros.fun2GenRef).
4. **3.4 `@inline def` → `inline def`** — Scala 2 optimizer hint → Scala 3 true compile-time inlining (Opt family, SharedExtensions, jiop). Whitelist: CborInput / JsonStringInput / RPCFramework preserved.

**In scope:** Pure syntax rewrites; no semantic changes.

**Out of scope:**
- Feature ports (Phase 4+)
- Optional braces / significant-indentation (deferred — separate decision)
- `@nowarn` removal (no warnings to suppress on Phase 1's stub baseline)
- Test-source un-wrapping (happens during feature-area restoration phases)

**Method:**
1. For each slice, identify the relevant fork-master commit (see CONTEXT.md `<canonical_refs>` + RESEARCH.md).
2. Per-file `git show origin/master:<path>` copy into our single-source tree, reconcile imports/stubs.
3. Apply edits, run `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` per slice.
4. Compare resulting file with `git show origin/master:<path>` for 1:1 shape match.
5. Open each PR off `upstream/scala-3` tip; merge sequential slices in order 3.1 → 3.2 → 3.3 → 3.4 via PR body metadata. Slice 3.5 (standalone parallel) can be opened + merged any time independently.

**Success Criteria per slice:**
- 3.1: `git grep 'implicit class' core/src/main/scala mongo/` → 0 hits.
- 3.2: `git grep -E '\[_(, _)*\]' core/src/main/scala mongo/` → 0 hits in APPLIED positions (kind-decls preserved).
- 3.3: `git grep -E '^\s*(inline\s+)?implicit (def|val)' core/src/main/scala mongo/ hocon/` → exactly 2 documented exceptions.
- 3.4: `git grep '@inline' core/src/main/scala/` → 0 hits outside whitelist (CborInput, JsonStringInput, RPCFramework).
- 3.5: `git ls-files .../Implicits.scala | wc -l` → 0; `git ls-files .../ImplicitNotFound.scala | wc -l` → 1; `git grep -nE '\bImplicits\.' -- '*.scala'` → 0 hits. (Standalone parallel PR — no slice dependency.)
- All slices: `compile + Test/compile + scalafmtCheckAll` green; no new `@nowarn`/`-Wconf`.
- Each PR draft, `[Scala 3]` prefix, milestone 1, body metadata block (slice / merge-order / depends-on / base-branch).

**Plans:** 5 plans (4 sequential slices + 1 standalone parallel slice 3.5). None stacked — each branches off `upstream/scala-3` tip. Slice 3.5 (Implicits deletion) is parallel-independent: touches files no other slice touches; can land any time.

Plans (4 sequential + 1 standalone parallel):
- [ ] 03-01-implicit-class-to-extension-PLAN.md — Sweep `implicit class XOps[A](...) extends AnyVal` → `extension` (or `given Conversion` for HKT receivers); 5 files (1 core + 4 mongo); MIGRATION.md §3 update; draft PR. (Sequential: merge before 3.2.)
- [ ] 03-02-hkt-wildcards-PLAN.md — Sweep applied-position `[_]`/`[_, _]` → `[?]`/`[?, ?]` across ~15 core + ~30 mongo + any hocon; kind-decls preserved; MIGRATION.md §3 update; draft PR. (Sequential: depends on 3.1.)
- [ ] 03-03-implicit-to-given-PLAN.md — Sweep `implicit def/val/object` → `given`, `(implicit X)` → `(using X)`; preserve OptArg.argToOptArg + SerializationMacros.fun2GenRef verbatim; BsonGenCodecs anonymous-given + @deprecated shims; MIGRATION.md §3 update; draft PR. (Sequential: depends on 3.2. **Note revision 2026-06-01:** Implicits object deletion moved OUT to slice 3.5.)
- [ ] 03-04-at-inline-to-inline-PLAN.md — Sweep `@inline def` → `inline def` (Opt family + SharedExtensions + jiop + concurrent); whitelist CborInput / JsonStringInput / RPCFramework verbatim; MIGRATION.md §3 update; draft PR. (Sequential: depends on 3.3.)
- [ ] 03-05-delete-implicits-object-PLAN.md — Delete `com.avsystem.commons.misc.Implicits` object outright (covered by `summon[T]`); extract `ImplicitNotFound` sealed trait to its own file; MIGRATION.md §1 update; draft PR. (**Standalone parallel** — no file overlap with 3.1/3.2/3.3/3.4; can land any time.)

### Phase 4: meta/ derivation core — foundation for serialization + RPC

**Goal:** Port `core/src/main/scala/com/avsystem/commons/meta/` derivation infrastructure from Phase-1 `???` stubs to working Scala 3 impl. Translate from halotukozak fork master (`origin/master:core/src/main/scala-3/com/avsystem/commons/meta/`). Phase 4 ports SCAFFOLDING (inline + macro-quote plumbing) — real reflection bodies deferred to Phase 6 per fork's own staging.

**In scope:** `MacroInstances`, `AdtMetadataCompanion` / `BoundedAdtMetadataCompanion`, `MetadataCompanion` / `BoundedMetadataCompanion`, `metaAnnotations`, plus 2 missing-from-our-tree files (`AllowDerivation`, `MetaMacros`).

**Out of scope:** `GenCodec.materialize` (Phase 6), `MongoEntityCompanion` macros (Phase 9), RPC framework (Phase 7).

**Method:** Crib from fork file-by-file, 5-slice stacked PRs (tight internal coupling — sequential merge):
- 4.1 Foundation (`AllowDerivation`, `OptionLike`, `metadata`, `Fallback`)
- 4.2 `MacroInstances` (inline given + named tuple)
- 4.3 `MetaMacros` + `MetadataCompanion`
- 4.4 `AdtMetadataCompanion` + `Bounded*`
- 4.5 `metaAnnotations.value`

**Plans:** 5 plans (one per slice).

Plans (5 sequential stacked slices):
- [ ] 04-01-foundation-PLAN.md — Port AllowDerivation (new) + Fallback + OptionLike (reconcile, keep BaseOptionLike shim + add made.Default bridge) + metadata.scala (strip @name("dupa") debug artifact); Wave-0 named-tuple probe; MIGRATION.md §3; draft PR base=upstream/scala-3.
- [ ] 04-02-macro-instances-PLAN.md — Port MacroInstances (inline given + named-tuple materialization, Instances <: AnyNamedTuple bound); MIGRATION.md §3 shape shift; draft PR stacked on 04-01-foundation.
- [ ] 04-03-meta-macros-PLAN.md — Port MetaMacros (new file — 7 traits + 3 ??? splice bodies per fork) + MetadataCompanion (polymorphic context-function givens); MIGRATION.md §1 deferral + §3; draft PR stacked on 04-02.
- [ ] 04-04-adt-metadata-companion-PLAN.md — Port AdtMetadataCompanion + BoundedAdtMetadataCompanion (M[X] <: TypedMetadata[X] bound tightening); un-wrap AdtMetadataTest with pending markers; MIGRATION.md §3 + §4 bincompat; draft PR stacked on 04-03.
- [ ] 04-05-meta-annotations-PLAN.md — Swap object infer to extend InferMacros; un-wrap MacroInstancesTest; phase-wide acceptance gate; MIGRATION.md §3 + backlog cleanup; draft PR stacked on 04-04; closes Phase 4 stack.

### Phase 5: Leaf feature restoration — parallel slices

**Goal:** Restore independent leaf macros that depend on Phase 4 `meta/` derivation core. Each feature ships as its own draft PR (parallel-shippable — no internal deps). Translate from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<file>`.

**In scope (one PR per feature):**
- `TypeString.materialize` + `JavaClassName.materialize` (single PR — coupled in TypeString.scala)
- `AnnotationOf` family — `AnnotationOf`, `OptAnnotationOf`, `AnnotationsOf`, `HasAnnotation`, `SelfAnnotation`, `SelfOptAnnotation`, `SelfAnnotations` (single PR — coupled file)
- `ApplierUnapplier.materialize`
- `Delegation.materialize`
- `SealedUtils` (`evidence`, related macros)
- `ValueEnum` companion macros
- `Bidirectional` — **review:** fork has it; consider deprecate-over-restore per [[feedback_deprecate_over_restore]]

**Out of scope:**
- `Sam.scala` / `SamCompanion.scala` — already deleted in slice 2.5 per don't-port-deprecated
- `GenCodec.materialize` — Phase 6
- `MongoEntityCompanion` macros — Phase 9

**Method:** Each PR off `upstream/scala-3` tip (parallel-safe — no file overlap between leaves). Crib from fork; replace `???` Phase-1 stub with Scala 3 macro impl using `meta/MacroInstances` + `MetaMacros` ports from Phase 4.

**Dependencies:** Phase 4 `meta/` derivation must be available (either merged or stacked on `04-05-meta-annotations` tip).

**Plans:** 8 plans — 1 foundation slice (5.0 MiscMacros bundle) + 7 leaves. Per RESEARCH recommendation: slice 5.0 ports the centralised `MiscMacros.scala` (~310 LOC) which 2 leaves (AnnotationOf family, Delegation) depend on; remaining 5 leaves are independent of 5.0.

Plans:
- [ ] 05-00-miscmacros-foundation-PLAN.md — Port `MiscMacros.scala` foundation (~310 LOC) verbatim from fork; centralised impl bundle for AnnotationOf family + Delegation Macros traits. (Wave 0; required by 5.2 + 5.5.)
- [ ] 05-01-bidirectional-deprecate-PLAN.md — Port Bidirectional as `@deprecated` object with `scala.compiletime.error` body (17 LOC verbatim); no test (fork dropped). (Wave 1; independent. Requirement: BIDIRECTIONAL-01.)
- [ ] 05-02-delegation-stub-PLAN.md — Port Delegation verbatim; companion extends `DelegationMacros` trait from MiscMacros (body remains `???` matching fork staging); DelegationTest stays `ignore`d. (Wave 1; depends on 5.0. Requirement: DELEGATION-01.)
- [ ] 05-03-applier-unapplier-PLAN.md — Port ApplierUnapplier via `Mirror.ProductOf`-based `given derived` (no quoted impl); un-wrap ApplierUnapplierTest. (Wave 1; independent. Requirement: APPLIERUNAPPLIER-01.)
- [ ] 05-04-typestring-javaclassname-PLAN.md — Port TypeString + JavaClassName (coupled in single fork file); companion-local `materializeImpl` + top-level `derivedImpl`; un-wrap SharedExtensionsTest TypeString cases. (Wave 1; independent. Requirements: TYPESTRING-01, JAVACLASSNAME-01.)
- [ ] 05-05-annotation-of-family-PLAN.md — Port AnnotationOf family (7 leaves coupled in one file); companions extend Macros traits from MiscMacros; HasAnnotation reshape to `opaque type ... <: RefiningAnnotation` (bincompat break, documented); un-wrap AnnotationOfTest. (Wave 1; depends on 5.0. Requirement: ANNOTOF-01.)
- [ ] 05-06-sealed-utils-PLAN.md — Port SealedUtils via pure inline (`compiletime.{summonAll, summonFrom, erasedValue}` + `Mirror.SumOf` + `scala.ValueOf`); remove `caseObjectsFor` (zero callers); un-wrap SealedEnumTest + NamedEnumTest. (Wave 1; independent. Requirement: SEALEDUTILS-01.)
- [ ] 05-07-value-enum-PLAN.md — Port ValueEnum verbatim; top-level `valNameImpl` via `Symbol.spliceOwner.owner` walk; preserve init-order machinery (Pitfall 8); un-wrap ValueEnumTest. (Wave 1; independent. Requirement: VALUEENUM-01.)

### Phase 6+: Feature restoration — backlog driven

After Phase 5 lands, restoration phases proceed with leaves available:

- **Phase 5 archived above.**
- **Phase 6:** GenCodec / GenKeyCodec / GenObjectCodec — serialization core.
- **Phase 7:** RPC framework (AsRawReal, RPCFramework).
- **Phase 8:** cbor codec.
- **Phase 9:** mongo (depends on serialization).
- **Phase 10:** hocon.
- **Phase 11:** JS variants re-enable.
- **Phase N:** `analyzer` module — re-enable as Scala 3 compiler plugin or formally drop.
- Test classes re-enabled inline with their feature PRs (per-file uncomment).

Exact tiers refined as each phase lands — backlog dependency graph drives priorities.

## Cross-Cutting Requirements

- WORKFLOW-01: Branch off prior phase branch (stacked).
- WORKFLOW-02: PR base = prior phase branch (or `scala-3` for Phase 1).
- WORKFLOW-03: User ack before push + before PR open.
- WORKFLOW-04: No GSD nomenclature in commits/PR.
- WORKFLOW-05: `.planning/` never committed.
- PR-01: `[Scala 3]` title prefix.
- PR-02: Milestone "Scala 3" (#1).
- PR-03: Draft on open.
- QUALITY-01: No new `@nowarn` / `-Wconf` (memory rule).
- QUALITY-02: `// format: off` around macro defs OK (memory rule — applies if any scala-2 macro syntax sneaks through).
- BACKLOG-01: Every commented block tagged `// TODO[scala3-port]: <feature>`. Restoration PRs reference these tags.
- BACKLOG-02: MIGRATION.md backlog stays in sync (entries removed as features restored).

## Notes

- Old cross-build phases archived: `.planning/phases/_archive_cross_build_strategy/`, `.planning/_archive_cross_build_ROADMAP.md`, `.planning/_archive_cross_build_REQUIREMENTS.md`.
- Closed PRs: #856, #859. Merged-but-not-on-scala-3: #857, #858 (stay merged into prior stack branches, harmless).
- Upstream `scala-3` remains at `1561d8dc` — fresh start base.
- `made` library: bumped to 0.1.1 during pre-pivot work — Phase 1 inherits.
- Pivot decision: `[[project_scala3_only_pivot]]` memory.
