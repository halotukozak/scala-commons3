# Phase 5: Leaf feature restoration - Context

**Gathered:** 2026-06-02
**Status:** Ready for planning

<domain>
## Phase Boundary

Port leaf macros from Phase-1 `???` stubs to working Scala 3 impl. Each leaf is independent (no internal deps between them) — parallel-shippable as standalone PRs off `upstream/scala-3` tip. Builds on Phase 4 `meta/` derivation infrastructure.

**Seven leaves:**
1. `TypeString` (+ coupled `JavaClassName`) — 120 LOC
2. `AnnotationOf` family (AnnotationOf, OptAnnotationOf, AnnotationsOf, HasAnnotation, SelfAnnotation, SelfOptAnnotation, SelfAnnotations) — 116 LOC, single file
3. `ApplierUnapplier` — 42 LOC
4. `Bidirectional` — 17 LOC, **deprecated stub per fork**
5. `Delegation` — 21 LOC
6. `SealedUtils` — 185 LOC
7. `ValueEnum` — 173 LOC

**Out of scope:**
- `Sam.scala` / `SamCompanion.scala` — already deleted in slice 2.5
- `GenCodec.materialize` — Phase 6
- `MongoEntityCompanion` materialize — Phase 9
- `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` real bodies — Phase 6 (currently `'{ ??? }` stubs from Phase 4)

</domain>

<decisions>
## Implementation Decisions

### Slice strategy — 7 parallel-safe PRs (one per leaf)

Per [[feedback_parallel_migration]]: no file overlap between leaves, parallel-shippable. Each PR off `upstream/scala-3` tip — NOT stacked on Phase 4 because Phase 4 isn't merged yet, BUT leaves that depend on Phase 4 `meta/` derivation must include Phase 4 commits in their branch (cherry-pick or stack-on-04-05).

**Decision:** stack on `04-05-meta-annotations` tip (Phase 4 final slice) — pragmatic until Phase 4 merges. Document base in PR body.

| # | Slice | Fork file | LOC | Phase 4 dep | Notes |
|---|-------|-----------|-----|-------------|-------|
| 5.1 | Bidirectional (deprecated stub) | misc/Bidirectional.scala | 17 | No | Per fork: ported as deprecated `@deprecated` stub with `scala.compiletime.error` body. Smallest, simplest. |
| 5.2 | Delegation | misc/Delegation.scala | 21 | Yes (MetaMacros splice) | Small port |
| 5.3 | ApplierUnapplier | misc/ApplierUnapplier.scala | 42 | Yes | Companion macro |
| 5.4 | TypeString (+ JavaClassName) | misc/TypeString.scala | 120 | Yes | 2 materialize macros in 1 file |
| 5.5 | AnnotationOf family | misc/AnnotationOf.scala | 116 | Yes | 7 macros in 1 file |
| 5.6 | SealedUtils | misc/SealedUtils.scala | 185 | Yes | Sealed-trait evidence machinery |
| 5.7 | ValueEnum | misc/ValueEnum.scala | 173 | Yes | Companion + `values` derivation |

### Method — crib from halotukozak fork

Per [[feedback_crib_from_master]]: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<file>` → copy verbatim into our tree, reconcile any divergence.

Phase 4 `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` are `'{ ??? }` stubs — leaves that call them inherit the runtime `???` failure mode. This matches fork's own staging. Document per slice.

### Bidirectional — deprecate over restore

Fork already deprecated. Port verbatim per [[feedback_deprecate_over_restore]] memory: `@deprecated` object with `inline def apply` that calls `scala.compiletime.error(...)`. No real macro impl needed.

### Test un-wrapping policy — per slice

For each leaf, un-wrap matching test file if exists:
- `TypeString` → `TypeStringTest.scala` (verify exists, un-wrap)
- `AnnotationOf` → `AnnotationOfTest.scala`
- `ApplierUnapplier` → `ApplierUnapplierTest.scala`
- `Delegation` → `DelegationTest.scala`
- `SealedUtils` → `SealedUtilsTest.scala`
- `ValueEnum` → `ValueEnumTest.scala`
- `Bidirectional` → no test (deprecated stub)

Cases that exercise runtime macro execution (depend on Phase 6 real bodies) → `pending` or stay wrapped.

### Commit cadence — fork pattern

Per slice, multiple Conventional Commits (NO squash):
- `feat(scala-3,core): port <Feature>` body: `Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<file>.`
- `test(scala-3,core): un-wrap <Feature>Test` (separate commit)
- `docs(migration): record <Feature> port`

### PR conventions

- Title: `[Scala 3] port <Feature>` (or `[Scala 3] deprecate Bidirectional` for 5.1)
- `--draft` on open
- Milestone 1
- Body metadata block: Slice 5.X / Parallel — independent / Depends on: #<Phase 4 last PR if branch off it> / Base branch: <base>
- MIGRATION.md update per slice (remove resolved TODO[scala3-port] entries)

### Claude's Discretion

- Exact slice ordering (5.1-5.7 above is by LOC ascending — refine per-execution)
- Whether to merge slices into fewer PRs if fork-shape allows
- Test pending vs wrapped strategy per case
- `MetaMacros` stub-dependency disclosure verbiage in PR body

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Fork master reference files
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/AnnotationOf.scala`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ApplierUnapplier.scala`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Delegation.scala`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/SealedUtils.scala`
- `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/ValueEnum.scala`

### Fork commit history relevant
- `origin/master@31970ec7` — fix(scala-3): AnnotationOf / OptAnnotationOf / AnnotationsOf real impls
- `origin/master@24e801ec` — fix(scala-3): SelfAnnotation / SelfOptAnnotation / SelfAnnotations real impls
- `origin/master@7085bd8f` — test(scala-3): re-enable ApplierUnapplierTest (Mirror-based derivation)
- `origin/master@dcf60e5d` — test(scala-3): re-enable SharedExtensionsTest (sanity for TypeString)
- `origin/master@f5c0b17e` — chore(scala-3): deprecate Bidirectional, drop scala-3 port
- `origin/master@3ec8c125` — chore(scala-3): SimpleRawRef codec via derived + SealedUtils uses scala.ValueOf

### Phase 4 dependency
- `04-05-meta-annotations @ f04cec6f` (current Phase 4 tip — base branch for leaves)

### Project rules
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md`
  - [[feedback_crib_from_master]] — translate, don't rewrite
  - [[feedback_parallel_migration]] — leaves are parallel-safe
  - [[feedback_deprecate_over_restore]] — Bidirectional
  - [[feedback_pr_title_prefix]] — `[Scala 3]` prefix
  - [[feedback_pr_draft]] — `--draft` on open
  - [[feedback_pr_milestone]] — milestone 1
  - [[feedback_migration_md_contract]] — MIGRATION.md updated per PR

### Scala 3 docs
- https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html
- https://docs.scala-lang.org/scala3/reference/contextual/derivation.html (for ValueEnum's `values` derivation)

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- Phase 4 `meta/MacroInstances`, `meta/MetaMacros`, `meta/MetadataCompanion`, `meta/AdtMetadataCompanion` — base trait hierarchies leaves extend or compose
- `meta/AllowDerivation`, `meta/Fallback`, `meta/OptionLike`, `meta/metadata.scala` from slice 4.1
- `serialization/macroCodecs.scala` uses some leaf APIs (TypeString, SealedUtils) — verify compile preservation

### Established Patterns
- Source layout: single `core/src/main/scala/` root
- Scala 3.8.2 baseline supports all required features (verified Phase 4 Wave-0 probe)
- Conventional Commits with `scala-3,core` scope per fork cadence
- `@publicInBinary` for private members accessed from inline bodies

### Integration Points
- `GenCodec.materialize` (Phase 6) calls `meta/MacroInstances.materialize`
- `MongoEntityCompanion.materialize` (Phase 9) → same chain
- `RPCFramework` (Phase 7) → uses `meta/AdtMetadataCompanion`
- Test sources: most leaves' tests wrapped in Phase 1 big-bang; un-wrap per slice

</code_context>

<specifics>
## Specific Ideas

- Each leaf maps 1:1 to fork file (same path, single-source layout post-pivot)
- Phase 4 `'{ ??? }` stubs (MetaMacros.valueImpl etc.) mean some leaf macros may compile but throw NotImplementedError at runtime — fork accepts this same state, so do we
- ValueEnum + SealedUtils share Mirror-based derivation patterns from fork (Scala 3 `compiletime.summonInline` + erasedValue)
- Bidirectional is "DELETED but kept as deprecated stub" — matches Sam.scala slice 2.5 pattern

</specifics>

<deferred>
## Deferred Ideas

- Real macro bodies for `MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` — Phase 6 (gates real `materialize` impls for all leaves)
- `GenCodec.materialize` — Phase 6
- `MongoEntityCompanion` macros — Phase 9
- RPC framework — Phase 7
- Phase 4 PR merge — pending; until then leaves stack on `04-05-meta-annotations` tip
- `Sam.apply` was deleted in slice 2.5 — not Phase 5 scope
- `analyzer` module re-enable — Phase N

</deferred>

---

*Phase: 05-leaf-feature-restoration*
*Context gathered: 2026-06-02*
