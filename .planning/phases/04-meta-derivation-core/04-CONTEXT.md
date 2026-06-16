# Phase 4: meta/ derivation core - Context

**Gathered:** 2026-06-01
**Status:** Ready for planning

<domain>
## Phase Boundary

Port `core/src/main/scala/com/avsystem/commons/meta/` derivation infrastructure from Phase-1 `???` stubs to working Scala 3 impl. Foundation layer for Phase 6 (GenCodec derivation) and Phase 7 (RPC framework).

**In scope:**
- `MacroInstances` — companion-implicits materialization via Scala 3 inline + named tuples
- `AdtMetadataCompanion` / `BoundedAdtMetadataCompanion` — ADT metadata derivation base traits
- `MetadataCompanion` / `BoundedMetadataCompanion` — metadata derivation base traits
- `metaAnnotations.scala` — annotation metadata (`adtParamMetadata`, `multi`, etc. — fork has `value` macro stub)
- `AllowDerivation`, `Fallback`, `MetaMacros`, `metadata.scala` — supporting infra fork adds (not yet in our tree)
- `OptionLike` — already mostly working; reconcile with fork shape

**Out of scope:**
- `GenCodec.materialize` (Phase 6)
- `ApplyUnapplyCodec.materialize` (Phase 6)
- `MongoEntityCompanion.materialize` (Phase 9)
- `forSealedEnum` (Phase 6)
- `Implicits.infer` (deleted in slice 3.5)
- RPC scaffolding (Phase 7)

</domain>

<decisions>
## Implementation Decisions

### Translation method — crib from halotukozak fork

Use fork `origin/master` `core/src/main/scala-3/com/avsystem/commons/meta/` as canonical source per [[feedback_crib_from_master]]. Method: `git show origin/master:core/src/main/scala-3/<path>` → copy to `core/src/main/scala/<path>`, reconcile against any divergence.

Fork files to port (each may be its own PR slice):
1. `MacroInstances.scala` — `inline given materialize[Implicits, Instances <: AnyNamedTuple]` + `transparent inline def materializeInstances[T <: Tuple]` using `compiletime.erasedValue` + `summonInline`
2. `AllowDerivation.scala` — derivation-permission marker
3. `MetaMacros.scala` — helper macros (shared between AdtMetadataCompanion and MetadataCompanion macro variants)
4. `AdtMetadataCompanion.scala` — extends `AdtMetadataCompanionMacros[M]` + `MetadataCompanion[M]`
5. `MetadataCompanion.scala` — `lazyMetadata` macro impl
6. `metaAnnotations.scala` — `value` macro impl (fork has it)
7. `metadata.scala` — TypedMetadata + supporting types
8. `Fallback.scala` — fallback typeclass resolution
9. `OptionLike.scala` — reconcile against fork

### Slice strategy — ~3-5 small PRs

Avoid one big PR — split by file cluster:
- **Slice 4.1:** Foundation — `AllowDerivation`, `Fallback`, `OptionLike` (reconcile), `metadata.scala` (no macros, pure types)
- **Slice 4.2:** `MacroInstances` materialization (inline + named tuple derivation)
- **Slice 4.3:** `MetaMacros` + `MetadataCompanion.lazyMetadata` (real macro impl)
- **Slice 4.4:** `AdtMetadataCompanion` + `Bounded*` (depends on 4.2 + 4.3)
- **Slice 4.5:** `metaAnnotations.value` macro impl

Each off `upstream/scala-3` (or stacked when real file dependencies require — meta has tight coupling). Sequential merge order, parallel-unsafe.

### `made.*` library usage — accept

Project memory [[project_made_already_on_branch]]: fork uses `made.*` macros under the hood (Defaults, Mirror-style derivation). Phase 4 ports may bring in additional `made` integration. Cellar lookup before adding:
```
cellar get-external pl.halotukozak:made_3:<version> made.Default
```

### Macro implementation — use Scala 3 `inline given` + `compiletime.summonInline` (no `made` macro wrapping at this layer)

Fork `MacroInstances` is hand-written Scala 3 inline (`erasedValue` pattern). Port verbatim. Do NOT wrap in `made.*` — `made` is consumed at GenCodec/RPC layer, not here.

### Test coverage

`MacroInstances`, `AdtMetadataCompanion`, `MetadataCompanion` tests live in `core/jvm/src/test/scala/com/avsystem/commons/meta/`. Many wrapped (`/* */`) in Phase 1 big-bang. Un-wrap per-slice as ports come online.

### Borderline preservations

- `Implicits.infer` references — none remaining (slice 3.5 deleted).
- `@deprecated` shims for renamed type-class lookups — likely needed for `MetadataCompanion.materialize` → `materializeRecursively` rename if fork did one. Check fork.

### Claude's Discretion

- Exact slice boundaries (4.1–4.5 above are a starting point — refine per fork-commit granularity)
- Whether to stack PRs (vs each off `upstream/scala-3`) — meta has tight internal coupling, stacking acceptable
- Test re-enable batching (one per slice vs all at end)
- `MetaMacros` private helper visibility

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Fork master reference files (read via `git show origin/master:<file>`)
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MacroInstances.scala` — inline materialize + named-tuple derivation
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AdtMetadataCompanion.scala` — ADT metadata base trait
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetadataCompanion.scala` — metadata derivation base trait
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metaAnnotations.scala` — meta annotation hierarchy
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metadata.scala` — TypedMetadata + ADT type encoding
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AllowDerivation.scala` — derivation-permission marker
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/Fallback.scala` — fallback typeclass resolution
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/MetaMacros.scala` — shared macro helpers
- `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/OptionLike.scala` — Option-like typeclass

### Fork commit history relevant
- `origin/master@80f82c62` — feat(scala-3): adapt GenCodec derivation to refined Made output (anchors meta/codec integration)
- `origin/master@95e43ff8` — feat(scala-3): port GenRef / RawRef macros
- `origin/master@ea49b65a` — feat(scala-3): expand AnnotationAggregate in AnnotationOf macros
- `origin/master@31970ec7` — fix(scala-3): AnnotationOf / OptAnnotationOf / AnnotationsOf real impls

### Project rules
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md`
  - [[feedback_crib_from_master]] — translate, don't rewrite
  - [[feedback_small_scoped_prs]] — 3-5 PRs per phase, stack when dependent
  - [[feedback_parallel_migration]] — N/A here (tight coupling)
  - [[feedback_stub_over_comment]] — current state uses `???` stubs
  - [[project_made_already_on_branch]] — `made.*` ~24 files; phase may add more
  - [[feedback_pr_title_prefix]] — `[Scala 3]` prefix
  - [[feedback_pr_draft]] — `--draft` on open
  - [[feedback_pr_milestone]] — milestone 1
  - [[feedback_migration_md_contract]] — MIGRATION.md updated per PR
- `.planning/MIGRATION.md` — backlog entries for meta/* TODOs removed as features restored

### Scala 3 docs
- https://docs.scala-lang.org/scala3/reference/metaprogramming/inline.html
- https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html
- https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html
- https://docs.scala-lang.org/scala3/reference/contextual/derivation.html

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- Phase-1 stubs already exist at correct paths (`core/src/main/scala/com/avsystem/commons/meta/*.scala`) — replace bodies with fork impl
- `made.*` integration already live on branch (some files in core/jvm use `made.Done`, `made.Of`)
- Slice 3.3 `summon[T]` patterns established — meta derivation builds on these

### Established Patterns
- Source layout: single `core/src/main/scala/` root (no scala-3/ overlay in our tree)
- Scala 3.8.2 pinned in `project/Commons.scala` — supports `inline given`, `compiletime.erasedValue`, named tuples, `summonInline`
- `@publicInBinary` precedent from slice 3.4 — apply to private members referenced from inline bodies
- Conventional Commits with `scala-3,core` scope per fork cadence

### Integration Points
- `GenCodec.materialize` (Phase 6) → calls `MacroInstances.materialize`
- `MongoEntityCompanion.materialize` (Phase 9) → same
- `RPCFramework` (Phase 7) → uses `AdtMetadataCompanion`
- `metaAnnotations.value` macro → consumed by GenCodec for `@transientDefault`, `@flatten`, etc.
- Test sources: `core/jvm/src/test/scala/com/avsystem/commons/meta/{AdtMetadataTest,MacroInstancesTest,...}.scala` — most wrapped in Phase 1 big-bang, un-wrap during ports

</code_context>

<specifics>
## Specific Ideas

- Phase 4 is foundation — gate all later feature-restoration phases (5+)
- Mirror fork's `inline given materialize` + `compiletime.erasedValue` pattern verbatim — proven Scala 3 idiom
- `MetadataCompanion.lazyMetadata` is the hot path — fork's impl uses `quotes.reflect` macro; port carefully
- `AdtMetadataCompanion` vs `BoundedAdtMetadataCompanion` deliberately not sharing code per fork comment ("must copy" for binary compatibility) — preserve this pattern

</specifics>

<deferred>
## Deferred Ideas

- `GenCodec.materialize` impl — Phase 6 (depends on this phase)
- `ApplyUnapplyCodec.materialize` — Phase 6
- `MongoEntityCompanion` macro ports — Phase 9
- RPC AsRaw/AsReal materialization — Phase 7
- `forSealedEnum` macro — Phase 6 (uses meta but is GenCodec-specific)
- `made` library version bump — phase-independent (track via [[project_made_already_on_branch]])
- `analyzer` module re-enable — Phase N
- Cross-build re-introduction — never (per [[project_scala3_only_pivot]])

</deferred>

---

*Phase: 04-meta-derivation-core*
*Context gathered: 2026-06-01*
