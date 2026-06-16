# Phase 2: Leaf debug/source macros — Context

**Gathered:** 2026-06-01
**Status:** Ready for planning

<domain>
## Phase Boundary

Restore independent leaf macros (no inter-macro dependencies, no derivation machinery) as a fan-out of small parallel PRs off `01-big-bang`. Each PR ports one logical concern from `???` stub to a working Scala 3 `inline` + `scala.quoted` implementation.

**In scope:**
- `SharedExtensions.show*` family + `sourceCode` / `withSourceCode` (debug + source reification)
- `annotation/positioned.here` + `misc/SourceInfo.here` (source-position helpers)
- `misc/Implicits.infer` / `infer(clue)` / `inferNonMacro`
- `misc/SimpleClassName.materialize`
- `misc/Sam.apply` / `SamCompanion.apply` / `isValidSam`

**Out of scope (deferred to later phases):**
- Anything depending on the above (e.g. `TypeString` uses `SimpleClassName` — Phase 3+)
- `meta/` derivation core (Phase 3 foundation)
- `AnnotationOf`, `ApplierUnapplier`, `Bidirectional`, `Delegation`, `SealedUtils`, `ValueEnum`
- GenCodec / RPC / cbor / mongo / hocon

</domain>

<decisions>
## Implementation Decisions

### PR granularity — 1 PR per logical concern
- ~5 PRs total. Each PR groups tightly-related macros (same file or same purpose).
- Proposed slicing (refined during planning):
  1. **debug-reify** — `SharedExtensions.show*` (10 macros) + `sourceCode` / `withSourceCode`
  2. **source-positions** — `positioned.here` + `SourceInfo.here`
  3. **implicit-lookup** — `Implicits.infer` / `infer(clue)` / `inferNonMacro`
  4. **class-name** — `SimpleClassName.materialize`
  5. **drop-sam** — deletion of `Sam` / `SamCompanion` (no Scala 3 impl per [[feedback-dont-port-deprecated]])
- Each PR self-contained, no cross-PR dependency on this phase's other slices.

### Macro style — mix per macro (Claude's discretion)
- Default: `inline def` + `${ impl[T] }` calling `def impl[T: Type](using Quotes): Expr[R]`
- Use `transparent inline` when the macro needs to refine the return type for callers (e.g. `SimpleClassName.materialize`, `SourceInfo.here` if they expose a singleton type or refined value).
- Use regular `inline` for the `show*` debug family — they all return `String`, no refinement needed.
- Claude picks per-macro; no need to ask user for each.

### Test policy — match original + smoke if missing
- For each restored macro: un-comment the matching test file from Phase 1's `/* */` wrap (per-file).
- If no original test existed, add a minimal smoke test: at least one compile-time invocation + result assertion.
- Restored test file should pass on `01-big-bang` after restoration.

### Branch / PR strategy — off `01-big-bang`, immediate parallel
- Each PR branches directly off `01-big-bang` (Phase 1 branch, still open as PR #860).
- Land in parallel — no stacking required since slices are independent (per [[feedback-parallel-migration]]).
- If #860 gets reshuffled during review, each Phase 2 PR rebases off the new tip.
- Each PR: `[Scala 3]` title prefix, milestone "Scala 3" (#1), draft on open (per existing memory rules).

### MIGRATION.md update policy
- Each PR removes the restored entries from the `## Backlog` section.
- Update `Total tags: N` count at top of backlog section.
- Do NOT add a "completed" marker — clean removal keeps the backlog scoped to remaining work.

### Claude's Discretion
- Exact `Quotes` API call patterns (`Type.of[T]`, `TypeRepr.of[T]`, `Expr.summon`, etc.)
- Error-reporting style (`report.error` vs `report.errorAndAbort` vs returning `Expr[Nothing]`)
- Whether to expose an `inline def` wrapper plus a non-inline helper or fold everything into the macro body
- How to handle the `???` stub removal — direct replacement vs incremental
- Edge-case handling for the `show*` macros (e.g. how to render a `Symbol` that's a constructor vs a method)
- Cribbing from upstream `master` (Scala 2 macro impl) is encouraged — translate idiomatically, don't transliterate

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Phase 1 closure
- `.planning/phases/01-big-bang-comment-and-green/01-06-SUMMARY.md` — Phase 1 final summary, PR #860 link
- `MIGRATION.md` §6 (Backlog) — 155-row TODO inventory; Phase 2 entries are the leaf macros listed above

### Scala 3 macro reference
- Scala 3 docs `scala.quoted` API — fetch via Context7 (`mcp__context7__*`) for Quotes/Expr/Type signatures during planning
- Upstream `master` branch of `AVSystem/scala-commons` — Scala 2 macro impls to crib from (e.g. `macros/src/main/scala/com/avsystem/commons/macros/misc/MiscMacros.scala` had show* impls; same path on master)

### Project rules
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md` — all PR rules (title prefix, milestone, draft), stub→impl conversion, parallel-PR strategy
- `~/.claude/CLAUDE.md` — `cellar` CLI for verifying Scala stdlib / 3rd-party API signatures

### Per-macro source locations
- `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:129-147` — show*/sourceCode/withSourceCode stubs
- `core/src/main/scala/com/avsystem/commons/annotation/positioned.scala:12` — positioned.here stub
- `core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala:28` — SourceInfo.here stub
- `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:5-9` — infer family stubs
- `core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala:8` — SimpleClassName.materialize stub
- `core/src/main/scala/com/avsystem/commons/misc/Sam.scala:9` + `core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala:11,19` — SAM family stubs

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- Each target file already has the `???` stub with original signature preserved (per Phase 1's stub-don't-comment rule). Restoration = swap body, no signature redesign.
- Phase 1 SUMMARYs (`01-02-SUMMARY.md` and onward) document widened return types where the original was macro-inferred — restoration may tighten these back.
- Matching test files exist in `core/src/test/scala/...` wrapped in `/* */`. Un-comment to restore.

### Established Patterns
- Stub convention: `// TODO[scala3-port]: <description> (S|M|L)` immediately above each broken def. Restoration removes both the TODO tag and the `= ???`.
- No `@nowarn` / `-Wconf` (project rule). Fix at source.
- `// format: off` permitted around scala-2 macro syntax — restored Scala 3 code shouldn't need it.

### Integration Points
- CI workflow: `build` job (compile + Test/compile) + `scalafmt` job (lint). Both must stay green per PR.
- `MIGRATION.md` backlog table: each PR removes restored rows.

</code_context>

<specifics>
## Specific Ideas

- "Restore matching test + smoke test if missing" — belt-and-suspenders coverage so Phase 2 doesn't leave silent regressions.
- "Mix `transparent inline` and regular `inline` per macro" — `transparent` only where call-site type refinement actually matters; otherwise the simpler form.
- Phase 2 = proof point for the small + parallel PR doctrine ([[feedback-small-scoped-prs]] + [[feedback-parallel-migration]]). If reviewer throughput on these ~5 PRs is healthy, scale the pattern up in Phase 3+.

</specifics>

<deferred>
## Deferred Ideas

- `TypeString` restoration — depends on `SimpleClassName` (Phase 2). Deferred to Phase 3.
- `meta/` derivation core (MacroInstances, AdtMetadataCompanion, MetadataCompanion, metaAnnotations) — foundation for GenCodec + RPC, sequential big-or-split-PR. Phase 3 candidate.
- `AnnotationOf` / `ApplierUnapplier` / `Bidirectional` / `Delegation` — moderate complexity macros, defer until Phase 3+ when meta/ is restored.
- `analyzer` module re-enable — separate sequencing decision (Scala 3 plugin rewrite is L).
- Restoring the test files that depend on `meta/` or `serialization/` stubs — they stay wrapped until their feature area is restored.

</deferred>

---

*Phase: 02-leaf-debug-source-macros*
*Context gathered: 2026-06-01*
