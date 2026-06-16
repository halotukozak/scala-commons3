# Phase 5: core — Scala 3 baseline port - Context

**Gathered:** 2026-06-01
**Status:** Ready for planning
**Mode:** auto (recommended defaults)

<domain>
## Phase Boundary

Land the `core` module's Scala 3 baseline so `++3.8.2 commons-core/compile` is GREEN for the first time. Two-pronged: (1) cherry-pick the ~111 Scala 3 source files from fork `master:core/src/main/scala-3/` (minus ones that depend on cbor/mongo/rpc which are deferred), (2) relocate the Scala-2-only macro-def + scala-2-shaped source files from shared `core/src/main/scala/` to `core/src/main/scala-2.13/` (the ~70-80 files from Plan 04-02's aborted Option B).

**In scope:**
- `core/src/main/scala-3/**/*.scala` — cherry-pick subset that compiles on Scala 3 standalone (no cbor/mongo/rpc deps inside core).
- `core/src/main/scala-2.13/**/*.scala` — relocate scala-2-only sources from shared `scala/`.
- `core/src/main/scala/**/*.scala` — leave only shared-byte-identical files (cross-version utilities).
- `MIGRATION.md` — `core` row Notes update.

**Out of scope:**
- Tests revival → Phase 7.
- `given/using/extension` cleanup sweep + serialization-impl port → Phase 6.
- `cbor`/`mongo`/`hocon`/`RPC` → Phase 8/9/11/12.

</domain>

<decisions>
## Implementation Decisions

### Cherry-pick from fork master
- Source of truth: `master @ bcc3bcbf` (or current `master` HEAD).
- 111 files exist on `master:core/src/main/scala-3/`. Port the subset whose imports resolve against:
  - Already-landed Phase 4 wiring primitives (`Opt`, `NOpt`, `OptArg`, `OptRef`, `madeAnnotationAliases`).
  - Standard library + `made` 0.1.0.
  - Files within `core/` itself (no cross-module deps to cbor/mongo/rpc).
- Iterative compile-fix loop: start with leaf files (no internal deps), add dependents, re-compile, repeat until `++3.8.2 commons-core/compile` exits 0.

### Scala-2 file relocation
- Files matching `git grep -nl '= macro ' core/src/main/scala/` → relocate to `scala-2.13/`.
- Files containing scala-2-only syntax (`def x[T]: T = macro Y.z[T]`, whitebox macros, etc.) → relocate.
- Files that compile on BOTH 2.13 and 3 (shared byte-identical) → leave in `scala/`.
- Use `git mv` to preserve history.

### Defer-list (DO NOT touch in Phase 5)
- `core/src/main/scala/com/avsystem/commons/serialization/cbor/**` → Phase 11.
- Any file requiring mongo/RPC types → Phase 9/12.
- Test files → Phase 7.

### Acceptance gate
- `++3.8.2 commons-core/compile` exits 0 (THE goal).
- `++2.13.18 commons-core/compile` exits 0 (regression guard).
- `++3.8.2 commons-macros/compile` exits 0 (Phase 3 protected).
- `sbt scalafmtCheckAll` exits 0.
- No `@nowarn`/`-Wconf` introduced (memory rule).
- `// format: off` around macro defs OK (memory rule).

### MIGRATION.md
- `core` row Notes: `Scala 3 compile-only baseline; tests + given/using sweep pending`.
- Status column: `wip` → `cross` (compile cross-builds; tests TBD).

### Cascading PR stack
- Branch from `04-made-integration @ c3e54b16` (cascading on PR #859).
- Push to AVSystem upstream.
- PR open as **draft** with `[Scala 3]` prefix, milestone "Scala 3" (#1), base `04-made-integration`.

### Claude's Discretion
- Subset of fork-master files to port — driven by compile-iteration. Start with leaf files (annotations, basic data types, jiop, concurrent), expand to derivation (GenCodec/GenKeyCodec/GenObjectCodec entry points), stop when compile is green. Defer remainder to Phase 6+.
- Stripping bodies that reference deferred types (e.g., cbor) inside ported files — keep file but trim cbor methods.

</decisions>

<canonical_refs>
## Canonical References

- `.planning/REQUIREMENTS.md` §CORE-01, §CORE-02
- `.planning/ROADMAP.md` Phase 5
- `.planning/phases/04-made-integration/04-CONTEXT.md` (Phase 4 substrate — what's already landed)
- `.planning/phases/04-made-integration/04-02-port-wiring-primitives-SUMMARY.md` (Plan 04-02 deferral notes)
- Fork master `core/src/main/scala-3/` (111 files — cherry-pick subset)
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md` (all PR + workflow rules)

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- 111 scala-3 files on fork master = cherry-pick library.
- Phase 4's 5 wiring-primitive files already on branch — base for derivation entries.
- Phase 1's `mkSourceDirs` helpers already route `scala-3/` source dirs.

### Risk
- Compile cascade: each cherry-picked file may need 1–5 more files (transitive imports). Iterative.
- Some fork-master scala-3 files reference cbor/mongo/RPC types — must trim those imports/methods OR skip the file.
- scalafmt under scala3 dialect may rewrap cherry-picked files; reformat as needed.

</code_context>

<deferred>
## Deferred Ideas

- Test revival (CodecTestData, etc.) → Phase 7.
- `given/using/extension` sweep + serialization-impl port → Phase 6.
- cbor/mongo/RPC derivation → Phase 9/11/12.
- Cleanup of `core/src/main/scala/` leftovers (any file still there post-Phase 5 that should move) → Phase 6.

</deferred>

---

*Phase: 05-core-scala-3-baseline-port*
*Context: 2026-06-01 auto*
