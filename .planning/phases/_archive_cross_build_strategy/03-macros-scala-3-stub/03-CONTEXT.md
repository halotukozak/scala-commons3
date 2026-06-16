# Phase 3: macros Scala 3 stub - Context

**Gathered:** 2026-05-30
**Status:** Ready for planning
**Mode:** auto (user AFK — recommended defaults selected)

<domain>
## Phase Boundary

Make the `macros` sbt module cross-build under Scala 3 by adding an empty (or minimal placeholder) `macros/src/main/scala-3/` source tree. Goal: downstream modules can keep `dependsOn(macros)` on the Scala 3 side without dragging in the whitebox/scala-2 macro implementations. No actual macro re-implementation — that work lives in later phases (core/cbor/etc.). MIGRATION.md `macros` row flips to "stub on Scala 3".

</domain>

<decisions>
## Implementation Decisions

### Stub strategy
- **No content in `scala-3/`** at first commit. The directory exists but holds zero Scala files.
- Rely on sbt's source-set resolution (already wired in Phase 1 via `mkSourceDirs`) to produce an empty `macros_3.jar`. Empty jars are valid Maven artifacts and an established cross-build idiom.
- If sbt refuses to produce a jar when the source dir is empty (some plugins balk), fall back to a single placeholder file: `macros/src/main/scala-3/com/avsystem/commons/macros/package.scala` containing only a `package object macros` declaration — zero exported symbols. Avoid `package.scala` with comments only; sbt may skip it.

### Source-dir creation
- Create `macros/src/main/scala-3/` with a `.gitkeep` if and only if Scala/Java tooling treats empty dirs as missing (git itself does — so `.gitkeep` is needed for the dir to land in the PR diff).
- If we fall back to the placeholder package object, drop `.gitkeep` (the `.scala` file is enough).

### Build config touches
- **Zero changes to `project/Commons.scala`** assumed: Phase 1 already wired `macros` into the `jvm` aggregate with `crossScalaVersions`. Verify during planning; if a per-module override silently excluded `macros` from Scala 3, the planner adds the minimal opt-in.
- Do NOT alter `crossScalaVersions` semantics; do NOT add a `Compile/sources` override.

### Whitebox / Scala 2 macro safety
- The Scala 2.13 macro impls under `macros/src/main/scala-2.13/` are untouched. `scala-3/` does NOT mirror file paths from `scala-2.13/`.
- No `using` / `inline` / `quotes`-based reimplementation in this phase. That belongs to per-call-site ports in Phases 5–11.

### Downstream sanity
- Verify `dependsOn(macros)` resolves on Scala 3 for at least one downstream module that already has `scala-3/` sources (`core` qualifies — see `core/src/main/scala-3/com/avsystem/commons/misc/`).
- Smoke command: `sbt '++3 core/compile'` exits 0. NOT a full test — just compile.

### Documentation
- MIGRATION.md `macros` row: status column `stub` on Scala 3; Notes column: `Empty scala-3 dir; whitebox impls remain 2.13-only.`
- Update happens in the same PR per DOC-02.

### PR/workflow
- One-commit PR is fine if checks pass; else two commits (stub + MIGRATION.md update).
- Same human-ack-before-push / human-ack-before-PR gates as Phase 2. PR target: `AVSystem/scala-commons:scala-3`.

### Claude's Discretion
- Whether to ship `.gitkeep` or placeholder `package.scala`. Default: try `.gitkeep` first; switch to placeholder if `sbt '++3 macros/compile'` errors with "no sources" or empty-jar packaging fails.
- Exact wording of the MIGRATION.md note (≤80 chars).

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Project requirements
- `.planning/REQUIREMENTS.md` §MACROS-01 — the single requirement driving this phase.
- `.planning/ROADMAP.md` Phase 3 — goal + success criteria.

### Phase 1 substrate (build foundation)
- `.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md` — `mkSourceDirs` helper, `jvm` aggregate, `crossScalaVersions` rules.
- `project/Commons.scala` — actual sbt build (read current state to confirm `macros` module is in the `jvm` aggregate with Scala 3 cross-version enabled).

### Phase 2 docs
- `MIGRATION.md` (after Phase 2 lands) — `macros` row gets flipped here.

### Existing Scala 3 source convention
- `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala` (and siblings) — example of an existing `scala-3/` source tree on this branch.

No external specs / ADRs.

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- `mkSourceDirs` / `sourceDirsSettings` helpers in `project/Commons.scala` (Phase 1 outcome) — already drive `scala-3/` resolution; no new sbt code required.
- `core/src/main/scala-3/` — proves the source-layout pattern works on this branch.

### Established Patterns
- `scala/` (shared) + `scala-2.13/` (2.13-only) + `scala-3/` (3-only). Empty `scala-3/` is novel for the `macros` module specifically.
- `.gitkeep` convention: not currently used in the repo for empty directories. Adding one for `macros/src/main/scala-3/` is acceptable per Phase 1 CONTEXT (which only said `.gitkeep` not used for the *Phase 1* slice — silent on other phases).

### Integration Points
- `macros/src/main/scala-3/` (new directory, possibly with `.gitkeep`).
- `MIGRATION.md` `macros` row.
- (Possibly) `project/Commons.scala` — only if a per-module `Compile/sources` override needs to be relaxed; ideally zero diff.

</code_context>

<specifics>
## Specific Ideas

- Empty-jar pattern: cats-effect's `kernel` cross-build idiom — when a module has no Scala-3-specific code but participates in cross-build, the dir exists but is empty. Common across the Scala ecosystem.

</specifics>

<deferred>
## Deferred Ideas

- Actual Scala 3 reimplementation of whitebox macros (annotation-driven derivation, `MaterializedXxx`). Belongs to per-feature porting phases (5–11).
- Replacing `macros` module entirely with `made`-driven derivation. Out of scope; Phase 4 covers `made` integration.
- Renaming `scala-2.13/` to `scala-2/` for parity with other modules. Stylistic; out of scope here.

</deferred>

---

*Phase: 03-macros-scala-3-stub*
*Context gathered: 2026-05-30 via /gsd:discuss-phase --auto*
