# Phase 2: MIGRATION.md skeleton + deprecation seed - Context

**Gathered:** 2026-05-30
**Status:** Ready for planning
**Mode:** auto (user AFK — recommended defaults selected)

<domain>
## Phase Boundary

Land a repo-root `MIGRATION.md` on upstream/scala-3 that tracks the cross-compile migration: per-module status table, deprecation log seeded from a `@deprecated` scan of fork master, and a "2.13-only modules" section. Documentation-only PR — no source/build changes (those landed in Phase 1).

</domain>

<decisions>
## Implementation Decisions

### Document location and format
- File: `MIGRATION.md` at repo root (matches DOC-01).
- Format: GitHub-flavored Markdown, single file. No HTML.
- Tone: terse, technical, no GSD nomenclature. Written for upstream maintainers, not internal narrative.
- Sections (in this order): Overview, Per-module status, 2.13-only modules, Deprecation log, Per-PR update contract.

### Per-module status table
- One row per module: macros, made, core, hocon, mongo, mongo-js, core-js, benchmark3, jetty, analyzer, spring, RPC, cbor.
- Columns: `Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes`.
- Status vocabulary (single token, lowercase):
  - `cross` — cross-builds green on both 2.13 and 3.x
  - `stub` — Scala 3 build wired with empty/placeholder sources
  - `2.13-only` — explicitly excluded from Scala 3 aggregate
  - `pending` — not yet started
  - `wip` — port in progress on a feature branch
- MiMa / Tasty-MiMa columns: `green` / `red` / `n/a` / `pending`.
- Notes column: free text, single line per module.
- Initial state for Phase 2 PR: every row is `pending` except modules listed `2.13-only` and Phase 1's substrate facts.

### 2.13-only modules section
- Lists: `jetty`, `analyzer`, `spring`, RPC modules (whatever ships under that umbrella in upstream).
- For each, a one-paragraph rationale: why Scala 3 cross-compile is deferred (Spring/Jetty servlet API churn, analyzer = scala-2 internals, RPC dependent on macro stack still pending).
- Explicitly cross-references the build exclusion in `project/Commons.scala` (the `jvm2` aggregate from Phase 1).

### Deprecation log seeding
- Source command (verbatim, documented in MIGRATION.md): `git grep -n '@deprecated' master -- '*.scala'`.
- Output captured as a fenced code block grouped by module path prefix (e.g., `core/`, `cbor/`).
- Each entry kept as a single line: `path:line — symbol — message snippet` (truncate message at ~80 chars).
- Two-pass policy: hits with stdlib replacements (per memory `feedback_dont_port_deprecated.md`) are tagged `[skip-port]` inline; everything else `[port]` so future phases can grep.
- The log is a SEED — Phase 2 does not resolve any of these; later phases tick them off.

### Per-PR update contract
- One short section at the top of MIGRATION.md titled "How to update".
- Rules (numbered, ≤7 lines total):
  1. Every PR that ports a module flips that module's row in the same PR (DOC-02).
  2. New deprecation discoveries append to the Deprecation log in the same PR.
  3. MiMa/Tasty-MiMa column changes only when CI matrix proves it.
  4. No GSD or internal tooling vocabulary in commit messages or doc body.
  5. `.planning/` never appears in PR diffs.

### Tone, header style, and writing rules
- Headings: `##` for top-level sections, `###` for subsections. No emoji. No banners.
- Tables: GFM, left-aligned, single-line cells.
- No "we"/"our" — write impersonal: "Module X cross-builds on Scala 3" not "We've ported X".
- Status flips are FACTS, not announcements. Avoid "✓ Done" — use `cross` / `green`.

### Claude's Discretion
- Exact wording of rationale paragraphs in "2.13-only modules".
- Column ordering inside the Notes free-text.
- Whether to split the deprecation log by module heading vs single flat block with anchored module markers — pick whichever renders cleaner after the actual grep output is in hand.

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Project requirements
- `.planning/REQUIREMENTS.md` §DOC-01, §DOC-02, §DOC-03, §DOC-04 — the four DOC requirements driving this phase.
- `.planning/ROADMAP.md` Phase 2 — goal + success criteria.
- `.planning/PROJECT.md` — non-negotiables (no GSD nomenclature in upstream PRs, `.planning/` gitignored).

### Phase 1 substrate (already merged on scala-3)
- `.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md` — module list, jvm/jvm2 aggregate split, scala versions.
- `project/Commons.scala` (after Phase 1) — source of truth for which modules are 2.13-only.

### Deprecation policy
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_dont_port_deprecated.md` — skip `@deprecated` symbols with stdlib replacements when porting.

No external specs / ADRs — requirements fully captured in the decisions above and the docs listed.

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala` (and siblings) — shows the `scala-3/` source dir convention already established.
- Phase 1's `jvm2` aggregate enumerates the 2.13-only modules — copy the list rather than re-deriving.

### Established Patterns
- Repo currently has no top-level migration doc; MIGRATION.md is net-new.
- Upstream commit style: lowercase imperative subject, no scope decoration beyond `build:`, `style:`, `fix:`.

### Integration Points
- `MIGRATION.md` at repo root.
- README link from existing README to MIGRATION.md is OPTIONAL — defer to user; not in Phase 2 scope unless trivial.

</code_context>

<specifics>
## Specific Ideas

- Deprecation log style: similar to a `CHANGELOG.md` "Deprecated" section — flat, grep-friendly, line per entry.
- Per-module status table style: like rustc's tracking issues for unstable features (column-per-axis, single-token statuses).

</specifics>

<deferred>
## Deferred Ideas

- Auto-generation of MIGRATION.md from build metadata (e.g., a `sbt migrationStatus` task). Possible future tooling phase, out of scope here.
- Linking each deprecated symbol to its replacement (annotated table). Would require Phase 11 cbor MiMa cleanup work to land first.
- README hero badge "scala-3 cross-build status". Out of scope; cosmetic.

</deferred>

---

*Phase: 02-migration-md-skeleton-deprecation-seed*
*Context gathered: 2026-05-30 via /gsd:discuss-phase --auto*
