# Phase 2: MIGRATION.md skeleton + deprecation seed - Research

**Researched:** 2026-05-30
**Domain:** Repo-root migration tracking doc (Markdown, single file)
**Confidence:** HIGH

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

**Document location and format**
- File: `MIGRATION.md` at repo root (matches DOC-01).
- Format: GitHub-flavored Markdown, single file. No HTML.
- Tone: terse, technical, no GSD nomenclature. Written for upstream maintainers, not internal narrative.
- Sections (in this order): Overview, Per-module status, 2.13-only modules, Deprecation log, Per-PR update contract.

**Per-module status table**
- One row per module: macros, made, core, hocon, mongo, mongo-js, core-js, benchmark3, jetty, analyzer, spring, RPC, cbor.
- Columns: `Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes`.
- Status vocabulary (single token, lowercase): `cross`, `stub`, `2.13-only`, `pending`, `wip`.
- MiMa / Tasty-MiMa columns: `green` / `red` / `n/a` / `pending`.
- Initial state for Phase 2 PR: every row is `pending` except modules listed `2.13-only` and Phase 1's substrate facts.

**2.13-only modules section**
- Lists: `jetty`, `analyzer`, `spring`, RPC modules.
- One-paragraph rationale per module (Spring/Jetty servlet churn, analyzer = scala-2 compiler internals, RPC depends on macro stack).
- Cross-reference the `jvm2` aggregate exclusion in `build.sbt` from Phase 1.

**Deprecation log seeding**
- Source command (documented verbatim in MIGRATION.md): `git grep -n '@deprecated' master -- '*.scala'`.
- Output as a fenced code block grouped by module path prefix.
- Each entry: `path:line — symbol — message snippet` (truncate at ~80 chars).
- Two-pass tagging: `[skip-port]` for hits with stdlib replacements (per `feedback_dont_port_deprecated.md`), `[port]` otherwise.
- The log is a SEED — Phase 2 does not resolve any entries.

**Per-PR update contract**
- Section titled "How to update" near top of MIGRATION.md.
- Rules (numbered, ≤7 lines):
  1. Every PR that ports a module flips that module's row in the same PR (DOC-02).
  2. New deprecation discoveries append to the Deprecation log in the same PR.
  3. MiMa/Tasty-MiMa column changes only when CI matrix proves it.
  4. No GSD or internal tooling vocabulary in commit messages or doc body.
  5. `.planning/` never appears in PR diffs.

**Tone, header style, writing rules**
- `##` top-level, `###` subsections. No emoji, no banners.
- GFM tables, left-aligned, single-line cells.
- Impersonal voice ("Module X cross-builds" not "We've ported X").
- Status flips are facts — avoid "✓ Done"; use `cross` / `green`.

### Claude's Discretion

- Exact wording of rationale paragraphs in "2.13-only modules".
- Column ordering inside the Notes free-text.
- Whether to split the deprecation log by module heading vs single flat block with anchored markers — choose whichever renders cleaner given the actual grep output.

### Deferred Ideas (OUT OF SCOPE)

- Auto-generation of MIGRATION.md from build metadata (future tooling phase).
- Linking each deprecated symbol to its replacement (annotated table) — needs Phase 11 cbor work.
- README hero badge "scala-3 cross-build status" — cosmetic.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| DOC-01 | Single top-level `MIGRATION.md` at repo root maintained across all PRs | Section structure + status vocabulary defined; file is net-new (verified: no existing `MIGRATION.md` at repo root). |
| DOC-03 | "Deprecated" section seeded from `@deprecated` scan in fork master | Grep executed: 152 hits across `core` (123) and `mongo` (29) on master. Concrete groupings + sample lines below. |
| DOC-04 | "2.13-only modules" section formalized (jetty, analyzer, spring, RPC) | Build state: `jetty` is the only module currently aggregated under `jvm2`; `analyzer`/`spring`/RPC are commented-out on both fork master and upstream/scala-3. Documented as 2.13-only with deferred reactivation. |
| DOC-02 (codified) | Per-PR update contract | Captured as "How to update" rules; phase 2 does not enforce, just codifies. |
</phase_requirements>

## Summary

Phase 2 is documentation-only: write `MIGRATION.md` at repo root and open one PR onto `upstream/scala-3`. The doc has five sections: Overview, How to update (per-PR contract), Per-module status table (13 rows), 2.13-only modules (4 rationale paragraphs), Deprecation log (seeded from a `@deprecated` grep against fork `master`).

The grep against fork master returns **152 hits across 2 modules** (`core`: 123 lines, `mongo`: 29 lines). All hits live under `core/src/main/scala-2.13`, `core/src/main/scala-3`, or `mongo/jvm/src/main/scala`. There are zero hits in `macros`, `hocon`, `cbor`, `jetty`, `benchmark3`, `core-js`, or `mongo-js`. This makes grouping trivial: a single flat block with two module headings (`### core/` and `### mongo/`) renders cleanly. No need to invent anchored markers.

**Primary recommendation:** Write MIGRATION.md as five `##` sections in this order: Overview → How to update → Per-module status → 2.13-only modules → Deprecation log. Capture the grep output as a fenced code block inside Deprecation log, with two `###` subheadings (core, mongo). Tag each line `[port]` or `[skip-port]` based on whether the deprecation message references a stdlib replacement.

## Standard Stack

This is a Markdown-only phase. No libraries to install. Tooling involved:

| Tool | Version | Purpose | Why Standard |
|------|---------|---------|--------------|
| GitHub-flavored Markdown | n/a | Document format | Renders natively on GitHub PR view |
| `git grep` | system | Seed deprecation log | Already used in build; deterministic output |

**Installation:** None.

**Version verification:** Not applicable (no package dependencies).

## Architecture Patterns

### Document layout (final order)

```
# Scala 3 Migration Status

<one-paragraph overview: what this doc is, who it's for, target branch>

## How to update

<5 numbered rules, ≤7 lines total>

## Per-module status

<GFM table, 13 rows>

## 2.13-only modules

### jetty
<rationale paragraph>

### analyzer
<rationale paragraph>

### spring
<rationale paragraph>

### RPC
<rationale paragraph>

## Deprecation log

<source command in fenced block>
<truncation rule sentence>

### core/
<fenced block of path:line — symbol — message [port|skip-port]>

### mongo/
<fenced block of path:line — symbol — message [port|skip-port]>
```

### Per-module status table — initial row values (for the Phase 2 PR)

Derived from `build.sbt` on this branch (current cwd) — which already carries the `jvm2`/`jetty` split landed by Phase 1's substrate work. The active `lazy val` declarations in `build.sbt` are: `macros`, `core`, `core-js`, `mongo`, `mongo-js`, `hocon`, `jetty`, `benchmark3`, `benchmark2`, `benchmark-compilation3`, `benchmark-compilation2`. Commented-out: `analyzer`, `spring`, RPC modules, `comprof`, `benchmark-js`. No `made` or `cbor` is a separate sbt module — `made` is a Scala-3-only dependency of `core`; cbor lives under `core/.../serialization/cbor/`.

| Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes |
|--------|------|-----|------|------------|-------|
| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty (Phase 3). |
| made | n/a | pending | n/a | n/a | Scala-3-only dep, pinned 0.1.0 (Phase 4). |
| core | cross | pending | green | pending | Cross-compile target; tests pending (Phases 5–7). |
| hocon | cross | pending | green | pending | Pure-Scala; first downstream port (Phase 8). |
| mongo | cross | pending | green | pending | Uses `for3Use2_13` wrapper (Phase 9). |
| mongo-js | cross | pending | n/a | n/a | ScalaJS variant (Phase 10). |
| core-js | cross | pending | n/a | n/a | ScalaJS variant (Phase 10). |
| benchmark3 | cross | pending | n/a | n/a | JMH benchmarks (Phase 10). |
| jetty | 2.13-only | n/a | n/a | n/a | Servlet API churn; under `jvm2`. |
| analyzer | 2.13-only | n/a | n/a | n/a | Compiler plugin against scala-2 internals. |
| spring | 2.13-only | n/a | n/a | n/a | DEPRECATED upstream; not commented in build. |
| RPC | 2.13-only | n/a | n/a | n/a | Macro-stack-dependent; deferred. |
| cbor | cross | pending | pending | pending | Lives inside `core`; MiMa filters land in Phase 11. |

**Note on `cbor` as a row:** cbor is not a separate sbt module — it's a package under `core`. CONTEXT.md lists it as a status-table row because the migration tracks it as a logical work item (Phase 11 has dedicated MiMa filter work). Keep the row; in Notes column write "Sub-package of `core`; tracked separately for MiMa scope."

**Note on `RPC`:** No sbt module named "rpc" or "RPC" — RPC code lives under `core/src/main/scala-2.13/com/avsystem/commons/rpc/` (see grep hits in `AsRawReal.scala`). The "RPC modules" reference in CONTEXT.md/REQUIREMENTS.md is umbrella language for: the RPC framework code inside `core` + downstream RPC users (jetty in particular consumes it). Treat as a 2.13-only logical concern, not a build module.

### Anti-Patterns to Avoid

- **Inline HTML for tables.** GitHub renders GFM tables. No `<table>`.
- **Emoji status markers.** Use word tokens (`cross`, `pending`, `green`). The lockedconvention says no `✓`.
- **First-person plural.** No "we ported", "our migration". Impersonal.
- **GSD vocabulary.** No "phase", "task", "wave", "plan", "research" in MIGRATION.md body. Use upstream-facing terms: "PR", "this change", "module port", "deprecation sweep".
- **Listing every deprecation symbol-by-symbol inline.** The seed is a code block, not a table. Annotated symbol→replacement tables are deferred (per CONTEXT.md).

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Deprecation discovery | Hand-curated list | `git grep -n '@deprecated' master -- '*.scala'` | Single source of truth, reproducible, grep-friendly. |
| Module enumeration | Hand-typed list | Read `build.sbt` `lazy val` declarations directly | Build is canon; doc drifts otherwise. |
| Status table format | Custom DSL / YAML front-matter | Plain GFM table | Rendered by GitHub PR review without tooling. |

**Key insight:** Every piece of data in MIGRATION.md should be either (a) directly grep-able from build/source, or (b) prose that the maintainer writes once and never has to maintain in two places. Don't create derived state.

## Common Pitfalls

### Pitfall 1: Stale module list
**What goes wrong:** Listing modules from CONTEXT.md without cross-checking `build.sbt`. Phase 1's substrate may have renamed or split things.
**Why it happens:** CONTEXT.md was written before Phase 1 landed.
**How to avoid:** Before writing the table, run `grep -n 'lazy val' build.sbt` and reconcile. See "Module list verification" below.
**Warning signs:** Rows for `made` or `cbor` (not sbt modules); missing `benchmark2` / `benchmark-compilation*`.

**Reconciliation:** CONTEXT.md lists 13 logical rows. `build.sbt` has 11 active `lazy val`s plus 4 commented-out. The CONTEXT.md list mixes (a) sbt modules, (b) logical concerns inside `core` (cbor, RPC), and (c) external deps (`made`). This is intentional per CONTEXT.md — keep the 13-row list. Add a one-line legend below the table: "Rows below `core` may be logical groupings, not separate sbt modules — see Notes."

### Pitfall 2: Grep output drift
**What goes wrong:** Running `git grep '@deprecated' master` on a stale checkout produces different output than CI/reviewer sees.
**Why it happens:** `master` ref moves over time.
**How to avoid:** Capture the SHA of `master` HEAD at seed time and quote it in the doc: "Seeded from `master@<short-sha>` on <date>." Re-run is then deterministic.
**Warning signs:** Reviewer reports different line counts.

### Pitfall 3: Tagging `[port]` vs `[skip-port]` incorrectly
**What goes wrong:** A `@deprecated` with a stdlib replacement gets marked `[port]`, causing Phase 6 to add a shim.
**Why it happens:** Tagger doesn't read the deprecation message carefully.
**How to avoid:** Decision rule — if the deprecation message contains any of: "stdlib", "scala.", "native", "since Scala 2.13", "use [scala.foo]" → tag `[skip-port]`. Otherwise `[port]`. Examples:
- `"Use scala.valueOf[T] from the standard library"` → `[skip-port]`
- `"Scala 2.13 has native scala.math.Ordering.orElse"` → `[skip-port]`
- `"Use GenCodec.materialize instead"` → `[port]` (internal replacement)
- `"use SAM syntax (lambda)"` → `[skip-port]` (language feature, not stdlib API)

### Pitfall 4: Including `scala-3/` dir hits
**What goes wrong:** Grep returns 60+ hits inside `core/src/main/scala-3/...` (e.g., `compat.scala` has 43 deprecations on Scala-3-side shims).
**Why it happens:** Phase 2 PR is on `upstream/scala-3` which already carries Scala-3 source dirs.
**How to avoid:** Decide explicitly: scope the seed to `**/scala-2.13/**` only? Or include `scala-3/` too with a note? **Recommendation:** Include both, but use a `### Module/Scala-3-side` subheading for `scala-3/` paths because they document deprecations that already exist on the Scala 3 side (not items to port from 2.13). This matches DOC-03's wording ("seeded by scanning `@deprecated` annotations in fork master") — fork master includes `scala-3/` dirs.

## Code Examples

### Deprecation seed command (verbatim for the doc)

```bash
# Seed command — run from repo root on a checkout that has fork `master` fetched.
git grep -n '@deprecated' master -- '*.scala'
```

### Per-module group (sample — first 10 lines of actual output)

```
core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala:840 — Ordering.orElse — "Scala 2.13 has native scala.math.Ordering.orElse implementation" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala:848 — Ordering.orElseBy — "Scala 2.13 has native scala.math.Ordering.orElseBy implementation" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/Sam.scala:4 — Sam — "use SAM syntax (lambda)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/SamCompanion.scala:6 — SamCompanion — "use SAM syntax (lambda)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/ValueOf.scala:10 — ValueOf — "Use scala.ValueOf[T] from the standard library" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/ValueOf.scala:20 — ValueOf.value — "Use scala.valueOf[T] from the standard library" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/ValueOf.scala:23 — ValueOf.apply — "Use scala.ValueOf[T] from the standard library" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/rpc/AsRawReal.scala:17 — AsRawReal — "use SAM syntax (lambda)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/rpc/AsRawReal.scala:56 — AsRawReal — "use SAM syntax (lambda)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/GenCodec.scala:82 — ApplyUnapplyCodec — "Use GenCodec.materialize instead; ApplyUnapplyCodec is being removed." [port]
```

### Grouping summary (from actual grep)

| Module | Hit count |
|--------|-----------|
| `core` | 123 |
| `mongo` | 29 |
| **total** | **152** |

Within `core`, the 123 hits split roughly: `core/src/main/scala-2.13/` (~26 hits), `core/src/main/scala-3/` (~97 hits — dominated by `compat.scala`'s 43, `GenCodecCompat.scala`'s 33, `GenCodecCreates.scala`'s 14). Within `mongo`, all 29 hits are under `mongo/jvm/src/main/scala/`.

### Per-PR update contract (verbatim block to land in MIGRATION.md)

```markdown
## How to update

1. When a PR ports a module, flip that module's row in the same PR.
2. When a PR discovers a new `@deprecated`, append it to the Deprecation log in the same PR.
3. MiMa / Tasty-MiMa columns change only after CI proves the new state green.
4. Commit messages and this document use upstream-facing terms only.
5. Internal planning artifacts never appear in PR diffs.
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Prior `MIGRATION.md` at SHA `52c2b122` | Net-new doc on `upstream/scala-3` | 2026-05-30 reset | Don't import old doc (per REQUIREMENTS.md "Out of Scope"). |
| Inline deprecation table | Code block seeded from grep | Phase 2 design | Reproducible; defer annotated table to Phase 11. |
| Per-module sub-docs | Single top-level file | DOC-01 | One file, one PR per module flip. |

**Deprecated/outdated:**
- Earlier `migration/NN-*` branches in fork — not used (REQUIREMENTS.md "Out of Scope").
- Importing prior MIGRATION.md content — explicitly forbidden.

## Open Questions

1. **Should the `master` SHA used for grep be pinned in MIGRATION.md?**
   - What we know: Grep output is deterministic given a fixed ref.
   - What's unclear: Whether the doc should record `master@<sha>` as a foot-note.
   - Recommendation: Yes — single line at top of Deprecation log: "Seeded from `<remote>/master@<short-sha>` on YYYY-MM-DD." Reviewer can reproduce.

2. **Include `scala-3/` directory hits in the seed?**
   - What we know: Fork master has 97 hits under `core/src/main/scala-3/` (mostly deprecation of Scala-3-side helpers like `compat.scala`).
   - What's unclear: DOC-03 says "scanning `@deprecated` annotations in fork master" — ambiguous whether scala-3/ dirs are in scope.
   - Recommendation: Include with a sub-heading. They document existing Scala-3-side deprecations that future maintainers need to see; excluding them would mean a follow-up phase has to re-discover them.

3. **`spring` module status — fork master vs upstream/scala-3.**
   - What we know: README marks `commons-spring` as DEPRECATED. Build.sbt on this branch does not declare `lazy val spring` (commented-out per Phase 1 decisions).
   - What's unclear: Whether to list as `2.13-only` (cross-build position) or call out separately as "deprecated upstream, no migration planned."
   - Recommendation: List as `2.13-only` in the status table, but the rationale paragraph in "2.13-only modules" explicitly notes "deprecated upstream; no port planned (see v2 SPRING-01)."

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | None (Markdown content checks via `grep`/file existence) |
| Config file | none — see Wave 0 |
| Quick run command | `bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (Wave 0 creates this) |
| Full suite command | same as quick run |

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|--------------|
| DOC-01 | `MIGRATION.md` exists at repo root | smoke | `test -f MIGRATION.md` | ❌ Wave 0 |
| DOC-01 | Has required top-level sections | smoke | `grep -E '^## (How to update\|Per-module status\|2.13-only modules\|Deprecation log)' MIGRATION.md \| wc -l` (expect ≥ 4) | ❌ Wave 0 |
| DOC-03 | Deprecation log section is non-empty | smoke | `awk '/^## Deprecation log/,/^## /' MIGRATION.md \| grep -c '@deprecated\|skip-port\|port'` (expect > 0) | ❌ Wave 0 |
| DOC-03 | Seed command is documented in the doc | smoke | `grep -F "git grep -n '@deprecated' master" MIGRATION.md` (expect 1 hit) | ❌ Wave 0 |
| DOC-04 | `2.13-only modules` section names all four | smoke | `awk '/^## 2.13-only modules/,/^## /' MIGRATION.md \| grep -Eo 'jetty\|analyzer\|spring\|RPC' \| sort -u \| wc -l` (expect 4) | ❌ Wave 0 |
| DOC-01 | Per-module status table has all 13 rows | smoke | `awk '/^## Per-module status/,/^## /' MIGRATION.md \| grep -Ec '^\| (macros\|made\|core\|hocon\|mongo\|mongo-js\|core-js\|benchmark3\|jetty\|analyzer\|spring\|RPC\|cbor) '` (expect 13) | ❌ Wave 0 |
| DOC-02 (codified) | "How to update" section enumerates 5 rules | smoke | `awk '/^## How to update/,/^## /' MIGRATION.md \| grep -cE '^[0-9]+\.'` (expect ≥ 5) | ❌ Wave 0 |
| WORKFLOW-04 | No GSD vocabulary leaks into MIGRATION.md | smoke | `grep -iE '\b(GSD\|wave\|phase [0-9]\|RESEARCH\.md\|PLAN\.md\|CONTEXT\.md)\b' MIGRATION.md` (expect zero hits) | ❌ Wave 0 |
| WORKFLOW-05 | `.planning/` not referenced in MIGRATION.md | smoke | `grep -F '.planning/' MIGRATION.md` (expect zero hits) | ❌ Wave 0 |
| (workflow) | scalafmt clean (no Scala source changes in PR) | smoke | `sbt scalafmtCheckAll` (sanity — should be a no-op for a docs PR) | ✅ |
| (workflow) | Repo still compiles (smoke) | smoke | `sbt '++2.13 jvm/compile'` | ✅ |

### Sampling Rate

- **Per task commit:** `bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` (runs all grep-based checks; sub-second).
- **Per wave merge:** same as per-task — there's no test suite for a docs phase. Add `sbt scalafmtCheckAll` as a sanity belt-and-suspenders.
- **Phase gate:** All grep checks green AND `sbt scalafmtCheckAll` green AND `sbt '++2.13 jvm/compile'` green (proves no accidental build edits).

### Wave 0 Gaps

- [ ] `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` — bash script consolidating all grep-based validation checks above. Exit non-zero on any failure. Idempotent.
- [ ] No framework install needed.
- [ ] No test files needed (Markdown content, not code).

## Sources

### Primary (HIGH confidence)
- `.planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md` — user-locked decisions; source of truth for scope.
- `.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md` — Phase 1 substrate facts (`jvm2`/`jetty` split, scala versions).
- `.planning/REQUIREMENTS.md` §DOC-01..04 — requirement text verbatim.
- `.planning/ROADMAP.md` Phase 2 — goal + success criteria 1–5.
- `build.sbt` (current branch HEAD on master, lines 213–480) — canonical module list, `jvm2` aggregate, commented-out modules.
- `git grep -n '@deprecated' master -- '*.scala'` (executed 2026-05-30) — 152 total hits; module split: core=123, mongo=29.
- `README.md` (repo root, current branch) — tone reference and module nomenclature.

### Secondary (MEDIUM confidence)
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_dont_port_deprecated.md` — informs `[port]` vs `[skip-port]` tagging rule.

### Tertiary (LOW confidence)
- None.

## Metadata

**Confidence breakdown:**
- Document format / structure: HIGH — Markdown, fully constrained by CONTEXT.md.
- Module list / status table seed: HIGH — verified against current `build.sbt`.
- Deprecation seed grep: HIGH — command run, output captured, counts verified.
- 2.13-only rationales: MEDIUM — narrative wording is Claude's discretion; substance is verified (analyzer = compiler plugin against scala-2 internals, jetty = servlet+RPC dependency, spring = upstream-deprecated, RPC = macro-stack-dependent).
- Pitfalls: HIGH — derived from concrete observations (e.g., 97 `scala-3/` hits is a real discovery, not speculation).

**Research date:** 2026-05-30
**Valid until:** 2026-06-30 (or until `master` HEAD moves significantly — re-run the grep at seed time).
