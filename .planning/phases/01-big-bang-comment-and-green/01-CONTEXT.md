# Phase 1: Big bang — comment broken, green CI - Context

**Gathered:** 2026-06-01
**Status:** Ready for planning

<domain>
## Phase Boundary

Single PR cut from `upstream/scala-3 @ 1561d8dc`. Pivots build to Scala 3 only and comments out anything that doesn't compile. End: green CI on Scala 3 × Java 17/21/25 with feature-poor codebase. Restoration is Phase 2+.

</domain>

<decisions>
## Implementation Decisions

### Build infra
- `scalaVersion := scala3Version` (3.8.2). Drop `crossScalaVersions` everywhere.
- Migrate scalac options to Scala 3 syntax. Specific items: drop `-Xsource:3`, drop `-Wconf` blocks, audit each remaining flag against Scala 3 docs.
- `made` dependency stays at `0.1.1` (already bumped pre-pivot).
- Archive `scala-2.13/` source dirs: rename to `__scala-2.13__/` so they're visible in git history but excluded from build by default sourceDir resolution. Phase 1 may eventually delete them — but archive-by-rename first to preserve content for reference.
- `.scalafmt.conf`: single `runner.dialect = scala3`. No `fileOverride`.
- CI matrix: single Scala 3 axis × Temurin 17/21/25 (3 shards).
- Remove `commons-` ProjectGroup prefix? — KEEP. sbt-nosbt convention preserved from upstream.
- Drop `jvm2` aggregate (was for 2.13-only modules). All modules either in `jvm` aggregate or excluded entirely.

### Module fate
Per module decision matrix:

| Module | Action | Notes |
|--------|--------|-------|
| `macros` | Keep, mostly empty | Whitebox impls commented or moved to `__scala-2.13__/`. Empty Scala 3 stub. |
| `core` | Keep, large commenting | Most of derivation/GenCodec/RPC commented. Keep what compiles. |
| `core-js` | Keep, follows core | Same commenting as core. |
| `hocon` | Keep, comment broken | Likely most still compiles since hocon is mostly data. |
| `mongo` | Keep, comment broken | Driver wrapper code likely needs commenting. |
| `mongo-js` | Keep, follows mongo | |
| `cbor` | Keep, comment broken | Annotation aggregate stuff scala-2 macro-heavy. Comment heavily. |
| `benchmark` / `benchmark3` | Keep, comment broken | |
| `jetty` | Disable (`Compile/skip := true`) | Deprecated upstream. |
| `analyzer` | Disable | Scala 2 compiler plugin. Can't run on Scala 3 compiler. |
| `spring` | Disable | Already deprecated upstream. |
| RPC | Disable / comment | Macro-heavy. Restore in dedicated phase. |

### Commenting convention
- Per-file granularity. Comment specific defs/classes/objects — NOT whole files (preserve package decl + imports + working code).
- Block comment style: `/* ... */`. NOT line-by-line `//`.
- Tag immediately above commented block: `// TODO[scala3-port]: <brief feature description>`.
- Test sources: per-file commenting too — broken test classes individually, not whole `Test/` dirs.

### MIGRATION.md (fresh, root)
Sections per memory rule `[[feedback-migration-md-contract]]`:
1. **Will not migrate** — symbols/modules dropped entirely. Rationale per entry.
2. **Deprecated on Scala 3** — kept behind `@deprecated`. `since` + `replaceWith` pointers.
3. **Source-compat breaks** — every place downstream `import`/call site changes. Per-module grouping.
4. **Binary-compat breaks** — empty for Phase 1 (no baseline yet). Activated later.
5. **Disabled tests / modules** — what's not running + why + restore-PR pointer if known.

Phase 1 seeds all 5 sections; subsequent phases prune as features restore.

### TODO format for grep-based backlog
- Tag: `// TODO[scala3-port]: <feature description>`
- Convention: feature description ~50 chars, grep-friendly.
- Optional effort suffix: `// TODO[scala3-port]: GenCodec derivation (L)` — S/M/L.

### Imports / unused
- After commenting, expect `unused import` warnings — DO NOT add `@nowarn`. Either remove the import or keep it commented next to its block.

### Acceptance gates (locked)
- `sbt 'show version'` exits 0
- `sbt compile` exits 0 (across enabled modules)
- `sbt Test/compile` exits 0
- `sbt scalafmtCheckAll` exits 0
- Fork CI + AVSystem PR CI both green
- `MIGRATION.md` populated with all 5 sections

### PR conventions (memory rules)
- Title: `[Scala 3] <subject>` ≤70 chars. Suggested: `[Scala 3] Pivot to Scala 3 only — comment broken, green CI`
- Base: `AVSystem/scala-commons:scala-3` (head of upstream)
- Draft on open
- Milestone: `Scala 3` (#1)
- Maintainer merges manually
- User ack before push + before PR open

### Claude's Discretion
- Exact per-file commenting boundaries during execution.
- Whether to keep some scala-2.13/ files inline-commented vs. archived under `__scala-2.13__/`.
- Order of module attack (probably: macros → core → cbor → mongo → hocon → js variants → tests).
- Whether to wholesale `git rm` some modules (jetty/spring/analyzer) vs. `Compile/skip := true`. Default: skip + comment in place to keep git history visible for future reference; full rm only if user later prefers it.

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Project requirements
- `.planning/REQUIREMENTS.md` — Phase 1 requirement IDs: BUILD-01..05, COMMENT-01..05, COMPILE-01..03, DOC-01..02, CI-01..02.
- `.planning/ROADMAP.md` — Phase 1 section.
- `.planning/PROJECT.md` — Vision and constraints.

### Memory rules (all relevant)
- `[[project-scala3-only-pivot]]` — drives the strategy.
- `[[feedback-pr-title-prefix]]` — `[Scala 3]` mandatory.
- `[[feedback-pr-milestone]]` — milestone 1 assignment via `gh api PATCH`.
- `[[feedback-pr-draft]]` — `--draft` on open.
- `[[feedback-migration-md-contract]]` — 5-section MIGRATION.md contract.
- `[[feedback-dont-port-deprecated]]` — skip `@deprecated` stdlib-replaceable symbols.
- `[[feedback-fix-dont-suppress-warnings]]` — no `@nowarn` / `-Wconf`.
- `[[feedback-format-off-macro-defs-ok]]` — `// format: off` around scala-2 macro defs OK if any sneak through.

### External docs
- Scala 3 migration guide: https://docs.scala-lang.org/scala3/guides/migration/
- sbt cross-build (reference only — we're dropping it): https://www.scala-sbt.org/1.x/docs/Cross-Build.html
- `made` library 0.1.1 on Maven Central: `io.github.halotukozak:made_3:0.1.1`

</canonical_refs>

<code_context>
## Existing Code Insights

### Starting point
- `upstream/scala-3 @ 1561d8dc` — same as upstream master, no Scala 3 work landed. This is what Phase 1 cuts from.
- `project/Commons.scala` on upstream/scala-3: uses sbt-nosbt `ProjectGroup`, declares `commons-jvm`/`commons-jvm2`/`commons-js` aggregates, wires `crossScalaVersions`, `made` 0.1.0 conditional dep, `mkSourceDirs` helpers.
- `.scalafmt.conf`: dialect `Scala213Source3` (we invert to `scala3` and drop overrides).
- `.github/workflows/ci.yml`: generated by sbt-github-actions.

### Reference points (fork master, for restoration later)
- `master @ bcc3bcbf`: extensive Scala 3 work — 111 files in `core/src/main/scala-3/`, plus other modules. Phase 2+ restoration cribs from these.
- Memory `[[project_made_already_on_branch]]`: reminder that fork master has the implementation; restoration consults it.

### Risk
- scalac option migration: some flags may have silent no-ops on Scala 3. Audit each.
- `commons-macros/Compile/skip` may not be enough — if `dependsOn(macros)` exists on every other module, sbt still tries to compile macros. May need to actually empty the source tree.
- Test sources: per-file commenting is more labor than just `Test/compile := skip` — but plan calls for per-file. Stick with per-file.
- ScalaJS modules (`core-js`, `mongo-js`, `benchmark-js`): need ScalaJS plugin Scala 3 support. Check version.

### Integration points (files Phase 1 touches)
- `project/Commons.scala` (or `build.sbt` — split varies)
- `.scalafmt.conf`
- `.github/workflows/ci.yml` (regenerated)
- `MIGRATION.md` (new at root)
- `*/src/main/scala/**` and `*/src/test/scala/**` (per-file commenting)
- `*/src/main/scala-2.13/**` and test counterparts (archived to `__scala-2.13__/` or removed)

</code_context>

<specifics>
## Specific Ideas

- Modus operandi (per user): "comment everything broken → green CI → then PR-by-PR uncomment with new impl".
- TODO tag format borrowed from Linux kernel `FIXME:`/`TODO:` conventions — grep-friendly + machine-parseable.
- `__scala-2.13__/` archive convention: leading/trailing double-underscore signals "don't touch, reference only". Hides from default sbt sourceDir resolution because sbt only auto-recognizes specific names.

</specifics>

<deferred>
## Deferred Ideas

- LTS (3.3.x) vs current (3.8.x) decision — deferred per milestone description.
- Tasty-MiMa activation — deferred until first Scala 3 release ships.
- Formal retirement of `jetty`/`analyzer`/`spring` modules (git rm vs disable) — Phase 1 disables; formal removal in follow-up PR.
- Cherry-pick from fork master for restoration — Phase 2+ pattern, NOT Phase 1.
- Test re-enablement — Phase 1 only requires `Test/compile` green; tests pass per restoration PR.

</deferred>

---

*Phase: 01-big-bang-comment-and-green*
*Context gathered: 2026-06-01*
