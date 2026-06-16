# Requirements: scala-commons → Scala 3 (big-bang)

**Defined:** 2026-06-01 (post-pivot)
**Strategy:** Single Phase 1 = comment everything broken + green CI. Phase 2+ = restoration PRs (per feature, requirements defined per phase).

## Phase 1: Big bang comment-and-green

### Build infra
- [x] **BUILD-01**: `scalaVersion := scala3Version` in `project/Commons.scala`. No `crossScalaVersions`.
- [x] **BUILD-02**: scalac options migrated to Scala 3 syntax. `-Xsource:3` removed. `-Wconf` blocks removed. Unused/deprecated flags audited.
- [x] **BUILD-03**: `scala-2.13/` source dirs archived (renamed `__scala-2.13__/` excluded from build) OR removed entirely. `sourceDirsSettings` cleaned.
- [x] **BUILD-04**: `.scalafmt.conf` simplified — single `runner.dialect = scala3`. No fileOverride for 2.13.
- [x] **BUILD-05**: `.github/workflows/ci.yml` runs single Scala 3 axis × Temurin 17/21/25 (3 shards).

### Comment-out
- [x] **COMMENT-01**: Every block that doesn't compile on Scala 3 wrapped in `/* ... */` block comment.
- [x] **COMMENT-02**: Each commented block annotated with `// TODO[scala3-port]: <feature description>` immediately above.
- [x] **COMMENT-03**: Per-file granularity — comment specific defs/classes, not whole files (preserves package decl + imports + working code).
- [x] **COMMENT-04**: Test sources commented per-file (broken test classes, not whole test dirs).
- [x] **COMMENT-05**: Modules that can't compile at all (jetty/analyzer/spring/RPC) → `Compile/skip := true` + commented contents OR full removal from aggregate.

### Compile gates
- [x] **COMPILE-01**: `sbt compile` exits 0 across all enabled modules.
- [x] **COMPILE-02**: `sbt Test/compile` exits 0.
- [x] **COMPILE-03**: `sbt scalafmtCheckAll` exits 0.

### MIGRATION.md
- [x] **DOC-01**: `MIGRATION.md` at repo root. Sections: Overview, Module status (each module: green/disabled/partial), Backlog grouped by module, Disabled tests, Disabled modules, Restoration order suggestion.
- [x] **DOC-02**: Backlog populated from `git grep -n 'TODO\[scala3-port\]'`. Each entry: location, brief description, S/M/L effort.

### CI
- [x] **CI-01**: Fork CI green on push.
- [x] **CI-02**: AVSystem PR CI green.

## Phase 2+: Feature restoration

Requirements defined per restoration PR. Pattern:
- One feature area un-commented + Scala 3 implementation + tests (if any commented).
- MIGRATION.md backlog entry removed.
- CI green.

## Cross-Cutting

- [x] **WORKFLOW-01**: Branch off prior phase branch (stacked).
- [x] **WORKFLOW-02**: PR base = prior phase branch (or `scala-3` for Phase 1).
- [x] **WORKFLOW-03**: User ack before push + before PR open.
- [x] **WORKFLOW-04**: No GSD nomenclature in commits/PR text.
- [x] **WORKFLOW-05**: `.planning/` never committed.
- [x] **PR-01**: `[Scala 3]` title prefix.
- [x] **PR-02**: Milestone "Scala 3" (#1).
- [x] **PR-03**: Draft on open.
- [x] **QUALITY-01**: No new `@nowarn` / `-Wconf`.
- [x] **QUALITY-02**: `// format: off` around macro defs OK per memory rule.

## Traceability

| Req | Phase | Status |
|-----|-------|--------|
| BUILD-01..05 | 1 | Pending |
| COMMENT-01..05 | 1 | Pending |
| COMPILE-01..03 | 1 | Pending |
| DOC-01..02 | 1 | Pending |
| CI-01..02 | 1 | Pending |
| WORKFLOW-01..05, PR-01..03, QUALITY-01..02 | cross-cutting | Pending |
