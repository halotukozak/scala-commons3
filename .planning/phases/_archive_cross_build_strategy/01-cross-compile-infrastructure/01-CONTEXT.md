# Phase 1: Cross-compile infrastructure - Context

**Gathered:** 2026-05-30
**Status:** Ready for planning

<domain>
## Phase Boundary

Land cross-compile foundation on `upstream/scala-3` as the first PR. Build infra only — no source ports. Files touched: `build.sbt`, `project/plugins.sbt`, `.scalafmt.conf`, `.github/workflows/ci.yml`. Slice is hand-authored on top of `upstream/scala-3` (currently == upstream master, no Scala 3 work landed), drawing from fork `master`'s state.

</domain>

<decisions>
## Implementation Decisions

### `made` library
- Pin `madeVersion := "0.1.0"` (published release on Sonatype Central as `io.github.halotukozak:made`).
- Master uses `0.1.1-SNAPSHOT` — do **not** carry SNAPSHOT into upstream PR.
- No SNAPSHOT resolver added.

### scalafmt dialects
- `runner.dialect = scala3` (default for whole project — Scala 3 sources).
- `fileOverride` for `**/src/{main,test}/scala-2.13/**` and `**/src/{main,test}/scala-2/**`: `runner.dialect = scala213source3`.
- Rationale: 2.13 sources are compiled with `-Xsource:3` so dialect must permit Scala 3 syntax constructs in 2.13 files. Master currently sets `scala213` (plain) — this phase upgrades to `scala213source3` per REQ INFRA-05.
- `allowSignificantIndentation`/`allowFewerBraces` stay enabled for the default `scala3` dialect; disabled in fileOverride for 2.13 dirs (already the master pattern).

### scalafmt version
- Accept upstream `scala-3` branch's scalafmt reformat baseline (3.11.1 per PROJECT.md). Phase 1 PR bases on whatever upstream `scala-3` ships and rebases conflicts toward upstream.
- Do not pin back to master's 3.10.4.

### MiMa plugins
- Bump `sbt-mima-plugin` to `1.1.5` in `project/plugins.sbt`.
- **Defer** `sbt-tasty-mima` — drift from REQ INFRA-04 acknowledged; tasty-mima added later (Phase 11 / v2 RELEASE-01).
- `tastyMiMaPreviousArtifacts` stays empty (no Scala 3 baseline exists yet).
- `previousCompatibleVersions` Set untouched (2.13 baseline only).

### CI workflow
- Keep `sbt-github-actions` autogen — do **not** hand-author `.github/workflows/ci.yml`.
- Configure sbt build keys (`githubWorkflowBuildPreamble`, `githubWorkflowBuild`, `githubWorkflowJavaVersions`, etc.) so the regenerated `ci.yml` runs the 5 gate commands per REQ INFRA-07: `+jvm/test`, `+jvm2/test`, `+js/test`, `++2.13 mimaReportBinaryIssues`, `scalafmtCheckAll`.
- Java 17 + 21 + 25 (matches upstream baseline; decision revised 2026-05-31 during push-gate review).
- Generated `ci.yml` committed in the PR (per sbt-github-actions contract).

### Makefile
- **Not included** in Phase 1 PR. Skipped per user choice.
- Per-PR workflow commands documented in MIGRATION.md when it lands (Phase 2) or run ad-hoc.

### Aggregate split
- `jvm` aggregate: cross-built modules (everything except 2.13-only ones).
- `jvm2` aggregate: 2.13-only stranded modules. Phase 1 adds only `jetty` to `jvm2` (matches master state).
- `analyzer`, `spring`, RPC: stay commented-out in build.sbt as on master; formalize and (if needed) reactivate under `jvm2` in Phase 12.
- Rationale for split: default `sbt test` runs `jvm/test` — 2.13-only modules in a separate aggregate prevent `++3.8.2 jvm/test` from failing on jetty.

### Cross-build configuration
- `crossScalaVersions := Seq(scala3Version, scala2Version)` on every cross-built module.
- `scala2Version = "2.13.18"`, `scala3Version = "3.8.2"` (match master).
- LTS (3.3.x) vs current (3.8.x) decision deferred to first `_3` release (per PROJECT.md constraints).
- `scalaVersion := scala3Version` at root (default version, override per command with `++`).

### Source layout
- Preserve upstream's existing `mkSourceDirs` / `sourceDirsSettings` helpers in `project/Commons.scala` verbatim. They are functionally equivalent to the `CrossVersion.partialVersion` idiom and already satisfy INFRA-03. Replacing them would inflate the diff with no behavioral change — minimum-diff against upstream wins.
- Directories recognized: `scala/` (shared), `scala-2.13/` (2.13 only), `scala-3/` (Scala 3 only).
- Test counterpart already wired identically by the upstream helper (`Test/unmanagedSourceDirectories`).
- **Decision revision 2026-05-30:** original "No custom `mkSourceDirs` helper" rule was written against fork master (which had inlined the build). Reverted because upstream/scala-3 already carries the helper.

### Source files in this PR
- **Zero source files.** Pure infra slice. Scala 3 modules compile to empty jars.
- No `scala-3/` directories created — sbt resolves them lazily when files exist.
- `.gitkeep` placeholders not used.
- Module ports (`macros` stub, `made` integration, `core` baseline) start in Phase 3+.

### Acceptance on Scala 3 side
- `++3.8.2 jvm/compile` green: every cross-built module compiles (empty jar OK on Scala 3).
- `++3.8.2 jvm/test` green: test compile may be no-op (no Scala 3 test sources yet).
- `+jvm/test` (cross both) green on 2.13; Scala 3 side compiles whatever exists.

### Aggregate exclusions
- `jetty/analyzer/spring/RPC` excluded from `jvm` aggregate (already master state — preserve).
- Phase 1 PR keeps `analyzer`/`spring`/RPC commented-out as on master.

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Project-level (mandatory)
- `.planning/PROJECT.md` — Cherry-pick model, target branch, user ack gate, conflict expectation (scalafmt 3.11.1 reformat on upstream/scala-3).
- `.planning/REQUIREMENTS.md` §"Build Infrastructure" — INFRA-01..09 verbatim.
- `.planning/ROADMAP.md` §"Phase 1" — Success criteria 1–6.

### Cherry-pick source files (fork master state)
- `build.sbt` (current branch = fork master) — Reference for `aggregateProjectSettings`, `jvm` / `jvm2` definitions, per-module `crossScalaVersions`, `made` dep wiring.
- `project/plugins.sbt` — Current plugin set; bump targets.
- `.scalafmt.conf` — Current dialect overrides; upgrade target for 2.13 fileOverride.

### Upstream target state
- `upstream/scala-3` HEAD `1561d8dc` (== upstream master) — Phase 1 PR bases here. No Scala 3 work landed.
- Expect scalafmt 3.11.1 reformat in upstream `scala-3` (per PROJECT.md). Reconcile by accepting upstream reformat.

### External docs
- sbt cross-build manual: https://www.scala-sbt.org/1.x/docs/Cross-Build.html — `crossScalaVersions`, `++` semantics.
- sbt-github-actions: https://github.com/sbt/sbt-github-actions — `githubWorkflowBuild`/`githubWorkflowBuildPreamble` keys for the 5 gate commands.
- `made` library on Maven Central: `io.github.halotukozak:made_3:0.1.0`.

### Memory rules
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_fix_dont_suppress_warnings.md` — No new `@nowarn`/`-Wconf`.
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_dont_port_deprecated.md` — Not directly applicable in Phase 1 (no source ports).

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets (from fork master)
- `build.sbt` `aggregateProjectSettings` val — applies `publish/skip` etc. to aggregate-only projects. Reuse for `jvm`/`jvm2`.
- `build.sbt` already defines `jvm2 .aggregate(jetty)` — Phase 1 lifts this onto upstream/scala-3.
- `madeVersion`/`scala2Version`/`scala3Version` val declarations at top of build.sbt — pattern to replicate.
- `inThisBuild { ... githubWorkflowBuildPreamble ++= Seq(Setup Node.js, Setup MongoDB ... )}` block — preamble keys to extend.

### Established Patterns
- sbt-github-actions drives `ci.yml`. Editing `ci.yml` by hand is forbidden (file says so in its header). All CI changes go through sbt keys.
- `Global / onChangedBuildSource := ReloadOnSourceChanges` at build.sbt top — preserve.
- `forIdeaImport` flag pattern at build.sbt top — preserve, untouched.

### Integration Points
- Phase 1 PR onto upstream/scala-3 is the FIRST PR. Subsequent module-port PRs (Phase 3+) `dependsOn` this infra landing first.
- `MIGRATION.md` (Phase 2) tracks state but does not exist in Phase 1 — don't reference it from build.sbt comments.

</code_context>

<specifics>
## Specific Ideas

- User invokes `git fetch upstream && git checkout -b 01-cross-compile-infra upstream/scala-3` to start the slice (per PROJECT.md per-PR workflow).
- Slice is hand-authored on top of upstream/scala-3 — not a raw `git cherry-pick` of master commits, because master commits include source ports.
- The 5 gate commands run locally before push: `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll`.
- Commit prefix examples: `build:`, `ci:`, `style(scalafmt):` — no GSD nomenclature.

</specifics>

<deferred>
## Deferred Ideas

- `sbt-tasty-mima` 1.4.0 plugin staging — deferred to Phase 11 / v2 RELEASE-01 (REQ INFRA-04 partial drift acknowledged).
- `analyzer` / `spring` / RPC reactivation under `jvm2` — Phase 12 formalization.
- `Makefile` with `make ci` target — out of scope. Reconsider if per-PR friction surfaces.
- Java 21/25 CI matrix — v2 CI-01.
- Scala 3 LTS (3.3.x) vs current (3.8.x) decision — first `_3` release tag (v2).
- `tastyMiMaPreviousArtifacts` activation — first `_3` release.
- Scalac warning flag harmonization between 2.13 and 3 — out of Phase 1 scope; chase as drift surfaces in module ports.

</deferred>

---

*Phase: 01-cross-compile-infrastructure*
*Context gathered: 2026-05-30*
