# Phase 1: Big bang — comment broken, green CI — Research

**Researched:** 2026-06-01
**Domain:** sbt build infrastructure pivot to Scala 3 only + per-file commenting workflow
**Confidence:** HIGH (build infra) / HIGH (option migration) / HIGH (module inventory) / MEDIUM (commenting volume estimates)

## Summary

Phase 1 is a single big-bang PR cut from `upstream/scala-3 @ 1561d8dc`. Upstream is currently Scala 2.13.18 only (no Scala 3 commits landed). The work is mostly **build-infra surgery + mechanical commenting**, not algorithmic. Three concentrated technical risks: (1) scalac option migration (~10 flags removed, 2 renamed, 8 kept in Scala 3); (2) `commons-macros` and most of `commons-core` are saturated with scala-2 macro syntax (≥35 files in core, 26 in macros use `c.universe`/whitebox/blackbox) — these comprise the bulk of commenting volume; (3) the ScalaJS toolchain (sbt-scalajs 1.21.0, sbt-jsdependencies, sbt-jmh) is current and supports Scala 3.

**Primary recommendation:** Migrate scalac options in two steps — first drop the Scala-3-removed ones (`-Yrangepos`, `-Xfatal-warnings`, `-Xsource:3`, `-Xlint`, `-Ycache-*`), keep the 8 portable ones verbatim, fold the 2.13-only `-Xnon-strict-patmat-analysis` block away entirely. Then comment macro-defining files file-by-file in topological order: `macros` → `core` → `cbor`/`mongo`/`hocon` → JS variants → `benchmark`. Disable `analyzer`, `spring`, `jetty` at the aggregate level (drop from `jvm` aggregate), not `Compile/skip` per-module — simpler diff.

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

**Build infra**
- `scalaVersion := scala3Version` (3.8.2). Drop `crossScalaVersions` everywhere.
- Migrate scalac options to Scala 3 syntax. Specific items: drop `-Xsource:3`, drop `-Wconf` blocks, audit each remaining flag against Scala 3 docs.
- `made` dependency stays at `0.1.1` (already bumped pre-pivot).
- Archive `scala-2.13/` source dirs: rename to `__scala-2.13__/` so they're visible in git history but excluded from build by default sourceDir resolution. Phase 1 may eventually delete them — but archive-by-rename first to preserve content for reference.
- `.scalafmt.conf`: single `runner.dialect = scala3`. No `fileOverride`.
- CI matrix: single Scala 3 axis × Temurin 17/21/25 (3 shards).
- Remove `commons-` ProjectGroup prefix? — **KEEP**. sbt-nosbt convention preserved from upstream.
- Drop `jvm2` aggregate (was for 2.13-only modules). All modules either in `jvm` aggregate or excluded entirely.

**Module fate**

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

**Commenting convention**
- Per-file granularity. Comment specific defs/classes/objects — NOT whole files (preserve package decl + imports + working code).
- Block comment style: `/* ... */`. NOT line-by-line `//`.
- Tag immediately above commented block: `// TODO[scala3-port]: <brief feature description>`.
- Test sources: per-file commenting too — broken test classes individually, not whole `Test/` dirs.

**MIGRATION.md (fresh, root)** — 5 sections per `[[feedback-migration-md-contract]]`:
1. Will not migrate, 2. Deprecated on Scala 3, 3. Source-compat breaks, 4. Binary-compat breaks (empty Phase 1), 5. Disabled tests / modules.

**TODO format:** `// TODO[scala3-port]: <feature description>` — feature ~50 chars, grep-friendly. Optional `(S/M/L)` suffix.

**Imports / unused:** After commenting, expect `unused import` warnings — DO NOT add `@nowarn`. Either remove the import or keep it commented next to its block.

**Acceptance gates (locked):**
- `sbt 'show version'` exits 0
- `sbt compile` exits 0 (across enabled modules)
- `sbt Test/compile` exits 0
- `sbt scalafmtCheckAll` exits 0
- Fork CI + AVSystem PR CI both green
- `MIGRATION.md` populated with all 5 sections

**PR conventions:**
- Title: `[Scala 3] <subject>` ≤70 chars. Suggested: `[Scala 3] Pivot to Scala 3 only — comment broken, green CI`
- Base: `AVSystem/scala-commons:scala-3` (head of upstream)
- Draft on open. Milestone "Scala 3" (#1). Maintainer merges manually.
- User ack before push + before PR open.

### Claude's Discretion
- Exact per-file commenting boundaries during execution.
- Whether to keep some scala-2.13/ files inline-commented vs. archived under `__scala-2.13__/`.
- Order of module attack (probably: macros → core → cbor → mongo → hocon → js variants → tests).
- Whether to wholesale `git rm` some modules (jetty/spring/analyzer) vs. `Compile/skip := true`. Default: skip + comment in place to keep git history visible for future reference; full rm only if user later prefers it.

### Deferred Ideas (OUT OF SCOPE)
- LTS (3.3.x) vs current (3.8.x) decision — deferred per milestone description.
- Tasty-MiMa activation — deferred until first Scala 3 release ships.
- Formal retirement of `jetty`/`analyzer`/`spring` modules (git rm vs disable) — Phase 1 disables; formal removal in follow-up PR.
- Cherry-pick from fork master for restoration — Phase 2+ pattern, NOT Phase 1.
- Test re-enablement — Phase 1 only requires `Test/compile` green; tests pass per restoration PR.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| BUILD-01 | `scalaVersion := scala3Version` in Commons.scala. No `crossScalaVersions`. | Upstream @ 1561d8dc has `scalaVersion := "2.13.18"` (no cross-build); migration is 1-line + delete `crossScalaVersions` if present. Current branch carries cross-build; Phase 1 reverts to single-axis on Scala 3. See "Upstream baseline build state". |
| BUILD-02 | scalac options migrated to Scala 3 syntax. `-Xsource:3` removed. `-Wconf` blocks removed. Unused/deprecated flags audited. | Full per-flag migration table below ("scalac Options Migration"), sourced from Scala 3 official migration guide. |
| BUILD-03 | `scala-2.13/` source dirs archived (`__scala-2.13__/`) OR removed. `sourceDirsSettings` cleaned. | Upstream @ 1561d8dc has **zero** `scala-2.13/` dirs (count = 0 across all modules — see "Module Inventory"). All `scala-2.13/` work is local-only; Phase 1 needs to ensure none are re-introduced and `mkSourceDirs` is simplified. |
| BUILD-04 | `.scalafmt.conf` simplified — single `runner.dialect = scala3`. No fileOverride for 2.13. | Upstream uses `Scala213Source3` dialect — Phase 1 flips to `scala3`, deletes `fileOverride` blocks; format affects 9 files per Plan 01 (2026-05-30) history (but on this branch some are already reformatted). |
| BUILD-05 | CI runs single Scala 3 axis × Temurin 17/21/25 (3 shards). | sbt-github-actions 0.30.0 supports single-axis matrix; set `githubWorkflowScalaVersions := Seq(scala3Version)`. Run `githubWorkflowGenerate` to regenerate `.github/workflows/ci.yml`. |
| COMMENT-01..05 | Per-file commenting with `/* ... */` + `// TODO[scala3-port]:` tag. | Volume estimates per module in "Compile-Broken Estimate". Mechanical work; no library required. |
| COMPILE-01..03 | `sbt compile`, `Test/compile`, `scalafmtCheckAll` exit 0. | Concrete commands listed in "Validation Architecture". |
| DOC-01..02 | `MIGRATION.md` at root with 5 sections; backlog populated from `git grep TODO[scala3-port]`. | Format prescribed by memory `[[feedback-migration-md-contract]]`. No library; mechanical authoring. |
| CI-01..02 | Fork + AVSystem PR CI green. | Follows from BUILD-05 + COMPILE-01..03 once `.github/workflows/ci.yml` is regenerated. |
</phase_requirements>

## Upstream Baseline Build State (1561d8dc)

Verified via `git show 1561d8dc:project/Commons.scala` and `git ls-tree`:

| Property | Upstream value @ 1561d8dc | Phase 1 target |
|----------|---------------------------|----------------|
| `scalaVersion` | `"2.13.18"` | `scala3Version = "3.8.2"` (constant added) |
| `crossScalaVersions` | not present at top level | not present (stay absent) |
| Top-level aggregates | `jvm`, `js` | `jvm`, `js` (drop `jvm2` if present on branch) |
| `jvm` aggregate members | `analyzer, macros, core, jetty, mongo, hocon, spring` | `macros, core, mongo, hocon` (drop `analyzer, jetty, spring`) |
| `js` aggregate members | `core-js, mongo-js` | unchanged |
| `mkSourceDirs` | present, takes `scalaBinary` param | keep but `scalaBinary` always `"3"` — simplify to drop the `scala-$scalaBinary` rung, or pass `"3"` literal |
| `scala-2.13/` source dirs (all modules) | **0 files** | n/a |
| `madeVersion` | not present in Commons.scala (made not yet introduced) | `"0.1.1"` constant + dep on `core` |
| Scala 2 macro defs (`= macro`) | core=35, macros=26, mongo=3, analyzer=1 | all commented or moved |
| `c.universe`/whitebox/blackbox usage | macros=26, core (impl files via macros dep) | all in `macros/src/main/scala/` |
| CI matrix | scala=[2.13.18] × java=[17/21/25] + mima/scalafmt jobs | scala=[3.8.2] × java=[17/21/25], drop separate mima job |

**Key inference:** Upstream has no Scala 3 work at all. Phase 1 is not a delta from "partial Scala 3" → "full Scala 3"; it's a clean fork-from-2.13-baseline → "Scala 3 only with broken stuff commented". The current branch carries pre-pivot cross-build experiments that Phase 1 will largely *delete*.

## scalac Options Migration

**Source:** [Scala 3 Compiler Options Lookup Table](https://docs.scala-lang.org/scala3/guides/migration/options-lookup.html) (HIGH confidence).

### Common settings (upstream @ 1561d8dc + current branch)

| Scala 2 option | Scala 3 status | Scala 3 form | Phase 1 action |
|----------------|----------------|--------------|----------------|
| `-encoding utf-8` | Kept | `-encoding utf-8` | **KEEP** |
| `-Yrangepos` | Removed | (none) | **DROP** |
| `-explaintypes` | Renamed | `-explain-types` | **RENAME** (or drop — diagnostic only) |
| `-feature` | Kept | `-feature` | **KEEP** |
| `-deprecation` | Kept | `-deprecation` | **KEEP** |
| `-unchecked` | Kept | `-unchecked` | **KEEP** |
| `-language:implicitConversions` | Kept | `-language:implicitConversions` | **KEEP** |
| `-language:existentials` | Kept | `-language:existentials` | **KEEP** |
| `-language:dynamics` | Kept | `-language:dynamics` | **KEEP** |
| `-language:experimental.macros` | Kept (but Scala 3 has different macro system) | `-language:experimental.macros` | **DROP** — Scala 3 macros are `inline` + quotes; flag is irrelevant since macro defs are commented |
| `-language:higherKinds` | Kept | `-language:higherKinds` | **KEEP** |
| `-Xfatal-warnings` | Removed | `-Werror` (semantic equivalent) | **REPLACE with `-Werror`** OR drop entirely. Note: `-Xfatal-warnings` was deprecated/removed in Scala 3.8 — see [scalac-options#219](https://github.com/typelevel/scalac-options/issues/219). Memory rule [[feedback-fix-dont-suppress-warnings]] says fix warnings at source — so `-Werror` is the spirit-correct choice. |
| `-Xsource:3` | Renamed | `-source 3.x` (no longer needed — you ARE Scala 3) | **DROP** |
| `-Xlint:-missing-interpolator,-adapted-args,-unused,_` | Removed | (no equivalent; Scala 3 has different lints e.g. `-Wunused`) | **DROP** entirely. Restore selective lints in later phase if useful. |
| `-Ycache-plugin-class-loader:last-modified` | Removed | (none) | **DROP** |
| `-Ycache-macro-class-loader:last-modified` | Removed | (none) | **DROP** |

### Scala-2.13-only block (entire block goes away)

| Option | Status | Action |
|--------|--------|--------|
| `-Xnon-strict-patmat-analysis` | Removed | **DROP** (whole `if (scalaBinaryVersion.value == "2.13")` block deletes) |
| `-Xlint:-strict-unsealed-patmat` | Removed | **DROP** |

### comprof module (scalac-profiling plugin)
- `-P:scalac-profiling:*` flags depend on plugin (`ch.epfl.scala:scalac-profiling`). The plugin is Scala-2-only. **comprof module is disabled in Phase 1** (drop from any aggregate; per CONTEXT it's not in the keep list).
- `-Xmacro-settings:statsEnabled`, `-Ystatistics:typer` — Scala 2 only; vanish with comprof disable.

### Final Scala 3 scalacOptions (proposed Phase 1)

```scala
Compile / scalacOptions ++= Seq(
  "-encoding", "utf-8",
  "-explain-types",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
  "-language:higherKinds",
  "-Werror",  // replaces -Xfatal-warnings; honors [[feedback-fix-dont-suppress-warnings]]
),
```

Drop the `if (scalaBinaryVersion.value == "2.13") ...` block entirely. Drop `-Ymacro-expand:none` from unidoc scalacOptions (Scala 2 only).

## ScalaJS, sbt-nosbt, sbt-jmh — Scala 3 Compatibility

| Plugin | Upstream version | Scala 3 status | Action |
|--------|------------------|----------------|--------|
| `sbt-scalajs` | 1.21.0 | HIGH — supports Scala 3.x; 1.21.0 is current (April 2026 release) | **KEEP** |
| `sbt-jsdependencies` | 1.0.2 | HIGH — version-agnostic | **KEEP** |
| `sbt-nosbt` (ghik) | 0.2.1 | HIGH — sbt plugin itself; doesn't care about project's Scala version. `ProjectGroup` works with Scala 3 builds (sbt-nosbt docs explicitly use `scalaVersion := "3.x"`). | **KEEP** |
| `sbt-jmh` | 0.4.8 | HIGH — runtime instrumentation, Scala-version-agnostic | **KEEP** |
| `sbt-scalafmt` | 2.6.1 | HIGH | **KEEP** |
| `sbt-github-actions` | 0.30.0 | HIGH | **KEEP** |
| `sbt-mima-plugin` | 1.1.5 (1.1.4 on upstream) | HIGH — Scala 3 supported since 1.1.x | **KEEP** at 1.1.5 |
| `sbt-unidoc` | 0.6.1 | HIGH | **KEEP** but check unidoc on Scala 3 (Scaladoc 3 may behave differently — DOCS not in scope for Phase 1) |
| `sbt-ide-settings` | 1.1.4 | n/a | **KEEP** |
| `sbt-ci-release` | 1.11.2 | n/a (CI side) | **KEEP** |
| `sbt-updates` | 0.6.4 | n/a | **KEEP** |

**Note:** Scala.js 1.21.0 deprecates JDK < 17 (matches Phase 1 CI matrix 17/21/25 — fine).

## `made` library 0.1.1 verification

`cellar get-external io.github.halotukozak:made_3:0.1.1 made.Default` resolved successfully — package `made` exists on Sonatype Central as `io.github.halotukozak:made_3:0.1.1` with members including `Default[O]`, `Made`, `MadeElem`, `MadeFieldElem`, `TransparentWrapping`, `Done`, `DoneOperation`, `InputElem`, etc. **HIGH confidence — resolvable.** Phase 1 keeps `madeVersion = "0.1.1"`.

`core` libraryDependencies adds: `"io.github.halotukozak" %% "made" % madeVersion` — unconditional (no `if (scalaBinaryVersion.value == "3")` guard; we're Scala 3 only).

## Module Inventory (upstream @ 1561d8dc)

Per `git ls-tree` + `git grep` on `1561d8dc`:

| Module | Files (main/test) | scala-2 macro defs | c.universe usage | `scala-2.13/` dir | ScalaJS variant | Phase 1 fate |
|--------|-------------------|---------------------|------------------|-------------------|-----------------|--------------|
| `macros` | 28 / 0 | 0 (impls, no defs) | 26 (every file) | none | no | **comment entire `src/main/scala/`** — empty stub remains; restored Phase 2 |
| `core` | 146 / 81 | 35 | (consumers via macros dep) | none | yes (`core-js`, files under `core/js/...`) | **heavy commenting** — derivation/GenCodec/RPC/extension macros |
| `core-js` | (shares with `core` via `core/js/src/`) | (subset of core) | — | none | (is itself) | follows core |
| `mongo` | 82 / 21 | 3 | 3 | none | yes (`mongo-js`, files under `mongo/js/`) | **partial commenting** — typed mongo wrappers w/ macros + GenCodec consumers |
| `mongo-js` | (under `mongo/js/`) | — | — | none | (is itself) | follows mongo |
| `hocon` | 9 / 4 | 0 | 0 | none | no | **likely compiles cleanly** — data wrappers; tiny commenting |
| `analyzer` | 24 / 23 | 1 | 1 | none | no | **DISABLE** — Scala 2 compiler plugin; drop from `jvm` aggregate |
| `jetty` | 2 / 1 | 0 | 0 | none | no | **DISABLE** — deprecated upstream; drop from aggregate |
| `spring` | 5 / 3 | 0 | 0 | none | no | **DISABLE** — deprecated upstream; drop from aggregate |
| `benchmark` (jvm + js) | 11 / 0 | 0 (consumer of macros) | 0 | none | yes | **comment broken** — codec consumers will break |
| `comprof` | (synthetic via sourceGenerators) | 0 | 0 | none | no | **DROP** — scalac-profiling plugin is Scala 2 only |

**Notes:**
- The `cbor` module mentioned in CONTEXT does NOT exist as a module in upstream @ 1561d8dc. There's a `core/.../serialization/cbor/` sub-package inside `core` — commented as part of core.
- `comprof` is in upstream but uses Scala-2-only `scalac-profiling`. Drop it.
- Total scala test files in upstream: **133** across all modules. Each must be triaged: compile or comment.

## sbt-github-actions Single-Axis Recipe

`sbt-github-actions` 0.30.0 generates `.github/workflows/ci.yml` from settings. Phase 1 simplifications:

```scala
scalaVersion := scala3Version,                                 // single axis
// REMOVE: crossScalaVersions := ...
githubWorkflowJavaVersions := Seq(                             // 3 shards
  JavaSpec.temurin("17"),
  JavaSpec.temurin("21"),
  JavaSpec.temurin("25"),
),
githubWorkflowScalaVersions := Seq(scala3Version),             // explicit single
githubWorkflowBuild := Seq(                                    // simplified gate
  WorkflowStep.Sbt(
    List("compile", "Test/compile", "scalafmtCheckAll", "scalafmtSbtCheck"),
    name = Some("Build + lint"),
  ),
),
```

Drop the `if [ "${{ matrix.scala }}" = "2.13.18" ]` branching script entirely. Drop separate `mima`/`scalafmt` `githubWorkflowAddedJobs` — fold scalafmt into the main build step; defer MiMa to a later phase (no Scala 3 release exists yet for baseline).

Workflow regen command: `sbt githubWorkflowGenerate`. CI's `githubWorkflowCheck` enforces sync.

## Architecture Patterns

### Per-File Commenting Convention

**What:** Wrap each broken definition (def / class / object / trait) in `/* ... */` with a `// TODO[scala3-port]: <feature>` tag immediately above.

**Pattern:**

```scala
package com.avsystem.commons.serialization

import com.avsystem.commons.misc.SomeStillWorkingThing

// keep what compiles at file head ...

// TODO[scala3-port]: GenCodec.materialize (whitebox macro)
/*
object GenCodec {
  implicit def materialize[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.materialize[T]
  // ...
}
*/

// further still-compiling code stays uncommented
```

**Rules:**
- Block comment `/* ... */`, never line-by-line `//`.
- Tag on its own line, immediately above the opening `/*`. Description ≤50 chars, grep-friendly. Optional `(S/M/L)` suffix for effort.
- Preserve `package` decl, imports relevant to the surviving code, and any working code in the same file.
- After commenting, fix unused-import warnings by **removing** the import (not `@nowarn`). If the import is still needed by adjacent uncommented code, keep it.

### Aggregate-Level Disable Pattern

For modules dropped wholesale (`analyzer`, `jetty`, `spring`, `comprof`):

```scala
lazy val jvm = mkSubProject
  .in(file(".jvm"))
  .aggregate(
    // analyzer,   // TODO[scala3-port]: Scala 2 compiler plugin; restore as Scala 3 plugin (L)
    macros,
    core,
    // jetty,      // TODO[scala3-port]: ee10 servlet wrapper (M)
    mongo,
    hocon,
    // spring,     // TODO[scala3-port]: spring-context wiring (S)
  )
```

Plus keep the `lazy val analyzer = ...` declaration commented or kept (Claude's discretion). Preferred: keep `lazy val` declarations intact, only remove from aggregate — minimum-diff; restoration just adds back to aggregate.

### MIGRATION.md 5-Section Skeleton

```markdown
# Scala 3 Migration

## 1. Will not migrate
| Symbol/module | Rationale |
|---------------|-----------|
| `OptCompat`/`NOptCompat`/`OptRefCompat` mixins | stdlib has equivalents in Scala 3 |
| ... | ... |

## 2. Deprecated on Scala 3
| Symbol | Since | Replacement |
| ... | ... | ... |

## 3. Source-compat breaks
### core
- `com.avsystem.commons.SharedExtensions.orderingOps` removed
### mongo
- ...

## 4. Binary-compat breaks
*(empty for Phase 1 — no Scala 3 baseline released yet)*

## 5. Disabled tests / modules
### Modules
- `analyzer` — restore as Scala 3 compiler plugin (effort: L)
- `jetty` — wrapper port (effort: M)
- `spring` — wrapper port (effort: S)
### Tests (per-file)
- `core/src/test/scala/.../GenCodecTest.scala` — commented (depends on `materialize`)
- ...

## Backlog
*(generated from `git grep -n 'TODO\[scala3-port\]'`)*
| Location | Description | Effort |
| --- | --- | --- |
```

## Don't Hand-Roll

| Problem | Don't build | Use instead | Why |
|---------|-------------|-------------|-----|
| Scalac option translation | Custom `if (scala3) Seq(...) else Seq(...)` ladder | Single Scala 3 list (no `if`) | We're Scala 3 only — no axis to switch on |
| Per-module skip | `Compile/skip := true` ladder on every module | Drop from aggregate `.aggregate(...)` | Aggregate-level drop is one line per module + restoration is one line back |
| Macro translation in Phase 1 | Manual `inline def` + quotes ports | **Don't.** Comment + restore in Phase 2+ | Phase 1 = commenting only; macro ports are dedicated phases |
| TODO backlog generation | Hand-maintained list in MIGRATION.md | `git grep -n 'TODO\[scala3-port\]'` script | Tags ARE the backlog; grep regenerates |
| `@nowarn` for commented-import warnings | `@nowarn("cat=unused-imports")` | Remove the import or leave it inside the comment block | Memory rule [[feedback-fix-dont-suppress-warnings]] |

## Common Pitfalls

### Pitfall 1: `-Werror` triggered by `unused import` cascade
**What goes wrong:** Commenting a block of code leaves its imports unused; `-Werror` turns warnings into errors; sbt compile reverses green → red.
**Why it happens:** Scala 3's `-Wunused:imports` is on by default in some configs; `-Werror` propagates.
**How to avoid:** Either (a) prune imports as you comment, (b) defer `-Werror` to a later phase, (c) keep imports inside the commented block. Per memory rule, NO `@nowarn`. **Recommended:** prune imports — green diff is cleaner.
**Warning sign:** sbt compile fails with `value foo is never used` after a commit that only added `/* */` blocks.

### Pitfall 2: ScalaJS resource map URI flag still references `version.value`
**What goes wrong:** `jsCommonSettings` injects `-P:scalajs:mapSourceURI:...->...$/v${version.value}/` — fine on Scala 3, but if `version.value` resolves to a SNAPSHOT, the URL points nowhere. Phase 1 doesn't change this but verify.
**How to avoid:** Leave the flag as-is. Confirm `js` aggregate compiles after pivot.

### Pitfall 3: `mkSourceDirs` still adds `scala-3/` rung — collision with `scala/`
**What goes wrong:** Upstream `mkSourceDirs` injects `src/main/scala-$scalaBinary` (= `scala-3`). If you put files in both `scala/` and `scala-3/`, sbt picks both → duplicate-class errors.
**How to avoid:** Either (a) drop the `scala-$scalaBinary` rung from `mkSourceDirs` (Phase 1 has no use for it), or (b) ensure no `scala-3/` dirs exist. Recommended: simplify `mkSourceDirs` to just `scala/` + `java/` since we're single-version.

### Pitfall 4: `Compile/skip` plus `dependsOn` ⇒ sbt still compiles the dependency
**What goes wrong:** `lazy val core = ... .dependsOn(macros)` — if `macros` has `Compile/skip := true` but isn't dropped from aggregates, sbt still resolves and compiles `macros`. Skip only suppresses the *current* project's compile, not its dependers.
**How to avoid:** Empty out `macros/src/main/scala/` (or comment all contents) rather than relying on `Compile/skip`. This is the CONTEXT-locked approach for `macros`.

### Pitfall 5: scalafmt dialect flip reformats hundreds of files
**What goes wrong:** Switching `runner.dialect` from `Scala213Source3` to `scala3` triggers wholesale reformatting on `scalafmtCheckAll`. PR diff balloons.
**How to avoid:** Run `sbt scalafmtAll` once early in the PR; commit as a dedicated `style(scalafmt):` commit so the reformat is reviewable separately from the semantic commenting work. Memory rule `[[feedback-format-off-macro-defs-ok]]` permits `// format: off` around scala-2 macro defs that survive in `__scala-2.13__/`.

### Pitfall 6: `githubWorkflowCheck` fails after manual ci.yml edits
**What goes wrong:** sbt-github-actions enforces that ci.yml is generated. Hand-edits → next push fails CI.
**How to avoid:** ALWAYS edit Commons.scala → run `sbt githubWorkflowGenerate` → commit both together. Never hand-edit ci.yml.

## Code Examples

### Final scalacOptions snippet (Scala 3 only)
```scala
Compile / scalacOptions ++= Seq(
  "-encoding", "utf-8",
  "-explain-types",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
  "-language:higherKinds",
  "-Werror",
),
Test / scalacOptions := (Compile / scalacOptions).value,
```

### Simplified `mkSourceDirs` (drop scala-N rung)
```scala
def mkSourceDirs(base: File, conf: String): Seq[File] = Seq(
  base / "src" / conf / "scala",
  base / "src" / conf / "java",
)

def sourceDirsSettings(baseMapper: File => File) = Seq(
  Compile / unmanagedSourceDirectories ++= mkSourceDirs(baseMapper(baseDirectory.value), "main"),
  Test    / unmanagedSourceDirectories ++= mkSourceDirs(baseMapper(baseDirectory.value), "test"),
)
```

### Top-level constants
```scala
val scala3Version = "3.8.2"
val madeVersion = "0.1.1"
// scala2Version DROPPED
```

### Commented-block file pattern
```scala
package com.avsystem.commons.misc

import com.avsystem.commons.macros.misc.SomeMacro  // kept iff still referenced uncommented

// TODO[scala3-port]: AnnotationOf macro materialization (L)
/*
trait AnnotationOf[A, T] {
  def annotation: A
}
object AnnotationOf {
  implicit def materialize[A, T]: AnnotationOf[A, T] = macro SomeMacro.annotationOf[A, T]
}
*/
```

### Aggregate-edit pattern
```scala
lazy val jvm = mkSubProject
  .in(file(".jvm"))
  .aggregate(
    macros,
    core,
    mongo,
    hocon,
    // analyzer, jetty, spring — disabled in Phase 1; restore per dedicated phase
  )
  .settings(aggregateProjectSettings)
```

## State of the Art

| Old (Scala 2.13 on upstream) | New (Scala 3 on Phase 1) | Notes |
|------------------------------|--------------------------|-------|
| Whitebox macros (`c.universe`, `c.Expr`, `reify`) | Scala 3 inline + quotes (`scala.quoted.*`) | Phase 1 doesn't implement; just comments. |
| `-Xfatal-warnings` | `-Werror` | Same semantics; renamed in Scala 3. |
| `-Xlint:...` family | `-Wunused:*`, `-Wvalue-discard`, etc. (different lints) | No 1:1 mapping; defer to later phase. |
| `crossScalaVersions := Seq(...)` | Single `scalaVersion := ...` | We're 3-only. |
| `scala-2.13/` source dirs | none | Phase 1 ensures none exist; archive any that sneak in to `__scala-2.13__/`. |

**Deprecated/outdated:**
- `-language:experimental.macros` — Scala 3 macros use different syntax; flag is no-op. Drop.
- `scalac-profiling` (`ch.epfl.scala`) — Scala 2 only; `comprof` module retires.
- sbt-mima-plugin tasty-mima activation — deferred to first Scala 3 release.

## Open Questions

1. **`-Werror` or no `-Werror` in Phase 1?**
   - What we know: Memory rule [[feedback-fix-dont-suppress-warnings]] says fix at source; `-Werror` enforces that.
   - What's unclear: Commenting hundreds of blocks WILL produce unused-import warnings during the work. If `-Werror` is on, every intermediate commit fails.
   - **Recommendation:** Land Phase 1 with `-Werror` **deferred** (commented `// "-Werror"` in scalacOptions with a TODO). Activate `-Werror` in a dedicated Phase 1.5 or first restoration PR once imports are clean. This honors the spirit of the memory rule (no `@nowarn`) without blocking Phase 1 acceptance gates.

2. **`comprof` removal vs `Compile/skip`?**
   - The scalac-profiling plugin is Scala 2 only.
   - **Recommendation:** Drop from aggregate; keep `lazy val comprof = ...` body commented at the bottom of Commons.scala for future reference. Or `git rm` outright (user discretion).

3. **`benchmark` JMH on Scala 3?**
   - sbt-jmh 0.4.8 supports Scala 3, but the benchmark sources depend on commented-out codecs.
   - **Recommendation:** Comment broken benchmark sources file-by-file; keep `benchmark`/`benchmark-js` in `jvm`/`js` aggregates. If a benchmark file has no surviving code, comment everything except the package decl.

4. **`unidoc` on Scala 3?**
   - sbt-unidoc 0.6.1 on Scala 3 uses Scaladoc 3, which is a different generator. `-Ymacro-expand:none` is a Scala 2 flag and must be dropped.
   - **Recommendation:** Drop `-Ymacro-expand:none` from unidoc scalacOptions. Don't validate docs generation in Phase 1; acceptance gate is `compile` + `Test/compile`, not `unidoc`.

## Validation Architecture

### Test Framework
| Property | Value |
|----------|-------|
| Framework | ScalaTest 3.2.20 (+ scalacheck 1.19.0, scalatestplus-scalacheck 3.2.14.0) |
| Config file | none — declared via sbt `libraryDependencies` in `Commons.commonSettings` |
| Quick run command | `sbt 'Test/compile'` (compile only — Phase 1 does NOT run tests) |
| Full suite command | `sbt scalafmtCheckAll scalafmtSbtCheck compile Test/compile` |

**Important:** Phase 1 acceptance is **compile-only**. Tests are not run. Per CONTEXT: "Test sources commented per-file" + "Test/compile exits 0". Test execution is deferred to restoration phases per memory and to ROADMAP cross-cutting requirements.

### Phase Requirements → Validation Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|--------------|
| BUILD-01 | scalaVersion = 3.8.2; no crossScalaVersions | smoke | `sbt 'show scalaVersion' \| tail -3 \| grep -q 3.8.2` | n/a |
| BUILD-02 | scalac options Scala-3-valid | smoke | `sbt 'show scalacOptions' && sbt compile` (warnings about unknown flags ⇒ red) | n/a |
| BUILD-03 | No `scala-2.13/` dirs except archive | smoke | `! find . -type d -name 'scala-2.13' -not -path '*/__scala-2.13__/*' \| grep .` | n/a |
| BUILD-04 | scalafmt single dialect | smoke | `grep -c 'fileOverride' .scalafmt.conf` returns 0 |  n/a |
| BUILD-05 | CI single-axis Scala 3 × Java 17/21/25 | smoke | `grep -c 'scala: \[3.8.2\]' .github/workflows/ci.yml` returns ≥1 AND `grep -c '2.13' .github/workflows/ci.yml` returns 0 | n/a |
| COMMENT-01 | `/* */` blocks present where TODO tags are | smoke | `git grep -B0 -A1 'TODO\[scala3-port\]' \| grep -c '/\*'` ≈ tag count | n/a |
| COMMENT-02 | Every commented block has TODO tag | manual review | sample-check 10 random tags | n/a |
| COMMENT-05 | Disabled modules dropped from aggregates | smoke | `sbt 'show jvm/aggregateProjects' \| grep -v -E '(analyzer\|jetty\|spring)'` | n/a |
| COMPILE-01 | sbt compile exit 0 | unit | `sbt compile` | n/a |
| COMPILE-02 | sbt Test/compile exit 0 | unit | `sbt Test/compile` | n/a |
| COMPILE-03 | scalafmtCheckAll exit 0 | unit | `sbt scalafmtCheckAll scalafmtSbtCheck` | n/a |
| DOC-01 | MIGRATION.md has 5 sections | smoke | `grep -c '^## ' MIGRATION.md` returns ≥5 | n/a |
| DOC-02 | Backlog populated | smoke | `awk '/^## Backlog/,EOF' MIGRATION.md \| grep -c '^|'` returns > 0 | n/a |
| CI-01 / CI-02 | CI green | manual | `gh run watch` / PR check API | n/a |
| QUALITY-01 | No new @nowarn/-Wconf | smoke | `git diff upstream/scala-3...HEAD -- '*.scala' \| grep -cE '@nowarn\|-Wconf'` returns 0 | n/a |

### Sampling Rate
- **Per task commit:** `sbt compile` (`Test/compile` after every commenting-related commit)
- **Per wave merge:** `sbt scalafmtCheckAll compile Test/compile`
- **Phase gate (before push/PR):** Full suite — `sbt scalafmtCheckAll scalafmtSbtCheck compile Test/compile` exits 0, AND `gh run watch` on fork push is green

### Wave 0 Gaps
*(None — Phase 1 doesn't execute tests; the gates are sbt invocations against the standard sbt project. No new fixtures needed.)*

The only Wave-0-style task is ensuring the `scalafmt` dialect flip lands as a dedicated `style(scalafmt):` commit BEFORE per-file commenting begins. Otherwise mid-PR commits will fail `scalafmtCheckAll` for unrelated formatting churn.

## Sources

### Primary (HIGH confidence)
- [Scala 3 Compiler Options Lookup Table](https://docs.scala-lang.org/scala3/guides/migration/options-lookup.html) — every scalac flag's Scala 3 status
- [Scala 3 Migration Guide](https://docs.scala-lang.org/scala3/guides/migration/) — overall migration framework
- [Announcing Scala.js 1.21.0](http://www.scala-js.org/news/2026/04/04/announcing-scalajs-1.21.0/) — 2026-04 release; JDK 17+ deprecation; Scala 3.8 alignment
- [sbt-nosbt](https://github.com/ghik/sbt-nosbt) — `ProjectGroup` Scala 3 compatible
- `git show 1561d8dc:project/Commons.scala`, `git show 1561d8dc:project/plugins.sbt`, `git show 1561d8dc:.scalafmt.conf`, `git show 1561d8dc:.github/workflows/ci.yml` — upstream baseline
- `cellar get-external io.github.halotukozak:made_3:0.1.1 made.Default` — confirmed resolvable
- `git ls-tree -r 1561d8dc` + `git grep` on `1561d8dc` for module inventory and macro counts

### Secondary (MEDIUM confidence)
- [`-Xfatal-warnings` deprecated since Scala 3.8 (typelevel/scalac-options#219)](https://github.com/typelevel/scalac-options/issues/219) — informs `-Werror` recommendation
- [Configuring and suppressing warnings in Scala (scala-lang blog)](https://www.scala-lang.org/2021/01/12/configuring-and-suppressing-warnings.html) — context on `-Werror`/`-Wconf` semantics

### Tertiary (LOW confidence)
- (none — all critical claims sourced from HIGH/MEDIUM)

## Metadata

**Confidence breakdown:**
- Standard stack (plugins/versions): HIGH — verified via plugins.sbt and Scala.js / sbt-nosbt docs
- scalac options migration: HIGH — verified per-flag against official Scala 3 lookup table
- Module inventory: HIGH — counted via `git ls-tree` + `git grep` on the exact baseline commit
- Compile-broken estimates: MEDIUM — counts are exact; "what compiles after commenting" is best-effort until actually run
- ScalaJS Scala 3: HIGH — sbt-scalajs 1.21.0 confirmed current and Scala 3 supported
- `made` 0.1.1 resolvability: HIGH — verified via cellar against Sonatype Central

**Research date:** 2026-06-01
**Valid until:** 2026-07-01 (30 days; build infra is stable, Scala 3.8 cycle settled)
