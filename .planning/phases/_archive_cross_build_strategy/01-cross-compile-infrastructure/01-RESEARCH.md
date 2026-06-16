# Phase 1: Cross-compile infrastructure - Research

**Researched:** 2026-05-30
**Domain:** sbt build infrastructure for Scala 2.13 + Scala 3 cross-compilation, CI automation via sbt-github-actions, scalafmt dialects, MiMa
**Confidence:** HIGH

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

**`made` library:**
- Pin `madeVersion := "0.1.0"` (published release on Sonatype Central as `io.github.halotukozak:made`).
- Master uses `0.1.1-SNAPSHOT` — do **not** carry SNAPSHOT into upstream PR.
- No SNAPSHOT resolver added.

**scalafmt dialects:**
- `runner.dialect = scala3` (default for whole project — Scala 3 sources).
- `fileOverride` for `**/src/{main,test}/scala-2.13/**` and `**/src/{main,test}/scala-2/**`: `runner.dialect = scala213source3`.
- Rationale: 2.13 sources are compiled with `-Xsource:3` so dialect must permit Scala 3 syntax constructs in 2.13 files. Master currently sets `scala213` (plain) — this phase upgrades to `scala213source3` per REQ INFRA-05.
- `allowSignificantIndentation`/`allowFewerBraces` stay enabled for the default `scala3` dialect; disabled in fileOverride for 2.13 dirs (already the master pattern).

**scalafmt version:**
- Accept upstream `scala-3` branch's scalafmt reformat baseline (3.11.1 per PROJECT.md). Phase 1 PR bases on whatever upstream `scala-3` ships and rebases conflicts toward upstream.
- Do not pin back to master's 3.10.4.

**MiMa plugins:**
- Bump `sbt-mima-plugin` to `1.1.5` in `project/plugins.sbt`.
- **Defer** `sbt-tasty-mima` — drift from REQ INFRA-04 acknowledged; tasty-mima added later (Phase 11 / v2 RELEASE-01).
- `tastyMiMaPreviousArtifacts` stays empty (no Scala 3 baseline exists yet).
- `previousCompatibleVersions` Set untouched (2.13 baseline only).

**CI workflow:**
- Keep `sbt-github-actions` autogen — do **not** hand-author `.github/workflows/ci.yml`.
- Configure sbt build keys (`githubWorkflowBuildPreamble`, `githubWorkflowBuild`, `githubWorkflowJavaVersions`, etc.) so the regenerated `ci.yml` runs the 5 gate commands per REQ INFRA-07: `+jvm/test`, `+jvm2/test`, `+js/test`, `++2.13 mimaReportBinaryIssues`, `scalafmtCheckAll`.
- Java 17 only (no 21/25 — deferred to v2 CI-01).
- Generated `ci.yml` committed in the PR (per sbt-github-actions contract).

**Makefile:** Not included in Phase 1 PR. Skipped per user choice.

**Aggregate split:**
- `jvm` aggregate: cross-built modules (everything except 2.13-only ones).
- `jvm2` aggregate: 2.13-only stranded modules. Phase 1 adds only `jetty` to `jvm2` (matches master state).
- `analyzer`, `spring`, RPC: stay commented-out in build.sbt as on master.
- Rationale for split: default `sbt test` runs `jvm/test` — 2.13-only modules in a separate aggregate prevent `++3.8.2 jvm/test` from failing on jetty.

**Cross-build configuration:**
- `crossScalaVersions := Seq(scala3Version, scala2Version)` on every cross-built module.
- `scala2Version = "2.13.18"`, `scala3Version = "3.8.2"`.
- `scalaVersion := scala3Version` at root.

**Source layout:**
- Use sbt built-in cross-version source resolution via `unmanagedSourceDirectories ++= …`.
- Directories recognized: `scala/` (shared), `scala-2.13/` (2.13 only), `scala-3/` (Scala 3 only).
- **No custom `mkSourceDirs` helper.** Use idiomatic sbt-built-in approach.
- Test counterpart wired identically.

**Source files in this PR:** Zero source files. Pure infra slice. Scala 3 modules compile to empty jars. No `scala-3/` directories created. No `.gitkeep` placeholders.

**Acceptance on Scala 3 side:**
- `++3.8.2 jvm/compile` green.
- `++3.8.2 jvm/test` green (test compile may be no-op).
- `+jvm/test` (cross both) green on 2.13; Scala 3 side compiles whatever exists.

### Claude's Discretion

- Exact wiring of `unmanagedSourceDirectories` for the "idiomatic sbt-built-in approach" replacing `mkSourceDirs` (CONTEXT explicitly says no custom helper, but specific shape is researcher/planner choice — see "Architecture Patterns" below).
- How to fit the 5 gate commands into `githubWorkflowBuild` (single `WorkflowStep.Sbt` with command list vs multiple steps).
- Whether to preserve upstream/scala-3's `ProjectGroup`/`sbt-nosbt` structure or revert to fork master's inline `build.sbt`. See "Critical Constraint" below — strong recommendation to preserve upstream structure.

### Deferred Ideas (OUT OF SCOPE)

- `sbt-tasty-mima` 1.4.0 plugin staging — deferred to Phase 11 / v2 RELEASE-01.
- `analyzer` / `spring` / RPC reactivation under `jvm2` — Phase 12 formalization.
- `Makefile` with `make ci` target.
- Java 21/25 CI matrix — v2 CI-01.
- Scala 3 LTS (3.3.x) vs current (3.8.x) decision — first `_3` release.
- `tastyMiMaPreviousArtifacts` activation — first `_3` release.
- Scalac warning flag harmonization between 2.13 and 3 — out of Phase 1 scope.

</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| INFRA-01 | `crossScalaVersions := [2.13.x, 3.8.x]` on every cross-built module | "Cross-build configuration" pattern below; per-module `crossScalaVersions := Seq(scala3Version, scala2Version)` |
| INFRA-02 | `jvm` (cross-built) + `jvm2` (2.13-only) aggregates in build.sbt | Existing fork-master pattern reused; upstream/scala-3 currently has only single `jvm` aggregate — must add `jvm2` |
| INFRA-03 | Cross-version source layout (`scala/`, `scala-2.13/`, `scala-3/`) | sbt built-in `unmanagedSourceDirectories` pattern (Section: Architecture Patterns / "Cross-version source dirs") |
| INFRA-04 | `sbt-mima-plugin` 1.1.5 (sbt-tasty-mima deferred) | Bump in `project/plugins.sbt` from upstream's `1.1.4` to `1.1.5` — verified release 2025-02-17 |
| INFRA-05 | scalafmt `fileOverride` covers `scala-2.13/` with `scala213source3` dialect; rest uses `scala3` | scalafmt 3.x supports `scala213source3` dialect; upstream's `.scalafmt.conf` uses `Scala213Source3` globally — Phase 1 inverts: default `scala3`, override `scala-2.13/` to `scala213source3` |
| INFRA-06 | `made` 0.1.0 dependency on Scala 3 side only | Conditional `libraryDependencies` keyed on `scalaBinaryVersion.value == "3"` |
| INFRA-07 | CI matrix runs 5 gate commands on Java 17 only | `githubWorkflowBuild` override + `githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))` |
| INFRA-08 | First PR green on Scala 2.13; Scala 3 compiles (empty jars OK) | Zero-source slice — sbt resolves `scala-3/` dirs lazily; empty jars are valid |
| INFRA-09 | jetty/analyzer/spring/RPC excluded from `jvm` aggregate | `jetty` lives under `jvm2`; analyzer/spring/RPC stay commented out |
| WORKFLOW-01 | Branch off latest `upstream/scala-3` | Documented in CONTEXT specifics — execution constraint, not build code |
| WORKFLOW-02 | PR targets `AVSystem/scala-commons:scala-3` | Execution constraint |
| WORKFLOW-03 | User ack before push AND before PR open | Execution constraint |
| WORKFLOW-04 | No GSD nomenclature in commit messages | Commit prefixes: `build:`, `ci:`, `style(scalafmt):` |
| WORKFLOW-05 | `.planning/` never in commit diff | `.gitignore` already excludes; verify before commit |
| QUALITY-01 | No new `@nowarn` / `-Wconf` | Build infra only — no source means no warnings |
| QUALITY-03 | MiMa filters carry per-filter justification comment | Existing `coreMimaFilters` in master already has per-filter comments — preserve pattern |

</phase_requirements>

## Summary

Phase 1 lands the cross-compile **build infrastructure** onto `upstream/scala-3` as a pure-infra slice (no source ports). The most important technical finding is that **upstream/scala-3 uses a fundamentally different build organization than fork master**: upstream defines the build in `project/Commons.scala` via `sbt-nosbt`'s `ProjectGroup` pattern (`mkRootProject`/`mkSubProject`), while fork master inlines everything in `build.sbt` using plain `project` / `lazy val`. Phase 1 must reconcile these two structures.

**Recommended approach:** preserve upstream/scala-3's `ProjectGroup` structure (`project/Commons.scala`) and apply the cross-compile changes inside it. This minimizes diff against upstream and respects the project owner's chosen organization. Fork master's inline `build.sbt` was a regression that should NOT be re-imposed in the migration PR.

The 5 CI gate commands (`+jvm/test`, `+jvm2/test`, `+js/test`, `++2.13 mimaReportBinaryIssues`, `scalafmtCheckAll`) replace upstream's single `test` invocation by overriding `githubWorkflowBuild` with a `WorkflowStep.Sbt` carrying the command list. `sbt-mima-plugin` bumps 1.1.4 → 1.1.5. `scalafmt` inverts dialects: default `scala3`, fileOverride `scala-2.13/` → `scala213source3`.

**Primary recommendation:** edit `project/Commons.scala` (not `build.sbt`); add `jvm2` aggregate alongside the existing `jvm`/`js`; bump plugins; rewrite `.scalafmt.conf` to invert dialect defaults; configure `githubWorkflowBuild` / `githubWorkflowJavaVersions` and regenerate `ci.yml` via `sbt githubWorkflowGenerate`.

## Critical Constraint: Upstream Build Structure

| Aspect | Fork master (current HEAD) | upstream/scala-3 (1561d8dc) | Phase 1 decision |
|--------|---------------------------|------------------------------|------------------|
| Build location | `build.sbt` (~500 lines inline) | `project/Commons.scala` via `ProjectGroup` | **Preserve upstream's `project/Commons.scala`** |
| Root project | `lazy val root = project.in(file("."))` | `lazy val root = mkRootProject` | Preserve `mkRootProject` |
| Subprojects | `lazy val foo = project` | `lazy val foo = mkSubProject` | Preserve `mkSubProject` |
| `build.sbt` content | Entire build | `lazy val root = Commons.root` | Preserve — single-line build.sbt |
| sbt version | 1.12.10 | 1.12.11 | Use 1.12.11 (upstream wins) |
| `sbt-nosbt` plugin | Not present | `0.2.1` | Keep `sbt-nosbt` in plugins.sbt |
| sbt-github-actions | 0.29.0 | 0.30.0 | Use 0.30.0 (upstream wins) |
| scalafmt plugin | 2.6.0 | 2.6.1 | Use 2.6.1 (upstream wins) |
| sbt-mima-plugin | 1.1.4 | 1.1.4 | Bump to **1.1.5** per REQ INFRA-04 |

**Why this matters:** if the implementer copies fork master's inline `build.sbt`, the PR will be a massive diff against upstream (reverting `ProjectGroup` refactor) and almost certainly rejected. The implementer MUST edit `project/Commons.scala` and add new modules / settings inside `Commons extends ProjectGroup("commons")`.

## Standard Stack

### Core (sbt plugins — already on upstream/scala-3)

| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| `sbt` | 1.12.11 | Build tool | Project pin via `project/build.properties` |
| `sbt-nosbt` | 0.2.1 | Move build def to Scala files via `ProjectGroup` | Project owner's choice on upstream — preserve |
| `sbt-scalajs` | 1.21.0 | Scala.js cross-build | Required for `core-js` / `mongo-js` |
| `sbt-jsdependencies` | 1.0.2 | JS deps for Scala.js | Used by benchmark-js etc. |
| `sbt-github-actions` | 0.30.0 | Generate `.github/workflows/ci.yml` | Project owner's choice; editing ci.yml by hand forbidden |
| `sbt-scalafmt` | 2.6.1 | Code formatting | Enforced in CI via `scalafmtCheckAll` |
| `sbt-mima-plugin` | **1.1.5** (bump) | Binary compatibility | Project owner uses MiMa; bumped per INFRA-04 |
| `sbt-ci-release` | 1.11.2 | Publish to Sonatype | Existing release flow |
| `sbt-unidoc` | 0.6.1 | Aggregated scaladoc | Existing |
| `sbt-jmh` | 0.4.8 | Benchmarks | Existing |
| `sbt-updates` | 0.6.4 | Dep update checks | Existing |
| `sbt-ide-settings` | 1.1.4 | IntelliJ integration | Existing |

### Supporting

| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `made` | `0.1.0` (Scala 3 only) | Annotation / derivation helpers | Conditional dep when `scalaBinaryVersion.value == "3"` |

### Scala versions

| Var | Value |
|-----|-------|
| `scala2Version` | `"2.13.18"` |
| `scala3Version` | `"3.8.2"` |
| `scalaVersion` (root) | `scala3Version` |
| `crossScalaVersions` (per module) | `Seq(scala3Version, scala2Version)` for cross-built modules; `Seq(scala2Version)` for `jetty` |

### Alternatives Considered

| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `ProjectGroup` / `sbt-nosbt` | Inline `build.sbt` (fork master) | Inline is simpler but contradicts upstream — would inflate PR diff and fail review |
| Per-module `crossScalaVersions` | `ThisBuild / crossScalaVersions` | ThisBuild leaks to `jetty` (which must stay 2.13-only); per-module is correct |
| `sbt-projectmatrix` | Cross-build via `crossScalaVersions` + `++` | Archived April 2025; sbt 2.x in-sources it. Out of scope per REQUIREMENTS.md |
| `sbt-tasty-mima` 1.4.0 staged now | Add it now | Deferred to Phase 11 — no Scala 3 baseline yet |

**Installation:** no `npm install`; sbt resolves plugins at startup.

**Version verification:**
- `sbt-mima-plugin` 1.1.5 — verified via GitHub releases (published 2025-02-17). Source: https://github.com/lightbend-labs/mima/releases.
- `sbt-github-actions` 0.30.0 — verified latest (2026-05-10). Source: https://github.com/sbt/sbt-github-actions.
- `sbt-nosbt` 0.2.1 — verified latest (2023-03-19). Source: https://github.com/ghik/sbt-nosbt.
- `io.github.halotukozak:made_3:0.1.0` — verified resolvable via `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` (resolved successfully).

## Architecture Patterns

### Recommended Project Structure

```
.                       # root project (mkRootProject) aggregates jvm, jvm2, js
├── build.sbt           # one line: lazy val root = Commons.root
├── project/
│   ├── Commons.scala   # ProjectGroup — all build logic lives here
│   ├── plugins.sbt     # sbt plugins
│   └── build.properties # sbt.version=1.12.11
├── .scalafmt.conf      # scala3 default + scala-2.13/ override
├── .github/workflows/
│   └── ci.yml          # AUTO-GENERATED; commit but never hand-edit
├── core/               # cross-built (Scala 2.13 + 3)
├── core/js/            # core-js Scala.js variant
├── macros/             # cross-built, empty Scala 3 jar
├── hocon/              # cross-built
├── mongo/, mongo/js    # cross-built (later phases)
├── jetty/              # 2.13 ONLY — under jvm2 aggregate
└── ...
```

Per-module source layout (recognized by sbt cross-version helpers):
```
<module>/src/{main,test}/
├── scala/        # shared between Scala 2.13 and 3
├── scala-2.13/   # 2.13 only
├── scala-3/      # Scala 3 only (created lazily as files arrive — NOT in Phase 1)
└── java/         # Java sources
```

### Pattern 1: Cross-version source dirs (no custom helper)

**What:** Wire `scala-2.13/` and `scala-3/` directories via sbt's standard `unmanagedSourceDirectories` API. Avoid the `mkSourceDirs(base, scalaBinary, conf)` helper — sbt-cross-version selection is the idiomatic approach.

**When to use:** every cross-built module.

**Example (recommended idiomatic pattern):**

```scala
// In Commons.scala (or build.sbt). Helper kept local since it's pure sbt API.
def crossVersionSourceSettings: Seq[Setting[?]] = Seq(
  Compile / unmanagedSourceDirectories ++= {
    val base = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(base / "scala-2.13", base / "scala-2")
      case Some((3, _)) => Seq(base / "scala-3")
      case _            => Seq.empty
    }
  },
  Test / unmanagedSourceDirectories ++= {
    val base = (Test / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(base / "scala-2.13", base / "scala-2")
      case Some((3, _)) => Seq(base / "scala-3")
      case _            => Seq.empty
    }
  },
)
```

**Why this is idiomatic:** uses `sourceDirectory` task (sbt-provided) and `CrossVersion.partialVersion` (sbt-provided). No custom `File => File` mapper. Matches what sbt itself does for `scala-2.13` resolution under the hood.

**Source:** sbt cross-build manual https://www.scala-sbt.org/1.x/docs/Cross-Build.html

### Pattern 2: ProjectGroup aggregate split (jvm / jvm2 / js)

```scala
// in object Commons extends ProjectGroup("commons")
lazy val root = mkRootProject
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(jvm, jvm2, js)
  .settings(noPublishSettings, name := "commons", /* ... */)

lazy val jvm = mkSubProject
  .in(file(".jvm"))
  .aggregate(macros, core, hocon /* ... cross-built modules ... */)
  .settings(aggregateProjectSettings)

// 2.13-only stranded modules; default Scala 3 build never tries to resolve them
lazy val jvm2 = mkSubProject
  .in(file(".jvm2"))
  .aggregate(jetty)
  .settings(aggregateProjectSettings)

lazy val js = mkSubProject
  .in(file(".js"))
  .aggregate(`core-js`)
  .settings(aggregateProjectSettings)
```

### Pattern 3: 2.13-only module under jvm2 (`jetty` template)

```scala
lazy val jetty = mkSubProject
  .dependsOn(core % CompileAndTest)
  .settings(
    jvmCommonSettings,
    crossScalaVersions := Seq(scala2Version),
    scalaVersion := scala2Version,
    // Prevent sbt's Smorrebrod resolver from pulling Scala 3 transitive deps
    // when the active version is 3.x.
    update / skip := scalaBinaryVersion.value != "2.13",
    Compile / skip := scalaBinaryVersion.value != "2.13",
    Test / skip := scalaBinaryVersion.value != "2.13",
    publish / skip := scalaBinaryVersion.value != "2.13",
    libraryDependencies ++= Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyVersion,
      "org.eclipse.jetty.ee10" % "jetty-ee10-servlet" % jettyVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
    ),
  )
```

### Pattern 4: Conditional `made` dependency (Scala 3 only)

```scala
libraryDependencies ++= {
  if (scalaBinaryVersion.value == "3")
    Seq("io.github.halotukozak" %% "made" % madeVersion)
  else Seq.empty
},
```

`%%` (single percent) correctly appends `_3` to give `io.github.halotukozak:made_3:0.1.0`. Verified via `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default`.

### Pattern 5: sbt-github-actions custom build step (5 gate commands)

```scala
// In Commons.scala buildSettings:
githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    List(
      "+jvm/test",
      "+jvm2/test",
      "+js/test",
      "++2.13 mimaReportBinaryIssues",
      "scalafmtCheckAll",
    ),
    name = Some("Run CI gate"),
  ),
),
```

**Important:**
- `+` (no number) cross-builds across all `crossScalaVersions` for the aggregate.
- `++2.13 mimaReportBinaryIssues` pins to Scala 2.13 because there is no Scala 3 MiMa baseline yet.
- Upstream's existing `githubWorkflowAddedJobs` for `mima` and `scalafmt` (separate jobs on Java 21) should be REMOVED in Phase 1 — they're folded into the single build step per REQ INFRA-07.
- **OR** alternative: keep separate added jobs but pin all to Java 17 only and override the build step to remove `mima`/`scalafmt` from there. Planner's choice; recommend the single-step approach for minimal CI surface area in Phase 1.

After config changes, run `sbt githubWorkflowGenerate` locally and commit the resulting `ci.yml`.

**Source:** https://github.com/sbt/sbt-github-actions README — `githubWorkflowBuild` defaults to `[sbt test]`, fully overridable.

### Pattern 6: scalafmt dialect inversion

`.scalafmt.conf` rewrite (current upstream/scala-3 has global `Scala213Source3`):

```hocon
version = "3.11.1"
runner.dialect = scala3
runner.dialectOverride.allowSignificantIndentation = true
runner.dialectOverride.allowFewerBraces = true

maxColumn = 120

fileOverride {
  "glob:**/src/{main,test}/scala-2.13/**" {
    runner.dialect = scala213source3
    runner.dialectOverride.allowSignificantIndentation = false
    runner.dialectOverride.allowFewerBraces = false
  }
  "glob:**/src/{main,test}/scala-2/**" {
    runner.dialect = scala213source3
    runner.dialectOverride.allowSignificantIndentation = false
    runner.dialectOverride.allowFewerBraces = false
  }
}

# ... rest of rules preserved from upstream/scala-3 ...
```

Note the dialect token is lowercase `scala213source3` in scalafmt config (case-insensitive but lowercase is canonical).

### Anti-Patterns to Avoid

- **Hand-editing `.github/workflows/ci.yml`** — sbt-github-actions regenerates and overwrites. File header says: "You should add and commit this file to your git repository. It goes without saying that you shouldn't edit this file by hand!" Always edit sbt keys and run `githubWorkflowGenerate`.
- **Reverting `ProjectGroup` refactor** by copying fork master's inline `build.sbt` — see Critical Constraint above.
- **Using `ThisBuild / crossScalaVersions`** — leaks to `jetty` which must stay 2.13-only. Set per-module.
- **Creating empty `scala-3/` directories or `.gitkeep` files** — CONTEXT bans this. sbt resolves dirs lazily; empty Scala 3 jars are valid sbt output.
- **Adding a SNAPSHOT resolver for `made`** — CONTEXT pins `0.1.0` release; no SNAPSHOT.
- **Bumping scalafmt version** away from upstream's `3.11.1` — accept upstream reformat baseline.
- **Adding `@nowarn` or `-Wconf` flags** — QUALITY-01 forbids; no source files anyway.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Cross-version source dir wiring | Custom `mkSourceDirs(base, sv, conf)` helper | sbt's `unmanagedSourceDirectories ++= …` with `CrossVersion.partialVersion` | CONTEXT decision: "No custom `mkSourceDirs` helper. Use idiomatic sbt-built-in approach." |
| CI YAML generation | Hand-author `.github/workflows/ci.yml` | `sbt-github-actions` regenerate | Plugin's contract; PR will be rejected if hand-edited |
| ProjectGroup-style build organization | Inline 500-line `build.sbt` | `sbt-nosbt` `ProjectGroup` in `project/Commons.scala` | Upstream's choice; preserve to minimize diff |
| Scala 3 / 2.13 conditional deps | Multi-project hack with different bases | `if (scalaBinaryVersion.value == "3") Seq(...) else Seq.empty` inside `libraryDependencies ++=` | Standard sbt cross-build idiom |
| Excluding 2.13-only modules from Scala 3 build | `Compile/skip` only | Separate `jvm2` aggregate + `Compile/skip` + `update/skip` | Both layers: aggregate to keep `+jvm/test` clean, skips so `++3 jetty/compile` no-ops |

**Key insight:** every "problem" in this phase has an established sbt/upstream pattern. The discipline is to USE that pattern, not invent.

## Common Pitfalls

### Pitfall 1: Smörrebrod resolver fetches Scala 3 deps for 2.13-only modules

**What goes wrong:** sbt 1.12 ("Smörrebrod" series) eagerly tries to resolve transitive deps for all aggregated modules even when the active Scala version is 3, causing failures for `jetty` (which has no Scala 3 deps).

**Why it happens:** `update` task runs across the whole aggregate; `crossScalaVersions := Seq(scala2Version)` alone is not enough — sbt still attempts resolution.

**How to avoid:** apply the full skip block to `jetty`:
```scala
update / skip := scalaBinaryVersion.value != "2.13",
Compile / skip := scalaBinaryVersion.value != "2.13",
Test / skip := scalaBinaryVersion.value != "2.13",
publish / skip := scalaBinaryVersion.value != "2.13",
```
AND keep it under a separate `jvm2` aggregate so `++3.8.2 jvm/test` never visits it.

**Warning signs:** `++3.8.2 jvm/test` fails with "no Scala 3 version of jetty-client_3 available" or similar coursier resolution error.

### Pitfall 2: scalafmt `scala213source3` dialect mismatch

**What goes wrong:** existing fork-master `.scalafmt.conf` uses `scala213` (plain) for `scala-2.13/` fileOverride. But the 2.13 sources are compiled with `-Xsource:3` and contain Scala-3-flavored syntax (e.g., `using`, `enum`-like shapes). Plain `scala213` dialect rejects these.

**Why it happens:** dialect tracks parser, not compiler flags. `-Xsource:3` is a compiler concept; scalafmt needs the explicit `scala213source3` dialect to parse Scala-3-isms in 2.13 files.

**How to avoid:** use `scala213source3` (not `scala213`) in fileOverride. Per CONTEXT decision and REQ INFRA-05.

**Warning signs:** `scalafmtCheckAll` fails with parser errors on `using`-bearing 2.13 files.

### Pitfall 3: scalafmt 3.11.1 reformat causes massive merge conflicts

**What goes wrong:** upstream/scala-3 bumped scalafmt to 3.11.1 with a sweeping reformat. Any Phase 1 change to scala source (none expected here) or scalafmt config produces conflicts.

**Why it happens:** scalafmt 3.10 → 3.11 changed defaults; whole tree was reformatted on upstream.

**How to avoid:** Phase 1 touches zero scala source files; only `.scalafmt.conf`, build files, plugins. Conflicts in `.scalafmt.conf` resolve by taking the new upstream version as the base, then editing in the `fileOverride` dialect changes.

**Warning signs:** `git merge upstream/scala-3` shows conflicts in .scala files (shouldn't happen if you started from `upstream/scala-3` HEAD per CONTEXT specifics).

### Pitfall 4: `sbt-github-actions` "Check that workflows are up to date" CI step

**What goes wrong:** sbt-github-actions adds a `githubWorkflowCheck` step that fails the build if the checked-in `ci.yml` doesn't match what `githubWorkflowGenerate` would produce.

**Why it happens:** plugin's contract — file is authoritative-from-sbt-config.

**How to avoid:** ALWAYS run `sbt githubWorkflowGenerate` after any change to `githubWorkflow*` keys, then commit the regenerated `ci.yml` in the same commit.

**Warning signs:** CI fails on "Check that workflows are up to date" before any build step runs.

### Pitfall 5: `+` cross-build vs explicit `++` pin

**What goes wrong:** confusion between `+jvm/test` (run jvm/test once per `crossScalaVersions`) and `++2.13 mimaReportBinaryIssues` (set version then run once). Mixing them in one sbt invocation requires correct ordering.

**Why it happens:** `++<version>` is a "set version, then run task" semicolon-equivalent; `+<task>` is "cross-run task".

**How to avoid:** order in `githubWorkflowBuild` matters when commands share state. `++2.13 mimaReportBinaryIssues` after `+jvm/test` is safe (just pins version). Verify locally before push.

**Warning signs:** `mimaReportBinaryIssues` runs against the wrong Scala version (Scala 3 — no baseline → no-op or false green).

### Pitfall 6: `made` library not findable on Maven Central search

**What goes wrong:** `search.maven.org` returns 0 results for `io.github.halotukozak`. Implementer worries the dep won't resolve.

**Why it happens:** publication via Sonatype Central (s01.oss.sonatype.org → maven central) sometimes lags the search index, or the artifact is under the newer Sonatype Central namespace.

**How to avoid:** trust resolution, not the search UI. Verified via `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` — artifact is present in the canonical Maven repo. sbt's coursier resolver will find it.

**Warning signs:** if `+jvm/update` actually fails on `made`, add explicit resolver — but expected to "just work."

### Pitfall 7: `previousCompatibleVersions` includes versions newer than supported

**What goes wrong:** fork master's `previousCompatibleVersions` ends at `2.27.1`. Upstream/scala-3's `Commons.scala` has the same set. MiMa check runs against artifacts that may have signatures the current code intentionally changes.

**Why it happens:** MiMa filters (`coreMimaFilters`, mongo filters in upstream) document the breaking changes.

**How to avoid:** Phase 1 doesn't change source — MiMa should still pass with existing filters. Don't add to `previousCompatibleVersions`; don't remove from it. Per CONTEXT: "`previousCompatibleVersions` Set untouched."

**Warning signs:** `++2.13 mimaReportBinaryIssues` fails with new problems — would indicate accidental source-level change snuck in.

## Code Examples

### Example 1: project/plugins.sbt (Phase 1 target)

```scala
// Source: derived from upstream/scala-3:project/plugins.sbt + REQ INFRA-04 bump
logLevel := Level.Warn

addSbtPlugin("com.github.ghik" % "sbt-nosbt" % "0.2.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.21.0")
addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.2")
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.30.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")  // BUMP from 1.1.4
```

### Example 2: project/Commons.scala — Phase 1 buildSettings additions

```scala
// Source: derived from upstream/scala-3:project/Commons.scala + CONTEXT decisions
override def buildSettings: Seq[Def.Setting[_]] = Seq(
  // ... existing organization/homepage/etc ...
  scalaVersion := scala3Version,                                  // CHANGE from "2.13.18"
  crossScalaVersions := Seq(scala3Version, scala2Version),        // ADD at ThisBuild for default
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowArtifactUpload := false,
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),      // CHANGE: drop 21, 25
  githubWorkflowEnv += "JAVA_OPTS" -> "-Dfile.encoding=UTF-8 -Xmx4G",
  githubWorkflowBuildMatrixFailFast := Some(false),
  // REMOVE: githubWorkflowAddedJobs for mima and scalafmt (folded into build step)
  githubWorkflowBuild := Seq(
    WorkflowStep.Sbt(
      List(
        "+jvm/test",
        "+jvm2/test",
        "+js/test",
        "++2.13 mimaReportBinaryIssues",
        "scalafmtCheckAll",
      ),
      name = Some("Run CI gate"),
    ),
  ),
  githubWorkflowBuildPreamble ++= Seq(/* Node.js, MongoDB — keep as upstream */),
  // ... rest preserved ...
)
```

**Note on `crossScalaVersions` at ThisBuild:** sbt-github-actions reads `ThisBuild / crossScalaVersions` to populate the matrix. CONTEXT decision is per-module crossScalaVersions, BUT the GH Actions matrix wants ThisBuild. Resolution: set BOTH — `ThisBuild / crossScalaVersions := Seq(scala3Version, scala2Version)` for matrix discovery, AND per-module overrides for `jetty` (which sets `Seq(scala2Version)`). The matrix from sbt-github-actions reflects the cross-built happy path; jetty's exclusion is enforced by `update/skip` + `Compile/skip` and by being under `jvm2`. (Confirmed pattern in fork master `build.sbt`.)

### Example 3: project/Commons.scala — aggregate definitions

```scala
// Add jvm2 alongside existing jvm/js. Drop analyzer/spring from jvm
// per CONTEXT (commented out on fork master).
lazy val root = mkRootProject
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(jvm, jvm2, js)
  .settings(
    noPublishSettings,
    name := "commons",
    ideExcludedDirectories := Seq(baseDirectory.value / ".bloop"),
    ScalaUnidoc / unidoc / scalacOptions += "-Ymacro-expand:none",
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      `core-js`,
    ),
  )

lazy val jvm = mkSubProject
  .in(file(".jvm"))
  .aggregate(
    macros,
    core,
    hocon,
  )
  .settings(aggregateProjectSettings)

lazy val jvm2 = mkSubProject
  .in(file(".jvm2"))
  .aggregate(jetty)
  .settings(aggregateProjectSettings)

lazy val js = mkSubProject
  .in(file(".js"))
  .aggregate(
    `core-js`,
  )
  .settings(aggregateProjectSettings)
```

### Example 4: Per-module crossScalaVersions wiring

```scala
// macros — cross-built, empty Scala 3 jar in Phase 1
lazy val macros = mkSubProject.settings(
  jvmCommonSettings,
  crossScalaVersions := Seq(scala3Version, scala2Version),
  scalaVersion := scala3Version,
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.13")
      Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    else Seq.empty
  },
  mimaPreviousArtifacts := Set.empty,
)

// core — cross-built; made on Scala 3 only
lazy val core = mkSubProject
  .dependsOn(macros)
  .settings(
    jvmCommonSettings,
    crossScalaVersions := Seq(scala3Version, scala2Version),
    scalaVersion := scala3Version,
    crossVersionSourceSettings,                  // ← idiomatic helper, not mkSourceDirs
    sourceDirsSettings(_ / "jvm"),               // ← or keep mkSourceDirs/sourceDirsSettings as in upstream
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion % Optional,
      "io.monix" %% "monix" % monixVersion % Optional,
    ),
    libraryDependencies ++= {
      if (scalaBinaryVersion.value == "3")
        Seq("io.github.halotukozak" %% "made" % madeVersion)
      else Seq.empty
    },
    mimaBinaryIssueFilters ++= coreMimaFilters,
  )
```

**Decision needed by planner:** CONTEXT says "no custom `mkSourceDirs`". Upstream's `Commons.scala` HAS a `mkSourceDirs` helper. Two options:
1. **Strict CONTEXT interpretation:** replace upstream's `mkSourceDirs`/`sourceDirsSettings` with the `CrossVersion.partialVersion` pattern shown in Pattern 1. Bigger diff, cleaner long-term.
2. **Pragmatic:** preserve upstream's `mkSourceDirs` helper (already there, already idiomatic enough). CONTEXT's "no custom helper" instruction was written assuming fork master state — upstream already has the helper inline in Commons.scala.

Recommend option 2 (preserve upstream helper). Surface to user during planning.

### Example 5: .scalafmt.conf (Phase 1 target)

See Pattern 6 above. The minimal diff against upstream/scala-3 is:
- Top-level `runner.dialect`: `Scala213Source3` → `scala3`
- Add `fileOverride` block for `scala-2.13/` and `scala-2/` paths pinning to `scala213source3`
- Re-add `runner.dialectOverride.allowSignificantIndentation`/`allowFewerBraces` toggles (upstream removed them when going whole-tree 2.13source3)

### Example 6: madeVersion declaration

```scala
// In Commons.scala, alongside guavaVersion/scalatestVersion/etc:
val madeVersion = "0.1.0"   // pinned release (Sonatype Central); NOT 0.1.1-SNAPSHOT
val scala2Version = "2.13.18"
val scala3Version = "3.8.2"
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `sbt-projectmatrix` for cross-build | `crossScalaVersions` + `+` invocation | Plugin archived April 2025 | Don't add sbt-projectmatrix; use sbt-built-in cross |
| Hand-authored `ci.yml` | `sbt-github-actions` regenerates | Long-established | Edit sbt keys, run `githubWorkflowGenerate` |
| `-Ytasty-reader` flag on 2.13 | Same flag still required when 2.13 consumes Scala 3 artifacts | Stable since 2.13.5 | Keep in 2.13 scalacOptions (already present) |
| Inline `build.sbt` | `ProjectGroup` via `sbt-nosbt` in `project/*.scala` | Upstream choice 2024+ | Preserve upstream layout |
| `scala213` scalafmt dialect for `-Xsource:3` code | `scala213source3` | scalafmt 3.x | Use `scala213source3` in fileOverride |

**Deprecated/outdated:**
- `sbt-projectmatrix` — archived April 2025; sbt 2.x in-sources. Out of scope per REQUIREMENTS.md.
- Plain `scala213` dialect for `-Xsource:3` files — superseded by `scala213source3`.
- Java < 17 — no longer in matrix (was already 17+).

## Open Questions

1. **Strict vs pragmatic interpretation of "no custom `mkSourceDirs` helper".**
   - What we know: CONTEXT explicitly bans the helper. Upstream/scala-3 already has it.
   - What's unclear: did user intend to ban it relative to fork master (where it lives in build.sbt) or universally (including the inherited upstream version)?
   - Recommendation: preserve upstream's existing helper in Commons.scala (it's already idiomatic-ish and removing it is a bigger diff). Confirm with user during /gsd:plan-phase review.

2. **Folding `mima`/`scalafmt` from `githubWorkflowAddedJobs` into the single build step.**
   - What we know: upstream/scala-3 has separate added jobs for mima (Java 21) and scalafmt (Java 21).
   - What's unclear: REQ INFRA-07 specifies one matrix on Java 17 — does this mean removing the added jobs entirely, or moving them to Java 17?
   - Recommendation: remove the added jobs; fold all 5 commands into `githubWorkflowBuild` per REQ INFRA-07 literal reading. Minimum CI surface for Phase 1.

3. **`benchmark` / `benchmark-js` / `comprof` modules under upstream/scala-3 but absent from `jvm`/`js` aggregates.**
   - What we know: upstream Commons.scala defines `benchmark`, `benchmark-js`, `comprof` but the root aggregates only `jvm`, `js`. They live as orphan top-level projects.
   - What's unclear: do they need crossScalaVersions wiring in Phase 1, or stay 2.13-only orphans?
   - Recommendation: per CONTEXT "No source ports — only build infra", leave them as-is in upstream Commons.scala. Phase 10 (BENCH-01) handles `benchmark3` cross-build.

4. **`previousCompatibleVersions` Set membership: keep `2.27.1` or trim?**
   - What we know: CONTEXT says "Set untouched".
   - What's unclear: nothing — directive is clear.
   - Recommendation: copy upstream's Set verbatim, do not modify.

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | sbt task graph — no separate test framework needed for Phase 1 |
| Config file | `project/Commons.scala`, `.scalafmt.conf`, `project/plugins.sbt`, `.github/workflows/ci.yml` |
| Quick run command | `sbt 'scalafmtCheckAll'` |
| Full suite command | `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll` |

Phase 1 is **infrastructure-only**; "tests" mean: does the build resolve, compile (empty jars OK), pass MiMa, and pass scalafmt? There are no unit tests to write because there are no source files to test.

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|--------------|
| INFRA-01 | `crossScalaVersions` includes 2.13 and 3.8 on cross-built modules | sbt-inspect | `sbt 'show core/crossScalaVersions'` returns `List(3.8.2, 2.13.18)` | ✅ (existing modules) |
| INFRA-02 | `jvm` and `jvm2` aggregates exist | sbt-inspect | `sbt 'projects'` lists `jvm`, `jvm2`, `js` | ✅ |
| INFRA-03 | Cross-version source dirs wired | sbt-inspect | `sbt '++3.8.2; show core/Compile/unmanagedSourceDirectories'` includes path ending in `scala-3`; `++2.13.18` includes `scala-2.13` | ✅ |
| INFRA-04 | sbt-mima-plugin at 1.1.5 | grep | `grep '"sbt-mima-plugin" % "1.1.5"' project/plugins.sbt` | ✅ |
| INFRA-05 | scalafmt dialect override applied | scalafmtCheckAll | `sbt scalafmtCheckAll` passes with no parser errors | ✅ (`.scalafmt.conf`) |
| INFRA-06 | `made` 0.1.0 resolves on Scala 3 | sbt resolve | `sbt '++3.8.2; core/update'` succeeds; `dependencyTree` shows `made_3:0.1.0` | ✅ (existing) |
| INFRA-07 | CI runs 5 gate commands on Java 17 | YAML grep | `grep -A2 'Run CI gate' .github/workflows/ci.yml` shows all 5 commands; `grep 'java-version: 17' ci.yml` present; `21`/`25` absent | ✅ (generated `ci.yml`) |
| INFRA-08 | Scala 3 side compiles (empty jar OK), 2.13 green | sbt build | `sbt '+jvm/test'` succeeds for both versions; `sbt '++3.8.2; jvm/compile'` succeeds with empty jars on Scala 3 | ✅ |
| INFRA-09 | jetty under jvm2; analyzer/spring/RPC excluded from jvm | sbt-inspect | `sbt 'show jvm/aggregateProjects'` does NOT include `jetty`/`analyzer`/`spring`; `sbt 'show jvm2/aggregateProjects'` includes `jetty` | ✅ |
| WORKFLOW-01..03 | Branch / target / ack | manual | Procedural — user verifies pre-push | n/a |
| WORKFLOW-04 | No GSD nomenclature in commits | grep | `git log upstream/scala-3..HEAD --format=%B \| grep -iE 'gsd\|phase \[0-9]\|plan-phase'` returns nothing | n/a |
| WORKFLOW-05 | `.planning/` not in diff | git | `git diff upstream/scala-3..HEAD --name-only \| grep '^\.planning'` returns nothing | n/a |
| QUALITY-01 | No new `@nowarn` / `-Wconf` | grep | `git diff upstream/scala-3..HEAD \| grep -E '^\+.*(@nowarn\|-Wconf)'` returns nothing | n/a |
| QUALITY-03 | MiMa filters carry justification | grep | every `exclude[...]` line in Commons.scala has a preceding `//` comment with rationale | ✅ (preserve upstream comments) |

### Sampling Rate

- **Per task commit:** `sbt scalafmtCheckAll` (fast, ~5s)
- **Per wave merge:** `sbt 'scalafmtCheckAll' '+jvm/compile' '++3.8.2; jvm/compile'` (~30-60s)
- **Phase gate (before `/gsd:verify-work`):** Full 5-command suite — `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll`

### Wave 0 Gaps

- [ ] `project/Commons.scala` — needs Phase 1 edits (jvm2 aggregate, crossScalaVersions per module, githubWorkflowBuild override, Java 17 only, made dep wiring, scalaVersion → scala3Version, mkRootProject/mkSubProject preserved)
- [ ] `project/plugins.sbt` — needs sbt-mima-plugin 1.1.4 → 1.1.5 bump
- [ ] `.scalafmt.conf` — needs dialect inversion (scala3 default + scala213source3 fileOverride)
- [ ] `.github/workflows/ci.yml` — regenerated by `sbt githubWorkflowGenerate` after Commons.scala edits, committed
- [ ] No new test files needed — Phase 1 is build-infra-only; validation is via sbt task execution against the existing module skeleton

## Sources

### Primary (HIGH confidence)

- `upstream/scala-3` HEAD `1561d8dc`:
  - `project/Commons.scala` — full ProjectGroup-based build definition
  - `project/plugins.sbt` — current plugin versions
  - `project/build.properties` — sbt 1.12.11
  - `.scalafmt.conf` — current scalafmt config
  - `.github/workflows/ci.yml` — generated workflow (reference, not edit target)
  - `build.sbt` — one-line `lazy val root = Commons.root`
- Fork `master`:
  - `build.sbt` — inline build (reference for jvm2, crossScalaVersions per module, jetty skip block, made conditional dep, sourceDirsSettings/mkSourceDirs)
  - `project/plugins.sbt` — reference (older versions)
  - `.scalafmt.conf` — reference (different dialect strategy)
- `.planning/REQUIREMENTS.md` §Build Infrastructure — INFRA-01..09
- `.planning/ROADMAP.md` §Phase 1 — success criteria
- `.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md` — user decisions
- sbt cross-build manual https://www.scala-sbt.org/1.x/docs/Cross-Build.html
- sbt-github-actions README https://github.com/sbt/sbt-github-actions
- sbt-mima-plugin 1.1.5 release https://github.com/lightbend-labs/mima/releases (verified 2025-02-17)
- sbt-nosbt 0.2.1 https://github.com/ghik/sbt-nosbt
- `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` — verified artifact resolution

### Secondary (MEDIUM confidence)

- General Scala community knowledge re: `scala213source3` dialect, `+` vs `++` sbt cross-build invocation semantics (confirmed via official sbt docs).

### Tertiary (LOW confidence)

- None for this phase. All findings are HIGH-MEDIUM.

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — versions verified directly against upstream/scala-3 HEAD + Maven Central / GitHub releases
- Architecture: HIGH — patterns directly observable in fork master and upstream/scala-3 source files
- Pitfalls: HIGH — derived from concrete code (jetty skip block, scalafmt dialect, sbt-github-actions plugin contract)

**Research date:** 2026-05-30
**Valid until:** 2026-06-29 (30 days — stable build infra domain)
