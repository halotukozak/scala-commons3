---
phase: 01-big-bang-comment-and-green
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - project/Commons.scala
  - .scalafmt.conf
  - .github/workflows/ci.yml
  - build.sbt
autonomous: true
commit_docs: false
requirements: [BUILD-01, BUILD-02, BUILD-03, BUILD-04, BUILD-05, QUALITY-01, QUALITY-02, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05]

must_haves:
  truths:
    - "Branch 01-big-bang cut from upstream/scala-3 @ 1561d8dc exists locally"
    - "sbt loads on Scala 3.8.2 (single axis, no crossScalaVersions)"
    - ".scalafmt.conf uses single scala3 dialect, no fileOverride"
    - ".github/workflows/ci.yml runs single Scala 3 axis x Temurin 17/21/25"
    - "No scala-2.13/ source dirs exist (none in upstream baseline)"
  artifacts:
    - path: "project/Commons.scala"
      provides: "Scala 3 only build config; madeVersion=0.1.1 unconditional"
      contains: "scala3Version"
    - path: ".scalafmt.conf"
      provides: "single scala3 dialect"
      contains: "runner.dialect = scala3"
    - path: ".github/workflows/ci.yml"
      provides: "single Scala 3 axis CI"
      contains: "3.8.2"
  key_links:
    - from: "project/Commons.scala"
      to: "sbt build load"
      via: "scalaVersion := scala3Version"
      pattern: "scalaVersion\\s*:=\\s*scala3Version"
    - from: "project/Commons.scala"
      to: ".github/workflows/ci.yml"
      via: "githubWorkflowScalaVersions + githubWorkflowJavaVersions"
      pattern: "githubWorkflowScalaVersions"
---

<objective>
Cut the `01-big-bang` branch from `upstream/scala-3 @ 1561d8dc` and pivot the build infrastructure to Scala 3 only. This is the foundation for everything else in Phase 1: no further work compiles until the build loads on Scala 3.

Purpose: Establish a clean Scala-3-only baseline from upstream — no crossScalaVersions, migrated scalac options, simplified scalafmt config, single-axis CI matrix. Subsequent plans depend on this loading successfully.

Output: A loaded sbt build at Scala 3.8.2, regenerated `ci.yml`, simplified `.scalafmt.conf`, branch `01-big-bang` tracking upstream/scala-3 with the build-infra commit on top.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/PROJECT.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/STATE.md
@.planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md
@.planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md
@.planning/phases/01-big-bang-comment-and-green/01-VALIDATION.md

<interfaces>
Upstream baseline @ 1561d8dc (verified in RESEARCH):

project/Commons.scala:
- `scalaVersion := "2.13.18"` (no crossScalaVersions at top level)
- aggregates: `jvm` (analyzer, macros, core, jetty, mongo, hocon, spring), `js` (core-js, mongo-js)
- `mkSourceDirs(base, conf, scalaBinary)` helper with scala-binary rung
- madeVersion NOT present (made not introduced upstream)

Scala 3 scalacOptions target (final form, from RESEARCH "scalac Options Migration"):
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
  // TODO[scala3-port]: enable -Werror after warnings clean
  // "-Werror",
)
```

Drop entirely: `-Xsource:3`, `-Yrangepos`, `-Xlint:*`, `-Ycache-*`, `-language:experimental.macros`, the whole `if (scalaBinaryVersion == "2.13")` block, `-Ymacro-expand:none` (unidoc).

CI YAML (post-regen) MUST contain `scala: [3.8.2]` and Java 17/21/25 only; MUST NOT contain `2.13`.
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut branch from upstream/scala-3 @ 1561d8dc</name>
  <files>(branch operation; no source files modified)</files>
  <read_first>
    - .planning/STATE.md (current branch is 05-core-scala-3-baseline-port; pivot abandons it)
    - .planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md (Branch + base decisions)
  </read_first>
  <action>
    1. `git fetch upstream` to ensure upstream/scala-3 is current.
    2. Verify upstream tip: `git rev-parse upstream/scala-3` MUST start with `1561d8dc` (if it has advanced, STOP and report — research baseline was at this exact commit; proceeding requires user ack).
    3. Verify working tree clean: `git status --porcelain` MUST be empty. If not, STOP.
    4. Create branch: `git checkout -b 01-big-bang upstream/scala-3`.
    5. Verify: `git rev-parse HEAD` matches `git rev-parse upstream/scala-3`.
    6. Sanity: `ls project/Commons.scala build.sbt .scalafmt.conf .github/workflows/ci.yml` — all must exist.
    7. Confirm there are no local working-dir `scala-2.13/` dirs to clean (upstream has zero per RESEARCH module inventory): `find . -type d -name 'scala-2.13' -not -path './.planning/*' -not -path './node_modules/*'` MUST return empty.

    Do NOT push the branch (Plan 06 handles push under checkpoint gate).
  </action>
  <verify>
    <automated>test "$(git rev-parse --abbrev-ref HEAD)" = "01-big-bang" && test "$(git rev-parse HEAD)" = "$(git rev-parse upstream/scala-3)" && test -z "$(find . -type d -name 'scala-2.13' -not -path './.planning/*' 2>/dev/null)"</automated>
  </verify>
  <done>
    Branch `01-big-bang` exists at `upstream/scala-3 @ 1561d8dc`. Working tree clean. No `scala-2.13/` source dirs anywhere.
  </done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Rewrite project/Commons.scala for Scala 3 only</name>
  <files>project/Commons.scala, build.sbt</files>
  <read_first>
    - `git show upstream/scala-3:project/Commons.scala` (current upstream baseline; this is what you're editing)
    - .planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md (sections: "Upstream Baseline Build State", "scalac Options Migration", "ScalaJS, sbt-nosbt, sbt-jmh", "Architecture Patterns", "Common Pitfalls")
    - .planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md (locked decisions: scalaVersion, made 0.1.1 unconditional, drop jvm2, aggregate-level disables, KEEP commons- ProjectGroup prefix, KEEP mkSourceDirs but drop scala-binary rung)
  </read_first>
  <action>
    Edit `project/Commons.scala` (single atomic edit, multiple subsections):

    1. **Top-level constants** — replace any scala2/cross version constants with:
       ```scala
       val scala3Version = "3.8.2"
       val madeVersion = "0.1.1"
       ```
       Remove `scala2Version` if present. Remove `crossScalaVersions` references everywhere.

    2. **scalaVersion** — at every site where `scalaVersion := ...` or `crossScalaVersions := ...` appears, set `scalaVersion := scala3Version` and DELETE `crossScalaVersions` lines.

    3. **scalacOptions** — replace the existing `Compile / scalacOptions` block with the locked Scala 3 list (see `<interfaces>` above). Keep `-Werror` commented with `// TODO[scala3-port]: enable -Werror after warnings clean` (per RESEARCH Open Question 1 and `<deep_work_rules>`). Delete entirely the `if (scalaBinaryVersion.value == "2.13") Seq(...)` block and any `-Ymacro-expand:none` in unidoc scalacOptions.

    4. **mkSourceDirs / sourceDirsSettings** — simplify to drop the `scala-$scalaBinary` rung. Final form (RESEARCH "Simplified `mkSourceDirs`"):
       ```scala
       def mkSourceDirs(base: File, conf: String): Seq[File] = Seq(
         base / "src" / conf / "scala",
         base / "src" / conf / "java",
       )
       ```
       Update call sites accordingly (drop the `scalaBinary` arg).

    5. **made dependency** — on `core` (and any other module that depends on it), add unconditional:
       ```scala
       libraryDependencies += "io.github.halotukozak" %% "made" % madeVersion
       ```
       Remove any `if (scalaBinaryVersion.value == "3") Seq(...) else Seq.empty` guard around it (we are Scala 3 only).

    6. **Aggregates** — the `jvm` aggregate keeps `macros, core, mongo, hocon, benchmark` (and any other module that has surviving sources). DROP from aggregate (comment out with `// TODO[scala3-port]: <reason> (effort)`):
       - `analyzer` — `// TODO[scala3-port]: Scala 2 compiler plugin; restore as Scala 3 plugin (L)`
       - `jetty` — `// TODO[scala3-port]: ee10 servlet wrapper (M)`
       - `spring` — `// TODO[scala3-port]: spring-context wiring (S)`
       - `comprof` if present — `// TODO[scala3-port]: scalac-profiling is Scala 2 only — restore or retire (M)`
       Keep `lazy val analyzer = ...`, `lazy val jetty = ...`, `lazy val spring = ...` declarations intact (only remove from `.aggregate(...)` list) — minimum-diff per RESEARCH "Aggregate-Level Disable Pattern".

       DROP `jvm2` aggregate entirely if present (CONTEXT decision).

       `js` aggregate keeps `core-js, mongo-js, benchmark-js` (if present in upstream).

    7. **GitHub Actions settings** — at the build-root settings block, set:
       ```scala
       ThisBuild / githubWorkflowScalaVersions := Seq(scala3Version),
       ThisBuild / githubWorkflowJavaVersions := Seq(
         JavaSpec.temurin("17"),
         JavaSpec.temurin("21"),
         JavaSpec.temurin("25"),
       ),
       ThisBuild / githubWorkflowBuild := Seq(
         WorkflowStep.Sbt(
           List("compile", "Test/compile", "scalafmtCheckAll", "scalafmtSbtCheck"),
           name = Some("Build + lint"),
         ),
       ),
       ```
       Drop `githubWorkflowAddedJobs` entries for mima and scalafmt (folded in). Drop any `if [ "${{ matrix.scala }}" = "2.13.18" ]` shell branching.

    `build.sbt`: usually a 1-line stub (`lazy val root = Commons.root`). If it carries any cross-build or scalaVersion settings, strip them so Commons.scala is single source of truth.

    DO NOT yet run `githubWorkflowGenerate` (next task does that). DO NOT touch `.scalafmt.conf` (next task). DO NOT touch any source files outside `project/`.

    Memory-rule audit before commit:
    - `git diff --staged | grep -E '@nowarn|-Wconf'` MUST return zero matches (QUALITY-01).
    - `git diff --staged -- .planning/` MUST be empty (WORKFLOW-05).
    - Commit message MUST NOT contain "GSD", "phase", or any planning nomenclature (WORKFLOW-04). Use conventional `build:` prefix.

    Commit (single atomic): `git add project/Commons.scala build.sbt && git commit -m "build: pivot to Scala 3 only, migrate scalac options"`.
  </action>
  <verify>
    <automated>grep -q 'scala3Version = "3.8.2"' project/Commons.scala && grep -q 'madeVersion = "0.1.1"' project/Commons.scala && ! grep -q 'crossScalaVersions' project/Commons.scala && ! grep -q '\-Xsource:3' project/Commons.scala && ! grep -q '\-Wconf' project/Commons.scala && ! grep -q 'scalaBinaryVersion.value == "2.13"' project/Commons.scala && sbt -batch 'show scalaVersion' 2>&1 | grep -q '3.8.2'</automated>
  </verify>
  <done>
    Commons.scala uses single Scala 3 axis. `sbt 'show scalaVersion'` returns `3.8.2`. No `crossScalaVersions`, no `-Xsource:3`, no `-Wconf`, no `scala-2.13` branching. `analyzer`/`jetty`/`spring` dropped from `jvm` aggregate (declarations preserved). `made` 0.1.1 unconditional on core. Commit landed on `01-big-bang`.
  </done>
</task>

<task type="auto" tdd="false">
  <name>Task 3: Simplify .scalafmt.conf and regenerate ci.yml</name>
  <files>.scalafmt.conf, .github/workflows/ci.yml</files>
  <read_first>
    - `.scalafmt.conf` current contents (upstream uses `Scala213Source3` dialect + fileOverride blocks)
    - .planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md ("sbt-github-actions Single-Axis Recipe", Pitfall 5, Pitfall 6)
    - .planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md (`.scalafmt.conf` decision: single `runner.dialect = scala3`, no fileOverride)
  </read_first>
  <action>
    1. **Edit `.scalafmt.conf`**:
       - Set `runner.dialect = scala3` (top-level).
       - DELETE every `fileOverride { "glob:..." { runner.dialect = ... } }` block (no per-path overrides; we're Scala 3 only).
       - Keep everything else (version pin, maxColumn, alignment rules) unchanged.

    2. **Run scalafmt** to absorb the dialect flip BEFORE generating CI (so the regen doesn't trip on unformatted files):
       ```
       sbt -batch scalafmtAll scalafmtSbtCheck
       ```
       If this rewrites files outside `project/` and `.scalafmt.conf` (which it likely will on Scala/source files — RESEARCH Pitfall 5), that's expected. The reformat lands as part of this commit per the locked strategy ("dedicated `style(scalafmt):` commit so the reformat is reviewable separately"). Stage all touched files.

    3. **Regenerate ci.yml**:
       ```
       sbt -batch githubWorkflowGenerate
       ```
       This rewrites `.github/workflows/ci.yml` from the settings in Commons.scala (set in Task 2). Verify post-regen:
       - `grep -c '3.8.2' .github/workflows/ci.yml` ≥ 1
       - `! grep -q '2.13' .github/workflows/ci.yml`
       - `grep -q 'temurin@17' .github/workflows/ci.yml`
       - `grep -q 'temurin@21' .github/workflows/ci.yml`
       - `grep -q 'temurin@25' .github/workflows/ci.yml`

    4. **Validation gate** before commit:
       - `sbt -batch scalafmtCheckAll` exit 0.
       - `sbt -batch 'show version'` exit 0 (build loads).
       - `git diff --staged | grep -E '@nowarn|-Wconf'` returns zero (QUALITY-01).
       - `git diff --staged -- .planning/` empty (WORKFLOW-05).

    5. **Commit strategy** — two separate commits for review clarity:
       - Commit A (style only): `git add .scalafmt.conf && git add -A '*.scala' && git commit -m "style(scalafmt): switch to single scala3 dialect"`. If the only `.scala` changes are reformatting (no semantic edits), this commit is pure formatting.
       - Commit B (CI regen): `git add .github/workflows/ci.yml && git commit -m "ci: regenerate workflow for single Scala 3 axis on Java 17/21/25"`.

    If Commit A has no staged content (i.e. the dialect flip produced no diff), skip it.

    No `@nowarn`/`-Wconf` introduced anywhere. No `.planning/` paths.
  </action>
  <verify>
    <automated>grep -q 'runner.dialect = scala3' .scalafmt.conf && ! grep -q 'fileOverride' .scalafmt.conf && ! grep -q 'scala213source3' .scalafmt.conf && grep -q '3.8.2' .github/workflows/ci.yml && ! grep -q '2.13' .github/workflows/ci.yml && grep -q 'temurin@17' .github/workflows/ci.yml && grep -q 'temurin@21' .github/workflows/ci.yml && grep -q 'temurin@25' .github/workflows/ci.yml && sbt -batch scalafmtCheckAll 2>&1 | tail -3 | grep -qE '(success|^\[success\])'</automated>
  </verify>
  <done>
    `.scalafmt.conf` is single scala3 dialect (no fileOverride). `ci.yml` regenerated with single Scala 3 axis × Temurin 17/21/25, no 2.13 references. `sbt scalafmtCheckAll` green. Build loads.
  </done>
</task>

</tasks>

<verification>
- `git rev-parse --abbrev-ref HEAD` → `01-big-bang`
- `git log --oneline upstream/scala-3..HEAD` → 2 or 3 commits (build pivot + optional style reformat + CI regen), all conventional prefixes, no GSD nomenclature
- `sbt -batch 'show scalaVersion'` exits 0 and prints `3.8.2`
- `sbt -batch scalafmtCheckAll` exits 0
- `git grep -nE '@nowarn|-Wconf' -- '*.scala'` → zero new occurrences vs upstream/scala-3
- `find . -type d -name 'scala-2.13' -not -path './.planning/*'` → empty
- `! grep -q 'crossScalaVersions' project/Commons.scala`
- `grep -q 'runner.dialect = scala3' .scalafmt.conf && ! grep -q 'fileOverride' .scalafmt.conf`
- `grep -q '3.8.2' .github/workflows/ci.yml && ! grep -q '2.13' .github/workflows/ci.yml`
</verification>

<success_criteria>
1. Branch `01-big-bang` exists locally at HEAD = upstream/scala-3 @ 1561d8dc + this plan's commits.
2. Build loads on Scala 3.8.2.
3. `.scalafmt.conf` single scala3 dialect, no fileOverride.
4. `.github/workflows/ci.yml` regenerated: single Scala 3 axis × Java 17/21/25.
5. `scalafmtCheckAll` green.
6. No `crossScalaVersions`, no `-Xsource:3`, no `-Wconf`, no `@nowarn`, no `scala-2.13/` dirs.
7. `analyzer`/`jetty`/`spring` dropped from jvm aggregate (declarations preserved).
8. `made` 0.1.1 unconditional on core.
9. Commit messages use conventional prefixes (`build:`, `style:`, `ci:`) — no GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/01-big-bang-comment-and-green/01-01-SUMMARY.md` summarizing:
- Branch cut commit hash + upstream tip used
- Scalac options before → after (one-line diff per flag)
- Aggregate membership before → after
- ci.yml matrix before → after
- Any deviations (e.g., if scalafmt reformat had to be split into multiple commits)
- All requirements satisfied (BUILD-01..05, QUALITY-01)
</output>
