---
phase: 01-cross-compile-infrastructure
plan: 02
type: execute
wave: 2
depends_on: [01]
files_modified:
  - project/Commons.scala
autonomous: true
requirements: [INFRA-01, INFRA-02, INFRA-03, INFRA-06, INFRA-08, INFRA-09]
must_haves:
  truths:
    - "`sbt 'projects'` lists `jvm`, `jvm2`, and `js` as aggregates under root"
    - "`sbt 'show jvm/aggregateProjects'` (or equivalent) shows cross-built modules EXCLUDING `jetty`"
    - "`sbt 'show jvm2/aggregateProjects'` shows `jetty` (and only jetty for Phase 1)"
    - "`sbt 'show core/crossScalaVersions'` returns a value containing both `3.8.2` and `2.13.18`"
    - "`sbt 'show jetty/crossScalaVersions'` returns `List(2.13.18)` (single-version)"
    - "`sbt '++3.8.2; show core/libraryDependencies' | grep -F 'io.github.halotukozak:made'` finds the made dep on Scala 3 only"
    - "`sbt '++2.13.18; show core/libraryDependencies' | grep -F 'io.github.halotukozak:made'` returns nothing (no made on 2.13)"
    - "`sbt '+jvm/compile'` exits 0 — every cross-built module compiles on both 2.13.18 and 3.8.2 (empty Scala 3 jars OK)"
    - "`sbt '++3.8.2; jvm/compile'` exits 0 — Scala 3 side compiles (REQ INFRA-08)"
    - "`sbt '++3.8.2; jetty/compile'` is a no-op (jetty skipped on Scala 3)"
  artifacts:
    - path: project/Commons.scala
      provides: "jvm/jvm2/js aggregates, per-module crossScalaVersions, scalaVersion = scala3Version, made dep on Scala 3, jetty skip block"
      contains: "lazy val jvm2 = mkSubProject"
  key_links:
    - from: "root aggregate"
      to: "jvm, jvm2, js sub-aggregates"
      via: "mkRootProject.aggregate(jvm, jvm2, js)"
      pattern: "\\.aggregate\\([\\s\\S]*?jvm,[\\s\\S]*?jvm2,[\\s\\S]*?js"
    - from: "core libraryDependencies on Scala 3"
      to: "io.github.halotukozak:made_3:0.1.0"
      via: 'if (scalaBinaryVersion.value == "3") Seq("io.github.halotukozak" %% "made" % madeVersion)'
      pattern: 'scalaBinaryVersion\\.value == "3"[\\s\\S]*?"made"'
    - from: "jetty module"
      to: "2.13-only build skip"
      via: "Compile/skip, Test/skip, update/skip, publish/skip := scalaBinaryVersion.value != \"2.13\""
      pattern: 'scalaBinaryVersion\\.value != "2\\.13"'
---

<objective>
Restructure `project/Commons.scala` to land the cross-compile build organization on upstream/scala-3:
1. Add `jvm2` aggregate (currently absent on upstream/scala-3) holding `jetty`.
2. Drop `analyzer` and `spring` from the `jvm` aggregate (CONTEXT: they stay commented-out / removed on master pattern; formalization happens in Phase 12).
3. Set `crossScalaVersions := Seq(scala3Version, scala2Version)` per cross-built module + `crossScalaVersions := Seq(scala2Version)` on `jetty`.
4. Add `scala3Version`, `scala2Version`, `madeVersion` val declarations.
5. Bump `scalaVersion` (buildSettings + per-module) from `"2.13.18"` to `scala3Version` (= `"3.8.2"`).
6. Wire `jetty` skip block (`Compile/skip`, `Test/skip`, `update/skip`, `publish/skip` keyed on `scalaBinaryVersion.value != "2.13"`).
7. Add conditional `made` dependency to `core` (Scala 3 only, pinned to `0.1.0`).
8. PRESERVE the existing `mkSourceDirs` / `sourceDirsSettings` helpers verbatim (per user note: "preserve upstream's mkSourceDirs helper"). They already implement REQ INFRA-03 idiomatically inside Commons.scala — no need to swap for `CrossVersion.partialVersion`.

Purpose: Land the build-organization side of Phase 1 in a single focused diff. CI workflow keys + ci.yml regeneration happen in Plan 03 to keep the Commons.scala diff readable.

Output: `project/Commons.scala` edited to support cross-compilation; `sbt '+jvm/compile'` green on both Scala versions; `sbt '++3.8.2; jvm/compile'` green with empty jars on Scala 3 side.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md
@.planning/phases/01-cross-compile-infrastructure/01-RESEARCH.md
@.planning/phases/01-cross-compile-infrastructure/01-01-SUMMARY.md
</context>

<interfaces>
<!-- Full upstream/scala-3 Commons.scala is the editing baseline. Critical exports the executor MUST reference: -->

From `upstream/scala-3:project/Commons.scala`:

```scala
object Commons extends ProjectGroup("commons") {
  val forIdeaImport: Boolean = ...
  val guavaVersion = "33.6.0-jre"
  val jettyVersion = "12.1.9"
  val springVersion = "6.2.18"
  val monixVersion = "3.4.1"
  // (other version vals)

  val previousCompatibleVersions: Set[String] = Set(..., "2.27.1")

  override def globalSettings: Seq[Def.Setting[_]] = Seq(...)
  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    ...
    scalaVersion := "2.13.18",   // <-- EDIT TARGET (change to scala3Version)
    ...
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21"), JavaSpec.temurin("25")),
    ...
    githubWorkflowAddedJobs += WorkflowJob(id = "mima", ...),       // <-- LEFT FOR PLAN 03
    githubWorkflowAddedJobs += WorkflowJob(id = "scalafmt", ...),   // <-- LEFT FOR PLAN 03
    ...
  )

  override def commonSettings: Seq[Def.Setting[_]] = Seq(
    ...
    Test / fork := true,
    mimaPreviousArtifacts := previousCompatibleVersions.map(v => organization.value %%% moduleName.value % v),
  )

  val jvmCommonSettings = Seq(...)
  val jsCommonSettings = Seq(...)
  val noPublishSettings = Seq(publish / skip := true, mimaPreviousArtifacts := Set.empty)
  val aggregateProjectSettings = noPublishSettings ++ Seq(ideSkipProject := true, ideExcludedDirectories := Seq(baseDirectory.value))

  val CompileAndTest = "compile->compile;test->test"

  lazy val root = mkRootProject
    .enablePlugins(ScalaUnidocPlugin)
    .aggregate(jvm, js)                            // <-- EDIT: add jvm2
    .settings(
      noPublishSettings,
      name := "commons",
      ...
      ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
        analyzer, macros, `core-js`, comprof,      // <-- analyzer removed from aggregate but still referenced here; see action below
      ),
    )

  lazy val jvm = mkSubProject.in(file(".jvm"))
    .aggregate(analyzer, macros, core, jetty, mongo, hocon, spring)  // <-- EDIT: drop analyzer, jetty, spring
    .settings(aggregateProjectSettings)

  lazy val js = mkSubProject.in(file(".js"))
    .aggregate(`core-js`, `mongo-js`)
    .settings(aggregateProjectSettings)

  lazy val analyzer = mkSubProject.dependsOn(core % Test).settings(...)

  def mkSourceDirs(base: File, scalaBinary: String, conf: String): Seq[File] = Seq(
    base / "src" / conf / "scala",
    base / "src" / conf / s"scala-$scalaBinary",
    base / "src" / conf / "java",
  )

  def sourceDirsSettings(baseMapper: File => File) = Seq(
    Compile / unmanagedSourceDirectories ++=
      mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "main"),
    Test / unmanagedSourceDirectories ++= mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "test"),
  )

  lazy val macros = mkSubProject.settings(
    jvmCommonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,    // <-- EDIT: gate on 2.13 only
    mimaPreviousArtifacts := Set.empty,
  )

  lazy val core = mkSubProject.dependsOn(macros).settings(
    jvmCommonSettings,
    sourceDirsSettings(_ / "jvm"),
    libraryDependencies ++= Seq("com.google.guava" % "guava" % guavaVersion % Optional, "io.monix" %% "monix" % monixVersion % Optional),
    mimaBinaryIssueFilters ++= coreMimaFilters,                                        // <-- EDIT: add crossScalaVersions + made dep
  )

  // ... mongo, `mongo-js`, hocon, spring, jetty, benchmark, `benchmark-js`, comprof
}
```

Key sbt API used:
- `crossScalaVersions: SettingKey[Seq[String]]` — set per-module
- `scalaVersion: SettingKey[String]` — set per-module to allow `++` switching
- `scalaBinaryVersion: SettingKey[String]` — task value at evaluation time; returns "2.13" or "3"
- `libraryDependencies: SettingKey[Seq[ModuleID]]` — `%%` appends `_<scalaBinary>` automatically
- `Compile / skip`, `Test / skip`, `update / skip`, `publish / skip`: `SettingKey[Boolean]` — task-scoped skip
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Add version vals + flip root scalaVersion to scala3Version + add jvm2 aggregate + drop 2.13-only modules from jvm aggregate</name>
  <files>project/Commons.scala</files>
  <read_first>
    - project/Commons.scala (full file — must preserve all unchanged sections byte-identical)
    - .planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md §decisions (Aggregate split, Cross-build configuration)
    - .planning/phases/01-cross-compile-infrastructure/01-RESEARCH.md §"Example 3: project/Commons.scala — aggregate definitions"
  </read_first>
  <action>
    Edit `/Users/bkozak/IdeaProjects/scala-commons3/project/Commons.scala`. All edits below are surgical — do NOT touch unrelated sections.

    (a) Add version vals. Locate the existing val block starting with `val guavaVersion = "33.6.0-jre"` and ending with `val slf4jVersion = "2.0.18" // test only`. AFTER the `val slf4jVersion` line and BEFORE the `val previousCompatibleVersions` declaration, insert exactly:

    ```scala

      val scala2Version = "2.13.18"
      val scala3Version = "3.8.2"
      val madeVersion = "0.1.0" // pinned release on Sonatype Central; NOT 0.1.1-SNAPSHOT
    ```

    (Indentation: two spaces, matching the surrounding val declarations inside `object Commons`.)

    (b) Flip default scalaVersion. Inside `override def buildSettings`, replace the line:
    ```scala
        scalaVersion := "2.13.18",
    ```
    with these TWO lines (in this exact order):
    ```scala
        scalaVersion := scala3Version,
        crossScalaVersions := Seq(scala3Version, scala2Version),
    ```
    Rationale: `crossScalaVersions` at `buildSettings` (= ThisBuild scope) is required by `sbt-github-actions` 0.30.0 to populate the workflow matrix. Per-module overrides (Task 2) still constrain `jetty` to 2.13 only. See RESEARCH §"Example 2" note on ThisBuild crossScalaVersions.

    (c) Add `jvm2` aggregate + drop `analyzer`, `jetty`, `spring` from `jvm` aggregate.

    Replace the existing `lazy val root = mkRootProject` block's `.aggregate(jvm, js,)` with `.aggregate(jvm, jvm2, js,)`.

    Replace the existing `lazy val jvm = mkSubProject` block (the whole `.aggregate(analyzer, macros, core, jetty, mongo, hocon, spring,)` list) with:

    ```scala
      lazy val jvm = mkSubProject
        .in(file(".jvm"))
        .aggregate(
          macros,
          core,
          mongo,
          hocon,
        )
        .settings(aggregateProjectSettings)

      lazy val jvm2 = mkSubProject
        .in(file(".jvm2"))
        .aggregate(jetty)
        .settings(aggregateProjectSettings)
    ```

    IMPORTANT: do NOT delete the `lazy val analyzer`, `lazy val spring`, `lazy val jetty` module definitions themselves — they stay defined in Commons.scala (jetty moves into jvm2; analyzer/spring become orphans referenced only by `ScalaUnidoc/unidocProjectFilter` and any explicit `dependsOn` chains). Phase 12 formalizes their out-of-cross-build status.

    Leave the `lazy val js` block unchanged.

    The `ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(analyzer, macros, `core-js`, comprof,)` line stays as-is — those projects still exist as definitions; we only changed which aggregate(s) wire them into the root build graph.

    (d) Do NOT touch anything else in this task: globalSettings, commonSettings, jvmCommonSettings, jsCommonSettings, noPublishSettings, aggregateProjectSettings, CompileAndTest val, mkSourceDirs/sourceDirsSettings helpers, sameNameAs helper, ALL module definitions besides root/jvm (those edited in Task 2), githubWorkflow* keys (Plan 03 owns those).

    DO NOT COMMIT YET. Task 2 makes per-module crossScalaVersions edits; Task 3 wires made + jetty skips. Single combined commit at end of Plan 02.

    Verify intermediate sanity: `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch 'reload' 'projects'` MUST exit 0 and the output MUST contain lines listing `jvm`, `jvm2`, and `js` as projects. If `sbt reload` fails, the most likely cause is a syntax error from the multi-line edit — re-read Commons.scala diff and fix before proceeding.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; grep -Fq 'val scala3Version = "3.8.2"' project/Commons.scala &amp;&amp; grep -Fq 'val scala2Version = "2.13.18"' project/Commons.scala &amp;&amp; grep -Fq 'val madeVersion = "0.1.0"' project/Commons.scala &amp;&amp; grep -Fq 'lazy val jvm2 = mkSubProject' project/Commons.scala &amp;&amp; grep -Fq 'scalaVersion := scala3Version,' project/Commons.scala &amp;&amp; sbt -batch 'reload' 'projects' | grep -E '\\* +jvm2'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -Fxq '  val scala2Version = "2.13.18"' project/Commons.scala` exits 0.
    - `grep -Fxq '  val scala3Version = "3.8.2"' project/Commons.scala` exits 0.
    - `grep -Fq 'val madeVersion = "0.1.0"' project/Commons.scala` exits 0.
    - `grep -Fq 'scalaVersion := scala3Version,' project/Commons.scala` exits 0.
    - `grep -Fq 'crossScalaVersions := Seq(scala3Version, scala2Version),' project/Commons.scala` exits 0 (at buildSettings level).
    - `grep -Fq 'scalaVersion := "2.13.18",' project/Commons.scala` exits 1 (old literal scalaVersion removed from buildSettings).
    - `grep -Fq 'lazy val jvm2 = mkSubProject' project/Commons.scala` exits 0.
    - `grep -Fq '.in(file(".jvm2"))' project/Commons.scala` exits 0.
    - `grep -Fq '.aggregate(jetty)' project/Commons.scala` exits 0 (jvm2 aggregates jetty).
    - `jvm` aggregate excludes 2.13-only modules: in the file's `lazy val jvm = mkSubProject` block, the `.aggregate(...)` listing MUST NOT contain identifiers `analyzer`, `jetty`, or `spring`. Verify by inspecting lines between `lazy val jvm = mkSubProject` and the next `lazy val` declaration: `sed -n '/lazy val jvm = mkSubProject/,/lazy val jvm2/p' project/Commons.scala | grep -E '^\\s*(analyzer|jetty|spring),?\\s*$'` exits 1.
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch 'reload' 'projects' 2&gt;&amp;1 | grep -E '\\bjvm2\\b'` exits 0 (jvm2 project visible to sbt).
  </acceptance_criteria>
  <done>Version vals declared; root aggregates jvm/jvm2/js; jvm strips 2.13-only modules; ThisBuild `scalaVersion` and `crossScalaVersions` flipped to Scala 3 + cross. `sbt projects` lists `jvm2`.</done>
</task>

<task type="auto">
  <name>Task 2: Set per-module crossScalaVersions + scalaVersion + macros scala-reflect gating</name>
  <files>project/Commons.scala</files>
  <read_first>
    - project/Commons.scala (post-Task-1 state)
    - .planning/phases/01-cross-compile-infrastructure/01-RESEARCH.md §"Example 4: Per-module crossScalaVersions wiring"
  </read_first>
  <action>
    Edit `/Users/bkozak/IdeaProjects/scala-commons3/project/Commons.scala` to add `crossScalaVersions` + `scalaVersion` to each cross-built module. The pattern repeated below is the same three lines — add as the FIRST settings inside each `.settings(...)` block, right after `jvmCommonSettings,` (or `jsCommonSettings,` for js modules):

    ```scala
        crossScalaVersions := Seq(scala3Version, scala2Version),
        scalaVersion := scala3Version,
    ```

    Apply to these modules (use the existing names in Commons.scala):
    - `macros` (cross-built — Scala 3 jar will be empty after Phase 3 stub; for Phase 1 it compiles via the conditional scala-reflect dep below)
    - `core` (cross-built)
    - `mongo` (cross-built)
    - `hocon` (cross-built)
    - `core-js` (cross-built, Scala.js — pattern still works)
    - `mongo-js` (cross-built, Scala.js)
    - `benchmark` (cross-built)
    - `benchmark-js` (cross-built)
    - `comprof` (cross-built — orphan but exists; cross-build it to avoid surprise breakage during `++3.8.2` resolution)

    Do NOT add these settings to `analyzer` or `spring` — they remain unaggregated 2.13-only orphans (Phase 12 formalizes). Do NOT add to `jetty` — Task 3 handles jetty separately.

    Additionally, for `macros`: the existing line
    ```scala
        libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    ```
    must be gated on Scala 2.13 (Scala 3 has no scala-reflect runtime). Replace with:
    ```scala
        libraryDependencies ++= {
          if (scalaBinaryVersion.value == "2.13")
            Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
          else Seq.empty
        },
    ```

    Do NOT touch the `mkSourceDirs` / `sourceDirsSettings` helpers — preserve verbatim per user directive ("preserve upstream's mkSourceDirs helper"). Existing `sourceDirsSettings(_ / "jvm")` calls in `core`, `mongo`, `benchmark`, `comprof` stay. Existing `sourceDirsSettings(_.getParentFile)` calls in `core-js`, `mongo-js`, `benchmark-js` stay.

    DO NOT COMMIT YET — Task 3 finishes the Commons.scala diff with made + jetty skip block.

    Sanity check after edit: `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch 'reload' '++3.8.2; show core/crossScalaVersions' '++3.8.2; show core/scalaVersion'` MUST exit 0; the `crossScalaVersions` show line MUST print a value containing both `3.8.2` and `2.13.18`; the `scalaVersion` show line MUST print `3.8.2`.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; [ "$(grep -c 'crossScalaVersions := Seq(scala3Version, scala2Version)' project/Commons.scala)" -ge 10 ] &amp;&amp; grep -Fq 'if (scalaBinaryVersion.value == "2.13")' project/Commons.scala &amp;&amp; grep -Fq '"scala-reflect"' project/Commons.scala &amp;&amp; sbt -batch 'reload' '++3.8.2; show core/scalaVersion' 2&gt;&amp;1 | grep -F '3.8.2'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'crossScalaVersions := Seq(scala3Version, scala2Version)' project/Commons.scala` prints a number `>= 10` (one ThisBuild from Task 1 + nine per-module: macros, core, mongo, hocon, core-js, mongo-js, benchmark, benchmark-js, comprof).
    - `grep -c 'scalaVersion := scala3Version,' project/Commons.scala` prints a number `>= 10` (one ThisBuild + nine per-module).
    - `macros` scala-reflect dep is gated: `grep -B1 -A3 '"scala-reflect"' project/Commons.scala | grep -F 'if (scalaBinaryVersion.value == "2.13")'` exits 0.
    - `mkSourceDirs` helper preserved: `grep -Fq 'def mkSourceDirs(base: File, scalaBinary: String, conf: String)' project/Commons.scala` exits 0.
    - `sourceDirsSettings` helper preserved: `grep -Fq 'def sourceDirsSettings(baseMapper: File =&gt; File)' project/Commons.scala` exits 0.
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++3.8.2; show core/scalaVersion' 2&gt;&amp;1 | tail -5 | grep -F '3.8.2'` exits 0.
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++2.13.18; show core/crossScalaVersions' 2&gt;&amp;1 | tail -5 | grep -F '2.13.18'` exits 0.
  </acceptance_criteria>
  <done>Every cross-built module declares `crossScalaVersions := Seq(scala3Version, scala2Version)` + `scalaVersion := scala3Version`. `macros` scala-reflect is 2.13-only. `mkSourceDirs` helper preserved. `sbt show core/crossScalaVersions` reflects both versions.</done>
</task>

<task type="auto">
  <name>Task 3: Wire made dep on core (Scala 3 only) + jetty skip block + commit</name>
  <files>project/Commons.scala</files>
  <read_first>
    - project/Commons.scala (post-Task-2 state)
    - .planning/phases/01-cross-compile-infrastructure/01-RESEARCH.md §"Pattern 3" and §"Pattern 4"
  </read_first>
  <action>
    Edit `/Users/bkozak/IdeaProjects/scala-commons3/project/Commons.scala` for the final two pieces of the build-organization slice.

    (a) Add `made` dep to `core`. Locate `lazy val core = mkSubProject.dependsOn(macros).settings(...)`. The current settings block contains:
    ```scala
        libraryDependencies ++= Seq(
          "com.google.guava" % "guava" % guavaVersion % Optional,
          "io.monix" %% "monix" % monixVersion % Optional,
        ),
        mimaBinaryIssueFilters ++= coreMimaFilters,
    ```

    AFTER the `libraryDependencies ++= Seq(... monix ...)` block and BEFORE the `mimaBinaryIssueFilters ++= coreMimaFilters,` line, insert exactly:
    ```scala
        libraryDependencies ++= {
          if (scalaBinaryVersion.value == "3")
            Seq("io.github.halotukozak" %% "made" % madeVersion)
          else Seq.empty
        },
    ```

    `%%` (double percent) appends `_3` to produce `io.github.halotukozak:made_3:0.1.0`. Verified resolvable via `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` (per RESEARCH.md "Version verification").

    Do NOT add a SNAPSHOT resolver. Do NOT add the dep to any other module (Phase 4 expands `made` integration).

    (b) Wire jetty skip block. Locate `lazy val jetty = mkSubProject.dependsOn(core % CompileAndTest).settings(...)`. Modify its settings block to add the four skip keys + crossScalaVersions + scalaVersion. The new jetty block (replace existing) MUST read exactly:

    ```scala
      lazy val jetty = mkSubProject
        .dependsOn(core % CompileAndTest)
        .settings(
          jvmCommonSettings,
          crossScalaVersions := Seq(scala2Version),
          scalaVersion := scala2Version,
          // jetty is Scala 2.13-only (jetty-ee10-servlet has no Scala 3 build).
          // Skip all phases on Scala 3 so `++3.8.2 jvm2/...` is a no-op rather than
          // a coursier resolution failure for missing _3 artifacts.
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

    Note: `crossScalaVersions := Seq(scala2Version)` (single-element) is INTENTIONAL for jetty — it overrides the ThisBuild-level `Seq(scala3Version, scala2Version)`. This is REQ INFRA-09 enforcement.

    (c) Sanity check before commit. From project root:
    ```
    cd /Users/bkozak/IdeaProjects/scala-commons3
    sbt -batch 'reload' '+jvm/compile' '++3.8.2; jvm/compile' '++3.8.2; jetty/compile'
    ```
    Expected:
    - `+jvm/compile` exits 0 (REQ INFRA-08: compiles on both 2.13.18 and 3.8.2; Scala 3 jars may be empty for stub-only modules — that is OK).
    - `++3.8.2; jvm/compile` exits 0.
    - `++3.8.2; jetty/compile` exits 0 AND prints a skip message (sbt's standard "skipping..." when `Compile/skip` is true).

    If compile fails on Scala 3 for any module due to a source-level issue (e.g., 2.13-only syntax in `core/src/main/scala/`), Phase 1 is OUT of scope to fix sources — that's Phase 5+ territory. In that case, surface the file and error to the user via the SUMMARY and STOP. The empty-jar acceptance is contingent on existing shared sources being parseable by Scala 3; CONTEXT assumes they are (the upstream/scala-3 branch already declared `scalaVersion := "2.13.18"` so this is the first time anything compiles on 3.8.2 — surprises possible).

    (d) Commit the entire Plan 02 diff as one commit. Message exactly:
    ```
    build(commons): cross-compile structure — jvm2 aggregate, per-module crossScalaVersions, made on Scala 3, jetty skip
    ```
    Use `git -C /Users/bkozak/IdeaProjects/scala-commons3 commit -m '...' -- project/Commons.scala`. No GSD nomenclature.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; grep -Fq '"io.github.halotukozak" %% "made" % madeVersion' project/Commons.scala &amp;&amp; grep -Fq 'if (scalaBinaryVersion.value == "3")' project/Commons.scala &amp;&amp; grep -c 'scalaBinaryVersion.value != "2.13"' project/Commons.scala | grep -Fxq '4' &amp;&amp; sbt -batch 'reload' '+jvm/compile' '++3.8.2; jetty/compile'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -Fq '"io.github.halotukozak" %% "made" % madeVersion' project/Commons.scala` exits 0.
    - `grep -Fq 'if (scalaBinaryVersion.value == "3")' project/Commons.scala` exits 0 (made dep gating).
    - `grep -c 'scalaBinaryVersion.value != "2.13"' project/Commons.scala` prints exactly `4` (update/Compile/Test/publish skip on jetty).
    - `grep -Fq 'crossScalaVersions := Seq(scala2Version),' project/Commons.scala` exits 0 (jetty single-version override).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '+jvm/compile' 2&gt;&amp;1 | tail -10 | grep -E '\\[success\\]'` exits 0.
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++3.8.2; jvm/compile' 2&gt;&amp;1 | tail -10 | grep -E '\\[success\\]'` exits 0 (REQ INFRA-08).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++3.8.2; show jetty/Compile/skip' 2&gt;&amp;1 | tail -5 | grep -F 'true'` exits 0 (REQ INFRA-09 enforced).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++3.8.2; show core/libraryDependencies' 2&gt;&amp;1 | grep -F 'io.github.halotukozak:made'` exits 0 (REQ INFRA-06 on Scala 3).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++2.13.18; show core/libraryDependencies' 2&gt;&amp;1 | grep -F 'io.github.halotukozak:made'` exits 1 (made NOT on 2.13).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++2.13.18; show core/libraryDependencies' 2&gt;&amp;1 | grep -F 'scala-reflect'` exits 1 (macros' scala-reflect dep is local to macros; verify it's NOT leaking into core's lib deps via transitivity by ensuring the literal string is absent from `show core/libraryDependencies` output — it's only on `macros`).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch '++2.13.18; show macros/libraryDependencies' 2&gt;&amp;1 | grep -F 'scala-reflect'` exits 0 (scala-reflect IS on macros under 2.13).
    - Commit message has prefix `build(commons):` — `git log -1 --format=%s | grep -E '^build\\(commons\\):'` exits 0.
    - Commit message contains no GSD nomenclature: `git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - Commit touches ONLY `project/Commons.scala`: `git show --stat HEAD --name-only | grep -v '^commit\\|^Author\\|^Date\\|^$\\|^    ' | grep -v '^project/Commons.scala$'` exits 1 (no other files in this commit).
  </acceptance_criteria>
  <done>`project/Commons.scala` carries made dep (Scala 3 only) on core, jetty skip block, jvm/jvm2/js aggregates, per-module cross. `+jvm/compile` and `++3.8.2 jvm/compile` both green. Single commit landed.</done>
</task>

</tasks>

<verification>
After Plan 02 completes on branch `01-cross-compile-infra`:

1. `git -C /Users/bkozak/IdeaProjects/scala-commons3 log upstream/scala-3..HEAD --oneline` shows exactly 3 commits (2 from Plan 01 + 1 from Plan 02).
2. `git -C /Users/bkozak/IdeaProjects/scala-commons3 log upstream/scala-3..HEAD --name-only` shows ONLY `project/plugins.sbt`, `.scalafmt.conf`, `project/Commons.scala`.
3. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch 'projects'` lists `jvm`, `jvm2`, `js`.
4. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch '+jvm/compile'` exits 0 (INFRA-08 partial: compile-level — tests/Plan 03 verify full gate).
5. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch '++3.8.2; jvm/compile'` exits 0.
6. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch '++3.8.2; show core/libraryDependencies'` contains `io.github.halotukozak:made` (INFRA-06).
7. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch '++3.8.2; show jetty/Compile/skip'` prints `true` (INFRA-09).
8. `.planning/` paths absent from diff (`git diff upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0`).
</verification>

<success_criteria>
- Aggregates: `jvm` (cross-built modules excluding jetty/analyzer/spring), `jvm2` (jetty), `js` (core-js, mongo-js) — INFRA-02, INFRA-09.
- Per-module `crossScalaVersions := Seq(scala3Version, scala2Version)` on all cross-built modules; `Seq(scala2Version)` on jetty — INFRA-01.
- `mkSourceDirs`/`sourceDirsSettings` helpers preserved (per user directive) — INFRA-03 satisfied via existing upstream pattern.
- `made` 0.1.0 resolves on Scala 3 side of `core` only — INFRA-06.
- `+jvm/compile` and `++3.8.2 jvm/compile` both green; Scala 3 jars empty for stubless modules — INFRA-08.
</success_criteria>

<output>
After completion, create `.planning/phases/01-cross-compile-infrastructure/01-02-SUMMARY.md` capturing:
- Final diff stats for `project/Commons.scala` (lines added/removed)
- Output of `sbt 'projects'` showing jvm/jvm2/js
- Output of `sbt '++3.8.2; show core/libraryDependencies' | grep made`
- Output of `sbt '++3.8.2; show jetty/Compile/skip'`
- Output of `sbt '+jvm/compile'` (last 10 lines)
- Any Scala 3 source-level surprises encountered + which modules they affect (will inform Phase 3+ planning)
</output>
