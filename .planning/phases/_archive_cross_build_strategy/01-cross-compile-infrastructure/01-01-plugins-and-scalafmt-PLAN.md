---
phase: 01-cross-compile-infrastructure
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - project/plugins.sbt
  - .scalafmt.conf
autonomous: true
requirements: [INFRA-04, INFRA-05, QUALITY-01, QUALITY-03, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05]
must_haves:
  truths:
    - "Working tree is on branch `01-cross-compile-infra` based on `upstream/scala-3` (HEAD 1561d8dc)"
    - "`project/plugins.sbt` declares `sbt-mima-plugin` at exact version `1.1.5`"
    - "`.scalafmt.conf` declares default `runner.dialect = scala3` (NOT `Scala213Source3`)"
    - "`.scalafmt.conf` declares a `fileOverride` block pinning `**/src/{main,test}/scala-2.13/**` and `**/src/{main,test}/scala-2/**` to `runner.dialect = scala213source3`"
    - "`sbt scalafmtCheckAll` exits 0 against the unchanged-source upstream/scala-3 tree"
  artifacts:
    - path: project/plugins.sbt
      provides: "Bumped MiMa plugin"
      contains: 'addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")'
    - path: .scalafmt.conf
      provides: "Inverted dialect: scala3 default + scala-2.13/ override to scala213source3"
      contains: "runner.dialect = scala3"
  key_links:
    - from: ".scalafmt.conf default dialect"
      to: "scalafmtCheckAll runner"
      via: "scalafmt 3.11.1 reading runner.dialect"
      pattern: "runner\\.dialect = scala3"
    - from: ".scalafmt.conf fileOverride globs"
      to: "scala-2.13/ source dirs (will host -Xsource:3 code in later phases)"
      via: "glob:**/src/{main,test}/scala-2.13/**"
      pattern: "scala213source3"
---

<objective>
Land the cross-cutting plugin and formatter changes onto `upstream/scala-3` as the first wave of Phase 1. Bumps `sbt-mima-plugin` to 1.1.5 per REQ INFRA-04 and inverts the scalafmt dialect strategy per REQ INFRA-05 (default `scala3`, override `scala-2.13/` to `scala213source3`).

Purpose: These two files have NO overlap with the upcoming `project/Commons.scala` edits in Plan 02/03, so they can land first and unblock the (currently green) `scalafmtCheckAll` gate immediately after the dialect rewrite.

Output: Updated `project/plugins.sbt` and `.scalafmt.conf` committed onto branch `01-cross-compile-infra` (branched from `upstream/scala-3`).
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md
@.planning/phases/01-cross-compile-infrastructure/01-RESEARCH.md
</context>

<interfaces>
<!-- Upstream baseline content the executor must NOT diverge from beyond the targeted edits. -->

Upstream `upstream/scala-3:project/plugins.sbt` (verbatim):

```sbt
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
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")
```

Upstream `upstream/scala-3:.scalafmt.conf` opening lines (verbatim):

```hocon
//description of properties: https://scalameta.org/scalafmt/docs/configuration.html
version = "3.11.1"
runner.dialect = Scala213Source3
maxColumn = 120
```

The rest of `.scalafmt.conf` (continuationIndent, align, binPack, newlines, rewrite.rules, trailingCommas, importSelectors, optIn, rewrite.neverInfix.excludeFilters) MUST be preserved byte-identical from upstream/scala-3.
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Cut working branch from upstream/scala-3 HEAD</name>
  <files>(none — git plumbing only)</files>
  <read_first>
    - .planning/PROJECT.md (for per-PR workflow if present; else proceed with the commands below)
    - .planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md §specifics
  </read_first>
  <action>
    Execute (in order, abort if any step fails):

    1. `git -C /Users/bkozak/IdeaProjects/scala-commons3 fetch upstream`
    2. Verify upstream HEAD: `git -C /Users/bkozak/IdeaProjects/scala-commons3 rev-parse upstream/scala-3` MUST print a 40-char SHA. RESEARCH.md pins the expected SHA at `1561d8dc` (or newer if upstream/scala-3 advanced since 2026-05-30); record the actual SHA in the SUMMARY.
    3. `git -C /Users/bkozak/IdeaProjects/scala-commons3 checkout -b 01-cross-compile-infra upstream/scala-3`
       - If the branch already exists from a prior attempt, use `git checkout 01-cross-compile-infra && git reset --hard upstream/scala-3` ONLY after confirming with the user. Otherwise prefer `git checkout -B 01-cross-compile-infra upstream/scala-3` ONLY if the user is AFK (this task runs YOLO per STATE.md mode).
    4. Confirm clean working tree: `git status --porcelain` MUST be empty.
    5. Confirm `.planning/` is gitignored: `git check-ignore -v .planning/STATE.md` MUST print `.gitignore:<n>:.planning/    .planning/STATE.md` (any rule that ignores it). If NOT ignored, abort and surface to user — REQ WORKFLOW-05 mandates `.planning/` never appears in any commit diff.

    Do NOT make any file edits in this task; subsequent tasks edit `project/plugins.sbt` and `.scalafmt.conf` on this branch.
  </action>
  <verify>
    <automated>git -C /Users/bkozak/IdeaProjects/scala-commons3 branch --show-current | grep -Fx '01-cross-compile-infra' &amp;&amp; git -C /Users/bkozak/IdeaProjects/scala-commons3 status --porcelain | wc -l | grep -Fx '       0' || git -C /Users/bkozak/IdeaProjects/scala-commons3 status --porcelain | wc -l | tr -d ' ' | grep -Fx '0'</automated>
  </verify>
  <acceptance_criteria>
    - `git -C /Users/bkozak/IdeaProjects/scala-commons3 branch --show-current` prints exactly `01-cross-compile-infra`.
    - `git -C /Users/bkozak/IdeaProjects/scala-commons3 rev-parse HEAD` equals `git rev-parse upstream/scala-3`.
    - `git -C /Users/bkozak/IdeaProjects/scala-commons3 status --porcelain` prints no lines.
    - `git -C /Users/bkozak/IdeaProjects/scala-commons3 check-ignore .planning/STATE.md` exits 0 (file is ignored).
  </acceptance_criteria>
  <done>Branch `01-cross-compile-infra` checked out at `upstream/scala-3` HEAD; tree clean; `.planning/` confirmed ignored.</done>
</task>

<task type="auto">
  <name>Task 2: Bump sbt-mima-plugin 1.1.4 → 1.1.5</name>
  <files>project/plugins.sbt</files>
  <read_first>
    - project/plugins.sbt (current state on branch 01-cross-compile-infra, which equals upstream/scala-3)
  </read_first>
  <action>
    Edit ONLY the `sbt-mima-plugin` line in `/Users/bkozak/IdeaProjects/scala-commons3/project/plugins.sbt`:

    Replace exactly:
    ```
    addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")
    ```
    with exactly:
    ```
    addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")
    ```

    Do NOT change any other line. Do NOT reorder lines. Do NOT add `sbt-tasty-mima` — CONTEXT explicitly defers it to Phase 11.
    The remaining ten `addSbtPlugin` lines and the top `logLevel := Level.Warn` MUST stay byte-identical.

    After editing, run `sbt reload` from the project root (`cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt reload`) and confirm it exits 0 — this proves the plugin resolves on Maven Central. If `sbt reload` fails with `unresolved dependency: com.typesafe#sbt-mima-plugin;1.1.5`, abort: the version is wrong (RESEARCH.md verified 1.1.5 released 2025-02-17, so this would indicate a typo).

    Commit with message exactly: `build(plugins): bump sbt-mima-plugin 1.1.4 -> 1.1.5`
    (Use `git -C /Users/bkozak/IdeaProjects/scala-commons3 commit -m '...' -- project/plugins.sbt`. No GSD nomenclature per REQ WORKFLOW-04.)
  </action>
  <verify>
    <automated>grep -Fxq 'addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")' /Users/bkozak/IdeaProjects/scala-commons3/project/plugins.sbt &amp;&amp; ! grep -Fq '1.1.4' /Users/bkozak/IdeaProjects/scala-commons3/project/plugins.sbt</automated>
  </verify>
  <acceptance_criteria>
    - `grep -Fxq 'addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.5")' project/plugins.sbt` exits 0.
    - `grep -c '^addSbtPlugin' project/plugins.sbt` prints exactly `11` (no plugins added or removed).
    - `grep -Fq 'sbt-tasty-mima' project/plugins.sbt` exits 1 (NOT present — deferred per CONTEXT).
    - `sbt-mima-plugin` appears exactly once: `grep -c 'sbt-mima-plugin' project/plugins.sbt` prints `1`.
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch reload` exits 0 (plugin resolves).
    - Commit message has prefix `build(plugins):` (no `gsd`, no `phase`, no `plan-phase` substrings — `git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1).
  </acceptance_criteria>
  <done>`project/plugins.sbt` declares sbt-mima-plugin 1.1.5; `sbt reload` succeeds; commit landed on `01-cross-compile-infra`.</done>
</task>

<task type="auto">
  <name>Task 3: Invert scalafmt dialect — default scala3, fileOverride scala-2.13/ → scala213source3</name>
  <files>.scalafmt.conf</files>
  <read_first>
    - .scalafmt.conf (current state on branch — full file; needed to preserve all unaffected sections byte-identical)
    - .planning/phases/01-cross-compile-infrastructure/01-RESEARCH.md §"Pattern 6: scalafmt dialect inversion"
  </read_first>
  <action>
    Edit `/Users/bkozak/IdeaProjects/scala-commons3/.scalafmt.conf` as follows.

    (a) Replace the line `runner.dialect = Scala213Source3` with these FOUR lines (in this exact order, after the existing `version = "3.11.1"` line):

    ```
    runner.dialect = scala3
    runner.dialectOverride.allowSignificantIndentation = true
    runner.dialectOverride.allowFewerBraces = true
    ```

    (b) AFTER the existing `maxColumn = 120` line and BEFORE the `continuationIndent {` block, insert a blank line followed by this fileOverride block exactly:

    ```
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
    ```

    (c) Do NOT touch ANY other line in `.scalafmt.conf`. Specifically, preserve byte-identical:
        - `version = "3.11.1"` (do NOT downgrade — accept upstream baseline per CONTEXT)
        - `maxColumn = 120`
        - `continuationIndent { ... }` block
        - `align.preset = none`
        - `binPack { ... }` block
        - `newlines { ... }` block
        - `rewrite.rules = [...]` line
        - `rewrite.sortModifiers.order = [...]` line
        - `rewrite.redundantBraces { ... }` block
        - `trailingCommas = multiple`
        - `importSelectors = singleLine`
        - `optIn { ... }` block
        - `rewrite.neverInfix.excludeFilters = [...]` array

    (d) After editing, run from the project root: `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt scalafmtCheckAll`. It MUST exit 0 against the unchanged upstream/scala-3 source tree (no .scala source files have been edited yet, so any failure indicates the dialect inversion broke parsing of existing files — most likely candidates: files under `core/src/main/scala-2/` or `core/src/main/scala-2.13/` that contained scala3-isms previously parsed under the global `Scala213Source3` dialect). If `scalafmtCheckAll` fails, surface the file list to the user; do NOT auto-format (CONTEXT bans whole-tree sweeps).

    Commit with message exactly: `style(scalafmt): invert dialect — scala3 default + scala-2.13 override to scala213source3`
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; grep -Fxq 'runner.dialect = scala3' .scalafmt.conf &amp;&amp; grep -Fq 'glob:**/src/{main,test}/scala-2.13/**' .scalafmt.conf &amp;&amp; grep -Fq 'runner.dialect = scala213source3' .scalafmt.conf &amp;&amp; ! grep -Fq 'Scala213Source3' .scalafmt.conf &amp;&amp; sbt -batch scalafmtCheckAll</automated>
  </verify>
  <acceptance_criteria>
    - `grep -Fxq 'runner.dialect = scala3' .scalafmt.conf` exits 0 (default dialect is `scala3`, NOT `Scala213Source3`).
    - `grep -Fxq 'version = "3.11.1"' .scalafmt.conf` exits 0 (version preserved from upstream).
    - `grep -c 'runner.dialect = scala213source3' .scalafmt.conf` prints exactly `2` (one per fileOverride glob — scala-2.13 and scala-2).
    - `grep -Fq 'glob:**/src/{main,test}/scala-2.13/**' .scalafmt.conf` exits 0.
    - `grep -Fq 'glob:**/src/{main,test}/scala-2/**' .scalafmt.conf` exits 0.
    - `grep -Fq 'Scala213Source3' .scalafmt.conf` exits 1 (old default removed).
    - `grep -Fq 'maxColumn = 120' .scalafmt.conf` exits 0 (preserved).
    - `grep -Fq 'rewrite.neverInfix.excludeFilters' .scalafmt.conf` exits 0 (preserved).
    - `cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; sbt -batch scalafmtCheckAll` exits 0.
    - Commit message starts with `style(scalafmt):` — `git log -1 --format=%s | grep -E '^style\(scalafmt\):'` exits 0.
    - Commit message contains no GSD nomenclature: `git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
  </acceptance_criteria>
  <done>`.scalafmt.conf` rewritten with `scala3` default + `scala213source3` fileOverride for 2.13 dirs; `scalafmtCheckAll` green; commit landed.</done>
</task>

</tasks>

<verification>
After all three tasks complete on branch `01-cross-compile-infra`:

1. `git -C /Users/bkozak/IdeaProjects/scala-commons3 log upstream/scala-3..HEAD --oneline` shows exactly 2 commits (the plugins bump and the scalafmt invert — Task 1 made no commit).
2. `git -C /Users/bkozak/IdeaProjects/scala-commons3 log upstream/scala-3..HEAD --name-only` shows ONLY `project/plugins.sbt` and `.scalafmt.conf`.
3. `git -C /Users/bkozak/IdeaProjects/scala-commons3 log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0` (REQ WORKFLOW-05).
4. `git -C /Users/bkozak/IdeaProjects/scala-commons3 log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1 (REQ WORKFLOW-04).
5. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch scalafmtCheckAll` exits 0 (QUALITY-01: no new warnings introduced; INFRA-05 dialect applied correctly).
6. `cd /Users/bkozak/IdeaProjects/scala-commons3 && sbt -batch reload` exits 0 (INFRA-04: mima 1.1.5 resolves).
</verification>

<success_criteria>
- Branch `01-cross-compile-infra` exists at upstream/scala-3 + 2 commits.
- `sbt-mima-plugin` declared at `1.1.5` (INFRA-04).
- `.scalafmt.conf` declares `scala3` default and `scala213source3` for `scala-2.13/` and `scala-2/` paths (INFRA-05).
- `scalafmtCheckAll` exits 0 (QUALITY-01 — no new warnings, no `@nowarn`/`-Wconf` introduced; QUALITY-03 — no MiMa filter changes so per-filter justification preservation is trivially intact).
- No `.planning/` paths in diff; no GSD nomenclature in commit messages (WORKFLOW-04/05).
</success_criteria>

<output>
After completion, create `.planning/phases/01-cross-compile-infrastructure/01-01-SUMMARY.md` capturing:
- Actual `upstream/scala-3` HEAD SHA at branch-cut time
- Exact commit SHAs landed on `01-cross-compile-infra`
- `sbt scalafmtCheckAll` run output (last 5 lines)
- Confirmation that `project/Commons.scala` and `.github/workflows/ci.yml` are UNTOUCHED (those are Plan 02 and Plan 03 territory)
</output>
