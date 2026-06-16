---
phase: 01-big-bang-comment-and-green
plan: 04
type: execute
wave: 4
depends_on: [03]
files_modified:
  - core/js/src/main/scala/**
  - mongo/js/src/main/scala/**
  - benchmark/js/src/main/scala/**
autonomous: true
commit_docs: false
requirements: [COMMENT-01, COMMENT-02, COMMENT-03, COMMENT-05, COMPILE-01, QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05]

must_haves:
  truths:
    - "sbt commons-js/compile exits 0 (every JS module compiles on Scala 3)"
    - "JS sources have same per-file commenting convention applied"
  artifacts:
    - path: "core/js/src/main/scala"
      provides: "JS-specific core sources commented where broken"
    - path: "mongo/js/src/main/scala"
      provides: "JS-specific mongo sources commented where broken"
    - path: "benchmark/js/src/main/scala"
      provides: "JS-specific benchmark sources commented where broken"
  key_links:
    - from: "commons-js aggregate"
      to: "compile green"
      via: "every js module compiles on Scala.js 1.21.0 + Scala 3.8.2"
      pattern: "TODO\\[scala3-port\\]"
---

<objective>
Make `commons-js` aggregate compile on Scala 3 (Scala.js 1.21.0). Builds on Plans 02 (core) and 03 (mongo) — JS variants share most code with their JVM counterparts but have js-specific source dirs.

Purpose: Close JS-side compile gates. After this plan, all enabled modules (JVM + JS) compile.

Output: `sbt commons-js/compile` exit 0.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md
@.planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md
@.planning/phases/01-big-bang-comment-and-green/01-02-SUMMARY.md
@.planning/phases/01-big-bang-comment-and-green/01-03-SUMMARY.md

<interfaces>
JS modules (per RESEARCH Module Inventory):
- `core-js` lives at `core/js/src/main/scala/**` (shares with core via `core/src/main/scala/**` shared sources + the `core/js/` js-platform-only layer)
- `mongo-js` lives at `mongo/js/src/main/scala/**`
- `benchmark-js` lives at `benchmark/js/src/main/scala/**` (if present)

ScalaJS 1.21.0 supports Scala 3.8.2 (RESEARCH "ScalaJS, sbt-nosbt, sbt-jmh"). No plugin bumps. The `-P:scalajs:mapSourceURI:...` flag stays as-is (RESEARCH Pitfall 2).

Most JS-specific files are thin platform shims — small per-file commenting expected unless they consume codecs that were commented in Plans 02/03.

Commenting convention: same as prior plans — `/* */` blocks with `// TODO[scala3-port]: <desc> [(S|M|L)]`.
</interfaces>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Comment broken JS sources until commons-js/compile is green</name>
  <files>core/js/src/main/scala/**, mongo/js/src/main/scala/**, benchmark/js/src/main/scala/**</files>
  <read_first>
    - Run `find core/js mongo/js benchmark/js -name '*.scala' -path '*/main/*' 2>/dev/null` to enumerate js-platform-only sources.
    - Run `sbt -batch commons-js/compile 2>&1 | tail -50` for the current Scala 3 + Scala.js error surface (with Plans 02/03 already merged).
    - .planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md ("ScalaJS, sbt-nosbt, sbt-jmh", Pitfall 2)
  </read_first>
  <action>
    Compile-driven iteration:

    1. `sbt -batch commons-js/compile`. Capture error log.
    2. For each error, locate the offending def/class/object in `core/js/`, `mongo/js/`, or `benchmark/js/`. Wrap in `/* */` with `// TODO[scala3-port]: <desc>` tag.
    3. Repeat until exit 0.

    Notes:
    - Many JS errors will cascade from already-commented JVM-side defs (shared sources in `core/src/main/scala/` were commented in Plan 02). Those should already be handled — if a JS-only file imports a now-commented JVM type, comment the import OR the consumer file.
    - For files in `core/js/`, `mongo/js/`, `benchmark/js/` with no surviving code post-commenting, use the all-body-wrap pattern (preserve package decl only).

    **Commit strategy:**
    - `refactor(core-js): comment broken JS sources pending Scala 3 port`
    - `refactor(mongo-js): comment broken JS sources pending Scala 3 port`
    - `refactor(benchmark-js): comment broken JS sources pending Scala 3 port` (only if benchmark-js exists)

    Memory-rule audit before each commit: no `@nowarn`/`-Wconf` (already absent — Werror deferred per Plan 01), no `.planning/` paths, no GSD nomenclature, conventional prefix.

    Sanity:
    - `sbt -batch commons-js/compile` exit 0
    - `sbt -batch scalafmtCheckAll` exit 0
  </action>
  <verify>
    <automated>sbt -batch commons-js/compile 2>&1 | tail -5 | grep -qE 'success' && sbt -batch scalafmtCheckAll 2>&1 | tail -3 | grep -qE 'success' && ! git diff upstream/scala-3..HEAD -- 'core/js/**/*.scala' 'mongo/js/**/*.scala' 'benchmark/js/**/*.scala' | grep -qE '^\+.*(@nowarn|-Wconf)'</automated>
  </verify>
  <done>
    `sbt commons-js/compile` exits 0. JS-specific broken sources commented per convention. `scalafmtCheckAll` green. No warning suppressions introduced.
  </done>
</task>

</tasks>

<verification>
- `sbt -batch commons-js/compile` exits 0
- `sbt -batch ';commons-jvm/compile ;commons-js/compile'` (combined gate) exits 0
- `sbt -batch scalafmtCheckAll` exits 0
- `! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'`
- Conventional commit prefixes
</verification>

<success_criteria>
1. `sbt commons-js/compile` exit 0.
2. Combined JVM + JS compile both green.
3. All commented blocks have `// TODO[scala3-port]:` tags.
4. No `@nowarn`/`-Wconf` introduced.
5. Conventional commit prefixes, no GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/01-big-bang-comment-and-green/01-04-SUMMARY.md`:
- Per-module JS TODO tag counts
- Confirmation of `commons-js/compile` green
- Confirmation: COMMENT-01..03, COMPILE-01 (full — both JVM and JS), QUALITY-01 satisfied
</output>
