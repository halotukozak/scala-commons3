---
phase: 01-big-bang-comment-and-green
plan: 03
type: execute
wave: 3
depends_on: [02]
files_modified:
  - hocon/src/main/scala/**
  - mongo/jvm/src/main/scala/**
  - mongo/src/main/scala/**
  - benchmark/jvm/src/main/scala/**
  - core/src/main/scala/com/avsystem/commons/serialization/cbor/**
autonomous: true
commit_docs: false
requirements: [COMMENT-01, COMMENT-02, COMMENT-03, COMMENT-05, COMPILE-01, QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05]

must_haves:
  truths:
    - "sbt commons-jvm/compile exits 0 (every enabled module in jvm aggregate compiles)"
    - "hocon, mongo (JVM), cbor sub-package, benchmark all green"
    - "analyzer/jetty/spring/comprof remain dropped from aggregate (no work needed if Plan 01 worked)"
  artifacts:
    - path: "hocon/src/main/scala"
      provides: "Hocon module — likely small commenting (RESEARCH: 0 macro defs, mostly data)"
    - path: "mongo/jvm/src/main/scala"
      provides: "Mongo JVM module — 3 macro defs to comment + GenCodec consumers"
    - path: "core/src/main/scala/com/avsystem/commons/serialization/cbor"
      provides: "CBOR sub-package (lives inside core, not separate module per RESEARCH)"
    - path: "benchmark/jvm/src/main/scala"
      provides: "JMH benchmarks — codec-consumer surface commented"
  key_links:
    - from: "commons-jvm aggregate"
      to: "compile green"
      via: "every aggregated module's Compile/compile exit 0"
      pattern: "TODO\\[scala3-port\\]"
---

<objective>
Make every enabled module in the `commons-jvm` aggregate compile on Scala 3 by commenting broken defs. Builds on Plan 02's macros+core compile.

Purpose: Close out JVM-side compile gates. `sbt commons-jvm/compile` exit 0 means the whole jvm aggregate is green — sets up Plans 04 (JS variants) and 05 (tests).

Output: All four remaining JVM modules (hocon, mongo, cbor sub-package within core, benchmark) compile on Scala 3. Multiple conventional-prefixed atomic commits per module.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md
@.planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md
@.planning/phases/01-big-bang-comment-and-green/01-01-SUMMARY.md
@.planning/phases/01-big-bang-comment-and-green/01-02-SUMMARY.md

<interfaces>
Module inventory (RESEARCH baseline 1561d8dc):

| Module | Main files | Test files | scala-2 macro defs | Expected commenting volume |
|--------|-----------|------------|---------------------|-----------------------------|
| hocon | 9 | 4 | 0 | LOW — mostly data wrappers; likely compiles cleanly |
| mongo (JVM portion) | most of 82 | 21 | 3 | MEDIUM — typed mongo wrappers + GenCodec consumers |
| cbor (sub-package in core) | n/a (lives in core/.../serialization/cbor/) | n/a | 0 macro defs of its own | MEDIUM — annotation aggregate machinery + GenCodec consumers |
| benchmark/jvm | 11 | 0 | 0 | MEDIUM — pure codec consumers; will break wherever materialize is gone |

Note: CONTEXT lists `cbor` as a module but RESEARCH clarifies it's a sub-package inside `core/src/main/scala/com/avsystem/commons/serialization/cbor/`. Treat as part of core.

Commenting convention (same as Plan 02): `/* */` blocks with `// TODO[scala3-port]: <desc> [(S|M|L)]` tag immediately above.
</interfaces>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Hocon + cbor sub-package compile-green</name>
  <files>hocon/src/main/scala/**, core/src/main/scala/com/avsystem/commons/serialization/cbor/**</files>
  <read_first>
    - Run `find hocon/src/main/scala -name '*.scala'` and `find core/src/main/scala/com/avsystem/commons/serialization/cbor -name '*.scala' 2>/dev/null` to enumerate.
    - Run `sbt -batch hocon/compile 2>&1 | tail -30` and `sbt -batch commons-core/compile 2>&1 | tail -30` to see current error surface (cbor errors surface via core).
    - .planning/phases/01-big-bang-comment-and-green/01-02-SUMMARY.md (what was already commented in core under cbor)
  </read_first>
  <action>
    **Hocon (likely small):**
    1. `sbt -batch hocon/compile`. If green: done; commit with empty diff means skip.
    2. If errors: per-file commenting of broken defs (same `/* */` + TODO tag pattern). Hocon is mostly data wrappers — expect ≤5 commented blocks, mostly around GenCodec consumers if any.
    3. Effort tags lean S (Hocon is small surface).

    **CBOR sub-package (inside core):**
    1. If Plan 02 didn't already cover it (check `git grep -nE 'TODO\[scala3-port\]' core/src/main/scala/com/avsystem/commons/serialization/cbor`), iterate compile-driven:
       - `sbt -batch commons-core/compile 2>&1 | grep cbor`
       - Comment broken defs file-by-file with TODO tags.
    2. CBOR uses annotation aggregate machinery + GenCodec consumers — expect medium volume.

    **Commit strategy:**
    - `refactor(hocon): comment broken defs pending Scala 3 port` (if any changes)
    - `refactor(core): comment cbor sub-package pending Scala 3 port` (if needed; if cbor already covered by Plan 02 commits, skip)

    Memory-rule audit before each commit: no `@nowarn`/`-Wconf` introduced, no `.planning/` paths, no GSD nomenclature, conventional prefix.

    Sanity:
    - `sbt -batch hocon/compile` exit 0
    - `sbt -batch commons-core/compile` exit 0 (unchanged from Plan 02 — should still be green)
    - `sbt -batch scalafmtCheckAll` exit 0
  </action>
  <verify>
    <automated>sbt -batch ';hocon/compile ;commons-core/compile' 2>&1 | tail -10 | grep -qE 'success' && sbt -batch scalafmtCheckAll 2>&1 | tail -3 | grep -qE 'success' && ! git diff upstream/scala-3..HEAD -- 'hocon/**/*.scala' 'core/src/main/scala/com/avsystem/commons/serialization/cbor/**/*.scala' | grep -qE '^\+.*(@nowarn|-Wconf)'</automated>
  </verify>
  <done>
    `sbt hocon/compile` and `sbt commons-core/compile` both exit 0. Cbor sub-package fully commented where broken. Every commented block has a TODO tag. No `@nowarn`/`-Wconf` introduced.
  </done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Mongo (JVM) + benchmark compile-green</name>
  <files>mongo/jvm/src/main/scala/**, mongo/src/main/scala/**, benchmark/jvm/src/main/scala/**</files>
  <read_first>
    - Run `find mongo -name '*.scala' -path '*/main/*' -not -path '*/js/*'` to enumerate JVM-side mongo.
    - Run `find benchmark -name '*.scala' -path '*/main/*' -not -path '*/js/*'` (or wherever benchmark lives).
    - Run `git grep -lE '= macro\b' mongo` to list the 3 macro defs.
    - Run `sbt -batch mongo/compile 2>&1 | tail -30` and `sbt -batch benchmark/compile 2>&1 | tail -30` for current error surface.
  </read_first>
  <action>
    **Mongo (JVM):**
    1. Comment the 3 known macro defs (locations from `git grep -E '= macro' mongo`) — wrap each in `/* */` with `// TODO[scala3-port]: <feature> (M|L)`.
    2. Compile-driven iteration: `sbt -batch mongo/compile`, comment whatever breaks, repeat until green. Mongo has GenCodec consumers — expect cascading commenting.
    3. Per CONTEXT: mongo is "Keep, comment broken". Driver wrapper code likely needs commenting around typed APIs that depend on materialize.

    **Benchmark (JVM):**
    1. `sbt -batch benchmark/compile`. Likely red because benchmarks consume codecs that are now commented.
    2. Per CONTEXT and RESEARCH Open Question 3: "Comment broken benchmark sources file-by-file; if a benchmark file has no surviving code, comment everything except the package decl."
    3. For benchmark files with no surviving code, use the all-body-wrap pattern (same as macros in Plan 02 Task 1):
       ```scala
       package com.avsystem.commons.benchmark

       // TODO[scala3-port]: <bench name> — depends on GenCodec materialize (M)
       /*
       <original body>
       */
       ```

    **Commit strategy:**
    - `refactor(mongo): comment broken defs pending Scala 3 port`
    - `refactor(benchmark): comment codec-consumer benchmarks pending Scala 3 port`

    Memory-rule audit before each commit (same as Task 1).

    Sanity (final gate for this plan):
    - `sbt -batch commons-jvm/compile` exit 0 (the AGGREGATE — every enabled module green)
    - `sbt -batch scalafmtCheckAll` exit 0
  </action>
  <verify>
    <automated>sbt -batch commons-jvm/compile 2>&1 | tail -10 | grep -qE 'success' && sbt -batch scalafmtCheckAll 2>&1 | tail -3 | grep -qE 'success' && ! git diff upstream/scala-3..HEAD -- 'mongo/**/*.scala' 'benchmark/**/*.scala' | grep -qE '^\+.*(@nowarn|-Wconf)'</automated>
  </verify>
  <done>
    `sbt commons-jvm/compile` exits 0 — entire JVM aggregate compiles on Scala 3. Mongo and benchmark broken parts commented with TODO tags. `scalafmtCheckAll` green. No warning suppressions introduced.
  </done>
</task>

</tasks>

<verification>
- `sbt -batch commons-jvm/compile` exits 0 (covers macros, core, hocon, mongo, benchmark — every enabled JVM module)
- `sbt -batch scalafmtCheckAll` exits 0
- `! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'`
- Conventional commit prefixes on all commits since Plan 02 tip
- Module-level TODO tag counts logged in SUMMARY
</verification>

<success_criteria>
1. Every module enabled in the `commons-jvm` aggregate compiles green on Scala 3.
2. `sbt commons-jvm/compile` exit 0.
3. `scalafmtCheckAll` exit 0.
4. Every commented block tagged with `// TODO[scala3-port]: <desc> [(S|M|L)]`.
5. No `@nowarn`/`-Wconf` introduced (memory rule).
6. Conventional commit prefixes, no GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/01-big-bang-comment-and-green/01-03-SUMMARY.md`:
- Per-module TODO tag counts (hocon / mongo / cbor-subpkg / benchmark)
- Any module that needed zero commenting (likely candidate: hocon)
- Confirmation of `commons-jvm/compile` green
- Confirmation: COMMENT-01..03, COMMENT-05, COMPILE-01 (JVM portion), QUALITY-01 satisfied
</output>
