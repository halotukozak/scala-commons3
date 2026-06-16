---
phase: 01-big-bang-comment-and-green
plan: 05
type: execute
wave: 5
depends_on: [04]
files_modified:
  - core/src/test/scala/**
  - core/jvm/src/test/scala/**
  - core/js/src/test/scala/**
  - mongo/src/test/scala/**
  - mongo/jvm/src/test/scala/**
  - mongo/js/src/test/scala/**
  - hocon/src/test/scala/**
  - macros/src/test/scala/**
autonomous: true
commit_docs: false
requirements: [COMMENT-01, COMMENT-02, COMMENT-04, COMPILE-02, QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05]

must_haves:
  truths:
    - "sbt Test/compile exits 0 across every enabled module"
    - "Test sources commented per-file (broken test classes individually)"
  artifacts:
    - path: "{core,mongo,hocon,macros}/{,jvm,js}/src/test/scala"
      provides: "Test sources with broken classes commented; surviving tests untouched"
  key_links:
    - from: "all enabled modules' Test/compile"
      to: "exit 0"
      via: "per-file commenting of test classes that depend on commented production code"
      pattern: "TODO\\[scala3-port\\]"
---

<objective>
Make `sbt Test/compile` exit 0 across every enabled module. Per-file commenting of broken test classes — NOT whole `Test/` dirs (CONTEXT lock).

Purpose: Close the COMPILE-02 acceptance gate. Tests don't need to RUN in Phase 1 — only compile. Execution is deferred to restoration phases.

Output: `sbt Test/compile` exit 0. Each commented test class tagged `// TODO[scala3-port]: <test name> — depends on <feature> (S|M|L)`.
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
@.planning/phases/01-big-bang-comment-and-green/01-04-SUMMARY.md

<interfaces>
Test source totals (RESEARCH baseline 1561d8dc):
- core: 81 test files
- mongo: 21 test files
- hocon: 4 test files
- analyzer/jetty/spring: disabled, not in scope
- macros: 0 test files in upstream baseline (per RESEARCH inventory)
- TOTAL: ~133 test files to triage

Per-file commenting convention (CONTEXT lock): "Test sources commented per-file (broken test classes individually, not whole Test/ dirs)."

Pattern (same as production code):
```scala
package com.avsystem.commons.serialization

import org.scalatest.funsuite.AnyFunSuite

// TODO[scala3-port]: GenCodecTest — depends on materialize (L)
/*
class GenCodecTest extends AnyFunSuite {
  ...
}
*/

// surviving tests in same file stay uncommented
```

If a test file has multiple classes/objects and SOME survive — comment only the broken ones. If ALL classes in the file are broken — wrap everything after the package decl + surviving imports in `/* */`.

Test execution NOT in scope. Acceptance gate is `Test/compile` only.
</interfaces>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Comment broken test classes until Test/compile is green</name>
  <files>core/src/test/scala/**, core/jvm/src/test/scala/**, core/js/src/test/scala/**, mongo/**/src/test/scala/**, hocon/src/test/scala/**, macros/src/test/scala/**</files>
  <read_first>
    - Run `sbt -batch Test/compile 2>&1 | tail -100` for the current error surface (with Plans 01-04 merged).
    - Run `find core mongo hocon macros -name '*.scala' -path '*/src/test/*' 2>/dev/null | wc -l` to confirm test file count.
    - .planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md (test sources: per-file commenting locked)
  </read_first>
  <action>
    Compile-driven iteration on `Test/compile`:

    1. `sbt -batch Test/compile 2>&1 | tee /tmp/test-errors.log`. Group errors by file.
    2. For each broken test file:
       - Open the file. Identify which class(es) are broken (compiler error references).
       - Wrap each broken class in `/* */` with a `// TODO[scala3-port]: <ClassName> — depends on <feature> (S|M|L)` tag.
       - Preserve `package` decl, imports needed by surviving classes (if any), and uncommented surviving classes.
       - If ALL classes in the file are broken, wrap-everything-after-package-decl pattern (single big `/* */`).
    3. Re-run `sbt Test/compile`. Repeat until exit 0.

    Order of attack (suggested, faster feedback):
    1. core test sources first (largest volume; many cascading errors)
    2. mongo
    3. hocon
    4. JS test variants (`core/js/src/test/`, `mongo/js/src/test/`)

    Efficiency tip: run a single `sbt -batch ';commons-jvm/Test/compile ;commons-js/Test/compile'` to get the full error log in one shot, then attack files in order of most-errors-first.

    **Commit strategy:** Multiple atomic commits OK, by module/subpackage:
    - `refactor(core): comment broken tests pending Scala 3 port`
    - `refactor(mongo): comment broken tests pending Scala 3 port`
    - `refactor(hocon): comment broken tests pending Scala 3 port`
    - `refactor(core-js): comment broken tests pending Scala 3 port`
    - `refactor(mongo-js): comment broken tests pending Scala 3 port`

    Memory-rule audit before each commit: no `@nowarn`/`-Wconf`, no `.planning/` paths, no GSD nomenclature, conventional prefix.

    Sanity (final gates for this plan AND the heart of Phase 1):
    - `sbt -batch Test/compile` exit 0
    - `sbt -batch scalafmtCheckAll` exit 0
    - `sbt -batch compile` exit 0 (regression check — production code still green)
  </action>
  <verify>
    <automated>sbt -batch ';compile ;Test/compile ;scalafmtCheckAll' 2>&1 | tail -10 | grep -qE 'success' && ! git diff upstream/scala-3..HEAD -- '**/src/test/scala/**/*.scala' | grep -qE '^\+.*(@nowarn|-Wconf)'</automated>
  </verify>
  <done>
    `sbt Test/compile` exits 0 across all enabled modules. `compile` still green. `scalafmtCheckAll` green. Each commented test class has a TODO tag. No `@nowarn`/`-Wconf` introduced.
  </done>
</task>

</tasks>

<verification>
- `sbt -batch compile` exit 0
- `sbt -batch Test/compile` exit 0
- `sbt -batch scalafmtCheckAll` exit 0
- `! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'`
- `git grep -cE 'TODO\[scala3-port\]' -- '**/src/test/scala'` ≥ 20 (rough lower bound — depends on commented-class count)
- Conventional commit prefixes
</verification>

<success_criteria>
1. `sbt Test/compile` exits 0 across every enabled module.
2. Per-file commenting — surviving test classes in mixed files remain uncommented.
3. Each commented class has `// TODO[scala3-port]: <ClassName> — depends on <feature> (S|M|L)`.
4. No `@nowarn`/`-Wconf` introduced.
5. Conventional commit prefixes, no GSD nomenclature.
6. COMPILE-02 acceptance gate satisfied.
</success_criteria>

<output>
After completion, create `.planning/phases/01-big-bang-comment-and-green/01-05-SUMMARY.md`:
- Test file totals: commented (whole) vs partially-commented vs untouched (survives compile as-is)
- Per-module TODO tag counts in test sources
- Confirmation of `Test/compile` green
- Confirmation: COMMENT-01..04, COMPILE-02, QUALITY-01 satisfied
</output>
