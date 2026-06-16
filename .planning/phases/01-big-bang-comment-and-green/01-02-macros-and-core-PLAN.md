---
phase: 01-big-bang-comment-and-green
plan: 02
type: execute
wave: 2
depends_on: [01]
files_modified:
  - macros/**
  - core/src/main/scala/**
  - core/jvm/src/main/scala/**
  - build.sbt
  - project/Commons.scala
autonomous: true
commit_docs: false
requirements: [COMMENT-01, COMMENT-02, COMMENT-03, COMMENT-05, COMPILE-01, QUALITY-01, QUALITY-02, WORKFLOW-04, WORKFLOW-05]

must_haves:
  truths:
    - "commons-macros module DELETED from build (no longer an sbt project)"
    - "commons-core compile exits 0 on Scala 3"
    - "Every stubbed body has a // TODO[scala3-port]: tag immediately above"
    - "No new @nowarn / -Wconf introduced anywhere"
  artifacts:
    - path: "build.sbt"
      provides: "macros project removed; core no longer dependsOn(macros)"
    - path: "core/src/main/scala"
      provides: "Surviving compilable subset of core; broken defs stubbed with `???` + TODO tags"
  key_links:
    - from: "build.sbt"
      to: "commons-core/compile"
      via: "macros module + dependsOn(macros) wiring removed"
      pattern: "commons-macros"
    - from: "core/src/main/scala"
      to: "commons-core/compile"
      via: "scala-2 macro defs replaced with `= ???` stubs preserving signatures + TODO tag"
      pattern: "TODO\\[scala3-port\\]"
---

<objective>
Delete the `commons-macros` module outright and make `commons-core` compile on Scala 3 by replacing every broken definition body with `???` stubs (keeping signatures intact).

Purpose: Land green `commons-core/compile` on Scala 3, which unblocks every downstream module (mongo/hocon/cbor/benchmark all `dependsOn(core)`). Macros module is pure scala-2 macro infrastructure with no Scala 3 analogue worth keeping — drop it entirely per [[project-deletable-modules]]. Stub-don't-comment strategy per [[feedback-stub-over-comment]] preserves callers' compilation.

Output: `commons-macros` no longer exists as an sbt project. `sbt commons-core/compile` exits 0. Multiple atomic commits OK; each conventional-prefixed (`refactor(core):`, `build:`).
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md
@.planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md
@.planning/phases/01-big-bang-comment-and-green/01-01-SUMMARY.md

<strategy_change>
**SUPERSEDES PLAN-AS-WRITTEN.** User directive 2026-06-01:

1. **`commons-macros` module: DELETE** (not comment, not stub). Pure scala-2 macro infrastructure (`c.universe`, blackbox/whitebox); zero portable surface in Scala 3 dialect. Remove from `build.sbt` aggregate, drop `dependsOn(macros)` from every module, delete `macros/` source tree.

2. **`commons-core` broken defs: STUB with `???`**, not block-comment. Keeps signatures in the namespace so callers continue to compile. Block-commenting (`/* ... */` around whole defs) removes them from the namespace → cascade compile breakage everywhere they're referenced.

   Stub pattern:
   ```scala
   // TODO[scala3-port]: GenCodec.materialize (whitebox macro) (L)
   implicit def materialize[T]: GenCodec[T] = ???
   ```

   For a `val`:
   ```scala
   // TODO[scala3-port]: BlackboxThing.foo (S)
   val foo: ResultType = ???
   ```

   For an entire `object`/`class` whose every member is broken: stub each member individually; do NOT delete the type.

3. **MIGRATION.md ## Will Not Migrate**: add `commons-macros` + rationale (Plan 06 commits this section).
</strategy_change>

<interfaces>
Stub convention (LOCKED, supersedes prior commenting convention for this plan):

```scala
package com.avsystem.commons.serialization

import com.avsystem.commons.misc.SomeStillWorkingThing  // keep iff still referenced

object GenCodec {
  // TODO[scala3-port]: materialize (whitebox macro) (L)
  implicit def materialize[T]: GenCodec[T] = ???
}
```

Rules:
- Replace the BODY only with `???`. Keep `def`/`val`/`implicit def`/`given`/`class`/`object` signatures intact.
- TODO tag on its own line immediately above the stubbed def. Description ~50 chars, grep-friendly. Optional `(S/M/L)` suffix.
- Prune unused imports after stubbing — DO NOT add `@nowarn`.
- `// format: off` permitted only if scalafmt actually complains (rare since `???` is valid Scala 3).
- If the original return type was inferred from the macro expansion, supply an explicit return type (often `Any` or the most-specific known type from RESEARCH). Mark with `// TODO[scala3-port]: tighten return type` if forced to widen.

When to delete vs stub (within core):
- **Stub:** the type or member name is referenced from outside the file/module → keep the signature alive with `= ???`.
- **Delete:** the def is truly internal and unreferenced (rare; verify with `git grep`).

Volume estimates (RESEARCH "Module Inventory" on baseline 1561d8dc):
- macros: 28 files in src/main/scala — ALL deleted (module removed).
- core: 146 main files, 35 contain `= macro` defs → stubbed with `???`. Files importing `commons.macros.*` get those imports pruned + any types they referenced stubbed.
</interfaces>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Delete commons-macros module</name>
  <files>build.sbt, project/Commons.scala, macros/**</files>
  <read_first>
    - `cat build.sbt` — locate `commons-macros` project def and every `.dependsOn(macros)` reference
    - `grep -rn 'commons.macros\|commons-macros\|macros %' project/ build.sbt`
    - `find macros -type f` — enumerate files about to be deleted
  </read_first>
  <action>
    1. Remove `commons-macros` lazy val project from `build.sbt` (and from any aggregate `commonsJVM`/`commonsJS`/`commons` aggregator).
    2. Remove every `.dependsOn(macros)` (or `.dependsOn(macros % "compile-internal, test-internal")`) clause from sibling projects.
    3. If `project/Commons.scala` has a `macroDependencies` helper or a `mkSourceDirs("macros", ...)`-style shared setting, drop it.
    4. `git rm -r macros/` (deletes the source tree).
    5. `sbt -batch reload` — must succeed without referencing `commons-macros`.
    6. `sbt -batch projects` — must NOT list `commons-macros`.

    Memory-rule audit:
    - `git diff --staged -- .planning/` empty
    - No GSD nomenclature in commit message

    Commit: `git add -A build.sbt project/ && git rm -r macros/ && git commit -m "build: drop commons-macros module (deleted from Scala 3 port)"`.
  </action>
  <verify>
    <automated>! grep -rn 'commons-macros\|dependsOn(macros)' build.sbt project/ && ! test -d macros && sbt -batch projects 2>&1 | grep -v '^\[' | grep -qv 'commons-macros'</automated>
  </verify>
  <done>
    `macros/` directory deleted from working tree and index. `build.sbt` no longer defines `commons-macros` and no module `dependsOn(macros)`. `sbt reload` clean.
  </done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Stub broken defs in commons-core until compile is green</name>
  <files>core/src/main/scala/**, core/jvm/src/main/scala/**</files>
  <read_first>
    - Run `sbt -batch commons-core/compile 2>&1 | tail -50` to get the current Scala 3 error surface after Task 1 (macros deletion will surface NEW errors in core where it imported `commons.macros.*`).
    - Run `git grep -nE '= macro\b' core/src/main/scala` to enumerate macro defs (RESEARCH: ~35 hits).
    - Run `git grep -nE 'import com\.avsystem\.commons\.macros' core/src/main/scala` — every import of the deleted module.
    - .planning/phases/01-big-bang-comment-and-green/01-RESEARCH.md
    - .planning/phases/01-big-bang-comment-and-green/01-CONTEXT.md
    - ~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md (especially [[feedback-stub-over-comment]] and [[project-deletable-modules]])
  </read_first>
  <action>
    Iterative loop until `sbt commons-core/compile` exits 0. Per-def granularity, `= ???` stubs, `// TODO[scala3-port]:` tags.

    **Pass 1 — preemptive stubbing of known-broken defs:**
    1. List all files with scala-2 macro defs: `git grep -lE '= macro\b' core/src/main/scala`.
    2. For each, replace the macro RHS with `???`:
       ```scala
       // TODO[scala3-port]: GenCodec.materialize (whitebox macro) (L)
       implicit def materialize[T]: GenCodec[T] = ???
       ```
       If the original was `def x = macro Y.z[T]` (no explicit return type), supply the most-specific return type you can infer from RESEARCH or the macro impl signature. If unclear, use the widest type that still satisfies the surrounding code (often the type the def is being assigned to — e.g. for `implicit def`, the implicit's declared interface).
    3. Prune every `import com.avsystem.commons.macros.*` line (the package no longer exists). Stub any reference to a name from that package by introducing a local placeholder type or removing the reference.

    **Pass 2 — compile-driven iteration:**
    1. `sbt -batch commons-core/compile 2>&1 | tee /tmp/core-errors.log`.
    2. Group errors by file. For each:
       - If error is "value X is not a member of Y" → X was probably defined by a now-stubbed macro. Stub the caller too, OR add the missing member as a `def x: T = ???` stub on Y.
       - If error is unrelated Scala 3 syntax (e.g. `_*` varargs, `private[this]` final-val), apply the minimum fix.
    3. After each batch of edits, re-run compile. Repeat until exit 0.
    4. For import-related errors, prune the import — DO NOT add `@nowarn` (memory rule).

    **Effort tag heuristic** (S/M/L) for TODO descriptions:
    - S: trivial extension method, one-off codec
    - M: single-feature port (HOCON wrapper, mongo helper)
    - L: derivation, GenCodec, RPC framework

    **Commit strategy:** Multiple atomic commits OK and encouraged. Group by subpackage:
    - `refactor(core): stub Scala 2 macro defs in serialization`
    - `refactor(core): stub GenCodec derivation surface`
    - `refactor(core): stub RPC framework`
    - etc.

    Memory-rule audit before each commit:
    - `git diff --staged | grep -E '@nowarn|-Wconf'` → zero
    - `git diff --staged -- .planning/` → empty
    - No GSD nomenclature in commit messages
    - No `/* ... */` block-commented defs in staged hunks (we use `???` now, not block-comments)

    Final sanity:
    - `sbt -batch commons-core/compile` exit 0
    - `sbt -batch scalafmtCheckAll` exit 0
    - `git grep -nE 'TODO\[scala3-port\]' core/src/main/scala | wc -l` ≥ 35
  </action>
  <verify>
    <automated>sbt -batch commons-core/compile 2>&1 | tail -5 | grep -qE 'success' && sbt -batch scalafmtCheckAll 2>&1 | tail -3 | grep -qE 'success' && ! git diff upstream/scala-3..HEAD -- 'core/**/*.scala' | grep -qE '^\+.*(@nowarn|-Wconf)' && test "$(git grep -cE 'TODO\[scala3-port\]' -- 'core/src/main/scala' | awk -F: '{s+=$2} END {print s}')" -ge 35</automated>
  </verify>
  <done>
    `sbt commons-core/compile` exits 0. Every broken def stubbed with `???` + TODO tag (≥35 tags in core). `scalafmtCheckAll` green. No `@nowarn`/`-Wconf`. Multiple conventional-prefixed commits on `01-big-bang`.
  </done>
</task>

</tasks>

<verification>
- `! test -d macros` (module physically gone)
- `! grep -rn 'commons-macros\|dependsOn(macros)' build.sbt project/`
- `sbt -batch commons-core/compile` exit 0
- `sbt -batch scalafmtCheckAll` exit 0
- `git grep -cE 'TODO\[scala3-port\]' -- 'core/src/main/scala'` ≥ 35
- `! git diff upstream/scala-3..HEAD | grep -qE '^\+.*(@nowarn|-Wconf)'`
- All commits use conventional prefixes; no GSD nomenclature
</verification>

<success_criteria>
1. `commons-macros` module deleted from build and working tree.
2. `commons-core` compiles green on Scala 3 with broken defs stubbed via `???`.
3. Every stub has a `// TODO[scala3-port]: <description> [(S|M|L)]` tag.
4. `scalafmtCheckAll` green.
5. Zero new `@nowarn`/`-Wconf` introduced.
6. Commits are atomic and conventional-prefixed.
</success_criteria>

<output>
After completion, create `.planning/phases/01-big-bang-comment-and-green/01-02-SUMMARY.md`:
- Macros module removal: files deleted count, build.sbt diff summary
- Total `TODO[scala3-port]` stub tags created in core
- Subpackage commit breakdown
- Notable surviving (NOT stubbed) areas in core — the "what compiles" baseline
- Files where stubbing forced a return-type widening (track for later port)
- Confirmation: COMMENT-01..03, COMMENT-05, COMPILE-01 (partial), QUALITY-01 satisfied
- Note for Plan 06: MIGRATION.md ## Will Not Migrate must list `commons-macros` with rationale
</output>
