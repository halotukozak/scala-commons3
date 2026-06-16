---
phase: 03-scala-3-syntax-modernization
plan: 04
type: execute
# NOTE on wave/depends_on: PRs in Phase 3 are NOT stacked - each branches off
# upstream/scala-3 tip. Frontmatter expresses no git-topology dependencies.
# Execution order (3.1 -> 3.2 -> 3.3 -> 3.4) is enforced via PR body metadata.
# The executor reads PR body, opens PRs in declared order, and lifts draft state
# in 3.1->3.2->3.3->3.4 order. Slice 3.5 is independent (parallel-safe).
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/Opt.scala
  - core/src/main/scala/com/avsystem/commons/misc/NOpt.scala
  - core/src/main/scala/com/avsystem/commons/misc/OptArg.scala
  - core/src/main/scala/com/avsystem/commons/misc/OptRef.scala
  - core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJStream.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJDoubleStream.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJLongStream.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJIntStream.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/JStreamUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/GuavaInterop.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/Java8CollectionUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/JBasicUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/JFunctionUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/concurrent/TaskExtensions.scala
  - MIGRATION.md
autonomous: true
requirements: [SYNTAX-34-AT-INLINE-TO-INLINE, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

must_haves:
  truths:
    - "@inline def in scope rewritten to inline def per fork commits 5fafdbd7, 33a5b792, ad505679, ee0be95e, a4ddad6b, 580625a9"
    - "Whitelist preserved verbatim: CborInput.scala, JsonStringInput.scala, RPCFramework.scala — fork leaves these as @inline"
    - "SharedExtensions and jiop files match fork's inline-def parity count (~49 in SharedExtensions target; 0 hits left in tree but additive sweep where fork added `inline` keyword)"
    - "sbt compile + Test/compile + scalafmtCheckAll all green"
    - "MIGRATION.md §3 documents `inline def` is implicitly `final` (override-breaking) — value-class targets mitigate impact"
    - "PR open off upstream/scala-3 tip, draft, [Scala 3] prefix, milestone 1"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/Opt.scala"
      provides: "Opt-family `@inline def` → `inline def` (34 hits)"
    - path: "core/src/main/scala/com/avsystem/commons/misc/NOpt.scala"
      provides: "NOpt `@inline def` → `inline def` (32 hits)"
    - path: "core/src/main/scala/com/avsystem/commons/misc/OptArg.scala"
      provides: "OptArg `@inline def` → `inline def` (22 hits); argToOptArg preserved per slice 3.3"
    - path: "core/src/main/scala/com/avsystem/commons/misc/OptRef.scala"
      provides: "OptRef `@inline def` → `inline def` (29 hits)"
    - path: "core/src/main/scala/com/avsystem/commons/SharedExtensions.scala"
      provides: "~49 `inline def` parity with fork target (additive — extensions methods get `inline` keyword per fork shape)"
    - path: "MIGRATION.md"
      provides: "§3 entry: inline def is implicitly final; value-class targets mitigate"
  key_links:
    - from: "Opt-family inline forwarders"
      to: "call sites"
      via: "compile-time AST splice (Scala 3 inline)"
      pattern: "inline def \\w+\\[.*\\]?\\([^)]*\\) ="
    - from: "extension blocks in SharedExtensions"
      to: "inline parameter usage"
      via: "(inline f: A => B)"
      pattern: "\\(inline \\w+: "
---

<objective>
Slice 3.4 of Phase 3: rewrite Scala 2's `@inline def` (JVM optimizer hint) to Scala 3's true `inline def` (mandatory compile-time AST splice) across Opt family + SharedExtensions + jiop streams. Preserve fork's whitelist (CborInput, JsonStringInput, RPCFramework keep `@inline`).

Purpose: Replace optimizer-hint with real Scala 3 inlining, enabling `inline` parameters (no `Function1` allocation at call sites). Major perf win per fork's `perf(scala-3): inline …` commit cadence.

Output:
- 4 Opt-family files (117 `@inline` → `inline` rewrites)
- SharedExtensions + 9 jiop/concurrent files (additive — `inline` keyword added to defs/params matching fork shape, even though our tree has 0 `@inline` annotations today on these)
- 3 whitelist files unchanged (CborInput, JsonStringInput, RPCFramework)
- MIGRATION.md §3 entry covering override-breaking implication
- Multiple Conventional Commits per fork cadence (mirror fork's 6-commit split)
- Draft PR `[Scala 3] @inline def → inline def` off `upstream/scala-3`, milestone 1
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/MIGRATION.md
@.planning/phases/03-scala-3-syntax-modernization/03-CONTEXT.md
@.planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md
@~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md

<interfaces>
Fork source-of-truth commits (each is a single-purpose commit in fork's perf sweep):

| Fork commit | What it inlines |
|-------------|-----------------|
| `33a5b792` | `perf(scala-3): inline` — Streams (`ScalaJStream`, `ScalaJDoubleStream`, `ScalaJLongStream`), `JStreamUtils`, `TaskExtensions` |
| `5fafdbd7` | `perf(scala-3): inline Opt family` — `Opt`, `NOpt`, `OptArg`, `OptRef`, `GuavaInterop`, `Java8CollectionUtils` |
| `ad505679` | `perf(scala-3): more inlines` — Streams round 2, `JBasicUtils` |
| `ee0be95e` | `perf(scala-3): inline IntStream` — `ScalaJIntStream` |
| `a4ddad6b` | `perf(scala-3): inline SharedExtensions` — `SharedExtensions.scala` (49 inline defs + 43 inline params) |
| `580625a9` | `perf(scala-3): inline JFunctionUtils` — `JFunctionUtils.scala` |

Whitelist (PRESERVE `@inline` verbatim — fork does NOT convert these):
- `core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala:56` — `@inline private def bits(off: Int): Long`. Tight-loop parser hot path; JVM optimizer hint preferred.
- `core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala:346,353,356,359,475` — 5 `@inline` parser hot-path methods. Same rationale.
- `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:44,52` — RPC dropped from scala-3 per fork (RPC sources are scala-2.13-only on fork); leave as-is, don't touch.

Verify whitelist via fork:
```bash
git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/cbor/CborInput.scala | grep -n '@inline'
git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/json/JsonStringInput.scala | grep -n '@inline'
# Both must return identical line counts to our tree.
```

Semantic differences (per RESEARCH `### Scala 2 @inline vs Scala 3 inline — semantics`):
- Scala 2 `@inline` = JVM optimizer hint (compiler MAY honor).
- Scala 3 `inline` = mandatory AST splice (compiler MUST inline). Implicitly `final` → override-breaking.
- `inline def` can carry `(inline x: T)` parameters — Opt-family extension methods leverage this for zero-allocation lambdas.
- Avoid `inline def` for: recursive defs, non-trivial allocator bodies, `def unapply`, `try`/`catch` with effectful handlers.

Edge cases (per RESEARCH `### Edge cases where blind conversion would break`):
1. Recursive defs cannot be `inline`. Opt-family has no recursive defs (all forwarders) — safe.
2. Allocator methods (`def apply(value: A | Null): Opt[A] = if (...) new Opt[A](...)`) — fork keeps as plain `def`. Only forwarders get `inline`.
3. Trivial accessors (`def isEmpty`, `def isDefined`, `def get`) — fork keeps as plain `def`.
4. `def unapply` — fork keeps as plain `def`.
5. Extension methods inside `extension [A](a: A) { … }` — fork DOES use `inline def`. Parameter lists can carry `inline` (e.g. `(inline f: A => B)`).
6. `@nowarn` on `inline def discard: Unit = ()` — fork annotates with `@nowarn` (inline causes per-call-site unused warning otherwise). Carry verbatim.
</interfaces>

**PR conventions:** branch off `upstream/scala-3`, draft, `[Scala 3] @inline def → inline def` title, milestone 1, body metadata block (Slice 3.4, depends on #<3.3 PR>, base upstream/scala-3 not stacked).

**Commit cadence (mirror fork's 6-commit split):**
1. `perf(scala-3,core): inline Opt family @inline def → inline def` (Opt, NOpt, OptArg, OptRef — 117 hits)
2. `perf(scala-3,core): inline SharedExtensions` (additive — match fork's 49 inline-def + 43 inline-param shape)
3. `perf(scala-3,core): inline jiop streams (ScalaJStream, ScalaJDoubleStream, ScalaJLongStream, ScalaJIntStream)`
4. `perf(scala-3,core): inline jiop utils (JStreamUtils, JBasicUtils, GuavaInterop, Java8CollectionUtils, JFunctionUtils)`
5. `perf(scala-3,core): inline concurrent (TaskExtensions)`
6. `docs(migration): record @inline def → inline def (final-implied; value-class scope mitigates)`

No squash.
</context>

<tasks>

<task type="auto">
  <name>Task 1: Branch + Opt family sweep (Opt, NOpt, OptArg, OptRef — 117 hits)</name>
  <files>
    core/src/main/scala/com/avsystem/commons/misc/Opt.scala
    core/src/main/scala/com/avsystem/commons/misc/NOpt.scala
    core/src/main/scala/com/avsystem/commons/misc/OptArg.scala
    core/src/main/scala/com/avsystem/commons/misc/OptRef.scala
  </files>
  <read_first>
    - `git show 5fafdbd7 --stat` — fork file list + diff size for Opt family.
    - For each of Opt.scala, NOpt.scala, OptArg.scala, OptRef.scala:
      - Current state in our tree.
      - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<X>.scala` — fork target shape.
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `## Slice 3.4` section + `### Edge cases where blind conversion would break`.
    - From slice 3.3: OptArg.argToOptArg is PRESERVED as `implicit def` — slice 3.4 must not touch this preservation.
  </read_first>
  <action>
    1. Cut branch: `git fetch upstream && git checkout -b 03-04-at-inline-to-inline upstream/scala-3`.
    2. Baseline grep counts: `git grep -c '@inline' -- 'core/src/main/scala/com/avsystem/commons/misc/Opt.scala' 'core/src/main/scala/com/avsystem/commons/misc/NOpt.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptArg.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptRef.scala' > /tmp/03-04-opt-baseline.txt` (expect 34+32+22+29 = 117).
    3. For each Opt-family file:
       - Read current; read `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<X>.scala`.
       - **Strategy: per-file copy + reconcile** (per RESEARCH primary recommendation):
         - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/<X>.scala > core/src/main/scala/com/avsystem/commons/misc/<X>.scala`
         - **Reconcile:**
           - Prune imports referencing dropped modules (`commons-macros`, RPC).
           - Drop `@TodoScala3Migration` annotations.
           - Re-apply slice 3.3 preservation for OptArg: `argToOptArg` MUST stay as `implicit def` with the verbatim erasure-bridge comment from fork — verify it's still present after the copy.
           - Per RESEARCH Edge cases: verify allocators (`apply(value: A | Null)`), accessors (`isEmpty`, `isDefined`, `get`), `unapply` are plain `def` (not `inline def`) per fork.
         - `sbt commons-jvm/compile` exit 0. If errors: most likely a recursive-def or non-trivial-body inline — revert that specific `inline` keyword and re-compile.
       - Run `sbt scalafmtAll`.
    4. Re-grep to confirm `@inline` is gone from these 4 files: `git grep -c '@inline' -- 'core/src/main/scala/com/avsystem/commons/misc/Opt.scala' 'core/src/main/scala/com/avsystem/commons/misc/NOpt.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptArg.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptRef.scala'` → 0.
    5. Verify slice-3.3 borderline preserved: `grep -nE '^\s*implicit\s+def\s+argToOptArg' core/src/main/scala/com/avsystem/commons/misc/OptArg.scala` → 1 hit, preceded by verbatim fork comment.
    6. `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    7. Commit: `perf(scala-3,core): inline Opt family @inline def → inline def` body: `Translated from origin/master@5fafdbd7. 4 files, 117 @inline → inline conversions. OptArg.argToOptArg preserved as implicit def per slice 3.3 erasure-bridge rationale.`
  </action>
  <verify>
    <automated>! git grep -nq '@inline' -- 'core/src/main/scala/com/avsystem/commons/misc/Opt.scala' 'core/src/main/scala/com/avsystem/commons/misc/NOpt.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptArg.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptRef.scala'</automated>
  </verify>
  <acceptance_criteria>
    - `git grep -c '@inline' -- 'core/src/main/scala/com/avsystem/commons/misc/Opt.scala' 'core/src/main/scala/com/avsystem/commons/misc/NOpt.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptArg.scala' 'core/src/main/scala/com/avsystem/commons/misc/OptRef.scala'` → 0 (all 4 files).
    - `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - OptArg.argToOptArg preserved as `implicit def` with verbatim fork comment.
    - Single commit named `perf(scala-3,core): inline Opt family @inline def → inline def`.
    - Per-file inline-def count parity with fork: `diff <(git show HEAD:core/src/main/scala/com/avsystem/commons/misc/Opt.scala | grep -cE '\binline (def|val)\b') <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala | grep -cE '\binline (def|val)\b')` exit 0 (counts match).
  </acceptance_criteria>
  <done>
    Opt family inlined; 117 @inline annotations gone; OptArg borderline preserved; compile + tests + scalafmt green; 1 commit.
  </done>
</task>

<task type="auto">
  <name>Task 2: SharedExtensions + jiop streams/utils + concurrent (additive inline-parity sweep — 5 commits)</name>
  <files>
    core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJStream.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJDoubleStream.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJLongStream.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/ScalaJIntStream.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/JStreamUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/GuavaInterop.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/Java8CollectionUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/JBasicUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/JFunctionUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/concurrent/TaskExtensions.scala
  </files>
  <read_first>
    - For each in-scope file: `git show origin/master:<path>` (jvm-only files are direct-match; `SharedExtensions` is in `scala-3/` overlay).
    - Fork commits: `git show a4ddad6b` (SharedExtensions), `git show 33a5b792` (streams + TaskExtensions), `git show ad505679` (streams round 2 + JBasicUtils), `git show ee0be95e` (ScalaJIntStream), `git show 580625a9` (JFunctionUtils), `git show 5fafdbd7` (GuavaInterop + Java8CollectionUtils overlap).
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### Files NOT covered by inline commits but ALSO touched by SharedExtensions/jiop commits` — note these files have 0 `@inline` hits today but require ADDITIVE `inline` keyword sweep to match fork's `inline def` / `(inline param: T)` shape.
    - Slices 3.1/3.2/3.3 may have already touched SharedExtensions; verify the current state before copy.
  </read_first>
  <action>
    Each of the five sub-tasks below is its own commit, applied in order:

    **2a. SharedExtensions** (`perf(scala-3,core): inline SharedExtensions`):
    1. `git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala > /tmp/fork-SharedExtensions.scala`
    2. Diff against current: `diff core/src/main/scala/com/avsystem/commons/SharedExtensions.scala /tmp/fork-SharedExtensions.scala | less`
    3. **Strategy: copy + reconcile**:
       - Copy fork's version on top: `cp /tmp/fork-SharedExtensions.scala core/src/main/scala/com/avsystem/commons/SharedExtensions.scala`
       - Reconcile:
         - Re-apply any Phase-1/2 stubs (`???`) for methods that referenced macros-derivation now gone.
         - Prune imports referencing dropped modules.
         - Drop `@TodoScala3Migration` annotations.
         - Re-apply slice 3.1 extension shape if fork's shape is already extension-based (likely identical).
         - Preserve slice 3.3 implicit→given conversions if any happened here.
         - Carry fork's `@nowarn` on `inline def discard: Unit = ()` verbatim (per RESEARCH Edge case 6).
       - `sbt commons-jvm/compile ;commons-js/compile` exit 0.
       - `sbt scalafmtAll`.
    4. Parity check: `diff <(grep -cE '\binline (def|val)\b' core/src/main/scala/com/avsystem/commons/SharedExtensions.scala) <(grep -cE '\binline (def|val)\b' /tmp/fork-SharedExtensions.scala)` exit 0 — counts must match (~49).
    5. Commit: `perf(scala-3,core): inline SharedExtensions` body: `Translated from origin/master@a4ddad6b. 49 inline def + 43 inline params. @nowarn on inline def discard carried verbatim.`

    **2b. jiop streams (`perf(scala-3,core): inline jiop streams`)**:
    Files: `ScalaJStream.scala`, `ScalaJDoubleStream.scala`, `ScalaJLongStream.scala`, `ScalaJIntStream.scala`.
    For each:
    1. `git show origin/master:core/jvm/src/main/scala/com/avsystem/commons/jiop/<file>` > target.
    2. Reconcile (same checklist as 2a).
    3. `sbt commons-jvm/compile` exit 0 per file (or per batch of 4).
    4. Parity check per file.
    Commit (single): `perf(scala-3,core): inline jiop streams (ScalaJStream/Double/Long/IntStream)` body: `Translated from origin/master@33a5b792 + ad505679 + ee0be95e.`

    **2c. jiop utils (`perf(scala-3,core): inline jiop utils`)**:
    Files: `JStreamUtils.scala`, `JBasicUtils.scala`, `GuavaInterop.scala`, `Java8CollectionUtils.scala`, `JFunctionUtils.scala`.
    Per-file copy + reconcile + parity.
    Commit: `perf(scala-3,core): inline jiop utils (JStreamUtils, JBasicUtils, GuavaInterop, Java8CollectionUtils, JFunctionUtils)` body: `Translated from origin/master@33a5b792 + ad505679 + 5fafdbd7 + 580625a9.`

    **2d. concurrent (`perf(scala-3,core): inline TaskExtensions`)**:
    File: `TaskExtensions.scala`.
    Per-file copy + reconcile + parity.
    Commit: `perf(scala-3,core): inline TaskExtensions` body: `Translated from origin/master@33a5b792.`

    After all commits: `sbt 'compile ;Test/compile ;scalafmtCheckAll'` exit 0.
  </action>
  <verify>
    <automated>sbt 'compile ;Test/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - SharedExtensions has `inline def` count matching fork (parity diff exit 0; ~49).
    - SharedExtensions retains `@nowarn` on `inline def discard: Unit = ()`.
    - Each jiop file has `inline def` count matching fork target (per-file parity diff exit 0).
    - `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - **Significant-indentation guard:** confirm `.scalafmt.conf` enforces braces (no significant-indentation in formatter output). Verification: `grep -E 'rewrite\.scala3|indent|significantIndentation' .scalafmt.conf || echo "no significant-indentation rewrite — default braces"`. If the fork-copy step (2a) brought in significant-indentation syntax, `sbt scalafmtAll` MUST reformat back to braces; if not, document the scalafmt dialect in PR body.
    - 4 new commits: 2a SharedExtensions, 2b jiop streams, 2c jiop utils, 2d concurrent. Each single-purpose per fork cadence.
    - Phase-2 stubs (`???`) preserved where applicable.
  </acceptance_criteria>
  <done>
    Additive `inline` sweep complete on SharedExtensions + jiop + concurrent matching fork shape; 4 commits; full gate green.
  </done>
</task>

<task type="auto">
  <name>Task 3: Whitelist verification + MIGRATION.md §3 update + final acceptance grep + draft PR</name>
  <files>
    MIGRATION.md
  </files>
  <read_first>
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `## Slice 3.4 — @inline def → inline def` AND `### Acceptance gate` AND `MIGRATION.md §3 entry for slice 3.4`.
    - MIGRATION.md current §3 structure.
    - Whitelist files (must remain unchanged):
      - `core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala`
      - `core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala`
      - `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala`
  </read_first>
  <action>
    1. **Whitelist verification** — confirm no inadvertent changes touched the whitelist:
       ```bash
       git diff upstream/scala-3..HEAD -- \
         'core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala' \
         'core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala' \
         'core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala'
       # Expected: EMPTY DIFF.
       ```
       If diff non-empty, `git checkout upstream/scala-3 -- <whitelist-file>` to revert.

    2. **Whitelist preservation count check**:
       ```bash
       git grep -c '@inline' core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala  # expect 1
       git grep -c '@inline' core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala  # expect 5
       git grep -c '@inline' core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala  # expect 2
       ```

    3. **Primary acceptance grep gate** (per CONTEXT.md):
       ```bash
       git grep -nE '@inline' -- 'core/src/main/scala' \
         | grep -vE '(CborInput|JsonStringInput|RPCFramework)\.scala:' \
         | wc -l
       # Expected: 0
       ```

    4. **Secondary parity diff** (SharedExtensions inline-def count vs fork):
       ```bash
       diff \
         <(git show HEAD:core/src/main/scala/com/avsystem/commons/SharedExtensions.scala | grep -cE '\binline (def|val)\b') \
         <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala | grep -cE '\binline (def|val)\b')
       # Expected: exit 0 (counts identical)
       ```

    5. **MIGRATION.md §3 update**:
       Add new sub-entries under `### core`:
       - "`@inline def` → `inline def` across Opt family (Opt, NOpt, OptArg, OptRef — 117 sites), SharedExtensions (49 sites), jiop streams + utils (ScalaJ*Stream, JStreamUtils, JBasicUtils, GuavaInterop, Java8CollectionUtils, JFunctionUtils), concurrent (TaskExtensions). Scala 3 `inline def` is implicitly `final` and cannot be overridden — source-compat break for downstream subclassers. Mitigation: Opt family targets are value classes (final by construction); SharedExtensions / jiop targets are utility traits / objects, not extension points."
       - "Preserved as `@inline` per JVM-optimizer-hint convention (tight-loop parser hot paths): `CborInput.bits` (1 site), `JsonStringInput` (5 sites — `read`, `isNext`, `isNextDigit`, `advance`, nested `update`), `RPCFramework` (2 sites — RPC module remains scala-2.13-only per fork strategy)."
       - "Binary-compat: Scala 3 `inline def` emits no bytecode method. MiMa would flag if/when scala-3 baseline is released; currently MiMa off (`mimaPreviousArtifacts := Set.empty`)."

       Commit: `docs(migration): record @inline def → inline def (final-implied; value-class scope mitigates)`.

    6. **Final full gate**:
       ```bash
       sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0
       ! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)' | grep -v 'inline def discard'
       # Note: SharedExtensions's `@nowarn inline def discard` is a PRE-EXISTING / fork-pattern preservation, not a NEW suppression.
       ! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'
       ! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'
       ```

    7. **Push + open draft PR**:
       ```bash
       git push -u origin 03-04-at-inline-to-inline
       gh pr create --repo AVSystem/scala-commons --base scala-3 --head halotukozak:03-04-at-inline-to-inline --draft \
         --title "[Scala 3] @inline def → inline def" \
         --body "$(cat <<'EOF'
       Rewrites Scala 2 `@inline def` (JVM optimizer hint) to Scala 3 `inline def` (mandatory compile-time AST splice) across Opt family + SharedExtensions + jiop + concurrent. Major perf win: `(inline f: A => B)` parameter usage eliminates Function1 allocations at call sites.

       ## Scope
       - **Rewritten:** 15 files
         - Opt family (Opt, NOpt, OptArg, OptRef) — 117 @inline sites
         - SharedExtensions — additive parity (49 inline defs + 43 inline params per fork shape)
         - jiop streams (ScalaJStream, ScalaJDoubleStream, ScalaJLongStream, ScalaJIntStream)
         - jiop utils (JStreamUtils, JBasicUtils, GuavaInterop, Java8CollectionUtils, JFunctionUtils)
         - concurrent (TaskExtensions)
       - **Whitelist (preserved verbatim):**
         - CborInput.scala:56 (1 @inline — tight-loop parser hot path)
         - JsonStringInput.scala (5 @inline — same)
         - RPCFramework.scala (2 @inline — RPC scala-2.13-only per fork)
       - OptArg.argToOptArg PRESERVED as `implicit def` per slice 3.3 erasure-bridge rationale.

       ## Acceptance
       - `git grep '@inline' core/src/main/scala/` outside whitelist → 0 hits
       - Per-file `inline def` count parity with fork master
       - `sbt compile + Test/compile + scalafmtCheckAll` green
       - No new `@nowarn` / `-Wconf` (the `@nowarn` on `inline def discard` is verbatim fork preservation, not new suppression)

       ## Source-compat impact
       Scala 3 `inline def` is implicitly `final` → cannot be overridden. Downstream subclassers of Opt family / SharedExtensions / jiop utility traits will break. Mitigation: Opt family targets are value classes (final by construction); other targets are not extension points.

       Translated from fork `origin/master` commits 5fafdbd7, 33a5b792, ad505679, ee0be95e, a4ddad6b, 580625a9.

       **Slice:** 3.4 of Phase 3 (Scala 3 syntax modernization)
       **Merge order:** 3.1 → 3.2 → 3.3 → 3.4
       **Depends on:** #<3.3 PR number>
       **Base branch:** upstream/scala-3 (not stacked)
       EOF
       )"
       gh api PATCH /repos/AVSystem/scala-commons/issues/<PR_NUM> -f milestone=1
       ```
  </action>
  <verify>
    <automated>gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone --jq '.isDraft and (.title | startswith("[Scala 3]")) and (.milestone.number == 1)'</automated>
  </verify>
  <acceptance_criteria>
    - Whitelist files unchanged: `git diff upstream/scala-3..HEAD -- <whitelist>` is empty.
    - Whitelist @inline counts intact: CborInput=1, JsonStringInput=5, RPCFramework=2.
    - Primary grep gate `git grep '@inline' core/src/main/scala/ | grep -v whitelist | wc -l` → 0.
    - SharedExtensions parity diff vs fork exit 0.
    - `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - PR open at AVSystem: draft, `[Scala 3] @inline def → inline def` title, milestone 1, base `scala-3`, body metadata block.
    - 7-8 commits on branch (1 Opt + 4 sweeps + 1 docs). No squash.
    - MIGRATION.md §3 has slice 3.4 entries (rewrite list + whitelist + final-implied warning).
    - **`@nowarn` carve-out (verbatim fork preservation, NOT a new suppression):** the ONLY allowed `@nowarn` is the verbatim-fork annotation on `inline def discard: Unit = ()` in `SharedExtensions.scala`. Carve-out verification command:
      ```bash
      ! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)' | grep -v 'inline def discard'
      # Exit 0 (no other new @nowarn / -Wconf lines added).
      ```
      Rationale: Scala 3 `inline def` emits a per-call-site unused-value warning on `discard`; fork suppresses with `@nowarn` at the def site. This is preservation of fork's annotation, not new suppression authored by us — verified by `git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala | grep -B1 'inline def discard'` showing the same annotation.
    - No `.planning/` in commits. No GSD nomenclature.
  </acceptance_criteria>
  <done>
    Slice 3.4 PR open at AVSystem with all 4 slices of Phase 3 syntax modernization now in flight. Merge order 3.1 → 3.2 → 3.3 → 3.4 enforced via PR body metadata.
  </done>
</task>

</tasks>

<verification>
```bash
# Primary acceptance: only whitelist remains
git grep -nE '@inline' -- 'core/src/main/scala' \
  | grep -vE '(CborInput|JsonStringInput|RPCFramework)\.scala:' \
  | wc -l
# Expect: 0

# Whitelist intact
git grep -c '@inline' core/src/main/scala/com/avsystem/commons/serialization/cbor/CborInput.scala  # 1
git grep -c '@inline' core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringInput.scala  # 5
git grep -c '@inline' core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala  # 2

# Parity diff
diff \
  <(git show HEAD:core/src/main/scala/com/avsystem/commons/SharedExtensions.scala | grep -cE '\binline (def|val)\b') \
  <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala | grep -cE '\binline (def|val)\b')

# Compile + tests
sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0

# Cleanliness
! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)' | grep -v 'inline def discard'
! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'
! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'

# PR conventions
gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone
```
</verification>

<success_criteria>
- Opt family (4 files / 117 hits) `@inline` → `inline` complete.
- SharedExtensions inline-def parity with fork (~49) achieved.
- jiop streams + utils + concurrent additive `inline` sweep matches fork shape.
- Whitelist (CborInput / JsonStringInput / RPCFramework) preserved verbatim — `git diff` empty for those files.
- `sbt compile ;Test/compile ;scalafmtCheckAll` exits 0.
- 7-8 commits per fork cadence; no squash.
- Draft PR at AVSystem with `[Scala 3]` title, milestone 1, base `scala-3`, body metadata.
- MIGRATION.md §3 has slice 3.4 entries documenting final-implied override-breaking + whitelist rationale.
- No new `@nowarn`/`-Wconf` (fork's discard `@nowarn` is preservation, not new). No `.planning/`. No GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/03-scala-3-syntax-modernization/03-04-SUMMARY.md` documenting per-file inline-def counts (ours vs fork parity), whitelist verification, PR URL.
</output>
