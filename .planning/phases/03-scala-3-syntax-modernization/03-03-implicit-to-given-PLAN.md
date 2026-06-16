---
phase: 03-scala-3-syntax-modernization
plan: 03
type: execute
# NOTE on wave/depends_on: PRs in Phase 3 are NOT stacked - each branches off
# upstream/scala-3 tip. Frontmatter expresses no git-topology dependencies.
# Execution order (3.1 -> 3.2 -> 3.3 -> 3.4) is enforced via PR body metadata.
# The executor reads PR body, opens PRs in declared order, and lifts draft state
# in 3.1->3.2->3.3->3.4 order. Slice 3.5 is independent (parallel-safe).
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
  - core/src/main/scala/com/avsystem/commons/misc/OptArg.scala
  - core/src/main/scala/com/avsystem/commons/serialization/SerializationMacros.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/GuavaInterop.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/JOptionalUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/JStreamUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/Java8CollectionUtils.scala
  - core/jvm/src/main/scala/com/avsystem/commons/jiop/JavaTimeInterop.scala
  - core/jvm/src/main/scala/com/avsystem/commons/concurrent/BlockingUtils.scala
  - core/js/src/main/scala/com/avsystem/commons/jsiop/JsInterop.scala
  - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala
  - mongo/jvm/src/main/scala/**/*.scala (60+ files per fork eef0edce + 848b8e9e — sed sweep `(implicit X: T)` → `(using X: T)`)
  - benchmark/jvm/src/main/scala/com/avsystem/commons/ser/GenCodecBenchmarks.scala
  - benchmark/jvm/src/main/scala/com/avsystem/commons/ser/StreamInputOutputBenchmark.scala
  - hocon/src/main/scala/com/avsystem/commons/hocon/ConfigCompanion.scala
  - hocon/src/main/scala/com/avsystem/commons/hocon/HTree.scala
  - MIGRATION.md
autonomous: true
requirements: [SYNTAX-33-IMPLICIT-TO-GIVEN, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

must_haves:
  truths:
    - "git grep -E '^\\s*(inline\\s+)?implicit\\s+(def|val)' core/src/main/scala/ mongo/ hocon/ → only documented exceptions (OptArg.argToOptArg, SerializationMacros.fun2GenRef) + any fork-preserved cases"
    - "Every preserved `implicit def/val` carries the verbatim explanatory comment from fork"
    - "sbt compile + Test/compile + scalafmtCheckAll all green"
    - "BsonGenCodecs uses anonymous `given` + `export X.given` + `@deprecated def name: T = summon` shims per fork 8f70be80"
    - "(implicit X: T) parameter lists in mongo rewritten to (using X: T) per fork eef0edce"
    - "PR open off upstream/scala-3 tip, draft, [Scala 3] prefix, milestone 1"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/OptArg.scala"
      provides: "argToOptArg PRESERVED as `implicit def` with fork's erasure-bridge explanatory comment verbatim"
    - path: "core/src/main/scala/com/avsystem/commons/serialization/SerializationMacros.scala"
      provides: "fun2GenRef PRESERVED as `inline implicit def` with fork's macro-splice explanatory comment verbatim"
    - path: "mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala"
      provides: "trait `export BsonGenCodecs.given`, object with anonymous `given GenCodec[X]` + `@deprecated def` shims (fork pattern from 8f70be80)"
    - path: "MIGRATION.md"
      provides: "§3 entry for implicit → given source-compat (named import callers, (implicit X) → (using X), @deprecated shims)"
  key_links:
    - from: "BsonGenCodecs trait"
      to: "BsonGenCodecs object given instances"
      via: "export BsonGenCodecs.given"
      pattern: "export .*\\.given"
    - from: "callers of named implicits"
      to: "@deprecated def name: T = summon"
      via: "summon[T] forwarder"
      pattern: "@deprecated.*\\n\\s*def \\w+:.* = summon"
---

<objective>
**Scope note (revision 2026-06-01):** The `Implicits` object deletion has been moved OUT of this slice into standalone slice 3.5 (`03-05-delete-implicits-object-PLAN.md`). Slice 3.3 now covers ONLY the `implicit def/val/object` → `given` rewrite and `(implicit X)` → `(using X)` sweep across core + mongo + hocon + benchmark.

Slice 3.3 of Phase 3: rewrite `implicit def` / `implicit val` / `implicit object` to Scala 3 `given` declarations, and `(implicit X: T)` parameter lists to `(using X: T)`. Preserve borderline cases (`OptArg.argToOptArg`, `SerializationMacros.fun2GenRef`) verbatim with fork's explanatory comments.

Purpose: Sweep Scala 2 `implicit` keyword out of definition sites. Source-compat impact for downstream callers using named-import lookup mitigated via `@deprecated def name: T = summon` shims per fork pattern.

Output:
- ~70-90 files rewritten across core (jiop, jsiop, concurrent, serialization, misc, SharedExtensions), mongo (BsonGenCodecs anonymous given + `(implicit X)` → `(using X)` sweep), benchmark, hocon
- Two borderline `implicit def`s preserved verbatim with fork comments
- MIGRATION.md §3 entry
- Multiple Conventional Commits per fork cadence (one per module/feature area)
- Draft PR `[Scala 3] implicit def/val → given` off `upstream/scala-3`, milestone 1
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
Fork source-of-truth commits:
- `39c047eb` — `refactor(scala-3): eliminate implicit keyword in core; remove RPC module from scala-3` (core)
- `ebffde26` — `refactor(scala-3): finish implicit→given sweep in core serialization/cbor`
- `eef0edce` — `refactor(scala-3,mongo): implicit val/def/class → given/using/extension, Conversion givens` (mongo + `(implicit X)` → `(using X)` sweep)
- `8f70be80` — `refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + deprecated named def`
- `848b8e9e` — `fix(scala-3,mongo): clear all -Werror warnings, eliminate remaining implicits`

Borderline kept-implicit cases (NON-NEGOTIABLE PRESERVATIONS — per RESEARCH `### Borderline implicits — KEEP as implicit (verbatim from fork)`):

1. **`OptArg.argToOptArg`** (`core/src/main/scala/.../misc/OptArg.scala`):
   ```scala
   /** Kept as `implicit def` (not `given Conversion[A, OptArg[A]]`): a polymorphic Conversion would
     * generate a JVM erasure-bridge collision — `A` and the `OptArg` value class both erase to `Object`. */
   implicit def argToOptArg[A](value: A): OptArg[A] = OptArg(value)
   ```
   Copy fork's comment + signature verbatim from `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala`.

2. **`SerializationMacros.fun2GenRef`** (or whichever object houses it post-Phase-2):
   ```scala
   // Kept as `inline implicit def` (not a `given Conversion`): the body is a macro splice over the
   // `inline fun` argument, which a `Conversion[S => T, GenRef[S, T]]`'s non-inline `apply` cannot carry.
   inline implicit def fun2GenRef[S, T](inline fun: S => T): GenRef[S, T] =
     ${ SerializationMacros.genRefImpl[S, T]('fun) }
   ```
   Note: SerializationMacros is Phase-2-stubbed (`???`); the preservation may apply at the `GenRef` companion if that's where fork keeps it. Cross-check `git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/`.

Fork patterns to inherit:
- **Anonymous given:** `given GenCodec[ObjectId] = GenCodec.nullable(…)` (no name) — preferred for canonical instances.
- **Named given when consumed by name:** `given foo: T = …` — only if fork chose named.
- **export pattern:** `trait BsonGenCodecs { export BsonGenCodecs.given }` + object holds anonymous givens.
- **@deprecated shim:** `@deprecated("Use summon[GenCodec[ObjectId]]", since = "scala-3") def objectIdCodec: GenCodec[ObjectId] = summon` — preserves source-compat for named-import callers.
- **Conversion given:** `given Conversion[A, B] = a => …` replaces `implicit def aToB(a: A): B = …`.
- **`(implicit X: T)` → `(using X: T)`:** Mechanical sed sweep across mongo (60+ files per fork `eef0edce`).
- **`implicit object X extends Y`** → `given X: Y with { … }` (or `given Y with { … }` anonymous).
</interfaces>

**PR conventions:** branch off `upstream/scala-3`, draft, `[Scala 3] implicit def/val → given` title, milestone 1, body metadata block (Slice 3.3, depends on #<3.2 PR>, base upstream/scala-3 not stacked).

**Commit cadence (per fork):**
- `refactor(scala-3,core): implicit def/val → given in jiop` (Java interop conversions — biggest cluster, ~40 hits)
- `refactor(scala-3,core): implicit def/val → given in jsiop + concurrent`
- `refactor(scala-3,core): implicit def/val → given in serialization + SharedExtensions`
- `fix(scala-3): preserve OptArg.argToOptArg implicit (erasure-bridge collision)` — verbatim comment from fork commit `39c047eb`
- `fix(scala-3): preserve SerializationMacros.fun2GenRef inline implicit (macro splice)` — verbatim comment from fork commit `ebffde26`
- `refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + @deprecated shims` — body `Translated from origin/master@8f70be80.`
- `refactor(scala-3,mongo): (implicit X: T) → (using X: T) parameter list sweep` — body `Translated from origin/master@eef0edce.`
- `refactor(scala-3,mongo): remaining implicit → given (clear -Werror)` — body `Translated from origin/master@848b8e9e.`
- `refactor(scala-3,benchmark): implicit val/def → given` (small)
- `refactor(scala-3,hocon): implicit val/def → given` (if any)
- `docs(migration): record implicit → given source-compat impact`

No squash.
</context>

<tasks>

<task type="auto">
  <name>Task 1: Branch + core sweep (jiop, jsiop, concurrent, serialization, SharedExtensions, misc) — 4-5 commits</name>
  <files>
    core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
    core/src/main/scala/com/avsystem/commons/serialization/SerializationMacros.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/GuavaInterop.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/JOptionalUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/JStreamUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/Java8CollectionUtils.scala
    core/jvm/src/main/scala/com/avsystem/commons/jiop/JavaTimeInterop.scala
    core/jvm/src/main/scala/com/avsystem/commons/concurrent/BlockingUtils.scala
    core/js/src/main/scala/com/avsystem/commons/jsiop/JsInterop.scala
  </files>
  <read_first>
    - `git show 39c047eb` and `git show ebffde26` — fork commit diffs covering core.
    - For each in-scope file: `git show origin/master:core/src/main/scala-3/<path>` OR `git show origin/master:core/jvm/src/main/scala/<path>` (jvm-only files are direct match).
    - `git grep -nE '\bimplicit\s+(def|val|object)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala'` — live inventory baseline.
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### Slice 3.3` AND `### Borderline implicits` sections.
  </read_first>
  <action>
    1. Cut branch: `git fetch upstream && git checkout -b 03-03-implicit-to-given upstream/scala-3`.
    2. Record baseline grep counts: `git grep -cE '\bimplicit\s+(def|val|object)' -- 'core' 'mongo' 'hocon' 'benchmark' > /tmp/03-03-baseline.txt`.
    3. **Cluster A: jiop (40+ hits, biggest)** — sweep `GuavaInterop`, `JOptionalUtils`, `JStreamUtils`, `Java8CollectionUtils`, `JavaTimeInterop`:
       - For each file: `git show origin/master:core/jvm/src/main/scala/com/avsystem/commons/jiop/<file>` → compare → translate each `implicit def xxx2AsScala/2AsJava` to `given Conversion[A, B] = a => …` (Conversion pattern per fork).
       - Re-apply our Phase-1/2 stubs if any (per RESEARCH Pitfall 2; jiop has none so far per Phase-1 SUMMARY).
       - `sbt commons-jvm/compile` exit 0.
       - Commit: `refactor(scala-3,core): implicit def → given Conversion in jiop` body: `Translated from origin/master@39c047eb (jiop subset).`
    4. **Cluster B: jsiop + concurrent** — `JsInterop.scala` (3 hits) + `BlockingUtils.scala` (2 hits):
       - Same per-file copy/reconcile.
       - `sbt commons-jvm/compile ;commons-js/compile` exit 0.
       - Commit: `refactor(scala-3,core): implicit def → given in jsiop + concurrent` body: `Translated from origin/master@39c047eb.`
    5. **Cluster C: serialization (excluding SerializationMacros) + SharedExtensions** — `SharedExtensions.scala` (post-Phase-2 has ~6 `implicit def xxxOps` synthesized to keep value-class wrappers; these become extensions or givens per fork — note slice 3.1 already may have handled most via `extension`):
       - Read fork: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala`.
       - For each remaining `implicit def xxxOps` not already converted to `extension` in slice 3.1: convert to anonymous `given` if fork did, or fold into existing `extension` block.
       - `sbt commons-jvm/compile` exit 0.
       - Commit: `refactor(scala-3,core): implicit def/val → given in serialization + SharedExtensions` body: `Translated from origin/master@ebffde26 + 39c047eb.`
    6. **Cluster E: borderline preservations (separate commits)** — apply explicit "preserve" commits:
       - **OptArg.argToOptArg:** Open `core/src/main/scala/com/avsystem/commons/misc/OptArg.scala`. Locate `argToOptArg`. Verify it is still `implicit def argToOptArg[A](value: A): OptArg[A] = OptArg(value)`. Add fork's explanatory comment ABOVE it (copy verbatim from `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala`). Commit: `fix(scala-3): preserve OptArg.argToOptArg implicit (erasure-bridge collision)` body: `Polymorphic Conversion[A, OptArg[A]] would generate JVM erasure-bridge collision — A and OptArg value class both erase to Object. Verbatim comment carried from fork @39c047eb.`
       - **SerializationMacros.fun2GenRef:** Locate (may be in `SerializationMacros.scala` or `GenRef.scala`). Confirm Phase-2 stub status; if stubbed (`???`), this preservation is moot for runtime but we still preserve the signature shape per fork. Add fork's explanatory comment verbatim. Commit: `fix(scala-3): preserve SerializationMacros.fun2GenRef inline implicit (macro splice over inline arg)` body: `Macro splice over inline fun arg cannot be expressed as Conversion[S => T, GenRef[S, T]] whose apply is non-inline. Verbatim comment from fork @ebffde26.`
    7. After all commits: `sbt commons-jvm/compile ;commons-js/compile` exit 0. Re-grep:
       ```bash
       git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala'
       # Expected: 2 hits — OptArg.argToOptArg and SerializationMacros.fun2GenRef (or GenRef.fun2GenRef).
       ```
  </action>
  <verify>
    <automated>git grep -cE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala'</automated>
  </verify>
  <acceptance_criteria>
    - After all core commits, `git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala'` returns exactly 2 hits (OptArg.argToOptArg + SerializationMacros.fun2GenRef or GenRef.fun2GenRef), each with verbatim fork explanatory comment ABOVE the signature.
    - `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - 5 commits on branch (3 clusters + 2 borderline preserve commits).
    - No new `@nowarn`/`-Wconf` in diff.
  </acceptance_criteria>
  <done>
    Core implicit → given sweep complete; 2 borderline cases preserved with verbatim fork comments; compile + Test/compile + scalafmt green.
  </done>
</task>

<task type="auto">
  <name>Task 2a: Mongo — BsonGenCodecs anonymous-given pattern (fork 8f70be80)</name>
  <files>
    mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala
  </files>
  <read_first>
    - `git show 8f70be80 -- mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala` — anonymous-given + @deprecated-shim pattern.
    - Current state of BsonGenCodecs.scala in our tree.
  </read_first>
  <action>
    1. Read current state + `git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala`.
    2. Replace trait + object per fork shape:
       ```scala
       trait BsonGenCodecs {
         export BsonGenCodecs.given
       }
       object BsonGenCodecs {
         given TransparentWrapping[ObjectId, ObjectId] = TransparentWrapping.identity
         given GenCodec[ObjectId] = GenCodec.nullable(…)
         given GenKeyCodec[ObjectId] = GenKeyCodec.create(new ObjectId(_), _.toHexString)

         @deprecated("Use summon[GenCodec[ObjectId]]", since = "scala-3")
         def objectIdCodec: GenCodec[ObjectId] = summon
         @deprecated("Use summon[GenKeyCodec[ObjectId]]", since = "scala-3")
         def objectIdKeyCodec: GenKeyCodec[ObjectId] = summon
       }
       ```
    3. Re-apply Phase-2 stubs if any reference materialize/macros that are `???` — keep stub `???` bodies if the canonical given depends on macro derivation that's still stubbed.
    4. `sbt commons-jvm/compile` exit 0.
    5. Commit: `refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + @deprecated shims` body: `Translated from origin/master@8f70be80.`
  </action>
  <verify>
    <automated>sbt commons-jvm/compile</automated>
  </verify>
  <acceptance_criteria>
    - BsonGenCodecs.scala matches fork shape: trait has `export BsonGenCodecs.given`; object holds anonymous `given` declarations + `@deprecated def name: T = summon` shims.
    - `sbt commons-jvm/compile` exit 0.
    - Single commit (`refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + @deprecated shims`).
  </acceptance_criteria>
  <done>
    BsonGenCodecs uses anonymous-given + @deprecated shim pattern; compile green; isolated commit landed before 2b sweep so a 2b failure doesn't strand this work.
  </done>
</task>

<task type="auto">
  <name>Task 2b: Mongo — `(implicit X: T)` → `(using X: T)` parameter list sweep (fork eef0edce, 848b8e9e)</name>
  <files>
    mongo/jvm/src/main/scala/**/*.scala (~60 files per fork eef0edce)
  </files>
  <read_first>
    - `git show eef0edce --stat -- mongo/` — `(implicit X)` → `(using X)` file list (~60 files).
    - `git show 848b8e9e --stat -- mongo/` — supplementary file list (also touches `(implicit X)` sweep).
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### Slice 3.3` mongo subsection.
  </read_first>
  <action>
    1. Generate file list:
       ```bash
       git show eef0edce --name-only --pretty=format: | grep '^mongo/' | sort -u > /tmp/03-03-mongo-using-files.txt
       ```
    2. For each file: open in our tree, find `(implicit <name>: <T>)` parameter lists, rewrite to `(using <name>: <T>)`. Preserve parameter NAMES (callers may use named-arg form).
    3. Cross-check by comparing with `git show origin/master:<path>` — fork's choice is the answer.
    4. **Skip** parameter lists in files whose entire body is Phase-2-stubbed (`???`) — those are deferred. Verify per file.
    5. After each batch of ~15 files, `sbt commons-jvm/compile` exit 0 — catch errors early.
    6. Final per-file gate after entire sweep: `sbt commons-jvm/compile ;Test/compile` exit 0.
    7. Commit: `refactor(scala-3,mongo): (implicit X: T) → (using X: T) parameter list sweep` body: `Translated from origin/master@eef0edce + 848b8e9e.`
  </action>
  <verify>
    <automated>! git grep -nE '\(implicit\s+\w+\s*:' -- 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala'</automated>
  </verify>
  <acceptance_criteria>
    - `git grep -nE '\(implicit\s+\w+\s*:' -- 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala'` returns 0 hits (no parameter-list `(implicit X: T)` patterns remain — cross-check per file vs fork).
    - `sbt commons-jvm/compile ;Test/compile` exit 0.
    - Single commit: `refactor(scala-3,mongo): (implicit X: T) → (using X: T) parameter list sweep`.
    - Phase-2 stubs preserved (no `???` bodies touched except for param-list rewrite).
  </acceptance_criteria>
  <done>
    `(implicit X)` → `(using X)` sweep complete across mongo; 2a's BsonGenCodecs work intact (separate commit means a 2b failure can't strand 2a); compile + tests green.
  </done>
</task>

<task type="auto">
  <name>Task 2c: Mongo — remaining `implicit val/def/object` → `given` cleanup (fork 848b8e9e)</name>
  <files>
    mongo/jvm/src/main/scala/**/*.scala (residual implicit val/def/object hits)
  </files>
  <read_first>
    - `git show 848b8e9e --stat -- mongo/` — remaining implicit cleanup file list.
    - `git grep -nE '^\s*(inline\s+)?implicit\s+(def|val|object)' -- 'mongo'` — live inventory.
  </read_first>
  <action>
    1. Run `git grep -nE '^\s*(inline\s+)?implicit\s+(def|val|object)' -- 'mongo'` — enumerate remaining hits (post-2a/2b).
    2. For each hit: compare with `git show origin/master:<path>` and apply fork's choice:
       - most become `given`
       - some become `given Conversion`
       - `implicit object X extends Y` → `given X: Y with { … }` (or anonymous `given Y with { … }`)
    3. `sbt commons-jvm/compile ;Test/compile` exit 0.
    4. Re-grep:
       ```bash
       git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'mongo'
       # Expected: 0 hits (or only what fork preserved — read fork output to confirm).
       ```
    5. Commit: `refactor(scala-3,mongo): remaining implicit → given (clear -Werror)` body: `Translated from origin/master@848b8e9e.`
  </action>
  <verify>
    <automated>git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'mongo' | wc -l</automated>
  </verify>
  <acceptance_criteria>
    - `git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'mongo'` returns 0 hits (or only fork-preserved cases — verify against `git show origin/master:<path>`).
    - `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - Single commit: `refactor(scala-3,mongo): remaining implicit → given (clear -Werror)`.
  </acceptance_criteria>
  <done>
    Mongo implicit → given sweep complete; 2a (BsonGenCodecs) + 2b (using sweep) + 2c (residuals) form three single-purpose commits so any individual failure doesn't strand the others.
  </done>
</task>

<task type="auto">
  <name>Task 3: Benchmark + hocon sweep + MIGRATION.md update + draft PR</name>
  <files>
    benchmark/jvm/src/main/scala/com/avsystem/commons/ser/GenCodecBenchmarks.scala
    benchmark/jvm/src/main/scala/com/avsystem/commons/ser/StreamInputOutputBenchmark.scala
    hocon/src/main/scala/com/avsystem/commons/hocon/ConfigCompanion.scala
    hocon/src/main/scala/com/avsystem/commons/hocon/HTree.scala
    MIGRATION.md
  </files>
  <read_first>
    - `git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'benchmark' 'hocon'`
    - Per file: `git show origin/master:<path>` (single-source for benchmark/hocon per RESEARCH path mapping).
    - MIGRATION.md current §3.
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### MIGRATION.md §3 (source-compat) deltas per slice` for 3.3.
  </read_first>
  <action>
    1. **Benchmark sweep** — `GenCodecBenchmarks.scala` (~2 `implicit val/def`), `StreamInputOutputBenchmark.scala` (~2 `implicit val/def`):
       - Per-file copy/reconcile vs fork.
       - `sbt commons-jvm/compile` exit 0 (benchmark may not be in jvm aggregate — also run `sbt benchmark/compile` if separate).
       - Commit: `refactor(scala-3,benchmark): implicit val/def → given`.
    2. **Hocon sweep** — `ConfigCompanion.scala`, `HTree.scala`:
       - Generate per-file inventory `git grep -nE '\bimplicit\s+(def|val)' -- 'hocon/src/main/scala'`.
       - For each hit, compare with fork; rewrite per fork choice.
       - `sbt commons-jvm/compile` exit 0.
       - Commit: `refactor(scala-3,hocon): implicit val/def → given` (skip if 0 hits — note in PR body that hocon already clean).
    3. **MIGRATION.md §3 update**:
       - Add sub-entries:
         - `### core`: "implicit def Java interop conversions in `jiop/` → `given Conversion[A, B]`. Downstream callers using these conversions via implicit-conversion syntax unchanged; callers referencing the named def (e.g. `jOptionalAsScala`) by name break — use `summon[Conversion[…, …]].apply(…)` or rely on conversion."
         - `### core`: "`OptArg.argToOptArg` PRESERVED as `implicit def` (erasure-bridge collision rationale documented inline)."
         - `### core`: "`SerializationMacros.fun2GenRef` PRESERVED as `inline implicit def` (macro splice rationale documented inline)."
         - `### mongo`: "`BsonGenCodecs.objectIdCodec` / `objectIdKeyCodec` now `@deprecated def … = summon`; canonical instances are anonymous `given` declarations. Source-compat preserved for named-import callers via the deprecation shim."
         - `### mongo`: "`(implicit X: T)` parameter lists rewritten to `(using X: T)` across ~60 files. Call-site impact: positional implicit calls unchanged (Scala 3 accepts both); named-argument call sites must update to `using` form."
         - `### mongo`: "`implicit object X extends Y` → `given X: Y with { … }`. Downstream `import X._` callers must switch to `import X.given` for given-import semantics."
       - Commit: `docs(migration): record implicit → given source-compat impact`.
    4. Final acceptance grep:
       ```bash
       git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala' 'mongo' 'hocon' 'benchmark'
       # Expected: exactly 2 hits — OptArg.argToOptArg + SerializationMacros.fun2GenRef (or GenRef.fun2GenRef).
       # Each MUST be preceded by the verbatim fork explanatory comment.
       ```
    5. Final full gate:
       ```bash
       sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0
       ! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'
       ! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'
       ! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'
       ```
    6. Push + open draft PR:
       ```bash
       git push -u origin 03-03-implicit-to-given
       gh pr create --repo AVSystem/scala-commons --base scala-3 --head halotukozak:03-03-implicit-to-given --draft \
         --title "[Scala 3] implicit def/val → given" \
         --body "$(cat <<'EOF'
       Rewrites `implicit def` / `implicit val` / `implicit object` to Scala 3 `given` declarations across core, mongo, hocon, benchmark. `(implicit X: T)` parameter lists rewritten to `(using X: T)` in mongo per fork commit eef0edce. Two borderline cases preserved verbatim with fork's explanatory comments:
       - `OptArg.argToOptArg` — polymorphic Conversion would generate JVM erasure-bridge collision (A and OptArg value class both erase to Object).
       - `SerializationMacros.fun2GenRef` — macro splice over inline arg cannot be expressed as non-inline `Conversion.apply`.

       ## Source-compat impact
       - `BsonGenCodecs` named accessors (`objectIdCodec`, `objectIdKeyCodec`) preserved as `@deprecated def … = summon` shims.
       - `(implicit X)` → `(using X)` — positional calls unchanged; named-argument call sites must update.
       - `implicit object X extends Y` → `given X: Y with { … }` — `import X._` callers must switch to `import X.given`.

       Translated from fork `origin/master` commits 39c047eb, ebffde26, eef0edce, 8f70be80, 848b8e9e.

       **Slice:** 3.3 of Phase 3 (Scala 3 syntax modernization)
       **Merge order:** 3.1 → 3.2 → 3.3 → 3.4
       **Depends on:** #<3.2 PR number>
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
    - Final acceptance grep returns exactly 2 hits, both with verbatim fork explanatory comments above signature.
    - `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - PR open at AVSystem: draft, `[Scala 3]` title, milestone 1, base `scala-3`, body metadata block.
    - 8-10 commits on branch (3-4 core clusters + 2 borderline preserves + 3 mongo + 1-2 benchmark/hocon + 1 docs). No squash.
    - MIGRATION.md §3 has comprehensive 3.3 entries.
    - No new `@nowarn`/`-Wconf`. No `.planning/`. No GSD nomenclature.
  </acceptance_criteria>
  <done>
    Slice 3.3 PR open at AVSystem with all sweeps complete + borderline cases preserved + MIGRATION.md updated.
  </done>
</task>

</tasks>

<verification>
```bash
# Acceptance: only 2 documented exceptions remain
git grep -nE '^\s*(inline\s+)?implicit\s+(def|val)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala' 'mongo' 'hocon' 'benchmark'
# Expect: exactly 2 hits (OptArg.argToOptArg + SerializationMacros.fun2GenRef / GenRef.fun2GenRef)

# Compile + tests
sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0

# Cleanliness
! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'
! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'
! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'

# PR conventions
gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone
```
</verification>

<success_criteria>
- 2 borderline `implicit def`s preserved verbatim with fork comments; all others rewritten.
- BsonGenCodecs uses anonymous-given + `export X.given` + `@deprecated def … = summon` pattern per fork.
- `(implicit X: T)` → `(using X: T)` sweep applied across ~60 mongo files.
- `sbt compile ;Test/compile ;scalafmtCheckAll` exits 0.
- 8-10 commits per fork cadence; no squash.
- Draft PR at AVSystem with `[Scala 3]` title, milestone 1, base `scala-3`, body metadata.
- MIGRATION.md §3 updated with all 3.3 source-compat sub-entries.
- No new `@nowarn`/`-Wconf`. No `.planning/`. No GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/03-scala-3-syntax-modernization/03-03-SUMMARY.md` documenting per-module file/commit counts, borderline preservation locations + comments, PR URL.
</output>
