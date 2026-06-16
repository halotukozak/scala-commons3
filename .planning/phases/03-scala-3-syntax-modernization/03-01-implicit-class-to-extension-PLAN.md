---
phase: 03-scala-3-syntax-modernization
plan: 01
type: execute
# NOTE on wave/depends_on: PRs in Phase 3 are NOT stacked - each branches off
# upstream/scala-3 tip. Frontmatter expresses no git-topology dependencies.
# Execution order (3.1 -> 3.2 -> 3.3 -> 3.4) is enforced via PR body metadata.
# The executor reads PR body, opens PRs in declared order, and lifts draft state
# in 3.1->3.2->3.3->3.4 order. Slice 3.5 is independent (parallel-safe).
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
  - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala
  - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala
  - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala
  - mongo/jvm/src/main/scala/com/avsystem/commons/mongo/reactive/ReactiveMongoExtensions.scala
  - MIGRATION.md
autonomous: true
requirements: [SYNTAX-31-IMPLICIT-CLASS, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

must_haves:
  truths:
    - "git grep 'implicit class' core/src/main/scala/ mongo/src/main/scala/ → 0 hits"
    - "sbt commons-jvm/compile + commons-js/compile + Test/compile + scalafmtCheckAll all green"
    - "Each affected file's extension/given shape matches fork origin/master semantically"
    - "PR opened off upstream/scala-3 tip, draft, [Scala 3] prefix, milestone 1"
    - "MIGRATION.md §3 has new entry for `implicit class` → `extension` source-compat"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala"
      provides: "4 private extensions (IterableOps, PairIterableOps, ListInputOps, ObjectInputOps) converted from implicit class to extension"
    - path: "mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala"
      provides: "ForCollection via given Conversion (HKT receiver pattern)"
    - path: "mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala"
      provides: "ForCollection via given Conversion"
    - path: "mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala"
      provides: "macroDslExtensions converted to extension block"
    - path: "mongo/jvm/src/main/scala/com/avsystem/commons/mongo/reactive/ReactiveMongoExtensions.scala"
      provides: "PublisherOps converted from implicit class AnyVal to extension on Publisher[T]"
    - path: "MIGRATION.md"
      provides: "§3 entry for `implicit class` → `extension`"
  key_links:
    - from: "UpdateOperatorsDsl.given Conversion"
      to: "ForCollection class"
      via: "anonymous given with [C[X] <: Iterable[X], T, R] => Conversion[...]"
      pattern: "given .* Conversion\\["
---

<objective>
Slice 3.1 of Phase 3: sweep all `implicit class` declarations to Scala 3 `extension` blocks (or `given Conversion` where extension can't infer HKT receiver). Land as a single narrow-scope PR off `upstream/scala-3` tip.

Purpose: Eliminate Scala 2-era `implicit class XOps[A](...) extends AnyVal` idiom in favor of Scala 3 canonical `extension [A](a: A) { ... }` syntax. First of four sequential slices (3.1 → 3.2 → 3.3 → 3.4); each branches off `upstream/scala-3` tip — merge order enforced via PR body metadata, NOT git topology.

Output:
- 5 source files updated (1 core + 4 mongo)
- MIGRATION.md §3 entry
- Multiple Conventional Commits within branch (mirror fork cadence — one per module/feature area)
- Draft PR at AVSystem/scala-commons titled `[Scala 3] convert implicit class to extension`, milestone 1
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/MIGRATION.md
@.planning/phases/03-scala-3-syntax-modernization/03-CONTEXT.md
@.planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md
@~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md

<interfaces>
Fork patterns to inherit verbatim (read with `git show origin/master:<path>`):

1. `core/src/main/scala-3/com/avsystem/commons/SharedExtensions.scala` — canonical `extension [A](a: A) { inline def …, def … }` shape.
2. `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala` (fork commit `eef0edce`) — HKT-receiver pattern using `given [C[X] <: Iterable[X], T, R] => Conversion[UpdateOperatorsDsl[C[T], R], ForCollection[C, T, R]] = ForCollection(_)`. Carry the explanatory comment verbatim.
3. `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala` — analogous shape.
4. `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala` — `macroDslExtensions` as `extension`.
5. `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/reactive/ReactiveMongoExtensions.scala` (fork commit `848b8e9e`) — `PublisherOps` → `extension [T](publisher: Publisher[T]) { … }`.
</interfaces>

**PR conventions (non-negotiable, from MEMORY.md):**
- Branch base: `upstream/scala-3` tip — NOT stacked.
- PR title: `[Scala 3] convert implicit class to extension`
- PR body MUST include this metadata block at the bottom:
  ```
  **Slice:** 3.1 of Phase 3 (Scala 3 syntax modernization)
  **Merge order:** 3.1 → 3.2 → 3.3 → 3.4
  **Depends on:** (none — first slice)
  **Base branch:** upstream/scala-3 (not stacked)
  ```
- `--draft` on open. Milestone 1 ("Scala 3"). No GSD nomenclature. No `.planning/` in commits. No new `@nowarn`/`-Wconf`.

**Fork-cadence commit style (per CONTEXT.md `### Fork workflow methodology`):**
- Multiple Conventional Commits within branch, one per module/feature area. NO squash.
- Example commits:
  - `refactor(scala-3,core): GenCodec implicit class → extension (4 private wrappers)`
  - `refactor(scala-3,mongo): UpdateOperatorsDsl / QueryOperatorsDsl implicit class → given Conversion (HKT receiver)`
  - `refactor(scala-3,mongo): MongoEntityCompanion macroDslExtensions implicit class → extension`
  - `refactor(scala-3,mongo): ReactiveMongoExtensions PublisherOps implicit class → extension`
  - `docs(migration): record implicit class → extension source-compat impact`
- Commit body MAY include: `Translated from origin/master@<sha>.`
</context>

<tasks>

<task type="auto">
  <name>Task 1: Branch + rewrite core GenCodec private value-class wrappers</name>
  <files>
    core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
  </files>
  <read_first>
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala (current state — find 4 `implicit class … extends AnyVal` at approx lines 400, 416, 429, 450 named `IterableOps`, `PairIterableOps`, `ListInputOps`, `ObjectInputOps`)
    - `git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala` (fork target shape; locate the same 4 wrappers and copy their `extension` form)
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### Slice 3.1` section
    - .planning/phases/03-scala-3-syntax-modernization/03-CONTEXT.md `<decisions>` (fork-cadence + translate method)
  </read_first>
  <action>
    1. Cut new branch: `git fetch upstream && git checkout -b 03-01-implicit-class-to-extension upstream/scala-3`.
    2. Run grep to verify starting count: `git grep -n 'implicit class' core/src/main/scala/ mongo/` — record baseline.
    3. Open `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala`.
    4. For each of the 4 `private implicit class X[…](…) extends AnyVal { … }` blocks (`IterableOps`, `PairIterableOps`, `ListInputOps`, `ObjectInputOps`):
       - Compare with fork's shape via `git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala | grep -n -A20 'extension '`
       - Rewrite to `extension` block matching fork's exact form (private extension wrappers — fork uses `private` modifier; preserve).
       - Match fork's `inline def` vs plain `def` decisions verbatim. Do NOT add or remove `inline` independently.
       - Preserve all method bodies byte-identical except for the receiver-binding name change (`private val a: A` → `(a: A)`).
    5. Run `sbt scalafmtAll` to normalize formatting.
    6. Compile: `sbt commons-jvm/compile` — must be exit 0.
    7. Commit: `git add core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala && git commit -m "refactor(scala-3,core): GenCodec implicit class → extension (4 private wrappers)"` (commit body: `Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala.`)
  </action>
  <verify>
    <automated>git grep -c 'implicit class' core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala</automated>
  </verify>
  <acceptance_criteria>
    - `git grep -n 'implicit class' core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala` → 0 hits.
    - `sbt commons-jvm/compile` exits 0.
    - `sbt scalafmtCheckAll` exits 0.
    - File contains 4 `extension` blocks where there were 4 `implicit class` blocks.
    - Diff vs fork (`diff <(git show HEAD:core/src/main/scala/.../GenCodec.scala | sed -n '/extension/,/^}/p') <(git show origin/master:core/src/main/scala-3/.../GenCodec.scala | sed -n '/extension/,/^}/p')`) shows semantic parity (allow scalafmt drift).
  </acceptance_criteria>
  <done>
    Single commit on `03-01-implicit-class-to-extension` branch named `refactor(scala-3,core): GenCodec implicit class → extension (4 private wrappers)`. `git grep 'implicit class' core/` → 0 hits. Compile + scalafmt green.
  </done>
</task>

<task type="auto">
  <name>Task 2: Rewrite mongo DSL files (UpdateOperatorsDsl, QueryOperatorsDsl, MongoEntityCompanion, ReactiveMongoExtensions) — 4 separate commits</name>
  <files>
    mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala
    mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala
    mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala
    mongo/jvm/src/main/scala/com/avsystem/commons/mongo/reactive/ReactiveMongoExtensions.scala
  </files>
  <read_first>
    - Each target file's current state.
    - `git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/UpdateOperatorsDsl.scala` (fork commit `eef0edce`) — locate `given [C[X] <: Iterable[X], T, R] => Conversion[UpdateOperatorsDsl[C[T], R], ForCollection[C, T, R]] = ForCollection(_)` with hoisted import + explanatory comment.
    - `git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/QueryOperatorsDsl.scala`
    - `git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala`
    - `git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/reactive/ReactiveMongoExtensions.scala` (fork commit `848b8e9e`).
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md Pitfall 6 (extension body can't contain imports) and Pitfall 7 (HKT-receiver named-arg call needs `given Conversion`, not `extension`).
  </read_first>
  <action>
    For each of the four mongo files, in order:

    **2a. UpdateOperatorsDsl.scala** (HKT-receiver — use `given Conversion`, NOT `extension`):
    1. Open file, locate `implicit class ForCollection[C[X] <: Iterable[X], T, R](dsl: UpdateOperatorsDsl[C[T], R]) { … }` and the inner `import MongoUpdateOperator._`.
    2. Hoist the `import MongoUpdateOperator._` ABOVE the converted block (extension/given body cannot contain imports per Pitfall 6).
    3. Replace `implicit class ForCollection` with `class ForCollection` + companion-level `given [C[X] <: Iterable[X], T, R] => Conversion[UpdateOperatorsDsl[C[T], R], ForCollection[C, T, R]] = ForCollection(_)`.
    4. Carry fork's explanatory comment VERBATIM (the one explaining "extension can't infer C/T from named-argument calls like push(sort = ...)").
    5. Match fork's exact shape — do not editorialize.
    6. `sbt commons-jvm/compile` exit 0 + `sbt scalafmtAll`.
    7. Commit: `refactor(scala-3,mongo): UpdateOperatorsDsl implicit class → given Conversion (HKT receiver)` body: `Translated from origin/master@eef0edce.`

    **2b. QueryOperatorsDsl.scala** — same pattern as 2a (HKT receiver, `given Conversion`, hoisted imports). Commit: `refactor(scala-3,mongo): QueryOperatorsDsl implicit class → given Conversion (HKT receiver)` body: `Translated from origin/master@eef0edce.`

    **2c. MongoEntityCompanion.scala** — `macroDslExtensions` `implicit class` block → `extension` block matching fork (plain `extension`, no HKT-receiver concern here per fork's choice). Commit: `refactor(scala-3,mongo): MongoEntityCompanion macroDslExtensions implicit class → extension` body: `Translated from origin/master@eef0edce.`

    **2d. ReactiveMongoExtensions.scala** — `PublisherOps` `implicit class … extends AnyVal` → `extension [T](publisher: Publisher[T]) { … }`. Match fork's shape from `git show origin/master:mongo/jvm/src/main/scala/com/avsystem/commons/mongo/reactive/ReactiveMongoExtensions.scala`. Commit: `refactor(scala-3,mongo): ReactiveMongoExtensions PublisherOps implicit class → extension` body: `Translated from origin/master@848b8e9e.`

    After all four commits:
    - Run full gate: `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` — exit 0.
    - Verify acceptance grep: `git grep -n 'implicit class' core/src/main/scala/ mongo/src/main/scala/ mongo/jvm/src/main/scala/ mongo/js/src/main/scala/` → 0 hits.
  </action>
  <verify>
    <automated>! git grep -nq 'implicit class' -- 'core/src/main/scala' 'mongo'</automated>
  </verify>
  <acceptance_criteria>
    - `git grep -n 'implicit class' core/src/main/scala/ mongo/` → 0 hits.
    - `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` exits 0.
    - 4 new commits on branch, each scoped to one file/module per fork cadence.
    - UpdateOperatorsDsl.scala + QueryOperatorsDsl.scala both contain `given .* Conversion[` (HKT pattern preserved per fork).
    - UpdateOperatorsDsl + QueryOperatorsDsl explanatory comment present (grep for "named-argument" or "extension methods fail to infer").
    - No `@nowarn` / `-Wconf` added: `! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'`.
    - All imports that used to live inside an `implicit class` body now hoisted above the `extension`/`given` declaration (no `extension { … import … }` patterns).
  </acceptance_criteria>
  <done>
    All four mongo files converted, four separate commits, full compile + scalafmt gate green, acceptance grep gate clean.
  </done>
</task>

<task type="auto">
  <name>Task 3: Update MIGRATION.md §3 + open draft PR</name>
  <files>
    MIGRATION.md
  </files>
  <read_first>
    - MIGRATION.md (current §3 source-compat structure)
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### MIGRATION.md §3 (source-compat) deltas per slice` for 3.1
    - ~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md (PR rules — title prefix, draft, milestone)
  </read_first>
  <action>
    1. Open MIGRATION.md, locate `## 3. Source-compat breaks` section.
    2. Add new sub-entry under `### core` (and/or `### mongo`) noting `implicit class` → `extension` rewrites:
       - GenCodec.scala: 4 private extensions — internal only, no downstream impact (private).
       - mongo/typed UpdateOperatorsDsl / QueryOperatorsDsl: `ForCollection` switched from `implicit class` to `given Conversion[…]` — callers using `new ForCollection(dsl)` directly break; standard implicit-conversion call sites unchanged.
       - mongo/typed MongoEntityCompanion.macroDslExtensions: extension methods — call-site transparent.
       - mongo/reactive ReactiveMongoExtensions.PublisherOps: extension on `Publisher[T]` — call-site transparent (extension methods resolved by same name).
    3. Commit: `docs(migration): record implicit class → extension source-compat impact`.
    4. Final gate sweep before push:
       - `sbt 'compile ;Test/compile ;scalafmtCheckAll'` exit 0
       - `git grep -n 'implicit class' core/src/main/scala/ mongo/` → 0 hits
       - `! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'` (no GSD nomenclature)
       - `! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'` (no .planning/)
       - `! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` (no new suppressions)
    5. Push: `git push -u origin 03-01-implicit-class-to-extension`.
    6. Open draft PR via `gh`:
       ```bash
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:03-01-implicit-class-to-extension \
         --draft \
         --title "[Scala 3] convert implicit class to extension" \
         --body "$(cat <<'EOF'
       Sweeps remaining `implicit class XOps[A](...) extends AnyVal` declarations to Scala 3 `extension` blocks. Mongo HKT-receiver DSLs (`UpdateOperatorsDsl.ForCollection`, `QueryOperatorsDsl.ForCollection`) use `given Conversion[…]` instead of `extension` because extension methods cannot infer `C[T]` from named-argument call sites — see fork commit eef0edce comment carried verbatim.

       ## Scope
       - core: `GenCodec.scala` 4 private value-class wrappers → `extension`
       - mongo: `UpdateOperatorsDsl`, `QueryOperatorsDsl`, `MongoEntityCompanion`, `ReactiveMongoExtensions` (5 implicit classes total)
       - MIGRATION.md §3 entries

       ## Acceptance
       - `git grep 'implicit class' core/src/main/scala mongo/` → 0 hits
       - `sbt compile + Test/compile + scalafmtCheckAll` green
       - No new `@nowarn` / `-Wconf`

       Translated from fork `origin/master` commits `eef0edce` (mongo) and `848b8e9e` (ReactiveMongoExtensions).

       **Slice:** 3.1 of Phase 3 (Scala 3 syntax modernization)
       **Merge order:** 3.1 → 3.2 → 3.3 → 3.4
       **Depends on:** (none — first slice)
       **Base branch:** upstream/scala-3 (not stacked)
       EOF
       )"
       ```
    7. Capture PR number from gh output; assign milestone:
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/<PR_NUMBER> -f milestone=1
       ```
  </action>
  <verify>
    <automated>gh pr view --repo AVSystem/scala-commons <PR_NUMBER> --json isDraft,title,milestone --jq '.isDraft and (.title | startswith("[Scala 3]")) and (.milestone.number == 1)'</automated>
  </verify>
  <acceptance_criteria>
    - MIGRATION.md §3 has new sub-entries for implicit class → extension changes in core (GenCodec) and mongo (UpdateOperatorsDsl, QueryOperatorsDsl, MongoEntityCompanion, ReactiveMongoExtensions).
    - PR opened at AVSystem/scala-commons with base `scala-3`, head `halotukozak:03-01-implicit-class-to-extension`.
    - PR is draft (`isDraft: true`), title prefix `[Scala 3]`, milestone "Scala 3" (#1).
    - PR body contains the four-line metadata block (Slice, Merge order, Depends on, Base branch).
    - Branch has 6 commits total: 1 core + 4 mongo + 1 docs(migration). No squash. No GSD nomenclature.
    - Final full gate `sbt 'compile ;Test/compile ;scalafmtCheckAll'` green.
  </acceptance_criteria>
  <done>
    Draft PR open at AVSystem/scala-commons with `[Scala 3]` title prefix, milestone 1, base `scala-3`, body metadata complete. PR URL printed for user to flip ready-for-review after CI confirms green.
  </done>
</task>

</tasks>

<verification>
Phase-level checks for slice 3.1:

```bash
# 1. Acceptance grep gate (locked)
! git grep -nq 'implicit class' -- 'core/src/main/scala' 'mongo'

# 2. Compile + tests + scalafmt
sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0

# 3. No new @nowarn / -Wconf
! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'

# 4. No .planning/ in commits
! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'

# 5. No GSD nomenclature
! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'

# 6. PR conventions
gh pr view <PR_NUM> --repo AVSystem/scala-commons --json isDraft,title,milestone
# Expect: isDraft=true, title starts with "[Scala 3]", milestone.number=1
```
</verification>

<success_criteria>
- Acceptance grep gate `git grep 'implicit class' core/src/main/scala mongo/` → 0 hits.
- `sbt compile + Test/compile + scalafmtCheckAll` exits 0.
- 6 commits on branch (1 per fork-style scope), no squash.
- MIGRATION.md §3 updated with `implicit class → extension` source-compat notes.
- Draft PR at AVSystem/scala-commons: base=`scala-3`, draft, `[Scala 3]` title, milestone 1, body metadata block present.
- No new `@nowarn` / `-Wconf`. No `.planning/` in commits. No GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/03-scala-3-syntax-modernization/03-01-SUMMARY.md` documenting:
- Files changed + LOC delta
- Commits landed (sha + subject)
- Acceptance gate results (grep counts before/after)
- PR URL + number
- Any deviations from fork shape with rationale
</output>
