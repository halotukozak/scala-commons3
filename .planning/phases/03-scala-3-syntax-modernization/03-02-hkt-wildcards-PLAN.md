---
phase: 03-scala-3-syntax-modernization
plan: 02
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
  - core/src/main/scala/com/avsystem/commons/collection/CollectionAliases.scala
  - core/src/main/scala/com/avsystem/commons/di/Component.scala
  - core/src/main/scala/com/avsystem/commons/di/Components.scala
  - core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
  - core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
  - core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala
  - core/src/main/scala/com/avsystem/commons/misc/SelfInstance.scala
  - core/src/main/scala/com/avsystem/commons/misc/TypeString.scala
  - core/src/main/scala/com/avsystem/commons/misc/TypedMap.scala
  - core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala
  - core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala
  - core/src/main/scala/com/avsystem/commons/rpc/RpcMetadataCompanion.scala
  - core/src/main/scala/com/avsystem/commons/rpc/StandardRPCFramework.scala
  - core/src/main/scala/com/avsystem/commons/rpc/rpcAnnotations.scala
  - core/src/main/scala/com/avsystem/commons/serialization/FieldValues.scala
  - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
  - core/src/main/scala/com/avsystem/commons/serialization/HasGenCodec.scala
  - core/src/main/scala/com/avsystem/commons/serialization/InputOutput.scala
  - core/src/main/scala/com/avsystem/commons/serialization/wrappers.scala
  - core/src/main/scala/com/avsystem/commons/serialization/macroCodecs.scala
  - core/src/main/scala/com/avsystem/commons/serialization/cbor/CborAdtMetadata.scala
  - core/src/main/scala/com/avsystem/commons/serialization/cbor/CborOutput.scala
  - core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringOutput.scala
  - core/src/main/scala/com/avsystem/commons/tuples/TupleDerivation.scala
  - MIGRATION.md
autonomous: true
requirements: [SYNTAX-32-HKT-WILDCARDS, WORKFLOW-01, WORKFLOW-02, WORKFLOW-03, WORKFLOW-04, WORKFLOW-05, PR-01, PR-02, PR-03, QUALITY-01]

must_haves:
  truths:
    - "Applied-position `[_]` / `[_, _]` rewritten to `[?]` / `[?, ?]` across in-scope files"
    - "Kind-parameter declarations `F[_]` / `K[_, _]` (declaration positions) preserved verbatim"
    - "sbt compile + Test/compile + scalafmtCheckAll all green"
    - "PR opened off upstream/scala-3 tip, draft, [Scala 3] prefix, milestone 1"
    - "MIGRATION.md §3 notes 3.2 is pure type-level rename (no source-compat impact)"
  artifacts:
    - path: "core/src/main/scala/**/*.scala (15 files inventoried)"
      provides: "Applied wildcards switched to `?`"
    - path: "mongo/jvm/src/main/scala/**/*.scala (~30 files per fork commit 848b8e9e)"
      provides: "Applied wildcards switched to `?`"
    - path: "MIGRATION.md"
      provides: "§3 entry noting no source-compat impact for 3.2"
  key_links:
    - from: "compiler -Werror (or fork commit 848b8e9e diff)"
      to: "line:col positions of applied `[_]`"
      via: "compiler warning OR per-file diff vs fork"
      pattern: "\\[_(\\s*,\\s*_)*\\]"
---

<objective>
Slice 3.2 of Phase 3: rewrite all *applied-position* HKT wildcards from `[_]` / `[_, _]` to `[?]` / `[?, ?]` per Scala 3 changed-features. Preserve all *kind-parameter declarations* (`class C[F[_]]`, `def m[K[_]: Foo]`) verbatim — Scala 3 still uses `_` in those positions.

Purpose: Sweep Scala 2's `F[_]` applied wildcard out of type-argument positions. Pure type-level rename; no source-compat impact for downstream users.

Output:
- ~15 core files + ~30 mongo files + any hocon files with applied wildcards rewritten
- MIGRATION.md §3 entry
- Multiple Conventional Commits (per-module cadence)
- Draft PR `[Scala 3] tighten HKT wildcards (_ → ?)` off `upstream/scala-3`, milestone 1
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
Fork source-of-truth:
- `git show 848b8e9e -- mongo/` — fork's mongo applied-wildcard sweep (35 files, +132/-125 lines). Read the diff to learn line:col positions and the kind-decl-vs-applied classification fork applied.
- For each core file in scope, `git show origin/master:core/src/main/scala-3/<path>` shows fork's target shape.

Classification rule (CRITICAL — see RESEARCH Pitfall 3):
- **Kind-parameter declarations (PRESERVE `_`):** Type parameter brackets that *define* a higher-kinded type — `class Foo[F[_]]`, `trait Bar[K[_]: Monad]`, `def baz[M[_], A]`. The `_` here is the wildcard at the kind level. SCALA 3 KEEPS `_` IN THIS POSITION.
- **Applied positions (REWRITE to `?`):** Type arguments at use sites — `val x: Map[String, ?]`, `def m(xs: List[?])`, `type T = Either[?, String]`. These were `_` in Scala 2; Scala 3 deprecates that and requires `?`.

Anti-pattern (Pitfall 3): rewriting `class TypedMap[K[_]]` → `class TypedMap[K[?]]` is a syntax error.
</interfaces>

**PR conventions (non-negotiable):**
- Branch base: `upstream/scala-3` tip — NOT stacked on 3.1.
- PR title: `[Scala 3] tighten HKT wildcards (_ → ?)`
- PR body metadata block:
  ```
  **Slice:** 3.2 of Phase 3 (Scala 3 syntax modernization)
  **Merge order:** 3.1 → 3.2 → 3.3 → 3.4
  **Depends on:** #<3.1 PR number>
  **Base branch:** upstream/scala-3 (not stacked)
  ```
- `--draft` on open. Milestone 1. No GSD nomenclature. No `.planning/`. No new `@nowarn`/`-Wconf`.

**Commit cadence:** Multiple Conventional Commits, one per module/feature area. Examples:
- `refactor(scala-3,core): F[_] → F[?] in applied positions (serialization)`
- `refactor(scala-3,core): F[_] → F[?] in applied positions (rpc)`
- `refactor(scala-3,core): F[_] → F[?] in applied positions (misc + meta + di + collection + tuples)`
- `refactor(scala-3,core): F[_] → F[?] in SharedExtensions`
- `refactor(scala-3,mongo): F[_] → F[?] in applied positions (sweep)`
- `refactor(scala-3,hocon): F[_] → F[?] in applied positions` (if hocon has any)
- `docs(migration): record HKT wildcard tightening (type-level only, no source-compat)`
</context>

<tasks>

<task type="auto">
  <name>Task 1: Branch + per-file classification step (kind-decl vs applied) for core 15 files</name>
  <files>
    core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
    core/src/main/scala/com/avsystem/commons/collection/CollectionAliases.scala
    core/src/main/scala/com/avsystem/commons/di/Component.scala
    core/src/main/scala/com/avsystem/commons/di/Components.scala
    core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
    core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
    core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala
    core/src/main/scala/com/avsystem/commons/misc/SelfInstance.scala
    core/src/main/scala/com/avsystem/commons/misc/TypeString.scala
    core/src/main/scala/com/avsystem/commons/misc/TypedMap.scala
    core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala
    core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala
    core/src/main/scala/com/avsystem/commons/rpc/RpcMetadataCompanion.scala
    core/src/main/scala/com/avsystem/commons/rpc/StandardRPCFramework.scala
    core/src/main/scala/com/avsystem/commons/rpc/rpcAnnotations.scala
    core/src/main/scala/com/avsystem/commons/serialization/FieldValues.scala
    core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala
    core/src/main/scala/com/avsystem/commons/serialization/HasGenCodec.scala
    core/src/main/scala/com/avsystem/commons/serialization/InputOutput.scala
    core/src/main/scala/com/avsystem/commons/serialization/wrappers.scala
    core/src/main/scala/com/avsystem/commons/serialization/macroCodecs.scala
    core/src/main/scala/com/avsystem/commons/serialization/cbor/CborAdtMetadata.scala
    core/src/main/scala/com/avsystem/commons/serialization/cbor/CborOutput.scala
    core/src/main/scala/com/avsystem/commons/serialization/json/JsonStringOutput.scala
    core/src/main/scala/com/avsystem/commons/tuples/TupleDerivation.scala
  </files>
  <read_first>
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### Slice 3.2` section AND `### Pitfall 3`.
    - `git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala'` — produces line:col inventory.
    - For each file in scope, `git show origin/master:core/src/main/scala-3/<path>` — fork's target shape.
    - `git show 848b8e9e --stat` — fork's mongo sweep file list (used in Task 2).
  </read_first>
  <action>
    1. Cut branch off upstream/scala-3 tip: `git fetch upstream && git checkout -b 03-02-hkt-wildcards upstream/scala-3`.
    2. Generate per-file inventory:
       ```bash
       git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala' > /tmp/03-02-core-inventory.txt
       ```
    3. For EACH line in inventory, classify:
       - **kind-decl (LEAVE ALONE):** preceded by `class `, `trait `, `def `, `type ` AS A TYPE PARAMETER LIST opener (i.e. inside the *defining* bracket pair `[F[_]]`).
       - **applied (REWRITE):** at a use site — any other position. Type ascription, type argument, type alias RHS, function parameter type, return type, generic instantiation.
       - Cross-check: open `git show origin/master:core/src/main/scala-3/<file>` and find the corresponding line. Fork's choice IS the answer — if fork wrote `?`, applied; if fork left `_`, kind-decl.
    4. For each file:
       - Apply the targeted `[_]` → `[?]` rewrite ONLY at applied positions.
       - Save file.
       - `sbt commons-jvm/compile` — must stay exit 0. If "wildcard imports are not allowed in type parameter position" appears, you swept a kind-decl — revert that hunk.
    5. Group files into commit clusters by feature area:
       - **Commit A: serialization** — `FieldValues`, `GenCodec`, `HasGenCodec`, `InputOutput`, `wrappers`, `macroCodecs`, `cbor/CborAdtMetadata`, `cbor/CborOutput`, `json/JsonStringOutput`. Subject: `refactor(scala-3,core): F[_] → F[?] in applied positions (serialization)`.
       - **Commit B: rpc** — `AsRawReal`, `RPCFramework`, `RpcMetadataCompanion`, `StandardRPCFramework`, `rpcAnnotations`. Subject: `refactor(scala-3,core): F[_] → F[?] in applied positions (rpc)`.
       - **Commit C: misc + meta + di + collection + tuples** — `SealedUtils`, `SelfInstance`, `TypeString`, `TypedMap`, `AdtMetadataCompanion`, `MetadataCompanion`, `Component`, `Components`, `CollectionAliases`, `TupleDerivation`. Subject: `refactor(scala-3,core): F[_] → F[?] in applied positions (misc + meta + di + collection + tuples)`.
       - **Commit D: SharedExtensions** — its own commit because it's central. Subject: `refactor(scala-3,core): F[_] → F[?] in applied positions (SharedExtensions)`.
    6. After all 4 commits: full gate `sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'` exit 0.
  </action>
  <verify>
    <automated>sbt 'commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - After all 4 commits, `git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala'` lists ONLY kind-parameter declaration positions (verified by inspection: each remaining hit must be inside a `class Foo[…]`/`trait Bar[…]`/`def baz[…]`/`type Q[…]` type parameter list).
    - For each remaining hit: cross-check fork — `git show origin/master:core/src/main/scala-3/<file>` shows the same `_` at that position.
    - No new compile errors mentioning "wildcard imports are not allowed in type parameter position".
    - `sbt commons-jvm/compile ;commons-js/compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - 4 commits on branch named per fork-cadence.
  </acceptance_criteria>
  <done>
    Core inventory swept; only kind-decls remain as `_`; compile + Test/compile + scalafmt green; 4 commits landed.
  </done>
</task>

<task type="auto">
  <name>Task 2: Mongo sweep (~30 files per fork commit 848b8e9e) + hocon spot-check + final acceptance gate</name>
  <files>
    mongo/jvm/src/main/scala/**/*.scala (per fork 848b8e9e)
    hocon/src/main/scala/**/*.scala (if any applied wildcards present)
    MIGRATION.md
  </files>
  <read_first>
    - `git show 848b8e9e --stat -- mongo/` — fork's mongo file list + diff size.
    - `git show 848b8e9e -- mongo/` — diff hunks (line:col positions are the source of truth for where to swap).
    - `git grep -nE '\[_(\s*,\s*_)*\]' -- 'mongo' 'hocon'` — live inventory in our tree.
    - .planning/phases/03-scala-3-syntax-modernization/03-RESEARCH.md `### Slice 3.2` table for mongo file count.
  </read_first>
  <action>
    1. Generate fork file list:
       ```bash
       git show 848b8e9e --name-only --pretty=format: | grep '^mongo/' | sort -u > /tmp/03-02-mongo-files.txt
       ```
    2. For each file in `/tmp/03-02-mongo-files.txt`:
       - Open file in our tree (path is direct match per RESEARCH path-mapping table — mongo is single-source on fork).
       - Compare line-by-line with `git show origin/master:<path>` for applied-position `[_]` → `[?]` rewrites.
       - Apply ONLY where fork applied (skip kind-decls fork left as `_`).
       - **Skip** mongo macro stub files that Phase 2 left as `???` if compile breaks — re-apply Phase 2 stubs after copy (per RESEARCH Pitfall 2). Affected stubs: `BsonRef.Creator.ref`, `DataRefDsl.{ref,as,is,isNot}`, `TypedMongoUtils.optionalizeFirstArg`, `MongoEntityCompanion.ID = Any`, `K[_]` → `K[Any]` widenings.
    3. After each batch (~10 files), run `sbt commons-jvm/compile` to catch errors early.
    4. Commit: `refactor(scala-3,mongo): F[_] → F[?] in applied positions (sweep)` body: `Translated from origin/master@848b8e9e (mongo subset).`
    5. Hocon spot-check:
       ```bash
       git grep -nE '\[_(\s*,\s*_)*\]' -- 'hocon/src/main/scala'
       ```
       If any applied-position hits exist, rewrite + commit: `refactor(scala-3,hocon): F[_] → F[?] in applied positions`. If 0 hits, skip.
    6. Update MIGRATION.md §3:
       - Add entry under `### core` and `### mongo`: "HKT wildcard `_` → `?` in applied positions per Scala 3. Kind-parameter declarations preserved. No source-compat impact — type-argument-position syntax only; downstream callers unaffected."
       - Commit: `docs(migration): record HKT wildcard tightening (type-level only, no source-compat)`.
    7. Final acceptance gates:
       ```bash
       # 7a. Applied-position grep should classify all remaining hits as kind-decls (manual verify)
       git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala' 'hocon/src/main/scala' > /tmp/03-02-remaining.txt
       # Inspect /tmp/03-02-remaining.txt: every line MUST be a kind-parameter declaration position.

       # 7b. Compile + scalafmt
       sbt 'compile ;Test/compile ;scalafmtCheckAll'   # exit 0

       # 7c. No new @nowarn / -Wconf
       ! git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'

       # 7d. No .planning/
       ! git log upstream/scala-3..HEAD --name-only | grep -q '^\.planning/'

       # 7e. No GSD nomenclature
       ! git log upstream/scala-3..HEAD --pretty=%B | grep -iE 'gsd|phase |plan-'
       ```
    8. Push + open draft PR (analogous to 03-01 Task 3):
       ```bash
       git push -u origin 03-02-hkt-wildcards
       gh pr create --repo AVSystem/scala-commons --base scala-3 --head halotukozak:03-02-hkt-wildcards --draft \
         --title "[Scala 3] tighten HKT wildcards (_ → ?)" \
         --body "(body with metadata block — slice 3.2, depends on #<3.1 PR>, base upstream/scala-3 not stacked)"
       gh api PATCH /repos/AVSystem/scala-commons/issues/<PR_NUM> -f milestone=1
       ```
  </action>
  <verify>
    <automated>sbt 'compile ;Test/compile ;scalafmtCheckAll'</automated>
    <automated>gh pr view "$PR_NUM" --repo AVSystem/scala-commons --json isDraft,title,milestone</automated>
    <!-- NOTE: $PR_NUM is captured from the `gh pr create` output earlier in the action (step 8). The gh-pr-view verification step runs ONLY after `gh pr create` completes and $PR_NUM is set. -->
  </verify>
  <acceptance_criteria>
    - `git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala' 'hocon/src/main/scala'` returns ONLY kind-parameter declaration positions (each verified by inspection against fork master).
    - `sbt compile ;Test/compile ;scalafmtCheckAll` exit 0.
    - PR open at AVSystem/scala-commons: draft, `[Scala 3]` title prefix, milestone 1, base `scala-3`, body metadata block present.
    - PR body references fork commit `848b8e9e` and 3.1 PR number under `**Depends on:**`.
    - No new `@nowarn`/`-Wconf`. No `.planning/`. No GSD nomenclature.
    - MIGRATION.md §3 has new sub-entry noting no source-compat impact for 3.2.
  </acceptance_criteria>
  <done>
    Mongo sweep complete, hocon spot-checked, MIGRATION.md updated, full gate green, draft PR open at AVSystem with correct metadata.
  </done>
</task>

</tasks>

<verification>
```bash
# Acceptance: all remaining [_] hits are kind-decls
git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala' 'hocon/src/main/scala'
# Each line MUST be a kind-param declaration (manual inspection against fork)

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
- All applied-position `[_]` / `[_, _]` rewritten to `[?]` / `[?, ?]` in core + mongo (+ hocon if applicable).
- Kind-parameter declarations preserved verbatim.
- `sbt compile + Test/compile + scalafmtCheckAll` exits 0.
- 5-7 commits per fork cadence (core split into 4 commits + mongo 1 + hocon 0-1 + docs 1).
- Draft PR at AVSystem/scala-commons: base=`scala-3`, draft, `[Scala 3]` title, milestone 1, body metadata block.
- MIGRATION.md §3 entry recorded.
- No new `@nowarn` / `-Wconf`. No `.planning/`. No GSD nomenclature.
</success_criteria>

<output>
After completion, create `.planning/phases/03-scala-3-syntax-modernization/03-02-SUMMARY.md` documenting per-file applied-vs-kind-decl classification + count of rewrites per module + PR URL.
</output>
