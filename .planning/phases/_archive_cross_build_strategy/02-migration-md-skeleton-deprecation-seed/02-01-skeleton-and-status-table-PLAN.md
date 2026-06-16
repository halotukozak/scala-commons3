---
phase: 02-migration-md-skeleton-deprecation-seed
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - MIGRATION.md
autonomous: true
commit_docs: false
requirements: [DOC-01, DOC-04, WORKFLOW-01]
must_haves:
  truths:
    - "Branch `02-migration-md` exists locally, cut from `upstream/scala-3`"
    - "`MIGRATION.md` exists at repo root"
    - "`MIGRATION.md` opens with H1 `# Scala 3 Migration Status` and one paragraph overview"
    - "Section `## How to update` lists exactly 5 numbered rules"
    - "Section `## Per-module status` carries a GFM table with 13 rows: macros, made, core, hocon, mongo, mongo-js, core-js, benchmark3, jetty, analyzer, spring, RPC, cbor"
    - "Section `## 2.13-only modules` exists with `### jetty`, `### analyzer`, `### spring`, `### RPC` subsections, each carrying a rationale paragraph"
    - "MIGRATION.md contains no emoji, no `✓`, no first-person plural (`we`/`our`), no GSD vocabulary, no `.planning/` reference"
  artifacts:
    - path: MIGRATION.md
      provides: "Top-level migration tracking doc with skeleton, status table, 2.13-only rationale"
      contains: "## Per-module status"
      min_lines: 80
  key_links:
    - from: "Per-module status table"
      to: "2.13-only modules section"
      via: "rows tagged `2.13-only` for jetty/analyzer/spring/RPC are explained in subsequent section"
      pattern: "2.13-only"
    - from: "Per-module status table"
      to: "project/Commons.scala jvm2 aggregate"
      via: "2.13-only modules section cross-references `jvm2` aggregate from Phase 1 substrate"
      pattern: "jvm2"
---

<objective>
Cut migration branch `02-migration-md` from `upstream/scala-3`, then create `MIGRATION.md` at repo root carrying the skeleton (Overview, How to update, Per-module status, 2.13-only modules) plus the rendered 13-row status table and 4 rationale paragraphs. Deprecation log section is intentionally STUBBED here (just the `## Deprecation log` heading) — Plan 02 populates it.

Purpose: Establish the file, its top-half content, and the canonical section ordering so Plan 02 (deprecation seed) and Plan 03 (check script) can grep against a stable structure. Documentation-only — no source/build/CI changes.

Output: Local branch `02-migration-md` carrying one commit that adds `MIGRATION.md` with the skeleton + status table + 2.13-only section.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/ROADMAP.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-RESEARCH.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-VALIDATION.md
</context>

<interfaces>
Section ordering for `MIGRATION.md` (locked by CONTEXT.md):

```
# Scala 3 Migration Status

<one-paragraph overview>

## How to update
<5 numbered rules, ≤7 lines total>

## Per-module status
<GFM table, 13 rows>

## 2.13-only modules
### jetty
### analyzer
### spring
### RPC

## Deprecation log
<populated by Plan 02 — Plan 01 leaves an empty heading + placeholder>
```

Per-module status table columns: `Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes`.

Status vocabulary (single token, lowercase): `cross`, `stub`, `2.13-only`, `pending`, `wip`.
MiMa / Tasty-MiMa column tokens: `green`, `red`, `n/a`, `pending`.

Row values for the Phase 2 PR (from RESEARCH.md, reconciled against current `build.sbt`):

| Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes |
|--------|------|-----|------|------------|-------|
| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty (next port). |
| made | n/a | pending | n/a | n/a | Scala-3-only dep, pinned 0.1.0. |
| core | cross | pending | green | pending | Cross-compile target; tests pending. |
| hocon | cross | pending | green | pending | Pure-Scala; first downstream port. |
| mongo | cross | pending | green | pending | Uses `for3Use2_13` wrapper. |
| mongo-js | cross | pending | n/a | n/a | ScalaJS variant. |
| core-js | cross | pending | n/a | n/a | ScalaJS variant. |
| benchmark3 | cross | pending | n/a | n/a | JMH benchmarks. |
| jetty | 2.13-only | n/a | n/a | n/a | Servlet API churn; under `jvm2`. |
| analyzer | 2.13-only | n/a | n/a | n/a | Compiler plugin against scala-2 internals. |
| spring | 2.13-only | n/a | n/a | n/a | Deprecated upstream; no port planned. |
| RPC | 2.13-only | n/a | n/a | n/a | Logical concern under `core`; macro-stack-dependent. |
| cbor | cross | pending | pending | pending | Sub-package of `core`; tracked separately for MiMa scope. |
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Cut branch `02-migration-md` from upstream/scala-3</name>
  <files>(none — git plumbing)</files>
  <read_first>
    - .planning/REQUIREMENTS.md §WORKFLOW-01 (branch off latest upstream/scala-3)
    - .planning/ROADMAP.md §"Per-PR Workflow" (steps 1-3)
  </read_first>
  <action>
    From project root, fetch upstream and cut a fresh branch off `upstream/scala-3`:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git fetch upstream
        git status --porcelain    # MUST be empty before branching; surface dirty tree to user
        git checkout -b 02-migration-md upstream/scala-3

    Rationale: REQ WORKFLOW-01 mandates branching off the latest `upstream/scala-3`. Phase 1's PR may not be merged yet — that is fine; Phase 2 is documentation-only and does not depend on Phase 1's build changes at the source level. If Phase 1 is merged, this branch will already carry Phase 1's substrate; if not, the docs still apply.

    Note: We use `02-migration-md` (no GSD nomenclature, no `phase` keyword) per REQ WORKFLOW-04. Do NOT use `gsd:phase-2-...` or similar.

    Sanity check after branch creation:

        git rev-parse --abbrev-ref HEAD    # MUST print `02-migration-md`
        git log -1 --format=%H upstream/scala-3    # SHOULD equal current HEAD SHA
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; git rev-parse --abbrev-ref HEAD | grep -Fxq '02-migration-md' &amp;&amp; [ "$(git rev-parse HEAD)" = "$(git rev-parse upstream/scala-3)" ]</automated>
  </verify>
  <acceptance_criteria>
    - `git rev-parse --abbrev-ref HEAD` prints exactly `02-migration-md`.
    - `git rev-parse HEAD` equals `git rev-parse upstream/scala-3` (branch base unchanged — no commits yet).
    - `git status --porcelain` is empty.
    - Branch name contains no GSD nomenclature: `echo 02-migration-md | grep -iE 'gsd|phase|plan-phase'` exits 1 (the `02-` numeric prefix does not match `phase [0-9]`).
  </acceptance_criteria>
  <done>Local branch `02-migration-md` cut from `upstream/scala-3`, working tree clean, no commits added yet.</done>
</task>

<task type="auto">
  <name>Task 2: Write MIGRATION.md skeleton with overview, How to update, per-module status table, 2.13-only sections, and empty Deprecation log heading</name>
  <files>MIGRATION.md</files>
  <read_first>
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md §"Implementation Decisions" (full)
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-RESEARCH.md §"Architecture Patterns" → "Document layout (final order)" and "Per-module status table — initial row values"
    - build.sbt (current branch HEAD on master, lines 213-480) — to confirm module list mapping at write time
  </read_first>
  <action>
    Create `/Users/bkozak/IdeaProjects/scala-commons3/MIGRATION.md` with the exact content below. Do NOT alter section ordering, heading levels, status vocabulary, or table column order.

    Tone rules (locked by CONTEXT.md):
    - No emoji. No banners. No `✓`.
    - Impersonal voice: "Module X cross-builds on Scala 3" — never "We've ported X" / "Our migration".
    - No GSD vocabulary: no occurrences of `gsd`, `wave`, `phase 1`, `phase 2`, `RESEARCH.md`, `PLAN.md`, `CONTEXT.md`, `.planning/`.
    - Status flips are facts, not announcements.

    Content to write VERBATIM (replace `<<<BR>>>` with a single blank line in the final file):

    ```markdown
    # Scala 3 Migration Status

    Tracks the Scala 3 cross-compile state of `AVSystem/scala-commons` on the `scala-3` branch. Source of truth for which modules cross-build on which Scala version, which carry MiMa baselines, and which deprecations remain to be resolved. Maintained PR-by-PR alongside the work it tracks.

    ## How to update

    1. When a PR ports a module, flip that module's row in the Per-module status table in the same PR.
    2. When a PR discovers a new `@deprecated` symbol, append it to the Deprecation log in the same PR.
    3. MiMa and Tasty-MiMa columns change only after CI proves the new state green.
    4. Commit messages and this document use upstream-facing terms only — no internal tooling vocabulary.
    5. Internal planning artifacts never appear in PR diffs.

    ## Per-module status

    Status tokens: `cross` (cross-builds on both versions), `stub` (Scala 3 wired with empty/placeholder sources), `2.13-only` (excluded from Scala 3 aggregate), `pending` (not yet started), `wip` (in progress on a feature branch). MiMa / Tasty-MiMa tokens: `green`, `red`, `n/a`, `pending`. Rows below `core` may be logical groupings, not separate sbt modules — see Notes.

    | Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes |
    |--------|------|-----|------|------------|-------|
    | macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty in the next port. |
    | made | n/a | pending | n/a | n/a | Scala-3-only dep, pinned to `io.github.halotukozak:made_3:0.1.0`. |
    | core | cross | pending | green | pending | Cross-compile target; tests still pending on Scala 3. |
    | hocon | cross | pending | green | pending | Pure-Scala; first downstream port after `core`. |
    | mongo | cross | pending | green | pending | Uses `CrossVersion.for3Use2_13` wrapper on Scala 3. |
    | mongo-js | cross | pending | n/a | n/a | ScalaJS variant of `mongo`. |
    | core-js | cross | pending | n/a | n/a | ScalaJS variant of `core`. |
    | benchmark3 | cross | pending | n/a | n/a | JMH benchmark module. |
    | jetty | 2.13-only | n/a | n/a | n/a | Servlet/RPC churn; lives under `jvm2` aggregate. |
    | analyzer | 2.13-only | n/a | n/a | n/a | Compiler plugin against scala-2 internals. |
    | spring | 2.13-only | n/a | n/a | n/a | Deprecated upstream; no port planned. |
    | RPC | 2.13-only | n/a | n/a | n/a | Logical concern under `core`; macro-stack-dependent. |
    | cbor | cross | pending | pending | pending | Sub-package of `core`; tracked separately for MiMa scope. |

    ## 2.13-only modules

    Modules in this group are excluded from the `jvm` aggregate (cross-built on both Scala versions) and live under the `jvm2` aggregate or remain unaggregated in `build.sbt`. Rationale per module follows.

    ### jetty

    `jetty` integrates with the Servlet API and consumes the RPC framework. Both depend on the macro stack and on Servlet API versions that have not stabilized for Scala 3. The module stays under the `jvm2` aggregate so `++3.x jvm/test` skips it cleanly. Reactivation is deferred pending an RPC port decision (see also `RPC`).

    ### analyzer

    `analyzer` is a Scala 2 compiler plugin that hooks into `scala.tools.nsc` internals. The Scala 3 compiler exposes a different plugin API; porting requires a rewrite against `dotty.tools.dotc.plugins` rather than a cross-build. Kept commented out in `build.sbt` and tracked here as `2.13-only`.

    ### spring

    `spring` integrates with Spring Framework annotations. The module is deprecated upstream — no port to Scala 3 is planned. Status `2.13-only` reflects its position in the build; no future reactivation is expected.

    ### RPC

    The RPC framework lives as a logical concern inside `core/src/main/scala-2.13/com/avsystem/commons/rpc/`. It is not a separate sbt module. Porting depends on the macro stack (see `macros`) and on the broader serialization / typeclass derivation port (see `core`). Tracked here so the deferral is explicit.

    ## Deprecation log

    Seeded from a `@deprecated` scan against fork `master`. Populated in the same PR as this skeleton.
    ```

    Concrete write steps:
    1. Create the file (it must not exist already — verify with `test ! -f /Users/bkozak/IdeaProjects/scala-commons3/MIGRATION.md` before writing).
    2. Use the Write tool with the content above, expanding `<<<BR>>>` literally to blank lines (the content above already has blank lines; do not introduce extra ones).
    3. After write, sanity-check by reading the file back and grepping for the headings.

    DO NOT commit yet — Plan 02 will populate the Deprecation log in the same MIGRATION.md, then Plan 02 commits its own slice. Plan 01 commits in Task 3 (the next task).

    Wait — correction: Plan 01 commits its skeleton at the end of THIS task. Plan 02 will then add the deprecation log content in a subsequent commit. Each plan produces its own commit so the branch history is auditable.

    Stage explicitly to avoid `.planning/` leakage:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git add MIGRATION.md
        git status --porcelain    # MUST show only `A  MIGRATION.md`; surface extras to user
        git commit -m 'docs(migration): add MIGRATION.md skeleton with per-module status and 2.13-only sections'

    Commit message rules: no GSD nomenclature; prefix `docs(migration):` is allowed per upstream convention.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; test -f MIGRATION.md &amp;&amp; head -1 MIGRATION.md | grep -Fxq '# Scala 3 Migration Status' &amp;&amp; grep -Fxq '## How to update' MIGRATION.md &amp;&amp; grep -Fxq '## Per-module status' MIGRATION.md &amp;&amp; grep -Fxq '## 2.13-only modules' MIGRATION.md &amp;&amp; grep -Fxq '## Deprecation log' MIGRATION.md &amp;&amp; grep -Fxq '### jetty' MIGRATION.md &amp;&amp; grep -Fxq '### analyzer' MIGRATION.md &amp;&amp; grep -Fxq '### spring' MIGRATION.md &amp;&amp; grep -Fxq '### RPC' MIGRATION.md &amp;&amp; [ "$(awk '/^## How to update/,/^## /' MIGRATION.md | grep -cE '^[0-9]+\.')" -ge 5 ] &amp;&amp; [ "$(awk '/^## Per-module status/,/^## 2/' MIGRATION.md | grep -cE '^\| (macros|made|core|hocon|mongo|mongo-js|core-js|benchmark3|jetty|analyzer|spring|RPC|cbor) ')" -eq 13 ] &amp;&amp; ! grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md &amp;&amp; ! grep -F '.planning/' MIGRATION.md &amp;&amp; ! grep -E '\b(we|our|✓)\b' MIGRATION.md &amp;&amp; git log -1 --format=%s | grep -E '^docs\(migration\):' &amp;&amp; ! git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'</automated>
  </verify>
  <acceptance_criteria>
    - `test -f /Users/bkozak/IdeaProjects/scala-commons3/MIGRATION.md` exits 0.
    - `head -1 MIGRATION.md` prints exactly `# Scala 3 Migration Status`.
    - `grep -Fxq '## How to update' MIGRATION.md` exits 0.
    - `grep -Fxq '## Per-module status' MIGRATION.md` exits 0.
    - `grep -Fxq '## 2.13-only modules' MIGRATION.md` exits 0.
    - `grep -Fxq '## Deprecation log' MIGRATION.md` exits 0.
    - `grep -Fxq '### jetty' MIGRATION.md`, `grep -Fxq '### analyzer' MIGRATION.md`, `grep -Fxq '### spring' MIGRATION.md`, `grep -Fxq '### RPC' MIGRATION.md` — each exits 0.
    - `awk '/^## How to update/,/^## /' MIGRATION.md | grep -cE '^[0-9]+\.'` prints `5` (exactly 5 numbered rules).
    - `awk '/^## Per-module status/,/^## 2/' MIGRATION.md | grep -cE '^\| (macros|made|core|hocon|mongo|mongo-js|core-js|benchmark3|jetty|analyzer|spring|RPC|cbor) '` prints `13` (all 13 rows present).
    - `grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md` exits 1 (no GSD vocabulary).
    - `grep -F '.planning/' MIGRATION.md` exits 1 (no internal planning paths).
    - `grep -E '\b(we|our|✓)\b' MIGRATION.md` exits 1 (impersonal voice, no checkmark emoji).
    - Commit prefix is `docs(migration):` — `git log -1 --format=%s | grep -E '^docs\(migration\):'` exits 0.
    - Commit message has no GSD nomenclature — `git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - Commit touches ONLY `MIGRATION.md`: `git show --stat HEAD --name-only --pretty= | grep -v '^$' | sort -u` prints exactly `MIGRATION.md`.
  </acceptance_criteria>
  <done>`MIGRATION.md` exists at repo root with skeleton, How-to-update rules, 13-row status table, four 2.13-only rationale subsections, empty Deprecation log heading; committed as a single `docs(migration):` commit; no GSD nomenclature; no `.planning/` leakage.</done>
</task>

</tasks>

<verification>
After Plan 01 completes:

1. `cd /Users/bkozak/IdeaProjects/scala-commons3 && git rev-parse --abbrev-ref HEAD` prints `02-migration-md`.
2. `git log upstream/scala-3..HEAD --oneline | wc -l` prints `1`.
3. `git diff upstream/scala-3..HEAD --name-only` prints exactly `MIGRATION.md`.
4. `MIGRATION.md` carries all four section headings and the 13-row table (per acceptance criteria above).
5. No `.planning/` paths in any commit (REQ WORKFLOW-05): `git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0`.
6. No GSD nomenclature in any commit message (REQ WORKFLOW-04): `git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
</verification>

<success_criteria>
- Branch `02-migration-md` cut from `upstream/scala-3` (REQ WORKFLOW-01).
- `MIGRATION.md` exists at repo root with all required sections in the locked order (REQ DOC-01).
- Per-module status table has all 13 rows with locked status vocabulary (REQ DOC-01).
- 2.13-only section names jetty/analyzer/spring/RPC with rationale paragraphs (REQ DOC-04).
- "How to update" section codifies 5 PR contract rules.
- No GSD nomenclature, no emoji, no first-person voice in doc body.
- Plan 01 produces exactly one commit prefixed `docs(migration):`.
</success_criteria>

<output>
After completion, create `.planning/phases/02-migration-md-skeleton-deprecation-seed/02-01-SUMMARY.md` capturing:
- Branch HEAD SHA on `02-migration-md`
- Output of `wc -l MIGRATION.md`
- Output of `git log upstream/scala-3..HEAD --oneline`
- Commit hash + subject from `git log -1 --format='%h %s'`
- Confirmation that no `.planning/` paths and no GSD nomenclature appear in branch history
</output>
