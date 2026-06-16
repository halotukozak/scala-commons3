---
phase: 05-core-scala-3-baseline-port
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala-2.13/**
  - core/src/main/scala/**
autonomous: true
commit_docs: false
requirements: [CORE-02, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05, QUALITY-01]

must_haves:
  truths:
    - "Branch 05-core-scala-3-baseline-port is cut off 04-made-integration tip (c3e54b16) so PR cascades on PR #859"
    - "Every Scala-2-only file (macro defs, whitebox impls, RPC framework) in core/src/main/scala/ has been moved to core/src/main/scala-2.13/ via git mv (history preserved)"
    - "Files that compile on BOTH 2.13 and 3 stay in core/src/main/scala/ (shared)"
    - "++2.13.18 commons-core/compile remains GREEN after the move"
    - "Commit message has NO GSD nomenclature; .planning/ not in diff"
  artifacts:
    - path: "core/src/main/scala-2.13/com/avsystem/commons/"
      provides: "Relocated Scala-2-only sources (di/Components, rpc/*, macroCodecs, GenCodec.scala, GenKeyCodec.scala, GenObjectCodec.scala, GenRef.scala, whenAbsent, Sam, SamCompanion, etc.)"
      min_lines: 1
    - path: "core/src/main/scala/com/avsystem/commons/"
      provides: "Shared-byte-identical files only (no `= macro` defs)"
  key_links:
    - from: "core/src/main/scala-2.13/"
      to: "sbt mkSourceDirs (Phase 1 build wiring)"
      via: "unmanagedSourceDirectories on Scala 2.13"
      pattern: "scala-2.13"
---

<objective>
Cut the Phase 5 branch off Phase 4's PR-tip and relocate every Scala-2-only source file under `core/src/main/scala/` into `core/src/main/scala-2.13/` (the cross-build directory that only Scala 2.13 sees). This is the prerequisite Plan 04-02 deferred (the reverted "Option B" mass move): without it, the Scala 3 compiler tries to compile macro-def syntax (`def x[T]: T = macro Y.z[T]`) and dies.

Purpose: prepare ground for Plan 05-02's cherry-pick of `core/src/main/scala-3/`. After this plan, `core/src/main/scala/` contains ONLY files that compile on both Scala versions; `core/src/main/scala-2.13/` holds the macro/legacy surface; `core/src/main/scala-3/` still holds only the 5 wiring primitives from Phase 4.

Output: ~30+ `git mv` operations, single big refactor commit, ++2.13 commons-core/compile still green.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/phases/05-core-scala-3-baseline-port/05-CONTEXT.md
@.planning/phases/04-made-integration/04-02-port-wiring-primitives-SUMMARY.md
@MIGRATION.md
@~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md

<workflow_rules>
- Branch off 04-made-integration tip `c3e54b16` (PR #859 base) — cascading PR stack on AVSystem.
- No `@nowarn` / `-Wconf` introductions (memory rule `feedback_fix_dont_suppress_warnings.md`).
- `// format: off` around scala-2 macro defs is OK if relocated files trip scalafmt (memory rule `feedback_format_off_macro_defs_ok.md`).
- Commit messages: NO GSD nomenclature, NO `.planning/` paths in diff.
- DO NOT push or open a PR in this plan — that is Plan 05-04.
</workflow_rules>

<known_seed_files>
From `git grep -l '= macro ' core/src/main/scala/` and §"Deprecation log" of MIGRATION.md (which already references `core/src/main/scala-2.13/...` paths — that doc was authored in anticipation of this very relocation):

Definite scala-2-only (`= macro ` present OR scala-2 whitebox/macro-bundle code):
- core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
- core/src/main/scala/com/avsystem/commons/annotation/AnnotationAggregate.scala
- core/src/main/scala/com/avsystem/commons/annotation/positioned.scala
- core/src/main/scala/com/avsystem/commons/di/Components.scala
- core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala
- core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala
- core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala
- core/src/main/scala/com/avsystem/commons/misc/{AnnotationOf,ApplierUnapplier,Delegation,Implicits,Sam,SamCompanion,SealedUtils,SelfInstance,SimpleClassName,SourceInfo,TypeString,ValueEnum}.scala
- core/src/main/scala/com/avsystem/commons/rpc/{AsRawReal,RPCFramework,RawRpcCompanion,RpcMetadataCompanion,RpcUtils}.scala
- core/src/main/scala/com/avsystem/commons/serialization/{GenCodec,GenKeyCodec,GenObjectCodec,GenRef,macroCodecs,whenAbsent}.scala

Plus any further file the planner finds via `git grep -nE '(= macro |whitebox\.Context|blackbox\.Context|c\.Tree)' core/src/main/scala/`.

Files that may also live on master's `scala-3/` AND in current `scala/` (overlap): per `git ls-tree -r master -- core/src/main/scala-3/`, master has scala-3 counterparts for many of the above (AnnotationAggregate, SharedExtensions, AdtMetadataCompanion, ApplierUnapplier, AnnotationOf, Delegation, Implicits, SealedUtils, SelfInstance, SimpleClassName, SourceInfo, TypeString, ValueEnum, GenCodec, GenKeyCodec, GenObjectCodec, GenRef, metaAnnotations, metadata.scala, MetadataCompanion). Those will be cherry-picked in Plan 05-02. This plan only relocates the 2.13 side — it does NOT touch `scala-3/` and does NOT bring in master's `scala-3/` files.
</known_seed_files>

<interfaces>
<!-- After this plan, the sbt source dirs look like: -->
<!-- core/src/main/scala/            — cross-version (byte-identical on both Scala versions) -->
<!-- core/src/main/scala-2.13/       — Scala 2.13 only (macro defs, RPC framework, deprecated APIs) -->
<!-- core/src/main/scala-3/          — Scala 3 only (5 wiring primitives from Phase 4; expanded in Plan 05-02) -->
<!-- Phase 1's mkSourceDirs already routes both scala-2.13/ and scala-3/ via unmanagedSourceDirectories. -->
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Cut branch off 04-made-integration tip and confirm clean baseline</name>
  <files>(no source changes — branch operation only)</files>
  <action>
1. From repo root, verify current state: `git rev-parse HEAD` should report `c3e54b16` (Phase 4 PR #859 tip); `git status` clean; `git branch --show-current` reports `04-made-integration`.
2. Create new branch off current HEAD: `git checkout -b 05-core-scala-3-baseline-port`.
3. Confirm branch tip: `git log --oneline -1` should show `c3e54b16 docs(migration): record made integration and core wiring port`.
4. Sanity baseline: run `sbt '++2.13.18 commons-core/compile' '++3.8.2 commons-macros/compile' scalafmtCheckAll` — all three MUST exit 0. This is the precondition. If any fails STOP and report; do not proceed to Task 2.
5. Capture baseline error count for ++3.8.2 commons-core/compile (expected RED, ~136 errors per Phase 4 SUMMARY): `sbt '++3.8.2 commons-core/compile' 2>&1 | tail -20`. Record the error count in execution log — it is the "before" baseline.

DO NOT push the new branch yet. DO NOT touch any source file in this task.
  </action>
  <verify>
    <automated>git branch --show-current | grep -qx 05-core-scala-3-baseline-port &amp;&amp; git rev-parse HEAD | grep -q ^c3e54b16</automated>
  </verify>
  <done>
- Branch `05-core-scala-3-baseline-port` exists locally at tip `c3e54b16`.
- ++2.13.18 commons-core/compile, ++3.8.2 commons-macros/compile, scalafmtCheckAll all GREEN.
- Working tree clean. No commits added yet.
  </done>
</task>

<task type="auto">
  <name>Task 2: Enumerate Scala-2-only files and produce relocation list</name>
  <files>(scratch list — no source moves yet)</files>
  <action>
Discover the full set of files that must move from `core/src/main/scala/` to `core/src/main/scala-2.13/`:

1. Macro-def heuristic (highest confidence):
   ```sh
   git grep -nl '= macro ' core/src/main/scala/
   ```
2. Whitebox/blackbox macro-bundle heuristic:
   ```sh
   git grep -nlE '(whitebox\.Context|blackbox\.Context|reflect\.macros\.)' core/src/main/scala/
   ```
3. Scala-2-syntax heuristic (deprecated `@`-style early-init, `with` mixin macros, etc.):
   ```sh
   git grep -nlE '(extends [A-Z][a-zA-Z0-9_]*Companion\(|object [A-Z][a-zA-Z0-9_]* extends StaticAnnotation with )' core/src/main/scala/
   ```
4. Files whose master scala-3 counterpart EXISTS (these will be cherry-picked in Plan 05-02 → their 2.13 source MUST relocate so the 3.x file can take effect under `scala-3/`):
   ```sh
   git ls-tree -r master --name-only -- core/src/main/scala-3/ | sed 's#scala-3/#scala/#' | while read f; do test -f "$f" &amp;&amp; echo "$f"; done
   ```
5. RPC framework files in `core/src/main/scala/com/avsystem/commons/rpc/` — all relocate (RPC stays 2.13-only per MIGRATION.md §"2.13-only modules").
6. Union all four lists, deduplicate, write to `/tmp/05-01-relocations.txt`.
7. Manual review (printed to terminal — judgement call): for any file in the list whose content is byte-identical-safe on both Scala versions AND has no scala-3 counterpart on master, EXCLUDE it from the move (leave in `scala/`). Conservative default: when in doubt, MOVE — Plan 05-02's cherry-pick will overlay scala-3/ counterparts atop relocated 2.13/ files, and shared genuinely-cross-version utilities can move back in Phase 6 if needed (deferred-cleanup decision per Phase 5 CONTEXT §`deferred`).
8. Final relocation list MUST contain at minimum the files in `<known_seed_files>` above. Expected total: ~30-50 files.

Output: `/tmp/05-01-relocations.txt` — one path per line, relative to repo root, all under `core/src/main/scala/`.
  </action>
  <verify>
    <automated>test -s /tmp/05-01-relocations.txt &amp;&amp; test "$(wc -l &lt; /tmp/05-01-relocations.txt)" -ge 25</automated>
  </verify>
  <done>
- `/tmp/05-01-relocations.txt` exists with ≥25 paths.
- All paths under `core/src/main/scala/`.
- List includes (at minimum) every file in `<known_seed_files>`.
- No source file moved yet.
  </done>
</task>

<task type="auto">
  <name>Task 3: git mv every file in the relocation list and commit</name>
  <files>core/src/main/scala-2.13/** (NEW); core/src/main/scala/** (REMOVED)</files>
  <action>
1. Ensure target dirs exist:
   ```sh
   while read f; do mkdir -p "$(dirname "${f/scala\//scala-2.13/}")"; done &lt; /tmp/05-01-relocations.txt
   ```
2. Execute moves preserving history:
   ```sh
   while read f; do git mv "$f" "${f/scala\//scala-2.13/}"; done &lt; /tmp/05-01-relocations.txt
   ```
3. Sanity check the working tree:
   ```sh
   git status --porcelain | grep -c '^R' # rename count should equal line count of relocations.txt
   git ls-files core/src/main/scala/ | xargs -I{} grep -l '= macro ' {} 2>/dev/null # MUST be empty
   git ls-files core/src/main/scala-2.13/ | head -5 # should list relocated paths
   ```
4. Compile gate (regression guard for 2.13):
   ```sh
   sbt '++2.13.18 commons-core/compile'
   ```
   MUST exit 0. If RED: diagnose — most likely cause is a file relocated incorrectly (still has cross-version content), move it back with `git mv` reverse, re-run. NO `@nowarn` / `-Wconf` introductions allowed.
5. Scala 3 macros regression guard (Phase 3 protection):
   ```sh
   sbt '++3.8.2 commons-macros/compile'
   ```
   MUST exit 0. (Macros module is unaffected by core source moves but we re-verify Phase 3 integrity.)
6. scalafmt regression guard:
   ```sh
   sbt scalafmtCheckAll
   ```
   If RED on a relocated file (scalafmt may re-route through `scala213source3` dialect override now that the file lives under `scala-2.13/`), run `sbt scalafmtAll` and accept the reformat as part of the same commit. If `scalafmtAll` rewrites macro-def syntax in a way that breaks compile, scope-OK: add `// format: off` / `// format: on` around the offending macro def block per memory rule `feedback_format_off_macro_defs_ok.md`.
7. Stage and commit:
   ```sh
   git add -A
   git commit -m "refactor(core): relocate scala-2-only sources to scala-2.13/ for cross-build" \
     -m "Move ~N files containing macro defs / whitebox impls / RPC framework / deprecated APIs from" \
     -m "core/src/main/scala/ to core/src/main/scala-2.13/ so the Scala 3 compiler stops seeing" \
     -m "Scala-2-syntax-only sources. Prerequisite for the scala-3/ cherry-pick in the next change." \
     -m "" \
     -m "++2.13.18 commons-core/compile remains green; ++3.8.2 commons-macros/compile green;" \
     -m "scalafmtCheckAll green. Scala 3 commons-core/compile is still red — addressed next."
   ```
   Replace `N` with the actual count.
8. Final assertion:
   ```sh
   git log --oneline -1
   git diff --stat HEAD~1 | tail -3
   ```
   Expect "X files changed, Y insertions(+), Y deletions(-)" with insertions == deletions (pure rename) or close to it (if scalafmt reformatted).

NO GSD nomenclature in commit message. NO `.planning/` paths in diff.
  </action>
  <verify>
    <automated>git log --oneline -1 | grep -q 'refactor(core): relocate' &amp;&amp; ! git ls-files core/src/main/scala/ | xargs grep -l '= macro ' 2>/dev/null &amp;&amp; sbt '++2.13.18 commons-core/compile' &gt;/dev/null 2>&amp;1</automated>
  </verify>
  <done>
- All files in `/tmp/05-01-relocations.txt` now live under `core/src/main/scala-2.13/`.
- `git grep -l '= macro ' core/src/main/scala/` returns empty.
- ++2.13.18 commons-core/compile, ++3.8.2 commons-macros/compile, scalafmtCheckAll all GREEN.
- Single new commit on branch with message `refactor(core): relocate scala-2-only sources to scala-2.13/ for cross-build`.
- `.planning/` not in `git diff HEAD~1` output.
  </done>
</task>

</tasks>

<verification>
After Task 3:
- `git log --oneline 04-made-integration..HEAD` shows exactly 1 commit (the relocation), or 2 if a scalafmt-fixup was needed as a separate commit.
- `find core/src/main/scala -name '*.scala' | xargs grep -l '= macro '` returns nothing.
- `find core/src/main/scala-2.13 -name '*.scala' | wc -l` ≥ 25.
- `++2.13.18 commons-core/compile` exits 0 with no new warnings.
- `++3.8.2 commons-macros/compile` exits 0.
- `scalafmtCheckAll` exits 0.
- No `@nowarn` / `-Wconf` token added in this plan's diff: `git diff 04-made-integration..HEAD | grep -E '(@nowarn|-Wconf)'` empty.
</verification>

<success_criteria>
- Branch `05-core-scala-3-baseline-port` exists locally, based on `c3e54b16`.
- Scala-2-only sources relocated to `core/src/main/scala-2.13/`; history preserved (git detects renames).
- ++2.13.18 commons-core/compile green; ++3.8.2 commons-macros/compile green; scalafmtCheckAll green.
- ++3.8.2 commons-core/compile may still be red (acceptable — closed in Plan 05-02).
- No GSD nomenclature in commit message; `.planning/` not in diff; no `@nowarn` introductions.
</success_criteria>

<output>
After completion, create `.planning/phases/05-core-scala-3-baseline-port/05-01-branch-and-relocate-scala-2-only-SUMMARY.md` using the summary template. Record:
- Total files relocated (count + list).
- Whether any file was excluded from the move and why.
- ++3.8.2 commons-core/compile error count delta (before vs after the move).
- Any `// format: off` blocks added.
- Branch tip SHA.
</output>
