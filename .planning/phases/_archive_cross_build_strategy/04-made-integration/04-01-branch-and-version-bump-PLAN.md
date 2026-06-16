---
phase: 04-made-integration
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - build.sbt
autonomous: true
requirements:
  - INFRA-06
  - WORKFLOW-01
must_haves:
  truths:
    - "Working branch `04-made-integration` exists, cut from `upstream/scala-3` HEAD"
    - "`++2.13 core/compile` is green on the freshly-cut branch BEFORE any source ports (Wave 0 preflight per VALIDATION.md)"
    - "`build.sbt` declares `val madeVersion = \"0.1.0\"` (no `-SNAPSHOT`)"
    - "sbt loads the build successfully after the version bump"
  artifacts:
    - path: "build.sbt"
      provides: "Pinned `madeVersion = \"0.1.0\"` constant"
      contains: "val madeVersion = \"0.1.0\""
  key_links:
    - from: "build.sbt line 27 (madeVersion constant)"
      to: "build.sbt lines 326–331 (core libraryDependencies) and lines 349–353 (core-js libraryDependencies)"
      via: "string interpolation of `madeVersion` value"
      pattern: "made\" % madeVersion"
---

<objective>
Cut working branch `04-made-integration` off the latest `upstream/scala-3`, confirm the baseline (`++2.13 core/compile`) is green BEFORE touching anything, then flip `madeVersion` from `"0.1.1-SNAPSHOT"` to `"0.1.0"` in `build.sbt`.

Purpose: Single-line build constant change is its own commit so the source-port wave (Plan 02) can be reviewed independently. The preflight gate enforces that any subsequent compile failure is attributable to the port, not to a pre-existing baseline issue.

Output: A clean working branch with one commit (`build:` prefix) bumping the made dep to the published 0.1.0 release. No source files touched.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/phases/04-made-integration/04-CONTEXT.md
@.planning/phases/04-made-integration/04-RESEARCH.md
@.planning/phases/04-made-integration/04-VALIDATION.md
@.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md
@build.sbt

<interfaces>
<!-- The only relevant section of build.sbt for this plan. Extracted from current branch. -->

`build.sbt` line 27 (current fork-master value, MUST CHANGE):
```scala
val madeVersion = "0.1.1-SNAPSHOT"
```

`build.sbt` lines 326–331 (core — already correct, MUST NOT TOUCH):
```scala
libraryDependencies ++= {
  // `made` is Scala 3 only.
  if (scalaBinaryVersion.value == "3")
    Seq("io.github.halotukozak" %% "made" % madeVersion)
  else Seq.empty
},
```

`build.sbt` lines 349–353 (core-js — already correct, MUST NOT TOUCH):
```scala
libraryDependencies ++= {
  if (scalaBinaryVersion.value == "3")
    Seq("io.github.halotukozak" %% "made" % madeVersion)
  else Seq.empty
},
```

Verified via `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` — published, signature-compatible, no resolver needed.
</interfaces>

**Memory rules to honor:**
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_dont_port_deprecated.md` (relevant in Plan 02, not here).
- `~/.claude/CLAUDE.md` — never push to master/main, never merge PRs/MRs (gates live in Plan 04).
</context>

<tasks>

<task type="auto">
  <name>Task 1: Wave 0 preflight — fetch upstream, cut branch, confirm baseline green</name>
  <files>(no files modified — git/branch state only)</files>
  <action>
Per VALIDATION.md "Wave 0 Requirements" — confirm baseline BEFORE any port.

1. Fetch upstream:
   ```sh
   git fetch upstream
   ```

2. Confirm `upstream/scala-3` ref exists:
   ```sh
   git rev-parse upstream/scala-3
   ```
   Must print a SHA. If it errors, stop and surface to user.

3. Stash or verify clean working tree on current branch (`master`):
   ```sh
   git status --porcelain
   ```
   Must be empty. If not empty, stop — the user has uncommitted work. Do NOT auto-stash.

4. Cut the working branch off `upstream/scala-3`:
   ```sh
   git checkout -b 04-made-integration upstream/scala-3
   ```
   (Per CLAUDE.md global rule: never push to master/main — we operate on a NEW branch, never push to `master`.)

5. Wave 0 baseline gate — `++2.13 core/compile` MUST be green BEFORE any source/build edits:
   ```sh
   sbt -batch '++2.13 core/compile'
   ```
   Expected exit code 0. If non-zero, the upstream/scala-3 baseline itself is broken — STOP and surface to user; do NOT proceed.

6. Sanity-check that the literal `madeVersion = "0.1.1-SNAPSHOT"` is NOT already in upstream/scala-3 (it shouldn't be — upstream never had it):
   ```sh
   grep -n "madeVersion" build.sbt
   ```
   The result depends on upstream/scala-3 state. Record what `madeVersion` is currently set to on this branch — Task 2 changes it to `"0.1.0"` regardless. If upstream/scala-3 has NO `madeVersion` declaration at all, Task 2 must ADD the line at the same logical position (top-of-file version constants block) AND wire the conditional `libraryDependencies` blocks in `core` and `core-js` per the `<interfaces>` snippets above.

DO NOT proceed if any step fails. Surface the failure to the user.
  </action>
  <verify>
    <automated>git rev-parse --abbrev-ref HEAD | grep -q '^04-made-integration$' && sbt -batch '++2.13 core/compile'</automated>
  </verify>
  <done>Branch `04-made-integration` is checked out, cut off `upstream/scala-3`; `sbt -batch '++2.13 core/compile'` exits 0; current `madeVersion` state in `build.sbt` is recorded in the task summary.</done>
</task>

<task type="auto">
  <name>Task 2: Bump madeVersion to "0.1.0" (or add it if missing on upstream/scala-3)</name>
  <files>build.sbt</files>
  <action>
**Single-line edit (most likely):** Replace the existing `madeVersion` declaration with `val madeVersion = "0.1.0"`.

Use the Edit tool on `build.sbt`:
- Find: `val madeVersion = "0.1.1-SNAPSHOT"` (or whatever Task 1 recorded as the current value).
- Replace with: `val madeVersion = "0.1.0"`

**Alternative branch — if upstream/scala-3 has NO `madeVersion` declaration** (Task 1 grep returned empty):
- ADD the line `val madeVersion = "0.1.0"` immediately after the other version constants (next to `val scala3Version = "3.8.2"` per fork-master pattern at lines 27–30).
- ADD the conditional `libraryDependencies` block to the `core` project settings (mirror lines 326–331 from fork master verbatim — already shown in `<interfaces>`).
- ADD the same conditional block to the `core-js` project settings (mirror lines 349–353).

**MUST NOT** add a snapshot resolver. **MUST NOT** add `Resolver.sonatypeOssRepos("snapshots")` anywhere.

**Why 0.1.0 (not snapshot):** Per RESEARCH.md "Pitfall 2: Snapshot resolver leakage" — CI lacks the snapshot resolver some local checkouts have; 0.1.0 is the published Sonatype Central release verified via cellar.

After the edit:
1. Verify no SNAPSHOT remains anywhere:
   ```sh
   ! grep -RnE '\-SNAPSHOT' build.sbt project/
   ```
2. Verify exactly one `madeVersion = "0.1.0"` declaration:
   ```sh
   grep -n 'madeVersion = "0.1.0"' build.sbt
   ```
3. Verify build still loads:
   ```sh
   sbt -batch 'show jvm/version'
   ```
   Exit 0.

Commit (single commit, `build:` prefix per WORKFLOW-04 — NO GSD nomenclature):
```sh
git add build.sbt
git commit -m "build(made): pin madeVersion to 0.1.0"
```
  </action>
  <verify>
    <automated>grep -q 'val madeVersion = "0.1.0"' build.sbt && ! grep -RqE '\-SNAPSHOT' build.sbt project/ && sbt -batch 'show jvm/version'</automated>
  </verify>
  <done>`build.sbt` has `val madeVersion = "0.1.0"` exactly once, no SNAPSHOT anywhere, `sbt 'show jvm/version'` exits 0, one commit made on `04-made-integration` branch.</done>
</task>

</tasks>

<verification>
End-of-plan gate (also re-run at top of Plan 03):

```sh
git rev-parse --abbrev-ref HEAD     # → 04-made-integration
grep -n 'madeVersion' build.sbt      # → exactly one match: val madeVersion = "0.1.0"
! grep -RqE '\-SNAPSHOT' build.sbt project/
sbt -batch 'show jvm/version'        # exit 0
git log upstream/scala-3..HEAD --oneline  # → exactly one commit, build: prefix
```
</verification>

<success_criteria>
- [ ] Branch `04-made-integration` checked out off `upstream/scala-3`
- [ ] `sbt -batch '++2.13 core/compile'` was green BEFORE Task 2 ran (Wave 0 preflight)
- [ ] `build.sbt` declares `val madeVersion = "0.1.0"`; no SNAPSHOT in `build.sbt` or `project/`
- [ ] No snapshot resolver added
- [ ] sbt loads the build (`show jvm/version` exits 0)
- [ ] Single commit with `build:` prefix (no GSD nomenclature)
</success_criteria>

<output>
After completion, create `.planning/phases/04-made-integration/04-01-SUMMARY.md` documenting:
- Final state of `madeVersion` line in `build.sbt`
- Whether Task 2 was an edit (most likely) or an add+wire (only if upstream/scala-3 had no `madeVersion`)
- Confirmation that Wave 0 preflight (`++2.13 core/compile`) was green BEFORE edit
- Commit SHA of the build-bump commit
</output>
</content>
