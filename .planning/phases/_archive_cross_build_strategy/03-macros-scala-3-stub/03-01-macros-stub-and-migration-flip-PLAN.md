---
phase: 03-macros-scala-3-stub
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - macros/src/main/scala-3/.gitkeep
  - MIGRATION.md
autonomous: true
commit_docs: false
requirements: [MACROS-01, DOC-02, WORKFLOW-01, WORKFLOW-04, WORKFLOW-05, QUALITY-01]
must_haves:
  truths:
    - "Branch `03-macros-scala-3-stub` exists locally, cut from `upstream/scala-3`"
    - "Directory `macros/src/main/scala-3/` exists on disk and is tracked in git"
    - "`sbt '++3 macros/compile'` exits 0 (empty/placeholder jar is acceptable)"
    - "`sbt '++3 core/compile'` exits 0 — downstream `dependsOn(macros)` resolves on Scala 3"
    - "`sbt '++2.13 jvm/compile'` still exits 0 — Scala 2.13 side untouched"
    - "MIGRATION.md `macros` row notes column reflects the stub landing (no longer 'next port')"
    - "No new `@nowarn` / `-Wconf` introduced (REQ QUALITY-01)"
    - "Branch carries 1 or 2 commits prefixed `build(macros):` and/or `docs(migration):`, no GSD nomenclature"
  artifacts:
    - path: "macros/src/main/scala-3/.gitkeep"
      provides: "Source directory anchor so sbt resolves `scala-3/` for the macros module on Scala 3 cross-build"
      contains: ""
    - path: "MIGRATION.md"
      provides: "`macros` row notes column updated; 3.x column remains `stub`"
      contains: "| macros |"
  key_links:
    - from: "build.sbt `lazy val macros` (crossScalaVersions := Seq(scala3Version, scala2Version))"
      to: "macros/src/main/scala-3/"
      via: "sbt `sourceDirsSettings` helper resolves `scala-3` source set lazily"
      pattern: "scala-3"
    - from: "build.sbt `lazy val core .dependsOn(macros)`"
      to: "macros_3.jar (empty or single-package-object)"
      via: "++3 core/compile resolves macros artifact for Scala 3"
      pattern: "dependsOn\\(macros\\)"
---

<objective>
Cut migration branch `03-macros-scala-3-stub` from `upstream/scala-3`, then add an empty `macros/src/main/scala-3/` source tree (anchored by `.gitkeep`) so the `macros` sbt module cross-builds under Scala 3. Confirm `++3 macros/compile` and `++3 core/compile` both succeed. Update the `macros` row of `MIGRATION.md` in the same PR per REQ DOC-02. If `.gitkeep` alone fails to produce a valid Scala 3 artifact, fall back to a single placeholder `package object macros` file.

Purpose: Satisfies REQ MACROS-01 — downstream `dependsOn(macros)` resolves on Scala 3 without inheriting whitebox impls. No macro reimplementation in this phase; the Scala 3 jar is intentionally empty (or holds a zero-export `package object`). Establishes the cross-build foothold for `core` (Phase 5+) and `cbor` (Phase 11) to consume on Scala 3.

Output: Local branch `03-macros-scala-3-stub` carrying 1–2 commits: (a) `build(macros): add empty scala-3 source tree for cross-build stub`, optionally (b) `docs(migration): mark macros scala-3 stub as landed`. Both compile gates green on Scala 3 and Scala 2.13.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/ROADMAP.md
@.planning/phases/03-macros-scala-3-stub/03-CONTEXT.md
@.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md
@build.sbt
</context>

<interfaces>
Relevant `build.sbt` declarations (current `master`; assume `upstream/scala-3` carries equivalent post-Phase-1 state):

```scala
val scala2Version = "2.13.18"
val scala3Version = "3.8.2"

// Cross-version source layout helper — already wired for all modules.
def mkSourceDirs(base: File, scalaBinary: String, conf: String): Seq[File] = Seq(
  base / "src" / conf / "scala",
  base / "src" / conf / s"scala-$scalaBinary",
  base / "src" / conf / "java",
)

def sourceDirsSettings(baseMapper: File => File) = Seq(
  Compile / unmanagedSourceDirectories ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "main"),
  Test / unmanagedSourceDirectories ++=
    mkSourceDirs(baseMapper(baseDirectory.value), scalaBinaryVersion.value, "test"),
)

// macros: already cross-built. Empty scala-3 source tree on this branch.
lazy val macros = project
  .settings(
    jvmCommonSettings,
    crossScalaVersions := Seq(scala3Version, scala2Version),
    scalaVersion := scala3Version,
    libraryDependencies ++= {
      if (scalaBinaryVersion.value == "2.13")
        Seq("org.scala-lang" % "scala-reflect" % scalaValue.value)  // 2.13 only
      else Seq.empty
    },
    mimaPreviousArtifacts := Set.empty,
  )

lazy val core = project
  .dependsOn(macros)
  .settings(
    jvmCommonSettings,
    crossScalaVersions := Seq(scala3Version, scala2Version),
    scalaVersion := scala3Version,
    sourceDirsSettings(_ / "jvm"),
    // ...
  )
```

Important: `macros` does NOT use `sourceDirsSettings(...)` directly. It inherits the default sbt layout, which already includes `src/main/scala-{binaryVersion}/` via `unmanagedSourceDirectories`. The `macros/src/main/scala-2.13/` tree exists on disk and is picked up automatically by sbt's standard `Compile / unmanagedSourceDirectories`. Creating `macros/src/main/scala-3/` makes the symmetric Scala 3 source set discoverable — no build-config edit needed.

Current macros source layout (verified at planning time):
```
macros/src/main/scala-2.13/        # exists; holds whitebox macro impls
macros/src/main/scala-3/           # does NOT exist; this plan creates it
```

MIGRATION.md `macros` row baseline (set in Phase 2):

```
| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty in the next port. |
```

Target after this phase lands (notes column updated; status tokens unchanged):

```
| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty. |
```

Note: the literal MIGRATION.md content depends on what was actually merged in Phase 2's PR. The task action greps for `^| macros |` and rewrites the notes column to drop "in the next port" — if the actual upstream wording differs, the executor MUST surface the discrepancy to the user rather than guess.

Fallback path if `.gitkeep` alone is insufficient: create `macros/src/main/scala-3/com/avsystem/commons/macros/package.scala` containing only:

```scala
package com.avsystem.commons

package object macros
```

Zero exported symbols, zero implementations. This guarantees sbt sees at least one Scala 3 source file and produces a valid (near-empty) jar.

Branch naming: `03-macros-scala-3-stub` (no GSD nomenclature, no `phase` keyword per REQ WORKFLOW-04; the `03-` numeric prefix does not match the forbidden `phase [0-9]` pattern).
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Cut branch `03-macros-scala-3-stub` from upstream/scala-3</name>
  <files>(none — git plumbing)</files>
  <read_first>
    - .planning/REQUIREMENTS.md §WORKFLOW-01 (branch off latest upstream/scala-3)
    - .planning/ROADMAP.md §"Per-PR Workflow" (steps 1–3)
  </read_first>
  <action>
    From project root, fetch upstream and cut a fresh branch off `upstream/scala-3`:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git fetch upstream
        git status --porcelain    # MUST be empty before branching; if dirty, surface to user and STOP
        git checkout -b 03-macros-scala-3-stub upstream/scala-3

    Rationale: REQ WORKFLOW-01 mandates branching off the latest `upstream/scala-3`. This phase assumes Phase 1 (build infra) and Phase 2 (MIGRATION.md skeleton) are MERGED on upstream — without Phase 1 the macros module would not be cross-built, and without Phase 2 there is no MIGRATION.md to flip. If `upstream/scala-3` does NOT show Phase 1+2 changes (no `crossScalaVersions` on macros; no `MIGRATION.md` at repo root), STOP and surface to the user.

    Sanity check after branch creation:

        git rev-parse --abbrev-ref HEAD                # MUST print `03-macros-scala-3-stub`
        [ "$(git rev-parse HEAD)" = "$(git rev-parse upstream/scala-3)" ] && echo OK
        test -f MIGRATION.md && echo "MIGRATION.md present (Phase 2 merged)" || echo "WARNING: MIGRATION.md missing; surface to user"
        grep -F 'crossScalaVersions := Seq(scala3Version, scala2Version)' build.sbt | head -3
        # ^ confirms Phase 1 substrate is present on the cut branch
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; git rev-parse --abbrev-ref HEAD | grep -Fxq '03-macros-scala-3-stub' &amp;&amp; [ "$(git rev-parse HEAD)" = "$(git rev-parse upstream/scala-3)" ] &amp;&amp; [ -z "$(git status --porcelain)" ]</automated>
  </verify>
  <acceptance_criteria>
    - `git rev-parse --abbrev-ref HEAD` prints exactly `03-macros-scala-3-stub`.
    - `git rev-parse HEAD` equals `git rev-parse upstream/scala-3` (branch base unchanged — no commits yet).
    - `git status --porcelain` is empty.
    - Branch name contains no GSD nomenclature: `echo 03-macros-scala-3-stub | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - Phase 1 substrate present on the cut branch: `grep -Fq 'crossScalaVersions := Seq(scala3Version, scala2Version)' build.sbt` exits 0.
  </acceptance_criteria>
  <done>Local branch `03-macros-scala-3-stub` cut from `upstream/scala-3`; working tree clean; no commits added yet; Phase 1 substrate confirmed present.</done>
</task>

<task type="auto">
  <name>Task 2: Create `macros/src/main/scala-3/` (try .gitkeep first; fall back to placeholder package object); verify `++3 macros/compile` and `++3 core/compile`; commit as `build(macros): ...`</name>
  <files>
    - macros/src/main/scala-3/.gitkeep
    - (fallback) macros/src/main/scala-3/com/avsystem/commons/macros/package.scala
  </files>
  <read_first>
    - .planning/phases/03-macros-scala-3-stub/03-CONTEXT.md §"Implementation Decisions" → "Stub strategy" + "Source-dir creation"
    - .planning/phases/03-macros-scala-3-stub/03-CONTEXT.md §"Claude's Discretion"
    - build.sbt lines 285–301 — `lazy val macros` declaration (confirm crossScalaVersions on Scala 3)
    - macros/src/main/scala-2.13/ — confirm whitebox sources untouched by listing top-level entries
  </read_first>
  <action>
    Step 1: Confirm the source dir does NOT already exist (Phase 3 should be additive):

        cd /Users/bkozak/IdeaProjects/scala-commons3
        test ! -d macros/src/main/scala-3 || { echo "WARNING: macros/src/main/scala-3 already exists; surface to user"; exit 2; }

    Step 2: Create the directory anchored with `.gitkeep` (preferred per CONTEXT.md):

        mkdir -p macros/src/main/scala-3
        : > macros/src/main/scala-3/.gitkeep    # zero-byte file
        ls -la macros/src/main/scala-3

    Rationale: git does not track empty directories. A zero-byte `.gitkeep` is the standard idiom and lands the directory in the PR diff. CONTEXT.md §"Source-dir creation" green-lights `.gitkeep` for this phase (Phase 1 did NOT use `.gitkeep` because Phase 1 created no `scala-3` dirs — silence on later phases means the convention is open).

    Step 3: Compile Scala 3 macros module:

        sbt -batch '++3 macros/compile'

    Expected outcome: exit 0 with a `[success]` line. sbt produces a near-empty `macros_3.jar` (only the standard `META-INF/MANIFEST.MF`). Note: `++3` is shorthand for `++3.x.y` resolved to `scala3Version = "3.8.2"` per build.sbt.

    If `sbt '++3 macros/compile'` FAILS with errors like `no sources`, `empty source list`, or jar packaging errors, switch to FALLBACK:

        # FALLBACK: create a single placeholder package object
        rm macros/src/main/scala-3/.gitkeep
        mkdir -p macros/src/main/scala-3/com/avsystem/commons/macros
        cat > macros/src/main/scala-3/com/avsystem/commons/macros/package.scala <<'EOF'
package com.avsystem.commons

package object macros
EOF
        sbt -batch '++3 macros/compile'    # retry

    The fallback file declares zero symbols. Do NOT add `inline`/`given`/`quotes` imports. Do NOT mirror the file paths from `macros/src/main/scala-2.13/`. CONTEXT.md §"Whitebox / Scala 2 macro safety" is explicit.

    Step 4: Downstream sanity — compile `core` on Scala 3 (proves `dependsOn(macros)` resolves):

        sbt -batch '++3 core/compile'

    Expected: exit 0. `core` already has Scala 3 sources (`core/src/main/scala-3/`) and dependsOn(macros) — this exercises the cross-version artifact resolution.

    Step 5: Scala 2.13 sanity — ensure the 2.13 whitebox macros still build (no regression):

        sbt -batch '++2.13 macros/compile' '++2.13 core/compile'

    Expected: exit 0. The `macros/src/main/scala-2.13/` tree is untouched; this is a smoke test.

    Step 6: Run scalafmt check (in case the fallback `package.scala` was added):

        sbt -batch scalafmtCheckAll

    If it fails on the new file, run `sbt scalafmtAll` and re-stage. The placeholder is two lines; format should be a no-op, but check anyway.

    Step 7: Stage and commit. EXPLICITLY stage to avoid `.planning/` leakage (REQ WORKFLOW-05):

        cd /Users/bkozak/IdeaProjects/scala-commons3
        # Path varies by branch (.gitkeep vs fallback). Stage whichever exists:
        if [ -f macros/src/main/scala-3/.gitkeep ]; then
          git add macros/src/main/scala-3/.gitkeep
        elif [ -f macros/src/main/scala-3/com/avsystem/commons/macros/package.scala ]; then
          git add macros/src/main/scala-3/com/avsystem/commons/macros/package.scala
        else
          echo "ERROR: neither .gitkeep nor fallback package.scala present"; exit 3
        fi
        git status --porcelain    # MUST show only the staged macros/scala-3/ entry; surface extras to user
        git commit -m 'build(macros): add empty scala-3 source tree for cross-build stub'

    Commit message rules: no GSD nomenclature (no `gsd`, no `phase N`, no `plan-phase`); prefix `build(macros):` is upstream-conventional. The message must NOT mention `.planning/` or internal artifacts.

    Final no-leakage check:

        git log -1 --name-only --format=%H | grep -F '.planning/' && { echo 'ERROR: .planning leak'; exit 4; } || true
        git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' && { echo 'ERROR: GSD nomenclature'; exit 5; } || true
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; test -d macros/src/main/scala-3 &amp;&amp; { test -f macros/src/main/scala-3/.gitkeep || test -f macros/src/main/scala-3/com/avsystem/commons/macros/package.scala; } &amp;&amp; sbt -batch '++3 macros/compile' &amp;&amp; sbt -batch '++3 core/compile' &amp;&amp; sbt -batch '++2.13 macros/compile' &amp;&amp; git log -1 --format=%s | grep -E '^build\(macros\):' &amp;&amp; ! git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; ! git show --stat HEAD --name-only --pretty= | grep -F '.planning/' &amp;&amp; git show --stat HEAD --name-only --pretty= | grep -q '^macros/src/main/scala-3/'</automated>
  </verify>
  <acceptance_criteria>
    - Directory exists: `test -d /Users/bkozak/IdeaProjects/scala-commons3/macros/src/main/scala-3` exits 0.
    - At least one of `.gitkeep` OR `com/avsystem/commons/macros/package.scala` exists under it.
    - `sbt -batch '++3 macros/compile'` exits 0 (Scala 3 cross-build foothold established).
    - `sbt -batch '++3 core/compile'` exits 0 (downstream `dependsOn(macros)` resolves on Scala 3 — CONTEXT.md "Downstream sanity").
    - `sbt -batch '++2.13 macros/compile'` exits 0 (2.13 whitebox impls untouched — no regression).
    - No new `@nowarn` / `-Wconf` introduced (REQ QUALITY-01): `git diff upstream/scala-3..HEAD | grep -E '^\+.*(@nowarn|-Wconf)'` exits 1.
    - Commit prefix is `build(macros):` — `git log -1 --format=%s | grep -E '^build\(macros\):'` exits 0.
    - Commit message contains no GSD nomenclature — `git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - Commit does NOT touch `.planning/` — `git show --stat HEAD --name-only --pretty= | grep -F '.planning/'` exits 1.
    - Commit DOES touch the new scala-3 path — `git show --stat HEAD --name-only --pretty= | grep -q '^macros/src/main/scala-3/'` exits 0.
    - `macros/src/main/scala-2.13/` directory is unchanged: `git diff upstream/scala-3..HEAD -- macros/src/main/scala-2.13/` prints empty.
  </acceptance_criteria>
  <done>`macros/src/main/scala-3/` exists (anchored by `.gitkeep` or placeholder `package.scala`); `++3 macros/compile`, `++3 core/compile`, `++2.13 macros/compile` all exit 0; one `build(macros):` commit on the branch with no `.planning/` leakage and no GSD nomenclature.</done>
</task>

<task type="auto">
  <name>Task 3: Flip MIGRATION.md `macros` row notes column; commit as `docs(migration): ...`</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (current state on the branch, post-Task-2) — locate the `^| macros |` row
    - .planning/phases/03-macros-scala-3-stub/03-CONTEXT.md §"Documentation" (target wording, ≤80 chars)
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-01-skeleton-and-status-table-PLAN.md §"Per-module status table" (baseline wording from Phase 2)
  </read_first>
  <action>
    Step 1: Verify MIGRATION.md exists at repo root (Phase 2 prerequisite):

        cd /Users/bkozak/IdeaProjects/scala-commons3
        test -f MIGRATION.md || { echo "ERROR: MIGRATION.md missing — Phase 2 not merged upstream"; exit 2; }

    Step 2: Locate the current `| macros |` row and verify it is in the expected baseline shape:

        grep -nE '^\| macros \|' MIGRATION.md

    Expected baseline (from Phase 2 Plan 01):

        | macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty in the next port. |

    If the row is different (different wording, different column count, missing), STOP and surface to the user with the actual line. Do NOT guess-replace.

    Step 3: Rewrite the `macros` row notes column to drop "in the next port" (the stub HAS landed in this PR — no longer "next"). Use Edit tool with exact strings:

        old_string: | macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty in the next port. |
        new_string: | macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty (empty scala-3/ source tree). |

    Rationale (CONTEXT.md §"Documentation"): notes column should read "Empty scala-3 dir; whitebox impls remain 2.13-only" (≤80 chars). The chosen wording above is 60 chars in the notes cell and stays factual / impersonal (no `we`, no `✓`).

    Tone rules (locked by Phase 2 CONTEXT.md, inherited):
    - No emoji. No `✓`.
    - Impersonal voice (no `we`/`our`).
    - No GSD vocabulary (no `gsd`, no `wave`, no `phase N`, no `RESEARCH.md`/`PLAN.md`/`CONTEXT.md`, no `.planning/`).

    Step 4: Verify the change is minimal and ONLY touches the macros row:

        git diff -- MIGRATION.md | head -40
        # Expect: 1 line removed, 1 line added, both `| macros |` rows. No other diffs.

    Step 5: Sanity — the 13-row table count stays at 13:

        awk '/^## Per-module status/,/^## 2/' MIGRATION.md | grep -cE '^\| (macros|made|core|hocon|mongo|mongo-js|core-js|benchmark3|jetty|analyzer|spring|RPC|cbor) '
        # MUST print 13

    Step 6: Stage and commit:

        git add MIGRATION.md
        git status --porcelain    # MUST show only ` M MIGRATION.md`
        git commit -m 'docs(migration): mark macros scala-3 stub as landed'

    Branch HEAD count after this task: 2 commits ahead of upstream/scala-3 (build commit from Task 2, docs commit from this task).
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; test -f MIGRATION.md &amp;&amp; grep -E '^\| macros \| cross \| stub \| n/a \| n/a \| Whitebox 2.13 macros; Scala 3 jar empty \(empty scala-3/ source tree\)\. \|$' MIGRATION.md &amp;&amp; [ "$(awk '/^## Per-module status/,/^## 2/' MIGRATION.md | grep -cE '^\| (macros|made|core|hocon|mongo|mongo-js|core-js|benchmark3|jetty|analyzer|spring|RPC|cbor) ')" -eq 13 ] &amp;&amp; ! grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md &amp;&amp; ! grep -F '.planning/' MIGRATION.md &amp;&amp; git log -1 --format=%s | grep -E '^docs\(migration\):' &amp;&amp; ! git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; [ "$(git show --stat HEAD --name-only --pretty= | grep -v '^$' | sort -u)" = "MIGRATION.md" ] &amp;&amp; [ "$(git log upstream/scala-3..HEAD --oneline | wc -l | tr -d ' ')" = "2" ]</automated>
  </verify>
  <acceptance_criteria>
    - MIGRATION.md contains the updated macros row exactly: `| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty (empty scala-3/ source tree). |`
    - Per-module status table still has 13 rows.
    - No GSD vocabulary anywhere in MIGRATION.md (`grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md` exits 1).
    - No `.planning/` references (`grep -F '.planning/' MIGRATION.md` exits 1).
    - Commit prefix is `docs(migration):` (`git log -1 --format=%s | grep -E '^docs\(migration\):'` exits 0).
    - Commit message contains no GSD nomenclature.
    - Commit touches ONLY `MIGRATION.md`.
    - Branch HEAD is exactly 2 commits ahead of `upstream/scala-3`.
  </acceptance_criteria>
  <done>MIGRATION.md `macros` row notes column updated to reflect the landed stub; one `docs(migration):` commit added; branch now carries exactly 2 commits (build + docs) on top of `upstream/scala-3`.</done>
</task>

</tasks>

<verification>
After Plan 01 completes:

1. `cd /Users/bkozak/IdeaProjects/scala-commons3 && git rev-parse --abbrev-ref HEAD` prints `03-macros-scala-3-stub`.
2. `git log upstream/scala-3..HEAD --oneline | wc -l` prints `2`.
3. `git diff upstream/scala-3..HEAD --name-only | sort -u` prints two paths: a `macros/src/main/scala-3/...` entry and `MIGRATION.md`.
4. `sbt -batch '++3 macros/compile'` exits 0.
5. `sbt -batch '++3 core/compile'` exits 0.
6. `sbt -batch '++2.13 macros/compile'` exits 0.
7. No `.planning/` paths in any commit (REQ WORKFLOW-05): `git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0`.
8. No GSD nomenclature in any commit message (REQ WORKFLOW-04): `git log upstream/scala-3..HEAD --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
9. No new `@nowarn`/`-Wconf` introduced (REQ QUALITY-01): `git diff upstream/scala-3..HEAD | grep -E '^\+.*(@nowarn|-Wconf)'` exits 1.
</verification>

<success_criteria>
- Branch `03-macros-scala-3-stub` cut from `upstream/scala-3` (REQ WORKFLOW-01).
- `macros/src/main/scala-3/` exists and is tracked (REQ MACROS-01 — Scala 3 stub source tree present).
- `++3 macros/compile` and `++3 core/compile` both green (REQ MACROS-01 — `dependsOn(macros)` resolves on Scala 3).
- `++2.13 macros/compile` still green (no regression on Scala 2.13 whitebox macros).
- MIGRATION.md `macros` row updated in same PR (REQ DOC-02).
- No GSD nomenclature in commits (REQ WORKFLOW-04); no `.planning/` leakage (REQ WORKFLOW-05); no new `@nowarn`/`-Wconf` (REQ QUALITY-01).
- Branch carries exactly 2 commits: one `build(macros):`, one `docs(migration):`.
</success_criteria>

<output>
After completion, create `.planning/phases/03-macros-scala-3-stub/03-01-SUMMARY.md` capturing:
- Branch HEAD SHA on `03-macros-scala-3-stub`
- Whether `.gitkeep` succeeded or fallback `package.scala` was used (record which)
- Output of `git log upstream/scala-3..HEAD --oneline` (expect 2 commits)
- Output of `git diff upstream/scala-3..HEAD --stat`
- `sbt '++3 macros/compile'` exit code + last `[success]` line
- `sbt '++3 core/compile'` exit code + last `[success]` line
- `sbt '++2.13 macros/compile'` exit code + last `[success]` line
- Confirmation that no `.planning/` paths and no GSD nomenclature appear in branch history
- Exact new MIGRATION.md macros row contents
</output>
</content>
</invoke>