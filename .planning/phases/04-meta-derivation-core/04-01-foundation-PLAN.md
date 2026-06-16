---
phase: 04-meta-derivation-core
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala
  - core/src/main/scala/com/avsystem/commons/meta/Fallback.scala
  - core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala
  - core/src/main/scala/com/avsystem/commons/meta/metadata.scala
  - MIGRATION.md
autonomous: true
requirements:
  - META-CORE-03
  - META-CORE-05
  - META-CORE-06
  - QUALITY-01
  - PR-01
  - PR-02
  - PR-03
  - WORKFLOW-01
  - WORKFLOW-02
  - WORKFLOW-03
  - WORKFLOW-04
  - WORKFLOW-05
  - DOC-02
must_haves:
  truths:
    - "AllowDerivation.scala exists with sealed trait + create[T] factory + AllowRecursiveDerivation companion (verbatim from fork)"
    - "Fallback.scala matches fork shape (no-op if already identical)"
    - "OptionLike.scala carries fork's given Default[O] bridge AND preserves our BaseOptionLike @bincompat shim (per Pitfall 3)"
    - "metadata.scala carries `import made.*` + `import made.annotation.*` + `import GenCodec.given` + @transparent/@name annotations from fork BUT @name(\"dupa\") debug artifact is STRIPPED"
    - "sbt commons-core/compile exits 0"
    - "sbt scalafmtCheckAll exits 0"
    - "No new @nowarn/-Wconf vs upstream/scala-3"
    - "Wave-0 named-tuple probe documented (verified scala.NamedTuple.AnyNamedTuple/DropNames usable on 3.8.2 without experimental flag)"
    - "MIGRATION.md updated: §3 OptionLike preservation note + metadata.scala @name(\"dupa\") strip note"
    - "Branch 04-01-foundation pushed; draft PR #N opened against upstream/scala-3 with [Scala 3] prefix + milestone 1 + body metadata block (slice 4.1, base = upstream/scala-3, depends on = none — base of stack)"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala"
      provides: "sealed trait AllowDerivation[T] + create[T] factory + AllowRecursiveDerivation object"
      contains: "sealed trait AllowDerivation"
    - path: "core/src/main/scala/com/avsystem/commons/meta/Fallback.scala"
      provides: "Fallback case class wrapper"
      contains: "case class Fallback"
    - path: "core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala"
      provides: "OptionLike typeclass + made.Default bridge"
      contains: "made.Default"
    - path: "core/src/main/scala/com/avsystem/commons/meta/metadata.scala"
      provides: "TypedMetadata + ParamFlags/MethodFlags/TypeFlags ADTs"
      contains: "import made"
    - path: "MIGRATION.md"
      provides: "§3 entries for OptionLike preservation + metadata.scala debug-artifact strip"
      contains: "OptionLike"
  key_links:
    - from: "core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala"
      to: "made.Default"
      via: "given … => made.Default[O] = () => optionLike.none"
      pattern: "made\\.Default"
    - from: "core/src/main/scala/com/avsystem/commons/meta/metadata.scala"
      to: "com.avsystem.commons.serialization.GenCodec.given"
      via: "import com.avsystem.commons.serialization.GenCodec.given"
      pattern: "GenCodec\\.given"
    - from: "core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala"
      to: "consumer slice 4.2 (MacroInstances.materializeInstances)"
      via: "AllowDerivation.create"
      pattern: "AllowDerivation\\.create"
---

<objective>
Slice 4.1 — Foundation. Port the leaf files of the meta/ derivation DAG from fork verbatim:
`AllowDerivation` (new file), `Fallback` (verify identical), `OptionLike` (reconcile — keep our BaseOptionLike shim,
add fork's made.Default bridge), `metadata.scala` (fork shape with @name("dupa") debug artifact stripped).

Purpose: Establish the leaves of the meta DAG so slices 4.2–4.5 can stack on top. No macros in this slice — pure ADT +
typeclass plumbing. Real macro work begins in slice 4.2.

Output: 4 source files matching fork shape (minus our 1 preservation + 1 debug strip), MIGRATION.md §3 entries,
branch `04-01-foundation` pushed, draft PR opened against `upstream/scala-3` as the base of the Phase 4 stack.

Validation gates: see `04-VALIDATION.md` per-slice section 4.1.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/ROADMAP.md
@.planning/REQUIREMENTS.md
@.planning/phases/04-meta-derivation-core/04-CONTEXT.md
@.planning/phases/04-meta-derivation-core/04-RESEARCH.md
@.planning/phases/04-meta-derivation-core/04-VALIDATION.md
@MIGRATION.md
@core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala
@core/src/main/scala/com/avsystem/commons/meta/Fallback.scala
@core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala
@core/src/main/scala/com/avsystem/commons/meta/metadata.scala

<interfaces>
<!-- Fork canonical sources — executor MUST git show these before each file port. -->
<!-- Pattern: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/<file>` -->

Fork files for this slice:
- origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AllowDerivation.scala (new — copy verbatim)
- origin/master:core/src/main/scala-3/com/avsystem/commons/meta/Fallback.scala (verify identical)
- origin/master:core/src/main/scala-3/com/avsystem/commons/meta/OptionLike.scala (reconcile — see RESEARCH Pitfall 3)
- origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metadata.scala (verbatim minus @name("dupa") strip — Pitfall 4)

Verbatim from RESEARCH §"Code Examples":

```scala
// AllowDerivation.scala (fork verbatim — 7 LOC)
package com.avsystem.commons
package meta

sealed trait AllowDerivation[T]
object AllowDerivation {
  private val reusable = new AllowDerivation[Any] {}
  def create[T]: AllowDerivation[T] = reusable.asInstanceOf[AllowDerivation[T]]
}

object AllowRecursiveDerivation
```

OptionLike fork-added line (keep our BaseOptionLike shim, add this on top):
```scala
given [O] => (optionLike: OptionLike[O]) => made.Default[O] = () => optionLike.none
```

metadata.scala fork imports (must be present after port):
```scala
import made.*
import made.annotation.*
import com.avsystem.commons.serialization.{GenCodec, HasGenCodec}
import com.avsystem.commons.serialization.GenCodec.given
```
</interfaces>
</context>

<tasks>

<task type="auto">
  <name>Task 1: Wave-0 — scala.NamedTuple stability probe + GenCodec.given existence check + cellar made.* preflight (records /tmp/phase4-wave0-probes.txt)</name>
  <read_first>
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Open Questions" #2, §"Common Pitfalls" #1, #7)
    - .planning/phases/04-meta-derivation-core/04-VALIDATION.md (Wave 0 Requirements)
    - core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala (grep for `given ` exports)
  </read_first>
  <action>
    Three sub-probes — block slice 4.1 if any fail. **Write all outcomes to `/tmp/phase4-wave0-probes.txt`**
    so the `<verify>` step asserts file contents (not an echo no-op).

    Start fresh:
    ```bash
    rm -f /tmp/phase4-wave0-probes.txt
    touch /tmp/phase4-wave0-probes.txt
    ```

    1. Named-tuple probe (Pitfall 1):
       ```bash
       cat > /tmp/named-tuple-probe.scala <<'EOF'
       import scala.NamedTuple.{AnyNamedTuple, DropNames}
       object Probe { type X[T <: AnyNamedTuple] = DropNames[T] }
       EOF
       cp /tmp/named-tuple-probe.scala core/src/main/scala/_NamedTupleProbe.scala
       if sbt commons-core/compile 2>&1 | tail -20 | grep -q '\[success\]'; then
         echo "NAMEDTUPLE_PROBE=OK (3.8.2 stable, no experimental flag)" >> /tmp/phase4-wave0-probes.txt
       else
         echo "NAMEDTUPLE_PROBE=FAIL — add -language:experimental.namedTuples to scalacOptions" >> /tmp/phase4-wave0-probes.txt
         rm -f core/src/main/scala/_NamedTupleProbe.scala
         exit 1
       fi
       rm -f core/src/main/scala/_NamedTupleProbe.scala
       ```

    2. GenCodec.given existence (Pitfall 7):
       ```bash
       COUNT=$(git grep -nE '^\s*given ' core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala | wc -l | tr -d ' ')
       # ANY count is acceptable — fork ships the wildcard import even at count=0 (silent no-op import is legal)
       echo "GENCODEC_GIVEN_PROBE=OK (count=$COUNT — wildcard import is silent no-op when 0, picks up givens when >0)" >> /tmp/phase4-wave0-probes.txt
       ```

    3. Cellar `made.*` preflight:
       ```bash
       ok=1
       cellar get-external io.github.halotukozak:made_3:0.1.1 made.annotation.transparent > /tmp/cellar-1.txt 2>&1 || ok=0
       cellar get-external io.github.halotukozak:made_3:0.1.1 made.annotation.name > /tmp/cellar-2.txt 2>&1 || ok=0
       cellar get-external io.github.halotukozak:made_3:0.1.1 made.Default > /tmp/cellar-3.txt 2>&1 || ok=0
       if [ "$ok" = "1" ]; then
         echo "CELLAR_MADE_PROBE=OK (made.annotation.transparent + made.annotation.name + made.Default all resolved)" >> /tmp/phase4-wave0-probes.txt
       else
         echo "CELLAR_MADE_PROBE=FAIL — see /tmp/cellar-{1,2,3}.txt" >> /tmp/phase4-wave0-probes.txt
         exit 1
       fi
       ```

    NO COMMIT in this task — `/tmp/phase4-wave0-probes.txt` is referenced in Task 2 commit message body.
  </action>
  <verify>
    <automated>test -f /tmp/phase4-wave0-probes.txt && grep -q "NAMEDTUPLE_PROBE=OK" /tmp/phase4-wave0-probes.txt && grep -q "GENCODEC_GIVEN_PROBE=OK" /tmp/phase4-wave0-probes.txt && grep -q "CELLAR_MADE_PROBE=OK" /tmp/phase4-wave0-probes.txt</automated>
  </verify>
  <acceptance_criteria>
    - `/tmp/phase4-wave0-probes.txt` exists and contains all three `*_PROBE=OK` markers
    - Named-tuple probe passed (no experimental flag needed) — if failed, slice 4.1 halts before Task 2
    - GenCodec.given count recorded
    - All 3 `cellar get-external` calls returned signatures
  </acceptance_criteria>
  <done>
    `/tmp/phase4-wave0-probes.txt` contains three `*_PROBE=OK` markers; no source files modified; ready to proceed to Task 2.
  </done>
</task>

<task type="auto">
  <name>Task 2: Cut branch 04-01-foundation off upstream/scala-3 tip; port AllowDerivation + Fallback</name>
  <read_first>
    - /tmp/phase4-wave0-probes.txt (verify Wave-0 probe outcomes — paste into commit body)
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AllowDerivation.scala
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/Fallback.scala
    - core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala (current state — likely doesn't exist)
    - core/src/main/scala/com/avsystem/commons/meta/Fallback.scala (current state)
  </read_first>
  <action>
    1. Verify branch base:
       ```bash
       git fetch upstream
       git checkout -b 04-01-foundation upstream/scala-3
       ```
       (If branch exists from prior attempt, `git checkout 04-01-foundation` and verify HEAD matches upstream/scala-3 tip.)

    2. Port AllowDerivation.scala (verbatim from fork):
       ```bash
       git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AllowDerivation.scala \
         > core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala
       ```

    3. Verify Fallback.scala — diff our copy vs fork:
       ```bash
       diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/Fallback.scala) \
            core/src/main/scala/com/avsystem/commons/meta/Fallback.scala
       ```
       If diff empty → skip the Fallback commit (already matches). If diff non-empty → overwrite from fork.

    4. Compile gate:
       ```bash
       sbt commons-core/compile
       sbt scalafmtCheckAll
       ```
       Both MUST exit 0.

    5. Commit AllowDerivation (separate commit per fork cadence). Paste Wave-0 probe lines from
       `/tmp/phase4-wave0-probes.txt` into the commit body:
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala
       git commit -m "feat(scala-3,core): port AllowDerivation

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/AllowDerivation.scala.

Wave-0 probes (from /tmp/phase4-wave0-probes.txt):
$(cat /tmp/phase4-wave0-probes.txt)"
       ```

    6. If Fallback diff non-empty, commit:
       ```bash
       git commit -m "feat(scala-3,core): port Fallback

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/Fallback.scala."
       ```
  </action>
  <verify>
    <automated>sbt commons-core/compile</automated>
  </verify>
  <acceptance_criteria>
    - `git diff HEAD~1 -- core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala` shows new file 7 LOC matching fork verbatim
    - `git log --oneline upstream/scala-3..HEAD -- core/src/main/scala/com/avsystem/commons/meta/` shows ≥1 fork-cadence commit
    - `sbt commons-core/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
    - `grep -E 'AllowDerivation' core/src/main/scala/com/avsystem/commons/meta/AllowDerivation.scala | wc -l` ≥ 3
  </acceptance_criteria>
  <done>
    AllowDerivation.scala new file committed; Fallback.scala either confirmed-identical (no commit) or ported (separate commit).
    Branch tip on `04-01-foundation`; no push yet.
  </done>
</task>

<task type="auto">
  <name>Task 3: Reconcile OptionLike + port metadata.scala (strip @name("dupa") debug artifact)</name>
  <read_first>
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/OptionLike.scala
    - git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metadata.scala
    - core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala (current — has BaseOptionLike shim per RESEARCH Pitfall 3)
    - core/src/main/scala/com/avsystem/commons/meta/metadata.scala (current state)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Divergences From Fork" + Pitfall 3, Pitfall 4)
  </read_first>
  <action>
    1. OptionLike reconcile (RESEARCH Pitfall 3 — KEEP our BaseOptionLike @bincompat shim, ADD fork's made.Default bridge):
       - Open `core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala`
       - Keep existing `BaseOptionLike` sealed trait + `@bincompat` ctor on `OptionLikeImpl` verbatim
       - Add fork's bridge line inside `object OptionLike`:
         ```scala
         given [O] => (optionLike: OptionLike[O]) => made.Default[O] = () => optionLike.none
         ```
       - Verify `import made.*` (or `import made.Default`) at top of file — add if missing
       - Do NOT drop BaseOptionLike or its bincompat scaffolding (per Pitfall 3 — preserves source-compat)

    2. Commit OptionLike (fork-cadence single commit):
       ```bash
       sbt commons-core/compile && sbt scalafmtCheckAll  # MUST exit 0 before commit
       git add core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala
       git commit -m "feat(scala-3,core): reconcile OptionLike with fork shape

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/OptionLike.scala.

Reconcile:
- Preserve our BaseOptionLike @bincompat shim (Pitfall 3 — source-compat for downstream).
- Add fork's given … => made.Default[O] = () => optionLike.none bridge.
- All other OptionLike content unchanged."
       ```

    3. metadata.scala port (Pitfall 4 — strip @name("dupa") debug artifact):
       ```bash
       git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metadata.scala \
         > core/src/main/scala/com/avsystem/commons/meta/metadata.scala
       ```
       Then edit: find `@name("dupa")` annotation on `ParamFlags.rawFlags` (or wherever it appears) and DELETE it.
       Verify zero occurrences:
       ```bash
       grep -n 'dupa' core/src/main/scala/com/avsystem/commons/meta/metadata.scala  # must be empty
       ```

    4. Compile gate (may surface Pitfall 7 — `GenCodec.given` empty import):
       ```bash
       sbt commons-core/compile
       sbt scalafmtCheckAll
       ```
       If both exit 0 → proceed.
       If `commons-core/compile` fails on `import com.avsystem.commons.serialization.GenCodec.given` (Pitfall 7):
         - Comment out the offending import line ONLY (not the rest of the file)
         - Add `// TODO[scala3-port]: re-enable GenCodec.given import once GenCodec exports givens (Phase 6)` above
         - Re-run compile; if still failing → comment out @transparent/@name annotations and HasGenCodec[X] companion calls similarly
         - Document the deferral in MIGRATION.md backlog (next task)

    5. Commit metadata.scala (separate fork-cadence commit):
       ```bash
       git add core/src/main/scala/com/avsystem/commons/meta/metadata.scala
       git commit -m "feat(scala-3,core): port metadata.scala (TypedMetadata + ParamFlags) — strip @name(\"dupa\") debug artifact

Translated from origin/master:core/src/main/scala-3/com/avsystem/commons/meta/metadata.scala.

- Carried fork's import block (made.*, made.annotation.*, GenCodec.given, HasGenCodec).
- Stripped @name(\"dupa\") debug artifact from ParamFlags.rawFlags per Pitfall 4 in RESEARCH.
- All other content verbatim from fork."
       ```
  </action>
  <verify>
    <automated>grep -c 'dupa' core/src/main/scala/com/avsystem/commons/meta/metadata.scala; sbt commons-core/compile</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'dupa' core/src/main/scala/com/avsystem/commons/meta/metadata.scala` → 0
    - `grep -c 'BaseOptionLike' core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala` ≥ 1 (shim preserved)
    - `grep -c 'made.Default' core/src/main/scala/com/avsystem/commons/meta/OptionLike.scala` ≥ 1 (bridge added)
    - `grep -E 'import made' core/src/main/scala/com/avsystem/commons/meta/metadata.scala` returns ≥ 1 match
    - `sbt commons-core/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
    - `git log --oneline upstream/scala-3..HEAD -- core/src/main/scala/com/avsystem/commons/meta/` shows ≥ 2 new commits (OptionLike + metadata)
  </acceptance_criteria>
  <done>
    OptionLike has both BaseOptionLike shim + fork's made.Default bridge; metadata.scala matches fork minus the dupa
    debug artifact. Both committed as separate fork-cadence commits. Branch tip on `04-01-foundation`.
  </done>
</task>

<task type="auto">
  <name>Task 4: MIGRATION.md updates (§3 OptionLike preservation + metadata strip notes)</name>
  <read_first>
    - MIGRATION.md (§3 source-compat section)
    - .planning/phases/04-meta-derivation-core/04-RESEARCH.md (§"Per-Slice Recommendations" 4.1 MIGRATION.md updates)
  </read_first>
  <action>
    Edit `MIGRATION.md` §3 source-compat — append a new sub-section under `### core` (or extend existing):

    ```markdown
    ### core — meta foundation (slice 4.1)

    - `meta/AllowDerivation` — new file, sealed-trait derivation-permission marker. Consumed by `MacroInstances`
      (slice 4.2). No public-API impact for downstream that didn't previously have access to this type.
    - `meta/OptionLike` — preserves `BaseOptionLike` `@bincompat` shim (intentional divergence from fork which
      drops it; preserves source-compat for downstream binaries built against our Phase 1 stub). Adds fork's
      `given … => made.Default[O]` bridge.
    - `meta/metadata` — `ParamFlags.rawFlags` no longer carries `@name("dupa")` annotation (fork debug artifact
      stripped). Affects serialized representation IF any downstream consumer was already serializing/round-tripping
      with the "dupa" key — extremely unlikely given the annotation was fork debug noise.
    ```

    If Task 3 had to defer `GenCodec.given` import or `@transparent`/`HasGenCodec` annotations, also append to
    backlog at bottom of MIGRATION.md:
    ```markdown
    - meta/metadata.scala — GenCodec.given / @transparent / HasGenCodec companions deferred to Phase 6
      (depends on real GenCodec macro derivation landing).
    ```

    Commit:
    ```bash
    git add MIGRATION.md
    git commit -m "docs(migration): record meta/ derivation foundation port (slice 4.1)

§3 source-compat additions:
- AllowDerivation new file (consumed by MacroInstances in slice 4.2)
- OptionLike preserves BaseOptionLike @bincompat shim (divergence from fork)
- metadata.scala strips fork's @name(\"dupa\") debug artifact from ParamFlags.rawFlags

Phase 4 slice 4.1 of 4.1→4.2→4.3→4.4→4.5 stacked PR chain."
    ```
  </action>
  <verify>
    <automated>grep -c 'slice 4.1' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -c 'slice 4.1' MIGRATION.md` ≥ 1
    - `grep -c 'BaseOptionLike' MIGRATION.md` ≥ 1
    - `grep -c 'dupa' MIGRATION.md` ≥ 1 (documenting the strip)
    - Commit subject starts with `docs(migration):`
  </acceptance_criteria>
  <done>
    MIGRATION.md §3 reflects all 3 changes; committed under `docs(migration):` prefix.
  </done>
</task>

<task type="auto">
  <name>Task 5: Final acceptance gate + push 04-01-foundation + open draft PR</name>
  <read_first>
    - ~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md (PR title prefix, milestone, draft rules)
    - .planning/STATE.md (PR opening precedent — gh api PATCH for milestone)
  </read_first>
  <action>
    1. Final acceptance gate (per phase_specific_constraints `4.1` gate):
       ```bash
       sbt commons-core/compile        # MUST exit 0
       sbt commons-core/Test/compile   # MUST exit 0
       sbt scalafmtCheckAll            # MUST exit 0
       ```
       If any fail → fix in a new fork-cadence commit, re-run.

    2. Sanity grep — no new @nowarn/-Wconf (QUALITY-01):
       ```bash
       git diff upstream/scala-3..HEAD -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)' | wc -l
       # MUST be 0
       ```

    3. No `.planning/` in any commit diff (WORKFLOW-05):
       ```bash
       git log --name-only upstream/scala-3..HEAD | grep -E '^\.planning/' | wc -l
       # MUST be 0
       ```

    4. Fork-shape parity check — verify per-file structure matches fork (semantic diff, not byte-exact):
       ```bash
       for f in AllowDerivation Fallback OptionLike metadata; do
         echo "=== $f ==="
         diff <(grep -cE '^(sealed|case|object|trait|class|given|inline|def) ' \
                  <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/$f.scala)) \
              <(grep -cE '^(sealed|case|object|trait|class|given|inline|def) ' \
                  core/src/main/scala/com/avsystem/commons/meta/$f.scala)
       done
       ```
       Expected: AllowDerivation/Fallback/metadata exact match; OptionLike differs by exactly the BaseOptionLike
       shim line count (documented divergence).

    5. Push branch to fork (WORKFLOW-03 exempt under yolo mode):
       ```bash
       git push -u origin 04-01-foundation
       ```

    6. Open draft PR against `AVSystem/scala-commons:scala-3` (base = upstream/scala-3, NOT stacked — 4.1 is base of stack):
       ```bash
       gh pr create \
         --repo AVSystem/scala-commons \
         --base scala-3 \
         --head halotukozak:04-01-foundation \
         --draft \
         --title "[Scala 3] port meta foundation (AllowDerivation/Fallback/OptionLike/metadata)" \
         --body "$(cat <<'EOF'
**Slice:** 4.1 of Phase 4 (meta/ derivation core)
**Merge order:** 4.1 → 4.2 → 4.3 → 4.4 → 4.5
**Depends on:** none — base of stack
**Base branch:** upstream/scala-3

## Summary
Ports the leaf files of the meta/ derivation DAG verbatim from `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/`:
- `AllowDerivation.scala` (new file)
- `Fallback.scala` (verbatim — likely no-op if already identical)
- `OptionLike.scala` (reconciled — preserves our `BaseOptionLike` @bincompat shim, adds fork's `made.Default` bridge)
- `metadata.scala` (verbatim minus `@name("dupa")` debug artifact strip)

No macros in this slice — pure ADT + typeclass plumbing.

## MIGRATION.md
- §3 entries for OptionLike `BaseOptionLike` preservation
- §3 note for `@name("dupa")` strip in `ParamFlags`

## Acceptance
- `sbt commons-core/compile` exit 0
- `sbt commons-core/Test/compile` exit 0
- `sbt scalafmtCheckAll` exit 0
- 0 new `@nowarn`/`-Wconf`
- Wave-0 probes: scala.NamedTuple stable on 3.8.2 (no experimental flag); cellar verified made_3:0.1.1 exports

Translated from `origin/master` fork files per [[feedback_crib_from_master]].
EOF
)"
       ```

    7. Capture PR number from `gh pr create` output (e.g., `PR_NUM=871`).

    8. Set milestone "Scala 3" (#1) via gh api PATCH (per memory precedent):
       ```bash
       gh api PATCH /repos/AVSystem/scala-commons/issues/$PR_NUM -f milestone=1
       ```

    9. Verify PR state:
       ```bash
       gh pr view $PR_NUM --repo AVSystem/scala-commons --json title,isDraft,milestone,baseRefName
       ```
       Expected: title starts with `[Scala 3]`, isDraft=true, milestone.number=1, baseRefName="scala-3".
  </action>
  <verify>
    <automated>gh pr list --repo AVSystem/scala-commons --head halotukozak:04-01-foundation --json number,isDraft,title | grep -c '"isDraft": true'</automated>
  </verify>
  <acceptance_criteria>
    - `sbt commons-core/compile` exit 0
    - `sbt commons-core/Test/compile` exit 0
    - `sbt scalafmtCheckAll` exit 0
    - `git diff upstream/scala-3..HEAD -- '*.scala' | grep -cE '^\+.*(@nowarn|-Wconf)'` → 0
    - `git log --name-only upstream/scala-3..HEAD | grep -c '^\.planning/'` → 0
    - Branch `04-01-foundation` exists on `origin` (halotukozak fork)
    - PR opened against `AVSystem/scala-commons:scala-3`, draft, `[Scala 3]` title prefix, milestone 1
    - PR body contains the required metadata block (Slice 4.1, Merge order, Depends on none, Base branch upstream/scala-3)
  </acceptance_criteria>
  <done>
    Branch pushed, draft PR opened against `AVSystem:scala-3`, milestone assigned, body metadata present.
    Slice 4.1 done. Slice 4.2 will branch off `04-01-foundation` tip.
  </done>
</task>

</tasks>

<verification>
- 4 source files match fork shape (3 verbatim + 1 reconciled OptionLike) minus the dupa strip
- `sbt commons-core/compile ;commons-core/Test/compile ;scalafmtCheckAll` exit 0
- 0 new `@nowarn`/`-Wconf` vs upstream/scala-3
- 0 `.planning/` paths in commit diff
- Fork-cadence: ≥3 atomic commits (AllowDerivation, OptionLike, metadata; optional Fallback)
- MIGRATION.md §3 updated
- Draft PR open at AVSystem/scala-commons against scala-3 base with full metadata block
</verification>

<success_criteria>
Slice 4.1 succeeds when:
1. All 4 foundation files compile + match fork shape (per acceptance grep)
2. `BaseOptionLike` shim preserved AND `made.Default` bridge added in `OptionLike.scala`
3. `@name("dupa")` removed from `metadata.scala`
4. MIGRATION.md §3 records all 3 changes
5. Branch pushed; draft PR open against `upstream/scala-3` with `[Scala 3]` prefix + milestone 1 + body metadata
6. Wave-0 named-tuple probe outcome documented (passed = no flag added; failed = `-language:experimental.namedTuples` added to scalacOptions and noted for slice 4.2)
</success_criteria>

<output>
After completion, create `.planning/phases/04-meta-derivation-core/04-01-SUMMARY.md`
</output>
