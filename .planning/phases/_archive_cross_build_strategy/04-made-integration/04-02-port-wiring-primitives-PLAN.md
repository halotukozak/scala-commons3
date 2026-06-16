---
phase: 04-made-integration
plan: 02
type: execute
wave: 2
depends_on: ["04-01"]
files_modified:
  - core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala
  - core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala
  - core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala
  - core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala
  - core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
autonomous: true
requirements:
  - MADE-01
  - DEPR-01
must_haves:
  truths:
    - "Five new files exist on branch `04-made-integration` under `core/src/main/scala-3/`: `misc/{Opt,NOpt,OptArg,OptRef}.scala` and `serialization/madeAnnotationAliases.scala`"
    - "`Opt.scala`, `NOpt.scala`, `OptArg.scala`, `OptRef.scala` import `made.Default` and contain `given … => Default[…]` instances"
    - "`object Opt`, `object NOpt`, `object OptRef` do NOT extend `OptCompat`/`NOptCompat`/`OptRefCompat` (deprecated shims dropped per `feedback_dont_port_deprecated.md`)"
    - "`compat.scala` is NOT created on this branch (it references deferred `GenCodec`/`GenKeyCodec` types — Phase 5)"
    - "`madeAnnotationAliases.scala` re-exports `generated`, `name`, `optionalParam`, `transparent`, `whenAbsent`, `TransparentWrapping` from the `made` library"
  artifacts:
    - path: "core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala"
      provides: "Scala 3 `Opt[A]` value-class + companion with `given Default[Opt[A]]`"
      contains: "given [A] => Default[Opt[A]]"
    - path: "core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala"
      provides: "Scala 3 `NOpt[A]` + companion with `given Default[NOpt[A]]`"
      contains: "import made.Default"
    - path: "core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala"
      provides: "Scala 3 `OptArg[A]` + companion with `given Default[OptArg[A]]`"
      contains: "import made.Default"
    - path: "core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala"
      provides: "Scala 3 `OptRef[A]` + companion with `given Default[OptRef[A]]`"
      contains: "import made.Default"
    - path: "core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala"
      provides: "Top-level `export` of `made.annotation.*` and `made.TransparentWrapping` under `com.avsystem.commons.serialization`"
      contains: "export made.annotation"
  key_links:
    - from: "object Opt (and NOpt, OptRef)"
      to: "given Conversion[Opt[A], Iterable[A]] inside the same companion"
      via: "in-companion given replaces deprecated opt2Iterable shim from OptCompat trait"
      pattern: "given.*Conversion\\[.*Opt"
    - from: "Opt.scala / NOpt.scala / OptArg.scala / OptRef.scala"
      to: "made.Default trait (made_3:0.1.0)"
      via: "given Default[Wrapper[A]] = emptyDefault.asInstanceOf[Default[Wrapper[A]]]"
      pattern: "Default\\[(Opt|NOpt|OptArg|OptRef)"
    - from: "madeAnnotationAliases.scala"
      to: "made.annotation.{generated,name,optionalParam,transparent,whenAbsent} and made.TransparentWrapping"
      via: "top-level Scala 3 export"
      pattern: "export made\\."
---

<objective>
Port the FIVE wiring-primitive source files from this clone's fork-master working tree (where they already exist verbatim) into the new `04-made-integration` branch (which was cut off `upstream/scala-3` and currently lacks these files). Drop `extends OptCompat`/`NOptCompat`/`OptRefCompat` from the companions per `feedback_dont_port_deprecated.md`. Do NOT create `compat.scala` — its deprecated traits are skipped and its non-deprecated traits reference deferred `GenCodec`/`GenKeyCodec` types (Phase 5).

Purpose: Establish the minimum-island `made`-integration surface — `Default` givens for the four value-class wrappers + annotation re-exports — without dragging in any deferred derivation surface (`GenCodec*`, `GenObjectCodec*`, `HasGenCodec*`, etc.).

Output: Five committed Scala 3 source files; no other files touched.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/REQUIREMENTS.md
@.planning/phases/04-made-integration/04-CONTEXT.md
@.planning/phases/04-made-integration/04-RESEARCH.md
@.planning/phases/04-made-integration/04-VALIDATION.md
@.planning/phases/04-made-integration/04-01-SUMMARY.md

**Source-of-truth files on the user's working clone** (path on the fork-master `worktree-equivalent` for this clone — the executor must access them BEFORE checking out `04-made-integration`, OR fetch them from `origin/master`):
@core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala
@core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala
@core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala
@core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala
@core/src/main/scala-3/com/avsystem/commons/misc/compat.scala
@core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala

<interfaces>
<!-- Authoritative `made` 0.1.0 contracts the ported files depend on. Verified via -->
<!-- `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` -->
<!-- and `cellar list-external io.github.halotukozak:made_3:0.1.0 made.annotation`. -->
<!-- The ported files use these SAM/trait surfaces unchanged from fork-master. -->

`made.Default` (made_3:0.1.0):
```scala
package made
trait Default[O] extends Function0[O]   // SAM — () => O is a valid Default[O]
```

`made.TransparentWrapping`:
```scala
package made
trait TransparentWrapping[R, T] { def wrap(r: R): T; def unwrap(t: T): R }
```

`made.annotation.*` (all classes, all present at 0.1.0):
```scala
package made.annotation
class generated     extends MetaAnnotation
class name          extends scala.annotation.Annotation with scala.annotation.RefiningAnnotation
class optionalParam extends MetaAnnotation
class transparent   extends MetaAnnotation
class whenAbsent[+T] extends MetaAnnotation
```

`Conversion` (Scala 3 stdlib) — used in the in-companion `given Conversion[Opt[A], Iterable[A]]` that supersedes the dropped `opt2Iterable` shim.
</interfaces>

<existing_source_dump>
<!-- For the executor's convenience — the actual fork-master version of compat.scala lines 50–63 -->
<!-- showing the three traits being DROPPED (their single deprecated method is supplanted by -->
<!-- a `given Conversion[…]` already in each companion). -->

```scala
// REMOVED — do NOT port compat.scala or these three traits:
trait OptCompat { this: Opt.type =>
  @deprecated("Use given Conversion directly", since = "3.0.0")
  def opt2Iterable[A](opt: Opt[A]): Iterable[A] = summon[Conversion[Opt[A], Iterable[A]]](opt)
}
trait NOptCompat { this: NOpt.type =>
  @deprecated("Use given Conversion directly", since = "3.0.0")
  def opt2Iterable[A](opt: NOpt[A]): Iterable[A] = summon[Conversion[NOpt[A], Iterable[A]]](opt)
}
trait OptRefCompat { this: OptRef.type =>
  @deprecated("Use given Conversion directly", since = "3.0.0")
  def opt2Iterable[A](opt: OptRef[A]): Iterable[A] = summon[Conversion[OptRef[A], Iterable[A]]](opt)
}
```
</existing_source_dump>

**Memory rules to honor:**
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_dont_port_deprecated.md` — skip `@deprecated` scala-2 APIs with stdlib/library replacements. The three `Opt*Compat` traits and their `opt2Iterable` methods are exactly this case (replaced by `given Conversion[…, Iterable[…]]` already in each companion).
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/feedback_fix_dont_suppress_warnings.md` — no new `@nowarn` / `-Wconf`. Enforced by QUALITY-01 grep gate in Plan 03.
</context>

<tasks>

<task type="auto">
  <name>Task 1: Copy the 4 misc/ Opt* files from origin/master and drop `extends *Compat` clauses</name>
  <files>
    core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala
    core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala
    core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala
    core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala
  </files>
  <action>
You are on branch `04-made-integration` (set up by Plan 01). The 5 source files do NOT exist on `upstream/scala-3` but DO exist on `origin/master`. Copy them from `origin/master` and apply two minimal edits.

**Step 1 — Confirm branch and prerequisites:**
```sh
git rev-parse --abbrev-ref HEAD          # must print: 04-made-integration
git rev-parse origin/master              # must succeed (the source-of-truth ref)
```

**Step 2 — Copy the four files verbatim from `origin/master`:**

For each path, use `git show origin/master:<path>` to dump and write to the working tree. (Do NOT use `git checkout` — that stages too.) Use the Bash tool with output redirection through Write — read the file via `git show`, then use the Write tool to author the file at the target path:

Files to copy (paths identical between source and destination):
- `core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala`
- `core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala`
- `core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala`
- `core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala`

Concretely, for each file, run:
```sh
mkdir -p core/src/main/scala-3/com/avsystem/commons/misc
git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala > core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala
```
…and analogous for `NOpt.scala`, `OptArg.scala`, `OptRef.scala`.

**Step 3 — Drop the `extends *Compat` clauses** (per `feedback_dont_port_deprecated.md`; the replacement `given Conversion[…]` is already inside each companion in the fork-master files):

Use the Edit tool on each file:

- `Opt.scala`: change `object Opt extends OptCompat {` → `object Opt {`
- `NOpt.scala`: change `object NOpt extends NOptCompat {` → `object NOpt {`
- `OptRef.scala`: change `object OptRef extends OptRefCompat {` → `object OptRef {`
- `OptArg.scala`: **NO change** — its companion does NOT extend any compat trait on fork-master (confirmed via grep in RESEARCH.md "Per-File Import Audit").

**Step 4 — Verify no stale `*Compat` reference remains in the four files:**
```sh
! grep -nE 'extends (Opt|NOpt|OptRef)Compat' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala
```
Must produce no output.

**Step 5 — Verify each file imports `made.Default`:**
```sh
for f in Opt NOpt OptArg OptRef; do
  grep -q 'import made.Default' "core/src/main/scala-3/com/avsystem/commons/misc/$f.scala" || { echo "MISSING import made.Default in $f.scala"; exit 1; }
done
```

**Step 6 — Verify no deferred-type imports leaked in** (the four Opt files MUST NOT import `GenCodec`, `GenKeyCodec`, `GenObjectCodec`, `GenRef`, `HasGenCodec`):
```sh
! grep -nE 'import .*(GenCodec|GenKeyCodec|GenObjectCodec|GenRef|HasGenCodec)' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala
```
Must produce no output.

**Step 7 — Commit (single commit for the four files):**
```sh
git add core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala
git commit -m "feat(core/scala-3): port Opt/NOpt/OptArg/OptRef with made.Default givens"
```

**Pitfalls** (see RESEARCH.md):
- Do NOT also copy `compat.scala` — Task 2 explicitly skips it.
- Do NOT copy `MiscAliases.scala` — deferred to Phase 5 per RESEARCH.md.
- Do NOT introduce `@nowarn` or `-Wconf` (QUALITY-01 enforced in Plan 03).
  </action>
  <verify>
    <automated>test -f core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala && test -f core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala && test -f core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala && test -f core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala && ! grep -qE 'extends (Opt|NOpt|OptRef)Compat' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala && ! grep -qE 'import .*(GenCodec|GenKeyCodec|GenObjectCodec|GenRef|HasGenCodec)' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala && grep -q 'import made.Default' core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala</automated>
  </verify>
  <done>Four Scala 3 source files exist at the listed paths; none extend a `*Compat` trait; none import deferred types; all four import `made.Default`; one commit added on the branch.</done>
</task>

<task type="auto">
  <name>Task 2: Copy madeAnnotationAliases.scala from origin/master and confirm compat.scala is NOT created</name>
  <files>
    core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
  </files>
  <action>
**Step 1 — Copy `madeAnnotationAliases.scala` verbatim from `origin/master`:**
```sh
mkdir -p core/src/main/scala-3/com/avsystem/commons/serialization
git show origin/master:core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala > core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
```

**Step 2 — Confirm content:**
```sh
grep -c '^export made\.' core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
```
Expected output: `6` (five `export made.annotation.*` lines + one `export made.TransparentWrapping`).

If the count is different from 6, do NOT silently re-author; surface the discrepancy to the user — origin/master may have drifted.

**Step 3 — EXPLICIT NEGATIVE GATE — `compat.scala` must NOT exist on this branch:**
```sh
test ! -e core/src/main/scala-3/com/avsystem/commons/misc/compat.scala
```
If the file exists at this point in execution, something has gone wrong (perhaps Task 1 accidentally copied extras). Remove it:
```sh
rm -f core/src/main/scala-3/com/avsystem/commons/misc/compat.scala
git rm --cached core/src/main/scala-3/com/avsystem/commons/misc/compat.scala 2>/dev/null || true
```

**Rationale (per RESEARCH.md "Required Pruning"):** `compat.scala` on fork master imports `com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}` (line 4) — both deferred-Phase-5 types. The three `Opt*Compat` traits hold deprecated `opt2Iterable` shims supplanted by an in-companion `given Conversion[…]`. The rest of the file references other deferred types. Skipping the file entirely is the locked decision.

**Step 4 — Verify grep for `extends *Compat` AND `compat.scala` existence:**
```sh
! test -e core/src/main/scala-3/com/avsystem/commons/misc/compat.scala
! grep -RnE 'extends (Opt|NOpt|OptRef)Compat' core/src/main/scala-3/com/avsystem/commons/misc/
```
Both must produce no output / pass.

**Step 5 — Commit:**
```sh
git add core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
git commit -m "feat(core/scala-3): port madeAnnotationAliases re-exports"
```
  </action>
  <verify>
    <automated>test -f core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala && [ "$(grep -c '^export made\.' core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala)" = "6" ] && ! test -e core/src/main/scala-3/com/avsystem/commons/misc/compat.scala</automated>
  </verify>
  <done>`madeAnnotationAliases.scala` exists with 6 `export made.*` lines; `compat.scala` does NOT exist on this branch; one commit added.</done>
</task>

</tasks>

<verification>
End-of-plan gate (Plan 03 re-runs the full sbt suite):

```sh
# 5 files exist, 1 absent
ls core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala
ls core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala
test ! -e core/src/main/scala-3/com/avsystem/commons/misc/compat.scala

# Compat clauses dropped
! grep -RqE 'extends (Opt|NOpt|OptRef)Compat' core/src/main/scala-3/com/avsystem/commons/misc/

# No deferred type imports leaked
! grep -RqE 'import .*(GenCodec|GenKeyCodec|GenObjectCodec|GenRef|HasGenCodec)' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala

# made.Default is wired
grep -c 'import made.Default' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala

# Two new commits since Plan 01
git log upstream/scala-3..HEAD --oneline | wc -l   # → 3 (1 from Plan 01 + 2 from Plan 02)
```
</verification>

<success_criteria>
- [ ] All five ported files exist at their target paths
- [ ] `compat.scala` does NOT exist
- [ ] `extends OptCompat` / `extends NOptCompat` / `extends OptRefCompat` removed from companions (OptArg unaffected — never extended one)
- [ ] No imports of `GenCodec`/`GenKeyCodec`/`GenObjectCodec`/`GenRef`/`HasGenCodec` in the four misc/ files
- [ ] `madeAnnotationAliases.scala` has exactly 6 `export made.*` lines
- [ ] All four misc/ Opt files import `made.Default`
- [ ] Two commits added (one for the four misc files, one for the aliases file) — both `feat(...)` prefix, no GSD nomenclature
</success_criteria>

<output>
After completion, create `.planning/phases/04-made-integration/04-02-SUMMARY.md` documenting:
- The 5 file paths committed
- Confirmation `compat.scala` was NOT created
- Confirmation `extends *Compat` clauses were removed from the 3 affected companions
- Two commit SHAs
- Any deviation from `origin/master` content (expected: NONE for the 5 files; the only edits are the three `extends *Compat` removals)
</output>
</content>
