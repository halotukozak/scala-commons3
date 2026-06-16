---
phase: 05-leaf-feature-restoration
plan: 04
type: execute
wave: 1
depends_on: []
files_modified:
  - core/src/main/scala/com/avsystem/commons/misc/TypeString.scala
  - core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala
  - MIGRATION.md
autonomous: false
requirements:
  - TYPESTRING-01
  - JAVACLASSNAME-01
must_haves:
  truths:
    - "`TypeString.of[List[Int]]` produces `\"List[Int]\"` at runtime via `TypeRepr.show(Printer.TypeReprShortCode)`"
    - "`JavaClassName.of[Foo]` matches `classOf[Foo].getName` at runtime"
    - "TypeString.scala matches fork verbatim — both `materializeImpl` (companion-local) and `derivedImpl` (top-level for JavaClassName) per fork"
    - "GenKeyCodec / GenCodec givens for TypeString switched to per-T `given [T] => ...` form per Pitfall 7"
    - "`commons-core/compile` + SharedExtensionsTest both green"
    - "MIGRATION.md §3 records reshape (incl. GenKeyCodec/GenCodec per-T given change if applicable)"
  artifacts:
    - path: "core/src/main/scala/com/avsystem/commons/misc/TypeString.scala"
      provides: "TypeString + JavaClassName + their companions with inline given + materialize/derived impls"
      contains: "inline given"
  key_links:
    - from: "TypeString.scala companion"
      to: "scala.quoted.Quotes"
      via: "private def materializeImpl[T: Type](using quotes: Quotes)"
      pattern: "Printer\\.TypeReprShortCode"
    - from: "JavaClassName.scala (or top-level in TypeString.scala)"
      to: "TypeRepr.of[T].dealias.typeSymbol"
      via: "derivedImpl"
      pattern: "Flags\\.Module"
---

<objective>
Port `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` verbatim from fork (120 LOC, coupled with `JavaClassName` per CONTEXT). Both materialize impls live in this single file (TypeString uses companion-local `materializeImpl`; JavaClassName uses top-level `derivedImpl` per fork — Pattern 1 + Open Question 2 in RESEARCH.md).

Per Pitfall 7: TypeString may need a per-`T` `given` for `GenKeyCodec`/`GenCodec` instead of single existential `implicit val` — verify diff before porting and document.

Output:
- Updated `TypeString.scala` (covers both leaves — fork is single file)
- Un-wrapped SharedExtensionsTest (per CONTEXT — covers TypeString smoke; fork commit `dcf60e5d`)
- Note: there is NO standalone `TypeStringTest` or `JavaClassNameTest` in tree (JavaClassNameTest depends on TestMacros, out of scope per VALIDATION.md)
- MIGRATION.md §3 entry
- 3 atomic commits (feat + test + docs) — own PR, independent base
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md
@.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md
@.planning/phases/05-leaf-feature-restoration/05-VALIDATION.md

<interfaces>
<!-- Fork TypeString.scala companion fragment (Pattern 1 in RESEARCH) -->
class TypeString[T](val value: String) extends AnyVal { override def toString = value }
object TypeString extends TypeStringCompat {
  inline given [T] => TypeString[T] = ${ materializeImpl[T] }
  def of[T: TypeString]: String = TypeString[T].value
  def apply[T](using ts: TypeString[T]): TypeString[T] = ts

  given [T] => GenKeyCodec[TypeString[T]] = GenKeyCodec.create[TypeString[T]](new TypeString(_), _.value)
  given [T] => GenCodec[TypeString[T]] = GenCodec.createSimple[TypeString[T]](...)

  private def materializeImpl[T: Type](using quotes: Quotes) = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val typeString = Expr(tpe.show(using Printer.TypeReprShortCode))
    '{ new TypeString[T]($typeString) }
  }
}

<!-- JavaClassName + top-level derivedImpl (per Open Question 2) -->
// derivedImpl lives at TOP LEVEL of TypeString.scala, referenced from JavaClassNameLowPriority trait
def derivedImpl[T: Type](using quotes: Quotes): Expr[JavaClassName[T]] = { ... }
</interfaces>

<branch_strategy>
Branch off `04-05-meta-annotations @ f04cec6f`. Independent of slice 5.0.
  git fetch origin upstream && git checkout -b 05-04-typestring-javaclassname 04-05-meta-annotations
</branch_strategy>
</context>

<tasks>

<task type="auto" tdd="false">
  <name>Task 1: Diff current vs fork; port TypeString.scala (incl. JavaClassName) verbatim</name>
  <files>core/src/main/scala/com/avsystem/commons/misc/TypeString.scala</files>
  <read_first>
    - `.planning/phases/05-leaf-feature-restoration/05-RESEARCH.md` (Pattern 1 — Inline Given + Splice; Code Examples Ex 1; Open Question 2; Pitfall 2; Pitfall 7)
    - `.planning/phases/05-leaf-feature-restoration/05-CONTEXT.md`
    - Current file (Phase-1 stub): `cat core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`
    - Fork source: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala`
    - Audit current GenKeyCodec/GenCodec given shape: `git grep -nE 'GenKeyCodec\\[TypeString|GenCodec\\[TypeString' -- '*.scala'`
    - Callers of TypeString / JavaClassName: `git grep -nE '\\b(TypeString|JavaClassName)\\b' -- '*.scala' | grep -v misc/TypeString`
  </read_first>
  <action>
    Port via diff-and-replace (NOT blind overwrite — Pitfall 7 requires diff-first):

    1. Cut branch: `git checkout -b 05-04-typestring-javaclassname 04-05-meta-annotations`
    2. Generate diff for executor reference:
       `diff core/src/main/scala/com/avsystem/commons/misc/TypeString.scala <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala) > /tmp/typestring.diff`
    3. Inspect diff: look for `implicit val keyCodec: GenKeyCodec[TypeString[_]]` (existential, Phase-1 stub) vs fork's `given [T] => GenKeyCodec[TypeString[T]]` (per-T). Pitfall 7: per-T given is BREAKING for callers that did `summon[GenKeyCodec[TypeString[Foo]]]`. Audit callers via grep before flipping.
    4. Overwrite: `git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala > core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`
    5. Reconcile any non-trivial import differences (single-source layout). Per Pitfall 2: preserve fork's exact `inline given [T] => TypeString[T] = ${ ... }` syntax — DO NOT normalize.
    6. Verify `TypeStringCompat` trait exists in tree (companion extends it): `git grep -n 'TypeStringCompat' core/src/main/scala/`. If missing — was it removed in a slice 3.X? Resolve before continuing.
    7. Run `sbt commons-core/compile`. EXPECTED: green. If failure references downstream consumers of `GenKeyCodec[TypeString[_]]`, fix the consumer to use per-T given (NOT revert the leaf) — document in commit body.
    8. `sbt scalafmtCheckAll` + auto-fix if needed.
    9. Commit: `feat(scala-3,core): port TypeString + JavaClassName (Pattern 1 + top-level derivedImpl)` with body referencing fork file + any GenKeyCodec/GenCodec reshape notes (per Pitfall 7).
  </action>
  <verify>
    <automated>grep -q 'inline given' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala &amp;&amp; grep -q 'Printer.TypeReprShortCode' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala &amp;&amp; sbt -batch 'commons-core/compile ;scalafmtCheckAll'</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'inline given \\[T\\] => TypeString\\[T\\]' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` exits 0
    - `grep -q 'Printer.TypeReprShortCode' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` exits 0
    - `grep -q 'JavaClassName' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` exits 0
    - `grep -q 'def derivedImpl' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` exits 0
    - `! grep -q '???' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`
    - `diff <(git show origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala) core/src/main/scala/com/avsystem/commons/misc/TypeString.scala` shows minimal diff (whitespace only)
    - `sbt -batch commons-core/compile` exits 0
    - `sbt -batch scalafmtCheckAll` exits 0
    - `git log -1 --pretty=%s` matches `^feat\\(scala-3,core\\): port TypeString \\+ JavaClassName`
  </acceptance_criteria>
  <done>TypeString.scala covers both TYPESTRING-01 + JAVACLASSNAME-01; compile green.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 2: Un-wrap SharedExtensionsTest TypeString smoke cases (fork commit dcf60e5d)</name>
  <files>core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala</files>
  <read_first>
    - Current test: `cat core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala`
    - Fork test (target): `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/SharedExtensionsTest.scala`
    - Fork commit context: `git show origin/master@dcf60e5d --stat`
  </read_first>
  <action>
    Per CONTEXT — un-wrap TypeString smoke cases inside SharedExtensionsTest. Per VALIDATION.md row TYPESTRING-01: this is the verify target.

    1. Diff current vs fork for SharedExtensionsTest. Likely current has `/* ... */` wraps around TypeString-dependent cases (Phase-1 big-bang). Un-wrap those cases.
    2. If wholesale overwrite is cleaner AND fork test doesn't introduce out-of-scope dependencies: `git show origin/master:core/src/test/scala-3/com/avsystem/commons/misc/SharedExtensionsTest.scala > core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala`. Else surgical un-wrap.
    3. Cases exercising features still in `???` stub state (e.g. anything via MacroInstances Phase-6 chain) → leave `ignore`d per fork pattern.
    4. Run `sbt 'commons-core/testOnly *.SharedExtensionsTest'`. EXPECTED: green (0 failures; some `ignored` OK).
    5. Commit: `test(scala-3,core): un-wrap SharedExtensionsTest TypeString smoke cases (fork dcf60e5d)`.
  </action>
  <verify>
    <automated>sbt -batch 'commons-core/testOnly *.SharedExtensionsTest'</automated>
  </verify>
  <acceptance_criteria>
    - `sbt -batch 'commons-core/testOnly *.SharedExtensionsTest'` exits 0 with 0 failures
    - `! grep -qE 'TODO\\[scala3-port\\].*TypeString' core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala`
    - `grep -q 'JavaClassName.of' core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala` exits 0 (runtime smoke for JAVACLASSNAME-01)
    - `git log -1 --pretty=%s` matches `^test\\(scala-3,core\\): un-wrap SharedExtensionsTest`
  </acceptance_criteria>
  <done>SharedExtensionsTest TypeString cases un-wrapped, green; second atomic commit.</done>
</task>

<task type="auto" tdd="false">
  <name>Task 3: Update MIGRATION.md §3 (TypeString + JavaClassName + GenKeyCodec/GenCodec per-T given reshape if applicable)</name>
  <files>MIGRATION.md</files>
  <read_first>
    - `MIGRATION.md`
    - Task 1 commit body — verify what GenKeyCodec/GenCodec reshape (if any) was applied
  </read_first>
  <action>
    §3 `core` row append:
    ```
    | misc/TypeString + JavaClassName | n/a | source-compat (per-T given) | Phase-5 slice 5.4: ported from `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala` (single file holds both leaves). `materializeImpl` companion-local; `derivedImpl` top-level for JavaClassName. GenKeyCodec/GenCodec for `TypeString[T]` switched from single existential `implicit val ...[TypeString[_]]` to per-T `given [T] => ...[TypeString[T]]` — callers doing `summon[GenKeyCodec[TypeString[Foo]]]` now resolve via parametric given. SharedExtensionsTest TypeString smoke re-enabled per fork `dcf60e5d`. |
    ```
    Backlog: remove TypeString + JavaClassName rows.

    Commit: `docs(migration): record TypeString + JavaClassName port`.

    If Task 1 actually did NOT change the GenKeyCodec/GenCodec shape (e.g. fork and current happened to match already), narrow the entry — omit the per-T given reshape sentence.
  </action>
  <verify>
    <automated>grep -q 'slice 5.4' MIGRATION.md &amp;&amp; grep -q 'TypeString' MIGRATION.md &amp;&amp; grep -q 'JavaClassName' MIGRATION.md</automated>
  </verify>
  <acceptance_criteria>
    - `grep -q 'slice 5.4' MIGRATION.md` exits 0
    - `grep -q 'TypeString' MIGRATION.md` exits 0
    - `grep -q 'JavaClassName' MIGRATION.md` exits 0
    - `git log -1 --pretty=%s` matches `^docs\\(migration\\): record TypeString \\+ JavaClassName port$`
  </acceptance_criteria>
  <done>MIGRATION.md updated; third atomic commit.</done>
</task>

<task type="checkpoint:human-verify" gate="blocking">
  <name>Task 4: Checkpoint — push branch and open draft PR</name>
  <action>Pause for human verification. See &lt;how-to-verify&gt; block for the exact commands to run.</action>
  <what-built>Branch `05-04-typestring-javaclassname` with 3 commits. Both TYPESTRING-01 and JAVACLASSNAME-01 satisfied in one PR (coupled file per fork).</what-built>
  <how-to-verify>
    Pre-push:
    1. `git log --oneline 04-05-meta-annotations..HEAD | wc -l` == 3
    2. `sbt 'commons-core/compile ;commons-core/testOnly *.SharedExtensionsTest ;scalafmtCheckAll'` exit 0
    3. `! git diff 04-05-meta-annotations..HEAD -- '*.scala' | grep -E '^\\+.*(@nowarn|-Wconf)'`
    4. `! git grep '???' core/src/main/scala/com/avsystem/commons/misc/TypeString.scala`

    Then:
    ```
    git push -u origin 05-04-typestring-javaclassname
    gh pr create --draft --base 04-05-meta-annotations --head halotukozak:05-04-typestring-javaclassname \
      --title "[Scala 3] port TypeString + JavaClassName" \
      --body "Slice 5.4 / Parallel — independent (no MiscMacros dep — impls live in TypeString.scala companion + top-level) / Depends on: none / Base branch: 04-05-meta-annotations / Requirements: TYPESTRING-01, JAVACLASSNAME-01 / Note: coupled in single fork file; companion-local materializeImpl for TypeString + top-level derivedImpl for JavaClassName per fork; GenKeyCodec/GenCodec per-T given reshape if Phase-1 stub differed"
    PR_NUM=$(gh pr view --json number -q .number)
    gh api -X PATCH "/repos/AVSystem/scala-commons/issues/$PR_NUM" -f milestone=1
    ```
  </how-to-verify>
  <resume-signal>Type "approved" after PR open + milestone, or describe issues.</resume-signal>
</task>

</tasks>

<verification>
- TypeString.scala matches fork; both materializeImpl + derivedImpl present
- SharedExtensionsTest green (TypeString cases active)
- MIGRATION.md updated
- Draft PR, prefix, milestone 1
</verification>

<success_criteria>
- TYPESTRING-01 + JAVACLASSNAME-01 both satisfied
- TypeString.of[List[Int]] == "List[Int]"
- JavaClassName.of[Foo] == classOf[Foo].getName
</success_criteria>

<output>
After completion, create `.planning/phases/05-leaf-feature-restoration/05-04-SUMMARY.md`
</output>
