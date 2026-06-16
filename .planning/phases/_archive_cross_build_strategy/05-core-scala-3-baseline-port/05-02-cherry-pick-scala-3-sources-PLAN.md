---
phase: 05-core-scala-3-baseline-port
plan: 02
type: execute
wave: 2
depends_on: ["05-01"]
files_modified:
  - core/src/main/scala-3/**
autonomous: true
commit_docs: false
requirements: [CORE-01, CORE-02, QUALITY-01]

must_haves:
  truths:
    - "++3.8.2 commons-core/compile exits 0 (THE Phase 5 goal)"
    - "++2.13.18 commons-core/compile remains GREEN (regression guard)"
    - "++3.8.2 commons-macros/compile remains GREEN (Phase 3 protection)"
    - "Phase 4 wiring primitives (Opt/NOpt/OptArg/OptRef/madeAnnotationAliases) preserved unchanged"
    - "No cbor/mongo/RPC code touched (sub-module dirs untouched; references in ported files trimmed)"
    - "No @nowarn / -Wconf introduced (memory rule)"
    - "// format: off blocks acceptable around macro defs only (memory rule)"
  artifacts:
    - path: "core/src/main/scala-3/com/avsystem/commons/"
      provides: "Cherry-picked Scala 3 source baseline — annotations, collection, concurrent, jiop, derivation, meta, misc, serialization entry points (sans cbor)"
      min_lines: 1
    - path: "core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala"
      provides: "GenCodec entry point (CORE-01 typeclass derivation)"
    - path: "core/src/main/scala-3/com/avsystem/commons/serialization/GenKeyCodec.scala"
      provides: "GenKeyCodec entry point (CORE-01)"
    - path: "core/src/main/scala-3/com/avsystem/commons/serialization/GenObjectCodec.scala"
      provides: "GenObjectCodec entry point (CORE-01)"
  key_links:
    - from: "core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala"
      to: "made.Default + Phase 4 wiring primitives"
      via: "given derivation"
      pattern: "import made\\."
    - from: "core/src/main/scala-3/"
      to: "core/src/main/scala-2.13/ (no overlap — exclusive partitions)"
      via: "sbt mkSourceDirs cross-version partitioning"
---

<objective>
Cherry-pick the Scala 3 source baseline from fork `master:core/src/main/scala-3/` into the branch, iteratively, until `++3.8.2 commons-core/compile` exits 0. This is the goal of Phase 5 (CORE-01 typeclass derivation entry points; CORE-02 source organization).

Source library: 111 files on `master:core/src/main/scala-3/` (from `git ls-tree -r master --name-only -- core/src/main/scala-3/`). Phase 5 ports the subset whose imports resolve against (a) the Scala stdlib, (b) `made` 0.1.0, (c) Phase 4 wiring primitives, (d) other Phase 5 cherry-picked files, (e) files in `core/` itself. Files referencing `cbor/`, `mongo`, or `RPC` types either get those references trimmed in-place OR get deferred to Phase 6+.

Out of scope: `core/src/main/scala-3/com/avsystem/commons/serialization/cbor/**` (deferred to Phase 11), any new test code, full `implicit→given/using/extension` sweep (deferred to Phase 6).

Output: multiple commits (one per coherent file batch — see <commit_strategy>), `++3.8.2 commons-core/compile` GREEN, ++2.13 + macros + scalafmtCheckAll all still GREEN.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.planning/REQUIREMENTS.md
@.planning/phases/05-core-scala-3-baseline-port/05-CONTEXT.md
@.planning/phases/05-core-scala-3-baseline-port/05-01-branch-and-relocate-scala-2-only-SUMMARY.md
@.planning/phases/04-made-integration/04-02-port-wiring-primitives-SUMMARY.md
@~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md

<workflow_rules>
- Plan 05-01 has run; Scala-2-only files now under `core/src/main/scala-2.13/`.
- No `@nowarn` / `-Wconf` introductions (memory `feedback_fix_dont_suppress_warnings.md`).
- Skip deprecated APIs that have stdlib replacements (memory `feedback_dont_port_deprecated.md`); do NOT port `compat.scala` from master — its content is deprecated + references deferred types.
- `// format: off` around problematic macro/derivation blocks OK (memory `feedback_format_off_macro_defs_ok.md`).
- Commit messages: NO GSD nomenclature; NO `.planning/` paths.
- DO NOT push or open PR — Plan 05-04.
- DO NOT touch MIGRATION.md in this plan — Plan 05-03.
</workflow_rules>

<known_files>
The 111 files on master are grouped roughly as:

LEAVES (no internal deps, port first — annotations, basic types, simple utils):
- annotation/* (AnnotationAggregate, AnnotationMacros, NotInheritedFromSealedTypes, TodoScala3Migration, atLeast, bincompat, defaultsToName, explicitGenerics, macroPrivate, positioned, showAst)
- collection/* (CloseableIterator, CollectionAliases, MutableStack)
- concurrent/* (DurationPostfixConverters, ObservableExtensions, RetryStrategy, TaskExtensions, executionContexts, package.scala)
- jiop/* (JBasicUtils, JCollectionUtils, JFactory)
- derivation/* (AllowImplicitMacro, DeferredInstance, Materialized)
- debugUtils.scala, CommonAliases.scala, SharedExtensions.scala

MID (some internal deps within Scala 3 baseline — meta + misc):
- meta/* (AdtMetadataCompanion, AllowDerivation, Fallback, MacroInstances, MetaMacros, MetadataCompanion, OptionLike, metaAnnotations, metadata)
- misc/* EXCEPT the 5 already-landed (port AnnotationOf, ApplierUnapplier, Bidirectional, BoxingUnboxing, Bytes, CaseMethods, CharSubSequence, Delegation, GraphUtils, Implicits, MiscAliases, MiscMacros, QuoteSupport, ScalaDurationExtensions, SealedUtils, SelfInstance, SimpleClassName, SourceInfo, Timestamp, TypeString, TypedMap, ValueEnum)
- misc/compat.scala — DO NOT port (deprecated + deferred-type imports)

HEAVY (CORE-01 derivation surface — port last):
- serialization/* EXCEPT cbor/ subdir:
  - Base64, DefaultCaseObjectInput, FieldValues
  - GenCodec, GenCodecCompat, GenCodecCreates, GenCodecDerivation, GenCodecFailures, GenCodecImpl, GenCodecJavaBuilder, GenCodecStructure, GenCodecUtils
  - GenKeyCodec, GenObjectCodec
  - GenRef, GenRefBuilder
  - HasGenCodec
  - IgnoreTransientDefaultMarker, InputOutput, PeekingObjectInput
  - SerializationMacros, SerializationName, SimpleValueInputOutput, StreamInputOutput
  - TransparentWrapperCompanion
  - customMarkerWrappers, defaultCase, flatten, outOfOrder, transientDefault, wrappers
  - json/* (JsonOptions, JsonStringInput, JsonStringOutput, JsonType, WrappedJson)
- tuples/TupleDerivation

SKIP entirely in Phase 5 (deferred to Phase 9/11):
- serialization/cbor/* (8 files)
- misc/compat.scala (deprecated)

ALREADY ON BRANCH (Phase 4 — DO NOT overwrite):
- misc/{Opt,NOpt,OptArg,OptRef}.scala
- serialization/madeAnnotationAliases.scala

This grouping is a HINT for batch order. Final boundaries are driven by the iterative compile — port a batch, run `++3.8.2 commons-core/compile`, look at the FIRST error, port the file it complains about, repeat.
</known_files>
</context>

<commit_strategy>
This plan produces MULTIPLE commits (not one big one) because the work is iterative. Acceptable commit shape:

```
feat(core): port Scala 3 annotations and basic utils
feat(core): port Scala 3 derivation + meta entry points
feat(core): port Scala 3 misc surface
feat(core): port Scala 3 serialization entry points (GenCodec/GenKeyCodec/GenObjectCodec)
feat(core): port Scala 3 serialization JSON support
fix(core): trim cbor/RPC references in ported sources
style(scalafmt): reformat ported Scala 3 sources
```

Concrete boundaries are at the executor's discretion based on the compile-iteration. Target ≤7 commits total. Each commit MUST leave `++2.13.18 commons-core/compile` green (regression guard). The final commit MUST leave `++3.8.2 commons-core/compile` green too.

NO GSD nomenclature anywhere. NO `.planning/` diff content.
</commit_strategy>

<tasks>

<task type="auto">
  <name>Task 1: Port leaf-and-mid Scala 3 sources (annotations, collection, concurrent, jiop, derivation, meta, misc)</name>
  <files>core/src/main/scala-3/com/avsystem/commons/{annotation,collection,concurrent,jiop,derivation,meta}/*.scala; core/src/main/scala-3/com/avsystem/commons/{CommonAliases,SharedExtensions,debugUtils}.scala; core/src/main/scala-3/com/avsystem/commons/misc/*.scala (except the 5 Phase 4 files and compat.scala); core/src/main/scala-3/com/avsystem/commons/tuples/TupleDerivation.scala</files>
  <action>
Iterative compile-driven port. For each file:

```sh
git checkout master -- <path>      # pull from master into working tree at scala-3/...
```

…then re-run `sbt '++3.8.2 commons-core/compile' 2>&1 | tail -30` and address the FIRST error.

Suggested batch order (port a batch, compile, fix, commit before next batch):

BATCH 1 — Annotations + top-level (no internal scala-3 deps):
- annotation/{AnnotationAggregate, AnnotationMacros, NotInheritedFromSealedTypes, TodoScala3Migration, atLeast, bincompat, defaultsToName, explicitGenerics, macroPrivate, positioned, showAst}.scala
- CommonAliases.scala, SharedExtensions.scala, debugUtils.scala

BATCH 2 — Collection + concurrent + jiop (stdlib + cats/monix deps only):
- collection/{CloseableIterator, CollectionAliases, MutableStack}.scala
- concurrent/{DurationPostfixConverters, ObservableExtensions, RetryStrategy, TaskExtensions, executionContexts, package}.scala
- jiop/{JBasicUtils, JCollectionUtils, JFactory}.scala

BATCH 3 — Derivation + meta:
- derivation/{AllowImplicitMacro, DeferredInstance, Materialized}.scala
- meta/{AdtMetadataCompanion, AllowDerivation, Fallback, MacroInstances, MetaMacros, MetadataCompanion, OptionLike, metaAnnotations, metadata}.scala

BATCH 4 — Misc surface (EXCEPT compat.scala and the 5 Phase 4 files):
- misc/{AnnotationOf, ApplierUnapplier, Bidirectional, BoxingUnboxing, Bytes, CaseMethods, CharSubSequence, Delegation, GraphUtils, Implicits, MiscAliases, MiscMacros, QuoteSupport, ScalaDurationExtensions, SealedUtils, SelfInstance, SimpleClassName, SourceInfo, Timestamp, TypeString, TypedMap, ValueEnum}.scala

BATCH 5 — Tuples:
- tuples/TupleDerivation.scala

For each batch:
1. `git checkout master -- <batch paths>`
2. `sbt '++3.8.2 commons-core/compile' 2>&1 | tail -50` — read FIRST error
3. If error is "cannot find cbor/RPC/mongo type X" in a ported file: trim that method/import from the file (keep the file minus the deferred-type reference). Document the trim in commit body.
4. If error is "cannot find type Y" where Y exists on master in a file we haven't yet ported: `git checkout master -- <Y's file>`, add to current batch.
5. If error is a scala-2-syntax error in a SHARED file (`core/src/main/scala/...`): that file should have moved in Plan 05-01 — abort and report (do NOT bypass with `@nowarn` — memory rule).
6. Loop until batch compiles. Then `sbt '++2.13.18 commons-core/compile'` (regression guard — MUST stay green; the 2.13 source set is unchanged so this should always pass).
7. `sbt scalafmtCheckAll` — if RED on newly-ported files: `sbt scalafmtAll`; commit reformat together with the port commit (or as a follow-up `style(scalafmt):` commit).
8. Commit: `git add core/src/main/scala-3/ &amp;&amp; git commit -m "feat(core): port Scala 3 &lt;batch-name&gt;" -m "&lt;brief body listing trims if any&gt;"`.

Hard rules:
- NEVER port `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala` (deprecated + deferred-type imports).
- NEVER port `core/src/main/scala-3/com/avsystem/commons/serialization/cbor/*` (Phase 11).
- NEVER overwrite Phase 4 files (Opt, NOpt, OptArg, OptRef, madeAnnotationAliases) — they're already correct.
- NEVER add `@nowarn` / `-Wconf`.
- `// format: off` around derivation/macro blocks OK if scalafmt mangles them.
- `.planning/` MUST NOT appear in any diff.
  </action>
  <verify>
    <automated>sbt '++2.13.18 commons-core/compile' 'set ThisBuild / scalaVersion := "3.8.2"' &gt;/dev/null 2>&amp;1 &amp;&amp; ! git diff 04-made-integration..HEAD -- 'core/src/main/scala-3/' | grep -E '(@nowarn|-Wconf)'</automated>
  </verify>
  <done>
- Annotations / collection / concurrent / jiop / derivation / meta / misc (sans compat.scala &amp; Phase 4 files) / tuples ported in batched commits.
- After each commit: `++2.13.18 commons-core/compile` green.
- After Task 1's final commit: `++3.8.2 commons-core/compile` MAY still be red (serialization surface in Task 2), but the error frontier has moved INTO `serialization/`.
- No `@nowarn` / `-Wconf` added.
- No commits touch cbor/ or compat.scala or the Phase 4 files.
  </done>
</task>

<task type="auto">
  <name>Task 2: Port serialization Scala 3 surface (sans cbor) until ++3.8.2 commons-core/compile is GREEN</name>
  <files>core/src/main/scala-3/com/avsystem/commons/serialization/*.scala (except cbor/ and madeAnnotationAliases.scala); core/src/main/scala-3/com/avsystem/commons/serialization/json/*.scala</files>
  <action>
This task closes CORE-01 (`GenCodec`/`GenKeyCodec`/`GenObjectCodec` entry points on Scala 3).

Suggested batch order (heaviest deps last):

BATCH 6 — Serialization base + simple I/O:
- serialization/{Base64, DefaultCaseObjectInput, FieldValues, InputOutput, PeekingObjectInput, SerializationName, IgnoreTransientDefaultMarker}.scala
- serialization/{customMarkerWrappers, defaultCase, flatten, outOfOrder, transientDefault, wrappers}.scala
- serialization/{SimpleValueInputOutput, StreamInputOutput}.scala
- serialization/SerializationMacros.scala
- serialization/TransparentWrapperCompanion.scala

BATCH 7 — GenCodec derivation surface:
- serialization/{GenCodec, GenCodecCompat, GenCodecCreates, GenCodecDerivation, GenCodecFailures, GenCodecImpl, GenCodecJavaBuilder, GenCodecStructure, GenCodecUtils}.scala
- serialization/{GenKeyCodec, GenObjectCodec}.scala
- serialization/HasGenCodec.scala

BATCH 8 — GenRef:
- serialization/{GenRef, GenRefBuilder}.scala

BATCH 9 — JSON:
- serialization/json/{JsonOptions, JsonStringInput, JsonStringOutput, JsonType, WrappedJson}.scala

For each batch follow the same iterative recipe as Task 1:
1. `git checkout master -- &lt;paths&gt;`
2. `sbt '++3.8.2 commons-core/compile' 2>&amp;1 | tail -50` — read FIRST error
3. If error references a cbor/mongo/RPC type:
   - If the reference is a single optional method/import in an OTHERWISE-portable file: trim the import + method body (replace with `// cbor support deferred to Phase 11` comment), record in commit body.
   - If the file is fundamentally cbor-specific: skip the file entirely (defer to Phase 11).
4. If error references a type that exists on master but isn't yet ported: pull it from master, add to batch.
5. If error is `core/src/main/scala/` syntax breakage: a file slipped through Plan 05-01 — STOP and report.
6. Regression guards each commit: `++2.13.18 commons-core/compile` AND `++3.8.2 commons-macros/compile`.
7. scalafmt as in Task 1.
8. Commit per batch.

Final check after Batch 9:
```sh
sbt '++3.8.2 commons-core/compile'    # MUST exit 0 — THE Phase 5 goal
sbt '++2.13.18 commons-core/compile'  # regression guard
sbt '++3.8.2 commons-macros/compile'  # Phase 3 guard
sbt scalafmtCheckAll                  # style guard
git grep -E '(@nowarn|-Wconf)' core/src/main/scala-3/ | grep -v 'core/src/main/scala-3' || true   # MUST be empty (no introductions)
```

If `++3.8.2 commons-core/compile` is still RED after porting every non-cbor scala-3 source:
1. Capture the residual error list with `sbt '++3.8.2 commons-core/compile' 2>&amp;1 | grep -E '\[error\]' &gt; /tmp/05-02-residual.txt`.
2. For each error, decide:
   - (a) Trim a non-essential method from the offending ported file (preferred when the error is a single method's signature mismatch against `made` 0.1.0 vs `0.1.1-SNAPSHOT`).
   - (b) Add a minimal scala-3-only stub in the offending file (NOT a `???`-stub — write the actual minimal implementation; QUALITY-02 forbids `???` in non-test source).
   - (c) Defer the file by removing it from the branch with `git rm` (last resort, drops a CORE-01 entry point — only acceptable for non-CORE-01-essential files like `GenRef` if it can move to Phase 6).
3. Loop until green.

ABSOLUTE STOP CONDITION: if after 3 iterations of (3) the goal is still red AND the residual errors all point to a single deep dependency (e.g., `Sam`/`SamCompanion` which we relocated to 2.13-only per QUALITY-DEPR-01), invoke the deprecated-skip memory rule: a Scala 3 caller of a deprecated 2.13-only API gets rewritten in-line to use the stdlib replacement OR the caller method is trimmed. Document the rewrite in commit body.

Hard rules (same as Task 1):
- NEVER port `serialization/cbor/*`.
- NEVER add `@nowarn` / `-Wconf`.
- NEVER use `???` in non-test source.
- NEVER touch `core/src/main/scala-2.13/` (those moved in 05-01 — Task 2 is scala-3/ only).
- NEVER touch the Phase 4 files (Opt/NOpt/OptArg/OptRef/madeAnnotationAliases) except to add a `given` companion if a downstream file requires it.
- `.planning/` MUST NOT appear in any diff.
  </action>
  <verify>
    <automated>sbt '++3.8.2 commons-core/compile' &gt;/dev/null 2>&amp;1 &amp;&amp; sbt '++2.13.18 commons-core/compile' &gt;/dev/null 2>&amp;1 &amp;&amp; sbt '++3.8.2 commons-macros/compile' &gt;/dev/null 2>&amp;1 &amp;&amp; sbt scalafmtCheckAll &gt;/dev/null 2>&amp;1</automated>
  </verify>
  <done>
- ++3.8.2 commons-core/compile exits 0 — Phase 5 goal achieved.
- ++2.13.18 commons-core/compile exits 0 (regression guard).
- ++3.8.2 commons-macros/compile exits 0 (Phase 3 guard).
- scalafmtCheckAll exits 0.
- GenCodec.scala, GenKeyCodec.scala, GenObjectCodec.scala exist under `core/src/main/scala-3/com/avsystem/commons/serialization/`.
- No `@nowarn` / `-Wconf` / `???` introduced.
- cbor/ subdir not touched.
- compat.scala not ported.
- Phase 4 wiring primitives byte-identical to start of plan (`git diff 04-made-integration..HEAD -- core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala` empty, same for NOpt/OptArg/OptRef and madeAnnotationAliases).
- Multiple commits, all with conventional-commit messages, no GSD nomenclature, `.planning/` not in any diff.
  </done>
</task>

</tasks>

<verification>
After Task 2:
1. THE goal: `sbt '++3.8.2 commons-core/compile'` exits 0.
2. Regression: `sbt '++2.13.18 commons-core/compile'` exits 0.
3. Macros guard: `sbt '++3.8.2 commons-macros/compile'` exits 0.
4. Style: `sbt scalafmtCheckAll` exits 0.
5. CORE-01 entry points exist: `test -f core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala &amp;&amp; test -f core/src/main/scala-3/com/avsystem/commons/serialization/GenKeyCodec.scala &amp;&amp; test -f core/src/main/scala-3/com/avsystem/commons/serialization/GenObjectCodec.scala`.
6. Memory rules honored: `git diff 04-made-integration..HEAD | grep -E '(@nowarn|-Wconf)'` empty; `git diff 04-made-integration..HEAD | grep -E '\\?\\?\\?'` empty (no `???` stubs).
7. Phase 4 untouched: `git diff 04-made-integration..HEAD -- core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala` empty.
8. cbor untouched: `git diff 04-made-integration..HEAD -- core/src/main/scala-3/com/avsystem/commons/serialization/cbor/` empty.
9. `.planning/` clean: `git diff 04-made-integration..HEAD -- .planning/` empty.
</verification>

<success_criteria>
- THE goal achieved: `++3.8.2 commons-core/compile` GREEN for the first time in the project's life.
- CORE-01 entry points (GenCodec, GenKeyCodec, GenObjectCodec) ported.
- CORE-02 source organization stabilized (scala-3/ contains ported Scala 3 baseline; scala/ contains shared; scala-2.13/ contains 2.13-only — three clean partitions).
- All regression guards green.
- No memory-rule violations.
- Multiple coherent commits on branch.
</success_criteria>

<output>
After completion, create `.planning/phases/05-core-scala-3-baseline-port/05-02-cherry-pick-scala-3-sources-SUMMARY.md`. Record:
- Count of files cherry-picked (per batch).
- Files DEFERRED (skipped to later phases) with rationale.
- In-file trims (which deferred-type methods/imports were removed and from which file).
- Final ++3.8.2 commons-core error count: 0.
- Per-batch commit SHAs.
- Any `// format: off` blocks introduced.
- Branch tip SHA.
</output>
