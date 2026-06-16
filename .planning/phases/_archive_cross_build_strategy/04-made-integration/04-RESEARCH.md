# Phase 4: `made` integration - Research

**Researched:** 2026-05-30
**Domain:** Scala 3 build wiring + minimum-island source port of `made`-dependent wiring primitives
**Confidence:** HIGH

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions
- `madeVersion = "0.1.0"` (downgrade from fork master's `"0.1.1-SNAPSHOT"`). No SNAPSHOT resolver added.
- Conditional dep keyed on `scalaBinaryVersion.value == "3"`. Apply to `core` (jvm) and `core-js` ONLY. NOT `cbor` / `mongo` this phase.
- No new sbt plugins. No resolver changes.
- Source files to port (verbatim if possible) from fork master `core/src/main/scala-3/`:
  - `com/avsystem/commons/misc/Opt.scala`
  - `com/avsystem/commons/misc/NOpt.scala`
  - `com/avsystem/commons/misc/OptArg.scala`
  - `com/avsystem/commons/misc/OptRef.scala`
  - `com/avsystem/commons/misc/madeAnnotationAliases.scala` (or wherever the alias object is declared — may need extraction).
- DO NOT port (deferred): `GenCodec*`, `GenObjectCodec*`, `GenKeyCodec*`, `GenRef*`, `HasGenCodec*`, `TransparentWrapperCompanion`, `flatten.scala`, `defaultCase.scala`, `transientDefault.scala`, `SerializationName.scala`, `cbor/CborAdtMetadata`.
- If any `Opt*.scala` file transitively needs `GenCodec` or similar, EITHER strip that section OR escalate the file's port to a later phase. Goal: minimum island.
- Alias object surface stays SMALL — only annotations used by ported files; rest deferred.
- Compile gates: `sbt '++3 core/compile'` green, `sbt '++2.13 core/compile'` green, `sbt '++3 core-js/compile'` green. No tests added (Phase 7).
- MIGRATION.md updates: add `made` row above `core`; append `core` Notes column with "made wiring primitives ported; full derivation pending"; `core` status stays `wip`.
- Skip `@deprecated` symbols from fork master with stdlib/library replacements.
- PR workflow: human-ack push + PR. Base `AVSystem/scala-commons:scala-3`. Branch `04-made-integration`.

### Claude's Discretion
- Exact partitioning when a single fork-master file mixes wiring primitives with deferred derivation — planner can extract minimal subsets.
- Whether to ship `madeAnnotationAliases.scala` as a NEW file (cleaner) or inline aliases. Default: new file.
- Whether `cbor` / `mongo` also need the conditional dep added now (probably no — they will pull it in via their own phases).

### Deferred Ideas (OUT OF SCOPE)
- Full `GenCodec` Scala 3 port — Phase 5.
- `cbor` derivation refresh — Phase 11.
- `mongo` derivation refresh — Phase 9.
- Bumping `made` to a future stable — backlog.
- Adding `madeAnnotationAliases` for the FULL annotation surface — backlog.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| MADE-01 | `made` integration code (annotation aliases, derivation hooks, `Default[Opt/NOpt/OptArg/OptRef]`) ported to Scala 3 side of relevant modules | This phase ports the minimum wiring slice — `Default[Opt/NOpt/OptArg/OptRef]` givens carried inside the four `Opt*.scala` companion objects, plus a small `madeAnnotationAliases.scala` re-export object. Full derivation hooks (Made / GenCodec) deferred to Phase 5. |
| INFRA-06 (re-affirmed) | `made` library (Scala-3-only) pinned to `0.1.0` (published release) | Bump `madeVersion` constant from `"0.1.1-SNAPSHOT"` to `"0.1.0"`. Already wired conditionally in `core` (lines 327–330) and `core-js` (lines 348–351); no resolver change. Cellar lookup confirms 0.1.0 publishes all symbols the ported files use. |
</phase_requirements>

## Summary

Phase 4 lands a minimum source island plus a one-line build constant change. The fork-master state already has the four `Opt*.scala` files authored with the exact wiring this phase needs; the file set has only one external dependency on `made` (the `Default` trait) and no transitive dependency on `GenCodec`/`GenObjectCodec`/`GenKeyCodec`/`GenRef`. The single non-trivial decision is **how to handle `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala`**, which is mixed into `object Opt`, `object NOpt`, `object OptRef` (via `extends OptCompat`/`NOptCompat`/`OptRefCompat`) AND contains unrelated traits referencing the deferred `GenCodec` / `GenKeyCodec` types. The planner MUST ship a stripped `compat.scala` (only the three `Opt*Compat` traits) — full port returns in Phase 5/6.

`made` 0.1.0 publishes every symbol the ported file set references (`made.Default`, `made.TransparentWrapping`, `made.annotation.{generated,name,optionalParam,transparent,whenAbsent}`), verified via `cellar get-external`. No source adaptation is required when downgrading from `0.1.1-SNAPSHOT` to `0.1.0` for these files. The build constant flips, snapshot resolver — there is none — stays absent, and the Phase 1 source-dir helper (`mkSourceDirs`) routes the new `scala-3/` files automatically.

**Primary recommendation:** Port the four `Opt*.scala` files verbatim from this branch, ship a stripped `compat.scala` containing only `OptCompat`/`NOptCompat`/`OptRefCompat`, port the existing `serialization/madeAnnotationAliases.scala` verbatim (no `GenCodec` exposure), and bump `madeVersion` to `"0.1.0"` in `build.sbt`. No source-level adaptation needed.

## Concrete File List

### Files to port (5 source files + 1 build.sbt edit)

| # | Path | State | Action |
|---|------|-------|--------|
| 1 | `core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala` | Already authored on this branch | Port verbatim |
| 2 | `core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala` | Already authored | Port verbatim |
| 3 | `core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala` | Already authored | Port verbatim |
| 4 | `core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala` | Already authored | Port verbatim |
| 5 | `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala` | Already authored — **prune required** | Strip to only `OptCompat`, `NOptCompat`, `OptRefCompat` traits |
| 6 | `core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala` | Already authored | Port verbatim — 5 export lines, no deferred-type deps |
| - | `build.sbt` line 27 | `val madeVersion = "0.1.1-SNAPSHOT"` | Change to `val madeVersion = "0.1.0"` |

### Files NOT to port (deferred)

The grep `grep -rln "import made" core/src/main/scala-3/` returns 23 files. Beyond the 6 above, all the rest belong to derivation surface (`GenCodec*`, `GenObjectCodec`, `GenKeyCodec`, `GenRef*`, `HasGenCodec`, `TransparentWrapperCompanion`, `flatten.scala`, `defaultCase.scala`, `transientDefault.scala`, `SerializationName.scala`, `meta/metadata.scala`, `cbor/CborAdtMetadata.scala`). All defer to Phase 5 (CORE-01) or later.

### Optional: `MiscAliases.scala`

`core/src/main/scala-3/com/avsystem/commons/misc/MiscAliases.scala` exists on this branch and re-exports `Opt`/`OptArg`/`NOpt`/`OptRef` into a package-level alias object. It does NOT import from `made`. **Recommendation: defer to Phase 5** (where other consumers land). Phase 4 does not need it — the Opt classes resolve at their canonical paths.

## Per-File Import Audit

All imports under `core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala`:

| File | Imports | References deferred types? |
|------|---------|---------------------------|
| `Opt.scala` | `made.Default`, `scala.annotation.publicInBinary` | NO |
| `NOpt.scala` | `made.Default`, `scala.annotation.publicInBinary` | NO |
| `OptArg.scala` | `made.Default`, `scala.annotation.{publicInBinary, targetName}`, `scala.language.implicitConversions` | NO |
| `OptRef.scala` | `made.Default`, `scala.annotation.publicInBinary` | NO |
| `serialization/madeAnnotationAliases.scala` | (none — only `export made.annotation.*` / `export made.TransparentWrapping`) | NO |
| `misc/compat.scala` (as-is) | `com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}` | **YES — must prune** |

`compat.scala` is the only ported file with a deferred-type dependency; the dependency is confined to four trait blocks (`TypeStringCompat`, `JavaClassNameCompat`, `NamedEnumCompanionCompat`, `ValueEnumCompanionCompat`) that have no relation to the Opt wiring.

## Required Pruning — `misc/compat.scala`

**KEEP** (these three traits are mixed into `object Opt extends OptCompat`, `object NOpt extends NOptCompat`, `object OptRef extends OptRefCompat`):

- `trait OptCompat { this: Opt.type => def opt2Iterable[A](opt: Opt[A]): Iterable[A] = summon[Conversion[Opt[A], Iterable[A]]](opt) }` (lines 50–53)
- `trait NOptCompat { this: NOpt.type => ... }` (lines 55–58)
- `trait OptRefCompat { this: OptRef.type => ... }` (lines 60–63)

Each is annotated `@deprecated("Use given Conversion directly", since = "3.0.0")`. Per memory `feedback_dont_port_deprecated.md`, the user's policy is to skip `@deprecated` scala-2 APIs with stdlib replacements. These three traits' single methods are deprecated source-compat shims pointing at the new `given Conversion[…, Iterable[…]]` already declared in each companion. **Recommendation: DROP all three `opt2Iterable` deprecated methods and remove the `extends OptCompat`/`NOptCompat`/`OptRefCompat` clauses from `object Opt`, `object NOpt`, `object OptRef`.** This removes `compat.scala` entirely from Phase 4 scope — the four Opt files become truly self-contained, the deferred `GenCodec`/`GenKeyCodec` reference disappears, and the deprecation-policy rule is honored.

**Fallback (if reviewer requests source-compat for `opt2Iterable`):** ship a stripped `compat.scala` containing ONLY the three `Opt*Compat` traits (delete `BoxingCompat`, `LowPrioBoxingCompat`, `UnboxingCompat`, `LowPrioUnboxingCompat`, `TimestampCompat`, `TypeStringCompat`, `JavaClassNameCompat`, `NamedEnumCompanionCompat`, `OrderedEnumCompat`, `ValueEnumCompanionCompat` — these reference deferred types or deferred companions). The kept traits have no `made` or `GenCodec` references.

**Cross-check:** None of the four `Opt*.scala` files reference any other trait from `compat.scala`. `OptArg.scala` already does not extend any compat trait, so it is unaffected either way.

### Annotation-alias subset

`serialization/madeAnnotationAliases.scala` re-exports exactly the five annotations published by `made` 0.1.0 plus `TransparentWrapping`:

```scala
export made.annotation.generated
export made.annotation.name
export made.annotation.optionalParam
export made.annotation.transparent
export made.annotation.whenAbsent
export made.TransparentWrapping
```

All six symbols verified present at 0.1.0 (see `cellar list-external io.github.halotukozak:made_3:0.1.0 made.annotation` and `cellar get-external io.github.halotukozak:made_3:0.1.0 made.TransparentWrapping`). None of the five `Opt*.scala` files reference these aliases, so this file is logically separate; ship it verbatim — it's already a minimum-island file, and Phase 4 scope mentions it as required.

## `made` 0.1.0 Signature Verification

Verified via `cellar get-external io.github.halotukozak:made_3:0.1.0 …` (Sonatype Central — `io.github.halotukozak:made_3:0.1.0`).

| Symbol Used | 0.1.0 Signature | Used By | Match? |
|-------------|----------------|---------|--------|
| `made.Default` | `trait Default[O] extends Function0[O]` (sealed-ish `extends (() => O)`) | `Opt.scala` line 50–51, `NOpt.scala` line 43–44, `OptArg.scala` line 36–37, `OptRef.scala` line 35–36 — `() => Opt.Empty.asInstanceOf[AnyRef]` (SAM via Function0) | ✅ |
| `made.TransparentWrapping` | `trait TransparentWrapping[R, T] { def wrap(r: R): T; def unwrap(t: T): R }` | `madeAnnotationAliases.scala` line 15 (`export made.TransparentWrapping`) | ✅ |
| `made.annotation.generated` | `class generated extends MetaAnnotation` | `madeAnnotationAliases.scala` line 10 | ✅ |
| `made.annotation.name` | `class name extends Annotation with RefiningAnnotation` | `madeAnnotationAliases.scala` line 11 | ✅ |
| `made.annotation.optionalParam` | `class optionalParam extends MetaAnnotation` | `madeAnnotationAliases.scala` line 12 | ✅ |
| `made.annotation.transparent` | `class transparent extends MetaAnnotation` | `madeAnnotationAliases.scala` line 13 | ✅ |
| `made.annotation.whenAbsent` | `class whenAbsent[+T] extends MetaAnnotation` | `madeAnnotationAliases.scala` line 14 | ✅ |

**No deviation between 0.1.0 and 0.1.1-SNAPSHOT for the Phase 4 file set.** The downgrade is signature-clean — zero per-file adaptation. (Compare cellar source dump: `made/Default.scala` line 13 — `trait Default[O] extends (() => O)`. The `Function0[O]` and `(() => O)` are syntactically interchangeable.)

## `build.sbt` Changes

**Exactly one line of `build.sbt` changes:**

```diff
- val madeVersion = "0.1.1-SNAPSHOT"
+ val madeVersion = "0.1.0"
```

Located at `build.sbt` line 27. Conditional wiring blocks at lines 326–331 (`core`) and lines 349–353 (`core-js`) are already present and idiomatic — **do not touch them**. No `Resolver.sonatypeOssRepos("snapshots")` exists in the build; no resolver edits needed. `mongo` (lines 357–373) and `cbor` are unaffected per CONTEXT decision (deferred).

**Verification:**

```bash
grep -n "madeVersion\|Resolver.*[Ss]napshot\|sonatype.*snapshot" build.sbt
```
Expected after edit: 3 lines (declaration + 2 dep uses). No snapshot resolver.

## Downstream Consumers (later-phase scope)

Files in `core/src/main/scala-3/` that import `Opt`/`NOpt`/`OptArg`/`OptRef` — these are deferred consumers, NOT touched in Phase 4 (only listed for traceability into Phase 5/6/7/11):

| File | Phase |
|------|-------|
| `com/avsystem/commons/misc/MiscAliases.scala` | 5 (CORE-01) |
| `com/avsystem/commons/SharedExtensions.scala` | 5 / 6 |
| `com/avsystem/commons/misc/QuoteSupport.scala` | 5 / 6 |
| `com/avsystem/commons/misc/Timestamp.scala` | 5 / 6 |
| `com/avsystem/commons/misc/TypedMap.scala` | 5 / 6 |
| `com/avsystem/commons/misc/MiscMacros.scala` | 5 / 6 |
| `com/avsystem/commons/misc/AnnotationOf.scala` | 5 / 6 |
| `com/avsystem/commons/serialization/wrappers.scala` | 5 (CORE-01) |
| `com/avsystem/commons/serialization/PeekingObjectInput.scala` | 5 |
| `com/avsystem/commons/serialization/SerializationMacros.scala` | 5 |
| `com/avsystem/commons/serialization/GenRefBuilder.scala` | 5 (uses `import com.avsystem.commons.misc.{Opt, OptArg, OptRef}`) |
| `com/avsystem/commons/serialization/FieldValues.scala` | 5 |
| `com/avsystem/commons/serialization/GenCodecFailures.scala` | 5 |
| `com/avsystem/commons/serialization/GenCodecStructure.scala` | 5 |
| `com/avsystem/commons/serialization/GenCodecImpl.scala` | 5 |
| `com/avsystem/commons/serialization/GenCodec.scala` | 5 |
| `com/avsystem/commons/serialization/StreamInputOutput.scala` | 5 |
| `com/avsystem/commons/serialization/SimpleValueInputOutput.scala` | 5 |
| `com/avsystem/commons/serialization/InputOutput.scala` | 5 |
| `com/avsystem/commons/serialization/customMarkerWrappers.scala` | 5 |
| `com/avsystem/commons/serialization/json/JsonOptions.scala` | 5 / 8 |
| `com/avsystem/commons/serialization/GenCodecDerivation.scala` | 5 |
| `com/avsystem/commons/serialization/cbor/CborKeyCodec.scala` | 11 |
| `com/avsystem/commons/serialization/cbor/definitions.scala` | 11 |
| `com/avsystem/commons/serialization/cbor/CborInput.scala` | 11 |
| `com/avsystem/commons/serialization/cbor/CborAdtMetadata.scala` | 11 |
| `com/avsystem/commons/serialization/cbor/CborOptimizedCodecs.scala` | 11 |
| `com/avsystem/commons/collection/MutableStack.scala` | 5 / 6 |
| `com/avsystem/commons/meta/metaAnnotations.scala` | 5 |
| `com/avsystem/commons/meta/metadata.scala` | 5 |

(Test consumers under `core/src/test/scala-3/` defer to Phase 7 per CORE-03.)

This list is the planner's traceability data — Phase 5 picks up CORE-01 with this set already known to compile against the wiring primitives Phase 4 lands.

## Standard Stack

### Core

| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| `io.github.halotukozak:made_3` | `0.1.0` | Provides `Default[O]` typeclass for empty-instance wiring + annotation surface (`optionalParam`, `name`, `whenAbsent`, `transparent`, `generated`) + `TransparentWrapping[R, T]` | Single source of truth for this project's Scala 3 derivation toolkit — replaces hand-written macro typeclasses that lived in `macros/` on Scala 2.13 |

### Version Verification

```bash
# Confirmed resolvable on Sonatype Central — cellar fetched the artifact:
cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default
# → returns Default trait with companion givens: given_Default_Option, given_Default_|
```

Publish date is checked transitively (cellar succeeds without resolver overrides → published on Maven Central / Sonatype Central). 0.1.1-SNAPSHOT on fork master is a local artifact NOT to land on upstream.

### Alternatives Considered

| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `made 0.1.0` | `made 0.1.1-SNAPSHOT` | Snapshot is local-dev only; upstream forbids. |
| `made.Default` | Hand-roll `trait Default[O] { def value: O }` per-project | Adds duplication, deviates from upstream story where `made` is the derivation toolkit; the wiring primitives become incompatible with Phase 5's full derivation port. Rejected. |

**Installation:** Already present at `build.sbt` lines 327–331 and 349–353. Only edit needed:

```diff
- val madeVersion = "0.1.1-SNAPSHOT"
+ val madeVersion = "0.1.0"
```

## Architecture Patterns

### Recommended Project Structure (Phase 4 slice)

```
core/src/main/scala-3/com/avsystem/commons/
├── misc/
│   ├── Opt.scala               # port verbatim
│   ├── NOpt.scala              # port verbatim
│   ├── OptArg.scala            # port verbatim
│   ├── OptRef.scala            # port verbatim
│   └── compat.scala            # OPTIONAL — only if deprecated opt2Iterable shims are kept
└── serialization/
    └── madeAnnotationAliases.scala   # port verbatim — 6 lines of `export`
```

### Pattern 1: Value-class `Default` instance via cast through `Default[AnyRef]`

**What:** Each `Opt*` companion provides a `given Default[Opt[A]]` by constructing a `Default[AnyRef]` SAM and casting. Avoids the erased bridge clash on value classes (`Opt extends AnyVal`).

**When to use:** Whenever you need a `made.Default[X]` for a value-class wrapper.

**Example (from `Opt.scala` lines 49–51, verbatim):**

```scala
// Cast through Default[AnyRef] to avoid erased bridge clash on value class Opt.
private val emptyDefault: Default[AnyRef] = () => Opt.Empty.asInstanceOf[AnyRef]
given [A] => Default[Opt[A]] = emptyDefault.asInstanceOf[Default[Opt[A]]]
```

**Why it works:** `made.Default[O] extends () => O` is a SAM type. The lambda `() => Opt.Empty.asInstanceOf[AnyRef]` produces a `Function0[AnyRef]` which the Scala 3 compiler upcasts. The double-cast bypasses the value-class bridge generation issue.

### Pattern 2: Annotation re-export via `export`

**What:** `madeAnnotationAliases.scala` does NOT import; it uses top-level `export` to introduce the made annotations into the `com.avsystem.commons.serialization` namespace.

**Example (full file, 16 lines):**

```scala
package com.avsystem.commons
package serialization

export made.annotation.generated
export made.annotation.name
export made.annotation.optionalParam
export made.annotation.transparent
export made.annotation.whenAbsent
export made.TransparentWrapping
```

**When to use:** Source-compat layer for upstream-named annotations that physically live in another library. Avoids `type alias` and `val` indirection that would obscure annotation semantics.

### Anti-Patterns to Avoid

- **Porting `compat.scala` verbatim:** Pulls in `import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}` (line 4) — both deferred types. Compile fails. Strip first.
- **Inlining `Default` givens into a separate `given` package:** the value-class cast pattern needs to live in the companion object so the inline factory methods (`Opt.empty`, `OptArg.Empty`) are visible. Keep givens inline.
- **Adding `made` to `mongo` / `cbor` this phase:** out of scope per CONTEXT — `mongo` and `cbor` haven't yet had their Scala 3 sources land, so the dep would be a no-op build wiring change with no consumer code. Wait for the dedicated phases.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Empty-value typeclass for `Opt`/`NOpt`/`OptArg`/`OptRef` | Custom `trait Default[T] { def empty: T }` | `made.Default` | Phase 5's full derivation reads `made.Default` resolutions during synthesis. A parallel type wouldn't be discovered. |
| Annotation re-export | Re-define `class optionalParam extends StaticAnnotation` etc. | `export made.annotation.optionalParam` (top-level export) | Re-defining loses `MetaAnnotation` parent; made's derivation machinery checks for that parent. |
| Source-compat shims for deprecated `opt2Iterable` | Keep `compat.scala`'s `OptCompat` traits to preserve old signatures | Memory rule `feedback_dont_port_deprecated.md`: skip @deprecated symbols with stdlib/library replacements | The `given Conversion[Opt[A], Iterable[A]]` already in `object Opt` is the replacement. |

**Key insight:** The Phase 4 island is dictated by `made`'s shape: any wiring that the rest of `core` consumes (`Default` typeclass, the 5 annotations, `TransparentWrapping`) MUST resolve to `made.*` types so that Phase 5's derivation works. Re-defining locally is a trap.

## Common Pitfalls

### Pitfall 1: `compat.scala` brings in deferred `GenCodec` / `GenKeyCodec`

**What goes wrong:** `compat.scala` line 4 reads `import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}`. If you port the whole file, `++3 core/compile` fails because `GenCodec` / `GenKeyCodec` files are not part of Phase 4.

**Why it happens:** The fork-master file groups all Scala 3 source-compat shims into one location. Phase 4 only needs the three `Opt*Compat` traits; the rest reference types that live in deferred files.

**How to avoid:** Either (a) drop `extends OptCompat`/`NOptCompat`/`OptRefCompat` and skip `compat.scala` entirely (recommended — deprecated method policy), or (b) ship a stripped `compat.scala` with only the three `Opt*Compat` traits.

**Warning signs:** `sbt '++3 core/compile'` error mentions `object serialization is not a member of …` or `not found: type GenCodec`.

### Pitfall 2: Snapshot resolver leakage

**What goes wrong:** Some local checkouts have a `~/.sbt/1.0/global.sbt` adding `Resolver.sonatypeOssRepos("snapshots")`. CI lacks it — so a stray `madeVersion = "0.1.1-SNAPSHOT"` works locally and fails CI.

**Why it happens:** Easy oversight when copying fork-master state.

**How to avoid:** Always grep `build.sbt` for the literal `"-SNAPSHOT"` before push. Phase 1 already confirmed `0.1.0` resolves clean.

**Warning signs:** CI logs say `unresolved dependency: io.github.halotukozak#made_3;0.1.1-SNAPSHOT`.

### Pitfall 3: `made.Default` SAM vs explicit trait impl

**What goes wrong:** Scala 3's SAM conversion for `Default[O] extends Function0[O]` requires `Default` to be a `trait` (it is). If `made` ever changes to abstract class, the `() => Opt.Empty.asInstanceOf[AnyRef]` lambda no longer auto-converts.

**Why it happens:** Library churn.

**How to avoid:** Pin `madeVersion = "0.1.0"` (locked decision). Cellar verified `trait Default[O] extends Function0[O]` at this version.

**Warning signs:** Compile error `expression of type () => AnyRef does not conform to type Default[AnyRef]`.

### Pitfall 4: Cross-build noise on Scala 2.13

**What goes wrong:** Adding `scala-3/` source under a cross-built module sometimes triggers `++2.13 core/compile` failures if a 2.13 file accidentally imports a 3-only type.

**Why it happens:** Phase 4 only touches `scala-3/` — no risk in this slice. Listed for completeness.

**How to avoid:** Phase 1's `mkSourceDirs` helper routes `scala-3/` files only to Scala 3 builds. Verify with `sbt '++2.13 core/compile'` — must stay green.

**Warning signs:** Compile errors on `++2.13` after a Scala-3-only change.

## Code Examples

### Default given for a value-class wrapper

```scala
// Source: core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala lines 1–5, 49–51
package com.avsystem.commons.misc

import made.Default

import scala.annotation.publicInBinary

object Opt extends OptCompat {
  // ...
  // Cast through Default[AnyRef] to avoid erased bridge clash on value class Opt.
  private val emptyDefault: Default[AnyRef] = () => Opt.Empty.asInstanceOf[AnyRef]
  given [A] => Default[Opt[A]] = emptyDefault.asInstanceOf[Default[Opt[A]]]
}
```

### Annotation re-export

```scala
// Source: core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala (full file)
package com.avsystem.commons
package serialization

export made.annotation.generated
export made.annotation.name
export made.annotation.optionalParam
export made.annotation.transparent
export made.annotation.whenAbsent
export made.TransparentWrapping
```

### Build wiring (already present — verbatim from `build.sbt` lines 326–331)

```scala
libraryDependencies ++= {
  // `made` is Scala 3 only.
  if (scalaBinaryVersion.value == "3")
    Seq("io.github.halotukozak" %% "made" % madeVersion)
  else Seq.empty
},
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Hand-rolled `@whenAbsent`/`@optionalParam` annotations in `core/serialization/` | `export made.annotation.*` via top-level `export` | This branch (fork master, 2026) | Removes duplication; one source of truth for the annotation contract used by derivation. |
| Macro-driven `Default[T]` per-type givens in `macros/` (Scala 2) | `made.Default` library-provided typeclass + per-companion `given Default[Wrapper[A]]` | Scala 3 port | Macros are inlined into the `made` library; project code only constructs given instances. |
| Snapshot `madeVersion = "0.1.1-SNAPSHOT"` | Pinned release `madeVersion = "0.1.0"` | Phase 4 (this PR) | Removes snapshot-resolver requirement; CI-clean. |

**Deprecated/outdated (do NOT port):**
- `opt2Iterable[A]` helper methods in `compat.scala` — replaced by `given Conversion[Opt[A], Iterable[A]] = _.toList` already present in each companion.
- Most other traits in `compat.scala` reference Phase 5+ types and are not Phase 4 concern.

## Open Questions

1. **Should `MiscAliases.scala` ship in Phase 4?**
   - What we know: It only `export`s `Opt`/`NOpt`/`OptArg`/`OptRef`. No `made` import.
   - What's unclear: Whether any Phase 4 acceptance criterion needs the `import com.avsystem.commons.misc.*` package-level aliases.
   - Recommendation: defer to Phase 5 (where downstream consumers via `SharedExtensions.scala`, `GenRefBuilder.scala` etc. land). Ports cleaner when its consumers exist.

2. **Should `extends OptCompat` (and friends) be dropped from companions, or should a stripped `compat.scala` ship?**
   - What we know: The traits provide a single deprecated `opt2Iterable` shim each.
   - What's unclear: Whether downstream consumers in the wider AVSystem ecosystem still call `Opt.opt2Iterable(x)` rather than the implicit `Conversion`.
   - Recommendation: drop per `feedback_dont_port_deprecated.md`. If a Phase-7 test discovers a remaining call, restore the shim in Phase 7 alongside the test.

3. **Does `++2.13 core/compile` succeed on this branch today?**
   - What we know: Recent commit `bcc3bcbf` isolated 2.13-only modules; recent `52095491` fixed scala-3 compile. The branch is in active migration state.
   - What's unclear: Whether the 2.13 source side has parallel `Opt`/`NOpt`/`OptArg`/`OptRef` definitions that would now clash with the Scala 3 sources.
   - Resolution: The `core/src/main/scala-2.13/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala` files exist (verified via `grep` — these are the 2.13 originals). They are routed to 2.13 builds only via `mkSourceDirs`. No conflict expected — planner verifies with the 2.13 gate command below.

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | scalatest 3.2.19 + scalacheck 1.19 (declared in `commonSettings`) — but **NO test sources added in Phase 4** |
| Config file | Per-module sbt build; no separate framework config file |
| Quick run command | `sbt '++3.8.2 core/compile'` (compile is the acceptance gate for this phase) |
| Full suite command | `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll` (the 5 gates from REQ INFRA-07) |

Phase 4 is compile-only: per CONTEXT, "No tests added in Phase 4; tests come in Phase 7 (CORE — tests revival)." The validation strategy is therefore **compile + lint gates**, not unit tests.

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|--------------|
| MADE-01 | `Default[Opt[_]]`, `Default[NOpt[_]]`, `Default[OptArg[_]]`, `Default[OptRef[_]]` resolve via `summon` on Scala 3 | compile-time (typer) | `sbt '++3.8.2 core/compile'` | ✅ (sbt) |
| MADE-01 | `Default[Opt[_]]` etc. NOT visible on Scala 2.13 (because `made` is Scala-3-only dep) | compile-time | `sbt '++2.13 core/compile'` | ✅ (sbt) |
| MADE-01 | `core-js` Scala 3 build picks up the same `Default` givens | compile-time | `sbt '++3.8.2 core-js/compile'` | ✅ (sbt) |
| MADE-01 | Annotation re-exports (`generated`, `name`, `optionalParam`, `transparent`, `whenAbsent`, `TransparentWrapping`) resolve at `com.avsystem.commons.serialization` namespace on Scala 3 | compile-time | included in `++3.8.2 core/compile` (file is in the compile graph) | ✅ |
| INFRA-06 | `madeVersion = "0.1.0"`; no snapshot resolver | grep check | `! grep -q SNAPSHOT build.sbt project/plugins.sbt` | ✅ (sbt) |
| INFRA-06 | `made` dep present on Scala 3 only for `core` and `core-js` | dependency check | `sbt '++3.8.2 core/dependencyTree' \| grep "made_3"` (non-empty); `sbt '++2.13 core/dependencyTree' \| grep "made_2.13"` (empty) | ✅ (sbt) |
| (style) | All ported files conform to scalafmt 3.11.1 | format check | `sbt scalafmtCheckAll` | ✅ (sbt) |
| (CI) | Full 5-gate CI suite still green | CI matrix | `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll` | ✅ (sbt) |

### Sampling Rate

- **Per task commit:** `sbt '++3.8.2 core/compile'` (fast: only Scala 3 core)
- **Per wave merge:** `sbt '++3.8.2 core/compile' '++2.13 core/compile' '++3.8.2 core-js/compile' scalafmtCheckAll`
- **Phase gate (before `/gsd:verify-work` and PR push):** full 5-gate suite — `sbt '+jvm/test' '+jvm2/test' '+js/test' '++2.13 mimaReportBinaryIssues' scalafmtCheckAll`

### Wave 0 Gaps

None — sbt is the test/lint framework, already installed and configured by Phase 1. No new test files are required for Phase 4 (per CONTEXT decision). No conftest/fixture files apply (Scala project, scalatest-based — used in later phases).

## Deviations Between Fork Master's `made` Use and `made` 0.1.0

For the **Phase 4 file set only**, there are **zero deviations**: every `made.*` symbol referenced by `Opt.scala`, `NOpt.scala`, `OptArg.scala`, `OptRef.scala`, `madeAnnotationAliases.scala` is published at 0.1.0 with the signature the source expects. Verified via:

```
cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default
cellar get-external io.github.halotukozak:made_3:0.1.0 made.TransparentWrapping
cellar list-external io.github.halotukozak:made_3:0.1.0 made.annotation
```

**Deviations exist** at `0.1.0` for `made.*` symbols used by deferred files (e.g. `made.Made`, `made.MadeElem`, `made.MadeFieldElem`, `made.MadeSubElem`, `made.GeneratedMadeElem`, `made.MacroUtils` — all present at 0.1.0 per `cellar list-external`, but those files are OUT OF SCOPE for Phase 4). Phase 5 must repeat this verification for its file set.

**Blockers the planner must address:**

1. **`compat.scala` references deferred types** — handle per "Required Pruning" section above (recommendation: drop `extends *Compat` from companions; ship without the file). This is the only Phase 4 blocker.

No other blockers identified.

## Sources

### Primary (HIGH confidence)

- `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` — `trait Default[O] extends Function0[O]`
- `cellar get-external io.github.halotukozak:made_3:0.1.0 made.TransparentWrapping` — `trait TransparentWrapping[R, T] { def wrap(r: R): T; def unwrap(t: T): R }`
- `cellar list-external io.github.halotukozak:made_3:0.1.0 made` — full package listing (Default, TransparentWrapping, Made, MadeElem family, MacroUtils, etc.)
- `cellar list-external io.github.halotukozak:made_3:0.1.0 made.annotation` — confirms 5 annotation classes present
- `cellar get-source io.github.halotukozak:made_3:0.1.0 made.Default` — source-level confirmation: `trait Default[O] extends (() => O)`
- Local source files (this branch):
  - `core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala`
  - `core/src/main/scala-3/com/avsystem/commons/misc/NOpt.scala`
  - `core/src/main/scala-3/com/avsystem/commons/misc/OptArg.scala`
  - `core/src/main/scala-3/com/avsystem/commons/misc/OptRef.scala`
  - `core/src/main/scala-3/com/avsystem/commons/misc/compat.scala`
  - `core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala`
  - `build.sbt` lines 27, 326–331, 349–353

### Secondary (MEDIUM confidence)

- `.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md` — `made` pinning rationale (0.1.0 only; no SNAPSHOT).
- `.planning/phases/04-made-integration/04-CONTEXT.md` — phase scope.
- `.planning/REQUIREMENTS.md` §INFRA-06, §MADE-01.

### Tertiary (LOW confidence)

None — every Phase 4 claim is backed by either a cellar invocation or a local file read.

## Metadata

**Confidence breakdown:**
- Standard stack (`made` 0.1.0): HIGH — cellar verifies every symbol.
- Architecture (value-class Default cast, annotation export): HIGH — pattern is directly read from current branch source.
- Pitfalls: HIGH — `compat.scala` GenCodec dependency is grep-verified; snapshot resolver pitfall is build-doc verified.
- Downstream consumer list: HIGH — grep over the entire `core/src/main/scala-3/` tree.

**Research date:** 2026-05-30
**Valid until:** 2026-06-29 (30 days; `made` 0.1.0 is published and immutable; only risk is the planner choosing to bump version, which would re-trigger verification)
