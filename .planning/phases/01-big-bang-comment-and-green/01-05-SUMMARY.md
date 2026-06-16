---
phase: 01-big-bang-comment-and-green
plan: 05
subsystem: tests
tags: [comment, test-compile, scala3, big-bang]
dependency_graph:
  requires: [01-04-SUMMARY]
  provides: [Test/compile green across all enabled modules, COMPILE-02 gate]
  affects: [core/src/test, core/jvm/src/test, mongo/jvm/src/test, hocon/src/test]
tech_stack:
  added: []
  patterns: [per-file commenting with TODO[scala3-port] tag, lazy-val override fix for SealedEnumCompanion.values]
key_files:
  created: []
  modified:
    - core/src/test/scala/com/avsystem/commons/macros/*.scala (5 files commented)
    - core/jvm/src/test/scala/com/avsystem/commons/di/*.scala (4 files commented)
    - core/jvm/src/test/scala/com/avsystem/commons/macros/TypeClassDerivationTest.scala
    - core/jvm/src/test/scala/com/avsystem/commons/serialization/{JCodecTestBase,JGenCodecTest}.scala
    - core/src/test/scala/com/avsystem/commons/misc/{ImplicitNotFoundTest,MacroInstancesTest,NamedEnumTest,SealedEnumTest}.scala
    - core/src/test/scala/com/avsystem/commons/rpc/Tag.scala
    - core/src/test/scala/com/avsystem/commons/serialization/{CodecTestData,GenCodecRoundtripTest,GenRefTest,IgnoreTransientDefaultMarkerTest,NotUsedTransientDefault,SimpleGenCodecTest,StreamGenCodecTest,StreamInputOutputTest}.scala
    - core/src/test/scala/com/avsystem/commons/serialization/cbor/{CborInputOutputTest,HFloatTest}.scala
    - core/src/test/scala/com/avsystem/commons/serialization/json/{JsonGenCodecRoundtripTest,JsonStringInputOutputTest}.scala
    - core/src/test/scala/com/avsystem/commons/testutil/CompilationErrorAssertions.scala
    - mongo/jvm/src/test/scala/com/avsystem/commons/mongo/BsonInputOutputTest.scala
    - mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/{MongoFilterTest,MongoIndexTest,MongoOrderTest,MongoProjectionTest,MongoRefTest,MongoUpdateTest,TypedMongoCollectionTest,testEntities}.scala
    - hocon/src/test/scala/com/avsystem/commons/hocon/{HoconInputTest,HoconGenCodecRoundtripTest}.scala
decisions:
  - "Whole-file wrap (CONTEXT-permitted) chosen over per-class wrap: every broken file had ALL classes broken (no surviving classes inside) since each touches a single subsystem stubbed in Plans 02-04"
  - "`val values: ... = caseObjects` flipped to `lazy val` in three test files (Tag, NamedEnumTest, SealedEnumTest) as Rule 1 auto-fix — same override-lazy-val pattern as Plan 03's SealedEnumCompanion fix in production code"
metrics:
  duration: ~18 min
  completed: "2026-06-01T13:48:00Z"
  files_modified: 41
  commits: 3
---

# Phase 01 Plan 05: Tests Compile Green Summary

`sbt Test/compile` exits 0 across every enabled module (commons-core JVM+JS, commons-mongo JVM+JS, commons-hocon, commons-cbor); 38 broken test classes commented with `TODO[scala3-port]` tags across 38 files; 3 lazy-val Rule-1 fixes; zero `@nowarn`/`-Wconf` introduced. COMPILE-02 gate satisfied.

## Inventory

| Module | Total test files | Files commented (whole) | Files untouched |
| --- | --- | --- | --- |
| core (shared + jvm + js) | 81 | 27 | 54 |
| mongo (jvm + jvm/scala-2.13) | 21 | 9 | 12 |
| hocon | 4 | 2 | 2 |
| **Total** | **106** | **38** | **68** |

Note: macros module deleted in Plan 02 — no macros tests in scope (per CONTEXT pre-flag).

## TODO Tag Counts

| Module | TODO tags |
| --- | --- |
| core | 27 |
| mongo | 9 |
| hocon | 2 |
| **Total** | **38** |

All 38 commented classes carry a single-line `// TODO[scala3-port]: <ClassName> — depends on <feature> (S|M|L)` marker above the `/* */` block.

## Categories of Broken Tests

1. **macro-test files using `def ... = macro TestMacros.X`** (Scala 2 macros — deleted in Plan 02) — KnownSubtypesTest, TreeForTypeTest, TypeStringTest, ApplyUnapplyTest, JavaClassNameTest, TypeClassDerivationTest, CompilationErrorAssertions.
2. **DI tests using `Components.component`/`Component.ref`** (`???`-stubbed in Plan 02) — ComponentComposition, ComponentsExample, ComponentsTest, MyApp.
3. **Serialization tests using `GenCodec.materialize`/`HasGenCodec`** (`???`-stubbed in Plan 02) — CodecTestData, GenCodecRoundtripTest, SimpleGenCodecTest, StreamGenCodecTest, StreamInputOutputTest, GenRefTest, NotUsedTransientDefault, IgnoreTransientDefaultMarkerTest, CborInputOutputTest, JsonGenCodecRoundtripTest, JsonStringInputOutputTest, JCodecTestBase, JGenCodecTest, MacroInstancesTest, ImplicitNotFoundTest.
4. **Mongo entity tests using `MongoEntityCompanion`/`MongoDataCompanion`** (`???`-stubbed in Plan 03) — testEntities + everything that depends on it (MongoFilterTest, MongoIndexTest, MongoOrderTest, MongoProjectionTest, MongoRefTest, MongoUpdateTest, TypedMongoCollectionTest), BsonInputOutputTest.
5. **HOCON tests using `GenCodec.materialize`** — HoconInputTest, HoconGenCodecRoundtripTest.
6. **CBOR oddball** — HFloatTest: blocked by missing `Short.toHexString` in Scala 3 stdlib (not a stub regression).

## Commits

| Commit | Type | Files | Summary |
| --- | --- | --- | --- |
| `65f507bc` | refactor(core) | 30 | Wrap broken core tests + 3 `lazy val` Rule-1 fixes |
| `3ffab524` | refactor(mongo) | 9 | Wrap broken mongo typed tests + BsonInputOutputTest |
| `555d2bb5` | refactor(hocon) | 2 | Wrap broken HoconInputTest + HoconGenCodecRoundtripTest |

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Override-lazy-val on SealedEnumCompanion.values in tests**
- **Found during:** Round 4 of compile-driven iteration (after wrapping macro/DI/serialization tests)
- **Issue:** `Tag.values`, `NamedEnumTest.SomeNamedEnum.values`, `NamedEnumTest.AnotherNamedEnum.values`, `SealedEnumTest.SomeEnum.values` declared as `val` but Plan 03 changed `SealedEnumCompanion.values` (production) to `lazy val` for Scala 3 override-rules compliance.
- **Fix:** Flipped each to `lazy val`; also rewrote `Tag[_]` to `Tag[?]` in the same line (warning fix).
- **Files modified:** core/src/test/scala/com/avsystem/commons/rpc/Tag.scala, core/src/test/scala/com/avsystem/commons/misc/{NamedEnumTest,SealedEnumTest}.scala
- **Commit:** `65f507bc` (bundled with the core test commenting commit)

### Scope deviations

**Whole-file wrap > per-class wrap:** Plan anticipated mixed files with some surviving classes. In practice, every broken file was 100% broken (e.g., entire DI files used `Components`, entire macro test files used `TestMacros`, entire serialization test files used `GenCodec.materialize`). No partial-file commenting was required.

**Volume vs estimate:** Plan estimated "~133 test files to triage" — actual count was 106 (macros module deletion in Plan 02 already removed test directory references, and JS test variants are sparse).

## Verification Gates (all green)

- `sbt -batch compile` — exit 0
- `sbt -batch Test/compile` — exit 0 across core (JVM+JS), mongo (JVM), hocon, cbor (folded into core)
- `sbt -batch scalafmtCheckAll` — exit 0
- `git diff -- '*.scala' | grep -E '^\+.*(@nowarn|-Wconf)'` — 0 matches (no warning suppression added)
- `git grep -cE 'TODO\[scala3-port\]' -- '*/src/test/scala/*'` — 38 (≥ 20 required)
- Commit prefixes — all `refactor(<module>):` conventional
- No `.planning/` paths in any commit diff
- No GSD nomenclature in commit messages
- No macros/src/test references (macros module gone since Plan 02)

## Requirements Closed

- **COMMENT-01** — Per-file commenting applied (whole-file scope where appropriate)
- **COMMENT-02** — TODO[scala3-port] tags on every commented class
- **COMMENT-04** — Conventional commit prefixes, no GSD nomenclature
- **COMPILE-02** — `sbt Test/compile` exits 0 across all enabled modules
- **QUALITY-01** — No `@nowarn`/`-Wconf` introduced
- **QUALITY-02** — `scalafmtCheckAll` green
- **WORKFLOW-04** — Atomic per-module commits (3 commits)
- **WORKFLOW-05** — `.planning/` excluded from all commits

## Phase 1 Status

With Plan 05 done, only Plan 06 remains: MIGRATION.md flip + push + PR. After Plan 06:

- `compile`, `Test/compile`, `scalafmtCheckAll` all green on Scala 3.8.2 + Scala.js 1.21.0
- Production code is green; tests compile (do not run)
- Phase 2 restoration work can begin on a clean baseline

## Self-Check: PASSED

- `.planning/phases/01-big-bang-comment-and-green/01-05-SUMMARY.md` — FOUND (this file)
- `65f507bc` — FOUND
- `3ffab524` — FOUND
- `555d2bb5` — FOUND
- `sbt Test/compile` exit 0 — verified at 2026-06-01T13:47:22Z
- 38 TODO[scala3-port] tags in `*/src/test/scala/*` — verified
- 0 `@nowarn`/`-Wconf` added in diff — verified
