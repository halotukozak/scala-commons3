# Requirements: scala-commons3 → upstream `scala-3`

**Defined:** 2026-05-30
**Core Value:** Every PR independently mergeable on upstream `scala-3` — green CI on both Scala versions, MIGRATION.md updated in the same change, user ack before open.

## v1 Requirements

(35 total — INFRA 9, WORKFLOW 5, DOC 4, QUALITY 3, MACROS 1, MADE 1, CORE 3, HOCON 1, MONGO 2, JS 1, BENCH 1, CBOR 1, DEPR 3)

### Build Infrastructure (first PR)

- [x] **INFRA-01**: `crossScalaVersions` set to `[2.13.x, 3.8.x]` on every module that will cross-build
- [x] **INFRA-02**: `jvm` aggregate (cross-built modules) + `jvm2` aggregate (2.13-only stranded modules) defined in `build.sbt`
- [x] **INFRA-03**: Cross-version source layout helpers (`scala/`, `scala-2.13/`, `scala-3/`) wired via `unmanagedSourceDirectories` / `mkSourceDirs`
- [x] **INFRA-04**: `sbt-mima-plugin` at 1.1.5 + `sbt-tasty-mima` 1.4.0 staged (no `tastyMiMaPreviousArtifacts` yet)
- [x] **INFRA-05**: scalafmt `fileOverride` covers `scala-2.13/` with `scala213source3` dialect; rest uses `scala3`
- [x] **INFRA-06**: `made` library (Scala-3-only) added as dependency, pinned to `0.1.0` (published release)
- [x] **INFRA-07**: CI matrix runs the 5 gate commands on Java 17, 21, 25 — pin-2.13 applied to jvm/jvm2/js/mima gates (lifts in Phase 3 when macros-3 stub lands); commons-* prefix on aggregate IDs; scalafmtCheckAll dialect-agnostic. Landed at upstream/scala-3 @ `84e21dee`.
- [x] **INFRA-08**: First PR compiles green on Scala 2.13; Scala 3 side compiles whatever is already ported (empty/stub if needed)
- [x] **INFRA-09**: `jetty` / `analyzer` / `spring` / RPC excluded from `jvm` aggregate; live under `jvm2` or `Compile/skip` on Scala 3

### Cross-Cutting (every PR)

- [x] **WORKFLOW-01**: Branch off latest `upstream/scala-3` for every PR
- [x] **WORKFLOW-02**: PR targets `AVSystem/scala-commons:scala-3` — Phase 1 satisfied via **direct-push by maintainer** to `AVSystem/scala-commons:scala-3` @ `84e21dee` (no PR opened; same target branch reached); fork-PR workflow remains the contract for subsequent phases.
- [x] **WORKFLOW-03**: User ack obtained before push AND before PR — Phase 1 satisfied via **direct-push by maintainer** (user = maintainer; approved the push; no external PR step to ack).
- [x] **WORKFLOW-04**: No GSD nomenclature in commit messages or PR title/body
- [x] **WORKFLOW-05**: `.planning/` never appears in any commit diff
- [x] **DOC-01**: Single top-level `MIGRATION.md` at repo root maintained across all PRs
- [x] **DOC-02**: `MIGRATION.md` updated in same PR as the work it tracks (status flip + notes)
- [x] **DOC-03**: `MIGRATION.md` "Deprecated" section seeded by scanning `@deprecated` annotations in fork master
- [x] **DOC-04**: `MIGRATION.md` "2.13-only modules" section formalized (jetty, analyzer, spring, RPC)
- [x] **QUALITY-01**: No new `@nowarn` / `-Wconf` introduced by migration PRs
- [ ] **QUALITY-02**: No `???` stubs or disabled tests introduced in non-test source
- [x] **QUALITY-03**: MiMa filters (when used) carry per-filter justification comment

### Source Ports (subsequent PRs)

- [x] **MACROS-01**: `macros` module — Scala 3 stub (empty-jar) so `dependsOn(macros)` resolves on both versions
- [x] **MADE-01**: `made` integration code (annotation aliases, derivation hooks, `Default[Opt/NOpt/OptArg/OptRef]`) ported to Scala 3 side of relevant modules
- [ ] **CORE-01**: `core` typeclass derivation entry points (`given` / `implicit def` for GenCodec, GenKeyCodec, GenObjectCodec) on Scala 3
- [x] **CORE-02**: `core` cross-version source organization — version-specific entry points, shared utilities
- [ ] **CORE-03**: `core` test suite green on Scala 3 (or explicitly marked & justified disabled)
- [ ] **HOCON-01**: `hocon` cross-built on Scala 2.13 + 3; tests green on both
- [ ] **MONGO-01**: `mongo` cross-built on Scala 2.13 + 3 with `CrossVersion.for3Use2_13` containment; tests green on both
- [ ] **MONGO-02**: `mongo-scala-driver` wrapped behind module-local interfaces; `sbt dependencyTree` clean
- [ ] **JS-01**: `core-js` / `mongo-js` cross-built on Scala 2.13 + 3
- [ ] **BENCH-01**: `benchmark3` runs on Scala 3; baseline captured
- [ ] **CBOR-01**: cbor codec changes (annotation aggregate removal, etc.) — MiMa filters justified

### Deprecation Sweep

- [ ] **DEPR-01**: Deprecated scala-2 APIs with stdlib replacements removed (not ported) from cross-built modules
- [ ] **DEPR-02**: Deprecation log in MIGRATION.md records each removal with rationale
- [ ] **DEPR-03**: `@deprecated` annotations already present in fork master propagated to MIGRATION.md "Deprecated" table

## v2 Requirements

Post-migration (after upstream `scala-3` reaches feature parity).

### Release

- **RELEASE-01**: First `_3` artifact release; Scala 3 MiMa baseline established; `sbt-tasty-mima` activated
- **MADE-02**: Pin `made` to stable release (when published)

### Module decisions

- **JETTY-02**: `jetty` / RPC port to Scala 3 OR formal sunset
- **ANALYZER-01**: `analyzer` (compiler plugin) port to Scala 3 plugin API OR sunset
- **SPRING-01**: `spring` port OR sunset

### Style modernization

- **STYLE-01**: `given`/`using` modernization sweep (already partial in master)
- **NULL-01**: `-Yexplicit-nulls` adoption + `T | Null` audit
- **GENCODEC-01**: Address derivation gap items from prior CONCERNS.md (18 items)

### CI

- **CI-01**: Add Java 21/25 to matrix
- **CI-02**: Decide Scala 3 LTS (3.3.x) vs current (3.8.x) at first `_3` release

## Out of Scope

| Feature | Reason |
|---------|--------|
| sbt-projectmatrix | Archived April 2025; sbt 2.x in-sources it |
| Cherry-picking from `upstream/master` | Source of truth is fork `master` only |
| Rebasing pre-existing `migration/NN-*` branches | User chose direct master-commit cherry-pick model |
| Importing prior `MIGRATION.md` (52c2b122) | Replaced by fresh MIGRATION.md authored on upstream `scala-3` |
| Committing `.planning/` files | Local-only; gitignored |
| GenCodec gap fixes during migration | Deferred to post-migration RELEASE-01 |
| Whole-file scalafmt sweeps in one PR | Noisy; format incrementally per slice |
| Combining cross-compile work with feature work in same PR | Each PR scoped to a single concern |
| Auto-merging PRs | Global rule — manual merge only |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| INFRA-01 | 1 | Complete |
| INFRA-02 | 1 | Complete |
| INFRA-03 | 1 | Complete |
| INFRA-04 | 1 | Complete |
| INFRA-05 | 1 | Complete |
| INFRA-06 | 1 (re-affirmed in 4) | Complete |
| INFRA-07 | 1 | Complete |
| INFRA-08 | 1 | Complete |
| INFRA-09 | 1 (re-affirmed in 12) | Complete |
| WORKFLOW-01 | 1 (cross-cutting) | Complete |
| WORKFLOW-02 | 1 (cross-cutting) | Complete (Phase 1: direct-push by maintainer) |
| WORKFLOW-03 | 1 (cross-cutting) | Complete (Phase 1: direct-push by maintainer) |
| WORKFLOW-04 | 1 (cross-cutting) | Complete |
| WORKFLOW-05 | 1 (cross-cutting) | Complete |
| DOC-01 | 2 | Complete |
| DOC-02 | 2 (cross-cutting) | Complete |
| DOC-03 | 2 | Complete |
| DOC-04 | 2 (refined in 12) | Complete |
| QUALITY-01 | 1 (cross-cutting) | Complete |
| QUALITY-02 | 6 (cross-cutting from 1) | Pending |
| QUALITY-03 | 1 (cross-cutting) | Complete |
| MACROS-01 | 3 | Complete |
| MADE-01 | 4 | Complete |
| CORE-01 | 5 | Pending |
| CORE-02 | 5 | Complete |
| CORE-03 | 7 | Pending |
| HOCON-01 | 8 | Pending |
| MONGO-01 | 9 | Pending |
| MONGO-02 | 9 | Pending |
| JS-01 | 10 | Pending |
| BENCH-01 | 10 | Pending |
| CBOR-01 | 11 | Pending |
| DEPR-01 | 6 | Pending |
| DEPR-02 | 6 | Pending |
| DEPR-03 | 11 | Pending |

**Coverage:**
- v1 requirements: 35 total
- Mapped to phases: 35 ✓
- Unmapped: 0 ✓

---
*Requirements defined: 2026-05-30 (post-reset, cherry-pick model)*
