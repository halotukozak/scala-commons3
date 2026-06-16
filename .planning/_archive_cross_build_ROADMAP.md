# Roadmap: scala-commons3 → upstream `scala-3`

**Created:** 2026-05-30 (post-reset, cherry-pick model)
**Granularity:** fine
**Mode:** yolo
**Target branch:** `AVSystem/scala-commons:scala-3`
**Source of truth:** fork `master`
**Coverage:** 28/28 v1 requirements mapped

## Core Value

Every PR independently mergeable on upstream `scala-3`: green CI on both Scala versions, MIGRATION.md updated in the same change, user ack before push and before PR.

## Phases

Each phase = one PR onto `upstream/scala-3`. Phases sequential. Cumulative — later phases build on earlier ones landing.

- [x] **Phase 1: Cross-compile infrastructure** — Build infra only; no source ports — landed on upstream/scala-3 @ `84e21dee` (2026-05-31)
- [x] **Phase 2: MIGRATION.md skeleton + deprecation seed** — Branch `02-migration-md` pushed to fork @ `4ae73373` (CI green); stacked PR open + force-push deferred to user (2026-05-31)
- [x] **Phase 3: macros Scala 3 stub** — `03-macros-stub` @ `221f3bda` pushed to AVSystem; PR #858 open (base `02-migration-md`, milestone Scala 3); CI green (2026-05-31)
- [x] **Phase 4: `made` integration** — `04-made-integration` @ `c3e54b16` pushed to AVSystem; PR #859 open (base `03-macros-stub`, milestone Scala 3); CI green; MADE-01 partially satisfied (wiring primitives only — derivation surface deferred to Phase 5) (2026-06-01)
- [ ] **Phase 5: core — Scala 3 baseline port** — Bring `core/src/main/scala-3/` skeleton: cross-version source organization, basic typeclass derivation entry points
- [ ] **Phase 6: core — given/using sweep + serialization port** — Bring `implicit`→`given/using/extension` sweep, serialization/cbor on Scala 3, deprecation drops
- [ ] **Phase 7: core — tests revival** — Re-enable previously-disabled `core` tests on Scala 3; record exceptions in MIGRATION.md §"Tests known to stay disabled"
- [ ] **Phase 8: hocon cross-build** — `hocon` cross-built; HoconInputTest + HoconGenCodecRoundtripTest re-enabled
- [ ] **Phase 9: mongo cross-build** — `mongo` cross-built using `CrossVersion.for3Use2_13`; driver wrapped; dependencyTree clean; tests green
- [ ] **Phase 10: mongo-js + core-js + benchmark3 sanity** — ScalaJS variants cross-built; `benchmark3` runs on Scala 3
- [ ] **Phase 11: cbor MiMa + deprecation cleanup** — cbor annotation-aggregate changes with justified MiMa filters; finalize deprecation log in MIGRATION.md
- [ ] **Phase 12: jetty / analyzer / spring / RPC formalization** — Out-of-cross-build status documented in MIGRATION.md; `Compile/skip` or `jvm2` placement confirmed

## Phase Details

### Phase 1: Cross-compile infrastructure
**Goal:** Land the cross-compile foundation on upstream `scala-3` so subsequent module ports have a substrate. No source ports — only `build.sbt`, `project/plugins.sbt`, `.scalafmt.conf`, `.github/workflows/ci.yml`, `Makefile`.
**Depends on:** None (first PR onto upstream/scala-3)
**Requirements:** INFRA-01, INFRA-02, INFRA-03, INFRA-04, INFRA-05, INFRA-06, INFRA-07, INFRA-08, INFRA-09, WORKFLOW-01..05, QUALITY-01, QUALITY-03
**Success Criteria:**
  1. `crossScalaVersions` set; `jvm`/`jvm2` aggregate split; `jetty`/`analyzer`/`spring`/RPC excluded from `jvm`
  2. Source layout helpers wired (`scala-2.13/`, `scala-3/` recognized by sbt)
  3. `sbt-mima-plugin` 1.1.5 + `sbt-tasty-mima` 1.4.0; scalafmt fileOverride with `scala213source3`; `made` dep at `0.1.0`
  4. CI matrix runs all 5 gate commands green on Java 17, 21, 25
  5. Clean local checkout reproduces green matrix
  6. PR opened on upstream/scala-3 after user ack; commit messages have no GSD nomenclature; `.planning/` not in diff
**Plans:** 3/3 plans executed — **PHASE COMPLETE** (100%)
- [x] 01-01-plugins-and-scalafmt-PLAN.md — Branch off upstream/scala-3; bump sbt-mima-plugin to 1.1.5; invert scalafmt dialect (scala3 default + scala-2.13/ override)
- [x] 01-02-commons-build-structure-PLAN.md — project/Commons.scala: jvm/jvm2/js aggregates, per-module crossScalaVersions, scala3Version baseline, made 0.1.0 on Scala 3, jetty skip block
- [x] 01-03-ci-workflow-PLAN.md — Java 17/21/25 matrix (revised from "Java 17 only") + single 5-gate WorkflowStep.Sbt with pin-2.13 on jvm/jvm2/js/mima (Phase 3 lifts pin); regenerate ci.yml; fork CI green; **direct-push to upstream/scala-3 by maintainer at `84e21dee`** (process deviation — fork-PR workflow bypassed)

### Phase 2: MIGRATION.md skeleton + deprecation seed
**Goal:** Repo-root tracking doc lands on upstream `scala-3`. Per-module status table + deprecation log seeded from `@deprecated` scan + "2.13-only modules" formalized.
**Depends on:** Phase 1
**Requirements:** DOC-01, DOC-03, DOC-04
**Success Criteria:**
  1. `MIGRATION.md` exists at repo root with per-module status table (macros, made, core, hocon, mongo, mongo-js, core-js, benchmark3, jetty, analyzer, spring, RPC, cbor)
  2. Deprecation log seeded from `git grep '@deprecated' master -- '*.scala'`
  3. "2.13-only modules" section documents jetty/analyzer/spring/RPC with rationale
  4. Per-PR update contract documented
  5. CI matrix green; user ack; PR open
**Plans:** 4/4 plans executed — **PHASE COMPLETE** (100%, PR open + force-push deferred to user)
- [x] 02-01-skeleton-and-status-table-PLAN.md — Cut branch off upstream/scala-3; create MIGRATION.md skeleton with H1, How-to-update (5 rules), 13-row per-module status table, 2.13-only rationale paragraphs (jetty/analyzer/spring/RPC), empty Deprecation log heading
- [x] 02-02-deprecation-log-seed-PLAN.md — Populated ## Deprecation log with 152 tagged entries from `git grep '@deprecated' master` (145 [port] / 7 [skip-port]); cited `origin/master@bcc3bcbf`
- [x] 02-03-check-script-and-verification-PLAN.md — Authored `.planning/.../check.sh` (18 assertions); `check.sh` + `scalafmtCheckAll` + `++2.13 commons-jvm/compile` all green
- [x] 02-04-push-and-pr-PLAN.md — Claude pushed `02-migration-md` to fork @ `4ae73373` (CI green); user took ownership of force-push refresh (to `0729e947`) + stacked PR #1/#2 open against `AVSystem:scala-3`

### Phase 3: macros Scala 3 stub
**Goal:** Empty `macros/src/main/scala-3/` so `dependsOn(macros)` resolves on Scala 3 without inheriting whitebox impls.
**Depends on:** Phase 2
**Requirements:** MACROS-01
**Success Criteria:**
  1. `macros` cross-built; Scala 3 jar is empty (or minimal placeholder)
  2. `dependsOn(macros)` works on both Scala versions for downstream modules
  3. CI green; MIGRATION.md `macros` row reflects "stub on Scala 3"
**Plans:** 2/2 plans executed — **PHASE COMPLETE** (100%, PR #858 open against AVSystem, awaits manual merge)
- [x] 03-01-macros-stub-and-migration-flip-PLAN.md — `.gitkeep` stub landed; `commons-macros_3` jar = 335 bytes; MIGRATION.md macros row notes flipped; commits `0864e85f` + `221f3bda` on branch `03-macros-stub`
- [x] 03-02-push-and-pr-PLAN.md — Branch pushed to AVSystem; PR #858 opened with cascading base `02-migration-md`, milestone "Scala 3"; CI green

### Phase 4: `made` integration
**Goal:** Bring `made` library + Scala 3 wiring (annotation aliases, derivation hooks, `Default[Opt/NOpt/OptArg/OptRef]`) onto upstream `scala-3`.
**Depends on:** Phase 3
**Requirements:** MADE-01, INFRA-06 (re-affirm)
**Success Criteria:**
  1. `made` 0.1.0 resolves; `core` Scala 3 side references `made.*` derivation primitives
  2. `madeAnnotationAliases`, `Default[Opt/...]` workaround present and usable
  3. CI green; MIGRATION.md updated
**Plans:** 4/4 plans executed — **PHASE COMPLETE** (100%, PR #859 open against AVSystem, awaits manual merge)
- [x] 04-01-branch-and-version-bump-PLAN.md — Branch `04-made-integration` cut off `03-macros-stub @ 221f3bda` (cascadowo); `madeVersion = "0.1.0"` reaffirmed; comment scrubbed; commit `bf8e961a`
- [x] 04-02-port-wiring-primitives-PLAN.md — 5 files ported from `origin/master` (`misc/{Opt,NOpt,OptArg,OptRef}.scala` with `*Compat` mixins dropped + `serialization/madeAnnotationAliases.scala`); commits `66fb1158` + `7e3a3035`; Scala 3 commons-core/compile RED (~136 errors) explicitly deferred to Phase 5
- [x] 04-03-sanity-gate-and-migration-flip-PLAN.md — Reduced 3-gate sanity (2.13 core compile + scalafmtCheckAll + 3.8.2 macros compile) all GREEN; MIGRATION.md `made` row flipped to `cross`, `core` Notes appended; commit `c3e54b16`
- [x] 04-04-push-and-pr-PLAN.md — Pushed `04-made-integration @ c3e54b16` to AVSystem; PR #859 opened (base `03-macros-stub`, milestone "Scala 3"); CI green; PR OPEN for manual merge

### Phase 5: core — Scala 3 baseline port
**Goal:** Establish `core/src/main/scala-3/` directory tree with version-specific entry points; cross-version source dedup into shared `scala/` where byte-identical.
**Depends on:** Phase 4
**Requirements:** CORE-01, CORE-02
**Success Criteria:**
  1. Typeclass derivation entry points (`GenCodec`, `GenKeyCodec`, `GenObjectCodec`) defined in version-specific dirs
  2. Shared utilities live in `scala/`; no implicit-resolution divergence
  3. `core` compiles green on both Scala versions (tests may stay disabled at this stage)
  4. CI matrix green; MIGRATION.md `core` row "compile-only"
**Plans:** 4 plans
- [ ] 05-01-branch-and-relocate-scala-2-only-PLAN.md — Cut branch off Phase 4 tip; git mv ~30+ scala-2-only macro/RPC/deprecated sources from scala/ to scala-2.13/
- [ ] 05-02-cherry-pick-scala-3-sources-PLAN.md — Iteratively cherry-pick scala-3/ files from fork master until ++3.8.2 commons-core/compile is GREEN; trim cbor/RPC refs in-place; defer compat.scala + cbor/
- [ ] 05-03-sanity-gate-and-migration-flip-PLAN.md — Re-run 5-gate matrix; flip MIGRATION.md core row to cross + Notes refresh; single doc commit
- [ ] 05-04-push-and-pr-PLAN.md — Push to AVSystem; open cascading draft PR onto PR #859 with [Scala 3] prefix + milestone 1; two human-verify gates

### Phase 6: core — given/using sweep + serialization port
**Goal:** Bring full `implicit`→`given/using/extension` sweep on Scala 3 core; serialization + cbor ported; deprecated 2.13 APIs with stdlib replacements removed.
**Depends on:** Phase 5
**Requirements:** CORE-01 (refine), DEPR-01, DEPR-02, QUALITY-01, QUALITY-02
**Success Criteria:**
  1. No `implicit` keyword in cross-built `core` Scala 3 sources where `given/using/extension` applies
  2. Serialization + cbor compile + (basic) test green on Scala 3
  3. Deprecated 2.13 APIs with stdlib replacements removed; MIGRATION.md deprecation log updated
  4. No new `@nowarn` / `-Wconf`; CI matrix green
**Plans:** TBD

### Phase 7: core — tests revival
**Goal:** Re-enable previously-disabled core tests on Scala 3. Record exceptions in MIGRATION.md §"Tests known to stay disabled".
**Depends on:** Phase 6
**Requirements:** CORE-03, QUALITY-02
**Success Criteria:**
  1. Tests previously commented out (per fork master notes) re-enabled where they pass
  2. Each test that stays disabled has rationale in MIGRATION.md §"Tests known to stay disabled" (not in source)
  3. CI green; `core/Test/test` passes on both Scala versions
**Plans:** TBD

### Phase 8: hocon cross-build
**Goal:** `hocon` cross-built. Smallest pure-Scala downstream — proves template.
**Depends on:** Phase 7
**Requirements:** HOCON-01
**Success Criteria:**
  1. `hocon` compiles + tests green on Scala 2.13 + 3
  2. `HoconInputTest`, `HoconGenCodecRoundtripTest` re-enabled
  3. MIGRATION.md `hocon` row → "done"
**Plans:** TBD

### Phase 9: mongo cross-build
**Goal:** `mongo` cross-built with `CrossVersion.for3Use2_13` contained behind module-local interfaces.
**Depends on:** Phase 8
**Requirements:** MONGO-01, MONGO-02
**Success Criteria:**
  1. `mongo` cross-built; `sbt dependencyTree` clean
  2. `mongo-scala-driver` wrapped — no `for3Use2_13` leakage in module APIs
  3. Mongo tests green on both Scala versions
  4. MIGRATION.md `mongo` row → "done"
**Plans:** TBD

### Phase 10: mongo-js + core-js + benchmark3 sanity
**Goal:** ScalaJS variants cross-built; benchmark3 baseline.
**Depends on:** Phase 9
**Requirements:** JS-01, BENCH-01
**Success Criteria:**
  1. `core-js`, `mongo-js` cross-built; `+js/test` green
  2. `benchmark3` runs on Scala 3; baseline captured
  3. MIGRATION.md updated
**Plans:** TBD

### Phase 11: cbor MiMa + deprecation cleanup
**Goal:** cbor annotation-aggregate removal MiMa filters with per-filter justification; final deprecation sweep.
**Depends on:** Phase 10
**Requirements:** CBOR-01, DEPR-03, QUALITY-03
**Success Criteria:**
  1. cbor changes carry justified MiMa filters; `++2.13 mimaReportBinaryIssues` green
  2. `MIGRATION.md` "Deprecated" table reflects all `@deprecated` annotations from master
  3. CI green; PR open
**Plans:** TBD

### Phase 12: jetty / analyzer / spring / RPC formalization
**Goal:** Documentation-only PR. Confirm out-of-cross-build status; place in `jvm2` aggregate or `Compile/skip` on Scala 3; MIGRATION.md "2.13-only modules" finalized.
**Depends on:** Phase 11
**Requirements:** INFRA-09 (re-affirm), DOC-04 (refine)
**Success Criteria:**
  1. `jetty`, `analyzer`, `spring`, RPC each documented in MIGRATION.md with rationale and current status
  2. `jvm2` aggregate / `Compile/skip` placement matches doc
  3. CI green; PR open
**Plans:** TBD

## Cross-Cutting Requirements

Threaded through every phase:

| Requirement | Owned by | Enforced in |
|-------------|----------|-------------|
| WORKFLOW-01..05 | Phase 1 (first PR establishes contract) | Every phase's success criteria |
| DOC-02 (MIGRATION.md updated in same PR) | Phase 2 (creates doc) | Every phase from 2 onward |
| QUALITY-01..03 | Phase 1 baseline | Every phase |

## Coverage

- v1 requirements: 28 total
- All 28 mapped to one or more phases
- Cross-cutting reqs (WORKFLOW-*, DOC-02, QUALITY-*) thread through every phase

## Per-PR Workflow (from MIGRATION.md §5 spirit)

1. `git fetch upstream`
2. `git checkout -b <slice-name> upstream/scala-3`
3. Cherry-pick relevant commits from fork `master` — or hand-author the slice
4. Resolve scalafmt 3.11.1 reformat conflicts (accept upstream reformat; rerun `sbt scalafmtAll` on slice-edited files)
5. Run local CI: `make ci` (= `+jvm/test +jvm2/test +js/test ++2.13 mimaReportBinaryIssues scalafmtCheckAll`)
6. Update `MIGRATION.md`
7. **Ask user for ack** — before push
8. `git push origin <slice-name>`
9. Confirm GitHub Actions green
10. **Ask user for ack** — before PR open
11. `gh pr create --base scala-3 --repo AVSystem/scala-commons` (no GSD nomenclature in title/body)
12. **Maintainer merges manually** — Claude never merges
