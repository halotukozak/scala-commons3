# Phase 4: `made` integration - Context

**Gathered:** 2026-05-30
**Status:** Ready for planning
**Mode:** auto (user AFK — recommended defaults selected)

<domain>
## Phase Boundary

Bring the `made` library wiring (build-time dep + Scala 3 source-side aliases/derivation hooks for `Default[Opt/NOpt/OptArg/OptRef]` etc.) onto the upstream/scala-3 PR train.

The bulk of `made`-integrated source code is already authored on the fork's `master` branch (24 files under `core/src/main/scala-3/` import from `made`). Phase 4 SELECTS the minimum slice that lets `made` resolve and the **wiring primitives** become available — it does NOT port full GenCodec/derivation surface. That goes to Phase 5 (CORE-01).

**In scope:**
- `build.sbt` (or `Commons.scala`) clauses adding `"io.github.halotukozak" %% "made" % madeVersion` conditionally on Scala 3.
- Wiring primitives only: `Default[Opt]`, `Default[NOpt]`, `Default[OptArg]`, `Default[OptRef]` (already use `made.Default`); `madeAnnotationAliases` (object that re-exports `made.annotation.*` under avsystem package); `TransparentWrapping` re-import where it's a thin alias.
- MIGRATION.md updates for `made` row + reflection in `core` row notes.

**Out of scope:**
- `GenCodec`, `GenObjectCodec`, `GenKeyCodec`, `GenRef`, `HasGenCodec` — those are Phase 5/6.
- `cbor` / `mongo` derivation — Phase 9/11.
- `made` library development itself (upstream Maven dep).

</domain>

<decisions>
## Implementation Decisions

### made version
- Pin `madeVersion = "0.1.0"` for upstream PR (matches roadmap success criterion 1).
- Fork master currently uses `"0.1.1-SNAPSHOT"` — that's local-dev only and MUST NOT land on upstream. The planner downgrades to `0.1.0` and removes any `-SNAPSHOT` resolver.
- Verified resolvable via `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` (per Phase 1 research).

### Build wiring
- Conditional dep keyed on `scalaBinaryVersion.value == "3"` (same idiom upstream already accepts).
- Apply to: `core` (jvm), `core-js`. (NOT `cbor` / `mongo` in this phase — those carry their own derivation work later.)
- No new sbt plugins. No resolver changes.

### Source files in this PR — wiring primitives only
Files to port from fork master `core/src/main/scala-3/` (verbatim if already authored there):
- `com/avsystem/commons/misc/Opt.scala` — uses `made.Default`. Confirm class works with `made` 0.1.0 (not snapshot).
- `com/avsystem/commons/misc/NOpt.scala`
- `com/avsystem/commons/misc/OptArg.scala`
- `com/avsystem/commons/misc/OptRef.scala`
- `com/avsystem/commons/misc/madeAnnotationAliases.scala` (or whatever file declares the alias object — may need to be extracted from a larger file)

DO NOT port (deferred to later phases):
- `GenCodec*`, `GenObjectCodec*`, `GenKeyCodec*`, `GenRef*`, `HasGenCodec*`, `TransparentWrapperCompanion`, `flatten.scala`, `defaultCase.scala`, `transientDefault.scala`, `SerializationName.scala`, `cbor/CborAdtMetadata`.

The planner verifies the import graph: if any of the four `Opt*.scala` files transitively need `GenCodec` or similar, EITHER strip that section in this PR OR escalate the file's port to a later phase. Goal is minimum island.

### Annotation alias surface
- `madeAnnotationAliases` re-exports a stable subset of `made.annotation.*` under the avsystem namespace. If the fork master file does this for many annotations, the Phase 4 PR keeps the alias object SMALL (only the ones used by `Opt*.scala`) and defers the rest until consumer code lands.

### Compile gate
- `sbt '++3 core/compile'` succeeds with the ported file set.
- `sbt '++2.13 core/compile'` succeeds (the 2.13 sources are unchanged — `made` only loads on Scala 3).
- `sbt '++3 core-js/compile'` succeeds.
- No tests added in Phase 4; tests come in Phase 7 (CORE — tests revival).

### MIGRATION.md updates
- Add new row above `core` row: `made | n/a | cross | n/a | n/a | external dep at 0.1.0, Scala 3 only`.
- `core` row Notes column appended: `made wiring primitives ported; full derivation pending`.
- Status column for `core`: still `wip` (not `cross` yet — full port unfinished).

### Deprecation policy reaffirmed
- Skip any `@deprecated` symbols from fork master that have stdlib/library replacements (per memory `feedback_dont_port_deprecated.md`).

### PR workflow
- Same human-ack gates: push, PR. Base: `AVSystem/scala-commons:scala-3`.
- Branch name: `04-made-integration` (numeric prefix not GSD nomenclature).

### Claude's Discretion
- Exact partitioning when a single fork-master file mixes wiring primitives with deferred derivation — planner can extract minimal subsets, leave deferred portions for Phase 5/6.
- Whether to ship `madeAnnotationAliases.scala` as a NEW file (cleaner) or inline aliases inside `Opt.scala` etc. Default: new file, separates concerns.
- Whether `cbor` / `mongo` also need the conditional dep added now (probably no — they will pull it in via their own phases).

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Project requirements
- `.planning/REQUIREMENTS.md` §MADE-01, §INFRA-06.
- `.planning/ROADMAP.md` Phase 4.

### Phase 1 substrate
- `.planning/phases/01-cross-compile-infrastructure/01-CONTEXT.md` — `made` dep pinned at `0.1.0`, conditional on `scalaBinaryVersion == "3"`.

### Existing made integration on fork master
- `core/src/main/scala-3/com/avsystem/commons/misc/Opt.scala` (and `NOpt`, `OptArg`, `OptRef`) — the wiring-primitive files.
- All `core/src/main/scala-3/**/*.scala` files importing `made.*` (24 files total — see scout output). Phase 4 ports the wiring subset; rest deferred.

### `made` library reference
- `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` — authoritative API lookup.
- `cellar get-external io.github.halotukozak:made_3:0.1.0 made.annotation.optionalParam` — annotation surface.

### Phase 2 docs
- `MIGRATION.md` — `made` row added, `core` row Notes appended.

No external specs / ADRs.

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- **24 already-authored Scala 3 files on this branch import from `made`.** This is NOT a green-field port. The planner reads these files to identify the minimum island.
- `build.sbt` already declares conditional `made` dep for `core` (lines 327–330) and `core-js` (lines 348–351). Only change needed: bump `madeVersion` constant from `"0.1.1-SNAPSHOT"` to `"0.1.0"`.

### Established Patterns
- Conditional dep via `if (scalaBinaryVersion.value == "3") Seq(...) else Seq.empty` — already idiomatic in this build.
- `scala-3/` source dir + Phase 1's `mkSourceDirs` already routes files correctly.

### Integration Points
- `build.sbt` `madeVersion` constant (line 27).
- `core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala`.
- New file: `core/src/main/scala-3/com/avsystem/commons/misc/madeAnnotationAliases.scala` (if not already present).
- `MIGRATION.md`.

### Risk / Pitfall
- If `Opt.scala` (etc.) on fork master uses `made` APIs that differ between `0.1.0` and `0.1.1-SNAPSHOT`, the downgrade may fail to compile. Planner runs `cellar get-external io.github.halotukozak:made_3:0.1.0 made.Default` to confirm signatures, and the executor falls back to a per-file adaptation if signatures diverge.

</code_context>

<specifics>
## Specific Ideas

- Minimum-island PR philosophy from CONTEXT 1: keep the diff narrow to ease upstream review.
- "Wiring primitives" framing (Opt+annotation aliases only) borrowed from typelevel ecosystem migrations (cats kernel → core split).

</specifics>

<deferred>
## Deferred Ideas

- Full `GenCodec` Scala 3 port (uses `made.Made` + derivation) — Phase 5.
- `cbor` derivation refresh — Phase 11.
- `mongo` derivation refresh — Phase 9.
- Bumping `made` to a future stable (`0.1.1` non-snapshot) once it ships upstream — backlog.
- Adding `madeAnnotationAliases` for the FULL annotation surface — backlog, driven by consumer needs.

</deferred>

---

*Phase: 04-made-integration*
*Context gathered: 2026-05-30 via /gsd:discuss-phase --auto*
