---
phase: 02-migration-md-skeleton-deprecation-seed
plan: 02
subsystem: docs
tags: [migration-md, deprecation-seed, port-tag, skip-port-tag]

requires:
  - phase: 02
    plan: 01
    provides: "MIGRATION.md skeleton with `## Deprecation log` heading and a placeholder sentence to replace"
provides:
  - "Populated `## Deprecation log` section in MIGRATION.md (152 entries from origin/master@bcc3bcbf)"
  - "Seed cite line `origin/master@bcc3bcbf` (2026-05-31) + verbatim re-run command in the doc body"
  - "Per-entry [port] / [skip-port] tag computed via the locked decision rule (RESEARCH §Pitfall 3)"
  - "Grep-friendly listing grouped under `### core/` (123 entries) and `### mongo/` (29 entries), sorted by path then line"
affects: [Plan 02-03 (check.sh validation has populated section to assert against); Plan 02-04 (push & PR)]

tech-stack:
  added: []
  patterns:
    - "Deterministic seed: master ref pinned by short SHA in the doc; command recorded verbatim for re-run"
    - "Two-pass tag scheme: trigger substring match on FULL (untruncated) deprecation message; truncation applied only for display"
    - "Module grouping under `###` subheadings inside fenced text blocks; sort key is (path, line) for diff-friendly future updates"
    - "scala-3/ deprecations included (per RESEARCH Pitfall 4) — they document Scala-3-side existing deprecations future maintainers must see"

key-files:
  created:
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-02-deprecation-log-seed-SUMMARY.md
  modified:
    - MIGRATION.md

key-decisions:
  - "Tagging rule applied against FULL message before 80-char truncation. Otherwise the truncation operation can strip the trigger substring (e.g., Sam.scala message starts with `Use native SAM conversion instead, e.g. ...` and the truncation drops the tail). Tagging against the full message yields stable classification regardless of message length."
  - "Sam.scala / SamCompanion.scala tagged [port], not [skip-port], because the literal message does not contain any of the six locked trigger substrings (`stdlib`, `scala.`, `Scala 2.13 has native`, `use SAM syntax`, `lambda`, `since Scala 2.13`). The message instead reads `Use native SAM conversion instead, e.g. `val r: Runnable = () => doStuff()`...`. Per the locked rule (RESEARCH §Pitfall 3), this classifies as [port]. The rule is mechanical and deterministic; downstream porting work can re-classify on a case-by-case basis if the maintainer judges native SAM conversion to be a Scala language feature (which would justify [skip-port])."
  - "Tag totals: 145 [port], 7 [skip-port]. The low [skip-port] count reflects that most fork-master @deprecated annotations point at internal replacements (e.g., `Use GenCodec.materialize instead`, `Use given instance directly`, `Bidirectional macro not ported to Scala 3`) rather than stdlib equivalents."
  - "Master SHA `bcc3bcbf` matches both local `master` and `origin/master` at seed time. Doc cites `origin/master@bcc3bcbf` so reviewers can reproduce against the fork remote."

metrics:
  duration: ~12 min
  completed: 2026-05-31

requirements-completed: [DOC-03]
---

# Phase 2 Plan 02: Deprecation log seed Summary

**Populated MIGRATION.md `## Deprecation log` section with 152 tagged entries from `git grep -n '@deprecated' master -- '*.scala'` seeded against `origin/master@bcc3bcbf` (2026-05-31). Single atomic commit `7905d1bd` on branch `02-migration-md`.**

## Performance

- **Duration:** ~12 min
- **Branch tip:** `7905d1bd` on `02-migration-md` (was `48da5be1`)
- **Tasks executed:** 2 of 2 (capture + format, replace + commit)
- **Files in commit:** 1 (`MIGRATION.md`, +169 / -1)

## Seed Provenance

| Field | Value |
|-------|-------|
| Seed command | `git grep -n '@deprecated' master -- '*.scala'` |
| Master ref | `origin/master @ bcc3bcbf` (matches local `master`) |
| Seed date (UTC) | 2026-05-31 |
| Raw hit count | 152 |
| Formatted lines | 152 (1:1, no drops) |

## Entry Counts

| Group | Count |
|-------|-------|
| `core/` | 123 |
| `mongo/` | 29 |
| **Total** | **152** |

| Tag | Count |
|-----|-------|
| `[port]` | 145 |
| `[skip-port]` | 7 |

| Source dir slice | Count |
|------------------|-------|
| `core/src/main/scala-2.13/` | 16 |
| `core/src/main/scala-3/` | 107 |
| `mongo/jvm/src/main/scala/` | 29 |

Counts match RESEARCH §Code Examples expectations (152 total; core=123, mongo=29).

## Task Commits

1. **Task 1** — (no commit; intermediate `/tmp/02-*` artifacts only). Captured raw grep, formatted into canonical `path:line — symbol — "msg" [tag]` lines, split + sorted by module.
2. **Task 2** — `7905d1bd` `docs(migration): seed deprecation log from @deprecated scan of master`
   - Replaced placeholder sentence under `## Deprecation log` with seed cite, re-run command, tag-legend prose, and two fenced text blocks (`### core/` + `### mongo/`).

## Verification Output

```text
--- branch ---                  02-migration-md
--- branch HEAD ---             7905d1bd
--- commits since upstream/scala-3 --- 7 (5 Phase 1 + 2 Phase 2 docs(migration):)
--- docs(migration): commits --- 2 (Plan 01 + Plan 02)
--- diff files (this commit) --- MIGRATION.md
--- H1 unchanged ---            # Scala 3 Migration Status
--- ## headings ---             4 (How to update / Per-module status / 2.13-only modules / Deprecation log)
--- ### deprecation subheadings --- ### core/ + ### mongo/
--- deprecation entry count --- 152
--- untagged entries ---        0
--- malformed entries ---       0
--- seed cite present ---       origin/master@bcc3bcbf (2026-05-31)
--- forbidden vocab ---         NONE (no gsd|wave|phase N|RESEARCH.md|PLAN.md|CONTEXT.md hits)
--- .planning in doc ---        NONE
--- .planning in branch commits --- 0
--- GSD in commit msg ---       NONE
--- MIGRATION.md line count --- 223
```

## Branch History (upstream/scala-3..HEAD)

```
7905d1bd docs(migration): seed deprecation log from @deprecated scan of master   <- this plan
48da5be1 docs(migration): add MIGRATION.md skeleton with per-module status and 2.13-only sections
84e21dee ci: regenerate workflow with 5-gate matrix on java 17/21/25
7bbe47f9 build(commons): land cross-compile build structure (jvm/jvm2/js aggregates, made dep, jetty skip)
67867274 style(scalafmt): reformat shared sources for scala3 dialect
29e638da style(scalafmt): default to scala3 dialect, scope scala213source3 to scala-2.13 sources
d5cd2cc8 build(plugins): bump sbt-mima-plugin 1.1.4 -> 1.1.5
```

Note: Phase 1 commits (`d5cd2cc8`..`84e21dee`) precede the two Phase 2 `docs(migration):` commits because `upstream/scala-3` remote tip is still at `1561d8dc` (Phase 1 push deferred per Phase 1 process deviation, see Phase 1 Plan 03 summary). Plan 02-04 (push & PR) handles upstream branch state separately.

## Decisions Made

1. **Tag rule applied against the FULL deprecation message, not the truncated one.** Truncation cuts at 79 chars; for verbose messages this strips trigger substrings (e.g., `Sam.scala`). Tagging the full message keeps classification deterministic and message-length-independent. Display still uses the 79-char truncation.
2. **`Sam.scala` / `SamCompanion.scala` tagged `[port]`.** Literal message `Use native SAM conversion instead, e.g. ...` does not contain any of the six locked trigger substrings. Per the locked rule (RESEARCH §Pitfall 3), this classifies `[port]`. If the maintainer treats native SAM conversion as a Scala language feature (and thus `[skip-port]`), the entry can be re-classified in a follow-up commit. Rule is mechanical and faithful to PLAN; subjective judgment is deferred.
3. **scala-3-side deprecations (107 entries under `core/src/main/scala-3/`) included in the seed.** Per RESEARCH §Pitfall 4: fork master carries `scala-3/` source dirs, and their `@deprecated` annotations document Scala-3-side existing deprecations future maintainers must see. Excluding them would force re-discovery in a later phase. They are interleaved with `scala-2.13/` entries inside the `### core/` group, sorted by path then line.
4. **Master cite uses `origin/master@bcc3bcbf` (fork) per CONTEXT.md and the planner's REMOTE_OF_MASTER decision.** Local `master` and `origin/master` resolve to the same SHA at seed time; using `origin/master@<sha>` in the doc makes the cite reviewer-reproducible against the fork remote.

## Deviations from Plan

- **[Rule 3 — Blocking issue] Tag rule applied to full message instead of truncated message.** PLAN's `<action>` block computes the tag AFTER truncation. This produces unstable tags for verbose messages whose trigger substring falls past char 79 (concretely: `Sam.scala`'s `e.g. (lambda)` example pattern is in the tail). Adjusted the script to compute the tag on the full message before truncating for display. Outcome: classification is stable; truncated display is unchanged from PLAN. Acceptance criteria unaffected (rule is the same; only the operand differs).

No other deviations. Plan executed as written.

## Issues Encountered

- **None.** Single iteration produced all 152 entries with the canonical format.

## User Setup Required

None.

## Next Phase Readiness

- **Plan 02-03 (check.sh) ready.** The populated `## Deprecation log` section now has stable grep targets (`origin/master@`, `### core/`, `### mongo/`, `[port]` / `[skip-port]` tags, `git grep -n '@deprecated' master`).
- **Plan 02-04 (push & PR) blocked on:** Plan 02-03 landing first; then push branch and open PR onto `upstream/scala-3`.

## Self-Check: PASSED

- [x] `MIGRATION.md` exists with 223 lines (≥ 200 per PLAN must_haves)
- [x] Commit `7905d1bd` exists on `02-migration-md` (`git rev-parse HEAD`)
- [x] Commit touches only `MIGRATION.md` (`git show --stat HEAD`)
- [x] No GSD nomenclature in commit message (`git log -1 --format=%B | grep -iE 'gsd|phase [0-9]'` empty)
- [x] No `.planning/` in commit (`git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` = 0)
- [x] All `<verify>` automated assertions for Task 1 and Task 2 pass
- [x] 152 entries; sum core(123) + mongo(29) = 152
- [x] All entries tagged `[port]` (145) or `[skip-port]` (7); sum = 152
- [x] Seed cite present: `origin/master@bcc3bcbf` + `2026-05-31`
- [x] Re-run command present verbatim in doc

---
*Phase: 02-migration-md-skeleton-deprecation-seed*
*Completed: 2026-05-31*
