# scala-commons3 — Scala 3 Migration (cherry-pick model)

## What This Is

Fork of `AVSystem/scala-commons` being merged into the upstream `scala-3` branch as small, independently-shippable PRs. Each PR cherry-picks coherent slices from the fork's `master` (258 commits of WIP draft) onto `upstream/scala-3` (currently == `upstream/master`, no Scala 3 work landed).

## Core Value

Every PR is independently mergeable onto upstream `scala-3`: green CI on Scala 2.13 + Scala 3 after rebase, MIGRATION.md updated in the same change, no new `@nowarn`/`-Wconf` suppressions, user ack before opening.

## Requirements

### Validated

<!-- Reality on disk -->

- ✓ Fork `master` carries 258 commits of WIP migration draft — existing
- ✓ `crossbuild-2-and-3` branch on fork = same tip as `master` — existing
- ✓ 14 `migration/NN-*` topic branches on fork (reference only — not the work unit) — existing
- ✓ `upstream/scala-3` == `upstream/master` (`1561d8dc`), no Scala 3 work landed — existing
- ✓ Fork `master` already has Scala 3 sources, `jvm`/`jvm2` aggregate, `made` integration, scalafmt dialect pin — existing
- ✓ `MIGRATION.md` (52c2b122) exists on fork master — to be **rewritten** when first PR lands on upstream/scala-3

### Active

- [ ] Build PR sequence onto `upstream/scala-3` by cherry-picking from fork `master`
- [ ] First PR: enable cross-compilation infrastructure on `upstream/scala-3` (build infra only; no source ports)
- [ ] Subsequent PRs: port modules / migrate features in dependency order (macros stub → made dep → core in chunks → hocon → mongo → ...)
- [ ] Each PR: green CI on Scala 2.13 + Scala 3, MIGRATION.md updated, no new suppressions, no GSD nomenclature in commits, user ack before push/PR
- [ ] Maintain MIGRATION.md at repo root: per-module status + curated deprecation list seeded from `@deprecated` scan
- [ ] Deprecate scala-2 APIs with stdlib replacements (skip porting, mark `@deprecated` or remove)
- [ ] Drop modules not making the trip: `jetty` / RPC, `analyzer`, `spring` (formalize in MIGRATION.md)

### Out of Scope

- Pre-existing `migration/NN-*` branches on fork (reference only — work unit is the upstream PR, cherry-picks come from `master` commits directly)
- Pre-existing `MIGRATION.md` on fork `master` (52c2b122) — replaced when new MIGRATION.md lands on upstream/scala-3
- Cherry-picking from `upstream/master` (not the source)
- GenCodec derivation gaps (18 items from earlier CONCERNS.md) — deferred post-migration
- Committing `.planning/` artifacts — gitignored
- sbt-projectmatrix adoption — archived

## Context

- **Upstream target:** `AVSystem/scala-commons:scala-3` (currently == upstream master; no Scala 3 work landed)
- **Source of truth:** fork's `master` branch
- **Workflow per PR:**
  1. Branch off latest `upstream/scala-3`
  2. Cherry-pick relevant commits (or hand-author the slice from master's reality)
  3. Run full local CI: `+jvm/test`, `+jvm2/test`, `+js/test`, `++2.13 mimaReportBinaryIssues`, `scalafmtCheckAll`
  4. Push to `origin` (fork)
  5. Confirm GitHub Actions green
  6. **Ask user for explicit ack** — always
  7. `gh pr create` against `AVSystem/scala-commons:scala-3`
  8. Update local MIGRATION.md in same PR
- **Conflict expectation:** upstream `scala-3` had a scalafmt 3.11.1 reformat — most PRs touching existing files will conflict on whitespace/style. Resolve in favor of upstream reformat, then `sbt scalafmtAll` on slice-introduced/edited files
- **Cross-build mechanics:** Scala 2.13.18 + Scala 3.8.2, `crossScalaVersions` + per-version source dirs (`scala/`, `scala-2.13/`, `scala-3/`), `jvm` aggregate (cross-built) + `jvm2` aggregate (2.13-only stranded modules)
- **Memory rules apply:** skip deprecated 2.13 APIs with stdlib replacements; fix warnings at source, never via `@nowarn`/`-Wconf`

## Constraints

- **Target branch:** every PR opens against `AVSystem/scala-commons:scala-3` — non-negotiable
- **Green CI per PR:** Scala 2.13 + Scala 3 both pass — non-negotiable
- **No new suppressions:** zero new `@nowarn` / `-Wconf` introduced by migration PRs
- **User ack gate:** explicit ack required before push to origin AND before `gh pr create` — ALWAYS
- **Commit messages:** no GSD nomenclature ("phase", "GSD", "Get Shit Done", etc.). Conventional prefixes only (`build:`, `refactor:`, `feat:`, `fix:`, `test:`, `docs:`, `ci:`, `style:`).
- **Tracking:** single top-level `MIGRATION.md`; `.planning/` never committed
- **Tooling:** sbt cross-build, scalafmt with dialect pinning (`scala213source3` for 2.13 sources since `-Xsource:3` is on), MiMa on 2.13 baseline only
- **Java version:** 17 only (21/25 deferred post-migration)
- **Scala 3 version policy:** defer LTS (3.3.x) vs current (3.8.x) until first `_3` release tag

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Cherry-pick from fork `master` onto `upstream/scala-3` per PR | Upstream branch empty; fork master has full WIP draft to mine | — Pending |
| Ignore pre-existing `migration/NN-*` branches as work units | User wants to cherry-pick from master commits directly, not rebase pre-organized branches | — Pending |
| First PR = enable cross-compilation infra only (no source ports) | Establish foundation; module ports follow in subsequent PRs | — Pending |
| MIGRATION.md rewritten in fresh plan, not imported from 52c2b122 | User wants fresh planning aligned with new model | — Pending |
| User ack required before push AND before PR open | User wants control over what lands upstream | — Pending |
| Drop deprecated scala-2 APIs with stdlib replacements during migration | User memory rule; reduces migration surface | — Pending |
| jetty / RPC / analyzer / spring documented as out-of-cross-build | Not making the trip; formalize in MIGRATION.md | — Pending |

---
*Last updated: 2026-05-30 after re-initialization (cherry-pick model)*
