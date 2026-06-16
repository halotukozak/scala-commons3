---
phase: 2
slug: migration-md-skeleton-deprecation-seed
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-05-30
---

# Phase 2 — Validation Strategy

> Per-phase validation contract for feedback sampling during execution. Docs-only phase — validation is grep-based.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | bash + grep/awk (see `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh`) |
| **Config file** | n/a |
| **Quick run command** | `bash .planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` |
| **Full suite command** | `make ci` (build sanity) + the check script |
| **Estimated runtime** | <2s for grep checks; ~3 min for `make ci` warm |

---

## Sampling Rate

- **After every task commit:** Run the relevant grep section of `check.sh` (or full script — it's fast).
- **After every plan wave:** Full `check.sh` + `sbt scalafmtCheckAll`.
- **Before `/gsd:verify-work`:** `check.sh` exits 0; `sbt '++2.13 jvm/compile'` green (docs PR didn't perturb build).
- **Max feedback latency:** <2s for grep, ~3 min for sbt sanity.

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|-----------|-------------------|-------------|--------|
| 2-01-01 | 01 | 1 | DOC-01 | file-exists | `test -f MIGRATION.md` | ❌ W0 | ⬜ pending |
| 2-01-02 | 01 | 1 | DOC-01 | section-grep | `grep -q '^## Per-module status' MIGRATION.md` | ❌ W0 | ⬜ pending |
| 2-01-03 | 01 | 1 | DOC-01 | row-count | `awk '/^\| Module/,/^$/' MIGRATION.md \| grep -cE '^\| (macros\|made\|core\|hocon\|mongo\|mongo-js\|core-js\|benchmark3\|jetty\|analyzer\|spring\|cbor\|rpc) '` ≥ 13 | ❌ W0 | ⬜ pending |
| 2-02-01 | 02 | 1 | DOC-04 | section-grep | `grep -q '^## 2.13-only modules' MIGRATION.md` | ❌ W0 | ⬜ pending |
| 2-02-02 | 02 | 1 | DOC-04 | content-grep | `grep -qE 'jetty\|analyzer\|spring\|RPC' (within section)` | ❌ W0 | ⬜ pending |
| 2-03-01 | 03 | 2 | DOC-03 | section-grep | `grep -q '^## Deprecation log' MIGRATION.md` | ❌ W0 | ⬜ pending |
| 2-03-02 | 03 | 2 | DOC-03 | line-count | deprecation log has ≥ 100 lines (152 hits expected) | ❌ W0 | ⬜ pending |
| 2-03-03 | 03 | 2 | DOC-03 | tag-coverage | every log line ends with `[port]` or `[skip-port]` | ❌ W0 | ⬜ pending |
| 2-04-01 | 04 | 2 | DOC-02 | section-grep | `grep -q '^## How to update' MIGRATION.md` | ❌ W0 | ⬜ pending |
| 2-04-02 | 04 | 2 | QUALITY (PR hygiene) | grep | `! grep -i 'GSD\|gsd:\|.planning' MIGRATION.md` | ❌ W0 | ⬜ pending |
| 2-04-03 | 04 | 2 | build sanity | sbt | `sbt scalafmtCheckAll && sbt '++2.13 jvm/compile'` | ✅ | ⬜ pending |

*Status: ⬜ pending · ✅ green · ❌ red · ⚠️ flaky*

---

## Wave 0 Requirements

- [ ] Create `.planning/phases/02-migration-md-skeleton-deprecation-seed/check.sh` with all grep assertions (drives every row above marked ❌ W0).
- [ ] Make the script idempotent and `set -euo pipefail`.

*No test framework install — pure POSIX tools + sbt (already present).*

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Test Instructions |
|----------|-------------|------------|-------------------|
| Tone matches upstream maintainer audience (terse, no GSD nomenclature, no "we"/"our") | DOC-01 | Stylistic judgment | Read MIGRATION.md end-to-end before PR |
| PR diff contains only `MIGRATION.md` and (optionally) the check script outside `.planning/` | Success Criterion 5, project rule | GitHub UI inspection | `git diff master -- ':!.planning/'` |
| User ack before push and before PR open | Success Criterion 5 | Human gate | Workflow checkpoints |

---

## Validation Sign-Off

- [ ] All tasks have `<automated>` verify or Wave 0 dependencies
- [ ] Sampling continuity: no 3 consecutive tasks without automated verify
- [ ] Wave 0 creates `check.sh`
- [ ] No watch-mode flags
- [ ] Feedback latency < 5s for grep checks
- [ ] `nyquist_compliant: true` set in frontmatter after Wave 0 done

**Approval:** pending
