---
phase: 1
slug: cross-compile-infrastructure
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-05-30
---

# Phase 1 — Validation Strategy

> Per-phase validation contract for feedback sampling during execution.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | sbt + scalatest (existing) — Phase 1 has no source tests; validation is build-level |
| **Config file** | `build.sbt`, `project/plugins.sbt`, `.scalafmt.conf`, `.github/workflows/ci.yml`, `Makefile` |
| **Quick run command** | `sbt scalafmtCheckAll` (~10s after first run) |
| **Full suite command** | `make ci` (runs all 5 gate commands) |
| **Estimated runtime** | ~12 min cold, ~3 min warm |

---

## Sampling Rate

- **After every task commit:** Run `sbt 'show version'` (config syntactic validity) + `sbt scalafmtCheckAll` when `.scalafmt.conf` or formatted file touched
- **After every plan wave:** Run `make ci` (or its individual targets) to assert all 5 gate commands green
- **Before `/gsd:verify-work`:** Full `make ci` green on Java 17; CI matrix green on PR
- **Max feedback latency:** ~30s for sbt reload; ~12 min for full `make ci` cold

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|-----------|-------------------|-------------|--------|
| 1-01-01 | 01 | 1 | INFRA-01,02 | build-load | `sbt 'show jvm/crossScalaVersions'` | ✅ | ⬜ pending |
| 1-01-02 | 01 | 1 | INFRA-03 | build-load | `sbt 'show jvm2/crossScalaVersions'` | ✅ | ⬜ pending |
| 1-01-03 | 01 | 1 | INFRA-04 | build-load | `sbt 'show jetty/skip'` | ✅ | ⬜ pending |
| 1-02-01 | 02 | 1 | INFRA-05 | plugin-load | `sbt 'reload' 'plugins'` (grep mima 1.1.5, tasty-mima 1.4.0) | ✅ | ⬜ pending |
| 1-02-02 | 02 | 1 | INFRA-06 | dep-resolve | `sbt 'show made/version'` or library lookup | ✅ | ⬜ pending |
| 1-03-01 | 03 | 2 | QUALITY-01 | format-check | `sbt scalafmtCheckAll` | ✅ | ⬜ pending |
| 1-03-02 | 03 | 2 | QUALITY-03 | format-check | dialect verification via `.scalafmt.conf` fileOverride | ✅ | ⬜ pending |
| 1-04-01 | 04 | 2 | INFRA-07, WORKFLOW-01..05 | yaml-lint + sbt | `sbt githubWorkflowCheck` | ✅ | ⬜ pending |
| 1-04-02 | 04 | 2 | INFRA-08, INFRA-09 | makefile | `make -n ci` (dry-run) + `make ci` | ✅ | ⬜ pending |

*Status: ⬜ pending · ✅ green · ❌ red · ⚠️ flaky*

---

## Wave 0 Requirements

- [ ] No new test framework needed — sbt itself is the validation harness.
- [ ] Ensure Java 17 toolchain available locally (matches CI matrix).

*Existing build infrastructure covers all phase requirements once sbt loads.*

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Test Instructions |
|----------|-------------|------------|-------------------|
| Clean local checkout reproduces green matrix | Success Criterion 5 | Requires fresh clone outside this worktree | `git clone <fork> /tmp/sc-clean && cd /tmp/sc-clean && git checkout scala-3 && make ci` |
| PR opened on upstream/scala-3 with no `.planning/` and no GSD nomenclature in commits | Success Criterion 6 | GitHub-side verification by human | Inspect PR diff and commit messages before submission |

---

## Validation Sign-Off

- [ ] All tasks have `<automated>` verify or Wave 0 dependencies
- [ ] Sampling continuity: no 3 consecutive tasks without automated verify
- [ ] Wave 0 covers all MISSING references
- [ ] No watch-mode flags
- [ ] Feedback latency < 720s (full `make ci` cold)
- [ ] `nyquist_compliant: true` set in frontmatter

**Approval:** pending
