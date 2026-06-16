---
phase: 1
slug: big-bang-comment-and-green
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-06-01
---

# Phase 1 — Validation Strategy

> Per-phase validation contract for the big-bang Scala-3-only pivot PR.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | sbt + scalafmt + GitHub Actions CI |
| **Config file** | `project/Commons.scala`, `.scalafmt.conf`, `.github/workflows/ci.yml` |
| **Quick run command** | `sbt -batch compile` |
| **Full suite command** | `sbt -batch ';scalafmtCheckAll ;compile ;Test/compile'` |
| **Estimated runtime** | ~30s loaded build · ~3-5 min cold compile · ~6-10 min CI per shard |

---

## Sampling Rate

- **After every task commit:** `sbt 'show version'` (loads) + `sbt scalafmtCheckAll` if formatted files touched.
- **After every plan wave:** the full suite.
- **Before push:** full suite + `git diff` hygiene (no `.planning/`, no `crossScalaVersions`, no `scala-2.13/` paths in commits).
- **Max feedback latency:** ~5 min warm; ~10 min cold.

---

## Per-Task Verification Map (rough; planner refines)

| Plan | Wave | Requirement | Test Type | Automated Command |
|------|------|-------------|-----------|-------------------|
| 01 (build pivot) | 1 | BUILD-01..05 | build-load | `sbt 'show jvm/scalaVersion'` returns `3.8.2` |
| 01 | 1 | BUILD-04 | grep | `grep -q 'runner.dialect = scala3' .scalafmt.conf && ! grep -q 'scala213source3' .scalafmt.conf` |
| 01 | 1 | BUILD-03 | grep | `! find . -type d -name 'scala-2.13' -not -path '*/_archive_*' -not -path '*/__*__/*'` |
| 02 (macros) | 2 | MACROS / COMMENT | sbt | `sbt -batch '++3 commons-macros/compile'` exits 0 |
| 03 (core) | 3 | COMPILE-01 | sbt | `sbt -batch '++3 commons-core/compile'` exits 0 |
| 04 (other JVM) | 4 | MOD-01..05 | sbt | `sbt -batch '++3 commons-jvm/compile'` exits 0 |
| 05 (JS) | 5 | JS-01 | sbt | `sbt -batch '++3 commons-js/compile'` exits 0 |
| 06 (tests) | 6 | TEST-01 | sbt | `sbt -batch '++3 Test/compile'` exits 0 |
| 07 (MIGRATION) | 7 | DOC-01..02 | grep | sections present + every TODO has effort tag |
| 08 (push+PR) | 8 | CI-01..02 | gh | `gh run watch` exits 0; PR draft + milestone + prefix |

(Plan split refined by planner — 5-6 plans likely sufficient.)

---

## Wave 0 Requirements

- [ ] Verify upstream baseline: `git fetch upstream; git show upstream/scala-3 -- project/Commons.scala` matches research expectation (no `crossScalaVersions`).
- [ ] `cellar get-external io.github.halotukozak:made_3:0.1.1 made.Default` resolves.
- [ ] Java 17 + 21 + 25 toolchains available locally (matches CI matrix).
- [ ] Fork has clean `01-big-bang` branch slot (or use whatever name planner chooses).

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Instructions |
|----------|-------------|------------|--------------|
| MIGRATION.md sections complete (Will not migrate / Deprecated / Source-compat / Bincompat / Disabled) | DOC-01 | Stylistic audit | Reviewer reads end-to-end before PR ready-for-review flip |
| Commented blocks tagged `// TODO[scala3-port]:` consistently | COMMENT-02 | Free-text audit | `git grep -nE 'TODO\[scala3-port\]'` should match commented-block count ±2 |
| No GSD nomenclature in commits / PR body | WORKFLOW-04 | Free-text check | Reviewer scans commit log + PR body |
| User ack before push + before PR open | WORKFLOW-03 | Human gate | Workflow checkpoints |

---

## Validation Sign-Off

- [ ] All tasks have `<automated>` verify or Wave 0 dependency
- [ ] Sampling continuity holds (no 3-consecutive-task automated-coverage gap)
- [ ] Wave 0 preflights pass
- [ ] No watch-mode flags
- [ ] Feedback latency < 600s (full suite)
- [ ] `nyquist_compliant: true` after Wave 0 done

**Approval:** pending
