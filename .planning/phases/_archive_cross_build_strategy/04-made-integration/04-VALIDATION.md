---
phase: 4
slug: made-integration
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-05-30
---

# Phase 4 — Validation Strategy

> Per-phase validation contract for the made-integration slice.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | sbt (build-level validation — Phase 4 ships no new tests) |
| **Config file** | `build.sbt`, `project/Commons.scala` |
| **Quick run command** | `sbt -batch '++3 core/compile'` |
| **Full suite command** | `sbt -batch scalafmtCheckAll; sbt -batch '++3 core/compile'; sbt -batch '++3 core-js/compile'; sbt -batch '++2.13 core/compile'` |
| **Estimated runtime** | ~2 min warm core/compile; ~5 min full |

---

## Sampling Rate

- **After every task commit:** `sbt '++3 core/compile'`.
- **After every plan wave:** full suite above.
- **Before push:** full suite + `git diff` hygiene (no `.planning/`, no SNAPSHOT, no `0.1.1`).
- **Max feedback latency:** ~2 min warm, ~5 min cold.

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | Status |
|---------|------|------|-------------|-----------|-------------------|--------|
| 4-01-01 | 01 | 1 | INFRA-06 | build-edit | `grep -q 'madeVersion = "0.1.0"' build.sbt && ! grep -q '0.1.1-SNAPSHOT' build.sbt` | ⬜ |
| 4-01-02 | 01 | 1 | INFRA-06 | build-load | `sbt -batch 'show jvm/version'` exits 0 | ⬜ |
| 4-02-01 | 02 | 2 | MADE-01 | file-port | each of `Opt.scala`, `NOpt.scala`, `OptArg.scala`, `OptRef.scala` exists at `core/src/main/scala-3/com/avsystem/commons/misc/` and has `import made.Default` | ⬜ |
| 4-02-02 | 02 | 2 | MADE-01 | file-port | `core/src/main/scala-3/com/avsystem/commons/serialization/madeAnnotationAliases.scala` exists + re-exports the minimal annotation surface | ⬜ |
| 4-02-03 | 02 | 2 | MADE-01 | content | `extends OptCompat\|NOptCompat\|OptRefCompat` removed from companions (per research recommendation) | ⬜ |
| 4-03-01 | 03 | 3 | MADE-01 | compile | `sbt -batch '++3 core/compile'` exits 0 | ⬜ |
| 4-03-02 | 03 | 3 | MADE-01 | compile | `sbt -batch '++3 core-js/compile'` exits 0 | ⬜ |
| 4-03-03 | 03 | 3 | (regression) | compile | `sbt -batch '++2.13 core/compile'` exits 0 | ⬜ |
| 4-03-04 | 03 | 3 | QUALITY-01 | grep | `! grep -RnE '@nowarn\|-Wconf' core/src/main/scala-3/com/avsystem/commons/misc/{Opt,NOpt,OptArg,OptRef}.scala` | ⬜ |
| 4-04-01 | 04 | 4 | DOC-02 | docs | `MIGRATION.md` has new row `\| made \|` AND `core` row Notes mention `made wiring primitives ported` | ⬜ |
| 4-05-01 | 05 | 5 | WORKFLOW-03 | gate | human-verify checkpoint before push | ⬜ |
| 4-05-02 | 05 | 5 | WORKFLOW-02 | gate | human-verify checkpoint before `gh pr create` against `AVSystem/scala-commons:scala-3` | ⬜ |

*Status: ⬜ pending · ✅ green · ❌ red · ⚠️ flaky*

---

## Wave 0 Requirements

- [ ] Verify `++2.13 core/compile` green on baseline before any port (pre-flight).
- [ ] Confirm Java 17 toolchain (matches CI matrix from Phase 1).
- [ ] `cellar` available for any signature lookup the executor needs mid-port.

*No test framework install.*

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Test Instructions |
|----------|-------------|------------|-------------------|
| Diff scope: only `build.sbt`, 4-5 source files under `core/src/main/scala-3/`, and `MIGRATION.md` | Success Criterion (minimum island) | Stylistic / reviewability | `git diff master --stat` |
| No GSD nomenclature in commits or PR body | WORKFLOW-04, WORKFLOW-05 | Free-text check | Reviewer reads commit messages and PR body |
| User ack before push and before PR | WORKFLOW-03 | Human gate | Workflow checkpoints |

---

## Validation Sign-Off

- [ ] All tasks have `<automated>` verify or Wave 0 dependency
- [ ] Sampling continuity holds (no 3 consecutive task gaps)
- [ ] `++2.13 core/compile` green pre-flight (Wave 0)
- [ ] `nyquist_compliant: true` after Wave 0 done

**Approval:** pending
