---
phase: 5
slug: leaf-feature-restoration
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-06-02
---

# Phase 5 — Validation Strategy

> Per-phase validation contract for feedback sampling during execution.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | ScalaTest 3.2.x (`AnyFunSuite`, `AnyWordSpec`) |
| **Config file** | `project/Commons.scala` (libraryDependencies) |
| **Quick run command** | `sbt 'commons-core/testOnly com.avsystem.commons.misc.<Feature>Test'` |
| **Full suite command** | `sbt 'commons-core/compile ;commons-core/test ;scalafmtCheckAll'` |
| **Estimated runtime** | quick ~10-30s · full ~2-5 min |

---

## Sampling Rate

- **After every task commit:** Quick run on the touched leaf's test (10-30s)
- **After every plan wave:** Full suite (`commons-core/compile ;commons-core/test ;scalafmtCheckAll`)
- **Before `/gsd:verify-work`:** Full suite green AND `git grep '???' core/src/main/scala/com/avsystem/commons/misc/` shows only `Delegation` + `DelegationApply` stubs (match fork state)
- **Max feedback latency:** 30s per task, 5 min per wave

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|-----------|-------------------|-------------|--------|
| 5-00-01 | 00 (foundation) | 0 | infra | compile | `sbt commons-core/compile` | ✅ | ⬜ pending |
| 5-01-01 | 01 TypeString+JavaClassName | 1 | TYPESTRING-01, JAVACLASSNAME-01 | unit | `sbt 'commons-core/testOnly *.SharedExtensionsTest'` | partial (un-wrap during slice) | ⬜ pending |
| 5-02-01 | 02 AnnotationOf family | 1 | ANNOTOF-01 | unit | `sbt 'commons-core/testOnly *.AnnotationOfTest'` | wrapped (un-wrap during slice) | ⬜ pending |
| 5-03-01 | 03 ApplierUnapplier | 1 | APPLIERUNAPPLIER-01 | unit | `sbt 'commons-core/testOnly *.ApplierUnapplierTest'` | wrapped (un-wrap during slice) | ⬜ pending |
| 5-04-01 | 04 SealedUtils | 1 | SEALEDUTILS-01 | unit | `sbt 'commons-core/testOnly *.SealedEnumTest *.NamedEnumTest'` | wrapped (un-wrap during slice) | ⬜ pending |
| 5-05-01 | 05 ValueEnum | 1 | VALUEENUM-01 | unit | `sbt 'commons-core/testOnly *.ValueEnumTest'` | wrapped (un-wrap during slice) | ⬜ pending |
| 5-06-01 | 06 Bidirectional | 1 | BIDIRECTIONAL-01 | compile-test | `sbt commons-core/compile` (no test — fork dropped) | n/a (fork DROPPED) | ⬜ pending |
| 5-07-01 | 07 Delegation | 1 | DELEGATION-01 | manual-only | n/a (test stays `ignore`d, matches fork) | wrapped + ignored | ⬜ pending |

*Status: ⬜ pending · ✅ green · ❌ red · ⚠️ flaky*

---

## Wave 0 Requirements

- [ ] None — test infrastructure exists; per-slice un-wrap is the action, not authoring.

Existing infrastructure covers all phase requirements. Fork tests in `origin/master:core/src/test/scala-3/com/avsystem/commons/misc/<Feature>Test.scala` available for cribbing if our wrapped versions diverge.

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Test Instructions |
|----------|-------------|------------|-------------------|
| `Delegation` stub compiles, runtime throws NotImplementedError | DELEGATION-01 | Fork keeps `???` body + `ignore`d test — real impl is Phase 6+ scope | Confirm `git grep '???' .../misc/Delegation*` matches fork shape; matching test stays `ignore`d |
| `Bidirectional` callers fail at COMPILE time | BIDIRECTIONAL-01 | `compiletime.error` is compile-time only; verify by hand-writing a transient call site if doubt | Add temporary `Bidirectional({case x: Int => x.toString})` line, confirm `sbt compile` aborts with the fork error message, then remove |

---

## Validation Sign-Off

- [ ] All tasks have `<automated>` verify or Wave 0 dependencies
- [ ] Sampling continuity: no 3 consecutive tasks without automated verify
- [ ] Wave 0 covers all MISSING references (N/A — none)
- [ ] No watch-mode flags
- [ ] Feedback latency < 30s per task
- [ ] `nyquist_compliant: true` set in frontmatter

**Approval:** pending
