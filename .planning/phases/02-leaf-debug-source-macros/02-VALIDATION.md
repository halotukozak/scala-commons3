---
phase: 02
slug: leaf-debug-source-macros
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-06-01
---

# Phase 02 — Validation Strategy

> Per-phase validation contract for feedback sampling during execution.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | ScalaTest 3.2.20 (sbt) |
| **Config file** | `build.sbt` + `project/Commons.scala` |
| **Quick run command** | `sbt -batch 'core/testQuick'` (or per-slice: `sbt -batch 'commons-core/testOnly *<Slice>Test'`) |
| **Full suite command** | `sbt -batch 'compile; Test/compile; scalafmtCheckAll; scalafmtSbtCheck'` |
| **Estimated runtime** | ~30-60s quick, ~3-5min full |

---

## Sampling Rate

- **After every task commit:** Run targeted `testOnly` for the slice being modified
- **After every plan completion:** Run `sbt -batch 'compile; Test/compile'`
- **Before opening PR:** Full suite must be green + `scalafmtCheckAll`
- **Max feedback latency:** ~60s per slice

---

## Observable Signals (per slice)

Each Phase 2 slice produces these grep-verifiable signals:

| Slice | Source signal | Test signal | MIGRATION signal |
|-------|---------------|-------------|------------------|
| debug-reify | `inline def show` present in `SharedExtensions.scala`; no `??? // TODO[scala3-port]` for show*/sourceCode | `testOnly *SharedExtensionsTest*` green | Rows for `SharedExtensions.scala:129-147` removed from §6 Backlog |
| source-positions | `inline def here` in `positioned.scala` + `SourceInfo.scala`; no `???` on those lines | Smoke test asserts `SourceInfo.here.lineNumber > 0` | Rows for `positioned.scala:12`, `SourceInfo.scala:28` removed |
| implicit-lookup | `inline def infer` + `inline def inferNonMacro` in `Implicits.scala` | Smoke test: `Implicits.infer[Ordering[Int]]` resolves | Rows for `Implicits.scala:5-9` removed; MIGRATION §3 entry for `inferNonMacro` narrowing |
| class-name | `inline def materialize` in `SimpleClassName.scala`; no `???` | `testOnly *SimpleClassNameTest*` green (restored from `/* */`) | Row for `SimpleClassName.scala:8` removed |

SAM slice **DROPPED** (deprecated APIs per [[feedback-dont-port-deprecated]]) — Sam.scala / SamCompanion.scala files deleted; MIGRATION §1 (Will Not Migrate) adds entry.

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|-----------|-------------------|-------------|--------|
| 02-01-T1 | 02-01-debug-reify | 1 | DEBUG-01, DEBUG-02 | compile + grep | `sbt -batch 'commons-core/compile' && grep -q 'inline def showAst' core/src/main/scala/com/avsystem/commons/SharedExtensions.scala && grep -q 'def showAstImpl\[A: Type\]' core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala && ! grep -nE 'TODO\[scala3-port\]: show\|TODO\[scala3-port\]: sourceCode\|TODO\[scala3-port\]: withSourceCode' core/src/main/scala/com/avsystem/commons/SharedExtensions.scala && sbt -batch 'scalafmtCheckAll'` | yes | pending |
| 02-01-T2 | 02-01-debug-reify | 1 | DEBUG-01, DEBUG-02 | unit test | `sbt -batch 'commons-core/testOnly com.avsystem.commons.SharedExtensionsShowTest'` | yes | pending |
| 02-01-T3 | 02-01-debug-reify | 1 | DEBUG-01, DEBUG-02 | docs grep | `! grep -nE 'SharedExtensions\.scala:(129\|131\|133\|135\|137\|139\|141\|143\|145\|147)' MIGRATION.md && grep -qE 'Total tags: [0-9]+' MIGRATION.md` | yes | pending |
| 02-02-T1 | 02-02-source-positions | 1 | POS-01, POS-02 | compile + grep | `sbt -batch 'commons-core/compile' && grep -q 'inline def here' core/src/main/scala/com/avsystem/commons/annotation/positioned.scala && grep -q 'inline implicit def here' core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala && sbt -batch 'scalafmtCheckAll'` | yes | pending |
| 02-02-T2 | 02-02-source-positions | 1 | POS-01, POS-02 | unit test | `sbt -batch 'commons-core/testOnly com.avsystem.commons.annotation.PositionedTest com.avsystem.commons.misc.SourceInfoTest'` | yes | pending |
| 02-02-T3 | 02-02-source-positions | 1 | POS-01, POS-02 | docs grep | `! grep -nE 'positioned\.scala:12\|SourceInfo\.scala:28' MIGRATION.md` | yes | pending |
| 02-03-T1 | 02-03-implicit-lookup | 1 | IMPL-01 | compile + grep | `sbt -batch 'commons-core/compile' && grep -q 'inline def infer' core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && grep -q 'inline def inferNonMacro' core/src/main/scala/com/avsystem/commons/misc/Implicits.scala && grep -q 'def inferImpl' core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala && sbt -batch 'scalafmtCheckAll'` | yes | pending |
| 02-03-T2 | 02-03-implicit-lookup | 1 | IMPL-01 | unit test | `sbt -batch 'commons-core/testOnly com.avsystem.commons.misc.ImplicitsTest'` | yes | pending |
| 02-03-T3 | 02-03-implicit-lookup | 1 | IMPL-01 | docs grep | `! grep -nE 'Implicits\.scala:(5\|7\|9)' MIGRATION.md && grep -q 'inferNonMacro' MIGRATION.md` | yes | pending |
| 02-04-T1 | 02-04-class-name | 1 | CLS-01 | compile + grep | `sbt -batch 'commons-core/compile' && grep -q 'inline implicit def materialize' core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala && grep -q 'def materializeImpl' core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala && sbt -batch 'scalafmtCheckAll'` | yes | pending |
| 02-04-T2 | 02-04-class-name | 1 | CLS-01 | unit test | `sbt -batch 'commons-core/testOnly com.avsystem.commons.misc.SimpleClassNameTest'` | yes | pending |
| 02-04-T3 | 02-04-class-name | 1 | CLS-01 | docs grep | `! grep -nE 'SimpleClassName\.scala:8' MIGRATION.md` | yes | pending |
| 02-05-T1 | 02-05-drop-sam | 1 | SAM-01 | deletion + compile + ref-check | `! test -f core/src/main/scala/com/avsystem/commons/misc/Sam.scala && ! test -f core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala && sbt -batch 'commons-core/compile' && ! grep -rlE '\b(Sam\|SamCompanion)\b' core/src/ mongo/ hocon/ 2>/dev/null \| grep -vE 'Sam\.scala\|SamCompanion\.scala\|MIGRATION\.md'` | yes | pending |
| 02-05-T2 | 02-05-drop-sam | 1 | SAM-01 | docs grep | `grep -q 'Sam' MIGRATION.md && ! grep -nE 'Sam\.scala:9\|SamCompanion\.scala:(11\|19)' MIGRATION.md` | yes | pending |

---

## Nyquist Compliance Checklist

- [ ] Every slice plan declares ≥1 observable signal in its `must_haves`
- [ ] Every slice plan has a smoke test or restored original test
- [ ] Every slice PR removes the corresponding MIGRATION backlog row
- [ ] CI green gate enforced before PR open
