---
phase: 4
slug: meta-derivation-core
status: draft
nyquist_compliant: true
wave_0_complete: false
created: 2026-06-02
---

# Phase 4 — Validation Strategy

> Per-phase validation contract for feedback sampling during execution of the
> 04-meta-derivation-core stacked PR chain (slices 4.1 → 4.5).

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | ScalaTest (sbt-driven) |
| **Config file** | `build.sbt` + `project/Commons.scala` (Scala 3.8.2) |
| **Quick run command** | `sbt commons-core/compile` |
| **Full suite command** | `sbt clean compile Test/compile scalafmtCheckAll` |
| **Estimated runtime** | ~60–180 seconds (clean), ~10–30 seconds (incremental) |

---

## Sampling Rate

- **After every task commit:** Run `sbt commons-core/compile` (and `commons-core/Test/compile` for slices 4.2+).
- **After every plan / slice:** Run `sbt commons-core/compile commons-core/Test/compile scalafmtCheckAll`.
- **Before `/gsd:verify-work`:** Full phase gate must be green — `sbt clean compile Test/compile scalafmtCheckAll`.
- **Max feedback latency:** ~180 seconds for full phase gate; ~30 seconds for per-task quick check.

---

## Per-Slice Gates

### 3.8.2 compile gate (all slices)

```bash
sbt commons-core/compile   # MUST exit 0
```

### 3.8.2 test-compile gate (slices 4.2+)

```bash
sbt commons-core/Test/compile   # MUST exit 0
```

Slice 4.1 does not require Test/compile (foundation files have no dedicated tests landing in 4.1).

### scalafmt gate (all slices)

```bash
sbt scalafmtCheckAll   # MUST exit 0
```

### Fork-shape parity (all slices)

Per-file semantic `diff` against `origin/master:core/src/main/scala-3/com/avsystem/commons/meta/<file>.scala`.
Significant-indentation drift is acceptable (Scala 3 reformatter may normalize). Use grep counts of
top-level declarations (`sealed|case|object|trait|class|given|inline|def`) to assert structural parity.

```bash
for f in AllowDerivation Fallback OptionLike metadata MacroInstances MetaMacros MetadataCompanion AdtMetadataCompanion metaAnnotations; do
  forkCount=$(git show origin/master:core/src/main/scala-3/com/avsystem/commons/meta/$f.scala 2>/dev/null \
    | grep -cE '^(sealed|case|object|trait|class|given|inline|def) ')
  ourCount=$(grep -cE '^(sealed|case|object|trait|class|given|inline|def) ' \
    core/src/main/scala/com/avsystem/commons/meta/$f.scala 2>/dev/null)
  echo "$f fork=$forkCount ours=$ourCount"
done
```

Expected: counts match except documented divergences (OptionLike's `BaseOptionLike` shim).

### No new @nowarn / -Wconf (all slices)

```bash
git diff <base>..HEAD | grep -E '^\+.*(@nowarn|-Wconf)'   # MUST be empty
```

Where `<base>` = `upstream/scala-3` (Phase 4 root) or the predecessor slice branch.

### Test un-wrap targets (per-slice)

| Slice | Target | Strategy |
|-------|--------|----------|
| 4.1 | none | Foundation files have no dedicated tests. |
| 4.2 | `MacroInstancesTest` | Un-wrap **deferred to slice 4.5** (gated on AdtMetadataCompanion landing in 4.4). |
| 4.3 | none | `MetadataCompanion` tested transitively via dependent suites in 4.4 / 4.5. |
| 4.4 | `AdtMetadataTest.scala` | Selectively un-wrap. Compile-time / implicit-resolution checks → live. Runtime-macro cases (route through `MetaMacros.dummy '{ ??? }`) → marked `pending`. |
| 4.5 | `MacroInstancesTest.scala` | Un-wrap. Classical-trait `Instances` declarations rewritten as named-tuple type aliases (per slice 4.2 `Instances <: AnyNamedTuple` bound). Runtime-macro-dependent cases → marked `pending`. |

**No suppression annotations:** `@nowarn` / `@ignore` are forbidden in un-wrapped tests (per
`feedback_fix_dont_suppress_warnings`). Use ScalaTest's `pending` idiom exclusively.

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|-----------|-------------------|-------------|--------|
| 04-01-01 | 01 | 1 | META-CORE-03, META-CORE-05, META-CORE-06 | Wave-0 probe | `test -f /tmp/phase4-wave0-probes.txt && grep -q NAMEDTUPLE_PROBE=OK /tmp/phase4-wave0-probes.txt && grep -q GENCODEC_GIVEN_PROBE=OK /tmp/phase4-wave0-probes.txt && grep -q CELLAR_MADE_PROBE=OK /tmp/phase4-wave0-probes.txt` | ✅ created in-task | ⬜ pending |
| 04-01-02 | 01 | 1 | META-CORE-03 | compile | `sbt commons-core/compile` | ✅ | ⬜ pending |
| 04-01-03 | 01 | 1 | META-CORE-05, META-CORE-06 | compile | `grep -c 'dupa' core/src/main/scala/com/avsystem/commons/meta/metadata.scala; sbt commons-core/compile` | ✅ | ⬜ pending |
| 04-01-04 | 01 | 1 | DOC-02 | docs | `grep -c 'slice 4.1' MIGRATION.md` | ✅ | ⬜ pending |
| 04-01-05 | 01 | 1 | PR-01..03, WORKFLOW-01..05 | gh API | `gh pr list --repo AVSystem/scala-commons --head halotukozak:04-01-foundation --json isDraft \| grep -c '"isDraft": true'` | n/a | ⬜ pending |
| 04-02-01 | 02 | 2 | META-CORE-01, META-CORE-02 | compile+test-compile | `sbt commons-core/compile && sbt commons-core/Test/compile` | ✅ | ⬜ pending |
| 04-02-02 | 02 | 2 | DOC-02 | docs | `grep -c 'slice 4.2' MIGRATION.md` | ✅ | ⬜ pending |
| 04-02-03 | 02 | 2 | PR-01..03, WORKFLOW-01..05 | gh API | `gh pr list --repo AVSystem/scala-commons --head halotukozak:04-02-macro-instances --json isDraft \| grep -c '"isDraft": true'` | n/a | ⬜ pending |
| 04-03-01 | 03 | 3 | META-CORE-02, META-CORE-04 | compile | `sbt commons-core/compile` | ✅ | ⬜ pending |
| 04-03-02 | 03 | 3 | META-CORE-04 | compile+test-compile | `sbt commons-core/compile && sbt commons-core/Test/compile` | ✅ | ⬜ pending |
| 04-03-03 | 03 | 3 | DOC-02 | docs | `grep -c 'slice 4.3' MIGRATION.md` | ✅ | ⬜ pending |
| 04-03-04 | 03 | 3 | PR-01..03, WORKFLOW-01..05 | gh API | `gh pr list --repo AVSystem/scala-commons --head halotukozak:04-03-meta-macros --json isDraft \| grep -c '"isDraft": true'` | n/a | ⬜ pending |
| 04-04-01 | 04 | 4 | META-CORE-02, META-CORE-07 | compile+test-compile | `sbt commons-core/compile && sbt commons-core/Test/compile` | ✅ | ⬜ pending |
| 04-04-02 | 04 | 4 | META-CORE-07 | test run | `sbt 'commons-core/testOnly *AdtMetadataTest'` | ✅ un-wrap | ⬜ pending |
| 04-04-03 | 04 | 4 | DOC-02 | docs | `grep -c 'slice 4.4' MIGRATION.md` | ✅ | ⬜ pending |
| 04-04-04 | 04 | 4 | PR-01..03, WORKFLOW-01..05 | gh API | `gh pr list --repo AVSystem/scala-commons --head halotukozak:04-04-adt-metadata-companion --json isDraft \| grep -c '"isDraft": true'` | n/a | ⬜ pending |
| 04-05-01 | 05 | 5 | META-CORE-02, META-CORE-07 | compile+test-compile | `sbt commons-core/compile && sbt commons-core/Test/compile` | ✅ | ⬜ pending |
| 04-05-02 | 05 | 5 | META-CORE-07 | test run | `sbt 'commons-core/testOnly *MacroInstancesTest'` | ✅ un-wrap | ⬜ pending |
| 04-05-03 | 05 | 5 | DOC-02 | docs+full gate | `sbt compile && sbt Test/compile && sbt scalafmtCheckAll` | n/a | ⬜ pending |
| 04-05-04 | 05 | 5 | PR-01..03, WORKFLOW-01..05 | gh API | `gh pr list --repo AVSystem/scala-commons --head halotukozak:04-05-meta-annotations --json isDraft \| grep -c '"isDraft": true'` | n/a | ⬜ pending |

*Status: ⬜ pending · ✅ green · ❌ red · ⚠️ flaky*

---

## Wave 0 Requirements

Slice 4.1 Task 1 produces `/tmp/phase4-wave0-probes.txt` containing the three Wave-0 probe results:

- [ ] `NAMEDTUPLE_PROBE=OK` — `scala.NamedTuple.{AnyNamedTuple,DropNames}` compiles on 3.8.2 without `-language:experimental.namedTuples`.
- [ ] `GENCODEC_GIVEN_PROBE=OK` — count of `given` declarations in `GenCodec.scala` recorded (zero is acceptable — fork ships a silent no-op wildcard import).
- [ ] `CELLAR_MADE_PROBE=OK` — all three `cellar get-external io.github.halotukozak:made_3:0.1.1` lookups returned signatures (`made.annotation.transparent`, `made.annotation.name`, `made.Default`).

No new test framework installation required — existing ScalaTest infrastructure from Phase 1 covers Phase 4 needs.

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Test Instructions |
|----------|-------------|------------|-------------------|
| PR review / merge sequencing on AVSystem/scala-commons | PR-01..03 | External reviewer action | Maintainer reviews 4.1 → 4.2 → 4.3 → 4.4 → 4.5 stack in order. Claude only opens drafts. |
| MiMa bincompat flagging | META-CORE-07 / §4 | MiMa re-enables in Phase 11 | Bound-tightening on `AdtMetadataCompanion` will be flagged by MiMa in Phase 11 — Phase 4 only documents the narrowing in MIGRATION.md §4. |

All Phase 4 code behaviors have automated `sbt`-driven verification. Runtime semantics that exercise
`MetaMacros.{valueImpl, lazyMetadataImpl, dummy}` `'{ ??? }` bodies are deferred to Phase 6 — tests
that would invoke those paths are marked `pending` (not manually verified, not skipped).

---

## Yolo Mode Note

Project is in `mode: yolo` per `.planning/config.json`. WORKFLOW-03 (user-ack before push / PR-open)
is exempted under yolo for all Phase 4 slices. PRs are still opened `--draft`; the user flips to
ready-for-review manually per `feedback_pr_draft`.

---

## Validation Sign-Off

- [x] All tasks have `<automated>` verify or Wave 0 dependencies
- [x] Sampling continuity: no 3 consecutive tasks without automated verify
- [x] Wave 0 covers all probes (named-tuple, GenCodec.given, cellar made.*)
- [x] No watch-mode flags
- [x] Feedback latency < 180s (full phase gate); < 30s (per-task quick)
- [x] `nyquist_compliant: true` set in frontmatter

**Approval:** pending — phase to begin execution
