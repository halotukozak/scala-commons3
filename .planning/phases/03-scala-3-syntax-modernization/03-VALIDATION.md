---
phase: 3
slug: scala-3-syntax-modernization
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-06-01
---

# Phase 3 — Validation Strategy

> Per-slice validation contract for feedback sampling during execution. Phase 3 is mechanical syntax rewrite — validation is compiler + scalafmt + grep gates, not new unit tests.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | sbt + ScalaTest (existing) |
| **Config file** | `build.sbt`, `project/Commons.scala` |
| **Quick run command** | `sbt commons-core/compile Test/compile` |
| **Full suite command** | `sbt compile Test/compile scalafmtCheckAll` |
| **Estimated runtime** | ~90 seconds compile-only; ~3–5 min with `scalafmtCheckAll` |

---

## Sampling Rate

- **After every file edit batch:** Run `sbt commons-core/compile` (or relevant module).
- **After every slice (PR-ready):** Run full suite + per-slice grep gate.
- **Before opening PR:** `scalafmtCheckAll` must pass; per-slice grep gate must return 0 hits in scope.
- **Max feedback latency:** ~90s compile, ~5min full.

---

## Per-Slice Verification Gates

**Merge order:** 3.1 → 3.2 → 3.3 → 3.4 (sequential — enforced via PR body metadata, not git topology). Slice 3.5 is **parallel-independent** — can land any time (no overlap with 3.1/3.2/3.3/3.4 file sets).

**Grep scope standard (use directory paths, NOT shell globs — `git grep` does not shell-expand `**/*.scala`):**
```
-- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala' 'hocon/src/main/scala'
```

| Slice | Idiom | Gate command | Pass criterion |
|-------|-------|--------------|----------------|
| 3.1 | `implicit class` → `extension` | `git grep -n 'implicit class' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala'` | 0 hits (excl. documented exceptions in MIGRATION.md) |
| 3.2 | `F[_]` → `F[?]` (applied position only) | `git grep -nE '\[_(\s*,\s*_)*\]' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala' 'hocon/src/main/scala'` then manual classify | 0 hits in applied positions; kind-decl `K[_]` preserved |
| 3.3 | `implicit def/val` → `given` | `git grep -nE '^\s*(inline\s+)?implicit\s+(def\|val)' -- 'core/src/main/scala' 'core/jvm/src/main/scala' 'core/js/src/main/scala' 'mongo/jvm/src/main/scala' 'mongo/js/src/main/scala' 'hocon/src/main/scala' 'benchmark/jvm/src/main/scala'` | exactly 2 documented exceptions (`OptArg.argToOptArg`, `SerializationMacros.fun2GenRef`) — both with verbatim fork explanatory comments |
| 3.4 | `@inline def` → `inline def` | `git grep -nE '@inline' -- 'core/src/main/scala' 'mongo/jvm/src/main/scala' \| grep -vE '(CborInput\|JsonStringInput\|RPCFramework)\.scala'` | 0 hits outside whitelist (CborInput / JsonStringInput / RPCFramework preserved verbatim per fork) |
| 3.5 | Delete `Implicits` object | `git ls-files core/src/main/scala/com/avsystem/commons/misc/Implicits.scala \| wc -l` AND `git ls-files core/src/main/scala/com/avsystem/commons/misc/ImplicitNotFound.scala \| wc -l` AND `git grep -nE '\bImplicits\.' -- '*.scala'` | first → 0 (deleted), second → 1 (created), third → 0 (no callers); `sbt compile` exits 0; MIGRATION.md §1 references removal |
| All | Compile clean | `sbt compile Test/compile` | exit 0, no new warnings |
| All | Format clean | `sbt scalafmtCheckAll` | exit 0 |
| All | No new suppressions | `git diff upstream/scala-3...HEAD -- '*.scala' \| grep -E '^\+.*(@nowarn\|-Wconf)' \| grep -v 'inline def discard'` | empty (only allowed carve-out is verbatim-fork `@nowarn` on `inline def discard` in slice 3.4) |
| All | Fork shape match | `diff <(git show origin/master:core/src/main/scala-3/<f>) <(cat core/src/main/scala/<f>)` (semantic, not byte-equal) | 1:1 transformation shape |

---

## Wave 0 Requirements

- [ ] Establish baseline: `git checkout upstream/scala-3 && sbt compile Test/compile scalafmtCheckAll` green before slice 3.1 starts.
- [ ] Fork commits accessible: `git fetch origin master` so `git show origin/master:<path>` works.
- [ ] No new test files needed — phase preserves existing test coverage.

---

## Manual-Only Verifications

| Behavior | Why Manual | Test Instructions |
|----------|------------|-------------------|
| Borderline `implicit` preserved correctly | Erasure-bridge / macro-splice cases; only humans can confirm fork's rationale carried over | For each kept implicit, inspect comment matches fork commit's explanatory text |
| Slice 3.2 false-positive classification | `[_, _]` appears in kind-decls (skip) and applied positions (rewrite); compiler can't tell intent | Per-file review: if `[_]` is in `class X[F[_]]` declaration → keep; if `Foo[Bar[_]]` applied → rewrite |
| Fork 1:1 shape match | Significant-indentation drift accepted; reviewer judges semantic equivalence | Side-by-side diff per file vs `origin/master:scala-3/<path>` |
| GitHub merge order honored | CI doesn't enforce sequential merge | Reviewer checks 3.2 PR not flipped to ready until 3.1 merged |

---

## GitHub Dependency Marking

Per user directive 2026-06-01: PRs are **NOT cascaded** (each off `upstream/scala-3` tip), but merge order must be visible.

Per-PR body MUST include (slices 3.1–3.4):

```
**Slice:** 3.X of Phase 3 (Scala 3 syntax modernization)
**Merge order:** 3.1 → 3.2 → 3.3 → 3.4
**Depends on:** #<prev-PR-number> (must merge first)
**Base branch:** upstream/scala-3 (not stacked on prior slice)
```

Slice 3.1 PR: no `Depends on`, just merge-order note.
Slice 3.2 PR: `Depends on: #<3.1>` — open as draft until 3.1 merges, then rebase + flip ready.
Slice 3.3 PR: `Depends on: #<3.2>` — same pattern.
Slice 3.4 PR: `Depends on: #<3.3>` — same pattern.

**Slice 3.5 PR (standalone parallel-safe):**
```
**Slice:** 3.5 of Phase 3 (Scala 3 syntax modernization)
**Merge order:** Independent — can land any time (no overlap with 3.1/3.2/3.3/3.4)
**Depends on:** none
**Base branch:** upstream/scala-3
```
Slice 3.5 has NO file overlap with 3.1/3.2/3.3/3.4 (touches only `Implicits.scala` + `ImplicitNotFound.scala`). Reviewer can land 3.5 in parallel with the sequential chain.

---

## Validation Sign-Off

- [ ] All slices have automated grep gate + compile gate
- [ ] Sampling continuity: every file edit followed by compile within slice
- [ ] No watch-mode flags
- [ ] Feedback latency < 5 min
- [ ] GitHub merge-order metadata present in each PR body
- [ ] `nyquist_compliant: true` set in frontmatter once approved

**Approval:** pending
