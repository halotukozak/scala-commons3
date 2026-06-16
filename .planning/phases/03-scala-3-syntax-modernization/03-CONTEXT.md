# Phase 3: Scala 3 syntax modernization — Context

**Gathered:** 2026-06-01
**Status:** Ready for planning

<domain>
## Phase Boundary

Sweep Scala 2 idioms out of the codebase in favor of native Scala 3 syntax. Land as **four sequential narrow-scope PRs**, each doing ONE rewrite kind, in this order:

1. **`implicit class XOps` → `extension`** (value-class extensions become extension blocks)
2. **HKT wildcard `_` → `?`** (`F[_]` higher-kinded bounds → `F[?]`)
3. **`implicit def/val` → `given`** (implicit definitions → given declarations)
4. **`@inline def` → `inline def`** (Scala 2 `@inline` hint → real Scala 3 `inline` — fork commits `33a5b792`, `5fafdbd7`, `ad505679`, `ee0be95e`, `a4ddad6b`, `580625a9`)

Each PR is self-contained, mechanical, parallel-unfriendly (no overlap with others by construction). All four sequenced so reviewer reads one transformation at a time. **PRs are NOT stacked** — each branches off `upstream/scala-3` tip; merge order enforced via PR body metadata, not git topology.

**In scope:** Pure syntax rewrites; no semantic changes. Feature ports remain deferred to Phase 4+.

</domain>

<decisions>
## Implementation Decisions

### Slice plan — three sequential PRs (NOT one big mechanical PR)

Supersedes the ROADMAP's "single big PR" wording. Three narrower PRs land in order:

| # | Title | What it rewrites |
|---|---|---|
| 3.1 | `[Scala 3] convert implicit class to extension` | `implicit class XOps[A](...) extends AnyVal { ... }` blocks → `extension [A] (a: A) { ... }` |
| 3.2 | `[Scala 3] tighten HKT wildcards (_ → ?)` | `F[_]` / `C[_, _]` in type-parameter bounds, type ascriptions, and existentials → `F[?]` / `C[?, ?]` |
| 3.3 | `[Scala 3] implicit def/val → given` | Implicit definitions → `given` declarations (with `using` for parameter lists where applicable) |
| 3.4 | `[Scala 3] @inline def → inline def` | `@inline def f(...) = ...` → `inline def f(...) = ...` (true Scala 3 inlining for function-taking ops, Opt/NOpt families, J* adapters, SharedExtensions, streams) |

Each PR has its own narrow acceptance gate (e.g. for 3.1: `git grep 'implicit class' core/src/main/scala/` → 0 hits; for 3.4: `git grep '@inline' core/src/main/scala/` → 0 hits in scope).

### Broader fork-vs-scala-3 comparison sweep (mandate)

Per user directive 2026-06-01: planner MUST compare halotukozak fork master source code against current `scala-3` branch state to identify ALL mechanical syntax/idiom rewrites fork applied. The four slices above are the known ones; planner should run a fork-diff sweep and surface any additional rewrite kinds (e.g. `compiletime.deferred` typo fixes, `Conversion` givens, explicit-nulls patches, `summon[T]` over `implicitly[T]`, `Symbol.newClass` patterns, named-tuple usage). Surface as deferred/future-slice candidates in RESEARCH.md if outside the four slices above.

### Method — translate from halotukozak's fork master, NOT scala3-migrate plugin

Originally ROADMAP suggested `sbt-scala3-migrate` plugin. User directive 2026-06-01: **translate from fork master commits instead** ([[feedback-crib-from-master]]).

Fork master has these working transformations already merged on `origin/master` branch:
- `39c047eb` — refactor(scala-3): eliminate implicit keyword in core; remove RPC module from scala-3
- `ebffde26` — refactor(scala-3): finish implicit→given sweep in core serialization/cbor
- `eef0edce` — refactor(scala-3,mongo): implicit val/def/class → given/using/extension, Conversion givens
- `8f70be80` — refactor(scala-3,mongo): BsonGenCodecs implicit val/def → anonymous given + deprecated named def
- `848b8e9e` — fix(scala-3,mongo): clear all -Werror warnings, eliminate remaining implicits

**Important caveat:** Fork commits operate on `core/src/main/scala-3/` paths from the pre-pivot cross-build layout. Our current layout has a single source root at `core/src/main/scala/`. Direct `git cherry-pick` fails on path mismatch.

**Preferred method — per-file copy:**

```bash
# Copy fork's working scala-3 version on top of our current single-source file
git show origin/master:core/src/main/scala-3/<path>.scala > core/src/main/scala/<path>.scala
```

Repeat per file touched by the relevant fork commit. Inspect resulting diff, then reconcile:
- Prune imports referencing dropped modules (`commons-macros`, RPC) — those imports won't resolve in our tree.
- Reconcile against our Phase 1 / Phase 2 changes (stubs, deletions): any divergence from fork's pre-deletion state must be re-applied or merged into the copied file.
- Re-run `scalafmtAll` to normalize formatting differences (fork may use different format settings).
- Drop `@TodoScala3Migration` annotations fork master sprinkles (specific to fork's tracking; we use MIGRATION.md backlog instead).

For files where fork's `scala-3/` overlay diverges drastically from our current state (e.g. our Phase 1 stubs + Phase 2 partial restoration), don't blind-copy — read fork's intent, apply the syntax change only.

Fork-commit diff text (`git show <sha>`) is the secondary source — useful when copy-then-reconcile is messier than re-applying the targeted change.

`sbt-scala3-migrate` plugin is NOT used. Removed from ROADMAP success criteria.

`sbt-scala3-migrate` plugin is NOT used. Removed from ROADMAP success criteria.

### Module + test coverage — split per feature rewriting

Each rewrite kind (3.1 / 3.2 / 3.3) is its OWN PR. Within each PR, sweep across all modules in a single pass (`core`, `mongo`, `hocon`, `cbor`, `benchmark`) where the idiom appears.

Test sources: include in the same PR as their main-source counterpart when the idiom appears in tests. Skip wrapped (`/* */`) test files — they'll be un-wrapped during their feature-area restoration in Phase 4+.

### Plugin lifecycle — N/A (no plugin used)

Decision moot because we're translating from fork, not running a tool. No new entries in `project/plugins.sbt`.

### Fork workflow methodology — mirror halotukozak's commit cadence

Per user directive 2026-06-01: model Phase 3 work after how fork master built up these changes. Observed pattern (`git log origin/master`):

1. **Single-purpose commits** — each commit does ONE kind of change (one rewrite kind, one feature port, one test revival). Never bundle.
2. **Conventional Commits with scope** — `refactor(scala-3,mongo): ...`, `perf(scala-3): ...`, `fix(scala-3,mongo): ...`, `test(scala-3): re-enable XxxTest`, `chore(scala-3): ...`. Scope captures module(s) + scala version.
3. **Stacked sweeps over multiple commits** — fork's `@inline` sweep landed across 4 separate commits (`33a5b792` streams, `5fafdbd7` Opt family, `ad505679` more streams, `ee0be95e` IntStream, `a4ddad6b` SharedExtensions, `580625a9` JFunctionUtils). Same rewrite kind, different files, separate commits for reviewer clarity.
4. **Tests revived in dedicated commits** — `test(scala-3): re-enable JavaInteropTest` etc., one suite per commit when non-trivial.
5. **`wip(...)` prefix for in-progress** — `wip(scala-3,mongo): re-enable mongo cross-build, partial scala-3 syntax fixups` used when work isn't terminal.
6. **Linear merge order** — small commits land sequentially on master, no merge commits in the rewrite flow.

**Applied to our PRs:**

Each slice PR (3.1, 3.2, 3.3, 3.4) maps to **one rewrite kind**. Within the PR, multiple Conventional Commits, one per:
- Module (`core`, `mongo`, …) — `refactor(scala-3,mongo): implicit class → extension in BsonRef`
- Feature area when module is big (`core/misc`, `core/serialization`, `core/serialization/cbor`)
- Edge-case decision (`fix(scala-3): preserve OptArg.argToOptArg implicit for erasure-bridge`)

Commit message body MAY reference the fork commit being translated: `Translated from origin/master@5fafdbd7.`

**Avoid** squashing per-PR commits — reviewer reads them in order (fork pattern). PR landed via "Rebase and merge" or "Create a merge commit", not squash.

### Manual follow-up policy — match fork master output 1:1

For each rewrite slice, the target end-state is **the equivalent shape fork master converged on**. Compare resulting file with `git show origin/master:<path>` (mapping `scala-3/` → `scala/`). If divergences appear:
- Spacing / formatting → `sbt scalafmtAll` normalizes
- Named vs anonymous given → match fork master's choice
- Extension parameter naming → match fork (typically `(a: A)`)
- Borderline cases fork left as `implicit` (e.g. `OptArg.argToOptArg` for erasure-bridge reasons) → preserve the `implicit`, copy fork's explanatory comment

No editorial polish beyond matching fork. Anything fork didn't change stays as-is in our PR.

### `Implicits` object — delete, do NOT deprecate

User directive 2026-06-01: `core/.../misc/Implicits.scala` — the `Implicits` object with `infer/inferNonMacro` stubs — is to be **deleted outright**, not deprecated.

- Fork commit `50272b26 fix(scala-3): Implicits.infer real impl + modern context-bound syntax` restored the macro. We are not restoring it.
- Project memory [[feedback-deprecate-over-restore]] (deprecate when stdlib covers it) does NOT apply here — `Implicits.infer` was an internal helper used to drive `ImplicitNotFound`-message machinery; with Scala 3 `summon[T]` + `@implicitNotFound` on `using` params the helper is unnecessary.
- `ImplicitNotFound` sealed trait + companion **stays** (separate concern — error-message machinery still useful).
- 0 callers in tree per `git grep -nE '\bImplicits\.'` (returned nothing). Safe outright delete.
- File belongs to **slice 3.5 PR** (standalone parallel-safe — moved out of slice 3.3 per revision 2026-06-01). See `03-05-delete-implicits-object-PLAN.md`. Slice 3.5 is independent: branches off `upstream/scala-3` tip, no overlap with 3.1/3.2/3.3/3.4 file sets, can land in parallel. Commits: `refactor(scala-3,core): extract ImplicitNotFound to its own file` then `refactor(scala-3,core): delete Implicits object (covered by summon[T])`.

### Claude's Discretion

- Exact diff hunk granularity per file (consolidate into one commit per slice unless a sub-area genuinely independent).
- Whether to split slice 3.3 (implicit → given) further if fork's `39c047eb` is too big — preference: keep as one PR per the "three sequential" plan unless reviewer requests split during review.
- Re-run `scalafmtAll` between commits if formatting drifts.
- Handling of borderline cases (kept-implicit defs): inherit fork's reasoning + comment verbatim.

</decisions>

<canonical_refs>
## Canonical References

**Downstream agents MUST read these before planning or implementing.**

### Fork master reference commits (read via `git show origin/master:<file>` or `git show <sha>`)
- `origin/master @ 39c047eb` — implicit → given sweep in `core/` (covers slice 3.3 most thoroughly)
- `origin/master @ ebffde26` — finish implicit → given in `core/serialization/cbor/`
- `origin/master @ eef0edce` — `implicit val/def/class → given/using/extension` in `mongo/` (covers slices 3.1 + 3.3 for mongo)
- `origin/master @ 8f70be80` — BsonGenCodecs given conversion pattern (anonymous given + deprecated named def alias)
- `origin/master @ 848b8e9e` — eliminate remaining `implicit` shims in mongo
- `origin/master @ 1ceab33a` — scalafmt config bump for significant-indentation (may need parallel update on our side)

### Fork target state (for diff comparison)
- `origin/master` branch tip: `bcc3bcbf` — final `extension`-style codebase to compare slice outputs against (map paths: `scala-3/` → `scala/`)

### Project rules
- `~/.claude/projects/-Users-bkozak-IdeaProjects-scala-commons3/memory/MEMORY.md`
  - [[feedback-crib-from-master]] — translate, don't rewrite
  - [[feedback-small-scoped-prs]] — three small PRs over one big
  - [[feedback-pr-title-prefix]] — `[Scala 3]` prefix, no slice-name colon, no "Phase"
  - [[feedback-fix-dont-suppress-warnings]] — no `@nowarn` / `-Wconf` added
  - [[feedback-scala3-migrate-syntax]] — superseded by this CONTEXT.md (no plugin)
- `.planning/MIGRATION.md` — update §3 (source-compat breaks) where `implicit class` member becomes `extension` (downstream call-site implications)

### Scala 3 docs
- https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html
- https://docs.scala-lang.org/scala3/reference/contextual/givens.html
- https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html (`?` for HKT wildcards)

</canonical_refs>

<code_context>
## Existing Code Insights

### Reusable Assets
- Current codebase still uses `implicit class XOps[A](...) extends AnyVal` extensively (e.g. `SharedExtensions.scala` lines 60-127 — `class UniversalOps[A](private val a: A) extends AnyVal`, plus 18 sibling extension classes). Note: post-Phase-2 these are `class` (not `implicit class`) wrapped by `implicit def universalOps[A](a: A): UniversalOps[A]` — the conversion is the implicit, not the class.
- Many `F[_]` HKT bounds across `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala` and elsewhere.
- Many `implicit def`/`implicit val` across `core` and `mongo`.

### Established Patterns
- Source layout: single `core/src/main/scala/` root (no `scala-2.13/` / `scala-3/` split post-pivot).
- Scala version pinned to `3.8.2` in `project/Commons.scala`.
- `commons-macros` and `commons-spring` modules deleted in Phase 1 — no syntax rewrites needed for them.
- `analyzer` subproject commented out in Phase 1 — skip.

### Integration Points
- Each rewrite slice PR branches off `upstream/scala-3` tip (post-Phase-1 merge `0887d555`+).
- CI: build job + scalafmt job (split in Phase 1). Both must stay green per PR.
- MIGRATION.md §3 (source-compat) gets new entries per slice — call-site changes for `extension` callers vs `implicit class` callers (usually transparent), `given` vs `implicit val` (downstream may need adjustments).
- Stacking strategy: 3.2 stacks on 3.1, 3.3 on 3.2 — OR each off `scala-3` and they land sequentially in CI order. Default: each off `scala-3`, merge in order 3.1 → 3.2 → 3.3.

</code_context>

<specifics>
## Specific Ideas

- "Compare with halotukozak's fork, do the same" — fork master is the **target shape**. Each PR's acceptance includes a path-mapped diff comparison.
- "Cherry-pick" was suggested but file-path divergence requires manual translation. The fork commits remain the source of truth for shape and edge-case decisions.
- Three sequential PRs (not one big mechanical PR) is a deliberate trade-off for reviewer cognitive load — overrides original ROADMAP wording.

</specifics>

<deferred>
## Deferred Ideas

- Optional braces / indented syntax (significant-indentation per fork's `1ceab33a`) — defer to a Phase 3.4 if appetite, otherwise leave significant-indentation off.
- `@nowarn` removal — out of scope this phase (no warnings to suppress on Phase 1's stub baseline).
- `sbt-scala3-migrate` plugin (originally suggested) — superseded by fork-cherry-pick approach. Not adding plugin.
- Feature ports (meta derivation, GenCodec, RPC, etc.) — Phase 4+.
- Test-source un-wrapping (`/* */` removal from Phase 1) — happens during each feature-area's restoration phase, not here.
- `OptArg.argToOptArg` and other borderline `implicit` shims fork kept — preserve them; document why per fork's commit messages.

</deferred>

---

*Phase: 03-scala-3-syntax-modernization*
*Context gathered: 2026-06-01*
