# Per-PR Workflow

Source of truth for how every slice lands on `upstream/scala-3`. Read before executing any phase.

## Endpoints

- **Target:** `AVSystem/scala-commons:scala-3`
- **Source:** fork `master` (commits + diffs)
- **Origin:** `https://github.com/halotukozak/scala-commons3.git` (fork)

## Branch naming

`scala-3/<slug>` — e.g. `scala-3/crossbuild-infra`, `scala-3/migration-md-skeleton`, `scala-3/macros-stub`, `scala-3/made-integration`.

No slice number in branch name (phase number lives in plan metadata, not the branch).

## Stack base policy

**GitHub stacked PRs.** Prior slice not yet merged → next slice branches off the prior slice's branch on origin, and the PR base is the prior PR's branch.

```bash
# After prior slice pushed
git fetch origin
git checkout -b scala-3/<next-slug> origin/scala-3/<prior-slug>
# ... cherry-pick / hand-author ...
gh pr create --repo AVSystem/scala-commons --base scala-3/<prior-slug> --head halotukozak:scala-3/<next-slug>
```

When prior slice merges to `upstream/scala-3`, GitHub auto-retargets next PR's base to `scala-3`.

## Post-merge rebase

When upstream merges a slice: rebase **only the next-up** pending branch onto new `upstream/scala-3` tip. Later branches stay where they are until each becomes next.

```bash
git fetch upstream
git checkout scala-3/<next-up>
git rebase upstream/scala-3
# resolve conflicts; rerun sbt scalafmtAll on slice-edited files
make ci
```

## Cherry-pick mechanics

**Default:** `git cherry-pick <sha>...` from fork `master`. Preserves commit attribution + message.

**When hopeless** (file restructured upstream, or `scalafmt-3.11.1` reformat clash with semantic changes): hand-author the slice fresh against current upstream state. Original SHAs referenced in commit body footer:

```
Original: <sha1>, <sha2>, ...
```

## Conflict policy

| Conflict type | Action |
|---------------|--------|
| scalafmt-only (whitespace/style from upstream 3.11.1 reformat) | Accept **upstream** version; commit a fixup `style: scalafmtAll on slice-edited files` after re-running `sbt scalafmtAll`. |
| Semantic (logic / types / signatures) | **STOP.** Surface to user with conflict markers + both sides' intent. |
| Mixed | Resolve scalafmt-only portion as upstream, then surface semantic portion to user. |

## Per-slice procedure

1. `git fetch upstream origin`
2. `git checkout -b scala-3/<slug> <base>` (base = `upstream/scala-3` or prior slice per stack policy)
3. Cherry-pick from `master` (or hand-author)
4. Resolve conflicts per policy
5. Edit `MIGRATION.md` (single tracking table — flip status row + add notes; deprecation log entries if any)
6. Local CI gate — **full `make ci` must be green**:
   ```bash
   make ci
   # = sbt "+jvm/test" "+jvm2/test" "+js/test" "++2.13 mimaReportBinaryIssues" scalafmtCheckAll
   ```
7. **Fix in PR** if CI red — do not abort. Add fixup commits with conventional prefix.
8. `git push origin scala-3/<slug>`
9. Wait for GitHub Actions matrix; confirm green.
10. **Ask user for explicit ack** — single gate, post-push, pre-PR.
11. `gh pr create --repo AVSystem/scala-commons --base <base> --head halotukozak:scala-3/<slug> --title "<subject>" --body "<see template>"`
12. Maintainer merges manually — **Claude never merges**.

## Commit message format

Conventional commits with type prefix:
```
<type>(scope): <subject>

<body>
```
Types: `build`, `feat`, `fix`, `refactor`, `test`, `docs`, `ci`, `style`, `chore`, `deprecate`.

**Never:** mention "GSD", "Get Shit Done", "phase", or any GSD nomenclature.

## PR title format

Conventional commits subject **without type prefix**:

- ✓ `enable Scala 3 cross-compile infrastructure`
- ✗ `build: enable Scala 3 cross-compile infrastructure`

(Commit messages still carry the type prefix; PR title strips it.)

## PR body template

```markdown
## Summary

<2-3 sentences: what this slice does + why now>

## Source commits

Cherry-picked from `halotukozak/scala-commons3:master`:
- `<sha>` — <subject>
- `<sha>` — <subject>

<or, if hand-authored: "Hand-authored against upstream state. References: `<sha>`, `<sha>`.">

## MIGRATION.md changes

```diff
<inline diff of the tracking table row(s) and deprecation log entries>
```

## CI proof

- GitHub Actions matrix: <run URL>
- Local `make ci`: green (logs preserved locally)

<!-- Optional sections — include when applicable -->

## Deprecations

- `<symbol>` removed: <stdlib replacement / rationale>
- `<symbol>` added with `@deprecated(message, since)`: <why>

## Test changes

- Re-enabled: `<test>` — <why now>
- Stays disabled: `<test>` — <rationale; mirrored in MIGRATION.md §"Tests known to stay disabled">

## MiMa filter changes

- `<problem>` for `<class>.<member>` — <justification>
```

## Ack gates

Exactly **one** ack gate per slice: post-push, pre-PR.

- Local CI green → push → wait for remote CI green → ask user
- User explicitly says "ack" / "go" / equivalent → `gh pr create`
- Anything else → do not open PR

## MIGRATION.md update contract

- **Single** table edited per PR (no per-phase append blocks).
- Status flip lands in the same commit as the work.
- Deprecation log entries land in the same commit as the deprecation.
- Tests-disabled rationale lives in MIGRATION.md §"Tests known to stay disabled", **not** in source comments.
