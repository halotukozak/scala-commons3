---
phase: 02-migration-md-skeleton-deprecation-seed
plan: 02
type: execute
wave: 2
depends_on: [01]
files_modified:
  - MIGRATION.md
autonomous: true
commit_docs: false
requirements: [DOC-03]
must_haves:
  truths:
    - "`MIGRATION.md` `## Deprecation log` section is populated by running `git grep -n '@deprecated' master -- '*.scala'` on a checkout that has fork master fetched"
    - "Source command is documented verbatim inside MIGRATION.md as a fenced bash code block"
    - "Seed cite line names `master@<short-sha>` and seed date `YYYY-MM-DD`"
    - "Log entries are grouped under two `###` subheadings: `### core/` and `### mongo/` (verified hit counts: core≈123, mongo≈29, total≈152)"
    - "Every log line is tagged either `[port]` or `[skip-port]` per the decision rule in RESEARCH.md §Pitfall 3"
    - "Lines with stdlib replacement hints in their message (`Use scala.`, `Scala 2.13 has native`, `use SAM syntax`, `stdlib`) are tagged `[skip-port]`; the rest `[port]`"
    - "Plan 02 produces exactly one new commit prefixed `docs(migration):`"
  artifacts:
    - path: MIGRATION.md
      provides: "Deprecation log section populated from @deprecated scan of fork master"
      contains: "git grep -n '@deprecated' master"
      min_lines: 200
  key_links:
    - from: "MIGRATION.md ## Deprecation log section"
      to: "fork master @deprecated symbols"
      via: "git grep output captured verbatim with [port]/[skip-port] tags"
      pattern: "@deprecated"
    - from: "[port] / [skip-port] tags"
      to: "future-phase deprecation handling"
      via: "tags are grep targets for downstream porting work"
      pattern: "\\[(port|skip-port)\\]"
---

<objective>
Populate the `## Deprecation log` section of `MIGRATION.md` by running `git grep -n '@deprecated' master -- '*.scala'`, formatting each hit as a single line `path:line — symbol — "message snippet" [port|skip-port]`, grouping by module under `### core/` and `### mongo/` subheadings, and committing the result as a single `docs(migration):` commit on the same branch Plan 01 created.

Purpose: Closes REQ DOC-03 — the deprecation log seed. Plan 03 (check script) and Plan 04 (push + PR) will verify and ship the result.

Output: One additional commit on `02-migration-md` that flips the `## Deprecation log` section from an empty placeholder to a fully-populated, grep-friendly listing of all `@deprecated` annotations in fork `master`.
</objective>

<execution_context>
@/Users/bkozak/.claude/get-shit-done/workflows/execute-plan.md
@/Users/bkozak/.claude/get-shit-done/templates/summary.md
</execution_context>

<context>
@.planning/REQUIREMENTS.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-RESEARCH.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-VALIDATION.md
@.planning/phases/02-migration-md-skeleton-deprecation-seed/02-01-SUMMARY.md
</context>

<interfaces>
Decision rule for `[port]` vs `[skip-port]` tagging (locked by RESEARCH.md §Pitfall 3, derived from user memory `feedback_dont_port_deprecated.md`):

If the deprecation message contains ANY of these substrings (case-insensitive) → tag `[skip-port]`:
- `stdlib`
- `scala.` (e.g., `Use scala.ValueOf[T]`)
- `Scala 2.13 has native`
- `use SAM syntax`
- `lambda`
- `since Scala 2.13`

Otherwise → tag `[port]`.

Examples (verbatim from RESEARCH.md):
- `"Use scala.valueOf[T] from the standard library"` → `[skip-port]`
- `"Scala 2.13 has native scala.math.Ordering.orElse"` → `[skip-port]`
- `"Use GenCodec.materialize instead"` → `[port]` (internal replacement, not stdlib)
- `"use SAM syntax (lambda)"` → `[skip-port]`

Expected hit volume (from RESEARCH.md, confirmed 2026-05-30 — re-verify at execution time):
- `core/` → ~123 hits (~26 under `scala-2.13/`, ~97 under `scala-3/` dominated by `compat.scala`/`GenCodecCompat.scala`/`GenCodecCreates.scala`)
- `mongo/` → ~29 hits (all under `mongo/jvm/src/main/scala/`)
- Total → ~152 hits

Line format (locked by CONTEXT.md):

    path:line — symbol — "message snippet (truncated to ~80 chars)" [port|skip-port]

Em-dash character is `—` (U+2014), not double-hyphen.

Section structure to insert into MIGRATION.md `## Deprecation log` section (REPLACE the placeholder sentence "Seeded from a `@deprecated` scan against fork `master`. Populated in the same PR as this skeleton."):

```markdown
## Deprecation log

Seeded from a `@deprecated` scan of fork `master` on `<remote>/master@<short-sha>` (`<YYYY-MM-DD>`). Re-runnable verbatim:

```bash
git grep -n '@deprecated' master -- '*.scala'
```

Lines tagged `[skip-port]` have a Scala standard library or language-feature replacement and are not ported during the Scala 3 migration. Lines tagged `[port]` reference internal replacements and must be addressed by the relevant module port. Messages are truncated to ~80 characters.

### core/

```
<line-per-hit entries from grep output, sorted by path then line number>
```

### mongo/

```
<line-per-hit entries from grep output, sorted by path then line number>
```
```

Note: The OUTER `## Deprecation log` Markdown section contains an INNER fenced bash block AND TWO INNER fenced text blocks. Use backticks of equal length for outer/inner (Markdown nests with same fence count fine in practice — but to be safe, use ```` ```` (4 backticks) for the outer wrapping when writing the file IF GFM rendering issues surface. For Plan 02's first attempt, use triple-backtick fences and verify rendering after.

Sort order inside each fenced block: by full path (lexicographic), then by line number (numeric). This makes the log diff-friendly across future PRs.
</interfaces>

<tasks>

<task type="auto">
  <name>Task 1: Capture grep output + master SHA, format entries with [port]/[skip-port] tags, write tmp file</name>
  <files>(none — produces an intermediate file at `/tmp/02-deprecation-seed.md`; not committed)</files>
  <read_first>
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-RESEARCH.md §"Code Examples" → "Deprecation seed command (verbatim for the doc)" and "Per-module group (sample)"
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-RESEARCH.md §"Common Pitfalls" §Pitfall 3 (tagging rule) and §Pitfall 4 (include scala-3/ hits)
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md §"Deprecation log seeding"
  </read_first>
  <action>
    Working dir: `/Users/bkozak/IdeaProjects/scala-commons3`.

    Step 1 — capture master SHA (short form, 8 chars). Use the fork's `master` ref, NOT upstream's:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git fetch origin master    # ensure fork's master is up to date locally
        MASTER_SHA=$(git rev-parse --short=8 master)
        SEED_DATE=$(date -u +%Y-%m-%d)
        echo "MASTER_SHA=$MASTER_SHA"
        echo "SEED_DATE=$SEED_DATE"

    Record both for use in Task 2's prose insertion.

    Step 2 — run the verbatim seed command and save raw output:

        cd /Users/bkozak/IdeaProjects/scala-commons3
        git grep -n '@deprecated' master -- '*.scala' > /tmp/02-grep-raw.txt
        wc -l /tmp/02-grep-raw.txt    # expect ~152

    The raw line format from `git grep -n` is: `master:path:line:matched-text`. Example:

        master:core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala:840:  @deprecated("Scala 2.13 has native scala.math.Ordering.orElse implementation", "2.0.0")

    Step 3 — parse each line into the canonical `path:line — symbol — "message" [tag]` format.

    Parsing algorithm per line:
    1. Strip the leading `master:` prefix.
    2. Split into `path`, `line`, `rest` (split on `:` from the left, keeping only the first two delimiters).
    3. From `rest`, extract the symbol — this is harder because the `@deprecated` annotation typically sits ABOVE the symbol declaration. The simplest reliable approach: use the file path's leaf component + an inferred symbol from the nearest `def`/`val`/`class`/`object`/`trait` declaration below. To keep Plan 02 simple, use a two-pass approach:
       - Pass A (cheap): extract symbol as the FIRST identifier on the line at `path:line+1` (the line immediately below the `@deprecated`) using `git show master:path | sed -n "$((line+1))p"` and a regex `(def|val|var|class|object|trait|case class|case object|type)[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)`. If the regex matches, capture group 2 is the symbol.
       - Pass B (fallback): if no match within +1 line (e.g., multi-line annotation), scan +2 and +3 the same way. If still no match, fall back to the file basename without extension as a coarse symbol marker.
    4. From the matched-text, extract the `@deprecated("MESSAGE", "since")` arguments. Capture MESSAGE; truncate to 80 chars and add `…` if truncated.
    5. Determine tag: if MESSAGE (case-insensitive) contains ANY of `stdlib`, `scala.`, `Scala 2.13 has native`, `use SAM syntax`, `lambda`, `since Scala 2.13` → `[skip-port]`; else `[port]`.
    6. Format: `<path>:<line> — <symbol> — "<message-truncated>" [<tag>]`.

    Implementation hint: this is fiddly enough that hand-writing a small shell script is preferable to inline awk. Write `/tmp/02-format-seed.sh` doing the above; run it; capture output to `/tmp/02-formatted.txt`. Pseudocode:

        #!/usr/bin/env bash
        set -euo pipefail
        while IFS= read -r raw; do
          # raw = master:path:line:matched
          rest=${raw#master:}
          path=${rest%%:*}; rest=${rest#*:}
          line=${rest%%:*}; matched=${rest#*:}
          # extract message between first pair of double quotes in matched
          msg=$(printf '%s\n' "$matched" | sed -nE 's/.*@deprecated\([[:space:]]*"([^"]*)".*/\1/p')
          [[ -z "$msg" ]] && msg="(message not captured)"
          # truncate to 80 chars
          if [[ ${#msg} -gt 80 ]]; then msg="${msg:0:79}…"; fi
          # extract symbol from line+1 (cheap pass)
          sym=$(git show "master:$path" 2>/dev/null | sed -n "$((line+1))p" \
                | sed -nE 's/^[[:space:]]*(override[[:space:]]+)?(def|val|var|lazy[[:space:]]+val|class|object|trait|case[[:space:]]+class|case[[:space:]]+object|type|implicit[[:space:]]+def|implicit[[:space:]]+val)[[:space:]]+\[?([A-Za-z_][A-Za-z0-9_]*).*/\3/p')
          if [[ -z "$sym" ]]; then
            sym=$(git show "master:$path" 2>/dev/null | sed -n "$((line+2))p" \
                  | sed -nE 's/^[[:space:]]*(override[[:space:]]+)?(def|val|var|lazy[[:space:]]+val|class|object|trait|case[[:space:]]+class|case[[:space:]]+object|type|implicit[[:space:]]+def|implicit[[:space:]]+val)[[:space:]]+\[?([A-Za-z_][A-Za-z0-9_]*).*/\3/p')
          fi
          [[ -z "$sym" ]] && sym="$(basename "$path" .scala)"
          # tag
          msg_lc=$(printf '%s' "$msg" | tr '[:upper:]' '[:lower:]')
          if [[ "$msg_lc" == *"stdlib"* || "$msg_lc" == *"scala."* || "$msg_lc" == *"scala 2.13 has native"* || "$msg_lc" == *"use sam syntax"* || "$msg_lc" == *"lambda"* || "$msg_lc" == *"since scala 2.13"* ]]; then
            tag="[skip-port]"
          else
            tag="[port]"
          fi
          printf '%s:%s — %s — "%s" %s\n' "$path" "$line" "$sym" "$msg" "$tag"
        done < /tmp/02-grep-raw.txt > /tmp/02-formatted.txt
        wc -l /tmp/02-formatted.txt

    Step 4 — split formatted output into `core/` and `mongo/` groups, each sorted by path then line:

        grep '^core/' /tmp/02-formatted.txt | sort -t: -k1,1 -k2,2n > /tmp/02-core.txt
        grep '^mongo/' /tmp/02-formatted.txt | sort -t: -k1,1 -k2,2n > /tmp/02-mongo.txt
        wc -l /tmp/02-core.txt /tmp/02-mongo.txt

    Sanity check: the sum of the two `wc -l` numbers SHOULD equal the line count of `/tmp/02-formatted.txt`. If not, there are hits outside `core/` and `mongo/` and the assumption from RESEARCH.md broke — surface to user, do not proceed.

    Step 5 — count tags for sanity:

        echo "[port] count: $(grep -c '\[port\]$' /tmp/02-formatted.txt)"
        echo "[skip-port] count: $(grep -c '\[skip-port\]$' /tmp/02-formatted.txt)"
        # sum MUST equal line count

    Outputs produced (intermediate, not committed):
    - `/tmp/02-grep-raw.txt` — raw grep output
    - `/tmp/02-formatted.txt` — formatted log lines
    - `/tmp/02-core.txt` — sorted core/ subset
    - `/tmp/02-mongo.txt` — sorted mongo/ subset
    - Two shell variables `MASTER_SHA` and `SEED_DATE` to be used in Task 2

    Persist `MASTER_SHA` and `SEED_DATE` to a file for Task 2 (which runs in a fresh shell):

        echo "MASTER_SHA=$MASTER_SHA" > /tmp/02-seed-meta.env
        echo "SEED_DATE=$SEED_DATE" >> /tmp/02-seed-meta.env
        echo "REMOTE_OF_MASTER=origin" >> /tmp/02-seed-meta.env

    Rationale for `origin` (not `upstream`): the seed grep uses the fork's master per CONTEXT.md ("git grep '@deprecated' master") — `master` resolves to the local `master` branch, which tracks `origin/master` (the fork). The doc text below cites `origin/master@<sha>`.
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; test -s /tmp/02-grep-raw.txt &amp;&amp; test -s /tmp/02-formatted.txt &amp;&amp; test -s /tmp/02-core.txt &amp;&amp; test -s /tmp/02-mongo.txt &amp;&amp; test -s /tmp/02-seed-meta.env &amp;&amp; [ "$(wc -l &lt; /tmp/02-formatted.txt)" -ge 100 ] &amp;&amp; [ "$(wc -l &lt; /tmp/02-formatted.txt)" -le 250 ] &amp;&amp; [ "$(($(wc -l &lt; /tmp/02-core.txt) + $(wc -l &lt; /tmp/02-mongo.txt)))" -eq "$(wc -l &lt; /tmp/02-formatted.txt)" ] &amp;&amp; ! grep -vE '\[(port|skip-port)\]$' /tmp/02-formatted.txt</automated>
  </verify>
  <acceptance_criteria>
    - `/tmp/02-grep-raw.txt` exists, non-empty.
    - `/tmp/02-formatted.txt` exists with between 100 and 250 lines (sanity window around the ~152 expected count).
    - `/tmp/02-core.txt` + `/tmp/02-mongo.txt` line counts sum exactly to `/tmp/02-formatted.txt` line count (no orphan modules).
    - Every line of `/tmp/02-formatted.txt` ends with either ` [port]` or ` [skip-port]` — `grep -vE '\[(port|skip-port)\]$' /tmp/02-formatted.txt` exits 1 (i.e., zero unmatched lines).
    - Every line matches the format `^[^:]+:[0-9]+ — .+ — ".*" \[(port|skip-port)\]$` — `grep -vE '^[^:]+:[0-9]+ — .+ — ".*" \[(port|skip-port)\]$' /tmp/02-formatted.txt | wc -l` prints `0`.
    - `/tmp/02-seed-meta.env` contains MASTER_SHA and SEED_DATE assignments — `grep -E '^MASTER_SHA=[0-9a-f]{8}$' /tmp/02-seed-meta.env` exits 0; `grep -E '^SEED_DATE=[0-9]{4}-[0-9]{2}-[0-9]{2}$' /tmp/02-seed-meta.env` exits 0.
    - Working tree unchanged: `git status --porcelain` shows `MIGRATION.md` only if Plan 01 left uncommitted changes (should be empty).
  </acceptance_criteria>
  <done>Raw grep captured, formatted into canonical `path:line — symbol — "msg" [tag]` lines, split into `core/` and `mongo/` sorted subsets, master SHA + seed date pinned. No commits yet.</done>
</task>

<task type="auto">
  <name>Task 2: Replace the Deprecation log placeholder in MIGRATION.md with the populated content; commit</name>
  <files>MIGRATION.md</files>
  <read_first>
    - MIGRATION.md (current state — Plan 01 left a placeholder sentence under `## Deprecation log`)
    - /tmp/02-core.txt, /tmp/02-mongo.txt, /tmp/02-seed-meta.env (intermediate outputs from Task 1)
    - .planning/phases/02-migration-md-skeleton-deprecation-seed/02-CONTEXT.md §"Tone, header style, and writing rules" (no emoji, no GSD vocab)
  </read_first>
  <action>
    Working dir: `/Users/bkozak/IdeaProjects/scala-commons3`.

    Step 1 — load meta:

        source /tmp/02-seed-meta.env
        echo "$MASTER_SHA $SEED_DATE $REMOTE_OF_MASTER"

    Step 2 — locate the placeholder sentence in MIGRATION.md and replace the entire `## Deprecation log` section content. Plan 01 left this exact block:

        ## Deprecation log

        Seeded from a `@deprecated` scan against fork `master`. Populated in the same PR as this skeleton.

    Replace it with (preserving the `## Deprecation log` heading, substituting `$MASTER_SHA` / `$SEED_DATE` / `$REMOTE_OF_MASTER` and inlining the contents of `/tmp/02-core.txt` and `/tmp/02-mongo.txt` into the fenced text blocks):

        ## Deprecation log

        Seeded from a `@deprecated` scan of fork `master` on `<REMOTE>/master@<SHA>` (`<DATE>`). Re-runnable verbatim:

        ```bash
        git grep -n '@deprecated' master -- '*.scala'
        ```

        Lines tagged `[skip-port]` have a Scala standard library or language-feature replacement and are not ported during the Scala 3 migration. Lines tagged `[port]` reference internal replacements and must be addressed by the relevant module port. Messages are truncated to ~80 characters.

        ### core/

        ```
        <contents of /tmp/02-core.txt, verbatim>
        ```

        ### mongo/

        ```
        <contents of /tmp/02-mongo.txt, verbatim>
        ```

    Substitutions:
    - `<REMOTE>` → value of `$REMOTE_OF_MASTER` (literal: `origin`).
    - `<SHA>` → value of `$MASTER_SHA` (8 hex chars).
    - `<DATE>` → value of `$SEED_DATE` (`YYYY-MM-DD`).

    Implementation approach (preferred — Edit tool):
    1. Read the current MIGRATION.md placeholder block.
    2. Use the Edit tool to replace the placeholder sentence with the rendered Deprecation log section. The `Edit` tool's `old_string` should include enough context to be unique: include the line `## Deprecation log` plus the placeholder sentence plus the trailing blank line (or EOF marker — Plan 01 ends the file there).

    Alternative (if Edit tool struggles with multi-line replace): write a small bash snippet that uses `awk` to print everything BEFORE `## Deprecation log`, then emit the new section, then print everything AFTER the next `## ` heading (there is none — `## Deprecation log` is last). Concretely:

        awk '/^## Deprecation log/{exit} {print}' MIGRATION.md > /tmp/02-head.md
        {
          echo '## Deprecation log'
          echo
          echo "Seeded from a \`@deprecated\` scan of fork \`master\` on \`${REMOTE_OF_MASTER}/master@${MASTER_SHA}\` (\`${SEED_DATE}\`). Re-runnable verbatim:"
          echo
          echo '```bash'
          echo "git grep -n '@deprecated' master -- '*.scala'"
          echo '```'
          echo
          echo 'Lines tagged `[skip-port]` have a Scala standard library or language-feature replacement and are not ported during the Scala 3 migration. Lines tagged `[port]` reference internal replacements and must be addressed by the relevant module port. Messages are truncated to ~80 characters.'
          echo
          echo '### core/'
          echo
          echo '```'
          cat /tmp/02-core.txt
          echo '```'
          echo
          echo '### mongo/'
          echo
          echo '```'
          cat /tmp/02-mongo.txt
          echo '```'
        } > /tmp/02-tail.md
        cat /tmp/02-head.md /tmp/02-tail.md > /tmp/MIGRATION.md.new
        mv /tmp/MIGRATION.md.new MIGRATION.md

    Step 3 — sanity-check the resulting file:

        head -1 MIGRATION.md    # MUST still be `# Scala 3 Migration Status`
        grep -c '^## ' MIGRATION.md    # MUST be 5 (How to update, Per-module status, 2.13-only modules, Deprecation log — wait, that is 4. Plus one for the H1? No, H1 uses `#` not `## `. So 4. Counts.)
        wc -l MIGRATION.md    # MUST be ≥ 200

    Correction on the heading count check: the file has exactly FOUR `## ` headings (How to update, Per-module status, 2.13-only modules, Deprecation log). The H1 `# Scala 3 Migration Status` uses a single `#` and is not counted.

    Step 4 — confirm every Deprecation log line is tagged:

        awk '/^## Deprecation log/,/^EOF$/' MIGRATION.md | grep -E '^[^:]+:[0-9]+ — ' | grep -vE '\[(port|skip-port)\]$'
        # MUST print nothing (exit 1)

    Step 5 — confirm no GSD vocabulary leaked through symbols/messages (paranoid pass — `@deprecated` messages should not contain GSD terms, but verify):

        grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md
        # MUST exit 1

        grep -F '.planning/' MIGRATION.md
        # MUST exit 1

    Step 6 — stage and commit. ONLY MIGRATION.md should change:

        git add MIGRATION.md
        git status --porcelain    # MUST show ` M MIGRATION.md` (or `M  MIGRATION.md` after add) ONLY
        git commit -m "$(cat <<'COMMIT_EOF'
        docs(migration): seed deprecation log from @deprecated scan of master

        Captures the output of `git grep -n '@deprecated' master -- '*.scala'`
        with each entry tagged [port] (internal replacement, needs migration
        work) or [skip-port] (stdlib / language-feature replacement, dropped
        during port per existing project policy).
        COMMIT_EOF
        )"

    Commit message rules: no GSD nomenclature (`grep -iE 'gsd|phase [0-9]|plan-phase'` on the message MUST exit 1).
  </action>
  <verify>
    <automated>cd /Users/bkozak/IdeaProjects/scala-commons3 &amp;&amp; head -1 MIGRATION.md | grep -Fxq '# Scala 3 Migration Status' &amp;&amp; grep -Fxq '## Deprecation log' MIGRATION.md &amp;&amp; grep -F "git grep -n '@deprecated' master -- '*.scala'" MIGRATION.md &amp;&amp; grep -Fxq '### core/' MIGRATION.md &amp;&amp; grep -Fxq '### mongo/' MIGRATION.md &amp;&amp; [ "$(awk '/^## Deprecation log/,0' MIGRATION.md | grep -cE '^[^:]+:[0-9]+ — ')" -ge 100 ] &amp;&amp; [ "$(awk '/^## Deprecation log/,0' MIGRATION.md | grep -cE '\[(port|skip-port)\]$')" -ge 100 ] &amp;&amp; ! awk '/^## Deprecation log/,0' MIGRATION.md | grep -E '^[^:]+:[0-9]+ — ' | grep -vE '\[(port|skip-port)\]$' &amp;&amp; ! grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md &amp;&amp; ! grep -F '.planning/' MIGRATION.md &amp;&amp; git log -1 --format=%s | grep -E '^docs\(migration\):' &amp;&amp; ! git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase' &amp;&amp; [ "$(git show --stat HEAD --name-only --pretty= | grep -v '^$' | sort -u | wc -l)" -eq 1 ] &amp;&amp; git show --stat HEAD --name-only --pretty= | grep -Fxq 'MIGRATION.md'</automated>
  </verify>
  <acceptance_criteria>
    - `head -1 MIGRATION.md` still prints `# Scala 3 Migration Status` (H1 unchanged).
    - All four `## ` sections still present: `grep -Fxq '## How to update'`, `grep -Fxq '## Per-module status'`, `grep -Fxq '## 2.13-only modules'`, `grep -Fxq '## Deprecation log'` — each exits 0.
    - Seed command documented verbatim in the doc — `grep -F "git grep -n '@deprecated' master -- '*.scala'" MIGRATION.md` exits 0.
    - Seed cite line present — `grep -E 'origin/master@[0-9a-f]{8}' MIGRATION.md` exits 0.
    - `### core/` and `### mongo/` subheadings present — `grep -Fxq '### core/' MIGRATION.md` and `grep -Fxq '### mongo/' MIGRATION.md` each exit 0.
    - At least 100 deprecation entries present in the log section — `awk '/^## Deprecation log/,0' MIGRATION.md | grep -cE '^[^:]+:[0-9]+ — '` is ≥ 100.
    - Every deprecation entry is tagged — `awk '/^## Deprecation log/,0' MIGRATION.md | grep -E '^[^:]+:[0-9]+ — ' | grep -vE '\[(port|skip-port)\]$' | wc -l` prints `0`.
    - No GSD vocabulary leaked — `grep -iE '\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b' MIGRATION.md` exits 1.
    - No `.planning/` references — `grep -F '.planning/' MIGRATION.md` exits 1.
    - Commit prefix is `docs(migration):` — `git log -1 --format=%s | grep -E '^docs\(migration\):'` exits 0.
    - Commit message has no GSD nomenclature — `git log -1 --format=%B | grep -iE 'gsd|phase [0-9]|plan-phase'` exits 1.
    - Commit touches ONLY `MIGRATION.md` — `git show --stat HEAD --name-only --pretty= | grep -v '^$' | sort -u` prints exactly `MIGRATION.md`.
    - Branch now has 2 commits since `upstream/scala-3` — `git log upstream/scala-3..HEAD --oneline | wc -l` prints `2`.
    - No `.planning/` paths in any commit on this branch — `git log upstream/scala-3..HEAD --name-only | grep -c '^\.planning'` prints `0`.
  </acceptance_criteria>
  <done>`## Deprecation log` section of MIGRATION.md replaced with seed cite line, verbatim seed command, two grouped fenced code blocks (`### core/`, `### mongo/`), and ≥100 tagged entries. Plan 02 commit landed; branch carries 2 `docs(migration):` commits.</done>
</task>

</tasks>

<verification>
After Plan 02 completes:

1. `cd /Users/bkozak/IdeaProjects/scala-commons3 && git log upstream/scala-3..HEAD --oneline | wc -l` prints `2`.
2. `git diff upstream/scala-3..HEAD --name-only` prints exactly `MIGRATION.md`.
3. `awk '/^## Deprecation log/,0' MIGRATION.md | grep -cE '^[^:]+:[0-9]+ — '` is ≥ 100 (deprecation log populated).
4. Every entry tagged: `awk '/^## Deprecation log/,0' MIGRATION.md | grep -E '^[^:]+:[0-9]+ — ' | grep -vE '\[(port|skip-port)\]$' | wc -l` prints `0`.
5. No `.planning/` paths in any commit (REQ WORKFLOW-05).
6. No GSD nomenclature in any commit message (REQ WORKFLOW-04).
</verification>

<success_criteria>
- `## Deprecation log` section of MIGRATION.md is populated from a deterministic `git grep` against fork `master` (REQ DOC-03).
- Seed command is documented verbatim inside the doc, alongside the `master@<sha>` cite and seed date.
- Entries are grouped under `### core/` and `### mongo/` subheadings, each in a fenced code block, sorted by `path` then `line`.
- Every entry carries a `[port]` or `[skip-port]` tag per the locked decision rule.
- Plan 02 produces exactly one new `docs(migration):` commit; branch has 2 total commits ahead of `upstream/scala-3`.
- No GSD nomenclature, no `.planning/` references in doc or commit messages.
</success_criteria>

<output>
After completion, create `.planning/phases/02-migration-md-skeleton-deprecation-seed/02-02-SUMMARY.md` capturing:
- Master SHA seeded against, seed date
- Total deprecation entries count (split by core/ vs mongo/)
- [port] vs [skip-port] counts
- Output of `git log upstream/scala-3..HEAD --oneline` (should be 2 commits)
- Output of `wc -l MIGRATION.md`
- Confirmation that no `.planning/` paths and no GSD nomenclature appear in branch history
</output>
