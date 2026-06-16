#!/usr/bin/env bash
# Phase 2 — MIGRATION.md content invariants.
# NOT committed (lives under .planning/, which is gitignored).
# Idempotent. Exits 0 on full green, non-zero on first failure with named assertion.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

assert() {
  local desc="$1"; shift
  printf '→ check: %s\n' "$desc"
  if "$@" >/dev/null 2>&1; then
    printf '   ok\n'
    PASS=$((PASS+1))
  else
    printf '   FAIL: %s\n' "$desc" >&2
    FAIL=$((FAIL+1))
    exit 1
  fi
}

# 1. DOC-01: file exists at repo root
assert "MIGRATION.md exists at repo root" test -f MIGRATION.md

# 2. DOC-01: H1 is exactly `# Scala 3 Migration Status`
assert "H1 is '# Scala 3 Migration Status'" bash -c 'head -1 MIGRATION.md | grep -Fxq "# Scala 3 Migration Status"'

# 3. DOC-01: all four ## sections present
assert "section '## How to update' present"      grep -Fxq '## How to update' MIGRATION.md
assert "section '## Per-module status' present"  grep -Fxq '## Per-module status' MIGRATION.md
assert "section '## 2.13-only modules' present"  grep -Fxq '## 2.13-only modules' MIGRATION.md
assert "section '## Deprecation log' present"    grep -Fxq '## Deprecation log' MIGRATION.md

# 4. DOC-01: per-module status table has all 13 rows
ROW_COUNT=$(awk '/^## Per-module status/,/^## 2/' MIGRATION.md \
  | grep -cE '^\| (macros|made|core|hocon|mongo|mongo-js|core-js|benchmark3|jetty|analyzer|spring|RPC|cbor) ' || true)
assert "per-module table has 13 rows (got $ROW_COUNT)" test "$ROW_COUNT" -eq 13

# 5. DOC-02 (codified): ## How to update enumerates ≥ 5 numbered rules
RULE_COUNT=$(awk '/^## How to update/{f=1;next} f && /^## /{exit} f' MIGRATION.md | grep -cE '^[0-9]+\.' || true)
assert "How to update has ≥ 5 numbered rules (got $RULE_COUNT)" test "$RULE_COUNT" -ge 5

# 6. DOC-03: ## Deprecation log has ≥ 100 entries
DEP_COUNT=$(awk '/^## Deprecation log/,0' MIGRATION.md | grep -cE '^[^:]+:[0-9]+ — ' || true)
assert "deprecation log has ≥ 100 entries (got $DEP_COUNT)" test "$DEP_COUNT" -ge 100

# 7. DOC-03: seed command documented verbatim
assert "seed command documented verbatim" grep -F "git grep -n '@deprecated' master -- '*.scala'" MIGRATION.md

# 8. DOC-03: every deprecation entry tagged [port] or [skip-port]
UNTAGGED=$(awk '/^## Deprecation log/,0' MIGRATION.md | grep -E '^[^:]+:[0-9]+ — ' | grep -vcE '\[(port|skip-port)\]$' || true)
assert "every deprecation entry tagged (untagged=$UNTAGGED)" test "$UNTAGGED" -eq 0

# 9. DOC-03: ### core/ and ### mongo/ subheadings present
assert "### core/ subheading present"  grep -Fxq '### core/' MIGRATION.md
assert "### mongo/ subheading present" grep -Fxq '### mongo/' MIGRATION.md

# 10. DOC-04: 2.13-only section names all four
TARGET_COUNT=$(awk '/^## 2.13-only modules/{f=1;next} f && /^## /{exit} f' MIGRATION.md \
  | grep -Eo '\b(jetty|analyzer|spring|RPC)\b' | sort -u | wc -l | tr -d ' ' || true)
assert "2.13-only section names jetty/analyzer/spring/RPC (got $TARGET_COUNT/4)" test "$TARGET_COUNT" -eq 4

# 11. WORKFLOW-04: no GSD vocabulary
assert "no GSD vocabulary in MIGRATION.md" bash -c '! grep -iE "\b(GSD|wave|phase [0-9]|RESEARCH\.md|PLAN\.md|CONTEXT\.md)\b" MIGRATION.md'

# 12. WORKFLOW-05: no .planning/ references
assert "no .planning/ references in MIGRATION.md" bash -c '! grep -F ".planning/" MIGRATION.md'

# 13. DOC-01 (tone): no ✓ emoji and no first-person plural
assert "no ✓ emoji in MIGRATION.md" bash -c '! grep -F "✓" MIGRATION.md'
assert "no first-person plural in MIGRATION.md" bash -c '! grep -E "\b(we|our)\b" MIGRATION.md'

printf '\nALL CHECKS GREEN (%d assertions)\n' "$PASS"
