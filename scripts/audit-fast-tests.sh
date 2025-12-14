#!/usr/bin/env bash
# Audit test suites for fast tier compatibility
# Usage: ./scripts/audit-fast-tests.sh

set -euo pipefail
cd "$(dirname "$0")/.."

echo "=== Fast Test Suite Audit ==="
echo "Analysing all *_SUITE.erl files..."
echo ""

export ROUTER_TEST_LEVEL=fast

# Count suites
TOTAL=$(ls test/*_SUITE.erl 2>/dev/null | wc -l)
echo "Total test suites: $TOTAL"
echo ""

# Count suites with timer:sleep
SLEEP_COUNT=$(grep -l "timer:sleep" test/*_SUITE.erl 2>/dev/null | wc -l)
echo "Suites using timer:sleep: $SLEEP_COUNT"

# Count suites with wait_for_condition
WAIT_COUNT=$(grep -l "wait_for_condition" test/*_SUITE.erl 2>/dev/null | wc -l)
echo "Suites using wait_for_condition: $WAIT_COUNT"
echo ""

# Find heavy-only suites (return [] for fast)
echo "=== Heavy-only suites (skip in fast mode) ==="
grep -l 'groups_for_level(_) ->' test/*_SUITE.erl | while read file; do
    if grep -A1 'groups_for_level(_) ->' "$file" | grep -q '\[\]\.'; then
        basename "$file"
    fi
done 2>/dev/null | head -20

echo ""
echo "=== Done ==="
