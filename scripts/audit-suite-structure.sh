#!/usr/bin/env bash
# Structural audit of all test suites
# Checks for required exports and group naming conventions
#
# Usage: ./scripts/audit-suite-structure.sh

set -euo pipefail
cd "$(dirname "$0")/.."

echo "=== Test Suite Structural Audit ==="
echo ""

# Count total suites
TOTAL=$(ls test/*_SUITE.erl 2>/dev/null | wc -l)
echo "Total test suites: $TOTAL"
echo ""

# Required exports check
echo "=== Checking required exports ==="

MISSING_ALL=0
MISSING_GROUPS=0
MISSING_GROUPS_FOR_LEVEL=0

for suite in test/*_SUITE.erl; do
    name=$(basename "$suite" .erl)
    
    # Check for all/0 export
    if ! grep -q "^-export(\[all/0" "$suite" && ! grep -q "all/0" "$suite" | grep -q export; then
        if ! grep -E "^all\(\)" "$suite" > /dev/null 2>&1; then
            ((MISSING_ALL++)) || true
        fi
    fi
    
    # Check for groups/0 export
    if ! grep -q "groups/0" "$suite"; then
        ((MISSING_GROUPS++)) || true
    fi
    
    # Check for groups_for_level/1
    if ! grep -q "groups_for_level" "$suite"; then
        echo "  Missing groups_for_level: $name"
        ((MISSING_GROUPS_FOR_LEVEL++)) || true
    fi
done

echo ""
echo "Summary:"
echo "  - Missing groups_for_level/1: $MISSING_GROUPS_FOR_LEVEL"
echo ""

# Check for heavy groups leaking into fast
echo "=== Checking for heavy groups in fast level ==="

HEAVY_PATTERNS="heavy_tests|stress_tests|soak_tests|chaos_tests|performance_tests|load_tests|fault_injection|chaos_engineering"

for suite in test/*_SUITE.erl; do
    name=$(basename "$suite" .erl)
    
    # Check if groups_for_level(fast) or groups_for_level(_) returns heavy groups
    if grep -q "groups_for_level(_)" "$suite"; then
        # Get the line after groups_for_level(_) ->
        line=$(grep -A1 "groups_for_level(_)" "$suite" | tail -1)
        
        if echo "$line" | grep -qE "$HEAVY_PATTERNS"; then
            echo "  WARNING: $name may have heavy groups in fast level"
        fi
    fi
done

echo ""
echo "=== Test level distribution ==="

# Count suites returning empty list for fast
FAST_SKIP=0
for suite in test/*_SUITE.erl; do
    if grep -A1 "groups_for_level(_)" "$suite" 2>/dev/null | grep -q '\[\]\.'; then
        ((FAST_SKIP++)) || true
    fi
done

echo "  Suites skipped in fast mode: $FAST_SKIP"
echo "  Suites with tests in fast mode: $((TOTAL - FAST_SKIP - MISSING_GROUPS_FOR_LEVEL))"
echo ""

echo "=== Done ==="
