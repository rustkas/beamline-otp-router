#!/usr/bin/env bash

set -e

# Ensure we are in the router root directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

LIMIT=500
FAILED=0

echo "Checking test file sizes (limit: $LIMIT lines)..."

# Modified path to be relative to router root
for file in $(find test -name "*_SUITE.erl"); do
    lines=$(wc -l < "$file")
    if [ "$lines" -gt "$LIMIT" ]; then
        echo "[FAIL] $file — $lines lines (limit $LIMIT)"
        FAILED=1
    else
        echo "[OK]   $file — $lines lines"
    fi
done

if [ "$FAILED" -eq 1 ]; then
    echo "Some test suites exceed the maximum size."
    exit 1
fi

echo "All test suites within limit."
