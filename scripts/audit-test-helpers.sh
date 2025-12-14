#!/bin/bash
set -e

# Audit Test Helpers Script
# Ensures that router_test_*_helpers modules are NOT used in src/ directory

echo "=== Auditing Test Helpers Usage ==="

FOUND=$(grep -r "router_test_.*_helpers" src/ || true)

if [ ! -z "$FOUND" ]; then
    echo "❌ ERROR: Test helpers found in production code:"
    echo "$FOUND"
    exit 1
fi

echo "✅ No test helpers found in src/"
