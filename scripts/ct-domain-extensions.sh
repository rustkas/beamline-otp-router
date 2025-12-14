#!/bin/bash
set -e

# Domain: Extensions
# Runs all extension-related test suites (excluding chaos/load)
# Tier: full (default)

echo "=== Starting Extensions Domain Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}"

export ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}

# Gather suites (excluding chaos, load, pipeline_load)
SUITES=$(ls test/router_extension*_SUITE.erl 2>/dev/null | \
    grep -v '_chaos_SUITE' | \
    grep -v '_load_SUITE' | \
    sort | uniq | tr '\n' ',' | sed 's/,$//')

if [ -z "$SUITES" ]; then
    echo "No suites found for Extensions domain."
    exit 1
fi

echo "Suites: $SUITES"
echo ""

rebar3 ct --suite "$SUITES"
