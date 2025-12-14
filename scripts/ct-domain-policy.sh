#!/bin/bash
set -e

# Domain: Policy
# Runs all Policy-related test suites (excluding load/stress)
# Tier: full (default)

echo "=== Starting Policy Domain Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}"

export ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}

# Gather suites (excluding load, stress, soak)
SUITES=$(ls test/router_policy*_SUITE.erl test/router_sticky_store_SUITE.erl test/router_provider_integration_SUITE.erl 2>/dev/null | \
    grep -v '_load_SUITE' | \
    grep -v '_stress_SUITE' | \
    grep -v '_soak_SUITE' | \
    sort | uniq | tr '\n' ',' | sed 's/,$//')

if [ -z "$SUITES" ]; then
    echo "No suites found for Policy domain."
    exit 1
fi

echo "Suites: $SUITES"
echo ""

rebar3 ct --suite "$SUITES"
