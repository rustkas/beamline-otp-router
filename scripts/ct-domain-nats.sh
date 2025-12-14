#!/bin/bash
set -e

# Domain: NATS
# Runs all NATS-related test suites (excluding soak/stress)
# Tier: full (default)

echo "=== Starting NATS Domain Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}"

export ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}

# Gather suites (excluding soak, stress, chaos)
SUITES=$(ls test/router_nats_*_SUITE.erl 2>/dev/null | \
    grep -v '_soak_SUITE' | \
    grep -v '_stress_SUITE' | \
    grep -v '_chaos_SUITE' | \
    tr '\n' ',' | sed 's/,$//')

if [ -z "$SUITES" ]; then
    echo "No suites found for NATS domain."
    exit 1
fi

echo "Suites: $SUITES"
echo ""

rebar3 ct --suite "$SUITES"
