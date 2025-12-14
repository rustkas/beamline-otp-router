#!/bin/bash
set -e

# Domain: Intake / E2E Flow
# Runs suites related to message intake, CAF handoff, and metrics
# Tier: full (default)

echo "=== Starting Intake Domain Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}"

export ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}

# Gather suites (excluding chaos, overload)
SUITES=$(ls test/router_intake_*_SUITE.erl test/router_caf_*_SUITE.erl test/router_cp1_fields_integration_SUITE.erl test/router_metrics_labels_integration_SUITE.erl 2>/dev/null | \
    grep -v '_chaos_SUITE' | \
    grep -v '_overload_SUITE' | \
    sort | uniq | tr '\n' ',' | sed 's/,$//')

if [ -z "$SUITES" ]; then
    echo "No suites found for Intake domain."
    exit 1
fi

echo "Suites: $SUITES"
echo ""

rebar3 ct --suite "$SUITES"
