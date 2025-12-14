#!/bin/bash
set -e

# Domain: GRPC
# Runs all GRPC-related test suites
# Tier: full (default)

echo "=== Starting GRPC Domain Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}"

export ROUTER_TEST_LEVEL=${ROUTER_TEST_LEVEL:-full}

# Gather suites
# Matches router_*_grpc_*_SUITE.erl plus explicit gateway/health suites
SUITES=$(ls test/router_*_grpc_*_SUITE.erl test/router_gateway_integration_SUITE.erl test/router_health_integration_SUITE.erl 2>/dev/null | sort | uniq | tr '\n' ',' | sed 's/,$//')

if [ -z "$SUITES" ]; then
    echo "No suites found for GRPC domain."
    exit 1
fi

echo "Suites: $SUITES"

rebar3 ct --suite "$SUITES"
