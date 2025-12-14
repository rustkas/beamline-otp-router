#!/bin/bash
set -e

# Integration Suite Runner
# Runs all integration-level test suites with ROUTER_TEST_LEVEL=full

echo "=== Starting Integration Test Run (Integration Gate) ==="
echo "Mode: ROUTER_TEST_LEVEL=full"

export ROUTER_TEST_LEVEL=full

# Define integration suites
# We dynamically find all files matching router_*_integration_SUITE.erl
SUITES=$(ls test/router_*_integration_SUITE.erl | tr '\n' ',' | sed 's/,$//')

echo "Suites: $SUITES"

# Run tests
rebar3 ct --suite "$SUITES"

# Check exit code (rebar3 ct returns non-zero on failure)
STATUS=$?

if [ $STATUS -eq 0 ]; then
    echo "=== Integration Tests PASSED ==="
else
    echo "=== Integration Tests FAILED ==="
fi

exit $STATUS
