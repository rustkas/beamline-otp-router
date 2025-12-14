#!/bin/bash
set -e

# Flakiness Probe Script
# Usage: ./scripts/ct-probe-flaky.sh <suite_path> [iterations]
# Example: ./scripts/ct-probe-flaky.sh test/router_nats_integration_SUITE.erl 5

SUITE=$1
ITERATIONS=${2:-5}

if [ -z "$SUITE" ]; then
    echo "Usage: $0 <suite_path> [iterations]"
    exit 1
fi

echo "=== Probing $SUITE for flakiness ($ITERATIONS iterations) ==="

for i in $(seq 1 $ITERATIONS); do
    echo "Run #$i..."
    ROUTER_TEST_LEVEL=full rebar3 ct --suite $SUITE > /dev/null 2>&1
    RET=$?
    if [ $RET -ne 0 ]; then
         echo "❌ FAILED on run #$i"
         echo "Run with verbose output to debug: ROUTER_TEST_LEVEL=full rebar3 ct --suite $SUITE"
         exit 1
    else
         echo "✅ PASSED"
    fi
done

echo "=== All $ITERATIONS runs passed! ==="
