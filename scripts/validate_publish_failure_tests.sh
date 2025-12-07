#!/bin/bash
# Script to validate stability of router_nats_publish_failure_SUITE
# Runs the suite multiple times to verify no flaky tests

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="${SCRIPT_DIR}/../apps/otp/router"
ITERATIONS=${1:-5}  # Default: 5 iterations

cd "$ROUTER_DIR" || exit 1

echo "=========================================="
echo "Publish Failure Test Suite Stability Check"
echo "=========================================="
echo ""
echo "Suite: router_nats_publish_failure_SUITE"
echo "Iterations: $ITERATIONS"
echo ""

# Track results
PASSED=0
FAILED=0
TOTAL_TIME=0

# Run tests multiple times
for i in $(seq 1 "$ITERATIONS"); do
    echo "----------------------------------------"
    echo "Iteration $i/$ITERATIONS"
    echo "----------------------------------------"
    
    START_TIME=$(date +%s)
    
    if rebar3 ct --suite test/router_nats_publish_failure_SUITE --logdir "ct_logs_stability_$i" > "stability_run_$i.log" 2>&1; then
        END_TIME=$(date +%s)
        DURATION=$((END_TIME - START_TIME))
        TOTAL_TIME=$((TOTAL_TIME + DURATION))
        PASSED=$((PASSED + 1))
        echo "✓ PASSED (${DURATION}s)"
    else
        END_TIME=$(date +%s)
        DURATION=$((END_TIME - START_TIME))
        TOTAL_TIME=$((TOTAL_TIME + DURATION))
        FAILED=$((FAILED + 1))
        echo "✗ FAILED (${DURATION}s)"
        echo "  See: stability_run_$i.log"
    fi
    
    echo ""
done

# Summary
echo "=========================================="
echo "Stability Check Summary"
echo "=========================================="
echo "Total iterations: $ITERATIONS"
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Average duration: $((TOTAL_TIME / ITERATIONS))s"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "✓ All iterations passed - suite is stable"
    exit 0
else
    echo "✗ Some iterations failed - suite may be flaky"
    echo ""
    echo "Failed runs:"
    for i in $(seq 1 "$ITERATIONS"); do
        if ! grep -q "passed" "stability_run_$i.log" 2>/dev/null; then
            echo "  - Iteration $i: stability_run_$i.log"
        fi
    done
    exit 1
fi

