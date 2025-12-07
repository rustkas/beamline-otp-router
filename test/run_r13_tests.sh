#!/bin/bash
# Script to run R13 Metrics Under Faults tests
# Usage: ./run_r13_tests.sh [test-group|all]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$SCRIPT_DIR"
OTP_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$OTP_DIR" || exit 1

# Default to all tests
TEST_GROUP="${1:-all}"

echo "=========================================="
echo "R13 Metrics Under Faults Tests"
echo "=========================================="
echo "Test Group: $TEST_GROUP"
echo "Router Directory: $ROUTER_DIR"
echo "OTP Directory: $OTP_DIR"
echo ""

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo "Error: rebar3 is not installed or not in PATH"
    exit 1
fi

# Compile first
echo "Step 1: Compiling..."
cd "$OTP_DIR" || exit 1
if ! rebar3 compile; then
    echo "Error: Compilation failed"
    exit 1
fi

echo ""
echo "Step 2: Running R13 tests..."

# Run tests based on group
cd "$ROUTER_DIR" || exit 1

case "$TEST_GROUP" in
    all)
        echo "Running all R13 test groups..."
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --logdir ct_logs/r13
        ;;
    aggregation)
        echo "Running aggregation tests..."
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group aggregation_tests --logdir ct_logs/r13
        ;;
    rate)
        echo "Running rate tests..."
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group rate_tests --logdir ct_logs/r13
        ;;
    cardinality)
        echo "Running cardinality tests..."
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group cardinality_tests --logdir ct_logs/r13
        ;;
    combined)
        echo "Running combined tests..."
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group combined_tests --logdir ct_logs/r13
        ;;
    *)
        echo "Error: Unknown test group: $TEST_GROUP"
        echo "Available groups: all, aggregation, rate, cardinality, combined"
        exit 1
        ;;
esac

EXIT_CODE=$?

echo ""
if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Some tests failed. Check logs in ct_logs/r13/"
fi

echo ""
echo "Test logs location: $ROUTER_DIR/ct_logs/r13/"
echo ""

exit $EXIT_CODE

