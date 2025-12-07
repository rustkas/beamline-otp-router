#!/bin/bash
# Run Extended Stress and Soak Tests
#
# Usage:
#   ./scripts/run_stress_soak_test.sh [test_case] [duration_hours]
#
# Examples:
#   ./scripts/run_stress_soak_test.sh test_single_fault_connect_soak 2
#   ./scripts/run_stress_soak_test.sh test_baseline_normal_soak 4
#   ./scripts/run_stress_soak_test.sh test_multi_fault_triple_soak 6
#
# Environment Variables:
#   STRESS_SOAK_DURATION_HOURS: Test duration in hours (default: 1)
#   STRESS_SOAK_SCENARIO: Test scenario name
#   STRESS_SOAK_OUTPUT_DIR: Output directory for reports (default: ./stress_soak_results)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default values
DURATION_HOURS="${STRESS_SOAK_DURATION_HOURS:-1}"
TEST_CASE="${1:-test_single_fault_connect_soak}"
OUTPUT_DIR="${STRESS_SOAK_OUTPUT_DIR:-$ROUTER_DIR/stress_soak_results}"

# Override duration if provided as second argument
if [ $# -ge 2 ]; then
    DURATION_HOURS="$2"
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Generate timestamp for this run
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RUN_DIR="$OUTPUT_DIR/${TEST_CASE}_${TIMESTAMP}"
mkdir -p "$RUN_DIR"

echo "=========================================="
echo "Extended Stress and Soak Test"
echo "=========================================="
echo "Test Case: $TEST_CASE"
echo "Duration: $DURATION_HOURS hours"
echo "Output Directory: $RUN_DIR"
echo "=========================================="
echo ""

# Export environment variables
export STRESS_SOAK_DURATION_HOURS="$DURATION_HOURS"
export STRESS_SOAK_OUTPUT_DIR="$RUN_DIR"

# Calculate expected duration in seconds (add 10% buffer)
EXPECTED_DURATION_SEC=$(echo "$DURATION_HOURS * 3600 * 1.1" | bc | cut -d. -f1)

# Run test with timeout
echo "Starting test at $(date)..."
echo ""

cd "$ROUTER_DIR"

# Run Common Test with extended timeout
if rebar3 ct \
    --suite router_stress_soak_SUITE \
    --case "$TEST_CASE" \
    --timeout "$EXPECTED_DURATION_SEC" \
    --logdir "$RUN_DIR/logs" \
    > "$RUN_DIR/test_output.log" 2>&1; then
    
    TEST_STATUS="PASSED"
    echo ""
    echo "=========================================="
    echo "Test PASSED"
    echo "=========================================="
else
    TEST_STATUS="FAILED"
    echo ""
    echo "=========================================="
    echo "Test FAILED"
    echo "=========================================="
fi

echo "Test completed at $(date)"
echo "Results saved to: $RUN_DIR"
echo ""

# Generate summary report
cat > "$RUN_DIR/summary.txt" <<EOF
Stress and Soak Test Summary
============================
Test Case: $TEST_CASE
Duration: $DURATION_HOURS hours
Status: $TEST_STATUS
Start Time: $(date -d @$(stat -c %Y "$RUN_DIR/test_output.log") 2>/dev/null || echo "N/A")
End Time: $(date)
Output Directory: $RUN_DIR
EOF

# Display summary
cat "$RUN_DIR/summary.txt"

# Exit with appropriate code
if [ "$TEST_STATUS" = "PASSED" ]; then
    exit 0
else
    exit 1
fi

