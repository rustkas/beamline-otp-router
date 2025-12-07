#!/bin/bash
# Run Quick Stress and Soak Test (for development)
#
# Usage:
#   ./scripts/run_quick_stress_soak_test.sh [test_case]
#
# This script runs a short (1-hour) stress/soak test for development purposes.
# It's faster than full tests and suitable for local validation.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default test case
TEST_CASE="${1:-test_single_fault_connect_soak}"

# Set duration to 1 hour for quick test
export STRESS_SOAK_DURATION_HOURS=1

echo "=========================================="
echo "Quick Stress and Soak Test (Development)"
echo "=========================================="
echo "Test Case: $TEST_CASE"
echo "Duration: 1 hour (quick test)"
echo "=========================================="
echo ""

cd "$ROUTER_DIR"

# Run test
echo "Starting quick test at $(date)..."
echo ""

if rebar3 ct \
    --suite router_stress_soak_SUITE \
    --case "$TEST_CASE" \
    --timeout 4000 \
    --verbose; then
    
    echo ""
    echo "=========================================="
    echo "Quick test PASSED"
    echo "=========================================="
    exit 0
else
    echo ""
    echo "=========================================="
    echo "Quick test FAILED"
    echo "=========================================="
    exit 1
fi

