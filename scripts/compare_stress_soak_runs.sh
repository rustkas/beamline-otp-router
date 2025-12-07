#!/bin/bash
# Compare Stress and Soak Test Runs
#
# Usage:
#   ./scripts/compare_stress_soak_runs.sh [baseline_dir] [current_dir]
#
# Examples:
#   ./scripts/compare_stress_soak_runs.sh stress_soak_results/test_*_20250126_* stress_soak_results/test_*_20250127_*
#
# This script compares two stress/soak test runs and generates a comparison report.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Check arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <baseline_dir> <current_dir>"
    echo ""
    echo "Examples:"
    echo "  $0 stress_soak_results/test_baseline_normal_soak_20250126_120000 stress_soak_results/test_baseline_normal_soak_20250127_120000"
    exit 1
fi

BASELINE_DIR="$1"
CURRENT_DIR="$2"

# Check if directories exist
if [ ! -d "$BASELINE_DIR" ]; then
    echo "Error: Baseline directory not found: $BASELINE_DIR"
    exit 1
fi

if [ ! -d "$CURRENT_DIR" ]; then
    echo "Error: Current directory not found: $CURRENT_DIR"
    exit 1
fi

# Create comparison output directory
COMPARISON_DIR="${CURRENT_DIR}_comparison"
mkdir -p "$COMPARISON_DIR"

echo "=========================================="
echo "Stress and Soak Test Comparison"
echo "=========================================="
echo "Baseline: $BASELINE_DIR"
echo "Current:  $CURRENT_DIR"
echo "Output:   $COMPARISON_DIR"
echo "=========================================="
echo ""

# Extract test information
BASELINE_TEST=$(basename "$BASELINE_DIR" | cut -d'_' -f1-4)
CURRENT_TEST=$(basename "$CURRENT_DIR" | cut -d'_' -f1-4)

if [ "$BASELINE_TEST" != "$CURRENT_TEST" ]; then
    echo "Warning: Comparing different test types: $BASELINE_TEST vs $CURRENT_TEST"
fi

# Compare summary files if they exist
if [ -f "$BASELINE_DIR/summary.txt" ] && [ -f "$CURRENT_DIR/summary.txt" ]; then
    echo "Comparing summaries..."
    {
        echo "=== Summary Comparison ==="
        echo ""
        echo "Baseline:"
        cat "$BASELINE_DIR/summary.txt"
        echo ""
        echo "Current:"
        cat "$CURRENT_DIR/summary.txt"
    } > "$COMPARISON_DIR/summary_comparison.txt"
fi

# Compare test outputs
if [ -f "$BASELINE_DIR/test_output.log" ] && [ -f "$CURRENT_DIR/test_output.log" ]; then
    echo "Comparing test outputs..."
    
    # Extract resource summaries
    BASELINE_RESOURCES=$(grep -A 20 "Resource Summary" "$BASELINE_DIR/test_output.log" || echo "No resource summary found")
    CURRENT_RESOURCES=$(grep -A 20 "Resource Summary" "$CURRENT_DIR/test_output.log" || echo "No resource summary found")
    
    {
        echo "=== Resource Summary Comparison ==="
        echo ""
        echo "Baseline:"
        echo "$BASELINE_RESOURCES"
        echo ""
        echo "Current:"
        echo "$CURRENT_RESOURCES"
    } > "$COMPARISON_DIR/resource_comparison.txt"
fi

# Generate comparison report
{
    echo "# Stress and Soak Test Comparison Report"
    echo ""
    echo "**Generated**: $(date)"
    echo ""
    echo "## Test Information"
    echo ""
    echo "- **Baseline**: $BASELINE_DIR"
    echo "- **Current**: $CURRENT_DIR"
    echo "- **Test Type**: $CURRENT_TEST"
    echo ""
    echo "## Comparison Results"
    echo ""
    
    # Check if tests passed
    BASELINE_STATUS=$(grep -i "status:" "$BASELINE_DIR/summary.txt" 2>/dev/null | cut -d: -f2 | tr -d ' ' || echo "UNKNOWN")
    CURRENT_STATUS=$(grep -i "status:" "$CURRENT_DIR/summary.txt" 2>/dev/null | cut -d: -f2 | tr -d ' ' || echo "UNKNOWN")
    
    echo "### Test Status"
    echo ""
    echo "- **Baseline Status**: $BASELINE_STATUS"
    echo "- **Current Status**: $CURRENT_STATUS"
    echo ""
    
    if [ "$BASELINE_STATUS" = "PASSED" ] && [ "$CURRENT_STATUS" != "PASSED" ]; then
        echo "⚠️ **Regression Detected**: Test status changed from PASSED to $CURRENT_STATUS"
    elif [ "$BASELINE_STATUS" != "PASSED" ] && [ "$CURRENT_STATUS" = "PASSED" ]; then
        echo "✅ **Improvement**: Test status changed from $BASELINE_STATUS to PASSED"
    elif [ "$BASELINE_STATUS" = "PASSED" ] && [ "$CURRENT_STATUS" = "PASSED" ]; then
        echo "✅ **Stable**: Both tests passed"
    fi
    echo ""
    
    echo "## Detailed Comparisons"
    echo ""
    echo "See:"
    echo "- [Summary Comparison](./summary_comparison.txt)"
    echo "- [Resource Comparison](./resource_comparison.txt)"
    echo ""
    
} > "$COMPARISON_DIR/comparison_report.md"

echo "Comparison report generated: $COMPARISON_DIR/comparison_report.md"
echo ""
echo "Files created:"
ls -lh "$COMPARISON_DIR"

