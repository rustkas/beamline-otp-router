#!/bin/bash
# CI Coverage Report
# Generates coverage report in CI-friendly format (JSON + HTML)
#
# Usage: ./scripts/ci_coverage_report.sh [output-dir]
# Output: coverage_report.json, coverage_report.html

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

OUTPUT_DIR="${1:-test_results}"
COVERAGE_JSON="${OUTPUT_DIR}/coverage_report.json"
COVERAGE_DIR="_build/test/cover"
COVERAGE_HTML_DIR="${COVERAGE_DIR}/html"

mkdir -p "${OUTPUT_DIR}"

echo "=== Generating CI Coverage Report ==="
echo "Output directory: ${OUTPUT_DIR}"
echo "Coverage JSON: ${COVERAGE_JSON}"
echo ""

# Check if coverage data exists
if [ ! -d "${COVERAGE_DIR}" ]; then
    echo "Warning: Coverage directory not found: ${COVERAGE_DIR}"
    echo "Run tests with coverage first: rebar3 ct --cover && rebar3 cover"
    exit 1
fi

# Generate coverage report JSON
cat > "${COVERAGE_JSON}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "component": "router",
  "coverage_type": "code_coverage",
  "coverage_directory": "${COVERAGE_DIR}",
  "html_report": "${COVERAGE_HTML_DIR}/index.html",
  "thresholds": {
    "line_coverage": 80,
    "branch_coverage": 70,
    "function_coverage": 90
  },
  "summary": {
    "note": "Detailed coverage metrics available in HTML report and cover.log"
  }
}
EOF

# Extract coverage metrics from cover.log if available
if [ -f "${COVERAGE_DIR}/cover.log" ]; then
    # Try to extract coverage percentages from cover.log
    LINE_COV=$(grep -oP "Line coverage: \K[0-9.]+" "${COVERAGE_DIR}/cover.log" | head -1 || echo "0")
    BRANCH_COV=$(grep -oP "Branch coverage: \K[0-9.]+" "${COVERAGE_DIR}/cover.log" | head -1 || echo "0")
    FUNC_COV=$(grep -oP "Function coverage: \K[0-9.]+" "${COVERAGE_DIR}/cover.log" | head -1 || echo "0")
    
    # Update JSON with actual coverage values
    cat > "${COVERAGE_JSON}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "component": "router",
  "coverage_type": "code_coverage",
  "coverage_directory": "${COVERAGE_DIR}",
  "html_report": "${COVERAGE_HTML_DIR}/index.html",
  "thresholds": {
    "line_coverage": 80,
    "branch_coverage": 70,
    "function_coverage": 90
  },
  "metrics": {
    "line_coverage": ${LINE_COV:-0},
    "branch_coverage": ${BRANCH_COV:-0},
    "function_coverage": ${FUNC_COV:-0}
  },
  "meets_thresholds": {
    "line_coverage": $(awk "BEGIN {print (${LINE_COV:-0} >= 80) ? "true" : "false"}"),
    "branch_coverage": $(awk "BEGIN {print (${BRANCH_COV:-0} >= 70) ? "true" : "false"}"),
    "function_coverage": $(awk "BEGIN {print (${FUNC_COV:-0} >= 90) ? "true" : "false"}")
  }
}
EOF
fi

echo "✅ Coverage report generated: ${COVERAGE_JSON}"
if [ -d "${COVERAGE_HTML_DIR}" ]; then
    echo "  HTML report: ${COVERAGE_HTML_DIR}/index.html"
fi
echo ""
echo "CI/CD systems can parse this JSON format for coverage reporting."

