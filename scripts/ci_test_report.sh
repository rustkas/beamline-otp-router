#!/bin/bash
# CI Test Result Reporting
# Generates JUnit XML format test results for CI/CD integration
#
# Usage: ./scripts/ci_test_report.sh [test-output-dir]
# Output: test_results/junit.xml

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

OUTPUT_DIR="${1:-test_results}"
JUNIT_XML="${OUTPUT_DIR}/junit.xml"
CT_LOGS_DIR="_build/test/logs"

mkdir -p "${OUTPUT_DIR}"

echo "=== Generating CI Test Report ==="
echo "Output directory: ${OUTPUT_DIR}"
echo "JUnit XML: ${JUNIT_XML}"
echo ""

# Check if test logs exist
if [ ! -d "${CT_LOGS_DIR}" ]; then
    echo "Warning: Test logs directory not found: ${CT_LOGS_DIR}"
    echo "Run tests first: rebar3 ct"
    exit 1
fi

# Generate JUnit XML from Common Test logs
# Common Test logs are in _build/test/logs/
# We'll parse the test run summary and generate JUnit XML

cat > "${JUNIT_XML}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
EOF

# Count test results from CT logs
TOTAL_TESTS=0
TOTAL_FAILURES=0
TOTAL_ERRORS=0
TOTAL_SKIPPED=0

# Parse Common Test logs and generate test suite entries
for log_file in "${CT_LOGS_DIR}"/*.log; do
    if [ -f "${log_file}" ]; then
        SUITE_NAME=$(basename "${log_file}" .log)
        # Extract test results from log (basic parsing)
        # Common Test logs contain test results in a specific format
        # This is a simplified parser - full implementation would parse CT logs more accurately
        
        # Count test cases from log
        TEST_COUNT=$(grep -c "TEST CASE:" "${log_file}" 2>/dev/null || echo "0")
        FAILURE_COUNT=$(grep -c "FAILED" "${log_file}" 2>/dev/null || echo "0")
        ERROR_COUNT=$(grep -c "ERROR" "${log_file}" 2>/dev/null || echo "0")
        SKIP_COUNT=$(grep -c "SKIPPED" "${log_file}" 2>/dev/null || echo "0")
        
        TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
        TOTAL_FAILURES=$((TOTAL_FAILURES + FAILURE_COUNT))
        TOTAL_ERRORS=$((TOTAL_ERRORS + ERROR_COUNT))
        TOTAL_SKIPPED=$((TOTAL_SKIPPED + SKIP_COUNT))
        
        # Generate test suite entry
        cat >> "${JUNIT_XML}" <<EOFSUITE
  <testsuite name="${SUITE_NAME}" tests="${TEST_COUNT}" failures="${FAILURE_COUNT}" errors="${ERROR_COUNT}" skipped="${SKIP_COUNT}" time="0">
EOFSUITE
        
        # Extract individual test cases (simplified - would need full CT log parser)
        if [ "${TEST_COUNT}" -gt 0 ]; then
            # For now, create a placeholder test case entry
            # Full implementation would parse each test case from the log
            cat >> "${JUNIT_XML}" <<EOFTEST
    <testcase classname="${SUITE_NAME}" name="test_execution" time="0"/>
EOFTEST
        fi
        
        cat >> "${JUNIT_XML}" <<EOFSUITE
  </testsuite>
EOFSUITE
    fi
done

# Close testsuites tag
cat >> "${JUNIT_XML}" <<EOF
</testsuites>
EOF

# Update testsuites attributes with totals
sed -i "s/<testsuites>/<testsuites tests=\"${TOTAL_TESTS}\" failures=\"${TOTAL_FAILURES}\" errors=\"${TOTAL_ERRORS}\" skipped=\"${TOTAL_SKIPPED}\">/" "${JUNIT_XML}"

echo "✅ Test report generated: ${JUNIT_XML}"
echo "  Total tests: ${TOTAL_TESTS}"
echo "  Failures: ${TOTAL_FAILURES}"
echo "  Errors: ${TOTAL_ERRORS}"
echo "  Skipped: ${TOTAL_SKIPPED}"
echo ""
echo "CI/CD systems can parse this JUnit XML format for test result reporting."

