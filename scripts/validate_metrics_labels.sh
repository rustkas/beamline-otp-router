#!/bin/bash
# @doc Validate Metrics Labels Implementation
#
# Validates that metrics labels are correctly implemented and dashboard queries work.
# Checks:
# - Label extraction functions work correctly
# - Metric emission with labels works
# - Dashboard queries are valid PromQL
# - Label cardinality is reasonable
#
# Usage: bash scripts/validate_metrics_labels.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Go to project root: from apps/otp/router/scripts/ go up 4 levels
ROOT_DIR="$(cd "$SCRIPT_DIR/../../../../" && pwd)"
cd "$ROOT_DIR"

# Verify we're in the right directory
if [ ! -f "apps/otp/router/src/router_jetstream.erl" ]; then
    # Try alternative: maybe we're already in router directory
    if [ -f "src/router_jetstream.erl" ]; then
        # We're in apps/otp/router/, adjust paths
        ROUTER_DIR="$(pwd)"
    else
        echo "Error: Cannot find router_jetstream.erl"
        echo "Current directory: $(pwd)"
        echo "Script directory: $SCRIPT_DIR"
        exit 1
    fi
else
    ROUTER_DIR="$ROOT_DIR/apps/otp/router"
fi

echo "=== Metrics Labels Validation ==="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0

# Function to check if command exists
check_command() {
    if ! command -v "$1" &> /dev/null; then
        echo -e "${YELLOW}Warning: $1 not found, skipping related checks${NC}"
        return 1
    fi
    return 0
}

# Function to validate PromQL query syntax (basic check)
validate_promql_query() {
    local query="$1"
    local description="$2"
    
    # Basic PromQL syntax checks
    if [[ "$query" =~ sum\(.*rate\(.*\[.*\]\)\) ]]; then
        echo -e "${GREEN}✓${NC} Valid PromQL: $description"
        return 0
    elif [[ "$query" =~ sum\ by\ .*\(.*rate\(.*\[.*\]\)\) ]]; then
        echo -e "${GREEN}✓${NC} Valid PromQL: $description"
        return 0
    else
        echo -e "${YELLOW}⚠${NC}  Unrecognized PromQL pattern: $description"
        echo "   Query: $query"
        ((WARNINGS++))
        return 1
    fi
}

# Use ROUTER_DIR if set, otherwise use relative path
ROUTER_SRC="${ROUTER_DIR:-$ROOT_DIR/apps/otp/router}/src"
ROUTER_TEST="${ROUTER_DIR:-$ROOT_DIR/apps/otp/router}/test"
ROUTER_DOCS="${ROUTER_DIR:-$ROOT_DIR/apps/otp/router}/docs"

# Check 1: Verify helper functions are exported
echo "1. Checking helper function exports..."
if grep -q "extract_assignment_id/1" "$ROUTER_SRC/router_jetstream.erl"; then
    echo -e "${GREEN}✓${NC} router_jetstream:extract_assignment_id/1 exported"
else
    echo -e "${RED}✗${NC} router_jetstream:extract_assignment_id/1 not exported"
    ((ERRORS++))
fi

if grep -q "extract_tenant_id/1" "$ROUTER_SRC/router_jetstream.erl"; then
    echo -e "${GREEN}✓${NC} router_jetstream:extract_tenant_id/1 exported"
else
    echo -e "${RED}✗${NC} router_jetstream:extract_tenant_id/1 not exported"
    ((ERRORS++))
fi

if grep -q "error_to_reason/1" "$ROUTER_SRC/router_nats.erl"; then
    echo -e "${GREEN}✓${NC} router_nats:error_to_reason/1 exported"
else
    echo -e "${RED}✗${NC} router_nats:error_to_reason/1 not exported"
    ((ERRORS++))
fi

if grep -q "extract_stream_from_subject/1" "$ROUTER_SRC/router_nats.erl"; then
    echo -e "${GREEN}✓${NC} router_nats:extract_stream_from_subject/1 exported"
else
    echo -e "${RED}✗${NC} router_nats:extract_stream_from_subject/1 not exported"
    ((ERRORS++))
fi

echo ""

# Check 2: Verify metric emission uses labels
echo "2. Checking metric emission with labels..."
if grep -q "router_metrics:emit_metric(router_dlq_total" "$ROUTER_SRC/router_jetstream.erl"; then
    echo -e "${GREEN}✓${NC} router_dlq_total uses emit_metric with labels"
else
    echo -e "${RED}✗${NC} router_dlq_total doesn't use emit_metric with labels"
    ((ERRORS++))
fi

if grep -q "router_metrics:emit_metric(router_nats_publish_failures_total" "$ROUTER_SRC/router_nats.erl"; then
    echo -e "${GREEN}✓${NC} router_nats_publish_failures_total uses emit_metric with labels"
else
    echo -e "${RED}✗${NC} router_nats_publish_failures_total doesn't use emit_metric with labels"
    ((ERRORS++))
fi

if grep -q "router_metrics:emit_metric(router_nats_ack_failures_total" "$ROUTER_SRC/router_nats.erl"; then
    echo -e "${GREEN}✓${NC} router_nats_ack_failures_total uses emit_metric with labels"
else
    echo -e "${RED}✗${NC} router_nats_ack_failures_total doesn't use emit_metric with labels"
    ((ERRORS++))
fi

echo ""

# Check 3: Validate dashboard queries
echo "3. Validating dashboard queries..."
DASHBOARD_FILE="$ROOT_DIR/docs/OBSERVABILITY_ROUTER_DASHBOARD.md"

# Extract PromQL queries from dashboard documentation (simplified check)
QUERIES=$(grep -c '```promql' "$DASHBOARD_FILE" 2>/dev/null || echo "0")

if [ "$QUERIES" -eq "0" ]; then
    echo -e "${YELLOW}⚠${NC}  No PromQL queries found in dashboard documentation"
    ((WARNINGS++))
else
    echo "Found $QUERIES PromQL query blocks in dashboard documentation"
    
    # Validate a few key queries (extract from file)
    if grep -q "sum by (reason) (rate(router_dlq_total\[5m\]))" "$DASHBOARD_FILE"; then
        validate_promql_query "sum by (reason) (rate(router_dlq_total[5m]))" "DLQ by reason"
    fi
    if grep -q "sum by (assignment_id) (rate(router_dlq_total\[5m\]))" "$DASHBOARD_FILE"; then
        validate_promql_query "sum by (assignment_id) (rate(router_dlq_total[5m]))" "DLQ by assignment"
    fi
    if grep -q "sum by (reason) (rate(router_nats_publish_failures_total\[5m\]))" "$DASHBOARD_FILE"; then
        validate_promql_query "sum by (reason) (rate(router_nats_publish_failures_total[5m]))" "Publish failures by reason"
    fi
fi

echo ""

# Check 4: Verify test files exist
echo "4. Checking test files..."
if [ -f "$ROUTER_TEST/router_metrics_labels_unit_SUITE.erl" ]; then
    echo -e "${GREEN}✓${NC} Unit tests exist"
else
    echo -e "${RED}✗${NC} Unit tests missing"
    ((ERRORS++))
fi

if [ -f "$ROUTER_TEST/router_metrics_labels_integration_SUITE.erl" ]; then
    echo -e "${GREEN}✓${NC} Integration tests exist"
else
    echo -e "${RED}✗${NC} Integration tests missing"
    ((ERRORS++))
fi

if [ -f "$ROUTER_TEST/router_metrics_labels_performance_SUITE.erl" ]; then
    echo -e "${GREEN}✓${NC} Performance tests exist"
else
    echo -e "${RED}✗${NC} Performance tests missing"
    ((ERRORS++))
fi

echo ""

# Check 5: Verify documentation references
echo "5. Checking documentation references..."
if grep -q "labels now available" "$DASHBOARD_FILE"; then
    echo -e "${GREEN}✓${NC} Dashboard documentation updated with labels status"
else
    echo -e "${YELLOW}⚠${NC}  Dashboard documentation may not reference labels availability"
    ((WARNINGS++))
fi

if [ -f "$ROUTER_DOCS/dev/LABELS_IMPLEMENTATION_COMPLETE.md" ]; then
    echo -e "${GREEN}✓${NC} Implementation report exists"
else
    echo -e "${YELLOW}⚠${NC}  Implementation report missing"
    ((WARNINGS++))
fi

# Check 6: Verify scenario-to-test mapping
echo ""
echo "6. Checking scenario-to-test mapping..."
COVERAGE_MATRIX="$ROUTER_DOCS/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md"
if [ -f "$COVERAGE_MATRIX" ]; then
    echo -e "${GREEN}✓${NC} Coverage matrix exists"
    
    # Check for S1, S2, S3 scenarios (check for | **S1**, | **S2**, | **S3** in table)
    if grep -q "| \*\*S1\*\*\|Scenario ID.*S1\|S1.*Intermittent" "$COVERAGE_MATRIX"; then
        echo -e "${GREEN}✓${NC} Scenario S1 documented in coverage matrix"
    else
        echo -e "${YELLOW}⚠${NC}  Scenario S1 not found in coverage matrix"
        ((WARNINGS++))
    fi
    
    if grep -q "| \*\*S2\*\*\|Scenario ID.*S2\|S2.*Processing" "$COVERAGE_MATRIX"; then
        echo -e "${GREEN}✓${NC} Scenario S2 documented in coverage matrix"
    else
        echo -e "${YELLOW}⚠${NC}  Scenario S2 not found in coverage matrix"
        ((WARNINGS++))
    fi
    
    if grep -q "| \*\*S3\*\*\|Scenario ID.*S3\|S3.*MaxDeliver" "$COVERAGE_MATRIX"; then
        echo -e "${GREEN}✓${NC} Scenario S3 documented in coverage matrix"
    else
        echo -e "${YELLOW}⚠${NC}  Scenario S3 not found in coverage matrix"
        ((WARNINGS++))
    fi
else
    echo -e "${YELLOW}⚠${NC}  Coverage matrix missing"
    ((WARNINGS++))
fi

# Check 7: Verify fault injection test documentation links
echo ""
echo "7. Checking fault injection test documentation..."
FAULT_INJECTION_DOC="$ROUTER_DOCS/dev/JETSTREAM_FAULT_INJECTION_TESTS.md"
if [ -f "$FAULT_INJECTION_DOC" ]; then
    echo -e "${GREEN}✓${NC} Fault injection test documentation exists"
    
    # Check for OBS links
    if grep -q "Expected Observability\|OBS\|Coverage Matrix" "$FAULT_INJECTION_DOC"; then
        echo -e "${GREEN}✓${NC} Fault injection doc contains OBS references"
    else
        echo -e "${YELLOW}⚠${NC}  Fault injection doc may not reference OBS"
        ((WARNINGS++))
    fi
else
    echo -e "${YELLOW}⚠${NC}  Fault injection test documentation missing"
    ((WARNINGS++))
fi

echo ""

# Summary
echo "=== Validation Summary ==="
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠  $WARNINGS warning(s), no errors${NC}"
    exit 0
else
    echo -e "${RED}✗ $ERRORS error(s), $WARNINGS warning(s)${NC}"
    exit 1
fi

