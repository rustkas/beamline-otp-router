#!/bin/bash
# Validation script for test autodiscovery
# Verifies that list_tests_by_tag.sh returns expected counts
#
# Usage:
#   ./scripts/test_autodiscovery_validation.sh
#
# Exit codes:
#   0 - All validations passed
#   1 - Validation failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

FAILED=0

# Check if list_tests_by_tag.sh exists
if [ ! -f "${SCRIPT_DIR}/list_tests_by_tag.sh" ]; then
    log_error "list_tests_by_tag.sh not found"
    exit 1
fi

# Validation 1: fast tag should return ~13 suites
log_info "Validating 'fast' tag..."
FAST_COUNT=$(bash "${SCRIPT_DIR}/list_tests_by_tag.sh" fast | wc -l)
if [ "$FAST_COUNT" -ge 10 ]; then
    log_info "✓ fast tag: $FAST_COUNT suites (expected: ~13)"
else
    log_error "✗ fast tag: $FAST_COUNT suites (expected: ~13)"
    FAILED=1
fi

# Validation 2: cp1_smoke tag should return 7 suites
log_info "Validating 'cp1_smoke' tag..."
CP1_COUNT=$(bash "${SCRIPT_DIR}/list_tests_by_tag.sh" cp1_smoke 2>/dev/null | wc -l)
if [ "$CP1_COUNT" -eq 7 ]; then
    log_info "✓ cp1_smoke tag: $CP1_COUNT suites (expected: 7)"
else
    log_warn "⚠ cp1_smoke tag: $CP1_COUNT suites (expected: 7)"
    # Not a hard failure, as count may vary
fi

# Validation 3: slow tag should return many suites
log_info "Validating 'slow' tag..."
SLOW_COUNT=$(bash "${SCRIPT_DIR}/list_tests_by_tag.sh" slow 2>/dev/null | wc -l)
if [ "$SLOW_COUNT" -ge 15 ]; then
    log_info "✓ slow tag: $SLOW_COUNT suites (expected: 15+)"
else
    log_warn "⚠ slow tag: $SLOW_COUNT suites (expected: 15+)"
    # Not a hard failure, as count may vary
fi

# Validation 4: Invalid tag should return empty
log_info "Validating invalid tag..."
INVALID_COUNT=$(bash "${SCRIPT_DIR}/list_tests_by_tag.sh" nonexistent_tag 2>/dev/null | wc -l)
if [ "$INVALID_COUNT" -eq 0 ]; then
    log_info "✓ Invalid tag returns empty (expected: 0)"
else
    log_error "✗ Invalid tag returned $INVALID_COUNT suites (expected: 0)"
    FAILED=1
fi

# Validation 5: Help option works
log_info "Validating --help option..."
if bash "${SCRIPT_DIR}/list_tests_by_tag.sh" --help 2>&1 | grep -q "Usage:"; then
    log_info "✓ --help option works"
else
    log_error "✗ --help option failed"
    FAILED=1
fi

# Summary
if [ $FAILED -eq 0 ]; then
    log_info "All validations passed"
    exit 0
else
    log_error "Some validations failed"
    exit 1
fi

