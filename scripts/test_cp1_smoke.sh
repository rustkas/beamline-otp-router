#!/bin/bash
# CP1 Baseline Smoke Test Runner
# Runs minimal CP1 test suite without heavy JetStream tests

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Show help
show_help() {
    cat <<EOF
CP1 Baseline Smoke Test Runner

DESCRIPTION:
  Runs minimal CP1 test suite without heavy JetStream tests.
  Verifies CP1 baseline functionality:
  - Router core functionality
  - E2E smoke tests
  - RBAC (Role-Based Access Control)
  - Policy enforcement
  - Decider logic
  - Policy store
  - Error handling

  Excludes CP2+ features:
  - JetStream E2E tests
  - Idempotency (CP2+)
  - Tenant validation (CP2+)
  - Enhanced CAF adapter features

USAGE:
  $0 [OPTIONS]

OPTIONS:
  -v, --verbose         Run tests in verbose mode
  -h, --help           Show this help message

EXAMPLES:
  # Run CP1 smoke tests (default)
  $0

  # Run CP1 smoke tests with verbose output
  $0 --verbose
  $0 -v

EXIT CODES:
  0  - All CP1 smoke tests passed
  1  - Test execution failed (one or more tests failed)
  2  - Compilation failed (Router not compiled)

TROUBLESHOOTING:
  Exit code 2 (Compilation failed):
    - Router not compiled: Run 'rebar3 compile'
    - Check compilation errors in output

  Exit code 1 (Test execution failed):
    - Check test logs: _build/test/logs/
    - Run with --verbose for more details

For more information, see:
  - docs/CP1_BASELINE.md
  - docs/TEST_CLASSIFICATION.md

EOF
}

# Helper functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

# Parse arguments
VERBOSE=false
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        *)
            log_error "Error: Unknown option '$1'"
            echo ""
            show_help
            exit 1
            ;;
    esac
done

echo -e "${GREEN}=== CP1 Baseline Smoke Test Suite ===${NC}"
echo ""
echo "Running minimal CP1 test suite (excluding JetStream heavy tests)"
echo ""

# CP1 Baseline test suites (minimal smoke set)
# Note: These suites are marked with @test_category cp1_smoke, fast in their module comments
#
# Autodiscovery: If ROUTER_TEST_AUTODISCOVERY=1, suites are discovered automatically via list_tests_by_tag.sh
# Otherwise, use explicit list below
if [ "${ROUTER_TEST_AUTODISCOVERY:-0}" = "1" ]; then
    log_info "Using autodiscovery for CP1 smoke test suites..."
    CP1_SMOKE_SUITES=()
    while IFS= read -r SUITE; do
        if [ -n "${SUITE}" ]; then
            CP1_SMOKE_SUITES+=("${SUITE}")
        fi
    done < <("${SCRIPT_DIR}/list_tests_by_tag.sh" cp1_smoke 2>/dev/null || true)
    
    if [ ${#CP1_SMOKE_SUITES[@]} -eq 0 ]; then
        log_warn "Autodiscovery found no CP1 smoke test suites, falling back to explicit list"
        # Fall back to explicit list
        CP1_SMOKE_SUITES=(
            "router_core_SUITE"              # @test_category cp1_smoke, fast
            "router_e2e_smoke_SUITE"         # @test_category cp1_smoke, fast
            "router_rbac_SUITE"              # @test_category cp1_smoke, fast
            "router_policy_enforcement_SUITE" # @test_category cp1_smoke, fast
            "router_decider_SUITE"           # @test_category cp1_smoke, fast
            "router_policy_store_SUITE"       # @test_category cp1_smoke, fast
            "router_error_SUITE"             # @test_category cp1_smoke, fast
        )
    else
        log_info "Autodiscovery found ${#CP1_SMOKE_SUITES[@]} CP1 smoke test suites"
    fi
else
    # Explicit list (default behavior)
    CP1_SMOKE_SUITES=(
        "router_core_SUITE"              # @test_category cp1_smoke, fast
        "router_e2e_smoke_SUITE"         # @test_category cp1_smoke, fast
        "router_rbac_SUITE"              # @test_category cp1_smoke, fast
        "router_policy_enforcement_SUITE" # @test_category cp1_smoke, fast
        "router_decider_SUITE"           # @test_category cp1_smoke, fast
        "router_policy_store_SUITE"       # @test_category cp1_smoke, fast
        "router_error_SUITE"             # @test_category cp1_smoke, fast
    )
fi

# CP2+ test suites (excluded from CP1 smoke)
CP2_EXCLUDED_SUITES=(
    "router_jetstream_e2e_SUITE"           # Heavy JetStream E2E tests
    "router_delivery_count_tracking_SUITE" # JetStream delivery count
    "router_idempotency_SUITE"             # CP2+ idempotency
    "router_tenant_allowlist_SUITE"        # CP2+ tenant validation
    "router_result_consumer_SUITE"         # May use JetStream features
    "router_caf_adapter_SUITE"             # May use JetStream features
    "router_caf_adapter_enhanced_SUITE"    # Enhanced features
    "router_nats_subscriber_caf_SUITE"     # NATS/JetStream integration
)

echo -e "${YELLOW}CP1 Baseline Test Suites (${#CP1_SMOKE_SUITES[@]} suites):${NC}"
for suite in "${CP1_SMOKE_SUITES[@]}"; do
    echo "  ✓ ${suite}"
done
echo ""

echo -e "${YELLOW}Excluded CP2+ Test Suites (${#CP2_EXCLUDED_SUITES[@]} suites):${NC}"
for suite in "${CP2_EXCLUDED_SUITES[@]}"; do
    echo "  ✗ ${suite}"
done
echo ""

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 not found. Please install rebar3 first.${NC}"
    exit 1
fi

# Compile test suites
echo -e "${GREEN}Compiling test suites...${NC}"
if ! rebar3 compile; then
    echo -e "${RED}Compilation failed${NC}"
    exit 2
fi

# Build test suites list for rebar3 ct
SUITES_ARG=""
for suite in "${CP1_SMOKE_SUITES[@]}"; do
    SUITES_ARG="${SUITES_ARG} --suite test/${suite}"
done

# Run Common Test with CP1 smoke suites
echo -e "${GREEN}Running CP1 smoke tests...${NC}"
echo ""

if [[ "${VERBOSE}" == "true" ]]; then
    rebar3 ct --dir test ${SUITES_ARG} --verbose
else
    rebar3 ct --dir test ${SUITES_ARG}
fi

EXIT_CODE=$?

if [[ ${EXIT_CODE} -eq 0 ]]; then
    echo ""
    echo -e "${GREEN}✓ All CP1 smoke tests passed${NC}"
else
    echo ""
    echo -e "${RED}✗ Some CP1 smoke tests failed (exit code: ${EXIT_CODE})${NC}"
fi

exit ${EXIT_CODE}

