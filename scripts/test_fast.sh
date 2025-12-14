#!/bin/bash
# Fast Test Runner (Smoke/Contract Tests)
# Runs fast test suites only (excludes load/JetStream E2E/property tests)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

# Use local rebar3 built for OTP 27
export PATH="$(pwd)/bin:$PATH"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Show help
show_help() {
    cat <<EOF
Fast Test Runner (Smoke/Contract Tests)

DESCRIPTION:
  Runs fast test suites only, excluding slow tests:
  - Load tests
  - JetStream E2E tests
  - Property-based tests

  Fast test suites include:
  - CP1 smoke tests (router_core_basic_SUITE, router_e2e_smoke_SUITE, etc.)
  - Contract tests (router_gateway_contract_smoke_SUITE)
  - Unit tests (router_caf_adapter_unit_SUITE, router_grpc_SUITE, etc.)

USAGE:
  $0 [OPTIONS]

OPTIONS:
  -v, --verbose         Run tests in verbose mode
  -h, --help           Show this help message

EXAMPLES:
  # Run fast tests (default)
  $0

  # Run fast tests with verbose output
  $0 --verbose
  $0 -v

EXIT CODES:
  0  - All fast tests passed
  1  - Test execution failed (one or more tests failed)
  2  - Compilation failed (Router not compiled)

TROUBLESHOOTING:
  Exit code 2 (Compilation failed):
    - Router not compiled: Run 'rebar3 compile'
    - Check compilation errors in output

  Exit code 1 (Test execution failed):
    - Check test logs: _build/test/logs/
    - Run with --verbose for more details

For more information, see: docs/TEST_CLASSIFICATION.md

EOF
}

# Helper functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
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

echo -e "${GREEN}=== Fast Test Suite (Smoke/Contract) ===${NC}"
echo ""
echo "Running fast test suites (excludes load/JetStream E2E/property tests)"
echo ""

# Fast test suites (smoke/contract)
# Note: These suites are marked with @test_category fast in their module comments
#
# Autodiscovery: If ROUTER_TEST_AUTODISCOVERY=1, suites are discovered automatically via list_tests_by_tag.sh
# Otherwise, use explicit list below
if [ "${ROUTER_TEST_AUTODISCOVERY:-0}" = "1" ]; then
    log_info "Using autodiscovery for fast test suites..."
    FAST_TEST_SUITES=()
    while IFS= read -r SUITE; do
        if [ -n "${SUITE}" ]; then
            FAST_TEST_SUITES+=("${SUITE}")
        fi
    done < <("${SCRIPT_DIR}/list_tests_by_tag.sh" fast 2>/dev/null || true)
    
    if [ ${#FAST_TEST_SUITES[@]} -eq 0 ]; then
        log_error "Autodiscovery found no fast test suites, falling back to explicit list"
        # Fall back to explicit list (same as below)
        FAST_TEST_SUITES=(
            "router_core_basic_SUITE"
            "router_e2e_smoke_SUITE"
            "router_decider_SUITE"
            "router_error_SUITE"
            "router_policy_store_SUITE"
            "router_policy_enforcement_SUITE"
            "router_policy_dsl_parsing_SUITE"
            "router_policy_validator_SUITE"
            "router_rbac_SUITE"
            "router_grpc_SUITE"
            "router_grpc_integration_SUITE"
            "router_nats_contract_validation_SUITE"
            "router_caf_adapter_unit_SUITE"
            "router_core_telemetry_contract_SUITE"
            "router_metrics_format_SUITE"
            "router_metrics_labels_unit_SUITE"
            "router_circuit_breaker_smoke_SUITE"
            "router_secrets_logging_SUITE"
        )
    else
        log_info "Autodiscovery found ${#FAST_TEST_SUITES[@]} fast test suites"
    fi
else
    # Explicit list (default behavior)
    # Covers critical domains: core, policy, NATS, metrics, circuit breaker, gRPC
    FAST_TEST_SUITES=(
        # Core domain
        "router_core_basic_SUITE"              # @test_category cp1_smoke, fast
        "router_e2e_smoke_SUITE"               # @test_category cp1_smoke, fast
        "router_decider_SUITE"                 # @test_category cp1_smoke, fast
        "router_error_SUITE"                   # @test_category cp1_smoke, fast
        
        # Policy domain
        "router_policy_store_SUITE"            # @test_category cp1_smoke, fast
        "router_policy_enforcement_SUITE"      # @test_category cp1_smoke, fast
        "router_policy_dsl_parsing_SUITE"      # DSL parsing (from_map/2)
        "router_policy_validator_SUITE"        # Policy validation
        
        # Security domain
        "router_rbac_SUITE"                    # @test_category cp1_smoke, fast
        
        # gRPC domain
        "router_grpc_SUITE"                    # @test_category fast
        "router_grpc_integration_SUITE"        # @test_category fast
        
        # NATS domain
        "router_nats_contract_validation_SUITE" # @test_category fast
        
        # CAF/Adapter domain
        "router_caf_adapter_unit_SUITE"        # @test_category fast
        
        # Metrics/Telemetry domain
        "router_core_telemetry_contract_SUITE" # @test_category fast
        "router_metrics_format_SUITE"          # Metrics format validation
        "router_metrics_labels_unit_SUITE"     # Metrics labels validation
        
        # Circuit breaker domain
        "router_circuit_breaker_smoke_SUITE"   # CB smoke tests
        
        # Security/Logging domain
        "router_secrets_logging_SUITE"         # @test_category fast
    )
fi

# Slow test suites (excluded from fast run)
# NOTE: Suite names updated to match current split structure
SLOW_TEST_SUITES=(
    "router_jetstream_e2e_SUITE"           # Heavy JetStream E2E tests
    "router_delivery_count_tracking_SUITE"  # JetStream delivery count
    "router_result_consumer_core_SUITE"     # Result consumer (core)
    "router_result_consumer_faults_SUITE"   # Result consumer (faults)
    "router_caf_adapter_SUITE"              # May use JetStream features
    "router_caf_adapter_enhanced_SUITE"     # Enhanced features
    "router_caf_adapter_advanced_SUITE"     # Advanced CAF tests
    "router_nats_subscriber_caf_SUITE"      # NATS/JetStream integration
    "router_decider_prop_SUITE"             # Property-based tests
    "router_policy_store_prop_SUITE"        # Property-based tests
    "router_normalize_boolean_prop_SUITE"   # Property-based tests
    "router_options_merge_prop_SUITE"       # Property-based tests
    "router_policy_store_load_SUITE"        # Load tests
    "router_idem_core_SUITE"                # Idempotency core
    "router_idempotency_core_SUITE"         # Idempotency core (alias)
    "router_tenant_allowlist_SUITE"         # CP2+ tenant validation
    "router_policy_store_fault_tolerance_SUITE"  # Fault tolerance
    "router_admin_grpc_integration_SUITE"   # CP2+ admin gRPC
    "router_admin_grpc_concurrency_SUITE"   # CP2+ concurrency
    "router_assignment_SUITE"               # Assignment handling
    "router_sticky_store_SUITE"             # Sticky session store
    "router_policy_SUITE"                   # Policy management
    "router_policy_validator_SUITE"         # Policy validation
    "router_ets_guard_SUITE"                # ETS guard tests
    "router_error_status_SUITE"             # Error status handling
    "router_decide_consumer_core_SUITE"     # Decide consumer (core)
    "router_decide_consumer_faults_SUITE"   # Decide consumer (faults)
    "router_decide_consumer_heavy_SUITE"    # Decide consumer (heavy)
)

echo -e "${YELLOW}Fast Test Suites (${#FAST_TEST_SUITES[@]} suites):${NC}"
for suite in "${FAST_TEST_SUITES[@]}"; do
    echo "  ✓ ${suite}"
done
echo ""

echo -e "${YELLOW}Excluded Slow Test Suites (${#SLOW_TEST_SUITES[@]} suites):${NC}"
for suite in "${SLOW_TEST_SUITES[@]}"; do
    echo "  ✗ ${suite}"
done
echo ""

# Static lint of suites before running CT
erl -noshell -pa test -pa test_support -eval 'case compile:file("test_support/router_suite_linter", [report]) of {ok, _} -> ok; Error -> io:format("router_suite_linter compile failed: ~p~n", [Error]), halt(1) end, case router_suite_linter:run() of ok -> halt(0); {error, Issues} -> io:format("router_suite_linter failed (~p issues)~n", [length(Issues)]), halt(1) end.'

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
for suite in "${FAST_TEST_SUITES[@]}"; do
    SUITES_ARG="${SUITES_ARG} --suite test/${suite}"
done

# Run Common Test with fast test suites
echo -e "${GREEN}Running fast tests...${NC}"
echo ""

if [[ "${VERBOSE}" == "true" ]]; then
    rebar3 ct ${SUITES_ARG} --verbose
else
    rebar3 ct ${SUITES_ARG}
fi

EXIT_CODE=$?

if [[ ${EXIT_CODE} -eq 0 ]]; then
    echo ""
    echo -e "${GREEN}✓ All fast tests passed${NC}"
else
    echo ""
    echo -e "${RED}✗ Some fast tests failed (exit code: ${EXIT_CODE})${NC}"
fi

exit ${EXIT_CODE}
