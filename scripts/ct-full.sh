#!/usr/bin/env bash
# ct-full.sh - Run full tier tests (non-heavy suites)
#
# Runs all unit and integration tests excluding:
# - soak tests (*_soak_SUITE)
# - stress tests (*_stress_SUITE)
# - chaos tests (*_chaos_SUITE)
# - load tests (*_load_SUITE)
# - heavy tests (*_heavy_SUITE)
#
# Usage:
#   ./scripts/ct-full.sh              # Run all full tier tests
#   ./scripts/ct-full.sh --profile    # Run with per-suite timing
#   ./scripts/ct-full.sh --list       # List suites that would be run
#
# Exit codes:
#   0  - All tests passed
#   1  - Test execution failed
#   2  - Compilation failed
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_DIR"

# Use local rebar3 built for OTP 27
export PATH="$(pwd)/bin:$PATH"

export ROUTER_TEST_LEVEL=full

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Full Tier Test Runner ===${NC}"
echo "Test level: $ROUTER_TEST_LEVEL"
echo ""

# Full tier suites (unit + integration, but not heavy/soak/stress/chaos)
# NOTE: Suite names must match files in test/ directory exactly
FULL_SUITES=(
    # === Core/Basic Tests ===
    router_core_basic_SUITE
    router_e2e_smoke_SUITE
    router_config_validator_SUITE
    router_compliance_SUITE
    router_test_structure_SUITE
    router_error_SUITE
    router_decider_SUITE
    router_assignment_SUITE
    
    # === Policy Tests ===
    router_policy_SUITE
    router_policy_validator_SUITE
    router_policy_dsl_parsing_SUITE
    router_policy_store_SUITE
    
    # === Circuit Breaker Tests (split suites) ===
    router_circuit_breaker_smoke_SUITE
    router_circuit_breaker_integration_SUITE
    router_circuit_breaker_transitions_SUITE
    router_circuit_breaker_recovery_SUITE
    router_circuit_breaker_invariants_SUITE
    router_circuit_breaker_open_SUITE
    # Excluded: router_circuit_breaker_prop_SUITE (property tests)
    
    # === RBAC/Security Tests ===
    router_rbac_SUITE
    router_tenant_allowlist_SUITE
    
    # === Admin Tests ===
    router_admin_cp_status_SUITE
    router_admin_self_check_SUITE
    router_admin_grpc_integration_SUITE
    router_admin_grpc_concurrency_SUITE
    router_admin_grpc_rbac_SUITE
    
    # === NATS Tests ===
    router_nats_contract_validation_SUITE
    router_nats_publish_fail_open_SUITE
    router_nats_publish_queueing_SUITE
    router_nats_publish_metrics_SUITE
    router_nats_publish_retry_SUITE
    
    # === Consumer Tests (split suites) ===
    router_decide_consumer_core_SUITE
    router_decide_consumer_faults_SUITE
    # Excluded: router_decide_consumer_heavy_SUITE (heavy)
    router_result_consumer_core_SUITE
    router_result_consumer_faults_SUITE
    
    # === CAF/Adapter Tests ===
    router_caf_adapter_core_SUITE
    router_caf_adapter_unit_SUITE
    router_caf_adapter_SUITE
    router_caf_adapter_advanced_SUITE
    router_caf_integration_SUITE
    # Excluded: router_caf_adapter_load_thresholds_SUITE (load)
    
    # === Idempotency Tests ===
    router_idem_core_SUITE
    router_idempotency_core_SUITE
    # Excluded: router_idem_advanced_SUITE (advanced/heavy)
    
    # === Metrics/Telemetry Tests ===
    router_metrics_format_SUITE
    router_metrics_labels_unit_SUITE
    router_metrics_contract_SUITE
    router_metrics_labels_integration_SUITE
    router_core_telemetry_contract_SUITE
    # Excluded: router_metrics_labels_performance_SUITE (performance)
    
    # === ETS/Store Tests ===
    router_sticky_store_SUITE
    router_ets_guard_SUITE
    
    # === Delivery Count Tests ===
    router_delivery_count_core_SUITE
    
    # === Alerts/Dashboard Tests ===
    router_alerts_test_SUITE
    router_dashboard_test_SUITE
    
    # === CI/Enforcement Tests ===
    router_ci_enforcement_SUITE
    
    # === Abuse Prevention ===
    router_abuse_SUITE
    
    # === Error Status ===
    router_error_status_SUITE
    router_errors_mapping_SUITE
)

# Suites explicitly excluded from full tier (per TEST_GOVERNANCE.md)
EXCLUDED_SUITES=(
    # Heavy/Long runtime
    router_decide_consumer_heavy_SUITE
    router_idem_advanced_SUITE
    router_concurrent_faults_soak_SUITE
    router_stress_soak_SUITE
    router_jetstream_soak_SUITE
    router_extensions_chaos_SUITE
    router_concurrent_faults_stress_SUITE
    router_chaos_engineering_SUITE
    # Load tests
    router_cb_load_concurrent_SUITE
    router_cb_load_spike_SUITE
    router_caf_adapter_load_thresholds_SUITE
    router_extensions_pipeline_load_SUITE
    router_metrics_labels_performance_SUITE
    router_performance_benchmark_SUITE
    router_resilience_benchmark_SUITE
    # Property-based tests
    router_decider_prop_SUITE
    router_circuit_breaker_prop_SUITE
)

show_help() {
    cat <<EOF
Full Tier Test Runner

USAGE:
  $0 [OPTIONS]

OPTIONS:
  --profile    Run tests with per-suite timing
  --list       List suites that would be run (dry run)
  --help       Show this help message

SUITES INCLUDED: ${#FULL_SUITES[@]}
SUITES EXCLUDED: ${#EXCLUDED_SUITES[@]} (heavy/soak/stress/chaos/load/prop)

TARGET RUNTIME: < 15 minutes
EOF
}

list_suites() {
    echo -e "${GREEN}Suites to be run (${#FULL_SUITES[@]}):${NC}"
    for suite in "${FULL_SUITES[@]}"; do
        echo "  ✓ ${suite}"
    done
    echo ""
    echo -e "${YELLOW}Excluded suites (${#EXCLUDED_SUITES[@]}):${NC}"
    for suite in "${EXCLUDED_SUITES[@]}"; do
        echo "  ✗ ${suite}"
    done
}

# Parse arguments
if [[ "${1:-}" == "--help" ]]; then
    show_help
    exit 0
fi

if [[ "${1:-}" == "--list" ]]; then
    list_suites
    exit 0
fi

# Static suite linter before running tests (MANDATORY - quality gate)
echo -e "${BLUE}Running suite linter (quality gate)...${NC}"
if ! erl -noshell -pa test -pa test_support -eval 'case compile:file("test_support/router_suite_linter", [report, {outdir, "test_support"}]) of {ok, _} -> ok; Error -> io:format("router_suite_linter compile failed: ~p~n", [Error]), halt(1) end, case router_suite_linter:run() of ok -> halt(0); {error, Issues} -> io:format("router_suite_linter failed (~p issues)~n", [length(Issues)]), halt(1) end.' 2>&1; then
    echo -e "${RED}✗ Suite linter failed - quality gate blocked${NC}"
    echo "  Fix linter issues before running full tier tests."
    echo "  Run: erl -noshell -pa test_support -s router_suite_linter run -s init stop"
    exit 1
fi
echo -e "${GREEN}✓ Suite linter passed${NC}"

# Compile first
echo -e "${BLUE}Compiling project...${NC}"
if ! rebar3 compile 2>&1; then
    echo -e "${RED}Compilation failed${NC}"
    exit 2
fi

# Profile mode - time each suite
if [[ "${1:-}" == "--profile" ]]; then
    echo -e "${BLUE}=== Full tier profiling ===${NC}"
    echo ""
    
    TOTAL_START=$(date +%s)
    declare -A SUITE_TIMES
    PASSED=0
    FAILED=0
    SKIPPED=0
    FAILED_SUITES=()
    
    for suite in "${FULL_SUITES[@]}"; do
        # Check if suite file exists
        if [[ ! -f "test/${suite}.erl" ]]; then
            echo -e "Testing $suite... ${YELLOW}SKIPPED (file not found)${NC}"
            ((SKIPPED++))
            continue
        fi
        
        SUITE_START=$(date +%s)
        echo -n "Testing $suite... "
        if rebar3 ct --suite "test/${suite}" 2>&1 | tail -3; then
            SUITE_END=$(date +%s)
            SUITE_TIME=$((SUITE_END - SUITE_START))
            SUITE_TIMES[$suite]=$SUITE_TIME
            echo -e "  ${GREEN}PASSED${NC} (${SUITE_TIME}s)"
            ((PASSED++))
        else
            echo -e "  ${RED}FAILED${NC}"
            ((FAILED++))
            FAILED_SUITES+=("$suite")
        fi
    done
    
    TOTAL_END=$(date +%s)
    TOTAL_TIME=$((TOTAL_END - TOTAL_START))
    
    echo ""
    echo -e "${BLUE}=== Timing Summary ===${NC}"
    for suite in "${!SUITE_TIMES[@]}"; do
        printf "  %-50s %3ds\n" "$suite" "${SUITE_TIMES[$suite]}"
    done | sort -t$'\t' -k2 -rn
    
    echo ""
    echo -e "${BLUE}=== Results Summary ===${NC}"
    echo -e "  Passed:  ${GREEN}${PASSED}${NC}"
    echo -e "  Failed:  ${RED}${FAILED}${NC}"
    echo -e "  Skipped: ${YELLOW}${SKIPPED}${NC}"
    echo "  Total time: ${TOTAL_TIME}s (target: < 900s)"
    
    if [[ ${#FAILED_SUITES[@]} -gt 0 ]]; then
        echo ""
        echo -e "${RED}Failed suites:${NC}"
        for suite in "${FAILED_SUITES[@]}"; do
            echo "  - $suite"
        done
        exit 1
    fi
else
    # Normal mode - filter to existing files first
    EXISTING_SUITES=()
    MISSING_SUITES=()
    for suite in "${FULL_SUITES[@]}"; do
        if [[ -f "test/${suite}.erl" ]]; then
            EXISTING_SUITES+=("$suite")
        else
            MISSING_SUITES+=("$suite")
        fi
    done
    
    if [[ ${#MISSING_SUITES[@]} -gt 0 ]]; then
        echo -e "${YELLOW}Note: ${#MISSING_SUITES[@]} suites not found (skipped)${NC}"
    fi
    
    echo -e "${BLUE}Running ${#EXISTING_SUITES[@]} suites...${NC}"
    echo ""
    
    # Build suite list
    SUITE_LIST=$(IFS=,; echo "${EXISTING_SUITES[*]}" | sed 's/,/,test\//g' | sed 's/^/test\//')

    # Record start time for baseline
    START_TIME=$(date +%s)

    if time rebar3 ct --suite "$SUITE_LIST"; then
        echo ""
        echo -e "${GREEN}✓ All full tier tests executed${NC}"
        
        # Run quality gates check
        echo ""
        echo -e "${BLUE}Running quality gates...${NC}"
        if "$SCRIPT_DIR/ci_full_quality_gates.sh"; then
            echo ""
            echo -e "${GREEN}✓ All quality gates passed - Full tier complete${NC}"
            
            # Generate baseline metrics
            END_TIME=$(date +%s)
            DURATION=$((END_TIME - START_TIME))
            "$SCRIPT_DIR/generate_baseline.sh" --parse-ct "$DURATION" "${#EXISTING_SUITES[@]}" 0
        else
            echo ""
            echo -e "${RED}✗ Quality gates failed${NC}"
            
            # Still generate baseline with failure status
            END_TIME=$(date +%s)
            DURATION=$((END_TIME - START_TIME))
            "$SCRIPT_DIR/generate_baseline.sh" --parse-ct "$DURATION" "${#EXISTING_SUITES[@]}" 1
            exit 1
        fi
    else
        echo ""
        echo -e "${RED}✗ Some full tier tests failed${NC}"
        # Still run quality gates to get detailed report
        "$SCRIPT_DIR/ci_full_quality_gates.sh" || true
        
        # Generate baseline with failure status
        END_TIME=$(date +%s)
        DURATION=$((END_TIME - START_TIME))
        "$SCRIPT_DIR/generate_baseline.sh" --parse-ct "$DURATION" "${#EXISTING_SUITES[@]}" 1
        exit 1
    fi
fi
