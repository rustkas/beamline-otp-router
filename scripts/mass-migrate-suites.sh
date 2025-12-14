#!/usr/bin/env bash
# mass-migrate-suites.sh - Automatically migrate suites to groups_for_level pattern
# Usage: ./scripts/mass-migrate-suites.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_DIR"

# Categories for suites
declare -A SUITE_TIER

# Heavy-only (chaos, load, stress, performance, soak, extended)
SUITE_TIER[router_extensions_chaos_SUITE]="heavy"
SUITE_TIER[router_intake_overload_SUITE]="heavy"
SUITE_TIER[router_performance_regression_SUITE]="heavy"
SUITE_TIER[router_policy_applier_load_SUITE]="heavy"
SUITE_TIER[router_jetstream_extended_recovery_SUITE]="heavy"

# Integration (e2e, integration)
SUITE_TIER[router_extensions_e2e_SUITE]="integration"
SUITE_TIER[router_gateway_integration_SUITE]="integration"
SUITE_TIER[router_headers_propagation_e2e_SUITE]="integration"
SUITE_TIER[router_health_integration_SUITE]="integration"
SUITE_TIER[router_jetstream_e2e_SUITE]="integration"
SUITE_TIER[router_metrics_labels_integration_SUITE]="integration"

# Unit/Fast (remaining)
# Default is unit

migrate_suite() {
    local suite_name=$1
    local tier=${SUITE_TIER[$suite_name]:-unit}
    local file="test/${suite_name}.erl"
    
    if [[ ! -f "$file" ]]; then
        echo "SKIP: $file not found"
        return
    fi
    
    if grep -q "groups_for_level" "$file"; then
        echo "SKIP: $suite_name already migrated"
        return
    fi
    
    echo "Migrating $suite_name as $tier tier..."
    
    # Generate the pattern based on tier
    local pattern_file
    case $tier in
        heavy)
            cat > /tmp/migration_pattern.erl << 'PATTERN_HEAVY'
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Heavy-only suite
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [];
groups_for_level(heavy) -> [{group, heavy_tests}].

PATTERN_HEAVY
            ;;
        integration)
            cat > /tmp/migration_pattern.erl << 'PATTERN_INT'
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Integration suite - runs in full tier
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [{group, integration_tests}];
groups_for_level(heavy) -> [{group, integration_tests}].

PATTERN_INT
            ;;
        *)
            cat > /tmp/migration_pattern.erl << 'PATTERN_UNIT'
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Unit tests run in fast tier
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, unit_tests}];
groups_for_level(full) -> [{group, unit_tests}];
groups_for_level(heavy) -> [{group, unit_tests}].

PATTERN_UNIT
            ;;
    esac
    
    echo "  Pattern generated for $tier"
}

# List of suites to migrate
SUITES=(
    router_cp1_minimal_mode_SUITE
    router_delivery_count_tracking_SUITE
    router_deployment_SUITE
    router_error_status_SUITE
    router_ets_consistency_prop_SUITE
    router_ets_guard_SUITE
    router_extension_invoker_telemetry_SUITE
    router_extension_registry_dual_mode_SUITE
    router_extensions_e2e_SUITE
    router_extensions_security_SUITE
    router_gateway_contract_smoke_SUITE
    router_gateway_integration_SUITE
    router_grpc_compatibility_SUITE
    router_headers_propagation_e2e_SUITE
    router_health_integration_SUITE
    router_idempotency_SUITE
    router_intake_error_codes_SUITE
    router_intake_error_handler_SUITE
    router_jetstream_e2e_SUITE
    router_jetstream_extended_recovery_SUITE
    router_jetstream_redelivery_metrics_SUITE
    router_jetstream_redelivery_runtime_SUITE
    router_metrics_dump_SUITE
    router_metrics_http_SUITE
    router_metrics_labels_integration_SUITE
    router_metrics_r10_SUITE
    router_nats_compatibility_SUITE
    router_nats_connection_failure_SUITE
    router_nats_publish_retry_SUITE
    router_nats_subscriber_caf_SUITE
    router_normalize_boolean_prop_SUITE
    router_observability_otel_spans_SUITE
    router_observability_SUITE
    router_policy_enforcement_SUITE
)

echo "=== Mass Suite Migration ==="
echo "Total suites to check: ${#SUITES[@]}"
echo ""

for suite in "${SUITES[@]}"; do
    migrate_suite "$suite"
done

echo ""
echo "=== Migration Summary ==="
./scripts/audit-suite-structure.sh 2>/dev/null | tail -10
