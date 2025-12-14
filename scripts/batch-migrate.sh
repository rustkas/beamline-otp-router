#!/usr/bin/env bash
# Batch migrate suites to groups_for_level pattern
# Usage: ./scripts/batch-migrate.sh

set -euo pipefail
cd "$(dirname "$0")/.."

# Heavy-only pattern (for load/performance/chaos/stress suites)
HEAVY_PATTERN='
all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

%% Heavy-only suite - only runs in heavy tier
groups_for_level(heavy) ->
    [{group, heavy_tests}];
groups_for_level(_) -> %% fast, full, sanity
    [].
'

# Integration pattern (for e2e/integration suites)
INTEGRATION_PATTERN='
all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

%% Integration suite - runs in full and heavy tiers
groups_for_level(heavy) ->
    [{group, integration_tests}];
groups_for_level(full) ->
    [{group, integration_tests}];
groups_for_level(_) -> %% fast, sanity
    [].
'

# Unit/Fast pattern (for unit/smoke suites)
UNIT_PATTERN='
all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

%% Unit tests run in all tiers
groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast, sanity
    [{group, unit_tests}].
'

echo "=== Heavy-only suites (load/performance/chaos/stress) ==="
for suite in router_extensions_pipeline_load router_intake_overload router_metrics_labels_performance \
             router_observability_performance router_performance_load router_performance_regression \
             router_policy_applier_load router_extensions_chaos; do
    if [ -f "test/${suite}_SUITE.erl" ]; then
        if ! grep -q "groups_for_level" "test/${suite}_SUITE.erl"; then
            echo "  Need migration: ${suite}_SUITE"
        fi
    fi
done

echo ""
echo "=== Integration suites (e2e/integration) ==="
for suite in router_extensions_e2e router_gateway_integration router_headers_propagation_e2e \
             router_health_integration router_jetstream_e2e router_jetstream_extended_recovery \
             router_metrics_labels_integration; do
    if [ -f "test/${suite}_SUITE.erl" ]; then
        if ! grep -q "groups_for_level" "test/${suite}_SUITE.erl"; then
            echo "  Need migration: ${suite}_SUITE"
        fi
    fi
done

echo ""
echo "=== Unit/Fast suites ==="
for suite in router_metrics_labels_unit router_ets_guard router_error_status router_errors_mapping \
             router_normalize_boolean_prop router_gateway_contract_smoke; do
    if [ -f "test/${suite}_SUITE.erl" ]; then
        if ! grep -q "groups_for_level" "test/${suite}_SUITE.erl"; then
            echo "  Need migration: ${suite}_SUITE"
        fi
    fi
done

echo ""
echo "=== Remaining uncategorized ==="
grep -L "groups_for_level" test/*_SUITE.erl 2>/dev/null | while read file; do
    name=$(basename "$file" .erl)
    case "$name" in
        *_load_*|*_performance_*|*_stress_*|*_chaos_*|*_soak_*) ;;
        *_e2e_*|*_integration_*) ;;
        *_unit_*|*_smoke_*|*_contract_*) ;;
        *) echo "  $name" ;;
    esac
done | head -20
