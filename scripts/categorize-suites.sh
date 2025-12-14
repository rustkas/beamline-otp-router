#!/usr/bin/env bash
# Batch migration helper for test suites
# Categorizes suites by domain and tier recommendation

set -euo pipefail
cd "$(dirname "$0")/.."

echo "=== Suite Categorization for Migration ==="
echo ""

# Categories based on suite name patterns
declare -A CATEGORIES=(
    ["metrics"]="router_metrics"
    ["nats"]="router_nats|router_jetstream"
    ["extensions"]="router_extension"
    ["performance"]="router_performance|_load_|_benchmark_"
    ["observability"]="router_observability|router_telemetry"
    ["policy"]="router_policy"
    ["gateway"]="router_gateway|router_intake"
    ["grpc"]="router_grpc"
    ["error"]="router_error"
    ["health"]="router_health|router_deployment"
)

# Find suites missing groups_for_level
MISSING=$(grep -L "groups_for_level" test/*_SUITE.erl 2>/dev/null | xargs -I{} basename {} .erl || true)

echo "=== Heavy-only suites (stress/soak/performance/chaos) ==="
for suite in $MISSING; do
    if echo "$suite" | grep -qE "performance|stress|soak|chaos|load|benchmark"; then
        echo "  $suite -> heavy_tests only"
    fi
done

echo ""
echo "=== Integration suites (full tier) ==="
for suite in $MISSING; do
    if echo "$suite" | grep -qE "integration|e2e|extended"; then
        echo "  $suite -> integration_tests (full)"
    fi
done

echo ""
echo "=== Unit/Fast suites ==="
for suite in $MISSING; do
    if echo "$suite" | grep -qE "unit|smoke|contract|mapping|guard|prop"; then
        echo "  $suite -> unit_tests (fast)"
    fi
done

echo ""
echo "=== Uncategorized (needs manual review) ==="
for suite in $MISSING; do
    if ! echo "$suite" | grep -qE "performance|stress|soak|chaos|load|benchmark|integration|e2e|extended|unit|smoke|contract|mapping|guard|prop"; then
        echo "  $suite"
    fi
done

echo ""
echo "Total missing groups_for_level: $(echo "$MISSING" | wc -w)"
