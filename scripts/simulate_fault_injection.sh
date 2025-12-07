#!/bin/bash
# @doc Simulate fault injection scenarios for staging validation
# 
# Simulates:
# - Tenant validation failures
# - Backpressure conditions
# - ACK/NAK errors
# 
# Usage:
#   ./scripts/simulate_fault_injection.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

echo "=== Simulating Fault Injection Scenarios ==="
echo ""

# Scenario 1: Tenant Validation Failures
echo "Scenario 1: Tenant Validation Failures"
rebar3 shell --eval "
ok = router_metrics:ensure(),
lists:foreach(fun(I) ->
    AssignmentId = list_to_binary(\"assign-tenant-fail-\" ++ integer_to_list(I)),
    RequestId = list_to_binary(\"req-tenant-fail-\" ++ integer_to_list(I)),
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        reason => <<\"tenant_validation_failed\">>,
        source => <<\"tenant_validation\">>
    })
end, lists:seq(1, 10)),
io:format(\"✓ Emitted 10 tenant validation failure redeliveries~n\"),
halt().
" 2>&1 | grep -E "(✓|error|Error)" || true

echo ""

# Scenario 2: Backpressure
echo "Scenario 2: Backpressure Conditions"
rebar3 shell --eval "
ok = router_metrics:ensure(),
lists:foreach(fun(I) ->
    RequestId = list_to_binary(\"req-backpressure-\" ++ integer_to_list(I)),
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        request_id => RequestId,
        reason => <<\"backpressure\">>,
        source => <<\"backpressure\">>
    })
end, lists:seq(1, 5)),
io:format(\"✓ Emitted 5 backpressure redeliveries~n\"),
halt().
" 2>&1 | grep -E "(✓|error|Error)" || true

echo ""

# Scenario 3: ACK/NAK Errors
echo "Scenario 3: ACK/NAK Errors"
rebar3 shell --eval "
ok = router_metrics:ensure(),
lists:foreach(fun(I) ->
    AssignmentId = list_to_binary(\"assign-ack-error-\" ++ integer_to_list(I)),
    Reason = case I rem 2 of
        0 -> <<\"ack_error\">>;
        1 -> <<\"nak_error\">>
    end,
    Source = case I rem 2 of
        0 -> <<\"ack_failure\">>;
        1 -> <<\"nak_failure\">>
    end,
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        reason => Reason,
        source => Source
    })
end, lists:seq(1, 8)),
io:format(\"✓ Emitted 8 ACK/NAK error redeliveries~n\"),
halt().
" 2>&1 | grep -E "(✓|error|Error)" || true

echo ""

# Generate metrics dump
echo "Generating metrics dump..."
rebar3 shell --eval "
ok = router_prometheus:dump(\"fault_injection_metrics.prom\"),
io:format(\"✓ Metrics dump created: fault_injection_metrics.prom~n\"),
halt().
" 2>&1 | grep -E "(✓|error|Error)" || true

echo ""
echo "=== Fault Injection Simulation Complete ==="
echo ""
echo "Metrics dump: fault_injection_metrics.prom"
echo ""
echo "Next steps:"
echo "  1. Check metrics dump: cat fault_injection_metrics.prom | grep router_jetstream_redelivery_total"
echo "  2. Verify labels: ./scripts/check_metrics_endpoint.sh file://\$(pwd)/fault_injection_metrics.prom"
echo "  3. Query Prometheus (if available): sum(rate(router_jetstream_redelivery_total[5m]))"

