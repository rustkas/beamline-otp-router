#!/usr/bin/env bash
# Simulate latency outage for R10 incident simulation
# Usage: ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret scripts/r10_simulate_latency_outage.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=./r10_fault_common.sh
source "${SCRIPT_DIR}/r10_fault_common.sh"

DELAY_MS="${DELAY_MS:-6000}"

echo "Enabling latency fault injection for publish with delay=${DELAY_MS}ms"
echo "This will cause circuit breaker to open via latency_threshold_exceeded trigger"
echo ""

erl_rpc "router_nats_fault_injection" "enable_fault" "[publish, {delay, ${DELAY_MS}}]"

echo ""
echo "✅ Fault injection enabled"
echo ""
echo "To clear fault later, run:"
echo "  ROUTER_NODE=${ROUTER_NODE} ROUTER_COOKIE=${ROUTER_COOKIE} scripts/r10_clear_faults.sh"
echo ""
echo "To check circuit breaker status:"
echo "  ./router_ctl r10 status <tenant> <provider>"

