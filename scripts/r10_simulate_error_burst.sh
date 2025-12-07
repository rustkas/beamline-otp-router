#!/usr/bin/env bash
# Simulate error burst for R10 incident simulation
# Usage: ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret scripts/r10_simulate_error_burst.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=./r10_fault_common.sh
source "${SCRIPT_DIR}/r10_fault_common.sh"

ERROR_TERM="${ERROR_TERM:-nats_unavailable}"

echo "Enabling error fault injection for publish with error=${ERROR_TERM}"
echo "This will cause circuit breaker to open via failure_threshold_exceeded or error_rate_threshold_exceeded trigger"
echo ""

erl_rpc "router_nats_fault_injection" "enable_fault" "[publish, {error, ${ERROR_TERM}}]"

echo ""
echo "✅ Fault injection enabled"
echo ""
echo "To clear fault later, run:"
echo "  ROUTER_NODE=${ROUTER_NODE} ROUTER_COOKIE=${ROUTER_COOKIE} scripts/r10_clear_faults.sh"
echo ""
echo "To check circuit breaker status:"
echo "  ./router_ctl r10 status <tenant> <provider>"

