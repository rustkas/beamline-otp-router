#!/usr/bin/env bash
# Clear all R10 fault injections
# Usage: ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret scripts/r10_clear_faults.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# shellcheck source=./r10_fault_common.sh
source "${SCRIPT_DIR}/r10_fault_common.sh"

echo "Clearing all router_nats fault injections"
echo ""

erl_rpc "router_nats_fault_injection" "clear_all_faults" "[]"

echo ""
echo "✅ All fault injections cleared"
echo ""
echo "Circuit breaker should recover: open → half_open → closed"
echo ""
echo "To verify recovery:"
echo "  ./router_ctl r10 status <tenant> <provider>"

