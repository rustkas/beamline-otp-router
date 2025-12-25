#!/usr/bin/env bash
set -euo pipefail

export ROUTER_TEST_LEVEL="sanity"

cd "$(dirname "$0")/.."

echo "[SMOKE] Running backpressure-related sanity suites"

rebar3 ct --suite=router_intake_overload_SUITE --retry
rebar3 ct --suite=router_gateway_integration_SUITE --retry
rebar3 ct --suite=router_caf_integration_SUITE --retry

echo "[SMOKE] Done"
