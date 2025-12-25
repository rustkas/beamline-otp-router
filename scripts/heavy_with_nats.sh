#!/usr/bin/env bash
set -euo pipefail

root_dir() {
  if command -v git >/dev/null 2>&1 && git rev-parse --show-toplevel >/dev/null 2>&1; then
    git rev-parse --show-toplevel
  else
    cd "$(dirname "$0")/.." && pwd
  fi
}

main() {
  local root
  root="$(root_dir)"
  cd "$root"

  mkdir -p _artifacts

  local suite="${1:-test/router_gateway_integration_SUITE.erl}"
  local ts
  ts="$(date +%Y%m%d_%H%M%S)"
  local ct_log="_artifacts/ct_heavy_with_nats_${ts}.log"

  trap 'echo "[TRAP] stopping nats"; ./scripts/nats_stop.sh || true' EXIT

  ./scripts/nats_start.sh
  ./scripts/nats_status.sh || true

  echo "[CT] running heavy suite: $suite"
  ROUTER_TEST_LEVEL=heavy \
    NATS_URL="${NATS_URL:-nats://127.0.0.1:4222}" \
    rebar3 as test ct --suite "$suite" 2>&1 | tee "$ct_log"

  echo "[OK] CT done. log: $ct_log"
}

main "$@"
