#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." >/dev/null && pwd)"
VIOLATION_FILE="$ROOT/test/router_mock_discipline_violation_SUITE.erl"
trap 'rm -f "$VIOLATION_FILE"' EXIT

cat <<'EOF' > "$VIOLATION_FILE"
%% Minimal suite that violates the mock discipline rule
-module(router_mock_discipline_violation_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    end_per_suite/1
]).

all() -> [].

init_per_suite(Config) ->
    _ = meck:new(router_nats, [passthrough]),
    _ = gen_server:call(router_nats, {ping, violation}),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

end_per_suite(Config) ->
    Config.
EOF

cd "$ROOT"

set +e
scripts/lint/check_router_mock_discipline.sh
STATUS=$?
set -e

if [ "$STATUS" -eq 0 ]; then
    echo "mock discipline check unexpectedly succeeded despite the intentional violation" >&2
    exit 1
else
    echo "mock discipline check failed as expected (violation detected)"
fi
