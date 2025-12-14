#!/usr/bin/env bash
# Run SANITY tests (Critical Path)
# Usage: ./scripts/ct-sanity.sh

set -euo pipefail
cd "$(dirname "$0")/.."

# Use local rebar3 built for OTP 27
export PATH="$(pwd)/bin:$PATH"

export ROUTER_TEST_LEVEL=sanity

# Static lint of suites before running CT
erl -noshell -pa test -pa test_support -eval 'case compile:file("test_support/router_suite_linter", [report, {outdir, "test_support"}]) of {ok, _} -> ok; Error -> io:format("router_suite_linter compile failed: ~p~n", [Error]), halt(1) end, case router_suite_linter:run() of ok -> halt(0); {error, Issues} -> io:format("router_suite_linter failed (~p issues)~n", [length(Issues)]), halt(1) end.'

# Run only critical suites (core functionality sanity check)
# Target: <15 seconds total execution
rebar3 ct --suite test/router_core_basic_SUITE,test/router_e2e_smoke_SUITE,test/router_policy_SUITE,test/router_circuit_breaker_smoke_SUITE
