# Progress (Step updates)

## Step 1 – Suite discovery
- `router_caf_adapter_SUITE.erl`: exists (`test/router_caf_adapter_SUITE.erl`), exports the required CT callbacks.
- `router_caf_adapter_enhanced_SUITE.erl`: exists, restored lifecycle callbacks after fixing export order.
- `router_jetstream_recovery_ext_SUITE.erl`: exists with rich test cases and explicit callbacks (unchanged since stabilization).

## Step 2 – Suite linter
- Command: `erl -noshell -pa test_support -s router_suite_linter run -s init stop` → exit 0; output: `router_suite_linter: ok (245 suites checked, mode=baseline)`.

## Step 3 – Full tier run
- Command: `./scripts/ct-full.sh` → exit 0; summary: 55 suites / 332 tests executed, all passed, quality gates (0-3) green; baseline saved.

## Step 4 – Targeted regression check
- Command: `rebar3 ct --suite test/router_idem_core_SUITE.erl` → exit 0; suite-specific run compiled `beamline_router` and reported “All 0 tests passed.”
