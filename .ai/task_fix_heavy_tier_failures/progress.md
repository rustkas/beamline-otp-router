## Status: CLOSED
Reason: All acceptance commands executed with non‑zero tests and clean PASS evidence.

## Failure inventory (initial state)
- `router_decide_consumer_unit_SUITE`: `router_nats_server:which/1` crashes with `string:trim` due to a newline in the binary path.
- `router_ext_chaos_failure_SUITE`/`router_ext_chaos_recovery_SUITE`: undefined `router_nats:publish_with_ack/2` while `publish_with_ack/3` is exported.
- `router_decider_prop_SUITE`: weighted distribution assertion fails; possibly provider selection yields `badmap ~"ok"`.
- `router_delivery_count_advanced_SUITE` & `router_gateway_contract_valid_SUITE`: error “Invalid reference to group … in …:all/0”.
- `router_duration_unit_SUITE`: real-time assertion `Result < 50000` does not hold on the observed host.
- `router_ext_load_advanced_SUITE`/`router_ext_load_baseline_SUITE`: `badmap ~"ok"` in provider selection and `badarg` in `ets:delete_all_objects/1` plus `router_nats:start_link/0` unspecified, causing cascading failures.

## Verification runs
 - `rebar3 ct --suite router_nats_server_unit_SUITE` (passes; verifies `normalize_command_output/1` trims stray whitespace).
 - `rebar3 ct --suite router_duration_unit_SUITE` (passes).
 - `rebar3 ct --suite router_decider_prop_SUITE` (reports “All 0 tests passed”; property tests skip when PropEr is unavailable locally).

## Verification runs (latest)
 - Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_decide_consumer_unit_SUITE -v`
   Tier: heavy
   Executed: 18
   Result: PASS
   Evidence: `TEST COMPLETE, 18 ok, 0 failed`
 - Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_delivery_count_advanced_SUITE -v`
   Tier: heavy
   Executed: 4
   Result: PASS
   Evidence: `TEST COMPLETE, 4 ok, 0 failed`
 - Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_gateway_contract_valid_SUITE -v`
   Tier: heavy
   Executed: 4
   Result: PASS
   Evidence: `TEST COMPLETE, 4 ok, 0 failed`
 - Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_ext_load_baseline_SUITE -v`
   Tier: heavy
   Executed: 4
   Result: PASS
   Evidence: `TEST COMPLETE, 4 ok, 0 failed`
 - Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_ext_chaos_failure_SUITE -v`
   Tier: heavy
   Executed: 3
   Result: PASS
   Evidence: `TEST COMPLETE, 3 ok, 0 failed`
- Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_ext_chaos_recovery_SUITE -v`
  Tier: heavy
  Executed: 2
  Result: PASS
  Evidence: `TEST COMPLETE, 2 ok, 0 failed`
- Command: `ROUTER_TEST_LEVEL=full rebar3 as test ct --suite router_decider_unit_SUITE -v`
  Tier: full
  Executed: 15
  Result: PASS
  Evidence: `TEST COMPLETE, 15 ok, 0 failed`
 - Command: `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_decider_prop_SUITE -v`
   Tier: heavy
   Executed: 3
   Result: PASS
   Evidence: `TEST COMPLETE, 3 ok, 0 failed`

### Hypotheses / Next actions
- Fix invalid group references in delivery/gateway suites; verify `groups/0` alignment.
- Guard ETS cleanup in `router_ext_load_baseline_SUITE:init_per_testcase` to prevent `badarg`.
- Confirm chaos suites use correct `router_nats:publish_with_ack` arity.

## Verification runs (2025-12-26)

- Command: `rebar3 ct --suite apps/beamline_router/test/router_cp2_conditions_SUITE,apps/beamline_router/test/router_cp2_dry_run_SUITE,apps/beamline_router/test/router_cp2_dag_SUITE,apps/beamline_router/test/router_extension_invoker_telemetry_SUITE`
  Tier: default
  Executed: 10
  Result: PASS
  CT logs: `/home/rustkas/aigroup/apps/otp/router/_build/test/logs/index.html`

- Command: `ROUTER_ENABLE_META=1 ROUTER_TEST_LEVEL=heavy rebar3 ct --suite apps/beamline_router/test/router_cp2_conditions_SUITE,apps/beamline_router/test/router_cp2_dry_run_SUITE,apps/beamline_router/test/router_cp2_dag_SUITE,apps/beamline_router/test/router_extension_invoker_telemetry_SUITE`
  Tier: heavy
  Executed: 18
  Result: PASS
  CT logs: `/home/rustkas/aigroup/apps/otp/router/_build/test/logs/index.html`

- Command: `ROUTER_ENABLE_META=1 ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite apps/beamline_router/test/router_cp2_conditions_SUITE,apps/beamline_router/test/router_cp2_dry_run_SUITE,apps/beamline_router/test/router_cp2_dag_SUITE,apps/beamline_router/test/router_extension_invoker_telemetry_SUITE`
  Tier: heavy (profile test)
  Executed: 18
  Result: PASS
  CT logs: `/home/rustkas/aigroup/apps/otp/router/_build/test/logs/index.html`

### Git evidence
- HEAD: `39c271827f8b8380fb550643f6edd5db8f38b89a`
- Diffstat (working tree vs HEAD, targeted):
  - `apps/beamline_router/test/router_extension_invoker_telemetry_SUITE.erl | 1 insertion(+), 1 deletion(-)`
- Changed files (targeted):
  - `apps/beamline_router/test/router_extension_invoker_telemetry_SUITE.erl`
