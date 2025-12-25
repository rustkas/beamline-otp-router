## Fix items
1. **Normalize the binary path for `router_nats_server:which/1`**
   - Hypothesis: the heavy-tier log shows `string:trim` crashing because a newline slipped into the binary path, so `which/1` needs to trim or tolerate trailing whitespace before constructing server args.
   - Target files/functions: `src/router_nats_server.erl`, `which/1` (and any helper that opens the nats binary path).
   - Verification: `rebar3 ct -s router_decide_consumer_unit_SUITE`.

2. **Reconcile `router_nats:publish_with_ack` arity**
   - Hypothesis: some chaos suites expect a `/2` helper while the module currently only exports `/3`, so we either add the missing /2 wrapper or adapt the callers to supply the extra context.
   - Target files/functions: `src/router_nats.erl` (export list, `publish_with_ack/2`/`/3`, `start_link/0` for cascades); review `test/router_ext_chaos_failure_SUITE.erl` and `test/router_ext_chaos_recovery_SUITE.erl` for call sites.
   - Verification: `rebar3 ct -s router_ext_chaos_failure_SUITE router_ext_chaos_recovery_SUITE` (can be two separate commands or a combined CT run that includes both suites).

3. **Stabilize the weighted distribution logic used by `router_decider_prop_SUITE` and load-related suites**
   - Hypothesis: the property asserts against a deterministic weighted distribution, but `router_decider:execute_provider_selection/5` sometimes returns `badmap ~"ok"` or weights that violate expectations, suggesting the provider selection or test fixture is misaligned.
   - Target files/functions: `src/router_decider.erl` (`execute_provider_selection/5`, any helper that interprets provider metadata) plus `test/router_decider_prop_SUITE.erl` for fixture expectations. Mention `router_ext_load_advanced_SUITE` while the fix addresses the same provider selection path.
   - Verification: `rebar3 ct -s router_decider_prop_SUITE router_ext_load_advanced_SUITE`

4. **Restore the group catalog referenced by delivery and gateway suites**
   - Hypothesis: both `router_delivery_count_advanced_SUITE` and `router_gateway_contract_valid_SUITE` panic with “Invalid reference to group … in …:all/0”, so the group definition data is missing or the `router_groups:all/0` API no longer matches the suite's expectations.
   - Target files/functions: `src/router_groups.erl`, any module that assembles the group list for those suites, and the suite fixtures themselves if they hard-code group names.
   - Verification: `rebar3 ct -s router_delivery_count_advanced_SUITE router_gateway_contract_valid_SUITE`

5. **Address the real-time assertion in `router_duration_unit_SUITE`**
   - Hypothesis: the suite asserts `Result < 50000` on a real-time measurement, which fails when the host is slower or when monotonic time is used; switching to monotonic measurement or raising the threshold should satisfy the check.
   - Target files/functions: `test/router_duration_unit_SUITE.erl`, any helper under `src/` that reports duration (likely `router_duration.erl`).
   - Verification: `rebar3 ct -s router_duration_unit_SUITE`

6. **Make ETS cleanup safe for baseline load suites**
   - Hypothesis: `router_ext_load_baseline_SUITE` crashes because `ets:delete_all_objects/1` receives an invalid table reference, perhaps due to `router_nats:start_link/0` never initializing before cleanup; ensuring the ETS table exists or guarding the delete call will prevent the badarg.
   - Target files/functions: `src/router_nats.erl` (start_link/setup that owns ETS tables) and any helper called from `test/router_ext_load_baseline_SUITE.erl` that purges state.
   - Verification: `rebar3 ct -s router_ext_load_baseline_SUITE`
