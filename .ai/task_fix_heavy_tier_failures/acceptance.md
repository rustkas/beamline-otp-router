## Acceptance Criteria
1. `router_decide_consumer_unit_SUITE` completes without hitting the `string:trim` failure in `router_nats_server:which/1`; reproduce with `rebar3 ct -s router_decide_consumer_unit_SUITE`.
2. Both chaos suites (`router_ext_chaos_failure_SUITE`, `router_ext_chaos_recovery_SUITE`) run without undefined-function errors and produce the expected publish/ack behavior; reproduce with `rebar3 ct -s router_ext_chaos_failure_SUITE router_ext_chaos_recovery_SUITE`.
3. `router_decider_prop_SUITE` and `router_ext_load_advanced_SUITE` pass consistently, demonstrating stable weighted distribution/provider selection logic; reproduce with `rebar3 ct -s router_decider_prop_SUITE router_ext_load_advanced_SUITE`.
4. `router_delivery_count_advanced_SUITE`, `router_gateway_contract_valid_SUITE`, and `router_ext_load_baseline_SUITE` no longer crash with group/ETS cleanup errors; reproduce with `rebar3 ct -s router_delivery_count_advanced_SUITE router_gateway_contract_valid_SUITE router_ext_load_baseline_SUITE`.
5. `router_duration_unit_SUITE` satisfies the real-time assertion using the host's timing characteristics; reproduce with `rebar3 ct -s router_duration_unit_SUITE`.
6. The new changes do not introduce regressions observable in the default full-tier run (e.g., `rebar3 ct -s router_decide_unit_SUITE` should still pass), keeping heavy-specific fixes isolated.
