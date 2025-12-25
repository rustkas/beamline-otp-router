Fix the regressions exposed by the heavy-tier stress execution of the router app, documenting and triaging each visible failure before touching the implementation so the full-tier behavior stays stable.

- `router_decide_consumer_unit_SUITE`: init_per_suite now bails out in `router_decide_consumer:init/1` with `{error, not_connected}` and noisy telemetry warnings because the suite bootstraps the real NATS stack without mocking or starting telemetry.
- `router_ext_chaos_failure_SUITE` and `router_ext_chaos_recovery_SUITE`: both suites crash/skip when `router_nats:publish_with_ack/2` is invoked but only `publish_with_ack/3` exists in the module.
- `router_decider_prop_SUITE`: the weighted-distribution property assertion fails, suggesting provider selections are skewed or the expected distribution is wrong.
- `router_delivery_count_advanced_SUITE` and `router_gateway_contract_valid_SUITE`: suites error out with “Invalid reference to group … in …:all/0”, indicating the group catalog or lookup path is misaligned.
- `router_duration_unit_SUITE`: real-time assertion `Result < 50000` is not satisfied, hinting at a timing measurement drift or stale clock usage.
- Additional heavy-tier noise: `router_ext_load_advanced_SUITE` crashes with `badmap ~"ok"` in `router_decider:execute_provider_selection/5`, `router_ext_load_baseline_SUITE` raises `badarg` in `ets:delete_all_objects/1`, and multiple `router_nats:start_link/0` undefined-function cascades kill app startups for the later suites.
