## Progress (state capture)

### router_test_bootstrap (implemented)
- Helper module: `test/router_test_bootstrap.erl`
- Exported callbacks: `init_per_suite/2`, `end_per_suite/2`, `init_per_testcase/3`, `end_per_testcase/3`
- Suite options (documented in-module): `app`, `reset_app`, `infra_mode`, `common_env`, `app_env`, `ensure_apps`, `start`, `wait_for_app_start`
- End-per-suite options: `app`, `start`, `stop`, `cleanup_mocks`
- Testcase options: `clear_metrics`, `metrics_test_helper`, `clear_faults`, `ensure_router_nats_alive`, `cleanup_mocks`
- Infra mode behavior:
  - Uses `router_testops_helper:get_chaos_mode/0` and caches it via `persistent_term` key `router_test_bootstrap_infra_mode`
  - If suite requests `infra_mode => docker` while detected mode is not docker, returns `{skip, "...Docker mode..."}` (no start/stop performed)
- Start/stop strategies:
  - Start: `router_suite` (default) -> `router_suite_helpers:start_router_suite/0`; `ensure_all_started` -> `application:ensure_all_started/1`; `router_app` -> `router_test_utils:start_router_app/0`; `none`
  - Stop: `router_suite` -> `router_suite_helpers:stop_router_suite/0`; `stop_app` -> `application:stop/1`; `none` (and `auto` resolves based on start)

### Migrated suites (using router_test_bootstrap:init_per_suite/2)
Status meanings:
- done: suite uses `router_test_bootstrap` for suite init and suite end
- partial: suite init uses `router_test_bootstrap`, but suite end does not

- done: `router_admin_cp_status_SUITE` (`test/router_admin_cp_status_SUITE.erl`)
- done: `router_admin_grpc_concurrency_SUITE` (`test/router_admin_grpc_concurrency_SUITE.erl`)
- done: `router_admin_grpc_integration_SUITE` (`test/router_admin_grpc_integration_SUITE.erl`)
- done: `router_admin_grpc_rbac_SUITE` (`test/router_admin_grpc_rbac_SUITE.erl`)
- partial: `router_admin_self_check_SUITE` (`test/router_admin_self_check_SUITE.erl`)
- done: `router_alerts_test_SUITE` (`test/router_alerts_test_SUITE.erl`)
- done: `router_circuit_breaker_invariants_SUITE` (`test/router_circuit_breaker_invariants_SUITE.erl`)
- done: `router_circuit_breaker_recovery_SUITE` (`test/router_circuit_breaker_recovery_SUITE.erl`)
- done: `router_circuit_breaker_smoke_SUITE` (`test/router_circuit_breaker_smoke_SUITE.erl`)
- done: `router_circuit_breaker_transitions_SUITE` (`test/router_circuit_breaker_transitions_SUITE.erl`)
- done: `router_circuit_breaker_prop_SUITE` (`test/router_circuit_breaker_prop_SUITE.erl`) (keeps circuit breaker alive + prunes R10 metrics after bootstrap)
- done: `router_compliance_SUITE` (`test/router_compliance_SUITE.erl`)
- done: `router_config_validator_SUITE` (`test/router_config_validator_SUITE.erl`)
- done: `router_core_basic_SUITE` (`test/router_core_basic_SUITE.erl`)
- done: `router_core_telemetry_contract_SUITE` (`test/router_core_telemetry_contract_SUITE.erl`)
- done: `router_cp1_fields_integration_SUITE` (`test/router_cp1_fields_integration_SUITE.erl`)
- done: `router_dashboard_test_SUITE` (`test/router_dashboard_test_SUITE.erl`)
- done: `router_decide_consumer_core_SUITE` (`test/router_decide_consumer_core_SUITE.erl`)
- done: `router_decide_consumer_faults_SUITE` (`test/router_decide_consumer_faults_SUITE.erl`)
- done: `router_metrics_labels_integration_SUITE` (`test/router_metrics_labels_integration_SUITE.erl`)
- done: `router_nats_connection_failure_SUITE` (`test/router_nats_connection_failure_SUITE.erl`)
- done: `router_nats_conn_failure_basic_SUITE` (`test/router_nats_conn_failure_basic_SUITE.erl`)
- done: `router_nats_conn_failure_jetstream_SUITE` (`test/router_nats_conn_failure_jetstream_SUITE.erl`)
- done: `router_nats_conn_failure_recovery_SUITE` (`test/router_nats_conn_failure_recovery_SUITE.erl`)
- done: `router_nats_publish_fail_open_SUITE` (`test/router_nats_publish_fail_open_SUITE.erl`)
- done: `router_nats_publish_metrics_SUITE` (`test/router_nats_publish_metrics_SUITE.erl`)
- done: `router_nats_publish_queueing_SUITE` (`test/router_nats_publish_queueing_SUITE.erl`)
- done: `router_policy_store_SUITE` (`test/router_policy_store_SUITE.erl`)
- done: `router_policy_SUITE` (`test/router_policy_SUITE.erl`)
- done: `router_metrics_faults_aggregation_SUITE` (`test/router_metrics_faults_aggregation_SUITE.erl`) (bootstrapped env + clears faults/metrics per test using helper)
- done: `router_metrics_faults_cardinality_SUITE` (`test/router_metrics_faults_cardinality_SUITE.erl`) (same env overrides + common test cleanup)
- done: `router_metrics_http_SUITE` (`test/router_metrics_http_SUITE.erl`) (bootstrap plus metrics counter/meck setup)
- done: `router_gateway_integration_SUITE` (`test/router_gateway_integration_SUITE.erl`) (bootstrap + policy/table prep)
- done: `router_grpc_compatibility_SUITE` (`test/router_grpc_compatibility_SUITE.erl`) (pure bootstrap)
- done: `router_health_integration_SUITE` (`test/router_health_integration_SUITE.erl`) (grpc/admin env via bootstrap)
- done: `router_decide_consumer_heavy_SUITE` (`test/router_decide_consumer_heavy_SUITE.erl`) (mock env + rate limiter meck + metrics ensure)
- done: `router_deployment_SUITE` (`test/router_deployment_SUITE.erl`)
- done: `router_nats_compatibility_SUITE` (`test/router_nats_compatibility_SUITE.erl`) (captures router_nats exports)
- done: `router_performance_benchmark_SUITE` (`test/router_performance_benchmark_SUITE.erl`) (policy baseline via bootstrap)
- done: `router_performance_load_SUITE` (`test/router_performance_load_SUITE.erl`) (policy + meck cleanup + metrics)
- done: `router_performance_regression_SUITE` (`test/router_performance_regression_SUITE.erl`) (baseline + metrics)
- done: `router_provider_integration_SUITE` (`test/router_provider_integration_SUITE.erl`) (sticky env + policy)

### Verification status (observed)
- `./scripts/ct-full.sh`: not yet verified as passing in this conversation; earlier attempt was blocked by suite linter with message `router_hotspots_3_SUITE.erl: rule ets_new exceeds baseline (current=1, allowed=0)`
- Targeted CT runs executed in this conversation (each reported: “All 0 tests passed.” unless noted):
  - `./bin/rebar3 ct --suite test/router_alerts_test_SUITE`
  - `./bin/rebar3 ct --suite test/router_dashboard_test_SUITE`
  - `./bin/rebar3 ct --suite test/router_config_validator_SUITE`
  - `./bin/rebar3 ct --suite test/router_compliance_SUITE`
  - `./bin/rebar3 ct --suite test/router_cp1_fields_integration_SUITE`
  - `./bin/rebar3 ct --suite test/router_metrics_faults_aggregation_SUITE`
  - `./bin/rebar3 ct --suite test/router_metrics_faults_cardinality_SUITE`
  - `./bin/rebar3 ct --suite test/router_metrics_http_SUITE` (All 1 tests passed)
  - `./bin/rebar3 ct --suite test/router_gateway_integration_SUITE`
  - `./bin/rebar3 ct --suite test/router_grpc_compatibility_SUITE`
  - `./bin/rebar3 ct --suite test/router_health_integration_SUITE`
  - `./bin/rebar3 ct --suite test/router_decide_consumer_heavy_SUITE`
  - `./bin/rebar3 ct --suite test/router_deployment_SUITE`
  - `./bin/rebar3 ct --suite test/router_nats_compatibility_SUITE`
  - `./bin/rebar3 ct --suite test/router_performance_benchmark_SUITE`
  - `./bin/rebar3 ct --suite test/router_performance_load_SUITE`
  - `./bin/rebar3 ct --suite test/router_performance_regression_SUITE`
  - `./bin/rebar3 ct --suite test/router_provider_integration_SUITE`
- Historical/baseline log exists: `test_baseline_20251213.log` contains “All 181 tests passed.” (relationship to current working tree is unknown/not verified)

### Not yet migrated (still uses router_suite_helpers:start_router_suite/0)
- None – every suite now uses `router_test_bootstrap` for the shared lifecycle.
