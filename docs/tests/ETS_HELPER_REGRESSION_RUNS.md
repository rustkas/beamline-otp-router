# ETS Helper Regression Runs

Date: 2025-12-21

Scope: CT suites that use `router_ets_helpers` after ETS standardization.

## ROUTER_TEST_LEVEL=full

PASS (tests executed)
- test/router_abuse_SUITE.erl (5)
- test/router_core_telemetry_contract_SUITE.erl (4)
- test/router_cp1_red_bar_SUITE.erl (12)
- test/router_decide_consumer_unit_SUITE.erl (18)
- test/router_delivery_count_advanced_SUITE.erl (4)
- test/router_ets_guard_unit_SUITE.erl (9)
- test/router_gateway_integration_SUITE.erl (8)
- test/router_headers_propagation_e2e_SUITE.erl (4)
- test/router_hotspots_3_SUITE.erl (21)
- test/router_idem_advanced_SUITE.erl (6)
- test/router_intake_error_config_SUITE.erl (6)
- test/router_jetstream_e2e_cp2_SUITE.erl (4)
- test/router_jetstream_e2e_integration_SUITE.erl (7)
- test/router_jetstream_redelivery_runtime_SUITE.erl (4)
- test/router_jetstream_unit_SUITE.erl (24 passed, 2 skipped by test mode)
- test/router_metrics_unit_SUITE.erl (11)
- test/router_nats_integ_recovery_SUITE.erl (7)
- test/router_nats_sub_caf_advanced_SUITE.erl (8)
- test/router_observability_otel_spans_SUITE.erl (3)
- test/router_worker_contract_SUITE.erl (7)

0 tests at full level (expected by suite grouping)
- test/router_cb_load_concurrent_SUITE.erl
- test/router_decide_consumer_heavy_SUITE.erl
- test/router_delivery_count_core_SUITE.erl
- test/router_ets_guard_SUITE.erl
- test/router_ext_load_advanced_SUITE.erl
- test/router_ext_load_baseline_SUITE.erl
- test/router_intake_overload_SUITE.erl
- test/router_jetstream_fi_connection_SUITE.erl
- test/router_nats_performance_SUITE.erl
- test/router_nats_sub_caf_core_SUITE.erl
- test/router_performance_load_SUITE.erl
- test/router_recovery_connection_SUITE.erl

## ROUTER_TEST_LEVEL=heavy (prior ETS validation set)

PASS
- test/router_concurrent_faults_soak_SUITE.erl (3)
- test/router_concurrent_faults_combo_SUITE.erl (4)
- test/router_decide_consumer_faults_SUITE.erl (4)
- test/router_result_consumer_faults_SUITE.erl (2)
- test/router_recovery_faults_SUITE.erl (7)
- test/router_ext_chaos_failure_SUITE.erl (3)
- test/router_ext_chaos_recovery_SUITE.erl (2)
- test/router_intake_chaos_advanced_SUITE.erl (2)
