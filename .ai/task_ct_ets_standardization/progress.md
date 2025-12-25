# Progress

Status: DONE

Shared ETS helper added: test/test_utils/router_ets_helpers.erl

Suites migrated to helper: all `test/*_SUITE.erl` with ETS usage, except
`test/router_test_init_SUITE.erl` (kept on router_test_init to validate
the helper being tested).

Verification (ROUTER_TEST_LEVEL=heavy) after migration:
- test/router_concurrent_faults_soak_SUITE.erl: PASS (3 tests)
- test/router_concurrent_faults_combo_SUITE.erl: PASS (4 tests)
- test/router_decide_consumer_faults_SUITE.erl: PASS (4 tests)
- test/router_result_consumer_faults_SUITE.erl: PASS (2 tests)
- test/router_recovery_faults_SUITE.erl: PASS (7 tests)
- test/router_ext_chaos_failure_SUITE.erl: PASS (3 tests)
- test/router_ext_chaos_recovery_SUITE.erl: PASS (2 tests)
- test/router_intake_chaos_advanced_SUITE.erl: PASS (2 tests)

Additional ETS regression verification (ROUTER_TEST_LEVEL=full):
- test/router_jetstream_e2e_cp2_SUITE.erl: PASS (4 tests)
- test/router_jetstream_redelivery_runtime_SUITE.erl: PASS (4 tests)

Consolidated log: docs/tests/ETS_HELPER_REGRESSION_RUNS.md
