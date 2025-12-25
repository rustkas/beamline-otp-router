# Progress

Status: IN PROGRESS

Batch #1 (heavy-only by filename)

- suite: router_concurrent_faults_soak_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (3 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (soak_tests group)

- suite: router_soak_baseline_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (3 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (baseline_soak group); STRESS_SOAK_DURATION_HOURS=0.01

- suite: router_soak_single_fault_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (4 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (single_fault_soak group)

- suite: router_soak_multi_fault_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (3 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (multi_fault_soak group); STRESS_SOAK_DURATION_HOURS=0.01

- suite: router_stress_soak_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; stress_soak_tests group placeholder

- suite: router_jetstream_soak_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; soak_tests group placeholder

- suite: router_jetstream_soak_restart_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; RUN_JETSTREAM_SOAK=false

- suite: router_jetstream_soak_prod_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (1 test)
  notes: clean run via scripts/run_ct_batch1_clean.sh; RUN_JETSTREAM_SOAK=false

- suite: router_jetstream_soak_combined_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; RUN_JETSTREAM_SOAK=false

- suite: router_jetstream_soak_perf_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; RUN_JETSTREAM_SOAK=false

- suite: router_ext_chaos_failure_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (3 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (failure_tests group)

- suite: router_ext_chaos_recovery_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (2 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (recovery_tests group)

- suite: router_extensions_chaos_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; chaos_tests group placeholder

- suite: router_intake_chaos_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh; chaos_tests group placeholder

- suite: router_intake_chaos_restart_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (3 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (restart_tests group)

- suite: router_intake_chaos_advanced_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (2 tests)
  notes: clean run via scripts/run_ct_batch1_clean.sh (advanced_tests group)

- suite: router_chaos_engineering_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=SKIP (RUN_CHAOS_TESTS=false)
  notes: clean run via scripts/run_ct_batch1_clean.sh; chaos tests gated

Batch #2 (stress/faults)

- suite: router_concurrent_faults_stress_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: heavy-only mapping applied; added stress_tests group placeholder

- suite: router_metrics_faults_aggregation_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (aggregation_tests group)

- suite: router_metrics_faults_cardinality_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (cardinality_tests group)

- suite: router_metrics_under_faults_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: core_tests group placeholder applied to all levels (ungrouped suite)

- suite: router_decide_consumer_faults_SUITE.erl
  status: DONE
  verification: fast=0, full=PASS, heavy=PASS
  notes: mapped to full/heavy only (fault_tests group)

- suite: router_result_consumer_faults_SUITE.erl
  status: DONE
  verification: fast=0, full=PASS, heavy=PASS
  notes: mapped to full/heavy only (fault_tests group)

- suite: router_recovery_faults_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (fault_tests group)

- suite: router_concurrent_faults_basic_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (basic_concurrent_faults group)

- suite: router_concurrent_faults_combo_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (combo_tests group)

- suite: router_concurrent_faults_extended_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (extended_scenarios group)

- suite: router_advanced_faults_mixed_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS
  notes: heavy-only mapping applied (mixed_pattern_tests group)

- suite: router_advanced_faults_triple_SUITE.erl
  status: DONE
  verification: fast=PASS, full=PASS, heavy=PASS
  notes: identical groups for fast/full/heavy (triple_fault_tests group)

- suite: router_advanced_concurrent_faults_SUITE.erl
  status: DONE
  verification: fast=PASS, full=PASS, heavy=PASS
  notes: fast=smoke; full/heavy=all fault groups

- suite: router_concurrent_faults_SUITE.erl
  status: DONE
  verification: fast=0, full=0, heavy=PASS (0 tests)
  notes: empty suite; groups_for_level returns [] for all levels
