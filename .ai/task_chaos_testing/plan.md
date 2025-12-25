# Implementation Plan: T-CHAOS-01

## Phase 1: Infrastructure Setup
1. Review existing chaos test helpers:
   - `test/router_network_partition_helper.erl`
   - `test/router_triple_fault_helper.erl`
   - `src/router_nats_fault_injection.erl`
2. Create chaos helper module: `test/router_chaos_helper.erl`
   - `kill_nats_process/0` - kills NATS via PID
   - `kill_router_process/0` - kills Router supervisor
   - `induce_jetstream_lag/2` - pauses ACKs for duration
   - `measure_time_to_green/1` - polls health until operational

## Phase 2: Test Suite Creation
1. Create `test/router_chaos_controlled_SUITE.erl`:
   - `init_per_suite/1` - start NATS, Router, create test streams
   - `end_per_suite/1` - cleanup
   - `test_nats_kill_recovery/1` - NATS kill scenario
   - `test_router_kill_recovery/1` - Router kill scenario
   - `test_jetstream_lag_recovery/1` - JetStream lag scenario
   - `test_recovery_slo_compliance/1` - SLO verification

## Phase 3: Chaos Orchestration Script
1. Create `scripts/chaos_test.sh`:
   - Start dependencies (NATS, Router)
   - Run chaos test suite
   - Collect evidence (logs, metrics)
   - Generate report
   - Cleanup

## Phase 4: Validation
1. Run full chaos suite locally
2. Verify all scenarios pass
3. Verify SLO compliance
4. Document failure modes

## Phase 5: Documentation
1. Create `docs/CHAOS_FAILURE_MODES.md` - catalog of observed failures
2. Update `docs/operations/RECOVERY_RUNBOOK.md` with chaos insights
3. Add chaos test instructions to `README.md`
