# Fault Injection Smoke Tests

## Purpose

This document defines the **minimal critical set** of fault injection tests for production incident scenarios. These smoke tests are designed to:
- Run quickly (< 2 minutes total)
- Be maximally stable (no flaky behavior)
- Cover the most common production incidents
- Be executable independently via tags or separate suite groups

## Smoke Test Selection Criteria

Smoke tests were selected based on:
1. **Production incident frequency**: Most common failure scenarios
2. **Criticality**: Scenarios that can cause service unavailability
3. **Recovery verification**: Scenarios that verify self-healing capabilities
4. **State consistency**: Scenarios that verify ETS/delivery count integrity

## Minimal Critical Smoke Test Set

### 1. NATS Connection Loss and Recovery

**Test**: `test_nats_connection_loss_recovery`  
**Suite**: `router_jetstream_fault_injection_SUITE`  
**Duration**: ~30-60 seconds  
**Scenario**: NATS connection is lost, then restored  
**Why Critical**: 
- Most common production incident
- Verifies router reconnection capability
- Ensures no message loss during reconnection

**Verifies**:
- Router reconnects automatically
- Consumers continue functioning
- New messages processed without anomalies
- ETS state preserved during reconnection

**Run Command**:
```bash
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE --case test_nats_connection_loss_recovery
```

### 2. Max Delivery Count Exhaustion

**Test**: `test_max_delivery_count_exhaustion` / `test_decide_max_delivery_count_exhaustion`  
**Suite**: `router_result_consumer_SUITE` / `router_decide_consumer_SUITE`  
**Duration**: ~30-60 seconds  
**Scenario**: Message goes through series of NAK/errors until max delivery count (default: 3)  
**Why Critical**:
- Prevents infinite retry loops
- Verifies DLQ/drop behavior
- Ensures resource exhaustion protection

**Verifies**:
- Router/consumer does not enter infinite retry
- Transitions to expected final state (DLQ/drop)
- MaxDeliver exhaustion metric is emitted
- Warning log is written

**Run Command**:
```bash
# Result consumer
rebar3 ct --suite test/router_result_consumer_SUITE --case test_max_delivery_count_exhaustion

# Decide consumer
rebar3 ct --suite test/router_decide_consumer_SUITE --case test_decide_max_delivery_count_exhaustion
```

### 3. Prolonged Fault Period with Recovery (No Router Restart)

**Test**: `test_prolonged_fault_period_recovery_no_router_restart` / `test_decide_prolonged_fault_period_recovery_no_router_restart`  
**Suite**: `router_result_consumer_SUITE` / `router_decide_consumer_SUITE`  
**Duration**: ~60-90 seconds  
**Scenario**: Extended period of ACK/publish failures (first 10 calls fail), then recovery  
**Why Critical**:
- Simulates extended network issues
- Verifies router continues living during prolonged faults
- Ensures recovery without manual intervention

**Verifies**:
- Router continues living throughout fault period
- Processes new messages after recovery without restart
- Old messages reach expected final state
- No process crashes or hangs

**Run Command**:
```bash
# Result consumer
rebar3 ct --suite test/router_result_consumer_SUITE --case test_prolonged_fault_period_recovery_no_router_restart

# Decide consumer
rebar3 ct --suite test/router_decide_consumer_SUITE --case test_decide_prolonged_fault_period_recovery_no_router_restart
```

## Running All Smoke Tests

### Quick Smoke Test Run (All 3 Scenarios)

```bash
# Run all smoke tests
cd apps/otp/router
rebar3 ct \
  --suite test/router_jetstream_fault_injection_SUITE --case test_nats_connection_loss_recovery \
  --suite test/router_result_consumer_SUITE --case test_max_delivery_count_exhaustion,test_prolonged_fault_period_recovery_no_router_restart \
  --suite test/router_decide_consumer_SUITE --case test_decide_max_delivery_count_exhaustion,test_decide_prolonged_fault_period_recovery_no_router_restart
```

### Via Makefile (if configured)

```bash
make test-fault-injection-smoke
```

## CI Integration

### Fast CI (PR Checks)
Smoke tests are **NOT** included in fast CI to keep PR feedback quick (< 5 minutes).

### Extended CI (Merge to main/staging)
Smoke tests are included in extended CI pipeline:
- Drone CI: `nats-fault-injection-tests` step
- GitLab CI: `router-observability-tests` job

### Nightly CI (Scheduled)
All fault injection tests (including smoke tests) run in nightly CI.

## Test Stability

### Stability Measures
1. **Bounded waits**: All waits use `test_helpers:wait_for_condition` instead of fixed sleeps
2. **Reduced delays**: Processing delays minimized (50ms instead of 100ms)
3. **Isolated state**: Each test sets up its own mocks and ETS tables
4. **Cleanup**: All tests properly clean up mocks and ETS tables

### Known Stability Issues
None currently identified. If flaky behavior is observed:
1. Check timing dependencies (replace fixed sleeps with bounded waits)
2. Verify test isolation (no shared state between tests)
3. Check for race conditions in concurrent message processing

## Maintenance

### Adding New Smoke Tests
1. Test must run in < 60 seconds
2. Test must be maximally stable (no flaky behavior)
3. Test must cover a common production incident scenario
4. Update this document with new smoke test entry
5. Add to CI pipeline if appropriate

### Removing Smoke Tests
1. Only remove if scenario is no longer relevant
2. Document reason for removal
3. Update CI pipeline configuration

## CI Integration Status

### Extended CI (Merge to main/staging)
✅ **Integrated**: Smoke tests are included in extended CI pipeline:
- **Drone CI**: `nats-fault-injection-tests` step runs smoke tests
- **GitLab CI**: `router-observability-tests` job runs smoke tests

### Nightly CI (Scheduled)
✅ **Integrated**: All fault injection tests (including smoke tests) run in nightly CI.

### Fast CI (PR Checks)
❌ **Not included**: Smoke tests are excluded from fast CI to keep PR feedback quick (< 5 minutes).

## References

- `apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md`: Full test scenarios documentation
- `apps/otp/router/test/FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`: Requirements traceability matrix
- `apps/otp/router/test/FAULT_INJECTION_TEST_AUDIT.md`: Test audit and differentiation analysis
- `apps/otp/router/test/test_helpers.erl`: Helper functions for bounded waits
- `apps/otp/router/docs/TEST_CLASSIFICATION.md`: Test classification and marking

