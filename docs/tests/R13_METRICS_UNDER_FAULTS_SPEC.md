# R13: Metrics Under Faults and Load - Specification

**Date**: 2025-11-30  
**Status**: Specification  
**Purpose**: Specification for extended metrics validation tests under faults and load

## Problem Statement

**Requirement R13**: No dedicated tests for extended metrics validation under faults (aggregation, rates, label cardinality under load).

### Current State

- Basic metrics tests exist (`router_metrics_*_SUITE.erl`)
- Triple fault tests check metrics correctness (`router_triple_fault_contract_SUITE.erl`)
- **Missing**: Dedicated tests for metrics behavior under:
  - Fault injection scenarios
  - High load conditions
  - Aggregation correctness
  - Rate calculation accuracy
  - Label cardinality limits

### Problem

Metrics may behave correctly in normal conditions but "drift" under:
- Partial failures
- Retries
- Network glitches
- Mass parallel events

## Goal

Create a **dedicated automated test suite** that:

1. **Simulates various failures and abnormal modes** (fault injection, delays, drops, timeouts, component restarts)
2. **Generates controlled load** (event/request streams)
3. **Collects and analyzes metrics**, verifying:
   - Aggregation correctness
   - Rate calculation accuracy
   - Label cardinality limits

**Result**: Confidence that under faults and load:
- Metrics are accurate
- Metrics don't "leak" (grow infinitely without reason)
- Metrics don't create label explosion

## What to Test

### 1. Metrics Aggregation

**Scenarios**:
- Some requests fail with errors
- Some requests go to retry
- Shard/node temporarily unavailable then recovers
- Flapping connections (toxic network)

**Checks**:
- Total counters (`counter`) reflect **actual counts**:
  - Successful operations
  - Failed operations
  - Retries (if tracked separately)
- Aggregated values (`sum`, `count`, `histogram`, `summary`) don't lose or **duplicate** events
- After recoveries/restarts, no **gaps** or jumps contradicting expected behavior

**Example**:
- Send 1000 requests, intentionally drop 200, 100 go to retry
- Verify:
  - `total_requests = 1000`
  - `failed_requests = 200`
  - `retried_requests = 100`, consistent with logs and scenarios

### 2. Rate Calculations

**Purpose**: Metrics like "requests per second", "errors per minute", built on `rate()`/`irate()` in Prometheus or equivalents.

**Scenarios**:
- Spike loads: short bursts of requests
- Long plateau loads
- Sudden stop/drop in load
- Failures where some requests don't complete

**Checks**:
- For known load pattern, **observed rate**:
  - Approximately matches expected (accounting for aggregation windows)
  - No anomalous spikes (huge peak on restart, "drops to zero" where not expected)
- After restarts/failures, no **false rate spikes** that don't correspond to actual load

**Example**:
- Generate 500 RPS for 30 seconds
- Expect `requests_rate` via `rate()` to be within acceptable error margin around 500 RPS on the interval

### 3. Label Cardinality Under Load

**Problem**: With many unique label values (e.g., `user_id`, `request_id`, dynamic keys), metrics can explode:
- Storage volume
- Prometheus/monitoring load
- RAM consumption

**Scenarios**:
- Mass load with many different:
  - `tenant_id`
  - `stream_name`
  - `error_code`
  - Other business labels that can grow in quantity
- Distortion/error where high-cardinality value leaks into label (`uuid`, full `url`, etc.)

**Checks**:
- Number of **unique label combinations** in metrics:
  - Stays within reasonable bounds
  - Matches expected model (e.g., limited by `tenant_id` and `status`, not growing linearly with each request)
- No "new" labels per request/error
- Under anomalous flow (e.g., inject many different `request_id`), system:
  - Either **doesn't put** this in label
  - Or **limits/sanitizes** (truncation, normalization, mapping)

**Technical**: Need to query exporter/Prometheus/store for number of time series for key metrics and compare with upper bound.

## Test Types

- **Separate test module/scenario**, not mixed with regular unit/integration tests
- **Possible variants**:
  - Integration tests with running service and metrics collection via `/metrics`
  - Load/functional tests with fault injection (chaos tests) + subsequent metrics analysis
- Tests must:
  - **Run automatically** (CI / separate pipeline / nightly)
  - Provide **clear assertions**: not just "look at graphs manually", but programmatically verify values/limits

## Expected Outputs

### Test Suite

**File**: `router_metrics_under_faults_SUITE.erl`

**Test Groups**:
1. **Aggregation Tests** (`aggregation_tests`)
   - `test_aggregation_under_partial_failures`
   - `test_aggregation_under_retries`
   - `test_aggregation_under_node_failures`
   - `test_aggregation_under_flapping_connections`
   - `test_aggregation_after_recovery`

2. **Rate Tests** (`rate_tests`)
   - `test_rate_under_spike_load`
   - `test_rate_under_plateau_load`
   - `test_rate_under_sudden_stop`
   - `test_rate_under_failures`
   - `test_rate_after_restart`

3. **Cardinality Tests** (`cardinality_tests`)
   - `test_cardinality_under_mass_load`
   - `test_cardinality_with_many_tenants`
   - `test_cardinality_with_many_streams`
   - `test_cardinality_with_high_cardinality_leak`
   - `test_cardinality_limits_enforced`

4. **Combined Tests** (`combined_tests`)
   - `test_metrics_under_combined_faults`
   - `test_metrics_under_chaos_scenarios`
   - `test_metrics_under_prolonged_load`

### Helper Functions

**File**: `router_metrics_under_faults_helpers.erl`

**Functions**:
- `generate_controlled_load/3` - Generate controlled request load
- `inject_faults/2` - Inject faults during test
- `collect_metrics_snapshot/0` - Collect metrics at point in time
- `calculate_rate/3` - Calculate rate from metrics snapshots
- `count_label_combinations/2` - Count unique label combinations
- `verify_aggregation/3` - Verify aggregation correctness
- `verify_rate/4` - Verify rate calculation
- `verify_cardinality/3` - Verify cardinality limits

### Documentation

**File**: `R13_METRICS_UNDER_FAULTS_README.md`

**Contents**:
- How to run tests
- What failure types are modeled
- What metrics and invariants are checked
- Expected test durations
- CI/CD integration

## Success Criteria

### Aggregation Tests

1. ✅ Total counters match expected values (± tolerance)
2. ✅ Aggregated values don't lose or duplicate events
3. ✅ No gaps or jumps after recoveries/restarts
4. ✅ Metrics consistent with logs and scenarios

### Rate Tests

1. ✅ Observed rate matches expected rate (± acceptable margin)
2. ✅ No anomalous spikes or drops
3. ✅ Rate calculations stable after restarts
4. ✅ Rate windows handled correctly

### Cardinality Tests

1. ✅ Unique label combinations stay within bounds
2. ✅ No label explosion under load
3. ✅ High-cardinality leaks prevented or sanitized
4. ✅ Cardinality matches expected model

### Combined Tests

1. ✅ All checks pass under combined scenarios
2. ✅ Metrics remain accurate under chaos
3. ✅ No resource leaks (memory, storage)

## Implementation Plan

### Phase 1: Foundation (Current)

- [x] Specification document (this file)
- [ ] Test suite structure
- [ ] Helper functions skeleton

### Phase 2: Aggregation Tests

- [ ] Implement aggregation test helpers
- [ ] Implement partial failure scenarios
- [ ] Implement retry scenarios
- [ ] Implement node failure scenarios
- [ ] Implement flapping connection scenarios
- [ ] Implement recovery scenarios

### Phase 3: Rate Tests

- [ ] Implement rate calculation helpers
- [ ] Implement spike load scenarios
- [ ] Implement plateau load scenarios
- [ ] Implement sudden stop scenarios
- [ ] Implement failure scenarios
- [ ] Implement restart scenarios

### Phase 4: Cardinality Tests

- [ ] Implement cardinality counting helpers
- [ ] Implement mass load scenarios
- [ ] Implement multi-tenant scenarios
- [ ] Implement multi-stream scenarios
- [ ] Implement high-cardinality leak scenarios
- [ ] Implement cardinality limit enforcement

### Phase 5: Combined Tests

- [ ] Implement combined fault scenarios
- [ ] Implement chaos scenarios
- [ ] Implement prolonged load scenarios

### Phase 6: Documentation and CI

- [ ] Complete README
- [ ] Integrate into CI/CD
- [ ] Add to test documentation

## Integration Points

### Existing Test Infrastructure

- **Fault Injection**: `router_nats_fault_injection` module
- **Metrics Collection**: `router_metrics`, `router_prometheus`
- **Test Helpers**: `router_test_helpers.erl`, `router_fault_injection_helpers.erl`
- **Metrics Contracts**: `router_metrics_contract_helpers.erl`

### CI/CD Integration

- Add to `.github/workflows/test.yml` or equivalent
- Run as separate test group (can be longer-running)
- Optionally run as nightly tests

## References

- `router_triple_fault_contract_SUITE.erl` - Contract tests with metrics checks
- `router_metrics_*_SUITE.erl` - Existing metrics tests
- `router_prometheus.erl` - Prometheus export
- `router_metrics.erl` - Metrics storage
- `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md` - Metrics contracts
- `docs/archive/dev/METRICS_LABELS_IMPLEMENTATION_SPEC.md` - Labels specification

