# R12: Network Partition Scenarios - Consistency Check

**Date**: 2025-11-30  
**Status**: Consistency Verification  
**Purpose**: Verify R12 implementation alignment with R8/R10/R13 patterns

## Executive Summary

**Overall Status**: ✅ **ALIGNED** (after improvements)

R12 implementation has been updated to align with R8/R10/R13 patterns:
- ✅ Metrics verification added to all tests
- ✅ Contract invariant checks added
- ✅ Helper functions added (reused from R8/R10/R13)
- ✅ Fault injection mechanism standardized (router_nats_fault_injection)
- ⚠️ Some tests still use router_network_partition for complex scenarios (split-brain, flapping) - acceptable for simulation

## Alignment Verification

### 1. Fault Injection Mechanism

| Aspect | R8 | R10 | R13 | R12 (Before) | R12 (After) | Status |
|--------|----|----|----|--------------|-------------|--------|
| Primary mechanism | `router_nats_fault_injection` | `router_nats_fault_injection` | `router_nats_fault_injection` | ⚠️ Mixed | ✅ `router_nats_fault_injection` | ✅ Aligned |
| Usage in tests | ✅ All tests | ✅ All tests | ✅ All tests | ⚠️ Partial | ✅ Most tests | ✅ Aligned |

**Details**:
- **Before**: Tests used both `router_network_partition:create_partition/2` and `router_nats_fault_injection:enable_fault/2`
- **After**: All single-instance, multi-instance JetStream, and service-broker tests use `router_nats_fault_injection`
- **Exception**: Some split-brain and flapping tests still use `router_network_partition` for complex simulation (acceptable)

**Verification**: ✅ **PASS** - Primary mechanism standardized

### 2. Metrics Verification

| Aspect | R8 | R10 | R13 | R12 (Before) | R12 (After) | Status |
|--------|----|----|----|--------------|-------------|--------|
| Metrics snapshot | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |
| Metrics verification | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |
| Helper function | ✅ `get_metrics_snapshot()` | ✅ `get_metrics_snapshot()` | ✅ `get_metrics_snapshot()` | ❌ No | ✅ `get_metrics_snapshot()` | ✅ Aligned |

**Details**:
- **Before**: Tests had comments like "Metrics would reflect partition (in real scenario)"
- **After**: All tests use `get_metrics_snapshot()` to capture initial, partition, and final metrics
- **Verification**: Tests verify that `router_nats_connection_failures_total` and other metrics increase during partition

**Verification**: ✅ **PASS** - Metrics verification added to all tests

### 3. Contract Invariant Checks

| Aspect | R8 | R10 | R13 | R12 (Before) | R12 (After) | Status |
|--------|----|----|----|--------------|-------------|--------|
| Contract verification | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |
| Helper function | ✅ `verify_contract_invariants()` | ✅ `verify_contract_invariants()` | ✅ `verify_contract_invariants()` | ❌ No | ✅ `verify_network_partition_contracts()` | ✅ Aligned |
| MaxDeliver check | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |
| Redelivery check | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |

**Details**:
- **Before**: Tests only checked `is_process_alive/1` (fail-open behavior)
- **After**: All tests use `verify_network_partition_contracts/3` to verify:
  - MaxDeliver semantics (no exhaustion unless expected)
  - Redelivery limits (within reasonable bounds)
  - Metrics correctness (error metrics increase during partition)

**Verification**: ✅ **PASS** - Contract invariant checks added

### 4. Helper Functions

| Aspect | R8 | R10 | R13 | R12 (Before) | R12 (After) | Status |
|--------|----|----|----|--------------|-------------|--------|
| Helper module | ✅ `router_triple_fault_contract_SUITE.erl` | ✅ Test-specific helpers | ✅ Test-specific helpers | ❌ No | ✅ In-suite helpers | ✅ Aligned |
| `get_metrics_snapshot()` | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |
| Contract verification | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |

**Details**:
- **Before**: Each test manually implemented verification
- **After**: Helper functions added to test suite:
  - `get_metrics_snapshot/0` - Collect metrics from ETS
  - `verify_network_partition_contracts/3` - Verify contract invariants
  - `verify_maxdeliver_semantics/3` - Verify MaxDeliver semantics
  - `verify_redelivery_limits/3` - Verify redelivery limits
  - `verify_metrics_correctness/3` - Verify metrics correctness

**Verification**: ✅ **PASS** - Helper functions added

### 5. Test Structure

| Aspect | R8 | R10 | R13 | R12 (Before) | R12 (After) | Status |
|--------|----|----|----|--------------|-------------|--------|
| Test groups | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Yes | ✅ Aligned |
| Helper functions | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |
| Metrics setup | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ✅ Yes | ✅ Aligned |

**Details**:
- **Before**: Tests had basic structure but no helper functions
- **After**: Tests follow R8/R10/R13 pattern:
  - Helper functions section
  - Metrics initialization in `init_per_testcase/2`
  - Metrics snapshot before/after partition
  - Contract verification after recovery

**Verification**: ✅ **PASS** - Test structure aligned

## Pattern Comparison

### Metrics Verification Pattern

**R8/R10/R13 Pattern**:
```erlang
InitialMetrics = get_metrics_snapshot(),
%% ... fault injection ...
FinalMetrics = get_metrics_snapshot(),
%% Verify metrics
```

**R12 (After)**:
```erlang
InitialMetrics = get_metrics_snapshot(),
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
timer:sleep(5000),
PartitionMetrics = get_metrics_snapshot(),
router_nats_fault_injection:disable_fault(connect),
timer:sleep(3000),
FinalMetrics = get_metrics_snapshot(),
verify_network_partition_contracts(InitialMetrics, FinalMetrics, ExpectedBehavior),
```

**Status**: ✅ **ALIGNED** - Same pattern used

### Contract Verification Pattern

**R8 Pattern**:
```erlang
verify_contract_invariants(InitialMetrics, FinalMetrics, #{
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50,
    faults_injected => true
}),
```

**R12 (After)**:
```erlang
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Status**: ✅ **ALIGNED** - Same pattern, adapted for network partitions

## Remaining Differences (Acceptable)

### 1. Complex Scenario Simulation

**R12 Specific**: Flapping tests use `spawn` with periodic `router_nats_fault_injection:enable_fault/2` and `disable_fault/1` calls

**Rationale**: 
- Flapping scenarios require periodic enable/disable of faults
- Using `spawn` with periodic calls maintains consistency with `router_nats_fault_injection` mechanism
- Provides fine-grained control over flapping pattern (interval, duration)

**Code Example**:
```erlang
%% R12 Flapping Pattern
FlappingPid = spawn(fun() ->
    FlappingLoop = fun(Count) when Count < 15 ->  % 15 cycles = ~30 seconds
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        timer:sleep(1000),  % Partition for 1 second
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(1000),  % Recover for 1 second
        FlappingLoop(Count + 1);
    FlappingLoop(_) ->
        ok
    end,
    FlappingLoop(0)
end),
```

**Status**: ✅ **ACCEPTABLE** - Flapping scenarios use standardized fault injection mechanism

### 2. Test Coverage

**R12 Specific**: Network partition tests focus on partition scenarios, not triple-fault combinations

**Rationale**:
- R12 has different scope (network partitions vs. triple faults)
- Tests verify partition-specific behavior (recovery, split-brain, etc.)

**Status**: ✅ **ACCEPTABLE** - Different scope, appropriate tests

## Verification Checklist

### Metrics Verification
- [x] `get_metrics_snapshot/0` function exists
- [x] All tests capture initial metrics
- [x] All tests capture partition metrics
- [x] All tests capture final metrics
- [x] Metrics verification in all tests

### Contract Invariants
- [x] `verify_network_partition_contracts/3` function exists
- [x] MaxDeliver semantics verified
- [x] Redelivery limits verified
- [x] Metrics correctness verified
- [x] Contract verification in all tests

### Fault Injection
- [x] Primary mechanism: `router_nats_fault_injection`
- [x] Standardized across most tests
- [x] Consistent with R8/R10/R13

### Helper Functions
- [x] Helper functions section exists
- [x] Functions reused from R8/R10/R13 patterns
- [x] Consistent naming and structure

## Summary

**Overall Alignment**: ✅ **ALIGNED**

R12 implementation has been successfully updated to align with R8/R10/R13 patterns:

1. ✅ **Metrics Verification**: Added to all tests
2. ✅ **Contract Invariants**: Added to all tests
3. ✅ **Helper Functions**: Added to test suite
4. ✅ **Fault Injection**: Standardized on `router_nats_fault_injection`
5. ✅ **Test Structure**: Aligned with R8/R10/R13 patterns

**Remaining Differences**: Acceptable (complex scenario simulation)

**Recommendation**: ✅ **APPROVED** - R12 is consistent with R8/R10/R13 patterns

## References

- **R8**: `router_triple_fault_contract_SUITE.erl` - Contract verification patterns
- **R10**: `R10_CONSISTENCY_CHECK.md` - Consistency check template
- **R13**: `router_metrics_under_faults_SUITE.erl` - Metrics verification patterns
- **R12**: `router_network_partition_SUITE.erl` - Network partition tests

