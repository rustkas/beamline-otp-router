# R12: Data Guarantees and Invariants

**Date**: 2025-11-30  
**Status**: Specification  
**Purpose**: Detailed specification of data guarantees and contract invariants for network partition scenarios

## Overview

This document defines the **data guarantees** and **contract invariants** that must be maintained during and after network partitions. All R12 tests verify these guarantees and invariants.

## Data Guarantees

### G1: No Message Loss

**Requirement**: Messages are not lost during network partitions.

**Details**:
- Messages "in transit" (queued for processing) are preserved
- Messages are retried until successful delivery or MaxDeliver exhaustion
- Messages exhausted MaxDeliver are not considered "lost" (they are explicitly exhausted)

**Verification**:
- MaxDeliver exhaustion is tracked: `router_jetstream_maxdeliver_exhausted_total`
- Messages processed: `router_nats_messages_processed_total`
- Messages queued: `router_nats_messages_queued_total`

**Test Coverage**: All network partition tests verify MaxDeliver semantics

**Example**:
```erlang
%% Message is queued during partition
%% After recovery, message is processed
%% If MaxDeliver is exhausted, message is not lost but explicitly exhausted
```

### G2: No Duplicate Processing

**Requirement**: Messages are not processed multiple times (idempotency).

**Details**:
- Idempotency layer prevents duplicate processing
- During split-brain, both instances may receive the same message, but only one processes it
- Duplicate detection is based on message ID and idempotency key

**Verification**:
- Duplicate processing metric: `router_duplicate_processing_total`
- Idempotency checks: ETS table consistency
- Max allowed duplicates: 0 (or configurable for split-brain recovery scenarios)

**Test Coverage**: Split-brain and recovery tests

**Example**:
```erlang
%% During split-brain, both instances receive message M1
%% Only one instance processes M1 (idempotency layer)
%% Duplicate processing metric remains 0
```

### G3: No State Inconsistencies

**Requirement**: System state remains consistent after network partition recovery.

**Details**:
- State synchronization after recovery
- No orphaned transactions or locks
- Consistent metrics and state across all instances

**Verification**:
- State consistency metric: `router_state_consistency_total`
- State consistency should not decrease (inconsistencies would show as negative delta)
- Metrics consistency checks

**Test Coverage**: Multi-instance and recovery tests

**Example**:
```erlang
%% After split-brain recovery, state is synchronized
%% All instances have consistent view of system state
%% No orphaned locks or transactions
```

### G4: Latency Bounds

**Requirement**: Operations complete within acceptable latency bounds.

**Details**:
- Operations should complete within configured latency bounds
- Latency degradation is acceptable but bounded
- Timeouts are handled gracefully

**Verification**:
- Latency metric: `router_nats_operation_latency_seconds`
- Timeout metric: `router_nats_timeout_total`
- Max latency: Configurable per test (e.g., 6s for latency degradation tests)

**Test Coverage**: Latency degradation and slow network tests

**Example**:
```erlang
%% During latency degradation, operations may take longer
%% But they complete within max_latency_ms bounds
%% Timeouts are logged but don't cause message loss
```

### G5: Packet Loss Tolerance

**Requirement**: System handles packet loss gracefully.

**Details**:
- Lost packets are retried until successful or MaxDeliver exhausted
- Retry attempts are within reasonable bounds
- Packet loss doesn't cause unbounded retries

**Verification**:
- Retry attempts: `router_nats_retry_attempts_total`
- Publish failures: `router_nats_publish_failures_total`
- Max expected retries: Configurable per test (e.g., 1000 for 30% packet loss)

**Test Coverage**: Partial packet loss and flapping network with packet loss tests

**Example**:
```erlang
%% With 30% packet loss, some packets are lost
%% Lost packets are retried until successful
%% Retry attempts are within reasonable bounds (e.g., < 1000)
```

## Contract Invariants

### I1: Fail-Open Behavior

**Requirement**: Router process remains alive during network partitions.

**Verification**:
- Process liveness check: `is_process_alive(RouterPid)`
- No crashes or unhandled exceptions
- Process continues operating in degraded mode

**Test Coverage**: All network partition tests

**Example**:
```erlang
%% During partition, Router process remains alive
true = is_process_alive(RouterPid),
%% Process continues operating in degraded mode
```

### I2: MaxDeliver Semantics

**Requirement**: Messages either deliver successfully or exhaust MaxDeliver.

**Details**:
- Messages don't exceed MaxDeliver count
- MaxDeliver exhaustion is tracked and logged
- Exhausted messages are not retried indefinitely

**Verification**:
- MaxDeliver exhausted metric: `router_jetstream_maxdeliver_exhausted_total`
- Delivery count tracking: ETS table consistency
- Tolerance: Configurable (default: 2)

**Test Coverage**: All network partition tests (via `verify_maxdeliver_semantics/3`)

**Example**:
```erlang
%% Message M1 is retried 3 times (MaxDeliver = 3)
%% After 3 retries, MaxDeliver is exhausted
%% Message is not retried again
%% MaxDeliver exhausted metric increments
```

### I3: Redelivery Limits

**Requirement**: Redelivery count ≤ MaxRedelivery (default: 50).

**Details**:
- Redelivery count is tracked per message
- Redelivery doesn't exceed MaxRedelivery
- Redelivery is logged and monitored

**Verification**:
- Redelivery metric: `router_jetstream_redelivery_total`
- Redelivery count validation
- Max redelivery: Configurable per test (default: 50)

**Test Coverage**: All network partition tests (via `verify_redelivery_limits/3`)

**Example**:
```erlang
%% Message M1 is redelivered 5 times
%% Redelivery count = 5 < MaxRedelivery (50)
%% Redelivery metric increments by 5
```

### I4: Metrics Correctness

**Requirement**: Metrics reflect actual partition state.

**Details**:
- Error metrics increase during partition
- Recovery metrics reflect recovery
- Metrics snapshot comparison: Initial → Partition → Final

**Verification**:
- Error metrics: `router_nats_connection_failures_total`, `router_nats_publish_failures_total`
- Recovery metrics: `router_nats_connection_restored_total`
- Metrics snapshot comparison

**Test Coverage**: All network partition tests (via `verify_metrics_correctness/3`)

**Example**:
```erlang
%% During partition, connection_failures_total increases
%% After recovery, connection_restored_total increases
%% Metrics reflect actual partition state
```

### I5: Data Guarantees

**Requirement**: No duplicates, losses, or inconsistencies during and after partitions.

**Details**:
- No duplicate processing (idempotency layer)
- No message loss (within guarantees)
- No state inconsistencies

**Verification**:
- Duplicate processing: `router_duplicate_processing_total`
- MaxDeliver exhaustion: `router_jetstream_maxdeliver_exhausted_total`
- State consistency: `router_state_consistency_total`

**Test Coverage**: All network partition tests (via `verify_data_guarantees/3`)

**Example**:
```erlang
%% During split-brain, no duplicate processing
%% After recovery, no message loss
%% State remains consistent
```

### I6: Latency Bounds

**Requirement**: Operations complete within acceptable latency bounds.

**Details**:
- Latency is bounded (configurable per test)
- Timeouts are handled gracefully
- Latency degradation is acceptable but bounded

**Verification**:
- Latency metric: `router_nats_operation_latency_seconds`
- Max latency: Configurable per test (e.g., 6s for latency degradation)
- Tolerance: Configurable (default: 1000ms)

**Test Coverage**: Latency degradation and slow network tests (via `verify_latency_bounds/3`)

**Example**:
```erlang
%% During latency degradation, operations take longer
%% But they complete within max_latency_ms bounds (e.g., 6s)
%% Timeouts are logged but don't cause message loss
```

### I7: Packet Loss Tolerance

**Requirement**: System handles packet loss gracefully.

**Details**:
- Lost packets are retried until successful
- Retry attempts are within reasonable bounds
- Packet loss doesn't cause unbounded retries

**Verification**:
- Retry attempts: `router_nats_retry_attempts_total`
- Publish failures: `router_nats_publish_failures_total`
- Max expected retries: Configurable per test

**Test Coverage**: Partial packet loss and flapping network with packet loss tests (via `verify_packet_loss_tolerance/3`)

**Example**:
```erlang
%% With 30% packet loss, some packets are lost
%% Lost packets are retried until successful
%% Retry attempts are within reasonable bounds (e.g., < 1000)
```

## Verification Functions

### verify_network_partition_contracts/3

**Purpose**: Main verification function that checks all contract invariants.

**Checks**:
1. MaxDeliver semantics (`verify_maxdeliver_semantics/3`)
2. Redelivery limits (`verify_redelivery_limits/3`)
3. Metrics correctness (`verify_metrics_correctness/3`)
4. Data guarantees (`verify_data_guarantees/3`)
5. Latency bounds (`verify_latency_bounds/3`)
6. Packet loss tolerance (`verify_packet_loss_tolerance/3`)

**Usage**:
```erlang
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50,
    max_latency_ms => 6000,
    packet_loss_percent => 30,
    allow_retries => true
}).
```

### verify_data_guarantees/3

**Purpose**: Verify data guarantees (no duplicates, losses, inconsistencies).

**Checks**:
- Duplicate processing: `router_duplicate_processing_total`
- MaxDeliver exhaustion: `router_jetstream_maxdeliver_exhausted_total`
- State consistency: `router_state_consistency_total`

**Parameters**:
- `max_allowed_duplicates`: Maximum allowed duplicates (default: 0)

### verify_latency_bounds/3

**Purpose**: Verify latency bounds.

**Checks**:
- Latency metric: `router_nats_operation_latency_seconds`
- Max latency: Configurable per test
- Tolerance: Configurable (default: 1000ms)

**Parameters**:
- `max_latency_ms`: Maximum allowed latency in milliseconds
- `latency_tolerance_ms`: Tolerance for latency checks (default: 1000ms)

### verify_packet_loss_tolerance/3

**Purpose**: Verify packet loss tolerance.

**Checks**:
- Retry attempts: `router_nats_retry_attempts_total`
- Publish failures: `router_nats_publish_failures_total`
- Max expected retries: Configurable per test

**Parameters**:
- `packet_loss_percent`: Packet loss percentage
- `allow_retries`: Whether retries are expected and allowed (default: false)
- `max_expected_retries`: Maximum expected retries (default: 1000)
- `max_expected_failures`: Maximum expected failures (default: 500)

## Test Coverage

### Single-Instance Tests

All single-instance tests verify:
- ✅ I1: Fail-open behavior
- ✅ I2: MaxDeliver semantics
- ✅ I3: Redelivery limits
- ✅ I4: Metrics correctness
- ✅ I5: Data guarantees

**New Tests**:
- `test_single_instance_latency_degradation`: I6 (Latency bounds)
- `test_single_instance_partial_packet_loss`: I7 (Packet loss tolerance)
- `test_single_instance_intermittent_connectivity`: I1, I2, I3, I4, I5
- `test_single_instance_slow_network`: I6 (Latency bounds)

### Multi-Instance Tests

All multi-instance tests verify:
- ✅ I1: Fail-open behavior
- ✅ I2: MaxDeliver semantics
- ✅ I3: Redelivery limits
- ✅ I4: Metrics correctness
- ✅ I5: Data guarantees (especially no duplicates during split-brain)

### Flapping Network Tests

All flapping network tests verify:
- ✅ I1: Fail-open behavior
- ✅ I2: MaxDeliver semantics
- ✅ I3: Redelivery limits
- ✅ I4: Metrics correctness
- ✅ I5: Data guarantees

**New Tests**:
- `test_flapping_network_with_latency`: I6 (Latency bounds)
- `test_flapping_network_with_packet_loss`: I7 (Packet loss tolerance)

## Summary

**Data Guarantees**:
1. ✅ No message loss
2. ✅ No duplicate processing
3. ✅ No state inconsistencies
4. ✅ Latency bounds
5. ✅ Packet loss tolerance

**Contract Invariants**:
1. ✅ Fail-open behavior
2. ✅ MaxDeliver semantics
3. ✅ Redelivery limits
4. ✅ Metrics correctness
5. ✅ Data guarantees
6. ✅ Latency bounds
7. ✅ Packet loss tolerance

**Verification Functions**:
- `verify_network_partition_contracts/3` - Main verification function
- `verify_data_guarantees/3` - Data guarantees verification
- `verify_latency_bounds/3` - Latency bounds verification
- `verify_packet_loss_tolerance/3` - Packet loss tolerance verification

**Test Coverage**: All 31 test cases (26 existing + 5 new) verify these guarantees and invariants.

