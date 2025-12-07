# R12: New Scenarios Added

**Date**: 2025-11-30  
**Status**: ✅ **Complete**  
**Purpose**: Summary of new network partition scenarios added to R12 test suite

## Summary

Added **5 new test cases** covering:
- Latency degradation
- Partial packet loss
- Intermittent connectivity
- Slow network
- Flapping network with latency
- Flapping network with packet loss

**Total Test Cases**: 31 (26 existing + 5 new)

## New Test Cases

### 1. Latency Degradation

**Test Case**: `test_single_instance_latency_degradation` (Line 1722)

**Scenario**: Network latency increases gradually (100ms → 500ms → 2000ms → 5000ms), causing timeouts and retries.

**Data Guarantees**:
- ✅ No message loss: All messages eventually processed or exhausted MaxDeliver
- ✅ No duplicates: Idempotency layer prevents duplicate processing
- ✅ Latency bounds: Operations complete within acceptable latency bounds (max 6s)

**Verification**:
- Timeout errors increase
- Latency metrics reflect degradation
- Contract invariants verified (including latency bounds)

### 2. Partial Packet Loss

**Test Case**: `test_single_instance_partial_packet_loss` (Line 1777)

**Scenario**: 30% packet loss on publish and ack operations, causing retries.

**Data Guarantees**:
- ✅ No message loss: Lost packets are retried until successful or MaxDeliver exhausted
- ✅ No duplicates: Idempotency layer prevents duplicate processing
- ✅ Packet loss tolerance: System handles partial loss gracefully

**Verification**:
- Retry attempts increase
- Publish failures increase
- Contract invariants verified (including packet loss tolerance)

### 3. Intermittent Connectivity

**Test Case**: `test_single_instance_intermittent_connectivity` (Line 1832)

**Scenario**: Connection alternates between available (2s) and unavailable (1s), repeating for 60 seconds.

**Data Guarantees**:
- ✅ No message loss: Messages are queued during disconnects and processed after reconnect
- ✅ No duplicates: Idempotency layer prevents duplicate processing
- ✅ Intermittent tolerance: System handles intermittent connectivity gracefully

**Verification**:
- Connection failures and restorations tracked
- Contract invariants verified

### 4. Slow Network

**Test Case**: `test_single_instance_slow_network` (Line 1900)

**Scenario**: Network is slow but not completely disconnected (3s delay on all operations).

**Data Guarantees**:
- ✅ No message loss: All messages eventually processed
- ✅ Latency bounds: Operations complete within acceptable bounds (max 4s)
- ✅ Throughput degradation: System handles slow network gracefully

**Verification**:
- Latency metrics reflect slow network
- Contract invariants verified (including latency bounds)

### 5. Flapping Network with Latency

**Test Case**: `test_flapping_network_with_latency` (Line 1951)

**Scenario**: Network flaps (disconnects for 1s, connects with 2s latency for 2s), repeating for 90 seconds.

**Data Guarantees**:
- ✅ No message loss: Messages are queued and processed after stable connection
- ✅ Latency bounds: Operations complete within acceptable bounds even with flapping (max 3s)
- ✅ Stability: System remains stable despite flapping and latency

**Verification**:
- No resource leaks (memory, processes)
- Contract invariants verified (including latency bounds)

### 6. Flapping Network with Packet Loss

**Test Case**: `test_flapping_network_with_packet_loss` (Line 2029)

**Scenario**: Network flaps (disconnects for 1s, connects with 20% packet loss for 2s), repeating for 90 seconds.

**Data Guarantees**:
- ✅ No message loss: Lost packets are retried until successful
- ✅ Packet loss tolerance: System handles packet loss gracefully
- ✅ Stability: System remains stable despite flapping and packet loss

**Verification**:
- Retry attempts increase due to packet loss
- No resource leaks (memory, processes)
- Contract invariants verified (including packet loss tolerance)

## Enhanced Verification Functions

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

## Updated Test Groups

### Single-Instance Tests

**Updated**: Added 4 new tests:
- `test_single_instance_latency_degradation`
- `test_single_instance_partial_packet_loss`
- `test_single_instance_intermittent_connectivity`
- `test_single_instance_slow_network`

**Total**: 13 tests (9 existing + 4 new)

### Flapping Network Tests

**Updated**: Added 2 new tests:
- `test_flapping_network_with_latency`
- `test_flapping_network_with_packet_loss`

**Total**: 5 tests (3 existing + 2 new)

## Updated Verification Function

### verify_network_partition_contracts/3

**Enhanced**: Now calls 6 verification functions:
1. `verify_maxdeliver_semantics/3` - MaxDeliver semantics
2. `verify_redelivery_limits/3` - Redelivery limits
3. `verify_metrics_correctness/3` - Metrics correctness
4. `verify_data_guarantees/3` - Data guarantees (NEW)
5. `verify_latency_bounds/3` - Latency bounds (NEW)
6. `verify_packet_loss_tolerance/3` - Packet loss tolerance (NEW)

## Test Coverage Summary

**Total Test Cases**: 31
- Single-instance: 13 tests
- Multi-instance: 11 tests
- Service-broker: 3 tests
- Flapping network: 5 tests

**All Tests Verify**:
- ✅ I1: Fail-open behavior
- ✅ I2: MaxDeliver semantics
- ✅ I3: Redelivery limits
- ✅ I4: Metrics correctness
- ✅ I5: Data guarantees
- ✅ I6: Latency bounds (new tests)
- ✅ I7: Packet loss tolerance (new tests)

## Documentation

**New Documents**:
- `R12_DATA_GUARANTEES_AND_INVARIANTS.md` - Detailed specification of data guarantees and invariants
- `R12_NEW_SCENARIOS_ADDED.md` - This document

**Updated Documents**:
- `router_network_partition_SUITE.erl` - Added 5 new test cases and 3 new verification functions

## Next Steps

1. ✅ All new scenarios implemented
2. ✅ Enhanced verification functions added
3. ✅ Documentation updated
4. ⏳ Execute tests and verify results
5. ⏳ Update results report with actual test outcomes

**Status**: ✅ **Implementation Complete**

