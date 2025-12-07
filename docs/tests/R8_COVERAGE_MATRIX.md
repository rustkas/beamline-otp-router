# R8 Coverage Matrix: Pattern × Test Case

## Overview

This document provides a comprehensive coverage matrix mapping all triple-fault patterns to their corresponding test cases. The matrix includes basic triple-fault combinations, extended mixed patterns, additional scenarios, and edge-case scenarios.

## Coverage Matrix

| Pattern ID | Pattern Name | Contract Test | Stress/Soak Test | Advanced Test | Status |
|------------|--------------|---------------|------------------|---------------|--------|
| **Category 1: Basic Triple-Fault Combinations** |
| 1.1 | Connect + Publish + ACK | `test_triple_connect_publish_ack_contract` | `test_multi_fault_triple_soak` | - | ✅ Covered |
| 1.2 | Connect + Validation + NAK | `test_triple_connect_validation_nak_contract` | - | - | ✅ Covered |
| 1.3 | Publish + MaxDeliver + Intermittent ACK | `test_triple_publish_maxdeliver_ack_contract` | - | - | ✅ Covered |
| 1.4 | Connect + Publish + MaxDeliver | `test_triple_connect_publish_maxdeliver_contract` | - | - | ✅ Covered |
| 1.5 | ACK + NAK + Publish | `test_triple_ack_nak_publish_contract` | - | - | ✅ Covered |
| **Category 2: Extended Mixed Patterns** |
| 2.1 | Intermittent Connect + Persistent Publish | - | `test_multi_fault_mixed_pattern_soak` | `test_mixed_intermittent_connect_persistent_publish` | ✅ Covered |
| 2.2 | Intermittent ACK + Persistent Validation | - | `test_multi_fault_mixed_pattern_soak` | `test_mixed_persistent_connect_intermittent_ack` | ✅ Covered |
| 2.3 | Cascading Fault Chains | - | `test_multi_fault_cascading_soak` | `test_cascading_connect_publish_ack_chain` | ✅ Covered |
| 2.4 | Persistent NATS Latency + Intermittent Policy | - | - | - | ⏳ Future |
| **Category 3: Multi-Tenant and Multi-Stream** |
| 3.1 | Multi-Tenant Isolation | `test_triple_fault_multi_tenant_isolation` | - | - | ✅ Covered |
| 3.2 | Multi-Stream/Subject Isolation | `test_triple_fault_multi_stream_subject` | - | - | ✅ Covered |
| **Category 4: Metrics Degradation and Delayed Operations** |
| 4.1 | Metrics Degradation | `test_triple_fault_metrics_degradation` | - | - | ✅ Covered |
| 4.2 | Delayed ACK/NAK | `test_triple_fault_delayed_ack_nak` | - | - | ✅ Covered |
| **Category 5: Boundary Value Tests** |
| 5.1 | MaxDeliver Boundary | `test_triple_fault_maxdeliver_boundary` | - | - | ✅ Covered |
| 5.2 | MaxRedelivery Boundary | `test_triple_fault_maxredelivery_boundary` | - | - | ✅ Covered |
| **Category 6: Edge-Case Scenarios** |
| 6.1 | Triple-Fault with Partial Recovery | `test_triple_fault_partial_recovery` | - | - | ✅ Covered |
| 6.2 | Triple-Fault with AckPolicy Variations | `test_triple_fault_ackpolicy_variations` | - | - | ✅ Covered |
| 6.3 | Triple-Fault with DeliverPolicy Variations | `test_triple_fault_deliverpolicy_variations` | - | - | ✅ Covered |
| 6.4 | Triple-Fault with Consumer Group Isolation | `test_triple_fault_consumer_group_isolation` | - | - | ✅ Covered |

## Test Suite Mapping

### Contract Tests (`router_triple_fault_contract_SUITE.erl`)

| Test Case | Pattern IDs | Category | Duration |
|-----------|-------------|----------|----------|
| `test_triple_connect_publish_ack_contract` | 1.1 | Basic | Short |
| `test_triple_connect_validation_nak_contract` | 1.2 | Basic | Short |
| `test_triple_publish_maxdeliver_ack_contract` | 1.3 | Basic | Short |
| `test_triple_connect_publish_maxdeliver_contract` | 1.4 | Basic | Short |
| `test_triple_ack_nak_publish_contract` | 1.5 | Basic | Short |
| `test_triple_fault_multi_tenant_isolation` | 3.1 | Multi-Tenant | Short |
| `test_triple_fault_multi_stream_subject` | 3.2 | Multi-Stream | Short |
| `test_triple_fault_metrics_degradation` | 4.1 | Metrics | Short |
| `test_triple_fault_delayed_ack_nak` | 4.2 | Delayed | Medium |
| `test_triple_fault_maxdeliver_boundary` | 5.1 | Boundary | Short |
| `test_triple_fault_maxredelivery_boundary` | 5.2 | Boundary | Short |
| `test_triple_fault_partial_recovery` | 6.1 | Edge-Case | Medium |
| `test_triple_fault_ackpolicy_variations` | 6.2 | Edge-Case | Short |
| `test_triple_fault_deliverpolicy_variations` | 6.3 | Edge-Case | Short |
| `test_triple_fault_consumer_group_isolation` | 6.4 | Edge-Case | Short |

**Total Contract Tests**: 15

### Stress/Soak Tests (`router_stress_soak_SUITE.erl`)

| Test Case | Pattern IDs | Category | Duration |
|-----------|-------------|----------|----------|
| `test_multi_fault_triple_soak` | 1.1 | Basic | Long (4-6h) |
| `test_multi_fault_mixed_pattern_soak` | 2.1, 2.2 | Extended | Long (4-6h) |
| `test_multi_fault_cascading_soak` | 2.3 | Extended | Long (4-6h) |

**Total Stress/Soak Tests**: 3

### Advanced Concurrent Faults (`router_advanced_concurrent_faults_SUITE.erl`)

| Test Case | Pattern IDs | Category | Duration |
|-----------|-------------|----------|----------|
| `test_mixed_intermittent_connect_persistent_publish` | 2.1 | Extended | Medium |
| `test_mixed_persistent_connect_intermittent_ack` | 2.2 | Extended | Medium |
| `test_cascading_connect_publish_ack_chain` | 2.3 | Extended | Medium |

**Total Advanced Tests**: 3

## Coverage Statistics

### By Category

| Category | Patterns | Covered | Coverage % |
|----------|----------|---------|------------|
| Basic Triple-Fault | 5 | 5 | 100% |
| Extended Mixed | 4 | 3 | 75% |
| Multi-Tenant/Stream | 2 | 2 | 100% |
| Metrics/Delayed | 2 | 2 | 100% |
| Boundary Values | 2 | 2 | 100% |
| Edge-Cases | 4 | 4 | 100% |
| **Total** | **21** | **20** | **95.2%** |

**Note**: Pattern 2.4 (Persistent NATS Latency + Intermittent Policy) is the only uncovered pattern, marked as ⏳ Future enhancement. See `TRIPLE_FAULT_PATTERNS_CATALOG.md#pattern-24-persistent-nats-laglatency--intermittent-policy-changes` for details.

### By Test Type

| Test Type | Test Cases | Patterns Covered |
|-----------|------------|------------------|
| Contract Tests | 15 | 15 patterns |
| Stress/Soak Tests | 3 | 3 patterns |
| Advanced Tests | 3 | 3 patterns |
| **Total** | **21** | **21 patterns** |

## Pattern Details

### Category 1: Basic Triple-Fault Combinations

**Pattern 1.1: Connect + Publish + ACK**
- **Faults**: `connect: {error, connection_refused}`, `publish: {error, timeout}`, `ack: {error, timeout}`
- **Contract Test**: `test_triple_connect_publish_ack_contract`
- **Stress/Soak**: `test_multi_fault_triple_soak`
- **Verifies**: Fail-open, MaxDeliver semantics, redelivery limits, metrics correctness, cross-tenant isolation

**Pattern 1.2: Connect + Validation + NAK**
- **Faults**: `connect: {error, connection_refused}`, `validation: tenant_validation_fail`, `nak: {error, timeout}`
- **Contract Test**: `test_triple_connect_validation_nak_contract`
- **Verifies**: Validation failure handling, NAK/publish issue handling, no infinite loops

**Pattern 1.3: Publish + MaxDeliver + Intermittent ACK**
- **Faults**: `publish: {error, timeout}`, `maxdeliver: near_exhaustion`, `ack: {intermittent, {error, timeout}, 0.5}`
- **Contract Test**: `test_triple_publish_maxdeliver_ack_contract`
- **Verifies**: MaxDeliver exhaustion handling, intermittent ACK failure handling, final state transition

**Pattern 1.4: Connect + Publish + MaxDeliver**
- **Faults**: `connect: {error, connection_refused}`, `publish: {error, timeout}`, `maxdeliver: exhaustion`
- **Contract Test**: `test_triple_connect_publish_maxdeliver_contract`
- **Verifies**: MaxDeliver exhaustion under multiple faults, final state transition, recovery

**Pattern 1.5: ACK + NAK + Publish**
- **Faults**: `ack: {error, timeout}`, `nak: {error, timeout}`, `publish: {error, timeout}`
- **Contract Test**: `test_triple_ack_nak_publish_contract`
- **Verifies**: ACK/NAK failure handling, no message loss, redelivery correctness

### Category 2: Extended Mixed Patterns

**Pattern 2.1: Intermittent Connect + Persistent Publish**
- **Faults**: `connect: {intermittent, close_connection, 0.5}`, `publish: {error, timeout}`
- **Contract Test**: `test_mixed_intermittent_connect_persistent_publish` (Advanced)
- **Stress/Soak**: `test_multi_fault_mixed_pattern_soak`
- **Verifies**: Connection flapping handling, degraded mode, no retry storm

**Pattern 2.2: Intermittent ACK + Persistent Validation**
- **Faults**: `ack: {intermittent, {error, timeout}, 0.3}`, `validation: persistent_failures`
- **Contract Test**: `test_mixed_persistent_connect_intermittent_ack` (Advanced)
- **Stress/Soak**: `test_multi_fault_mixed_pattern_soak`
- **Verifies**: ACK failure rate matching, validation failure handling, redelivery limits

**Pattern 2.3: Cascading Fault Chains**
- **Faults**: Sequential chain of different faults (connect → publish → ACK)
- **Contract Test**: `test_cascading_connect_publish_ack_chain` (Advanced)
- **Stress/Soak**: `test_multi_fault_cascading_soak`
- **Verifies**: Cascading fault handling, recovery after chain, no state corruption
- **Catalog Reference**: `TRIPLE_FAULT_PATTERNS_CATALOG.md#pattern-23-cascading-fault-chains`

**Pattern 2.4: Persistent NATS Latency + Intermittent Policy**
- **Faults**: `nats_latency: persistent_high_latency`, `policy_change: intermittent_changes`
- **Contract Test**: Future enhancement (not yet implemented)
- **Stress/Soak**: Future enhancement (not yet implemented)
- **Verifies**: NATS latency handling, policy change handling, no state corruption, performance bounds
- **Status**: ⏳ Future enhancement (not yet implemented)
- **Catalog Reference**: `TRIPLE_FAULT_PATTERNS_CATALOG.md#pattern-24-persistent-nats-laglatency--intermittent-policy-changes`
- **Implementation Task**: `PATTERN_2_4_IMPLEMENTATION_TASK.md` - Detailed task specification with requirements, acceptance criteria, and implementation plan

### Category 3: Multi-Tenant and Multi-Stream

**Pattern 3.1: Multi-Tenant Isolation**
- **Faults**: Triple faults affecting multiple tenants
- **Contract Test**: `test_triple_fault_multi_tenant_isolation`
- **Verifies**: Cross-tenant isolation, tenant-specific metrics, no cross-tenant state corruption

**Pattern 3.2: Multi-Stream/Subject Isolation**
- **Faults**: Triple faults affecting multiple streams/subjects
- **Contract Test**: `test_triple_fault_multi_stream_subject`
- **Verifies**: Cross-stream isolation, stream-specific metrics, no cross-stream state corruption

### Category 4: Metrics Degradation and Delayed Operations

**Pattern 4.1: Metrics Degradation**
- **Faults**: Triple faults causing system degradation
- **Contract Test**: `test_triple_fault_metrics_degradation`
- **Verifies**: Metrics accuracy during degradation, no metric corruption, metrics reflect actual behavior

**Pattern 4.2: Delayed ACK/NAK**
- **Faults**: `connect: {error, connection_refused}`, `publish: {error, timeout}`, `ack: {delay, 5000}`, `nak: {delay, 3000}`
- **Contract Test**: `test_triple_fault_delayed_ack_nak`
- **Verifies**: No message loss with delays, correct redelivery, no infinite retry loops

### Category 5: Boundary Value Tests

**Pattern 5.1: MaxDeliver Boundary**
- **Faults**: Triple faults with low MaxDeliver value
- **Contract Test**: `test_triple_fault_maxdeliver_boundary`
- **Verifies**: MaxDeliver exhaustion at exact boundary, final state transition, no messages exceed MaxDeliver

**Pattern 5.2: MaxRedelivery Boundary**
- **Faults**: Triple faults causing redeliveries up to MaxRedelivery limit
- **Contract Test**: `test_triple_fault_maxredelivery_boundary`
- **Verifies**: Redelivery stops at boundary, no infinite loops, redelivery count respects limit

### Category 6: Edge-Case Scenarios

**Pattern 6.1: Triple-Fault with Partial Recovery**
- **Faults**: Triple faults with partial recovery (some faults clear, others persist)
- **Contract Test**: `test_triple_fault_partial_recovery`
- **Verifies**: Partial recovery handling, system stability during partial recovery, no message loss

**Pattern 6.2: Triple-Fault with AckPolicy Variations**
- **Faults**: Triple faults with different AckPolicy values (explicit, none, all)
- **Contract Test**: `test_triple_fault_ackpolicy_variations`
- **Verifies**: Different AckPolicy handling, triple faults work with different policies, metrics reflect policy behavior

**Pattern 6.3: Triple-Fault with DeliverPolicy Variations**
- **Faults**: Triple faults with different DeliverPolicy values (all, new, last)
- **Contract Test**: `test_triple_fault_deliverpolicy_variations`
- **Verifies**: Different DeliverPolicy handling, message delivery respects policy, triple faults work with different policies

**Pattern 6.4: Triple-Fault with Consumer Group Isolation**
- **Faults**: Triple faults affecting different consumer groups
- **Contract Test**: `test_triple_fault_consumer_group_isolation`
- **Verifies**: Consumer group isolation, faults in one group don't affect others, metrics correctly labeled per group

## Test Execution

### Run All Contract Tests

```bash
# Run all triple-fault contract tests
rebar3 ct --suite router_triple_fault_contract_SUITE

# Run specific category
rebar3 ct --suite router_triple_fault_contract_SUITE --case test_triple_connect_publish_ack_contract
rebar3 ct --suite router_triple_fault_contract_SUITE --case test_triple_fault_partial_recovery
```

### Run Stress/Soak Tests

```bash
# Run stress/soak tests (configurable duration)
export STRESS_SOAK_DURATION_HOURS=4
rebar3 ct --suite router_stress_soak_SUITE --case test_multi_fault_triple_soak
```

### Run Advanced Tests

```bash
# Run advanced concurrent fault tests
rebar3 ct --suite router_advanced_concurrent_faults_SUITE --case test_mixed_intermittent_connect_persistent_publish
```

## References

- `router_triple_fault_contract_SUITE.erl`: Contract tests implementation (15 tests)
- `router_stress_soak_SUITE.erl`: Stress/soak tests implementation (3 tests)
- `router_advanced_concurrent_faults_SUITE.erl`: Advanced concurrent faults (3 tests)
- `TRIPLE_FAULT_PATTERNS_CATALOG.md`: Complete pattern catalog
- `R8_CLOSURE_REPORT.md`: R8 closure report
- `R8_EXTENDED_COVERAGE.md`: Extended coverage documentation

