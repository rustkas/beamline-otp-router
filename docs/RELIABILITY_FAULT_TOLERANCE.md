# Router Reliability & Fault Tolerance

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Comprehensive overview of Router reliability guarantees, fault tolerance mechanisms, and test coverage.

## Overview

Router implements comprehensive fault tolerance mechanisms to ensure reliable operation under various failure conditions. This document summarizes:

- **Reliability guarantees**: What the system promises under fault conditions
- **Fault tolerance mechanisms**: How the system handles failures
- **Test coverage**: What scenarios are tested and verified
- **Known limitations**: Consciously accepted risks

## Reliability Guarantees

### 1. Process Resilience

**Guarantee**: Router consumer processes (`router_result_consumer`, `router_decide_consumer`) remain alive during fault conditions.

**Mechanisms**:
- Supervisor-based process management
- Automatic restart on process crashes
- Graceful error handling (no unhandled exceptions)

**Test Coverage**:
- ✅ All concurrent fault tests verify `is_process_alive(ConsumerPid)`
- ✅ Consumer restart scenarios tested (`test_prolonged_faults_with_consumer_restart`)
- ✅ Extended soak tests verify no process leaks

**See**: `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for detailed test scenarios.

### 2. Message Semantics

**Guarantee**: Messages are not lost beyond contract-defined retry limits, and final states are deterministic.

**Mechanisms**:
- JetStream durable subscriptions with ACK/NAK
- MaxDeliver limits prevent infinite retries
- ETS-based delivery count tracking
- Idempotency layer prevents duplicate processing

**Test Coverage**:
- ✅ Concurrent fault scenarios (S1-S4) verify no message loss
- ✅ Final state verification (`test_final_state_and_idempotency_multi_retry`)
- ✅ MaxDeliver exhaustion scenarios tested
- ✅ Idempotency verified under concurrent faults

**See**: `test/FAULT_INJECTION_TEST_SCENARIOS.md` for detailed scenarios.

### 3. Tenant Isolation

**Guarantee**: Faults affecting one tenant do not block or degrade processing for other tenants.

**Mechanisms**:
- Per-tenant validation and processing
- Independent error handling per tenant
- Tenant-specific metrics and logging

**Test Coverage**:
- ✅ Tenant isolation stress tests (`test_tenant_isolation_during_concurrent_faults`)
- ✅ Concurrent faults with multiple tenants verified
- ✅ Metrics/logs correctly tagged by tenant

**See**: `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for tenant isolation scenarios.

### 4. Recovery Without Restart

**Guarantee**: System recovers from transient faults without requiring process restarts.

**Mechanisms**:
- Automatic retry with exponential backoff
- Connection resilience (NATS/JetStream reconnection)
- State preservation during faults (ETS tables)

**Test Coverage**:
- ✅ Prolonged fault period recovery (`test_prolonged_fault_period_recovery_no_router_restart`)
- ✅ Multiple recovery cycles (`test_multiple_fault_recovery_cycles`)
- ✅ NATS connection resilience (54 tests in NATS resilience suites)

**See**: `docs/NATS_CONNECTION_RESILIENCE.md` for NATS resilience details.

## Fault Tolerance Mechanisms

### Concurrent Fault Handling

**Scenarios Covered**:
1. **ACK errors + tenant validation failures** (S1, S2)
   - Message A: ACK fails, Message B: validation fails (concurrent)
   - Same message: ACK fails, then validation fails on retry
2. **NAK + publish failures** (S3, S4)
   - Publish fails → NAK → retry → success
   - Batch messages with mixed failures

**Mechanisms**:
- Parallel message processing with ETS synchronization
- Independent error handling per message
- Retry logic with MaxDeliver limits
- Comprehensive metrics and logging

**Test Coverage**: 8 tests per consumer (result + decide) = 16 concurrent fault tests

**See**: `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for detailed scenarios.

### Extended Fault Scenarios

**Scenarios Covered**:
1. **Consumer restart during faults**
   - Subscriptions restored after restart
   - Messages not lost
   - No double processing beyond retries
2. **Tenant isolation under concurrent faults**
   - Tenant A faults do not block Tenant B
   - Metrics/logs correctly isolated
3. **Final states and idempotency**
   - Each message reaches deterministic final state
   - Processing count does not exceed contract
4. **Comprehensive metrics/logging**
   - All metrics incremented correctly
   - No contradictory metrics
   - No duplicate metrics for same event

**Test Coverage**: 4 extended tests per consumer = 8 extended tests

**See**: `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for extended scenarios.

### NATS/JetStream Resilience

**Mechanisms**:
- Automatic reconnection with exponential backoff
- Message queueing during disconnections
- Fail-open mode (optional degraded operation)
- Comprehensive monitoring (metrics, logs)

**Test Coverage**: 54 tests across 4 test suites

**See**: `docs/NATS_CONNECTION_RESILIENCE.md` for complete documentation.

## Test Coverage Summary

### Concurrent Fault Tests

| Test Suite | Tests | Status | CI Pipeline |
|------------|-------|--------|-------------|
| `router_result_consumer_SUITE` | 8 concurrent fault tests | ✅ Stable | PR (standard) |
| `router_decide_consumer_SUITE` | 8 concurrent fault tests | ✅ Stable | PR (standard) |
| `router_concurrent_faults_stress_SUITE` | 3 stress tests | ✅ Stable | Nightly (optional) |

**Total**: 19 concurrent fault tests

### NATS Resilience Tests

| Test Suite | Tests | Status | CI Pipeline |
|------------|-------|--------|-------------|
| `router_nats_connection_failure_SUITE` | 22 | ✅ Stable | PR (standard) |
| `router_jetstream_fault_injection_SUITE` | 15 | ✅ Stable | PR (standard) |
| `router_nats_integration_SUITE` | 10 | ✅ Stable | PR (standard) |
| `router_nats_performance_SUITE` | 7 | ⚠️ Slow | Nightly/Extended |

**Total**: 54 NATS resilience tests

### Metrics Contract Tests

| Test Suite | Tests | Status | CI Pipeline |
|------------|-------|--------|-------------|
| `router_jetstream_fault_injection_SUITE` | 4 metric contract tests | ✅ Stable | PR (standard) |

**Total**: 4 metric contract tests

**See**: `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md` for metric contract details.

## Known Limitations and Accepted Risks

### 1. Message Loss During Process Crashes

**Risk**: Messages in-flight during process crash may be lost if not yet ACKed.

**Mitigation**:
- JetStream durable subscriptions ensure messages are redelivered after consumer restart
- MaxDeliver limits prevent infinite redelivery
- ETS state is preserved during NATS restarts (not process crashes)

**Acceptance**: This is a known limitation of at-least-once delivery semantics. Applications should be designed to handle duplicate messages (idempotency).

### 2. MaxDeliver Exhaustion

**Risk**: Messages that exceed MaxDeliver limit are dropped (not delivered to DLQ by default).

**Mitigation**:
- MaxDeliver exhaustion metric (`router_jetstream_maxdeliver_exhausted_total`) alerts operators
- Warning logs emitted on exhaustion
- MaxDeliver can be configured per stream/consumer

**Acceptance**: This is expected behavior per JetStream contract. Operators should monitor exhaustion metrics and adjust MaxDeliver or investigate root causes.

### 3. Tenant Validation Failures

**Risk**: Messages for invalid/blocked tenants are rejected (NAK) and may exhaust MaxDeliver.

**Mitigation**:
- Tenant rejection metrics (`router_results_tenant_rejected_total`) alert operators
- Rejection reasons logged for debugging
- Tenant validation can be configured per tenant

**Acceptance**: This is expected behavior. Invalid tenants should be fixed in configuration, not retried indefinitely.

### 4. Publish Failures

**Risk**: Usage events or result publications may fail due to NATS/JetStream issues.

**Mitigation**:
- Publish failure metrics (`router_usage_emit_failed_total`) alert operators
- Automatic retry with NAK for original message
- Connection resilience handles transient failures

**Acceptance**: Transient failures are retried. Persistent failures require operator intervention (check NATS connectivity, quotas).

## Observability Integration

### Metrics

**Key Metrics**:
- `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}` - Message redeliveries
- `router_jetstream_maxdeliver_exhausted_total{assignment_id,request_id,reason}` - MaxDeliver exhaustion
- `router_results_tenant_rejected_total{assignment_id,request_id,reason,tenant_id}` - Tenant rejections
- `router_usage_emit_failed_total{subject,error,tenant_id}` - Publish failures

**See**: `docs/PROMETHEUS_ALERTS.md` for alert definitions.

### Logs

**Key Log Events**:
- `NATS_CONNECTION_*` - Connection events
- `NATS_PUBLISH_*` - Publish events
- `NATS_ACK_*` - ACK events
- `TENANT_VALIDATION_*` - Tenant validation events

**See**: `docs/NATS_CONNECTION_RESILIENCE.md` for complete log format.

### Alerts

**Critical Alerts**:
- `RouterJetStreamMaxDeliverExhausted` - MaxDeliver exhausted (> 0/min for 5m)
- `RouterPublishFailuresBurst` - High publish failure rate

**Warning Alerts**:
- `RouterJetStreamRedeliveryHigh` - High redelivery rate (> 20/min for 10m)
- `RouterTenantRejectionHigh` - High tenant rejection rate

**See**: `docs/PROMETHEUS_ALERTS.md` for complete alert definitions.

## Production Recommendations

### Configuration

**MaxDeliver**: Configure per stream/consumer based on retry requirements (default: 3)

**Reconnection**: 
- Attempts: 10 (sufficient for most network issues)
- Delay: 1000ms base, 30000ms max (prevents tight loops)

**Fail-Open Mode**: `false` (ensure message delivery, not silent loss)

**Queue Size**: 5000 (high traffic support, prevents memory exhaustion)

**See**: `docs/OPERATIONAL_GUIDE.md` for complete configuration recommendations.

### Monitoring

**Required Dashboards**:
- Redelivery rate by tenant/reason
- MaxDeliver exhaustion rate
- Tenant rejection rate by reason
- Publish failure rate by subject

**Required Alerts**:
- MaxDeliver exhaustion (critical)
- High redelivery rate (warning)
- High tenant rejection rate (warning)
- Publish failure burst (critical)

**See**: `docs/PROMETHEUS_ALERTS.md` for alert definitions and `docs/OBSERVABILITY_DASHBOARD.md` for dashboard queries.

## References

- **Test Documentation**: 
  - `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` - Concurrent fault scenarios
  - `test/FAULT_INJECTION_TEST_SCENARIOS.md` - General fault injection scenarios
  - `test/CONCURRENT_FAULTS_CI_STABILITY.md` - CI stability verification
- **Metrics Contract**: `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md`
- **NATS Resilience**: `docs/NATS_CONNECTION_RESILIENCE.md`
- **Operational Guide**: `docs/OPERATIONAL_GUIDE.md`
- **Prometheus Alerts**: `docs/PROMETHEUS_ALERTS.md`

## Change History

**v1.0 (2025-11-30)**:
- Initial reliability and fault tolerance documentation
- Integration of concurrent faults test coverage
- Known limitations and accepted risks documented
- Observability integration summarized

