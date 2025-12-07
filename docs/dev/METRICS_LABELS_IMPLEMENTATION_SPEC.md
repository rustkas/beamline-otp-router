# Metrics Labels Implementation Specification

**Date**: 2025-11-30  
**Status**: Specification Complete  
**Purpose**: Specification for adding labels to Router metrics to enable detailed observability breakdowns

## Purpose

This document specifies the required labels for Router metrics to enable detailed observability breakdowns in dashboards and alerts. Labels are critical for:
- Identifying problematic assignments, tenants, or sources
- Root cause analysis during incidents
- Capacity planning and performance optimization

## Priority Classification

### Priority 1: Critical (Immediate Implementation Required)

These labels are required for existing dashboard panels to function correctly:

1. **`router_dlq_total`** - Required for DLQ breakdown panels (JS-003)
2. **`router_nats_*_failures_total`** - Required for NATS failure breakdown panels (NATS-001, NATS-004, NATS-005)

### Priority 2: Important (Short-term Implementation)

These labels improve observability but are not blocking existing panels:

1. **`router_nats_connect_failures_total`** - Improves connection failure analysis
2. **`router_nats_reconnect_failures_total`** - Improves reconnection failure analysis
3. **`router_nats_publish_failures_total`** - Improves publish failure analysis

## Label Specifications

### 1. `router_dlq_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 1 (Critical)  
**Scenario**: JS-003 (DLQ Growth)  
**Coverage Analysis**: `apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#missing-labels-on-dlq-metric`

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `assignment_id` | string | Yes | Assignment identifier | `"decide"`, `"result"`, `"ack"` |
| `tenant_id` | string | Optional | Tenant identifier | `"tenant-123"` |
| `reason` | string | Yes | Reason for DLQ | `"maxdeliver_exhausted"`, `"payload_invalid"`, `"unsupported_version"` |
| `source` | string | Optional | Source of message | `"tenant_validation"`, `"processing_timeout"` |
| `msg_id` | string | Optional | Message identifier | `"msg-abc123"` |
| `request_id` | string | Optional | Request correlation ID | `"req-xyz789"` |

**Implementation Notes**:
- `assignment_id` and `reason` are **required** for DLQ breakdown panels
- `tenant_id` enables tenant-specific DLQ analysis
- `msg_id` and `request_id` enable log correlation

**Code Location**: `apps/otp/router/src/router_jetstream.erl` (DLQ emission)

**Example Implementation**:
```erlang
%% Current (no labels)
router_metrics:inc(router_dlq_total).

%% Target (with labels)
router_metrics:inc(router_dlq_total, [
    {assignment_id, AssignmentId},
    {reason, Reason},
    {tenant_id, TenantId},  %% if available
    {source, Source},       %% if available
    {msg_id, MsgId},       %% if available
    {request_id, RequestId} %% if available
]).
```

**Migration Considerations**:
- Existing metric values will be reset (counter reset)
- Dashboard queries need to be updated to use labels
- Alert rules may need adjustment if using label filters

### 2. `router_nats_connect_failures_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 2 (Important)  
**Scenario**: NATS-001 (NATS Connection Failures)

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `reason` | string | Yes | Failure reason | `"dns_error"`, `"auth_failed"`, `"timeout"`, `"connection_refused"` |
| `cluster` | string | Optional | NATS cluster identifier | `"nats-cluster-1"` |
| `source` | string | Optional | Connection source | `"primary"`, `"fallback"` |

**Implementation Notes**:
- `reason` is **required** for failure breakdown analysis
- `cluster` enables multi-cluster failure analysis
- `source` enables primary/fallback connection analysis

**Code Location**: `apps/otp/router/src/router_nats.erl` (connection failure handling)

**Example Implementation**:
```erlang
%% Current (no labels)
router_metrics:inc(router_nats_connect_failures_total).

%% Target (with labels)
router_metrics:inc(router_nats_connect_failures_total, [
    {reason, Reason},      %% e.g., "timeout", "auth_failed"
    {cluster, Cluster},    %% if available
    {source, Source}       %% if available
]).
```

### 3. `router_nats_reconnect_failures_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 2 (Important)  
**Scenario**: NATS-001 (NATS Connection Failures)

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `reason` | string | Yes | Failure reason | `"timeout"`, `"auth_failed"`, `"connection_refused"` |
| `cluster` | string | Optional | NATS cluster identifier | `"nats-cluster-1"` |
| `attempt` | integer | Optional | Reconnect attempt number | `1`, `2`, `3` |

**Implementation Notes**:
- `reason` is **required** for failure breakdown analysis
- `attempt` enables analysis of retry patterns

**Code Location**: `apps/otp/router/src/router_nats.erl` (reconnection failure handling)

### 4. `router_nats_publish_failures_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 1 (Critical)  
**Scenario**: NATS-004 (High Publish Failure Rate)

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `reason` | string | Yes | Failure reason | `"timeout"`, `"authorization"`, `"no_route"`, `"stream_not_found"` |
| `subject` | string | Optional | NATS subject | `"beamline.router.v1.decide"` |
| `stream` | string | Optional | JetStream stream name | `"router-stream"` |
| `source` | string | Optional | Publish source | `"usage_event"`, `"dlq"` |

**Implementation Notes**:
- `reason` is **required** for failure breakdown analysis
- `subject` enables subject-specific failure analysis
- `stream` enables JetStream stream-specific analysis

**Code Location**: `apps/otp/router/src/router_nats.erl` (publish failure handling)

**Example Implementation**:
```erlang
%% Current (no labels)
router_metrics:inc(router_nats_publish_failures_total).

%% Target (with labels)
router_metrics:inc(router_nats_publish_failures_total, [
    {reason, Reason},      %% e.g., "timeout", "authorization"
    {subject, Subject},    %% if available
    {stream, Stream},      %% if available (JetStream)
    {source, Source}       %% if available
]).
```

### 5. `router_nats_ack_failures_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 1 (Critical)  
**Scenario**: NATS-005 (High ACK Failure Rate)

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `reason` | string | Yes | Failure reason | `"timeout"`, `"invalid_msg_id"`, `"connection_lost"` |
| `subject` | string | Optional | NATS subject | `"beamline.router.v1.decide"` |
| `stream` | string | Optional | JetStream stream name | `"router-stream"` |
| `consumer` | string | Optional | JetStream consumer name | `"router-results"` |

**Implementation Notes**:
- `reason` is **required** for failure breakdown analysis
- `subject` and `stream` enable stream/consumer-specific analysis

**Code Location**: `apps/otp/router/src/router_nats.erl` (ACK failure handling)

### 6. `router_nats_nak_failures_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 2 (Important)  
**Scenario**: NATS-006 (High NAK Failure Rate)

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `reason` | string | Yes | Failure reason | `"timeout"`, `"invalid_msg_id"`, `"connection_lost"` |
| `subject` | string | Optional | NATS subject | `"beamline.router.v1.decide"` |
| `stream` | string | Optional | JetStream stream name | `"router-stream"` |

**Implementation Notes**:
- `reason` is **required** for failure breakdown analysis

**Code Location**: `apps/otp/router/src/router_nats.erl` (NAK failure handling)

### 7. `router_nats_subscribe_failures_total` Labels

**Current Status**: No labels (simple counter)  
**Priority**: Priority 2 (Important)  
**Scenario**: NATS-007 (High Subscribe Failure Rate)

**Required Labels**:

| Label | Type | Required | Description | Example Values |
|-------|------|----------|-------------|----------------|
| `reason` | string | Yes | Failure reason | `"stream_not_found"`, `"consumer_not_found"`, `"authorization"`, `"timeout"` |
| `subject` | string | Optional | NATS subject | `"beamline.router.v1.decide"` |
| `stream` | string | Optional | JetStream stream name | `"router-stream"` |
| `consumer` | string | Optional | JetStream consumer name | `"router-results"` |

**Implementation Notes**:
- `reason` is **required** for failure breakdown analysis
- `stream` and `consumer` enable JetStream-specific analysis

**Code Location**: `apps/otp/router/src/router_nats.erl` (subscribe failure handling)

## Implementation Plan

### Phase 1: Priority 1 Labels (Critical)

**Timeline**: Immediate  
**Metrics**:
1. `router_dlq_total` - Add `assignment_id`, `reason` (required), `tenant_id`, `source`, `msg_id`, `request_id` (optional)
2. `router_nats_publish_failures_total` - Add `reason` (required), `subject`, `stream`, `source` (optional)
3. `router_nats_ack_failures_total` - Add `reason` (required), `subject`, `stream`, `consumer` (optional)

**Tasks**:
1. Update metric emission code in `router_jetstream.erl` and `router_nats.erl`
2. Update dashboard queries to use labels
3. Update alert rules to use labels (if applicable)
4. Test label cardinality (ensure no explosion)
5. Update documentation

### Phase 2: Priority 2 Labels (Important)

**Timeline**: Short-term  
**Metrics**:
1. `router_nats_connect_failures_total` - Add `reason`, `cluster`, `source`
2. `router_nats_reconnect_failures_total` - Add `reason`, `cluster`, `attempt`
3. `router_nats_nak_failures_total` - Add `reason`, `subject`, `stream`
4. `router_nats_subscribe_failures_total` - Add `reason`, `subject`, `stream`, `consumer`

**Tasks**:
1. Update metric emission code
2. Update dashboard queries
3. Test and validate
4. Update documentation

## Label Cardinality Considerations

**Critical**: Labels must not cause metric cardinality explosion.

**Guidelines**:
- **Low cardinality labels** (< 100 values): `reason`, `assignment_id`, `status`
- **Medium cardinality labels** (100-1000 values): `tenant_id`, `subject`
- **High cardinality labels** (> 1000 values): `msg_id`, `request_id` - **Use sparingly or exclude from production**

**Recommendations**:
- Use `msg_id` and `request_id` only in development/debugging contexts
- For production, use `msg_id` and `request_id` only in logs, not metrics
- Monitor label cardinality in Prometheus

## Testing Requirements

### Unit Tests

1. **Label Format Validation**:
   - Verify all labels are strings (or appropriate types)
   - Verify label names match specification
   - Verify required labels are always present

2. **Label Value Validation**:
   - Verify label values are not empty (unless optional)
   - Verify label values match expected formats

### Integration Tests

1. **Metric Emission Tests**:
   - Verify metrics are emitted with correct labels
   - Verify label values match context

2. **Dashboard Query Tests**:
   - Verify dashboard queries work with new labels
   - Verify breakdown panels display correctly

3. **Alert Rule Tests**:
   - Verify alert rules work with new labels (if applicable)
   - Verify label filters in alerts work correctly

### Performance Tests

1. **Cardinality Tests**:
   - Verify label cardinality does not explode
   - Monitor Prometheus memory usage
   - Test with realistic tenant/assignment counts

## Migration Strategy

### Step 1: Add Labels to Code (Backward Compatible)

- Add labels to metric emission
- Keep old metric name (no breaking change)
- Labels are additive (existing queries still work)

### Step 2: Update Dashboard Queries

- Update dashboard queries to use labels
- Add new breakdown panels
- Test dashboard functionality

### Step 3: Update Alert Rules (If Applicable)

- Update alert rules to use labels (if needed)
- Add label filters for better alerting
- Test alert functionality

### Step 4: Deprecate Old Queries

- Mark old queries as deprecated
- Document migration path
- Remove after transition period

## References

- **Coverage Analysis**: `apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md`
- **Metrics Catalog**: `config/observability/metrics.catalog.yaml`
- **Router Metrics Module**: `apps/otp/router/src/router_metrics.erl`
- **NATS Module**: `apps/otp/router/src/router_nats.erl`
- **JetStream Module**: `apps/otp/router/src/router_jetstream.erl`

---

**Prepared by**: WORKER wrk-9 (Documentation & Developer Experience)  
**Status**: Specification Complete  
**Next Steps**: Implementation team to implement Priority 1 labels

