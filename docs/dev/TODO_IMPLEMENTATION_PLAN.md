# TODO Implementation Plan

**Date**: 2025-01-27  
**Purpose**: Track and plan implementation of TODO comments in router codebase

## router_nats.erl TODOs

### High Priority

#### 1. Extract Cluster from Config
**Location**: Lines 88, 383  
**Current**: Hardcoded `<<"default">>`  
**Action**: 
- Add cluster configuration to application environment
- Extract from `application:get_env(beamline_router, nats_cluster, <<"default">>)`
- Update both locations

#### 2. Implement Actual NATS Connection
**Location**: Lines 261, 745  
**Current**: Mock/stub implementation  
**Action**:
- Replace mock with real NATS client connection
- Implement actual NATS nak functionality
- Add connection resilience and retry logic

### Medium Priority

#### 3. Extract Context Information
**Location**: Lines 643, 646-648, 659-661, 707-708, 719-720  
**Current**: Hardcoded `<<"unknown">>` for subject/stream/consumer  
**Action**:
- Add context extraction from MsgId
- Store context in message metadata
- Extract subject/stream/consumer from context when available
- Fallback to `<<"unknown">>` only when context truly unavailable

## router_intake_backpressure.erl TODOs

### CP3/Pre-Release Tasks

#### 1. Real-time JetStream Queries
**Current**: Uses cached ETS values  
**Action**: Query JetStream consumer info via NATS API for real-time values

#### 2. P95 Calculation from Histogram
**Current**: Uses cached ETS values  
**Action**: Calculate P95 from histogram metrics for real-time values

#### 3. Gateway Integration
**Action**: Complete Gateway → Router backpressure integration

#### 4. Testing
**Action**: Add end-to-end overload scenarios testing

#### 5. Production Readiness
**Action**: 
- Add production-ready backpressure policies
- Full observability integration (metrics, alerts, dashboards)

## router_admin_nats.erl TODOs

### Low Priority

#### 1. Track Executed Extensions
**Location**: Line 159  
**Current**: Empty array `executed_extensions => []`  
**Action**:
- Track extensions as they execute
- Store in response metadata
- Use for debugging and observability

## Implementation Priority

1. **High**: Extract cluster from config (simple, low risk)
2. **High**: Extract context information (improves observability)
3. **Medium**: Real NATS connection (requires external dependency)
4. **Medium**: Real-time JetStream queries (requires NATS API)
5. **Low**: Extension tracking (nice to have)

## Notes

- Most TODOs are for future CP3/Pre-Release work
- Some TODOs require external dependencies (NATS client library)
- Context extraction can be implemented incrementally
- Cluster config extraction is straightforward and can be done immediately

