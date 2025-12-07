# Rate Limiting Invariants

**Version**: 1.0  
**Date**: 2025-11-30  
**Status**: CP2-LC  
**Component**: Router (`apps/otp/router/`)

## Purpose

This document explicitly defines invariants for Router rate limiting implementation, ensuring consistent behavior and preventing edge cases.

## Invariants

### 1. Disabled Rate Limit Behavior

**Invariant**: When rate limiting is disabled (`enabled: false`), all requests must be allowed regardless of rate limit configuration.

**Implementation**:
- `router_rate_limit_store:check_rate_limit/3` checks `enabled` flag first
- If `enabled = false`, returns `{ok, allow}` immediately (no bucket check)
- `router_policy_applier:check_policy_rate_limit/3` also checks `enabled` flag

**Code References**:
- `apps/otp/router/src/router_rate_limit_store.erl:66-69`
- `apps/otp/router/src/router_policy_applier.erl:567-570`

**Test Coverage**:
- `router_rate_limit_store_SUITE:test_rate_limit_disabled/1` - Verifies disabled rate limit allows all requests

**Edge Cases**:
- Rate limit disabled with very low limits (1 req/s, burst 1) → still allows all requests
- Rate limit disabled after being enabled → bucket state is ignored
- Rate limit configuration missing `enabled` field → defaults to `false` (allow all)

### 2. No Double Execution Guarantee

**Invariant**: Rate limiting check must not cause double execution of requests. Rate limiting is a **read-only check** that does not modify request state.

**Implementation**:
- Rate limit check is **idempotent** (can be called multiple times with same result)
- Rate limit check does **not** consume tokens until request is actually processed
- Rate limit check happens **before** provider selection, not during execution
- Rate limit check is **stateless** from request perspective (only bucket state changes)

**Code References**:
- `apps/otp/router/src/router_policy_applier.erl:66-77` - Rate limit check before policy application
- `apps/otp/router/src/router_rate_limit_store.erl:check_rate_limit/3` - Token consumption is atomic

**Test Coverage**:
- **Missing**: Test for concurrent rate limit checks (race conditions)
- **Missing**: Test for rate limit check retry after failure

**Edge Cases**:
- Concurrent rate limit checks for same policy → only one should consume token
- Rate limit check succeeds, but request fails before provider selection → token already consumed (by design)
- Rate limit check fails, but retry succeeds → second check should also fail (no token refill)

### 3. Restart/Reset Policy

**Invariant**: On Router restart or rate limit reset, all buckets are cleared. New requests start with fresh bucket state (full burst capacity).

**Implementation**:
- ETS table `rate_limits` is **in-memory only** (not persisted)
- On Router restart, ETS table is recreated empty
- `router_rate_limit_store:reset_rate_limit/2` deletes bucket from ETS table
- First request after reset/restart creates new bucket with full burst capacity

**Code References**:
- `apps/otp/router/src/router_rate_limit_store.erl:32-44` - ETS table initialization
- `apps/otp/router/src/router_rate_limit_store.erl:151-154` - Reset implementation
- `apps/otp/router/src/router_rate_limit_store.erl:129-146` - First request creates bucket with full burst

**Test Coverage**:
- `router_rate_limit_store_SUITE:test_rate_limit_reset/1` - Verifies reset clears bucket
- **Missing**: Test for Router restart behavior (ETS table recreation)
- **Missing**: Test for bucket recreation after reset (full burst capacity)

**Edge Cases**:
- Router restart during active rate limiting → all buckets lost, fresh start
- Reset during active rate limiting → bucket cleared, next request gets full burst
- Multiple resets in quick succession → bucket cleared each time, no side effects

### 4. Token Bucket Algorithm Invariants

**Invariant**: Token bucket must maintain correct token count and refill rate.

**Implementation**:
- Tokens refill at `requests_per_second` rate (tokens per second)
- Maximum tokens = `burst` (bucket capacity)
- Token consumption is atomic (ETS update is atomic)
- Token refill calculation: `NewTokens = min(Tokens + TimeElapsed * RPS, Burst)`

**Code References**:
- `apps/otp/router/src/router_rate_limit_store.erl:84-87` - Token refill calculation
- `apps/otp/router/src/router_rate_limit_store.erl:89-107` - Token consumption logic

**Test Coverage**:
- `router_rate_limit_store_SUITE:test_rate_limit_refill/1` - Verifies token refill over time
- `router_rate_limit_store_SUITE:test_rate_limit_burst/1` - Verifies burst capacity
- **Missing**: Test for token refill edge cases (very long time elapsed, clock skew)
- **Missing**: Test for token refill precision (fractional seconds)

**Edge Cases**:
- Very long time elapsed (e.g., 1 hour) → tokens should cap at `burst`, not exceed
- Clock skew (system time goes backward) → should handle gracefully (use `max(0, TimeElapsed)`)
- Fractional seconds in refill → should round down (integer tokens only)

### 5. Configuration Change Policy

**Invariant**: When rate limit configuration changes (e.g., `requests_per_second` or `burst` changes), existing buckets should be updated or reset.

**Implementation**:
- **Current behavior**: Configuration change requires bucket reset (manual or automatic)
- **Future enhancement**: Automatic bucket update on configuration change (CP2+)

**Code References**:
- `apps/otp/router/src/router_rate_limit_store.erl:77-83` - Bucket lookup uses existing configuration
- **Missing**: Configuration change detection and bucket update logic

**Test Coverage**:
- **Missing**: Test for configuration change (RPS change, burst change)
- **Missing**: Test for bucket update vs reset on configuration change

**Edge Cases**:
- Configuration change from 100 req/s to 50 req/s → existing bucket should be updated or reset
- Configuration change from 50 burst to 100 burst → existing bucket should be updated or reset
- Configuration change during active rate limiting → should not cause race conditions

### 6. Scope Isolation

**Invariant**: Rate limits at different scopes (policy, tenant, global) must be independent. Policy rate limit does not affect tenant rate limit, and vice versa.

**Implementation**:
- Different scopes use different ETS keys: `{TenantId, PolicyId}` for policy, `TenantId` for tenant
- `router_rate_limit_store:build_key/2` creates unique keys per scope
- Each scope has its own bucket with independent token count

**Code References**:
- `apps/otp/router/src/router_rate_limit_store.erl:199-205` - Key building logic
- `apps/otp/router/src/router_rate_limit_store.erl:74` - Key used for bucket lookup

**Test Coverage**:
- **Missing**: Test for scope isolation (policy limit vs tenant limit)
- **Missing**: Test for multiple policies in same tenant (independent limits)

**Edge Cases**:
- Policy A and Policy B in same tenant → should have independent rate limits
- Tenant limit and policy limit both configured → both should be checked independently
- Global limit and policy limit both configured → both should be checked independently

### 7. Error Handling Policy

**Invariant**: Rate limit check errors should **fail open** (allow request) to prevent rate limiting from blocking legitimate requests.

**Implementation**:
- Invalid rate limit configuration → `{ok, allow}` (fail open)
- Rate limit store unavailable → `{ok, allow}` (fail open)
- ETS table errors → `{ok, allow}` (fail open)

**Code References**:
- `apps/otp/router/src/router_policy_applier.erl:580-582` - Invalid configuration → allow
- **Missing**: Error handling for rate limit store unavailability

**Test Coverage**:
- **Missing**: Test for rate limit store unavailable (gen_server down)
- **Missing**: Test for invalid rate limit configuration (negative values, missing fields)
- **Missing**: Test for ETS table errors (table not found, permission errors)

**Edge Cases**:
- Rate limit store gen_server crashes → should fail open (allow requests)
- Invalid `requests_per_second` (negative, zero, very large) → should validate and fail open if invalid
- Missing `burst` field → should use default (50) or fail open

### 8. Metrics Invariants

**Invariant**: Rate limit metrics must be emitted for every rate limit check, regardless of outcome.

**Implementation**:
- `router_rate_limit_allowed_total` emitted on allow
- `router_rate_limit_exceeded_total` emitted on exceed
- Metrics include scope and tenant_id labels

**Code References**:
- `apps/otp/router/src/router_rate_limit_store.erl:102-105` - Metrics on allow
- `apps/otp/router/src/router_rate_limit_store.erl:116-119` - Metrics on exceed

**Test Coverage**:
- **Missing**: Test for metrics emission (verify metrics are incremented)
- **Missing**: Test for metrics labels (scope, tenant_id)

**Edge Cases**:
- Rate limit disabled → metrics should not be emitted (no check performed)
- Rate limit check fails (error) → metrics should not be emitted (fail open, no check)

## Implementation Verification

### Current Implementation Status

**✅ Implemented**:
- Disabled rate limit behavior (allows all requests)
- Token bucket algorithm (refill, burst, consumption)
- Reset functionality (clears bucket)
- Status query (get current bucket state)
- Basic test coverage (allowed, exceeded, burst, refill, disabled, reset, status)

**⚠️ Partially Implemented**:
- Configuration change handling (requires manual reset)
- Error handling (some edge cases not covered)

**❌ Missing**:
- Concurrent rate limit check tests (race conditions)
- Router restart behavior tests
- Configuration change tests
- Scope isolation tests
- Error handling tests (store unavailable, invalid config)
- Metrics emission tests

## Recommendations

### Immediate Actions (CP2)

1. **Add missing tests**:
   - Concurrent rate limit checks (race conditions)
   - Router restart behavior
   - Configuration change handling
   - Scope isolation
   - Error handling (fail open)

2. **Document edge cases**:
   - Clock skew handling
   - Very long time elapsed
   - Fractional seconds in refill

3. **Improve error handling**:
   - Rate limit store unavailable → fail open
   - Invalid configuration → validate and fail open

### Future Enhancements (CP2+)

1. **Automatic bucket update on configuration change**
2. **Redis-backed storage for distributed rate limiting**
3. **Per-provider rate limiting**
4. **Rate limit metrics dashboard**

## References

- `apps/otp/router/src/router_rate_limit_store.erl` - Implementation
- `apps/otp/router/src/router_policy_store.erl` - Policy parsing
- `apps/otp/router/src/router_policy_applier.erl` - Rate limit check integration
- `apps/otp/router/test/router_rate_limit_store_SUITE.erl` - Test suite
- `docs/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` - Design document
- `docs/ROUTING_POLICY.md` - Policy DSL specification

