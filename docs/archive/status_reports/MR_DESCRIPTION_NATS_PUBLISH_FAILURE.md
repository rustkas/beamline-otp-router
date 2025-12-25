# NATS Publish/Publish_with_ack Failure Handling - MR Description

## Summary

This MR adds comprehensive test coverage and documentation for `router_nats` publish and `publish_with_ack` failure scenarios. It explicitly defines and tests expected behavior for all failure types, ensuring predictable system behavior during NATS connectivity issues.

## What Changed

### Tests Added

**New test suite additions** (`router_nats_publish_failure_SUITE.erl`):
- ✅ 6 new tests for upper-level behavior (router/router_jetstream impact)
- ✅ Total test coverage: **29 tests** (was 23, now 29)
- ✅ All failure scenarios explicitly tested:
  - `publish` errors (8 scenarios)
  - `publish_with_ack` errors (8 scenarios)
  - `msg_id` behavior (3 scenarios)
  - Metrics behavior (4 scenarios)
  - **Upper-level behavior (6 new scenarios)**

**New test scenarios**:
1. `test_router_fail_open_continues_processing` - Verifies router continues operation in fail-open mode
2. `test_router_queueing_blocks_on_errors` - Verifies queueing mode behavior
3. `test_router_jetstream_handles_publish_failures` - Verifies router_jetstream error handling
4. `test_router_retry_exhaustion_behavior` - Verifies retry exhaustion handling
5. `test_fail_open_system_availability` - Verifies system availability in fail-open mode
6. `test_queueing_system_blocking_behavior` - Verifies system blocking in queueing mode

### Documentation Added

**New documentation**:
1. **`NATS_PUBLISH_FAILURE_MONITORING.md`** - Operational monitoring and alerting guide
   - PromQL queries for monitoring
   - Alert recommendations (critical and warning)
   - Dashboard recommendations
   - Operational procedures for incident investigation
   - Fail-open vs queueing mode decision guidance

**Updated documentation**:
1. **`NATS_PUBLISH_FAILURE_BEHAVIOR.md`** - Enhanced with:
   - Upper-level behavior section (router/router_jetstream impact)
   - Expected behavior summary table
   - Metrics behavior details with testing requirements
   - Complete test coverage documentation

**Documentation links added**:
- `docs/README.md` - Added links to new documents
- `apps/otp/router/docs/FULL_DOCS.md` - Updated with new document references
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Added reference to monitoring guide

## Expected Behavior (What is "Normal" Now)

### Fail-Open Mode (`nats_fail_open_mode = true`)

**What happens**:
- ✅ `router_nats` returns success (`ok` / `{ok, stub-msg-id}`) even on failures
- ✅ System continues accepting new messages without blocking
- ✅ Messages are **NOT queued** (may be lost)
- ✅ Failures are logged and metered, but do not propagate to upper layers
- ✅ `router_caf_adapter` treats failures as success (no retries)

**When to use**: High availability requirements, best-effort delivery acceptable

### Queueing Mode (`nats_fail_open_mode = false`) - Default

**What happens**:
- ✅ `router_nats` returns `{error, Reason}` on failures
- ✅ Messages are **queued** for retry after reconnection
- ✅ Upper layers receive errors and implement retry logic
- ✅ `router_caf_adapter` retries up to MaxRetries with exponential backoff
- ✅ System may block if queue fills up

**When to use**: Guaranteed delivery requirements, message ordering must be preserved

### Metrics Behavior

**`router_nats_publish_failures_total`** and **`router_nats_publish_with_ack_failures_total`**:
- ✅ Incremented **exactly once** per failed operation
- ✅ Incremented **regardless of fail-open/queueing mode**
- ✅ Incremented **before** returning result to caller
- ✅ **NOT incremented** on successful operations
- ✅ **NOT reset** on reconnection (cumulative counter)

## Testing

**Run tests**:
```bash
# Run all publish failure tests
cd apps/otp/router
rebar3 ct --suite test/router_nats_publish_failure_SUITE

# Or run in parallel (faster)
rebar3 ct -j 4 --suite test/router_nats_publish_failure_SUITE

# Or use Makefile
make test-parallel
```

**Test execution time**: ~30-50 seconds (29 tests total)

**CI Integration**: Tests are automatically run in CI (`.github/workflows/ci.yml`)

## Monitoring

**Key metrics to monitor**:
- `rate(router_nats_publish_failures_total[5m])` - Publish failure rate
- `rate(router_nats_publish_with_ack_failures_total[5m])` - Publish_with_ack failure rate
- `router_nats_pending_operations_count` - Queue size
- `router_nats_connection_status` - Connection health

**Recommended alerts** (see `NATS_PUBLISH_FAILURE_MONITORING.md`):
- Critical: Failure rate > 10/sec, queue overflow, persistent connection loss
- Warning: Failure rate > 5/sec, queue > 80% capacity

## Documentation

**For developers**:
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Complete behavior specification

**For SRE/operators**:
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md` - Monitoring and alerting guide

**Quick reference**:
- `apps/otp/router/docs/FULL_DOCS.md` - Unified router documentation (includes links)

## Breaking Changes

**None** - This MR only adds tests and documentation. No code changes to existing behavior.

## Checklist

- [x] Tests added and passing
- [x] Documentation updated
- [x] Links added to README/architecture docs
- [x] CI integration verified
- [x] Metrics behavior documented
- [x] Monitoring recommendations provided

## Related Issues

- Explicitly defines expected behavior for all NATS publish failure scenarios
- Provides operational guidance for monitoring and alerting
- Ensures team understands "what is normal" during failures

