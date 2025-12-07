# Publish Error Tests - Requirements Mapping

**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**  
**Component**: Router (apps/otp/router)

## Purpose

This document maps formal requirements for `router_nats:publish/2` error handling in JetStream consumers to implemented test cases.

## Requirements Summary

### Core Requirements

1. **Resilience**: Publish errors must not break JetStream message processing
2. **Logging**: Publish errors must be logged with appropriate levels
3. **Metrics**: Publish errors must be tracked in metrics
4. **Consumer Stability**: Consumer processes must remain alive after publish errors

## Requirements to Tests Mapping

### Requirement 1: Publish Errors Do Not Break Message Processing

**Requirement**: When `router_nats:publish/2` returns an error or throws an exception, JetStream consumer must:
- Continue processing subsequent messages
- Not crash or terminate
- Complete message processing (ACK/NAK as appropriate)

**Test Coverage**:

| Consumer | Test Case | Status |
|----------|-----------|--------|
| `router_result_consumer` | `test_usage_publish_error_return` | ✅ |
| `router_result_consumer` | `test_usage_publish_error_exception` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_return` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_exception` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_consumer_resilience` | ✅ |
| `router_intake_error_handler` | `test_dlq_publish_error_return` | ✅ |
| `router_intake_error_handler` | `test_dlq_publish_error_exception` | ✅ |
| `router_intake_error_handler` | `test_error_response_publish_error` | ✅ |

**Verification Criteria**:
- ✅ Consumer process remains alive after publish error
- ✅ ACK/NAK operations complete successfully
- ✅ Subsequent messages can be processed

### Requirement 2: Publish Errors Are Logged

**Requirement**: When `router_nats:publish/2` fails, errors must be logged with:
- Appropriate log level (`error` for fatal, `warning` for expected failures)
- Error reason/code
- Context information (subject, message type, etc.)

**Test Coverage**:

| Consumer | Test Case | Log Level | Status |
|----------|-----------|-----------|--------|
| `router_result_consumer` | `test_usage_publish_error_return` | `error` | ✅ |
| `router_result_consumer` | `test_usage_publish_error_exception` | `error` | ✅ |
| `router_intake_error_handler` | `test_dlq_publish_error_return` | `error` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_return` | (implicit) | ✅ |

**Verification Criteria**:
- ✅ Error log entry is created
- ✅ Log contains error reason
- ✅ Log contains context (subject, error type)

**Note**: `router_decide_consumer` does not explicitly log publish errors (publish is fire-and-forget for reply messages). This is acceptable per design.

### Requirement 3: Publish Errors Are Tracked in Metrics

**Requirement**: When `router_nats:publish/2` fails, metrics must be incremented:
- For usage events: `router_usage_emit_failed_total`
- For DLQ: DLQ failure metrics
- Metrics must include error type/reason in metadata

**Test Coverage**:

| Consumer | Test Case | Metric | Status |
|----------|-----------|--------|--------|
| `router_result_consumer` | `test_usage_publish_error_return` | `router_usage_emit_failed_total` | ✅ |
| `router_result_consumer` | `test_usage_publish_error_metrics` | `router_usage_emit_failed_total` | ✅ |
| `router_intake_error_handler` | `test_dlq_publish_error_return` | DLQ failure metrics | ✅ |

**Verification Criteria**:
- ✅ Error metric is incremented
- ✅ Metric metadata contains error information
- ✅ Metric metadata contains subject/context

### Requirement 4: Consumer Processes Remain Alive

**Requirement**: Consumer processes must not crash or terminate when publish errors occur.

**Test Coverage**:

| Consumer | Test Case | Status |
|----------|-----------|--------|
| `router_result_consumer` | `test_usage_publish_error_return` | ✅ |
| `router_result_consumer` | `test_usage_publish_error_exception` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_return` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_exception` | ✅ |
| `router_decide_consumer` | `test_reply_publish_error_consumer_resilience` | ✅ |

**Verification Criteria**:
- ✅ Consumer PID remains alive after publish error
- ✅ Consumer can process subsequent messages
- ✅ No supervisor restarts occur

## Error Types Covered

### Return Errors

| Error Type | Test Case | Consumer |
|------------|-----------|----------|
| `{error, timeout}` | `test_usage_publish_error_return` | `router_result_consumer` |
| `{error, timeout}` | `test_reply_publish_error_return` | `router_decide_consumer` |
| `{error, timeout}` | `test_dlq_publish_error_return` | `router_intake_error_handler` |
| `{error, connection_lost}` | `test_usage_publish_error_metrics` | `router_result_consumer` |

### Exceptions

| Exception Type | Test Case | Consumer |
|----------------|-----------|----------|
| `erlang:error(connection_lost)` | `test_usage_publish_error_exception` | `router_result_consumer` |
| `erlang:error(connection_lost)` | `test_reply_publish_error_exception` | `router_decide_consumer` |
| `erlang:error(connection_lost)` | `test_dlq_publish_error_exception` | `router_intake_error_handler` |

## Test Integration

### Test Suites

All new tests are integrated into existing test suites:

- `router_result_consumer_SUITE.erl` - 3 new tests
- `router_decide_consumer_SUITE.erl` - 3 new tests
- `router_intake_error_handler_SUITE.erl` - 3 new tests

### CI/CD Integration

Tests are automatically included in standard test runs:

```bash
# Run all consumer tests
rebar3 ct --suite test/router_result_consumer_SUITE
rebar3 ct --suite test/router_decide_consumer_SUITE
rebar3 ct --suite test/router_intake_error_handler_SUITE
```

### Test Execution

All tests use:
- `meck` for mocking `router_nats:publish/2`
- `test_helpers` for bounded waits
- ETS tables for tracking logs and metrics
- Process PID checks for resilience verification

## Coverage Summary

### By Consumer

| Consumer | Total Tests | Publish Error Tests | Coverage |
|----------|-------------|---------------------|----------|
| `router_result_consumer` | 10 | 3 | ✅ Complete |
| `router_decide_consumer` | 13 | 3 | ✅ Complete |
| `router_intake_error_handler` | 12 | 3 | ✅ Complete |

### By Requirement

| Requirement | Tests | Status |
|-------------|-------|--------|
| Resilience | 8 | ✅ Complete |
| Logging | 4 | ✅ Complete |
| Metrics | 3 | ✅ Complete |
| Process Stability | 5 | ✅ Complete |

## Gaps and Future Work

### Known Gaps

1. **Admin Consumer**: `router_admin_nats_subscriber` uses `router_nats:publish/2` but does not have dedicated publish error tests
   - **Status**: Low priority (admin operations are less critical)
   - **Recommendation**: Add tests if admin operations become critical path

2. **Retry Scenarios**: No tests for publish errors with retry mechanisms
   - **Status**: Not required per current design (publish is fire-and-forget)
   - **Recommendation**: Add if retry logic is implemented

3. **Error Type Differentiation**: Limited coverage of different error types (timeout, connection_lost, etc.)
   - **Status**: Basic coverage exists
   - **Recommendation**: Add more error types if specific handling is required

### Future Enhancements

1. **Comprehensive Error Type Coverage**: Add tests for all error types mentioned in requirements
2. **Retry Mechanism Tests**: Add tests if retry logic is implemented for publish operations
3. **Performance Tests**: Add tests for publish error impact on throughput
4. **Integration Tests**: Add E2E tests with real NATS server failures

## Maintenance Process

### Keeping This Document Up-to-Date

**CRITICAL**: This mapping document must be kept synchronized with:
- Changes to publish error handling requirements
- Addition of new JetStream consumers that use `router_nats:publish/2`
- Changes to test coverage or verification criteria

### Rule 1: New Consumer or New `router_nats:publish/2` Usage

**When**: A new JetStream consumer is added or a new usage of `router_nats:publish/2` is introduced.

**Required Actions**:

1. **Add Publish Error Tests** (minimum required scenarios):
   - `test_{consumer_type}_publish_error_return` - Mock `router_nats:publish/2` to return `{error, Reason}`
   - `test_{consumer_type}_publish_error_exception` - Mock `router_nats:publish/2` to throw exception
   - `test_{consumer_type}_publish_error_resilience` - Process multiple messages with publish errors
   - Verify consumer process remains alive
   - Verify ACK/NAK behavior per requirements
   - Verify error logging (if applicable)
   - Verify metrics tracking (if applicable)

2. **Update This Document**:
   - Add consumer to test coverage tables (Requirement 1, 2, 3, 4)
   - Add test cases to "Test Coverage" sections
   - Update "Coverage Summary" tables
   - Add error types to "Error Types Covered" section
   - Update "References" section

3. **Update Implementation Summary** (if needed):
   - Add consumer to coverage summary
   - Update total test count

**Test Naming Convention**: `test_{consumer_type}_publish_error_{scenario}`

**Example**: For a new `router_notification_consumer`:
- `test_notification_publish_error_return`
- `test_notification_publish_error_exception`
- `test_notification_publish_error_resilience`

### Rule 2: Changes to Logging/Metrics Requirements

**When**: Requirements change for:
- Log levels for publish errors
- Log message format for publish errors
- Metric structure/names/labels for publish errors

**Required Actions**:

1. **Synchronize Implementation**:
   - Update consumer implementation to match new requirements
   - Update existing tests to verify new rules (avoid brittle checks)

2. **Update This Document**:
   - Update "Requirement 2: Publish Errors Are Logged" section with new log levels/format
   - Update "Requirement 3: Publish Errors Are Tracked in Metrics" section with new metric structure
   - Update test coverage tables if new verification criteria are added
   - Update "Verification Criteria" for affected requirements

3. **Update Testing Recommendations** (if general approach changes):
   - Update "Publish Error Tests" section in `TESTING_RECOMMENDATIONS.md`
   - Update test implementation pattern if needed

**Note**: Tests should verify essential requirements (log level, metric name) but avoid checking implementation details that may change during refactoring.

### Rule 3: CI Stability Monitoring

**When**: During major releases or CI infrastructure changes.

**Required Actions**:

1. **Check Test Stability**:
   - Monitor for flaky test failures (random failures due to timing, environment)
   - Check test execution time remains acceptable
   - Review CI logs for publish error test failures

2. **If Tests Are Flaky**:
   - **Stabilize tests**: Add explicit synchronization, relax non-critical checks, increase timeouts if needed
   - **Document environment requirements**: If issue is environment-related, document NATS/JetStream configuration requirements

3. **Update Documentation** (if needed):
   - Document environment requirements in test documentation
   - Update troubleshooting section if common issues are identified

**Monitoring Frequency**: 
- Before major releases
- After CI infrastructure changes
- When flaky test reports are received

### When to Update This Document

**Update this document when**:

1. **New Consumer Added**:
   - New JetStream consumer is created that uses `router_nats:publish/2`
   - **Action**: Add publish error tests and update mapping tables

2. **Requirements Changed**:
   - New requirements for publish error handling are added
   - Existing requirements are modified
   - **Action**: Update requirements section and test coverage tables

3. **Tests Added/Modified**:
   - New publish error tests are added
   - Existing tests are modified or removed
   - **Action**: Update test coverage tables and verification criteria

4. **Error Types Added**:
   - New error types are added to error handling
   - **Action**: Update "Error Types Covered" section

### Update Checklist

When updating this document, ensure:

- [ ] Requirements section reflects current requirements
- [ ] Test coverage tables include all relevant tests
- [ ] Verification criteria match actual test assertions
- [ ] Error types section lists all covered error types
- [ ] Coverage summary tables are accurate
- [ ] References point to correct files and test cases
- [ ] Gaps and future work section reflects current state

### Code Review Checklist

**For reviewers**: When reviewing changes that affect publish error handling:

- [ ] Are publish error tests added/updated for affected consumers?
- [ ] Is `PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md` updated?
- [ ] Do tests cover minimum required scenarios (error return + exception + resilience)?
- [ ] Are logging and metrics verified (if applicable)?

### Adding Tests for New Consumers

**Minimum required test scenarios**:

1. **Error Return Test**: Mock `router_nats:publish/2` to return `{error, Reason}`
   - Verify consumer process remains alive
   - Verify message processing completes (ACK/NAK)
   - Verify error is logged (if applicable)
   - Verify metrics are incremented (if applicable)

2. **Exception Test**: Mock `router_nats:publish/2` to throw exception
   - Verify consumer process remains alive
   - Verify exception is handled gracefully
   - Verify error is logged (if applicable)

3. **Resilience Test**: Process multiple messages with publish errors
   - Verify consumer can process subsequent messages
   - Verify no message loss or infinite retries

**Test naming convention**: `test_{consumer_type}_publish_error_{scenario}`

**Example**:
- `test_usage_publish_error_return` (for usage events)
- `test_reply_publish_error_exception` (for reply messages)
- `test_dlq_publish_error_return` (for DLQ)

## References

- `apps/otp/router/test/router_result_consumer_SUITE.erl` - Usage event publish error tests
- `apps/otp/router/test/router_decide_consumer_SUITE.erl` - Reply message publish error tests
- `apps/otp/router/test/router_intake_error_handler_SUITE.erl` - DLQ/error response publish error tests
- `apps/otp/router/src/router_result_consumer.erl` - Usage event consumer implementation
- `apps/otp/router/src/router_decide_consumer.erl` - Decide request consumer implementation
- `apps/otp/router/src/router_intake_error_handler.erl` - Intake error handler implementation
- `apps/otp/router/docs/dev/PUBLISH_ERROR_TESTS_IMPLEMENTATION_SUMMARY.md` - Implementation summary

