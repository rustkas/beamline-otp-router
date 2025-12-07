# Testing Recommendations and Best Practices

## Overview

This document provides recommendations for running tests efficiently, maintaining test stability, and optimizing CI/CD pipelines.

## Quick Start

### Parallel Test Execution (Recommended)

```bash
cd apps/otp/router
rebar3 ct -j 4
```

**Why**: Parallel execution with 4 workers significantly reduces wall-clock time while maintaining test isolation through ephemeral ports.

### Individual Test Suites

```bash
# Unit tests (fast, can run in parallel)
rebar3 ct --suite test/router_core_SUITE
rebar3 ct --suite test/router_caf_adapter_unit_SUITE

# Integration tests (slower, may require sequence)
rebar3 ct --suite test/router_admin_grpc_integration_SUITE

# Property-based tests
rebar3 as test ct --suite test/router_policy_store_prop_SUITE
rebar3 as test ct --suite test/router_ets_consistency_prop_SUITE
```

## Test Profile Configuration

The test profile (`rebar3 as test`) includes:

- **PropEr**: Property-based testing framework
- **Meck**: Mocking framework
- **Disabled ETS heir/transfer**: Faster initialization
- **Minimized telemetry**: Reduced overhead in tests
- **JUnit reporter**: `cth_surefire` can be added for CI/CD integration (optional)

### Test Profile Settings

```erlang
{test, [
    {env, [
        {beamline_router, [
            {disable_heir, true},           %% Faster ETS initialization
            {telemetry_enabled, false}      %% Minimize telemetry overhead
        ]}
    ]},
    {ct_opts, [
        {ct_hooks, [
            {cth_surefire, [{path, "test_results/junit.xml"}]}
        ]}
    ]}
]}
```

## Test Helpers

### Standardized Bounded Waits

All test suites should use `test_helpers` module for bounded polling:

```erlang
-include_lib("common_test/include/ct.hrl").

%% Wait for process to start
test_helpers:wait_for_app_start(router_result_consumer, 1000).

%% Wait for meck call
test_helpers:wait_for_meck_call(Module, Function, Args, 500).

%% Wait for condition
test_helpers:wait_for_condition(fun() -> check_condition() end, 1000).
```

**Benefits**:
- Consistent timeout handling
- Short polling intervals (10ms default)
- Clear error messages on timeout
- No blocking sleeps

### Migration from timer:sleep

**Before**:
```erlang
timer:sleep(200),  %% Blocking, arbitrary delay
```

**After**:
```erlang
test_helpers:wait_for_condition(fun() -> check_ready() end, 1000).
```

## CT Groups for Parallelization

### Unit Tests (Parallel)

Fast, isolated unit tests should use `parallel` groups:

```erlang
all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_policy_parsing,
            test_basic_decision,
            test_missing_tenant_id
        ]}
    ].
```

### Integration Tests (Sequence)

Integration tests that share state should use `sequence`:

```erlang
all() ->
    [{group, integration_tests}].

groups() ->
    [
        {integration_tests, [sequence], [
            test_setup,
            test_main_flow,
            test_cleanup
        ]}
    ].
```

## Property-Based Testing

### Running Property Tests

```bash
# All property tests
rebar3 as test ct --suite test/router_policy_store_prop_SUITE \
                  --suite test/router_options_merge_prop_SUITE \
                  --suite test/router_normalize_boolean_prop_SUITE \
                  --suite test/router_decider_prop_SUITE \
                  --suite test/router_ets_consistency_prop_SUITE

# Individual suite
rebar3 as test ct --suite test/router_policy_store_prop_SUITE
```

### CI Configuration

**Fast CI runs** (recommended):
```erlang
proper:quickcheck(Prop, [{numtests, 50}, {seed, 12345}]).
```

**Nightly deep runs**:
```erlang
proper:quickcheck(Prop, [{numtests, 300}, {seed, 67890}]).
```

### Property Test Style

All property tests follow unified style:
- Always include `proper.hrl` in test profile
- Runtime check: `case code:which(proper) of ...`
- Skip fallbacks: `prop_*_skip/1` functions
- Consistent error handling for `proper:quickcheck` results

See [docs/PROPERTY_TESTING.md](PROPERTY_TESTING.md) for details.

## Common Test Execution Patterns

### From Application Directory (Recommended)

```bash
cd apps/otp/router
rebar3 ct -j 4
```

**Why**: Avoids issues with `rebar_prv_common_test` option parsing when running from repo root.

### With Custom Options

```bash
cd apps/otp/router
rebar3 ct --suite test/router_core_SUITE --logdir ct_logs
```

### Full Test Suite

```bash
cd apps/otp/router
rebar3 as test ct -j 4
```

## Performance Optimization

### Historical Bottlenecks (Resolved)

1. **Sequential execution**: Use `-j 4` for parallel runs
2. **Blocking waits**: Replaced with bounded polling
3. **Heavy ETS initialization**: Disabled heir/transfer in test profile
4. **Excessive telemetry**: Minimized in test profile
5. **Incorrect CT contracts**: Fixed property test return values

### Current Optimizations

- **Ephemeral ports**: All tests use port 0 (OS-assigned) for isolation
- **Test mode**: `disable_heir = true` speeds up ETS initialization
- **Minimal telemetry**: `telemetry_enabled = false` in test profile
- **CT groups**: Parallel execution for unit tests
- **Bounded waits**: Short polling intervals (10ms) with timeouts

## CI/CD Integration

### GitHub Actions Configuration

The project uses parallel test execution (`-j 4`) and collects both CT logs and JUnit reports:

```yaml
- name: Common Test (router) - Parallel execution
  working-directory: apps/otp/router
  run: rebar3 as test ct -j 4 --logdir ct_logs
  continue-on-error: true

- name: Upload CT logs
  uses: actions/upload-artifact@v4
  if: always()
  with:
    name: ct_logs_${{ matrix.otp }}
    path: apps/otp/router/ct_logs
    retention-days: 7

- name: Upload JUnit reports (if available)
  uses: actions/upload-artifact@v4
  if: always()
  with:
    name: junit_reports_${{ matrix.otp }}
    path: apps/otp/router/test_results/*.xml
    retention-days: 7
    continue-on-error: true
```

**Key points**:
- Tests run from `apps/otp/router` directory (required for proper option parsing)
- Parallel execution with 4 workers (`-j 4`)
- Test profile (`as test`) enables optimized settings (disabled heir, minimal telemetry)
- CT logs always uploaded (HTML reports in `ct_logs/`)
- JUnit reports uploaded if `cth_surefire` is available

### JUnit Reports (Optional)

JUnit reports can be enabled by adding `cth_surefire` to test dependencies and configuring the hook:

```erlang
{test, [
    {deps, [
        {cth_surefire, "~> 2.0"}  %% Add if available
    ]},
    {ct_opts, [
        {ct_hooks, [
            {cth_surefire, [{path, "test_results/junit.xml"}]}
        ]}
    ]}
]}
```

**Note**: `cth_surefire` may not be available in all Erlang/OTP versions. Common Test HTML reports are always available in `ct_logs/`.

### CI/CD Best Practices

1. **Always run from app directory**: `cd apps/otp/router && rebar3 as test ct -j 4`
2. **Use test profile**: `as test` enables optimized settings
3. **Parallel execution**: `-j 4` reduces wall-clock time
4. **Collect artifacts**: Upload both CT logs and JUnit reports
5. **Continue on error**: Use `continue-on-error: true` to collect all results

## Publish Error Tests

### Overview

Publish error tests verify that JetStream consumers handle `router_nats:publish/2` errors gracefully without breaking message processing. These tests ensure resilience, proper logging, and metrics tracking when publish operations fail.

### Documentation

- **Implementation Summary**: `apps/otp/router/docs/dev/PUBLISH_ERROR_TESTS_IMPLEMENTATION_SUMMARY.md`
- **Requirements Mapping**: `apps/otp/router/docs/dev/PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md`

### Test Coverage

Publish error tests are integrated into consumer test suites:

- `router_result_consumer_SUITE.erl` - Usage event publish error tests (3 tests)
- `router_decide_consumer_SUITE.erl` - Reply message publish error tests (3 tests)
- `router_intake_error_handler_SUITE.erl` - DLQ/error response publish error tests (3 tests)

### Running Publish Error Tests

```bash
cd apps/otp/router

# Run specific consumer tests (includes publish error tests)
rebar3 ct --suite test/router_result_consumer_SUITE
rebar3 ct --suite test/router_decide_consumer_SUITE
rebar3 ct --suite test/router_intake_error_handler_SUITE

# Run all tests (publish error tests included automatically)
rebar3 ct
```

### Adding Tests for New Consumers

When adding a new JetStream consumer that uses `router_nats:publish/2`, follow these guidelines:

#### Minimum Required Test Scenarios

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

#### Test Naming Convention

Use pattern: `test_{consumer_type}_publish_error_{scenario}`

**Examples**:
- `test_usage_publish_error_return` (for usage events)
- `test_reply_publish_error_exception` (for reply messages)
- `test_dlq_publish_error_return` (for DLQ)

#### Test Implementation Pattern

```erlang
%% @doc Test: {consumer_type} publish error (return error)
%% Verifies that JetStream consumer continues processing when router_nats:publish/2 returns error
test_{consumer_type}_publish_error_return(_Config) ->
    %% Setup tracking for logs and metrics
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    
    LogCalls = ets:new(log_calls, [set, private]),
    MetricCalls = ets:new(metric_calls, [set, private]),
    
    %% Mock router_nats:publish to return error
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        {error, timeout}
    end),
    
    %% Get consumer process PID before processing
    ConsumerPid = whereis(consumer_module),
    true = is_pid(ConsumerPid),
    
    %% Process message (should handle publish error gracefully)
    consumer_module:handle_info({nats_message, Subject, Payload}, #{}),
    
    timer:sleep(300),
    
    %% CRITERIA 1: RESILIENCE VERIFICATION
    true = is_process_alive(ConsumerPid),
    
    %% CRITERIA 2: LOGGING VERIFICATION (if applicable)
    AllLogs = ets:tab2list(LogCalls),
    ErrorLogs = [L || {log, error, _Message, _Context} = L <- AllLogs],
    true = length(ErrorLogs) > 0,
    
    %% CRITERIA 3: METRICS VERIFICATION (if applicable)
    AllMetrics = ets:tab2list(MetricCalls),
    ErrorMetrics = [M || {metric, _, MetricName, _} = M <- AllMetrics,
                        MetricName =:= expected_error_metric],
    true = length(ErrorMetrics) > 0,
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(LogCalls),
    ets:delete(MetricCalls),
    
    ok.
```

#### Updating Requirements Mapping

**CRITICAL**: When adding publish error tests for a new consumer:

1. Add tests to consumer's test suite
2. Update `PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md`:
   - Add consumer to test coverage tables
   - Update coverage summary
   - Add test cases to requirements mapping

See `PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md` for detailed maintenance process.

### Code Review Checklist

When reviewing changes that affect publish error handling:

- [ ] Are publish error tests added/updated for affected consumers?
- [ ] Is `PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md` updated?
- [ ] Do tests cover minimum required scenarios (error return + exception + resilience)?
- [ ] Are logging and metrics verified (if applicable)?
- [ ] Does consumer process remain alive after publish errors?

### Maintenance Rules

**Rule 1: New Consumer or New `router_nats:publish/2` Usage**

When adding a new JetStream consumer or new usage of `router_nats:publish/2`:
- Add minimum required publish error tests (error return + exception + resilience)
- Update `PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md`

**Rule 2: Changes to Logging/Metrics Requirements**

When requirements change for log levels, log format, or metrics:
- Synchronize consumer implementation and tests
- Update mapping document and testing recommendations

**Rule 3: CI Stability Monitoring**

Periodically (before major releases, after CI changes):
- Check for flaky test failures
- Verify test execution time remains acceptable
- Stabilize tests or document environment requirements if needed

**See**: `apps/otp/router/docs/dev/PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md` - "Maintenance Process" section for detailed guidelines.

## Nightly Property Tests

### Overview

Nightly property tests run with increased test count (300 tests) and fixed seed for reproducibility. Counterexamples are automatically collected and saved as artifacts.

### Configuration

Nightly property tests are configured via environment variables:

- `PROPERTY_TEST_NUMTESTS`: Number of tests to run (default: 100, nightly: 300)
- `PROPERTY_TEST_SEED`: Fixed seed for reproducibility (nightly: YYYYMMDD format)
- `PROPERTY_TEST_MODE`: Set to `nightly` for nightly runs

### Running Nightly Tests Locally

```bash
cd apps/otp/router
export PROPERTY_TEST_NUMTESTS=300
export PROPERTY_TEST_SEED=20250127
export PROPERTY_TEST_MODE=nightly
rebar3 as test ct \
  --suite test/router_policy_store_prop_SUITE \
  --suite test/router_options_merge_prop_SUITE \
  --suite test/router_normalize_boolean_prop_SUITE \
  --suite test/router_decider_prop_SUITE
```

### CI/CD Integration

Nightly property tests run automatically via GitHub Actions:

- **Schedule**: Daily at 2:00 AM UTC
- **Manual trigger**: Available via `workflow_dispatch`
- **Test count**: 300 tests per property
- **Fixed seed**: YYYYMMDD format for reproducibility
- **Artifacts**: CT logs (30 days), counterexamples (90 days), test summary (30 days)

### Counterexample Collection

Counterexamples are automatically extracted from CT logs and saved as artifacts:

1. **Automatic extraction**: Counterexamples are extracted from CT logs after test execution
2. **Artifact storage**: Counterexamples are saved in `counterexamples/` directory
3. **Retention**: Counterexamples are kept for 90 days for analysis
4. **Format**: Each counterexample includes full log context and test details

### Viewing Counterexamples

1. Go to GitHub Actions → Nightly Property Tests workflow
2. Select the failed run
3. Download `nightly-counterexamples-<otp-version>` artifact
4. Review counterexample files in `counterexamples/` directory

## Risk Mitigation

### 1. Test Execution Location

**Risk**: Running `rebar3 ct` from repo root with custom flags can break `rebar_prv_common_test` option parsing.

**Mitigation**:
- ✅ All CI/CD workflows use `working-directory: apps/otp/router`
- ✅ `Makefile` targets include location check via `scripts/check_test_run_location.sh`
- ✅ Documentation emphasizes running from app directory

**Best Practice**: Always run tests from `apps/otp/router` directory:
```bash
cd apps/otp/router
rebar3 ct -j 4
```

### 2. INDEX_TABLE Bag Table Duplicates

**Risk**: Complex race conditions in `bag` table can cause duplicates during `upsert -> delete -> upsert` cycles.

**Mitigation**:
- ✅ Explicit deduplication logic in `do_upsert_policy`:
  - Checks for existing entries before insert
  - Detects and cleans up duplicates automatically
  - Verifies insertion succeeded (defensive check)
- ✅ Property tests (`prop_index_deduplication`) verify consistency
- ✅ `ets:match_delete` removes all matching entries before insert

**Best Practice**: When extending index logic, always:
1. Check for existing entries before insert
2. Handle duplicate detection and cleanup
3. Verify consistency after operations

### 3. Telemetry Overhead in Tests

**Risk**: Excessive telemetry events slow down CI/CD execution.

**Mitigation**:
- ✅ `telemetry_enabled = false` in test profile (`rebar.config`)
- ✅ `router_telemetry_helper` module checks `telemetry_enabled` before emitting events
- ✅ All telemetry calls use `router_telemetry_helper:execute/3` and `router_telemetry_helper:span/3`
- ✅ Test mode silently skips telemetry (no overhead)

**Implementation**: `router_telemetry_helper` provides safe wrappers:
```erlang
%% Only emits if telemetry_enabled = true
router_telemetry_helper:execute([router_core, route], #{duration_us => 100}, #{tenant_id => <<"t1">>}).
```

### 4. Parallel Test Execution Safety

**Risk**: Parallel tests may conflict on global resources (ETS tables, registered processes).

**Mitigation**:
- ✅ All tests use ephemeral ports (port 0, OS-assigned)
- ✅ ETS tables accessed only through gen_server (serialized access)
- ✅ No direct ETS access from test code
- ✅ Test isolation: each test suite uses separate tenant IDs
- ✅ CT groups: `parallel` for unit tests, `sequence` for integration tests

**Global Resources**:
- **ETS tables**: `named_table` but accessed via gen_server (safe)
- **Registered processes**: Each test uses unique names or ephemeral ports
- **NATS subjects**: Unique per test via tenant_id and message_id

**Best Practice**: When adding new global resources:
1. Use gen_server for serialized access
2. Use ephemeral ports for network resources
3. Use unique identifiers (tenant_id, message_id) for isolation
4. Test in parallel CT groups to verify safety

**Checklist for New Global Resources**:
- ✅ **Name**: Is the resource name unique and namespaced?
- ✅ **Isolation**: Can multiple test instances coexist (ephemeral ports, unique IDs)?
- ✅ **Lifecycle**: Is cleanup handled in `end_per_suite` or `end_per_testcase`?
- ✅ **Access**: Is access serialized (gen_server) or thread-safe (ETS with proper options)?
- ✅ **State**: Does the resource maintain state between tests? If yes, is it reset?
- ✅ **Parallel Safety**: Test in `parallel` CT group to verify no conflicts

## Performance Optimizations Applied

### 1. Parallel Execution

**Problem**: Sequential test execution without `-j` flag and without CT groups.

**Solution**:
- ✅ All unit tests use `parallel` CT groups
- ✅ All integration tests use `sequence` CT groups
- ✅ CI/CD runs with `rebar3 ct -j 4`
- ✅ Documentation emphasizes parallel execution

**Result**: 3-4x faster test execution.

### 2. Bounded Waits

**Problem**: Blocking `timer:sleep` calls waiting for external events (NATS/ACKS).

**Solution**:
- ✅ All `timer:sleep` replaced with `test_helpers:wait_for_*` functions
- ✅ Bounded polling loops with 10ms intervals
- ✅ Timeout limits prevent infinite waits
- ✅ Early exit when condition is met

**Result**: Tests complete as soon as conditions are met, not after fixed delays.

### 3. ETS Initialization Optimization

**Problem**: Heavy ETS initialization with heir/transfer, timeouts, and retries in tests.

**Solution**:
- ✅ `disable_heir = true` in test profile (`rebar.config`)
- ✅ All test suites set `disable_heir` in `init_per_suite`
- ✅ Fault tolerance tests explicitly keep heir (for testing heir mechanism)
- ✅ No transfer timeouts or retries in test mode

**Result**: Faster test initialization, no unnecessary waits.

### 4. Property Test Contracts

**Problem**: Incorrect CT contracts in property tests (`bad_return_value`) causing unnecessary initializations.

**Solution**:
- ✅ All property test functions accept `Config` parameter (`prop_*(_Config)`)
- ✅ Skip functions properly return `{skip, Reason}`
- ✅ Property functions return `ok` on success or `{skip, Reason}` when PropEr unavailable
- ✅ No 0-arity property test functions

**Result**: No unnecessary initializations, proper CT contract compliance.

## Low-Priority Recommendations

### ETS Invariants

**Recommendation**: Add explicit assertions when creating ETS tables to catch configuration errors early.

**Implementation**:
- ✅ Added `ets:info(Table, type)` checks after table creation
- ✅ Added `ets:info(Table, keypos)` checks after table creation
- ✅ Logs errors if invariants are violated (only in production, not in tests)

**Example**:
```erlang
%% Verify table invariants after creation
case ets:info(Table, type) of
    set -> ok;  %% Expected
    _ -> router_logger:error("Table type invariant violated", ...)
end.
```

### Meck in Parallel Tests

**Recommendation**: When expanding test coverage, ensure unique mock module/process names in CT groups to avoid conflicts.

**Best Practice**:
- Use unique names per test: `meck:new(router_nats_#{test_name}, [passthrough])`
- Clean up in `end_per_testcase`: `meck:unload(router_nats_#{test_name})`
- Document mock naming convention in test helpers

### Index Consistency Logging

**Recommendation**: Ensure index inconsistency logs don't create noise in tests.

**Implementation**:
- ✅ `audit_index_consistency` checks `telemetry_enabled` before logging
- ✅ Logs only in production mode (`telemetry_enabled=true`)
- ✅ Silent in test mode to reduce noise

### Random Seed Control

**Recommendation**: For rare flaky tests, add periodic `rand:seed/3` control in test init (if using generators outside PropEr).

**Example**:
```erlang
init_per_suite(Config) ->
    %% Fix random seed for deterministic testing
    rand:seed(exs1024, {1, 2, 3}),
    Config.
```

**Note**: PropEr uses its own seed via `test_helpers:get_proper_options()`, so this applies only to non-PropEr random usage.

### Error Mapping Decision Matrix

**Recommendation**: Use centralized error mapping (`router_error:to_grpc/1`) for consistent gRPC status codes.

**Decision Matrix** (when to choose which gRPC status code):

| Error Type | gRPC Status | Code | When to Use | Examples |
|------------|-------------|------|-------------|----------|
| **Client Input Errors** | `INVALID_ARGUMENT` | 3 | Client provided invalid/missing input | `missing_tenant_id`, `invalid_policy`, `invalid_request`, `missing_message` |
| **Resource Not Found** | `NOT_FOUND` | 5 | Requested resource doesn't exist | `policy_not_found` |
| **Quota/Budget Limits** | `RESOURCE_EXHAUSTED` | 8 | Rate limits, quotas, budgets exceeded | `rate_limit_exceeded`, `quota_exceeded` |
| **Service Unavailable** | `UNAVAILABLE` | 14 | Service temporarily unavailable (retryable) | `service_down`, `timeout` |
| **Internal Errors** | `INTERNAL` | 13 | Unexpected internal server errors (non-retryable) | `no_provider_available`, `internal_error`, unknown errors |

**Decision Criteria**:

1. **INVALID_ARGUMENT (3)**: Client can fix by changing input
   - Missing required fields
   - Invalid format/type
   - Validation failures

2. **NOT_FOUND (5)**: Resource doesn't exist (may be created later)
   - Policy not found
   - Tenant not found
   - Resource ID doesn't exist

3. **RESOURCE_EXHAUSTED (8)**: Quota/budget limits reached
   - Rate limit exceeded
   - Quota exceeded
   - Budget exhausted

4. **UNAVAILABLE (14)**: Temporary service unavailability (client should retry)
   - Service temporarily down
   - Timeout (may succeed on retry)
   - Temporary overload

5. **INTERNAL (13)**: Unexpected internal errors (client should not retry immediately)
   - No provider available (internal routing failure)
   - Unexpected exceptions
   - Unknown error types

**Implementation**: See `router_error.erl` for centralized mapping table.

### Centralized Error Mapping

**Recommendation**: Use `router_error:to_grpc/1` for all error mapping to ensure consistency.

**Implementation**:
- ✅ `router_error.erl`: Centralized error mapping module
- ✅ Uses `persistent_term` for performance (cached mapping table)
- ✅ Supports context override for custom error messages
- ✅ Maps unknown errors to `INTERNAL (13)`

**Usage**:
```erlang
%% In router_grpc.erl
{Status, Message} = router_error:to_grpc(ErrorReason, ErrorContext),
throw({grpc_error, {Status, Message}}).
```

**Benefits**:
- Single source of truth for error mapping
- Easy to update mapping policy
- Consistent error codes across all endpoints
- Unit tests ensure no regressions

### ETS Guard Module

**Recommendation**: Use `router_ets_guard:ensure_table/2` for ETS table invariant verification.

**Implementation**:
- ✅ `router_ets_guard.erl`: Centralized ETS invariant verification
- ✅ Checks: `type`, `keypos`, `read_concurrency`, `write_concurrency`, `compressed`
- ✅ Logs violations only in production (not in tests)

**Usage**:
```erlang
%% In router_policy_store.erl
TableSpec = #{
    type => set,
    keypos => 1,
    read_concurrency => true,
    write_concurrency => false,
    compressed => false
},
router_ets_guard:ensure_table(Table, TableSpec).
```

**Benefits**:
- Catches configuration errors early in integration tests
- Explicit table specifications (no hidden defaults)
- Centralized verification logic

## Troubleshooting

### Test Timeouts

**Symptom**: Tests fail with timeout errors

**Solution**:
- Increase timeout in bounded waits: `wait_for_condition(Fun, 2000)` (2 seconds)
- Check for blocking operations (remove `timer:sleep`)
- Verify test isolation (ephemeral ports, clean state)

### PropEr Not Available

**Symptom**: Property tests skip with "PropEr not available"

**Solution**:
- Ensure test profile: `rebar3 as test ct`
- Check PropEr dependency: `rebar3 as test deps`
- Verify `proper.hrl` is included in test files

### Port Conflicts

**Symptom**: Tests fail with port binding errors

**Solution**:
- Use ephemeral ports (port 0) in test configuration
- Run tests sequentially if parallel execution causes conflicts
- Check for leftover processes: `rebar3 clean`

### Slow Test Execution

**Symptom**: Tests take too long

**Solution**:
- Use parallel execution: `rebar3 ct -j 4`
- Enable CT groups for parallel unit tests
- Minimize telemetry: `telemetry_enabled = false`
- Disable ETS heir: `disable_heir = true`
- Reduce property test iterations in CI: `{numtests, 50}`

## Best Practices Summary

1. **Always use parallel execution**: `rebar3 ct -j 4`
2. **Use bounded waits**: `test_helpers` module instead of `timer:sleep`
3. **CT groups for organization**: `parallel` for unit tests, `sequence` for integration
4. **Test profile for property tests**: `rebar3 as test ct`
5. **Minimize telemetry in tests**: `telemetry_enabled = false`
6. **Ephemeral ports**: Use port 0 for test isolation
7. **JUnit reports**: Enable `cth_surefire` for CI/CD integration
8. **Run from app directory**: Avoid root-level flag issues

## References

- [Property Testing Guide](PROPERTY_TESTING.md)
- [Full Documentation](FULL_DOCS.md)
- [Operational Guide](OPERATIONAL_GUIDE.md)

