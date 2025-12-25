# Router Observability Tests

## Overview

This directory contains unit, integration, and performance tests for Router observability features (CP1 compliant).

## Test Structure

```
test/
├── router_observability_SUITE.erl          # Unit tests (12 tests + 7 edge case tests)
├── router_health_integration_SUITE.erl     # Integration tests (gRPC health endpoint)
└── router_observability_performance_SUITE.erl # Performance tests (5 tests)
```

## Test Suites

### Unit Tests (`router_observability_SUITE.erl`)

**Total**: 19 tests (12 core + 7 edge case)

**Core Tests**:
- `test_log_format_json` - Validates JSON log format
- `test_log_required_fields` - Validates required fields (timestamp, level, component, message)
- `test_log_optional_fields` - Validates optional fields (context, CP1 fields)
- `test_correlation_fields` - Validates CP1 correlation fields at top level
- `test_error_code_and_latency` - Validates error_code and latency_ms fields
- `test_pii_filtering` - Validates PII filtering
- `test_secret_pattern_detection` - Validates secret pattern detection
- `test_log_levels` - Validates all log levels (ERROR, WARN, INFO, DEBUG)
- `test_health_endpoint` - Validates gRPC health endpoint configuration
- `test_nats_error_logging` - Validates NATS error logging
- `test_routing_error_logging` - Validates routing error logging
- `test_invalid_payload_logging` - Validates invalid payload logging
- `test_internal_error_logging` - Validates internal error logging

**Edge Case Tests**:
- `test_very_long_message` - Very long log messages (100KB)
- `test_very_long_cp1_fields` - Very long CP1 field values (1000 chars)
- `test_empty_null_cp1_fields` - Empty/null CP1 fields
- `test_special_characters` - Special characters (JSON escaping)
- `test_very_large_context` - Very large context objects (1000 fields)
- `test_invalid_json_in_context` - Invalid JSON in context (nested structures)
- `test_concurrent_logging` - Concurrent logging (10 processes, 100 logs each)

### Integration Tests (`router_health_integration_SUITE.erl`)

**Total**: Multiple tests for gRPC health endpoint

**Tests**:
- Health endpoint availability
- Health check response format validation
- Health status values (SERVING, UNKNOWN, NOT_SERVING)
- Empty service name health check
- Specific service name health check
- Error handling

### Performance Tests (`router_observability_performance_SUITE.erl`)

**Total**: 5 tests

**Tests**:
- `test_log_generation_throughput` - Logging throughput (logs/second)
- `test_pii_filtering_performance` - PII filtering latency (microseconds per entry)
- `test_json_serialization_performance` - JSON serialization performance
- `test_memory_usage_during_logging` - Memory overhead (bytes per log entry)
- `test_concurrent_logging_performance` - Concurrent logging throughput

## Building Tests

### Using rebar3

```bash
cd apps/otp/router

# Get dependencies
rebar3 get-deps

# Compile
rebar3 compile
```

## Running Tests

### All Observability Tests

```bash
cd apps/otp/router

# Run all observability tests
rebar3 ct --suite test/router_observability_SUITE \
          --suite test/router_health_integration_SUITE \
          --suite test/router_observability_performance_SUITE
```

### Unit Tests Only

```bash
cd apps/otp/router

# Run unit tests
rebar3 ct --suite test/router_observability_SUITE
```

### Integration Tests Only

```bash
cd apps/otp/router

# Run integration tests
rebar3 ct --suite test/router_health_integration_SUITE
```

### Performance Tests Only

```bash
cd apps/otp/router

# Run performance tests
rebar3 ct --suite test/router_observability_performance_SUITE
```

### Using Makefile

```bash
cd apps/otp/router

# Run all tests with coverage
make test-coverage

# Run tests only
make test

# Generate coverage report only
make coverage-report
```

## Test Examples

### Example 1: Basic Logging Test

```erlang
test_log_format_json(_Config) ->
    Message = ~"Test log message",
    Context = #{
        ~"tenant_id" => ~"test_tenant",
        ~"trace_id" => ~"trace_123"
    },
    
    router_logger:info(Message, Context),
    
    %% Verify log file exists and contains valid JSON
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    {ok, LogContent} = file:read_file(LogFile),
    Lines = binary:split(LogContent, ~"\n", [global]),
    ValidLines = [L || L <- Lines, byte_size(L) > 0],
    
    %% Verify each line is valid JSON
    lists:foreach(fun(Line) ->
        LogEntry = jsx:decode(Line, [return_maps]),
        ?assert(is_map(LogEntry), "Log entry should be a map")
    end, ValidLines),
    
    ok.
```

### Example 2: CP1 Correlation Fields Test

```erlang
test_correlation_fields(_Config) ->
    Message = ~"Test CP1 correlation fields",
    Context = #{
        ~"tenant_id" => ~"test_tenant",
        ~"trace_id" => ~"trace_abc123",
        ~"run_id" => ~"run_789",
        ~"flow_id" => ~"flow_456",
        ~"step_id" => ~"step_123"
    },
    
    router_logger:info(Message, Context),
    
    %% Read log file and verify CP1 fields are at top level
    {ok, LogContent} = file:read_file(LogFile),
    Lines = binary:split(LogContent, ~"\n", [global]),
    [LastLine | _] = [L || L <- Lines, byte_size(L) > 0],
    LogEntry = jsx:decode(LastLine, [return_maps]),
    
    %% Verify CP1 fields are at top level (not in correlation object)
    ?assert(maps:is_key(~"tenant_id", LogEntry), "tenant_id should be at top level"),
    ?assert(maps:is_key(~"trace_id", LogEntry), "trace_id should be at top level"),
    ?assert(maps:is_key(~"run_id", LogEntry), "run_id should be at top level"),
    
    ok.
```

### Example 3: PII Filtering Test

```erlang
test_pii_filtering(_Config) ->
    Message = ~"Test PII filtering",
    Context = #{
        ~"api_key" => ~"sk-1234567890abcdef",
        ~"password" => ~"secret_password",
        ~"token" => ~"bearer_token_123"
    },
    
    router_logger:info(Message, Context),
    
    %% Read log file and verify PII is filtered
    {ok, LogContent} = file:read_file(LogFile),
    [LastLine | _] = [L || L <- binary:split(LogContent, ~"\n", [global]), byte_size(L) > 0],
    LogEntry = jsx:decode(LastLine, [return_maps]),
    ContextMap = maps:get(~"context", LogEntry, #{}),
    
    %% Verify PII fields are filtered
    ?assertEqual(~"[REDACTED]", maps:get(~"api_key", ContextMap), "api_key should be filtered"),
    ?assertEqual(~"[REDACTED]", maps:get(~"password", ContextMap), "password should be filtered"),
    
    ok.
```

## Debugging Guide

### Test Failures

**Problem**: Tests fail with "Log file not created" error.

**Solutions**:
1. Check that `telemetry_enabled` is set to `true` in test configuration
2. Verify log directory exists and is writable:
   ```bash
   mkdir -p /tmp/router_test_logs
   chmod 755 /tmp/router_test_logs
   ```
3. Check application environment:
   ```erlang
   application:get_env(beamline_router, telemetry_enabled, false)
   ```

**Problem**: Tests fail with "Invalid JSON" error.

**Solutions**:
1. Check for special characters in log messages that need escaping
2. Verify JSON serialization is working:
   ```erlang
   jsx:encode(#{~"test" => ~"value"})
   ```
3. Review `router_logger.erl` implementation for JSON encoding issues

**Problem**: Performance tests fail thresholds.

**Solutions**:
1. Check system load (CPU, memory)
2. Run tests multiple times to get average values
3. Adjust thresholds if system is under heavy load
4. Review performance test implementation for bottlenecks

### Viewing Test Logs

**During Test Execution**:
```bash
# View test logs in real-time
tail -f /tmp/router_test_logs/router_$(date +%Y-%m-%d).jsonl

# Parse JSON logs with jq
cat /tmp/router_test_logs/router_2025-11-30.jsonl | jq '.'

# Filter by level
cat /tmp/router_test_logs/router_2025-11-30.jsonl | jq 'select(.level == "ERROR")'

# Filter by trace_id
cat /tmp/router_test_logs/router_2025-11-30.jsonl | jq 'select(.trace_id == "trace_abc123")'
```

**Common Test Logs**:
- Test logs: `/tmp/router_test_logs/router_YYYY-MM-DD.jsonl`
- Common Test logs: `_build/test/logs/ct_run.*/`

### Running Tests in Debug Mode

**Enable DEBUG logging**:
```erlang
%% In test configuration
{log_level, debug}
```

**View DEBUG logs**:
```bash
cat /tmp/router_test_logs/router_2025-11-30.jsonl | jq 'select(.level == "DEBUG")'
```

### Common Test Issues

**Issue**: Tests timeout.

**Solution**: Increase test timeout in `rebar.config`:
```erlang
{ct_opts, [
    {timeout, 60000}  %% 60 seconds
]}
```

**Issue**: Tests fail due to missing dependencies.

**Solution**: Install dependencies:
```bash
rebar3 get-deps
rebar3 compile
```

**Issue**: Performance tests show inconsistent results.

**Solution**: Run multiple iterations and average:
```bash
for i in {1..5}; do
    rebar3 ct --suite test/router_observability_performance_SUITE
done
```

## Coverage Metrics

### Running Coverage Analysis

```bash
cd apps/otp/router

# Run tests with coverage
rebar3 ct --cover --suite test/router_observability_SUITE

# Generate coverage report
rebar3 cover

# Or use the coverage script
bash scripts/generate_coverage.sh
```

### Coverage Thresholds

**Target Coverage**:
- **Line coverage**: >80%
- **Branch coverage**: >70%
- **Function coverage**: >90%

### Viewing Coverage Reports

**HTML Report**:
```bash
# Open coverage report
open _build/test/cover/html/index.html

# Or view in browser
file:///path/to/router/_build/test/cover/html/index.html
```

**Coverage Summary**:
```bash
# View coverage summary
cat _build/test/cover/cover.log | grep -E "Coverage|covered"
```

### Coverage Metrics Interpretation

**Line Coverage**: Percentage of code lines executed during tests.

**Branch Coverage**: Percentage of code branches (if/else, case) executed.

**Function Coverage**: Percentage of functions called during tests.

**Improving Coverage**:
1. Identify untested code paths in coverage report
2. Add tests for uncovered functions
3. Add edge case tests for uncovered branches
4. Review coverage report regularly

## Performance Test Interpretation

### Performance Metrics

**Log Generation Throughput**:
- **Threshold**: >= 1000 logs/second
- **Interpretation**: Higher is better. Measures raw logging performance.

**PII Filtering Latency**:
- **Threshold**: < 100 microseconds per entry
- **Interpretation**: Lower is better. Measures PII filtering overhead.

**JSON Serialization Latency**:
- **Threshold**: < 50 microseconds per entry
- **Interpretation**: Lower is better. Measures JSON encoding performance.

**Memory Overhead**:
- **Threshold**: < 1024 bytes per log entry
- **Interpretation**: Lower is better. Measures memory usage per log.

**Concurrent Logging Throughput**:
- **Threshold**: >= 5000 logs/second
- **Interpretation**: Higher is better. Measures concurrent logging performance.

### Benchmarking

**Run Benchmark Script**:
```bash
cd apps/otp/router

# Run benchmark with default parameters
bash scripts/benchmark_observability.sh

# Run with custom iterations
BENCHMARK_ITERATIONS=20000 bash scripts/benchmark_observability.sh
```

**Benchmark Report**:
- Location: `reports/benchmark/observability_benchmark_YYYYMMDD_HHMMSS.json`
- Format: JSON with metrics and thresholds
- View: `cat reports/benchmark/observability_benchmark_*.json | jq '.'`

## CI/CD Integration

### GitHub Actions

Tests are automatically run in `.github/workflows/router-observability-tests.yml`:
- Unit tests
- Integration tests
- E2E test script (if Router is running)

### GitLab CI

Tests are automatically run in `.gitlab-ci.yml`:
- `router-observability-tests` job

### Drone CI

Tests are automatically run in `.drone.yml`:
- `router-observability-tests` pipeline

## Test Execution Examples

### Example 1: Run All Tests

```bash
cd apps/otp/router

# Run all observability tests
rebar3 ct --suite test/router_observability_SUITE \
          --suite test/router_health_integration_SUITE \
          --suite test/router_observability_performance_SUITE
```

### Example 2: Run Tests with Coverage

```bash
cd apps/otp/router

# Run tests with coverage
rebar3 ct --cover --suite test/router_observability_SUITE

# Generate coverage report
rebar3 cover

# View coverage report
open _build/test/cover/html/index.html
```

### Example 3: Run Performance Tests

```bash
cd apps/otp/router

# Run performance tests
rebar3 ct --suite test/router_observability_performance_SUITE

# Or use benchmark script
bash scripts/benchmark_observability.sh
```

### Example 4: Run Specific Test Group

```bash
cd apps/otp/router

# Run only edge case tests
rebar3 ct --suite test/router_observability_SUITE --group edge_case_tests
```

## References

- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability documentation
- `apps/otp/router/scripts/generate_coverage.sh` - Coverage generation script
- `apps/otp/router/scripts/benchmark_observability.sh` - Performance benchmarking script
- `docs/OBSERVABILITY.md` - Unified observability requirements
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/archive/dev/ROUTER_OBSERVABILITY_TEST.md` - E2E test script documentation

## Test Maintenance

### Adding New Tests

1. **Unit Tests**: Add to `router_observability_SUITE.erl`
2. **Integration Tests**: Add to `router_health_integration_SUITE.erl`
3. **Performance Tests**: Add to `router_observability_performance_SUITE.erl`

### Test Naming Conventions

- **Unit tests**: `test_<feature>`
- **Integration tests**: `test_<component>_<feature>`
- **Performance tests**: `test_<metric>_<measurement>`

### Test Documentation

- Document test purpose in `@doc` comments
- Include test examples in this README
- Update test coverage table when adding new tests

---

**Status**: ✅ **All tests passing**  
**Coverage**: See coverage reports for current metrics  
**Last Updated**: 2025-11-30

