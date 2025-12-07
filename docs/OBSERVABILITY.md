# Observability (CP1 → CP2/Pre‑release)

This document describes the observability features for Router in CP1, including structured JSON logging and health endpoints.

CP2/Pre‑release extends Router observability with full OpenTelemetry tracing (OTLP export), Prometheus `/metrics` (HTTP, port 9001), and Grafana dashboards using CP1 correlation fields as filters. See `docs/dev/CP2_OBSERVABILITY_PLAN.md` and `docs/OBSERVABILITY_CP2_TEST_PROFILE.md`.

## Structured JSON Logging

Router uses structured JSON logging (JSONL format) for all log entries. Logs are written to files in the format `router_YYYY-MM-DD.jsonl`.

### Log Format

Each log entry is a JSON object with the following structure:

```json
{
  "timestamp": "2025-11-30T12:00:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Request processed successfully",
  "tenant_id": "tenant_123",
  "run_id": "run_789",
  "flow_id": "flow_456",
  "step_id": "step_123",
  "trace_id": "abc123def456",
  "error_code": "ROUTER_ERROR_001",
  "latency_ms": 250,
  "context": {
    "subject": "beamline.router.v1.decide",
    "policy_id": "policy_456"
  }
}
```

### Required Fields

- **timestamp**: ISO-8601 timestamp in UTC (format: `YYYY-MM-DDTHH:MM:SS.ssssssZ` with 6 digits for microseconds)
- **level**: Log level (`ERROR`, `WARN`, `INFO`, `DEBUG`)
- **component**: Component identifier (always `"router"` for Router component)
- **message**: Human-readable log message

### Optional Fields

#### CP1 Correlation Fields

CP1 correlation fields are placed at the **top level** of the log entry (not in a nested object):

- **tenant_id**: Tenant identifier (when available)
- **run_id**: Run identifier for workflow runs (when available)
- **flow_id**: Flow identifier for multi-step flows (when available)
- **step_id**: Step identifier within a flow (when available, optional for Router)
- **trace_id**: Trace identifier for distributed tracing (when available)

All CP1 fields are optional. They are only included when the corresponding context is available.

#### Error Information

- **error_code**: Error code string (e.g., `"ROUTER_ERROR_001"`, `"NATS_CONNECTION_FAILED"`)
- **latency_ms**: Request latency in milliseconds (integer)

#### Context

- **context**: Additional structured context (JSON object)

### Log Levels

- **ERROR**: Critical errors requiring immediate attention
- **WARN**: Warnings, potential issues
- **INFO**: Informational messages (requests, responses, state changes)
- **DEBUG**: Detailed debugging information

### PII Filtering

All log entries are automatically filtered for PII (Personally Identifiable Information) and secrets:

**Filtered Fields**:
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`
- NATS headers: `bearer`, `x-api-key`, `x-auth-token`, `x-authorization` (case-insensitive)

**Replacement**: All PII fields are replaced with `"[REDACTED]"` before logging.

**Pattern Detection**: The logger also detects secret patterns in values (e.g., `"API key sk-123..."`) and masks them.

### Log File Location

Log files are written to:
- Default: `.windsurf/reports/router_YYYY-MM-DD.jsonl`
- Configurable via `beamline_router.log_dir` application environment variable

### Usage Examples

#### Basic Logging

```erlang
router_logger:info(<<"Request received">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"subject">> => <<"beamline.router.v1.decide">>
}).
```

#### Logging with Correlation

```erlang
router_logger:info(<<"Routing decision made">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"run_id">> => <<"run_789">>,
    <<"flow_id">> => <<"flow_456">>,
    <<"step_id">> => <<"step_123">>,
    <<"policy_id">> => <<"policy_456">>
}).
```

#### Logging Errors with Error Code and Latency

```erlang
router_logger:error(<<"NATS connection failed">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"error_code">> => <<"NATS_CONNECTION_FAILED">>,
    <<"latency_ms">> => 5000,
    <<"subject">> => <<"beamline.router.v1.decide">>
}).
```

## Health Endpoints

Router provides health checking via **gRPC health service** (not HTTP). This differs from the HTTP `/_health` specification used by Gateway and Worker, but is acceptable for Router as it uses gRPC as its primary protocol.

### gRPC Health Service

**Service**: `grpc.health.v1.Health`

**Method**: `Check`

**Port**: 9000 (default, configurable via `beamline_router.grpc_port`)

**Authentication**: Required (API key in gRPC metadata)

### Health Check via Admin API

Router also provides a health check via the Admin gRPC API:

**RPC**: `GetValidatorsHealth`

**Request**: `GetValidatorsHealthRequest` (optional `tenant_id`)

**Response**: `GetValidatorsHealthResponse` with validator statuses:

```protobuf
message GetValidatorsHealthResponse {
  repeated ValidatorStatus validators = 1;
}

message ValidatorStatus {
  string name = 1;
  string status = 2;  // "serving" | "not_serving" | "unknown"
  int64 timestamp_ms = 3;
}
```

**Example Response**:
```json
{
  "validators": [
    {
      "name": "grpc_health",
      "status": "serving",
      "timestamp_ms": 1706356800000
    }
  ]
}
```

### Health Check Tools

#### Using grpc_health_probe

```bash
grpc_health_probe -addr=localhost:9000 -service=grpc.health.v1.Health
```

#### Using grpcurl

```bash
grpcurl -plaintext -H "x-api-key: YOUR_API_KEY" \
  localhost:9000 grpc.health.v1.Health/Check
```

#### Using Admin API

```bash
grpcurl -plaintext -H "x-api-key: YOUR_API_KEY" \
  -d '{}' \
  localhost:9000 beamline.flow.v1.RouterAdmin/GetValidatorsHealth
```

### Health Status Values

- **serving**: Service is healthy and ready to accept requests
- **not_serving**: Service is unhealthy and should not receive traffic
- **unknown**: Health status is unknown

## Configuration

### Logging Configuration

Logging can be configured via application environment variables:

```erlang
%% Enable/disable logging
{telemetry_enabled, true},

%% Log directory
{log_dir, ".windsurf/reports"}
```

### Health Endpoint Configuration

Health endpoint is configured via gRPC server settings:

```erlang
%% Enable/disable gRPC
{grpc_enabled, true},

%% gRPC port (default: 9000)
{grpc_port, 9000}
```

## Local Development

### Viewing Logs

Logs are written to JSONL files. To view logs:

```bash
# View latest log file
tail -f .windsurf/reports/router_$(date +%Y-%m-%d).jsonl

# Parse JSON logs with jq
cat .windsurf/reports/router_2025-11-30.jsonl | jq '.'

# Filter by level
cat .windsurf/reports/router_2025-11-30.jsonl | jq 'select(.level == "ERROR")'

# Filter by trace_id
cat .windsurf/reports/router_2025-11-30.jsonl | jq 'select(.trace_id == "trace_abc123")'

# Filter by run_id
cat .windsurf/reports/router_2025-11-30.jsonl | jq 'select(.run_id == "run_789")'
```

### Testing Health Endpoint

```bash
# Check health via grpc_health_probe
grpc_health_probe -addr=localhost:9000

# Check health via Admin API
grpcurl -plaintext -H "x-api-key: test_key" \
  -d '{}' \
  localhost:9000 beamline.flow.v1.RouterAdmin/GetValidatorsHealth
```

## MVP Scope

**Included in CP1**:
- ✅ Structured JSON logging with correlation fields
- ✅ Error codes and latency tracking
- ✅ PII filtering
- ✅ Health endpoint via gRPC

**Excluded from CP1** (future iterations):
- ❌ Prometheus metrics
- ❌ Grafana dashboards
- ❌ Loki log aggregation
- ❌ Distributed tracing (OpenTelemetry)
- ❌ Alertmanager integration

**CP2 Implementation**: See `docs/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` for CP2 Prometheus metrics specification.

## Prometheus Metrics (CP2)

### Metrics Endpoint

Router exports Prometheus metrics via HTTP endpoint:

- **Protocol**: HTTP
- **Path**: `GET /metrics`
- **Port**: 9001 (separate from gRPC health port 9000)
- **Content-Type**: `text/plain; version=0.0.4; charset=utf-8`
- **Format**: Prometheus text format (RFC 4180)

**Configuration**:
- Port can be configured via `beamline_router.metrics_port` (default: 9001)
- Endpoint is started via `router_metrics_http:start/0`

**Example**:
```bash
# Query metrics endpoint
curl http://localhost:9001/metrics
```

### Metrics Export Contract

**Primary Export**: HTTP `/metrics` endpoint (port 9001)
- **Purpose**: Production metrics scraping by Prometheus
- **Format**: Prometheus text format with HELP and TYPE headers
- **Availability**: Always available when Router is running (CP2)

**Secondary Export**: File dump (for testing/debugging)
- **Purpose**: Local testing and validation
- **Format**: Same Prometheus text format
- **Usage**: `router_prometheus:dump/1` for test files
- **Location**: `metrics_dump/metrics.prom` (default)

**Synchronization with Gateway**:
- Router: HTTP `/metrics` on port 9001
- Gateway: HTTP `/metrics` on port 3001
- Both use same Prometheus text format
- Both follow CP2 metrics specification

### Base Metrics Contract

**Required Metrics** (must be present in all dumps):

**JetStream Metrics**:
- `router_jetstream_ack_total` (counter) - Total JetStream acknowledgements
- `router_redelivery_total` (counter) - Total message redeliveries
- `router_dlq_total` (counter) - Total messages sent to Dead Letter Queue

**Idempotency Metrics**:
- `router_idem_hits_total` (counter) - Total idempotency cache hits
- `router_idem_miss_total` (counter) - Total idempotency cache misses

**ACL Metrics**:
- `router_acl_allowed_total` (counter) - Total ACL allowed decisions
- `router_acl_denied_total` (counter) - Total ACL denied decisions

**Reference**: See `docs/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` for complete metrics specification.

## Testing

### Test Script

Router observability can be tested using the E2E test script:

```bash
# Test Router observability (requires Router running on port 9000)
bash scripts/observability/test_router_observability.sh
```

**Prerequisites**:
- Router running on port 9000 (default)
- `grpc_health_probe` or `grpcurl` installed
- `jq` (optional, for JSON parsing)

**See**: `docs/dev/ROUTER_OBSERVABILITY_TEST.md` for detailed testing instructions.

### Unit Tests

Router observability is covered by unit tests in `test/router_observability_SUITE.erl`:

```bash
# Run observability tests
rebar3 ct --suite test/router_observability_SUITE
```

**Test Coverage**: 19 tests (12 core + 7 edge case)
- Log format validation
- CP1 correlation fields
- PII filtering
- Log levels
- Error logging scenarios
- Edge cases (long messages, special characters, large context)

### Integration Tests

Router gRPC health endpoint is covered by integration tests in `test/router_health_integration_SUITE.erl`:

```bash
# Run health endpoint integration tests
rebar3 ct --suite test/router_health_integration_SUITE
```

**Test Coverage**:
- Health endpoint availability
- Health check response format validation
- Health status values (SERVING, UNKNOWN, NOT_SERVING)
- Empty service name health check
- Specific service name health check
- Error handling

### Performance Tests

Router observability performance is covered by performance tests in `test/router_observability_performance_SUITE.erl`:

```bash
# Run performance tests
rebar3 ct --suite test/router_observability_performance_SUITE

# Or use benchmark script
bash scripts/benchmark_observability.sh
```

**Test Coverage**: 5 tests
- Log generation throughput
- PII filtering latency
- JSON serialization performance
- Memory usage during logging
- Concurrent logging performance

### Code Coverage

Router observability tests include code coverage analysis:

```bash
# Run tests with coverage
rebar3 ct --cover --suite test/router_observability_SUITE

# Generate coverage report
rebar3 cover

# Or use coverage script
bash scripts/generate_coverage.sh
```

**Coverage Thresholds**:
- Line coverage: >80%
- Branch coverage: >70%
- Function coverage: >90%

**Coverage Report**: `_build/test/cover/html/index.html`

### Test Documentation

**See**: `test/README.md` for comprehensive test documentation including:
- Test structure and examples
- Debugging guide
- Coverage metrics
- Performance test interpretation
- CI/CD integration

## Troubleshooting

### Common Issues

#### Logs Not Appearing

**Problem**: Logs are not being written to files.

**Solutions**:
1. Check that `telemetry_enabled` is set to `true` in application configuration:
   ```erlang
   {telemetry_enabled, true}
   ```
2. Verify log directory exists and is writable:
   ```bash
   ls -la .windsurf/reports/
   chmod 755 .windsurf/reports/
   ```
3. Check application logs for errors:
   ```bash
   # Check Erlang/OTP logs
   tail -f /var/log/router/router.log
   ```

#### Health Endpoint Not Responding

**Problem**: gRPC health check fails or returns errors.

**Solutions**:
1. Verify Router is running:
   ```bash
   ps aux | grep beamline_router
   ```
2. Check gRPC port is correct (default: 9000):
   ```bash
   netstat -tlnp | grep 9000
   ```
3. Test health endpoint manually:
   ```bash
   grpc_health_probe -addr=localhost:9000
   ```
4. Check gRPC server configuration:
   ```erlang
   {grpc_enabled, true},
   {grpc_port, 9000}
   ```

#### PII Not Being Filtered

**Problem**: Sensitive data appears in logs.

**Solutions**:
1. Verify PII filtering is enabled (enabled by default)
2. Check that sensitive fields match filter patterns:
   - Fields: `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`
   - NATS headers: `bearer`, `x-api-key`, `x-auth-token`, `x-authorization` (case-insensitive)
3. Review log entries to ensure filtering is working:
   ```bash
   cat .windsurf/reports/router_2025-11-30.jsonl | jq 'select(.context.api_key != null)'
   ```
   Should return empty or show `"[REDACTED]"` values.

#### Invalid JSON in Logs

**Problem**: Log entries are not valid JSON.

**Solutions**:
1. Check for special characters in log messages that need escaping
2. Verify JSON serialization is working:
   ```bash
   cat .windsurf/reports/router_2025-11-30.jsonl | jq '.'
   ```
3. Review `router_logger.erl` implementation for JSON encoding issues

#### Missing CP1 Correlation Fields

**Problem**: CP1 fields (`tenant_id`, `run_id`, `flow_id`, `trace_id`) are not present in logs.

**Solutions**:
1. Verify context is being passed to logger:
   ```erlang
   router_logger:info(<<"Message">>, #{
       <<"tenant_id">> => <<"tenant_123">>,
       <<"trace_id">> => <<"trace_abc">>
   }).
   ```
2. Check that context is available in the calling code
3. CP1 fields are optional - they only appear when context is available

### Debug Mode

Enable DEBUG level logging for detailed troubleshooting:

```erlang
{log_level, debug}
```

View DEBUG logs:
```bash
cat .windsurf/reports/router_2025-11-30.jsonl | jq 'select(.level == "DEBUG")'
```

## Production Logging Guide

### Log Rotation

Router writes logs to files in the format `router_YYYY-MM-DD.jsonl`. In production, these files should be rotated to prevent disk space issues.

#### Using logrotate

Create `/etc/logrotate.d/beamline-router`:

```bash
.windsurf/reports/router_*.jsonl {
    daily
    rotate 7
    compress
    delaycompress
    notifempty
    create 0640 router router
    missingok
    postrotate
        # Reload Router if needed (optional)
        systemctl reload beamline-router || true
    endscript
}
```

**Configuration Options**:
- `daily`: Rotate logs daily
- `rotate 7`: Keep 7 days of logs
- `compress`: Compress rotated logs
- `delaycompress`: Compress on next rotation cycle
- `notifempty`: Don't rotate empty files
- `create 0640 router router`: Create new log files with permissions 0640, owned by router user
- `missingok`: Don't error if log files are missing

#### Using systemd

Configure systemd service with log rotation:

```ini
[Service]
StandardOutput=append:/var/log/router/router.log
StandardError=append:/var/log/router/router_error.log
ExecStart=/usr/bin/beamline_router
```

Use systemd's built-in log rotation:

```ini
[Service]
StandardOutput=journal
StandardError=journal
```

Then configure journald rotation in `/etc/systemd/journald.conf`:

```ini
[Journal]
SystemMaxUse=1G
SystemKeepFree=2G
MaxRetentionSec=7d
```

#### Using Docker

For Docker deployments, use volume mounts and external log rotation:

```yaml
services:
  router:
    image: beamline/router:latest
    volumes:
      - ./logs:/app/.windsurf/reports
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "7"
```

Or use Docker logging driver with rotation:

```bash
docker run \
  --log-driver json-file \
  --log-opt max-size=10m \
  --log-opt max-file=7 \
  beamline/router:latest
```

### Log Aggregation

#### Loki Integration

Router logs can be collected by Loki for centralized log aggregation:

1. **File-based collection** (using Promtail):

```yaml
# promtail-config.yml
server:
  http_listen_port: 9080
  grpc_listen_port: 0

positions:
  filename: /tmp/positions.yaml

clients:
  - url: http://loki:3100/loki/api/v1/push

scrape_configs:
  - job_name: router
    static_configs:
      - targets:
          - localhost
        labels:
          job: router
          component: router
          __path__: /var/log/router/router_*.jsonl
```

2. **Direct HTTP push** (if Router supports it in CP2):

Router could push logs directly to Loki via HTTP API (planned for CP2).

#### ELK Stack Integration

For ELK (Elasticsearch, Logstash, Kibana) integration:

1. **Filebeat configuration**:

```yaml
# filebeat.yml
filebeat.inputs:
  - type: log
    enabled: true
    paths:
      - /var/log/router/router_*.jsonl
    json.keys_under_root: true
    json.add_error_key: true

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
  index: "router-logs-%{+yyyy.MM.dd}"

processors:
  - add_fields:
      fields:
        component: router
```

2. **Logstash configuration** (alternative):

```ruby
# logstash.conf
input {
  file {
    path => "/var/log/router/router_*.jsonl"
    codec => json_lines
  }
}

filter {
  json {
    source => "message"
  }
  
  date {
    match => ["timestamp", "ISO8601"]
  }
}

output {
  elasticsearch {
    hosts => ["elasticsearch:9200"]
    index => "router-logs-%{+YYYY.MM.dd}"
  }
}
```

### Production Best Practices

1. **Log Retention**: Keep logs for 7-30 days depending on compliance requirements
2. **Disk Space**: Monitor disk usage and set up alerts
3. **Log Compression**: Enable compression for rotated logs to save space
4. **Centralized Logging**: Use log aggregation (Loki, ELK) for production
5. **Log Level**: Use `INFO` level in production, `DEBUG` only for troubleshooting
6. **PII Filtering**: Always enabled - verify it's working in production
7. **Health Monitoring**: Set up health check monitoring (Prometheus, Alertmanager in CP2)

### Performance Considerations

- **Log I/O**: Logging is asynchronous to minimize performance impact
- **JSON Serialization**: JSON encoding is optimized for performance
- **PII Filtering**: Recursive filtering may add small overhead for large context objects
- **File Rotation**: Use logrotate or systemd for efficient log rotation

## Best Practices

### When to Use Each Log Level

**ERROR**: Use for critical errors that require immediate attention:
- NATS connection failures
- Routing decision failures
- Policy loading errors
- Internal server errors
- Authentication/authorization failures
- Invalid request format

**WARN**: Use for warnings and potential issues:
- Rate limit approaching
- Policy not found (fallback to default)
- Provider unavailable (fallback to another)
- Configuration issues
- Performance degradation
- Deprecated API usage

**INFO**: Use for informational messages:
- Request processed successfully
- Routing decision made
- Policy loaded from cache
- Health check passed
- State changes
- Normal operation events

**DEBUG**: Use for detailed debugging information:
- Request parsing details
- Internal state transitions
- Detailed context information
- Provider selection logic
- Policy evaluation details
- Development and troubleshooting

### How to Structure Context Objects

**Best Practices**:
- Keep context objects small and focused (avoid very large nested structures)
- Use consistent field names across logs
- Include relevant identifiers (subject, policy_id, provider)
- Avoid duplicating information already in top-level fields (tenant_id, trace_id, etc.)
- Use meaningful field names that describe the context

**Example**:
```erlang
router_logger:info(<<"Routing decision made">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"run_id">> => <<"run_789">>,
    <<"flow_id">> => <<"flow_456">>,
    <<"subject">> => <<"beamline.router.v1.decide">>,
    <<"policy_id">> => <<"policy_456">>,
    <<"provider">> => <<"openai">>,
    <<"decision_reason">> => <<"weighted_selection">>
}).
```

**Avoid**:
- Very large context objects (>1000 fields)
- Duplicating CP1 fields in context (they're already at top level)
- Including sensitive data (will be filtered, but better to avoid)

### PII Filtering Best Practices

**Always filter sensitive data**:
- Never log passwords, API keys, or tokens directly
- Use PII filtering for all user input and context data
- Verify filtered fields match patterns (case-insensitive)
- Test PII filtering in development before production

**Filtered patterns** (case-insensitive):
- `password`, `api_key`, `secret`, `token`
- `access_token`, `refresh_token`, `authorization`
- `credit_card`, `ssn`, `email`, `phone`
- NATS headers: `bearer`, `x-api-key`, `x-auth-token`, `x-authorization`

**Automatic filtering**:
- All context maps are automatically filtered recursively
- PII fields are replaced with `"[REDACTED]"`
- Pattern detection in values (e.g., `"API key sk-123..."`)

**Manual filtering** (if needed):
```erlang
%% Filter PII before logging (if needed)
FilteredContext = router_logger:filter_pii(Context),
router_logger:info(<<"Message">>, FilteredContext).
```

### Performance Considerations

**Logging Performance**:
- Logging is asynchronous to minimize performance impact
- JSON serialization is optimized for performance
- PII filtering adds minimal overhead (~10-50 microseconds per log entry)
- File I/O is buffered for better performance

**Best Practices**:
- Use appropriate log levels (avoid excessive DEBUG in production)
- Keep context objects small (large context objects increase serialization time)
- Batch related logs when possible
- Monitor logging performance in production

**Production Recommendations**:
- Use `INFO` level in production (DEBUG only for troubleshooting)
- Limit context object size (<100 fields recommended)
- Monitor log file size and disk usage
- Use log rotation (see Production Logging Guide)
- Enable log aggregation (Loki, ELK) for centralized collection

### gRPC Health Check Best Practices

**Health Check Configuration**:
- Health endpoint is available via gRPC health service
- Service: `grpc.health.v1.Health/Check`
- Port: 9000 (default, configurable)
- Authentication: Required (API key in gRPC metadata)

**Best Practices**:
- Monitor health endpoint availability
- Set up health check monitoring (Prometheus, Alertmanager in CP2)
- Use health checks for load balancer integration
- Verify health status before routing requests

**Health Check Tools**:
- `grpc_health_probe` - Preferred tool for health checks
- `grpcurl` - Alternative tool for manual testing
- Admin API: `GetValidatorsHealth` - Additional health information

## Migration Guide

### Upgrading from Older Versions

**From pre-CP1 versions**:

1. **Log Format Changes**:
   - **Old**: Plain text or custom format
   - **New**: Structured JSON with CP1 fields at top level
   - **Action**: Update log parsing scripts to handle JSON format

2. **Timestamp Format**:
   - **Old**: Various formats (Unix timestamp, custom format)
   - **New**: ISO 8601 with microsecond precision (6 digits: `YYYY-MM-DDTHH:MM:SS.ssssssZ`)
   - **Action**: Update timestamp parsing logic

3. **CP1 Fields**:
   - **Old**: Fields may be in nested objects or missing
   - **New**: CP1 fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`) at top level
   - **Action**: Update log querying logic to use top-level fields

4. **PII Filtering**:
   - **Old**: Manual filtering or no filtering
   - **New**: Automatic recursive JSON filtering
   - **Action**: Verify filtered patterns match requirements, test filtering in development

### Breaking Changes

**CP1 Compliance (v1.0.0)**:
- ✅ Log format changed to structured JSON (JSONL format)
- ✅ CP1 fields moved to top level (not in correlation object)
- ✅ Timestamp format standardized to ISO 8601 with microseconds
- ✅ PII filtering enabled by default (automatic recursive filtering)

**Compatibility Notes**:
- Old log parsers may need updates to handle JSON format
- Log aggregation systems should support JSON parsing (JSONL format)
- Health endpoint format changed to gRPC health service (not HTTP)
- CP1 fields are now at top level (not in nested correlation object)

### Step-by-Step Migration

**Step 1: Update Log Parsing Scripts**

**Before** (plain text parsing):
```bash
# Old: Plain text log parsing
grep "ERROR" router.log
```

**After** (JSON parsing):
```bash
# New: JSON log parsing
cat router_2025-11-30.jsonl | jq 'select(.level == "ERROR")'
```

**Step 2: Update Timestamp Parsing**

**Before** (Unix timestamp):
```erlang
%% Old: Unix timestamp
Timestamp = erlang:system_time(second).
```

**After** (ISO 8601):
```erlang
%% New: ISO 8601 timestamp (automatic in router_logger)
Timestamp = router_logger:get_timestamp().  %% Returns ISO 8601 format
```

**Step 3: Update CP1 Field Access**

**Before** (nested correlation object):
```erlang
%% Old: CP1 fields in correlation object (deprecated)
LogEntry = #{
    <<"correlation">> => #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"trace_id">> => <<"trace_abc">>
    }
}.
```

**After** (top-level CP1 fields):
```erlang
%% New: CP1 fields at top level
router_logger:info(<<"Message">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc">>
}).

%% CP1 fields are automatically placed at top level
%% Log entry:
%% {
%%   "tenant_id": "tenant_123",
%%   "trace_id": "trace_abc",
%%   ...
%% }
```

**Step 4: Verify PII Filtering**

**Before** (manual filtering):
```erlang
%% Old: Manual PII filtering
FilteredContext = manually_filter_pii(Context),
log(FilteredContext).
```

**After** (automatic filtering):
```erlang
%% New: Automatic PII filtering
router_logger:info(<<"Message">>, #{
    <<"api_key">> => <<"sk-123456">>,  %% Automatically filtered
    <<"password">> => <<"secret">>     %% Automatically filtered
}).

%% Log entry:
%% {
%%   "context": {
%%     "api_key": "[REDACTED]",
%%     "password": "[REDACTED]"
%%   }
%% }
```

### Common Pitfalls and Solutions

**Pitfall 1**: Log parsing scripts fail with JSON format.

**Solution**: Update scripts to use JSON parser (jq, Python json, etc.):
```bash
# Use jq for JSON parsing
cat router_2025-11-30.jsonl | jq 'select(.level == "ERROR")'
```

**Pitfall 2**: CP1 fields not found (looking in wrong location).

**Solution**: CP1 fields are now at top level, not in correlation object:
```bash
# Correct: Top-level field access
cat router_2025-11-30.jsonl | jq '.tenant_id'

# Incorrect: Nested correlation object (deprecated)
cat router_2025-11-30.jsonl | jq '.correlation.tenant_id'  # Will return null
```

**Pitfall 3**: Timestamp format mismatch.

**Solution**: Use ISO 8601 parser:
```bash
# Parse ISO 8601 timestamp
cat router_2025-11-30.jsonl | jq -r '.timestamp' | date -f -
```

**Pitfall 4**: PII filtering not working as expected.

**Solution**: Verify field names match filtered patterns (case-insensitive):
```erlang
%% These will be filtered (case-insensitive):
<<"api_key">> => <<"sk-123">>,      %% Filtered
<<"API_KEY">> => <<"sk-123">>,      %% Filtered
<<"password">> => <<"secret">>,     %% Filtered
<<"PASSWORD">> => <<"secret">>,     %% Filtered
<<"x-api-key">> => <<"key">>,       %% Filtered (NATS header)
<<"X-Api-Key">> => <<"key">>,       %% Filtered (NATS header)
```

### Testing Migration

**Before deploying to production**:

1. **Test log format**:
   ```bash
   # Verify JSON format
   cat router_2025-11-30.jsonl | jq '.'
   ```

2. **Test CP1 fields**:
   ```bash
   # Verify CP1 fields at top level
   cat router_2025-11-30.jsonl | jq 'select(.tenant_id != null)'
   ```

3. **Test PII filtering**:
   ```bash
   # Verify PII is filtered
   cat router_2025-11-30.jsonl | jq 'select(.context.api_key == "[REDACTED]")'
   ```

4. **Test health endpoint**:
   ```bash
   # Verify gRPC health endpoint
   grpc_health_probe -addr=localhost:9000
   ```

## API Reference

### Logging Functions

#### `router_logger:error/2`

Log an error message with context.

**Signature**:
```erlang
-spec error(Message :: binary() | string(), Context :: map()) -> ok.
```

**Parameters**:
- `Message`: Error message (binary or string, automatically converted to binary)
- `Context`: Context map containing:
  - CP1 fields: `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` (optional)
  - Error information: `error_code` (optional), `latency_ms` (optional)
  - Additional context: Any key-value pairs (automatically filtered for PII)

**Return Value**: `ok`

**Example**:
```erlang
router_logger:error(<<"NATS connection failed">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"error_code">> => <<"NATS_CONNECTION_FAILED">>,
    <<"latency_ms">> => 5000,
    <<"subject">> => <<"beamline.router.v1.decide">>
}).
```

**Log Entry**:
```json
{
  "timestamp": "2025-11-30T12:00:00.123456Z",
  "level": "ERROR",
  "component": "router",
  "message": "NATS connection failed",
  "tenant_id": "tenant_123",
  "trace_id": "trace_abc123",
  "error_code": "NATS_CONNECTION_FAILED",
  "latency_ms": 5000,
  "context": {
    "subject": "beamline.router.v1.decide"
  }
}
```

#### `router_logger:warn/2`

Log a warning message with context.

**Signature**:
```erlang
-spec warn(Message :: binary() | string(), Context :: map()) -> ok.
```

**Parameters**:
- `Message`: Warning message (binary or string)
- `Context`: Context map (same as `error/2`)

**Return Value**: `ok`

**Example**:
```erlang
router_logger:warn(<<"Rate limit approaching">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"rate_limit">> => 90,
    <<"threshold">> => 80
}).
```

#### `router_logger:info/2`

Log an informational message with context.

**Signature**:
```erlang
-spec info(Message :: binary() | string(), Context :: map()) -> ok.
```

**Parameters**:
- `Message`: Informational message (binary or string)
- `Context`: Context map (same as `error/2`)

**Return Value**: `ok`

**Example**:
```erlang
router_logger:info(<<"Routing decision made">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"run_id">> => <<"run_789">>,
    <<"flow_id">> => <<"flow_456">>,
    <<"policy_id">> => <<"policy_456">>,
    <<"provider">> => <<"openai">>,
    <<"latency_ms">> => 45
}).
```

#### `router_logger:debug/2`

Log a debug message with context.

**Signature**:
```erlang
-spec debug(Message :: binary() | string(), Context :: map()) -> ok.
```

**Parameters**:
- `Message`: Debug message (binary or string)
- `Context`: Context map (same as `error/2`)

**Return Value**: `ok`

**Example**:
```erlang
router_logger:debug(<<"Policy evaluation details">>, #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"trace_id">> => <<"trace_abc123">>,
    <<"policy_id">> => <<"policy_456">>,
    <<"evaluation_steps">> => 5,
    <<"cache_hit">> => true
}).
```

### Utility Functions

#### `router_logger:is_enabled/0`

Check if logging is enabled.

**Signature**:
```erlang
-spec is_enabled() -> boolean().
```

**Parameters**: None

**Return Value**: `true` if logging is enabled, `false` otherwise

**Example**:
```erlang
case router_logger:is_enabled() of
    true ->
        router_logger:info(<<"Message">>, #{});
    false ->
        ok
end.
```

**Configuration**: Set via `beamline_router.telemetry_enabled` application environment variable (default: `true`)

#### `router_logger:filter_pii/1`

Filter PII from a context map (exported for testing).

**Signature**:
```erlang
-spec filter_pii(Context :: map()) -> map().
```

**Parameters**:
- `Context`: Context map (may contain PII)

**Return Value**: New map with PII fields replaced by `"[REDACTED]"` (recursive filtering)

**Example**:
```erlang
Context = #{
    <<"api_key">> => <<"sk-123456">>,
    <<"password">> => <<"secret">>,
    <<"nested">> => #{
        <<"token">> => <<"bearer_token">>
    }
},
FilteredContext = router_logger:filter_pii(Context),
%% Result:
%% #{
%%     <<"api_key">> => <<"[REDACTED]">>,
%%     <<"password">> => <<"[REDACTED]">>,
%%     <<"nested">> => #{
%%         <<"token">> => <<"[REDACTED]">>
%%     }
%% }
```

**Note**: This function is automatically called by logging functions. Manual use is only needed for testing or pre-filtering.

### Internal Functions

The following functions are internal and not exported, but documented for reference:

#### `build_log_entry/3`

Build structured log entry from level, message, and context.

**Signature**:
```erlang
-spec build_log_entry(Level :: atom(), Message :: binary() | string(), Context :: map()) -> map().
```

**Parameters**:
- `Level`: Log level atom (`error`, `warn`, `info`, `debug`)
- `Message`: Log message (binary or string)
- `Context`: Context map

**Return Value**: Map representing log entry (ready for JSON encoding)

#### `get_timestamp/0`

Get ISO-8601 timestamp with microsecond precision.

**Signature**:
```erlang
-spec get_timestamp() -> binary().
```

**Return Value**: ISO-8601 timestamp binary (format: `YYYY-MM-DDTHH:MM:SS.ssssssZ`)

#### `write_jsonl/1`

Write JSONL entry to file.

**Signature**:
```erlang
-spec write_jsonl(Json :: binary()) -> ok.
```

**Parameters**:
- `Json`: JSON-encoded log entry (binary)

**Return Value**: `ok`

**File Location**: `.windsurf/reports/router_YYYY-MM-DD.jsonl` (configurable via `beamline_router.log_dir`)

### Error Handling

**Logging Functions**:
- All logging functions return `ok` (never raise exceptions)
- If file write fails, logs are written to stderr as fallback
- If logging is disabled (`telemetry_enabled = false`), functions return `ok` without logging

**PII Filtering**:
- PII filtering never fails (returns empty map `#{}` if input is invalid)
- Recursive filtering handles nested maps of any depth
- Pattern detection in values uses regex (may fail silently if regex compilation fails)

**JSON Encoding**:
- JSON encoding uses `jsx:encode/1` (may raise exception for invalid data)
- Invalid data is converted to string representation via `ensure_binary/1`
- Logging functions catch encoding errors and write to stderr

## Risk Test Metrics Pattern (X_rN_metrics)

**CRITICAL**: New risk tests must go through their own `*_metrics` module.

### Pattern Overview

For each risk theme (R10, R11, R12, etc.), create a dedicated metrics access layer module following the `router_r10_metrics` pattern. This ensures:

- **No direct ETS access** in tests and code
- **Single source of truth** for metric constants and helpers
- **Consistent metric access patterns** across all risk themes
- **Easier maintenance** and refactoring

### Implementation Requirements

**For next risk theme (R11/R12 or other module)**:

1. **Create `<module>_metrics.erl`** following `router_r10_metrics` pattern:
   - Metric reading functions (`get_metric_value/2`, etc.)
   - Metric constants (trigger reasons, state values, etc.)
   - Debugging helpers (`dump_metrics/0`, `clear_metrics/0`)
   - Assertion helpers (`wait_for_*`, `assert_*`)

2. **Enforce no direct ETS access**:
   - All tests must use `<module>_metrics` functions
   - Production code should use `<module>_metrics` where appropriate
   - No `ets:lookup`, `ets:info`, `ets:delete_all_objects` in tests

3. **Reference Implementation**:
   - `src/router_r10_metrics.erl` - R10 circuit breaker metrics
   - `src/router_idem_metrics.erl` - Idempotency metrics (example)

### Reset/Lifecycle Pattern

**For gen_servers with ETS that participate in tests**:

1. **Implement reset/lifecycle pattern**:
   - `init/1` → `do_init/1` (separation for safe reset)
   - `handle_call(reset_all, ...)` for safe reset
   - `reset/0` public API function

2. **Lifecycle helpers in `*_test_utils`**:
   - `reset_<module>/0` - Reset state via API
   - `ensure_<module>_alive/0` - Verify process is running
   - `ensure_<module>_table/0` - Verify ETS table exists

3. **Purpose**: Reduce chance of repeating old "ETS+CT+sup" problems

4. **Reference Implementation**:
   - `src/router_circuit_breaker.erl` - Circuit breaker with reset pattern
   - `src/router_rbac.erl` - RBAC with reset pattern
   - `src/router_idem.erl` - Idempotency with reset pattern

### Examples

**Creating a new metrics module**:

```erlang
-module(router_r11_metrics).

-export([
    get_metric_value/2,
    clear_metrics/0,
    dump_metrics/0
]).

%% Follow router_r10_metrics.erl pattern
```

**Using metrics in tests**:

```erlang
%% ✅ CORRECT: Use metrics access layer
Value = router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => TenantId,
    provider_id => ProviderId
}).

%% ❌ WRONG: Direct ETS access
Value = case ets:lookup(router_metrics, router_circuit_breaker_state) of
    [{router_circuit_breaker_state, V}] -> V;
    [] -> 0
end.
```

**Using reset pattern in tests**:

```erlang
%% ✅ CORRECT: Use reset API
ok = router_rbac:reset().
ok = router_idem:reset().

%% ❌ WRONG: Direct ETS manipulation
ets:delete_all_objects(rbac_roles).
ets:delete_all_objects(router_idem).
```

## References

- `config/observability/logging.json` - Logging format schema
- `src/router_logger.erl` - Logger implementation
- `src/router_r10_metrics.erl` - R10 metrics access layer (reference implementation)
- `src/router_idem_metrics.erl` - Idempotency metrics access layer (example)
- `src/router_circuit_breaker.erl` - Reset/lifecycle pattern example
- `src/router_rbac.erl` - Reset/lifecycle pattern example
- `test/router_test_utils.erl` - Lifecycle helpers
- `test/router_observability_SUITE.erl` - Observability unit tests
- `test/router_health_integration_SUITE.erl` - Health endpoint integration tests
- `scripts/observability/test_router_observability.sh` - E2E test script
- `docs/dev/ROUTER_OBSERVABILITY_TEST.md` - Test script documentation
- `docs/dev/METRICS_MODULE_TEMPLATE.md` - Metrics module template
- `docs/OPERATIONAL_GUIDE.md` - Operational procedures
- `docs/PRODUCTION_LOGGING.md` - Production logging guide (log rotation, aggregation)
