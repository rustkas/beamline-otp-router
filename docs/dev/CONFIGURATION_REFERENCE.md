# Configuration Reference

**Version**: 1.0  
**Last Updated**: 2025-01-27

## Overview

Router configuration is managed via Erlang application environment variables.

## Configuration Categories

### Core Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `grpc_port` | integer | 0 | gRPC server port (0 = disabled) |
| `grpc_enabled` | boolean | false | Enable gRPC server |
| `nats_mode` | atom | mock | NATS mode: `mock` or `real` |
| `nats_url` | string | undefined | NATS server URL |
| `nats_cluster` | string | <<"default">> | NATS cluster name |

### CP2+ Features

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `cp2_plus_allowed` | boolean | false | Enable CP2+ features |
| `ack_enabled` | boolean | false | Enable ACK consumer (requires cp2_plus_allowed) |
| `idempotency_enabled` | boolean | false | Enable idempotency layer (requires cp2_plus_allowed) |

### Observability

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `telemetry_enabled` | boolean | true | Enable telemetry events |
| `metrics_export_enabled` | boolean | false | Enable metrics HTTP endpoint |
| `tracing_enabled` | boolean | false | Enable OpenTelemetry tracing |

### Rate Limiting

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `rate_limit_enabled` | boolean | true | Enable rate limiting |
| `rate_limit_per_tenant` | integer | 1000 | Requests per second per tenant |
| `rate_limit_per_user` | integer | 100 | Requests per second per user |

### Circuit Breaker

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `circuit_breaker_enabled` | boolean | true | Enable circuit breaker |
| `circuit_breaker_failure_threshold` | integer | 5 | Failure threshold |
| `circuit_breaker_error_rate_threshold` | float | 0.5 | Error rate threshold (0.0-1.0) |
| `circuit_breaker_latency_threshold_ms` | integer | 5000 | Latency threshold in milliseconds |

### Backpressure

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `queue_overload` | integer | 1000 | Queue overload threshold |
| `latency_overload_ms` | integer | 5000 | Latency overload threshold (ms) |
| `inflight_overload` | integer | 500 | In-flight message overload threshold |

### NATS Connection

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `nats_reconnect_attempts` | integer | 10 | Maximum reconnect attempts |
| `nats_reconnect_delay_ms` | integer | 1000 | Base reconnect delay (ms) |
| `nats_max_reconnect_delay_ms` | integer | 30000 | Maximum reconnect delay (ms) |
| `nats_fail_open_mode` | boolean | false | Fail-open mode when NATS unavailable |
| `nats_max_pending_operations` | integer | 1000 | Maximum pending operations queue size |

### Publish Retry

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `publish_retry_enabled` | boolean | false | Enable publish retry |
| `publish_retry_max_attempts` | integer | 3 | Maximum retry attempts |
| `publish_retry_backoff_strategy` | atom | exponential | Backoff strategy: `exponential` or `linear` |
| `publish_retry_backoff_base_ms` | integer | 100 | Base backoff delay (ms) |
| `publish_retry_backoff_max_ms` | integer | 5000 | Maximum backoff delay (ms) |
| `publish_retry_jitter_percent` | integer | 20 | Jitter percentage (0-100) |
| `publish_retry_timeout_per_attempt_ms` | integer | 2000 | Timeout per attempt (ms) |
| `publish_retry_total_deadline_ms` | integer | 10000 | Total deadline (ms) |

## Configuration Examples

### Minimal Configuration (CP1)

```erlang
[
  {beamline_router, [
    {grpc_enabled, false},
    {nats_mode, mock},
    {telemetry_enabled, true},
    {metrics_export_enabled, false}
  ]}
].
```

### Production Configuration

```erlang
[
  {beamline_router, [
    {grpc_enabled, true},
    {grpc_port, 9090},
    {nats_mode, real},
    {nats_url, "nats://localhost:4222"},
    {nats_cluster, <<"production">>},
    {cp2_plus_allowed, true},
    {ack_enabled, true},
    {idempotency_enabled, true},
    {telemetry_enabled, true},
    {metrics_export_enabled, true},
    {tracing_enabled, true},
    {rate_limit_enabled, true},
    {rate_limit_per_tenant, 10000},
    {circuit_breaker_enabled, true},
    {publish_retry_enabled, true},
    {publish_retry_max_attempts, 5}
  ]}
].
```

### Test Configuration

```erlang
[
  {beamline_router, [
    {grpc_port, 0},
    {grpc_enabled, false},
    {nats_mode, mock},
    {test_mode, true},
    {telemetry_enabled, true},
    {metrics_export_enabled, false},
    {tracing_enabled, false},
    {disable_heir, true}
  ]}
].
```

## Configuration Validation

Configuration is validated at application startup. Invalid configuration will cause startup to fail.

### Validation Rules

1. **cp2_plus_allowed**: Must be `true` for `ack_enabled` or `idempotency_enabled`
2. **metrics_export_enabled**: Requires `telemetry_enabled` to be `true`
3. **grpc_port**: Must be valid port number (0-65535)
4. **rate_limit_per_tenant**: Must be positive integer
5. **circuit_breaker_*_threshold**: Must be positive number

## Environment Variables

Configuration can be set via:
1. Application environment (sys.config, config/*.config)
2. Environment variables (converted to application env)
3. Runtime configuration (application:set_env/3)

## See Also

- `src/beamline_router_sup.erl` - Configuration usage in supervisor
- `config/` - Configuration files directory

