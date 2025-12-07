# Complete Configuration Reference

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: Complete

## Overview

This document provides complete configuration reference for Router component, including all configuration options, examples, and validation rules.

## Configuration Methods

### Application Environment

Configuration is set via `application:set_env/3` or in `sys.config` / `rebar.config`:

```erlang
{beamline_router, [
    %% Configuration options here
]}
```

### Runtime Configuration

Configuration can be changed at runtime:

```erlang
application:set_env(beamline_router, key, value),
application:stop(beamline_router),
application:start(beamline_router).
```

## Configuration Options

### gRPC Configuration

#### `grpc_port`

**Type**: `integer()`  
**Default**: `9000`  
**Description**: Port for gRPC server (Router.Decide and RouterAdmin)

**Example**:
```erlang
{beamline_router, [
    {grpc_port, 9000}
]}
```

#### `grpc_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable gRPC server

**Example**:
```erlang
{beamline_router, [
    {grpc_enabled, true}
]}
```

### Metrics Configuration

#### `metrics_port`

**Type**: `integer()`  
**Default**: `9001`  
**Description**: Port for Prometheus metrics HTTP server

**Example**:
```erlang
{beamline_router, [
    {metrics_port, 9001}
]}
```

#### `telemetry_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable telemetry emission

**Example**:
```erlang
{beamline_router, [
    {telemetry_enabled, true}
]}
```

### NATS Configuration

#### `nats_mode`

**Type**: `atom()`  
**Default**: `mock`  
**Values**: `mock` | `real`  
**Description**: NATS connection mode (mock for testing, real for production)

**Example**:
```erlang
{beamline_router, [
    {nats_mode, mock}
]}
```

#### `nats_url`

**Type**: `string()` | `binary()`  
**Default**: `undefined`  
**Description**: NATS server URL (required when `nats_mode = real`)

**Example**:
```erlang
{beamline_router, [
    {nats_url, "nats://localhost:4222"}
]}
```

#### `nats_max_payload_size`

**Type**: `integer()`  
**Default**: `1048576` (1MB)  
**Description**: Maximum NATS message payload size in bytes

**Example**:
```erlang
{beamline_router, [
    {nats_max_payload_size, 1048576}
]}
```

#### `nats_tls_enabled`

**Type**: `boolean()`  
**Default**: `false`  
**Description**: Enable TLS for NATS connections (REQUIRED in production)

**Example**:
```erlang
{beamline_router, [
    {nats_tls_enabled, true},
    {nats_tls_cert_file, "/path/to/cert.pem"},
    {nats_tls_key_file, "/path/to/key.pem"},
    {nats_tls_ca_file, "/path/to/ca.pem"}
]}
```

#### `nats_connect_timeout_ms`

**Type**: `integer()`  
**Default**: `5000`  
**Description**: NATS connection timeout in milliseconds

**Example**:
```erlang
{beamline_router, [
    {nats_connect_timeout_ms, 5000}
]}
```

#### `nats_reconnect_attempts`

**Type**: `integer()`  
**Default**: `10`  
**Description**: Maximum NATS reconnection attempts

**Example**:
```erlang
{beamline_router, [
    {nats_reconnect_attempts, 10}
]}
```

#### `nats_reconnect_delay_ms`

**Type**: `integer()`  
**Default**: `1000`  
**Description**: Base NATS reconnect delay in milliseconds (exponential backoff)

**Example**:
```erlang
{beamline_router, [
    {nats_reconnect_delay_ms, 1000}
]}
```

#### `nats_max_reconnect_delay_ms`

**Type**: `integer()`  
**Default**: `30000`  
**Description**: Maximum NATS reconnect delay in milliseconds

**Example**:
```erlang
{beamline_router, [
    {nats_max_reconnect_delay_ms, 30000}
]}
```

#### `nats_fail_open_mode`

**Type**: `boolean()`  
**Default**: `false`  
**Description**: Enable fail-open mode (allow requests when NATS unavailable)

**Example**:
```erlang
{beamline_router, [
    {nats_fail_open_mode, false}
]}
```

#### `nats_max_pending_operations`

**Type**: `integer()`  
**Default**: `5000`  
**Description**: Maximum pending operations queue size

**Example**:
```erlang
{beamline_router, [
    {nats_max_pending_operations, 5000}
]}
```

### CAF Integration Configuration

#### `caf_push_assignment_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Global enable/disable flag for CAF assignment publishing (kill switch)

**Example**:
```erlang
{beamline_router, [
    {caf_push_assignment_enabled, true}
]}
```

#### `caf_assignment_subject`

**Type**: `binary()` | `string()`  
**Default**: `<<"caf.exec.assign.v1">>`  
**Description**: Default NATS subject for ExecAssignment publication

**Example**:
```erlang
{beamline_router, [
    {caf_assignment_subject, <<"caf.exec.assign.v1">>}
]}
```

#### `caf_push_assignment_allowed_tenants`

**Type**: `list(binary())` | `map()` | `undefined`  
**Default**: `undefined`  
**Description**: Tenant allowlist for CAF assignment publishing

**Example**:
```erlang
{beamline_router, [
    {caf_push_assignment_allowed_tenants, [<<"tenant1">>, <<"tenant2">>]}
]}
```

#### `caf_max_retries`

**Type**: `integer()`  
**Default**: `3`  
**Description**: Maximum retry attempts for CAF assignment publishing

**Example**:
```erlang
{beamline_router, [
    {caf_max_retries, 3}
]}
```

#### `caf_retry_base_ms`

**Type**: `integer()`  
**Default**: `100`  
**Description**: Base retry delay in milliseconds (exponential backoff)

**Example**:
```erlang
{beamline_router, [
    {caf_retry_base_ms, 100}
]}
```

#### `caf_deadline_multiplier`

**Type**: `float()`  
**Default**: `5.0`  
**Description**: Deadline multiplier for CAF assignment publishing

**Example**:
```erlang
{beamline_router, [
    {caf_deadline_multiplier, 5.0}
]}
```

#### `caf_deadline_min_ms`

**Type**: `integer()`  
**Default**: `5000`  
**Description**: Minimum deadline in milliseconds

**Example**:
```erlang
{beamline_router, [
    {caf_deadline_min_ms, 5000}
]}
```

#### `caf_deadline_max_ms`

**Type**: `integer()`  
**Default**: `60000`  
**Description**: Maximum deadline in milliseconds

**Example**:
```erlang
{beamline_router, [
    {caf_deadline_max_ms, 60000}
]}
```

### CP2 Features Configuration

#### `idempotency_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable idempotency layer (CP2)

**Example**:
```erlang
{beamline_router, [
    {idempotency_enabled, true}
]}
```

#### `idempotency_ttl_seconds`

**Type**: `integer()`  
**Default**: `3600`  
**Description**: Idempotency entry TTL in seconds (CP2)

**Example**:
```erlang
{beamline_router, [
    {idempotency_ttl_seconds, 3600}
]}
```

#### `tracing_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable OpenTelemetry distributed tracing (CP2)

**Example**:
```erlang
{beamline_router, [
    {tracing_enabled, true}
]}
```

#### `tenant_validation_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable tenant validation and ACL (CP2)

**Example**:
```erlang
{beamline_router, [
    {tenant_validation_enabled, true}
]}
```

#### `admin_grpc_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable RouterAdmin gRPC service (CP2)

**Example**:
```erlang
{beamline_router, [
    {admin_grpc_enabled, true}
]}
```

#### `admin_api_key`

**Type**: `binary()` | `string()`  
**Default**: `undefined`  
**Description**: API key for RouterAdmin service authentication (CP2)

**Example**:
```erlang
{beamline_router, [
    {admin_api_key, <<"your-secret-api-key">>}
]}
```

**Security**: Store in environment variables or secure configuration, never commit to repository.

### JetStream Configuration (CP2)

#### `jetstream_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable JetStream support (CP2)

**Example**:
```erlang
{beamline_router, [
    {jetstream_enabled, true}
]}
```

#### `jetstream_stream_name`

**Type**: `binary()` | `string()`  
**Default**: `<<"beamline_router">>`  
**Description**: JetStream stream name

**Example**:
```erlang
{beamline_router, [
    {jetstream_stream_name, <<"beamline_router">>}
]}
```

#### `jetstream_consumer_name`

**Type**: `binary()` | `string()`  
**Default**: `<<"router_consumer">>`  
**Description**: JetStream consumer name

**Example**:
```erlang
{beamline_router, [
    {jetstream_consumer_name, <<"router_consumer">>}
]}
```

### Circuit Breaker Configuration

#### `circuit_breaker_default_failure_threshold`

**Type**: `integer()`  
**Default**: `5`  
**Description**: Default failure threshold for circuit breaker

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_failure_threshold, 5}
]}
```

#### `circuit_breaker_default_success_threshold`

**Type**: `integer()`  
**Default**: `2`  
**Description**: Default success threshold for circuit breaker

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_success_threshold, 2}
]}
```

#### `circuit_breaker_default_timeout_ms`

**Type**: `integer()`  
**Default**: `60000`  
**Description**: Default timeout in milliseconds before half-open transition

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_timeout_ms, 60000}
]}
```

#### `circuit_breaker_default_half_open_max_calls`

**Type**: `integer()`  
**Default**: `3`  
**Description**: Default maximum calls in half-open state

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_half_open_max_calls, 3}
]}
```

#### `circuit_breaker_default_error_rate_threshold`

**Type**: `float()`  
**Default**: `0.5`  
**Description**: Default error rate threshold (0.0-1.0)

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_error_rate_threshold, 0.5}
]}
```

#### `circuit_breaker_default_error_rate_window_seconds`

**Type**: `integer()`  
**Default**: `60`  
**Description**: Default error rate window size in seconds

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_error_rate_window_seconds, 60}
]}
```

#### `circuit_breaker_default_latency_threshold_ms`

**Type**: `integer()`  
**Default**: `5000`  
**Description**: Default latency threshold in milliseconds (R10)

**Example**:
```erlang
{beamline_router, [
    {circuit_breaker_default_latency_threshold_ms, 5000}
]}
```

### Rate Limiting Configuration

#### `rate_limiter_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable rate limiting

**Example**:
```erlang
{beamline_router, [
    {rate_limiter_enabled, true}
]}
```

#### `rate_limiter_default_limit_per_minute`

**Type**: `integer()`  
**Default**: `1000`  
**Description**: Default rate limit per minute

**Example**:
```erlang
{beamline_router, [
    {rate_limiter_default_limit_per_minute, 1000}
]}
```

#### `rate_limiter_window_seconds`

**Type**: `integer()`  
**Default**: `60`  
**Description**: Rate limit window size in seconds

**Example**:
```erlang
{beamline_router, [
    {rate_limiter_window_seconds, 60}
]}
```

## Configuration Examples

### Development Configuration

```erlang
{beamline_router, [
    {grpc_port, 9000},
    {grpc_enabled, true},
    {metrics_port, 9001},
    {telemetry_enabled, true},
    {nats_mode, mock},
    {tracing_enabled, false},
    {admin_grpc_enabled, true},
    {admin_api_key, <<"dev-key">>}
]}
```

### Production Configuration

```erlang
{beamline_router, [
    {grpc_port, 9000},
    {grpc_enabled, true},
    {metrics_port, 9001},
    {telemetry_enabled, true},
    {nats_mode, real},
    {nats_url, "nats://nats-server:4222"},
    {nats_tls_enabled, true},
    {nats_tls_cert_file, "/etc/ssl/certs/router.pem"},
    {nats_tls_key_file, "/etc/ssl/private/router.key"},
    {nats_tls_ca_file, "/etc/ssl/certs/ca.pem"},
    {nats_reconnect_attempts, 10},
    {nats_reconnect_delay_ms, 1000},
    {nats_max_reconnect_delay_ms, 30000},
    {nats_fail_open_mode, false},
    {nats_max_pending_operations, 5000},
    {tracing_enabled, true},
    {tenant_validation_enabled, true},
    {admin_grpc_enabled, true},
    {admin_api_key, undefined}  %% Set via environment variable
]}
```

### Test Configuration

```erlang
{beamline_router, [
    {grpc_port, 0},  %% Random port
    {grpc_enabled, false},
    {metrics_port, 0},  %% Random port
    {telemetry_enabled, false},
    {nats_mode, mock},
    {tracing_enabled, false},
    {disable_heir, true}
]}
```

## Configuration Validation Rules

### Port Validation

- `grpc_port`: Must be in range [1, 65535], or 0 for random port
- `metrics_port`: Must be in range [1, 65535], or 0 for random port

### NATS Configuration Validation

- `nats_url`: Required when `nats_mode = real`
- `nats_tls_*`: Required when `nats_tls_enabled = true`
- `nats_reconnect_attempts`: Must be >= 0
- `nats_reconnect_delay_ms`: Must be > 0
- `nats_max_reconnect_delay_ms`: Must be >= `nats_reconnect_delay_ms`
- `nats_max_pending_operations`: Must be > 0

### CAF Configuration Validation

- `caf_max_retries`: Must be >= 0
- `caf_retry_base_ms`: Must be > 0
- `caf_deadline_multiplier`: Must be > 0.0
- `caf_deadline_min_ms`: Must be > 0
- `caf_deadline_max_ms`: Must be >= `caf_deadline_min_ms`

### Circuit Breaker Configuration Validation

- `circuit_breaker_default_failure_threshold`: Must be > 0
- `circuit_breaker_default_success_threshold`: Must be > 0
- `circuit_breaker_default_timeout_ms`: Must be > 0
- `circuit_breaker_default_half_open_max_calls`: Must be > 0
- `circuit_breaker_default_error_rate_threshold`: Must be in range [0.0, 1.0]
- `circuit_breaker_default_error_rate_window_seconds`: Must be > 0
- `circuit_breaker_default_latency_threshold_ms`: Must be > 0

### Rate Limiting Configuration Validation

- `rate_limiter_default_limit_per_minute`: Must be > 0
- `rate_limiter_window_seconds`: Must be > 0

## Environment Variables

### Production Secrets

**CRITICAL**: Never commit secrets to repository. Use environment variables:

```bash
export BEAMLINE_ROUTER_ADMIN_API_KEY="your-secret-api-key"
export BEAMLINE_ROUTER_NATS_URL="nats://nats-server:4222"
export BEAMLINE_ROUTER_NATS_TLS_CERT_FILE="/etc/ssl/certs/router.pem"
export BEAMLINE_ROUTER_NATS_TLS_KEY_FILE="/etc/ssl/private/router.key"
export BEAMLINE_ROUTER_NATS_TLS_CA_FILE="/etc/ssl/certs/ca.pem"
```

### Configuration Loading

Configuration is loaded from:
1. `sys.config` (if present)
2. `rebar.config` (application environment)
3. Environment variables (via `application:get_env/2`)
4. Runtime `application:set_env/3` calls

## References

- `docs/CONFIG.md` - CAF integration configuration
- `src/beamline_router.app.src` - Application resource file
- `config/` - Configuration files directory

