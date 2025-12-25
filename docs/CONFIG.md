# Router Configuration Reference

## Overview

This document describes all configuration options for Router application, with focus on CAF integration settings.

**Version**: CP1  
**Last Updated**: 2025-11-30

## Application Environment Variables

All configuration is done via `application:set_env/3` or in `sys.config` / `rebar.config`:

```erlang
{beamline_router, [
    %% Configuration options here
]}
```

## CAF Integration Configuration

### `caf_push_assignment_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Global enable/disable flag for CAF assignment publishing (kill switch)

**Usage**:
```erlang
%% Disable all CAF assignments globally (kill switch)
{beamline_router, [
    {caf_push_assignment_enabled, false}
]}
```

**Behavior**:
- When `false`: All `push_assignment` requests are ignored, `assignments_skipped_total` counter incremented
- When `true`: Normal processing (subject to tenant allowlist)
- **Kill Switch**: This flag acts as an emergency shutdown for CAF assignment publishing. When set to `false`, no assignments will be published regardless of `push_assignment` flag in requests.

**Use Cases**:
- **Emergency shutdown**: Disable CAF publishing during incidents without code changes
- **Maintenance mode**: Temporarily stop publishing during CAF maintenance
- **Testing**: Disable publishing in test environments

**Location**: Checked in `router_nats_subscriber.erl` and `router_caf_adapter.erl`

**Example - Emergency Shutdown**:
```erlang
%% In production, during incident:
application:set_env(beamline_router, caf_push_assignment_enabled, false),
%% All new requests will skip CAF publishing, incrementing assignments_skipped_total
%% Re-enable after incident:
application:set_env(beamline_router, caf_push_assignment_enabled, true)
```

### `caf_assignment_subject`

**Type**: `binary()` | `string()`  
**Default**: `~"caf.exec.assign.v1"`  
**Description**: Default NATS subject for ExecAssignment publication

**Default Value**: `"caf.exec.assign.v1"` (as binary: `~"caf.exec.assign.v1"`)

**Usage**:
```erlang
{beamline_router, [
    {caf_assignment_subject, ~"caf.exec.assign.v1.custom"}
]}
```

**Behavior**:
- Used when `DecideRequest.assignment_subject` is not provided
- Can be overridden per request via `DecideRequest.assignment_subject`

### `caf_push_assignment_allowed_tenants`

**Type**: `list(binary())` | `map()` | `undefined`  
**Default**: `undefined`  
**Description**: Tenant allowlist for CAF assignment publishing

**Usage**:
```erlang
%% List of allowed tenants
{beamline_router, [
    {caf_push_assignment_allowed_tenants, [~"tenant1", ~"tenant2"]}
]}

%% Map of allowed tenants (for future extensions)
{beamline_router, [
    {caf_push_assignment_allowed_tenants, #{
        ~"tenant1" => #{},
        ~"tenant2" => #{}
    }}
]}
```

**Behavior**:
- `undefined`: Allow all tenants (default)
- `list()`: Only tenants in list are allowed
- `map()`: Only tenants with keys in map are allowed
- If allowlist is configured and `tenant_id` is missing, assignment is blocked
- Blocked assignments increment `assignments_blocked_total` counter

### `caf_max_retries`

**Type**: `non_neg_integer()`  
**Default**: `3`  
**Description**: Maximum number of retry attempts for NATS publish failures

**Default Value**: `3`

**Usage**:
```erlang
{beamline_router, [
    {caf_max_retries, 5}
]}
```

**Behavior**:
- Retries use exponential backoff with jitter
- Each retry increments `assignments_retry_total` counter
- After max retries, `assignments_failed_total` counter incremented

### `caf_retry_base_ms`

**Type**: `non_neg_integer()`  
**Default**: `100`  
**Description**: Base delay for exponential backoff (milliseconds)

**Default Value**: `100` milliseconds

**Usage**:
```erlang
{beamline_router, [
    {caf_retry_base_ms, 200}
]}
```

**Behavior**:
- Formula: `delay = base_ms * 2^retry + jitter`
- Jitter: Random value up to 10% of calculated delay
- Example: retry 1 = ~200ms, retry 2 = ~400ms, retry 3 = ~800ms

### `caf_deadline_multiplier`

**Type**: `number()`  
**Default**: `5`  
**Description**: Multiplier for calculating deadline from expected latency

**Default Value**: `5`

**Usage**:
```erlang
{beamline_router, [
    {caf_deadline_multiplier, 10}
]}
```

**Behavior**:
- Formula: `deadline = expected_latency_ms * multiplier`
- Applied before min/max caps
- Example: 1000ms latency * 5 = 5000ms deadline

### `caf_deadline_min_ms`

**Type**: `non_neg_integer()`  
**Default**: `5000`  
**Description**: Minimum deadline value (milliseconds)

**Default Value**: `5000` milliseconds

**Usage**:
```erlang
{beamline_router, [
    {caf_deadline_min_ms, 10000}
]}
```

**Behavior**:
- Applied after multiplier calculation
- Example: 100ms latency * 5 = 500ms, but min is 5000ms → deadline = 5000ms

### `caf_deadline_max_ms`

**Type**: `non_neg_integer()`  
**Default**: `60000`  
**Description**: Maximum deadline value (milliseconds)

**Default Value**: `60000` milliseconds (60 seconds)

**Usage**:
```erlang
{beamline_router, [
    {caf_deadline_max_ms, 120000}
]}
```

**Behavior**:
- Applied after multiplier calculation
- Example: 20000ms latency * 5 = 100000ms, but max is 60000ms → deadline = 60000ms
- Warning logged if deadline exceeds expected latency by >10x

## NATS Configuration

### `nats_url`

**Type**: `string()`  
**Default**: `"nats://localhost:4222"`  
**Description**: NATS server URL

**Usage**:
```erlang
{beamline_router, [
    {nats_url, "nats://nats-server:4222"}
]}
```

### `nats_mode`

**Type**: `real` | `mock`  
**Default**: `real`  
**Description**: NATS connection mode

**Usage**:
```erlang
%% Use mock mode for testing
{beamline_router, [
    {nats_mode, mock}
]}
```

**Behavior**:
- `real`: Connect to actual NATS server with **real JetStream support**
  - JetStream durable subscriptions with explicit ack policy
  - Publication with acknowledgment (pub ack) for guaranteed delivery
  - Automatic reconnection with configurable retry attempts
- `mock`: Use in-memory mock (for testing)

### `nats_max_payload_size`

**Type**: `integer()`  
**Default**: `1048576` (1 MB)  
**Description**: Maximum payload size in bytes for NATS messages. Messages exceeding this limit are rejected before parsing.

**Usage**:
```erlang
{beamline_router, [
    {nats_max_payload_size, 2097152}  %% 2 MB
]}
```

**Behavior**:
- Early validation before JSON parsing
- Prevents memory exhaustion from oversized messages
- Error logged with actual and maximum sizes

**Recommendations**:
- Set based on expected `DecideRequest` size
- Consider `ExecAssignment` size when using `push_assignment`
- Default 1MB should be sufficient for most use cases

### `nats_tls_enabled`

**Type**: `boolean()`  
**Default**: `false`  
**Description**: Enable TLS for NATS connections.

**Usage**:
```erlang
{beamline_router, [
    {nats_tls_enabled, true},
    {nats_tls_cert_file, "/path/to/cert.pem"},
    {nats_tls_key_file, "/path/to/key.pem"},
    {nats_tls_ca_file, "/path/to/ca.pem"}
]}
```

**Behavior**:
- When `true`, NATS connections use TLS
- Requires certificate and key files (or CA file for verification)
- If files are missing, connection fails with error

### `nats_tls_cert_file`

**Type**: `string()` | `undefined`  
**Default**: `undefined`  
**Description**: Path to TLS certificate file (PEM format).

**Required**: Only when `nats_tls_enabled = true`

### `nats_tls_key_file`

**Type**: `string()` | `undefined`  
**Default**: `undefined`  
**Description**: Path to TLS private key file (PEM format).

**Required**: Only when `nats_tls_enabled = true`

### `nats_tls_ca_file`

**Type**: `string()` | `undefined`  
**Default**: `undefined`  
**Description**: Path to TLS CA certificate file for server verification (PEM format).

**Usage**: Optional, for custom CA verification

### `nats_connect_timeout_ms`

**Type**: `integer()`  
**Default**: `5000` (5 seconds)  
**Description**: NATS connection timeout in milliseconds.

**Usage**:
```erlang
{beamline_router, [
    {nats_connect_timeout_ms, 10000}  %% 10 seconds
]}
```

### `nats_reconnect_attempts`

**Type**: `integer()`
**Default**: `10`
**Description**: Maximum number of reconnection attempts after connection loss.

**Usage**:
```erlang
{beamline_router, [
    {nats_reconnect_attempts, 20}
]}
```

**Behavior**:
- After max attempts, connection is considered failed
- Error logged with attempt count

## CP1-LC: Result and ACK Consumer Configuration

### `assignment_enabled`

**Type**: `boolean()`
**Default**: `true`
**Description**: Enable assignment publishing (alias for `caf_push_assignment_enabled`)

**Usage**:
```erlang
{beamline_router, [
    {assignment_enabled, true}
]}
```

**Note**: This is an alias for `caf_push_assignment_enabled` for consistency.

### `ack_enabled`

**Type**: `boolean()`
**Default**: `false`
**Description**: Enable ACK consumer for ExecAssignmentAck messages

**Usage**:
```erlang
{beamline_router, [
    {ack_enabled, true}
]}
```

**Behavior**:
- When `false`: ACK consumer does not start (default, optional feature)
- When `true`: ACK consumer subscribes to `ack_subject` and processes acknowledgments
- ACK consumer logs accepted/rejected status and increments rejection counters

### `ack_subject`

**Type**: `binary()` | `string()`
**Default**: `~"caf.exec.assign.v1.ack"`
**Description**: NATS subject for ExecAssignmentAck messages

**Usage**:
```erlang
{beamline_router, [
    {ack_subject, ~"caf.exec.assign.v1.ack"}
]}
```

### `result_subject`

**Type**: `binary()` | `string()`
**Default**: `~"caf.exec.result.v1"`
**Description**: NATS subject for ExecResult messages from CAF

**Usage**:
```erlang
{beamline_router, [
    {result_subject, ~"caf.exec.result.v1"}
]}
```

**Behavior**:
- Router subscribes to this subject to receive execution results
- Results are parsed, correlated, and usage events are emitted

### `usage_subject`

**Type**: `binary()` | `string()`
**Default**: `~"beamline.usage.v1.metered"`
**Description**: NATS subject for usage metering events

**Usage**:
```erlang
{beamline_router, [
    {usage_subject, ~"beamline.usage.v1.metered"}
]}
```

**Behavior**:
- Router publishes usage events to this subject after processing ExecResult
- Usage events contain tenant_id, provider_id, event_type, latency_ms, cost, status

### `nats_js_durable_group_results`

**Type**: `binary()` | `string()`
**Default**: `~"router-results"`
**Description**: JetStream durable consumer group name for result subscription

**Usage**:
```erlang
{beamline_router, [
    {nats_js_durable_group_results, ~"router-results"}
]}
```

**Behavior**:
- Used for JetStream durable subscription to `result_subject`
- Ensures message delivery even if Router restarts
- **Real JetStream implementation**: Router uses JetStream durable consumers with explicit ack policy
- Messages are acknowledged after successful processing
- Supports push mode for automatic message delivery

## JetStream Features

Router implements **real NATS/JetStream client** with the following features:

### JetStream Durable Subscriptions

- **Durable Consumers**: Subscriptions use durable consumer names (`nats_js_durable_group_results`, `nats_js_durable_group_acks`)
- **Ack Policy**: Explicit acknowledgment policy (messages must be explicitly acked)
- **Delivery Mode**: Push mode for automatic message delivery
- **Message Persistence**: Messages are persisted on the server until acknowledged
- **Automatic Replay**: Unacknowledged messages are redelivered on consumer restart

### JetStream Publication with Acknowledgment

- **Pub Ack**: All `ExecAssignment` publications use JetStream `publish_with_ack` for guaranteed delivery
- **Acknowledgment ID**: Each publication receives a unique pub ack ID for tracking
- **Error Handling**: Publication failures are retried with exponential backoff
- **Telemetry**: Pub ack IDs are included in telemetry events for correlation

### JetStream Message Acknowledgment

- **ACK**: Messages are acknowledged after successful processing (`router_nats:ack_message/1`)
- **NAK**: Negative acknowledgment for message redelivery (`router_nats:nak_message/1`)
- **In-Progress**: Mark messages as in-progress to extend ack wait time (`router_nats:in_progress_message/1`)

### `result_ack_allowed_tenants`

**Type**: `list()` | `map()` | `undefined`  
**Default**: `undefined`  
**Description**: Allowlist for tenant validation in ExecResult and ExecAssignmentAck messages. If `undefined`, all tenants are allowed.

**Usage**:
```erlang
{beamline_router, [
    %% List of allowed tenant IDs
    {result_ack_allowed_tenants, [~"tenant1", ~"tenant2"]},
    
    %% Or map for additional metadata
    {result_ack_allowed_tenants, #{
        ~"tenant1" => #{},
        ~"tenant2" => #{}
    }}
]}
```

**Behavior**:
- If `undefined`: All tenants are allowed (no validation)
- If list/map: Only tenants in allowlist are allowed
- Tenant validation checks:
  1. Allowlist check (if configured)
  2. Policy registry check (if policy exists for tenant)
- Failed validations emit audit events and reject messages
- Messages are not acknowledged on validation failure (redelivered)

**Audit Events**:
- `router_tenant_audit_total`: Counter for all tenant validation events
- `router_results_tenant_rejected_total`: Counter for rejected ExecResult messages
- `router_acks_tenant_rejected_total`: Counter for rejected ACK messages

### `nats_js_durable_group_acks`

**Type**: `binary()` | `string()`
**Default**: `~"router-acks"`
**Description**: JetStream durable consumer group name for ACK subscription

**Usage**:
```erlang
{beamline_router, [
    {nats_js_durable_group_acks, ~"router-acks"}
]}
```

**Behavior**:
- Used for JetStream durable subscription to `ack_subject`
- **Real JetStream implementation**: Router uses JetStream durable consumers with explicit ack policy
- Messages are acknowledged after successful processing
- Supports push mode for automatic message delivery
- Only used when `ack_enabled = true`
- Ensures ACK delivery even if Router restarts

### `nats_js_deliver_group_results`

**Type**: `binary()` | `string()` | `undefined`
**Default**: `~"router-results-group"`
**Description**: Queue group name for JetStream result consumer. Enables horizontal scaling - multiple Router instances share message load. Each message is delivered to only one consumer in the group.

**Usage**:
```erlang
{beamline_router, [
    {nats_js_deliver_group_results, ~"router-results-group"}
]}
```

**Behavior**:
- Multiple Router instances with same `nats_js_deliver_group_results` form a queue group
- Messages are load-balanced across consumers in the group
- Each message is delivered to exactly one consumer
- Set to `undefined` to disable queue group (single consumer)

### `nats_js_deliver_group_acks`

**Type**: `binary()` | `string()` | `undefined`
**Default**: `~"router-acks-group"`
**Description**: Queue group name for JetStream ACK consumer. Enables horizontal scaling.

**Usage**:
```erlang
{beamline_router, [
    {nats_js_deliver_group_acks, ~"router-acks-group"}
]}
```

**Behavior**:
- Multiple Router instances with same `nats_js_deliver_group_acks` form a queue group
- Messages are load-balanced across consumers in the group
- Set to `undefined` to disable queue group (single consumer)

### `nats_js_max_deliver`

**Type**: `integer()`
**Default**: `3`
**Description**: Maximum number of delivery attempts for JetStream messages. After MaxDeliver attempts, message goes to DLQ or is discarded.

**Usage**:
```erlang
{beamline_router, [
    {nats_js_max_deliver, 3}
]}
```

**Behavior**:
- Prevents infinite redelivery loops
- After MaxDeliver attempts, message is no longer redelivered
- Should match length of `nats_js_backoff_seconds` array

### `nats_js_ack_wait_seconds`

**Type**: `integer()`
**Default**: `30`
**Description**: ACK wait time in seconds. If message is not ACKed within this time, it will be redelivered.

**Usage**:
```erlang
{beamline_router, [
    {nats_js_ack_wait_seconds, 30}
]}
```

**Behavior**:
- Timer starts when message is delivered
- If ACK not received within this time, message is redelivered
- Should be set based on expected processing time

### `nats_js_backoff_seconds`

**Type**: `[integer()]`
**Default**: `[1, 2, 4]`
**Description**: Exponential backoff between redeliveries in seconds. Array length should match `nats_js_max_deliver`.

**Usage**:
```erlang
{beamline_router, [
    {nats_js_backoff_seconds, [1, 2, 4]}  %% First redelivery: 1s, second: 2s, third: 4s
]}
```

**Behavior**:
- Exponential backoff reduces load on system during failures
- First redelivery after 1 second, second after 2 seconds, third after 4 seconds
- Array length must match `nats_js_max_deliver`
- Used when NAK is called for controlled redelivery

**NAK Behavior**:
- When tenant validation fails, Router calls `router_nats:nak_message(MsgId)` to request redelivery
- NAK respects `MaxDeliver` configuration - message will be redelivered up to `MaxDeliver` times
- Backoff delays are applied between redeliveries
- After `MaxDeliver` attempts, message is no longer redelivered (goes to DLQ or discarded)
- Redelivery metric `router_jetstream_redelivery_total` is emitted on each NAK call

### `idempotency_ttl_seconds`

**Type**: `integer()`
**Default**: `3600` (1 hour)
**Description**: Time-to-live for idempotency cache entries in seconds. Messages processed within TTL are considered duplicates.

**Usage**:
```erlang
{beamline_router, [
    {idempotency_ttl_seconds, 3600}  %% 1 hour
]}
```

**Behavior**:
- ETS-based idempotency cache prevents duplicate processing
- Supports keys: `assignment_id`, `request_id`, `ack_id`, `usage_id`
- Atomic check-and-mark operations via `gen_server:call`
- Automatic cleanup of expired entries
- Metrics: `router_idempotency_hit_total` (cache hit), `router_idempotency_miss_total` (cache miss)

**Idempotency Protection**:
- **Result Processing**: Prevents duplicate ExecResult processing by `assignment_id` or `request_id`
- **ACK Processing**: Prevents duplicate ExecAssignmentAck processing by `ack_id`
- **Usage Emission**: Prevents duplicate usage event emission by `usage_id`
- **Concurrent Processing**: Atomic operations ensure idempotency even under concurrent load

**Metrics**:
- `router_idempotency_hit_total{key_type, message_id}`: Message already seen (cache hit)
- `router_idempotency_miss_total{key_type, message_id, reason}`: Message not seen or expired (cache miss)

### `nats_subject`

**Type**: `binary()`  
**Default**: `~"beamline.router.v1.decide"`  
**Description**: NATS subject for incoming DecideRequest messages

**Usage**:
```erlang
{beamline_router, [
    {nats_subject, ~"beamline.router.v1.decide"}
]}
```

### `nats_timeout_ms`

**Type**: `non_neg_integer()`  
**Default**: `5000`  
**Description**: NATS operation timeout (milliseconds)

**Usage**:
```erlang
{beamline_router, [
    {nats_timeout_ms, 10000}
]}
```

## gRPC Configuration

### `grpc_port`

**Type**: `non_neg_integer()`  
**Default**: `9000`  
**Description**: gRPC server port

**Usage**:
```erlang
{beamline_router, [
    {grpc_port, 9001}
]}
```

**Note**: Set to `0` for ephemeral port (OS-assigned)

### `grpc_enabled`

**Type**: `boolean()`  
**Default**: `true`  
**Description**: Enable/disable gRPC server

**Usage**:
```erlang
%% Disable gRPC (for NATS-only mode)
{beamline_router, [
    {grpc_enabled, false}
]}
```

**Behavior**:
- When `false`: gRPC server not started, application continues without gRPC
- When proto modules not found: Warning logged, empty supervisor tree returned

## Performance Thresholds

### `latency_warn_ms`

**Type**: `non_neg_integer()`  
**Default**: `10`  
**Description**: Warning threshold for operation latency (milliseconds)

### `latency_crit_ms`

**Type**: `non_neg_integer()`  
**Default**: `50`  
**Description**: Critical threshold for operation latency (milliseconds)

### `list_policies_latency_warn_ms`

**Type**: `non_neg_integer()`  
**Default**: `5`  
**Description**: Warning threshold for list_policies operation (milliseconds)

### `queue_warn`

**Type**: `non_neg_integer()`  
**Default**: `10`  
**Description**: Warning threshold for queue length

### `queue_crit`

**Type**: `non_neg_integer()`  
**Default**: `100`  
**Description**: Critical threshold for queue length

### `policy_count_warn`

**Type**: `non_neg_integer()`  
**Default**: `100`  
**Description**: Warning threshold for policy count

## Security Configuration

### `admin_api_key`

**Type**: `binary()`  
**Default**: `~"dev-admin-key"`  
**Description**: API key for admin operations (gRPC)

**Usage**:
```erlang
{beamline_router, [
    {admin_api_key, ~"production-api-key"}
]}
```

**Security Note**: Never commit real API keys to version control. Use CI/CD secrets or environment variables.

## Configuration Examples

### Development Configuration

```erlang
{beamline_router, [
    {grpc_port, 9000},
    {grpc_enabled, true},
    {nats_mode, mock},  %% Use mock for local development
    {caf_push_assignment_enabled, true},
    {caf_max_retries, 3},
    {caf_retry_base_ms, 100}
]}
```

### Production Configuration

```erlang
{beamline_router, [
    {grpc_port, 9000},
    {grpc_enabled, true},
    {nats_url, "nats://nats-prod:4222"},
    {nats_mode, real},
    {caf_push_assignment_enabled, true},
    {caf_assignment_subject, ~"caf.exec.assign.v1"},
    {caf_push_assignment_allowed_tenants, [~"tenant1", ~"tenant2"]},
    {caf_max_retries, 5},
    {caf_retry_base_ms, 200},
    {caf_deadline_multiplier, 5},
    {caf_deadline_min_ms, 5000},
    {caf_deadline_max_ms, 60000},
    {admin_api_key, ~"${ADMIN_API_KEY}"}  %% From environment variable
]}
```

### Testing Configuration

```erlang
{beamline_router, [
    {grpc_port, 0},  %% Ephemeral port
    {grpc_enabled, false},  %% Disable gRPC for tests
    {nats_mode, mock},  %% Use mock NATS
    {caf_push_assignment_enabled, true},
    {caf_max_retries, 2},  %% Fewer retries for faster tests
    {caf_retry_base_ms, 10}  %% Fast retries for tests
]}
```

## Configuration Priority

1. **Request-level** (highest priority):
   - `DecideRequest.assignment_subject` → overrides `caf_assignment_subject`
   - `DecideRequest.options.deadline_ms` → overrides calculated deadline
   - `DecideRequest.options.retry` → overrides default retry config

2. **Application environment** (medium priority):
   - All `caf_*` and `nats_*` settings
   - Applied at application startup

3. **Default values** (lowest priority):
   - Hardcoded defaults in source code
   - Used when environment variable not set

## Runtime Configuration Changes

**Note**: Most configuration is read at application startup. To change configuration:

1. **Stop application**: `application:stop(beamline_router)`
2. **Update environment**: `application:set_env(beamline_router, Key, Value)`
3. **Restart application**: `application:start(beamline_router)`

**Hot-reloadable settings** (future):
- Some settings may support hot-reload in future versions
- Currently, restart required for most changes

## Validation

Configuration is validated at:
- **Application startup**: Invalid values may cause startup failure
- **Runtime**: Invalid values may cause warnings or fallback to defaults

## References

- `src/beamline_router.app.src` - Default configuration
- `src/router_caf_adapter.erl` - CAF adapter implementation
- `src/router_nats_subscriber.erl` - NATS subscriber implementation
- `docs/API_CONTRACTS.md` - Message contracts
- `docs/NATS_SUBJECTS.md` - NATS subject specifications
- `docs/TELEMETRY_CAF_ADAPTER.md` - Telemetry events
### Checkpoint (CP) flags (no DevState integration)

- `current_cp` — binary, текущий чекпойнт (`~"CP1-LC"`, `~"CP2-LC"`).
- `cp2_plus_allowed` — boolean, включает CP2+ возможности (Admin gRPC, idempotency, ack). 

Примечание: DevState — отдельный инструмент и не интегрируется с Router. Управляйте CP флагами через env.

