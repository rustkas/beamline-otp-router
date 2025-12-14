# Router NATS Metrics Contract and Test Harness

## Overview

This document describes the metrics contract for NATS publish operations in the Router and the shared test harness for verifying metric emissions.

## Metrics Contract

### Publish Operations

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `router_nats_publish_total` | `inc` | (none) | Incremented on successful publish |
| `router_nats_publish_attempts_total` | `emit_metric` | `status`, `retry_count` | Tracks publish attempts |
| `router_nats_publish_failures_total` | `emit_metric` | `reason`, `subject`, `stream`, `source` | Tracks publish failures |
| `router_nats_publish_errors_total` | `emit_metric` | `error_type` | Tracks publish errors by type |
| `router_nats_publish_latency_seconds` | `emit_metric` | (none) | Publish latency histogram |

### Publish With Ack Operations

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `router_nats_publish_with_ack_total` | `inc` | (none) | Incremented on successful publish_with_ack |
| `router_nats_publish_with_ack_failures_total` | `emit_metric` | `reason`, `subject`, `stream`, `source` | Tracks publish_with_ack failures |

### Connection Operations

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `router_nats_connection_established_total` | `inc` | (none) | Connection established |
| `router_nats_connection_lost_total` | `inc` | (none) | Connection lost (async, via DOWN message) |
| `router_nats_connection_restored_total` | `inc` | (none) | Connection restored after reconnect |
| `router_nats_connection_status` | `emit_metric` | `state` | Current connection status (0/0.5/1) |
| `router_nats_reconnect_attempts_total` | `inc` | (none) | Reconnect attempts |
| `router_nats_reconnect_failures_total` | `emit_metric` | `reason`, `cluster`, `attempt` | Reconnect failures |
| `router_nats_reconnection_exhausted_total` | `inc` | (none) | Reconnect attempts exhausted |

## Shared Test Harness

### Module: `router_metrics_test_helper`

Located at `test/router_metrics_test_helper.erl`, this module provides a centralized harness for capturing and asserting metrics in tests.

### Usage

```erlang
%% In init_per_testcase/2:
router_metrics_test_helper:setup(),
Config.

%% In end_per_testcase/2:
router_metrics_test_helper:teardown(),
ok.

%% In test cases:
%% Wait for an emit_metric call
ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1),

%% Wait for an inc call
ok = router_metrics_test_helper:wait_for_inc(router_nats_connection_lost_total, 1),

%% Get all captured metrics
Metrics = router_metrics_test_helper:get_all_metrics(),

%% Dump metrics for debugging
router_metrics_test_helper:debug_dump().
```

### API

| Function | Description |
|----------|-------------|
| `setup()` | Setup metric capture (default options) |
| `setup([{debug, true}])` | Setup with debug logging enabled |
| `teardown()` | Cleanup meck and ETS table |
| `clear()` | Clear captured metrics without teardown |
| `wait_for_metric(Name, Count)` | Wait for N emit_metric calls (2s timeout) |
| `wait_for_metric(Name, Count, TimeoutMs)` | Wait with custom timeout |
| `wait_for_inc(Name, Count)` | Wait for N inc calls (2s timeout) |
| `wait_for_inc(Name, Count, TimeoutMs)` | Wait with custom timeout |
| `get_all_metrics()` | Get all captured metrics |
| `get_metrics_by_name(Name)` | Get metrics filtered by name |

### Captured Metric Format

Metrics are stored as 6-tuples in ETS:
```erlang
{Type, MetricName, Measurements, Labels, Timestamp, UniqueRef}
```

Where:
- `Type` is `emit_metric` or `inc`
- `MetricName` is the metric atom
- `Measurements` is a map (e.g., `#{count => 1}`)
- `Labels` is a map of label values
- `Timestamp` is milliseconds since epoch
- `UniqueRef` is a unique reference to prevent duplicate detection in bag tables

## Test Suites Using This Harness

1. **`router_nats_publish_fail_open_SUITE`** - Tests fail-open mode behavior
2. **`router_nats_publish_queueing_SUITE`** - Tests queueing mode behavior  
3. **`router_nats_publish_metrics_SUITE`** - Authoritative metrics contract tests
4. **`router_metrics_capture_smoke_SUITE`** - Harness smoke tests

## Guidelines

### DO
- Use `router_metrics_test_helper` for all metric-related assertions
- Use `wait_for_metric` for `emit_metric/3` calls
- Use `wait_for_inc` for `inc/1` calls
- Check the correct metric based on the error path (e.g., `publish_failures_total` for publish errors)

### DON'T
- ❌ Set up ad-hoc `meck:expect(router_metrics, emit_metric, fun(_,_,_) -> ok end)` in tests
- ❌ Use conflicting expectations in individual tests when harness is setup
- ❌ Expect `connection_lost_total` for synchronous publish errors (it's async)
- ❌ Use arbitrary sleeps instead of proper metric waiting

## Sync vs Async Metrics

**Synchronous** (emitted before publish returns):
- `router_nats_publish_failures_total`
- `router_nats_publish_with_ack_failures_total`
- `router_nats_publish_errors_total`
- `router_nats_publish_latency_seconds`

**Asynchronous** (emitted via DOWN message processing):
- `router_nats_connection_lost_total`
- Other connection-state metrics
