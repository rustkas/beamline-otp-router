# Configuration Alignment Verification Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **All Configurations Aligned**

## Overview

This report verifies that all configuration values in `beamline_router.app.src` are correctly used in the codebase, specifically:
- MaxDeliver, AckWait, Backoff configuration
- Deliver groups for horizontal scaling
- Idempotency TTL

## Configuration Values Verification

### 1. MaxDeliver Configuration

**Configuration** (`beamline_router.app.src:44`):
```erlang
{nats_js_max_deliver, 3}
```

**Usage** (`router_nats.erl:487`):
```erlang
MaxDeliver = application:get_env(beamline_router, nats_js_max_deliver, 3),
```

**JetStream Consumer Config** (`router_nats.erl:495`):
```erlang
<<"max_deliver">> => MaxDeliver,
```

**Status**: ✅ **Aligned** - Default value `3` matches code usage

### 2. AckWait Configuration

**Configuration** (`beamline_router.app.src:45`):
```erlang
{nats_js_ack_wait_seconds, 30}
```

**Usage** (`router_nats.erl:488`):
```erlang
AckWaitSeconds = application:get_env(beamline_router, nats_js_ack_wait_seconds, 30),
```

**JetStream Consumer Config** (`router_nats.erl:496`):
```erlang
<<"ack_wait">> => AckWaitSeconds * 1000000000,  %% Convert to nanoseconds
```

**Status**: ✅ **Aligned** - Default value `30` seconds matches code usage, converted to nanoseconds

### 3. Backoff Configuration

**Configuration** (`beamline_router.app.src:46`):
```erlang
{nats_js_backoff_seconds, [1, 2, 4]}
```

**Usage** (`router_nats.erl:489`):
```erlang
BackoffSeconds = application:get_env(beamline_router, nats_js_backoff_seconds, [1, 2, 4]),
```

**JetStream Consumer Config** (`router_nats.erl:497`):
```erlang
<<"backoff">> => [B * 1000000000 || B <- BackoffSeconds]  %% Convert to nanoseconds
```

**Status**: ✅ **Aligned** - Default value `[1, 2, 4]` matches code usage, converted to nanoseconds

### 4. Deliver Group for Results

**Configuration** (`beamline_router.app.src:42`):
```erlang
{nats_js_deliver_group_results, <<"router-results-group">>}
```

**Usage** (`router_result_consumer.erl:86`):
```erlang
DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_results, undefined),
```

**Subscription** (`router_result_consumer.erl:87`):
```erlang
router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, DeliverGroup, push)
```

**Status**: ✅ **Aligned** - Default value `<<"router-results-group">>` used in result consumer

### 5. Deliver Group for ACKs

**Configuration** (`beamline_router.app.src:43`):
```erlang
{nats_js_deliver_group_acks, <<"router-acks-group">>}
```

**Usage** (`router_ack_consumer.erl:85`):
```erlang
DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_acks, undefined),
```

**Subscription** (`router_ack_consumer.erl:86`):
```erlang
router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, DeliverGroup, push)
```

**Status**: ✅ **Aligned** - Default value `<<"router-acks-group">>` used in ACK consumer

### 6. Idempotency TTL

**Configuration** (`beamline_router.app.src:47`):
```erlang
{idempotency_ttl_seconds, 3600}
```

**Usage** (`router_idempotency.erl:43`):
```erlang
{ok, TtlSeconds} = application:get_env(beamline_router, idempotency_ttl_seconds, ?DEFAULT_TTL_SECONDS),
```

**Default** (`router_idempotency.erl:14`):
```erlang
-define(DEFAULT_TTL_SECONDS, 3600).  %% 1 hour default TTL
```

**Status**: ✅ **Aligned** - Default value `3600` seconds (1 hour) matches code usage

## Configuration Flow

### JetStream Consumer Creation

**Flow**:
1. `router_result_consumer.erl` or `router_ack_consumer.erl` calls `subscribe_to_results/2` or `subscribe_to_acks/2`
2. Retrieves `DeliverGroup` from application config
3. Calls `router_nats:subscribe_jetstream/5` with `DeliverGroup`
4. `router_nats.erl` retrieves `MaxDeliver`, `AckWaitSeconds`, `BackoffSeconds` from application config
5. Creates JetStream consumer with all configuration values

**Code Path**:
```
router_result_consumer:subscribe_to_results/2
  → router_nats:subscribe_jetstream/5
    → router_nats:do_subscribe_jetstream/6
      → application:get_env(beamline_router, nats_js_max_deliver, 3)
      → application:get_env(beamline_router, nats_js_ack_wait_seconds, 30)
      → application:get_env(beamline_router, nats_js_backoff_seconds, [1, 2, 4])
      → ConsumerConfig with all values
```

## Verification Summary

| Configuration | Default Value | Used In | Status |
|--------------|---------------|---------|--------|
| `nats_js_max_deliver` | `3` | `router_nats.erl:487` | ✅ Aligned |
| `nats_js_ack_wait_seconds` | `30` | `router_nats.erl:488` | ✅ Aligned |
| `nats_js_backoff_seconds` | `[1, 2, 4]` | `router_nats.erl:489` | ✅ Aligned |
| `nats_js_deliver_group_results` | `<<"router-results-group">>` | `router_result_consumer.erl:86` | ✅ Aligned |
| `nats_js_deliver_group_acks` | `<<"router-acks-group">>` | `router_ack_consumer.erl:85` | ✅ Aligned |
| `idempotency_ttl_seconds` | `3600` | `router_idempotency.erl:43` | ✅ Aligned |

## Configuration Usage Details

### MaxDeliver, AckWait, Backoff

**Purpose**: Control JetStream message redelivery behavior

**Configuration Location**: `beamline_router.app.src:44-46`

**Code Usage**:
- Retrieved in `router_nats.erl:do_subscribe_jetstream/6`
- Included in JetStream consumer configuration
- Used by JetStream server for redelivery logic

**Behavior**:
- `MaxDeliver`: Maximum number of delivery attempts (default: 3)
- `AckWaitSeconds`: Time to wait for ACK before redelivery (default: 30 seconds)
- `BackoffSeconds`: Exponential backoff delays between redeliveries (default: [1, 2, 4] seconds)

### Deliver Groups

**Purpose**: Enable horizontal scaling via queue groups

**Configuration Location**: `beamline_router.app.src:42-43`

**Code Usage**:
- Retrieved in `router_result_consumer.erl:subscribe_to_results/2`
- Retrieved in `router_ack_consumer.erl:subscribe_to_acks/2`
- Passed to `router_nats:subscribe_jetstream/5` as `DeliverGroup` parameter

**Behavior**:
- Multiple Router instances with same `DeliverGroup` form a queue group
- Messages are load-balanced across consumers in the group
- Each message delivered to exactly one consumer

### Idempotency TTL

**Purpose**: Control idempotency cache expiration

**Configuration Location**: `beamline_router.app.src:47`

**Code Usage**:
- Retrieved in `router_idempotency.erl:init/1`
- Used to set expiration time for cache entries
- Automatic cleanup runs periodically

**Behavior**:
- Messages processed within TTL are considered duplicates
- Expired entries are automatically cleaned up
- Default: 1 hour (3600 seconds)

## References

- `src/beamline_router.app.src`: Application configuration
- `src/router_nats.erl`: NATS client with JetStream consumer creation
- `src/router_result_consumer.erl`: Result consumer with deliver group
- `src/router_ack_consumer.erl`: ACK consumer with deliver group
- `src/router_idempotency.erl`: Idempotency cache with TTL
- `docs/CONFIG.md`: Configuration documentation

