# Telemetry Events Specification

## Overview

This document describes all telemetry events emitted by Router Core components, including event names, measurements, metadata, and usage examples.

## Router Core Events

### Span Events

#### `[router_core, route, start]`

**Purpose**: Route operation started

**Measurements**: None

**Metadata**:
```erlang
#{
    tenant_id => binary() | undefined,
    policy_id => binary() | undefined
}
```

**Example**:
```erlang
telemetry:span([router_core, route], #{
    tenant_id => <<"default_tenant">>,
    policy_id => <<"default">>
}, fun() ->
    %% Route operation
end).
```

#### `[router_core, route, stop]`

**Purpose**: Route operation completed successfully

**Measurements**:
```erlang
#{
    duration => integer()  %% microseconds
}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    provider_id => binary(),
    reason => <<"weighted">> | <<"sticky">> | <<"fallback">>,
    result => ok
}
```

**Example**:
```erlang
%% Automatically emitted by telemetry:span/3 on successful completion
```

#### `[router_core, route, exception]`

**Purpose**: Route operation failed with exception

**Measurements**:
```erlang
#{
    duration => integer()  %% microseconds
}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    error => atom(),  %% missing_tenant_id | policy_not_found | no_provider_available
    error_context => map(),
    result => error
}
```

**Example**:
```erlang
%% Automatically emitted by telemetry:span/3 on exception
```

### Counter Events

#### `[router_core, routes_total]`

**Purpose**: Total route requests (success + error)

**Measurements**:
```erlang
#{count => 1}
```

**Metadata** (Success):
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    provider_id => binary(),
    reason => <<"weighted">> | <<"sticky">> | <<"fallback">>,
    result => ok
}
```

**Metadata** (Error):
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    result => error,
    error => atom()
}
```

**Example**:
```erlang
telemetry:execute([router_core, routes_total], #{count => 1}, #{
    tenant_id => <<"default_tenant">>,
    policy_id => <<"default">>,
    provider_id => <<"openai">>,
    reason => <<"weighted">>,
    result => ok
}).
```

#### `[router_core, resolutions_total]`

**Purpose**: Successful route resolutions

**Measurements**:
```erlang
#{count => 1}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    provider_id => binary()
}
```

**Example**:
```erlang
telemetry:execute([router_core, resolutions_total], #{count => 1}, #{
    tenant_id => <<"default_tenant">>,
    policy_id => <<"default">>,
    provider_id => <<"openai">>
}).
```

#### `[router_core, errors_total]`

**Purpose**: Route errors by reason

**Measurements**:
```erlang
#{count => 1}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    error => atom(),  %% missing_tenant_id | policy_not_found | no_provider_available
    error_context => map(),
    result => error
}
```

**Example**:
```erlang
telemetry:execute([router_core, errors_total], #{count => 1}, #{
    tenant_id => <<"default_tenant">>,
    policy_id => <<"default">>,
    error => missing_tenant_id,
    error_context => #{
        context => <<"tenant_id is required in message">>,
        message_id => <<"msg_123">>
    },
    result => error
}).
```

## Policy Store Events

### `[router_policy_store, load_policy]`

**Purpose**: Policy loaded from store

**Measurements**:
```erlang
#{
    duration_us => integer(),  %% microseconds
    queue_len => integer()     %% absolute queue length
}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    table => policy_store,
    correlation_id => binary() | undefined
}
```

### `[router_policy_store, upsert_policy]`

**Purpose**: Policy created or updated

**Measurements**:
```erlang
#{
    duration_us => integer(),  %% microseconds
    queue_len => integer(),    %% absolute queue length
    count => 1                 %% always 1 for upsert
}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    table => policy_store,
    correlation_id => binary() | undefined
}
```

### `[router_policy_store, delete_policy]`

**Purpose**: Policy deleted

**Measurements**:
```erlang
#{
    duration_us => integer(),  %% microseconds
    queue_len => integer(),    %% absolute queue length
    count => 1                 %% always 1 for delete
}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    policy_id => binary(),
    table => policy_store,
    correlation_id => binary() | undefined
}
```

### `[router_policy_store, list_policies]`

**Purpose**: Policies listed for tenant

**Measurements**:
```erlang
#{
    duration_us => integer(),  %% microseconds
    queue_len => integer(),    %% absolute queue length
    count => integer()         %% number of policies returned
}
```

**Metadata**:
```erlang
#{
    tenant_id => binary(),
    table => policy_store,
    correlation_id => binary() | undefined
}
```

## Telemetry Handler

The `router_telemetry_handler` aggregates events in-memory:

- **`routes_total`**: Counted by result type (ok/error)
- **`resolutions_total`**: Total count of successful routes
- **`errors_total`**: Counted by error reason
- **`route_duration`**: Last 100 durations per result type

**API**:
- `router_telemetry_handler:get_metrics/0` - Returns aggregated metrics map
- `router_telemetry_handler:reset_metrics/0` - Resets all metrics

## Event Naming Convention

All events follow the pattern: `[component, operation, phase]`

- **Component**: `router_core`, `router_policy_store`
- **Operation**: `route`, `load_policy`, `upsert_policy`, etc.
- **Phase**: `start`, `stop`, `exception` (for spans), or event name (for counters)

## Metadata Fields

### Common Fields

- `tenant_id`: Tenant identifier (binary)
- `policy_id`: Policy identifier (binary)
- `provider_id`: Selected provider (binary, for successful routes)
- `reason`: Routing reason (binary: `<<"weighted">>`, `<<"sticky">>`, `<<"fallback">>`)
- `result`: Operation result (`ok` | `error`)
- `error`: Error reason (atom, for failed operations)
- `error_context`: Error context map (for failed operations)
- `correlation_id`: Correlation identifier for tracing (binary | undefined)
- `table`: ETS table name (atom, for policy store events)

### Units

- **Duration**: microseconds (`duration_us`, `duration`)
- **Queue length**: absolute count (`queue_len`)
- **Count**: absolute count (`count`)

## Usage Examples

### Attaching Handlers

```erlang
telemetry:attach_many(
    <<"router_core_handlers">>,
    [
        [router_core, route, start],
        [router_core, route, stop],
        [router_core, routes_total],
        [router_core, errors_total]
    ],
    fun handle_event/4,
    undefined
).
```

### Querying Metrics

```erlang
Metrics = router_telemetry_handler:get_metrics(),
RoutesTotal = maps:get({routes_total, ok}, Metrics, 0),
ErrorsTotal = maps:get({errors_total, missing_tenant_id}, Metrics, 0).
```

## References

- `router_core.erl`: Router Core telemetry implementation
- `router_policy_store.erl`: Policy Store telemetry implementation
- `router_telemetry_handler.erl`: Telemetry event aggregation

