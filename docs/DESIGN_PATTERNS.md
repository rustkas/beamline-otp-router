# Design Patterns

This document describes design patterns used in the router project, including lifecycle patterns and error handling patterns.

## Lifecycle Patterns

### Reset/Lifecycle Pattern

The reset/lifecycle pattern provides a safe way to reset gen_server state for testing without killing the process.

#### Pattern Overview

The pattern consists of:

1. **Public API Function**: `reset/0` - Wraps gen_server call with error handling
2. **Gen Server Handler**: `handle_call(reset_all, ...)` - Clears ETS tables but keeps process alive
3. **Error Handling**: Handles noproc, timeout, and other errors gracefully

#### Pattern Structure

```erlang
%% Public API
-spec reset() -> ok | {error, term()}.
reset() ->
    try
        gen_server:call(?MODULE, reset_all, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(~"Server not running for reset", #{
                ~"event" => ~"module_reset"
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(~"Reset timeout", #{
                ~"event" => ~"module_reset"
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(~"Reset error", #{
                ~"event" => ~"module_reset",
                ~"error" => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

%% Gen Server Handler
handle_call(reset_all, _From, State = #state{table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(~"Reset_all: ETS table undefined", #{
                ~"event" => ~"module_reset_all"
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(~"Reset_all: table cleared", #{
                ~"event" => ~"module_reset_all"
            }),
            {reply, ok, State}
    end;
```

#### Implementation Checklist

When implementing the reset/lifecycle pattern:

- [ ] Add `reset/0` function to public API exports
- [ ] Implement `reset/0` with try-catch error handling
- [ ] Add `handle_call(reset_all, ...)` handler
- [ ] Check ETS table existence before clearing
- [ ] Log warnings/errors appropriately
- [ ] Handle multiple tables if applicable (use safe deletion)
- [ ] Keep process alive (don't kill or restart)

#### Reference Implementations

**Simple Single Table** (router_circuit_breaker.erl):
```erlang
handle_call(reset_all, _From, State = #state{table = Table}) ->
    case ets:info(Table) of
        undefined ->
            router_logger:warn(~"Circuit breaker reset_all: ETS table undefined", #{
                ~"event" => ~"circuit_breaker_reset_all"
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(~"Circuit breaker reset_all: table cleared", #{
                ~"event" => ~"circuit_breaker_reset_all"
            }),
            {reply, ok, State}
    end;
```

**Multiple Tables** (router_rbac.erl):
```erlang
handle_call(reset_all, _From, State) ->
    SafeDeleteAllObjects = fun(Table) ->
        try
            case catch ets:info(Table) of
                undefined -> ok;
                {'EXIT', _} -> ok;
                _Info when is_list(_Info) ->
                    ets:delete_all_objects(Table),
                    ok;
                _ -> ok
            end
        catch
            _:_ -> ok
        end
    end,
    
    SafeDeleteAllObjects(State#state.roles_table),
    SafeDeleteAllObjects(State#state.user_roles_table),
    SafeDeleteAllObjects(State#state.permissions_table),
    {reply, ok, State}
end;
```

**With Index Table** (router_policy_store.erl):
```erlang
handle_call(reset_all, _From, State = #state{table = Table, index_table = IndexTable}) ->
    case ets:info(Table) of
        undefined ->
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            case IndexTable of
                undefined -> ok;
                _ -> ets:delete_all_objects(IndexTable)
            end,
            %% Reload fixtures after reset
            load_fixtures(Table),
            %% Rebuild index after reset
            case IndexTable of
                undefined -> ok;
                _ -> rebuild_index(Table, IndexTable)
            end,
            {reply, ok, State}
    end;
```

#### Template

See `src/router_gen_server_lifecycle_template.erl` for a complete template.

### Modules Using Reset/Lifecycle Pattern

- ✅ `router_circuit_breaker.erl` - Single table reset
- ✅ `router_rbac.erl` - Multiple tables with safe deletion
- ✅ `router_rate_limit_store.erl` - Single table reset
- ✅ `router_rate_limiter.erl` - Single table reset
- ✅ `router_policy_store.erl` - Multiple tables with index rebuild
- ✅ `router_idem.erl` - ETS table reset (non-gen_server)

## Error Handling Patterns

### Error Handling Overview

The router project uses standardized error handling patterns to ensure consistency across all modules.

#### Error Handling Principles

1. **Consistent Return Values**: Always return `{error, Reason}` for errors
2. **Error Mapping**: Use `router_error:to_grpc/1` for gRPC error mapping
3. **Structured Logging**: Use `router_logger` for all error logging
4. **Error Sanitization**: Sanitize error messages to prevent information disclosure
5. **Error Recovery**: Use try-catch for error recovery where appropriate

#### Error Return Pattern

```erlang
%% Standard error return format
{error, Reason}  %% Reason is an atom or {atom(), map()}

%% Examples:
{error, invalid_input}
{error, not_found}
{error, {timeout, #{context => ~"operation"}}}
```

#### Error Mapping Pattern

```erlang
%% Map internal error to gRPC status code
{Status, Message} = router_error:to_grpc(ErrorReason),
{error, {Status, Message}}

%% Error mapping table:
%% - invalid_input → INVALID_ARGUMENT (3)
%% - not_found → NOT_FOUND (5)
%% - rate_limit_exceeded → RESOURCE_EXHAUSTED (8)
%% - timeout → UNAVAILABLE (14)
%% - internal_error → INTERNAL (13)
```

#### Error Logging Pattern

```erlang
%% Log error with context
router_logger:error(~"Operation failed", #{
    ~"error" => Reason,
    ~"context" => Context,
    ~"input" => sanitize_input(Input)
}).

%% Sanitize sensitive data
sanitize_input(Input) ->
    maps:remove(~"password", maps:remove(~"api_key", Input)).
```

#### Try-Catch Pattern

```erlang
try
    %% Operation that may fail
    Result = risky_operation(Input),
    {ok, Result}
catch
    Class:Reason:Stack ->
        %% Log error with full context
        router_logger:error(~"Operation error", #{
            ~"error" => {Class, Reason},
            ~"stack" => iolist_to_binary(io_lib:format("~p", [Stack])),
            ~"input" => sanitize_input(Input)
        }),
        {error, internal_error}
end.
```

#### Error Recovery Pattern

```erlang
case primary_operation(Input) of
    {ok, Result} ->
        {ok, Result};
    {error, Reason} ->
        %% Attempt recovery
        case recover_from_error(Reason, Input) of
            {ok, RecoveredResult} ->
                router_logger:info(~"Recovered from error", #{
                    ~"error" => Reason,
                    ~"recovery" => success
                }),
                {ok, RecoveredResult};
            {error, RecoveryError} ->
                router_logger:error(~"Recovery failed", #{
                    ~"original_error" => Reason,
                    ~"recovery_error" => RecoveryError
                }),
                {error, RecoveryError}
        end
end.
```

#### Implementation Checklist

When implementing error handling:

- [ ] Use `{error, Reason}` for all error returns
- [ ] Use `router_error:to_grpc/1` for gRPC error mapping
- [ ] Use `router_logger` for all error logging
- [ ] Sanitize sensitive data in error messages
- [ ] Use try-catch for error recovery where appropriate
- [ ] Log errors with sufficient context for debugging
- [ ] Map errors to appropriate gRPC status codes

#### Reference Implementations

**Basic Error Handling** (router_core.erl):
```erlang
case validate_tenant_id(TenantId, Message) of
    {error, ErrorInfo} ->
        handle_routing_error({error, ErrorInfo}, TenantId, FinalPolicyId, Message),
        {error, ErrorInfo};
    ok ->
        %% Continue processing
end.
```

**Error Mapping** (router_grpc.erl):
```erlang
case router_core:route(Message, Context) of
    {ok, Decision} ->
        {ok, Decision};
    {error, ErrorReason} ->
        {Status, Message} = router_error:to_grpc(ErrorReason),
        {error, {Status, Message}}
end.
```

**Error Recovery** (router_rbac.erl):
```erlang
try
    gen_server:call(?MODULE, reset_all, 5000)
catch
    exit:{noproc, _} ->
        router_logger:error(~"RBAC server not running for reset", #{
            ~"event" => ~"rbac_reset"
        }),
        {error, service_unavailable};
    exit:{timeout, _} ->
        router_logger:error(~"RBAC reset timeout", #{
            ~"event" => ~"rbac_reset"
        }),
        {error, timeout};
    Class:Reason ->
        router_logger:error(~"RBAC reset error", #{
            ~"event" => ~"rbac_reset",
            ~"error" => {Class, Reason}
        }),
        {error, {Class, Reason}}
end.
```

#### Template

See `src/router_error_handling_template.erl` for a complete template.

### Error Handling in Different Contexts

#### gRPC Error Handling

```erlang
%% Map error to gRPC status
{Status, Message} = router_error:to_grpc(ErrorReason),
grpcbox_stream:send_error(Stream, Status, Message).
```

#### NATS Error Handling

```erlang
%% Send error response via NATS
ErrorResponse = build_error_response(Request, ErrorReason, ErrorContext),
ErrorResponseJson = jsx:encode(ErrorResponse),
ReplySubject = <<Subject/binary, ".reply">>,
router_nats:publish(ReplySubject, ErrorResponseJson).
```

#### Internal Error Handling

```erlang
%% Normalize error for internal use
NormalizedError = normalize_error(ErrorInfo),
handle_routing_error(NormalizedError, TenantId, FinalPolicyId, Message).
```

### Error Codes Reference

See `src/router_error.erl` for complete error code mapping:

- **INVALID_ARGUMENT (3)**: Client input errors
- **NOT_FOUND (5)**: Resource not found
- **PERMISSION_DENIED (7)**: Authorization failures
- **RESOURCE_EXHAUSTED (8)**: Rate limits, quotas
- **INTERNAL (13)**: Internal server errors
- **UNAVAILABLE (14)**: Service temporarily unavailable
- **UNAUTHENTICATED (16)**: Authentication failures

## Pattern Templates

### Lifecycle Pattern Template

See `src/router_gen_server_lifecycle_template.erl` for:
- Complete gen_server lifecycle pattern
- Reset function implementation
- Error handling in reset
- Multiple table support

### Error Handling Template

See `src/router_error_handling_template.erl` for:
- Basic error handling
- Error mapping to gRPC
- Error recovery patterns
- Error sanitization

## Related Documentation

- `src/router_circuit_breaker.erl` - Lifecycle pattern reference
- `src/router_rbac.erl` - Multiple table lifecycle pattern
- `src/router_error.erl` - Error mapping reference
- `src/router_core.erl` - Error handling reference
- `SECURITY_GUIDE.md#error-handling` - Security best practices

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

