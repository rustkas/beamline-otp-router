-module(router_extension_invoker).


-doc "Extension Invoker".
%% Invokes extensions via NATS request-reply with retry and timeout handling
-export([invoke/3, invoke_with_retry/4]).

-ignore_xref([
  {router_extension_invoker, update_latency_metrics, 2},
  {router_extension_invoker, create_health_record_with_latency, 2}
]).

-include("../include/beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_extension_invoker]).

%% Returns: {ok, Response} | {error, Reason}
-spec invoke(binary(), map(), map()) -> {ok, map()} | {error, term()}.
invoke(ExtensionId, Request, Context) ->
    case maps:get(~"dry_run", Context, false) of
        true ->
            router_logger:debug(~"Dry-run: skipping extension execution", #{
                ~"extension_id" => ExtensionId,
                ~"tenant_id" => maps:get(~"tenant_id", Context, maps:get(~"tenant_id", Request, undefined)),
                ~"policy_id" => maps:get(~"policy_id", Context, maps:get(~"policy_id", Request, undefined))
            }),
            {ok, #{}};
        _ ->
    %% Check circuit breaker (CP3)
    case check_circuit_breaker(ExtensionId) of
        {ok, allow} ->
            %% Lookup extension with versioning and load balancing (CP3)
            ExtensionResult = case router_extension_versioning:lookup_with_version(ExtensionId, Context) of
                {ok, Ext} ->
                    {ok, Ext};
                {error, _} ->
                    %% Fallback to regular lookup
                    router_extension_registry:lookup(ExtensionId)
            end,
            
            case ExtensionResult of
                {ok, Extension} ->
                    %% Select instance for load balancing (CP3)
                    SubjectResult = case router_extension_load_balancer:select_instance(ExtensionId, Context) of
                        {ok, SelectedSubject} ->
                            %% Use selected subject from load balancer
                            ExtensionWithSubject = Extension#extension{subject = SelectedSubject},
                            {ok, ExtensionWithSubject};
                        {error, _} ->
                            %% Fallback to extension's default subject
                            {ok, Extension}
                    end,
                    
                    case SubjectResult of
                        {ok, FinalExtension} ->
                            invoke_extension_with_health(FinalExtension, Request, Context);
                        {error, Reason} ->
                            {error, {extension_load_balancer_error, #{
                                extension_id => ExtensionId,
                                reason => normalize_error(Reason),
                                context => Context
                            }}}
                    end;
                {error, not_found} ->
                    {error, {extension_not_found, #{
                        extension_id => ExtensionId,
                        reason => ~"Extension not found in registry",
                        context => Context
                    }}};
                {error, Reason} ->
                    {error, {extension_registry_error, #{
                        extension_id => ExtensionId,
                        reason => normalize_error(Reason),
                        context => Context
                    }}}
            end;
        {error, circuit_open} ->
            {error, {extension_circuit_open, #{
                extension_id => ExtensionId,
                reason => ~"Circuit breaker is open",
                context => Context
            }}}
    end
    end.

-spec invoke_with_retry(binary(), map(), map(), integer()) -> {ok, map()} | {error, term()}.
invoke_with_retry(ExtensionId, Request, Context, MaxRetries) ->
    case router_extension_registry:lookup(ExtensionId) of
        {ok, Extension} ->
            invoke_with_retry_internal(Extension, Request, Context, MaxRetries, MaxRetries);
        {error, not_found} ->
            {error, {extension_not_found, ExtensionId}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal: Invoke extension with health tracking
invoke_extension_with_health(Extension, Request, Context) ->
    ExtensionId = Extension#extension.id,
    ExtensionType = Extension#extension.type,
    ExtensionSubject = Extension#extension.subject,
    StartTime = erlang:system_time(microsecond),
    
    %% Extract correlation fields from Context
    TenantId = maps:get(~"tenant_id", Context, maps:get(~"tenant_id", Request, undefined)),
    PolicyId = maps:get(~"policy_id", Context, maps:get(~"policy_id", Request, undefined)),
    
    Result = invoke_extension(Extension, Request, Context),
    
    LatencyMs = (erlang:system_time(microsecond) - StartTime) / 1000.0,
    
    %% Determine status and retries used
    {Status, RetriesUsed} = case Result of
        {ok, _Response} ->
            {success, 0};  % Retries not tracked in current implementation
        {error, {extension_max_retries_exceeded, #{max_retries := MaxRetries}}} ->
            {max_retries_exceeded, MaxRetries};
        {error, {extension_timeout, #{retries := Retries}}} ->
            {timeout, Retries};
        {error, _Reason} ->
            {error, 0}
    end,
    
    %% Emit unified telemetry
    emit_unified_telemetry(ExtensionId, ExtensionType, ExtensionSubject, Status, LatencyMs, RetriesUsed, TenantId, PolicyId),
    
    %% Log structured event
    log_extension_invocation(ExtensionId, ExtensionType, ExtensionSubject, Status, LatencyMs, RetriesUsed, TenantId, PolicyId, Result),
    
    %% Update health metrics based on result
    case Result of
        {ok, Resp} ->
            router_extension_circuit_breaker:record_success(ExtensionId),
            update_health_metrics(ExtensionId, LatencyMs, success),
            {ok, Resp};
        {error, Reason} ->
            router_extension_circuit_breaker:record_failure(ExtensionId),
            update_health_metrics(ExtensionId, LatencyMs, failure),
            {error, Reason}
    end.

%% Internal: Invoke extension
invoke_extension(#extension{timeout_ms = TimeoutMs, retry = Retry} = Extension, Request, Context) ->
    %% Build NATS request payload
    Payload = build_request_payload(Request, Context),
    
    %% Validate payload size (security: prevent DoS)
    case validate_payload_size(Payload, Extension) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            %% Validate metadata size (security: prevent DoS)
            Metadata = maps:get(~"metadata", Request, #{}),
            case validate_metadata_size(Metadata, Extension) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    %% Validate timeout (security: prevent abuse)
                    case validate_timeout(TimeoutMs) of
                        {error, Reason2} ->
                            {error, Reason2};
                        ok ->
                            %% Use retry if configured
                            RetryCount = case Retry of
                                R when is_integer(R), R > 0 -> R;
                                _ -> 0
                            end,
                            
                            %% Validate retry count (security: prevent abuse)
                            case validate_retry_count(RetryCount) of
                                {error, Reason3} ->
                                    {error, Reason3};
                                ok ->
                                    %% MaxRetries passed to internal function represents total attempts (Initial + Retries)
                                    TotalAttempts = RetryCount + 1,
                                    invoke_with_retry_internal(Extension, 
                                                                Request, Context, TotalAttempts, TotalAttempts)
                            end
                    end
            end
    end.

%% Internal: Invoke with retry
invoke_with_retry_internal(Extension, _Request, Context, 0, MaxRetries) ->
    {error, {extension_max_retries_exceeded, #{
        extension_id => Extension#extension.id,
        reason => ~"Maximum retries exceeded",
        max_retries => MaxRetries,
        retries_used => MaxRetries,
        context => Context
    }}};
invoke_with_retry_internal(Extension, Request, Context, RetriesLeft, MaxRetries) ->
    #extension{subject = Subject, timeout_ms = TimeoutMs} = Extension,
    
    %% Build request payload
    Payload = build_request_payload(Request, Context),
    PayloadJson = jsx:encode(Payload),
    
    %% Track retries used
    RetriesUsed = MaxRetries - RetriesLeft,
    
    %% Emit telemetry
    StartTime = erlang:system_time(microsecond),
    
    %% Invoke via NATS
    Result = case router_nats:request(Subject, PayloadJson, TimeoutMs) of
        {ok, ResponseJson} ->
            handle_nats_response(ResponseJson, Extension, StartTime, RetriesUsed);
        {error, timeout} ->
            handle_nats_timeout(Extension, StartTime, RetriesUsed, RetriesLeft, MaxRetries, Request, Context);
        {error, Reason} ->
            handle_nats_error(Reason, Extension, StartTime, RetriesUsed, Context)
    end,
    
    Result.

%% Internal: Handle NATS response
-spec handle_nats_response(binary(), #extension{}, integer(), integer()) ->
    {ok, map()} | {error, atom()}.
handle_nats_response(ResponseJson, Extension, StartTime, RetriesUsed) ->
    Latency = calculate_latency(StartTime),
    case jsx:decode(ResponseJson, [{return_maps, true}]) of
        Response when is_map(Response) ->
            emit_telemetry(success, Extension, Latency, RetriesUsed),
            {ok, Response};
        _ ->
            emit_telemetry(error, Extension, Latency, RetriesUsed),
            {error, invalid_response_format}
    end.

%% Internal: Handle NATS timeout
-spec handle_nats_timeout(#extension{}, integer(), integer(), integer(), integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
handle_nats_timeout(Extension, StartTime, RetriesUsed, RetriesLeft, MaxRetries, Request, Context) ->
    Latency = calculate_latency(StartTime),
    emit_telemetry(timeout, Extension, Latency, RetriesUsed),
    
    %% Retry on timeout if retries left
    case RetriesLeft > 1 of
        true ->
            BackoffMs = calculate_backoff(MaxRetries - RetriesLeft + 1),
            timer:sleep(BackoffMs),
            invoke_with_retry_internal(Extension, Request, Context, RetriesLeft - 1, MaxRetries);
        false ->
            {error, {extension_timeout, #{
                extension_id => Extension#extension.id,
                reason => ~"Extension timeout after retries",
                timeout_ms => Extension#extension.timeout_ms,
                retries => MaxRetries,
                retries_used => MaxRetries,
                context => Context
            }}}
    end.

%% Internal: Handle NATS error
-spec handle_nats_error(term(), #extension{}, integer(), integer(), map()) ->
    {error, term()}.
handle_nats_error(Reason, Extension, StartTime, RetriesUsed, Context) ->
    Latency = calculate_latency(StartTime),
    emit_telemetry(error, Extension, Latency, RetriesUsed),
    {error, {extension_invocation_error, #{
        extension_id => Extension#extension.id,
        reason => normalize_error(Reason),
        retries_used => RetriesUsed,
        context => Context
    }}}.

%% Internal: Calculate latency in milliseconds
-spec calculate_latency(integer()) -> float().
calculate_latency(StartTime) ->
    (erlang:system_time(microsecond) - StartTime) / 1000.0.
    
%% Internal: Build request payload
build_request_payload(Request, Context) ->
    %% Extract trace context
    TraceId = maps:get(~"trace_id", Context, maps:get(~"trace_id", Request, undefined)),
    TenantId = maps:get(~"tenant_id", Context, maps:get(~"tenant_id", Request, undefined)),
    
    #{
        ~"trace_id" => TraceId,
        ~"tenant_id" => TenantId,
        ~"payload" => maps:get(~"payload", Request, #{}),
        ~"metadata" => maps:get(~"metadata", Request, #{})
    }.

%% Internal: Validate payload size (security: prevent DoS)
validate_payload_size(Payload, Extension) ->
    PayloadJson = ensure_json_binary(jsx:encode(Payload)),
    PayloadSize = byte_size(PayloadJson),
    
    %% Check per-extension limit first (if configured)
    ExtensionMaxSize = case Extension#extension.metadata of
        Metadata when is_map(Metadata) ->
            maps:get(~"max_payload_size", Metadata, undefined);
        _ ->
            undefined
    end,
    
    %% Use extension-specific limit or global limit
    MaxSize = case ExtensionMaxSize of
        S when is_integer(S), S > 0 -> S;
        _ ->
            application:get_env(beamline_router, extension_max_payload_size, 1048576)  % Default: 1MB
    end,
    
    case PayloadSize > MaxSize of
        true ->
            {error, {payload_too_large, #{
                extension_id => Extension#extension.id,
                size => PayloadSize,
                max_size => MaxSize
            }}};
        false ->
            ok
    end.

%% Internal: Validate metadata size (security: prevent DoS)
validate_metadata_size(Metadata, Extension) when is_map(Metadata) ->
    MetadataJson = ensure_json_binary(jsx:encode(Metadata)),
    MetadataSize = byte_size(MetadataJson),

    %% Check per-extension limit first (if configured)
    ExtensionMaxSize = case Extension#extension.metadata of
        ExtMeta when is_map(ExtMeta) ->
            maps:get(~"max_metadata_size", ExtMeta, undefined);
        _ ->
            undefined
    end,

    %% Use extension-specific limit or global limit
    MaxSize = case ExtensionMaxSize of
        S when is_integer(S), S > 0 -> S;
        _ ->
            application:get_env(beamline_router, extension_max_metadata_size, 65536)  % Default: 64KB
    end,

    case MetadataSize > MaxSize of
        true ->
            {error, {metadata_too_large, #{
                extension_id => Extension#extension.id,
                size => MetadataSize,
                max_size => MaxSize
            }}};
        false ->
            ok
    end;
validate_metadata_size(_, _Extension) ->
    %% Non-map metadata is allowed (empty or undefined)
    ok.

%% Internal: Validate timeout (security: prevent abuse)
validate_timeout(TimeoutMs) ->
    MinTimeout = application:get_env(beamline_router, extension_min_timeout_ms, 10),
    MaxTimeout = application:get_env(beamline_router, extension_max_timeout_ms, 5000),
    case TimeoutMs < MinTimeout orelse TimeoutMs > MaxTimeout of
        true ->
            {error, {timeout_out_of_range, #{
                timeout_ms => TimeoutMs,
                min_timeout_ms => MinTimeout,
                max_timeout_ms => MaxTimeout
            }}};
        false ->
            ok
    end.

%% Internal: Validate retry count (security: prevent abuse)
validate_retry_count(RetryCount) ->
    MaxRetries = application:get_env(beamline_router, extension_max_retries, 3),
    case RetryCount > MaxRetries of
        true ->
            {error, {retry_limit_exceeded, #{
                retry_count => RetryCount,
                max_retries => MaxRetries
            }}};
        false ->
            ok
    end.

%% Internal: Calculate exponential backoff
calculate_backoff(Attempt) ->
    %% Exponential backoff: 100ms, 200ms, 400ms, ...
    BaseMs = 100,
    trunc(BaseMs * math:pow(2, Attempt - 1)).

%% Internal: Emit telemetry with retries
emit_telemetry(Status, Extension, LatencyMs, RetriesUsed) ->
    ExtensionId = Extension#extension.id,
    ExtensionType = Extension#extension.type,
    ExtensionSubject = Extension#extension.subject,
    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [invocation_total], #{count => 1}, #{
        extension_id => ExtensionId,
        type => ExtensionType,
        subject => ExtensionSubject,
        status => Status,
        latency_ms => LatencyMs,
        retries_used => RetriesUsed
    }).

%% Internal: Emit unified telemetry with all required fields
emit_unified_telemetry(ExtensionId, ExtensionType, ExtensionSubject, Status, LatencyMs, RetriesUsed, TenantId, PolicyId) ->
    Metadata = #{
        extension_id => ExtensionId,
        type => ExtensionType,
        subject => ExtensionSubject,
        status => Status,
        latency_ms => LatencyMs,
        retries_used => RetriesUsed
    },
    %% Add optional correlation fields if available
    MetadataWithCorrelation = case TenantId of
        undefined -> Metadata;
        _ -> maps:put(tenant_id, TenantId, Metadata)
    end,
    FinalMetadata = case PolicyId of
        undefined -> MetadataWithCorrelation;
        _ -> maps:put(policy_id, PolicyId, MetadataWithCorrelation)
    end,
    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [invocation_total], #{count => 1}, FinalMetadata).

%% Internal: Log structured extension invocation event
log_extension_invocation(ExtensionId, ExtensionType, ExtensionSubject, Status, LatencyMs, RetriesUsed, TenantId, PolicyId, Result) ->
    %% Build context map
    Context = #{
        extension_id => ExtensionId,
        type => ExtensionType,
        subject => ExtensionSubject,
        status => Status,
        latency_ms => LatencyMs,
        retries_used => RetriesUsed
    },
    %% Add optional correlation fields
    ContextWithTenant = case TenantId of
        undefined -> Context;
        _ -> maps:put(~"tenant_id", TenantId, Context)
    end,
    ContextWithPolicy = case PolicyId of
        undefined -> ContextWithTenant;
        _ -> maps:put(~"policy_id", PolicyId, ContextWithTenant)
    end,
    %% Add error details if failed
    FinalContext = case Result of
        {error, ErrorInfo} when is_map(ErrorInfo) ->
            maps:put(~"error", ErrorInfo, ContextWithPolicy);
        {error, Reason} ->
            maps:put(~"error", #{reason => normalize_error(Reason)}, ContextWithPolicy);
        _ ->
            ContextWithPolicy
    end,
    %% Log based on status
    case Status of
        success ->
            router_logger:info(~"Extension invocation succeeded", FinalContext);
        timeout ->
            router_logger:warn(~"Extension invocation timeout", FinalContext);
        max_retries_exceeded ->
            router_logger:warn(~"Extension invocation max retries exceeded", FinalContext);
        error ->
            router_logger:error(~"Extension invocation failed", FinalContext);
        _ ->
            router_logger:info(~"Extension invocation completed", FinalContext)
    end.

%% Internal: Check circuit breaker (CP3)
check_circuit_breaker(ExtensionId) ->
    try
        ExtensionRegistryConfig = application:get_env(beamline_router, extension_registry, []),
        CircuitBreakerEnabled = proplists:get_value(circuit_breaker_enabled, ExtensionRegistryConfig, false),
        case CircuitBreakerEnabled of
            true ->
                router_extension_circuit_breaker:check_circuit(ExtensionId);
            false ->
                {ok, allow}
        end
    catch
        Error:Reason ->
            router_logger:debug(~"Circuit breaker check failed", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"circuit_breaker_check_failed"
            }),
            %% Circuit breaker not available, allow
            {ok, allow}
    end.

%% Internal: Update health metrics
update_health_metrics(ExtensionId, LatencyMs, _Status) ->
    try
        %% Update latency metrics (including percentile tracking)
        update_latency_metrics(ExtensionId, trunc(LatencyMs)),
        
        %% Update basic health data
        HealthData = #{
            last_latency_ms => trunc(LatencyMs)
        },
        router_extension_registry_db:update_health(ExtensionId, HealthData)
    catch
        Error:Reason ->
            router_logger:debug(~"Health metrics update failed", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"health_update_failed"
            })
    end.

%% Internal: Ensure JSON is binary
-spec ensure_json_binary(jsx:json_text() | {incomplete, jsx:encoder()}) -> binary().
ensure_json_binary(Bin) when is_binary(Bin) -> Bin;
ensure_json_binary(_) -> error(json_encoding_failed).

%% Internal: Update latency metrics (percentile tracking)
update_latency_metrics(ExtensionId, LatencyMs) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                %% Get current metrics
                Query = "SELECT latency_samples, latency_sum, p50_latency_ms, p95_latency_ms, p99_latency_ms, "
                        "latency_samples_window_start, updated_at "
                        "FROM extension_health WHERE extension_id = $1",
                case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
                    {ok, _, [{Samples, Sum, P50, P95, P99, WindowStart, _UpdatedAt}]} ->
                        %% Check if window expired (reset if > 5 minutes)
                        Now = calendar:universal_time(),
                        WindowExpired = case WindowStart of
                            null -> true;
                            _ ->
                                ElapsedSeconds = calendar:datetime_to_gregorian_seconds(Now) - 
                                                 calendar:datetime_to_gregorian_seconds(WindowStart),
                                ElapsedSeconds > 300  % 5 minutes window
                        end,
                        
                        {NewSamples, NewSum, NewP50, NewP95, NewP99} = case WindowExpired of
                            true ->
                                %% Reset window
                                {1, LatencyMs, LatencyMs, LatencyMs, LatencyMs};
                            false ->
                                %% Update running statistics
                                NewS = Samples + 1,
                                NewSumVal = Sum + LatencyMs,
                                NewAvg = NewSumVal / NewS,
                                
                                %% Simple percentile approximation (for production, use proper percentile tracking)
                                NewP50Val = case P50 of
                                    null -> LatencyMs;
                                    _ -> trunc((P50 + NewAvg) / 2)
                                end,
                                NewP95Val = case P95 of
                                    null -> LatencyMs;
                                    _ -> trunc((P95 * 0.95) + (LatencyMs * 0.05))
                                end,
                                NewP99Val = case P99 of
                                    null -> LatencyMs;
                                    _ -> trunc((P99 * 0.99) + (LatencyMs * 0.01))
                                end,
                                {NewS, NewSumVal, NewP50Val, NewP95Val, NewP99Val}
                        end,
                        
                        %% Update database
                        UpdateQuery = "UPDATE extension_health SET "
                                     "latency_samples = $2, "
                                     "latency_sum = $3, "
                                     "avg_latency_ms = $4, "
                                     "p50_latency_ms = $5, "
                                     "p95_latency_ms = $6, "
                                     "p99_latency_ms = $7, "
                                     "latency_samples_window_start = CASE WHEN $8 THEN CURRENT_TIMESTAMP ELSE latency_samples_window_start END, "
                                     "updated_at = CURRENT_TIMESTAMP "
                                     "WHERE extension_id = $1",
                        AvgLatency = NewSum / NewSamples,
                        catch apply(epgsql, equery, [Conn, UpdateQuery, [
                            ExtensionId, NewSamples, NewSum, AvgLatency,
                            NewP50, NewP95, NewP99, WindowExpired
                        ]]);
                    {ok, _, []} ->
                        create_health_record_with_latency(ExtensionId, LatencyMs),
                        ok;
                    {error, _} ->
                        ok
                end;
            _ ->
                ok
        end
    catch
        Error:Reason ->
            router_logger:debug(~"Latency metrics update failed", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"latency_metrics_update_failed"
            }),
            ok
    end.

%% Internal: Create health record with latency
create_health_record_with_latency(ExtensionId, LatencyMs) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "INSERT INTO extension_health (extension_id, latency_samples, latency_sum, "
                        "avg_latency_ms, p50_latency_ms, p95_latency_ms, p99_latency_ms, last_latency_ms, "
                        "latency_samples_window_start, circuit_breaker_state) "
                        "VALUES ($1, 1, $2, $2, $2, $2, $2, $2, CURRENT_TIMESTAMP, 'closed') "
                        "ON CONFLICT (extension_id) DO NOTHING",
                catch apply(epgsql, equery, [Conn, Query, [ExtensionId, LatencyMs]]);
            _ ->
                ok
        end
    catch
        Error:Reason ->
            router_logger:debug(~"Latency metrics update failed", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"latency_metrics_update_failed"
            }),
            ok
    end.

%% Internal: Normalize error to binary or map format
normalize_error({Reason, Metadata}) when is_map(Metadata) ->
    Reason;
normalize_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
normalize_error(Reason) when is_binary(Reason) ->
    Reason;
normalize_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
normalize_error(Reason) ->
    %% Convert to binary string
    list_to_binary(io_lib:format("~p", [Reason])).

%% Internal: Sanitize error for logging (masks secrets)
-spec sanitize_error_for_logging(term()) -> binary() | term().
sanitize_error_for_logging(Error) ->
    %% Convert error to binary for pattern matching
    ErrorBin = case is_binary(Error) of
        true -> Error;
        false -> iolist_to_binary(io_lib:format("~p", [Error]))
    end,
    %% Check for common secret patterns in error message
    case re:run(ErrorBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            ~"[REDACTED: contains sensitive data]";
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.
