-module(router_caf_adapter).

-doc "CAF Adapter".
%%
%% Publishes ExecAssignment messages to CAF via NATS (JSON format).
%% Enhanced with retries, tenant control, and improved telemetry.
%%
%% Integration:
%% - Receives route decisions from router_decider
%% - Publishes assignments to CAF via NATS subject (default: "caf.exec.assign.v1")
%% - Supports tenant allowlist for controlled rollout
%% - Implements retry logic with exponential backoff
%% - Emits telemetry events for observability
%%
%% @see INTEGRATION_GUIDE.md#caf-integration For CAF integration procedures and examples
%% @see src/router_decider.erl Source of route decisions
%% @see src/router_nats.erl NATS publishing implementation
-export([publish_assignment/2]).
-export([build_exec_assignment/3, generate_assignment_id/0, calculate_deadline/1, check_tenant_allowed/1]).  %% Exported for testing
-export([validate_assignment_request/1, validate_route_decision/1, get_assignment_status/1]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_caf_adapter]).
-define(DEFAULT_ASSIGNMENT_SUBJECT, ~"caf.exec.assign.v1").

%% OpenTelemetry span names
-define(SPAN_ROUTER_PUBLISH_ASSIGNMENT, ~"beamline.router.publish.assignment").
-define(DEFAULT_MAX_RETRIES, 3).
-define(DEFAULT_RETRY_BASE_MS, 100).
-define(DEFAULT_DEADLINE_MULTIPLIER, 5).
-define(DEFAULT_DEADLINE_MIN_MS, 5000).
-define(DEFAULT_DEADLINE_MAX_MS, 60000).

%% RequestMap: DecideRequest map (JSON decoded)
%% DecisionRec: #route_decision{} record
-spec publish_assignment(map(), #route_decision{}) -> ok | error.
publish_assignment(RequestMap, DecisionRec) ->
    %% Validate inputs
    case validate_assignment_request(RequestMap) of
        {error, Reason} ->
            router_logger:error(~"Invalid assignment request", #{
                ~"error" => sanitize_error_for_logging(Reason)
            }),
            error;
        {ok, _} ->
            case validate_route_decision(DecisionRec) of
                {error, Reason} ->
                    router_logger:error(~"Invalid route decision", #{
                        ~"error" => sanitize_error_for_logging(Reason)
                    }),
                    error;
                {ok, _} ->
                    do_publish_assignment_checked(RequestMap, DecisionRec)
            end
    end.

%% Internal: Perform publication after validation
-spec do_publish_assignment_checked(map(), #route_decision{}) -> ok | error.
do_publish_assignment_checked(RequestMap, DecisionRec) ->
    %% Check global enable flag (support both caf_push_assignment_enabled and assignment_enabled)
    AssignmentEnabled = case get_config(assignment_enabled, undefined) of
        undefined -> get_config(caf_push_assignment_enabled, true);
        Val -> Val
    end,
    case AssignmentEnabled of
        false ->
            emit_counter(router_assignment_skipped_total, #{
                reason => global_disabled
            }),
            ok;
        true ->
            %% Check tenant allowlist
            TenantId = maps:get(~"tenant_id", RequestMap, undefined),
            IsAllowed = check_tenant_allowed(TenantId),
            case IsAllowed of
                false ->
                    emit_counter(router_assignment_blocked_total, #{
                        tenant_id => TenantId,
                        reason => tenant_not_allowed
                    }),
                    log_blocked(TenantId),
                    ok;
                true ->
                    do_publish_assignment(RequestMap, DecisionRec, TenantId)
            end
    end.

%% Internal: Perform actual publication with retries
-spec do_publish_assignment(map(), #route_decision{}, binary() | undefined) -> ok | error.
do_publish_assignment(RequestMap, DecisionRec, TenantId) ->
    %% Get configuration
    Subject = get_assignment_subject(RequestMap),
    AssignmentId = generate_assignment_id(),
    RequestId = maps:get(~"request_id", RequestMap, undefined),
    ExpectedLatencyMs = DecisionRec#route_decision.expected_latency_ms,
    
    %% Calculate deadline for span metadata
    DeadlineMs = calculate_deadline(ExpectedLatencyMs),
    
    %% Extract trace_id from request and create parent context
    TraceId = maps:get(~"trace_id", RequestMap, undefined),
    ParentContext = case TraceId of
        undefined -> undefined;
        _ -> #{~"trace_id" => TraceId}
    end,
    
    %% Start OpenTelemetry span with trace context
    SpanAttributes = #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        tenant_id => TenantId,
        subject => Subject,
        expected_latency_ms => ExpectedLatencyMs,
        deadline_ms => DeadlineMs,
        trace_id => TraceId,
        provider_id => DecisionRec#route_decision.provider_id,
        priority => DecisionRec#route_decision.priority,
        expected_cost => DecisionRec#route_decision.expected_cost,
        reason => DecisionRec#route_decision.reason
    },
    
    Result = router_tracing:with_span(
        ?SPAN_ROUTER_PUBLISH_ASSIGNMENT,
        SpanAttributes,
        ParentContext,
        fun() ->
            try
                ExecAssignment = build_exec_assignment(AssignmentId, RequestMap, DecisionRec),
                Json = encode_json_binary(ExecAssignment),
                
                %% Log assignment publication
                log_publication(Subject, AssignmentId, RequestId),
                
                %% Publish with retries (headers will be built in publish_with_retries)
                PublishResult = publish_with_retries(Subject, Json, AssignmentId, RequestId, TenantId, RequestMap),
                
                %% Set span attributes based on result
                case PublishResult of
                    {ok, Retries} ->
                        %% PubAckId is not available here, it's in publish_with_retries
                        router_tracing:set_span_attribute(~"assignment.publish_result", ~"ok", string),
                        router_tracing:set_span_attribute(~"assignment.retries", Retries, integer),
                        router_tracing:set_span_status(ok, undefined);
                    {error, ErrorKind, Retries, Error} ->
                        %% Sanitize error before setting span attributes to prevent information disclosure
                        SanitizedError = sanitize_error_for_logging(Error),
                        router_tracing:set_span_attribute(~"assignment.publish_result", ~"error", string),
                        router_tracing:set_span_attribute(~"assignment.error_kind", erlang:atom_to_binary(ErrorKind, utf8), string),
                        router_tracing:set_span_attribute(~"assignment.retries", Retries, integer),
                        router_tracing:set_span_attribute(~"assignment.error", SanitizedError, string),
                        router_tracing:set_span_status(error, SanitizedError)
                end,
                
                PublishResult
            catch
                Class:Reason:Stack ->
                    ErrorKindVal = classify_exception(Class, Reason),
                    %% CP2+: Always sanitize error before logging to prevent information disclosure
                    SanitizedReason = sanitize_error_for_logging(Reason),
                    %% Log error with router_logger
                    router_logger:error(~"CAF adapter publish failed", #{
                        ~"assignment_id" => AssignmentId,
                        ~"request_id" => RequestId,
                        ~"tenant_id" => TenantId,
                        ~"subject" => Subject,
                        ~"error_kind" => ErrorKindVal,
                        ~"error" => Class,
                        ~"reason" => SanitizedReason,
                        ~"event" => ~"caf_publish_failed"
                    }),
                    router_tracing:set_span_attribute(~"error", erlang:atom_to_binary(Class, utf8), string),
                    router_tracing:set_span_attribute(~"error.reason", SanitizedReason, string),
                    router_tracing:set_span_status(error, SanitizedReason),
                    ErrorMetadata = #{
                        assignment_id => AssignmentId,
                        request_id => RequestId,
                        tenant_id => TenantId,
                        subject => Subject,
                        error_kind => ErrorKindVal,
                        error => Class,
                        reason => Reason
                    },
                    emit_counter(router_assignment_publish_failures_total, ErrorMetadata),
                    erlang:raise(Class, Reason, Stack)
            end
        end
    ),
    
    %% Return result
    case Result of
        {ok, _Retries} ->
            ok;
        {error, _ErrorKind, _Retries, Error} ->
            log_error(Subject, AssignmentId, Error),
            error
    end.

%% Internal: Publish with retry logic
-spec publish_with_retries(binary(), binary(), binary(), binary() | undefined, binary() | undefined, map()) ->
    {ok, non_neg_integer()} | {error, atom(), non_neg_integer(), term()}.
publish_with_retries(Subject, Json, AssignmentId, RequestId, TenantId, RequestMap) ->
    MaxRetries = ensure_integer(get_config(caf_max_retries, ?DEFAULT_MAX_RETRIES), ?DEFAULT_MAX_RETRIES),
    publish_with_retries(Subject, Json, AssignmentId, RequestId, TenantId, RequestMap, 0, MaxRetries).

-spec publish_with_retries(binary(), binary(), binary(), binary() | undefined, binary() | undefined, map(), non_neg_integer(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, atom(), non_neg_integer(), term()}.
publish_with_retries(_Subject, _Json, _AssignmentId, _RequestId, _TenantId, _RequestMap, Retries, MaxRetries) when Retries >= MaxRetries ->
    {error, max_retries_exceeded, Retries, retries_exhausted};
publish_with_retries(Subject, Json, AssignmentId, RequestId, TenantId, RequestMap, Retries, MaxRetries) ->
    %% Build headers from request context and inject trace context
    TraceId = maps:get(~"trace_id", RequestMap, undefined),
    Version = ~"1",
    BaseHeaders = case TraceId of
        undefined ->
            #{
                ~"tenant_id" => TenantId,
                ~"version" => Version
            };
        _ ->
            #{
                ~"trace_id" => TraceId,
                ~"tenant_id" => TenantId,
                ~"version" => Version
            }
    end,
    %% CP2+: Inject OpenTelemetry trace context (W3C Trace Context format) if tracing enabled
    TracingEnabled = application:get_env(beamline_router, tracing_enabled, false),
    Headers = case TracingEnabled of
        true ->
            ParentContext = case TraceId of
                undefined -> undefined;
                _ -> #{~"trace_id" => TraceId}
            end,
            router_tracing:inject_trace_context(BaseHeaders, ParentContext);
        false ->
            BaseHeaders  %% CP1 baseline: No trace context injection
    end,
    %% Use JetStream publish with acknowledgment and headers for guaranteed delivery
    case router_nats:publish_with_ack(Subject, Json, Headers) of
        {ok, PubAckId} ->
            case Retries > 0 of
                true ->
                    emit_counter(router_assignment_retry_total, #{
                        assignment_id => AssignmentId,
                        request_id => RequestId,
                        tenant_id => TenantId,
                        subject => Subject,
                        retries => Retries
                    });
                false ->
                    ok
            end,
            emit_counter(router_assignment_published_total, #{
                assignment_id => AssignmentId,
                request_id => RequestId,
                tenant_id => TenantId,
                subject => Subject,
                retries => Retries,
                pub_ack_id => PubAckId
            }),
            {ok, Retries};
        {error, Reason} = Error ->
            ErrorKind = classify_nats_error(Reason),
            NextRetry = Retries + 1,
            
            case NextRetry < MaxRetries of
                true ->
                    %% Calculate exponential backoff with jitter
                    BackoffMs = calculate_backoff(NextRetry),
                    timer:sleep(BackoffMs),
                    publish_with_retries(Subject, Json, AssignmentId, RequestId, TenantId, RequestMap, NextRetry, MaxRetries);
                false ->
                    %% Emit retry exhausted metric
                    emit_counter(router_retry_exhausted_total, #{
                        assignment_id => AssignmentId,
                        request_id => RequestId,
                        tenant_id => TenantId,
                        subject => Subject,
                        error_kind => ErrorKind,
                        error => Error,
                        retries => NextRetry
                    }),
                    emit_counter(router_degraded_mode_active_total, #{
                        assignment_id => AssignmentId,
                        request_id => RequestId,
                        tenant_id => TenantId,
                        subject => Subject,
                        error_kind => ErrorKind,
                        retries => NextRetry
                    }),
                    %% Also emit publish failure metric for consistency
                    emit_counter(router_assignment_publish_failures_total, #{
                        assignment_id => AssignmentId,
                        request_id => RequestId,
                        tenant_id => TenantId,
                        subject => Subject,
                        error_kind => ErrorKind,
                        error => Error,
                        retries => NextRetry
                    }),
                    {error, ErrorKind, NextRetry, Error}
            end
    end.

%% Internal: Classify NATS error
-spec classify_nats_error(term()) -> atom().
classify_nats_error(timeout) -> timeout;
classify_nats_error({timeout, _}) -> timeout;
classify_nats_error(connection_failed) -> connection_failed;
classify_nats_error({connection_failed, _}) -> connection_failed;
classify_nats_error(nats_unavailable) -> nats_unavailable;
classify_nats_error({nats_unavailable, _}) -> nats_unavailable;
classify_nats_error(invalid_format) -> invalid_format;
classify_nats_error({invalid_format, _}) -> invalid_format;
classify_nats_error(_) -> unknown_error.

%% Internal: Classify exception
-spec classify_exception(atom(), term()) -> atom().
classify_exception(error, {badarg, _}) -> bad_argument;
classify_exception(error, {badmatch, _}) -> bad_match;
classify_exception(error, {function_clause, _}) -> function_clause;
classify_exception(throw, _) -> throw_exception;
classify_exception(exit, _) -> exit_exception;
classify_exception(_, _) -> unknown_exception.

%% Internal: Calculate exponential backoff with jitter
%% Called from publish_with_retries/7 when retry is needed
-spec calculate_backoff(non_neg_integer()) -> non_neg_integer().
calculate_backoff(Retry) ->
    BaseMs = ensure_integer(get_config(caf_retry_base_ms, ?DEFAULT_RETRY_BASE_MS), ?DEFAULT_RETRY_BASE_MS),
    Exponential = BaseMs * trunc(math:pow(2, Retry)),
    JitterMax = trunc(Exponential * 0.1),
    Jitter = case JitterMax > 0 of
        true -> rand:uniform(JitterMax) - 1;
        false -> 0
    end,
    Exponential + Jitter.

%% Internal: Check if tenant is allowed
-spec check_tenant_allowed(binary() | undefined) -> boolean().
check_tenant_allowed(undefined) ->
    %% If no tenant allowlist configured, allow all
    case get_config(caf_push_assignment_allowed_tenants, undefined) of
        undefined -> true;
        _ -> false  %% Require tenant_id if allowlist is configured
    end;
check_tenant_allowed(TenantId) ->
    case get_config(caf_push_assignment_allowed_tenants, undefined) of
        undefined -> true;  %% No allowlist = allow all
        AllowedTenants when is_list(AllowedTenants) ->
            %% Support both binary and string tenant IDs
            %% Normalize TenantId to binary for comparison
            TenantIdBin = ensure_binary(TenantId, TenantId),
            %% Check if any element in allowlist matches (supporting both binary and string)
            lists:any(fun(Allowed) ->
                case Allowed of
                    AllowedBin when is_binary(AllowedBin) -> AllowedBin =:= TenantIdBin;
                    %% Non-binary allowlist entries are not supported for safety
                    _List when is_list(_List) -> false;
                    _ -> false
                end
            end, AllowedTenants);
        AllowedTenants when is_map(AllowedTenants) ->
            %% Normalize TenantId to binary for map key lookup
            TenantIdBin = ensure_binary(TenantId, TenantId),
            maps:is_key(TenantIdBin, AllowedTenants) orelse
            (is_list(TenantId) andalso maps:is_key(list_to_binary(TenantId), AllowedTenants));
        _ -> false
    end.

%% Internal: Get assignment subject from config or request
-spec get_assignment_subject(map()) -> binary().
get_assignment_subject(RequestMap) ->
    case maps:get(~"assignment_subject", RequestMap, undefined) of
        undefined ->
            %% Support both caf_assignment_subject and assignment_subject
            ensure_binary_default(
                case get_config(assignment_subject, undefined) of
                    undefined -> get_config(caf_assignment_subject, ?DEFAULT_ASSIGNMENT_SUBJECT);
                    Val -> Val
                end,
                ?DEFAULT_ASSIGNMENT_SUBJECT
            );
        Subject when is_binary(Subject) ->
            Subject;
        Subject when is_list(Subject) ->
            list_to_binary(Subject);
        _ ->
            ?DEFAULT_ASSIGNMENT_SUBJECT
    end.

%% Internal: Get configuration value
-spec get_config(atom(), term()) -> term().
get_config(Key, Default) ->
    application:get_env(beamline_router, Key, Default).

-spec encode_json_binary(jsx:json_term()) -> binary().
encode_json_binary(Term) ->
    Encoded = jsx:encode(Term),
    case Encoded of
        Bin when is_binary(Bin) -> Bin;
        List when is_list(List) -> iolist_to_binary(List);
        {incomplete, _Encoder} -> erlang:error(badarg)
    end.

-spec ensure_integer(term(), integer()) -> integer().
ensure_integer(Value, Default) ->
    case Value of
        V when is_integer(V) -> V;
        F when is_float(F) -> trunc(F);
        _ -> Default
    end.

-spec ensure_binary(term(), binary() | undefined) -> binary() | undefined.
ensure_binary(Value, Default) ->
    case Value of
        Bin when is_binary(Bin) -> Bin;
        _ -> Default
    end.

-spec ensure_binary_default(term(), binary()) -> binary().
ensure_binary_default(Value, Default) ->
    case Value of
        Bin when is_binary(Bin) -> Bin;
        _ -> Default
    end.

%% Internal: Emit telemetry counter
-spec emit_counter(atom(), map()) -> ok.
emit_counter(CounterName, Metadata) ->
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [CounterName],
        #{count => 1},
        Metadata
    ).

%% Internal: Log publication
-spec log_publication(binary(), binary(), binary() | undefined) -> ok.
log_publication(Subject, AssignmentId, RequestId) ->
    router_logger:info(~"Publishing ExecAssignment to CAF", #{
        ~"subject" => Subject,
        ~"assignment_id" => AssignmentId,
        ~"request_id" => RequestId
    }).

%% Internal: Log error
-spec log_error(binary(), binary(), term()) -> ok.
log_error(Subject, AssignmentId, Error) ->
    router_logger:error(~"Failed to publish ExecAssignment", #{
        ~"error" => sanitize_error_for_logging(Error),
        ~"subject" => Subject,
        ~"assignment_id" => AssignmentId
    }).

%% Internal: Log blocked tenant
-spec log_blocked(binary() | undefined) -> ok.
log_blocked(TenantId) ->
    router_logger:warn(~"Assignment blocked: tenant not allowed", #{
        ~"tenant_id" => TenantId
    }).

%% Internal: Build ExecAssignment JSON map
-spec build_exec_assignment(binary(), map(), #route_decision{}) -> map().
build_exec_assignment(AssignmentId, Request, #route_decision{
    provider_id = ProviderId,
    priority = Priority,
    expected_latency_ms = Latency,
    expected_cost = Cost,
    reason = Reason,
    metadata = DecisionMeta
}) ->
    RequestId = maps:get(~"request_id", Request, undefined),
    TraceId = maps:get(~"trace_id", Request, undefined),
    Task = maps:get(~"task", Request, #{}),
    Options = maps:get(~"options", Request, #{}),
    RequestMetadata = maps:get(~"metadata", Request, #{}),
    
    %% Build executor info
    Executor = #{
        ~"provider_id" => ProviderId,
        ~"channel" => ~"nats"
    },
    
    %% Build job info (copy from task)
    Job = Task,
    
    %% Calculate deadline with min/max caps
    DeadlineMs = calculate_deadline(Latency),
    
    %% Build options (merge with defaults)
    OptionsWithDefaults = maps:merge(#{
        ~"priority" => Priority,
        ~"deadline_ms" => DeadlineMs,
        ~"retry" => #{
            ~"max_attempts" => 2,
            ~"backoff_ms" => 200
        }
    }, Options),
    
    %% Build correlation
    Correlation = case TraceId of
        undefined -> #{};
        _ -> #{~"trace_id" => TraceId}
    end,
    
    %% Build decision context (key fields from decision)
    DecisionContext = #{
        ~"provider_id" => ProviderId,
        ~"priority" => Priority,
        ~"expected_latency_ms" => Latency,
        ~"expected_cost" => Cost,
        ~"reason" => Reason
    },
    
    %% Add metadata if present
    DecisionContextWithMeta = case DecisionMeta of
        #{} when map_size(DecisionMeta) > 0 ->
            maps:put(~"metadata", DecisionMeta, DecisionContext);
        _ ->
            DecisionContext
    end,
    
    %% Build base assignment map
    Assignment = #{
        ~"version" => ~"1",
        ~"assignment_id" => AssignmentId,
        ~"request_id" => RequestId,
        ~"executor" => Executor,
        ~"job" => Job,
        ~"options" => OptionsWithDefaults,
        ~"correlation" => Correlation,
        ~"decision" => DecisionContextWithMeta,
        ~"metadata" => RequestMetadata
    },
    
    %% Add tenant_id if present in request
    TenantId = maps:get(~"tenant_id", Request, undefined),
    AssignmentWithTenant = case TenantId of
        undefined -> Assignment;
        _ -> maps:put(~"tenant_id", TenantId, Assignment)
    end,
    
    %% Store correlation context (assignment_id/request_id mapping)
    case RequestId of
        undefined -> AssignmentWithTenant;
        _ ->
            CorrelationContext = #{
                ~"assignment_id" => AssignmentId,
                ~"request_id" => RequestId,
                ~"trace_id" => TraceId,
                ~"tenant_id" => TenantId,
                ~"provider_id" => ProviderId,
                ~"priority" => Priority,
                ~"expected_latency_ms" => Latency,
                ~"expected_cost" => Cost,
                ~"reason" => Reason,
                ~"created_at" => erlang:system_time(millisecond)
            },
            router_correlation_context:store(AssignmentId, RequestId, CorrelationContext),
            AssignmentWithTenant
    end.

%% Internal: Generate assignment ID using unified UUID generator
-spec generate_assignment_id() -> binary().
generate_assignment_id() ->
    %% Use unified UUID generator
    router_uuid:generate_v4().

%% Internal: Calculate deadline based on expected latency with min/max caps
-spec calculate_deadline(non_neg_integer()) -> non_neg_integer().
calculate_deadline(ExpectedLatencyMs) ->
    Multiplier = ensure_integer(get_config(caf_deadline_multiplier, ?DEFAULT_DEADLINE_MULTIPLIER), ?DEFAULT_DEADLINE_MULTIPLIER),
    MinMs = ensure_integer(get_config(caf_deadline_min_ms, ?DEFAULT_DEADLINE_MIN_MS), ?DEFAULT_DEADLINE_MIN_MS),
    MaxMs = ensure_integer(get_config(caf_deadline_max_ms, ?DEFAULT_DEADLINE_MAX_MS), ?DEFAULT_DEADLINE_MAX_MS),
    
    Calculated = max(MinMs, ExpectedLatencyMs * Multiplier),
    Capped = min(MaxMs, Calculated),
    
    case (ExpectedLatencyMs > 0) andalso (Capped > ExpectedLatencyMs * 10) of
        true -> log_deadline_exceeded(ExpectedLatencyMs, Capped);
        false -> ok
    end,
    
    Capped.

%% Internal: Format error term to binary
-spec format_error_term(term()) -> binary().
format_error_term(Error) when is_binary(Error) ->
    Error;
format_error_term(Error) when is_atom(Error) ->
    erlang:atom_to_binary(Error, utf8);
format_error_term(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

%% Internal: Sanitize error for logging (masks secrets)
-spec sanitize_error_for_logging(term()) -> binary().
sanitize_error_for_logging(Term) ->
    Formatted = format_error_term(Term),
    %% Check for common secret patterns
    case re:run(Formatted, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            ~"[REDACTED: contains sensitive data]";
        nomatch ->
            Formatted
    end.

%% Internal: Log deadline exceeded
-spec log_deadline_exceeded(non_neg_integer(), non_neg_integer()) -> ok.
log_deadline_exceeded(ExpectedLatencyMs, DeadlineMs) ->
    Ratio = case ExpectedLatencyMs > 0 of
        true -> DeadlineMs / ExpectedLatencyMs;
        false -> 0
    end,
    router_logger:warn(~"Deadline significantly exceeds expected latency", #{
        ~"expected_latency_ms" => ExpectedLatencyMs,
        ~"deadline_ms" => DeadlineMs,
        ~"ratio" => Ratio
    }).

%% ============================================================================
%% Validation and Status Functions
%% ============================================================================

%% Returns {ok, RequestMap} or {error, Reason}
-spec validate_assignment_request(map()) -> {ok, map()} | {error, term()}.
validate_assignment_request(RequestMap) when is_map(RequestMap) ->
    %% Check required fields
    RequiredFields = [~"tenant_id"],
    MissingFields = lists:filter(fun(Field) ->
        not maps:is_key(Field, RequestMap)
    end, RequiredFields),
    
    case MissingFields of
        [] ->
            %% Validate tenant_id format
            TenantId = maps:get(~"tenant_id", RequestMap),
            case is_valid_tenant_id(TenantId) of
                true ->
                    {ok, RequestMap};
                false ->
                    {error, invalid_tenant_id}
            end;
        _ ->
            {error, {missing_fields, MissingFields}}
    end;
validate_assignment_request(_) ->
    {error, invalid_request_format}.

%% Returns {ok, #route_decision{}} or {error, Reason}
-spec validate_route_decision(#route_decision{}) -> {ok, #route_decision{}} | {error, term()}.
validate_route_decision(Decision) when is_record(Decision, route_decision) ->
    %% Validate provider_id
    ProviderId0 = Decision#route_decision.provider_id,
    ProviderId = ensure_binary(ProviderId0, undefined),
    case is_valid_provider_id(ProviderId) of
        true ->
            %% Validate expected_latency_ms
            Latency = Decision#route_decision.expected_latency_ms,
            case is_valid_latency(Latency) of
                true ->
                    {ok, Decision};
                false ->
                    {error, invalid_latency}
            end;
        false ->
            {error, invalid_provider_id}
    end;
validate_route_decision(_) ->
    {error, invalid_decision_format}.

%% Returns status map with assignment information
-spec get_assignment_status(binary()) -> map() | {error, not_found}.
get_assignment_status(AssignmentId) when is_binary(AssignmentId) ->
    %% Check correlation context for assignment tracking
    case (erlang:function_exported(router_correlation_context, get_by_assignment_id, 1) andalso
          apply(router_correlation_context, get_by_assignment_id, [AssignmentId])) of
        {ok, Context} ->
            #{
                assignment_id => AssignmentId,
                request_id => maps:get(~"request_id", Context, undefined),
                tenant_id => maps:get(~"tenant_id", Context, undefined),
                provider_id => maps:get(~"provider_id", Context, undefined),
                status => ~"published",
                created_at => maps:get(~"created_at", Context, undefined)
            };
        {error, not_found} ->
            {error, not_found}
    end.

%% Helper: Check if tenant_id is valid
-spec is_valid_tenant_id(binary() | undefined) -> boolean().
is_valid_tenant_id(undefined) -> false;
is_valid_tenant_id(TenantId) when is_binary(TenantId) ->
    Size = byte_size(TenantId),
    Size > 0 andalso Size =< 256;
is_valid_tenant_id(_) -> false.

%% Helper: Check if provider_id is valid
-spec is_valid_provider_id(binary() | undefined) -> boolean().
is_valid_provider_id(undefined) -> false;
is_valid_provider_id(ProviderId) when is_binary(ProviderId) ->
    Size = byte_size(ProviderId),
    Size > 0 andalso Size =< 128;
is_valid_provider_id(_) -> false.

%% Helper: Check if latency is valid
-spec is_valid_latency(non_neg_integer() | undefined) -> boolean().
is_valid_latency(undefined) -> false;
is_valid_latency(Latency) when is_integer(Latency) ->
    Latency >= 0 andalso Latency =< 3600000;  %% Max 1 hour
is_valid_latency(_) -> false.
