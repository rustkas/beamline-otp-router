%% @doc Result Consumer
%% Subscribes to caf.exec.result.v1, parses ExecResult, correlates by assignment_id/request_id,
%% and emits beamline.usage.v1.metered events
-module(router_result_consumer).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([validate_nats_headers/3]).  %% Exported for testing
-export([process_validated_result/12]).  %% Exported for testing (CB integration tests)
-export([check_maxdeliver_exhaustion/4, cleanup_delivery_count/1, track_delivery_count/1]).

-ignore_xref([
    {router_result_consumer, start_link, 0},
    {router_result_consumer, validate_nats_headers, 3},
    {router_result_consumer, process_validated_result, 12}
]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_result_consumer]).
-define(DEFAULT_RESULT_SUBJECT, <<"caf.exec.result.v1">>).
-define(DEFAULT_USAGE_SUBJECT, <<"beamline.usage.v1.metered">>).
-define(DEFAULT_JS_DURABLE_GROUP, <<"router-results">>).

%% OpenTelemetry span names
-define(SPAN_ROUTER_PROCESS_RESULT, <<"beamline.router.process.result">>).
-define(SPAN_ROUTER_EMIT_USAGE, <<"beamline.router.emit.usage">>).

-record(state, {
    connection :: pid() | undefined,
    result_subject :: binary(),
    usage_subject :: binary(),
    js_durable_group :: binary(),
    correlation_map :: #{binary() => map()}  %% assignment_id/request_id -> correlation context
}).

%% ETS table for tracking delivery count per message (for MaxDeliver exhaustion detection)
-define(DELIVERY_COUNT_TABLE, router_delivery_count).

%% @doc Start result consumer
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize consumer
-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    %% Get configuration
    ResultSubject = get_config(result_subject, ?DEFAULT_RESULT_SUBJECT),
    UsageSubject = get_config(usage_subject, ?DEFAULT_USAGE_SUBJECT),
    JSDurableGroup = get_config(nats_js_durable_group_results, ?DEFAULT_JS_DURABLE_GROUP),
    
    %% Create ETS table for delivery count tracking (use named_table for global access)
    _DeliveryTable = ets:new(?DELIVERY_COUNT_TABLE, [
        set,
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    %% Log ETS initialization
    case erlang:function_exported(router_logger, info, 2) of
        true ->
            router_logger:info(<<"Delivery count ETS initialized">>, #{
                <<"table">> => atom_to_binary(?DELIVERY_COUNT_TABLE, utf8),
                <<"options">> => [
                    <<"set">>,
                    <<"named_table">>,
                    <<"public">>,
                    <<"write_concurrency:true">>,
                    <<"read_concurrency:true">>
                ]
            });
        false -> ok
    end,
    
    %% Subscribe to result subject (JetStream durable queue)
    case subscribe_to_results(ResultSubject, JSDurableGroup) of
        ok ->
            {ok, #state{
                connection = undefined,
                result_subject = ResultSubject,
                usage_subject = UsageSubject,
                js_durable_group = JSDurableGroup,
                correlation_map = #{}
            }};
        Error ->
            ets:delete(?DELIVERY_COUNT_TABLE),
            {stop, Error}
    end.

%% @doc Handle calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handle casts
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
-spec handle_info({nats_message, binary(), binary()} | {nats_message, binary(), binary(), map()} | {nats_message, binary(), binary(), map(), binary() | undefined} | {nats_message, binary(), binary(), binary() | undefined, map(), binary() | undefined}, #state{}) -> {noreply, #state{}}.
handle_info({nats_message, Subject, Payload}, State) ->
    %% Backward compatibility: no headers, no msg_id
    handle_result_message(Subject, Payload, #{}, undefined),
    {noreply, State};
handle_info({nats_message, Subject, Payload, Headers}, State) ->
    %% Backward compatibility: headers but no msg_id
    handle_result_message(Subject, Payload, Headers, undefined),
    {noreply, State};
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    %% Track delivery count for MaxDeliver exhaustion detection
    track_delivery_count(MsgId),
    handle_result_message(Subject, Payload, Headers, MsgId),
    {noreply, State};
%% New format with ReplyTo (ignored for this subscriber)
handle_info({nats_message, Subject, Payload, _ReplyTo, Headers, MsgId}, State) ->
    %% Track delivery count for MaxDeliver exhaustion detection
    track_delivery_count(MsgId),
    handle_result_message(Subject, Payload, Headers, MsgId),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Internal: Subscribe to result subject (JetStream durable)
-spec subscribe_to_results(binary(), binary()) -> ok | {error, term()}.
subscribe_to_results(Subject, DurableGroup) ->
    %% Use router_jetstream wrapper for JetStream durable subscription
    %% Get DeliverGroup from config for horizontal scaling (queue group)
    DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_results, undefined),
    case router_jetstream:subscribe_results(#{
        subject => Subject,
        durable_group => DurableGroup,
        deliver_group => DeliverGroup,
        ack_policy => explicit,
        mode => push
    }) of
        {ok, _ConsumerId} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal: Extract header or payload value (headers have priority)
-spec extract_header_or_payload(map(), map(), binary(), binary()) -> binary() | undefined.
extract_header_or_payload(Headers, Payload, HeaderKey, PayloadKey) ->
    %% Try headers first (priority), then payload (fallback)
    case maps:get(HeaderKey, Headers, undefined) of
        undefined ->
            maps:get(PayloadKey, Payload, undefined);
        HeaderValue ->
            HeaderValue
    end.

%% Internal: Validate NATS headers against contract
%% Validates headers according to docs/NATS_SUBJECTS.md and docs/API_CONTRACTS.md
%% Returns: {ok, Violations} where Violations is a list of violation descriptions
%% Violations are logged and metrics emitted, but processing continues (backward compat)
-spec validate_nats_headers(binary(), map(), binary() | undefined) -> {ok, [binary()]}.
validate_nats_headers(Subject, Headers, MsgId) ->
    Violations = [],
    
    %% Only validate if headers are present (backward compat: empty headers = CP1 baseline)
    Violations1 = case map_size(Headers) > 0 of
        true ->
            %% Headers present - validate according to contract
            ViolationsTraceId = validate_trace_id_header(Headers),
            ViolationsTenantId = validate_tenant_id_header(Headers),
            ViolationsVersion = validate_version_header(Headers),
            ViolationsMsgId = validate_msg_id_header(Headers, MsgId),
            Violations ++ ViolationsTraceId ++ ViolationsTenantId ++ ViolationsVersion ++ ViolationsMsgId;
        false ->
            %% No headers - CP1 baseline, no validation needed
            Violations
    end,
    
    %% Log violations and emit metrics if any
    case Violations1 of
        [] ->
            ok;
        _ ->
            log_contract_violations(Subject, Headers, MsgId, Violations1),
            emit_counter(router_nats_contract_violations_total, #{
                subject => Subject,
                violation_count => length(Violations1),
                msg_id => MsgId
            })
    end,
    
    {ok, Violations1}.

%% Internal: Validate trace_id header
-spec validate_trace_id_header(map()) -> [binary()].
validate_trace_id_header(Headers) ->
    case maps:get(<<"trace_id">>, Headers, undefined) of
        undefined ->
            %% trace_id is optional - no violation
            [];
        TraceId when is_binary(TraceId) ->
            %% Valid binary - check if empty
            case TraceId =:= <<>> of
                true -> [<<"trace_id header is empty">>];
                false -> []
            end;
        _ ->
            %% Invalid type (not binary)
            [<<"trace_id header must be binary">>]
    end.

%% Internal: Validate tenant_id header
-spec validate_tenant_id_header(map()) -> [binary()].
validate_tenant_id_header(Headers) ->
    case maps:get(<<"tenant_id">>, Headers, undefined) of
        undefined ->
            %% tenant_id is optional - no violation
            [];
        TenantId when is_binary(TenantId) ->
            %% Valid binary - check if empty
            case TenantId =:= <<>> of
                true -> [<<"tenant_id header is empty">>];
                false -> []
            end;
        _ ->
            %% Invalid type (not binary)
            [<<"tenant_id header must be binary">>]
    end.

%% Internal: Validate version header
-spec validate_version_header(map()) -> [binary()].
validate_version_header(Headers) ->
    case maps:get(<<"version">>, Headers, undefined) of
        undefined ->
            %% version is optional - no violation
            [];
        <<"1">> ->
            %% Valid version
            [];
        Version when is_binary(Version) ->
            %% Invalid version value
            [<<"version header must be \"1\"">>];
        _ ->
            %% Invalid type (not binary)
            [<<"version header must be binary">>]
    end.

%% Internal: Validate Nats-Msg-Id header (required for JetStream ACK/NAK)
-spec validate_msg_id_header(map(), binary() | undefined) -> [binary()].
validate_msg_id_header(Headers, MsgId) ->
    %% Nats-Msg-Id is required for JetStream messages (for ACK/NAK operations)
    %% Check both "Nats-Msg-Id" and "nats-msg-id" (case-insensitive)
    HeaderMsgId = case maps:get(<<"nats-msg-id">>, Headers, undefined) of
        undefined -> maps:get(<<"Nats-Msg-Id">>, Headers, undefined);
        Value -> Value
    end,
    
    case {HeaderMsgId, MsgId} of
        {undefined, undefined} ->
            %% No msg_id in headers or message - violation for JetStream
            [<<"Nats-Msg-Id header missing (required for JetStream ACK/NAK)">>];
        {undefined, _} ->
            %% msg_id in message but not in headers - acceptable fallback
            [];
        {HeaderMsgIdValue, _} when is_binary(HeaderMsgIdValue) ->
            %% Valid binary - check if empty
            case HeaderMsgIdValue =:= <<>> of
                true -> [<<"Nats-Msg-Id header is empty">>];
                false -> []
            end;
        _ ->
            %% Invalid type (not binary)
            [<<"Nats-Msg-Id header must be binary">>]
    end.

%% Internal: Log contract violations
-spec log_contract_violations(binary(), map(), binary() | undefined, [binary()]) -> ok.
log_contract_violations(Subject, Headers, MsgId, Violations) ->
    router_logger:warn(<<"NATS contract violation detected">>, #{
        <<"contract_violation">> => true,
        <<"subject">> => Subject,
        <<"msg_id">> => MsgId,
        <<"violations">> => Violations,
        <<"header_keys">> => maps:keys(Headers),
        <<"header_count">> => map_size(Headers)
    }).

%% Internal: Handle result message (callback from NATS subscription)
-spec handle_result_message(binary(), binary(), map(), binary() | undefined) -> ok.
handle_result_message(Subject, Payload, Headers, MsgId) ->
    handle_result_message_internal(Subject, Payload, Headers, MsgId).

-spec handle_result_message_internal(binary(), binary(), map(), binary() | undefined) -> ok.
handle_result_message_internal(Subject, Payload, Headers, MsgId) ->
    try
        %% Use unified intake validator (CP2+)
        case router_intake_validator:validate_intake_message(Subject, Payload, Headers, result) of
            {ok, ValidatedMessage} ->
                %% All validations passed - process result
                ResultMap = ValidatedMessage,
                {ok, _Violations} = validate_nats_headers(Subject, Headers, MsgId),
                TraceId = extract_header_or_payload(Headers, ResultMap, <<"trace_id">>, <<"trace_id">>),
                TenantId = extract_header_or_payload(Headers, ResultMap, <<"tenant_id">>, <<"tenant_id">>),
                Version = extract_header_or_payload(Headers, ResultMap, <<"version">>, <<"version">>),
                TracingEnabled = application:get_env(beamline_router, tracing_enabled, false),
                ParentContext = case TracingEnabled andalso TraceId =/= undefined of
                    true -> #{<<"trace_id">> => TraceId};
                    false -> undefined
                end,
                case TracingEnabled of
                    true ->
                        router_tracing:with_span(
                            ?SPAN_ROUTER_PROCESS_RESULT,
                            #{
                                subject => Subject,
                                assignment_id => maps:get(<<"assignment_id">>, ResultMap, undefined),
                                request_id => maps:get(<<"request_id">>, ResultMap, undefined),
                                trace_id => TraceId,
                                tenant_id => TenantId,
                                version => Version,
                                status => maps:get(<<"status">>, ResultMap, undefined),
                                provider_id => maps:get(<<"provider_id">>, ResultMap, undefined),
                                job_type => get_job_type(ResultMap),
                                latency_ms => maps:get(<<"latency_ms">>, ResultMap, undefined),
                                cost => maps:get(<<"cost">>, ResultMap, undefined),
                                msg_id => MsgId
                            },
                            ParentContext,
                            fun() ->
                                process_exec_result(ResultMap, Headers, TraceId, TenantId, Version, MsgId)
                            end
                        );
                    false ->
                        process_exec_result(ResultMap, Headers, TraceId, TenantId, Version, MsgId)
                end;
            {error, {ErrorCode, ErrorMessage, ErrorContext}} ->
                %% Validation failed - handle error
                FullContext = maps:merge(ErrorContext, #{
                    <<"msg_id">> => MsgId
                }),
                router_intake_error_handler:handle_intake_error(
                    ErrorCode, ErrorMessage, Subject, Payload, Headers, MsgId, FullContext
                )
        end
    catch
        Class:Exception:_Stack ->
            %% Log error with router_logger before handling via error handler
            SanitizedException = sanitize_error_for_logging(Exception),
            router_logger:error(<<"Result consumer validation failed with exception">>, #{
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId,
                <<"error">> => Class,
                <<"reason">> => SanitizedException,
                <<"event">> => <<"result_consumer_validation_exception">>
            }),
            %% Internal error - handle via error handler
            CatchErrorCode = internal_validation_error,
            CatchErrorMessage = router_intake_error_codes:error_code_message(CatchErrorCode, #{
                <<"reason">> => SanitizedException
            }),
            Context = #{
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId,
                <<"exception">> => Exception
            },
            router_intake_error_handler:handle_intake_error(
                CatchErrorCode, CatchErrorMessage, Subject, Payload, Headers, MsgId, Context
            )
    end.

%% Internal: Process validated result (after tenant validation)
-spec process_validated_result(map(), binary(), binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined, integer() | undefined, float() | undefined, binary() | undefined, integer(), binary() | undefined) -> ok.
process_validated_result(Result, ValidatedTenantId, AssignmentId, RequestId, Status, JobType, ProviderId, LatencyMs, Cost, TraceId, Timestamp, MsgId) ->
    %% Get usage subject from config
    UsageSubject = get_config(usage_subject, ?DEFAULT_USAGE_SUBJECT),
    
    %% Emit telemetry
    emit_result_received(Result, Status, JobType, ProviderId, LatencyMs, Cost),
    
    %% Check idempotency for usage emission
    UsageId = <<"usage-", (case AssignmentId of undefined -> RequestId; _ -> AssignmentId end)/binary>>,
    case router_idempotency:check_and_mark(<<"usage_id">>, UsageId, #{
        tenant_id => ValidatedTenantId,
        provider_id => ProviderId,
        status => Status
    }) of
        {ok, seen} ->
            %% Usage already emitted - skip
            router_logger:info(<<"Usage event already emitted (idempotency)">>, #{
                <<"assignment_id">> => AssignmentId,
                <<"request_id">> => RequestId
            });
        {ok, not_seen} ->
            %% Not seen before - emit usage event
            TraceContext = case TraceId of
                undefined -> undefined;
                _ -> #{<<"trace_id">> => TraceId}
            end,
            router_tracing:with_span(
                ?SPAN_ROUTER_EMIT_USAGE,
                #{
                    tenant_id => ValidatedTenantId,
                    provider_id => ProviderId,
                    event_type => JobType,
                    status => Status,
                    trace_id => TraceId,
                    assignment_id => AssignmentId,
                    request_id => RequestId,
                    usage_subject => UsageSubject,
                    latency_ms => LatencyMs,
                    cost => Cost
                },
                TraceContext,
                fun() ->
                    case emit_usage_event(UsageSubject, #{
                        tenant_id => ValidatedTenantId,
                        provider_id => ProviderId,
                        event_type => JobType,
                        latency_ms => LatencyMs,
                        cost => Cost,
                        status => Status,
                        trace_id => TraceId,
                        timestamp => Timestamp,
                        assignment_id => AssignmentId,
                        request_id => RequestId
                    }) of
                        ok ->
                            router_tracing:set_span_attribute(<<"usage.emit_result">>, <<"ok">>, string),
                            router_tracing:set_span_status(ok, undefined);
                        {error, Error} ->
                            router_tracing:set_span_attribute(<<"usage.emit_result">>, <<"error">>, string),
                            router_tracing:set_span_attribute(<<"usage.emit_error">>, erlang:iolist_to_binary(io_lib:format("~p", [Error])), string),
                            router_tracing:set_span_status(error, erlang:iolist_to_binary(io_lib:format("~p", [Error])))
                    end
                end
            );
        {error, IdempotencyError} ->
            %% Security: Sanitize error before logging to prevent secret leakage
            SanitizedError = sanitize_error_for_logging(IdempotencyError),
            router_logger:error(<<"Idempotency check failed for usage emission">>, #{
                <<"assignment_id">> => AssignmentId,
                <<"request_id">> => RequestId,
                <<"error">> => SanitizedError
            }),
            %% On idempotency error, still emit (fail open)
            TraceContext = case TraceId of
                undefined -> undefined;
                _ -> #{<<"trace_id">> => TraceId}
            end,
            router_tracing:with_span(
                ?SPAN_ROUTER_EMIT_USAGE,
                #{
                    tenant_id => ValidatedTenantId,
                    provider_id => ProviderId,
                    event_type => JobType,
                    status => Status,
                    trace_id => TraceId,
                    assignment_id => AssignmentId,
                    request_id => RequestId,
                    usage_subject => UsageSubject,
                    latency_ms => LatencyMs,
                    cost => Cost
                },
                TraceContext,
                fun() ->
                    case emit_usage_event(UsageSubject, #{
                        tenant_id => ValidatedTenantId,
                        provider_id => ProviderId,
                        event_type => JobType,
                        latency_ms => LatencyMs,
                        cost => Cost,
                        status => Status,
                        trace_id => TraceId,
                        timestamp => Timestamp,
                        assignment_id => AssignmentId,
                        request_id => RequestId
                    }) of
                        ok ->
                            router_tracing:set_span_attribute(<<"usage.emit_result">>, <<"ok">>, string),
                            router_tracing:set_span_status(ok, undefined);
                        {error, Error} ->
                            router_tracing:set_span_attribute(<<"usage.emit_result">>, <<"error">>, string),
                            router_tracing:set_span_attribute(<<"usage.emit_error">>, erlang:iolist_to_binary(io_lib:format("~p", [Error])), string),
                            router_tracing:set_span_status(error, erlang:iolist_to_binary(io_lib:format("~p", [Error])))
                    end
                end
            )
    end,
    
    %% Log result
    log_result(AssignmentId, RequestId, Status, JobType),
    
    %% Set additional trace attributes for result processing
    router_tracing:set_span_attribute(<<"result.status">>, Status, string),
    router_tracing:set_span_attribute(<<"result.job_type">>, JobType, string),
    router_tracing:set_span_attribute(<<"result.provider_id">>, ProviderId, string),
    router_tracing:set_span_attribute(<<"result.latency_ms">>, LatencyMs, integer),
    router_tracing:set_span_attribute(<<"result.cost">>, Cost, float),
    router_tracing:set_span_attribute(<<"result.tenant_id">>, ValidatedTenantId, string),
    router_tracing:set_span_status(ok, undefined),
    
    %% CP2: Record Circuit Breaker success/failure based on provider result
    record_circuit_breaker_result(ValidatedTenantId, ProviderId, Status, Result),
    
    %% Acknowledge JetStream message (use msg_id from message, fallback to payload)
    FinalMsgId = case MsgId of
        undefined -> maps:get(<<"msg_id">>, Result, undefined);
        _ -> MsgId
    end,
    case FinalMsgId of
        undefined -> ok;
        MsgIdBin when is_binary(MsgIdBin) ->
            %% Use router_jetstream for ACK with metrics
            _ = router_jetstream:ack(#{id => MsgIdBin}),
            %% Clean up delivery count tracking on successful ACK
            cleanup_delivery_count(MsgIdBin)
    end.

%% Internal: Process ExecResult
-spec process_exec_result(map(), map(), binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined) -> ok.
process_exec_result(Result, _Headers, TraceIdFromHeaders, TenantIdFromHeaders, _VersionFromHeaders, MsgId) when is_map(Result) ->
    %% Extract correlation IDs
    AssignmentId = maps:get(<<"assignment_id">>, Result, undefined),
    RequestId = maps:get(<<"request_id">>, Result, undefined),
    
    %% Extract result fields
    Status = maps:get(<<"status">>, Result, undefined),
    JobType = get_job_type(Result),
    ProviderId = maps:get(<<"provider_id">>, Result, undefined),
    LatencyMs = maps:get(<<"latency_ms">>, Result, undefined),
    Cost = maps:get(<<"cost">>, Result, undefined),
    
    %% Use trace_id, tenant_id from headers (priority) or payload (fallback)
    TraceId = case TraceIdFromHeaders of
        undefined -> maps:get(<<"trace_id">>, Result, undefined);
        _ -> TraceIdFromHeaders
    end,
    TenantId = case TenantIdFromHeaders of
        undefined -> maps:get(<<"tenant_id">>, Result, undefined);
        _ -> TenantIdFromHeaders
    end,
    
    Timestamp = maps:get(<<"timestamp">>, Result, erlang:system_time(millisecond)),
    
    %% Validate required fields
    case validate_result(Result, AssignmentId, RequestId, Status) of
        ok ->
            %% Validate tenant_id against allowlist and policy registry
            ValidationContext = #{
                assignment_id => AssignmentId,
                request_id => RequestId,
                provider_id => ProviderId,
                job_type => JobType,
                trace_id => TraceId,
                source => <<"ExecResult">>
            },
            %% Check idempotency before processing
            IdempotencyKey = case AssignmentId of
                undefined -> RequestId;
                _ -> AssignmentId
            end,
            case router_idempotency:check_and_mark(<<"assignment_id">>, IdempotencyKey, #{
                status => Status,
                job_type => JobType,
                provider_id => ProviderId
            }) of
                {ok, seen} ->
                    %% Already processed - skip
                    router_logger:info(<<"Result already processed (idempotency)">>, #{
                        <<"assignment_id">> => AssignmentId,
                        <<"request_id">> => RequestId
                    }),
                    emit_counter(router_results_duplicate_total, #{
                        assignment_id => AssignmentId,
                        request_id => RequestId
                    }),
                    %% Still acknowledge message
                    case MsgId of
                        undefined -> ok;
                        _ -> router_jetstream:ack(#{id => MsgId})
                    end,
                    ok;
                {ok, not_seen} ->
                    %% Not seen before - validate tenant and process
                    case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
                        {ok, ValidatedTenantId} ->
                            %% Use validated tenant_id
                            process_validated_result(Result, ValidatedTenantId, AssignmentId, RequestId, Status, JobType, ProviderId, LatencyMs, Cost, TraceId, Timestamp, MsgId);
                        {error, Reason, ErrorContext} ->
                            %% Tenant validation failed - emit audit event and NAK for controlled redelivery
                            log_tenant_validation_error(AssignmentId, RequestId, Reason, ErrorContext),
                            emit_counter(router_results_tenant_rejected_total, maps:merge(ErrorContext, #{
                                assignment_id => AssignmentId,
                                request_id => RequestId,
                                reason => Reason,
                                error_code => error_code_reason(tenant_validation_failed)
                            })),
                            %% NAK message for controlled redelivery (respects MaxDeliver)
                            case MsgId of
                                undefined -> ok;  %% No msg_id available, cannot NAK
                                _ ->
                                    %% Check MaxDeliver exhaustion before NAK
                                    check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
                                    %% Use router_jetstream for NAK with metrics and backoff
                                    _ = router_jetstream:nak(#{id => MsgId}, tenant_validation_failed, #{
                                        assignment_id => AssignmentId,
                                        request_id => RequestId,
                                        source => <<"tenant_validation">>
                                    })
                            end,
                            ok
                    end;
                {error, IdempotencyError} ->
                    %% Security: Sanitize error before logging to prevent secret leakage
                    SanitizedError = sanitize_error_for_logging(IdempotencyError),
                    router_logger:error(<<"Idempotency check failed">>, #{
                        <<"assignment_id">> => AssignmentId,
                        <<"request_id">> => RequestId,
                        <<"error">> => SanitizedError
                    }),
                    %% On idempotency error, still try to process (fail open)
                    case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
                        {ok, ValidatedTenantId} ->
                            process_validated_result(Result, ValidatedTenantId, AssignmentId, RequestId, Status, JobType, ProviderId, LatencyMs, Cost, TraceId, Timestamp, MsgId);
                        {error, Reason, ErrorContext} ->
                            log_tenant_validation_error(AssignmentId, RequestId, Reason, ErrorContext),
                            emit_counter(router_results_tenant_rejected_total, maps:merge(ErrorContext, #{
                                assignment_id => AssignmentId,
                                request_id => RequestId,
                                reason => Reason,
                                error_code => error_code_reason(tenant_validation_failed)
                            })),
                            case MsgId of
                                undefined -> ok;
                                _ ->
                                    %% Check MaxDeliver exhaustion before NAK
                                    check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
                                    %% Use router_jetstream for NAK with metrics and backoff
                                    _ = router_jetstream:nak(#{id => MsgId}, tenant_validation_failed, #{
                                        assignment_id => AssignmentId,
                                        request_id => RequestId,
                                        source => <<"tenant_validation">>
                                    })
                            end,
                            ok
                    end
            end;
        {error, Reason} ->
            log_validation_error(AssignmentId, RequestId, Reason),
            emit_counter(router_results_validation_failed_total, #{
                assignment_id => AssignmentId,
                request_id => RequestId,
                reason => Reason,
                error_code => error_code_reason(Reason)
            })
    end.

%% Internal: Get job type from result
-spec get_job_type(map()) -> binary() | undefined.
get_job_type(Result) ->
    case maps:get(<<"job">>, Result, undefined) of
        Job when is_map(Job) ->
            maps:get(<<"type">>, Job, undefined);
        _ ->
            undefined
    end.

%% Internal: Validate result according to API_CONTRACTS.md
-spec validate_result(map(), binary() | undefined, binary() | undefined, binary() | undefined) ->
    ok | {error, atom()}.
validate_result(_Result, undefined, undefined, _Status) ->
    {error, missing_correlation_id};
validate_result(_Result, AssignmentId, RequestId, Status) ->
    %% At least one correlation ID must be present
    case AssignmentId =/= undefined orelse RequestId =/= undefined of
        false ->
            {error, missing_correlation_id};
        true ->
            %% Status is required and must be valid
            case Status of
                undefined ->
                    {error, missing_status};
                <<"success">> -> ok;
                <<"error">> -> ok;
                <<"timeout">> -> ok;
                <<"cancelled">> -> ok;
                _ ->
                    {error, invalid_status}
            end
    end.


%% Internal: Emit result received telemetry
-spec emit_result_received(map(), binary(), binary() | undefined, binary() | undefined, integer() | undefined, float() | undefined) -> ok.
emit_result_received(_Result, Status, JobType, ProviderId, LatencyMs, Cost) ->
    Metadata = #{
        status => Status,
        job_type => JobType,
        provider_id => ProviderId,
        latency_ms => LatencyMs,
        cost => Cost
    },
    emit_counter(router_results_total, Metadata),
    
    %% Emit latency histogram if available
    case LatencyMs of
        undefined -> ok;
        _ ->
            telemetry:execute(
                ?TELEMETRY_PREFIX ++ [router_result_latency_ms],
                #{value => LatencyMs},
                Metadata
            )
    end.

%% Internal: Emit usage event
-spec emit_usage_event(binary(), map()) -> ok | {error, term()}.
emit_usage_event(UsageSubject, UsageData) ->
    %% Build usage message
    UsageMessage = build_usage_message(UsageData),
    Json = jsx:encode(UsageMessage),
    
    %% Publish to usage subject
    case router_nats:publish(UsageSubject, Json) of
        ok ->
            emit_counter(router_usage_emitted_total, #{
                status => maps:get(status, UsageData, undefined),
                provider_id => maps:get(provider_id, UsageData, undefined)
            }),
            log_usage_emitted(UsageSubject, UsageData),
            ok;
        Error ->
            log_usage_error(UsageSubject, Error),
            emit_counter(router_usage_emit_failed_total, #{
                subject => UsageSubject,
                error => Error,
                error_code => error_code_reason(usage_emit_failed)
            }),
            {error, Error}
    end.

%% Internal: Build usage message
-spec build_usage_message(map()) -> map().
build_usage_message(Data) ->
    #{
        <<"version">> => <<"1">>,
        <<"tenant_id">> => maps:get(tenant_id, Data, undefined),
        <<"provider_id">> => maps:get(provider_id, Data, undefined),
        <<"event_type">> => maps:get(event_type, Data, undefined),
        <<"latency_ms">> => maps:get(latency_ms, Data, undefined),
        <<"cost">> => maps:get(cost, Data, undefined),
        <<"status">> => maps:get(status, Data, undefined),
        <<"trace_id">> => maps:get(trace_id, Data, undefined),
        <<"timestamp">> => maps:get(timestamp, Data, erlang:system_time(millisecond)),
        <<"assignment_id">> => maps:get(assignment_id, Data, undefined),
        <<"request_id">> => maps:get(request_id, Data, undefined)
    }.

%% Internal: Log tenant validation error
-spec log_tenant_validation_error(binary() | undefined, binary() | undefined, atom(), map()) -> ok.
log_tenant_validation_error(AssignmentId, RequestId, Reason, Context) ->
    %% Security: Filter PII from context before logging
    %% Context may contain sensitive data from tenant validation
    FilteredContext = router_logger:filter_pii(Context),
    router_logger:warn(<<"Tenant validation failed for ExecResult">>, maps:merge(FilteredContext, #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        validation_reason => Reason
    })).

%% Internal: Emit telemetry counter
-spec emit_counter(atom(), map()) -> ok.
emit_counter(CounterName, Metadata) ->
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [CounterName],
        #{count => 1},
        Metadata
    ).

%% Internal: Log functions (kept for future use)
%% Internal: Log functions (kept for future use)
%% -spec log_parse_error(binary(), term()) -> ok.
%% log_parse_error(Subject, Error) ->
%%     case erlang:function_exported(router_logger, error, 2) of
%%         true ->
%%             %% Security: Sanitize error before logging to prevent secret leakage
%%             SanitizedError = sanitize_error_for_logging(Error),
%%             router_logger:error(<<"Failed to parse ExecResult">>, #{
%%                 <<"subject">> => Subject,
%%                 <<"error">> => SanitizedError
%%             });
%%         false ->
%%             ok  %% Fallback: no logging if router_logger not available
%%     end.

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
            <<"[REDACTED: contains sensitive data]">>;
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.

-spec log_validation_error(binary() | undefined, binary() | undefined, atom()) -> ok.
log_validation_error(AssignmentId, RequestId, Reason) ->
    router_logger:warn(<<"ExecResult validation failed">>, #{
        <<"assignment_id">> => AssignmentId,
        <<"request_id">> => RequestId,
        <<"reason">> => Reason
    }).

-spec log_result(binary() | undefined, binary() | undefined, binary(), binary() | undefined) -> ok.
log_result(AssignmentId, RequestId, Status, JobType) ->
    case erlang:function_exported(router_logger, info, 2) of
        true ->
            router_logger:info(<<"ExecResult received">>, #{
                <<"assignment_id">> => AssignmentId,
                <<"request_id">> => RequestId,
                <<"status">> => Status,
                <<"job_type">> => JobType
            })
    end.

-spec log_usage_emitted(binary(), map()) -> ok.
log_usage_emitted(Subject, Data) ->
    case erlang:function_exported(router_logger, debug, 2) of
        true ->
            router_logger:debug(<<"Usage event emitted">>, #{
                <<"subject">> => Subject,
                <<"tenant_id">> => maps:get(tenant_id, Data, undefined),
                <<"provider_id">> => maps:get(provider_id, Data, undefined)
            });
        false ->
            ok
    end.

-spec log_usage_error(binary(), term()) -> ok.
log_usage_error(Subject, Error) ->
    case erlang:function_exported(router_logger, error, 2) of
        true ->
            %% Security: Sanitize error before logging to prevent secret leakage
            SanitizedError = sanitize_error_for_logging(Error),
            router_logger:error(<<"Failed to emit usage event">>, #{
                <<"subject">> => Subject,
                <<"error">> => SanitizedError
            })
    end.

%% Internal: Track delivery count for MaxDeliver exhaustion detection
-spec track_delivery_count(binary() | undefined) -> ok.
track_delivery_count(undefined) ->
    ok;
track_delivery_count(MsgId) ->
    %% Get or create ETS table for delivery count tracking
    Table = case ets:whereis(?DELIVERY_COUNT_TABLE) of
        undefined ->
            %% Table doesn't exist, create it (should be created in init, but handle race condition)
            ets:new(?DELIVERY_COUNT_TABLE, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        ExistingTable ->
            ExistingTable
    end,
    %% Log table create/reuse
    case ets:info(?DELIVERY_COUNT_TABLE) of
        undefined -> ok;
        _Info ->
            case erlang:function_exported(router_logger, info, 2) of
                true -> router_logger:info(<<"Delivery count ETS ready">>, #{<<"table">> => atom_to_binary(?DELIVERY_COUNT_TABLE, utf8)});
                false -> ok
            end
    end,
    %% Increment delivery count atomically
    case ets:lookup(Table, MsgId) of
        [] ->
            ets:insert(Table, {MsgId, 1}),
            %% DEBUG: first insert
            case erlang:function_exported(router_logger, debug, 2) of
                true -> router_logger:debug(<<"Delivery count insert">>, #{<<"msg_id">> => MsgId, <<"delivery_count">> => 1});
                false -> ok
            end;
        [{MsgId, _Count}] ->
            NewCount = ets:update_counter(Table, MsgId, 1),
            %% DEBUG: counter increment
            case erlang:function_exported(router_logger, debug, 2) of
                true -> router_logger:debug(<<"Delivery count increment">>, #{<<"msg_id">> => MsgId, <<"delivery_count">> => NewCount});
                false -> ok
            end
    end,
    ok.

%% Internal: Check and emit MaxDeliver exhaustion metric
-spec check_maxdeliver_exhaustion(binary() | undefined, binary() | undefined, binary() | undefined, map()) -> ok.
check_maxdeliver_exhaustion(undefined, _AssignmentId, _RequestId, _ErrorContext) ->
    ok;
check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext) ->
    case ets:whereis(?DELIVERY_COUNT_TABLE) of
        undefined ->
            ok;  %% Table not initialized yet
        Table ->
            %% Get MaxDeliver configuration
            MaxDeliver = application:get_env(beamline_router, nats_js_max_deliver, 3),
            case ets:lookup(Table, MsgId) of
                [] ->
                    ok;  %% No tracking data
                [{MsgId, DeliveryCount}] when DeliveryCount >= MaxDeliver ->
                    %% MaxDeliver exhausted - emit metric
                    emit_counter(router_jetstream_maxdeliver_exhausted_total, maps:merge(ErrorContext, #{
                        assignment_id => AssignmentId,
                        request_id => RequestId,
                        msg_id => MsgId,
                        delivery_count => DeliveryCount,
                        max_deliver => MaxDeliver,
                        reason => <<"maxdeliver_exhausted">>
                    })),
                    router_logger:warn(<<"MaxDeliver exhausted for message">>, #{
                        <<"msg_id">> => MsgId,
                        <<"assignment_id">> => AssignmentId,
                        <<"request_id">> => RequestId,
                        <<"delivery_count">> => DeliveryCount,
                        <<"max_deliver">> => MaxDeliver
                    }),
                    %% Remove from tracking (message will not be redelivered)
                    _ = ets:delete(Table, MsgId),
                    ok;
                _ ->
                    ok  %% Not exhausted yet
            end
    end.

%% Internal: Clean up delivery count tracking after successful ACK
-spec cleanup_delivery_count(binary() | undefined) -> ok.
cleanup_delivery_count(undefined) ->
    ok;
cleanup_delivery_count(MsgId) ->
    case ets:whereis(?DELIVERY_COUNT_TABLE) of
        undefined ->
            ok;
        Table ->
            ets:delete(Table, MsgId),
            %% DEBUG: cleanup
            case erlang:function_exported(router_logger, debug, 2) of
                true -> router_logger:debug(<<"Delivery count cleanup">>, #{<<"msg_id">> => MsgId});
                false -> ok
            end
    end,
    ok.

%% Internal: Get configuration
-spec get_config(atom(), term()) -> term().
get_config(Key, Default) ->
    application:get_env(beamline_router, Key, Default).
-spec error_code_reason(atom()) -> binary().
error_code_reason(parse_error) -> <<"PARSE_ERROR">>;
error_code_reason(missing_correlation_id) -> <<"MISSING_CORRELATION_ID">>;
error_code_reason(missing_status) -> <<"MISSING_STATUS">>;
error_code_reason(invalid_status) -> <<"INVALID_STATUS">>;
error_code_reason(tenant_validation_failed) -> <<"TENANT_VALIDATION_FAILED">>;
error_code_reason(usage_emit_failed) -> <<"USAGE_EMIT_FAILED">>;
error_code_reason(_) -> <<"UNKNOWN_ERROR">>.

%% ============================================================================
%% Circuit Breaker Integration (CP2)
%% ============================================================================

%% Internal: Record Circuit Breaker result based on provider outcome
%% Only records failures that should affect CB (timeout, 5xx, connection_error, provider_unavailable)
%% Does NOT record: 4xx, validation_error, rate_limit_exceeded, cancelled
-spec record_circuit_breaker_result(binary() | undefined, binary() | undefined, binary() | undefined, map()) -> ok.
record_circuit_breaker_result(undefined, _, _, _) ->
    %% No tenant_id - cannot record CB state
    ok;
record_circuit_breaker_result(_, undefined, _, _) ->
    %% No provider_id - cannot record CB state
    ok;
record_circuit_breaker_result(_, _, undefined, _) ->
    %% No status - cannot determine outcome
    ok;
record_circuit_breaker_result(TenantId, ProviderId, Status, Result) when is_binary(TenantId), is_binary(ProviderId), is_binary(Status) ->
    case should_record_cb_result(Status, Result) of
        success ->
            %% Record success for CB
            router_circuit_breaker:record_success(TenantId, ProviderId);
        failure ->
            %% Record failure for CB (only CB-relevant error types)
            router_circuit_breaker:record_failure(TenantId, ProviderId);
        ignore ->
            %% Do not record (4xx, validation_error, rate_limit_exceeded, cancelled)
            ok
    end.

%% Internal: Determine if result should be recorded in Circuit Breaker
%% Returns: success | failure | ignore
-spec should_record_cb_result(binary(), map()) -> success | failure | ignore.
should_record_cb_result(<<"success">>, _Result) ->
    success;
should_record_cb_result(<<"timeout">>, _Result) ->
    %% Timeout is always a failure for CB
    failure;
should_record_cb_result(<<"cancelled">>, _Result) ->
    %% Cancelled is not a failure for CB
    ignore;
should_record_cb_result(<<"error">>, Result) ->
    %% Check error type/code to determine if it's CB-relevant
    ErrorCode = maps:get(<<"error_code">>, Result, undefined),
    ErrorType = maps:get(<<"error_type">>, Result, undefined),
    ErrorMessage = maps:get(<<"error_message">>, Result, undefined),
    
    %% Check if error is CB-relevant (timeout, 5xx, connection_error, provider_unavailable)
    case is_cb_relevant_error(ErrorCode, ErrorType, ErrorMessage) of
        true ->
            failure;
        false ->
            %% 4xx, validation_error, rate_limit_exceeded - ignore for CB
            ignore
    end;
should_record_cb_result(_, _Result) ->
    %% Unknown status - ignore
    ignore.

%% Internal: Check if error is relevant for Circuit Breaker
%% CB-relevant errors: timeout, 5xx, connection_error, provider_unavailable
%% NOT relevant: 4xx, validation_error, rate_limit_exceeded
-spec is_cb_relevant_error(binary() | undefined, binary() | undefined, binary() | undefined) -> boolean().
is_cb_relevant_error(ErrorCode, ErrorType, ErrorMessage) ->
    %% Check error_code first (most specific)
    case ErrorCode of
        <<"TIMEOUT">> -> true;
        <<"CONNECTION_ERROR">> -> true;
        <<"PROVIDER_UNAVAILABLE">> -> true;
        <<"5XX">> -> true;
        <<"HTTP_5XX">> -> true;
        <<"SERVER_ERROR">> -> true;
        _ ->
            %% Check error_type
            case ErrorType of
                <<"timeout">> -> true;
                <<"connection_error">> -> true;
                <<"provider_unavailable">> -> true;
                <<"5xx">> -> true;
                <<"server_error">> -> true;
                _ ->
                    %% Check error_message for patterns (fallback)
                    case ErrorMessage of
                        undefined -> false;
                        Msg when is_binary(Msg) ->
                            %% Check for timeout, connection, 5xx patterns in message
                            (binary:match(Msg, <<"timeout">>) =/= nomatch) orelse
                            (binary:match(Msg, <<"connection">>) =/= nomatch) orelse
                            (binary:match(Msg, <<"5">>) =/= nomatch andalso binary:match(Msg, <<"5">>) =/= nomatch) orelse
                            (binary:match(Msg, <<"unavailable">>) =/= nomatch);
                        _ -> false
                    end
            end
    end.
