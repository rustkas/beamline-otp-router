%% @doc Decide Consumer
%% Subscribes to beamline.router.v1.decide via JetStream, parses DecideRequest,
%% routes through router_core, and publishes DecideResponse
%% CP2+: JetStream durable consumer with explicit ack policy
-module(router_decide_consumer).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([track_delivery_count/1, check_maxdeliver_exhaustion/3, cleanup_delivery_count/1]).
-export([normalize_boolean/1, check_tenant_allowed/1]).  %% Exported for testing only
-export([handle_decide_message/4]). %% Exported for testing
 -export([build_decide_response/2]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_decide_consumer]).
-define(DEFAULT_DECIDE_SUBJECT, <<"beamline.router.v1.decide">>).
-define(DEFAULT_JS_DURABLE_GROUP, <<"router-decide-consumer">>).

%% OpenTelemetry span names
-define(SPAN_ROUTER_PROCESS_DECIDE, <<"beamline.router.process.decide">>).

-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}  %% Monitor ref -> Request context for correlation
}).

%% ETS table for tracking delivery count per message (for MaxDeliver exhaustion detection)
-define(DELIVERY_COUNT_TABLE, router_decide_delivery_count).

%% @doc Start decide consumer
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize consumer
-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    %% Get configuration
    DecideSubject = get_config(decide_subject, ?DEFAULT_DECIDE_SUBJECT),
    JSDurableGroup = get_config(nats_js_durable_group_decide, ?DEFAULT_JS_DURABLE_GROUP),
    
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
            router_logger:info(<<"Delivery count ETS initialized for decide consumer">>, #{
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
    
    %% Subscribe to decide subject (JetStream durable queue)
    case subscribe_to_decide(DecideSubject, JSDurableGroup) of
        ok ->
            {ok, #state{
                connection = undefined,
                decide_subject = DecideSubject,
                js_durable_group = JSDurableGroup,
                publication_monitors = #{}
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
-spec handle_info({nats_message, binary(), binary()} | {nats_message, binary(), binary(), map()} | {nats_message, binary(), binary(), map(), binary() | undefined} | {nats_message, binary(), binary(), binary() | undefined, map(), binary() | undefined} | {'DOWN', reference(), process, pid(), term()}, #state{}) -> {noreply, #state{}}.
handle_info({nats_message, Subject, Payload}, State) ->
    %% Backward compatibility: no headers, no msg_id
    handle_decide_message(Subject, Payload, #{}, undefined),
    {noreply, State};
handle_info({nats_message, Subject, Payload, Headers}, State) ->
    %% Backward compatibility: headers but no msg_id
    handle_decide_message(Subject, Payload, Headers, undefined),
    {noreply, State};
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    %% Track delivery count for MaxDeliver exhaustion detection
    track_delivery_count(MsgId),
    handle_decide_message(Subject, Payload, Headers, MsgId),
    {noreply, State};
%% New format with ReplyTo (ignored for this subscriber)
handle_info({nats_message, Subject, Payload, _ReplyTo, Headers, MsgId}, State) ->
    %% Track delivery count for MaxDeliver exhaustion detection
    track_delivery_count(MsgId),
    handle_decide_message(Subject, Payload, Headers, MsgId),
    {noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    %% Handle publication process crash/exit
    %% Log error with correlation context if available
    case Reason of
        normal ->
            %% Normal exit - publication completed successfully
            ok;
        _ ->
            %% Abnormal exit - log error for monitoring
            log_publication_error(Pid, Reason, MonitorRef)
    end,
    %% Remove monitor reference from state (if tracking)
    NewMonitors = maps:remove(MonitorRef, State#state.publication_monitors),
    {noreply, State#state{publication_monitors = NewMonitors}};
handle_info(_Info, State) ->
    {noreply, State}.

%% Internal: Subscribe to decide subject (JetStream durable)
-spec subscribe_to_decide(binary(), binary()) -> ok | {error, term()}.
subscribe_to_decide(Subject, DurableGroup) ->
    %% Use router_jetstream wrapper for JetStream durable subscription
    %% Get DeliverGroup from config for horizontal scaling (queue group)
    DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_decide, undefined),
    case router_jetstream:subscribe_decide(#{
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

%% Internal: Handle decide message (callback from NATS subscription)
-spec handle_decide_message(binary(), binary(), map(), binary() | undefined) -> ok.
handle_decide_message(Subject, Payload, Headers, MsgId) ->
    handle_decide_message_internal(Subject, Payload, Headers, MsgId).

-spec handle_decide_message_internal(binary(), binary(), map(), binary() | undefined) -> ok.
handle_decide_message_internal(Subject, Payload, Headers, MsgId) ->
    try
        %% Check backpressure status before processing
        case router_intake_backpressure:check_backpressure(Subject) of
            {backpressure_active, RetryAfter} ->
                %% Backpressure active - reject message with NAK
                router_logger:warn(<<"Router intake backpressure active - rejecting message">>, #{
                    <<"subject">> => Subject,
                    <<"msg_id">> => MsgId,
                    <<"retry_after_seconds">> => RetryAfter
                }),
                %% NAK message (will be redelivered later)
                nak_message_if_needed(MsgId, RetryAfter),
                %% Increment rejection counter
                telemetry:execute([router_intake_backpressure, rejections], #{
                    count => 1
                }, #{
                    subject => Subject,
                    reason => backpressure_active
                }),
                ok;
            {backpressure_warning, _} ->
                %% Backpressure warning - continue processing but log
                router_logger:info(<<"Router intake backpressure warning - continuing processing">>, #{
                    <<"subject">> => Subject,
                    <<"msg_id">> => MsgId
                }),
                process_message_with_backpressure_warning(Subject, Payload, Headers, MsgId);
            {backpressure_inactive, _} ->
                %% No backpressure - process normally
                process_message_normal(Subject, Payload, Headers, MsgId)
        end
    catch
        Class:Reason:Stack ->
            %% Error handling
            ErrorCode = internal_validation_error,
            ErrorMessage = router_intake_error_codes:error_code_message(ErrorCode, #{
                <<"reason">> => <<"Internal error during message processing">>
            }),
            Context = #{
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId,
                <<"error">> => {Class, Reason},
                <<"stack">> => Stack
            },
            router_intake_error_handler:handle_intake_error(
                ErrorCode, ErrorMessage, Subject, Payload, Headers, MsgId, Context
            ),
            ok
    end.

%% Internal: Process message normally (no backpressure)
-spec process_message_normal(binary(), binary(), map(), binary() | undefined) -> ok.
process_message_normal(Subject, Payload, Headers, MsgId) ->
    try
        %% Early validation: Check payload size before parsing
        PayloadSize = byte_size(Payload),
        MaxPayloadSize = application:get_env(beamline_router, nats_max_payload_size, 1048576), %% Default: 1MB
        case PayloadSize > MaxPayloadSize of
            true ->
                ErrorCode = schema_validation_failed,
                ErrorMessage = router_intake_error_codes:error_code_message(ErrorCode, #{
                    <<"reason">> => <<"Payload size exceeds limit">>
                }),
                Context = #{
                    <<"subject">> => Subject,
                    <<"msg_id">> => MsgId,
                    <<"payload_size">> => PayloadSize,
                    <<"max_payload_size">> => MaxPayloadSize
                },
                router_intake_error_handler:handle_intake_error(
                    ErrorCode, ErrorMessage, Subject, Payload, Headers, MsgId, Context
                ),
                ok;
            false ->
                %% Track payload size for abuse detection (before validation)
                %% Track payload and check for abuse patterns
                check_and_track_payload(Subject, Payload, PayloadSize, Headers, MsgId),
                
                %% Use unified intake validator (CP2+)
                validate_and_process_message(Subject, Payload, Headers, MsgId)
        end
    catch
        _:Exception ->
            %% Internal error - handle via error handler
            CatchErrorCode = internal_validation_error,
            CatchErrorMessage = router_intake_error_codes:error_code_message(CatchErrorCode, #{
                <<"reason">> => iolist_to_binary(io_lib:format("~p", [Exception]))
            }),
            CatchContext = #{
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId,
                <<"exception">> => Exception
            },
            router_intake_error_handler:handle_intake_error(
                CatchErrorCode, CatchErrorMessage, Subject, Payload, Headers, MsgId, CatchContext
            ),
            ok
    end.

%% Internal: Check and track payload for abuse patterns
-spec check_and_track_payload(binary(), binary(), integer(), map(), binary() | undefined) -> ok.
check_and_track_payload(Subject, Payload, PayloadSize, Headers, MsgId) ->
    TenantId = extract_tenant_id_from_payload(Payload),
    case TenantId of
        undefined ->
            ok;
        TId when is_binary(TId) ->
            initialize_payload_tracker(),
            router_payload_tracker:track_payload(TId, decide, PayloadSize),
            handle_abuse_patterns(TId, Subject, Payload, Headers, MsgId)
    end.

%% Internal: Initialize payload tracker if needed
-spec initialize_payload_tracker() -> ok.
initialize_payload_tracker() ->
    case ets:whereis(router_payload_size_tracking) of
        undefined ->
            router_payload_tracker:init();
        _ ->
            ok
    end.

%% Internal: Handle abuse patterns
-spec handle_abuse_patterns(binary(), binary(), binary(), map(), binary() | undefined) -> ok.
handle_abuse_patterns(TId, Subject, Payload, Headers, MsgId) ->
    case router_payload_tracker:check_abuse_pattern(TId, decide) of
        {abuse, heavy_payload, AbuseContext} ->
            log_abuse_event(heavy_payload, TId, Subject, Payload, Headers, MsgId, AbuseContext),
            emit_abuse_metric(heavy_payload, TId, AbuseContext);
        {ok, normal} ->
            ok
    end.

%% Internal: Validate and process message
-spec validate_and_process_message(binary(), binary(), map(), binary() | undefined) -> ok.
validate_and_process_message(Subject, Payload, Headers, MsgId) ->
    case router_intake_validator:validate_intake_message(Subject, Payload, Headers, decide) of
        {ok, ValidatedMessage} ->
            process_validated_message(Subject, ValidatedMessage, Headers, MsgId);
        {error, {ErrorCode, ErrorMessage, ErrorContext}} ->
            handle_validation_error(ErrorCode, ErrorMessage, ErrorContext, Subject, Payload, Headers, MsgId)
    end.

%% Internal: Process validated message
-spec process_validated_message(binary(), map(), map(), binary() | undefined) -> ok.
process_validated_message(Subject, ValidatedMessage, Headers, MsgId) ->
    IdempotencyStatus = maps:get(<<"idempotency_status">>, ValidatedMessage, new),
    case IdempotencyStatus of
        duplicate ->
            handle_duplicate_request(ValidatedMessage, Subject, MsgId);
        new ->
            handle_decide_request(Subject, ValidatedMessage, Headers, MsgId)
    end.

%% Internal: Handle duplicate request
-spec handle_duplicate_request(map(), binary(), binary() | undefined) -> ok.
handle_duplicate_request(ValidatedMessage, Subject, MsgId) ->
    RequestId = maps:get(<<"request_id">>, ValidatedMessage, <<"unknown">>),
    IdempotencyKey = maps:get(<<"idempotency_key">>, ValidatedMessage, undefined),
    router_logger:info(<<"Duplicate decide request (idempotency)">>, #{
        <<"request_id">> => RequestId,
        <<"idempotency_key">> => IdempotencyKey,
        <<"subject">> => Subject,
        <<"msg_id">> => MsgId
    }),
    ack_message_if_needed(MsgId),
    ok.

%% Internal: Handle validation error
-spec handle_validation_error(atom(), binary(), map(), binary(), binary(), map(), binary() | undefined) -> ok.
handle_validation_error(ErrorCode, ErrorMessage, ErrorContext, Subject, Payload, Headers, MsgId) ->
    FullContext = maps:merge(ErrorContext, #{
        <<"msg_id">> => MsgId
    }),
    router_intake_error_handler:handle_intake_error(
        ErrorCode, ErrorMessage, Subject, Payload, Headers, MsgId, FullContext
    ),
    ok.

%% Internal: Handle DecideRequest
-spec handle_decide_request(binary(), map(), map(), binary() | undefined) -> ok.
handle_decide_request(Subject, Request, Headers, MsgId) ->
    %% CP2+: Check for missing headers and emit metric
    check_missing_headers(Headers, Request),
    
    %% Extract fields from DecideRequest
    TenantId = maps:get(<<"tenant_id">>, Request, undefined),
    Task = maps:get(<<"task">>, Request, #{}),
    PolicyId = maps:get(<<"policy_id">>, Request, undefined),
    Constraints = maps:get(<<"constraints">>, Request, #{}),
    RequestMetadata = maps:get(<<"metadata">>, Request, #{}),
    
    %% Extract CP1 correlation fields (run_id, flow_id, step_id)
    RunId = maps:get(<<"run_id">>, Request, undefined),
    FlowId = maps:get(<<"flow_id">>, Request, undefined),
    StepId = maps:get(<<"step_id">>, Request, undefined),
    RequestId = maps:get(<<"request_id">>, Request, undefined),
    TraceId = maps:get(<<"trace_id">>, Request, undefined),
    
    %% CP2+: Validate tenant_id if validation is enabled
    TenantValidationEnabled = application:get_env(beamline_router, tenant_validation_enabled, true),
    ValidationContext = #{
        <<"request_id">> => RequestId,
        <<"trace_id">> => TraceId,
        <<"subject">> => Subject,
        <<"msg_id">> => MsgId,
        <<"source">> => <<"decide_consumer">>
    },
    
    case validate_tenant_if_enabled(TenantValidationEnabled, TenantId, ValidationContext, MsgId, Subject, Request) of
        {error, tenant_validation_failed} ->
            %% Tenant validation failed - error response already sent, message NAKed
            ok;
        ok ->
            %% Continue with routing
            continue_decide_request(Subject, Request, Headers, MsgId, TenantId, Task, PolicyId,
                                   Constraints, RequestMetadata, RunId, FlowId, StepId)
    end.

%% Internal: Validate tenant if enabled
-spec validate_tenant_if_enabled(boolean(), binary() | undefined, map(), binary() | undefined, binary(), map()) -> 
    ok | {error, tenant_validation_failed}.
validate_tenant_if_enabled(false, _TenantId, _ValidationContext, _MsgId, _Subject, _Request) ->
    ok;
validate_tenant_if_enabled(true, TenantId, ValidationContext, MsgId, Subject, Request) ->
    case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
        {ok, _ValidatedTenantId} ->
            ok;
        {error, Reason, ErrorContext} ->
            %% Tenant validation failed - emit metric, log, send NAK
            RequestId = maps:get(<<"request_id">>, Request, <<"unknown">>),
            _TraceId = maps:get(<<"trace_id">>, Request, undefined),
            
            emit_counter(router_decide_tenant_rejected_total, maps:merge(ErrorContext, #{
                tenant_id => TenantId,
                reason => Reason,
                request_id => RequestId
            })),
            
            router_logger:warn(<<"Tenant validation failed for decide request">>, #{
                <<"request_id">> => RequestId,
                <<"tenant_id">> => TenantId,
                <<"reason">> => Reason,
                <<"msg_id">> => MsgId
            }),
            
            %% Build and send error response
            ErrorResponse = build_error_response(Request, tenant_not_allowed, ErrorContext),
            ErrorResponseJson = jsx:encode(ErrorResponse),
            ReplySubject = <<Subject/binary, ".reply">>,
            router_nats:publish(ReplySubject, ErrorResponseJson),
            
            %% NAK message for controlled redelivery (respects MaxDeliver)
            case MsgId of
                undefined -> ok;
                _ ->
                    check_maxdeliver_exhaustion(MsgId, RequestId, ErrorContext),
                    _ = router_jetstream:nak(#{id => MsgId}, tenant_validation_failed, #{
                        request_id => RequestId,
                        source => <<"tenant_validation">>
                    })
            end,
            
            {error, tenant_validation_failed}
    end.

%% Internal: Continue processing decide request after tenant validation
-spec continue_decide_request(binary(), map(), map(), binary() | undefined, binary() | undefined, 
                              map(), binary() | undefined, map(), map(), binary() | undefined,
                              binary() | undefined, binary() | undefined) -> ok.
continue_decide_request(Subject, Request, _Headers, MsgId, TenantId, Task, PolicyId,
                       Constraints, RequestMetadata, RunId, FlowId, StepId) ->
    %% Build message map for RouteRequest
    %% Include CP1 correlation fields (run_id, flow_id, step_id) in message for propagation
    Message = #{
        <<"tenant_id">> => TenantId,
        <<"message_id">> => maps:get(<<"request_id">>, Request, undefined),
        <<"trace_id">> => maps:get(<<"trace_id">>, Request, undefined),
        <<"message_type">> => maps:get(<<"type">>, Task, <<"unknown">>),
        <<"payload_ref">> => maps:get(<<"payload_ref">>, Task, undefined),
        <<"payload">> => maps:get(<<"payload">>, Task, undefined),
        <<"run_id">> => RunId,
        <<"flow_id">> => FlowId,
        <<"step_id">> => StepId
    },
    
    %% Build context with constraints and metadata
    Context = maps:merge(Constraints, RequestMetadata),
    
    %% Create RouteRequest
    RouteRequest = #route_request{
        message = Message,
        policy_id = PolicyId,
        context = Context
    },
    
    %% Route the message (accept {Result, StopMetadata} or Result)
    CoreResult = router_core:route(RouteRequest, #{}),
    case CoreResult of
        {{ok, Decision}, _StopMetadata} ->
            %% Build DecideResponse
            Response = build_decide_response(Request, Decision),
            ResponseJson = jsx:encode(Response),
            
            %% Publish response (reply-inbox or .reply subject)
            ReplySubject = <<Subject/binary, ".reply">>,
            router_nats:publish(ReplySubject, ResponseJson),
            
            %% ACK JetStream message after successful processing
            ack_message_if_needed(MsgId),
            
            %% Optional: push assignment to CAF if requested
            %% Check global enable flag and tenant allowlist before calling adapter
            %% Use async spawn_monitor to avoid blocking subscriber and track errors
            case should_publish_assignment(Request) of
                true ->
                    %% Spawn monitored process for publication
                    %% The adapter handles retries and telemetry internally
                    %% We monitor the process to catch unexpected crashes
                    {_Pid, _MonitorRef} = spawn_monitor(fun() ->
                        router_caf_adapter:publish_assignment(Request, Decision)
                    end),
                    ok;
                false ->
                    ok
            end;
        {ok, Decision} ->
            %% Build DecideResponse
            Response = build_decide_response(Request, Decision),
            ResponseJson = jsx:encode(Response),
            
            %% Publish response (reply-inbox or .reply subject)
            ReplySubject = <<Subject/binary, ".reply">>,
            router_nats:publish(ReplySubject, ResponseJson),
            
            %% ACK JetStream message after successful processing
            ack_message_if_needed(MsgId),
            
            %% Optional: push assignment to CAF if requested
            case should_publish_assignment(Request) of
                true ->
                    {_Pid, _MonitorRef} = spawn_monitor(fun() ->
                        router_caf_adapter:publish_assignment(Request, Decision)
                    end),
                    ok;
                false ->
                    ok
            end;
        {{error, {ErrorReason, ErrorContext}}, _StopMetadata} ->
            %% Build ErrorResponse
            ErrorResponse = build_error_response(Request, ErrorReason, ErrorContext),
            ErrorResponseJson = jsx:encode(ErrorResponse),
            ReplySubject = <<Subject/binary, ".reply">>,
            router_nats:publish(ReplySubject, ErrorResponseJson),
            
            %% Security: Filter PII from error context before logging
            FilteredErrorContext = router_logger:filter_pii(ErrorContext),
            %% Build log context with CP1 correlation fields
            LogContext = build_log_context(Request, #{
                <<"error">> => ErrorReason,
                <<"error_context">> => FilteredErrorContext,
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId
            }),
            router_logger:error(<<"Routing failed">>, LogContext),
            
            %% ACK message even on error (error response sent to Gateway)
            ack_message_if_needed(MsgId);
        {error, {ErrorReason, ErrorContext}} ->
            %% Build ErrorResponse
            ErrorResponse = build_error_response(Request, ErrorReason, ErrorContext),
            ErrorResponseJson = jsx:encode(ErrorResponse),
            ReplySubject = <<Subject/binary, ".reply">>,
            router_nats:publish(ReplySubject, ErrorResponseJson),
            
            %% Security: Filter PII from error context before logging
            FilteredErrorContext = router_logger:filter_pii(ErrorContext),
            %% Build log context with CP1 correlation fields
            LogContext = build_log_context(Request, #{
                <<"error">> => ErrorReason,
                <<"error_context">> => FilteredErrorContext,
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId
            }),
            router_logger:error(<<"Routing failed">>, LogContext),
            
            %% ACK message even on error (error response sent to Gateway)
            ack_message_if_needed(MsgId)
    end.

%% Internal: ACK message if needed (JetStream)
-spec ack_message_if_needed(binary() | undefined) -> ok.
ack_message_if_needed(undefined) ->
    ok;
ack_message_if_needed(MsgId) when is_binary(MsgId) ->
    %% Use router_jetstream for ACK with metrics
    _ = router_jetstream:ack(#{id => MsgId}),
    %% Clean up delivery count tracking on successful ACK
    cleanup_delivery_count(MsgId),
    ok;
ack_message_if_needed(_) ->
    ok.

%% Internal: NAK message if needed (with optional retry after delay)
%% Note: RequestId may not be available at this point (backpressure check happens before parsing)
-spec nak_message_if_needed(binary() | undefined, non_neg_integer()) -> ok.
nak_message_if_needed(undefined, _RetryAfter) ->
    ok;
nak_message_if_needed(MsgId, RetryAfter) when is_binary(MsgId), is_integer(RetryAfter), RetryAfter >= 0 ->
    %% Use router_jetstream for NAK with metrics and backoff
    %% RequestId is not available at this point (backpressure check happens before parsing)
    _ = router_jetstream:nak(#{id => MsgId}, backpressure, #{
        request_id => <<"unknown">>,
        source => <<"backpressure">>
    }),
    ok;
nak_message_if_needed(_, _) ->
    ok.

%% Internal: Process message with backpressure warning (continue but log)
-spec process_message_with_backpressure_warning(binary(), binary(), map(), binary() | undefined) -> ok.
process_message_with_backpressure_warning(Subject, Payload, Headers, MsgId) ->
    %% Process normally but with awareness of backpressure warning
    process_message_normal(Subject, Payload, Headers, MsgId).

%% Internal: Check if assignment should be published
%% Verifies global enable flag and tenant allowlist
-spec should_publish_assignment(map()) -> boolean().
should_publish_assignment(Request) ->
    %% Check if push_assignment flag is set
    PushAssignment = maps:get(<<"push_assignment">>, Request, false),
    case normalize_boolean(PushAssignment) of
        false ->
            false;
        true ->
            %% Check global enable flag
            case application:get_env(beamline_router, caf_push_assignment_enabled, true) of
                false ->
                    false;
                true ->
                    %% Check tenant allowlist
                    TenantId = maps:get(<<"tenant_id">>, Request, undefined),
                    check_tenant_allowed(TenantId)
            end
    end.

%% Internal: Check if tenant is allowed
-spec check_tenant_allowed(binary() | undefined) -> boolean().
check_tenant_allowed(undefined) ->
    %% If no tenant allowlist configured, allow all
    case application:get_env(beamline_router, caf_push_assignment_allowed_tenants, undefined) of
        undefined -> true;
        _ -> false  %% Require tenant_id if allowlist is configured
    end;
check_tenant_allowed(TenantId) ->
    case application:get_env(beamline_router, caf_push_assignment_allowed_tenants, undefined) of
        undefined -> true;  %% No allowlist = allow all
        AllowedTenants when is_list(AllowedTenants) ->
            %% Support both binary and string tenant IDs
            %% Normalize TenantId to binary for comparison
            TenantIdBin = case TenantId of
                Bin when is_binary(Bin) -> Bin;
                TenantStr when is_list(TenantStr) -> list_to_binary(TenantStr);
                _ -> TenantId
            end,
            %% Check if any element in allowlist matches (supporting both binary and string)
            lists:any(fun(Allowed) ->
                case Allowed of
                    AllowedBin when is_binary(AllowedBin) -> AllowedBin =:= TenantIdBin;
                    AllowedStr when is_list(AllowedStr) -> list_to_binary(AllowedStr) =:= TenantIdBin;
                    _ -> Allowed =:= TenantId
                end
            end, AllowedTenants);
        AllowedTenants when is_map(AllowedTenants) ->
            %% Normalize TenantId to binary for map key lookup
            TenantIdBin = case TenantId of
                Bin when is_binary(Bin) -> Bin;
                TenantStr when is_list(TenantStr) -> list_to_binary(TenantStr);
                _ -> TenantId
            end,
            maps:is_key(TenantIdBin, AllowedTenants) orelse
            (is_list(TenantId) andalso maps:is_key(list_to_binary(TenantId), AllowedTenants));
        _ -> false
    end.

%% Internal: Build DecideResponse
-spec build_decide_response(map(), #route_decision{}) -> map().
build_decide_response(Request, #route_decision{
    provider_id = ProviderId,
    reason = Reason,
    priority = Priority,
    expected_latency_ms = Latency,
    expected_cost = Cost,
    metadata = Metadata
}) ->
    RequestId = maps:get(<<"request_id">>, Request, undefined),
    TraceId = maps:get(<<"trace_id">>, Request, undefined),
    
    %% Build decision object
    Decision = #{
        <<"provider_id">> => ProviderId,
        <<"priority">> => Priority,
        <<"expected_latency_ms">> => Latency,
        <<"expected_cost">> => Cost,
        <<"reason">> => Reason
    },
    
    %% Add optional fields
    DecisionWithOptional = case Metadata of
        #{} when map_size(Metadata) > 0 ->
            maps:put(<<"metadata">>, Metadata, Decision);
        _ ->
            Decision
    end,
    
    %% Build context
    Context = case TraceId of
        undefined -> #{<<"request_id">> => RequestId};
        _ -> #{<<"request_id">> => RequestId, <<"trace_id">> => TraceId}
    end,
    
    #{
        <<"ok">> => true,
        <<"decision">> => DecisionWithOptional,
        <<"context">> => Context
    }.

%% Internal: Build ErrorResponse
-spec build_error_response(map(), atom() | binary(), map() | binary()) -> map().
build_error_response(Request, ErrorReason, ErrorContext) ->
    RequestId = maps:get(<<"request_id">>, Request, undefined),
    TraceId = maps:get(<<"trace_id">>, Request, undefined),
    
    %% Handle both old format (atom, map) and new format (binary, binary)
    {ErrorCode, ErrorMessage, ErrorDetails} = case {ErrorReason, ErrorContext} of
        {Code, Context} when is_atom(Code), is_map(Context) ->
            %% Check if this is an extension error
            case is_extension_error(Code) of
                true ->
                    %% Use extension error mapper
                    {ExtCode, ExtMsg, ExtDetails} = router_extension_error_mapper:map_extension_error({error, {Code, Context}}),
                    {ExtCode, ExtMsg, ExtDetails};
                false ->
                    %% Old format: atom reason, map context
                    CodeBin = case Code of
                        missing_tenant_id -> <<"invalid_request">>;
                        policy_not_found -> <<"policy_not_found">>;
                        no_provider_available -> <<"decision_failed">>;
                        invalid_policy -> <<"invalid_request">>;
                        _ -> <<"internal">>
                    end,
                    Msg = maps:get(<<"context">>, Context, <<"Routing failed">>),
                    {CodeBin, Msg, Context}
            end;
        {Code, Message} when is_binary(Code), is_binary(Message) ->
            %% New format: binary code, binary message
            {Code, Message, #{}};
        {Code, NonMapContext} when is_atom(Code) ->
            %% Check if this is an extension error
            _ = NonMapContext,  %% Non-map context not used in fallback path
            case is_extension_error(Code) of
                true ->
                    %% Use extension error mapper (with empty context)
                    {ExtCode, ExtMsg, ExtDetails} = router_extension_error_mapper:map_extension_error({error, {Code, #{}}}),
                    {ExtCode, ExtMsg, ExtDetails};
                false ->
                    %% Fallback: atom reason, non-map context
                    CodeBin = case Code of
                        missing_tenant_id -> <<"invalid_request">>;
                        policy_not_found -> <<"policy_not_found">>;
                        no_provider_available -> <<"decision_failed">>;
                        invalid_policy -> <<"invalid_request">>;
                        _ -> <<"internal">>
                    end,
                    {CodeBin, <<"Routing failed">>, #{}}
            end
    end,
    
    %% Build error object
    Error = #{
        <<"code">> => ErrorCode,
        <<"message">> => ErrorMessage
    },
    
    %% Add details if available
    ErrorWithDetails = case ErrorDetails of
        DetailsMap when is_map(DetailsMap), map_size(DetailsMap) > 0 ->
            maps:put(<<"details">>, DetailsMap, Error);
        _ ->
            Error
    end,
    
    %% Build response context
    ResponseContext = case TraceId of
        undefined -> #{<<"request_id">> => RequestId};
        _ -> #{<<"request_id">> => RequestId, <<"trace_id">> => TraceId}
    end,
    
    #{
        <<"ok">> => false,
        <<"error">> => ErrorWithDetails,
        <<"context">> => ResponseContext
    }.

%% Internal: Check if error is extension-related
-spec is_extension_error(atom()) -> boolean().
is_extension_error(extension_not_found) -> true;
is_extension_error(extension_timeout) -> true;
is_extension_error(validator_blocked) -> true;
is_extension_error(post_processor_failed) -> true;
is_extension_error(extension_circuit_open) -> true;
is_extension_error(extension_invocation_error) -> true;
is_extension_error(extension_max_retries_exceeded) -> true;
is_extension_error(extension_registry_error) -> true;
is_extension_error(extension_load_balancer_error) -> true;
is_extension_error(pipeline_too_deep) -> true;
is_extension_error(too_many_pre_processors) -> true;
is_extension_error(too_many_validators) -> true;
is_extension_error(too_many_post_processors) -> true;
is_extension_error(_) -> false.


%% Internal: Normalize boolean value
%% Handles: true, false, <<"true">>, <<"false">>, 1, 0, etc.
-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(false) -> false;
normalize_boolean(<<"true">>) -> true;
normalize_boolean(<<"false">>) -> false;
normalize_boolean(1) -> true;
normalize_boolean(0) -> false;
normalize_boolean(_) -> false.  %% Default to false for unknown values

%% Internal: Log publication error with correlation context
-spec log_publication_error(pid(), term(), reference()) -> ok.
log_publication_error(Pid, Reason, MonitorRef) ->
    %% Log error for monitoring/debugging
    case erlang:function_exported(router_logger, error, 2) of
        true ->
            %% Security: Sanitize reason before logging to prevent secret leakage
            SanitizedReason = sanitize_error_for_logging(Reason),
            router_logger:error(<<"CAF assignment publication process crashed">>, #{
                pid => Pid,
                reason => SanitizedReason,
                monitor_ref => MonitorRef
            })
    end,
    ok.

%% Internal: Build log context with CP1 correlation fields
-spec build_log_context(map(), map()) -> map().
build_log_context(Request, BaseContext) ->
    %% Extract CP1 correlation fields (run_id, flow_id, step_id)
    RunId = maps:get(<<"run_id">>, Request, undefined),
    FlowId = maps:get(<<"flow_id">>, Request, undefined),
    StepId = maps:get(<<"step_id">>, Request, undefined),
    
    %% Add correlation fields to context if present
    Context1 = case RunId of
        undefined -> BaseContext;
        _ -> maps:put(<<"run_id">>, RunId, BaseContext)
    end,
    Context2 = case FlowId of
        undefined -> Context1;
        _ -> maps:put(<<"flow_id">>, FlowId, Context1)
    end,
    Context3 = case StepId of
        undefined -> Context2;
        _ -> maps:put(<<"step_id">>, StepId, Context2)
    end,
    Context3.

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
    %% Increment delivery count atomically
    case ets:lookup(Table, MsgId) of
        [] ->
            ets:insert(Table, {MsgId, 1});
        [{MsgId, _Count}] ->
            _NewCount = ets:update_counter(Table, MsgId, 1),
            ok
    end,
    ok.

%% Internal: Check and emit MaxDeliver exhaustion metric
-spec check_maxdeliver_exhaustion(binary() | undefined, binary() | undefined, map()) -> ok.
check_maxdeliver_exhaustion(undefined, _RequestId, _ErrorContext) ->
    ok;
check_maxdeliver_exhaustion(MsgId, RequestId, ErrorContext) ->
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
                        request_id => RequestId,
                        msg_id => MsgId,
                        delivery_count => DeliveryCount,
                        max_deliver => MaxDeliver,
                        reason => <<"maxdeliver_exhausted">>
                    })),
                    router_logger:warn(<<"MaxDeliver exhausted for decide message">>, #{
                        <<"msg_id">> => MsgId,
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
            ets:delete(Table, MsgId)
    end,
    ok.

%% Internal: Emit telemetry counter
-spec emit_counter(atom(), map()) -> ok.
emit_counter(CounterName, Metadata) ->
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [CounterName],
        #{count => 1},
        Metadata
    ).

%% Internal: Check for missing headers and emit metric
%% CP2+ requirement: Track missing trace_id, span_id, tenant_id headers
-spec check_missing_headers(map(), map()) -> ok.
check_missing_headers(Headers, Request) ->
    %% Extract headers (priority) or payload (fallback) for key headers
    TraceId = case maps:get(<<"trace_id">>, Headers, undefined) of
        undefined -> maps:get(<<"trace_id">>, Request, undefined);
        V1 -> V1
    end,
    SpanId = case maps:get(<<"span_id">>, Headers, undefined) of
        undefined -> maps:get(<<"span_id">>, Request, undefined);
        V2 -> V2
    end,
    TenantId = case maps:get(<<"tenant_id">>, Headers, undefined) of
        undefined -> maps:get(<<"tenant_id">>, Request, undefined);
        V3 -> V3
    end,
    
    %% Check for missing headers
    MissingHeaders = [],
    MissingHeaders1 = case TraceId of
        undefined -> [<<"trace_id">> | MissingHeaders];
        _ -> MissingHeaders
    end,
    MissingHeaders2 = case SpanId of
        undefined -> [<<"span_id">> | MissingHeaders1];
        _ -> MissingHeaders1
    end,
    MissingHeaders3 = case TenantId of
        undefined -> [<<"tenant_id">> | MissingHeaders2];
        _ -> MissingHeaders2
    end,
    
    %% Emit metric if any headers are missing
    case MissingHeaders3 of
        [] ->
            ok;
        _ ->
            router_metrics:inc(ctx_missing_headers_total),
            router_logger:warn(<<"Missing headers detected">>, #{
                <<"missing_headers">> => MissingHeaders3,
                <<"subject">> => <<"beamline.router.v1.decide">>
            })
    end,
    ok.

%% Internal: Extract tenant_id from payload (for abuse tracking)
-spec extract_tenant_id_from_payload(binary()) -> binary() | undefined.
extract_tenant_id_from_payload(Payload) ->
    try
        Decoded = jsx:decode(Payload, [return_maps]),
        maps:get(<<"tenant_id">>, Decoded, undefined)
    catch
        _:_ ->
            undefined
    end.

%% Internal: Log abuse event
-spec log_abuse_event(atom(), binary(), binary(), binary(), map(), binary() | undefined, map()) -> ok.
log_abuse_event(heavy_payload, TenantId, Subject, Payload, Headers, MsgId, AbuseContext) ->
    _ = Payload,  %% Payload size tracked in AbuseContext
    RequestId = maps:get(<<"request_id">>, AbuseContext, undefined),
    TraceId = maps:get(<<"trace_id">>, Headers, maps:get(<<"trace_id">>, AbuseContext, undefined)),
    
    router_logger:warn(<<"Abuse detected: heavy payload pattern">>, #{
        <<"event_type">> => <<"abuse.heavy_payload">>,
        <<"tenant_id">> => TenantId,
        <<"request_id">> => RequestId,
        <<"trace_id">> => TraceId,
        <<"subject">> => Subject,
        <<"msg_id">> => MsgId,
        <<"context">> => AbuseContext
    }).

%% Internal: Emit abuse metric
-spec emit_abuse_metric(atom(), binary(), map()) -> ok.
emit_abuse_metric(heavy_payload, TenantId, AbuseContext) ->
    router_metrics:emit_metric(
        router_abuse_heavy_payload_total,
        #{count => 1},
        maps:merge(AbuseContext, #{
            tenant_id => TenantId
        })
    );
emit_abuse_metric(empty_payload, TenantId, AbuseContext) ->
    router_metrics:emit_metric(
        router_abuse_empty_payload_total,
        #{count => 1},
        maps:merge(AbuseContext, #{
            tenant_id => TenantId
        })
    );
emit_abuse_metric(targeted_tenant, TenantId, AbuseContext) ->
    router_metrics:emit_metric(
        router_abuse_targeted_tenant_total,
        #{count => 1},
        maps:merge(AbuseContext, #{
            tenant_id => TenantId
        })
    ).

%% Internal: Get configuration
-spec get_config(atom(), term()) -> term().
get_config(Key, Default) ->
    application:get_env(beamline_router, Key, Default).

%% ====================================================================
%% GenServer Boilerplate
%% ====================================================================

%% @doc Handle shutdown
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
