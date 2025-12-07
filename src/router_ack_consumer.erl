%% @doc ACK Consumer (Optional)
%% Subscribes to caf.exec.assign.v1.ack, logs accepted/rejected status,
%% and increments counters for rejections
-module(router_ack_consumer).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([track_delivery_count/1, check_maxdeliver_exhaustion/3, cleanup_delivery_count/1]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_ack_consumer]).
-define(DEFAULT_ACK_SUBJECT, <<"caf.exec.assign.v1.ack">>).
-define(DEFAULT_JS_DURABLE_GROUP, <<"router-acks">>).

%% OpenTelemetry span names
-define(SPAN_ROUTER_PROCESS_ACK, <<"beamline.router.process.ack">>).

-record(state, {
    connection :: pid() | undefined,
    ack_subject :: binary(),
    js_durable_group :: binary()
}).

%% ETS table for tracking delivery count per message (for MaxDeliver exhaustion detection)
-define(DELIVERY_COUNT_TABLE, router_ack_delivery_count).

%% @doc Start ACK consumer
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize consumer
-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    %% Check if ACK consumer is enabled
    case get_config(ack_enabled, false) of
        false ->
            {stop, ack_disabled};
        true ->
            %% Get configuration
            AckSubject = get_config(ack_subject, ?DEFAULT_ACK_SUBJECT),
            JSDurableGroup = get_config(nats_js_durable_group_acks, ?DEFAULT_JS_DURABLE_GROUP),
            
            %% Subscribe to ACK subject (JetStream durable queue)
            case subscribe_to_acks(AckSubject, JSDurableGroup) of
                ok ->
                    {ok, #state{
                        connection = undefined,
                        ack_subject = AckSubject,
                        js_durable_group = JSDurableGroup
                    }};
                Error ->
                    {stop, Error}
            end
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
    handle_ack_message(Subject, Payload, #{}, undefined),
    {noreply, State};
handle_info({nats_message, Subject, Payload, Headers}, State) ->
    %% Backward compatibility: headers but no msg_id
    handle_ack_message(Subject, Payload, Headers, undefined),
    {noreply, State};
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    %% Track delivery count for MaxDeliver exhaustion detection
    track_delivery_count(MsgId),
    handle_ack_message(Subject, Payload, Headers, MsgId),
    {noreply, State};
%% New format with ReplyTo (ignored for this subscriber)
handle_info({nats_message, Subject, Payload, _ReplyTo, Headers, MsgId}, State) ->
    %% Track delivery count for MaxDeliver exhaustion detection
    track_delivery_count(MsgId),
    handle_ack_message(Subject, Payload, Headers, MsgId),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Internal: Subscribe to ACK subject (JetStream durable)
-spec subscribe_to_acks(binary(), binary()) -> ok | {error, term()}.
subscribe_to_acks(Subject, DurableGroup) ->
    %% Use router_jetstream wrapper for JetStream durable subscription
    %% Get DeliverGroup from config for horizontal scaling (queue group)
    DeliverGroup = application:get_env(beamline_router, nats_js_deliver_group_acks, undefined),
    case router_jetstream:subscribe_acks(#{
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

%% Internal: Handle ACK message (callback from NATS subscription)
-spec handle_ack_message(binary(), binary(), map(), binary() | undefined) -> ok.
handle_ack_message(Subject, Payload, Headers, MsgId) ->
    handle_ack_message_internal(Subject, Payload, Headers, MsgId).

-spec handle_ack_message_internal(binary(), binary(), map(), binary() | undefined) -> ok.
handle_ack_message_internal(Subject, Payload, Headers, MsgId) ->
    try
        %% Use unified intake validator (CP2+)
        case router_intake_validator:validate_intake_message(Subject, Payload, Headers, ack) of
            {ok, ValidatedMessage} ->
                %% All validations passed - process ack
                Ack = ValidatedMessage,
                %% Extract trace_id, tenant_id, version from headers (priority) or payload (fallback)
                TraceId = extract_header_or_payload(Headers, Ack, <<"trace_id">>, <<"trace_id">>),
                TenantId = extract_header_or_payload(Headers, Ack, <<"tenant_id">>, <<"tenant_id">>),
                Version = extract_header_or_payload(Headers, Ack, <<"version">>, <<"version">>),
            
            %% CP2+: Create parent context from trace_id (if tracing enabled)
            TracingEnabled = application:get_env(beamline_router, tracing_enabled, false),
            ParentContext = case TracingEnabled andalso TraceId =/= undefined of
                true -> #{<<"trace_id">> => TraceId};
                false -> undefined
            end,
            %% CP2+: Process with OpenTelemetry span (if tracing enabled)
            case TracingEnabled of
                true ->
                    router_tracing:with_span(
                        ?SPAN_ROUTER_PROCESS_ACK,
                        #{
                            subject => Subject,
                            assignment_id => maps:get(<<"assignment_id">>, Ack, undefined),
                            trace_id => TraceId,
                            tenant_id => TenantId,
                            version => Version,
                            status => maps:get(<<"status">>, Ack, undefined),
                            message => maps:get(<<"message">>, Ack, undefined),
                            msg_id => MsgId
                        },
                        ParentContext,
                        fun() ->
                            process_ack(Ack, Headers, TraceId, TenantId, Version, MsgId)
                        end
                    );
                false ->
                    %% CP1 baseline: Process without tracing
                    process_ack(Ack, Headers, TraceId, TenantId, Version, MsgId)
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
        ExceptionType:Exception ->
            %% Internal error - handle via error handler
            ErrCode2 = internal_validation_error,
            ErrMessage2 = router_intake_error_codes:error_code_message(ErrCode2, #{
                <<"reason">> => iolist_to_binary(io_lib:format("~p", [Exception]))
            }),
            Context = #{
                <<"subject">> => Subject,
                <<"msg_id">> => MsgId,
                <<"exception">> => Exception,
                <<"exception_type">> => ExceptionType
            },
            router_intake_error_handler:handle_intake_error(
                ErrCode2, ErrMessage2, Subject, Payload, Headers, MsgId, Context
            )
    end.

%% Internal: Process ACK
-spec process_ack(map(), map(), binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined) -> ok.
process_ack(Ack, _Headers, _TraceIdFromHeaders, TenantIdFromHeaders, _VersionFromHeaders, MsgId) ->
    AssignmentId = maps:get(<<"assignment_id">>, Ack, undefined),
    Status = maps:get(<<"status">>, Ack, undefined),
    Message = maps:get(<<"message">>, Ack, undefined),
    
    %% Use tenant_id from headers (priority) or payload (fallback)
    TenantId = case TenantIdFromHeaders of
        undefined -> maps:get(<<"tenant_id">>, Ack, undefined);
        _ -> TenantIdFromHeaders
    end,
    
    %% Validate required fields
    case validate_ack(Ack, AssignmentId, Status) of
        ok ->
            %% Validate tenant_id against allowlist and policy registry
            ValidationContext = #{
                assignment_id => AssignmentId,
                status => Status,
                message => Message,
                source => <<"ExecAssignmentAck">>
            },
            %% Check idempotency before processing
            case router_idempotency:check_and_mark(<<"ack_id">>, AssignmentId, #{
                status => Status,
                message => Message
            }) of
                {ok, seen} ->
                    %% Already processed - skip
                    router_logger:info(<<"ACK already processed (idempotency)">>, #{
                        <<"assignment_id">> => AssignmentId
                    }),
                    emit_counter(router_acks_duplicate_total, #{
                        assignment_id => AssignmentId
                    }),
                    %% Still acknowledge message
                    case MsgId of
                        undefined -> ok;
                        MsgIdBin when is_binary(MsgIdBin) ->
                            %% Use router_jetstream for ACK with metrics
                            _ = router_jetstream:ack(#{id => MsgIdBin}),
                            cleanup_delivery_count(MsgIdBin)
                    end,
                    ok;
                {ok, not_seen} ->
                    %% Not seen before - validate tenant and process
                    case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
                        {ok, _ValidatedTenantId} ->
                            %% Tenant validated - process ACK
                            process_validated_ack(Ack, AssignmentId, Status, Message, MsgId);
                        {error, Reason, ErrorContext} ->
                            %% Tenant validation failed - emit audit event and NAK for controlled redelivery
                            log_tenant_validation_error(AssignmentId, Reason, ErrorContext),
                            emit_counter(router_acks_tenant_rejected_total, maps:merge(ErrorContext, #{
                                assignment_id => AssignmentId,
                                reason => Reason
                            })),
                            %% NAK message for controlled redelivery (respects MaxDeliver)
                            case MsgId of
                                undefined -> ok;  %% No msg_id available, cannot NAK
                                _ ->
                                    %% Check MaxDeliver exhaustion before NAK
                                    check_maxdeliver_exhaustion(MsgId, AssignmentId, ErrorContext),
                                    %% Use router_jetstream for NAK with metrics and backoff
                                    _ = router_jetstream:nak(#{id => MsgId}, tenant_validation_failed, #{
                                        assignment_id => AssignmentId,
                                        source => <<"tenant_validation">>
                                    })
                            end,
                            ok
                    end;
                {error, IdempotencyError} ->
                    router_logger:error(<<"Idempotency check failed">>, #{
                        <<"assignment_id">> => AssignmentId,
                        <<"error">> => IdempotencyError
                    }),
                    %% On idempotency error, still try to process (fail open)
                    case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
                        {ok, _ValidatedTenantId} ->
                            process_validated_ack(Ack, AssignmentId, Status, Message, MsgId);
                        {error, Reason, ErrorContext} ->
                            log_tenant_validation_error(AssignmentId, Reason, ErrorContext),
                            emit_counter(router_acks_tenant_rejected_total, maps:merge(ErrorContext, #{
                                assignment_id => AssignmentId,
                                reason => Reason
                            })),
                            case MsgId of
                                undefined -> ok;
                                _ ->
                                    %% Check MaxDeliver exhaustion before NAK
                                    check_maxdeliver_exhaustion(MsgId, AssignmentId, ErrorContext),
                                    %% Use router_jetstream for NAK with metrics and backoff
                                    _ = router_jetstream:nak(#{id => MsgId}, tenant_validation_failed, #{
                                        assignment_id => AssignmentId,
                                        source => <<"tenant_validation">>
                                    })
                            end,
                            ok
                    end
            end;
        {error, Reason} ->
            log_validation_error(AssignmentId, Reason),
            emit_counter(router_acks_validation_failed_total, #{
                assignment_id => AssignmentId,
                reason => Reason
            })
    end.

%% Internal: Process validated ACK (after tenant validation)
-spec process_validated_ack(map(), binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined) -> ok.
process_validated_ack(Ack, AssignmentId, Status, Message, MsgId) ->
    %% Extract tenant_id from ACK for trace attributes
    ValidatedTenantId = maps:get(<<"tenant_id">>, Ack, undefined),
    
    %% Log ACK
    log_ack(AssignmentId, Status, Message),
    
    %% Emit telemetry
    emit_ack_received(Status, AssignmentId),
    
    %% Set additional trace attributes for ACK processing
    router_tracing:set_span_attribute(<<"ack.status">>, Status, string),
    router_tracing:set_span_attribute(<<"ack.message">>, Message, string),
    case ValidatedTenantId of
        undefined -> ok;
        _ -> router_tracing:set_span_attribute(<<"ack.tenant_id">>, ValidatedTenantId, string)
    end,
    router_tracing:set_span_status(ok, undefined),
    
    %% Increment rejection counter if rejected
    case Status of
        <<"rejected">> ->
            emit_counter(router_assignments_rejected_total, #{
                assignment_id => AssignmentId,
                reason => Message
            });
        <<"error">> ->
            emit_counter(router_assignments_ack_error_total, #{
                assignment_id => AssignmentId,
                error => Message
            });
        _ ->
            ok
    end,
    
    %% Acknowledge JetStream message (use msg_id from message, fallback to payload)
    FinalMsgId = case MsgId of
        undefined -> maps:get(<<"msg_id">>, Ack, undefined);
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

%% Internal: Validate ACK
-spec validate_ack(map(), binary() | undefined, binary() | undefined) ->
    ok | {error, atom()}.
validate_ack(_Ack, undefined, _Status) ->
    {error, missing_assignment_id};
validate_ack(_Ack, _AssignmentId, undefined) ->
    {error, missing_status};
validate_ack(_Ack, _AssignmentId, Status) when Status =/= <<"accepted">>, Status =/= <<"rejected">>, Status =/= <<"error">> ->
    {error, invalid_status};
validate_ack(_Ack, _AssignmentId, _Status) ->
    ok.

%% Internal: Emit ACK received telemetry
-spec emit_ack_received(binary(), binary() | undefined) -> ok.
emit_ack_received(Status, AssignmentId) ->
    Metadata = #{
        status => Status,
        assignment_id => AssignmentId
    },
    emit_counter(router_acks_total, Metadata).

%% Internal: Emit telemetry counter
-spec emit_counter(atom(), map()) -> ok.
emit_counter(CounterName, Metadata) ->
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [CounterName],
        #{count => 1},
        Metadata
    ).


-spec log_tenant_validation_error(binary() | undefined, atom(), map()) -> ok.
log_tenant_validation_error(AssignmentId, Reason, Context) ->
    router_logger:warn(<<"Tenant validation failed for ExecAssignmentAck">>, maps:merge(Context, #{
        assignment_id => AssignmentId,
        validation_reason => Reason
    })).

-spec log_validation_error(binary() | undefined, atom()) -> ok.
log_validation_error(AssignmentId, Reason) ->
    router_logger:warn(<<"ExecAssignmentAck validation failed">>, #{
        <<"assignment_id">> => AssignmentId,
        <<"reason">> => Reason
    }).

-spec log_ack(binary() | undefined, binary(), binary() | undefined) -> ok.
log_ack(AssignmentId, Status, Message) ->
    router_logger:info(<<"ExecAssignmentAck received">>, #{
        <<"assignment_id">> => AssignmentId,
        <<"status">> => Status,
        <<"message">> => Message
    }).

%% Internal: Track delivery count for MaxDeliver exhaustion detection
-spec track_delivery_count(binary() | undefined) -> ok.
track_delivery_count(undefined) ->
    ok;
track_delivery_count(MsgId) ->
    %% Get or create ETS table for delivery count tracking
    Table = case ets:whereis(?DELIVERY_COUNT_TABLE) of
        undefined ->
            %% Table doesn't exist, create it (should be created in init, but handle race condition)
            NewTab = ets:new(?DELIVERY_COUNT_TABLE, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            %% INFO: initialization with options
            case erlang:function_exported(router_logger, info, 2) of
                true -> router_logger:info(<<"ACK delivery count ETS initialized">>, #{
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
            NewTab;
        ExistingTable ->
            ExistingTable
    end,
    %% Log table create/reuse
    case ets:info(?DELIVERY_COUNT_TABLE) of
        undefined -> ok;
        _Info ->
            case erlang:function_exported(router_logger, info, 2) of
                true -> router_logger:info(<<"ACK delivery count ETS ready">>, #{<<"table">> => atom_to_binary(?DELIVERY_COUNT_TABLE, utf8)});
                false -> ok
            end
    end,
    %% Increment delivery count atomically
    case ets:lookup(Table, MsgId) of
        [] ->
            ets:insert(Table, {MsgId, 1}),
            %% DEBUG: first insert
            case erlang:function_exported(router_logger, debug, 2) of
                true -> router_logger:debug(<<"ACK delivery count insert">>, #{<<"msg_id">> => MsgId, <<"delivery_count">> => 1});
                false -> ok
            end;
        [{MsgId, _Count}] ->
            NewCount = ets:update_counter(Table, MsgId, 1),
            %% DEBUG: counter increment
            case erlang:function_exported(router_logger, debug, 2) of
                true -> router_logger:debug(<<"ACK delivery count increment">>, #{<<"msg_id">> => MsgId, <<"delivery_count">> => NewCount});
                false -> ok
            end
    end,
    ok.

%% Internal: Check and emit MaxDeliver exhaustion metric
-spec check_maxdeliver_exhaustion(binary() | undefined, binary() | undefined, map()) -> ok.
check_maxdeliver_exhaustion(undefined, _AssignmentId, _ErrorContext) ->
    ok;
check_maxdeliver_exhaustion(MsgId, AssignmentId, ErrorContext) ->
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
                        msg_id => MsgId,
                        delivery_count => DeliveryCount,
                        max_deliver => MaxDeliver,
                        reason => <<"maxdeliver_exhausted">>
                    })),
                    router_logger:warn(<<"MaxDeliver exhausted for ACK message">>, #{
                        <<"msg_id">> => MsgId,
                        <<"assignment_id">> => AssignmentId,
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
                true -> router_logger:debug(<<"ACK delivery count cleanup">>, #{<<"msg_id">> => MsgId});
                false -> ok
            end
    end,
    ok.

%% Internal: Get configuration
-spec get_config(atom(), term()) -> term().
get_config(Key, Default) ->
    application:get_env(beamline_router, Key, Default).
