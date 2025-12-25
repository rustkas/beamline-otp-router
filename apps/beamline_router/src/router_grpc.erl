-module(router_grpc).

-doc "gRPC Service Implementation".
%%
%% Implements Router.Decide service from flow.proto.
%% This is the main entry point for routing requests via gRPC API.
%%
%% Key responsibilities:
%% - Request validation and decoding
%% - Rate limiting enforcement
%% - Policy lookup and evaluation
%% - Route decision execution
%% - Response encoding and error handling
%%
%% @see DEVELOPER_GUIDE.md For development workflow and code review process
%% @see SECURITY_GUIDE.md For security best practices and input validation
%% @see PERFORMANCE_GUIDE.md For performance tuning and optimization
%% @see API_DOCUMENTATION.md For gRPC API reference

-export([decide/2]).
%% Exported for testing
-export([extract_correlation_id/1]).

-include("beamline_router.hrl").
-include("flow_pb.hrl").
-include_lib("grpcbox/include/grpcbox.hrl").

%% Use centralized error mapping
-import(router_error, [to_grpc/2]).
%% Use error sanitization from router_jetstream
-import(router_jetstream, [sanitize_error_for_logging/1]).

%% Telemetry prefix for gRPC metrics
-define(TELEMETRY_PREFIX, [router_grpc]).

%% NOTE: Internal helper functions are used in complex contexts (try-catch, nested case)
%% The compiler may not detect all usages statically, but they are all called.
%% Suppress warnings for these internal functions that are definitely used:

%% Looks for 'x-correlation-id' or 'correlation-id' header
%% @dialyzer {nowarn_function, extract_correlation_id/1}
-dialyzer({nowarn_function, extract_correlation_id/1}).
extract_correlation_id(Ctx) ->
    Metadata = maps:get(metadata, Ctx, []),
    case proplists:get_value(~"x-correlation-id", Metadata) of
        undefined ->
            case proplists:get_value(~"correlation-id", Metadata) of
                undefined -> undefined;
                CorrId -> CorrId
            end;
        CorrId -> CorrId
    end.

%% 
%% Handles RouteRequest and returns RouteDecision with provider selection.
%% 
%% Request Processing:
%% 1. Extracts tenant_id and correlation_id from request/context
%% 2. Decodes RouteRequest from protobuf binary
%% 3. Checks rate limits (per-tenant/user)
%% 4. Routes message via router_core:route/2
%% 5. Encodes RouteDecision to protobuf binary
%% 6. Emits telemetry metrics (requests_total, request_duration_seconds, errors_total)
%% 
%% gRPC Error Codes:
%% - 3 (INVALID_ARGUMENT): Invalid RouteRequest, missing_message, missing_tenant_id
%% - 5 (NOT_FOUND): Policy not found
%% - 8 (RESOURCE_EXHAUSTED): Rate limit exceeded
%% - 13 (INTERNAL): Internal routing error, no_provider_available
%% - 14 (UNAVAILABLE): Timeout, NATS unavailable
%% 
%% Metadata Headers:
%% - x-correlation-id or correlation-id: Correlation ID for tracing
%% 
%% Returns: {ok, Response, Ctx} or throws {grpc_error, {Status, Message}}
%% 
%% @param Ctx gRPC context with metadata
%% @param Request Protobuf binary RouteRequest
%% @returns {ok, Response, Ctx} on success
%% @throws {grpc_error, {Status, Message}} on error
%% 
%% @see router_error:to_grpc/2 for error code mapping
%% @see router_core:route/2 for routing logic
decide(Ctx, Request) ->
    StartTime = erlang:monotonic_time(microsecond),
    Method = ~"decide",
    
    %% Extract tenant_id and correlation_id for metrics and tracing
    CorrelationId = extract_correlation_id(Ctx),
    TenantId = extract_tenant_id_from_request(Request),
    
    %% Set trace_id in process dictionary for logging
    case CorrelationId of
        undefined -> ok;
        _ when is_binary(CorrelationId) -> erlang:put(trace_id, CorrelationId);
        _ -> ok
    end,
    
    %% Initialize OTel span
    OCtx = case catch otel_ctx:get_current() of
        {'EXIT', _} -> undefined;
        C -> C
    end,
    Span = case OCtx of
        undefined -> undefined;
        _ -> catch otel_tracer:start_span(OCtx, ~"grpc.decide", #{attributes => #{
            ~"rpc.service" => ~"beamline.router.v1.Router",
            ~"rpc.method" => ~"Decide",
            ~"tenant_id" => TenantId
        }})
    end,
    try
        %% Emit request counter
        router_telemetry_helper:execute(
            ?TELEMETRY_PREFIX ++ [requests_total],
            #{count => 1},
            #{
                method => Method,
                tenant_id => TenantId
            }
        ),
        
        %% Decode RouteRequest from protobuf binary
        case decode_route_request(Request) of
            {ok, RouteRequest} ->
                %% Check rate limit before processing (if rate limiter is enabled)
                check_rate_limit_for_request(RouteRequest, TenantId, Method),
                %% Route the message with correlation_id in context
                %% Ensure RouteContext is always a map (never undefined)
                RouteContext = case CorrelationId of
                    undefined -> #{};
                    _ when is_binary(CorrelationId) -> #{correlation_id => CorrelationId};
                    _ -> #{}
                end,
                %% Accept both shapes from core: {Result, StopMetadata} or Result
                CoreResult = router_core:route(RouteRequest, RouteContext),
                case CoreResult of
                    {{ok, Decision}, _StopMetadata} ->
                        %% Convert decision to protobuf format
                        RouteDecisionPb = encode_route_decision(Decision),
                        Response = flow_pb:encode_msg(RouteDecisionPb, 'RouteDecision'),
                        
                        %% Emit success metrics
                        EndTime = erlang:monotonic_time(microsecond),
                        DurationSeconds = router_duration:duration_seconds(StartTime, EndTime),
                        
                        router_telemetry_helper:execute(
                            ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                            #{duration_seconds => DurationSeconds},
                            #{
                                method => Method,
                                tenant_id => TenantId,
                                status => ~"success"
                            }
                        ),
                        
                        %% Close span on success
                        case Span of
                            undefined -> ok;
                            _ -> catch otel_span:end_span(Span)
                        end,
                        {ok, Response, Ctx};
                {ok, Decision} ->
                        %% Convert decision to protobuf format
                        RouteDecisionPb = encode_route_decision(Decision),
                        Response = flow_pb:encode_msg(RouteDecisionPb, 'RouteDecision'),
                        
                        %% Emit success metrics
                        EndTime = erlang:monotonic_time(microsecond),
                        DurationSeconds = router_duration:duration_seconds(StartTime, EndTime),
                        
                        router_telemetry_helper:execute(
                            ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                            #{duration_seconds => DurationSeconds},
                            #{
                                method => Method,
                                tenant_id => TenantId,
                                status => ~"success"
                            }
                        ),
                        
                        %% Close span on success
                        case Span of
                            undefined -> ok;
                            _ -> catch otel_span:end_span(Span)
                        end,
                        {ok, Response, Ctx};
                    {{error, ErrorInfo}, _StopMetadata} ->
                        %% Extract error reason and context
                        {ErrorReason, ErrorContext} = case ErrorInfo of
                            {ErrReason, ErrContext} when is_map(ErrContext) ->
                                {ErrReason, ErrContext};
                            ErrReason ->
                                {ErrReason, #{}}
                        end,
                        %% Map error reason to gRPC status using centralized mapping
                        {Status, Message} = to_grpc(ErrorReason, ErrorContext),
                        
                        %% Emit error metrics
                        EndTime = erlang:monotonic_time(microsecond),
                        DurationSeconds = router_duration:duration_seconds(StartTime, EndTime),
                        
                        router_telemetry_helper:execute(
                            ?TELEMETRY_PREFIX ++ [errors_total],
                            #{count => 1},
                            #{
                                method => Method,
                                code => Status,
                                tenant_id => TenantId,
                                error_reason => atom_to_binary(ErrorReason, utf8)
                            }
                        ),
                        
                        router_telemetry_helper:execute(
                            ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                            #{duration_seconds => DurationSeconds},
                            #{
                                method => Method,
                                tenant_id => TenantId,
                                status => ~"error"
                            }
                        ),
                        
                        %% Close span on error
                        case Span of
                            undefined -> ok;
                            _ -> catch otel_span:end_span(Span)
                        end,
                        throw({grpc_error, {Status, Message}});
                    {error, ErrorInfo} ->
                        %% Extract error reason and context
                        {ErrorReason, ErrorContext} = case ErrorInfo of
                            {ErrReason, ErrContext} when is_map(ErrContext) ->
                                {ErrReason, ErrContext};
                            ErrReason ->
                                {ErrReason, #{}}
                        end,
                        %% Map error reason to gRPC status using centralized mapping
                        {Status, Message} = to_grpc(ErrorReason, ErrorContext),
                        
                        %% Emit error metrics
                        EndTime = erlang:monotonic_time(microsecond),
                        DurationSeconds = router_duration:duration_seconds(StartTime, EndTime),
                        
                        router_telemetry_helper:execute(
                            ?TELEMETRY_PREFIX ++ [errors_total],
                            #{count => 1},
                            #{
                                method => Method,
                                code => Status,
                                tenant_id => TenantId,
                                error_reason => atom_to_binary(ErrorReason, utf8)
                            }
                        ),
                        
                        router_telemetry_helper:execute(
                            ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                            #{duration_seconds => DurationSeconds},
                            #{
                                method => Method,
                                tenant_id => TenantId,
                                status => ~"error"
                            }
                        ),
                        
                        %% OTel annotate error and close span
                        case Span of
                            undefined -> ok;
                            _ -> 
                                catch otel_span:add_event(Span, ~"error", #{code => Status}),
                                catch otel_span:end_span(Span)
                        end,
                        throw({grpc_error, {Status, Message}})
                end;
            {error, invalid_request} ->
                {Status, Message} = to_grpc(invalid_request, #{}),
                
                %% Emit error metrics
                EndTime = erlang:monotonic_time(microsecond),
                DurationSeconds = router_duration:duration_seconds(StartTime, EndTime),
                
                router_telemetry_helper:execute(
                    ?TELEMETRY_PREFIX ++ [errors_total],
                    #{count => 1},
                    #{
                        method => Method,
                        code => Status,
                        tenant_id => TenantId,
                        error_reason => ~"invalid_request"
                    }
                ),
                
                router_telemetry_helper:execute(
                    ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                    #{duration_seconds => DurationSeconds},
                    #{
                        method => Method,
                        tenant_id => TenantId,
                        status => ~"error"
                    }
                ),
                
                case Span of
                    undefined -> ok;
                    _ -> 
                        catch otel_span:add_event(Span, ~"error", #{code => Status}),
                        catch otel_span:end_span(Span)
                end,
                throw({grpc_error, {Status, Message}});
            {error, missing_message} ->
                {Status, Message} = to_grpc(missing_message, #{}),
                
                %% Emit error metrics
                EndTime = erlang:monotonic_time(microsecond),
                DurationSeconds = router_duration:duration_seconds(StartTime, EndTime),
                
                router_telemetry_helper:execute(
                    ?TELEMETRY_PREFIX ++ [errors_total],
                    #{count => 1},
                    #{
                        method => Method,
                        code => Status,
                        tenant_id => TenantId,
                        error_reason => ~"missing_message"
                    }
                ),
                
                router_telemetry_helper:execute(
                    ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                    #{duration_seconds => DurationSeconds},
                    #{
                        method => Method,
                        tenant_id => TenantId,
                        status => ~"error"
                    }
                ),
                
                %% Close span on error
                case Span of
                    undefined -> ok;
                    _ -> 
                        catch otel_span:add_event(Span, ~"error", #{code => Status}),
                        catch otel_span:end_span(Span)
                end,
                throw({grpc_error, {Status, Message}})
        end
    catch
        {grpc_error, {GrpcStatus, _Message}} = Error ->
            case Span of
                undefined -> ok;
                _ -> 
                    catch otel_span:add_event(Span, ~"grpc_error", #{code => GrpcStatus}),
                    catch otel_span:end_span(Span)
            end,
            %% Emit error metrics for gRPC errors
            %% Calculate duration inside catch block (variables from try are not accessible)
            CatchEndTime = erlang:monotonic_time(microsecond),
            CatchDurationSeconds = router_duration:duration_seconds(StartTime, CatchEndTime),
            
            router_telemetry_helper:execute(
                ?TELEMETRY_PREFIX ++ [errors_total],
                #{count => 1},
                #{
                    method => Method,
                    code => GrpcStatus,
                    tenant_id => TenantId,
                    error_reason => ~"grpc_error"
                }
            ),
            
            router_telemetry_helper:execute(
                ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                #{duration_seconds => CatchDurationSeconds},
                #{
                    method => Method,
                    tenant_id => TenantId,
                    status => ~"error"
                }
            ),
            
            throw(Error);
        Class:Error:Stacktrace ->
            %% Log unexpected error with sanitization
            router_logger:error(~"Unexpected error in gRPC handler", #{
                ~"method" => Method,
                ~"tenant_id" => TenantId,
                ~"error" => Class,
                ~"reason" => sanitize_error_for_logging(Error),
                ~"event" => ~"grpc_unexpected_error"
            }),
            case Span of
                undefined -> ok;
                _ -> catch otel_span:add_event(Span, ~"exception", #{error => {Class, Error}})
            end,
            %% Emit error metrics for unexpected errors
            %% Calculate duration inside catch block (variables from try are not accessible)
            CatchEndTime = erlang:monotonic_time(microsecond),
            CatchDurationSeconds = router_duration:duration_seconds(StartTime, CatchEndTime),
            
            router_telemetry_helper:execute(
                ?TELEMETRY_PREFIX ++ [errors_total],
                #{count => 1},
                #{
                    method => Method,
                    code => ?GRPC_STATUS_INTERNAL,
                    tenant_id => TenantId,
                    error_reason => ~"unexpected_error"
                }
            ),
            
            router_telemetry_helper:execute(
                ?TELEMETRY_PREFIX ++ [request_duration_seconds],
                #{duration_seconds => CatchDurationSeconds},
                #{
                    method => Method,
                    tenant_id => TenantId,
                    status => ~"error"
                }
            ),
            
            case Span of
                undefined -> ok;
                _ -> catch otel_span:end_span(Span)
            end,
            throw_internal_error({Class, Error, Stacktrace})
    end.

%% Internal: Throw internal error
throw_internal_error(Reason) ->
    %% Sanitize error message to prevent information disclosure
    SanitizedReason = sanitize_error_for_logging(Reason),
    ErrorMsg = case is_binary(SanitizedReason) of
        true -> SanitizedReason;
        false -> iolist_to_binary(io_lib:format("~p", [SanitizedReason]))
    end,
    throw({grpc_error, {?GRPC_STATUS_INTERNAL, ErrorMsg}}).

%% Returns user_id if available in metadata, undefined otherwise
%% @dialyzer {nowarn_function, extract_user_id_from_context/1}
-dialyzer({nowarn_function, extract_user_id_from_context/1}).
extract_user_id_from_context(Ctx) ->
    Metadata = case is_map(Ctx) of
        true ->
            maps:get(metadata, Ctx, []);
        false ->
            []
    end,
    
    case proplists:get_value(~"x-user-id", Metadata) of
        undefined -> proplists:get_value(~"X-User-Id", Metadata, undefined);
        UserId -> UserId
    end.

%% Returns user_id if available in message, undefined otherwise
%% @dialyzer {nowarn_function, extract_user_id_from_request/1}
-dialyzer({nowarn_function, extract_user_id_from_request/1}).
extract_user_id_from_request(RouteRequest) ->
    try
        Message = maps:get(message, RouteRequest, #{}),
        maps:get(~"user_id", Message, undefined)
    catch
        _:_ ->
            undefined
    end.

%% Returns map with correlation fields if present
-spec extract_correlation_fields_from_request(#route_request{}) -> map().
%% @dialyzer {nowarn_function, extract_correlation_fields_from_request/1}
-dialyzer({nowarn_function, extract_correlation_fields_from_request/1}).
extract_correlation_fields_from_request(RouteRequest) ->
    try
        #route_request{message = Message, context = Context} = RouteRequest,
        %% Check message first, then context
        RunId = case maps:get(~"run_id", Message, undefined) of
            undefined -> maps:get(~"run_id", Context, undefined);
            R -> R
        end,
        FlowId = case maps:get(~"flow_id", Message, undefined) of
            undefined -> maps:get(~"flow_id", Context, undefined);
            F -> F
        end,
        StepId = case maps:get(~"step_id", Message, undefined) of
            undefined -> maps:get(~"step_id", Context, undefined);
            S -> S
        end,
        %% Build correlation context
        Correlation = #{},
        Correlation1 = case RunId of
            undefined -> Correlation;
            _ -> maps:put(~"run_id", RunId, Correlation)
        end,
        Correlation2 = case FlowId of
            undefined -> Correlation1;
            _ -> maps:put(~"flow_id", FlowId, Correlation1)
        end,
        Correlation3 = case StepId of
            undefined -> Correlation2;
            _ -> maps:put(~"step_id", StepId, Correlation2)
        end,
        Correlation3
    catch
        _:_ ->
            #{}
    end.

%% Returns tenant_id if available in message, undefined otherwise
%% @dialyzer {nowarn_function, extract_tenant_id_from_request/1}
-dialyzer({nowarn_function, extract_tenant_id_from_request/1}).
extract_tenant_id_from_request(Request) ->
    try
        case decode_route_request_internal(Request) of
            {ok, RouteRequest} ->
                Message = maps:get(message, RouteRequest, #{}),
                maps:get(~"tenant_id", Message, undefined);
            _ ->
                undefined
        end
    catch
        _:_ ->
            undefined
    end.

%% Internal: Decode RouteRequest from protobuf binary
%% @dialyzer {nowarn_function, decode_route_request/1}
-dialyzer({nowarn_function, decode_route_request/1}).
decode_route_request(Request) when is_binary(Request) ->
    decode_route_request_internal(Request);
decode_route_request(_) ->
    {error, invalid_request}.

%% Internal: Decode RouteRequest (extracted for reuse)
%% @dialyzer {nowarn_function, decode_route_request_internal/1}
-dialyzer({nowarn_function, decode_route_request_internal/1}).
decode_route_request_internal(Request) when is_binary(Request) ->
    try
        RouteRequestPb = flow_pb:decode_msg(Request, 'RouteRequest'),
        convert_route_request(RouteRequestPb)
    catch
        _:_ ->
            {error, invalid_request}
    end.

%% Internal: Convert Protobuf RouteRequest to internal record
%% @dialyzer {nowarn_function, convert_route_request/1}
-dialyzer({nowarn_function, convert_route_request/1}).
convert_route_request(#'RouteRequest'{message = undefined}) ->
    {error, missing_message};
convert_route_request(#'RouteRequest'{message = MessagePb, policy_id = PolicyId, context = ContextPb}) ->
    %% Convert Protobuf Message to map
    Message = convert_message(MessagePb),
    
    %% Convert Protobuf context (list of {key, value} tuples) to map
    %% Ensure Context is always a map (never undefined)
    Context = case convert_context(ContextPb) of
        C when is_map(C) -> C;
        _ -> #{}
    end,
    
    RouteRequest = #route_request{
        message = Message,
        policy_id = PolicyId,
        context = Context
    },
    {ok, RouteRequest}.

%% Internal: Convert Protobuf Message to map
%% @dialyzer {nowarn_function, convert_message/1}
-dialyzer({nowarn_function, convert_message/1}).
convert_message(#'Message'{
    message_id = MessageId,
    tenant_id = TenantId,
    trace_id = TraceId,
    message_type = MessageType,
    payload = Payload,
    metadata = MetadataPb,
    timestamp_ms = TimestampMs
}) ->
    %% Convert Protobuf metadata (list of {key, value} tuples) to map
    Metadata = convert_metadata(MetadataPb),
    
    #{
        ~"message_id" => MessageId,
        ~"tenant_id" => TenantId,
        ~"trace_id" => TraceId,
        ~"message_type" => MessageType,
        ~"payload" => Payload,
        ~"metadata" => Metadata,
        ~"timestamp_ms" => TimestampMs
    }.

%% Internal: Convert Protobuf context to map
%% @dialyzer {nowarn_function, convert_context/1}
-dialyzer({nowarn_function, convert_context/1}).
convert_context([]) ->
    #{};
convert_context(ContextPb) when is_list(ContextPb) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            maps:put(Key, Value, Acc)
        end,
        #{},
        ContextPb
    );
convert_context(_) ->
    #{}.

%% Internal: Convert Protobuf metadata to map
%% @dialyzer {nowarn_function, convert_metadata/1}
-dialyzer({nowarn_function, convert_metadata/1}).
convert_metadata([]) ->
    #{};
convert_metadata(MetadataPb) when is_list(MetadataPb) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            maps:put(Key, Value, Acc)
        end,
        #{},
        MetadataPb
    );
convert_metadata(_) ->
    #{}.

%% Internal: Encode RouteDecision to Protobuf record
%% @dialyzer {nowarn_function, encode_route_decision/1}
-dialyzer({nowarn_function, encode_route_decision/1}).
encode_route_decision(Decision) ->
    #route_decision{
        provider_id = ProviderId,
        reason = Reason,
        priority = Priority,
        expected_latency_ms = Latency,
        expected_cost = Cost,
        metadata = Metadata
    } = Decision,
    
    %% Convert metadata map to Protobuf format (list of {key, value} tuples)
    MetadataPb = convert_map_to_proto_metadata(Metadata),
    
    #'RouteDecision'{
        provider_id = ProviderId,
        reason = Reason,
        priority = Priority,
        expected_latency_ms = Latency,
        expected_cost = Cost,
        metadata = MetadataPb
    }.

%% Internal: Convert map to Protobuf metadata format
%% @dialyzer {nowarn_function, convert_map_to_proto_metadata/1}
-dialyzer({nowarn_function, convert_map_to_proto_metadata/1}).
convert_map_to_proto_metadata(Metadata) when is_map(Metadata) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            [{Key, Value} | Acc]
        end,
        [],
        Metadata
    );
convert_map_to_proto_metadata(_) ->
    [].

%% Internal: Check rate limit for request
-spec check_rate_limit_for_request(#route_request{}, binary(), binary()) -> ok.
check_rate_limit_for_request(RouteRequest, TenantId, Method) ->
    case whereis(router_rate_limiter) of
        undefined ->
            %% Rate limiter not started, skip check
            ok;
        _ ->
            check_rate_limit_with_correlation(RouteRequest, TenantId, Method)
    end.

%% Internal: Check rate limit with correlation fields
-spec check_rate_limit_with_correlation(#route_request{}, binary(), binary()) -> ok.
check_rate_limit_with_correlation(RouteRequest, TenantId, Method) ->
    CorrelationFields = extract_correlation_fields_from_request(RouteRequest),
    try
        UserId = extract_user_id_from_request(RouteRequest),
        handle_rate_limit_check(UserId, TenantId, Method, CorrelationFields)
    catch
        exit:{noproc, _} ->
            log_rate_limit_error(~"Rate limiter not running, allowing request", TenantId, CorrelationFields),
            ok;
        exit:{timeout, _} ->
            log_rate_limit_error(~"Rate limit check timeout, allowing request", TenantId, CorrelationFields),
            ok;
        RateLimitErrorClass:RateLimitErrorReason ->
            ErrorContext = maps:merge(CorrelationFields, #{
                ~"tenant_id" => TenantId,
                ~"error" => {RateLimitErrorClass, RateLimitErrorReason}
            }),
            router_logger:warn(~"Rate limit check error, allowing request", ErrorContext),
            ok
    end.

%% Internal: Handle rate limit check result
-spec handle_rate_limit_check(binary(), binary(), binary(), map()) -> ok.
handle_rate_limit_check(UserId, TenantId, Method, CorrelationFields) ->
    case router_rate_limiter:check_rate_limit(TenantId, ~"decide", UserId) of
        {ok, _Remaining} ->
            %% Rate limit check passed, continue
            ok;
        {error, rate_limit_exceeded, _Remaining} ->
            %% Rate limit exceeded, return error
            router_telemetry_helper:execute(
                ?TELEMETRY_PREFIX ++ [rate_limit_violations_total],
                #{count => 1},
                #{
                    method => Method,
                    tenant_id => TenantId,
                    user_id => UserId
                }
            ),
            LogContext = maps:merge(CorrelationFields, #{
                ~"tenant_id" => TenantId,
                ~"user_id" => UserId,
                ~"method" => Method
            }),
            router_logger:warn(~"Rate limit exceeded", LogContext),
            throw({grpc_error, {8, ~"Rate limit exceeded"}});  %% 8 = RESOURCE_EXHAUSTED
        {error, Other} ->
            %% Other error, log but continue (fail open)
            ErrorContext = maps:merge(CorrelationFields, #{
                ~"tenant_id" => TenantId,
                ~"user_id" => UserId,
                ~"error" => Other
            }),
            router_logger:warn(~"Rate limit check error, allowing request", ErrorContext),
            ok
    end.

%% Internal: Log rate limit error
-spec log_rate_limit_error(binary(), binary(), map()) -> ok.
log_rate_limit_error(Message, TenantId, CorrelationFields) ->
    LogContext = maps:merge(CorrelationFields, #{
        ~"tenant_id" => TenantId
    }),
    router_logger:warn(Message, LogContext).
