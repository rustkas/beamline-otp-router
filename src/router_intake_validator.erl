%% @doc Intake Validator
%% Unified validation layer for all incoming Router messages
%% CP2+: Schema, version, and correlation field validation
-module(router_intake_validator).
-ignore_xref([
    router_intake_validator,
    {router_intake_validator, validate_intake_message, 4},
    {router_intake_validator, validate_schema, 2},
    {router_intake_validator, validate_version, 3},
    {router_intake_validator, validate_correlation_fields, 2},
    {router_intake_validator, validate_uuid_v4, 1},
    {router_intake_validator, validate_ulid, 1},
    {router_intake_validator, validate_w3c_trace_context, 1},
    {router_intake_validator, validate_decide_specific_fields, 1},
    {router_intake_validator, validate_tenant, 2},
    {router_intake_validator, validate_idempotency, 3}
]).

-export([validate_intake_message/4]).
-export([validate_schema/2, validate_version/3, validate_correlation_fields/2]).
-export([validate_uuid_v4/1, validate_ulid/1, validate_w3c_trace_context/1]).
-export([validate_decide_specific_fields/1, validate_tenant/2, validate_idempotency/3]).

-include("beamline_router.hrl").
-include("flow_pb.hrl").
%% Include worker_pb.hrl when protobuf code is generated
%% -include("worker_pb.hrl").
%% Optional worker protobuf include
-ifdef(WORKER_PB).
-include("worker_pb.hrl").
-endif.

-define(SUPPORTED_VERSIONS, [<<"1">>]).

%% Message types
-type message_type() :: decide | result | ack.

%% @doc Main validation function
-spec validate_intake_message(
    Subject :: binary(),
    Payload :: binary(),
    Headers :: map(),
    MessageType :: message_type()
) -> {ok, map()} | {error, {router_intake_error_codes:error_code(), binary(), map()}}.
validate_intake_message(Subject, Payload, Headers, MessageType) ->
    try
        validate_intake_message_steps(Subject, Payload, Headers, MessageType)
    catch
        ExceptionType:Exception ->
            {error, {
                internal_validation_error,
                router_intake_error_codes:error_code_message(internal_validation_error, #{
                    <<"reason">> => iolist_to_binary(io_lib:format("~p", [Exception]))
                }),
                #{
                    <<"validation_stage">> => <<"internal">>,
                    <<"subject">> => Subject,
                    <<"exception">> => Exception,
                    <<"exception_type">> => ExceptionType
                }
            }}
    end.

%% Internal: Step-by-step validation pipeline
-spec validate_intake_message_steps(binary(), binary(), map(), message_type()) ->
    {ok, map()} | {error, {router_intake_error_codes:error_code(), binary(), map()}}.
validate_intake_message_steps(Subject, Payload, Headers, MessageType) ->
    %% Step 1: Schema validation
    case validate_schema(Payload, MessageType) of
        {error, Reason} ->
            build_validation_error(schema_validation_failed, Subject, <<"schema">>, #{
                <<"reason">> => atom_to_binary(Reason, utf8)
            });
        {ok, DecodedMessage} ->
            validate_version_step(Subject, DecodedMessage, Headers, MessageType)
    end.

%% Internal: Version validation step
-spec validate_version_step(binary(), map(), map(), message_type()) ->
    {ok, map()} | {error, {router_intake_error_codes:error_code(), binary(), map()}}.
validate_version_step(Subject, DecodedMessage, Headers, MessageType) ->
    case validate_version(Subject, DecodedMessage, Headers) of
        {error, Reason} ->
            Version = maps:get(<<"version">>, DecodedMessage, <<"unknown">>),
            build_validation_error(version_unsupported, Subject, <<"version">>, #{
                <<"version">> => Version,
                <<"supported_versions">> => ?SUPPORTED_VERSIONS,
                <<"reason">> => atom_to_binary(Reason, utf8)
            });
        {ok, Version} ->
            validate_correlation_step(Subject, DecodedMessage, MessageType, Version)
    end.

%% Internal: Correlation fields validation step
-spec validate_correlation_step(binary(), map(), message_type(), binary()) ->
    {ok, map()} | {error, {router_intake_error_codes:error_code(), binary(), map()}}.
validate_correlation_step(Subject, DecodedMessage, MessageType, Version) ->
    case validate_correlation_fields(DecodedMessage, MessageType) of
        {error, Reason} ->
            build_validation_error(correlation_fields_invalid, Subject, <<"correlation">>, #{
                <<"version">> => Version,
                <<"reason">> => atom_to_binary(Reason, utf8)
            });
        {ok, CorrelationContext} ->
            validate_message_specific_step(Subject, DecodedMessage, MessageType, CorrelationContext, Version)
    end.

%% Internal: Message-specific validation step
-spec validate_message_specific_step(binary(), map(), message_type(), map(), binary()) ->
    {ok, map()} | {error, {router_intake_error_codes:error_code(), binary(), map()}}.
validate_message_specific_step(Subject, DecodedMessage, MessageType, CorrelationContext, Version) ->
    case validate_message_specific_fields(DecodedMessage, MessageType, CorrelationContext) of
        {error, Reason} ->
            build_validation_error(correlation_fields_invalid, Subject, <<"message_specific">>, #{
                <<"version">> => Version,
                <<"reason">> => atom_to_binary(Reason, utf8)
            });
        {ok, AdditionalContext} ->
            %% All validations passed - merge all contexts
            MergedContext = maps:merge(CorrelationContext, AdditionalContext),
            {ok, maps:merge(DecodedMessage, MergedContext)}
    end.

%% Internal: Build validation error response
-spec build_validation_error(atom(), binary(), binary(), map()) ->
    {error, {router_intake_error_codes:error_code(), binary(), map()}}.
build_validation_error(ErrorCode, Subject, Stage, ExtraContext) ->
    ErrorMessage = router_intake_error_codes:error_code_message(ErrorCode, ExtraContext),
    Context = maps:merge(ExtraContext, #{
        <<"validation_stage">> => Stage,
        <<"subject">> => Subject
    }),
    {error, {ErrorCode, ErrorMessage, Context}}.

%% @doc Validate message schema (protobuf decode for decide, JSON for results/ack)
-spec validate_schema(binary(), message_type()) -> {ok, map()} | {error, atom()}.
validate_schema(Payload, decide) ->
    %% Protobuf decode for decide messages
    try
        RouteRequestPb = flow_pb:decode_msg(Payload, 'RouteRequest'),
        case convert_route_request_to_map(RouteRequestPb) of
            {ok, MessageMap} ->
                {ok, MessageMap};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ ->
            %% Try JSON decode as fallback (for backward compatibility)
            try
                case jsx:decode(Payload, [return_maps]) of
                    Message when is_map(Message) ->
                        {ok, Message};
                    _ ->
                        {error, invalid_format}
                end
            catch
                _:_ ->
                    {error, invalid_format}
            end
    end;
validate_schema(Payload, result) ->
    %% Protobuf decode for result messages (with JSON fallback for backward compatibility)
    try
        %% Try protobuf decode first (when worker_pb module is available)
        case code:which(worker_pb) of
            non_existing ->
                %% Protobuf code not generated yet, use JSON decode
                validate_schema_json(Payload);
            _ ->
                %% Protobuf code available, try protobuf decode
                try
                    ExecResultPb = worker_pb:decode_msg(Payload, 'ExecResult'),
                    case convert_exec_result_to_map(ExecResultPb) of
                        {ok, MessageMap} -> {ok, MessageMap};
                        {error, _Reason} -> validate_schema_json(Payload)
                    end
                catch
                    _:_ -> validate_schema_json(Payload)
                end
        end
    catch
        _:_ -> {error, decode_failed}
    end;
validate_schema(Payload, ack) ->
    %% Protobuf decode for ack messages (with JSON fallback for backward compatibility)
    try
        %% Try protobuf decode first (when worker_pb module is available)
        case code:which(worker_pb) of
            non_existing ->
                %% Protobuf code not generated yet, use JSON decode
                validate_schema_json(Payload);
            _ ->
                %% Protobuf code available, try protobuf decode
                try
                    ExecAssignmentAckPb = worker_pb:decode_msg(Payload, 'ExecAssignmentAck'),
                    case convert_exec_assignment_ack_to_map(ExecAssignmentAckPb) of
                        {ok, MessageMap} -> {ok, MessageMap};
                        {error, _Reason} -> validate_schema_json(Payload)
                    end
                catch
                    _:_ -> validate_schema_json(Payload)
                end
        end
    catch
        _:_ -> {error, decode_failed}
    end.

%% @doc Validate schema version
-spec validate_version(binary(), map(), map()) -> {ok, binary()} | {error, atom()}.
validate_version(Subject, Message, Headers) ->
    %% Extract version from subject or payload
    VersionFromSubject = extract_version_from_subject(Subject),
    VersionFromPayload = maps:get(<<"version">>, Message, undefined),
    VersionFromHeaders = maps:get(<<"version">>, Headers, undefined),
    
    %% Priority: Headers > Payload > Subject
    Version = case VersionFromHeaders of
        undefined ->
            case VersionFromPayload of
                undefined -> VersionFromSubject;
                V -> V
            end;
        V -> V
    end,
    
    %% Validate version
    case Version of
        undefined -> {error, missing_version};
        <<"1">> -> {ok, <<"1">>};
        _ -> {error, unsupported_version}
    end.

%% @doc Validate correlation fields
-spec validate_correlation_fields(map(), message_type()) -> {ok, map()} | {error, atom()}.
validate_correlation_fields(Message, _MessageType) ->
    %% Extract correlation fields
    TenantId = maps:get(<<"tenant_id">>, Message, undefined),
    RunId = maps:get(<<"run_id">>, Message, undefined),
    FlowId = maps:get(<<"flow_id">>, Message, undefined),
    StepId = maps:get(<<"step_id">>, Message, undefined),
    IdempotencyKey = maps:get(<<"idempotency_key">>, Message, undefined),
    TraceId = maps:get(<<"trace_id">>, Message, undefined),
    
    %% Validate tenant_id (required for all message types)
    case TenantId of
        undefined ->
            {error, missing_tenant_id};
        <<>> ->
            {error, empty_tenant_id};
        _ ->
            %% Validate correlation field dependencies
            case validate_correlation_dependencies(RunId, FlowId, StepId) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    %% Validate formats (if fields present)
                    case validate_field_formats(RunId, FlowId, StepId, TraceId, IdempotencyKey) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            %% Build correlation context
                            CorrelationContext = #{
                                <<"tenant_id">> => TenantId,
                                <<"run_id">> => RunId,
                                <<"flow_id">> => FlowId,
                                <<"step_id">> => StepId,
                                <<"idempotency_key">> => IdempotencyKey,
                                <<"trace_id">> => TraceId
                            },
                            {ok, CorrelationContext}
                    end
            end
    end.

%% Internal functions

%% Extract version from subject (e.g., "beamline.router.v1.decide" -> "1")
extract_version_from_subject(Subject) ->
    case re:run(Subject, <<"\.v([0-9]+)\.">>, [{capture, [1], binary}]) of
        {match, [Version]} ->
            Version;
        _ ->
            undefined
    end.

%% Validate correlation field dependencies
validate_correlation_dependencies(RunId, FlowId, StepId) ->
    %% If run_id present, flow_id and step_id must be present
    case {RunId, FlowId, StepId} of
        {R, undefined, _} when R =/= undefined ->
            {error, flow_id_required_when_run_id_present};
        {R, _, undefined} when R =/= undefined ->
            {error, step_id_required_when_run_id_present};
        {undefined, F, _} when F =/= undefined ->
            {error, run_id_required_when_flow_id_present};
        {undefined, _, S} when S =/= undefined ->
            {error, run_id_required_when_step_id_present};
        {_, undefined, S} when S =/= undefined ->
            {error, flow_id_required_when_step_id_present};
        _ ->
            ok
    end.

%% Validate field formats (UUID v4, ULID, W3C Trace Context)
-spec validate_field_formats(binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined, binary() | undefined) -> ok | {error, atom()}.
validate_field_formats(RunId, FlowId, StepId, TraceId, IdempotencyKey) ->
    %% Validate run_id format (UUID v4 or ULID)
    RunIdResult = case RunId of
        undefined -> ok;
        _ ->
            case validate_uuid_v4(RunId) of
                ok -> ok;
                {error, _} ->
                    case validate_ulid(RunId) of
                        ok -> ok;
                        {error, _} -> {error, invalid_run_id_format}
                    end
            end
    end,
    case RunIdResult of
        {error, _} -> RunIdResult;
        ok ->
            %% Validate flow_id format (UUID v4 or ULID)
            FlowIdResult = case FlowId of
                undefined -> ok;
                _ ->
                    case validate_uuid_v4(FlowId) of
                        ok -> ok;
                        {error, _} ->
                            case validate_ulid(FlowId) of
                                ok -> ok;
                                {error, _} -> {error, invalid_flow_id_format}
                            end
                    end
            end,
            case FlowIdResult of
                {error, _} -> FlowIdResult;
                ok ->
                    %% Validate step_id format (UUID v4 or ULID)
                    StepIdResult = case StepId of
                        undefined -> ok;
                        _ ->
                            case validate_uuid_v4(StepId) of
                                ok -> ok;
                                {error, _} ->
                                    case validate_ulid(StepId) of
                                        ok -> ok;
                                        {error, _} -> {error, invalid_step_id_format}
                                    end
                            end
                    end,
                    case StepIdResult of
                        {error, _} -> StepIdResult;
                        ok ->
                            %% Validate trace_id format (W3C Trace Context or UUID v4)
                            TraceIdResult = case TraceId of
                                undefined -> ok;
                                _ ->
                                    case validate_w3c_trace_context(TraceId) of
                                        ok -> ok;
                                        {error, _} ->
                                            case validate_uuid_v4(TraceId) of
                                                ok -> ok;
                                                {error, _} -> {error, invalid_trace_id_format}
                                            end
                                    end
                            end,
                            case TraceIdResult of
                                {error, _} -> TraceIdResult;
                                ok ->
                                    %% Validate idempotency_key format (non-empty string, max 256 chars)
                                    case IdempotencyKey of
                                        undefined -> ok;
                                        <<>> -> {error, empty_idempotency_key};
                                        Key when is_binary(Key) ->
                                            case byte_size(Key) > 256 of
                                                true -> {error, idempotency_key_too_long};
                                                false -> ok
                                            end;
                                        _ -> {error, invalid_idempotency_key_format}
                                    end
                            end
                    end
            end
    end.

%% Convert Protobuf RouteRequest to map (for decide messages)
convert_route_request_to_map(#'RouteRequest'{message = undefined}) ->
    {error, missing_message};
convert_route_request_to_map(#'RouteRequest'{message = MessagePb, policy_id = PolicyId, context = ContextPb}) ->
    %% Convert Protobuf Message to map
    Message = convert_message_to_map(MessagePb),
    
    %% Convert Protobuf context (list of {key, value} tuples) to map
    Context = case ContextPb of
        [] -> #{};
        ContextList when is_list(ContextList) ->
            lists:foldl(
                fun({Key, Value}, Acc) ->
                    maps:put(Key, Value, Acc)
                end,
                #{},
                ContextList
            );
        _ -> #{}
    end,
    
    %% Build DecideRequest-like map (for compatibility with existing code)
    RequestMap = maps:merge(Message, #{
        <<"policy_id">> => PolicyId,
        <<"context">> => Context,
        <<"version">> => <<"1">>  %% Add version for compatibility
    }),
    
    {ok, RequestMap}.

%% Convert Protobuf Message to map
convert_message_to_map(#'Message'{
    message_id = MessageId,
    tenant_id = TenantId,
    trace_id = TraceId,
    message_type = MessageType,
    payload = Payload,
    metadata = MetadataPb,
    timestamp_ms = TimestampMs
}) ->
    %% Convert Protobuf metadata (list of {key, value} tuples) to map
    Metadata = case MetadataPb of
        [] -> #{};
        MetadataList when is_list(MetadataList) ->
            lists:foldl(
                fun({Key, Value}, Acc) ->
                    maps:put(Key, Value, Acc)
                end,
                #{},
                MetadataList
            );
        _ -> #{}
    end,
    
    %% Build message map (compatible with existing DecideRequest format)
    #{
        <<"message_id">> => MessageId,
        <<"tenant_id">> => TenantId,
        <<"trace_id">> => TraceId,
        <<"message_type">> => MessageType,
        <<"payload">> => Payload,
        <<"metadata">> => Metadata,
        <<"timestamp_ms">> => TimestampMs
    }.

%% @doc Validate UUID v4 format
-spec validate_uuid_v4(binary()) -> ok | {error, atom()}.
validate_uuid_v4(UUID) when is_binary(UUID) ->
    %% UUID v4 format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
    %% where x is hex digit, y is one of 8, 9, a, b
    case re:run(UUID, <<"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>, [{capture, none}, caseless]) of
        match -> ok;
        _ -> {error, invalid_uuid_v4_format}
    end;
validate_uuid_v4(_) ->
    {error, not_binary}.

%% @doc Validate ULID format
-spec validate_ulid(binary()) -> ok | {error, atom()}.
validate_ulid(ULID) when is_binary(ULID) ->
    %% ULID format: 26 characters, base32 encoded (0-9, A-Z, excluding I, L, O, U)
    %% Length: exactly 26 characters
    case byte_size(ULID) =:= 26 of
        false -> {error, invalid_ulid_length};
        true ->
            %% Check base32 characters (0-9, A-Z, excluding I, L, O, U)
            case re:run(ULID, <<"^[0-9A-HJ-KM-NP-TV-Z]{26}$">>, [{capture, none}]) of
                match -> ok;
                _ -> {error, invalid_ulid_format}
            end
    end;
validate_ulid(_) ->
    {error, not_binary}.

%% @doc Validate W3C Trace Context format
-spec validate_w3c_trace_context(binary()) -> ok | {error, atom()}.
validate_w3c_trace_context(TraceId) when is_binary(TraceId) ->
    %% W3C Trace Context format: 32 hex characters (16 bytes)
    case byte_size(TraceId) =:= 32 of
        false -> {error, invalid_w3c_trace_context_length};
        true ->
            %% Check hex characters (0-9, a-f, A-F)
            case re:run(TraceId, <<"^[0-9a-fA-F]{32}$">>, [{capture, none}]) of
                match -> ok;
                _ -> {error, invalid_w3c_trace_context_format}
            end
    end;
validate_w3c_trace_context(_) ->
    {error, not_binary}.

%% @doc Validate message-type specific fields
-spec validate_message_specific_fields(map(), message_type(), map()) -> {ok, map()} | {error, atom()}.
validate_message_specific_fields(Message, decide, CorrelationContext) ->
    %% Validate decide-specific fields
    case validate_decide_specific_fields(Message) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            %% Validate tenant (ACL/allowlist check)
            TenantId = maps:get(<<"tenant_id">>, CorrelationContext),
            case validate_tenant(TenantId, CorrelationContext) of
                {error, Reason, TenantContext} ->
                    _ = TenantContext,  %% TenantContext may be used in future
                    {error, Reason};
                {ok, ValidatedTenantId} ->
                    _ = ValidatedTenantId,  %% ValidatedTenantId used implicitly in validation
                    %% Validate idempotency (if key present)
                    IdempotencyKey = maps:get(<<"idempotency_key">>, CorrelationContext),
                    case validate_idempotency(IdempotencyKey, Message, CorrelationContext) of
                        {error, Reason} ->
                            {error, Reason};
                        {ok, IdempotencyStatus} ->
                            %% Add idempotency status to context
                            AdditionalContext = #{
                                <<"idempotency_status">> => IdempotencyStatus
                            },
                            {ok, AdditionalContext}
                    end
            end
    end;
validate_message_specific_fields(_Message, result, _CorrelationContext) ->
    %% For result messages, tenant validation is done in router_result_consumer
    %% Idempotency is checked separately for results
    {ok, #{}};
validate_message_specific_fields(_Message, ack, _CorrelationContext) ->
    %% For ack messages, tenant validation is done in router_ack_consumer
    %% Idempotency is checked separately for acks
    {ok, #{}}.

%% @doc Validate decide-specific required fields
-spec validate_decide_specific_fields(map()) -> ok | {error, atom()}.
validate_decide_specific_fields(Message) ->
    %% Required fields for DecideRequest:
    %% - request_id (required)
    %% - task.type (required)
    %% - task.payload_ref OR task.payload (at least one must be present)
    
    RequestId = maps:get(<<"request_id">>, Message, undefined),
    Task = maps:get(<<"task">>, Message, undefined),
    
    %% Validate request_id
    case RequestId of
        undefined ->
            {error, missing_request_id};
        <<>> ->
            {error, empty_request_id};
        _ when is_binary(RequestId) ->
            %% Validate task structure
            case Task of
                undefined ->
                    {error, missing_task};
                TaskMap when is_map(TaskMap) ->
                    TaskType = maps:get(<<"type">>, TaskMap, undefined),
                    PayloadRef = maps:get(<<"payload_ref">>, TaskMap, undefined),
                    Payload = maps:get(<<"payload">>, TaskMap, undefined),
                    
                    %% Validate task.type
                    case TaskType of
                        undefined ->
                            {error, missing_task_type};
                        <<>> ->
                            {error, empty_task_type};
                        _ when is_binary(TaskType) ->
                            %% Validate that either payload_ref or payload is present
                            case {PayloadRef, Payload} of
                                {undefined, undefined} ->
                                    {error, missing_task_payload};
                                _ ->
                                    %% Validate payload content (empty payload detection)
                                    case validate_payload_content(PayloadRef, Payload) of
                                        ok -> ok;
                                        {error, payload_too_small} ->
                                            {error, payload_too_small};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end
                            end;
                        _ ->
                            {error, invalid_task_type}
                    end;
                _ ->
                    {error, invalid_task_format}
            end;
        _ ->
            {error, invalid_request_id_format}
    end.

%% @doc Validate payload content (empty payload detection)
-spec validate_payload_content(binary() | undefined, map() | undefined) -> ok | {error, atom()}.
validate_payload_content(undefined, Payload) when is_map(Payload) ->
    %% Payload is present - check if it's empty or too small
    PayloadJson = jsx:encode(Payload),
    PayloadSize = byte_size(PayloadJson),
    MinPayloadSize = application:get_env(beamline_router, min_payload_size, 10),  %% Default: 10 bytes
    
    case PayloadSize < MinPayloadSize of
        true ->
            {error, payload_too_small};
        false ->
            ok
    end;
validate_payload_content(PayloadRef, undefined) when is_binary(PayloadRef) ->
    %% Payload ref is present - payload content validation not applicable
    ok;
validate_payload_content(undefined, undefined) ->
    %% Neither payload nor payload_ref - should not reach here (caught earlier)
    {error, missing_task_payload};
validate_payload_content(_PayloadRef, Payload) when is_map(Payload) ->
    %% Both payload_ref and payload present - validate payload content
    PayloadJson = jsx:encode(Payload),
    PayloadSize = byte_size(PayloadJson),
    MinPayloadSize = application:get_env(beamline_router, min_payload_size, 10),  %% Default: 10 bytes
    
    case PayloadSize < MinPayloadSize of
        true ->
            {error, payload_too_small};
        false ->
            ok
    end;
validate_payload_content(_PayloadRef, _Payload) ->
    %% Invalid payload format
    {error, invalid_payload_format}.

%% @doc Validate tenant (ACL/allowlist check)
-spec validate_tenant(binary(), map()) -> {ok, binary()} | {error, atom(), map()}.
validate_tenant(TenantId, Context) ->
    %% Use router_tenant_validator for tenant validation
    case erlang:function_exported(router_tenant_validator, validate_tenant, 2) of
        true ->
            router_tenant_validator:validate_tenant(TenantId, Context);
        false ->
            %% Fallback: basic validation if router_tenant_validator not available
            case TenantId of
                undefined -> {error, tenant_missing, Context};
                <<>> -> {error, tenant_empty, Context};
                _ when is_binary(TenantId) -> {ok, TenantId};
                _ -> {error, tenant_invalid_format, Context}
            end
    end.

%% @doc Validate idempotency (check and mark)
-spec validate_idempotency(binary() | undefined, map(), map()) -> {ok, new | duplicate} | {error, atom()}.
validate_idempotency(undefined, _Message, _Context) ->
    %% Idempotency key optional for CP1 baseline
    {ok, new};
validate_idempotency(IdempotencyKey, Message, Context) when is_binary(IdempotencyKey) ->
    %% Check idempotency using router_idempotency
    case erlang:function_exported(router_idempotency, check_and_mark, 3) of
        true ->
            %% Build additional data for idempotency check
            AdditionalData = #{
                tenant_id => maps:get(<<"tenant_id">>, Context, undefined),
                request_id => maps:get(<<"request_id">>, Message, undefined),
                task_type => get_task_type(Message)
            },
            
            case router_idempotency:check_and_mark(<<"intake_id">>, IdempotencyKey, AdditionalData) of
                {ok, not_seen} ->
                    {ok, new};
                {ok, seen} ->
                    {ok, duplicate};
                {error, Reason} ->
                    {error, {idempotency_check_failed, Reason}}
            end;
        false ->
            %% Fallback: if router_idempotency not available, treat as new
            {ok, new}
    end;
validate_idempotency(_IdempotencyKey, _Message, _Context) ->
    {error, invalid_idempotency_key_format}.

%% Internal: Get task type from message
get_task_type(Message) ->
    case maps:get(<<"task">>, Message, undefined) of
        Task when is_map(Task) ->
            maps:get(<<"type">>, Task, undefined);
        _ ->
            undefined
    end.

%% @doc JSON decode helper (used as fallback for result/ack messages)
-spec validate_schema_json(binary()) -> {ok, map()} | {error, atom()}.
validate_schema_json(Payload) ->
    try
        case jsx:decode(Payload, [return_maps]) of
            Message when is_map(Message) ->
                {ok, Message};
            _ ->
                {error, invalid_json_format}
        end
    catch
        _:_ ->
            {error, json_decode_failed}
    end.

%% @doc Convert ExecResult protobuf to map
%% Called when worker_pb module is available and protobuf decode succeeds
-ifdef(WORKER_PB).
-spec convert_exec_result_to_map(term()) -> {ok, map()} | {error, atom()}.
convert_exec_result_to_map(ExecResultPb) when is_record(ExecResultPb, 'ExecResult') ->
    try
        #'ExecResult'{
            assignment_id = AssignmentId,
            request_id = RequestId,
            status = Status,
            provider_id = ProviderId,
            job = JobPb,
            latency_ms = LatencyMs,
            cost = Cost,
            trace_id = TraceId,
            tenant_id = TenantId,
            timestamp = Timestamp,
            error_code = ErrorCode,
            error_message = ErrorMessage
        } = ExecResultPb,
        
        %% Convert Job protobuf to map
        Job = case JobPb of
            undefined -> #{};
            #'Job'{type = JobType, metadata = JobMetadataPb} ->
                JobMetadata = case JobMetadataPb of
                    [] -> #{};
                    MetadataList when is_list(MetadataList) ->
                        lists:foldl(
                            fun({Key, Value}, Acc) ->
                                maps:put(Key, Value, Acc)
                            end,
                            #{},
                            MetadataList
                        );
                    _ -> #{}
                end,
                #{
                    <<"type">> => JobType,
                    <<"metadata">> => JobMetadata
                }
        end,
        
        %% Build ExecResult map (compatible with existing JSON format)
        ResultMap = #{
            <<"assignment_id">> => AssignmentId,
            <<"request_id">> => RequestId,
            <<"status">> => Status,
            <<"provider_id">> => ProviderId,
            <<"job">> => Job,
            <<"latency_ms">> => LatencyMs,
            <<"cost">> => Cost
        },
        
        %% Add optional fields if present
        ResultMap1 = case TraceId of
            undefined -> ResultMap;
            <<>> -> ResultMap;
            _ -> maps:put(<<"trace_id">>, TraceId, ResultMap)
        end,
        ResultMap2 = case TenantId of
            undefined -> ResultMap1;
            <<>> -> ResultMap1;
            _ -> maps:put(<<"tenant_id">>, TenantId, ResultMap1)
        end,
        ResultMap3 = case Timestamp of
            undefined -> ResultMap2;
            0 -> ResultMap2;
            _ -> maps:put(<<"timestamp">>, Timestamp, ResultMap2)
        end,
        ResultMap4 = case ErrorCode of
            undefined -> ResultMap3;
            <<>> -> ResultMap3;
            _ -> maps:put(<<"error_code">>, ErrorCode, ResultMap3)
        end,
        ResultMap5 = case ErrorMessage of
            undefined -> ResultMap4;
            <<>> -> ResultMap4;
            _ -> maps:put(<<"error_message">>, ErrorMessage, ResultMap4)
        end,
        
        {ok, ResultMap5}
    catch
        _:_ -> {error, conversion_failed}
    end;
convert_exec_result_to_map(_) ->
    {error, invalid_protobuf_record}.
-else.
-spec convert_exec_result_to_map(term()) -> {ok, map()} | {error, atom()}.
convert_exec_result_to_map(_ExecResultPb) -> {error, invalid_protobuf_record}.
-endif.

%% @doc Convert ExecAssignmentAck protobuf to map
%% Called when worker_pb module is available and protobuf decode succeeds
-ifdef(WORKER_PB).
-spec convert_exec_assignment_ack_to_map(term()) -> {ok, map()} | {error, atom()}.
convert_exec_assignment_ack_to_map(ExecAssignmentAckPb) when is_record(ExecAssignmentAckPb, 'ExecAssignmentAck') ->
    try
        #'ExecAssignmentAck'{
            assignment_id = AssignmentId,
            status = Status,
            message = Message,
            request_id = RequestId,
            trace_id = TraceId,
            tenant_id = TenantId,
            acknowledged_at = AcknowledgedAt
        } = ExecAssignmentAckPb,
        
        %% Build ExecAssignmentAck map (compatible with existing JSON format)
        AckMap = #{
            <<"assignment_id">> => AssignmentId,
            <<"status">> => Status
        },
        
        %% Add optional fields if present
        AckMap1 = case Message of
            undefined -> AckMap;
            <<>> -> AckMap;
            _ -> maps:put(<<"message">>, Message, AckMap)
        end,
        AckMap2 = case RequestId of
            undefined -> AckMap1;
            <<>> -> AckMap1;
            _ -> maps:put(<<"request_id">>, RequestId, AckMap1)
        end,
        AckMap3 = case TraceId of
            undefined -> AckMap2;
            <<>> -> AckMap2;
            _ -> maps:put(<<"trace_id">>, TraceId, AckMap2)
        end,
        AckMap4 = case TenantId of
            undefined -> AckMap3;
            <<>> -> AckMap3;
            _ -> maps:put(<<"tenant_id">>, TenantId, AckMap3)
        end,
        AckMap5 = case AcknowledgedAt of
            undefined -> AckMap4;
            0 -> AckMap4;
            _ -> maps:put(<<"acknowledged_at">>, AcknowledgedAt, AckMap4)
        end,
        
        {ok, AckMap5}
    catch
        _:_ -> {error, conversion_failed}
    end;
convert_exec_assignment_ack_to_map(_) ->
    {error, invalid_protobuf_record}.
-else.
-spec convert_exec_assignment_ack_to_map(term()) -> {ok, map()} | {error, atom()}.
convert_exec_assignment_ack_to_map(_ExecAssignmentAckPb) -> {error, invalid_protobuf_record}.
-endif.
