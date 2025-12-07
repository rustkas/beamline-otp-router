%% @doc Intake Error Handler
%% Handles validation errors: DLQ, audit, metrics, ACK/NAK
%% CP2+: Unified error handling for intake validation failures
-module(router_intake_error_handler).

-export([handle_intake_error/7]).
-export([send_to_dlq/5, build_error_response/3, handle_nats_message_fate/3]).

-ignore_xref([
  {router_intake_error_handler, build_error_response, 3},
  {router_intake_error_handler, handle_nats_message_fate, 3},
  {router_intake_error_handler, send_to_dlq, 5}
]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_intake]).

%% @doc Main error handling function
-spec handle_intake_error(
    ErrorCode :: router_intake_error_codes:error_code(),
    ErrorMessage :: binary(),
    Subject :: binary(),
    Payload :: binary(),
    Headers :: map(),
    MsgId :: binary() | undefined,
    Context :: map()
) -> ok.
handle_intake_error(ErrorCode, ErrorMessage, Subject, Payload, _Headers, MsgId, Context) ->
    %% 1. Audit logging
    log_intake_validation_failed(ErrorCode, ErrorMessage, Subject, Context),
    
    %% 2. Metrics
    emit_validation_error_metric(ErrorCode, Subject, Context),
    
    %% 3. DLQ (for schema errors)
    DLQEnabled = application:get_env(beamline_router, dlq_enabled, true),
    case DLQEnabled andalso should_send_to_dlq(ErrorCode) of
        true ->
            send_to_dlq(Subject, Payload, ErrorCode, ErrorMessage, Context);
        false ->
            ok
    end,
    
    %% 4. NATS message fate (ACK/NAK)
    handle_nats_message_fate(ErrorCode, MsgId, Context),
    
    %% 5. Error response (for request-reply pattern)
    case is_request_reply_subject(Subject) of
        true ->
            build_and_send_error_response(Subject, ErrorCode, ErrorMessage, Context);
        false ->
            ok
    end,
    
    ok.

%% @doc Send message to DLQ
-spec send_to_dlq(
    Subject :: binary(),
    Payload :: binary(),
    ErrorCode :: router_intake_error_codes:error_code(),
    ErrorMessage :: binary(),
    Context :: map()
) -> ok.
send_to_dlq(Subject, Payload, ErrorCode, ErrorMessage, Context) ->
    %% Build DLQ subject
    DLQSubject = build_dlq_subject(Subject),
    
    %% Build DLQ message (with payload hash, not full payload)
    PayloadHash = crypto:hash(sha256, Payload),
    PayloadHashHex = binary_to_hex(PayloadHash),
    
    DLQMessage = #{
        <<"original_subject">> => Subject,
        <<"original_payload_hash">> => PayloadHashHex,
        <<"validation_error">> => #{
            <<"code">> => router_intake_error_codes:error_code_to_string(ErrorCode),
            <<"message">> => ErrorMessage,
            <<"severity">> => atom_to_binary(router_intake_error_codes:error_code_severity(ErrorCode), utf8)
        },
        <<"context">> => filter_pii_from_context(Context),
        <<"received_at">> => erlang:system_time(millisecond),
        <<"router_node_id">> => atom_to_binary(node(), utf8)
    },
    
    %% Publish to DLQ with error handling
    DLQJson = jsx:encode(DLQMessage),
    case router_nats:publish_with_ack(DLQSubject, DLQJson, #{}) of
        {ok, _MsgId} ->
            %% Successfully published to DLQ
            router_logger:debug(<<"DLQ message published">>, #{
                <<"dlq_subject">> => DLQSubject,
                <<"error_code">> => router_intake_error_codes:error_code_to_string(ErrorCode),
                <<"original_subject">> => Subject
            }),
            %% Emit DLQ metric
            emit_dlq_metric(ErrorCode, Subject),
            ok;
        {error, Reason} ->
            %% Failed to publish to DLQ - log error but don't fail
            router_logger:error(<<"Failed to publish to DLQ">>, #{
                <<"dlq_subject">> => DLQSubject,
                <<"error_code">> => router_intake_error_codes:error_code_to_string(ErrorCode),
                <<"original_subject">> => Subject,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"payload_hash">> => PayloadHashHex
            }),
            %% Emit DLQ failure metric
            emit_dlq_failure_metric(ErrorCode, Subject, Reason),
            ok  %% Don't fail - DLQ is best-effort
    end.

%% @doc Build error response for Gateway
-spec build_error_response(
    ErrorCode :: router_intake_error_codes:error_code(),
    ErrorMessage :: binary(),
    Context :: map()
) -> map().
build_error_response(ErrorCode, ErrorMessage, Context) ->
    %% Map intake error code to Gateway-compatible error code
    GatewayErrorCode = map_intake_error_to_gateway_code(ErrorCode),
    ErrorCodeStr = router_intake_error_codes:error_code_to_string(ErrorCode),
    
    Error = #{
        <<"code">> => GatewayErrorCode,  %% Gateway-compatible code
        <<"message">> => ErrorMessage,
        <<"intake_error_code">> => ErrorCodeStr  %% Original intake error code for debugging
    },
    
    %% Add details if available
    ErrorWithDetails = case maps:get(<<"details">>, Context, undefined) of
        undefined -> Error;
        Details when is_map(Details) -> maps:put(<<"details">>, Details, Error)
    end,
    
    RequestId = maps:get(<<"request_id">>, Context, undefined),
    TraceId = maps:get(<<"trace_id">>, Context, undefined),
    
    ResponseContext = case TraceId of
        undefined -> #{<<"request_id">> => RequestId};
        _ -> #{<<"request_id">> => RequestId, <<"trace_id">> => TraceId}
    end,
    
    #{
        <<"ok">> => false,
        <<"error">> => ErrorWithDetails,
        <<"context">> => ResponseContext
    }.

%% @doc Handle NATS message fate (ACK/NAK)
-spec handle_nats_message_fate(
    ErrorCode :: router_intake_error_codes:error_code(),
    MsgId :: binary() | undefined,
    Context :: map()
) -> ok.
handle_nats_message_fate(ErrorCode, MsgId, Context) ->
    case MsgId of
        undefined ->
            ok;
        _ ->
            %% Schema errors → always ACK (don't retry)
            case should_ack_on_error(ErrorCode) of
                true ->
                    router_nats:ack_message(MsgId);
                false ->
                    %% Temporary errors → NAK (with MaxDeliver check)
                    check_and_nak(MsgId, Context)
            end
    end,
    ok.

%% Internal functions

%% Check if error should be sent to DLQ
should_send_to_dlq(ErrorCode) ->
    case ErrorCode of
        schema_validation_failed -> true;
        version_unsupported -> true;
        correlation_fields_invalid -> true;
        tenant_forbidden -> true;
        _ -> false  %% Internal errors may be temporary
    end.

%% Check if error should ACK (don't retry)
should_ack_on_error(ErrorCode) ->
    case ErrorCode of
        schema_validation_failed -> true;
        version_unsupported -> true;
        correlation_fields_invalid -> true;
        tenant_forbidden -> true;
        _ -> false  %% Internal errors may be temporary
    end.

%% Check if subject is request-reply
is_request_reply_subject(Subject) ->
    case Subject of
        <<"beamline.router.v1.decide">> -> true;
        _ -> false
    end.

%% Build DLQ subject
build_dlq_subject(Subject) ->
    %% Check for configurable DLQ subject pattern
    case application:get_env(beamline_router, dlq_subject_pattern, undefined) of
        undefined ->
            %% Default: append .dlq to original subject
            <<Subject/binary, ".dlq">>;
        Pattern when is_binary(Pattern) ->
            %% Use configured pattern (e.g., "beamline.router.v1.intake.dlq")
            Pattern;
        Pattern when is_list(Pattern) ->
            %% Convert list to binary
            list_to_binary(Pattern);
        _ ->
            %% Fallback to default
            <<Subject/binary, ".dlq">>
    end.

%% Build and send error response
build_and_send_error_response(Subject, ErrorCode, ErrorMessage, Context) ->
    ErrorResponse = build_error_response(ErrorCode, ErrorMessage, Context),
    ErrorResponseJson = jsx:encode(ErrorResponse),
    ReplySubject = <<Subject/binary, ".reply">>,
    router_nats:publish(ReplySubject, ErrorResponseJson),
    ok.

%% Check MaxDeliver and NAK if needed
check_and_nak(MsgId, Context) ->
    case check_maxdeliver_exhaustion(MsgId, Context) of
        {ok, exhausted} ->
            router_nats:ack_message(MsgId),
            ok;
        {ok, not_exhausted} ->
            router_nats:nak_message(MsgId),
            ok;
        {error, _Reason} ->
            router_nats:nak_message(MsgId),
            ok
    end.

%% Check MaxDeliver exhaustion (reuse logic from consumer modules)
-spec check_maxdeliver_exhaustion(binary() | undefined, map()) -> {ok, exhausted | not_exhausted} | {error, atom()}.
check_maxdeliver_exhaustion(undefined, _Context) ->
    {ok, not_exhausted};
check_maxdeliver_exhaustion(MsgId, Context) ->
    %% Try to find delivery count table (from any consumer)
    Tables = [
        router_delivery_count,  %% router_result_consumer
        router_decide_delivery_count,  %% router_decide_consumer
        router_ack_delivery_count  %% router_ack_consumer (if exists)
    ],
    
    %% Get MaxDeliver configuration
    MaxDeliver = application:get_env(beamline_router, nats_js_max_deliver, 3),
    
    %% Check all tables (message might be tracked in any of them)
    CheckTable = fun(TableName) ->
        case ets:whereis(TableName) of
            undefined ->
                {ok, not_exhausted};
            Table ->
                case ets:lookup(Table, MsgId) of
                    [] ->
                        {ok, not_exhausted};
                    [{MsgId, DeliveryCount}] when DeliveryCount >= MaxDeliver ->
                        %% MaxDeliver exhausted
                        router_logger:warn(<<"MaxDeliver exhausted for message">>, maps:merge(Context, #{
                            <<"msg_id">> => MsgId,
                            <<"delivery_count">> => DeliveryCount,
                            <<"max_deliver">> => MaxDeliver
                        })),
                        {ok, exhausted};
                    _ ->
                        {ok, not_exhausted}
                end
        end
    end,
    
    %% Check all tables
    Results = [CheckTable(Table) || Table <- Tables],
    case lists:any(fun(R) -> case R of {ok, exhausted} -> true; _ -> false end end, Results) of
        true -> {ok, exhausted};
        false -> {ok, not_exhausted}
    end.

%% Log intake validation failure
log_intake_validation_failed(ErrorCode, ErrorMessage, Subject, Context) ->
    %% Filter PII from context
    FilteredContext = router_logger:filter_pii(Context),
    
    %% Build audit entry
    AuditEntry = #{
        <<"event_type">> => <<"router.intake.validation_failed">>,
        <<"error_code">> => router_intake_error_codes:error_code_to_string(ErrorCode),
        <<"error_message">> => ErrorMessage,
        <<"subject">> => Subject,
        <<"tenant_id">> => maps:get(<<"tenant_id">>, FilteredContext, undefined),
        <<"run_id">> => maps:get(<<"run_id">>, FilteredContext, undefined),
        <<"flow_id">> => maps:get(<<"flow_id">>, FilteredContext, undefined),
        <<"step_id">> => maps:get(<<"step_id">>, FilteredContext, undefined),
        <<"idempotency_key">> => maps:get(<<"idempotency_key">>, FilteredContext, undefined),
        <<"trace_id">> => maps:get(<<"trace_id">>, FilteredContext, undefined),
        <<"received_at">> => erlang:system_time(millisecond),
        <<"router_node_id">> => atom_to_binary(node(), utf8),
        <<"msg_id">> => maps:get(<<"msg_id">>, FilteredContext, undefined)
    },
    
    %% Log via router_logger
    Severity = router_intake_error_codes:error_code_severity(ErrorCode),
    case Severity of
        error ->
            router_logger:error(<<"Intake validation failed">>, AuditEntry);
        warn ->
            router_logger:warn(<<"Intake validation failed">>, AuditEntry)
    end,
    
    ok.

%% Emit validation error metric
emit_validation_error_metric(ErrorCode, Subject, Context) ->
    ErrorCodeStr = router_intake_error_codes:error_code_to_string(ErrorCode),
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [validation_errors_total],
        #{count => 1},
        #{
            error_code => ErrorCodeStr,
            subject => Subject,
            tenant_id => maps:get(<<"tenant_id">>, Context, undefined)
        }
    ),
    ok.

%% Emit DLQ metric
emit_dlq_metric(ErrorCode, Subject) ->
    ErrorCodeStr = router_intake_error_codes:error_code_to_string(ErrorCode),
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [dlq_messages_total],
        #{count => 1},
        #{
            reason => <<"validation_failed">>,
            error_code => ErrorCodeStr,
            subject => Subject
        }
    ),
    ok.

%% Emit DLQ failure metric
emit_dlq_failure_metric(ErrorCode, Subject, Reason) ->
    ErrorCodeStr = router_intake_error_codes:error_code_to_string(ErrorCode),
    telemetry:execute(
        ?TELEMETRY_PREFIX ++ [dlq_publish_failed_total],
        #{count => 1},
        #{
            reason => <<"validation_failed">>,
            error_code => ErrorCodeStr,
            subject => Subject,
            failure_reason => sanitize_error_for_logging(Reason)
        }
    ),
    ok.

%% Map intake error code to Gateway-compatible error code
map_intake_error_to_gateway_code(ErrorCode) ->
    case ErrorCode of
        schema_validation_failed -> <<"invalid_request">>;
        version_unsupported -> <<"invalid_request">>;
        correlation_fields_invalid -> <<"invalid_request">>;
        tenant_forbidden -> <<"unauthorized">>;
        idempotency_violation -> <<"invalid_request">>;
        internal_validation_error -> <<"internal">>
    end.

%% Sanitize error for logging (remove sensitive data)
sanitize_error_for_logging(Error) when is_atom(Error) ->
    atom_to_binary(Error, utf8);
sanitize_error_for_logging(Error) when is_binary(Error) ->
    Error;
sanitize_error_for_logging(Error) when is_list(Error) ->
    list_to_binary(Error);
sanitize_error_for_logging(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

%% Filter PII from context
filter_pii_from_context(Context) ->
    router_logger:filter_pii(Context).

%% Convert binary to hex string
binary_to_hex(Binary) ->
    << <<(hex_digit(H)), (hex_digit(L))>> || <<H:4, L:4>> <= Binary >>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.
