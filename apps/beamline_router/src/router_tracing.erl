-module(router_tracing).

-doc "OpenTelemetry Tracing Module".
%% Provides distributed tracing with context propagation and span management
-export([
    start_span/3,
    end_span/1,
    set_span_attribute/3,
    set_span_status/2,
    get_trace_id/0,
    get_span_id/0,
    extract_trace_context/1,
    inject_trace_context/2,
    with_span/4,
    get_sampling_config/0,
    set_sampling_config/1,
    should_sample/1,
    get_current_trace_context/0,
    get_tracer/0,
    format_error/1
]).
-ignore_xref([
    {router_tracing, end_span, 1},
    {router_tracing, get_trace_id, 0},
    {otel_span, span_span_id, 1},
    {otel_span, span_trace_id, 1},
    {otel_status, set_status, 2},
    {otel_status, status, 2}
]).
%% Avoid xref on internal safe wrapper calling otel_status
%% by marking safe_otel_set_status as ignored
-ignore_xref([{router_tracing, safe_otel_set_status, 3}]).

%% Silence compile-time unused warnings for helper functions when OpenTelemetry is disabled

-include("beamline_router.hrl").

%% Include OpenTelemetry tracer macros if available
%% OpenTelemetry macros are optional; operate via runtime checks to avoid
%% compile-time dependency on opentelemetry headers.

%% Span names
-define(SPAN_ROUTER_DECIDE, ~"beamline.router.decide").
-define(SPAN_ROUTER_POLICY_LOAD, ~"beamline.router.policy.load").
-define(SPAN_ROUTER_DECISION, ~"beamline.router.decision").
-define(SPAN_ROUTER_PROVIDER_SELECT, ~"beamline.router.provider.select").
-define(SPAN_ROUTER_PUBLISH_ASSIGNMENT, ~"beamline.router.publish.assignment").
-define(SPAN_ROUTER_PROCESS_RESULT, ~"beamline.router.process.result").
-define(SPAN_ROUTER_PROCESS_ACK, ~"beamline.router.process.ack").
-define(SPAN_ROUTER_EMIT_USAGE, ~"beamline.router.emit.usage").

%% Tracer name
-define(TRACER_NAME, ~"beamline-router").

%% Default sampling rate (1.0 = 100%, 0.0 = 0%)
-define(DEFAULT_SAMPLING_RATE, 1.0).

%% Removed start_span/2 spec to avoid unused export warnings; use start_span/3

-spec start_span(binary(), map(), map() | undefined) -> {ok, map()} | {error, term()}.
start_span(SpanName, Attributes, ParentContext) ->
    try
        %% Check sampling before creating span
        case should_sample(SpanName) of
            false ->
                %% Not sampled - return stub span
                TraceContext =
                    ParentContext0 = case ParentContext of
                        undefined -> create_new_context();
                        _ -> extract_trace_context(ParentContext)
                    end,
                    normalize_trace_context(ParentContext0),
                Span = create_stub_span(SpanName, Attributes, TraceContext),
                put(current_span, Span),
                put(trace_id, maps:get(trace_id, TraceContext, undefined)),
                {ok, Span};
            true ->
                %% Sampled - create real span
                %% Get or create tracer
                Tracer = get_tracer(),

                %% Extract trace context from parent or create new
                TraceContext0 = case ParentContext of
                    undefined ->
                        %% Create new trace context
                        create_new_context();
                    _ ->
                        %% Extract trace context from parent
                        extract_trace_context(ParentContext)
                end,
                TraceContext = normalize_trace_context(TraceContext0),

                %% Start span with OpenTelemetry
                Span = start_otel_span(Tracer, SpanName, Attributes, TraceContext),

                %% Store span in process dictionary for context propagation
                put(current_span, Span),
                put(trace_id, maps:get(trace_id, TraceContext, undefined)),
                put(span_id, maps:get(span_id, Span, undefined)),

                {ok, Span}
        end
    catch
        CatchError:CatchReason ->
            router_logger:error(~"Failed to start OpenTelemetry span", #{
                ~"span_name" => SpanName,
                ~"error" => CatchError,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"span_start_failed"
            }),
            %% Fallback to stub span
            FallbackTraceContext0 = case ParentContext of
                undefined -> create_new_context();
                _ -> extract_trace_context(ParentContext)
            end,
            FallbackTraceContext = normalize_trace_context(FallbackTraceContext0),
            FallbackSpan = create_stub_span(SpanName, Attributes, FallbackTraceContext),
            put(current_span, FallbackSpan),
            put(trace_id, maps:get(trace_id, FallbackTraceContext, undefined)),
            {ok, FallbackSpan}
    end.

-spec end_span(ok | error) -> ok.
end_span(Status) ->
    try
        case get(current_span) of
            undefined ->
                ok;
            Span ->
                %% Get span name before try block for use in catch
                SpanName = maps:get(name, Span, ~"unknown"),
                %% Emit span duration metric
                try
                    StartUs = maps:get(start_time, Span, erlang:system_time(microsecond)),
                    EndUs = erlang:system_time(microsecond),
                    DurationSeconds = (EndUs - StartUs) / 1000000.0,
                    router_metrics:emit_metric(router_span_duration_seconds, #{duration_seconds => DurationSeconds}, #{span_name => SpanName})
                catch
                    MetricError:MetricReason ->
                        router_logger:debug(~"Failed to emit span duration metric", #{
                            ~"span_name" => SpanName,
                            ~"error" => MetricError,
                            ~"reason" => sanitize_error_for_logging(MetricReason),
                            ~"event" => ~"span_metric_emission_failed"
                        }),
                        ok
                end,
                %% Emit error counter if status is error
                case Status of
                    error ->
                        %% Get span name before try block for use in catch
                        ErrorSpanName = maps:get(name, Span, ~"unknown"),
                        try
                            router_metrics:emit_metric(router_error_total, #{count => 1}, #{span_name => ErrorSpanName})
                        catch
                            ErrorMetricError:ErrorMetricReason ->
                                router_logger:debug(~"Failed to emit error metric", #{
                                    ~"span_name" => ErrorSpanName,
                                    ~"error" => ErrorMetricError,
                                    ~"reason" => sanitize_error_for_logging(ErrorMetricReason),
                                    ~"event" => ~"error_metric_emission_failed"
                                }),
                                ok
                        end;
                    _ -> ok
                end,
                %% Set span status
                case Status of
                    ok ->
                        set_span_status(ok, undefined);
                    error ->
                        set_span_status(error, undefined);
                    _ ->
                        ok
                end,
                %% End span
                end_otel_span(Span),
                erase(current_span),
                ok
        end
    catch
        EndError:EndReason ->
            router_logger:error(~"Failed to end OpenTelemetry span", #{
                ~"error" => EndError,
                ~"reason" => sanitize_error_for_logging(EndReason),
                ~"event" => ~"span_end_failed"
            }),
            %% Fallback: just clear process dictionary
            erase(current_span),
            erase(trace_id),
            ok
    end.

-spec set_span_attribute(atom() | binary(), term(), atom()) -> ok.
set_span_attribute(Key, Value, Type) ->
    try
        case get(current_span) of
            undefined ->
                ok;
            Span ->
                set_otel_span_attribute(Span, Key, Value, Type),
                ok
        end
    catch
        _:_ ->
            ok
    end.

-spec set_span_status(ok | error, binary() | undefined) -> ok.
set_span_status(Status, Message) ->
    try
        case get(current_span) of
            undefined ->
                ok;
            Span ->
                set_otel_span_status(Span, Status, Message),
                ok
        end
    catch
        _:_ ->
            ok
    end.

-spec get_trace_id() -> binary() | undefined.
get_trace_id() ->
    case get(trace_id) of
        undefined ->
            %% Try to extract from current span
            case get(current_span) of
                undefined ->
                    undefined;
                Span ->
                    extract_trace_id_from_span(Span)
            end;
        TraceId ->
            TraceId
    end.

-spec get_span_id() -> binary() | undefined.
get_span_id() ->
    case get(span_id) of
        undefined ->
            %% Try to extract from current span
            case get(current_span) of
                undefined ->
                    undefined;
                Span ->
                    maps:get(span_id, Span, undefined)
            end;
        SpanId ->
            SpanId
    end.

-spec get_current_trace_context() -> map() | undefined.
get_current_trace_context() ->
    TraceId = get_trace_id(),
    SpanId = get_span_id(),
    case TraceId of
        undefined ->
            undefined;
        _ ->
            #{
                trace_id => TraceId,
                span_id => SpanId
            }
    end.

-spec extract_trace_context(map()) -> map() | undefined.
extract_trace_context(Context) when is_map(Context) ->
    try
        %% Try OpenTelemetry W3C Trace Context format first
        TraceParent = maps:get(~"traceparent", Context,
            maps:get(~"trace-parent", Context, undefined)),
        
        case TraceParent of
            undefined ->
                %% Fallback to custom format
                extract_custom_trace_context(Context);
            _ ->
                extract_w3c_trace_context(TraceParent)
        end
    catch
        Error:Reason ->
            router_logger:debug(~"Failed to extract trace context", #{
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"trace_context_extraction_failed"
            }),
            extract_custom_trace_context(Context)
    end;
extract_trace_context(_) ->
    undefined.

-spec inject_trace_context(map(), map() | undefined) -> map().
inject_trace_context(Context, TraceContext) ->
    try
        CurrentTraceId = case TraceContext of
            undefined ->
                get_trace_id();
            _ ->
                maps:get(trace_id, TraceContext, get_trace_id())
        end,
        
        CurrentSpanId = case TraceContext of
            undefined ->
                generate_span_id();
            _ ->
                maps:get(span_id, TraceContext, generate_span_id())
        end,
        
        %% Inject W3C Trace Context format
        TraceParent = format_w3c_traceparent(CurrentTraceId, CurrentSpanId),
        
        %% Also inject custom format for compatibility
        Context#{
            ~"traceparent" => TraceParent,
            ~"trace_id" => CurrentTraceId,
            ~"span_id" => CurrentSpanId,
            ~"X-Trace-Id" => CurrentTraceId,
            ~"X-Span-Id" => CurrentSpanId
        }
    catch
        Error:Reason ->
            router_logger:debug(~"Failed to inject trace context", #{
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"trace_context_injection_failed"
            }),
            %% Fallback: just add trace_id
            TraceId = get_trace_id(),
            maps:put(~"trace_id", TraceId, Context)
    end.

%% Removed with_span/3 spec to avoid unused export warnings; use with_span/4

-spec with_span(binary(), map(), map() | undefined, fun(() -> Result)) -> Result when Result :: term().
with_span(SpanName, Attributes, ParentContext, Fun) ->
    case start_span(SpanName, Attributes, ParentContext) of
        {ok, _Span} ->
            try
                Result = Fun(),
                end_span(ok),
                Result
            catch
                Error:Reason:Stacktrace ->
                    %% Log error with router_logger before raising
                    SanitizedReason = sanitize_error_for_logging(Reason),
                    router_logger:error(~"Tracing span execution failed", #{
                        ~"span_name" => SpanName,
                        ~"error" => Error,
                        ~"reason" => SanitizedReason,
                        ~"event" => ~"tracing_span_execution_failed"
                    }),
                    end_span(error),
                    set_span_attribute(~"error", erlang:atom_to_binary(Error, utf8), string),
                    set_span_attribute(~"error.reason", SanitizedReason, string),
                    erlang:raise(Error, Reason, Stacktrace)
            end;
        {error, _Reason} ->
            %% Fallback: execute without span
            Fun()
    end.

%% Internal: Get or create tracer
-spec get_tracer() -> opentelemetry:tracer() | undefined.
get_tracer() ->
    case code:which(opentelemetry) of
        non_existing -> undefined;
        _ ->
            try opentelemetry:get_tracer(?TRACER_NAME) catch
                Error:Reason ->
                    router_logger:debug(~"Failed to get OpenTelemetry tracer", #{
                        ~"tracer_name" => ?TRACER_NAME,
                        ~"error" => Error,
                        ~"reason" => sanitize_error_for_logging(Reason),
                        ~"event" => ~"tracer_get_failed"
                    }),
                    undefined
            end
    end.

%% Internal: Start OpenTelemetry span
-spec start_otel_span(term(), binary(), map(), map() | undefined) -> map().
start_otel_span(undefined, SpanName, Attributes, TraceContext) ->
    create_stub_span(SpanName, Attributes, TraceContext);
start_otel_span(Tracer, SpanName, Attributes, TraceContext) ->
    case {Tracer, code:which(otel_tracer)} of
        {undefined, _} -> create_stub_span(SpanName, Attributes, TraceContext);
        {_, non_existing} -> create_stub_span(SpanName, Attributes, TraceContext);
        _ ->
            try
                ParentSpanCtx = case TraceContext of
                    undefined -> undefined;
                    _ -> extract_parent_span_context(TraceContext)
                end,
                StartOpts = #{attributes => normalize_attributes(Attributes)},
                StartOpts1 = case ParentSpanCtx of
                    undefined -> StartOpts;
                    _ -> maps:put(parent, ParentSpanCtx, StartOpts)
                end,
                SpanCtx = otel_tracer:start_span(Tracer, SpanName, StartOpts1),
                TraceId = case TraceContext of
                    undefined -> extract_trace_id_from_span_ctx(SpanCtx);
                    _ -> maps:get(trace_id, TraceContext, extract_trace_id_from_span_ctx(SpanCtx))
                end,
                _ = otel_tracer:set_current_span(SpanCtx),
                #{
                    span_ctx => SpanCtx,
                    span_id => extract_span_id_from_span_ctx(SpanCtx),
                    trace_id => TraceId,
                    name => SpanName,
                    attributes => Attributes,
                    start_time => erlang:system_time(microsecond)
                }
            catch
                Error:Reason ->
                    router_logger:debug(~"Failed to create OpenTelemetry span, using stub", #{
                        ~"span_name" => SpanName,
                        ~"error" => Error,
                        ~"reason" => sanitize_error_for_logging(Reason),
                        ~"event" => ~"otel_span_creation_failed"
                    }),
                    create_stub_span(SpanName, Attributes, TraceContext)
            end
    end.

%% Internal: End OpenTelemetry span
-spec end_otel_span(map()) -> ok.
end_otel_span(#{span_ctx := SpanCtx}) ->
    case code:which(otel_span) of
        non_existing -> ok;
        _ -> try otel_span:end_span(SpanCtx) catch
            Error:Reason ->
                router_logger:debug(~"Failed to end OpenTelemetry span", #{
                    ~"error" => Error,
                    ~"reason" => sanitize_error_for_logging(Reason),
                    ~"event" => ~"otel_span_end_failed"
                }),
                ok
        end
    end;
end_otel_span(_) -> ok.

%% Internal: Set OpenTelemetry span attribute
-spec set_otel_span_attribute(map(), atom() | binary(), term(), atom()) -> ok.
set_otel_span_attribute(#{span_ctx := SpanCtx}, Key, Value, Type) ->
    case code:which(otel_span) of
        non_existing -> ok;
        _ ->
            try
                AttrKey = normalize_key(Key),
                AttrValue = normalize_value(Value, Type),
                otel_span:set_attribute(SpanCtx, AttrKey, AttrValue)
            catch
                Error:Reason ->
                    router_logger:debug(~"Failed to set OpenTelemetry span attribute", #{
                        ~"key" => normalize_key(Key),
                        ~"error" => Error,
                        ~"reason" => sanitize_error_for_logging(Reason),
                        ~"event" => ~"otel_span_attribute_set_failed"
                    }),
                    ok
            end
    end;
set_otel_span_attribute(_, _, _, _) -> ok.

%% Internal: Set OpenTelemetry span status
-spec set_otel_span_status(map(), ok | error, binary() | undefined) -> ok.
set_otel_span_status(#{span_ctx := SpanCtx}, Status, Message) ->
    try
        case Status of
            ok -> _ = safe_otel_set_status(SpanCtx, ok, <<>>), ok;
            error -> _ = safe_otel_set_status(SpanCtx, error, Message), ok;
            _ -> ok
        end
    catch
        Error:Reason ->
            router_logger:debug(~"Failed to set OpenTelemetry span status", #{
                ~"status" => Status,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"otel_span_status_set_failed"
            }),
            ok
    end;
set_otel_span_status(_, _, _) -> ok.

%% Internal: Create stub span (fallback when OpenTelemetry not available)
-spec create_stub_span(binary(), map(), map() | undefined) -> map().
create_stub_span(SpanName, Attributes, ParentContext) ->
    SpanId = generate_span_id(),
    TraceId = case ParentContext of
        undefined ->
            generate_trace_id();
        _ ->
            extract_trace_id_from_context(ParentContext)
    end,
    
    #{
        span_id => SpanId,
        trace_id => TraceId,
        name => SpanName,
        attributes => Attributes,
        start_time => erlang:system_time(microsecond)
    }.

%% Internal: Extract W3C Trace Context format
-spec extract_w3c_trace_context(binary()) -> map() | undefined.
extract_w3c_trace_context(TraceParent) when is_binary(TraceParent) ->
    %% W3C Trace Context format: version-trace_id-parent_id-trace_flags
    %% Example: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
    case binary:split(TraceParent, ~"-", [global]) of
        [~"00", TraceIdHex, ParentIdHex, _Flags] ->
            TraceId = hex_to_binary(TraceIdHex),
            SpanId = hex_to_binary(ParentIdHex),
            #{
                trace_id => TraceId,
                span_id => SpanId
            };
        _ ->
            undefined
    end;
extract_w3c_trace_context(_) ->
    undefined.

%% Internal: Extract custom trace context format
-spec extract_custom_trace_context(map()) -> map() | undefined.
extract_custom_trace_context(Context) ->
    TraceId = maps:get(~"trace_id", Context,
        maps:get(~"trace-id", Context,
            maps:get(~"traceId", Context,
                maps:get(~"X-Trace-Id", Context, undefined)))),
    
    SpanId = maps:get(~"span_id", Context,
        maps:get(~"span-id", Context,
            maps:get(~"spanId", Context,
                maps:get(~"X-Span-Id", Context, undefined)))),
    
    case TraceId of
        undefined ->
            undefined;
        _ ->
            #{
                trace_id => TraceId,
                span_id => SpanId
            }
    end.

%% Internal: Format W3C Trace Context
-spec format_w3c_traceparent(binary(), binary()) -> binary().
format_w3c_traceparent(TraceId, SpanId) ->
    %% W3C Trace Context format: version-trace_id-parent_id-trace_flags
    TraceIdHex = binary_to_hex(TraceId),
    SpanIdHex = binary_to_hex(SpanId),
    <<"00-", TraceIdHex/binary, "-", SpanIdHex/binary, "-01">>.

%% Internal: Extract parent span context from trace context
-spec extract_parent_span_context(map()) -> term() | undefined.
extract_parent_span_context(TraceContext) ->
    case {code:which(otel_ctx), code:which(otel_tracer)} of
        {non_existing, _} -> undefined;
        {_, non_existing} -> undefined;
        _ ->
            try
                TraceId = maps:get(trace_id, TraceContext),
                SpanId = maps:get(span_id, TraceContext),
                _ = otel_ctx:set_value(otel_ctx:get_current(), ~"trace_id", TraceId),
                _ = otel_ctx:set_value(otel_ctx:get_current(), ~"span_id", SpanId),
                otel_tracer:current_span_ctx()
            catch
                Error:Reason ->
                    router_logger:debug(~"Failed to extract parent span context", #{
                        ~"error" => Error,
                        ~"reason" => sanitize_error_for_logging(Reason),
                        ~"event" => ~"parent_span_context_extraction_failed"
                    }),
                    undefined
            end
    end.

%% Internal: Extract trace ID from span context
-spec extract_trace_id_from_span_ctx(term()) -> binary() | undefined.
extract_trace_id_from_span_ctx(SpanCtx) ->
    case code:which(otel_span) of
        non_existing -> generate_trace_id();
        _ -> try otel_span:span_trace_id(SpanCtx) catch
            Error:Reason ->
                router_logger:debug(~"Failed to extract trace ID from span context", #{
                    ~"error" => Error,
                    ~"reason" => sanitize_error_for_logging(Reason),
                    ~"event" => ~"trace_id_extraction_failed"
                }),
                generate_trace_id()
        end
    end.

%% Internal: Extract span ID from span context
-spec extract_span_id_from_span_ctx(term()) -> binary() | undefined.
extract_span_id_from_span_ctx(SpanCtx) ->
    case code:which(otel_span) of
        non_existing -> generate_span_id();
        _ -> try otel_span:span_span_id(SpanCtx) catch
            Error:Reason ->
                router_logger:debug(~"Failed to extract span ID from span context", #{
                    ~"error" => Error,
                    ~"reason" => sanitize_error_for_logging(Reason),
                    ~"event" => ~"span_id_extraction_failed"
                }),
                generate_span_id()
        end
    end.

%% Internal: Extract trace ID from span
-spec extract_trace_id_from_span(map()) -> binary() | undefined.
extract_trace_id_from_span(#{trace_id := TraceId}) ->
    TraceId;
extract_trace_id_from_span(#{span_ctx := SpanCtx}) ->
    extract_trace_id_from_span_ctx(SpanCtx);
extract_trace_id_from_span(_) ->
    undefined.

%% Internal: Create new trace context
-spec create_new_context() -> map().
create_new_context() ->
    #{
        trace_id => generate_trace_id(),
        span_id => generate_span_id()
    }.

%% Internal: Normalize attributes for OpenTelemetry
-spec normalize_attributes(map()) -> list().
normalize_attributes(Attributes) ->
    maps:fold(fun(Key, Value, Acc) ->
        NormalizedKey = normalize_key(Key),
        NormalizedValue = normalize_value(Value, infer_type(Value)),
        [{NormalizedKey, NormalizedValue} | Acc]
    end, [], Attributes).

%% Internal: Normalize key
-spec normalize_key(atom() | binary() | string()) -> binary().
normalize_key(Key) when is_atom(Key) ->
    erlang:atom_to_binary(Key, utf8);
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) when is_list(Key) ->
    list_to_binary(Key);
normalize_key(Key) ->
    erlang:term_to_binary(Key).

%% Internal: Normalize value
-spec normalize_value(term(), atom()) -> term().
normalize_value(Value, string) when is_binary(Value) ->
    binary_to_list(Value);
normalize_value(Value, string) when is_atom(Value) ->
    erlang:atom_to_list(Value);
normalize_value(Value, integer) when is_integer(Value) ->
    Value;
normalize_value(Value, float) when is_float(Value) ->
    Value;
normalize_value(Value, boolean) when is_boolean(Value) ->
    Value;
normalize_value(Value, _) ->
    erlang:term_to_binary(Value).

%% Internal: Infer value type
-spec infer_type(term()) -> atom().
infer_type(Value) when is_binary(Value) -> string;
infer_type(Value) when is_list(Value) -> string;
infer_type(Value) when is_atom(Value) -> string;
infer_type(Value) when is_integer(Value) -> integer;
infer_type(Value) when is_float(Value) -> float;
infer_type(Value) when is_boolean(Value) -> boolean;
infer_type(_) -> string.

%% Internal: Format error for attributes
%% Note: This function is kept for potential future use or callback requirements
-spec format_error(term()) -> binary().

format_error(Error) when is_binary(Error) ->
    Error;
format_error(Error) when is_atom(Error) ->
    erlang:atom_to_binary(Error, utf8);
format_error(Error) ->
    erlang:term_to_binary(Error).

%% Internal: Generate trace ID (128-bit hex string)
-spec generate_trace_id() -> binary().
generate_trace_id() ->
    <<A:32, B:32, C:32, D:32>> = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format("~32.16.0b~32.16.0b~32.16.0b~32.16.0b", [A, B, C, D])).

%% Internal: Generate span ID (64-bit hex string)
-spec generate_span_id() -> binary().
generate_span_id() ->
    <<A:32, B:32>> = crypto:strong_rand_bytes(8),
    list_to_binary(io_lib:format("~32.16.0b~32.16.0b", [A, B])).

%% Internal: Extract trace ID from context
-spec extract_trace_id_from_context(map()) -> binary().
extract_trace_id_from_context(Context) when is_map(Context) ->
    maps:get(trace_id, Context, generate_trace_id());
extract_trace_id_from_context(_) ->
    generate_trace_id().

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

%% Internal: Binary to hex
-spec binary_to_hex(binary()) -> binary().
binary_to_hex(Binary) ->
    << <<(hex_digit(N))>> || <<N:4>> <= Binary >>.

%% Internal: Hex to binary
-spec hex_to_binary(binary()) -> binary().
hex_to_binary(Hex) ->
    << <<(hex_value(C)):4>> || <<C>> <= Hex >>.

%% Internal: Hex digit
-spec hex_digit(0..15) -> 48..57 | 97..102.
hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

%% Internal: Hex value
-spec hex_value(48..57 | 97..102) -> 0..15.
hex_value(C) when C >= $0, C =< $9 -> C - $0;
hex_value(C) when C >= $a, C =< $f -> C - $a + 10;
hex_value(C) when C >= $A, C =< $F -> C - $A + 10.
safe_otel_set_status(SpanCtx, StatusAtom, Message) ->
    case code:which(otel_status) of
        non_existing -> ok;
        _ ->
            try
                _ = otel_status:set_status(SpanCtx, otel_status:status(StatusAtom, Message)),
                ok
            catch
                Error:Reason ->
                    router_logger:debug(~"Failed to set OpenTelemetry span status safely", #{
                        ~"error" => Error,
                        ~"reason" => sanitize_error_for_logging(Reason),
                        ~"event" => ~"otel_status_set_failed"
                    }),
                    ok
            end
    end.

-spec get_sampling_config() -> map().
get_sampling_config() ->
    SamplingRate = application:get_env(beamline_router, trace_sampling_rate, ?DEFAULT_SAMPLING_RATE),
    SamplingStrategy = application:get_env(beamline_router, trace_sampling_strategy, always),
    #{
        sampling_rate => SamplingRate,
        sampling_strategy => SamplingStrategy
    }.

-spec set_sampling_config(map()) -> ok.
set_sampling_config(Config) ->
    case maps:get(sampling_rate, Config, undefined) of
        undefined -> ok;
        Rate when is_float(Rate), Rate >= 0.0, Rate =< 1.0 ->
            application:set_env(beamline_router, trace_sampling_rate, Rate)
    end,
    case maps:get(sampling_strategy, Config, undefined) of
        undefined -> ok;
        Strategy when Strategy =:= always; Strategy =:= never; Strategy =:= probabilistic ->
            application:set_env(beamline_router, trace_sampling_strategy, Strategy)
    end,
    ok.

%% Internal: Normalize trace context to a map
normalize_trace_context(undefined) -> create_new_context();
normalize_trace_context(Context) when is_map(Context) -> Context;
normalize_trace_context(_) -> create_new_context().

-spec should_sample(binary()) -> boolean().
should_sample(_SpanName) ->
    Config = get_sampling_config(),
    Strategy = maps:get(sampling_strategy, Config, always),
    Rate = maps:get(sampling_rate, Config, ?DEFAULT_SAMPLING_RATE),
    
    case Strategy of
        always ->
            true;
        never ->
            false;
        probabilistic ->
            %% Use probabilistic sampling based on rate
            Random = rand:uniform(),
            Random =< Rate;
        _ ->
            true
    end.
