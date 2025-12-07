%% @doc Router Observability Module (CP2 Minimal Increment)
%% Provides minimal OpenTelemetry spans for critical Router paths
%% Scope: decide + result handling (without dashboards/alerts)
-module(router_observability).
-export([
    init/0,
    create_decide_span/2,
    create_result_span/2,
    extract_cp1_attributes/1,
    get_span_attributes/1
]).

-ignore_xref([
  {router_observability, init, 0},
  {router_observability, create_decide_span, 2},
  {router_observability, create_result_span, 2},
  {router_observability, extract_cp1_attributes, 1},
  {router_observability, get_span_attributes, 1}
]).

-include("beamline_router.hrl").

%% @doc Initialize observability module
%% CP2 Minimal: Just verify router_tracing is available
init() ->
    case erlang:function_exported(router_tracing, start_span, 3) of
        true ->
            router_logger:info(<<"Router observability initialized (CP2 minimal)">>, #{
                <<"tracing_enabled">> => true
            }),
            ok;
        false ->
            router_logger:warn(<<"Router observability: router_tracing not available">>, #{}),
            ok
    end.

%% @doc Create span for decide path (CP2 minimal)
%% Extracts CP1 correlation fields and creates span with proper attributes
-spec create_decide_span(map(), map()) -> {ok, map()} | {error, term()}.
create_decide_span(Request, Headers) ->
    %% Extract CP1 correlation fields
    Attributes = extract_cp1_attributes_from_decide(Request, Headers),
    
    %% Create span using router_tracing
    router_tracing:start_span(
        <<"beamline.router.process.decide">>,
        Attributes,
        extract_parent_context(Headers)
    ).

%% @doc Create span for result handling path (CP2 minimal)
%% Extracts CP1 correlation fields and creates span with proper attributes
-spec create_result_span(map(), map()) -> {ok, map()} | {error, term()}.
create_result_span(Result, Headers) ->
    %% Extract CP1 correlation fields
    Attributes = extract_cp1_attributes_from_result(Result, Headers),
    
    %% Create span using router_tracing
    router_tracing:start_span(
        <<"beamline.router.process.result">>,
        Attributes,
        extract_parent_context(Headers)
    ).

%% @doc Extract CP1 correlation fields as span attributes
%% Returns map with standard CP1 correlation fields for span attributes
-spec extract_cp1_attributes(map()) -> map().
extract_cp1_attributes(Context) when is_map(Context) ->
    #{
        tenant_id => maps:get(<<"tenant_id">>, Context, undefined),
        trace_id => maps:get(<<"trace_id">>, Context, undefined),
        run_id => maps:get(<<"run_id">>, Context, undefined),
        flow_id => maps:get(<<"flow_id">>, Context, undefined),
        step_id => maps:get(<<"step_id">>, Context, undefined),
        subject => maps:get(<<"subject">>, Context, undefined)
    }.

%% @doc Get span attributes from context (helper for tests)
-spec get_span_attributes(map()) -> map().
get_span_attributes(Context) ->
    extract_cp1_attributes(Context).

%% Internal: Extract CP1 attributes from decide request
-spec extract_cp1_attributes_from_decide(map(), map()) -> map().
extract_cp1_attributes_from_decide(Request, Headers) ->
    %% Priority: headers first, then payload
    TenantId = extract_header_or_payload(Headers, Request, <<"tenant_id">>, <<"tenant_id">>),
    TraceId = extract_header_or_payload(Headers, Request, <<"trace_id">>, <<"trace_id">>),
    RunId = extract_header_or_payload(Headers, Request, <<"run_id">>, <<"run_id">>),
    FlowId = extract_header_or_payload(Headers, Request, <<"flow_id">>, <<"flow_id">>),
    StepId = extract_header_or_payload(Headers, Request, <<"step_id">>, <<"step_id">>),
    RequestId = maps:get(<<"request_id">>, Request, undefined),
    PolicyId = maps:get(<<"policy_id">>, Request, undefined),
    
    %% Build attributes map (only include non-undefined values)
    Attributes = #{},
    Attributes1 = case TenantId of
        undefined -> Attributes;
        _ -> maps:put(tenant_id, TenantId, Attributes)
    end,
    Attributes2 = case TraceId of
        undefined -> Attributes1;
        _ -> maps:put(trace_id, TraceId, Attributes1)
    end,
    Attributes3 = case RunId of
        undefined -> Attributes2;
        _ -> maps:put(run_id, RunId, Attributes2)
    end,
    Attributes4 = case FlowId of
        undefined -> Attributes3;
        _ -> maps:put(flow_id, FlowId, Attributes3)
    end,
    Attributes5 = case StepId of
        undefined -> Attributes4;
        _ -> maps:put(step_id, StepId, Attributes4)
    end,
    Attributes6 = case RequestId of
        undefined -> Attributes5;
        _ -> maps:put(request_id, RequestId, Attributes5)
    end,
    Attributes7 = case PolicyId of
        undefined -> Attributes6;
        _ -> maps:put(policy_id, PolicyId, Attributes6)
    end,
    Attributes7.

%% Internal: Extract CP1 attributes from result
-spec extract_cp1_attributes_from_result(map(), map()) -> map().
extract_cp1_attributes_from_result(Result, Headers) ->
    %% Priority: headers first, then payload
    TenantId = extract_header_or_payload(Headers, Result, <<"tenant_id">>, <<"tenant_id">>),
    TraceId = extract_header_or_payload(Headers, Result, <<"trace_id">>, <<"trace_id">>),
    RunId = extract_header_or_payload(Headers, Result, <<"run_id">>, <<"run_id">>),
    FlowId = extract_header_or_payload(Headers, Result, <<"flow_id">>, <<"flow_id">>),
    StepId = extract_header_or_payload(Headers, Result, <<"step_id">>, <<"step_id">>),
    AssignmentId = maps:get(<<"assignment_id">>, Result, undefined),
    RequestId = maps:get(<<"request_id">>, Result, undefined),
    Status = maps:get(<<"status">>, Result, undefined),
    ProviderId = maps:get(<<"provider_id">>, Result, undefined),
    
    %% Build attributes map (only include non-undefined values)
    Attributes = #{},
    Attributes1 = case TenantId of
        undefined -> Attributes;
        _ -> maps:put(tenant_id, TenantId, Attributes)
    end,
    Attributes2 = case TraceId of
        undefined -> Attributes1;
        _ -> maps:put(trace_id, TraceId, Attributes1)
    end,
    Attributes3 = case RunId of
        undefined -> Attributes2;
        _ -> maps:put(run_id, RunId, Attributes2)
    end,
    Attributes4 = case FlowId of
        undefined -> Attributes3;
        _ -> maps:put(flow_id, FlowId, Attributes3)
    end,
    Attributes5 = case StepId of
        undefined -> Attributes4;
        _ -> maps:put(step_id, StepId, Attributes4)
    end,
    Attributes6 = case AssignmentId of
        undefined -> Attributes5;
        _ -> maps:put(assignment_id, AssignmentId, Attributes5)
    end,
    Attributes7 = case RequestId of
        undefined -> Attributes6;
        _ -> maps:put(request_id, RequestId, Attributes6)
    end,
    Attributes8 = case Status of
        undefined -> Attributes7;
        _ -> maps:put(status, Status, Attributes7)
    end,
    Attributes9 = case ProviderId of
        undefined -> Attributes8;
        _ -> maps:put(provider_id, ProviderId, Attributes8)
    end,
    Attributes9.

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

%% Internal: Extract parent context from headers (for trace propagation)
-spec extract_parent_context(map()) -> map() | undefined.
extract_parent_context(Headers) ->
    TraceId = maps:get(<<"trace_id">>, Headers, undefined),
    case TraceId of
        undefined ->
            undefined;
        _ ->
            #{<<"trace_id">> => TraceId}
    end.
