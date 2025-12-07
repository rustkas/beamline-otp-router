%% @doc Core Routing Logic
%% Interface for routing decisions with minimal algorithm stub (CP1)
%% CP2: OpenTelemetry tracing integration
-module(router_core).
-export([route/2]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_core]).

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc Route message - main interface for routing decisions
%% @param RouteRequest - #route_request{} with message and optional policy_id
%% @param Context - Additional context map (can be undefined)
%% @returns {ok, #route_decision{}} | {error, {Reason, Context}}
%% @errors {missing_tenant_id, Context}, {policy_not_found, Context}, 
%%         {no_provider_available, Context}, {invalid_policy, Context}
-spec route(#route_request{}, map() | undefined) -> 
    {ok, #route_decision{}} | {error, {atom(), map()}}.
route(RouteRequest, Context) ->
    #route_request{message = Message, policy_id = PolicyId, context = ReqContext} = RouteRequest,
    
    %% Prepare safe context and extract metadata
    SafeReqContext = normalize_context(ReqContext),
    SafeContext = normalize_context(Context),
    FinalPolicyId = determine_policy_id(PolicyId, SafeReqContext),
    
    %% Extract trace context for OpenTelemetry
    TraceContext = extract_trace_context_from_request(RouteRequest, Context),
    
    %% Prepare span attributes
    SpanAttributes = prepare_span_attributes(Message, FinalPolicyId),
    
    %% Execute routing with tracing and telemetry
    router_tracing:with_span(
        <<"beamline.router.route">>,
        SpanAttributes,
        TraceContext,
        fun() ->
            execute_routing(RouteRequest, Message, FinalPolicyId, SafeContext)
        end).

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% @doc Execute routing logic with telemetry
-spec execute_routing(#route_request{}, map(), binary(), map()) ->
    {ok, #route_decision{}} | {error, {atom(), map()}}.
execute_routing(RouteRequest, Message, FinalPolicyId, SafeContext) ->
    %% Extract tenant_id from message
    TenantId = maps:get(<<"tenant_id">>, Message, undefined),
    
    %% Validate tenant_id
    case validate_tenant_id(TenantId, Message) of
        {error, ErrorInfo} ->
            handle_routing_error({error, ErrorInfo}, TenantId, FinalPolicyId, Message),
            {error, ErrorInfo};
        ok ->
            %% Apply policy and get routing decision
            case router_policy_applier:apply_policy(RouteRequest, TenantId, FinalPolicyId, SafeContext) of
                {ok, PolicyResult} ->
                    Decision = convert_policy_result_to_decision(PolicyResult),
                    emit_success_telemetry(TenantId, FinalPolicyId, Decision, Message),
                    set_success_span_attributes(Decision),
                    {ok, Decision};
                {error, ErrorInfo} ->
                    NormalizedError = normalize_error(ErrorInfo),
                    handle_routing_error(NormalizedError, TenantId, FinalPolicyId, Message),
                    set_error_span_attributes(NormalizedError),
                    NormalizedError
            end
    end.

%% @doc Validate tenant_id from message
-spec validate_tenant_id(binary() | undefined, map()) -> 
    ok | {error, {atom(), map()}}.
validate_tenant_id(undefined, Message) ->
    {error, {missing_tenant_id, #{
        context => <<"tenant_id is required in message">>,
        message_id => maps:get(<<"message_id">>, Message, undefined)
    }}};
validate_tenant_id(<<>>, Message) ->
    {error, {missing_tenant_id, #{
        context => <<"tenant_id cannot be empty">>,
        message_id => maps:get(<<"message_id">>, Message, undefined)
    }}};
validate_tenant_id(_TenantId, _Message) ->
    ok.

%% @doc Convert policy result to route decision
-spec convert_policy_result_to_decision(map()) -> #route_decision{}.
convert_policy_result_to_decision(PolicyResult) ->
    ProviderId = maps:get(provider_id, PolicyResult),
    Explanation = maps:get(explanation, PolicyResult),
    Reason = maps:get(reason, Explanation),
    Priority = maps:get(priority, Explanation, 50),
    
    %% Log explanation for audit
    router_audit:log_decision(Explanation),
    
    #route_decision{
        provider_id = ProviderId,
        reason = Reason,
        priority = Priority,
        metadata = maps:get(context, Explanation, #{})
    }.

%% @doc Normalize error format for consistency
-spec normalize_error(term()) -> {error, {atom(), map()}}.
normalize_error({ErrorReason, ErrorContext}) when is_map(ErrorContext) ->
    {error, {ErrorReason, ErrorContext}};
normalize_error(ErrorReason) when is_atom(ErrorReason) ->
    {error, {ErrorReason, #{}}};
normalize_error(ErrorInfo) ->
    {error, {internal_error, #{error => ErrorInfo}}}.

%% @doc Emit success telemetry events
-spec emit_success_telemetry(binary(), binary(), #route_decision{}, map()) -> ok.
emit_success_telemetry(TenantId, FinalPolicyId, Decision, _Message) ->
    RouteProviderId = Decision#route_decision.provider_id,
    RouteDecisionReason = Decision#route_decision.reason,
    
    %% Emit counter for successful routes
    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [routes_total], #{count => 1}, #{
        tenant_id => TenantId,
        policy_id => FinalPolicyId,
        provider_id => RouteProviderId,
        reason => RouteDecisionReason,
        route => FinalPolicyId,
        outcome => <<"ok">>,
        result => ok
    }),
    
    %% Emit counter for resolutions
    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [resolutions_total], #{count => 1}, #{
        tenant_id => TenantId,
        policy_id => FinalPolicyId,
        provider_id => RouteProviderId,
        route => FinalPolicyId,
        outcome => <<"ok">>
    }),
    ok.

%% @doc Handle routing error and emit telemetry
-spec handle_routing_error({error, {atom(), map()}}, binary() | undefined, binary(), map()) -> ok.
handle_routing_error({error, {ErrReason, ErrContext}}, TenantId, FinalPolicyId, _Message) ->
    SafeErrContext = case ErrContext of
        C when is_map(C) -> C;
        _ -> #{}
    end,
    
    %% Emit counter for errors
    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [errors_total], #{count => 1}, #{
        tenant_id => TenantId,
        policy_id => FinalPolicyId,
        error => ErrReason,
        error_context => SafeErrContext,
        route => FinalPolicyId,
        outcome => <<"error">>,
        result => error
    }),
    
    %% Emit counter for routes (including errors)
    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [routes_total], #{count => 1}, #{
        tenant_id => TenantId,
        policy_id => FinalPolicyId,
        route => FinalPolicyId,
        outcome => <<"error">>,
        result => error,
        error => ErrReason
    }),
    ok.

%% @doc Set OpenTelemetry span attributes for success
-spec set_success_span_attributes(#route_decision{}) -> ok.
set_success_span_attributes(Decision) ->
    router_tracing:set_span_attribute(<<"routing.provider_id">>, Decision#route_decision.provider_id, string),
    router_tracing:set_span_attribute(<<"routing.reason">>, Decision#route_decision.reason, string),
    router_tracing:set_span_status(ok, undefined),
    ok.

%% @doc Set OpenTelemetry span attributes for error
-spec set_error_span_attributes({error, {atom(), map()}}) -> ok.
set_error_span_attributes({error, {ErrReason, _ErrContext}}) ->
    ErrReasonBin = case ErrReason of
        A when is_atom(A) -> erlang:atom_to_binary(A, utf8);
        B when is_binary(B) -> B;
        _ -> <<"internal_error">>
    end,
    router_tracing:set_span_attribute(<<"routing.error">>, ErrReasonBin, string),
    router_tracing:set_span_status(error, ErrReasonBin),
    ok.

%% @doc Normalize context to safe map
-spec normalize_context(map() | undefined) -> map().
normalize_context(undefined) ->
    #{};
normalize_context(C) when is_map(C) ->
    C;
normalize_context(_) ->
    #{}.

%% @doc Determine policy_id from request or context
-spec determine_policy_id(binary() | undefined, map()) -> binary().
determine_policy_id(undefined, SafeReqContext) ->
    maps:get(<<"policy_id">>, SafeReqContext, <<"default">>);
determine_policy_id(PolicyId, _SafeReqContext) ->
    PolicyId.

%% @doc Prepare span attributes for OpenTelemetry
-spec prepare_span_attributes(map(), binary()) -> map().
prepare_span_attributes(Message, FinalPolicyId) ->
    #{
        <<"tenant_id">> => maps:get(<<"tenant_id">>, Message, undefined),
        <<"policy_id">> => FinalPolicyId,
        <<"message_id">> => maps:get(<<"message_id">>, Message, undefined)
    }.

%% @doc Extract trace context from request
-spec extract_trace_context_from_request(#route_request{}, map() | undefined) -> map() | undefined.
extract_trace_context_from_request(RouteRequest, Context) ->
    #route_request{context = ReqContext} = RouteRequest,
    
    %% Merge request context and additional context
    SafeReqContext = normalize_context(ReqContext),
    SafeContext = normalize_context(Context),
    MergedContext = maps:merge(SafeReqContext, SafeContext),
    
    %% Extract trace context using router_tracing
    router_tracing:extract_trace_context(MergedContext).
