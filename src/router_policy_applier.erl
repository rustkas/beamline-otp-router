%% @doc Unified Policy Application Module
%% Single entry point for applying routing policy
%% Input: request + tenant + policy_id (optional)
%% Output: selected provider, extensions list, decision explanation (for audit)
%% CP2: OpenTelemetry tracing integration
-module(router_policy_applier).
-export([apply_policy/3, apply_policy/4]).
-export([from_map/2]).  %% DSL parsing: convert map to #policy{}

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_policy_applier]).

%% @doc Apply policy to request
%% Args:
%%   Request - route request (map or #route_request{})
%%   TenantId - tenant identifier (binary)
%%   PolicyId - policy identifier (binary, optional, defaults to "default")
%% Returns:
%%   {ok, #{
%%     provider_id => binary(),
%%     extensions => #{pre => [], validators => [], post => []},
%%     explanation => #{
%%       reason => binary(),
%%       steps => [binary()],
%%       context => map()
%%     }
%%   }} | {error, {Reason, Context}}
apply_policy(Request, TenantId, PolicyId) when is_binary(PolicyId) ->
    apply_policy(Request, TenantId, PolicyId, #{});
apply_policy(Request, TenantId, undefined) ->
    apply_policy(Request, TenantId, <<"default">>, #{}).

%% @doc Apply policy to request with additional context
%% Args:
%%   Request - route request (map or #route_request{})
%%   TenantId - tenant identifier (binary)
%%   PolicyId - policy identifier (binary, optional)
%%   Context - additional context (map)
%% Returns:
%%   {ok, Result} | {error, {Reason, Context}}
apply_policy(Request, TenantId, PolicyId, Context) when is_binary(TenantId) ->
    %% Normalize request to map format
    RequestMap = normalize_request(Request),
    
    %% Extract trace context from request (CP2: OpenTelemetry tracing)
    TraceContext = extract_trace_context_from_request(Request, Context),
    
    %% Start OpenTelemetry span for policy application
    SpanAttributes = #{
        <<"tenant_id">> => TenantId,
        <<"policy_id">> => PolicyId
    },
    
    router_tracing:with_span(
        <<"beamline.router.policy.apply">>,
        SpanAttributes,
        TraceContext,
        fun() ->
            %% Measure total policy application latency
            ApplicationStartTime = erlang:monotonic_time(millisecond),
            
            %% Check tenant validation before loading policy (tenant/role-based access control)
            %% Uses router_tenant_validator (single source of truth for ACL decisions)
            %% Replaces deprecated router_acl:allow/3 call
            ValidationContext = maps:merge(Context, #{
                policy_id => PolicyId,
                tenant_id => TenantId,
                action => <<"route">>  %% Action: route decision
            }),
            
            %% Validate tenant using router_tenant_validator (replaces router_acl:allow/3)
            Result = case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
                {error, Reason, ErrorContext} ->
                    %% Tenant validation failed, return error immediately
                    ReasonBin = erlang:atom_to_binary(Reason, utf8),
                    {error, {acl_denied, maps:merge(ErrorContext, #{
                        tenant_id => TenantId,
                        policy_id => PolicyId,
                        acl_reason => Reason,
                        context => <<"Tenant validation failed: ", ReasonBin/binary>>
                    })}};
                {ok, _ValidatedTenantId} ->
                    %% Tenant validation passed, continue with policy loading and application
                    apply_policy_after_acl(TenantId, PolicyId, RequestMap, Context)
            end,
            
            %% Measure total application latency and emit metric
            ApplicationEndTime = erlang:monotonic_time(millisecond),
            ApplicationLatency = ApplicationEndTime - ApplicationStartTime,
            router_metrics:emit_metric(router_policy_application_latency_ms, #{value => ApplicationLatency}, #{
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            
            %% Set OTel span attributes based on result
            case Result of
                {ok, PolicyResult} ->
                    ProviderId = maps:get(provider_id, PolicyResult),
                    router_tracing:set_span_attribute(<<"policy.provider_id">>, ProviderId, string),
                    router_tracing:set_span_status(ok, undefined);
                {error, {acl_denied, _}} ->
                    router_tracing:set_span_attribute(<<"policy.acl_denied">>, true, boolean),
                    router_tracing:set_span_status(error, <<"acl_denied">>);
                {error, ErrorInfo} ->
                    case ErrorInfo of
                        {ErrReason, _ErrContext} ->
                            router_tracing:set_span_attribute(<<"policy.error">>, erlang:atom_to_binary(ErrReason, utf8), string),
                            router_tracing:set_span_status(error, erlang:atom_to_binary(ErrReason, utf8));
                        ErrReason when is_atom(ErrReason) ->
                            router_tracing:set_span_attribute(<<"policy.error">>, erlang:atom_to_binary(ErrReason, utf8), string),
                            router_tracing:set_span_status(error, erlang:atom_to_binary(ErrReason, utf8));
                        _ ->
                            router_tracing:set_span_status(error, <<"policy_error">>)
                    end
            end,
            
            Result
        end).

%% Internal: Apply policy after ACL check passed
-spec apply_policy_after_acl(binary(), binary(), map(), map()) ->
    {ok, map()} | {error, term()}.
apply_policy_after_acl(TenantId, PolicyId, RequestMap, Context) ->
    case router_policy:load_policy(TenantId, PolicyId) of
        {ok, Policy} ->
            apply_policy_with_rate_limit(Policy, TenantId, PolicyId, RequestMap, Context);
        {error, not_found} ->
            {error, {policy_not_found, #{
                tenant_id => TenantId,
                policy_id => PolicyId,
                context => <<"Policy not found in store">>
            }}};
        {error, Reason} ->
            {error, {Reason, #{
                tenant_id => TenantId,
                policy_id => PolicyId,
                context => <<"Policy load error">>
            }}}
    end.

%% Internal: Apply policy with rate limit check
-spec apply_policy_with_rate_limit(#policy{}, binary(), binary(), map(), map()) ->
    {ok, map()} | {error, term()}.
apply_policy_with_rate_limit(Policy, TenantId, PolicyId, RequestMap, Context) ->
    RateLimitResult = check_policy_rate_limit(Policy, TenantId, PolicyId),
    case RateLimitResult of
        {ok, allow} ->
            %% Apply policy decision
            apply_policy_decision(RequestMap, Policy, Context);
        {error, {rate_limit_exceeded, Details}} ->
            %% Rate limit exceeded, return error immediately
            {error, {rate_limit_exceeded, Details}}
    end.

%% Internal: Extract trace context from request
-spec extract_trace_context_from_request(term(), map() | undefined) -> map() | undefined.
extract_trace_context_from_request(Request, Context) ->
    %% Normalize request to extract context
    RequestMap = normalize_request(Request),
    ReqContext = maps:get(context, RequestMap, #{}),
    
    %% Merge request context and additional context
    SafeReqContext = case ReqContext of
        undefined -> #{};
        RC when is_map(RC) -> RC;
        _ -> #{}
    end,
    SafeContext = case Context of
        undefined -> #{};
        C when is_map(C) -> C;
        _ -> #{}
    end,
    MergedContext = maps:merge(SafeReqContext, SafeContext),
    
    %% Extract trace context using router_tracing
    router_tracing:extract_trace_context(MergedContext).

%% Internal: Normalize request to map format
normalize_request(#route_request{message = Message, context = ReqContext}) ->
    #{
        message => Message,
        context => case ReqContext of
            undefined -> #{};
            C when is_map(C) -> C;
            _ -> #{}
        end
    };
normalize_request(Request) when is_map(Request) ->
    Request;
normalize_request(_) ->
    #{}.

%% Internal: Apply policy decision
apply_policy_decision(RequestMap, Policy, Context) ->
    #policy{
        weights = _Weights,
        fallback = _Fallback,
        fallbacks = _Fallbacks,
        sticky = _Sticky,
        pre = _Pre,
        validators = _Validators,
        post = _Post
    } = Policy,
    
    Message = maps:get(message, RequestMap, #{}),
    ReqContext = maps:get(context, RequestMap, #{}),
    MergedContext = maps:merge(ReqContext, Context),
    MergedContextWithTenant = maps:put(<<"tenant_id">>, maps:get(<<"tenant_id">>, Message, undefined), MergedContext),
    
    %% Create RouteRequest for router_decider
    RouteRequest = #route_request{
        message = Message,
        context = ReqContext
    },
    
    %% Measure decision latency (time to make routing decision)
    DecisionStartTime = erlang:monotonic_time(millisecond),
    
    %% Make decision using router_decider
    case router_decider:decide(RouteRequest, Policy, MergedContext) of
        {ok, Decision} ->
            DecisionEndTime = erlang:monotonic_time(millisecond),
            DecisionLatency = DecisionEndTime - DecisionStartTime,
            
            ProviderId = Decision#route_decision.provider_id,
            Reason = Decision#route_decision.reason,
            TenantId = Policy#policy.tenant_id,
            PolicyId = Policy#policy.policy_id,
            
            %% Emit decision latency metric
            router_metrics:emit_metric(router_policy_decision_latency_ms, #{value => DecisionLatency}, #{
                tenant_id => TenantId,
                policy_id => PolicyId,
                reason => Reason
            }),
            
            %% Emit decisions total counter
            router_metrics:emit_metric(router_policy_decisions_total, #{count => 1}, #{
                tenant_id => TenantId,
                policy_id => PolicyId,
                reason => Reason
            }),
            
            %% Record circuit breaker state initialization (if enabled)
            case Policy#policy.circuit_breaker of
                CB when CB =/= undefined andalso is_map(CB) ->
                    case maps:get(<<"enabled">>, CB, false) of
                        true when TenantId =/= undefined ->
                            %% Use record_state_with_config to pass policy configuration
                            router_circuit_breaker:record_state_with_config(TenantId, ProviderId, CB);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end,
            
            %% Build explanation for audit (with detail level from context)
            DetailLevel = maps:get(<<"detail_level">>, MergedContextWithTenant, <<"detailed">>),
            Explanation = build_explanation(Decision, Policy, MergedContextWithTenant, DetailLevel),
            
            %% Extract extensions from policy
            ExtensionsMap = extract_extensions(Policy),
            
            %% Emit telemetry
            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [policy_applied], #{count => 1}, #{
                tenant_id => TenantId,
                policy_id => PolicyId,
                provider_id => ProviderId,
                reason => Reason
            }),
            
            {ok, #{
                provider_id => ProviderId,
                extensions => ExtensionsMap,
                explanation => Explanation
            }};
        {error, ErrorInfo} ->
            DecisionEndTime = erlang:monotonic_time(millisecond),
            DecisionLatency = DecisionEndTime - DecisionStartTime,
            TenantId = Policy#policy.tenant_id,
            PolicyId = Policy#policy.policy_id,
            
            %% Emit decision latency metric even on error
            router_metrics:emit_metric(router_policy_decision_latency_ms, #{value => DecisionLatency}, #{
                tenant_id => TenantId,
                policy_id => PolicyId,
                reason => <<"error">>
            }),
            
            {error, ErrorInfo}
    end.

%% Internal: Build explanation for audit
%% DetailLevel: "minimal" | "detailed" | "verbose" (default: "detailed")
-spec build_explanation(#route_decision{}, #policy{}, map(), binary()) -> map().
build_explanation(Decision, Policy, Context, DetailLevel) ->
    #route_decision{
        provider_id = ProviderId,
        reason = Reason,
        priority = Priority,
        metadata = DecisionMetadata
    } = Decision,
    
    #policy{
        policy_id = PolicyId,
        version = Version,
        weights = Weights,
        sticky = Sticky,
        fallbacks = Fallbacks
    } = Policy,
    
    %% Build steps list (with detail level)
    Steps = build_explanation_steps(Reason, Sticky, Weights, Fallbacks, Context, DetailLevel),
    
    %% Build explanation according to formal specification in ROUTING_POLICY.md
    %% Required fields: reason, provider_id, policy_id, policy_version, priority, steps, context
    MergedContext = maps:merge(Context, DecisionMetadata),
    
    %% Ensure tenant_id is always present in context (required field per specification)
    FinalContext = case maps:get(<<"tenant_id">>, MergedContext, undefined) of
        undefined ->
            %% Set to "unknown" if not available
            maps:put(<<"tenant_id">>, <<"unknown">>, MergedContext);
        _ ->
            MergedContext
    end,
    
    Explanation = #{
        reason => Reason,                    % Required: "sticky" | "weighted" | "fallback" | "retry"
        provider_id => ProviderId,           % Required: selected provider ID
        policy_id => PolicyId,              % Required: policy ID used
        policy_version => Version,          % Required: policy version (e.g., "1.0")
        priority => Priority,                % Required: decision priority (25, 50, 100)
        steps => Steps,                     % Required: array of step-by-step explanation strings
        context => FinalContext             % Required: context with tenant_id (required) and optional fields
    },
    
    Explanation.

%% Internal: Build explanation steps
%% DetailLevel: "minimal" | "detailed" | "verbose" (default: "detailed")
-spec build_explanation_steps(binary(), map() | undefined, map(), list(), map(), binary()) -> [binary()].
build_explanation_steps(Reason, Sticky, Weights, Fallbacks, Context, DetailLevel) ->
    %% Determine detail level (minimal, detailed, verbose)
    IsMinimal = DetailLevel =:= <<"minimal">>,
    IsVerbose = DetailLevel =:= <<"verbose">>,
    
    Steps = [],
    
    %% Step 1: Check sticky
    Steps1 = case Reason of
        <<"sticky">> ->
            SessionKey = case Sticky of
                StickyCfg when is_map(StickyCfg) ->
                    maps:get(<<"session_key">>, StickyCfg, <<"session_id">>);
                _ ->
                    <<"session_id">>
            end,
            SessionValue = case maps:get(SessionKey, Context, undefined) of
                undefined -> <<"undefined">>;
                SV -> SV
            end,
            case IsMinimal of
                true ->
                    [<<"1. Sticky session: found">> | Steps];
                false ->
                    case IsVerbose of
                        true ->
                            TTL = case Sticky of
                                StickyCfg2 when is_map(StickyCfg2) ->
                                    maps:get(<<"ttl">>, StickyCfg2, <<"unknown">>);
                                _ ->
                                    <<"unknown">>
                            end,
                            [<<"1. Checked sticky session: found existing provider for key ">>, SessionKey, <<" = ">>, SessionValue, <<" (TTL: ">>, TTL, <<")">> | Steps];
                        false ->
                            [<<"1. Checked sticky session: found existing provider for key ">>, SessionKey, <<" = ">>, SessionValue | Steps]
                    end
            end;
        _ ->
            case Sticky of
                StickyCfg when is_map(StickyCfg) ->
                    case maps:get(<<"enabled">>, StickyCfg, false) of
                        true ->
                            case IsMinimal of
                                true ->
                                    Steps;
                                _ ->
                                    [<<"1. Checked sticky session: no existing session found">> | Steps]
                            end;
                        _ ->
                            Steps
                    end;
                _ ->
                    Steps
            end
    end,
    
    %% Step 2: Apply weights
    Steps2 = case Reason of
        <<"weighted">> ->
            ProviderCount = map_size(Weights),
            TotalWeight = lists:sum([W || W <- maps:values(Weights)]),
            TotalWeightStr = case TotalWeight of
                W when is_float(W) ->
                    list_to_binary(io_lib:format("~.2f", [W]));
                W when is_integer(W) ->
                    integer_to_binary(W);
                _ ->
                    <<"0.00">>
            end,
            case IsMinimal of
                true ->
                    [<<"2. Weighted: ">>, integer_to_binary(ProviderCount), <<" providers">> | Steps1];
                false ->
                    case IsVerbose of
                        true ->
                            %% Verbose: include individual provider weights
                            WeightDetails = lists:map(fun({PId, W}) ->
                                WStr = case W of
                                    Wf when is_float(Wf) ->
                                        list_to_binary(io_lib:format("~.2f", [Wf]));
                                    Wi when is_integer(Wi) ->
                                        integer_to_binary(Wi);
                                    _ ->
                                        <<"0">>
                                end,
                                [PId, <<": ">>, WStr]
                            end, maps:to_list(Weights)),
                            WeightDetailsStr = lists:foldl(fun(Detail, Acc) ->
                                case Acc of
                                    <<>> -> iolist_to_binary(Detail);
                                    _ -> <<Acc/binary, ", ", (iolist_to_binary(Detail))/binary>>
                                end
                            end, <<>>, WeightDetails),
                            [<<"2. Applied weighted distribution: ">>, integer_to_binary(ProviderCount), <<" providers, total weight: ">>, TotalWeightStr, <<" (">>, WeightDetailsStr, <<")">> | Steps1];
                        false ->
                            [<<"2. Applied weighted distribution: ">>, integer_to_binary(ProviderCount), <<" providers, total weight: ">>, TotalWeightStr | Steps1]
                    end
            end;
        _ ->
            case map_size(Weights) > 0 of
                true ->
                    case IsMinimal of
                        true ->
                            Steps1;
                        _ ->
                            [<<"2. Skipped weighted distribution (provider selected via ">>, Reason, <<")">> | Steps1]
                    end;
                false ->
                    Steps1
            end
    end,
    
    %% Step 3: Check fallbacks (with retry/backoff info)
    Steps3 = case Reason of
        <<"fallback">> ->
            FallbackCount = length(Fallbacks),
            RetryAttemptsUsed = maps:get(<<"retry_attempts_used">>, Context, undefined),
            RetryMax = maps:get(<<"retry_max">>, Context, undefined),
            Step3Base = case {RetryAttemptsUsed, RetryMax} of
                {undefined, undefined} ->
                    case IsMinimal of
                        true ->
                            [<<"3. Fallback applied">>];
                        _ ->
                            [<<"3. Applied fallback rule: ">>, integer_to_binary(FallbackCount), <<" fallback rules evaluated">>]
                    end;
                {Used, Max} when is_integer(Used), is_integer(Max) ->
                    case IsMinimal of
                        true ->
                            [<<"3. Fallback after retry">>];
                        false ->
                            case IsVerbose of
                                true ->
                                    FallbackRuleId = maps:get(<<"fallback_rule_id">>, Context, <<"unknown">>),
                                    [<<"3. Applied fallback rule after ">>, integer_to_binary(Used), <<"/">>, integer_to_binary(Max), <<" retry attempts exhausted (rule: ">>, FallbackRuleId, <<")">>];
                                false ->
                                    [<<"3. Applied fallback rule after ">>, integer_to_binary(Used), <<"/">>, integer_to_binary(Max), <<" retry attempts exhausted">>]
                            end
                    end;
                _ ->
                    case IsMinimal of
                        true ->
                            [<<"3. Fallback applied">>];
                        _ ->
                            [<<"3. Applied fallback rule: ">>, integer_to_binary(FallbackCount), <<" fallback rules evaluated">>]
                    end
            end,
            [Step3Base | Steps2];
        <<"retry">> ->
            RetryAttempt = maps:get(<<"retry_attempt">>, Context, undefined),
            RetryMax = maps:get(<<"retry_max">>, Context, undefined),
            BackoffMs = maps:get(<<"backoff_ms">>, Context, undefined),
            Step3Retry = case {RetryAttempt, RetryMax, BackoffMs} of
                {Attempt, Max, BMs} when is_integer(Attempt), is_integer(Max), is_integer(BMs) ->
                    case IsMinimal of
                        true ->
                            [<<"3. Retry ">>, integer_to_binary(Attempt), <<"/">>, integer_to_binary(Max)];
                        false ->
                            case IsVerbose of
                                true ->
                                    BackoffStrategy = maps:get(<<"backoff_strategy">>, Context, <<"unknown">>),
                                    [<<"3. Retry attempt ">>, integer_to_binary(Attempt), <<"/">>, integer_to_binary(Max), <<" with backoff ">>, integer_to_binary(BMs), <<"ms (strategy: ">>, BackoffStrategy, <<")">>];
                                false ->
                                    [<<"3. Retry attempt ">>, integer_to_binary(Attempt), <<"/">>, integer_to_binary(Max), <<" with backoff ">>, integer_to_binary(BMs), <<"ms">>]
                            end
                    end;
                {Attempt, Max, _} when is_integer(Attempt), is_integer(Max) ->
                    case IsMinimal of
                        true ->
                            [<<"3. Retry ">>, integer_to_binary(Attempt), <<"/">>, integer_to_binary(Max)];
                        _ ->
                            [<<"3. Retry attempt ">>, integer_to_binary(Attempt), <<"/">>, integer_to_binary(Max)]
                    end;
                _ ->
                    case IsMinimal of
                        true ->
                            [<<"3. Retry">>];
                        _ ->
                            [<<"3. Retry attempt (retry count not exhausted)">>]
                    end
            end,
            [Step3Retry | Steps2];
        _ ->
            case length(Fallbacks) > 0 of
                true ->
                    case IsMinimal of
                        true ->
                            Steps2;
                        _ ->
                            [<<"3. Skipped fallbacks (provider selected via ">>, Reason, <<")">> | Steps2]
                    end;
                false ->
                    Steps2
            end
    end,
    
    %% Reverse to get chronological order
    lists:reverse(Steps3).

%% Internal: Extract extensions from policy
extract_extensions(Policy) ->
    #policy{
        pre = Pre,
        validators = Validators,
        post = Post
    } = Policy,
    
    #{
        pre => Pre,
        validators => Validators,
        post => Post
    }.

%% Internal: Check policy rate limit
check_policy_rate_limit(Policy, TenantId, PolicyId) ->
    #policy{rate_limit = RateLimit} = Policy,
    case RateLimit of
        undefined ->
            {ok, allow};
        RateLimitMap when is_map(RateLimitMap) ->
            Enabled = maps:get(<<"enabled">>, RateLimitMap, false),
            case Enabled of
                false ->
                    {ok, allow};
                true ->
                    case router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, RateLimitMap) of
                        {ok, allow} -> {ok, allow};
                        {error, {rate_limit_exceeded, Details}} -> {error, {rate_limit_exceeded, Details}};
                        Other -> {error, {rate_limit_error, Other}}
                    end
            end;
        _ ->
            {ok, allow}
    end.

%% ============================================================================
%% DSL Parsing: from_map/2
%% ============================================================================

%% @doc Parse a policy map (JSON-like) into a #policy{} record
%% Used for Policy DSL/CP1 rule parsing
%% @param TenantId The tenant identifier
%% @param PolicyMap A map representing a policy in DSL format
%% @returns #policy{} record
-spec from_map(binary(), map()) -> #policy{}.
from_map(TenantId, PolicyMap) when is_binary(TenantId), is_map(PolicyMap) ->
    PolicyId = maps:get(<<"policy_id">>, PolicyMap, <<"default">>),
    
    %% Parse providers array into weights map, or use legacy weights format
    Weights = case maps:get(<<"providers">>, PolicyMap, undefined) of
        undefined ->
            %% Check for legacy <<"weights">> format (direct map)
            case maps:get(<<"weights">>, PolicyMap, undefined) of
                undefined -> #{};
                LegacyWeights when is_map(LegacyWeights) -> LegacyWeights;
                _ -> #{}
            end;
        Providers when is_list(Providers) ->
            parse_providers(Providers);
        _ -> #{}
    end,
    
    %% Parse fallbacks array
    Fallbacks = parse_fallbacks(maps:get(<<"fallbacks">>, PolicyMap, [])),
    
    %% Parse legacy fallback (single object)
    Fallback = maps:get(<<"fallback">>, PolicyMap, undefined),
    
    %% Parse sticky session configuration
    Sticky = parse_sticky(maps:get(<<"sticky">>, PolicyMap, undefined)),
    
    %% Parse extensions
    Extensions = maps:get(<<"extensions">>, PolicyMap, #{}),
    Pre = parse_extensions_list(maps:get(<<"pre">>, Extensions, [])),
    Validators = parse_extensions_list(maps:get(<<"validators">>, Extensions, [])),
    Post = parse_extensions_list(maps:get(<<"post">>, Extensions, [])),
    
    %% Parse rate limit and circuit breaker
    RateLimit = maps:get(<<"rate_limit">>, PolicyMap, undefined),
    CircuitBreaker = maps:get(<<"circuit_breaker">>, PolicyMap, undefined),
    
    %% Parse metadata
    Metadata = maps:get(<<"metadata">>, PolicyMap, #{}),
    
    #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = Weights,
        fallback = Fallback,
        fallbacks = Fallbacks,
        sticky = Sticky,
        rate_limit = RateLimit,
        circuit_breaker = CircuitBreaker,
        pre = Pre,
        validators = Validators,
        post = Post,
        metadata = Metadata
    }.

%% @doc Parse providers array into weights map
%% Converts [{id, weight}, ...] array to #{provider_id => normalized_weight}
-spec parse_providers(list()) -> map().
parse_providers(Providers) when is_list(Providers) ->
    %% Calculate total weight for normalization
    TotalWeight = lists:foldl(fun(Provider, Acc) ->
        Weight = maps:get(<<"weight">>, Provider, 0),
        Acc + Weight
    end, 0, Providers),
    
    %% Build weights map with normalized weights
    lists:foldl(fun(Provider, Acc) ->
        Id = maps:get(<<"id">>, Provider, undefined),
        Weight = maps:get(<<"weight">>, Provider, 0),
        NormalizedWeight = case TotalWeight of
            0 -> 0.0;
            _ -> Weight / TotalWeight
        end,
        case Id of
            undefined -> Acc;
            _ -> maps:put(Id, NormalizedWeight, Acc)
        end
    end, #{}, Providers);
parse_providers(_) ->
    #{}.

%% @doc Parse fallbacks array
%% Each fallback has: when conditions, to provider, retry config
-spec parse_fallbacks(list()) -> list().
parse_fallbacks(Fallbacks) when is_list(Fallbacks) ->
    lists:map(fun(Fallback) ->
        WhenConditions = maps:get(<<"when">>, Fallback, []),
        ToProvider = maps:get(<<"to">>, Fallback, undefined),
        Retry = maps:get(<<"retry">>, Fallback, undefined),
        #{
            when_conditions => WhenConditions,
            to => ToProvider,
            retry => Retry
        }
    end, Fallbacks);
parse_fallbacks(_) ->
    [].

%% @doc Parse sticky session configuration
%% Converts TTL strings like "5m", "1h" to milliseconds
-spec parse_sticky(map() | undefined) -> map() | undefined.
parse_sticky(undefined) -> undefined;
parse_sticky(Sticky) when is_map(Sticky) ->
    Enabled = maps:get(<<"enabled">>, Sticky, false),
    Key = maps:get(<<"key">>, Sticky, <<"session_id">>),
    TtlStr = maps:get(<<"ttl">>, Sticky, <<"5m">>),
    TtlMs = parse_duration_to_ms(TtlStr),
    #{
        enabled => Enabled,
        key => Key,
        ttl_ms => TtlMs
    };
parse_sticky(_) -> undefined.

%% @doc Parse duration string to milliseconds
%% Supports: "5m" (minutes), "1h" (hours), "30s" (seconds)
-spec parse_duration_to_ms(binary() | integer()) -> integer().
parse_duration_to_ms(Ms) when is_integer(Ms) -> Ms;
parse_duration_to_ms(DurationStr) when is_binary(DurationStr) ->
    DurationList = binary_to_list(DurationStr),
    case re:run(DurationList, "^([0-9]+)([smh])$", [{capture, all_but_first, list}]) of
        {match, [NumStr, "s"]} ->
            list_to_integer(NumStr) * 1000;
        {match, [NumStr, "m"]} ->
            list_to_integer(NumStr) * 60 * 1000;
        {match, [NumStr, "h"]} ->
            list_to_integer(NumStr) * 60 * 60 * 1000;
        _ ->
            %% Default: treat as milliseconds
            case catch list_to_integer(DurationList) of
                Ms when is_integer(Ms) -> Ms;
                _ -> 300000  %% Default: 5 minutes
            end
    end.

%% @doc Parse extensions list
%% Each extension has: id, config (optional)
-spec parse_extensions_list(list()) -> list().
parse_extensions_list(Extensions) when is_list(Extensions) ->
    lists:map(fun(Ext) ->
        #{
            id => maps:get(<<"id">>, Ext, undefined),
            config => maps:get(<<"config">>, Ext, #{})
        }
    end, Extensions);
parse_extensions_list(_) ->
    [].
