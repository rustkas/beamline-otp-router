-module(router_decider).

-doc "Route Decision Engine".
%%
%% Core routing logic with extensions pipeline support.
%% Implements provider selection algorithm (weights/sticky/fallback) and
%% executes extension pipeline: pre → validators → provider → post.
%%
%% Integration:
%% - Called by router_grpc for Decide requests
%% - Uses router_policy_applier for policy evaluation
%% - Integrates with router_caf_adapter for assignment publishing
%% - Supports OpenTelemetry tracing (CP2)
%%
%% Pipeline:
%% 1. Pre-processors: Transform request before validation
%% 2. Validators: Validate request against policy rules
%% 3. Provider Selection: Choose provider based on algorithm
%% 4. Post-processors: Transform response after decision
%%
%% @see DEVELOPER_GUIDE.md For development workflow and key modules
%% @see PERFORMANCE_GUIDE.md For performance tuning and optimization strategies
%% @see INTEGRATION_GUIDE.md#decider-integration For integration procedures
%% @see src/router_grpc.erl gRPC API entry point
%% @see src/router_policy_applier.erl Policy evaluation
%% @see src/router_caf_adapter.erl Assignment publishing
-export([decide/3, execute_post_processors/3, execute_provider_selection/5]).
-export([calculate_pipeline_complexity/3, get_pipeline_complexity/2]).
-export([validate_provider_selection/2, get_provider_selection_status/1, list_available_providers/1]).
-export([evaluate_when_condition/2]).



-include("beamline_router.hrl").

%% ============================================================================
%% Public API
%% ============================================================================

-spec decide(#route_request{}, #policy{}, map()) -> {ok, #route_decision{}} | {error, term()}.
decide(RouteRequest, Policy, Context) ->
    #route_request{message = Message, context = ReqContext} = RouteRequest,
    #policy{pre = Pre, validators = Validators, post = Post} = Policy,
    
    %% Validate pipeline depth (security: prevent DoS)
    case validate_pipeline_depth(Pre, Validators, Post) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            %% Calculate and log pipeline complexity
            Complexity = calculate_pipeline_complexity(Pre, Validators, Post),
            PolicyId = Policy#policy.policy_id,
            TenantId = maps:get(~"tenant_id", Message, undefined),
            log_pipeline_complexity(Complexity, PolicyId, TenantId),
            
            %% Prepare merged context
            MergedContext = prepare_context(ReqContext, Context, Message),
            
            %% Execute pipeline: pre → validators → provider selection
            execute_pipeline(Pre, Validators, RouteRequest, Policy, Message, MergedContext)
    end.

-spec execute_post_processors(list(), map(), map()) -> {ok, map(), map()} | {error, term()}.
execute_post_processors([], Response, Context) ->
    {ok, Response, Context};
execute_post_processors([PostItem | Rest], Response, Context) ->
    case execute_post_processor_item(PostItem, Response, Context) of
        {skip, SkippedPayload, SkippedContext} ->
            execute_post_processors(Rest, SkippedPayload, SkippedContext);
        {ok, ProcessedPayload, ProcessedContext, Status} ->
            %% Track executed extension in context
            ExtId = maps:get(id, PostItem, undefined),
            UpdatedContext = case ExtId of
                undefined -> ProcessedContext;
                _ ->
                    CurrentExtensions = maps:get(<<"executed_extensions">>, ProcessedContext, []),
                    SafeStatus = case Status of
                        S when is_atom(S) -> atom_to_binary(S, utf8);
                        {error, _} -> <<"error">>;
                        S when is_binary(S) -> S;
                        _ -> <<"unknown">>
                    end,
                    NewExtensions = [#{type => <<"post">>, id => ExtId, status => SafeStatus} | CurrentExtensions],
                    maps:put(<<"executed_extensions">>, NewExtensions, ProcessedContext)
            end,
            execute_post_processors(Rest, ProcessedPayload, UpdatedContext);
        {error, Reason} ->
            {error, Reason}
    end.

%% Optimized: Cache application env lookups to reduce repeated calls
-spec calculate_pipeline_complexity(list(), list(), list()) -> map().
calculate_pipeline_complexity(Pre, Validators, Post) ->
    PreCount = length(Pre),
    ValidatorsCount = length(Validators),
    PostCount = length(Post),
    TotalCount = PreCount + ValidatorsCount + PostCount,
    
    %% Optimization: Cache application env lookups (these are called frequently)
    RecommendedMaxTotal = application:get_env(beamline_router, extension_recommended_max_pipeline_total, 4),
    RecommendedMaxPre = application:get_env(beamline_router, extension_recommended_max_pre_count, 2),
    RecommendedMaxValidators = application:get_env(beamline_router, extension_recommended_max_validators_count, 2),
    RecommendedMaxPost = application:get_env(beamline_router, extension_recommended_max_post_count, 2),
    EstimatedLatencyPerExtension = application:get_env(beamline_router, extension_estimated_latency_per_extension_ms, 30),
    
    %% Calculate values once
    ComplexityScore = calculate_complexity_score(PreCount, ValidatorsCount, PostCount, RecommendedMaxTotal),
    EstimatedLatencyMs = TotalCount * EstimatedLatencyPerExtension,
    ComplexityLevel = determine_complexity_level(ComplexityScore),
    
    %% Only generate warnings/recommendations if needed (optimization: avoid unnecessary work)
    Warnings = case ComplexityScore > 0.8 of
        true -> generate_complexity_warnings(PreCount, ValidatorsCount, PostCount,
                                            RecommendedMaxPre, RecommendedMaxValidators, RecommendedMaxPost,
                                            RecommendedMaxTotal, EstimatedLatencyMs);
        false -> []
    end,
    Recommendations = case ComplexityScore > 0.6 of
        true -> generate_complexity_recommendations(PreCount, ValidatorsCount, PostCount,
                                                   RecommendedMaxPre, RecommendedMaxValidators, RecommendedMaxPost,
                                                   RecommendedMaxTotal);
        false -> []
    end,
    
    #{
        total_extensions => TotalCount,
        pre_count => PreCount,
        validators_count => ValidatorsCount,
        post_count => PostCount,
        complexity_score => ComplexityScore,
        complexity_level => ComplexityLevel,
        estimated_latency_ms => EstimatedLatencyMs,
        recommended_limits => #{
            max_total => RecommendedMaxTotal,
            max_pre => RecommendedMaxPre,
            max_validators => RecommendedMaxValidators,
            max_post => RecommendedMaxPost
        },
        warnings => Warnings,
        recommendations => Recommendations
    }.

-spec get_pipeline_complexity(binary(), binary()) -> {ok, map()} | {error, term()}.
get_pipeline_complexity(TenantId, PolicyId) ->
    case router_policy_store:get_policy(TenantId, PolicyId) of
        {ok, Policy} ->
            Complexity = calculate_pipeline_complexity(
                Policy#policy.pre,
                Policy#policy.validators,
                Policy#policy.post
            ),
            {ok, Complexity};
        {error, _Reason} = Error ->
            Error
    end.

%% ============================================================================
%% Internal: Pipeline Execution
%% ============================================================================

-spec execute_pipeline(list(), list(), #route_request{}, #policy{}, map(), map()) ->
    {ok, #route_decision{}} | {error, term()}.
execute_pipeline(Pre, Validators, RouteRequest, Policy, Message, MergedContext) ->
    %% DEBUG
    io:format("DEBUG: execute_pipeline Pre=~p Validators=~p Post=~p~n", [length(Pre), length(Validators), length(Policy#policy.post)]),
    %% Check execution status (deadline/cancel) before starting
    case check_execution_status(MergedContext) of
        ok ->
            %% Initialize executed extensions tracking
            InitialExecutedExtensions = [],
            case execute_pre_processors(Pre, Message, MergedContext, InitialExecutedExtensions) of
                {ok, ProcessedMessage, ProcessedContext, ExecutedPre} ->
                    %% Check status after pre-processors
                    case check_execution_status(ProcessedContext) of
                        ok ->
                            case execute_validators(Validators, ProcessedMessage, ProcessedContext, ExecutedPre) of
                                {ok, ValidatedMessage, ValidatedContext, ExecutedPreAndValidators} ->
                                    %% Check status after validators
                                    case check_execution_status(ValidatedContext) of
                                        ok ->
                                            %% Store executed extensions in context metadata for later retrieval
                                            ContextWithExtensions = maps:put(<<"executed_extensions">>, ExecutedPreAndValidators, ValidatedContext),
                                            case execute_provider_selection(RouteRequest, Policy, ValidatedMessage, ContextWithExtensions, MergedContext) of
                                                {ok, Decision} ->
                                                    %% Execute post-processors
                                                    %% We convert decision fields to a map for post-processors payload
                                                    DecisionPayload = #{
                                                        <<"provider_id">> => Decision#route_decision.provider_id,
                                                        <<"reason">> => Decision#route_decision.reason,
                                                        <<"priority">> => Decision#route_decision.priority
                                                    },
                                                    DecisionContext = Decision#route_decision.metadata,
                                                    case execute_post_processors(Policy#policy.post, DecisionPayload, DecisionContext) of
                                                        {ok, _PostPayload, PostContext} ->
                                                            %% Update decision with new context/metadata (including post extensions)
                                                            FinalDecision = Decision#route_decision{metadata = PostContext},
                                                            {ok, FinalDecision};
                                                        {error, Reason} ->
                                                            enrich_error_with_extensions({error, Reason}, maps:get(<<"executed_extensions">>, DecisionContext, []))
                                                    end;
                                                Error -> Error
                                            end;
                                        Error -> 
                                            enrich_error_with_extensions(Error, ExecutedPreAndValidators)
                                    end;
                                {error, Reason} ->
                                    %% Validators already enriched (or partially), but let's ensure consistency if needed
                                    %% execute_validators currently enriches validator_blocked.
                                    {error, Reason}
                            end;
                        Error -> 
                            enrich_error_with_extensions(Error, ExecutedPre)
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        Error -> Error
    end.

-spec check_execution_status(map()) -> ok | {error, term()}.
check_execution_status(Context) ->
    case maps:get(<<"deadline">>, Context, undefined) of
        undefined ->
            ok;
        Deadline when is_integer(Deadline) ->
            Now = erlang:system_time(millisecond),
            case Now > Deadline of
                true ->
                    {error, {deadline_exceeded, #{
                        deadline => Deadline,
                        now => Now,
                        context => Context
                    }}};
                false ->
                    ok
            end;
        _ ->
            ok
    end.

-spec enrich_error_with_extensions({error, term()}, list()) -> {error, term()}.
enrich_error_with_extensions({error, Reason}, ExecutedExtensions) ->
    {error, enrich_error_reason(Reason, ExecutedExtensions)}.

enrich_error_reason({ErrorType, Info}, ExecutedExtensions) when is_map(Info) ->
    {ErrorType, maps:put(<<"executed_extensions">>, lists:reverse(ExecutedExtensions), Info)};
enrich_error_reason(Reason, ExecutedExtensions) ->
    {extension_failed, #{
        reason => Reason,
        executed_extensions => lists:reverse(ExecutedExtensions)
    }}.

-spec execute_provider_selection(#route_request{}, #policy{}, map(), map(), map()) ->
    {ok, #route_decision{}} | {error, term()}.
execute_provider_selection(_RouteRequest, Policy, Message, ProcessedContext, OriginalContext) ->
    TenantId = maps:get(~"tenant_id", Message, undefined),
    
    %% Start OpenTelemetry span for provider selection
    SpanAttributes = #{
        ~"tenant_id" => TenantId,
        ~"policy_id" => Policy#policy.policy_id
    },
    TraceContext = router_tracing:extract_trace_context(OriginalContext),
    
    Res = router_tracing:with_span(
        ~"beamline.router.provider.select",
        SpanAttributes,
        TraceContext,
        fun() ->
            Decision = select_provider(Policy, TenantId, ProcessedContext, OriginalContext),
            annotate_span_with_decision(Decision),
            Decision
        end),
    %% Cast Result to satisfy eqWAlizer
    case Res of
        {ok, #route_decision{} = D} -> {ok, D};
        {error, R} -> {error, R}
    end.

-spec select_provider(#policy{}, binary() | undefined, map(), map()) ->
    {ok, #route_decision{}} | {error, term()}.
select_provider(Policy, TenantId, ProcessedContext, OriginalContext) ->
    #policy{circuit_breaker = CircuitBreaker} = Policy,
    
    %% Check circuit breaker before provider selection
    case check_circuit_breaker_before_selection(CircuitBreaker, TenantId, ProcessedContext, OriginalContext) of
        {error, circuit_open, FallbackProvider} when FallbackProvider =/= undefined ->
            create_decision(FallbackProvider, ~"circuit_breaker_fallback", 20, OriginalContext);
        {error, circuit_open, undefined} ->
            continue_provider_selection(Policy, TenantId, ProcessedContext, OriginalContext);
        {ok, allow} ->
            continue_provider_selection(Policy, TenantId, ProcessedContext, OriginalContext)
    end.

-spec continue_provider_selection(#policy{}, binary() | undefined, map(), map()) ->
    {ok, #route_decision{}} | {error, term()}.
continue_provider_selection(Policy, TenantId, ProcessedContext, OriginalContext) ->
    #policy{weights = Weights, fallback = Fallback, fallbacks = Fallbacks, sticky = Sticky} = Policy,
    
    %% Step 1: Check sticky session
    case check_sticky(Sticky, ProcessedContext) of
        {ok, ProviderId} ->
            record_sticky_hit(TenantId),
            store_sticky_provider(TenantId, Sticky, ProcessedContext, ProviderId),
            record_circuit_breaker_state(Policy, TenantId, ProviderId),
            create_decision(ProviderId, ~"sticky", 100, ProcessedContext);
        {error, not_found} ->
            record_sticky_miss(TenantId),
            try_weighted_selection(Weights, Sticky, Policy, TenantId, ProcessedContext, OriginalContext, Fallbacks, Fallback)
    end.

-spec try_weighted_selection(map(), map() | undefined, #policy{}, binary() | undefined, map(), map(), list(), map() | undefined) ->
    {ok, #route_decision{}} | {error, term()}.
try_weighted_selection(Weights, Sticky, Policy, TenantId, ProcessedContext, OriginalContext, Fallbacks, Fallback) ->
    case apply_weights(Weights) of
        {ok, ProviderId} ->
            store_sticky_provider(TenantId, Sticky, ProcessedContext, ProviderId),
            record_circuit_breaker_state(Policy, TenantId, ProviderId),
            create_decision(ProviderId, ~"weighted", 50, ProcessedContext);
        {error, no_providers} ->
            try_fallbacks(Fallbacks, Fallback, ProcessedContext, OriginalContext, Policy)
    end.

-spec try_fallbacks(list(), map() | undefined, map(), map(), #policy{}) ->
    {ok, #route_decision{}} | {error, term()}.
try_fallbacks(Fallbacks, Fallback, ProcessedContext, OriginalContext, Policy) ->
    case check_fallbacks_with_retry(Fallbacks, ProcessedContext, OriginalContext) of
        {ok, FallbackProvider, RetryInfo} ->
            DecisionMetadata = merge_retry_info(ProcessedContext, RetryInfo),
            create_decision(FallbackProvider, ~"fallback", 25, DecisionMetadata);
        {retry, ProviderId, RetryInfo} ->
            DecisionMetadata = merge_retry_info(ProcessedContext, RetryInfo),
            create_decision(ProviderId, ~"retry", 50, DecisionMetadata);
        {error, no_fallback} ->
            try_legacy_fallback(Fallback, ProcessedContext, OriginalContext, Policy)
    end.

-spec try_legacy_fallback(map() | undefined, map(), map(), #policy{}) ->
    {ok, #route_decision{}} | {error, term()}.
try_legacy_fallback(Fallback, ProcessedContext, _OriginalContext, Policy) ->
    case check_fallback(Fallback) of
        {ok, FallbackProvider} ->
            create_decision(FallbackProvider, ~"fallback", 25, ProcessedContext);
        {error, no_fallback} ->
            {error, {no_provider_available, #{
                reason => ~"No provider available: weights failed and no fallback configured",
                tenant_id => maps:get(~"tenant_id", ProcessedContext, undefined),
                policy_id => Policy#policy.policy_id,
                context => ProcessedContext
            }}}
    end.

%% ============================================================================
%% Internal: Pre-processors
%% ============================================================================

-spec execute_pre_processors(list(), map(), map(), list()) -> {ok, map(), map(), list()} | {error, term()}.
execute_pre_processors([], Message, Context, ExecutedExtensions) ->
    {ok, Message, Context, ExecutedExtensions};
execute_pre_processors([PreItem | Rest], Message, Context, ExecutedExtensions) ->
    case execute_pre_processor_item(PreItem, Message, Context) of
        {skip, SkippedMessage, SkippedContext} ->
            execute_pre_processors(Rest, SkippedMessage, SkippedContext, ExecutedExtensions);
        {ok, ProcessedPayload, ProcessedContext, Status} ->
            ExtId = maps:get(id, PreItem, undefined),
            NewExecutedExtensions = case ExtId of
                undefined -> ExecutedExtensions;
                _ -> 
                    SafeStatus = case Status of
                        S when is_atom(S) -> atom_to_binary(S, utf8);
                        {error, _} -> <<"error">>;
                        S when is_binary(S) -> S;
                        _ -> <<"unknown">>
                    end,
                    [#{type => <<"pre">>, id => ExtId, status => SafeStatus} | ExecutedExtensions]
            end,
            execute_pre_processors(Rest, ProcessedPayload, ProcessedContext, NewExecutedExtensions);
        {error, Reason} ->
            enrich_error_with_extensions({error, Reason}, ExecutedExtensions)
    end.

-spec execute_pre_processor_item(map(), map(), map()) -> {ok, map(), map(), term()} | {skip, map(), map()} | {error, term()}.
execute_pre_processor_item(PreItem, Message, Context) ->
    case PreItem of
        #{id := ExtId, mode := Mode} ->
            %% Check "when" condition (CP2: DAG v0)
            ShouldExecute = case maps:get(~"when", PreItem, undefined) of
                undefined -> true;
                WhenCondition -> evaluate_when_condition(WhenCondition, Context)
            end,

            case ShouldExecute of
                false ->
                    {skip, Message, Context};
                true ->
                    StartTime = erlang:system_time(microsecond),
            Config = maps:get(config, PreItem, #{}),
            Request = #{
                ~"payload" => Message,
                ~"config" => Config,
                ~"metadata" => Context
            },
            TenantId = maps:get(~"tenant_id", Context, maps:get(~"tenant_id", Message, undefined)),
            PolicyId = maps:get(~"policy_id", Context, maps:get(~"policy_id", Message, undefined)),
            
            %% Log extension execution start
            router_logger:debug(~"Executing pre-processor extension", #{
                ~"extension_id" => ExtId,
                ~"extension_type" => ~"pre",
                ~"tenant_id" => TenantId,
                ~"policy_id" => PolicyId
            }),
            
            Result = case router_extension_invoker:invoke(ExtId, Request, Context) of
                {ok, Response} ->
                    PayloadRaw = maps:get(~"payload", Response, Message),
                    %% Normalize payload: ensure downstream validators receive message map
                    NormalizedPayload = case is_map(PayloadRaw) of
                        true -> PayloadRaw;
                        false ->
                            case (is_binary(PayloadRaw) orelse is_list(PayloadRaw)) of
                                true -> maps:put(~"payload", PayloadRaw, Message);
                                false -> Message
                            end
                    end,
                    ProcessedContext = maps:merge(Context, maps:get(~"metadata", Response, #{})),
                    {ok, NormalizedPayload, ProcessedContext, ok};
                {error, Reason} ->
                    handle_pre_processor_error(Mode, ExtId, Reason, Context, Message)
            end,
            
            %% Calculate latency
            LatencyMs = (erlang:system_time(microsecond) - StartTime) / 1000.0,
            
            %% Emit metrics
            {Status, MetricStatus} = case Result of
                {ok, _, _, _} -> {success, ~"success"};
                {error, _} -> {error, ~"error"}
            end,
            router_metrics:emit_metric(router_extension_execution_total, #{count => 1}, #{
                extension_id => ExtId,
                extension_type => ~"pre",
                status => MetricStatus,
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            router_metrics:emit_metric(router_extension_execution_latency_ms, #{value => LatencyMs}, #{
                extension_id => ExtId,
                extension_type => ~"pre",
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            
            %% Log extension execution result
            case Status of
                success ->
                    router_logger:info(~"Pre-processor extension executed successfully", #{
                        ~"extension_id" => ExtId,
                        ~"extension_type" => ~"pre",
                        ~"latency_ms" => LatencyMs,
                        ~"tenant_id" => TenantId,
                        ~"policy_id" => PolicyId
                    });
                error ->
                    router_logger:warn(~"Pre-processor extension execution failed", #{
                        ~"extension_id" => ExtId,
                        ~"extension_type" => ~"pre",
                        ~"latency_ms" => LatencyMs,
                        ~"tenant_id" => TenantId,
                        ~"policy_id" => PolicyId
                    })
            end,
            
            Result
            end;
        _ ->
            %% Invalid pre-item, skip and continue
            {ok, Message, Context, ok}
    end.

-spec handle_pre_processor_error(binary(), binary(), term(), map(), map()) -> {ok, map(), map(), term()} | {error, term()}.
handle_pre_processor_error(~"required", ExtId, Reason, Context, _Message) ->
    {error, {pre_processor_failed, #{
        extension_id => ExtId,
        reason => normalize_error(Reason),
        context => Context
    }}};
handle_pre_processor_error(~"optional", ExtId, Reason, Context, Message) ->
    router_logger:warn(~"Pre-processor failed (optional)", #{
        ~"extension_id" => ExtId,
        ~"reason" => normalize_error(Reason)
    }),
    {ok, Message, Context, {error, Reason}};
handle_pre_processor_error(_, ExtId, Reason, Context, Message) ->
    %% Default to optional behavior
    router_logger:warn(~"Pre-processor failed (default optional)", #{
        ~"extension_id" => ExtId,
        ~"reason" => normalize_error(Reason)
    }),
    {ok, Message, Context, {error, Reason}}.

%% ============================================================================
%% Internal: Validators
%% ============================================================================

-spec execute_validators(list(), map(), map(), list()) -> {ok, map(), map(), list()} | {error, term()}.
execute_validators([], Message, Context, ExecutedExtensions) ->
    {ok, Message, Context, ExecutedExtensions};
execute_validators([ValidatorItem | Rest], Message, Context, ExecutedExtensions) ->
    case execute_validator_item(ValidatorItem, Message, Context) of
        {ok, ValidatedMessage, ValidatedContext, Status} ->
            %% Track executed extension
            ExtId = maps:get(id, ValidatorItem, undefined),
            NewExecutedExtensions = case ExtId of
                undefined -> ExecutedExtensions;
                _ -> [#{type => <<"validator">>, id => ExtId, status => atom_to_binary(Status, utf8)} | ExecutedExtensions]
            end,
            execute_validators(Rest, ValidatedMessage, ValidatedContext, NewExecutedExtensions);
        {error, Reason} ->
            EnrichedReason = case Reason of
                {validator_blocked, Info} when is_map(Info) ->
                    {validator_blocked, maps:put(executed_extensions, ExecutedExtensions, Info)};
                _ -> Reason
            end,
            {error, EnrichedReason}
    end.

-spec execute_validator_item(map(), map(), map()) -> {ok, map(), map(), atom()} | {error, term()}.
execute_validator_item(ValidatorItem, Message, Context) ->
    case ValidatorItem of
        #{id := ExtId, on_fail := OnFail} ->
            %% Check "when" condition (CP2: DAG v0)
            ShouldExecute = case maps:get(~"when", ValidatorItem, undefined) of
                undefined -> true;
                WhenCondition -> evaluate_when_condition(WhenCondition, Context)
            end,

            case ShouldExecute of
                false ->
                    %% Skip execution if condition not met
                    {ok, Message, Context, skipped};
                true ->
                    StartTime = erlang:system_time(microsecond),
                    Request = #{
                        ~"payload" => Message,
                        ~"metadata" => Context
                    },
            TenantId = maps:get(~"tenant_id", Context, maps:get(~"tenant_id", Message, undefined)),
            PolicyId = maps:get(~"policy_id", Context, maps:get(~"policy_id", Message, undefined)),
            
            %% Log extension execution start
            router_logger:debug(~"Executing validator extension", #{
                ~"extension_id" => ExtId,
                ~"extension_type" => ~"validator",
                ~"tenant_id" => TenantId,
                ~"policy_id" => PolicyId
            }),
            
            Result = case router_extension_invoker:invoke(ExtId, Request, Context) of
                {ok, Response} ->
                    Valid = maps:get(~"valid", Response, true),
                    case Valid of
                        true ->
                            {ok, Message, Context, ok};
                        false ->
                            handle_validator_failure(OnFail, ExtId, Message, Context)
                    end;
                {error, Reason} ->
                    handle_validator_failure(OnFail, ExtId, Message, Context, Reason)
            end,
            
            %% Calculate latency
            LatencyMs = (erlang:system_time(microsecond) - StartTime) / 1000.0,
            
            %% Emit metrics
            {Status, MetricStatus} = case Result of
                {ok, _, _, _} -> {success, ~"success"};
                {error, _} -> {error, ~"error"}
            end,
            router_metrics:emit_metric(router_extension_execution_total, #{count => 1}, #{
                extension_id => ExtId,
                extension_type => ~"validator",
                status => MetricStatus,
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            router_metrics:emit_metric(router_extension_execution_latency_ms, #{value => LatencyMs}, #{
                extension_id => ExtId,
                extension_type => ~"validator",
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            
            %% Log extension execution result
            case Status of
                success ->
                    router_logger:info(~"Validator extension executed successfully", #{
                        ~"extension_id" => ExtId,
                        ~"extension_type" => ~"validator",
                        ~"latency_ms" => LatencyMs,
                        ~"tenant_id" => TenantId,
                        ~"policy_id" => PolicyId
                    });
                error ->
                    router_logger:warn(~"Validator extension execution failed", #{
                        ~"extension_id" => ExtId,
                        ~"extension_type" => ~"validator",
                        ~"latency_ms" => LatencyMs,
                        ~"tenant_id" => TenantId,
                        ~"policy_id" => PolicyId
                    })
            end,
            
            Result
            end;
        _ ->
            %% Invalid validator item, skip and continue
            {ok, Message, Context, ok}
    end.

-spec handle_validator_failure(binary(), binary(), map(), map()) -> {ok, map(), map(), atom()} | {error, term()}.
handle_validator_failure(~"block", ExtId, _Message, Context) ->
    {error, {validator_blocked, #{
        extension_id => ExtId,
        reason => ~"Validator blocked request",
        context => Context
    }}};
handle_validator_failure(~"warn", ExtId, Message, Context) ->
    router_logger:warn(~"Validator warning", #{
        ~"extension_id" => ExtId,
        ~"context" => Context
    }),
    {ok, Message, Context, warn};
handle_validator_failure(~"ignore", _ExtId, Message, Context) ->
    {ok, Message, Context, ignore};
handle_validator_failure(_, ExtId, _Message, Context) ->
    {error, {validator_blocked, #{
        extension_id => ExtId,
        reason => ~"Unknown on_fail mode",
        context => Context
    }}}.

-spec handle_validator_failure(binary(), binary(), map(), map(), term()) -> {ok, map(), map(), atom()} | {error, term()}.
handle_validator_failure(~"block", ExtId, _Message, Context, Error) ->
    {error, {validator_blocked, #{
        extension_id => ExtId,
        reason => normalize_error(Error),
        context => Context
    }}};
handle_validator_failure(~"warn", ExtId, Message, Context, Error) ->
    router_logger:warn(~"Validator error", #{
        ~"extension_id" => ExtId,
        ~"reason" => normalize_error(Error),
        ~"context" => Context
    }),
    {ok, Message, Context, warn};
handle_validator_failure(~"ignore", _ExtId, Message, Context, _Error) ->
    {ok, Message, Context, ignore};
handle_validator_failure(_, ExtId, _Message, Context, Error) ->
    {error, {validator_blocked, #{
        extension_id => ExtId,
        reason => normalize_error(Error),
        context => Context
    }}}.

%% ============================================================================
%% Internal: Post-processors
%% ============================================================================

-spec execute_post_processor_item(map(), map(), map()) -> {ok, map(), map(), term()} | {skip, map(), map()} | {error, term()}.
execute_post_processor_item(PostItem, Response, Context) ->
    case PostItem of
        #{id := ExtId, mode := Mode} ->
            %% Check "when" condition (CP2: DAG v0)
            ShouldExecute = case maps:get(<<"when">>, PostItem, undefined) of
                undefined -> true;
                WhenCondition -> evaluate_when_condition(WhenCondition, Context)
            end,

            case ShouldExecute of
                false ->
                    %% Skip execution if condition not met
                    {skip, Response, Context};
                true ->
                    StartTime = erlang:system_time(microsecond),
                    Config = maps:get(config, PostItem, #{}),
                    Request = #{
                        <<"payload">> => Response,
                        <<"config">> => Config,
                        <<"metadata">> => Context
                    },
                    TenantId = maps:get(<<"tenant_id">>, Context, maps:get(<<"tenant_id">>, Response, undefined)),
                    PolicyId = maps:get(<<"policy_id">>, Context, maps:get(<<"policy_id">>, Response, undefined)),
            
            %% Log extension execution start
            router_logger:debug(<<"Executing post-processor extension">>, #{
                <<"extension_id">> => ExtId,
                <<"extension_type">> => <<"post">>,
                <<"tenant_id">> => TenantId,
                <<"policy_id">> => PolicyId
            }),
            
            Result = case router_extension_invoker:invoke(ExtId, Request, Context) of
                {ok, PostResponse} ->
                    ProcessedPayload = maps:get(<<"payload">>, PostResponse, Response),
                    ProcessedContext = maps:merge(Context, maps:get(<<"metadata">>, PostResponse, #{})),
                    {ok, ProcessedPayload, ProcessedContext, ok};
                {error, Reason} ->
                    handle_post_processor_error(Mode, ExtId, Reason, Context, Response)
            end,
            
            %% Calculate latency
            LatencyMs = (erlang:system_time(microsecond) - StartTime) / 1000.0,
            
            %% Emit metrics
            {Status, MetricStatus} = case Result of
                {ok, _, _, _} -> {success, <<"success">>};
                {error, _} -> {error, <<"error">>}
            end,
            router_metrics:emit_metric(router_extension_execution_total, #{count => 1}, #{
                extension_id => ExtId,
                extension_type => <<"post">>,
                status => MetricStatus,
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            router_metrics:emit_metric(router_extension_execution_latency_ms, #{value => LatencyMs}, #{
                extension_id => ExtId,
                extension_type => <<"post">>,
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            
            %% Log extension execution result
            case Status of
                success ->
                    router_logger:info(<<"Post-processor extension executed successfully">>, #{
                        <<"extension_id">> => ExtId,
                        <<"extension_type">> => <<"post">>,
                        <<"latency_ms">> => LatencyMs,
                        <<"tenant_id">> => TenantId,
                        <<"policy_id">> => PolicyId
                    });
                error ->
                    router_logger:warn(<<"Post-processor extension execution failed">>, #{
                        <<"extension_id">> => ExtId,
                        <<"extension_type">> => <<"post">>,
                        <<"latency_ms">> => LatencyMs,
                        <<"tenant_id">> => TenantId,
                        <<"policy_id">> => PolicyId
                    })
            end,
            
            Result
            end;
        _ ->
            %% Invalid post-item, skip and continue
            {ok, Response, Context, ok}
    end.

-spec handle_post_processor_error(binary(), binary(), term(), map(), map()) -> {ok, map(), map(), term()} | {error, term()}.
handle_post_processor_error(<<"required">>, ExtId, Reason, Context, _Response) ->
    {error, {post_processor_failed, #{
        extension_id => ExtId,
        reason => normalize_error(Reason),
        context => Context
    }}};
handle_post_processor_error(<<"optional">>, ExtId, Reason, Context, Response) ->
    router_logger:warn(<<"Post-processor failed (optional)">>, #{
        <<"extension_id">> => ExtId,
        <<"reason">> => normalize_error(Reason)
    }),
    {ok, Response, Context, {error, Reason}};
handle_post_processor_error(_, ExtId, Reason, Context, Response) ->
    %% Default to optional behavior
    router_logger:warn(<<"Post-processor failed (default optional)">>, #{
        <<"extension_id">> => ExtId,
        <<"reason">> => normalize_error(Reason)
    }),
    {ok, Response, Context, {error, Reason}}.

%% ============================================================================
%% Internal: Provider Selection
%% ============================================================================

-spec check_sticky(map() | undefined, map()) -> {ok, binary()} | {error, not_found}.
check_sticky(Sticky, Context) when is_map(Sticky) ->
    Enabled = maps:get(~"enabled", Sticky, maps:get(enabled, Sticky, false)),
    case Enabled of
        true ->
    TenantId = maps:get(~"tenant_id", Context, undefined),
    SessionKeyName = maps:get(~"session_key", Sticky, ~"session_id"),
    SessionValue = maps:get(SessionKeyName, Context, undefined),
    
    case {TenantId, SessionValue} of
        {T, S} when T =/= undefined, S =/= undefined ->
            case router_sticky_store:get_provider(T, S) of
                {ok, ProviderId} ->
                    {ok, ProviderId};
                {error, not_found} ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end;
        false ->
            {error, not_found}
    end;
check_sticky(_, _Context) ->
    {error, not_found}.

-spec apply_weights(map()) -> {ok, binary()} | {error, no_providers}.
apply_weights(Weights) when map_size(Weights) =:= 0 ->
    {error, no_providers};
apply_weights(Weights) ->
    Providers = maps:keys(Weights),
    TotalWeight = lists:sum([maps:get(P, Weights) || P <- Providers]),
    
    case TotalWeight =< 0.0 of
        true ->
            {error, no_providers};
        false ->
            Random = rand:uniform() * TotalWeight,
            select_provider_by_weight(Providers, Weights, Random, 0.0)
    end.

-spec select_provider_by_weight([binary()], map(), float(), float()) -> {ok, binary()} | {error, no_providers}.
select_provider_by_weight([Provider | Rest], Weights, Random, Accum) ->
    Weight = maps:get(Provider, Weights),
    NewAccum = Accum + Weight,
    case Random =< NewAccum of
        true ->
            {ok, Provider};
        false ->
            select_provider_by_weight(Rest, Weights, Random, NewAccum)
    end;
select_provider_by_weight([], _, _, _) ->
    {error, no_providers}.

-spec check_fallbacks_with_retry(list(), map(), map()) ->
    {ok, binary(), map()} | {retry, binary(), map()} | {error, no_fallback}.
check_fallbacks_with_retry([], _Context, _OriginalContext) ->
    {error, no_fallback};
check_fallbacks_with_retry([FallbackRule | Rest], Context, OriginalContext) when is_map(FallbackRule) ->
    When = maps:get(~"when", FallbackRule, #{}),
    case evaluate_when_condition(When, Context) of
        true ->
            RetryCount = maps:get(~"retry", FallbackRule, 1),
            To = maps:get(~"to", FallbackRule, undefined),
            BackoffConfig = maps:get(~"backoff", FallbackRule, undefined),
            CurrentProvider = maps:get(~"current_provider", Context, undefined),
            FallbackRuleId = get_fallback_rule_id(FallbackRule),
            RetryAttemptsKey = <<"retry_attempts_", FallbackRuleId/binary>>,
            CurrentAttempts = maps:get(RetryAttemptsKey, Context, 0),
            
            case CurrentAttempts < RetryCount of
                true ->
                    NextAttempts = CurrentAttempts + 1,
                    BackoffMs = calculate_backoff(NextAttempts, BackoffConfig),
                    RetryInfo = #{
                        ~"fallback_rule_id" => FallbackRuleId,
                        ~"retry_attempt" => NextAttempts,
                        ~"retry_max" => RetryCount,
                        ~"backoff_ms" => BackoffMs,
                        ~"backoff_applied" => true
                    },
                    case CurrentProvider of
                        undefined ->
                            check_fallbacks_with_retry(Rest, Context, OriginalContext);
                        ProviderId ->
                            {retry, ProviderId, RetryInfo}
                    end;
                false ->
                    case To of
                        undefined ->
                            check_fallbacks_with_retry(Rest, Context, OriginalContext);
                        FallbackProvider ->
                            RetryInfo = #{
                                ~"fallback_rule_id" => FallbackRuleId,
                                ~"retry_attempts_used" => CurrentAttempts,
                                ~"retry_max" => RetryCount,
                                ~"fallback_applied" => true
                            },
                            {ok, FallbackProvider, RetryInfo}
                    end
            end;
        false ->
            check_fallbacks_with_retry(Rest, Context, OriginalContext)
    end;
check_fallbacks_with_retry([_ | Rest], Context, OriginalContext) ->
    check_fallbacks_with_retry(Rest, Context, OriginalContext);
check_fallbacks_with_retry(_, _Context, _OriginalContext) ->
    {error, no_fallback}.

-spec check_fallback(term()) -> {ok, binary()} | {error, no_fallback}.
check_fallback(undefined) ->
    {error, no_fallback};
check_fallback(Fallback) when not(is_map(Fallback)) ->
    {error, no_fallback};
check_fallback(Fallback) ->
    case maps:get(~"provider", Fallback, undefined) of
        Provider when is_binary(Provider) ->
            {ok, Provider};
        _ ->
            {error, no_fallback}
    end.

%% ============================================================================
%% Internal: Circuit Breaker
%% ============================================================================

-spec check_circuit_breaker_before_selection(map() | undefined, binary() | undefined, map(), map()) ->
    {ok, allow} | {error, circuit_open, binary() | undefined}.
check_circuit_breaker_before_selection(undefined, _TenantId, _ProcessedContext, _OriginalContext) ->
    {ok, allow};
check_circuit_breaker_before_selection(CircuitBreaker, TenantId, ProcessedContext, OriginalContext) when is_map(CircuitBreaker) ->
    case maps:get(~"enabled", CircuitBreaker, false) of
        false ->
            {ok, allow};
        true ->
            ProviderCandidates = get_provider_candidates_from_context(ProcessedContext, OriginalContext),
            check_circuit_breaker_for_providers(CircuitBreaker, TenantId, ProviderCandidates, OriginalContext)
    end;
check_circuit_breaker_before_selection(_Invalid, _TenantId, _ProcessedContext, _OriginalContext) ->
    {ok, allow}.

-spec get_provider_candidates_from_context(map(), map()) -> [binary()].
get_provider_candidates_from_context(ProcessedContext, OriginalContext) ->
    case maps:get(~"selected_provider", ProcessedContext, undefined) of
        undefined ->
            case maps:get(~"selected_provider", OriginalContext, undefined) of
                undefined ->
                    [];
                Provider ->
                    [Provider]
            end;
        Provider ->
            [Provider]
    end.

-spec check_circuit_breaker_for_providers(map(), binary() | undefined, [binary()], map()) ->
    {ok, allow} | {error, circuit_open, binary() | undefined}.
check_circuit_breaker_for_providers(_CircuitBreaker, _TenantId, [], _OriginalContext) ->
    {ok, allow};
check_circuit_breaker_for_providers(_CircuitBreaker, TenantId, [ProviderId | _Rest], OriginalContext) ->
    case TenantId of
        undefined ->
            {ok, allow};
        _ ->
            case router_circuit_breaker:should_allow(TenantId, ProviderId) of
                {ok, allow} ->
                    {ok, allow};
                {error, circuit_open} ->
                    FallbackProvider = find_fallback_provider_for_circuit_breaker(OriginalContext),
                    {error, circuit_open, FallbackProvider}
            end
    end.

-spec find_fallback_provider_for_circuit_breaker(map()) -> binary() | undefined.
find_fallback_provider_for_circuit_breaker(Context) ->
    case maps:get(~"fallback_provider", Context, undefined) of
        undefined ->
            case maps:get(~"fallback_rules", Context, []) of
                [] ->
                    undefined;
                [FirstRule | _] when is_map(FirstRule) ->
                    maps:get(~"to", FirstRule, undefined);
                _ ->
                    undefined
            end;
        Provider ->
            Provider
    end.

%% ============================================================================
%% Internal: Helper Functions
%% ============================================================================

-spec prepare_context(map() | undefined, map() | undefined, map()) -> map().
prepare_context(ReqContext, Context, Message) ->
    SafeReqContext = normalize_context(ReqContext),
    SafeContext = normalize_context(Context),
    MergedContext = maps:merge(SafeReqContext, SafeContext),
    TenantId = maps:get(~"tenant_id", Message, undefined),
    MergedContextWithTenant = maps:put(~"tenant_id", TenantId, MergedContext),
    Environment = get_environment(SafeContext, SafeReqContext, Message),
    case Environment of
        undefined ->
            MergedContextWithTenant;
        Env ->
            maps:put(~"environment", Env, MergedContextWithTenant)
    end.

-spec normalize_context(map() | undefined) -> map().
normalize_context(undefined) ->
    #{};
normalize_context(C) when is_map(C) ->
    C;
normalize_context(_) ->
    #{}.

-spec get_environment(map(), map(), map()) -> binary() | undefined.
get_environment(SafeContext, SafeReqContext, Message) ->
    EnvFromApp = application:get_env(beamline_router, environment, undefined),
    case EnvFromApp of
        Val when is_binary(Val) ->
            Val;
        Val when is_list(Val) ->
            list_to_binary(Val);
        _ ->
            maps:get(~"environment", SafeContext,
                maps:get(~"environment", SafeReqContext,
                    maps:get(~"environment", Message,
                        case os:getenv("ENVIRONMENT") of
                            false ->
                                undefined;
                            EnvList ->
                                list_to_binary(EnvList)
                        end)))
    end.

-spec record_sticky_hit(binary() | undefined) -> ok.
record_sticky_hit(TenantId) ->
    TenantLabel = case TenantId of undefined -> ~"unknown"; _ -> TenantId end,
    router_metrics:emit_metric(router_sticky_hits_total, #{count => 1}, #{tenant_id => TenantLabel}),
    ok.

-spec record_sticky_miss(binary() | undefined) -> ok.
record_sticky_miss(TenantId) ->
    TenantLabel = case TenantId of undefined -> ~"unknown"; _ -> TenantId end,
    router_metrics:emit_metric(router_sticky_miss_total, #{count => 1}, #{tenant_id => TenantLabel}),
    ok.

-spec store_sticky_provider(binary() | undefined, map() | undefined, map(), binary()) -> ok.
store_sticky_provider(TenantId, Sticky, ProcessedContext, ProviderId) ->
    case {TenantId, Sticky} of
        {T, S} when T =/= undefined, is_map(S) ->
            Enabled = maps:get(~"enabled", S, maps:get(enabled, S, false)),
            case Enabled of
                true ->
                    SessionKeyName = maps:get(~"session_key", S, ~"session_id"),
                    case maps:get(SessionKeyName, ProcessedContext, undefined) of
                        undefined ->
                            ok;
                        SessionValue ->
                            router_sticky_store:set_provider(T, SessionValue, ProviderId)
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

-spec record_circuit_breaker_state(#policy{}, binary() | undefined, binary()) -> ok.
record_circuit_breaker_state(Policy, TenantId, ProviderId) ->
    case Policy#policy.circuit_breaker of
        CB when CB =/= undefined andalso is_map(CB) ->
            case maps:get(~"enabled", CB, false) of
                true when TenantId =/= undefined ->
                    %% Use record_state_with_config to pass policy configuration
                    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, CB);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

-spec annotate_span_with_decision({ok, #route_decision{}} | {error, term()}) -> ok.
annotate_span_with_decision({ok, RouteDecision}) ->
    router_tracing:set_span_attribute(~"provider.selection.provider_id", RouteDecision#route_decision.provider_id, string),
    router_tracing:set_span_attribute(~"provider.selection.reason", RouteDecision#route_decision.reason, string),
    router_tracing:set_span_status(ok, undefined),
    ok;
annotate_span_with_decision({error, ErrorInfo}) ->
    ErrorReason = extract_error_reason(ErrorInfo),
    router_tracing:set_span_attribute(~"provider.selection.error", ErrorReason, string),
    router_tracing:set_span_status(error, ErrorReason),
    ok.

-spec extract_error_reason(term()) -> binary().
extract_error_reason({ErrReason, _ErrContext}) when is_atom(ErrReason) ->
    erlang:atom_to_binary(ErrReason, utf8);
extract_error_reason(ErrReason) when is_atom(ErrReason) ->
    erlang:atom_to_binary(ErrReason, utf8);
extract_error_reason(_) ->
    ~"provider_selection_error".

-spec create_decision(binary(), binary(), integer(), map()) -> {ok, #route_decision{}}.
create_decision(ProviderId, Reason, Priority, Context) ->
    ExpectedLatency = maps:get(~"expected_latency_ms", Context, 500),
    ExpectedCost = maps:get(~"expected_cost", Context, 0.01),
    
    %% Extract executed extensions from context
    ExecutedExtensions = maps:get(~"executed_extensions", Context, []),
    %% Reverse to get chronological order (pre -> validators -> post)
    ExecutedExtensionsReversed = lists:reverse(ExecutedExtensions),
    
    %% Build metadata with executed extensions
    BaseMetadata = maps:get(~"metadata", Context, #{}),
    MetadataWithExtensions = maps:put(~"executed_extensions", ExecutedExtensionsReversed, BaseMetadata),
    
    Decision = #route_decision{
        provider_id = ProviderId,
        reason = Reason,
        priority = Priority,
        expected_latency_ms = ExpectedLatency,
        expected_cost = ExpectedCost,
        metadata = MetadataWithExtensions
    },
    {ok, Decision}.

-spec merge_retry_info(map(), map() | undefined) -> map().
merge_retry_info(OriginalContext, undefined) ->
    OriginalContext;
merge_retry_info(OriginalContext, RetryInfo) when is_map(RetryInfo) ->
    maps:merge(OriginalContext, RetryInfo);
merge_retry_info(OriginalContext, _) ->
    OriginalContext.

-spec normalize_error(term()) -> binary().
normalize_error({Reason, _Metadata}) when is_map(_Metadata) ->
    normalize_error(Reason);
normalize_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
normalize_error(Reason) when is_binary(Reason) ->
    Reason;
normalize_error(Reason) when is_list(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason]));
normalize_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

-spec get_fallback_rule_id(map()) -> binary().
get_fallback_rule_id(FallbackRule) ->
    Hash = erlang:phash2(FallbackRule),
    integer_to_binary(Hash).

-spec calculate_backoff(integer(), map() | undefined) -> integer().
calculate_backoff(Attempt, undefined) ->
    BaseMs = 100,
    Exponential = trunc(BaseMs * math:pow(2, Attempt - 1)),
    JitterMax = trunc(Exponential * 0.1),
    Jitter = case JitterMax > 0 of
        true ->
            rand:uniform(JitterMax) - 1;
        false ->
            0
    end,
    Exponential + Jitter;
calculate_backoff(Attempt, BackoffConfig) when is_map(BackoffConfig) ->
    Strategy = maps:get(~"strategy", BackoffConfig, ~"exponential"),
    BaseMs = maps:get(~"base_ms", BackoffConfig, 100),
    MaxMs = maps:get(~"max_ms", BackoffConfig, 5000),
    JitterEnabled = maps:get(~"jitter", BackoffConfig, true),
    
    Delay = case Strategy of
        ~"exponential" ->
            trunc(BaseMs * math:pow(2, Attempt - 1));
        ~"linear" ->
            BaseMs * Attempt;
        ~"fixed" ->
            BaseMs;
        _ ->
            trunc(BaseMs * math:pow(2, Attempt - 1))
    end,
    
    Jitter = case JitterEnabled of
        true ->
            JitterMax = trunc(Delay * 0.1),
            case JitterMax > 0 of
                true ->
                    rand:uniform(JitterMax) - 1;
                false ->
                    0
            end;
        false ->
            0
    end,
    
    FinalDelay = Delay + Jitter,
    min(FinalDelay, MaxMs);
calculate_backoff(Attempt, _) ->
    calculate_backoff(Attempt, undefined).

-spec evaluate_when_condition(map(), map()) -> boolean().
evaluate_when_condition(When, _Context) when is_map(When), map_size(When) =:= 0 ->
    true;
evaluate_when_condition(When, Context) when is_map(When) ->
    maps:fold(fun(Key, Values, Acc) ->
        Acc andalso evaluate_condition(Key, Values, Context)
    end, true, When);
evaluate_when_condition(_, _Context) ->
    false.

-spec evaluate_condition(binary(), term(), map()) -> boolean().
evaluate_condition(Op, [Left, Right], Context) when Op =:= ~"=="; Op =:= ~"!="; Op =:= ~">"; Op =:= ~"<"; Op =:= ~">="; Op =:= ~"<="; Op =:= ~"in" ->
    Val1 = resolve_operand(Left, Context),
    Val2 = resolve_operand(Right, Context),
    do_compare(Op, Val1, Val2);
evaluate_condition(Key, Values, Context) when is_binary(Key), is_list(Values) ->
    ContextValue = maps:get(Key, Context, undefined),
    case ContextValue of
        undefined ->
            false;
        _ ->
            lists:any(fun(Value) ->
                case Value of
                    ContextValue ->
                        true;
                    _ when is_binary(Value), is_binary(ContextValue) ->
                        Value =:= ContextValue;
                    _ ->
                        false
                end
            end, Values)
    end;
evaluate_condition(Key, Value, Context) when is_binary(Key) ->
    ContextValue = maps:get(Key, Context, undefined),
    case ContextValue of
        undefined ->
            false;
        _ ->
            ContextValue =:= Value
    end;
evaluate_condition(_, _, _) ->
    false.

-spec resolve_operand(term(), map()) -> term().
resolve_operand(#{<< "var" >> := Path}, Context) when is_binary(Path) ->
    resolve_var(Path, Context);
resolve_operand(Value, _Context) ->
    Value.

-spec do_compare(binary(), term(), term()) -> boolean().
do_compare(_, undefined, _) -> false;
do_compare(_, _, undefined) -> false;
do_compare(~"==", A, B) -> A =:= B;
do_compare(~"!=", A, B) -> A =/= B;
do_compare(~">", A, B) -> A > B;
do_compare(~"<", A, B) -> A < B;
do_compare(~">=", A, B) -> A >= B;
do_compare(~"<=", A, B) -> A =< B;
do_compare(~"in", A, B) when is_list(B) -> lists:member(A, B);
do_compare(~"in", _, _) -> false.

-spec resolve_var(binary(), map()) -> term().
resolve_var(Path, Context) ->
    Parts = binary:split(Path, <<".">>, [global]),
    case Parts of
        [<<"context">> | Rest] ->
            resolve_path_in_map(Rest, Context);
        _ ->
            resolve_path_in_map(Parts, Context)
    end.

resolve_path_in_map([], Value) -> Value;
resolve_path_in_map([Key | Rest], Map) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> undefined;
        NextValue -> resolve_path_in_map(Rest, NextValue)
    end;
resolve_path_in_map(_, _) -> undefined.

%% ============================================================================
%% Internal: Pipeline Validation & Complexity
%% ============================================================================

-spec validate_pipeline_depth(list(), list(), list()) -> ok | {error, term()}.
validate_pipeline_depth(Pre, Validators, Post) ->
    TotalDepth = length(Pre) + length(Validators) + length(Post),
    MaxDepth = application:get_env(beamline_router, extension_max_pipeline_depth, 10),
    case TotalDepth > MaxDepth of
        true ->
            {error, {pipeline_too_deep, #{
                depth => TotalDepth,
                max_depth => MaxDepth,
                pre_count => length(Pre),
                validators_count => length(Validators),
                post_count => length(Post)
            }}};
        false ->
            validate_per_type_limits(Pre, Validators, Post)
    end.

-spec validate_per_type_limits(list(), list(), list()) -> ok | {error, term()}.
validate_per_type_limits(Pre, Validators, Post) ->
    MaxPre = application:get_env(beamline_router, extension_max_pre_count, 5),
    MaxValidators = application:get_env(beamline_router, extension_max_validators_count, 5),
    MaxPost = application:get_env(beamline_router, extension_max_post_count, 5),
    
    case length(Pre) > MaxPre of
        true ->
            {error, {too_many_pre_processors, #{
                count => length(Pre),
                max_count => MaxPre
            }}};
        false ->
            case length(Validators) > MaxValidators of
                true ->
                    {error, {too_many_validators, #{
                        count => length(Validators),
                        max_count => MaxValidators
                    }}};
                false ->
                    case length(Post) > MaxPost of
                        true ->
                            {error, {too_many_post_processors, #{
                                count => length(Post),
                                max_count => MaxPost
                            }}};
                        false ->
                            ok
                    end
            end
    end.

-spec determine_complexity_level(integer()) -> binary().
determine_complexity_level(Score) when Score < 30 ->
    ~"low";
determine_complexity_level(Score) when Score < 60 ->
    ~"medium";
determine_complexity_level(Score) when Score < 80 ->
    ~"high";
determine_complexity_level(_) ->
    ~"very_high".

-spec calculate_complexity_score(integer(), integer(), integer(), integer()) -> integer().
calculate_complexity_score(PreCount, ValidatorsCount, PostCount, RecommendedMaxTotal) ->
    TotalCount = PreCount + ValidatorsCount + PostCount,
    
    TotalScore = case TotalCount of
        Count when Count =< RecommendedMaxTotal -> 
            trunc((Count / RecommendedMaxTotal) * 60);
        Count ->
            60 + min(20, (Count - RecommendedMaxTotal) * 5)
    end,
    
    PrePenalty = case PreCount > 2 of true -> 5; false -> 0 end,
    ValidatorsPenalty = case ValidatorsCount > 2 of true -> 5; false -> 0 end,
    PostPenalty = case PostCount > 2 of true -> 5; false -> 0 end,
    TotalPenalty = case TotalCount > RecommendedMaxTotal of
        true ->
            min(20, (TotalCount - RecommendedMaxTotal) * 4);
        false ->
            0
    end,
    
    min(100, TotalScore + PrePenalty + ValidatorsPenalty + PostPenalty + TotalPenalty).

-spec generate_complexity_warnings(integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer()) -> [binary()].
generate_complexity_warnings(PreCount, ValidatorsCount, PostCount,
                            RecommendedMaxPre, RecommendedMaxValidators, RecommendedMaxPost,
                            RecommendedMaxTotal, EstimatedLatencyMs) ->
    Warnings = [],
    TotalCount = PreCount + ValidatorsCount + PostCount,
    
    Warnings1 = case TotalCount > RecommendedMaxTotal of
        true ->
            [iolist_to_binary(io_lib:format(~"Pipeline has ~p extensions, recommended maximum is ~p. This may cause high latency.",
                [TotalCount, RecommendedMaxTotal])) | Warnings];
        false ->
            Warnings
    end,
    
    Warnings2 = case PreCount > RecommendedMaxPre of
        true ->
            [iolist_to_binary(io_lib:format(~"Pre-processors count (~p) exceeds recommended limit (~p). Consider reducing or parallelizing.",
                [PreCount, RecommendedMaxPre])) | Warnings1];
        false ->
            Warnings1
    end,
    
    Warnings3 = case ValidatorsCount > RecommendedMaxValidators of
        true ->
            [iolist_to_binary(io_lib:format(~"Validators count (~p) exceeds recommended limit (~p). This may slow down request processing.",
                [ValidatorsCount, RecommendedMaxValidators])) | Warnings2];
        false ->
            Warnings2
    end,
    
    Warnings4 = case PostCount > RecommendedMaxPost of
        true ->
            [iolist_to_binary(io_lib:format(~"Post-processors count (~p) exceeds recommended limit (~p). Consider reducing or parallelizing.",
                [PostCount, RecommendedMaxPost])) | Warnings3];
        false ->
            Warnings3
    end,
    
    Warnings5 = case EstimatedLatencyMs > 150 of
        true ->
            [iolist_to_binary(io_lib:format(~"Estimated pipeline latency (~pms) exceeds recommended P95 target (150ms). Consider optimizing.",
                [EstimatedLatencyMs])) | Warnings4];
        false ->
            Warnings4
    end,
    
    Warnings5.

-spec generate_complexity_recommendations(integer(), integer(), integer(), integer(), integer(), integer(), integer()) -> [binary()].
generate_complexity_recommendations(PreCount, ValidatorsCount, PostCount,
                                   RecommendedMaxPre, RecommendedMaxValidators, RecommendedMaxPost,
                                   RecommendedMaxTotal) ->
    Recommendations = [],
    TotalCount = PreCount + ValidatorsCount + PostCount,
    
    Recommendations1 = case TotalCount > RecommendedMaxTotal of
        true ->
            [iolist_to_binary(io_lib:format(~"Consider reducing total extensions to ~p or fewer for optimal performance",
                [RecommendedMaxTotal])) | Recommendations];
        false ->
            Recommendations
    end,
    
    Recommendations2 = case PreCount > RecommendedMaxPre of
        true ->
            [iolist_to_binary(io_lib:format(~"Reduce pre-processors to ~p or fewer, or consider parallel execution",
                [RecommendedMaxPre])) | Recommendations1];
        false ->
            Recommendations1
    end,
    
    Recommendations3 = case ValidatorsCount > RecommendedMaxValidators of
        true ->
            [iolist_to_binary(io_lib:format(~"Reduce validators to ~p or fewer for faster validation",
                [RecommendedMaxValidators])) | Recommendations2];
        false ->
            Recommendations2
    end,
    
    Recommendations4 = case PostCount > RecommendedMaxPost of
        true ->
            [iolist_to_binary(io_lib:format(~"Reduce post-processors to ~p or fewer, or consider parallel execution",
                [RecommendedMaxPost])) | Recommendations3];
        false ->
            Recommendations3
    end,
    
    Recommendations4.

-spec log_pipeline_complexity(map(), policy_id(), tenant_id() | undefined) -> ok.
log_pipeline_complexity(Complexity, PolicyId, TenantId) ->
    ComplexityLevel = maps:get(complexity_level, Complexity),
    Warnings = maps:get(warnings, Complexity),
    TotalCount = maps:get(total_extensions, Complexity),
    EstimatedLatency = maps:get(estimated_latency_ms, Complexity),
    
    case ComplexityLevel of
        ~"high" ->
            router_logger:warn(~"Pipeline complexity is high", #{
                ~"policy_id" => PolicyId,
                ~"tenant_id" => TenantId,
                ~"total_extensions" => TotalCount,
                ~"estimated_latency_ms" => EstimatedLatency,
                ~"complexity_score" => maps:get(complexity_score, Complexity),
                ~"warnings" => Warnings
            });
        ~"very_high" ->
            router_logger:error(~"Pipeline complexity is very high", #{
                ~"policy_id" => PolicyId,
                ~"tenant_id" => TenantId,
                ~"total_extensions" => TotalCount,
                ~"estimated_latency_ms" => EstimatedLatency,
                ~"complexity_score" => maps:get(complexity_score, Complexity),
                ~"warnings" => Warnings
            });
        _ ->
            ok
    end,
    
    telemetry:execute([router_pipeline, complexity], #{
        complexity_score => maps:get(complexity_score, Complexity),
        total_extensions => TotalCount,
        estimated_latency_ms => EstimatedLatency
    }, #{
        policy_id => PolicyId,
        tenant_id => TenantId,
        complexity_level => ComplexityLevel
    }),
    
    Recommendations = maps:get(recommendations, Complexity),
    case length(Recommendations) > 0 of
        true ->
            router_logger:info(~"Pipeline complexity recommendations", #{
                ~"policy_id" => PolicyId,
                ~"tenant_id" => TenantId,
                ~"recommendations" => Recommendations
            });
        false ->
            ok
    end.

%% ============================================================================
%% Provider Selection Validation and Status
%% ============================================================================

%% Returns {ok, Policy} or {error, Reason}
-spec validate_provider_selection(#policy{}, map()) -> {ok, #policy{}} | {error, term()}.
validate_provider_selection(Policy, _) when is_record(Policy, policy) ->
    #policy{weights = Weights, fallback = Fallback, fallbacks = Fallbacks, sticky = Sticky} = Policy,
    
    %% Validate weights
    case validate_weights(Weights) of
        {error, Reason} ->
            {error, {invalid_weights, Reason}};
        ok ->
            %% Validate sticky configuration
            case validate_sticky(Sticky) of
                {error, Reason} ->
                    {error, {invalid_sticky, Reason}};
                ok ->
                    %% Validate fallback configuration
                    case validate_fallback(Fallback, Fallbacks) of
                        {error, Reason} ->
                            {error, {invalid_fallback, Reason}};
                        ok ->
                            %% Check if at least one provider is available
                            case has_available_providers(Weights, Fallback, Fallbacks) of
                                false ->
                                    {error, no_providers_configured};
                                true ->
                                    {ok, Policy}
                            end
                    end
            end
    end;
validate_provider_selection(_, _) ->
    {error, invalid_policy}.

%% Returns status map with provider information
-spec get_provider_selection_status(#policy{}) -> map().
get_provider_selection_status(Policy) when is_record(Policy, policy) ->
    #policy{
        policy_id = PolicyId,
        tenant_id = TenantId,
        weights = Weights,
        fallback = Fallback,
        fallbacks = Fallbacks,
        sticky = Sticky
    } = Policy,
    
    AvailableProviders = get_providers_from_weights(Weights),
    FallbackProviders = get_providers_from_fallback(Fallback, Fallbacks),
    StickyEnabled = is_sticky_enabled(Sticky),
    
    #{
        policy_id => PolicyId,
        tenant_id => TenantId,
        available_providers => AvailableProviders,
        provider_count => length(AvailableProviders),
        fallback_providers => FallbackProviders,
        sticky_enabled => StickyEnabled,
        weights => Weights,
        total_weight => calculate_total_weight(Weights)
    }.

%% Returns list of provider IDs
-spec list_available_providers(#policy{}) -> [binary()].
list_available_providers(Policy) when is_record(Policy, policy) ->
    #policy{weights = Weights, fallback = Fallback, fallbacks = Fallbacks} = Policy,
    
    WeightProviders = get_providers_from_weights(Weights),
    FallbackProviders = get_providers_from_fallback(Fallback, Fallbacks),
    
    AllProviders = lists:usort(WeightProviders ++ FallbackProviders),
    AllProviders.

%% Helper: Validate weights configuration
-spec validate_weights(map()) -> ok | {error, term()}.
validate_weights(Weights) when is_map(Weights) ->
    case map_size(Weights) of
        0 ->
            {error, empty_weights};
        _ ->
            %% Check all weights are positive numbers
            InvalidWeights = maps:fold(fun
                (ProviderId, Weight, Acc) ->
                    case is_valid_weight(Weight) of
                        true -> Acc;
                        false -> [{ProviderId, Weight} | Acc]
                    end
            end, [], Weights),
            
            case InvalidWeights of
                [] -> ok;
                _ -> {error, {invalid_weights, InvalidWeights}}
            end
    end;
validate_weights(_) ->
    {error, invalid_weights_format}.

%% Helper: Validate sticky configuration
-spec validate_sticky(term()) -> ok | {error, term()}.
validate_sticky(undefined) -> ok;
validate_sticky(Sticky) when is_map(Sticky) ->
    Enabled = maps:get(~"enabled", Sticky, false),
    case Enabled of
        true ->
            SessionKey = maps:get(~"session_key", Sticky, ~"session_id"),
            case is_binary(SessionKey) of
                true -> ok;
                false -> {error, invalid_session_key}
            end;
        false ->
            ok
    end;
validate_sticky(_) ->
    {error, invalid_sticky_format}.

%% Helper: Validate fallback configuration
-spec validate_fallback(term(), list()) -> ok | {error, term()}.
validate_fallback(undefined, []) -> ok;
validate_fallback(Fallback, _) when is_binary(Fallback) ->
    case byte_size(Fallback) > 0 of
        true -> ok;
        false -> {error, empty_fallback}
    end;
validate_fallback(_, Fallbacks) when is_list(Fallbacks) ->
    %% Validate fallback rules
    InvalidFallbacks = lists:foldl(fun
        (FallbackRule, Acc) when is_map(FallbackRule) ->
            To = maps:get(~"to", FallbackRule, undefined),
            case is_binary(To) andalso byte_size(To) > 0 of
                true -> Acc;
                false -> [FallbackRule | Acc]
            end;
        (FallbackRule, Acc) ->
            [FallbackRule | Acc]
    end, [], Fallbacks),
    
    case InvalidFallbacks of
        [] -> ok;
        _ -> {error, {invalid_fallbacks, InvalidFallbacks}}
    end;
validate_fallback(_, _) ->
    {error, invalid_fallback_format}.

%% Helper: Check if at least one provider is available
-spec has_available_providers(map(), term(), list()) -> boolean().
has_available_providers(Weights, Fallback, Fallbacks) ->
    HasWeights = map_size(Weights) > 0,
    HasFallback = Fallback =/= undefined,
    HasFallbacks = length(Fallbacks) > 0,
    HasWeights orelse HasFallback orelse HasFallbacks.

%% Helper: Get providers from weights
-spec get_providers_from_weights(map()) -> [binary()].
get_providers_from_weights(Weights) when is_map(Weights) ->
    maps:keys(Weights);
get_providers_from_weights(_) ->
    [].

%% Helper: Get providers from fallback configuration
-spec get_providers_from_fallback(term(), list()) -> [binary()].
get_providers_from_fallback(Fallback, Fallbacks) ->
    FallbackList = case Fallback of
        undefined -> [];
        #{ ~"provider" := P } when is_binary(P) -> [P];
        P when is_binary(P) -> [P];
        _ -> []
    end,
    FallbackProviders = lists:foldl(fun
        (FallbackRule, Acc) when is_map(FallbackRule) ->
            To = maps:get(~"to", FallbackRule, undefined),
            case is_binary(To) of
                true -> [To | Acc];
                false -> Acc
            end;
        (_, Acc) ->
            Acc
    end, [], Fallbacks),
    lists:usort(FallbackList ++ FallbackProviders).

%% Helper: Check if sticky is enabled
-spec is_sticky_enabled(map() | undefined) -> boolean().
is_sticky_enabled(undefined) -> false;
is_sticky_enabled(Sticky) when is_map(Sticky) ->
    maps:get(~"enabled", Sticky, false);
is_sticky_enabled(_) -> false.

%% Helper: Calculate total weight
-spec calculate_total_weight(map()) -> float().
calculate_total_weight(Weights) when is_map(Weights) ->
    lists:sum([maps:get(P, Weights, 0.0) || P <- maps:keys(Weights)]);
calculate_total_weight(_) ->
    0.0.

%% Helper: Check if weight is valid
-spec is_valid_weight(number()) -> boolean().
is_valid_weight(Weight) when is_number(Weight) ->
    Weight >= 0.0;
is_valid_weight(_) ->
    false.
