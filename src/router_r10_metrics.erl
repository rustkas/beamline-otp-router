%% @doc R10 Circuit Breaker Metrics Access Layer
%%
%% This module provides centralized metric reading for all R10 (Circuit Breaker) tests.
%% It ensures no direct ETS access in tests and provides a single source of truth
%% for metric constants and helpers.
%%
%% Pattern: This module follows the "X_rN_metrics" pattern for risk theme metrics.
%% For future risk themes (R11, R12, etc.), create similar modules following this pattern.
%%
%% @see OBSERVABILITY_CONVENTIONS.md For observability conventions and metrics access layer pattern
%% @see src/router_rN_metrics_template.erl Template for creating new risk theme metrics modules
%% @see src/router_idem_metrics.erl Example of simpler metrics access layer
%% @see docs/R10_P0_COMPLETE_FINAL.md#r10-metrics-access-layer
-module(router_r10_metrics).

-export([
    %% Metric Reading
    get_metric_value/2,
    get_latest_trigger_reason/2,
    get_publish_attempts_total/0,
    get_publish_errors_total/0,
    get_publish_attempts_delta/1,
    get_publish_errors_delta/1,
    
    %% Assertions
    wait_for_trigger_reason/3,
    wait_for_trigger_reason/4,
    assert_trigger_reason_in/3,
    
    %% Debugging
    dump_metrics/0,
    clear_metrics/0,
    metrics_table_exists/0,
    prune_old_test_metrics/1,
    
    %% Trigger Reason Constants
    trigger_reason_failure_threshold/0,
    trigger_reason_error_rate/0,
    trigger_reason_latency/0,
    trigger_reason_half_open_failure/0,
    trigger_reason_timeout/0,
    
    %% Dashboard Queries
    get_trigger_reason_distribution/0,
    get_trigger_reason_distribution/1,
    get_circuit_state_summary/0,
    get_circuit_state_summary/1,
    
    %% Alert Evaluation
    check_circuit_breaker_alert_conditions/0,
    check_circuit_breaker_alert_conditions/1,
    get_open_circuits_count/0,
    get_circuit_transition_rate/1
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Trigger Reason Constants
%% ============================================================================

-spec trigger_reason_failure_threshold() -> binary().
trigger_reason_failure_threshold() ->
    <<"failure_threshold_exceeded">>.

-spec trigger_reason_error_rate() -> binary().
trigger_reason_error_rate() ->
    <<"error_rate_threshold_exceeded">>.

-spec trigger_reason_latency() -> binary().
trigger_reason_latency() ->
    <<"latency_threshold_exceeded">>.

-spec trigger_reason_half_open_failure() -> binary().
trigger_reason_half_open_failure() ->
    <<"half_open_failure">>.

-spec trigger_reason_timeout() -> binary().
trigger_reason_timeout() ->
    <<"timeout_elapsed">>.

%% ============================================================================
%% Metric Reading
%% ============================================================================

%% @doc Read any metric value by name and labels
%% @param MetricName Atom metric name (e.g., router_circuit_breaker_state)
%% @param Labels Map with label keys (e.g., #{tenant_id => <<"t1">>, provider_id => <<"p1">>})
%% @returns Metric value (integer, float) or 0 if not found
-spec get_metric_value(atom(), map()) -> integer() | float() | 0.
get_metric_value(MetricName, Labels) ->
    router_metrics:ensure(),
    
    %% Normalize labels for consistent key lookup
    LabelsKey = router_metrics:normalize_labels(Labels),
    Key = {MetricName, LabelsKey},
    
    case ets:lookup(router_metrics, Key) of
        [{Key, Value}] when is_integer(Value); is_float(Value) ->
            Value;
        [] ->
            %% Try backward-compatible format (no labels)
            case ets:lookup(router_metrics, MetricName) of
                [{MetricName, Value}] when is_integer(Value); is_float(Value) ->
                    Value;
                [] ->
                    0
            end
    end.

%% @doc Get latest trigger reason for tenant/provider pair
%% @param TenantId Tenant identifier
%% @param ProviderId Provider identifier
%% @returns {ok, Reason} | {error, not_found}
-spec get_latest_trigger_reason(binary(), binary()) -> {ok, binary()} | {error, not_found}.
get_latest_trigger_reason(TenantId, ProviderId) ->
    %% Find which reason was triggered by checking all possible reasons
    Reasons = [
        {trigger_reason_failure_threshold(), trigger_reason_failure_threshold()},
        {trigger_reason_error_rate(), trigger_reason_error_rate()},
        {trigger_reason_latency(), trigger_reason_latency()},
        {trigger_reason_half_open_failure(), trigger_reason_half_open_failure()},
        {trigger_reason_timeout(), trigger_reason_timeout()}
    ],
    find_triggered_reason(TenantId, ProviderId, Reasons).

find_triggered_reason(_TenantId, _ProviderId, []) ->
    {error, not_found};
find_triggered_reason(TenantId, ProviderId, [{_, ReasonValue} | Rest]) ->
    Value = get_metric_value(router_circuit_breaker_trigger_reason, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        reason => ReasonValue
    }),
    case Value > 0 of
        true ->
            {ok, ReasonValue};
        false ->
            find_triggered_reason(TenantId, ProviderId, Rest)
    end.

%% @doc Get total publish attempts
-spec get_publish_attempts_total() -> integer().
get_publish_attempts_total() ->
    get_metric_value(router_nats_publish_attempts_total, #{}).

%% @doc Get total publish errors
-spec get_publish_errors_total() -> integer().
get_publish_errors_total() ->
    get_metric_value(router_nats_publish_errors_total, #{}).

%% @doc Get publish attempts delta before/after action
%% @param ActionFun Function to execute between before/after measurements
%% @returns {Before, After, Delta}
-spec get_publish_attempts_delta(fun(() -> term())) -> {integer(), integer(), integer()}.
get_publish_attempts_delta(ActionFun) ->
    Before = get_publish_attempts_total(),
    _Result = ActionFun(),
    After = get_publish_attempts_total(),
    Delta = After - Before,
    {Before, After, Delta}.

%% @doc Get publish errors delta before/after action
%% @param ActionFun Function to execute between before/after measurements
%% @returns {Before, After, Delta}
-spec get_publish_errors_delta(fun(() -> term())) -> {integer(), integer(), integer()}.
get_publish_errors_delta(ActionFun) ->
    Before = get_publish_errors_total(),
    _Result = ActionFun(),
    After = get_publish_errors_total(),
    Delta = After - Before,
    {Before, After, Delta}.

%% ============================================================================
%% Assertions
%% ============================================================================

%% @doc Wait for trigger reason to appear with default timeout (3000ms)
-spec wait_for_trigger_reason(binary(), binary(), [binary()]) -> ok | {error, term()}.
wait_for_trigger_reason(TenantId, ProviderId, AllowedReasons) ->
    wait_for_trigger_reason(TenantId, ProviderId, AllowedReasons, 3000).

%% @doc Wait for trigger reason to appear with timeout
%% @param TenantId Tenant identifier
%% @param ProviderId Provider identifier
%% @param AllowedReasons List of allowed trigger reason binaries
%% @param TimeoutMs Timeout in milliseconds
%% @returns ok | {error, Reason}
-spec wait_for_trigger_reason(binary(), binary(), [binary()], integer()) -> ok | {error, term()}.
wait_for_trigger_reason(TenantId, ProviderId, AllowedReasons, TimeoutMs) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_trigger_reason_loop(TenantId, ProviderId, AllowedReasons, Start, TimeoutMs).

wait_for_trigger_reason_loop(TenantId, ProviderId, AllowedReasons, Start, TimeoutMs) ->
    case get_latest_trigger_reason(TenantId, ProviderId) of
        {ok, Reason} ->
            case lists:member(Reason, AllowedReasons) of
                true ->
                    ok;
                false ->
                    Now = erlang:monotonic_time(millisecond),
                    Elapsed = Now - Start,
                    case Elapsed >= TimeoutMs of
                        true ->
                            {error, {unexpected_trigger_reason, Reason, AllowedReasons}};
                        false ->
                            timer:sleep(50),
                            wait_for_trigger_reason_loop(TenantId, ProviderId, AllowedReasons, Start, TimeoutMs)
                    end
            end;
        {error, not_found} ->
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - Start,
            case Elapsed >= TimeoutMs of
                true ->
                    {error, {trigger_reason_not_found, TenantId, ProviderId}};
                false ->
                    timer:sleep(50),
                    wait_for_trigger_reason_loop(TenantId, ProviderId, AllowedReasons, Start, TimeoutMs)
            end
    end.

%% @doc Assert trigger reason is in allowed list (instant check, after state confirmed)
%% @param TenantId Tenant identifier
%% @param ProviderId Provider identifier
%% @param AllowedReasons List of allowed trigger reason binaries
%% @returns ok | {error, Reason}
-spec assert_trigger_reason_in(binary(), binary(), [binary()]) -> ok | {error, term()}.
assert_trigger_reason_in(TenantId, ProviderId, AllowedReasons) ->
    case get_latest_trigger_reason(TenantId, ProviderId) of
        {ok, Reason} ->
            case lists:member(Reason, AllowedReasons) of
                true ->
                    ok;
                false ->
                    {error, {unexpected_trigger_reason, Reason, AllowedReasons}}
            end;
        {error, not_found} ->
            {error, {trigger_reason_not_found, TenantId, ProviderId}}
    end.

%% ============================================================================
%% Debugging
%% ============================================================================

%% @doc Dump all metrics from ETS (for debugging)
%% Uses router_logger for structured logging (no io:format in production code)
-spec dump_metrics() -> list().
dump_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            router_logger:info(<<"Metrics table does not exist">>, #{
                <<"event">> => <<"dump_metrics">>
            }),
            [];
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            router_logger:debug(<<"Dumping all metrics">>, #{
                <<"event">> => <<"dump_metrics">>,
                <<"count">> => length(AllMetrics)
            }),
            AllMetrics
    end.

%% @doc Clear all R10 metrics from ETS table
%% Safe wrapper for clearing metrics without direct ETS access
-spec clear_metrics() -> ok.
clear_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            ok;
        _ ->
            %% Clear only R10-related metrics (circuit breaker, publish attempts/errors)
            R10MetricPatterns = [
                router_circuit_breaker_state,
                router_circuit_breaker_trigger_reason,
                router_circuit_breaker_state_transitions_total,
                router_circuit_breaker_error_rate,
                router_circuit_breaker_timeout_remaining_ms,
                router_nats_publish_attempts_total,
                router_nats_publish_errors_total
            ],
            lists:foreach(fun(Pattern) ->
                %% Match objects where key is {Pattern, Labels} (labeled metrics)
                %% Object structure: {{MetricName, Labels}, Value}
                %% Also match non-tuple keys (unlabeled): {MetricName, Value}
                MatchSpec = [
                    {{{Pattern, '_'}, '_'}, [], [true]}, 
                    {{Pattern, '_'}, [], [true]}
                ],
                ets:select_delete(router_metrics, MatchSpec)
            end, R10MetricPatterns),
            ok
    end.

%% @doc Check if metrics table exists
%% Safe wrapper for checking table existence
-spec metrics_table_exists() -> boolean().
metrics_table_exists() ->
    case catch ets:info(router_metrics) of
        undefined -> false;
        {'EXIT', _} -> false;
        _Info when is_list(_Info) -> true
    end.

%% @doc Prune old test metrics (for test cleanup)
%% Removes metrics older than specified seconds
%% @param AgeSeconds Age in seconds for pruning
%% @returns {ok, PrunedCount}
-spec prune_old_test_metrics(integer()) -> {ok, integer()}.
prune_old_test_metrics(_) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            {ok, 0};
        _ ->
            %% For now, just clear all R10 metrics (simpler than tracking timestamps)
            %% In future, could add timestamp tracking to metrics
            clear_metrics(),
            {ok, 0}
    end.

%% ============================================================================
%% Dashboard Queries
%% ============================================================================

%% @doc Get trigger reason distribution (for pie/bar charts)
%% Returns map of reason -> count
-spec get_trigger_reason_distribution() -> map().
get_trigger_reason_distribution() ->
    get_trigger_reason_distribution(#{}).

%% @doc Get trigger reason distribution with filters
%% @param Filters Map with tenant_id and/or provider_id filters
-spec get_trigger_reason_distribution(map()) -> map().
get_trigger_reason_distribution(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            TriggerReasonMetrics = lists:filter(fun
                ({{router_circuit_breaker_trigger_reason, _Labels}, Value}) when Value > 0 -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:foldl(fun
                ({{router_circuit_breaker_trigger_reason, Labels}, Value}, Acc) ->
                    TenantId = extract_label_from_list(tenant_id, Labels),
                    ProviderId = extract_label_from_list(provider_id, Labels),
                    Reason = extract_label_from_list(reason, Labels),
                    
                    case matches_filters(TenantId, ProviderId, Filters) andalso Reason =/= undefined of
                        true ->
                            Current = maps:get(Reason, Acc, 0),
                            maps:put(Reason, Current + Value, Acc);
                        false ->
                            Acc
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, TriggerReasonMetrics)
    end.

%% @doc Get circuit state summary
%% Returns map of state -> count
-spec get_circuit_state_summary() -> map().
get_circuit_state_summary() ->
    get_circuit_state_summary(#{}).

%% @doc Get circuit state summary with filters
%% @param Filters Map with tenant_id and/or provider_id filters
-spec get_circuit_state_summary(map()) -> map().
get_circuit_state_summary(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            StateMetrics = lists:filter(fun
                ({{router_circuit_breaker_state, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:foldl(fun
                ({{router_circuit_breaker_state, Labels}, Value}, Acc) ->
                    TenantId = extract_label_from_list(tenant_id, Labels),
                    ProviderId = extract_label_from_list(provider_id, Labels),
                    State = value_to_state(Value),
                    
                    case matches_filters(TenantId, ProviderId, Filters) andalso State =/= undefined of
                        true ->
                            Current = maps:get(State, Acc, 0),
                            maps:put(State, Current + 1, Acc);
                        false ->
                            Acc
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, StateMetrics)
    end.

%% Helper: Extract label from labels list
extract_label_from_list(Key, Labels) when is_list(Labels) ->
    case lists:keyfind(Key, 1, Labels) of
        {Key, Value} -> Value;
        _ -> undefined
    end;
extract_label_from_list(_Key, _Labels) ->
    undefined.

%% Helper: Check if labels match filters
matches_filters(TenantId, ProviderId, Filters) ->
    TenantIdFilter = maps:get(tenant_id, Filters, undefined),
    ProviderIdFilter = maps:get(provider_id, Filters, undefined),
    
    TenantIdMatch = case TenantIdFilter of
        undefined -> true;
        _ -> TenantId =:= TenantIdFilter
    end,
    
    ProviderIdMatch = case ProviderIdFilter of
        undefined -> true;
        _ -> ProviderId =:= ProviderIdFilter
    end,
    
    TenantIdMatch andalso ProviderIdMatch.

%% Helper: Convert metric value to state atom
value_to_state(+0.0) -> closed;
value_to_state(0.5) -> half_open;
value_to_state(1.0) -> open;
value_to_state(_) -> undefined.

%% ============================================================================
%% Alert Evaluation
%% ============================================================================

%% @doc Check circuit breaker alert conditions
%% Returns map with alert condition status
-spec check_circuit_breaker_alert_conditions() -> map().
check_circuit_breaker_alert_conditions() ->
    check_circuit_breaker_alert_conditions(#{}).

%% @doc Check circuit breaker alert conditions with filters
%% @param Filters Map with tenant_id and/or provider_id filters
-spec check_circuit_breaker_alert_conditions(map()) -> map().
check_circuit_breaker_alert_conditions(Filters) ->
    #{
        open_circuits_count => get_open_circuits_count(Filters),
        transition_rate => get_circuit_transition_rate(300),  %% 5 minutes
        has_open_circuits => get_open_circuits_count(Filters) > 0
    }.

%% @doc Get count of open circuits
-spec get_open_circuits_count() -> integer().
get_open_circuits_count() ->
    get_open_circuits_count(#{}).

%% @doc Get count of open circuits with filters
-spec get_open_circuits_count(map()) -> integer().
get_open_circuits_count(Filters) ->
    States = get_circuit_state_summary(Filters),
    maps:get(open, States, 0).

%% @doc Get circuit transition rate over time window
%% @param WindowSeconds Time window in seconds
-spec get_circuit_transition_rate(integer()) -> float().
get_circuit_transition_rate(WindowSeconds) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            0.0;
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            TransitionMetrics = lists:filter(fun
                ({{router_circuit_breaker_state_transitions_total, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            TotalTransitions = lists:foldl(fun
                ({{router_circuit_breaker_state_transitions_total, _Labels}, Value}, Acc) ->
                    Acc + Value;
                (_, Acc) ->
                    Acc
            end, 0, TransitionMetrics),
            
            case WindowSeconds > 0 of
                true -> TotalTransitions / WindowSeconds;
                false -> 0.0
            end
    end.

