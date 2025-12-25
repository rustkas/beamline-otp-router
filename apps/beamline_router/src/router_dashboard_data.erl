-module(router_dashboard_data).

-doc "Dashboard Data Aggregation".
%%
%% Provides functions to aggregate metrics data for dashboard visualization.
%% These functions query metrics and format data for Grafana dashboards.
%%
%% @see router_dashboard_config.erl For dashboard configuration
%% @see router_r10_metrics.erl For R10 metrics access

-export([
    get_r10_dashboard_data/0,
    get_r10_dashboard_data/1,
    get_performance_dashboard_data/0,
    get_performance_dashboard_data/1,
    get_health_dashboard_data/0,
    get_trigger_reason_dashboard_data/0,
    get_trigger_reason_dashboard_data/1,
    aggregate_trigger_reasons/0,
    aggregate_trigger_reasons/1
]).

-include("beamline_router.hrl").

%% ============================================================================
%% R10 Dashboard Data
%% ============================================================================

-spec get_r10_dashboard_data() -> map().
get_r10_dashboard_data() ->
    get_r10_dashboard_data(#{}).

%% @param Filters Map with tenant_id and/or provider_id filters
-spec get_r10_dashboard_data(map()) -> map().
get_r10_dashboard_data(Filters) ->
    #{
        circuit_states => get_circuit_states(Filters),
        state_transitions => get_state_transitions(Filters),
        trigger_reasons => get_trigger_reasons_summary(Filters),
        error_rates => get_error_rates(Filters),
        timeout_remaining => get_timeout_remaining(Filters),
        open_circuits_count => get_open_circuits_count(Filters)
    }.

%% ============================================================================
%% Performance Dashboard Data
%% ============================================================================

-spec get_performance_dashboard_data() -> map().
get_performance_dashboard_data() ->
    get_performance_dashboard_data(#{}).

%% @param Filters Map with tenant_id filter
-spec get_performance_dashboard_data(map()) -> map().
get_performance_dashboard_data(Filters) ->
    #{
        throughput => get_request_throughput(Filters),
        latency_p95 => get_latency_p95(Filters),
        latency_p99 => get_latency_p99(Filters),
        error_rate => get_request_error_rate(Filters),
        active_requests => get_active_requests(Filters)
    }.

%% ============================================================================
%% Health Dashboard Data
%% ============================================================================

-spec get_health_dashboard_data() -> map().
get_health_dashboard_data() ->
    #{
        system_health => get_system_health_status(),
        memory_usage => get_memory_usage(),
        process_count => get_process_count(),
        ets_table_sizes => get_ets_table_sizes(),
        component_health => get_component_health()
    }.

%% ============================================================================
%% Trigger Reason Dashboard Data
%% ============================================================================

-spec get_trigger_reason_dashboard_data() -> map().
get_trigger_reason_dashboard_data() ->
    get_trigger_reason_dashboard_data(#{}).

%% @param Filters Map with tenant_id, provider_id, and/or reason filters
-spec get_trigger_reason_dashboard_data(map()) -> map().
get_trigger_reason_dashboard_data(Filters) ->
    #{
        distribution => aggregate_trigger_reasons(Filters),
        over_time => get_trigger_reasons_over_time(Filters),
        by_tenant_provider => get_trigger_reasons_by_tenant_provider(Filters)
    }.

-spec aggregate_trigger_reasons() -> map().
aggregate_trigger_reasons() ->
    aggregate_trigger_reasons(#{}).

-spec aggregate_trigger_reasons(map()) -> map().
aggregate_trigger_reasons(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            TriggerReasonMetrics = lists:filter(fun
                ({{router_circuit_breaker_trigger_reason, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            Aggregated = lists:foldl(fun
                ({{router_circuit_breaker_trigger_reason, Labels}, Value}, Acc) when is_list(Labels) ->
                    Reason = extract_reason_from_labels(Labels, Filters),
                    case Reason of
                        undefined -> Acc;
                        _ ->
                            Current = maps:get(Reason, Acc, 0),
                            maps:put(Reason, Current + Value, Acc)
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, TriggerReasonMetrics),
            
            Aggregated
    end.

%% ============================================================================
%% Internal Helper Functions
%% ============================================================================

get_circuit_states(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            StateMetrics = lists:filter(fun
                ({{router_circuit_breaker_state, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:foldl(fun
                ({{router_circuit_breaker_state, Labels}, Value}, Acc) ->
                    case matches_filters(Labels, Filters) of
                        true ->
                            Key = build_key_from_labels(Labels),
                            maps:put(Key, Value, Acc);
                        false ->
                            Acc
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, StateMetrics)
    end.

get_state_transitions(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            TransitionMetrics = lists:filter(fun
                ({{router_circuit_breaker_state_transitions_total, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:foldl(fun
                ({{router_circuit_breaker_state_transitions_total, Labels}, Value}, Acc) ->
                    case matches_filters(Labels, Filters) of
                        true ->
                            Key = build_transition_key_from_labels(Labels),
                            maps:put(Key, Value, Acc);
                        false ->
                            Acc
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, TransitionMetrics)
    end.

get_trigger_reasons_summary(Filters) ->
    aggregate_trigger_reasons(Filters).

get_error_rates(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            ErrorRateMetrics = lists:filter(fun
                ({{router_circuit_breaker_error_rate, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:foldl(fun
                ({{router_circuit_breaker_error_rate, Labels}, Value}, Acc) ->
                    case matches_filters(Labels, Filters) of
                        true ->
                            Key = build_key_from_labels(Labels),
                            maps:put(Key, Value, Acc);
                        false ->
                            Acc
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, ErrorRateMetrics)
    end.

get_timeout_remaining(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            TimeoutMetrics = lists:filter(fun
                ({{router_circuit_breaker_timeout_remaining_ms, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:foldl(fun
                ({{router_circuit_breaker_timeout_remaining_ms, Labels}, Value}, Acc) ->
                    case matches_filters(Labels, Filters) of
                        true ->
                            Key = build_key_from_labels(Labels),
                            maps:put(Key, Value, Acc);
                        false ->
                            Acc
                    end;
                (_, Acc) ->
                    Acc
            end, #{}, TimeoutMetrics)
    end.

get_open_circuits_count(Filters) ->
    States = get_circuit_states(Filters),
    OpenCount = maps:fold(fun
        (_Key, Value, Acc) when Value =:= 1.0 -> Acc + 1;  %% open state
        (_Key, _Value, Acc) -> Acc
    end, 0, States),
    OpenCount.

get_request_throughput(_Filters) ->
    %% Placeholder - would query router_requests_total metric
    #{}.

get_latency_p95(_Filters) ->
    %% Placeholder - would query router_request_duration_seconds P95
    #{}.

get_latency_p99(_Filters) ->
    %% Placeholder - would query router_request_duration_seconds P99
    #{}.

get_request_error_rate(_Filters) ->
    %% Placeholder - would calculate error rate from metrics
    #{}.

get_active_requests(_Filters) ->
    %% Placeholder - would query router_requests_active metric
    0.

get_system_health_status() ->
    %% Placeholder - would aggregate component health
    0.

get_memory_usage() ->
    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo, 0),
    TotalMemory.

get_process_count() ->
    erlang:system_info(process_count).

get_ets_table_sizes() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            Info = ets:info(router_metrics),
            Size = proplists:get_value(size, Info, 0),
            Memory = proplists:get_value(memory, Info, 0),
            #{
                router_metrics => #{
                    size => Size,
                    memory => Memory
                }
            }
    end.

get_component_health() ->
    %% Placeholder - would query component health metrics
    #{}.

get_trigger_reasons_over_time(_Filters) ->
    %% Placeholder - would query time-series data
    #{}.

get_trigger_reasons_by_tenant_provider(Filters) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> [];
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            TriggerReasonMetrics = lists:filter(fun
                ({{router_circuit_breaker_trigger_reason, _Labels}, _Value}) -> true;
                (_) -> false
            end, AllMetrics),
            
            lists:map(fun
                ({{router_circuit_breaker_trigger_reason, Labels}, Value}) ->
                    case matches_filters(Labels, Filters) of
                        true ->
                            #{
                                tenant_id => extract_label(tenant_id, Labels),
                                provider_id => extract_label(provider_id, Labels),
                                reason => extract_label(reason, Labels),
                                count => Value
                            };
                        false ->
                            undefined
                    end
            end, TriggerReasonMetrics)
    end.

%% Helper: Extract reason from labels
extract_reason_from_labels(Labels, Filters) ->
    Reason = extract_label(reason, Labels),
    case maps:get(reason, Filters, undefined) of
        undefined -> Reason;
        FilterReason when FilterReason =:= Reason -> Reason;
        _ -> undefined
    end.

%% Helper: Extract label value from labels list
extract_label(Key, Labels) when is_list(Labels) ->
    case lists:keyfind(Key, 1, Labels) of
        {Key, Value} -> Value;
        _ -> undefined
    end;
extract_label(_Key, _Labels) ->
    undefined.

%% Helper: Check if labels match filters
matches_filters(Labels, Filters) ->
    TenantIdFilter = maps:get(tenant_id, Filters, undefined),
    ProviderIdFilter = maps:get(provider_id, Filters, undefined),
    
    TenantIdMatch = case TenantIdFilter of
        undefined -> true;
        _ -> extract_label(tenant_id, Labels) =:= TenantIdFilter
    end,
    
    ProviderIdMatch = case ProviderIdFilter of
        undefined -> true;
        _ -> extract_label(provider_id, Labels) =:= ProviderIdFilter
    end,
    
    TenantIdMatch andalso ProviderIdMatch.

%% Helper: Build key from labels
build_key_from_labels(Labels) ->
    TenantId = extract_label(tenant_id, Labels),
    ProviderId = extract_label(provider_id, Labels),
    {TenantId, ProviderId}.

%% Helper: Build transition key from labels
build_transition_key_from_labels(Labels) ->
    From = extract_label(from, Labels),
    To = extract_label(to, Labels),
    {From, To}.

