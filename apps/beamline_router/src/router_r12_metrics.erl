-module(router_r12_metrics).

-doc "R12 Risk Theme Metrics Access Layer".
%%
%% This module provides centralized metric reading for all R12 risk theme tests.
%% It ensures no direct ETS access in tests and provides a single source of truth
%% for R12 metric constants and helpers.
%%
%% Pattern: This module follows the "X_rN_metrics" pattern for risk theme metrics.
%% Created from router_rN_metrics_template.erl following router_r10_metrics pattern.
%%
%% @see OBSERVABILITY_CONVENTIONS.md For observability conventions and metrics access layer pattern
%% @see src/router_r10_metrics.erl Reference implementation with advanced features
%% @see src/router_rN_metrics_template.erl Template for creating new risk theme metrics modules
%% @see src/router_idem_metrics.erl Example of simpler metrics access layer

-export([
    %% Metric Reading
    get_metric_value/2,
    get_r12_failures_total/0,
    get_r12_errors_total/0,
    get_r12_success_total/0,
    
    %% Debugging
    dump_metrics/0,
    clear_metrics/0,
    metrics_table_exists/0
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Metric Reading
%% ============================================================================

%% @param MetricName Atom metric name (e.g., router_r12_failures_total)
%% @param Labels Map with label keys (e.g., #{tenant_id => ~"t1", provider_id => ~"p1"})
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

-spec get_r12_failures_total() -> integer().
get_r12_failures_total() ->
    get_metric_value(router_r12_failures_total, #{}).

-spec get_r12_errors_total() -> integer().
get_r12_errors_total() ->
    get_metric_value(router_r12_errors_total, #{}).

-spec get_r12_success_total() -> integer().
get_r12_success_total() ->
    get_metric_value(router_r12_success_total, #{}).

%% ============================================================================
%% Debugging
%% ============================================================================

-spec dump_metrics() -> list().
dump_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            router_logger:info(~"Metrics table does not exist", #{
                ~"event" => ~"dump_metrics"
            }),
            [];
        _ ->
            %% Filter only R12 risk theme-related metrics
            AllMetrics = ets:tab2list(router_metrics),
            R12Metrics = lists:filter(fun
                ({router_r12_failures_total, _}) -> true;
                ({router_r12_errors_total, _}) -> true;
                ({router_r12_success_total, _}) -> true;
                ({{router_r12_failures_total, _}, _}) -> true;
                ({{router_r12_errors_total, _}, _}) -> true;
                ({{router_r12_success_total, _}, _}) -> true;
                (_) -> false
            end, AllMetrics),
            router_logger:debug(~"Dumping R12 risk theme metrics", #{
                ~"event" => ~"dump_metrics",
                ~"count" => length(R12Metrics)
            }),
            R12Metrics
    end.

%% Safe wrapper for clearing metrics without direct ETS access
-spec clear_metrics() -> ok.
clear_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            ok;
        _ ->
            %% Clear only R12 risk theme-related metrics
            R12MetricPatterns = [
                router_r12_failures_total,
                router_r12_errors_total,
                router_r12_success_total
            ],
            lists:foreach(fun(Pattern) ->
                %% Match all entries with this metric name (with or without labels)
                MatchSpec = [{{Pattern, '_'}, [], [true]}, {{Pattern}, [], [true]}],
                ets:select_delete(router_metrics, MatchSpec)
            end, R12MetricPatterns),
            ok
    end.

%% Safe wrapper for checking table existence
-spec metrics_table_exists() -> boolean().
metrics_table_exists() ->
    case catch ets:info(router_metrics) of
        undefined -> false;
        {'EXIT', _} -> false;
        _Info when is_list(_Info) -> true
    end.

