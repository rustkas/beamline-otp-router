%% @doc Risk Theme Metrics Access Layer Template
%%
%% This is a template for creating metrics access layers for future risk themes (R11, R12, etc.).
%% Copy this file and rename it to `router_r<N>_metrics.erl` where <N> is the risk theme number.
%%
%% Pattern: This module follows the "X_rN_metrics" pattern for risk theme metrics.
%% Reference implementations:
%% - router_r10_metrics.erl (Circuit Breaker)
%% - router_idem_metrics.erl (Idempotency)
%%
%% @see docs/R10_P0_COMPLETE_FINAL.md#r10-metrics-access-layer
%% @see src/router_r10_metrics.erl (reference implementation)
%% @see src/router_idem_metrics.erl (simpler example)
%%
%% ⚠️ TEMPLATE FILE - DO NOT USE DIRECTLY
%% Copy this file and customize for your risk theme.
-module(router_rN_metrics_template).

-export([
    %% Metric Reading
    get_metric_value/2,
    %% Add specific metric getters here (e.g., get_rN_failures_total/0)
    
    %% Debugging
    dump_metrics/0,
    clear_metrics/0,
    metrics_table_exists/0
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Metric Reading
%% ============================================================================

%% @doc Read any metric value by name and labels
%% @param MetricName Atom metric name (e.g., router_rN_failures_total)
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

%% @doc Get specific metric (customize for your risk theme)
%% Example:
%% -spec get_rN_failures_total() -> integer().
%% get_rN_failures_total() ->
%%     get_metric_value(router_rN_failures_total, #{}).

%% ============================================================================
%% Debugging
%% ============================================================================

%% @doc Dump all risk theme metrics from ETS (for debugging)
%% Customize the filter to match your risk theme's metric names
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
            %% Filter only risk theme-related metrics
            %% CUSTOMIZE: Replace router_rN_* with your actual metric name prefixes
            AllMetrics = ets:tab2list(router_metrics),
            RiskMetrics = lists:filter(fun
                ({router_rN_failures_total, _}) -> true;
                ({router_rN_errors_total, _}) -> true;
                ({{router_rN_failures_total, _}, _}) -> true;
                ({{router_rN_errors_total, _}, _}) -> true;
                (_) -> false
            end, AllMetrics),
            router_logger:debug(<<"Dumping risk theme metrics">>, #{
                <<"event">> => <<"dump_metrics">>,
                <<"count">> => length(RiskMetrics)
            }),
            RiskMetrics
    end.

%% @doc Clear all risk theme metrics from ETS table
%% Safe wrapper for clearing metrics without direct ETS access
%% CUSTOMIZE: Replace router_rN_* with your actual metric names
-spec clear_metrics() -> ok.
clear_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            ok;
        _ ->
            %% Clear only risk theme-related metrics
            %% CUSTOMIZE: Add your actual metric names here
            RiskMetricPatterns = [
                router_rN_failures_total,
                router_rN_errors_total
            ],
            lists:foreach(fun(Pattern) ->
                %% Match all entries with this metric name (with or without labels)
                MatchSpec = [{{Pattern, '_'}, [], [true]}, {{Pattern}, [], [true]}],
                ets:select_delete(router_metrics, MatchSpec)
            end, RiskMetricPatterns),
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

