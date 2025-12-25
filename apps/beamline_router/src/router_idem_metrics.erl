-module(router_idem_metrics).

-doc "Idempotency Metrics Access Layer".
%%
%% This module provides centralized metric reading for all idempotency tests.
%% It ensures no direct ETS access in tests and provides a single source of truth
%% for idempotency metric constants and helpers.
%%
%% Pattern: This module follows the "X_rN_metrics" pattern for risk theme metrics.
%% Created following router_r10_metrics pattern for consistency.
%%
%% @see OBSERVABILITY_CONVENTIONS.md For observability conventions and metrics access layer pattern
%% @see src/router_r10_metrics.erl Reference implementation with advanced features
%% @see src/router_rN_metrics_template.erl Template for creating new risk theme metrics modules
%%

-export([
    %% Metric Reading
    get_metric_value/2,
    get_idem_hits_total/0,
    get_idem_miss_total/0,
    
    %% Debugging
    dump_metrics/0,
    clear_metrics/0,
    metrics_table_exists/0
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Metric Reading
%% ============================================================================

%% @param MetricName Atom metric name (e.g., router_idem_hits_total)
%% @param Labels Map with label keys (optional for idempotency metrics)
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

-spec get_idem_hits_total() -> integer().
get_idem_hits_total() ->
    get_metric_value(router_idem_hits_total, #{}).

-spec get_idem_miss_total() -> integer().
get_idem_miss_total() ->
    get_metric_value(router_idem_miss_total, #{}).

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
            %% Filter only idempotency-related metrics
            AllMetrics = ets:tab2list(router_metrics),
            IdemMetrics = lists:filter(fun
                ({router_idem_hits_total, _}) -> true;
                ({router_idem_miss_total, _}) -> true;
                ({{router_idem_hits_total, _}, _}) -> true;
                ({{router_idem_miss_total, _}, _}) -> true;
                (_) -> false
            end, AllMetrics),
            router_logger:debug(~"Dumping idempotency metrics", #{
                ~"event" => ~"dump_metrics",
                ~"count" => length(IdemMetrics)
            }),
            IdemMetrics
    end.

%% Safe wrapper for clearing metrics without direct ETS access
-spec clear_metrics() -> ok.
clear_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            ok;
        _ ->
            %% Clear only idempotency-related metrics
            IdemMetricPatterns = [
                router_idem_hits_total,
                router_idem_miss_total
            ],
            lists:foreach(fun(Pattern) ->
                %% Match all entries with this metric name (with or without labels)
                MatchSpec = [{{Pattern, '_'}, [], [true]}, {{Pattern}, [], [true]}],
                ets:select_delete(router_metrics, MatchSpec)
            end, IdemMetricPatterns),
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

