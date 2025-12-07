%% @doc Generic Metrics Test Helper
%%
%% Provides generic metrics access functions for tests that don't have a specific
%% metrics access layer (like router_r10_metrics or router_idem_metrics).
%%
%% For risk theme-specific metrics (R10, R11, R12, etc.), use the appropriate
%% access layer module (router_rN_metrics.erl) instead of this helper.
%%
%% This helper is intended for:
%% - General metrics tests (router_metrics_labels_*_SUITE.erl)
%% - Integration tests that check multiple metric types
%% - Tests that need generic metric access before a specific access layer exists
%%
%% Pattern: Risk theme metrics should use router_rN_metrics modules.
%% See: src/router_rN_metrics_template.erl for creating new access layers.
%%
%% @see src/router_r10_metrics.erl (R10 Circuit Breaker metrics)
%% @see src/router_idem_metrics.erl (Idempotency metrics)
%% @see src/router_rN_metrics_template.erl (Template for future risk themes)
-module(router_metrics_test_helper).

-export([
    %% Generic metric reading
    get_metric_value/2,
    get_metric_value/1,
    
    %% Table management
    clear_all_metrics/0,
    metrics_table_exists/0,
    dump_all_metrics/0
]).

-include_lib("common_test/include/ct.hrl").
-include("beamline_router.hrl").

%% ============================================================================
%% Generic Metric Reading
%% ============================================================================

%% @doc Read any metric value by name and labels
%% @param MetricName Atom metric name (e.g., router_nats_publish_total)
%% @param Labels Map with label keys (e.g., #{tenant_id => <<"t1">>})
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

%% @doc Read metric value without labels (backward compatible)
%% @param MetricName Atom metric name
%% @returns Metric value (integer, float) or 0 if not found
-spec get_metric_value(atom()) -> integer() | float() | 0.
get_metric_value(MetricName) ->
    get_metric_value(MetricName, #{}).

%% ============================================================================
%% Table Management
%% ============================================================================

%% @doc Clear all metrics from ETS table
%% Use with caution: This clears ALL metrics, not just specific risk theme metrics.
%% For risk theme-specific clearing, use the appropriate router_rN_metrics:clear_metrics/0.
-spec clear_all_metrics() -> ok.
clear_all_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            ok;
        _ ->
            router_metrics:clear_all(),
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

%% @doc Dump all metrics from ETS (for debugging)
%% Returns all metrics in the table. For risk theme-specific dumps, use router_rN_metrics:dump_metrics/0.
-spec dump_all_metrics() -> list().
dump_all_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            ct:pal("Metrics table does not exist", []),
            [];
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            ct:pal("Dumping all metrics (~p entries)", [length(AllMetrics)]),
            AllMetrics
    end.

