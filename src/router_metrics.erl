%% @doc Metrics Infrastructure
%%
%% Provides centralized metrics storage and emission for the router.
%% Uses ETS tables for in-memory metric storage with support for labels.
%%
%% Key features:
%% - Automatic ETS table creation and management
%% - Label normalization for consistent metric keys
%% - Support for counters, gauges, and histograms
%% - Backward compatibility with non-labeled metrics
%%
%% Performance:
%% - Uses read_concurrency and write_concurrency for high-throughput scenarios
%% - Label cardinality should be limited to prevent metric explosion
%% - Batch updates recommended for high-frequency metrics
%%
%% @see PERFORMANCE_GUIDE.md#metrics-collection-optimization For performance tuning
%% @see OBSERVABILITY_CONVENTIONS.md For metrics access layer patterns
%% @see src/router_r10_metrics.erl Example metrics access layer
-module(router_metrics).
-export([ensure/0, inc/1, emit_metric/3, normalize_labels/1, clear_all/0, get_table_size/0, get_table_memory/0, check_size_limit/0]).
-export([count_label_cardinality/1, get_all_label_combinations/1, check_cardinality_limit/2, enforce_cardinality_limit/2, get_cardinality_stats/1, monitor_cardinality/1, validate_label_names/1, get_metric_labels/1]).
-export([get_metrics_for_dashboard/1, aggregate_metrics_by_label/2, get_metric_time_series/2]).
-export([check_alert_condition/3, get_error_rate/1, get_performance_metrics/0]).

ensure() ->
  case ets:info(router_metrics) of
    undefined ->
      try
        _ = ets:new(router_metrics, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
        ok
      catch
        error:badarg ->
          %% Table was created by another process between check and create
          ok
      end;
    _ -> ok
  end.

inc(Name) ->
  ensure(),
  case ets:update_counter(router_metrics, Name, 1, {Name, 0}) of
    _ -> ok
  end.

%% @doc Emit metric via telemetry
%% MetricName: atom() - name of the metric
%% Measurements: map() - measurements (e.g., #{value => 100} or #{count => 1})
%% Metadata: map() - additional metadata (e.g., #{tenant_id => <<"t1">>})
%% 
%% For metrics with labels, stores in ETS as: {{MetricName, LabelsKey}, Value}
%% For metrics without labels, stores as: {MetricName, Value} (backward compatible)
-spec emit_metric(atom(), map(), map()) -> ok.
emit_metric(MetricName, Measurements, Metadata) ->
    %% Add trace_id to metadata if available and not already present
    MetadataWithTrace = case maps:get(trace_id, Metadata, undefined) of
        undefined ->
            %% Try to get trace_id from router_tracing if available
            case erlang:function_exported(router_tracing, get_trace_id, 0) of
                true ->
                    try
                        TraceId = router_tracing:get_trace_id(),
                        case TraceId of
                            undefined -> Metadata;
                            _ -> maps:put(trace_id, TraceId, Metadata)
                        end
                    catch
                        Error:Reason ->
                            router_logger:debug(<<"Failed to get trace ID for metrics">>, #{
                                <<"error">> => Error,
                                <<"reason">> => sanitize_error_for_logging(Reason),
                                <<"event">> => <<"trace_id_get_failed">>
                            }),
                            Metadata
                    end;
                false ->
                    Metadata
            end;
        _ ->
            Metadata
    end,
    
    Event = [router_metrics, MetricName],
    router_telemetry_helper:execute(Event, Measurements, MetadataWithTrace),
    ensure(),
    
    %% Check if metadata contains labels (non-empty map)
    HasLabels = map_size(MetadataWithTrace) > 0,
    
    case {maps:get(count, Measurements, undefined),
          maps:get(value, Measurements, undefined),
          maps:get(duration_seconds, Measurements, undefined),
          HasLabels} of
        {Count, _, _, false} when is_integer(Count) ->
            %% No labels: backward compatible format
            _ = ets:update_counter(router_metrics, MetricName, Count, {MetricName, 0}),
            ok;
        {Count, _, _, true} when is_integer(Count) ->
            %% Has labels: store with labels key
            LabelsKey = normalize_labels(MetadataWithTrace),
            Key = {MetricName, LabelsKey},
            _ = ets:update_counter(router_metrics, Key, Count, {Key, 0}),
            ok;
        {_, Val, _, false} when is_float(Val); is_integer(Val) ->
            %% No labels: backward compatible format
            _ = ets:insert(router_metrics, {MetricName, Val}),
            ok;
        {_, Val, _, true} when is_float(Val); is_integer(Val) ->
            %% Has labels: store with labels key
            LabelsKey = normalize_labels(MetadataWithTrace),
            Key = {MetricName, LabelsKey},
            _ = ets:insert(router_metrics, {Key, Val}),
            ok;
        {_, _, Dur, false} when is_float(Dur) ->
            %% No labels: backward compatible format
            _ = ets:insert(router_metrics, {MetricName, Dur}),
            ok;
        {_, _, Dur, true} when is_float(Dur) ->
            %% Has labels: store with labels key
            LabelsKey = normalize_labels(MetadataWithTrace),
            Key = {MetricName, LabelsKey},
            _ = ets:insert(router_metrics, {Key, Dur}),
            ok;
        _ ->
            ok
    end.

%% @doc Normalize labels map to a consistent key for ETS storage
%% Converts metadata map to a sorted list of {Key, Value} tuples
%% This ensures consistent key generation for same label sets
%% Optimized: Avoids unnecessary conversions and uses efficient comparison
-spec normalize_labels(map()) -> list({atom() | binary(), binary() | atom() | integer() | float()}).
normalize_labels(Metadata) when map_size(Metadata) =:= 0 ->
    [];
normalize_labels(Metadata) ->
    %% Convert map to list and sort efficiently
    %% Optimization: Use native map iteration and direct comparison
    LabelsList = maps:to_list(Metadata),
    %% Sort by key (optimized comparison - avoid binary conversion when possible)
    lists:sort(fun({K1, _}, {K2, _}) ->
        case {is_atom(K1), is_atom(K2)} of
            {true, true} -> K1 =< K2;
            {true, false} -> atom_to_binary(K1, utf8) =< K2;
            {false, true} -> K1 =< atom_to_binary(K2, utf8);
            {false, false} -> K1 =< K2
        end
    end, LabelsList).

%% @doc Clear all metrics from ETS table
%% Use with caution - clears entire metrics table
%% Primarily intended for testing and reset scenarios
-spec clear_all() -> ok.
clear_all() ->
    ensure(),
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    ok.

%% @doc Get current table size (number of entries)
-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(router_metrics, size) of
        undefined -> undefined;
        Size -> Size
    end.

%% @doc Get current table memory usage in bytes
-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(router_metrics, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

%% @doc Check if table size exceeds configured limit
-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, metrics_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(router_metrics, Limit);
        _ -> {error, invalid_limit}
    end.

%% @doc Count label cardinality for a metric
%% Returns the number of unique label combinations for a given metric
-spec count_label_cardinality(atom()) -> integer().
count_label_cardinality(MetricName) ->
    ensure(),
    Count = ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{M, _LabelsKey}, _Value} when M =:= MetricName ->
                Acc + 1;
            {M, _Value} when M =:= MetricName ->
                Acc + 1;
            _ ->
                Acc
        end
    end, 0, router_metrics),
    Count.

%% @doc Get all label combinations for a metric
%% Returns list of label maps for a given metric
-spec get_all_label_combinations(atom()) -> [map()].
get_all_label_combinations(MetricName) ->
    ensure(),
    LabelCombinations = ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{M, LabelsKey}, _Value} when M =:= MetricName ->
                %% Convert labels key back to map
                LabelsMap = lists:foldl(fun({K, V}, Map) ->
                    maps:put(K, V, Map)
                end, #{}, LabelsKey),
                [LabelsMap | Acc];
            {M, _Value} when M =:= MetricName ->
                %% No labels
                [#{} | Acc];
            _ ->
                Acc
        end
    end, [], router_metrics),
    LabelCombinations.

%% @doc Check if cardinality exceeds limit for a metric
-spec check_cardinality_limit(atom(), integer()) -> {ok, integer()} | {error, exceeded, integer(), integer()}.
check_cardinality_limit(MetricName, MaxCardinality) when is_atom(MetricName), is_integer(MaxCardinality), MaxCardinality > 0 ->
    CurrentCardinality = count_label_cardinality(MetricName),
    case CurrentCardinality > MaxCardinality of
        true ->
            {error, exceeded, CurrentCardinality, MaxCardinality};
        false ->
            {ok, CurrentCardinality}
    end.

%% @doc Enforce cardinality limit by removing oldest entries
-spec enforce_cardinality_limit(atom(), integer()) -> {ok, integer(), integer()} | {error, term()}.
enforce_cardinality_limit(MetricName, MaxCardinality) when is_atom(MetricName), is_integer(MaxCardinality), MaxCardinality > 0 ->
    CurrentCardinality = count_label_cardinality(MetricName),
    case CurrentCardinality > MaxCardinality of
        true ->
            %% Get all entries for this metric with timestamps (if available)
            %% For now, we'll remove entries in reverse order
            EntriesToRemove = CurrentCardinality - MaxCardinality,
            Removed = remove_oldest_metric_entries(MetricName, EntriesToRemove),
            {ok, Removed, CurrentCardinality - Removed};
        false ->
            {ok, 0, CurrentCardinality}
    end.

%% @doc Get cardinality statistics for a metric
-spec get_cardinality_stats(atom()) -> map().
get_cardinality_stats(MetricName) ->
    CurrentCardinality = count_label_cardinality(MetricName),
    LabelCombinations = get_all_label_combinations(MetricName),
    
    %% Count unique label keys
    AllLabelKeys = lists:foldl(fun(LabelsMap, Acc) ->
        maps:keys(LabelsMap) ++ Acc
    end, [], LabelCombinations),
    UniqueLabelKeys = lists:usort(AllLabelKeys),
    
    #{
        metric_name => MetricName,
        current_cardinality => CurrentCardinality,
        unique_label_keys => length(UniqueLabelKeys),
        label_keys => UniqueLabelKeys,
        label_combinations => LabelCombinations
    }.

%% @doc Monitor cardinality for all metrics
-spec monitor_cardinality([atom()]) -> map().
monitor_cardinality(MetricNames) ->
    Stats = lists:foldl(fun(MetricName, Acc) ->
        Stats = get_cardinality_stats(MetricName),
        maps:put(MetricName, Stats, Acc)
    end, #{}, MetricNames),
    Stats.

%% @doc Validate label names (check for readable names)
-spec validate_label_names(map()) -> {ok, map()} | {error, [binary()]}.
validate_label_names(Labels) when is_map(Labels) ->
    InvalidLabels = lists:foldl(fun({Key, _Value}, Acc) ->
        KeyStr = case is_atom(Key) of
            true -> atom_to_binary(Key, utf8);
            false -> Key
        end,
        %% Check if label name is readable (alphanumeric, underscore, hyphen)
        case re:run(KeyStr, "^[a-zA-Z0-9_-]+$", [{capture, none}]) of
            match ->
                Acc;
            nomatch ->
                [KeyStr | Acc]
        end
    end, [], maps:to_list(Labels)),
    
    case InvalidLabels of
        [] ->
            {ok, Labels};
        _ ->
            {error, InvalidLabels}
    end.

%% @doc Get all labels for a metric
-spec get_metric_labels(atom()) -> [map()].
get_metric_labels(MetricName) ->
    get_all_label_combinations(MetricName).

%% Internal: Remove oldest metric entries
-spec remove_oldest_metric_entries(atom(), integer()) -> integer().
remove_oldest_metric_entries(MetricName, Count) ->
    ensure(),
    %% Get all entries for this metric
    AllEntries = ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{M, _LabelsKey}, _Value} = E when M =:= MetricName ->
                [E | Acc];
            {M, _Value} = E when M =:= MetricName ->
                [E | Acc];
            _ ->
                Acc
        end
    end, [], router_metrics),
    
    %% Remove first Count entries (simple approach - in production, use timestamps)
    EntriesToRemove = lists:sublist(AllEntries, Count),
    lists:foreach(fun(Entry) ->
        Key = element(1, Entry),
        ets:delete(router_metrics, Key)
    end, EntriesToRemove),
    length(EntriesToRemove).

%% ============================================================================
%% Dashboard Aggregation Functions
%% ============================================================================

%% @doc Get metrics for dashboard visualization
%% @param MetricName Atom metric name
%% @returns Map of label combinations -> values
-spec get_metrics_for_dashboard(atom()) -> map().
get_metrics_for_dashboard(MetricName) ->
    ensure(),
    ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{M, LabelsKey}, Value} when M =:= MetricName ->
                LabelsMap = labels_key_to_map(LabelsKey),
                maps:put(LabelsMap, Value, Acc);
            {M, Value} when M =:= MetricName ->
                maps:put(#{}, Value, Acc);
            _ ->
                Acc
        end
    end, #{}, router_metrics).

%% @doc Aggregate metrics by a specific label
%% @param MetricName Atom metric name
%% @param LabelKey Label key to aggregate by
%% @returns Map of label value -> aggregated metric value
-spec aggregate_metrics_by_label(atom(), atom() | binary()) -> map().
aggregate_metrics_by_label(MetricName, LabelKey) ->
    ensure(),
    ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{M, LabelsKey}, Value} when M =:= MetricName ->
                LabelsMap = labels_key_to_map(LabelsKey),
                LabelValue = maps:get(LabelKey, LabelsMap, undefined),
                case LabelValue of
                    undefined ->
                        Acc;
                    _ ->
                        Current = maps:get(LabelValue, Acc, 0),
                        maps:put(LabelValue, Current + Value, Acc)
                end;
            {M, _} when M =:= MetricName ->
                %% No labels - aggregate under "default" or skip
                Acc;
            _ ->
                Acc
        end
    end, #{}, router_metrics).

%% @doc Get metric time series data (simplified - returns current values)
%% @param MetricName Atom metric name
%% @param TimeRange Time range in seconds (for future time-series support)
%% @returns List of {timestamp, value} tuples
-spec get_metric_time_series(atom(), integer()) -> list({integer(), number()}).
get_metric_time_series(MetricName, _TimeRange) ->
    ensure(),
    Timestamp = erlang:system_time(second),
    ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{M, _LabelsKey}, Value} when M =:= MetricName ->
                [{Timestamp, Value} | Acc];
            {M, Value} when M =:= MetricName ->
                [{Timestamp, Value} | Acc];
            _ ->
                Acc
        end
    end, [], router_metrics).

%% Helper: Convert labels key (list) back to map
-spec labels_key_to_map(list({atom() | binary(), term()})) -> map().
labels_key_to_map(LabelsKey) when is_list(LabelsKey) ->
    lists:foldl(fun({K, V}, Map) ->
        maps:put(K, V, Map)
    end, #{}, LabelsKey);
labels_key_to_map(_) ->
    #{}.

%% ============================================================================
%% Alert Condition Checking
%% ============================================================================

%% @doc Check alert condition for a metric
%% @param MetricName Atom metric name
%% @param Condition Map with condition type and parameters
%% @param Labels Map with label filters
%% @returns {ok, boolean()} | {error, term()}
-spec check_alert_condition(atom(), map(), map()) -> {ok, boolean()} | {error, term()}.
check_alert_condition(MetricName, Condition, Labels) ->
    ConditionType = maps:get(type, Condition, undefined),
    
    case ConditionType of
        threshold ->
            Threshold = maps:get(threshold, Condition),
            Operator = maps:get(operator, Condition, greater_than),
            Value = get_metric_value_for_alert(MetricName, Labels),
            Result = case Operator of
                greater_than -> Value > Threshold;
                less_than -> Value < Threshold;
                equal -> Value =:= Threshold;
                _ -> false
            end,
            {ok, Result};
        rate ->
            Threshold = maps:get(threshold, Condition),
            DurationSeconds = maps:get(duration_seconds, Condition, 60),
            Rate = calculate_error_rate(MetricName, Labels, DurationSeconds),
            {ok, Rate > Threshold};
        _ ->
            {error, unsupported_condition_type}
    end.

%% @doc Get error rate for a metric
%% @param DurationSeconds Time window in seconds
%% @returns Error rate as float (0.0 to 1.0)
-spec get_error_rate(integer()) -> float().
get_error_rate(_) ->
    ErrorCount = get_metric_value_for_alert(router_request_errors_total, #{}),
    TotalCount = get_metric_value_for_alert(router_requests_total, #{}),
    
    case TotalCount > 0 of
        true -> ErrorCount / TotalCount;
        false -> 0.0
    end.

%% @doc Get performance metrics for alerting
-spec get_performance_metrics() -> map().
get_performance_metrics() ->
    #{
        error_rate => get_error_rate(60),
        total_requests => get_metric_value_for_alert(router_requests_total, #{}),
        total_errors => get_metric_value_for_alert(router_request_errors_total, #{}),
        memory_usage => get_memory_usage_for_alert()
    }.

%% Helper: Get metric value for alert evaluation
get_metric_value_for_alert(MetricName, Labels) ->
    ensure(),
    case ets:info(router_metrics) of
        undefined ->
            0;
        _ ->
            Key = {MetricName, normalize_labels(Labels)},
            case ets:lookup(router_metrics, Key) of
                [{Key, Value}] when is_number(Value) ->
                    Value;
                [] ->
                    case ets:lookup(router_metrics, MetricName) of
                        [{MetricName, Value}] when is_number(Value) ->
                            Value;
                        [] ->
                            0
                    end
            end
    end.

%% Helper: Calculate error rate
calculate_error_rate(MetricName, Labels, DurationSeconds) ->
    ErrorCount = get_metric_value_for_alert(MetricName, Labels),
    TotalCount = get_metric_value_for_alert(router_requests_total, #{}),
    
    case TotalCount > 0 andalso DurationSeconds > 0 of
        true -> (ErrorCount / TotalCount) / DurationSeconds;
        false -> 0.0
    end.

%% Helper: Get memory usage for alerting
get_memory_usage_for_alert() ->
    MemoryInfo = erlang:memory(),
    proplists:get_value(total, MemoryInfo, 0).

%% Internal: Sanitize error for logging (masks secrets)
-spec sanitize_error_for_logging(term()) -> binary() | term().
sanitize_error_for_logging(Error) ->
    %% Convert error to binary for pattern matching
    ErrorBin = case is_binary(Error) of
        true -> Error;
        false -> iolist_to_binary(io_lib:format("~p", [Error]))
    end,
    %% Check for common secret patterns in error message
    case re:run(ErrorBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            <<"[REDACTED: contains sensitive data]">>;
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.
