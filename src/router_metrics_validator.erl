%% @doc Metrics Validator Module
%% Provides validation and documentation for metrics
%%
%% Features:
%% - Validate metric names match implementation
%% - Document all metric names and labels
%% - Verify metrics have readable labels
%% - Check for metric name mismatches
%%
%% @see OBSERVABILITY_CONVENTIONS.md For observability conventions
-module(router_metrics_validator).

-export([
    validate_metric_name/1,
    validate_metric_labels/2,
    get_all_metrics/0,
    get_metric_documentation/1,
    document_all_metrics/0,
    find_metric_mismatches/1
]).

-include("beamline_router.hrl").

%% @doc Validate metric name format
-spec validate_metric_name(atom()) -> {ok, atom()} | {error, binary()}.
validate_metric_name(MetricName) when is_atom(MetricName) ->
    MetricNameStr = atom_to_binary(MetricName, utf8),
    %% Check if metric name follows Prometheus naming conventions
    case re:run(MetricNameStr, "^[a-zA-Z_][a-zA-Z0-9_]*$", [{capture, none}]) of
        match ->
            %% Check if ends with _total, _count, _sum, _bucket, etc.
            case re:run(MetricNameStr, "_(total|count|sum|bucket|seconds|bytes|percent)$", [{capture, none}]) of
                match ->
                    {ok, MetricName};
                nomatch ->
                    %% Check if it's a gauge or info metric (no suffix required)
                    {ok, MetricName}
            end;
        nomatch ->
            {error, <<"Metric name does not follow Prometheus naming conventions">>}
    end.

%% @doc Validate metric labels
-spec validate_metric_labels(atom(), map()) -> {ok, map()} | {error, [binary()]}.
validate_metric_labels(MetricName, Labels) when is_atom(MetricName), is_map(Labels) ->
    %% Validate label names
    case router_metrics:validate_label_names(Labels) of
        {ok, _} ->
            %% Check for reserved label names
            ReservedLabels = [<<"le">>, <<"quantile">>, <<"__name__">>],
            InvalidLabels = lists:filter(fun({Key, _}) ->
                KeyBin = case is_atom(Key) of
                    true -> atom_to_binary(Key, utf8);
                    false -> Key
                end,
                lists:member(KeyBin, ReservedLabels)
            end, maps:to_list(Labels)),
            
            case InvalidLabels of
                [] ->
                    {ok, Labels};
                _ ->
                    InvalidNames = [case is_atom(K) of true -> atom_to_binary(K, utf8); false -> K end || {K, _} <- InvalidLabels],
                    {error, InvalidNames}
            end;
        {error, InvalidNames} ->
            {error, InvalidNames}
    end.

%% @doc Get all metrics from ETS table
-spec get_all_metrics() -> [atom()].
get_all_metrics() ->
    router_metrics:ensure(),
    Metrics = ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {{MetricName, _LabelsKey}, _Value} ->
                case lists:member(MetricName, Acc) of
                    true -> Acc;
                    false -> [MetricName | Acc]
                end;
            {MetricName, _Value} ->
                case lists:member(MetricName, Acc) of
                    true -> Acc;
                    false -> [MetricName | Acc]
                end;
            _ ->
                Acc
        end
    end, [], router_metrics),
    lists:usort(Metrics).

%% @doc Get metric documentation
-spec get_metric_documentation(atom()) -> map().
get_metric_documentation(MetricName) ->
    LabelCombinations = router_metrics:get_metric_labels(MetricName),
    Cardinality = router_metrics:count_label_cardinality(MetricName),
    
    %% Extract unique label keys
    AllLabelKeys = lists:foldl(fun(LabelsMap, Acc) ->
        maps:keys(LabelsMap) ++ Acc
    end, [], LabelCombinations),
    UniqueLabelKeys = lists:usort(AllLabelKeys),
    
    #{
        metric_name => MetricName,
        metric_name_string => atom_to_binary(MetricName, utf8),
        cardinality => Cardinality,
        label_keys => UniqueLabelKeys,
        label_combinations => LabelCombinations,
        type => infer_metric_type(MetricName)
    }.

%% @doc Document all metrics
-spec document_all_metrics() -> map().
document_all_metrics() ->
    AllMetrics = get_all_metrics(),
    Documentation = lists:foldl(fun(MetricName, Acc) ->
        Doc = get_metric_documentation(MetricName),
        maps:put(MetricName, Doc, Acc)
    end, #{}, AllMetrics),
    Documentation.

%% @doc Find metric name mismatches between expected and actual
-spec find_metric_mismatches([atom()]) -> {ok, [atom()]} | {error, [atom()], [atom()]}.
find_metric_mismatches(ExpectedMetrics) ->
    ActualMetrics = get_all_metrics(),
    
    MissingMetrics = lists:filter(fun(M) -> not lists:member(M, ActualMetrics) end, ExpectedMetrics),
    UnexpectedMetrics = lists:filter(fun(M) -> not lists:member(M, ExpectedMetrics) end, ActualMetrics),
    
    case {MissingMetrics, UnexpectedMetrics} of
        {[], []} ->
            {ok, ActualMetrics};
        _ ->
            {error, MissingMetrics, UnexpectedMetrics}
    end.

%% Internal: Infer metric type from name
-spec infer_metric_type(atom()) -> binary().
infer_metric_type(MetricName) ->
    MetricNameStr = atom_to_binary(MetricName, utf8),
    case re:run(MetricNameStr, "_total$|_count$", [{capture, none}]) of
        match -> <<"counter">>;
        nomatch ->
            case re:run(MetricNameStr, "_bucket$", [{capture, none}]) of
                match -> <<"histogram">>;
                nomatch ->
                    case re:run(MetricNameStr, "_sum$", [{capture, none}]) of
                        match -> <<"summary">>;
                        nomatch -> <<"gauge">>
                    end
            end
    end.

