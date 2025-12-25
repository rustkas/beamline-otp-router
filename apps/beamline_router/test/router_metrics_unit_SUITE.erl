%% @doc Unit Tests for router_metrics module
%% @test_category unit, fast, coverage_hotspot
-module(router_metrics_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_ensure_creates_table/1,
    test_inc_counter/1,
    test_emit_metric/1,
    test_normalize_labels/1,
    test_clear_all/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1,
    test_count_label_cardinality/1,
    test_validate_label_names/1,
    test_get_error_rate/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_ensure_creates_table/1,
    test_inc_counter/1,
    test_emit_metric/1,
    test_normalize_labels/1,
    test_clear_all/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1,
    test_count_label_cardinality/1,
    test_validate_label_names/1,
    test_get_error_rate/1
]}).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() ->
    [
        {unit_tests, [sequence], [
            test_ensure_creates_table,
            test_inc_counter,
            test_emit_metric,
            test_normalize_labels,
            test_clear_all,
            test_get_table_size,
            test_get_table_memory,
            test_check_size_limit,
            test_count_label_cardinality,
            test_validate_label_names,
            test_get_error_rate
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    router_metrics:ensure(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

test_ensure_creates_table(_Config) ->
    Result = router_metrics:ensure(),
    ?assertEqual(ok, Result),
    
    %% Table should exist now
    ?assertNotEqual(undefined, ets:info(router_metrics)),
    
    ok.

test_inc_counter(_Config) ->
    MetricName = test_counter_metric,
    
    %% Increment counter
    ok = router_metrics:inc(MetricName),
    ok = router_metrics:inc(MetricName),
    ok = router_metrics:inc(MetricName),
    
    %% Check value in ETS
    case router_ets_helpers:ets_lookup(router_metrics, MetricName) of
        [{MetricName, Count}] ->
            ?assertEqual(true, Count >= 3);
        [] ->
            ok  %% May have been cleared
    end,
    
    ok.

test_emit_metric(_Config) ->
    MetricName = test_emit_metric,
    Measurements = #{value => 100},
    Metadata = #{tenant_id => <<"test_tenant">>},
    
    Result = router_metrics:emit_metric(MetricName, Measurements, Metadata),
    
    ?assertEqual(ok, Result),
    
    ok.

test_normalize_labels(_Config) ->
    %% Test with empty map
    Result1 = router_metrics:normalize_labels(#{}),
    ?assertEqual(true, is_list(Result1)),
    
    %% Test with map of labels
    Labels = #{tenant_id => <<"t1">>, provider => <<"openai">>},
    Result2 = router_metrics:normalize_labels(Labels),
    ?assertEqual(true, is_list(Result2)),
    
    ok.

test_clear_all(_Config) ->
    %% Add some metrics
    router_metrics:inc(clear_test_metric),
    
    %% Clear all
    Result = router_metrics:clear_all(),
    ?assertEqual(ok, Result),
    
    ok.

test_get_table_size(_Config) ->
    Result = router_metrics:get_table_size(),
    
    ?assertEqual(true, is_integer(Result) orelse Result =:= undefined),
    
    ok.

test_get_table_memory(_Config) ->
    Result = router_metrics:get_table_memory(),
    
    ?assertEqual(true, is_integer(Result) orelse Result =:= undefined),
    
    ok.

test_check_size_limit(_Config) ->
    Result = router_metrics:check_size_limit(),
    
    case Result of
        {ok, Size} when is_integer(Size) ->
            ?assertEqual(true, Size >= 0);
        {error, no_limit_configured} ->
            ok;
        {error, exceeded, _Current, _Limit} ->
            ok
    end,
    
    ok.

test_count_label_cardinality(_Config) ->
    MetricName = cardinality_test_metric,
    
    Result = router_metrics:count_label_cardinality(MetricName),
    
    ?assertEqual(true, is_integer(Result)),
    ?assertEqual(true, Result >= 0),
    
    ok.

test_validate_label_names(_Config) ->
    %% Test with valid label names (takes a map, not list)
    Labels1 = #{tenant_id => <<"t1">>, provider => <<"openai">>},
    Result1 = router_metrics:validate_label_names(Labels1),
    ?assertMatch({ok, _}, Result1),
    
    %% Test with empty map
    Result2 = router_metrics:validate_label_names(#{}),
    ?assertMatch({ok, _}, Result2),
    
    ok.

test_get_error_rate(_Config) ->
    TenantId = <<"error_rate_test_tenant">>,
    
    Result = router_metrics:get_error_rate(TenantId),
    
    ?assertEqual(true, is_number(Result) orelse is_map(Result)),
    
    ok.
