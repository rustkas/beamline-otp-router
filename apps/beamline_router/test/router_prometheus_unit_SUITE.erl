%% @doc Unit Tests for router_prometheus module
%% Targeted coverage tests for Prometheus metrics export
%% @test_category unit, fast, coverage_hotspot, observability
-module(router_prometheus_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_render/1,
    test_dump_export/1,
    test_is_metric_tuple/1,
    test_format_labels/1,
    test_get_metric_metadata/1
]).

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
            test_module_exports,
            test_render,
            test_dump_export,
            test_is_metric_tuple,
            test_format_labels,
            test_get_metric_metadata
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    {module, router_prometheus} = code:ensure_loaded(router_prometheus),
    Exports = router_prometheus:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ?assertEqual(true, lists:member({render, 0}, Exports)),
    ?assertEqual(true, lists:member({dump, 0}, Exports)),
    ok.

%% ============================================================================
%% Tests for render/0
%% ============================================================================

test_render(_Config) ->
    Result = router_prometheus:render(),
    ?assertEqual(true, is_binary(Result) orelse is_list(Result)),
    ok.

%% ============================================================================
%% Tests for dump/0 export
%% ============================================================================

test_dump_export(_Config) ->
    Exports = router_prometheus:module_info(exports),
    ?assertEqual(true, lists:member({dump, 0}, Exports)),
    ?assertEqual(true, lists:member({dump, 1}, Exports)),
    ok.

%% ============================================================================
%% Tests for is_metric_tuple (if exported)
%% ============================================================================

test_is_metric_tuple(_Config) ->
    Exports = router_prometheus:module_info(exports),
    case lists:member({is_metric_tuple, 1}, Exports) of
        true ->
            ?assertEqual(true, router_prometheus:is_metric_tuple({test_metric, 42})),
            ?assertEqual(true, router_prometheus:is_metric_tuple({test_metric, 3.14})),
            ?assertEqual(true, router_prometheus:is_metric_tuple({{test_metric, [{label, value}]}, 100})),
            ?assertEqual(false, router_prometheus:is_metric_tuple(invalid));
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for format_labels (if exported)
%% ============================================================================

test_format_labels(_Config) ->
    Exports = router_prometheus:module_info(exports),
    case lists:member({format_labels, 1}, Exports) of
        true ->
            Labels = [{tenant_id, <<"test">>}, {provider_id, <<"openai">>}],
            Result = router_prometheus:format_labels(Labels),
            ?assertEqual(true, is_list(Result) orelse is_binary(Result));
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for get_metric_metadata (if exported)
%% ============================================================================

test_get_metric_metadata(_Config) ->
    Exports = router_prometheus:module_info(exports),
    case lists:member({get_metric_metadata, 1}, Exports) of
        true ->
            %% Known metrics
            {Type1, Desc1} = router_prometheus:get_metric_metadata(router_jetstream_ack_total),
            ?assertEqual(true, is_list(Type1)),
            ?assertEqual(true, is_list(Desc1)),
            
            %% Unknown metric
            {Type2, Desc2} = router_prometheus:get_metric_metadata(unknown_metric),
            ?assertEqual(true, is_list(Type2)),
            ?assertEqual(true, is_list(Desc2));
        false ->
            ok
    end,
    ok.
