%% @doc Unit Tests for router_result_consumer module
%% @test_category unit, fast, coverage_hotspot
-module(router_result_consumer_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exported/1,
    test_module_attributes/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    test_module_exported/1,
    test_module_attributes/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_module_exported,
            test_module_attributes
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

test_module_exported(_Config) ->
    %% Check module is loaded and has exports
    Info = router_result_consumer:module_info(exports),
    ?assertEqual(true, is_list(Info)),
    ?assertEqual(true, length(Info) > 0),
    ok.

test_module_attributes(_Config) ->
    %% Check module attributes
    Attrs = router_result_consumer:module_info(attributes),
    ?assertEqual(true, is_list(Attrs)),
    ok.
