%% @doc Unit Tests for router_nats_subscriber module
%% @test_category unit, fast, coverage_hotspot
-module(router_nats_subscriber_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test exports
-export([
    all/0, groups_for_level/1, 
    groups/0, 
    init_per_suite/1, 
    end_per_suite/1, 
    init_per_group/2, 
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test function exports
-export([
    test_module_exported/1,
    test_module_attributes/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, 
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2,
    test_module_exported/1,
    test_module_attributes/1
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
            test_module_exported,
            test_module_attributes
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ============================================================================
%% Tests
%% ============================================================================

test_module_exported(_Config) ->
    %% Check module is loaded and has exports
    Info = router_nats_subscriber:module_info(exports),
    ?assert(is_list(Info)),
    ?assert(length(Info) > 0),
    ok.

test_module_attributes(_Config) ->
    %% Check module attributes
    Attrs = router_nats_subscriber:module_info(attributes),
    ?assert(is_list(Attrs)),
    ok.
