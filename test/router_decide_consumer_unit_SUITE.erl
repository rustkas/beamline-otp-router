%% @doc Unit Tests for router_decide_consumer module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_decide_consumer_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports  
-export([
    test_module_exports/1,
    test_gen_server_callbacks/1,
    test_start_link_exported/1,
    test_behaviour_gen_server/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_module_exports,
            test_gen_server_callbacks,
            test_start_link_exported,
            test_behaviour_gen_server
        ]}
    ].

init_per_suite(Config) ->
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
    %% Verify module is loaded and has exports
    {module, router_decide_consumer} = code:ensure_loaded(router_decide_consumer),
    Exports = router_decide_consumer:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ok.

test_start_link_exported(_Config) ->
    %% Test that module exports start_link
    Exports = router_decide_consumer:module_info(exports),
    ?assertEqual(true, lists:member({start_link, 0}, Exports)),
    ok.

test_gen_server_callbacks(_Config) ->
    %% Verify module implements gen_server callbacks
    Exports = router_decide_consumer:module_info(exports),
    
    %% Check required callbacks
    ?assertEqual(true, lists:member({init, 1}, Exports)),
    ?assertEqual(true, lists:member({handle_call, 3}, Exports)),
    ?assertEqual(true, lists:member({handle_cast, 2}, Exports)),
    ?assertEqual(true, lists:member({handle_info, 2}, Exports)),
    ok.

test_behaviour_gen_server(_Config) ->
    %% Verify module compiles as gen_server behaviour
    %% Check module attributes for behaviour declaration
    Attrs = router_decide_consumer:module_info(attributes),
    Behaviours = proplists:get_value(behaviour, Attrs, []) ++ 
                 proplists:get_value(behavior, Attrs, []),
    ?assertEqual(true, lists:member(gen_server, Behaviours)),
    ok.
