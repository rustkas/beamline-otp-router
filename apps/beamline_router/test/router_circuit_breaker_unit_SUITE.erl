%% @doc Unit Tests for router_circuit_breaker module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_circuit_breaker_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_gen_server_behaviour/1,
    test_public_api_exports/1,
    test_table_info_exports/1,
    test_recovery_exports/1,
    test_state_check_exports/1
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
        {unit_tests, [parallel], [
            test_module_exports,
            test_gen_server_behaviour,
            test_public_api_exports,
            test_table_info_exports,
            test_recovery_exports,
            test_state_check_exports
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
    {module, router_circuit_breaker} = code:ensure_loaded(router_circuit_breaker),
    Exports = router_circuit_breaker:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ok.

test_gen_server_behaviour(_Config) ->
    %% Verify gen_server behaviour
    Attrs = router_circuit_breaker:module_info(attributes),
    Behaviours = proplists:get_value(behaviour, Attrs, []) ++ 
                 proplists:get_value(behavior, Attrs, []),
    ?assertEqual(true, lists:member(gen_server, Behaviours)),
    
    %% Check callbacks
    Exports = router_circuit_breaker:module_info(exports),
    ?assertEqual(true, lists:member({init, 1}, Exports)),
    ?assertEqual(true, lists:member({handle_call, 3}, Exports)),
    ?assertEqual(true, lists:member({handle_cast, 2}, Exports)),
    ?assertEqual(true, lists:member({handle_info, 2}, Exports)),
    ok.

%% ============================================================================
%% Tests for public API exports
%% ============================================================================

test_public_api_exports(_Config) ->
    Exports = router_circuit_breaker:module_info(exports),
    
    %% Core API
    ?assertEqual(true, lists:member({should_allow, 2}, Exports)),
    ?assertEqual(true, lists:member({record_success, 2}, Exports)),
    ?assertEqual(true, lists:member({record_failure, 2}, Exports)),
    ?assertEqual(true, lists:member({record_state, 2}, Exports)),
    ?assertEqual(true, lists:member({record_state_with_config, 3}, Exports)),
    ok.

%% ============================================================================
%% Tests for table info exports
%% ============================================================================

test_table_info_exports(_Config) ->
    Exports = router_circuit_breaker:module_info(exports),
    
    %% Table info functions
    ?assertEqual(true, lists:member({get_table_size, 0}, Exports)),
    ?assertEqual(true, lists:member({get_table_memory, 0}, Exports)),
    ?assertEqual(true, lists:member({check_size_limit, 0}, Exports)),
    ok.

%% ============================================================================
%% Tests for recovery exports
%% ============================================================================

test_recovery_exports(_Config) ->
    Exports = router_circuit_breaker:module_info(exports),
    
    %% Recovery API
    ?assertEqual(true, lists:member({force_recovery, 2}, Exports)),
    ?assertEqual(true, lists:member({force_recovery_to_half_open, 2}, Exports)),
    ?assertEqual(true, lists:member({get_recovery_status, 2}, Exports)),
    ?assertEqual(true, lists:member({reset_recovery_state, 2}, Exports)),
    ok.

%% ============================================================================
%% Tests for state check exports
%% ============================================================================

test_state_check_exports(_Config) ->
    Exports = router_circuit_breaker:module_info(exports),
    
    %% State checking API
    ?assertEqual(true, lists:member({get_state, 2}, Exports)),
    ?assertEqual(true, lists:member({is_open, 2}, Exports)),
    ?assertEqual(true, lists:member({is_half_open, 2}, Exports)),
    ?assertEqual(true, lists:member({is_closed, 2}, Exports)),
    ?assertEqual(true, lists:member({get_status, 2}, Exports)),
    ok.
