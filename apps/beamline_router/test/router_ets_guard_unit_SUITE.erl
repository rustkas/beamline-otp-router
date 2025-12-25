%% @doc Unit Tests for router_ets_guard module
%% Targeted coverage tests for ETS guard functions
%% @test_category unit, fast, coverage_hotspot
-module(router_ets_guard_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_check_spec_valid/1,
    test_check_spec_invalid/1,
    test_check_spec_not_map/1,
    test_validate_spec/1,
    test_ensure_table_valid/1,
    test_ensure_table_invalid_spec/1,
    test_verify_table_valid/1,
    test_verify_table_violations/1
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
            test_check_spec_valid,
            test_check_spec_invalid,
            test_check_spec_not_map,
            test_validate_spec,
            test_ensure_table_valid,
            test_ensure_table_invalid_spec,
            test_verify_table_valid,
            test_verify_table_violations
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
    {module, router_ets_guard} = code:ensure_loaded(router_ets_guard),
    Exports = router_ets_guard:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ?assertEqual(true, lists:member({ensure_table, 2}, Exports)),
    ?assertEqual(true, lists:member({verify_table, 2}, Exports)),
    ?assertEqual(true, lists:member({check_spec, 1}, Exports)),
    ?assertEqual(true, lists:member({validate_spec, 1}, Exports)),
    ok.

%% ============================================================================
%% Tests for check_spec/1 with valid spec
%% ============================================================================

test_check_spec_valid(_Config) ->
    %% All allowed keys
    Spec1 = #{type => set, keypos => 1},
    ?assertEqual(ok, router_ets_guard:check_spec(Spec1)),
    
    Spec2 = #{type => set, keypos => 1, read_concurrency => true},
    ?assertEqual(ok, router_ets_guard:check_spec(Spec2)),
    
    Spec3 = #{type => ordered_set, keypos => 2, write_concurrency => true, compressed => false},
    ?assertEqual(ok, router_ets_guard:check_spec(Spec3)),
    
    %% Empty spec is valid
    Spec4 = #{},
    ?assertEqual(ok, router_ets_guard:check_spec(Spec4)),
    ok.

%% ============================================================================
%% Tests for check_spec/1 with invalid keys
%% ============================================================================

test_check_spec_invalid(_Config) ->
    %% Invalid key
    Spec1 = #{type => set, unknown_key => value},
    Result1 = router_ets_guard:check_spec(Spec1),
    ?assertMatch({error, _}, Result1),
    
    Spec2 = #{invalid => true, also_invalid => false},
    Result2 = router_ets_guard:check_spec(Spec2),
    ?assertMatch({error, _}, Result2),
    ok.

%% ============================================================================
%% Tests for check_spec/1 with non-map
%% ============================================================================

test_check_spec_not_map(_Config) ->
    Result1 = router_ets_guard:check_spec(not_a_map),
    ?assertEqual({error, not_a_map}, Result1),
    
    Result2 = router_ets_guard:check_spec([]),
    ?assertEqual({error, not_a_map}, Result2),
    
    Result3 = router_ets_guard:check_spec(<<"binary">>),
    ?assertEqual({error, not_a_map}, Result3),
    ok.

%% ============================================================================
%% Tests for validate_spec/1 (alias for check_spec)
%% ============================================================================

test_validate_spec(_Config) ->
    Spec1 = #{type => set},
    ?assertEqual(ok, router_ets_guard:validate_spec(Spec1)),
    
    Spec2 = #{invalid_key => value},
    ?assertMatch({error, _}, router_ets_guard:validate_spec(Spec2)),
    ok.

%% ============================================================================
%% Tests for ensure_table/2 with valid table
%% ============================================================================

test_ensure_table_valid(_Config) ->
    %% Create a test table
    TableName = ets_guard_test_table,
    router_ets_helpers:ensure_named_ets_table(TableName, [set, named_table, {keypos, 1}, {read_concurrency, true}]),
    
    try
        Spec = #{type => set, keypos => 1, read_concurrency => true},
        Result = router_ets_guard:ensure_table(TableName, Spec),
        ?assertMatch({ok, #{checked := true}}, Result)
    after
        router_test_init:delete_ets_table(TableName)
    end,
    ok.

%% ============================================================================
%% Tests for ensure_table/2 with invalid spec
%% ============================================================================

test_ensure_table_invalid_spec(_Config) ->
    TableName = ets_guard_test_invalid,
    router_ets_helpers:ensure_named_ets_table(TableName, [set, named_table]),
    
    try
        %% Invalid spec key
        Spec = #{invalid_key => value},
        Result = router_ets_guard:ensure_table(TableName, Spec),
        ?assertMatch({error, _}, Result)
    after
        router_test_init:delete_ets_table(TableName)
    end,
    ok.

%% ============================================================================
%% Tests for verify_table/2 with valid table
%% ============================================================================

test_verify_table_valid(_Config) ->
    TableName = ets_guard_verify_table,
    router_ets_helpers:ensure_named_ets_table(TableName, [ordered_set, named_table, {keypos, 2}]),
    
    try
        Spec = #{type => ordered_set, keypos => 2},
        Result = router_ets_guard:verify_table(TableName, Spec),
        ?assertEqual(ok, Result)
    after
        router_test_init:delete_ets_table(TableName)
    end,
    ok.

%% ============================================================================
%% Tests for verify_table/2 with violations
%% ============================================================================

test_verify_table_violations(_Config) ->
    TableName = ets_guard_verify_violations,
    router_ets_helpers:ensure_named_ets_table(TableName, [set, named_table, {keypos, 1}]),
    
    try
        %% Expect different type
        Spec = #{type => ordered_set, keypos => 1},
        Result = router_ets_guard:verify_table(TableName, Spec),
        ?assertMatch({error, _}, Result)
    after
        router_test_init:delete_ets_table(TableName)
    end,
    ok.
