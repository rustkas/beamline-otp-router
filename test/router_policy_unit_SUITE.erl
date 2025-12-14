%% @doc Unit Tests for router_policy module
%% Targeted coverage tests for policy loading and parsing
%% @test_category unit, fast, coverage_hotspot
-module(router_policy_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_parse_policy_json_valid/1,
    test_parse_policy_json_invalid_syntax/1,
    test_parse_policy_json_map_passthrough/1,
    test_parse_policy_json_invalid_type/1,
    test_load_policy_export/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_module_exports,
            test_parse_policy_json_valid,
            test_parse_policy_json_invalid_syntax,
            test_parse_policy_json_map_passthrough,
            test_parse_policy_json_invalid_type,
            test_load_policy_export
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
    {module, router_policy} = code:ensure_loaded(router_policy),
    Exports = router_policy:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ?assertEqual(true, lists:member({load_policy, 2}, Exports)),
    ?assertEqual(true, lists:member({parse_policy_json, 1}, Exports)),
    ok.

%% ============================================================================
%% Tests for parse_policy_json/1 with valid JSON
%% ============================================================================

test_parse_policy_json_valid(_Config) ->
    %% Valid JSON policy
    ValidJson = <<"{\"policy_id\": \"test-policy\", \"weights\": {\"provider_a\": 1.0}}">>,
    Result = router_policy:parse_policy_json(ValidJson),
    ?assertMatch({ok, _}, Result),
    
    {ok, Policy} = Result,
    ?assertEqual(true, is_map(Policy)),
    ?assertEqual(<<"test-policy">>, maps:get(<<"policy_id">>, Policy)),
    ok.

%% ============================================================================
%% Tests for parse_policy_json/1 with invalid syntax
%% ============================================================================

test_parse_policy_json_invalid_syntax(_Config) ->
    %% Invalid JSON syntax
    InvalidJson = <<"not valid json">>,
    Result = router_policy:parse_policy_json(InvalidJson),
    ?assertMatch({error, {invalid_json, _}}, Result),
    
    %% Missing closing brace
    InvalidJson2 = <<"{\"key\": \"value\"">>,
    Result2 = router_policy:parse_policy_json(InvalidJson2),
    ?assertMatch({error, {invalid_json, _}}, Result2),
    ok.

%% ============================================================================
%% Tests for parse_policy_json/1 with map passthrough
%% ============================================================================

test_parse_policy_json_map_passthrough(_Config) ->
    %% Already a map - should pass through
    PolicyMap = #{<<"policy_id">> => <<"test-policy">>},
    Result = router_policy:parse_policy_json(PolicyMap),
    ?assertMatch({ok, _}, Result),
    
    {ok, ReturnedPolicy} = Result,
    ?assertEqual(PolicyMap, ReturnedPolicy),
    ok.

%% ============================================================================
%% Tests for parse_policy_json/1 with invalid type
%% ============================================================================

test_parse_policy_json_invalid_type(_Config) ->
    %% Atom input
    Result1 = router_policy:parse_policy_json(invalid),
    ?assertMatch({error, invalid_format}, Result1),
    
    %% Integer input
    Result2 = router_policy:parse_policy_json(123),
    ?assertMatch({error, invalid_format}, Result2),
    
    %% List input
    Result3 = router_policy:parse_policy_json([]),
    ?assertMatch({error, invalid_format}, Result3),
    ok.

%% ============================================================================
%% Tests for load_policy/2 export
%% ============================================================================

test_load_policy_export(_Config) ->
    Exports = router_policy:module_info(exports),
    ?assertEqual(true, lists:member({load_policy, 2}, Exports)),
    ok.
