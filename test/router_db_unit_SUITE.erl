%% @doc Unit Tests for router_db module
%% Targeted coverage tests for database stub functions
%% @test_category unit, fast, coverage_hotspot
-module(router_db_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_load_policy_unavailable/1,
    test_query_unavailable/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_module_exports,
            test_load_policy_unavailable,
            test_query_unavailable
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
    {module, router_db} = code:ensure_loaded(router_db),
    Exports = router_db:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ?assertEqual(true, lists:member({load_policy, 2}, Exports)),
    ?assertEqual(true, lists:member({query, 2}, Exports)),
    ok.

%% ============================================================================
%% Tests for load_policy/2 (stub - database not available)
%% ============================================================================

test_load_policy_unavailable(_Config) ->
    TenantId = <<"test-tenant">>,
    PolicyId = <<"test-policy">>,
    
    Result = router_db:load_policy(TenantId, PolicyId),
    %% In CP1/test env, database is not available
    ?assertMatch({error, _}, Result),
    ok.

%% ============================================================================
%% Tests for query/2 (stub - database not available)
%% ============================================================================

test_query_unavailable(_Config) ->
    Query = <<"SELECT * FROM test">>,
    Params = [],
    
    Result = router_db:query(Query, Params),
    ?assertEqual({error, database_not_available}, Result),
    ok.
