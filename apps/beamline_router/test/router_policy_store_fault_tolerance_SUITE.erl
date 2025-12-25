%% @doc Fault tolerance tests for router_policy_store
%% Tests heir mechanism, table transfer, and recovery after crashes
-module(router_policy_store_fault_tolerance_SUITE).

%% INTENTIONAL_EMPTY: placeholder suite, no test cases yet.
%% See docs/tests/README_CT_GROUPS.md.
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, fault_tolerance_tests}];
groups_for_level(_) -> %% fast, full
    [].

groups() ->
    [
        {fault_tolerance_tests, [], []}
    ].

%% Standardized lifecycle hooks: no shared state; keep configuration untouched
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
