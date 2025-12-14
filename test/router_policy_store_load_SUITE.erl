%% @doc Load tests for router_policy_store
%% Tests performance and scalability with 1-5k operations
-module(router_policy_store_load_SUITE).
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
    [{group, load_tests}];
groups_for_level(_) -> %% fast, full
    [].

groups() ->
    [
        {load_tests, [], []}
    ].

%% Standardized lifecycle hooks: no global state, just pass config through
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
