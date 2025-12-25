-module(router_metrics_under_faults_SUITE).

%% INTENTIONAL_EMPTY: placeholder suite, no test cases yet.
%% See docs/tests/README_CT_GROUPS.md.
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, core_tests}];
groups_for_level(full) ->
    [{group, core_tests}];
groups_for_level(_) ->
    [{group, core_tests}].

groups() ->
    [{core_tests, [sequence], []}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.
