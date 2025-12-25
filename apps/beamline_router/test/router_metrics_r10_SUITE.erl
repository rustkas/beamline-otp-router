-module(router_metrics_r10_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups_for_level/1, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.
