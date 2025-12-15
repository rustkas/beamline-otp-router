%% @doc Test suite for NATS contract validation
%% Tests runtime contract assertions for NATS headers
%% @test_category fast
-module(router_nats_contract_validation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_header_validation_placeholder/1]).

all() ->
    router_ct_groups:all_selection(?MODULE, [{group, unit_tests}]).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    router_ct_groups:groups_definitions(?MODULE, base_groups()).

base_groups() ->
    [
        {unit_tests, [], [test_header_validation_placeholder]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.



test_header_validation_placeholder(_Config) ->
    ct:comment("This test suite serves as a placeholder for NATS header contract validation logic"),
    ok.
