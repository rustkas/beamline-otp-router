%% @doc Unit tests for router_nats_server helper utilities
-module(router_nats_server_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         test_normalize_command_output_trims_whitespace/1]).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() -> [{unit_tests, [sequence], [test_normalize_command_output_trims_whitespace]}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.

%% ==========================================================================
%% Tests
%% ==========================================================================
test_normalize_command_output_trims_whitespace(_Config) ->
    Trimmed1 = router_nats_server:normalize_command_output("/usr/local/bin/nats-server\n"),
    ?assertEqual("/usr/local/bin/nats-server", Trimmed1),

    Trimmed2 = router_nats_server:normalize_command_output(<<"/usr/local/bin/nats-server\n">>),
    ?assertEqual("/usr/local/bin/nats-server", Trimmed2),

    Trimmed3 = router_nats_server:normalize_command_output("\n  spaced\t"),
    ?assertEqual("spaced", Trimmed3),

    Trimmed4 = router_nats_server:normalize_command_output("\n"),
    ?assertEqual([] , Trimmed4),

    ok.
