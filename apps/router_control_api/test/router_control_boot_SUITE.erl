-module(router_control_boot_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, groups_for_level/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([test_control_sup_starts/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(_Level) ->
    [{group, core}].

groups() ->
    [
        {core, [], [
            test_control_sup_starts
        ]}
    ].

init_per_suite(Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    Config.

end_per_suite(_Config) ->
    ok = router_mock_helpers:cleanup_and_verify(),
    ok.

test_control_sup_starts(_Config) ->
    SupPid = case router_control_sup:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,
    case is_pid(whereis(router_control)) of
        true -> ok;
        false -> ct:fail(router_control_not_started)
    end,
    case is_pid(whereis(router_control_nats)) of
        true -> ok;
        false -> ct:fail(router_control_nats_not_started)
    end,
    unlink(SupPid),
    exit(SupPid, shutdown),
    ok.
