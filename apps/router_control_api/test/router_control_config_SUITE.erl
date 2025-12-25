-module(router_control_config_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, groups_for_level/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([test_defaults/1, test_env_overrides/1]).

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
            test_defaults,
            test_env_overrides
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_defaults(_Config) ->
    with_clean_env(fun() ->
        assert_equal(false, router_control_config:control_mode_enabled()),
        assert_equal(<<"local">>, router_control_config:control_tenant_id()),
        assert_equal(undefined, router_control_config:control_token())
    end).

test_env_overrides(_Config) ->
    with_env([
        {"ROUTER_CONTROL_MODE", "true"},
        {"ROUTER_CONTROL_TENANT_ID", "team-1"},
        {"ROUTER_CONTROL_TOKEN", "secret"}
    ], fun() ->
        assert_equal(true, router_control_config:control_mode_enabled()),
        assert_equal(<<"team-1">>, router_control_config:control_tenant_id()),
        assert_equal(<<"secret">>, router_control_config:control_token())
    end).

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

with_clean_env(Fun) ->
    with_env([
        {"ROUTER_CONTROL_MODE", unset},
        {"ROUTER_CONTROL_TENANT_ID", unset},
        {"ROUTER_CONTROL_TOKEN", unset}
    ], Fun).

with_env(EnvVars, Fun) ->
    Prev = [{Key, os:getenv(Key)} || {Key, _} <- EnvVars],
    lists:foreach(fun({Key, Value}) -> set_env(Key, Value) end, EnvVars),
    try
        Fun()
    after
        lists:foreach(fun({Key, Value}) -> restore_env(Key, Value) end, Prev)
    end.

set_env(Key, unset) ->
    _ = os:unsetenv(Key),
    ok;
set_env(Key, Value) ->
    _ = os:putenv(Key, Value),
    ok.

restore_env(Key, false) ->
    _ = os:unsetenv(Key),
    ok;
restore_env(Key, Value) ->
    _ = os:putenv(Key, Value),
    ok.

assert_equal(Expected, Actual) ->
    case Actual =:= Expected of
        true -> ok;
        false -> ct:fail({assert_equal, Expected, Actual})
    end.
