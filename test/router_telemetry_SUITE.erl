-module(router_telemetry_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_telemetry_enabled/1]).
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(fast) -> [];
groups_for_level(_) -> [{group, basic_telemetry}].

groups() ->
    [{basic_telemetry, [sequence], [test_telemetry_enabled]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    Config.

end_per_suite(_Config) -> ok.
init_per_testcase(_, Config) -> Config.
end_per_testcase(_, _) -> ok.

test_telemetry_enabled(_Config) ->
    ct:comment("Verify telemetry app env is set"),
    {ok, true} = application:get_env(beamline_router, telemetry_enabled),
    ok.