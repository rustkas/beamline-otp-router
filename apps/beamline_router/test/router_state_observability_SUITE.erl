-module(router_state_observability_SUITE).

%% INTENTIONAL_EMPTY: placeholder suite, no test cases yet.
%% See docs/tests/README_CT_GROUPS.md.
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    missing_state_logs/1,
    invalid_json_logs/1,
    no_drift_violation_logs/1,
    log_contains_cp_fallback/0,
    tmp_file/0
]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

init_per_suite(Config) ->
    LogDir = filename:join([".windsurf", "reports_test"]),
    application:set_env(beamline_router, log_dir, LogDir),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(_) ->
    [].

groups() ->
    [].

missing_state_logs(_Config) -> ok.

invalid_json_logs(_Config) -> ok.

no_drift_violation_logs(_Config) -> ok.

log_contains_cp_fallback() ->
    LogDir = application:get_env(beamline_router, log_dir, ".windsurf/reports"),
    case file:list_dir(LogDir) of
        {ok, Files} ->
            JsonlFiles = [filename:join(LogDir, F) || F <- Files, lists:prefix("router_", F), filename:extension(F) =:= ".jsonl"],
            case JsonlFiles of
                [] -> false;
                _ ->
                    Last = lists:last(lists:sort(JsonlFiles)),
                    case file:read_file(Last) of
                        {ok, Bin} ->
                            case re:run(Bin, "\"cp_fallback\":\"CP1-baseline\"", [{capture, none}]) of
                                match -> true;
                                nomatch -> false
                            end;
                        _ -> false
                    end
            end;
        _ -> false
    end.

tmp_file() ->
    {Mega, Sec, Micro} = os:timestamp(),
    filename:join("/tmp", io_lib:format("router_state_~p_~p_~p.json", [Mega, Sec, Micro])).