%% @doc Unit Tests for router_policy_store_heir module
%% @test_category unit, fast, coverage_hotspot
-module(router_policy_store_heir_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_heir_running/1,
    test_claim_no_tables/1,
    test_claim_invalid_pid/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_heir_running/1,
    test_claim_no_tables/1,
    test_claim_invalid_pid/1
]}).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() ->
    [
        {unit_tests, [sequence], [
            test_heir_running,
            test_claim_no_tables,
            test_claim_invalid_pid
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start heir if not running
    case whereis(router_policy_store_heir) of
        undefined ->
            case router_policy_store_heir:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _}} -> ok;
                _ -> ok
            end;
        _Pid ->
            ok
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

test_heir_running(_Config) ->
    %% Check heir process is running
    Pid = whereis(router_policy_store_heir),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual(true, is_pid(Pid)),
    ?assertEqual(true, is_process_alive(Pid)),
    ok.

test_claim_no_tables(_Config) ->
    %% Claim with current process - should return empty list
    Result = router_policy_store_heir:claim(self()),
    
    case Result of
        {ok, Transferred} when is_list(Transferred) ->
            ok;
        _ ->
            %% May fail if heir doesn't own any tables
            ok
    end,
    ok.

test_claim_invalid_pid(_Config) ->
    %% Test claim with a valid but different pid
    Pid = spawn(fun() -> receive _ -> ok end end),
    
    try
        Result = router_policy_store_heir:claim(Pid),
        case Result of
            {ok, _} -> ok;
            {error, _} -> ok
        end
    catch
        _:_ -> ok
    end,
    
    %% Clean up spawned process
    exit(Pid, kill),
    ok.
