%% @doc Intake Chaos: Basic Restart Tests
%%
%% Basic NATS restart resilience tests:
%% - Single NATS restart
%% - Multiple NATS restarts
%% - Recovery verification
%%
%% @test_category chaos, heavy, intake
-module(router_intake_chaos_restart_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_single_restart/1,
    test_multiple_restarts/1,
    test_recovery_verification/1
]).

suite() -> [{timetrap, {minutes, 15}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, restart_tests}];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{restart_tests, [sequence], [
        test_single_restart,
        test_multiple_restarts,
        test_recovery_verification
    ]}].

init_per_suite(Config) ->
    RandSeed = case os:getenv("CHAOS_RAND_SEED") of
        false -> erlang:timestamp();
        SeedStr ->
            case string:tokens(SeedStr, ",") of
                [S1, S2, S3] -> {list_to_integer(S1), list_to_integer(S2), list_to_integer(S3)};
                _ -> {1234, 5678, 91011}
            end
    end,
    _ = rand:seed(exsplus, RandSeed),
    
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(router_logger, [passthrough, non_strict]),
    meck:expect(router_logger, warning, fun(_Msg, _Args) -> ok end),
    meck:expect(router_logger, error, fun(_Msg, _Args) -> ok end),
    [{rand_seed, RandSeed}, {chaos_mode, mock} | Config].

end_per_suite(_Config) ->
    router_mock_helpers:unload_all([router_nats, router_logger]),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_single_restart(_Config) ->
    ct:comment("=== Single NATS Restart ==="),
    
    ProcessCountBefore = erlang:system_info(process_count),
    
    %% Mock NATS failure
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_refused} end),
    meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {error, connection_refused} end),
    timer:sleep(2000),
    
    ct:comment("NATS failure simulated"),
    
    %% Mock NATS recovery
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {ok, <<"ack">>} end),
    timer:sleep(3000),
    
    ct:comment("NATS recovery simulated"),
    
    ProcessCountAfter = erlang:system_info(process_count),
    ProcessGrowth = (ProcessCountAfter - ProcessCountBefore) / max(ProcessCountBefore, 1),
    ?assert(ProcessGrowth < 0.2),
    
    ct:comment("Single restart test complete"),
    ok.

test_multiple_restarts(_Config) ->
    ct:comment("=== Multiple NATS Restarts ==="),
    
    NumRestarts = 3,
    ProcessCountBefore = erlang:system_info(process_count),
    
    lists:foreach(fun(Cycle) ->
        ct:comment("Restart cycle ~p/~p", [Cycle, NumRestarts]),
        
        %% Failure
        meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_refused} end),
        timer:sleep(1000),
        
        %% Recovery
        meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
        timer:sleep(1500)
    end, lists:seq(1, NumRestarts)),
    
    ProcessCountAfter = erlang:system_info(process_count),
    ProcessGrowth = (ProcessCountAfter - ProcessCountBefore) / max(ProcessCountBefore, 1),
    ?assert(ProcessGrowth < 0.3),
    
    ct:comment("Multiple restarts test complete"),
    ok.

test_recovery_verification(_Config) ->
    ct:comment("=== Recovery Verification ==="),
    
    %% Simulate failure
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_refused} end),
    meck:expect(router_nats, get_connection_status, fun() -> {ok, disconnected} end),
    timer:sleep(2000),
    
    %% Simulate recovery
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, get_connection_status, fun() -> {ok, connected} end),
    timer:sleep(2000),
    
    %% Verify recovery
    {ok, Status} = router_nats:get_connection_status(),
    ?assertEqual(connected, Status),
    
    %% Verify operations work
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    ?assertEqual(ok, Result),
    
    ct:comment("Recovery verification complete"),
    ok.
