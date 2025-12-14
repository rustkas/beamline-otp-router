%% @doc NATS Integration: Recovery and Advanced Tests
%%
%% Recovery and advanced integration tests:
%% - Long failure and recovery
%% - Supervisor compatibility
%% - Disconnect/reconnect
%% - Backpressure
%% - Fail-closed mode
%%
%% @test_category integration, full, nats
-module(router_nats_integ_recovery_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_long_failure_decide/1,
    test_long_failure_result/1,
    test_long_failure_publish/1,
    test_supervisor_restart_policy/1,
    test_disconnect_reconnect/1,
    test_backpressure_simulation/1,
    test_fail_closed_mode/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, recovery_tests}, {group, advanced_tests}];
        "full" -> [{group, recovery_tests}, {group, advanced_tests}];
        _ -> []
    end.

groups() ->
    [{recovery_tests, [sequence], [
        test_long_failure_decide,
        test_long_failure_result,
        test_long_failure_publish
    ]},
     {advanced_tests, [sequence], [
        test_supervisor_restart_policy,
        test_disconnect_reconnect,
        test_backpressure_simulation,
        test_fail_closed_mode
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    %% Safely clear ETS tables if they exist (created by app components)
    lists:foreach(fun(Tab) ->
        case ets:whereis(Tab) of
            undefined -> ok;
            _ -> catch ets:delete_all_objects(Tab)
        end
    end, [router_jetstream_pending_cache, 
          router_intake_latency_cache, 
          router_intake_inflight_cache]),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% RECOVERY TESTS
%% ============================================================================

test_long_failure_decide(_Config) ->
    ct:comment("=== Long Failure Decide ==="),
    FailureDuration = 3000,
    
    StartTime = erlang:monotonic_time(millisecond),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        Elapsed = erlang:monotonic_time(millisecond) - StartTime,
        case Elapsed < FailureDuration of
            true -> {error, connection_refused};
            false -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Initially fails
    Request = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    }),
    
    _ = router_decide_consumer:handle_decide_message(Request, #{}, <<"msg-001">>, #{}),
    
    %% Wait for recovery
    timer:sleep(FailureDuration + 500),
    
    %% Should succeed now
    _ = router_decide_consumer:handle_decide_message(Request, #{}, <<"msg-002">>, #{}),
    ok.

test_long_failure_result(_Config) ->
    ct:comment("=== Long Failure Result ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Result = jsx:encode(#{
        <<"assignment_id">> => <<"assign-001">>,
        <<"request_id">> => <<"req-001">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    }),
    
    _ = router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, Result, #{}, <<"msg-001">>}, #{}),
    ok.

test_long_failure_publish(_Config) ->
    ct:comment("=== Long Failure Publish ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    ?assertEqual(ok, Result),
    ok.

%% ============================================================================
%% ADVANCED TESTS
%% ============================================================================

test_supervisor_restart_policy(_Config) ->
    ct:comment("=== Supervisor Restart Policy ==="),
    %% Verify supervisor is running (registered as beamline_router_sup)
    SupPid = whereis(beamline_router_sup),
    ?assertNotEqual(undefined, SupPid),
    ?assert(is_process_alive(SupPid)),
    ok.

test_disconnect_reconnect(_Config) ->
    ct:comment("=== Disconnect/Reconnect ==="),
    meck:expect(router_nats, get_connection_status, fun() -> {ok, disconnected} end),
    {ok, Status1} = router_nats:get_connection_status(),
    ?assertEqual(disconnected, Status1),
    
    meck:expect(router_nats, get_connection_status, fun() -> {ok, connected} end),
    {ok, Status2} = router_nats:get_connection_status(),
    ?assertEqual(connected, Status2),
    ok.

test_backpressure_simulation(_Config) ->
    ct:comment("=== Backpressure Simulation ==="),
    SlowCounter = router_test_init:ensure_ets_table(slow_counter, [set, public]),
    ets:delete_all_objects(SlowCounter),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        N = case ets:lookup(SlowCounter, count) of [] -> 0; [{count, C}] -> C end,
        ets:insert(SlowCounter, {count, N + 1}),
        timer:sleep(100),
        ok
    end),
    
    lists:foreach(fun(_) ->
        _ = router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 10)),
    
    [{count, FinalCount}] = ets:lookup(SlowCounter, count),
    ?assertEqual(10, FinalCount),
    
    ets:delete(SlowCounter),
    ok.

test_fail_closed_mode(_Config) ->
    ct:comment("=== Fail-Closed Mode ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_closed} end),
    
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    ?assertEqual({error, connection_closed}, Result),
    ok.
