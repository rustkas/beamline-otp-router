%% @doc Integration Test Suite for router_nats with Router Components
%% 
%% Verifies that router_nats behavior doesn't break existing use-cases:
%% - Normal operation without NATS failures
%% - Decide/result consumers work correctly
%% - Supervisor restart policies are compatible
-module(router_nats_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_normal_operation_decide_consumer/1,
    test_normal_operation_result_consumer/1,
    test_normal_operation_publish/1,
    test_fail_open_mode_decide_consumer/1,
    test_fail_open_mode_result_consumer/1,
    test_fail_open_mode_publish/1,
    test_long_failure_and_recovery_decide_consumer/1,
    test_long_failure_and_recovery_result_consumer/1,
    test_long_failure_and_recovery_publish/1,
    test_supervisor_restart_policy_compatibility/1
]}).

%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test functions
-export([
    test_normal_operation_decide_consumer/1,
    test_normal_operation_result_consumer/1,
    test_normal_operation_publish/1,
    test_fail_open_mode_decide_consumer/1,
    test_fail_open_mode_result_consumer/1,
    test_fail_open_mode_publish/1,
    test_long_failure_and_recovery_decide_consumer/1,
    test_long_failure_and_recovery_result_consumer/1,
    test_long_failure_and_recovery_publish/1,
    test_supervisor_restart_policy_compatibility/1
]).

all() ->
    [
        %% Normal operation (no failures)
        test_normal_operation_decide_consumer,
        test_normal_operation_result_consumer,
        test_normal_operation_publish,
        
        %% Fail-open mode
        test_fail_open_mode_decide_consumer,
        test_fail_open_mode_result_consumer,
        test_fail_open_mode_publish,
        
        %% Long failures and recovery
        test_long_failure_and_recovery_decide_consumer,
        test_long_failure_and_recovery_result_consumer,
        test_long_failure_and_recovery_publish,
        
        %% Supervisor compatibility
        test_supervisor_restart_policy_compatibility
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
    ok = application:set_env(beamline_router, nats_reconnect_delay_ms, 500),
    ok = application:set_env(beamline_router, nats_max_reconnect_delay_ms, 2000),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    
    case application:start(beamline_router) of
        ok ->
            timer:sleep(1000),
            Config;
        {error, {already_started, _}} ->
            timer:sleep(1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear any fault injections
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:clear_all_faults();
        false ->
            ok
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Normal Operation Tests (No Failures)
%% ============================================================================

%% @doc Test: Decide consumer works normally without NATS failures
test_normal_operation_decide_consumer(_Config) ->
    RouterNatsPid = whereis(router_nats),
    DecideConsumerPid = whereis(router_decide_consumer),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(DecideConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Verify connection status
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= connected),
    
    %% Verify decide consumer can subscribe (stub implementation)
    Result = router_nats:subscribe_jetstream(
        <<"beamline.router.v1.decide">>,
        <<"decide-consumer">>,
        explicit,
        undefined,
        push
    ),
    true = (case Result of {ok, _} -> true; ok -> true; _ -> false end),
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Result consumer works normally without NATS failures
test_normal_operation_result_consumer(_Config) ->
    RouterNatsPid = whereis(router_nats),
    ResultConsumerPid = whereis(router_result_consumer),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(ResultConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Verify connection status
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= connected),
    
    %% Verify result consumer can subscribe (stub implementation)
    Result = router_nats:subscribe_jetstream(
        <<"caf.exec.result.v1">>,
        <<"result-consumer">>,
        explicit,
        undefined,
        push
    ),
    true = (case Result of {ok, _} -> true; ok -> true; _ -> false end),
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Publish works normally without NATS failures
test_normal_operation_publish(_Config) ->
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Verify publish works
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    true = (Result =:= ok),
    
    %% Verify publish_with_ack works
    Result2 = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (case Result2 of {ok, _} -> true; ok -> true; _ -> false end),
    
    exit(StubPid, normal),
    
    ok.

%% ============================================================================
%% Fail-Open Mode Tests
%% ============================================================================

%% @doc Test: Decide consumer works in fail-open mode
test_fail_open_mode_decide_consumer(_Config) ->
    RouterNatsPid = whereis(router_nats),
    DecideConsumerPid = whereis(router_decide_consumer),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Enable fail-open mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, true),
    
    %% Restart router_nats to pick up new config
    supervisor:terminate_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    supervisor:restart_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(DecideConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_fail_open}),
    timer:sleep(300),
    
    %% Verify connection is disconnected
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= disconnected orelse State =:= reconnecting),
    
    %% Verify decide consumer can still operate (fail-open mode)
    %% In fail-open mode, operations return ok even when disconnected
    Result = router_nats:publish(<<"beamline.router.v1.decide">>, <<"test">>),
    true = (Result =:= ok),  %% Fail-open: ok returned
    
    %% Restore normal mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    
    ok.

%% @doc Test: Result consumer works in fail-open mode
test_fail_open_mode_result_consumer(_Config) ->
    RouterNatsPid = whereis(router_nats),
    ResultConsumerPid = whereis(router_result_consumer),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Enable fail-open mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, true),
    
    %% Restart router_nats to pick up new config
    supervisor:terminate_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    supervisor:restart_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(ResultConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_fail_open}),
    timer:sleep(300),
    
    %% Verify result consumer can still operate (fail-open mode)
    Result = router_nats:publish(<<"caf.exec.result.v1">>, <<"test">>),
    true = (Result =:= ok),  %% Fail-open: ok returned
    
    %% Restore normal mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    
    ok.

%% @doc Test: Publish works in fail-open mode
test_fail_open_mode_publish(_Config) ->
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Enable fail-open mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, true),
    
    %% Restart router_nats to pick up new config
    supervisor:terminate_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    supervisor:restart_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_fail_open}),
    timer:sleep(300),
    
    %% Verify publish returns ok in fail-open mode
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    true = (Result =:= ok),  %% Fail-open: ok returned
    
    %% Verify publish_with_ack returns ok in fail-open mode
    Result2 = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (Result2 =:= ok),  %% Fail-open: ok returned
    
    %% Restore normal mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    
    ok.

%% ============================================================================
%% Long Failures and Recovery Tests
%% ============================================================================

%% @doc Test: Decide consumer recovers after long failure
test_long_failure_and_recovery_decide_consumer(_Config) ->
    RouterNatsPid = whereis(router_nats),
    DecideConsumerPid = whereis(router_decide_consumer),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(DecideConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate long failure
    gen_server:cast(router_nats, {connection_lost, test_long_failure}),
    timer:sleep(2000),  %% Wait for multiple reconnect attempts
    
    %% Verify processes still alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(DecideConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(500),
    
    %% Verify connection restored
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= connected),
    
    %% Verify decide consumer can operate after recovery
    Result = router_nats:subscribe_jetstream(
        <<"beamline.router.v1.decide">>,
        <<"decide-consumer">>,
        explicit,
        undefined,
        push
    ),
    true = (case Result of {ok, _} -> true; ok -> true; _ -> false end),
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Result consumer recovers after long failure
test_long_failure_and_recovery_result_consumer(_Config) ->
    RouterNatsPid = whereis(router_nats),
    ResultConsumerPid = whereis(router_result_consumer),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(ResultConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate long failure
    gen_server:cast(router_nats, {connection_lost, test_long_failure}),
    timer:sleep(2000),  %% Wait for multiple reconnect attempts
    
    %% Verify processes still alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(ResultConsumerPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(500),
    
    %% Verify connection restored
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= connected),
    
    %% Verify result consumer can operate after recovery
    Result = router_nats:subscribe_jetstream(
        <<"caf.exec.result.v1">>,
        <<"result-consumer">>,
        explicit,
        undefined,
        push
    ),
    true = (case Result of {ok, _} -> true; ok -> true; _ -> false end),
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Publish recovers after long failure
test_long_failure_and_recovery_publish(_Config) ->
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Verify all processes are alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate long failure
    gen_server:cast(router_nats, {connection_lost, test_long_failure}),
    timer:sleep(2000),  %% Wait for multiple reconnect attempts
    
    %% Verify processes still alive
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    
    %% Simulate recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(500),
    
    %% Verify connection restored
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= connected),
    
    %% Verify publish works after recovery
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    true = (Result =:= ok),
    
    %% Verify publish_with_ack works after recovery
    Result2 = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (case Result2 of {ok, _} -> true; ok -> true; _ -> false end),
    
    exit(StubPid, normal),
    
    ok.

%% ============================================================================
%% Supervisor Compatibility Tests
%% ============================================================================

%% @doc Test: Supervisor restart policy is compatible with router_nats reconnect logic
test_supervisor_restart_policy_compatibility(_Config) ->
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Get supervisor restart policy
    {ok, SupSpec} = supervisor:get_childspec(beamline_router_sup, router_nats),
    Restart = proplists:get_value(restart, SupSpec),
    Shutdown = proplists:get_value(shutdown, SupSpec),
    
    %% Verify restart policy is permanent (router_nats should always restart)
    true = (Restart =:= permanent),
    
    %% Verify shutdown timeout is reasonable (5 seconds default)
    true = (Shutdown =:= 5000 orelse Shutdown =:= infinity),
    
    %% Simulate connection loss (should not trigger supervisor restart)
    gen_server:cast(router_nats, {connection_lost, test_supervisor_compat}),
    timer:sleep(1000),
    
    %% Verify router_nats is still alive (supervisor didn't restart it)
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    
    %% Verify router_nats handles reconnection internally (not via supervisor)
    {ok, State} = router_nats:get_connection_status(),
    true = (State =:= disconnected orelse State =:= reconnecting orelse State =:= connected),
    
    ok.

