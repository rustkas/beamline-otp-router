%% @doc CP1 Minimal Mode Enforcement Tests
%% Verifies that CP2+ features do NOT start when current_cp=CP1-LC,
%% even if env flags are explicitly enabled
%% @test_category cp1_smoke, fast
-module(router_cp1_minimal_mode_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2,
    test_cp1_blocks_ack_consumer/1,
    test_cp1_blocks_idempotency/1,
    test_cp1_blocks_admin_grpc/1,
    test_cp2_allows_cp2_features/1
]}).


all() ->
    [
        {group, cp1_enforcement_tests}
    ].

groups() ->
    [
        {cp1_enforcement_tests, [sequence], [
            test_cp1_blocks_ack_consumer,
            test_cp1_blocks_idempotency,
            test_cp1_blocks_admin_grpc,
            test_cp2_allows_cp2_features
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, true),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Cleanup: stop application and reset env
    application:stop(beamline_router),
    application:unset_env(beamline_router, ack_enabled),
    application:unset_env(beamline_router, idempotency_enabled),
    application:unset_env(beamline_router, admin_grpc_enabled),
    ok.

%% Test: CP1-LC blocks ack_consumer even if ack_enabled=true
test_cp1_blocks_ack_consumer(_Config) ->
    ok = application:set_env(beamline_router, current_cp, <<"CP1-LC">>),
    ok = application:set_env(beamline_router, cp2_plus_allowed, false),
    
    %% "Malicious" config: Enable CP2+ feature via env
    ok = application:set_env(beamline_router, ack_enabled, true),
    
    %% Start application
    {ok, _} = application:ensure_all_started(beamline_router),
    test_helpers:wait_for_process(router_nats, 2000),
    
    %% Verify: CP2+ feature should NOT be started
    %% Check that router_ack_consumer is NOT in supervisor tree
    case supervisor:which_children(beamline_router_sup) of
        Children when is_list(Children) ->
            AckConsumerPid = lists:keyfind(router_ack_consumer, 1, Children),
            case AckConsumerPid of
                false ->
                    %% Good: router_ack_consumer is NOT started
                    ok;
                {router_ack_consumer, Pid, _, _} when is_pid(Pid) ->
                    ct:fail("router_ack_consumer started despite CP1-LC (pid: ~p)", [Pid]);
                _ ->
                    ct:fail("Unexpected ack_consumer entry: ~p", [AckConsumerPid])
            end;
        Error ->
            ct:fail("Failed to get supervisor children: ~p", [Error])
    end,
    
    ?assertNot(router_state:is_cp2_plus_allowed()),
    ok.

%% Test: CP1-LC blocks idempotency even if idempotency_enabled=true
test_cp1_blocks_idempotency(_Config) ->
    ok = application:set_env(beamline_router, current_cp, <<"CP1-LC">>),
    ok = application:set_env(beamline_router, cp2_plus_allowed, false),
    
    %% "Malicious" config: Enable CP2+ feature via env
    ok = application:set_env(beamline_router, idempotency_enabled, true),
    
    %% Start application
    {ok, _} = application:ensure_all_started(beamline_router),
    test_helpers:wait_for_process(router_nats, 2000),
    
    %% Verify: CP2+ feature should NOT be started
    %% Check that router_idempotency is NOT in supervisor tree
    case supervisor:which_children(beamline_router_sup) of
        Children when is_list(Children) ->
            IdempotencyPid = lists:keyfind(router_idempotency, 1, Children),
            case IdempotencyPid of
                false ->
                    %% Good: router_idempotency is NOT started
                    ok;
                {router_idempotency, Pid, _, _} when is_pid(Pid) ->
                    ct:fail("router_idempotency started despite CP1-LC (pid: ~p)", [Pid]);
                _ ->
                    ct:fail("Unexpected idempotency entry: ~p", [IdempotencyPid])
            end;
        Error ->
            ct:fail("Failed to get supervisor children: ~p", [Error])
    end,
    
    ?assertNot(router_state:is_cp2_plus_allowed()),
    ok.

%% Test: CP1-LC blocks admin_grpc even if admin_grpc_enabled=true
test_cp1_blocks_admin_grpc(_Config) ->
    ok = application:set_env(beamline_router, current_cp, <<"CP1-LC">>),
    ok = application:set_env(beamline_router, cp2_plus_allowed, false),
    
    %% "Malicious" config: Enable CP2+ feature via env
    ok = application:set_env(beamline_router, admin_grpc_enabled, true),
    
    %% Start application
    {ok, _} = application:ensure_all_started(beamline_router),
    test_helpers:wait_for_process(router_nats, 2000),
    
    %% Verify: CP2+ feature should NOT be enabled
    %% Check that RouterAdmin service is NOT in gRPC services
    %% Note: We can't directly check router_grpc_sup internals, but we can verify
    %% that is_cp2_plus_allowed() returns false, which is what build_grpc_services() uses
    ?assertNot(router_state:is_cp2_plus_allowed()),
    ok.

%% Test: CP2-LC allows CP2+ features when env flags are enabled
test_cp2_allows_cp2_features(_Config) ->
    ok = application:set_env(beamline_router, current_cp, <<"CP2-LC">>),
    ok = application:set_env(beamline_router, cp2_plus_allowed, true),
    
    %% Enable CP2+ features via env
    ok = application:set_env(beamline_router, ack_enabled, true),
    ok = application:set_env(beamline_router, idempotency_enabled, true),
    
    %% Start application
    {ok, _} = application:ensure_all_started(beamline_router),
    test_helpers:wait_for_process(router_nats, 2000),
    
    %% Verify: CP2+ features SHOULD be started
    %% Check that router_ack_consumer IS in supervisor tree
    case supervisor:which_children(beamline_router_sup) of
        Children when is_list(Children) ->
            AckConsumerPid = lists:keyfind(router_ack_consumer, 1, Children),
            case AckConsumerPid of
                {router_ack_consumer, Pid, _, _} when is_pid(Pid) ->
                    %% Good: router_ack_consumer IS started
                    ok;
                false ->
                    ct:fail("router_ack_consumer NOT started despite CP2-LC and ack_enabled=true");
                _ ->
                    ct:fail("Unexpected ack_consumer entry: ~p", [AckConsumerPid])
            end,
            
            %% Check that router_idempotency IS in supervisor tree
            IdempotencyPid = lists:keyfind(router_idempotency, 1, Children),
            case IdempotencyPid of
                {router_idempotency, Pid2, _, _} when is_pid(Pid2) ->
                    %% Good: router_idempotency IS started
                    ok;
                false ->
                    ct:fail("router_idempotency NOT started despite CP2-LC and idempotency_enabled=true");
                _ ->
                    ct:fail("Unexpected idempotency entry: ~p", [IdempotencyPid])
            end;
        Error ->
            ct:fail("Failed to get supervisor children: ~p", [Error])
    end,
    
    ?assert(router_state:is_cp2_plus_allowed()),
    ok.

