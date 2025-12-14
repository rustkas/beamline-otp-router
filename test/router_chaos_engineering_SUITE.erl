%% @doc Chaos Engineering Test Suite
%% 
%% Comprehensive chaos engineering tests for router resilience:
%% - Network partition scenarios
%% - Service degradation scenarios
%% - Cascading failure scenarios
%% - Recovery validation scenarios
%%
%% @test_category chaos, resilience, slow
-module(router_chaos_engineering_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

%% Common Test callbacks - MUST be exported for CT to find them
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Export test functions for Common Test
-export([
    test_chaos_network_partition_single_instance/1,
    test_chaos_network_partition_multi_instance/1,
    test_chaos_service_degradation_latency/1,
    test_chaos_service_degradation_errors/1,
    test_chaos_cascading_failure/1,
    test_chaos_recovery_validation/1,
    test_chaos_flapping_network/1,
    test_chaos_mass_failure_recovery/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, chaos_engineering_tests}];
groups_for_level(_) -> %% fast, full
    [].

groups() ->
    [
        {chaos_engineering_tests, [sequence], [
            test_chaos_network_partition_single_instance,
            test_chaos_network_partition_multi_instance,
            test_chaos_service_degradation_latency,
            test_chaos_service_degradation_errors,
            test_chaos_cascading_failure,
            test_chaos_recovery_validation,
            test_chaos_flapping_network,
            test_chaos_mass_failure_recovery
        ]}
    ].

init_per_suite(Config) ->
    %% === TESTOPS: Initialize chaos mode detection ===
    Config1 = router_testops_helper:init_chaos_mode(Config),
    
    %% === TESTOPS: Enforce Docker mode in CI if required ===
    router_testops_helper:ci_fail_on_mock_mode(),
    
    %% Check if chaos tests should run (require special env var)
    %% These tests require: NATS server, network tools (tc, iptables)
    case os:getenv("RUN_CHAOS_TESTS") of
        "true" ->
            %% Start router application
            ok = start_router_app(),
            ok = ensure_circuit_breaker_alive(),
            router_metrics:ensure(),
            router_r10_metrics:clear_metrics(),
            
            %% Start chaos testing infrastructure
            case start_chaos_infrastructure() of
                ok ->
                    safe_clear_faults(),
                    router_testops_helper:ci_log_test_mode(Config1),
                    Config1;
                {error, Reason} ->
                    stop_router_app(),
                    {skip, io_lib:format("Chaos infrastructure not available: ~p", [Reason])}
            end;
        _ ->
            {skip, "Chaos tests disabled (set RUN_CHAOS_TESTS=true to enable)"}
    end.

end_per_suite(_Config) ->
    safe_clear_faults(),
    stop_router_app(),
    ok.

init_per_testcase(TestCase, Config) ->
    %% === TESTOPS: Setup deterministic seed for reproducibility ===
    Config1 = router_testops_helper:setup_deterministic_seed(TestCase, Config),
    
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = reset_circuit_breaker(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    safe_clear_faults(),
    
    %% Check if chaos infrastructure is available for tests that need it
    NeedsNetworkPartition = lists:member(TestCase, [
        test_chaos_network_partition_single_instance,
        test_chaos_network_partition_multi_instance,
        test_chaos_flapping_network
    ]),
    case NeedsNetworkPartition of
        true ->
            case whereis(router_network_partition) of
                undefined ->
                    %% Try to start it
                    case router_network_partition:start_link() of
                        {ok, _} -> Config1;
                        _ -> {skip, "router_network_partition not available"}
                    end;
                _ -> Config1
            end;
        false ->
            Config1
    end.

end_per_testcase(TestCase, Config) ->
    safe_clear_faults(),
    
    %% === TESTOPS: Check mock discipline ===
    router_testops_helper:check_mock_discipline(TestCase, Config),
    
    %% === TESTOPS: Log seed on failure for reproduction ===
    case proplists:get_value(test_seed, Config) of
        undefined -> ok;
        Seed -> router_testops_helper:log_seed_for_reproduction(Seed)
    end,
    
    Config.

%% Start chaos testing infrastructure gen_servers
start_chaos_infrastructure() ->
    %% Ensure fault injection module is loaded
    _ = code:ensure_loaded(router_nats_fault_injection),
    
    %% Start router_network_partition if not already running
    StartResult = case whereis(router_network_partition) of
        undefined ->
            ct:pal("Starting router_network_partition..."),
            case router_network_partition:start_link() of
                {ok, Pid} ->
                    ct:pal("router_network_partition started: ~p", [Pid]),
                    ok;
                {error, {already_started, _}} -> ok;
                {error, Reason1} ->
                    ct:pal("Failed to start router_network_partition: ~p", [Reason1]),
                    {error, {network_partition_start_failed, Reason1}}
            end;
        Pid ->
            ct:pal("router_network_partition already running: ~p", [Pid]),
            ok
    end,
    %% Verify it's actually running now (only if start succeeded)
    case StartResult of
        {error, _} = Err -> Err;
        ok -> case whereis(router_network_partition) of
            undefined ->
                ct:pal("router_network_partition not found after start!"),
                {error, network_partition_not_started};
            _ ->
                %% router_nats_fault_injection is a module without gen_server (uses ETS)
                %% Check if its functions are available after loading
                case erlang:function_exported(router_nats_fault_injection, clear_all_faults, 0) of
                    true -> ok;
                    false -> {error, fault_injection_not_available}
                end
        end
    end.

%% Safe wrapper for fault injection clear
safe_clear_faults() ->
    catch router_nats_fault_injection:clear_all_faults(),
    ok.

%% ========================================================================
%% CHAOS ENGINEERING TESTS
%% ========================================================================

%% @doc Chaos test: Network partition - single instance
test_chaos_network_partition_single_instance(_Config) ->
    ct:comment("Chaos test: Network partition - single instance"),
    
    %% Create network partition
    Config = #{
        type => single_instance,
        from => <<"router">>,
        to => <<"nats">>,
        action => drop
    },
    
    {ok, PartitionId} = router_network_partition:create_partition(single_instance, Config),
    
    %% Verify partition is active
    {ok, Status} = router_network_partition:get_partition_status(PartitionId),
    ?assertEqual(active, maps:get(status, Status)),
    
    %% Perform operations (should handle partition gracefully)
    _ = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 10)),
    
    %% Verify router is still alive
    RouterPid = whereis(router_nats),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    %% Heal partition
    ok = router_network_partition:heal_partition(PartitionId),
    
    %% Verify partition is removed
    {error, not_found} = router_network_partition:get_partition_status(PartitionId),
    
    ok.

%% @doc Chaos test: Network partition - multi instance
test_chaos_network_partition_multi_instance(_Config) ->
    ct:comment("Chaos test: Network partition - multi instance"),
    
    %% Create network partition
    Config = #{
        type => multi_instance,
        from => <<"router1">>,
        to => <<"router2">>,
        action => drop
    },
    
    {ok, PartitionId} = router_network_partition:create_partition(multi_instance, Config),
    
    %% Verify partition is active
    {ok, Status} = router_network_partition:get_partition_status(PartitionId),
    ?assertEqual(active, maps:get(status, Status)),
    
    %% Perform operations (should handle partition gracefully)
    _ = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 10)),
    
    %% Verify router is still alive
    RouterPid = whereis(router_nats),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    %% Heal partition
    ok = router_network_partition:heal_partition(PartitionId),
    
    ok.

%% @doc Chaos test: Service degradation - latency
test_chaos_service_degradation_latency(_Config) ->
    ct:comment("Chaos test: Service degradation - latency"),
    
    %% Enable latency degradation
    router_nats_fault_injection:enable_fault(publish, {delay, 5000}),
    
    %% Measure latency
    StartTime = erlang:monotonic_time(millisecond),
    _ = router_nats:publish(<<"test.subject">>, <<"test">>),
    EndTime = erlang:monotonic_time(millisecond),
    Latency = EndTime - StartTime,
    
    ct:log("Latency with degradation: ~p ms", [Latency]),
    
    %% Verify latency is increased
    ?assert(Latency >= 5000, "Latency should be >= 5000ms"),
    
    %% Disable degradation
    router_nats_fault_injection:disable_fault(publish),
    
    ok.

%% @doc Chaos test: Service degradation - errors
test_chaos_service_degradation_errors(_Config) ->
    ct:comment("Chaos test: Service degradation - errors"),
    
    %% Enable error degradation (50% failure rate)
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.5}),
    
    %% Perform operations
    Results = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 100)),
    
    %% Count successes and failures
    SuccessCount = length([R || R <- Results, R =:= ok]),
    FailureCount = length([R || R <- Results, R =/= ok]),
    
    ct:log("Success count: ~p", [SuccessCount]),
    ct:log("Failure count: ~p", [FailureCount]),
    
    %% Verify approximately 50% failure rate (allow ±10% tolerance)
    ?assert(SuccessCount >= 40 andalso SuccessCount =< 60, "Success count should be ~50%"),
    ?assert(FailureCount >= 40 andalso FailureCount =< 60, "Failure count should be ~50%"),
    
    %% Disable degradation
    router_nats_fault_injection:disable_fault(publish),
    
    ok.

%% @doc Chaos test: Cascading failure
test_chaos_cascading_failure(_Config) ->
    ct:comment("Chaos test: Cascading failure"),
    
    %% Enable multiple faults to simulate cascading failure
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, nats_unavailable}),
    
    %% Perform operations
    _ = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 50)),
    
    %% Verify router is still alive
    RouterPid = whereis(router_nats),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify circuit breaker opened (if applicable)
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    case router_circuit_breaker:get_state(TenantId, ProviderId) of
        {ok, State} ->
            ct:log("Circuit breaker state: ~p", [State]);
        {error, not_found} ->
            ok
    end,
    
    %% Clear all faults
    router_nats_fault_injection:clear_all_faults(),
    
    ok.

%% @doc Chaos test: Recovery validation
test_chaos_recovery_validation(_Config) ->
    ct:comment("Chaos test: Recovery validation"),
    
    %% Enable fault
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Perform operations (should fail)
    Results1 = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 10)),
    
    %% Verify all operations failed
    lists:foreach(fun(Result) ->
        ?assertNot(Result =:= ok, "Operation should fail")
    end, Results1),
    
    %% Disable fault (recovery)
    router_nats_fault_injection:disable_fault(publish),
    
    %% Wait for recovery
    timer:sleep(2000),
    
    %% Perform operations (should succeed)
    Results2 = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 10)),
    
    %% Verify all operations succeeded
    lists:foreach(fun(Result) ->
        ?assertEqual(ok, Result)
    end, Results2),
    
    ok.

%% @doc Chaos test: Flapping network
test_chaos_flapping_network(_Config) ->
    ct:comment("Chaos test: Flapping network"),
    
    %% Create flapping network partition
    Config = #{
        type => flapping,
        from => <<"router">>,
        to => <<"nats">>,
        action => drop
    },
    
    {ok, PartitionId} = router_network_partition:simulate_flapping(Config, 1000, 1000),
    
    %% Perform operations during flapping
    _ = lists:map(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end, lists:seq(1, 20)),
    
    %% Verify router is still alive
    RouterPid = whereis(router_nats),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    %% Stop flapping
    ok = router_network_partition:stop_flapping(PartitionId),
    
    ok.

%% @doc Chaos test: Mass failure recovery
test_chaos_mass_failure_recovery(_Config) ->
    ct:comment("Chaos test: Mass failure recovery"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Enable mass failure
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Trigger circuit breaker opening
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 10)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Disable fault (recovery)
    router_nats_fault_injection:disable_fault(publish),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(1100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Record successes to meet threshold
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 2)),
    
    timer:sleep(100),
    
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    ok.

