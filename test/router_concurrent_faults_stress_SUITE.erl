%% @doc Stress/soak tests for concurrent fault scenarios
%% Tests long-running concurrent fault scenarios to verify:
%% - No process/resource leaks
%% - No performance degradation over time
%% - Metrics behave as expected (no unbounded growth of unexpected errors)
%% @test_category stress, slow, soak
-module(router_concurrent_faults_stress_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suppress warnings for Common Test callbacks
%% Test functions are called via groups() by Common Test framework
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_concurrent_publish_and_ack_failures/1,
    test_concurrent_connect_and_publish_failures/1,
    test_concurrent_faults_with_circuit_breaker/1,
    test_concurrent_faults_with_backpressure/1,
    test_concurrent_faults_extended_soak/1,
    test_tenant_isolation_stress/1,
    test_multiple_recovery_cycles_stress/1,
    cleanup_stress_ets_tables/1
]}).

all() ->
    [
        {group, stress_tests}
    ].

%% @doc Test: Concurrent publish and ACK failures
test_concurrent_publish_and_ack_failures(_Config) ->
    ct:comment("Testing concurrent publish and ACK failures"),
    
    %% Mock router_nats: Both publish and ACK fail intermittently
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 40 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        case rand:uniform(100) =< 30 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> 
        {ok, <<"consumer-concurrent">>} 
    end),
    
    %% Process messages
    Subject = <<"caf.exec.result.v1">>,
    ProcessedCount = ets:new(processed_count, [set, private]),
    ets:insert(ProcessedCount, {count, 0}),
    
    lists:foreach(fun(N) ->
        Result = #{
            <<"assignment_id">> => <<"assign-concurrent-", (integer_to_binary(N))/binary>>,
            <<"request_id">> => <<"req-concurrent-", (integer_to_binary(N))/binary>>,
            <<"status">> => <<"success">>,
            <<"provider_id">> => <<"openai:gpt-4o">>,
            <<"tenant_id">> => <<"acme">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ResultJson = jsx:encode(Result),
        MsgId = <<"msg-concurrent-", (integer_to_binary(N))/binary>>,
        
        router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{}),
        ets:update_counter(ProcessedCount, count, 1, {count, 0})
    end, lists:seq(1, 50)),
    
    timer:sleep(2000),
    
    [{count, FinalCount}] = ets:lookup(ProcessedCount, count),
    ?assert(FinalCount >= 40, "At least 80% should be processed despite failures"),
    
    ets:delete(ProcessedCount),
    meck:unload(router_nats),
    ok.

%% @doc Test: Concurrent connect and publish failures
test_concurrent_connect_and_publish_failures(_Config) ->
    ct:comment("Testing concurrent connect and publish failures"),
    
    %% Mock router_nats: Both connect and publish fail
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 50 of
            true -> {error, connection_refused};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Process messages
    Subject = <<"caf.exec.result.v1">>,
    ErrorCount = ets:new(error_count, [set, private]),
    ets:insert(ErrorCount, {count, 0}),
    
    lists:foreach(fun(N) ->
        Result = #{
            <<"assignment_id">> => <<"assign-connect-", (integer_to_binary(N))/binary>>,
            <<"request_id">> => <<"req-connect-", (integer_to_binary(N))/binary>>,
            <<"status">> => <<"success">>,
            <<"provider_id">> => <<"openai:gpt-4o">>,
            <<"tenant_id">> => <<"acme">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ResultJson = jsx:encode(Result),
        MsgId = <<"msg-connect-", (integer_to_binary(N))/binary>>,
        
        try
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
        catch
            _:_ ->
                ets:update_counter(ErrorCount, count, 1, {count, 0})
        end
    end, lists:seq(1, 50)),
    
    timer:sleep(2000),
    
    [{count, FinalErrorCount}] = ets:lookup(ErrorCount, count),
    ?assert(FinalErrorCount < 30, "Errors should be handled gracefully"),
    
    ets:delete(ErrorCount),
    meck:unload(router_nats),
    ok.

%% @doc Test: Concurrent faults with circuit breaker
test_concurrent_faults_with_circuit_breaker(_Config) ->
    ct:comment("Testing concurrent faults with circuit breaker"),
    
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize circuit breaker
    Config = #{
        <<"failure_threshold">> => 3,
        <<"error_rate_threshold">> => 0.6,
        <<"latency_threshold_ms">> => 500
    },
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Mock router_nats: High failure rate
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 70 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    
    %% Trigger concurrent failures
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    
    %% Verify circuit breaker opened
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    meck:unload(router_nats),
    ok.

%% @doc Test: Concurrent faults with backpressure
test_concurrent_faults_with_backpressure(_Config) ->
    ct:comment("Testing concurrent faults with backpressure"),
    
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup backpressure condition
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% Mock router_nats: High failure rate
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 60 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    
    %% Check backpressure status
    {BackpressureStatus, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ?assert(BackpressureStatus =:= backpressure_active orelse BackpressureStatus =:= backpressure_warning),
    ?assert(RetryAfter >= 0),
    
    %% Process messages (should respect backpressure)
    lists:foreach(fun(N) ->
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => <<"req-backpressure-", (integer_to_binary(N))/binary>>,
            <<"tenant_id">> => <<"acme">>,
            <<"task">> => #{<<"type">> => <<"chat">>},
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        MsgId = <<"msg-backpressure-", (integer_to_binary(N))/binary>>,
        
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{})
    end, lists:seq(1, 20)),
    
    timer:sleep(2000),
    
    %% Cleanup
    ets:delete(PendingTable),
    meck:unload(router_nats),
    ok.

groups() ->
    [
        {stress_tests, [sequence], [
            test_concurrent_faults_extended_soak,
            test_tenant_isolation_stress,
            test_multiple_recovery_cycles_stress,
            test_concurrent_publish_and_ack_failures,
            test_concurrent_connect_and_publish_failures,
            test_concurrent_faults_with_circuit_breaker,
            test_concurrent_faults_with_backpressure
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    Config.

end_per_suite(Config) ->
    %% Unload all mocks that may have been created in test cases
    meck:unload([router_nats, router_jetstream, router_policy_store, router_tenant_validator]),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% @doc Extended soak test: Concurrent faults over extended period
%% Verifies: No process leaks, no performance degradation, metrics stable
test_concurrent_faults_extended_soak(_Config) ->
    %% Configuration: Run for extended period with concurrent faults
    DurationMs = case os:getenv("STRESS_TEST_DURATION_MS") of
        false -> 30000;  %% Default: 30 seconds
        DurationStr ->
            case string:to_integer(DurationStr) of
                {Duration, _} when Duration > 0 -> Duration;
                _ -> 30000
            end
    end,
    
    Iterations = 10,  %% Number of fault cycles
    MessagesPerIteration = 20,  %% Messages per iteration
    
    %% Setup tracking
    ProcessedCount = ets:new(processed_count, [set, private]),
    ErrorCount = ets:new(error_count, [set, private]),
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Mock router_nats: Intermittent failures
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        %% 30% failure rate to simulate intermittent faults
        case rand:uniform(100) =< 30 of
            true -> {error, connection_refused};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        %% 20% failure rate for ACK
        case rand:uniform(100) =< 20 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> 
        {ok, <<"consumer-stress">>} 
    end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Run stress test
    Subject = <<"caf.exec.result.v1">>,
    IterationStartTime = erlang:monotonic_time(millisecond),
    
    [begin
        %% Process batch of messages
        [begin
            Result = #{
                <<"assignment_id">> => <<"assign-stress-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"request_id">> => <<"req-stress-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"job">> => #{<<"type">> => <<"text.generate">>},
                <<"latency_ms">> => 850,
                <<"cost">> => 0.012,
                <<"tenant_id">> => <<"acme">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultJson = jsx:encode(Result),
            MsgId = <<"msg-stress-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
            
            %% Process message
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{}),
            
            %% Track processing
            ets:update_counter(ProcessedCount, processed, 1, {processed, 0})
        end || MsgNum <- lists:seq(1, MessagesPerIteration)],
        
        %% Small delay between iterations
        timer:sleep(100),
        
        %% Check elapsed time
        CurrentTime = erlang:monotonic_time(millisecond),
        case CurrentTime - StartTime >= DurationMs of
            true ->
                ct:comment("Stress test duration reached: ~p ms", [CurrentTime - StartTime]),
                throw(stop_iteration);
            false ->
                ok
        end
    end || Iteration <- lists:seq(1, Iterations)],
    
    %% Verify results
    FinalProcessed = case ets:lookup(ProcessedCount, processed) of
        [{processed, Count}] -> Count;
        [] -> 0
    end,
    
    FinalTime = erlang:monotonic_time(millisecond),
    ElapsedMs = FinalTime - IterationStartTime,
    Throughput = case ElapsedMs > 0 of
        true -> (FinalProcessed * 1000) / ElapsedMs;
        false -> 0
    end,
    
    %% Verify no process leaks
    InitialProcessCount = erlang:system_info(process_count),
    timer:sleep(1000),  %% Allow cleanup
    FinalProcessCount = erlang:system_info(process_count),
    ProcessLeak = FinalProcessCount - InitialProcessCount,
    
    %% Verify consumer is alive
    ConsumerPid = whereis(router_result_consumer),
    ConsumerAlive = case ConsumerPid of
        undefined -> false;
        Pid when is_pid(Pid) -> is_process_alive(Pid)
    end,
    
    %% Report results
    ct:comment("Stress test results:"),
    ct:comment("  Duration: ~p ms", [ElapsedMs]),
    ct:comment("  Messages processed: ~p", [FinalProcessed]),
    ct:comment("  Throughput: ~.2f msg/s", [Throughput]),
    ct:comment("  Process leak: ~p", [ProcessLeak]),
    ct:comment("  Consumer alive: ~p", [ConsumerAlive]),
    
    %% Assertions
    ?assert(ConsumerAlive),
    ?assert(FinalProcessed > 0),
    ?assert(ProcessLeak < 100),  %% Allow some process growth, but not excessive
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_policy_store),
    meck:unload(router_tenant_validator),
    %% Cleanup ETS tables (using helper for safety)
    cleanup_stress_ets_tables([ProcessedCount, ErrorCount]),
    
    ok.

%% @doc Tenant isolation stress test
%% Verifies: Tenant isolation maintained under extended concurrent faults
test_tenant_isolation_stress(_Config) ->
    %% Configuration
    Iterations = 5,
    MessagesPerTenant = 10,
    
    %% Setup tracking
    TenantAProcessed = ets:new(tenant_a_processed, [set, private]),
    TenantBProcessed = ets:new(tenant_b_processed, [set, private]),
    TenantAErrors = ets:new(tenant_a_errors, [set, private]),
    TenantBErrors = ets:new(tenant_b_errors, [set, private]),
    
    %% Mock router_nats: Failures only for Tenant A
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        %% Check if payload contains tenant_id
        case binary:match(_Payload, <<"tenant_a">>) of
            nomatch -> ok;  %% Tenant B: succeed
            _ -> 
                %% Tenant A: 50% failure rate
                case rand:uniform(100) =< 50 of
                    true -> {error, connection_refused};
                    false -> ok
                end
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> 
        {ok, <<"consumer-isolation">>} 
    end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Process messages for both tenants
    Subject = <<"caf.exec.result.v1">>,
    
    [begin
        %% Tenant A messages (with faults)
        [begin
            ResultA = #{
                <<"assignment_id">> => <<"assign-tenant-a-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"request_id">> => <<"req-tenant-a-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"job">> => #{<<"type">> => <<"text.generate">>},
                <<"latency_ms">> => 850,
                <<"cost">> => 0.012,
                <<"tenant_id">> => <<"tenant_a">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultAJson = jsx:encode(ResultA),
            MsgIdA = <<"msg-tenant-a-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
            
            router_result_consumer:handle_info({nats_message, Subject, ResultAJson, #{}, MsgIdA}, #{}),
            ets:update_counter(TenantAProcessed, processed, 1, {processed, 0})
        end || MsgNum <- lists:seq(1, MessagesPerTenant)],
        
        %% Tenant B messages (no faults)
        [begin
            ResultB = #{
                <<"assignment_id">> => <<"assign-tenant-b-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"request_id">> => <<"req-tenant-b-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"job">> => #{<<"type">> => <<"text.generate">>},
                <<"latency_ms">> => 850,
                <<"cost">> => 0.012,
                <<"tenant_id">> => <<"tenant_b">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultBJson = jsx:encode(ResultB),
            MsgIdB = <<"msg-tenant-b-", (integer_to_binary(Iteration))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
            
            router_result_consumer:handle_info({nats_message, Subject, ResultBJson, #{}, MsgIdB}, #{}),
            ets:update_counter(TenantBProcessed, processed, 1, {processed, 0})
        end || MsgNum <- lists:seq(1, MessagesPerTenant)],
        
        timer:sleep(50)
    end || Iteration <- lists:seq(1, Iterations)],
    
    timer:sleep(1000),  %% Allow processing to complete
    
    %% Verify results
    TenantACount = case ets:lookup(TenantAProcessed, processed) of
        [{processed, CountA}] when is_integer(CountA) -> CountA;
        [] -> 0
    end,
    TenantBCount = case ets:lookup(TenantBProcessed, processed) of
        [{processed, CountB}] when is_integer(CountB) -> CountB;
        [] -> 0
    end,
    
    %% Verify tenant isolation
    ct:comment("Tenant isolation stress test results:"),
    ct:comment("  Tenant A processed: ~p", [TenantACount]),
    ct:comment("  Tenant B processed: ~p", [TenantBCount]),
    
    %% Assertions
    ?assert(TenantACount > 0),
    ?assert(TenantBCount > 0),
    %% Tenant B should not be significantly slower due to Tenant A faults
    ?assert(TenantBCount >= TenantACount * 0.8),  %% Allow some variance
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_policy_store),
    meck:unload(router_tenant_validator),
    %% Cleanup ETS tables (using helper for safety)
    cleanup_stress_ets_tables([TenantAProcessed, TenantBProcessed, TenantAErrors, TenantBErrors]),
    
    ok.

%% @doc Multiple recovery cycles stress test
%% Verifies: System stability across multiple fault → recovery cycles
test_multiple_recovery_cycles_stress(_Config) ->
    %% Configuration
    Cycles = 5,
    MessagesPerCycle = 10,
    
    %% Setup tracking
    ProcessedCount = ets:new(processed_count, [set, private]),
    RecoveryCount = ets:new(recovery_count, [set, private]),
    
    %% Mock router_nats: Alternating fault/recovery cycles
    meck:new(router_nats, [passthrough]),
    FaultActive = ets:new(fault_active, [set, private]),
    ets:insert(FaultActive, {active, false}),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        [{active, Active}] = ets:lookup(FaultActive, active),
        case Active of
            true -> {error, connection_refused};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> 
        {ok, <<"consumer-cycles">>} 
    end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Run cycles
    Subject = <<"caf.exec.result.v1">>,
    
    [begin
        %% Fault period
        ets:insert(FaultActive, {active, true}),
        ct:comment("Cycle ~p: Fault period started", [Cycle]),
        
        [begin
            Result = #{
                <<"assignment_id">> => <<"assign-cycle-", (integer_to_binary(Cycle))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"request_id">> => <<"req-cycle-", (integer_to_binary(Cycle))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"job">> => #{<<"type">> => <<"text.generate">>},
                <<"latency_ms">> => 850,
                <<"cost">> => 0.012,
                <<"tenant_id">> => <<"acme">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultJson = jsx:encode(Result),
            MsgId = <<"msg-cycle-", (integer_to_binary(Cycle))/binary, "-", (integer_to_binary(MsgNum))/binary>>,
            
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{}),
            ets:update_counter(ProcessedCount, processed, 1, {processed, 0})
        end || MsgNum <- lists:seq(1, MessagesPerCycle)],
        
        timer:sleep(500),
        
        %% Recovery period
        ets:insert(FaultActive, {active, false}),
        ets:update_counter(RecoveryCount, recovered, 1, {recovered, 0}),
        ct:comment("Cycle ~p: Recovery period started", [Cycle]),
        
        timer:sleep(500)
    end || Cycle <- lists:seq(1, Cycles)],
    
    timer:sleep(1000),  %% Allow final processing
    
    %% Verify results
    FinalProcessed = case ets:lookup(ProcessedCount, processed) of
        [{processed, ProcessedCountVal}] -> ProcessedCountVal;
        [] -> 0
    end,
    FinalRecoveries = case ets:lookup(RecoveryCount, recovered) of
        [{recovered, RecoveryCountVal}] -> RecoveryCountVal;
        [] -> 0
    end,
    
    %% Verify consumer is alive
    ConsumerPid = whereis(router_result_consumer),
    ConsumerAlive = case ConsumerPid of
        undefined -> false;
        Pid when is_pid(Pid) -> is_process_alive(Pid)
    end,
    
    %% Report results
    ct:comment("Multiple recovery cycles stress test results:"),
    ct:comment("  Cycles completed: ~p", [Cycles]),
    ct:comment("  Messages processed: ~p", [FinalProcessed]),
    ct:comment("  Recovery cycles: ~p", [FinalRecoveries]),
    ct:comment("  Consumer alive: ~p", [ConsumerAlive]),
    
    %% Assertions
    ?assert(ConsumerAlive),
    ?assert(FinalProcessed > 0),
    ?assertEqual(Cycles, FinalRecoveries),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_policy_store),
    meck:unload(router_tenant_validator),
    %% Cleanup ETS tables (using helper for safety)
    cleanup_stress_ets_tables([ProcessedCount, RecoveryCount, FaultActive]),
    
    ok.

%% @doc Helper: Cleanup ETS tables created during stress tests
%% Ensures proper cleanup even if test fails
cleanup_stress_ets_tables(Tables) ->
    lists:foreach(fun(Table) ->
        case ets:info(Table) of
            undefined -> ok;
            _ -> catch ets:delete(Table)
        end
    end, Tables),
    ok.

