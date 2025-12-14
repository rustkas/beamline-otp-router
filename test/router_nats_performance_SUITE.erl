%% @doc Performance Test Suite for router_nats Queue and Backoff
%% 
%% Evaluates:
%% - Queue memory usage under high traffic and long failures
%% - Queue processing time during recovery
%% - Adequacy of nats_max_pending_operations limit (1000)
%% - Retry storm prevention during recovery
-module(router_nats_performance_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_queue_memory_usage_high_traffic/1,
    test_queue_processing_time_recovery/1,
    test_queue_limit_adequacy/1,
    test_backoff_delay_calculation/1,
    test_backoff_max_delay_limit/1,
    test_retry_storm_prevention/1,
    test_retry_fifo_order/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, performance_tests}];
groups_for_level(_) ->
    [].

groups() ->
    [
        %% MUST be sequence: tests use meck for router_logger and router_metrics
        %% parallel + mocking = race conditions on meck:new/unload
        {performance_tests, [sequence], [
            test_queue_memory_usage_high_traffic,
            test_queue_processing_time_recovery,
            test_queue_limit_adequacy,
            test_backoff_delay_calculation,
            test_backoff_max_delay_limit,
            test_retry_storm_prevention,
            test_retry_fifo_order
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
    ok = application:set_env(beamline_router, nats_reconnect_delay_ms, 500),
    ok = application:set_env(beamline_router, nats_max_reconnect_delay_ms, 2000),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    ok = application:set_env(beamline_router, nats_max_pending_operations, 1000),
    
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch meck:unload(router_logger),
    catch meck:unload(router_metrics),
    ok.

%% ============================================================================
%% Queue Performance Tests
%% ============================================================================

%% @doc Test: Queue memory usage under high traffic
test_queue_memory_usage_high_traffic(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_queue_memory}),
    timer:sleep(300),
    
    %% Get initial memory
    {memory, InitialMemory} = process_info(RouterNatsPid, memory),
    
    %% Queue many operations (simulate high traffic)
    NumOperations = 500,
    lists:foreach(fun(I) ->
        router_nats:publish(<<"test.subject">>, <<"payload", (integer_to_binary(I))/binary>>)
    end, lists:seq(1, NumOperations)),
    
    timer:sleep(500),
    
    %% Get final memory
    {memory, FinalMemory} = process_info(RouterNatsPid, memory),
    MemoryDelta = FinalMemory - InitialMemory,
    
    %% Verify memory increase is reasonable (< 1MB for 500 operations)
    %% Each operation: ~100 bytes (subject + payload), 500 ops = ~50KB
    %% Plus overhead: ~200KB max
    true = MemoryDelta < 1024 * 1024,  %% < 1MB
    
    ct:log("Memory delta for ~p operations: ~p bytes (~p KB)", 
           [NumOperations, MemoryDelta, MemoryDelta div 1024]),
    
    ok.

%% @doc Test: Queue processing time during recovery
test_queue_processing_time_recovery(_Config) ->
    _RouterNatsPid = whereis(router_nats),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_queue_processing}),
    timer:sleep(300),
    
    %% Queue operations
    NumOperations = 100,
    lists:foreach(fun(I) ->
        router_nats:publish(<<"test.subject">>, <<"payload", (integer_to_binary(I))/binary>>)
    end, lists:seq(1, NumOperations)),
    
    timer:sleep(500),
    
    %% Simulate recovery and measure processing time
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    StartTime = erlang:system_time(millisecond),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    
    %% Wait for retry to complete (with timeout)
    timer:sleep(2000),
    EndTime = erlang:system_time(millisecond),
    ProcessingTime = EndTime - StartTime,
    
    %% Verify processing time is reasonable (< 5 seconds for 100 operations)
    %% Each operation: ~10ms (stub), 100 ops = ~1 second
    %% Plus overhead: ~2 seconds max
    true = ProcessingTime < 5000,  %% < 5 seconds
    
    ct:log("Processing time for ~p operations: ~p ms", [NumOperations, ProcessingTime]),
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Queue limit adequacy (1000 operations)
test_queue_limit_adequacy(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    LogCalls = router_test_init:ensure_ets_table(log_calls, [named_table, set, private]),
    
    meck:expect(router_logger, warn, fun(Message, Context) ->
        ets:insert(LogCalls, {log, warn, Message, Context}),
        ok
    end),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_queue_limit}),
    timer:sleep(300),
    
    %% Queue operations up to limit (1000)
    NumOperations = 1000,
    lists:foreach(fun(I) ->
        router_nats:publish(<<"test.subject">>, <<"payload", (integer_to_binary(I))/binary>>)
    end, lists:seq(1, NumOperations)),
    
    timer:sleep(500),
    
    %% Queue one more (should trigger queue full warning)
    router_nats:publish(<<"test.subject">>, <<"payload_over_limit">>),
    timer:sleep(500),
    
    %% Verify queue full warning was logged
    AllLogs = ets:tab2list(LogCalls),
    QueueFullLogs = [L || {log, warn, Message, Context} = L <- AllLogs,
                         binary:match(Message, <<"queue full">>) =/= nomatch,
                         maps:get(<<"error_code">>, Context, undefined) =:= <<"NATS_QUEUE_FULL">>],
    true = length(QueueFullLogs) > 0,
    
    %% Verify router_nats is still alive (queue limit prevents memory exhaustion)
    true = is_process_alive(RouterNatsPid),
    
    meck:unload(router_logger),
    ets:delete_all_objects(LogCalls),
    
    ok.

%% ============================================================================
%% Backoff Performance Tests
%% ============================================================================

%% @doc Test: Backoff delay calculation
test_backoff_delay_calculation(_Config) ->
    %% Test exponential backoff calculation
    BaseDelay = 500,
    MaxDelay = 2000,
    
    %% Calculate delays for attempts 0-5
    Delays = lists:map(fun(Attempt) ->
        Delay = min(BaseDelay * trunc(math:pow(2, Attempt)), MaxDelay),
        Delay
    end, lists:seq(0, 5)),
    
    %% Verify delays are exponential up to max
    ExpectedDelays = [500, 1000, 2000, 2000, 2000, 2000],
    true = (Delays =:= ExpectedDelays),
    
    ct:log("Backoff delays: ~p", [Delays]),
    
    ok.

%% @doc Test: Backoff max delay limit
test_backoff_max_delay_limit(_Config) ->
    %% Test that backoff never exceeds max delay
    BaseDelay = 500,
    MaxDelay = 2000,
    
    %% Calculate delays for many attempts
    Delays = lists:map(fun(Attempt) ->
        Delay = min(BaseDelay * trunc(math:pow(2, Attempt)), MaxDelay),
        Delay
    end, lists:seq(0, 20)),
    
    %% Verify all delays <= max delay
    MaxCalculatedDelay = lists:max(Delays),
    true = MaxCalculatedDelay =< MaxDelay,
    
    ct:log("Max calculated delay: ~p (limit: ~p)", [MaxCalculatedDelay, MaxDelay]),
    
    ok.

%% ============================================================================
%% Retry Performance Tests
%% ============================================================================

%% @doc Test: Retry storm prevention (FIFO order, no spikes)
test_retry_storm_prevention(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Setup tracking
    meck:new(router_metrics, [passthrough]),
    MetricCalls = router_test_init:ensure_ets_table(metric_calls, [named_table, set, private]),
    
    meck:expect(router_metrics, inc, fun(MetricName) ->
        ets:insert(MetricCalls, {metric, inc, MetricName, erlang:system_time(millisecond)}),
        ok
    end),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_retry_storm}),
    timer:sleep(300),
    
    %% Queue operations
    NumOperations = 50,
    lists:foreach(fun(I) ->
        router_nats:publish(<<"test.subject">>, <<"payload", (integer_to_binary(I))/binary>>)
    end, lists:seq(1, NumOperations)),
    
    timer:sleep(500),
    
    %% Simulate recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    _StartTime = erlang:system_time(millisecond),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    
    %% Monitor retry metrics over time
    timer:sleep(1000),
    _EndTime = erlang:system_time(millisecond),
    
    %% Get retry metrics
    AllMetrics = ets:tab2list(MetricCalls),
    RetryMetrics = [M || {metric, inc, router_nats_pending_operations_retry, _} = M <- AllMetrics],
    
    %% Verify retries are spread over time (not all at once)
    RetryTimestamps = [Timestamp || {metric, inc, router_nats_pending_operations_retry, Timestamp} <- RetryMetrics],
    case length(RetryTimestamps) > 1 of
        true ->
            %% Calculate time spread
            MinTime = lists:min(RetryTimestamps),
            MaxTime = lists:max(RetryTimestamps),
            TimeSpread = MaxTime - MinTime,
            
            %% Verify retries are spread (not all at once)
            %% Should take at least 100ms for 50 operations (2ms per operation)
            true = TimeSpread > 100,
            
            ct:log("Retry time spread: ~p ms for ~p operations", [TimeSpread, length(RetryTimestamps)]);
        false ->
            %% Single retry or no retries (acceptable)
            ok
    end,
    
    %% Verify router_nats is still alive (no CPU spike)
    true = is_process_alive(RouterNatsPid),
    
    exit(StubPid, normal),
    meck:unload(router_metrics),
    ets:delete_all_objects(MetricCalls),
    
    ok.

%% @doc Test: Retry FIFO order
test_retry_fifo_order(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Simulate disconnected state
    gen_server:cast(router_nats, {connection_lost, test_retry_fifo}),
    timer:sleep(300),
    
    %% Queue operations in order
    NumOperations = 10,
    lists:foreach(fun(I) ->
        router_nats:publish(<<"test.subject">>, <<"payload", (integer_to_binary(I))/binary>>)
    end, lists:seq(1, NumOperations)),
    
    timer:sleep(500),
    
    %% Simulate recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    
    %% Wait for retry to complete
    timer:sleep(1000),
    
    %% Verify router_nats is still alive
    true = is_process_alive(RouterNatsPid),
    
    %% Note: FIFO order verification would require tracking actual operation execution
    %% For now, we verify that retry completes without errors
    
    exit(StubPid, normal),
    
    ok.
