%% @doc Shared helper functions for JetStream recovery test suites
%%
%% This module provides common functionality used by both:
%% - router_jetstream_extended_recovery_SUITE (extended functional tests)
%% - router_jetstream_soak_SUITE (ultra-long soak tests)
%%
%% @test_category helpers, shared
-module(router_jetstream_recovery_helpers).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    %% Duration scaling
    speedup_factor/0,
    scale_duration/1,
    
    %% Metrics and measurements
    collect_metrics_snapshot/0,
    get_memory_usage/0,
    measure_throughput/2,
    measure_latency/2,
    track_resources/0,
    wait_for_recovery/3,
    collect_periodic_metrics/3,
    analyze_stability_metrics/1,
    
    %% Load generation
    measure_baseline_throughput/1,
    process_test_message/0,
    process_message_batch/1,
    loop_until_time/2,
    
    %% Mocks and fault injection
    setup_standard_mocks/0,
    cleanup_mocks/0,
    
    %% Phase helpers
    run_network_partition_phase/1,
    run_jetstream_restart_phase/1,
    run_maxdeliver_exhaustion_phase/1,
    run_router_restart_phase/1,
    run_consumer_hang_phase/1,
    run_combined_faults_phase/1,
    
    %% Fault injection helpers
    inject_network_partition/0,
    remove_network_partition/0,
    inject_jetstream_restart/0,
    remove_jetstream_restart/0,
    inject_ack_failures/0,
    remove_ack_failures/0,
    
    %% Resource verification
    verify_no_resource_leaks/2,
    assert_throughput_ok/2
]).

%% ========================================================================
%% DURATION SCALING
%% ========================================================================

%% @doc Read speedup factor from ENV.
%% EXTENDED_TEST_SPEEDUP = "60" => all durations are divided by 60
-spec speedup_factor() -> pos_integer().
speedup_factor() ->
    case os:getenv("EXTENDED_TEST_SPEEDUP") of
        false ->
            1;
        Str ->
            case string:to_integer(Str) of
                {ok, N} when N >= 1 ->
                    N;
                _ ->
                    1
            end
    end.

%% @doc Scale duration in milliseconds according to speedup_factor().
%% Guarantees a minimum of 10 ms to avoid zero/too small sleeps.
-spec scale_duration(non_neg_integer()) -> non_neg_integer().
scale_duration(BaseMs) ->
    Factor = speedup_factor(),
    case Factor > 1 of
        true ->
            erlang:max(10, BaseMs div Factor);
        false ->
            BaseMs
    end.

%% ========================================================================
%% METRICS AND MEASUREMENTS
%% ========================================================================

%% @doc Collect metrics snapshot
-spec collect_metrics_snapshot() -> map().
collect_metrics_snapshot() ->
    AllMetrics = case router_jetstream_recovery_store:get_all_telemetry_events() of
        {error, _Reason} ->
            [];
        Events when is_list(Events) ->
            Events
    end,
    #{
        timestamp => erlang:monotonic_time(millisecond),
        metrics_count => length(AllMetrics),
        process_count => erlang:system_info(process_count),
        memory_usage => get_memory_usage()
    }.

%% @doc Get memory usage (if available)
-spec get_memory_usage() -> integer() | undefined.
get_memory_usage() ->
    try
        {memory, MemoryBytes} = erlang:process_info(self(), memory),
        MemoryBytes
    catch
        _:_ -> undefined
    end.

%% @doc Measure throughput over time period
-spec measure_throughput(fun(() -> ok), integer()) -> float().
measure_throughput(Action, DurationMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    _ = router_jetstream_recovery_store:init_processed_count(),
    
    %% Run action in loop
    spawn(fun() ->
        EndTime = StartTime + DurationMs,
        loop_until_time(Action, EndTime)
    end),
    
    %% Wait for duration
    timer:sleep(DurationMs),
    
    %% Get count
    Count = case router_jetstream_recovery_store:get_processed_count() of
        {ok, C} -> C;
        not_found -> 0;
        {error, _Reason} -> 0
    end,
    _ = router_jetstream_recovery_store:delete_processed_count(),
    
    %% Calculate throughput
    case DurationMs > 0 of
        true -> (Count * 1000) / DurationMs;
        false -> 0.0
    end.

%% @doc Loop until end time
-spec loop_until_time(fun(() -> ok), integer()) -> ok.
loop_until_time(Action, EndTime) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime >= EndTime of
        true ->
            ok;
        false ->
            Action(),
            _ = router_jetstream_recovery_store:inc_processed_count(),
            timer:sleep(10),
            loop_until_time(Action, EndTime)
    end.

%% @doc Measure latency (simplified - tracks processing time)
-spec measure_latency(fun(() -> ok), integer()) -> {float(), float(), float()}.
measure_latency(Action, SampleCount) ->
    Latencies = [begin
        Start = erlang:monotonic_time(microsecond),
        Action(),
        End = erlang:monotonic_time(microsecond),
        (End - Start) / 1000.0  %% Convert to milliseconds
    end || _ <- lists:seq(1, SampleCount)],
    
    Sorted = lists:sort(Latencies),
    P50 = lists:nth(round(length(Sorted) * 0.5), Sorted),
    P95 = lists:nth(round(length(Sorted) * 0.95), Sorted),
    P99 = lists:nth(round(length(Sorted) * 0.99), Sorted),
    
    {P50, P95, P99}.

%% @doc Track resource usage
-spec track_resources() -> map().
track_resources() ->
    #{
        process_count => erlang:system_info(process_count),
        memory_usage => get_memory_usage(),
        timestamp => erlang:monotonic_time(millisecond)
    }.

%% @doc Wait for system to recover to baseline
-spec wait_for_recovery(fun(() -> float()), float(), integer()) -> {ok, integer()} | {timeout, integer()}.
wait_for_recovery(MeasureFn, BaselineValue, MaxWaitMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_recovery_loop(MeasureFn, BaselineValue, MaxWaitMs, StartTime).

-spec wait_for_recovery_loop(fun(() -> float()), float(), integer(), integer()) -> {ok, integer()} | {timeout, integer()}.
wait_for_recovery_loop(MeasureFn, BaselineValue, MaxWaitMs, StartTime) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= MaxWaitMs of
        true ->
            {timeout, CurrentTime - StartTime};
        false ->
            CurrentValue = MeasureFn(),
            %% Consider recovered if within 10% of baseline
            Tolerance = BaselineValue * 0.1,
            case abs(CurrentValue - BaselineValue) =< Tolerance of
                true ->
                    {ok, CurrentTime - StartTime};
                false ->
                    timer:sleep(scale_duration(1000)),
                    wait_for_recovery_loop(MeasureFn, BaselineValue, MaxWaitMs, StartTime)
            end
    end.

%% @doc Collect periodic metrics
-spec collect_periodic_metrics(integer(), integer(), list()) -> list().
collect_periodic_metrics(EndTime, IntervalMs, Acc) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime >= EndTime of
        true ->
            lists:reverse(Acc);
        false ->
            Metrics = collect_metrics_snapshot(),
            timer:sleep(IntervalMs),
            collect_periodic_metrics(EndTime, IntervalMs, [Metrics | Acc])
    end.

%% @doc Analyze stability metrics
-spec analyze_stability_metrics(list()) -> ok.
analyze_stability_metrics(Metrics) ->
    %% Check for trends (simplified)
    %% Metrics contain process_count, memory_usage, etc.
    %% For now, just verify we collected metrics
    case Metrics of
        [] ->
            ct:comment("No metrics collected (may be expected)");
        _ ->
            ct:comment("Collected ~p metric snapshots", [length(Metrics)])
    end,
    ok.

%% ========================================================================
%% LOAD GENERATION
%% ========================================================================

%% @doc Measure baseline throughput
-spec measure_baseline_throughput(integer()) -> float().
measure_baseline_throughput(DurationMs) ->
    measure_throughput(fun() -> process_test_message() end, DurationMs).

%% @doc Process test message
-spec process_test_message() -> ok.
process_test_message() ->
    Result = #{
        <<"assignment_id">> => <<"assign-test">>,
        <<"request_id">> => <<"req-test">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => <<"acme">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    MsgId = <<"msg-test">>,
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson, #{}, MsgId}, #{}),
    ok.

%% @doc Process message batch
-spec process_message_batch(integer()) -> ok.
process_message_batch(Count) ->
    [process_test_message() || _ <- lists:seq(1, Count)],
    ok.

%% ========================================================================
%% MOCKS AND FAULT INJECTION
%% ========================================================================

%% @doc Setup standard mocks for tests
-spec setup_standard_mocks() -> ok.
setup_standard_mocks() ->
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        {ok, <<"consumer-test">>}
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    ok.

%% @doc Cleanup mocks
-spec cleanup_mocks() -> ok.
cleanup_mocks() ->
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_policy_store),
    meck:unload(router_tenant_validator),
    meck:unload(router_idempotency),
    ok.

%% ========================================================================
%% PHASE HELPERS
%% ========================================================================

%% @doc Run network partition phase
-spec run_network_partition_phase(integer()) -> ok.
run_network_partition_phase(DurationMs) ->
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_lost} end),
    timer:sleep(DurationMs),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    ok.

%% @doc Run JetStream restart phase
-spec run_jetstream_restart_phase(integer()) -> ok.
run_jetstream_restart_phase(DurationMs) ->
    %% Simulate periodic restarts
    Cycles = DurationMs div scale_duration(5 * 60 * 1000),
    [begin
        meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
            {error, connection_lost}
        end),
        timer:sleep(scale_duration(10000)),
        meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
            {ok, <<"consumer-restart">>}
        end),
        timer:sleep(scale_duration(290000))
    end || _ <- lists:seq(1, Cycles)],
    ok.

%% @doc Run MaxDeliver exhaustion phase
-spec run_maxdeliver_exhaustion_phase(integer()) -> ok.
run_maxdeliver_exhaustion_phase(DurationMs) ->
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
    timer:sleep(DurationMs),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    ok.

%% @doc Run router restart phase
-spec run_router_restart_phase(integer()) -> ok.
run_router_restart_phase(DurationMs) ->
    %% Similar to JetStream restart
    run_jetstream_restart_phase(DurationMs),
    ok.

%% @doc Run consumer hang phase
-spec run_consumer_hang_phase(integer()) -> ok.
run_consumer_hang_phase(DurationMs) ->
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
    timer:sleep(DurationMs),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    ok.

%% @doc Run combined faults phase
-spec run_combined_faults_phase(integer()) -> ok.
run_combined_faults_phase(DurationMs) ->
    %% Combine network partition and ACK failures
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_lost} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
    timer:sleep(DurationMs),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    ok.

%% ========================================================================
%% FAULT INJECTION HELPERS
%% ========================================================================

%% @doc Inject network partition
-spec inject_network_partition() -> ok.
inject_network_partition() ->
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_lost} end),
    ok.

%% @doc Remove network partition
-spec remove_network_partition() -> ok.
remove_network_partition() ->
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    ok.

%% @doc Inject JetStream restart
-spec inject_jetstream_restart() -> ok.
inject_jetstream_restart() ->
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        {error, connection_lost}
    end),
    ok.

%% @doc Remove JetStream restart
-spec remove_jetstream_restart() -> ok.
remove_jetstream_restart() ->
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        {ok, <<"consumer-restart">>}
    end),
    ok.

%% @doc Inject ACK failures
-spec inject_ack_failures() -> ok.
inject_ack_failures() ->
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
    ok.

%% @doc Remove ACK failures
-spec remove_ack_failures() -> ok.
remove_ack_failures() ->
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    ok.

%% ========================================================================
%% RESOURCE VERIFICATION
%% ========================================================================

%% @doc Verify no resource leaks
-spec verify_no_resource_leaks(map(), map()) -> ok.
verify_no_resource_leaks(Initial, Final) ->
    InitialProcessCount = maps:get(process_count, Initial, 0),
    FinalProcessCount = maps:get(process_count, Final, 0),
    ProcessLeak = FinalProcessCount - InitialProcessCount,
    
    %% Allow some process growth, but not excessive
    ?assert(ProcessLeak < 100),
    
    ct:comment("Process leak: ~p (acceptable)", [ProcessLeak]),
    ok.

%% ========================================================================
%% THROUGHPUT ASSERTIONS
%% ========================================================================

%% @doc Assert that current throughput is acceptable compared to baseline.
%% Uses RECOVERY_THROUGHPUT_MIN_RATIO env var (default 0.9 = 90%).
-spec assert_throughput_ok(float(), float()) -> ok | no_return().
assert_throughput_ok(Baseline, Current) ->
    MinRatio = case os:getenv("RECOVERY_THROUGHPUT_MIN_RATIO") of
        false ->
            0.9;  %% Default: 90% of baseline
        Str ->
            case string:to_float(Str) of
                {ok, R} when R >= 0.0, R =< 1.0 ->
                    R;
                _ ->
                    0.9
            end
    end,
    
    MinAcceptable = Baseline * MinRatio,
    ct:pal("Throughput check: Baseline=~.2f, Current=~.2f, MinRatio=~.2f, MinAcceptable=~.2f", 
               [Baseline, Current, MinRatio, MinAcceptable]),
    
    ?assert(Current >= MinAcceptable, 
            io_lib:format("Throughput ~.2f is below minimum acceptable ~.2f (ratio ~.2f)", 
                         [Current, MinAcceptable, MinRatio])),
    ok.

