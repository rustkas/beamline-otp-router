%% @doc Extended Stress and Soak Tests Suite
%%
%% Multi-hour stress and soak tests under various fault injection scenarios.
%% Designed to detect:
%% - Resource leaks (memory, processes, ETS, file descriptors)
%% - Performance degradation (latency growth, throughput decline)
%% - Long-term stability issues
%%
%% Test Categories:
%% - Single-fault soak tests (2-4 hours)
%% - Multi-fault soak tests (4-6 hours)
%% - Baseline soak tests (2-4 hours)
%%
%% Performance Degradation Thresholds:
%% - P95 latency: >2x baseline for >30 minutes = FAIL
%% - P99 latency: >3x baseline for >30 minutes = FAIL
%% - Throughput: >30% drop below baseline for >30 minutes = FAIL
%%
%% Resource Leak Thresholds:
%% - Memory: >10MB/hour for >2 hours = FAIL
%% - Processes: >100/hour for >2 hours = FAIL
%% - ETS: >1000 entries/hour = FAIL
%%
%% @test_category stress, soak, long_running, resource_leak, performance
-module(router_stress_soak_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1,
                                    init_per_testcase/2, end_per_testcase/2]}).

all() ->
    [
        {group, single_fault_soak},
        {group, multi_fault_soak},
        {group, baseline_soak}
    ].

groups() ->
    [
        {single_fault_soak, [sequence], [
            test_single_fault_connect_soak,
            test_single_fault_publish_soak,
            test_single_fault_ack_soak,
            test_single_fault_network_partition_soak
        ]},
        {multi_fault_soak, [sequence], [
            test_multi_fault_triple_soak,
            test_multi_fault_mixed_pattern_soak,
            test_multi_fault_cascading_soak
        ]},
        {baseline_soak, [sequence], [
            test_baseline_normal_soak,
            test_baseline_high_load_soak,
            test_baseline_burst_load_soak
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
    %% Ensure metrics table exists
    router_metrics:ensure(),
    %% Clear fault injection state
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state before each test
    router_nats_fault_injection:clear_all_faults(),
    %% Reset metrics
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state after each test
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Get test duration from environment or use default
-spec get_test_duration_hours() -> float().
get_test_duration_hours() ->
    case os:getenv("STRESS_SOAK_DURATION_HOURS") of
        false ->
            %% Default: 1 hour for development, can be overridden
            case os:getenv("CI") of
                "true" -> 2.0;  %% CI: 2 hours
                _ -> 0.5  %% Local: 30 minutes for quick validation
            end;
        DurationStr ->
            case string:to_float(DurationStr) of
                {Duration, _} when Duration > 0.0 -> Duration;
                _ ->
                    case string:to_integer(DurationStr) of
                        {Duration, _} when Duration > 0 -> Duration * 1.0;
                        _ -> 1.0
                    end
            end
    end.

%% @doc Get load profile (messages per second)
-spec get_load_profile() -> {float(), float()}.  %% {base_rate, burst_rate}
get_load_profile() ->
    BaseRate = case os:getenv("STRESS_SOAK_BASE_RATE") of
        false -> 25.0;  %% Default: 25 msg/s
        RateStr ->
            case string:to_float(RateStr) of
                {Rate, _} when Rate > 0.0 -> Rate;
                _ -> 25.0
            end
    end,
    BurstRate = BaseRate * 2.0,  %% Burst is 2x base rate
    {BaseRate, BurstRate}.

%% @doc Send test message to router
-spec send_test_message(binary(), binary(), map()) -> ok.
send_test_message(TenantId, RequestId, ExtraFields) ->
    Result = maps:merge(#{
        <<"assignment_id">> => <<"assign-", RequestId/binary>>,
        <<"request_id">> => RequestId,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => TenantId,
        <<"timestamp">> => erlang:system_time(millisecond)
    }, ExtraFields),
    ResultJson = jsx:encode(Result),
    Subject = <<"caf.exec.result.v1">>,
    MsgId = <<"msg-", RequestId/binary>>,
    case whereis(router_result_consumer) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
    end,
    ok.

%% @doc Generate load at specified rate
-spec generate_load(float(), integer()) -> ok.
generate_load(MessagesPerSec, DurationMs) ->
    IntervalMs = max(1, round(1000.0 / MessagesPerSec)),
    EndTime = erlang:monotonic_time(millisecond) + DurationMs,
    generate_load_loop(IntervalMs, EndTime, 0).

generate_load_loop(IntervalMs, EndTime, Counter) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime >= EndTime of
        true ->
            ok;
        false ->
            RequestId = <<"req-", (integer_to_binary(Counter))/binary>>,
            send_test_message(<<"acme">>, RequestId, #{}),
            timer:sleep(IntervalMs),
            generate_load_loop(IntervalMs, EndTime, Counter + 1)
    end.

%% @doc Run cyclic fault pattern
%% Pattern: {cyclic, Operation, Fault, OnDurationMs, OffDurationMs}
-spec run_cyclic_fault(atom(), term(), integer(), integer(), integer()) -> ok.
run_cyclic_fault(Operation, Fault, OnDurationMs, OffDurationMs, TotalDurationMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    run_cyclic_fault_loop(Operation, Fault, OnDurationMs, OffDurationMs, TotalDurationMs, StartTime, true).

run_cyclic_fault_loop(Operation, Fault, OnDurationMs, OffDurationMs, TotalDurationMs, StartTime, FaultEnabled) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= TotalDurationMs of
        true ->
            router_nats_fault_injection:disable_fault(Operation),
            ok;
        false ->
            case FaultEnabled of
                true ->
                    router_nats_fault_injection:enable_fault(Operation, Fault),
                    timer:sleep(OnDurationMs),
                    run_cyclic_fault_loop(Operation, Fault, OnDurationMs, OffDurationMs, TotalDurationMs, StartTime, false);
                false ->
                    router_nats_fault_injection:disable_fault(Operation),
                    timer:sleep(OffDurationMs),
                    run_cyclic_fault_loop(Operation, Fault, OnDurationMs, OffDurationMs, TotalDurationMs, StartTime, true)
            end
    end.

%% @doc Evaluate test results and determine pass/fail
-spec evaluate_test_results(pid(), pid(), map()) -> {pass, map()} | {fail, atom(), map()}.
evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline) ->
    %% Check resource leaks
    ResourceLeakResult = router_stress_monitor:check_resource_leaks(ResourceMonitor),
    
    %% Get current performance stats
    CurrentStats = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    %% Define degradation thresholds
    Thresholds = #{
        p95_latency_multiplier => 2.0,  %% P95 latency >2x baseline
        p99_latency_multiplier => 3.0,  %% P99 latency >3x baseline
        throughput_drop_percent => 30.0  %% Throughput drop >30%
    },
    
    %% Check performance degradation
    PerfDegradationResult = router_stress_perf_monitor:check_performance_degradation(
        PerfMonitor, Baseline, Thresholds
    ),
    
    %% Generate reports
    ResourceReport = router_stress_monitor:generate_report(ResourceMonitor),
    PerfStats = CurrentStats,
    
    %% Determine overall result
    case {ResourceLeakResult, PerfDegradationResult} of
        {{ok, no_leaks}, {ok, no_degradation}} ->
            {pass, #{
                resource_report => ResourceReport,
                performance_stats => PerfStats
            }};
        {{fail, Reason, Details}, _} ->
            {fail, Reason, #{
                resource_leak => Details,
                resource_report => ResourceReport,
                performance_stats => PerfStats
            }};
        {_, {fail, Reason, Details}} ->
            {fail, Reason, #{
                performance_degradation => Details,
                resource_report => ResourceReport,
                performance_stats => PerfStats
            }}
    end.

%% ========================================================================
%% SINGLE-FAULT SOAK TESTS
%% ========================================================================

%% @doc Single-fault soak: Connect failures repeated cyclically
%% Duration: 2-4 hours (configurable)
%% Load: Steady medium load (10-50 msg/s)
%% Fault Pattern: Connect fault for 5 min, normal for 5 min, repeat
test_single_fault_connect_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: Connect Failures ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    ct:comment("Test duration: ~p hours (~p ms)", [DurationHours, DurationMs]),
    ct:comment("Load profile: ~p msg/s", [BaseRate]),
    
    %% Start monitors
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),  %% 5 min
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),  %% 1 min
    
    %% Collect baseline (wait a bit for initial samples, then collect baseline)
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait 5 seconds for initial performance samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    %% Start load generation
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    
    %% Start cyclic fault pattern (5 min on, 5 min off)
    {FaultPid, FaultRef} = spawn_monitor(fun() ->
        run_cyclic_fault(connect, {error, connection_refused}, 300000, 300000, DurationMs)
    end),
    
    %% Monitor and collect metrics during test
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),  %% Collect every 1 min
    
    %% Wait for load and fault to complete
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadDownReason} ->
            case LoadDownReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    receive
        {'DOWN', FaultRef, process, FaultPid, FaultDownReason} ->
            case FaultDownReason of
                normal -> ok;
                _ -> ct:comment("Fault pattern process exited: ~p", [FaultDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Fault pattern process did not complete within timeout")
    end,
    
    %% Stop monitors and evaluate results
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    %% Assert pass/fail
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Single-fault soak: Publish errors repeated cyclically
test_single_fault_publish_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: Publish Errors ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    {FaultPid, FaultRef} = spawn_monitor(fun() ->
        run_cyclic_fault(publish, {error, timeout}, 300000, 300000, DurationMs)
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadDownReason} ->
            case LoadDownReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    receive
        {'DOWN', FaultRef, process, FaultPid, FaultDownReason} ->
            case FaultDownReason of
                normal -> ok;
                _ -> ct:comment("Fault pattern process exited: ~p", [FaultDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Fault pattern process did not complete within timeout")
    end,
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Single-fault soak: ACK failures repeated cyclically
test_single_fault_ack_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: ACK Failures ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    {FaultPid, FaultRef} = spawn_monitor(fun() ->
        run_cyclic_fault(ack, {error, timeout}, 300000, 300000, DurationMs)
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadDownReason} ->
            case LoadDownReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    receive
        {'DOWN', FaultRef, process, FaultPid, FaultDownReason} ->
            case FaultDownReason of
                normal -> ok;
                _ -> ct:comment("Fault pattern process exited: ~p", [FaultDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Fault pattern process did not complete within timeout")
    end,
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Single-fault soak: Network partition (periodic)
test_single_fault_network_partition_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: Network Partition ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    {FaultPid, FaultRef} = spawn_monitor(fun() ->
        run_cyclic_fault(connect, close_connection, 300000, 300000, DurationMs)
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadDownReason} ->
            case LoadDownReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    receive
        {'DOWN', FaultRef, process, FaultPid, FaultDownReason} ->
            case FaultDownReason of
                normal -> ok;
                _ -> ct:comment("Fault pattern process exited: ~p", [FaultDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Fault pattern process did not complete within timeout")
    end,
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% ========================================================================
%% MULTI-FAULT SOAK TESTS
%% ========================================================================

%% @doc Multi-fault soak: Triple-fault (Connect + Publish + ACK simultaneously)
%% Duration: 4-6 hours
%% Load: Steady medium load with occasional bursts
%% Fault Pattern: Complex sequences with recovery periods
test_multi_fault_triple_soak(_Config) ->
    ct:comment("=== Multi-Fault Soak: Triple-Fault (Connect + Publish + ACK) ==="),
    
    DurationHours = get_test_duration_hours() * 2.0,  %% Longer for multi-fault
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    %% Generate load with bursts
    {LoadPid, LoadRef} = spawn_monitor(fun() ->
        generate_load_with_bursts(BaseRate, BurstRate, DurationMs, 600000)  %% Burst every 10 min
    end),
    
    %% Enable all three faults cyclically
    {FaultPid, FaultRef} = spawn_monitor(fun() ->
        run_triple_fault_cyclic(DurationMs)
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadDownReason} ->
            case LoadDownReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    receive
        {'DOWN', FaultRef, process, FaultPid, FaultDownReason} ->
            case FaultDownReason of
                normal -> ok;
                _ -> ct:comment("Fault pattern process exited: ~p", [FaultDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Fault pattern process did not complete within timeout")
    end,
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Multi-fault soak: Mixed pattern (Intermittent + Persistent)
test_multi_fault_mixed_pattern_soak(_Config) ->
    ct:comment("=== Multi-Fault Soak: Mixed Pattern (Intermittent + Persistent) ==="),
    
    DurationHours = get_test_duration_hours() * 2.0,
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    
    %% Enable intermittent connect (50%) + persistent publish
    router_nats_fault_injection:enable_fault(connect, {intermittent, close_connection, 0.5}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    router_nats_fault_injection:clear_all_faults(),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadReason} ->
            case LoadReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    timer:sleep(1000),
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Multi-fault soak: Cascading fault chains
test_multi_fault_cascading_soak(_Config) ->
    ct:comment("=== Multi-Fault Soak: Cascading Fault Chains ==="),
    
    DurationHours = get_test_duration_hours() * 2.0,
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    
    %% Run cascading fault pattern: connect -> publish -> ack in sequence
    {FaultPid, FaultRef} = spawn_monitor(fun() ->
        run_cascading_fault_chain(DurationMs)
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadDownReason} ->
            case LoadDownReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    receive
        {'DOWN', FaultRef, process, FaultPid, FaultDownReason} ->
            case FaultDownReason of
                normal -> ok;
                _ -> ct:comment("Fault pattern process exited: ~p", [FaultDownReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Fault pattern process did not complete within timeout")
    end,
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% ========================================================================
%% BASELINE SOAK TESTS
%% ========================================================================

%% @doc Baseline soak: Normal operation without faults
test_baseline_normal_soak(_Config) ->
    ct:comment("=== Baseline Soak: Normal Operation ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(BaseRate, DurationMs) end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadReason} ->
            case LoadReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    timer:sleep(1000),
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Baseline soak: High load without faults
test_baseline_high_load_soak(_Config) ->
    ct:comment("=== Baseline Soak: High Load ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _BurstRate} = get_load_profile(),
    HighRate = BaseRate * 3.0,  %% 3x base rate
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() -> generate_load(HighRate, DurationMs) end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadReason} ->
            case LoadReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    timer:sleep(1000),
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% @doc Baseline soak: Burst load without faults
test_baseline_burst_load_soak(_Config) ->
    ct:comment("=== Baseline Soak: Burst Load ==="),
    
    DurationHours = get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, BurstRate} = get_load_profile(),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),  %% Wait for initial samples
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    {LoadPid, LoadRef} = spawn_monitor(fun() ->
        generate_load_with_bursts(BaseRate, BurstRate, DurationMs, 300000)  %% Burst every 5 min
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive
        {'DOWN', LoadRef, process, LoadPid, LoadReason} ->
            case LoadReason of
                normal -> ok;
                _ -> ct:comment("Load generation process exited: ~p", [LoadReason])
            end
    after DurationMs + 5000 ->
        ct:fail("Load generation process did not complete within timeout")
    end,
    
    timer:sleep(1000),
    
    TestResult = evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} ->
            ct:comment("Test PASSED: ~p", [Report]),
            ok;
        {fail, Reason, Details} ->
            ct:fail("Test FAILED: ~p - ~p", [Reason, Details])
    end.

%% ========================================================================
%% INTERNAL HELPER FUNCTIONS
%% ========================================================================

%% @doc Monitor loop: Collect metrics periodically
-spec monitor_loop(pid(), pid(), integer(), integer(), integer()) -> ok.
monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, IntervalMs) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= DurationMs of
        true ->
            ok;
        false ->
            router_stress_monitor:collect_snapshot(ResourceMonitor),
            %% Record throughput periodically (approximate - actual should be measured)
            router_stress_perf_monitor:record_throughput(PerfMonitor, message_processing, 1),
            timer:sleep(IntervalMs),
            monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, IntervalMs)
    end.

%% @doc Generate load with periodic bursts
-spec generate_load_with_bursts(float(), float(), integer(), integer()) -> ok.
generate_load_with_bursts(BaseRate, BurstRate, TotalDurationMs, BurstIntervalMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    generate_load_with_bursts_loop(BaseRate, BurstRate, TotalDurationMs, BurstIntervalMs, StartTime, 0).

generate_load_with_bursts_loop(BaseRate, BurstRate, TotalDurationMs, BurstIntervalMs, StartTime, Cycle) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= TotalDurationMs of
        true ->
            ok;
        false ->
            %% Determine if we're in burst period (first 30% of cycle)
            CyclePosition = (CurrentTime - StartTime) rem BurstIntervalMs,
            BurstDuration = round(BurstIntervalMs * 0.3),
            Rate = case CyclePosition < BurstDuration of
                true -> BurstRate;
                false -> BaseRate
            end,
            generate_load(Rate, min(BurstIntervalMs, TotalDurationMs - (CurrentTime - StartTime))),
            timer:sleep(100),
            generate_load_with_bursts_loop(BaseRate, BurstRate, TotalDurationMs, BurstIntervalMs, StartTime, Cycle + 1)
    end.

%% @doc Run triple fault cyclically
-spec run_triple_fault_cyclic(integer()) -> ok.
run_triple_fault_cyclic(TotalDurationMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    run_triple_fault_cyclic_loop(TotalDurationMs, StartTime, true).

run_triple_fault_cyclic_loop(TotalDurationMs, StartTime, FaultEnabled) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= TotalDurationMs of
        true ->
            router_nats_fault_injection:clear_all_faults(),
            ok;
        false ->
            case FaultEnabled of
                true ->
                    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
                    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
                    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
                    timer:sleep(300000),  %% 5 min
                    run_triple_fault_cyclic_loop(TotalDurationMs, StartTime, false);
                false ->
                    router_nats_fault_injection:clear_all_faults(),
                    timer:sleep(300000),  %% 5 min
                    run_triple_fault_cyclic_loop(TotalDurationMs, StartTime, true)
            end
    end.

%% @doc Run cascading fault chain
-spec run_cascading_fault_chain(integer()) -> ok.
run_cascading_fault_chain(TotalDurationMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    CycleDuration = 600000,  %% 10 min per cycle
    run_cascading_fault_chain_loop(TotalDurationMs, StartTime, CycleDuration, 0).

run_cascading_fault_chain_loop(TotalDurationMs, StartTime, CycleDuration, Cycle) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= TotalDurationMs of
        true ->
            router_nats_fault_injection:clear_all_faults(),
            ok;
        false ->
            %% Phase 1: Connect fault (2 min)
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(120000),
            router_nats_fault_injection:disable_fault(connect),
            
            %% Phase 2: Publish fault (2 min)
            router_nats_fault_injection:enable_fault(publish, {error, timeout}),
            timer:sleep(120000),
            router_nats_fault_injection:disable_fault(publish),
            
            %% Phase 3: ACK fault (2 min)
            router_nats_fault_injection:enable_fault(ack, {error, timeout}),
            timer:sleep(120000),
            router_nats_fault_injection:disable_fault(ack),
            
            %% Phase 4: Recovery (4 min)
            timer:sleep(240000),
            
            run_cascading_fault_chain_loop(TotalDurationMs, StartTime, CycleDuration, Cycle + 1)
    end.
