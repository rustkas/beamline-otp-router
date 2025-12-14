%% @doc Shared helper for stress/soak tests
%%
%% Common functionality:
%% - Test duration configuration
%% - Load profile management  
%% - Message generation
%% - Cyclic fault patterns
%% - Result evaluation
%%
%% @test_category helper
-module(router_stress_soak_helper).

-export([
    get_test_duration_hours/0,
    get_load_profile/0,
    send_test_message/3,
    generate_load/2,
    run_cyclic_fault/5,
    evaluate_test_results/3,
    init_suite/1,
    end_suite/1,
    init_testcase/1,
    end_testcase/1
]).

%% @doc Get test duration from environment or use default
-spec get_test_duration_hours() -> float().
get_test_duration_hours() ->
    case os:getenv("STRESS_SOAK_DURATION_HOURS") of
        false ->
            case os:getenv("CI") of
                "true" -> 2.0;
                _ -> 0.5
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
-spec get_load_profile() -> {float(), float()}.
get_load_profile() ->
    BaseRate = case os:getenv("STRESS_SOAK_BASE_RATE") of
        false -> 25.0;
        RateStr ->
            case string:to_float(RateStr) of
                {Rate, _} when Rate > 0.0 -> Rate;
                _ -> 25.0
            end
    end,
    BurstRate = BaseRate * 2.0,
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
        undefined -> ok;
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
        true -> ok;
        false ->
            RequestId = <<"req-", (integer_to_binary(Counter))/binary>>,
            send_test_message(<<"acme">>, RequestId, #{}),
            timer:sleep(IntervalMs),
            generate_load_loop(IntervalMs, EndTime, Counter + 1)
    end.

%% @doc Run cyclic fault pattern
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

%% @doc Evaluate test results
-spec evaluate_test_results(pid(), pid(), map()) -> {pass, map()} | {fail, atom(), map()}.
evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline) ->
    ResourceLeakResult = router_stress_monitor:check_resource_leaks(ResourceMonitor),
    CurrentStats = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    Thresholds = #{
        p95_latency_multiplier => 2.0,
        p99_latency_multiplier => 3.0,
        throughput_drop_percent => 30.0
    },
    
    PerfDegradationResult = router_stress_perf_monitor:check_performance_degradation(
        PerfMonitor, Baseline, Thresholds
    ),
    
    ResourceReport = router_stress_monitor:generate_report(ResourceMonitor),
    PerfStats = CurrentStats,
    
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

%% @doc Initialize suite for stress/soak tests
init_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    router_metrics:ensure(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% @doc End suite cleanup
end_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

%% @doc Initialize testcase
init_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    Config.

%% @doc End testcase cleanup
end_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.
