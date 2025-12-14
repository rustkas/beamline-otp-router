%% @doc Multi-Fault Soak Tests
%%
%% Multiple fault types soak tests (4-6 hours each):
%% - Triple faults (connect + publish + ack)
%% - Mixed patterns (intermittent + persistent)
%% - Cascading faults
%%
%% @test_category soak, long_running, heavy
-module(router_soak_multi_fault_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_triple_soak/1,
    test_mixed_pattern_soak/1,
    test_cascading_soak/1
]).

suite() -> [{timetrap, {hours, 8}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, multi_fault_soak}];
        _ -> []
    end.

groups() ->
    [{multi_fault_soak, [sequence], [
        test_triple_soak,
        test_mixed_pattern_soak,
        test_cascading_soak
    ]}].

init_per_suite(Config) -> router_stress_soak_helper:init_suite(Config).
end_per_suite(Config) -> router_stress_soak_helper:end_suite(Config).
init_per_testcase(_TC, Config) -> router_stress_soak_helper:init_testcase(Config).
end_per_testcase(_TC, Config) -> router_stress_soak_helper:end_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_triple_soak(_Config) ->
    ct:comment("=== Multi-Fault Soak: Triple Faults ==="),
    run_multi_fault_soak([
        {connect, {error, connection_refused}},
        {publish, {error, timeout}},
        {ack, {error, timeout}}
    ]).

test_mixed_pattern_soak(_Config) ->
    ct:comment("=== Multi-Fault Soak: Mixed Patterns ==="),
    run_multi_fault_soak([
        {connect, {intermittent, {error, connection_refused}, 0.3}},
        {publish, {error, timeout}}
    ]).

test_cascading_soak(_Config) ->
    ct:comment("=== Multi-Fault Soak: Cascading Faults ==="),
    run_cascading_fault_soak().

%% ============================================================================
%% HELPERS
%% ============================================================================

run_multi_fault_soak(Faults) ->
    DurationHours = router_stress_soak_helper:get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _} = router_stress_soak_helper:get_load_profile(),
    
    ct:comment("Duration: ~p hours, Load: ~p msg/s, Faults: ~p", [DurationHours, BaseRate, length(Faults)]),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    Self = self(),
    LoadPid = spawn_link(fun() ->
        router_stress_soak_helper:generate_load(BaseRate, DurationMs),
        Self ! {load_done, self()}
    end),
    
    FaultPids = lists:map(fun({Op, Fault}) ->
        spawn_link(fun() ->
            router_stress_soak_helper:run_cyclic_fault(Op, Fault, 300000, 300000, DurationMs),
            Self ! {fault_done, self()}
        end)
    end, Faults),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive {load_done, LoadPid} -> ok after DurationMs + 5000 -> ok end,
    lists:foreach(fun(Pid) ->
        receive {fault_done, Pid} -> ok after 1000 -> ok end
    end, FaultPids),
    
    TestResult = router_stress_soak_helper:evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} -> ct:comment("PASSED: ~p", [Report]), ok;
        {fail, Reason, Details} -> ct:fail("FAILED: ~p - ~p", [Reason, Details])
    end.

run_cascading_fault_soak() ->
    DurationHours = router_stress_soak_helper:get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _} = router_stress_soak_helper:get_load_profile(),
    
    ct:comment("Duration: ~p hours, Load: ~p msg/s (Cascading)", [DurationHours, BaseRate]),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    Self = self(),
    spawn_link(fun() ->
        router_stress_soak_helper:generate_load(BaseRate, DurationMs),
        Self ! load_done
    end),
    
    %% Cascading: enable faults progressively
    PhaseMs = DurationMs div 3,
    spawn_link(fun() ->
        %% Phase 1: Connect only
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        timer:sleep(PhaseMs),
        %% Phase 2: Add publish
        router_nats_fault_injection:enable_fault(publish, {error, timeout}),
        timer:sleep(PhaseMs),
        %% Phase 3: Add ack
        router_nats_fault_injection:enable_fault(ack, {error, timeout}),
        timer:sleep(PhaseMs),
        router_nats_fault_injection:clear_all_faults(),
        Self ! faults_done
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive load_done -> ok after DurationMs + 5000 -> ok end,
    receive faults_done -> ok after DurationMs + 5000 -> ok end,
    
    TestResult = router_stress_soak_helper:evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} -> ct:comment("PASSED: ~p", [Report]), ok;
        {fail, Reason, Details} -> ct:fail("FAILED: ~p - ~p", [Reason, Details])
    end.

monitor_loop(ResourceMonitor, PerfMonitor, StartTime, TotalDurationMs, IntervalMs) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= TotalDurationMs of
        true -> ok;
        false ->
            router_stress_monitor:collect_snapshot(ResourceMonitor),
            router_stress_perf_monitor:collect_sample(PerfMonitor),
            timer:sleep(IntervalMs),
            monitor_loop(ResourceMonitor, PerfMonitor, StartTime, TotalDurationMs, IntervalMs)
    end.
