%% @doc Baseline Soak Tests
%%
%% Baseline soak tests without faults (2-4 hours each):
%% - Normal load
%% - High load
%% - Burst load
%%
%% @test_category soak, long_running, heavy, baseline
-module(router_soak_baseline_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_normal_soak/1,
    test_high_load_soak/1,
    test_burst_load_soak/1
]).

suite() -> [{timetrap, {hours, 6}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, baseline_soak}];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{baseline_soak, [sequence], [
        test_normal_soak,
        test_high_load_soak,
        test_burst_load_soak
    ]}].

init_per_suite(Config) -> router_stress_soak_helper:init_suite(Config).
end_per_suite(Config) -> router_stress_soak_helper:end_suite(Config).
init_per_testcase(_TC, Config) -> router_stress_soak_helper:init_testcase(Config).
end_per_testcase(_TC, Config) -> router_stress_soak_helper:end_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_normal_soak(_Config) ->
    ct:comment("=== Baseline Soak: Normal Load ==="),
    {BaseRate, _} = router_stress_soak_helper:get_load_profile(),
    run_baseline_soak(BaseRate).

test_high_load_soak(_Config) ->
    ct:comment("=== Baseline Soak: High Load ==="),
    {BaseRate, _} = router_stress_soak_helper:get_load_profile(),
    run_baseline_soak(BaseRate * 2.0).

test_burst_load_soak(_Config) ->
    ct:comment("=== Baseline Soak: Burst Load ==="),
    run_burst_soak().

%% ============================================================================
%% HELPERS
%% ============================================================================

run_baseline_soak(Rate) ->
    DurationHours = router_stress_soak_helper:get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    
    ct:comment("Duration: ~p hours, Load: ~p msg/s", [DurationHours, Rate]),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    Self = self(),
    spawn_link(fun() ->
        router_stress_soak_helper:generate_load(Rate, DurationMs),
        Self ! load_done
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive load_done -> ok after DurationMs + 5000 -> ok end,
    
    TestResult = router_stress_soak_helper:evaluate_test_results(ResourceMonitor, PerfMonitor, Baseline),
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    case TestResult of
        {pass, Report} -> ct:comment("PASSED: ~p", [Report]), ok;
        {fail, Reason, Details} -> ct:fail("FAILED: ~p - ~p", [Reason, Details])
    end.

run_burst_soak() ->
    DurationHours = router_stress_soak_helper:get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, BurstRate} = router_stress_soak_helper:get_load_profile(),
    
    ct:comment("Duration: ~p hours, Base: ~p msg/s, Burst: ~p msg/s", [DurationHours, BaseRate, BurstRate]),
    
    {ok, ResourceMonitor} = router_stress_monitor:start(#{collection_interval_ms => 300000}),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
    
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    Self = self(),
    spawn_link(fun() ->
        %% Alternate between base and burst every 5 minutes
        PhaseMs = 300000,
        NumPhases = DurationMs div PhaseMs,
        lists:foreach(fun(I) ->
            Rate = case I rem 2 of
                0 -> BaseRate;
                1 -> BurstRate
            end,
            router_stress_soak_helper:generate_load(Rate, PhaseMs)
        end, lists:seq(1, NumPhases)),
        Self ! load_done
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive load_done -> ok after DurationMs + 5000 -> ok end,
    
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
