%% @doc Single-Fault Soak Tests
%%
%% Single fault type soak tests (2-4 hours each):
%% - Connect failures
%% - Publish errors
%% - ACK failures
%% - Network partitions
%%
%% @test_category soak, long_running, heavy
-module(router_soak_single_fault_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_connect_soak/1,
    test_publish_soak/1,
    test_ack_soak/1,
    test_network_partition_soak/1
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
    [{group, single_fault_soak}];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{single_fault_soak, [sequence], [
        test_connect_soak,
        test_publish_soak,
        test_ack_soak,
        test_network_partition_soak
    ]}].

init_per_suite(Config) -> router_stress_soak_helper:init_suite(Config).
end_per_suite(Config) -> router_stress_soak_helper:end_suite(Config).
init_per_testcase(_TC, Config) -> router_stress_soak_helper:init_testcase(Config).
end_per_testcase(_TC, Config) -> router_stress_soak_helper:end_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_connect_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: Connect Failures ==="),
    run_single_fault_soak(connect, {error, connection_refused}).

test_publish_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: Publish Errors ==="),
    run_single_fault_soak(publish, {error, timeout}).

test_ack_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: ACK Failures ==="),
    run_single_fault_soak(ack, {error, timeout}).

test_network_partition_soak(_Config) ->
    ct:comment("=== Single-Fault Soak: Network Partition ==="),
    run_single_fault_soak(connect, close_connection).

%% ============================================================================
%% HELPER
%% ============================================================================

run_single_fault_soak(Operation, Fault) ->
    DurationHours = router_stress_soak_helper:get_test_duration_hours(),
    DurationMs = round(DurationHours * 3600000),
    {BaseRate, _} = router_stress_soak_helper:get_load_profile(),
    
    ct:comment("Duration: ~p hours, Load: ~p msg/s", [DurationHours, BaseRate]),
    
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
    
    FaultPid = spawn_link(fun() ->
        router_stress_soak_helper:run_cyclic_fault(Operation, Fault, 300000, 300000, DurationMs),
        Self ! {fault_done, self()}
    end),
    
    StartTime = erlang:monotonic_time(millisecond),
    monitor_loop(ResourceMonitor, PerfMonitor, StartTime, DurationMs, 60000),
    
    receive {load_done, LoadPid} -> ok after DurationMs + 5000 -> ok end,
    receive {fault_done, FaultPid} -> ok after DurationMs + 5000 -> ok end,
    
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
