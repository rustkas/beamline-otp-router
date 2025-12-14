%% @doc JetStream Soak: Performance Scenarios
%%
%% - Long running stability (4+ hours)
%% - Recovery time measurement
%%
%% @test_category soak, nightly, heavy, performance
-module(router_jetstream_soak_perf_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_long_running_stability/1,
    test_recovery_time_measurement/1
]).

suite() -> [{timetrap, {hours, 6}}].

soak_enabled() -> os:getenv("RUN_JETSTREAM_SOAK") =:= "1".

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" ->
            case soak_enabled() of
                true -> [{group, performance_scenarios}];
                false -> []
            end;
        _ -> []
    end.

groups() ->
    [{performance_scenarios, [sequence], [
        test_long_running_stability,
        test_recovery_time_measurement
    ]}].

init_per_suite(Config) -> router_jetstream_ct_helpers:init_suite(Config).
end_per_suite(Config) -> router_jetstream_ct_helpers:end_suite(Config).
init_per_testcase(TC, Config) -> router_jetstream_ct_helpers:init_case(TC, Config).
end_per_testcase(TC, Config) -> router_jetstream_ct_helpers:end_case(TC, Config).

scale_duration(BaseMs) -> router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_long_running_stability(_Config) ->
    ct:comment("=== Long Running Stability (4+ hours) ==="),
    
    TotalDurationMs = scale_duration(4 * 60 * 60 * 1000),
    CheckIntervalMs = scale_duration(15 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(60000)),
    
    StartTime = erlang:monotonic_time(millisecond),
    stability_check_loop(StartTime, TotalDurationMs, CheckIntervalMs, BaselineThroughput, InitialResources),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_recovery_time_measurement(_Config) ->
    ct:comment("=== Recovery Time Measurement ==="),
    
    FaultTypes = [jetstream_restart, network_partition, connection_errors],
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(60000)),
    
    RecoveryTimes = lists:map(fun(FaultType) ->
        ct:comment("Testing recovery from: ~p", [FaultType]),
        
        %% Apply fault
        apply_fault(FaultType),
        timer:sleep(scale_duration(60000)),
        
        %% Clear fault and measure recovery
        FaultClearTime = erlang:monotonic_time(millisecond),
        clear_fault(FaultType),
        
        RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
            fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(5000)) end,
            BaselineThroughput,
            scale_duration(5 * 60 * 1000)
        ),
        
        RecoveryTimeMs = case RecoveryResult of
            {ok, Time} -> Time;
            {timeout, _} -> erlang:monotonic_time(millisecond) - FaultClearTime
        end,
        
        ct:comment("~p recovery time: ~p ms", [FaultType, RecoveryTimeMs]),
        {FaultType, RecoveryTimeMs}
    end, FaultTypes),
    
    ct:comment("Recovery times: ~p", [RecoveryTimes]),
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

stability_check_loop(StartTime, TotalDurationMs, CheckIntervalMs, BaselineThroughput, InitialResources) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    Elapsed = CurrentTime - StartTime,
    
    case Elapsed >= TotalDurationMs of
        true -> ok;
        false ->
            ct:comment("Stability check at ~p/~p ms", [Elapsed, TotalDurationMs]),
            
            CurrentThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(
                router_jetstream_recovery_helpers:scale_duration(10000)
            ),
            ?assert(CurrentThroughput >= BaselineThroughput * 0.7),
            
            CurrentResources = router_jetstream_recovery_helpers:track_resources(),
            router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, CurrentResources),
            
            timer:sleep(CheckIntervalMs),
            stability_check_loop(StartTime, TotalDurationMs, CheckIntervalMs, BaselineThroughput, InitialResources)
    end.

apply_fault(jetstream_restart) ->
    router_nats_fault_injection:enable_fault(subscribe, {error, connection_lost});
apply_fault(network_partition) ->
    router_jetstream_recovery_helpers:simulate_network_partition();
apply_fault(connection_errors) ->
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}).

clear_fault(jetstream_restart) ->
    router_nats_fault_injection:disable_fault(subscribe);
clear_fault(network_partition) ->
    router_jetstream_recovery_helpers:heal_network_partition();
clear_fault(connection_errors) ->
    router_nats_fault_injection:disable_fault(connect).
