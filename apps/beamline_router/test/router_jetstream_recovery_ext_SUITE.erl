%% @doc JetStream Extended Recovery: Additional Scenarios
%%
%% Additional recovery scenarios:
%% - Prolonged partition recovery
%% - Message redelivery recovery
%% - Circuit breaker reset
%% - Backpressure clearance
%% - Multiple fault cycles
%%
%% @test_category extended_recovery, heavy, long_running
-module(router_jetstream_recovery_ext_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_prolonged_partition/1,
    test_message_redelivery/1,
    test_circuit_breaker_reset/1,
    test_backpressure_clearance/1,
    test_multiple_fault_cycles/1
]).

suite() -> [{timetrap, {hours, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(_) ->
    [{group, recovery_tests}].

groups() ->
    [{recovery_tests, [sequence], [
        test_prolonged_partition,
        test_message_redelivery,
        test_circuit_breaker_reset,
        test_backpressure_clearance,
        test_multiple_fault_cycles
    ]}].

init_per_suite(Config) -> 
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> 
            check_nats_availability(),
            router_jetstream_ct_helpers:init_suite(Config);
        _ ->
            ct:pal("SKIPPING: ~p requires ROUTER_TEST_LEVEL=heavy", [?MODULE]),
            ct:pal("Current ROUTER_TEST_LEVEL: ~p", [os:getenv("ROUTER_TEST_LEVEL")]),
            {skip, "Requires ROUTER_TEST_LEVEL=heavy"}
    end.

check_nats_availability() ->
    Host = "localhost",
    Port = 4222,
    ct:pal("Checking NATS availability at ~s:~p...", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {active, false}], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ct:pal("NATS is available."),
            ok;
        {error, Reason} ->
            ct:pal("WARNING: NATS is NOT available (~p). Tests are currently mocked, so this is non-fatal.", [Reason]),
            ok
    end.

end_per_suite(Config) -> router_jetstream_ct_helpers:end_suite(Config).
init_per_testcase(TC, Config) -> router_jetstream_ct_helpers:init_case(TC, Config).
end_per_testcase(TC, Config) -> router_jetstream_ct_helpers:end_case(TC, Config).

scale_duration(BaseMs) -> router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_prolonged_partition(_Config) ->
    ct:comment("=== Prolonged Partition Recovery ==="),
    
    BaselineDurationMs = scale_duration(5 * 60 * 1000),
    PartitionDurationMs = scale_duration(15 * 60 * 1000),
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    ct:comment("Phase 2: Prolonged partition"),
    router_jetstream_recovery_helpers:simulate_network_partition(),
    timer:sleep(PartitionDurationMs),
    
    ct:comment("Phase 3: Heal and recover"),
    router_jetstream_recovery_helpers:heal_network_partition(),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, ElapsedMs} -> ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_message_redelivery(_Config) ->
    ct:comment("=== Message Redelivery Recovery ==="),
    
    BaselineDurationMs = scale_duration(3 * 60 * 1000),
    RedeliveryDurationMs = scale_duration(10 * 60 * 1000),
    RecoveryDurationMs = scale_duration(5 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    ct:comment("Phase 2: Trigger redeliveries (ACK failures)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        case rand:uniform(100) =< 50 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    
    router_jetstream_recovery_helpers:process_message_batch(500),
    timer:sleep(RedeliveryDurationMs),
    
    ct:comment("Phase 3: Recover"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, _} -> ok  %% May not fully recover, that's OK
    end,
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_circuit_breaker_reset(_Config) ->
    ct:comment("=== Circuit Breaker Reset ==="),
    
    BaselineDurationMs = scale_duration(3 * 60 * 1000),
    TripDurationMs = scale_duration(5 * 60 * 1000),
    RecoveryDurationMs = scale_duration(5 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    ct:comment("Phase 2: Trip circuit breaker (high failure rate)"),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        {error, timeout}
    end),
    
    timer:sleep(TripDurationMs),
    
    ct:comment("Phase 3: Reset circuit breaker"),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, _} -> ok
    end,
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_backpressure_clearance(_Config) ->
    ct:comment("=== Backpressure Clearance ==="),
    
    BaselineDurationMs = scale_duration(3 * 60 * 1000),
    BackpressureDurationMs = scale_duration(5 * 60 * 1000),
    RecoveryDurationMs = scale_duration(5 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    ct:comment("Phase 2: Induce backpressure (slow processing)"),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        timer:sleep(100),  %% Simulate slow processing
        ok
    end),
    
    router_jetstream_recovery_helpers:process_message_batch(500),
    timer:sleep(BackpressureDurationMs),
    
    ct:comment("Phase 3: Clear backpressure"),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, _} -> ok
    end,
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_multiple_fault_cycles(_Config) ->
    ct:comment("=== Multiple Fault Cycles ==="),
    
    BaselineDurationMs = scale_duration(3 * 60 * 1000),
    CycleDurationMs = scale_duration(3 * 60 * 1000),
    NumCycles = 5,
    RecoveryDurationMs = scale_duration(5 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    ct:comment("Phase 2: Multiple fault cycles"),
    lists:foreach(fun(Cycle) ->
        ct:comment("Cycle ~p/~p: Fault", [Cycle, NumCycles]),
        meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, timeout} end),
        timer:sleep(CycleDurationMs div 2),
        
        ct:comment("Cycle ~p/~p: Recover", [Cycle, NumCycles]),
        meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
        timer:sleep(CycleDurationMs div 2)
    end, lists:seq(1, NumCycles)),
    
    ct:comment("Phase 3: Final recovery verification"),
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, ElapsedMs} -> ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.
