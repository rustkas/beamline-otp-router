%% @doc JetStream Extended Recovery: MaxDeliver Scenarios
%%
%% MaxDeliver exhaustion tests:
%% - Gradual accumulation
%% - Mass exhaustion
%% - Periodic consumer hang
%%
%% @test_category extended_recovery, heavy, long_running
-module(router_jetstream_maxdeliver_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    smoke_test/1,
    test_gradual_accumulation/1,
    test_mass_exhaustion/1,
    test_periodic_consumer_hang/1
]).

suite() -> [{timetrap, {hours, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [smoke_test, {group, maxdeliver_tests}];
        "full" -> [smoke_test];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [smoke_test, {group, maxdeliver_tests}];
        "full" -> [smoke_test];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [smoke_test, {group, maxdeliver_tests}];
        "full" -> [smoke_test];
        _ -> []
    end.
groups() ->
    [{maxdeliver_tests, [sequence], [
        test_gradual_accumulation,
        test_mass_exhaustion,
        test_periodic_consumer_hang
    ]}].

init_per_suite(Config) -> router_jetstream_ct_helpers:init_suite(Config).
end_per_suite(Config) -> router_jetstream_ct_helpers:end_suite(Config).
init_per_testcase(TC, Config) -> router_jetstream_ct_helpers:init_case(TC, Config).
end_per_testcase(TC, Config) -> router_jetstream_ct_helpers:end_case(TC, Config).

scale_duration(BaseMs) -> router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ============================================================================
%% SMOKE TEST
%% ============================================================================

smoke_test(_Config) ->
    ct:comment("=== MaxDeliver SUITE Smoke Test ==="),
    _ = router_jetstream_recovery_store:ensure(),
    _ = router_jetstream_recovery_helpers:speedup_factor(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_gradual_accumulation(_Config) ->
    ct:comment("=== Gradual MaxDeliver Accumulation ==="),
    
    BaselineDurationMs = scale_duration(5 * 60 * 1000),
    AccumulationDurationMs = scale_duration(10 * 60 * 1000),
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    ct:comment("Phase 2: Accumulation (30% ACK failure)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        case rand:uniform(100) =< 30 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    
    _ = router_jetstream_recovery_helpers:measure_baseline_throughput(AccumulationDurationMs),
    
    ct:comment("Phase 3: Recovery"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, ElapsedMs} -> ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_mass_exhaustion(_Config) ->
    ct:comment("=== Mass MaxDeliver Exhaustion ==="),
    
    BaselineDurationMs = scale_duration(3 * 60 * 1000),
    MassFailureDurationMs = scale_duration(5 * 60 * 1000),
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    ct:comment("Phase 2: Mass failure (100% ACK failure)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
    
    router_jetstream_recovery_helpers:process_message_batch(1000),
    timer:sleep(MassFailureDurationMs),
    
    ct:comment("Phase 3: Recovery"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, ElapsedMs} -> ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_periodic_consumer_hang(_Config) ->
    ct:comment("=== Periodic Consumer Hang ==="),
    
    BaselineDurationMs = scale_duration(5 * 60 * 1000),
    HangCycleDurationMs = scale_duration(5 * 60 * 1000),
    NumCycles = 3,
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    ct:comment("Phase 2: Cyclic hang/recovery"),
    lists:foreach(fun(Cycle) ->
        ct:comment("Cycle ~p/~p: Hang", [Cycle, NumCycles]),
        meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
            timer:sleep(5000),
            {error, timeout}
        end),
        timer:sleep(HangCycleDurationMs div 2),
        
        ct:comment("Cycle ~p/~p: Recover", [Cycle, NumCycles]),
        meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
        timer:sleep(HangCycleDurationMs div 2)
    end, lists:seq(1, NumCycles)),
    
    ct:comment("Phase 3: Final recovery"),
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
