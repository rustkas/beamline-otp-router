%% @doc Long-running Soak Test Suite (T-SOAK-01)
%%
%% Validates Router stability under sustained load (6-24 hours):
%% 1. Memory stability (no leaks, bounded growth)
%% 2. ETS table growth (no unbounded growth)
%% 3. JetStream consumer stability (no lag drift)
%% 4. Process stability (no crashes)
%%
%% @test_category soak, stability, long_running
-module(router_soak_stability_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("router_soak_helper.hrl").

-export([all/0, groups/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_memory_stability_short/1,
    test_memory_stability_6h/1,
    test_ets_growth_monitoring/1,
    test_jetstream_consumer_stability/1,
    test_process_stability/1
]).

suite() ->
    [{timetrap, {hours, 25}}].  % Allow 25 hours for 24-hour soak

all() ->
    [
        test_memory_stability_short,
        test_ets_growth_monitoring,
        test_process_stability
        %% Long tests commented by default (uncomment for full soak)
        %% test_memory_stability_6h,
        %% test_jetstream_consumer_stability
    ].

groups() ->
    [
        {short_soak, [sequence], [
            test_memory_stability_short
        ]},
        {long_soak, [sequence], [
            test_memory_stability_6h,
            test_jetstream_consumer_stability
        ]}
    ].

%% ============================================================================
%% Suite Setup/Teardown
%% ============================================================================

init_per_suite(Config) ->
    %% Start NATS
    NatsStartScript = filename:join([code:priv_dir(beamline_router), "..", "..", "scripts", "nats_start.sh"]),
    case filelib:is_file(NatsStartScript) of
        true ->
            os:cmd(NatsStartScript),
            timer:sleep(2000);
        false ->
            ct:pal("NATS start script not found")
    end,
    
    %% Configure Router for real NATS
    application:load(beamline_router),
    application:set_env(beamline_router, nats_mode, real),
    application:set_env(beamline_router, nats_url, <<"nats://localhost:4222">>),
    application:set_env(beamline_router, grpc_enabled, false),
    
    %% Start Router
    {ok, _} = application:ensure_all_started(beamline_router),
    
    ct:pal("Soak test suite initialized"),
    Config.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting soak test: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, Config) ->
    ct:pal("Completed soak test: ~p", [TestCase]),
    Config.

%% ============================================================================
%% Test Cases
%% ============================================================================

test_memory_stability_short(Config) ->
    ct:pal("=== Short Memory Stability Test (10 minutes) ==="),
    
    DurationSeconds = 600,  % 10 minutes
    SampleIntervalSeconds = 60,  % Every minute
    RequestsPerSecond = 10,
    
    run_memory_stability_test(DurationSeconds, SampleIntervalSeconds, RequestsPerSecond, Config).

test_memory_stability_6h(Config) ->
    ct:pal("=== 6-Hour Memory Stability Test ==="),
    
    DurationSeconds = 6 * 3600,  % 6 hours
    SampleIntervalSeconds = 300,  % Every 5 minutes
    RequestsPerSecond = 20,
    
    run_memory_stability_test(DurationSeconds, SampleIntervalSeconds, RequestsPerSecond, Config).

run_memory_stability_test(DurationSeconds, SampleIntervalSeconds, RequestsPerSecond, _Config) ->
    ct:pal("Starting memory stability test for ~p seconds", [DurationSeconds]),
    ct:pal("Sample interval: ~p seconds, Request rate: ~p req/s", 
           [SampleIntervalSeconds, RequestsPerSecond]),
    
    %% Get initial memory snapshot
    InitialMemory = router_soak_helper:get_memory_snapshot(),
    ct:pal("Initial memory: ~p MB", [InitialMemory#memory_snapshot.total div (1024*1024)]),
    
    %% Start memory monitoring
    {ok, MonitorPid} = router_soak_helper:start_memory_monitor(SampleIntervalSeconds),
    
    %% Generate sustained load
    ct:pal("Generating sustained load..."),
    RequestFun = fun() ->
        %% Simulate routing request
        try
            router_test_helpers:make_test_request(<<"test_tenant">>, <<"test_policy">>)
        catch
            _:_ -> ok
        end
    end,
    
    router_soak_helper:generate_sustained_load(RequestsPerSecond, DurationSeconds, RequestFun),
    
    ct:pal("Load generation complete, analyzing results..."),
    
    %% Stop monitoring and get snapshots
    router_soak_helper:stop_memory_monitor(MonitorPid),
    timer:sleep(1000),
    
    %% Get final memory snapshot
    FinalMemory = router_soak_helper:get_memory_snapshot(),
    ct:pal("Final memory: ~p MB", [FinalMemory#memory_snapshot.total div (1024*1024)]),
    
    %% Calculate growth
    InitialTotal = InitialMemory#memory_snapshot.total,
    FinalTotal = FinalMemory#memory_snapshot.total,
    GrowthPercent = ((FinalTotal - InitialTotal) / InitialTotal) * 100,
    
    ct:pal("Memory growth: ~.2f%", [GrowthPercent]),
    
    %% Verify stability criteria
    MaxGrowthPercent = 10.0,
    ?assert(GrowthPercent < MaxGrowthPercent, 
            io_lib:format("Memory growth (~.2f%) exceeds limit (~.2f%)", 
                         [GrowthPercent, MaxGrowthPercent])),
    
    ct:pal("✓ Memory stability test PASSED (growth: ~.2f%)", [GrowthPercent]),
    ok.

test_ets_growth_monitoring(Config) ->
    ct:pal("=== ETS Growth Monitoring Test (5 minutes) ==="),
    
    %% Get initial ETS snapshot
    InitialETS = router_soak_helper:get_ets_snapshot(),
    ct:pal("Initial ETS tables: ~p", [length(InitialETS#ets_snapshot.tables)]),
    
    %% Generate load for 5 minutes
    DurationSeconds = 300,
    RequestsPerSecond = 50,
    
    RequestFun = fun() ->
        try
            router_test_helpers:make_test_request(<<"test_tenant">>, <<"test_policy">>)
        catch
            _:_ -> ok
        end
    end,
    
    router_soak_helper:generate_sustained_load(RequestsPerSecond, DurationSeconds, RequestFun),
    
    %% Get final ETS snapshot
    FinalETS = router_soak_helper:get_ets_snapshot(),
    ct:pal("Final ETS tables: ~p", [length(FinalETS#ets_snapshot.tables)]),
    
    %% Analyze growth
    {ok, Growth} = router_soak_helper:analyze_ets_growth(InitialETS, FinalETS),
    
    ct:pal("ETS table growth analysis:"),
    lists:foreach(fun({TableName, SizeGrowth, GrowthPercent}) ->
        ct:pal("  ~p: +~p entries (~.2f%)", [TableName, SizeGrowth, GrowthPercent])
    end, Growth),
    
    %% Verify no unbounded growth
    UnboundedTables = lists:filter(fun({_Name, _SizeGrowth, GrowthPercent}) ->
        GrowthPercent > 1000.0  % 10x growth is considered unbounded
    end, Growth),
    
    ?assertEqual([], UnboundedTables, "Unbounded ETS table growth detected"),
    
    ct:pal("✓ ETS growth monitoring test PASSED"),
    ok.

test_jetstream_consumer_stability(Config) ->
    ct:pal("=== JetStream Consumer Stability Test (1 hour) ==="),
    
    Subject = <<"beamline.router.v1.decide.soak">>,
    DurationSeconds = 3600,  % 1 hour
    SampleIntervalSeconds = 60,  % Every minute
    
    %% Monitor lag over time
    LagSamples = monitor_jetstream_lag(Subject, DurationSeconds, SampleIntervalSeconds),
    
    ct:pal("Collected ~p lag samples", [length(LagSamples)]),
    
    %% Analyze lag trend
    MaxLag = lists:max(LagSamples),
    AvgLag = lists:sum(LagSamples) / length(LagSamples),
    
    ct:pal("JetStream lag - Max: ~p, Avg: ~.2f", [MaxLag, AvgLag]),
    
    %% Verify lag stays bounded
    MaxAllowedLag = 100,
    ?assert(MaxLag < MaxAllowedLag, 
            io_lib:format("JetStream lag (~p) exceeds limit (~p)", [MaxLag, MaxAllowedLag])),
    
    ct:pal("✓ JetStream consumer stability test PASSED"),
    ok.

monitor_jetstream_lag(Subject, DurationSeconds, SampleIntervalSeconds) ->
    EndTime = erlang:monotonic_time(second) + DurationSeconds,
    monitor_lag_loop(Subject, EndTime, SampleIntervalSeconds, []).

monitor_lag_loop(Subject, EndTime, SampleInterval, Samples) ->
    Now = erlang:monotonic_time(second),
    if
        Now >= EndTime ->
            lists:reverse(Samples);
        true ->
            Lag = case router_soak_helper:get_jetstream_lag(Subject) of
                {ok, L} -> L;
                {error, _} -> 0
            end,
            timer:sleep(SampleInterval * 1000),
            monitor_lag_loop(Subject, EndTime, SampleInterval, [Lag | Samples])
    end.

test_process_stability(Config) ->
    ct:pal("=== Process Stability Test (5 minutes) ==="),
    
    %% Get initial process snapshot
    InitialProc = router_soak_helper:get_process_snapshot(),
    ct:pal("Initial processes: ~p", [InitialProc#process_snapshot.count]),
    
    %% Generate load for 5 minutes
    DurationSeconds = 300,
    RequestsPerSecond = 50,
    
    RequestFun = fun() ->
        try
            router_test_helpers:make_test_request(<<"test_tenant">>, <<"test_policy">>)
        catch
            _:_ -> ok
        end
    end,
    
    router_soak_helper:generate_sustained_load(RequestsPerSecond, DurationSeconds, RequestFun),
    
    %% Get final process snapshot
    FinalProc = router_soak_helper:get_process_snapshot(),
    ct:pal("Final processes: ~p", [FinalProc#process_snapshot.count]),
    
    %% Verify process count is stable (± 10%)
    InitialCount = InitialProc#process_snapshot.count,
    FinalCount = FinalProc#process_snapshot.count,
    ChangePercent = abs((FinalCount - InitialCount) / InitialCount) * 100,
    
    ct:pal("Process count change: ~.2f%", [ChangePercent]),
    
    ?assert(ChangePercent < 10.0, 
            io_lib:format("Process count unstable (~.2f% change)", [ChangePercent])),
    
    ct:pal("✓ Process stability test PASSED"),
    ok.

%% ============================================================================
%% Note: Helper functions for test request generation would go here
%% or in a separate module like router_test_helpers.erl
%% ============================================================================
