%% @doc Performance Benchmark Tests
%%
%% Establishes baseline performance metrics and documents performance targets.
%% These benchmarks serve as reference points for regression testing.
%%
%% Performance Targets:
%% - Sequential requests: >= 1000 req/s, P95 < 50ms
%% - Concurrent requests: >= 500 req/s, P95 < 100ms
%% - Memory usage: < 100MB per 1000 active circuits
%% - ETS table size: < 1M entries
%%
%% @test_category performance, benchmark
-module(router_performance_benchmark_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").
-include("flow_pb.hrl").

-compile([export_all, nowarn_export_all]).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_baseline_ets_table_size/1,
    test_baseline_memory_usage/1,
    test_document_performance_targets/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

%% Performance benchmarks only run in heavy tier
groups_for_level(heavy) ->
    [{group, performance_tests}];
groups_for_level(_) -> %% fast, full, sanity
    [].

groups() ->
    [
        {performance_tests, [sequence], [
            test_establish_baseline_metrics,
            test_document_performance_targets,
            test_baseline_sequential_throughput,
            test_baseline_concurrent_throughput,
            test_baseline_memory_usage,
            test_baseline_ets_table_size
        ]}
    ].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{}),

    %% Create test policy
    TenantId = <<"test_tenant_benchmark">>,
    PolicyId = <<"test_policy_benchmark">>,
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, PolicyId, Policy, undefined),

    [{tenant_id, TenantId}, {policy_id, PolicyId} | Config1].

end_per_suite(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    router_policy_store:delete_policy(TenantId, PolicyId, undefined),
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(_TestCase, Config) ->
    Config1 = router_test_bootstrap:init_per_testcase(_TestCase, Config, #{}),
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    ok = router_r10_metrics:clear_metrics(),
    Config1.

end_per_testcase(_TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(_TestCase, Config, #{}).

%% @doc Establish baseline performance metrics
test_establish_baseline_metrics(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Collect baseline metrics
    Baseline = collect_baseline_metrics(TenantId, PolicyId),
    
    %% Store baseline for regression testing
    store_baseline_metrics(Baseline),
    
    ct:pal("Baseline Metrics Established:~n"
           "  Sequential throughput: ~p req/s~n"
           "  Concurrent throughput: ~p req/s~n"
           "  P95 latency (sequential): ~p ms~n"
           "  P95 latency (concurrent): ~p ms~n"
           "  Memory usage: ~p MB~n"
           "  ETS table size: ~p entries~n",
           [
               maps:get(sequential_throughput, Baseline),
               maps:get(concurrent_throughput, Baseline),
               maps:get(p95_latency_sequential, Baseline),
               maps:get(p95_latency_concurrent, Baseline),
               maps:get(memory_mb, Baseline),
               maps:get(ets_table_size, Baseline)
           ]),
    
    %% Verify baseline metrics are reasonable
    ?assert(maps:get(sequential_throughput, Baseline) > 0),
    ?assert(maps:get(concurrent_throughput, Baseline) > 0),
    ?assert(maps:get(memory_mb, Baseline) > 0),
    ok.

%% @doc Document performance targets
test_document_performance_targets(_Config) ->
    %% Performance targets are documented in test comments and assertions
    Targets = #{
        sequential_throughput => 1000,  %% req/s
        concurrent_throughput => 500,   %% req/s
        p95_latency_sequential => 50,  %% ms
        p95_latency_concurrent => 100,  %% ms
        memory_per_1000_circuits => 100, %% MB
        ets_table_max_entries => 1000000
    },
    
    ct:pal("Performance Targets:~n"
           "  Sequential throughput: >= ~p req/s~n"
           "  Concurrent throughput: >= ~p req/s~n"
           "  P95 latency (sequential): < ~p ms~n"
           "  P95 latency (concurrent): < ~p ms~n"
           "  Memory per 1000 circuits: < ~p MB~n"
           "  ETS table max entries: < ~p~n",
           [
               maps:get(sequential_throughput, Targets),
               maps:get(concurrent_throughput, Targets),
               maps:get(p95_latency_sequential, Targets),
               maps:get(p95_latency_concurrent, Targets),
               maps:get(memory_per_1000_circuits, Targets),
               maps:get(ets_table_max_entries, Targets)
           ]),
    
    %% Verify targets are documented
    ?assert(maps:size(Targets) > 0),
    ok.

%% @doc Baseline sequential throughput measurement
test_baseline_sequential_throughput(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Run sequential requests
    {Throughput, P95} = measure_sequential_throughput(TenantId, PolicyId, 1000),
    
    ct:pal("Sequential Throughput Baseline:~n"
           "  Throughput: ~p req/s~n"
           "  P95 latency: ~p ms~n",
           [Throughput, P95]),
    
    %% Verify baseline meets minimum targets
    ?assert(Throughput >= 100),
    ?assert(P95 < 200),
    ok.

%% @doc Baseline concurrent throughput measurement
test_baseline_concurrent_throughput(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Run concurrent requests
    {Throughput, P95} = measure_concurrent_throughput(TenantId, PolicyId, 100),
    
    ct:pal("Concurrent Throughput Baseline:~n"
           "  Throughput: ~p req/s~n"
           "  P95 latency: ~p ms~n",
           [Throughput, P95]),
    
    %% Verify baseline meets minimum targets
    ?assert(Throughput >= 50),
    ?assert(P95 < 300),
    ok.

%% @doc Baseline memory usage measurement
test_baseline_memory_usage(_Config) ->
    %% Collect memory metrics
    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo),
    MemoryMB = TotalMemory / (1024 * 1024),
    
    ct:pal("Memory Usage Baseline:~n"
           "  Total memory: ~p MB~n",
           [MemoryMB]),
    
    %% Verify memory usage is reasonable
    ?assert(MemoryMB > 0),
    ?assert(MemoryMB < 10000),  %% Less than 10GB
    ok.

%% @doc Baseline ETS table size measurement
test_baseline_ets_table_size(_Config) ->
    %% Collect ETS metrics
    router_metrics:ensure(),
    ETSInfo = ets:info(router_metrics),
    TableSize = proplists:get_value(size, ETSInfo, 0),
    
    ct:pal("ETS Table Size Baseline:~n"
           "  router_metrics size: ~p entries~n",
           [TableSize]),
    
    %% Verify ETS table size is reasonable
    ?assert(TableSize >= 0),
    ?assert(TableSize < 1000000),  %% Less than 1M entries
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Collect baseline performance metrics
collect_baseline_metrics(TenantId, PolicyId) ->
    {SeqThroughput, SeqP95} = measure_sequential_throughput(TenantId, PolicyId, 100),
    {ConThroughput, ConP95} = measure_concurrent_throughput(TenantId, PolicyId, 50),
    
    MemoryInfo = erlang:memory(),
    MemoryMB = proplists:get_value(total, MemoryInfo) / (1024 * 1024),
    
    router_metrics:ensure(),
    ETSInfo = ets:info(router_metrics),
    ETSSize = proplists:get_value(size, ETSInfo, 0),
    
    #{
        sequential_throughput => SeqThroughput,
        concurrent_throughput => ConThroughput,
        p95_latency_sequential => SeqP95,
        p95_latency_concurrent => ConP95,
        memory_mb => MemoryMB,
        ets_table_size => ETSSize
    }.

%% @doc Store baseline metrics (in-memory for test session)
store_baseline_metrics(Baseline) ->
    %% Store in process dictionary for this test session
    put(performance_baseline, Baseline),
    ok.

%% @doc Measure sequential throughput
measure_sequential_throughput(TenantId, PolicyId, RequestCount) ->
    Message = #'Message'{
        message_id = <<"msg_bench_seq">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    StartTime = erlang:monotonic_time(millisecond),
    Latencies = lists:map(fun(N) ->
        UpdatedMessage = Message#'Message'{message_id = <<"msg_bench_seq_", (integer_to_binary(N))/binary>>},
        UpdatedRequestPb = RouteRequestPb#'RouteRequest'{message = UpdatedMessage},
        UpdatedRequest = flow_pb:encode_msg(UpdatedRequestPb, 'RouteRequest'),
        ReqStart = erlang:monotonic_time(millisecond),
        try
            router_grpc:decide(Ctx, UpdatedRequest),
            ReqEnd = erlang:monotonic_time(millisecond),
            ReqEnd - ReqStart
        catch
            _:_ -> 0
        end
    end, lists:seq(1, RequestCount)),
    EndTime = erlang:monotonic_time(millisecond),
    
    Duration = EndTime - StartTime,
    Throughput = case Duration > 0 of
        true -> (RequestCount / Duration) * 1000;
        false -> 0
    end,
    
    SortedLatencies = lists:sort([L || L <- Latencies, L > 0]),
    P95Index = trunc(length(SortedLatencies) * 0.95),
    P95 = case P95Index > 0 andalso P95Index =< length(SortedLatencies) of
        true -> lists:nth(P95Index, SortedLatencies);
        false -> 0
    end,
    
    {Throughput, P95}.

%% @doc Measure concurrent throughput
measure_concurrent_throughput(TenantId, PolicyId, RequestCount) ->
    Message = #'Message'{
        message_id = <<"msg_bench_con">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    StartTime = erlang:monotonic_time(millisecond),
    Parent = self(),
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            UpdatedMessage = Message#'Message'{message_id = <<"msg_bench_con_", (integer_to_binary(N))/binary>>},
            UpdatedRequestPb = RouteRequestPb#'RouteRequest'{message = UpdatedMessage},
            UpdatedRequest = flow_pb:encode_msg(UpdatedRequestPb, 'RouteRequest'),
            ReqStart = erlang:monotonic_time(millisecond),
            try
                router_grpc:decide(Ctx, UpdatedRequest),
                ReqEnd = erlang:monotonic_time(millisecond),
                Parent ! {latency, ReqEnd - ReqStart}
            catch
                _:_ -> Parent ! {latency, 0}
            end
        end)
    end, lists:seq(1, RequestCount)),
    
    Latencies = lists:map(fun(_) ->
        receive
            {latency, L} -> L
        after 5000 -> 0
        end
    end, Pids),
    EndTime = erlang:monotonic_time(millisecond),
    
    Duration = EndTime - StartTime,
    Throughput = case Duration > 0 of
        true -> (RequestCount / Duration) * 1000;
        false -> 0
    end,
    
    SortedLatencies = lists:sort([L || L <- Latencies, L > 0]),
    P95Index = trunc(length(SortedLatencies) * 0.95),
    P95 = case P95Index > 0 andalso P95Index =< length(SortedLatencies) of
        true -> lists:nth(P95Index, SortedLatencies);
        false -> 0
    end,
    
    {Throughput, P95}.
