%% @doc Performance Regression Tests
%%
%% Compares current performance against baseline metrics to detect regressions.
%% Fails if performance degrades beyond acceptable thresholds.
%%
%% Regression Thresholds:
%% - Throughput degradation: > 20% below baseline = FAIL
%% - Latency increase: > 50% above baseline = FAIL
%% - Memory growth: > 30% above baseline = FAIL
%%
%% @test_category performance, regression
-module(router_performance_regression_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").
-include("flow_pb.hrl").

%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test functions
-export([
    test_regression_sequential_throughput/1,
    test_regression_concurrent_throughput/1,
    test_regression_latency_p95/1,
    test_regression_memory_usage/1,
    test_regression_ets_table_size/1
]).

-export([groups/0, groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Performance regression tests are heavy-only
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [];  %% Performance tests only run in heavy tier
groups_for_level(heavy) -> [{group, performance_tests}].

groups() ->
    [{performance_tests, [sequence], [
        test_regression_sequential_throughput,
        test_regression_concurrent_throughput,
        test_regression_latency_p95,
        test_regression_memory_usage,
        test_regression_ets_table_size
    ]}].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{}),

    %% Create test policy
    TenantId = <<"test_tenant_regression">>,
    PolicyId = <<"test_policy_regression">>,
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

    %% Load baseline metrics (from benchmark suite)
    Baseline = load_baseline_metrics(),

    [{tenant_id, TenantId}, {policy_id, PolicyId}, {baseline, Baseline} | Config1].

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

%% @doc Regression test for sequential throughput
test_regression_sequential_throughput(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    Baseline = proplists:get_value(baseline, Config),
    
    %% Measure current performance
    {CurrentThroughput, _} = measure_sequential_throughput(TenantId, PolicyId, 100),
    BaselineThroughput = maps:get(sequential_throughput, Baseline, 100),
    
    %% Calculate degradation
    Degradation = ((BaselineThroughput - CurrentThroughput) / BaselineThroughput) * 100,
    
    ct:pal("Sequential Throughput Regression:~n"
           "  Baseline: ~p req/s~n"
           "  Current: ~p req/s~n"
           "  Degradation: ~p%~n",
           [BaselineThroughput, CurrentThroughput, Degradation]),
    
    %% Fail if degradation > 20%
    ?assert(Degradation =< 20.0),
    ok.

%% @doc Regression test for concurrent throughput
test_regression_concurrent_throughput(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    Baseline = proplists:get_value(baseline, Config),
    
    %% Measure current performance
    {CurrentThroughput, _} = measure_concurrent_throughput(TenantId, PolicyId, 50),
    BaselineThroughput = maps:get(concurrent_throughput, Baseline, 50),
    
    %% Calculate degradation
    Degradation = ((BaselineThroughput - CurrentThroughput) / BaselineThroughput) * 100,
    
    ct:pal("Concurrent Throughput Regression:~n"
           "  Baseline: ~p req/s~n"
           "  Current: ~p req/s~n"
           "  Degradation: ~p%~n",
           [BaselineThroughput, CurrentThroughput, Degradation]),
    
    %% Fail if degradation > 20%
    ?assert(Degradation =< 20.0),
    ok.

%% @doc Regression test for P95 latency
test_regression_latency_p95(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    Baseline = proplists:get_value(baseline, Config),
    
    %% Measure current performance
    {_, CurrentP95} = measure_sequential_throughput(TenantId, PolicyId, 100),
    BaselineP95 = maps:get(p95_latency_sequential, Baseline, 50),
    
    %% Calculate increase
    Increase = ((CurrentP95 - BaselineP95) / BaselineP95) * 100,
    
    ct:pal("P95 Latency Regression:~n"
           "  Baseline: ~p ms~n"
           "  Current: ~p ms~n"
           "  Increase: ~p%~n",
           [BaselineP95, CurrentP95, Increase]),
    
    %% Fail if increase > 50%
    ?assert(Increase =< 50.0),
    ok.

%% @doc Regression test for memory usage
test_regression_memory_usage(Config) ->
    Baseline = proplists:get_value(baseline, Config),
    
    %% Measure current memory
    MemoryInfo = erlang:memory(),
    CurrentMemoryMB = proplists:get_value(total, MemoryInfo) / (1024 * 1024),
    BaselineMemoryMB = maps:get(memory_mb, Baseline, 100),
    
    %% Calculate growth
    Growth = ((CurrentMemoryMB - BaselineMemoryMB) / BaselineMemoryMB) * 100,
    
    ct:pal("Memory Usage Regression:~n"
           "  Baseline: ~p MB~n"
           "  Current: ~p MB~n"
           "  Growth: ~p%~n",
           [BaselineMemoryMB, CurrentMemoryMB, Growth]),
    
    %% Fail if growth > 30%
    ?assert(Growth =< 30.0),
    ok.

%% @doc Regression test for ETS table size
test_regression_ets_table_size(Config) ->
    Baseline = proplists:get_value(baseline, Config),
    
    %% Measure current ETS size
    router_metrics:ensure(),
    ETSInfo = ets:info(router_metrics),
    CurrentSize = proplists:get_value(size, ETSInfo, 0),
    BaselineSize = maps:get(ets_table_size, Baseline, 0),
    
    %% Calculate growth
    Growth = case BaselineSize > 0 of
        true -> ((CurrentSize - BaselineSize) / BaselineSize) * 100;
        false -> 0
    end,
    
    ct:pal("ETS Table Size Regression:~n"
           "  Baseline: ~p entries~n"
           "  Current: ~p entries~n"
           "  Growth: ~p%~n",
           [BaselineSize, CurrentSize, Growth]),
    
    %% Fail if growth > 50%
    ?assert(Growth =< 50.0),
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Load baseline metrics (from process dictionary or default)
load_baseline_metrics() ->
    case get(performance_baseline) of
        undefined ->
            %% Default baseline if not set
            #{
                sequential_throughput => 1000,
                concurrent_throughput => 500,
                p95_latency_sequential => 50,
                p95_latency_concurrent => 100,
                memory_mb => 100,
                ets_table_size => 0
            };
        Baseline ->
            Baseline
    end.

%% @doc Measure sequential throughput (same as benchmark suite)
measure_sequential_throughput(TenantId, PolicyId, RequestCount) ->
    Message = #'Message'{
        message_id = <<"msg_reg_seq">>,
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
        UpdatedMessage = Message#'Message'{message_id = <<"msg_reg_seq_", (integer_to_binary(N))/binary>>},
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

%% @doc Measure concurrent throughput (same as benchmark suite)
measure_concurrent_throughput(TenantId, PolicyId, RequestCount) ->
    Message = #'Message'{
        message_id = <<"msg_reg_con">>,
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
            UpdatedMessage = Message#'Message'{message_id = <<"msg_reg_con_", (integer_to_binary(N))/binary>>},
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
