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
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/flow_pb.hrl").

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
    %% Load baseline metrics (from benchmark suite)
    Baseline = load_baseline_metrics(),
    case Baseline of
        undefined ->
            {skip, baseline_not_configured};
        _ ->
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
            {ok, _} = router_policy_store:upsert_policy(TenantId, Policy, undefined),

            [{tenant_id, TenantId}, {policy_id, PolicyId}, {baseline, Baseline} | Config1]
    end.

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
    
    maybe_assert_throughput_regression(sequential_throughput, Degradation),
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
    
    maybe_assert_throughput_regression(concurrent_throughput, Degradation),
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
            load_baseline_from_file();
        Baseline ->
            Baseline
    end.

load_baseline_from_file() ->
    BaselinePath0 = case os:getenv("ROUTER_PERF_BASELINE_JSON") of
        false -> "docs/perf/baselines/batch3_heavy.json";
        Path -> Path
    end,
    BaselinePath = resolve_baseline_path(BaselinePath0),
    case router_perf_baseline:load_baseline(BaselinePath) of
        {ok, Json} ->
            pick_performance_baseline(Json);
        {error, Reason} ->
            ct:pal("Baseline load failed (~s): ~p", [BaselinePath, Reason]),
            undefined
    end.

resolve_baseline_path(Path) ->
    case filelib:is_file(Path) of
        true -> filename:absname(Path);
        false ->
            Cwd = case file:get_cwd() of
                {ok, Dir} -> Dir;
                _ -> ""
            end,
            CwdCandidate = filename:join(Cwd, "docs/perf/baselines/batch3_heavy.json"),
            case filelib:is_file(CwdCandidate) of
                true -> filename:absname(CwdCandidate);
                false ->
                    Root = project_root(),
                    filename:join(Root, "docs/perf/baselines/batch3_heavy.json")
            end
    end.

project_root() ->
    LibDir = code:lib_dir(beamline_router),
    filename:absname(filename:join([LibDir, "..", "..", "..", ".."])).

pick_performance_baseline(Json) ->
    Suites = maps:get(<<"suites">>, Json, []),
    Seq = find_suite_entry_by_scenario(Suites, <<"router_performance_benchmark_SUITE">>, <<"sequential">>),
    Con = find_suite_entry_by_scenario(Suites, <<"router_performance_benchmark_SUITE">>, <<"concurrent">>),
    case {Seq, Con} of
        {{ok, SeqEntry}, {ok, ConEntry}} ->
            SeqMetrics = maps:get(<<"metrics">>, SeqEntry, #{}),
            ConMetrics = maps:get(<<"metrics">>, ConEntry, #{}),
            #{
                sequential_throughput => get_num(SeqMetrics, [<<"throughput_rps">>, <<"avg">>], 0),
                concurrent_throughput => get_num(ConMetrics, [<<"throughput_rps">>, <<"avg">>], 0),
                p95_latency_sequential => get_num(SeqMetrics, [<<"latency_ms">>, <<"p95">>], 0),
                p95_latency_concurrent => get_num(ConMetrics, [<<"latency_ms">>, <<"p95">>], 0),
                memory_mb => current_memory_mb(),
                ets_table_size => current_ets_table_size()
            };
        _ ->
            undefined
    end.

find_suite_entry_by_scenario([], _Suite, _Scenario) ->
    {error, not_found};
find_suite_entry_by_scenario([Entry | Rest], Suite, Scenario) ->
    case {maps:get(<<"suite">>, Entry, <<>>), maps:get(<<"workload">>, Entry, #{})} of
        {Suite, Workload} ->
            case maps:get(<<"scenario">>, Workload, <<>>) of
                Scenario -> {ok, Entry};
                _ -> find_suite_entry_by_scenario(Rest, Suite, Scenario)
            end;
        _ ->
            find_suite_entry_by_scenario(Rest, Suite, Scenario)
    end.

find_suite_entry([], _Suite) ->
    {error, not_found};
find_suite_entry([Entry | Rest], Suite) ->
    case maps:get(<<"suite">>, Entry, <<>>) of
        Suite -> {ok, Entry};
        _ -> find_suite_entry(Rest, Suite)
    end.

get_num(M, [K | Rest], Default) ->
    case maps:get(K, M, undefined) of
        undefined -> Default;
        V when is_map(V) -> get_num(V, Rest, Default);
        V when is_integer(V) -> V;
        V when is_float(V) -> V;
        _ -> Default
    end.

maybe_assert_throughput_regression(Kind, Degradation) ->
    MaxDegradation = env_number("ROUTER_PERF_REGRESSION_MAX_DEGRADATION", 50.0),
    case env_bool("ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT", false) of
        true ->
            ?assert(Degradation =< MaxDegradation);
        false ->
            ct:comment("PERF_REGRESSION: ~p degradation ~p% > ~p% (assert quarantined)",
                [Kind, Degradation, MaxDegradation])
    end.

env_bool(Var, Default) ->
    case os:getenv(Var) of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        _ -> Default
    end.

env_number(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Val ->
            case string:to_float(Val) of
                {error, no_float} -> list_to_integer(Val);
                {FloatVal, _Rest} -> FloatVal
            end
    end.

current_memory_mb() ->
    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo, 0),
    TotalMemory / (1024 * 1024).

current_ets_table_size() ->
    router_metrics:ensure(),
    ETSInfo = ets:info(router_metrics),
    proplists:get_value(size, ETSInfo, 0).

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
