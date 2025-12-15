%% @doc Extensions Pipeline Load: Baseline Tests
%%
%% Baseline load tests:
%% - Registry lookup performance
%% - No errors baseline
%% - Latency distribution
%% - Throughput measurement
%%
%% @test_category load, heavy, extensions
-module(router_ext_load_baseline_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-record(stats, {p50_ms, p95_ms, p99_ms, max_ms, avg_ms, p50_us, p95_us, p99_us, max_us, avg_us}).

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_registry_lookup_performance/1,
    test_no_errors/1,
    test_latency_distribution/1,
    test_throughput_measurement/1
]).

suite() -> [{timetrap, {minutes, 15}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, baseline_tests}];
        _ -> []
    end.

groups() ->
    [{baseline_tests, [sequence], [
        test_registry_lookup_performance,
        test_no_errors,
        test_latency_distribution,
        test_throughput_measurement
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, extension_registry, [{source, fixtures}]),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = router_mock_helpers:setup_router_nats_mock(),
    MetricsTable = router_test_init:ensure_ets_table(ext_metrics, [set, public]),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> test_helpers:wait_for_app_start(router_extension_registry, 1000), [{metrics_table, MetricsTable} | Config];
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TC, Config) ->
    case proplists:get_value(metrics_table, Config) of
        T when T =/= undefined -> clear_metrics_table(T);
        _ -> ok
    end,
    Config.

end_per_testcase(_TC, Config) ->
    case proplists:get_value(metrics_table, Config) of
        T when T =/= undefined -> clear_metrics_table(T);
        _ -> ok
    end,
    Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_registry_lookup_performance(_Config) ->
    ct:comment("=== Registry Lookup Performance ==="),
    NumLookups = 10000,
    
    StartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        _ = router_extension_registry:lookup(<<"normalize_text">>)
    end, lists:seq(1, NumLookups)),
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTimeUs = EndTime - StartTime,
    AvgTimeUs = TotalTimeUs / NumLookups,
    
    ct:comment("~p lookups in ~p us, avg ~p us/lookup", [NumLookups, TotalTimeUs, AvgTimeUs]),
    
    ?assert(AvgTimeUs < 100, "Lookup should be < 100us avg"),
    ok.

test_no_errors(_Config) ->
    ct:comment("=== Baseline No Errors ==="),
    NumRequests = 1000,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        timer:sleep(10),
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}]),
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    Stats = calculate_statistics(Latencies),
    
    ct:comment("Requests: ~p, Success: ~p, Errors: ~p", [NumRequests, SuccessCount, ErrorCount]),
    ct:comment("P50: ~p ms, P95: ~p ms, P99: ~p ms", [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms]),
    
    ?assertEqual(NumRequests, SuccessCount),
    ?assertEqual(0, ErrorCount),
    ?assert(Stats#stats.p95_ms < 80),
    ok.

test_latency_distribution(_Config) ->
    ct:comment("=== Latency Distribution ==="),
    NumRequests = 500,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        Jitter = rand:uniform(20),
        timer:sleep(10 + Jitter),
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}]),
    {Latencies, _SuccessCount, _ErrorCount} = execute_load_test(NumRequests, Policy),
    Stats = calculate_statistics(Latencies),
    
    ct:comment("P50: ~p ms, P95: ~p ms, P99: ~p ms, Max: ~p ms", 
               [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms, Stats#stats.max_ms]),
    
    ?assert(Stats#stats.p50_ms < Stats#stats.p95_ms),
    ?assert(Stats#stats.p95_ms < Stats#stats.p99_ms),
    ok.

test_throughput_measurement(_Config) ->
    ct:comment("=== Throughput Measurement ==="),
    DurationMs = 10000,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        timer:sleep(5),
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}]),
    
    StartTime = erlang:monotonic_time(millisecond),
    {_, SuccessCount, _} = execute_timed_load_test(DurationMs, Policy),
    EndTime = erlang:monotonic_time(millisecond),
    
    ActualDurationMs = EndTime - StartTime,
    Throughput = SuccessCount / (ActualDurationMs / 1000),
    
    ct:comment("Duration: ~p ms, Requests: ~p, Throughput: ~p req/s", [ActualDurationMs, SuccessCount, Throughput]),
    
    ?assert(Throughput > 10, "Should achieve > 10 req/s"),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

create_policy(Extensions) ->
    Pre = proplists:get_value(pre, Extensions, []),
    PreItems = lists:map(fun({id, Id, mode, Mode}) -> #{id => Id, mode => Mode, config => #{}} end, Pre),
    #policy{tenant_id = <<"test">>, policy_id = <<"test">>, version = <<"1.0">>, pre = PreItems,
            validators = [], post = [], weights = #{<<"openai:gpt-4">> => 1.0}, metadata = #{}}.

execute_load_test(NumRequests, Policy) ->
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    Results = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        Req = #route_request{message = #{<<"message_id">> => <<"msg">>, <<"payload">> => <<"test">>},
                             policy_id = Policy#policy.policy_id, context = #{}},
        Res = router_decider:decide(Req, Policy, Context),
        End = erlang:monotonic_time(microsecond),
        timer:sleep(5),
        {Res, End - Start}
    end, lists:seq(1, NumRequests)),
    Latencies = [L || {_, L} <- Results],
    SuccessCount = length([ok || {{ok, _}, _} <- Results]),
    ErrorCount = NumRequests - SuccessCount,
    {Latencies, SuccessCount, ErrorCount}.

execute_timed_load_test(DurationMs, Policy) ->
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    StartTime = erlang:monotonic_time(millisecond),
    timed_loop(StartTime, DurationMs, Policy, Context, [], 0, 0).

timed_loop(StartTime, DurationMs, Policy, Context, Latencies, Success, Errors) ->
    Current = erlang:monotonic_time(millisecond),
    case Current - StartTime >= DurationMs of
        true -> {Latencies, Success, Errors};
        false ->
            Start = erlang:monotonic_time(microsecond),
            Req = #route_request{message = #{<<"message_id">> => <<"msg">>, <<"payload">> => <<"test">>},
                                 policy_id = Policy#policy.policy_id, context = #{}},
            Res = router_decider:decide(Req, Policy, Context),
            End = erlang:monotonic_time(microsecond),
            {NewSuccess, NewErrors} = case Res of
                {ok, _} -> {Success + 1, Errors};
                _ -> {Success, Errors + 1}
            end,
            timer:sleep(5),
            timed_loop(StartTime, DurationMs, Policy, Context, [End - Start | Latencies], NewSuccess, NewErrors)
    end.

calculate_statistics(Latencies) when length(Latencies) > 0 ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    P50 = lists:nth(max(1, trunc(Len * 0.5)), Sorted),
    P95 = lists:nth(max(1, trunc(Len * 0.95)), Sorted),
    P99 = lists:nth(max(1, trunc(Len * 0.99)), Sorted),
    Max = lists:max(Sorted),
    Avg = lists:sum(Sorted) / Len,
    #stats{p50_ms = P50/1000, p95_ms = P95/1000, p99_ms = P99/1000, max_ms = Max/1000, avg_ms = Avg/1000,
           p50_us = P50, p95_us = P95, p99_us = P99, max_us = Max, avg_us = Avg};
calculate_statistics([]) ->
    #stats{p50_ms = 0, p95_ms = 0, p99_ms = 0, max_ms = 0, avg_ms = 0,
           p50_us = 0, p95_us = 0, p99_us = 0, max_us = 0, avg_us = 0}.

clear_metrics_table(Table) when Table =:= undefined ->
    ok;
clear_metrics_table(Table) ->
    case catch ets:delete_all_objects(Table) of
        {'EXIT', badarg} -> ok;
        {'EXIT', {badarg, _}} -> ok;
        _ -> ok
    end.
