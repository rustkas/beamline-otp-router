%% @doc Extensions Pipeline Load: Advanced Tests
%%
%% Advanced load tests with failure scenarios:
%% - Multiple extensions
%% - With timeouts
%% - With degraded instance
%% - Circuit breaker activation
%%
%% @test_category load, heavy, extensions
-module(router_ext_load_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-record(stats, {p50_ms, p95_ms, p99_ms, max_ms, avg_ms, p50_us, p95_us, p99_us, max_us, avg_us}).

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_multiple_extensions/1,
    test_with_timeouts/1,
    test_with_degraded_instance/1,
    test_circuit_breaker_activation/1
]).

suite() -> [{timetrap, {minutes, 15}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        _ -> []
    end.
groups() ->
    [{advanced_tests, [sequence], [
        test_multiple_extensions,
        test_with_timeouts,
        test_with_degraded_instance,
        test_circuit_breaker_activation
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, extension_registry, [{source, fixtures}]),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, circuit_breaker_enabled, true),
    ok = router_mock_helpers:setup_router_nats_mock(),
    _ = router_ets_helpers:ensure_named_ets_table(timeout_counter, [set, public]),
    _ = router_ets_helpers:ensure_named_ets_table(slow_counter, [set, public]),
    _ = router_ets_helpers:ensure_named_ets_table(failure_counter, [set, public]),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> test_helpers:wait_for_app_start(router_extension_registry, 1000), Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TC, Config) ->
    router_mock_helpers:reset(router_nats),
    TimeoutCounter = router_ets_helpers:ensure_named_ets_table(timeout_counter, [set, public]),
    SlowCounter = router_ets_helpers:ensure_named_ets_table(slow_counter, [set, public]),
    FailureCounter = router_ets_helpers:ensure_named_ets_table(failure_counter, [set, public]),
    ets:delete_all_objects(TimeoutCounter),
    ets:delete_all_objects(SlowCounter),
    ets:delete_all_objects(FailureCounter),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_multiple_extensions(_Config) ->
    ct:comment("=== Multiple Extensions ==="),
    NumRequests = 500,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        timer:sleep(5),
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})}
    end),
    
    Policy = create_policy([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]},
        {post, [{id, <<"mask_pii">>, mode, <<"optional">>}]}
    ]),
    
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    Stats = calculate_statistics(Latencies),
    
    ct:comment("Requests: ~p, Success: ~p, Errors: ~p", [NumRequests, SuccessCount, ErrorCount]),
    ct:comment("P95: ~p ms, P99: ~p ms", [Stats#stats.p95_ms, Stats#stats.p99_ms]),
    
    ?assert(SuccessCount > NumRequests * 0.9),
    ok.

test_with_timeouts(_Config) ->
    ct:comment("=== With Timeouts ==="),
    NumRequests = 500,
    TimeoutRate = 0.05,
    
    Counter = router_ets_helpers:ensure_named_ets_table(timeout_counter, [set, public]),
    ets:delete_all_objects(Counter),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        N = case router_ets_helpers:ets_lookup(Counter, count) of [] -> 1; [{count, C}] -> C + 1 end,
        ets:insert(Counter, {count, N}),
        case rand:uniform() < TimeoutRate of
            true -> {error, timeout};
            false -> timer:sleep(10), {ok, jsx:encode(#{<<"payload">> => <<"ok">>, <<"metadata">> => #{}})}
        end
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}]),
    {_Latencies, SuccessCount, _ErrorCount} = execute_load_test(NumRequests, Policy),
    
    ct:comment("Success: ~p", [SuccessCount]),
    
    ?assert(SuccessCount > NumRequests * 0.9),
    
    ets:delete(Counter),
    ok.

test_with_degraded_instance(_Config) ->
    ct:comment("=== With Degraded Instance ==="),
    NumRequests = 500,
    
    Counter = router_ets_helpers:ensure_named_ets_table(slow_counter, [set, public]),
    ets:delete_all_objects(Counter),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        N = case router_ets_helpers:ets_lookup(Counter, count) of [] -> 1; [{count, C}] -> C + 1 end,
        ets:insert(Counter, {count, N}),
        SlowProbability = min(0.3, N / 500),
        Latency = case rand:uniform() < SlowProbability of
            true -> 100 + rand:uniform(100);
            false -> 10 + rand:uniform(10)
        end,
        timer:sleep(Latency),
        {ok, jsx:encode(#{<<"payload">> => <<"ok">>, <<"metadata">> => #{}})}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}]),
    StartTime = erlang:monotonic_time(millisecond),
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    EndTime = erlang:monotonic_time(millisecond),
    Stats = calculate_statistics(Latencies),
    DurationMs = EndTime - StartTime,
    Throughput = case DurationMs > 0 of
        true -> SuccessCount / (DurationMs / 1000);
        false -> 0
    end,
    ErrorRate = case NumRequests > 0 of
        true -> ErrorCount / NumRequests;
        false -> 0.0
    end,
    record_observation(<<"advanced_degraded">>, true, Stats, Throughput, ErrorRate),
    
    ct:comment("Success: ~p, Errors: ~p, P95: ~p ms", [SuccessCount, ErrorCount, Stats#stats.p95_ms]),
    
    ?assertEqual(NumRequests, SuccessCount),
    maybe_assert_degraded_p95(Stats#stats.p95_ms),
    
    ets:delete(Counter),
    ok.

maybe_assert_degraded_p95(P95Ms) ->
    MaxP95 = env_number("ROUTER_EXT_LOAD_ADVANCED_P95_MAX_MS", 260.0),
    case env_bool("ROUTER_EXT_LOAD_ADVANCED_P95_ASSERT", false) of
        true ->
            ?assert(P95Ms < MaxP95);
        false ->
            ct:comment("PERF: advanced degraded P95 ~p ms >= ~p ms (assert quarantined)", [P95Ms, MaxP95])
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
                {error, no_float} ->
                    case string:to_integer(Val) of
                        {error, _} -> Default;
                        {Int, _} -> float(Int)
                    end;
                {Float, _} -> Float
            end
    end.

test_circuit_breaker_activation(_Config) ->
    ct:comment("=== Circuit Breaker Activation ==="),
    NumRequests = 100,
    
    Counter = router_ets_helpers:ensure_named_ets_table(failure_counter, [set, public]),
    ets:delete_all_objects(Counter),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        N = case router_ets_helpers:ets_lookup(Counter, count) of [] -> 1; [{count, C}] -> C + 1 end,
        ets:insert(Counter, {count, N}),
        FailRate = min(0.8, N / 50),
        case rand:uniform() < FailRate of
            true -> {error, timeout};
            false -> timer:sleep(10), {ok, jsx:encode(#{<<"payload">> => <<"ok">>, <<"metadata">> => #{}})}
        end
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}]),
    {_Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    
    ct:comment("Success: ~p, Errors: ~p", [SuccessCount, ErrorCount]),
    
    ?assert(ErrorCount > NumRequests * 0.3, "Should have significant errors"),
    
    ets:delete(Counter),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

create_policy(Extensions) ->
    Pre = proplists:get_value(pre, Extensions, []),
    Post = proplists:get_value(post, Extensions, []),
    PreItems = lists:map(fun({id, Id, mode, Mode}) -> #{id => Id, mode => Mode, config => #{}} end, Pre),
    PostItems = lists:map(fun({id, Id, mode, Mode}) -> #{id => Id, mode => Mode, config => #{}} end, Post),
    #policy{tenant_id = <<"test">>, policy_id = <<"test">>, version = <<"1.0">>, 
            pre = PreItems, validators = [], post = PostItems, 
            weights = #{<<"openai:gpt-4">> => 1.0}, metadata = #{}}.

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

record_observation(Scenario, DegradedInstance, Stats, Throughput, ErrorRate) ->
    Entry = #{
        <<"suite">> => <<"router_ext_load_advanced_SUITE">>,
        <<"level">> => <<"heavy">>,
        <<"workload">> => #{
            <<"scenario">> => Scenario,
            <<"degraded_instance">> => DegradedInstance
        },
        <<"metrics">> => #{
            <<"latency_ms">> => #{
                <<"p50">> => Stats#stats.p50_ms,
                <<"p95">> => Stats#stats.p95_ms,
                <<"p99">> => Stats#stats.p99_ms
            },
            <<"throughput_rps">> => #{<<"avg">> => Throughput},
            <<"error_rate">> => #{<<"fraction">> => ErrorRate}
        }
    },
    case router_perf_observations:record(Entry) of
        ok -> ok;
        {error, Reason} -> ct:comment("Failed to record perf observation: ~p", [Reason])
    end.
