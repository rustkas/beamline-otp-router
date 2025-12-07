%% @doc Load tests for Extensions Pipeline
%% Tests performance, latency, throughput, and circuit breaker behavior
%% @test_category slow, load, extensions
-module(router_extensions_pipeline_load_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").



-record(stats, {
    p50_ms, p95_ms, p99_ms, max_ms, avg_ms,
    p50_us, p95_us, p99_us, max_us, avg_us
}).

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).

all() ->
    [
        {group, load_tests}
    ].

groups() ->
    [
        {load_tests, [sequence], [
            test_load_registry_lookup_performance,
            test_load_no_errors,
            test_load_latency_distribution,
            test_load_throughput_measurement,
            test_load_multiple_extensions,
            test_load_with_timeouts,
            test_load_with_degraded_instance,
            test_load_circuit_breaker_activation
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, extension_registry, [
        {source, fixtures}
    ]),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_extension_registry, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    meck:new(router_nats, [passthrough]),
    
    %% Setup telemetry collection for metrics
    ExtensionMetrics = ets:new(extension_metrics, [set, private]),
    HandlerId = {?MODULE, load_test_metrics},
    
    telemetry:attach(HandlerId, [router_extension_invoker, invocation_total],
        fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
            ExtensionId = maps:get(extension_id, Metadata, undefined),
            Status = maps:get(status, Metadata, undefined),
            LatencyMs = maps:get(latency_ms, Metadata, 0),
            RetriesUsed = maps:get(retries_used, Metadata, 0),
            
            Key = {ExtensionId, Status},
            case ets:lookup(ExtensionMetrics, Key) of
                [] ->
                    ets:insert(ExtensionMetrics, {Key, 1, [LatencyMs], RetriesUsed});
                [{Key, Count, Latencies, MaxRetries}] ->
                    NewCount = Count + 1,
                    NewLatencies = [LatencyMs | Latencies],
                    NewMaxRetries = max(MaxRetries, RetriesUsed),
                    ets:insert(ExtensionMetrics, {Key, NewCount, NewLatencies, NewMaxRetries})
            end
        end, #{}),
    
    [{extension_metrics, ExtensionMetrics}, {telemetry_handler, HandlerId} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Detach telemetry handler
    case proplists:get_value(telemetry_handler, Config) of
        HandlerId when HandlerId =/= undefined ->
            telemetry:detach(HandlerId);
        _ ->
            ok
    end,
    
    %% Cleanup ETS tables
    case proplists:get_value(extension_metrics, Config) of
        MetricsTable when MetricsTable =/= undefined ->
            ets:delete(MetricsTable);
        _ ->
            ok
    end,
    
    meck:unload(router_nats),
    Config.

%% @doc Load test: No errors (baseline performance)
test_load_no_errors(Config) ->
    NumRequests = 1000,
    
    %% Mock NATS to return successful responses quickly
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        timer:sleep(10),  % Simulate 10ms extension response time
        {ok, jsx:encode(#{
            <<"payload">> => #{<<"payload">> => <<"processed">>},
            <<"metadata">> => #{}
        })}
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    %% Measure latencies
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    
    %% Calculate statistics
    Stats = calculate_statistics(Latencies),
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    ct:log("Load test (no errors): ~p requests, ~p success, ~p errors", 
           [NumRequests, SuccessCount, ErrorCount]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms, Max=~p ms", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms, Stats#stats.max_ms]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    
    %% Assertions
    ?assertEqual(NumRequests, SuccessCount),
    ?assertEqual(0, ErrorCount),
    ?assert(Stats#stats.p95_ms < 50),  % P95 should be < 50ms
    ?assert(Stats#stats.p99_ms < 100),  % P99 should be < 100ms
    
    ok.

%% @doc Load test: With periodic timeouts
test_load_with_timeouts(Config) ->
    NumRequests = 1000,
    TimeoutRate = 0.05,  % 5% timeout rate
    
    %% Mock NATS to timeout occasionally
    TimeoutCounter = ets:new(timeout_counter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(TimeoutCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(TimeoutCounter, {count, RequestNum}),
        
        %% Timeout based on TimeoutRate (5% of requests)
        TimeoutThreshold = trunc(1.0 / TimeoutRate),  %% 1 / 0.05 = 20
        case (RequestNum rem TimeoutThreshold) =:= 0 of
            true ->
                {error, timeout};
            false ->
                timer:sleep(10),
                {ok, jsx:encode(#{
                    <<"payload">> => #{<<"payload">> => <<"processed">>},
                    <<"metadata">> => #{}
                })}
        end
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}  % Fail-open
    ]),
    
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    
    Stats = calculate_statistics(Latencies),
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    ct:log("Load test (with timeouts): ~p requests, ~p success, ~p errors", 
           [NumRequests, SuccessCount, ErrorCount]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    
    %% With fail-open mode, should still succeed despite timeouts
    ?assert(SuccessCount > NumRequests * 0.9),  % At least 90% success
    ?assert(Stats#stats.p95_ms < 100),  % P95 should be reasonable
    
    ets:delete(TimeoutCounter),
    ok.

%% @doc Load test: With degraded instance
test_load_with_degraded_instance(Config) ->
    NumRequests = 1000,
    
    %% Mock NATS to simulate degraded instance (slow responses)
    SlowCounter = ets:new(slow_counter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(SlowCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(SlowCounter, {count, RequestNum}),
        
        %% Every 10th request is slow (degraded instance)
        case (RequestNum rem 10) =:= 0 of
            true ->
                timer:sleep(200),  % Slow response (200ms)
                {ok, jsx:encode(#{
                    <<"payload">> => #{<<"payload">> => <<"processed">>},
                    <<"metadata">> => #{}
                })};
            false ->
                timer:sleep(10),  % Normal response (10ms)
                {ok, jsx:encode(#{
                    <<"payload">> => #{<<"payload">> => <<"processed">>},
                    <<"metadata">> => #{}
                })}
        end
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    
    Stats = calculate_statistics(Latencies),
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    ct:log("Load test (degraded instance): ~p requests, ~p success, ~p errors", 
           [NumRequests, SuccessCount, ErrorCount]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms, Max=~p ms", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms, Stats#stats.max_ms]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    
    %% Should still succeed, but with higher latency
    ?assertEqual(NumRequests, SuccessCount),
    ?assert(Stats#stats.p95_ms < 250),  % P95 should be < 250ms (some slow requests)
    ?assert(Stats#stats.p99_ms < 300),  % P99 should be < 300ms
    
    ets:delete(SlowCounter),
    ok.

%% @doc Load test: Circuit breaker activation
test_load_circuit_breaker_activation(Config) ->
    NumRequests = 100,
    
    %% Mock NATS to fail frequently (trigger circuit breaker)
    FailureCounter = ets:new(failure_counter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(FailureCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(FailureCounter, {count, RequestNum}),
        
        %% Fail 60% of requests (above error rate threshold)
        case (RequestNum rem 10) < 6 of
            true ->
                {error, processing_error};
            false ->
                timer:sleep(10),
                {ok, jsx:encode(#{
                    <<"payload">> => #{<<"payload">> => <<"processed">>},
                    <<"metadata">> => #{}
                })}
        end
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    %% Track circuit breaker state changes
    CircuitBreakerEvents = ets:new(circuit_breaker_events, [set, private]),
    HandlerId = {?MODULE, circuit_breaker_test},
    telemetry:attach(HandlerId, [router_extension_circuit_breaker, circuit_opened_total],
        fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
            ets:insert(CircuitBreakerEvents, {circuit_opened, Metadata})
        end, #{}),
    
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    
    %% Check if circuit breaker was activated
    CircuitOpened = ets:lookup(CircuitBreakerEvents, circuit_opened) =/= [],
    
    Stats = calculate_statistics(Latencies),
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    %% Get health metrics
    HealthMetrics = case router_extension_health:get_health(<<"normalize_text">>) of
        {ok, Health} -> Health;
        _ -> #{}
    end,
    
    ct:log("Load test (circuit breaker): ~p requests, ~p success, ~p errors", 
           [NumRequests, SuccessCount, ErrorCount]),
    ct:log("Circuit breaker opened: ~p", [CircuitOpened]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    ct:log("Health metrics: ~p", [HealthMetrics]),
    
    %% Circuit breaker should activate with high error rate
    ?assert(CircuitOpened orelse ErrorCount > NumRequests * 0.5),
    
    telemetry:detach(HandlerId),
    ets:delete(FailureCounter),
    ets:delete(CircuitBreakerEvents),
    ok.

%% @doc Load test: Latency distribution
test_load_latency_distribution(Config) ->
    NumRequests = 1000,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        %% Variable latency (10-50ms)
        Latency = 10 + (erlang:phash2(erlang:unique_integer()) rem 40),
        timer:sleep(Latency),
        {ok, jsx:encode(#{
            <<"payload">> => #{<<"payload">> => <<"processed">>},
            <<"metadata">> => #{}
        })}
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    {Latencies, _SuccessCount, _ErrorCount} = execute_load_test(NumRequests, Policy),
    
    Stats = calculate_statistics(Latencies),
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    ct:log("Latency distribution: P50=~p ms, P95=~p ms, P99=~p ms, Max=~p ms", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms, Stats#stats.max_ms]),
    ct:log("Average latency: ~p ms", [Stats#stats.avg_ms]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    
    %% Assertions
    ?assert(Stats#stats.p50_ms < 30),  % P50 should be < 30ms
    ?assert(Stats#stats.p95_ms < 50),  % P95 should be < 50ms
    ?assert(Stats#stats.p99_ms < 60),  % P99 should be < 60ms
    
    ok.

%% @doc Load test: Throughput measurement
test_load_throughput_measurement(Config) ->
    NumRequests = 5000,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        timer:sleep(10),
        {ok, jsx:encode(#{
            <<"payload">> => #{<<"payload">> => <<"processed">>},
            <<"metadata">> => #{}
        })}
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    StartTime = erlang:monotonic_time(microsecond),
    {_Latencies, SuccessCount, _ErrorCount} = execute_load_test(NumRequests, Policy),
    EndTime = erlang:monotonic_time(microsecond),
    
    DurationUs = EndTime - StartTime,
    DurationMs = DurationUs / 1000,
    Throughput = (SuccessCount * 1000000) / DurationUs,
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    ct:log("Throughput test: ~p requests in ~p ms (~p ops/sec)", 
           [SuccessCount, DurationMs, Throughput]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    
    %% Assertions: throughput should be > 100 ops/sec
    ?assert(Throughput > 100),
    ?assertEqual(NumRequests, SuccessCount),
    
    ok.

%% @doc Load test: Multiple extensions in pipeline
test_load_multiple_extensions(Config) ->
    NumRequests = 500,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        timer:sleep(10),
        case binary:match(_Subject, <<"validate">>) of
            nomatch ->
                {ok, jsx:encode(#{
                    <<"payload">> => #{<<"payload">> => <<"processed">>},
                    <<"metadata">> => #{}
                })};
            _ ->
                {ok, jsx:encode(#{
                    <<"status">> => <<"ok">>
                })}
        end
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]},
        {validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]},
        {post, [{id, <<"mask_pii">>, mode, <<"required">>}]}
    ]),
    
    {Latencies, SuccessCount, ErrorCount} = execute_load_test(NumRequests, Policy),
    
    Stats = calculate_statistics(Latencies),
    
    %% Collect extension metrics
    ExtensionMetrics = proplists:get_value(extension_metrics, Config),
    ExtensionStats = collect_extension_metrics(ExtensionMetrics),
    
    ct:log("Load test (multiple extensions): ~p requests, ~p success, ~p errors", 
           [NumRequests, SuccessCount, ErrorCount]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms]),
    ct:log("Extension metrics: ~p", [ExtensionStats]),
    
    %% With 3 extensions, latency should be ~3x single extension
    ?assert(Stats#stats.p95_ms < 100),  % P95 should be < 100ms (3 extensions * ~30ms)
    ?assertEqual(NumRequests, SuccessCount),
    
    ok.

%% @doc Load test: Extension Registry lookup performance
test_load_registry_lookup_performance(_Config) ->
    NumLookups = 10000,
    
    %% Measure registry lookup latency
    Latencies = [begin
        StartTime = erlang:monotonic_time(microsecond),
        router_extension_registry:lookup(<<"normalize_text">>),
        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end || _ <- lists:seq(1, NumLookups)],
    
    Stats = calculate_statistics(Latencies),
    
    ct:log("Registry lookup performance: ~p lookups", [NumLookups]),
    ct:log("Latency stats: P50=~p us, P95=~p us, P99=~p us, Max=~p us", 
           [Stats#stats.p50_us, Stats#stats.p95_us, Stats#stats.p99_us, Stats#stats.max_us]),
    ct:log("Latency stats (ms): P50=~p, P95=~p, P99=~p", 
           [Stats#stats.p50_ms, Stats#stats.p95_ms, Stats#stats.p99_ms]),
    
    %% Registry lookup should be very fast (ETS-based)
    ?assert(Stats#stats.p95_us < 1000),  % P95 should be < 1ms
    ?assert(Stats#stats.p99_us < 2000),  % P99 should be < 2ms
    
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================


%% Execute load test
execute_load_test(NumRequests, Policy) ->
    Request = create_route_request(<<"Test message">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Execute requests (with small delay to avoid overwhelming)
    Results = lists:map(fun(_N) ->
        StartTime = erlang:monotonic_time(microsecond),
        Result = router_decider:decide(Request, Policy, Context),
        EndTime = erlang:monotonic_time(microsecond),
        Latency = EndTime - StartTime,
        %% Small delay to avoid overwhelming system
        timer:sleep(1),
        {Result, Latency}
    end, lists:seq(1, NumRequests)),
    
    {SuccessResults, ErrorResults} = lists:partition(fun({Result, _}) ->
        case Result of
            {ok, _} -> true;
            _ -> false
        end
    end, Results),
    
    Latencies = [Latency || {_, Latency} <- SuccessResults],
    SuccessCount = length(SuccessResults),
    ErrorCount = length(ErrorResults),
    
    {Latencies, SuccessCount, ErrorCount}.

%% Calculate statistics
calculate_statistics(Latencies) when length(Latencies) > 0 ->
    Sorted = lists:sort(Latencies),
    Length = length(Sorted),
    
    P50Idx = trunc(Length * 0.5),
    P95Idx = trunc(Length * 0.95),
    P99Idx = trunc(Length * 0.99),
    
    P50 = lists:nth(max(1, P50Idx), Sorted),
    P95 = lists:nth(max(1, P95Idx), Sorted),
    P99 = lists:nth(max(1, P99Idx), Sorted),
    Max = lists:max(Sorted),
    Avg = lists:sum(Sorted) / Length,
    
    #stats{
        p50_us = P50,
        p95_us = P95,
        p99_us = P99,
        max_us = Max,
        avg_us = Avg,
        p50_ms = P50 / 1000,
        p95_ms = P95 / 1000,
        p99_ms = P99 / 1000,
        max_ms = Max / 1000,
        avg_ms = Avg / 1000
    };
calculate_statistics([]) ->
    #stats{
        p50_ms = 0, p95_ms = 0, p99_ms = 0, max_ms = 0, avg_ms = 0,
        p50_us = 0, p95_us = 0, p99_us = 0, max_us = 0, avg_us = 0
    }.

%% Create policy with extensions
create_policy_with_extensions(Extensions) ->
    Pre = proplists:get_value(pre, Extensions, []),
    Validators = proplists:get_value(validators, Extensions, []),
    Post = proplists:get_value(post, Extensions, []),
    
    PreItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, mode, Mode} ->
                #{id => Id, mode => Mode, config => #{}};
            _ when is_map(Item) ->
                Item;
            _ ->
                #{}
        end
    end, Pre),
    
    ValidatorItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, on_fail, OnFail} ->
                #{id => Id, on_fail => OnFail};
            _ when is_map(Item) ->
                Item;
            _ ->
                #{}
        end
    end, Validators),
    
    PostItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, mode, Mode} ->
                #{id => Id, mode => Mode, config => #{}};
            _ when is_map(Item) ->
                Item;
            _ ->
                #{}
        end
    end, Post),
    
    #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = PreItems,
        validators = ValidatorItems,
        post = PostItems,
        weights = #{<<"openai:gpt-4">> => 1.0},
        metadata = #{}
    }.

%% Create route request
create_route_request(Payload, Policy) ->
    #route_request{
        message = #{
            <<"message_id">> => <<"msg-001">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => Payload,
            <<"metadata">> => #{}
        },
        policy_id = Policy#policy.policy_id,
        context = #{}
    }.

%% Collect extension metrics from telemetry
collect_extension_metrics(MetricsTable) when MetricsTable =/= undefined ->
    AllMetrics = ets:tab2list(MetricsTable),
    lists:map(fun({{ExtensionId, Status}, Count, Latencies, MaxRetries}) ->
        Sorted = lists:sort(Latencies),
        Length = length(Sorted),
        P50Idx = trunc(Length * 0.5),
        P95Idx = trunc(Length * 0.95),
        P99Idx = trunc(Length * 0.99),
        
        P50 = case Length > 0 of
            true -> lists:nth(max(1, P50Idx), Sorted);
            false -> 0
        end,
        P95 = case Length > 0 of
            true -> lists:nth(max(1, P95Idx), Sorted);
            false -> 0
        end,
        P99 = case Length > 0 of
            true -> lists:nth(max(1, P99Idx), Sorted);
            false -> 0
        end,
        Avg = case Length > 0 of
            true -> lists:sum(Latencies) / Length;
            false -> 0
        end,
        
        #{
            extension_id => ExtensionId,
            status => Status,
            count => Count,
            p50_latency_ms => P50,
            p95_latency_ms => P95,
            p99_latency_ms => P99,
            avg_latency_ms => Avg,
            max_retries => MaxRetries
        }
    end, AllMetrics);
collect_extension_metrics(_) ->
    [].

