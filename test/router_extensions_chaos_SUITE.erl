%% @doc Chaos/Resilience tests for Extensions Pipeline
%% Tests system behavior under failure conditions:
%% - NATS complete failure
%% - Extension instance flapping
%% - Latency degradation
%% - Mass degradation of multiple extensions
%% @test_category slow, chaos, resilience, extensions
-module(router_extensions_chaos_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").


-record(chaos_stats, {
    total_requests,
    success_count,
    error_count,
    timeout_count,
    circuit_breaker_opened_count,
    recovery_time_ms,
    client_errors,
    p50_latency_ms,
    p95_latency_ms,
    p99_latency_ms,
    max_latency_ms
}).


-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).

all() ->
    [
        {group, chaos_tests}
    ].

groups() ->
    [
        {chaos_tests, [sequence], [
            test_chaos_nats_complete_failure,
            test_chaos_extension_instance_flapping,
            test_chaos_latency_degradation,
            test_chaos_mass_degradation,
            test_chaos_recovery_after_failure
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
    ok = application:set_env(beamline_router, circuit_breaker_enabled, true),
    ok = application:set_env(beamline_router, circuit_breaker_failure_threshold, 5),
    ok = application:set_env(beamline_router, circuit_breaker_timeout_seconds, 10),
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
    meck:new(router_decider, [passthrough]),
    meck:new(router_extension_registry, [passthrough]),
    meck:new(router_extension_invoker, [passthrough]),
    
    %% Mock router_decider to return successful decisions
    meck:expect(router_decider, decide, fun(_Request, _Policy, _Context) ->
        {ok, #route_decision{
            provider_id = <<"openai:gpt-4o">>,
            priority = 50,
            expected_latency_ms = 850,
            expected_cost = 0.012,
            reason = <<"best_score">>
        }}
    end),
    
    %% Mock extension registry to return test extensions
    meck:expect(router_extension_registry, lookup, fun(ExtensionId) ->
        case ExtensionId of
            <<"normalize_text">> ->
                {ok, #extension{
                    id = <<"normalize_text">>,
                    type = <<"pre">>,
                    subject = <<"beamline.ext.pre.normalize_text.v1">>,
                    timeout_ms = 1000,
                    retry = 0,
                    enabled = true
                }};
            <<"pii_guard">> ->
                {ok, #extension{
                    id = <<"pii_guard">>,
                    type = <<"validator">>,
                    subject = <<"beamline.ext.validate.pii_guard.v1">>,
                    timeout_ms = 1000,
                    retry = 0,
                    enabled = true
                }};
            <<"mask_pii">> ->
                {ok, #extension{
                    id = <<"mask_pii">>,
                    type = <<"post">>,
                    subject = <<"beamline.ext.post.mask_pii.v1">>,
                    timeout_ms = 1000,
                    retry = 0,
                    enabled = true
                }};
            _ ->
                {error, not_found}
        end
    end),
    
    %% Setup telemetry collection for chaos metrics
    ChaosMetrics = ets:new(chaos_metrics, [set, private]),
    HandlerId = {?MODULE, chaos_test_metrics},
    
    telemetry:attach(HandlerId, [router_extension_invoker, invocation_total],
        fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
            ExtensionId = maps:get(extension_id, Metadata, undefined),
            Status = maps:get(status, Metadata, undefined),
            LatencyMs = maps:get(latency_ms, Metadata, 0),
            ErrorReason = maps:get(error_reason, Metadata, undefined),
            
            Key = {ExtensionId, Status, ErrorReason},
            case ets:lookup(ChaosMetrics, Key) of
                [] ->
                    ets:insert(ChaosMetrics, {Key, 1, [LatencyMs]});
                [{Key, Count, Latencies}] ->
                    NewCount = Count + 1,
                    NewLatencies = [LatencyMs | Latencies],
                    ets:insert(ChaosMetrics, {Key, NewCount, NewLatencies})
            end
        end, #{}),
    
    %% Track circuit breaker events
    CircuitBreakerEvents = ets:new(circuit_breaker_events, [set, private]),
    CBHandlerId = {?MODULE, chaos_circuit_breaker},
    telemetry:attach(CBHandlerId, [router_extension_circuit_breaker, circuit_opened_total],
        fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
            ExtensionId = maps:get(extension_id, Metadata, undefined),
            Timestamp = erlang:monotonic_time(millisecond),
            ets:insert(CircuitBreakerEvents, {circuit_opened, ExtensionId, Timestamp})
        end, #{}),
    
    [{chaos_metrics, ChaosMetrics}, 
     {telemetry_handler, HandlerId},
     {circuit_breaker_events, CircuitBreakerEvents},
     {circuit_breaker_handler, CBHandlerId} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Detach telemetry handlers
    case proplists:get_value(telemetry_handler, Config) of
        HandlerId when HandlerId =/= undefined ->
            telemetry:detach(HandlerId);
        _ ->
            ok
    end,
    case proplists:get_value(circuit_breaker_handler, Config) of
        CBHandlerId when CBHandlerId =/= undefined ->
            telemetry:detach(CBHandlerId);
        _ ->
            ok
    end,
    
    %% Cleanup ETS tables
    case proplists:get_value(chaos_metrics, Config) of
        MetricsTable when MetricsTable =/= undefined ->
            ets:delete(MetricsTable);
        _ ->
            ok
    end,
    case proplists:get_value(circuit_breaker_events, Config) of
        CBEvents when CBEvents =/= undefined ->
            ets:delete(CBEvents);
        _ ->
            ok
    end,
    
    meck:unload([router_nats, router_decider, router_extension_registry, router_extension_invoker]),
    Config.

%% @doc Chaos test: Complete NATS failure
%% Scenario: NATS server completely unavailable
%% Expected: Router should handle gracefully, return appropriate errors to client
test_chaos_nats_complete_failure(Config) ->
    NumRequests = 100,
    
    %% Mock NATS to always fail (connection error)
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {error, connection_error}
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    _ = erlang:monotonic_time(millisecond),
    {Results, Latencies} = execute_chaos_test(NumRequests, Policy),
    _ = erlang:monotonic_time(millisecond),
    
    %% Analyze results
    {SuccessCount, ErrorCount, TimeoutCount, ClientErrors} = analyze_results(Results),
    
    %% Collect metrics
    ChaosMetrics = proplists:get_value(chaos_metrics, Config),
    Metrics = collect_chaos_metrics(ChaosMetrics),
    
    %% Get circuit breaker events
    CBEvents = proplists:get_value(circuit_breaker_events, Config),
    CircuitBreakerOpened = ets:tab2list(CBEvents),
    
    Stats = calculate_chaos_statistics(Latencies),
    
    ct:log("=== NATS Complete Failure Test ==="),
    ct:log("Total requests: ~p", [NumRequests]),
    ct:log("Success: ~p, Errors: ~p, Timeouts: ~p", [SuccessCount, ErrorCount, TimeoutCount]),
    ct:log("Client errors: ~p", [ClientErrors]),
    ct:log("Circuit breaker opened: ~p times", [length(CircuitBreakerOpened)]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms, Max=~p ms", 
           [Stats#chaos_stats.p50_latency_ms, Stats#chaos_stats.p95_latency_ms, 
            Stats#chaos_stats.p99_latency_ms, Stats#chaos_stats.max_latency_ms]),
    ct:log("Metrics: ~p", [Metrics]),
    
    %% Expected behavior:
    %% - All requests should fail (NATS unavailable)
    %% - Router should return error to client (not hang)
    %% - Circuit breaker should open after threshold
    %% - Error should be clear (extension_unavailable or similar)
    ?assertEqual(0, SuccessCount),
    ?assert(ErrorCount > 0),
    ?assert(length(CircuitBreakerOpened) > 0, "Circuit breaker should open"),
    
    %% Check error types
    ExtensionUnavailableErrors = [Err || Err = {error, {extension_unavailable, _}} <- ClientErrors],
    ?assert(length(ExtensionUnavailableErrors) > 0, "Should return extension_unavailable errors"),
    
    ok.

%% @doc Chaos test: Extension instance flapping
%% Scenario: Extension instance alternates between available and unavailable
%% Expected: Circuit breaker should handle flapping, Router should recover quickly
test_chaos_extension_instance_flapping(Config) ->
    NumRequests = 200,
    FlapPeriod = 10,  % Flap every 10 requests
    
    %% Mock NATS to simulate flapping (alternate success/failure)
    FlapCounter = ets:new(flap_counter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(FlapCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(FlapCounter, {count, RequestNum}),
        
        %% Flap: available for 5 requests, then unavailable for 5 requests
        IsAvailable = ((RequestNum div FlapPeriod) rem 2) =:= 0,
        case IsAvailable of
            true ->
                timer:sleep(10),
                {ok, jsx:encode(#{
                    <<"payload">> => #{<<"payload">> => <<"processed">>},
                    <<"metadata">> => #{}
                })};
            false ->
                {error, timeout}
        end
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}  % Fail-open
    ]),
    
    StartTime = erlang:monotonic_time(millisecond),
    {Results, Latencies} = execute_chaos_test(NumRequests, Policy),
    EndTime = erlang:monotonic_time(millisecond),
    
    {SuccessCount, ErrorCount, TimeoutCount, _ClientErrors} = analyze_results(Results),
    
    ChaosMetrics = proplists:get_value(chaos_metrics, Config),
    Metrics = collect_chaos_metrics(ChaosMetrics),
    
    CBEvents = proplists:get_value(circuit_breaker_events, Config),
    CircuitBreakerOpened = ets:tab2list(CBEvents),
    
    Stats = calculate_chaos_statistics(Latencies),
    RecoveryTime = EndTime - StartTime,
    
    ct:log("=== Extension Instance Flapping Test ==="),
    ct:log("Total requests: ~p", [NumRequests]),
    ct:log("Success: ~p, Errors: ~p, Timeouts: ~p", [SuccessCount, ErrorCount, TimeoutCount]),
    ct:log("Circuit breaker opened: ~p times", [length(CircuitBreakerOpened)]),
    ct:log("Recovery time: ~p ms", [RecoveryTime]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms", 
           [Stats#chaos_stats.p50_latency_ms, Stats#chaos_stats.p95_latency_ms, 
            Stats#chaos_stats.p99_latency_ms]),
    ct:log("Metrics: ~p", [Metrics]),
    
    %% Expected behavior:
    %% - Some requests should succeed (when extension available)
    %% - Some requests should fail (when extension unavailable)
    %% - Circuit breaker may open/close multiple times
    %% - With fail-open mode, Router should continue processing
    ?assert(SuccessCount > 0, "Some requests should succeed"),
    ?assert(ErrorCount > 0, "Some requests should fail"),
    
    %% With fail-open mode, success rate should be reasonable
    SuccessRate = SuccessCount / NumRequests,
    ?assert(SuccessRate > 0.3, "Success rate should be > 30% with fail-open"),
    
    ets:delete(FlapCounter),
    ok.

%% @doc Chaos test: Latency degradation
%% Scenario: Extension latency increases dramatically (from 10ms to 500ms+)
%% Expected: Router should handle gracefully, circuit breaker may open, client should see timeouts
test_chaos_latency_degradation(Config) ->
    NumRequests = 150,
    
    %% Mock NATS to simulate latency degradation (gradual increase)
    LatencyCounter = ets:new(latency_counter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(LatencyCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(LatencyCounter, {count, RequestNum}),
        
        %% Latency increases: 10ms -> 50ms -> 100ms -> 200ms -> 500ms
        BaseLatency = 10,
        DegradationFactor = min(50, RequestNum div 30),  % Increase every 30 requests
        Latency = BaseLatency + (DegradationFactor * 10),
        
        %% Cap at 500ms (but may timeout)
        ActualLatency = min(Latency, 500),
        timer:sleep(ActualLatency),
        
        {ok, jsx:encode(#{
            <<"payload">> => #{<<"payload">> => <<"processed">>},
            <<"metadata">> => #{}
        })}
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    _ = erlang:monotonic_time(millisecond),
    {Results, Latencies} = execute_chaos_test(NumRequests, Policy),
    _ = erlang:monotonic_time(millisecond),
    
    {SuccessCount, ErrorCount, TimeoutCount, _ClientErrors} = analyze_results(Results),
    
    ChaosMetrics = proplists:get_value(chaos_metrics, Config),
    Metrics = collect_chaos_metrics(ChaosMetrics),
    
    Stats = calculate_chaos_statistics(Latencies),
    
    ct:log("=== Latency Degradation Test ==="),
    ct:log("Total requests: ~p", [NumRequests]),
    ct:log("Success: ~p, Errors: ~p, Timeouts: ~p", [SuccessCount, ErrorCount, TimeoutCount]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms, Max=~p ms", 
           [Stats#chaos_stats.p50_latency_ms, Stats#chaos_stats.p95_latency_ms, 
            Stats#chaos_stats.p99_latency_ms, Stats#chaos_stats.max_latency_ms]),
    ct:log("Metrics: ~p", [Metrics]),
    
    %% Expected behavior:
    %% - Early requests should succeed (low latency)
    %% - Later requests may timeout or fail (high latency)
    %% - P95/P99 latency should reflect degradation
    ?assert(SuccessCount > 0, "Some requests should succeed"),
    ?assert(Stats#chaos_stats.p95_latency_ms > 50, "P95 latency should reflect degradation"),
    ?assert(Stats#chaos_stats.max_latency_ms > 200, "Max latency should be high"),
    
    ets:delete(LatencyCounter),
    ok.

%% @doc Chaos test: Mass degradation of multiple extensions
%% Scenario: Multiple extensions fail simultaneously
%% Expected: Router should handle gracefully, circuit breakers should open, client should see errors
test_chaos_mass_degradation(Config) ->
    NumRequests = 100,
    DegradationStart = 30,  % Start degradation after 30 requests
    
    %% Mock NATS to simulate mass degradation
    DegradationCounter = ets:new(degradation_counter, [set, private]),
    meck:expect(router_nats, request, fun(Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(DegradationCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(DegradationCounter, {count, RequestNum}),
        
        %% After degradation starts, fail 80% of requests
        IsDegraded = RequestNum >= DegradationStart,
        ShouldFail = IsDegraded andalso ((RequestNum rem 10) < 8),
        
        case ShouldFail of
            true ->
                {error, processing_error};
            false ->
                timer:sleep(10),
                case binary:match(Subject, <<"validate">>) of
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
        end
    end),
    
    Policy = create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]},
        {validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]},
        {post, [{id, <<"mask_pii">>, mode, <<"optional">>}]}
    ]),
    
    _ = erlang:monotonic_time(millisecond),
    {Results, Latencies} = execute_chaos_test(NumRequests, Policy),
    _ = erlang:monotonic_time(millisecond),
    
    {SuccessCount, ErrorCount, TimeoutCount, _ClientErrors} = analyze_results(Results),
    
    ChaosMetrics = proplists:get_value(chaos_metrics, Config),
    Metrics = collect_chaos_metrics(ChaosMetrics),
    
    CBEvents = proplists:get_value(circuit_breaker_events, Config),
    CircuitBreakerOpened = ets:tab2list(CBEvents),
    
    Stats = calculate_chaos_statistics(Latencies),
    
    ct:log("=== Mass Degradation Test ==="),
    ct:log("Total requests: ~p", [NumRequests]),
    ct:log("Success: ~p, Errors: ~p, Timeouts: ~p", [SuccessCount, ErrorCount, TimeoutCount]),
    ct:log("Circuit breaker opened: ~p times", [length(CircuitBreakerOpened)]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms", 
           [Stats#chaos_stats.p50_latency_ms, Stats#chaos_stats.p95_latency_ms, 
            Stats#chaos_stats.p99_latency_ms]),
    ct:log("Metrics: ~p", [Metrics]),
    
    %% Expected behavior:
    %% - Early requests should succeed
    %% - After degradation, most requests should fail
    %% - Circuit breakers should open for failing extensions
    %% - Client should see appropriate errors
    ?assert(SuccessCount > 0, "Some requests should succeed (before degradation)"),
    ?assert(ErrorCount > 0, "Some requests should fail (after degradation)"),
    ?assert(length(CircuitBreakerOpened) > 0, "Circuit breakers should open"),
    
    %% Success rate should drop after degradation
    _ = SuccessCount,  % Simplified: all success is early
    _ = DegradationStart / NumRequests,
    ?assert(SuccessCount >= DegradationStart * 0.8, "Most early requests should succeed"),
    
    ets:delete(DegradationCounter),
    ok.

%% @doc Chaos test: Recovery after failure
%% Scenario: Extension fails, then recovers
%% Expected: Circuit breaker should transition to half-open, then closed, Router should recover
test_chaos_recovery_after_failure(Config) ->
    NumRequests = 150,
    FailureStart = 20,
    RecoveryStart = 80,
    
    %% Mock NATS to simulate failure then recovery
    RecoveryCounter = ets:new(recovery_counter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case ets:lookup(RecoveryCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(RecoveryCounter, {count, RequestNum}),
        
        %% Fail from request 20-80, then recover
        IsFailurePeriod = RequestNum >= FailureStart andalso RequestNum < RecoveryStart,
        case IsFailurePeriod of
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
    
    StartTime = erlang:monotonic_time(millisecond),
    {Results, Latencies} = execute_chaos_test(NumRequests, Policy),
    EndTime = erlang:monotonic_time(millisecond),
    
    {SuccessCount, ErrorCount, TimeoutCount, _ClientErrors} = analyze_results(Results),
    
    ChaosMetrics = proplists:get_value(chaos_metrics, Config),
    Metrics = collect_chaos_metrics(ChaosMetrics),
    
    CBEvents = proplists:get_value(circuit_breaker_events, Config),
    CircuitBreakerOpened = ets:tab2list(CBEvents),
    
    %% Get circuit breaker state at end
    FinalCBState = case router_extension_circuit_breaker:get_all_circuit_states() of
        {ok, States} -> States;
        _ -> #{}
    end,
    
    Stats = calculate_chaos_statistics(Latencies),
    RecoveryTime = EndTime - StartTime,
    
    ct:log("=== Recovery After Failure Test ==="),
    ct:log("Total requests: ~p", [NumRequests]),
    ct:log("Success: ~p, Errors: ~p, Timeouts: ~p", [SuccessCount, ErrorCount, TimeoutCount]),
    ct:log("Circuit breaker opened: ~p times", [length(CircuitBreakerOpened)]),
    ct:log("Final circuit breaker state: ~p", [FinalCBState]),
    ct:log("Recovery time: ~p ms", [RecoveryTime]),
    ct:log("Latency stats: P50=~p ms, P95=~p ms, P99=~p ms", 
           [Stats#chaos_stats.p50_latency_ms, Stats#chaos_stats.p95_latency_ms, 
            Stats#chaos_stats.p99_latency_ms]),
    ct:log("Metrics: ~p", [Metrics]),
    
    %% Expected behavior:
    %% - Early requests should succeed
    %% - Failure period: requests should fail, circuit breaker should open
    %% - Recovery period: circuit breaker should transition to half-open, then closed
    %% - Late requests should succeed
    ?assert(SuccessCount > 0, "Some requests should succeed (before failure and after recovery)"),
    ?assert(ErrorCount > 0, "Some requests should fail (during failure period)"),
    
    %% Success rate should reflect recovery
    ExpectedSuccessRate = ((FailureStart - 1) + (NumRequests - RecoveryStart + 1)) / NumRequests,
    ActualSuccessRate = SuccessCount / NumRequests,
    ?assert(ActualSuccessRate >= ExpectedSuccessRate * 0.8, "Success rate should reflect recovery"),
    
    ets:delete(RecoveryCounter),
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

%% Execute chaos test
execute_chaos_test(NumRequests, Policy) ->
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Execute requests
    Results = lists:map(fun(_N) ->
        StartTime = erlang:monotonic_time(microsecond),
        Request = create_route_request(<<"Test message">>, Policy),
        Result = router_decider:decide(Request, Policy, Context),
        EndTime = erlang:monotonic_time(microsecond),
        Latency = EndTime - StartTime,
        %% Small delay to avoid overwhelming system
        timer:sleep(5),
        {Result, Latency}
    end, lists:seq(1, NumRequests)),
    
    {Results, [Latency || {_, Latency} <- Results]}.

%% Analyze results
analyze_results(Results) ->
    {SuccessResults, ErrorResults} = lists:partition(fun({Result, _}) ->
        case Result of
            {ok, _} -> true;
            _ -> false
        end
    end, Results),
    
    SuccessCount = length(SuccessResults),
    ErrorCount = length(ErrorResults),
    
    %% Categorize errors
    TimeoutCount = length([R || {R, _} <- ErrorResults, 
                                case R of
                                    {error, {timeout, _}} -> true;
                                    {error, {extension_timeout, _}} -> true;
                                    _ -> false
                                end]),
    
    ClientErrors = [R || {R, _} <- ErrorResults],
    
    {SuccessCount, ErrorCount, TimeoutCount, ClientErrors}.

%% Calculate chaos statistics
calculate_chaos_statistics(Latencies) when length(Latencies) > 0 ->
    Sorted = lists:sort(Latencies),
    Length = length(Sorted),
    
    P50Idx = trunc(Length * 0.5),
    P95Idx = trunc(Length * 0.95),
    P99Idx = trunc(Length * 0.99),
    
    P50 = lists:nth(max(1, P50Idx), Sorted) / 1000,
    P95 = lists:nth(max(1, P95Idx), Sorted) / 1000,
    P99 = lists:nth(max(1, P99Idx), Sorted) / 1000,
    Max = lists:max(Sorted) / 1000,
    
    #chaos_stats{
        p50_latency_ms = P50,
        p95_latency_ms = P95,
        p99_latency_ms = P99,
        max_latency_ms = Max
    };
calculate_chaos_statistics([]) ->
    #chaos_stats{
        p50_latency_ms = 0,
        p95_latency_ms = 0,
        p99_latency_ms = 0,
        max_latency_ms = 0
    }.

%% Collect chaos metrics
collect_chaos_metrics(MetricsTable) when MetricsTable =/= undefined ->
    AllMetrics = ets:tab2list(MetricsTable),
    lists:map(fun({{ExtensionId, Status, ErrorReason}, Count, Latencies}) ->
        Sorted = lists:sort(Latencies),
        Length = length(Sorted),
        P50Idx = trunc(Length * 0.5),
        P95Idx = trunc(Length * 0.95),
        P99Idx = trunc(Length * 0.99),
        
        P50 = case Length > 0 of
            true -> lists:nth(max(1, P50Idx), Sorted) / 1000;
            false -> 0
        end,
        P95 = case Length > 0 of
            true -> lists:nth(max(1, P95Idx), Sorted) / 1000;
            false -> 0
        end,
        P99 = case Length > 0 of
            true -> lists:nth(max(1, P99Idx), Sorted) / 1000;
            false -> 0
        end,
        Avg = case Length > 0 of
            true -> lists:sum(Latencies) / Length / 1000;
            false -> 0
        end,
        
        #{
            extension_id => ExtensionId,
            status => Status,
            error_reason => ErrorReason,
            count => Count,
            p50_latency_ms => P50,
            p95_latency_ms => P95,
            p99_latency_ms => P99,
            avg_latency_ms => Avg
        }
    end, AllMetrics);
collect_chaos_metrics(_) ->
    [].

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

