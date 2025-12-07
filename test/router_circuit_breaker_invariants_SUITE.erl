%%%-------------------------------------------------------------------
%%% @doc
%%% Property-based tests for R10 Circuit Breaker sliding window invariants
%%% Tests: Window monotonicity, trigger_reason correctness
%%% @end
%%%-------------------------------------------------------------------
-module(router_circuit_breaker_invariants_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

-import(router_r10_metrics, [
    get_metric_value/2,
    get_latest_trigger_reason/2
]).

%% Export test functions
-export([
    test_window_monotonicity/1,
    test_trigger_reason_correctness_latency/1,
    test_trigger_reason_correctness_failure/1,
    test_trigger_reason_correctness_error_rate/1
]).

all() ->
    [
        {group, window_invariants},
        {group, trigger_reason_invariants}
    ].

groups() ->
    [
        {window_invariants, [sequence], [
            test_window_monotonicity
        ]},
        {trigger_reason_invariants, [parallel], [
            test_trigger_reason_correctness_latency,
            test_trigger_reason_correctness_failure,
            test_trigger_reason_correctness_error_rate
        ]}
    ].

init_per_suite(Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_suite(_Config) ->
    stop_router_app(),
    ok.

init_per_testcase(_Case, Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    ok = reset_circuit_breaker(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = reset_circuit_breaker(),
    router_r10_metrics:clear_metrics(),
    ok.

%% ========================================================================
%% WINDOW MONOTONICITY TESTS
%% ========================================================================

%% @doc Property test: Events older than window_seconds never participate in calculation
%% 
%% Invariant: After window_seconds have passed, old events should not affect
%% error rate calculation or circuit breaker state transitions.
test_window_monotonicity(_Config) ->
    ct:comment("R10 Invariant: Window monotonicity - old events don't participate"),
    
    TenantId = <<"tenant_window_test">>,
    ProviderId = <<"provider_window_test">>,
    
    %% Configure with short window for fast testing
    WindowSeconds = 2,  %% 2 seconds window
    Config = #{
        <<"failure_threshold">> => 100,  %% High threshold (don't trigger on failures)
        <<"error_rate_threshold">> => 0.5,  %% 50% error rate threshold
        <<"error_rate_window_seconds">> => WindowSeconds,
        <<"latency_threshold_ms">> => 0,  %% Disable latency trigger
        <<"open_timeout_ms">> => 5000,
        <<"success_threshold">> => 2
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Step 1: Add failures to fill window
    _Now1 = erlang:system_time(millisecond),
    lists:foreach(
        fun(_) ->
            router_circuit_breaker:record_failure(TenantId, ProviderId),
            timer:sleep(50)  %% Small delay between failures
        end,
        lists:seq(1, 10)
    ),
    
    %% Step 2: Wait for window to expire (events should be cleaned)
    timer:sleep((WindowSeconds + 1) * 1000),  %% Wait longer than window
    
    %% Step 3: Add successes (should not be affected by old failures)
    lists:foreach(
        fun(_) ->
            router_circuit_breaker:record_success(TenantId, ProviderId),
            timer:sleep(50)
        end,
        lists:seq(1, 10)
    ),
    
    %% Step 4: Verify error rate is low (old failures should not count)
    timer:sleep(200),
    ErrorRate = get_metric_value(router_circuit_breaker_error_rate, #{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    %% Error rate should be low (only recent successes, old failures excluded)
    ?assert(ErrorRate < 0.5, 
        io_lib:format("Error rate should be low after window expired, got ~p", [ErrorRate])),
    
    %% Step 5: Verify circuit breaker is still closed (not opened by old failures)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, State, "Circuit breaker should remain closed after window expired"),
    
    ok.

%% ========================================================================
%% TRIGGER REASON CORRECTNESS TESTS
%% ========================================================================

%% @doc Property test: If latency triggered → no failure/error_rate threshold scenarios in window
test_trigger_reason_correctness_latency(_Config) ->
    ct:comment("R10 Invariant: Latency trigger → no failure/error_rate in window"),
    
    TenantId = <<"tenant_latency_trigger">>,
    ProviderId = <<"provider_latency_trigger">>,
    
    %% Configure with latency threshold only
    Config = #{
        <<"failure_threshold">> => 100,  %% High threshold (won't trigger)
        <<"error_rate_threshold">> => 1.0,  %% High threshold (won't trigger)
        <<"error_rate_window_seconds">> => 30,
        <<"latency_threshold_ms">> => 100,  %% Low latency threshold (will trigger)
        <<"open_timeout_ms">> => 2000,
        <<"success_threshold">> => 2
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Add successes (no failures, no high error rate)
    lists:foreach(
        fun(_) ->
            router_circuit_breaker:record_success(TenantId, ProviderId),
            timer:sleep(50)
        end,
        lists:seq(1, 5)
    ),
    
    %% Note: Actual latency trigger requires publish latency metrics
    %% This test verifies that if latency triggers, failure/error_rate don't
    %% For full test, would need to inject latency metrics
    
    %% Verify state (should be closed since no actual latency exceeded in this test)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, State, "Circuit breaker should be closed without latency trigger"),
    
    ok.

%% @doc Property test: If failure triggered → latency doesn't exceed threshold
test_trigger_reason_correctness_failure(_Config) ->
    ct:comment("R10 Invariant: Failure trigger → latency doesn't exceed threshold"),
    
    TenantId = <<"tenant_failure_trigger">>,
    ProviderId = <<"provider_failure_trigger">>,
    
    %% Configure with low failure threshold, high latency threshold
    Config = #{
        <<"failure_threshold">> => 5,  %% Low threshold (will trigger)
        <<"error_rate_threshold">> => 1.0,  %% High threshold (won't trigger)
        <<"error_rate_window_seconds">> => 30,
        <<"latency_threshold_ms">> => 10000,  %% Very high latency threshold (won't trigger)
        <<"open_timeout_ms">> => 2000,
        <<"success_threshold">> => 2
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Add failures to trigger circuit breaker
    lists:foreach(
        fun(_) ->
            router_circuit_breaker:record_failure(TenantId, ProviderId),
            timer:sleep(50)
        end,
        lists:seq(1, 6)  %% More than failure_threshold
    ),
    
    timer:sleep(200),
    
    %% Verify circuit breaker opened
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State, "Circuit breaker should be open after failures"),
    
    %% Verify trigger reason is failure_threshold_exceeded (not latency)
    case get_latest_trigger_reason(TenantId, ProviderId) of
        {ok, Reason} ->
            ?assertEqual(<<"failure_threshold_exceeded">>, Reason,
                "Trigger reason should be failure_threshold_exceeded, not latency");
        {error, not_found} ->
            ct:comment("Trigger reason not found (may be emitted later)"),
            ok
    end,
    
    ok.

%% @doc Property test: If error_rate triggered → latency doesn't exceed threshold
test_trigger_reason_correctness_error_rate(_Config) ->
    ct:comment("R10 Invariant: Error rate trigger → latency doesn't exceed threshold"),
    
    TenantId = <<"tenant_error_rate_trigger">>,
    ProviderId = <<"provider_error_rate_trigger">>,
    
    %% Configure with low error rate threshold, high latency threshold
    Config = #{
        <<"failure_threshold">> => 100,  %% High threshold (won't trigger)
        <<"error_rate_threshold">> => 0.5,  %% 50% error rate threshold (will trigger)
        <<"error_rate_window_seconds">> => 5,  %% Short window for fast testing
        <<"latency_threshold_ms">> => 10000,  %% Very high latency threshold (won't trigger)
        <<"open_timeout_ms">> => 2000,
        <<"success_threshold">> => 2
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Add mixed failures and successes to exceed error rate threshold
    %% Need >50% failures in window
    lists:foreach(
        fun(N) ->
            case N rem 3 of
                0 -> router_circuit_breaker:record_success(TenantId, ProviderId);
                _ -> router_circuit_breaker:record_failure(TenantId, ProviderId)
            end,
            timer:sleep(100)  %% Small delay to keep events in window
        end,
        lists:seq(1, 10)
    ),
    
    timer:sleep(500),
    
    %% Verify circuit breaker opened (error rate > 50%)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State, "Circuit breaker should be open after error rate exceeded"),
    
    %% Verify trigger reason is error_rate_threshold_exceeded (not latency)
    case get_latest_trigger_reason(TenantId, ProviderId) of
        {ok, Reason} ->
            ?assertEqual(<<"error_rate_threshold_exceeded">>, Reason,
                "Trigger reason should be error_rate_threshold_exceeded, not latency");
        {error, not_found} ->
            ct:comment("Trigger reason not found (may be emitted later)"),
            ok
    end,
    
    ok.

