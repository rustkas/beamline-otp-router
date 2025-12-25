%% @doc Circuit Breaker - Opening Tests
%% 
%% Tests for circuit breaker opening on different thresholds.
%% Runs with ROUTER_TEST_LEVEL=full or heavy.
%%
%% @test_category circuit_breaker, unit, r10
-module(router_circuit_breaker_open_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_opens_on_failure_threshold/1,
    test_opens_on_error_rate_threshold/1,
    test_opens_on_latency_threshold/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, open_tests}];
        "heavy" -> [{group, open_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, open_tests}];
        "heavy" -> [{group, open_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, open_tests}];
        "heavy" -> [{group, open_tests}];
        _ -> []
    end.
groups() ->
    [{open_tests, [sequence], [
        test_opens_on_failure_threshold,
        test_opens_on_error_rate_threshold,
        test_opens_on_latency_threshold
    ]}].

init_per_suite(Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_suite(_Config) ->
    stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = reset_circuit_breaker(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, Config) -> Config.

verify_cb_alive() ->
    case whereis(router_circuit_breaker) of
        undefined ->
            ok = start_router_app(),
            timer:sleep(200),
            ok = ensure_circuit_breaker_alive();
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ok;
                false -> ct:fail({circuit_breaker_dead, pid, Pid})
            end
    end.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_opens_on_failure_threshold(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    FailureThreshold = 5,
    
    Config = #{
        <<"failure_threshold">> => FailureThreshold,
        <<"error_rate_threshold">> => 2.0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, FailureThreshold)),
    
    timer:sleep(100),
    
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    ok.

test_opens_on_error_rate_threshold(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    ErrorRateThreshold = 0.5,
    
    Config = #{
        <<"failure_threshold">> => 100,
        <<"error_rate_threshold">> => ErrorRateThreshold,
        <<"error_rate_window_seconds">> => 30
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Record 6 failures and 4 successes (60% error rate)
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 6)),
    
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 4)),
    
    timer:sleep(200),
    
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    ok.

test_opens_on_latency_threshold(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    LatencyThreshold = 5000,  %% 5 seconds
    
    Config = #{
        <<"failure_threshold">> => 100,
        <<"error_rate_threshold">> => 1.0,
        <<"latency_threshold_ms">> => LatencyThreshold
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    router_metrics:ensure(),
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 6.0}, #{}),
    
    timer:sleep(50),
    
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    timer:sleep(200),
    
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    ok.
