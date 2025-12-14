%% @doc Circuit Breaker - State Transitions Tests
%% 
%% Tests for circuit breaker half-open transitions and closing.
%% Runs with ROUTER_TEST_LEVEL=full or heavy.
%%
%% @test_category circuit_breaker, unit, r10
-module(router_circuit_breaker_transitions_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_half_open_after_timeout/1,
    test_closes_after_success_threshold/1,
    test_reopens_on_half_open_failure/1,
    test_no_badmatch_regression/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, transition_tests}];
        "heavy" -> [{group, transition_tests}];
        _ -> []
    end.

groups() ->
    [{transition_tests, [sequence], [
        test_half_open_after_timeout,
        test_closes_after_success_threshold,
        test_reopens_on_half_open_failure,
        test_no_badmatch_regression
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

open_circuit(TenantId, ProviderId, Config) ->
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    timer:sleep(100).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_half_open_after_timeout(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2
    },
    
    open_circuit(TenantId, ProviderId, Config),
    
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    timer:sleep(TimeoutMs + 100),
    
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    ok.

test_closes_after_success_threshold(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    SuccessThreshold = 2,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => SuccessThreshold
    },
    
    open_circuit(TenantId, ProviderId, Config),
    
    timer:sleep(1100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, SuccessThreshold)),
    
    timer:sleep(100),
    
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    ok.

test_reopens_on_half_open_failure(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2
    },
    
    open_circuit(TenantId, ProviderId, Config),
    
    timer:sleep(1100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    ok.

test_no_badmatch_regression(_Config) ->
    verify_cb_alive(),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2
    },
    
    open_circuit(TenantId, ProviderId, Config),
    
    timer:sleep(1100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% This should NOT cause badmatch
    try
        router_circuit_breaker:record_failure(TenantId, ProviderId),
        timer:sleep(100),
        {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
        ?assertEqual(open, OpenState),
        ok
    catch
        error:{badmatch, _} = BadmatchError ->
            ct:fail({regression_badmatch_bug, BadmatchError})
    end.
