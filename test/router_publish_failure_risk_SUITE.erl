%% @doc Publish Failure E2E - Risk Scenarios Tests
%% 
%% Tests for latency, error rate, and risk scenarios.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category e2e, r10, heavy
-module(router_publish_failure_risk_SUITE).
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
    scenario_latency_based_trigger/1,
    scenario_error_rate_partial_failure/1,
    scenario_thundering_herd_recovery/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, risk_scenario_tests}];
        _ -> []
    end.

groups() ->
    [{risk_scenario_tests, [parallel], [
        scenario_latency_based_trigger,
        scenario_error_rate_partial_failure,
        scenario_thundering_herd_recovery
    ]}].

init_per_suite(Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_suite(_Config) ->
    router_nats_fault_injection:clear_all_faults(),
    stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = reset_circuit_breaker(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

scenario_latency_based_trigger(_Config) ->
    TenantId = <<"tenant-latency">>,
    ProviderId = <<"provider-latency">>,
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

scenario_error_rate_partial_failure(_Config) ->
    TenantId = <<"tenant-error-rate">>,
    ProviderId = <<"provider-error-rate">>,
    ErrorRateThreshold = 0.5,  %% 50%
    
    Config = #{
        <<"failure_threshold">> => 100,
        <<"error_rate_threshold">> => ErrorRateThreshold,
        <<"error_rate_window_seconds">> => 30
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% 60% error rate
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

scenario_thundering_herd_recovery(_Config) ->
    TenantId = <<"tenant-herd">>,
    ProviderId = <<"provider-herd">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open breaker
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Wait for timeout
    timer:sleep(1100),
    
    %% Simulate multiple concurrent requests trying to recover
    lists:foreach(fun(_) ->
        _ = router_circuit_breaker:should_allow(TenantId, ProviderId)
    end, lists:seq(1, 10)),
    
    timer:sleep(100),
    
    %% Verify still in controlled state (not crashed)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(State =:= half_open orelse State =:= open),
    ok.
