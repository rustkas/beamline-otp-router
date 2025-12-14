%% @doc Publish Failure E2E - Mass Failure Tests
%% 
%% Tests for mass failure scenarios and breaker behavior.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category e2e, r10, heavy
-module(router_publish_failure_mass_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    ensure_router_nats_alive/0,
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
    scenario_mass_failure_opens_breaker/1,
    scenario_recovery_after_failure/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, mass_failure_tests}];
        _ -> []
    end.

groups() ->
    [{mass_failure_tests, [sequence], [
        scenario_mass_failure_opens_breaker,
        scenario_recovery_after_failure
    ]}].

init_per_suite(Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = ensure_router_nats_alive(),
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

scenario_mass_failure_opens_breaker(_Config) ->
    TenantId = <<"tenant-mass">>,
    ProviderId = <<"provider-mass">>,
    FailureThreshold = 5,
    
    Config = #{
        <<"failure_threshold">> => FailureThreshold,
        <<"error_rate_threshold">> => 2.0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Enable fault injection
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Simulate mass failures
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId),
        timer:sleep(50)
    end, lists:seq(1, FailureThreshold)),
    
    timer:sleep(200),
    
    %% Verify breaker is open
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    router_nats_fault_injection:clear_all_faults(),
    ok.

scenario_recovery_after_failure(_Config) ->
    TenantId = <<"tenant-recovery">>,
    ProviderId = <<"provider-recovery">>,
    TimeoutMs = 1000,
    SuccessThreshold = 2,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"success_threshold">> => SuccessThreshold,
        <<"half_open_max_calls">> => 3
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open breaker
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(TimeoutMs + 100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Record successes to close
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, SuccessThreshold)),
    
    timer:sleep(100),
    
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    ok.
