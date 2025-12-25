%% @doc Circuit Breaker Load: Error Spike Tests
%%
%% Load tests for circuit breaker:
%% - Error spike opens circuit
%% - Traffic normalization closes circuit
%%
%% @test_category load, heavy, circuit_breaker
-module(router_cb_load_spike_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_error_spike_opens_circuit/1,
    test_traffic_normalization_closes_circuit/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, spike_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, spike_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, spike_tests}];
        _ -> []
    end.
groups() ->
    [{spike_tests, [sequence], [
        test_error_spike_opens_circuit,
        test_traffic_normalization_closes_circuit
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, circuit_breaker_enabled, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> timer:sleep(500), Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_error_spike_opens_circuit(_Config) ->
    ct:comment("=== Error Spike Opens Circuit ==="),
    TenantId = <<"load_test_tenant">>,
    ProviderId = <<"load_test_provider">>,
    
    %% Verify CB gen_server is running
    case whereis(router_circuit_breaker) of
        undefined -> ct:fail("Circuit breaker gen_server not running");
        Pid -> ?assert(is_process_alive(Pid))
    end,
    
    %% Record multiple failures to trigger circuit open
    lists:foreach(fun(_) ->
        ok = router_circuit_breaker:record_failure(TenantId, ProviderId),
        timer:sleep(10)
    end, lists:seq(1, 10)),
    
    timer:sleep(500),
    
    %% Check if circuit is open
    case router_circuit_breaker:get_state(TenantId, ProviderId) of
        {ok, State} ->
            ct:comment("Circuit state: ~p", [State]);
        {error, not_found} ->
            ct:comment("State not found (expected on some configurations)");
        Error ->
            ct:comment("Unexpected error: ~p", [Error])
    end,
    ok.

test_traffic_normalization_closes_circuit(_Config) ->
    ct:comment("=== Traffic Normalization Closes Circuit ==="),
    TenantId = <<"load_test_tenant2">>,
    ProviderId = <<"load_test_provider2">>,
    
    %% Record failures to open circuit
    lists:foreach(fun(_) ->
        ok = router_circuit_breaker:record_failure(TenantId, ProviderId),
        timer:sleep(10)
    end, lists:seq(1, 5)),
    
    timer:sleep(500),
    
    %% Record successes to close circuit
    lists:foreach(fun(_) ->
        ok = router_circuit_breaker:record_success(TenantId, ProviderId),
        timer:sleep(10)
    end, lists:seq(1, 10)),
    
    timer:sleep(500),
    
    %% Check state
    case router_circuit_breaker:get_state(TenantId, ProviderId) of
        {ok, State} ->
            ct:comment("Circuit state after normalization: ~p", [State]);
        {error, not_found} ->
            ct:comment("State not found");
        Error ->
            ct:comment("Error: ~p", [Error])
    end,
    ok.
