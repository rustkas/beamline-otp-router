%% @doc Circuit Breaker Recovery Test Suite
%% 
%% Comprehensive test coverage for circuit breaker recovery scenarios:
%% - Automatic recovery (timeout-based transitions)
%% - Manual recovery (force recovery functions)
%% - Recovery status monitoring
%% - Recovery state reset
%%
%% @test_category unit, circuit_breaker, recovery
-module(router_circuit_breaker_recovery_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

%% Export test functions for Common Test
-export([
    test_automatic_recovery_open_to_half_open/1,
    test_automatic_recovery_half_open_to_closed/1,
    test_force_recovery_open_to_closed/1,
    test_force_recovery_open_to_half_open/1,
    test_get_recovery_status_open/1,
    test_get_recovery_status_half_open/1,
    test_reset_recovery_state/1,
    test_recovery_after_multiple_failures/1
]).

all() ->
    [
        {group, recovery_tests}
    ].

groups() ->
    [
        {recovery_tests, [sequence], [
            test_automatic_recovery_open_to_half_open,
            test_automatic_recovery_half_open_to_closed,
            test_force_recovery_open_to_closed,
            test_force_recovery_open_to_half_open,
            test_get_recovery_status_open,
            test_get_recovery_status_half_open,
            test_reset_recovery_state,
            test_recovery_after_multiple_failures
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

init_per_testcase(_TestCase, Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = reset_circuit_breaker(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ========================================================================
%% RECOVERY TESTS
%% ========================================================================

%% @doc Test automatic recovery: open to half-open transition
test_automatic_recovery_open_to_half_open(_Config) ->
    ct:comment("Testing automatic recovery: open to half-open"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Wait for timeout
    timer:sleep(TimeoutMs + 100),
    
    %% Trigger state check
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Verify half-open
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    ok.

%% @doc Test automatic recovery: half-open to closed transition
test_automatic_recovery_half_open_to_closed(_Config) ->
    ct:comment("Testing automatic recovery: half-open to closed"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,
    SuccessThreshold = 2,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => SuccessThreshold,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(TimeoutMs + 100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Record successes to meet threshold
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, SuccessThreshold)),
    
    timer:sleep(100),
    
    %% Verify closed
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    ok.

%% @doc Test force recovery: open to closed
test_force_recovery_open_to_closed(_Config) ->
    ct:comment("Testing force recovery: open to closed"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 60000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Force recovery
    ok = router_circuit_breaker:force_recovery(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Verify closed
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    ok.

%% @doc Test force recovery: open to half-open
test_force_recovery_open_to_half_open(_Config) ->
    ct:comment("Testing force recovery: open to half-open"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 60000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Force recovery to half-open
    ok = router_circuit_breaker:force_recovery_to_half_open(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Verify half-open
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    ok.

%% @doc Test get recovery status: open state
test_get_recovery_status_open(_Config) ->
    ct:comment("Testing get recovery status: open state"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 5000,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    
    %% Get recovery status
    {ok, Status} = router_circuit_breaker:get_recovery_status(TenantId, ProviderId),
    ?assertEqual(open, maps:get(state, Status)),
    ?assertEqual(<<"timeout_based">>, maps:get(recovery_type, Status)),
    ?assert(maps:get(time_until_half_open_ms, Status) >= 0),
    ?assert(maps:get(time_until_half_open_ms, Status) =< TimeoutMs),
    ?assert(maps:get(progress_percent, Status) >= 0),
    ?assert(maps:get(progress_percent, Status) =< 100),
    
    ok.

%% @doc Test get recovery status: half-open state
test_get_recovery_status_half_open(_Config) ->
    ct:comment("Testing get recovery status: half-open state"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,
    SuccessThreshold = 2,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => SuccessThreshold,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(TimeoutMs + 100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Record one success
    router_circuit_breaker:record_success(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Get recovery status
    {ok, Status} = router_circuit_breaker:get_recovery_status(TenantId, ProviderId),
    ?assertEqual(half_open, maps:get(state, Status)),
    ?assertEqual(<<"success_based">>, maps:get(recovery_type, Status)),
    ?assertEqual(1, maps:get(success_count, Status)),
    ?assertEqual(SuccessThreshold, maps:get(success_threshold, Status)),
    ?assert(maps:get(progress_percent, Status) >= 0),
    ?assert(maps:get(progress_percent, Status) =< 100),
    
    ok.

%% @doc Test reset recovery state
test_reset_recovery_state(_Config) ->
    ct:comment("Testing reset recovery state"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 60000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Reset recovery state
    ok = router_circuit_breaker:reset_recovery_state(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Verify closed
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    ok.

%% @doc Test recovery after multiple failures
test_recovery_after_multiple_failures(_Config) ->
    ct:comment("Testing recovery after multiple failures"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,
    SuccessThreshold = 2,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => SuccessThreshold,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit with multiple failures
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 10)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(TimeoutMs + 100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Record successes to meet threshold
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, SuccessThreshold)),
    
    timer:sleep(100),
    
    %% Verify closed
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    ok.

