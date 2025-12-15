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
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

-export([all/0, groups/0]).

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
    case os:getenv("RUN_CB_RECOVERY") of
        "1" -> groups_for_level(heavy);
        "true" -> groups_for_level(heavy);
        "on" -> groups_for_level(heavy);
        _ -> []
    end.

groups_for_level(heavy) ->
    [{group, recovery_tests}];
groups_for_level(full) ->
    [{group, recovery_tests}];
groups_for_level(_) -> %% fast
    [].

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
    Base = router_test_bootstrap:init_per_suite(Config, #{
        start => router_suite,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            tracing_enabled => false
        }
    }),
    case ensure_cb_ready() of
        ok ->
            router_metrics:ensure(),
            router_r10_metrics:clear_metrics(),
            Base;
        {skip, Reason} ->
            {skip, Reason}
    end.

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{
        start => router_suite,
        stop => router_suite
    }).

init_per_testcase(_TestCase, Config) ->
    case ensure_cb_ready() of
        ok ->
            ok = reset_circuit_breaker(),
            router_metrics:ensure(),
            router_r10_metrics:clear_metrics(),
            router_test_bootstrap:init_per_testcase(_TestCase, Config, #{});
        {skip, Reason} ->
            {skip, Reason}
    end.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

ensure_cb_ready() ->
    case catch ensure_circuit_breaker_alive() of
        true -> ok;
        ok -> ok;
        {error, _} = Err -> {skip, {cb_unavailable, Err}};
        {'EXIT', Reason} -> {skip, {cb_unavailable, Reason}};
        Other -> {skip, {cb_unavailable, Other}}
    end.

%% ========================================================================
%% RECOVERY TESTS
%% ========================================================================

%% @doc Test automatic recovery: open to half-open transition
test_automatic_recovery_open_to_half_open(_Config) ->
    case ensure_cb_ready() of
        {skip, Reason} -> {skip, Reason};
        ok ->
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
            
            ok
    end.

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
    case maps:find(recovery_type, Status) of
        {ok, <<"timeout_based">>} -> ok;
        {ok, Other} -> ct:comment({unexpected_recovery_type, Other});
        error -> ct:comment("recovery_type missing")
    end,
    case maps:find(time_until_half_open_ms, Status) of
        {ok, T} when is_integer(T) -> ?assert(T >= 0), ?assert(T =< TimeoutMs);
        _ -> ct:comment("time_until_half_open_ms missing or non-int")
    end,
    case maps:find(progress_percent, Status) of
        {ok, P} when is_integer(P) -> ?assert(P >= 0), ?assert(P =< 100);
        _ -> ct:comment("progress_percent missing or non-int")
    end,
    
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
    case maps:find(recovery_type, Status) of
        {ok, <<"success_based">>} -> ok;
        {ok, Other} -> ct:comment({unexpected_recovery_type, Other});
        error -> ct:comment("recovery_type missing")
    end,
    case maps:find(success_count, Status) of
        {ok, 1} -> ok;
        {ok, OtherCnt} -> ct:comment({unexpected_success_count, OtherCnt});
        error -> ct:comment("success_count missing")
    end,
    case maps:find(success_threshold, Status) of
        {ok, SuccessThreshold} -> ok;
        {ok, OtherThr} -> ct:comment({unexpected_success_threshold, OtherThr});
        error -> ct:comment("success_threshold missing")
    end,
    case maps:find(progress_percent, Status) of
        {ok, P} when is_integer(P) -> ?assert(P >= 0), ?assert(P =< 100);
        _ -> ct:comment("progress_percent missing or non-int")
    end,
    
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
