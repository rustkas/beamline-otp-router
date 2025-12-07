%%%-------------------------------------------------------------------
%%% @doc
%%% Mini-Randomization E2E Test Suite for R10
%%% 
%%% Purpose: Catch hidden timing dependencies without creating new scenarios
%%% - Takes random profile (ci/heavy) within reasonable bounds
%%% - Randomizes NumClients and RequestsPerClient in narrow range (±20% from base)
%%% - Runs existing scenarios with randomization
%%% @end
%%%-------------------------------------------------------------------
-module(router_publish_failure_e2e_randomized_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1]}).

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    ensure_router_nats_alive/0,
    reset_circuit_breaker/0,
    wait_for_breaker_state/4
]).

%% Export test functions
-export([
    scenario_mass_failure_randomized/1,
    scenario_recovery_randomized/1
]).

all() ->
    [
        {group, randomized_scenarios}
    ].

groups() ->
    [
        {randomized_scenarios, [parallel], [
            scenario_mass_failure_randomized,
            scenario_recovery_randomized
        ]}
    ].

init_per_suite(Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = ensure_router_nats_alive(),
    router_metrics:ensure(),
    {ok, _PrunedCount} = router_r10_metrics:prune_old_test_metrics(5),
    router_r10_metrics:clear_metrics(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_suite(_Config) ->
    router_nats_fault_injection:clear_all_faults(),
    stop_router_app(),
    ok.

init_per_testcase(_Case, Config) ->
    ok = start_router_app(),
    timer:sleep(200),
    ok = ensure_circuit_breaker_alive(),
    ok = ensure_router_nats_alive(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    router_nats_fault_injection:clear_all_faults(),
    ok = reset_circuit_breaker(),
    %% Set timetrap for randomized scenarios (longer timeout)
    [{timetrap, {seconds, 60}} | Config].

end_per_testcase(_Case, _Config) ->
    router_nats_fault_injection:clear_all_faults(),
    ok = reset_circuit_breaker(),
    router_r10_metrics:clear_metrics(),
    ok.

%% ========================================================================
%% RANDOMIZED SCENARIOS
%% ========================================================================

%% @doc Randomized version of scenario_mass_failure_opens_breaker
%% Randomizes profile (ci/heavy) and client/request counts within ±20%
scenario_mass_failure_randomized(_Config) ->
    ct:comment("R10 Randomized: Mass failure → breaker opens"),
    
    %% Randomize profile
    Profile = random_profile(),
    ct:comment("Using randomized profile: ~p", [Profile]),
    
    %% Get base values from profile
    {BaseNumClients, BaseRequestsPerClient} = case Profile of
        ci -> {10, 20};
        heavy -> {50, 100}
    end,
    
    %% Randomize within ±20%
    NumClients = randomize_value(BaseNumClients, 0.2),
    RequestsPerClient = randomize_value(BaseRequestsPerClient, 0.2),
    
    ct:comment("Randomized: NumClients=~p (base=~p), RequestsPerClient=~p (base=~p)",
        [NumClients, BaseNumClients, RequestsPerClient, BaseRequestsPerClient]),
    
    %% Ensure circuit breaker is alive
    ok = ensure_circuit_breaker_alive(),
    
    %% Unique tenant/provider for this scenario
    TenantId = <<"tenant_r10_random_s1">>,
    ProviderId = <<"provider_r10_random_s1">>,
    
    %% Configure circuit breaker
    BreakerConfig = #{
        <<"failure_threshold">> => 10,
        <<"timeout_ms">> => 2000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, BreakerConfig),
    
    %% Warmup
    warmup_publishes(20),
    
    %% Enable fault injection
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    
    %% Spawn clients with randomized counts
    ClientPids = spawn_clients(NumClients, RequestsPerClient),
    timer:sleep(500),
    
    %% Wait for breaker to open
    ok = wait_for_breaker_state(TenantId, ProviderId, open, 5000),
    wait_for_clients(ClientPids, 10000),
    
    %% Verify breaker is open
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State, "Circuit breaker should be open"),
    
    %% Verify should_allow returns error
    ?assertMatch({error, circuit_open}, router_circuit_breaker:should_allow(TenantId, ProviderId)),
    
    ok.

%% @doc Randomized version of scenario_recovery_after_failure
%% Randomizes profile (ci/heavy) and client/request counts within ±20%
scenario_recovery_randomized(_Config) ->
    _ = {timetrap, {seconds, 60}},
    ct:comment("R10 Randomized: Recovery after failure"),
    
    %% Randomize profile
    Profile = random_profile(),
    ct:comment("Using randomized profile: ~p", [Profile]),
    
    %% Get base values from profile
    {BaseNumClients, BaseRequestsPerClient} = case Profile of
        ci -> {10, 20};
        heavy -> {50, 100}
    end,
    
    %% Randomize within ±20%
    NumClients = randomize_value(BaseNumClients, 0.2),
    RequestsPerClient = randomize_value(BaseRequestsPerClient, 0.2),
    
    ct:comment("Randomized: NumClients=~p (base=~p), RequestsPerClient=~p (base=~p)",
        [NumClients, BaseNumClients, RequestsPerClient, BaseRequestsPerClient]),
    
    %% Ensure circuit breaker is alive
    ok = ensure_circuit_breaker_alive(),
    
    %% Unique tenant/provider
    TenantId = <<"tenant_r10_random_s2">>,
    ProviderId = <<"provider_r10_random_s2">>,
    
    %% Configure circuit breaker
    BreakerConfig = #{
        <<"failure_threshold">> => 10,
        <<"timeout_ms">> => 2000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, BreakerConfig),
    
    %% Warmup
    warmup_publishes(20),
    
    %% Enable fault injection
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    
    %% Spawn clients with randomized counts
    ClientPids = spawn_clients(NumClients, RequestsPerClient),
    timer:sleep(500),
    
    %% Wait for breaker to open
    ok = wait_for_breaker_state(TenantId, ProviderId, open, 5000),
    wait_for_clients(ClientPids, 10000),
    
    %% Verify breaker is open
    {ok, open} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Disable fault injection
    ok = router_nats_fault_injection:disable_fault(publish),
    
    %% Wait for timeout → half_open
    timer:sleep(2500),
    
    %% Wait for recovery → closed
    ok = wait_for_breaker_state(TenantId, ProviderId, closed, 10000),
    
    %% Verify breaker is closed
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, State, "Circuit breaker should be closed after recovery"),
    
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Randomly choose profile (ci or heavy)
random_profile() ->
    case rand:uniform(2) of
        1 -> ci;
        2 -> heavy
    end.

%% @doc Randomize value within ±percent range
randomize_value(Base, Percent) ->
    %% Calculate range: Base ± (Base * Percent)
    Min = max(1, trunc(Base * (1 - Percent))),
    Max = trunc(Base * (1 + Percent)),
    Min + rand:uniform(Max - Min + 1) - 1.

%% @doc Warmup with normal publishes
warmup_publishes(Count) ->
    Subject = <<"test.subject">>,
    Payload = <<"warmup">>,
    lists:foreach(
        fun(_) ->
            _ = router_nats:publish(Subject, Payload),
            timer:sleep(10)
        end,
        lists:seq(1, Count)
    ),
    ok.

%% @doc Spawn parallel clients
spawn_clients(NumClients, RequestsPerClient) ->
    Subject = <<"test.subject">>,
    Payload = <<"test">>,
    lists:map(
        fun(_) ->
            spawn(fun() ->
                lists:foreach(
                    fun(_) ->
                        _ = router_nats:publish(Subject, Payload),
                        timer:sleep(10)
                    end,
                    lists:seq(1, RequestsPerClient)
                )
            end)
        end,
        lists:seq(1, NumClients)
    ).

%% @doc Wait for all clients to complete
wait_for_clients(ClientPids, TimeoutMs) ->
    StartTime = erlang:system_time(millisecond),
    wait_for_clients_internal(ClientPids, StartTime, TimeoutMs).

wait_for_clients_internal([], _StartTime, _TimeoutMs) ->
    ok;
wait_for_clients_internal(ClientPids, StartTime, TimeoutMs) ->
    Elapsed = erlang:system_time(millisecond) - StartTime,
    case Elapsed >= TimeoutMs of
        true ->
            ct:comment("Timeout waiting for clients, ~p still running", [length(ClientPids)]),
            ok;
        false ->
            AlivePids = [Pid || Pid <- ClientPids, is_process_alive(Pid)],
            case AlivePids of
                [] -> ok;
                _ ->
                    timer:sleep(100),
                    wait_for_clients_internal(AlivePids, StartTime, TimeoutMs)
            end
    end.

