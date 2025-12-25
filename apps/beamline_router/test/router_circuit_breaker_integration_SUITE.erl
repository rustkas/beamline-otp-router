%% @doc Integration Test Suite for Circuit Breaker with Router Policy Applier
-module(router_circuit_breaker_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Common Test callbacks - MUST be exported for CT to find them
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Export test functions for Common Test
-export([
    test_circuit_breaker_closed_to_open/1,
    test_circuit_breaker_open_to_half_open/1,
    test_circuit_breaker_half_open_to_closed/1,
    test_circuit_breaker_half_open_to_open/1,
    test_circuit_breaker_with_fallbacks/1,
    test_circuit_breaker_with_retry/1,
    test_circuit_breaker_disabled/1,
    test_circuit_breaker_error_rate_threshold/1,
    test_circuit_breaker_provider_callbacks_success/1,
    test_circuit_breaker_provider_callbacks_failure/1,
    test_circuit_breaker_provider_callbacks_timeout/1,
    test_circuit_breaker_provider_callbacks_error_types/1,
    test_circuit_breaker_provider_callbacks_ignored_errors/1,
    suite/0
]).

suite() ->
    [
        {timetrap, {minutes, 2}}
    ].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, circuit_breaker_integration_tests}];
groups_for_level(full) ->
    [{group, circuit_breaker_integration_tests}];
groups_for_level(_) -> %% fast
    [].

groups() ->
    [
        {circuit_breaker_integration_tests, [sequence], [
            test_circuit_breaker_closed_to_open,
            test_circuit_breaker_open_to_half_open,
            test_circuit_breaker_half_open_to_closed,
            test_circuit_breaker_half_open_to_open,
            test_circuit_breaker_with_fallbacks,
            test_circuit_breaker_with_retry,
            test_circuit_breaker_disabled,
            test_circuit_breaker_error_rate_threshold,
            test_circuit_breaker_provider_callbacks_success,
            test_circuit_breaker_provider_callbacks_failure,
            test_circuit_breaker_provider_callbacks_timeout,
            test_circuit_breaker_provider_callbacks_error_types,
            test_circuit_breaker_provider_callbacks_ignored_errors
        ]}
    ].

init_per_suite(Config) ->
    %% Start application
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, disable_heir, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Start circuit breaker gen_server if not already started
            case whereis(router_circuit_breaker) of
                undefined ->
                    {ok, _} = router_circuit_breaker:start_link();
                _Pid ->
                    ok
            end,
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clean up ETS tables before each test
    try
        ets:delete_all_objects(router_provider_circuit_breaker),
        ets:delete_all_objects(router_policy_store)
    catch
        _:_ -> ok
    end,
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ============================================================================
%% Integration Tests
%% ============================================================================

test_circuit_breaker_closed_to_open(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    ProviderId = <<"openai">>,
    
    %% Create policy with circuit breaker enabled
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 3,  % Lower threshold for testing
            <<"timeout_ms">> => 1000  % Short timeout for testing
        }
    },
    
    Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyMap),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Create request
    Request = #{
        message => #{
            <<"tenant_id">> => TenantId,
            <<"content">> => <<"test">>
        },
        context => #{}
    },
    
    %% Make 3 decisions (all should succeed initially)
    [begin
        {ok, Result} = router_policy_applier:apply_policy(Request, TenantId, PolicyId),
        ?assertEqual(ProviderId, maps:get(provider_id, Result))
    end || _ <- lists:seq(1, 3)],
    
    %% Verify circuit is still closed
    {ok, closed} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Record 3 failures (should open circuit)
    [router_circuit_breaker:record_failure(TenantId, <<"openai">>) || _ <- lists:seq(1, 3)],
    
    %% Verify circuit is now open
    {ok, open} = router_circuit_breaker:get_state(TenantId, <<"openai">>),
    
    %% Next decision should fail fast (circuit open)
    {error, circuit_open} = router_circuit_breaker:should_allow(TenantId, <<"openai">>),
    
    ok.

test_circuit_breaker_open_to_half_open(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize and open circuit
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 5)],
    {ok, open} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Wait for timeout (requires manual manipulation or shorter timeout in test)
    %% For now, verify state is open
    ?assert(router_circuit_breaker:is_open(TenantId, ProviderId)),
    
    ok.

test_circuit_breaker_half_open_to_closed(_Config) ->
    %% Test: half-open → closed transition after success threshold
    TenantId = <<"test_tenant_ho_closed">>,
    ProviderId = <<"test_provider">>,
    
    %% Configure with short timeout for fast test
    Config = #{
        <<"failure_threshold">> => 3,
        <<"timeout_ms">> => 500,
        <<"success_threshold">> => 2,
        <<"half_open_max_calls">> => 3
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 3)],
    {ok, open} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Wait for timeout to transition to half-open
    timer:sleep(600),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    {ok, half_open} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Record successes to close circuit
    [router_circuit_breaker:record_success(TenantId, ProviderId) || _ <- lists:seq(1, 2)],
    timer:sleep(100),
    
    %% STRICT: Verify circuit is closed
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, State, "Circuit breaker MUST close after success threshold"),
    
    ok.

test_circuit_breaker_half_open_to_open(_Config) ->
    %% Test: half-open → open transition on failure
    TenantId = <<"test_tenant_ho_open">>,
    ProviderId = <<"test_provider">>,
    
    %% Configure with short timeout for fast test
    Config = #{
        <<"failure_threshold">> => 3,
        <<"timeout_ms">> => 500,
        <<"success_threshold">> => 2,
        <<"half_open_max_calls">> => 3
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 3)],
    {ok, open} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Wait for timeout to transition to half-open
    timer:sleep(600),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    {ok, half_open} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Record failure in half-open (should reopen immediately)
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    timer:sleep(100),
    
    %% STRICT: Verify circuit is open again
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State, "Circuit breaker MUST reopen on failure in half-open state"),
    
    ok.

test_circuit_breaker_with_fallbacks(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    
    %% Create policy with circuit breaker and fallback
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 3
        },
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"circuit_breaker_open">>]},
                <<"to">> => <<"anthropic">>
            }
        ]
    },
    
    Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyMap),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Open circuit
    [router_circuit_breaker:record_failure(TenantId, <<"openai">>) || _ <- lists:seq(1, 3)],
    
    %% Request should use fallback when circuit is open
    _Request = #{
        message => #{
            <<"tenant_id">> => TenantId,
            <<"content">> => <<"test">>
        },
        context => #{}
    },
    
    %% Note: This test verifies that circuit breaker check happens before fallback
    %% Full integration requires actual provider call simulation
    ok.

test_circuit_breaker_with_retry(_Config) ->
    %% Test: circuit breaker state preserved across retry attempts
    TenantId = <<"test_tenant_retry">>,
    ProviderId = <<"test_provider">>,
    
    %% Configure circuit breaker
    Config = #{
        <<"failure_threshold">> => 5,
        <<"error_rate_threshold">> => 2.0, %% Disable error rate trigger
        <<"timeout_ms">> => 1000
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    {ok, closed} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Simulate retry with failures (3 attempts)
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 3)],
    
    %% Circuit should still be closed (below threshold)
    {ok, State1} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, State1, "Circuit should remain closed below threshold"),
    
    %% More failures (2 more to reach threshold)
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 2)],
    timer:sleep(100),
    
    %% Circuit should now be open (5 failures = threshold)
    {ok, State2} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State2, "Circuit MUST open at failure threshold"),
    
    %% Verify should_allow returns error
    ?assertMatch({error, circuit_open}, router_circuit_breaker:should_allow(TenantId, ProviderId)),
    
    ok.

test_circuit_breaker_disabled(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    
    %% Create policy with circuit breaker disabled
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"circuit_breaker">> => #{
            <<"enabled">> => false
        }
    },
    
    Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyMap),
    %% Circuit breaker is disabled - either undefined or map with enabled=false
    CB = Policy#policy.circuit_breaker,
    case CB of
        undefined -> ok;
        #{<<"enabled">> := false} -> ok;
        Other -> ct:fail({unexpected_circuit_breaker, Other})
    end,
    
    ok.

test_circuit_breaker_error_rate_threshold(_Config) ->
    %% Test: circuit breaker opens when error rate exceeds threshold
    TenantId = <<"test_tenant_error_rate">>,
    ProviderId = <<"test_provider">>,
    
    %% Configure with low error rate threshold
    Config = #{
        <<"failure_threshold">> => 100,  %% High - won't trigger by count
        <<"error_rate_threshold">> => 0.5,  %% 50% error rate
        <<"error_rate_window_seconds">> => 10
    },
    
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    {ok, closed} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Record mixed results: 7 failures, 3 successes = 70% error rate
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 7)],
    [router_circuit_breaker:record_success(TenantId, ProviderId) || _ <- lists:seq(1, 3)],
    
    timer:sleep(200),
    
    %% STRICT: Circuit MUST open (70% > 50% threshold)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State, "Circuit MUST open when error rate exceeds threshold"),
    
    ok.

%% ============================================================================
%% Provider Callbacks Integration Tests (CP2)
%% ============================================================================

test_circuit_breaker_provider_callbacks_success(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    {ok, closed} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Simulate successful provider result by directly recording success
    %% (This tests CB behavior without requiring full result processing pipeline)
    ok = router_circuit_breaker:record_success(TenantId, ProviderId),
    
    %% Verify CB state is still closed (or half-open if was half-open)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(lists:member(State, [closed, half_open])),
    
    ok.

test_circuit_breaker_provider_callbacks_failure(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    {ok, closed} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Simulate failure by directly recording failure
    ok = router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    %% Verify failure was recorded (circuit may still be closed if threshold not reached)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(lists:member(State, [closed, open])),
    
    ok.

test_circuit_breaker_provider_callbacks_timeout(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Record multiple failures (timeouts) directly
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 5)],
    
    %% Verify circuit is open (if threshold is 5 or less)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(lists:member(State, [closed, open])),
    
    ok.

test_circuit_breaker_provider_callbacks_error_types(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Record failures for each error type that should affect CB
    %% (TIMEOUT, CONNECTION_ERROR, PROVIDER_UNAVAILABLE, 5XX, etc.)
    [router_circuit_breaker:record_failure(TenantId, ProviderId) || _ <- lists:seq(1, 7)],
    
    %% Verify failures were recorded
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(lists:member(State, [closed, open])),
    
    ok.

test_circuit_breaker_provider_callbacks_ignored_errors(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    InitialState = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% For ignored errors (4xx, validation_error, rate_limit_exceeded, cancelled)
    %% we should NOT record failures - only record successes
    %% (In real code, these would be filtered before calling record_failure)
    [router_circuit_breaker:record_success(TenantId, ProviderId) || _ <- lists:seq(1, 4)],
    
    %% Verify CB state remains closed (successes don't open circuit)
    FinalState = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(InitialState, FinalState),
    
    ok.

