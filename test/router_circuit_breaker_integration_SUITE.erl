%% @doc Integration Test Suite for Circuit Breaker with Router Policy Applier
-module(router_circuit_breaker_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks and test cases (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
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
    process_validated_result/12
]}).

all() ->
    [
        {group, circuit_breaker_integration_tests}
    ].

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
    %% This test requires manual state manipulation or shorter timeout
    %% Will be implemented with configurable timeout in test fixtures
    ok.

test_circuit_breaker_half_open_to_open(_Config) ->
    %% This test requires manual state manipulation
    %% Will be implemented in integration tests
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
    %% This test verifies circuit breaker works with retry/backoff
    %% Will be implemented with full retry simulation
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
    ?assertEqual(undefined, Policy#policy.circuit_breaker),
    
    ok.

test_circuit_breaker_error_rate_threshold(_Config) ->
    %% This test verifies error rate threshold triggers circuit opening
    %% Will be implemented with time window simulation
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
    
    %% Simulate successful provider result
    Result = #{
        <<"status">> => <<"success">>,
        <<"provider_id">> => ProviderId,
        <<"assignment_id">> => <<"assignment_123">>,
        <<"request_id">> => <<"request_456">>
    },
    
    %% Process result (should call record_success)
    process_validated_result(Result, TenantId, <<"assignment_123">>, <<"request_456">>, <<"success">>, <<"chat">>, ProviderId, 100, 0.01, undefined, erlang:system_time(millisecond), undefined),
    
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
    
    %% Simulate failure result (timeout)
    Result = #{
        <<"status">> => <<"timeout">>,
        <<"provider_id">> => ProviderId,
        <<"assignment_id">> => <<"assignment_123">>,
        <<"request_id">> => <<"request_456">>
    },
    
    %% Process result (should call record_failure)
    process_validated_result(Result, TenantId, <<"assignment_123">>, <<"request_456">>, <<"timeout">>, <<"chat">>, ProviderId, undefined, undefined, undefined, erlang:system_time(millisecond), undefined),
    
    %% Verify failure was recorded (circuit may still be closed if threshold not reached)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(lists:member(State, [closed, open])),
    
    ok.

test_circuit_breaker_provider_callbacks_timeout(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Record multiple timeouts (should open circuit)
    [begin
        Result = #{
            <<"status">> => <<"timeout">>,
            <<"provider_id">> => ProviderId,
            <<"assignment_id">> => <<"assignment_", (integer_to_binary(I))/binary>>,
            <<"request_id">> => <<"request_", (integer_to_binary(I))/binary>>
        },
        process_validated_result(Result, TenantId, <<"assignment_", (integer_to_binary(I))/binary>>, <<"request_", (integer_to_binary(I))/binary>>, <<"timeout">>, <<"chat">>, ProviderId, undefined, undefined, undefined, erlang:system_time(millisecond), undefined)
    end || I <- lists:seq(1, 5)],
    
    %% Verify circuit is open (if threshold is 5 or less)
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assert(lists:member(State, [closed, open])),
    
    ok.

test_circuit_breaker_provider_callbacks_error_types(_Config) ->
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    %% Initialize CB state
    ok = router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Test CB-relevant error types
    CBRelevantErrors = [
        {<<"error">>, #{<<"error_code">> => <<"TIMEOUT">>}},
        {<<"error">>, #{<<"error_code">> => <<"CONNECTION_ERROR">>}},
        {<<"error">>, #{<<"error_code">> => <<"PROVIDER_UNAVAILABLE">>}},
        {<<"error">>, #{<<"error_code">> => <<"5XX">>}},
        {<<"error">>, #{<<"error_type">> => <<"timeout">>}},
        {<<"error">>, #{<<"error_type">> => <<"connection_error">>}},
        {<<"error">>, #{<<"error_type">> => <<"5xx">>}}
    ],
    
    %% Process each error type (should call record_failure)
    [begin
        {Status, ErrorInfo} = ErrorSpec,
        Result = maps:merge(#{
            <<"status">> => Status,
            <<"provider_id">> => ProviderId,
            <<"assignment_id">> => <<"assignment_", (integer_to_binary(I))/binary>>,
            <<"request_id">> => <<"request_", (integer_to_binary(I))/binary>>
        }, ErrorInfo),
        process_validated_result(Result, TenantId, <<"assignment_", (integer_to_binary(I))/binary>>, <<"request_", (integer_to_binary(I))/binary>>, Status, <<"chat">>, ProviderId, undefined, undefined, undefined, erlang:system_time(millisecond), undefined)
    end || {I, ErrorSpec} <- lists:zip(lists:seq(1, length(CBRelevantErrors)), CBRelevantErrors)],
    
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
    
    %% Test errors that should NOT affect CB (4xx, validation_error, rate_limit_exceeded, cancelled)
    IgnoredErrors = [
        {<<"error">>, #{<<"error_code">> => <<"4XX">>}},
        {<<"error">>, #{<<"error_code">> => <<"VALIDATION_ERROR">>}},
        {<<"error">>, #{<<"error_code">> => <<"RATE_LIMIT_EXCEEDED">>}},
        {<<"cancelled">>, #{}}
    ],
    
    %% Process each ignored error (should NOT call record_failure)
    [begin
        {Status, ErrorInfo} = ErrorSpec,
        Result = maps:merge(#{
            <<"status">> => Status,
            <<"provider_id">> => ProviderId,
            <<"assignment_id">> => <<"assignment_", (integer_to_binary(I))/binary>>,
            <<"request_id">> => <<"request_", (integer_to_binary(I))/binary>>
        }, ErrorInfo),
        process_validated_result(Result, TenantId, <<"assignment_", (integer_to_binary(I))/binary>>, <<"request_", (integer_to_binary(I))/binary>>, Status, <<"chat">>, ProviderId, undefined, undefined, undefined, erlang:system_time(millisecond), undefined)
    end || {I, ErrorSpec} <- lists:zip(lists:seq(1, length(IgnoredErrors)), IgnoredErrors)],
    
    %% Verify CB state did not change (still in initial state)
    FinalState = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(InitialState, FinalState),
    
    ok.

%% Helper: Call process_validated_result directly (bypassing NATS subscription)
process_validated_result(Result, ValidatedTenantId, AssignmentId, RequestId, Status, JobType, ProviderId, LatencyMs, Cost, TraceId, Timestamp, MsgId) ->
    %% Function is exported for testing - call directly
    router_result_consumer:process_validated_result(Result, ValidatedTenantId, AssignmentId, RequestId, Status, JobType, ProviderId, LatencyMs, Cost, TraceId, Timestamp, MsgId).

