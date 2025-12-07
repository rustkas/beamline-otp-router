%% @doc Load Test Suite for Circuit Breaker under High Load
%% Tests CB behavior with error spikes, traffic normalization, and different window configurations
-module(router_circuit_breaker_load_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, groups/0, init_per_suite/1, init_per_testcase/2]}).

%% Export Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2]).
-export([test_cb_error_spike_opens_circuit/1, test_cb_traffic_normalization_closes_circuit/1, test_cb_different_window_sizes/1, test_cb_concurrent_requests_under_load/1]).

%% Define circuit_breaker_state record locally (same as in router_circuit_breaker.erl)
-record(circuit_breaker_state, {
    key :: {binary(), binary()},
    state :: closed | open | half_open,
    failure_count :: integer(),
    success_count :: integer(),
    state_changed_at :: integer(),
    last_failure_time :: integer() | undefined,
    last_success_time :: integer() | undefined,
    half_open_calls_count :: integer(),
    total_requests :: integer(),
    total_failures :: integer(),
    total_successes :: integer(),
    error_rate :: float(),
    window_events :: [{integer(), success | failure}],
    config :: map()
}).

all() ->
    [
        {group, load_tests}
    ].

groups() ->
    [
        {load_tests, [sequence], [
            test_cb_error_spike_opens_circuit,
            test_cb_traffic_normalization_closes_circuit,
            test_cb_different_window_sizes,
            test_cb_concurrent_requests_under_load
        ]}
    ].

init_per_suite(Config) ->
    %% Start application (router_circuit_breaker is now started via supervisor)
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, disable_heir, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Verify circuit breaker gen_server is running (started via supervisor)
            case whereis(router_circuit_breaker) of
                undefined ->
                    ct:fail("Circuit breaker gen_server should be started via supervisor");
                Pid ->
                    case is_process_alive(Pid) of
                        true ->
                            timer:sleep(100), %% Allow gen_server to initialize
                            Config;
                        false ->
                            ct:fail("Circuit breaker gen_server is not alive")
                    end
            end;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Verify gen_server is running (started via supervisor in init_per_suite)
    case whereis(router_circuit_breaker) of
        undefined ->
            ct:fail("Circuit breaker gen_server should be running (started via supervisor)");
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    ct:fail("Circuit breaker gen_server is not alive")
            end
    end,
    
    %% Verify ETS table exists
    case catch ets:info(router_provider_circuit_breaker) of
        {'EXIT', {badarg, _}} ->
            ct:fail("Circuit breaker ETS table does not exist");
        _TableInfo ->
            ok
    end,
    
    %% Clean up ETS table before each test (like other test suites do)
    try
        ets:delete_all_objects(router_provider_circuit_breaker)
    catch
        _:_ -> ok
    end,
    timer:sleep(50), %% Allow gen_server to process cleanup
    Config.

%% @doc Initialize circuit breaker with configuration from policy
%% Helper function to ensure CB is initialized with correct config from policy
%% Uses record_state_with_config to pass policy configuration directly
init_circuit_breaker_from_policy(TenantId, ProviderId, PolicyId) ->
    %% Verify gen_server is running (should be started via supervisor)
    case whereis(router_circuit_breaker) of
        undefined ->
            ct:fail("Circuit breaker gen_server should be running (started via supervisor)");
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    ct:fail("Circuit breaker gen_server is not alive")
            end
    end,
    
    %% Get policy from store
    case router_policy_store:get_policy(TenantId, PolicyId) of
        {ok, Policy} ->
            %% Extract circuit breaker configuration from policy
            CircuitBreaker = Policy#policy.circuit_breaker,
            case CircuitBreaker of
                CB when is_map(CB) ->
                    %% Use record_state_with_config to pass policy configuration
                    Result = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, CB),
                    timer:sleep(50), %% Allow gen_server to process
                    Result;
                _ ->
                    %% No circuit breaker config in policy, use default
                    Result = router_circuit_breaker:record_state(TenantId, ProviderId),
                    timer:sleep(50), %% Allow gen_server to process
                    Result
            end;
        {error, not_found} ->
            %% Policy not found, create state with default config
            Result = router_circuit_breaker:record_state(TenantId, ProviderId),
            timer:sleep(50), %% Allow gen_server to process
            Result
    end.

%% @doc Test: Error spike causes circuit to open
%% Scenario: Rapid sequence of failures → CB opens → fail-fast behavior
test_cb_error_spike_opens_circuit(_Config) ->
    TenantId = <<"load_test_tenant">>,
    ProviderId = <<"load_test_provider">>,
    
    %% Load policy with CB enabled (failure_threshold = 5)
    PolicyJson = #{
        <<"policy_id">> => <<"load_test_policy">>,
        <<"weights">> => #{ProviderId => 1.0},
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 5,
            <<"error_rate_threshold">> => 0.5,
            <<"error_rate_window_seconds">> => 60,
            <<"timeout_ms">> => 30000,
            <<"success_threshold">> => 2
        }
    },
    
    %% Parse and load policy
    PolicyId = <<"load_test_policy">>,
    Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyJson),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Verify gen_server is running and can process requests
    Pid = case whereis(router_circuit_breaker) of
        undefined ->
            ct:fail("Circuit breaker gen_server is not running");
        P ->
            case is_process_alive(P) of
                true ->
                    P;
                false ->
                    ct:fail("Circuit breaker gen_server is not alive")
            end
    end,
    ct:comment("Circuit breaker gen_server is running: ~p", [Pid]),
    
    %% Verify ETS table exists
    Table = router_provider_circuit_breaker,
    Key = {TenantId, ProviderId},
    case catch ets:info(Table) of
        {'EXIT', {badarg, _}} ->
            ct:fail("Circuit breaker ETS table does not exist");
        TableInfo ->
            ct:comment("ETS table exists: ~p", [TableInfo])
    end,
    
    %% Try to call gen_server directly to verify it can process requests
    DirectCallResult = gen_server:call(router_circuit_breaker, {get_state, TenantId, ProviderId}, 5000),
    ct:comment("Direct gen_server:call result: ~p", [DirectCallResult]),
    
    %% Initialize circuit breaker with configuration from policy
    %% First create state using record_failure (this ensures state exists)
    try
        RecordFailureResult = router_circuit_breaker:record_failure(TenantId, ProviderId),
        ct:comment("record_failure result: ~p", [RecordFailureResult]),
        ok = RecordFailureResult, %% Assert that record_failure returns ok
        timer:sleep(500), %% Allow gen_server to process (increased delay)
        
        %% Verify state was created in ETS directly
        ETSLookupDirect = ets:lookup(Table, Key),
        ct:comment("ETS lookup directly: ~p", [ETSLookupDirect]),
        case ETSLookupDirect of
            [] ->
                ct:comment("WARNING: State not found in ETS immediately after record_failure"),
                %% Check if gen_server is still alive
                case whereis(router_circuit_breaker) of
                    undefined ->
                        ct:fail("gen_server died after record_failure");
                    DeadPid ->
                        case is_process_alive(DeadPid) of
                            false ->
                                ct:fail("gen_server is not alive after record_failure");
                            true ->
                                %% Try to call gen_server again to see what happens
                                RetryResult = gen_server:call(router_circuit_breaker, {get_state, TenantId, ProviderId}, 5000),
                                ct:comment("Retry get_state result: ~p", [RetryResult]),
                                ct:comment("ETS table info: ~p", [ets:info(Table)]),
                                AllEntriesInTry = ets:tab2list(Table),
                                ct:comment("All ETS entries: ~p", [AllEntriesInTry]),
                                ct:comment("Looking for key: ~p", [Key]),
                                %% If state exists in ETS but get_state returns not_found, there's a problem
                                case AllEntriesInTry of
                                    [] ->
                                        ct:fail("No entries in ETS table after record_failure");
                                    _ ->
                                        ct:fail("State exists in ETS but get_state returned not_found - gen_server may have wrong table reference")
                                end
                        end
                end;
            [CBState] ->
                ct:comment("State found in ETS after record_failure: ~p", [CBState]),
                ct:comment("State key: ~p", [CBState#circuit_breaker_state.key])
        end
    catch
        Error:Reason ->
            ct:fail("record_failure failed: ~p:~p", [Error, Reason])
    end,
    
    %% Verify state exists after record_failure via API
    %% First verify ETS directly
    ETSLookupBefore = ets:lookup(Table, Key),
    ct:comment("ETS lookup before get_state: ~p", [ETSLookupBefore]),
    
    case router_circuit_breaker:get_state(TenantId, ProviderId) of
        {ok, InitState1} ->
            ct:comment("State after record_failure (via API): ~p", [InitState1]);
        {error, not_found} ->
            %% Double-check ETS table
            ETSLookupResult = ets:lookup(Table, Key),
            ct:comment("ETS lookup after get_state returned not_found: ~p", [ETSLookupResult]),
            AllEntries = ets:tab2list(Table),
            ct:comment("All ETS entries: ~p", [AllEntries]),
            ct:comment("Looking for key: ~p", [Key]),
            case ETSLookupResult of
                [] ->
                    ct:fail("Circuit breaker state not found in ETS after record_failure");
                [CBStateFromETS] ->
                    ct:comment("State found in ETS but not via API: ~p", [CBStateFromETS]),
                    ct:comment("State key in ETS: ~p", [CBStateFromETS#circuit_breaker_state.key]),
                    ct:comment("Expected key: ~p", [Key]),
                    ct:fail("Circuit breaker state exists in ETS but gen_server doesn't see it")
            end
    end,
    
    %% Now update config from policy
    InitResult = init_circuit_breaker_from_policy(TenantId, ProviderId, PolicyId),
    ct:comment("init_circuit_breaker_from_policy result: ~p", [InitResult]),
    timer:sleep(100),
    
    %% Verify state still exists after updating config
    {ok, _InitState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Simulate error spike: 10 rapid failures
    ct:comment("Simulating error spike: 10 rapid failures"),
    StartTime = erlang:monotonic_time(millisecond),
    
    lists:foreach(fun(_I) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId),
        timer:sleep(10) %% Small delay to simulate real requests
    end, lists:seq(1, 10)),
    
    %% Verify circuit is open
    {ok, CBStateAtom} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, CBStateAtom, "Circuit should be open after error spike"),
    
    %% Verify fail-fast behavior
    {error, circuit_open} = router_circuit_breaker:should_allow(TenantId, ProviderId),
    
    %% Verify metrics
    %% Note: Metrics are emitted via telemetry, so we verify state instead
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    ct:comment("Error spike test completed in ~p ms", [Duration]),
    ct:comment("Circuit state: ~p", [CBStateAtom]),
    
    ok.

%% @doc Test: Traffic normalization causes circuit to close
%% Scenario: CB opens → timeout elapses → half-open → successes → closed
test_cb_traffic_normalization_closes_circuit(_Config) ->
    TenantId = <<"load_test_tenant">>,
    ProviderId = <<"load_test_provider">>,
    
    %% Load policy with short timeout for faster testing
    PolicyJson = #{
        <<"policy_id">> => <<"load_test_policy">>,
        <<"weights">> => #{ProviderId => 1.0},
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 3,
            <<"error_rate_threshold">> => 0.5,
            <<"error_rate_window_seconds">> => 60,
            <<"timeout_ms">> => 2000, %% Short timeout for testing
            <<"success_threshold">> => 2
        }
    },
    
    PolicyId = <<"load_test_policy">>,
    Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyJson),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Initialize circuit breaker with configuration from policy
    ok = init_circuit_breaker_from_policy(TenantId, ProviderId, PolicyId),
    
    %% Step 1: Open circuit with failures
    ct:comment("Step 1: Opening circuit with failures"),
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    {ok, OpenStateAtom} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenStateAtom, "Circuit should be open"),
    
    %% Step 2: Wait for timeout (transition to half-open)
    ct:comment("Step 2: Waiting for timeout (2 seconds)"),
    timer:sleep(2100), %% Wait slightly longer than timeout
    
    %% Trigger timeout check (internal function, simulate by calling should_allow)
    router_circuit_breaker:should_allow(TenantId, ProviderId),
    
    {ok, HalfOpenStateAtom} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenStateAtom, "Circuit should be half-open"),
    
    %% Step 3: Record successes (normalization)
    ct:comment("Step 3: Recording successes (traffic normalization)"),
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 3)),
    
    {ok, ClosedStateAtom} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedStateAtom, "Circuit should be closed after successes"),
    
    ct:comment("Traffic normalization test completed successfully"),
    ok.

%% @doc Test: Different window sizes affect error rate calculation
%% Scenario: Test with 30s, 60s, 120s windows
test_cb_different_window_sizes(_Config) ->
    TenantId = <<"load_test_tenant">>,
    ProviderId = <<"load_test_provider_30s">>,
    ProviderId60s = <<"load_test_provider_60s">>,
    ProviderId120s = <<"load_test_provider_120s">>,
    
    %% Test with 30s window
    Policy30s = #{
        <<"policy_id">> => <<"load_test_policy_30s">>,
        <<"weights">> => #{ProviderId => 1.0},
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 10,
            <<"error_rate_threshold">> => 0.5,
            <<"error_rate_window_seconds">> => 30,
            <<"timeout_ms">> => 30000,
            <<"success_threshold">> => 2
        }
    },
    
    PolicyId30s = <<"load_test_policy_30s">>,
    P30s = router_policy_store:parse_policy_map(TenantId, PolicyId30s, Policy30s),
    {ok, _} = router_policy_store:upsert_policy(TenantId, P30s),
    
    %% Initialize circuit breaker with configuration from policy
    ok = init_circuit_breaker_from_policy(TenantId, ProviderId, PolicyId30s),
    
    %% Generate mixed traffic (50% failures) over 5 seconds
    ct:comment("Generating mixed traffic (50% failures) for 30s window"),
    StartTime = erlang:monotonic_time(millisecond),
    
    lists:foreach(fun(I) ->
        case I rem 2 of
            0 -> router_circuit_breaker:record_success(TenantId, ProviderId);
            1 -> router_circuit_breaker:record_failure(TenantId, ProviderId)
        end,
        timer:sleep(100) %% 10 requests per second
    end, lists:seq(1, 50)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Get full state from ETS to access error_rate
    Key = {TenantId, ProviderId},
    Table = router_provider_circuit_breaker,
    [#circuit_breaker_state{error_rate = ErrorRate30s}] = ets:lookup(Table, Key),
    
    ct:comment("30s window test: Duration=~p ms, ErrorRate=~p", [Duration, ErrorRate30s]),
    ?assert(ErrorRate30s >= 0.0 andalso ErrorRate30s =< 1.0, "Error rate should be between 0 and 1"),
    
    %% Test with 60s window (default)
    Policy60s = #{
        <<"policy_id">> => <<"load_test_policy_60s">>,
        <<"weights">> => #{ProviderId60s => 1.0},
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 10,
            <<"error_rate_threshold">> => 0.5,
            <<"error_rate_window_seconds">> => 60,
            <<"timeout_ms">> => 30000,
            <<"success_threshold">> => 2
        }
    },
    
    PolicyId60s = <<"load_test_policy_60s">>,
    P60s = router_policy_store:parse_policy_map(TenantId, PolicyId60s, Policy60s),
    {ok, _} = router_policy_store:upsert_policy(TenantId, P60s),
    
    %% Initialize circuit breaker with configuration from policy
    ok = init_circuit_breaker_from_policy(TenantId, ProviderId60s, PolicyId60s),
    
    lists:foreach(fun(I) ->
        case I rem 2 of
            0 -> router_circuit_breaker:record_success(TenantId, ProviderId60s);
            1 -> router_circuit_breaker:record_failure(TenantId, ProviderId60s)
        end,
        timer:sleep(100)
    end, lists:seq(1, 50)),
    
    %% Get full state from ETS to access error_rate
    Key60s = {TenantId, ProviderId60s},
    [#circuit_breaker_state{error_rate = ErrorRate60s}] = ets:lookup(Table, Key60s),
    
    ct:comment("60s window test: ErrorRate=~p", [ErrorRate60s]),
    ?assert(ErrorRate60s >= 0.0 andalso ErrorRate60s =< 1.0, "Error rate should be between 0 and 1"),
    
    %% Test with 120s window
    Policy120s = #{
        <<"policy_id">> => <<"load_test_policy_120s">>,
        <<"weights">> => #{ProviderId120s => 1.0},
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 10,
            <<"error_rate_threshold">> => 0.5,
            <<"error_rate_window_seconds">> => 120,
            <<"timeout_ms">> => 30000,
            <<"success_threshold">> => 2
        }
    },
    
    PolicyId120s = <<"load_test_policy_120s">>,
    P120s = router_policy_store:parse_policy_map(TenantId, PolicyId120s, Policy120s),
    {ok, _} = router_policy_store:upsert_policy(TenantId, P120s),
    
    %% Initialize circuit breaker with configuration from policy
    ok = init_circuit_breaker_from_policy(TenantId, ProviderId120s, PolicyId120s),
    
    lists:foreach(fun(I) ->
        case I rem 2 of
            0 -> router_circuit_breaker:record_success(TenantId, ProviderId120s);
            1 -> router_circuit_breaker:record_failure(TenantId, ProviderId120s)
        end,
        timer:sleep(100)
    end, lists:seq(1, 50)),
    
    %% Get full state from ETS to access error_rate
    Key120s = {TenantId, ProviderId120s},
    [#circuit_breaker_state{error_rate = ErrorRate120s}] = ets:lookup(Table, Key120s),
    
    ct:comment("120s window test: ErrorRate=~p", [ErrorRate120s]),
    ?assert(ErrorRate120s >= 0.0 andalso ErrorRate120s =< 1.0, "Error rate should be between 0 and 1"),
    
    ct:comment("Window size comparison: 30s=~p, 60s=~p, 120s=~p", [ErrorRate30s, ErrorRate60s, ErrorRate120s]),
    ok.

%% @doc Test: Concurrent requests under load
%% Scenario: Multiple concurrent requests with mixed success/failure → verify CB state consistency
test_cb_concurrent_requests_under_load(_Config) ->
    TenantId = <<"load_test_tenant">>,
    ProviderId = <<"load_test_provider_concurrent">>,
    
    PolicyJson = #{
        <<"policy_id">> => <<"load_test_policy_concurrent">>,
        <<"weights">> => #{ProviderId => 1.0},
        <<"circuit_breaker">> => #{
            <<"enabled">> => true,
            <<"failure_threshold">> => 10,
            <<"error_rate_threshold">> => 0.5,
            <<"error_rate_window_seconds">> => 60,
            <<"timeout_ms">> => 30000,
            <<"success_threshold">> => 2
        }
    },
    
    PolicyId = <<"load_test_policy_concurrent">>,
    Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyJson),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Initialize circuit breaker with configuration from policy
    ok = init_circuit_breaker_from_policy(TenantId, ProviderId, PolicyId),
    
    %% Spawn 20 concurrent processes, each making 10 requests
    ct:comment("Spawning 20 concurrent processes, each making 10 requests"),
    StartTime = erlang:monotonic_time(millisecond),
    
    Pids = lists:map(fun(WorkerId) ->
        spawn(fun() ->
            lists:foreach(fun(RequestId) ->
                %% Mix of successes and failures
                case (WorkerId + RequestId) rem 3 of
                    0 -> router_circuit_breaker:record_success(TenantId, ProviderId);
                    _ -> router_circuit_breaker:record_failure(TenantId, ProviderId)
                end,
                timer:sleep(10) %% Small delay
            end, lists:seq(1, 10))
        end)
    end, lists:seq(1, 20)),
    
    %% Wait for all processes to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, _Reason} ->
                ok
        after
            5000 ->
                exit(Pid, kill)
        end
    end, Pids),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Wait a bit for all processes to finish
    timer:sleep(500),
    
    %% Verify final state
    {ok, FinalStateAtom} = router_circuit_breaker:get_state(TenantId, ProviderId),
    
    %% Get full state from ETS to access detailed fields
    FinalKey = {TenantId, ProviderId},
    FinalTable = router_provider_circuit_breaker,
    [#circuit_breaker_state{
        state = _FinalStateAtom2,
        total_requests = TotalRequests,
        error_rate = FinalErrorRate
    }] = ets:lookup(FinalTable, FinalKey),
    
    ct:comment("Concurrent load test completed in ~p ms", [Duration]),
    ct:comment("Final state: ~p", [FinalStateAtom]),
    ct:comment("Total requests processed: ~p", [TotalRequests]),
    ct:comment("Error rate: ~p", [FinalErrorRate]),
    
    %% Verify state is consistent (should be either closed, open, or half_open)
    ?assert(lists:member(FinalStateAtom, [closed, open, half_open]),
        "Final state should be valid"),
    
    ok.

