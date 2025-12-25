%% @doc Common Test utilities for router test suites
%%
%% Provides standardized initialization, teardown, and helper functions for all
%% router test suites. This module ensures consistent test setup and reduces
%% code duplication across test files.
%%
%% Usage:
%% - Call `start_router_app/0` in `init_per_suite/1`
%% - Call `stop_router_app/0` in `end_per_suite/1`
%% - Use lifecycle helpers (`ensure_*_alive/0`, `reset_*/0`) in `init_per_testcase/2`
%% - Use waiters (`wait_for_metric/3`, `wait_for_breaker_state/4`) for deterministic assertions
%%
%% Test Data Requirements:
%% - Tests should use tenant/provider IDs from test configuration
%% - Metrics are automatically cleared between test cases
%% - Circuit breaker state is reset before each test
%%
%% @see TESTING_GUIDE.md For test execution procedures and test data requirements
%% @see test/router_circuit_breaker_SUITE.erl Example usage
%% @see test/router_metrics_r10_SUITE.erl Example usage
%% @test_category utils, common
-module(router_test_utils).

-export([
    %% Lifecycle functions
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    ensure_router_nats_alive/0,
    reset_circuit_breaker/0,
    %% Breaker state waiters
    wait_for_breaker_state/4,
    get_breaker_state/2,
    %% Generic metric waiter (accepts function, not specific metric)
    wait_for_metric/3,
    wait_for_metric_in_ets/3,
    ensure_nats_connected/0,
    %% Debugging utilities
    dump_metrics/0,
    dump_supervisor_children/0,
    set_env_if_missing/3,
    get_test_level/0,
    %% Additional lifecycle helpers
    reset_rbac/0,
    reset_idem/0,
    reset_rate_limit_store/0,
    reset_rate_limiter/0,
    reset_policy_store/0,
    ensure_rbac_alive/0,
    ensure_idem_table/0
]).

-include_lib("common_test/include/ct.hrl").

%% @doc Ensure NATS is connected, reconnecting/waiting if necessary
-spec ensure_nats_connected() -> ok.
ensure_nats_connected() ->
    WaitFun = fun() ->
        case catch router_nats:get_connection_status() of
            {ok, #{state := connected}} -> true;
            {ok, connected} -> true;
            {ok, #{state := disconnected}} ->
                 catch router_nats:reconnect(),
                 false;
            {ok, disconnected} -> 
                 catch router_nats:reconnect(),
                 false;
             _ -> false
        end
    end,
    ensure_nats_connected_loop(WaitFun, 50, 100).

ensure_nats_connected_loop(_, 0, _) -> ct:fail(nats_connection_timeout);
ensure_nats_connected_loop(Fun, Retries, Delay) ->
    case Fun() of
        true -> ok;
        false -> 
            timer:sleep(Delay),
            ensure_nats_connected_loop(Fun, Retries-1, Delay)
    end.

%% @doc Wait for a specific metric name to appear in the ETS table mocked by router_metrics
%% Checks if count >= 1
-spec wait_for_metric_in_ets(ets:tid(), atom(), integer()) -> ok.
wait_for_metric_in_ets(EtsTable, MetricName, TimeoutMs) ->
    CheckFun = fun() ->
        try
            AllMetrics = ets:tab2list(EtsTable),
            Matching = [M || {metric, inc, Name} = M <- AllMetrics, Name =:= MetricName],
            length(Matching)
        catch
            _:_ -> 0
        end
    end,
    wait_for_metric(CheckFun, 1, TimeoutMs).

%% @doc Start beamline_router application with standard configuration
%% Idempotent: safe to call multiple times (checks if already started)
%% Fails immediately if application cannot start
-spec start_router_app() -> ok.
start_router_app() ->
    %% Check if already started (idempotent)
    case lists:keyfind(beamline_router, 1, application:which_applications()) of
        {beamline_router, _, _} ->
            %% Application is started, but verify child is in supervisor
            case whereis(beamline_router_sup) of
                undefined ->
                    %% Supervisor not found - need to restart
                    ct:pal("Application started but supervisor not found, restarting...", []),
                    _ = application:stop(beamline_router),
                    timer:sleep(200),
                    start_router_app();
                SupPid when is_pid(SupPid) ->
                    %% Check if CB child is in supervisor
                    Children = supervisor:which_children(beamline_router_sup),
                    case lists:keyfind(router_circuit_breaker, 1, Children) of
                        false ->
                            %% Child not in supervisor - need to restart
                            _ = dump_supervisor_children(),
                            _ = application:stop(beamline_router),
                            timer:sleep(200),
                            start_router_app();
                        {router_circuit_breaker, undefined, worker, [router_circuit_breaker]} ->
                            %% Child failed to start - need to restart
                            _ = dump_supervisor_children(),
                            _ = application:stop(beamline_router),
                            timer:sleep(200),
                            start_router_app();
                        {router_circuit_breaker, Pid, worker, [router_circuit_breaker]} when is_pid(Pid) ->
                            %% Child exists - verify process is alive
                            case is_process_alive(Pid) of
                                true ->
                                    ok; % Already started and alive
                                false ->
                                    %% Process dead - need to restart
                                    _ = application:stop(beamline_router),
                                    timer:sleep(200),
                                    start_router_app()
                            end;
                        Error ->
                            ct:fail({circuit_breaker_child_unexpected_format, Error})
                    end
            end;
        false ->
            _ = application:load(beamline_router),
            set_env_if_missing(beamline_router, grpc_port, 0),
            set_env_if_missing(beamline_router, grpc_enabled, false),
            set_env_if_missing(beamline_router, nats_mode, mock),
            set_env_if_missing(beamline_router, tracing_enabled, false),
            set_env_if_missing(beamline_router, disable_heir, true),
            case application:ensure_all_started(beamline_router) of
                {ok, _} ->
                    %% Wait for supervisor to start all children
                    timer:sleep(500),
                    %% Verify supervisor is running
                    case whereis(beamline_router_sup) of
                        undefined ->
                            ct:fail(beamline_router_supervisor_not_started);
                        SupPid when is_pid(SupPid) ->
                            Children = supervisor:which_children(beamline_router_sup),
                            
                            %% Check if circuit breaker is in children
                            case lists:keyfind(router_circuit_breaker, 1, Children) of
                                false ->
                                    %% Child not found in list - this is the problem!
                                    ct:fail({circuit_breaker_child_missing_from_supervisor, children, Children});
                                {router_circuit_breaker, undefined, worker, [router_circuit_breaker]} ->
                                    %% Child failed to start - check process registry
                                    case whereis(router_circuit_breaker) of
                                        undefined ->
                                            ct:fail({circuit_breaker_child_failed_to_start, children, Children});
                                        Pid when is_pid(Pid) ->
                                            %% Process exists but supervisor shows undefined - race condition?
                                            ok
                                    end;
                                {router_circuit_breaker, Pid, worker, [router_circuit_breaker]} when is_pid(Pid) ->
                                    %% Child is running
                                    ok;
                                Error ->
                                    ct:fail({circuit_breaker_child_error, Error})
                            end
                    end,
                    ok;
                {error, Reason} ->
                    ct:log(error, "beamline_router start failed: ~p", [Reason]),
                    ct:fail({cannot_start_beamline_router, Reason})
            end
    end.

%% @doc Stop beamline_router application
-spec stop_router_app() -> ok.
stop_router_app() ->
    _ = application:stop(beamline_router),
    ok.

%% @doc Ensure circuit breaker process is alive
%% Waits up to 2 seconds for process to start (retries every 100ms)
%% Fails if process is not running after retries
-spec ensure_circuit_breaker_alive() -> ok.
ensure_circuit_breaker_alive() ->
    ensure_circuit_breaker_alive(20).  % 20 retries × 100ms = 2 seconds

ensure_circuit_breaker_alive(0) ->
    %% Diagnostic: check supervisor and children
    _ = dump_supervisor_children(),
    SupPid = whereis(beamline_router_sup),
    SupInfo = case SupPid of
        undefined -> {supervisor_not_found};
        P when is_pid(P) ->
            case catch supervisor:which_children(beamline_router_sup) of
                Children when is_list(Children) ->
                    CBChild = lists:keyfind(router_circuit_breaker, 1, Children),
                    {supervisor_found, P, children_count, length(Children), cb_child, CBChild};
                Error -> {supervisor_children_error, Error}
            end
    end,
    %% Check process registry directly
    CBProc = whereis(router_circuit_breaker),
    ct:fail({router_circuit_breaker_not_started, 
             diagnostic, SupInfo, 
             process_registry, CBProc});
ensure_circuit_breaker_alive(Retries) ->
    case whereis(router_circuit_breaker) of
        undefined ->
            timer:sleep(100),
            ensure_circuit_breaker_alive(Retries - 1);
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ok;
                false ->
                    timer:sleep(100),
                    ensure_circuit_breaker_alive(Retries - 1)
            end
    end.

%% @doc Ensure router_nats process is alive
%% Fails immediately if process is not running (no fallback start)
-spec ensure_router_nats_alive() -> ok.
ensure_router_nats_alive() ->
    case whereis(router_nats) of
        undefined ->
            ct:fail(router_nats_not_started);
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ok;
                false -> ct:fail(router_nats_not_alive)
            end
    end.

%% @doc Reset circuit breaker state (test-only operation)
%% Clears ETS table and resets internal state
-spec reset_circuit_breaker() -> ok.
reset_circuit_breaker() ->
    case whereis(router_circuit_breaker) of
        undefined ->
            %% In tests: not started yet, not an error
            ct:pal("reset_circuit_breaker: router_circuit_breaker not started", []),
            ok;
        _ ->
            case gen_server:call(router_circuit_breaker, reset_all, 5000) of
                ok ->
                    ok;
                Other ->
                    ct:fail({reset_circuit_breaker_failed, Other})
            end
    end.

%% @doc Wait for metric to reach expected value
%% Retries until timeout, then fails if value not reached
%% @param FetchFun Function that returns current metric value
%% @param Expected Minimum expected value
%% @param TimeoutMs Timeout in milliseconds
-spec wait_for_metric(fun(() -> number()), number(), integer()) -> ok.
wait_for_metric(FetchFun, Expected, TimeoutMs) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_metric_loop(FetchFun, Expected, Start, TimeoutMs).

%% @doc Internal loop for metric waiting
-spec wait_for_metric_loop(fun(() -> number()), number(), integer(), integer()) -> ok.
wait_for_metric_loop(FetchFun, Expected, Start, TimeoutMs) ->
    Value = FetchFun(),
    case Value >= Expected of
        true ->
            ok;
        false ->
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - Start,
            case Elapsed >= TimeoutMs of
                true ->
                    %% Dump metrics snapshot before failing
                    _ = router_r10_metrics:dump_metrics(),
                    ct:fail({metric_not_reached, Expected, Value, elapsed_ms, Elapsed});
                false ->
                    timer:sleep(50),  % Increased from 10ms to 50ms for less CPU usage
                    wait_for_metric_loop(FetchFun, Expected, Start, TimeoutMs)
            end
    end.

%% @doc Dump all metrics from ETS for debugging
%% P2: Delegated to router_r10_metrics to centralize metric access
-spec dump_metrics() -> list().
dump_metrics() ->
    router_r10_metrics:dump_metrics().

%% @doc Dump supervisor children for debugging
-spec dump_supervisor_children() -> list().
dump_supervisor_children() ->
    case whereis(beamline_router_sup) of
        undefined ->
            ct:pal("beamline_router_sup not found", []),
            [];
        _ ->
            case catch supervisor:which_children(beamline_router_sup) of
                Children when is_list(Children) ->
                    ct:pal("Supervisor children (~p):", [length(Children)]),
                    lists:foreach(fun({Id, Pid, Type, Mods}) ->
                        ct:pal("  ~p: ~p (~p, ~p)", [Id, Pid, Type, Mods])
                    end, Children),
                    Children;
                Error ->
                    ct:pal("Failed to get supervisor children: ~p", [Error]),
                    []
            end
    end.

%% @doc Wait for circuit breaker to reach expected state
%% @param TenantId Tenant identifier
%% @param ProviderId Provider identifier
%% @param ExpectedState Expected state (closed, open, half_open)
%% @param TimeoutMs Timeout in milliseconds
-spec wait_for_breaker_state(binary(), binary(), atom(), integer()) -> ok.
wait_for_breaker_state(TenantId, ProviderId, ExpectedState, TimeoutMs) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_breaker_state_loop(TenantId, ProviderId, ExpectedState, Start, TimeoutMs).

%% @doc Internal loop for breaker state waiting
-spec wait_for_breaker_state_loop(binary(), binary(), atom(), integer(), integer()) -> ok.
wait_for_breaker_state_loop(TenantId, ProviderId, ExpectedState, Start, TimeoutMs) ->
    CurrentState = get_breaker_state(TenantId, ProviderId),
    case CurrentState =:= ExpectedState of
        true ->
            ok;
        false ->
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - Start,
            case Elapsed >= TimeoutMs of
                true ->
                    ct:fail({breaker_state_not_reached, ExpectedState, CurrentState, elapsed_ms, Elapsed});
                false ->
                    timer:sleep(50),
                    wait_for_breaker_state_loop(TenantId, ProviderId, ExpectedState, Start, TimeoutMs)
            end
    end.

%% @doc Get current circuit breaker state
%% @param TenantId Tenant identifier
%% @param ProviderId Provider identifier
%% @returns State atom (closed, open, half_open) or undefined
-spec get_breaker_state(binary(), binary()) -> atom() | undefined.
get_breaker_state(TenantId, ProviderId) ->
    try
        case router_circuit_breaker:get_state(TenantId, ProviderId) of
            {ok, State} -> State;
            {error, _} -> undefined
        end
    catch
        _:_ -> undefined
    end.

%% P2: Metric functions removed - use router_r10_metrics instead

%% ========================================================================
%% Lifecycle Helpers (Pattern Replication)
%% ========================================================================

%% @doc Reset RBAC state (for testing)
%% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
%% Safe reset via handle_call(reset_all, ...) - clears ETS tables but keeps process alive
-spec reset_rbac() -> ok | {error, term()}.
reset_rbac() ->
    router_rbac:reset().

%% @doc Reset idempotency state (for testing)
%% Pattern: Reuse reset/lifecycle pattern
%% Safe reset - clears ETS table but keeps table alive
-spec reset_idem() -> integer().
reset_idem() ->
    router_idem:reset().

%% @doc Reset rate limit store state (for testing)
%% Pattern: Reuse reset/lifecycle pattern
-spec reset_rate_limit_store() -> ok | {error, term()}.
reset_rate_limit_store() ->
    router_rate_limit_store:reset().

%% @doc Reset rate limiter state (for testing)
%% Pattern: Reuse reset/lifecycle pattern
-spec reset_rate_limiter() -> ok | {error, term()}.
reset_rate_limiter() ->
    router_rate_limiter:reset().

%% @doc Reset policy store state (for testing)
%% Pattern: Reuse reset/lifecycle pattern
-spec reset_policy_store() -> ok | {error, term()}.
reset_policy_store() ->
    router_policy_store:reset().

%% @doc Ensure RBAC gen_server is alive (for testing)
%% Similar to ensure_circuit_breaker_alive/0
-spec ensure_rbac_alive() -> ok.
ensure_rbac_alive() ->
    case whereis(router_rbac) of
        undefined ->
            ct:fail("RBAC gen_server is not running");
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    ct:fail("RBAC gen_server is not alive")
            end
    end.

%% @doc Ensure idempotency ETS table exists (for testing)
-spec ensure_idem_table() -> ok.
ensure_idem_table() ->
    case ets:info(router_idem) of
        undefined ->
            router_idem:init([]);
        _ ->
            ok
    end.

%% ========================================================================
%% Standardized Test Utility Patterns
%% ========================================================================

%% Pattern: Lifecycle Functions
%% - start_*/0: Start component (idempotent)
%% - stop_*/0: Stop component
%% - ensure_*_alive/0: Ensure component is alive (with retries)
%% - reset_*/0: Reset component state (for testing)

%% Pattern: Waiters
%% - wait_for_*/N: Wait for condition with timeout
%% - wait_for_*_loop/N: Internal loop with retry logic
%% - Uses erlang:monotonic_time(millisecond) for timing
%% - Sleeps 50ms between retries

%% Pattern: Helpers
%% - get_*/N: Get component state/values
%% - Returns {ok, Value} | {error, Reason} for consistency

%% Pattern: Debugging
%% - dump_*/0: Dump component state for debugging
%% - Uses ct:pal for test output
%% - Returns list of state information

%% @doc Set application environment variable only if it's currently undefined
-spec set_env_if_missing(atom(), atom(), term()) -> ok.
set_env_if_missing(App, Key, Val) ->
    case application:get_env(App, Key) of
        undefined ->
            application:set_env(App, Key, Val);
        {ok, _} ->
            ok
    end.

%% @doc Get test level from environment variable
-spec get_test_level() -> string().
get_test_level() ->
    os:getenv("ROUTER_TEST_LEVEL", "ci").
