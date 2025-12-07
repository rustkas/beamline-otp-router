%% @doc Test Utilities Template
%%
%% This is a template for creating test utility modules for specific test categories.
%% Copy this file and customize it for your test category (e.g., router_integration_test_utils.erl).
%%
%% Pattern: Test utilities should follow consistent patterns:
%% - Lifecycle functions: start_*/0, stop_*/0, ensure_*_alive/0, reset_*/0
%% - Waiters: wait_for_*/N functions with timeout support
%% - Helpers: get_*/N functions for retrieving state
%% - Debugging: dump_*/0 functions for test debugging
%%
%% @see test/router_test_utils.erl Reference implementation
%% @see TESTING_GUIDE.md#test-utilities For test utility usage patterns
%%
%% ⚠️ TEMPLATE FILE - DO NOT USE DIRECTLY
%% Copy this file and customize for your test category.
-module(router_test_utils_template).

-export([
    %% Lifecycle functions
    start_test_environment/0,
    stop_test_environment/0,
    ensure_component_alive/0,
    reset_component/0,
    
    %% Waiters
    wait_for_condition/2,
    
    %% Helpers
    get_component_state/0,
    
    %% Debugging
    dump_component_state/0
]).

-include_lib("common_test/include/ct.hrl").

%% ============================================================================
%% Lifecycle Functions
%% ============================================================================

%% @doc Start test environment
%% Idempotent: safe to call multiple times
-spec start_test_environment() -> ok.
start_test_environment() ->
    %% Check if already started
    case is_test_environment_running() of
        true ->
            ok;
        false ->
            %% Start test environment
            start_test_environment_internal()
    end.

%% @doc Stop test environment
-spec stop_test_environment() -> ok.
stop_test_environment() ->
    %% Stop test environment
    stop_test_environment_internal(),
    ok.

%% @doc Ensure component is alive
%% Waits up to 2 seconds for component to start
-spec ensure_component_alive() -> ok.
ensure_component_alive() ->
    ensure_component_alive(20).  % 20 retries × 100ms = 2 seconds

ensure_component_alive(0) ->
    ct:fail(component_not_started);
ensure_component_alive(Retries) ->
    case is_component_alive() of
        true ->
            ok;
        false ->
            timer:sleep(100),
            ensure_component_alive(Retries - 1)
    end.

%% @doc Reset component state (for testing)
-spec reset_component() -> ok.
reset_component() ->
    %% Reset component state
    reset_component_internal(),
    ok.

%% ============================================================================
%% Waiters
%% ============================================================================

%% @doc Wait for condition to be true
%% @param CheckFun Function that returns true when condition is met
%% @param TimeoutMs Timeout in milliseconds
%% @returns ok | timeout
-spec wait_for_condition(fun(() -> boolean()), integer()) -> ok | timeout.
wait_for_condition(CheckFun, TimeoutMs) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_condition_loop(CheckFun, Start, TimeoutMs).

wait_for_condition_loop(CheckFun, Start, TimeoutMs) ->
    case CheckFun() of
        true ->
            ok;
        false ->
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - Start,
            case Elapsed >= TimeoutMs of
                true ->
                    timeout;
                false ->
                    timer:sleep(50),
                    wait_for_condition_loop(CheckFun, Start, TimeoutMs)
            end
    end.

%% ============================================================================
%% Helpers
%% ============================================================================

%% @doc Get component state
-spec get_component_state() -> map() | undefined.
get_component_state() ->
    %% Get component state
    get_component_state_internal().

%% ============================================================================
%% Debugging
%% ============================================================================

%% @doc Dump component state for debugging
-spec dump_component_state() -> list().
dump_component_state() ->
    State = get_component_state(),
    ct:pal("Component state: ~p", [State]),
    State.

%% ============================================================================
%% Internal Functions
%% ============================================================================

is_test_environment_running() ->
    %% Check if test environment is running
    false.

start_test_environment_internal() ->
    %% Start test environment
    ok.

stop_test_environment_internal() ->
    %% Stop test environment
    ok.

is_component_alive() ->
    %% Check if component is alive
    false.

reset_component_internal() ->
    %% Reset component state
    ok.

get_component_state_internal() ->
    %% Get component state
    undefined.

