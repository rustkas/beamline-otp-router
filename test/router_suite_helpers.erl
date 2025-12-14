-module(router_suite_helpers).

-include_lib("common_test/include/ct.hrl").

%% Public API
-export([
    start_router_suite/0,
    stop_router_suite/0,
    reset_mocks/0,
    ensure_ets/1,
    wait_until/2,
    ensure_no_faults/0,
    await_router_started/1
]).

%% @doc Start router application for a suite with common defaults.
%% IMPORTANT: If nats_mode=mock, this will setup router_nats mock automatically
%% to prevent undef errors on router_nats:start_link/0 during supervisor startup.
%% NOTE: Always resets the mock to ensure clean expectations for fault injection.
-spec start_router_suite() -> ok.
start_router_suite() ->
    %% Clear any previous fault injections to start clean
    ensure_no_faults(),
    %% If nats_mode=mock, ALWAYS setup router_nats mock BEFORE starting app
    %% This ensures fresh mock expectations for fault injection support
    case application:get_env(beamline_router, nats_mode) of
        {ok, mock} ->
            ok = router_mock_helpers:setup_router_nats_mock();
        _ ->
            ok
    end,
    %% Start router app via shared test utility (handles env defaults)
    ok = router_test_utils:start_router_app(),
    ok.

%% @doc Stop router application for a suite and clear faults.
%% Also unloads all mocks to prevent stray mocks between suites.
-spec stop_router_suite() -> ok.
stop_router_suite() ->
    ensure_no_faults(),
    ok = router_test_utils:stop_router_app(),
    %% Unload all mocks to prevent stray mocks (especially with no_link option)
    router_mock_helpers:unload_all(),
    ok.

%% @doc Reset all active meck mocks (no-op if none).
-spec reset_mocks() -> ok.
reset_mocks() ->
    case catch meck:mocked() of
        {'EXIT', _} -> ok;
        Modules when is_list(Modules) ->
            lists:foreach(fun(M) -> catch meck:reset(M) end, Modules),
            ok
    end.

%% @doc Ensure a named ETS table exists and is empty (default set/public).
-spec ensure_ets(atom()) -> ets:tid() | atom().
ensure_ets(Name) ->
    router_test_init:ensure_ets_table(Name, [named_table, set, public]).

%% @doc Wait until predicate returns true or timeout (ms) elapses.
-spec wait_until(fun(() -> boolean()), non_neg_integer()) -> ok | no_return().
wait_until(Fun, TimeoutMs) ->
    test_helpers:wait_for_condition(Fun, TimeoutMs).

%% @doc Clear known fault-injection hooks (best-effort).
-spec ensure_no_faults() -> ok.
ensure_no_faults() ->
    catch router_nats_fault_injection:clear_all_faults(),
    ok.

%% @doc Wait for router to be fully started (supervisor and key children alive).
%% Returns ok when router is ready, or exits with timeout error.
%% This is the canonical "is router up?" check for CP1 suites.
-spec await_router_started(TimeoutMs :: non_neg_integer()) -> ok | no_return().
await_router_started(TimeoutMs) ->
    Start = erlang:monotonic_time(millisecond),
    await_router_started_loop(Start, TimeoutMs).

await_router_started_loop(Start, TimeoutMs) ->
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    case Elapsed >= TimeoutMs of
        true ->
            SupStatus = whereis(beamline_router_sup),
            ct:fail({router_not_started_within_timeout, elapsed_ms, Elapsed, sup_status, SupStatus});
        false ->
            case router_is_ready() of
                true -> ok;
                false ->
                    timer:sleep(50),
                    await_router_started_loop(Start, TimeoutMs)
            end
    end.

%% @doc Check if router is ready (supervisor up + key children alive).
-spec router_is_ready() -> boolean().
router_is_ready() ->
    case whereis(beamline_router_sup) of
        undefined -> false;
        SupPid when is_pid(SupPid) ->
            case is_process_alive(SupPid) of
                false -> false;
                true ->
                    %% Check that at least some key children are running
                    case catch supervisor:which_children(beamline_router_sup) of
                        Children when is_list(Children), length(Children) > 0 ->
                            %% Verify at least one child is alive
                            AliveCount = length([1 || {_Id, Pid, _Type, _Mods} <- Children,
                                                       is_pid(Pid) andalso is_process_alive(Pid)]),
                            AliveCount > 0;
                        _ -> false
                    end
            end
    end.
