%% Shared helpers used by multiple Common Test suites.
%%
%% Rules:
%% - No module-level warning suppression.
%% - Avoid `catch`; use try/catch.
%%
-module(router_suite_helpers).

-export([
    sleep/1,
    wait_for_condition/2,
    reset_mocks/0,
    ensure_no_faults/0,
    router_is_ready/0,
    wait_for_router/1,
    await_router_started/1,
    start_router_suite/0,
    stop_router_suite/0
]).

-spec sleep(non_neg_integer()) -> ok.
sleep(Ms) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    ok.

-spec wait_for_condition(fun(() -> boolean()), non_neg_integer()) -> ok.
wait_for_condition(Fun, TimeoutMs) when is_function(Fun, 0), is_integer(TimeoutMs), TimeoutMs > 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_condition_loop(Fun, TimeoutMs, Deadline).

-spec reset_mocks() -> ok.
reset_mocks() ->
    Modules = try meck:mocked() of
        Ms when is_list(Ms) -> Ms;
        _ -> []
    catch
        _:_ -> []
    end,
    lists:foreach(fun(M) ->
        _ = try meck:reset(M) of
            _ -> ok
        catch
            _:_ -> ok
        end
    end, Modules),
    ok.

-spec ensure_no_faults() -> ok.
ensure_no_faults() ->
    _ = try router_nats_fault_injection:clear_all_faults() of
        _ -> ok
    catch
        _:_ -> ok
    end,
    ok.

-spec router_is_ready() -> boolean().
router_is_ready() ->
    case whereis(beamline_router_sup) of
        undefined ->
            false;
        _ ->
            ChildrenList = try supervisor:which_children(beamline_router_sup) of
                ChildSpecs when is_list(ChildSpecs) -> ChildSpecs;
                _ -> []
            catch
                _:_ -> []
            end,
            case ChildrenList of
                List when is_list(List), length(List) > 0 -> true;
                _ -> false
            end
    end.

-spec wait_for_router(non_neg_integer()) -> ok.
wait_for_router(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 ->
    wait_for_condition(fun router_is_ready/0, TimeoutMs).

-spec await_router_started(non_neg_integer()) -> ok.
await_router_started(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    await_router_started_loop(TimeoutMs, Deadline).

-spec start_router_suite() -> ok.
start_router_suite() ->
    router_test_utils:start_router_app().

-spec stop_router_suite() -> ok.
stop_router_suite() ->
    router_test_utils:stop_router_app().

%% Internal

-spec wait_for_condition_loop(fun(() -> boolean()), non_neg_integer(), integer()) -> ok.
wait_for_condition_loop(Fun, TimeoutMs, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    if
        Now >= Deadline ->
            ct:fail("Condition not met within ~p ms", [TimeoutMs]);
        true ->
            case Fun() of
                true -> ok;
                false ->
                    timer:sleep(25),
                    wait_for_condition_loop(Fun, TimeoutMs, Deadline)
            end
    end.

-spec await_router_started_loop(non_neg_integer(), integer()) -> ok.
await_router_started_loop(TimeoutMs, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case router_is_ready() of
        true ->
            ok;
        false when Now >= Deadline ->
            ct:fail({router_not_started_within_timeout, TimeoutMs, Now, Deadline, router_status()});
        false ->
            timer:sleep(25),
            await_router_started_loop(TimeoutMs, Deadline)
    end.

-spec router_status() -> term().
router_status() ->
    case whereis(beamline_router_sup) of
        undefined ->
            not_running;
        Pid ->
            Children = try supervisor:which_children(beamline_router_sup) of
                C when is_list(C) -> C;
                _ -> []
            catch
                _:_ -> []
            end,
            {running, Pid, length(Children)}
    end.
