-module(router_nats_test_helpers).
-export([is_service_available/0, wait_for_status/2, wait_for_status_any/2, get_state/0, wait_for_pending_empty/1]).

is_service_available() ->
    case whereis(router_nats) of
        undefined -> false;
        Pid when is_pid(Pid) -> is_process_alive(Pid)
    end.

get_state() ->
    case catch router_nats:get_connection_status() of
        {ok, Status} when is_map(Status) -> maps:get(state, Status, undefined);
        _ -> undefined
    end.

wait_for_status(Expected, TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_status_loop(Expected, Deadline).

wait_for_status_loop(Expected, Deadline) ->
    Current = get_state(),
    case Current =:= Expected of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> ct:fail({nats_status_timeout, Expected, Current});
                false -> timer:sleep(100), wait_for_status_loop(Expected, Deadline)
            end
    end.

wait_for_status_any(ExpectedList, TimeoutMs) when is_list(ExpectedList), is_integer(TimeoutMs), TimeoutMs >= 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_status_any_loop(ExpectedList, Deadline).

wait_for_status_any_loop(ExpectedList, Deadline) ->
    Current = get_state(),
    case lists:member(Current, ExpectedList) of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> ct:fail({nats_status_timeout_any, ExpectedList, Current});
                false -> timer:sleep(100), wait_for_status_any_loop(ExpectedList, Deadline)
            end
    end.

wait_for_pending_empty(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_pending_empty_loop(Deadline).

wait_for_pending_empty_loop(Deadline) ->
    case catch router_nats:get_connection_status() of
        {ok, Status} when is_map(Status) ->
            case maps:get(pending_operations_count, Status, 0) of
                0 -> ok;
                _ ->
                    case erlang:monotonic_time(millisecond) >= Deadline of
                        true -> ct:fail(nats_pending_not_empty);
                        false -> timer:sleep(100), wait_for_pending_empty_loop(Deadline)
                    end
            end;
        _ ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> ct:fail(nats_status_unavailable);
                false -> timer:sleep(100), wait_for_pending_empty_loop(Deadline)
            end
    end.
