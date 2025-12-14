-module(router_backpressure_test_helpers).

-export([
    wait_for_backpressure/2,
    wait_for_backpressure/3,
    wait_for_rejection_count/2,
    wait_for_rejection_count/3
]).

-define(DEFAULT_SUBJECT, <<"beamline.router.v1.decide">>).

wait_for_backpressure(Status, TimeoutMs) ->
    wait_for_backpressure(Status, ?DEFAULT_SUBJECT, TimeoutMs).

wait_for_backpressure(ExpectedStatus, Subject, TimeoutMs) ->
    Start = erlang:system_time(millisecond),
    Deadline = Start + TimeoutMs,
    do_wait_status(ExpectedStatus, Subject, Start, Deadline).

do_wait_status(ExpectedStatus, Subject, Start, Deadline) ->
    Now = erlang:system_time(millisecond),
    case Now >= Deadline of
        true -> {error, timeout};
        false ->
            case router_intake_backpressure:get_backpressure_status(Subject) of
                ExpectedStatus -> ok;
                _ ->
                    timer:sleep(50),
                    do_wait_status(ExpectedStatus, Subject, Start, Deadline)
            end
    end.

wait_for_rejection_count(Count, TimeoutMs) ->
    wait_for_rejection_count(Count, ?DEFAULT_SUBJECT, TimeoutMs).

wait_for_rejection_count(ExpectedCount, _Subject, TimeoutMs) ->
    Start = erlang:system_time(millisecond),
    Deadline = Start + TimeoutMs,
    do_wait_nak(ExpectedCount, Start, Deadline).

do_wait_nak(ExpectedCount, Start, Deadline) ->
    Now = erlang:system_time(millisecond),
    case Now >= Deadline of
        true -> {error, timeout};
        false ->
            Calls = try meck:num_calls(router_jetstream, nak, '_') catch _:_ -> 0 end,
            case Calls >= ExpectedCount of
                true -> ok;
                false ->
                    timer:sleep(50),
                    do_wait_nak(ExpectedCount, Start, Deadline)
            end
    end.
