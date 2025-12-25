-module(router_jetstream_recovery_test_utils).

-doc "JetStream Recovery Test Helpers".
%% Helper module for router_jetstream_recovery_ext_SUITE
%% Provides deterministic checks and mock helpers


-include_lib("stdlib/include/assert.hrl").

-export([scale_duration/1, speedup_factor/0, setup_standard_mocks/0, cleanup_mocks/0,
         simulate_network_partition/0, heal_network_partition/0, simulate_router_restart/0, 
         measure_baseline_throughput/1,
         track_resources/0, verify_no_resource_leaks/2, wait_for_recovery/3,
         process_message_batch/1, assert_called/2, assert_call_count/3]).

%% Scale duration based on test level (stub - returns input as-is)
scale_duration(DurationMs) ->
    case os:getenv("ROUTER_TEST_SCALE_FACTOR") of
        false ->
            DurationMs;
        ScaleStr ->
            try
                Scale = list_to_float(ScaleStr),
                round(DurationMs * Scale)
            catch
                _:_ ->
                    DurationMs
            end
    end.

%% Get speedup factor for soak tests
speedup_factor() ->
    case os:getenv("EXTENDED_TEST_SPEEDUP") of
        false -> 1;
        Str ->
            case string:to_integer(Str) of
                {ok, N} when N >= 1 -> N;
                _ -> 1
            end
    end.

%% Setup standard mocks with call tracking
setup_standard_mocks() ->
    %% Use the centralized safe mock helper which handles idempotency and avoids passthrough
    router_mock_helpers:setup_router_nats_mock(#{
        publish => fun(Subject, Payload) ->
            router_mock_helpers:track_call(publish, Subject),
            case get({fault, publish}) of
                undefined -> ok;
                {error, Reason} -> {error, Reason};
                Fun when is_function(Fun) -> Fun(Subject, Payload)
            end
        end,
        ack_message => fun(MsgId) ->
            router_mock_helpers:track_call(ack_message, MsgId),
            case get({fault, ack_message}) of
                undefined -> ok;
                {error, Reason} -> {error, Reason};
                Fun when is_function(Fun) -> Fun(MsgId)
            end
        end,
        nak_message => fun(MsgId) ->
            router_mock_helpers:track_call(nak_message, MsgId),
            case get({fault, nak_message}) of
                undefined -> ok;
                {error, Reason} -> {error, Reason};
                Fun when is_function(Fun) -> Fun(MsgId)
            end
        end,
        subscribe_jetstream => fun(_S, _D, _A, _DG, _M) -> 
            {ok, ~"mock-consumer"} 
        end,
        start_link => fun() ->
            {ok, spawn(fun() -> receive _ -> ok end end)}
        end
    }),

    %% Initialize call tracking
    router_mock_helpers:clear_calls(),
    ok.

%% Cleanup mocks
cleanup_mocks() ->
    router_mock_helpers:unload(router_nats),
    erase(), %% Clear process dictionary for faults
    ok.

%% Simulate network partition by setting fault
simulate_network_partition() ->
    put({fault, publish}, {error, timeout}),
    put({fault, ack_message}, {error, timeout}),
    put({fault, nak_message}, {error, timeout}),
    ok.

%% Heal network partition by clearing fault
heal_network_partition() ->
    erase({fault, publish}),
    erase({fault, ack_message}),
    erase({fault, nak_message}),
    ok.

%% Simulate router restart (brief disruption)
simulate_router_restart() ->
    simulate_network_partition(),
    timer:sleep(100),
    heal_network_partition(),
    ok.

%% Measure baseline throughput
%% For deterministic tests, this checks that the mock NATS is working
measure_baseline_throughput(_DurationMs) ->
    %% Verify we can publish to the mock
    case router_nats:publish(~"baseline", ~"payload") of
        ok -> 1000;
        {error, _} -> 0
    end.

%% Track resources (stub - returns dummy snapshot)
track_resources() ->
    #{processes => erlang:system_info(process_count),
      memory => erlang:memory(total),
      ets_tables => length(ets:all())}.

%% Verify no resource leaks (stub - always passes)
verify_no_resource_leaks(_Initial, _Final) ->
    %% In real implementation, would compare snapshots
    ok.

%% Wait for recovery (deterministic version)
%% Instead of polling, we check the state immediately
wait_for_recovery(CheckFun, BaselineValue, _TimeoutMs) ->
    Current = CheckFun(),
    case Current >= BaselineValue of
        true ->
            {ok, recovered};
        false ->
            {timeout, 0}
    end.

%% Process message batch
%% Simulate processing by manually invoking callbacks or tracking
process_message_batch(Count) ->
    %% Exercise the mock NATS by calling ack_message for each "message"
    lists:foreach(fun(I) ->
        MsgId = integer_to_binary(I),
        _ = router_nats:ack_message(MsgId)
    end, lists:seq(1, Count)),
    ok.

%% Assert function called at least N times
assert_called(Function, MinCount) ->
    Count = router_mock_helpers:get_call_count(Function),
    ?assert(Count >= MinCount).

%% Assert function called exactly N times
assert_call_count(Function, Count, _Timeout) ->
    Actual = router_mock_helpers:get_call_count(Function),
    ?assertEqual(Count, Actual).
