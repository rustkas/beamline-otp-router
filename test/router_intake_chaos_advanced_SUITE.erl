%% @doc Intake Chaos: Advanced Failure Tests
%%
%% Advanced failure scenarios:
%% - Randomized failures
%% - Failures during message processing
%%
%% @test_category chaos, heavy, intake
-module(router_intake_chaos_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_randomized_failures/1,
    test_during_message_processing/1
]).

suite() -> [{timetrap, {minutes, 20}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        _ -> []
    end.

groups() ->
    [{advanced_tests, [sequence], [
        test_randomized_failures,
        test_during_message_processing
    ]}].

init_per_suite(Config) ->
    RandSeed = case os:getenv("CHAOS_RAND_SEED") of
        false -> erlang:timestamp();
        SeedStr ->
            case string:tokens(SeedStr, ",") of
                [S1, S2, S3] -> {list_to_integer(S1), list_to_integer(S2), list_to_integer(S3)};
                _ -> {1234, 5678, 91011}
            end
    end,
    _ = rand:seed(exsplus, RandSeed),
    
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(router_logger, [passthrough, non_strict]),
    meck:expect(router_logger, warning, fun(_Msg, _Args) -> ok end),
    meck:expect(router_logger, error, fun(_Msg, _Args) -> ok end),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> timer:sleep(500), [{rand_seed, RandSeed} | Config];
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload_all([router_nats, router_logger]),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_randomized_failures(_Config) ->
    ct:comment("=== Randomized NATS Failures ==="),
    
    NumCycles = 10,
    ProcessCountBefore = erlang:system_info(process_count),
    
    FailureCounter = router_test_init:ensure_ets_table(failure_counter, [set, public, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(FailureCounter, {failures, 0}),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        ShouldFail = rand:uniform(100) =< 30,
        case ShouldFail of
            true ->
                FailureCount = case ets:lookup(FailureCounter, failures) of
                    [] -> 0;
                    [{failures, C}] -> C
                end,
                ets:insert(FailureCounter, {failures, FailureCount + 1}),
                {error, connection_refused};
            false ->
                ok
        end
    end),
    
    lists:foreach(fun(Cycle) ->
        ct:comment("Cycle ~p/~p", [Cycle, NumCycles]),
        lists:foreach(fun(_) ->
            _ = router_nats:publish(<<"test.subject">>, <<"payload">>)
        end, lists:seq(1, 20)),
        timer:sleep(500)
    end, lists:seq(1, NumCycles)),
    
    [{failures, TotalFailures}] = ets:lookup(FailureCounter, failures),
    ct:comment("Total failures simulated: ~p", [TotalFailures]),
    
    ProcessCountAfter = erlang:system_info(process_count),
    ProcessGrowth = (ProcessCountAfter - ProcessCountBefore) / max(ProcessCountBefore, 1),
    ?assert(ProcessGrowth < 0.3),
    ?assert(TotalFailures > 0, "Should have some failures"),
    
    ets:delete(FailureCounter),
    ok.

test_during_message_processing(_Config) ->
    ct:comment("=== Failures During Message Processing ==="),
    
    ProcessedMessages = router_test_init:ensure_ets_table(processed_messages, [set, public, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(ProcessedMessages, {count, 0}),
    ets:insert(ProcessedMessages, {errors, 0}),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        timer:sleep(10),
        ShouldFail = rand:uniform(100) =< 20,
        case ShouldFail of
            true ->
                [{errors, E}] = ets:lookup(ProcessedMessages, errors),
                ets:insert(ProcessedMessages, {errors, E + 1}),
                {error, timeout};
            false ->
                [{count, C}] = ets:lookup(ProcessedMessages, count),
                ets:insert(ProcessedMessages, {count, C + 1}),
                ok
        end
    end),
    
    %% Simulate concurrent message processing
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            lists:foreach(fun(_) ->
                _ = router_nats:publish(<<"test.msg.", (integer_to_binary(N))/binary>>, <<"payload">>),
                timer:sleep(50)
            end, lists:seq(1, 10))
        end)
    end, lists:seq(1, 5)),
    
    %% Wait for all processes to complete
    lists:foreach(fun(Pid) ->
        MRef = monitor(process, Pid),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 30000 ->
            exit(Pid, kill)
        end
    end, Pids),
    
    [{count, SuccessCount}] = ets:lookup(ProcessedMessages, count),
    [{errors, ErrorCount}] = ets:lookup(ProcessedMessages, errors),
    
    ct:comment("Processed: ~p success, ~p errors", [SuccessCount, ErrorCount]),
    
    TotalMessages = SuccessCount + ErrorCount,
    ?assert(TotalMessages > 0, "Should have processed messages"),
    ?assert(SuccessCount > ErrorCount, "Should have more successes than errors"),
    
    ets:delete(ProcessedMessages),
    ok.
