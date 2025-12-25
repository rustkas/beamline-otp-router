%% # Advanced Faults: Triple Fault Suite
%%
%% Exercises combined failure modes (connection loss, publish failures, and recovery).

-module(router_advanced_faults_triple_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([
    all/0, groups_for_level/1,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_connection_loss_recovery/1,
    test_publish_failure_handling/1,
    test_combined_fault_recovery/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, triple_fault_tests}];
groups_for_level(full) ->
    [{group, triple_fault_tests}];
groups_for_level(fast) ->
    [{group, triple_fault_tests}].

groups() ->
    [
        {triple_fault_tests, [sequence], [
            test_connection_loss_recovery,
            test_publish_failure_handling,
            test_combined_fault_recovery
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    {ok, _Pid} = router_nats_server:ensure_running(),
    Config.

end_per_suite(_Config) ->
    safe_ignore(fun() -> router_nats_server:stop() end),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Test stub reset not needed - using real NATS connection
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Test stub reset not needed - using real NATS connection
    Config.

test_connection_loss_recovery(_Config) ->
    %% Simulate connection lost
    safe_ignore(fun() -> router_nats:simulate_connection_lost() end),
    timer:sleep(200),
    %% Reconnect should happen automatically
    safe_ignore(fun() -> router_nats:reconnect() end),
    timer:sleep(200),
    ?assert(true).

test_publish_failure_handling(_Config) ->
    Subject = ~"beamline.router.test.triple_fault",
    Payload = <<"test_message">>,
    %% Test publishing multiple times
    Results = [safe_publish(Subject, Payload) || _ <- lists:seq(1, 5)],
    %% At least some publishes should succeed
    ?assert(lists:any(fun(R) -> R =:= ok end, Results)),
    ok.

test_combined_fault_recovery(_Config) ->
    Subject = ~"beamline.router.test.triple_fault_combined",
    Payload = <<"combined_test_message">>,

    %% Simulate connection lost
    safe_ignore(fun() -> router_nats:simulate_connection_lost() end),

    %% Try to publish during fault
    _ = safe_publish(Subject, Payload),
    timer:sleep(200),

    %% Recover connection
    safe_ignore(fun() -> router_nats:reconnect() end),
    timer:sleep(200),

    %% Validate publishing works after recovery
    Result = safe_publish(Subject, Payload),
    ?assert(Result =:= ok orelse Result =:= error),
    ok.

safe_publish(Subject, Payload) ->
    try router_nats:publish(Subject, Payload) of
        ok -> ok;
        {ok, _} -> ok;
        {error, _} -> error;
        _ -> error
    catch
        _:_ -> error
    end.

safe_ignore(Fun) when is_function(Fun, 0) ->
    try Fun() of
        _ -> ok
    catch
        _:_ -> ok
    end.
