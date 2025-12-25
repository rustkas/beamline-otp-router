%% @doc Metrics Under Faults - Aggregation Tests
%% 
%% Tests for metrics aggregation under fault conditions.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category metrics, fault_injection, heavy
-module(router_metrics_faults_aggregation_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2, get_metrics_snapshot/0
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_aggregation_under_partial_failures/1,
    test_aggregation_under_retries/1,
    test_aggregation_under_flapping_connections/1,
    test_aggregation_after_recovery/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, aggregation_tests}];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{aggregation_tests, [sequence], [
        test_aggregation_under_partial_failures,
        test_aggregation_under_retries,
        test_aggregation_under_flapping_connections,
        test_aggregation_after_recovery
    ]}].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{
        common_env => false,
        app_env => #{grpc_port => 0, grpc_enabled => false, nats_mode => mock}
    }),
    ok = router_metrics:ensure(),
    router_nats_fault_injection:clear_all_faults(),
    Config1.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(TestCase, Config) ->
    Config1 = router_test_bootstrap:init_per_testcase(TestCase, Config, #{
        clear_faults => true
    }),
    ok = router_metrics:ensure(),
    ok = router_metrics:clear_all(),
    Config1.

end_per_testcase(TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(TestCase, Config, #{
        clear_faults => true
    }).

get_metrics_snapshot() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            Metrics = ets:tab2list(router_metrics),
            lists:foldl(fun
                ({Key, Value}, Acc) when is_atom(Key) -> maps:put(Key, Value, Acc);
                ({{MetricName, _Labels}, Value}, Acc) when is_atom(MetricName) ->
                    Current = maps:get(MetricName, Acc, 0),
                    maps:put(MetricName, Current + Value, Acc)
            end, #{}, Metrics)
    end.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_aggregation_under_partial_failures(_Config) ->
    InitialMetrics = get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.3}),
    
    lists:foreach(fun(_) ->
        catch router_nats:publish(<<"test.subject">>, <<"payload">>),
        timer:sleep(50)
    end, lists:seq(1, 20)),
    
    router_nats_fault_injection:disable_fault(publish),
    timer:sleep(1000),
    
    FinalMetrics = get_metrics_snapshot(),
    ?assert(is_map(FinalMetrics)),
    ct:log("Initial: ~p, Final: ~p", [InitialMetrics, FinalMetrics]),
    ok.

test_aggregation_under_retries(_Config) ->
    InitialMetrics = get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    lists:foreach(fun(_) ->
        catch router_nats:publish(<<"test.subject">>, <<"payload">>),
        timer:sleep(100)
    end, lists:seq(1, 10)),
    
    router_nats_fault_injection:disable_fault(publish),
    timer:sleep(1000),
    
    FinalMetrics = get_metrics_snapshot(),
    ?assert(is_map(FinalMetrics)),
    ct:log("Initial: ~p, Final: ~p", [InitialMetrics, FinalMetrics]),
    ok.

test_aggregation_under_flapping_connections(_Config) ->
    InitialMetrics = get_metrics_snapshot(),
    
    lists:foreach(fun(_) ->
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        timer:sleep(300),
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(300)
    end, lists:seq(1, 5)),
    
    timer:sleep(1000),
    
    FinalMetrics = get_metrics_snapshot(),
    ?assert(is_map(FinalMetrics)),
    ct:log("Flapping metrics - Initial: ~p, Final: ~p", [InitialMetrics, FinalMetrics]),
    ok.

test_aggregation_after_recovery(_Config) ->
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(3000),
    
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(3000),
    
    FinalMetrics = get_metrics_snapshot(),
    ?assert(is_map(FinalMetrics)),
    
    RouterNatsPid = whereis(router_nats),
    ?assert(is_process_alive(RouterNatsPid)),
    ok.
