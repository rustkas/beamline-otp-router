-module(router_advanced_concurrent_faults_SUITE).

%% @doc Advanced concurrent fault scenarios (triple, mixed, cascading).
%% Runs a smoke subset in CI, full set in heavy/full levels.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0, groups_for_level/1, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_triple_fault_scenario_a_connect_publish_ack/1,
    test_triple_fault_scenario_b_connect_publish_nak/1,
    test_triple_fault_scenario_c_flapping_connect_publish_ack/1,
    test_mixed_intermittent_connect_persistent_publish/1,
    test_mixed_persistent_connect_intermittent_ack/1,
    test_cascading_connect_publish_ack_chain/1,
    test_cascading_multiple_recovery_cycles/1
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
    [{group, triple_fault_tests}, {group, mixed_pattern_tests}, {group, cascading_fault_tests}];
groups_for_level(full) ->
    [{group, triple_fault_tests}, {group, mixed_pattern_tests}, {group, cascading_fault_tests}];
groups_for_level(fast) ->
    [{group, triple_fault_smoke}].

groups() ->
    [
        {triple_fault_smoke, [sequence], [
            test_triple_fault_scenario_a_connect_publish_ack
        ]},
        {triple_fault_tests, [sequence], [
            test_triple_fault_scenario_a_connect_publish_ack,
            test_triple_fault_scenario_b_connect_publish_nak,
            test_triple_fault_scenario_c_flapping_connect_publish_ack
        ]},
        {mixed_pattern_tests, [sequence], [
            test_mixed_intermittent_connect_persistent_publish,
            test_mixed_persistent_connect_intermittent_ack
        ]},
        {cascading_fault_tests, [sequence], [
            test_cascading_connect_publish_ack_chain,
            test_cascading_multiple_recovery_cycles
        ]}
    ].

init_per_suite(Config) ->
    router_concurrent_faults_helper:init_common_suite(Config).

end_per_suite(Config) ->
    router_concurrent_faults_helper:end_common_suite(Config).

init_per_testcase(_TestCase, Config) ->
    router_concurrent_faults_helper:init_common_testcase(Config).

end_per_testcase(_TestCase, Config) ->
    router_concurrent_faults_helper:end_common_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_triple_fault_scenario_a_connect_publish_ack(_Config) ->
    ct:comment("=== Triple Fault A: connect + publish + ack ==="),
    Faults = [
        {connect, {error, connection_refused}},
        {publish, {error, timeout}},
        {ack, {error, timeout}}
    ],
    {InitialMetrics, _FaultMetrics, FinalMetrics} =
        router_concurrent_faults_helper:run_fault_injection_lifecycle(
            Faults,
            fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 8, <<"adv-a">>) end,
            2000
        ),
    RouterPid = whereis(router_result_consumer),
    ?assert(RouterPid =:= undefined orelse is_process_alive(RouterPid)),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_triple_fault_scenario_b_connect_publish_nak(_Config) ->
    ct:comment("=== Triple Fault B: connect + publish + nak ==="),
    Faults = [
        {connect, close_connection},
        {publish, {error, timeout}},
        {nak, {error, connection_refused}}
    ],
    {InitialMetrics, _FaultMetrics, FinalMetrics} =
        router_concurrent_faults_helper:run_fault_injection_lifecycle(
            Faults,
            fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 6, <<"adv-b">>) end,
            2000
        ),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_triple_fault_scenario_c_flapping_connect_publish_ack(_Config) ->
    ct:comment("=== Triple Fault C: flapping connect + publish + ack ==="),
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    lists:foreach(fun(_) ->
        router_nats_fault_injection:enable_fault(connect, close_connection),
        router_concurrent_faults_helper:send_test_message(<<"acme">>, <<"adv-c">>, #{}),
        timer:sleep(200),
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(200)
    end, lists:seq(1, 5)),
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:disable_fault(ack),
    timer:sleep(1500),
    FinalMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_mixed_intermittent_connect_persistent_publish(_Config) ->
    ct:comment("=== Mixed: intermittent connect + persistent publish ==="),
    Faults = [
        {connect, {intermittent, close_connection, 0.5}},
        {publish, {error, quota_exceeded}}
    ],
    {InitialMetrics, _FaultMetrics, FinalMetrics} =
        router_concurrent_faults_helper:run_fault_injection_lifecycle(
            Faults,
            fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 6, <<"adv-m1">>) end,
            2000
        ),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_mixed_persistent_connect_intermittent_ack(_Config) ->
    ct:comment("=== Mixed: persistent connect + intermittent ack ==="),
    Faults = [
        {connect, {error, connection_refused}},
        {ack, {intermittent, {error, timeout}, 0.3}}
    ],
    {InitialMetrics, _FaultMetrics, FinalMetrics} =
        router_concurrent_faults_helper:run_fault_injection_lifecycle(
            Faults,
            fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 6, <<"adv-m2">>) end,
            2000
        ),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_cascading_connect_publish_ack_chain(_Config) ->
    ct:comment("=== Cascading: connect -> publish -> ack ==="),
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(500),
    router_nats_fault_injection:disable_fault(connect),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    timer:sleep(500),
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    router_concurrent_faults_helper:send_message_batch(<<"acme">>, 5, <<"adv-casc">>),
    timer:sleep(500),
    router_nats_fault_injection:disable_fault(ack),
    timer:sleep(1500),
    FinalMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_cascading_multiple_recovery_cycles(_Config) ->
    ct:comment("=== Cascading: multiple recovery cycles ==="),
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    lists:foreach(fun(Cycle) ->
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        router_concurrent_faults_helper:send_test_message(<<"acme">>, <<"adv-cycle-", (integer_to_binary(Cycle))/binary>>, #{}),
        timer:sleep(300),
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(500)
    end, lists:seq(1, 3)),
    FinalMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.
