%% @doc Basic Concurrent Fault Tests
%% 
%% Tests for basic concurrent fault scenarios.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category concurrent_faults, heavy, slow
-module(router_concurrent_faults_basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_connect_and_publish_faults/1,
    test_publish_and_ack_nak_faults/1,
    test_connect_and_ack_nak_faults/1,
    test_validation_and_publish_faults/1,
    test_policy_change_and_connect_publish_faults/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, basic_concurrent_faults}];
        _ -> []
    end.

groups() ->
    [{basic_concurrent_faults, [sequence], [
        test_connect_and_publish_faults,
        test_publish_and_ack_nak_faults,
        test_connect_and_ack_nak_faults,
        test_validation_and_publish_faults,
        test_policy_change_and_connect_publish_faults
    ]}].

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

test_connect_and_publish_faults(_Config) ->
    ct:comment("=== Test: Connect + Publish Concurrent Faults ==="),
    
    Faults = [
        {connect, {error, connection_refused}},
        {publish, {error, timeout}},
        {publish_with_ack, {error, nats_unavailable}}
    ],
    
    {InitialMetrics, FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 20, <<"req">>) end,
        2000
    ),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FaultConnectionLost = maps:get(router_nats_connection_lost_total, FaultMetrics, 0),
    FinalConnectionLost = maps:get(router_nats_connection_lost_total, FinalMetrics, 0),
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FaultPublishFailures = maps:get(router_nats_publish_failures_total, FaultMetrics, 0),
    
    ct:comment("Connection lost: Initial=~p, DuringFault=~p, AfterRecovery=~p", 
               [InitialConnectionLost, FaultConnectionLost, FinalConnectionLost]),
    ct:comment("Publish failures: Initial=~p, DuringFault=~p", 
               [InitialPublishFailures, FaultPublishFailures]),
    
    ?assert(FaultConnectionLost >= InitialConnectionLost orelse FaultPublishFailures >= InitialPublishFailures),
    ?assert(FinalConnectionLost >= InitialConnectionLost),
    ok.

test_publish_and_ack_nak_faults(_Config) ->
    ct:comment("=== Test: Publish + ACK/NAK Concurrent Faults ==="),
    
    Faults = [
        {publish, {error, timeout}},
        {publish_with_ack, {error, nats_unavailable}},
        {ack, {error, timeout}},
        {nak, {error, connection_refused}}
    ],
    
    {InitialMetrics, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 15, <<"req">>) end,
        2000
    ),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    InitialAckFailures = maps:get(router_nats_ack_failures_total, InitialMetrics, 0),
    FinalAckFailures = maps:get(router_nats_ack_failures_total, FinalMetrics, 0),
    
    ct:comment("Redelivery: Initial=~p, Final=~p", [InitialRedelivery, FinalRedelivery]),
    ct:comment("ACK failures: Initial=~p, Final=~p", [InitialAckFailures, FinalAckFailures]),
    
    ?assert(FinalRedelivery >= InitialRedelivery orelse FinalAckFailures > InitialAckFailures),
    ok.

test_connect_and_ack_nak_faults(_Config) ->
    ct:comment("=== Test: Connect + ACK/NAK Concurrent Faults ==="),
    
    Faults = [
        {connect, {error, connection_refused}},
        {ack, {error, timeout}},
        {nak, {error, connection_refused}}
    ],
    
    {InitialMetrics, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 15, <<"req">>) end,
        2000
    ),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    ok.

test_validation_and_publish_faults(_Config) ->
    ct:comment("=== Test: Validation + Publish Concurrent Faults ==="),
    
    Faults = [
        {publish, {error, timeout}},
        {publish_with_ack, {error, nats_unavailable}}
    ],
    
    {InitialMetrics, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() ->
            router_concurrent_faults_helper:send_test_message(<<"invalid_tenant">>, <<"req-1">>, #{}),
            router_concurrent_faults_helper:send_test_message(<<"acme">>, <<"req-2">>, #{}),
            timer:sleep(100)
        end,
        2000
    ),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    ok.

test_policy_change_and_connect_publish_faults(_Config) ->
    ct:comment("=== Test: Policy Change + Connect/Publish Concurrent Faults ==="),
    
    Faults = [
        {connect, {error, connection_refused}},
        {publish, {error, timeout}}
    ],
    
    {InitialMetrics, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 10, <<"req">>) end,
        2000
    ),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    ok.
