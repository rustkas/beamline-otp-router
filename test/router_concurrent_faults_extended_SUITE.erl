%% @doc Extended Concurrent Fault Scenarios
%% 
%% Extended scenarios A-E for concurrent fault testing.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category concurrent_faults, heavy, slow
-module(router_concurrent_faults_extended_SUITE).
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
    test_scenario_a_connect_and_publish_detailed/1,
    test_scenario_b_connect_and_publish_with_ack_detailed/1,
    test_scenario_c_ack_nak_and_tenant_validation_fail/1,
    test_scenario_d_jetstream_outage/1,
    test_scenario_e_nats_flapping_connection/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, extended_scenarios}];
        _ -> []
    end.

groups() ->
    [{extended_scenarios, [sequence], [
        test_scenario_a_connect_and_publish_detailed,
        test_scenario_b_connect_and_publish_with_ack_detailed,
        test_scenario_c_ack_nak_and_tenant_validation_fail,
        test_scenario_d_jetstream_outage,
        test_scenario_e_nats_flapping_connection
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

test_scenario_a_connect_and_publish_detailed(_Config) ->
    ct:comment("=== Scenario A: Connect + Publish Detailed ==="),
    
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    %% Phase 1: Enable connection fault
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(1000),
    
    %% Phase 2: Add publish fault
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_concurrent_faults_helper:send_message_batch(<<"acme">>, 10, <<"req-a">>),
    timer:sleep(1000),
    
    %% Get metrics
    FaultMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    %% Phase 3: Recover connection
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(1000),
    
    %% Phase 4: Recover publish
    router_nats_fault_injection:disable_fault(publish),
    timer:sleep(2000),
    
    FinalMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FaultPublishFailures = maps:get(router_nats_publish_failures_total, FaultMetrics, 0),
    ct:comment("Publish failures increased: ~p -> ~p", [InitialPublishFailures, FaultPublishFailures]),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_scenario_b_connect_and_publish_with_ack_detailed(_Config) ->
    ct:comment("=== Scenario B: Connect + Publish with ACK Detailed ==="),
    
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    Faults = [
        {connect, {error, connection_refused}},
        {publish_with_ack, {error, nats_unavailable}},
        {ack, {error, timeout}}
    ],
    
    {_, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 15, <<"req-b">>) end,
        2000
    ),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_scenario_c_ack_nak_and_tenant_validation_fail(_Config) ->
    ct:comment("=== Scenario C: ACK/NAK + Tenant Validation Fail ==="),
    
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    Faults = [
        {ack, {error, timeout}},
        {nak, {error, connection_refused}}
    ],
    
    {_, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() ->
            %% Mix valid and invalid tenants
            router_concurrent_faults_helper:send_test_message(<<"invalid_tenant">>, <<"req-c1">>, #{}),
            router_concurrent_faults_helper:send_test_message(<<"acme">>, <<"req-c2">>, #{}),
            router_concurrent_faults_helper:send_test_message(<<"invalid_too">>, <<"req-c3">>, #{}),
            router_concurrent_faults_helper:send_test_message(<<"acme">>, <<"req-c4">>, #{})
        end,
        2000
    ),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_scenario_d_jetstream_outage(_Config) ->
    ct:comment("=== Scenario D: JetStream Outage ==="),
    
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    %% Simulate JetStream outage
    Faults = [
        {connect, {error, connection_refused}},
        {subscribe, {error, jetstream_unavailable}},
        {publish_with_ack, {error, stream_not_found}}
    ],
    
    {_, _FaultMetrics, FinalMetrics} = router_concurrent_faults_helper:run_fault_injection_lifecycle(
        Faults,
        fun() -> router_concurrent_faults_helper:send_message_batch(<<"acme">>, 10, <<"req-d">>) end,
        3000
    ),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.

test_scenario_e_nats_flapping_connection(_Config) ->
    ct:comment("=== Scenario E: NATS Flapping Connection ==="),
    
    InitialMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    %% Flapping: rapid on/off cycles
    lists:foreach(fun(_) ->
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        router_concurrent_faults_helper:send_test_message(<<"acme">>, <<"req-flap">>, #{}),
        timer:sleep(300),
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(300)
    end, lists:seq(1, 5)),
    
    timer:sleep(2000),
    
    FinalMetrics = router_concurrent_faults_helper:get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_pid(RouterPid)),
    ?assert(is_process_alive(RouterPid)),
    
    router_concurrent_faults_helper:verify_test_scenario([], InitialMetrics, FinalMetrics, true),
    ok.
