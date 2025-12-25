%% @doc Single-Instance Network Partition Tests
%% 
%% Tests for network partitions affecting a single router instance.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category network_partition, heavy, slow
-module(router_network_partition_single_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_partial_partition/1,
    test_full_partition/1,
    test_partition_healing/1,
    test_jetstream_partition_short/1,
    test_jetstream_partition_recovery/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, single_instance_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, single_instance_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, single_instance_tests}];
        _ -> []
    end.
groups() ->
    [{single_instance_tests, [sequence], [
        test_partial_partition,
        test_full_partition,
        test_partition_healing,
        test_jetstream_partition_short,
        test_jetstream_partition_recovery
    ]}].

init_per_suite(Config) ->
    router_network_partition_helper:init_common_suite(Config).

end_per_suite(Config) ->
    router_network_partition_helper:end_common_suite(Config).

init_per_testcase(_TestCase, Config) ->
    router_network_partition_helper:init_common_testcase(Config).

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_partial_partition(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(2000),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(3000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    ?assert(is_process_alive(RouterPid)),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

test_full_partition(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, connection_refused}),
    timer:sleep(2000),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    router_nats_fault_injection:disable_fault(connect),
    router_nats_fault_injection:disable_fault(publish),
    timer:sleep(3000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    ?assert(is_process_alive(RouterPid)),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

test_partition_healing(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(5000),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(5000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    ?assert(is_process_alive(RouterPid)),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

test_jetstream_partition_short(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(5000),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(3000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    ?assert(is_process_alive(RouterPid)),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

test_jetstream_partition_recovery(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(5000),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(5000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    ?assert(is_process_alive(RouterPid)),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.
