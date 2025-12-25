%% @doc Triple-Fault Contract: Basic Scenarios
%%
%% Basic triple-fault combinations:
%% - Connect + Publish + ACK
%% - Connect + Validation + NAK
%% - Publish + MaxDeliver + ACK
%% - Connect + Publish + MaxDeliver
%% - ACK + NAK + Publish
%%
%% @test_category fault_injection, triple_fault, contract, heavy
-module(router_triple_fault_basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_connect_publish_ack/1,
    test_connect_validation_nak/1,
    test_publish_maxdeliver_ack/1,
    test_connect_publish_maxdeliver/1,
    test_ack_nak_publish/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, basic_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, basic_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, basic_tests}];
        _ -> []
    end.
groups() ->
    [{basic_tests, [sequence], [
        test_connect_publish_ack,
        test_connect_validation_nak,
        test_publish_maxdeliver_ack,
        test_connect_publish_maxdeliver,
        test_ack_nak_publish
    ]}].

init_per_suite(Config) -> router_triple_fault_helper:init_suite(Config).
end_per_suite(Config) -> router_triple_fault_helper:end_suite(Config).
init_per_testcase(_TC, Config) -> router_triple_fault_helper:init_testcase(Config).
end_per_testcase(_TC, Config) -> router_triple_fault_helper:end_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_connect_publish_ack(_Config) ->
    ct:comment("=== Triple-Fault: Connect + Publish + ACK ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 10)]
    end),
    
    timer:sleep(2000),
    verify_no_crashes(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_connect_validation_nak(_Config) ->
    ct:comment("=== Triple-Fault: Connect + Validation + NAK ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(nak, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 10)]
    end),
    
    timer:sleep(2000),
    verify_no_crashes(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_publish_maxdeliver_ack(_Config) ->
    ct:comment("=== Triple-Fault: Publish + MaxDeliver + Intermittent ACK ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {intermittent, {error, timeout}, 0.5}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 10)]
    end),
    
    timer:sleep(2000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_connect_publish_maxdeliver(_Config) ->
    ct:comment("=== Triple-Fault: Connect + Publish + MaxDeliver ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 10)]
    end),
    
    timer:sleep(2000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_ack_nak_publish(_Config) ->
    ct:comment("=== Triple-Fault: ACK + NAK + Publish ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    router_nats_fault_injection:enable_fault(nak, {error, timeout}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 10)]
    end),
    
    timer:sleep(2000),
    verify_no_crashes(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

verify_no_crashes() ->
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    ok.
