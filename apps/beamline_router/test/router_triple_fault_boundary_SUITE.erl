%% @doc Triple-Fault Contract: Boundary and Edge Cases
%%
%% Boundary value and edge case tests:
%% - MaxDeliver boundary
%% - MaxRedelivery boundary
%% - Partial recovery
%% - Delayed ACK/NAK
%% - Metrics degradation
%% - AckPolicy variations
%% - DeliverPolicy variations
%%
%% @test_category fault_injection, triple_fault, contract, heavy
-module(router_triple_fault_boundary_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_maxdeliver_boundary/1,
    test_maxredelivery_boundary/1,
    test_partial_recovery/1,
    test_delayed_ack_nak/1,
    test_metrics_degradation/1,
    test_ackpolicy_variations/1,
    test_deliverpolicy_variations/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, boundary_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, boundary_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, boundary_tests}];
        _ -> []
    end.
groups() ->
    [{boundary_tests, [sequence], [
        test_maxdeliver_boundary,
        test_maxredelivery_boundary,
        test_partial_recovery,
        test_delayed_ack_nak,
        test_metrics_degradation,
        test_ackpolicy_variations,
        test_deliverpolicy_variations
    ]}].

init_per_suite(Config) -> router_triple_fault_helper:init_suite(Config).
end_per_suite(Config) -> router_triple_fault_helper:end_suite(Config).
init_per_testcase(_TC, Config) -> router_triple_fault_helper:init_testcase(Config).
end_per_testcase(_TC, Config) -> router_triple_fault_helper:end_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_maxdeliver_boundary(_Config) ->
    ct:comment("=== Triple-Fault: MaxDeliver Boundary ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    %% Enable faults that cause MaxDeliver to be reached
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-maxdel-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 5)]
    end),
    
    timer:sleep(3000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_maxredelivery_boundary(_Config) ->
    ct:comment("=== Triple-Fault: MaxRedelivery Boundary ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(nak, {error, timeout}),
    router_nats_fault_injection:enable_fault(connect, {intermittent, {error, connection_refused}, 0.5}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-maxredel-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 5)]
    end),
    
    timer:sleep(3000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_partial_recovery(_Config) ->
    ct:comment("=== Triple-Fault: Partial Recovery ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    %% Enable all faults
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-partial-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 5)]
    end),
    
    timer:sleep(1500),
    
    %% Partial recovery: disable only some faults
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(1500),
    
    router_nats_fault_injection:disable_fault(publish),
    timer:sleep(1500),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_delayed_ack_nak(_Config) ->
    ct:comment("=== Triple-Fault: Delayed ACK/NAK ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(ack, {delayed, {ok, acked}, 500}),
    router_nats_fault_injection:enable_fault(nak, {delayed, {ok, nacked}, 500}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-delayed-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 5)]
    end),
    
    timer:sleep(4000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_metrics_degradation(_Config) ->
    ct:comment("=== Triple-Fault: Metrics Degradation ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {intermittent, {error, connection_refused}, 0.3}),
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.3}),
    router_nats_fault_injection:enable_fault(ack, {intermittent, {error, timeout}, 0.3}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-metrics-", (integer_to_binary(N))/binary>>, #{}) ||
         N <- lists:seq(1, 20)]
    end),
    
    timer:sleep(5000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 100}),
    ok.

test_ackpolicy_variations(_Config) ->
    ct:comment("=== Triple-Fault: AckPolicy Variations ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-ack-", (integer_to_binary(N))/binary>>, #{
            <<"ack_policy">> => <<"explicit">>
        }) || N <- lists:seq(1, 5)]
    end),
    
    timer:sleep(3000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.

test_deliverpolicy_variations(_Config) ->
    ct:comment("=== Triple-Fault: DeliverPolicy Variations ==="),
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(nak, {error, timeout}),
    
    spawn(fun() ->
        [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-deliver-", (integer_to_binary(N))/binary>>, #{
            <<"deliver_policy">> => <<"new">>
        }) || N <- lists:seq(1, 5)]
    end),
    
    timer:sleep(3000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    ok.
