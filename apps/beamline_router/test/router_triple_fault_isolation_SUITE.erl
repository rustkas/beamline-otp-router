%% @doc Triple-Fault Contract: Multi-Tenant and Multi-Stream Scenarios
%%
%% Cross-tenant and multi-stream isolation tests:
%% - Multi-tenant isolation
%% - Multi-stream/subject scenarios
%% - Consumer group isolation
%%
%% @test_category fault_injection, triple_fault, contract, heavy
-module(router_triple_fault_isolation_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_multi_tenant_isolation/1,
    test_multi_stream_subject/1,
    test_consumer_group_isolation/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, isolation_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, isolation_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, isolation_tests}];
        _ -> []
    end.
groups() ->
    [{isolation_tests, [sequence], [
        test_multi_tenant_isolation,
        test_multi_stream_subject,
        test_consumer_group_isolation
    ]}].

init_per_suite(Config) -> router_triple_fault_helper:init_suite(Config).
end_per_suite(Config) -> router_triple_fault_helper:end_suite(Config).
init_per_testcase(_TC, Config) -> router_triple_fault_helper:init_testcase(Config).
end_per_testcase(_TC, Config) -> router_triple_fault_helper:end_testcase(Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_multi_tenant_isolation(_Config) ->
    ct:comment("=== Triple-Fault: Multi-Tenant Isolation ==="),
    
    Tenants = [<<"tenant-a">>, <<"tenant-b">>, <<"tenant-c">>],
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    %% Configure faults
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    %% Send messages for each tenant
    lists:foreach(fun(Tenant) ->
        spawn(fun() ->
            [router_triple_fault_helper:send_test_message(Tenant, <<"req-", Tenant/binary, "-", (integer_to_binary(N))/binary>>, #{}) ||
             N <- lists:seq(1, 5)]
        end)
    end, Tenants),
    
    timer:sleep(3000),
    
    %% Verify no crashes and tenants are isolated
    RouterNatsPid = whereis(router_nats),
    true = is_process_alive(RouterNatsPid),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 100}),
    ok.

test_multi_stream_subject(_Config) ->
    ct:comment("=== Triple-Fault: Multi-Stream/Subject ==="),
    
    Subjects = [<<"stream.a">>, <<"stream.b">>, <<"stream.c">>],
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(connect, {intermittent, {error, connection_refused}, 0.3}),
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.3}),
    
    lists:foreach(fun(Subject) ->
        spawn(fun() ->
            [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", Subject/binary, "-", (integer_to_binary(N))/binary>>, #{
                <<"subject">> => Subject
            }) || N <- lists:seq(1, 5)]
        end)
    end, Subjects),
    
    timer:sleep(3000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 100}),
    ok.

test_consumer_group_isolation(_Config) ->
    ct:comment("=== Triple-Fault: Consumer Group Isolation ==="),
    
    ConsumerGroups = [<<"group-1">>, <<"group-2">>, <<"group-3">>],
    InitialMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    router_nats_fault_injection:enable_fault(nak, {error, timeout}),
    
    lists:foreach(fun(Group) ->
        spawn(fun() ->
            [router_triple_fault_helper:send_test_message(<<"acme">>, <<"req-", Group/binary, "-", (integer_to_binary(N))/binary>>, #{
                <<"consumer_group">> => Group
            }) || N <- lists:seq(1, 5)]
        end)
    end, ConsumerGroups),
    
    timer:sleep(3000),
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    FinalMetrics = router_triple_fault_helper:get_metrics_snapshot(),
    router_triple_fault_helper:verify_contract_invariants(InitialMetrics, FinalMetrics, #{max_redelivery => 100}),
    ok.
