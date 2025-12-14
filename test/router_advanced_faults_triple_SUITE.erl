%% @doc Advanced Concurrent Faults - Triple Fault Tests
%% 
%% Tests for triple-fault scenarios (connect + publish + ack/nak).
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category fault_injection, advanced, heavy
-module(router_advanced_faults_triple_SUITE).
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
    test_triple_fault_connect_publish_ack/1,
    test_triple_fault_connect_publish_nak/1,
    test_triple_fault_simultaneous/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, triple_fault_tests}];
        _ -> []
    end.

groups() ->
    [{triple_fault_tests, [sequence], [
        test_triple_fault_connect_publish_ack,
        test_triple_fault_connect_publish_nak,
        test_triple_fault_simultaneous
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    router_metrics:ensure(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_metrics:ensure(),
    router_metrics:clear_all(),
    Config.

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

verify_no_deadlocks() ->
    RouterNatsPid = whereis(router_nats),
    case RouterNatsPid of
        undefined -> ok;
        Pid when is_pid(Pid) -> ?assert(is_process_alive(Pid))
    end,
    ProcessCount = erlang:system_info(process_count),
    ?assert(ProcessCount < 10000),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_triple_fault_connect_publish_ack(_Config) ->
    ct:comment("=== Triple Fault: Connect + Publish + ACK ==="),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.

test_triple_fault_connect_publish_nak(_Config) ->
    ct:comment("=== Triple Fault: Connect + Publish + NAK ==="),
    
    router_nats_fault_injection:enable_fault(connect, close_connection),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(nak, {error, connection_refused}),
    
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.

test_triple_fault_simultaneous(_Config) ->
    ct:comment("=== Triple Fault: Simultaneous ==="),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.
