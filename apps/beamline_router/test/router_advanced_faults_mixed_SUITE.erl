%% @doc Advanced Concurrent Faults - Mixed Pattern Tests
%% 
%% Tests for mixed intermittent + persistent fault patterns.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category fault_injection, advanced, heavy
-module(router_advanced_faults_mixed_SUITE).
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
    test_intermittent_connect_persistent_publish/1,
    test_persistent_connect_intermittent_ack/1,
    test_intermittent_publish_persistent_ack/1,
    test_cascading_faults/1
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
    [{group, mixed_pattern_tests}];
groups_for_level(full) ->
    [];
groups_for_level(fast) ->
    [].
groups() ->
    [{mixed_pattern_tests, [sequence], [
        test_intermittent_connect_persistent_publish,
        test_persistent_connect_intermittent_ack,
        test_intermittent_publish_persistent_ack,
        test_cascading_faults
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

test_intermittent_connect_persistent_publish(_Config) ->
    ct:comment("=== Mixed: Intermittent Connect + Persistent Publish ==="),
    
    router_nats_fault_injection:enable_fault(connect, {intermittent, close_connection, 0.5}),
    router_nats_fault_injection:enable_fault(publish, {error, quota_exceeded}),
    
    timer:sleep(3000),
    
    verify_no_deadlocks(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.

test_persistent_connect_intermittent_ack(_Config) ->
    ct:comment("=== Mixed: Persistent Connect + Intermittent ACK ==="),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(ack, {intermittent, {error, timeout}, 0.3}),
    
    timer:sleep(3000),
    
    verify_no_deadlocks(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.

test_intermittent_publish_persistent_ack(_Config) ->
    ct:comment("=== Mixed: Intermittent Publish + Persistent ACK ==="),
    
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.4}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    timer:sleep(3000),
    
    verify_no_deadlocks(),
    
    router_nats_fault_injection:clear_all_faults(),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.

test_cascading_faults(_Config) ->
    ct:comment("=== Cascading Faults ==="),
    
    %% Phase 1: Connect fault
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(1000),
    
    %% Phase 2: Add publish fault
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    timer:sleep(1000),
    
    %% Phase 3: Add ACK fault
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    timer:sleep(1000),
    
    verify_no_deadlocks(),
    
    %% Recovery: Remove faults in reverse order
    router_nats_fault_injection:disable_fault(ack),
    timer:sleep(500),
    router_nats_fault_injection:disable_fault(publish),
    timer:sleep(500),
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(2000),
    
    verify_no_deadlocks(),
    ok.
