%% @doc NATS Connection Failure - JetStream Tests
%% 
%% Tests for JetStream-specific failure scenarios.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category nats, jetstream, failure, heavy
-module(router_nats_conn_failure_jetstream_SUITE).
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
    test_jetstream_no_responders/1,
    test_jetstream_not_enabled/1,
    test_jetstream_consumer_deleted/1,
    test_jetstream_publish_errors/1,
    test_jetstream_ack_errors/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, jetstream_failure_tests}];
        _ -> []
    end.

groups() ->
    [{jetstream_failure_tests, [sequence], [
        test_jetstream_no_responders,
        test_jetstream_not_enabled,
        test_jetstream_consumer_deleted,
        test_jetstream_publish_errors,
        test_jetstream_ack_errors
    ]}].

init_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Base = router_test_bootstrap:init_per_suite(Config, #{
        start => router_suite,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            tracing_enabled => false
        }
    }),
    Base.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:end_per_suite(Config, #{
        start => router_suite,
        stop => router_suite
    }).

init_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:init_per_testcase(_TestCase, Config, #{}).

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:end_per_testcase(_TestCase, Config, #{}).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_jetstream_no_responders(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:enable_fault(subscribe, {error, no_responders}),
    ok = router_nats_test_helpers:wait_for_status(connected, 3000),
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:disable_fault(subscribe),
    ok.

test_jetstream_not_enabled(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:enable_fault(subscribe, {error, jetstream_not_enabled}),
    ok = router_nats_test_helpers:wait_for_status(connected, 3000),
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:disable_fault(subscribe),
    ok.

test_jetstream_consumer_deleted(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:enable_fault(subscribe, {error, consumer_deleted}),
    ok = router_nats_test_helpers:wait_for_status(connected, 3000),
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:disable_fault(subscribe),
    ok.

test_jetstream_publish_errors(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    %% Test timeout error
    router_nats_fault_injection:enable_fault(publish_with_ack, {error, timeout}),
    catch router_nats:publish_with_ack(<<"test.subject">>, <<"test payload">>),
    ?assert(router_nats_test_helpers:is_service_available()),
    
    %% Test nats_unavailable error
    router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    catch router_nats:publish_with_ack(<<"test.subject">>, <<"test payload">>),
    ?assert(router_nats_test_helpers:is_service_available()),
    
    router_nats_fault_injection:disable_fault(publish_with_ack),
    ok.

test_jetstream_ack_errors(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    %% Test ACK timeout
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    catch router_nats:ack_message(<<"msg-123">>),
    ?assert(router_nats_test_helpers:is_service_available()),
    
    %% Test NAK error
    router_nats_fault_injection:enable_fault(nak, {error, not_connected}),
    catch router_nats:nak_message(<<"msg-456">>),
    ?assert(router_nats_test_helpers:is_service_available()),
    
    router_nats_fault_injection:disable_fault(ack),
    router_nats_fault_injection:disable_fault(nak),
    ok.
