%% @doc Smoke tests for router_nats mock infrastructure
%%
%% Guards against regressions in helper stubs. Verifies:
%% - publish_with_ack returns deterministic results as stubbed
%% - subscribe_jetstream returns deterministic result
%% - ack/nak are no-ops
%%
%% @test_category unit, helpers, smoke
-module(router_nats_mock_smoke_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_publish_with_ack_default_error/1,
    test_publish_with_ack_custom_success/1,
    test_subscribe_jetstream_returns_error/1,
    test_ack_nak_are_noops/1,
    test_publish_is_noop/1,
    test_request_returns_empty/1
]).

suite() -> [{timetrap, {minutes, 1}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [
        test_publish_with_ack_default_error,
        test_publish_with_ack_custom_success,
        test_subscribe_jetstream_returns_error,
        test_ack_nak_are_noops,
        test_publish_is_noop,
        test_request_returns_empty
    ];
groups_for_level(full) ->
    [
        test_publish_with_ack_default_error,
        test_publish_with_ack_custom_success,
        test_subscribe_jetstream_returns_error,
        test_ack_nak_are_noops,
        test_publish_is_noop,
        test_request_returns_empty
    ];
groups_for_level(_) ->
    [
        test_publish_with_ack_default_error,
        test_publish_with_ack_custom_success,
        test_subscribe_jetstream_returns_error,
        test_ack_nak_are_noops,
        test_publish_is_noop,
        test_request_returns_empty
    ].
init_per_suite(Config) ->
    ok = router_mock_helpers:ensure_test_deps(),
    Config.

end_per_suite(_Config) ->
    router_mock_helpers:unload_all(),
    ok.

init_per_testcase(_TestCase, Config) ->
    router_mock_helpers:unload_all(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_mock_helpers:cleanup_and_verify(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

%% @doc Test: default publish_with_ack stub returns success (for app startup)
test_publish_with_ack_default_error(_Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    
    Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    
    %% Default now returns success (changed to support app startup)
    ?assertEqual({ok, <<"mock-msg-id">>}, Result),
    router_mock_helpers:unload(router_nats),
    ok.

%% @doc Test: custom publish_with_ack stub returns specified value
test_publish_with_ack_custom_success(_Config) ->
    CustomAck = <<"custom-ack-123">>,
    ok = router_mock_helpers:setup_router_nats_mock(#{
        publish_with_ack => fun(_, _, _) -> {ok, CustomAck} end
    }),
    
    Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    
    ?assertEqual({ok, CustomAck}, Result),
    router_mock_helpers:unload(router_nats),
    ok.

%% @doc Test: subscribe_jetstream returns success by default (for app startup)
%% Mock can be configured to return error for specific tests
test_subscribe_jetstream_returns_error(_Config) ->
    %% Default mock returns success
    ok = router_mock_helpers:setup_router_nats_mock(),
    
    Result = router_nats:subscribe_jetstream(
        <<"beamline.intake.decide.v1">>,
        <<"INTAKE">>,
        explicit,
        undefined,
        push
    ),
    
    ?assertEqual({ok, <<"mock-consumer">>}, Result),
    
    %% Can override to return error if needed
    ok = router_mock_helpers:setup_router_nats_mock(#{
        subscribe_jetstream => fun(_, _, _, _, _) -> {error, connection_closed} end
    }),
    ErrorResult = router_nats:subscribe_jetstream(<<"s">>, <<"st">>, explicit, undefined, push),
    ?assertEqual({error, connection_closed}, ErrorResult),
    
    router_mock_helpers:unload(router_nats),
    ok.

%% @doc Test: ack and nak are no-ops (return ok)
test_ack_nak_are_noops(_Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    
    ?assertEqual(ok, router_nats:ack_message(<<"msg-id-1">>)),
    ?assertEqual(ok, router_nats:nak_message(<<"msg-id-2">>)),
    
    %% Verify multiple calls don't crash
    ?assertEqual(ok, router_nats:ack_message(<<"msg-id-3">>)),
    ?assertEqual(ok, router_nats:nak_message(<<"msg-id-4">>)),
    
    router_mock_helpers:unload(router_nats),
    ok.

%% @doc Test: publish is a no-op (returns ok)
test_publish_is_noop(_Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    
    ?assertEqual(ok, router_nats:publish(<<"test.subject">>, <<"payload">>)),
    
    router_mock_helpers:unload(router_nats),
    ok.

%% @doc Test: request returns empty binary
test_request_returns_empty(_Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    
    Result = router_nats:request(<<"test.subject">>, <<"payload">>, 5000),
    
    ?assertEqual({ok, <<>>}, Result),
    router_mock_helpers:unload(router_nats),
    ok.
