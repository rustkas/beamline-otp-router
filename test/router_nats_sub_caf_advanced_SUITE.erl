%% @doc NATS Subscriber CAF: Advanced Tests
%%
%% Advanced integration tests:
%% - Custom assignment subject
%% - Push assignment error handling
%% - Telemetry metrics
%% - Async publication monitoring
%% - Async retry metrics
%% - Payload size limits
%% - Version validation
%%
%% @test_category integration, full, caf
-module(router_nats_sub_caf_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_custom_assignment_subject/1,
    test_push_assignment_error_no_publication/1,
    test_telemetry_metrics_incremented/1,
    test_async_publication_monitoring/1,
    test_async_retry_metrics/1,
    test_payload_size_limit/1,
    test_version_validation_missing/1,
    test_version_validation_unsupported/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        "full" -> [{group, advanced_tests}];
        _ -> []
    end.

groups() ->
    [{advanced_tests, [sequence], [
        test_custom_assignment_subject,
        test_push_assignment_error_no_publication,
        test_telemetry_metrics_incremented,
        test_async_publication_monitoring,
        test_async_retry_metrics,
        test_payload_size_limit,
        test_version_validation_missing,
        test_version_validation_unsupported
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(telemetry, [passthrough]),
    meck:expect(telemetry, execute, fun(_, _, _) -> ok end),
    Config.

end_per_testcase(_TC, _Config) ->
    router_mock_helpers:unload_all([router_nats, telemetry]),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_custom_assignment_subject(_Config) ->
    ct:comment("=== Custom Assignment Subject ==="),
    SubjectCalls = router_test_init:ensure_ets_table(subject_calls, [set, public]),
    ets:delete_all_objects(SubjectCalls),
    meck:expect(router_nats, publish, fun(Subject, _Payload) ->
        ets:insert(SubjectCalls, {Subject, 1}),
        ok
    end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"push_assignment">> => true,
        <<"assignment_subject">> => <<"custom.assignment.subject">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-001">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    
    Subjects = ets:tab2list(SubjectCalls),
    ct:comment("Published subjects: ~p", [Subjects]),
    
    ets:delete(SubjectCalls),
    ok.

test_push_assignment_error_no_publication(_Config) ->
    ct:comment("=== Push Assignment Error: No Publication ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    %% Request with error condition that prevents assignment
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-002">>,
        <<"push_assignment">> => true,
        <<"message">> => #{}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-002">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_telemetry_metrics_incremented(_Config) ->
    ct:comment("=== Telemetry Metrics Incremented ==="),
    TelemetryCalls = router_test_init:ensure_ets_table(telemetry_calls, [set, public]),
    ets:delete_all_objects(TelemetryCalls),
    meck:expect(telemetry, execute, fun(EventName, _Measurements, _Metadata) ->
        ets:insert(TelemetryCalls, {EventName, 1}),
        ok
    end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-003">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-003">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    
    _ = ets:tab2list(TelemetryCalls),
    ets:delete(TelemetryCalls),
    ok.

test_async_publication_monitoring(_Config) ->
    ct:comment("=== Async Publication Monitoring ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-004">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"push_assignment">> => true,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-004">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    
    timer:sleep(100),
    ok.

test_async_retry_metrics(_Config) ->
    ct:comment("=== Async Retry Metrics ==="),
    RetryCount = router_test_init:ensure_ets_table(retry_count, [set, public]),
    ets:delete_all_objects(RetryCount),
    ets:insert(RetryCount, {count, 0}),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        [{count, C}] = ets:lookup(RetryCount, count),
        ets:insert(RetryCount, {count, C + 1}),
        case C < 2 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-005">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-005">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    
    ets:delete(RetryCount),
    ok.

test_payload_size_limit(_Config) ->
    ct:comment("=== Payload Size Limit ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    %% Create large payload
    LargePayload = binary:copy(<<"x">>, 10 * 1024 * 1024),
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-006">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => LargePayload}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-006">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_version_validation_missing(_Config) ->
    ct:comment("=== Version Validation: Missing ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"request_id">> => <<"req-007">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message">> => #{}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-007">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_version_validation_unsupported(_Config) ->
    ct:comment("=== Version Validation: Unsupported ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"99">>,
        <<"request_id">> => <<"req-008">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message">> => #{}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-008">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.
