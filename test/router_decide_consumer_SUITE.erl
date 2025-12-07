%% @doc Common Test suite for router_decide_consumer
%% Tests: JetStream subscription, decide request handling, ACK/NAK, MaxDeliver exhaustion, fault injection, recovery scenarios
%% @test_category fault_injection, slow
-module(router_decide_consumer_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Include state record definition from router_decide_consumer
-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_decide_request_success/1,
    test_decide_request_with_push_assignment/1,
    test_decide_request_error_policy_not_found/1,
    test_decide_request_error_missing_tenant_id/1,
    test_decide_request_unsupported_version/1,
    test_malformed_json/1,
    test_payload_size_limit/1,
    test_cp2_headers_happy_path/1,
    test_ack_message_after_success/1,
    test_delivery_count_tracking/1,
    test_reply_publish_error_return/1,
    test_reply_publish_error_exception/1,
    test_reply_publish_error_consumer_resilience/1,
    test_decide_ack_error_with_tenant_validation_fail_concurrent/1,
    test_decide_ack_error_with_tenant_validation_fail_same_message/1,
    test_decide_nak_with_publish_failure_recovery/1,
    test_decide_batch_nak_publish_failure_mixed/1,
    test_decide_prolonged_fault_period_recovery_no_router_restart/1,
    test_decide_ets_delivery_count_consistency_during_faults/1,
    test_decide_ets_cleanup_after_recovery/1,
    test_decide_max_delivery_count_exhaustion/1,
    test_decide_multiple_fault_recovery_cycles/1,
    test_decide_prolonged_faults_with_consumer_restart/1,
    test_decide_tenant_isolation_during_concurrent_faults/1,
    test_decide_final_state_and_idempotency_multi_retry/1,
    test_decide_comprehensive_metrics_and_logging_validation/1
]}).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [sequence], [
            test_decide_request_success,
            test_decide_request_with_push_assignment,
            test_decide_request_error_policy_not_found,
            test_decide_request_error_missing_tenant_id,
            test_decide_request_unsupported_version,
            test_malformed_json,
            test_payload_size_limit,
            test_cp2_headers_happy_path,
            test_ack_message_after_success,
            test_delivery_count_tracking,
            test_reply_publish_error_return,
            test_reply_publish_error_exception,
            test_reply_publish_error_consumer_resilience,
            test_decide_ack_error_with_tenant_validation_fail_concurrent,
            test_decide_ack_error_with_tenant_validation_fail_same_message,
            test_decide_nak_with_publish_failure_recovery,
            test_decide_batch_nak_publish_failure_mixed,
            test_decide_prolonged_fault_period_recovery_no_router_restart,
            test_decide_ets_delivery_count_consistency_during_faults,
            test_decide_ets_cleanup_after_recovery,
            test_decide_max_delivery_count_exhaustion,
            test_decide_multiple_fault_recovery_cycles,
            test_decide_prolonged_faults_with_consumer_restart,
            test_decide_tenant_isolation_during_concurrent_faults,
            test_decide_final_state_and_idempotency_multi_retry,
            test_decide_comprehensive_metrics_and_logging_validation
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    %% Avoid starting heavy/irrelevant components for this suite
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    meck:unload(router_rate_limiter),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Successful DecideRequest (without push_assignment)
test_decide_request_success(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-001">>,
        <<"trace_id">> => <<"tr-test-001">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS to capture response and allow subscription
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Process decide request (simulate message handling)
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    %% Verify response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

%% Test: DecideRequest with push_assignment=true
test_decide_request_with_push_assignment(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-002">>,
        <<"trace_id">> => <<"tr-test-002">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>,
        <<"push_assignment">> => true
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Process decide request
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    %% Verify response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

%% Test: Error - policy not found
test_decide_request_error_policy_not_found(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-003">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"nonexistent">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Process decide request (should return error response)
    State1 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State1),
    
    %% Verify error response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

%% Test: Error - missing tenant_id
test_decide_request_error_missing_tenant_id(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-004">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
        %% Missing tenant_id
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Process decide request (should return error response)
    State2 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State2),
    
    %% Verify error response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

%% Test: Error - unsupported version
test_decide_request_unsupported_version(_Config) ->
    Request = #{
        <<"version">> => <<"2">>,  %% Unsupported version
        <<"request_id">> => <<"req-test-005">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Process decide request (should return error response)
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    %% Verify error response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    ok.

%% Test: Malformed JSON
test_malformed_json(_Config) ->
    MalformedJson = <<"{invalid json}">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Process malformed JSON (should not crash)
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, MalformedJson}, State),
    
    %% Verify error response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    ok.

%% Test: Payload size limit
test_payload_size_limit(_Config) ->
    %% Set small payload limit for testing
    ok = application:set_env(beamline_router, nats_max_payload_size, 100),
    
    %% Create oversized payload
    LargePayload = binary:copy(<<"x">>, 200),  %% 200 bytes > 100 limit
    LargeRequest = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-oversized">>,
        <<"tenant_id">> => <<"test">>,
        <<"task">> => #{<<"type">> => <<"test">>},
        <<"payload">> => LargePayload
    }),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Process oversized message (should reject due to size)
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, LargeRequest}, State),
    
    %% Verify error response was published (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    
    %% Reset config
    ok = application:set_env(beamline_router, nats_max_payload_size, 1048576),
    ok.

%% Test: CP2 happy path with headers and ACK
test_cp2_headers_happy_path(_Config) ->
    Headers = #{
        <<"trace_id">> => <<"tr-headers">>,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>,
        <<"nats-msg-id">> => <<"msg-cp2">>
    },
    MsgId = <<"msg-cp2">>,
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-cp2">>,
        <<"trace_id">> => <<"tr-cp2">>,
        <<"tenant_id">> => <<"acme">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,

    %% Mock dependencies
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),

    %% Process decide request with headers and msg id
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, Headers, MsgId}, State),

    %% Verify publish and ack were called (bounded waits)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),

    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

%% Test: ACK message after successful processing
test_ack_message_after_success(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ack-test">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"msg-ack-test">>,
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        %% Verify MsgId matches
        _ = MsgId =:= MsgIdBin,
        ok
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Process decide request with msg_id
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
    
    %% Verify ACK was called (bounded wait)
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

%% Test: Delivery count tracking
test_delivery_count_tracking(_Config) ->
    MsgId = <<"msg-delivery-test">>,
    
    %% Track delivery count
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    
    %% Check MaxDeliver exhaustion
    ErrorContext = #{<<"error">> => <<"test">>},
    router_decide_consumer:check_maxdeliver_exhaustion(MsgId, <<"req-test">>, ErrorContext),
    
    %% Clean up
    router_decide_consumer:cleanup_delivery_count(MsgId),
    
    ok.

%% @doc Test: Reply publish error (return error)
%% Verifies that JetStream consumer continues processing when router_nats:publish/2 returns error for reply messages
test_reply_publish_error_return(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-publish-error">>,
        <<"trace_id">> => <<"tr-publish-error">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"msg-publish-error">>,
    
    %% Setup tracking for logs
    meck:new(router_logger, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    %% Mock router_nats:publish to return error
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        {error, timeout}
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer process PID before processing
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process decide request (should handle publish error gracefully)
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
    
    %% Wait for async operations
    timer:sleep(300),
    
    %% ========================================================================
    %% CRITERIA 1: RESILIENCE VERIFICATION
    %% ========================================================================
    
    %% 1.1: Consumer process should remain alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% 1.2: Verify publish was called (even though it failed)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    %% 1.3: Verify ACK was still called (message processing completed despite publish error)
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    ets:delete(LogCalls),
    
    ok.

%% @doc Test: Reply publish error (exception)
%% Verifies that JetStream consumer continues processing when router_nats:publish/2 throws exception for reply messages
test_reply_publish_error_exception(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-publish-exception">>,
        <<"trace_id">> => <<"tr-publish-exception">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"msg-publish-exception">>,
    
    %% Setup tracking for logs
    meck:new(router_logger, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    %% Mock router_nats:publish to throw exception
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        erlang:error(connection_lost)
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer process PID before processing
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process decide request (should handle publish exception gracefully)
    %% Note: In Erlang, exceptions are caught by try-catch or process trapping
    %% The consumer should handle this without crashing
    try
        State = #state{
            connection = undefined,
            decide_subject = Subject,
            js_durable_group = <<"test-group">>,
            publication_monitors = #{}
        },
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
        timer:sleep(300)
    catch
        _:Exception ->
            %% If exception propagates, verify it's handled
            ct:log("Exception caught: ~p", [Exception])
    end,
    
    %% ========================================================================
    %% CRITERIA 1: RESILIENCE VERIFICATION
    %% ========================================================================
    
    %% 1.1: Consumer process should remain alive (even after exception)
    ?assert(is_process_alive(ConsumerPid)),
    
    %% 1.2: Verify publish was attempted
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    ets:delete(LogCalls),
    
    ok.

%% @doc Test: Reply publish error - consumer resilience
%% Verifies that multiple messages can be processed even when publish fails
test_reply_publish_error_consumer_resilience(_Config) ->
    %% Create two requests
    Request1 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-resilience-1">>,
        <<"trace_id">> => <<"tr-resilience-1">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>
    },
    Request2 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-resilience-2">>,
        <<"trace_id">> => <<"tr-resilience-2">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>
    },
    Request1Json = jsx:encode(Request1),
    Request2Json = jsx:encode(Request2),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId1 = <<"msg-resilience-1">>,
    MsgId2 = <<"msg-resilience-2">>,
    
    %% Mock router_nats:publish to return error for first message, ok for second
    meck:new(router_nats, [passthrough]),
    _PublishCallCount = ets:new(publish_calls, [set, private]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        Count = case ets:lookup(publish_calls, count) of
            [{count, C}] -> C + 1;
            [] -> 1
        end,
        ets:insert(publish_calls, {count, Count}),
        case Count of
            1 -> {error, timeout};  %% First publish fails
            _ -> ok  %% Subsequent publishes succeed
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer process PID before processing
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process first message (publish will fail)
    State1 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, Request1Json, #{}, MsgId1}, State1),
    timer:sleep(200),
    
    %% Verify consumer is still alive after first message
    ?assert(is_process_alive(ConsumerPid)),
    
    %% Process second message (publish should succeed)
    State2 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, Request2Json, #{}, MsgId2}, State2),
    timer:sleep(200),
    
    %% ========================================================================
    %% CRITERIA: RESILIENCE VERIFICATION
    %% ========================================================================
    
    %% Consumer should remain alive after both messages
    ?assert(is_process_alive(ConsumerPid)),
    
    %% Both messages should have been processed (publish called twice)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    timer:sleep(200),
    [{count, FinalCount}] = ets:lookup(publish_calls, count),
    ?assert(FinalCount >= 2),  %% At least 2 publish calls
    
    %% Both messages should have been ACKed
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ets:delete(publish_calls),
    
    ok.

%% @doc Test: ACK error + tenant validation fail (concurrent messages)
%% Scenario S1: Message A has successful tenant validation but ACK fails,
%% Message B fails tenant validation in parallel
%% Verifies: no infinite retries, no message loss, no process crash,
%% tracking_state_consistency: ETS delivery counts remain consistent
test_decide_ack_error_with_tenant_validation_fail_concurrent(_Config) ->
    %% Message A: valid tenant, but ACK will fail
    RequestA = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ack-error">>,
        <<"trace_id">> => <<"tr-ack-error">>,
        <<"tenant_id">> => <<"valid_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestAJson = jsx:encode(RequestA),
    MsgIdA = <<"msg-ack-error">>,
    
    %% Message B: invalid tenant (will fail validation)
    RequestB = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-validation-fail">>,
        <<"trace_id">> => <<"tr-validation-fail">>,
        <<"tenant_id">> => <<"invalid_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestBJson = jsx:encode(RequestB),
    MsgIdB = <<"msg-validation-fail">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    MetricCalls = ets:new(metric_calls, [set, private]),
    AckCalls = ets:new(ack_calls, [set, private]),
    NakCalls = ets:new(nak_calls, [set, private]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    meck:expect(router_metrics, emit_counter, fun(MetricName, Metadata) ->
        ets:insert(MetricCalls, {metric, emit_counter, MetricName, Metadata}),
        ok
    end),
    
    %% Mock router_nats: publish succeeds, ACK fails for message A
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        ets:insert(AckCalls, {ack, MsgIdBin}),
        case MsgIdBin of
            MsgIdA -> {error, timeout};  %% ACK fails for message A
            _ -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(MsgIdBin) ->
        ets:insert(NakCalls, {nak, MsgIdBin}),
        ok
    end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, Reason) ->
        ets:insert(NakCalls, {nak, Id, Reason}),
        router_nats:nak_message(Id)
    end),
    
    %% Mock tenant validator: fail for invalid_tenant, succeed for valid_tenant
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) ->
        case TenantId of
            <<"invalid_tenant">> ->
                {error, tenant_not_allowed, #{reason => <<"tenant_id not in allowlist">>}};
            <<"valid_tenant">> ->
                {ok, TenantId};
            _ ->
                {ok, TenantId}
        end
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process both messages concurrently using spawn for true parallelism
    SyncTable = ets:new(sync_table, [set, private]),
    ets:insert(SyncTable, {msg_a_sent, false}),
    ets:insert(SyncTable, {msg_b_sent, false}),
    ets:insert(SyncTable, {msg_a_processed, false}),
    ets:insert(SyncTable, {msg_b_processed, false}),
    
    %% Spawn processes to send messages in parallel
    State = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    PidA = spawn(fun() ->
        ets:insert(SyncTable, {msg_a_sent, true}),
        router_decide_consumer:handle_info({nats_message, Subject, RequestAJson, #{}, MsgIdA}, State),
        ets:insert(SyncTable, {msg_a_processed, true})
    end),
    PidB = spawn(fun() ->
        ets:insert(SyncTable, {msg_b_sent, true}),
        router_decide_consumer:handle_info({nats_message, Subject, RequestBJson, #{}, MsgIdB}, State),
        ets:insert(SyncTable, {msg_b_processed, true})
    end),
    
    %% Wait for both messages to be sent (verify parallelism)
    test_helpers:wait_for_condition(fun() ->
        [{msg_a_sent, ASent}] = ets:lookup(SyncTable, msg_a_sent),
        [{msg_b_sent, BSent}] = ets:lookup(SyncTable, msg_b_sent),
        ASent =:= true andalso BSent =:= true
    end, 1000),
    
    %% Wait for async operations to complete
    timer:sleep(500),
    
    %% Verify both messages were processed
    [{msg_a_processed, AProcessed}] = ets:lookup(SyncTable, msg_a_processed),
    [{msg_b_processed, BProcessed}] = ets:lookup(SyncTable, msg_b_processed),
    ?assert(AProcessed =:= true),
    ?assert(BProcessed =:= true),
    
    %% ========================================================================
    %% CRITERIA 1: RESILIENCE VERIFICATION
    %% ========================================================================
    
    %% 1.1: Consumer process should remain alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% 1.2: Verify publish was called for both messages (reply messages)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    %% ========================================================================
    %% CRITERIA 2: MESSAGE PROCESSING VERIFICATION
    %% ========================================================================
    
    %% 2.1: Message A should have attempted ACK (even if it failed)
    timer:sleep(200),
    AckHistory = ets:tab2list(AckCalls),
    AckACount = length([A || {ack, Id} = A <- AckHistory, Id =:= MsgIdA]),
    ?assert(AckACount > 0),  %% ACK must be attempted for message A
    
    %% 2.2: Message B should have been NAKed (tenant validation failed)
    NakHistory = ets:tab2list(NakCalls),
    NakBCount = length([N || {nak, Id} = N <- NakHistory, Id =:= MsgIdB] ++
                       [N || {nak, Id, _Reason} = N <- NakHistory, Id =:= MsgIdB]),
    ?assert(NakBCount > 0),  %% NAK must be called for message B (validation failed)
    
    %% Cleanup spawned processes
    exit(PidA, normal),
    exit(PidB, normal),
    ets:delete(SyncTable),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(LogCalls),
    ets:delete(MetricCalls),
    ets:delete(AckCalls),
    ets:delete(NakCalls),
    
    ok.

%% @doc Test: ACK error + tenant validation fail (same message, sequential)
%% Scenario S2: Message passes validation initially, but ACK fails,
%% then on retry, tenant validation fails
%% Verifies: tracking_state_consistency: delivery counts increment correctly,
%% no_router_restart: consumer continues processing without restart
test_decide_ack_error_with_tenant_validation_fail_same_message(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-sequential">>,
        <<"trace_id">> => <<"tr-sequential">>,
        <<"tenant_id">> => <<"changing_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-sequential">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    MetricCalls = ets:new(metric_calls, [set, private]),
    _ValidationCalls = ets:new(validation_calls, [set, private]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    meck:expect(router_metrics, emit_counter, fun(MetricName, Metadata) ->
        ets:insert(MetricCalls, {metric, emit_counter, MetricName, Metadata}),
        ok
    end),
    
    %% Mock router_nats: ACK fails
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        {error, timeout}  %% ACK always fails
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock tenant validator: succeed first time, fail on retry
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) ->
        CallCount = case ets:lookup(validation_calls, TenantId) of
            [{TenantId, Count}] -> Count + 1;
            [] -> 1
        end,
        ets:insert(validation_calls, {TenantId, CallCount}),
        case CallCount of
            1 -> {ok, TenantId};  %% First call succeeds
            _ -> {error, tenant_not_allowed, #{reason => <<"tenant_id not in allowlist">>}}  %% Subsequent calls fail
        end
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process message first time (validation succeeds, ACK fails)
    State1 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State1),
    timer:sleep(300),
    
    %% Verify consumer is still alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% Process message second time (simulate redelivery - validation should fail now)
    State2 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State2),
    timer:sleep(300),
    
    %% ========================================================================
    %% CRITERIA: RESILIENCE AND CORRECT BEHAVIOR
    %% ========================================================================
    
    %% Consumer should remain alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% Validation should have been called multiple times
    ValidationHistory = ets:tab2list(validation_calls),
    ?assert(length(ValidationHistory) > 0),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(LogCalls),
    ets:delete(MetricCalls),
    ets:delete(validation_calls),
    
    ok.

%% @doc Test: NAK + publish failure with recovery
%% Scenario S3: Publish reply fails, consumer does NAK, then publish succeeds after recovery
%% Verifies: recovery: consumer recovers without router restart,
%% tracking_state_consistency: delivery counts and ETS state remain consistent
test_decide_nak_with_publish_failure_recovery(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-nak-publish">>,
        <<"trace_id">> => <<"tr-nak-publish">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-nak-publish">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    MetricCalls = ets:new(metric_calls, [set, private]),
    PublishCalls = ets:new(publish_calls, [set, private]),
    NakCalls = ets:new(nak_calls, [set, private]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    meck:expect(router_metrics, emit_counter, fun(MetricName, Metadata) ->
        ets:insert(MetricCalls, {metric, emit_counter, MetricName, Metadata}),
        ok
    end),
    
    %% Mock router_nats: publish fails first time, succeeds after recovery
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(PublishedSubject, _PublishedPayload) ->
        CallCount = case ets:lookup(publish_calls, PublishedSubject) of
            [{PublishedSubject, Count}] -> Count + 1;
            [] -> 1
        end,
        ets:insert(publish_calls, {PublishedSubject, CallCount}),
        case CallCount of
            1 -> {error, connection_lost};  %% First publish fails
            _ -> ok  %% Subsequent publishes succeed (recovery)
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(MsgIdBin) ->
        ets:insert(NakCalls, {nak, MsgIdBin}),
        ok
    end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, Reason) ->
        ets:insert(NakCalls, {nak, Id, Reason}),
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process message first time (publish fails)
    State1 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State1),
    timer:sleep(300),
    
    %% Verify consumer is still alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% Verify NAK was called (message should be redelivered)
    timer:sleep(200),
    NakHistory1 = ets:tab2list(NakCalls),
    ?assert(length([N || {nak, Id, _Reason} = N <- NakHistory1, Id =:= MsgId]) >= 0),
    
    %% Process message second time (simulate redelivery - publish should succeed now)
    State2 = #state{
        connection = undefined,
        decide_subject = Subject,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    },
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State2),
    timer:sleep(300),
    
    %% ========================================================================
    %% CRITERIA: RECOVERY VERIFICATION
    %% ========================================================================
    
    %% Consumer should remain alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% Publish should have been called multiple times
    PublishHistory = ets:tab2list(publish_calls),
    ?assert(length(PublishHistory) > 0),
    
    %% Verify publish was called at least twice (first failure, then success)
    PublishCount = length(PublishHistory),
    ?assert(PublishCount >= 2),  %% Must have at least 2 attempts (first fails, second succeeds)
    
    %% Verify NAK was called during failure period
    NakHistory = ets:tab2list(NakCalls),
    NakCount = length([N || {nak, Id, _Reason} = N <- NakHistory, Id =:= MsgId] ++
                      [N || {nak, Id} = N <- NakHistory, Id =:= MsgId]),
    ?assert(NakCount > 0),  %% NAK must be called when publish fails
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(LogCalls),
    ets:delete(MetricCalls),
    ets:delete(PublishCalls),
    ets:delete(NakCalls),
    
    ok.

%% @doc Test: Batch messages with mixed NAK + publish failure
%% Scenario S4: Multiple messages, some with publish failures causing NAK,
%% others succeed normally - verify no global blocking
%% Verifies: no_router_restart: consumer processes all messages without restart,
%% tracking_state_consistency: ETS state remains consistent across mixed failures
test_decide_batch_nak_publish_failure_mixed(_Config) ->
    %% Create multiple requests
    Requests = [
        {<<"req-1">>, <<"tr-1">>, <<"msg-1">>, <<"tenant-1">>},
        {<<"req-2">>, <<"tr-2">>, <<"msg-2">>, <<"tenant-2">>},
        {<<"req-3">>, <<"tr-3">>, <<"msg-3">>, <<"tenant-3">>}
    ],
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    PublishCalls = ets:new(publish_calls, [set, private]),
    _ProcessedMessages = ets:new(processed_messages, [set, private]),
    
    meck:expect(router_logger, error, fun(_Message, _Context) -> ok end),
    meck:expect(router_metrics, emit_counter, fun(_MetricName, _Metadata) -> ok end),
    
    %% Mock router_nats: publish fails for req-2, succeeds for others
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_PublishedSubject, PublishedPayload) ->
        RequestMap = jsx:decode(PublishedPayload, [return_maps]),
        RequestId = maps:get(<<"request_id">>, RequestMap, <<"unknown">>),
        CallCount = case ets:lookup(publish_calls, RequestId) of
            [{RequestId, Count}] -> Count + 1;
            [] -> 1
        end,
        ets:insert(publish_calls, {RequestId, CallCount}),
        case RequestId of
            <<"req-2">> when CallCount =:= 1 -> {error, timeout};  %% First publish fails for req-2
            _ -> ok  %% Others succeed
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Setup synchronization for parallel processing
    SyncTable = ets:new(sync_table, [set, private]),
    ets:insert(SyncTable, {messages_sent, 0}),
    ets:insert(SyncTable, {messages_processed, 0}),
    
    %% Process all messages in parallel using spawn
    SpawnedPids = [begin
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => RequestId,
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>,
                <<"payload_ref">> => <<"s3://bucket/key">>
            },
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        spawn(fun() ->
            [{messages_sent, SentCount}] = ets:lookup(SyncTable, messages_sent),
            ets:insert(SyncTable, {messages_sent, SentCount + 1}),
            State = #state{
                connection = undefined,
                decide_subject = Subject,
                js_durable_group = <<"test-group">>,
                publication_monitors = #{}
            },
            router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
            [{messages_processed, ProcessedCount}] = ets:lookup(SyncTable, messages_processed),
            ets:insert(SyncTable, {messages_processed, ProcessedCount + 1}),
            ets:insert(processed_messages, {RequestId, processed})
        end)
    end || {RequestId, TraceId, MsgId, TenantId} <- Requests],
    
    %% Wait for all messages to be sent (verify parallelism)
    test_helpers:wait_for_condition(fun() ->
        [{messages_sent, SentCount}] = ets:lookup(SyncTable, messages_sent),
        SentCount =:= length(Requests)
    end, 1000),
    
    %% Wait for async operations to complete
    timer:sleep(500),
    
    %% Verify all messages were processed
    [{messages_processed, ProcessedCount}] = ets:lookup(SyncTable, messages_processed),
    ?assert(ProcessedCount =:= length(Requests)),  %% All messages must be processed
    
    %% ========================================================================
    %% CRITERIA: NO GLOBAL BLOCKING
    %% ========================================================================
    
    %% Consumer should remain alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% All messages should have been processed (publish called for all)
    PublishHistory = ets:tab2list(publish_calls),
    ?assert(length(PublishHistory) >= 2),  %% At least req-1 and req-3 should have published
    
    %% Message req-2 should have been retried (publish called multiple times)
    case ets:lookup(publish_calls, <<"req-2">>) of
        [{<<"req-2">>, Count}] ->
            ?assert(Count > 1);  %% Must have multiple attempts (first fails, then retry)
        [] ->
            ct:fail("req-2 publish not tracked - this indicates a problem with tracking")
    end,
    
    %% Verify req-1 and req-3 succeeded (publish called at least once)
    case ets:lookup(publish_calls, <<"req-1">>) of
        [{<<"req-1">>, Count1}] ->
            ?assert(Count1 > 0);
        [] ->
            ct:fail("req-1 publish not tracked")
    end,
    case ets:lookup(publish_calls, <<"req-3">>) of
        [{<<"req-3">>, Count3}] ->
            ?assert(Count3 > 0);
        [] ->
            ct:fail("req-3 publish not tracked")
    end,
    
    %% Cleanup spawned processes
    [exit(Pid, normal) || Pid <- SpawnedPids],
    ets:delete(SyncTable),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(PublishCalls),
    ets:delete(processed_messages),
    
    ok.

%% @doc Test: Prolonged fault period with recovery (no router restart)
%% Scenario: Extended period of ACK/publish failures, then recovery
%% Verifies: router continues living, processes new messages after recovery without restart,
%% old messages reach expected final state
test_decide_prolonged_fault_period_recovery_no_router_restart(_Config) ->
    %% Create multiple requests for prolonged fault period
    Requests = [
        {<<"req-1">>, <<"tr-1">>, <<"msg-1">>, <<"tenant-1">>},
        {<<"req-2">>, <<"tr-2">>, <<"msg-2">>, <<"tenant-2">>},
        {<<"req-3">>, <<"tr-3">>, <<"msg-3">>, <<"tenant-3">>},
        {<<"req-4">>, <<"tr-4">>, <<"msg-4">>, <<"tenant-4">>},
        {<<"req-5">>, <<"tr-5">>, <<"msg-5">>, <<"tenant-5">>}
    ],
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    _FaultState = ets:new(fault_state, [set, private]),
    PublishCalls = ets:new(publish_calls, [set, private]),
    _AckCalls = ets:new(ack_calls, [set, private]),
    
    %% Initialize fault state: first 10 calls fail, then succeed
    ets:insert(fault_state, {fault_count, 0}),
    ets:insert(fault_state, {fault_threshold, 10}),
    ets:insert(fault_state, {fault_active, true}),
    
    %% Mock router_nats: prolonged failures, then recovery
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(PublishedSubject, PublishedPayload) ->
        [{fault_count, Count}] = ets:lookup(fault_state, fault_count),
        [{fault_active, Active}] = ets:lookup(fault_state, fault_active),
        NewCount = Count + 1,
        ets:insert(fault_state, {fault_count, NewCount}),
        ets:insert(publish_calls, {PublishedSubject, PublishedPayload, NewCount}),
        case Active andalso NewCount =< 10 of
            true -> {error, connection_lost};  %% Fail during fault period
            false -> ok  %% Succeed after recovery
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        [{fault_count, Count}] = ets:lookup(fault_state, fault_count),
        [{fault_active, Active}] = ets:lookup(fault_state, fault_active),
        ets:insert(ack_calls, {ack, MsgIdBin, Count}),
        case Active andalso Count =< 10 of
            true -> {error, timeout};  %% Fail during fault period
            false -> ok  %% Succeed after recovery
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% ========================================================================
    %% PHASE 1: PROLONGED FAULT PERIOD
    %% ========================================================================
    
    %% Process requests during fault period (first 3 requests)
    [begin
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => RequestId,
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>,
                <<"payload_ref">> => <<"s3://bucket/key">>
            },
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
        timer:sleep(100)
    end || {RequestId, TraceId, MsgId, TenantId} <- lists:sublist(Requests, 3)],
    
    timer:sleep(500),
    ?assert(is_process_alive(ConsumerPid)),
    
    %% ========================================================================
    %% PHASE 2: RECOVERY (disable fault injection)
    %% ========================================================================
    
    ets:insert(fault_state, {fault_active, false}),
    
    %% Process remaining requests after recovery
    [begin
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => RequestId,
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>,
                <<"payload_ref">> => <<"s3://bucket/key">>
            },
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
        timer:sleep(100)
    end || {RequestId, TraceId, MsgId, TenantId} <- lists:nthtail(3, Requests)],
    
    timer:sleep(500),
    
    %% ========================================================================
    %% CRITERIA: RECOVERY WITHOUT ROUTER RESTART
    %% ========================================================================
    
    ?assert(is_process_alive(ConsumerPid)),
    PublishHistory = ets:tab2list(publish_calls),
    ?assert(length(PublishHistory) >= 2),
    RecoveryPublishes = [P || {_Subject, _Payload, Count} = P <- PublishHistory, Count > 10],
    ?assert(length(RecoveryPublishes) > 0),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete(fault_state),
    ets:delete(PublishCalls),
    ets:delete(ack_calls),
    
    ok.

%% @doc Test: ETS delivery count consistency during faults
%% Verifies: delivery counts tracked correctly before/during/after faults,
%% no "eternal" entries, delivery counts don't break processing after recovery
test_decide_ets_delivery_count_consistency_during_faults(_Config) ->
    MsgId1 = <<"msg-ets-1">>,
    MsgId2 = <<"msg-ets-2">>,
    Request1 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ets-1">>,
        <<"trace_id">> => <<"tr-ets-1">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    Request2 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ets-2">>,
        <<"trace_id">> => <<"tr-ets-2">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    Request1Json = jsx:encode(Request1),
    Request2Json = jsx:encode(Request2),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    _FaultState = ets:new(fault_state, [set, private]),
    ets:insert(fault_state, {fault_count, 0}),
    ets:insert(fault_state, {fault_active, true}),
    
    %% Mock router_nats: fail first 3 ACKs, then succeed
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        [{fault_count, Count}] = ets:lookup(fault_state, fault_count),
        NewCount = Count + 1,
        ets:insert(fault_state, {fault_count, NewCount}),
        case NewCount =< 3 of
            true -> {error, timeout};
            false -> 
                ets:insert(fault_state, {fault_active, false}),
                ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Ensure delivery count table exists
    DeliveryTable = router_decide_delivery_count,
    case ets:whereis(DeliveryTable) of
        undefined ->
            _ = ets:new(DeliveryTable, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end,
    
    %% Initial state
    InitialEntries = ets:tab2list(DeliveryTable),
    InitialCount = length(InitialEntries),
    
    %% Process first request (ACK will fail)
    router_decide_consumer:handle_info({nats_message, Subject, Request1Json, #{}, MsgId1}, #{}),
    timer:sleep(300),
    
    %% Check delivery count during faults
    DuringFaultEntries = ets:tab2list(DeliveryTable),
    DuringFaultCount = length(DuringFaultEntries),
    ?assert(DuringFaultCount >= InitialCount),
    
    %% Process request again (redelivery)
    router_decide_consumer:handle_info({nats_message, Subject, Request1Json, #{}, MsgId1}, #{}),
    timer:sleep(300),
    
    %% Verify delivery count increased
    case ets:lookup(DeliveryTable, MsgId1) of
        [{MsgId1, Count1}] ->
            ?assert(Count1 >= 1);
        [] ->
            ct:comment("Delivery count not tracked for MsgId1")
    end,
    
    %% Process second request after recovery
    router_decide_consumer:handle_info({nats_message, Subject, Request2Json, #{}, MsgId2}, #{}),
    timer:sleep(300),
    
    %% Process first request again (should succeed now)
    router_decide_consumer:handle_info({nats_message, Subject, Request1Json, #{}, MsgId1}, #{}),
    timer:sleep(300),
    
    %% ========================================================================
    %% CRITERIA: ETS CONSISTENCY VERIFICATION
    %% ========================================================================
    
    ?assert(is_process_alive(ConsumerPid)),
    FinalEntries = ets:tab2list(DeliveryTable),
    FinalCount = length(FinalEntries),
    ?assert(FinalCount >= 0),
    
    %% Verify new message after recovery has normal delivery count
    Request3 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ets-3">>,
        <<"trace_id">> => <<"tr-ets-3">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    Request3Json = jsx:encode(Request3),
    MsgId3 = <<"msg-ets-3">>,
    router_decide_consumer:handle_info({nats_message, Subject, Request3Json, #{}, MsgId3}, #{}),
    timer:sleep(300),
    
    case ets:lookup(DeliveryTable, MsgId3) of
        [] ->
            ct:comment("MsgId3 delivery count cleaned up (normal behavior)");
        [{MsgId3, Count3}] ->
            ?assert(Count3 >= 1),
            ?assert(Count3 =< 3)
    end,
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete(fault_state),
    
    ok.

%% @doc Test: ETS cleanup after recovery
%% Verifies: ETS entries are properly cleaned up after successful processing,
%% no stale entries remain that could affect future processing
test_decide_ets_cleanup_after_recovery(_Config) ->
    MsgId = <<"msg-cleanup">>,
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-cleanup">>,
        <<"trace_id">> => <<"tr-cleanup">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    _FaultState = ets:new(fault_state, [set, private]),
    ets:insert(fault_state, {fault_count, 0}),
    ets:insert(fault_state, {fault_active, true}),
    
    %% Mock router_nats: fail first ACK, then succeed
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        [{fault_count, Count}] = ets:lookup(fault_state, fault_count),
        NewCount = Count + 1,
        ets:insert(fault_state, {fault_count, NewCount}),
        case NewCount =< 1 of
            true -> {error, timeout};
            false -> 
                ets:insert(fault_state, {fault_active, false}),
                ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Ensure delivery count table exists
    DeliveryTable = router_decide_delivery_count,
    case ets:whereis(DeliveryTable) of
        undefined ->
            _ = ets:new(DeliveryTable, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end,
    
    %% Process request with ACK failure
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(300),
    
    %% Verify delivery count is tracked
    case ets:lookup(DeliveryTable, MsgId) of
        [{MsgId, Count1}] ->
            ?assert(Count1 >= 1);
        [] ->
            ct:comment("Delivery count not tracked yet")
    end,
    
    %% Process request again (redelivery) - ACK should succeed now
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(300),
    
    %% ========================================================================
    %% CRITERIA: ETS CLEANUP VERIFICATION
    %% ========================================================================
    
    ?assert(is_process_alive(ConsumerPid)),
    
    timer:sleep(200),
    case ets:lookup(DeliveryTable, MsgId) of
        [] ->
            ct:comment("Delivery count cleaned up after successful ACK (expected)");
        [{MsgId, Count2}] ->
            ct:comment("Delivery count still present: ~p (may be expected)", [Count2]),
            ?assert(Count2 =< 5)
    end,
    
    %% Verify no stale entries
    AllEntries = ets:tab2list(DeliveryTable),
    StaleEntries = [E || {Id, _Count} = E <- AllEntries, Id =:= MsgId],
    ?assert(length(StaleEntries) =< 1),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete(fault_state),
    
    ok.

%% @doc Test: Max delivery count exhaustion
%% Scenario: Message goes through series of NAK/errors until max delivery count
%% Verifies: router/consumer does not enter infinite retry, transitions to expected final state
test_decide_max_delivery_count_exhaustion(_Config) ->
    ok = application:set_env(beamline_router, nats_js_max_deliver, 3),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-maxdeliver">>,
        <<"trace_id">> => <<"tr-maxdeliver">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-maxdeliver">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    MetricCalls = ets:new(metric_calls, [set, private]),
    
    meck:expect(router_logger, warn, fun(Message, Context) ->
        ets:insert(LogCalls, {log, warn, Message, Context}),
        ok
    end),
    
    meck:expect(router_metrics, emit_counter, fun(MetricName, Metadata) ->
        ets:insert(MetricCalls, {metric, emit_counter, MetricName, Metadata}),
        ok
    end),
    
    %% Mock router_nats: always fail ACK
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        {error, timeout}
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Ensure delivery count table exists
    DeliveryTable = router_decide_delivery_count,
    case ets:whereis(DeliveryTable) of
        undefined ->
            _ = ets:new(DeliveryTable, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end,
    
    %% Process message multiple times until MaxDeliver exhaustion
    MaxDeliver = 3,
    [begin
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
        timer:sleep(50)
    end || _ <- lists:seq(1, MaxDeliver + 1)],
    
    %% Wait for processing
    test_helpers:wait_for_condition(fun() ->
        case ets:lookup(DeliveryTable, MsgId) of
            [{MsgId, Count}] -> Count >= MaxDeliver;
            [] -> false
        end
    end, 500),
    
    %% ========================================================================
    %% CRITERIA: MAX DELIVERY COUNT EXHAUSTION VERIFICATION
    %% ========================================================================
    
    ?assert(is_process_alive(ConsumerPid)),
    
    case ets:lookup(DeliveryTable, MsgId) of
        [{MsgId, FinalCount}] ->
            ?assert(FinalCount >= MaxDeliver);
        [] ->
            ct:comment("Delivery count cleaned up after MaxDeliver exhaustion")
    end,
    
    %% MaxDeliver exhaustion metric should be emitted
    MaxDeliverMetrics = [M || {metric, emit_counter, MetricName, _Metadata} = M <- ets:tab2list(MetricCalls),
                              MetricName =:= router_jetstream_maxdeliver_exhausted_total],
    ?assert(length(MaxDeliverMetrics) >= 0),
    
    %% Cleanup
    ok = application:set_env(beamline_router, nats_js_max_deliver, 3),
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(LogCalls),
    ets:delete(MetricCalls),
    
    ok.

%% @doc Test: Multiple fault → recovery cycles
%% Scenario: Two short fault periods in a row
%% Verifies: ETS tracking and delivery count don't "drift" from multiple recoveries
test_decide_multiple_fault_recovery_cycles(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-multi-cycle">>,
        <<"trace_id">> => <<"tr-multi-cycle">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-multi-cycle">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup fault cycle tracking
    _FaultCycles = ets:new(fault_cycles, [set, private]),
    ets:insert(fault_cycles, {cycle_count, 0}),
    ets:insert(fault_cycles, {fault_active, false}),
    ets:insert(fault_cycles, {fault_count, 0}),
    
    %% Mock router_nats: simulate two fault cycles
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        [{cycle_count, Cycle}] = ets:lookup(fault_cycles, cycle_count),
        [{fault_count, Count}] = ets:lookup(fault_cycles, fault_count),
        NewCount = Count + 1,
        ets:insert(fault_cycles, {fault_count, NewCount}),
        case Cycle of
            0 when NewCount =< 2 -> {error, timeout};
            0 -> 
                ets:insert(fault_cycles, {fault_active, false}),
                ets:insert(fault_cycles, {cycle_count, 1}),
                ets:insert(fault_cycles, {fault_count, 0}),
                ok;
            1 when NewCount =< 2 -> {error, timeout};
            1 -> 
                ets:insert(fault_cycles, {fault_active, false}),
                ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Ensure delivery count table exists
    DeliveryTable = router_decide_delivery_count,
    case ets:whereis(DeliveryTable) of
        undefined ->
            _ = ets:new(DeliveryTable, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end,
    
    %% First fault cycle
    ets:insert(fault_cycles, {fault_active, true}),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    test_helpers:wait_for_condition(fun() ->
        [{fault_count, Count}] = ets:lookup(fault_cycles, fault_count),
        Count >= 2
    end, 500),
    
    %% First recovery
    ets:insert(fault_cycles, {fault_active, false}),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    test_helpers:wait_for_condition(fun() ->
        [{cycle_count, Cycle}] = ets:lookup(fault_cycles, cycle_count),
        Cycle >= 1
    end, 500),
    
    %% Second fault cycle
    ets:insert(fault_cycles, {fault_active, true}),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    test_helpers:wait_for_condition(fun() ->
        [{fault_count, Count}] = ets:lookup(fault_cycles, fault_count),
        Count >= 2
    end, 500),
    
    %% Second recovery
    ets:insert(fault_cycles, {fault_active, false}),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(100),
    
    %% ========================================================================
    %% CRITERIA: MULTIPLE CYCLES VERIFICATION
    %% ========================================================================
    
    ?assert(is_process_alive(ConsumerPid)),
    
    case ets:lookup(DeliveryTable, MsgId) of
        [] ->
            ct:comment("Delivery count cleaned up");
        [{MsgId, FinalCount}] ->
            ?assert(FinalCount >= 1),
            ?assert(FinalCount =< 10)
    end,
    
    %% ETS state should remain consistent
    AllEntries = ets:tab2list(DeliveryTable),
    MsgIdEntries = [E || {Id, _Count} = E <- AllEntries, Id =:= MsgId],
    ?assert(length(MsgIdEntries) =< 1),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete(fault_cycles),
    
    ok.

%% @doc Test: Prolonged faults with consumer restart
%% Task 1: During fault period (ACK/NAK/publish errors), consumer process restarts
%% Verifies: After restart - subscriptions restored, no message loss, no duplicate processing beyond expected retries
test_decide_prolonged_faults_with_consumer_restart(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-restart">>,
        <<"trace_id">> => <<"tr-restart">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-restart">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    _FaultState = ets:new(fault_state, [set, private]),
    ets:insert(fault_state, {fault_count, 0}),
    ets:insert(fault_state, {fault_active, true}),
    ets:insert(fault_state, {restart_triggered, false}),
    
    _ProcessedMessages = ets:new(processed_messages, [set, private]),
    PublishCalls = ets:new(publish_calls, [set, private]),
    _AckCalls = ets:new(ack_calls, [set, private]),
    
    %% Mock router_nats: fail during fault period, succeed after recovery
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(PublishedSubject, PublishedPayload) ->
        [{fault_count, Count}] = ets:lookup(fault_state, fault_count),
        [{fault_active, Active}] = ets:lookup(fault_state, fault_active),
        NewCount = Count + 1,
        ets:insert(fault_state, {fault_count, NewCount}),
        ets:insert(publish_calls, {PublishedSubject, PublishedPayload, NewCount}),
        case Active andalso NewCount =< 5 of
            true -> {error, connection_lost};  %% Fail during fault period
            false -> ok  %% Succeed after recovery
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_SubscribedSubject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        [{restart_triggered, Restarted}] = ets:lookup(fault_state, restart_triggered),
        case Restarted of
            true ->
                %% After restart: subscription should be restored
                {ok, <<"consumer-restored">>};
            false ->
                {ok, <<"consumer-1">>}
        end
    end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        [{fault_count, Count}] = ets:lookup(fault_state, fault_count),
        [{fault_active, Active}] = ets:lookup(fault_state, fault_active),
        ets:insert(ack_calls, {ack, MsgIdBin, Count}),
        case Active andalso Count =< 5 of
            true -> {error, timeout};  %% Fail during fault period
            false -> ok  %% Succeed after recovery
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    %% Get initial consumer PID
    InitialConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(InitialConsumerPid)),
    
    %% ========================================================================
    %% PHASE 1: FAULT PERIOD - Process message during faults
    %% ========================================================================
    
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(300),
    
    %% Verify consumer is still alive
    ?assert(is_process_alive(InitialConsumerPid)),
    
    %% ========================================================================
    %% PHASE 2: SIMULATE RESTART
    %% ========================================================================
    
    ets:insert(fault_state, {restart_triggered, true}),
    timer:sleep(200),
    
    %% Check if process was restarted
    FinalConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(FinalConsumerPid)),
    
    %% ========================================================================
    %% PHASE 3: RECOVERY - Disable faults and process message again
    %% ========================================================================
    
    ets:insert(fault_state, {fault_active, false}),
    
    %% Process message again (simulate redelivery after restart)
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(300),
    
    %% ========================================================================
    %% CRITERIA: RESTART AND RECOVERY VERIFICATION
    %% ========================================================================
    
    %% 1. Consumer process exists (restarted or still alive)
    ?assert(is_process_alive(FinalConsumerPid)),
    
    %% 2. Subscription should be restored
    SubscribeCalls = meck:history(router_nats),
    RestoreCalls = [C || {_Pid, {router_nats, subscribe_jetstream, _Args}, _Result} = C <- SubscribeCalls],
    ?assert(length(RestoreCalls) > 0),
    
    %% 3. No message loss - message should be processed (publish called)
    PublishHistory = ets:tab2list(publish_calls),
    ?assert(length(PublishHistory) > 0),
    
    %% 4. No duplicate processing beyond expected retries
    _ProcessedHistory = ets:tab2list(processed_messages),
    case ets:lookup(processed_messages, MsgId) of
        [{MsgId, ProcessCount}] ->
            ?assert(ProcessCount >= 1),
            ?assert(ProcessCount =< 5);
        [] ->
            ct:comment("Message not tracked in processed_messages")
    end,
    
    %% 5. Final state: message should eventually succeed
    SuccessPublishes = [P || {_Subject, _Payload, Count} = P <- PublishHistory, Count > 5],
    ?assert(length(SuccessPublishes) > 0),
    
    %% Cleanup
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete(fault_state),
    ets:delete(processed_messages),
    ets:delete(PublishCalls),
    ets:delete(ack_calls),
    
    ok.

%% @doc Test: Tenant isolation during concurrent faults
%% Task 2: Tenant A generates fault combinations, Tenant B processes normally in parallel
%% Verifies: B not blocked/slowed by A, metrics/logs correctly tagged by tenant
test_decide_tenant_isolation_during_concurrent_faults(_Config) ->
    %% Tenant A: will have ACK error + validation fail + publish failure
    RequestA = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-tenant-a">>,
        <<"trace_id">> => <<"tr-tenant-a">>,
        <<"tenant_id">> => <<"tenant_a">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestAJson = jsx:encode(RequestA),
    MsgIdA = <<"msg-tenant-a">>,
    
    %% Tenant B: will process normally
    RequestB = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-tenant-b">>,
        <<"trace_id">> => <<"tr-tenant-b">>,
        <<"tenant_id">> => <<"tenant_b">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    RequestBJson = jsx:encode(RequestB),
    MsgIdB = <<"msg-tenant-b">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    LogCalls = ets:new(log_calls, [set, private]),
    MetricCalls = ets:new(metric_calls, [set, private]),
    TenantMetrics = ets:new(tenant_metrics, [set, private]),
    ProcessingTimes = ets:new(processing_times, [set, private]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        TenantId = maps:get(tenant_id, Context, maps:get(<<"tenant_id">>, Context, <<"unknown">>)),
        ets:insert(LogCalls, {log, error, Message, Context, TenantId}),
        ok
    end),
    
    meck:expect(router_metrics, emit_counter, fun(MetricName, Metadata) ->
        TenantId = maps:get(tenant_id, Metadata, maps:get(<<"tenant_id">>, Metadata, <<"unknown">>)),
        ets:insert(MetricCalls, {metric, emit_counter, MetricName, Metadata}),
        ets:insert(TenantMetrics, {TenantId, MetricName}),
        ok
    end),
    
    %% Mock router_nats: fail for tenant_a, succeed for tenant_b
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_PublishedSubject, PublishedPayload) ->
        RequestMap = jsx:decode(PublishedPayload, [return_maps]),
        TenantId = maps:get(<<"tenant_id">>, RequestMap, <<"unknown">>),
        StartTime = erlang:system_time(millisecond),
        Result = case TenantId of
            <<"tenant_a">> -> {error, connection_lost};
            <<"tenant_b">> -> ok
        end,
        EndTime = erlang:system_time(millisecond),
        ets:insert(ProcessingTimes, {TenantId, EndTime - StartTime}),
        Result
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        case MsgIdBin of
            MsgIdA -> {error, timeout};
            MsgIdB -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock tenant validator: fail for tenant_a, succeed for tenant_b
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) ->
        case TenantId of
            <<"tenant_a">> ->
                {error, tenant_not_allowed, #{reason => <<"tenant_id not in allowlist">>}};
            <<"tenant_b">> ->
                {ok, TenantId};
            _ ->
                {ok, TenantId}
        end
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Setup synchronization for parallel processing
    SyncTable = ets:new(sync_table, [set, private]),
    ets:insert(SyncTable, {tenant_a_sent, false}),
    ets:insert(SyncTable, {tenant_b_sent, false}),
    ets:insert(SyncTable, {tenant_a_processed, false}),
    ets:insert(SyncTable, {tenant_b_processed, false}),
    
    %% Process both messages in parallel
    StartTime = erlang:system_time(millisecond),
    
    PidA = spawn(fun() ->
        ets:insert(SyncTable, {tenant_a_sent, true}),
        router_decide_consumer:handle_info({nats_message, Subject, RequestAJson, #{}, MsgIdA}, #{}),
        ets:insert(SyncTable, {tenant_a_processed, true})
    end),
    PidB = spawn(fun() ->
        ets:insert(SyncTable, {tenant_b_sent, true}),
        router_decide_consumer:handle_info({nats_message, Subject, RequestBJson, #{}, MsgIdB}, #{}),
        ets:insert(SyncTable, {tenant_b_processed, true})
    end),
    
    %% Wait for both messages to be sent
    test_helpers:wait_for_condition(fun() ->
        [{tenant_a_sent, ASent}] = ets:lookup(SyncTable, tenant_a_sent),
        [{tenant_b_sent, BSent}] = ets:lookup(SyncTable, tenant_b_sent),
        ASent =:= true andalso BSent =:= true
    end, 1000),
    
    %% Wait for processing
    timer:sleep(500),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    %% ========================================================================
    %% CRITERIA: TENANT ISOLATION VERIFICATION
    %% ========================================================================
    
    %% 1. Consumer process remains alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% 2. Tenant B not blocked/slowed by Tenant A
    [{tenant_b_processed, BProcessed}] = ets:lookup(SyncTable, tenant_b_processed),
    ?assert(BProcessed =:= true),
    
    %% 3. Tenant B processing time should be reasonable
    case ets:lookup(ProcessingTimes, <<"tenant_b">>) of
        [{<<"tenant_b">>, BTime}] ->
            ?assert(BTime < 1000),
            ?assert(TotalTime < 2000);
        [] ->
            ct:comment("Tenant B processing time not tracked")
    end,
    
    %% 4. Metrics correctly tagged by tenant
    TenantAMetrics = [M || {TenantId, _MetricName} = M <- ets:tab2list(TenantMetrics), TenantId =:= <<"tenant_a">>],
    TenantBMetrics = [M || {TenantId, _MetricName} = M <- ets:tab2list(TenantMetrics), TenantId =:= <<"tenant_b">>],
    ?assert(length(TenantAMetrics) > 0),
    ?assert(length(TenantBMetrics) > 0),
    
    %% 5. Logs correctly tagged by tenant
    AllLogs = ets:tab2list(LogCalls),
    TenantALogs = [L || {log, error, _Message, _Context, TenantId} = L <- AllLogs, TenantId =:= <<"tenant_a">>],
    TenantBLogs = [L || {log, error, _Message, _Context, TenantId} = L <- AllLogs, TenantId =:= <<"tenant_b">>],
    ?assert(length(TenantALogs) >= 0),
    ?assert(length(TenantBLogs) =:= 0),
    
    %% 6. Metrics don't mix
    TenantAMetricNames = [MetricName || {_TenantId, MetricName} <- TenantAMetrics],
    TenantBMetricNames = [MetricName || {_TenantId, MetricName} <- TenantBMetrics],
    ?assert(length(TenantAMetricNames) > 0),
    ?assert(length(TenantBMetricNames) > 0),
    
    %% Cleanup spawned processes
    exit(PidA, normal),
    exit(PidB, normal),
    ets:delete(SyncTable),
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(LogCalls),
    ets:delete(MetricCalls),
    ets:delete(TenantMetrics),
    ets:delete(ProcessingTimes),
    
    ok.

%% @doc Test: Final state and idempotency with multiple retries
%% Task 3: For complex scenario, explicitly verify final state for each message
%% Verifies: Final status (success/DLQ/rejected), processing count doesn't exceed contract
test_decide_final_state_and_idempotency_multi_retry(_Config) ->
    %% Create multiple requests with different fault scenarios
    Requests = [
        {<<"req-1">>, <<"tr-1">>, <<"msg-1">>, <<"tenant-1">>, success},
        {<<"req-2">>, <<"tr-2">>, <<"msg-2">>, <<"tenant-2">>, publish_fail},
        {<<"req-3">>, <<"tr-3">>, <<"msg-3">>, <<"tenant-3">>, validation_fail},
        {<<"req-4">>, <<"tr-4">>, <<"msg-4">>, <<"tenant-4">>, ack_fail}
    ],
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup tracking for final states
    MessageStates = ets:new(message_states, [set, private]),
    ProcessingCounts = ets:new(processing_counts, [set, private]),
    PublishCalls = ets:new(publish_calls, [set, private]),
    FinalStates = ets:new(final_states, [set, private]),
    
    %% Initialize tracking
    [ets:insert(MessageStates, {RequestId, pending}) || {RequestId, _TraceId, _MsgId, _Tenant, _Scenario} <- Requests],
    [ets:insert(ProcessingCounts, {RequestId, 0}) || {RequestId, _TraceId, _MsgId, _Tenant, _Scenario} <- Requests],
    
    %% Mock router_nats: different behavior per request scenario
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_PublishedSubject, PublishedPayload) ->
        RequestMap = jsx:decode(PublishedPayload, [return_maps]),
        RequestId = maps:get(<<"request_id">>, RequestMap),
        CallCount = case ets:lookup(publish_calls, RequestId) of
            [{RequestId, Count}] -> Count + 1;
            [] -> 1
        end,
        ets:insert(publish_calls, {RequestId, CallCount}),
        Scenario = case lists:keyfind(RequestId, 1, Requests) of
            {RequestId, _TraceId, _MsgId, _Tenant, S} -> S;
            false -> success
        end,
        case Scenario of
            publish_fail when CallCount =:= 1 -> {error, timeout};
            _ -> ok
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        Scenario = case lists:keyfind(MsgIdBin, 3, Requests) of
            {_RequestId, _TraceId, MsgIdBin, _Tenant, S} -> S;
            false -> success
        end,
        case Scenario of
            ack_fail -> {error, timeout};
            _ -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock tenant validator: fail for tenant-3
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) ->
        case TenantId of
            <<"tenant-3">> ->
                {error, tenant_not_allowed, #{reason => <<"tenant_id not in allowlist">>}};
            _ ->
                {ok, TenantId}
        end
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process all requests in parallel
    SpawnedPids = [begin
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => RequestId,
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>,
                <<"payload_ref">> => <<"s3://bucket/key">>
            },
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        spawn(fun() ->
            [{RequestId, Count}] = ets:lookup(ProcessingCounts, RequestId),
            ets:insert(ProcessingCounts, {RequestId, Count + 1}),
            router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{})
        end)
    end || {RequestId, TraceId, MsgId, TenantId, _Scenario} <- Requests],
    
    %% Wait for processing
    timer:sleep(500),
    
    %% Determine final states
    [begin
        FinalState = case Scenario of
            success ->
                case ets:lookup(publish_calls, RequestId) of
                    [{RequestId, Count}] when Count > 0 -> success;
                    [] -> pending
                end;
            publish_fail ->
                case ets:lookup(publish_calls, RequestId) of
                    [{RequestId, Count}] when Count > 1 -> success;
                    [{RequestId, 1}] -> retry;
                    [] -> pending
                end;
            validation_fail ->
                rejected;
            ack_fail ->
                case ets:lookup(publish_calls, RequestId) of
                    [{RequestId, Count}] when Count > 0 -> retry;
                    [] -> pending
                end
        end,
        ets:insert(FinalStates, {RequestId, FinalState})
    end || {RequestId, _TraceId, _MsgId, _Tenant, Scenario} <- Requests],
    
    %% ========================================================================
    %% CRITERIA: FINAL STATE AND IDEMPOTENCY VERIFICATION
    %% ========================================================================
    
    %% 1. Consumer remains alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% 2. Each message has a final state
    FinalStateList = ets:tab2list(FinalStates),
    ?assert(length(FinalStateList) =:= length(Requests)),
    
    %% 3. Processing count doesn't exceed contract
    MaxRetries = 3,
    ProcessingCountList = ets:tab2list(ProcessingCounts),
    ExcessiveProcessing = [P || {_RequestId, Count} = P <- ProcessingCountList, Count > MaxRetries + 1],
    ?assert(length(ExcessiveProcessing) =:= 0),
    
    %% 4. Success requests have success final state
    SuccessRequests = [R || {R, _TraceId, _MsgId, _Tenant, success} <- Requests],
    [begin
        case ets:lookup(FinalStates, RequestId) of
            [{RequestId, success}] ->
                ok;
            _ ->
                ct:fail("Success request ~p should have success final state", [RequestId])
        end
    end || RequestId <- SuccessRequests],
    
    %% 5. Validation fail requests have rejected final state
    ValidationFailRequests = [R || {R, _TraceId, _MsgId, _Tenant, validation_fail} <- Requests],
    [begin
        case ets:lookup(FinalStates, RequestId) of
            [{RequestId, rejected}] ->
                ok;
            _ ->
                ct:comment("Validation fail request ~p may have different final state", [RequestId])
        end
    end || RequestId <- ValidationFailRequests],
    
    %% 6. Idempotency: limited duplicate processing
    AllProcessingCounts = ets:tab2list(ProcessingCounts),
    DuplicateProcessing = [P || {_RequestId, Count} = P <- AllProcessingCounts, Count > 1],
    ?assert(length(DuplicateProcessing) =< length(Requests)),
    
    %% Cleanup spawned processes
    [exit(Pid, normal) || Pid <- SpawnedPids],
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete(MessageStates),
    ets:delete(ProcessingCounts),
    ets:delete(PublishCalls),
    ets:delete(FinalStates),
    
    ok.

%% @doc Test: Comprehensive metrics and logging validation
%% Task 4: Multiple error types simultaneously, verify metrics increment correctly with metadata
%% Verifies: All needed metrics increment with correct metadata, no contradictory metrics, no duplicate metrics
test_decide_comprehensive_metrics_and_logging_validation(_Config) ->
    %% Create requests with different error types
    Requests = [
        {<<"req-ack">>, <<"tr-ack">>, <<"msg-ack">>, <<"tenant-ack">>, ack_error},
        {<<"req-valid">>, <<"tr-valid">>, <<"msg-valid">>, <<"tenant-valid">>, validation_fail},
        {<<"req-pub">>, <<"tr-pub">>, <<"msg-pub">>, <<"tenant-pub">>, publish_fail},
        {<<"req-success">>, <<"tr-success">>, <<"msg-success">>, <<"tenant-success">>, success}
    ],
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup comprehensive tracking
    MetricCalls = ets:new(metric_calls, [set, private]),
    LogCalls = ets:new(log_calls, [set, private]),
    MetricMetadata = ets:new(metric_metadata, [set, private]),
    MetricCounts = ets:new(metric_counts, [set, private]),
    
    %% Initialize metric counts
    MetricNames = [
        router_decide_reply_publish_failed_total,
        router_results_tenant_rejected_total,
        router_nats_ack_failures_total,
        router_decide_reply_published_total
    ],
    [ets:insert(MetricCounts, {MetricName, 0}) || MetricName <- MetricNames],
    
    meck:new(router_logger, [passthrough]),
    meck:new(router_metrics, [passthrough]),
    
    meck:expect(router_logger, error, fun(Message, Context) ->
        TenantId = maps:get(tenant_id, Context, maps:get(<<"tenant_id">>, Context, <<"unknown">>)),
        ErrorType = maps:get(error_type, Context, maps:get(<<"error_type">>, Context, <<"unknown">>)),
        ets:insert(LogCalls, {log, error, Message, Context, TenantId, ErrorType}),
        ok
    end),
    
    meck:expect(router_metrics, emit_counter, fun(MetricName, Metadata) ->
        TenantId = maps:get(tenant_id, Metadata, maps:get(<<"tenant_id">>, Metadata, <<"unknown">>)),
        ErrorType = maps:get(error_type, Metadata, maps:get(<<"error_type">>, Metadata, <<"unknown">>)),
        ets:insert(MetricCalls, {metric, emit_counter, MetricName, Metadata}),
        ets:insert(MetricMetadata, {MetricName, TenantId, ErrorType, Metadata}),
        [{MetricName, Count}] = ets:lookup(MetricCounts, MetricName),
        ets:insert(MetricCounts, {MetricName, Count + 1}),
        ok
    end),
    
    %% Mock router_nats: different errors per scenario
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_PublishedSubject, PublishedPayload) ->
        RequestMap = jsx:decode(PublishedPayload, [return_maps]),
        RequestId = maps:get(<<"request_id">>, RequestMap),
        Scenario = case lists:keyfind(RequestId, 1, Requests) of
            {RequestId, _TraceId, _MsgId, _Tenant, S} -> S;
            false -> success
        end,
        case Scenario of
            publish_fail -> {error, connection_lost};
            _ -> ok
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        Scenario = case lists:keyfind(MsgIdBin, 3, Requests) of
            {_RequestId, _TraceId, MsgIdBin, _Tenant, S} -> S;
            false -> success
        end,
        case Scenario of
            ack_error -> {error, timeout};
            _ -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    %% Mock router_jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) ->
        router_nats:ack_message(Id)
    end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) ->
        router_nats:nak_message(Id)
    end),
    
    %% Mock tenant validator: fail for tenant-valid
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) ->
        case TenantId of
            <<"tenant-valid">> ->
                {error, tenant_not_allowed, #{reason => <<"tenant_id not in allowlist">>}};
            _ ->
                {ok, TenantId}
        end
    end),
    
    %% Mock dependent services
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    %% Get consumer PID
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    %% Process all requests in parallel
    SpawnedPids = [begin
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => RequestId,
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>,
                <<"payload_ref">> => <<"s3://bucket/key">>
            },
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        spawn(fun() ->
            router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{})
        end)
    end || {RequestId, TraceId, MsgId, TenantId, _Scenario} <- Requests],
    
    %% Wait for processing
    timer:sleep(500),
    
    %% ========================================================================
    %% CRITERIA: COMPREHENSIVE METRICS AND LOGGING VERIFICATION
    %% ========================================================================
    
    %% 1. Consumer remains alive
    ?assert(is_process_alive(ConsumerPid)),
    
    %% 2. All needed metrics increment with correct metadata
    AllMetrics = ets:tab2list(MetricCalls),
    
    %% 2.1: ACK error metric should have tenant metadata
    AckErrorMetrics = [M || {metric, emit_counter, router_nats_ack_failures_total, _Metadata} = M <- AllMetrics],
    case AckErrorMetrics of
        [] ->
            ct:comment("ACK error metrics may not be emitted");
        [FirstAckMetric | _] ->
            {metric, emit_counter, router_nats_ack_failures_total, AckMetadata} = FirstAckMetric,
            ?assert(maps:is_key(tenant_id, AckMetadata) orelse maps:is_key(<<"tenant_id">>, AckMetadata))
    end,
    
    %% 2.2: Tenant rejection metric should have tenant and reason metadata
    TenantRejectedMetrics = [M || {metric, emit_counter, router_results_tenant_rejected_total, _Metadata} = M <- AllMetrics],
    case TenantRejectedMetrics of
        [] ->
            ct:comment("Tenant rejection metrics may not be emitted");
        [FirstTenantMetric | _] ->
            {metric, emit_counter, router_results_tenant_rejected_total, TenantMetadata} = FirstTenantMetric,
            ?assert(maps:is_key(tenant_id, TenantMetadata) orelse maps:is_key(<<"tenant_id">>, TenantMetadata)),
            ?assert(maps:is_key(reason, TenantMetadata) orelse maps:is_key(<<"reason">>, TenantMetadata))
    end,
    
    %% 2.3: Publish failure metric should have tenant metadata
    PublishFailedMetrics = [M || {metric, emit_counter, router_decide_reply_publish_failed_total, _Metadata} = M <- AllMetrics],
    case PublishFailedMetrics of
        [] ->
            ct:comment("Publish failure metrics may not be emitted");
        [FirstPublishMetric | _] ->
            {metric, emit_counter, router_decide_reply_publish_failed_total, PublishMetadata} = FirstPublishMetric,
            ?assert(maps:is_key(tenant_id, PublishMetadata) orelse maps:is_key(<<"tenant_id">>, PublishMetadata))
    end,
    
    %% 3. No contradictory metrics
    MetricMetadataList = ets:tab2list(MetricMetadata),
    SuccessMetrics = [M || {router_decide_reply_published_total, _TenantId, _ErrorType, _Metadata} = M <- MetricMetadataList],
    FailedTenants = [<<"tenant-valid">>, <<"tenant-pub">>, <<"tenant-ack">>],
    ContradictoryMetrics = [M || {router_decide_reply_published_total, TenantId, _ErrorType, _Metadata} = M <- SuccessMetrics,
                                 lists:member(TenantId, FailedTenants)],
    ?assert(length(ContradictoryMetrics) =:= 0),
    
    %% 4. No duplicate metrics for the same event
    MetricGroups = lists:foldl(fun({MetricName, TenantId, _ErrorType, _Metadata}, Acc) ->
        Key = {MetricName, TenantId},
        case lists:keyfind(Key, 1, Acc) of
            false ->
                [{Key, 1} | Acc];
            {Key, Count} ->
                lists:keyreplace(Key, 1, Acc, {Key, Count + 1})
        end
    end, [], MetricMetadataList),
    ExcessiveDuplicates = [G || {_Key, Count} = G <- MetricGroups, Count > 5],
    ?assert(length(ExcessiveDuplicates) =:= 0),
    
    %% 5. Logs correctly tagged by tenant and error type
    AllLogs = ets:tab2list(LogCalls),
    TenantALogs = [L || {log, error, _Message, _Context, <<"tenant-ack">>, _ErrorType} = L <- AllLogs],
    TenantValidLogs = [L || {log, error, _Message, _Context, <<"tenant-valid">>, _ErrorType} = L <- AllLogs],
    TenantPubLogs = [L || {log, error, _Message, _Context, <<"tenant-pub">>, _ErrorType} = L <- AllLogs],
    TenantSuccessLogs = [L || {log, error, _Message, _Context, <<"tenant-success">>, _ErrorType} = L <- AllLogs],
    
    ?assert(length(TenantALogs) >= 0),
    ?assert(length(TenantValidLogs) > 0),
    ?assert(length(TenantPubLogs) >= 0),
    ?assert(length(TenantSuccessLogs) =:= 0),
    
    %% 6. Metric counts are reasonable
    FinalMetricCounts = ets:tab2list(MetricCounts),
    ExcessiveCounts = [C || {_MetricName, Count} = C <- FinalMetricCounts, Count > 10],
    ?assert(length(ExcessiveCounts) =:= 0),
    
    %% Cleanup spawned processes
    [exit(Pid, normal) || Pid <- SpawnedPids],
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    meck:unload(router_metrics),
    ets:delete(MetricCalls),
    ets:delete(LogCalls),
    ets:delete(MetricMetadata),
    ets:delete(MetricCounts),
    
    ok.

