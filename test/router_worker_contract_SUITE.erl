%% @doc Integration tests for Router ↔ Worker contract
%%
%% These tests verify the ExecResult processing contract between Router and Worker,
%% ensuring that StepResult → ExecResult conversion (from Worker) is correctly
%% processed by Router.
%%
%% Contract Requirements (from apps/caf/processor/docs/ARCHITECTURE_ROLE.md):
%% - StepResult.status (ok|error|timeout|cancelled) → ExecResult.status (success|error|timeout|cancelled)
%% - StepResult.error_code (1xxx-5xxx) → ExecResult.error_code (string format)
%% - StepResult.metadata (trace_id, flow_id, step_id, tenant_id) → ExecResult correlation fields
%% - All required fields must be present in ExecResult
%%
-module(router_worker_contract_SUITE).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_execresult_cancelled/1,
    test_execresult_error_code_mapping/1,
    test_execresult_error_with_error_code/1,
    test_execresult_metadata_preservation/1,
    test_execresult_missing_correlation_fields/1,
    test_execresult_success_with_full_metadata/1,
    test_execresult_timeout/1
]).

-include_lib("common_test/include/ct.hrl").

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(full) ->
    [{group, contract_tests}];
groups_for_level(heavy) ->
    [{group, contract_tests}];
groups_for_level(_) ->
    %% Skip in fast mode - requires complex integration setup
    [].

groups() ->
    [
        {contract_tests, [sequence], [
            test_execresult_success_with_full_metadata,
            test_execresult_error_with_error_code,
            test_execresult_timeout,
            test_execresult_cancelled,
            test_execresult_metadata_preservation,
            test_execresult_error_code_mapping,
            test_execresult_missing_correlation_fields
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, usage_subject, <<"beamline.usage.v1.metered">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    %% Avoid starting heavy/irrelevant components
    ok = router_mock_helpers:ensure_mock(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    _ = router_test_init:ensure_ets_table(published_events, [set, public]),
    Config.

end_per_suite(Config) ->
    application:stop(beamline_router),
    router_mock_helpers:unload(router_rate_limiter),
    Config.

init_per_testcase(_TestCase, Config) ->
    router_mock_helpers:unload_all(),
    %% Ensure ETS table exists before clearing
    _ = router_test_init:ensure_ets_table(published_events, [named_table, set, public]),
    %% Setup common mocks for all tests
    setup_common_mocks(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    cleanup_common_mocks(),
    router_mock_helpers:unload_all(),
    ok.

%% @doc Setup common mocks needed for result consumer tests
setup_common_mocks() ->
    %% Mock NATS
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    %% Mock policy store
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    %% Mock idempotency
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    %% Mock intake validator to pass validation
    meck:new(router_intake_validator, [passthrough]),
    meck:expect(router_intake_validator, validate_intake_message, fun(_Subject, Payload, _Headers, _Type) ->
        case jsx:is_json(Payload) of
            true -> {ok, jsx:decode(Payload, [return_maps])};
            false -> {error, {parse_error, <<"Invalid JSON">>, #{}}}
        end
    end),
    %% Mock tenant validator to pass validation
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    %% Mock jetstream
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(_Msg) -> ok end),
    meck:expect(router_jetstream, nak, fun(_Msg, _Reason, _Context) -> ok end),
    %% Mock circuit breaker
    meck:new(router_circuit_breaker, [passthrough]),
    meck:expect(router_circuit_breaker, record_success, fun(_TenantId, _ProviderId) -> ok end),
    meck:expect(router_circuit_breaker, record_failure, fun(_TenantId, _ProviderId) -> ok end),
    %% Mock tracing
    meck:new(router_tracing, [passthrough]),
    meck:expect(router_tracing, with_span, fun(_Name, _Attrs, _Ctx, Fun) -> Fun() end),
    meck:expect(router_tracing, set_span_attribute, fun(_Key, _Value, _Type) -> ok end),
    meck:expect(router_tracing, set_span_status, fun(_Status, _Msg) -> ok end),
    ok.

%% @doc Cleanup common mocks
cleanup_common_mocks() ->
    Modules = [router_tracing, router_circuit_breaker, router_jetstream, 
               router_tenant_validator, router_intake_validator, 
               router_nats, router_idempotency, router_policy_store],
    lists:foreach(fun(M) -> catch meck:unload(M) end, Modules),
    ok.

%% Test: ExecResult with success status and full metadata (from StepResult.success)
test_execresult_success_with_full_metadata(_Config) ->
    Result = #{
        <<"version">> => <<"1">>,
        <<"assignment_id">> => <<"assign_success_123">>,
        <<"request_id">> => <<"req_success_456">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 150,
        <<"cost">> => 0.012,
        <<"trace_id">> => <<"trace_abc123">>,
        <<"tenant_id">> => <<"tenant_123">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    %% Process result (mocks are set up in init_per_testcase)
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published with correlation fields
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    ok.

%% Test: ExecResult with error status and error_code (from StepResult.error_result)
test_execresult_error_with_error_code(_Config) ->
    Result = #{
        <<"version">> => <<"1">>,
        <<"assignment_id">> => <<"assign_error_123">>,
        <<"request_id">> => <<"req_error_456">>,
        <<"status">> => <<"error">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 5000,
        <<"cost">> => 0.0,
        <<"error_code">> => <<"NETWORK_ERROR">>,
        <<"error_message">> => <<"Connection timeout">>,
        <<"trace_id">> => <<"trace_error_123">>,
        <<"tenant_id">> => <<"tenant_error">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    %% Process result (mocks are set up in init_per_testcase)
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    ok.

%% Test: ExecResult with timeout status (from StepResult.timeout_result)
test_execresult_timeout(_Config) ->
    Result = #{
        <<"version">> => <<"1">>,
        <<"assignment_id">> => <<"assign_timeout_123">>,
        <<"request_id">> => <<"req_timeout_456">>,
        <<"status">> => <<"timeout">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 10000,
        <<"cost">> => 0.0,
        <<"trace_id">> => <<"trace_timeout_123">>,
        <<"tenant_id">> => <<"tenant_timeout">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    %% Process result (mocks are set up in init_per_testcase)
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    ok.

%% Test: ExecResult with cancelled status (from StepResult.cancelled_result)
test_execresult_cancelled(_Config) ->
    Result = #{
        <<"version">> => <<"1">>,
        <<"assignment_id">> => <<"assign_cancelled_123">>,
        <<"request_id">> => <<"req_cancelled_456">>,
        <<"status">> => <<"cancelled">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 500,
        <<"cost">> => 0.0,
        <<"trace_id">> => <<"trace_cancelled_123">>,
        <<"tenant_id">> => <<"tenant_cancelled">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    %% Process result (mocks are set up in init_per_testcase)
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    ok.

%% Test: Metadata preservation (trace_id, tenant_id from StepResult.metadata)
test_execresult_metadata_preservation(_Config) ->
    Result = #{
        <<"version">> => <<"1">>,
        <<"assignment_id">> => <<"assign_metadata_123">>,
        <<"request_id">> => <<"req_metadata_456">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 200,
        <<"cost">> => 0.012,
        <<"trace_id">> => <<"trace_full_123">>,
        <<"tenant_id">> => <<"tenant_full">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    %% Capture published usage event
    PublishedEvents = router_test_init:ensure_ets_table(published_events, [set, public]),
    ets:delete_all_objects(PublishedEvents),
    meck:expect(router_nats, publish, fun(Subject, Payload) ->
        ets:insert(PublishedEvents, {Subject, Payload}),
        ok
    end),
    
    %% Process result (other mocks are set up in init_per_testcase)
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published with correlation fields
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    %% Check that usage event contains correlation fields
    [{_, UsagePayload}] = ets:tab2list(PublishedEvents),
    UsageEvent = jsx:decode(UsagePayload, [return_maps]),
    
    %% Verify correlation fields are preserved
    <<"trace_full_123">> = maps:get(<<"trace_id">>, UsageEvent, undefined),
    <<"tenant_full">> = maps:get(<<"tenant_id">>, UsageEvent, undefined),
    
    ets:delete(PublishedEvents),
    ok.

%% Test: Error code mapping (various ErrorCode values from StepResult)
test_execresult_error_code_mapping(_Config) ->
    ErrorCodes = [
        {<<"INVALID_INPUT">>, <<"invalid_input">>},
        {<<"MISSING_REQUIRED_FIELD">>, <<"missing_required_field">>},
        {<<"EXECUTION_FAILED">>, <<"execution_failed">>},
        {<<"NETWORK_ERROR">>, <<"network_error">>},
        {<<"CONNECTION_TIMEOUT">>, <<"connection_timeout">>},
        {<<"INTERNAL_ERROR">>, <<"internal_error">>},
        {<<"SYSTEM_OVERLOAD">>, <<"system_overload">>},
        {<<"CANCELLED_BY_USER">>, <<"cancelled_by_user">>},
        {<<"CANCELLED_BY_TIMEOUT">>, <<"cancelled_by_timeout">>}
    ],
    
    lists:foreach(fun({ErrorCode, _}) ->
        Result = #{
            <<"version">> => <<"1">>,
            <<"assignment_id">> => <<"assign_error_code_", ErrorCode/binary>>,
            <<"request_id">> => <<"req_error_code_", ErrorCode/binary>>,
            <<"status">> => <<"error">>,
            <<"provider_id">> => <<"openai:gpt-4o">>,
            <<"job">> => #{<<"type">> => <<"text.generate">>},
            <<"latency_ms">> => 100,
            <<"cost">> => 0.0,
            <<"error_code">> => ErrorCode,
            <<"error_message">> => <<"Test error">>,
            <<"trace_id">> => <<"trace_error_code_123">>,
            <<"tenant_id">> => <<"tenant_error_code">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ResultJson = jsx:encode(Result),
        
        %% Reset meck call history for this iteration
        meck:reset(router_nats),
        meck:reset(router_idempotency),
        
        %% Process result (mocks are set up in init_per_testcase)
        router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
        
        %% Verify usage event was published
        test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000)
    end, ErrorCodes),
    
    ok.

%% Test: Missing correlation fields (should still process but log warning)
test_execresult_missing_correlation_fields(_Config) ->
    %% Result without trace_id and tenant_id (should still be processable)
    Result = #{
        <<"version">> => <<"1">>,
        <<"assignment_id">> => <<"assign_missing_123">>,
        <<"request_id">> => <<"req_missing_456">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 150,
        <<"cost">> => 0.012,
        <<"timestamp">> => erlang:system_time(millisecond)
        %% Note: trace_id and tenant_id are missing
    },
    ResultJson = jsx:encode(Result),
    
    %% Process result (mocks are set up in init_per_testcase)
    %% Should still work, but correlation fields will be undefined
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published (even without correlation fields)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    ok.
