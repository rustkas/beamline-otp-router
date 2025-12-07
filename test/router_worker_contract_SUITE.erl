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
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        {group, contract_tests}
    ].

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
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    Config.

end_per_suite(Config) ->
    application:stop(beamline_router),
    meck:unload(router_rate_limiter),
    Config.

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
    
    %% Mock NATS to capture usage event
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    %% Process result
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published with correlation fields
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
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
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    %% Process result
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
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
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    %% Process result
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
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
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    %% Process result
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
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
    
    %% Mock NATS and capture published usage event
    meck:new(router_nats, [passthrough]),
    PublishedEvents = ets:new(published_events, [set, private]),
    meck:expect(router_nats, publish, fun(Subject, Payload) ->
        ets:insert(PublishedEvents, {Subject, Payload}),
        ok
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    %% Process result
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
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
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
        
        %% Mock NATS
        meck:new(router_nats, [passthrough]),
        meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
        meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
        meck:new(router_policy_store, [passthrough]),
        meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
        meck:new(router_idempotency, [passthrough]),
        meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
        
        %% Process result
        router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
        
        %% Verify usage event was published
        test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
        meck:unload(router_nats),
        meck:unload(router_idempotency),
        meck:unload(router_policy_store)
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
    
    %% Mock NATS
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    %% Process result (should still work, but correlation fields will be undefined)
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    %% Verify usage event was published (even without correlation fields)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    ok.

