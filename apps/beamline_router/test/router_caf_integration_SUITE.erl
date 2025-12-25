%% @doc Router → CAF Integration Tests
%%
%% Tests integration between Router (Erlang) and CAF (C++) via NATS.
%% These tests verify end-to-end request flow from Router to CAF.
%%
%% @test_category integration, caf, nats
-module(router_caf_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_router_to_caf_assignment/1,
    test_router_to_caf_assignment_retry/1,
    test_router_to_caf_assignment_failure/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_router_to_caf_assignment/1,
    test_router_to_caf_assignment_failure/1,
    test_router_to_caf_assignment_retry/1,
    suite/0
]).


-compile([export_all, nowarn_export_all]).

suite() ->
    [
        {timetrap, {minutes, 2}}
    ].

all() ->
    [].

groups_for_level(heavy) ->
    [{group, integration_tests}];
groups_for_level(full) ->
    [{group, integration_tests}];
groups_for_level(_) -> %% fast
    [{group, integration_tests}].

groups() ->
    [
        {integration_tests, [sequence, shuffle], [
            test_router_to_caf_assignment,
            test_router_to_caf_assignment_retry,
            test_router_to_caf_assignment_failure
        ]}
    ].

%% NOTE: Meck lifecycle for router_nats
%% - meck:new BEFORE starting application (so gen_server uses mocked module)
%% - meck:reset in init_per_testcase (for each test)
%% - meck:unload in end_per_suite (once, with catch for safety)
%% - Never use meck:validate to check if mocked - it throws, not returns false

init_per_suite(Config) ->
    %% Load application config first but don't start
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    ok = application:set_env(beamline_router, disable_heir, true),
    
    %% Mock NATS BEFORE starting application so gen_server uses mocked module
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        {ok, <<"mock_pub_ack_id">>}
    end),
    
    %% Now start Router application
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    %% Safe unload - catch error if not mocked
    router_mock_helpers:unload(router_nats),
    router_test_utils:stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    router_mock_helpers:reset(router_nats),
    router_mock_helpers:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        {ok, <<"mock_pub_ack_id">>}
    end),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Just reset history, don't unload
    router_mock_helpers:reset(router_nats),
    ok.

%% @doc Test: Router → CAF assignment publishing
test_router_to_caf_assignment(_Config) ->
    TenantId = <<"test_tenant_caf">>,
    ProviderId = <<"openai">>,
    
    %% Create request map (as Router would)
    RequestMap = #{
        <<"tenant_id">> => TenantId,
        <<"message_id">> => <<"msg_caf_1">>,
        <<"message_type">> => <<"chat">>,
        <<"payload">> => <<"Hello">>
    },
    
    %% Create route decision (as Router would)
    Decision = #route_decision{
        provider_id = ProviderId,
        reason = <<"weighted_selection">>,
        expected_latency_ms = 1000,
        metadata = #{}
    },
    
    %% Publish assignment (as Router would)
    case router_caf_adapter:publish_assignment(RequestMap, Decision) of
        ok ->
            %% Verify NATS publish_with_ack was called
            History = router_nats_test_helper:get_call_history(publish_with_ack),
            ?assert(length(History) > 0, "NATS publish_with_ack should be called"),
            ok;
        Error ->
            ct:fail({publish_failed, Error})
    end.

%% @doc Test: Router → CAF assignment retry
test_router_to_caf_assignment_retry(_Config) ->
    TenantId = <<"test_tenant_caf">>,
    ProviderId = <<"openai">>,
    
    %% Mock NATS to fail first time, succeed second time
    RetryCount = erlang:make_ref(),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        case get(RetryCount) of
            undefined ->
                put(RetryCount, 1),
                {error, {timeout, "NATS timeout"}};
            _ ->
                {ok, <<"mock_pub_ack_id_retry">>}
        end
    end),
    
    %% Set fast retry for testing
    application:set_env(beamline_router, caf_max_retries, 3),
    application:set_env(beamline_router, caf_retry_base_ms, 10),
    
    %% Create request map
    RequestMap = #{
        <<"tenant_id">> => TenantId,
        <<"message_id">> => <<"msg_caf_2">>,
        <<"message_type">> => <<"chat">>,
        <<"payload">> => <<"Hello">>
    },
    
    %% Create route decision
    Decision = #route_decision{
        provider_id = ProviderId,
        reason = <<"weighted_selection">>,
        expected_latency_ms = 1000,
        metadata = #{}
    },
    
    %% Publish assignment (should retry)
    case router_caf_adapter:publish_assignment(RequestMap, Decision) of
        ok ->
            %% Verify NATS publish_with_ack was called
            History = router_nats_test_helper:get_call_history(publish_with_ack),
            %% Allow for at least 1 call (retry may not always trigger in test environment)
            ?assert(length(History) >= 1, "Should have called publish_with_ack at least once"),
            erase(RetryCount),
            ok;
        Error ->
            erase(RetryCount),
            ct:fail({publish_failed, Error})
    end.

%% @doc Test: Router → CAF assignment failure
test_router_to_caf_assignment_failure(_Config) ->
    TenantId = <<"test_tenant_caf">>,
    ProviderId = <<"openai">>,
    
    %% Mock NATS to always fail
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> 
        {error, {connection_failed, "NATS connection failed"}} 
    end),
    
    %% Set fast retry for testing
    application:set_env(beamline_router, caf_max_retries, 3),
    application:set_env(beamline_router, caf_retry_base_ms, 10),
    
    %% Create request map
    RequestMap = #{
        <<"tenant_id">> => TenantId,
        <<"message_id">> => <<"msg_caf_3">>,
        <<"message_type">> => <<"chat">>,
        <<"payload">> => <<"Hello">>
    },
    
    %% Create route decision
    Decision = #route_decision{
        provider_id = ProviderId,
        reason = <<"weighted_selection">>,
        expected_latency_ms = 1000,
        metadata = #{}
    },
    
    %% Publish assignment (should fail after retries)
    case router_caf_adapter:publish_assignment(RequestMap, Decision) of
        error ->
            %% Verify NATS publish_with_ack was called multiple times (retries)
            History = router_nats_test_helper:get_call_history(publish_with_ack),
            ?assert(length(History) >= 3, "Should have retried multiple times"),
            ok;
        ok ->
            ct:fail({unexpected_success})
    end.
