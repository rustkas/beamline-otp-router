%% @doc Integration Test Suite for Router gRPC Health Endpoint
%% Tests gRPC health service availability, response format, and status values
%% @test_category cp1_smoke, integration
-module(router_health_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/flow_pb.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/_build/test/lib/grpcbox/include/grpcbox.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([groups_for_level/1]).
-export([
    test_health_module_loaded/1,
    test_health_check_serving/1,
    test_get_checkpoint_status/1,
    test_get_validators_health/1,
    suite/0
]).

suite() ->
    [
        {timetrap, {minutes, 1}}
    ].

all() ->
    [].

groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, fast_smoke_tests}];  %% Minimal smoke at fast level
groups_for_level(full) -> [{group, fast_smoke_tests}, {group, health_tests}];
groups_for_level(heavy) -> [{group, fast_smoke_tests}, {group, health_tests}].

groups() ->
    [
        {fast_smoke_tests, [], [
            test_health_module_loaded
        ]},
        {health_tests, [parallel], [
            test_health_check_serving,
            test_get_checkpoint_status,
            test_get_validators_health
        ]}
    ].

init_per_suite(Config) ->
    router_test_bootstrap:init_per_suite(Config, #{
        common_env => false,
        app_env => #{
            grpc_enabled => true,
            admin_grpc_enabled => true,
            grpc_port => 0,
            admin_api_key => <<"test-admin-key">>
        }
    }).

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(_TestCase, Config) ->
    router_test_bootstrap:init_per_testcase(_TestCase, Config, #{}).

end_per_testcase(_TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(_TestCase, Config, #{}).

%% ============================================================================
%% Fast Smoke Tests (minimal, no gRPC connection required)
%% ============================================================================

%% @doc Verify that health-related modules are loaded and export expected functions
%% This is a minimal smoke test for fast tier that doesn't require full gRPC setup
test_health_module_loaded(_Config) ->
    %% Verify router_admin_grpc module is loaded
    {module, router_admin_grpc} = code:ensure_loaded(router_admin_grpc),
    
    %% Verify expected exports exist
    Exports = router_admin_grpc:module_info(exports),
    ?assert(lists:member({get_checkpoint_status, 2}, Exports)),
    ?assert(lists:member({get_validators_health, 2}, Exports)),
    ?assert(lists:member({get_admin_key, 0}, Exports)),
    ok.

%% ============================================================================
%% Full Integration Tests (require gRPC setup)
%% ============================================================================

%% @doc Verify that the gRPC health check service is running
test_health_check_serving(_Config) ->
    %% Check if router_grpc_sup is running
    SupPid = whereis(router_grpc_sup),
    ?assert(is_pid(SupPid)),
    ?assert(is_process_alive(SupPid)),
    ok.

%% @doc Verify Checkpoint Status endpoint
test_get_checkpoint_status(_Config) ->
    Req = #'GetCheckpointStatusRequest'{},
    EncodedReq = flow_pb:encode_msg(Req, 'GetCheckpointStatusRequest'),
    Ctx = create_context(),
    
    {ok, EncodedResp, _} = router_admin_grpc:get_checkpoint_status(Ctx, EncodedReq),
    Resp = flow_pb:decode_msg(EncodedResp, 'GetCheckpointStatusResponse'),
    
    Status = Resp#'GetCheckpointStatusResponse'.status,
    ?assert(is_binary(Status#'CheckpointStatus'.current_cp)),
    ok.

%% @doc Verify Validators Health endpoint
test_get_validators_health(_Config) ->
    Req = #'GetValidatorsHealthRequest'{},
    EncodedReq = flow_pb:encode_msg(Req, 'GetValidatorsHealthRequest'),
    Ctx = create_context(),
    
    {ok, EncodedResp, _} = router_admin_grpc:get_validators_health(Ctx, EncodedReq),
    Resp = flow_pb:decode_msg(EncodedResp, 'GetValidatorsHealthResponse'),
    
    Validators = Resp#'GetValidatorsHealthResponse'.validators,
    ?assert(is_list(Validators)),
    %% Identify the default validator
    [Val] = Validators,
    ?assertEqual(<<"grpc_health">>, Val#'ValidatorStatus'.name),
    ?assertEqual(<<"serving">>, Val#'ValidatorStatus'.status),
    ok.

create_context() ->
    ApiKey = router_admin_grpc:get_admin_key(),
    #{metadata => [{<<"x-api-key">>, ApiKey}]}.
