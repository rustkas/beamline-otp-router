%% @doc Common Test Suite for RouterAdmin CP Status & Validators Health
-module(router_admin_cp_status_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
%% -include_lib("grpcbox/include/grpcbox.hrl"). %% Removed - not needed for this test

%% Define GRPC status codes locally (from grpcbox.hrl)
-define(GRPC_STATUS_UNAUTHENTICATED, 16).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
%% Test functions are called via groups() by Common Test framework
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_get_checkpoint_status_ok/1,
    test_get_validators_health_ok/1,
    test_get_checkpoint_status_unauthenticated/1,
    test_get_validators_health_unauthenticated/1,
    create_context_with_auth/1, create_context_without_auth/0, decode_json/1
]}).


all() ->
    [
        {group, admin_cp_status}
    ].

groups() ->
    [
        {admin_cp_status, [sequence], [
            test_get_checkpoint_status_ok,
            test_get_validators_health_ok,
            test_get_checkpoint_status_unauthenticated,
            test_get_validators_health_unauthenticated
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, admin_api_key, <<"test-admin-key">>),
    ok = application:set_env(beamline_router, rate_limits, #{default_requests_per_minute => 100}),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% Test: GetCheckpointStatus success
test_get_checkpoint_status_ok(_Config) ->
    Ctx = create_context_with_auth(<<"test-admin-key">>),
    {ok, ResponseBin, _} = router_admin_grpc:get_checkpoint_status(Ctx, <<>>),
    ?assert(is_binary(ResponseBin)),
    ok.

%% Test: GetValidatorsHealth success
test_get_validators_health_ok(_Config) ->
    Ctx = create_context_with_auth(<<"test-admin-key">>),
    {ok, ResponseBin, _} = router_admin_grpc:get_validators_health(Ctx, <<>>),
    ?assert(is_binary(ResponseBin)),
    ok.

%% Test: GetCheckpointStatus unauthenticated
test_get_checkpoint_status_unauthenticated(_Config) ->
    Ctx = create_context_without_auth(),
    try
        router_admin_grpc:get_checkpoint_status(Ctx, <<>>),
        ct:fail("Should have thrown grpc_error")
    catch
        {grpc_error, {Status, _Msg}} ->
            ?assertEqual(?GRPC_STATUS_UNAUTHENTICATED, Status)
    end,
    ok.

%% Test: GetValidatorsHealth unauthenticated
test_get_validators_health_unauthenticated(_Config) ->
    Ctx = create_context_without_auth(),
    try
        router_admin_grpc:get_validators_health(Ctx, <<>>),
        ct:fail("Should have thrown grpc_error")
    catch
        {grpc_error, {Status, _Msg}} ->
            ?assertEqual(?GRPC_STATUS_UNAUTHENTICATED, Status)
    end,
    ok.

%% Helpers
create_context_with_auth(AdminKey) ->
    #{metadata => [{<<"x-api-key">>, AdminKey}]}.

create_context_without_auth() ->
    #{metadata => []}.

decode_json(Bin) when is_binary(Bin) ->
    jsx:decode(Bin, [return_maps]).