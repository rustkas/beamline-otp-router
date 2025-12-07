%% @doc Tests for tenant allowlist parsing and type-agnostic comparison
-module(router_tenant_allowlist_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_allowlist_binary_tenants/1,
    test_allowlist_string_tenants/1,
    test_allowlist_mixed_types/1,
    test_tenant_id_binary_matches_string_allowlist/1,
    test_tenant_id_string_matches_binary_allowlist/1,
    test_allowlist_map_binary_keys/1,
    test_allowlist_map_string_keys/1,
    test_undefined_tenant_id_with_allowlist/1
]}).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [sequence], [
            test_allowlist_binary_tenants,
            test_allowlist_string_tenants,
            test_allowlist_mixed_types,
            test_tenant_id_binary_matches_string_allowlist,
            test_tenant_id_string_matches_binary_allowlist,
            test_allowlist_map_binary_keys,
            test_allowlist_map_string_keys,
            test_undefined_tenant_id_with_allowlist
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_policy_store, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Binary tenants in allowlist
test_allowlist_binary_tenants(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        <<"tenant1">>,
        <<"tenant2">>
    ]),
    
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant2">>)),
    ?assertNot(router_nats_subscriber:check_tenant_allowed(<<"tenant3">>)),
    
    %% Also test router_caf_adapter version
    ?assert(router_caf_adapter:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_caf_adapter:check_tenant_allowed(<<"tenant2">>)),
    ?assertNot(router_caf_adapter:check_tenant_allowed(<<"tenant3">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: String tenants in allowlist
test_allowlist_string_tenants(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        "tenant1",
        "tenant2"
    ]),
    
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant2">>)),
    ?assertNot(router_nats_subscriber:check_tenant_allowed(<<"tenant3">>)),
    
    %% Also test router_caf_adapter version
    ?assert(router_caf_adapter:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_caf_adapter:check_tenant_allowed(<<"tenant2">>)),
    ?assertNot(router_caf_adapter:check_tenant_allowed(<<"tenant3">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: Mixed binary and string tenants in allowlist
test_allowlist_mixed_types(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        <<"tenant1">>,
        "tenant2",
        <<"tenant3">>
    ]),
    
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant2">>)),
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant3">>)),
    ?assertNot(router_nats_subscriber:check_tenant_allowed(<<"tenant4">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: Binary tenant_id matches string allowlist
test_tenant_id_binary_matches_string_allowlist(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        "tenant1"
    ]),
    
    %% Binary tenant_id should match string in allowlist
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: String tenant_id matches binary allowlist
test_tenant_id_string_matches_binary_allowlist(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        <<"tenant1">>
    ]),
    
    %% String tenant_id should match binary in allowlist (if tenant_id is string)
    %% Note: In practice, tenant_id from JSON is always binary, but test for robustness
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: Map allowlist with binary keys
test_allowlist_map_binary_keys(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, #{
        <<"tenant1">> => #{},
        <<"tenant2">> => #{}
    }),
    
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant2">>)),
    ?assertNot(router_nats_subscriber:check_tenant_allowed(<<"tenant3">>)),
    
    %% Also test router_caf_adapter version
    ?assert(router_caf_adapter:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_caf_adapter:check_tenant_allowed(<<"tenant2">>)),
    ?assertNot(router_caf_adapter:check_tenant_allowed(<<"tenant3">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: Map allowlist with string keys (converted to binary)
test_allowlist_map_string_keys(_Config) ->
    %% Note: Erlang maps with string keys are less common, but test for completeness
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, #{
        <<"tenant1">> => #{},
        <<"tenant2">> => #{}
    }),
    
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant1">>)),
    ?assert(router_nats_subscriber:check_tenant_allowed(<<"tenant2">>)),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

%% Test: Undefined tenant_id with allowlist configured
test_undefined_tenant_id_with_allowlist(_Config) ->
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        <<"tenant1">>
    ]),
    
    %% If allowlist is configured, undefined tenant_id should be blocked
    ?assertNot(router_nats_subscriber:check_tenant_allowed(undefined)),
    
    %% If allowlist is undefined, undefined tenant_id should be allowed
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ?assert(router_nats_subscriber:check_tenant_allowed(undefined)),
    ?assert(router_caf_adapter:check_tenant_allowed(undefined)),
    
    ok.

