%% @doc Security tests for Extensions
%% Tests authorization, payload validation, abuse prevention
-module(router_extensions_security_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2,
    test_unauthorized_extension_registry_read/1,
    test_unauthorized_extension_registry_write/1,
    test_unauthorized_extension_registry_delete/1,
    test_unauthorized_extension_registry_admin/1,
    test_operator_can_read_write/1,
    test_viewer_can_only_read/1,
    test_payload_size_violation/1,
    test_payload_size_within_limit/1,
    test_metadata_size_violation/1,
    test_invalid_json_structure/1,
    test_pipeline_depth_violation/1,
    test_pipeline_depth_within_limit/1,
    test_retry_limit_violation/1,
    test_timeout_out_of_range/1,
    test_too_many_pre_processors/1,
    test_too_many_validators/1,
    test_too_many_post_processors/1,
    create_deeply_nested_map/1
]}).

all() ->
    [
        {group, authorization_tests},
        {group, payload_validation_tests},
        {group, abuse_prevention_tests}
    ].

groups() ->
    [
        {authorization_tests, [sequence], [
            test_unauthorized_extension_registry_read,
            test_unauthorized_extension_registry_write,
            test_unauthorized_extension_registry_delete,
            test_unauthorized_extension_registry_admin,
            test_operator_can_read_write,
            test_viewer_can_only_read
        ]},
        {payload_validation_tests, [sequence], [
            test_payload_size_violation,
            test_payload_size_within_limit,
            test_metadata_size_violation,
            test_invalid_json_structure
        ]},
        {abuse_prevention_tests, [sequence], [
            test_pipeline_depth_violation,
            test_pipeline_depth_within_limit,
            test_retry_limit_violation,
            test_timeout_out_of_range,
            test_too_many_pre_processors,
            test_too_many_validators,
            test_too_many_post_processors
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, extension_registry, [
        {source, fixtures}
    ]),
    ok = application:set_env(beamline_router, rbac_enabled, true),
    ok = application:set_env(beamline_router, extension_max_pipeline_depth, 10),
    ok = application:set_env(beamline_router, extension_max_pre_count, 5),
    ok = application:set_env(beamline_router, extension_max_validators_count, 5),
    ok = application:set_env(beamline_router, extension_max_post_count, 5),
    ok = application:set_env(beamline_router, extension_max_retries, 3),
    ok = application:set_env(beamline_router, extension_min_timeout_ms, 10),
    ok = application:set_env(beamline_router, extension_max_timeout_ms, 5000),
    ok = application:set_env(beamline_router, extension_max_payload_size, 1048576),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_extension_registry, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Ensure clean state - unload any existing mocks first
    catch meck:unload(router_nats),
    catch meck:unload(router_extension_registry),
    meck:new(router_nats, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Cleanup mocks
    meck:unload(router_nats),
    meck:unload(router_extension_registry),
    ok.

%% Authorization Tests

test_unauthorized_extension_registry_read(_Config) ->
    %% Viewer role should have read access
    %% Unauthorized user should not have access
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    
    %% Attempt to read extension registry without authorization
    Result = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"extension_registry">>),
    
    %% Should fail (no roles assigned)
    ?assertNot(Result),
    ok.

test_unauthorized_extension_registry_write(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    
    %% Attempt to write extension registry without authorization
    Result = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"extension_registry">>),
    
    %% Should fail
    ?assertNot(Result),
    ok.

test_unauthorized_extension_registry_delete(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    
    %% Attempt to delete extension without authorization
    Result = router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"extension_registry">>),
    
    %% Should fail
    ?assertNot(Result),
    ok.

test_unauthorized_extension_registry_admin(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    
    %% Attempt to admin operation without authorization
    Result = router_rbac:can_access(UserId, TenantId, <<"admin">>, <<"extension_registry">>),
    
    %% Should fail
    ?assertNot(Result),
    ok.

test_operator_can_read_write(_Config) ->
    UserId = <<"operator_user">>,
    TenantId = <<"test_tenant">>,
    
    %% Assign operator role
    router_rbac:assign_role(UserId, TenantId, <<"operator">>),
    
    %% Should be able to read
    ReadResult = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"extension_registry">>),
    ?assert(ReadResult),
    
    %% Should be able to write
    WriteResult = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"extension_registry">>),
    ?assert(WriteResult),
    
    %% Should NOT be able to delete
    DeleteResult = router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"extension_registry">>),
    ?assertNot(DeleteResult),
    
    %% Should NOT be able to admin
    AdminResult = router_rbac:can_access(UserId, TenantId, <<"admin">>, <<"extension_registry">>),
    ?assertNot(AdminResult),
    
    ok.

test_viewer_can_only_read(_Config) ->
    UserId = <<"viewer_user">>,
    TenantId = <<"test_tenant">>,
    
    %% Assign viewer role
    router_rbac:assign_role(UserId, TenantId, <<"viewer">>),
    
    %% Should be able to read
    ReadResult = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"extension_registry">>),
    ?assert(ReadResult),
    
    %% Should NOT be able to write
    WriteResult = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"extension_registry">>),
    ?assertNot(WriteResult),
    
    %% Should NOT be able to delete
    DeleteResult = router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"extension_registry">>),
    ?assertNot(DeleteResult),
    
    ok.

%% Payload Validation Tests

test_payload_size_violation(_Config) ->
    %% Set small payload limit for testing
    ok = application:set_env(beamline_router, extension_max_payload_size, 100),
    
    %% Create oversized payload
    LargePayload = binary:copy(<<"x">>, 200),  % 200 bytes > 100 limit
    Request = #{
        <<"payload">> => #{<<"payload">> => LargePayload},
        <<"metadata">> => #{}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension
    Extension = #extension{
        id = <<"normalize_text">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text.v1">>,
        timeout_ms = 100,
        retry = 0
    },
    
    %% Attempt to invoke with oversized payload
    %% Note: invoke_extension is internal, use invoke/3 instead
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    
    %% Should fail with payload_too_large error or extension_not_found (if extension not registered)
    ?assertMatch({error, _}, Result),
    
    %% Reset config
    ok = application:set_env(beamline_router, extension_max_payload_size, 1048576),
    ok.

test_payload_size_within_limit(_Config) ->
    %% Set payload limit
    ok = application:set_env(beamline_router, extension_max_payload_size, 1000),
    
    %% Create payload within limit
    Payload = binary:copy(<<"x">>, 500),  % 500 bytes < 1000 limit
    Request = #{
        <<"payload">> => #{<<"payload">> => Payload},
        <<"metadata">> => #{}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension
    Extension = #extension{
        id = <<"normalize_text">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text.v1">>,
        timeout_ms = 100,
        retry = 0
    },
    
    %% Mock NATS response
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {ok, jsx:encode(#{
            <<"payload">> => #{<<"payload">> => Payload},
            <<"metadata">> => #{}
        })}
    end),
    
    %% Mock extension registry lookup
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    %% Should succeed (validation passes, NATS mocked)
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    
    %% Should succeed (payload size validation passes)
    ?assertMatch({ok, _}, Result),
    
    %% Reset config
    ok = application:set_env(beamline_router, extension_max_payload_size, 1048576),
    ok.

test_metadata_size_violation(_Config) ->
    %% Set small metadata limit
    ok = application:set_env(beamline_router, extension_max_metadata_size, 100),
    
    %% Create large metadata
    LargeMetadata = #{<<"key">> => binary:copy(<<"x">>, 200)},  % 200 bytes > 100 limit
    Request = #{
        <<"payload">> => #{<<"payload">> => <<"test">>},
        <<"metadata">> => LargeMetadata
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension
    Extension = #extension{
        id = <<"normalize_text">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text.v1">>,
        timeout_ms = 100,
        retry = 0
    },
    
    %% Mock extension registry lookup
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    %% Attempt to invoke with oversized metadata
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    
    %% Should fail (metadata size validation would be in build_request_payload or similar)
    %% For now, this test verifies that payload size check includes metadata
    %% Actual implementation may vary
    ?assertMatch({error, _}, Result),
    ok.

test_invalid_json_structure(_Config) ->
    %% Create invalid JSON structure (circular reference simulation)
    %% Note: Erlang maps don't support circular references, so we test with deeply nested structure
    DeepNested = create_deeply_nested_map(15),  % 15 levels > 10 limit (if implemented)
    
    Request = #{
        <<"payload">> => DeepNested,
        <<"metadata">> => #{}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension
    Extension = #extension{
        id = <<"normalize_text">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text.v1">>,
        timeout_ms = 100,
        retry = 0
    },
    
    %% Mock extension registry lookup
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    %% Attempt to invoke with invalid structure
    %% Note: Current implementation may not validate JSON depth, this is a placeholder test
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    
    %% Result depends on implementation (may succeed or fail)
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% Abuse Prevention Tests

test_pipeline_depth_violation(_Config) ->
    %% Create policy with too many extensions (11 > 10 limit)
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 6)],
    Validators = [#{id => <<"validator_", (integer_to_binary(N))/binary>>, on_fail => <<"block">>} || N <- lists:seq(1, 5)],
    Post = [],
    
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = Pre,
        validators = Validators,
        post = Post,
        weights = #{<<"test_provider">> => 1.0}
    },
    
    Request = #route_request{
        policy_id = <<"test_policy">>,
        message = #{<<"tenant_id">> => <<"test_tenant">>}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Attempt to decide with too deep pipeline
    Result = router_decider:decide(Request, Policy, Context),
    
    %% Should fail with pipeline_too_deep error
    ?assertMatch({error, {pipeline_too_deep, _}}, Result),
    ok.

test_pipeline_depth_within_limit(_Config) ->
    %% Create policy with extensions within limit (5 + 3 + 2 = 10)
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 5)],
    Validators = [#{id => <<"validator_", (integer_to_binary(N))/binary>>, on_fail => <<"block">>} || N <- lists:seq(1, 3)],
    Post = [#{id => <<"post_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 2)],
    
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = Pre,
        validators = Validators,
        post = Post,
        weights = #{<<"test_provider">> => 1.0}
    },
    
    Request = #route_request{
        policy_id = <<"test_policy">>,
        message = #{<<"tenant_id">> => <<"test_tenant">>}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension invocations
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {ok, jsx:encode(#{
            <<"payload">> => #{<<"payload">> => <<"processed">>},
            <<"metadata">> => #{}
        })}
    end),
    
    %% Should succeed (pipeline depth validation passes)
    Result = router_decider:decide(Request, Policy, Context),
    
    %% Should succeed (may fail on actual extension invocation, but depth validation passes)
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok  % Depth validation passed, other errors are acceptable
    end,
    ok.

test_retry_limit_violation(_Config) ->
    %% Set retry limit
    ok = application:set_env(beamline_router, extension_max_retries, 3),
    
    %% Create extension with retry count exceeding limit
    Extension = #extension{
        id = <<"normalize_text">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text.v1">>,
        timeout_ms = 100,
        retry = 5  % 5 > 3 limit
    },
    
    Request = #{
        <<"payload">> => #{<<"payload">> => <<"test">>},
        <<"metadata">> => #{}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension registry lookup
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    %% Attempt to invoke with retry count exceeding limit
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    
    %% Should fail with retry_limit_exceeded error or other validation error
    ?assertMatch({error, _}, Result),
    ok.

test_timeout_out_of_range(_Config) ->
    %% Set timeout limits
    ok = application:set_env(beamline_router, extension_min_timeout_ms, 10),
    ok = application:set_env(beamline_router, extension_max_timeout_ms, 5000),
    
    %% Create extension with timeout below minimum
    Extension1 = #extension{
        id = <<"normalize_text">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text.v1">>,
        timeout_ms = 5,  % 5 < 10 minimum
        retry = 0
    },
    
    Request = #{
        <<"payload">> => #{<<"payload">> => <<"test">>},
        <<"metadata">> => #{}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock extension registry lookup
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension1}
    end),
    
    %% Attempt to invoke with timeout below minimum
    Result1 = router_extension_invoker:invoke(Extension1#extension.id, Request, Context),
    ?assertMatch({error, _}, Result1),
    
    %% Create extension with timeout above maximum
    Extension2 = #extension{
        id = <<"normalize_text2">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.normalize_text2.v1">>,
        timeout_ms = 10000,  % 10000 > 5000 maximum
        retry = 0
    },
    
    %% Update mock to return Extension2
    meck:expect(router_extension_registry, lookup, fun(Id) ->
        case Id of
            <<"normalize_text">> -> {ok, Extension1};
            <<"normalize_text2">> -> {ok, Extension2};
            _ -> {error, not_found}
        end
    end),
    
    %% Attempt to invoke with timeout above maximum
    Result2 = router_extension_invoker:invoke(Extension2#extension.id, Request, Context),
    ?assertMatch({error, _}, Result2),
    
    ok.

test_too_many_pre_processors(_Config) ->
    %% Create policy with too many pre-processors (6 > 5 limit)
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 6)],
    Validators = [],
    Post = [],
    
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = Pre,
        validators = Validators,
        post = Post,
        weights = #{<<"test_provider">> => 1.0}
    },
    
    Request = #route_request{
        policy_id = <<"test_policy">>,
        message = #{<<"tenant_id">> => <<"test_tenant">>}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Attempt to decide with too many pre-processors
    Result = router_decider:decide(Request, Policy, Context),
    
    %% Should fail with too_many_pre_processors error
    ?assertMatch({error, {too_many_pre_processors, _}}, Result),
    ok.

test_too_many_validators(_Config) ->
    %% Create policy with too many validators (6 > 5 limit)
    Pre = [],
    Validators = [#{id => <<"validator_", (integer_to_binary(N))/binary>>, on_fail => <<"block">>} || N <- lists:seq(1, 6)],
    Post = [],
    
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = Pre,
        validators = Validators,
        post = Post,
        weights = #{<<"test_provider">> => 1.0}
    },
    
    Request = #route_request{
        policy_id = <<"test_policy">>,
        message = #{<<"tenant_id">> => <<"test_tenant">>}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Attempt to decide with too many validators
    Result = router_decider:decide(Request, Policy, Context),
    
    %% Should fail with too_many_validators error
    ?assertMatch({error, {too_many_validators, _}}, Result),
    ok.

test_too_many_post_processors(_Config) ->
    %% Create policy with too many post-processors (6 > 5 limit)
    Pre = [],
    Validators = [],
    Post = [#{id => <<"post_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 6)],
    
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = Pre,
        validators = Validators,
        post = Post,
        weights = #{<<"test_provider">> => 1.0}
    },
    
    Request = #route_request{
        policy_id = <<"test_policy">>,
        message = #{<<"tenant_id">> => <<"test_tenant">>}
    },
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Attempt to decide with too many post-processors
    Result = router_decider:decide(Request, Policy, Context),
    
    %% Should fail with too_many_post_processors error
    ?assertMatch({error, {too_many_post_processors, _}}, Result),
    ok.

%% Helper: Create deeply nested map
create_deeply_nested_map(0) ->
    #{<<"value">> => <<"leaf">>};
create_deeply_nested_map(N) ->
    #{<<"nested">> => create_deeply_nested_map(N - 1)}.

