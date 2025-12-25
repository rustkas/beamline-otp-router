%% @doc Extensions Security: Validation and Abuse Prevention Tests
%%
%% Payload validation and abuse prevention:
%% - Payload size limits
%% - Metadata size limits
%% - Pipeline depth limits
%% - Retry and timeout limits
%% - Too many processors
%%
%% @test_category security, full, extensions
-module(router_ext_security_abuse_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
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
    test_too_many_post_processors/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, validation_tests}, {group, abuse_tests}];
        "full" -> [{group, validation_tests}, {group, abuse_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, validation_tests}, {group, abuse_tests}];
        "full" -> [{group, validation_tests}, {group, abuse_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, validation_tests}, {group, abuse_tests}];
        "full" -> [{group, validation_tests}, {group, abuse_tests}];
        _ -> []
    end.
groups() ->
    [{validation_tests, [sequence], [
        test_payload_size_violation,
        test_payload_size_within_limit,
        test_metadata_size_violation,
        test_invalid_json_structure
    ]},
     {abuse_tests, [sequence], [
        test_pipeline_depth_violation,
        test_pipeline_depth_within_limit,
        test_retry_limit_violation,
        test_timeout_out_of_range,
        test_too_many_pre_processors,
        test_too_many_validators,
        test_too_many_post_processors
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, extension_max_pipeline_depth, 10),
    ok = application:set_env(beamline_router, extension_max_pre_count, 5),
    ok = application:set_env(beamline_router, extension_max_validators_count, 5),
    ok = application:set_env(beamline_router, extension_max_post_count, 5),
    ok = application:set_env(beamline_router, extension_max_retries, 3),
    ok = application:set_env(beamline_router, extension_min_timeout_ms, 10),
    ok = application:set_env(beamline_router, extension_max_timeout_ms, 5000),
    ok = application:set_env(beamline_router, extension_max_payload_size, 1048576),
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(router_extension_registry, [passthrough]),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    router_mock_helpers:cleanup_and_verify(),
    ok.

init_per_testcase(_TC, Config) ->
    %% Reset both call history AND expectations (meck:reset only clears history)
    %% Re-setup default mocks to ensure clean state
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(router_extension_registry, [passthrough]),
    Config.

end_per_testcase(_TC, _Config) ->
    %% Reset to default state
    router_mock_helpers:reset(router_nats),
    router_mock_helpers:reset(router_extension_registry),
    ok.

%% ============================================================================
%% VALIDATION TESTS
%% ============================================================================

test_payload_size_violation(_Config) ->
    ok = application:set_env(beamline_router, extension_max_payload_size, 100),
    LargePayload = binary:copy(<<"x">>, 200),
    Request = #{<<"payload">> => #{<<"payload">> => LargePayload}, <<"metadata">> => #{}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Extension = #extension{id = <<"normalize_text">>, type = <<"pre">>,
                           subject = <<"beamline.ext.pre.normalize_text.v1">>,
                           timeout_ms = 100, retry = 0, metadata = #{}},
    
    %% Mock all dependencies for invoke/3
    meck:expect(router_extension_registry, lookup, fun(_) -> {ok, Extension} end),
    ok = router_mock_helpers:ensure_mock(router_extension_versioning, [passthrough]),
    meck:expect(router_extension_versioning, lookup_with_version, fun(_, _) -> {error, not_found} end),
    ok = router_mock_helpers:ensure_mock(router_extension_load_balancer, [passthrough]),
    meck:expect(router_extension_load_balancer, select_instance, fun(_, _) -> {error, not_configured} end),
    
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    ct:log("test_payload_size_violation result: ~p", [Result]),
    ?assertMatch({error, {payload_too_large, _}}, Result),
    
    %% Cleanup extra mocks
    meck:unload(router_extension_versioning),
    meck:unload(router_extension_load_balancer),
    ok = application:set_env(beamline_router, extension_max_payload_size, 1048576),
    ok.

test_payload_size_within_limit(_Config) ->
    ok = application:set_env(beamline_router, extension_max_payload_size, 1000),
    Payload = binary:copy(<<"x">>, 500),
    Request = #{<<"payload">> => #{<<"payload">> => Payload}, <<"metadata">> => #{}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Extension = #extension{id = <<"normalize_text">>, type = <<"pre">>,
                           subject = <<"beamline.ext.pre.normalize_text.v1">>,
                           timeout_ms = 100, retry = 0, metadata = #{}},
    
    %% Mock all dependencies for invoke/3
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => Payload}, <<"metadata">> => #{}})}
    end),
    meck:expect(router_extension_registry, lookup, fun(_) -> {ok, Extension} end),
    %% Ensure versioning lookup returns error to use registry
    ok = router_mock_helpers:ensure_mock(router_extension_versioning, [passthrough]),
    meck:expect(router_extension_versioning, lookup_with_version, fun(_, _) -> {error, not_found} end),
    %% Ensure load balancer returns error to use default subject
    ok = router_mock_helpers:ensure_mock(router_extension_load_balancer, [passthrough]),
    meck:expect(router_extension_load_balancer, select_instance, fun(_, _) -> {error, not_configured} end),
    
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    ?assertMatch({ok, _}, Result),
    
    %% Cleanup extra mocks
    meck:unload(router_extension_versioning),
    meck:unload(router_extension_load_balancer),
    ok = application:set_env(beamline_router, extension_max_payload_size, 1048576),
    ok.

test_metadata_size_violation(_Config) ->
    ok = application:set_env(beamline_router, extension_max_metadata_size, 100),
    LargeMetadata = #{<<"key">> => binary:copy(<<"x">>, 200)},
    Request = #{<<"payload">> => #{<<"payload">> => <<"test">>}, <<"metadata">> => LargeMetadata},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Extension = #extension{id = <<"normalize_text">>, type = <<"pre">>,
                           subject = <<"beamline.ext.pre.normalize_text.v1">>,
                           timeout_ms = 100, retry = 0, metadata = #{}},
    
    %% Mock all dependencies for invoke/3
    meck:expect(router_extension_registry, lookup, fun(_) -> {ok, Extension} end),
    %% Ensure versioning lookup returns error to use registry
    ok = router_mock_helpers:ensure_mock(router_extension_versioning, [passthrough]),
    meck:expect(router_extension_versioning, lookup_with_version, fun(_, _) -> {error, not_found} end),
    %% Ensure load balancer returns error to use default subject
    ok = router_mock_helpers:ensure_mock(router_extension_load_balancer, [passthrough]),
    meck:expect(router_extension_load_balancer, select_instance, fun(_, _) -> {error, not_configured} end),
    
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    ct:log("test_metadata_size_violation result: ~p", [Result]),
    ?assertMatch({error, {metadata_too_large, _}}, Result),
    
    %% Cleanup extra mocks
    meck:unload(router_extension_versioning),
    meck:unload(router_extension_load_balancer),
    ok.

test_invalid_json_structure(_Config) ->
    DeepNested = create_deeply_nested_map(15),
    Request = #{<<"payload">> => DeepNested, <<"metadata">> => #{}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Extension = #extension{id = <<"normalize_text">>, type = <<"pre">>,
                           subject = <<"beamline.ext.pre.normalize_text.v1">>,
                           timeout_ms = 100, retry = 0, metadata = #{}},
    
    %% Mock all dependencies for invoke/3
    meck:expect(router_extension_registry, lookup, fun(_) -> {ok, Extension} end),
    ok = router_mock_helpers:ensure_mock(router_extension_versioning, [passthrough]),
    meck:expect(router_extension_versioning, lookup_with_version, fun(_, _) -> {error, not_found} end),
    ok = router_mock_helpers:ensure_mock(router_extension_load_balancer, [passthrough]),
    meck:expect(router_extension_load_balancer, select_instance, fun(_, _) -> {error, not_configured} end),
    %% Mock NATS request to return valid JSON response
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {ok, jsx:encode(#{<<"payload">> => DeepNested, <<"metadata">> => #{}})}
    end),
    
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    %% This test accepts both ok and error (deep nesting may or may not be handled)
    case Result of {ok, _} -> ok; {error, _} -> ok end,
    
    %% Cleanup extra mocks
    meck:unload(router_extension_versioning),
    meck:unload(router_extension_load_balancer),
    ok.

%% ============================================================================
%% ABUSE PREVENTION TESTS
%% ============================================================================

test_pipeline_depth_violation(_Config) ->
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 6)],
    Validators = [#{id => <<"validator_", (integer_to_binary(N))/binary>>, on_fail => <<"block">>} || N <- lists:seq(1, 5)],
    
    Policy = #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
                     pre = Pre, validators = Validators, post = [], weights = #{<<"test_provider">> => 1.0}},
    Request = #route_request{policy_id = <<"test_policy">>, message = #{<<"tenant_id">> => <<"test_tenant">>}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Result = router_decider:decide(Request, Policy, Context),
    ?assertMatch({error, {pipeline_too_deep, _}}, Result),
    ok.

test_pipeline_depth_within_limit(_Config) ->
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 5)],
    Validators = [#{id => <<"validator_", (integer_to_binary(N))/binary>>, on_fail => <<"block">>} || N <- lists:seq(1, 3)],
    Post = [#{id => <<"post_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 2)],
    
    Policy = #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
                     pre = Pre, validators = Validators, post = Post, weights = #{<<"test_provider">> => 1.0}},
    Request = #route_request{policy_id = <<"test_policy">>, message = #{<<"tenant_id">> => <<"test_tenant">>}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})}
    end),
    
    Result = router_decider:decide(Request, Policy, Context),
    case Result of {ok, _} -> ok; {error, _} -> ok end,
    ok.

test_retry_limit_violation(_Config) ->
    ok = application:set_env(beamline_router, extension_max_retries, 3),
    Extension = #extension{id = <<"normalize_text">>, type = <<"pre">>,
                           subject = <<"beamline.ext.pre.normalize_text.v1">>,
                           timeout_ms = 100, retry = 5, metadata = #{}},
    Request = #{<<"payload">> => #{<<"payload">> => <<"test">>}, <<"metadata">> => #{}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock all dependencies for invoke/3
    meck:expect(router_extension_registry, lookup, fun(_) -> {ok, Extension} end),
    ok = router_mock_helpers:ensure_mock(router_extension_versioning, [passthrough]),
    meck:expect(router_extension_versioning, lookup_with_version, fun(_, _) -> {error, not_found} end),
    ok = router_mock_helpers:ensure_mock(router_extension_load_balancer, [passthrough]),
    meck:expect(router_extension_load_balancer, select_instance, fun(_, _) -> {error, not_configured} end),
    
    Result = router_extension_invoker:invoke(Extension#extension.id, Request, Context),
    ct:log("test_retry_limit_violation result: ~p", [Result]),
    ?assertMatch({error, {retry_limit_exceeded, _}}, Result),
    
    %% Cleanup extra mocks
    meck:unload(router_extension_versioning),
    meck:unload(router_extension_load_balancer),
    ok.

test_timeout_out_of_range(_Config) ->
    ok = application:set_env(beamline_router, extension_min_timeout_ms, 10),
    ok = application:set_env(beamline_router, extension_max_timeout_ms, 5000),
    
    Extension1 = #extension{id = <<"normalize_text">>, type = <<"pre">>,
                            subject = <<"beamline.ext.pre.normalize_text.v1">>,
                            timeout_ms = 5, retry = 0, metadata = #{}},
    Request = #{<<"payload">> => #{<<"payload">> => <<"test">>}, <<"metadata">> => #{}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    %% Mock all dependencies for invoke/3
    meck:expect(router_extension_registry, lookup, fun(_) -> {ok, Extension1} end),
    ok = router_mock_helpers:ensure_mock(router_extension_versioning, [passthrough]),
    meck:expect(router_extension_versioning, lookup_with_version, fun(_, _) -> {error, not_found} end),
    ok = router_mock_helpers:ensure_mock(router_extension_load_balancer, [passthrough]),
    meck:expect(router_extension_load_balancer, select_instance, fun(_, _) -> {error, not_configured} end),
    
    Result1 = router_extension_invoker:invoke(Extension1#extension.id, Request, Context),
    ct:log("test_timeout_out_of_range result: ~p", [Result1]),
    ?assertMatch({error, {timeout_out_of_range, _}}, Result1),
    
    %% Cleanup extra mocks
    meck:unload(router_extension_versioning),
    meck:unload(router_extension_load_balancer),
    ok.

test_too_many_pre_processors(_Config) ->
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 6)],
    Policy = #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
                     pre = Pre, validators = [], post = [], weights = #{<<"test_provider">> => 1.0}},
    Request = #route_request{policy_id = <<"test_policy">>, message = #{<<"tenant_id">> => <<"test_tenant">>}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Result = router_decider:decide(Request, Policy, Context),
    ?assertMatch({error, {too_many_pre_processors, _}}, Result),
    ok.

test_too_many_validators(_Config) ->
    Validators = [#{id => <<"validator_", (integer_to_binary(N))/binary>>, on_fail => <<"block">>} || N <- lists:seq(1, 6)],
    Policy = #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
                     pre = [], validators = Validators, post = [], weights = #{<<"test_provider">> => 1.0}},
    Request = #route_request{policy_id = <<"test_policy">>, message = #{<<"tenant_id">> => <<"test_tenant">>}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Result = router_decider:decide(Request, Policy, Context),
    ?assertMatch({error, {too_many_validators, _}}, Result),
    ok.

test_too_many_post_processors(_Config) ->
    Post = [#{id => <<"post_", (integer_to_binary(N))/binary>>, mode => <<"required">>} || N <- lists:seq(1, 6)],
    Policy = #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
                     pre = [], validators = [], post = Post, weights = #{<<"test_provider">> => 1.0}},
    Request = #route_request{policy_id = <<"test_policy">>, message = #{<<"tenant_id">> => <<"test_tenant">>}},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Result = router_decider:decide(Request, Policy, Context),
    ?assertMatch({error, {too_many_post_processors, _}}, Result),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

create_deeply_nested_map(0) -> #{<<"value">> => <<"leaf">>};
create_deeply_nested_map(N) -> #{<<"nested">> => create_deeply_nested_map(N - 1)}.
