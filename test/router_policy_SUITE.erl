%% @doc Common Test Suite for Router Policy
%% Tests policy loading, parsing, and basic operations
%% @test_category cp1_smoke, fast
-module(router_policy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

-define(TENANT_ID, <<"test_tenant">>).
-define(POLICY_ID, <<"test_policy">>).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_load_policy_success/1,
    test_load_policy_not_found/1,
    test_parse_policy_json_valid/1,
    test_parse_policy_json_invalid/1,
    test_parse_policy_json_map/1
]}).

%% Test suite configuration
all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_load_policy_success,
            test_load_policy_not_found,
            test_parse_policy_json_valid,
            test_parse_policy_json_invalid,
            test_parse_policy_json_map
        ]}
    ].

init_per_suite(Config) ->
    %% Start application with ephemeral port
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for policy store to initialize
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

%% Test: Load policy successfully
test_load_policy_success(_Config) ->
    TenantId = ?TENANT_ID,
    PolicyId = ?POLICY_ID,
    
    %% Create and store a test policy
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    
    %% Store policy
    {ok, _StoredPolicy} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Load policy using router_policy:load_policy/2
    {ok, LoadedPolicy} = router_policy:load_policy(TenantId, PolicyId),
    
    %% Verify loaded policy matches stored policy
    ?assertEqual(PolicyId, LoadedPolicy#policy.policy_id),
    ?assertEqual(TenantId, LoadedPolicy#policy.tenant_id),
    ?assertEqual(0.7, maps:get(<<"openai">>, LoadedPolicy#policy.weights)),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, LoadedPolicy#policy.weights)),
    
    ok.

%% Test: Load non-existent policy
test_load_policy_not_found(_Config) ->
    TenantId = ?TENANT_ID,
    NonExistentPolicyId = <<"non_existent_policy">>,
    
    %% Try to load non-existent policy
    {error, not_found} = router_policy:load_policy(TenantId, NonExistentPolicyId),
    
    ok.

%% Test: Parse valid JSON policy
test_parse_policy_json_valid(_Config) ->
    ValidJson = jsx:encode(#{
        <<"version">> => <<"1.0">>,
        <<"weights">> => #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        }
    }),
    
    {ok, ParsedPolicy} = router_policy:parse_policy_json(ValidJson),
    ?assertEqual(<<"1.0">>, maps:get(<<"version">>, ParsedPolicy)),
    ?assertEqual(0.7, maps:get(<<"openai">>, maps:get(<<"weights">>, ParsedPolicy))),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, maps:get(<<"weights">>, ParsedPolicy))),
    
    ok.

%% Test: Parse invalid JSON policy
test_parse_policy_json_invalid(_Config) ->
    InvalidJson = <<"{invalid json}">>,
    
    %% Should return error for invalid JSON
    Result = router_policy:parse_policy_json(InvalidJson),
    ?assertMatch({error, _}, Result),
    
    %% Test with non-JSON binary - jsx:decode returns {error, _} which becomes {error, {invalid_json, _}}
    Result2 = router_policy:parse_policy_json(<<"not json">>),
    ?assertMatch({error, {invalid_json, _}}, Result2),
    
    ok.

%% Test: Parse policy JSON that is already a map
test_parse_policy_json_map(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"weights">> => #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        }
    },
    
    {ok, ParsedPolicy} = router_policy:parse_policy_json(PolicyMap),
    ?assertEqual(PolicyMap, ParsedPolicy),
    
    ok.
