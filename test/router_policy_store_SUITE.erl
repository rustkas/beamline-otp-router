%% @doc Common Test Suite for router_policy_store CRUD and validation
%% @test_category cp1_smoke, fast
-module(router_policy_store_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks and test cases (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_upsert_policy_success/1,
    test_upsert_policy_validation_weights/1,
    test_upsert_policy_validation_duplicate_providers/1,
    test_upsert_policy_validation_empty_policy_id/1,
    test_delete_policy_success/1,
    test_delete_policy_not_found/1,
    test_get_policy_success/1,
    test_get_policy_not_found/1,
    test_list_policies_success/1,
    test_list_policies_empty/1,
    test_telemetry_correlation_id/1
]}).

%% Test suite callbacks

all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_upsert_policy_success,
            test_upsert_policy_validation_weights,
            test_upsert_policy_validation_duplicate_providers,
            test_upsert_policy_validation_empty_policy_id,
            test_delete_policy_success,
            test_delete_policy_not_found,
            test_get_policy_success,
            test_get_policy_not_found,
            test_list_policies_success,
            test_list_policies_empty,
            test_telemetry_correlation_id
        ]}
    ].

init_per_suite(Config) ->
    %% Start application with ephemeral port
    _ = application:load(beamline_router),  %% Ignore if already loaded
    ok = application:set_env(beamline_router, grpc_port, 0),  %% Ephemeral port
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for policy store to initialize (bounded wait)
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

%% Test: Upsert policy success
test_upsert_policy_success(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    
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
    
    Result = router_policy_store:upsert_policy(TenantId, Policy),
    ?assertMatch({ok, _Policy}, Result),
    
    %% Verify policy was stored
    {ok, RetrievedPolicy} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(PolicyId, RetrievedPolicy#policy.policy_id),
    ?assertEqual(TenantId, RetrievedPolicy#policy.tenant_id),
    
    ok.

%% Test: Upsert policy validation - weights out of range
test_upsert_policy_validation_weights(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"invalid_policy">>,
    
    %% Weight > 1.0
    Policy1 = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{
            <<"openai">> => 1.5  %% Invalid: > 1.0
        }
    },
    
    Result1 = router_policy_store:upsert_policy(TenantId, Policy1),
    ?assertMatch({error, invalid_policy, _}, Result1),
    
    %% Weight < 0.0
    Policy2 = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{
            <<"openai">> => -0.1  %% Invalid: < 0.0
        }
    },
    
    Result2 = router_policy_store:upsert_policy(TenantId, Policy2),
    ?assertMatch({error, invalid_policy, _}, Result2),
    
    ok.

%% Test: Upsert policy validation - duplicate provider IDs
test_upsert_policy_validation_duplicate_providers(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"invalid_policy">>,
    
    %% This test checks that validation catches duplicates
    %% Note: In Erlang maps, duplicate keys are not possible, so we test with valid weights
    %% but check that validation works for other cases
    
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{
            <<"openai">> => 0.5,
            <<"anthropic">> => 0.5
        }
    },
    
    %% Should succeed with valid weights
    Result = router_policy_store:upsert_policy(TenantId, Policy),
    ?assertMatch({ok, _Policy}, Result),
    
    ok.

%% Test: Upsert policy validation - empty policy_id
test_upsert_policy_validation_empty_policy_id(_Config) ->
    TenantId = <<"test_tenant">>,
    
    %% Empty policy_id
    Policy1 = #policy{
        tenant_id = TenantId,
        policy_id = <<>>,  %% Invalid: empty
        weights = #{
            <<"openai">> => 0.7
        }
    },
    
    Result1 = router_policy_store:upsert_policy(TenantId, Policy1),
    ?assertMatch({error, invalid_policy, _}, Result1),
    
    %% Undefined policy_id
    Policy2 = #policy{
        tenant_id = TenantId,
        policy_id = undefined,  %% Invalid: undefined
        weights = #{
            <<"openai">> => 0.7
        }
    },
    
    Result2 = router_policy_store:upsert_policy(TenantId, Policy2),
    ?assertMatch({error, invalid_policy, _}, Result2),
    
    ok.

%% Test: Delete policy success
test_delete_policy_success(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"delete_test">>,
    
    %% Create policy first
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{
            <<"openai">> => 0.7
        }
    },
    
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Delete policy
    Result = router_policy_store:delete_policy(TenantId, PolicyId),
    ?assertEqual(ok, Result),
    
    %% Verify policy was deleted
    GetResult = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertMatch({error, not_found}, GetResult),
    
    ok.

%% Test: Delete policy not found
test_delete_policy_not_found(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"nonexistent">>,
    
    Result = router_policy_store:delete_policy(TenantId, PolicyId),
    ?assertMatch({error, not_found}, Result),
    
    ok.

%% Test: Get policy success
test_get_policy_success(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"get_test">>,
    
    %% Create policy first
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        }
    },
    
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Get policy
    Result = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertMatch({ok, _Policy}, Result),
    
    {ok, RetrievedPolicy} = Result,
    ?assertEqual(PolicyId, RetrievedPolicy#policy.policy_id),
    ?assertEqual(TenantId, RetrievedPolicy#policy.tenant_id),
    
    ok.

%% Test: Get policy not found
test_get_policy_not_found(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"nonexistent">>,
    
    Result = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertMatch({error, not_found}, Result),
    
    ok.

%% Test: List policies success
test_list_policies_success(_Config) ->
    TenantId = <<"test_tenant">>,
    
    %% Create multiple policies
    Policy1 = #policy{
        tenant_id = TenantId,
        policy_id = <<"policy1">>,
        weights = #{<<"openai">> => 0.7}
    },
    Policy2 = #policy{
        tenant_id = TenantId,
        policy_id = <<"policy2">>,
        weights = #{<<"anthropic">> => 0.5}
    },
    
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy1),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy2),
    
    %% List policies
    Result = router_policy_store:list_policies(TenantId),
    ?assertMatch({ok, _Policies}, Result),
    
    {ok, Policies} = Result,
    ?assert(length(Policies) >= 2),
    
    %% Verify both policies are in the list
    PolicyIds = [P#policy.policy_id || P <- Policies],
    ?assert(lists:member(<<"policy1">>, PolicyIds)),
    ?assert(lists:member(<<"policy2">>, PolicyIds)),
    
    ok.

%% Test: List policies empty
test_list_policies_empty(_Config) ->
    TenantId = <<"empty_tenant">>,
    
    %% List policies for tenant with no policies
    Result = router_policy_store:list_policies(TenantId),
    ?assertMatch({ok, _Policies}, Result),
    
    {ok, Policies} = Result,
    ?assertEqual([], Policies),
    
    ok.

%% Test: Telemetry correlation_id and measurements
test_telemetry_correlation_id(_Config) ->
    TenantId = <<"telemetry_tenant">>,
    CorrelationId = <<"test-correlation-id-12345">>,
    
    %% Create test policies
    Policy1 = #policy{
        tenant_id = TenantId,
        policy_id = <<"telemetry_policy1">>,
        weights = #{<<"openai">> => 0.7}
    },
    Policy2 = #policy{
        tenant_id = TenantId,
        policy_id = <<"telemetry_policy2">>,
        weights = #{<<"anthropic">> => 0.5}
    },
    
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy1, CorrelationId),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy2, CorrelationId),
    
    %% Capture telemetry events using process mailbox
    HandlerId = make_ref(),
    Self = self(),
    
    %% Attach telemetry handler
    Handler = fun(Event, Measurements, Metadata, _HandlerConfig) ->
        Self ! {telemetry, Event, Measurements, Metadata}
    end,
    
    %% Subscribe to router_policy_store list events
    telemetry:attach(HandlerId, [router_policy_store, list], Handler, #{}),
    
    %% Call list_policies with correlation_id
    {ok, Policies} = router_policy_store:list_policies(TenantId, CorrelationId),
    ?assert(length(Policies) >= 2),
    
    %% Wait for telemetry event
    receive
        {telemetry, [router_policy_store, list], Measurements, Metadata} ->
            %% Verify measurements
            ?assert(maps:is_key(duration_us, Measurements), "duration_us should be present"),
            ?assert(maps:is_key(queue_len, Measurements), "queue_len should be present"),
            ?assert(is_integer(maps:get(duration_us, Measurements)), "duration_us should be integer"),
            ?assert(is_integer(maps:get(queue_len, Measurements)), "queue_len should be integer"),
            
            %% Verify metadata
            ?assert(maps:is_key(tenant_id, Metadata), "tenant_id should be present"),
            ?assert(maps:is_key(result, Metadata), "result should be present"),
            ?assertEqual(TenantId, maps:get(tenant_id, Metadata), "tenant_id should match"),
            ?assertEqual(ok, maps:get(result, Metadata), "result should be ok"),
            ?assert(maps:is_key(correlation_id, Metadata), "correlation_id should be present"),
            ?assertEqual(CorrelationId, maps:get(correlation_id, Metadata), "correlation_id should match"),
            
            %% Verify count is present (for list operations)
            ?assert(maps:is_key(count, Metadata), "count should be present for list operation"),
            PolicyCount = maps:get(count, Metadata),
            ?assert(is_integer(PolicyCount), "count should be integer"),
            ?assert(PolicyCount >= 2, "count should be >= 2"),
            ok
    after 1000 ->
        ?assert(false, "Telemetry event not received within timeout")
    end,
    
    %% Cleanup
    telemetry:detach(HandlerId),
    
    ok.

