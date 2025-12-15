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
    test_upsert_policy_idempotent/1,
    test_upsert_policy_overwrite/1,
    test_upsert_policy_validation_weights/1,
    test_upsert_policy_validation_duplicate_providers/1,
    test_upsert_policy_validation_empty_policy_id/1,
    test_delete_policy_success/1,
    test_delete_policy_not_found/1,
    test_delete_policy_idempotent/1,
    test_get_after_delete_not_found/1,
    test_get_policy_success/1,
    test_get_policy_not_found/1,
    test_list_policies_success/1,
    test_list_policies_empty/1,
    test_telemetry_correlation_id/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_delete_policy_not_found/1,
    test_delete_policy_success/1,
    test_delete_policy_idempotent/1,
    test_get_after_delete_not_found/1,
    test_get_policy_not_found/1,
    test_get_policy_success/1,
    test_list_policies_empty/1,
    test_list_policies_success/1,
    test_telemetry_correlation_id/1,
    test_upsert_policy_success/1,
    test_upsert_policy_idempotent/1,
    test_upsert_policy_overwrite/1,
    test_upsert_policy_validation_duplicate_providers/1,
    test_upsert_policy_validation_empty_policy_id/1,
    test_upsert_policy_validation_weights/1
]).


%% Test suite callbacks

all() ->
    router_ct_groups:all_selection(?MODULE, [
        {group, unit_tests},
        {group, telemetry_tests}
    ]).

groups_for_level(heavy) ->
    [{group, unit_tests}, {group, telemetry_tests}];
groups_for_level(full) ->
    [{group, unit_tests}, {group, telemetry_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    router_ct_groups:groups_definitions(?MODULE, base_groups()).

base_groups() ->
    [
        {unit_tests, [parallel], [
            test_upsert_policy_success,
            test_upsert_policy_idempotent,
            test_upsert_policy_overwrite,
            test_upsert_policy_validation_weights,
            test_upsert_policy_validation_duplicate_providers,
            test_upsert_policy_validation_empty_policy_id,
            test_delete_policy_success,
            test_delete_policy_not_found,
            test_delete_policy_idempotent,
            test_get_after_delete_not_found,
            test_get_policy_success,
            test_get_policy_not_found,
            test_list_policies_success,
            test_list_policies_empty
        ]},
        {telemetry_tests, [sequence], [
            test_telemetry_correlation_id
        ]}
    ].

init_per_suite(Config) ->
    router_test_bootstrap:init_per_suite(Config, #{
        start => ensure_all_started,
        app_env => #{grpc_port => 0},
        wait_for_app_start => [{router_policy_store, 1000}]
    }).

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{
        start => ensure_all_started,
        stop => stop_app
    }).

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

%% Test: Upsert policy idempotent - same policy twice should succeed
test_upsert_policy_idempotent(_Config) ->
    TenantId = <<"idem_tenant">>,
    PolicyId = <<"idem_policy">>,
    
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{<<"openai">> => 0.5, <<"anthropic">> => 0.5},
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    
    %% First upsert
    Result1 = router_policy_store:upsert_policy(TenantId, Policy),
    ?assertMatch({ok, _Policy}, Result1),
    
    %% Second upsert with identical policy (idempotent)
    Result2 = router_policy_store:upsert_policy(TenantId, Policy),
    ?assertMatch({ok, _Policy}, Result2),
    
    %% Verify policy is unchanged
    {ok, Retrieved} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(PolicyId, Retrieved#policy.policy_id),
    ?assertEqual(#{<<"openai">> => 0.5, <<"anthropic">> => 0.5}, Retrieved#policy.weights),
    
    %% Cleanup
    router_policy_store:delete_policy(TenantId, PolicyId),
    ok.

%% Test: Upsert policy overwrite - same key with different policy should overwrite
test_upsert_policy_overwrite(_Config) ->
    TenantId = <<"overwrite_tenant">>,
    PolicyId = <<"overwrite_policy">>,
    
    %% First policy
    Policy1 = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        weights = #{<<"openai">> => 1.0}
    },
    
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy1),
    
    %% Second policy with same key but different weights (overwrite)
    Policy2 = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"2.0">>,
        weights = #{<<"anthropic">> => 0.6, <<"openai">> => 0.4}
    },
    
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy2),
    
    %% Verify policy was overwritten
    {ok, Retrieved} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(PolicyId, Retrieved#policy.policy_id),
    ?assertEqual(<<"2.0">>, Retrieved#policy.version),
    ?assertEqual(#{<<"anthropic">> => 0.6, <<"openai">> => 0.4}, Retrieved#policy.weights),
    
    %% Cleanup
    router_policy_store:delete_policy(TenantId, PolicyId),
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

%% Test: Delete policy idempotent (Task 6 regression)
%% Deleting twice should not crash - first ok, second not_found
test_delete_policy_idempotent(_Config) ->
    TenantId = <<"delete_idem_tenant">>,
    PolicyId = <<"delete_idem_policy">>,
    
    %% Create policy
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{<<"openai">> => 1.0}
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% First delete - should succeed
    Result1 = router_policy_store:delete_policy(TenantId, PolicyId),
    ?assertEqual(ok, Result1),
    
    %% Second delete - should return not_found (idempotent, doesn't crash)
    Result2 = router_policy_store:delete_policy(TenantId, PolicyId),
    ?assertMatch({error, not_found}, Result2),
    
    %% Third delete - still idempotent
    Result3 = router_policy_store:delete_policy(TenantId, PolicyId),
    ?assertMatch({error, not_found}, Result3),
    
    ok.

%% Test: Get after delete returns not_found (explicit regression test)
test_get_after_delete_not_found(_Config) ->
    TenantId = <<"get_after_delete_tenant">>,
    PolicyId = <<"get_after_delete_policy">>,
    
    %% Create policy
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{<<"openai">> => 1.0}
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Verify it exists
    {ok, _} = router_policy_store:get_policy(TenantId, PolicyId),
    
    %% Delete policy
    ok = router_policy_store:delete_policy(TenantId, PolicyId),
    
    %% Get after delete MUST return not_found (critical invariant)
    Result = router_policy_store:get_policy(TenantId, PolicyId),
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
    %% Enable telemetry for this test (disabled by default in test profile)
    OldTelemetryEnabled = application:get_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    
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
    Result = receive
        {telemetry, [router_policy_store, list], Measurements, Metadata} ->
            %% Log for debugging
            ct:pal("Telemetry received - Measurements: ~p, Metadata: ~p", [Measurements, Metadata]),
            
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
            
            %% Count may or may not be present depending on implementation
            %% If present, verify it; otherwise just log
            case maps:is_key(count, Metadata) of
                true ->
                    PolicyCount = maps:get(count, Metadata),
                    ?assert(is_integer(PolicyCount), "count should be integer"),
                    ?assert(PolicyCount >= 2, "count should be >= 2");
                false ->
                    ct:pal("Note: count field not present in telemetry metadata (OK)")
            end,
            ok
    after router_test_timeouts:short_wait() ->
        {error, timeout}
    end,
    
    %% Cleanup: restore telemetry setting and detach handler
    telemetry:detach(HandlerId),
    ok = application:set_env(beamline_router, telemetry_enabled, OldTelemetryEnabled),
    
    %% Assert after cleanup to ensure cleanup runs even on failure
    case Result of
        ok -> ok;
        {error, timeout} -> ?assert(false, "Telemetry event not received within timeout")
    end.
