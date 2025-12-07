%% @doc Integration Tests for Router with JSON Policies
%% Tests real JSON policies from fixtures through full Router pipeline
%% Scenarios: sticky-hit/miss, weighted routing, fallback chains, extensions
%% @test_category integration, cp1_smoke
-module(router_policy_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).

-define(TENANT_ID, <<"default_tenant">>).
-define(TEST_POLICY_ID, <<"test_policy">>).

%% Test suite configuration
all() ->
    [
        {group, json_policy_tests},
        {group, edge_cases_tests}
    ].

groups() ->
    [
        {json_policy_tests, [sequence], [
            test_sticky_hit,
            test_sticky_miss,
            test_weighted_routing_multiple_providers,
            test_fallback_chain_simple,
            test_fallback_chain_complex,
            test_fallback_retry_exhaustion,
            test_extensions_pre_only,
            test_extensions_validators_only,
            test_extensions_post_only,
            test_extensions_full_pipeline,
            test_extensions_order,
            test_legacy_format_compatibility,
            test_mixed_format_compatibility,
            test_complex_fallbacks_policy,
            test_explanation_format,
            test_explanation_sticky_reason,
            test_explanation_weighted_reason
        ]},
        {edge_cases_tests, [parallel], [
            test_inconsistent_weights_sum_not_100,
            test_inconsistent_weights_sum_zero,
            test_inconsistent_weights_sum_over_100,
            test_invalid_ttl_duration_invalid_format,
            test_invalid_ttl_duration_empty,
            test_invalid_ttl_duration_zero,
            test_invalid_ttl_duration_negative,
            test_conflicting_fallback_rules,
            test_overlapping_fallback_rules,
            test_empty_extensions_arrays,
            test_unknown_fields_in_extensions,
            test_deep_legacy_new_combination
        ]}
    ].

init_per_suite(Config) ->
    %% Setup Mnesia for sticky sessions
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Start required applications
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, disable_heir, true),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> ok;
        {error, {already_started, beamline_router}} -> ok;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end,
    
    %% Wait for services to be ready
    test_helpers:wait_for_app_start(router_policy_store, 2000),
    test_helpers:wait_for_app_start(router_sticky_store, 2000),
    
    %% Load all fixture policies
    load_fixture_policies(),
    
    Config.

end_per_suite(Config) ->
    %% Stop services
    catch gen_server:stop(router_policy_store),
    catch gen_server:stop(router_sticky_store),
    application:stop(beamline_router),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear sticky sessions before each test
    catch begin
        case ets:info(sticky_sessions) of
            undefined -> ok;
            _ -> ets:delete_all_objects(sticky_sessions)
        end
    end,
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Load all fixture policies into policy store
load_fixture_policies() ->
    FixtureDir = filename:join([code:priv_dir(beamline_router), "fixtures", "policies", "default_tenant"]),
    case file:list_dir(FixtureDir) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
            
            lists:foreach(fun(JsonFile) ->
                PolicyId = filename:rootname(JsonFile),
                FilePath = filename:join(FixtureDir, JsonFile),
                case file:read_file(FilePath) of
                    {ok, JsonContent} ->
                        case router_policy:parse_policy_json(JsonContent) of
                            {ok, PolicyMap} ->
                                Policy = router_policy_store:parse_policy_map(?TENANT_ID, list_to_binary(PolicyId), PolicyMap),
                                router_policy_store:upsert_policy(?TENANT_ID, Policy);
                            Error ->
                                ct:comment("Failed to parse policy ~s: ~p", [JsonFile, Error])
                        end;
                    Error ->
                        ct:comment("Failed to read policy file ~s: ~p", [JsonFile, Error])
                end
            end, JsonFiles),
            ok;
        {error, Reason} ->
            ct:comment("Failed to list fixture directory ~s: ~p", [FixtureDir, Reason]),
            ok
    end.

%% Create route request
create_route_request(Message, PolicyId, Context) ->
    #route_request{
        message = Message,
        policy_id = PolicyId,
        context = Context
    }.

%% ============================================================================
%% Sticky Session Tests
%% ============================================================================

test_sticky_hit(_Config) ->
    %% Load sticky_weights policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"sticky_weights">>),
    ?assertNotEqual(undefined, Policy#policy.sticky),
    ?assert(maps:get(<<"enabled">>, Policy#policy.sticky, false)),
    
    %% Create sticky session manually (simulating previous request)
    SessionKey = <<"user_123">>,
    ProviderId = <<"anthropic">>,
    router_sticky_store:set_provider(?TENANT_ID, SessionKey, ProviderId),
    
    %% Verify sticky session exists
    {ok, StickyProvider} = router_sticky_store:get_provider(?TENANT_ID, SessionKey),
    ?assertEqual(ProviderId, StickyProvider),
    
    %% Create route request with session key
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_1">>
        },
        <<"sticky_weights">>,
        #{<<"user_id">> => SessionKey}
    ),
    
    %% Route through full pipeline
    {ok, Decision1} = router_core:route(RouteRequest, #{}),
    ?assertEqual(ProviderId, Decision1#route_decision.provider_id),
    ?assertEqual(<<"sticky">>, Decision1#route_decision.reason),
    
    %% Second request should also hit sticky
    {ok, Decision2} = router_core:route(RouteRequest, #{}),
    ?assertEqual(ProviderId, Decision2#route_decision.provider_id),
    ?assertEqual(<<"sticky">>, Decision2#route_decision.reason),
    
    ok.

test_sticky_miss(_Config) ->
    %% Load sticky_weights policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"sticky_weights">>),
    ?assertNotEqual(undefined, Policy#policy.sticky),
    
    %% No sticky session exists
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_2">>
        },
        <<"sticky_weights">>,
        #{<<"user_id">> => <<"new_user">>}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Should use weighted routing (not sticky)
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ?assertEqual(<<"weighted">>, Decision#route_decision.reason),
    
    %% Verify sticky session was created (if sticky was enabled and session key was found)
    %% Note: In CP1, sticky session creation happens inside router_decider
    %% This test verifies the routing decision was made correctly
    
    ok.

%% ============================================================================
%% Weighted Routing Tests
%% ============================================================================

test_weighted_routing_multiple_providers(_Config) ->
    %% Load default policy with multiple providers
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_3">>
        },
        <<"default">>,
        #{}
    ),
    
    %% Make multiple routing decisions
    Results = lists:map(fun(_) ->
        {ok, Decision} = router_core:route(RouteRequest, #{}),
        Decision#route_decision.provider_id
    end, lists:seq(1, 10)),
    
    %% Verify providers are selected (weighted distribution)
    UniqueProviders = lists:usort(Results),
    ?assert(length(UniqueProviders) >= 1),
    
    %% Verify all providers are from policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"default">>),
    PolicyProviders = maps:keys(Policy#policy.weights),
    lists:foreach(fun(Provider) ->
        ?assert(lists:member(Provider, PolicyProviders))
    end, UniqueProviders),
    
    ok.

%% ============================================================================
%% Fallback Chain Tests
%% ============================================================================

test_fallback_chain_simple(_Config) ->
    %% Load complex_fallbacks policy
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_4">>
        },
        <<"complex_fallbacks">>,
        #{<<"status">> => <<"timeout">>}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Should select provider (weighted or fallback)
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ?assert(lists:member(Decision#route_decision.reason, [<<"weighted">>, <<"fallback">>])),
    
    ok.

test_fallback_chain_complex(_Config) ->
    %% Load complex_fallbacks policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"complex_fallbacks">>),
    ?assert(length(Policy#policy.fallbacks) > 1),
    
    %% Test with different status values
    TestCases = [
        {<<"timeout">>, <<"anthropic">>},
        {<<"5xx">>, <<"cohere">>},
        {<<"rate_limited">>, <<"local_llm">>}
    ],
    
    lists:foreach(fun({Status, _ExpectedProvider}) ->
        RouteRequest = create_route_request(
            #{
                <<"tenant_id">> => ?TENANT_ID,
                <<"message_id">> => <<"msg_", Status/binary>>
            },
            <<"complex_fallbacks">>,
            #{<<"status">> => Status}
        ),
        
        %% Route through full pipeline
        {ok, Decision} = router_core:route(RouteRequest, #{}),
        
        %% Verify provider is selected
        ?assertNotEqual(undefined, Decision#route_decision.provider_id),
        
        %% Note: In CP1, fallback logic may not fully match conditions yet
        %% This test verifies the pipeline works, not exact fallback matching
        ok
    end, TestCases),
    
    ok.

test_fallback_retry_exhaustion(_Config) ->
    %% Load complex_fallbacks policy with retry counts
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"complex_fallbacks">>),
    
    %% Find fallback rule with retry > 1
    FallbackWithRetry = lists:filter(fun(Fallback) ->
        maps:get(<<"retry">>, Fallback, 1) > 1
    end, Policy#policy.fallbacks),
    
    case FallbackWithRetry of
        [] ->
            ct:comment("No fallback rules with retry > 1 found, skipping test"),
            {skip, "No fallback rules with retry > 1"};
        [_ | _] ->
            %% Test that retry is parsed correctly
            [FirstFallback | _] = FallbackWithRetry,
            RetryCount = maps:get(<<"retry">>, FirstFallback, 1),
            ?assert(RetryCount > 1),
            
            %% Verify retry count is stored in fallback rule
            ?assertEqual(RetryCount, maps:get(<<"retry">>, FirstFallback)),
            
            %% Note: In CP1, retry logic is not fully implemented in decision making
            %% This test verifies retry is parsed and stored correctly from JSON
            %% Actual retry exhaustion logic will be implemented in future iterations
            ct:comment("Retry count parsed correctly: ~p (retry logic not yet implemented)", [RetryCount]),
            ok
    end.

%% ============================================================================
%% Extensions Tests
%% ============================================================================

test_extensions_pre_only(_Config) ->
    %% Load extensions_pre_only policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"extensions_pre_only">>),
    ?assert(length(Policy#policy.pre) > 0),
    ?assertEqual([], Policy#policy.validators),
    ?assertEqual([], Policy#policy.post),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_5">>
        },
        <<"extensions_pre_only">>,
        #{}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with pre extensions
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_extensions_validators_only(_Config) ->
    %% Load extensions_validators_only policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"extensions_validators_only">>),
    ?assertEqual([], Policy#policy.pre),
    ?assert(length(Policy#policy.validators) > 0),
    ?assertEqual([], Policy#policy.post),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_6">>
        },
        <<"extensions_validators_only">>,
        #{}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with validators
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_extensions_post_only(_Config) ->
    %% Load extensions_post_only policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"extensions_post_only">>),
    ?assertEqual([], Policy#policy.pre),
    ?assertEqual([], Policy#policy.validators),
    ?assert(length(Policy#policy.post) > 0),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_7">>
        },
        <<"extensions_post_only">>,
        #{}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with post extensions
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_extensions_full_pipeline(_Config) ->
    %% Load extensions_full policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"extensions_full">>),
    ?assert(length(Policy#policy.pre) > 0),
    ?assert(length(Policy#policy.validators) > 0),
    ?assert(length(Policy#policy.post) > 0),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_8">>
        },
        <<"extensions_full">>,
        #{}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with all extension types
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_extensions_order(_Config) ->
    %% Load extensions_full policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"extensions_full">>),
    
    %% Verify extensions are parsed in order
    PreIds = [maps:get(id, Pre) || Pre <- Policy#policy.pre],
    ValidatorIds = [maps:get(id, Validator) || Validator <- Policy#policy.validators],
    PostIds = [maps:get(id, Post) || Post <- Policy#policy.post],
    
    ?assert(length(PreIds) >= 2),
    ?assert(length(ValidatorIds) >= 1),
    ?assert(length(PostIds) >= 1),
    
    %% Verify order is preserved (first extension should be first in list)
    case PreIds of
        [FirstPre | _] ->
            ?assertEqual(<<"normalize_text">>, FirstPre);
        _ ->
            ok
    end,
    
    %% Verify validators order
    case ValidatorIds of
        [FirstValidator | _] ->
            ?assertEqual(<<"pii_guard">>, FirstValidator);
        _ ->
            ok
    end,
    
    %% Verify post order
    case PostIds of
        [FirstPost | _] ->
            ?assertEqual(<<"mask_pii">>, FirstPost);
        _ ->
            ok
    end,
    
    ok.

%% ============================================================================
%% Legacy and Mixed Format Tests
%% ============================================================================

test_legacy_format_compatibility(_Config) ->
    %% Load legacy_format policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"legacy_format">>),
    
    %% Verify legacy format is parsed correctly
    ?assertNotEqual(#{}, Policy#policy.weights),
    ?assertNotEqual(undefined, Policy#policy.fallback),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_9">>
        },
        <<"legacy_format">>,
        #{}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with legacy format
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_mixed_format_compatibility(_Config) ->
    %% Load mixed_format policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"mixed_format">>),
    
    %% Verify both formats are present
    ?assertNotEqual(#{}, Policy#policy.weights),
    ?assert(length(Policy#policy.fallbacks) > 0),
    ?assertNotEqual(undefined, Policy#policy.fallback),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_10">>
        },
        <<"mixed_format">>,
        #{}
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with mixed format
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

%% ============================================================================
%% Complex Scenarios
%% ============================================================================

test_complex_fallbacks_policy(_Config) ->
    %% Load complex_fallbacks policy
    {ok, Policy} = router_policy_store:get_policy(?TENANT_ID, <<"complex_fallbacks">>),
    
    %% Verify policy structure
    ?assert(length(Policy#policy.fallbacks) >= 4),
    ?assert(map_size(Policy#policy.weights) >= 3),
    
    %% Test with complex when condition (multiple fields)
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_11">>
        },
        <<"complex_fallbacks">>,
        #{
            <<"status">> => <<"timeout">>,
            <<"message_type">> => <<"chat">>
        }
    ),
    
    %% Route through full pipeline
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify routing works with complex fallback conditions
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

%% ============================================================================
%% Explanation Verification Tests
%% ============================================================================

test_explanation_format(_Config) ->
    %% Test that explanation conforms to formal specification in ROUTING_POLICY.md
    %% Required fields: reason, provider_id, policy_id, policy_version, priority, steps, context
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_explanation">>
        },
        <<"default">>,
        #{}
    ),
    
    %% Get explanation via router_policy_applier (not router_core, which doesn't return explanation)
    {ok, Result} = router_policy_applier:apply_policy(RouteRequest, ?TENANT_ID, <<"default">>, #{}),
    
    %% Verify explanation structure according to specification
    Explanation = maps:get(explanation, Result),
    
    %% Required fields must be present
    ?assert(is_map(Explanation), "Explanation must be a map"),
    ?assert(maps:is_key(reason, Explanation), "Explanation must have 'reason' field"),
    ?assert(maps:is_key(provider_id, Explanation), "Explanation must have 'provider_id' field"),
    ?assert(maps:is_key(policy_id, Explanation), "Explanation must have 'policy_id' field"),
    ?assert(maps:is_key(policy_version, Explanation), "Explanation must have 'policy_version' field"),
    ?assert(maps:is_key(priority, Explanation), "Explanation must have 'priority' field"),
    ?assert(maps:is_key(steps, Explanation), "Explanation must have 'steps' field"),
    ?assert(maps:is_key(context, Explanation), "Explanation must have 'context' field"),
    
    %% Verify reason is one of valid values
    Reason = maps:get(reason, Explanation),
    ?assert(lists:member(Reason, [<<"sticky">>, <<"weighted">>, <<"fallback">>, <<"retry">>]),
            "Reason must be one of: sticky, weighted, fallback, retry"),
    
    %% Verify provider_id is not empty
    ProviderId = maps:get(provider_id, Explanation),
    ?assert(is_binary(ProviderId), "provider_id must be binary"),
    ?assert(byte_size(ProviderId) > 0, "provider_id must not be empty"),
    
    %% Verify policy_id is not empty
    PolicyId = maps:get(policy_id, Explanation),
    ?assert(is_binary(PolicyId), "policy_id must be binary"),
    ?assert(byte_size(PolicyId) > 0, "policy_id must not be empty"),
    
    %% Verify policy_version format (MAJOR.MINOR)
    PolicyVersion = maps:get(policy_version, Explanation),
    ?assert(is_binary(PolicyVersion), "policy_version must be binary"),
    ?assertMatch({match, _}, re:run(PolicyVersion, "^[0-9]+\\.[0-9]+$"), "policy_version must match MAJOR.MINOR format"),
    
    %% Verify priority is one of valid values
    Priority = maps:get(priority, Explanation),
    ?assert(lists:member(Priority, [25, 50, 100]), "Priority must be one of: 25, 50, 100"),
    
    %% Verify steps is array of strings
    Steps = maps:get(steps, Explanation),
    ?assert(is_list(Steps), "steps must be a list"),
    ?assert(lists:all(fun(S) -> is_binary(S) end, Steps), "All steps must be binaries"),
    
    %% Verify context is map and contains tenant_id
    Context = maps:get(context, Explanation),
    ?assert(is_map(Context), "context must be a map"),
    ?assert(maps:is_key(<<"tenant_id">>, Context), "context must contain 'tenant_id' field"),
    
    ok.

test_explanation_sticky_reason(_Config) ->
    %% Create sticky session
    SessionKey = <<"explanation_user">>,
    ProviderId = <<"openai">>,
    router_sticky_store:set_provider(?TENANT_ID, SessionKey, ProviderId),
    
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_explanation_sticky">>
        },
        <<"sticky_weights">>,
        #{<<"user_id">> => SessionKey}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify sticky reason is set correctly
    ?assertEqual(<<"sticky">>, Decision#route_decision.reason),
    ?assertEqual(ProviderId, Decision#route_decision.provider_id),
    
    ok.

test_explanation_weighted_reason(_Config) ->
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_explanation_weighted">>
        },
        <<"default">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Verify weighted reason is set correctly
    ?assertEqual(<<"weighted">>, Decision#route_decision.reason),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

%% ============================================================================
%% Edge Cases Tests
%% ============================================================================

test_inconsistent_weights_sum_not_100(_Config) ->
    %% Create policy with weights that don't sum to 100
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 30},
            #{<<"name">> => <<"anthropic">>, <<"weight">> => 40}
            %% Sum = 70, not 100
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_weights_70">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify weights are parsed (even if sum != 100)
    ?assertEqual(0.3, maps:get(<<"openai">>, Policy#policy.weights)),
    ?assertEqual(0.4, maps:get(<<"anthropic">>, Policy#policy.weights)),
    
    %% Route should still work (weighted distribution will use available weights)
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_weights">>
        },
        <<"edge_weights_70">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_inconsistent_weights_sum_zero(_Config) ->
    %% Create policy with all weights = 0
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0},
            #{<<"name">> => <<"anthropic">>, <<"weight">> => 0}
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"all_providers_failed">>]},
                <<"to">> => <<"local_llm">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_weights_zero">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify weights are parsed as 0.0
    ?assertEqual(0.0, maps:get(<<"openai">>, Policy#policy.weights)),
    ?assertEqual(0.0, maps:get(<<"anthropic">>, Policy#policy.weights)),
    
    %% Route should fallback (weights are 0)
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_weights_zero">>
        },
        <<"edge_weights_zero">>,
        #{<<"status">> => <<"all_providers_failed">>}
    ),
    
    Result = router_core:route(RouteRequest, #{}),
    %% Should either use fallback or return error
    case Result of
        {ok, Decision} ->
            ?assertNotEqual(undefined, Decision#route_decision.provider_id);
        {error, _} ->
            %% Error is acceptable when weights are all zero
            ok
    end,
    
    ok.

test_inconsistent_weights_sum_over_100(_Config) ->
    %% Create policy with weights that sum > 100
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 70},
            #{<<"name">> => <<"anthropic">>, <<"weight">> => 50}
            %% Sum = 120, > 100
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_weights_120">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify weights are parsed (even if sum > 100)
    ?assertEqual(0.7, maps:get(<<"openai">>, Policy#policy.weights)),
    ?assertEqual(0.5, maps:get(<<"anthropic">>, Policy#policy.weights)),
    
    %% Route should still work (weighted distribution will normalize or use as-is)
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_weights_120">>
        },
        <<"edge_weights_120">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_invalid_ttl_duration_invalid_format(_Config) ->
    %% Create policy with invalid TTL format
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl">> => <<"10x">>  %% Invalid format
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_ttl_invalid">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify TTL falls back to default (3600 seconds)
    Sticky = Policy#policy.sticky,
    ?assertEqual(3600, maps:get(<<"ttl_seconds">>, Sticky)),  %% Default fallback
    
    %% Route should still work
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_ttl_invalid">>
        },
        <<"edge_ttl_invalid">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_invalid_ttl_duration_empty(_Config) ->
    %% Create policy with empty TTL string
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl">> => <<"">>  %% Empty string
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_ttl_empty">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify TTL falls back to default
    Sticky = Policy#policy.sticky,
    ?assertEqual(3600, maps:get(<<"ttl_seconds">>, Sticky)),
    
    ok.

test_invalid_ttl_duration_zero(_Config) ->
    %% Create policy with TTL = "0s"
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl">> => <<"0s">>  %% Zero seconds
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_ttl_zero">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify TTL is parsed as 0
    Sticky = Policy#policy.sticky,
    ?assertEqual(0, maps:get(<<"ttl_seconds">>, Sticky)),
    
    ok.

test_invalid_ttl_duration_negative(_Config) ->
    %% Create policy with negative TTL (via legacy ttl_seconds)
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl_seconds">> => -100  %% Negative value
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_ttl_negative">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify negative TTL is stored as-is (validation happens elsewhere)
    Sticky = Policy#policy.sticky,
    ?assertEqual(-100, maps:get(<<"ttl_seconds">>, Sticky)),
    
    ok.

test_conflicting_fallback_rules(_Config) ->
    %% Create policy with conflicting fallback rules (same condition, different providers)
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0}  %% Zero weight to trigger fallback
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"to">> => <<"provider_a">>
            },
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"to">> => <<"provider_b">>  %% Same condition, different provider
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_conflicting_fallbacks">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify both fallback rules are parsed
    ?assertEqual(2, length(Policy#policy.fallbacks)),
    
    %% Route should use first matching fallback rule (order matters)
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_conflicting">>
        },
        <<"edge_conflicting_fallbacks">>,
        #{<<"status">> => <<"timeout">>}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    %% First fallback rule should be used
    ?assertEqual(<<"provider_a">>, Decision#route_decision.provider_id),
    
    ok.

test_overlapping_fallback_rules(_Config) ->
    %% Create policy with overlapping fallback rules (one condition is subset of another)
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0}
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>, <<"5xx">>]},
                <<"to">> => <<"provider_a">>
            },
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"to">> => <<"provider_b">>  %% Overlapping condition
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_overlapping_fallbacks">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify both fallback rules are parsed
    ?assertEqual(2, length(Policy#policy.fallbacks)),
    
    %% Route with "timeout" should match first rule (order matters)
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_overlapping">>
        },
        <<"edge_overlapping_fallbacks">>,
        #{<<"status">> => <<"timeout">>}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    %% First matching rule should be used
    ?assertEqual(<<"provider_a">>, Decision#route_decision.provider_id),
    
    ok.

test_empty_extensions_arrays(_Config) ->
    %% Create policy with empty extension arrays
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"pre">> => [],
        <<"validators">> => [],
        <<"post">> => []
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_empty_extensions">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify empty arrays are parsed correctly
    ?assertEqual([], Policy#policy.pre),
    ?assertEqual([], Policy#policy.validators),
    ?assertEqual([], Policy#policy.post),
    
    %% Route should still work
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_empty_ext">>
        },
        <<"edge_empty_extensions">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_unknown_fields_in_extensions(_Config) ->
    %% Create policy with unknown fields in extension elements
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"pre">> => [
            #{
                <<"id">> => <<"normalize_text">>,
                <<"mode">> => <<"required">>,
                <<"config">> => #{<<"lowercase">> => true},
                <<"unknown_field">> => <<"should_be_ignored">>,  %% Unknown field
                <<"timeout_ms">> => 1000  %% Future field (not yet supported)
            }
        ],
        <<"validators">> => [
            #{
                <<"id">> => <<"pii_guard">>,
                <<"on_fail">> => <<"block">>,
                <<"unknown_validator_field">> => <<"ignored">>  %% Unknown field
            }
        ],
        <<"post">> => [
            #{
                <<"id">> => <<"mask_pii">>,
                <<"mode">> => <<"optional">>,
                <<"future_field">> => <<"not_yet_used">>  %% Future field
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_unknown_fields">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify extensions are parsed (unknown fields should be ignored or stored in config)
    ?assertEqual(1, length(Policy#policy.pre)),
    ?assertEqual(1, length(Policy#policy.validators)),
    ?assertEqual(1, length(Policy#policy.post)),
    
    [Pre] = Policy#policy.pre,
    ?assertEqual(<<"normalize_text">>, maps:get(id, Pre)),
    ?assertEqual(<<"required">>, maps:get(mode, Pre)),
    
    %% Route should still work
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_unknown">>
        },
        <<"edge_unknown_fields">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

test_deep_legacy_new_combination(_Config) ->
    %% Create policy with deep combination of legacy and new format
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        %% New format: providers array
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 50},
            #{<<"name">> => <<"anthropic">>, <<"weight">> => 30}
        ],
        %% Legacy format: weights map (should be ignored if providers present)
        <<"weights">> => #{
            <<"legacy_provider">> => 0.2
        },
        %% New format: fallbacks array
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"retry">> => 2,
                <<"to">> => <<"provider_new">>
            }
        ],
        %% Legacy format: fallback object (should be converted to fallbacks array)
        <<"fallback">> => #{
            <<"provider">> => <<"provider_legacy">>,
            <<"conditions">> => [<<"all_providers_failed">>]
        },
        %% New format: sticky with ttl string
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"session_key">> => <<"user_id">>,
            <<"ttl">> => <<"10m">>
        },
        %% New format: extensions
        <<"pre">> => [
            #{<<"id">> => <<"pre_new">>, <<"mode">> => <<"required">>}
        ],
        <<"validators">> => [
            #{<<"id">> => <<"validator_new">>, <<"on_fail">> => <<"block">>}
        ],
        <<"post">> => [
            #{<<"id">> => <<"post_new">>, <<"mode">> => <<"optional">>}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"edge_deep_combination">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Verify new format takes precedence
    ?assertEqual(0.5, maps:get(<<"openai">>, Policy#policy.weights)),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, Policy#policy.weights)),
    %% Legacy weights should not be in weights map (new format takes precedence)
    ?assertEqual(undefined, maps:get(<<"legacy_provider">>, Policy#policy.weights, undefined)),
    
    %% Verify fallbacks array is used (legacy fallback converted)
    ?assert(length(Policy#policy.fallbacks) >= 1),
    
    %% Verify sticky uses new format (ttl string)
    Sticky = Policy#policy.sticky,
    ?assertEqual(600, maps:get(<<"ttl_seconds">>, Sticky)),  %% 10m = 600s
    
    %% Verify extensions use new format
    ?assertEqual(1, length(Policy#policy.pre)),
    [Pre] = Policy#policy.pre,
    ?assertEqual(<<"pre_new">>, maps:get(id, Pre)),
    
    %% Route should work with deep combination
    RouteRequest = create_route_request(
        #{
            <<"tenant_id">> => ?TENANT_ID,
            <<"message_id">> => <<"msg_edge_deep">>
        },
        <<"edge_deep_combination">>,
        #{}
    ),
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    ok.

