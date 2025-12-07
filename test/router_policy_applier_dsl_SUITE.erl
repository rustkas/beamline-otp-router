%% @doc Common Test Suite for Policy Applier DSL
%% Tests all DSL branches including edge-cases:
%% - Providers array parsing (weights 0-100 → 0.0-1.0)
%% - Fallbacks array parsing with when conditions
%% - Sticky routing with TTL string parsing
%% - Extensions parsing (pre, validators, post)
%% - Edge cases: empty values, invalid formats, boundary conditions
%% @test_category cp1_smoke, fast
-module(router_policy_applier_dsl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Define policy record locally to avoid include path issues
-record(policy, {
    tenant_id :: binary(),
    policy_id :: binary(),
    weights :: map(),
    fallback :: map() | undefined,
    fallbacks :: list(),
    sticky :: map() | undefined,
    circuit_breaker :: map() | undefined,
    rate_limit :: map() | undefined,
    pre :: list(),
    validators :: list(),
    post :: list(),
    metadata :: map()
}).

%% Records are defined in include/beamline_router.hrl

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).

-define(TENANT_ID, <<"test_tenant">>).
-define(POLICY_ID, <<"test_policy">>).

%% Test suite configuration
all() ->
    [
        {group, dsl_parsing_tests},
        {group, policy_applier_tests},
        {group, edge_cases_tests}
    ].

groups() ->
    [
        {dsl_parsing_tests, [parallel], [
            test_providers_array_parsing,
            test_providers_weight_conversion,
            test_providers_empty_array,
            test_providers_invalid_format,
            test_fallbacks_array_parsing,
            test_fallbacks_when_condition_parsing,
            test_fallbacks_multiple_rules,
            test_fallbacks_empty_array,
            test_sticky_ttl_string_parsing,
            test_sticky_ttl_various_formats,
            test_sticky_session_key_parsing,
            test_sticky_disabled,
            test_extensions_pre_parsing,
            test_extensions_validators_parsing,
            test_extensions_post_parsing,
            test_extensions_empty,
            test_legacy_weights_map,
            test_legacy_fallback_object,
            test_legacy_sticky_ttl_seconds
        ]},
        {policy_applier_tests, [parallel], [
            test_apply_policy_with_providers_array,
            test_apply_policy_with_fallbacks_array,
            test_apply_policy_with_sticky_enabled,
            test_apply_policy_with_extensions,
            test_apply_policy_explanation_sticky,
            test_apply_policy_explanation_weighted,
            test_apply_policy_explanation_fallback,
            test_apply_policy_extensions_extraction,
            test_apply_policy_with_context
        ]},
        {edge_cases_tests, [parallel], [
            test_empty_policy_map,
            test_missing_required_fields,
            test_invalid_weight_values,
            test_weight_sum_not_100,
            test_fallback_without_when,
            test_fallback_without_to,
            test_sticky_without_enabled,
            test_invalid_ttl_format,
            test_extensions_invalid_format,
            test_mixed_legacy_and_new_format,
            test_provider_weight_zero,
            test_provider_weight_100,
            test_fallback_condition_no_match,
            test_sticky_session_expired,
            test_extensions_missing_id,
            test_providers_large_array,
            test_fallbacks_large_array,
            test_provider_weight_float_in_array,
            test_fallback_when_single_value,
            test_sticky_ttl_edge_cases,
            test_extensions_all_types_empty,
            test_apply_policy_without_policy_id,
            test_fallback_multiple_when_conditions
        ]}
    ].

init_per_suite(Config) ->
    %% Setup Mnesia for sticky sessions
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Start required services
    case whereis(router_policy_store) of
        undefined -> {ok, _} = router_policy_store:start_link();
        _ -> ok
    end,
    case whereis(router_sticky_store) of
        undefined -> {ok, _} = router_sticky_store:start_link();
        _ -> ok
    end,
    
    Config.

end_per_suite(Config) ->
    %% Stop services
    catch gen_server:stop(router_policy_store),
    catch gen_server:stop(router_sticky_store),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear policy store and sticky store
    catch router_policy_store:reload_fixtures(),
    %% Clear sticky sessions by deleting all entries (if table exists)
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
%% DSL Parsing Tests
%% ============================================================================

test_providers_array_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 70},
            #{<<"name">> => <<"anthropic">>, <<"weight">> => 30}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(0.7, maps:get(<<"openai">>, Policy#policy.weights)),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, Policy#policy.weights)),
    ok.

test_providers_weight_conversion(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"provider_a">>, <<"weight">> => 0},
            #{<<"name">> => <<"provider_b">>, <<"weight">> => 50},
            #{<<"name">> => <<"provider_c">>, <<"weight">> => 100}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(0.0, maps:get(<<"provider_a">>, Policy#policy.weights)),
    ?assertEqual(0.5, maps:get(<<"provider_b">>, Policy#policy.weights)),
    ?assertEqual(1.0, maps:get(<<"provider_c">>, Policy#policy.weights)),
    ok.

test_providers_empty_array(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => []
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(#{}, Policy#policy.weights),
    ok.

test_providers_invalid_format(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => <<"invalid">>  % Not an array
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    %% Should fallback to empty weights
    ?assertEqual(#{}, Policy#policy.weights),
    ok.

test_fallbacks_array_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>, <<"5xx">>]},
                <<"retry">> => 2,
                <<"to">> => <<"provider_b">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(1, length(Policy#policy.fallbacks)),
    [Fallback] = Policy#policy.fallbacks,
    ?assertEqual(<<"provider_b">>, maps:get(<<"to">>, Fallback)),
    ?assertEqual(2, maps:get(<<"retry">>, Fallback)),
    ok.

test_fallbacks_when_condition_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"to">> => <<"provider_b">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    [Fallback] = Policy#policy.fallbacks,
    When = maps:get(<<"when">>, Fallback),
    ?assertEqual([<<"timeout">>], maps:get(<<"status">>, When)),
    ok.

test_fallbacks_multiple_rules(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"retry">> => 2,
                <<"to">> => <<"provider_b">>
            },
            #{
                <<"when">> => #{<<"status">> => [<<"rate_limited">>]},
                <<"retry">> => 1,
                <<"to">> => <<"provider_c">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(2, length(Policy#policy.fallbacks)),
    [Fallback1, Fallback2] = Policy#policy.fallbacks,
    ?assertEqual(<<"provider_b">>, maps:get(<<"to">>, Fallback1)),
    ?assertEqual(<<"provider_c">>, maps:get(<<"to">>, Fallback2)),
    ok.

test_fallbacks_empty_array(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => []
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual([], Policy#policy.fallbacks),
    ok.

test_sticky_ttl_string_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"session_key">> => <<"user_id">>,
            <<"ttl">> => <<"10m">>
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    ?assertEqual(600, maps:get(<<"ttl_seconds">>, Sticky)),  % 10 minutes = 600 seconds
    ?assertEqual(<<"user_id">>, maps:get(<<"session_key">>, Sticky)),
    ok.

test_sticky_ttl_various_formats(_Config) ->
    TestCases = [
        {<<"30s">>, 30},
        {<<"5m">>, 300},
        {<<"1h">>, 3600},
        {<<"2h">>, 7200}
    ],
    
    lists:foreach(fun({TTLString, ExpectedSeconds}) ->
        PolicyMap = #{
            <<"version">> => <<"1.0">>,
            <<"sticky">> => #{
                <<"enabled">> => true,
                <<"ttl">> => TTLString
            }
        },
        Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
        Sticky = Policy#policy.sticky,
        ?assertEqual(ExpectedSeconds, maps:get(<<"ttl_seconds">>, Sticky))
    end, TestCases),
    ok.

test_sticky_session_key_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"session_key">> => <<"message_id">>
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    ?assertEqual(<<"message_id">>, maps:get(<<"session_key">>, Sticky)),
    ok.

test_sticky_disabled(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"sticky">> => #{
            <<"enabled">> => false
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    ?assertNot(maps:get(<<"enabled">>, Sticky)),
    ok.

test_extensions_pre_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"pre">> => [
            #{
                <<"id">> => <<"normalize_text">>,
                <<"mode">> => <<"required">>,
                <<"config">> => #{<<"lowercase">> => true}
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(1, length(Policy#policy.pre)),
    [Pre] = Policy#policy.pre,
    ?assertEqual(<<"normalize_text">>, maps:get(id, Pre)),
    ?assertEqual(<<"required">>, maps:get(mode, Pre)),
    ok.

test_extensions_validators_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"validators">> => [
            #{
                <<"id">> => <<"pii_guard">>,
                <<"on_fail">> => <<"block">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(1, length(Policy#policy.validators)),
    [Validator] = Policy#policy.validators,
    ?assertEqual(<<"pii_guard">>, maps:get(id, Validator)),
    ?assertEqual(<<"block">>, maps:get(on_fail, Validator)),
    ok.

test_extensions_post_parsing(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"post">> => [
            #{
                <<"id">> => <<"mask_pii">>,
                <<"mode">> => <<"optional">>,
                <<"config">> => #{<<"mask_email">> => true}
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(1, length(Policy#policy.post)),
    [Post] = Policy#policy.post,
    ?assertEqual(<<"mask_pii">>, maps:get(id, Post)),
    ?assertEqual(<<"optional">>, maps:get(mode, Post)),
    ok.

test_extensions_empty(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual([], Policy#policy.pre),
    ?assertEqual([], Policy#policy.validators),
    ?assertEqual([], Policy#policy.post),
    ok.

test_legacy_weights_map(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"weights">> => #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(0.7, maps:get(<<"openai">>, Policy#policy.weights)),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, Policy#policy.weights)),
    ok.

test_legacy_fallback_object(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallback">> => #{
            <<"provider">> => <<"local_llm">>,
            <<"conditions">> => [<<"timeout">>]
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertNotEqual(undefined, Policy#policy.fallback),
    ?assertEqual(1, length(Policy#policy.fallbacks)),  % Converted to fallbacks array
    ok.

test_legacy_sticky_ttl_seconds(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl_seconds">> => 3600
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    ?assertEqual(3600, maps:get(<<"ttl_seconds">>, Sticky)),
    ok.

%% ============================================================================
%% Policy Applier Tests
%% ============================================================================

test_apply_policy_with_providers_array(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    %% Insert policy into store
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    ?assertEqual(<<"openai">>, maps:get(provider_id, Result)),
    
    Explanation = maps:get(explanation, Result),
    verify_explanation_format(Explanation),
    ?assertEqual(<<"weighted">>, maps:get(reason, Explanation)),
    ok.

test_apply_policy_with_fallbacks_array(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0}  % Zero weight to trigger fallback
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"all_providers_failed">>]},
                <<"to">> => <<"fallback_provider">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{<<"status">> => <<"all_providers_failed">>}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    ?assertEqual(<<"fallback_provider">>, maps:get(provider_id, Result)),
    
    Explanation = maps:get(explanation, Result),
    verify_explanation_format(Explanation),
    ?assertEqual(<<"fallback">>, maps:get(reason, Explanation)),
    ok.

test_apply_policy_with_sticky_enabled(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"session_key">> => <<"user_id">>,
            <<"ttl">> => <<"10m">>
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Create sticky session
    router_sticky_store:set_provider(?TENANT_ID, <<"user_123">>, <<"anthropic">>),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{<<"user_id">> => <<"user_123">>}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    ?assertEqual(<<"anthropic">>, maps:get(provider_id, Result)),
    
    Explanation = maps:get(explanation, Result),
    verify_explanation_format(Explanation),
    ?assertEqual(<<"sticky">>, maps:get(reason, Explanation)),
    ok.

test_apply_policy_with_extensions(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"pre">> => [
            #{<<"id">> => <<"normalize_text">>, <<"mode">> => <<"required">>}
        ],
        <<"validators">> => [
            #{<<"id">> => <<"pii_guard">>, <<"on_fail">> => <<"block">>}
        ],
        <<"post">> => [
            #{<<"id">> => <<"mask_pii">>, <<"mode">> => <<"optional">>}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    Extensions = maps:get(extensions, Result),
    PreList = maps:get(pre, Extensions),
    ValidatorsList = maps:get(validators, Extensions),
    PostList = maps:get(post, Extensions),
    ?assertEqual(1, length(PreList)),
    ?assertEqual(1, length(ValidatorsList)),
    ?assertEqual(1, length(PostList)),
    ok.

%% Helper: Verify explanation conforms to formal specification in ROUTING_POLICY.md
verify_explanation_format(Explanation) ->
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

test_apply_policy_explanation_sticky(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"session_key">> => <<"user_id">>
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    router_sticky_store:set_provider(?TENANT_ID, <<"user_123">>, <<"anthropic">>),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{<<"user_id">> => <<"user_123">>}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    Explanation = maps:get(explanation, Result),
    %% Verify explanation conforms to formal specification
    verify_explanation_format(Explanation),
    
    %% Verify specific fields for sticky
    ?assertEqual(<<"sticky">>, maps:get(reason, Explanation)),
    Steps = maps:get(steps, Explanation),
    ?assert(length(Steps) > 0),
    ok.

test_apply_policy_explanation_weighted(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 70},
            #{<<"name">> => <<"anthropic">>, <<"weight">> => 30}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    Explanation = maps:get(explanation, Result),
    %% Verify explanation conforms to formal specification
    verify_explanation_format(Explanation),
    
    %% Verify specific fields for weighted
    ?assertEqual(<<"weighted">>, maps:get(reason, Explanation)),
    Steps = maps:get(steps, Explanation),
    ?assert(length(Steps) > 0),
    ok.

test_apply_policy_explanation_fallback(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0}
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"to">> => <<"fallback_provider">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{<<"status">> => <<"timeout">>}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    Explanation = maps:get(explanation, Result),
    %% Verify explanation conforms to formal specification
    verify_explanation_format(Explanation),
    
    %% Verify specific fields for fallback
    ?assertEqual(<<"fallback">>, maps:get(reason, Explanation)),
    Steps = maps:get(steps, Explanation),
    ?assert(length(Steps) > 0),
    ok.

test_apply_policy_extensions_extraction(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"pre">> => [
            #{<<"id">> => <<"pre1">>, <<"mode">> => <<"required">>},
            #{<<"id">> => <<"pre2">>, <<"mode">> => <<"optional">>}
        ],
        <<"validators">> => [
            #{<<"id">> => <<"val1">>, <<"on_fail">> => <<"block">>}
        ],
        <<"post">> => [
            #{<<"id">> => <<"post1">>, <<"mode">> => <<"required">>}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    Extensions = maps:get(extensions, Result),
    PreList = maps:get(pre, Extensions),
    ValidatorsList = maps:get(validators, Extensions),
    PostList = maps:get(post, Extensions),
    ?assertEqual(2, length(PreList)),
    ?assertEqual(1, length(ValidatorsList)),
    ?assertEqual(1, length(PostList)),
    ok.

test_apply_policy_with_context(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{}
    },
    AdditionalContext = #{
        <<"trace_id">> => <<"trace_123">>,
        <<"user_id">> => <<"user_456">>
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID, AdditionalContext),
    
    Explanation = maps:get(explanation, Result),
    %% Verify explanation conforms to formal specification
    verify_explanation_format(Explanation),
    
    Context = maps:get(context, Explanation),
    ?assertEqual(<<"trace_123">>, maps:get(<<"trace_id">>, Context)),
    ok.

%% ============================================================================
%% Edge Cases Tests
%% ============================================================================

test_empty_policy_map(_Config) ->
    PolicyMap = #{},
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    %% Verify policy structure (version is not in record, check other fields)
    ?assertEqual(#{}, Policy#policy.weights),
    ?assertEqual([], Policy#policy.fallbacks),
    ok.

test_missing_required_fields(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>
        % No providers, no weights
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(#{}, Policy#policy.weights),
    ok.

test_invalid_weight_values(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"provider_a">>, <<"weight">> => -10},  % Negative
            #{<<"name">> => <<"provider_b">>, <<"weight">> => 150}   % > 100
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    %% Should still parse, validation happens later
    ?assertEqual(-0.1, maps:get(<<"provider_a">>, Policy#policy.weights)),
    ?assertEqual(1.5, maps:get(<<"provider_b">>, Policy#policy.weights)),
    ok.

test_weight_sum_not_100(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"provider_a">>, <<"weight">> => 30},
            #{<<"name">> => <<"provider_b">>, <<"weight">> => 40}
            % Sum = 70, not 100
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    %% Should still parse, validation happens later
    ?assertEqual(0.3, maps:get(<<"provider_a">>, Policy#policy.weights)),
    ?assertEqual(0.4, maps:get(<<"provider_b">>, Policy#policy.weights)),
    ok.

test_fallback_without_when(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => [
            #{
                <<"to">> => <<"provider_b">>
                % Missing "when"
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    [Fallback] = Policy#policy.fallbacks,
    ?assertEqual(#{}, maps:get(<<"when">>, Fallback, #{})),
    ok.

test_fallback_without_to(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]}
                % Missing "to"
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    [Fallback] = Policy#policy.fallbacks,
    ?assertEqual(undefined, maps:get(<<"to">>, Fallback, undefined)),
    ok.

test_sticky_without_enabled(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"sticky">> => #{
            <<"session_key">> => <<"user_id">>
            % Missing "enabled"
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    ?assertNot(maps:get(<<"enabled">>, Sticky, false)),
    ok.

test_invalid_ttl_format(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl">> => <<"invalid_format">>
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    ?assertEqual(3600, maps:get(<<"ttl_seconds">>, Sticky)),  % Default fallback
    ok.

test_extensions_invalid_format(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"pre">> => <<"invalid">>,  % Not an array
        <<"validators">> => 123,      % Not an array
        <<"post">> => #{}             % Not an array
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual([], Policy#policy.pre),
    ?assertEqual([], Policy#policy.validators),
    ?assertEqual([], Policy#policy.post),
    ok.

test_mixed_legacy_and_new_format(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"new_provider">>, <<"weight">> => 50}
        ],
        <<"weights">> => #{
            <<"legacy_provider">> => 0.5
        },
        <<"fallbacks">> => [
            #{<<"when">> => #{<<"status">> => [<<"timeout">>]}, <<"to">> => <<"new_fallback">>}
        ],
        <<"fallback">> => #{
            <<"provider">> => <<"legacy_fallback">>
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    %% New format should take precedence
    ?assertEqual(0.5, maps:get(<<"new_provider">>, Policy#policy.weights)),
    ?assertEqual(1, length(Policy#policy.fallbacks)),
    ok.

test_provider_weight_zero(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"provider_a">>, <<"weight">> => 0},
            #{<<"name">> => <<"provider_b">>, <<"weight">> => 100}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(0.0, maps:get(<<"provider_a">>, Policy#policy.weights)),
    ?assertEqual(1.0, maps:get(<<"provider_b">>, Policy#policy.weights)),
    ok.

test_provider_weight_100(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"provider_a">>, <<"weight">> => 100}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(1.0, maps:get(<<"provider_a">>, Policy#policy.weights)),
    ok.

test_fallback_condition_no_match(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0}
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => [<<"timeout">>]},
                <<"to">> => <<"fallback_provider">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{<<"status">> => <<"5xx">>}  % Different status
    },
    
    %% Should not match fallback condition
    Result = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    %% Should either use legacy fallback or return error
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

test_sticky_session_expired(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ],
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"session_key">> => <<"user_id">>,
            <<"ttl">> => <<"1s">>  % Very short TTL
        }
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    %% Create sticky session (will expire quickly)
    router_sticky_store:set_provider(?TENANT_ID, <<"user_123">>, <<"anthropic">>),
    
    %% Wait for expiration
    timer:sleep(2000),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{<<"user_id">> => <<"user_123">>}
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    Explanation = maps:get(explanation, Result),
    verify_explanation_format(Explanation),
    
    %% Should fallback to weighted (sticky expired)
    ?assertEqual(<<"weighted">>, maps:get(reason, Explanation)),
    ok.

test_extensions_missing_id(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"pre">> => [
            #{
                <<"mode">> => <<"required">>
                % Missing "id"
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    [Pre] = Policy#policy.pre,
    ?assertEqual(#{}, Pre),  % Empty map for invalid entry
    ok.

test_providers_large_array(_Config) ->
    %% Test with many providers
    Providers = lists:map(fun(I) ->
        #{
            <<"name">> => list_to_binary("provider_" ++ integer_to_list(I)),
            <<"weight">> => 1
        }
    end, lists:seq(1, 100)),
    
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => Providers
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(100, map_size(Policy#policy.weights)),
    ok.

test_fallbacks_large_array(_Config) ->
    %% Test with many fallback rules
    Fallbacks = lists:map(fun(I) ->
        #{
            <<"when">> => #{<<"status">> => [list_to_binary("error_" ++ integer_to_list(I))]},
            <<"to">> => list_to_binary("provider_" ++ integer_to_list(I))
        }
    end, lists:seq(1, 50)),
    
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => Fallbacks
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(50, length(Policy#policy.fallbacks)),
    ok.

test_provider_weight_float_in_array(_Config) ->
    %% Test with float weight in providers array (should still convert)
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"provider_a">>, <<"weight">> => 50.0}  % Float instead of integer
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual(0.5, maps:get(<<"provider_a">>, Policy#policy.weights)),
    ok.

test_fallback_when_single_value(_Config) ->
    %% Test when condition with single value (not array)
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"fallbacks">> => [
            #{
                <<"when">> => #{<<"status">> => <<"timeout">>},  % Single value, not array
                <<"to">> => <<"provider_b">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    [Fallback] = Policy#policy.fallbacks,
    When = maps:get(<<"when">>, Fallback),
    ?assertEqual(<<"timeout">>, maps:get(<<"status">>, When)),
    ok.

test_sticky_ttl_edge_cases(_Config) ->
    %% Test edge cases for TTL parsing
    TestCases = [
        {<<"0s">>, 0},      % Zero seconds
        {<<"0m">>, 0},      % Zero minutes
        {<<"999s">>, 999},  % Large number
        {<<"99m">>, 5940}   % Large minutes
    ],
    
    lists:foreach(fun({TTLString, ExpectedSeconds}) ->
        PolicyMap = #{
            <<"version">> => <<"1.0">>,
            <<"sticky">> => #{
                <<"enabled">> => true,
                <<"ttl">> => TTLString
            }
        },
        Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
        Sticky = Policy#policy.sticky,
        ?assertEqual(ExpectedSeconds, maps:get(<<"ttl_seconds">>, Sticky))
    end, TestCases),
    ok.

test_extensions_all_types_empty(_Config) ->
    %% Test with all extension types empty
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"pre">> => [],
        <<"validators">> => [],
        <<"post">> => []
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    
    ?assertEqual([], Policy#policy.pre),
    ?assertEqual([], Policy#policy.validators),
    ?assertEqual([], Policy#policy.post),
    ok.

test_apply_policy_without_policy_id(_Config) ->
    %% Test applying policy without policy_id (should use default)
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 100}
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, <<"default">>, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{}
    },
    
    %% Apply without policy_id (should use default)
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, undefined),
    
    ?assertEqual(<<"openai">>, maps:get(provider_id, Result)),
    ok.

test_fallback_multiple_when_conditions(_Config) ->
    %% Test fallback with multiple when conditions
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => [
            #{<<"name">> => <<"openai">>, <<"weight">> => 0}
        ],
        <<"fallbacks">> => [
            #{
                <<"when">> => #{
                    <<"status">> => [<<"timeout">>, <<"5xx">>],
                    <<"message_type">> => [<<"chat">>]
                },
                <<"to">> => <<"provider_b">>
            }
        ]
    },
    Policy = router_policy_store:parse_policy_map(?TENANT_ID, ?POLICY_ID, PolicyMap),
    router_policy_store:upsert_policy(?TENANT_ID, Policy),
    
    Request = #{
        message => #{<<"tenant_id">> => ?TENANT_ID},
        context => #{
            <<"status">> => <<"timeout">>,
            <<"message_type">> => <<"chat">>
        }
    },
    
    {ok, Result} = router_policy_applier:apply_policy(Request, ?TENANT_ID, ?POLICY_ID),
    
    ?assertEqual(<<"provider_b">>, maps:get(provider_id, Result)),
    ok.

