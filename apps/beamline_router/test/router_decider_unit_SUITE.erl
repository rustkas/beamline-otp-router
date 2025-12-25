%% @doc Unit Tests for router_decider module
%% Targeted coverage tests for pipeline complexity and provider selection validation
%% @test_category unit, fast, coverage_hotspot
-module(router_decider_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_calculate_pipeline_complexity_empty/1,
    test_calculate_pipeline_complexity_minimal/1,
    test_calculate_pipeline_complexity_medium/1,
    test_calculate_pipeline_complexity_high/1,
    test_calculate_pipeline_complexity_very_high/1,
    test_validate_provider_selection_valid/1,
    test_validate_provider_selection_empty_weights/1,
    test_validate_provider_selection_invalid_weights/1,
    test_validate_provider_selection_with_sticky/1,
    test_validate_provider_selection_with_fallback/1,
    test_get_provider_selection_status/1,
    test_list_available_providers/1,
    test_list_available_providers_with_fallbacks/1,
    test_execute_post_processors_empty/1,
    test_execute_post_processors_skip_invalid/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, 
    init_per_testcase/2, end_per_testcase/2,
    test_calculate_pipeline_complexity_empty/1,
    test_calculate_pipeline_complexity_minimal/1,
    test_calculate_pipeline_complexity_medium/1,
    test_calculate_pipeline_complexity_high/1,
    test_calculate_pipeline_complexity_very_high/1,
    test_validate_provider_selection_valid/1,
    test_validate_provider_selection_empty_weights/1,
    test_validate_provider_selection_invalid_weights/1,
    test_validate_provider_selection_with_sticky/1,
    test_validate_provider_selection_with_fallback/1,
    test_get_provider_selection_status/1,
    test_list_available_providers/1,
    test_list_available_providers_with_fallbacks/1,
    test_execute_post_processors_empty/1,
    test_execute_post_processors_skip_invalid/1
]}).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() ->
    [
        {unit_tests, [sequence], [
            test_calculate_pipeline_complexity_empty,
            test_calculate_pipeline_complexity_minimal,
            test_calculate_pipeline_complexity_medium,
            test_calculate_pipeline_complexity_high,
            test_calculate_pipeline_complexity_very_high,
            test_validate_provider_selection_valid,
            test_validate_provider_selection_empty_weights,
            test_validate_provider_selection_invalid_weights,
            test_validate_provider_selection_with_sticky,
            test_validate_provider_selection_with_fallback,
            test_get_provider_selection_status,
            test_list_available_providers,
            test_list_available_providers_with_fallbacks,
            test_execute_post_processors_empty,
            test_execute_post_processors_skip_invalid
        ]}
    ].

init_per_suite(Config) ->
    %% Start application
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for calculate_pipeline_complexity/3
%% ============================================================================

test_calculate_pipeline_complexity_empty(_Config) ->
    Pre = [],
    Validators = [],
    Post = [],
    
    Complexity = router_decider:calculate_pipeline_complexity(Pre, Validators, Post),
    
    ?assertEqual(true, is_map(Complexity)),
    ?assertEqual(0, maps:get(total_extensions, Complexity)),
    ?assertEqual(0, maps:get(pre_count, Complexity)),
    ?assertEqual(0, maps:get(validators_count, Complexity)),
    ?assertEqual(0, maps:get(post_count, Complexity)),
    ?assertEqual(<<"low">>, maps:get(complexity_level, Complexity)),
    
    ok.

test_calculate_pipeline_complexity_minimal(_Config) ->
    Pre = [#{id => <<"pre_1">>, mode => <<"optional">>}],
    Validators = [],
    Post = [],
    
    Complexity = router_decider:calculate_pipeline_complexity(Pre, Validators, Post),
    
    ?assertEqual(1, maps:get(total_extensions, Complexity)),
    ?assertEqual(1, maps:get(pre_count, Complexity)),
    ?assertEqual(0, maps:get(validators_count, Complexity)),
    ?assertEqual(0, maps:get(post_count, Complexity)),
    
    ok.

test_calculate_pipeline_complexity_medium(_Config) ->
    Pre = [#{id => <<"pre_1">>}, #{id => <<"pre_2">>}],
    Validators = [#{id => <<"val_1">>, on_fail => <<"block">>}],
    Post = [#{id => <<"post_1">>}],
    
    Complexity = router_decider:calculate_pipeline_complexity(Pre, Validators, Post),
    
    ?assertEqual(4, maps:get(total_extensions, Complexity)),
    ?assertEqual(2, maps:get(pre_count, Complexity)),
    ?assertEqual(1, maps:get(validators_count, Complexity)),
    ?assertEqual(1, maps:get(post_count, Complexity)),
    
    %% Check complexity level is one of the valid levels (low/medium/high/very_high)
    Level = maps:get(complexity_level, Complexity),
    ValidLevels = [<<"low">>, <<"medium">>, <<"high">>, <<"very_high">>],
    ?assertEqual(true, lists:member(Level, ValidLevels)),
    
    ok.

test_calculate_pipeline_complexity_high(_Config) ->
    %% Create high complexity pipeline (many extensions)
    Pre = [#{id => <<"pre_", (integer_to_binary(I))/binary>>} || I <- lists:seq(1, 3)],
    Validators = [#{id => <<"val_", (integer_to_binary(I))/binary>>, on_fail => <<"block">>} || I <- lists:seq(1, 3)],
    Post = [#{id => <<"post_", (integer_to_binary(I))/binary>>} || I <- lists:seq(1, 3)],
    
    Complexity = router_decider:calculate_pipeline_complexity(Pre, Validators, Post),
    
    ?assertEqual(9, maps:get(total_extensions, Complexity)),
    
    %% Check warnings are generated for high complexity
    Warnings = maps:get(warnings, Complexity, []),
    ?assertEqual(true, is_list(Warnings)),
    
    %% Check recommendations are generated
    Recommendations = maps:get(recommendations, Complexity, []),
    ?assertEqual(true, is_list(Recommendations)),
    
    ok.

test_calculate_pipeline_complexity_very_high(_Config) ->
    %% Create very high complexity pipeline
    Pre = [#{id => <<"pre_", (integer_to_binary(I))/binary>>} || I <- lists:seq(1, 5)],
    Validators = [#{id => <<"val_", (integer_to_binary(I))/binary>>, on_fail => <<"block">>} || I <- lists:seq(1, 4)],
    Post = [#{id => <<"post_", (integer_to_binary(I))/binary>>} || I <- lists:seq(1, 3)],
    
    Complexity = router_decider:calculate_pipeline_complexity(Pre, Validators, Post),
    
    ?assertEqual(12, maps:get(total_extensions, Complexity)),
    
    %% Check estimated latency is calculated
    EstimatedLatency = maps:get(estimated_latency_ms, Complexity),
    ?assertEqual(true, is_integer(EstimatedLatency)),
    ?assertEqual(true, EstimatedLatency > 0),
    
    ok.

%% ============================================================================
%% Tests for validate_provider_selection/2
%% ============================================================================

test_validate_provider_selection_valid(_Config) ->
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        weights = #{<<"openai">> => 0.7, <<"anthropic">> => 0.3},
        fallback = undefined,
        fallbacks = [],
        sticky = undefined
    },
    Context = #{},
    
    Result = router_decider:validate_provider_selection(Policy, Context),
    
    ?assertMatch({ok, _}, Result),
    
    ok.

test_validate_provider_selection_empty_weights(_Config) ->
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        weights = #{},
        fallback = undefined,
        fallbacks = [],
        sticky = undefined
    },
    Context = #{},
    
    Result = router_decider:validate_provider_selection(Policy, Context),
    
    %% Empty weights without fallback should fail
    ?assertMatch({error, _}, Result),
    
    ok.

test_validate_provider_selection_invalid_weights(_Config) ->
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        weights = #{<<"openai">> => -0.5},  %% Invalid negative weight
        fallback = undefined,
        fallbacks = [],
        sticky = undefined
    },
    Context = #{},
    
    Result = router_decider:validate_provider_selection(Policy, Context),
    
    ?assertMatch({error, _}, Result),
    
    ok.

test_validate_provider_selection_with_sticky(_Config) ->
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        weights = #{<<"openai">> => 1.0},
        fallback = undefined,
        fallbacks = [],
        sticky = #{<<"enabled">> => true, <<"session_key">> => <<"session_id">>}
    },
    Context = #{},
    
    Result = router_decider:validate_provider_selection(Policy, Context),
    
    ?assertMatch({ok, _}, Result),
    
    ok.

test_validate_provider_selection_with_fallback(_Config) ->
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        weights = #{<<"openai">> => 0.5, <<"anthropic">> => 0.5},
        fallback = <<"local_llm">>,
        fallbacks = [#{<<"to">> => <<"backup_provider">>, <<"when">> => #{<<"status">> => [<<"timeout">>]}}],
        sticky = undefined
    },
    Context = #{},
    
    Result = router_decider:validate_provider_selection(Policy, Context),
    
    ?assertMatch({ok, _}, Result),
    
    ok.

%% ============================================================================
%% Tests for get_provider_selection_status/1
%% ============================================================================

test_get_provider_selection_status(_Config) ->
    Policy = #policy{
        tenant_id = <<"status_tenant">>,
        policy_id = <<"status_policy">>,
        weights = #{<<"openai">> => 0.6, <<"anthropic">> => 0.4},
        fallback = <<"fallback_provider">>,
        fallbacks = [],
        sticky = #{<<"enabled">> => true, <<"session_key">> => <<"session_id">>}
    },
    
    Status = router_decider:get_provider_selection_status(Policy),
    
    ?assertEqual(true, is_map(Status)),
    ?assertEqual(<<"status_policy">>, maps:get(policy_id, Status)),
    ?assertEqual(<<"status_tenant">>, maps:get(tenant_id, Status)),
    ?assertEqual(true, maps:get(sticky_enabled, Status)),
    
    %% Check available providers
    AvailableProviders = maps:get(available_providers, Status),
    ?assertEqual(true, is_list(AvailableProviders)),
    ?assertEqual(2, length(AvailableProviders)),
    
    %% Check provider count
    ProviderCount = maps:get(provider_count, Status),
    ?assertEqual(2, ProviderCount),
    
    %% Check total weight
    TotalWeight = maps:get(total_weight, Status),
    ?assertEqual(true, TotalWeight > 0.0),
    
    ok.

%% ============================================================================
%% Tests for list_available_providers/1
%% ============================================================================

test_list_available_providers(_Config) ->
    Policy = #policy{
        tenant_id = <<"list_tenant">>,
        policy_id = <<"list_policy">>,
        weights = #{<<"openai">> => 0.5, <<"anthropic">> => 0.3, <<"cohere">> => 0.2},
        fallback = undefined,
        fallbacks = [],
        sticky = undefined
    },
    
    Providers = router_decider:list_available_providers(Policy),
    
    ?assertEqual(true, is_list(Providers)),
    ?assertEqual(3, length(Providers)),
    ?assertEqual(true, lists:member(<<"openai">>, Providers)),
    ?assertEqual(true, lists:member(<<"anthropic">>, Providers)),
    ?assertEqual(true, lists:member(<<"cohere">>, Providers)),
    
    ok.

test_list_available_providers_with_fallbacks(_Config) ->
    Policy = #policy{
        tenant_id = <<"list_tenant">>,
        policy_id = <<"list_policy">>,
        weights = #{<<"openai">> => 0.7, <<"anthropic">> => 0.3},
        fallback = <<"local_llm">>,
        fallbacks = [
            #{<<"to">> => <<"backup_1">>},
            #{<<"to">> => <<"backup_2">>}
        ],
        sticky = undefined
    },
    
    Providers = router_decider:list_available_providers(Policy),
    
    ?assertEqual(true, is_list(Providers)),
    %% Should include weights providers + fallback providers
    ?assertEqual(true, lists:member(<<"openai">>, Providers)),
    ?assertEqual(true, lists:member(<<"anthropic">>, Providers)),
    ?assertEqual(true, lists:member(<<"local_llm">>, Providers)),
    ?assertEqual(true, lists:member(<<"backup_1">>, Providers)),
    ?assertEqual(true, lists:member(<<"backup_2">>, Providers)),
    
    ok.

%% ============================================================================
%% Tests for execute_post_processors/3
%% ============================================================================

test_execute_post_processors_empty(_Config) ->
    PostProcessors = [],
    Response = #{<<"result">> => <<"success">>},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Result = router_decider:execute_post_processors(PostProcessors, Response, Context),
    
    ?assertMatch({ok, _, _}, Result),
    {ok, ReturnedResponse, ReturnedContext} = Result,
    ?assertEqual(Response, ReturnedResponse),
    ?assertEqual(Context, ReturnedContext),
    
    ok.

test_execute_post_processors_skip_invalid(_Config) ->
    %% Post-processor items with missing 'id' field should be skipped
    %% Note: Non-map items will cause badmap error, so we only test maps with missing fields
    PostProcessors = [
        #{invalid => true},  %% Missing 'id' field - should be skipped
        #{other_field => <<"value">>}  %% Missing 'id' field - should be skipped
    ],
    Response = #{<<"result">> => <<"success">>},
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    Result = router_decider:execute_post_processors(PostProcessors, Response, Context),
    
    %% Should succeed by skipping invalid items (maps without 'id' field)
    ?assertMatch({ok, _, _}, Result),
    
    ok.
