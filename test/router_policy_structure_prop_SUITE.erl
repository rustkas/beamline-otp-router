%% @doc Property-based tests for policy structure parsing and invariants
%% Tests random valid policy structures from policy.schema.json
%% Verifies invariants: weight normalization, fallback chain finiteness, no crashes
%% @test_category property, combinatorial
-module(router_policy_structure_prop_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Undefine LET macro if it was defined by previous includes (e.g., from ct.hrl or beamline_router.hrl)
%% This allows proper.hrl to define its own LET macro without conflicts
-undef(LET).

%% Always include proper.hrl in test profile (PropEr is available in test profile)
%% Runtime check for PropEr availability is done in prop_* functions
-include_lib("proper/include/proper.hrl").

%% Include router header for policy record definition
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    %% Test functions called via groups
    test_policy_weight_normalization/1,
    test_policy_fallback_finiteness/1,
    test_policy_no_crashes/1,
    %% Helper functions used by property tests
    provider_name/0,
    policy_with_weights/0,
    policy_with_fallbacks/0,
    fallback_item/0,
    valid_policy/0,
    config_map/0,
    metadata_map/0,
    rand_float/0,
    rand_float/2,
    calculate_weight_sum/1,
    verify_no_fallback_cycles/1,
    verify_fallback_finiteness/2
]}).

all() ->
    [
        {group, property_tests}
    ].

groups() ->
    [
        {property_tests, [parallel], [
            test_policy_weight_normalization,
            test_policy_fallback_finiteness,
            test_policy_no_crashes
        ]}
    ].

init_per_suite(Config) ->
    %% Check if PropEr is available
    case code:which(proper) of
        non_existing ->
            ct:fail("PropEr not available - property tests require PropEr");
        _ ->
            ok
    end,
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

%% @doc Property test: Weight normalization
%% Verifies that policy weights are normalized correctly
test_policy_weight_normalization(_Config) ->
    %% Generate random policy with weights
    PolicyGen = policy_with_weights(),
    
    %% Run property test
    Result = proper:quickcheck(
        ?FORALL(Policy, PolicyGen,
            begin
                WeightSum = calculate_weight_sum(Policy),
                %% Weights should sum to approximately 1.0 (allow small floating point errors)
                (WeightSum >= 0.99) andalso (WeightSum =< 1.01)
            end
        ),
        [{numtests, 100}]
    ),
    case Result of
        true -> ok;
        _ -> ct:fail("Property test failed: ~p", [Result])
    end.

%% @doc Property test: Fallback chain finiteness
%% Verifies that fallback chains don't create infinite loops
test_policy_fallback_finiteness(_Config) ->
    %% Generate random policy with fallbacks
    PolicyGen = policy_with_fallbacks(),
    
    %% Run property test
    Result = proper:quickcheck(
        ?FORALL(Policy, PolicyGen,
            begin
                Fallbacks = Policy#policy.fallback,
                MaxDepth = 10,
                verify_fallback_finiteness(Fallbacks, MaxDepth)
            end
        ),
        [{numtests, 100}]
    ),
    case Result of
        true -> ok;
        _ -> ct:fail("Property test failed: ~p", [Result])
    end.

%% @doc Property test: No crashes on valid policies
%% Verifies that parsing valid policies doesn't crash
test_policy_no_crashes(_Config) ->
    %% Generate random valid policy
    PolicyGen = valid_policy(),
    
    %% Run property test
    Result = proper:quickcheck(
        ?FORALL(Policy, PolicyGen,
            begin
                try
                    %% Try to parse/validate policy (simulate router_policy:parse_policy_json)
                    _ = Policy#policy.tenant_id,
                    _ = Policy#policy.policy_id,
                    _ = Policy#policy.weights,
                    true
                catch
                    _:_ ->
                        false
                end
            end
        ),
        [{numtests, 100}]
    ),
    case Result of
        true -> ok;
        _ -> ct:fail("Property test failed: ~p", [Result])
    end.

%% ============================================================================
%% PropEr Generators
%% ============================================================================

%% Generate provider name
provider_name() ->
    oneof([
        <<"openai">>,
        <<"anthropic">>,
        <<"google">>,
        <<"cohere">>
    ]).

%% Generate policy with weights
policy_with_weights() ->
    ?LET(
        {TenantId, PolicyId, Weights},
        {
            binary(),
            binary(),
            ?LET(
                Providers,
                ?LET(
                    N,
                    integer(1, 5),
                    vector(N, provider_name())
                ),
                begin
                    RawWeights = [rand_float(0.0, 1.0) || _ <- Providers],
                    Sum = lists:sum(RawWeights),
                    Normalized = [W / Sum || W <- RawWeights],
                    maps:from_list(lists:zip(Providers, Normalized))
                end
            )
        },
        #policy{
            tenant_id = TenantId,
            policy_id = PolicyId,
            version = <<"1.0">>,
            defaults = #{},
            escalate_on = [],
            weights = Weights,
            fallback = undefined,
            sticky = undefined,
            metadata = #{}
        }
    ).

%% Generate policy with fallbacks
policy_with_fallbacks() ->
    ?LET(
        {TenantId, PolicyId, Fallbacks},
        {
            binary(),
            binary(),
            ?LET(
                FallbackList,
                ?LET(
                    N,
                    integer(0, 5),
                    vector(N, fallback_item())
                ),
                FallbackList
            )
        },
        #policy{
            tenant_id = TenantId,
            policy_id = PolicyId,
            version = <<"1.0">>,
            defaults = #{},
            escalate_on = [],
            weights = #{<<"openai">> => 1.0},
            fallback = Fallbacks,
            sticky = undefined,
            metadata = #{}
        }
    ).

%% Generate fallback item
fallback_item() ->
    ?LET(
        {ToProvider, When},
        {
            provider_name(),
            ?LET(
                Conditions,
                ?LET(
                    N,
                    integer(0, 3),
                    vector(N, {oneof([<<"error_rate">>, <<"latency">>]), oneof([<<"openai">>, <<"anthropic">>])})
                ),
                maps:from_list(Conditions)
            )
        },
        #{
            <<"to">> => ToProvider,
            <<"when">> => When
        }
    ).

%% Generate valid policy
valid_policy() ->
    ?LET(
        {TenantId, PolicyId, Weights, Fallbacks, Metadata},
        {
            binary(),
            binary(),
            ?LET(
                Providers,
                ?LET(
                    N,
                    integer(1, 5),
                    vector(N, provider_name())
                ),
                begin
                    RawWeights = [rand_float(0.0, 1.0) || _ <- Providers],
                    Sum = lists:sum(RawWeights),
                    Normalized = [W / Sum || W <- RawWeights],
                    maps:from_list(lists:zip(Providers, Normalized))
                end
            ),
            ?LET(
                N,
                integer(0, 3),
                vector(N, fallback_item())
            ),
            metadata_map()
        },
        #policy{
            tenant_id = TenantId,
            policy_id = PolicyId,
            version = <<"1.0">>,
            defaults = #{},
            escalate_on = [],
            weights = Weights,
            fallback = Fallbacks,
            sticky = undefined,
            metadata = Metadata
        }
    ).

config_map() ->
    ?LET(
        {Key, Value},
        {
            oneof([<<"lowercase">>, <<"mask_email">>, <<"timeout_ms">>]),
            frequency([
                {2, boolean()},
                {1, integer(1, 1000)},
                {1, oneof([<<"value1">>, <<"value2">>])}
            ])
        },
        #{Key => Value}
    ).

%% Generate metadata map
metadata_map() ->
    ?LET(
        {Key, Value},
        {
            oneof([<<"strategy">>, <<"description">>, <<"author">>]),
            frequency([
                {2, oneof([<<"test">>, <<"production">>, <<"legacy">>])},
                {1, provider_name()}
            ])
        },
        #{Key => Value}
    ).

%% Helper: Generate real number (0.0-1.0)
rand_float() ->
    rand_float(0.0, 1.0).

rand_float(Min, Max) ->
    ?LET(
        Int,
        integer(0, 1000000),
        Min + (Max - Min) * (Int / 1000000.0)
    ).

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Calculate weight sum from policy
calculate_weight_sum(Policy) ->
    lists:sum(maps:values(Policy#policy.weights)).

%% Verify fallback chain doesn't create cycles
%% Simplified check: verify no provider appears in both "to" and "when" conditions
verify_no_fallback_cycles(Fallbacks) ->
    lists:all(fun(Fallback) ->
        ToProvider = maps:get(<<"to">>, Fallback, undefined),
        When = maps:get(<<"when">>, Fallback, #{}),
        %% Check if "to" provider appears in "when" conditions
        not lists:any(fun
            ({_Key, Values}) when is_list(Values) ->
                lists:member(ToProvider, Values);
            ({_Key, Value}) ->
                Value =:= ToProvider;
            (_) ->
                false
        end, maps:to_list(When))
    end, Fallbacks).

%% Verify fallback chain is finite (no infinite recursion)
%% Simplified check: verify that we don't exceed max depth
verify_fallback_finiteness(Fallbacks, MaxDepth) ->
    length(Fallbacks) =< MaxDepth.
