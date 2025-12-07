%% @doc Property-based tests for router_decider
%% Tests weighted distribution, fallback behavior, and statistical properties
%% @test_category property, combinatorial
-module(router_decider_prop_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Always include proper.hrl in test profile (PropEr is available in test profile)
%% Runtime check for PropEr availability is done in prop_* functions
%% Note: eunit.hrl is included after proper.hrl to avoid LET macro redefinition error
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    provider_id/0,
    weight/0,
    calculate_distribution/2,
    test_weighted_distribution/1,
    test_fallback_behavior/1,
    test_statistical_properties/1,
    rand_float/2
]}).

%% Test suite configuration
all() ->
    [
        {group, property_tests}
    ].

groups() ->
    [
        {property_tests, [parallel], [
            test_weighted_distribution,
            test_fallback_behavior,
            test_statistical_properties
        ]}
    ].

init_per_suite(Config) ->
    %% Check if PropEr is available
    case code:which(proper) of
        non_existing ->
            ct:comment("PropEr not available, skipping property tests"),
            {skip, "PropEr not available"};
        _ ->
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
            end
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% PropEr generators

provider_id() ->
    oneof([<<"openai">>, <<"anthropic">>, <<"cohere">>, <<"fallback">>]).

weight() ->
    ?LET(
        W,
        real(),
        max(0.0, min(1.0, abs(W)))
    ).

%% Helpers

calculate_distribution(Results, Providers) ->
    lists:foldl(fun(Provider, Acc) ->
        Count = length([R || R <- Results, R =:= Provider]),
        maps:put(Provider, Count, Acc)
    end, #{}, Providers).

%% Property tests

%% @doc Test: Weighted distribution matches expected weights
test_weighted_distribution(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            ?assert(proper:quickcheck(
                ?FORALL(
                    {Providers, Weights},
                    {
                        ?LET(
                            N,
                            integer(2, 5),
                            vector(N, provider_id())
                        ),
                        ?LET(
                            Providers,
                            ?LET(
                                N,
                                integer(2, 5),
                                vector(N, provider_id())
                            ),
                            begin
                                RawWeights = [rand_float(0.0, 1.0) || _ <- Providers],
                                Sum = lists:sum(RawWeights),
                                Normalized = [W / Sum || W <- RawWeights],
                                maps:from_list(lists:zip(Providers, Normalized))
                            end
                        )
                    },
                    begin
                        Policy = #policy{
                            tenant_id = <<"test_tenant">>,
                            policy_id = <<"test_policy">>,
                            version = <<"1.0">>,
                            weights = Weights,
                            defaults = #{},
                            escalate_on = [],
                            fallback = undefined,
                            sticky = undefined,
                            metadata = #{}
                        },
                        %% Generate decisions
                        Results = [begin
                            {ok, Decision} = router_decider:decide(
                                #route_request{
                                    message = #{<<"test">> => <<"message">>},
                                    policy_id = <<"test_policy">>,
                                    context = #{}
                                },
                                Policy,
                                #{}
                            ),
                            Decision#route_decision.provider_id
                        end || _ <- lists:seq(1, 1000)],
                        %% Calculate distribution
                        Distribution = calculate_distribution(Results, Providers),
                        %% Verify distribution is close to expected weights
                        lists:all(fun(Provider) ->
                            Expected = maps:get(Provider, Weights, 0.0),
                            Actual = maps:get(Provider, Distribution, 0) / 1000.0,
                            abs(Expected - Actual) < 0.1  %% Allow 10% variance
                        end, Providers)
                    end
                ),
                [{numtests, 10}]
            ))
    end.

%% @doc Test: Fallback behavior when primary providers fail
test_fallback_behavior(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            ?assert(proper:quickcheck(
                ?FORALL(
                    {PrimaryProvider, FallbackProvider},
                    {provider_id(), provider_id()},
                    begin
                        Policy = #policy{
                            tenant_id = <<"test_tenant">>,
                            policy_id = <<"test_policy">>,
                            version = <<"1.0">>,
                            weights = #{PrimaryProvider => 1.0},
                            defaults = #{},
                            escalate_on = [],
                            fallback = #{
                                <<"provider_id">> => FallbackProvider,
                                <<"reason">> => <<"fallback">>
                            },
                            sticky = undefined,
                            metadata = #{}
                        },
                        %% Mock primary provider failure
                        meck:new(router_provider, [passthrough]),
                        meck:expect(router_provider, check_health, fun(P) ->
                            case P =:= PrimaryProvider of
                                true -> {error, unavailable};
                                false -> {ok, healthy}
                            end
                        end),
                        {ok, Decision} = router_decider:decide(
                            #route_request{
                                message = #{<<"test">> => <<"message">>},
                                policy_id = <<"test_policy">>,
                                context = #{}
                            },
                            Policy,
                            #{}
                        ),
                        meck:unload(router_provider),
                        Decision#route_decision.provider_id =:= FallbackProvider
                    end
                ),
                [{numtests, 10}]
            ))
    end.

%% @doc Test: Statistical properties of decision distribution
test_statistical_properties(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            ?assert(proper:quickcheck(
                ?FORALL(
                    Weights,
                    ?LET(
                        Providers,
                        ?LET(
                            N,
                            integer(2, 5),
                            vector(N, provider_id())
                        ),
                        begin
                            RawWeights = [rand_float(0.0, 1.0) || _ <- Providers],
                            Sum = lists:sum(RawWeights),
                            Normalized = [W / Sum || W <- RawWeights],
                            maps:from_list(lists:zip(Providers, Normalized))
                        end
                    ),
                    begin
                        Policy = #policy{
                            tenant_id = <<"test_tenant">>,
                            policy_id = <<"test_policy">>,
                            version = <<"1.0">>,
                            weights = Weights,
                            defaults = #{},
                            escalate_on = [],
                            fallback = undefined,
                            sticky = undefined,
                            metadata = #{}
                        },
                        %% Generate many decisions
                        Results = [begin
                            {ok, Decision} = router_decider:decide(
                                #route_request{
                                    message = #{<<"test">> => <<"message">>},
                                    policy_id = <<"test_policy">>,
                                    context = #{}
                                },
                                Policy,
                                #{}
                            ),
                            Decision#route_decision.provider_id
                        end || _ <- lists:seq(1, 10000)],
                        %% Verify all providers appear in results
                        Providers = maps:keys(Weights),
                        lists:all(fun(Provider) ->
                            lists:member(Provider, Results)
                        end, Providers)
                    end
                ),
                [{numtests, 10}]
            ))
    end.

%% Helper function for random float generation
rand_float(Min, Max) ->
    Min + (Max - Min) * (rand:uniform()).
