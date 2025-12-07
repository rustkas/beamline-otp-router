%% @doc Property-based tests for normalize_boolean
-module(router_normalize_boolean_prop_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Always include proper.hrl in test profile (PropEr is available in test profile)
%% Runtime check for PropEr availability is done in prop_* functions
%% Note: eunit.hrl is included after proper.hrl to avoid LET macro redefinition error
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    prop_normalize_boolean_boolean_skip/1,
    prop_normalize_boolean_binary_skip/1,
    prop_normalize_boolean_integer_skip/1,
    prop_normalize_boolean_unknown_skip/1,
    prop_normalize_boolean_boolean/1,
    prop_normalize_boolean_binary/1,
    prop_normalize_boolean_integer/1,
    prop_normalize_boolean_unknown/1
]}).


all() ->
    [
        {group, property_tests}
    ].

groups() ->
    case code:which(proper) of
        non_existing -> [
            {property_tests, [parallel], [
                prop_normalize_boolean_boolean_skip,
                prop_normalize_boolean_binary_skip,
                prop_normalize_boolean_integer_skip,
                prop_normalize_boolean_unknown_skip
            ]}
        ];
        _ -> [
            {property_tests, [parallel], [
                prop_normalize_boolean_boolean,
                prop_normalize_boolean_binary,
                prop_normalize_boolean_integer,
                prop_normalize_boolean_unknown
            ]}
        ]
    end.

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    %% Disable heir/transfer logic for faster test execution
    ok = application:set_env(beamline_router, disable_heir, true),
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

%% Skip tests if PropEr not available
prop_normalize_boolean_boolean_skip(_Config) ->
    {skip, "PropEr not available"}.
prop_normalize_boolean_binary_skip(_Config) ->
    {skip, "PropEr not available"}.
prop_normalize_boolean_integer_skip(_Config) ->
    {skip, "PropEr not available"}.
prop_normalize_boolean_unknown_skip(_Config) ->
    {skip, "PropEr not available"}.

%% Property: Boolean values normalize correctly
prop_normalize_boolean_boolean(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            Prop = ?FORALL(Value, boolean(),
                begin
                    Result = router_nats_subscriber:normalize_boolean(Value),
                    Result =:= Value
                end
            ),
            Options = test_helpers:get_proper_options(),
            Result = proper:quickcheck(Prop, Options),
            case Result of
                true -> 
                    ok;
                false ->
                    ct:fail("Property failed");
                {false, CounterExample} ->
                    ct:fail("Property failed with counterexample: ~p", [CounterExample]);
                Other ->
                    ct:fail("Unexpected result from proper:quickcheck: ~p", [Other])
            end
    end.

%% Property: Binary "true"/"false" normalize correctly
prop_normalize_boolean_binary(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            Prop = ?FORALL(Value, oneof([<<"true">>, <<"false">>, <<"TRUE">>, <<"FALSE">>, <<"True">>, <<"False">>]),
                begin
                    Result = router_nats_subscriber:normalize_boolean(Value),
                    case Value of
                        <<"true">> -> Result =:= true;
                        <<"false">> -> Result =:= false;
                        _ -> Result =:= false  %% Case-sensitive, only lowercase works
                    end
                end
            ),
            Options = test_helpers:get_proper_options(),
            Result = proper:quickcheck(Prop, Options),
            case Result of
                true -> 
                    ok;
                false ->
                    ct:fail("Property failed");
                {false, CounterExample} ->
                    ct:fail("Property failed with counterexample: ~p", [CounterExample]);
                Other ->
                    ct:fail("Unexpected result from proper:quickcheck: ~p", [Other])
            end
    end.

%% Property: Integer 0/1 normalize correctly
prop_normalize_boolean_integer(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            Prop = ?FORALL(Value, oneof([0, 1]),
                begin
                    Result = router_nats_subscriber:normalize_boolean(Value),
                    case Value of
                        0 -> Result =:= false;
                        1 -> Result =:= true
                    end
                end
            ),
            Options = test_helpers:get_proper_options(),
            Result = proper:quickcheck(Prop, Options),
            case Result of
                true -> 
                    ok;
                false ->
                    ct:fail("Property failed");
                {false, CounterExample} ->
                    ct:fail("Property failed with counterexample: ~p", [CounterExample]);
                Other ->
                    ct:fail("Unexpected result from proper:quickcheck: ~p", [Other])
            end
    end.

%% Property: Unknown values default to false
prop_normalize_boolean_unknown(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            %% Generate values that are NOT known boolean values
            Prop = ?FORALL(Value, ?SUCHTHAT(_, oneof([
                ?SUCHTHAT(A, atom(), A =/= true andalso A =/= false),
                ?SUCHTHAT(I, integer(), I =/= 1 andalso I =/= 0),
                ?SUCHTHAT(B, binary(), B =/= <<"true">> andalso B =/= <<"false">>),
                list(),
                float()
            ]), true),
                begin
                    Result = router_nats_subscriber:normalize_boolean(Value),
                    %% All unknown values should return false
                    Result =:= false
                end
            ),
            Options = test_helpers:get_proper_options(),
            Result = proper:quickcheck(Prop, Options),
            case Result of
                true -> 
                    ok;
                false ->
                    ct:fail("Property failed");
                {false, CounterExample} ->
                    ct:fail("Property failed with counterexample: ~p", [CounterExample]);
                Other ->
                    ct:fail("Unexpected result from proper:quickcheck: ~p", [Other])
            end
    end.
