-module(router_cp2_conditions_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [
        test_equality,
        test_inequality,
        test_comparisons,
        test_inclusion,
        test_complex_conditions,
        test_undefined_vars
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

test_equality(_Config) ->
    Context = #{
        <<"user">> => #{<<"tier">> => <<"premium">>},
        <<"count">> => 10
    },
    
    %% Literal match
    true = router_decider:evaluate_when_condition(#{<<"==">> => [#{<<"var">> => <<"context.user.tier">>}, <<"premium">>]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"==">> => [#{<<"var">> => <<"context.user.tier">>}, <<"basic">>]}, Context),
    
    %% Numeric match
    true = router_decider:evaluate_when_condition(#{<<"==">> => [#{<<"var">> => <<"context.count">>}, 10]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"==">> => [#{<<"var">> => <<"context.count">>}, 20]}, Context),
    
    ok.

test_inequality(_Config) ->
    Context = #{
        <<"user">> => #{<<"tier">> => <<"basic">>}
    },
    
    true = router_decider:evaluate_when_condition(#{<<"!=">> => [#{<<"var">> => <<"context.user.tier">>}, <<"premium">>]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"!=">> => [#{<<"var">> => <<"context.user.tier">>}, <<"basic">>]}, Context),
    
    ok.

test_comparisons(_Config) ->
    Context = #{
        <<"age">> => 25,
        <<"score">> => 100.5
    },
    
    %% Greater than
    true = router_decider:evaluate_when_condition(#{<<">">> => [#{<<"var">> => <<"context.age">>}, 18]}, Context),
    false = router_decider:evaluate_when_condition(#{<<">">> => [#{<<"var">> => <<"context.age">>}, 30]}, Context),
    
    %% Less than
    true = router_decider:evaluate_when_condition(#{<<"<">> => [#{<<"var">> => <<"context.age">>}, 30]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"<">> => [#{<<"var">> => <<"context.age">>}, 20]}, Context),
    
    %% Greater or equal
    true = router_decider:evaluate_when_condition(#{<<">=">> => [#{<<"var">> => <<"context.age">>}, 25]}, Context),
    true = router_decider:evaluate_when_condition(#{<<">=">> => [#{<<"var">> => <<"context.age">>}, 18]}, Context),
    false = router_decider:evaluate_when_condition(#{<<">=">> => [#{<<"var">> => <<"context.age">>}, 26]}, Context),
    
    %% Less or equal
    true = router_decider:evaluate_when_condition(#{<<"<=">> => [#{<<"var">> => <<"context.age">>}, 25]}, Context),
    true = router_decider:evaluate_when_condition(#{<<"<=">> => [#{<<"var">> => <<"context.age">>}, 30]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"<=">> => [#{<<"var">> => <<"context.age">>}, 24]}, Context),
    
    ok.

test_inclusion(_Config) ->
    Context = #{
        <<"role">> => <<"admin">>,
        <<"groups">> => [<<"staff">>, <<"editor">>]
    },
    
    %% Element in list (literal list)
    true = router_decider:evaluate_when_condition(#{<<"in">> => [#{<<"var">> => <<"context.role">>}, [<<"user">>, <<"admin">>]]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"in">> => [#{<<"var">> => <<"context.role">>}, [<<"user">>, <<"guest">>]]}, Context),
    
    ok.

test_complex_conditions(_Config) ->
    Context = #{
        <<"user">> => #{<<"tier">> => <<"premium">>},
        <<"age">> => 30
    },
    
    %% Multiple conditions (implicit AND)
    Condition = #{
        <<"==">> => [#{<<"var">> => <<"context.user.tier">>}, <<"premium">>],
        <<">">> => [#{<<"var">> => <<"context.age">>}, 18]
    },
    
    true = router_decider:evaluate_when_condition(Condition, Context),
    
    %% One fails
    ConditionFail = #{
        <<"==">> => [#{<<"var">> => <<"context.user.tier">>}, <<"premium">>],
        <<">">> => [#{<<"var">> => <<"context.age">>}, 50]
    },
    
    false = router_decider:evaluate_when_condition(ConditionFail, Context),
    
    ok.

test_undefined_vars(_Config) ->
    Context = #{
        <<"existing">> => 1
    },
    
    %% Undefined var should result in false for comparisons
    false = router_decider:evaluate_when_condition(#{<<"==">> => [#{<<"var">> => <<"context.missing">>}, 1]}, Context),
    false = router_decider:evaluate_when_condition(#{<<"!=">> => [#{<<"var">> => <<"context.missing">>}, 1]}, Context),
    false = router_decider:evaluate_when_condition(#{<<">">> => [#{<<"var">> => <<"context.missing">>}, 0]}, Context),
    
    ok.
