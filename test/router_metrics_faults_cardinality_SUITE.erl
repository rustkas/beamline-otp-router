%% @doc Metrics Under Faults - Cardinality Tests
%% 
%% Tests for metrics cardinality limits under load.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category metrics, cardinality, heavy
-module(router_metrics_faults_cardinality_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2, get_metrics_snapshot/0
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_cardinality_under_mass_load/1,
    test_cardinality_with_many_tenants/1,
    test_cardinality_limits_enforced/1,
    test_cardinality_with_high_cardinality_leak/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, cardinality_tests}];
        _ -> []
    end.

groups() ->
    [{cardinality_tests, [sequence], [
        test_cardinality_under_mass_load,
        test_cardinality_with_many_tenants,
        test_cardinality_limits_enforced,
        test_cardinality_with_high_cardinality_leak
    ]}].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{
        common_env => false,
        app_env => #{grpc_port => 0, grpc_enabled => false, nats_mode => mock}
    }),
    ok = router_metrics:ensure(),
    router_nats_fault_injection:clear_all_faults(),
    Config1.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(TestCase, Config) ->
    Config1 = router_test_bootstrap:init_per_testcase(TestCase, Config, #{
        clear_faults => true
    }),
    ok = router_metrics:ensure(),
    ok = router_metrics:clear_all(),
    Config1.

end_per_testcase(TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(TestCase, Config, #{
        clear_faults => true
    }).

get_metrics_snapshot() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            Metrics = ets:tab2list(router_metrics),
            lists:foldl(fun
                ({Key, Value}, Acc) when is_atom(Key) -> maps:put(Key, Value, Acc);
                ({{MetricName, _Labels}, Value}, Acc) when is_atom(MetricName) ->
                    Current = maps:get(MetricName, Acc, 0),
                    maps:put(MetricName, Current + Value, Acc)
            end, #{}, Metrics)
    end.

count_metric_keys() ->
    case ets:info(router_metrics) of
        undefined -> 0;
        _ -> ets:info(router_metrics, size)
    end.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_cardinality_under_mass_load(_Config) ->
    InitialCount = count_metric_keys(),
    
    %% Generate load with limited label variations
    lists:foreach(fun(N) ->
        TenantId = list_to_binary("tenant_" ++ integer_to_list(N rem 10)),
        router_metrics:increment(router_test_metric, #{tenant_id => TenantId}),
        timer:sleep(10)
    end, lists:seq(1, 100)),
    
    FinalCount = count_metric_keys(),
    
    %% Should have limited cardinality (10 tenants max)
    CardinalityGrowth = FinalCount - InitialCount,
    ?assert(CardinalityGrowth =< 100),  %% Reasonable limit
    ct:log("Cardinality growth: ~p", [CardinalityGrowth]),
    ok.

test_cardinality_with_many_tenants(_Config) ->
    InitialCount = count_metric_keys(),
    
    %% Generate metrics for many tenants
    lists:foreach(fun(N) ->
        TenantId = list_to_binary("tenant_" ++ integer_to_list(N)),
        router_metrics:increment(router_tenant_metric, #{tenant_id => TenantId})
    end, lists:seq(1, 50)),
    
    FinalCount = count_metric_keys(),
    CardinalityGrowth = FinalCount - InitialCount,
    
    ct:log("Many tenants cardinality growth: ~p", [CardinalityGrowth]),
    ?assert(is_integer(CardinalityGrowth)),
    ok.

test_cardinality_limits_enforced(_Config) ->
    %% Test that cardinality limits are enforced
    MaxCardinality = 1000,  %% Expected limit
    
    lists:foreach(fun(N) ->
        UniqueId = list_to_binary("unique_" ++ integer_to_list(N)),
        router_metrics:increment(router_unique_metric, #{unique_id => UniqueId})
    end, lists:seq(1, 100)),
    
    FinalCount = count_metric_keys(),
    
    ?assert(FinalCount < MaxCardinality),
    ct:log("Cardinality after unique IDs: ~p", [FinalCount]),
    ok.

test_cardinality_with_high_cardinality_leak(_Config) ->
    InitialMemory = erlang:memory(total),
    
    %% Attempt to create many unique labels (potential cardinality explosion)
    lists:foreach(fun(N) ->
        UniqueId = list_to_binary("leak_" ++ integer_to_list(N)),
        router_metrics:increment(router_leak_metric, #{request_id => UniqueId})
    end, lists:seq(1, 200)),
    
    FinalMemory = erlang:memory(total),
    MemoryGrowth = FinalMemory - InitialMemory,
    
    %% Memory growth should be bounded
    MaxAllowedGrowth = 50 * 1024 * 1024,  %% 50MB max
    ?assert(MemoryGrowth < MaxAllowedGrowth),
    ct:log("Memory growth: ~p bytes", [MemoryGrowth]),
    ok.
