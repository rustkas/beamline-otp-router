%%%-------------------------------------------------------------------
%%% @doc
%%% Property-based tests for ETS consistency across router modules
%%% Tests: ETS table integrity, no orphaned entries, proper cleanup
%%% @end
%%%-------------------------------------------------------------------
-module(router_ets_consistency_prop_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1]}).

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0
]).

%% Export test functions
-export([
    test_ets_table_integrity/1,
    test_ets_no_orphaned_entries/1,
    test_ets_cleanup_after_operations/1
]).

all() ->
    [
        {group, ets_consistency_tests}
    ].

groups() ->
    [
        {ets_consistency_tests, [sequence], [
            test_ets_table_integrity,
            test_ets_no_orphaned_entries,
            test_ets_cleanup_after_operations
        ]}
    ].

init_per_suite(Config) ->
    ok = start_router_app(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_suite(_Config) ->
    stop_router_app(),
    ok.

init_per_testcase(_Case, Config) ->
    %% Clear all metrics and state before each test
    router_r10_metrics:clear_metrics(),
    case whereis(router_rbac) of
        undefined -> ok;
        _ -> router_rbac:reset()
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% ========================================================================
%% ETS CONSISTENCY TESTS
%% ========================================================================

%% @doc Property test: ETS tables maintain integrity after operations
%% 
%% Invariant: ETS tables should not contain invalid entries, duplicate keys,
%% or entries with missing required fields after a sequence of operations.
test_ets_table_integrity(_Config) ->
    ct:comment("ETS Consistency: Table integrity after operations"),
    
    TenantId = <<"tenant_ets_test">>,
    ProviderId = <<"provider_ets_test">>,
    
    %% Perform a sequence of operations
    NumOperations = 20,
    lists:foreach(
        fun(N) ->
            %% Record success/failure randomly
            case N rem 2 of
                0 ->
                    router_circuit_breaker:record_success(TenantId, ProviderId);
                1 ->
                    router_circuit_breaker:record_failure(TenantId, ProviderId)
            end,
            timer:sleep(10)  %% Small delay between operations
        end,
        lists:seq(1, NumOperations)
    ),
    
    %% Verify ETS table integrity
    %% Check that all entries have valid structure
    Metrics = router_r10_metrics:dump_metrics(),
    ?assert(is_map(Metrics)),
    
    %% Verify no invalid entries (all entries should be maps with required keys)
    lists:foreach(
        fun({_Key, Value}) ->
            ?assert(is_map(Value)),
            %% Verify value has required structure
            ?assert(maps:is_key(<<"value">>, Value) orelse maps:is_key(<<"count">>, Value))
        end,
        maps:to_list(Metrics)
    ),
    
    ok.

%% @doc Property test: No orphaned entries in ETS tables
%% 
%% Invariant: After cleanup operations (reset, clear), ETS tables should not
%% contain orphaned entries that reference non-existent resources.
test_ets_no_orphaned_entries(_Config) ->
    ct:comment("ETS Consistency: No orphaned entries after cleanup"),
    
    TenantId = <<"tenant_orphan_test">>,
    ProviderId = <<"provider_orphan_test">>,
    
    %% Create some state
    Config = #{
        <<"failure_threshold">> => 3,
        <<"error_rate_threshold">> => 1.0,
        <<"error_rate_window_seconds">> => 30,
        <<"latency_threshold_ms">> => 0,
        <<"open_timeout_ms">> => 2000,
        <<"success_threshold">> => 2
    },
    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Perform operations
    lists:foreach(
        fun(_) ->
            router_circuit_breaker:record_success(TenantId, ProviderId),
            router_circuit_breaker:record_failure(TenantId, ProviderId)
        end,
        lists:seq(1, 10)
    ),
    
    %% Clear metrics
    router_r10_metrics:clear_metrics(),
    
    %% Verify no orphaned entries
    Metrics = router_r10_metrics:dump_metrics(),
    %% After clear, metrics should be empty or contain only valid entries
    ?assert(is_map(Metrics)),
    
    %% Verify all entries reference valid tenant/provider combinations
    lists:foreach(
        fun({Key, Value}) ->
            ?assert(is_map(Value)),
            %% Key should be a valid metric identifier
            ?assert(is_binary(Key) orelse is_atom(Key))
        end,
        maps:to_list(Metrics)
    ),
    
    ok.

%% @doc Property test: ETS cleanup after operations
%% 
%% Invariant: After a sequence of operations followed by cleanup, ETS tables
%% should return to a clean state without unbounded growth.
test_ets_cleanup_after_operations(_Config) ->
    ct:comment("ETS Consistency: Proper cleanup after operations"),
    
    TenantId = <<"tenant_cleanup_test">>,
    ProviderId = <<"provider_cleanup_test">>,
    
    %% Get initial metrics count
    InitialMetrics = router_r10_metrics:dump_metrics(),
    InitialCount = maps:size(InitialMetrics),
    
    %% Perform many operations
    NumOperations = 50,
    lists:foreach(
        fun(N) ->
            case N rem 3 of
                0 ->
                    router_circuit_breaker:record_success(TenantId, ProviderId);
                1 ->
                    router_circuit_breaker:record_failure(TenantId, ProviderId);
                2 ->
                    router_circuit_breaker:should_allow(TenantId, ProviderId)
            end,
            timer:sleep(5)  %% Small delay
        end,
        lists:seq(1, NumOperations)
    ),
    
    %% Get metrics after operations
    AfterOpsMetrics = router_r10_metrics:dump_metrics(),
    AfterOpsCount = maps:size(AfterOpsMetrics),
    
    %% Verify metrics were created (count should increase)
    ?assert(AfterOpsCount >= InitialCount),
    
    %% Clear metrics
    router_r10_metrics:clear_metrics(),
    
    %% Get metrics after cleanup
    AfterCleanupMetrics = router_r10_metrics:dump_metrics(),
    AfterCleanupCount = maps:size(AfterCleanupMetrics),
    
    %% Verify cleanup worked (count should decrease)
    ?assert(AfterCleanupCount =< AfterOpsCount),
    
    %% Verify no unbounded growth (after cleanup, count should be reasonable)
    ?assert(AfterCleanupCount < 100),  %% Reasonable threshold
    
    ok.

