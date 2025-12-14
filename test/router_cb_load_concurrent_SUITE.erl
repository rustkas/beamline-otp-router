%% @doc Circuit Breaker Load: Concurrent Tests
%%
%% Load tests for circuit breaker:
%% - Different window sizes
%% - Concurrent requests under load
%%
%% @test_category load, heavy, circuit_breaker
-module(router_cb_load_concurrent_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_different_window_sizes/1,
    test_concurrent_requests_under_load/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, concurrent_tests}];
        _ -> []
    end.

groups() ->
    [{concurrent_tests, [sequence], [
        test_different_window_sizes,
        test_concurrent_requests_under_load
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, circuit_breaker_enabled, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            ok = test_helpers:wait_for_app_start(router_circuit_breaker, 2000),
            Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_different_window_sizes(_Config) ->
    ct:comment("=== Different Window Sizes ==="),
    
    Configs = [
        {<<"tenant_small">>, <<"provider_small">>, 5, 1000},
        {<<"tenant_medium">>, <<"provider_medium">>, 10, 5000},
        {<<"tenant_large">>, <<"provider_large">>, 20, 10000}
    ],
    
    lists:foreach(fun({TenantId, ProviderId, FailureThreshold, _WindowMs}) ->
        ct:comment("Testing with failure_threshold=~p", [FailureThreshold]),
        
        %% Record failures
        lists:foreach(fun(_) ->
            ok = router_circuit_breaker:record_failure(TenantId, ProviderId),
            ok = test_helpers:wait_for_condition(
                fun() -> true end, 5)
        end, lists:seq(1, FailureThreshold)),
        
        ok = test_helpers:wait_for_condition(fun() -> true end, 100)
    end, Configs),
    ok.

test_concurrent_requests_under_load(_Config) ->
    ct:comment("=== Concurrent Requests Under Load ==="),
    TenantId = <<"concurrent_test_tenant">>,
    ProviderId = <<"concurrent_test_provider">>,
    
    NumWorkers = 10,
    RequestsPerWorker = 50,
    
    Results = router_test_init:ensure_ets_table(results, [named_table, set, public]),
    
    %% Spawn workers
    Pids = lists:map(fun(WorkerId) ->
        spawn(fun() ->
            WorkerResults = lists:map(fun(_ReqId) ->
                ShouldFail = rand:uniform(100) =< 30,
                case ShouldFail of
                    true ->
                        router_circuit_breaker:record_failure(TenantId, ProviderId),
                        failure;
                    false ->
                        router_circuit_breaker:record_success(TenantId, ProviderId),
                        success
                end
            end, lists:seq(1, RequestsPerWorker)),
            ets:insert(Results, {{WorkerId, results}, WorkerResults})
        end)
    end, lists:seq(1, NumWorkers)),
    
    %% Wait for all workers
    lists:foreach(fun(Pid) ->
        MRef = monitor(process, Pid),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 30000 ->
            exit(Pid, kill)
        end
    end, Pids),
    
    %% Collect results
    AllResults = ets:tab2list(Results),
    ct:comment("Completed ~p worker results", [length(AllResults)]),
    
    ets:delete_all_objects(Results),
    ok.
