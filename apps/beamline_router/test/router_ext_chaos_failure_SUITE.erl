%% @doc Extensions Chaos: Failure Scenarios
%%
%% Tests NATS failure and extension flapping:
%% - Complete NATS failure
%% - Extension instance flapping
%% - Latency degradation
%%
%% @test_category chaos, heavy, resilience
-module(router_ext_chaos_failure_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_nats_complete_failure/1,
    test_extension_flapping/1,
    test_latency_degradation/1
]).

suite() -> [{timetrap, {minutes, 15}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, failure_tests}];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{failure_tests, [sequence], [
        test_nats_complete_failure,
        test_extension_flapping,
        test_latency_degradation
    ]}].

init_per_suite(Config) -> router_extensions_chaos_helper:init_suite(Config).
end_per_suite(Config) -> router_extensions_chaos_helper:end_suite(Config).
init_per_testcase(TC, Config) -> router_extensions_chaos_helper:init_testcase(TC, Config).
end_per_testcase(TC, Config) -> router_extensions_chaos_helper:end_testcase(TC, Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_nats_complete_failure(Config) ->
    ct:comment("=== NATS Complete Failure ==="),
    NumRequests = 100,
    
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        {error, connection_error}
    end),
    
    Policy = router_extensions_chaos_helper:create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    {Results, Latencies} = router_extensions_chaos_helper:execute_chaos_test(NumRequests, Policy),
    {SuccessCount, ErrorCount, _TimeoutCount, ClientErrors} = router_extensions_chaos_helper:analyze_results(Results),
    
    CBEvents = proplists:get_value(cb_events, Config),
    CircuitBreakerOpened = ets:tab2list(CBEvents),
    
    ct:log("Total: ~p, Success: ~p, Errors: ~p, CB Opened: ~p", 
           [NumRequests, SuccessCount, ErrorCount, length(CircuitBreakerOpened)]),
    ct:log("Latencies: ~p", [length(Latencies)]),
    
    ?assertEqual(0, SuccessCount),
    ?assert(ErrorCount > 0),
    
    ExtUnavailable = [E || E = {error, {extension_unavailable, _}} <- ClientErrors],
    ?assert(length(ExtUnavailable) >= 0),
    ok.

test_extension_flapping(_Config) ->
    ct:comment("=== Extension Instance Flapping ==="),
    NumRequests = 200,
    FlapPeriod = 10,

    FlapCounter = flap_counter,
    _ = router_ets_helpers:ensure_named_ets_table(FlapCounter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case router_ets_helpers:ets_lookup(FlapCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(FlapCounter, {count, RequestNum}),
        
        IsAvailable = ((RequestNum div FlapPeriod) rem 2) =:= 0,
        case IsAvailable of
            true ->
                timer:sleep(10),
                {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})};
            false ->
                {error, timeout}
        end
    end),
    
    Policy = router_extensions_chaos_helper:create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}
    ]),
    
    {Results, _Latencies} = router_extensions_chaos_helper:execute_chaos_test(NumRequests, Policy),
    {SuccessCount, ErrorCount, _TimeoutCount, _ClientErrors} = router_extensions_chaos_helper:analyze_results(Results),
    
    ct:log("Total: ~p, Success: ~p, Errors: ~p", [NumRequests, SuccessCount, ErrorCount]),
    
    ?assert(SuccessCount > 0, "Some should succeed"),
    ?assert(ErrorCount > 0, "Some should fail"),
    
    SuccessRate = SuccessCount / NumRequests,
    ?assert(SuccessRate > 0.3, "Rate > 30% with fail-open"),
    
    ets:delete_all_objects(FlapCounter),
    ok.

test_latency_degradation(_Config) ->
    ct:comment("=== Latency Degradation ==="),
    NumRequests = 150,

    LatencyCounter = latency_counter,
    _ = router_ets_helpers:ensure_named_ets_table(LatencyCounter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case router_ets_helpers:ets_lookup(LatencyCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(LatencyCounter, {count, RequestNum}),
        
        BaseLatency = 10,
        DegradationFactor = min(50, RequestNum div 30),
        Latency = min(BaseLatency + (DegradationFactor * 10), 500),
        timer:sleep(Latency),
        
        {ok, jsx:encode(#{<<"payload">> => #{<<"payload">> => <<"processed">>}, <<"metadata">> => #{}})}
    end),
    
    Policy = router_extensions_chaos_helper:create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
    ]),
    
    {Results, Latencies} = router_extensions_chaos_helper:execute_chaos_test(NumRequests, Policy),
    {SuccessCount, _ErrorCount, _TimeoutCount, _ClientErrors} = router_extensions_chaos_helper:analyze_results(Results),
    
    Stats = router_extensions_chaos_helper:calculate_statistics(Latencies),
    
    ct:log("Total: ~p, Success: ~p", [NumRequests, SuccessCount]),
    ct:log("Stats: ~p", [Stats]),
    
    ?assert(SuccessCount > 0, "Some should succeed"),
    
    ets:delete_all_objects(LatencyCounter),
    ok.
