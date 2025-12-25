%% @doc Extensions Chaos: Recovery Scenarios
%%
%% Tests mass degradation and recovery:
%% - Mass degradation of multiple extensions
%% - Recovery after failure
%%
%% @test_category chaos, heavy, resilience
-module(router_ext_chaos_recovery_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_mass_degradation/1,
    test_recovery_after_failure/1
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
    [{group, recovery_tests}];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{recovery_tests, [sequence], [
        test_mass_degradation,
        test_recovery_after_failure
    ]}].

init_per_suite(Config) -> router_extensions_chaos_helper:init_suite(Config).
end_per_suite(Config) -> router_extensions_chaos_helper:end_suite(Config).
init_per_testcase(TC, Config) -> router_extensions_chaos_helper:init_testcase(TC, Config).
end_per_testcase(TC, Config) -> router_extensions_chaos_helper:end_testcase(TC, Config).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_mass_degradation(_Config) ->
    ct:comment("=== Mass Degradation ==="),
    NumRequests = 150,
    DegradationStart = 30,

    MassCounter = mass_counter,
    _ = router_ets_helpers:ensure_named_ets_table(MassCounter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case router_ets_helpers:ets_lookup(MassCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(MassCounter, {count, RequestNum}),
        
        IsDegraded = RequestNum >= DegradationStart,
        case IsDegraded of
            false ->
                timer:sleep(10),
                {ok, jsx:encode(#{<<"payload">> => <<"processed">>, <<"metadata">> => #{}})};
            true ->
                FailRate = min(0.9, (RequestNum - DegradationStart) / 100),
                case rand:uniform() < FailRate of
                    true -> {error, timeout};
                    false ->
                        timer:sleep(100),
                        {ok, jsx:encode(#{<<"payload">> => <<"processed">>, <<"metadata">> => #{}})}
                end
        end
    end),
    
    Policy = router_extensions_chaos_helper:create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}
    ]),
    
    {Results, _Latencies} = router_extensions_chaos_helper:execute_chaos_test(NumRequests, Policy),
    {SuccessCount, ErrorCount, _TimeoutCount, _ClientErrors} = router_extensions_chaos_helper:analyze_results(Results),
    
    ct:log("Total: ~p, Success: ~p, Errors: ~p", [NumRequests, SuccessCount, ErrorCount]),
    
    ?assert(SuccessCount > 0, "Some should succeed (early requests and fail-open)"),
    ?assert(SuccessCount < NumRequests, "Not all should succeed (degradation)"),
    
    ets:delete_all_objects(MassCounter),
    ok.

test_recovery_after_failure(_Config) ->
    ct:comment("=== Recovery After Failure ==="),
    NumRequests = 200,
    FailureStart = 50,
    FailureEnd = 120,

    RecoveryCounter = recovery_counter,
    _ = router_ets_helpers:ensure_named_ets_table(RecoveryCounter, [set, private]),
    meck:expect(router_nats, request, fun(_Subject, _Payload, _Timeout) ->
        RequestNum = case router_ets_helpers:ets_lookup(RecoveryCounter, count) of
            [] -> 1;
            [{count, C}] -> C + 1
        end,
        ets:insert(RecoveryCounter, {count, RequestNum}),
        
        IsFailurePeriod = RequestNum >= FailureStart andalso RequestNum < FailureEnd,
        case IsFailurePeriod of
            true ->
                {error, connection_error};
            false ->
                timer:sleep(10),
                {ok, jsx:encode(#{<<"payload">> => <<"processed">>, <<"metadata">> => #{}})}
        end
    end),
    
    Policy = router_extensions_chaos_helper:create_policy_with_extensions([
        {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}
    ]),
    
    {Results, _Latencies} = router_extensions_chaos_helper:execute_chaos_test(NumRequests, Policy),
    {SuccessCount, ErrorCount, _TimeoutCount, _ClientErrors} = router_extensions_chaos_helper:analyze_results(Results),
    
    ct:log("Total: ~p, Success: ~p, Errors: ~p", [NumRequests, SuccessCount, ErrorCount]),
    
    ?assert(SuccessCount > 0, "Some should succeed (before and after failure)"),
    ?assert(ErrorCount > 0, "Some should fail (during failure period)"),
    
    ExpectedSuccessMin = (FailureStart - 1) + (NumRequests - FailureEnd + 1),
    ?assert(SuccessCount >= ExpectedSuccessMin * 0.8, "Recovery should restore success rate"),
    
    ets:delete_all_objects(RecoveryCounter),
    ok.
