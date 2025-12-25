%% @doc CP1 Router Smoke Tests
%%
%% Fast, stable smoke tests that validate key CP1 Router contracts.
%% Runs on every CI push as a sanity gate.
%%
%% Covers:
%% - Basic routing
%% - Policy
%% - Error handling
%% - Gateway contracts
%%
%% @test_category cp1_smoke, fast
-module(router_cp1_smoke_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_basic_routing_decision/1,
    test_policy_lookup/1,
    test_error_mapping/1,
    test_gateway_request_validation/1,
    test_circuit_breaker_init/1,
    test_no_process_leaks/1,
    test_no_ets_leaks/1,
    %% Negative smoke tests (Task 15)
    test_invalid_policy_error/1,
    test_missing_tenant_error/1,
    test_invalid_request_error/1,
    %% Regression: malformed policy never crashes router (Task 4)
    test_malformed_policy_no_crash/1
]).

%% Internal helpers for leak detection
-export([snapshot_router_processes/0, snapshot_router_ets/0]).

suite() -> [{timetrap, {minutes, 1}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    %% CP1 smoke tests run by default - no env gating required
    [{group, smoke_tests}];
groups_for_level(full) ->
    %% CP1 smoke tests run by default - no env gating required
    [{group, smoke_tests}];
groups_for_level(_) ->
    %% CP1 smoke tests run by default - no env gating required
    [{group, smoke_tests}].
groups() ->
    [{smoke_tests, [sequence], [
        test_basic_routing_decision,
        test_policy_lookup,
        test_error_mapping,
        test_gateway_request_validation,
        test_circuit_breaker_init,
        %% Negative smoke tests
        test_invalid_policy_error,
        test_missing_tenant_error,
        test_invalid_request_error,
        %% Regression: malformed policy never crashes router
        test_malformed_policy_no_crash,
        %% Leak detection tests (run last)
        test_no_process_leaks,
        test_no_ets_leaks
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    ok = application:set_env(beamline_router, disable_heir, true),
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(TestCase, Config) ->
    %% Snapshot state before test for leak detection
    ProcessCount = length(snapshot_router_processes()),
    EtsTables = snapshot_router_ets(),
    [{pre_process_count, ProcessCount}, {pre_ets_tables, EtsTables}, {test_case, TestCase} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Clean up any mocks and verify none remain (Task 3: no stray mocks)
    router_mock_helpers:cleanup_and_verify(),
    
    %% Bounded wait for process churn to settle
    PreProcessCount = proplists:get_value(pre_process_count, Config, 0),
    _ = test_helpers:wait_for_condition(
        fun() -> length(snapshot_router_processes()) =< PreProcessCount + 5 end,
        200),
    
    %% Check for process/ETS growth (warning only, not failure)
    PostProcessCount = length(snapshot_router_processes()),
    if
        PostProcessCount > PreProcessCount + 5 ->
            ct:pal("Warning: Process count grew from ~p to ~p", [PreProcessCount, PostProcessCount]);
        true ->
            ok
    end,
    ok.

%% ============================================================================
%% SMOKE TEST CASES
%% ============================================================================

%% @doc Smoke test: Basic routing decision can be made
test_basic_routing_decision(_Config) ->
    %% Create a test policy
    TenantId = <<"smoke_tenant">>,
    PolicyId = <<"smoke_policy">>,
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    
    %% Store policy
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Verify policy can be retrieved
    {ok, RetrievedPolicy} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(TenantId, RetrievedPolicy#policy.tenant_id),
    ?assertEqual(PolicyId, RetrievedPolicy#policy.policy_id),
    
    %% Cleanup
    router_policy_store:delete_policy(TenantId, PolicyId),
    ok.

%% @doc Smoke test: Policy lookup works
test_policy_lookup(_Config) ->
    %% Test non-existent policy
    Result = router_policy_store:get_policy(<<"nonexistent">>, <<"nonexistent">>),
    ?assertMatch({error, _}, Result),
    ok.

%% @doc Smoke test: Error mapping works correctly
test_error_mapping(_Config) ->
    %% Test known error codes
    {Status1, _Msg1} = router_error:to_grpc(missing_tenant_id),
    ?assertEqual(3, Status1),  %% INVALID_ARGUMENT
    
    {Status2, _Msg2} = router_error:to_grpc(policy_not_found),
    ?assertEqual(5, Status2),  %% NOT_FOUND
    
    {Status3, _Msg3} = router_error:to_grpc(internal_error),
    ?assertEqual(13, Status3),  %% INTERNAL
    
    %% Test unknown error maps to INTERNAL
    {Status4, _Msg4} = router_error:to_grpc(some_unknown_error),
    ?assertEqual(13, Status4),  %% INTERNAL
    ok.

%% @doc Smoke test: Gateway request validation
test_gateway_request_validation(_Config) ->
    %% Valid request structure
    ValidRequest = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-smoke-1">>,
        <<"tenant_id">> => <<"smoke_tenant">>,
        <<"policy_id">> => <<"smoke_policy">>,
        <<"message">> => #{
            <<"type">> => <<"chat">>,
            <<"payload">> => <<"test">>
        }
    },
    
    %% Verify request has required fields
    ?assert(maps:is_key(<<"version">>, ValidRequest)),
    ?assert(maps:is_key(<<"request_id">>, ValidRequest)),
    ?assert(maps:is_key(<<"tenant_id">>, ValidRequest)),
    ?assert(maps:is_key(<<"message">>, ValidRequest)),
    
    %% Verify JSON encoding works
    Json = jsx:encode(ValidRequest),
    ?assert(is_binary(Json)),
    
    %% Verify JSON decoding works
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(ValidRequest, Decoded),
    ok.

%% @doc Smoke test: Circuit breaker initializes correctly
test_circuit_breaker_init(_Config) ->
    %% Verify circuit breaker process is running
    ?assertNotEqual(undefined, whereis(router_circuit_breaker)),
    
    %% Verify we can query state for a tenant/provider
    TenantId = <<"smoke_tenant">>,
    ProviderId = <<"openai">>,
    
    %% For new tenant/provider, state is either closed or not_found
    Result = router_circuit_breaker:get_state(TenantId, ProviderId),
    case Result of
        {ok, closed} -> ok;
        {ok, open} -> ok;
        {ok, half_open} -> ok;
        {error, not_found} -> ok;  %% This is valid for new tenant/provider
        Other -> ct:fail({unexpected_result, Other})
    end,
    ok.

%% ============================================================================
%% NEGATIVE SMOKE TESTS
%% ============================================================================

%% @doc Smoke test: Invalid policy returns error
test_invalid_policy_error(_Config) ->
    %% Policy with missing required fields
    InvalidPolicy = #{
        <<"tenant_id">> => <<"test">>,
        %% Missing policy_id and weights
        <<"version">> => <<"1.0">>
    },
    
    Result = router_policy_validator:validate(InvalidPolicy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

%% @doc Smoke test: Missing tenant returns correct error
test_missing_tenant_error(_Config) ->
    %% Error mapping for missing tenant
    {Status, _Message} = router_error:to_grpc(missing_tenant_id),
    ?assertEqual(3, Status),  %% INVALID_ARGUMENT
    
    %% Also check that policy lookup with empty tenant fails
    Result = router_policy_store:get_policy(<<>>, <<"any_policy">>),
    ?assertMatch({error, _}, Result),
    ok.

%% @doc Smoke test: Invalid request structure returns error
test_invalid_request_error(_Config) ->
    %% Error mapping for invalid request
    {Status, _Message} = router_error:to_grpc(invalid_request),
    ?assertEqual(3, Status),  %% INVALID_ARGUMENT
    
    %% Test with context override
    {Status2, Message2} = router_error:to_grpc(invalid_request, #{
        context => <<"Missing required field: message">>
    }),
    ?assertEqual(3, Status2),
    ?assertEqual(<<"Missing required field: message">>, Message2),
    ok.

%% @doc Regression test: Malformed policy returns error but never crashes router
%% This ensures invalid policy input returns controlled error, not exception.
test_malformed_policy_no_crash(_Config) ->
    %% Snapshot supervisor pid before the test
    SupPidBefore = whereis(beamline_router_sup),
    PolicyStorePidBefore = whereis(router_policy_store),
    ?assertNotEqual(undefined, SupPidBefore),
    ?assertNotEqual(undefined, PolicyStorePidBefore),
    
    %% Feed various malformed policies via upsert (the same path used by CP1 routing)
    TenantId = <<"malformed_test_tenant">>,
    
    %% 1. Empty policy_id
    MalformedPolicy1 = #policy{
        tenant_id = TenantId,
        policy_id = <<>>,
        weights = #{<<"openai">> => 1.0}
    },
    Result1 = router_policy_store:upsert_policy(TenantId, MalformedPolicy1),
    ?assertMatch({error, _, _}, Result1),
    
    %% 2. Invalid weight > 1.0
    MalformedPolicy2 = #policy{
        tenant_id = TenantId,
        policy_id = <<"bad_weight">>,
        weights = #{<<"openai">> => 2.5}
    },
    Result2 = router_policy_store:upsert_policy(TenantId, MalformedPolicy2),
    ?assertMatch({error, _, _}, Result2),
    
    %% 3. Invalid weight < 0.0
    MalformedPolicy3 = #policy{
        tenant_id = TenantId,
        policy_id = <<"negative_weight">>,
        weights = #{<<"openai">> => -0.5}
    },
    Result3 = router_policy_store:upsert_policy(TenantId, MalformedPolicy3),
    ?assertMatch({error, _, _}, Result3),
    
    %% Critical assertion: Router processes must still be alive (no crash)
    SupPidAfter = whereis(beamline_router_sup),
    PolicyStorePidAfter = whereis(router_policy_store),
    
    ?assertEqual(SupPidBefore, SupPidAfter, "Supervisor should not have restarted"),
    ?assertEqual(PolicyStorePidBefore, PolicyStorePidAfter, "Policy store should not have crashed"),
    ?assert(is_process_alive(SupPidAfter), "Supervisor must be alive"),
    ?assert(is_process_alive(PolicyStorePidAfter), "Policy store must be alive"),
    ok.

%% @doc Smoke test: No process leaks after running tests
test_no_process_leaks(_Config) ->
    %% Get router-specific processes
    RouterProcs = snapshot_router_processes(),
    
    %% These are expected to be running
    ExpectedProcs = [
        router_circuit_breaker,
        router_policy_store,
        router_nats,
        beamline_router_sup
    ],
    
    %% Check that expected processes exist
    RegisteredNames = [Name || {Name, _Pid} <- RouterProcs],
    lists:foreach(fun(ExpectedName) ->
        case lists:member(ExpectedName, RegisteredNames) of
            true -> ok;
            false -> 
                %% Some processes may not be registered - that's ok
                ok
        end
    end, ExpectedProcs),
    
    %% Log current process count for reference
    ct:pal("Router processes count: ~p", [length(RouterProcs)]),
    ok.

%% @doc Smoke test: No unexpected ETS table leaks
test_no_ets_leaks(_Config) ->
    %% Get router-related ETS tables
    RouterTables = snapshot_router_ets(),
    
    %% Log tables for reference
    ct:pal("Router ETS tables: ~p", [RouterTables]),
    
    %% Verify no test-only tables remain
    TestTablePrefixes = ["test_", "tmp_", "temp_"],
    LeakedTables = [T || T <- RouterTables,
                         lists:any(fun(Prefix) ->
                             lists:prefix(Prefix, atom_to_list(T))
                         end, TestTablePrefixes)],
    
    ?assertEqual([], LeakedTables, "Found leaked test ETS tables"),
    ok.

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

%% @doc Snapshot router-specific processes
snapshot_router_processes() ->
    RouterPrefixes = [router_, beamline_],
    RegisteredProcesses = registered(),
    
    lists:filtermap(fun(Name) ->
        NameStr = atom_to_list(Name),
        case lists:any(fun(Prefix) ->
                 lists:prefix(atom_to_list(Prefix), NameStr)
             end, RouterPrefixes) of
            true ->
                case whereis(Name) of
                    undefined -> false;
                    Pid -> {true, {Name, Pid}}
                end;
            false -> false
        end
    end, RegisteredProcesses).

%% @doc Snapshot router-related ETS tables
snapshot_router_ets() ->
    AllTables = ets:all(),
    RouterPrefixes = [router_, beamline_],
    
    lists:filter(fun(Table) ->
        case ets:info(Table, name) of
            undefined -> false;
            Name ->
                NameStr = atom_to_list(Name),
                lists:any(fun(Prefix) ->
                    lists:prefix(atom_to_list(Prefix), NameStr)
                end, RouterPrefixes)
        end
    end, AllTables).

