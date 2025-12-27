-module(router_cp2_dag_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dag_when_condition/1, test_dag_timeout_cancel/1, test_dag_failure_path/1, test_dag_determinism/1]).

all() -> [test_dag_when_condition, test_dag_timeout_cancel, test_dag_failure_path, test_dag_determinism].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, nats_mode, mock),
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    application:unset_env(beamline_router, nats_mode),
    ok.

%% Test 1: DAG "when" condition
%% Verify that extensions are skipped if "when" evaluates to false
test_dag_when_condition(_Config) ->
    TenantId = <<"dag_tenant">>,
    PolicyId = <<"dag_policy">>,
    
    %% Policy with two pre-processors: one executes, one skipped
    PolicyMap = #{
        <<"policy_id">> => PolicyId,
        <<"weights">> => #{<<"provider_c">> => 1.0},
        <<"pre">> => [
            #{
                <<"id">> => <<"executed_pre">>,
                <<"mode">> => <<"required">>,
                <<"when">> => #{<<"==">> => [#{<<"var">> => <<"user_type">>}, <<"premium">>]}
            },
            #{
                <<"id">> => <<"skipped_pre">>,
                <<"mode">> => <<"required">>,
                <<"when">> => #{<<"==">> => [#{<<"var">> => <<"user_type">>}, <<"standard">>]}
            }
        ]
    },
    {ok, _} = router_policy_store:upsert_policy_map(TenantId, PolicyMap),
    
    %% Request with user_type=premium (should execute first, skip second)
    %% We use dry_run=true to avoid mocking NATS
    RouteRequest = #route_request{
        message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_dag">>},
        policy_id = PolicyId,
        context = #{<<"dry_run">> => true, <<"user_type">> => <<"premium">>}
    },
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    Metadata = Decision#route_decision.metadata,
    Executed = maps:get(<<"executed_extensions">>, Metadata, []),
    
    ct:pal("DEBUG: Executed extensions: ~p", [Executed]),

    ExecutedPre = lists:any(fun(E) -> maps:get(id, E) =:= <<"executed_pre">> end, Executed),
    SkippedPre = lists:any(fun(E) -> maps:get(id, E) =:= <<"skipped_pre">> end, Executed),
    
    ?assert(ExecutedPre, "Should execute pre-processor with matching condition"),
    ?assertNot(SkippedPre, "Should skip pre-processor with non-matching condition"),
    
    ok.

%% Test 2: DAG Timeout/Cancel
%% Verify that pipeline execution stops if deadline is exceeded
test_dag_timeout_cancel(_Config) ->
    TenantId = <<"timeout_tenant">>,
    PolicyId = <<"timeout_policy">>,
    
    PolicyMap = #{
        <<"policy_id">> => PolicyId,
        <<"weights">> => #{<<"provider_c">> => 1.0},
        <<"pre">> => [
            #{<<"id">> => <<"pre1">>, <<"mode">> => <<"mandatory">>}
        ]
    },
    {ok, _} = router_policy_store:upsert_policy_map(TenantId, PolicyMap),
    
    %% Case 1: Deadline in the future (should succeed)
    FutureDeadline = erlang:system_time(millisecond) + 5000,
    ReqFuture = #route_request{
        message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_ok">>},
        policy_id = PolicyId,
        context = #{<<"dry_run">> => true, <<"deadline">> => FutureDeadline}
    },
    {ok, _} = router_core:route(ReqFuture, #{}),
    
    %% Case 2: Deadline in the past (should fail)
    PastDeadline = erlang:system_time(millisecond) - 1000,
    ReqPast = #route_request{
        message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_fail">>},
        policy_id = PolicyId,
        context = #{<<"dry_run">> => true, <<"deadline">> => PastDeadline}
    },
    Result = router_core:route(ReqPast, #{}),
    ?assertMatch({error, {deadline_exceeded, _}}, Result),
    
    ok.

%% Test 3: DAG Failure Path Trace
%% Verify that error result includes executed extensions trace
test_dag_failure_path(_Config) ->
    TenantId = <<"fail_trace_tenant">>,
    PolicyId = <<"fail_trace_policy">>,
    
    PolicyMap = #{
        <<"policy_id">> => PolicyId,
        <<"weights">> => #{<<"provider_c">> => 1.0},
        <<"pre">> => [
            #{<<"id">> => <<"pre_success">>, <<"mode">> => <<"required">>},
            #{<<"id">> => <<"pre_fail">>, <<"mode">> => <<"required">>}
        ]
    },
    {ok, _} = router_policy_store:upsert_policy_map(TenantId, PolicyMap),
    
    %% Setup mocks
    meck:new(router_extension_invoker, [passthrough]),
    meck:expect(router_extension_invoker, invoke, fun
        (<<"pre_success">>, _, _) -> 
            {ok, #{<<"payload">> => #{<<"foo">> => <<"bar">>}}};
        (<<"pre_fail">>, _, _) -> 
            {error, <<"mocked_failure">>};
        (ExtId, Req, Ctx) ->
            meck:passthrough([ExtId, Req, Ctx])
    end),
    
    try
        Req = #route_request{
            message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_fail_trace">>},
            policy_id = PolicyId,
            context = #{<<"dry_run">> => false}
        },
        
        Result = router_core:route(Req, #{}),
        
        ?assertMatch({error, {pre_processor_failed, _}}, Result),
        {error, {pre_processor_failed, Info}} = Result,
        
        Executed = maps:get(<<"executed_extensions">>, Info, []),
        
        %% Should have pre_success in trace
        ?assertEqual(1, length(Executed), "Should have exactly one executed extension"),
        
        [ExtensionMap] = Executed,
        ?assertEqual(<<"pre">>, maps:get(type, ExtensionMap)),
        ?assertEqual(<<"pre_success">>, maps:get(id, ExtensionMap)),
        ?assertEqual(<<"ok">>, maps:get(status, ExtensionMap))
    after
        meck:unload(router_extension_invoker)
    end,
    ok.

%% Test 4: DAG Determinism
%% Verify that identical input produces identical trace and decision
test_dag_determinism(_Config) ->
    TenantId = <<"det_tenant">>,
    PolicyId = <<"det_policy">>,
    
    PolicyMap = #{
        <<"policy_id">> => PolicyId,
        <<"weights">> => #{<<"provider_c">> => 1.0},
        <<"pre">> => [
            #{<<"id">> => <<"pre_det">>, <<"mode">> => <<"mandatory">>}
        ],
        <<"validators">> => [
            #{<<"id">> => <<"val_det">>, <<"on_fail">> => <<"block">>}
        ],
        <<"post">> => [
            #{<<"id">> => <<"post_det">>, <<"mode">> => <<"mandatory">>}
        ]
    },
    {ok, _} = router_policy_store:upsert_policy_map(TenantId, PolicyMap),
    
    Req = #route_request{
        message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_det">>},
        policy_id = PolicyId,
        context = #{<<"dry_run">> => true}
    },
    
    {ok, Decision1} = router_core:route(Req, #{}),
    {ok, Decision2} = router_core:route(Req, #{}),
    
    Trace1 = maps:get(<<"executed_extensions">>, Decision1#route_decision.metadata),
    Trace2 = maps:get(<<"executed_extensions">>, Decision2#route_decision.metadata),
    
    ?assertEqual(Trace1, Trace2, "Traces should be identical"),
    ?assertEqual(3, length(Trace1), "Trace should have 3 items"),
    
    ok.
