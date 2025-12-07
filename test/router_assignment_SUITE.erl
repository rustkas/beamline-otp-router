%% @doc Common Test suite for router_assignment (ExecAssignment publishing)
-module(router_assignment_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_build_exec_assignment/1,
    test_publish_assignment_success/1,
    test_publish_assignment_failure/1,
    test_exec_assignment_format/1,
    test_exec_assignment_with_tenant_id/1,
    test_exec_assignment_correlation/1
]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_build_exec_assignment/1,
    test_publish_assignment_success/1,
    test_publish_assignment_failure/1,
    test_exec_assignment_format/1,
    test_exec_assignment_with_tenant_id/1,
    test_exec_assignment_correlation/1
]).

all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [sequence], [
            test_build_exec_assignment,
            test_publish_assignment_success,
            test_publish_assignment_failure,
            test_exec_assignment_format,
            test_exec_assignment_with_tenant_id,
            test_exec_assignment_correlation
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, caf_push_assignment_enabled, true),
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

end_per_testcase(_TestCase, Config) ->
    Config.

%% Test: Build ExecAssignment according to API_CONTRACTS.md
test_build_exec_assignment(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-123">>,
        <<"trace_id">> => <<"tr-456">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"options">> => #{
            <<"priority">> => 60
        },
        <<"metadata">> => #{
            <<"user_id">> => <<"u-42">>
        }
    },
    Decision = #route_decision{
        provider_id = <<"openai:gpt-4o">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        reason = <<"best_score">>,
        metadata = #{}
    },
    AssignmentId = router_caf_adapter:generate_assignment_id(),
    ExecAssignment = router_caf_adapter:build_exec_assignment(AssignmentId, Request, Decision),
    
    %% Verify required fields according to API_CONTRACTS.md
    ?assertEqual(<<"1">>, maps:get(<<"version">>, ExecAssignment)),
    ?assertEqual(AssignmentId, maps:get(<<"assignment_id">>, ExecAssignment)),
    ?assertEqual(<<"req-123">>, maps:get(<<"request_id">>, ExecAssignment)),
    ?assert(maps:is_key(<<"executor">>, ExecAssignment)),
    ?assert(maps:is_key(<<"job">>, ExecAssignment)),
    ?assert(maps:is_key(<<"options">>, ExecAssignment)),
    ?assert(maps:is_key(<<"correlation">>, ExecAssignment)),
    ?assert(maps:is_key(<<"decision">>, ExecAssignment)),
    ?assert(maps:is_key(<<"tenant_id">>, ExecAssignment)),
    ok.

%% Test: Successful assignment publication
test_publish_assignment_success(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-success">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"push_assignment">> => true
    },
    Decision = #route_decision{
        provider_id = <<"openai:gpt-4o">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        reason = <<"best_score">>
    },
    
    meck:new(router_nats, [passthrough]),
    %% Mock publish_with_ack/3 to return success with PubAck ID
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {ok, <<"ack-123">>} end),
    
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    
    ?assertEqual(ok, Result),
    
    meck:unload(router_nats),
    ok.

%% Test: Assignment publication failure
test_publish_assignment_failure(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-failure">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"push_assignment">> => true
    },
    Decision = #route_decision{
        provider_id = <<"openai:gpt-4o">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        reason = <<"best_score">>
    },
    
    meck:new(router_nats, [passthrough]),
    %% Mock publish_with_ack/3 instead of publish/2
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {error, connection_failed} end),
    
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    
    %% Should return error after retries exhausted
    ?assertEqual(error, Result),
    
    meck:unload(router_nats),
    ok.

%% Test: ExecAssignment format validation
test_exec_assignment_format(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-format">>,
        <<"trace_id">> => <<"tr-789">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Decision = #route_decision{
        provider_id = <<"openai:gpt-4o">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        reason = <<"best_score">>
    },
    AssignmentId = router_caf_adapter:generate_assignment_id(),
    ExecAssignment = router_caf_adapter:build_exec_assignment(AssignmentId, Request, Decision),
    
    %% Verify executor structure
    Executor = maps:get(<<"executor">>, ExecAssignment),
    ?assertEqual(<<"openai:gpt-4o">>, maps:get(<<"provider_id">>, Executor)),
    ?assertEqual(<<"nats">>, maps:get(<<"channel">>, Executor)),
    
    %% Verify job structure
    Job = maps:get(<<"job">>, ExecAssignment),
    ?assertEqual(<<"text.generate">>, maps:get(<<"type">>, Job)),
    ?assertEqual(<<"s3://bucket/key">>, maps:get(<<"payload_ref">>, Job)),
    
    %% Verify options structure
    Options = maps:get(<<"options">>, ExecAssignment),
    ?assert(maps:is_key(<<"priority">>, Options)),
    ?assert(maps:is_key(<<"deadline_ms">>, Options)),
    ?assert(maps:is_key(<<"retry">>, Options)),
    
    %% Verify correlation
    Correlation = maps:get(<<"correlation">>, ExecAssignment),
    ?assertEqual(<<"tr-789">>, maps:get(<<"trace_id">>, Correlation)),
    
    %% Verify decision context
    DecisionCtx = maps:get(<<"decision">>, ExecAssignment),
    ?assertEqual(<<"openai:gpt-4o">>, maps:get(<<"provider_id">>, DecisionCtx)),
    ?assertEqual(850, maps:get(<<"expected_latency_ms">>, DecisionCtx)),
    ?assertEqual(0.012, maps:get(<<"expected_cost">>, DecisionCtx)),
    ?assertEqual(<<"best_score">>, maps:get(<<"reason">>, DecisionCtx)),
    ok.

%% Test: ExecAssignment with tenant_id
test_exec_assignment_with_tenant_id(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-tenant">>,
        <<"tenant_id">> => <<"acme">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    Decision = #route_decision{
        provider_id = <<"openai:gpt-4o">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        reason = <<"best_score">>
    },
    AssignmentId = router_caf_adapter:generate_assignment_id(),
    ExecAssignment = router_caf_adapter:build_exec_assignment(AssignmentId, Request, Decision),
    
    %% Verify tenant_id is included
    ?assertEqual(<<"acme">>, maps:get(<<"tenant_id">>, ExecAssignment)),
    ok.

%% Test: Correlation IDs in ExecAssignment
test_exec_assignment_correlation(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-correlate">>,
        <<"trace_id">> => <<"tr-correlate">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    Decision = #route_decision{
        provider_id = <<"openai:gpt-4o">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        reason = <<"best_score">>
    },
    AssignmentId = router_caf_adapter:generate_assignment_id(),
    ExecAssignment = router_caf_adapter:build_exec_assignment(AssignmentId, Request, Decision),
    
    %% Verify correlation IDs
    ?assertEqual(AssignmentId, maps:get(<<"assignment_id">>, ExecAssignment)),
    ?assertEqual(<<"req-correlate">>, maps:get(<<"request_id">>, ExecAssignment)),
    ?assertEqual(<<"tr-correlate">>, maps:get(<<"trace_id">>, maps:get(<<"correlation">>, ExecAssignment))),
    ok.

