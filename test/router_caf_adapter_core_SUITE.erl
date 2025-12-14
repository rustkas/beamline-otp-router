%% @doc CAF Adapter Enhanced: Core Tests
%%
%% Core CAF adapter tests:
%% - Retry success on second attempt
%% - Retry exhausted
%% - Tenant blocked
%% - Global disable
%%
%% @test_category caf, unit
-module(router_caf_adapter_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_retry_success/1,
    test_retry_exhausted/1,
    test_tenant_blocked/1,
    test_global_disable/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, core_tests}];
        "full" -> [{group, core_tests}];
        _ -> []  %% Skip in fast mode - requires CAF integration
    end.

groups() ->
    [{core_tests, [sequence], [
        test_retry_success,
        test_retry_exhausted,
        test_tenant_blocked,
        test_global_disable
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    _ = router_test_init:ensure_ets_table(retry_counter, [set, named_table, public]),
    ets:delete_all_objects(retry_counter),
    ok = router_mock_helpers:setup_router_nats_mock(),
    router_mock_helpers:reset(router_nats),
    Config.
end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_retry_success(_Config) ->
    ct:comment("=== Retry Success ==="),
    case code:which(meck) of
        non_existing -> {skip, "meck not available"};
        _ ->
            Counter = router_test_init:ensure_ets_table(retry_counter, [set, named_table, public]),
            ets:insert(Counter, {count, 0}),
            
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) ->
                [{count, C}] = ets:lookup(Counter, count),
                ets:insert(Counter, {count, C + 1}),
                case C of
                    0 -> {error, timeout};
                    _ -> {ok, <<"ack">>}
                end
            end),
            
            Request = #{
                <<"version">> => <<"1">>,
                <<"request_id">> => <<"req-retry">>,
                <<"tenant_id">> => <<"test-tenant">>,
                <<"task">> => #{<<"type">> => <<"text.generate">>}
            },
            Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                                       priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
            
            Result = router_caf_adapter:publish_assignment(Request, Decision),
            ?assertEqual(ok, Result),
            
            ets:delete_all_objects(Counter),
            ok
    end.

test_retry_exhausted(_Config) ->
    ct:comment("=== Retry Exhausted ==="),
    case code:which(meck) of
        non_existing -> {skip, "meck not available"};
        _ ->
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) ->
                {error, {connection_failed, "NATS unavailable"}}
            end),
            
            Request = #{
                <<"version">> => <<"1">>,
                <<"request_id">> => <<"req-fail">>,
                <<"tenant_id">> => <<"test-tenant">>,
                <<"task">> => #{<<"type">> => <<"text.generate">>}
            },
            Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                                       priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
            
            Result = router_caf_adapter:publish_assignment(Request, Decision),
            %% Result may be 'error' atom or {error, _} tuple
            case Result of
                error -> ok;
                {error, _} -> ok;
                Other -> ct:fail("Expected error, got: ~p", [Other])
            end,
            
            ok
    end.

test_tenant_blocked(_Config) ->
    ct:comment("=== Tenant Blocked ==="),
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [<<"allowed-tenant">>]),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-blocked">>,
        <<"tenant_id">> => <<"blocked-tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>}
    },
    Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                               priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
    
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    ?assertEqual(ok, Result),
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    ok.

test_global_disable(_Config) ->
    ct:comment("=== Global Disable ==="),
    %% Must set assignment_enabled (takes precedence over caf_push_assignment_enabled)
    application:set_env(beamline_router, assignment_enabled, false),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-disabled">>,
        <<"tenant_id">> => <<"test-tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>}
    },
    Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                               priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
    
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    ?assertEqual(ok, Result),
    
    application:set_env(beamline_router, assignment_enabled, true),
    ok.
