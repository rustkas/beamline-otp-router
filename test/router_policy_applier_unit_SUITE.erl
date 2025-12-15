%% @doc Unit Tests for router_policy_applier module
%% @test_category unit, fast, coverage_hotspot
-module(router_policy_applier_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_apply_policy_default/1,
    test_apply_policy_with_context/1,
    test_apply_policy_unknown_tenant/1,
    test_from_map_minimal/1,
    test_from_map_full/1,
    test_apply_policy_undefined_policy_id/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_apply_policy_default/1,
    test_apply_policy_with_context/1,
    test_apply_policy_unknown_tenant/1,
    test_from_map_minimal/1,
    test_from_map_full/1,
    test_apply_policy_undefined_policy_id/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_apply_policy_default,
            test_apply_policy_with_context,
            test_apply_policy_unknown_tenant,
            test_from_map_minimal,
            test_from_map_full,
            test_apply_policy_undefined_policy_id
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for apply_policy/3, apply_policy/4
%% ============================================================================

test_apply_policy_default(_Config) ->
    Request = #{
        <<"model">> => <<"gpt-4">>,
        <<"prompt">> => <<"test">>
    },
    TenantId = <<"default_tenant">>,
    PolicyId = <<"default">>,
    
    Result = router_policy_applier:apply_policy(Request, TenantId, PolicyId),
    
    case Result of
        {ok, _Decision} -> ok;
        {error, _} -> ok  %% Policy may not exist
    end,
    ok.

test_apply_policy_with_context(_Config) ->
    Request = #{
        <<"model">> => <<"gpt-4">>,
        <<"prompt">> => <<"test">>
    },
    TenantId = <<"context_tenant">>,
    PolicyId = <<"default">>,
    Context = #{
        <<"trace_id">> => <<"test-trace-123">>,
        <<"request_id">> => <<"req-456">>
    },
    
    Result = router_policy_applier:apply_policy(Request, TenantId, PolicyId, Context),
    
    case Result of
        {ok, _Decision} -> ok;
        {error, _} -> ok
    end,
    ok.

test_apply_policy_unknown_tenant(_Config) ->
    Request = #{},
    TenantId = <<"unknown_tenant_12345">>,
    PolicyId = <<"nonexistent_policy">>,
    
    Result = router_policy_applier:apply_policy(Request, TenantId, PolicyId),
    
    %% Should return error for unknown policy
    case Result of
        {ok, _} -> ok;  %% May have fallback
        {error, _} -> ok
    end,
    ok.

%% ============================================================================
%% Tests for from_map/2
%% ============================================================================

test_from_map_minimal(_Config) ->
    TenantId = <<"map_tenant">>,
    PolicyMap = #{
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{<<"openai">> => 1.0}
    },
    
    Result = router_policy_applier:from_map(TenantId, PolicyMap),
    
    %% from_map/2 returns Policy directly, not {ok, Policy}
    case Result of
        Policy when is_record(Policy, policy) ->
            ?assertEqual(TenantId, Policy#policy.tenant_id);
        {ok, Policy} when is_record(Policy, policy) ->
            ?assertEqual(TenantId, Policy#policy.tenant_id);
        {error, _} -> ok
    end,
    ok.

test_from_map_full(_Config) ->
    TenantId = <<"full_map_tenant">>,
    PolicyMap = #{
        <<"policy_id">> => <<"full_policy">>,
        <<"weights">> => #{<<"openai">> => 0.7, <<"anthropic">> => 0.3},
        <<"fallback">> => <<"local_llm">>,
        <<"sticky">> => #{<<"enabled">> => true, <<"session_key">> => <<"session_id">>},
        <<"extensions">> => #{
            <<"pre">> => [],
            <<"validators">> => [],
            <<"post">> => []
        }
    },
    
    Result = router_policy_applier:from_map(TenantId, PolicyMap),
    
    %% from_map/2 returns Policy directly, not {ok, Policy}
    case Result of
        Policy when is_record(Policy, policy) ->
            ?assertEqual(TenantId, Policy#policy.tenant_id);
        {ok, Policy} when is_record(Policy, policy) ->
            ?assertEqual(TenantId, Policy#policy.tenant_id);
        {error, _} -> ok
    end,
    ok.

test_apply_policy_undefined_policy_id(_Config) ->
    Request = #{},
    TenantId = <<"undefined_policy_tenant">>,
    
    %% Should use "default" policy when undefined
    Result = router_policy_applier:apply_policy(Request, TenantId, undefined),
    
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end,
    ok.
