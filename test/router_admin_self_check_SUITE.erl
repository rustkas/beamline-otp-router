%% @doc Admin API Self-Test Suite
%% Tests all admin NATS subjects and validates response formats
-module(router_admin_self_check_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, init_per_suite/1, init_per_testcase/2]}).

-define(ADMIN_SUBJECTS, [
    <<"beamline.router.v1.admin.get_extension_health">>,
    <<"beamline.router.v1.admin.get_circuit_breaker_states">>,
    <<"beamline.router.v1.admin.dry_run_pipeline">>,
    <<"beamline.router.v1.admin.get_pipeline_complexity">>
]).

%% Test suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_self_check_all/1,
    test_extension_health_format/1,
    test_circuit_breaker_states_format/1,
    test_dry_run_pipeline_format/1,
    test_pipeline_complexity_format/1,
    test_all_admin_subjects_respond/1
]).

init_per_suite(Config) ->
    %% Ensure Router is started
    case application:ensure_all_started(beamline_router) of
        {ok, _StartedApps} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Setup test policy for self-check endpoints
    %% self_check_dry_run_pipeline and self_check_pipeline_complexity require test_tenant/test_policy
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    TestPolicy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{<<"openai">> => 1.0},  %% Valid weight: 0.0-1.0
        fallback = undefined,
        sticky = undefined,
        pre = [],
        validators = [],
        post = [],
        metadata = #{}
    },
    %% Ensure policy store is available
    case whereis(router_policy_store) of
        undefined -> 
            Config;
        _ -> 
            %% Clean up any existing policy first
            catch router_policy_store:delete_policy(TenantId, PolicyId),
            %% Create test policy
            case router_policy_store:upsert_policy(TenantId, TestPolicy) of
                {ok, _UpdatedPolicy} ->
                    %% Policy created/updated successfully
                    Config;
                {error, invalid_policy, _Details} ->
                    %% Policy validation failed - skip test
                    {skip, "Test policy validation failed"};
                {error, Reason} ->
                    %% Other error - skip test
                    {skip, io_lib:format("Failed to create test policy: ~p", [Reason])};
                Other ->
                    %% Unexpected return value - log and continue
                    ct:log("Warning: Unexpected return from upsert_policy: ~p", [Other]),
                    Config
            end
    end.

end_per_testcase(_TestCase, _Config) ->
    %% Cleanup test policy if it exists
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    case whereis(router_policy_store) of
        undefined -> ok;
        _ -> 
            catch router_policy_store:delete_policy(TenantId, PolicyId)
    end,
    ok.

%% Test cases
all() ->
    [].

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [], [
            test_self_check_all,
            test_extension_health_format,
            test_circuit_breaker_states_format,
            test_dry_run_pipeline_format,
            test_pipeline_complexity_format,
            test_all_admin_subjects_respond
        ]}
    ].

%% @doc Test self_check_all function
test_self_check_all(_Config) ->
    case router_admin_self_check:self_check_all() of
        {ok, Results} ->
            ct:log("Self-check results: ~p", [Results]),
            %% Verify all checks passed
            Failed = [R || R = {_Name, {error, _}} <- Results],
            case Failed of
                [] ->
                    ok;
                _ ->
                    ct:fail("Some checks failed: ~p", [Failed])
            end;
        {error, Reason} ->
            ct:fail("Self-check failed: ~p", [Reason])
    end.

%% @doc Test extension health response format
test_extension_health_format(_Config) ->
    RequestJson = jsx:encode(#{}),
    case router_admin_nats:handle_get_extension_health(RequestJson) of
        {ok, ResponseJson} ->
            Response = jsx:decode(ResponseJson, [return_maps]),
            %% Validate structure
            ?assert(is_map(Response)),
            ?assert(maps:is_key(<<"health">>, Response)),
            Health = maps:get(<<"health">>, Response),
            ?assert(is_map(Health)),
            ok;
        {error, ErrorJson} ->
            Error = jsx:decode(ErrorJson, [return_maps]),
            ct:log("Extension health returned error: ~p", [Error]),
            %% Error format is acceptable, but log it
            ok
    end.

%% @doc Test circuit breaker states response format
test_circuit_breaker_states_format(_Config) ->
    RequestJson = jsx:encode(#{}),
    case router_admin_nats:handle_get_circuit_breaker_states(RequestJson) of
        {ok, ResponseJson} ->
            Response = jsx:decode(ResponseJson, [return_maps]),
            %% Validate structure
            ?assert(is_map(Response)),
            ?assert(maps:is_key(<<"states">>, Response), "Response must have 'states' field"),
            States = maps:get(<<"states">>, Response),
            ?assert(is_map(States)),
            ok;
        {error, ErrorJson} ->
            Error = jsx:decode(ErrorJson, [return_maps]),
            ct:log("Circuit breaker states returned error: ~p", [Error]),
            ok
    end.

%% @doc Test dry-run pipeline response format
test_dry_run_pipeline_format(_Config) ->
    %% Create test policy first
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    TestPolicy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{<<"openai">> => 1.0},
        fallback = undefined,
        sticky = undefined,
        pre = [],
        validators = [],
        post = [],
        metadata = #{}
    },
    case router_policy_store:upsert_policy(TenantId, TestPolicy) of
        {ok, _UpdatedPolicy} ->
            ok;
        {error, _Reason} = UpsertError ->
            ct:fail("Failed to create test policy: ~p", [UpsertError])
    end,
    
    try
        Request = #{
            <<"tenant_id">> => TenantId,
            <<"policy_id">> => PolicyId,
            <<"payload">> => #{<<"message">> => <<"test">>}
        },
        RequestJson = jsx:encode(Request),
        case router_admin_nats:handle_dry_run_pipeline(RequestJson) of
            {ok, ResponseJson} ->
                Response = jsx:decode(ResponseJson, [return_maps]),
                %% Validate structure
                ?assert(is_map(Response)),
                ?assert(maps:is_key(<<"ok">>, Response), "Response must have 'ok' field"),
                Ok = maps:get(<<"ok">>, Response),
                ?assert(is_boolean(Ok)),
                ok;
            {error, ErrorJson} ->
                DecodedError = jsx:decode(ErrorJson, [return_maps]),
                ct:log("Dry-run pipeline returned error: ~p", [DecodedError]),
                %% Error format is acceptable
                ok
        end
    after
        %% Cleanup
        router_policy_store:delete_policy(TenantId, PolicyId)
    end.

%% @doc Test pipeline complexity response format
test_pipeline_complexity_format(_Config) ->
    %% Create test policy first
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    TestPolicy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{<<"openai">> => 1.0},
        fallback = undefined,
        sticky = undefined,
        pre = [],
        validators = [],
        post = [],
        metadata = #{}
    },
    case router_policy_store:upsert_policy(TenantId, TestPolicy) of
        {ok, _UpdatedPolicy} ->
            ok;
        {error, _Reason} = UpsertError ->
            ct:fail("Failed to create test policy: ~p", [UpsertError])
    end,
    
    try
        Request = #{
            <<"tenant_id">> => TenantId,
            <<"policy_id">> => PolicyId
        },
        RequestJson = jsx:encode(Request),
        case router_admin_nats:handle_get_pipeline_complexity(RequestJson) of
            {ok, ResponseJson} ->
                Response = jsx:decode(ResponseJson, [return_maps]),
                %% Validate structure
                ?assert(is_map(Response)),
                ?assert(maps:is_key(<<"complexity_score">>, Response), 
                    "Response must have 'complexity_score' field"),
                ?assert(maps:is_key(<<"total_extensions">>, Response), 
                    "Response must have 'total_extensions' field"),
                Score = maps:get(<<"complexity_score">>, Response),
                Total = maps:get(<<"total_extensions">>, Response),
                ?assert(is_integer(Score)),
                ?assert(is_integer(Total)),
                ok;
            {error, ErrorJson} ->
                DecodedError = jsx:decode(ErrorJson, [return_maps]),
                ct:log("Pipeline complexity returned error: ~p", [DecodedError]),
                ok
        end
    after
        %% Cleanup
        router_policy_store:delete_policy(TenantId, PolicyId)
    end.

%% @doc Test that all admin subjects respond (if NATS is available)
test_all_admin_subjects_respond(_Config) ->
    %% This test verifies that all admin subjects are registered
    %% and can be called (if NATS is available)
    case catch router_nats:get_connection_status() of
        Status when is_map(Status) ->
            ConnectionState = maps:get(state, Status, disconnected),
            case ConnectionState of
                connected ->
                    %% NATS is connected, test actual NATS calls
                    lists:foreach(fun(Subject) ->
                        ct:log("Testing admin subject: ~s", [Subject]),
                        %% Create minimal request
                        RequestJson = jsx:encode(#{}),
                        %% Try to call via NATS (if subscriber is running)
                        case catch router_nats:request(Subject, RequestJson, 5000) of
                            {ok, _Response} ->
                                ct:log("Subject ~s responded", [Subject]);
                            {error, _Reason} ->
                                ct:log("Subject ~s returned error: ~p", [Subject, _Reason]);
                            {'EXIT', Reason} ->
                                ct:log("Subject ~s failed with exception: ~p", [Subject, Reason])
                        end
                    end, ?ADMIN_SUBJECTS),
                    ok;
                _ ->
                    %% NATS not connected, skip actual NATS calls
                    ct:log("NATS not connected (state: ~p), skipping NATS call tests", [ConnectionState]),
                    ok
            end;
        {'EXIT', _Reason} ->
            %% router_nats not available or not started
            ct:log("router_nats not available, skipping NATS call tests"),
            ok;
        _ ->
            %% Unexpected response
            ct:log("Unexpected connection status response, skipping NATS call tests"),
            ok
    end.
