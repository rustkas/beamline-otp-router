%% @doc Unit Tests for router_decide_consumer module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_decide_consumer_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports  
-export([
    test_module_exports/1,
    test_gen_server_callbacks/1,
    test_start_link_exported/1,
    test_behaviour_gen_server/1,
    test_normalize_boolean/1,
    test_check_tenant_allowed/1,
    test_track_delivery_count/1,
    test_cleanup_delivery_count/1,
    test_handle_decide_message_export/1,
    test_check_maxdeliver_exhaustion/1,
    test_track_then_check_maxdeliver/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_module_exports,
            test_gen_server_callbacks,
            test_start_link_exported,
            test_behaviour_gen_server,
            test_normalize_boolean,
            test_check_tenant_allowed,
            test_track_delivery_count,
            test_cleanup_delivery_count,
            test_handle_decide_message_export,
            test_check_maxdeliver_exhaustion,
            test_track_then_check_maxdeliver
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    %% Create ETS table for delivery count tracking (needed by some tests)
    case ets:info(router_decide_delivery_count) of
        undefined -> 
            ets:new(router_decide_delivery_count, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear the ETS table before each test
    case ets:whereis(router_decide_delivery_count) of
        undefined -> ok;
        Tab -> ets:delete_all_objects(Tab)
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    %% Verify module is loaded and has exports
    {module, router_decide_consumer} = code:ensure_loaded(router_decide_consumer),
    Exports = router_decide_consumer:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ok.

test_start_link_exported(_Config) ->
    %% Test that module exports start_link
    Exports = router_decide_consumer:module_info(exports),
    ?assertEqual(true, lists:member({start_link, 0}, Exports)),
    ok.

test_gen_server_callbacks(_Config) ->
    %% Verify module implements gen_server callbacks
    Exports = router_decide_consumer:module_info(exports),
    
    %% Check required callbacks
    ?assertEqual(true, lists:member({init, 1}, Exports)),
    ?assertEqual(true, lists:member({handle_call, 3}, Exports)),
    ?assertEqual(true, lists:member({handle_cast, 2}, Exports)),
    ?assertEqual(true, lists:member({handle_info, 2}, Exports)),
    ok.

test_behaviour_gen_server(_Config) ->
    %% Verify module compiles as gen_server behaviour
    %% Check module attributes for behaviour declaration
    Attrs = router_decide_consumer:module_info(attributes),
    Behaviours = proplists:get_value(behaviour, Attrs, []) ++ 
                 proplists:get_value(behavior, Attrs, []),
    ?assertEqual(true, lists:member(gen_server, Behaviours)),
    ok.

%% ============================================================================
%% Tests for normalize_boolean/1
%% ============================================================================

test_normalize_boolean(_Config) ->
    %% Test atom true
    ?assertEqual(true, router_decide_consumer:normalize_boolean(true)),
    %% Test atom false
    ?assertEqual(false, router_decide_consumer:normalize_boolean(false)),
    %% Test binary "true"
    ?assertEqual(true, router_decide_consumer:normalize_boolean(<<"true">>)),
    %% Test binary "false"
    ?assertEqual(false, router_decide_consumer:normalize_boolean(<<"false">>)),
    %% Test integer 1
    ?assertEqual(true, router_decide_consumer:normalize_boolean(1)),
    %% Test integer 0
    ?assertEqual(false, router_decide_consumer:normalize_boolean(0)),
    %% Test unknown value
    ?assertEqual(false, router_decide_consumer:normalize_boolean(unknown)),
    ok.

%% ============================================================================
%% Tests for check_tenant_allowed/1
%% ============================================================================

test_check_tenant_allowed(_Config) ->
    %% Test with undefined tenant
    Result1 = router_decide_consumer:check_tenant_allowed(undefined),
    ?assertEqual(true, is_boolean(Result1)),
    
    %% Test with binary tenant
    Result2 = router_decide_consumer:check_tenant_allowed(<<"test-tenant">>),
    ?assertEqual(true, is_boolean(Result2)),
    ok.

%% ============================================================================
%% Tests for track_delivery_count/1
%% ============================================================================

test_track_delivery_count(_Config) ->
    %% Test tracking undefined
    Result1 = router_decide_consumer:track_delivery_count(undefined),
    ?assertEqual(ok, Result1),
    
    %% Test tracking binary msg_id
    MsgId = <<"test-msg-id-", (integer_to_binary(erlang:system_time(nanosecond)))/binary>>,
    Result2 = router_decide_consumer:track_delivery_count(MsgId),
    ?assertEqual(ok, Result2),
    
    %% Also track it again to test increment path
    Result3 = router_decide_consumer:track_delivery_count(MsgId),
    ?assertEqual(ok, Result3),
    ok.

%% ============================================================================
%% Tests for cleanup_delivery_count/1
%% ============================================================================

test_cleanup_delivery_count(_Config) ->
    %% Test cleanup undefined
    Result1 = router_decide_consumer:cleanup_delivery_count(undefined),
    ?assertEqual(ok, Result1),
    
    %% Test cleanup binary msg_id
    MsgId = <<"cleanup-test-msg-id">>,
    router_decide_consumer:track_delivery_count(MsgId),
    Result2 = router_decide_consumer:cleanup_delivery_count(MsgId),
    ?assertEqual(ok, Result2),
    ok.

%% ============================================================================
%% Tests for handle_decide_message export
%% ============================================================================

test_handle_decide_message_export(_Config) ->
    %% Verify that handle_decide_message is exported (for testing infra)
    Exports = router_decide_consumer:module_info(exports),
    ?assertEqual(true, lists:member({handle_decide_message, 4}, Exports)),
    ok.

%% ============================================================================
%% Tests for check_maxdeliver_exhaustion/3
%% ============================================================================

test_check_maxdeliver_exhaustion(_Config) ->
    %% Test with undefined msg_id
    Result1 = router_decide_consumer:check_maxdeliver_exhaustion(undefined, undefined, #{}),
    ?assertEqual(ok, Result1),
    
    %% Test with binary msg_id that's not tracked
    MsgId = <<"maxdeliver-test-", (integer_to_binary(erlang:system_time(nanosecond)))/binary>>,
    Result2 = router_decide_consumer:check_maxdeliver_exhaustion(MsgId, <<"req-123">>, #{}),
    ?assertEqual(ok, Result2),
    ok.

%% ============================================================================
%% Tests for track + check_maxdeliver flow
%% ============================================================================

test_track_then_check_maxdeliver(_Config) ->
    MsgId = <<"flow-test-", (integer_to_binary(erlang:system_time(nanosecond)))/binary>>,
    
    %% Track multiple deliveries (simulate MaxDeliver exhaustion)
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    
    %% Check exhaustion (should emit metric if >= MaxDeliver)
    Result = router_decide_consumer:check_maxdeliver_exhaustion(MsgId, <<"req-abc">>, #{
        <<"tenant_id">> => <<"test-tenant">>
    }),
    ?assertEqual(ok, Result),
    ok.


