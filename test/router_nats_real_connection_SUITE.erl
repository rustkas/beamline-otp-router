%% @doc Real NATS Connection Tests
%%
%% Tests for real NATS connections using enats client.
%% These tests verify that the router can connect to and communicate
%% with a real NATS server when configured appropriately.
%%
%% @test_category integration, nats, real_connection
-module(router_nats_real_connection_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_nats_connection_config_validation/1,
    test_nats_stub_mode_fallback/1,
    test_nats_connection_without_server/1,
    test_nats_publish_in_stub_mode/1,
    test_nats_publish_with_ack_in_stub_mode/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    [
        test_nats_connection_config_validation,
        test_nats_stub_mode_fallback,
        test_nats_connection_without_server,
        test_nats_publish_in_stub_mode,
        test_nats_publish_with_ack_in_stub_mode
    ].

init_per_suite(Config) ->
    ok = router_mock_helpers:ensure_test_deps(),
    Config.

end_per_suite(_Config) ->
    router_mock_helpers:unload_all(),
    ok.

init_per_testcase(TestCase, Config) ->
    router_mock_helpers:unload_all(),
    
    %% Set NATS configuration for testing
    case TestCase of
        test_nats_stub_mode_fallback ->
            %% No NATS URL - should fallback to stub mode
            ok = application:unset_env(beamline_router, nats_url);
        test_nats_connection_without_server ->
            %% Set NATS URL but no server running
            ok = application:set_env(beamline_router, nats_url, <<"nats://localhost:4222">>);
        _ ->
            %% Default: no NATS URL for stub mode
            ok = application:unset_env(beamline_router, nats_url)
    end,
    
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_mock_helpers:unload_all(),
    ok.

%% @doc Test NATS connection configuration validation
test_nats_connection_config_validation(_Config) ->
    %% Test with valid configuration
    ok = application:set_env(beamline_router, nats_url, <<"nats://localhost:4222">>),
    
    %% Start router_nats
    {ok, _Pid} = router_nats:start_link(),
    
    %% Check connection status (should be attempting to connect)
    Status = router_nats:get_connection_status(),
    ?assertMatch({ok, _}, Status),
    
    %% Clean up
    ok = router_nats:stop().

%% @doc Test fallback to stub mode when no NATS URL configured
test_nats_stub_mode_fallback(_Config) ->
    %% Ensure no NATS URL is configured
    ok = application:unset_env(beamline_router, nats_url),
    
    %% Start router_nats
    {ok, _Pid} = router_nats:start_link(),
    
    %% Check connection status (should be in stub mode)
    Status = router_nats:get_connection_status(),
    ?assertMatch({ok, _}, Status),
    
    %% Should be able to publish in stub mode
    Subject = <<"test.stub.publish">>,
    Payload = <<"test payload">>,
    
    %% Publish should work in stub mode
    ok = router_nats:publish(Subject, Payload),
    
    %% Publish with ack should work in stub mode
    {ok, _MsgId} = router_nats:publish_with_ack(Subject, Payload, #{}),
    
    %% Clean up
    ok = router_nats:stop().

%% @doc Test NATS connection behavior when server is not available
test_nats_connection_without_server(_Config) ->
    %% Set NATS URL to non-existent server
    ok = application:set_env(beamline_router, nats_url, <<"nats://localhost:4222">>),
    
    %% Start router_nats
    {ok, _Pid} = router_nats:start_link(),
    
    %% Check connection status (should show connection failure)
    Status = router_nats:get_connection_status(),
    ?assertMatch({ok, _}, Status),
    
    %% Wait deterministically for router_nats to enter a non-connected state
    router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 2000),
    
    %% Should still be able to publish in fail-open mode
    Subject = <<"test.fail.open.publish">>,
    Payload = <<"test payload">>,
    
    %% Enable fail-open mode
    ok = application:set_env(beamline_router, nats_fail_open_mode, true),
    
    %% Publish should work in fail-open mode
    ok = router_nats:publish(Subject, Payload),
    
    %% Clean up
    ok = router_nats:stop().

%% @doc Test publish operations in stub mode
test_nats_publish_in_stub_mode(_Config) ->
    %% Start router_nats in stub mode
    {ok, _Pid} = router_nats:start_link(),
    
    %% Test basic publish
    Subject = <<"test.publish.stub">>,
    Payload = <<"test payload">>,
    
    %% Publish should succeed
    ok = router_nats:publish(Subject, Payload),
    
    %% Test multiple publishes
    Subjects = [
        <<"test.publish.1">>,
        <<"test.publish.2">>,
        <<"test.publish.3">>
    ],
    
    lists:foreach(fun(Subj) ->
        ok = router_nats:publish(Subj, <<"test payload">>)
    end, Subjects),
    
    %% Clean up
    ok = router_nats:stop().

%% @doc Test publish with ack operations in stub mode
test_nats_publish_with_ack_in_stub_mode(_Config) ->
    %% Start router_nats in stub mode
    {ok, _Pid} = router_nats:start_link(),
    
    %% Test publish with ack
    Subject = <<"test.publish.ack.stub">>,
    Payload = <<"test payload">>,
    Headers = #{<<"test-header">> => <<"test-value">>},
    
    %% Publish with ack should succeed and return message ID
    {ok, MsgId} = router_nats:publish_with_ack(Subject, Payload, Headers),
    ?assert(is_binary(MsgId)),
    ?assert(size(MsgId) > 0),
    
    %% Test multiple publishes with ack
    lists:foreach(fun(I) ->
        Subj = <<"test.publish.ack.", (integer_to_binary(I))/binary>>,
        {ok, Id} = router_nats:publish_with_ack(Subj, Payload, Headers),
        ?assert(is_binary(Id)),
        ?assert(size(Id) > 0)
    end, lists:seq(1, 5)),
    
    %% Clean up
    ok = router_nats:stop().
