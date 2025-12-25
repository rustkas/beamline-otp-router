%% @doc Integration Test Suite for gRPC Router.Decide Service
%% Tests actual gRPC client-server communication
%% @test_category fast
-module(router_grpc_integration_SUITE).

%% INTENTIONAL_EMPTY: placeholder suite, no test cases yet.
%% See docs/tests/README_CT_GROUPS.md.
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/flow_pb.hrl").


%% Test suite callbacks
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [
        {timetrap, {minutes, 2}}
    ].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(_) ->
    [].

groups() ->
    [].

%% Standard lifecycle hooks: placeholder to allow future shared setup/teardown
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
