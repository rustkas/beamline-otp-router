%% @doc Integration Test Suite for gRPC Router.Decide Service
%% Tests actual gRPC client-server communication
%% @test_category fast
-module(router_grpc_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").
-include("../include/flow_pb.hrl").


%% Test suite callbacks