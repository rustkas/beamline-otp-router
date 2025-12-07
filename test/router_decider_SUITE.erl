%% @doc Unit tests for router_decider module
%% Tests: sticky sessions, weighted distribution, fallback policies
%% @test_category cp1_smoke, fast
-module(router_decider_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-define(TENANT_ID, <<"tenant-123">>).
-define(PROVIDER_A, <<"provider-a">>).
-define(PROVIDER_B, <<"provider-b">>).
-define(PROVIDER_FALLBACK, <<"provider-fallback">>).

%% ========================================================================
%% Test suite configuration
%% ========================================================================

all() ->
    [
        {group, decider_tests}
    ].

groups() ->
    [
        {decider_tests, [sequence], [
            %% Placeholder: tests will be added when router_decider module is implemented
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.